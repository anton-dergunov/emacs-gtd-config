;;; ps-scrollbar.el --- Auto-hiding, theme-colored fringe scrollbars -*- lexical-binding: t; -*-

;;; Commentary:
;; A custom, auto-hiding scrollbar drawn in each window's right fringe.
;;
;; Why custom: on the macOS GNU Emacs NS (Cocoa) build the toolkit draws the
;; native scroll bar itself and ignores the `scroll-bar' face, so it can't be
;; recolored and won't honor the system "hide while idle" setting.  We draw the
;; thumb ourselves as a fringe bitmap colored by a theme-aware face, show it on
;; scroll and on mouse hover, fade it after a short idle delay, and let the user
;; click-drag it.
;;
;; The thumb is built from fringe bitmaps placed on the screen rows it covers.
;; To stay continuous (no inter-line gaps -- the "Christmas lights" look of
;; segmented scrollbars) the body bitmap is *periodic*, so it tiles to fill any
;; line height, and the rounded end caps are tall solid bitmaps clipped to the
;; line.  Overlays carry a `window' property so a buffer shown in several
;; windows gets an independent thumb in each.
;;
;; All redraws happen from a low-frequency timer, never from a redisplay hook,
;; which keeps it robust against reentrancy and flicker (including under
;; ultra-scroll's smooth pixel scrolling).

;;; Code:

;;; State

(defvar ps/scrollbar--shown nil "List of windows currently showing a thumb.")
(defvar ps/scrollbar--timer nil "Repeating refresh timer, or nil.")

;;; Customization

(defgroup ps-scrollbar nil
  "Auto-hiding, theme-colored scrollbar drawn in the right fringe."
  :group 'ps)

(defcustom ps/scrollbar-fringe-width 10
  "Width in pixels reserved for the right fringe that hosts the thumb.
Should be at least 8 (the thumb bitmap width); a little extra leaves a
small gap between the pill and the window edge."
  :type 'integer
  :group 'ps-scrollbar)

(defcustom ps/scrollbar-hide-delay 1.0
  "Seconds of inactivity (no scroll, no hover) before the thumb fades."
  :type 'number
  :group 'ps-scrollbar)

(defcustom ps/scrollbar-show-on-hover t
  "When non-nil, reveal the thumb whenever the mouse is over a window."
  :type 'boolean
  :group 'ps-scrollbar)

(defcustom ps/scrollbar-min-thumb-lines 1
  "Minimum thumb height in screen lines."
  :type 'integer
  :group 'ps-scrollbar)

(defcustom ps/scrollbar-tick-interval 0.12
  "Seconds between scrollbar refresh ticks (hover detection, scroll follow)."
  :type 'number
  :group 'ps-scrollbar)

(defcustom ps/scrollbar-exclude-modes '(treemacs-mode which-key-mode)
  "Major modes whose windows never get a scrollbar.
Terminal modes (e.g. `eat-mode' for the Claude Code window) are deliberately
NOT excluded: the scrollbar is read-only fringe decoration and never touches
the terminal's input region or cursor."
  :type '(repeat symbol)
  :group 'ps-scrollbar)

(defface ps/scrollbar-thumb
  '((t :inherit shadow))
  "Face whose foreground colors the scrollbar thumb.
Inherits from `shadow' so it stays calm and adapts to light/dark themes."
  :group 'ps-scrollbar)

(defface ps/scrollbar-thumb-active
  '((t :inherit (shadow bold)))
  "Face for the thumb while the window is hovered or the thumb is dragged."
  :group 'ps-scrollbar)

;;; Fringe bitmaps
;; 8px-wide pill: a 6px solid core with a 1px gap each side (#b01111110), and
;; 2px/4px rounded corners.  Caps are tall (clipped to the line); the body is
;; periodic (tiled) so it is continuous at any line height.

(defconst ps/scrollbar--body-row #b01111110)
(defconst ps/scrollbar--round1-row #b00011000)
(defconst ps/scrollbar--round2-row #b00111100)
(defconst ps/scrollbar--cap-height 64
  "Height of the end-cap bitmaps; larger than any sane line so they clip.")

(defun ps/scrollbar--define-bitmaps ()
  "Define (or redefine) the thumb fringe bitmaps."
  (let* ((h ps/scrollbar--cap-height)
         (top (make-vector h ps/scrollbar--body-row))
         (bot (make-vector h ps/scrollbar--body-row))
         (single (vector ps/scrollbar--round1-row ps/scrollbar--round2-row
                         ps/scrollbar--body-row ps/scrollbar--body-row
                         ps/scrollbar--body-row ps/scrollbar--body-row
                         ps/scrollbar--body-row ps/scrollbar--body-row
                         ps/scrollbar--body-row ps/scrollbar--body-row
                         ps/scrollbar--round2-row ps/scrollbar--round1-row)))
    (aset top 0 ps/scrollbar--round1-row)
    (aset top 1 ps/scrollbar--round2-row)
    (aset bot (- h 1) ps/scrollbar--round1-row)
    (aset bot (- h 2) ps/scrollbar--round2-row)
    ;; Periodic 6px body: tiles to fill any line height -> continuous bar.
    (define-fringe-bitmap 'ps/scrollbar--thumb-mid
      (vector ps/scrollbar--body-row ps/scrollbar--body-row) 2 8 '(center t))
    (define-fringe-bitmap 'ps/scrollbar--thumb-top top h 8 'top)
    (define-fringe-bitmap 'ps/scrollbar--thumb-bottom bot h 8 'bottom)
    (define-fringe-bitmap 'ps/scrollbar--thumb-single single
      (length single) 8 'center)))

;;; Geometry (pure)

(defun ps/scrollbar--thumb-geometry (pmin pmax wstart wend body-lines min-lines)
  "Return (TOP-LINE . LEN-LINES) for the thumb, or nil if content fits.
PMIN/PMAX are buffer bounds, WSTART/WEND the window's visible char range,
BODY-LINES the window body height in screen lines, MIN-LINES the minimum
thumb height.  Char-position based, like the native scroll bar."
  (let ((total (- pmax pmin)))
    (when (and (> total 0)
               (> body-lines 0)
               ;; Only when something is actually off-screen.
               (or (> wstart pmin) (< wend pmax)))
      (let* ((top-frac (/ (float (- wstart pmin)) total))
             (vis-frac (/ (float (- wend wstart)) total))
             (len (min body-lines (max min-lines (round (* vis-frac body-lines)))))
             (top (max 0 (min (floor (* top-frac body-lines))
                              (- body-lines len)))))
        (cons top len)))))

;;; Per-window overlays

(defun ps/scrollbar--clear (window)
  "Delete WINDOW's scrollbar overlays and mark it not shown."
  (dolist (ov (window-parameter window 'ps/scrollbar--overlays))
    (when (overlayp ov) (delete-overlay ov)))
  (set-window-parameter window 'ps/scrollbar--overlays nil)
  (set-window-parameter window 'ps/scrollbar--shown-at nil)
  (setq ps/scrollbar--shown (delq window ps/scrollbar--shown)))

(defun ps/scrollbar--put (window pos bitmap face)
  "Make a fringe overlay in WINDOW at POS showing BITMAP in FACE.
Return the overlay."
  (let ((ov (make-overlay pos pos))
        (s (copy-sequence " ")))
    (put-text-property 0 1 'display (list 'right-fringe bitmap face) s)
    (overlay-put ov 'window window)
    (overlay-put ov 'ps/scrollbar t)
    (overlay-put ov 'before-string s)
    ov))

(defun ps/scrollbar--candidate-window-p (window)
  "Non-nil if WINDOW should ever display a scrollbar."
  (and (window-live-p window)
       (not (window-minibuffer-p window))
       (not (memq (buffer-local-value 'major-mode (window-buffer window))
                  ps/scrollbar-exclude-modes))))

(defun ps/scrollbar--draw (window &optional active)
  "Draw WINDOW's thumb (clearing any previous one).
With ACTIVE non-nil use the hover/drag face."
  (when (ps/scrollbar--candidate-window-p window)
    (let* ((buf (window-buffer window))
           (face (if active 'ps/scrollbar-thumb-active 'ps/scrollbar-thumb))
           (body (window-body-height window))
           geom)
      (with-current-buffer buf
        (setq geom (ps/scrollbar--thumb-geometry
                    (point-min) (point-max)
                    (window-start window) (window-end window t)
                    body ps/scrollbar-min-thumb-lines)))
      (ps/scrollbar--clear window)
      (when geom
        (let* ((top (car geom))
               (len (cdr geom))
               (ovs '()))
          (with-selected-window window
            (save-excursion
              (goto-char (window-start window))
              (when (> (vertical-motion top) -1) ; reach first thumb row
                (let ((i 0) (continue t))
                  (while (and continue (< i len))
                    (let* ((pos (point))
                           (bmp (cond ((= len 1) 'ps/scrollbar--thumb-single)
                                      ((= i 0) 'ps/scrollbar--thumb-top)
                                      ((= i (1- len)) 'ps/scrollbar--thumb-bottom)
                                      (t 'ps/scrollbar--thumb-mid))))
                      (push (ps/scrollbar--put window pos bmp face) ovs)
                      (setq i (1+ i))
                      (when (and (< i len) (zerop (vertical-motion 1)))
                        (setq continue nil))))))))
          (set-window-parameter window 'ps/scrollbar--overlays ovs)
          (set-window-parameter window 'ps/scrollbar--shown-at (float-time))
          (set-window-parameter window 'ps/scrollbar--last-start
                                (window-start window))
          (unless (memq window ps/scrollbar--shown)
            (push window ps/scrollbar--shown)))))))

;;; Show/hide lifecycle

(defun ps/scrollbar--scrolled-p (window)
  "Non-nil if WINDOW's start moved since it was last drawn."
  (not (eql (window-start window)
            (window-parameter window 'ps/scrollbar--last-start))))

(defun ps/scrollbar--mouse-window ()
  "Return the live window under the mouse, or nil."
  (let* ((mp (mouse-position))
         (frame (car mp)) (x (cadr mp)) (y (cddr mp)))
    (and (frame-live-p frame) (numberp x) (numberp y)
         (window-at x y frame))))

(defun ps/scrollbar--tick ()
  "Refresh thumbs: reveal on hover/scroll, fade after the idle delay."
  (when (bound-and-true-p ps/scrollbar-mode)
    (let ((now (float-time))
          (hovered (and ps/scrollbar-show-on-hover (ps/scrollbar--mouse-window)))
          (sel (selected-window)))
      ;; Reveal / follow the hovered window every tick (keeps it up while hovering).
      (when (and hovered (ps/scrollbar--candidate-window-p hovered))
        (ps/scrollbar--draw hovered t))
      ;; Reveal / follow any window that just scrolled.
      (dolist (w (cons sel (copy-sequence ps/scrollbar--shown)))
        (when (and (not (eq w hovered))
                   (window-live-p w)
                   (ps/scrollbar--candidate-window-p w)
                   (ps/scrollbar--scrolled-p w))
          (ps/scrollbar--draw w nil)))
      ;; Fade windows idle past the delay.
      (dolist (w (copy-sequence ps/scrollbar--shown))
        (if (not (window-live-p w))
            (ps/scrollbar--clear w)
          (unless (eq w hovered)
            (let ((t0 (window-parameter w 'ps/scrollbar--shown-at)))
              (when (and t0 (> (- now t0) ps/scrollbar-hide-delay))
                (ps/scrollbar--clear w)))))))))

;;; Click-drag

(defun ps/scrollbar--drag-to (window y offset)
  "Scroll WINDOW so the thumb top tracks mouse pixel Y (minus grab OFFSET)."
  (let* ((edges (window-inside-pixel-edges window))
         (px-height (max 1 (- (nth 3 edges) (nth 1 edges))))
         (frac (max 0.0 (min 1.0 (/ (float (- y offset)) px-height))))
         (buf (window-buffer window)))
    (with-current-buffer buf
      (let ((target (+ (point-min) (floor (* frac (- (point-max) (point-min)))))))
        (set-window-start
         window
         (save-excursion (goto-char target) (line-beginning-position)))))
    (ps/scrollbar--draw window t)))

(defun ps/scrollbar--thumb-top-pixel (window)
  "Return the thumb's top edge in WINDOW pixels, or 0."
  (let ((ovs (window-parameter window 'ps/scrollbar--overlays)))
    (or (and ovs
             (let* ((ov (car (last ovs))) ; overlays were pushed, so last = top row
                    (posn (and (overlayp ov)
                               (posn-at-point (overlay-start ov) window))))
               (and posn (cdr (posn-x-y posn)))))
        0)))

(defun ps/scrollbar--start-drag (event)
  "Drag the thumb under EVENT to scroll its window."
  (interactive "e")
  (let* ((start (event-start event))
         (window (posn-window start)))
    ;; Only drag when this window is actually showing a thumb -- a click on an
    ;; empty fringe should do nothing.
    (when (and (window-live-p window)
               (window-parameter window 'ps/scrollbar--overlays))
      (let* ((y0 (cdr (posn-x-y start)))
             (thumb-top (ps/scrollbar--thumb-top-pixel window))
             ;; Grab offset so the thumb doesn't jump to the cursor on click.
             (offset (if (and (numberp y0) (>= y0 thumb-top))
                         (- y0 thumb-top) 0)))
        (track-mouse
          (let (ev)
            (while (progn (setq ev (read-event))
                          (mouse-movement-p ev))
              (let* ((posn (event-start ev))
                     (y (cdr (posn-x-y posn))))
                (when (and (eq (posn-window posn) window) (numberp y))
                  (ps/scrollbar--drag-to window y offset))))))))))

;;; Enable / disable

(defvar ps/scrollbar-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [right-fringe down-mouse-1] #'ps/scrollbar--start-drag)
    (define-key map [right-fringe mouse-1] #'ignore)
    map)
  "Keymap active while `ps/scrollbar-mode' is on.")

(defun ps/scrollbar--apply-fringe-width ()
  "Reserve the right fringe on all frames (and future ones)."
  (modify-all-frames-parameters
   (list (cons 'right-fringe ps/scrollbar-fringe-width))))

(defun ps/scrollbar--clear-all ()
  "Remove every thumb."
  (dolist (w (copy-sequence ps/scrollbar--shown))
    (ps/scrollbar--clear w)))

;;;###autoload
(define-minor-mode ps/scrollbar-mode
  "Global minor mode: auto-hiding, theme-colored fringe scrollbars."
  :global t
  :group 'ps-scrollbar
  (if ps/scrollbar-mode
      (progn
        (ps/scrollbar--define-bitmaps)
        (ps/scrollbar--apply-fringe-width)
        (when ps/scrollbar--timer (cancel-timer ps/scrollbar--timer))
        (setq ps/scrollbar--timer
              (run-with-timer ps/scrollbar-tick-interval
                              ps/scrollbar-tick-interval
                              #'ps/scrollbar--tick)))
    (when ps/scrollbar--timer
      (cancel-timer ps/scrollbar--timer)
      (setq ps/scrollbar--timer nil))
    (ps/scrollbar--clear-all)))

(provide 'ps-scrollbar)
;;; ps-scrollbar.el ends here
