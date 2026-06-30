;;; ps-scrollbar.el --- Auto-hiding scroll-position indicator -*- lexical-binding: t; -*-

;;; Commentary:
;; A minimal, auto-hiding scroll-position indicator: a small coloured "pill" at
;; the right edge of a window that appears while you scroll and fades when idle.
;; It shows WHERE you are; it is not draggable -- scroll with the
;; wheel/trackpad/keyboard (smooth via ultra-scroll).  The native scroll bar is
;; disabled in config.org because the macOS NS toolkit ignores its face (it
;; can't be themed or auto-hidden).
;;
;; The pill is a tiny child frame whose background colour is the thumb, sized to
;; the thumb and positioned at its location.  A few hard-won notes about the
;; macOS NS build (especially behind a scaled / mirrored virtual display, e.g.
;; BetterDisplay):
;; - The thumb is a solid-coloured frame, not an image: a narrow rectangle drawn
;;   inside an SVG collapses to ~1px on a scaled display, while frame sizes
;;   render correctly.
;; - A child frame composites on a mirrored display (a top-level frame would,
;;   but it carries an unremovable window shadow); however it is invisible if
;;   its content is painted before it is shown, so we show first, then redisplay.
;; - Our own `window-size-change-functions' handler must ignore the size changes
;;   we make, or it destroys the pill mid-render.
;; - The pill width scales with the actual fringe width (a fixed pixel count is
;;   too thin on a HiDPI screen).
;; - macOS lets the user move/resize this borderless window and no frame
;;   parameter prevents it; an accidental nudge is snapped back from the timer.

;;; Code:

;;; State

(defvar ps/scrollbar-mode)              ; defined by `define-minor-mode' below

(defvar ps/scrollbar--frames (make-hash-table :test 'eq)
  "Hash of parent frame -> its pill child frame.")
(defvar ps/scrollbar--timer nil)
(defvar ps/scrollbar--visible-frame nil
  "The pill frame currently visible, or nil.")
(defvar ps/scrollbar--last-sig nil
  "Signature of the last render, to skip redundant work.")
(defvar ps/scrollbar--shown-at nil
  "`float-time' of the last scroll/hover activity.")
(defvar ps/scrollbar--busy nil
  "Non-nil while we create/show/move/delete our own frame, so our
`window-size-change-functions' handler does not react to it.")
(defvar ps/scrollbar--saved-right-fringe 'unset
  "Prior `default-frame-alist' right-fringe, restored on disable.")

;;; Customization

(defgroup ps-scrollbar nil
  "Auto-hiding scroll-position indicator."
  :group 'ps)

(defcustom ps/scrollbar-width 6
  "Visible pill width, in pixels at `ps/scrollbar-fringe-width'.
The pill scales with the window's actual fringe width, so on a HiDPI /
scaled display it stays proportional rather than turning into a sliver."
  :type 'integer :group 'ps-scrollbar)

(defcustom ps/scrollbar-fringe-width 14
  "Right fringe reserved (in pixels) for the pill strip."
  :type 'integer :group 'ps-scrollbar)

(defcustom ps/scrollbar-hide-delay 1.0
  "Seconds of inactivity before the pill fades."
  :type 'number :group 'ps-scrollbar)

(defcustom ps/scrollbar-show-on-hover nil
  "When non-nil, also reveal the pill while the mouse is over a window.
Off by default: the pill appears only when you scroll."
  :type 'boolean :group 'ps-scrollbar)

(defcustom ps/scrollbar-min-thumb-pixels 24
  "Minimum pill height in pixels."
  :type 'integer :group 'ps-scrollbar)

(defcustom ps/scrollbar-tick-interval 0.12
  "Seconds between refresh ticks (scroll/hover detection, snap-back)."
  :type 'number :group 'ps-scrollbar)

(defcustom ps/scrollbar-exclude-modes '(treemacs-mode which-key-mode)
  "Major modes whose windows never get a pill."
  :type '(repeat symbol) :group 'ps-scrollbar)

(defface ps/scrollbar-thumb
  '((t :inherit shadow))
  "Face whose foreground colours the pill (calm, theme-aware via `shadow')."
  :group 'ps-scrollbar)

;;; Pure geometry (unit-tested)

(defun ps/scrollbar--thumb-span (pmin pmax wstart wend strip-h min-h)
  "Return (Y . H) pill pixels within a STRIP-H tall track, or nil if content fits.
PMIN/PMAX are buffer bounds, WSTART/WEND the window's visible char range,
MIN-H the minimum pill height."
  (let ((total (- pmax pmin)))
    (when (and (> total 0) (> strip-h 0)
               (or (> wstart pmin) (< wend pmax)))
      (let* ((top-frac (/ (float (- wstart pmin)) total))
             (size-frac (/ (float (- wend wstart)) total))
             (h (min strip-h (max min-h (round (* size-frac strip-h)))))
             (y (max 0 (min (round (* top-frac strip-h)) (- strip-h h)))))
        (cons y h)))))

(defun ps/scrollbar--color ()
  "Resolve the pill colour from `ps/scrollbar-thumb'."
  (or (face-foreground 'ps/scrollbar-thumb nil t) "gray60"))

;;; Child frame

(defun ps/scrollbar--buffer ()
  "Return the shared pill child-frame buffer, creating it if needed.
The buffer is empty: the pill is the frame's own background colour."
  (or (get-buffer " *ps-scrollbar*")
      (with-current-buffer (get-buffer-create " *ps-scrollbar*")
        (setq-local mode-line-format nil
                    header-line-format nil
                    tab-line-format nil
                    cursor-type nil
                    cursor-in-non-selected-windows nil
                    left-margin-width 0
                    right-margin-width 0
                    truncate-lines t
                    line-spacing 0
                    show-trailing-whitespace nil)
        (current-buffer))))

(defun ps/scrollbar--make-frame (parent)
  "Create the invisible pill child frame for PARENT."
  (let* ((buf (ps/scrollbar--buffer))
         (frame (make-frame
                 `((parent-frame . ,parent)
                   (no-accept-focus . t)
                   (no-focus-on-map . t)
                   (undecorated . t)
                   (unsplittable . t)
                   (no-other-frame . t)
                   (minibuffer . nil)
                   (visibility . nil)
                   (internal-border-width . 0)
                   (left-fringe . 0)
                   (right-fringe . 0)
                   (vertical-scroll-bars . nil)
                   (horizontal-scroll-bars . nil)
                   (menu-bar-lines . 0)
                   (tool-bar-lines . 0)
                   (tab-bar-lines . 0)
                   (line-spacing . 0)
                   (cursor-type . nil)
                   ;; min-width/height 0 so the pixel size we set is honoured
                   ;; (otherwise Emacs clamps to a ~15-char minimum).
                   (min-width . 0)
                   (min-height . 0)
                   (background-color . ,(frame-parameter parent 'background-color))
                   (width . 1) (height . 1)
                   (desktop-dont-save . t)))))
    (with-current-buffer buf (let ((inhibit-read-only t)) (erase-buffer)))
    (set-window-buffer (frame-root-window frame) buf)
    (set-window-dedicated-p (frame-root-window frame) t)
    frame))

(defun ps/scrollbar--ensure-frame (parent)
  "Return PARENT's pill frame, creating it if needed."
  (let ((child (gethash parent ps/scrollbar--frames)))
    (unless (and child (frame-live-p child))
      (setq child (ps/scrollbar--make-frame parent))
      (puthash parent child ps/scrollbar--frames))
    child))

(defun ps/scrollbar--own-frame-p (frame)
  "Non-nil if FRAME is one of our pill frames."
  (let (found)
    (maphash (lambda (_p c) (when (eq c frame) (setq found t)))
             ps/scrollbar--frames)
    found))

(defun ps/scrollbar--destroy (frame)
  "Delete FRAME and drop it from the cache.
We never `make-frame-invisible' our frames: on a mirrored display a child
frame that is hidden and re-shown can end up visible-but-not-painting.
Deleting and recreating a fresh frame on the next show is self-healing."
  (when (framep frame)
    (maphash (lambda (p c) (when (eq c frame) (remhash p ps/scrollbar--frames)))
             ps/scrollbar--frames)
    (when (frame-live-p frame)
      (let ((ps/scrollbar--busy t))
        (delete-frame frame)))))

(defun ps/scrollbar--reveal (child)
  "Make CHILD the single visible pill frame, destroying any other."
  (when (and ps/scrollbar--visible-frame
             (not (eq ps/scrollbar--visible-frame child))
             (frame-live-p ps/scrollbar--visible-frame))
    (ps/scrollbar--destroy ps/scrollbar--visible-frame))
  (setq ps/scrollbar--visible-frame child)
  (unless (frame-visible-p child)
    (make-frame-visible child)))

(defun ps/scrollbar--hide ()
  "Hide the pill by destroying its frame (recreated on the next show)."
  (when (and ps/scrollbar--visible-frame
             (frame-live-p ps/scrollbar--visible-frame))
    (ps/scrollbar--destroy ps/scrollbar--visible-frame))
  (setq ps/scrollbar--visible-frame nil
        ps/scrollbar--last-sig nil))

;;; Rendering

(defun ps/scrollbar--render (window)
  "Show/position the pill for WINDOW.
Return `skip' (window-end unknown), `same' (nothing changed), `hidden'
\(content fits) or `rendered'."
  (let ((parent (window-frame window))
        (wend (window-end window)))           ; no update flag (cheap)
    (if (null wend)
        'skip
      (pcase-let* ((`(,_iL ,iT ,_iR ,iB) (window-inside-pixel-edges window))
                   (buf (window-buffer window))
                   (pmin (with-current-buffer buf (point-min)))
                   (pmax (with-current-buffer buf (point-max)))
                   (wstart (window-start window))
                   (strip-h (- iB iT))
                   (sig (list window pmax wstart wend iT strip-h
                              (window-pixel-left window)
                              (window-pixel-width window))))
        (if (equal sig ps/scrollbar--last-sig)
            'same
          (setq ps/scrollbar--last-sig sig)
          (let ((span (ps/scrollbar--thumb-span
                       pmin pmax wstart wend strip-h
                       ps/scrollbar-min-thumb-pixels)))
            (if (null span)
                (progn (ps/scrollbar--hide) 'hidden)
              (pcase-let* ((`(,thumb-y . ,thumb-h) span)
                           ;; The pill sits over the window's right fringe.  Its
                           ;; position is PARENT-RELATIVE, so subtract the parent
                           ;; frame's native origin (pl/pt).
                           (`(,_wl ,_wt ,wr ,_wb) (window-edges window nil t t))
                           (`(,_bl ,bt ,br ,_bb)  (window-edges window t   t t))
                           (`(,pl ,pt ,_pr ,_pb)  (frame-edges parent 'native-edges))
                           (strip-w (max 2 (- wr br)))
                           ;; Width as a fraction of the actual fringe width, so
                           ;; it scales with the display.
                           (pill-w (min strip-w
                                        (max 2 (round (* strip-w
                                                         (/ (float ps/scrollbar-width)
                                                            (max 1 ps/scrollbar-fringe-width)))))))
                           (pill-x (max 0 (round (/ (- strip-w pill-w) 2.0))))
                           (fleft (+ (- br pl) pill-x))
                           (ftop (+ (- bt pt) thumb-y))
                           (color (ps/scrollbar--color)))
                (let* ((ps/scrollbar--busy t)
                       (child (ps/scrollbar--ensure-frame parent))
                       (geom (list fleft ftop pill-w thumb-h)))
                  (unless (equal color (frame-parameter child 'background-color))
                    (set-frame-parameter child 'background-color color))
                  (unless (equal geom (frame-parameter child 'ps/scrollbar-geom))
                    (set-frame-parameter child 'ps/scrollbar-geom geom)
                    (set-frame-size child pill-w thumb-h t)
                    (set-frame-position child fleft ftop))
                  (ps/scrollbar--reveal child)
                  (redisplay t))
                'rendered))))))))

;;; Lifecycle / detection

(defun ps/scrollbar--candidate-window-p (window)
  "Non-nil if WINDOW should display a pill."
  (and (window-live-p window)
       (not (window-minibuffer-p window))
       (not (ps/scrollbar--own-frame-p (window-frame window)))
       (not (eq (window-buffer window) (get-buffer " *ps-scrollbar*")))
       (not (memq (buffer-local-value 'major-mode (window-buffer window))
                  ps/scrollbar-exclude-modes))))

(defun ps/scrollbar--mouse-window ()
  "Return the live window under the mouse, or nil."
  (let* ((mp (mouse-position))
         (frame (car mp)) (x (cadr mp)) (y (cddr mp)))
    (and (frame-live-p frame) (numberp x) (numberp y)
         (window-at x y frame))))

(defun ps/scrollbar--scrolled-window (w)
  "Non-nil if W's `window-start' changed since the last tick (updates it)."
  (and (window-live-p w)
       (let ((cur (window-start w))
             (prev (window-parameter w 'ps/scrollbar--last-start)))
         (set-window-parameter w 'ps/scrollbar--last-start cur)
         (and prev (not (eql cur prev))))))

(defun ps/scrollbar--snap-back ()
  "Re-apply the visible pill's intended geometry (undo an accidental nudge).
macOS lets the user move/resize the borderless pill window; we put it back."
  (let ((f ps/scrollbar--visible-frame))
    (when (and f (frame-live-p f))
      (let ((geom (frame-parameter f 'ps/scrollbar-geom)))
        (when geom
          (pcase-let ((`(,l ,tp ,w ,h) geom)
                      (ps/scrollbar--busy t))
            (unless (and (= (frame-pixel-width f) w)
                         (= (frame-pixel-height f) h))
              (set-frame-size f w h t))
            (set-frame-position f l tp)))))))

(defun ps/scrollbar--tick ()
  "Refresh the pill: reveal on scroll/hover, fade when idle, snap back nudges."
  (when ps/scrollbar-mode
    (condition-case err
        (ps/scrollbar--tick-1)
      (error (message "ps/scrollbar: %S" err)))))

(defun ps/scrollbar--tick-1 ()
  (let* ((now (float-time))
         (mouse-win (ps/scrollbar--mouse-window))
         (sel (selected-window))
         ;; Mouse-wheel scrolls the window under the pointer (maybe not selected);
         ;; keyboard scrolls the selected one.  Watch both.
         (mouse-scrolled (and mouse-win (ps/scrollbar--scrolled-window mouse-win)))
         (sel-scrolled (and (not (eq sel mouse-win))
                            (ps/scrollbar--scrolled-window sel)))
         (hovered (and ps/scrollbar-show-on-hover mouse-win))
         target)
    (cond
     ((and mouse-scrolled (ps/scrollbar--candidate-window-p mouse-win))
      (setq target mouse-win))
     ((and sel-scrolled (ps/scrollbar--candidate-window-p sel))
      (setq target sel))
     ((and hovered (ps/scrollbar--candidate-window-p mouse-win))
      (setq target mouse-win)))
    (if (and target (display-graphic-p (window-frame target)))
        (let ((res (ps/scrollbar--render target)))
          (unless (eq res 'skip)
            (setq ps/scrollbar--shown-at now)))
      ;; No activity: fade when idle, otherwise snap an accidental nudge back.
      (if (and ps/scrollbar--visible-frame
               ps/scrollbar--shown-at
               (> (- now ps/scrollbar--shown-at) ps/scrollbar-hide-delay))
          (ps/scrollbar--hide)
        (ps/scrollbar--snap-back)))))

(defun ps/scrollbar--on-size-change (frame)
  "Hide the pill only when a real (non-pill) FRAME's pixel size changes.
`window-size-change-functions' also fires merely from creating/showing our
own pill, so we compare FRAME's outer size to its last known size and act
only on a genuine resize.  Ignored while `--busy' and for our own frames."
  (unless (or ps/scrollbar--busy (ps/scrollbar--own-frame-p frame))
    (let ((size (cons (frame-pixel-width frame) (frame-pixel-height frame)))
          (prev (frame-parameter frame 'ps/scrollbar--last-size)))
      (unless (equal size prev)
        (set-frame-parameter frame 'ps/scrollbar--last-size size)
        (when prev (ps/scrollbar--hide))))))

;;; Enable / disable

(defun ps/scrollbar--delete-all-frames ()
  "Delete every cached pill frame."
  (maphash (lambda (_parent child)
             (when (frame-live-p child) (delete-frame child)))
           ps/scrollbar--frames)
  (clrhash ps/scrollbar--frames))

(defun ps/scrollbar--on-delete-frame (frame)
  "Drop cache entries when FRAME (a parent or a pill) is deleted."
  (let ((child (gethash frame ps/scrollbar--frames)))
    (when child
      (remhash frame ps/scrollbar--frames)
      (when (frame-live-p child) (delete-frame child))))
  (maphash (lambda (parent child)
             (when (eq child frame) (remhash parent ps/scrollbar--frames)))
           ps/scrollbar--frames))

(defun ps/scrollbar--enable ()
  (setq frame-resize-pixelwise t)
  (when (eq ps/scrollbar--saved-right-fringe 'unset)
    (setq ps/scrollbar--saved-right-fringe
          (cdr (assq 'right-fringe default-frame-alist))))
  (modify-all-frames-parameters
   (list (cons 'right-fringe ps/scrollbar-fringe-width)))
  (add-hook 'delete-frame-functions #'ps/scrollbar--on-delete-frame)
  (add-hook 'window-size-change-functions #'ps/scrollbar--on-size-change)
  (setq ps/scrollbar--last-sig nil)
  (when ps/scrollbar--timer (cancel-timer ps/scrollbar--timer))
  (setq ps/scrollbar--timer
        (run-with-timer ps/scrollbar-tick-interval ps/scrollbar-tick-interval
                        #'ps/scrollbar--tick)))

(defun ps/scrollbar--disable ()
  (when ps/scrollbar--timer
    (cancel-timer ps/scrollbar--timer)
    (setq ps/scrollbar--timer nil))
  (remove-hook 'delete-frame-functions #'ps/scrollbar--on-delete-frame)
  (remove-hook 'window-size-change-functions #'ps/scrollbar--on-size-change)
  (ps/scrollbar--delete-all-frames)
  (unless (eq ps/scrollbar--saved-right-fringe 'unset)
    (modify-all-frames-parameters
     (list (cons 'right-fringe ps/scrollbar--saved-right-fringe)))
    (setq ps/scrollbar--saved-right-fringe 'unset))
  (setq ps/scrollbar--visible-frame nil
        ps/scrollbar--last-sig nil))

;;;###autoload
(define-minor-mode ps/scrollbar-mode
  "Global minor mode: an auto-hiding scroll-position indicator pill."
  :global t
  :group 'ps-scrollbar
  (if ps/scrollbar-mode
      (ps/scrollbar--enable)
    (ps/scrollbar--disable)))

(provide 'ps-scrollbar)
;;; ps-scrollbar.el ends here
