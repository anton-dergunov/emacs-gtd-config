;;; ps-scrollbar.el --- Auto-hiding, theme-colored child-frame scrollbar -*- lexical-binding: t; -*-

;;; Commentary:
;; A custom, auto-hiding scrollbar drawn as a single floating child frame
;; containing one contiguous SVG rounded pill.
;;
;; Why a child frame rather than the fringe: on the macOS GNU Emacs NS (Cocoa)
;; build the native scroll bar ignores the `scroll-bar' face (can't be recolored
;; or auto-hidden).  An earlier fringe-bitmap version drew the thumb per screen
;; row, which (a) segmented on soft-wrapped lines -- a fringe bitmap only renders
;; on the first screen row of a logical line -- and (b) perturbed layout at wrap
;; boundaries (text reflow), and (c) was slow (per-row overlay churn + forced
;; `window-end' redisplay every timer tick).  Drawing the thumb as one image in a
;; floating frame fixes all three: nothing touches buffer text, the pill is
;; contiguous, and rendering is cheap.
;;
;; Design notes:
;; - ONE child frame per parent OS frame, cached in `ps/scrollbar--frames' and
;;   never reparented (reparenting is the worst NS flicker source).
;; - NS flicker comes from frame-PARAMETER changes (size/position/visibility),
;;   not from swapping the buffer's image.  So frame geometry is only touched
;;   when the target window or its geometry changes; during plain scrolling we
;;   only swap the SVG image.
;; - All work runs from one low-frequency timer, never a redisplay hook.
;; - `(window-end window)' is read WITHOUT the update flag (the update flag forces
;;   a redisplay); when it returns nil we skip the tick.

;;; Code:

(require 'svg)
(require 'cl-lib)

;;; State

(defvar ps/scrollbar-mode)              ; defined by `define-minor-mode' below

(defvar ps/scrollbar--drag-map
  (let ((map (make-sparse-keymap)))
    (define-key map [down-mouse-1] #'ps/scrollbar--start-drag)
    (define-key map [drag-mouse-1] #'ignore)
    (define-key map [mouse-1] #'ignore)
    map)
  "Keymap of the scrollbar child-frame buffer.")

(defvar ps/scrollbar--frames (make-hash-table :test 'eq)
  "Hash of parent frame -> its scrollbar child frame.")
(defvar ps/scrollbar--timer nil)
(defvar ps/scrollbar--visible-frame nil
  "The child frame currently visible, or nil.")
(defvar ps/scrollbar--target nil
  "The window the thumb is currently attached to (used by drag).")
(defvar ps/scrollbar--last-sig nil
  "Signature of the last render, to skip redundant work.")
(defvar ps/scrollbar--shown-at nil
  "`float-time' of the last scroll/hover activity.")
(defvar ps/scrollbar--last-sel-start nil
  "Selected window's `window-start' at the previous tick (scroll detection).")
(defvar ps/scrollbar--svg-p nil
  "Non-nil when SVG images are available (set at mode enable).")
(defvar ps/scrollbar--saved-right-fringe 'unset
  "Prior `default-frame-alist' right-fringe, restored on disable.")

;;; Customization

(defgroup ps-scrollbar nil
  "Auto-hiding, theme-colored child-frame scrollbar."
  :group 'ps)

(defcustom ps/scrollbar-width 8
  "Width of the thumb pill in pixels (clamped to the reserved right fringe)."
  :type 'integer :group 'ps-scrollbar)

(defcustom ps/scrollbar-fringe-width 10
  "Right fringe width reserved (in pixels) so the pill never overlaps text.
Should be >= `ps/scrollbar-width'."
  :type 'integer :group 'ps-scrollbar)

(defcustom ps/scrollbar-hide-delay 1.0
  "Seconds of inactivity (no scroll, no hover) before the thumb fades."
  :type 'number :group 'ps-scrollbar)

(defcustom ps/scrollbar-show-on-hover t
  "When non-nil, reveal the thumb whenever the mouse is over a window."
  :type 'boolean :group 'ps-scrollbar)

(defcustom ps/scrollbar-min-thumb-pixels 24
  "Minimum thumb height in pixels."
  :type 'integer :group 'ps-scrollbar)

(defcustom ps/scrollbar-tick-interval 0.12
  "Seconds between scrollbar refresh ticks."
  :type 'number :group 'ps-scrollbar)

(defcustom ps/scrollbar-exclude-modes '(treemacs-mode which-key-mode)
  "Major modes whose windows never get a scrollbar.
Terminal modes (e.g. `eat-mode') are deliberately not excluded."
  :type '(repeat symbol) :group 'ps-scrollbar)

(defface ps/scrollbar-thumb
  '((t :inherit shadow))
  "Face whose foreground colors the thumb.  Inherits `shadow' (theme-aware)."
  :group 'ps-scrollbar)

(defface ps/scrollbar-thumb-active
  '((t :inherit (shadow bold)))
  "Face for the thumb while the window is hovered or the thumb is dragged."
  :group 'ps-scrollbar)

;;; Pure geometry (unit-tested)

(defun ps/scrollbar--thumb-span (pmin pmax wstart wend strip-h min-h)
  "Return (Y . H) thumb pixels within a STRIP-H tall strip, or nil if content fits.
PMIN/PMAX are buffer bounds, WSTART/WEND the window's visible char range,
MIN-H the minimum thumb height in pixels."
  (let ((total (- pmax pmin)))
    (when (and (> total 0) (> strip-h 0)
               (or (> wstart pmin) (< wend pmax)))
      (let* ((top-frac (/ (float (- wstart pmin)) total))
             (size-frac (/ (float (- wend wstart)) total))
             (h (min strip-h (max min-h (round (* size-frac strip-h)))))
             (y (max 0 (min (round (* top-frac strip-h)) (- strip-h h)))))
        (cons y h)))))

(defun ps/scrollbar--drag-frac (y abs-top strip-h thumb-h offset)
  "Map absolute cursor Y to a [0,1] scroll fraction over the thumb travel.
ABS-TOP is the window text area's absolute screen top, STRIP-H its pixel
height, THUMB-H the thumb height, OFFSET the grab offset within the thumb."
  (let ((travel (- strip-h thumb-h)))
    (if (<= travel 0)
        0.0
      (max 0.0 (min 1.0 (/ (float (- y abs-top offset)) travel))))))

;;; Colors / image

(defun ps/scrollbar--color (active)
  "Resolve the thumb fill color for ACTIVE state."
  (or (face-foreground (if active 'ps/scrollbar-thumb-active 'ps/scrollbar-thumb)
                       nil t)
      "gray60"))

(defun ps/scrollbar--image (w h y th color)
  "Return a fresh SVG image: a W*H transparent canvas with a rounded pill.
The pill is COLOR-filled, TH tall, starting at Y, spanning the full width W."
  (let ((svg (svg-create w h)))
    (svg-rectangle svg 0 y w th
                   :rx (/ w 2.0) :ry (/ w 2.0)
                   :fill color)
    (svg-image svg :ascent 'center :margin 0)))

;;; Child frame

(defun ps/scrollbar--buffer ()
  "Return the shared scrollbar child-frame buffer, creating it if needed."
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
                    show-trailing-whitespace nil
                    buffer-read-only nil)
        (use-local-map ps/scrollbar--drag-map)
        (current-buffer))))

(defun ps/scrollbar--make-frame (parent)
  "Create the invisible scrollbar child frame parented to PARENT."
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
                   (background-color . ,(frame-parameter parent 'background-color))
                   (width . 1) (height . 1)
                   (skip-taskbar . t)
                   (desktop-dont-save . t)))))
    (set-window-buffer (frame-root-window frame) buf)
    (set-window-dedicated-p (frame-root-window frame) t)
    frame))

(defun ps/scrollbar--ensure-frame (parent)
  "Return PARENT's scrollbar child frame, creating it if needed."
  (let ((child (gethash parent ps/scrollbar--frames)))
    (unless (and child (frame-live-p child))
      (setq child (ps/scrollbar--make-frame parent))
      (puthash parent child ps/scrollbar--frames))
    child))

(defun ps/scrollbar--own-frame-p (frame)
  "Non-nil if FRAME is one of our scrollbar child frames."
  (let (found)
    (maphash (lambda (_p c) (when (eq c frame) (setq found t)))
             ps/scrollbar--frames)
    found))

(defun ps/scrollbar--show-image (image)
  "Display IMAGE (or clear) as the sole content of the scrollbar buffer."
  (with-current-buffer (ps/scrollbar--buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (when image (insert-image image)))))

(defun ps/scrollbar--set-geom (child left top width height)
  "Reposition/resize CHILD only when its geometry actually changed."
  (let ((cur (frame-parameter child 'ps/scrollbar-geom))
        (new (list left top width height)))
    (unless (equal cur new)
      (set-frame-size child width height t)
      (set-frame-position child left top)
      (set-frame-parameter child 'ps/scrollbar-geom new))))

(defun ps/scrollbar--reveal (child)
  "Make CHILD the single visible scrollbar frame."
  (unless (eq ps/scrollbar--visible-frame child)
    (when (and ps/scrollbar--visible-frame
               (frame-live-p ps/scrollbar--visible-frame))
      (make-frame-invisible ps/scrollbar--visible-frame))
    (setq ps/scrollbar--visible-frame child))
  (unless (frame-visible-p child)
    (make-frame-visible child)))

(defun ps/scrollbar--hide ()
  "Hide the scrollbar."
  (when (and ps/scrollbar--visible-frame
             (frame-live-p ps/scrollbar--visible-frame))
    (make-frame-invisible ps/scrollbar--visible-frame))
  (setq ps/scrollbar--visible-frame nil
        ps/scrollbar--last-sig nil
        ps/scrollbar--target nil))

;;; Rendering

(defun ps/scrollbar--render (window active)
  "Render the thumb for WINDOW.
Return `skip' when `window-end' is not yet known, `same' when nothing
changed, `hidden' when content fits, `rendered' otherwise."
  (let ((parent (window-frame window))
        (wend (window-end window)))      ; no update flag (cheap)
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
                              (window-pixel-width window) active)))
        (if (equal sig ps/scrollbar--last-sig)
            'same
          (setq ps/scrollbar--last-sig sig
                ps/scrollbar--target window)
          (let ((span (ps/scrollbar--thumb-span
                       pmin pmax wstart wend strip-h
                       ps/scrollbar-min-thumb-pixels)))
            (if (null span)
                (progn (ps/scrollbar--hide) 'hidden)
              (pcase-let* ((`(,y . ,th) span)
                           (ibw (or (frame-parameter parent 'internal-border-width) 0))
                           (rfringe (nth 1 (window-fringes window)))
                           (strip-w (max 2 (min ps/scrollbar-width rfringe)))
                           (left (+ ibw (- (+ (window-pixel-left window)
                                              (window-pixel-width window))
                                           strip-w)))
                           (top (+ ibw iT))
                           (color (ps/scrollbar--color active))
                           (child (ps/scrollbar--ensure-frame parent)))
                ;; Keep the strip blended with the (possibly re-themed) background.
                (let ((bg (frame-parameter parent 'background-color)))
                  (unless (equal bg (frame-parameter child 'background-color))
                    (set-frame-parameter child 'background-color bg)))
                (if ps/scrollbar--svg-p
                    (progn
                      (ps/scrollbar--show-image
                       (ps/scrollbar--image strip-w strip-h y th color))
                      (ps/scrollbar--set-geom child left top strip-w strip-h))
                  ;; No-SVG fallback: the frame itself is the pill.
                  (ps/scrollbar--show-image nil)
                  (set-frame-parameter child 'background-color color)
                  (ps/scrollbar--set-geom child left (+ top y) strip-w th))
                (force-window-update (frame-root-window child))
                (ps/scrollbar--reveal child)
                'rendered))))))))

;;; Lifecycle / detection

(defun ps/scrollbar--candidate-window-p (window)
  "Non-nil if WINDOW should display a scrollbar."
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

(defun ps/scrollbar--tick ()
  "Refresh the thumb: reveal on hover/scroll, fade after the idle delay."
  (when ps/scrollbar-mode
    (condition-case err
        (ps/scrollbar--tick-1)
      (error (message "ps/scrollbar: %S" err)))))

(defun ps/scrollbar--tick-1 ()
  (let* ((now (float-time))
         (sel (selected-window))
         (sel-start (and (window-live-p sel) (window-start sel)))
         (scrolled (and sel-start
                        (not (eql sel-start ps/scrollbar--last-sel-start))))
         (hovered (and ps/scrollbar-show-on-hover (ps/scrollbar--mouse-window)))
         target active)
    (setq ps/scrollbar--last-sel-start sel-start)
    (cond
     ((and hovered (ps/scrollbar--candidate-window-p hovered))
      (setq target hovered active t))
     ((and scrolled (ps/scrollbar--candidate-window-p sel))
      (setq target sel active nil)))
    (if (and target (display-graphic-p (window-frame target)))
        (let ((res (ps/scrollbar--render target active)))
          (unless (eq res 'skip)
            (setq ps/scrollbar--shown-at now)))
      (when (and ps/scrollbar--visible-frame
                 ps/scrollbar--shown-at
                 (> (- now ps/scrollbar--shown-at) ps/scrollbar-hide-delay))
        (ps/scrollbar--hide)))))

;;; Click-drag (event arrives in the child-frame buffer)

(defun ps/scrollbar--scroll-to (window pmin pmax frac)
  "Scroll WINDOW so FRAC (0..1) of its buffer is above the top."
  (let ((target (+ pmin (floor (* frac (- pmax pmin))))))
    (with-current-buffer (window-buffer window)
      (set-window-start
       window
       (save-excursion (goto-char (max pmin (min pmax target)))
                       (line-beginning-position))))))

(defun ps/scrollbar--start-drag (_event)
  "Drag the thumb to scroll its window.  Bound in the child-frame buffer."
  (interactive "e")
  (let ((window ps/scrollbar--target))
    (when (window-live-p window)
      (pcase-let* ((parent (window-frame window))
                   (`(,_iL ,iT ,_iR ,iB) (window-inside-pixel-edges window))
                   (`(,_fL ,fT ,_fR ,_fB) (frame-edges parent 'native))
                   (ibw (or (frame-parameter parent 'internal-border-width) 0))
                   (strip-h (- iB iT))
                   (abs-top (+ fT ibw iT))
                   (buf (window-buffer window))
                   (pmin (with-current-buffer buf (point-min)))
                   (pmax (with-current-buffer buf (point-max)))
                   (span (ps/scrollbar--thumb-span
                          pmin pmax (window-start window) (window-end window)
                          strip-h ps/scrollbar-min-thumb-pixels))
                   (thumb-h (if span (cdr span) strip-h))
                   (thumb-y (if span (car span) 0))
                   (y0 (cdr (mouse-absolute-pixel-position)))
                   (local (- y0 abs-top))
                   (offset (if (and (>= local thumb-y) (<= local (+ thumb-y thumb-h)))
                               (- local thumb-y)
                             (/ thumb-h 2))))
        (track-mouse
          (while (mouse-movement-p (read-event))
            (let* ((y (cdr (mouse-absolute-pixel-position)))
                   (frac (ps/scrollbar--drag-frac y abs-top strip-h thumb-h offset)))
              (ps/scrollbar--scroll-to window pmin pmax frac)
              (ps/scrollbar--render window t))))))))

;;; Enable / disable

(defun ps/scrollbar--delete-all-frames ()
  "Delete every cached child frame."
  (maphash (lambda (_parent child)
             (when (frame-live-p child) (delete-frame child)))
           ps/scrollbar--frames)
  (clrhash ps/scrollbar--frames))

(defun ps/scrollbar--on-delete-frame (frame)
  "Drop cache entries when FRAME (a parent or a child) is deleted."
  (let ((child (gethash frame ps/scrollbar--frames)))
    (when child
      (remhash frame ps/scrollbar--frames)
      (when (frame-live-p child) (delete-frame child))))
  (maphash (lambda (parent child)
             (when (eq child frame) (remhash parent ps/scrollbar--frames)))
           ps/scrollbar--frames))

(defun ps/scrollbar--enable ()
  (setq ps/scrollbar--svg-p (image-type-available-p 'svg)
        frame-resize-pixelwise t)
  (when (eq ps/scrollbar--saved-right-fringe 'unset)
    (setq ps/scrollbar--saved-right-fringe
          (cdr (assq 'right-fringe default-frame-alist))))
  (modify-all-frames-parameters
   (list (cons 'right-fringe ps/scrollbar-fringe-width)))
  (add-hook 'delete-frame-functions #'ps/scrollbar--on-delete-frame)
  (setq ps/scrollbar--last-sel-start nil
        ps/scrollbar--last-sig nil)
  (when ps/scrollbar--timer (cancel-timer ps/scrollbar--timer))
  (setq ps/scrollbar--timer
        (run-with-timer ps/scrollbar-tick-interval ps/scrollbar-tick-interval
                        #'ps/scrollbar--tick)))

(defun ps/scrollbar--disable ()
  (when ps/scrollbar--timer
    (cancel-timer ps/scrollbar--timer)
    (setq ps/scrollbar--timer nil))
  (remove-hook 'delete-frame-functions #'ps/scrollbar--on-delete-frame)
  (ps/scrollbar--delete-all-frames)
  (unless (eq ps/scrollbar--saved-right-fringe 'unset)
    (modify-all-frames-parameters
     (list (cons 'right-fringe ps/scrollbar--saved-right-fringe)))
    (setq ps/scrollbar--saved-right-fringe 'unset))
  (setq ps/scrollbar--visible-frame nil
        ps/scrollbar--last-sig nil
        ps/scrollbar--target nil))

;;;###autoload
(define-minor-mode ps/scrollbar-mode
  "Global minor mode: an auto-hiding, theme-colored child-frame scrollbar."
  :global t
  :group 'ps-scrollbar
  (if ps/scrollbar-mode
      (ps/scrollbar--enable)
    (ps/scrollbar--disable)))

(provide 'ps-scrollbar)
;;; ps-scrollbar.el ends here
