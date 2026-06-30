;;; ps-scrollbar.el --- Auto-hiding scroll-position indicator -*- lexical-binding: t; -*-

;;; Commentary:
;; A minimal, auto-hiding scroll-position indicator: a small coloured "pill" at
;; the right edge of a window that appears while you scroll and fades when idle.
;; It is not draggable (macOS would not let this borderless child frame stay
;; put under a drag, see below) -- instead, click anywhere in the track to
;; jump there, or scroll with the wheel/trackpad/keyboard (smooth via
;; ultra-scroll).  The native scroll bar is disabled in config.org because the
;; macOS NS toolkit ignores its face (it can't be themed or auto-hidden).
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
(defvar ps/scrollbar--ducked-at nil
  "`float-time' the pill was last ducked (see `ps/scrollbar--duck'), or nil.")
(defvar ps/scrollbar--fade-start nil
  "`float-time' when the current fade began, or nil (not fading).")
(defvar ps/scrollbar--fade-from nil
  "Starting color (hex string) of the current fade.")
(defvar ps/scrollbar--fade-to nil
  "Target color (hex string) of the current fade (parent's background).")

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

(defcustom ps/scrollbar-show-on-hover t
  "When non-nil, also reveal the pill while the mouse is over its track.
Scoped to the scrollbar's own strip, not the whole window, so the bar does
not turn into a permanent fixture -- on by default so the strip is visible
to interact with even when you have not just scrolled."
  :type 'boolean :group 'ps-scrollbar)

(defcustom ps/scrollbar-min-thumb-pixels 24
  "Minimum pill height in pixels."
  :type 'integer :group 'ps-scrollbar)

(defcustom ps/scrollbar-tick-interval 0.05
  "Seconds between refresh ticks (scroll/hover detection, snap-back).
The pill's position only updates on a tick, so this is the upper bound on
the lag between scrolling and the thumb visually catching up. Lower is
snappier; each tick is cheap when nothing changed, so this can go fairly
low without a real cost."
  :type 'number :group 'ps-scrollbar)

(defcustom ps/scrollbar-exclude-modes '(treemacs-mode which-key-mode)
  "Major modes whose windows never get a pill."
  :type '(repeat symbol) :group 'ps-scrollbar)

(defcustom ps/scrollbar-click-to-scroll t
  "When non-nil, clicking anywhere in the scrollbar track jumps there,
centering the thumb on the click point.  A quick kill switch in case click
handling misbehaves in some configuration."
  :type 'boolean :group 'ps-scrollbar)

(defcustom ps/scrollbar-fade-duration 0.25
  "Seconds over which the pill fades out after `ps/scrollbar-hide-delay'.
Set to 0 for the old abrupt-hide behaviour."
  :type 'number :group 'ps-scrollbar)

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

(defun ps/scrollbar--frac-to-start (pmin pmax size-frac center-frac)
  "Buffer position whose window-start centers the thumb at CENTER-FRAC of the
track, given the thumb's SIZE-FRAC (fraction of the buffer visible at once).
The mathematical inverse of `ps/scrollbar--thumb-span''s forward geometry, so
clicking the rendered thumb's exact center round-trips to the window's
current start."
  (let* ((top-frac (max 0.0 (min (- 1.0 size-frac) (- center-frac (/ size-frac 2.0)))))
         (total (- pmax pmin)))
    (+ pmin (round (* top-frac total)))))

(defun ps/scrollbar--in-strip-p (px py rect)
  "Non-nil if pixel point PX,PY falls within RECT (LEFT TOP RIGHT BOTTOM)."
  (pcase-let ((`(,l ,top ,r ,b) rect))
    (and (>= px l) (< px r) (>= py top) (< py b))))

(defun ps/scrollbar--strip-rect-1 (wr br bt pl pt it ib)
  "Pure geometry behind `ps/scrollbar--strip-rect': a window's scrollbar
track as (LEFT TOP RIGHT BOTTOM), in pixels relative to its frame's native
origin.  WR/BR/BT are the window's (basic and body) right/bottom edges and
PL/PT the parent frame's native origin, all in absolute screen pixels; IT/IB
are the window's inside (text-area) top/bottom, already frame-relative."
  (let* ((strip-w (max 2 (- wr br)))
         (left (- br pl))
         (top (- bt pt)))
    (list left top (+ left strip-w) (+ top (- ib it)))))

(defun ps/scrollbar--color ()
  "Resolve the pill colour from `ps/scrollbar-thumb'."
  (or (face-foreground 'ps/scrollbar-thumb nil t) "gray60"))

(defun ps/scrollbar--hex-to-rgb (color)
  "Parse a #rrggbb COLOR string to a (r g b) list of [0,1] floats.
Returns nil if COLOR is not a well-formed 6-digit hex color string.
Does not call `x-color-values' so it works correctly in batch mode."
  (when (and (stringp color)
             (string-match
              "^#\\([0-9a-fA-F]\\{2\\}\\)\\([0-9a-fA-F]\\{2\\}\\)\\([0-9a-fA-F]\\{2\\}\\)$"
              color))
    (list (/ (string-to-number (match-string 1 color) 16) 255.0)
          (/ (string-to-number (match-string 2 color) 16) 255.0)
          (/ (string-to-number (match-string 3 color) 16) 255.0))))

(defun ps/scrollbar--lerp-color (from to frac)
  "Linearly interpolate between #rrggbb hex colors FROM and TO at FRAC (0.0–1.0).
FRAC is clamped to [0,1].  Returns a #rrggbb hex string.
Parses hex directly, without `x-color-values', so it works in batch mode."
  (let* ((f (or (ps/scrollbar--hex-to-rgb from) '(0.0 0.0 0.0)))
         (t- (or (ps/scrollbar--hex-to-rgb to)  '(1.0 1.0 1.0)))
         (frac (max 0.0 (min 1.0 frac)))
         (r (+ (nth 0 f) (* frac (- (nth 0 t-) (nth 0 f)))))
         (g (+ (nth 1 f) (* frac (- (nth 1 t-) (nth 1 f)))))
         (b (+ (nth 2 f) (* frac (- (nth 2 t-) (nth 2 f))))))
    (format "#%02x%02x%02x"
            (max 0 (min 255 (round (* r 255))))
            (max 0 (min 255 (round (* g 255))))
            (max 0 (min 255 (round (* b 255)))))))

;;; Child frame

(defun ps/scrollbar--forward-wheel (event)
  "Forward a wheel/touch EVENT received by the pill frame to the real window
beneath it.  The pill is a separate OS-level window, so the system delivers
scroll input to it instead of the parent when the pointer is over the thumb;
without this, scrolling appears to do nothing while the cursor is on the pill.
Rewrites EVENT's window in place and re-dispatches via whatever command is
actually bound to it in the target window (`mwheel-scroll' or
`pixel-scroll-precision', without hardcoding either).  Re-injecting EVENT via
`unread-command-events' instead and letting Emacs's own command loop dispatch
it was tried and made the real window's mode-line selection state get stuck
wrong more often, so this drives the scroll directly and pins the selection
itself instead."
  (interactive "e")
  (let ((target (frame-parameter (selected-frame) 'ps/scrollbar-window)))
    (when (and (window-live-p target) (consp event) (consp (nth 1 event)))
      ;; Dispatching this command at all means Emacs already selected the
      ;; pill's own window to look up its local map.  Select TARGET
      ;; unconditionally and leave it selected -- it is in practice already
      ;; the window the user is interacting with (the pill never steals
      ;; OS-level keyboard focus), so this just pins down what would
      ;; otherwise already be true, rather than relying on a temporary
      ;; selection (e.g. via `with-selected-window') that reverts to
      ;; whatever Emacs happened to select first, which is not reliable here.
      (let ((inhibit-redisplay t))
        (setcar (nth 1 event) target)
        (select-window target)
        (let* ((cmd (key-binding (vector (event-basic-type event)) t nil (nth 1 event)))
               (last-input-event event))
          (when (commandp cmd)
            (call-interactively cmd)))))))

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
        (let ((map (make-sparse-keymap)))
          (dolist (type '(wheel-up wheel-down wheel-left wheel-right touch-end))
            (define-key map (vector type) #'ps/scrollbar--forward-wheel))
          (define-key map [down-mouse-1] #'ps/scrollbar--click-on-pill)
          ;; The matching release is never consumed by us, since
          ;; `ps/scrollbar--click-on-pill' reacts to the press and returns
          ;; immediately rather than running its own `track-mouse' loop like
          ;; `mouse-drag-region' (the usual `down-mouse-1' binding) does.  By
          ;; the time it fires, we have already repositioned the window
          ;; underneath, so it falls through to Emacs's default handling
          ;; against a now-different context -- and even a sub-pixel jitter
          ;; between press and release reads as a drag there, silently
          ;; setting the mark.  Swallow both event shapes explicitly.
          (define-key map [mouse-1] #'ignore)
          (define-key map [drag-mouse-1] #'ignore)
          (use-local-map map)
          ;; `pixel-scroll-precision-mode-map' is a minor-mode map and outranks
          ;; a plain local map, so it must be beaten the same way.
          (setq-local minor-mode-overriding-map-alist
                      (list (cons 'pixel-scroll-precision-mode map))))
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

(defun ps/scrollbar--fade-cancel ()
  "Cancel an in-progress fade.
Clears the fade state and invalidates the last render signature so the
next render always does a full pass (restoring the full thumb colour)."
  (setq ps/scrollbar--fade-start nil
        ps/scrollbar--fade-from nil
        ps/scrollbar--fade-to nil
        ps/scrollbar--last-sig nil))

(defun ps/scrollbar--hide-now ()
  "Instantly hide the pill by destroying its frame.
Used when the pill must vanish immediately: window resize, mode disable,
content fits in view, or fade completion.  Normal idle hide uses
`ps/scrollbar--hide' instead (which fades first)."
  (ps/scrollbar--fade-cancel)
  (when (and ps/scrollbar--visible-frame
             (frame-live-p ps/scrollbar--visible-frame))
    (ps/scrollbar--destroy ps/scrollbar--visible-frame))
  (setq ps/scrollbar--visible-frame nil
        ps/scrollbar--last-sig nil))

(defun ps/scrollbar--hide ()
  "Begin fading the pill out (or hide it instantly if fade is disabled).
Does nothing if a fade is already in progress."
  (cond
   ;; No frame to fade, or fade disabled: instant hide.
   ((or (zerop ps/scrollbar-fade-duration)
        (null ps/scrollbar--visible-frame)
        (not (frame-live-p ps/scrollbar--visible-frame)))
    (ps/scrollbar--hide-now))
   ;; Already fading: let the existing fade run.
   (ps/scrollbar--fade-start nil)
   ;; Start a new fade.
   (t
    (let* ((child ps/scrollbar--visible-frame)
           (from (or (frame-parameter child 'background-color)
                     (ps/scrollbar--color)))
           (parent (frame-parent child))
           (to (if (and parent (frame-live-p parent))
                   (or (frame-parameter parent 'background-color)
                       (face-background 'default nil t)
                       "white")
                 "white")))
      (setq ps/scrollbar--fade-start (float-time)
            ps/scrollbar--fade-from from
            ps/scrollbar--fade-to to)))))

(defun ps/scrollbar--duck ()
  "Make the visible pill instantly invisible without destroying its frame.
Used only to get the pill out of the way of an input event it just caught
\(see `ps/scrollbar--forward-wheel'): destroying and recreating a native
window on every wheel tick was visibly heavier on this NS build than just
unmapping the same one, which is what caused the mode-line flicker `--hide'
produced when called at that frequency.  The next render forces a fresh
position/colour application and an explicit redisplay, which is relied on to
avoid the \"visible but not painting\" failure a hidden-then-shown child frame
can hit here (see `ps/scrollbar--destroy')."
  (when (and ps/scrollbar--visible-frame
             (frame-live-p ps/scrollbar--visible-frame))
    (let ((ps/scrollbar--busy t))
      (make-frame-invisible ps/scrollbar--visible-frame)))
  (setq ps/scrollbar--visible-frame nil
        ps/scrollbar--last-sig nil
        ps/scrollbar--ducked-at (float-time))
  (ps/scrollbar--fade-cancel))

;;; Rendering

(defun ps/scrollbar--strip-rect (window)
  "Return WINDOW's scrollbar track as (LEFT TOP RIGHT BOTTOM), in pixels
relative to its frame's native origin -- the same coordinate space as
`mouse-pixel-position' and the pill child frame's own position.  Used both
to position the pill and, by `ps/scrollbar--strip-hover-window', to test
whether the mouse is within the track."
  (let ((parent (window-frame window)))
    (pcase-let* ((`(,_iL ,it ,_iR ,ib) (window-inside-pixel-edges window))
                 (`(,_wl ,_wt ,wr ,_wb) (window-edges window nil t t))
                 (`(,_bl ,bt ,br ,_bb)  (window-edges window t   t t))
                 (`(,pl ,pt ,_pr ,_pb)  (frame-edges parent 'native-edges)))
      (ps/scrollbar--strip-rect-1 wr br bt pl pt it ib))))

(defun ps/scrollbar--render (window)
  "Show/position the pill for WINDOW.
Return `skip' (window-end unknown), `same' (nothing changed), `hidden'
\(content fits) or `rendered'."
  (let ((parent (window-frame window))
        (wend (window-end window))             ; no update flag (cheap)
        ;; Creating/showing our own child frame can silently select it as a
        ;; side effect of frame creation; nothing else here re-selects
        ;; afterward, which left whatever window this is called for (often
        ;; not the one the user is actually looking at, e.g. a hover on a
        ;; window they're not editing) looking "unfocused" -- dimmed
        ;; mode-line, outline-only org-modern TODO/tag pills -- for as long
        ;; as the pill stayed visible.  Always restore the prior selection.
        (orig-window (selected-window)))
    (if (null wend)
        'skip
      (unwind-protect
          (ps/scrollbar--render-1 window parent wend)
        (unless (eq (selected-window) orig-window)
          (select-window orig-window 'norecord))))))

(defun ps/scrollbar--render-1 (window parent wend)
  "Body of `ps/scrollbar--render', factored out so the selection-restoring
`unwind-protect' there can wrap it cleanly."
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
            (progn (ps/scrollbar--hide-now) 'hidden)
          (pcase-let* ((`(,thumb-y . ,thumb-h) span)
                       ;; The pill sits over the window's right fringe; its
                       ;; position is PARENT-RELATIVE, matching the track
                       ;; rect's own coordinate space.
                       (`(,strip-l ,strip-t ,strip-r ,_strip-b)
                        (ps/scrollbar--strip-rect window))
                       (strip-w (max 2 (- strip-r strip-l)))
                       ;; Width as a fraction of the actual fringe width, so
                       ;; it scales with the display.
                       (pill-w (min strip-w
                                    (max 2 (round (* strip-w
                                                     (/ (float ps/scrollbar-width)
                                                        (max 1 ps/scrollbar-fringe-width)))))))
                       (pill-x (max 0 (round (/ (- strip-w pill-w) 2.0))))
                       (fleft (+ strip-l pill-x))
                       (ftop (+ strip-t thumb-y))
                       (color (ps/scrollbar--color)))
            (let* ((ps/scrollbar--busy t)
                   (child (ps/scrollbar--ensure-frame parent))
                   (geom (list fleft ftop pill-w thumb-h)))
              ;; Always refreshed (cheap): which real window the pill
              ;; currently represents, read by `ps/scrollbar--forward-wheel'.
              (set-frame-parameter child 'ps/scrollbar-window window)
              (unless (equal color (frame-parameter child 'background-color))
                (set-frame-parameter child 'background-color color))
              (unless (equal geom (frame-parameter child 'ps/scrollbar-geom))
                (set-frame-parameter child 'ps/scrollbar-geom geom)
                (set-frame-size child pill-w thumb-h t)
                (set-frame-position child fleft ftop))
              ;; If we were ducked out of a forwarded wheel event's way,
              ;; stay invisible for a beat before the tick loop re-shows
              ;; the pill at its (now updated) position.
              (if (and ps/scrollbar--ducked-at
                       (< (- (float-time) ps/scrollbar--ducked-at)
                          (* 1.5 ps/scrollbar-tick-interval)))
                  (setq ps/scrollbar--visible-frame nil)
                (ps/scrollbar--reveal child))
              ;; `ps/scrollbar--ensure-frame' may have just created a fresh
              ;; child frame above (e.g. after the previous one was ducked or
              ;; destroyed), which can itself select that frame as a side
              ;; effect of `make-frame'.  The caller's `unwind-protect' only
              ;; restores the right selection once this function returns,
              ;; which is too late for THIS explicit redisplay -- force it
              ;; back before forcing the paint, or this one paints with the
              ;; wrong window selected (dimmed mode-line, outline-only
              ;; org-modern pills elsewhere in the parent's buffer).
              (unless (eq (selected-window) window)
                (select-window window 'norecord))
              (redisplay t))
            'rendered))))))

(defun ps/scrollbar--reposition (window click-y)
  "Scroll WINDOW so its thumb centers on CLICK-Y (pixels, relative to the top
of WINDOW's own scrollbar track -- see `ps/scrollbar--strip-rect')."
  (when (window-live-p window)
    (pcase-let* ((`(,_iL ,it ,_iR ,ib) (window-inside-pixel-edges window))
                 (strip-h (- ib it))
                 (buf (window-buffer window))
                 (pmin (with-current-buffer buf (point-min)))
                 (pmax (with-current-buffer buf (point-max)))
                 (wstart (window-start window))
                 (wend (or (window-end window t) pmax))
                 (total (- pmax pmin)))
      (when (and (> total 0) (> strip-h 0))
        (let* ((size-frac (/ (float (- wend wstart)) total))
               (center-frac (/ (float click-y) strip-h))
               ;; `--frac-to-start' is a linear interpolation over character
               ;; count, with no reason to land on a line boundary; starting
               ;; display mid-line shows only that line's tail and skips its
               ;; line-number, until a later redisplay snaps it back -- so
               ;; snap it ourselves first.
               (target (with-current-buffer buf
                         (save-excursion
                           (goto-char (ps/scrollbar--frac-to-start
                                       pmin pmax size-frac center-frac))
                           (line-beginning-position)))))
          ;; Move point along with the view: with the default NOFORCE=nil,
          ;; `set-window-start' still gets silently overridden by the next
          ;; redisplay whenever point would end up off-screen at the
          ;; requested start -- which it almost always would be for a
          ;; jump-to-click, since point is still wherever it was before the
          ;; click -- so point has to move into the new view too.
          (set-window-point window target)
          (set-window-start window target)
          ;; Force an immediate re-render so the pill snaps to the new
          ;; position now, rather than waiting for the next tick.
          (setq ps/scrollbar--last-sig nil)
          (when (display-graphic-p (window-frame window))
            (ps/scrollbar--render window)))))))

(defun ps/scrollbar--click-on-pill (event)
  "Handle a click on the pill frame itself by re-dispatching an equivalent
click against the parent frame's bare fringe (`ps/scrollbar--click-on-fringe',
via `unread-command-events'), rather than handling it here directly.  Driving
the reposition from inside the pill's own event dispatch -- even with the
real window explicitly selected throughout -- still left a transient
mis-rendering elsewhere in the parent (dimmed mode-line, outline-only
org-modern TODO/tag pills) that survived every selection fix tried; routing
through the fringe-click path, which is not subject to Emacs selecting a
*separate frame* to look up a local keymap in the first place, avoids the
mechanism rather than chasing its side effects.  No need to duck the pill
first: the re-dispatched click's position is synthesized to name the real
window directly (not resolved from the click's physical screen pixel), and
the reposition this triggers re-renders -- and so repositions -- this same
pill frame momentarily afterward anyway."
  (interactive "e")
  (when ps/scrollbar-click-to-scroll
    (let* ((frame (selected-frame))
           (window (frame-parameter frame 'ps/scrollbar-window))
           (geom (frame-parameter frame 'ps/scrollbar-geom)))
      (when (and (window-live-p window) geom)
        (pcase-let* ((`(,_fleft ,ftop ,_pill-w ,_thumb-h) geom)
                     (local-y (cdr (posn-x-y (event-start event))))
                     (`(,_l ,strip-t ,_r ,_b) (ps/scrollbar--strip-rect window))
                     (click-y (+ (- ftop strip-t) local-y))
                     (posn (list window 'right-fringe (cons 0 click-y) (float-time))))
          (push (list 'down-mouse-1 posn) unread-command-events))))))

(defun ps/scrollbar--click-on-fringe (event)
  "Handle a click on the parent frame's bare scrollbar fringe (the common
case, since the pill only covers the thumb's current pixels, not the whole
track): jump to the clicked position."
  (interactive "e")
  (when ps/scrollbar-click-to-scroll
    (let* ((posn (event-start event))
           (window (posn-window posn)))
      (when (and (window-live-p window) (ps/scrollbar--candidate-window-p window))
        (ps/scrollbar--reposition window (cdr (posn-x-y posn)))))))

(defun ps/scrollbar--click-on-vertical-line (event)
  "Handle down-mouse-1 in the vertical-line area near a scrollbar track.
On some platforms the vertical-line grab zone extends several pixels into the
adjacent right fringe, so clicks intended for the scrollbar track are
misclassified as window-resize attempts.  Use pixel-precise mouse position to
check against every scrollbar strip; if the click is inside one, reposition
that window; otherwise pass through to the standard window-resize handler
\(`mouse-drag-vertical-line')."
  (interactive "e")
  (let* ((ppos (mouse-pixel-position))
         (frame (car ppos)) (px (cadr ppos)) (py (cddr ppos))
         handled)
    (when (and ps/scrollbar-click-to-scroll
               (frame-live-p frame) (integerp px) (integerp py))
      (catch 'found
        (dolist (win (window-list frame))
          (when (ps/scrollbar--candidate-window-p win)
            (let ((rect (ps/scrollbar--strip-rect win)))
              (when (ps/scrollbar--in-strip-p px py rect)
                (ps/scrollbar--reposition win (- py (nth 1 rect)))
                (setq handled t)
                (throw 'found nil)))))))
    (unless handled
      (mouse-drag-vertical-line event))))

;;; Lifecycle / detection

(defun ps/scrollbar--candidate-window-p (window)
  "Non-nil if WINDOW should display a pill."
  (and (window-live-p window)
       (not (window-minibuffer-p window))
       (not (ps/scrollbar--own-frame-p (window-frame window)))
       (not (eq (window-buffer window) (get-buffer " *ps-scrollbar*")))
       (not (memq (buffer-local-value 'major-mode (window-buffer window))
                  ps/scrollbar-exclude-modes))
       ;; Claude Code (claude-code-ide.el) is an Ink/React TUI that manages
       ;; scroll history entirely inside its JavaScript process.  eat's
       ;; window-start never changes (always pmin), so our proportion math
       ;; returns 'hidden permanently -- no meaningful pill is possible.
       ;; See design-docs/scroll-bars.md for the full investigation.
       (not (string-prefix-p "*claude-code["
                             (buffer-name (window-buffer window))))))

(defun ps/scrollbar--mouse-window ()
  "Return the live window under the mouse, or nil."
  (let* ((mp (mouse-position))
         (frame (car mp)) (x (cadr mp)) (y (cddr mp)))
    (and (frame-live-p frame) (numberp x) (numberp y)
         (window-at x y frame))))

(defun ps/scrollbar--strip-hover-window ()
  "Return the window whose scrollbar track the mouse is currently within, or
nil.  Pixel-precise (`mouse-pixel-position'), unlike the coarse char-cell
`ps/scrollbar--mouse-window' used for scroll detection -- the track is often
narrower than one character cell, so a char-cell test would be too coarse."
  (pcase-let ((`(,frame ,x . ,y) (mouse-pixel-position)))
    (cond
     ((not (frame-live-p frame)) nil)
     ;; Hovering the pill itself: it only ever covers track pixels, and we
     ;; already cached which window it represents.
     ((ps/scrollbar--own-frame-p frame)
      (frame-parameter frame 'ps/scrollbar-window))
     ((not (and (integerp x) (integerp y))) nil)
     (t
      (let ((win (ignore-errors
                   (window-at (/ x (frame-char-width frame))
                              (/ y (frame-char-height frame))
                              frame))))
        (when (and win (ps/scrollbar--in-strip-p
                         x y (ps/scrollbar--strip-rect win)))
          win))))))

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
         ;; Pixel-precise and scoped to the track itself, not the whole
         ;; window; only computed when hover-reveal is on, so the common
         ;; scroll-only path pays no extra cost.
         (hovered (and ps/scrollbar-show-on-hover
                       (ps/scrollbar--strip-hover-window)))
         target)
    (cond
     ((and mouse-scrolled (ps/scrollbar--candidate-window-p mouse-win))
      (setq target mouse-win))
     ((and sel-scrolled (ps/scrollbar--candidate-window-p sel))
      (setq target sel))
     ((and hovered (ps/scrollbar--candidate-window-p hovered))
      (setq target hovered)))
    (if (and target (display-graphic-p (window-frame target)))
        (progn
          ;; New activity: cancel any in-progress fade so the next render
          ;; restores the full thumb colour (fade-cancel clears last-sig).
          (when ps/scrollbar--fade-start (ps/scrollbar--fade-cancel))
          (let ((res (ps/scrollbar--render target)))
            (unless (eq res 'skip)
              (setq ps/scrollbar--shown-at now))))
      ;; No activity.
      (cond
       ;; Fade in progress: advance one step.
       (ps/scrollbar--fade-start
        (let* ((elapsed (- now ps/scrollbar--fade-start))
               (frac (min 1.0 (/ elapsed (max 0.001 ps/scrollbar-fade-duration))))
               (f ps/scrollbar--visible-frame))
          (if (or (>= frac 1.0) (null f) (not (frame-live-p f)))
              (ps/scrollbar--hide-now)
            (let ((color (ps/scrollbar--lerp-color
                          ps/scrollbar--fade-from ps/scrollbar--fade-to frac)))
              (let ((ps/scrollbar--busy t))
                (set-frame-parameter f 'background-color color))))))
       ;; Idle long enough: begin the fade.
       ((and ps/scrollbar--visible-frame
             ps/scrollbar--shown-at
             (> (- now ps/scrollbar--shown-at) ps/scrollbar-hide-delay))
        (ps/scrollbar--hide))
       ;; Nothing else to do: just keep the pill snapped to its position.
       (t
        (ps/scrollbar--snap-back))))))

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
        (when prev (ps/scrollbar--hide-now))))))

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
        ps/scrollbar--last-sig nil)
  (ps/scrollbar--fade-cancel))

(defvar ps/scrollbar-mode-map
  (let ((map (make-sparse-keymap)))
    ;; The common click case: the parent frame's bare fringe (the pill only
    ;; covers the thumb's current pixels, not the whole track).  A click that
    ;; lands on the pill itself instead is handled by its own buffer-local map
    ;; (see `ps/scrollbar--buffer'), since that is a different frame.
    (define-key map [right-fringe down-mouse-1] #'ps/scrollbar--click-on-fringe)
    ;; On some platforms the vertical-line grab zone extends several pixels
    ;; into the right fringe, misclassifying scrollbar-track clicks as window-
    ;; resize attempts.  Intercept those and check the pixel position: if
    ;; inside a scrollbar strip, reposition; otherwise fall through to
    ;; `mouse-drag-vertical-line' (the normal resize handler).
    (define-key map [vertical-line down-mouse-1] #'ps/scrollbar--click-on-vertical-line)
    ;; Swallow the matching release events (same reason as the pill buffer's
    ;; [mouse-1]/[drag-mouse-1] bindings): when our handler ran, the release
    ;; must not fall through to default handling or it sets the mark.
    (define-key map [vertical-line mouse-1]      #'ignore)
    (define-key map [vertical-line drag-mouse-1] #'ignore)
    map)
  "Keymap for `ps/scrollbar-mode'.")

;;;###autoload
(define-minor-mode ps/scrollbar-mode
  "Global minor mode: an auto-hiding scroll-position indicator pill."
  :global t
  :group 'ps-scrollbar
  :keymap ps/scrollbar-mode-map
  (if ps/scrollbar-mode
      (ps/scrollbar--enable)
    (ps/scrollbar--disable)))

(provide 'ps-scrollbar)
;;; ps-scrollbar.el ends here
