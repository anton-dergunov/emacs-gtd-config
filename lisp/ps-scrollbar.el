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

(require 'cl-lib)

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
(defvar ps/scrollbar--drag-in-progress nil
  "Non-nil while a window-divider drag is active; suppresses the tick.
Set by `ps/scrollbar--resize-advice' and by `ps/scrollbar--click-on-vertical-line'
(because on macOS NS the divider drag is handled at C/OS level after our
Lisp handler exits, so the advice alone is not enough).")
(defvar ps/scrollbar--drag-suppress-timer nil
  "Fallback timer that clears drag-in-progress 2 s after last VL event.")
(defvar ps/scrollbar--drag-saved-window nil
  "Window selected when VL drag started; restored by release event or timer.")
(defvar ps/scrollbar--drag-initial-widths nil
  "Alist of (window . pixel-width) captured at start of VL press.
Used by the release handler to distinguish click from drag.")
(defvar ps/scrollbar--last-mp nil
  "Cached (frame col row) from the previous `mouse-position' call.
Used by `ps/scrollbar--tick-1' to skip expensive work when the mouse
has not moved since the last tick.")
(defvar ps/scrollbar--drag-switch-timer nil
  "Timer that switches the selected window 200 ms after a VL press.
Cancelled by `ps/scrollbar--vl-release' on quick releases (clicks).")
(defvar ps/scrollbar--fade-start nil
  "`float-time' when the current fade began, or nil (not fading).")
(defvar ps/scrollbar--fade-from nil
  "Starting color (hex string) of the current fade.")
(defvar ps/scrollbar--fade-to nil
  "Target color (hex string) of the current fade (parent's background).")
(defvar ps/scrollbar--teardown-timer nil
  "Timer that fully removes scrollbar hooks after a long unfocused stretch.
Set on focus loss (alongside pausing the tick timer), cancelled on focus
gain. See `ps/scrollbar--teardown-hooks'.")
(defvar ps/scrollbar--timer-speed nil
  "Rate the tick timer is currently running at: `fast', `slow', or nil.
See `ps/scrollbar-idle-poll-interval' for why the tick has two speeds.")

(defvar ps/scrollbar--fast-until nil
  "`float-time' until which the tick stays at the fast rate, or nil.
Pushed forward by `ps/scrollbar--note-activity' on each scroll or render, so
the tick keeps full responsiveness through a burst of activity and for one
reveal/fade cycle afterwards before dropping back to the idle rate.")

(defvar ps/scrollbar--last-hover nil
  "Window whose track the pointer was last found to be over, or nil.
Reused on ticks where `mouse-position' is unchanged: if the pointer has not
moved, the hover result cannot have changed either.  Besides skipping a
`mouse-pixel-position' call, this fixes the bug that made hover-reveal
effectively useless -- a motionless pointer counted as \"not hovering\", so a
hovered pill faded out after `ps/scrollbar-hide-delay' while the pointer was
still sitting on the track.")

(defvar ps/scrollbar--tick-count 0
  "Number of tick-timer firings since load.
Read by the freeze-log heartbeat (see `lisp/ps-freeze-log.el') so that after
a freeze the log shows whether this 0.15 s timer was still firing, and how
fast, right up to the wedge.  Every timer firing makes Emacs run
`redisplay_preserve_echo_area' from `detect_input_pending_run_timers', which
on the NS build enters a nested AppKit event loop -- the operation a freeze
gets stuck in -- so this rate is the module's freeze exposure.")

(defvar ps/scrollbar--torn-down nil
  "Non-nil while the hooks/advice below are fully uninstalled.
Set by `ps/scrollbar--teardown-hooks', cleared by
`ps/scrollbar--reinstall-hooks'. Distinct from the tick timer alone being
paused: this additionally removes `window-size-change-functions' and the
`mouse-drag-vertical-line' advice, so nothing in this module runs at all
while Emacs sits unfocused for a long time.")

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
low without a real cost.

Only in effect while there is something to do -- see
`ps/scrollbar-idle-poll-interval'."
  :type 'number :group 'ps-scrollbar)

(defcustom ps/scrollbar-idle-poll-interval 1.0
  "Seconds between ticks while idle: no pill shown, nothing scrolling.
The tick runs at `ps/scrollbar-tick-interval' only when there is work --
during and just after a scroll, and while the pill is visible or fading --
and drops to this much slower rate the rest of the time (which is most of
the time).  A scroll re-escalates immediately via `window-scroll-functions',
so scroll-reveal is not delayed by this.

Beyond saving CPU, this bounds a real failure mode: every timer firing makes
Emacs run a redisplay from `detect_input_pending_run_timers', which on the
macOS NS build enters a nested AppKit event loop, and that loop very
occasionally never exits -- wedging all of Emacs until it is restarted (see
design-docs/scroll-bars.md).  The idle rate is therefore this module's
standing exposure to that bug.  Raising it lowers exposure; the only cost is
that hover-reveal can take up to this long to notice the pointer entering
the track."
  :type 'number :group 'ps-scrollbar)

(defcustom ps/scrollbar-exclude-modes '(treemacs-mode which-key-mode)
  "Major modes whose windows never get a pill."
  :type '(repeat symbol) :group 'ps-scrollbar)

(defcustom ps/scrollbar-teardown-delay (* 30 60)
  "Seconds Emacs must stay unfocused before scrollbar hooks are fully removed.
Losing OS focus already pauses the tick timer (no CPU used in the
background), but `window-size-change-functions' and the
`mouse-drag-vertical-line' advice stay installed and can still fire from
non-timer events. After this many seconds unfocused, `ps/scrollbar--enable's
hooks/advice are fully uninstalled (`ps/scrollbar--teardown-hooks') and
transparently reinstalled the moment focus returns -- so nothing in this
module is reachable at all during a long idle stretch (e.g. overnight)."
  :type 'number :group 'ps-scrollbar)

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

(defun ps/scrollbar--log-op (label)
  "Record LABEL as the operation in progress, via `ps/freeze-log-op-begin'.
A thin, loosely-coupled wrapper (checked with `fboundp', not `require'd) so
this module stays self-contained if `ps-freeze-log' (a temporary diagnostic;
see its Commentary) is removed. Used to bracket every native Cocoa call this
module makes -- `make-frame', `set-frame-size'/`-position', `delete-frame',
`mouse-position'/`mouse-pixel-position' -- any of which is a candidate for
the whole-main-thread freeze under investigation (see design-docs/scroll-bars)."
  (when (fboundp 'ps/freeze-log-op-begin) (ps/freeze-log-op-begin label)))

(defun ps/scrollbar--log-op-done ()
  "Clear the in-progress marker set by `ps/scrollbar--log-op'."
  (when (fboundp 'ps/freeze-log-op-end) (ps/freeze-log-op-end)))

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
        ;; Do NOT call (select-window target) here: the scroll command reads
        ;; its target window from the event's posn-window (which we've just
        ;; rewritten to `target'), so it scrolls the correct window without
        ;; needing it to be selected.  Selecting it would persistently
        ;; "activate" the window (darken its mode-line), which is the wrong
        ;; UX when scrolling over the pill of an inactive window.
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
  (ps/scrollbar--log-op "make-frame")
  (unwind-protect
      (ps/scrollbar--make-frame-1 parent)
    (ps/scrollbar--log-op-done)))

(defun ps/scrollbar--make-frame-1 (parent)
  "Body of `ps/scrollbar--make-frame', factored out for the `unwind-protect' there."
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
        (ps/scrollbar--log-op "delete-frame")
        (unwind-protect
            (delete-frame frame)
          (ps/scrollbar--log-op-done))))))

(defun ps/scrollbar--reveal (child)
  "Make CHILD the single visible pill frame, destroying any other."
  (when (and ps/scrollbar--visible-frame
             (not (eq ps/scrollbar--visible-frame child))
             (frame-live-p ps/scrollbar--visible-frame))
    (ps/scrollbar--destroy ps/scrollbar--visible-frame))
  (setq ps/scrollbar--visible-frame child)
  (unless (frame-visible-p child)
    (ps/scrollbar--log-op "make-frame-visible")
    (unwind-protect
        (make-frame-visible child)
      (ps/scrollbar--log-op-done))))

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
          (ps/scrollbar--render-1 window parent wend orig-window)
        (unless (eq (selected-window) orig-window)
          (select-window orig-window 'norecord))))))

(defun ps/scrollbar--render-1 (window parent wend orig-window)
  "Body of `ps/scrollbar--render', factored out so the selection-restoring
`unwind-protect' there can wrap it cleanly.  ORIG-WINDOW is the window
that was selected before this render was triggered; it is restored before
the forced redisplay so the mode-line of WINDOW (which may be an inactive
window the user is just hovering over) does not flash active."
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
                (ps/scrollbar--log-op "set-geom")
                (unwind-protect
                    (progn
                      (set-frame-size child pill-w thumb-h t)
                      (set-frame-position child fleft ftop))
                  (ps/scrollbar--log-op-done)))
              ;; If we were ducked out of a forwarded wheel event's way,
              ;; stay invisible for a beat before the tick loop re-shows
              ;; the pill at its (now updated) position.
              (if (and ps/scrollbar--ducked-at
                       (< (- (float-time) ps/scrollbar--ducked-at)
                          (* 1.5 ps/scrollbar-tick-interval)))
                  (setq ps/scrollbar--visible-frame nil)
                (ps/scrollbar--reveal child))
              ;; `ps/scrollbar--ensure-frame' may have just selected the
              ;; pill's own frame as a side effect of `make-frame'.  Restore
              ;; ORIG-WINDOW before forcing redisplay so the paint uses the
              ;; *correct* selection (the window the user was in before the
              ;; tick fired), not WINDOW (which may be an inactive window
              ;; the user is hovering over -- selecting it here would make
              ;; its mode-line flash active).  org-modern's border-syncing
              ;; hook is disabled (org-modern-label-border = nil) so it no
              ;; longer reads selected-frame's background during redisplay,
              ;; removing the earlier reason to select WINDOW specifically.
              (unless (eq (selected-window) orig-window)
                (select-window orig-window 'norecord))
              ;; Diagnostic: bracket the forced synchronous flush (`redisplay
              ;; t' -> `ns_flush_display' on the NS build), one of several
              ;; suspects for the whole-main-thread freeze under
              ;; investigation.  A BEGIN with no matching END in
              ;; ps-freeze.log (or a stale marker) means the wedge happened
              ;; right here.  See lisp/ps-freeze-log.el and
              ;; design-docs/scroll-bars.md.
              (when (fboundp 'ps/freeze-log)
                (ps/freeze-log 'scrollbar "redisplay BEGIN win=%s geom=%s"
                               window geom))
              (ps/scrollbar--log-op "redisplay")
              (unwind-protect
                  (redisplay t)
                (ps/scrollbar--log-op-done))
              (when (fboundp 'ps/freeze-log)
                (ps/freeze-log 'scrollbar "redisplay END win=%s" window)))
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

(defun ps/scrollbar--reposition-at-mouse ()
  "Reposition the scrollbar of whichever window the mouse is over.
Used after `mouse-drag-vertical-line' returns without resizing (click case)."
  (let* ((ppos (mouse-pixel-position))
         (frame (car ppos)) (px (cadr ppos)) (py (cddr ppos)))
    (when (and (frame-live-p frame) (integerp px) (integerp py))
      (catch 'found
        (dolist (win (window-list frame))
          (when (ps/scrollbar--candidate-window-p win)
            (let ((rect (ps/scrollbar--strip-rect win)))
              (when (ps/scrollbar--in-strip-p px py rect)
                (ps/scrollbar--reposition win (- py (nth 1 rect)))
                (throw 'found nil)))))))))

(defun ps/scrollbar--click-on-vertical-line (event)
  "Handle down-mouse-1 in the vertical-line area near a scrollbar track.
On macOS NS the backend fires this repeatedly during a drag.

Two-layer suppression to prevent the pill appearing during the drag:
1. `drag-in-progress' = t  → tick is suppressed directly.
2. Switch selected window away from the dragged window → even if the tick
   fires, the hover check `(eq hovered (selected-window))' is nil.

On the first event: save the selected window and window widths, then
switch.  The release event (`ps/scrollbar--vl-release') restores
everything and handles click-to-reposition."
  (interactive "e")
  (let ((posn-win (posn-window (event-start event))))
    ;; First press only: save state and schedule a delayed window switch.
    ;; The switch fires after 200 ms -- long enough to confirm a drag, short
    ;; enough not to matter for scrollbar clicks.  `vl-release' cancels it
    ;; on quick releases so clicks never see a window switch.
    (when (and (window-live-p posn-win)
               (eq posn-win (selected-window))
               (null ps/scrollbar--drag-saved-window))
      (setq ps/scrollbar--drag-saved-window  (selected-window)
            ps/scrollbar--drag-initial-widths
            (mapcar (lambda (w) (cons w (window-pixel-width w)))
                    (window-list))
            ps/scrollbar--drag-switch-timer
            (run-with-timer 0.2 nil #'ps/scrollbar--drag-do-switch))))
  (condition-case nil
      (mouse-drag-vertical-line event)
    (user-error nil))
  (ps/scrollbar--hide-now)
  (ps/scrollbar--drag-suppress-extend))
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
  (pcase-let ((`(,frame ,x . ,y)
               (progn (ps/scrollbar--log-op "mouse-pixel-position")
                      (unwind-protect (mouse-pixel-position)
                        (ps/scrollbar--log-op-done)))))
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

(defun ps/scrollbar--resolve-hover (mouse-moved)
  "Return the live window whose track the pointer is over, or nil.

Recomputes (via `ps/scrollbar--strip-hover-window', which costs a
`mouse-pixel-position' call) only when MOUSE-MOVED says the pointer changed
character cell; otherwise reuses `ps/scrollbar--last-hover', because a
pointer that has not moved cannot have entered or left the track.

That reuse is the fix for hover-reveal being useless in practice: treating a
motionless pointer as \"not hovering\" left no render target, so a hovered
pill faded out after `ps/scrollbar-hide-delay' while the pointer was still
resting on the track."
  (when ps/scrollbar-show-on-hover
    (let ((h (if mouse-moved
                 (setq ps/scrollbar--last-hover
                       (ps/scrollbar--strip-hover-window))
               ps/scrollbar--last-hover)))
      (and (window-live-p h) h))))

(defun ps/scrollbar--scrolled-window (w)
  "Non-nil if W's `window-start' changed since the last tick (updates it)."
  (and (window-live-p w)
       (let ((cur (window-start w))
             (prev (window-parameter w 'ps/scrollbar--last-start)))
         (set-window-parameter w 'ps/scrollbar--last-start cur)
         (and prev (not (eql cur prev))))))

(defun ps/scrollbar--snap-back ()
  "Re-apply the visible pill's intended geometry (undo an accidental nudge).
macOS lets the user move/resize the borderless pill window; we put it back.
Position is only written when the frame has actually moved, to avoid
triggering a redisplay on every tick while the pill is stationary.

A pill whose window has been deleted is snapped nowhere -- it is hidden.
`ps/scrollbar--on-size-change' already hides on layout changes; this is the
backstop for a window dying without one (and the tick is the only thing that
would otherwise keep such a pill pinned in place)."
  (let ((f ps/scrollbar--visible-frame))
    (when (and f (frame-live-p f)
               (not (window-live-p (frame-parameter f 'ps/scrollbar-window))))
      (ps/scrollbar--hide-now)
      (setq f nil))
    (when (and f (frame-live-p f))
      (let ((geom (frame-parameter f 'ps/scrollbar-geom)))
        (when geom
          (pcase-let ((`(,l ,tp ,w ,h) geom)
                      (ps/scrollbar--busy t))
            (unless (and (= (frame-pixel-width f) w)
                         (= (frame-pixel-height f) h))
              (ps/scrollbar--log-op "snap-back-size")
              (unwind-protect (set-frame-size f w h t)
                (ps/scrollbar--log-op-done)))
            (let ((cur (frame-position f)))
              (unless (and (= (car cur) l) (= (cdr cur) tp))
                (ps/scrollbar--log-op "snap-back-position")
                (unwind-protect (set-frame-position f l tp)
                  (ps/scrollbar--log-op-done))))))))))

;;; Tick scheduling (two-speed: see `ps/scrollbar-idle-poll-interval')

(defun ps/scrollbar--start-timer (speed)
  "(Re)start the tick timer at SPEED, either `fast' or `slow'."
  (when ps/scrollbar--timer (cancel-timer ps/scrollbar--timer))
  (let ((interval (if (eq speed 'fast)
                      ps/scrollbar-tick-interval
                    ps/scrollbar-idle-poll-interval)))
    (setq ps/scrollbar--timer-speed speed
          ps/scrollbar--timer (run-with-timer interval interval
                                              #'ps/scrollbar--tick))))

(defun ps/scrollbar--stop-timer ()
  "Cancel the tick timer, if any."
  (when ps/scrollbar--timer (cancel-timer ps/scrollbar--timer))
  (setq ps/scrollbar--timer nil
        ps/scrollbar--timer-speed nil))

(defun ps/scrollbar--fast-linger ()
  "Seconds the tick stays fast after the last activity.
Covers one full reveal-and-fade cycle, so the pill never drops to the idle
rate mid-fade (which would make the fade visibly stutter)."
  (+ ps/scrollbar-hide-delay ps/scrollbar-fade-duration))

(defun ps/scrollbar--want-fast-p (now visible fading fast-until)
  "Non-nil if the tick should run at the fast rate.  Pure/testable.
VISIBLE and FADING are the current pill/fade state; FAST-UNIT is the
`ps/scrollbar--fast-until' deadline (nil for none)."
  (and (or visible fading (and fast-until (< now fast-until))) t))

(defun ps/scrollbar--apply-speed ()
  "Move the tick between the fast and idle rates to match current state.
A no-op when no timer is running (mode off, or focus-paused): those states
must not be silently restarted from here."
  (when ps/scrollbar--timer
    (let ((want (if (ps/scrollbar--want-fast-p
                     (float-time)
                     (and ps/scrollbar--visible-frame
                          (frame-live-p ps/scrollbar--visible-frame))
                     ps/scrollbar--fade-start
                     ps/scrollbar--fast-until)
                    'fast 'slow)))
      (unless (eq want ps/scrollbar--timer-speed)
        (ps/scrollbar--start-timer want)))))

(defun ps/scrollbar--note-activity ()
  "Record interaction and make sure the tick is running at the fast rate."
  (setq ps/scrollbar--fast-until (+ (float-time) (ps/scrollbar--fast-linger)))
  ;; Only escalate an already-running tick: a nil timer means the mode is off
  ;; or focus-paused, and must stay that way.
  (when (and ps/scrollbar--timer (not (eq ps/scrollbar--timer-speed 'fast)))
    (ps/scrollbar--start-timer 'fast)))

(defun ps/scrollbar--on-scroll (_window _start)
  "`window-scroll-functions' hook: a window scrolled, so go to full speed.
This is the event-driven half of the two-speed tick -- without it, the first
render after a scroll could wait a whole `ps/scrollbar-idle-poll-interval'.
Deliberately minimal: it runs inside redisplay, so it only touches timer
state and never buffers, windows or frames."
  (ps/scrollbar--note-activity))

(defun ps/scrollbar--on-focus-change ()
  "Pause or resume the tick timer when Emacs gains or loses OS focus.
Wired into `after-focus-change-function' by `ps/scrollbar--enable'.
On focus loss: cancel the timer and hide the pill -- no CPU used while
Emacs is in the background -- and arm `ps/scrollbar--teardown-timer' to
fully uninstall the remaining hooks/advice if the loss lasts long enough
\(see `ps/scrollbar-teardown-delay').  On focus gain: cancel that timer,
reinstall the hooks/advice if they were torn down, and restart the tick
timer so scroll and hover detection resume immediately."
  (if (frame-focus-state)
      (progn
        (when ps/scrollbar--teardown-timer
          (cancel-timer ps/scrollbar--teardown-timer)
          (setq ps/scrollbar--teardown-timer nil))
        (when ps/scrollbar--torn-down
          (ps/scrollbar--reinstall-hooks))
        ;; Emacs regained focus: restart the tick, idle-rate until something
        ;; actually happens (a scroll escalates it via `--on-scroll').
        (when (and ps/scrollbar-mode (null ps/scrollbar--timer))
          (ps/scrollbar--start-timer 'slow)))
    ;; Emacs lost focus: stop the timer and hide the pill.
    (ps/scrollbar--stop-timer)
    (setq ps/scrollbar--fast-until nil
          ps/scrollbar--last-hover nil)
    (ps/scrollbar--hide-now)
    (when ps/scrollbar--teardown-timer
      (cancel-timer ps/scrollbar--teardown-timer))
    (setq ps/scrollbar--teardown-timer
          (run-with-timer ps/scrollbar-teardown-delay nil
                          #'ps/scrollbar--teardown-hooks))))

(defun ps/scrollbar--teardown-hooks ()
  "Fully remove scrollbar hooks/advice after a long unfocused stretch.
Called by the timer `ps/scrollbar--on-focus-change' arms on focus loss.
Leaves `ps/scrollbar-mode' itself enabled (from the user's perspective the
mode is still on) -- `ps/scrollbar--reinstall-hooks' puts everything back
the moment focus returns.  `delete-frame-functions' is left installed: it
only fires on the rare deletion of a whole frame, not periodically, so it
is not part of what this guards against."
  (setq ps/scrollbar--teardown-timer nil)
  (remove-hook 'window-size-change-functions #'ps/scrollbar--on-size-change)
  (remove-hook 'window-scroll-functions      #'ps/scrollbar--on-scroll)
  (advice-remove 'mouse-drag-vertical-line #'ps/scrollbar--resize-advice)
  (ps/scrollbar--delete-all-frames)
  (ps/scrollbar--hide-now)
  (setq ps/scrollbar--torn-down t)
  (when (fboundp 'ps/freeze-log)
    (ps/freeze-log 'scrollbar "teardown (long unfocus): hooks/advice removed")))

(defun ps/scrollbar--reinstall-hooks ()
  "Undo `ps/scrollbar--teardown-hooks': reinstall hooks/advice on focus gain."
  (add-hook 'window-size-change-functions #'ps/scrollbar--on-size-change)
  (add-hook 'window-scroll-functions      #'ps/scrollbar--on-scroll)
  (advice-add 'mouse-drag-vertical-line :around #'ps/scrollbar--resize-advice)
  (setq ps/scrollbar--torn-down nil)
  (when (fboundp 'ps/freeze-log)
    (ps/freeze-log 'scrollbar "reinstall (focus regained): hooks/advice restored")))

(defun ps/scrollbar--tick ()
  "Refresh the pill: reveal on scroll/hover, fade when idle, snap back nudges."
  (setq ps/scrollbar--tick-count (1+ ps/scrollbar--tick-count))
  (when (and ps/scrollbar-mode
             (not ps/scrollbar--drag-in-progress)
             ;; Skip entirely when Emacs is definitely not focused: no hover
             ;; is possible and no keyboard scroll can happen, so the expensive
             ;; mouse-position call would be wasted.  nil = definitely unfocused;
             ;; t or 'unknown = run normally.
             (not (null (frame-focus-state))))
    (condition-case err
        (ps/scrollbar--tick-1)
      (error (message "ps/scrollbar: %S" err))))
  ;; Outside the guard above: the rate must be re-evaluated every cycle,
  ;; including cycles whose body was skipped, so an idle or drag-suppressed
  ;; tick still drops back to `ps/scrollbar-idle-poll-interval'.
  (ps/scrollbar--apply-speed))

(defun ps/scrollbar--tick-1 ()
  (let* ((now (float-time))
         (sel (selected-window))
         ;; Call mouse-position ONCE and cache the result.  The cached value is
         ;; compared each tick to avoid calling mouse-pixel-position (the more
         ;; expensive strip-hover check) when the mouse hasn't moved.
         ;; Diagnostic: this runs unconditionally on every 0.15s tick, making
         ;; it the highest-frequency native NS call in this module and a
         ;; prime suspect for the whole-main-thread freeze (see
         ;; design-docs/scroll-bars.md); bracketed with the freeze-log marker
         ;; rather than the append log to stay cheap at this frequency.
         (mp (progn (ps/scrollbar--log-op "mouse-position")
                    (unwind-protect (mouse-position)
                      (ps/scrollbar--log-op-done))))
         (mouse-moved (not (equal mp ps/scrollbar--last-mp)))
         ;; Derive mouse-win from the already-computed mp — no second call.
         (mouse-win (pcase-let ((`(,frame ,x . ,y) mp))
                      (and (frame-live-p frame) (numberp x) (numberp y)
                           (window-at x y frame))))
         ;; Scroll detection always runs: it is cheap (just window-start + window
         ;; parameter) and must not be called speculatively before the render
         ;; decision — ps/scrollbar--scrolled-window has a side effect (updates
         ;; the cache), so calling it twice for the same window in the same tick
         ;; would make the second call always return nil.
         (mouse-scrolled (and mouse-win (ps/scrollbar--scrolled-window mouse-win)))
         (sel-scrolled   (and (not (eq sel mouse-win))
                              (ps/scrollbar--scrolled-window sel)))
         (hovered (ps/scrollbar--resolve-hover mouse-moved))
         target)
    (setq ps/scrollbar--last-mp mp)
    (cond
     ((and mouse-scrolled (ps/scrollbar--candidate-window-p mouse-win))
      (setq target mouse-win))
     ((and sel-scrolled (ps/scrollbar--candidate-window-p sel))
      (setq target sel))
     ;; Hover: only reveal for the currently selected (active) window.
     ;; An inactive window keeps its mode-line dim; showing its pill on
     ;; hover would make it briefly appear active.
     ((and hovered
           (eq hovered (selected-window))
           (ps/scrollbar--candidate-window-p hovered))
      (setq target hovered)))
    (if (and target (display-graphic-p (window-frame target)))
        (progn
          ;; New activity: cancel any in-progress fade so the next render
          ;; restores the full thumb colour (fade-cancel clears last-sig).
          (when ps/scrollbar--fade-start (ps/scrollbar--fade-cancel))
          (let ((res (ps/scrollbar--render target)))
            (unless (eq res 'skip)
              (setq ps/scrollbar--shown-at now
                    ;; Keep the tick at full speed through this reveal and
                    ;; the fade that follows it.
                    ps/scrollbar--fast-until
                    (+ now (ps/scrollbar--fast-linger))))))
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

(defun ps/scrollbar--drag-do-switch ()
  "Switch the selected window 200 ms after a VL press (drag confirmed).
If the press was a quick click, `ps/scrollbar--vl-release' cancels this
timer before it fires and no switch occurs."
  (setq ps/scrollbar--drag-switch-timer nil)
  (when (and ps/scrollbar--drag-saved-window
             (window-live-p ps/scrollbar--drag-saved-window)
             (eq ps/scrollbar--drag-saved-window (selected-window)))
    (other-window 1 nil t)))

(defun ps/scrollbar--drag-suppress-end ()
  "Fallback timer callback: 2 s with no VL events → drag is done.
Restores window selection and clears all drag state.  Under normal
conditions `ps/scrollbar--vl-release' handles this on the release event;
this fallback covers areas whose drag-mouse-1 we do not have a binding for."
  (when ps/scrollbar--drag-switch-timer
    (cancel-timer ps/scrollbar--drag-switch-timer)
    (setq ps/scrollbar--drag-switch-timer nil))
  (setq ps/scrollbar--drag-in-progress    nil
        ps/scrollbar--drag-suppress-timer nil
        ps/scrollbar--drag-initial-widths nil)
  (when (and ps/scrollbar--drag-saved-window
             (window-live-p ps/scrollbar--drag-saved-window))
    (select-window ps/scrollbar--drag-saved-window 'norecord))
  (setq ps/scrollbar--drag-saved-window nil))

(defun ps/scrollbar--drag-suppress-extend ()
  "Assert drag suppression and reset the 2 s fallback timer.
NS fires [vertical-line down-mouse-1] repeatedly during a drag, so each
call pushes the fallback further out.  The release event fires
`ps/scrollbar--vl-release' to clean up immediately in the common case."
  (setq ps/scrollbar--drag-in-progress t)
  (when ps/scrollbar--drag-suppress-timer
    (cancel-timer ps/scrollbar--drag-suppress-timer))
  (setq ps/scrollbar--drag-suppress-timer
    (run-with-timer 2.0 nil #'ps/scrollbar--drag-suppress-end)))

(defun ps/scrollbar--vl-release (event)
  "Handle mouse-1/drag-mouse-1 after a vertical-line down-mouse-1.
Immediately restores the window selection saved by the press handler.
If no windows were resized (plain click, not drag), repositions the
scrollbar at the current mouse position."
  (interactive "e")
  (ignore event)
  (let ((was-click
         (and ps/scrollbar--drag-initial-widths
              (not (cl-some (lambda (entry)
                              (and (window-live-p (car entry))
                                   (/= (window-pixel-width (car entry))
                                       (cdr entry))))
                            ps/scrollbar--drag-initial-widths)))))
    ;; Cancel the delayed window-switch (click was quick enough).
    (when ps/scrollbar--drag-switch-timer
      (cancel-timer ps/scrollbar--drag-switch-timer)
      (setq ps/scrollbar--drag-switch-timer nil))
    (when ps/scrollbar--drag-suppress-timer
      (cancel-timer ps/scrollbar--drag-suppress-timer))
    (setq ps/scrollbar--drag-in-progress    nil
          ps/scrollbar--drag-suppress-timer nil
          ps/scrollbar--drag-initial-widths nil)
    ;; Reposition before restoring window (mouse is still at click spot).
    (when (and was-click ps/scrollbar-click-to-scroll)
      (ps/scrollbar--reposition-at-mouse))
    (when (and ps/scrollbar--drag-saved-window
               (window-live-p ps/scrollbar--drag-saved-window))
      (select-window ps/scrollbar--drag-saved-window 'norecord))
    (setq ps/scrollbar--drag-saved-window nil)))

(defun ps/scrollbar--on-size-change (frame)
  "React to window-layout changes in FRAME.
`window-size-change-functions' fires both when the frame itself is resized
and when windows within it rearrange -- a vertical-line divider drag, but
equally a window opening or closing (ediff, the file tree, a split).  We
ignore our own changes (`--busy') and our own pill frames.

Any layout change invalidates the pill: it means \"you are scrolling here,
now\", and \"here\" just moved.  So the pill is hidden outright and the
per-window scroll baselines are dropped, then it comes back on the next
real scroll or hover.  Drag suppression is only *extended* here, never
started -- see below."
  (unless (or ps/scrollbar--busy (ps/scrollbar--own-frame-p frame))
    ;; Bookkeeping kept so a frame resize stays distinguishable from a
    ;; window rearrange, though both now hide the pill.
    (let ((size (cons (frame-pixel-width frame) (frame-pixel-height frame))))
      (unless (equal size (frame-parameter frame 'ps/scrollbar--last-size))
        (set-frame-parameter frame 'ps/scrollbar--last-size size)))
    ;; A no-op (and so free of native frame calls) whenever no pill is up.
    (ps/scrollbar--hide-now)
    ;; Drop the scroll baselines for this frame's windows.  Restoring a
    ;; window configuration -- what ediff does on quit -- gives windows a
    ;; `window-start' that differs from the one recorded before the layout
    ;; changed, and `ps/scrollbar--scrolled-window' would read that as a
    ;; user scroll and reveal a pill nobody asked for.  It returns nil for a
    ;; nil baseline and re-seeds it, so the next tick simply re-baselines.
    (dolist (w (window-list frame 'no-minibuf))
      (set-window-parameter w 'ps/scrollbar--last-start nil))
    ;; Extend -- never start -- drag suppression.  An NS-level divider drag
    ;; runs at C level after our Lisp handler exits, so its resize steps have
    ;; to keep the suppression alive; but `ps/scrollbar--click-on-vertical-line'
    ;; has already set `--drag-in-progress' before the first of those steps
    ;; can arrive, so gating on it costs the drag nothing.  Starting
    ;; suppression from cold here instead froze the tick for the full
    ;; `ps/scrollbar--drag-suppress-extend' fallback timeout (2 s) on every
    ;; *programmatic* window change, which is what left a stray pill frozen
    ;; on screen after ediff opened or closed.  That call was written when the
    ;; timeout was 200 ms and a different commit later repurposed it as the
    ;; missed-release fallback.  `track-mouse' covers Lisp-level drag loops we
    ;; have no advice on (e.g. `mouse-drag-mode-line') and is nil for
    ;; programmatic changes.
    (when (or ps/scrollbar--drag-in-progress
              (bound-and-true-p track-mouse))
      (ps/scrollbar--drag-suppress-extend))))

;;; Enable / disable

(defun ps/scrollbar--resize-advice (orig &rest args)
  "Hide the scrollbar pill while a vertical window divider is being dragged.
Sets `ps/scrollbar--drag-in-progress' globally (not via `let') so that
idle-timer callbacks -- which run with global dynamic bindings, not inside
the let's scope -- also see it and suppress rendering for the duration.
Installed around `mouse-drag-vertical-line' by `ps/scrollbar--enable'."
  (setq ps/scrollbar--drag-in-progress t)
  (ps/scrollbar--hide-now)
  (unwind-protect
      (apply orig args)
    (setq ps/scrollbar--drag-in-progress nil)))

;; `mouse-drag-line' -- the workhorse behind `mouse-drag-vertical-line' -- sets
;; the global `track-mouse' to `dragging' and then restores it from
;; `set-transient-map''s on-exit function with a plain `setq', not a `let'.
;; That `setq' runs in whatever buffer happens to be current when the drag
;; ends, so if that buffer has a buffer-local `track-mouse' (`eat-mode' makes
;; it one, which covers the Claude terminal pane) the restore lands in the
;; local slot and the *global* value is left at `dragging' forever.
;;
;; Emacs then honours that value's documented meaning -- "the mouse cursor
;; retains its appearance during mouse motion" -- in every buffer without a
;; local binding, so the pointer shape freezes globally: no hand over org links
;; or file-tree nodes, no resize cursor over dividers, until Emacs restarts.
;; `mouse-face' highlighting and clicking keep working, which is what makes it
;; look like a broken-links problem rather than a global one.
;;
;; This module turns that from a rare race into a routine one:
;; `ps/scrollbar--drag-do-switch' deliberately calls `other-window' 200 ms into
;; a divider drag, so drags regularly end in a different buffer than they
;; started -- often the terminal pane.
;;
;; The reset cannot go in `ps/scrollbar--resize-advice''s `unwind-protect':
;; `mouse-drag-line' returns as soon as the transient map is installed, long
;; before the drag ends, and clearing `track-mouse' there would cut off the
;; very motion events the drag needs.  So we re-check from `post-command-hook'
;; and act only once the drag is demonstrably over.  Like the burst detector in
;; `ps-claude.el', this cannot itself get stuck: it re-derives the answer from
;; live state every time rather than tracking state of its own.

(defun ps/scrollbar--track-mouse-stuck-p (global transient-map-active dragging)
  "Non-nil if GLOBAL is a leftover pointer-freezing `track-mouse' value.

GLOBAL is the global (default) value of `track-mouse'.
TRANSIENT-MAP-ACTIVE is non-nil while a drag's transient keymap is still
installed -- i.e. while `mouse-drag-line' is genuinely still dragging.
DRAGGING is `ps/scrollbar--drag-in-progress', which also covers the
NS-level divider drag that runs at C level with no transient map of ours.

Only `dragging' counts as stuck.  `dropping' freezes the pointer too, but
belongs to drag-and-drop, which runs inside a `track-mouse' let-binding
rather than a transient map -- clearing it could clobber a live drag."
  (and (eq global 'dragging)
       (not transient-map-active)
       (not dragging)))

(defun ps/scrollbar--unstick-track-mouse ()
  "Restore a globally stuck `track-mouse' left behind by a finished drag.
Installed on `post-command-hook' by `ps/scrollbar--enable'."
  (when (ps/scrollbar--track-mouse-stuck-p
         (default-value 'track-mouse)
         overriding-terminal-local-map
         ps/scrollbar--drag-in-progress)
    (setq-default track-mouse nil)))

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
  (add-hook 'delete-frame-functions       #'ps/scrollbar--on-delete-frame)
  (add-hook 'window-size-change-functions #'ps/scrollbar--on-size-change)
  (add-function :after after-focus-change-function #'ps/scrollbar--on-focus-change)
  (advice-add 'mouse-drag-vertical-line :around #'ps/scrollbar--resize-advice)
  (add-hook 'post-command-hook #'ps/scrollbar--unstick-track-mouse)
  (add-hook 'window-scroll-functions #'ps/scrollbar--on-scroll)
  (setq ps/scrollbar--last-sig nil
        ps/scrollbar--torn-down nil
        ps/scrollbar--fast-until nil
        ps/scrollbar--last-hover nil)
  ;; Start idle: nothing is happening yet, and a scroll escalates instantly.
  (ps/scrollbar--start-timer 'slow))

(defun ps/scrollbar--disable ()
  (ps/scrollbar--stop-timer)
  (remove-hook 'window-scroll-functions #'ps/scrollbar--on-scroll)
  (setq ps/scrollbar--fast-until nil
        ps/scrollbar--last-hover nil)
  (when ps/scrollbar--teardown-timer
    (cancel-timer ps/scrollbar--teardown-timer)
    (setq ps/scrollbar--teardown-timer nil))
  (remove-hook 'delete-frame-functions       #'ps/scrollbar--on-delete-frame)
  ;; Harmless no-ops if `ps/scrollbar--teardown-hooks' already removed these.
  (remove-hook 'window-size-change-functions #'ps/scrollbar--on-size-change)
  (remove-function after-focus-change-function #'ps/scrollbar--on-focus-change)
  (advice-remove 'mouse-drag-vertical-line #'ps/scrollbar--resize-advice)
  ;; Not torn down by `ps/scrollbar--teardown-hooks': unlike the pill work that
  ;; guards against, this is an `eq' on one variable, and a drag that ends just
  ;; as focus is lost would otherwise leave the pointer frozen until focus
  ;; returns.  Only disabling the mode removes it.
  (remove-hook 'post-command-hook #'ps/scrollbar--unstick-track-mouse)
  (ps/scrollbar--delete-all-frames)
  (unless (eq ps/scrollbar--saved-right-fringe 'unset)
    (modify-all-frames-parameters
     (list (cons 'right-fringe ps/scrollbar--saved-right-fringe)))
    (setq ps/scrollbar--saved-right-fringe 'unset))
  (setq ps/scrollbar--visible-frame nil
        ps/scrollbar--last-sig nil
        ps/scrollbar--torn-down nil)
  (ps/scrollbar--fade-cancel))

(defvar ps/scrollbar-mode-map
  (let ((map (make-sparse-keymap)))
    ;; The common click case: the parent frame's bare fringe (the pill only
    ;; covers the thumb's current pixels, not the whole track).  A click that
    ;; lands on the pill itself instead is handled by its own buffer-local map
    ;; (see `ps/scrollbar--buffer'), since that is a different frame.
    (define-key map [right-fringe down-mouse-1] #'ps/scrollbar--click-on-fringe)
    ;; Vertical-line area: the grab zone for window-divider resizing extends
    ;; several pixels into the right fringe, so clicks there arrive as
    ;; [vertical-line ...] rather than [right-fringe ...].
    ;; down-mouse-1: redirect to scrollbar if pixel is in our strip, or
    ;;   re-inject to let mouse-drag-vertical-line handle resize.
    ;; mouse-1 / drag-mouse-1: swallow so the release events don't fall
    ;;   through to default Emacs handling after our reposition ran.
    (define-key map [vertical-line down-mouse-1] #'ps/scrollbar--click-on-vertical-line)
    (define-key map [vertical-line mouse-1]      #'ps/scrollbar--vl-release)
    (define-key map [vertical-line drag-mouse-1] #'ps/scrollbar--vl-release)
    ;; Drag often ends in the left-fringe area; handle the release there too.
    (define-key map [left-fringe drag-mouse-1]   #'ps/scrollbar--vl-release)
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
