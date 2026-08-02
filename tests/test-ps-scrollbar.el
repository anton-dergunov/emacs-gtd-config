;;; test-ps-scrollbar.el --- ERT tests for ps-scrollbar -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(add-to-list 'load-path "lisp")
(require 'ps-scrollbar)

;;; Thumb geometry (pixels)

(ert-deftest ps/scrollbar--span-nil-when-content-fits ()
  "No pill when the whole buffer is visible."
  (should-not (ps/scrollbar--thumb-span 1 1001 1 1001 400 24)))

(ert-deftest ps/scrollbar--span-top ()
  "Scrolled to the top: pill starts at pixel 0."
  (let ((s (ps/scrollbar--thumb-span 1 1001 1 251 400 24)))
    (should s)
    (should (= (car s) 0))
    (should (= (cdr s) 100))))          ; 250/1000 * 400

(ert-deftest ps/scrollbar--span-middle ()
  "Scrolled to the middle: pill roughly centered."
  (let ((s (ps/scrollbar--thumb-span 1 1001 376 626 400 24)))
    (should s)
    (should (= (cdr s) 100))
    (should (= (car s) 150))))          ; 375/1000 * 400

(ert-deftest ps/scrollbar--span-bottom-clamped ()
  "At the bottom the pill never runs past the track."
  (let ((s (ps/scrollbar--thumb-span 1 1001 751 1001 400 24)))
    (should s)
    (should (= (+ (car s) (cdr s)) 400))))

(ert-deftest ps/scrollbar--span-min-height ()
  "A huge buffer still yields at least the minimum pill height."
  (let ((s (ps/scrollbar--thumb-span 1 1000001 1 401 400 24)))
    (should s)
    (should (>= (cdr s) 24))))

(ert-deftest ps/scrollbar--span-guards ()
  "Degenerate inputs return nil rather than erroring."
  (should-not (ps/scrollbar--thumb-span 1 1 1 1 400 24))     ; empty buffer
  (should-not (ps/scrollbar--thumb-span 1 1000 1 500 0 24))) ; zero track

;;; Click-to-reposition geometry (inverse of thumb-span)

(ert-deftest ps/scrollbar--frac-to-start-round-trips-thumb-span ()
  "Clicking the rendered thumb's exact center reproduces the current start."
  (let* ((span (ps/scrollbar--thumb-span 1 1001 251 501 400 24))
         (size-frac 0.25)
         (center-frac (/ (+ (car span) (/ (cdr span) 2.0)) 400.0)))
    (should (= (ps/scrollbar--frac-to-start 1 1001 size-frac center-frac) 251))))

(ert-deftest ps/scrollbar--frac-to-start-top-clamped ()
  "Clicking above the track never asks for a start before PMIN."
  (should (= (ps/scrollbar--frac-to-start 1 1001 0.25 0.0) 1)))

(ert-deftest ps/scrollbar--frac-to-start-bottom-clamped ()
  "Clicking below the track never pushes the thumb past the bottom."
  (should (= (ps/scrollbar--frac-to-start 1 1001 0.25 1.0) 751)))

;;; Track rect / hover hit-test (pixels)

(ert-deftest ps/scrollbar--strip-rect-1-basic ()
  "Track rect is window-right-edge-relative-to-parent, fringe-width wide."
  ;; Window's right (basic) edge at absolute x=814, body edge at x=800 (a
  ;; 14px fringe); parent frame's native origin at (100, 50); inside text
  ;; area spans y=70..470 (frame-relative).
  (should (equal (ps/scrollbar--strip-rect-1 814 800 120 100 50 70 470)
                 (list 700 70 714 470))))

(ert-deftest ps/scrollbar--strip-rect-1-min-width ()
  "Track is at least 2px wide even if WR and BR coincide."
  (let ((rect (ps/scrollbar--strip-rect-1 800 800 100 0 0 0 100)))
    (should (= (- (nth 2 rect) (nth 0 rect)) 2))))

(ert-deftest ps/scrollbar--in-strip-p-inside-and-outside ()
  (let ((rect '(700 70 714 470)))
    (should (ps/scrollbar--in-strip-p 705 200 rect))
    (should (ps/scrollbar--in-strip-p 700 70 rect))       ; inclusive corner
    (should-not (ps/scrollbar--in-strip-p 714 200 rect))  ; exclusive right
    (should-not (ps/scrollbar--in-strip-p 705 470 rect))  ; exclusive bottom
    (should-not (ps/scrollbar--in-strip-p 699 200 rect))
    (should-not (ps/scrollbar--in-strip-p 705 69 rect))))

;;; Fade colour interpolation

(ert-deftest ps/scrollbar--lerp-color-endpoints ()
  "At frac 0 and 1 the result matches from and to exactly."
  (let ((from "#3c3c3c") (to "#ffffff"))
    ;; frac=0 → from color; allow ±1 in each channel for rounding
    (should (string-match-p "^#" (ps/scrollbar--lerp-color from to 0.0)))
    (should (equal (ps/scrollbar--lerp-color from to 0.0) from))
    (should (equal (ps/scrollbar--lerp-color from to 1.0) to))))

(ert-deftest ps/scrollbar--lerp-color-midpoint ()
  "At frac 0.5 the channels are midway (±1 for rounding)."
  (let* ((mid (ps/scrollbar--lerp-color "#000000" "#ffffff" 0.5))
         (rgb (ps/scrollbar--hex-to-rgb mid)))
    ;; Each channel should be near 0.5
    (should (> (nth 0 rgb) 0.49))
    (should (< (nth 0 rgb) 0.51))))

(ert-deftest ps/scrollbar--lerp-color-clamped ()
  "FRAC is clamped to [0,1]: out-of-range FRAC clips to the endpoint."
  (let ((from "#3c3c3c") (to "#ffffff"))
    (should (equal (ps/scrollbar--lerp-color from to -0.5) from))
    (should (equal (ps/scrollbar--lerp-color from to  2.0) to))))

;;; API / face

(ert-deftest ps/scrollbar--api-defined ()
  (should (fboundp 'ps/scrollbar-mode))
  (should (commandp 'ps/scrollbar-mode))
  (should (fboundp 'ps/scrollbar--snap-back))
  (should (fboundp 'ps/scrollbar--hide-now))
  (should (fboundp 'ps/scrollbar--hide))
  (should (fboundp 'ps/scrollbar--lerp-color)))

(ert-deftest ps/scrollbar--face-and-color ()
  (should (facep 'ps/scrollbar-thumb))
  (should (stringp (ps/scrollbar--color))))

(ert-deftest ps/scrollbar--terminal-not-excluded ()
  "eat-mode and term-mode are not in the mode-based exclude list.
Claude Code session buffers are excluded by name (not by mode) -- see
`ps/scrollbar--candidate-window-p' and design-docs/scroll-bars.md."
  (should-not (memq 'eat-mode ps/scrollbar-exclude-modes))
  (should-not (memq 'term-mode ps/scrollbar-exclude-modes)))

(ert-deftest ps/scrollbar--mode-map-binds-fringe-click ()
  (should (keymapp ps/scrollbar-mode-map))
  (should (eq (lookup-key ps/scrollbar-mode-map [right-fringe down-mouse-1])
              #'ps/scrollbar--click-on-fringe)))

(ert-deftest ps/scrollbar--mode-map-binds-vertical-line-click ()
  "Vertical-line click handler is bound; release events call vl-release."
  (should (eq (lookup-key ps/scrollbar-mode-map [vertical-line down-mouse-1])
              #'ps/scrollbar--click-on-vertical-line))
  (should (eq (lookup-key ps/scrollbar-mode-map [vertical-line mouse-1])
              #'ps/scrollbar--vl-release))
  (should (eq (lookup-key ps/scrollbar-mode-map [vertical-line drag-mouse-1])
              #'ps/scrollbar--vl-release))
  (should (eq (lookup-key ps/scrollbar-mode-map [left-fringe drag-mouse-1])
              #'ps/scrollbar--vl-release)))

;;; Hover resolution (sustained-hover fix)

(ert-deftest ps/scrollbar--resolve-hover-disabled-returns-nil ()
  "With hover-reveal off, nothing is computed and nil is returned."
  (let ((ps/scrollbar-show-on-hover nil)
        (ps/scrollbar--last-hover (selected-window))
        (called nil))
    (cl-letf (((symbol-function 'ps/scrollbar--strip-hover-window)
               (lambda () (setq called t) (selected-window))))
      (should-not (ps/scrollbar--resolve-hover t))
      (should-not called))))

(ert-deftest ps/scrollbar--resolve-hover-recomputes-when-moved ()
  "A moved pointer recomputes the hover window and caches it."
  (let ((ps/scrollbar-show-on-hover t)
        (ps/scrollbar--last-hover nil)
        (calls 0))
    (cl-letf (((symbol-function 'ps/scrollbar--strip-hover-window)
               (lambda () (setq calls (1+ calls)) (selected-window))))
      (should (eq (ps/scrollbar--resolve-hover t) (selected-window)))
      (should (= calls 1))
      (should (eq ps/scrollbar--last-hover (selected-window))))))

(ert-deftest ps/scrollbar--resolve-hover-reuses-cache-when-still ()
  "A motionless pointer reuses the cached window WITHOUT recomputing.
This is the regression guard for the fade-while-hovering bug."
  (let ((ps/scrollbar-show-on-hover t)
        (ps/scrollbar--last-hover (selected-window))
        (calls 0))
    (cl-letf (((symbol-function 'ps/scrollbar--strip-hover-window)
               (lambda () (setq calls (1+ calls)) nil)))
      ;; Still hovering, and no expensive recomputation happened.
      (should (eq (ps/scrollbar--resolve-hover nil) (selected-window)))
      (should (= calls 0)))))

(ert-deftest ps/scrollbar--resolve-hover-drops-dead-window ()
  "A cached window that has since died is not returned."
  (let* ((ps/scrollbar-show-on-hover t)
         (dead (with-temp-buffer (split-window)))
         (ps/scrollbar--last-hover dead))
    (delete-window dead)
    (should-not (ps/scrollbar--resolve-hover nil))))

;;; Two-speed tick (activity-driven scheduling)

(ert-deftest ps/scrollbar--want-fast-p-states ()
  "Fast while the pill is visible, fading, or inside the activity window."
  ;; Visible pill -> fast.
  (should (ps/scrollbar--want-fast-p 100.0 t nil nil))
  ;; Mid-fade -> fast (so the fade does not stutter at the idle rate).
  (should (ps/scrollbar--want-fast-p 100.0 nil 99.5 nil))
  ;; Within the post-activity linger -> fast.
  (should (ps/scrollbar--want-fast-p 100.0 nil nil 101.0))
  ;; Fully idle -> slow.
  (should-not (ps/scrollbar--want-fast-p 100.0 nil nil nil))
  ;; Linger expired -> slow.
  (should-not (ps/scrollbar--want-fast-p 100.0 nil nil 99.0)))

(ert-deftest ps/scrollbar--fast-linger-covers-reveal-and-fade ()
  "The linger spans a whole reveal+fade cycle, so speed never drops mid-fade."
  (let ((ps/scrollbar-hide-delay 1.0)
        (ps/scrollbar-fade-duration 0.25))
    (should (= (ps/scrollbar--fast-linger) 1.25))))

(ert-deftest ps/scrollbar--note-activity-does-not-start-a-paused-tick ()
  "Activity must never resurrect a timer that is off (mode off / focus-paused)."
  (let ((ps/scrollbar--timer nil)
        (ps/scrollbar--timer-speed nil)
        (ps/scrollbar--fast-until nil))
    (ps/scrollbar--note-activity)
    ;; Deadline is recorded, but no timer is created.
    (should ps/scrollbar--fast-until)
    (should (null ps/scrollbar--timer))
    (should (null ps/scrollbar--timer-speed))))

(ert-deftest ps/scrollbar--note-activity-escalates-a-running-slow-tick ()
  "A running idle tick is escalated to the fast rate by activity."
  (let* ((ps/scrollbar-tick-interval 0.15)
         (ps/scrollbar-idle-poll-interval 1.0)
         (ps/scrollbar--timer nil)
         (ps/scrollbar--timer-speed nil)
         (ps/scrollbar--fast-until nil))
    (unwind-protect
        (progn
          (ps/scrollbar--start-timer 'slow)
          (should (eq ps/scrollbar--timer-speed 'slow))
          (ps/scrollbar--note-activity)
          (should (eq ps/scrollbar--timer-speed 'fast))
          (should (timerp ps/scrollbar--timer)))
      (ps/scrollbar--stop-timer))))

(ert-deftest ps/scrollbar--stop-timer-clears-speed ()
  (let ((ps/scrollbar--timer nil)
        (ps/scrollbar--timer-speed nil)
        (ps/scrollbar-idle-poll-interval 1.0))
    (ps/scrollbar--start-timer 'slow)
    (should (timerp ps/scrollbar--timer))
    (ps/scrollbar--stop-timer)
    (should (null ps/scrollbar--timer))
    (should (null ps/scrollbar--timer-speed))))

(ert-deftest ps/scrollbar--on-scroll-notes-activity ()
  "The `window-scroll-functions' hook records activity (ignoring its args)."
  (let ((ps/scrollbar--timer nil)
        (ps/scrollbar--timer-speed nil)
        (ps/scrollbar--fast-until nil))
    (ps/scrollbar--on-scroll (selected-window) 1)
    (should ps/scrollbar--fast-until)))

;;; Long-unfocus teardown / reinstall

(defmacro ps/scrollbar--with-clean-focus-state (&rest body)
  "Run BODY with fresh, isolated teardown-related state, restored after."
  (declare (indent 0))
  `(let ((ps/scrollbar-mode t)
         (ps/scrollbar--timer nil)
         (ps/scrollbar--teardown-timer nil)
         (ps/scrollbar--torn-down nil))
     (unwind-protect (progn ,@body)
       (when (timerp ps/scrollbar--timer) (cancel-timer ps/scrollbar--timer))
       (when (timerp ps/scrollbar--teardown-timer)
         (cancel-timer ps/scrollbar--teardown-timer)))))

(ert-deftest ps/scrollbar--focus-loss-arms-teardown-timer ()
  "Losing focus pauses the tick timer and arms the teardown timer."
  (ps/scrollbar--with-clean-focus-state
    (cl-letf (((symbol-function 'frame-focus-state) (lambda (&rest _) nil))
              ((symbol-function 'ps/scrollbar--hide-now) #'ignore))
      (ps/scrollbar--on-focus-change)
      (should (null ps/scrollbar--timer))
      (should (timerp ps/scrollbar--teardown-timer))
      (should-not ps/scrollbar--torn-down))))

(ert-deftest ps/scrollbar--quick-refocus-cancels-teardown-without-tearing-down ()
  "Regaining focus before the teardown delay elapses never tears anything down."
  (ps/scrollbar--with-clean-focus-state
    (cl-letf (((symbol-function 'frame-focus-state) (lambda (&rest _) nil))
              ((symbol-function 'ps/scrollbar--hide-now) #'ignore))
      (ps/scrollbar--on-focus-change))    ; lose focus: arms teardown timer
    (cl-letf (((symbol-function 'frame-focus-state) (lambda (&rest _) t)))
      (ps/scrollbar--on-focus-change))    ; regain focus quickly
    (should (null ps/scrollbar--teardown-timer))
    (should-not ps/scrollbar--torn-down)
    (should (timerp ps/scrollbar--timer))))

(ert-deftest ps/scrollbar--teardown-hooks-removes-size-hook-and-advice ()
  "After the long-unfocus delay, the size-change hook and drag advice are gone,
and the module reports itself torn down."
  (add-hook 'window-size-change-functions #'ps/scrollbar--on-size-change)
  (advice-add 'mouse-drag-vertical-line :around #'ps/scrollbar--resize-advice)
  (unwind-protect
      (progn
        (ps/scrollbar--teardown-hooks)
        (should-not (memq 'ps/scrollbar--on-size-change
                          window-size-change-functions))
        (should-not (advice-member-p 'ps/scrollbar--resize-advice
                                     'mouse-drag-vertical-line))
        (should ps/scrollbar--torn-down))
    (remove-hook 'window-size-change-functions #'ps/scrollbar--on-size-change)
    (advice-remove 'mouse-drag-vertical-line #'ps/scrollbar--resize-advice)
    (setq ps/scrollbar--torn-down nil)))

(ert-deftest ps/scrollbar--reinstall-hooks-undoes-teardown ()
  "Reinstalling after a teardown restores the size-change hook and advice."
  (ps/scrollbar--teardown-hooks)
  (unwind-protect
      (progn
        (ps/scrollbar--reinstall-hooks)
        (should (memq 'ps/scrollbar--on-size-change window-size-change-functions))
        (should (advice-member-p 'ps/scrollbar--resize-advice
                                 'mouse-drag-vertical-line))
        (should-not ps/scrollbar--torn-down))
    (remove-hook 'window-size-change-functions #'ps/scrollbar--on-size-change)
    (advice-remove 'mouse-drag-vertical-line #'ps/scrollbar--resize-advice)
    (setq ps/scrollbar--torn-down nil)))

(ert-deftest ps/scrollbar--refocus-after-teardown-reinstalls-hooks ()
  "Regaining focus after a real teardown reinstalls hooks and resumes the timer."
  (ps/scrollbar--with-clean-focus-state
    (ps/scrollbar--teardown-hooks)
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'frame-focus-state) (lambda (&rest _) t)))
            (ps/scrollbar--on-focus-change))
          (should-not ps/scrollbar--torn-down)
          (should (memq 'ps/scrollbar--on-size-change window-size-change-functions))
          (should (advice-member-p 'ps/scrollbar--resize-advice
                                   'mouse-drag-vertical-line))
          (should (timerp ps/scrollbar--timer)))
      (remove-hook 'window-size-change-functions #'ps/scrollbar--on-size-change)
      (advice-remove 'mouse-drag-vertical-line #'ps/scrollbar--resize-advice))))

;;; Window-layout changes (ps/scrollbar--on-size-change)

(defmacro ps/scrollbar--with-clean-drag-state (&rest body)
  "Run BODY with fresh drag/pill state, cancelling any timer it leaves behind."
  (declare (indent 0))
  `(let ((ps/scrollbar--busy nil)
         (ps/scrollbar--visible-frame nil)
         (ps/scrollbar--drag-in-progress nil)
         (ps/scrollbar--drag-suppress-timer nil)
         (track-mouse nil)
         (hidden 0))
     (cl-letf (((symbol-function 'ps/scrollbar--hide-now)
                (lambda () (setq hidden (1+ hidden)))))
       (unwind-protect (progn ,@body)
         (when (timerp ps/scrollbar--drag-suppress-timer)
           (cancel-timer ps/scrollbar--drag-suppress-timer))))))

(ert-deftest ps/scrollbar--layout-change-hides-the-pill ()
  "A window-layout change hides the pill even with the frame size unchanged.
This is the ediff case: windows open and close inside a frame whose own
pixel size never changes, which used to leave the pill frozen on screen."
  (ps/scrollbar--with-clean-drag-state
    (let ((frame (selected-frame)))
      ;; Seed the size baseline so the frame reads as *not* resized.
      (set-frame-parameter frame 'ps/scrollbar--last-size
                           (cons (frame-pixel-width frame)
                                 (frame-pixel-height frame)))
      (ps/scrollbar--on-size-change frame)
      (should (= hidden 1)))))

(ert-deftest ps/scrollbar--layout-change-does-not-start-drag-suppression ()
  "A programmatic layout change must not freeze the tick.
Starting drag suppression from cold here suppressed `ps/scrollbar--tick'
for the full fallback timeout, which is what stranded the pill on screen."
  (ps/scrollbar--with-clean-drag-state
    (ps/scrollbar--on-size-change (selected-frame))
    (should-not ps/scrollbar--drag-in-progress)
    (should-not ps/scrollbar--drag-suppress-timer)))

(ert-deftest ps/scrollbar--layout-change-extends-an-active-drag ()
  "During a real divider drag the flag is already set, so it is re-armed.
`ps/scrollbar--click-on-vertical-line' sets it before the NS-level drag can
produce its first resize step, so drag suppression is unaffected."
  (ps/scrollbar--with-clean-drag-state
    (setq ps/scrollbar--drag-in-progress t)
    (ps/scrollbar--on-size-change (selected-frame))
    (should ps/scrollbar--drag-in-progress)
    (should (timerp ps/scrollbar--drag-suppress-timer))))

(ert-deftest ps/scrollbar--layout-change-extends-during-track-mouse ()
  "A Lisp-level mouse drag we have no advice on still suppresses the pill."
  (ps/scrollbar--with-clean-drag-state
    (setq track-mouse t)
    (ps/scrollbar--on-size-change (selected-frame))
    (should ps/scrollbar--drag-in-progress)))

(ert-deftest ps/scrollbar--layout-change-clears-scroll-baselines ()
  "Scroll baselines are dropped so a restored layout is not read as a scroll.
`window-start' differs after a window-configuration restore; without this,
the next tick would treat that as a user scroll and reveal a pill."
  (ps/scrollbar--with-clean-drag-state
    (let ((w (selected-window)))
      (set-window-parameter w 'ps/scrollbar--last-start 1234)
      (ps/scrollbar--on-size-change (selected-frame))
      (should-not (window-parameter w 'ps/scrollbar--last-start))
      ;; A nil baseline reports "no scroll" and re-seeds itself.
      (should-not (ps/scrollbar--scrolled-window w))
      (should (window-parameter w 'ps/scrollbar--last-start)))))

(ert-deftest ps/scrollbar--layout-change-ignores-own-frames-and-busy ()
  "Our own pill frames, and our own frame operations, are still skipped."
  (ps/scrollbar--with-clean-drag-state
    (let ((ps/scrollbar--busy t))
      (ps/scrollbar--on-size-change (selected-frame)))
    (should (= hidden 0))
    (cl-letf (((symbol-function 'ps/scrollbar--own-frame-p) (lambda (_) t)))
      (ps/scrollbar--on-size-change (selected-frame)))
    (should (= hidden 0))))

(ert-deftest ps/scrollbar--snap-back-hides-pill-of-dead-window ()
  "A pill whose window has been deleted is hidden, not snapped into place."
  (ps/scrollbar--with-clean-drag-state
    (cl-letf* ((geom-asked nil)
               ((symbol-function 'frame-live-p) (lambda (_) t))
               ((symbol-function 'frame-parameter)
                (lambda (_f param)
                  (pcase param
                    ('ps/scrollbar-window 'a-dead-window)
                    ('ps/scrollbar-geom (setq geom-asked t) '(0 0 6 24))
                    (_ nil)))))
      (setq ps/scrollbar--visible-frame 'pill)
      (ps/scrollbar--snap-back)
      (should (= hidden 1))
      (should-not geom-asked))))

;;; Stuck `track-mouse' after a divider drag

(ert-deftest ps/scrollbar--track-mouse-stuck-after-drag ()
  "`dragging' left over with no drag running is stuck."
  (should (ps/scrollbar--track-mouse-stuck-p 'dragging nil nil)))

(ert-deftest ps/scrollbar--track-mouse-not-stuck-during-transient-map ()
  "A live `mouse-drag-line' still owns its transient map: leave it alone."
  (should-not (ps/scrollbar--track-mouse-stuck-p 'dragging 'some-keymap nil)))

(ert-deftest ps/scrollbar--track-mouse-not-stuck-during-ns-drag ()
  "An NS-level divider drag has no transient map, so honour our own flag."
  (should-not (ps/scrollbar--track-mouse-stuck-p 'dragging nil t)))

(ert-deftest ps/scrollbar--track-mouse-healthy-values-untouched ()
  "Only `dragging' is treated as stuck."
  (should-not (ps/scrollbar--track-mouse-stuck-p nil nil nil))
  (should-not (ps/scrollbar--track-mouse-stuck-p t nil nil))
  ;; `dropping' freezes the pointer too, but drag-and-drop let-binds
  ;; `track-mouse', so clearing it could clobber a live drag.
  (should-not (ps/scrollbar--track-mouse-stuck-p 'dropping nil nil))
  (should-not (ps/scrollbar--track-mouse-stuck-p 'drag-source nil nil)))

(ert-deftest ps/scrollbar--unstick-restores-global-track-mouse ()
  "The hook clears a stuck global without disturbing a buffer-local value.
Reproduces the real failure: `mouse-drag-line' restores `track-mouse' with
a plain `setq', so when a drag ends in a buffer with a local binding the
global keeps the pointer-freezing `dragging'."
  (let ((saved (default-value 'track-mouse)))
    (unwind-protect
        (with-temp-buffer
          (setq-default track-mouse 'dragging)
          (setq-local track-mouse t)    ; as `eat-mode' does
          (let ((ps/scrollbar--drag-in-progress nil)
                (overriding-terminal-local-map nil))
            (ps/scrollbar--unstick-track-mouse))
          (should-not (default-value 'track-mouse))
          (should (eq track-mouse t)))  ; local binding untouched
      (setq-default track-mouse saved))))

(ert-deftest ps/scrollbar--unstick-leaves-live-drag-alone ()
  "The hook must not cut a running drag's motion events short."
  (let ((saved (default-value 'track-mouse)))
    (unwind-protect
        (progn
          (setq-default track-mouse 'dragging)
          (let ((ps/scrollbar--drag-in-progress t)
                (overriding-terminal-local-map nil))
            (ps/scrollbar--unstick-track-mouse))
          (should (eq (default-value 'track-mouse) 'dragging)))
      (setq-default track-mouse saved))))

(provide 'test-ps-scrollbar)
;;; test-ps-scrollbar.el ends here
