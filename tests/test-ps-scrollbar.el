;;; test-ps-scrollbar.el --- ERT tests for ps-scrollbar -*- lexical-binding: t; -*-

(require 'ert)
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
  "Vertical-line click handler is bound; release events are swallowed."
  (should (eq (lookup-key ps/scrollbar-mode-map [vertical-line down-mouse-1])
              #'ps/scrollbar--click-on-vertical-line))
  (should (eq (lookup-key ps/scrollbar-mode-map [vertical-line mouse-1])
              #'ignore))
  (should (eq (lookup-key ps/scrollbar-mode-map [vertical-line drag-mouse-1])
              #'ignore)))

(provide 'test-ps-scrollbar)
;;; test-ps-scrollbar.el ends here
