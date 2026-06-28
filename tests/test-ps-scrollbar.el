;;; test-ps-scrollbar.el --- ERT tests for ps-scrollbar -*- lexical-binding: t; -*-

(require 'ert)
(add-to-list 'load-path "lisp")
(require 'ps-scrollbar)

;;; Thumb geometry (pixels)

(ert-deftest ps/scrollbar--span-nil-when-content-fits ()
  "No thumb when the whole buffer is visible."
  (should-not (ps/scrollbar--thumb-span 1 1001 1 1001 400 24)))

(ert-deftest ps/scrollbar--span-top ()
  "Scrolled to the top: thumb starts at pixel 0."
  (let ((s (ps/scrollbar--thumb-span 1 1001 1 251 400 24)))
    (should s)
    (should (= (car s) 0))
    (should (= (cdr s) 100))))          ; 250/1000 * 400

(ert-deftest ps/scrollbar--span-middle ()
  "Scrolled to the middle: thumb roughly centered."
  (let ((s (ps/scrollbar--thumb-span 1 1001 376 626 400 24)))
    (should s)
    (should (= (cdr s) 100))
    (should (= (car s) 150))))          ; 375/1000 * 400

(ert-deftest ps/scrollbar--span-bottom-clamped ()
  "At the bottom the thumb never runs past the strip."
  (let ((s (ps/scrollbar--thumb-span 1 1001 751 1001 400 24)))
    (should s)
    (should (<= (+ (car s) (cdr s)) 400))
    (should (= (+ (car s) (cdr s)) 400))))

(ert-deftest ps/scrollbar--span-min-height ()
  "A huge buffer still yields at least the minimum thumb height."
  (let ((s (ps/scrollbar--thumb-span 1 1000001 1 401 400 24)))
    (should s)
    (should (>= (cdr s) 24))))

(ert-deftest ps/scrollbar--span-guards ()
  "Degenerate inputs return nil rather than erroring."
  (should-not (ps/scrollbar--thumb-span 1 1 1 1 400 24))     ; empty buffer
  (should-not (ps/scrollbar--thumb-span 1 1000 1 500 0 24))) ; zero strip

;;; Drag fraction (travel-based, with grab offset)

(ert-deftest ps/scrollbar--drag-frac-endpoints ()
  "Top of travel maps to 0, bottom of travel maps to 1."
  (should (= (ps/scrollbar--drag-frac 100 100 400 100 0) 0.0))
  (should (= (ps/scrollbar--drag-frac 400 100 400 100 0) 1.0))) ; travel = 300

(ert-deftest ps/scrollbar--drag-frac-midpoint-and-offset ()
  "Midpoint is 0.5; the grab offset shifts the mapping."
  (should (= (ps/scrollbar--drag-frac 250 100 400 100 0) 0.5))
  ;; cursor 150px below top, grabbed 50px into the thumb -> (150-50)/300
  (should (< (abs (- (ps/scrollbar--drag-frac 250 100 400 100 50) 0.3333)) 0.001)))

(ert-deftest ps/scrollbar--drag-frac-clamped ()
  "Out-of-range cursor positions clamp to [0,1]; zero travel is safe."
  (should (= (ps/scrollbar--drag-frac 0 100 400 100 0) 0.0))
  (should (= (ps/scrollbar--drag-frac 9999 100 400 100 0) 1.0))
  (should (= (ps/scrollbar--drag-frac 250 100 400 400 0) 0.0))) ; travel <= 0

;;; API / faces / image

(ert-deftest ps/scrollbar--api-defined ()
  (should (fboundp 'ps/scrollbar-mode))
  (should (commandp 'ps/scrollbar-mode))
  (should (commandp 'ps/scrollbar--start-drag)))

(ert-deftest ps/scrollbar--faces-defined ()
  (should (facep 'ps/scrollbar-thumb))
  (should (facep 'ps/scrollbar-thumb-active)))

(ert-deftest ps/scrollbar--image-builds ()
  "The SVG image builder returns an image when SVG is available."
  (skip-unless (image-type-available-p 'svg))
  (let ((img (ps/scrollbar--image 8 400 150 100 "gray60")))
    (should (eq (car img) 'image))))

(ert-deftest ps/scrollbar--terminal-not-excluded ()
  "Terminal majors (e.g. the Claude Code window) keep their scrollbar."
  (should-not (memq 'eat-mode ps/scrollbar-exclude-modes))
  (should-not (memq 'term-mode ps/scrollbar-exclude-modes)))

(provide 'test-ps-scrollbar)
;;; test-ps-scrollbar.el ends here
