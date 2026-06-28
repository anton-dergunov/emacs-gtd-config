;;; test-ps-scrollbar.el --- ERT tests for ps-scrollbar -*- lexical-binding: t; -*-

(require 'ert)
(add-to-list 'load-path "lisp")
(require 'ps-scrollbar)

;;; Geometry

(ert-deftest ps/scrollbar--geometry-nil-when-content-fits ()
  "No thumb when the whole buffer is visible."
  (should-not (ps/scrollbar--thumb-geometry 1 100 1 100 40 1)))

(ert-deftest ps/scrollbar--geometry-top ()
  "Scrolled to the top: thumb starts at line 0."
  (let ((g (ps/scrollbar--thumb-geometry 1 1000 1 251 40 1)))
    (should g)
    (should (= (car g) 0))
    ;; ~250/1000 of 40 lines visible -> ~10 lines tall.
    (should (= (cdr g) 10))))

(ert-deftest ps/scrollbar--geometry-middle ()
  "Scrolled to the middle: thumb roughly centered."
  (let ((g (ps/scrollbar--thumb-geometry 1 1000 376 626 40 1)))
    (should g)
    (should (= (cdr g) 10))
    ;; top-frac ~375/999 -> floor(0.375*40)=15
    (should (= (car g) 15))))

(ert-deftest ps/scrollbar--geometry-bottom-clamped ()
  "At the bottom the thumb never runs past the window body."
  (let* ((g (ps/scrollbar--thumb-geometry 1 1000 751 1000 40 1)))
    (should g)
    (should (<= (+ (car g) (cdr g)) 40))))

(ert-deftest ps/scrollbar--geometry-min-thumb ()
  "A huge buffer still yields at least the minimum thumb height."
  (let ((g (ps/scrollbar--thumb-geometry 1 1000000 1 41 40 3)))
    (should g)
    (should (>= (cdr g) 3))))

(ert-deftest ps/scrollbar--geometry-guards ()
  "Degenerate inputs return nil rather than erroring."
  (should-not (ps/scrollbar--thumb-geometry 1 1 1 1 40 1))   ; empty buffer
  (should-not (ps/scrollbar--thumb-geometry 1 1000 1 500 0 1))) ; zero body

;;; API / faces / bitmaps

(ert-deftest ps/scrollbar--api-defined ()
  "Public entry points exist."
  (should (fboundp 'ps/scrollbar-mode))
  (should (commandp 'ps/scrollbar-mode))
  (should (commandp 'ps/scrollbar--start-drag)))

(ert-deftest ps/scrollbar--faces-defined ()
  (should (facep 'ps/scrollbar-thumb))
  (should (facep 'ps/scrollbar-thumb-active)))

(ert-deftest ps/scrollbar--bitmaps-defined ()
  "Defining the bitmaps registers all four shapes."
  (skip-unless (fboundp 'define-fringe-bitmap)) ; unavailable on emacs-nox
  (ps/scrollbar--define-bitmaps)
  (should (fringe-bitmap-p 'ps/scrollbar--thumb-mid))
  (should (fringe-bitmap-p 'ps/scrollbar--thumb-top))
  (should (fringe-bitmap-p 'ps/scrollbar--thumb-bottom))
  (should (fringe-bitmap-p 'ps/scrollbar--thumb-single)))

(ert-deftest ps/scrollbar--terminal-not-excluded ()
  "Terminal majors (e.g. the Claude Code window) keep their scrollbar."
  (should-not (memq 'eat-mode ps/scrollbar-exclude-modes))
  (should-not (memq 'term-mode ps/scrollbar-exclude-modes)))

(provide 'test-ps-scrollbar)
;;; test-ps-scrollbar.el ends here
