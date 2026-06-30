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

;;; API / face

(ert-deftest ps/scrollbar--api-defined ()
  (should (fboundp 'ps/scrollbar-mode))
  (should (commandp 'ps/scrollbar-mode))
  (should (fboundp 'ps/scrollbar--snap-back)))

(ert-deftest ps/scrollbar--face-and-color ()
  (should (facep 'ps/scrollbar-thumb))
  (should (stringp (ps/scrollbar--color))))

(ert-deftest ps/scrollbar--terminal-not-excluded ()
  "Terminal majors (e.g. the Claude Code window) keep their pill."
  (should-not (memq 'eat-mode ps/scrollbar-exclude-modes))
  (should-not (memq 'term-mode ps/scrollbar-exclude-modes)))

(provide 'test-ps-scrollbar)
;;; test-ps-scrollbar.el ends here
