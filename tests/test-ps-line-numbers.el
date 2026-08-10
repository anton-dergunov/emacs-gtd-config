;;; test-ps-line-numbers.el --- ERT tests for ps-line-numbers -*- lexical-binding: t; -*-

(require 'ert)
(add-to-list 'load-path "lisp")
(require 'ps-line-numbers)

(ert-deftest ps/line-numbers-test-default-spec ()
  "The gutter face gets both the inherited colour and the scaled height."
  (should (equal (ps/line-numbers--spec 0.8 'shadow t)
                 '(:inherit shadow :height 0.8))))

(ert-deftest ps/line-numbers-test-current-line-spec-has-no-height ()
  "The current-line entry carries colour only.
`line-number-current-line' inherits `line-number', and `:inherit' resolves
through `face-remapping-alist', so a height here would multiply with the
one on `line-number' and render the current line smaller than the rest."
  (should (equal (ps/line-numbers--spec 0.8 'shadow nil)
                 '(:inherit shadow))))

(ert-deftest ps/line-numbers-test-inherit-precedes-height ()
  "`:inherit' comes first, so a height on the inherited face cannot win."
  (let ((spec (ps/line-numbers--spec 0.8 'shadow t)))
    (should (memq :height (memq :inherit spec)))))

(ert-deftest ps/line-numbers-test-scale-of-one-omits-height ()
  "A scale of 1 means \"leave the size alone\"."
  (should (equal (ps/line-numbers--spec 1.0 'shadow t) '(:inherit shadow)))
  (should (equal (ps/line-numbers--spec 1 'shadow t) '(:inherit shadow))))

(ert-deftest ps/line-numbers-test-nonsense-scale-omits-height ()
  "A zero or negative height is not a valid face attribute; drop it."
  (should (equal (ps/line-numbers--spec 0 'shadow t) '(:inherit shadow)))
  (should (equal (ps/line-numbers--spec -1.0 'shadow t) '(:inherit shadow))))

(ert-deftest ps/line-numbers-test-no-face-omits-inherit ()
  "nil means \"leave the colours alone\"; an undefined face is ignored too."
  (should (equal (ps/line-numbers--spec 0.8 nil t) '(:height 0.8)))
  (should (equal (ps/line-numbers--spec 0.8 'ps/line-numbers-no-such-face t)
                 '(:height 0.8))))

(ert-deftest ps/line-numbers-test-neutral-settings-give-no-spec ()
  "Nothing to change means no remapping at all, not an empty one."
  (should-not (ps/line-numbers--spec 1.0 nil t))
  (should-not (ps/line-numbers--spec 1.0 nil nil)))

;;; test-ps-line-numbers.el ends here
