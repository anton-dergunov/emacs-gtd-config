;;; test-ps-tags.el --- ERT tests for ps-tags -*- lexical-binding: t; -*-

(require 'ert)
(add-to-list 'load-path "lisp")
(require 'ps-tags)

(ert-deftest ps/tags--settings-inline-adds-no-padding ()
  "`inline' puts tags at column 0 and turns auto-alignment off.
Column 0 is what keeps the tag pill from ever being pushed past the window
edge and split by the visual-line wrap."
  (should (equal (ps/tags--settings 'inline -77) '(0 . nil))))

(ert-deftest ps/tags--settings-right-keeps-the-column ()
  "`right' passes the configured column through and re-enables auto-alignment."
  (should (equal (ps/tags--settings 'right -77) '(-77 . t)))
  (should (equal (ps/tags--settings 'right 60) '(60 . t))))

(ert-deftest ps/tags--settings-unknown-degrades-to-inline ()
  "An unrecognised alignment falls back to the layout that cannot overflow."
  (should (equal (ps/tags--settings 'sideways -77) '(0 . nil)))
  (should (equal (ps/tags--settings nil -77) '(0 . nil))))

(ert-deftest ps/tags-apply-sets-the-variables-buffer-locally ()
  "`ps/tags-apply' only ever touches the current buffer."
  (with-temp-buffer
    (let ((ps/tags-alignment 'right)
          (ps/tags-column -60))
      (ps/tags-apply)
      (should (local-variable-p 'org-tags-column))
      (should (local-variable-p 'org-auto-align-tags))
      (should (equal org-tags-column -60))
      (should (equal org-auto-align-tags t))))
  (with-temp-buffer
    (should-not (local-variable-p 'org-tags-column))))

;;; test-ps-tags.el ends here
