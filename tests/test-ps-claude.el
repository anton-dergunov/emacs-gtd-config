;;; test-ps-claude.el --- ERT tests for ps-claude -*- lexical-binding: t; -*-

(require 'ert)
(add-to-list 'load-path "lisp")
(require 'ps-claude)

;;; Session buffer detection

(ert-deftest ps/claude-test-session-buffer-p-name ()
  "Recognizes the `*claude-code[...]*' naming convention from a string."
  (should (ps/claude--session-buffer-p "*claude-code[my-project]*"))
  (should-not (ps/claude--session-buffer-p "*scratch*"))
  (should-not (ps/claude--session-buffer-p "claude-code[my-project]")))

(ert-deftest ps/claude-test-session-buffer-p-buffer ()
  "Recognizes the naming convention from a live buffer object."
  (let ((buf (generate-new-buffer "*claude-code[demo]*")))
    (unwind-protect
        (should (ps/claude--session-buffer-p buf))
      (kill-buffer buf)))
  (let ((buf (generate-new-buffer "*not-claude*")))
    (unwind-protect
        (should-not (ps/claude--session-buffer-p buf))
      (kill-buffer buf))))

(ert-deftest ps/claude-test-session-buffer-p-non-string ()
  "Returns nil for nil/non-string, non-buffer input rather than erroring."
  (should-not (ps/claude--session-buffer-p nil)))

;;; Resize debounce scheduling

(ert-deftest ps/claude-test-on-window-size-change-no-claude-buffer ()
  "No timer is scheduled when no Claude session buffer is displayed."
  (let ((ps/claude--resize-timer nil))
    (ps/claude--on-window-size-change (selected-frame))
    (should-not ps/claude--resize-timer)))

(provide 'test-ps-claude)
;;; test-ps-claude.el ends here
