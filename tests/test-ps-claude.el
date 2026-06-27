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

;;; Working directory override

(defvar my-org-base-directory)

(ert-deftest ps/claude-test-working-directory-uses-org-base ()
  "Always returns the expanded `my-org-base-directory', not the project root."
  (let ((my-org-base-directory "~/org/"))
    (should (equal (ps/claude--working-directory)
                   (expand-file-name "~/org/")))))

;;; Buffer project key for selection / active-buffer tracking

(ert-deftest ps/claude-test-buffer-under-org-base-p ()
  "True only for file-backed buffers under `my-org-base-directory'."
  (let ((my-org-base-directory "/tmp/org-base/"))
    (with-temp-buffer
      (setq buffer-file-name "/tmp/org-base/Areas/Inbox.org")
      (should (ps/claude--buffer-under-org-base-p)))
    (with-temp-buffer
      (setq buffer-file-name "/tmp/other/file.el")
      (should-not (ps/claude--buffer-under-org-base-p)))
    (with-temp-buffer
      (setq buffer-file-name nil)
      (should-not (ps/claude--buffer-under-org-base-p)))))

(ert-deftest ps/claude-test-buffer-project-advice-under-base ()
  "Under the Org base, returns the base and never calls ORIG-FN."
  (let ((my-org-base-directory "/tmp/org-base/"))
    (with-temp-buffer
      (setq buffer-file-name "/tmp/org-base/Areas/Inbox.org")
      (should (equal (ps/claude--buffer-project-advice
                      (lambda (&rest _) (error "ORIG-FN should not be called")))
                     (expand-file-name "/tmp/org-base/"))))))

(ert-deftest ps/claude-test-buffer-project-advice-falls-through ()
  "Outside the Org base, delegates to ORIG-FN unchanged."
  (let ((my-org-base-directory "/tmp/org-base/"))
    (with-temp-buffer
      (setq buffer-file-name "/tmp/elsewhere/x.el")
      (should (equal (ps/claude--buffer-project-advice (lambda (&rest _) "ORIG"))
                     "ORIG")))))

;;; Silent reload of stale, unmodified buffers

(ert-deftest ps/claude-test-revert-stale-skips-modified ()
  "A modified buffer is left alone so genuine edit conflicts still prompt."
  (let* ((dir (make-temp-file "ps-claude-" t))
         (my-org-base-directory (file-name-as-directory dir))
         (file (expand-file-name "note.org" dir)))
    (unwind-protect
        (let ((buf (find-file-noselect file)))
          (unwind-protect
              (with-current-buffer buf
                (insert "local edit\n")
                (should (buffer-modified-p))
                (with-temp-file file (insert "changed on disk\n"))
                (set-file-times file (time-add (current-time) 100))
                (ps/claude--revert-stale-unmodified file)
                (should (buffer-modified-p))
                (should (string-match-p "local edit" (buffer-string))))
            (set-buffer-modified-p nil)
            (kill-buffer buf)))
      (delete-directory dir t))))

(ert-deftest ps/claude-test-revert-stale-reverts-unmodified ()
  "An unmodified, stale buffer is reverted to the on-disk content."
  (let* ((dir (make-temp-file "ps-claude-" t))
         (my-org-base-directory (file-name-as-directory dir))
         (file (expand-file-name "note.org" dir)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "original\n"))
          (let ((buf (find-file-noselect file)))
            (unwind-protect
                (with-current-buffer buf
                  (should-not (buffer-modified-p))
                  (with-temp-file file (insert "new content\n"))
                  (set-file-times file (time-add (current-time) 100))
                  (should-not (verify-visited-file-modtime buf))
                  (ps/claude--revert-stale-unmodified file)
                  (should (string-match-p "new content" (buffer-string)))
                  (should-not (buffer-modified-p)))
              (set-buffer-modified-p nil)
              (kill-buffer buf))))
      (delete-directory dir t))))

(ert-deftest ps/claude-test-revert-stale-ignores-outside-base ()
  "Paths outside `my-org-base-directory' are not touched."
  (let ((my-org-base-directory "/tmp/org-base/"))
    ;; No buffer visits this path; should simply do nothing without error.
    (should-not (ps/claude--revert-stale-unmodified "/tmp/elsewhere/x.org"))
    (should-not (ps/claude--revert-stale-unmodified nil))))

;;; eat output-queue crash guard

(ert-deftest ps/claude-test-eat-output-guard-passes-value ()
  "Normal calls pass their return value through untouched."
  (let ((ps/claude--resize-timer nil))
    (should (equal (ps/claude--eat-output-guard (lambda (&rest _) 'ok) 'arg) 'ok))
    (should-not ps/claude--resize-timer)))

(ert-deftest ps/claude-test-eat-output-guard-swallows-range-error ()
  "An `args-out-of-range' is swallowed and a resync is scheduled."
  (let ((ps/claude--resize-timer nil))
    (should-not (ps/claude--eat-output-guard
                 (lambda (&rest _) (signal 'args-out-of-range '("s" -1 2)))))
    (should (timerp ps/claude--resize-timer))
    (when (timerp ps/claude--resize-timer)
      (cancel-timer ps/claude--resize-timer))))

(provide 'test-ps-claude)
;;; test-ps-claude.el ends here
