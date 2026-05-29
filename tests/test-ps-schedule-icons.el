;;; test-ps-schedule-icons.el --- ERT tests for ps-schedule-icons -*- lexical-binding: t; -*-

(require 'ert)
(require 'org)
(add-to-list 'load-path "lisp")
(require 'ps-schedule-icons)

(defmacro ps/schedule-icons-test--in-org (content &rest body)
  "Run BODY in a temp `org-mode' buffer with CONTENT, icons enabled + fontified."
  (declare (indent 1))
  `(with-temp-buffer
     (let ((org-mode-hook nil)) (org-mode))
     (insert ,content)
     (ps/schedule-icons-enable)
     (font-lock-ensure)
     (goto-char (point-min))
     ,@body))

(defconst ps/schedule-icons-test--sample
  "* Task A
SCHEDULED: <2026-05-04 Mon>
* Task B
DEADLINE: <2026-05-06 Wed>
")

;;; -------------------------------------------------------
;;; defcustom defaults
;;; -------------------------------------------------------

(ert-deftest ps/schedule-icons--defaults ()
  "The icon and face defcustoms have the documented defaults."
  (should (equal ps/schedule-icons-scheduled "⏱ "))
  (should (equal ps/schedule-icons-deadline "⚑ "))
  (should (equal ps/schedule-icons-face '(:foreground "#000000" :weight bold))))

;;; -------------------------------------------------------
;;; display replacement
;;; -------------------------------------------------------

(ert-deftest ps/schedule-icons--scheduled-gets-icon ()
  "SCHEDULED: receives the scheduled icon as its display property."
  (ps/schedule-icons-test--in-org ps/schedule-icons-test--sample
    (should (search-forward "SCHEDULED:" nil t))
    (should (equal (get-text-property (match-beginning 0) 'display)
                   ps/schedule-icons-scheduled))))

(ert-deftest ps/schedule-icons--deadline-gets-icon ()
  "DEADLINE: receives the deadline icon as its display property."
  (ps/schedule-icons-test--in-org ps/schedule-icons-test--sample
    (should (search-forward "DEADLINE:" nil t))
    (should (equal (get-text-property (match-beginning 0) 'display)
                   ps/schedule-icons-deadline))))

(ert-deftest ps/schedule-icons--applies-face ()
  "The configured face spec is applied to the replaced keyword."
  (ps/schedule-icons-test--in-org ps/schedule-icons-test--sample
    (should (search-forward "SCHEDULED:" nil t))
    (should (equal (get-text-property (match-beginning 0) 'face)
                   ps/schedule-icons-face))))

(ert-deftest ps/schedule-icons--custom-icon-honored ()
  "A let-bound custom scheduled icon is used during fontification."
  (let ((ps/schedule-icons-scheduled "S> "))
    (ps/schedule-icons-test--in-org "* T\nSCHEDULED: <2026-05-04 Mon>\n"
      (should (search-forward "SCHEDULED:" nil t))
      (should (equal (get-text-property (match-beginning 0) 'display) "S> ")))))

(ert-deftest ps/schedule-icons--custom-face-honored ()
  "A let-bound custom face spec is applied during fontification."
  (let ((ps/schedule-icons-face '(:foreground "red")))
    (ps/schedule-icons-test--in-org "* T\nDEADLINE: <2026-05-06 Wed>\n"
      (should (search-forward "DEADLINE:" nil t))
      (should (equal (get-text-property (match-beginning 0) 'face)
                     '(:foreground "red"))))))

;;; test-ps-schedule-icons.el ends here
