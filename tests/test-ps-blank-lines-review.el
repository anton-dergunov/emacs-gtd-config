;;; test-ps-blank-lines-review.el --- Tests for ps-blank-lines-review.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Covers the guards that stand between an Ediff review and the disk, and the
;; Ediff exit contract the loop is built on.  The exit-order test is the point
;; of the file: `ps/blank-lines-review--commit' has to run while buffers A and
;; B are still alive, which depends on a buffer-local `ediff-quit-hook' entry
;; firing ahead of the global `ediff-cleanup-mess'.  That is Ediff's behaviour,
;; not ours, so it is asserted rather than assumed.

;;; Code:

(require 'ert)
(require 'cl-lib)
(add-to-list 'load-path (expand-file-name "lisp" default-directory))
(require 'ps-blank-lines)
(require 'ps-blank-lines-review)

;;; Helpers

(defmacro ps/blank-lines-review-test--with-file (text var &rest body)
  "Bind VAR to a temp file containing TEXT and run BODY, then clean up."
  (declare (indent 2))
  `(let* ((dir (make-temp-file "ps-blr" t))
          (,var (expand-file-name "sample.org" dir)))
     (unwind-protect
         (progn (with-temp-file ,var (insert ,text)) ,@body)
       (dolist (buffer (buffer-list))
         (when (and (buffer-file-name buffer)
                    (string-prefix-p dir (buffer-file-name buffer)))
           (with-current-buffer buffer (set-buffer-modified-p nil))
           (kill-buffer buffer)))
       (delete-directory dir t))))

(defun ps/blank-lines-review-test--result (file scanned proposed)
  "Return a result for FILE proposing PROPOSED in place of SCANNED."
  (ps/blank-lines--make-result
   :file file :relpath (file-name-nondirectory file)
   :sha "0123456789abcdef" :time "2026-08-08T00:00:00" :candidates 1
   :restored 2 :removed 0 :changes nil :whitespace-only 0
   :scanned scanned :proposed proposed))

(defun ps/blank-lines-review-test--read (file)
  "Return FILE's contents on disk."
  (with-temp-buffer (insert-file-contents file) (buffer-string)))

(defvar ps/blank-lines-review-test--damaged "* One\ntext\n* Two\nmore\n")
(defvar ps/blank-lines-review-test--healthy "* One\n\ntext\n\n* Two\nmore\n")

(defmacro ps/blank-lines-review-test--noninteractive (&rest body)
  "Run BODY with Ediff's confirmation prompts answered yes."
  `(cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
             ((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
     ,@body))

;;; Pure guards

(ert-deftest ps/blank-lines-review--counts-blank-lines ()
  (should (= 0 (ps/blank-lines-review-blank-count "* One\ntext\n")))
  (should (= 2 (ps/blank-lines-review-blank-count "* One\n\ntext\n\n* Two\n")))
  ;; Whitespace-only counts as blank, as it does to the parser.
  (should (= 1 (ps/blank-lines-review-blank-count "a\n   \nb\n"))))

(ert-deftest ps/blank-lines-review--only-blank-line-changes-are-safe-to-save ()
  (should (ps/blank-lines-review-safe-to-save-p "* A\ntext\n" "* A\n\ntext\n"))
  (should (ps/blank-lines-review-safe-to-save-p "* A\ntext\n" "* A\ntext\n"))
  ;; A changed word is not a blank line.
  (should-not (ps/blank-lines-review-safe-to-save-p "* A\ntext\n" "* A\n\nTEXT\n"))
  ;; Nor is a deleted heading.
  (should-not (ps/blank-lines-review-safe-to-save-p "* A\n* B\n" "* A\n")))

(ert-deftest ps/blank-lines-review--nothing-to-apply-is-not-actionable ()
  (let ((same (ps/blank-lines-review-test--result "/tmp/x.org" "* A\n" "* A\n"))
        (diff (ps/blank-lines-review-test--result "/tmp/x.org" "* A\n" "* A\n\n"))
        (bad  (ps/blank-lines--make-result :file "/tmp/x.org" :relpath "x.org"
                                           :error 'parse-working
                                           :scanned "* A\n" :proposed nil)))
    (should-not (ps/blank-lines-review-actionable-p same))
    (should (ps/blank-lines-review-actionable-p diff))
    (should-not (ps/blank-lines-review-actionable-p bad))))

(ert-deftest ps/blank-lines-review--staleness-catches-a-changed-file ()
  (ps/blank-lines-review-test--with-file ps/blank-lines-review-test--damaged file
    (let* ((result (ps/blank-lines-review-test--result
                    file ps/blank-lines-review-test--damaged
                    ps/blank-lines-review-test--healthy))
           (buffer (find-file-noselect file)))
      (should-not (ps/blank-lines-review-staleness result buffer))
      ;; Edited on disk since the scan: the proposal no longer describes it.
      (with-current-buffer buffer
        (goto-char (point-max))
        (insert "* Three\n"))
      (should (equal "the buffer has unsaved changes"
                     (ps/blank-lines-review-staleness result buffer)))
      (with-current-buffer buffer (save-buffer))
      (should (equal "the file changed since the scan"
                     (ps/blank-lines-review-staleness result buffer))))))

;;; The Ediff exit contract

(ert-deftest ps/blank-lines-review--local-quit-hook-runs-before-cleanup-mess ()
  "A buffer-local `ediff-quit-hook' entry must see A and B still alive.
The whole write path hangs on this, and it is Ediff's behaviour, not ours."
  (let ((order '()) (alive nil))
    (ps/blank-lines-review-test--noninteractive
     (let ((a (get-buffer-create "*blr-exit-a*"))
           (b (get-buffer-create "*blr-exit-b*"))
           (advice (lambda (&rest _) (push 'cleanup-mess order))))
       (unwind-protect
           (progn
             (with-current-buffer a (erase-buffer) (insert "* One\ntext\n"))
             (with-current-buffer b (erase-buffer) (insert "* One\n\ntext\n"))
             (advice-add 'ediff-cleanup-mess :before advice)
             (let ((control (ediff-buffers a b)))
               (with-current-buffer control
                 (add-hook 'ediff-quit-hook
                           (lambda ()
                             (push 'local-quit-hook order)
                             (setq alive (and (buffer-live-p a) (buffer-live-p b))))
                           nil t)
                 (ediff-quit nil))))
         (advice-remove 'ediff-cleanup-mess advice)
         (dolist (buffer (list "*blr-exit-a*" "*blr-exit-b*"))
           (when (get-buffer buffer) (kill-buffer buffer))))))
    (should (equal '(local-quit-hook cleanup-mess) (nreverse order)))
    (should alive)))

;;; End to end

(defun ps/blank-lines-review-test--commit (result file proposal)
  "Commit PROPOSAL as the reviewed outcome for RESULT's FILE."
  (let ((current (find-file-noselect file))
        (proposed (generate-new-buffer " *test-proposal*")))
    (with-current-buffer proposed (insert proposal))
    (ps/blank-lines-review--commit result current proposed)))

(ert-deftest ps/blank-lines-review--accepting-everything-writes-the-file ()
  "Quitting with the proposal untouched writes the whole proposal."
  (ps/blank-lines-review-test--with-file ps/blank-lines-review-test--damaged file
    (ps/blank-lines-review-test--noninteractive
     (let ((result (ps/blank-lines-review-test--result
                    file ps/blank-lines-review-test--damaged
                    ps/blank-lines-review-test--healthy)))
       (ps/blank-lines-review-test--commit
        result file ps/blank-lines-review-test--healthy)
       (should (equal ps/blank-lines-review-test--healthy
                      (ps/blank-lines-review-test--read file)))
       (should (assoc (file-name-nondirectory file)
                      ps/blank-lines-review--applied))))))

(ert-deftest ps/blank-lines-review--rejecting-everything-writes-nothing ()
  "Rejecting every change leaves the proposal equal to the file, so no write."
  (ps/blank-lines-review-test--with-file ps/blank-lines-review-test--damaged file
    (ps/blank-lines-review-test--noninteractive
     (let ((result (ps/blank-lines-review-test--result
                    file ps/blank-lines-review-test--damaged
                    ps/blank-lines-review-test--healthy)))
       (ps/blank-lines-review-test--commit
        result file ps/blank-lines-review-test--damaged)
       (should (equal ps/blank-lines-review-test--damaged
                      (ps/blank-lines-review-test--read file)))
       (should-not ps/blank-lines-review--applied)
       (should (equal "nothing accepted"
                      (cdr (assoc (file-name-nondirectory file)
                                  ps/blank-lines-review--skipped))))))))

(ert-deftest ps/blank-lines-review--a-partial-acceptance-writes-just-that ()
  "Rejecting one change of two writes the other one and nothing more."
  (ps/blank-lines-review-test--with-file ps/blank-lines-review-test--damaged file
    (ps/blank-lines-review-test--noninteractive
     (let ((result (ps/blank-lines-review-test--result
                    file ps/blank-lines-review-test--damaged
                    ps/blank-lines-review-test--healthy))
           (partial "* One\n\ntext\n* Two\nmore\n"))
       (ps/blank-lines-review-test--commit result file partial)
       (should (equal partial (ps/blank-lines-review-test--read file)))
       (should (equal 1 (cdr (assoc (file-name-nondirectory file)
                                    ps/blank-lines-review--applied))))))))

(ert-deftest ps/blank-lines-review--a-content-edit-is-never-saved-silently ()
  "If the review typed into the proposal, saving takes an explicit yes."
  (ps/blank-lines-review-test--with-file ps/blank-lines-review-test--damaged file
    (let ((result (ps/blank-lines-review-test--result
                   file ps/blank-lines-review-test--damaged
                   ps/blank-lines-review-test--healthy))
          (asked nil))
      (cl-letf (((symbol-function 'yes-or-no-p)
                 (lambda (&rest _) (setq asked t) nil)))
        (ps/blank-lines-review-test--commit
         result file "* One\n\nSOMETHING ELSE\n\n* Two\nmore\n"))
      (should asked)
      (should (equal ps/blank-lines-review-test--damaged
                     (ps/blank-lines-review-test--read file))))))

(ert-deftest ps/blank-lines-review--a-stale-file-is-skipped-not-reverted ()
  "A file edited since the scan must not be overwritten with stale content."
  (ps/blank-lines-review-test--with-file ps/blank-lines-review-test--damaged file
    (ps/blank-lines-review-test--noninteractive
     (let ((result (ps/blank-lines-review-test--result
                    file "* Something\nolder\n"   ; not what is on disk
                    ps/blank-lines-review-test--healthy)))
       (ps/blank-lines-review-start (list result))
       (should (equal ps/blank-lines-review-test--damaged
                      (ps/blank-lines-review-test--read file)))))))

;;; Applying from the report, without Ediff

(ert-deftest ps/blank-lines-review--apply-writes-the-whole-proposal ()
  (ps/blank-lines-review-test--with-file ps/blank-lines-review-test--damaged file
    (let ((result (ps/blank-lines-review-test--result
                   file ps/blank-lines-review-test--damaged
                   ps/blank-lines-review-test--healthy)))
      (should (equal '(t . 2) (ps/blank-lines-review-apply result)))
      (should (equal ps/blank-lines-review-test--healthy
                     (ps/blank-lines-review-test--read file))))))

(ert-deftest ps/blank-lines-review--apply-refuses-a-stale-file ()
  (ps/blank-lines-review-test--with-file ps/blank-lines-review-test--damaged file
    (let* ((result (ps/blank-lines-review-test--result
                    file "* Something\nolder\n"
                    ps/blank-lines-review-test--healthy))
           (outcome (ps/blank-lines-review-apply result)))
      (should-not (car outcome))
      (should (equal "the file changed since the scan" (cdr outcome)))
      (should (equal ps/blank-lines-review-test--damaged
                     (ps/blank-lines-review-test--read file))))))

(ert-deftest ps/blank-lines-review--apply-refuses-a-content-changing-proposal ()
  "The engine cannot produce this, so nothing else would catch it."
  (ps/blank-lines-review-test--with-file ps/blank-lines-review-test--damaged file
    (let* ((result (ps/blank-lines-review-test--result
                    file ps/blank-lines-review-test--damaged
                    "* One\n\nDIFFERENT\n\n* Two\nmore\n"))
           (outcome (ps/blank-lines-review-apply result)))
      (should-not (car outcome))
      (should (equal ps/blank-lines-review-test--damaged
                     (ps/blank-lines-review-test--read file))))))

(ert-deftest ps/blank-lines-review--report-accepts-and-dismisses-a-row ()
  (ps/blank-lines-review-test--with-file ps/blank-lines-review-test--damaged file
    (let ((accept (ps/blank-lines-review-test--result
                   file ps/blank-lines-review-test--damaged
                   ps/blank-lines-review-test--healthy))
          (dismiss (ps/blank-lines-review-test--result
                    file ps/blank-lines-review-test--damaged
                    ps/blank-lines-review-test--healthy)))
      (with-temp-buffer
        (ps-blank-lines-mode)
        (setq ps/blank-lines--results (list accept dismiss)
              ps/blank-lines--rule (ps/blank-lines-rule-empty))
        (ps/blank-lines--render)
        ;; Both rows start pending and carry their result.
        (should (ps/blank-lines--pending-p accept))
        (should (ps/blank-lines--goto-row accept))
        (should (eq accept (ps/blank-lines--result-at-point)))
        (ps/blank-lines-accept-this-file)
        (should (equal 2 (ps/blank-lines-result-applied accept)))
        (should-not (ps/blank-lines--pending-p accept))
        (should (equal ps/blank-lines-review-test--healthy
                       (ps/blank-lines-review-test--read file)))
        ;; A dismissed row is left alone and drops out of the pending set.
        (ps/blank-lines--goto-row dismiss)
        (ps/blank-lines-reject-this-file)
        (should (eq 'rejected (ps/blank-lines-result-applied dismiss)))
        (should-not (ps/blank-lines--pending-p dismiss))
        ;; The view survives: the rows are still rendered, now as outcomes.
        (goto-char (point-min))
        (should (search-forward "blank line(s) restored" nil t))
        (goto-char (point-min))
        (should (search-forward "dismissed" nil t))))))

(ert-deftest ps/blank-lines-review--a-settled-row-cannot-be-applied-twice ()
  (ps/blank-lines-review-test--with-file ps/blank-lines-review-test--damaged file
    (let ((result (ps/blank-lines-review-test--result
                   file ps/blank-lines-review-test--damaged
                   ps/blank-lines-review-test--healthy)))
      (with-temp-buffer
        (ps-blank-lines-mode)
        (setq ps/blank-lines--results (list result)
              ps/blank-lines--rule (ps/blank-lines-rule-empty))
        (ps/blank-lines--render)
        (ps/blank-lines--goto-row result)
        (ps/blank-lines-accept-this-file)
        (ps/blank-lines--goto-row result)
        (should-error (ps/blank-lines-accept-this-file) :type 'user-error)))))

;;; Layout

(ert-deftest ps/blank-lines-review--layout-is-single-frame-and-restored ()
  "The review must not open frames, and must put Ediff's settings back."
  (let ((ediff-window-setup-function #'ediff-setup-windows-multiframe)
        (ediff-split-window-function #'split-window-vertically)
        (ps/blank-lines-review-side-by-side t))
    (ps/blank-lines-review--enter-layout)
    (should (eq ediff-window-setup-function #'ediff-setup-windows-plain))
    (should (eq ediff-split-window-function #'split-window-horizontally))
    (ps/blank-lines-review--exit-layout)
    ;; Restored, including for a user who prefers multiframe elsewhere.
    (should (eq ediff-window-setup-function #'ediff-setup-windows-multiframe))
    (should (eq ediff-split-window-function #'split-window-vertically))
    ;; Exiting twice must not clobber the restored values.
    (ps/blank-lines-review--exit-layout)
    (should (eq ediff-window-setup-function #'ediff-setup-windows-multiframe))))

(ert-deftest ps/blank-lines-review--layout-honours-the-stacked-preference ()
  (let ((ediff-split-window-function #'split-window-horizontally)
        (ps/blank-lines-review-side-by-side nil))
    (ps/blank-lines-review--enter-layout)
    (should (eq ediff-split-window-function #'split-window-vertically))
    (ps/blank-lines-review--exit-layout)))

(ert-deftest ps/blank-lines-review--the-proposal-is-editable-and-in-org-mode ()
  "The right side is what gets written, so it must be editable."
  (ps/blank-lines-review-test--with-file ps/blank-lines-review-test--damaged file
    (let* ((result (ps/blank-lines-review-test--result
                    file ps/blank-lines-review-test--damaged
                    ps/blank-lines-review-test--healthy))
           (buffer (ps/blank-lines-review--proposed-buffer result)))
      (unwind-protect
          (with-current-buffer buffer
            (should-not buffer-read-only)
            (should (eq major-mode 'org-mode)))
        (kill-buffer buffer)))))

(ert-deftest ps/blank-lines-review--the-file-is-read-only-during-a-session ()
  "The left side is a reference; a session must not let it be typed into."
  (ps/blank-lines-review-test--with-file ps/blank-lines-review-test--damaged file
    (ps/blank-lines-review-test--noninteractive
     (let ((result (ps/blank-lines-review-test--result
                    file ps/blank-lines-review-test--damaged
                    ps/blank-lines-review-test--healthy)))
       (ps/blank-lines-review--session result)
       (unwind-protect
           (with-current-buffer (find-file-noselect file)
             (should buffer-read-only)
             (should-error (insert "typed by accident") :type 'buffer-read-only))
         ;; Ediff restores the flag on quit, so cancelling also clears it.
         (ps/blank-lines-review-cancel))
       (with-current-buffer (find-file-noselect file)
         (should-not buffer-read-only))
       ;; Cancelling writes nothing.
       (should (equal ps/blank-lines-review-test--damaged
                      (ps/blank-lines-review-test--read file)))))))

(ert-deftest ps/blank-lines-review--the-report-survives-ediffs-window-takeover ()
  "The report sits in a side window, the one kind `delete-other-windows' keeps."
  (let ((report (get-buffer-create "*blr-report-test*")))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (switch-to-buffer report)
          (let ((side (ps/blank-lines-review--keep-visible report)))
            (should (window-live-p side))
            ;; Right: the file tree owns the left, so the diff replaces the
            ;; tree rather than the report.
            (should (eq 'right (window-parameter side 'window-side)))
            (should (window-parameter side 'no-other-window))
            (should (window-parameter side 'no-delete-other-windows))
            ;; A main window must be selected, or Ediff's own
            ;; `delete-other-windows' would signal on the side window.
            (should-not (window-parameter (selected-window) 'window-side))
            (delete-other-windows)
            (should (window-live-p side))
            (should (eq report (window-buffer side)))))
      (kill-buffer report))))

(ert-deftest ps/blank-lines-review--an-empty-queue-does-not-touch-the-layout ()
  (let ((ediff-window-setup-function #'ediff-setup-windows-multiframe))
    (should (= 0 (ps/blank-lines-review-start nil)))
    (should (eq ediff-window-setup-function #'ediff-setup-windows-multiframe))
    (should-not ps/blank-lines-review--saved)))

(ert-deftest ps/blank-lines-review--commands-exist ()
  (should (commandp 'ps/blank-lines-review-all))
  (should (commandp 'ps/blank-lines-review-this-file))
  (should (commandp 'ps/blank-lines-review-accept-all))
  (should (commandp 'ps/blank-lines-review-abort)))

(provide 'test-ps-blank-lines-review)
;;; test-ps-blank-lines-review.el ends here
