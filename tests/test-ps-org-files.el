;;; test-ps-org-files.el --- ERT tests for ps-org-files -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(add-to-list 'load-path "lisp")
(require 'ps-org-files)

;; `(defvar my-org-base-directory)' with no value, as in ps-org-files.el, only
;; marks the symbol special within that file -- repeat it here so the `let'
;; bindings below are dynamic rather than lexical.
(defvar my-org-base-directory)

(defmacro ps/org-files-test--with-tree (entries &rest body)
  "Create a temp dir containing ENTRIES, bind `dir', run BODY, then clean up.
Each entry is a relative name; names ending in \"/\" become directories, the
rest become empty files (their parent directories are created as needed)."
  (declare (indent 1))
  `(let ((dir (make-temp-file "ps-org-files-" t)))
     (unwind-protect
         (progn
           (dolist (name ,entries)
             (if (string-suffix-p "/" name)
                 (make-directory (expand-file-name name dir) t)
               (let ((file (expand-file-name name dir)))
                 (make-directory (file-name-directory file) t)
                 (with-temp-file file (insert "")))))
           ,@body)
       (delete-directory dir t))))

(defun ps/org-files-test--names (files dir)
  "Return FILES as names relative to DIR."
  (mapcar (lambda (f) (file-relative-name f dir)) files))

;;; -------------------------------------------------------
;;; ps/org-files-root
;;; -------------------------------------------------------

(ert-deftest ps/org-files-test-root-defaults-to-base-directory ()
  "With `ps/org-files-root' unset, the Org base directory is the scan root."
  (let ((ps/org-files-root nil)
        (my-org-base-directory "/tmp/org-base/"))
    (should (equal (ps/org-files-root) "/tmp/org-base/"))))

(ert-deftest ps/org-files-test-root-override-wins ()
  "An explicit `ps/org-files-root' overrides the Org base directory."
  (let ((ps/org-files-root "/tmp/other/")
        (my-org-base-directory "/tmp/org-base/"))
    (should (equal (ps/org-files-root) "/tmp/other/"))))

;;; -------------------------------------------------------
;;; ps/org-files-in-directory
;;; -------------------------------------------------------

(ert-deftest ps/org-files-test-scan-is-recursive ()
  "Files are found at any depth, not just directly in the scan root."
  (ps/org-files-test--with-tree '("Inbox.org" "ML/Deep.org" "ML/older/Test.org")
    (should (equal (ps/org-files-test--names (ps/org-files-in-directory dir) dir)
                   '("Inbox.org" "ML/Deep.org" "ML/older/Test.org")))))

(ert-deftest ps/org-files-test-scan-ignores-non-org ()
  "Only .org files are returned."
  (ps/org-files-test--with-tree '("Work/Career.org" "Work/notes.md" "AGENTS.md")
    (should (equal (ps/org-files-test--names (ps/org-files-in-directory dir) dir)
                   '("Work/Career.org")))))

(ert-deftest ps/org-files-test-excluded-directories-are-pruned ()
  "Journal/, Archive/ and dotted directories are skipped, subtrees included."
  (ps/org-files-test--with-tree '("Work/Career.org"
                                  "Journal/20260715.org"
                                  "Archive/Old.org"
                                  ".git/config.org"
                                  "Work/Archive/Done.org")
    (should (equal (ps/org-files-test--names (ps/org-files-in-directory dir) dir)
                   '("Work/Career.org")))))

(ert-deftest ps/org-files-test-excluded-files-are-dropped ()
  "init.org and workspace.org never enter the scan, at any depth."
  (ps/org-files-test--with-tree '("init.org" "workspace.org" "Inbox.org"
                                  "Work/workspace.org" "Work/Career.org")
    (should (equal (ps/org-files-test--names (ps/org-files-in-directory dir) dir)
                   '("Inbox.org" "Work/Career.org")))))

(ert-deftest ps/org-files-test-exclusions-are-customizable ()
  "Emptying the exclusion lists brings the excluded files back."
  (ps/org-files-test--with-tree '("init.org" "Journal/20260715.org")
    (let ((ps/org-files-exclude-directories '("\\`\\."))
          (ps/org-files-exclude-files nil))
      (should (equal (ps/org-files-test--names (ps/org-files-in-directory dir) dir)
                     '("Journal/20260715.org" "init.org"))))))

(ert-deftest ps/org-files-test-scan-is-sorted ()
  "Results come back in a stable sorted order regardless of creation order."
  (ps/org-files-test--with-tree '("Work/Prep.org" "Admin/Home.org" "ML/Deep.org")
    (should (equal (ps/org-files-test--names (ps/org-files-in-directory dir) dir)
                   '("Admin/Home.org" "ML/Deep.org" "Work/Prep.org")))))

(ert-deftest ps/org-files-test-missing-directory-is-empty ()
  "A nil or nonexistent directory yields nil rather than an error."
  (should-not (ps/org-files-in-directory nil))
  (should-not (ps/org-files-in-directory "/nonexistent/ps-org-files/")))

;;; -------------------------------------------------------
;;; ps/org-files-in-scope-p
;;; -------------------------------------------------------

(ert-deftest ps/org-files-test-in-scope-p-accepts-root-level-file ()
  "A .org file directly under the root is in scope."
  (ps/org-files-test--with-tree '("Inbox.org")
    (should (ps/org-files-in-scope-p (expand-file-name "Inbox.org" dir) dir))))

(ert-deftest ps/org-files-test-in-scope-p-accepts-nested-file ()
  "A .org file nested under the root is in scope."
  (ps/org-files-test--with-tree '("Work/Career.org")
    (should (ps/org-files-in-scope-p (expand-file-name "Work/Career.org" dir) dir))))

(ert-deftest ps/org-files-test-in-scope-p-rejects-wrong-extension ()
  "A non-.org file is never in scope."
  (ps/org-files-test--with-tree '("Work/notes.md")
    (should-not (ps/org-files-in-scope-p (expand-file-name "Work/notes.md" dir) dir))))

(ert-deftest ps/org-files-test-in-scope-p-rejects-outside-root ()
  "A file outside ROOT entirely is not in scope."
  (ps/org-files-test--with-tree '("Inbox.org")
    (should-not (ps/org-files-in-scope-p "/tmp/elsewhere/Other.org" dir))))

(ert-deftest ps/org-files-test-in-scope-p-rejects-excluded-file ()
  "init.org and workspace.org are excluded at any depth."
  (ps/org-files-test--with-tree '("init.org" "Work/workspace.org")
    (should-not (ps/org-files-in-scope-p (expand-file-name "init.org" dir) dir))
    (should-not (ps/org-files-in-scope-p (expand-file-name "Work/workspace.org" dir) dir))))

(ert-deftest ps/org-files-test-in-scope-p-rejects-excluded-directory ()
  "A file under Journal/, Archive/, or a dotted directory is excluded."
  (ps/org-files-test--with-tree '("Journal/20260715.org"
                                  "Work/Archive/Done.org"
                                  ".git/config.org")
    (should-not (ps/org-files-in-scope-p (expand-file-name "Journal/20260715.org" dir) dir))
    (should-not (ps/org-files-in-scope-p (expand-file-name "Work/Archive/Done.org" dir) dir))
    (should-not (ps/org-files-in-scope-p (expand-file-name ".git/config.org" dir) dir))))

(ert-deftest ps/org-files-test-in-scope-p-uses-buffer-defaults ()
  "With no FILE/ROOT args, `buffer-file-name' and `ps/org-files-root' are used."
  (ps/org-files-test--with-tree '("Inbox.org")
    (let ((ps/org-files-root dir))
      (let ((buf (find-file-noselect (expand-file-name "Inbox.org" dir))))
        (unwind-protect
            (with-current-buffer buf
              (should (ps/org-files-in-scope-p)))
          (kill-buffer buf))))))

(ert-deftest ps/org-files-test-in-scope-p-matches-in-directory-scan ()
  "The predicate agrees exactly with `ps/org-files-in-directory''s own filtering."
  (ps/org-files-test--with-tree '("Inbox.org" "Work/Career.org" "init.org"
                                  "Journal/20260715.org" "Work/notes.md")
    (let* ((all-org (directory-files-recursively dir "\\.org\\'"))
           (expected (sort (seq-filter (lambda (f) (ps/org-files-in-scope-p f dir)) all-org)
                            #'string<)))
      (should (equal (ps/org-files-in-directory dir) expected)))))

;;; -------------------------------------------------------
;;; ps/org-files-all
;;; -------------------------------------------------------

(ert-deftest ps/org-files-test-all-scans-the-root ()
  "`ps/org-files-all' is the scan applied to `ps/org-files-root'."
  (ps/org-files-test--with-tree '("Inbox.org" "Work/Career.org")
    (let ((ps/org-files-root dir))
      (should (equal (ps/org-files-test--names (ps/org-files-all) dir)
                     '("Inbox.org" "Work/Career.org"))))))

(provide 'test-ps-org-files)
;;; test-ps-org-files.el ends here
