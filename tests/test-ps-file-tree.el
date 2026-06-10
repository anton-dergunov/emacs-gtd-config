;;; test-ps-file-tree.el --- ERT tests for ps-file-tree -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(add-to-list 'load-path "lisp")
(require 'ps-file-tree)

;;; -------------------------------------------------------
;;; ps/file-tree--ignored-p
;;; -------------------------------------------------------

(ert-deftest ps/file-tree--ignored-default-hides-init-org ()
  "init.org is hidden by the default ignore list."
  (let ((ps/file-tree-ignored-files (default-value 'ps/file-tree-ignored-files)))
    (should (ps/file-tree--ignored-p "init.org" "/some/path/init.org"))))

(ert-deftest ps/file-tree--ignored-default-hides-dotfiles ()
  "Dotfiles are hidden by the default ignore list."
  (let ((ps/file-tree-ignored-files (default-value 'ps/file-tree-ignored-files)))
    (should (ps/file-tree--ignored-p ".git" "/some/path/.git"))))

(ert-deftest ps/file-tree--ignored-default-keeps-regular-org-files ()
  "A regular Org file is not hidden by the default ignore list."
  (let ((ps/file-tree-ignored-files (default-value 'ps/file-tree-ignored-files)))
    (should-not (ps/file-tree--ignored-p "Career.org" "/some/path/Career.org"))))

(ert-deftest ps/file-tree--ignored-respects-customization ()
  "Custom regexps in `ps/file-tree-ignored-files' are honored."
  (let ((ps/file-tree-ignored-files '("\\`Secret\\.org\\'")))
    (should (ps/file-tree--ignored-p "Secret.org" "/some/path/Secret.org"))
    (should-not (ps/file-tree--ignored-p "init.org" "/some/path/init.org"))))

;;; -------------------------------------------------------
;;; ps/file-tree--list-subdirs
;;; -------------------------------------------------------

(defmacro ps/file-tree-test--with-base-dir (entries &rest body)
  "Create a temp dir containing ENTRIES, bind `dir', run BODY, then clean up.
Each entry is a name; names ending in \"/\" are created as subdirectories,
others as empty files."
  (declare (indent 1))
  `(let ((dir (make-temp-file "ps-file-tree-" t)))
     (unwind-protect
         (progn
           (dolist (name ,entries)
             (if (string-suffix-p "/" name)
                 (make-directory (expand-file-name name dir) t)
               (with-temp-file (expand-file-name name dir) (insert ""))))
           ,@body)
       (delete-directory dir t))))

(ert-deftest ps/file-tree--list-subdirs-returns-only-visible-dirs ()
  "Only non-ignored subdirectories are returned, files are excluded."
  (let ((ps/file-tree-ignored-files (default-value 'ps/file-tree-ignored-files)))
    (ps/file-tree-test--with-base-dir '("Areas/" "Current/" ".git/" "notes.org")
      (let ((names (mapcar #'car (ps/file-tree--list-subdirs dir))))
        (should (equal names '("Areas" "Current")))))))

(ert-deftest ps/file-tree--list-subdirs-sorted-by-name ()
  "Entries are sorted alphabetically by name."
  (let ((ps/file-tree-ignored-files (default-value 'ps/file-tree-ignored-files)))
    (ps/file-tree-test--with-base-dir '("Vision/" "Areas/" "Current/")
      (let ((names (mapcar #'car (ps/file-tree--list-subdirs dir))))
        (should (equal names '("Areas" "Current" "Vision")))))))

(ert-deftest ps/file-tree--list-subdirs-entries-are-abs-paths ()
  "Each entry maps NAME to an absolute path of the subdirectory."
  (let ((ps/file-tree-ignored-files (default-value 'ps/file-tree-ignored-files)))
    (ps/file-tree-test--with-base-dir '("Areas/")
      (let ((entry (car (ps/file-tree--list-subdirs dir))))
        (should (equal (car entry) "Areas"))
        (should (file-name-absolute-p (cdr entry)))
        (should (string-suffix-p "Areas" (cdr entry)))))))

(ert-deftest ps/file-tree--list-subdirs-missing-dir ()
  "A non-existent base directory yields nil."
  (let ((ps/file-tree-ignored-files (default-value 'ps/file-tree-ignored-files)))
    (should (null (ps/file-tree--list-subdirs "/no/such/dir/at/all")))))

;;; test-ps-file-tree.el ends here
