;;; test-ps-file-tree.el --- ERT tests for ps-file-tree -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(add-to-list 'load-path "lisp")
(require 'ps-file-tree)

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
;;; ps/file-tree--set-hidden-p / file sets
;;; -------------------------------------------------------

(ert-deftest ps/file-tree--set-hidden-default-all-shows-everything ()
  "The default \"All\" set (nil/nil) hides nothing."
  (let ((ps/file-tree-file-sets '(("All" . (:include nil :exclude nil))))
        (ps/file-tree-current-set "All"))
    (should-not (ps/file-tree--set-hidden-p "/base/Areas/Secret.org"))))

(ert-deftest ps/file-tree--set-hidden-exclude-only ()
  "A path matching :exclude is hidden; non-matching paths are not."
  (let ((ps/file-tree-file-sets
         '(("NoSecrets" . (:include nil :exclude ("Secret")))))
        (ps/file-tree-current-set "NoSecrets"))
    (should (ps/file-tree--set-hidden-p "/base/Areas/Secret.org"))
    (should-not (ps/file-tree--set-hidden-p "/base/Areas/Career.org"))))

(ert-deftest ps/file-tree--set-hidden-include-only-hides-non-matching ()
  "With :include set, a non-matching leaf path is hidden."
  (let ((ps/file-tree-file-sets
         '(("Work" . (:include ("/Work/") :exclude nil))))
        (ps/file-tree-current-set "Work"))
    (should-not (ps/file-tree--set-hidden-p "/base/Areas/Work/Project.org"))
    (should (ps/file-tree--set-hidden-p "/base/Areas/Personal/Diary.org"))))

(ert-deftest ps/file-tree--set-hidden-include-shows-dir-with-whitelisted-descendant ()
  "A directory with a whitelisted descendant remains visible for navigation."
  (let ((ps/file-tree-file-sets
         '(("Work" . (:include ("Project\\.org") :exclude nil))))
        (ps/file-tree-current-set "Work"))
    (ps/file-tree-test--with-base-dir '("Areas/" "Areas/Work/" "Areas/Work/Project.org" "Areas/Other.org")
      ;; "Areas" itself doesn't match, but contains a descendant that does.
      (should-not (ps/file-tree--set-hidden-p (expand-file-name "Areas" dir)))
      (should-not (ps/file-tree--set-hidden-p (expand-file-name "Areas/Work" dir)))
      (should-not (ps/file-tree--set-hidden-p (expand-file-name "Areas/Work/Project.org" dir)))
      ;; Sibling file with no matching descendant and no match itself: hidden.
      (should (ps/file-tree--set-hidden-p (expand-file-name "Areas/Other.org" dir))))))

(ert-deftest ps/file-tree--set-hidden-combined-include-and-exclude ()
  "Exclude wins even within an included subtree."
  (let ((ps/file-tree-file-sets
         '(("Work" . (:include ("/Work/") :exclude ("Confidential")))))
        (ps/file-tree-current-set "Work"))
    (should-not (ps/file-tree--set-hidden-p "/base/Areas/Work/Plan.org"))
    (should (ps/file-tree--set-hidden-p "/base/Areas/Work/Confidential.org"))))

(ert-deftest ps/file-tree--ignored-p-set-exclude-combines-with-ignored-files ()
  "`ps/file-tree--ignored-p' hides files via either ignore-list or set exclude."
  (let ((ps/file-tree-ignored-files '("\\`init\\.org\\'"))
        (ps/file-tree-file-sets '(("Secret" . (:include nil :exclude ("Diary")))))
        (ps/file-tree-current-set "Secret"))
    (should (ps/file-tree--ignored-p "init.org" "/base/init.org"))
    (should (ps/file-tree--ignored-p "Diary.org" "/base/Areas/Diary.org"))
    (should-not (ps/file-tree--ignored-p "Career.org" "/base/Areas/Career.org"))))

;;; -------------------------------------------------------
;;; ps/file-tree--ensure-valid-set / set switching
;;; -------------------------------------------------------

(ert-deftest ps/file-tree--ensure-valid-set-keeps-valid-current ()
  "A current set that exists in the alist is left unchanged."
  (let ((ps/file-tree-file-sets '(("All" . (:include nil :exclude nil))
                                   ("Work" . (:include nil :exclude nil))))
        (ps/file-tree-current-set "Work"))
    (ps/file-tree--ensure-valid-set)
    (should (equal ps/file-tree-current-set "Work"))))

(ert-deftest ps/file-tree--ensure-valid-set-falls-back-to-first ()
  "An unknown current set falls back to the first entry in the alist."
  (let ((ps/file-tree-file-sets '(("All" . (:include nil :exclude nil))
                                   ("Work" . (:include nil :exclude nil))))
        (ps/file-tree-current-set "DoesNotExist"))
    (ps/file-tree--ensure-valid-set)
    (should (equal ps/file-tree-current-set "All"))))

(ert-deftest ps/file-tree-cycle-file-set-wraps-around ()
  "Cycling moves to the next set and wraps back to the first."
  (let ((ps/file-tree-file-sets '(("All" . (:include nil :exclude nil))
                                   ("Work" . (:include nil :exclude nil))
                                   ("Personal" . (:include nil :exclude nil))))
        (ps/file-tree-current-set "All"))
    (ps/file-tree-cycle-file-set)
    (should (equal ps/file-tree-current-set "Work"))
    (ps/file-tree-cycle-file-set)
    (should (equal ps/file-tree-current-set "Personal"))
    (ps/file-tree-cycle-file-set)
    (should (equal ps/file-tree-current-set "All"))))

;;; -------------------------------------------------------
;;; ps/file-tree--list-subdirs
;;; -------------------------------------------------------

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

;;; -------------------------------------------------------
;;; ps/file-tree-transform-file-name / ps/file-tree-transform-dir-name
;;; -------------------------------------------------------

(ert-deftest ps/file-tree-transform-file-name-strips-org-extension ()
  "A trailing .org extension is stripped from the displayed name."
  (let ((ps/file-tree-name-spacing (default-value 'ps/file-tree-name-spacing)))
    (should (equal (ps/file-tree-transform-file-name "Career.org") " Career"))))

(ert-deftest ps/file-tree-transform-file-name-strips-org-case-insensitively ()
  "A trailing .ORG / .Org extension is also stripped."
  (let ((ps/file-tree-name-spacing (default-value 'ps/file-tree-name-spacing)))
    (should (equal (ps/file-tree-transform-file-name "Career.ORG") " Career"))
    (should (equal (ps/file-tree-transform-file-name "Career.Org") " Career"))))

(ert-deftest ps/file-tree-transform-file-name-leaves-non-org-files-alone ()
  "Files not ending in .org keep their extension, just gain leading spacing."
  (let ((ps/file-tree-name-spacing (default-value 'ps/file-tree-name-spacing)))
    (should (equal (ps/file-tree-transform-file-name "notes.txt") " notes.txt"))
    (should (equal (ps/file-tree-transform-file-name "Career.org.bak") " Career.org.bak"))))

(ert-deftest ps/file-tree-transform-file-name-bare-dot-org-unchanged ()
  "A file literally named \".org\" is left unchanged (edge case)."
  (let ((ps/file-tree-name-spacing (default-value 'ps/file-tree-name-spacing)))
    (should (equal (ps/file-tree-transform-file-name ".org") " .org"))))

(ert-deftest ps/file-tree-transform-file-name-respects-spacing-customization ()
  "Custom `ps/file-tree-name-spacing' controls the gap width via display property."
  (let ((ps/file-tree-name-spacing 0.75))
    (should (equal (get-text-property 0 'display (ps/file-tree-transform-file-name "Career.org"))
                   '(space :width 0.75)))))

(ert-deftest ps/file-tree-transform-dir-name-adds-spacing-only ()
  "Directory names gain leading spacing without any extension stripping."
  (let ((ps/file-tree-name-spacing (default-value 'ps/file-tree-name-spacing)))
    (should (equal (ps/file-tree-transform-dir-name "Areas") " Areas"))
    (should (equal (ps/file-tree-transform-dir-name "Career.org") " Career.org"))))

;;; -------------------------------------------------------
;;; ps/file-tree--expandable-state-p
;;; -------------------------------------------------------

(ert-deftest ps/file-tree--expandable-state-p-accepts-open-close-states ()
  "Returns t for all open/closed node states that support expand/collapse."
  (dolist (state '(root-node-open  root-node-closed
                    dir-node-open   dir-node-closed
                    file-node-open  file-node-closed
                    tag-node-open   tag-node-closed))
    (should (ps/file-tree--expandable-state-p state))))

(ert-deftest ps/file-tree--expandable-state-p-rejects-leaf-and-nil ()
  "Returns nil for leaf nodes and nil state."
  (should-not (ps/file-tree--expandable-state-p 'tag-node))
  (should-not (ps/file-tree--expandable-state-p nil))
  (should-not (ps/file-tree--expandable-state-p 'unknown-state)))

;;; -------------------------------------------------------
;;; ps/file-tree--target-window
;;; -------------------------------------------------------

(ert-deftest ps/file-tree--target-window-picks-most-recently-used ()
  "Among non-tree windows, the most-recently-selected one is returned."
  (let ((tree-buf (generate-new-buffer "tree"))
        (buf-a (generate-new-buffer "a"))
        (buf-b (generate-new-buffer "b")))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (set-window-buffer (selected-window) tree-buf)
          (let* ((tree-win (selected-window))
                 (win-a (split-window tree-win))
                 (win-b (split-window win-a)))
            (set-window-buffer win-a buf-a)
            (set-window-buffer win-b buf-b)
            (select-window win-a)
            (select-window win-b)
            (cl-letf (((symbol-function 'treemacs-get-local-window)
                       (lambda () tree-win)))
              (should (eq (ps/file-tree--target-window) win-b))
              (select-window win-a)
              (should (eq (ps/file-tree--target-window) win-a)))))
      (mapc #'kill-buffer (list tree-buf buf-a buf-b)))))

(ert-deftest ps/file-tree--target-window-nil-when-only-tree-window ()
  "Returns nil when the file tree is the only window."
  (let ((tree-buf (generate-new-buffer "tree")))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (set-window-buffer (selected-window) tree-buf)
          (let ((tree-win (selected-window)))
            (cl-letf (((symbol-function 'treemacs-get-local-window)
                       (lambda () tree-win)))
              (should-not (ps/file-tree--target-window)))))
      (kill-buffer tree-buf))))

;;; test-ps-file-tree.el ends here
