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

(ert-deftest ps/file-tree--ignored-default-hides-config-files ()
  "The config files that sit beside the notes are hidden from the tree."
  (let ((ps/file-tree-ignored-files (default-value 'ps/file-tree-ignored-files)))
    (should (ps/file-tree--ignored-p "workspace.org" "/base/workspace.org"))
    (should (ps/file-tree--ignored-p "AGENTS.md" "/base/AGENTS.md"))
    (should (ps/file-tree--ignored-p "CLAUDE.md" "/base/CLAUDE.md"))
    ;; Only those exact names — similar files are still shown.
    (should-not (ps/file-tree--ignored-p "Agents.org" "/base/Work/Agents.org"))
    (should-not (ps/file-tree--ignored-p "Notes.md" "/base/Notes.md"))))

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
    (should-not (ps/file-tree--set-hidden-p "/base/Admin/Secret.org"))))

(ert-deftest ps/file-tree--set-hidden-exclude-only ()
  "A path matching :exclude is hidden; non-matching paths are not."
  (let ((ps/file-tree-file-sets
         '(("NoSecrets" . (:include nil :exclude ("Secret")))))
        (ps/file-tree-current-set "NoSecrets"))
    (should (ps/file-tree--set-hidden-p "/base/Admin/Secret.org"))
    (should-not (ps/file-tree--set-hidden-p "/base/Work/Career.org"))))

(ert-deftest ps/file-tree--set-hidden-include-only-hides-non-matching ()
  "With :include set, a non-matching leaf path is hidden."
  (let ((ps/file-tree-file-sets
         '(("Work" . (:include ("/Work/") :exclude nil))))
        (ps/file-tree-current-set "Work"))
    (should-not (ps/file-tree--set-hidden-p "/base/Work/Projects/Plan.org"))
    (should (ps/file-tree--set-hidden-p "/base/Mind/Personal/Diary.org"))))

(ert-deftest ps/file-tree--set-hidden-include-shows-dir-with-whitelisted-descendant ()
  "A directory with a whitelisted descendant remains visible for navigation."
  (let ((ps/file-tree-file-sets
         '(("Work" . (:include ("Plan\\.org") :exclude nil))))
        (ps/file-tree-current-set "Work"))
    (ps/file-tree-test--with-base-dir '("Work/" "Work/Projects/" "Work/Projects/Plan.org" "Work/Other.org")
      ;; "Work" itself doesn't match, but contains a descendant that does.
      (should-not (ps/file-tree--set-hidden-p (expand-file-name "Work" dir)))
      (should-not (ps/file-tree--set-hidden-p (expand-file-name "Work/Projects" dir)))
      (should-not (ps/file-tree--set-hidden-p (expand-file-name "Work/Projects/Plan.org" dir)))
      ;; Sibling file with no matching descendant and no match itself: hidden.
      (should (ps/file-tree--set-hidden-p (expand-file-name "Work/Other.org" dir))))))

(ert-deftest ps/file-tree--set-hidden-combined-include-and-exclude ()
  "Exclude wins even within an included subtree."
  (let ((ps/file-tree-file-sets
         '(("Work" . (:include ("/Work/") :exclude ("Confidential")))))
        (ps/file-tree-current-set "Work"))
    (should-not (ps/file-tree--set-hidden-p "/base/Work/Projects/Plan.org"))
    (should (ps/file-tree--set-hidden-p "/base/Work/Projects/Confidential.org"))))

(ert-deftest ps/file-tree--ignored-p-set-exclude-combines-with-ignored-files ()
  "`ps/file-tree--ignored-p' hides files via either ignore-list or set exclude."
  (let ((ps/file-tree-ignored-files '("\\`init\\.org\\'"))
        (ps/file-tree-file-sets '(("Secret" . (:include nil :exclude ("Diary")))))
        (ps/file-tree-current-set "Secret"))
    (should (ps/file-tree--ignored-p "init.org" "/base/init.org"))
    (should (ps/file-tree--ignored-p "Diary.org" "/base/Mind/Diary.org"))
    (should-not (ps/file-tree--ignored-p "Career.org" "/base/Work/Career.org"))))

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
;;; ps/file-tree-filter-files / agenda filter toggle
;;; -------------------------------------------------------

(ert-deftest ps/file-tree-filter-files-noop-when-disabled ()
  "When the toggle is off, FILES is returned unchanged regardless of set."
  (let ((ps/file-tree-file-sets '(("All" . (:include nil :exclude nil))
                                   ("Work" . (:include ("/Work/Prep\\.org\\'")
                                              :exclude nil))))
        (ps/file-tree-current-set "Work")
        (ps/file-tree-set-applies-to-agenda nil)
        (files '("/base/Work/Prep.org" "/base/Work/Career.org")))
    (should (equal (ps/file-tree-filter-files files) files))))

(ert-deftest ps/file-tree-filter-files-removes-hidden-when-enabled ()
  "When the toggle is on, files hidden by the current set are removed."
  (let ((ps/file-tree-file-sets '(("All" . (:include nil :exclude nil))
                                   ("Work" . (:include ("/Work/Prep\\.org\\'")
                                              :exclude nil))))
        (ps/file-tree-current-set "Work")
        (ps/file-tree-set-applies-to-agenda t)
        (files '("/base/Work/Prep.org" "/base/Work/Career.org")))
    (should (equal (ps/file-tree-filter-files files) '("/base/Work/Prep.org")))))

(ert-deftest ps/file-tree-toggle-agenda-filter-flips-variable ()
  "Toggling twice returns to the original value."
  (let ((ps/file-tree-set-applies-to-agenda nil))
    (ps/file-tree-toggle-agenda-filter)
    (should (eq ps/file-tree-set-applies-to-agenda t))
    (ps/file-tree-toggle-agenda-filter)
    (should-not ps/file-tree-set-applies-to-agenda)))

(ert-deftest ps/file-tree--modeline-shows-agenda-marker-when-enabled ()
  "The mode-line indicator gains a marker when the toggle is on."
  (let ((ps/file-tree-file-sets '(("All" . (:include nil :exclude nil))))
        (ps/file-tree-current-set "All"))
    (let ((ps/file-tree-set-applies-to-agenda nil))
      (should-not (string-match-p "📅" (ps/file-tree--modeline))))
    (let ((ps/file-tree-set-applies-to-agenda t))
      (should (string-match-p "📅" (ps/file-tree--modeline))))))

;;; -------------------------------------------------------
;;; ps/file-tree--list-subdirs
;;; -------------------------------------------------------

(ert-deftest ps/file-tree--list-subdirs-returns-only-visible-dirs ()
  "Only non-ignored subdirectories are returned, files are excluded."
  (let ((ps/file-tree-ignored-files (default-value 'ps/file-tree-ignored-files)))
    (ps/file-tree-test--with-base-dir '("ML/" "Current/" ".git/" "notes.org")
      (let ((names (mapcar #'car (ps/file-tree--list-subdirs dir))))
        (should (equal names '("Current" "ML")))))))

(ert-deftest ps/file-tree--list-subdirs-sorted-by-name ()
  "Entries are sorted alphabetically by name."
  (let ((ps/file-tree-ignored-files (default-value 'ps/file-tree-ignored-files)))
    (ps/file-tree-test--with-base-dir '("Vision/" "ML/" "Current/")
      (let ((names (mapcar #'car (ps/file-tree--list-subdirs dir))))
        (should (equal names '("Current" "ML" "Vision")))))))

(ert-deftest ps/file-tree--list-subdirs-entries-are-abs-paths ()
  "Each entry maps NAME to an absolute path of the subdirectory."
  (let ((ps/file-tree-ignored-files (default-value 'ps/file-tree-ignored-files)))
    (ps/file-tree-test--with-base-dir '("ML/")
      (let ((entry (car (ps/file-tree--list-subdirs dir))))
        (should (equal (car entry) "ML"))
        (should (file-name-absolute-p (cdr entry)))
        (should (string-suffix-p "ML" (cdr entry)))))))

(ert-deftest ps/file-tree--list-subdirs-missing-dir ()
  "A non-existent base directory yields nil."
  (let ((ps/file-tree-ignored-files (default-value 'ps/file-tree-ignored-files)))
    (should (null (ps/file-tree--list-subdirs "/no/such/dir/at/all")))))

;;; -------------------------------------------------------
;;; ps/file-tree--root-hidden-p
;;; -------------------------------------------------------

(ert-deftest ps/file-tree--root-hidden-by-default ()
  "With the default single-root layout, the root line is hidden."
  (let ((ps/file-tree-hide-root t)
        (ps/file-tree-root-mode 'single))
    (should (ps/file-tree--root-hidden-p))))

(ert-deftest ps/file-tree--root-not-hidden-in-subdirs-mode ()
  "In `subdirs' mode the roots are the section headers, so they stay visible.
Hiding them would leave the tree with nothing to group by, and collapse-all
with no way back."
  (let ((ps/file-tree-hide-root t)
        (ps/file-tree-root-mode 'subdirs))
    (should-not (ps/file-tree--root-hidden-p))))

(ert-deftest ps/file-tree--root-not-hidden-when-disabled ()
  "Turning `ps/file-tree-hide-root' off restores the root line."
  (let ((ps/file-tree-hide-root nil)
        (ps/file-tree-root-mode 'single))
    (should-not (ps/file-tree--root-hidden-p))))

;;; -------------------------------------------------------
;;; ps/file-tree--desired-projects
;;; -------------------------------------------------------

(ert-deftest ps/file-tree--desired-projects-single-is-the-base-dir ()
  "In `single' mode the whole Org directory is one project, named after it."
  (let ((ps/file-tree-root-mode 'single)
        (ps/file-tree-root-name nil))
    (ps/file-tree-test--with-base-dir '("ML/" "Work/" "Inbox.org")
      (let ((projects (ps/file-tree--desired-projects dir)))
        (should (= (length projects) 1))
        (should (equal (car (car projects))
                       (file-name-nondirectory (directory-file-name dir))))
        (should (equal (cdr (car projects)) (directory-file-name dir)))))))

(ert-deftest ps/file-tree--desired-projects-single-honors-root-name ()
  "`ps/file-tree-root-name' overrides the label of the single root."
  (let ((ps/file-tree-root-mode 'single)
        (ps/file-tree-root-name "Notes"))
    (ps/file-tree-test--with-base-dir '("ML/")
      (should (equal (car (car (ps/file-tree--desired-projects dir))) "Notes")))))

(ert-deftest ps/file-tree--desired-projects-subdirs-is-one-per-subdir ()
  "In `subdirs' mode each immediate subdirectory becomes its own project."
  (let ((ps/file-tree-root-mode 'subdirs)
        (ps/file-tree-root-name "Notes")
        (ps/file-tree-ignored-files (default-value 'ps/file-tree-ignored-files)))
    (ps/file-tree-test--with-base-dir '("ML/" "Work/" ".git/" "Inbox.org")
      (should (equal (mapcar #'car (ps/file-tree--desired-projects dir))
                     '("ML" "Work"))))))

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
    (should (equal (ps/file-tree-transform-dir-name "Work") " Work"))
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
  "Among real editor windows, the most-recently-selected one is returned;
the dedicated file-tree (side) window is excluded."
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
            ;; Mark the tree window dedicated, like the real treemacs side
            ;; window; it must never be chosen.
            (set-window-dedicated-p tree-win t)
            (set-window-buffer win-a buf-a)
            (set-window-buffer win-b buf-b)
            (select-window win-a)
            (select-window win-b)
            (should (eq (ps/file-tree--target-window) win-b))
            (select-window win-a)
            (should (eq (ps/file-tree--target-window) win-a))))
      (mapc #'kill-buffer (list tree-buf buf-a buf-b)))))

(ert-deftest ps/file-tree--target-window-nil-when-only-tree-window ()
  "Returns nil when the only window is the dedicated file tree."
  (let ((tree-buf (generate-new-buffer "tree")))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (set-window-buffer (selected-window) tree-buf)
          (set-window-dedicated-p (selected-window) t)
          (should-not (ps/file-tree--target-window)))
      (kill-buffer tree-buf))))

;;; ps/file-tree-visit-file
;;; -------------------------------------------------------

(ert-deftest ps/file-tree-visit-file-reuses-editor-window-not-tree ()
  "Visiting a not-yet-open file from the dedicated tree window reuses the
editor window instead of splitting a new one (the get-buffer-window-of-nil bug)."
  (let* ((tmp (make-temp-file "ps-visit" nil ".txt"))
         (tree-buf (generate-new-buffer "tree"))
         (ed-buf (generate-new-buffer "editor")))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (set-window-buffer (selected-window) tree-buf)
          (let* ((tree-win (selected-window))
                 (ed-win (split-window tree-win)))
            (set-window-buffer ed-win ed-buf)
            ;; Emulate the treemacs side window and a click happening in it.
            (set-window-dedicated-p tree-win t)
            (select-window tree-win)
            (let ((before (length (window-list))))
              (ps/file-tree-visit-file tmp)
              (should (= (length (window-list)) before))
              (should (eq (selected-window) ed-win))
              (should (eq (window-buffer ed-win) (get-file-buffer tmp))))))
      (when (get-file-buffer tmp) (kill-buffer (get-file-buffer tmp)))
      (kill-buffer tree-buf)
      (kill-buffer ed-buf)
      (delete-file tmp))))

(ert-deftest ps/file-tree-visit-file-reuses-window-already-showing-file ()
  "If FILE is already shown in a window, that window is reused."
  (let* ((tmp (make-temp-file "ps-visit" nil ".txt"))
         (buf-a (generate-new-buffer "a")))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (set-window-buffer (selected-window) buf-a)
          (let* ((w1 (selected-window))
                 (w2 (split-window w1)))
            (set-window-buffer w2 (find-file-noselect tmp))
            (select-window w1)
            (ps/file-tree-visit-file tmp)
            (should (eq (selected-window) w2))))
      (when (get-file-buffer tmp) (kill-buffer (get-file-buffer tmp)))
      (kill-buffer buf-a)
      (delete-file tmp))))

;;; test-ps-file-tree.el ends here
