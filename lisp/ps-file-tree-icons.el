;;; ps-file-tree-icons.el --- Category icons for the file tree -*- lexical-binding: t; -*-

;; Provided by treemacs; declared here so this file loads (and its pure
;; functions are testable) without treemacs installed.
(declare-function treemacs-create-theme "treemacs-themes")
(declare-function treemacs-load-theme "treemacs-themes")
(declare-function treemacs-create-icon "treemacs-icons")

(defun ps/file-tree-icons--build-alist (icon-dir)
  "Build a list of (CATEGORY . FILE) for SVGs in ICON-DIR.
Mirrors `ps/agenda-icons--build-alist'. Returns nil when ICON-DIR is missing
or has no SVGs."
  (let ((icons '()))
    (when (file-directory-p icon-dir)
      (dolist (file (directory-files icon-dir t "\\.svg$"))
        (push (cons (file-name-base file) file) icons)))
    icons))

(defun ps/file-tree-icons--merge (icon-dirs)
  "Merge ICON-DIRS (lowest to highest priority) into one alist, last wins."
  (let (merged)
    (dolist (dir icon-dirs)
      (dolist (entry (ps/file-tree-icons--build-alist dir))
        (setq merged (cons entry (assoc-delete-all (car entry) merged)))))
    (nreverse merged)))

(defun ps/file-tree-icons--register (category file)
  "Register FILE as the file-tree icon for CATEGORY.org files."
  (eval
   `(treemacs-create-icon
     :icons-dir ,(file-name-directory file)
     :file ,(file-name-nondirectory file)
     :extensions (,(concat category ".org"))
     :fallback "")
   t))

(defun ps/file-tree-icons-apply (icon-dirs)
  "Register a treemacs theme mapping <Category>.org files to category SVGs.
ICON-DIRS is a list of directories ordered from lowest to highest priority,
as in `ps/agenda-icons-apply'. Does nothing if treemacs isn't available or no
icons are found in any directory."
  (when (require 'treemacs nil t)
    (let ((merged (ps/file-tree-icons--merge icon-dirs)))
      (when merged
        (treemacs-create-theme "ps-file-tree"
          :extends "Default"
          :config
          (dolist (entry merged)
            (ps/file-tree-icons--register (car entry) (cdr entry))))
        (treemacs-load-theme "ps-file-tree")))))

(provide 'ps-file-tree-icons)
;;; ps-file-tree-icons.el ends here
