;;; ps-agenda-icons.el --- Category icons for the org agenda -*- lexical-binding: t; -*-

;; Set by `ps/agenda-icons-apply'; declared by org-agenda.
(defvar org-agenda-category-icon-alist)

(defun ps/agenda-icons--build-alist (icon-dir)
  "Build an `org-agenda-category-icon-alist' from SVG files in ICON-DIR.
Each .svg yields (CATEGORY FILE nil nil :ascent center) where CATEGORY is the
file's base name.  Returns nil when ICON-DIR is missing or has no SVGs."
  (let ((icons '()))
    (when (file-directory-p icon-dir)
      (dolist (file (directory-files icon-dir t "\\.svg$"))
        (let ((category (file-name-base file)))
          (push `(,category ,file nil nil :ascent center) icons))))
    icons))

(defun ps/agenda-icons-apply (icon-dirs)
  "Populate `org-agenda-category-icon-alist' from SVG files in ICON-DIRS.
ICON-DIRS is a list of directories ordered from lowest to highest priority:
if the same category appears in more than one directory, the entry from the
directory listed last wins. Missing directories are skipped. Does nothing if
no icons are found in any directory."
  (let (merged)
    (dolist (dir icon-dirs)
      (dolist (entry (ps/agenda-icons--build-alist dir))
        (setq merged (cons entry (assoc-delete-all (car entry) merged)))))
    (when merged
      (customize-set-value 'org-agenda-category-icon-alist (nreverse merged)))))

(provide 'ps-agenda-icons)
;;; ps-agenda-icons.el ends here
