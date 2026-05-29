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

(defun ps/agenda-icons-apply (icon-dir)
  "Populate `org-agenda-category-icon-alist' from SVG files in ICON-DIR.
Does nothing when ICON-DIR does not exist, so callers can treat the presence
of this call as the on/off switch for agenda category icons."
  (when (file-directory-p icon-dir)
    (customize-set-value
     'org-agenda-category-icon-alist
     (ps/agenda-icons--build-alist icon-dir))))

(provide 'ps-agenda-icons)
;;; ps-agenda-icons.el ends here
