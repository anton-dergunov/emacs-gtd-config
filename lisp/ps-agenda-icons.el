;;; ps-agenda-icons.el --- Category icons for the org agenda -*- lexical-binding: t; -*-

(require 'ps-material-icons)

;; Set by `ps/agenda-icons-apply'; declared by org-agenda.
(defvar org-agenda-category-icon-alist)

(defun ps/agenda-icons--build-alist ()
  "Build an `org-agenda-category-icon-alist' from `ps/material-icons-category-map'.
Each `(CATEGORY . NAME)' yields a (CATEGORY SVG svg t :ascent center :height H)
entry rendering the Material Symbols glyph NAME as an in-memory SVG image.
Entries whose icon name does not resolve are skipped. Returns nil for an empty
map."
  (let ((height (ps/material-icons--pixel-height))
        (icons '()))
    (dolist (entry ps/material-icons-category-map)
      (when-let ((svg (ps/material-icons-svg (cdr entry))))
        (push `(,(car entry) ,svg svg t :ascent center :height ,height) icons)))
    (nreverse icons)))

(defun ps/agenda-icons-apply ()
  "Populate `org-agenda-category-icon-alist' from `ps/material-icons-category-map'.
Does nothing when the Material Symbols font is unavailable or no category icons
resolve, leaving any existing value untouched."
  (when (ps/material-icons-available-p)
    (let ((alist (ps/agenda-icons--build-alist)))
      (when alist
        (customize-set-value 'org-agenda-category-icon-alist alist)))))

(provide 'ps-agenda-icons)
;;; ps-agenda-icons.el ends here
