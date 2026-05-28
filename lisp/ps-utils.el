;;; ps-utils.el --- Org utility functions -*- lexical-binding: t; -*-

(require 'subr-x)
(require 'xml)

(defcustom ps/org-link-title-max-length 200
  "Maximum title length before truncation with ellipsis."
  :type 'integer)

(defun ps/org--shorten (s)
  "Truncate string S to `ps/org-link-title-max-length' chars, appending \"...\"."
  (if (and s (> (length s) ps/org-link-title-max-length))
      (concat (substring s 0 ps/org-link-title-max-length) "...")
    s))

(defun ps/org--clean-title (s)
  "Trim whitespace from string S; return nil if S is nil or empty."
  (when (and s (not (string-empty-p s)))
    (string-trim s)))

(provide 'ps-utils)
;;; ps-utils.el ends here
