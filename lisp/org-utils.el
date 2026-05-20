(require 'subr-x)
(require 'xml)

(defcustom my/org-link-title-max-length 200
  "Max title length before truncation."
  :type 'integer)

(defun my/org--shorten (s)
  (if (and s (> (length s) my/org-link-title-max-length))
      (concat
       (substring s 0 my/org-link-title-max-length)
       "...")
    s))

(defun my/org--clean-title (s)
  (when (and s (not (string-empty-p s)))
    (string-trim s)))

(provide 'org-utils)
