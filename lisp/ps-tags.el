;;; ps-tags.el --- Where headline tags sit in Org files -*- lexical-binding: t; -*-

;;; Commentary:

;; Org's default is to pad every tagged headline out to column 77 so the tags
;; form a right-hand column.  In a window narrower than that the padding runs
;; past the right edge, `visual-line-mode' wraps the line, and the org-modern
;; tag pill is split by the wrap: a stub at the right edge and the tag name
;; alone on the continuation line.
;;
;; `ps/tags-alignment' picks between that classic column (`right') and placing
;; the tags one space after the headline text (`inline', the default), which
;; cannot overflow because it adds no padding at all.
;;
;; Note that Org aligns with *real whitespace* -- `org--align-tags-here' does
;; `delete-region' + `indent-to', so the padding is in the file, not a display
;; property.  That is why the column is a fixed setting rather than something
;; recomputed from the window width: refitting it on every resize would rewrite
;; every tagged headline in the buffer each time.

;;; Code:

;; Provided by Org; declared here so this file loads (and its pure function is
;; testable) without Org.
(defvar org-tags-column)
(defvar org-auto-align-tags)

(defgroup ps-tags nil
  "Placement of Org headline tags."
  :group 'org)

(defcustom ps/tags-alignment 'inline
  "Where tags sit on an Org headline.

`inline'  one space after the headline text (no padding, so tags can never
          be pushed off the window edge).
`right'   right-aligned to `ps/tags-column', Org's classic layout -- readable
          only in a window at least that wide."
  :type '(choice (const :tag "One space after the headline text" inline)
                 (const :tag "Right-aligned column" right))
  :group 'ps-tags)

(defcustom ps/tags-column -77
  "Column tags are aligned to when `ps/tags-alignment' is `right'.
Negative means right-aligned so the tags *end* at that column, matching
`org-tags-column'.  Ignored under `inline'."
  :type 'integer
  :group 'ps-tags)

(defun ps/tags--settings (alignment column)
  "Return (TAGS-COLUMN . AUTO-ALIGN) for ALIGNMENT and COLUMN.
The car is a value for `org-tags-column' and the cdr one for
`org-auto-align-tags'.  Any ALIGNMENT other than `right' is treated as
`inline', so an unrecognised setting degrades to the layout that cannot
overflow."
  (if (eq alignment 'right)
      (cons column t)
    ;; Column 0 puts the tags directly after the headline text; auto-align is
    ;; then pure churn -- it would re-indent to that same column on every edit.
    (cons 0 nil)))

(defun ps/tags-apply ()
  "Apply `ps/tags-alignment' to the current buffer."
  (let ((settings (ps/tags--settings ps/tags-alignment ps/tags-column)))
    (setq-local org-tags-column (car settings))
    (setq-local org-auto-align-tags (cdr settings))))

;;;###autoload
(defun ps/tags-setup ()
  "Apply the tag alignment to every Org buffer."
  (add-hook 'org-mode-hook #'ps/tags-apply))

(provide 'ps-tags)
;;; ps-tags.el ends here
