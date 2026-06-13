;;; ps-org-src-comment.el --- Comment/uncomment toggling inside org src blocks -*- lexical-binding: t; -*-

;;; Commentary:
;; `C-x C-;' normally toggles org's own "# " comment prefix, which doesn't
;; recognize a language's native line-comment syntax (e.g. Lisp's ";; ") as
;; already commented -- so "uncommenting" example code inside a
;; #+begin_src block would just add another prefix on top instead of
;; removing one.  Inside a src block, `ps/org-src-comment-dwim' toggles the
;; block's own comment prefix line-by-line over the region (or current
;; line), keeping the comment markers aligned in a single column the way
;; `comment-region' does.
;;
;; The region is always snapped to whole lines: the first line is included
;; in full even if the region starts mid-line, and a trailing line is only
;; included if the region extends into its actual content (not just up to
;; its start) -- matching the leniency of `comment-only-p', which treats a
;; region ending at the start of the next line as "comment only" when
;; everything before it is a comment.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-src)

(defun ps/org-src-comment-string ()
  "Return the line-comment prefix (e.g. \";; \") for the src block at point."
  (let* ((lang (nth 0 (org-babel-get-src-block-info t)))
         (mode (org-src-get-lang-mode lang)))
    (with-temp-buffer
      (funcall mode)
      (concat (make-string (1+ (or comment-add 0)) (string-to-char comment-start))
              (or comment-padding "")))))

(defun ps/org-src-comment-dwim ()
  "Toggle a line comment using the src block's language, over the active
region (or the current line). Outside a src block, fall back to the normal
`comment-line'.

If every non-blank line in range is already commented, remove one comment
prefix from each. Otherwise add the prefix to each non-blank line, aligned
to the minimum indentation across the range -- matching the alignment
`comment-region' produces."
  (interactive)
  (if (not (org-in-src-block-p))
      (comment-line 1)
    (let* ((cstr (ps/org-src-comment-string))
           (cre (regexp-quote (string-trim-right cstr)))
           (beg0 (if (use-region-p) (region-beginning) (point)))
           (end (if (use-region-p) (region-end) (line-end-position)))
           (beg (save-excursion (goto-char beg0) (line-beginning-position)))
           (line-starts nil)
           all-commented min-indent)
      (save-excursion
        (goto-char beg)
        (while (< (point) end)
          (push (point-marker) line-starts)
          (forward-line 1)))
      (setq line-starts (nreverse line-starts))
      (setq all-commented
            (and line-starts
                 (cl-every (lambda (m)
                             (save-excursion
                               (goto-char m)
                               (or (looking-at-p "[ \t]*$")
                                   (looking-at-p (concat "[ \t]*" cre)))))
                           line-starts)))
      (unless all-commented
        (setq min-indent
              (cl-reduce #'min
                         (mapcar (lambda (m)
                                   (save-excursion
                                     (goto-char m)
                                     (if (looking-at-p "[ \t]*$")
                                         most-positive-fixnum
                                       (progn (skip-chars-forward " \t") (current-column)))))
                                 line-starts))))
      (dolist (m line-starts)
        (save-excursion
          (goto-char m)
          (unless (looking-at-p "[ \t]*$")
            (if all-commented
                (when (looking-at (concat "\\([ \t]*\\)" cre " ?"))
                  (replace-match "\\1"))
              (progn (move-to-column min-indent t) (insert cstr))))))
      (deactivate-mark))))

(provide 'ps-org-src-comment)
;;; ps-org-src-comment.el ends here
