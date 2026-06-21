;;; ps-heading-stars.el --- Keep half-typed heading stars visible -*- lexical-binding: t; -*-

;;; Commentary:

;; With `org-hide-emphasis-markers' enabled (needed for the org-appear live
;; preview), leading heading stars render badly *while you are typing them*.
;;
;; Org's emphasis fontifier (`org-do-emphasis-faces') already refuses to treat
;; headline stars as bold -- but only for a *complete* headline, because its
;; guard checks `org-outline-regexp-bol' (\"^\\*+ \", which requires a trailing
;; space).  A half-typed heading (\"**\", \"***\" with no space yet) is not a
;; valid headline, so the guard does not fire and the stars are parsed as bold,
;; whose markers then get an `invisible' text property:
;;   - \"***\"  -> the outer pair hides, only 1 star shows;
;;   - \"****\" -> 2 stars show;
;;   - a stray \"**\" on its own line pairs across the newline with a
;;     neighbouring headline's star, hiding it and breaking its bullet.
;;
;; This module appends a font-lock matcher that runs after Org's emphasis
;; keyword and strips that `invisible' property from any leading star run on a
;; heading or would-be-heading line, so the literal stars always show.  The
;; bullet still appears via org-superstar/org-modern once the headline is
;; complete (i.e. once the space is typed).

;;; Code:

(defun ps/heading-stars--show-literal (limit)
  "Reveal leading heading stars hidden by Org's emphasis fontification.
For every leading star run up to LIMIT that is followed by a space/tab (a real
headline) or end of line (a half-typed one), remove the `invisible' property
\(value t, which is what emphasis sets) so the literal stars stay visible.

A body line that merely begins with inline bold (\"*word*\", a star followed
immediately by a non-space) is deliberately not matched, so its genuinely
hidden opening marker stays hidden.  Returns nil; used as a font-lock matcher."
  (while (re-search-forward "^\\*+\\(?:[ \t]\\|$\\)" limit t)
    (let* ((beg (match-beginning 0))
           (end (save-excursion
                  (goto-char beg)
                  (skip-chars-forward "*")
                  (point))))
      (when (text-property-any beg end 'invisible t)
        (remove-text-properties beg end '(invisible nil)))))
  nil)

(defun ps/heading-stars-enable ()
  "Register the leading-star reveal in the current buffer.
The keyword is appended (APPEND=t) so it runs after Org's own emphasis
fontification, undoing the `invisible' it puts on mis-parsed heading stars."
  (font-lock-add-keywords
   nil
   '((ps/heading-stars--show-literal))
   t))

(provide 'ps-heading-stars)
;;; ps-heading-stars.el ends here
