;;; ps-heading-stars.el --- Keep half-typed heading stars literal -*- lexical-binding: t; -*-

;;; Commentary:

;; With `org-hide-emphasis-markers' enabled (needed for the org-appear live
;; preview), leading heading stars render badly *while you are typing them*.
;;
;; Org's emphasis fontifier (`org-do-emphasis-faces') already refuses to treat
;; headline stars as bold -- but only for a *complete* headline, because its
;; guard checks `org-outline-regexp-bol' (\"^\\*+ \", which requires a trailing
;; space).  A half-typed heading (\"**\", \"***\" with no space yet) is not a
;; valid headline, so the guard does not fire and the stars are parsed as bold:
;;   - a long pure star run (\"**********\") shows its middle stars bold;
;;   - a bare \"**\" pairs across the newline with the following headline,
;;     putting the bold face + `invisible' on that headline's stars and so
;;     hiding the bullet org-superstar composes onto them.
;;
;; This module extends Org's own guard to also treat a line that is *nothing
;; but* leading stars as headline stars, by binding `org-outline-regexp-bol'
;; around `org-do-emphasis-faces' so it additionally matches \"^\\*+$\".  Then
;; emphasis never touches a pure star run nor spans from a \"**\" line into the
;; next headline; the literal stars stay plain and the bullet appears (via
;; org-superstar/org-modern) once the heading is completed with a space.  A
;; line that merely begins with real inline bold (\"*word*\") is unaffected.

;;; Code:

(require 'org)

(defun ps/heading-stars--protect-emphasis (orig limit)
  "Around advice for `org-do-emphasis-faces' (ORIG, LIMIT passed through).
Treat a line that is nothing but leading stars (a heading being typed, before
its trailing space) as headline stars, so a pure star run is never fontified as
bold and a stray \"**\" cannot form a cross-line bold that hides the next
headline's bullet."
  (let ((org-outline-regexp-bol
         (concat "\\(?:" org-outline-regexp-bol "\\)\\|^\\*+$")))
    (funcall orig limit)))

(defun ps/heading-stars-enable ()
  "Install the leading-star emphasis guard.
The advice is global and `advice-add' is idempotent, so re-running this (e.g.
from `org-mode-hook') is a no-op."
  (advice-add 'org-do-emphasis-faces :around #'ps/heading-stars--protect-emphasis))

(defun ps/heading-stars-disable ()
  "Remove the leading-star emphasis guard installed by `ps/heading-stars-enable'."
  (advice-remove 'org-do-emphasis-faces #'ps/heading-stars--protect-emphasis))

(provide 'ps-heading-stars)
;;; ps-heading-stars.el ends here
