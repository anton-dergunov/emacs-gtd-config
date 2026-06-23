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
;; Two complementary tweaks, both applied by binding variables around
;; `org-do-emphasis-faces', keep heading stars literal -- the fix prevents the
;; mis-parse at the source rather than repairing it afterwards (a repair pass
;; cannot work reliably: Org matches emphasis with an *unbounded* `looking-at',
;; so while editing one line it reaches past the refontified region into the
;; next line and clobbers it, but any cleanup matcher is bounded by that region
;; and never runs on the next line):
;;
;; 1. Treat a line that is *nothing but* leading stars as headline stars (by
;;    also matching \"^\\*+$\" via `org-outline-regexp-bol').  This stops
;;    emphasis from *opening* on a pure star run (\"**********\", a stray \"**\"
;;    line), so those stars are never fontified as bold.
;;
;; 2. Forbid the emphasis body from spanning a newline into a line that begins
;;    with stars (by binding `org-emph-re'/`org-verbatim-re' to variants whose
;;    across-newline body alternative requires the continuation line to start
;;    with a non-star).  Org's own guard only checks the *opening* marker, so an
;;    unterminated `*' typed on an otherwise-complete heading line (e.g.
;;    \"* Foo *test\") would otherwise open a legitimate mid-line emphasis whose
;;    body spans the newline and whose closing marker lands on the *next*
;;    headline's stars -- putting bold + `invisible' on them and hiding the
;;    bullet org-superstar/org-modern composes over them.  This keeps the closing
;;    marker from ever reaching a headline's stars.
;;
;; The literal stars stay plain and the bullet appears once the heading is
;; completed with a space.  Real inline bold (\"*word*\") and ordinary multi-line
;; emphasis whose continuation line does not start with a star are unaffected.

;;; Code:

(require 'org)

(defvar ps/heading-stars--emph-re nil
  "`org-emph-re' variant forbidding a body newline before leading stars.")
(defvar ps/heading-stars--verbatim-re nil
  "`org-verbatim-re' variant forbidding a body newline before leading stars.")

(defun ps/heading-stars--build-re (markers)
  "Build an emphasis regexp for MARKERS that cannot span a newline into stars.
Mirrors Org's `org-set-emph-re' template, but the across-newline body
alternative requires the continuation line to start with a non-star, so
emphasis can never reach into a headline (or a heading being typed)."
  (pcase-let* ((`(,pre ,post ,border ,body ,nl) org-emphasis-regexp-components)
               (body (if (<= nl 0) body
                       (format "%s*?\\(?:\n[^*\n]%s*?\\)\\{0,%d\\}" body body nl)))
               (template (format (concat "\\([%s]\\|^\\)"
                                         "\\(\\([%%s]\\)\\([^%s]\\|[^%s]%s[^%s]\\)\\3\\)"
                                         "\\([%s]\\|$\\)")
                                 pre border border body border post)))
    (format template markers)))

(defun ps/heading-stars--ensure-res ()
  "Compute and cache the newline-guarded emphasis regexps once."
  (unless ps/heading-stars--emph-re
    (setq ps/heading-stars--emph-re     (ps/heading-stars--build-re "*/_+")
          ps/heading-stars--verbatim-re (ps/heading-stars--build-re "=~"))))

(defun ps/heading-stars--protect-emphasis (orig limit)
  "Around advice for `org-do-emphasis-faces' (ORIG, LIMIT passed through).
Stop emphasis from claiming heading stars in two ways: (1) a pure star line (a
heading being typed) is treated as headline stars so emphasis never opens on it
\(extends `org-outline-regexp-bol' with \"^\\*+$\"); (2) the emphasis body may
not span a newline into a line that begins with stars, so an unterminated `*' on
a heading-content line cannot pair its closing marker onto the next headline's
stars."
  (ps/heading-stars--ensure-res)
  (let ((org-outline-regexp-bol
         (concat "\\(?:" org-outline-regexp-bol "\\)\\|^\\*+$"))
        (org-emph-re ps/heading-stars--emph-re)
        (org-verbatim-re ps/heading-stars--verbatim-re))
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
