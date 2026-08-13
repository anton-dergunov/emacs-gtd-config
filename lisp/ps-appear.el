;;; ps-appear.el --- Keep org-appear's reveal alive while typing -*- lexical-binding: t; -*-

;;; Commentary:

;; `org-appear' shows the raw Org syntax of the element under the cursor
;; (`*bold*', `=verbatim=', `[[link][desc]]', `\alpha', `x_1') and hides it
;; again on the way out.  Without this module the reveal collapses again on
;; every keystroke typed *inside* the element -- the raw markers flash away
;; and you end up editing text whose syntax you cannot see.
;;
;; Why it collapses.  `jinx' (the typo checker) registers a jit-lock function,
;; so `jit-lock-functions' holds both `jinx--mark-pending' and
;; `font-lock-fontify-region'.  `jit-lock--run-functions' marks as `fontified'
;; the *intersection* of the bounds its functions report: font-lock reports the
;; region it actually fontified, which `font-lock-default-fontify-region'
;; extends out to whole lines, while `jinx--mark-pending' returns nil and so
;; counts as "exactly the region I was asked for".  The intersection is
;; therefore the requested region, never the extended one.
;;
;; `org-appear--show-with-lock' calls `font-lock-ensure' over just the
;; element's bounds before removing the invisibility, so with that narrowed
;; intersection the rest of the edited line stays unfontified.  The redisplay
;; right after the keystroke fontifies that leftover, extends back to whole
;; lines, and `org-do-emphasis-faces' re-hides the markers -- after
;; org-appear's `post-command-hook' has already revealed them.  Nothing runs
;; again until the next command, so the collapsed form is what stays on screen.
;;
;; The fix is to re-assert the reveal at the end of every font-lock region
;; pass, still inside that same pass, so the collapsed form is never displayed.
;; Hooking the region pass rather than `org-do-emphasis-faces' is deliberate:
;; one hook then covers every element type org-appear toggles -- emphasis,
;; verbatim/code, links, entities, sub/superscripts and hidden keywords -- and
;; it fires once per region instead of once per match.
;;
;; Leaving an element is safe: `org-appear--post-cmd' clears
;; `org-appear--elem-toggled' *before* it hides, so the guards below already
;; refuse to fight the hide.
;;
;; Wired from config.org's "Show formatting markup only at point" block.

;;; Code:

;; org-appear internals, referenced defensively below.  Declared so this file
;; loads and byte-compiles when org-appear isn't installed.
(defvar org-appear-mode)
(defvar org-appear--prev-elem)
(defvar org-appear--elem-toggled)

(defun ps/appear--reassert-reveal (&rest _)
  "Re-reveal the markup org-appear is showing for the cursor.
Added as `:after' advice to `font-lock-default-fontify-region', so it cancels
the re-hiding that Org's own fontification applies whenever a region is
refontified while point sits inside the element.

Uses org-appear's tracked element rather than the element at `point': during
fontification `point' is font-lock's scan position, not the user's cursor."
  (when (and (bound-and-true-p org-appear-mode)
             (bound-and-true-p org-appear--elem-toggled)
             (bound-and-true-p org-appear--prev-elem))
    (ignore-errors (org-appear--show-invisible org-appear--prev-elem))))

(defun ps/appear-setup ()
  "Keep org-appear's reveal alive across refontification.
Idempotent; `advice-add' installs a given function only once."
  (advice-add 'font-lock-default-fontify-region :after
              #'ps/appear--reassert-reveal))

(provide 'ps-appear)
;;; ps-appear.el ends here
