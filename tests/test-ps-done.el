;;; test-ps-done.el --- ERT tests for ps-done -*- lexical-binding: t; -*-

(require 'ert)
(require 'org)
(require 'cl-lib)
(add-to-list 'load-path "lisp")
(require 'ps-done)

;; Bound dynamically by org during a TODO state change; declared special here
;; so the test's `let' binding is dynamic.
(defvar org-state)

;;; Test helpers

(defconst ps/done-test--sample-org
  "* TODO Active task
Some body text.

* DONE Finished task
SCHEDULED: <2026-05-04 Mon>
Done body line one.
Done body line two.

* TODO Another active
More text.
"
  "Sample org content with one DONE subtree and two TODO subtrees.")

(defmacro ps/done-test--with-org-buffer (content &rest body)
  "Evaluate BODY in a temp `org-mode' buffer containing CONTENT."
  (declare (indent 1))
  `(with-temp-buffer
     (let ((org-mode-hook nil))      ; avoid user hooks during tests
       (org-mode))
     (insert ,content)
     (goto-char (point-min))
     ,@body))

;;; -------------------------------------------------------
;;; Public API exists
;;; -------------------------------------------------------

(ert-deftest ps/done--api-functions-defined ()
  "All public commands and the setup function are defined."
  (should (fboundp 'ps/done-collapse-subtrees))
  (should (fboundp 'ps/done-collapse))
  (should (fboundp 'ps/done-expand))
  (should (fboundp 'ps/done-fade-subtrees))
  (should (fboundp 'ps/done-setup-hooks)))

(ert-deftest ps/done--commands-are-interactive ()
  "The user-facing commands are interactive."
  (should (commandp 'ps/done-collapse-subtrees))
  (should (commandp 'ps/done-collapse))
  (should (commandp 'ps/done-expand))
  (should (commandp 'ps/done-fade-subtrees)))

(ert-deftest ps/done--fade-color-defcustom ()
  "The fade color defcustom has the documented default."
  (should (equal ps/done-fade-color "gray75")))

(ert-deftest ps/done--fade-color-resolves-literal ()
  "A literal color string is returned unchanged."
  (let ((ps/done-fade-color "gray75"))
    (should (equal (ps/done--fade-color) "gray75"))))

(ert-deftest ps/done--fade-color-auto-derives-from-theme ()
  "The `auto' symbol resolves to a concrete color string (shadow or fallback)."
  (let ((ps/done-fade-color 'auto))
    (let ((resolved (ps/done--fade-color)))
      (should (stringp resolved))
      (should-not (eq resolved 'auto)))))

;;; -------------------------------------------------------
;;; Fade overlays
;;; -------------------------------------------------------

(ert-deftest ps/done--fade-creates-overlay-on-done ()
  "Fading a buffer with a DONE task creates at least one fade overlay."
  (ps/done-test--with-org-buffer ps/done-test--sample-org
    (ps/done-fade-subtrees)
    (should (cl-some (lambda (ov) (overlay-get ov 'ps-done-fade))
                     (overlays-in (point-min) (point-max))))))

(ert-deftest ps/done--clear-removes-overlays ()
  "Clearing removes all fade overlays previously created."
  (ps/done-test--with-org-buffer ps/done-test--sample-org
    (ps/done-fade-subtrees)
    (ps/done--clear-fade-overlays)
    (should-not (cl-some (lambda (ov) (overlay-get ov 'ps-done-fade))
                         (overlays-in (point-min) (point-max))))))

(ert-deftest ps/done--no-fade-without-done ()
  "A buffer with no DONE tasks gets no fade overlays."
  (ps/done-test--with-org-buffer "* TODO Only active\nbody\n"
    (ps/done-fade-subtrees)
    (should-not (cl-some (lambda (ov) (overlay-get ov 'ps-done-fade))
                         (overlays-in (point-min) (point-max))))))

(ert-deftest ps/done--fade-idempotent ()
  "Running fade twice does not accumulate duplicate overlays."
  (ps/done-test--with-org-buffer ps/done-test--sample-org
    (ps/done-fade-subtrees)
    (let ((n1 (length (cl-remove-if-not
                       (lambda (ov) (overlay-get ov 'ps-done-fade))
                       (overlays-in (point-min) (point-max))))))
      (ps/done-fade-subtrees)
      (let ((n2 (length (cl-remove-if-not
                         (lambda (ov) (overlay-get ov 'ps-done-fade))
                         (overlays-in (point-min) (point-max))))))
        (should (= n1 n2))))))

(ert-deftest ps/done--refade-excludes-new-sibling-heading ()
  "Rebuilding fades after typing a new sibling heading leaves it un-faded.
Regression: text typed right after a DONE task used to inherit the stale fade
overlay; a fresh `ps/done-fade-subtrees' must not cover the new heading."
  (ps/done-test--with-org-buffer ps/done-test--sample-org
    (ps/done-fade-subtrees)
    ;; Type a new sibling heading at the end of the DONE subtree (just before
    ;; the next heading), mimicking the user's edit.
    (goto-char (point-min))
    (should (re-search-forward "^\\* TODO Another active" nil t))
    (beginning-of-line)
    (insert "* This is a test\n")
    ;; Re-fade (the debounced worker does exactly this).
    (ps/done-fade-subtrees)
    ;; No fade overlay should touch the new heading line.
    (goto-char (point-min))
    (should (re-search-forward "^\\* This is a test" nil t))
    (let ((line-beg (line-beginning-position))
          (line-end (line-end-position)))
      (should-not (cl-some (lambda (ov) (overlay-get ov 'ps-done-fade))
                           (overlays-in line-beg line-end))))))

(ert-deftest ps/done--fade-survives-a-half-typed-heading ()
  "A lone \"*\" in a file with no headline yet does not signal.
Regression: the scan used `org-heading-regexp', whose title part is
optional, so it matched the bare star a user has just typed.  Org does not
consider that a headline, so `org-get-todo-state' walked back looking for
one and signalled \"Before first headline\" -- surfaced from the debounced
idle timer while the user was still typing."
  (ps/done-test--with-org-buffer "#+TITLE: Unsorted\n\nSome text\n\n*"
    (should-not (ps/done-fade-subtrees))))

(ert-deftest ps/done--fade-does-not-rescan-a-star-only-line ()
  "A star-only line inside a DONE subtree yields one fade overlay, not two.
The old regexp matched that line, and since Org resolves it to the DONE
headline above, the scan faded the same region a second time -- overlays
stacked on every rebuild until the star grew a title."
  (ps/done-test--with-org-buffer "* DONE Finished\nBody.\n\n*\n\n* TODO Next\n"
    (ps/done-fade-subtrees)
    (should (= 1 (length (cl-remove-if-not
                          (lambda (ov)
                            ;; Timestamp pills also carry the fade tag.
                            (and (overlay-get ov 'ps-done-fade)
                                 (not (overlay-get ov 'display))))
                          (overlays-in (point-min) (point-max))))))))

(ert-deftest ps/done--refade-demotes-errors ()
  "The debounced worker never lets an error escape into the idle timer."
  (ps/done-test--with-org-buffer ps/done-test--sample-org
    (cl-letf (((symbol-function 'ps/done-fade-subtrees)
               (lambda (&rest _) (error "Boom"))))
      (should-not (ps/done--refade-now (current-buffer))))
    ;; The pending-timer flag is cleared even when the rebuild fails, so the
    ;; next edit can arm a new timer.
    (should-not ps/done--refade-timer)))

;;; -------------------------------------------------------
;;; Folding
;;; -------------------------------------------------------

(ert-deftest ps/done--fold-helper-folds-body ()
  "The fold helper, called on a heading, folds that subtree's body."
  (ps/done-test--with-org-buffer ps/done-test--sample-org
    (goto-char (point-min))
    (should (re-search-forward "^\\* DONE" nil t))
    (beginning-of-line)
    (ps/done--fold-subtree-keep-newlines)
    (forward-line 2)                ; into the DONE body
    (should (org-fold-folded-p (point)))))

(ert-deftest ps/done--fold-helper-preserves-trailing-blank ()
  "The fold helper leaves the trailing blank line before the next heading visible."
  (ps/done-test--with-org-buffer ps/done-test--sample-org
    (goto-char (point-min))
    (should (re-search-forward "^\\* DONE" nil t))
    (beginning-of-line)
    (ps/done--fold-subtree-keep-newlines)
    ;; The blank line just before the next heading must not be folded.
    (goto-char (point-min))
    (should (re-search-forward "^\\* TODO Another active" nil t))
    (forward-line -1)               ; the blank line before it
    (should-not (org-fold-folded-p (point)))))

(ert-deftest ps/done--collapse-and-expand-run-clean ()
  "The collapse/expand map-entries wrappers execute without error."
  (ps/done-test--with-org-buffer ps/done-test--sample-org
    (should (progn (ps/done-collapse-subtrees) t))
    (should (progn (ps/done-collapse) t))
    (should (progn (ps/done-expand) t))))

;;; -------------------------------------------------------
;;; Hook setup
;;; -------------------------------------------------------

(ert-deftest ps/done--setup-hooks-registers-all ()
  "setup-hooks installs the buffer-local hooks."
  (ps/done-test--with-org-buffer ps/done-test--sample-org
    (ps/done-setup-hooks)
    (should (memq #'ps/done--clear-fade-overlays before-save-hook))
    (should (memq #'ps/done-fade-subtrees after-save-hook))
    (should (memq #'ps/done-fade-subtrees org-cycle-hook))
    (should (memq #'ps/done--after-todo-change-refresh
                  org-after-todo-state-change-hook))
    (should (memq #'ps/done--refresh-after-revert after-revert-hook))
    (should (memq #'ps/done--schedule-refade after-change-functions))))

;;; -------------------------------------------------------
;;; TODO-change refresh + timestamp strike-through
;;; -------------------------------------------------------

(ert-deftest ps/done--after-todo-change-fades ()
  "Refreshing after a change to DONE creates a fade overlay."
  (ps/done-test--with-org-buffer ps/done-test--sample-org
    (goto-char (point-min))
    (should (re-search-forward "^\\* DONE" nil t))
    (beginning-of-line)
    (let ((org-state "DONE"))
      (ps/done--after-todo-change-refresh))
    (should (cl-some (lambda (ov) (overlay-get ov 'ps-done-fade))
                     (overlays-in (point-min) (point-max))))))

(ert-deftest ps/done--fade-strikes-timestamp ()
  "A timestamp inside a DONE subtree gets a strike-through fade overlay."
  (ps/done-test--with-org-buffer ps/done-test--sample-org
    (ps/done-fade-subtrees)
    (should (cl-some
             (lambda (ov)
               (let ((face (overlay-get ov 'face)))
                 (and (overlay-get ov 'ps-done-fade)
                      (listp face)
                      (plist-get face :strike-through))))
             (overlays-in (point-min) (point-max))))))

;;; test-ps-done.el ends here
