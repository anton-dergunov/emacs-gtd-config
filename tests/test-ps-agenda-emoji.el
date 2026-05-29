;;; test-ps-agenda-emoji.el --- ERT tests for ps-agenda-emoji -*- lexical-binding: t; -*-

(require 'ert)
(add-to-list 'load-path "lisp")
(require 'ps-agenda-emoji)

;; Declare special so a `let' binding below is dynamic (org-agenda may be
;; unloaded in batch, leaving the symbol otherwise lexical here).
(defvar org-agenda-finalize-hook)

;;; A small fixture mimicking rendered agenda lines.

(defconst ps/agenda-emoji-test--sample
  "  work:      TODO Write the report
  home:      Scheduled: NEXT Buy groceries
  proj:      IN-PROGRESS Refactor module
             Some non-task line
  misc:      DONE Already finished
"
  "Agenda-like buffer content. DONE lines must not match (no DONE in the keyword group).")

(defmacro ps/agenda-emoji-test--with-buffer (content &rest body)
  "Run BODY in a temp buffer containing CONTENT."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,content)
     (goto-char (point-min))
     ,@body))

;;; -------------------------------------------------------
;;; defcustom / API
;;; -------------------------------------------------------

(ert-deftest ps/agenda-emoji--matcher-path-is-file ()
  "The matcher path defcustom points at org_emoji_matcher.py."
  (should (stringp ps/agenda-emoji-matcher-path))
  (should (string-suffix-p "org_emoji_matcher.py" ps/agenda-emoji-matcher-path)))

(ert-deftest ps/agenda-emoji--setup-adds-hook ()
  "setup registers the append function on org-agenda-finalize-hook."
  (let ((org-agenda-finalize-hook nil))
    (ps/agenda-emoji-setup)
    (should (memq 'ps/agenda-emoji--append org-agenda-finalize-hook))))

;;; -------------------------------------------------------
;;; extract-tasks
;;; -------------------------------------------------------

(ert-deftest ps/agenda-emoji--extract-finds-todo ()
  "A plain TODO task is extracted with its text only."
  (ps/agenda-emoji-test--with-buffer ps/agenda-emoji-test--sample
    (let ((tasks (ps/agenda-emoji--extract-tasks)))
      (should (member "Write the report" tasks)))))

(ert-deftest ps/agenda-emoji--extract-finds-scheduled-next ()
  "A NEXT task behind a Scheduled: prefix is extracted."
  (ps/agenda-emoji-test--with-buffer ps/agenda-emoji-test--sample
    (let ((tasks (ps/agenda-emoji--extract-tasks)))
      (should (member "Buy groceries" tasks)))))

(ert-deftest ps/agenda-emoji--extract-finds-in-progress ()
  "An IN-PROGRESS task is extracted."
  (ps/agenda-emoji-test--with-buffer ps/agenda-emoji-test--sample
    (let ((tasks (ps/agenda-emoji--extract-tasks)))
      (should (member "Refactor module" tasks)))))

(ert-deftest ps/agenda-emoji--extract-ignores-non-task ()
  "Lines without a TODO-like keyword are not extracted."
  (ps/agenda-emoji-test--with-buffer ps/agenda-emoji-test--sample
    (let ((tasks (ps/agenda-emoji--extract-tasks)))
      (should-not (member "Some non-task line" tasks)))))

(ert-deftest ps/agenda-emoji--extract-ignores-done ()
  "DONE is not in the keyword group, so DONE lines are not extracted."
  (ps/agenda-emoji-test--with-buffer ps/agenda-emoji-test--sample
    (let ((tasks (ps/agenda-emoji--extract-tasks)))
      (should-not (member "Already finished" tasks)))))

;;; -------------------------------------------------------
;;; apply
;;; -------------------------------------------------------

(ert-deftest ps/agenda-emoji--apply-appends-emoji ()
  "Emojis from the map are appended to the end of a matching task line."
  (ps/agenda-emoji-test--with-buffer ps/agenda-emoji-test--sample
    (ps/agenda-emoji--apply '(("Write the report" . ("X" "Y"))))
    (goto-char (point-min))
    (should (re-search-forward "TODO Write the report X Y" nil t))))

(ert-deftest ps/agenda-emoji--apply-skips-unmapped ()
  "A task with no entry in the map is left unchanged."
  (ps/agenda-emoji-test--with-buffer ps/agenda-emoji-test--sample
    (ps/agenda-emoji--apply '(("Nonexistent" . ("Z"))))
    (goto-char (point-min))
    (should-not (re-search-forward " Z$" nil t))))

(ert-deftest ps/agenda-emoji--apply-emoji-has-height-face ()
  "Appended emojis carry the reduced-height face property."
  (ps/agenda-emoji-test--with-buffer ps/agenda-emoji-test--sample
    (ps/agenda-emoji--apply '(("Write the report" . ("X"))))
    (goto-char (point-min))
    (should (re-search-forward "TODO Write the report " nil t))
    ;; Point is now right before the appended "X".
    (should (equal (get-text-property (point) 'face) '(:height 0.8)))))

;;; test-ps-agenda-emoji.el ends here
