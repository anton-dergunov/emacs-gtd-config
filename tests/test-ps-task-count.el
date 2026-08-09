;;; test-ps-task-count.el --- ERT tests for ps-task-count -*- lexical-binding: t; -*-

(require 'ert)
(require 'org)
(require 'cl-lib)
(add-to-list 'load-path "lisp")
(require 'ps-task-count)

;; `(defvar my-org-base-directory)' with no value, as in ps-org-files.el, only
;; marks the symbol special within that file -- repeat it here so the `let'
;; bindings below are dynamic rather than lexical.
(defvar my-org-base-directory)

;;; Test helpers

(defconst ps/task-count-test--todo-keywords
  '((sequence "TODO(t)" "NEXT(n)" "INPR(i)" "WAIT(w)" "MAYB(m)" "|" "DONE(d)"))
  "Mirrors config.org's real `org-todo-keywords', so `org-todo-keywords-1'
and `org-done-keywords' match production in these tests.")

(defconst ps/task-count-test--sample-org
  "* TODO First
* NEXT Second
* Plain section
Some prose, no TODO state.
** DONE Nested done
* DONE Third
* WAIT Fourth
"
  "One heading per state except INPR/MAYB (left at 0), plus a plain heading.")

(defmacro ps/task-count-test--with-org-buffer (content &rest body)
  "Evaluate BODY in a temp `org-mode' buffer containing CONTENT.
`org-todo-keywords' is bound to the repo's real sequence before `org-mode'
runs, so `org-todo-keywords-1'/`org-done-keywords' are populated as they
would be in production."
  (declare (indent 1))
  `(with-temp-buffer
     (let ((org-mode-hook nil)         ; avoid user hooks during tests
           (org-todo-keywords ps/task-count-test--todo-keywords))
       (org-mode))
     (insert ,content)
     (goto-char (point-min))
     ,@body))

(defmacro ps/task-count-test--with-file-buffer (dir relative-name content &rest body)
  "Visit RELATIVE-NAME under DIR (writing CONTENT first), run BODY, clean up.
Unlike `ps/task-count-test--with-org-buffer', this gives BODY a real
`buffer-file-name' so `ps/org-files-in-scope-p' has something to test."
  (declare (indent 3))
  `(let ((file (expand-file-name ,relative-name ,dir)))
     (make-directory (file-name-directory file) t)
     (with-temp-file file (insert ,content))
     (let ((buf (find-file-noselect file)))
       (unwind-protect
           (with-current-buffer buf
             (let ((org-mode-hook nil)
                   (org-todo-keywords ps/task-count-test--todo-keywords))
               (org-mode))
             ,@body)
         (kill-buffer buf)))))

;;; -------------------------------------------------------
;;; ps/task-count--tally
;;; -------------------------------------------------------

(ert-deftest ps/task-count--tally-counts-by-state ()
  "Each state's heading count is correct, ordered by `org-todo-keywords-1'."
  (ps/task-count-test--with-org-buffer ps/task-count-test--sample-org
    (should (equal (ps/task-count--tally)
                   '(("TODO" . 1) ("NEXT" . 1) ("INPR" . 0)
                     ("WAIT" . 1) ("MAYB" . 0) ("DONE" . 2))))))

(ert-deftest ps/task-count--tally-ignores-plain-headings ()
  "A heading with no TODO keyword contributes to no state's count."
  (ps/task-count-test--with-org-buffer "* Plain\n* TODO A\n"
    (should (equal (cdr (assoc "TODO" (ps/task-count--tally))) 1))))

(ert-deftest ps/task-count--tally-all-zero-on-empty-buffer ()
  "Every state is present at 0 when the buffer has no headings at all."
  (ps/task-count-test--with-org-buffer "Just prose, no headings.\n"
    (should (equal (ps/task-count--tally)
                   '(("TODO" . 0) ("NEXT" . 0) ("INPR" . 0)
                     ("WAIT" . 0) ("MAYB" . 0) ("DONE" . 0))))))

;;; -------------------------------------------------------
;;; ps/task-count--total-open / --total-all
;;; -------------------------------------------------------

(ert-deftest ps/task-count--total-open-excludes-done ()
  "The open total sums every state except DONE."
  (ps/task-count-test--with-org-buffer ps/task-count-test--sample-org
    (should (= (ps/task-count--total-open (ps/task-count--tally)) 3))))

(ert-deftest ps/task-count--total-all-includes-done ()
  "The all-states total includes DONE."
  (ps/task-count-test--with-org-buffer ps/task-count-test--sample-org
    (should (= (ps/task-count--total-all (ps/task-count--tally)) 5))))

;;; -------------------------------------------------------
;;; ps/task-count--tooltip
;;; -------------------------------------------------------

(ert-deftest ps/task-count--tooltip-lists-every-state ()
  "The tooltip lists all six states, in order, including zero counts."
  (ps/task-count-test--with-org-buffer ps/task-count-test--sample-org
    (should (equal (ps/task-count--tooltip (ps/task-count--tally))
                   "TODO: 1\nNEXT: 1\nINPR: 0\nWAIT: 1\nMAYB: 0\nDONE: 2"))))

;;; -------------------------------------------------------
;;; ps/task-count--recompute
;;; -------------------------------------------------------

(ert-deftest ps/task-count--recompute-sets-open-and-tooltip ()
  "A normal file with open tasks gets a non-nil count and tooltip."
  (ps/task-count-test--with-org-buffer ps/task-count-test--sample-org
    (ps/task-count--recompute (current-buffer))
    (should (= ps/mode-line--task-count-open 3))
    (should (stringp ps/mode-line--task-count-tooltip))))

(ert-deftest ps/task-count--recompute-nil-when-no-recognized-headings ()
  "A file with no TODO-state headings at all clears both mode-line vars."
  (ps/task-count-test--with-org-buffer "* Plain\nJust a section.\n"
    (ps/task-count--recompute (current-buffer))
    (should-not ps/mode-line--task-count-open)
    (should-not ps/mode-line--task-count-tooltip)))

(ert-deftest ps/task-count--recompute-zero-when-all-done ()
  "A file whose only recognized headings are DONE shows 0, not nil."
  (ps/task-count-test--with-org-buffer "* DONE Only one\n"
    (ps/task-count--recompute (current-buffer))
    (should (eql ps/mode-line--task-count-open 0))
    (should (stringp ps/mode-line--task-count-tooltip))))

(ert-deftest ps/task-count--recompute-bumps-generation ()
  "Each recompute bumps the generation counter the mode-line cache keys on."
  (ps/task-count-test--with-org-buffer ps/task-count-test--sample-org
    (let ((before ps/mode-line--task-count-gen))
      (ps/task-count--recompute (current-buffer))
      (should (= ps/mode-line--task-count-gen (1+ before))))))

;;; -------------------------------------------------------
;;; ps/task-count--schedule (debounce)
;;; -------------------------------------------------------

(ert-deftest ps/task-count--schedule-debounces-to-one-pending-timer ()
  "A second call while a timer is pending does not arm another one."
  (ps/task-count-test--with-org-buffer ps/task-count-test--sample-org
    (unwind-protect
        (progn
          (should-not ps/task-count--timer)
          (ps/task-count--schedule)
          (should ps/task-count--timer)
          (let ((first ps/task-count--timer))
            (ps/task-count--schedule)
            (should (eq first ps/task-count--timer))))
      (when ps/task-count--timer
        (cancel-timer ps/task-count--timer)))))

;;; -------------------------------------------------------
;;; ps/task-count--org-setup (scope gating)
;;; -------------------------------------------------------

(ert-deftest ps/task-count--org-setup-scans-in-scope-file ()
  "A file under the Org base directory gets the hook and an initial count."
  (let ((dir (make-temp-file "ps-task-count-" t)))
    (unwind-protect
        (let ((ps/org-files-root dir))
          (ps/task-count-test--with-file-buffer dir "Inbox.org" "* TODO A\n"
            (ps/task-count--org-setup)
            (should (memq #'ps/task-count--schedule after-change-functions))
            (should (eql ps/mode-line--task-count-open 1))))
      (delete-directory dir t))))

(ert-deftest ps/task-count--org-setup-skips-excluded-file ()
  "workspace.org (excluded by ps-org-files.el) gets no hook and no scan."
  (let ((dir (make-temp-file "ps-task-count-" t)))
    (unwind-protect
        (let ((ps/org-files-root dir))
          (ps/task-count-test--with-file-buffer dir "workspace.org" "* TODO A\n"
            (ps/task-count--org-setup)
            (should-not (memq #'ps/task-count--schedule after-change-functions))
            (should-not ps/mode-line--task-count-open)
            (should (= ps/mode-line--task-count-gen 0))))
      (delete-directory dir t))))

(provide 'test-ps-task-count)
;;; test-ps-task-count.el ends here
