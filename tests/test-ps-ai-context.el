;;; test-ps-ai-context.el --- ERT tests for ps-ai-context -*- lexical-binding: t; -*-

(require 'ert)
(add-to-list 'load-path "lisp")
(require 'ps-ai-context)

;; `(defvar my-org-base-directory)' with no value, as in ps-ai-context.el, only
;; marks the symbol special for the rest of *that* file -- it does not make
;; `let'-binding it dynamically visible from this file. Redeclaring it here
;; (mirroring tests/test-ps-claude.el and tests/test-ps-conflicts.el) is what
;; makes the `let'-bindings below actually reach `ps/ai-context-sync'.
(defvar my-org-base-directory)

;;; Parsing org-todo-keywords

(ert-deftest ps/ai-context-test-parse-todo-keywords-with-pipe ()
  "Splits the fast-select suffixes off and separates active vs. terminal states."
  (let ((parsed (ps/ai-context--parse-todo-keywords
                 '((sequence "TODO(t)" "NEXT(n)" "INPR(i)" "WAIT(w)" "MAYB(m)"
                             "|" "DONE(d)")))))
    (should (equal (car parsed) '("TODO" "NEXT" "INPR" "WAIT" "MAYB")))
    (should (equal (cdr parsed) '("DONE")))))

(ert-deftest ps/ai-context-test-parse-todo-keywords-without-pipe ()
  "Without a separator, every keyword is treated as active."
  (let ((parsed (ps/ai-context--parse-todo-keywords '((sequence "TODO(t)" "DONE(d)")))))
    (should (equal (car parsed) '("TODO" "DONE")))
    (should-not (cdr parsed))))

;;; Tag-alist extraction

(ert-deftest ps/ai-context-test-tag-names-plain-and-cons ()
  "Extracts tag strings from both plain-string and (tag . char) entries."
  (should (equal (ps/ai-context--tag-names '("work" ("home" . ?h)))
                 '("work" "home"))))

(ert-deftest ps/ai-context-test-tag-names-skips-group-markers ()
  "Group markers like (:startgroup) are skipped, not stringified."
  (should (equal (ps/ai-context--tag-names '((:startgroup) "work" (:endgroup)))
                 '("work"))))

(ert-deftest ps/ai-context-test-tag-names-nil-for-nil ()
  "An empty/nil alist yields nil, not an empty list of garbage."
  (should-not (ps/ai-context--tag-names nil)))

;;; Rendering (pure function, golden output)

(ert-deftest ps/ai-context-test-render-conventions-golden ()
  "Renders the exact expected block for a representative set of inputs."
  (let ((block (ps/ai-context--render-conventions
                '("TODO" "NEXT" "INPR" "WAIT" "MAYB") '("DONE")
                ?A ?C
                "." nil
                "Journal/" "%Y-%m-%d.org"
                nil)))
    (should (string-prefix-p "<!-- BEGIN ps-generated -->\n" block))
    (should (string-suffix-p "<!-- END ps-generated -->" block))
    (should (string-match-p
             "Task states, in order:\\*\\* `TODO` → `NEXT` → `INPR` → `WAIT` → `MAYB` → `DONE`"
             block))
    (should (string-match-p "`DONE` is the only terminal state" block))
    (should (string-match-p "`\\[#A\\]` (highest) to `\\[#C\\]` (lowest)" block))
    (should (string-match-p "no fixed tag list" block))
    (should (string-match-p "every `.org` file in these notes feeds the agenda" block))
    (should (string-match-p "does not log a timestamp automatically" block))
    (should (string-match-p "`Journal/`, named like `%Y-%m-%d.org`" block))))

(ert-deftest ps/ai-context-test-render-conventions-subdir-scope ()
  "A scan root below the Org base is named explicitly, not called \"these notes\"."
  (let ((block (ps/ai-context--render-conventions
                '("TODO") '("DONE") ?A ?C
                "Areas/" nil
                "Journal/" "%Y-%m-%d.org" nil)))
    (should (string-match-p "under `Areas/` (any depth)" block))
    (should (string-match-p "not in `Areas/`" block))
    (should-not (string-match-p "these notes" block))))

(ert-deftest ps/ai-context-test-render-conventions-no-journal ()
  "Without a journal configured, the journaling bullet is omitted entirely."
  (let ((block (ps/ai-context--render-conventions
                '("TODO") '("DONE") ?A ?C "." nil nil nil nil)))
    (should-not (string-match-p "Journaling" block))))

(ert-deftest ps/ai-context-test-render-conventions-fixed-tags ()
  "A defined tag list is rendered instead of the 'no fixed tag list' line."
  (let ((block (ps/ai-context--render-conventions
                '("TODO") '("DONE") ?A ?C "." '("work" "home") nil nil nil)))
    (should (string-match-p "a fixed set is defined: `work`, `home`" block))
    (should-not (string-match-p "no fixed tag list" block))))

(ert-deftest ps/ai-context-test-render-conventions-log-done ()
  "When DONE logging is on, the block says so instead of the opposite."
  (let ((block (ps/ai-context--render-conventions
                '("TODO") '("DONE") ?A ?C "." nil nil nil t)))
    (should (string-match-p "logs a `CLOSED:` timestamp automatically" block))))

;;; Region replacement

(ert-deftest ps/ai-context-test-replace-region-found ()
  "Replaces text strictly between the markers, keeping the surrounding prose."
  (let ((text (concat "before\n<!-- BEGIN ps-generated -->\nold\n<!-- END ps-generated -->\nafter\n")))
    (should (equal (ps/ai-context--replace-region text "<!-- BEGIN ps-generated -->\nnew\n<!-- END ps-generated -->")
                   "before\n<!-- BEGIN ps-generated -->\nnew\n<!-- END ps-generated -->\nafter\n"))))

(ert-deftest ps/ai-context-test-replace-region-missing-markers ()
  "Returns nil when the text has no delimited region to replace."
  (should-not (ps/ai-context--replace-region "no markers here" "new block")))

;;; ps/ai-context-sync: write-only-on-change behavior

(ert-deftest ps/ai-context-test-sync-writes-and-is-idempotent ()
  "First sync rewrites the file; a second sync with the same inputs is a no-op."
  (let* ((dir (make-temp-file "ps-ai-context-" t))
         (my-org-base-directory (file-name-as-directory dir))
         (file (expand-file-name "AGENTS.md" dir))
         (org-todo-keywords '((sequence "TODO(t)" "|" "DONE(d)")))
         (org-highest-priority ?A)
         (org-lowest-priority ?C)
         (org-log-done nil)
         (org-tag-alist nil)
         (org-tag-persistent-alist nil))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "# Guide\n\n<!-- BEGIN ps-generated -->\nstale\n<!-- END ps-generated -->\n"))
          (ps/ai-context-sync)
          (let ((after-first (with-temp-buffer (insert-file-contents file) (buffer-string))))
            (should (string-match-p "Task states, in order" after-first))
            (should-not (string-match-p "stale" after-first))
            (let ((mtime-before (file-attribute-modification-time (file-attributes file))))
              (sleep-for 1)
              (ps/ai-context-sync)
              (let ((after-second (with-temp-buffer (insert-file-contents file) (buffer-string)))
                    (mtime-after (file-attribute-modification-time (file-attributes file))))
                (should (equal after-first after-second))
                (should (equal mtime-before mtime-after))))))
      (delete-directory dir t))))

(ert-deftest ps/ai-context-test-sync-rewrites-on-real-change ()
  "Changing an input (the TODO keywords) does rewrite the file with new content."
  (let* ((dir (make-temp-file "ps-ai-context-" t))
         (my-org-base-directory (file-name-as-directory dir))
         (file (expand-file-name "AGENTS.md" dir))
         (org-highest-priority ?A)
         (org-lowest-priority ?C)
         (org-log-done nil)
         (org-tag-alist nil)
         (org-tag-persistent-alist nil))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "<!-- BEGIN ps-generated -->\nstale\n<!-- END ps-generated -->\n"))
          (let ((org-todo-keywords '((sequence "TODO(t)" "|" "DONE(d)"))))
            (ps/ai-context-sync))
          (let ((org-todo-keywords '((sequence "TODO(t)" "|" "DONE(d)" "CANCELLED(c)"))))
            (ps/ai-context-sync))
          (let ((final (with-temp-buffer (insert-file-contents file) (buffer-string))))
            (should (string-match-p "CANCELLED" final))))
      (delete-directory dir t))))

(ert-deftest ps/ai-context-test-sync-disabled-noop ()
  "Nothing is written when `ps/ai-context-enabled' is nil."
  (let* ((dir (make-temp-file "ps-ai-context-" t))
         (my-org-base-directory (file-name-as-directory dir))
         (file (expand-file-name "AGENTS.md" dir))
         (ps/ai-context-enabled nil)
         (org-todo-keywords '((sequence "TODO(t)" "|" "DONE(d)")))
         (org-highest-priority ?A)
         (org-lowest-priority ?C)
         (org-log-done nil))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "<!-- BEGIN ps-generated -->\nstale\n<!-- END ps-generated -->\n"))
          (ps/ai-context-sync)
          (let ((after (with-temp-buffer (insert-file-contents file) (buffer-string))))
            (should (string-match-p "stale" after))))
      (delete-directory dir t))))

(ert-deftest ps/ai-context-test-sync-missing-file-noop ()
  "Does nothing (and does not error) when AGENTS.md does not exist."
  (let* ((dir (make-temp-file "ps-ai-context-" t))
         (my-org-base-directory (file-name-as-directory dir))
         (org-todo-keywords '((sequence "TODO(t)" "|" "DONE(d)")))
         (org-highest-priority ?A)
         (org-lowest-priority ?C)
         (org-log-done nil))
    (unwind-protect
        (should-not (ps/ai-context-sync))
      (delete-directory dir t))))

(provide 'test-ps-ai-context)
;;; test-ps-ai-context.el ends here
