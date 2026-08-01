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
                nil nil)))
    (should (string-prefix-p "## Current conventions" block))
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
                "Journal/" "%Y-%m-%d.org" nil nil)))
    (should (string-match-p "under `Areas/` (any depth)" block))
    (should (string-match-p "not in `Areas/`" block))
    (should-not (string-match-p "these notes" block))))

(ert-deftest ps/ai-context-test-render-conventions-no-journal ()
  "Without a journal configured, the journaling bullet is omitted entirely."
  (let ((block (ps/ai-context--render-conventions
                '("TODO") '("DONE") ?A ?C "." nil nil nil nil nil)))
    (should-not (string-match-p "Journaling" block))))

(ert-deftest ps/ai-context-test-render-conventions-fixed-tags ()
  "A defined tag list is rendered instead of the 'no fixed tag list' line."
  (let ((block (ps/ai-context--render-conventions
                '("TODO") '("DONE") ?A ?C "." '("work" "home") nil nil nil nil)))
    (should (string-match-p "a fixed set is defined: `work`, `home`" block))
    (should-not (string-match-p "no fixed tag list" block))))

(ert-deftest ps/ai-context-test-render-conventions-log-done ()
  "When DONE logging is on, the block says so instead of the opposite."
  (let ((block (ps/ai-context--render-conventions
                '("TODO") '("DONE") ?A ?C "." nil nil nil t nil)))
    (should (string-match-p "logs a `CLOSED:` timestamp automatically" block))))

(ert-deftest ps/ai-context-test-render-conventions-next-keyword ()
  "The \"Next up\" fact names the configured keyword when one is set."
  (let ((block (ps/ai-context--render-conventions
                '("TODO" "NEXT") '("DONE") ?A ?C "." nil nil nil nil "NEXT")))
    (should (string-match-p "a task marked `NEXT` is gathered into the" block))
    (should (string-match-p "\"Next up\" section" block))))

(ert-deftest ps/ai-context-test-render-conventions-no-next-keyword ()
  "Without a configured keyword, the \"Next up\" fact is omitted entirely."
  (let ((block (ps/ai-context--render-conventions
                '("TODO" "NEXT") '("DONE") ?A ?C "." nil nil nil nil nil)))
    (should-not (string-match-p "Next up" block))))

;;; #+SUBTITLE: extraction

(ert-deftest ps/ai-context-test-subtitle-plain ()
  "Reads the subtitle out of a normal file header, trimmed."
  (should (equal (ps/ai-context--subtitle-from-header
                  "#+TITLE: Piano\n#+SUBTITLE:   Playing for pleasure  \n\n* Repertoire\n")
                 "Playing for pleasure")))

(ert-deftest ps/ai-context-test-subtitle-absent ()
  "A header with no subtitle yields nil."
  (should-not (ps/ai-context--subtitle-from-header "#+TITLE: Piano\n\n* Repertoire\n")))

(ert-deftest ps/ai-context-test-subtitle-empty-value ()
  "An empty `#+SUBTITLE:' is treated as no subtitle at all."
  (should-not (ps/ai-context--subtitle-from-header "#+SUBTITLE:   \n\n* Repertoire\n")))

(ert-deftest ps/ai-context-test-subtitle-stops-at-first-heading ()
  "A `#+SUBTITLE:' below the first headline is not the file's subtitle."
  (should-not (ps/ai-context--subtitle-from-header
               "#+TITLE: Piano\n* Repertoire\n#+SUBTITLE: not the file's\n")))

;;; File index rendering (pure)

(ert-deftest ps/ai-context-test-render-file-index-empty ()
  "With no subtitled files the whole section disappears."
  (should (equal (ps/ai-context--render-file-index nil) "")))

(ert-deftest ps/ai-context-test-render-file-index-rows ()
  "Each entry becomes one table row, path in backticks, in the given order."
  (let ((index (ps/ai-context--render-file-index
                '(("Play/Piano.org" . "Playing for pleasure")
                  ("Work/Job.org" . "The job search")))))
    (should (string-match-p "^## File index$" index))
    (should (string-match-p "| `Play/Piano.org` | Playing for pleasure |" index))
    (should (string-match-p "| `Work/Job.org` | The job search |" index))
    (should (< (string-match "Piano" index) (string-match "Job" index)))))

(ert-deftest ps/ai-context-test-render-file-index-escapes-pipe ()
  "A `|' in a subtitle is escaped so it cannot break the table."
  (should (string-match-p
           "| `A.org` | left \\\\| right |"
           (ps/ai-context--render-file-index '(("A.org" . "left | right"))))))

;;; Which saved files trigger a resync

(ert-deftest ps/ai-context-test-scanned-file-p ()
  "Mirrors the scan's exclusions: only non-excluded .org files under the root."
  (let ((root "/notes/"))
    (should (ps/ai-context--scanned-file-p "/notes/Work/Job.org" root))
    (should (ps/ai-context--scanned-file-p "/notes/Inbox.org" root))
    (should-not (ps/ai-context--scanned-file-p "/notes/AGENTS.md" root))
    (should-not (ps/ai-context--scanned-file-p "/notes/workspace.org" root))
    (should-not (ps/ai-context--scanned-file-p "/notes/Journal/2026-08-01.org" root))
    (should-not (ps/ai-context--scanned-file-p "/notes/.claude/skills/x.org" root))
    (should-not (ps/ai-context--scanned-file-p "/elsewhere/Job.org" root))))

;;; ps/ai-context-sync: write-only-on-change behavior

(defmacro ps/ai-context-test--with-notes (bindings &rest body)
  "Run BODY in a temp Org base directory with the settings sync reads bound.
BINDINGS are extra `let*' bindings evaluated after the defaults, so a test can
override any of them.  `dir' is the base directory and `file' the generated
context file inside it; the directory is removed afterwards."
  (declare (indent 1))
  `(let* ((dir (file-name-as-directory (make-temp-file "ps-ai-context-" t)))
          (my-org-base-directory dir)
          (ps/org-files-root nil)
          (file (expand-file-name ps/ai-context-file dir))
          (org-todo-keywords '((sequence "TODO(t)" "|" "DONE(d)")))
          (org-highest-priority ?A)
          (org-lowest-priority ?C)
          (org-log-done nil)
          (org-tag-alist nil)
          (org-tag-persistent-alist nil)
          ,@bindings)
     (ignore file)
     (unwind-protect (progn ,@body)
       (delete-directory dir t))))

(defun ps/ai-context-test--read (file)
  "Return the contents of FILE as a string."
  (with-temp-buffer (insert-file-contents file) (buffer-string)))

(ert-deftest ps/ai-context-test-sync-creates-file-and-parent ()
  "The generated file and its (dotted) parent directory are created if missing."
  (ps/ai-context-test--with-notes ()
    (should-not (file-exists-p file))
    (ps/ai-context-sync)
    (should (file-exists-p file))
    (should (string-match-p "Task states, in order" (ps/ai-context-test--read file)))))

(ert-deftest ps/ai-context-test-sync-is-idempotent ()
  "A second sync with the same inputs leaves the file's contents and mtime alone."
  (ps/ai-context-test--with-notes ()
    (ps/ai-context-sync)
    (let ((after-first (ps/ai-context-test--read file))
          (mtime-before (file-attribute-modification-time (file-attributes file))))
      (sleep-for 1)
      (ps/ai-context-sync)
      (should (equal after-first (ps/ai-context-test--read file)))
      (should (equal mtime-before
                     (file-attribute-modification-time (file-attributes file)))))))

(ert-deftest ps/ai-context-test-sync-rewrites-on-real-change ()
  "Changing an input (the TODO keywords) does rewrite the file with new content."
  (ps/ai-context-test--with-notes ()
    (ps/ai-context-sync)
    (let ((org-todo-keywords '((sequence "TODO(t)" "|" "DONE(d)" "CANCELLED(c)"))))
      (ps/ai-context-sync))
    (should (string-match-p "CANCELLED" (ps/ai-context-test--read file)))))

(ert-deftest ps/ai-context-test-sync-indexes-subtitled-files ()
  "Scanned files with a subtitle are listed; ones without, and excluded ones, are not."
  (ps/ai-context-test--with-notes ()
    (make-directory (expand-file-name "Work" dir))
    (with-temp-file (expand-file-name "Work/Job.org" dir)
      (insert "#+TITLE: Job\n#+SUBTITLE: The job search\n\n* TODO Apply\n"))
    (with-temp-file (expand-file-name "Plain.org" dir)
      (insert "#+TITLE: Plain\n\n* TODO Something\n"))
    (with-temp-file (expand-file-name "workspace.org" dir)
      (insert "#+SUBTITLE: Config, not a plan file\n"))
    (ps/ai-context-sync)
    (let ((text (ps/ai-context-test--read file)))
      (should (string-match-p "| `Work/Job.org` | The job search |" text))
      (should-not (string-match-p "Plain.org" text))
      (should-not (string-match-p "workspace.org" text)))))

(ert-deftest ps/ai-context-test-sync-omits-empty-index ()
  "With no subtitled files at all, the file-index section is absent entirely."
  (ps/ai-context-test--with-notes ()
    (with-temp-file (expand-file-name "Plain.org" dir)
      (insert "#+TITLE: Plain\n\n* TODO Something\n"))
    (ps/ai-context-sync)
    (should-not (string-match-p "File index" (ps/ai-context-test--read file)))))

(ert-deftest ps/ai-context-test-sync-disabled-noop ()
  "Nothing is written when `ps/ai-context-enabled' is nil."
  (ps/ai-context-test--with-notes ((ps/ai-context-enabled nil))
    (ps/ai-context-sync)
    (should-not (file-exists-p file))))

(provide 'test-ps-ai-context)
;;; test-ps-ai-context.el ends here
