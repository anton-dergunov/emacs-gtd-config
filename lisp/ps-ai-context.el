;;; ps-ai-context.el --- Generate the AI assistant's context file -*- lexical-binding: t; -*-

;;; Commentary:
;; An AI assistant editing the user's Org files (see lisp/ps-claude.el and
;; samples/realistic/AGENTS.md) needs two kinds of facts that a human would
;; otherwise have to keep up to date by hand:
;;
;;   1. How tasks are represented -- TODO keywords, priorities, tags, DONE
;;      logging, journal layout, which directory feeds the agenda. config.org is
;;      the single source of truth for all of it.
;;   2. What each plan file is for -- taken from each file's `#+SUBTITLE:' line,
;;      so the assistant can route something without opening every file.
;;
;; Duplicating either into a hand-written AGENTS.md creates a second source that
;; silently drifts; re-deriving them every session wastes tokens. So Emacs
;; generates them into one file, `ps/ai-context-file' (by default
;; `.claude/generated-context.md' under `my-org-base-directory'), which AGENTS.md
;; simply points at. AGENTS.md itself stays entirely hand-written and is never
;; touched from here -- which also means the generated file can be gitignored.
;; See design-docs/agent-context-sync.md for the fuller analysis.
;;
;; `ps/ai-context-sync' writes that file, but only when the rendered content has
;; actually changed, so a no-op run leaves the file's mtime untouched (this
;; matters for git/Obsidian-style file sync). `ps/ai-context--render-document'
;; and the renderers below it are pure functions of explicit values, kept
;; separate so they can be tested without any live Emacs/Org state.
;;
;; It is called from two places. Once at the very end of config.org, after every
;; value it reads has already been set earlier in the same load pass -- that
;; single call site covers both a fresh Emacs startup and `ps/reload-config'
;; (`C-c p R'), since the latter just re-runs config.org end to end. And, via
;; `ps/ai-context-setup-hooks', a debounced rescan after any scanned .org file is
;; saved, which is what keeps the file index current as subtitles are edited and
;; files are added. Saving config.org (`ps/tangle-config-on-save') deliberately
;; does *not* trigger a sync: it only re-tangles to config.el on disk without
;; re-executing the buffer, so the live values (e.g. `org-todo-keywords') haven't
;; actually changed yet -- syncing there would regenerate from stale state.

;;; Code:

(require 'subr-x)
(require 'ps-org-files)

(defvar my-org-base-directory)
(defvar org-todo-keywords)
(defvar org-highest-priority)
(defvar org-lowest-priority)
(defvar org-log-done)
(defvar org-tag-alist)
(defvar org-tag-persistent-alist)
(defvar org-journal-dir)
(defvar org-journal-file-format)

(defgroup ps-ai-context nil
  "Generate the AI assistant's context file from config.org and the notes."
  :group 'ps)

(defcustom ps/ai-context-enabled t
  "When non-nil, `ps/ai-context-sync' regenerates `ps/ai-context-file'."
  :type 'boolean
  :group 'ps-ai-context)

(defcustom ps/ai-context-file ".claude/generated-context.md"
  "Where the generated context is written, relative to `my-org-base-directory'.
Missing parent directories are created.  The default keeps it out of the way:
a dotted directory is excluded from the .org scan (see
`ps/org-files-exclude-directories') and hidden in the file tree, and the file
can be gitignored since Emacs rewrites it on every startup."
  :type 'string
  :group 'ps-ai-context)

(defcustom ps/ai-context-save-debounce 2.0
  "Idle seconds to wait after saving an .org file before regenerating.
Saves arrive in bursts, and a regeneration reads the header of every scanned
file, so the work is coalesced rather than done once per save."
  :type 'number
  :group 'ps-ai-context)

(defcustom ps/ai-context-next-keyword nil
  "TODO keyword the agenda gathers into its \"Next up\" section, or nil.
Set from config.org, which owns the keyword names -- when non-nil, the
generated file tells an assistant that marking a task with this keyword
surfaces it in the agenda. nil omits that fact entirely."
  :type '(choice (const :tag "No such section" nil) string)
  :group 'ps-ai-context)

(defconst ps/ai-context--header-bytes 4096
  "How much of a file's start to read when looking for its `#+SUBTITLE:'.
The line belongs in the file header, so there is no reason to read further.")

(defvar ps/ai-context--save-timer nil
  "Pending one-shot idle timer scheduled by `ps/ai-context--maybe-sync-after-save'.")

(defun ps/ai-context--parse-todo-keywords (raw)
  "Split RAW (the value of `org-todo-keywords') into (ACTIVE . DONE) lists.
Reads the first sequence only, strips fast-select suffixes like \"(t)\" from
each keyword, and splits on the \"|\" separator into active vs. terminal
states. If there is no \"|\", every keyword is treated as active."
  (let* ((seq (cdr (car raw)))
         (clean (lambda (kw) (replace-regexp-in-string "(.*)\\'" "" kw)))
         (pipe (seq-position seq "|" #'string=)))
    (if pipe
        (cons (mapcar clean (seq-take seq pipe))
              (mapcar clean (seq-drop seq (1+ pipe))))
      (cons (mapcar clean seq) nil))))

(defun ps/ai-context--tag-names (alist)
  "Extract plain tag-name strings from ALIST (`org-tag-alist'-shaped), or nil.
Skips group markers and other non-string entries."
  (delq nil (mapcar (lambda (entry)
                       (cond ((stringp entry) entry)
                             ((and (consp entry) (stringp (car entry))) (car entry))
                             (t nil)))
                     alist)))

(defun ps/ai-context--terminal-state-sentence (done-keywords)
  "Describe DONE-KEYWORDS (a list of plain keyword names) as terminal states."
  (let ((quoted (mapcar (lambda (k) (format "`%s`" k)) done-keywords)))
    (cond
     ((null quoted) "")
     ((= (length quoted) 1)
      (format "%s is the only terminal state; the rest are all \"open\"."
              (car quoted)))
     (t
      (format "%s are the terminal states; the rest are all \"open\"."
              (mapconcat #'identity quoted ", "))))))

(defun ps/ai-context--subtitle-from-header (text)
  "Return the `#+SUBTITLE:' value in TEXT, trimmed, or nil if there is none.
Only the file header is searched -- scanning stops at the first headline, so a
`#+SUBTITLE:' further down (inside a subtree, or in an example block) is
ignored. Pure: TEXT is the start of a file, not a buffer or a filename."
  (let ((case-fold-search t)
        (result nil))
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (catch 'done
        (while (not (eobp))
          (let ((line (buffer-substring-no-properties
                       (line-beginning-position) (line-end-position))))
            (cond
             ((string-match-p "\\`\\*" line) (throw 'done nil))
             ((string-match "\\`[ \t]*#\\+SUBTITLE:[ \t]*\\(.*\\)\\'" line)
              (let ((value (string-trim (match-string 1 line))))
                (unless (string-empty-p value)
                  (setq result value))
                (throw 'done nil)))))
          (forward-line 1))))
    result))

(defun ps/ai-context--file-subtitle (file)
  "Return the `#+SUBTITLE:' of FILE, or nil.
Reads only the first `ps/ai-context--header-bytes' bytes."
  (with-temp-buffer
    (insert-file-contents file nil 0 ps/ai-context--header-bytes)
    (ps/ai-context--subtitle-from-header (buffer-string))))

(defun ps/ai-context--collect-entries ()
  "Return an alist of (RELATIVE-PATH . SUBTITLE) for every scanned .org file.
The file set and its exclusions come from `ps/org-files-all', so this index can
never list something the agenda does not see. Files without a `#+SUBTITLE:' are
omitted. Paths are relative to `my-org-base-directory'."
  (delq nil
        (mapcar (lambda (file)
                  (let ((subtitle (ps/ai-context--file-subtitle file)))
                    (when subtitle
                      (cons (file-relative-name file my-org-base-directory)
                            subtitle))))
                (ps/org-files-all))))

(defun ps/ai-context--escape-cell (text)
  "Escape TEXT for use inside a markdown table cell."
  (replace-regexp-in-string "|" "\\\\|" text))

(defun ps/ai-context--render-file-index (entries)
  "Render ENTRIES as a markdown file-index section, or \"\" when empty.
ENTRIES is an alist of (RELATIVE-PATH . SUBTITLE), already in display order.
Pure: formats its argument and reads no Emacs/Org state."
  (if (null entries)
      ""
    (concat
     "\n"
     "## File index\n"
     "\n"
     "What each file is for, taken from its `#+SUBTITLE:` line. Read the target file's\n"
     "own opening prose before routing anything into it -- the subtitle is only a label.\n"
     "Files with no `#+SUBTITLE:` are not listed here.\n"
     "\n"
     "| File | Purpose |\n"
     "|---|---|\n"
     (mapconcat (lambda (entry)
                  (format "| `%s` | %s |\n"
                          (car entry)
                          (ps/ai-context--escape-cell (cdr entry))))
                entries ""))))

(defun ps/ai-context--render-conventions (active-keywords done-keywords
                                           priority-high priority-low
                                           agenda-subdir tag-names
                                           journal-subdir journal-format
                                           log-done next-keyword)
  "Render the generated conventions section as markdown.
ACTIVE-KEYWORDS and DONE-KEYWORDS are lists of plain TODO keyword names (no
fast-select suffix), in `org-todo-keywords' order. PRIORITY-HIGH/-LOW are the
`org-highest-priority'/`org-lowest-priority' characters. AGENDA-SUBDIR is the
path, relative to the Org base, recursively scanned for the agenda -- nil,
\".\" or \"./\" mean the Org base directory itself, which is rendered as
\"these notes\" rather than as a subdirectory name.
TAG-NAMES is a list of tag strings, or nil if no fixed tag list is defined.
JOURNAL-SUBDIR/JOURNAL-FORMAT are `org-journal-dir' (relative to the Org
base) and `org-journal-file-format', or nil if no journal is configured.
LOG-DONE mirrors `org-log-done'.
NEXT-KEYWORD is `ps/ai-context-next-keyword' -- the keyword the agenda pulls
into its \"Next up\" section, or nil to omit that fact.
Pure: takes no Emacs/Org state, only formats its arguments, so it is
ERT-testable in isolation from `ps/ai-context-sync'."
  (let* ((states-str (mapconcat (lambda (k) (format "`%s`" k))
                                 (append active-keywords done-keywords) " → "))
         (whole-tree (member (or agenda-subdir ".") '("." "./")))
         ;; How the scanned area is named in prose, and where "elsewhere" is.
         (scope (if whole-tree "these notes" (format "`%s`" agenda-subdir))))
    (concat
     "## Current conventions (read from the Emacs configuration)\n"
     "\n"
     (format "- **Task states, in order:** %s.\n  %s\n"
             states-str (ps/ai-context--terminal-state-sentence done-keywords))
     (format (concat "- **Priorities:** `[#%s]` (highest) to `[#%s]` (lowest). `[#%s]` tasks get\n"
                      "  pulled into a dedicated \"High-priority\" section of the agenda -- use it\n"
                      "  for what genuinely matters.\n")
             (char-to-string priority-high) (char-to-string priority-low)
             (char-to-string priority-high))
     (if next-keyword
         (format (concat "- **Picking what to start next:** a task marked `%s` is gathered into the\n"
                          "  agenda's \"Next up\" section. Keep that shortlist short -- it is meant to be\n"
                          "  scannable, not a second task list.\n")
                 next-keyword)
       "")
     (if tag-names
         (format "- **Tags:** a fixed set is defined: %s. Prefer these over inventing new ones.\n"
                 (mapconcat (lambda (tg) (format "`%s`" tg)) tag-names ", "))
       (concat "- **Tags:** no fixed tag list. Reuse whatever tags a file already has; don't\n"
               "  invent a scheme.\n"))
     (if whole-tree
         (concat "- **Where tasks live:** every `.org` file in these notes feeds the agenda, at\n"
                 "  any depth. The directory layout is yours to choose -- put a task in\n"
                 "  whichever file fits, and it will show up.\n")
       (format (concat "- **Where tasks live:** only `.org` files under `%s` (any depth) feed the\n"
                        "  agenda. Files elsewhere are not scanned automatically -- put something\n"
                        "  under `%s` if you want it to show up in the agenda.\n")
               agenda-subdir agenda-subdir))
     (if log-done
         "- **Marking something DONE** logs a `CLOSED:` timestamp automatically.\n"
       (concat "- **Marking something DONE** does not log a timestamp automatically -- don't\n"
               "  add a `CLOSED:` line or logbook entry yourself unless the file already does.\n"))
     (if journal-subdir
         (format (concat "- **Journaling:** if asked to journal something, that goes in a file under\n"
                          "  `%s`, named like `%s` -- not in %s. The journal is not\n"
                          "  scanned for the agenda.\n")
                 journal-subdir journal-format scope)
       ""))))

(defun ps/ai-context--render-document (active-keywords done-keywords
                                        priority-high priority-low
                                        agenda-subdir tag-names
                                        journal-subdir journal-format
                                        log-done next-keyword entries)
  "Render the whole generated context file.
The arguments up to NEXT-KEYWORD are passed straight to
`ps/ai-context--render-conventions'; ENTRIES goes to
`ps/ai-context--render-file-index'. Pure, like both of them."
  (concat
   "<!-- Generated by Emacs (ps/ai-context-sync) from config.org and these notes.\n"
   "     Do not edit: it is rewritten on every Emacs start. Change config.org, or a\n"
   "     file's own #+SUBTITLE: line, instead. -->\n"
   "\n"
   (ps/ai-context--render-conventions
    active-keywords done-keywords priority-high priority-low
    agenda-subdir tag-names journal-subdir journal-format
    log-done next-keyword)
   (ps/ai-context--render-file-index entries)))

;;;###autoload
(defun ps/ai-context-sync ()
  "Regenerate `ps/ai-context-file' under `my-org-base-directory'.
Reads the live task-representation settings from this config and the
`#+SUBTITLE:' of every scanned .org file, renders them via
`ps/ai-context--render-document', and writes the result -- but only when it
actually differs from what is already on disk, so a no-op leaves the file's
mtime untouched. Does nothing if `ps/ai-context-enabled' is nil or
`my-org-base-directory' isn't set."
  (interactive)
  (when (and ps/ai-context-enabled
             (boundp 'my-org-base-directory) my-org-base-directory)
    (let* ((file (expand-file-name ps/ai-context-file my-org-base-directory))
           (parsed (ps/ai-context--parse-todo-keywords org-todo-keywords))
           ;; Read from the same source as `ps/agenda-files-refresh', so the
           ;; generated file can never drift from what is actually scanned.
           ;; "." when the scan root is the Org base directory itself.
           (agenda-subdir (file-relative-name (ps/org-files-root)
                                              my-org-base-directory))
           (tag-names (or (ps/ai-context--tag-names org-tag-alist)
                           (ps/ai-context--tag-names org-tag-persistent-alist)))
           (journal-subdir (and (boundp 'org-journal-dir) org-journal-dir
                                 (file-relative-name org-journal-dir
                                                      my-org-base-directory)))
           (journal-format (and (boundp 'org-journal-file-format)
                                 org-journal-file-format))
           (new-text (ps/ai-context--render-document
                      (car parsed) (cdr parsed)
                      org-highest-priority org-lowest-priority
                      agenda-subdir tag-names
                      journal-subdir journal-format
                      org-log-done ps/ai-context-next-keyword
                      (ps/ai-context--collect-entries)))
           (old-text (and (file-exists-p file)
                          (with-temp-buffer
                            (insert-file-contents file)
                            (buffer-string)))))
      (unless (equal new-text old-text)
        (make-directory (file-name-directory file) t)
        (with-temp-file file (insert new-text))))))

(defun ps/ai-context--scanned-file-p (file root)
  "Return non-nil if FILE would be picked up by a scan of ROOT.
Applies the same name-matched exclusions as `ps/org-files-in-directory' -- the
file's own name against `ps/org-files-exclude-files', and every directory name
between ROOT and FILE against `ps/org-files-exclude-directories' -- but purely
on the path, with no directory walk, since this runs on every save."
  (and file root (string-suffix-p ".org" file)
       (let ((relative (file-relative-name file root)))
         (and (not (string-prefix-p ".." relative))
              (not (file-name-absolute-p relative))
              (not (ps/org-files--name-matches-p
                    (file-name-nondirectory relative)
                    ps/org-files-exclude-files))
              (not (seq-some (lambda (dir)
                               (ps/org-files--name-matches-p
                                dir ps/org-files-exclude-directories))
                             (butlast (split-string relative "/"))))))))

(defun ps/ai-context--maybe-sync-after-save ()
  "Schedule a debounced `ps/ai-context-sync' if the saved buffer is a plan file.
Added to `after-save-hook'. Only scanned .org files matter -- editing a
`#+SUBTITLE:' or adding a file is what changes the generated index."
  (when (and ps/ai-context-enabled
             buffer-file-name
             (ps/ai-context--scanned-file-p buffer-file-name
                                            (ps/org-files-root)))
    (when (timerp ps/ai-context--save-timer)
      (cancel-timer ps/ai-context--save-timer))
    (setq ps/ai-context--save-timer
          (run-with-idle-timer ps/ai-context-save-debounce nil
                               #'ps/ai-context-sync))))

(defun ps/ai-context-setup-hooks ()
  "Keep the generated context file current as plan files are saved."
  (add-hook 'after-save-hook #'ps/ai-context--maybe-sync-after-save))

(provide 'ps-ai-context)
;;; ps-ai-context.el ends here
