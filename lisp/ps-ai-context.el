;;; ps-ai-context.el --- Generate AGENTS.md's conventions block from config.org -*- lexical-binding: t; -*-

;;; Commentary:
;; config.org is the single source of truth for how tasks are represented in
;; this system (TODO keywords, priorities, where the agenda looks for tasks,
;; tags, DONE logging, journal layout). An AI assistant editing the user's Org
;; files (see lisp/ps-claude.el and samples/realistic/AGENTS.md) needs those
;; facts, but hand-duplicating them in AGENTS.md would create a second source
;; that can silently drift, and re-reading config.org every session wastes
;; tokens. See design-docs/agent-context-sync.md for the fuller analysis.
;;
;; `ps/ai-context-sync' reads the live values and rewrites a delimited region
;; (between the `ps-generated' BEGIN/END markers) inside
;; `my-org-base-directory'/AGENTS.md -- but only when the rendered content has
;; actually changed, so a no-op run leaves the file's mtime untouched (this
;; matters for git/Obsidian-style file sync). `ps/ai-context--render-conventions'
;; is a pure function of explicit values, kept separate so it can be tested
;; without any live Emacs/Org state.
;;
;; This is called once, at the very end of config.org, after every value it
;; reads has already been set earlier in the same load pass. That single call
;; site covers both a fresh Emacs startup and `ps/reload-config' (`C-c p R'),
;; since the latter just re-runs config.org end to end. Saving config.org
;; (`ps/tangle-config-on-save') deliberately does *not* trigger a sync: it only
;; re-tangles to config.el on disk without re-executing the buffer, so the live
;; values (e.g. `org-todo-keywords') haven't actually changed yet -- syncing
;; there would regenerate from stale state.

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
  "Generate AGENTS.md's conventions block from config.org."
  :group 'ps)

(defcustom ps/ai-context-enabled t
  "When non-nil, `ps/ai-context-sync' refreshes AGENTS.md's generated block."
  :type 'boolean
  :group 'ps-ai-context)

(defconst ps/ai-context--begin-marker "<!-- BEGIN ps-generated -->"
  "Marker opening the generated conventions region in AGENTS.md.")

(defconst ps/ai-context--end-marker "<!-- END ps-generated -->"
  "Marker closing the generated conventions region in AGENTS.md.")

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

(defun ps/ai-context--render-conventions (active-keywords done-keywords
                                           priority-high priority-low
                                           agenda-subdir tag-names
                                           journal-subdir journal-format
                                           log-done)
  "Render the AGENTS.md generated-conventions markdown block.
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
Pure: takes no Emacs/Org state, only formats its arguments, so it is
ERT-testable in isolation from `ps/ai-context-sync'."
  (let* ((states-str (mapconcat (lambda (k) (format "`%s`" k))
                                 (append active-keywords done-keywords) " → "))
         (whole-tree (member (or agenda-subdir ".") '("." "./")))
         ;; How the scanned area is named in prose, and where "elsewhere" is.
         (scope (if whole-tree "these notes" (format "`%s`" agenda-subdir))))
    (concat
     ps/ai-context--begin-marker "\n"
     "<!-- Auto-generated from config.org by ps/ai-context-sync. Edit config.org,\n"
     "     not this block directly -- it will be overwritten. -->\n"
     "\n"
     "## Current conventions (read from this config)\n"
     "\n"
     (format "- **Task states, in order:** %s.\n  %s\n"
             states-str (ps/ai-context--terminal-state-sentence done-keywords))
     (format (concat "- **Priorities:** `[#%s]` (highest) to `[#%s]` (lowest). `[#%s]` tasks get\n"
                      "  pulled into a dedicated \"High-priority\" section of the agenda -- use it\n"
                      "  for what genuinely matters.\n")
             (char-to-string priority-high) (char-to-string priority-low)
             (char-to-string priority-high))
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
       "")
     "\n"
     ps/ai-context--end-marker)))

(defun ps/ai-context--replace-region (text new-block)
  "Return TEXT with its `ps-generated' delimited region replaced by NEW-BLOCK.
NEW-BLOCK must itself include the BEGIN/END markers. Returns nil if TEXT does
not contain both markers in order."
  (let ((begin (string-search ps/ai-context--begin-marker text)))
    (when begin
      (let ((end (string-search ps/ai-context--end-marker text begin)))
        (when end
          (concat (substring text 0 begin)
                  new-block
                  (substring text (+ end (length ps/ai-context--end-marker)))))))))

(defun ps/ai-context-sync ()
  "Refresh the generated conventions block in `my-org-base-directory'/AGENTS.md.
Reads the live task-representation settings from this config, renders them
via `ps/ai-context--render-conventions', and rewrites the file's
`ps-generated' delimited region -- but only when the rendered content
actually differs from what's already there, so a no-op leaves the file's
mtime untouched. Does nothing if `ps/ai-context-enabled' is nil,
`my-org-base-directory' isn't set, the file doesn't exist, or it has no
delimited region to replace."
  (when (and ps/ai-context-enabled
             (boundp 'my-org-base-directory) my-org-base-directory)
    (let ((file (expand-file-name "AGENTS.md" my-org-base-directory)))
      (when (file-exists-p file)
        (let* ((parsed (ps/ai-context--parse-todo-keywords org-todo-keywords))
               ;; Read from the same source as `ps/agenda-files-refresh', so the
               ;; generated block can never drift from what is actually scanned.
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
               (block (ps/ai-context--render-conventions
                       (car parsed) (cdr parsed)
                       org-highest-priority org-lowest-priority
                       agenda-subdir tag-names
                       journal-subdir journal-format
                       org-log-done))
               (old-text (with-temp-buffer
                           (insert-file-contents file)
                           (buffer-string)))
               (new-text (ps/ai-context--replace-region old-text block)))
          (when (and new-text (not (string= new-text old-text)))
            (with-temp-file file (insert new-text))))))))

(provide 'ps-ai-context)
;;; ps-ai-context.el ends here
