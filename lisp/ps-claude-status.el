;;; ps-claude-status.el --- Mode line for Claude Code session buffers -*- lexical-binding: t; -*-

;;; Commentary:
;; Replaces the default eat/minor-mode mode line in *claude-code[...]* buffers
;; with a compact, planning-focused line:
;;
;;   Claude Code · <slug> · <Model Version> [thinking]
;;
;; Session data (slug, model, extended-thinking flag) is read directly from
;; Claude Code's JSONL session files in ~/.claude/projects/.  No hooks or
;; external configuration are required.
;;
;; The mode line updates within ps/claude-status-refresh-interval seconds.
;; A best-effort file-notify watch on the project directory provides faster
;; updates when the OS supports it (catches /model switches and /resume).

;;; Code:

(require 'seq)
(require 'ps-mode-line)

(declare-function ps/claude--session-buffer-p "ps-claude" (buffer-or-name))

;;; Customization

(defgroup ps-claude-status nil
  "Mode line for Claude Code session buffers."
  :group 'ps)

(defcustom ps/claude-status-refresh-interval 8
  "Seconds between JSONL refresh polls for the Claude Code mode line."
  :type 'number
  :group 'ps-claude-status)

;;; Buffer-local state

(defvar-local ps/claude-status--state nil
  "Plist (:file PATH :pos INT :slug STR :model STR :thinking BOOL).
Buffer-local in each *claude-code[...]* buffer.")

(defvar-local ps/claude-status--timer nil
  "Periodic refresh timer for this buffer.")

(defvar-local ps/claude-status--dir-watch nil
  "File-notify watch on the ~/.claude/projects/ tree, or nil.")

;;; JSONL discovery

(defun ps/claude-status--project-dir ()
  "Return the ~/.claude/projects/ subdir for the current org base session.
Derives the directory by encoding `my-org-base-directory' the same way the
Claude Code CLI does: replace every \"/\" in the expanded path with \"-\"."
  (when (and (boundp 'my-org-base-directory) my-org-base-directory)
    (let* ((cwd     (directory-file-name (expand-file-name my-org-base-directory)))
           (encoded (replace-regexp-in-string "/" "-" cwd))
           (dir     (expand-file-name encoded "~/.claude/projects/")))
      (and (file-directory-p dir) dir))))

(defun ps/claude-status--find-latest-jsonl ()
  "Return the most recently modified .jsonl in the session's project directory.
Uses `my-org-base-directory' to identify the right directory, so only files
belonging to the org-base session are considered (not other Claude sessions)."
  (let ((dir (ps/claude-status--project-dir)))
    (when dir
      (let (best best-time)
        (dolist (f (directory-files dir t "\\.jsonl\\'"))
          (when-let ((attrs (file-attributes f)))
            (let ((mt (file-attribute-modification-time attrs)))
              (when (or (null best-time) (time-less-p best-time mt))
                (setq best f best-time mt)))))
        best))))

;;; Model display name

(defun ps/claude-status--display-model (model-id)
  "Return a human-readable label for MODEL-ID.
\"claude-sonnet-4-6\"        → \"Sonnet 4.6\"
\"claude-haiku-4-5-20251001\" → \"Haiku 4.5\"
\"claude-opus-4-8\"          → \"Opus 4.8\"
\"claude-fable-5\"           → \"Fable 5\"
Unknown model IDs are returned unchanged."
  (if (and model-id
           (string-match
            "claude-\\([a-z]+\\)-\\([0-9]+\\)\\(?:-\\([0-9]+\\)\\)?"
            model-id))
      (let ((family (capitalize (match-string 1 model-id)))
            (major  (match-string 2 model-id))
            (minor  (match-string 3 model-id)))
        (if minor
            (concat family " " major "." minor)
          (concat family " " major)))
    (or model-id "")))

;;; Incremental JSONL parse

(defun ps/claude-status--thinking-p (content)
  "Return non-nil if any element of the CONTENT list has type \"thinking\"."
  (seq-some (lambda (c)
              (and (consp c) (equal (alist-get 'type c) "thinking")))
            content))

(defun ps/claude-status--parse-new (state)
  "Read appended lines from :file in STATE starting at :pos.
Mutates and returns STATE."
  (let ((file (plist-get state :file))
        (pos  (plist-get state :pos)))
    (when (and file (file-readable-p file))
      (with-temp-buffer
        (insert-file-contents file nil pos nil)
        (let ((new-bytes (buffer-size)))
          (goto-char (point-min))
          (while (not (eobp))
            (let* ((beg (point))
                   (end (progn (end-of-line) (point)))
                   (line (buffer-substring-no-properties beg end)))
              (when (> (length line) 2)
                (condition-case nil
                    (let* ((obj  (json-parse-string line
                                                    :object-type 'alist
                                                    :array-type  'list))
                           (type (alist-get 'type obj)))
                      (when (equal type "assistant")
                        (let* ((msg     (alist-get 'message obj))
                               (slug    (alist-get 'slug obj))
                               (model   (alist-get 'model msg))
                               (content (alist-get 'content msg)))
                          ;; slug: take the latest (reflects current session after /resume)
                          (when slug (plist-put state :slug slug))
                          ;; model: take the latest real model (skip synthetic placeholders)
                          (when (and model (not (string-prefix-p "<" model)))
                            (plist-put state :model model))
                          ;; thinking: latch on, never reset within a session
                          (unless (plist-get state :thinking)
                            (when (ps/claude-status--thinking-p content)
                              (plist-put state :thinking t))))))
                  (error nil)))
              (forward-line 1)))
          (plist-put state :pos (+ pos new-bytes))))))
  state)

;;; Mode line renderer

(defun ps/claude-status--render (state)
  "Return the mode-line string for STATE."
  (let* ((sep        ps/mode-line-separator)
         (slug       (and state (plist-get state :slug)))
         (model-id   (and state (plist-get state :model)))
         (thinking   (and state (plist-get state :thinking)))
         (model-str  (or (ps/claude-status--display-model model-id) model-id ""))
         (slug-str   (if slug (truncate-string-to-width slug 32 nil nil "…") ""))
         (effort-str (if thinking " [thinking]" "")))
    (concat " "
            (propertize "Claude Code" 'face 'mode-line-emphasis)
            (if (string-empty-p slug-str) "" (concat sep slug-str))
            (if (string-empty-p model-str) "" (concat sep model-str effort-str)))))

;;; Refresh

(defun ps/claude-status--do-refresh (buffer)
  "Refresh state for BUFFER and force a mode-line redisplay."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (unless ps/claude-status--state
        (setq ps/claude-status--state
              (list :file nil :pos 0 :slug nil :model nil :thinking nil)))
      (let ((file (ps/claude-status--find-latest-jsonl))
            (cur  (plist-get ps/claude-status--state :file)))
        ;; Different file: new session started or /resume used.
        ;; Reset position to re-read from the top, but KEEP slug/model/thinking so
        ;; the mode line stays informative until the new file has assistant entries.
        (unless (equal file cur)
          (plist-put ps/claude-status--state :file file)
          (plist-put ps/claude-status--state :pos 0))
        (when file
          (ps/claude-status--parse-new ps/claude-status--state)))
      (force-mode-line-update))))

;;; File-notify watch on the projects directory tree (best-effort)

(defun ps/claude-status--install-watch (buffer)
  "Watch ~/.claude/projects/ for changes; on any event refresh BUFFER."
  (let ((dir (expand-file-name "~/.claude/projects/")))
    (when (and (featurep 'filenotify) (file-directory-p dir))
      (condition-case nil
          (with-current-buffer buffer
            (setq ps/claude-status--dir-watch
                  (file-notify-add-watch
                   dir '(change)
                   (lambda (_event)
                     (when (buffer-live-p buffer)
                       (ps/claude-status--do-refresh buffer))))))
        (error nil)))))

;;; Timer

(defun ps/claude-status--start-timer (buffer)
  "Start the periodic refresh timer for BUFFER."
  (with-current-buffer buffer
    (setq ps/claude-status--timer
          (run-with-timer 0 ps/claude-status-refresh-interval
                          #'ps/claude-status--do-refresh buffer))))

;;; Cleanup

(defun ps/claude-status--cleanup ()
  "Cancel timer and file-notify watch.  Runs from `kill-buffer-hook'."
  (when (timerp ps/claude-status--timer)
    (cancel-timer ps/claude-status--timer)
    (setq ps/claude-status--timer nil))
  (when ps/claude-status--dir-watch
    (condition-case nil
        (file-notify-rm-watch ps/claude-status--dir-watch)
      (error nil))
    (setq ps/claude-status--dir-watch nil)))

;;; Buffer installation

(defun ps/claude-status--install ()
  "Install the Claude Code mode line in the current buffer if it is a session buffer.
Added to `eat-mode-hook' by `ps/claude-status-setup'."
  (when (ps/claude--session-buffer-p (current-buffer))
    (setq-local ps/claude-status--state
                (list :file nil :pos 0 :slug nil :model nil :thinking nil))
    (setq-local mode-line-format
                '((:eval (ps/claude-status--render ps/claude-status--state))))
    (add-hook 'kill-buffer-hook #'ps/claude-status--cleanup nil t)
    (ps/claude-status--install-watch (current-buffer))
    (ps/claude-status--start-timer (current-buffer))))

;;;###autoload
(defun ps/claude-status-setup ()
  "Enable the compact Claude Code mode line in session buffers."
  (add-hook 'eat-mode-hook #'ps/claude-status--install))

(provide 'ps-claude-status)
;;; ps-claude-status.el ends here
