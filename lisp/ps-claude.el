;;; ps-claude.el --- Claude Code IDE window/resize tweaks -*- lexical-binding: t; -*-

;;; Commentary:
;; Small fixes for `claude-code-ide.el' (eat backend):
;;
;; 1. The side window opens too wide by default (100 columns); we offer a
;;    tunable `ps/claude-window-width' instead.
;;
;; 2. Resizing the Claude Code window can leave new output garbled until a
;;    second resize.  `claude-code-ide--terminal-reflow-filter' (the
;;    workaround for upstream claude-code#1422) suppresses
;;    `eat--adjust-process-window-size' for height-only changes, but that
;;    function has *already* resized eat's internal terminal by the time it
;;    runs -- the underlying `claude' process is never told its new
;;    dimensions via `set-process-window-size'/SIGWINCH, so it keeps
;;    rendering for the old size until something else triggers a resync.
;;    We add a debounced `window-size-change-functions' hook that re-syncs
;;    the process size via `claude-code-ide--sync-terminal-dimensions' once
;;    the resize settles, regardless of what the reflow filter suppressed.
;;
;; 3. `claude-code-ide--get-working-directory' defaults to the current
;;    project root, which for any buffer in this repo is this config's own
;;    source tree. This integration exists to assist with org-mode planning
;;    content, not this config's source code, so we override it to always
;;    use `my-org-base-directory' instead.
;;
;; 4. Selection / active-buffer tracking is keyed by project: the running
;;    session is stored under the working directory (pinned to
;;    `my-org-base-directory' by #3), but
;;    `claude-code-ide-mcp--get-buffer-project' looks the session up by
;;    `project-current', which for an Org file in this repo resolves to the
;;    git repo root -- a different key.  The lookup misses, so the editor
;;    selection never reaches Claude.  We advise that lookup to return
;;    `my-org-base-directory' for buffers under it, matching the session key.
;;
;; 5. When Claude writes a file it visits it again (via `find-file-noselect'
;;    in the ediff/open path).  If `global-auto-revert' has not yet reverted
;;    the unmodified buffer, `find-file-noselect' pops a "Reread from disk?"
;;    prompt.  We advise the open/diff entry points to quietly revert a
;;    *stale, unmodified* visiting buffer first, closing the race.  Modified
;;    buffers are deliberately left alone, so a genuine edit conflict still
;;    prompts ("Discard your edits?").
;;
;; 6. When the diff window first opens, eat's terminal width state can briefly
;;    desync and `eat--process-output-queue' signals `args-out-of-range' from
;;    inside its timer.  We wrap it to swallow that transient error and
;;    schedule a resync so the terminal re-renders cleanly.
;;
;; 7. `claude-code-ide-window-side' is a fixed `right' by default, which is
;;    impractical when the Emacs frame itself is taller than it is wide (e.g.
;;    positioned on the narrow edge of an ultrawide monitor).  We advise the
;;    side window's display function to dock `right' when the frame is wider
;;    than tall, `bottom' otherwise -- recomputed fresh on every open, so it
;;    tracks whatever shape the frame currently has.  Deliberately stays a
;;    side window (not unified with the take-over-the-selected-window rule
;;    used by the Agenda/Calendar/Tasks/Availability/Conflicts views, see
;;    `lisp/ps-window.el'): the point of this panel is to stay in a stable,
;;    predictable spot while you work elsewhere, the same way the file tree
;;    does.
;;
;; If upstream #1422 is fixed and the reflow workaround is no longer needed,
;; the reflow-glitch suppression itself can be disabled with:
;;   (setq claude-code-ide-prevent-reflow-glitch nil)
;; The resync hook below is harmless either way and can stay.

;;; Code:

(require 'seq)

(declare-function claude-code-ide--sync-terminal-dimensions "claude-code-ide")
(declare-function claude-code-ide--get-working-directory "claude-code-ide")
(declare-function claude-code-ide-mcp--get-buffer-project "claude-code-ide-mcp")
(declare-function claude-code-ide-mcp--create-diff-buffers "claude-code-ide-mcp-handlers")
(declare-function claude-code-ide-mcp-handle-open-file "claude-code-ide-mcp-handlers")
(declare-function eat--process-output-queue "eat")
(declare-function claude-code-ide--display-buffer-in-side-window "claude-code-ide")
(defvar claude-code-ide-window-width)
(defvar claude-code-ide-window-side)
(defvar my-org-base-directory)

(defcustom ps/claude-window-width 90
  "Width (in columns) of the Claude Code IDE side window.
Applied to `claude-code-ide-window-width' by `ps/claude-setup'."
  :type 'integer
  :group 'claude-code-ide)

(defcustom ps/claude-resize-debounce-delay 0.3
  "Idle delay, in seconds, before re-syncing Claude Code terminal dimensions
after a window resize settles."
  :type 'number
  :group 'claude-code-ide)

(defvar ps/claude--resize-timer nil
  "Pending idle timer for the next terminal-size resync, or nil.")

(defun ps/claude--session-buffer-p (buffer-or-name)
  "Return non-nil if BUFFER-OR-NAME is a Claude Code session buffer.
Pure string match on the `*claude-code[...]*' naming convention used by
`claude-code-ide.el', so it works without loading that package."
  (let ((name (if (bufferp buffer-or-name)
                   (buffer-name buffer-or-name)
                 buffer-or-name)))
    (and (stringp name) (string-prefix-p "*claude-code[" name))))

(defun ps/claude--resync-windows ()
  "Re-sync terminal dimensions for all live Claude Code session windows."
  (setq ps/claude--resize-timer nil)
  (dolist (window (window-list))
    (when (and (window-live-p window)
               (ps/claude--session-buffer-p (window-buffer window)))
      (claude-code-ide--sync-terminal-dimensions (window-buffer window) window))))

(defun ps/claude--schedule-resync ()
  "(Re)arm the debounced timer that re-syncs Claude Code window dimensions."
  (when (timerp ps/claude--resize-timer)
    (cancel-timer ps/claude--resize-timer))
  (setq ps/claude--resize-timer
        (run-with-idle-timer ps/claude-resize-debounce-delay nil
                              #'ps/claude--resync-windows)))

(defun ps/claude--on-window-size-change (_frame)
  "Schedule a debounced terminal resync after a window-size change.
Added to `window-size-change-functions'; only schedules work when a Claude
Code session buffer is currently displayed."
  (when (seq-some (lambda (w) (ps/claude--session-buffer-p (window-buffer w)))
                   (window-list))
    (ps/claude--schedule-resync)))

(defun ps/claude--working-directory ()
  "Always use `my-org-base-directory' as the Claude Code IDE working directory.
Overrides `claude-code-ide--get-working-directory', whose default
(current project root) would resolve to this config's own source tree
for any buffer in this repo."
  (expand-file-name my-org-base-directory))

;;; Selection / active-buffer project key (fix #4)

(defun ps/claude--path-under-base-p (path)
  "Non-nil if PATH is inside `my-org-base-directory' (by expanded name).
Uses a plain expanded-name prefix test -- not `file-in-directory-p' -- so it
matches how claude-code-ide keys sessions (via `expand-file-name', not
`file-truename') and does not depend on the directory existing on disk."
  (and (stringp path)
       (boundp 'my-org-base-directory)
       my-org-base-directory
       (string-prefix-p (file-name-as-directory
                         (expand-file-name my-org-base-directory))
                        (expand-file-name path))))

(defun ps/claude--buffer-under-org-base-p ()
  "Non-nil if the current buffer visits a file under `my-org-base-directory'."
  (and buffer-file-name (ps/claude--path-under-base-p buffer-file-name)))

(defun ps/claude--buffer-project-advice (orig-fn &rest args)
  "Return `my-org-base-directory' as the project for buffers under it.
Keeps the project key used for selection / active-buffer tracking in sync
with the session key (also pinned to `my-org-base-directory' by
`ps/claude--working-directory').  Without this, selections from Org buffers
are looked up under the git repo root and silently dropped.  Buffers outside
the Org base fall through to ORIG-FN unchanged."
  (if (ps/claude--buffer-under-org-base-p)
      (expand-file-name my-org-base-directory)
    (apply orig-fn args)))

;;; Silent reload of stale, unmodified buffers Claude just wrote (fix #5)

(defun ps/claude--revert-stale-unmodified (path)
  "Quietly revert an unmodified, stale buffer visiting PATH under the Org base.
Closes the race where claude-code-ide re-visits a file it just wrote (via
`find-file-noselect') before `global-auto-revert' reverts the buffer, which
would otherwise pop a \"Reread from disk?\" prompt.  Modified buffers are
left alone so genuine edit conflicts still prompt (\"Discard your edits?\")."
  (when (ps/claude--path-under-base-p path)
    (when-let ((buf (find-buffer-visiting path)))
      (with-current-buffer buf
        (when (and (not (buffer-modified-p))
                   (not (verify-visited-file-modtime buf)))
          (revert-buffer :ignore-auto :noconfirm))))))

(defun ps/claude--diff-revert-advice (old-file-path &rest _)
  "Before opening a diff for OLD-FILE-PATH, refresh a stale unmodified buffer."
  (ps/claude--revert-stale-unmodified old-file-path))

(defun ps/claude--open-revert-advice (arguments &rest _)
  "Before opening a file from ARGUMENTS, refresh a stale unmodified buffer."
  (ps/claude--revert-stale-unmodified (alist-get 'filePath arguments)))

;;; eat output-queue crash guard (fix #6)

(defun ps/claude--eat-output-guard (orig-fn &rest args)
  "Swallow transient `args-out-of-range' errors from eat's output timer.
When the Claude diff window first opens, eat's terminal width state can
momentarily desync, making `eat--process-output-queue' signal
`args-out-of-range' from inside its timer.  Catch it, schedule a window
resync so the terminal re-renders cleanly, and continue."
  (condition-case err
      (apply orig-fn args)
    (args-out-of-range
     (ps/claude--schedule-resync)
     (message "ps/claude: recovered from eat output glitch (%s)"
              (error-message-string err))
     nil)))

;;; Adaptive dock side (fix #7)

(defun ps/claude--adaptive-side-advice (orig-fn &rest args)
  "Dock the Claude Code side window `right' when the frame is wider than
tall, `bottom' otherwise.  Recomputed on every call, so it tracks the
frame's current shape rather than whatever it was when Emacs started."
  (let ((claude-code-ide-window-side
         (if (> (frame-pixel-width) (frame-pixel-height)) 'right 'bottom)))
    (apply orig-fn args)))

(defun ps/claude--install-mode-line ()
  "Replace the default eat mode line with a plain \"Claude Code\" label."
  (when (ps/claude--session-buffer-p (current-buffer))
    (setq-local mode-line-format
                '(" " (:propertize "Claude Code" face mode-line-emphasis)))))

(defun ps/claude-setup ()
  "Apply Claude Code IDE window-size, working-directory and reliability tweaks.
Sets `claude-code-ide-window-width' from `ps/claude-window-width', installs
the debounced resize-resync hook, pins the working directory and project key
to `my-org-base-directory', silences the post-write \"Reread from disk?\"
race for unmodified buffers, guards eat's output timer against transient
`args-out-of-range' glitches, and docks the panel `right'/`bottom' to match
the frame's current shape.  Idempotent."
  (setq claude-code-ide-window-width ps/claude-window-width)
  (add-hook 'window-size-change-functions #'ps/claude--on-window-size-change)
  (advice-add 'claude-code-ide--get-working-directory
              :override #'ps/claude--working-directory)
  (advice-add 'claude-code-ide-mcp--get-buffer-project
              :around #'ps/claude--buffer-project-advice)
  (advice-add 'claude-code-ide-mcp--create-diff-buffers
              :before #'ps/claude--diff-revert-advice)
  (advice-add 'claude-code-ide-mcp-handle-open-file
              :before #'ps/claude--open-revert-advice)
  (advice-add 'eat--process-output-queue
              :around #'ps/claude--eat-output-guard)
  (advice-add 'claude-code-ide--display-buffer-in-side-window
              :around #'ps/claude--adaptive-side-advice)
  (add-hook 'eat-mode-hook #'ps/claude--install-mode-line))

(provide 'ps-claude)
;;; ps-claude.el ends here
