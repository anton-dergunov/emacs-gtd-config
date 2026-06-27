;;; ps-claude.el --- Claude Code IDE window/resize tweaks -*- lexical-binding: t; -*-

;;; Commentary:
;; Three small fixes for `claude-code-ide.el' (eat backend):
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
;; If upstream #1422 is fixed and the reflow workaround is no longer needed,
;; the reflow-glitch suppression itself can be disabled with:
;;   (setq claude-code-ide-prevent-reflow-glitch nil)
;; The resync hook below is harmless either way and can stay.

;;; Code:

(require 'seq)

(declare-function claude-code-ide--sync-terminal-dimensions "claude-code-ide")
(declare-function claude-code-ide--get-working-directory "claude-code-ide")
(defvar claude-code-ide-window-width)
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

(defun ps/claude--on-window-size-change (_frame)
  "Schedule a debounced terminal resync after a window-size change.
Added to `window-size-change-functions'; only schedules work when a Claude
Code session buffer is currently displayed."
  (when (seq-some (lambda (w) (ps/claude--session-buffer-p (window-buffer w)))
                   (window-list))
    (when (timerp ps/claude--resize-timer)
      (cancel-timer ps/claude--resize-timer))
    (setq ps/claude--resize-timer
          (run-with-idle-timer ps/claude-resize-debounce-delay nil
                                #'ps/claude--resync-windows))))

(defun ps/claude--working-directory ()
  "Always use `my-org-base-directory' as the Claude Code IDE working directory.
Overrides `claude-code-ide--get-working-directory', whose default
(current project root) would resolve to this config's own source tree
for any buffer in this repo."
  (expand-file-name my-org-base-directory))

(defun ps/claude-setup ()
  "Apply Claude Code IDE window-size and working-directory tweaks.
Sets `claude-code-ide-window-width' from `ps/claude-window-width',
installs the debounced resize-resync hook, and pins the working directory
to `my-org-base-directory'.  Idempotent."
  (setq claude-code-ide-window-width ps/claude-window-width)
  (add-hook 'window-size-change-functions #'ps/claude--on-window-size-change)
  (advice-add 'claude-code-ide--get-working-directory
              :override #'ps/claude--working-directory))

(provide 'ps-claude)
;;; ps-claude.el ends here
