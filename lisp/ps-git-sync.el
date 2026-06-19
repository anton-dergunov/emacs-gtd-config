;;; ps-git-sync.el --- Background git auto-sync for org files -*- lexical-binding: t; -*-

(require 'subr-x)

;;; Customization

(defcustom ps/git-sync-interval 60
  "Seconds between automatic git sync attempts."
  :type 'integer)

;;; Icons

(defvar ps/git-sync--icon-ok "✓")
(defvar ps/git-sync--icon-syncing "↻")
(defvar ps/git-sync--icon-offline "⊘")
(defvar ps/git-sync--icon-error "⚠")

;;; State

(defvar ps/git-sync--directory nil
  "Working directory (a git repo) the sync runs in. Set by `ps/git-sync-start'.")
(defvar ps/git-sync--timer nil)
(defvar ps/git-sync--running nil)

(defvar ps/git-sync-paused nil
  "When non-nil, automatic git sync is suspended.
This is a public toggle: set it to t to disable syncing (e.g. from the
command line during development with `--eval \"(setq ps/git-sync-paused t)\"').
It is also set automatically by `ps/git-sync--handle-conflict' on a merge
conflict, and cleared by `ps/git-sync-resume'.")
(defvar ps/git-sync--last-status ps/git-sync--icon-offline)
(defvar ps/git-sync--last-message "")
(defvar ps/git-sync--last-success-time nil
  "Clock time (\"HH:MM\") of the last successful sync, or nil.
Surfaced in the mode-line tooltip rather than the bar itself.")

;;; Modeline

(defun ps/git-sync--label ()
  "Return the text label for the current sync status, e.g. \"✓ Sync\"."
  (cond
   ((equal ps/git-sync--last-status ps/git-sync--icon-syncing)
    (concat ps/git-sync--icon-syncing " Syncing"))
   ((equal ps/git-sync--last-status ps/git-sync--icon-error)
    (concat ps/git-sync--icon-error " Sync Failed"))
   ((equal ps/git-sync--last-status ps/git-sync--icon-ok)
    (concat ps/git-sync--icon-ok " Sync"))
   (t
    (concat ps/git-sync--icon-offline " Sync Off"))))

(defun ps/git-sync--help-echo ()
  "Return the mode-line tooltip.
In the OK state the label already conveys success, so only the time is
shown (\"Last successful sync: HH:MM\").  Other states show their status,
plus the last successful sync time on a second line when one is known."
  (let ((sync-line (and ps/git-sync--last-success-time
                        (concat "Last successful sync: "
                                ps/git-sync--last-success-time))))
    (cond
     ((and (equal ps/git-sync--last-status ps/git-sync--icon-ok) sync-line)
      sync-line)
     (sync-line
      (concat ps/git-sync--last-message "\n" sync-line))
     (t ps/git-sync--last-message))))

(defun ps/git-sync--modeline ()
  "Return the propertized git-sync status label for the mode line.
Rendered inside the file-tree mode line (see `ps/file-tree--modeline')."
  (propertize
   (ps/git-sync--label)
   'help-echo (ps/git-sync--help-echo)
   'face
   ;; Inherit `mode-line' so the label uses the same font family/height as the
   ;; rest of the file-tree mode line (e.g. the file-set selector); only the
   ;; colour differs per status.
   (cond
    (ps/git-sync-paused
     '(:inherit mode-line :foreground "firebrick"))
    ((equal ps/git-sync--last-status ps/git-sync--icon-ok)
     '(:inherit mode-line :foreground "gray40"))
    (t
     '(:inherit mode-line :foreground "gray60")))))

;;; Git helpers

(defun ps/git-sync--root ()
  "Return the git toplevel for `ps/git-sync--directory', or nil."
  (when (and ps/git-sync--directory
             (file-directory-p ps/git-sync--directory))
    (let ((default-directory ps/git-sync--directory))
      (string-trim
       (shell-command-to-string
        "git rev-parse --show-toplevel 2>/dev/null")))))

(defun ps/git-sync--inside-repo-p ()
  "Return non-nil if `ps/git-sync--directory' is inside a git repo."
  (let ((root (ps/git-sync--root)))
    (and root
         (not (string-empty-p root)))))

(defun ps/git-sync--set-status (icon message)
  "Set the modeline ICON and help-echo MESSAGE, then refresh the mode line."
  (setq ps/git-sync--last-status icon)
  (setq ps/git-sync--last-message message)
  (force-mode-line-update t))

;;; Conflict handling

(defun ps/git-sync--handle-conflict (output)
  "Pause syncing and show git OUTPUT in a dedicated conflict buffer."
  (setq ps/git-sync-paused t)

  (ps/git-sync--set-status
   ps/git-sync--icon-error
   "Git sync paused due to conflict")

  (with-current-buffer
      (get-buffer-create "*Org Git Conflict*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert output)
      (goto-char (point-min))
      (special-mode))
    (display-buffer (current-buffer))))

;;; Main sync

(defun ps/git-sync--run ()
  "Pull, commit any changes, and push in `ps/git-sync--directory' asynchronously."
  (when (and
         (not ps/git-sync--running)
         (not ps/git-sync-paused)
         (ps/git-sync--inside-repo-p))

    (setq ps/git-sync--running t)

    (ps/git-sync--set-status
     ps/git-sync--icon-syncing
     "Git sync in progress")

    ;; Save quietly: `org-save-all-org-buffers' echoes "Saving all Org
    ;; buffers...done", which would flash in the echo area (and pile up in
    ;; *Messages*) on every 60s sync.
    (let ((inhibit-message t) (message-log-max nil))
      (org-save-all-org-buffers))

    (let* ((default-directory ps/git-sync--directory)
           (host system-name)
           (timestamp
            (format-time-string "%Y-%m-%d %H:%M:%S"))
           (commit-message
            (format "Auto backup: %s (%s)"
                    timestamp
                    host))

           (cmd
            (format
             (concat
              "git pull && "
              "git add -A && "
              "if ! git diff --cached --quiet; then "
              "git commit -m %S; "
              "fi && "
              "git push")
             commit-message))

           (buffer
            (generate-new-buffer
             " *org-git-sync*")))

      (make-process
       :name "org-git-sync"
       :buffer buffer
       :command (list "sh" "-c" cmd)
       :noquery t

       :sentinel
       (lambda (proc _event)

         (when (memq (process-status proc)
                     '(exit signal))

           (setq ps/git-sync--running nil)

           (let ((output
                  (with-current-buffer
                      (process-buffer proc)
                    (buffer-string))))

             (if (= (process-exit-status proc) 0)

                 ;; Success. The timestamp lives only in
                 ;; `ps/git-sync--last-success-time' (shown in the tooltip), so
                 ;; the status message stays time-free and never duplicates it.
                 (progn
                   (setq ps/git-sync--last-success-time
                         (format-time-string "%H:%M"))
                   (ps/git-sync--set-status
                    ps/git-sync--icon-ok
                    "Git sync OK"))

               ;; Failure
               (if (string-match-p
                    (regexp-opt
                     '("CONFLICT"
                       "Automatic merge failed"))
                    output)

                   ;; serious issue
                   (ps/git-sync--handle-conflict output)

                 ;; transient issue
                 (progn
                   (ps/git-sync--set-status
                    ps/git-sync--icon-offline
                    "Temporary git sync issue")

                   (message
                    "[org-git] temporary issue: %s"
                    (string-trim output))))))

           (kill-buffer (process-buffer proc))))))))

;;; Public API

(defun ps/git-sync-start (directory)
  "Begin background git sync in DIRECTORY (a path inside a git repo).
Starts the periodic timer.  The status indicator is rendered in the file
tree mode line (see `ps/file-tree--modeline'); it is intentionally not
injected into `global-mode-string', so it does not appear in every window."
  (setq ps/git-sync--directory directory)
  (when (ps/git-sync--inside-repo-p)
    ;; (Re)start the timer.
    (when ps/git-sync--timer
      (cancel-timer ps/git-sync--timer))
    (setq ps/git-sync--timer
          (run-with-timer 10 ps/git-sync-interval #'ps/git-sync--run))))

(defun ps/git-sync-resume ()
  "Clear the paused state after a conflict was resolved."
  (interactive)
  (setq ps/git-sync-paused nil)
  (ps/git-sync--set-status
   ps/git-sync--icon-offline
   "Git sync resumed"))

(defun ps/git-sync-toggle ()
  "Toggle automatic git sync on/off by flipping `ps/git-sync-paused'."
  (interactive)
  (setq ps/git-sync-paused (not ps/git-sync-paused))
  (ps/git-sync--set-status
   (if ps/git-sync-paused ps/git-sync--icon-offline ps/git-sync--icon-ok)
   (if ps/git-sync-paused "Git sync disabled" "Git sync enabled"))
  (message "Git sync %s" (if ps/git-sync-paused "disabled" "enabled")))

(provide 'ps-git-sync)
;;; ps-git-sync.el ends here
