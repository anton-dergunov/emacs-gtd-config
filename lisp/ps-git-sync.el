;;; ps-git-sync.el --- Background git auto-sync for org files -*- lexical-binding: t; -*-

;;; Commentary:

;; Pull/commit/push the Org directory on a timer, with a status indicator in
;; the file-tree mode line (see `ps/file-tree--modeline').
;;
;; Reporting a *failure* is the delicate part, because the sync retries every
;; `ps/git-sync-interval' seconds and a remote outage lasts minutes.  Three
;; rules keep it out of the way:
;;
;; - Git's output is stripped of progress chatter and reduced to one reason
;;   line before it is shown anywhere.  The raw output is many lines of
;;   "Counting objects:  9% (1/11)"; `message' with that text resizes the echo
;;   area to cover the frame.
;; - The echo area is used once per *distinct* failure (and once on recovery).
;;   A failure that repeats unchanged is already on screen in the mode line,
;;   so re-announcing it every tick only interrupts.
;; - A failure that will clear itself (server error, no network) is a
;;   different state from one that needs the user (conflict, auth, rejected
;;   push): `retrying' in the `warning' face vs `failed' in `error'.  Neither
;;   is the `off' state, which means the user turned sync off — collapsing
;;   those two is why a GitHub outage used to look exactly like "sync is off".
;;
;; Only a merge conflict displays a buffer, because only it needs an action
;; before syncing can continue.  Everything else leaves the details in the
;; sync log (`ps/git-sync-show-log', also mouse-1 on the indicator).

;;; Code:

(require 'subr-x)
(require 'seq)

;;; Customization

(defcustom ps/git-sync-interval 60
  "Seconds between automatic git sync attempts."
  :type 'integer)

(defcustom ps/git-sync-timeout 120
  "Seconds after which an in-progress sync is treated as hung.
A sync whose process never finishes (e.g. a `git pull' left hanging by a
laptop sleep) would otherwise keep the in-progress guard set forever and
block all future syncs.  When a sync has been running longer than this, the
watchdog kills it and lets a fresh sync start."
  :type 'integer)

;;; Icons

(defvar ps/git-sync--icon-ok "✓")
(defvar ps/git-sync--icon-syncing "↻")
(defvar ps/git-sync--icon-offline "⊘")
(defvar ps/git-sync--icon-error "⚠")

;;; States

(defvar ps/git-sync--states
  `((ok       ,ps/git-sync--icon-ok      "Sync"          nil)
    (syncing  ,ps/git-sync--icon-syncing "Syncing"       nil)
    (off      ,ps/git-sync--icon-offline "Sync Off"      nil)
    (retrying ,ps/git-sync--icon-error   "Sync Retrying" warning)
    (failed   ,ps/git-sync--icon-error   "Sync Failed"   error))
  "Sync states as (STATE ICON TEXT FACE).
FACE is nil for the states that are not a problem, so the label inherits
the contextual mode-line face and matches the rest of the mode line in both
active and inactive windows.  `retrying' and `failed' inherit the theme's
`warning' and `error' faces rather than naming a colour.")

(defun ps/git-sync--state-field (state index)
  "Return field INDEX of STATE in `ps/git-sync--states', defaulting to `off'."
  (nth index (or (assq state ps/git-sync--states)
                 (assq 'off ps/git-sync--states))))

;;; Failure classification

(defvar ps/git-sync--progress-regexp
  (concat "Enumerating objects:\\|Counting objects:\\|Compressing objects:"
          "\\|Writing objects:\\|Receiving objects:\\|Resolving deltas:"
          "\\|Delta compression using\\|^Total [0-9]")
  "Lines of git output that carry only progress, and are dropped.
Left unfiltered these are ~50 of the ~60 lines of a failed push.")

(defvar ps/git-sync--failure-patterns
  '((conflict     . "CONFLICT\\|Automatic merge failed")
    (local        . "would be overwritten by\\|Please commit your changes or stash\
\\|You have unmerged\\|needs merge")
    (auth         . "Authentication failed\\|could not read Username\
\\|Permission denied\\|403 Forbidden\\|Invalid username or password")
    (rejected     . "non-fast-forward\\|Updates were rejected\\|fetch first")
    (remote-error . "Internal Server Error\\|Bad Gateway\\|Service Unavailable\
\\|HTTP 5[0-9][0-9]\\|remote unpack failed\\|remote rejected")
    (offline      . "Could not resolve host\\|unable to access\\|Failed to connect\
\\|Connection refused\\|Network is unreachable\\|Operation timed out\
\\|Connection timed out\\|no address associated"))
  "Ordered (CLASS . REGEXP) list matched against git's output.
Order is most-specific first: a push rejected by a server error mentions
both `remote rejected' and `Internal Server Error', and a non-fast-forward
rejection must be recognised before either.")

(defvar ps/git-sync--class-phrases
  '((conflict     . "merge conflict, sync paused")
    (local        . "local changes block the pull")
    (auth         . "the remote refused the credentials")
    (rejected     . "push rejected, the remote has newer commits")
    (remote-error . "the server rejected the push")
    (offline      . "cannot reach the remote")
    (unknown      . "sync failed"))
  "How each failure class is described to the user.")

(defvar ps/git-sync--attention-classes '(conflict local auth rejected)
  "Failure classes that will not clear on their own.
Everything else is shown as `retrying': the next tick may well succeed.")

(defun ps/git-sync--strip-progress (output)
  "Return OUTPUT with git's progress chatter and blank lines removed.
Splits on carriage returns too, since git redraws progress in place."
  (string-join
   (seq-remove (lambda (line)
                 (or (string-empty-p line)
                     (string-match-p ps/git-sync--progress-regexp line)))
               (split-string (or output "") "[\n\r]" t "[ \t]+"))
   "\n"))

(defun ps/git-sync--classify (output)
  "Return the failure class of git OUTPUT, or `unknown'."
  (or (car (seq-find (lambda (entry)
                       (string-match-p (cdr entry) (or output "")))
                     ps/git-sync--failure-patterns))
      'unknown))

(defun ps/git-sync--class-severity (class)
  "Return the sync state (`failed' or `retrying') for failure CLASS."
  (if (memq class ps/git-sync--attention-classes) 'failed 'retrying))

(defun ps/git-sync--class-phrase (class)
  "Return the human description of failure CLASS."
  (or (alist-get class ps/git-sync--class-phrases)
      (alist-get 'unknown ps/git-sync--class-phrases)))

(defvar ps/git-sync--reason-preferences
  '("CONFLICT" "\\`remote: " "\\`\\(fatal\\|error\\): " "\\`! \\[")
  "Regexps picking the one line of git output worth showing, best first.")

(defun ps/git-sync--reason (output)
  "Return the single most informative line of git OUTPUT, or nil.
The leading `remote: ' / `fatal: ' / `error: ' marker is dropped — the
surrounding text already says this is a sync failure."
  (let ((lines (split-string (ps/git-sync--strip-progress output) "\n" t)))
    (when lines
      (let* ((line (or (seq-some (lambda (re)
                                   (seq-find (lambda (l) (string-match-p re l))
                                             lines))
                                 ps/git-sync--reason-preferences)
                       (car (last lines))))
             (line (replace-regexp-in-string
                    "\\`\\(remote: \\|fatal: \\|error: \\)" "" line)))
        (truncate-string-to-width (string-trim line) 120 nil nil t)))))

(defun ps/git-sync--status-message (class reason)
  "Return the one-line status text for failure CLASS with REASON."
  (concat "Git sync: " (ps/git-sync--class-phrase class)
          (if reason (format " (%s)" reason) "")))

(defvar ps/git-sync--echo-reason-width 50
  "How much of the reason the echo line shows.
Shorter than the tooltip's, so the trailing pointer at the sync log survives
the truncation `message-truncate-lines' applies on a narrow frame.")

(defun ps/git-sync--echo-line (class reason)
  "Return the single echo-area line announcing failure CLASS with REASON."
  (concat (ps/git-sync--status-message
           class
           (and reason (truncate-string-to-width
                        reason ps/git-sync--echo-reason-width nil nil t)))
          " — M-x ps/git-sync-show-log for details"))

;;; State

(defvar ps/git-sync--directory nil
  "Working directory (a git repo) the sync runs in. Set by `ps/git-sync-start'.")
(defvar ps/git-sync--timer nil)
(defvar ps/git-sync--running nil)
(defvar ps/git-sync--process nil
  "The currently running sync process, or nil.
Tracked so the watchdog can detect a hung or vanished sync (see
`ps/git-sync--reap-stale').")
(defvar ps/git-sync--start-time nil
  "Time the current sync started, or nil when idle.")

(defvar ps/git-sync-paused nil
  "When non-nil, automatic git sync is suspended.
This is a public toggle: set it to t to disable syncing (e.g. from the
command line during development with `--eval \"(setq ps/git-sync-paused t)\"').
It is also set automatically by `ps/git-sync--handle-conflict' on a merge
conflict, and cleared by `ps/git-sync-toggle'.")
(defvar ps/git-sync--state 'off
  "Current sync state, a key of `ps/git-sync--states'.")
(defvar ps/git-sync--last-message "")
(defvar ps/git-sync--last-success-time nil
  "Time value of the last successful sync, or nil.
Surfaced in the mode-line tooltip (see `ps/git-sync--format-success-time')
rather than the bar itself.")

(defvar ps/git-sync--failure-signature nil
  "Class and reason of the failure currently in effect, or nil when healthy.
A repeat of the same signature is not re-announced in the echo area.")
(defvar ps/git-sync--failure-count 0
  "Consecutive failures carrying the current signature.")
(defvar ps/git-sync--failure-since nil
  "Time the current failure signature first appeared, or nil.")

(defvar ps/git-sync--log nil
  "Recent failures, newest first: a list of (TIME CLASS REASON OUTPUT).")
(defvar ps/git-sync--log-max 20
  "How many failures `ps/git-sync--log' keeps.")

;;; Modeline

(defun ps/git-sync--label ()
  "Return the text label for the current sync state, e.g. \"✓ Sync\"."
  (concat (ps/git-sync--state-field ps/git-sync--state 1) " "
          (ps/git-sync--state-field ps/git-sync--state 2)))

(defun ps/git-sync--format-success-time ()
  "Render `ps/git-sync--last-success-time' for the tooltip, or nil.
Shows just the clock time (\"HH:MM\") when the last sync was today, and the
full date too (\"YYYY-MM-DD HH:MM\") when it was yesterday or earlier."
  (when ps/git-sync--last-success-time
    (let ((today (string= (format-time-string "%F")
                          (format-time-string "%F" ps/git-sync--last-success-time))))
      (format-time-string (if today "%H:%M" "%Y-%m-%d %H:%M")
                          ps/git-sync--last-success-time))))

(defun ps/git-sync--attempts-line ()
  "Return \"Failing since HH:MM (N attempts)\", or nil when not failing."
  (when (and ps/git-sync--failure-since (> ps/git-sync--failure-count 0))
    (format "Failing since %s (%d attempt%s)"
            (format-time-string "%H:%M" ps/git-sync--failure-since)
            ps/git-sync--failure-count
            (if (= ps/git-sync--failure-count 1) "" "s"))))

(defun ps/git-sync--help-echo ()
  "Return the mode-line tooltip.
In the OK state the label already conveys success, so the status message is
dropped and only the time is shown.  While failing, the reason and how long
it has been failing come first, since that is what the red label raises."
  (let* ((time (ps/git-sync--format-success-time))
         (sync-line (and time (concat "Last successful sync: " time))))
    (string-join
     (delq nil
           (list (unless (and (eq ps/git-sync--state 'ok) sync-line)
                   ps/git-sync--last-message)
                 (and (memq ps/git-sync--state '(retrying failed))
                      (ps/git-sync--attempts-line))
                 sync-line
                 "mouse-1: show sync log"))
     "\n")))

(defvar ps/git-sync--modeline-map
  (let ((map (make-sparse-keymap)))
    ;; mouse-1 only: mouse-2 and mouse-3 are disabled across the mode line by
    ;; `ps/mode-line--disable-destructive-mouse'.
    (define-key map [mode-line mouse-1] #'ps/git-sync-show-log)
    map)
  "Keymap on the git-sync mode-line indicator.")

(defun ps/git-sync--modeline ()
  "Return the propertized git-sync status label for the mode line.
Rendered inside the file-tree mode line (see `ps/file-tree--modeline')."
  (let ((face (ps/git-sync--state-field ps/git-sync--state 3)))
    (apply #'propertize (ps/git-sync--label)
           'help-echo (ps/git-sync--help-echo)
           'mouse-face 'mode-line-highlight
           'local-map ps/git-sync--modeline-map
           (when face (list 'face face)))))

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

(defun ps/git-sync--set-status (state message)
  "Set the modeline STATE and help-echo MESSAGE, then refresh the mode line."
  (setq ps/git-sync--state state)
  (setq ps/git-sync--last-message message)
  (force-mode-line-update t))

(defun ps/git-sync--message (text)
  "Show TEXT in the echo area, forced onto a single line.
`message-truncate-lines' is the backstop that keeps the echo area one line
tall even if TEXT still turns out longer than the frame."
  (let ((message-truncate-lines t))
    (message "%s" (replace-regexp-in-string "[\n\r]+" " " text))))

;;; Sync log

(defun ps/git-sync--log-record (class reason output)
  "Prepend a failure (CLASS, REASON, OUTPUT) to `ps/git-sync--log'."
  (push (list (current-time) class reason output) ps/git-sync--log)
  (when (> (length ps/git-sync--log) ps/git-sync--log-max)
    (setq ps/git-sync--log (seq-take ps/git-sync--log ps/git-sync--log-max))))

(defun ps/git-sync--log-render (log &optional conflict)
  "Return the text of the sync log for LOG, newest first.
With CONFLICT non-nil, lead with what to do about the merge conflict that
paused syncing."
  (concat
   "Git sync log\n\n"
   (when conflict
     (concat "Sync is paused because of a merge conflict.\n"
             "Resolve it (e.g. with Magit, C-x g), then re-enable sync from\n"
             "Productivity → Sync & Version Control → Git Sync Enabled.\n\n"))
   (if (null log)
       "No sync failures recorded.\n"
     (mapconcat
      (lambda (entry)
        (seq-let (time class reason output) entry
          (format "[%s] %s\n%s\n"
                  (format-time-string "%Y-%m-%d %H:%M:%S" time)
                  (ps/git-sync--status-message class reason)
                  output)))
      log "\n"))))

(defun ps/git-sync--log-buffer ()
  "Return the sync log buffer, refreshed from `ps/git-sync--log'."
  (with-current-buffer (get-buffer-create "*Org Git Sync*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (ps/git-sync--log-render
               ps/git-sync--log
               (and ps/git-sync-paused (eq ps/git-sync--state 'failed))))
      (goto-char (point-min))
      (special-mode))
    (current-buffer)))

(defun ps/git-sync-show-log ()
  "Show recent git sync failures with their full git output."
  (interactive)
  (display-buffer (ps/git-sync--log-buffer)))

;;; Failure and success bookkeeping

(defun ps/git-sync--note-failure (output)
  "Record a failed sync described by git OUTPUT and return its class.
Announces the failure in the echo area only when it differs from the one
already in effect, so an outage lasting an hour interrupts once."
  (let* ((clean (ps/git-sync--strip-progress output))
         (class (ps/git-sync--classify clean))
         (reason (ps/git-sync--reason clean))
         (signature (cons class reason))
         (new (not (equal signature ps/git-sync--failure-signature))))
    (if new
        (setq ps/git-sync--failure-signature signature
              ps/git-sync--failure-count 1
              ps/git-sync--failure-since (current-time))
      (setq ps/git-sync--failure-count (1+ ps/git-sync--failure-count)))
    (ps/git-sync--log-record class reason clean)
    (ps/git-sync--set-status (ps/git-sync--class-severity class)
                             (ps/git-sync--status-message class reason))
    (when new
      (ps/git-sync--message (ps/git-sync--echo-line class reason)))
    class))

(defun ps/git-sync--note-success ()
  "Record a successful sync, announcing recovery if we had been failing."
  (let ((was-failing ps/git-sync--failure-signature)
        (count ps/git-sync--failure-count)
        (since ps/git-sync--failure-since))
    (setq ps/git-sync--last-success-time (current-time)
          ps/git-sync--failure-signature nil
          ps/git-sync--failure-count 0
          ps/git-sync--failure-since nil)
    ;; The timestamp lives only in `ps/git-sync--last-success-time' (shown in
    ;; the tooltip), so the status message stays time-free and never
    ;; duplicates it.
    (ps/git-sync--set-status 'ok "Git sync OK")
    (when was-failing
      (ps/git-sync--message
       (format "Git sync recovered (%d failed attempt%s since %s)"
               count (if (= count 1) "" "s")
               (format-time-string "%H:%M" since))))))

;;; Conflict handling

(defun ps/git-sync--handle-conflict ()
  "Pause syncing and show the sync log, which explains how to resume.
The only failure that displays a buffer: it cannot clear on its own, and
syncing stays paused until the user resolves it."
  (setq ps/git-sync-paused t)
  (ps/git-sync--set-status 'failed "Git sync paused due to a merge conflict")
  (display-buffer (ps/git-sync--log-buffer)))

;;; Watchdog

(defun ps/git-sync--reap-stale ()
  "Clear a stuck in-progress sync so a fresh one can start.
`ps/git-sync--running' is only cleared by the process sentinel; if that
process is interrupted (laptop sleep) the sentinel may never fire, leaving
the guard set forever.  Reset the guard when the tracked process has died
without the sentinel running, or kill it when it has hung past
`ps/git-sync-timeout'.  Returns non-nil when a stale sync was reaped."
  (when ps/git-sync--running
    (let ((proc ps/git-sync--process))
      (cond
       ;; Process gone or already dead — the sentinel never cleared the guard.
       ((not (process-live-p proc))
        (setq ps/git-sync--running nil
              ps/git-sync--process nil
              ps/git-sync--start-time nil)
        t)
       ;; Still alive but running too long — kill it (the sentinel fires on the
       ;; kill and clears the guard/process/start-time).
       ((and ps/git-sync--start-time
             (> (float-time (time-subtract nil ps/git-sync--start-time))
                ps/git-sync-timeout))
        (ignore-errors (delete-process proc))
        (setq ps/git-sync--running nil
              ps/git-sync--process nil
              ps/git-sync--start-time nil)
        t)
       (t nil)))))

;;; Main sync

(defun ps/git-sync--run ()
  "Pull, commit any changes, and push in `ps/git-sync--directory' asynchronously."
  (ps/git-sync--reap-stale)
  (when (and
         (not ps/git-sync--running)
         (not ps/git-sync-paused)
         (ps/git-sync--inside-repo-p))

    (setq ps/git-sync--running t
          ps/git-sync--start-time (current-time))

    (ps/git-sync--set-status 'syncing "Git sync in progress")

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

      (setq ps/git-sync--process
       (make-process
       :name "org-git-sync"
       :buffer buffer
       :command (list "sh" "-c" cmd)
       :noquery t

       :sentinel
       (lambda (proc _event)

         (when (memq (process-status proc)
                     '(exit signal))

           (setq ps/git-sync--running nil
                 ps/git-sync--process nil
                 ps/git-sync--start-time nil)

           (let ((output
                  (with-current-buffer
                      (process-buffer proc)
                    (buffer-string))))

             (if (= (process-exit-status proc) 0)

                 (progn
                   (ps/git-sync--note-success)
                   ;; "Already up to date." is git's exact wording when the pull
                   ;; brought in nothing; its absence means files may have
                   ;; arrived from another device, so the tree needs a look.
                   ;; `ps/file-tree-refresh' itself is quiet when the tree isn't
                   ;; even open (`treemacs-get-local-buffer' returns nil then).
                   (when (and (fboundp 'ps/file-tree-refresh)
                              (not (string-match-p "Already up to date" output)))
                     (ps/file-tree-refresh)))

               (when (eq (ps/git-sync--note-failure output) 'conflict)
                 (ps/git-sync--handle-conflict))))

           (kill-buffer (process-buffer proc)))))))))

;;; Public API

(defun ps/git-sync--ensure-timer ()
  "Make sure the periodic sync timer is scheduled, (re)creating it if not.
Re-enabling sync after the timer was somehow lost would otherwise require an
Emacs restart."
  (unless (memq ps/git-sync--timer timer-list)
    (when ps/git-sync--timer
      (cancel-timer ps/git-sync--timer))
    (setq ps/git-sync--timer
          (run-with-timer 10 ps/git-sync-interval #'ps/git-sync--run))))

(defun ps/git-sync-start (directory)
  "Begin background git sync in DIRECTORY (a path inside a git repo).
Starts the periodic timer.  The status indicator is rendered in the file
tree mode line (see `ps/file-tree--modeline'); it is intentionally not
injected into `global-mode-string', so it does not appear in every window."
  (setq ps/git-sync--directory directory)
  (when (ps/git-sync--inside-repo-p)
    (ps/git-sync--ensure-timer)))

(defun ps/git-sync-toggle ()
  "Toggle automatic git sync on/off by flipping `ps/git-sync-paused'.
Re-enabling fully recovers: it clears any stuck in-progress state, makes
sure the timer is alive, and syncs immediately rather than waiting for the
next tick.  This is also the way to resume after resolving a merge conflict."
  (interactive)
  (setq ps/git-sync-paused (not ps/git-sync-paused))
  (if ps/git-sync-paused
      (ps/git-sync--set-status 'off "Git sync disabled")
    ;; Resuming is a fresh start: whatever failure paused us is no longer in
    ;; effect, so the next failure counts as new and is announced.
    (setq ps/git-sync--failure-signature nil
          ps/git-sync--failure-count 0
          ps/git-sync--failure-since nil)
    (ps/git-sync--reap-stale)
    (ps/git-sync--ensure-timer)
    (ps/git-sync--set-status 'ok "Git sync enabled")
    (ps/git-sync--run))
  (message "Git sync %s" (if ps/git-sync-paused "disabled" "enabled")))

(provide 'ps-git-sync)
;;; ps-git-sync.el ends here
