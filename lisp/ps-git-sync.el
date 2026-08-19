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
;; Only the failures in `ps/git-sync--pausing-classes' display a buffer,
;; because only they need an action before syncing can continue.  Everything
;; else leaves the details in the sync log (`ps/git-sync-show-log', also
;; mouse-1 on the indicator).
;;
;; The sync also reports what it finds *beside* git: a vault kept in a cloud
;; folder as well as in git collects "conflicted copy" files whenever two
;; machines write the same file, and those are invisible until something
;; trips over them.  See `ps/git-sync--cloud-copies' and the `copies' state.

;;; Code:

(require 'subr-x)
(require 'seq)
(require 'ps-vault)
(require 'ps-org-save)

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

(defcustom ps/git-sync-cloud-copy-regexp "conflicted copy"
  "Regexp matching the file names a cloud syncer leaves behind on a collision.
Dropbox names them \"Notes (Your Name's conflicted copy 2026-08-19).org\";
the account name and the date vary, so only the fixed middle is matched.
Files whose name matches are reported by `ps/git-sync--cloud-copies' after
each sync.  Set to nil to stop looking."
  :type '(choice (const :tag "Do not look" nil) regexp))

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
    (copies   ,ps/git-sync--icon-error   "Conflicted Copies" warning)
    (retrying ,ps/git-sync--icon-error   "Sync Retrying" warning)
    (failed   ,ps/git-sync--icon-error   "Sync Failed"   error))
  "Sync states as (STATE ICON TEXT FACE).
FACE is nil for the states that are not a problem, so the label inherits
the contextual mode-line face and matches the rest of the mode line in both
active and inactive windows.  `retrying' and `failed' inherit the theme's
`warning' and `error' faces rather than naming a colour.

`copies' is the odd one: the sync itself succeeded, and what needs the user
is in the working tree rather than in git.  It is `warning' rather than
`error' for exactly that reason -- nothing is broken, but two versions of a
file are sitting side by side waiting to be merged.")

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
  '((cloud-copy   . "conflicted copy\\|bad object refs/")
    (conflict     . "CONFLICT\\|Automatic merge failed")
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
rejection must be recognised before either.  `cloud-copy' leads because a
repository damaged by a cloud syncer usually fails at the push, and so
reports a rejection as well.

These are matched case-*sensitively* (see `ps/git-sync--classify'), which
`conflict' depends on: git writes `CONFLICT' in capitals, while Dropbox
names its files \"conflicted copy\" -- folding case makes every Dropbox
casualty look like a merge conflict and sends the user to Magit to resolve
a merge that never happened.")

(defvar ps/git-sync--class-phrases
  '((cloud-copy   . "conflicted copies inside the repository, sync paused")
    (conflict     . "merge conflict, sync paused")
    (local        . "local changes block the pull")
    (auth         . "the remote refused the credentials")
    (rejected     . "push rejected, the remote has newer commits")
    (remote-error . "the server rejected the push")
    (offline      . "cannot reach the remote")
    (unknown      . "sync failed"))
  "How each failure class is described to the user.")

(defvar ps/git-sync--attention-classes '(cloud-copy conflict local auth rejected)
  "Failure classes that will not clear on their own.
Everything else is shown as `retrying': the next tick may well succeed.")

(defvar ps/git-sync--pausing-classes '(conflict cloud-copy)
  "Failure classes that stop the sync until the user has acted.
The rest of `ps/git-sync--attention-classes' still need attention, but
retrying them costs nothing -- credentials may be fixed in the keychain, and
a rejected push clears on the next successful pull.  These two do not: git
refuses to do anything further until the working tree or the repository
itself is repaired, so a timer firing every minute only fills the log.")

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
  "Return the failure class of git OUTPUT, or `unknown'.
Matching is case-sensitive -- see `ps/git-sync--failure-patterns' for why
that is load-bearing."
  (let ((case-fold-search nil))
    (or (car (seq-find (lambda (entry)
                         (string-match-p (cdr entry) (or output "")))
                       ps/git-sync--failure-patterns))
        'unknown)))

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

;;; Conflicted copies left in the working tree

(defvar ps/git-sync--repo-scan-directories
  '(".git" ".git/refs/heads" ".git/logs/refs/heads")
  "Directories inside `.git' checked for conflicted copies.
The recursive walk deliberately skips dotted directories, so `.git' is not
part of it: an object store is tens of megabytes and walking it once a
minute would cost far more than the answer is worth.  These three are
checked non-recursively instead, because they are small and because a copy
landing there is fatal rather than untidy -- git enumerates every file under
`refs/heads' as a branch, so a copy there becomes a branch literally named
`main (Your Name's conflicted copy ...)' and every git command afterwards
fails with `bad object'.

A repository whose `.git' is outside the cloud folder (the fix this check
exists to point at) can never collide here, and the scan simply finds
nothing.")

(defun ps/git-sync--cloud-copy-name-p (name)
  "Return non-nil if NAME is a file a cloud syncer left after a collision."
  (and ps/git-sync-cloud-copy-regexp
       (string-match-p ps/git-sync-cloud-copy-regexp name)))

(defun ps/git-sync--cloud-copies-below (directory)
  "Return conflicted copies under DIRECTORY, recursively, as absolute paths.
Dotted directories are not descended into.  A matching *directory* is
reported and not descended into either -- everything below it is a copy of
something already reported one level up."
  (let (found)
    (dolist (entry (directory-files directory t directory-files-no-dot-files-regexp))
      (let ((name (file-name-nondirectory entry)))
        (cond
         ((ps/git-sync--cloud-copy-name-p name) (push entry found))
         ((and (file-directory-p entry) (not (string-prefix-p "." name)))
          (setq found (nconc (ps/git-sync--cloud-copies-below entry) found))))))
    found))

(defun ps/git-sync--cloud-copies-in-repo (directory)
  "Return conflicted copies in DIRECTORY's `.git', as absolute paths.
Only the directories named by `ps/git-sync--repo-scan-directories', and only
their own entries."
  (let (found)
    (dolist (sub ps/git-sync--repo-scan-directories)
      (let ((dir (expand-file-name sub directory)))
        (when (file-directory-p dir)
          (dolist (entry (directory-files dir t directory-files-no-dot-files-regexp))
            (when (ps/git-sync--cloud-copy-name-p (file-name-nondirectory entry))
              (push entry found))))))
    found))

(defun ps/git-sync--cloud-copies (directory)
  "Return conflicted-copy files in the repository at DIRECTORY, sorted.
Paths are relative to DIRECTORY, which reads better in the sync log than the
absolute ones.  Returns nil when DIRECTORY is missing or the check is turned
off with `ps/git-sync-cloud-copy-regexp'."
  (when (and ps/git-sync-cloud-copy-regexp
             directory
             (file-directory-p directory))
    (sort (mapcar (lambda (file) (file-relative-name file directory))
                  (append (ps/git-sync--cloud-copies-below directory)
                          (ps/git-sync--cloud-copies-in-repo directory)))
          #'string<)))

;;; State

(defvar ps/git-sync--directory nil
  "Working directory (a git repo) the sync runs in. Set by `ps/git-sync-start'.")
(defvar ps/git-sync--interval nil
  "Interval this vault syncs at, overriding `ps/git-sync-interval', or nil.
Set by `ps/git-sync-start'; a vault names it in its workspace.org or state
file (see `ps/vault-git-sync').")
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
It is also set automatically by `ps/git-sync--handle-pause' for the classes
in `ps/git-sync--pausing-classes', and cleared by `ps/git-sync-toggle'.")
(defvar ps/git-sync--paused-class nil
  "Failure class that paused the sync, or nil.
Selects which remedy `ps/git-sync--log-render' prints -- resolving a merge
and deleting a cloud syncer's conflicted copies are different jobs.")
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

(defvar ps/git-sync--copies nil
  "Conflicted-copy files found by the last completed sync, relative paths.")

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
  "Return non-nil if `ps/git-sync--directory' is itself a git working tree.
Note \"itself\": syncing must not climb to an enclosing repository the way
`ps/git-sync--root' does, or a vault kept inside a larger checkout would have
that outer repository committed and pushed on its behalf.  Being a repo is
also what *enables* sync at all -- see `ps/git-sync-maybe-start'."
  (ps/vault-git-repo-p ps/git-sync--directory))

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

(defvar ps/git-sync--resume-line
  "Then re-enable sync from Productivity → Sync & Version Control → Git Sync Enabled.\n"
  "How every pause remedy ends: the one way back to syncing.")

(defvar ps/git-sync--pause-remedies
  `((conflict
     . ,(concat "Sync is paused because of a merge conflict.\n"
                "Resolve it (e.g. with Magit, C-x g).\n"))
    (cloud-copy
     . ,(concat "Sync is paused: a cloud syncer (Dropbox) has left \"conflicted\n"
                "copy\" files inside the repository itself, which Git cannot read.\n"
                "Delete them — they are copies, not your work:\n\n"
                "    find . -name \"*conflicted copy*\" -print -delete\n\n"
                "This happens when the cloud folder syncs .git as well.  For the\n"
                "permanent fix see docs/Dropbox-and-git.org.\n")))
  "What to do about each class in `ps/git-sync--pausing-classes'.")

(defun ps/git-sync--copies-render (copies)
  "Return the sync log's block listing conflicted-copy files COPIES, or nil."
  (when copies
    (concat "A cloud syncer has left conflicted copies in your vault:\n\n"
            (mapconcat (lambda (file) (format "    %s\n" file)) copies "")
            "\nMerge anything you need out of them, then delete them.  They are\n"
            "ignored by Git and by the agenda, so nothing else will pick them up.\n\n")))

(defun ps/git-sync--log-render (log &optional paused-class copies)
  "Return the text of the sync log for LOG, newest first.
With PAUSED-CLASS non-nil, lead with what to do about the failure of that
class that paused syncing.  COPIES, when non-nil, are conflicted-copy files
found in the working tree and are listed too."
  (concat
   "Git sync log\n\n"
   (when paused-class
     (concat (or (alist-get paused-class ps/git-sync--pause-remedies)
                 "Sync is paused.\n")
             ps/git-sync--resume-line "\n"))
   (ps/git-sync--copies-render copies)
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
               (and ps/git-sync-paused (eq ps/git-sync--state 'failed)
                    ps/git-sync--paused-class)
               ps/git-sync--copies))
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

(defun ps/git-sync--copies-message (copies)
  "Return the one echo-area line announcing conflicted-copy files COPIES."
  (format "Git sync: %d conflicted cop%s left by a cloud syncer (%s) \
— M-x ps/git-sync-show-log for the list"
          (length copies)
          (if (= (length copies) 1) "y" "ies")
          (file-name-nondirectory (car copies))))

(defun ps/git-sync--note-copies ()
  "Look for conflicted copies in the vault and report them.
Called after a *successful* sync, so the state it overrides is `ok': git did
its job, but two versions of a file are sitting side by side waiting to be
merged.  A failing sync keeps its own state — the more urgent of the two.
Announced in the echo area once per distinct set, the way a failure is."
  (let ((copies (ps/git-sync--cloud-copies ps/git-sync--directory))
        (previous ps/git-sync--copies))
    (setq ps/git-sync--copies copies)
    (when copies
      (ps/git-sync--set-status 'copies (ps/git-sync--copies-message copies))
      (unless (equal copies previous)
        (ps/git-sync--message (ps/git-sync--copies-message copies))))))

;;; Conflict handling

(defun ps/git-sync--handle-pause (class)
  "Pause syncing after failure CLASS and show the sync log.
The log says how to resume.  These are the only failures that display a
buffer: they cannot clear on their own, and syncing stays paused until the
user has acted.  See `ps/git-sync--pausing-classes'."
  (setq ps/git-sync-paused t
        ps/git-sync--paused-class class)
  (ps/git-sync--set-status
   'failed (format "Git sync paused: %s" (ps/git-sync--class-phrase class)))
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

    ;; Quiet, and it refuses to write a buffer whose file changed on disk
    ;; behind it -- committing what is in Emacs must never be what destroys
    ;; the copy Dropbox just brought down.  See `ps-org-save.el'.
    (ps/org-save-all-org-buffers-quietly)

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
                     (ps/file-tree-refresh))
                   ;; After the sync, not before: the pull is exactly what
                   ;; brings the other machine's copy of a file down, and the
                   ;; walk is off the critical path here.
                   (ps/git-sync--note-copies))

               (let ((class (ps/git-sync--note-failure output)))
                 (when (memq class ps/git-sync--pausing-classes)
                   (ps/git-sync--handle-pause class)))))

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
          (run-with-timer 10 (or ps/git-sync--interval ps/git-sync-interval)
                          #'ps/git-sync--run))))

(defun ps/git-sync-start (directory &optional interval)
  "Begin background git sync in DIRECTORY, a git working tree.
Starts the periodic timer, every INTERVAL seconds when given and
`ps/git-sync-interval' otherwise.  The status indicator is rendered in the
file tree mode line (see `ps/file-tree--modeline'); it is intentionally not
injected into `global-mode-string', so it does not appear in every window."
  (setq ps/git-sync--directory directory
        ps/git-sync--interval interval)
  (when (ps/git-sync--inside-repo-p)
    (ps/git-sync--ensure-timer)
    ;; State the indicator honestly straight away.  The first tick is ten
    ;; seconds out, and until it lands the status is whatever the previous
    ;; vault left behind -- after a switch that reads "Git sync stopped" for a
    ;; vault that is, in fact, syncing.
    (ps/git-sync--set-status 'ok "Git sync enabled")))

(defun ps/git-sync-stop ()
  "Stop syncing and clear every trace of the outgoing repository.
The teardown half of a vault switch.  Cancelling the timer is the easy part:
the failure signature, attempt count and log all describe a *particular* repo,
and carrying them into the next vault would show its sync as broken for
reasons that have nothing to do with it."
  (when (timerp ps/git-sync--timer)
    (cancel-timer ps/git-sync--timer))
  (setq ps/git-sync--timer nil)
  ;; Kills or disowns a sync still running against the old repo.
  (ps/git-sync--reap-stale)
  (setq ps/git-sync--directory nil
        ps/git-sync--interval nil
        ps/git-sync--running nil
        ps/git-sync--process nil
        ps/git-sync--start-time nil
        ps/git-sync-paused nil
        ps/git-sync--last-success-time nil
        ps/git-sync--failure-signature nil
        ps/git-sync--failure-count 0
        ps/git-sync--failure-since nil
        ps/git-sync--paused-class nil
        ps/git-sync--copies nil
        ps/git-sync--log nil)
  (ps/git-sync--set-status 'off "Git sync stopped"))

(defun ps/git-sync-maybe-start (directory)
  "Start syncing DIRECTORY if it should, and say why in the mode line if not.
Whether a vault syncs is *detected*, not configured: it syncs when it is
itself a git working tree, so `git init' in a vault (and adding a remote) is
what turns this on.  `ps/vault-git-sync' can override that per vault."
  (let ((interval (and directory (ps/vault-git-sync-interval directory))))
    (cond
     ((getenv "PS_GIT_SYNC_DISABLE")
      (ps/git-sync--set-status 'off "Git sync disabled by PS_GIT_SYNC_DISABLE"))
     ((null directory)
      (ps/git-sync--set-status 'off "No vault is open"))
     (interval
      (ps/git-sync-start directory interval))
     ((not (ps/vault-git-repo-p directory))
      (ps/git-sync--set-status
       'off "Not a git repository — run `git init' in this vault to sync it"))
     (t
      (ps/git-sync--set-status 'off "Git sync turned off for this vault")))))

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
          ps/git-sync--failure-since nil
          ps/git-sync--paused-class nil)
    (ps/git-sync--reap-stale)
    (ps/git-sync--ensure-timer)
    (ps/git-sync--set-status 'ok "Git sync enabled")
    (ps/git-sync--run))
  (message "Git sync %s" (if ps/git-sync-paused "disabled" "enabled")))

(provide 'ps-git-sync)
;;; ps-git-sync.el ends here
