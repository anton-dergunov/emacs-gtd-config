;;; test-ps-git-sync.el --- ERT tests for ps-git-sync -*- lexical-binding: t; -*-

(require 'ert)
(add-to-list 'load-path "lisp")
(require 'ps-git-sync)

;;; Test helpers

(defmacro ps/git-sync-test--with-repo (&rest body)
  "Run BODY with `ps/git-sync--directory' pointing at a fresh temp git repo.
Skipped when git is unavailable. Cleans up the repo afterward."
  (declare (indent 0))
  `(progn
     (skip-unless (executable-find "git"))
     (let ((dir (make-temp-file "ps-git-sync-test-" t))
           (ps/git-sync--directory nil))
       (unwind-protect
           (let ((default-directory dir))
             (call-process "git" nil nil nil "init")
             (setq ps/git-sync--directory dir)
             ,@body)
         (delete-directory dir t)))))

(defmacro ps/git-sync-test--with-non-repo (&rest body)
  "Run BODY with `ps/git-sync--directory' pointing at a temp dir that is NOT a repo."
  (declare (indent 0))
  `(let ((dir (make-temp-file "ps-git-sync-nonrepo-" t))
         (ps/git-sync--directory nil))
     (unwind-protect
         (progn
           (setq ps/git-sync--directory dir)
           ,@body)
       (delete-directory dir t))))

;;; -------------------------------------------------------
;;; Icons / defaults
;;; -------------------------------------------------------

(ert-deftest ps/git-sync--interval-default ()
  "The sync interval defcustom has the documented default."
  (should (= ps/git-sync-interval 60)))

(ert-deftest ps/git-sync--icons-defined ()
  "All four status icons are non-empty strings."
  (dolist (icon (list ps/git-sync--icon-ok
                      ps/git-sync--icon-syncing
                      ps/git-sync--icon-offline
                      ps/git-sync--icon-error))
    (should (stringp icon))
    (should (> (length icon) 0))))

;;; -------------------------------------------------------
;;; set-status / modeline
;;; -------------------------------------------------------

(ert-deftest ps/git-sync--set-status-updates-vars ()
  "set-status updates both the status icon and the help message."
  (let ((ps/git-sync--last-status nil)
        (ps/git-sync--last-message nil))
    (ps/git-sync--set-status ps/git-sync--icon-ok "all good")
    (should (equal ps/git-sync--last-status ps/git-sync--icon-ok))
    (should (equal ps/git-sync--last-message "all good"))))

(ert-deftest ps/git-sync--modeline-contains-icon ()
  "The modeline string contains the current status icon."
  (let ((ps/git-sync--last-status ps/git-sync--icon-ok)
        (ps/git-sync--last-message "msg")
        (ps/git-sync-paused nil))
    (should (string-match-p (regexp-quote ps/git-sync--icon-ok)
                            (ps/git-sync--modeline)))))

(ert-deftest ps/git-sync--label-text-per-status ()
  "Each status maps to its documented text label."
  (let ((ps/git-sync--last-status ps/git-sync--icon-ok))
    (should (equal (ps/git-sync--label) (concat ps/git-sync--icon-ok " Sync"))))
  (let ((ps/git-sync--last-status ps/git-sync--icon-syncing))
    (should (equal (ps/git-sync--label) (concat ps/git-sync--icon-syncing " Syncing"))))
  (let ((ps/git-sync--last-status ps/git-sync--icon-error))
    (should (equal (ps/git-sync--label) (concat ps/git-sync--icon-error " Sync Failed"))))
  (let ((ps/git-sync--last-status ps/git-sync--icon-offline))
    (should (equal (ps/git-sync--label) (concat ps/git-sync--icon-offline " Sync Off")))))

(ert-deftest ps/git-sync--help-echo-appends-success-time ()
  "The tooltip appends the last successful sync time when known."
  ;; Non-OK status: status message on one line, time on the next.
  (let ((ps/git-sync--last-status ps/git-sync--icon-syncing)
        (ps/git-sync--last-message "Git sync in progress")
        (ps/git-sync--last-success-time (current-time)))
    (should (string-match-p
             (concat "last successful sync: "
                     (regexp-quote (ps/git-sync--format-success-time)))
             (ps/git-sync--help-echo))))
  (let ((ps/git-sync--last-status ps/git-sync--icon-offline)
        (ps/git-sync--last-message "msg")
        (ps/git-sync--last-success-time nil))
    (should (equal (ps/git-sync--help-echo) "msg"))))

(ert-deftest ps/git-sync--help-echo-ok-shows-only-time ()
  "In the OK state the tooltip shows the time once, not a duplicated message."
  (let ((ps/git-sync--last-status ps/git-sync--icon-ok)
        (ps/git-sync--last-message "Git sync OK")
        (ps/git-sync--last-success-time (current-time)))
    (should (equal (ps/git-sync--help-echo)
                   (concat "last successful sync: "
                           (ps/git-sync--format-success-time))))))

(ert-deftest ps/git-sync--format-success-time-today ()
  "A timestamp from today renders as just the clock time."
  (let ((ps/git-sync--last-success-time (current-time)))
    (should (equal (ps/git-sync--format-success-time)
                   (format-time-string "%H:%M")))))

(ert-deftest ps/git-sync--format-success-time-earlier ()
  "A timestamp from a previous day includes the full date."
  (let ((ps/git-sync--last-success-time
         (time-subtract (current-time) (* 2 86400))))
    (should (equal (ps/git-sync--format-success-time)
                   (format-time-string "%Y-%m-%d %H:%M"
                                       ps/git-sync--last-success-time)))))

(ert-deftest ps/git-sync--format-success-time-nil ()
  "No recorded time yields nil."
  (let ((ps/git-sync--last-success-time nil))
    (should (null (ps/git-sync--format-success-time)))))

(ert-deftest ps/git-sync--modeline-help-echo ()
  "The modeline string carries the last message as a help-echo property."
  (let ((ps/git-sync--last-status ps/git-sync--icon-ok)
        (ps/git-sync--last-message "hello there")
        (ps/git-sync-paused nil))
    (let ((s (ps/git-sync--modeline)))
      (should (equal (get-text-property (1- (length s)) 'help-echo s)
                     "hello there")))))

(ert-deftest ps/git-sync--modeline-error-face ()
  "An error status uses the firebrick (red) face."
  (let ((ps/git-sync--last-status ps/git-sync--icon-error)
        (ps/git-sync--last-message "failed")
        (ps/git-sync-paused t))
    (let* ((s (ps/git-sync--modeline))
           (face (get-text-property (1- (length s)) 'face s)))
      (should (equal face '(:foreground "firebrick"))))))

(ert-deftest ps/git-sync--modeline-ok-face ()
  "A non-error status carries no face, so it inherits the mode-line colour."
  (let ((ps/git-sync--last-status ps/git-sync--icon-ok)
        (ps/git-sync--last-message "ok")
        (ps/git-sync-paused nil))
    (let* ((s (ps/git-sync--modeline))
           (face (get-text-property (1- (length s)) 'face s)))
      (should (null face)))))

(ert-deftest ps/git-sync--modeline-off-face ()
  "The off/paused (offline) status also inherits the mode-line colour."
  (let ((ps/git-sync--last-status ps/git-sync--icon-offline)
        (ps/git-sync--last-message "off")
        (ps/git-sync-paused t))
    (let* ((s (ps/git-sync--modeline))
           (face (get-text-property (1- (length s)) 'face s)))
      (should (null face)))))

;;; -------------------------------------------------------
;;; conflict handling
;;; -------------------------------------------------------

(ert-deftest ps/git-sync--handle-conflict-pauses-and-shows ()
  "handle-conflict pauses sync, sets the error icon, and shows the output buffer."
  (let ((ps/git-sync-paused nil)
        (ps/git-sync--last-status nil)
        (ps/git-sync--last-message nil))
    (unwind-protect
        (progn
          (ps/git-sync--handle-conflict "CONFLICT (content): merge in foo.org")
          (should ps/git-sync-paused)
          (should (equal ps/git-sync--last-status ps/git-sync--icon-error))
          (let ((buf (get-buffer "*Org Git Conflict*")))
            (should buf)
            (with-current-buffer buf
              (should (string-match-p "CONFLICT (content): merge in foo.org"
                                      (buffer-string))))))
      (when (get-buffer "*Org Git Conflict*")
        (kill-buffer "*Org Git Conflict*")))))

;;; -------------------------------------------------------
;;; watchdog (reap stale syncs)
;;; -------------------------------------------------------

(ert-deftest ps/git-sync--reap-stale-clears-dead-process ()
  "reap-stale clears a stuck running flag when no live process backs it.
This is the laptop-sleep case: the sentinel never fired, so the guard would
otherwise stay set forever and block all future syncs."
  (let ((ps/git-sync--running t)
        (ps/git-sync--process nil)
        (ps/git-sync--start-time (current-time)))
    (should (ps/git-sync--reap-stale))
    (should-not ps/git-sync--running)
    (should-not ps/git-sync--process)
    (should-not ps/git-sync--start-time)))

(ert-deftest ps/git-sync--reap-stale-noop-when-idle ()
  "reap-stale does nothing when no sync is running."
  (let ((ps/git-sync--running nil))
    (should-not (ps/git-sync--reap-stale))))

(ert-deftest ps/git-sync--reap-stale-kills-hung-process ()
  "reap-stale kills a live process that has run past the timeout."
  (skip-unless (executable-find "sleep"))
  (let* ((proc (start-process "ps-git-sync-test-hung" nil "sleep" "30"))
         (ps/git-sync--running t)
         (ps/git-sync--process proc)
         (ps/git-sync--start-time (time-subtract (current-time) 9999))
         (ps/git-sync-timeout 1))
    (unwind-protect
        (progn
          (should (ps/git-sync--reap-stale))
          (should-not ps/git-sync--running))
      (when (process-live-p proc) (delete-process proc)))))

;;; -------------------------------------------------------
;;; toggle
;;; -------------------------------------------------------

(ert-deftest ps/git-sync--toggle-flips-paused-and-icon ()
  "toggle flips the paused flag and updates the status icon both ways.
Re-enabling also recovers (reap-stale) and ensures the periodic timer exists."
  (let ((ps/git-sync-paused nil)
        (ps/git-sync--last-status nil)
        (ps/git-sync--last-message nil)
        (ps/git-sync--directory nil)
        (ps/git-sync--running nil)
        (ps/git-sync--timer nil))
    (unwind-protect
        (progn
          (ps/git-sync-toggle)
          (should ps/git-sync-paused)
          (should (equal ps/git-sync--last-status ps/git-sync--icon-offline))
          (ps/git-sync-toggle)
          (should-not ps/git-sync-paused)
          (should (equal ps/git-sync--last-status ps/git-sync--icon-ok))
          ;; Re-enabling makes sure the periodic timer is scheduled.
          (should (timerp ps/git-sync--timer)))
      (when (timerp ps/git-sync--timer)
        (cancel-timer ps/git-sync--timer)))))

(ert-deftest ps/git-sync--toggle-is-interactive ()
  "toggle is an interactive command."
  (should (commandp 'ps/git-sync-toggle)))

;;; -------------------------------------------------------
;;; paused gates the sync
;;; -------------------------------------------------------

(ert-deftest ps/git-sync--run-skips-when-paused ()
  "With `ps/git-sync-paused' set, run is a no-op (never marks itself running).
This is the toggle the dev script relies on to disable sync during testing."
  (ps/git-sync-test--with-repo
    (let ((ps/git-sync-paused t)
          (ps/git-sync--running nil))
      (ps/git-sync--run)
      (should-not ps/git-sync--running))))

;;; -------------------------------------------------------
;;; repo detection
;;; -------------------------------------------------------

(ert-deftest ps/git-sync--inside-repo-true ()
  "inside-repo-p is non-nil in a real git repo."
  (ps/git-sync-test--with-repo
    (should (ps/git-sync--inside-repo-p))))

(ert-deftest ps/git-sync--inside-repo-false ()
  "inside-repo-p is nil in a plain directory."
  (ps/git-sync-test--with-non-repo
    (should-not (ps/git-sync--inside-repo-p))))

(ert-deftest ps/git-sync--root-nil-without-directory ()
  "root returns nil when no directory has been configured."
  (let ((ps/git-sync--directory nil))
    (should (null (ps/git-sync--root)))))

(ert-deftest ps/git-sync--root-returns-toplevel ()
  "root returns the git toplevel directory inside a real repo."
  (ps/git-sync-test--with-repo
    (let ((root (ps/git-sync--root)))
      (should (stringp root))
      (should (file-directory-p root))
      ;; Path may be symlink-resolved (e.g. /tmp -> /private/tmp), but the
      ;; temp-dir basename is preserved at the end of the toplevel.
      (should (string-match-p
               (regexp-quote (file-name-nondirectory
                              (directory-file-name ps/git-sync--directory)))
               root)))))

;;; -------------------------------------------------------
;;; start
;;; -------------------------------------------------------

(ert-deftest ps/git-sync--start-sets-directory-and-timer ()
  "start records the directory and creates a repeating timer in a repo.
It must NOT touch `global-mode-string' — the indicator now lives in the
file-tree mode line, not in every window."
  (ps/git-sync-test--with-repo
    (let ((ps/git-sync--timer nil)
          (global-mode-string nil))
      (unwind-protect
          (progn
            (ps/git-sync-start ps/git-sync--directory)
            (should (timerp ps/git-sync--timer))
            (should (null global-mode-string)))
        (when (timerp ps/git-sync--timer)
          (cancel-timer ps/git-sync--timer))))))

(ert-deftest ps/git-sync--start-no-timer-outside-repo ()
  "start does not create a timer when the directory is not a repo."
  (ps/git-sync-test--with-non-repo
    (let ((ps/git-sync--timer nil)
          (global-mode-string nil))
      (ps/git-sync-start ps/git-sync--directory)
      (should (null ps/git-sync--timer)))))

;;; test-ps-git-sync.el ends here
