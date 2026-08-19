;;; test-ps-git-sync.el --- ERT tests for ps-git-sync -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
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

(defmacro ps/git-sync-test--capturing-messages (var &rest body)
  "Run BODY with `ps/git-sync--message' collecting into VAR instead of echoing."
  (declare (indent 1))
  `(let ((,var nil))
     (cl-letf (((symbol-function 'ps/git-sync--message)
                (lambda (text) (setq ,var (append ,var (list text))))))
       ,@body)))

(defmacro ps/git-sync-test--with-clean-state (&rest body)
  "Run BODY with all failure/status state freshly bound."
  (declare (indent 0))
  `(let ((ps/git-sync--state 'ok)
         (ps/git-sync--last-message "")
         (ps/git-sync--last-success-time nil)
         (ps/git-sync--failure-signature nil)
         (ps/git-sync--failure-count 0)
         (ps/git-sync--failure-since nil)
         (ps/git-sync--log nil)
         (ps/git-sync--paused-class nil)
         (ps/git-sync--copies nil)
         (ps/git-sync-paused nil))
     ,@body))

;;; Fixtures

(defconst ps/git-sync-test--github-500
  "Already up to date.
[main 23f8150] Auto backup: 2026-08-13 15:36:03 (Antons-MacBook-Air-13.local)
 2 files changed, 8 insertions(+), 1 deletion(-)
Enumerating objects: 11, done.
Counting objects:   9% (1/11)
Counting objects: 100% (11/11)
Counting objects: 100% (11/11), done.
Delta compression using up to 8 threads
Compressing objects:  16% (1/6)
Compressing objects: 100% (6/6), done.
Writing objects:  16% (1/6)
Writing objects: 100% (6/6), 1.93 KiB | 1.93 MiB/s, done.
Total 6 (delta 4), reused 0 (delta 0), pack-reused 0 (from 0)
remote: Resolving deltas:   0% (0/4)
remote: Resolving deltas: 100% (4/4), completed with 4 local objects.
remote: Internal Server Error
remote: Request ID 9FF9:30012A:249069:27E057:6A7DD655
remote: Time 2026-08-13T14:36:06Z
To https://github.com/example/notes.git
 ! [remote rejected] main -> main (Internal Server Error)
error: failed to push some refs to 'https://github.com/example/notes.git'"
  "The real output of the GitHub outage this reporting was built for.")

(defconst ps/git-sync-test--dropbox-refs
  "fatal: bad object refs/heads/main (Anton Dergunov's conflicted copy 2026-08-19)
error: https://github.com/example/notes.git did not send all necessary objects"
  "The real output of 2026-08-19: Dropbox syncing .git between two laptops
left a conflicted copy in refs/heads, which git read as a branch name.")

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
;;; Progress stripping
;;; -------------------------------------------------------

(ert-deftest ps/git-sync--strip-progress-drops-chatter ()
  "The progress lines that made the echo area cover the frame are dropped."
  (let ((clean (ps/git-sync--strip-progress ps/git-sync-test--github-500)))
    (dolist (noise '("Counting objects" "Compressing objects" "Writing objects"
                     "Enumerating objects" "Delta compression"
                     "Resolving deltas" "Total 6"))
      (should-not (string-match-p (regexp-quote noise) clean)))
    ;; …while everything that says what went wrong survives.
    (should (string-match-p "remote: Internal Server Error" clean))
    (should (string-match-p "remote rejected" clean))
    (should (string-match-p "failed to push some refs" clean))
    ;; 21 lines of git output reduced to a handful.
    (should (< (length (split-string clean "\n")) 10))))

(ert-deftest ps/git-sync--strip-progress-splits-carriage-returns ()
  "Progress redrawn in place with \\r is split apart and dropped too."
  (should (equal (ps/git-sync--strip-progress
                  "Writing objects:  50% (1/2)\rWriting objects: 100% (2/2)\rdone\n")
                 "done")))

(ert-deftest ps/git-sync--strip-progress-handles-empty ()
  "Empty and nil output strip to the empty string rather than erroring."
  (should (equal (ps/git-sync--strip-progress nil) ""))
  (should (equal (ps/git-sync--strip-progress "\n\n  \n") "")))

;;; -------------------------------------------------------
;;; Classification
;;; -------------------------------------------------------

(ert-deftest ps/git-sync--classify-github-500 ()
  "The GitHub outage is a server error, not a rejected push or a conflict."
  (should (eq (ps/git-sync--classify ps/git-sync-test--github-500)
              'remote-error)))

(ert-deftest ps/git-sync--classify-each-class ()
  "Each failure class is recognised from representative git output."
  (should (eq (ps/git-sync--classify
               "CONFLICT (content): Merge conflict in Work.org")
              'conflict))
  (should (eq (ps/git-sync--classify
               "error: Your local changes to the following files would be \
overwritten by merge: Work.org")
              'local))
  (should (eq (ps/git-sync--classify
               "remote: Support for password authentication was removed.\n\
fatal: Authentication failed for 'https://github.com/example/notes.git/'")
              'auth))
  (should (eq (ps/git-sync--classify
               " ! [rejected]        main -> main (non-fast-forward)")
              'rejected))
  (should (eq (ps/git-sync--classify
               "fatal: unable to access 'https://github.com/example/notes.git/': \
Could not resolve host: github.com")
              'offline))
  (should (eq (ps/git-sync--classify "something else entirely") 'unknown)))

(ert-deftest ps/git-sync--classify-conflict-wins-over-network ()
  "A conflict is reported as a conflict even when the output also looks flaky.
Ordering in `ps/git-sync--failure-patterns' is what guarantees this."
  (should (eq (ps/git-sync--classify
               "CONFLICT (content): Merge conflict in Work.org\n\
fatal: unable to access remote")
              'conflict)))

(ert-deftest ps/git-sync--classify-dropbox-conflicted-copy ()
  "The 2026-08-19 failure is a cloud-syncer casualty, not a merge conflict.
Dropbox replicated .git between two laptops and left a conflicted copy in
refs/heads; git read it as a branch and every command died with `bad
object'.  Nothing about it can be fixed in Magit."
  (should (eq (ps/git-sync--classify ps/git-sync-test--dropbox-refs)
              'cloud-copy)))

(ert-deftest ps/git-sync--classify-is-case-sensitive ()
  "Case folding is what made a \"conflicted copy\" look like a `CONFLICT'.
`case-fold-search' defaults to t and `string-match-p' honours it, so this
is a property of the classifier and not of the patterns."
  (should (eq (ps/git-sync--classify "CONFLICT (content): Merge conflict in W.org")
              'conflict))
  (should-not (eq (ps/git-sync--classify
                   "fatal: bad object refs/heads/main (conflicted copy)")
                  'conflict))
  ;; Nor the other way round: a real conflict is not a cloud copy.
  (should-not (eq (ps/git-sync--classify
                   "CONFLICT (content): Merge conflict in Work.org")
                  'cloud-copy)))

(ert-deftest ps/git-sync--classify-cloud-copy-outranks-the-push-error ()
  "A damaged repository fails at the push, so it reports both."
  (should (eq (ps/git-sync--classify
               (concat ps/git-sync-test--dropbox-refs
                       "\n ! [rejected]        main -> main (non-fast-forward)"))
              'cloud-copy)))

(ert-deftest ps/git-sync--pausing-classes-are-the-unrecoverable-ones ()
  "Only failures the next tick cannot possibly clear stop the sync."
  (should (equal (sort (copy-sequence ps/git-sync--pausing-classes) #'string<)
                 '(cloud-copy conflict)))
  (dolist (class ps/git-sync--pausing-classes)
    (should (memq class ps/git-sync--attention-classes))))

(ert-deftest ps/git-sync--classify-a-missing-repository ()
  "A .git file pointing at a repository that is not on this machine.
That is the layout `docs/Dropbox-and-git.org' recommends, and the failure a
machine that never had its half of the setup done will show.  It must read
as `needs you' rather than as something the next tick might fix."
  (should (eq (ps/git-sync--classify
               "fatal: not a git repository: /Users/you/.gitrepos/notes-org")
              'no-repo))
  (should (eq (ps/git-sync--class-severity 'no-repo) 'failed))
  ;; But it does not pause: an unmounted volume comes back on its own.
  (should-not (memq 'no-repo ps/git-sync--pausing-classes)))

(ert-deftest ps/git-sync--class-severity-splits-attention-from-retry ()
  "Failures that need the user are `failed'; the self-healing ones `retrying'."
  (dolist (class '(cloud-copy conflict no-repo local auth rejected))
    (should (eq (ps/git-sync--class-severity class) 'failed)))
  (dolist (class '(remote-error offline unknown))
    (should (eq (ps/git-sync--class-severity class) 'retrying))))

(ert-deftest ps/git-sync--cloud-copies-finds-them-in-the-tree ()
  "A conflicted copy anywhere under the vault is found; ordinary files are not."
  (ps/git-sync-test--with-non-repo
    (make-directory (expand-file-name "Work" dir))
    (dolist (name '("Inbox.org"
                    "Inbox (Anton Dergunov's conflicted copy 2026-08-19).org"
                    "Work/Plans.org"
                    "Work/Plans (Anton's conflicted copy 2026-08-19).org"))
      (write-region "" nil (expand-file-name name dir)))
    (should (equal (ps/git-sync--cloud-copies dir)
                   '("Inbox (Anton Dergunov's conflicted copy 2026-08-19).org"
                     "Work/Plans (Anton's conflicted copy 2026-08-19).org")))))

(ert-deftest ps/git-sync--cloud-copies-skips-dotted-directories ()
  "The walk does not descend into .git — an object store is far too big to
walk once a minute — but the three named directories inside it are checked,
because a copy landing in refs/heads is what breaks git outright."
  (ps/git-sync-test--with-non-repo
    (make-directory (expand-file-name ".git/refs/heads" dir) t)
    (make-directory (expand-file-name ".git/objects/ab" dir) t)
    (write-region "" nil (expand-file-name
                          ".git/refs/heads/main (conflicted copy 2026-08-19)" dir))
    (write-region "" nil (expand-file-name
                          ".git/objects/ab/cdef (conflicted copy 2026-08-19)" dir))
    (should (equal (ps/git-sync--cloud-copies dir)
                   '(".git/refs/heads/main (conflicted copy 2026-08-19)")))))

(ert-deftest ps/git-sync--cloud-copies-can-be-turned-off ()
  "A nil regexp stops the check, and a missing directory is not an error."
  (ps/git-sync-test--with-non-repo
    (write-region "" nil (expand-file-name "A (conflicted copy).org" dir))
    (let ((ps/git-sync-cloud-copy-regexp nil))
      (should-not (ps/git-sync--cloud-copies dir))))
  (should-not (ps/git-sync--cloud-copies "/nonexistent/vault/"))
  (should-not (ps/git-sync--cloud-copies nil)))

(ert-deftest ps/git-sync--note-copies-overrides-a-healthy-sync ()
  "The sync worked; the working tree still needs the user.  Announced once."
  (ps/git-sync-test--with-non-repo
    (ps/git-sync-test--with-clean-state
      (ps/git-sync-test--capturing-messages msgs
        (write-region "" nil (expand-file-name "A (conflicted copy).org" dir))
        (ps/git-sync--note-success)
        (should (eq ps/git-sync--state 'ok))
        (ps/git-sync--note-copies)
        (should (eq ps/git-sync--state 'copies))
        (should (equal ps/git-sync--copies '("A (conflicted copy).org")))
        (should (= (length msgs) 1))
        ;; The same set on the next tick keeps the state but stays quiet.
        (ps/git-sync--note-copies)
        (should (eq ps/git-sync--state 'copies))
        (should (= (length msgs) 1))))))

(ert-deftest ps/git-sync--note-copies-is-silent-when-there-are-none ()
  "A clean tree leaves the sync's own state alone."
  (ps/git-sync-test--with-non-repo
    (ps/git-sync-test--with-clean-state
      (ps/git-sync-test--capturing-messages msgs
        (ps/git-sync--note-success)
        (ps/git-sync--note-copies)
        (should (eq ps/git-sync--state 'ok))
        (should-not ps/git-sync--copies)
        (should-not msgs)))))

;;; -------------------------------------------------------
;;; Reason extraction / echo line
;;; -------------------------------------------------------

(ert-deftest ps/git-sync--reason-picks-the-server-error ()
  "The reason is the remote's own message, with its `remote: ' marker dropped."
  (should (equal (ps/git-sync--reason ps/git-sync-test--github-500)
                 "Internal Server Error")))

(ert-deftest ps/git-sync--reason-prefers-conflict-line ()
  "For a conflict the named file matters more than git's closing advice."
  (should (equal (ps/git-sync--reason
                  "Auto-merging Work.org
CONFLICT (content): Merge conflict in Work.org
Automatic merge failed; fix conflicts and then commit the result.")
                 "CONFLICT (content): Merge conflict in Work.org")))

(ert-deftest ps/git-sync--reason-falls-back-to-last-line ()
  "With nothing recognisable, the last line is still better than nothing."
  (should (equal (ps/git-sync--reason "first thing\nlast thing\n") "last thing"))
  (should (null (ps/git-sync--reason ""))))

(ert-deftest ps/git-sync--echo-line-is-one-line ()
  "The echo line never contains a newline — that is what broke the echo area."
  (let ((line (ps/git-sync--echo-line
               (ps/git-sync--classify ps/git-sync-test--github-500)
               (ps/git-sync--reason ps/git-sync-test--github-500))))
    (should-not (string-match-p "\n" line))
    (should (string-match-p "Internal Server Error" line))
    (should (string-match-p "ps/git-sync-show-log" line))))

(ert-deftest ps/git-sync--echo-line-keeps-the-log-pointer ()
  "A long reason is shortened so the pointer at the log is not truncated away."
  (let* ((reason (concat "unable to access 'https://example.invalid/a/very/long/"
                         "repository/path.git/': Could not resolve host"))
         (line (ps/git-sync--echo-line 'offline reason)))
    (should (< (length line) 130))
    (should (string-match-p "cannot reach the remote" line))
    (should (string-match-p "ps/git-sync-show-log for details\\'" line))
    ;; The tooltip and the log keep the full reason.
    (should (string-match-p (regexp-quote reason)
                            (ps/git-sync--status-message 'offline reason)))))

;;; -------------------------------------------------------
;;; Echo policy: announce once per distinct failure
;;; -------------------------------------------------------

(ert-deftest ps/git-sync--repeat-failure-is-announced-once ()
  "An outage that repeats every tick interrupts the user exactly once."
  (ps/git-sync-test--with-clean-state
    (ps/git-sync-test--capturing-messages msgs
      (dotimes (_ 5) (ps/git-sync--note-failure ps/git-sync-test--github-500))
      (should (= (length msgs) 1))
      (should (= ps/git-sync--failure-count 5))
      (should (eq ps/git-sync--state 'retrying)))))

(ert-deftest ps/git-sync--changed-failure-is-announced-again ()
  "A different failure is news, so it is announced even mid-outage."
  (ps/git-sync-test--with-clean-state
    (ps/git-sync-test--capturing-messages msgs
      (ps/git-sync--note-failure ps/git-sync-test--github-500)
      (ps/git-sync--note-failure "fatal: Authentication failed for 'origin'")
      (should (= (length msgs) 2))
      ;; …and the counter restarts rather than carrying the old failure over.
      (should (= ps/git-sync--failure-count 1))
      (should (eq ps/git-sync--state 'failed)))))

(ert-deftest ps/git-sync--recovery-is-announced-once ()
  "Recovery is reported once, and only when we had actually been failing."
  (ps/git-sync-test--with-clean-state
    (ps/git-sync-test--capturing-messages msgs
      (ps/git-sync--note-failure ps/git-sync-test--github-500)
      (ps/git-sync--note-failure ps/git-sync-test--github-500)
      (ps/git-sync--note-success)
      (should (= (length msgs) 2))
      (should (string-match-p "recovered" (nth 1 msgs)))
      (should (string-match-p "2 failed attempts" (nth 1 msgs)))
      (should (eq ps/git-sync--state 'ok))
      (should (null ps/git-sync--failure-signature))
      ;; A second success says nothing more.
      (ps/git-sync--note-success)
      (should (= (length msgs) 2)))))

(ert-deftest ps/git-sync--success-alone-is-silent ()
  "A healthy sync never touches the echo area."
  (ps/git-sync-test--with-clean-state
    (ps/git-sync-test--capturing-messages msgs
      (ps/git-sync--note-success)
      (should (null msgs))
      (should ps/git-sync--last-success-time))))

(ert-deftest ps/git-sync--message-collapses-to-one-line ()
  "Even handed multi-line text, the echo area gets a single line."
  (let ((got nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (setq got (apply #'format fmt args)))))
      (ps/git-sync--message "line one\nline two\rline three"))
    (should (equal got "line one line two line three"))))

;;; -------------------------------------------------------
;;; Sync log
;;; -------------------------------------------------------

(ert-deftest ps/git-sync--log-records-failures-newest-first ()
  "Each failure is logged with its full (stripped) output, newest first."
  (ps/git-sync-test--with-clean-state
    (ps/git-sync-test--capturing-messages _msgs
      (ps/git-sync--note-failure ps/git-sync-test--github-500)
      (ps/git-sync--note-failure "fatal: Authentication failed for 'origin'")
      (should (= (length ps/git-sync--log) 2))
      (should (eq (nth 1 (car ps/git-sync--log)) 'auth))
      (should (eq (nth 1 (cadr ps/git-sync--log)) 'remote-error)))))

(ert-deftest ps/git-sync--log-is-bounded ()
  "The log keeps only the most recent entries."
  (ps/git-sync-test--with-clean-state
    (let ((ps/git-sync--log-max 3))
      (ps/git-sync-test--capturing-messages _msgs
        (dotimes (i 10)
          (ps/git-sync--note-failure (format "fatal: failure number %d" i)))
        (should (= (length ps/git-sync--log) 3))
        (should (string-match-p "number 9" (nth 3 (car ps/git-sync--log))))))))

(ert-deftest ps/git-sync--log-render-empty ()
  "An empty log renders a sentence rather than a blank buffer."
  (should (string-match-p "No sync failures recorded"
                          (ps/git-sync--log-render nil))))

(ert-deftest ps/git-sync--log-render-conflict-guidance ()
  "The conflict case leads with how to resolve and resume."
  (let ((text (ps/git-sync--log-render nil 'conflict)))
    (should (string-match-p "merge conflict" text))
    (should (string-match-p "Magit" text))
    (should (string-match-p "Git Sync Enabled" text)))
  (should-not (string-match-p "Magit" (ps/git-sync--log-render nil))))

(ert-deftest ps/git-sync--log-render-cloud-copy-guidance ()
  "A Dropbox-damaged repository gets its own remedy, not the merge one.
Sending the user to Magit to resolve a merge that never happened is exactly
what this whole class exists to stop."
  (let ((text (ps/git-sync--log-render nil 'cloud-copy)))
    (should (string-match-p "conflicted" text))
    (should (string-match-p "Dropbox-and-git" text))
    (should (string-match-p "Git Sync Enabled" text))
    (should-not (string-match-p "Magit" text))))

(ert-deftest ps/git-sync--log-render-lists-copies ()
  "Conflicted copies found in the tree are named in the log, paused or not."
  (let ((text (ps/git-sync--log-render
               nil nil '("Inbox (conflicted copy 2026-08-19).org"))))
    (should (string-match-p "Inbox (conflicted copy 2026-08-19).org" text)))
  (should-not (string-match-p "conflicted copies in your vault"
                              (ps/git-sync--log-render nil))))

(ert-deftest ps/git-sync--show-log-is-interactive ()
  "The log is reachable by name and by the mode-line click."
  (should (commandp 'ps/git-sync-show-log)))

;;; -------------------------------------------------------
;;; set-status / modeline
;;; -------------------------------------------------------

(ert-deftest ps/git-sync--set-status-updates-vars ()
  "set-status updates both the state and the help message."
  (let ((ps/git-sync--state nil)
        (ps/git-sync--last-message nil))
    (ps/git-sync--set-status 'ok "all good")
    (should (eq ps/git-sync--state 'ok))
    (should (equal ps/git-sync--last-message "all good"))))

(ert-deftest ps/git-sync--modeline-contains-icon ()
  "The modeline string contains the current status icon."
  (ps/git-sync-test--with-clean-state
    (should (string-match-p (regexp-quote ps/git-sync--icon-ok)
                            (ps/git-sync--modeline)))))

(ert-deftest ps/git-sync--label-text-per-state ()
  "Each state maps to its documented icon and text label."
  (dolist (case `((ok       ,ps/git-sync--icon-ok      "Sync")
                  (syncing  ,ps/git-sync--icon-syncing "Syncing")
                  (off      ,ps/git-sync--icon-offline "Sync Off")
                  (retrying ,ps/git-sync--icon-error   "Sync Retrying")
                  (failed   ,ps/git-sync--icon-error   "Sync Failed")))
    (let ((ps/git-sync--state (nth 0 case)))
      (should (equal (ps/git-sync--label)
                     (concat (nth 1 case) " " (nth 2 case)))))))

(ert-deftest ps/git-sync--label-off-and-retrying-differ ()
  "A remote outage must not look like the user turning sync off.
Collapsing these two was the original reporting bug."
  (let ((off (let ((ps/git-sync--state 'off)) (ps/git-sync--label)))
        (retrying (let ((ps/git-sync--state 'retrying)) (ps/git-sync--label))))
    (should-not (equal off retrying))))

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

(ert-deftest ps/git-sync--help-echo-appends-success-time ()
  "The tooltip appends the last successful sync time when known."
  (let ((ps/git-sync--state 'syncing)
        (ps/git-sync--last-message "Git sync in progress")
        (ps/git-sync--last-success-time (current-time)))
    (let ((echo (ps/git-sync--help-echo)))
      (should (string-match-p "Git sync in progress" echo))
      (should (string-match-p
               (concat "Last successful sync: "
                       (regexp-quote (ps/git-sync--format-success-time)))
               echo)))))

(ert-deftest ps/git-sync--help-echo-ok-shows-only-time ()
  "In the OK state the tooltip shows the time once, not a duplicated message."
  (let ((ps/git-sync--state 'ok)
        (ps/git-sync--last-message "Git sync OK")
        (ps/git-sync--last-success-time (current-time)))
    (let ((echo (ps/git-sync--help-echo)))
      (should-not (string-match-p "Git sync OK" echo))
      (should (string-match-p "Last successful sync: " echo)))))

(ert-deftest ps/git-sync--help-echo-shows-attempts-while-failing ()
  "While failing, the tooltip says what failed and for how long."
  (ps/git-sync-test--with-clean-state
    (ps/git-sync-test--capturing-messages _msgs
      (ps/git-sync--note-failure ps/git-sync-test--github-500)
      (ps/git-sync--note-failure ps/git-sync-test--github-500)
      (ps/git-sync--note-failure ps/git-sync-test--github-500)
      (let ((echo (ps/git-sync--help-echo)))
        (should (string-match-p "Internal Server Error" echo))
        (should (string-match-p "Failing since .* (3 attempts)" echo))
        (should (string-match-p "mouse-1: show sync log" echo))))))

(ert-deftest ps/git-sync--modeline-help-echo ()
  "The modeline string carries the last message as a help-echo property."
  (ps/git-sync-test--with-clean-state
    (setq ps/git-sync--state 'syncing
          ps/git-sync--last-message "hello there")
    (let ((s (ps/git-sync--modeline)))
      (should (string-match-p "hello there"
                              (get-text-property (1- (length s)) 'help-echo s))))))

(ert-deftest ps/git-sync--modeline-failure-faces ()
  "Retryable failures use `warning', ones needing action use `error'."
  (let ((ps/git-sync--state 'retrying)
        (ps/git-sync--last-message "retrying"))
    (let ((s (ps/git-sync--modeline)))
      (should (eq (get-text-property (1- (length s)) 'face s) 'warning))))
  (let ((ps/git-sync--state 'failed)
        (ps/git-sync--last-message "failed"))
    (let ((s (ps/git-sync--modeline)))
      (should (eq (get-text-property (1- (length s)) 'face s) 'error)))))

(ert-deftest ps/git-sync--modeline-healthy-states-carry-no-face ()
  "OK, syncing and off inherit the mode-line colour instead of naming one."
  (dolist (state '(ok syncing off))
    (let ((ps/git-sync--state state)
          (ps/git-sync--last-message "msg"))
      (let ((s (ps/git-sync--modeline)))
        (should (null (get-text-property (1- (length s)) 'face s)))))))

(ert-deftest ps/git-sync--modeline-is-clickable ()
  "mouse-1 on the indicator opens the sync log."
  (ps/git-sync-test--with-clean-state
    (let* ((s (ps/git-sync--modeline))
           (map (get-text-property 0 'local-map s)))
      (should (keymapp map))
      (should (eq (lookup-key map [mode-line mouse-1]) #'ps/git-sync-show-log))
      (should (get-text-property 0 'mouse-face s)))))

;;; -------------------------------------------------------
;;; conflict handling
;;; -------------------------------------------------------

(ert-deftest ps/git-sync--handle-pause-pauses-and-shows ()
  "handle-pause pauses sync, sets the failed state, and shows the log."
  (ps/git-sync-test--with-clean-state
    (ps/git-sync-test--capturing-messages _msgs
      (unwind-protect
          (progn
            (ps/git-sync--note-failure
             "CONFLICT (content): Merge conflict in Work.org")
            (ps/git-sync--handle-pause 'conflict)
            (should ps/git-sync-paused)
            (should (eq ps/git-sync--paused-class 'conflict))
            (should (eq ps/git-sync--state 'failed))
            (let ((buf (get-buffer "*Org Git Sync*")))
              (should buf)
              (with-current-buffer buf
                (should (string-match-p "Merge conflict in Work.org"
                                        (buffer-string)))
                (should (string-match-p "Magit" (buffer-string))))))
        (when (get-buffer "*Org Git Sync*")
          (kill-buffer "*Org Git Sync*"))))))

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

(ert-deftest ps/git-sync--toggle-flips-paused-and-state ()
  "toggle flips the paused flag and updates the state both ways.
Re-enabling also recovers (reap-stale) and ensures the periodic timer exists."
  (let ((ps/git-sync-paused nil)
        (ps/git-sync--state nil)
        (ps/git-sync--last-message nil)
        (ps/git-sync--directory nil)
        (ps/git-sync--running nil)
        (ps/git-sync--timer nil))
    (unwind-protect
        (progn
          (ps/git-sync-toggle)
          (should ps/git-sync-paused)
          (should (eq ps/git-sync--state 'off))
          (ps/git-sync-toggle)
          (should-not ps/git-sync-paused)
          (should (eq ps/git-sync--state 'ok))
          ;; Re-enabling makes sure the periodic timer is scheduled.
          (should (timerp ps/git-sync--timer)))
      (when (timerp ps/git-sync--timer)
        (cancel-timer ps/git-sync--timer)))))

(ert-deftest ps/git-sync--toggle-clears-failure-state ()
  "Resuming starts fresh, so the next failure is announced rather than swallowed."
  (ps/git-sync-test--with-clean-state
    (let ((ps/git-sync--directory nil)
          (ps/git-sync--running nil)
          (ps/git-sync--timer nil))
      (unwind-protect
          (ps/git-sync-test--capturing-messages msgs
            (ps/git-sync--note-failure ps/git-sync-test--github-500)
            (setq ps/git-sync-paused t)
            (ps/git-sync-toggle)          ; resume
            (should (null ps/git-sync--failure-signature))
            (should (= ps/git-sync--failure-count 0))
            (ps/git-sync--note-failure ps/git-sync-test--github-500)
            (should (= (length msgs) 2)))
        (when (timerp ps/git-sync--timer)
          (cancel-timer ps/git-sync--timer))))))

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

(ert-deftest ps/git-sync--inside-repo-false-in-a-subdirectory ()
  "A vault nested inside a repo does not sync -- that repo is not its own.
Syncing it would commit and push the enclosing checkout on the vault's behalf."
  (ps/git-sync-test--with-repo
    (let ((ps/git-sync--directory (expand-file-name "notes/" ps/git-sync--directory)))
      (make-directory ps/git-sync--directory t)
      (should-not (ps/git-sync--inside-repo-p)))))

;;; -------------------------------------------------------
;;; start / stop
;;; -------------------------------------------------------

(ert-deftest ps/git-sync-stop-clears-every-sticky-variable ()
  "Stopping leaves nothing describing the old repo behind.
Each variable is named here on purpose: a new piece of sticky state added
without a matching reset would silently show the next vault as broken."
  (let ((ps/git-sync--timer nil)
        (ps/git-sync--directory "/tmp/old/")
        (ps/git-sync--interval 300)
        (ps/git-sync--running t)
        (ps/git-sync--process nil)
        (ps/git-sync--start-time (current-time))
        (ps/git-sync-paused t)
        (ps/git-sync--last-success-time (current-time))
        (ps/git-sync--failure-signature '(auth . "denied"))
        (ps/git-sync--failure-count 7)
        (ps/git-sync--failure-since (current-time))
        (ps/git-sync--log '((nil auth "denied" "")))
        (ps/git-sync--paused-class 'conflict)
        (ps/git-sync--copies '("Inbox (conflicted copy).org"))
        (ps/git-sync--state 'error))
    (ps/git-sync-stop)
    (should-not ps/git-sync--timer)
    (should-not ps/git-sync--directory)
    (should-not ps/git-sync--interval)
    (should-not ps/git-sync--running)
    (should-not ps/git-sync--process)
    (should-not ps/git-sync--start-time)
    (should-not ps/git-sync-paused)
    (should-not ps/git-sync--last-success-time)
    (should-not ps/git-sync--failure-signature)
    (should (= ps/git-sync--failure-count 0))
    (should-not ps/git-sync--failure-since)
    (should-not ps/git-sync--log)
    (should-not ps/git-sync--paused-class)
    (should-not ps/git-sync--copies)
    (should (eq ps/git-sync--state 'off))))

(ert-deftest ps/git-sync-maybe-start-syncs-a-repo ()
  "A vault that is a git working tree starts syncing."
  (ps/git-sync-test--with-repo
    (let ((dir ps/git-sync--directory)
          (process-environment (cons "PS_GIT_SYNC_DISABLE" process-environment))
          (ps/git-sync--timer nil))
      (setenv "PS_GIT_SYNC_DISABLE" nil)
      (unwind-protect
          (progn
            (ps/git-sync-maybe-start dir)
            (should (equal ps/git-sync--directory dir))
            (should (timerp ps/git-sync--timer))
            ;; Not left saying whatever the previous vault said: the first
            ;; tick is ten seconds out and the indicator is on screen now.
            (should (eq ps/git-sync--state 'ok)))
        (when (timerp ps/git-sync--timer) (cancel-timer ps/git-sync--timer))))))

(ert-deftest ps/git-sync-maybe-start-explains-a-non-repo ()
  "A vault with no .git is left alone, and the mode line says why."
  (ps/git-sync-test--with-non-repo
    (let ((dir ps/git-sync--directory)
          (process-environment (cons "PS_GIT_SYNC_DISABLE" process-environment))
          (ps/git-sync--timer nil)
          (ps/git-sync--state 'ok))
      (setenv "PS_GIT_SYNC_DISABLE" nil)
      (ps/git-sync-maybe-start dir)
      (should (eq ps/git-sync--state 'off))
      (should-not ps/git-sync--timer)
      (should (string-match-p "git init" ps/git-sync--last-message)))))

(ert-deftest ps/git-sync-maybe-start-without-a-vault ()
  "With no vault open there is nothing to sync, and no error either."
  (let ((ps/git-sync--timer nil)
        (ps/git-sync--state 'ok))
    (ps/git-sync-maybe-start nil)
    (should (eq ps/git-sync--state 'off))
    (should-not ps/git-sync--timer)))

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
