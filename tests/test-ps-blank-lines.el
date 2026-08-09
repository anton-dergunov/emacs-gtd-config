;;; test-ps-blank-lines.el --- ERT tests for ps-blank-lines -*- lexical-binding: t; -*-

(require 'ert)
(add-to-list 'load-path "lisp")
(require 'ps-blank-lines)

;;; Test helpers

(defmacro ps/blank-lines-driver-test--with-repo (&rest body)
  "Run BODY in a fresh temp git repo bound to `dir', with `root' its git root.

Configured entirely on the command line so the developer's own git config —
signing, hooks, default branch, identity — cannot change the outcome.  No
remote is ever added, so nothing here can reach a real repository."
  (declare (indent 0))
  `(progn
     (skip-unless (executable-find "git"))
     (let ((dir (make-temp-file "ps-blank-lines-test-" t)))
       (unwind-protect
           (let* ((default-directory dir)
                  (root (file-name-as-directory dir)))
             (call-process "git" nil nil nil "init" "-q" "-b" "main")
             ,@body)
         (delete-directory dir t)))))

(defun ps/blank-lines-driver-test--commit (dir relpath content message &optional days-ago)
  "Write CONTENT to RELPATH under DIR and commit it with MESSAGE.
DAYS-AGO, if given, back-dates the commit that many days, which is what the
`--since' window is measured against."
  (let* ((default-directory dir)
         (file (expand-file-name relpath dir))
         (process-environment
          (if days-ago
              (let ((stamp (format-time-string
                            "%Y-%m-%dT%H:%M:%S%z"
                            (time-subtract (current-time) (days-to-time days-ago)))))
                (append (list (concat "GIT_AUTHOR_DATE=" stamp)
                              (concat "GIT_COMMITTER_DATE=" stamp))
                        process-environment))
            process-environment)))
    (make-directory (file-name-directory file) t)
    (with-temp-file file (insert content))
    (call-process "git" nil nil nil "add" relpath)
    (call-process "git" nil nil nil
                  "-c" "user.name=Test" "-c" "user.email=test@example.com"
                  "-c" "commit.gpgsign=false"
                  "commit" "-q" "--no-verify" "-m" message)))

(defun ps/blank-lines-driver-test--shas (root)
  "Return every commit sha in ROOT, newest first."
  (split-string (cdr (ps/blank-lines-git--run root '("log" "--format=%H"))) "\n" t))

;;; --------------------------------------------------------------------------
;;; The read-only guarantee
;;; --------------------------------------------------------------------------

(ert-deftest ps/blank-lines--refuses-every-writing-git-subcommand ()
  "The guard is what makes `reads only' a property of the code, not a promise."
  (dolist (args '(("commit" "-m" "x") ("add" ".") ("push") ("checkout" "main")
                  ("reset" "--hard") ("rebase") ("clean" "-fd") ("stash")))
    (should-error (ps/blank-lines-git--assert-read-only args)))
  (dolist (args '(("log") ("show" "HEAD:f") ("rev-parse" "--show-toplevel")))
    (should (ps/blank-lines-git--assert-read-only args))))

(ert-deftest ps/blank-lines--a-full-run-leaves-history-untouched ()
  "Scanning must not create, move or remove a single commit."
  (ps/blank-lines-driver-test--with-repo
    (ps/blank-lines-driver-test--commit dir "notes.org" "* A\n\n** 1\nbody\n\n** 2\n" "good")
    (ps/blank-lines-driver-test--commit dir "notes.org" "* A\n** 1\nbody\n** 2\n" "stripped")
    (let ((before (ps/blank-lines-driver-test--shas root))
          (my-org-base-directory dir)
          (ps/org-files-root dir))
      (ps/blank-lines-scan)
      (should (equal before (ps/blank-lines-driver-test--shas root))))))

;;; --------------------------------------------------------------------------
;;; Ancestor selection
;;; --------------------------------------------------------------------------

(ert-deftest ps/blank-lines--picks-the-version-that-remembers-the-most ()
  "Selection walks back past the damage to the last healthy version."
  (ps/blank-lines-driver-test--with-repo
    (ps/blank-lines-driver-test--commit dir "notes.org" "* A\n\n** 1\nbody\n\n** 2\n" "good")
    (ps/blank-lines-driver-test--commit dir "notes.org" "* A\n** 1\nbody\n** 2\n" "stripped")
    (ps/blank-lines-driver-test--commit dir "notes.org" "* A\n** 1\nbody\n** 2\n** 3\n" "mobile edit")
    (let* ((working (with-temp-buffer
                      (insert-file-contents (expand-file-name "notes.org" dir))
                      (buffer-string)))
           (pick (ps/blank-lines-select-ancestor root (expand-file-name "notes.org" dir)
                                                 working)))
      (should pick)
      (should (equal (plist-get pick :score) 2))
      (should (equal (plist-get pick :text) "* A\n\n** 1\nbody\n\n** 2\n")))))

(ert-deftest ps/blank-lines--no-ancestor-when-nothing-is-recoverable ()
  "A file that was never damaged yields no candidate, so no proposal."
  (ps/blank-lines-driver-test--with-repo
    (ps/blank-lines-driver-test--commit dir "notes.org" "* A\n\n** 1\nbody\n" "good")
    (should-not (ps/blank-lines-select-ancestor
                 root (expand-file-name "notes.org" dir) "* A\n\n** 1\nbody\n"))))

(ert-deftest ps/blank-lines--show-returns-nil-for-a-path-not-in-that-commit ()
  "A file added later must not look like an empty ancestor."
  (ps/blank-lines-driver-test--with-repo
    (ps/blank-lines-driver-test--commit dir "first.org" "* A\n" "first")
    (let ((sha (car (last (ps/blank-lines-driver-test--shas root)))))
      (should-not (ps/blank-lines-git-show root sha "later.org"))
      (should (ps/blank-lines-git-show root sha "first.org")))))

;;; --------------------------------------------------------------------------
;;; How far back to look
;;; --------------------------------------------------------------------------

(ert-deftest ps/blank-lines--the-day-limit-bounds-the-candidates ()
  "A commit older than the day window is out of reach; zero days lifts the
limit entirely, leaving the commit count to decide on its own."
  (ps/blank-lines-driver-test--with-repo
    (ps/blank-lines-driver-test--commit dir "notes.org" "* A\n\n** 1\nbody\n" "good" 10)
    (ps/blank-lines-driver-test--commit dir "notes.org" "* A\n** 1\nbody\n" "stripped")
    (should (= 1 (length (ps/blank-lines-git-log-commits root "notes.org" 50 1))))
    (should (= 2 (length (ps/blank-lines-git-log-commits root "notes.org" 50 0))))
    ;; And the ancestor that only the wider window can see.
    (let ((file (expand-file-name "notes.org" dir))
          (working "* A\n** 1\nbody\n"))
      (should-not (ps/blank-lines-select-ancestor root file working :max-days 1))
      (should (ps/blank-lines-select-ancestor root file working :max-days 0)))))

(ert-deftest ps/blank-lines--stepping-reaches-a-single-commit ()
  "`-' has to get all the way down to one commit: a file edited on the phone
once is exactly one commit, and the old halving floor of five never let the
search get that narrow."
  (let ((commits 5))
    (dolist (expected '(4 3 2 1 1))
      (setq commits (car (ps/blank-lines--step commits -1)))
      (should (= commits expected)))))

(ert-deftest ps/blank-lines--stepping-is-reversible ()
  "Down a rung and back up returns to where it started -- halving and
doubling did not (50/30 came back as 48/32)."
  (let* ((start '(50 . 7))
         (down (ps/blank-lines--step (car start) -1))
         (up (ps/blank-lines--step (car down) 1)))
    (should (equal down '(20 . 3)))
    (should (equal up start))))

(ert-deftest ps/blank-lines--stepping-stops-at-both-ends ()
  "Past either end the outermost rung is kept, rather than nothing."
  (should (equal (ps/blank-lines--step 1 -1) '(1 . 1)))
  (should (equal (ps/blank-lines--step 1000 1) '(1000 . 365))))

(ert-deftest ps/blank-lines--stepping-works-from-a-typed-in-count ()
  "A count typed with `c' is not on the ladder; stepping moves to the nearest
rung past it in that direction."
  (should (equal (ps/blank-lines--step 37 1) '(50 . 7)))
  (should (equal (ps/blank-lines--step 37 -1) '(20 . 3))))

(ert-deftest ps/blank-lines--exact-limits-are-clamped ()
  "At least one commit is needed to have anything to compare against; days may
be zero, which means any age."
  (let ((ps/blank-lines-ancestor-max-commits 5)
        (ps/blank-lines-ancestor-max-days 1))
    (cl-letf (((symbol-function 'ps/blank-lines-recover) #'ignore))
      (ps/blank-lines-set-commit-limit 0)
      (should (= ps/blank-lines-ancestor-max-commits 1))
      (ps/blank-lines-set-commit-limit 37)
      (should (= ps/blank-lines-ancestor-max-commits 37))
      (ps/blank-lines-set-day-limit -1)
      (should (= ps/blank-lines-ancestor-max-days 0))
      (ps/blank-lines-set-day-limit 14)
      (should (= ps/blank-lines-ancestor-max-days 14)))))

(ert-deftest ps/blank-lines--history-label-reads-as-a-phrase ()
  "The report and the echo area both say the limits in words, and no day
limit reads as `any age' rather than as zero days."
  (should (equal (ps/blank-lines-history-label 1 1) "1 commit, 1 day"))
  (should (equal (ps/blank-lines-history-label 5 30) "5 commits, 30 days"))
  (should (equal (ps/blank-lines-history-label 37 0) "37 commits, any age")))

;;; --------------------------------------------------------------------------
;;; End to end
;;; --------------------------------------------------------------------------

(ert-deftest ps/blank-lines--scan-reports-the-recoverable-file ()
  "A stripped file is found, attributed to its ancestor, and counted."
  (ps/blank-lines-driver-test--with-repo
    (ps/blank-lines-driver-test--commit dir "notes.org" "* A\n\n** 1\nbody\n\n** 2\n" "good")
    (ps/blank-lines-driver-test--commit dir "notes.org" "* A\n** 1\nbody\n** 2\n" "stripped")
    (let* ((my-org-base-directory dir)
           (ps/org-files-root dir)
           (results (cdr (ps/blank-lines-scan)))
           (result (car results)))
      (should (equal (length results) 1))
      (should (equal (ps/blank-lines-result-restored result) 2))
      (should (equal (ps/blank-lines-result-removed result) 0))
      (should (ps/blank-lines-result-sha result))
      (should-not (ps/blank-lines-result-error result)))))

(ert-deftest ps/blank-lines--scan-needs-a-git-repository ()
  "Outside a repo there is no history, and the command says so plainly."
  (let ((dir (make-temp-file "ps-blank-lines-nogit-" t)))
    (unwind-protect
        (let ((my-org-base-directory dir)
              (ps/org-files-root dir))
          (should-error (ps/blank-lines-scan) :type 'user-error))
      (delete-directory dir t))))

;;; --------------------------------------------------------------------------
;;; Wiring
;;; --------------------------------------------------------------------------

(ert-deftest ps/blank-lines--command-is-available ()
  "The entry point stays a command, since config.org binds it to F7."
  (should (commandp 'ps/blank-lines-recover)))

(ert-deftest ps/blank-lines--the-report-binds-the-history-keys ()
  "The report is the only place these limits can be changed from."
  (dolist (binding '(("-" . ps/blank-lines-look-less-far-back)
                     ("+" . ps/blank-lines-look-further-back)
                     ("c" . ps/blank-lines-set-commit-limit)
                     ("y" . ps/blank-lines-set-day-limit)))
    (should (eq (lookup-key ps-blank-lines-mode-map (kbd (car binding)))
                (cdr binding)))))

(ert-deftest ps/blank-lines--rule-based-reinsertion-is-gone ()
  "The superseded command must not linger; a stray F7 would rewrite every
agenda file with no review, which is what this feature exists to replace."
  (should-not (fboundp 'ps/blank-lines-reinsert)))

(provide 'test-ps-blank-lines)
;;; test-ps-blank-lines.el ends here
