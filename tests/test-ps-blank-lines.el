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

(defun ps/blank-lines-driver-test--commit (dir relpath content message)
  "Write CONTENT to RELPATH under DIR and commit it with MESSAGE."
  (let ((default-directory dir)
        (file (expand-file-name relpath dir)))
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

(ert-deftest ps/blank-lines--rule-based-reinsertion-is-gone ()
  "The superseded command must not linger; a stray F7 would rewrite every
agenda file with no review, which is what this feature exists to replace."
  (should-not (fboundp 'ps/blank-lines-reinsert)))

(provide 'test-ps-blank-lines)
;;; test-ps-blank-lines.el ends here
