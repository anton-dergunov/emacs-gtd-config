;;; ps-blank-lines-git.el --- Find the healthiest ancestor of an Org file -*- lexical-binding: t; -*-

;;; Commentary:

;; The only place blank-line recovery touches git, and it only ever reads.
;; `ps/blank-lines-git--assert-read-only' enforces that mechanically rather
;; than by convention: every invocation goes through one function, and that
;; function refuses any subcommand outside a small allowlist.  Recovery writes
;; files in the working tree and lets the user's normal sync commit them.
;;
;; Ancestor selection needs no commit attribution.  We never ask who made a
;; commit — only which version of the file remembers the most blank lines.
;; That matters because attribution would have been wrong anyway: edits made
;; in VS Code are good edits that Emacs never sees, and they reach git through
;; the same sync as everything else.
;;
;; Candidates are scored with `ps/blank-lines-score-recoverable', which is
;; deliberately rule-free — a candidate should win on what it remembers, not
;; on what a corpus-wide rule would have guessed for any candidate equally.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'ps-blank-lines-engine)

;;; Customization

(defcustom ps/blank-lines-ancestor-max-commits 5
  "How many commits back to consider when looking for a healthy version.

A phone edit arrives as a single commit — the app holds the whole file in
memory and writes it once, on leaving the editor — so the healthy version is
usually the commit right before it.  A handful is plenty; the report's `+'
widens this when it isn't."
  :type 'integer
  :group 'ps-blank-lines)

(defcustom ps/blank-lines-ancestor-max-days 1
  "How far back in days to consider when looking for a healthy version.
Zero means no age limit, leaving the commit count to decide on its own."
  :type 'integer
  :group 'ps-blank-lines)

(defcustom ps/blank-lines-ancestor-steps
  '((1 . 1) (2 . 1) (3 . 1) (4 . 1) (5 . 1)
    (10 . 2) (20 . 3) (50 . 7) (100 . 14) (200 . 30) (500 . 90) (1000 . 365))
  "Rungs of (COMMITS . DAYS) that `-' and `+' step through in the report.

A ladder rather than a scale factor, for two reasons.  Halving cannot land on
every small value that matters — a single edited file is exactly one commit —
and it is not reversible, so `-' `-' `+' `+' left the limits somewhere the user
never chose.  Both limits move together because git ANDs them: raising only the
commit count looks like it did nothing whenever the day limit is the binding
one."
  :type '(alist :key-type integer :value-type integer)
  :group 'ps-blank-lines)

;;; Read-only enforcement

(defconst ps/blank-lines-git--read-only-commands
  '("log" "show" "rev-parse" "cat-file" "ls-tree")
  "The only git subcommands this feature may run.")

(defun ps/blank-lines-git--assert-read-only (args)
  "Signal unless ARGS invokes a read-only git subcommand.  Returns ARGS."
  (let ((subcommand (car args)))
    (unless (member subcommand ps/blank-lines-git--read-only-commands)
      (error "ps/blank-lines: refusing to run `git %s'; reads only"
             (or subcommand "<none>")))
    args))

;;; Running git

(defun ps/blank-lines-git--run (root args)
  "Run git with ARGS in ROOT.  Returns (EXIT . STDOUT), STDOUT decoded UTF-8.

Uses an argument list rather than a shell command line, so no path or ref
needs quoting and nothing can be interpreted as a shell metacharacter."
  (ps/blank-lines-git--assert-read-only args)
  (with-temp-buffer
    (let ((default-directory (file-name-as-directory root))
          (coding-system-for-read 'binary)
          (coding-system-for-write 'binary))
      (let ((exit (apply #'process-file "git" nil t nil args)))
        (cons exit (decode-coding-string (buffer-string) 'utf-8-unix))))))

(defun ps/blank-lines-git-root (directory)
  "Return the git top level containing DIRECTORY, or nil."
  (when (and directory (file-directory-p directory))
    (pcase-let ((`(,exit . ,out)
                 (ps/blank-lines-git--run directory '("rev-parse" "--show-toplevel"))))
      (let ((path (string-trim (or out ""))))
        (and (zerop exit) (not (string-empty-p path)) (file-name-as-directory path))))))

(defun ps/blank-lines-git-relpath (root path)
  "Return PATH relative to ROOT, as git names it.

Both sides go through `file-truename' because `git rev-parse --show-toplevel'
already reports a resolved path: on macOS a directory under /var is really
/private/var, and an Org directory reached through a symlink is ordinary.
Comparing a resolved root against an unresolved path yields a relative name
full of \"..\", which git matches against nothing — so every file silently
looks as though it has no history."
  (file-relative-name (file-truename (expand-file-name path))
                      (file-name-as-directory (file-truename root))))

(defun ps/blank-lines-git-log-commits (root relpath &optional max-commits max-days)
  "Return commits touching RELPATH as ((SHA . TIME) ...), newest first.

MAX-DAYS of zero (or nil once `ps/blank-lines-ancestor-max-days' is zero) drops
`--since' altogether, so the commit count alone decides how far back to look."
  (let ((days (or max-days ps/blank-lines-ancestor-max-days)))
    (pcase-let ((`(,exit . ,out)
                 (ps/blank-lines-git--run
                  root
                  (append (list "log" "--format=%H%x00%cI")
                          (list "-n" (number-to-string
                                      (or max-commits ps/blank-lines-ancestor-max-commits)))
                          (when (and days (> days 0))
                            (list (format "--since=%d.days.ago" days)))
                          (list "--" relpath)))))
      (when (zerop exit)
        (delq nil
              (mapcar (lambda (line)
                        (let ((parts (split-string line "\0")))
                          (when (= (length parts) 2)
                            (cons (nth 0 parts) (nth 1 parts)))))
                      (split-string out "\n" t)))))))

(defun ps/blank-lines-git-show (root sha relpath)
  "Return RELPATH's content at SHA, or nil when it was not in that commit."
  (pcase-let ((`(,exit . ,out)
               (ps/blank-lines-git--run root (list "show" (format "%s:%s" sha relpath)))))
    (and (zerop exit) out)))

;;; Ancestor selection

(cl-defun ps/blank-lines-select-ancestor (root path wtext &key max-commits max-days)
  "Return the version of PATH that would restore the most blank lines.

Returns a plist (:sha :time :text :score :candidates), or nil when no commit
in the window would restore anything.  Ties go to the most recent commit,
since `ps/blank-lines-git-log-commits' returns newest first and a later
candidate must beat the incumbent outright."
  (let ((relpath (ps/blank-lines-git-relpath root path))
        (best nil)
        (n 0))
    (dolist (commit (ps/blank-lines-git-log-commits root relpath max-commits max-days))
      (when-let* ((text (ps/blank-lines-git-show root (car commit) relpath)))
        (setq n (1+ n))
        (unless (equal text wtext)
          (let ((score (ps/blank-lines-score-recoverable wtext text)))
            (when (and (> score 0) (or (null best) (> score (plist-get best :score))))
              (setq best (list :sha (car commit) :time (cdr commit)
                               :text text :score score)))))))
    (when best (append best (list :candidates n)))))

(provide 'ps-blank-lines-git)
;;; ps-blank-lines-git.el ends here
