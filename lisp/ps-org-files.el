;;; ps-org-files.el --- Which .org files this system sees -*- lexical-binding: t; -*-

;;; Commentary:

;; Single source of truth for the set of .org files the agenda, the conflict
;; checker and the availability computation scan.  The scan root is the Org
;; base directory itself, walked recursively, so any directory layout works --
;; a flat folder, one level of topic directories, or arbitrary nesting.
;;
;; Two name-matched exclusion lists keep infrastructure and non-task files out:
;; `ps/org-files-exclude-directories' prunes whole subtrees (dotfiles, the
;; journal, archives) and `ps/org-files-exclude-files' drops individual files
;; (`init.org', `workspace.org').

;;; Code:

(require 'cl-lib)
(require 'seq)

(defvar my-org-base-directory)

(defgroup ps-org-files nil
  "The set of Org files scanned by the agenda and planning tools."
  :group 'ps)

(defcustom ps/org-files-root nil
  "Directory recursively scanned for .org files.
When nil (the default), `my-org-base-directory' is used.  Set this only to
scan a subdirectory of the Org base rather than all of it."
  :type '(choice (const :tag "The Org base directory" nil) directory)
  :group 'ps-org-files)

(defcustom ps/org-files-exclude-directories
  '("\\`\\." "\\`Journal\\'" "\\`Archive\\'")
  "Regexps matched against a directory NAME; matching directories are skipped.
The scan does not descend into a matching directory, so its whole subtree is
excluded.  The defaults skip dotted directories (`.git', `.claude'), the
org-journal directory and archives."
  :type '(repeat regexp)
  :group 'ps-org-files)

(defcustom ps/org-files-exclude-files
  '("\\`init\\.org\\'" "\\`workspace\\.org\\'")
  "Regexps matched against a file NAME; matching .org files are excluded.
The defaults drop the two config files that live in the Org base directory."
  :type '(repeat regexp)
  :group 'ps-org-files)

(defun ps/org-files--name-matches-p (name regexps)
  "Return non-nil if NAME matches any regexp in REGEXPS."
  (cl-some (lambda (rx) (string-match-p rx name)) regexps))

(defun ps/org-files-root ()
  "Return the directory scanned for .org files.
`ps/org-files-root' when set, otherwise `my-org-base-directory'."
  (or ps/org-files-root
      (and (boundp 'my-org-base-directory) my-org-base-directory)))

(defun ps/org-files-in-scope-p (&optional file root)
  "Return non-nil if FILE is a .org file `ps/org-files-in-directory' would find.
FILE defaults to `buffer-file-name'; ROOT defaults to `ps/org-files-root'.
Tests FILE directly against the same two exclusion lists
`ps/org-files-in-directory' uses, without walking the filesystem -- cheap
enough to call once per visited buffer, unlike a full rescan."
  (let ((file (or file buffer-file-name))
        (root (or root (ps/org-files-root))))
    (when (and file root (string-match-p "\\.org\\'" file))
      (let* ((file (expand-file-name file))
             (root (file-name-as-directory (expand-file-name root))))
        (when (string-prefix-p root file)
          (and (not (ps/org-files--name-matches-p
                     (file-name-nondirectory file) ps/org-files-exclude-files))
               (not (cl-some
                     (lambda (component)
                       (ps/org-files--name-matches-p
                        component ps/org-files-exclude-directories))
                     (split-string (or (file-name-directory (substring file (length root))) "")
                                   "/" t)))))))))

(defun ps/org-files-in-directory (directory)
  "Return the sorted .org files under DIRECTORY, recursively.
Directories whose name matches `ps/org-files-exclude-directories' are not
descended into; files whose name matches `ps/org-files-exclude-files' are
dropped.  Returns nil if DIRECTORY is nil or does not exist."
  (when (and directory (file-directory-p directory))
    (sort
     (seq-filter
      (lambda (file) (ps/org-files-in-scope-p file directory))
      (directory-files-recursively
       directory "\\.org\\'" nil
       (lambda (dir)
         (not (ps/org-files--name-matches-p (file-name-nondirectory
                                             (directory-file-name dir))
                                            ps/org-files-exclude-directories)))))
     #'string<)))

(defun ps/org-files-all ()
  "Return every .org file the system scans, sorted.
That is `ps/org-files-in-directory' applied to `ps/org-files-root'."
  (ps/org-files-in-directory (ps/org-files-root)))

(provide 'ps-org-files)
;;; ps-org-files.el ends here
