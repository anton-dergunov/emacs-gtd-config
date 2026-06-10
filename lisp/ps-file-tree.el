;;; ps-file-tree.el --- File tree sidebar (treemacs) helpers -*- lexical-binding: t; -*-

(require 'cl-lib)

;; Provided by treemacs; declared here so this file loads (and its pure
;; functions are testable) without treemacs installed.
(defvar treemacs-ignored-file-predicates)
(declare-function treemacs "treemacs")
(declare-function treemacs-get-local-window "treemacs-scope")
(declare-function treemacs-get-local-buffer "treemacs-scope")
(declare-function treemacs-toggle-node "treemacs-interface")
(declare-function treemacs-collapse-all-projects "treemacs-interface")
(declare-function treemacs-button-get "treemacs-core-utils")
(declare-function treemacs-do-add-project-to-workspace "treemacs-workspaces")
(declare-function treemacs-do-remove-project-from-workspace "treemacs-workspaces")
(declare-function treemacs-current-workspace "treemacs-workspaces")
(declare-function treemacs-workspace->projects "treemacs-workspaces")
(declare-function treemacs-project->path "treemacs-workspaces")
(declare-function treemacs-canonical-path "treemacs-core-utils")
(declare-function treemacs--filename "treemacs-core-utils")

;;; Customization

(defgroup ps-file-tree nil
  "File tree sidebar customization."
  :group 'ps)

(defcustom ps/file-tree-ignored-files
  '("\\`init\\.org\\'" "\\`\\." "\\`elpa\\'" "\\.elc\\'")
  "Regexps matched against file/dir names to hide in the file tree.
A file or directory is hidden if its name matches any regexp here."
  :type '(repeat regexp)
  :group 'ps-file-tree)

;;; Ignore predicate

(defun ps/file-tree--ignored-p (filename _absolute-path)
  "Return non-nil if FILENAME should be hidden from the file tree.
Matched against `ps/file-tree-ignored-files'."
  (cl-some (lambda (rx) (string-match-p rx filename))
           ps/file-tree-ignored-files))

;;;###autoload
(defun ps/file-tree-setup-ignore ()
  "Register `ps/file-tree--ignored-p' with treemacs."
  (add-to-list 'treemacs-ignored-file-predicates #'ps/file-tree--ignored-p))

;;; Show / hide / toggle

(defun ps/file-tree-window-exists-p ()
  "Return non-nil if the file tree window is visible in this frame."
  (and (fboundp 'treemacs-get-local-window)
       (treemacs-get-local-window)))

(defun ps/file-tree-hide ()
  "Hide the file tree window if visible, without killing its buffer."
  (when (ps/file-tree-window-exists-p)
    (delete-window (treemacs-get-local-window))))

(defun ps/file-tree-show ()
  "Show the file tree, restoring it if it was previously hidden."
  (unless (ps/file-tree-window-exists-p)
    (save-selected-window (treemacs))))

(defun ps/file-tree-toggle ()
  "Toggle the file tree window."
  (interactive)
  (treemacs))

;;; Expand / collapse all

(defun ps/file-tree--toggle-matching (states recursive)
  "Toggle every node whose :state is in STATES, in a single forward pass.
RECURSIVE is passed to `treemacs-toggle-node', which expands/collapses each
matching node and all of its descendants in one call. Toggling only inserts
or removes lines after the current position, so the scan can continue
forward without restarting."
  (save-excursion
    (let ((pos (next-button (point-min) t)))
      (while pos
        (when (memq (treemacs-button-get pos :state) states)
          (goto-char pos)
          (treemacs-toggle-node recursive))
        (setq pos (next-button pos))))))

(defun ps/file-tree-expand-all ()
  "Recursively expand every directory in the file tree."
  (interactive)
  (let ((buf (treemacs-get-local-buffer)))
    (when buf
      (with-current-buffer buf
        (ps/file-tree--toggle-matching '(root-node-closed dir-node-closed) t)))))

(defun ps/file-tree-collapse-all ()
  "Recursively collapse every directory in the file tree."
  (interactive)
  (treemacs-collapse-all-projects))

;;; Multi-root project setup

(defun ps/file-tree--list-subdirs (base-dir)
  "Return (NAME . PATH) for each visible immediate subdirectory of BASE-DIR.
Hidden via `ps/file-tree--ignored-p', sorted by name."
  (let (dirs)
    (when (file-directory-p base-dir)
      (dolist (entry (directory-files base-dir t))
        (let ((name (file-name-nondirectory entry)))
          (when (and (file-directory-p entry)
                     (not (member name '("." "..")))
                     (not (ps/file-tree--ignored-p name entry)))
            (push (cons name entry) dirs)))))
    (sort dirs (lambda (a b) (string< (car a) (car b))))))

(defun ps/file-tree-set-projects (base-dir)
  "Make the workspace contain exactly one project per subdirectory of BASE-DIR.
Removes any existing projects whose path is not one of BASE-DIR's immediate
subdirectories, and adds projects for any that are missing."
  (let* ((desired (ps/file-tree--list-subdirs base-dir))
         (desired-paths (mapcar (lambda (d) (treemacs-canonical-path (cdr d))) desired))
         (existing (treemacs-workspace->projects (treemacs-current-workspace))))
    (dolist (project existing)
      (unless (member (treemacs-project->path project) desired-paths)
        (treemacs-do-remove-project-from-workspace project t nil)))
    (let ((existing-paths
           (mapcar #'treemacs-project->path
                   (treemacs-workspace->projects (treemacs-current-workspace)))))
      (dolist (dir desired)
        (let ((path (treemacs-canonical-path (cdr dir))))
          (unless (member path existing-paths)
            (treemacs-do-add-project-to-workspace path (car dir))))))))

(defun ps/file-tree-init (base-dir)
  "Set up the file tree projects for BASE-DIR and expand everything.
Idempotent — safe to call repeatedly (e.g. from staggered timers) while
treemacs's async directory rendering settles."
  (when (treemacs-get-local-buffer)
    (ps/file-tree-set-projects base-dir)
    (ps/file-tree-expand-all)))

(provide 'ps-file-tree)
;;; ps-file-tree.el ends here
