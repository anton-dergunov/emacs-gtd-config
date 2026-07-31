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
(declare-function treemacs-current-button "treemacs-core-utils")
(declare-function treemacs-do-add-project-to-workspace "treemacs-workspaces")
(declare-function treemacs-do-remove-project-from-workspace "treemacs-workspaces")
(declare-function treemacs-current-workspace "treemacs-workspaces")
(declare-function treemacs-workspace->projects "treemacs-workspaces")
(declare-function treemacs-project->path "treemacs-workspaces")
(declare-function treemacs-canonical-path "treemacs-core-utils")
(declare-function treemacs--filename "treemacs-core-utils")
(declare-function org-find-exact-headline-in-buffer "org")
;; Optional: git-sync status is appended to the file-tree mode line when the
;; module is loaded.  Guarded with `fboundp', so this file stays standalone.
(declare-function ps/git-sync--modeline "ps-git-sync" ())
(require 'subr-x)

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

(defcustom ps/file-tree-file-sets
  '(("All" . (:include nil :exclude nil)))
  "Named file-visibility sets for the file tree.
Each entry is (NAME . PLIST) where PLIST has:
  :include - list of regexps; if non-nil, only paths matching one of these
             (or directories containing a matching descendant) are shown.
  :exclude - list of regexps; paths matching any of these are hidden,
             regardless of :include.
Regexps are matched as substrings against the absolute path.

This is purely a display filter for the file tree — it does not affect the
agenda or any other part of the system, which continue to see every file.

The first entry should remain (\"All\" . (:include nil :exclude nil)),
showing everything. It is also used as the fallback set by
`ps/file-tree--ensure-valid-set' when `ps/file-tree-current-set' names a
set that no longer exists.

Switching sets re-renders the existing projects but does not recompute the
project list. With the default `ps/file-tree-root-mode' of `single' the
whole tree is one project, so a set may target any directory in it. With
`subdirs', a set should filter within the top-level project directories
rather than targeting those directories themselves."
  :type '(alist :key-type string
                 :value-type (plist :key-type symbol :value-type (repeat regexp)))
  :group 'ps-file-tree)

(defcustom ps/file-tree-current-set "All"
  "Name of the currently active file set (a key in `ps/file-tree-file-sets').
Persisted across restarts via `savehist-additional-variables'. If the
saved name is no longer present in `ps/file-tree-file-sets',
`ps/file-tree--ensure-valid-set' falls back to the first entry there."
  :type 'string
  :group 'ps-file-tree)

(defcustom ps/file-tree-set-applies-to-agenda nil
  "When non-nil, the active file set also restricts the agenda.
On top of filtering the file tree's display, the files hidden by
`ps/file-tree-current-set' (per `ps/file-tree--set-hidden-p') are also
excluded from `org-agenda-files' and from the standalone Conflicts/
Availability buffers (`ps/show-conflicts', `ps/org-show-availability').

Toggle with `ps/file-tree-toggle-agenda-filter'. Persisted across restarts
via `savehist-additional-variables', like `ps/file-tree-current-set'."
  :type 'boolean
  :group 'ps-file-tree)

(defcustom ps/file-tree-root-mode 'single
  "How the Org base directory is turned into treemacs projects.
`single' (the default) makes the base directory itself one project, so files
sitting directly in it are visible and subdirectories nest inside it to any
depth. `subdirs' makes one project per immediate subdirectory instead, giving
each a bold top-level label — but files directly in the base directory are then
not shown anywhere. treemacs does not allow overlapping projects, so these two
modes are mutually exclusive."
  :type '(choice (const :tag "One project for the whole Org directory" single)
                 (const :tag "One project per immediate subdirectory" subdirs))
  :group 'ps-file-tree)

(defcustom ps/file-tree-root-name nil
  "Label for the single root project when `ps/file-tree-root-mode' is `single'.
When nil, the Org base directory's own name is used."
  :type '(choice (const :tag "The directory's own name" nil) string)
  :group 'ps-file-tree)

(defcustom ps/file-tree-use-custom-icons t
  "Whether to use custom category icons in the file tree.
When non-nil (the default), the custom \"ps-file-tree\" icon theme
(`ps/file-tree-icons-apply') is loaded, drawing `<Category>.org' icons from
the Material Symbols font per `ps/material-icons-category-map'. When nil,
treemacs's built-in \"Default\" theme (generic file/folder icons) is used."
  :type 'boolean
  :group 'ps-file-tree)

(defcustom ps/file-tree-name-spacing 0.5
  "Width (in characters) of the gap between a tree icon and its label.
May be fractional, e.g. 0.5 for half a character width."
  :type 'number
  :group 'ps-file-tree)

(defcustom ps/file-tree-icon-ascent 75
  "Vertical alignment of file-tree icons, as a percentage above the baseline.
Passed as the `:ascent' of every file-tree icon image (0-100), root projects
and files alike. Higher values raise the icon relative to its label; lower
values drop it. Icon size is `ps/material-icons-height' (auto-derived from the
font), so this one value stays valid across font sizes."
  :type 'integer
  :group 'ps-file-tree)

(defcustom ps/file-tree-icon-fallback-dir
  (expand-file-name "icons" user-emacs-directory)
  "Directory holding the no-font fallback SVGs.
Used only when `ps/material-icons-font-family' is not installed: files use
`draft.svg' and project roots use `folder_open.svg'/`folder.svg' from here.
Each file is named after the Material Symbols glyph it stands in for."
  :type 'directory
  :group 'ps-file-tree)

;;; Ignore predicate

(defun ps/file-tree--current-set-spec ()
  "Return the plist for `ps/file-tree-current-set', or nil if not found."
  (cdr (assoc ps/file-tree-current-set ps/file-tree-file-sets)))

(defun ps/file-tree--path-matches-any-p (path regexps)
  "Return non-nil if PATH contains a substring match for any of REGEXPS."
  (cl-some (lambda (rx) (string-match-p rx path)) regexps))

(defun ps/file-tree--descendant-included-p (dir include-regexps)
  "Return non-nil if DIR has a descendant whose path matches INCLUDE-REGEXPS.
Recurses into subdirectories; stops at the first match."
  (cl-some
   (lambda (entry)
     (let ((name (file-name-nondirectory entry)))
       (unless (member name '("." ".."))
         (or (ps/file-tree--path-matches-any-p entry include-regexps)
             (and (file-directory-p entry)
                  (ps/file-tree--descendant-included-p entry include-regexps))))))
   (and (file-directory-p dir) (directory-files dir t))))

(defun ps/file-tree--set-hidden-p (absolute-path)
  "Return non-nil if ABSOLUTE-PATH is hidden by the current file set.
Hidden if ABSOLUTE-PATH matches the current set's :exclude regexps, or if
:include is non-nil and neither ABSOLUTE-PATH nor (for directories) any of
its descendants matches an :include regexp."
  (let ((spec (ps/file-tree--current-set-spec)))
    (when spec
      (let ((include (plist-get spec :include))
            (exclude (plist-get spec :exclude)))
        (or
         (and exclude (ps/file-tree--path-matches-any-p absolute-path exclude))
         (and include
              (not (ps/file-tree--path-matches-any-p absolute-path include))
              (not (and (file-directory-p absolute-path)
                        (ps/file-tree--descendant-included-p absolute-path include)))))))))

(defun ps/file-tree-filter-files (files)
  "Return FILES, minus those hidden by the active file set, if enabled.
When `ps/file-tree-set-applies-to-agenda' is nil, FILES is returned
unchanged. Used to scope `org-agenda-files' and the standalone Conflicts/
Availability buffers to the active file set."
  (if ps/file-tree-set-applies-to-agenda
      (seq-remove #'ps/file-tree--set-hidden-p files)
    files))

(defun ps/file-tree--ignored-p (filename absolute-path)
  "Return non-nil if FILENAME/ABSOLUTE-PATH should be hidden from the file tree.
Hidden if FILENAME matches `ps/file-tree-ignored-files', or if
ABSOLUTE-PATH is hidden by the current file set
(`ps/file-tree-file-sets' / `ps/file-tree-current-set')."
  (or (cl-some (lambda (rx) (string-match-p rx filename))
               ps/file-tree-ignored-files)
      (ps/file-tree--set-hidden-p absolute-path)))

;;;###autoload
(defun ps/file-tree-setup-ignore ()
  "Register `ps/file-tree--ignored-p' with treemacs."
  (add-to-list 'treemacs-ignored-file-predicates #'ps/file-tree--ignored-p))

;;; Display-name transformers

(defun ps/file-tree--strip-org-extension (name)
  "Return NAME with a trailing \".org\" extension removed, case-insensitively.
Leaves NAME unchanged if it doesn't end in \".org\" or is exactly \".org\"."
  (if (and (> (length name) (length ".org"))
           (string-match-p "\\.org\\'" (downcase name)))
      (substring name 0 (- (length name) (length ".org")))
    name))

(defun ps/file-tree--spacer ()
  "Return a propertized space `ps/file-tree-name-spacing' characters wide."
  (propertize " " 'display (list 'space :width ps/file-tree-name-spacing)))

(defun ps/file-tree-transform-file-name (name)
  "Transform NAME for display: strip \".org\" and add leading spacing.
Suitable for `treemacs-file-name-transformer'. Does not affect the
underlying path used to open the file."
  (concat (ps/file-tree--spacer)
          (ps/file-tree--strip-org-extension name)))

(defun ps/file-tree-transform-dir-name (name)
  "Transform NAME for display: add leading spacing.
Suitable for `treemacs-directory-name-transformer'."
  (concat (ps/file-tree--spacer) name))

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

;;; File sets

(defun ps/file-tree--ensure-valid-set ()
  "Reset `ps/file-tree-current-set' to the default if it names no set.
Falls back to the first entry of `ps/file-tree-file-sets'."
  (unless (assoc ps/file-tree-current-set ps/file-tree-file-sets)
    (setq ps/file-tree-current-set (car (car ps/file-tree-file-sets)))))

(defun ps/file-tree--refresh ()
  "Re-render the file tree, re-applying the active file set's filters.
Collapses and re-expands every project so each directory is rescanned
against the current `ps/file-tree-current-set' rather than reusing
already-rendered nodes."
  (when-let* ((buf (and (fboundp 'treemacs-get-local-buffer)
                         (treemacs-get-local-buffer))))
    (with-current-buffer buf
      (ps/file-tree-collapse-all)
      (ps/file-tree-expand-all))))

;;;###autoload
(defun ps/file-tree-set-file-set (name)
  "Switch the active file set to NAME and refresh the file tree."
  (interactive
   (list (completing-read "File set: "
                           (mapcar #'car ps/file-tree-file-sets)
                           nil t nil nil ps/file-tree-current-set)))
  (setq ps/file-tree-current-set name)
  (ps/file-tree--ensure-valid-set)
  (ps/file-tree--refresh)
  (when (fboundp 'ps/agenda-files-refresh)
    (ps/agenda-files-refresh))
  (force-mode-line-update t))

(defun ps/file-tree-cycle-file-set ()
  "Switch to the next file set in `ps/file-tree-file-sets', wrapping around."
  (interactive)
  (let* ((names (mapcar #'car ps/file-tree-file-sets))
         (pos (or (cl-position ps/file-tree-current-set names :test #'equal) -1))
         (next (nth (mod (1+ pos) (length names)) names)))
    (ps/file-tree-set-file-set next)))

;;;###autoload
(defun ps/file-tree-toggle-agenda-filter ()
  "Toggle whether the active file set also restricts the agenda.
See `ps/file-tree-set-applies-to-agenda'."
  (interactive)
  (setq ps/file-tree-set-applies-to-agenda
        (not ps/file-tree-set-applies-to-agenda))
  (when (fboundp 'ps/agenda-files-refresh)
    (ps/agenda-files-refresh))
  (force-mode-line-update t)
  (message "File set %s restricts the agenda"
           (if ps/file-tree-set-applies-to-agenda "now" "no longer")))

;;; Mode line

(defun ps/file-tree--modeline-click (event)
  "Show a popup menu of file sets and switch to the one EVENT selects."
  (interactive "e")
  (let* ((names (mapcar #'car ps/file-tree-file-sets))
         (menu (list "File Set" (cons "File Sets" (mapcar (lambda (n) (cons n n)) names))))
         (choice (x-popup-menu event menu)))
    (when choice
      (ps/file-tree-set-file-set choice))))

(defun ps/file-tree--modeline ()
  "Return the file-tree mode line: file-set selector + git-sync status.
The file-set selector is clickable (mouse-1 switches sets).  The git-sync
status (text label + tooltip) is appended when `ps-git-sync' is loaded."
  (let ((fileset
         (propertize (format " %s ▾%s" ps/file-tree-current-set
                             (if ps/file-tree-set-applies-to-agenda " 📅" ""))
                     'face 'mode-line-emphasis
                     'mouse-face 'mode-line-highlight
                     'help-echo "mouse-1: switch file set"
                     'local-map
                     ;; mouse-3 (and mouse-2) are disabled globally by
                     ;; `ps/mode-line--disable-destructive-mouse'.
                     (let ((map (make-sparse-keymap)))
                       (define-key map [mode-line mouse-1] #'ps/file-tree--modeline-click)
                       map)))
        (sync (and (fboundp 'ps/git-sync--modeline)
                   (ps/git-sync--modeline))))
    ;; No separator before the sync status: the `▾' already separates it.
    (if (and sync (> (length sync) 0))
        (concat fileset " " sync)
      fileset)))

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

(defun ps/file-tree--desired-projects (base-dir)
  "Return the (NAME . PATH) projects the tree should show for BASE-DIR.
Per `ps/file-tree-root-mode': one entry for BASE-DIR itself (`single'), named
`ps/file-tree-root-name' or after the directory; or one per immediate
subdirectory (`subdirs')."
  (if (eq ps/file-tree-root-mode 'subdirs)
      (ps/file-tree--list-subdirs base-dir)
    (list (cons (or ps/file-tree-root-name
                    (file-name-nondirectory (directory-file-name base-dir)))
                (directory-file-name base-dir)))))

(defun ps/file-tree-set-projects (base-dir)
  "Make the workspace contain exactly the projects BASE-DIR should show.
Removes any existing project that `ps/file-tree--desired-projects' does not
list, and adds any that are missing."
  (let* ((desired (ps/file-tree--desired-projects base-dir))
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

;;; Mouse click handlers

(defun ps/file-tree--expandable-state-p (state)
  "Return non-nil if STATE supports expand/collapse via `treemacs-toggle-node'."
  (memq state '(root-node-open  root-node-closed
                 dir-node-open   dir-node-closed
                 file-node-open  file-node-closed
                 tag-node-open   tag-node-closed)))

(defun ps/file-tree--toggle-expandable ()
  "Toggle expand/collapse of the node at point, if it has children.
No-op for leaf nodes or when there is no node at point."
  (when-let* ((btn (treemacs-current-button))
              (state (treemacs-button-get btn :state))
              ((ps/file-tree--expandable-state-p state)))
    (treemacs-toggle-node)))

(defun ps/file-tree--node-target (btn)
  "Return (FILE . POS) describing what label-click on BTN should open.
FILE is the path to visit; POS, if non-nil, is a buffer position to jump
to after visiting FILE (used for org headings). Returns nil for nodes
with nothing to open (project roots, plain directories)."
  (pcase (treemacs-button-get btn :state)
    ((or 'file-node-open 'file-node-closed)
     (cons (treemacs-button-get btn :path) nil))
    ((or 'tag-node-open 'tag-node-closed 'tag-node)
     (let ((file (car (treemacs-button-get btn :path)))
           (heading (treemacs-button-get btn :key)))
       (cons file (when (and file heading)
                     (org-find-exact-headline-in-buffer
                      heading (find-file-noselect file))))))
    (_ nil)))

(defun ps/file-tree--target-window ()
  "Return the window where file-tree clicks should open files.
This is the most-recently-used real editor window: never the file tree, a
side window, or a dedicated window.  Excluding side/dedicated windows (rather
than only the tree by identity) ensures files never land in a dedicated window,
where Emacs would split off a new one — which is what made windows multiply and
the target alternate.  Returns nil if no editor window exists yet."
  (let ((cands (seq-filter
                (lambda (w)
                  (and (not (window-dedicated-p w))
                       (not (window-parameter w 'window-side))))
                (window-list nil 'no-mini))))
    (car (sort cands (lambda (a b)
                       (> (window-use-time a) (window-use-time b)))))))

(defun ps/file-tree-visit-file (file &optional pos)
  "Visit FILE in the most-recently-used editor window, then optionally go to POS.
Reuses a window already showing FILE; otherwise the most-recently-used real
editor window (never the file tree or any side/dedicated window), creating one
only if none exists.  With POS, move point there (revealing it in Org buffers).

Note: only consult `get-buffer-window' when FILE actually has a buffer — calling
it on a nil buffer returns the *current* window (the tree at click time), which
would make Emacs split a new window to escape the dedicated side window."
  (let* ((buf (get-file-buffer file))
         (win (or (and buf (get-buffer-window buf))
                  (ps/file-tree--target-window))))
    (when (window-live-p win) (select-window win))
    (find-file file)
    (when pos
      (goto-char pos)
      (when (eq major-mode 'org-mode)
        (if (fboundp 'org-fold-show-context)
            (org-fold-show-context)
          (org-show-context))))))

(defun ps/file-tree--click-label ()
  "Handle a label click on the node at point.
Files and org headings (leaf or intermediate) open in the editor window.
Project roots and plain directories toggle expand/collapse, as before."
  (when-let* ((btn (treemacs-current-button))
              (state (treemacs-button-get btn :state)))
    (if (memq state '(root-node-open root-node-closed
                      dir-node-open  dir-node-closed))
        (treemacs-toggle-node)
      (when-let* ((target (ps/file-tree--node-target btn))
                  (file (car target)))
        (ps/file-tree-visit-file file (cdr target))))))

;;;###autoload
(defun ps/file-tree-click (event)
  "Open or toggle the clicked file-tree node, depending on click target.
Clicking a node's icon toggles expand/collapse (no-op for leaf nodes).
Clicking a file's or org heading's label opens it on the right; clicking
a project root's or directory's label toggles expand/collapse."
  (interactive "e")
  (when (eq (car event) 'mouse-1)
    (let ((posn (event-start event)))
      (select-window (posn-window posn))
      (goto-char (posn-point posn))
      (if (posn-image posn)
          (ps/file-tree--toggle-expandable)
        (ps/file-tree--click-label)))))

;;;###autoload
(defun ps/file-tree-shift-click (event)
  "Toggle expand/collapse of the shift-clicked file-tree node.
No-op for leaf nodes."
  (interactive "e")
  (when (eq (car event) 'S-mouse-1)
    (let ((posn (event-start event)))
      (select-window (posn-window posn))
      (goto-char (posn-point posn))
      (ps/file-tree--toggle-expandable))))

(provide 'ps-file-tree)
;;; ps-file-tree.el ends here
