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
(declare-function treemacs--evade-image "treemacs-core-utils")
(declare-function treemacs-find-in-dom "treemacs-dom")
(declare-function treemacs-dom-node->position "treemacs-dom")
(declare-function org-find-exact-headline-in-buffer "org")
(declare-function hl-line-highlight "hl-line")
(defvar my-org-base-directory)
(defvar treemacs-indentation)
;; Current-line highlight overlays we clear when nothing matches; both are
;; revived by `move-overlay' the next time their owner redraws.
(defvar hl-line-overlay)
(defvar treemacs--fringe-indicator-overlay)
(defvar treemacs-post-buffer-init-hook)
(defvar treemacs-post-refresh-hook)
(defvar treemacs-mode-hook)
;; Optional: git-sync status is appended to the file-tree mode line when the
;; module is loaded.  Guarded with `fboundp', so this file stays standalone.
(declare-function ps/git-sync--modeline "ps-git-sync" ())
(require 'subr-x)

;;; Customization

(defgroup ps-file-tree nil
  "File tree sidebar customization."
  :group 'ps)

(defcustom ps/file-tree-ignored-files
  '("\\`init\\.org\\'" "\\`workspace\\.org\\'"
    "\\`AGENTS\\.md\\'" "\\`CLAUDE\\.md\\'"
    "\\`\\." "\\`elpa\\'" "\\.elc\\'")
  "Regexps matched against file/dir names to hide in the file tree.
A file or directory is hidden if its name matches any regexp here.  The
defaults hide the config files that live beside your notes (`init.org',
`workspace.org', `AGENTS.md', `CLAUDE.md') along with dotfiles and build
artifacts.

This is a *display* list.  What the agenda scans is a separate question,
answered by `ps/org-files-exclude-directories' and
`ps/org-files-exclude-files'."
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
When nil, the Org base directory's own name is used.  Ignored while
`ps/file-tree-hide-root' is on, since the label is then not drawn at all."
  :type '(choice (const :tag "The directory's own name" nil) string)
  :group 'ps-file-tree)

(defcustom ps/file-tree-hide-root t
  "Whether to hide the project root line, showing its contents directly.
The tree then starts at the Org directory's own entries instead of naming
the directory first.  Only meaningful when `ps/file-tree-root-mode' is
`single' -- with `subdirs' the root labels are the section headers and
hiding them would leave nothing to group by."
  :type 'boolean
  :group 'ps-file-tree)

(defcustom ps/file-tree-top-level-spacing 0.35
  "Height of the blank strip drawn above each top-level section.
A fraction of one line: 0.35 is roughly a third of a blank line.  Separates the
tree into sections the way a blank line used to when every top-level folder was
its own project.  Set to 0 for no gap."
  :type 'number
  :group 'ps-file-tree)

(defcustom ps/file-tree-bold-top-level t
  "Whether top-level directories are drawn bold, as section headers.
Takes over from `treemacs-root-face', which no longer applies once
`ps/file-tree-hide-root' removes the root line."
  :type 'boolean
  :group 'ps-file-tree)

(defcustom ps/file-tree-order nil
  "Explicit display order for file-tree entries, or nil for alphabetical.
A list of names.  Entries named before the keyword `:rest' are pinned to the
top in the order given, entries named after it are pinned to the bottom, and
everything unlisted fills the middle alphabetically.  With no `:rest' the
unlisted entries all go last.

A name matches an entry by its file name, its file name without the .org
extension, or its path relative to the Org base directory -- the same matching
`ps/material-icons-folder-map' uses.  For example:

  (setq ps/file-tree-order \\='(:rest \"Current\" \"Vision\"))

Treemacs always renders directories before files, so \"bottom\" means the
bottom of the folder list: files sitting directly in the Org directory still
come after every folder, whatever this says."
  :type '(repeat (choice string (const :rest)))
  :group 'ps-file-tree)

(defcustom ps/file-tree-show-toggle-button t
  "Whether to show the collapse/expand-all button in the tree's header line.
The button sits in the top-right corner and reflects the tree's state: it
offers to collapse while anything is expanded, and to expand once everything
is closed."
  :type 'boolean
  :group 'ps-file-tree)

(defcustom ps/file-tree-follow-current-file t
  "Whether the file tree highlights the file being edited.
The tree stays exactly as it is: nothing is expanded, no project is added or
removed, and the view is not re-rooted.  When the current buffer has no entry
in the tree -- it is not a file, or it is hidden by
`ps/file-tree-ignored-files', filtered out by the active file set, or inside a
collapsed folder -- the highlight is removed rather than left pointing at
something you are not editing.  It comes back as soon as you click or move in
the tree."
  :type 'boolean
  :group 'ps-file-tree)

(defcustom ps/file-tree-follow-delay 0.2
  "Idle seconds to wait before moving the highlight to the current file.
Debounces `ps/file-tree-follow-current-file' so rapid buffer switching does
not repeatedly search the tree."
  :type 'number
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

;;; Display order

(defun ps/file-tree--order-names (path)
  "Return the names PATH may be matched against in `ps/file-tree-order'."
  (let* ((bare (directory-file-name path))
         (name (file-name-nondirectory bare)))
    (delq nil (list name
                    (file-name-sans-extension name)
                    (and (boundp 'my-org-base-directory) my-org-base-directory
                         (file-relative-name bare my-org-base-directory))))))

(defun ps/file-tree--order-position (path)
  "Return PATH\='s index in `ps/file-tree-order', or nil if unlisted."
  (let ((names (ps/file-tree--order-names path))
        (index 0)
        (found nil))
    (dolist (entry ps/file-tree-order found)
      (when (and (null found) (stringp entry) (member entry names))
        (setq found index))
      (setq index (1+ index)))))

(defun ps/file-tree--order-group (position rest)
  "Return the sort group for POSITION given the `:rest' index REST.
0 pinned to the top, 1 unlisted, 2 pinned to the bottom."
  (cond ((null position) 1)
        ((< position rest) 0)
        (t 2)))

(defun ps/file-tree-sort-predicate (f1 f2)
  "Return non-nil if F1 sorts before F2, per `ps/file-tree-order'.
Suitable as a `treemacs-sorting' value, which takes a predicate over absolute
file paths.  With `ps/file-tree-order' unset this is plain alphabetical order,
matching treemacs\='s own `alphabetic-asc' default."
  (let* ((rest (or (cl-position :rest ps/file-tree-order)
                   (length ps/file-tree-order)))
         (p1 (ps/file-tree--order-position f1))
         (p2 (ps/file-tree--order-position f2))
         (g1 (ps/file-tree--order-group p1 rest))
         (g2 (ps/file-tree--order-group p2 rest)))
    (cond ((/= g1 g2) (< g1 g2))
          ((and p1 p2) (< p1 p2))
          (t (string-lessp f1 f2)))))

;;; Ignore predicate

(defun ps/file-tree--current-set-spec ()
  "Return the plist for `ps/file-tree-current-set', or nil if not found."
  (cdr (assoc ps/file-tree-current-set ps/file-tree-file-sets)))

(defun ps/file-tree--path-matches-any-p (path regexps)
  "Return non-nil if PATH contains a substring match for any of REGEXPS."
  (cl-some (lambda (rx) (string-match-p rx path)) regexps))

(defun ps/file-tree--set-shows-file-p (file include exclude)
  "Return non-nil if a file set with INCLUDE/EXCLUDE would show FILE."
  (and (not (and exclude (ps/file-tree--path-matches-any-p file exclude)))
       (or (null include) (ps/file-tree--path-matches-any-p file include))))

(defun ps/file-tree--set-has-visible-file-p (dir include exclude)
  "Return non-nil if DIR contains a file a set with INCLUDE/EXCLUDE would show.
Recurses to any depth and stops at the first hit.  Entries whose name matches
`ps/file-tree-ignored-files' are skipped: they are never drawn, so a folder
holding only those has nothing to show either."
  (cl-some
   (lambda (entry)
     (let ((name (file-name-nondirectory entry)))
       (unless (or (member name '("." ".."))
                   (cl-some (lambda (rx) (string-match-p rx name))
                            ps/file-tree-ignored-files))
         (if (file-directory-p entry)
             (and (not (and exclude
                            (ps/file-tree--path-matches-any-p entry exclude)))
                  (ps/file-tree--set-has-visible-file-p entry include exclude))
           (ps/file-tree--set-shows-file-p entry include exclude)))))
   (and (file-directory-p dir) (directory-files dir t))))

(defun ps/file-tree--set-hidden-p (absolute-path)
  "Return non-nil if ABSOLUTE-PATH is hidden by the current file set.
Hidden if ABSOLUTE-PATH matches the set\='s :exclude regexps; if it is a file
and :include is non-nil but matches none of them; or if it is a *directory the
set has emptied* -- one with no remaining visible file at any depth.

That last rule only applies while a set is actually filtering.  Under a set
with neither :include nor :exclude (the default \"All\"), nothing is hidden, so
a folder that is empty on disk still shows."
  (let ((spec (ps/file-tree--current-set-spec)))
    (when spec
      (let ((include (plist-get spec :include))
            (exclude (plist-get spec :exclude)))
        (cond
         ;; A set that filters nothing hides nothing -- including empty folders.
         ((and (null include) (null exclude)) nil)
         ((and exclude (ps/file-tree--path-matches-any-p absolute-path exclude)) t)
         ((file-directory-p absolute-path)
          (not (ps/file-tree--set-has-visible-file-p absolute-path include exclude)))
         (t (and include
                 (not (ps/file-tree--path-matches-any-p absolute-path include)))))))))

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
Collapses and re-expands every project so each directory is rescanned against
the current `ps/file-tree-current-set' rather than reusing already-rendered
nodes.

Collapses via `treemacs-collapse-all-projects' rather than
`ps/file-tree-collapse-all': the latter deliberately leaves a hidden root
expanded, and a root that never closes is a root whose children are never
re-read -- which would leave the top-level entries frozen as the set changes.
Nothing is visible in between, since the re-expand follows immediately."
  (when-let* ((buf (and (fboundp 'treemacs-get-local-buffer)
                         (treemacs-get-local-buffer))))
    (with-current-buffer buf
      ;; Treemacs announces the collapse in the echo area; here it is an
      ;; implementation detail of switching sets, not something to report.
      (let ((inhibit-message t))
        (treemacs-collapse-all-projects))
      (ps/file-tree-expand-all)
      (ps/file-tree--decorate))))

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
        (ps/file-tree--toggle-matching '(root-node-closed dir-node-closed) t)
        (ps/file-tree--decorate)))))

(defun ps/file-tree-collapse-all ()
  "Recursively collapse every directory in the file tree.
With the root line hidden, the root itself is left expanded -- collapsing it
would hide the whole tree behind a line that isn't drawn, leaving no way to
open it again."
  (interactive)
  (if (ps/file-tree--root-hidden-p)
      (let ((buf (treemacs-get-local-buffer)))
        (when buf
          (with-current-buffer buf
            (ps/file-tree--toggle-matching '(dir-node-open) t)
            (ps/file-tree--decorate))))
    (treemacs-collapse-all-projects)))

;;; Rendering decorations (hidden root, section gaps, bold top level)

;; Treemacs offers no way to render a project without its root line, and no
;; single hook fires after every re-render (`treemacs-toggle-node' fires none at
;; all).  So the look is re-applied by one idempotent pass, `ps/file-tree--decorate',
;; called from `treemacs-post-buffer-init-hook', `treemacs-post-refresh-hook', a
;; buffer-local `post-command-hook', and our own expand/collapse commands.

(defconst ps/file-tree--invisible-spec 'ps/file-tree-root
  "Symbol used to hide the root line, via `buffer-invisibility-spec'.
A dedicated symbol so we never hide text belonging to anything else.")

(defvar-local ps/file-tree--decorated-tick nil
  "Value of `buffer-chars-modified-tick' when this buffer was last decorated.")

(defun ps/file-tree--root-hidden-p ()
  "Return non-nil if the root line should be hidden.
Only the `single' root mode has a root worth hiding."
  (and ps/file-tree-hide-root (not (eq ps/file-tree-root-mode 'subdirs))))

(defun ps/file-tree--hide-line (beg end)
  "Hide the line spanning BEG to END, including any icon image on it.
`invisible' alone leaves an image drawn by a `display' property on screen, so
the property is stripped as well -- the line is going away, and what is left
underneath is a plain space."
  (remove-text-properties beg end '(display nil))
  (put-text-property beg (min (point-max) (1+ end))
                     'invisible ps/file-tree--invisible-spec))

(defconst ps/file-tree--zero-width '(space :width 0)
  "Display spec collapsing a character to nothing.")

(defun ps/file-tree--plain-space-p (pos)
  "Return non-nil if POS holds a space carrying no icon.
Icons are themselves space characters with an image `display' property, so a
bare `char-after' test is not enough to tell indentation from an icon."
  (and (eq (char-after pos) ?\s)
       (let ((d (get-text-property pos 'display)))
         (or (null d) (equal d ps/file-tree--zero-width)))))

(defun ps/file-tree--trim-indent (beg)
  "Collapse one indentation level of leading space on the line at BEG.
With the root line hidden its children still render at depth 1, so every line
carries one level of indent that now leads nowhere.  Collapsing it to zero
width shifts the whole tree left without touching buffer positions.  Uses
`display' rather than `invisible' deliberately: an invisible character at the
start of a line swallows the gap overlay's `before-string' and leaves the line
highlight starting mid-label."
  (let ((stop beg)
        (limit (min (line-end-position) (+ beg treemacs-indentation))))
    (while (and (< stop limit) (ps/file-tree--plain-space-p stop))
      (setq stop (1+ stop)))
    (when (> stop beg)
      (put-text-property beg stop 'display ps/file-tree--zero-width))))

(defun ps/file-tree--add-gap (beg)
  "Draw a blank strip above the line starting at BEG.
An overlay `before-string' rather than a `line-spacing' text property on the
line above: `line-spacing' adds its space *below* that line, so the previous
entry's highlight extends into the gap and it reads as part of that entry
rather than as a separator.  A `before-string' gap belongs to neither line and
stays clear of both highlights."
  (when (and (> ps/file-tree-top-level-spacing 0) (> beg (point-min)))
    (let ((ov (make-overlay beg beg)))
      (overlay-put ov 'ps/file-tree-gap t)
      ;; Treemacs marks the current line with an overlay of its own, whose
      ;; `before-string' carries a fringe bitmap and sits at this same position.
      ;; Emacs orders competing overlay strings by priority, and the fringe
      ;; overlay already exists by the time this one is made -- without a lower
      ;; priority here the bitmap is drawn on the blank strip instead of on the
      ;; line, which reads as a stray mark floating above the entry.
      (overlay-put ov 'priority -100)
      (overlay-put ov 'before-string
                   (propertize "\n" 'face
                               (list :height ps/file-tree-top-level-spacing))))))

(defun ps/file-tree--decorate ()
  "Re-apply the file tree's structural styling to the current buffer.
Hides the root line (`ps/file-tree-hide-root') and the now-pointless
indentation level it leaves behind, draws a gap above each top-level section
\(`ps/file-tree-top-level-spacing') and renders top-level directories bold
\(`ps/file-tree-bold-top-level').  Idempotent: safe to call after any render.
Changes no characters, so it does not bump `buffer-chars-modified-tick' and
cannot retrigger itself."
  (when (derived-mode-p 'treemacs-mode)
    ;; Same effect as treemacs's own `treemacs-with-writable-buffer', spelled
    ;; out so this file needs no treemacs macro at load time.
    (let ((buffer-read-only nil)
          (hide-root (ps/file-tree--root-hidden-p)))
      (remove-overlays (point-min) (point-max) 'ps/file-tree-gap t)
      ;; Nothing else in a file tree uses `invisible' -- treemacs sets it only on
      ;; the hidden parent of a variadic extension node, and there are none here.
      (remove-text-properties (point-min) (point-max) '(invisible nil))
      (save-excursion
        (let ((pos (next-button (point-min) t))
              (seen-top-file nil))
          (while pos
            (goto-char pos)
            (let ((state (treemacs-button-get pos :state))
                  (depth (treemacs-button-get pos :depth))
                  (beg (line-beginning-position))
                  (end (line-end-position)))
              (cond
               ((memq state '(root-node-open root-node-closed))
                (when hide-root (ps/file-tree--hide-line beg end)))
               ((eql depth 1)
                (let ((dir (memq state '(dir-node-open dir-node-closed))))
                  ;; A gap before every top-level directory -- including the
                  ;; first, so the tree doesn't start flush against the window
                  ;; edge -- and one before the block of loose files that
                  ;; follows them.  Treemacs renders every directory before any
                  ;; file, so those files are a single block at the end.
                  (when (or dir (not seen-top-file))
                    (ps/file-tree--add-gap beg))
                  (when (and dir ps/file-tree-bold-top-level
                             (not (get-text-property beg 'ps/file-tree-bold)))
                    (add-face-text-property beg end '(:weight bold))
                    (put-text-property beg (1+ beg) 'ps/file-tree-bold t))
                  (unless dir (setq seen-top-file t)))))
              (when (and hide-root (not (memq state '(root-node-open root-node-closed))))
                (ps/file-tree--trim-indent beg)))
            (setq pos (next-button pos)))))
      (setq ps/file-tree--decorated-tick (buffer-chars-modified-tick)))))

(defun ps/file-tree--decorate-buffer ()
  "Decorate the file tree buffer, wherever it is called from."
  (when-let* ((buf (and (fboundp 'treemacs-get-local-buffer)
                        (treemacs-get-local-buffer))))
    (with-current-buffer buf
      (ps/file-tree--decorate))))

(defun ps/file-tree--decorate-if-changed ()
  "Re-decorate the tree if its text changed since the last pass.
Cheap enough for `post-command-hook': the guard is a tick comparison, and
text-property edits do not bump `buffer-chars-modified-tick'."
  (unless (eql ps/file-tree--decorated-tick (buffer-chars-modified-tick))
    (ps/file-tree--decorate)))

;;;###autoload
(defun ps/file-tree-decorations-setup ()
  "Install the file tree's rendering decorations.
Call once from the treemacs `:config' block.  The buffer-local
`post-command-hook' entry is what catches interactive expand/collapse, which
treemacs itself reports through no hook."
  (add-hook 'treemacs-post-buffer-init-hook #'ps/file-tree--decorate)
  (add-hook 'treemacs-post-refresh-hook #'ps/file-tree--decorate-buffer)
  (add-hook 'treemacs-mode-hook #'ps/file-tree--decorations-mode-setup))

(defun ps/file-tree--decorations-mode-setup ()
  "Buffer-local setup for the tree's decorations, for `treemacs-mode-hook'."
  (add-to-invisibility-spec ps/file-tree--invisible-spec)
  (add-hook 'post-command-hook #'ps/file-tree--decorate-if-changed nil t))

;;; Follow the current file

;; Unlike `treemacs-follow-mode', this never expands a node and never changes
;; what the tree shows: it is a pure "is this file already on screen, and where"
;; lookup followed by a point move.

(defvar ps/file-tree--follow-timer nil
  "Pending idle timer for `ps/file-tree--follow'.")

(defun ps/file-tree--visible-node-position (path)
  "Return the buffer position of PATH in the current tree buffer, or nil.
Pure: nil means \"not rendered right now\", and nothing is expanded to make it
so.  `treemacs-dom' holds an entry only for nodes that are actually drawn, so
the lookup doubles as the visibility test.  Deliberately avoids
`treemacs-find-visible-node', which despite its name falls back to
`treemacs-find-node' -- and that expands ancestors -- whenever the dom node's
position marker has not been cached yet."
  (let ((canonical (treemacs-canonical-path path)))
    (when-let* ((dom-node (treemacs-find-in-dom canonical)))
      (let ((marker (treemacs-dom-node->position dom-node)))
        (if (and marker (marker-position marker))
            (marker-position marker)
          ;; No cached marker -- the usual case for a leaf file, since treemacs
          ;; fills the slot in lazily.  Find the button by scanning, which is
          ;; what keeps this from having to expand anything.
          (save-excursion
            (let ((pos (next-button (point-min) t))
                  (found nil))
              (while (and pos (not found))
                (when (equal (treemacs-button-get pos :path) canonical)
                  (setq found (copy-marker pos nil)))
                (setq pos (next-button pos)))
              (and found (marker-position found)))))))))

(defun ps/file-tree--clear-highlight (buf)
  "Remove the current-line highlight from the file tree buffer BUF.
Deletes the `hl-line' overlay and treemacs\='s fringe indicator rather than
switching their modes off: both are re-created by `move-overlay' the moment
their owner redraws, so the highlight returns by itself as soon as you touch
the tree."
  (with-current-buffer buf
    (when (and (boundp 'hl-line-overlay) hl-line-overlay)
      (delete-overlay hl-line-overlay))
    (when (and (boundp 'treemacs--fringe-indicator-overlay)
               treemacs--fringe-indicator-overlay)
      (delete-overlay treemacs--fringe-indicator-overlay))))

(defun ps/file-tree--follow ()
  "Point the file tree's highlight at the current buffer's file.
Clears the highlight instead when that file has no entry in the tree."
  (setq ps/file-tree--follow-timer nil)
  (when (and ps/file-tree-follow-current-file
             (not (minibufferp))
             (not (derived-mode-p 'treemacs-mode)))
    (let* ((buf (and (fboundp 'treemacs-get-local-buffer)
                     (treemacs-get-local-buffer)))
           (win (and buf (treemacs-get-local-window)))
           (file (and win buffer-file-name))
           (pos (and file (file-exists-p file)
                     (with-current-buffer buf
                       (ps/file-tree--visible-node-position file)))))
      (when win
        (if pos
            ;; `with-selected-window' rather than `select-window': focus stays
            ;; in the buffer being edited.  `set-window-point' makes the
            ;; position stick, and `hl-line-highlight' redraws the highlight.
            (with-selected-window win
              (goto-char pos)
              (when (fboundp 'treemacs--evade-image) (treemacs--evade-image))
              (hl-line-highlight)
              (set-window-point win (point)))
          (ps/file-tree--clear-highlight buf))))))

(defun ps/file-tree--follow-schedule ()
  "Schedule a debounced `ps/file-tree--follow' run."
  (when (and ps/file-tree-follow-current-file
             (not ps/file-tree--follow-timer))
    (setq ps/file-tree--follow-timer
          (run-with-idle-timer ps/file-tree-follow-delay nil
                               #'ps/file-tree--follow))))

;;;###autoload
(defun ps/file-tree-follow-setup ()
  "Make the file tree's highlight track the buffer being edited.
Call once from the treemacs `:config' block; `ps/file-tree-follow-current-file'
is re-checked on every run, so it can be toggled at any time."
  (add-hook 'buffer-list-update-hook #'ps/file-tree--follow-schedule))

;;; Collapse / expand toggle button

(defconst ps/file-tree--collapse-icon "unfold_less"
  "Material Symbols name for the \"collapse everything\" button.")
(defconst ps/file-tree--expand-icon "unfold_more"
  "Material Symbols name for the \"expand everything\" button.")

(defun ps/file-tree--any-expanded-p ()
  "Return non-nil if any directory in the tree is currently expanded.
Stops at the first one, so this is cheap enough to call from redisplay."
  (save-excursion
    (let ((pos (next-button (point-min) t))
          (found nil))
      (while (and pos (not found))
        (when (eq (treemacs-button-get pos :state) 'dir-node-open)
          (setq found t))
        (setq pos (next-button pos)))
      found)))

;;;###autoload
(defun ps/file-tree-toggle-all ()
  "Collapse the whole tree, or expand it if it is already fully collapsed."
  (interactive)
  (when-let* ((buf (and (fboundp 'treemacs-get-local-buffer)
                        (treemacs-get-local-buffer))))
    (with-current-buffer buf
      (if (ps/file-tree--any-expanded-p)
          (ps/file-tree-collapse-all)
        (ps/file-tree-expand-all)))))

(defun ps/file-tree--toggle-button-click (event)
  "Toggle the tree open or closed, for a click on EVENT."
  (interactive "e")
  (ignore event)
  (ps/file-tree-toggle-all))

(defun ps/file-tree--button-glyph (name fallback)
  "Return an icon string for Material Symbols NAME, or FALLBACK text.
Resolved through `ps/material-icons-image' only when that module happens to be
loaded, so this file keeps working on its own."
  (or (and (fboundp 'ps/material-icons-image)
           (when-let* ((image (ps/material-icons-image
                               name ps/file-tree-icon-ascent)))
             (propertize " " 'display image)))
      fallback))

(defun ps/file-tree--header-line ()
  "Return the tree's header line: a right-aligned collapse/expand button."
  (when ps/file-tree-show-toggle-button
    (let* ((expanded (ps/file-tree--any-expanded-p))
           (glyph (ps/file-tree--button-glyph
                   (if expanded
                       ps/file-tree--collapse-icon
                     ps/file-tree--expand-icon)
                   (if expanded "-" "+")))
           (map (let ((m (make-sparse-keymap)))
                  (define-key m [header-line mouse-1]
                              #'ps/file-tree--toggle-button-click)
                  m)))
      (list
       ;; Push the button to the right edge, leaving room for the glyph itself.
       (propertize " " 'display '(space :align-to (- right 3)))
       (propertize glyph
                   'mouse-face 'mode-line-highlight
                   'help-echo (if expanded
                                  "mouse-1: collapse everything"
                                "mouse-1: expand everything")
                   'local-map map)))))

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
    (ps/file-tree-expand-all)
    (ps/file-tree--decorate-buffer)))

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
