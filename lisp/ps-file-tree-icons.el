;;; ps-file-tree-icons.el --- Category icons for the file tree -*- lexical-binding: t; -*-

(require 'ps-file-tree)

;; Provided by treemacs/ht; declared here so this file loads (and its pure
;; functions are testable) without treemacs installed.
(declare-function treemacs-create-theme "treemacs-themes")
(declare-function treemacs-load-theme "treemacs-themes")
(declare-function treemacs-theme->gui-icons "treemacs-themes")
(declare-function treemacs-theme->tui-icons "treemacs-themes")
(declare-function ht-set! "ht")
(defvar treemacs--current-theme)

;; Icon basenames reserved for structural purposes (root icons, per-directory
;; file icon overrides, fallback) rather than <Category>.org matching.
(defconst ps/file-tree-icons--structural-names
  '("FolderOpen" "FolderClosed" "Current" "Vision" "File")
  "Icon basenames excluded from per-category <Category>.org registration.")

(defun ps/file-tree-icons--build-alist (icon-dir)
  "Build a list of (CATEGORY . FILE) for SVGs in ICON-DIR.
Mirrors `ps/agenda-icons--build-alist'. Returns nil when ICON-DIR is missing
or has no SVGs."
  (let ((icons '()))
    (when (file-directory-p icon-dir)
      (dolist (file (directory-files icon-dir t "\\.svg$"))
        (push (cons (file-name-base file) file) icons)))
    icons))

(defun ps/file-tree-icons--merge (icon-dirs)
  "Merge ICON-DIRS (lowest to highest priority) into one alist, last wins."
  (let (merged)
    (dolist (dir icon-dirs)
      (dolist (entry (ps/file-tree-icons--build-alist dir))
        (setq merged (cons entry (assoc-delete-all (car entry) merged)))))
    (nreverse merged)))

(defun ps/file-tree-icons--register (category file)
  "Register FILE as the file-tree icon for CATEGORY.org files.
Unlike `treemacs-create-icon', this preserves FILE's natural size and
aspect ratio instead of stretching it to a square."
  (let ((ext (downcase (concat category ".org")))
        (gui-icon (propertize " " 'display
                               (create-image file nil nil :ascent 'center))))
    (ht-set! (treemacs-theme->gui-icons treemacs--current-theme) ext gui-icon)
    (ht-set! (treemacs-theme->tui-icons treemacs--current-theme) ext "")))

(defun ps/file-tree-icons--image-for (merged name)
  "Return a propertized icon image for icon NAME in MERGED, or nil."
  (let ((file (cdr (assoc name merged))))
    (when file
      (propertize " " 'display (create-image file nil nil :ascent 'center)))))

(defun ps/file-tree-icons--register-root-icons (merged)
  "Set the icon shown before every top-level project root label.
Uses `FolderOpen'/`FolderClosed' from MERGED for `root-open'/`root-closed',
the same for every project. Falls back to no icon if not present.
Appends the standard icon-to-label spacer after each icon."
  (dolist (pair '((root-open . "FolderOpen") (root-closed . "FolderClosed")))
    (let* ((icon (ps/file-tree-icons--image-for merged (cdr pair)))
           (image (if icon (concat icon (ps/file-tree--spacer)) "")))
      (ht-set! (treemacs-theme->gui-icons treemacs--current-theme) (car pair) image)
      (ht-set! (treemacs-theme->tui-icons treemacs--current-theme) (car pair) ""))))

(defun ps/file-tree-icons--register-file (file image)
  "Register IMAGE as the file-tree icon for FILE (matched by exact name)."
  (let ((key (downcase (file-name-nondirectory file))))
    (ht-set! (treemacs-theme->gui-icons treemacs--current-theme) key image)
    (ht-set! (treemacs-theme->tui-icons treemacs--current-theme) key "")))

(defun ps/file-tree-icons--register-dir-icon (dir image)
  "Register IMAGE as the file-tree icon for every .org file directly in DIR."
  (when (and image (file-directory-p dir))
    (dolist (file (directory-files dir nil "\\.org\\'"))
      (ps/file-tree-icons--register-file file image))))

(defun ps/file-tree-icons--register-fallback (dir merged image)
  "Register IMAGE as the file-tree icon for .org files in DIR with no
category-specific icon in MERGED (i.e. no `<Category>.svg' matching
`<Category>.org')."
  (when (and image (file-directory-p dir))
    (dolist (file (directory-files dir nil "\\.org\\'"))
      (unless (assoc (file-name-base file) merged)
        (ps/file-tree-icons--register-file file image)))))

(defun ps/file-tree-icons-apply (icon-dirs base-dir)
  "Register a treemacs theme mapping <Category>.org files to category SVGs,
set the icon shown before top-level project root labels, and set up
directory-based file icon overrides for `Current'/`Vision'/`Areas' under
BASE-DIR. ICON-DIRS is a list of directories ordered from lowest to highest
priority, as in `ps/agenda-icons-apply'. Does nothing if treemacs isn't
available."
  (when (require 'treemacs nil t)
    (let ((merged (ps/file-tree-icons--merge icon-dirs)))
      (treemacs-create-theme "ps-file-tree"
        :extends "Default"
        :config
        (progn
          (dolist (entry merged)
            (unless (member (car entry) ps/file-tree-icons--structural-names)
              (ps/file-tree-icons--register (car entry) (cdr entry))))
          (ps/file-tree-icons--register-root-icons merged)
          (ps/file-tree-icons--register-dir-icon
           (expand-file-name "Current" base-dir)
           (ps/file-tree-icons--image-for merged "Current"))
          (ps/file-tree-icons--register-dir-icon
           (expand-file-name "Vision" base-dir)
           (ps/file-tree-icons--image-for merged "Vision"))
          (ps/file-tree-icons--register-fallback
           (expand-file-name "Areas" base-dir) merged
           (ps/file-tree-icons--image-for merged "File"))))
      (treemacs-load-theme "ps-file-tree"))))

(provide 'ps-file-tree-icons)
;;; ps-file-tree-icons.el ends here
