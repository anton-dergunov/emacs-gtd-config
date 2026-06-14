;;; ps-file-tree-icons.el --- Category icons for the file tree -*- lexical-binding: t; -*-

(require 'ps-file-tree)
(require 'ps-material-icons)

;; Provided by treemacs/ht; declared here so this file loads (and its pure
;; functions are testable) without treemacs installed.
(declare-function treemacs-create-theme "treemacs-themes")
(declare-function treemacs-load-theme "treemacs-themes")
(declare-function treemacs-theme->gui-icons "treemacs-themes")
(declare-function treemacs-theme->tui-icons "treemacs-themes")
(declare-function ht-set! "ht")
(declare-function ht-get "ht")
(defvar treemacs--current-theme)

;; Material Symbols names for the structural icons (hardcoded — not part of the
;; user-facing category map).
(defconst ps/file-tree-icons--folder-closed "folder"
  "Material Symbols name for the closed-folder (project root) icon.")
(defconst ps/file-tree-icons--folder-open "folder_open"
  "Material Symbols name for the open-folder (project root) icon.")
(defconst ps/file-tree-icons--file "draft"
  "Material Symbols name for the generic file icon.")

;;; Image / icon-string builders

(defun ps/file-tree-icons--file-image (file &optional mask)
  "Image for an SVG/PNG FILE at the shared icon height/ascent.
Height is `ps/material-icons--pixel-height' (font-derived); `:ascent' is
`ps/file-tree-icon-ascent'. MASK is the image `:mask' (e.g. \\='heuristic)."
  (create-image file nil nil
                :height (ps/material-icons--pixel-height)
                :ascent ps/file-tree-icon-ascent
                :mask mask))

(defun ps/file-tree-icons--icon-string (image)
  "Wrap IMAGE in a propertized space plus the standard icon-to-label spacer."
  (concat (propertize " " 'display image) (ps/file-tree--spacer)))

(defun ps/file-tree-icons--glyph (name)
  "Return a file-tree icon string for Material Symbols NAME, or nil if unknown."
  (when-let ((image (ps/material-icons-image name ps/file-tree-icon-ascent)))
    (ps/file-tree-icons--icon-string image)))

(defun ps/file-tree-icons--fallback-svg (basename &optional mask)
  "Return a file-tree icon string for BASENAME.svg in the fallback dir, or nil."
  (let ((file (expand-file-name (concat basename ".svg")
                                ps/file-tree-icon-fallback-dir)))
    (when (file-readable-p file)
      (ps/file-tree-icons--icon-string
       (ps/file-tree-icons--file-image file mask)))))

;;; treemacs theme registration

(defun ps/file-tree-icons--set (key icon)
  "Register ICON (a string) under KEY in the current theme's GUI+TUI icons."
  (ht-set! (treemacs-theme->gui-icons treemacs--current-theme) key (or icon ""))
  (ht-set! (treemacs-theme->tui-icons treemacs--current-theme) key ""))

(defun ps/file-tree-icons--register-file (file icon)
  "Register ICON as the file-tree icon for FILE (matched by exact name)."
  (when icon
    (ps/file-tree-icons--set (downcase (file-name-nondirectory file)) icon)))

(defun ps/file-tree-icons--register-dir-files (dir icon)
  "Register ICON as the file-tree icon for every .org file directly in DIR."
  (when (and icon (file-directory-p dir))
    (dolist (file (directory-files dir nil "\\.org\\'"))
      (ps/file-tree-icons--register-file file icon))))

(defun ps/file-tree-icons--register-root-icons (open-icon closed-icon)
  "Set the icons shown before every top-level project root label."
  (ps/file-tree-icons--set 'root-open open-icon)
  (ps/file-tree-icons--set 'root-closed closed-icon))

(defun ps/file-tree-icons--register-categories ()
  "Register a glyph icon for each `(BASENAME . NAME)' in the category map."
  (dolist (entry ps/material-icons-category-map)
    (ps/file-tree-icons--register-file
     (concat (car entry) ".org")
     (ps/file-tree-icons--glyph (cdr entry)))))

(defun ps/file-tree-icons--register-folders (base-dir)
  "Register whole-folder glyph icons per `ps/material-icons-folder-map'.
Each `(FOLDER . NAME)' icons every .org file directly in BASE-DIR/FOLDER."
  (dolist (entry ps/material-icons-folder-map)
    (ps/file-tree-icons--register-dir-files
     (expand-file-name (car entry) base-dir)
     (ps/file-tree-icons--glyph (cdr entry)))))

(defun ps/file-tree-icons--register-file-fallback (base-dir)
  "Register the generic `draft' glyph for unmapped .org files under BASE-DIR.
Applies to direct .org files of the `Areas' subdirectory that have no entry in
`ps/material-icons-category-map'."
  (when-let* ((icon (ps/file-tree-icons--glyph ps/file-tree-icons--file))
              (dir (expand-file-name "Areas" base-dir))
              ((file-directory-p dir)))
    (dolist (file (directory-files dir nil "\\.org\\'"))
      (unless (assoc (file-name-base file) ps/material-icons-category-map)
        (ps/file-tree-icons--register-file file icon)))))

(defun ps/file-tree-icons--override-tag-icons ()
  "Restyle treemacs's tag icons (org headings) to match file/dir icons.
By default tag icons differ from file/dir icons in two ways: a wider,
full-width gap between icon and label, and a different vertical alignment.
For each tag icon in the current theme, re-create its image at our height
and ascent and replace treemacs's trailing full-width space with the
standard `ps/file-tree--spacer', so tags match files exactly. Skips any
icon that has no image (e.g. a TUI fallback string)."
  (let ((gui-icons (treemacs-theme->gui-icons treemacs--current-theme)))
    (dolist (key '(tag-leaf tag-open tag-closed))
      (let* ((icon (ht-get gui-icons key))
             (image (and (stringp icon) (> (length icon) 0)
                         (get-text-property 0 'display icon)))
             (file (and (eq (car-safe image) 'image)
                        (image-property image :file))))
        (when file
          (ht-set! gui-icons key
                   (ps/file-tree-icons--icon-string
                    (ps/file-tree-icons--file-image file 'heuristic))))))))

;;; Font-missing fallback (uses the three SVGs in `ps/file-tree-icon-fallback-dir')

(defun ps/file-tree-icons--apply-fallback (base-dir)
  "Register the no-font fallback icons: folder SVGs for roots, File for files."
  (ps/file-tree-icons--register-root-icons
   (ps/file-tree-icons--fallback-svg "FolderOpen")
   (ps/file-tree-icons--fallback-svg "FolderClosed"))
  (when-let ((file-icon (ps/file-tree-icons--fallback-svg "File")))
    (dolist (sub '("Areas" "Current" "Vision"))
      (ps/file-tree-icons--register-dir-files
       (expand-file-name sub base-dir) file-icon))))

;;; Public entry point

(defun ps/file-tree-icons-apply (base-dir)
  "Register a treemacs theme drawing file-tree icons from Material Symbols.
Maps each `<Category>.org' under BASE-DIR to its glyph
(`ps/material-icons-category-map'), icons the `Current'/`Vision' folders
wholesale (`ps/material-icons-folder-map'), uses the generic `draft' glyph for
unmapped files, and the folder glyphs for project roots. When the Material
Symbols font is unavailable, falls back to the SVGs in
`ps/file-tree-icon-fallback-dir'. Does nothing if treemacs isn't available."
  (when (require 'treemacs nil t)
    (treemacs-create-theme "ps-file-tree"
      :extends "Default"
      :config
      (if (ps/material-icons-available-p)
          (progn
            (ps/file-tree-icons--register-categories)
            (ps/file-tree-icons--register-folders base-dir)
            (ps/file-tree-icons--register-file-fallback base-dir)
            (ps/file-tree-icons--register-root-icons
             (ps/file-tree-icons--glyph ps/file-tree-icons--folder-open)
             (ps/file-tree-icons--glyph ps/file-tree-icons--folder-closed))
            (ps/file-tree-icons--override-tag-icons))
        (ps/file-tree-icons--apply-fallback base-dir)))
    (treemacs-load-theme "ps-file-tree")))

(provide 'ps-file-tree-icons)
;;; ps-file-tree-icons.el ends here
