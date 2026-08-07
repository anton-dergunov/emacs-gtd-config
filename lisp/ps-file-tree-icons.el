;;; ps-file-tree-icons.el --- Category icons for the file tree -*- lexical-binding: t; -*-

;;; Commentary:

;; Builds a treemacs icon theme for the Org file tree.  Icon selection is split
;; in two: `ps/file-tree-icons--collect' is a pure function mapping treemacs
;; icon keys to Material Symbols glyph *names*, and the apply step turns each
;; name into an image (a font glyph, or the same-named SVG in
;; `ps/file-tree-icon-fallback-dir' when the font is missing) and registers it.
;;
;; The collected keys cover every kind of node treemacs draws:
;;   - `root-open' / `root-closed'  -- project root labels
;;   - `dir-open' / `dir-closed'    -- the generic directory icons
;;   - "<dir>-open" / "<dir>-closed" -- a directory with its own mapped icon
;;   - "<file>.org"                 -- a file
;;   - "org"                        -- the extension key every other .org node
;;                                     falls back to
;; Directories and files are found by walking the whole Org tree, so nesting to
;; any depth works.

;;; Code:

(require 'cl-lib)
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
  "Material Symbols name for the closed-folder icon (project roots and dirs).")
(defconst ps/file-tree-icons--folder-open "folder_open"
  "Material Symbols name for the open-folder icon (project roots and dirs).")
(defconst ps/file-tree-icons--file "draft"
  "Material Symbols name for the generic file icon.")

;;; Tree walk

(defun ps/file-tree-icons--ignored-name-p (name)
  "Return non-nil if NAME matches `ps/file-tree-ignored-files'."
  (cl-some (lambda (rx) (string-match-p rx name)) ps/file-tree-ignored-files))

(defun ps/file-tree-icons--walk (dir)
  "Return (DIRS . FILES) for everything under DIR, recursively.
DIRS is every subdirectory and FILES every .org file, as absolute paths.
Names matching `ps/file-tree-ignored-files' are skipped along with their
contents, so the walk sees exactly what the file tree displays."
  (let ((dirs '()) (files '()))
    (when (file-directory-p dir)
      (dolist (entry (directory-files dir t))
        (let ((name (file-name-nondirectory entry)))
          (unless (or (member name '("." ".."))
                      (ps/file-tree-icons--ignored-name-p name))
            (if (file-directory-p entry)
                (let ((sub (ps/file-tree-icons--walk entry)))
                  (setq dirs (append (car sub) (cons entry dirs)))
                  (setq files (append (cdr sub) files)))
              (when (string-match-p "\\.org\\'" name)
                (push entry files)))))))
    (cons (sort dirs #'string<) (sort files #'string<))))

(defun ps/file-tree-icons--folder-icon (dir base-dir map)
  "Return MAP's icon name for DIR, or nil.
DIR matches an entry keyed by its own name or by its path relative to
BASE-DIR, so \"older\" matches every folder of that name and \"ML/older\"
matches just the one."
  (let ((name (file-name-nondirectory (directory-file-name dir)))
        (relative (directory-file-name (file-relative-name dir base-dir))))
    (cdr (or (assoc name map) (assoc relative map)))))

;;; Icon-key collection (pure)

(defun ps/file-tree-icons--collect (base-dir)
  "Return an alist of (KEY . GLYPH-NAME) for every icon the tree should draw.
KEY is a treemacs icon key: one of the symbols `root-open', `root-closed',
`dir-open', `dir-closed'; a \"<dir>-open\"/\"<dir>-closed\" string for a
directory with its own mapped icon; or a downcased file name.  Takes no
Emacs/treemacs state beyond the icon maps and the contents of BASE-DIR, so it
is ERT-testable in isolation."
  (let* ((walk (ps/file-tree-icons--walk base-dir))
         (dirs (car walk))
         (files (cdr walk))
         (keys '())
         (add (lambda (key name) (push (cons key name) keys))))
    ;; 1. Structural icons.  The root may carry its own mapped icon; every
    ;;    other directory falls back to the generic folder glyphs.
    (let ((root (ps/file-tree-icons--folder-icon
                 base-dir base-dir ps/material-icons-folder-map)))
      (funcall add 'root-open (or root ps/file-tree-icons--folder-open))
      (funcall add 'root-closed (or root ps/file-tree-icons--folder-closed)))
    (funcall add 'dir-open ps/file-tree-icons--folder-open)
    (funcall add 'dir-closed ps/file-tree-icons--folder-closed)
    ;; 2. Per-directory icons, at any depth.  Sorted paths mean a nested entry
    ;;    is registered after (and so wins over) its parent's.
    (dolist (dir dirs)
      (when-let ((icon (ps/file-tree-icons--folder-icon
                        dir base-dir ps/material-icons-folder-map)))
        (let ((key (downcase (file-name-nondirectory (directory-file-name dir)))))
          (funcall add (concat key "-open") icon)
          (funcall add (concat key "-closed") icon))))
    ;; 3. Files, weakest first: the extension key, the generic glyph, then
    ;;    per-category icons, then whole-folder contents icons.
    ;;    treemacs resolves a file icon as filename -> extension -> its own
    ;;    fallback, so the extension key is what every .org node without a
    ;;    filename key of its own lands on: one created or renamed after this
    ;;    walk, or one whose file is already gone.  Without it treemacs draws
    ;;    its own `org' image, which matches neither our icon height/ascent nor
    ;;    the icon-to-label spacer.
    (funcall add "org" ps/file-tree-icons--file)
    (dolist (file files)
      (funcall add (downcase (file-name-nondirectory file))
               ps/file-tree-icons--file))
    (dolist (entry ps/material-icons-category-map)
      (funcall add (downcase (concat (car entry) ".org")) (cdr entry)))
    (dolist (dir dirs)
      (when-let ((icon (ps/file-tree-icons--folder-icon
                        dir base-dir ps/material-icons-folder-contents-map)))
        (dolist (file files)
          (when (string-prefix-p (file-name-as-directory dir) file)
            (funcall add (downcase (file-name-nondirectory file)) icon)))))
    (nreverse keys)))

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

(defun ps/file-tree-icons--apply-keys (keys renderer)
  "Register KEYS, an alist of (KEY . GLYPH-NAME), rendering each via RENDERER.
RENDERER takes a glyph name and returns an icon string, or nil when it cannot
draw that name — such keys are left alone so treemacs's own icon shows."
  (dolist (entry keys)
    (when-let ((icon (funcall renderer (cdr entry))))
      (ps/file-tree-icons--set (car entry) icon))))

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

;;; Public entry point

(defun ps/file-tree-icons-apply (base-dir)
  "Register a treemacs theme drawing file-tree icons from Material Symbols.
Walks BASE-DIR and icons every directory and .org file in it, at any depth:
files per `ps/material-icons-category-map', directories per
`ps/material-icons-folder-map', whole folders of files per
`ps/material-icons-folder-contents-map', and the generic folder/file glyphs
for everything else.  When the Material Symbols font is unavailable, the same
icon names are drawn from the same-named SVGs in
`ps/file-tree-icon-fallback-dir' (only the structural glyphs ship with the
repo, so mapped icons simply keep treemacs's default there).  Does nothing if
treemacs isn't available."
  (when (require 'treemacs nil t)
    (let ((keys (ps/file-tree-icons--collect base-dir))
          (font (ps/material-icons-available-p)))
      (treemacs-create-theme "ps-file-tree"
        :extends "Default"
        :config
        (progn
          (ps/file-tree-icons--apply-keys
           keys (if font
                    #'ps/file-tree-icons--glyph
                  #'ps/file-tree-icons--fallback-svg))
          (when font (ps/file-tree-icons--override-tag-icons)))))
    (treemacs-load-theme "ps-file-tree")))

(provide 'ps-file-tree-icons)
;;; ps-file-tree-icons.el ends here
