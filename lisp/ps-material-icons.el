;;; ps-material-icons.el --- Render Material Symbols glyphs as icons -*- lexical-binding: t; -*-

;;; Commentary:
;; Renders Google's Material Symbols font glyphs as Emacs images, without any
;; per-icon SVG files.  A glyph is drawn into an in-memory SVG `<text>' element
;; (mirroring the attributes the old icon SVGs used) and turned into an image via
;; `create-image', so it goes through the same pipeline — and gets the same
;; sizing/alignment — as a normal image icon.
;;
;; Icons are named by their Material Symbols name (e.g. "edit_square" or
;; "Edit Square"); names are resolved to codepoints from the official
;; `*.codepoints' list shipped alongside this file.  Two declarative maps drive
;; the agenda and file-tree icons:
;;   `ps/material-icons-category-map' — <Category>.org basename -> icon name
;;   `ps/material-icons-folder-map'   — folder name             -> icon name
;;
;;; Code:

(require 'subr-x)

(defgroup ps-material-icons nil
  "Material Symbols font icons."
  :group 'ps)

(defcustom ps/material-icons-font-family "Material Symbols Outlined"
  "Font family used to render Material Symbols glyphs.
Used both to detect availability (`ps/material-icons-available-p') and as the
`font-family' of the generated SVG.  Install the static \"Regular\" instance of
this family for the glyphs to render; otherwise icons fall back per consumer."
  :type 'string
  :group 'ps-material-icons)

(defcustom ps/material-icons-color "#5f6368"
  "Fill color for Material Symbols glyph icons."
  :type 'string
  :group 'ps-material-icons)

(defcustom ps/material-icons-codepoints-file
  (expand-file-name "icons/material-symbols.codepoints" user-emacs-directory)
  "Path to the Material Symbols codepoints list (NAME CODEPOINT per line).
The same list applies to the Outlined, Rounded, and Sharp styles."
  :type 'file
  :group 'ps-material-icons)

(defcustom ps/material-icons-height 'auto
  "Pixel height every glyph icon is scaled to.
Either an integer number of pixels, or the symbol `auto' to derive the height
from the current font (`ps/material-icons-height-scale' times the default font
height).  `auto' keeps icons proportional to the text across fonts/OSes so
their vertical alignment stays stable without manual retuning."
  :type '(choice (const :tag "Derive from font" auto)
                 (integer :tag "Fixed pixels"))
  :group 'ps-material-icons)

(defcustom ps/material-icons-height-scale 1.1
  "Multiplier applied to the default font height when `ps/material-icons-height'
is `auto'.  Tune once so glyph icons look right next to your text."
  :type 'number
  :group 'ps-material-icons)

(defcustom ps/material-icons-category-map nil
  "Alist mapping a <Category>.org file basename to a Material Symbols name.
Each entry is a cons cell (BASENAME . ICON-NAME), e.g. (\"Blog\" . \"edit_square\").
Consumed by both the agenda category icons and the file tree.  Left empty in
`config.org'; real mappings are typically supplied by the per-Org-folder
`workspace.org'.  Files with no entry get a generic icon."
  :type '(alist :key-type string :value-type string)
  :group 'ps-material-icons)

(defcustom ps/material-icons-folder-map nil
  "Alist mapping a folder to a Material Symbols name for the folder's own icon.
Each entry is (FOLDER . ICON-NAME), e.g. (\"Current\" . \"calendar_month\"), and
sets the icon the file tree draws before that *directory* — at any depth.
FOLDER is matched against a directory's name or its path relative to the Org
base, so \"older\" icons every folder of that name while \"ML/older\" icons just
the one.  The same icon is used open and closed.  Directories with no entry get
the generic folder glyphs.  To icon the *files inside* a folder instead, see
`ps/material-icons-folder-contents-map'."
  :type '(alist :key-type string :value-type string)
  :group 'ps-material-icons)

(defcustom ps/material-icons-folder-contents-map nil
  "Alist mapping a folder to a Material Symbols name for the files inside it.
Each entry is (FOLDER . ICON-NAME), e.g. (\"Vision\" . \"mountain_flag\"), and
icons every .org file under that folder, at any depth, that would otherwise
fall back to the generic file glyph.  FOLDER is matched like
`ps/material-icons-folder-map'.  Entries here take precedence over
`ps/material-icons-category-map', so a whole folder can be iconed uniformly
without listing its files."
  :type '(alist :key-type string :value-type string)
  :group 'ps-material-icons)

;;; Name -> codepoint resolution

(defvar ps/material-icons--table nil
  "Hash table mapping normalized icon name -> integer codepoint, or nil.
Loaded lazily from `ps/material-icons-codepoints-file'.")

(defun ps/material-icons--normalize (name)
  "Normalize NAME to a codepoints-file key: downcase, spaces/hyphens -> underscore."
  (replace-regexp-in-string "[ -]" "_" (downcase (string-trim name))))

(defun ps/material-icons--load-table ()
  "Load and return the name->codepoint hash, parsing the codepoints file once."
  (or ps/material-icons--table
      (setq ps/material-icons--table
            (let ((table (make-hash-table :test 'equal)))
              (when (file-readable-p ps/material-icons-codepoints-file)
                (with-temp-buffer
                  (insert-file-contents ps/material-icons-codepoints-file)
                  (goto-char (point-min))
                  (while (re-search-forward "^\\([^ \t]+\\)[ \t]+\\([0-9a-fA-F]+\\)$" nil t)
                    (puthash (match-string 1)
                             (string-to-number (match-string 2) 16)
                             table))))
              table))))

(defun ps/material-icons-codepoint (name)
  "Return the integer codepoint for icon NAME, or nil if unknown.
NAME may be a Material Symbols name in any case, with spaces or underscores."
  (when (and name (stringp name) (not (string-empty-p name)))
    (gethash (ps/material-icons--normalize name) (ps/material-icons--load-table))))

;;; Sizing

(defun ps/material-icons--pixel-height ()
  "Resolve `ps/material-icons-height' to a pixel height.
When `auto', derive it from the default font; falls back to 20 when there is no
graphical display (e.g. batch)."
  (if (integerp ps/material-icons-height)
      ps/material-icons-height
    (if (display-graphic-p)
        (max 1 (round (* ps/material-icons-height-scale (default-font-height))))
      20)))

;;; Rendering

(defun ps/material-icons-svg (name &optional color)
  "Return an SVG string drawing icon NAME, or nil if NAME is unknown.
Mirrors the attributes the old icon SVGs used (height=20 width=24,
viewBox=\"0 -960 960 960\"): the font em is 960 units, so font-size 960 at
baseline y=0 fills the box.  COLOR defaults to `ps/material-icons-color'."
  (when-let ((codepoint (ps/material-icons-codepoint name)))
    (format
     (concat "<svg xmlns=\"http://www.w3.org/2000/svg\""
             " height=\"20px\" width=\"24px\" viewBox=\"0 -960 960 960\""
             " fill=\"%s\"><text x=\"0\" y=\"0\" font-family=\"%s\""
             " font-size=\"960\">&#x%x;</text></svg>")
     (or color ps/material-icons-color)
     ps/material-icons-font-family codepoint)))

(defun ps/material-icons-image (name &optional ascent height color)
  "Return an image rendering icon NAME, or nil if NAME is unknown.
ASCENT sets the image vertical alignment (integer 0-100 or `center', default
`center').  HEIGHT overrides the pixel height, which otherwise comes from
`ps/material-icons--pixel-height'.  COLOR overrides `ps/material-icons-color';
it has to be passed here rather than left to a face, because a face cannot
recolour an image -- which is how a dimmed icon ends up looking exactly like a
live one."
  (when-let ((svg (ps/material-icons-svg name color)))
    (create-image svg 'svg t
                  :height (or height (ps/material-icons--pixel-height))
                  :ascent (or ascent 'center))))

;;; Availability

(defun ps/material-icons-available-p ()
  "Return non-nil if `ps/material-icons-font-family' is installed."
  (find-font (font-spec :family ps/material-icons-font-family)))

;;; Declarative-map convenience

(defun ps/material-icons-add (alist)
  "Merge ALIST of (BASENAME . ICON-NAME) into `ps/material-icons-category-map'.
Later entries (from ALIST) override earlier ones for the same basename.
Intended for use from `workspace.org'."
  (dolist (entry alist)
    (setq ps/material-icons-category-map
          (cons entry (assoc-delete-all (car entry) ps/material-icons-category-map))))
  ps/material-icons-category-map)

(provide 'ps-material-icons)
;;; ps-material-icons.el ends here
