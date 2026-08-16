;;; ps-dired.el --- A folder listing worth reading -*- lexical-binding: t; -*-

;;; Commentary:
;; `ls -l' answers a question from 1971.  Permissions, link count, owner and
;; group are six columns of noise in front of the two things anyone browsing a
;; captured folder wants: what this is, and how big it is.  Worse, the one fact
;; the columns *do* carry usefully -- file or folder -- is carried by the
;; leading `d', which `dired-hide-details-mode' hides along with everything
;; else, leaving the distinction to colour alone.
;;
;; So: hide the details (Dired tags that region on every line as it reads it,
;; so this costs nothing and `(' brings it all back), and put two columns of
;; our own in front of the name -- a type icon and, for files, a human-readable
;; size.
;;
;; Three things here are deliberate.
;;
;; The icons are the file tree's own two Material Symbols, referenced from
;; `ps-file-tree-icons.el' rather than restated, so the two views of the same
;; disk cannot drift apart.  ONE icon for every file whatever its extension:
;; the per-category icons Org plan files get in the tree say what a plan is
;; *for*, which means nothing in a folder of captured material.
;;
;; The annotation is a `before-string' on a one-character overlay rather than
;; inserted text.  Nothing in the buffer moves, so `dired-move-to-filename',
;; marking, and every other Dired command that counts columns keeps working.
;;
;; The size comes from `file-attributes' rather than from parsing the `ls'
;; output, because the switches are not ours to rely on: the listing is GNU
;; `ls' where coreutils is installed and Emacs's own `ls-lisp' where it is not.

;;; Code:

(require 'seq)

(declare-function dired-move-to-filename "dired")
(declare-function dired-get-filename "dired")
(declare-function ps/material-icons-image "ps-material-icons")

(defvar ps/file-tree-icons--folder-closed)
(defvar ps/file-tree-icons--file)
(defvar ps/file-tree-icon-ascent)

(defgroup ps/dired nil
  "What a folder listing shows."
  :group 'ps)

(defcustom ps/dired-decorate-listing t
  "Whether to replace the `ls -l' columns with a type icon and a size.
Turn this off to get stock Dired back; `(' toggles the full `ls' details in
either case."
  :type 'boolean
  :group 'ps/dired)

(defcustom ps/dired-size-width 7
  "Width of the size column, in characters.
Wide enough for the longest thing `file-size-human-readable' produces, and
narrow enough that names still start near the left edge."
  :type 'integer
  :group 'ps/dired)

(defconst ps/dired-omit-files
  "\\`[.]?#\\|\\`[.]\\'\\|\\`\\.DS_Store\\'"
  "What a folder listing leaves out, for `dired-omit-files'.

Lock and auto-save files, `.', and the junk macOS leaves in every synced
folder.  Stated in full rather than appended to dired-x's default, which would
grow by one alternative every time the config was reloaded -- and, more to the
point, because that default omits `..' as well.  `..' stays: it is how you
walk back up, and it is the only way to do that with the mouse.")

(defcustom ps/dired-parent-icon "drive_folder_upload"
  "Material Symbols name for the `..' entry.
Deliberately not the folder icon: the way out of a folder should not look like
the ways further in."
  :type 'string
  :group 'ps/dired)

(defcustom ps/dired-fallback-glyphs
  '((parent . "↑") (directory . "▸") (file . "·"))
  "Text drawn instead of an icon where Material Symbols is not installed.
The type marker matters more than its prettiness: without it the only thing
separating a folder from a file is its colour."
  :type '(alist :key-type symbol :value-type string)
  :group 'ps/dired)

;;; What a line is

(defun ps/dired--kind (name attributes)
  "Return `parent', `directory' or `file' for NAME with ATTRIBUTES.
ATTRIBUTES is what `file-attributes' returns, or nil when the file could not
be read -- a broken symlink, say, which is still a file as far as this is
concerned."
  (cond ((member (file-name-nondirectory (directory-file-name name)) '(".." "."))
         'parent)
        ((eq (file-attribute-type attributes) t) 'directory)
        (t 'file)))

(defun ps/dired--size-label (kind attributes)
  "Return the size to show for a KIND entry with ATTRIBUTES, as a string.
Empty for anything that is not a file: a directory's own size is its entry
table, which is neither what it holds nor anything anyone wants to read."
  (if (and (eq kind 'file) attributes)
      (file-size-human-readable (file-attribute-size attributes) 'si)
    ""))

(defun ps/dired--annotation (glyph size)
  "Return the text put in front of a file name: GLYPH then SIZE, right-aligned."
  (format "%s %s  " glyph (string-pad size ps/dired-size-width nil :start)))

;;; Drawing it

(defun ps/dired--glyph (kind)
  "Return the display string for a KIND entry's type icon."
  (let ((fallback (or (cdr (assq kind ps/dired-fallback-glyphs)) " "))
        (name (pcase kind
                ('parent ps/dired-parent-icon)
                ('directory (bound-and-true-p ps/file-tree-icons--folder-closed))
                (_ (bound-and-true-p ps/file-tree-icons--file)))))
    (or (and name
             (display-graphic-p)
             (fboundp 'ps/material-icons-image)
             (when-let* ((image (ps/material-icons-image
                                 name (bound-and-true-p ps/file-tree-icon-ascent))))
               (propertize " " 'display image)))
        fallback)))

(defun ps/dired--annotate-line ()
  "Put the type icon and size in front of the file name on this line, if any."
  (when-let* ((start (dired-move-to-filename))
              (name (ignore-errors (dired-get-filename nil t))))
    (let* ((attributes (ignore-errors (file-attributes name)))
           (kind (ps/dired--kind name attributes))
           (annotation (ps/dired--annotation
                        (ps/dired--glyph kind)
                        (ps/dired--size-label kind attributes)))
           ;; One character wide, not empty: an empty overlay with `evaporate'
           ;; is deleted the moment the buffer is modified, and omitting
           ;; `evaporate' would leave the icon behind when `dired-omit-mode'
           ;; deletes the line it belongs to.
           (overlay (make-overlay start (min (1+ start) (point-max)))))
      (overlay-put overlay 'ps/dired t)
      (overlay-put overlay 'evaporate t)
      (overlay-put overlay 'before-string annotation))))

(defun ps/dired--clear ()
  "Remove annotations left by an earlier pass over this buffer."
  (dolist (overlay (overlays-in (point-min) (point-max)))
    (when (overlay-get overlay 'ps/dired)
      (delete-overlay overlay))))

;;;###autoload
(defun ps/dired-decorate ()
  "Annotate every entry in this Dired buffer with its type and size.

On `dired-after-readin-hook' as a *global* entry, which is what puts it after
`dired-omit-expunge': that one is added buffer-locally by `dired-omit-mode',
and a buffer-local hook runs before the global part of the same hook.
Annotating first would mean annotating lines that are about to be deleted."
  (when (and ps/dired-decorate-listing
             (derived-mode-p 'dired-mode)
             ;; `file-attributes' over a remote listing is a round trip per
             ;; line.  The columns are not worth that.
             (not (file-remote-p default-directory)))
    (save-excursion
      (ps/dired--clear)
      (goto-char (point-min))
      (while (not (eobp))
        ;; Demoted rather than ignored: this runs from a hook Dired cannot do
        ;; without, so a bad line must not take the listing with it -- but a
        ;; line that silently loses its icon is a bug that never gets reported.
        (with-demoted-errors "ps-dired: %S" (ps/dired--annotate-line))
        (forward-line 1)))))

;;;###autoload
(defun ps/dired-setup ()
  "Draw folder listings as a type icon, a size and a name.
Idempotent, so it survives a config reload."
  (add-hook 'dired-after-readin-hook #'ps/dired-decorate))

(provide 'ps-dired)
;;; ps-dired.el ends here
