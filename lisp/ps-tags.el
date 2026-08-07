;;; ps-tags.el --- Keep Org tag pills whole when a headline wraps -*- lexical-binding: t; -*-

;;; Commentary:

;; Tags render exactly where they sit in the file -- nothing here aligns,
;; pads or re-spaces anything.  The one thing it fixes is a drawing artifact.
;;
;; `org-modern--tag' builds each tag pill by putting a `display' string on the
;; tag's first and last character: ":ml:" becomes the display strings " m" and
;; "l ", so the pill's padding is a *literal space character inside a display
;; string*.  Emacs' `word-wrap' (on in Org buffers through `visual-line-mode')
;; treats the first byte of a display string as a line-break opportunity when
;; it is a space, so a wrapping headline can break *between* two pills -- and
;; the padding space, still carrying the `org-modern-tag' background, is left
;; stranded at the right edge as an empty grey box.
;;
;; Replacing those padding spaces with no-break spaces removes every break
;; opportunity inside the tag group.  The group becomes atomic: the only place
;; the line can still break is the real whitespace before the tags, so a
;; headline with no room left moves the whole `:a:b:' group to the next line
;; instead of splitting a pill.
;;
;; This runs as a font-lock keyword appended *after* org-modern's own, so it
;; sees and rewrites what org-modern has just produced.

;;; Code:

;; Provided by Org; declared here so this file loads (and its pure function is
;; testable) without Org.
(defvar org-tag-re)

(defconst ps/tags--nobreak-space ?\N{NO-BREAK SPACE}
  "Character used in place of a tag pill's padding space.
Unlike a plain space it is not a `word-wrap' break opportunity.")

(defun ps/tags--nobreak-string (string)
  "Return STRING with every space replaced by a no-break space.
Text properties are preserved -- org-modern stores a `cursor' property
inside the display strings this rewrites.  Returns STRING itself when it
holds no space, so repeated fontification is a no-op."
  (if (not (string-search " " string))
      string
    ;; The strings org-modern builds are often unibyte, and storing a no-break
    ;; space into one of those would store a raw byte instead of the character.
    ;; `string-to-multibyte' drops text properties, so carry them across
    ;; afterwards -- the substitution is one character for one, so the
    ;; positions still line up.
    (let ((copy (copy-sequence (string-to-multibyte string))))
      (dotimes (i (length copy))
        (when (eq (aref copy i) ?\s)
          (aset copy i ps/tags--nobreak-space))
        (set-text-properties i (1+ i) (text-properties-at i string) copy))
      copy)))

(defun ps/tags--unbreak-region (beg end)
  "Make the tag pills between BEG and END unbreakable.
Rewrites the padding spaces of every `display' string org-modern placed
in the region."
  (let ((pos beg))
    (while (< pos end)
      (let ((next (next-single-property-change pos 'display nil end))
            (spec (get-text-property pos 'display)))
        (when (stringp spec)
          (let ((fixed (ps/tags--nobreak-string spec)))
            (unless (eq fixed spec)
              (put-text-property pos next 'display fixed))))
        (setq pos next)))))

(defun ps/tags--pill-p (pos)
  "Non-nil when POS carries the `org-modern-tag' face.
Guards against rewriting a line that merely ends in something shaped like
a tag group but was never rendered as pills."
  (let ((face (get-text-property pos 'face)))
    (or (eq face 'org-modern-tag)
        (and (listp face) (memq 'org-modern-tag face)))))

(defun ps/tags--fontify (limit)
  "Keep tag pills from being split by line wrap, up to LIMIT.
A font-lock matcher: it works by side effect and always returns nil."
  (let ((regexp (concat "\\( \\)\\(:\\(?:" org-tag-re "::?\\)+\\)[ \t]*$")))
    (while (re-search-forward regexp limit t)
      (let ((beg (match-beginning 2))
            (end (match-end 2)))
        ;; org-modern gives the tag *text* the pill face; the colons keep the
        ;; headline face, so probe just inside the leading colon.
        (when (and (< (1+ beg) end) (ps/tags--pill-p (1+ beg)))
          (ps/tags--unbreak-region beg end)))))
  nil)

(defun ps/tags-apply ()
  "Set the current buffer up to draw whole tag pills."
  ;; Without this Emacs draws the no-break spaces with the `nobreak-space'
  ;; face, speckling every pill.
  (setq-local nobreak-char-display nil)
  ;; APPEND=t so this runs after org-modern's keywords and can rewrite the
  ;; display strings they just added.
  (font-lock-add-keywords nil '((ps/tags--fontify 0 t)) t))

;;;###autoload
(defun ps/tags-setup ()
  "Keep tag pills whole in every Org buffer."
  (add-hook 'org-mode-hook #'ps/tags-apply))

(provide 'ps-tags)
;;; ps-tags.el ends here
