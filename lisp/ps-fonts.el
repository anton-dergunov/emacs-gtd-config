;;; ps-fonts.el --- Role-based font selection with fallback -*- lexical-binding: t; -*-

;;; Commentary:

;; One place to name the fonts this config uses, by *role* rather than by face:
;;
;;   `ps/font-mono'  -- anything laid out on a character grid: the `default'
;;                      face, and `fixed-pitch' for the islands that have to
;;                      stay on the grid (tables, code blocks) inside a buffer
;;                      that is otherwise proportional.
;;   `ps/font-prose' -- `variable-pitch', for running text.
;;
;; Each role is a *list* of families, most-wanted first; the first one actually
;; installed on this machine wins.  That fallback is the reason this module
;; exists.  Naming a font directly, as the old `ps/main-font' did with a single
;; "Family-Size" string, fails badly when the family is absent: the string form
;; signals on a graphical frame, and the `:family' form silently resolves to
;; whatever the OS considers closest.  Here an absent family is simply skipped,
;; and a role whose candidates are *all* absent is left at whatever Emacs
;; chose -- so the worst case is "looks default", never "does not start".
;;
;; Two size conventions are deliberately different, and `:height' means
;; different things depending on its type:
;;
;;   - `default' takes an *absolute* size (an integer in 1/10 pt) from
;;     `ps/font-size'.  It is the frame's ruler: `frame-char-width' and every
;;     column-based measurement -- window widths, margins, `(space :align-to)'
;;     -- derive from it, so it has to be a real size, not a multiplier.
;;   - `fixed-pitch' takes no size at all, so it tracks `default'.  Giving it an
;;     absolute height would freeze it while the rest of the frame scales.
;;   - `variable-pitch' takes a *relative* float (`ps/font-prose-scale'),
;;     because a serif set at the same point size as a mono reads larger.  A
;;     float keeps that correction proportional when `ps/font-size' changes.
;;
;; Nothing here is applied automatically on load; `config.org' calls
;; `ps/fonts-apply' from its `Editor & UI / Font' block, after
;; `face-font-rescale-alist' is in place (that list has to be set before any
;; font is opened).

;;; Code:

(require 'seq)

;;; Customization

(defgroup ps-fonts nil
  "Fonts, named by role rather than by face."
  :group 'ps)

(defcustom ps/font-mono '("Monaco")
  "Families for grid-aligned text, most-wanted first.
The first family installed on this machine is applied to `default' and
`fixed-pitch'; families that are not installed are skipped.  When none of
them is available both faces are left alone.

This is the frame's ruler as well as its editing font: everything measured
in columns -- window widths, margins, `(space :align-to)' -- follows it, so
the agenda, the schedule ruler and the report buffers all depend on it being
monospaced."
  :type '(repeat string)
  :group 'ps-fonts)

(defcustom ps/font-prose '("Charter")
  "Families for `variable-pitch' (running text), most-wanted first.
Resolved the same way as `ps/font-mono'.  Emacs' own default here is a
generic sans, which on macOS resolves to Helvetica; naming a font makes the
proportional half of the UI intentional rather than inherited."
  :type '(repeat string)
  :group 'ps-fonts)

(defcustom ps/font-size 14
  "Size of the `default' face, in points.
Applied as an absolute `:height', so every face that sizes itself with a
relative (float) `:height' -- the Org heading ramp, the line-number gutter,
the agenda badges -- scales with it.  nil leaves the size alone."
  :type '(choice (const :tag "Leave the size alone" nil) number)
  :group 'ps-fonts)

(defcustom ps/font-prose-scale 1.0
  "Size of `variable-pitch' relative to `default'.
A serif at the same point size as a monospace font reads larger, so this is
usually slightly below 1.  Applied as a relative `:height', which keeps the
correction proportional when `ps/font-size' changes.  A value of 1 leaves the
size alone."
  :type 'float
  :group 'ps-fonts)

(defcustom ps/font-rescale-alist
  '(("Apple Color Emoji" . 0.8)
    ("Arial Unicode MS"  . 0.9)
    ("STIX Two Math"     . 0.85)
    ("Apple Symbols"     . 0.9)
    ("Zapf Dingbats"     . 0.9)
    ("Apple Braille"     . 0.9))
  "Scale factors for fallback fonts, applied to `face-font-rescale-alist'.
Emoji and symbol glyphs the main font lacks are drawn from fallback fonts
whose line boxes are taller, so a line containing one is a few pixels taller
than its neighbours -- text visibly shifts as such glyphs scroll past or
animate.  Emacs has no way to cap a line's height (`line-height' raises its
floor, it does not lower its ceiling), so instead the fallback fonts are
scaled to fit.  These factors bring every tested glyph to the same height as
the main font on macOS; adjust per font if a glyph still sits taller.

Note that these are tuned against a particular main font: changing
`ps/font-mono' changes which characters count as missing, and therefore which
fallback fonts get chosen at all."
  :type '(alist :key-type string :value-type number)
  :group 'ps-fonts)

;;; Resolution (pure)

(defun ps/fonts--candidates (value)
  "Return VALUE as a clean list of family names.
Accepts a single family string as well as a list, and drops anything that is
not a non-empty string, so a half-edited setting degrades to \"try the rest\"
rather than signalling."
  (seq-filter (lambda (family)
                (and (stringp family) (not (string-empty-p family))))
              (if (listp value) value (list value))))

(defun ps/fonts--available-p (family)
  "Non-nil when FAMILY is installed on this machine.
Always nil in batch, where there is no font backend to ask -- which is why
`ps/fonts--first-available' takes an injectable predicate, and why
`ps/fonts-apply' has to treat \"nothing found\" as \"leave the face alone\"
rather than as an error."
  (and (find-font (font-spec :family family)) t))

(defun ps/fonts--first-available (value &optional availablep)
  "Return the first family in VALUE that is installed, or nil if none is.
VALUE is anything `ps/fonts--candidates' accepts.  AVAILABLEP overrides the
installed-font test, and defaults to `ps/fonts--available-p'."
  (seq-find (or availablep #'ps/fonts--available-p)
            (ps/fonts--candidates value)))

(defun ps/fonts--face-spec (family height)
  "Return `set-face-attribute' arguments for FAMILY at HEIGHT, as a plist.
FAMILY nil means \"leave the family alone\"; HEIGHT nil, or a height that
would be a no-op, means \"leave the size alone\".  Returns nil when there is
nothing to set at all.

HEIGHT is passed through to `:height' verbatim, so its *type* carries the
meaning: an integer is an absolute size in 1/10 pt, a float is a multiplier
of the inherited face's size.  A float of 1.0 is dropped rather than applied,
so that remapping this face elsewhere is not stacked onto a redundant
multiplier."
  (append
   (and (stringp family) (not (string-empty-p family)) (list :family family))
   (and (numberp height) (> height 0) (not (equal height 1.0))
        (list :height height))))

(defun ps/fonts--points-to-height (points)
  "Convert POINTS to an absolute `:height', an integer in 1/10 pt.
Returns nil for a non-positive or non-numeric POINTS, which reads as \"leave
the size alone\"."
  (and (numberp points) (> points 0) (round (* 10 points))))

;;; Application

(defun ps/fonts--set (face spec)
  "Apply SPEC to FACE when SPEC is non-nil.  Return non-nil when it was applied."
  (when spec
    (apply #'set-face-attribute face nil spec)
    t))

;;;###autoload
(defun ps/fonts-apply ()
  "Apply `ps/font-mono' and `ps/font-prose' to the frame's base faces.
Never signals: a role whose families are all missing is left as Emacs had it,
so naming a font this machine does not have cannot break startup.  Returns
the (MONO . PROSE) families that were actually applied, either of which may
be nil."
  (let* ((mono   (ps/fonts--first-available ps/font-mono))
         (prose  (ps/fonts--first-available ps/font-prose))
         (height (ps/fonts--points-to-height ps/font-size)))
    ;; Before any face below opens a font: the rescale factors are consulted
    ;; when a font is opened, not when it is drawn, so setting them here rather
    ;; than leaving it to a caller makes the ordering impossible to get wrong.
    (setq face-font-rescale-alist ps/font-rescale-alist)
    ;; `default' carries the absolute size; `fixed-pitch' takes the family only
    ;; so that it keeps tracking `default' when the size changes.
    (ps/fonts--set 'default (ps/fonts--face-spec mono height))
    (ps/fonts--set 'fixed-pitch (ps/fonts--face-spec mono nil))
    (ps/fonts--set 'variable-pitch (ps/fonts--face-spec prose ps/font-prose-scale))
    (cons mono prose)))

(provide 'ps-fonts)
;;; ps-fonts.el ends here
