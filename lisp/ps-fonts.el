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
(require 'ps-window)

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

;;; Roles

(defconst ps/fonts--roles
  '((mono  . ps/font-mono)
    (prose . ps/font-prose))
  "Role name -> the setting holding that role's candidate list.
One list drives the settings, the audition commands and the preview buffer,
so a role cannot be added in one place and forgotten in another.")

(defun ps/fonts--role-variable (role)
  "Return the settings variable for ROLE, or nil if ROLE is unknown."
  (cdr (assq role ps/fonts--roles)))

(defun ps/fonts--promote (family candidates)
  "Return CANDIDATES with FAMILY first and the rest in their original order.
This is the list to paste back into the settings block after an audition: the
font just chosen becomes the preference, and the fallbacks that were already
there stay behind it rather than being lost."
  (cons family (seq-remove (lambda (other) (string= other family))
                           (ps/fonts--candidates candidates))))

(defun ps/fonts--setting-line (variable families)
  "Return the `setq' line that would make FAMILIES the value of VARIABLE."
  (format "(setq %s '(%s))"
          variable
          (mapconcat (lambda (family) (format "%S" family)) families " ")))

;;; Application

(defun ps/fonts--set (face spec)
  "Apply SPEC to FACE when SPEC is non-nil.  Return non-nil when it was applied."
  (when spec
    (apply #'set-face-attribute face nil spec)
    t))

(defun ps/fonts--apply-role (role family)
  "Apply FAMILY as ROLE's font, now.  A nil FAMILY applies sizes only.
The settings are not touched, so this is what an audition does: it changes
what you see without changing what `config.org' says."
  (pcase role
    ('mono
     ;; `default' carries the absolute size; `fixed-pitch' takes the family
     ;; only, so that it keeps tracking `default' when the size changes.
     (ps/fonts--set 'default
                    (ps/fonts--face-spec family (ps/fonts--points-to-height ps/font-size)))
     (ps/fonts--set 'fixed-pitch (ps/fonts--face-spec family nil)))
    ('prose
     (ps/fonts--set 'variable-pitch
                    (ps/fonts--face-spec family ps/font-prose-scale)))))

;;;###autoload
(defun ps/fonts-apply ()
  "Apply `ps/font-mono' and `ps/font-prose' to the frame's base faces.
Never signals: a role whose families are all missing is left as Emacs had it,
so naming a font this machine does not have cannot break startup.  Returns
the (MONO . PROSE) families that were actually applied, either of which may
be nil.

Also the way back from an audition -- it re-reads the settings, so it undoes
anything `ps/font-try' or the preview buffer applied."
  (interactive)
  (let ((mono  (ps/fonts--first-available ps/font-mono))
        (prose (ps/fonts--first-available ps/font-prose)))
    ;; Before any face below opens a font: the rescale factors are consulted
    ;; when a font is opened, not when it is drawn, so setting them here rather
    ;; than leaving it to a caller makes the ordering impossible to get wrong.
    (setq face-font-rescale-alist ps/font-rescale-alist)
    (ps/fonts--apply-role 'mono mono)
    (ps/fonts--apply-role 'prose prose)
    (cons mono prose)))

;;; Auditioning

(defun ps/fonts--installed-families ()
  "Return the font families this frame can use, sorted, without duplicates.
Empty in batch, where there is no font backend."
  (sort (delete-dups (font-family-list)) #'string-lessp))

(defun ps/fonts--read-role (prompt)
  "Read a role name from the minibuffer with PROMPT."
  (intern (completing-read prompt (mapcar #'car ps/fonts--roles) nil t nil nil "mono")))

;;;###autoload
(defun ps/font-try (role family)
  "Apply FAMILY as ROLE's font right now, to see what it looks like.
Deliberately does not persist: `config.org' stays the single source of truth,
so this echoes the settings line that would make the choice permanent, and
`ps/fonts-apply' puts everything back."
  (interactive
   (let ((role (ps/fonts--read-role "Role: ")))
     (list role (completing-read (format "Font for %s: " role)
                                 (ps/fonts--installed-families) nil t))))
  (ps/fonts--apply-role role family)
  (let ((variable (ps/fonts--role-variable role)))
    (message "%s — keep it with:  %s"
             family
             (ps/fonts--setting-line
              variable (ps/fonts--promote family (symbol-value variable))))))

;;;###autoload
(defun ps/font-size-try (points)
  "Set the body text size to POINTS and re-apply every role.
Unlike `ps/font-try' this does change the setting, since size is one number
rather than a preference list -- `ps/font-size' in Settings is where it
persists."
  (interactive (list (read-number "Body text size (points): " ps/font-size)))
  (setq ps/font-size points)
  (ps/fonts-apply)
  (message "Body text %spt — keep it with:  (setq ps/font-size %s)" points points))

;;; Preview

(defcustom ps/font-preview-candidates
  '(;; Monospaced -- for `ps/font-mono'
    "Monaco" "Menlo" "SF Mono" "PT Mono"
    "JetBrains Mono" "Commit Mono" "Iosevka" "IBM Plex Mono"
    ;; Proportional -- for `ps/font-prose'
    "Charter" "Georgia" "Palatino" "Iowan Old Style" "Hoefler Text"
    "Literata" "Source Serif 4" "Spectral" "Merriweather" "Newsreader"
    "IBM Plex Serif"
    "Inter" "IBM Plex Sans" "Avenir Next" "Seravek"
    ;; Ships its Mono and Sans variants as *styles* of one family rather than
    ;; as separate families, so naming the family reaches only its default
    ;; instance -- the monospaced half is not selectable this way.
    "Recursive")
  "Font families offered by `ps/font-preview'.
Families that are not installed are listed at the bottom of the preview
rather than drawn, so this can name fonts you have not installed yet.  The
name to use is the family name the font itself declares, which is not always
the name of the download -- if something you installed shows up as missing,
check with `M-x describe-font' or add the name the system reports."
  :type '(repeat string)
  :group 'ps-fonts)

(defun ps/fonts--preview-families (&optional availablep)
  "Return (INSTALLED . MISSING) families for the preview buffer.
The families the settings currently name come first, so what you are already
looking at is at the top to compare against, followed by
`ps/font-preview-candidates'.  AVAILABLEP overrides the installed-font test."
  (let ((seen (make-hash-table :test #'equal))
        (test (or availablep #'ps/fonts--available-p))
        installed missing)
    (dolist (family (append (ps/fonts--candidates ps/font-mono)
                            (ps/fonts--candidates ps/font-prose)
                            (ps/fonts--candidates ps/font-preview-candidates)))
      (unless (gethash family seen)
        (puthash family t seen)
        (if (funcall test family)
            (push family installed)
          (push family missing))))
    (cons (nreverse installed) (nreverse missing))))

(defconst ps/fonts--preview-sample
  '((heading . "Quarterly review")
    (heading . "Prepare the deck")
    (heading . "Draft the outline")
    (styles  . nil)
    (prose   . "Plan prose reads for paragraphs, not for columns — 0123456789.")
    (task    . "TODO  [A]  Draft the quarterly review          Work    Tue")
    (grid    . "09:00-10:30 ┆ Standup  ┄┄┄┄┄┄┄┄┄┄  ✓ ⏰")
    (width   . "iiiiiiiiiiiiiiii")
    (width   . "MMMMMMMMMMMMMMMM"))
  "The sample rendered for each candidate, as (KIND . TEXT) in order.
The two `width' lines are the monospace check: equal length means every glyph
occupies one cell.  The `grid' line carries the characters the schedule ruler
and the agenda badges need, so a font that lacks them shows its fallback.")

(defun ps/fonts--preview-kind-face (kind family index)
  "Return the face spec for a sample line of KIND in FAMILY.
INDEX is the heading level for `heading' lines.  Heading heights come from
the live `org-level-N' faces when Org is loaded, so the preview shows the
ramp actually configured rather than a copy of it."
  (let ((level (intern (format "org-level-%d" index))))
    (pcase kind
      ('heading (if (facep level)
                    (list :family family :inherit level)
                  (list :family family :weight 'bold)))
      ('prose   (list :family family))
      ('task    (list :family family))
      ('grid    (list :family family))
      ('width   (list :family family))
      (_        (list :family family)))))

(defun ps/fonts--preview-insert-styles (family)
  "Insert the regular/bold/italic row for FAMILY.
The point of the row: a family with no real bold or italic (Monaco ships
Regular only) gets a smeared or mechanically sheared imitation from the OS,
and that is only visible side by side with a family that has all four."
  (insert "  ")
  (dolist (style '(("Regular"     :weight normal :slant normal)
                   ("Bold"        :weight bold   :slant normal)
                   ("Italic"      :weight normal :slant italic)
                   ("Bold Italic" :weight bold   :slant italic)))
    (insert (propertize (car style)
                        'face (append (list :family family) (cdr style)))
            "   "))
  (insert "\n"))

(defun ps/fonts--preview-insert-family (family)
  "Insert one preview block for FAMILY, with its two apply buttons.
The whole block carries a `ps-font-family' text property, so the keyboard
commands can tell which block point is in without parsing the text back."
  (let ((start (point)))
    (insert (propertize family 'face '(:weight bold :height 1.1)))
    (insert "   ")
    (dolist (role '(mono prose))
      (insert-text-button (format "[use as %s]" role)
                          'family family
                          'role role
                          'follow-link t
                          'help-echo (format "Apply %s as the %s font" family role)
                          'action #'ps/fonts--preview-use-button)
      (insert " "))
    (insert "\n")
    (let ((heading 0))
      (dolist (line ps/fonts--preview-sample)
        (pcase (car line)
          ('styles (ps/fonts--preview-insert-styles family))
          (kind
           (when (eq kind 'heading) (setq heading (1+ heading)))
           (insert "  "
                   (propertize (cdr line)
                               'face (ps/fonts--preview-kind-face kind family heading))
                   "\n")))))
    (insert "\n")
    (put-text-property start (point) 'ps-font-family family)))

(defun ps/fonts--preview-use-button (button)
  "Apply the family BUTTON names to the role it names."
  (let ((family (button-get button 'family))
        (role   (button-get button 'role)))
    (ps/font-try role family)))

(defun ps/fonts--preview-insert-ramp ()
  "Insert the resolved pixel height of each Org heading level.
`:height' multipliers are stored as integers in 1/10 pt and then rounded to
whole pixels, so a deliberately small ramp can round several levels onto the
same size.  This shows what the configured numbers actually resolve to at the
current font and size, which is the only way to tune them on purpose."
  (when (facep 'org-level-1)
    (insert (propertize "Heading ramp, as it actually resolves\n"
                        'face '(:weight bold)))
    (dolist (level '(1 2 3 4 5))
      ;; `:height' without INHERIT, so this shows the multiplier as configured
      ;; rather than the absolute size it resolves to through the face chain --
      ;; the multiplier is the number being tuned, the pixels are the result.
      (let* ((face (intern (format "org-level-%d" level)))
             (spec (and (facep face) (face-attribute face :height))))
        (insert (format "  level %d   :height %-10s → %s px\n"
                        level
                        (if (numberp spec) spec "inherited")
                        (if (facep face) (window-font-height nil face) "?")))))
    (insert "\n")))

(defun ps/fonts--preview-render ()
  "Redraw the *Font Preview* buffer."
  (let ((inhibit-read-only t)
        (families (ps/fonts--preview-families)))
    (erase-buffer)
    (insert (propertize "Font preview\n" 'face '(:weight bold :height 1.2)))
    (insert "Click a button (or press m / p on a block) to apply a font now — "
            "nothing is saved.\n")
    (insert (format "Currently: mono %s · prose %s · %spt.  "
                    (or (ps/fonts--first-available ps/font-mono) "—")
                    (or (ps/fonts--first-available ps/font-prose) "—")
                    ps/font-size))
    (insert "Keys: m/p apply · s size · g refresh · r revert · q quit\n\n")
    (ps/fonts--preview-insert-ramp)
    (dolist (family (car families))
      (ps/fonts--preview-insert-family family))
    (when (cdr families)
      (insert (propertize "Not installed\n" 'face '(:weight bold)))
      (insert "  " (string-join (cdr families) ", ") "\n")
      (insert "  Most are available as Homebrew casks, e.g."
              " brew install --cask font-jetbrains-mono\n"))
    (goto-char (point-min))))

(defun ps/fonts--preview-family-at-point ()
  "Return the family whose block point is inside, or nil.
Reads the `ps-font-family' property the block was rendered with; the blank
line that ends a block carries it too, so point never falls between blocks."
  (get-text-property (point) 'ps-font-family))

(defun ps/font-preview-use-mono ()
  "Apply the family at point as the monospaced font."
  (interactive)
  (if-let ((family (ps/fonts--preview-family-at-point)))
      (ps/font-try 'mono family)
    (user-error "Point is not inside a font block")))

(defun ps/font-preview-use-prose ()
  "Apply the family at point as the prose font."
  (interactive)
  (if-let ((family (ps/fonts--preview-family-at-point)))
      (ps/font-try 'prose family)
    (user-error "Point is not inside a font block")))

(defun ps/font-preview-refresh ()
  "Redraw the preview, keeping point where it is."
  (interactive)
  (let ((line (line-number-at-pos)))
    (ps/fonts--preview-render)
    (forward-line (1- line))))

(defun ps/font-preview-revert ()
  "Undo every audition and go back to what the settings say."
  (interactive)
  (ps/fonts-apply)
  (ps/font-preview-refresh)
  (message "Back to the fonts config.org names"))

(defvar ps/font-preview-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "m") #'ps/font-preview-use-mono)
    (define-key map (kbd "p") #'ps/font-preview-use-prose)
    (define-key map (kbd "s") #'ps/font-size-try)
    (define-key map (kbd "g") #'ps/font-preview-refresh)
    (define-key map (kbd "r") #'ps/font-preview-revert)
    map)
  "Keymap for `ps-font-preview-mode'.")

(define-derived-mode ps-font-preview-mode special-mode "Font Preview"
  "Major mode for the *Font Preview* buffer."
  (setq truncate-lines t))

;;;###autoload
(defun ps/font-preview ()
  "Show the same sample rendered in every candidate font, to compare them.
Applying one from here is an audition only; set `ps/font-mono' /
`ps/font-prose' in Settings to keep it, or press `r' to go back."
  (interactive)
  (let ((buffer (get-buffer-create "*Font Preview*")))
    (with-current-buffer buffer
      (unless (eq major-mode 'ps-font-preview-mode)
        (ps-font-preview-mode))
      (ps/fonts--preview-render))
    (ps/window-show-here buffer)))

(provide 'ps-fonts)
;;; ps-fonts.el ends here
