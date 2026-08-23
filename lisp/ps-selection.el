;;; ps-selection.el --- How the selection looks -*- lexical-binding: t; -*-

;;; Commentary:
;; Two things about the region that make Emacs feel unlike every other
;; editor, both fixed here:
;;
;; 1. It disappears the moment another window is selected.  The selection is
;;    still there -- come back and it is exactly where it was -- but Emacs
;;    only draws the region in the *selected* window
;;    (`highlight-nonselected-windows' defaults to nil), so it reads as a
;;    selection that was silently thrown away.  Turning that variable on is
;;    not the fix: it draws the region in full strength everywhere, which is
;;    the opposite of what other applications do.  They keep it visible in a
;;    dimmed colour, so we do the same with an overlay in
;;    `ps/selection-inactive', added when a window stops being selected and
;;    removed when it is selected again.  Emacs draws nothing itself while
;;    the window is unselected, so the two can never both appear.
;;
;; 2. It is drawn too heavily.  Solarized's `region' sets a *foreground* as
;;    well as a background, so every piece of text inside a selection loses
;;    its own colour -- headings, links and org-modern's TODO pills all turn
;;    inside out.  `ps/selection-apply' keeps the theme's hue but washes it
;;    toward the page background and drops the foreground, so a selection
;;    tints the text it covers instead of replacing it.  The colour is
;;    *derived* from whatever theme is loaded rather than hardcoded, and
;;    re-derived whenever the theme changes; blending toward the page
;;    background (rather than lightening) is what makes it work on a dark
;;    theme too.  The wash is then neutralised (`ps/selection-neutral'),
;;    because a washed colour keeps its hue -- Solarized's blue-grey over a
;;    cream page lands on a green-grey, which reads as a colour rather than
;;    as a highlight.
;;
;; 3. A selection repaints org-modern's TODO and priority pills.  Those two
;;    faces are drawn with `:inverse-video', which does not name the label's
;;    text colour at all -- it paints the text in whatever background is *in
;;    effect* at that spot, and inside a selection that is the selection's own
;;    colour, so the letters vanish while every other pill (tags, dates, DONE)
;;    stays readable, those naming a `:foreground' outright.  Giving the face
;;    its inherited background directly is *not* enough (the selection still
;;    wins the merge), so `ps/selection--flatten-face' resolves the inversion
;;    instead: it reads the colours the face would have swapped and states
;;    them plainly, with `:inverse-video' off.  The label then has a real
;;    foreground of its own, which no selection can take away.
;;
;; Not fixable here: point must stay visible in the selected window -- an
;; Emacs display invariant, which `ultra-scroll' implements faithfully -- so
;; scrolling far past an active region drags its end along.  That is why
;; `lisp/ps-claude.el' remembers the last region rather than trusting the
;; live one.

;;; Code:

(require 'color)
(require 'seq)

(defgroup ps/selection nil
  "How the selection (the region) is drawn."
  :group 'ps)

(defcustom ps/selection-pale 0.55
  "How far the selection colour is washed toward the page background.
0.0 keeps the theme's own selection colour, 1.0 makes it invisible.  The
default is pale enough that text inside a selection keeps its own colours."
  :type 'number
  :group 'ps/selection)

(defcustom ps/selection-inactive-pale 0.7
  "How far the selection is washed out in a window that is not selected.
Higher than `ps/selection-pale', so a selection you left behind is visible
but clearly not the one you are working with."
  :type 'number
  :group 'ps/selection)

(defcustom ps/selection-keep-foreground nil
  "When non-nil, keep the theme's selection foreground colour.
Off by default: a selection that repaints the text under it is what makes
headings and TODO pills turn inside out when you select them."
  :type 'boolean
  :group 'ps/selection)

(defcustom ps/selection-show-inactive t
  "When non-nil, keep the selection visible after its window loses focus."
  :type 'boolean
  :group 'ps/selection)

(defcustom ps/selection-dim-unfocused t
  "When non-nil, dim the selection while Emacs itself is not the focused app.
What every other application does, and the reason is practical: with the
assistant's panel or another app in front, a selection drawn at full
strength claims attention it no longer deserves."
  :type 'boolean
  :group 'ps/selection)

(defcustom ps/selection-neutral 1.0
  "How much of the selection colour's hue to remove, from 0.0 to 1.0.
Washing a coloured selection toward the page keeps its hue, so Solarized's
blue-grey ends up green-grey over a cream page.  1.0 leaves a neutral grey
that reads as a highlight rather than as a colour; 0.0 keeps the theme's
hue."
  :type 'number
  :group 'ps/selection)

(defcustom ps/selection-pinned-faces '(org-modern-todo org-modern-priority)
  "Faces whose text colour must not follow the selection.
These are drawn with `:inverse-video', which never names a text colour --
it paints the text in whatever background is in effect, which inside a
selection is the selection's own colour.  Each is rewritten with the
colours it would have swapped stated plainly instead."
  :type '(repeat face)
  :group 'ps/selection)

(defface ps/selection-inactive '((t :inherit region))
  "Face for the selection in a window that is not the selected one.
Recoloured by `ps/selection-apply' from the theme's own selection colour."
  :group 'ps/selection)

(defvar ps/selection--colors nil
  "Cons of the (ACTIVE . INACTIVE) selection backgrounds now in use.")

(defvar ps/selection--source nil
  "Cons of (BACKGROUND . FOREGROUND) of `region' as the theme defines it.
Captured once and re-captured on a theme change, so re-applying can never
wash an already-washed colour a second time.")

;;; Colours

(defun ps/selection--rgb (color)
  "Return COLOR as a list of three 0.0-1.0 components, or nil.
Hex strings are parsed directly rather than through `color-name-to-rgb',
which quantises to what the *current* display can show -- on a frameless
Emacs that turns every colour into black, white or a primary, so the
arithmetic below could not be trusted or tested there.  Named colours still
go through Emacs, which is the only thing that knows them."
  (when (stringp color)
    (if (string-match "\\`#\\([0-9a-fA-F]+\\)\\'" color)
        (let* ((digits (match-string 1 color))
               (width (/ (length digits) 3)))
          (when (and (> width 0) (= (* width 3) (length digits)))
            (let ((scale (float (1- (expt 16 width)))))
              (list (/ (string-to-number (substring digits 0 width) 16) scale)
                    (/ (string-to-number (substring digits width (* 2 width)) 16) scale)
                    (/ (string-to-number (substring digits (* 2 width)) 16) scale)))))
      (color-name-to-rgb color))))

(defun ps/selection--neutral (color amount)
  "Return COLOR with AMOUNT (0.0-1.0) of its colourfulness removed.
Lightness is preserved, so only the hue goes -- a wash that was faintly
green becomes the same grey.  Pure."
  (let ((rgb (ps/selection--rgb color))
        (share (max 0.0 (min 1.0 (or amount 0.0)))))
    (if (or (null rgb) (zerop share))
        color
      (let* ((hsl (apply #'color-rgb-to-hsl rgb))
             (drained (color-hsl-to-rgb (nth 0 hsl)
                                        (* (nth 1 hsl) (- 1.0 share))
                                        (nth 2 hsl))))
        (apply #'color-rgb-to-hex (append drained (list 2)))))))

(defun ps/selection--blend (color background fraction)
  "Return COLOR moved FRACTION of the way toward BACKGROUND, as a hex string.
Returns nil when either colour cannot be read.  Blending toward the page
rather than lightening is what makes one setting work on light and dark
themes alike.  Pure."
  (let ((from (ps/selection--rgb color))
        (to (ps/selection--rgb background))
        (amount (max 0.0 (min 1.0 (or fraction 0.0)))))
    (when (and from to)
      (apply #'color-rgb-to-hex
             (append (seq-mapn (lambda (a b) (+ (* a (- 1.0 amount)) (* b amount)))
                               from to)
                     (list 2))))))

(defun ps/selection--wash (color background fraction)
  "Return COLOR washed FRACTION toward BACKGROUND and neutralised."
  (ps/selection--neutral (ps/selection--blend color background fraction)
                         ps/selection-neutral))

(defun ps/selection--capture ()
  "Remember the selection colours as the current theme defines them."
  (setq ps/selection--source
        (cons (face-attribute 'region :background nil t)
              (face-attribute 'region :foreground nil t))))

(defun ps/selection--color-p (color)
  "Non-nil if COLOR is a colour Emacs can actually paint with."
  (and (stringp color) (not (string-prefix-p "unspecified" color))))

(defun ps/selection--page-background ()
  "Return the page background, or nil if this Emacs has no real colours."
  (let ((background (face-attribute 'default :background nil t)))
    (if (ps/selection--color-p background)
        background
      (let ((parameter (frame-parameter nil 'background-color)))
        (and (ps/selection--color-p parameter) parameter)))))

(defun ps/selection--flatten-face (face)
  "State FACE's inverted colours plainly, so a selection cannot repaint it.
`:inverse-video' names no text colour: the text is painted in whatever
background is in effect, which inside a selection is the selection's own.
Reading the two colours it would have swapped and setting them directly
gives the label a foreground of its own.

Deliberately does not clear the face first: setting an attribute to
`unspecified' *erases* it rather than restoring what the theme said, which
would lose the very colours this reads.  Nothing needs clearing anyway --
a face this has already flattened no longer declares `:inverse-video', so
running again leaves it alone, and enabling a theme re-applies its own
spec (inversion included), which is exactly when it should be redone.
Returns non-nil when FACE exists, whether or not it needed this."
  (when (facep face)
    (let ((foreground (face-attribute face :foreground nil t))
          (background (face-attribute face :background nil t))
          (inverted (eq (face-attribute face :inverse-video nil t) t)))
      (when (and inverted (ps/selection--color-p foreground))
        (let ((text (if (ps/selection--color-p background)
                        background
                      (ps/selection--page-background))))
          (when text
            (set-face-attribute face nil :inverse-video nil
                                :background foreground :foreground text)))))
    t))

(defun ps/selection--flatten-faces ()
  "Flatten every face in `ps/selection-pinned-faces'.  Non-nil if any exist."
  (let (found)
    (dolist (face ps/selection-pinned-faces found)
      (when (ps/selection--flatten-face face) (setq found t)))))

(defun ps/selection--flatten-faces-once ()
  "Flatten the label faces the first time an Org buffer makes them exist."
  (when (ps/selection--flatten-faces)
    (remove-hook 'org-mode-hook #'ps/selection--flatten-faces-once)))

(defun ps/selection--frame-focused-p ()
  "Non-nil if any frame has the input focus.
A frame whose focus state is unknown counts as focused: guessing that
Emacs is away would leave the selection dimmed on a platform that cannot
tell us."
  (seq-some (lambda (frame) (frame-focus-state frame)) (frame-list)))

(defun ps/selection--update-focus (&rest _)
  "Dim the selection while Emacs is not the focused application."
  (when (and ps/selection-dim-unfocused ps/selection--colors)
    (set-face-attribute 'region nil :background
                        (if (ps/selection--frame-focused-p)
                            (car ps/selection--colors)
                          (cdr ps/selection--colors)))))

(defun ps/selection-apply ()
  "Recolour the selection from the theme currently loaded.
Run again after changing `ps/selection-pale' or its companions."
  (interactive)
  (unless ps/selection--source (ps/selection--capture))
  (let* ((background (face-attribute 'default :background nil t))
         (source (car ps/selection--source))
         (active (ps/selection--wash source background ps/selection-pale))
         (inactive (ps/selection--wash source background
                                       ps/selection-inactive-pale)))
    (when active
      (set-face-attribute 'region nil :background active :extend t
                          :foreground (if ps/selection-keep-foreground
                                          (cdr ps/selection--source)
                                        'unspecified)))
    (when inactive
      (set-face-attribute 'ps/selection-inactive nil :background inactive
                          :foreground 'unspecified :extend t))
    (when (and active inactive)
      (setq ps/selection--colors (cons active inactive))
      (ps/selection--update-focus))
    (ps/selection--flatten-faces)))

(defun ps/selection--on-theme-change (&rest _)
  "Re-derive the selection colours after the colour theme changed."
  (setq ps/selection--source nil)
  (ps/selection-apply))

;;; Keeping the selection visible while its window is not selected

(defvar-local ps/selection--overlay nil
  "Overlay showing this buffer's selection while its window is unselected.")

(defun ps/selection--hide ()
  "Remove the dimmed selection overlay from the current buffer."
  (when (overlayp ps/selection--overlay)
    (delete-overlay ps/selection--overlay))
  (setq ps/selection--overlay nil))

(defun ps/selection--show ()
  "Draw the dimmed selection overlay over this buffer's region, if any."
  (if (and ps/selection-show-inactive (region-active-p))
      (let ((start (region-beginning))
            (end (region-end)))
        (if (overlayp ps/selection--overlay)
            (move-overlay ps/selection--overlay start end (current-buffer))
          (setq ps/selection--overlay (make-overlay start end nil nil t))
          (overlay-put ps/selection--overlay 'face 'ps/selection-inactive)
          (overlay-put ps/selection--overlay 'ps/selection t)))
    (ps/selection--hide)))

(defun ps/selection--windows (frame-or-window)
  "Return the windows FRAME-OR-WINDOW stands for.
`window-selection-change-functions' hands its *default* value a frame --
only a buffer-local registration is handed a window -- so a handler that
assumed a window would never fire at all.  Pure."
  (cond ((framep frame-or-window) (window-list frame-or-window 'no-mini))
        ((window-live-p frame-or-window) (list frame-or-window))))

(defun ps/selection--refresh-window (window)
  "Show or hide WINDOW's dimmed selection, depending on whether it is selected."
  (when (window-live-p window)
    (with-current-buffer (window-buffer window)
      (if (eq window (selected-window))
          (ps/selection--hide)
        (ps/selection--show)))))

(defun ps/selection--refresh (frame-or-window)
  "Refresh the dimmed selection for every window FRAME-OR-WINDOW covers."
  (dolist (window (ps/selection--windows frame-or-window))
    (ps/selection--refresh-window window)))

(defun ps/selection-setup ()
  "Recolour the selection and keep it visible when its window loses focus.
Idempotent; call after the colour theme has been loaded."
  ;; Emacs must not draw the region itself in unselected windows: the dimmed
  ;; overlay below is what those windows show.
  (setq highlight-nonselected-windows nil)
  (ps/selection-apply)
  (add-hook 'window-selection-change-functions #'ps/selection--refresh)
  (add-hook 'deactivate-mark-hook #'ps/selection--hide)
  ;; Late in `org-mode-hook', so `org-modern' -- and this config's own tuning
  ;; of the faces it draws pills with -- has loaded by the time we rewrite them.
  (add-hook 'org-mode-hook #'ps/selection--flatten-faces-once 90)
  (add-function :after after-focus-change-function #'ps/selection--update-focus)
  (when (boundp 'enable-theme-functions)
    (add-hook 'enable-theme-functions #'ps/selection--on-theme-change)))

(provide 'ps-selection)
;;; ps-selection.el ends here
