;;; ps-line-numbers.el --- Quieter line-number gutter in Org buffers -*- lexical-binding: t; -*-

;;; Commentary:

;; Line numbers are on globally (`global-display-line-numbers-mode'), which is
;; right for code but loud in an Org prose buffer: the digits are as large as
;; the text, and themes tend to draw the *current* line's number in the body
;; text colour -- under Solarized `line-number-current-line' is base0, the
;; brightest thing in the gutter.  This shrinks the gutter font and mutes both
;; faces, in Org buffers only.
;;
;; `display-line-numbers' looks its faces up through the buffer's
;; `face-remapping-alist' -- that is why line numbers already scale with
;; `text-scale-mode' -- so a buffer-local `face-remap-add-relative' is the way
;; to restyle the gutter in some buffers and not others.  There is no
;; per-buffer setting for it; remapping is the mechanism.
;;
;; One detail is load-bearing.  `line-number-current-line' is defined as
;; `:inherit line-number', and `:inherit' resolves named faces through
;; `face-remapping-alist' too -- so a `:height' on *both* entries multiplies
;; (0.8 * 0.8 = 0.64) and the current line's number comes out visibly smaller
;; than its neighbours.  The height therefore goes on `line-number' alone and
;; reaches the current line by inheritance; the second entry carries only the
;; colour, which it must, because a theme's explicit `:foreground' there would
;; otherwise win (remap specs take precedence over the base face, but only for
;; the attributes they actually set).

;;; Code:

(require 'face-remap)

;;; Customization

(defcustom ps/line-numbers-scale 0.8
  "Height of line numbers in Org buffers, relative to the body text.
A value of 1 leaves the size alone.  Note that the gutter's width in
pixels follows the digit width, so a smaller value also narrows the
gutter and shifts the text left; the width in *columns* is
`display-line-numbers-width', set in config.org."
  :type 'float)

(defcustom ps/line-numbers-face 'shadow
  "Face the Org line-number gutter takes its colour from.
The face is inherited rather than copied, so the colour follows the active
theme.  nil leaves the gutter's colours alone."
  :type '(choice (const :tag "Leave colours alone" nil) face))

;;; Remapping

(defun ps/line-numbers--spec (scale face with-height)
  "Return a face-remap spec for the line-number gutter, or nil for none.
FACE, when non-nil and defined, is inherited for its colours.  SCALE is
applied as a relative `:height' when WITH-HEIGHT is non-nil and SCALE is a
positive ratio other than 1.  `:inherit' comes first so that a height on
the inherited face cannot override SCALE."
  (append
   (and face (facep face) (list :inherit face))
   (and with-height (numberp scale) (> scale 0) (/= scale 1.0)
        (list :height scale))))

(defvar-local ps/line-numbers--cookies nil
  "Face-remap cookies this module applied to the current buffer.")

;;;###autoload
(defun ps/line-numbers-enable ()
  "Apply the quiet line-number style to the current buffer.
Does nothing visible in a buffer without a line-number gutter.  Safe to
call more than once: relative `:height' remappings stack multiplicatively,
so any remapping left from a previous call is removed first."
  (mapc #'face-remap-remove-relative ps/line-numbers--cookies)
  (setq ps/line-numbers--cookies nil)
  (let ((base (ps/line-numbers--spec ps/line-numbers-scale ps/line-numbers-face t))
        (current (ps/line-numbers--spec ps/line-numbers-scale ps/line-numbers-face nil)))
    (when base
      (push (apply #'face-remap-add-relative 'line-number base)
            ps/line-numbers--cookies))
    (when current
      (push (apply #'face-remap-add-relative 'line-number-current-line current)
            ps/line-numbers--cookies))))

(provide 'ps-line-numbers)
;;; ps-line-numbers.el ends here
