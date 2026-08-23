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
;;    theme too.
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

(defcustom ps/selection-pale 0.7
  "How far the selection colour is washed toward the page background.
0.0 keeps the theme's own selection colour, 1.0 makes it invisible.  The
default is pale enough that text inside a selection keeps its own colours."
  :type 'number
  :group 'ps/selection)

(defcustom ps/selection-inactive-pale 0.85
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

(defface ps/selection-inactive '((t :inherit region))
  "Face for the selection in a window that is not the selected one.
Recoloured by `ps/selection-apply' from the theme's own selection colour."
  :group 'ps/selection)

(defvar ps/selection--source nil
  "Cons of (BACKGROUND . FOREGROUND) of `region' as the theme defines it.
Captured once and re-captured on a theme change, so re-applying can never
wash an already-washed colour a second time.")

;;; Colours

(defun ps/selection--blend (color background fraction)
  "Return COLOR moved FRACTION of the way toward BACKGROUND, as a hex string.
Returns nil when either colour cannot be read.  Blending toward the page
rather than lightening is what makes one setting work on light and dark
themes alike.  Pure."
  (let ((from (and (stringp color) (color-name-to-rgb color)))
        (to (and (stringp background) (color-name-to-rgb background)))
        (amount (max 0.0 (min 1.0 (or fraction 0.0)))))
    (when (and from to)
      (apply #'color-rgb-to-hex
             (append (seq-mapn (lambda (a b) (+ (* a (- 1.0 amount)) (* b amount)))
                               from to)
                     (list 2))))))

(defun ps/selection--capture ()
  "Remember the selection colours as the current theme defines them."
  (setq ps/selection--source
        (cons (face-attribute 'region :background nil t)
              (face-attribute 'region :foreground nil t))))

(defun ps/selection-apply ()
  "Recolour the selection from the theme currently loaded.
Run again after changing `ps/selection-pale' or its companions."
  (interactive)
  (unless ps/selection--source (ps/selection--capture))
  (let* ((background (face-attribute 'default :background nil t))
         (source (car ps/selection--source))
         (active (ps/selection--blend source background ps/selection-pale))
         (inactive (ps/selection--blend source background
                                        ps/selection-inactive-pale)))
    (when active
      (set-face-attribute 'region nil :background active :extend t
                          :foreground (if ps/selection-keep-foreground
                                          (cdr ps/selection--source)
                                        'unspecified)))
    (when inactive
      (set-face-attribute 'ps/selection-inactive nil :background inactive
                          :foreground 'unspecified :extend t))))

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
  (when (boundp 'enable-theme-functions)
    (add-hook 'enable-theme-functions #'ps/selection--on-theme-change)))

(provide 'ps-selection)
;;; ps-selection.el ends here
