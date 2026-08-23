;;; test-ps-selection.el --- ERT tests for ps-selection -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(add-to-list 'load-path "lisp")
(require 'ps-selection)

;;; Colour blending

(ert-deftest ps/selection-test-blend-endpoints ()
  "Fraction 0 keeps the colour; fraction 1 returns the background."
  (should (equal (ps/selection--blend "#000000" "#ffffff" 0.0) "#000000"))
  (should (equal (ps/selection--blend "#000000" "#ffffff" 1.0) "#ffffff")))

(ert-deftest ps/selection-test-blend-midpoint ()
  "Half way between black and white is grey, not a lightened black."
  (should (equal (ps/selection--blend "#000000" "#ffffff" 0.5) "#7f7f7f")))

(ert-deftest ps/selection-test-blend-moves-toward-a-dark-background ()
  "On a dark theme the wash darkens rather than lightens."
  (should (equal (ps/selection--blend "#ffffff" "#000000" 0.75) "#3f3f3f"))
  ;; A real pair, which only works because hex is parsed rather than looked up.
  (should (equal (ps/selection--blend "#586e75" "#fdf6e3" 0.55) "#b2b8b1")))

(ert-deftest ps/selection-test-blend-clamps-the-fraction ()
  "Out-of-range fractions are clamped instead of producing invalid colours."
  (should (equal (ps/selection--blend "#000000" "#ffffff" -1.0) "#000000"))
  (should (equal (ps/selection--blend "#000000" "#ffffff" 2.0) "#ffffff"))
  (should (equal (ps/selection--blend "#000000" "#ffffff" nil) "#000000")))

(ert-deftest ps/selection-test-blend-tolerates-unspecified ()
  "An unspecified face colour yields nil rather than signalling."
  (should-not (ps/selection--blend 'unspecified "#ffffff" 0.5))
  (should-not (ps/selection--blend "#000000" 'unspecified 0.5))
  (should-not (ps/selection--blend nil nil 0.5)))

;;; Applying the colours

(ert-deftest ps/selection-test-apply-washes-and-drops-the-foreground ()
  "The region keeps the theme's hue, washed toward the page, and stops
repainting the text under it -- Solarized sets a foreground, which is what
turned headings and TODO pills inside out inside a selection."
  (let ((ps/selection--source nil)
        (ps/selection-pale 0.5)
        (ps/selection-inactive-pale 0.75)
        (ps/selection-keep-foreground nil)
        (ps/selection-dim-unfocused nil)
        (ps/selection-neutral 0.0)
        (applied nil))
    (cl-letf (((symbol-function 'face-attribute)
               (lambda (face attribute &rest _)
                 (cond ((and (eq face 'region) (eq attribute :background)) "#000000")
                       ((and (eq face 'region) (eq attribute :foreground)) "#ffffff")
                       ((and (eq face 'default) (eq attribute :background)) "#ffffff")
                       (t 'unspecified))))
              ((symbol-function 'set-face-attribute)
               (lambda (face _frame &rest args) (push (cons face args) applied))))
      (ps/selection-apply))
    (let ((region (alist-get 'region applied))
          (inactive (alist-get 'ps/selection-inactive applied)))
      (should (equal (plist-get region :background) "#7f7f7f"))
      (should (eq (plist-get region :foreground) 'unspecified))
      (should (eq (plist-get region :extend) t))
      (should (equal (plist-get inactive :background) "#bfbfbf")))))

(ert-deftest ps/selection-test-apply-can-keep-the-foreground ()
  "Asking for the theme's foreground back restores it verbatim."
  (let ((ps/selection--source nil)
        (ps/selection-pale 0.5)
        (ps/selection-keep-foreground t)
        (ps/selection-dim-unfocused nil)
        (ps/selection-neutral 0.0)
        (applied nil))
    (cl-letf (((symbol-function 'face-attribute)
               (lambda (face attribute &rest _)
                 (cond ((and (eq face 'region) (eq attribute :background)) "#000000")
                       ((and (eq face 'region) (eq attribute :foreground)) "#ffffff")
                       ((and (eq face 'default) (eq attribute :background)) "#ffffff")
                       (t 'unspecified))))
              ((symbol-function 'set-face-attribute)
               (lambda (face _frame &rest args) (push (cons face args) applied))))
      (ps/selection-apply))
    (should (equal (plist-get (alist-get 'region applied) :foreground) "#ffffff"))))

(ert-deftest ps/selection-test-apply-tolerates-an-unthemed-display ()
  "With no colours to read (a terminal, a batch frame) nothing is set."
  (let ((ps/selection--source nil)
        (applied nil))
    (cl-letf (((symbol-function 'face-attribute) (lambda (&rest _) 'unspecified))
              ((symbol-function 'set-face-attribute)
               (lambda (face &rest _) (push face applied))))
      (ps/selection-apply))
    (should-not applied)))

(ert-deftest ps/selection-test-theme-change-recaptures ()
  "A theme change forgets the captured colours so they are re-derived."
  (let ((ps/selection--source (cons "#123456" "#abcdef")))
    (cl-letf (((symbol-function 'ps/selection-apply) #'ignore))
      (ps/selection--on-theme-change 'some-theme))
    (should-not ps/selection--source)))

;;; Faces a selection must not repaint

(defface ps/selection-test-inherited-face
  '((t :foreground "#112233" :background "#445566"))
  "Stand-in for a face an inverse-video label inherits its colours from.")

(defface ps/selection-test-label-face
  '((t :inherit ps/selection-test-inherited-face :inverse-video t))
  "Stand-in for org-modern's TODO pill: colours inherited, inverse video.")

(ert-deftest ps/selection-test-flatten-face-states-the-swapped-colours ()
  "The inversion is resolved into plain colours the label owns.
That is the whole fix: `:inverse-video' names no text colour, so the label
was drawn in whatever background was in effect -- the selection's own,
inside a selection, which made TODO keywords unreadable there."
  (set-face-attribute 'ps/selection-test-inherited-face nil
                      :foreground "#112233" :background "#445566")
  (set-face-attribute 'ps/selection-test-label-face nil
                      :foreground 'unspecified :background 'unspecified
                      :inverse-video t)
  (should (ps/selection--flatten-face 'ps/selection-test-label-face))
  ;; The fill is what the inversion would have used it for...
  (should (equal (face-attribute 'ps/selection-test-label-face :background nil nil)
                 "#112233"))
  ;; ...and the text now has a colour of its own instead of borrowing one.
  (should (equal (face-attribute 'ps/selection-test-label-face :foreground nil nil)
                 "#445566"))
  (should-not (face-attribute 'ps/selection-test-label-face :inverse-video nil nil)))

(ert-deftest ps/selection-test-flatten-face-follows-a-theme-change ()
  "Flattening again after the inherited colours changed picks up the new
ones, rather than freezing the first values it ever saw."
  (set-face-attribute 'ps/selection-test-inherited-face nil
                      :foreground "#112233" :background "#445566")
  (ps/selection--flatten-face 'ps/selection-test-label-face)
  (set-face-attribute 'ps/selection-test-inherited-face nil
                      :foreground "#778899" :background "#aabbcc")
  ;; Enabling a theme re-applies the face's own spec, inversion included --
  ;; which is the state flattening has to cope with a second time.
  (set-face-attribute 'ps/selection-test-label-face nil
                      :foreground 'unspecified :background 'unspecified
                      :inverse-video t)
  (ps/selection--flatten-face 'ps/selection-test-label-face)
  (should (equal (face-attribute 'ps/selection-test-label-face :background nil nil)
                 "#778899"))
  (should (equal (face-attribute 'ps/selection-test-label-face :foreground nil nil)
                 "#aabbcc")))

(ert-deftest ps/selection-test-flatten-face-leaves-plain-faces-alone ()
  "A face that never inverted anything is not rewritten."
  (set-face-attribute 'ps/selection-test-inherited-face nil
                      :foreground "#112233" :background "#445566")
  (should (ps/selection--flatten-face 'ps/selection-test-inherited-face))
  (should (equal (face-attribute 'ps/selection-test-inherited-face :foreground nil nil)
                 "#112233"))
  (should (equal (face-attribute 'ps/selection-test-inherited-face :background nil nil)
                 "#445566"))
  ;; Already flattened: a second pass is a no-op, not another swap.
  (set-face-attribute 'ps/selection-test-label-face nil
                      :foreground 'unspecified :background 'unspecified
                      :inverse-video t)
  (ps/selection--flatten-face 'ps/selection-test-label-face)
  (let ((background (face-attribute 'ps/selection-test-label-face :background nil nil))
        (foreground (face-attribute 'ps/selection-test-label-face :foreground nil nil)))
    (ps/selection--flatten-face 'ps/selection-test-label-face)
    (should (equal (face-attribute 'ps/selection-test-label-face :background nil nil)
                   background))
    (should (equal (face-attribute 'ps/selection-test-label-face :foreground nil nil)
                   foreground))))

(ert-deftest ps/selection-test-flatten-face-skips-unknown-faces ()
  "A face the user has not installed is skipped rather than signalling."
  (should-not (ps/selection--flatten-face 'ps/selection-test-no-such-face))
  (let ((ps/selection-pinned-faces '(ps/selection-test-no-such-face)))
    (should-not (ps/selection--flatten-faces))))

;;; Reading colours without a display

(ert-deftest ps/selection-test-rgb-parses-hex-directly ()
  "Hex colours are parsed by us, not by `color-name-to-rgb', which
quantises to what the current display can show -- on a frameless Emacs it
reports this very colour as pure blue."
  (should (equal (color-name-to-rgb "#586e75") '(0.0 0.0 1.0)))
  (let ((rgb (ps/selection--rgb "#586e75")))
    (should (< (abs (- (nth 0 rgb) 0.345)) 0.001))
    (should (< (abs (- (nth 1 rgb) 0.431)) 0.001))
    (should (< (abs (- (nth 2 rgb) 0.459)) 0.001)))
  ;; Four-digit-per-component hex, which Emacs also produces.
  (should (equal (ps/selection--rgb "#ffffffffffff") '(1.0 1.0 1.0)))
  (should-not (ps/selection--rgb "#12345"))
  (should-not (ps/selection--rgb nil)))

;;; Neutralising the wash

(ert-deftest ps/selection-test-neutral-removes-the-hue ()
  "A washed colour keeps its hue -- Solarized's blue-grey over a cream page
lands on a green-grey -- so the hue is drained without touching lightness."
  (should (equal (ps/selection--neutral "#b2b8b1" 1.0) "#b4b4b4"))
  ;; Half way keeps some of it.
  (should-not (equal (ps/selection--neutral "#b2b8b1" 0.5) "#b4b4b4"))
  ;; Zero leaves the colour untouched, as does an unreadable one.
  (should (equal (ps/selection--neutral "#b2b8b1" 0.0) "#b2b8b1"))
  (should (equal (ps/selection--neutral 'unspecified 1.0) 'unspecified)))

(ert-deftest ps/selection-test-wash-is-grey-by-default ()
  "The end-to-end colour for Solarized Light: a neutral grey, not a green one."
  (let ((ps/selection-neutral 1.0))
    (should (equal (ps/selection--wash "#586e75" "#fdf6e3" 0.55) "#b4b4b4"))
    (should (equal (ps/selection--wash "#586e75" "#fdf6e3" 0.7) "#c7c7c7"))))

;;; Dimming while Emacs is not the focused application

(ert-deftest ps/selection-test-focus-state-treats-unknown-as-focused ()
  "A platform that cannot report focus must not leave the selection dimmed."
  (cl-letf (((symbol-function 'frame-focus-state) (lambda (&rest _) 'unknown)))
    (should (ps/selection--frame-focused-p)))
  (cl-letf (((symbol-function 'frame-focus-state) (lambda (&rest _) nil)))
    (should-not (ps/selection--frame-focused-p)))
  (cl-letf (((symbol-function 'frame-focus-state) (lambda (&rest _) t)))
    (should (ps/selection--frame-focused-p))))

(ert-deftest ps/selection-test-focus-swaps-the-region-colour ()
  "Losing focus dims the selection; regaining it restores it."
  (let ((ps/selection--colors (cons "#aaaaaa" "#dddddd"))
        (ps/selection-dim-unfocused t)
        applied)
    (cl-letf (((symbol-function 'set-face-attribute)
               (lambda (_face _frame &rest args) (setq applied args)))
              ((symbol-function 'frame-focus-state) (lambda (&rest _) nil)))
      (ps/selection--update-focus)
      (should (equal (plist-get applied :background) "#dddddd")))
    (cl-letf (((symbol-function 'set-face-attribute)
               (lambda (_face _frame &rest args) (setq applied args)))
              ((symbol-function 'frame-focus-state) (lambda (&rest _) t)))
      (ps/selection--update-focus)
      (should (equal (plist-get applied :background) "#aaaaaa")))))

(ert-deftest ps/selection-test-focus-dimming-can-be-turned-off ()
  "With the option off the selection keeps one colour throughout."
  (let ((ps/selection--colors (cons "#aaaaaa" "#dddddd"))
        (ps/selection-dim-unfocused nil)
        applied)
    (cl-letf (((symbol-function 'set-face-attribute)
               (lambda (_face _frame &rest args) (setq applied args)))
              ((symbol-function 'frame-focus-state) (lambda (&rest _) nil)))
      (ps/selection--update-focus)
      (should-not applied))))

;;; The dimmed overlay

(ert-deftest ps/selection-test-show-covers-the-region ()
  "An unselected window's region is covered by the dimmed overlay."
  (with-temp-buffer
    (insert "one\ntwo\nthree\n")
    (goto-char (point-min))
    (set-mark (point))
    (goto-char (point-max))
    (activate-mark)
    (unwind-protect
        (progn
          (ps/selection--show)
          (should (overlayp ps/selection--overlay))
          (should (eq (overlay-get ps/selection--overlay 'face)
                      'ps/selection-inactive))
          (should (equal (overlay-start ps/selection--overlay) (point-min)))
          (should (equal (overlay-end ps/selection--overlay) (point-max)))
          ;; Showing twice moves the one overlay rather than stacking them.
          (let ((overlay ps/selection--overlay))
            (ps/selection--show)
            (should (eq overlay ps/selection--overlay))))
      (ps/selection--hide))))

(ert-deftest ps/selection-test-show-without-a-region-hides ()
  "With no region there is nothing to keep visible."
  (with-temp-buffer
    (insert "one\ntwo\n")
    (deactivate-mark)
    (ps/selection--show)
    (should-not ps/selection--overlay)))

(ert-deftest ps/selection-test-show-respects-the-setting ()
  "Turning the feature off leaves no overlay behind."
  (with-temp-buffer
    (insert "one\ntwo\n")
    (goto-char (point-min))
    (set-mark (point))
    (goto-char (point-max))
    (activate-mark)
    (ps/selection--show)
    (should (overlayp ps/selection--overlay))
    (let ((ps/selection-show-inactive nil))
      (ps/selection--show)
      (should-not ps/selection--overlay))))

(ert-deftest ps/selection-test-hide-is-idempotent ()
  "Hiding twice, or with nothing shown, does not signal."
  (with-temp-buffer
    (ps/selection--hide)
    (should-not ps/selection--overlay)
    (ps/selection--hide)
    (should-not ps/selection--overlay)))

(ert-deftest ps/selection-test-refresh-hides-in-the-selected-window ()
  "The selected window shows Emacs's own region, so the overlay is removed."
  (let ((buffer (generate-new-buffer "*ps-selection-test*")))
    (unwind-protect
        (progn
          (set-window-buffer (selected-window) buffer)
          (with-current-buffer buffer
            (insert "one\ntwo\n")
            (goto-char (point-min))
            (set-mark (point))
            (goto-char (point-max))
            (activate-mark)
            (ps/selection--show)
            (should (overlayp ps/selection--overlay)))
          (ps/selection--refresh (selected-window))
          (with-current-buffer buffer
            (should-not ps/selection--overlay)))
      (set-window-buffer (selected-window) (get-buffer-create "*scratch*"))
      (kill-buffer buffer))))

(ert-deftest ps/selection-test-refresh-ignores-dead-windows ()
  "A window that is gone is skipped rather than signalling."
  (should-not (ps/selection--refresh nil)))

(ert-deftest ps/selection-test-windows-accepts-a-frame ()
  "The hook's default value is handed a frame, so a frame must resolve to
its windows -- a handler that only understood windows would never fire."
  (should (equal (ps/selection--windows (selected-window))
                 (list (selected-window))))
  (should (member (selected-window)
                  (ps/selection--windows (selected-frame))))
  (should-not (ps/selection--windows nil)))

(provide 'test-ps-selection)
;;; test-ps-selection.el ends here
