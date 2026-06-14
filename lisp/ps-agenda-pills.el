;;; ps-agenda-pills.el --- Rounded "pill" badge images for the agenda -*- lexical-binding: t; -*-

;;; Commentary:
;; Renders small rounded-rectangle "pill" badges (the TODO/NEXT/#A/"11d ago"
;; tags in the redesigned agenda) as in-memory SVG images, reusing the same
;; `create-image' 'svg pipeline as `ps-material-icons'.  Each pill is sized in
;; the SVG to a whole number of character columns and pinned to the text line
;; height, so it lines up with the surrounding text and a following
;; `(space :align-to COL)' can snap the next column into place regardless of the
;; pill's exact rendered width.  Images are cached by spec key.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defgroup ps-agenda-pills nil
  "Rounded pill badges for the org agenda."
  :group 'ps)

(defcustom ps/agenda-pills-pad 1
  "Horizontal padding, in character columns, added on each side of a pill label."
  :type 'integer
  :group 'ps-agenda-pills)

(defcustom ps/agenda-pills-radius-scale 0.45
  "Pill corner radius as a fraction of the line height."
  :type 'number
  :group 'ps-agenda-pills)

(defcustom ps/agenda-pills-font-scale 0.78
  "Pill label font size as a fraction of the line height."
  :type 'number
  :group 'ps-agenda-pills)

(defcustom ps/agenda-pills-font-family nil
  "Font family for pill labels, or nil to use the default face family."
  :type '(choice (const :tag "Default face family" nil) string)
  :group 'ps-agenda-pills)

(defvar ps/agenda-pills--cache (make-hash-table :test 'equal)
  "Cache mapping a pill spec key to its image.")

(defun ps/agenda-pills-reset-cache ()
  "Drop all cached pill images.  Call after a font-size or theme change."
  (clrhash ps/agenda-pills--cache))

;;; Metrics (fall back to fixed values when there is no graphical display)

(defun ps/agenda-pills--char-width ()
  "Default character width in pixels (8 in batch)."
  (if (display-graphic-p) (default-font-width) 8))

(defun ps/agenda-pills--line-height ()
  "Default line height in pixels (16 in batch)."
  (if (display-graphic-p) (default-font-height) 16))

(defun ps/agenda-pills-columns (label)
  "Return the integer column width a pill for LABEL occupies."
  (+ (string-width label) (* 2 ps/agenda-pills-pad)))

;;; SVG

(defun ps/agenda-pills--escape (s)
  "XML-escape S for embedding in an SVG text element."
  (replace-regexp-in-string
   "[&<>\"]"
   (lambda (m)
     (pcase m ("&" "&amp;") ("<" "&lt;") (">" "&gt;") ("\"" "&quot;") (_ m)))
   s t t))

(defun ps/agenda-pills--svg (label cols bg fg stroke cw ch)
  "Return an SVG string for a pill of LABEL spanning COLS columns.
BG/FG are fill colors, STROKE an optional border color.  CW/CH are the
character width and line height the geometry is derived from."
  (let* ((w (* cols cw))
         (h ch)
         (r (max 1 (round (* ps/agenda-pills-radius-scale h))))
         (fs (max 6 (round (* ps/agenda-pills-font-scale h))))
         (baseline (round (* h 0.72)))
         (family (or ps/agenda-pills-font-family
                     (face-attribute 'default :family nil t)
                     "sans-serif")))
    (format
     (concat
      "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"%d\" height=\"%d\">"
      "<rect x=\"0.5\" y=\"0.5\" width=\"%d\" height=\"%d\" rx=\"%d\" ry=\"%d\""
      " fill=\"%s\"%s/>"
      "<text x=\"%d\" y=\"%d\" font-family=\"%s\" font-size=\"%d\" fill=\"%s\""
      " text-anchor=\"middle\">%s</text>"
      "</svg>")
     w h
     (max 1 (- w 1)) (max 1 (- h 1)) r r bg
     (if stroke (format " stroke=\"%s\" stroke-width=\"1\"" stroke) "")
     (/ w 2) baseline (ps/agenda-pills--escape family) fs fg
     (ps/agenda-pills--escape label))))

(cl-defun ps/agenda-pills-image (label &key bg fg stroke (ascent 'center))
  "Return an SVG image of a rounded pill showing LABEL.
BG/FG/STROKE are colors; ASCENT sets the vertical alignment.  Returns nil for an
empty LABEL.  Results are cached."
  (when (and label (stringp label) (not (string-empty-p label)))
    (let* ((cw (ps/agenda-pills--char-width))
           (ch (ps/agenda-pills--line-height))
           (cols (ps/agenda-pills-columns label))
           (bg (or bg "#dddddd"))
           (fg (or fg "#333333"))
           (key (list label cols bg fg stroke ascent cw ch
                      ps/agenda-pills-radius-scale ps/agenda-pills-font-scale
                      ps/agenda-pills-font-family)))
      (or (gethash key ps/agenda-pills--cache)
          (puthash key
                   (create-image (ps/agenda-pills--svg label cols bg fg stroke cw ch)
                                 'svg t :ascent ascent :height ch)
                   ps/agenda-pills--cache)))))

(provide 'ps-agenda-pills)
;;; ps-agenda-pills.el ends here
