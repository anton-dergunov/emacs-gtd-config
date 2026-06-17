;;; ps-schedule-view.el --- Timeline and Events views for the Schedule section -*- lexical-binding: t; -*-

;;; Commentary:
;; Renders the org-agenda Schedule (time-grid) section in two styles:
;;
;;   Timeline — time axis with a tick at each grid interval:
;;     08:00-08:15 ┆ [cat] [STATE] [PRI] [emoji]  Title…
;;     08:00       ┆
;;     10:00-14:00 ┆ [cat] [STATE] [PRI] [emoji]  Title…
;;     ┄┄┄┄┄┄┄┄┄┄┄┄┆┄┄┄┄┄┄  now · 13:47  ┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄
;;     15:00       ┆ [cat] [STATE] [PRI] [emoji]  Title…   ⚠ overlap
;;
;;   Events — same but grid ticks hidden; only timed events shown.
;;
;; The ┆ bar is fixed at column 12 (0-indexed).  Task body uses the same
;; column geometry as ps-agenda-layout, shifted right by 14 columns.
;;
;; Requires ps-agenda-layout to be loaded first.  Sets
;; `ps/schedule-view-override' (declared in ps-agenda-layout) so the layout
;; pass leaves the Schedule section untouched.
;;
;; Auto-refresh: a timer fires at the start of each calendar minute so the
;; now-line and DONE-task visibility stay current.  Cursor and scroll are
;; preserved across refreshes.

;;; Code:
(require 'cl-lib)
(require 'subr-x)
(require 'ps-agenda-layout)

;; Functions from ps-agenda-layout used here (defined in that file).
(declare-function ps/agenda-layout--columns        "ps-agenda-layout" ())
(declare-function ps/agenda-layout--window-cols    "ps-agenda-layout" ())
(declare-function ps/agenda-layout--header-p       "ps-agenda-layout" ())
(declare-function ps/agenda-layout--header-schedule-p "ps-agenda-layout" (header))
(declare-function ps/agenda-layout--item-p         "ps-agenda-layout" ())
(declare-function ps/agenda-layout--title          "ps-agenda-layout" ())
(declare-function ps/agenda-layout--priority-char  "ps-agenda-layout" ())
(declare-function ps/agenda-layout--emoji          "ps-agenda-layout" (title))
(declare-function ps/agenda-layout--render-category "ps-agenda-layout" (cols))
(declare-function ps/agenda-layout--space-to       "ps-agenda-layout" (col))
(declare-function ps/agenda-layout--space-to-right "ps-agenda-layout" (cols))
(declare-function ps/agenda-layout--space-before-right "ps-agenda-layout" (badge cols))
(declare-function ps/agenda-layout--scope-has-priority-p "ps-agenda-layout" (schedule-scope))
(defvar ps/agenda-layout--reserve-priority)
(declare-function ps/agenda-layout--state-text     "ps-agenda-layout" (state))
(declare-function ps/agenda-layout--priority-text  "ps-agenda-layout" (char))
(declare-function ps/agenda-layout--tags-string    "ps-agenda-layout" (tags))
(declare-function ps/agenda-layout--truncate       "ps-agenda-layout" (s maxcols))
(declare-function ps/agenda-layout--replace-line   "ps-agenda-layout" (bol eol display))
(declare-function ps/agenda-layout--hide-line      "ps-agenda-layout" (bol eol))
(declare-function ps/agenda-layout--clear          "ps-agenda-layout" ())

(defvar ps/schedule-view-override)        ; declared below, defined here
(defvar org-agenda-finalize-hook)
(defvar ps/agenda-layout-left-margin-cols)
(defvar ps/agenda-layout-right-margin-cols)
(defvar ps/agenda-layout-truncate)
(defvar ps/agenda-layout-schedule-group)
(defvar ps/agenda-layout-emoji-face)

;;; Defgroup

(defgroup ps-schedule-view nil
  "Timeline and Events views for the org-agenda Schedule section."
  :group 'ps)

;;; Defcustoms

(defcustom ps/schedule-view-style 'timeline
  "Rendering style for the Schedule (time-grid) section.
`timeline' shows the full time axis with a tick at each grid interval.
`events' shows only timed events; grid-tick rows are hidden."
  :type '(choice (const :tag "Timeline" timeline)
                 (const :tag "Events"   events))
  :group 'ps-schedule-view)

(defcustom ps/schedule-view-auto-refresh t
  "When non-nil, refresh the agenda at the start of every calendar minute.
The refresh moves the now-indicator and removes tasks marked DONE.
Cursor and scroll position are preserved across refreshes."
  :type 'boolean
  :group 'ps-schedule-view)

(defcustom ps/schedule-view-bar-ascent 82
  "Vertical alignment (`:ascent', 0-100) of the dotted separator image.
The image is as tall as the tallest Schedule row, so it both draws the dotted
line and sets the row height.  `center' (50) pushes half of it below the
baseline and inflates rows; a text-baseline-ish value keeps rows compact.  Lower
it slightly if a hairline gap appears at the top of the line."
  :type 'integer
  :group 'ps-schedule-view)

;;; Faces

(defface ps/schedule-view-small
  '((t :height 0.9))
  "Base face holding the (slightly smaller) size of schedule time labels.
The single place to tune the time-column/now font size; other faces inherit it
so there is no per-face hardcoded number."
  :group 'ps-schedule-view)

(defface ps/schedule-view-grid
  '((t :inherit shadow))
  "Face for the ┆ bar and the now-line dashes (kept at full size)."
  :group 'ps-schedule-view)

(defface ps/schedule-view-time
  '((t :inherit (org-scheduled-today ps/schedule-view-small)))
  "Face for the HH:MM-HH:MM range of a scheduled task (blue, slightly smaller)."
  :group 'ps-schedule-view)

(defface ps/schedule-view-tick
  '((t :inherit (ps/schedule-view-grid ps/schedule-view-small)))
  "Face for a grid-tick time label (grey, slightly smaller)."
  :group 'ps-schedule-view)

(defface ps/schedule-view-now
  '((t :inherit shadow :height 0.8))
  "Face for the `now · HH:MM' label on the current-time line.
Smaller than the time labels and not bold, so it stays unobtrusive."
  :group 'ps-schedule-view)

(defface ps/schedule-view-overlap
  '((t :inherit (org-warning org-modern-label) :weight semibold))
  "Face for the ⚠ overlap indicator on colliding tasks.
Inherits `org-modern-label' so it renders at the same scale as reldate badges."
  :group 'ps-schedule-view)

;;; Layout constants

(defconst ps/schedule-view--time-col-width 11
  "Width of the time field: \"HH:MM-HH:MM\" = 11 chars.")

(defconst ps/schedule-view--bar " ┆ "
  "Three-character separator between the time field and the task body.
A thin dotted vertical line (U+2506); the ┆ lands at column
\(ps/schedule-view--time-col-width + 1) = 12.")

(defconst ps/schedule-view--prefix-width
  (+ ps/schedule-view--time-col-width (string-width ps/schedule-view--bar))
  "Total columns consumed by \"HH:MM-HH:MM │ \" = 14.")

(defun ps/schedule-view--bar-col ()
  "Absolute column (char-width units) where the ┆ separator is drawn.
One column past the time field, matching the now-line's ┆ so every row's bar
lines up vertically regardless of the time label's font size."
  (+ ps/agenda-layout-left-margin-cols (1+ ps/schedule-view--time-col-width)))

;;; Pure time utilities (unit-tested in test-ps-schedule-view.el)

(defun ps/schedule-view--to-mins (hhmm)
  "Convert HHMM integer (e.g. 930) to minutes since midnight (570)."
  (+ (* (/ hhmm 100) 60) (% hhmm 100)))

(defun ps/schedule-view--end-mins (tod dur)
  "End time in minutes for start TOD (HHMM) and DUR duration in minutes.
Returns start minutes when DUR is nil or non-positive (zero-length event)."
  (+ (ps/schedule-view--to-mins tod)
     (if (and dur (numberp dur) (> dur 0)) (round dur) 0)))

(defun ps/schedule-view--fmt-tod (hhmm)
  "Format HHMM integer as zero-padded \"HH:MM\"."
  (format "%02d:%02d" (/ hhmm 100) (% hhmm 100)))

(defun ps/schedule-view--time-range-str (tod dur)
  "Time column string for a task at TOD with DUR minutes duration.
Returns \"HH:MM-HH:MM\" when DUR > 0, otherwise \"HH:MM\" padded to
`ps/schedule-view--time-col-width' with spaces."
  (let* ((start (ps/schedule-view--fmt-tod tod))
         (s (if (and dur (numberp dur) (> dur 0))
                (let* ((end-m (ps/schedule-view--end-mins tod dur))
                       (end   (format "%02d:%02d" (/ end-m 60) (% end-m 60))))
                  (concat start "-" end))
              start)))
    (truncate-string-to-width s ps/schedule-view--time-col-width nil ?\s)))

(defun ps/schedule-view--grid-str (hhmm)
  "Time column string for a grid tick (no task) at HHMM.
Returns \"HH:MM\" padded to `ps/schedule-view--time-col-width' with spaces."
  (truncate-string-to-width (ps/schedule-view--fmt-tod hhmm)
                            ps/schedule-view--time-col-width nil ?\s))

;;; Overlap detection (unit-tested)

(defun ps/schedule-view--find-overlaps (items)
  "Return markers of items that start during another earlier item's duration.
Only the later-starting task in an overlapping pair is flagged; the task
already running when the conflict begins does not get an indicator.
ITEMS is a list of (start-mins end-mins marker) triples."
  (let (result)
    (dolist (j items)
      (let ((s-j (nth 0 j)) (m-j (nth 2 j)))
        (when (cl-some (lambda (i)
                         (and (not (eq m-j (nth 2 i)))
                              (< (nth 0 i) s-j)    ; i starts strictly before j
                              (> (nth 1 i) s-j)))   ; i still running at j's start
                       items)
          (push m-j result))))
    result))

;;; Column geometry

(defun ps/schedule-view--cols ()
  "Column plist for schedule-section items, shifted right by the time prefix.
Each column position from `ps/agenda-layout--columns' is offset by
\\(prefix-width − left-margin), placing :cat at column
`ps/schedule-view--prefix-width'."
  (let* ((base  (ps/agenda-layout--columns))
         (delta ps/schedule-view--prefix-width)
         result)
    (cl-do ((l base (cddr l)))
        ((null l) (nreverse result))
      (push (car l) result)
      (push (+ (cadr l) delta) result))))

;;; Now-line detection and rendering

(defun ps/schedule-view--now-hhmm ()
  "Return the current time as an HHMM integer."
  (let ((d (decode-time (current-time))))
    (+ (* (nth 2 d) 100) (nth 1 d))))

(defun ps/schedule-view--is-past-midnight-p (now-hhmm)
  "Non-nil when NOW-HHMM falls in the `org-extend-today-until' window.
When active, today's agenda shows yesterday's date and the current time
is before the first grid entry, so the now-line belongs at the bottom."
  (and (boundp 'org-extend-today-until)
       (integerp org-extend-today-until)
       (> org-extend-today-until 0)
       (< now-hhmm (* org-extend-today-until 100))))

(defun ps/schedule-view--is-now-line-p (bol eol)
  "Non-nil when the agenda line [BOL, EOL) is org's current-time marker."
  ;; org sets `org-agenda-current-time' on the now-indicator line.
  ;; Fall back to text search so reformatted lines are also detected.
  (or (get-text-property bol 'org-agenda-current-time)
      (and (not (org-get-at-bol 'org-marker))
           (string-match-p "now" (buffer-substring-no-properties bol eol)))))

(defun ps/schedule-view--now-line-str (hhmm win-cols)
  "Full-width now-line string for HHMM with window width WIN-COLS.
Starts with `ps/agenda-layout-left-margin-cols' spaces so it aligns with
item and grid lines.  The `now · HH:MM' label is centred across the full
usable width.  ┆ is fixed at column (left-margin + time-col-width + 1)
within the string.  Dashes and bar in `ps/schedule-view-grid' face;
label in `ps/schedule-view-now' face."
  (let* ((margin      (make-string ps/agenda-layout-left-margin-cols ?\s))
         (usable      (- win-cols ps/agenda-layout-left-margin-cols))
         ;; ┆ lands at usable-col = time-col-width + 1 = 12
         (bar-col     (1+ ps/schedule-view--time-col-width))
         (label       (format " now · %s " (ps/schedule-view--fmt-tod hhmm)))
         (label-w     (string-width label))
         (total-fill  (max 0 (- usable label-w)))
         (left-fill   (/ total-fill 2))
         (right-fill  (- total-fill left-fill))
         ;; bar-col dashes left of ┆, then mid-fill dashes right of ┆ before label
         (left-dashes  (make-string bar-col ?┄))
         (mid-fill    (max 0 (- left-fill (1+ bar-col))))
         (mid-dashes  (make-string mid-fill ?┄))
         ;; One dash shorter on the right so the rule does not crowd the edge.
         (right-dashes (make-string (max 0 (1- right-fill)) ?┄)))
    (concat
     margin
     (propertize left-dashes  'face 'ps/schedule-view-grid)
     (propertize "┆"          'face 'ps/schedule-view-grid)
     (propertize mid-dashes   'face 'ps/schedule-view-grid)
     (propertize label        'face 'ps/schedule-view-now)
     (propertize right-dashes 'face 'ps/schedule-view-grid))))

;;; Item body rendering

(defun ps/schedule-view--render-item-body (cols overlap-p &optional title-face)
  "Propertized task-body string for the schedule item at point.
COLS is the shifted column plist.  Appends a right-aligned ⚠ overlap
indicator when OVERLAP-P is non-nil.  TITLE-FACE, when non-nil, is applied
to the title text (preserving the theme's scheduled-task colour)."
  (let* ((title      (ps/agenda-layout--title))
         (state      (org-get-at-bol 'todo-state))
         (tags       (org-get-at-bol 'tags))
         (pri        (ps/agenda-layout--priority-char))
         (emoji      (ps/agenda-layout--emoji title))
         (tag-str    (ps/agenda-layout--tags-string tags))
         ;; U+FE0E (text variation selector) keeps ⚠ a narrow monochrome glyph
         ;; instead of a wide color emoji, so its width matches the reldate
         ;; glyphs and the badge right edge lines up with the reldate badges.
         (overlap-str (and overlap-p
                           (propertize " ⚠︎ overlap"
                                       'face 'ps/schedule-view-overlap)))
         (right-cols (if overlap-str (string-width overlap-str) 0))
         (title-col  (plist-get cols :title))
         (avail (max 4 (floor (- (ps/agenda-layout--window-cols)
                                 title-col
                                 ps/agenda-layout-right-margin-cols
                                 (if overlap-str right-cols 0)
                                 (string-width tag-str)))))
         (title-text (if ps/agenda-layout-truncate
                         (ps/agenda-layout--truncate (or title "") avail)
                       (or title "")))
         (parts (list (ps/agenda-layout--render-category cols))))
    (push (ps/agenda-layout--space-to (plist-get cols :state)) parts)
    (when state (push (ps/agenda-layout--state-text state) parts))
    (push (ps/agenda-layout--space-to (plist-get cols :pri)) parts)
    (when pri (push (ps/agenda-layout--priority-text pri) parts))
    (push (ps/agenda-layout--space-to (plist-get cols :emoji)) parts)
    (when emoji
      (push (if ps/agenda-layout-emoji-face
                (propertize emoji 'face ps/agenda-layout-emoji-face)
              emoji)
            parts))
    (push (ps/agenda-layout--space-to title-col) parts)
    (push (propertize title-text
                      'face (or title-face 'default)
                      'help-echo (or title title-text)) parts)
    (when (> (length tag-str) 0) (push tag-str parts))
    (when overlap-str
      (push (ps/agenda-layout--space-before-right
             overlap-str ps/agenda-layout-right-margin-cols) parts)
      (push overlap-str parts))
    (apply #'concat (nreverse parts))))

(defun ps/schedule-view--render-item-line (cols tod dur overlap-p &optional title-face)
  "Display string for a schedule item at TOD (HHMM) with DUR minutes.
OVERLAP-P adds the ⚠ overlap indicator.  TITLE-FACE is applied to the title."
  (concat
   (make-string ps/agenda-layout-left-margin-cols ?\s)
   (propertize (ps/schedule-view--time-range-str tod dur)
               'face 'ps/schedule-view-time)
   ;; Place the ┆ at an absolute column so it stays aligned with the now-line and
   ;; the grid ticks even when the (smaller) time label renders narrower.
   (ps/agenda-layout--space-to (ps/schedule-view--bar-col))
   (propertize "┆" 'face 'ps/schedule-view-grid)
   (ps/schedule-view--render-item-body cols overlap-p title-face)))

(defun ps/schedule-view--render-grid-line (hhmm)
  "Display string for a grid-tick row (no task) at HHMM."
  (concat (propertize (concat (make-string ps/agenda-layout-left-margin-cols ?\s)
                              (ps/schedule-view--grid-str hhmm))
                      'face 'ps/schedule-view-tick)
          (ps/agenda-layout--space-to (ps/schedule-view--bar-col))
          (propertize "┆" 'face 'ps/schedule-view-grid)))

;;; Empty-section handling

(defun ps/schedule-view--schedule-region ()
  "Return (HEADER-BOL . END) spanning the Schedule section, or nil.
END is the beginning of the next section header (or `point-max').  Used to hide
the whole section on days whose only Schedule content is the empty time axis."
  (save-excursion
    (goto-char (point-min))
    (let ((header "") start)
      (catch 'done
        (while (not (eobp))
          (let ((bol (line-beginning-position)))
            (when (ps/agenda-layout--header-p)
              (setq header (string-trim (buffer-substring-no-properties
                                         bol (line-end-position))))
              (cond
               (start (throw 'done (cons start bol)))
               ((ps/agenda-layout--header-schedule-p header) (setq start bol)))))
          (forward-line 1))
        (and start (cons start (point-max)))))))

;;; Drawn dotted separator + row-height equalisation

(defvar ps/schedule-view--bar-image-cache nil
  "Cons (KEY . IMAGE) caching the dotted separator image.
KEY is (CHAR-WIDTH HEIGHT COLOR) so it rebuilds after a font/theme/height change.")

(defun ps/schedule-view--bar-image (height)
  "Return a dotted vertical-line image HEIGHT pixels tall, or nil.
Nil on non-graphical frames so the ┆ glyph is used as-is.  One character wide,
coloured with the `ps/schedule-view-grid' foreground.  Because the image is as
tall as the tallest Schedule row, placing it on every row both draws a
continuous dotted line and forces uniform row heights."
  (when (and (display-graphic-p) (> height 0))
    (let* ((w     (frame-char-width))
           (color (or (face-foreground 'ps/schedule-view-grid nil t) "gray"))
           (key   (list w height color ps/schedule-view-bar-ascent)))
      (unless (equal (car ps/schedule-view--bar-image-cache) key)
        (setq ps/schedule-view--bar-image-cache
              (cons key
                    (create-image
                     (format
                      (concat "<svg xmlns='http://www.w3.org/2000/svg' "
                              "width='%d' height='%d'>"
                              "<line x1='%s' y1='0' x2='%s' y2='%d' "
                              "stroke='%s' stroke-width='1' "
                              "stroke-dasharray='2,2'/></svg>")
                      w height (/ w 2.0) (/ w 2.0) height color)
                     'svg t :ascent ps/schedule-view-bar-ascent))))
      (cdr ps/schedule-view--bar-image-cache))))

(defun ps/schedule-view--draw-bars ()
  "Draw the dotted separator on every Schedule line and even their heights.
Measures the tallest Schedule row, builds the dotted-line image at that height,
and replaces each row's ┆ glyph `display' with it.  The equal-height image draws
a continuous dotted line and forces all rows (task, timeline, now) to one height.
No-op without a window (e.g. batch) — the plain ┆ glyph is kept."
  (let ((win (get-buffer-window (current-buffer))))
    (when win
      (condition-case nil
          (let ((header "") (h 0) lines)
            (save-excursion
              (goto-char (point-min))
              (while (not (eobp))
                (let ((bol (line-beginning-position))
                      (eol (line-end-position)))
                  (cond
                   ((ps/agenda-layout--header-p)
                    (setq header (string-trim
                                  (buffer-substring-no-properties bol eol))))
                   ((ps/agenda-layout--header-schedule-p header)
                    (push (cons bol eol) lines)
                    (setq h (max h (cdr (window-text-pixel-size win bol eol))))))
                  (forward-line 1))))
            (let ((img (ps/schedule-view--bar-image h)))
              (when img
                (let ((inhibit-read-only t))
                  (dolist (l lines)
                    (save-excursion
                      (goto-char (car l))
                      (when (search-forward "┆" (cdr l) t)
                        (put-text-property (1- (point)) (point) 'display img))))))))
        (error nil)))))

;;; Main apply pass

(defun ps/schedule-view--apply ()
  "Render the Schedule section with the timeline or events layout."
  (when (derived-mode-p 'org-agenda-mode)
    (let* ((inhibit-read-only t)
           (ps/agenda-layout--reserve-priority
            (ps/agenda-layout--scope-has-priority-p t))
           (cols     (ps/schedule-view--cols))
           (win-cols (ps/agenda-layout--window-cols))
           (style    ps/schedule-view-style)
           (header   "")
           items)
      ;; Pass 1: collect items for overlap detection.
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (when (ps/agenda-layout--header-p)
            (setq header (string-trim
                          (buffer-substring-no-properties
                           (line-beginning-position) (line-end-position)))))
          (when (and (ps/agenda-layout--header-schedule-p header)
                     (ps/agenda-layout--item-p))
            (let ((tod (org-get-at-bol 'time-of-day))
                  (dur (org-get-at-bol 'duration))
                  (m   (org-get-at-bol 'org-marker)))
              (when (and tod m)
                (push (list (ps/schedule-view--to-mins tod)
                            (ps/schedule-view--end-mins tod dur)
                            m)
                      items))))
          (forward-line 1)))
      ;; Clear hiding overlays left by a previous pass.
      (ps/agenda-layout--clear)
      ;; No timed task this day → hide the whole (empty) Schedule section instead
      ;; of showing a bare time axis; otherwise render it normally.
      (if (null items)
          (when-let ((region (ps/schedule-view--schedule-region)))
            (let ((ov (make-overlay (car region) (cdr region))))
              (overlay-put ov 'ps/agenda-layout t)
              (overlay-put ov 'evaporate t)
              (overlay-put ov 'invisible t)))
        (let ((overlapping (ps/schedule-view--find-overlaps items))
            last-vis-eol
            now-bottom-str)
        ;; Pass 2: render lines.
        (setq header "")
        (save-excursion
          (goto-char (point-min))
          (while (not (eobp))
            (let ((bol (line-beginning-position))
                  (eol (line-end-position)))
              (cond
               ;; Track the current section header.
               ((ps/agenda-layout--header-p)
                (setq header (string-trim
                              (buffer-substring-no-properties bol eol))))
               ;; Lines inside the Schedule section.
               ((ps/agenda-layout--header-schedule-p header)
                (cond
                 ;; Task/event item.
                 ((ps/agenda-layout--item-p)
                  (let* ((tod        (org-get-at-bol 'time-of-day))
                         (dur        (org-get-at-bol 'duration))
                         (m          (org-get-at-bol 'org-marker))
                         (overlap-p  (and m (memq m overlapping)))
                         ;; Use the standard org scheduled-task face.  All
                         ;; time-grid items in the Schedule section are today's
                         ;; events, so org-scheduled-today is always correct.
                         ;; Past-scheduled items get org-warning (red/orange),
                         ;; matching how the layout module treats overdue items.
                         (title-face
                          (if (string= (org-get-at-bol 'type) "past-scheduled")
                              'org-warning
                            'org-scheduled-today)))
                    (ps/agenda-layout--replace-line
                     bol eol
                     (ps/schedule-view--render-item-line
                      cols tod dur overlap-p title-face))
                    (setq last-vis-eol (point))))
                 ;; Grid filler or now-line (no org-marker, has time-of-day).
                 ((org-get-at-bol 'time-of-day)
                  (let ((hhmm (org-get-at-bol 'time-of-day)))
                    (cond
                     ;; Current-time indicator: always shown.
                     ;; Use (ps/schedule-view--now-hhmm) for the label so
                     ;; it shows the real current time, not the stale grid-tick
                     ;; value stored in the `time-of-day' text property.
                     ((ps/schedule-view--is-now-line-p bol eol)
                      (let ((now-hhmm (ps/schedule-view--now-hhmm)))
                        (if (ps/schedule-view--is-past-midnight-p now-hhmm)
                            ;; Past midnight (org-extend-today-until active):
                            ;; hide in-place, then render at bottom after loop.
                            (progn
                              (ps/agenda-layout--hide-line bol eol)
                              (setq now-bottom-str
                                    (ps/schedule-view--now-line-str now-hhmm win-cols)))
                          (progn
                            (ps/agenda-layout--replace-line
                             bol eol
                             (ps/schedule-view--now-line-str now-hhmm win-cols))
                            (setq last-vis-eol (point))))))
                     ;; Grid tick: shown in timeline mode, hidden in events mode.
                     ((eq style 'timeline)
                      (ps/agenda-layout--replace-line
                       bol eol
                       (ps/schedule-view--render-grid-line hhmm))
                      (setq last-vis-eol (point)))
                     (t
                      (ps/agenda-layout--hide-line bol eol))))))))
            (forward-line 1))))
        ;; When past midnight the original now-line was hidden at the top of the
        ;; grid; re-attach it as a normal line right after the last visible
        ;; schedule line via an `after-string' overlay (stays inside the section,
        ;; renders with its own grid/now faces).  Tagged so --clear removes it.
        (when (and now-bottom-str last-vis-eol)
          (let ((ov (make-overlay last-vis-eol last-vis-eol)))
            (overlay-put ov 'ps/agenda-layout t)
            (overlay-put ov 'after-string (concat "\n" now-bottom-str)))))
        ;; Draw the dotted separator and even out the row heights.
        (ps/schedule-view--draw-bars)))))

;;; Auto-refresh timer

(defvar ps/schedule-view--timer nil
  "Timer that fires at the start of the next calendar minute.")

(defvar ps/schedule-view--resize-timer nil
  "Idle timer debouncing schedule-view layout refreshes on window resize.")

(defun ps/schedule-view--seconds-to-next-minute ()
  "Seconds until the start of the next calendar minute."
  (let ((s (nth 0 (decode-time (current-time)))))
    (if (= s 0) 60 (- 60 s))))

(defun ps/schedule-view--refresh-agenda ()
  "Refresh *Org Agenda*, preserving cursor and scroll position."
  (let* ((buf (get-buffer "*Org Agenda*")))
    (when (buffer-live-p buf)
      (let* ((win (get-buffer-window buf t))
             (pt  (with-current-buffer buf (point)))
             (ws  (and win (window-start win))))
        (condition-case err
            ;; org-agenda-redo calls recenter internally, which requires
            ;; the selected window to display the current buffer.  When the
            ;; timer fires while focus is elsewhere, with-current-buffer
            ;; alone is not enough — we must also select the agenda window.
            ;; Suppress the "Rebuilding agenda buffer…done" echo-area/Messages
            ;; noise that fires on every auto-refresh.
            (let ((inhibit-message t)
                  (message-log-max nil))
              (if win
                  (with-selected-window win (org-agenda-redo))
                (with-current-buffer buf  (org-agenda-redo))))
          (error
           (message "ps/schedule-view: redo error: %s" (error-message-string err))
           (ignore-errors
             (with-current-buffer buf
               (let ((inhibit-read-only t))
                 (run-hooks 'org-agenda-finalize-hook))))))
        (ignore-errors
          (with-current-buffer buf (goto-char (min pt (point-max)))))
        (when (and win (window-live-p win))
          (ignore-errors
            (with-selected-window win
              (set-window-start win (min ws (point-max)) t))))))))

(defun ps/schedule-view--do-refresh ()
  "Timer callback: refresh agenda and schedule the next fire."
  ;; Wrap the whole refresh so any unexpected error can't stop the timer.
  (condition-case err
      (ps/schedule-view--refresh-agenda)
    (error (message "ps/schedule-view: refresh error: %s"
                    (error-message-string err))))
  ;; Always reschedule — even after errors — so the timer never stops silently.
  (ps/schedule-view--schedule-next-refresh))

(defun ps/schedule-view--schedule-next-refresh ()
  "Schedule `ps/schedule-view--do-refresh' at the next minute boundary."
  (when ps/schedule-view-auto-refresh
    (when (timerp ps/schedule-view--timer)
      (cancel-timer ps/schedule-view--timer))
    (setq ps/schedule-view--timer
          (run-with-timer (ps/schedule-view--seconds-to-next-minute)
                          nil
                          #'ps/schedule-view--do-refresh))))

;;; Resize hook — mirrors ps-agenda-layout's debounce approach

(defun ps/schedule-view--refresh-layout ()
  "Re-run the schedule-view layout pass in *Org Agenda*, if open."
  (let ((buf (get-buffer "*Org Agenda*")))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (ps/schedule-view--apply)))))

(defun ps/schedule-view--after-layout-refresh (&rest _)
  "Hook run after `ps/agenda-layout-refresh' to update the schedule section."
  (let ((buf (get-buffer "*Org Agenda*")))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (ps/schedule-view--apply)))))

;;; Public API

(defun ps/schedule-view-set-style (style)
  "Set the schedule view STYLE and refresh the agenda."
  (interactive (list (intern (completing-read "Schedule style: " '("timeline" "events") nil t))))
  (setq ps/schedule-view-style style)
  (ps/schedule-view--refresh-agenda))

(defun ps/schedule-view-toggle-auto-refresh ()
  "Toggle `ps/schedule-view-auto-refresh' and start/cancel the timer."
  (interactive)
  (setq ps/schedule-view-auto-refresh (not ps/schedule-view-auto-refresh))
  (if ps/schedule-view-auto-refresh
      (ps/schedule-view--schedule-next-refresh)
    (when (timerp ps/schedule-view--timer)
      (cancel-timer ps/schedule-view--timer)
      (setq ps/schedule-view--timer nil))))

(defun ps/schedule-view-setup ()
  "Enable the schedule-view layout.
- Signals `ps-agenda-layout' to skip the Schedule section.
- Hooks into `org-agenda-finalize-hook' (appended, runs after layout).
- Advises `ps/agenda-layout-refresh' so resizes also update the schedule.
- Starts the auto-refresh timer when `ps/schedule-view-auto-refresh' is set."
  (setq ps/schedule-view-override t)
  (add-hook 'org-agenda-finalize-hook #'ps/schedule-view--apply t)
  (advice-add 'ps/agenda-layout-refresh :after
              #'ps/schedule-view--after-layout-refresh
              '((name . ps/schedule-view)))
  (ps/schedule-view--schedule-next-refresh))

(provide 'ps-schedule-view)
;;; ps-schedule-view.el ends here
