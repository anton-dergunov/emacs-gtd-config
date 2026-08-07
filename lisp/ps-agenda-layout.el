;;; ps-agenda-layout.el --- Aligned, badge-based layout for the org agenda -*- lexical-binding: t; -*-

;;; Commentary:
;; Re-lays each org-agenda task line into aligned columns:
;;
;;   [category icon] [STATE badge] [PRIORITY badge] [emoji]  Title… [tags]   [sched badge]
;;
;; The work happens on `org-agenda-finalize-hook'.  For every item line we read
;; org-agenda's own per-line text properties (`org-marker', `todo-state',
;; `org-category', `tags', `time-of-day', `type', …) plus the source heading via
;; its marker, then replace the line with a display string built from propertized
;; text (face-based badges using `org-modern' faces) and `(space :align-to COL)'
;; separators.  Category icons remain SVG images.  Because the column positions
;; are fixed (in character columns) and `:align-to' ignores the pixel width of
;; preceding images, task titles line up across every section.  The buffer text
;; and its line-start markers are never touched, so navigation (RET/TAB/bulk)
;; keeps working.
;;
;; The Schedule (time-grid) block is special and switchable via
;; `ps/agenda-layout-schedule-style':
;;   `grid'    — leave Org's native time ruler (familiar); only the list
;;               sections are reformatted.
;;   `compact' — hide the empty grid filler rows and lay timed events out with
;;               the same columns as the other sections (so titles align
;;               everywhere), with the time range as the right-hand badge.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'ps-emphasis)

(defvar ps/schedule-view-override nil
  "When non-nil, `ps-schedule-view' is handling the Schedule section.
`ps/agenda-layout--apply' will skip all lines inside the Schedule block
so the two modules don't reformat the same lines.")

(defvar ps/agenda-layout-view-kind nil
  "Which agenda view built the current buffer: `agenda', `calendar', `situation',
or nil.  Let-bound by the agenda custom commands (see
`org-agenda-custom-commands' in config.org, and `ps/situations--custom-commands'
for the generated situation ones).  The Calendar view (`calendar') draws
span-switch controls in its date header and surfaces times for timed items in
multi-day spans; a Situation view (`situation') draws an undated plate naming
the query; the Agenda view (`agenda') does neither.
`ps/agenda-layout--apply' copies this into the buffer-local
`ps/agenda-layout--view-kind' so it survives a resize re-layout.")

(defvar-local ps/agenda-layout--view-kind nil
  "Buffer-local copy of `ps/agenda-layout-view-kind' for the *Org Agenda* buffer.")

(defun ps/agenda-layout--calendarp ()
  "Non-nil when the current agenda buffer is a Calendar view."
  (eq ps/agenda-layout--view-kind 'calendar))

(defun ps/agenda-layout--situationp ()
  "Non-nil when the current agenda buffer is a Situation view."
  (eq ps/agenda-layout--view-kind 'situation))

;; org / org-agenda are loaded by the time the finalize hook runs.
(declare-function org-get-at-bol "org" (property))
(declare-function org-with-point-at "org-macs" (pom &rest body))
(declare-function org-get-heading "org" (&optional no-tags no-todo no-priority no-comment))
(declare-function org-heading-components "org" ())
(declare-function org-get-deadline-time "org" (pom &optional inherit))
(declare-function org-get-scheduled-time "org" (pom &optional inherit))
(declare-function ps/material-icons-image "ps-material-icons" (name &optional ascent height))
(declare-function ps/material-icons-available-p "ps-material-icons" ())
(declare-function ps/agenda-emoji-lookup "ps-agenda-emoji" (title))
(declare-function ps/situations-plate-label "ps-situations" (&optional key))
(declare-function ps/situations-plate-icon "ps-situations" (&optional key))
(declare-function ps/situations-switch "ps-situations" (&optional event))
(declare-function org-agenda-earlier "org-agenda" (arg))
(declare-function org-agenda-later "org-agenda" (arg))
(declare-function org-agenda-redo "org-agenda" (&optional all))
(declare-function org-agenda-check-type "org-agenda" (error &rest types))
(declare-function org-agenda-day-view "org-agenda" (&optional day-of-month))
(declare-function org-agenda-week-view "org-agenda" (&optional iso-week))
(declare-function org-agenda-month-view "org-agenda" (&optional month))
(declare-function org-agenda-year-view "org-agenda" (&optional year))
(declare-function org-agenda-goto-date "org-agenda" (date))
(declare-function org-agenda-find-same-or-today-or-agenda "org-agenda" (&optional cnt))
(declare-function org-agenda-compute-starting-span "org-agenda" (sd span &optional n))
(declare-function org-time-from-absolute "org" (d))
(declare-function org-time-string-to-time "org" (s))
(declare-function org-read-date "org" (&optional with-time to-time from-string prompt default-time default-input inactive))
(declare-function org-days-to-iso-week "org" (days))
(declare-function org-today "org" ())
(declare-function calendar-gregorian-from-absolute "calendar" (date))
(declare-function calendar-last-day-of-month "calendar" (month year))
(declare-function calendar-leap-year-p "calendar" (year))
(defvar org-agenda-finalize-hook)
(defvar org-agenda-overriding-arguments)
(defvar org-agenda-overriding-cmd)
(defvar org-agenda-current-span)
(defvar org-agenda-start-on-weekday)
(defvar org-starting-day)
(defvar calendar-week-start-day)
(defvar org-done-keywords)
(defvar org-todo-all-keywords)
(defvar ps/material-icons-category-map)

(defvar-local ps/agenda-layout--cal-start nil
  "Cached absolute start day of the Calendar's current span (resize-safe).")
(defvar-local ps/agenda-layout--cal-span nil
  "Cached span of the Calendar's current view (resize-safe).")

;;; Customization

(defgroup ps-agenda-layout nil
  "Aligned, badge-based layout for the org agenda."
  :group 'ps)

(defcustom ps/agenda-layout-enabled t
  "When non-nil, reformat agenda task lines into aligned columns."
  :type 'boolean
  :group 'ps-agenda-layout)

(defcustom ps/agenda-layout-category-display 'icon
  "What to show in the leftmost (category) column.
`icon' shows only the category icon, `name' only the category name (its TITLE),
`both' shows both, and `none' omits the column entirely."
  :type '(choice (const :tag "Icon only" icon)
                 (const :tag "Name only" name)
                 (const :tag "Icon and name" both)
                 (const :tag "Nothing" none))
  :group 'ps-agenda-layout)

(defcustom ps/agenda-layout-category-fallback-icon "description"
  "Material Symbols icon name used when a category has no mapping, or nil."
  :type '(choice (const :tag "None" nil) string)
  :group 'ps-agenda-layout)

(defcustom ps/agenda-layout-schedule-style 'grid
  "How the Schedule (time-grid) block is rendered.
`grid' keeps Org's native time ruler and reformats only the list sections;
`compact' hides the empty grid rows and lays timed events out like the list
sections, with the time range as the right-hand badge."
  :type '(choice (const :tag "Keep native time grid" grid)
                 (const :tag "Compact event list" compact))
  :group 'ps-agenda-layout)

(defcustom ps/agenda-layout-schedule-group "Schedule"
  "Substring identifying the time-grid group's header line.
Matched against each super-agenda/section header to decide which items belong to
the Schedule block."
  :type 'string
  :group 'ps-agenda-layout)

(defcustom ps/agenda-layout-state-labels nil
  "Alist remapping a TODO keyword to a shorter badge label.
Each entry is (KEYWORD . LABEL), e.g. (\"WAIT\" . \"W\").  Empty means
the keyword is shown verbatim."
  :type '(alist :key-type string :value-type string)
  :group 'ps-agenda-layout)

(defcustom ps/agenda-layout-truncate t
  "When non-nil, truncate titles that would overflow the line."
  :type 'boolean
  :group 'ps-agenda-layout)

(defcustom ps/agenda-layout-ellipsis "…"
  "String marking a truncated title."
  :type 'string
  :group 'ps-agenda-layout)

;; Column widths, in character columns.
(defcustom ps/agenda-layout-left-margin-cols 1
  "Left margin before the first column, in character columns."
  :type 'integer :group 'ps-agenda-layout)

(defcustom ps/agenda-layout-gap-cols 1
  "Gap between adjacent columns, in character columns."
  :type 'integer :group 'ps-agenda-layout)

(defcustom ps/agenda-layout-category-cols 3
  "Width reserved for the category icon, in character columns."
  :type 'integer :group 'ps-agenda-layout)

(defcustom ps/agenda-layout-category-name-cols 12
  "Width reserved for the category name, in character columns."
  :type 'integer :group 'ps-agenda-layout)

(defcustom ps/agenda-layout-state-cols nil
  "Width in columns reserved for the TODO-state badge, or nil to auto-compute.
When nil, the width is derived at render time from the longest keyword in
`org-todo-all-keywords' plus 2 padding spaces."
  :type '(choice (const :tag "Auto" nil) integer)
  :group 'ps-agenda-layout)

(defcustom ps/agenda-layout-priority-cols 4
  "Width reserved for the priority badge, in character columns."
  :type 'integer :group 'ps-agenda-layout)

(defcustom ps/agenda-layout-emoji-cols 2
  "Width reserved for the semantic emoji, in character columns."
  :type 'integer :group 'ps-agenda-layout)

(defcustom ps/agenda-layout-right-margin-cols 2
  "Empty margin kept to the right of the scheduling badge, in character columns."
  :type 'integer :group 'ps-agenda-layout)

(defcustom ps/agenda-layout-reldate-glyphs
  '(("deadline" . "⚑") ("scheduled" . "⏱"))
  "Alist mapping a scheduling TYPE substring to a leading glyph.
Each entry is (TYPE-SUBSTRING . GLYPH); the glyph is prepended to the
relative-date badge text for items whose `type' text property contains
TYPE-SUBSTRING.  Set to nil to disable leading glyphs entirely."
  :type '(alist :key-type string :value-type string)
  :group 'ps-agenda-layout)

(defcustom ps/agenda-layout-emoji-face nil
  "Face spec applied to the in-column semantic emoji, or nil for none."
  :type '(choice (const :tag "None" nil) sexp)
  :group 'ps-agenda-layout)

;;; Faces

(defface ps/agenda-layout-reldate-overdue
  '((t :inherit (org-warning org-modern-label) :weight semibold))
  "Face for overdue relative-date text.  Colored text at label scale; no fill."
  :group 'ps-agenda-layout)

(defface ps/agenda-layout-reldate-today
  '((t :inherit (org-scheduled-today org-modern-label)))
  "Face for today's relative-date text.  Colored text at label scale; no fill."
  :group 'ps-agenda-layout)

(defface ps/agenda-layout-reldate-future
  '((t :inherit (shadow org-modern-label)))
  "Face for future relative-date text."
  :group 'ps-agenda-layout)

(defface ps/agenda-layout-reldate-time
  '((t :inherit (shadow org-modern-label)))
  "Face for time-range text (compact Schedule events)."
  :group 'ps-agenda-layout)

(defface ps/agenda-layout-day-section
  '((t :inherit org-super-agenda-header :weight bold :extend t))
  "Face for a Calendar per-day section header (week/month/year views).
Inherits the Agenda sections' theme-aware grey background, but is its own
tunable face: left-aligned, bold, normal size."
  :group 'ps-agenda-layout)

(defface ps/agenda-layout-control-label
  '((t :inherit org-agenda-date))
  "Face for the span label in the Calendar control row (e.g. \"18–24 May 2026\")."
  :group 'ps-agenda-layout)

(defface ps/agenda-layout-situation-label
  '((t :inherit ps/agenda-layout-control-label :weight bold))
  "Face for the situation name on a Situation view's plate.
Bold, unlike the Calendar's date label: it is the only thing naming the view,
and the list beneath it carries no section headers to anchor the eye."
  :group 'ps-agenda-layout)

(defface ps/agenda-layout-span-button
  '((t :inherit default :box (:line-width (1 . -1))))
  "Face for the Calendar span-switch buttons (D W M Y / Range…).
The thin box makes them read as clickable."
  :group 'ps-agenda-layout)

(defface ps/agenda-layout-span-button-active
  '((t :inherit ps/agenda-layout-span-button :weight bold :inverse-video t))
  "Face for the Calendar span button matching the current span (filled)."
  :group 'ps-agenda-layout)

;;; Line classification and source reads

(defun ps/agenda-layout--item-p ()
  "Non-nil when the agenda line at point is a real task/event item."
  (let ((m (org-get-at-bol 'org-marker)))
    (and m (markerp m) (marker-buffer m))))

(defun ps/agenda-layout--header-p ()
  "Non-nil when the agenda line at point is a section/group header."
  (and (not (org-get-at-bol 'org-agenda-date-header))
       (or (org-get-at-bol 'org-super-agenda-header)
           (org-get-at-bol 'org-agenda-structural-header))))

(defun ps/agenda-layout--title ()
  "Return the clean heading title for the item line at point, or nil."
  (let ((m (org-get-at-bol 'org-hd-marker)))
    (when (and m (markerp m) (marker-buffer m))
      (let ((title (org-with-point-at m (org-get-heading t t t t))))
        (and (stringp title) (> (length (string-trim title)) 0)
             (string-trim (substring-no-properties title)))))))

(defun ps/agenda-layout--priority-char ()
  "Return the priority character (e.g. ?A) for the item line at point, or nil."
  (let ((m (org-get-at-bol 'org-hd-marker)))
    (when (and m (markerp m) (marker-buffer m))
      (org-with-point-at m (nth 3 (org-heading-components))))))

(defun ps/agenda-layout--emoji (title)
  "Return the semantic emoji for TITLE, or nil."
  (when (and title (fboundp 'ps/agenda-emoji-lookup))
    (ps/agenda-emoji-lookup title)))

(defun ps/agenda-layout--header-schedule-p (header)
  "Non-nil when HEADER names the Schedule (time-grid) group itself.
Matches the group's header line exactly (\"Schedule:\" or HEADER equal to
`ps/agenda-layout-schedule-group'), so other groups whose names merely contain
the same substring (e.g. \"Scheduled earlier:\") are not misclassified."
  (and (stringp header) (stringp ps/agenda-layout-schedule-group)
       (not (string-empty-p ps/agenda-layout-schedule-group))
       (let ((trimmed (string-trim header))
             (target (string-trim ps/agenda-layout-schedule-group)))
         (or (string= trimmed target)
             (string= trimmed (concat target ":"))))))

;;; Pure formatting helpers (unit-tested)

(defun ps/agenda-layout--state-label (state)
  "Return the badge label for TODO keyword STATE."
  (or (cdr (assoc state ps/agenda-layout-state-labels)) state))

(defun ps/agenda-layout--reldate-string (days)
  "Return a compact relative-date label for a signed DAYS offset from today."
  (cond
   ((= days 0) "today")
   ((> days 0) (if (>= days 14) (format "in %dw" (round days 7)) (format "in %dd" days)))
   (t (let ((a (- days)))
        (if (>= a 14) (format "%dw ago" (round a 7)) (format "%dd ago" a))))))

(defun ps/agenda-layout--reldate-glyph (type)
  "Return the leading glyph for scheduling TYPE, or nil.
TYPE is the agenda item's `type' text property (e.g. \"deadline\" or
\"scheduled\"); the glyph is looked up in `ps/agenda-layout-reldate-glyphs' by
matching the alist key as a substring of TYPE."
  (and (stringp type)
       (cdr (cl-find-if (lambda (entry) (string-match-p (car entry) type))
                         ps/agenda-layout-reldate-glyphs))))

(defun ps/agenda-layout--reldate-tint (days)
  "Return the scheduling tint symbol for a signed DAYS offset."
  (cond ((< days 0) 'overdue) ((= days 0) 'today) (t 'future)))

(defun ps/agenda-layout--fmt-tod (hhmm &optional pad)
  "Format integer HHMM (e.g. 930) as \"9:30\", or \"09:30\" when PAD is non-nil."
  (format (if pad "%02d:%02d" "%d:%02d") (/ hhmm 100) (% hhmm 100)))

(defun ps/agenda-layout--time-range (tod dur &optional pad)
  "Format a time range from TOD (HHMM integer) and DUR (minutes), or just TOD.
With PAD non-nil the hours are zero-padded (\"08:00–08:30\")."
  (if (and dur (numberp dur) (> dur 0))
      (let* ((mins (+ (* (/ tod 100) 60) (% tod 100) (round dur)))
             (end (+ (* (/ mins 60) 100) (% mins 60))))
        (format "%s–%s" (ps/agenda-layout--fmt-tod tod pad)
                (ps/agenda-layout--fmt-tod end pad)))
    (ps/agenda-layout--fmt-tod tod pad)))

(defun ps/agenda-layout--truncate (s maxcols)
  "Truncate S to MAXCOLS display columns, appending the ellipsis if shortened."
  (if (<= (string-width s) maxcols)
      s
    (truncate-string-to-width s (max 1 maxcols) nil nil ps/agenda-layout-ellipsis)))

;;; Column geometry

(defun ps/agenda-layout--effective-state-cols ()
  "Return the effective state-column width in character-width units.
On graphical frames this is the *measured* rendered width of the widest TODO
badge (the badges render condensed via `org-modern-label', so their pixel width
is narrower than their character count); the result may be fractional so the
residual gap to the next field is exactly one space.  When `state-cols' is set
it is honoured verbatim; on non-graphical frames the character-count estimate
\(longest keyword + 2 padding) is used so batch tests stay deterministic."
  (cond
   (ps/agenda-layout-state-cols ps/agenda-layout-state-cols)
   ((display-graphic-p)
    (let ((kws (if (boundp 'org-todo-all-keywords) org-todo-all-keywords '("TODO"))))
      (/ (apply #'max 1 (mapcar (lambda (kw)
                                  (string-pixel-width
                                   (ps/agenda-layout--state-text kw)))
                                kws))
         (float (frame-char-width)))))
   (t (+ 2 (if (boundp 'org-todo-all-keywords)
               (apply #'max 0 (mapcar #'string-width org-todo-all-keywords))
             4)))))

(defvar ps/agenda-layout--reserve-priority t
  "When nil, the priority column is collapsed to zero width.
Let-bound per render scope (Schedule vs. the rest) so that a scope with no
prioritised tasks leaves only the two surrounding gaps instead of an empty pill
slot.  See `ps/agenda-layout--scope-has-priority-p'.")

(defun ps/agenda-layout--effective-priority-cols ()
  "Return the priority-column width in character-width units.
Zero when `ps/agenda-layout--reserve-priority' is nil (no prioritised task in
scope).  Otherwise, graphical frames use the measured width of the rendered
\" A \" pill (condensed, ≈2 cols); other frames fall back to
`ps/agenda-layout-priority-cols'."
  (cond
   ((not ps/agenda-layout--reserve-priority) 0)
   ((display-graphic-p)
    (/ (string-pixel-width (ps/agenda-layout--priority-text ?A))
       (float (frame-char-width))))
   (t ps/agenda-layout-priority-cols)))

(defun ps/agenda-layout--scope-has-priority-p (schedule-scope)
  "Non-nil if any agenda item in scope carries an explicit priority cookie.
SCHEDULE-SCOPE non-nil restricts the scan to the Schedule section; nil restricts
it to every other section.  Short-circuits on the first match."
  (save-excursion
    (goto-char (point-min))
    (let ((header "") found)
      (while (and (not found) (not (eobp)))
        (cond
         ((ps/agenda-layout--header-p)
          (setq header (string-trim (buffer-substring-no-properties
                                     (line-beginning-position)
                                     (line-end-position)))))
         ((and (ps/agenda-layout--item-p)
               (eq (and (ps/agenda-layout--header-schedule-p header) t)
                   (and schedule-scope t))
               (ps/agenda-layout--priority-char))
          (setq found t)))
        (forward-line 1))
      found)))

(defun ps/agenda-layout--icon-cols ()
  "Return the category-icon width in character-width units.
Graphical frames derive it from the icon's natural pixel size (the Material
SVG's 24×20 aspect at the configured pixel height); other frames fall back to
`ps/agenda-layout-category-cols'."
  (if (and (display-graphic-p) (fboundp 'ps/material-icons--pixel-height))
      (/ (* (ps/material-icons--pixel-height) (/ 24.0 20.0))
         (float (frame-char-width)))
    ps/agenda-layout-category-cols))

(defun ps/agenda-layout--effective-category-cols ()
  "Return the category-column width in character-width units."
  (pcase ps/agenda-layout-category-display
    ('none 0)
    ('name ps/agenda-layout-category-name-cols)
    ('both (+ (ps/agenda-layout--icon-cols) 1 ps/agenda-layout-category-name-cols))
    (_     (ps/agenda-layout--icon-cols))))

(defun ps/agenda-layout--columns ()
  "Return a plist of column start positions (in character-width units).
Widths are the fields' measured rendered sizes on graphical frames, so every
inter-field gap is exactly `ps/agenda-layout-gap-cols' (one space) while columns
still line up across rows.  Positions may be fractional."
  (let* ((left ps/agenda-layout-left-margin-cols)
         (gap ps/agenda-layout-gap-cols)
         (cat-cols (ps/agenda-layout--effective-category-cols))
         (state (+ left cat-cols (if (> cat-cols 0) gap 0)))
         (pri (+ state (ps/agenda-layout--effective-state-cols) gap))
         (emoji (+ pri (ps/agenda-layout--effective-priority-cols) gap))
         (title (+ emoji ps/agenda-layout-emoji-cols gap)))
    (list :cat left :state state :pri pri :emoji emoji :title title)))

(defun ps/agenda-layout--window-cols ()
  "Return the agenda window's text width in columns (fallback 80)."
  (let ((win (get-buffer-window (current-buffer))))
    (if win (window-text-width win) 80)))

;;; Display-string building blocks

(defun ps/agenda-layout--space-to (col)
  "A blank that stretches to absolute column COL."
  (propertize " " 'display `(space :align-to ,col)))

(defun ps/agenda-layout--space-to-right (cols)
  "A blank that stretches to COLS columns from the right window edge."
  (propertize " " 'display `(space :align-to (- right ,cols))))

(defun ps/agenda-layout--space-before-right (badge cols)
  "Spacer placing BADGE so its right edge sits COLS columns from the right edge.
Uses BADGE's actual pixel width (`string-pixel-width'), so condensed/scaled
`org-modern-label' faces still align exactly with full-scale text."
  (propertize " " 'display
              `(space :align-to (- right ,cols (,(string-pixel-width badge))))))

(defun ps/agenda-layout--image-cell (image)
  "A single character displaying IMAGE."
  (propertize " " 'display image))

(defun ps/agenda-layout--category-image (category)
  "Return a Material Symbols image for CATEGORY, or nil."
  (when (and (fboundp 'ps/material-icons-image)
             (fboundp 'ps/material-icons-available-p)
             (ps/material-icons-available-p))
    (let ((name (or (and (boundp 'ps/material-icons-category-map)
                         (cdr (assoc category ps/material-icons-category-map)))
                    ps/agenda-layout-category-fallback-icon)))
      (and name (ps/material-icons-image name)))))

(defun ps/agenda-layout--state-text (state)
  "Return propertized badge text for TODO keyword STATE.
Uses `org-modern-done' for done keywords and `org-modern-todo' otherwise.
Padding is applied via display properties on the first/last chars of the
label (matching exactly what `org-modern--todo' does), so the badge renders
identically to org-modern's own keyword badges."
  (let* ((label (ps/agenda-layout--state-label state))
         (done-p (and (boundp 'org-done-keywords)
                      (member state org-done-keywords)))
         (face (if done-p 'org-modern-done 'org-modern-todo))
         (len (length label)))
    (if (= len 0)
        ""
      (let ((s (copy-sequence label)))
        (if (= len 1)
            (put-text-property 0 1 'display (format " %c " (aref s 0)) s)
          (put-text-property 0 1 'display (format " %c" (aref s 0)) s)
          (put-text-property (1- len) len 'display (string (aref s (1- len)) ?\s) s))
        (add-face-text-property 0 len face nil s)
        s))))

(defun ps/agenda-layout--priority-text (char)
  "Return propertized badge text for priority CHAR (e.g. ?A), or nil.
The badge's real text is just the letter; the surrounding pill padding is
applied via a `display' property on that one char (mirroring
`ps/agenda-layout--state-text'), so it renders as \" A \" with the green
`org-modern-priority' fill yet navigates as a single character — matching how
org-modern prettifies priorities in org files."
  (when char
    (let ((s (string char)))
      (put-text-property 0 1 'display (format " %c " char) s)
      (put-text-property 0 1 'face 'org-modern-priority s)
      s)))

(defun ps/agenda-layout--reldate-text (text tint)
  "Return propertized badge text for reldate TEXT with color TINT symbol.
TINT is one of `overdue', `today', `future', or `time' (timed events)."
  (propertize (concat " " text)
              'face (pcase tint
                      ('overdue 'ps/agenda-layout-reldate-overdue)
                      ('today   'ps/agenda-layout-reldate-today)
                      ('future  'ps/agenda-layout-reldate-future)
                      (_        'ps/agenda-layout-reldate-time))))

(defun ps/agenda-layout--tags-string (tags)
  "Return a propertized inline tag string for TAGS, or empty string."
  (if (and tags (consp tags))
      (concat " " (propertize (concat ":" (mapconcat #'identity tags ":") ":")
                              'face 'org-modern-tag))
    ""))

(defun ps/agenda-layout--reldate-here ()
  "Return (TEXT . TINT) describing the deadline/scheduled date of the item, or nil."
  (let* ((type (org-get-at-bol 'type))
         (m (org-get-at-bol 'org-hd-marker)))
    (when (and type m (markerp m) (marker-buffer m))
      (let* ((deadline-p (string-match-p "deadline" type))
             (time (org-with-point-at m
                     (if deadline-p (org-get-deadline-time (point))
                       (org-get-scheduled-time (point))))))
        (when time
          (let* ((days (- (time-to-days time) (time-to-days (current-time))))
                 (glyph (ps/agenda-layout--reldate-glyph type))
                 (text (ps/agenda-layout--reldate-string days)))
            (cons (if glyph (concat glyph " " text) text)
                  (ps/agenda-layout--reldate-tint days))))))))

(defun ps/agenda-layout--right-element (schedule-compact tod dur)
  "Return (COLS . STRING) for the right-hand badge, or (0 . nil) when absent.
When SCHEDULE-COMPACT and TOD is set, the badge is the time range; otherwise it
is the relative deadline/scheduled date."
  (cond
   ((and schedule-compact tod)
    (let* ((txt (ps/agenda-layout--time-range tod dur))
           (badge (ps/agenda-layout--reldate-text txt 'time)))
      (cons (string-width badge) badge)))
   (t
    (let ((rd (ps/agenda-layout--reldate-here)))
      (if rd
          (let ((badge (ps/agenda-layout--reldate-text (car rd) (cdr rd))))
            (cons (string-width badge) badge))
        (cons 0 nil))))))

(defun ps/agenda-layout--render-category (cols)
  "Return the category column string using the COLS plist."
  (let ((start (plist-get cols :cat))
        (category (org-get-at-bol 'org-category)))
    (pcase ps/agenda-layout-category-display
      ('none "")
      ('name (concat (ps/agenda-layout--space-to start)
                     (truncate-string-to-width (or category "")
                                               ps/agenda-layout-category-name-cols
                                               nil ?\s)))
      (disp
       (let* ((img (ps/agenda-layout--category-image category))
              (s (concat (ps/agenda-layout--space-to start)
                         (if img (ps/agenda-layout--image-cell img) ""))))
         (if (eq disp 'both)
             (concat s " "
                     (truncate-string-to-width (or category "")
                                               ps/agenda-layout-category-name-cols
                                               nil ?\s))
           s))))))

(defun ps/agenda-layout--render-item (cols schedule-compact)
  "Return the display string for the item line at point.
COLS is the column plist; SCHEDULE-COMPACT is non-nil for compact Schedule rows."
  (let* ((title (ps/agenda-layout--title))
         (state (org-get-at-bol 'todo-state))
         (tags (org-get-at-bol 'tags))
         (tod (org-get-at-bol 'time-of-day))
         (dur (org-get-at-bol 'duration))
         (pri (ps/agenda-layout--priority-char))
         (emoji (ps/agenda-layout--emoji title))
         ;; The Calendar is date-scoped (the day header / control row carries the
         ;; date), so a relative-date pill ("4w ago") has no meaning there: a
         ;; timed item shows its time (the same subtle pill as the Agenda, but
         ;; zero-padded so times right-align); an untimed item shows no right
         ;; badge.  Other views keep the reldate / compact-Schedule behaviour.
         (right (cond
                 ((and (ps/agenda-layout--calendarp) tod)
                  (let ((badge (ps/agenda-layout--reldate-text
                                (ps/agenda-layout--time-range tod dur t) 'time)))
                    (cons (string-width badge) badge)))
                 ((ps/agenda-layout--calendarp) (cons 0 nil))
                 (t (ps/agenda-layout--right-element schedule-compact tod dur))))
         (right-cols (car right))
         (right-str (cdr right))
         (reldate-tint (cdr (ps/agenda-layout--reldate-here)))
         (tag-str (ps/agenda-layout--tags-string tags))
         (title-col (plist-get cols :title))
         (avail (max 4 (floor (- (ps/agenda-layout--window-cols) title-col
                                 ps/agenda-layout-right-margin-cols
                                 (if right-str right-cols 0)
                                 (string-width tag-str)))))
         ;; Emphasis is rendered before truncating: the markers are gone by
         ;; then, so the width the columns are laid out with is the width that
         ;; is actually drawn.
         (shown (ps/emphasis-render (or title "")))
         (title-text (if ps/agenda-layout-truncate
                         (ps/agenda-layout--truncate shown avail)
                       shown))
         (parts (list (ps/agenda-layout--render-category cols))))
    (push (ps/agenda-layout--space-to (plist-get cols :state)) parts)
    (when state
      (push (ps/agenda-layout--state-text state) parts))
    (push (ps/agenda-layout--space-to (plist-get cols :pri)) parts)
    (when pri
      (push (ps/agenda-layout--priority-text pri) parts))
    (push (ps/agenda-layout--space-to (plist-get cols :emoji)) parts)
    (when emoji
      (push (if ps/agenda-layout-emoji-face
                (propertize emoji 'face ps/agenda-layout-emoji-face)
              emoji)
            parts))
    (push (ps/agenda-layout--space-to title-col) parts)
    ;; `add-face-text-property' rather than `propertize ... 'face': the latter
    ;; would replace the per-span emphasis faces instead of layering on top of
    ;; them.  Overdue red is meaningful only in the Agenda; the Calendar shows
    ;; every item in the normal foreground.
    (let ((s (copy-sequence title-text)))
      (put-text-property 0 (length s) 'help-echo (or title title-text) s)
      (when (and (not (ps/agenda-layout--calendarp))
                 (eq reldate-tint 'overdue))
        (add-face-text-property 0 (length s) 'org-warning nil s))
      (push s parts))
    (when (> (length tag-str) 0) (push tag-str parts))
    (when right-str
      (push (ps/agenda-layout--space-before-right
             right-str ps/agenda-layout-right-margin-cols) parts)
      (push right-str parts))
    (apply #'concat (nreverse parts))))

;;; Buffer-text replacement

(defun ps/agenda-layout--clear ()
  "Remove layout overlays previously placed in the current buffer."
  (remove-overlays (point-min) (point-max) 'ps/agenda-layout t))

(defun ps/agenda-layout--strip-display-props (props)
  "Return PROPS (a text-property plist) with `display', `face' and `help-echo' removed.
Used to carry org-agenda's navigation properties (`org-marker', `org-hd-marker',
`type', `todo-state', …) from the original line text onto its replacement,
without also dragging along the original line's own display/face/tooltip."
  (let (result)
    (while props
      (let ((key (car props)) (val (cadr props)))
        (unless (memq key '(display face help-echo org-not-done-regexp org-todo-regexp))
          (push key result)
          (push val result)))
      (setq props (cddr props)))
    (nreverse result)))

(defun ps/agenda-layout--replace-line (bol eol display)
  "Replace the item line [BOL, EOL) with DISPLAY.
DISPLAY is a propertized string (its own `display'/`face'/`help-echo' text
properties are kept verbatim, e.g. badge faces and `(space :align-to …)'
spacers).  The original line's org-agenda navigation properties
(`org-marker', `org-hd-marker', `type', `todo-state', …) are reapplied across
the whole replacement, so RET/TAB/bulk commands and `org-get-at-bol' keep
working anywhere on the line.  Point ends at the end of the inserted text."
  (let ((nav-props (ps/agenda-layout--strip-display-props (text-properties-at bol))))
    (goto-char bol)
    (delete-region bol eol)
    (insert display)
    (add-text-properties bol (point) nav-props)))

(defun ps/agenda-layout--hide-line (bol eol)
  "Make the whole line [BOL, EOL] (including its newline) invisible."
  (let ((ov (make-overlay bol (min (point-max) (1+ eol)))))
    (overlay-put ov 'ps/agenda-layout t)
    (overlay-put ov 'evaporate t)
    (overlay-put ov 'invisible t)))

;;; Date header

(defface ps/agenda-layout-date-button
  '((t :inherit shadow))
  "Face for the date-header navigation / refresh buttons."
  :group 'ps-agenda-layout)

(defun ps/agenda-layout--date-dotted (text)
  "Turn an org date header into a `Weekday · rest' form.
The run of spaces org inserts after the weekday (e.g.
\"Wednesday  17 June 2026\") becomes \" · \"; TEXT is returned unchanged when
there is no such run."
  (let ((s (string-trim text)))
    (if (string-match "\\`\\([^ \t]+\\)[ \t]\\{2,\\}\\(.+\\)\\'" s)
        (concat (match-string 1 s) " · " (match-string 2 s))
      s)))

;;; Calendar span / day labels

(defun ps/agenda-layout--dom (day)
  "Day-of-month number (as a string) for absolute DAY."
  (number-to-string (nth 1 (calendar-gregorian-from-absolute day))))

(defun ps/agenda-layout--date-range-label (d1 d2)
  "Compact label for the inclusive absolute-day range D1..D2.
\"18–24 May 2026\" within one month, \"28 May – 3 June 2026\" across months,
\"28 Dec 2025 – 3 Jan 2026\" across years."
  (let* ((t1 (org-time-from-absolute d1)) (t2 (org-time-from-absolute d2))
         (g1 (calendar-gregorian-from-absolute d1))
         (g2 (calendar-gregorian-from-absolute d2)))
    (cond
     ((and (= (nth 2 g1) (nth 2 g2)) (= (car g1) (car g2)))
      (format "%s–%s %s" (ps/agenda-layout--dom d1) (ps/agenda-layout--dom d2)
              (format-time-string "%B %Y" t1)))
     ((= (nth 2 g1) (nth 2 g2))
      (format "%s %s – %s %s %s"
              (ps/agenda-layout--dom d1) (format-time-string "%B" t1)
              (ps/agenda-layout--dom d2) (format-time-string "%B" t2)
              (format-time-string "%Y" t1)))
     (t
      (format "%s %s %s – %s %s %s"
              (ps/agenda-layout--dom d1) (format-time-string "%B" t1)
              (format-time-string "%Y" t1)
              (ps/agenda-layout--dom d2) (format-time-string "%B" t2)
              (format-time-string "%Y" t2))))))

(defun ps/agenda-layout--span-header-label (start span)
  "Label for the Calendar control row given absolute START day and SPAN."
  (let ((tm (org-time-from-absolute start)))
    (cond
     ((eq span 'month) (format-time-string "%B %Y" tm))
     ((eq span 'year) (format-time-string "%Y" tm))
     ((eq span 'week)
      (format "%s · W%02d" (ps/agenda-layout--date-range-label start (+ start 6))
              (org-days-to-iso-week start)))
     ((eq span 'fortnight)
      (format "%s · W%02d" (ps/agenda-layout--date-range-label start (+ start 13))
              (org-days-to-iso-week start)))
     ((and (numberp span) (> span 1))
      (ps/agenda-layout--date-range-label start (+ start (1- span))))
     (t  ;; a single day
      (format "%s · %s %s %s" (format-time-string "%A" tm)
              (ps/agenda-layout--dom start) (format-time-string "%B" tm)
              (format-time-string "%Y" tm))))))

(defun ps/agenda-layout--day-section-label (day)
  "Label for a Calendar per-day section, e.g. \"Monday · 18 May\", for absolute DAY."
  (let ((tm (org-time-from-absolute day)))
    (format "%s · %s %s" (format-time-string "%A" tm)
            (ps/agenda-layout--dom day) (format-time-string "%B" tm))))

(defun ps/agenda-layout-date-prev ()
  "Step the agenda back by its current span (a day, week, month, …)."
  (interactive)
  (when (fboundp 'org-agenda-earlier) (org-agenda-earlier 1)))

(defun ps/agenda-layout-date-next ()
  "Step the agenda forward by its current span (a day, week, month, …)."
  (interactive)
  (when (fboundp 'org-agenda-later) (org-agenda-later 1)))

(defun ps/agenda-layout-date-refresh ()
  "Reload the agenda."
  (interactive)
  (when (fboundp 'org-agenda-redo) (org-agenda-redo)))

(defun ps/agenda-layout-date-today ()
  "Show today in the agenda."
  (interactive)
  (when (fboundp 'org-agenda-goto-today) (org-agenda-goto-today)))

(defun ps/agenda-layout-date-goto ()
  "Prompt for a date and jump the agenda there (keeps the current span)."
  (interactive)
  (when (fboundp 'org-agenda-goto-date)
    (call-interactively #'org-agenda-goto-date)))

;;; Span switching (Calendar view)

(defun ps/agenda-layout--current-span ()
  "Return the span of the current Calendar view (symbol or integer day count)."
  (or ps/agenda-layout--cal-span
      (let ((args (get-text-property (min (1- (point-max)) (point)) 'org-last-args)))
        (nth 2 args))
      (and (boundp 'org-agenda-current-span) org-agenda-current-span)
      'day))

(defun ps/agenda-layout--current-start ()
  "Return the absolute start day of the current Calendar span."
  (or ps/agenda-layout--cal-start
      (let ((args (get-text-property (min (1- (point-max)) (point)) 'org-last-args)))
        (nth 1 args))
      (and (boundp 'org-starting-day) org-starting-day)
      (and (fboundp 'org-today) (org-today))))

(defun ps/agenda-layout--current-ndays ()
  "Return the number of days the current Calendar span covers, as an integer."
  (let ((span (ps/agenda-layout--current-span))
        (sd (ps/agenda-layout--current-start)))
    (cond
     ((numberp span) span)
     ((eq span 'day) 1)
     ((eq span 'week) 7)
     ((eq span 'fortnight) 14)
     ((and (eq span 'month) sd)
      (let ((g (calendar-gregorian-from-absolute sd)))
        (calendar-last-day-of-month (car g) (nth 2 g))))
     ((and (eq span 'year) sd)
      (let ((g (calendar-gregorian-from-absolute sd)))
        (if (calendar-leap-year-p (nth 2 g)) 366 365)))
     (t 1))))

(defun ps/agenda-layout--rebuild (start span)
  "Rebuild the agenda showing SPAN days starting at absolute day START.
SPAN is a span symbol or a positive integer day count.  Mirrors the mechanism
Org uses in `org-agenda-later': set `org-agenda-overriding-arguments' (whose
third element is the span) and redo, so the active custom command re-runs."
  (when (derived-mode-p 'org-agenda-mode)
    (org-agenda-check-type t 'agenda)
    (let* ((pos  (min (1- (point-max)) (point)))
           (args (get-text-property pos 'org-last-args))
           (org-agenda-overriding-cmd (get-text-property pos 'org-series-cmd))
           (org-agenda-overriding-arguments (list (car args) start span)))
      (org-agenda-redo)
      (when (fboundp 'org-agenda-find-same-or-today-or-agenda)
        (org-agenda-find-same-or-today-or-agenda)))))

(defun ps/agenda-layout--set-span (span)
  "Rebuild the agenda with SPAN, aligning the start to its calendar boundary.
A symbol span (`day', `week', `month', `year') is aligned to the containing
period (the week respects `calendar-week-start-day')."
  (when (derived-mode-p 'org-agenda-mode)
    (org-agenda-check-type t 'agenda)
    (let* ((pos (min (1- (point-max)) (point)))
           (args (get-text-property pos 'org-last-args))
           (sd0 (or (org-get-at-bol 'day) (nth 1 args)
                    ps/agenda-layout--cal-start org-starting-day))
           (org-agenda-start-on-weekday
            (if (memq span '(week fortnight)) calendar-week-start-day
              (and (boundp 'org-agenda-start-on-weekday) org-agenda-start-on-weekday)))
           (sd (if (memq span '(day week fortnight month year))
                   (org-agenda-compute-starting-span sd0 span)
                 sd0)))
      (ps/agenda-layout--rebuild sd span))))

(defun ps/agenda-layout-span-day ()
  "Switch the Calendar to a single-day view."
  (interactive)
  (ps/agenda-layout--set-span 'day))

(defun ps/agenda-layout-span-week ()
  "Switch the Calendar to a week view."
  (interactive)
  (ps/agenda-layout--set-span 'week))

(defun ps/agenda-layout-span-month ()
  "Switch the Calendar to a month view."
  (interactive)
  (ps/agenda-layout--set-span 'month))

(defun ps/agenda-layout-span-year ()
  "Switch the Calendar to a year view."
  (interactive)
  (ps/agenda-layout--set-span 'year))

(defun ps/agenda-layout-span-range (start end)
  "Show the Calendar for the inclusive date range START..END.
Interactively prompts for both dates with Org's date picker (the same one
`C-c C-s' uses); any time entered is ignored — only the dates matter."
  (interactive
   (list (org-read-date nil nil nil "Calendar range — start date")
         (org-read-date nil nil nil "Calendar range — end date")))
  (let* ((d1 (time-to-days (org-time-string-to-time start)))
         (d2 (time-to-days (org-time-string-to-time end)))
         (lo (min d1 d2))
         (hi (max d1 d2)))
    (ps/agenda-layout--rebuild lo (1+ (- hi lo)))))

(defun ps/agenda-layout--date-is-today-p (pos)
  "Return non-nil when the agenda date header at POS is today."
  (let ((day (get-text-property pos 'day)))
    (and day (fboundp 'org-today) (= day (org-today)))))

(defun ps/agenda-layout--date-button (icon-name fallback cmd help)
  "Return a clickable one-column button.
Shows Material Symbol ICON-NAME (or FALLBACK glyph on non-graphical frames),
runs CMD on click/RET, with tooltip HELP."
  (let ((img (and (display-graphic-p)
                  (fboundp 'ps/material-icons-image)
                  (ps/material-icons-image icon-name)))
        (map (make-sparse-keymap)))
    (define-key map [mouse-1] cmd)
    (define-key map (kbd "RET") cmd)
    (let ((s (propertize (if img " " fallback)
                         'keymap map 'mouse-face 'highlight
                         'help-echo help 'face 'ps/agenda-layout-date-button)))
      (when img (put-text-property 0 1 'display img s))
      s)))

(defun ps/agenda-layout--text-button (label cmd help &optional active)
  "Return a clickable boxed button showing LABEL, running CMD, tooltip HELP.
When ACTIVE, render it filled to mark the span the view is currently on."
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] cmd)
    (define-key map (kbd "RET") cmd)
    (propertize (concat " " label " ")
                'keymap map 'mouse-face 'highlight 'help-echo help
                'face (if active 'ps/agenda-layout-span-button-active
                        'ps/agenda-layout-span-button))))

(defun ps/agenda-layout--span-row ()
  "Return the Calendar span-switch controls: D W M Y and a date Range… picker.
The control matching the currently displayed span is filled."
  (let* ((span (ps/agenda-layout--current-span))
         (cur (cond ((memq span '(day 1 nil)) 'day) ((eq span 'week) 'week)
                    ((eq span 'month) 'month) ((eq span 'year) 'year) (t 'custom))))
    (concat
     (ps/agenda-layout--text-button "D" #'ps/agenda-layout-span-day   "Day view"   (eq cur 'day))
     " "
     (ps/agenda-layout--text-button "W" #'ps/agenda-layout-span-week  "Week view"  (eq cur 'week))
     " "
     (ps/agenda-layout--text-button "M" #'ps/agenda-layout-span-month "Month view" (eq cur 'month))
     " "
     (ps/agenda-layout--text-button "Y" #'ps/agenda-layout-span-year  "Year view"  (eq cur 'year))
     "  "
     (ps/agenda-layout--text-button "Range…" #'ps/agenda-layout-span-range
                                    "Show an arbitrary date range…" (eq cur 'custom)))))

(defun ps/agenda-layout--situation-row ()
  "Return the Situation view's switcher control, or nil when unavailable.
A single dropdown button: with seven-odd situations a button each would crowd
the row, and the same list is one click away in the mode line."
  (when (fboundp 'ps/situations-switch)
    (ps/agenda-layout--text-button "Situations ▾" #'ps/situations-switch
                                   "Switch to another situation…")))

(defun ps/agenda-layout--centered-controls (label face show-today
                                                  &optional right nav prefix)
  "Display string for a control row: LABEL (in FACE), centred, hugged by buttons.

NAV draws the date navigation — prev/next chevrons flanking the label, plus the
go-to-today button when SHOW-TODAY is non-nil.  A refresh button always follows
the label.  RIGHT, when given, is a control string right-aligned at the content
edge (the Calendar's span switcher, the Situations switcher).

PREFIX is a decoration placed just left of LABEL (a Situation's icon) and is
deliberately *excluded* from the centring: it is LABEL that should read as
centred, and folding the icon into the measured width pushes the words off
centre by its own width.  Used only where NAV is nil, since the chevrons
occupy the same space.

Date navigation is meaningful only in a dated view: the Situation views pass
NAV nil, since stepping \"a period\" back from a tag search means nothing."
  (let* ((tw (string-width label))
         (win (ps/agenda-layout--window-cols))
         ;; Centre within the content area [left-margin, win - right-margin].
         (tstart (max ps/agenda-layout-left-margin-cols
                      (+ ps/agenda-layout-left-margin-cols
                         (/ (- (- win ps/agenda-layout-left-margin-cols
                                  ps/agenda-layout-right-margin-cols)
                               tw)
                            2)))))
    (concat
     (when (and nav show-today)
       (concat
        (ps/agenda-layout--space-to (max 0 (- tstart 4)))
        (ps/agenda-layout--date-button "today" "⊙"
                                       #'ps/agenda-layout-date-today "Go to today")))
     (when nav
       (concat
        (ps/agenda-layout--space-to (max 0 (- tstart 2)))
        (ps/agenda-layout--date-button "chevron_left" "‹"
                                       #'ps/agenda-layout-date-prev "Previous period")))
     (when (and prefix (not nav))
       (concat
        (ps/agenda-layout--space-to (max 0 (- tstart (1+ (string-width prefix)))))
        prefix))
     (ps/agenda-layout--space-to tstart)
     (propertize label 'face face 'help-echo label)
     (when nav
       (concat
        (ps/agenda-layout--space-to (+ tstart tw 1))
        (ps/agenda-layout--date-button "chevron_right" "›"
                                       #'ps/agenda-layout-date-next "Next period")))
     (ps/agenda-layout--space-to (+ tstart tw (if nav 4 2)))
     (ps/agenda-layout--date-button "refresh" "⟳"
                                    #'ps/agenda-layout-date-refresh "Reload agenda")
     (when right
       (concat
        (ps/agenda-layout--space-before-right
         right ps/agenda-layout-right-margin-cols)
        right)))))

(defun ps/agenda-layout--reformat-date-header (bol eol &optional _controls)
  "Rewrite the Agenda's date-header line [BOL, EOL) as a centred control row.
The date text and its org day face are stashed (`ps/date-text' / `ps/date-face')
and reused on every re-decoration, so a resize re-centres for the new width and
today's highlight / weekend underline are preserved.  (The Calendar uses its own
control row and day sections; this is the Agenda day header.)"
  (let* ((stored (get-text-property bol 'ps/date-text))
         (text   (or stored (string-trim (buffer-substring-no-properties bol eol))))
         (face   (if stored (get-text-property bol 'ps/date-face)
                   (get-text-property bol 'face)))
         (dotted (ps/agenda-layout--date-dotted text)))
    (ps/agenda-layout--replace-line
     bol eol
     (ps/agenda-layout--centered-controls
      dotted face (not (ps/agenda-layout--date-is-today-p bol)) nil t))
    (put-text-property bol (point) 'ps/date-text text)
    (put-text-property bol (point) 'ps/date-face face)))

(defun ps/agenda-layout--reformat-control-row (bol eol label show-today
                                                   &optional right nav face gap prefix)
  "Turn the line [BOL, EOL) into a top control row showing LABEL.
SHOW-TODAY, RIGHT, NAV and PREFIX are passed to
`ps/agenda-layout--centered-controls'; FACE styles LABEL (default
`ps/agenda-layout-control-label').

GAP adds one blank line beneath the row.  Only the Situation views ask for it:
after the Calendar's block header Org already leaves a real empty line, while a
tag search runs straight into its first match.

The gap is a `before-string' on the line *below*, not an `after-string' on this
one — the same form the file tree uses for the strip above each top-level
section (`ps/file-tree--gap-overlay').  An `after-string' on an empty overlay
sitting at this line's newline is not drawn.  It carries an inert keymap so a
stray click on the blank does nothing, rather than falling through to the
nearest control-row button."
  (ps/agenda-layout--replace-line
   bol eol
   (ps/agenda-layout--centered-controls
    label (or face 'ps/agenda-layout-control-label) show-today right nav prefix))
  (when gap
    (save-excursion
      (forward-line 1)
      (let ((ov (make-overlay (point) (point)))
            (m (make-sparse-keymap)))
        (define-key m [mouse-1] #'ignore)
        (overlay-put ov 'ps/agenda-layout t)
        (overlay-put ov 'before-string (propertize "\n" 'keymap m))))))

(defun ps/agenda-layout--reformat-day-section (bol eol)
  "Turn a Calendar day header [BOL, EOL) into a left-aligned grey day section.
The label (\"Monday · 18 May\") sits at the left margin; the grey band fills to
the window edge via the face's `:extend' on the trailing newline (so it never
overruns the right edge).  `ps-agenda-fold' adds the ▾/▸ collapse indicator."
  (let* ((day   (org-get-at-bol 'day))
         (label (if day (ps/agenda-layout--day-section-label day)
                  (ps/agenda-layout--date-dotted
                   (string-trim (buffer-substring-no-properties bol eol)))))
         (inner (concat (make-string ps/agenda-layout-left-margin-cols ?\s) label)))
    (ps/agenda-layout--replace-line
     bol eol
     (propertize inner 'face 'ps/agenda-layout-day-section))
    ;; Extend the band to the window edge through the newline (face has :extend t),
    ;; rather than padding with spaces (which overran the right edge).
    (when (< (point) (point-max))
      (let ((ov (make-overlay (point) (min (point-max) (1+ (point))))))
        (overlay-put ov 'ps/agenda-layout t)
        (overlay-put ov 'evaporate t)
        (overlay-put ov 'face 'ps/agenda-layout-day-section)))))

;;; Main pass

(defvar ps/agenda-layout--relayout nil
  "Non-nil while re-laying out without a fresh agenda build (a resize).
When set, `ps/agenda-layout--apply' keeps the buffer-local view kind instead of
recomputing it, since the custom command's `ps/agenda-layout-view-kind' let is
not in scope during a resize.")

(defun ps/agenda-layout--apply ()
  "Reformat the agenda task lines in the current buffer."
  (when (and ps/agenda-layout-enabled (derived-mode-p 'org-agenda-mode))
    ;; Record the view kind for this build.  A fresh build (finalize hook) always
    ;; resets it from the command's let — `agenda' / `calendar', or `other' for a
    ;; plain render with no command (Tasks, stock `a', a timestamp click) — so a
    ;; stale value can never leak across views.  A resize re-layout keeps it (the
    ;; let is out of scope then).  Only the Agenda (`agenda') drives the
    ;; auto-refresh timer; only the Calendar (`calendar') draws span controls.
    (unless ps/agenda-layout--relayout
      (setq-local ps/agenda-layout--view-kind
                  (or ps/agenda-layout-view-kind 'other)))
    ;; Cache the Calendar span/start on a fresh build so the control-row label and
    ;; span detection survive a resize re-layout (when the org vars are unbound).
    (when (and (ps/agenda-layout--calendarp) (not ps/agenda-layout--relayout))
      (setq-local ps/agenda-layout--cal-start
                  (and (boundp 'org-starting-day) org-starting-day))
      (setq-local ps/agenda-layout--cal-span
                  (and (boundp 'org-agenda-current-span) org-agenda-current-span)))
    (let* ((inhibit-read-only t)
           (ps/agenda-layout--reserve-priority
            (ps/agenda-layout--scope-has-priority-p nil))
           (cols (ps/agenda-layout--columns))
           (style ps/agenda-layout-schedule-style)
           (calendarp (ps/agenda-layout--calendarp))
           (situationp (ps/agenda-layout--situationp))
           (cal-start (and calendarp (ps/agenda-layout--current-start)))
           (cal-span  (and calendarp (ps/agenda-layout--current-span)))
           (cal-ndays (and calendarp (ps/agenda-layout--current-ndays)))
           (cal-multi (and calendarp (> (or cal-ndays 1) 1)))
           (show-today (and calendarp cal-start
                            (let ((today (org-today)))
                              (not (and (>= today cal-start)
                                        (< today (+ cal-start cal-ndays)))))))
           (header "")
           (control-done nil)
           (first-date t))
      (ps/agenda-layout--clear)
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (let ((bol (line-beginning-position))
                (eol (line-end-position)))
            (cond
             ;; Calendar / Situation: the first block (structural) header becomes
             ;; the top control row — labelled with the whole span (day / week
             ;; range / …) or with the situation's name.  Both views deliberately
             ;; leave `org-agenda-overriding-header' unset so Org emits this line
             ;; for us to rewrite.
             ((and (or calendarp situationp) (not control-done)
                   (ps/agenda-layout--header-p)
                   (not (org-get-at-bol 'org-agenda-date-header)))
              (if situationp
                  (ps/agenda-layout--reformat-control-row
                   bol eol
                   (if (fboundp 'ps/situations-plate-label)
                       (ps/situations-plate-label)
                     "Situation")
                   nil (ps/agenda-layout--situation-row) nil
                   'ps/agenda-layout-situation-label t
                   (and (fboundp 'ps/situations-plate-icon)
                        (ps/situations-plate-icon)))
                (ps/agenda-layout--reformat-control-row
                 bol eol
                 (ps/agenda-layout--span-header-label cal-start cal-span)
                 show-today (ps/agenda-layout--span-row) t))
              (setq control-done t))
             ;; Date headers.
             ((org-get-at-bol 'org-agenda-date-header)
              (cond
               ((not calendarp)
                (ps/agenda-layout--reformat-date-header bol eol first-date)
                (setq first-date nil))
               (cal-multi
                ;; week/month/year: a left-aligned, collapsible day section.
                (ps/agenda-layout--reformat-day-section bol eol))
               (t
                ;; day view: the control row already names the day.
                (ps/agenda-layout--hide-line bol eol))))
             ((ps/agenda-layout--header-p)
              (setq header (string-trim (buffer-substring-no-properties bol eol))))
             ((ps/agenda-layout--item-p)
              (let ((in-sched (ps/agenda-layout--header-schedule-p header)))
                (unless (and in-sched
                             (or (eq style 'grid) ps/schedule-view-override))
                  (ps/agenda-layout--replace-line
                   bol eol
                   (ps/agenda-layout--render-item cols in-sched)))))
             ((and (org-get-at-bol 'time-of-day)
                   (ps/agenda-layout--header-schedule-p header)
                   (eq style 'compact)
                   (not ps/schedule-view-override))
              (ps/agenda-layout--hide-line bol eol))))
          (forward-line 1))))))

;;; Public API

(defun ps/agenda-layout-refresh ()
  "Re-run the layout pass on the *Org Agenda* buffer, if it exists.
This is a re-layout (e.g. after a window resize), not a fresh agenda build, so
the buffer-local view kind is preserved."
  (let ((buf (get-buffer "*Org Agenda*")))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (let ((ps/agenda-layout--relayout t))
          (ps/agenda-layout--apply))))))

(defvar ps/agenda-layout--resize-timer nil
  "Idle timer debouncing layout refreshes on window resize.")

(defun ps/agenda-layout--on-window-resize (_frame)
  "Debounce a layout refresh after the *Org Agenda* window changes size.
Titles are truncated to the agenda window's width, so a resize must
re-run the layout pass for the title column to grow or shrink to match."
  (when (get-buffer-window "*Org Agenda*" t)
    (when ps/agenda-layout--resize-timer
      (cancel-timer ps/agenda-layout--resize-timer))
    (setq ps/agenda-layout--resize-timer
          (run-with-idle-timer
           0.2 nil
           (lambda ()
             (setq ps/agenda-layout--resize-timer nil)
             (ps/agenda-layout-refresh))))))

(defun ps/agenda-layout-setup ()
  "Enable the aligned agenda layout after each agenda render.
Also re-runs the layout when the *Org Agenda* window is resized, so titles
reflow to the new width."
  (add-hook 'org-agenda-finalize-hook #'ps/agenda-layout--apply t)
  (add-hook 'window-size-change-functions #'ps/agenda-layout--on-window-resize))

(provide 'ps-agenda-layout)
;;; ps-agenda-layout.el ends here
