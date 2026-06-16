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
(defvar org-agenda-finalize-hook)
(defvar org-done-keywords)
(defvar org-todo-all-keywords)
(defvar ps/material-icons-category-map)

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
  '((t :inherit (org-warning org-modern-label) :inverse-video t))
  "Badge face for overdue relative dates."
  :group 'ps-agenda-layout)

(defface ps/agenda-layout-reldate-today
  '((t :inherit (org-scheduled-today org-modern-label) :inverse-video t))
  "Badge face for today's relative dates."
  :group 'ps-agenda-layout)

(defface ps/agenda-layout-reldate-future
  '((t :inherit (shadow org-modern-label) :inverse-video t))
  "Badge face for future relative dates."
  :group 'ps-agenda-layout)

(defface ps/agenda-layout-reldate-time
  '((t :inherit (shadow org-modern-label) :inverse-video t))
  "Badge face for time-range badges (compact Schedule events)."
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
             (string-trim title))))))

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

(defun ps/agenda-layout--fmt-tod (hhmm)
  "Format integer HHMM (e.g. 930) as \"9:30\"."
  (format "%d:%02d" (/ hhmm 100) (% hhmm 100)))

(defun ps/agenda-layout--time-range (tod dur)
  "Format a time range from TOD (HHMM integer) and DUR (minutes), or just TOD."
  (if (and dur (numberp dur) (> dur 0))
      (let* ((mins (+ (* (/ tod 100) 60) (% tod 100) (round dur)))
             (end (+ (* (/ mins 60) 100) (% mins 60))))
        (format "%s–%s" (ps/agenda-layout--fmt-tod tod) (ps/agenda-layout--fmt-tod end)))
    (ps/agenda-layout--fmt-tod tod)))

(defun ps/agenda-layout--truncate (s maxcols)
  "Truncate S to MAXCOLS display columns, appending the ellipsis if shortened."
  (if (<= (string-width s) maxcols)
      s
    (truncate-string-to-width s (max 1 maxcols) nil nil ps/agenda-layout-ellipsis)))

;;; Column geometry

(defun ps/agenda-layout--effective-state-cols ()
  "Return the effective state-column width in character columns.
Uses `ps/agenda-layout-state-cols' when set to a non-nil integer; otherwise
derives the width from the longest keyword in `org-todo-all-keywords' plus
2 padding spaces (one on each side of the label)."
  (or ps/agenda-layout-state-cols
      (+ 2 (if (boundp 'org-todo-all-keywords)
               (apply #'max 0 (mapcar #'string-width org-todo-all-keywords))
             4))))

(defun ps/agenda-layout--columns ()
  "Return a plist of column start positions (in columns) from the settings."
  (let* ((left ps/agenda-layout-left-margin-cols)
         (gap ps/agenda-layout-gap-cols)
         (cat-cols (pcase ps/agenda-layout-category-display
                     ('none 0)
                     ('icon ps/agenda-layout-category-cols)
                     ('name ps/agenda-layout-category-name-cols)
                     ('both (+ ps/agenda-layout-category-cols 1
                               ps/agenda-layout-category-name-cols))))
         (state (+ left cat-cols (if (> cat-cols 0) gap 0)))
         (pri (+ state (ps/agenda-layout--effective-state-cols) gap))
         (emoji (+ pri ps/agenda-layout-priority-cols gap))
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
Uses `org-modern-done' for done keywords and `org-modern-todo' otherwise,
matching the badge style org-modern applies in org buffers."
  (let* ((label (ps/agenda-layout--state-label state))
         (done-p (and (boundp 'org-done-keywords)
                      (member state org-done-keywords)))
         (face (if done-p 'org-modern-done 'org-modern-todo)))
    (propertize (concat " " label " ") 'face face)))

(defun ps/agenda-layout--priority-text (char)
  "Return propertized badge text for priority CHAR (e.g. ?A), or nil."
  (when char
    (propertize (format " #%c " char) 'face 'org-modern-priority)))

(defun ps/agenda-layout--reldate-text (text tint)
  "Return propertized badge text for reldate TEXT with color TINT symbol.
TINT is one of `overdue', `today', `future', or `time' (timed events)."
  (propertize (concat " " text " ")
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
         (right (ps/agenda-layout--right-element schedule-compact tod dur))
         (right-cols (car right))
         (right-str (cdr right))
         (tag-str (ps/agenda-layout--tags-string tags))
         (title-col (plist-get cols :title))
         (avail (max 4 (- (ps/agenda-layout--window-cols) title-col
                          (if right-str (+ right-cols ps/agenda-layout-right-margin-cols) 0)
                          (string-width tag-str))))
         (title-text (if ps/agenda-layout-truncate
                         (ps/agenda-layout--truncate (or title "") avail)
                       (or title "")))
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
    (push (propertize title-text 'help-echo (or title title-text)) parts)
    (when (> (length tag-str) 0) (push tag-str parts))
    (when right-str
      (push (ps/agenda-layout--space-to-right right-cols) parts)
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
        (unless (memq key '(display face help-echo))
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

;;; Main pass

(defun ps/agenda-layout--apply ()
  "Reformat the agenda task lines in the current buffer."
  (when (and ps/agenda-layout-enabled (derived-mode-p 'org-agenda-mode))
    (let ((inhibit-read-only t)
          (cols (ps/agenda-layout--columns))
          (style ps/agenda-layout-schedule-style)
          (header ""))
      (ps/agenda-layout--clear)
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (let ((bol (line-beginning-position))
                (eol (line-end-position)))
            (cond
             ((ps/agenda-layout--header-p)
              (setq header (string-trim (buffer-substring-no-properties bol eol))))
             ((ps/agenda-layout--item-p)
              (let ((in-sched (ps/agenda-layout--header-schedule-p header)))
                (unless (and in-sched (eq style 'grid))
                  (ps/agenda-layout--replace-line
                   bol eol
                   (ps/agenda-layout--render-item cols in-sched)))))
             ((and (org-get-at-bol 'time-of-day)
                   (ps/agenda-layout--header-schedule-p header)
                   (eq style 'compact))
              (ps/agenda-layout--hide-line bol eol))))
          (forward-line 1))))))

;;; Public API

(defun ps/agenda-layout-refresh ()
  "Re-run the layout pass on the *Org Agenda* buffer, if it exists."
  (let ((buf (get-buffer "*Org Agenda*")))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (ps/agenda-layout--apply)))))

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
