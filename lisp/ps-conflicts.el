;;; ps-conflicts.el --- Org scheduling conflict detection -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'calendar)
(require 'ps-file-tree)
(require 'ps-org-files)
(require 'ps-window)
(require 'ps-mode-line)

;;; Customization

(defcustom ps/conflicts-gap-minutes 15
  "Minimum gap between consecutive tasks in minutes to consider sufficient."
  :type 'integer)

(defcustom ps/conflicts-include-past nil
  "Whether to include past events when checking for conflicts.
Applies to both the interactive buffer default and the agenda check."
  :type 'boolean)

;;; Faces

(defface ps/conflicts--button-face
  '((t :inherit custom-button))
  "Face for clickable buttons in the *Org Conflicts* header.")

(defface ps/conflicts--button-active-face
  '((t :inherit custom-button-pressed))
  "Face for active/enabled toggle buttons in the *Org Conflicts* header.")

;;; Constants

(defconst ps/conflicts--timestamp-re
  (concat "<\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)"
          " [A-Za-z]+"
          " \\([0-9]\\{2\\}:[0-9]\\{2\\}\\)-\\([0-9]\\{2\\}:[0-9]\\{2\\}\\)>")
  "Regexp matching org timed timestamps with explicit time ranges.
Groups: 1=date (YYYY-MM-DD), 2=start (HH:MM), 3=end (HH:MM).")

(defconst ps/conflicts--heading-re
  "^\\*+ +\\(.*\\)"
  "Regexp matching org headings; group 1 is the title text.")

;;; Data structure

(cl-defstruct (ps/conflict--event
               (:constructor ps/conflict--make-event))
  "A timed org task with location, title, and time range."
  file line title month day year start end)

;;; Time/date helpers

(defun ps/conflicts--parse-time (str)
  "Parse HH:MM string STR to minutes since midnight."
  (let* ((parts (split-string str ":"))
         (h (string-to-number (car parts)))
         (m (string-to-number (cadr parts))))
    (+ (* h 60) m)))

(defun ps/conflicts--parse-date (str)
  "Parse YYYY-MM-DD string STR to (month day year) list."
  (let* ((parts (split-string str "-"))
         (year  (string-to-number (nth 0 parts)))
         (month (string-to-number (nth 1 parts)))
         (day   (string-to-number (nth 2 parts))))
    (list month day year)))

(defun ps/conflicts--format-time (mins)
  "Format MINS (minutes since midnight) as HH:MM string."
  (format "%02d:%02d" (/ mins 60) (% mins 60)))

(defun ps/conflicts--today ()
  "Return today as (month day year) list."
  (let ((now (decode-time)))
    (list (nth 4 now) (nth 3 now) (nth 5 now))))

(defun ps/conflicts--date< (a b)
  "Return t if date A is strictly earlier than date B.
Dates are (month day year) lists."
  (let ((ya (nth 2 a)) (yb (nth 2 b))
        (ma (nth 0 a)) (mb (nth 0 b))
        (da (nth 1 a)) (db (nth 1 b)))
    (or (< ya yb)
        (and (= ya yb) (< ma mb))
        (and (= ya yb) (= ma mb) (< da db)))))

(defun ps/conflicts--event-date (event)
  "Return the date of EVENT as a (month day year) list."
  (list (ps/conflict--event-month event)
        (ps/conflict--event-day event)
        (ps/conflict--event-year event)))

;;; Parsing

(defun ps/conflicts--extract-events-from-file (path)
  "Extract timed events from the org file at PATH.
Returns a list of `ps/conflict--event' structs."
  (let ((events '())
        (current-title nil))
    (with-temp-buffer
      (insert-file-contents path)
      (goto-char (point-min))
      (while (not (eobp))
        (let* ((line-num (line-number-at-pos))
               (line (buffer-substring-no-properties
                      (point) (line-end-position))))
          (when (string-match ps/conflicts--heading-re line)
            (setq current-title (string-trim (match-string 1 line))))
          (when (and current-title
                     (string-match ps/conflicts--timestamp-re line))
            (let* ((date-str  (match-string 1 line))
                   (start-str (match-string 2 line))
                   (end-str   (match-string 3 line))
                   (date  (ps/conflicts--parse-date date-str))
                   (start (ps/conflicts--parse-time start-str))
                   (end   (ps/conflicts--parse-time end-str)))
              (push (ps/conflict--make-event
                     :file  path
                     :line  line-num
                     :title current-title
                     :month (nth 0 date)
                     :day   (nth 1 date)
                     :year  (nth 2 date)
                     :start start
                     :end   end)
                    events)))
          (forward-line 1))))
    (nreverse events)))

(defun ps/conflicts--load-events (directory)
  "Load timed events from all .org files under DIRECTORY, at any depth.
Returns a list of `ps/conflict--event' structs. The file list comes from
`ps/org-files-in-directory' (so the same exclusions as the agenda apply) and
is passed through `ps/file-tree-filter-files', so it is scoped to the active
file set when `ps/file-tree-set-applies-to-agenda' is enabled."
  (let ((events '()))
    (dolist (file (ps/file-tree-filter-files
                   (ps/org-files-in-directory directory)))
      (setq events (append events
                            (ps/conflicts--extract-events-from-file file))))
    events))

;;; Conflict detection

(defun ps/conflicts--group-by-day (events)
  "Group EVENTS by date.
Returns a sorted alist of ((month day year) . events-list)."
  (let ((table (make-hash-table :test 'equal)))
    (dolist (e events)
      (let ((key (ps/conflicts--event-date e)))
        (puthash key (append (gethash key table '()) (list e)) table)))
    (let ((result '()))
      (maphash (lambda (k v) (push (cons k v) result)) table)
      (sort result (lambda (a b) (ps/conflicts--date< (car a) (car b)))))))

(defun ps/conflicts--find-overlaps (events)
  "Find overlapping event pairs within EVENTS.
Returns a list of (event-a event-b) pairs.
An overlap occurs when event-b starts before event-a ends."
  (let* ((sorted (sort (copy-sequence events)
                       (lambda (a b)
                         (< (ps/conflict--event-start a)
                            (ps/conflict--event-start b)))))
         (n (length sorted))
         (overlaps '()))
    (dotimes (i n)
      (let ((a (nth i sorted))
            (j (1+ i)))
        (while (and (< j n)
                    (< (ps/conflict--event-start (nth j sorted))
                       (ps/conflict--event-end a)))
          (push (list a (nth j sorted)) overlaps)
          (setq j (1+ j)))))
    (nreverse overlaps)))

(defun ps/conflicts--find-small-gaps (events gap-minutes)
  "Find consecutive event pairs in EVENTS with a gap smaller than GAP-MINUTES.
Returns a list of (event-a event-b gap-in-minutes) triples.
Only non-negative gaps (i.e. non-overlapping pairs) are considered."
  (let* ((sorted (sort (copy-sequence events)
                       (lambda (a b)
                         (< (ps/conflict--event-start a)
                            (ps/conflict--event-start b)))))
         (n (length sorted))
         (gaps '()))
    (dotimes (i (max 0 (1- n)))
      (let* ((a (nth i sorted))
             (b (nth (1+ i) sorted))
             (gap (- (ps/conflict--event-start b)
                     (ps/conflict--event-end a))))
        (when (and (>= gap 0) (< gap gap-minutes))
          (push (list a b gap) gaps))))
    (nreverse gaps)))

;;; Public API

(defun ps/conflicts--find-all (directory gap-minutes include-past)
  "Find all scheduling conflicts in DIRECTORY.
GAP-MINUTES is the threshold below which a gap is flagged.
INCLUDE-PAST non-nil includes events from before today.
Returns a plist with :overlaps and :gaps, each an alist of (date . pairs)."
  (let* ((events (ps/conflicts--load-events directory))
         (events (if include-past
                     events
                   (let ((today (ps/conflicts--today)))
                     (cl-remove-if
                      (lambda (e)
                        (ps/conflicts--date< (ps/conflicts--event-date e) today))
                      events))))
         (by-day (ps/conflicts--group-by-day events))
         (all-overlaps '())
         (all-gaps '()))
    (dolist (entry by-day)
      (let* ((day-key    (car entry))
             (day-events (cdr entry))
             (overlaps   (ps/conflicts--find-overlaps day-events))
             (gaps       (ps/conflicts--find-small-gaps day-events gap-minutes)))
        (when overlaps (push (cons day-key overlaps) all-overlaps))
        (when gaps     (push (cons day-key gaps)     all-gaps))))
    (list :overlaps (nreverse all-overlaps)
          :gaps     (nreverse all-gaps))))

(defun ps/conflicts--count (result)
  "Return the total number of conflict entries in RESULT plist."
  (let ((n 0))
    (dolist (entry (plist-get result :overlaps)) (cl-incf n (length (cdr entry))))
    (dolist (entry (plist-get result :gaps))     (cl-incf n (length (cdr entry))))
    n))

;;; Formatting

(defun ps/conflicts--format-date (date)
  "Format DATE (month day year) as YYYY-MM-DD string."
  (format "%04d-%02d-%02d" (nth 2 date) (nth 0 date) (nth 1 date)))

(defun ps/conflicts--format-event-label (event)
  "Format EVENT as 'TITLE | HH:MM-HH:MM (basename:line)'."
  (format "%s | %s-%s (%s:%d)"
          (ps/conflict--event-title event)
          (ps/conflicts--format-time (ps/conflict--event-start event))
          (ps/conflicts--format-time (ps/conflict--event-end event))
          (file-name-nondirectory (ps/conflict--event-file event))
          (ps/conflict--event-line event)))

;;; Interactive buffer

(defvar-local ps/conflicts--cur-directory nil
  "Org directory for the current *Org Conflicts* session.")
(defvar-local ps/conflicts--cur-gap nil
  "Gap threshold (minutes) for the current session.")
(defvar-local ps/conflicts--cur-include-past nil
  "Whether to include past events in the current session.")

(define-derived-mode ps-conflicts-mode special-mode "Conflicts"
  "Major mode for the *Org Conflicts* buffer.
\\{ps-conflicts-mode-map}"
  (setq-local mode-line-format '((:eval (ps/mode-line--simple-view-render "Conflicts")))))

(let ((map ps-conflicts-mode-map))
  (define-key map (kbd "g")   #'ps/conflicts--buffer-refresh)
  (define-key map (kbd "r")   #'ps/conflicts--buffer-refresh)
  (define-key map (kbd "+")   #'ps/conflicts--buffer-more-gap)
  (define-key map (kbd "-")   #'ps/conflicts--buffer-less-gap)
  (define-key map (kbd "p")   #'ps/conflicts--buffer-toggle-past)
  (define-key map (kbd "RET") #'ps/conflicts--visit-at-point))

(defun ps/conflicts--buffer-refresh ()
  "Re-render the conflicts buffer."
  (interactive)
  (ps/conflicts--buffer-render))

(defun ps/conflicts--buffer-more-gap ()
  "Increase gap threshold by 5 minutes and refresh."
  (interactive)
  (setq ps/conflicts--cur-gap (+ ps/conflicts--cur-gap 5))
  (ps/conflicts--buffer-render))

(defun ps/conflicts--buffer-less-gap ()
  "Decrease gap threshold by 5 minutes (minimum 0) and refresh."
  (interactive)
  (setq ps/conflicts--cur-gap (max 0 (- ps/conflicts--cur-gap 5)))
  (ps/conflicts--buffer-render))

(defun ps/conflicts--buffer-toggle-past ()
  "Toggle inclusion of past events and refresh."
  (interactive)
  (setq ps/conflicts--cur-include-past (not ps/conflicts--cur-include-past))
  (ps/conflicts--buffer-render))

(defun ps/conflicts--visit-at-point ()
  "Activate the button at point, jumping to the conflict location."
  (interactive)
  (let ((btn (button-at (point))))
    (when btn (button-activate btn))))

(defun ps/conflicts--insert-button (label action &optional active)
  "Insert a styled button with LABEL that calls ACTION when pressed.
ACTIVE non-nil renders it in the pressed/active face."
  (insert-button label
                 'face       (if active
                                 'ps/conflicts--button-active-face
                               'ps/conflicts--button-face)
                 'mouse-face 'custom-button-pressed-unraised
                 'action     action
                 'follow-link t))

(defun ps/conflicts--insert-header (buf)
  "Insert the interactive control header into BUF."
  (with-current-buffer buf
    (insert "  Gap: ")
    (ps/conflicts--insert-button
     " − "
     (lambda (_b)
       (with-current-buffer buf
         (setq ps/conflicts--cur-gap (max 0 (- ps/conflicts--cur-gap 5)))
         (ps/conflicts--buffer-render))))
    (insert (format "  %d min  " ps/conflicts--cur-gap))
    (ps/conflicts--insert-button
     " + "
     (lambda (_b)
       (with-current-buffer buf
         (setq ps/conflicts--cur-gap (+ ps/conflicts--cur-gap 5))
         (ps/conflicts--buffer-render))))
    (insert "    Include past: ")
    (ps/conflicts--insert-button
     (if ps/conflicts--cur-include-past " on  " " off ")
     (lambda (_b)
       (with-current-buffer buf
         (setq ps/conflicts--cur-include-past
               (not ps/conflicts--cur-include-past))
         (ps/conflicts--buffer-render)))
     ps/conflicts--cur-include-past)
    (insert "    ")
    (ps/conflicts--insert-button
     " ↺ refresh "
     (lambda (_b) (with-current-buffer buf (ps/conflicts--buffer-render))))
    (insert "\n")
    (insert (make-string 78 ?─))
    (insert "\n")))

(defun ps/conflicts--insert-event-button (event)
  "Insert EVENT as a clickable button that visits its location in the org file."
  (let ((label (ps/conflicts--format-event-label event))
        (file  (ps/conflict--event-file event))
        (line  (ps/conflict--event-line event)))
    (insert-button label
                   'face        'default
                   'mouse-face  'highlight
                   'action      (lambda (_b)
                                  (find-file-other-window file)
                                  (goto-char (point-min))
                                  (forward-line (1- line)))
                   'follow-link t)))

(defun ps/conflicts--buffer-render ()
  "Redraw the *Org Conflicts* buffer with current session settings."
  (let ((buf (current-buffer))
        (inhibit-read-only t))
    (erase-buffer)
    (ps/conflicts--insert-header buf)
    (let* ((result   (ps/conflicts--find-all
                      ps/conflicts--cur-directory
                      ps/conflicts--cur-gap
                      ps/conflicts--cur-include-past))
           (overlaps (plist-get result :overlaps))
           (gaps     (plist-get result :gaps)))
      (if (and (null overlaps) (null gaps))
          (insert "No conflicts found.\n")
        (let* ((all-days
                (sort (cl-remove-duplicates
                       (append (mapcar #'car overlaps) (mapcar #'car gaps))
                       :test #'equal)
                      #'ps/conflicts--date<)))
          (dolist (day all-days)
            (insert (format "\n=== %s ===\n" (ps/conflicts--format-date day)))
            (let ((day-overlaps (cdr (assoc day overlaps)))
                  (day-gaps     (cdr (assoc day gaps))))
              (when day-overlaps
                (insert "\nOverlapping tasks:\n")
                (dolist (pair day-overlaps)
                  (insert "  - ")
                  (ps/conflicts--insert-event-button (nth 0 pair))
                  (insert "\n    ")
                  (ps/conflicts--insert-event-button (nth 1 pair))
                  (insert "\n")))
              (when day-gaps
                (insert (format "\nTasks with gap < %d min:\n"
                                ps/conflicts--cur-gap))
                (dolist (triple day-gaps)
                  (insert (format "  - (%d min)\n" (nth 2 triple)))
                  (insert "    ")
                  (ps/conflicts--insert-event-button (nth 0 triple))
                  (insert "\n    ")
                  (ps/conflicts--insert-event-button (nth 1 triple))
                  (insert "\n"))))))))
    (goto-char (point-min))))

(defun ps/show-conflicts ()
  "Show scheduling conflicts from the org notes directory in a dedicated buffer.
Use +/- to adjust the gap threshold, p to toggle past events, g/r to refresh.
Press RET or click on any task to jump to it in its org file."
  (interactive)
  (let ((buf (get-buffer-create "*Org Conflicts*")))
    (with-current-buffer buf
      (unless (eq major-mode 'ps-conflicts-mode)
        (ps-conflicts-mode))
      (unless ps/conflicts--cur-directory
        (setq ps/conflicts--cur-gap          ps/conflicts-gap-minutes
              ps/conflicts--cur-include-past ps/conflicts-include-past))
      (setq ps/conflicts--cur-directory
            (ps/org-files-root))
      (ps/conflicts--buffer-render))
    (ps/window-show-here buf)))

;;; Agenda integration (async via idle timer)

;; Let-bound by the agenda custom commands to the view kind (`agenda' / `calendar'
;; / nil); active during `org-agenda-finalize-hook'.  Used to run the conflict
;; check for the Agenda only.
(defvar ps/agenda-layout-view-kind)
;; Buffer-local agenda mode-line state, defined in `ps-mode-line'; set here so
;; the conflict count surfaces in the mode line instead of the buffer text.
(defvar ps/mode-line--agenda-conflict-count)

(defvar ps/conflicts--agenda-timer nil
  "Pending idle timer for the agenda conflict check.")

(defun ps/conflicts--agenda-check ()
  "Recompute the scheduling-conflict count for the agenda mode line."
  (when (and (boundp 'my-org-base-directory)
             (buffer-live-p (get-buffer org-agenda-buffer-name)))
    (let* ((directory (ps/org-files-root))
           (result (ps/conflicts--find-all
                    directory ps/conflicts-gap-minutes ps/conflicts-include-past))
           (n (ps/conflicts--count result)))
      (with-current-buffer (get-buffer org-agenda-buffer-name)
        (setq-local ps/mode-line--agenda-conflict-count n)
        (force-mode-line-update)))))

(defun ps/conflicts--agenda-schedule-check ()
  "Schedule an idle-time conflict check after the agenda finishes rendering.
Cancels any previously pending check first.  Runs only for the Agenda view —
conflicts belong there; the Calendar and Tasks views skip this full re-scan of
every file (it is span-independent work and briefly freezes Emacs)."
  (when (eq (and (boundp 'ps/agenda-layout-view-kind) ps/agenda-layout-view-kind)
            'agenda)
    (when ps/conflicts--agenda-timer
      (cancel-timer ps/conflicts--agenda-timer))
    (setq ps/conflicts--agenda-timer
          (run-with-idle-timer 1.0 nil #'ps/conflicts--agenda-check))))

(provide 'ps-conflicts)
;;; ps-conflicts.el ends here
