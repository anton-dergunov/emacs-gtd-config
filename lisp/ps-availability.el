;;; ps-availability.el --- Org availability computation -*- lexical-binding: t; -*-

(require 'calendar)
(require 'cl-lib)

;;; Customization

(defcustom ps/org-avail-days 15
  "Number of days to show availability for."
  :type 'integer)

(defcustom ps/org-avail-work-start "10:00"
  "Work day start time in HH:MM format."
  :type 'string)

(defcustom ps/org-avail-work-end "22:00"
  "Work day end time in HH:MM format."
  :type 'string)

(defcustom ps/org-avail-buffer-minutes 15
  "Minutes of buffer time to add around each event."
  :type 'integer)

(defcustom ps/org-avail-min-duration 30
  "Minimum free slot duration in minutes to include in output."
  :type 'integer)

(defcustom ps/org-avail-include-weekends nil
  "Whether to include Saturday and Sunday."
  :type 'boolean)

(defcustom ps/org-avail-time-format "12h"
  "Time format for output: \"12h\" or \"24h\"."
  :type '(choice (const "12h") (const "24h")))

(defcustom ps/org-avail-layout "bullets"
  "Output layout: \"bullets\" (multi-line) or \"inline\" (one line per day)."
  :type '(choice (const "bullets") (const "inline")))

;;; Internal constant

(defconst ps/org-avail--timestamp-re
  (concat "<\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)"
          " [A-Za-z]+"
          " \\([0-9]\\{2\\}:[0-9]\\{2\\}\\)-\\([0-9]\\{2\\}:[0-9]\\{2\\}\\)>")
  "Regexp matching org timestamps with explicit time ranges.
Groups: 1=date (YYYY-MM-DD), 2=start (HH:MM), 3=end (HH:MM).")

;;; Time helpers (all times are integers: minutes since midnight, 0-1439)

(defun ps/org-avail--parse-time (str)
  "Parse HH:MM string STR to minutes since midnight."
  (let* ((parts (split-string str ":"))
         (h (string-to-number (car parts)))
         (m (string-to-number (cadr parts))))
    (+ (* h 60) m)))

;;; Date helpers (dates are (month day year) calendar lists throughout)

(defun ps/org-avail--parse-date (str)
  "Parse YYYY-MM-DD string STR to (month day year) calendar list."
  (let* ((parts (split-string str "-"))
         (year  (string-to-number (nth 0 parts)))
         (month (string-to-number (nth 1 parts)))
         (day   (string-to-number (nth 2 parts))))
    (list month day year)))

(defun ps/org-avail--today ()
  "Return today as (month day year) calendar list."
  (let ((now (decode-time)))
    (list (nth 4 now) (nth 3 now) (nth 5 now))))

(defun ps/org-avail--date-add (date n)
  "Return DATE (month day year) plus N days."
  (calendar-gregorian-from-absolute
   (+ (calendar-absolute-from-gregorian date) n)))

(defun ps/org-avail--weekend-p (date)
  "Return non-nil if DATE (month day year) falls on Saturday or Sunday."
  (let ((dow (calendar-day-of-week date)))  ; 0=Sunday 6=Saturday
    (or (= dow 0) (= dow 6))))

;;; Org file parsing

(defun ps/org-avail--extract-events-from-file (path)
  "Parse timed events from the org file at PATH.
Returns a list of (month day year start-mins end-mins) entries."
  (let ((events '()))
    (with-temp-buffer
      (insert-file-contents path)
      (goto-char (point-min))
      (while (re-search-forward ps/org-avail--timestamp-re nil t)
        ;; Capture all match strings immediately — helper functions like
        ;; split-string clobber global match data if called via match-string.
        (let* ((date-str  (match-string 1))
               (start-str (match-string 2))
               (end-str   (match-string 3))
               (date  (ps/org-avail--parse-date date-str))
               (start (ps/org-avail--parse-time start-str))
               (end   (ps/org-avail--parse-time end-str)))
          (push (list (nth 0 date) (nth 1 date) (nth 2 date) start end)
                events))))
    (nreverse events)))

(defun ps/org-avail--load-events (directory)
  "Load timed events from all .org files in DIRECTORY.
Returns a list of (month day year start-mins end-mins) entries."
  (let ((events '()))
    (dolist (file (directory-files directory t "\\.org$"))
      (setq events (append events
                            (ps/org-avail--extract-events-from-file file))))
    events))

;;; Core interval arithmetic
;;; Slots are (start-mins . end-mins) cons pairs.

(defun ps/org-avail--subtract-intervals (base busy)
  "Subtract BUSY intervals from BASE intervals.
Both BASE and BUSY are lists of (start . end) cons pairs.
Returns free slots as a list of (start . end) cons pairs."
  (let ((result '()))
    (dolist (base-slot base)
      (let ((current (list base-slot)))
        (dolist (b busy)
          (let ((b-start (car b))
                (b-end   (cdr b))
                (new-current '()))
            (dolist (c current)
              (let ((c-start (car c))
                    (c-end   (cdr c)))
                (if (or (<= b-end c-start) (>= b-start c-end))
                    ;; No overlap — keep as-is
                    (push c new-current)
                  ;; Overlap — split around busy block
                  (when (> b-start c-start)
                    (push (cons c-start b-start) new-current))
                  (when (< b-end c-end)
                    (push (cons b-end c-end) new-current)))))
            (setq current (nreverse new-current))))
        (setq result (append result current))))
    result))

(defun ps/org-avail--apply-buffer (slots buffer-minutes)
  "Expand each slot in SLOTS by BUFFER-MINUTES on each side."
  (mapcar (lambda (s)
            (cons (- (car s) buffer-minutes)
                  (+ (cdr s) buffer-minutes)))
          slots))

(defun ps/org-avail--filter-min-duration (slots min-minutes)
  "Return only SLOTS whose duration is >= MIN-MINUTES."
  (cl-remove-if-not
   (lambda (s) (>= (- (cdr s) (car s)) min-minutes))
   slots))

;;; Formatting

(defun ps/org-avail--format-time (mins use-12h)
  "Format minutes-since-midnight MINS as a time string.
USE-12H non-nil produces 12-hour format (e.g. \"3:30pm\")."
  (let ((h (/ mins 60))
        (m (% mins 60)))
    (if use-12h
        (let* ((period (if (< h 12) "am" "pm"))
               (h12   (cond ((= h 0)  12)
                            ((> h 12) (- h 12))
                            (t        h))))
          (format "%d:%02d%s" h12 m period))
      (format "%02d:%02d" h m))))

(defun ps/org-avail--format-slot (start end use-12h)
  "Format a time slot (START . END) in minutes as a string."
  (format "%s-%s"
          (ps/org-avail--format-time start use-12h)
          (ps/org-avail--format-time end   use-12h)))

(defun ps/org-avail--day-name (date)
  "Return the weekday name string for DATE (month day year)."
  (aref calendar-day-name-array (calendar-day-of-week date)))

(defun ps/org-avail--month-name (month)
  "Return the month name string for 1-based MONTH number."
  (aref calendar-month-name-array (1- month)))

(defun ps/org-avail--format-date-header (date)
  "Format DATE (month day year) as \"Weekday, D Month\"."
  (format "%s, %d %s"
          (ps/org-avail--day-name date)
          (nth 1 date)
          (ps/org-avail--month-name (nth 0 date))))

(defun ps/org-avail--format-day-inline (date slots use-12h)
  "Format DATE and SLOTS as a single-line string."
  (let ((header (ps/org-avail--format-date-header date)))
    (if (null slots)
        (format "- %s: (no availability)" header)
      (format "- %s: %s"
              header
              (mapconcat (lambda (s)
                           (ps/org-avail--format-slot (car s) (cdr s) use-12h))
                         slots ", ")))))

(defun ps/org-avail--format-day-bullets (date slots use-12h)
  "Format DATE and SLOTS as a multi-line bullet string."
  (let ((header (ps/org-avail--format-date-header date)))
    (if (null slots)
        (format "%s\n  (no availability)" header)
      (concat header ":\n"
              (mapconcat (lambda (s)
                           (format "  - %s"
                                   (ps/org-avail--format-slot (car s) (cdr s) use-12h)))
                         slots "\n")))))

;;; Public API

(cl-defun ps/org-avail-generate
    (directory
     &key
     (days        ps/org-avail-days)
     (start       ps/org-avail-work-start)
     (end         ps/org-avail-work-end)
     (buffer-mins ps/org-avail-buffer-minutes)
     (min-dur     ps/org-avail-min-duration)
     (weekends    ps/org-avail-include-weekends)
     (time-fmt    ps/org-avail-time-format)
     (layout      ps/org-avail-layout)
     (start-date  nil)
     (raw         nil))
  "Generate an availability string from .org files in DIRECTORY.
Keyword args default to the corresponding ps/org-avail-* defcustom.
RAW non-nil disables buffer expansion and minimum-duration filtering."
  (let* ((use-12h    (string= time-fmt "12h"))
         (work-start (ps/org-avail--parse-time start))
         (work-end   (ps/org-avail--parse-time end))
         (today      (if start-date
                         (ps/org-avail--parse-date start-date)
                       (ps/org-avail--today)))
         (events     (ps/org-avail--load-events directory))
         (lines      '()))
    (dotimes (i days)
      (let ((day (ps/org-avail--date-add today i)))
        (when (or weekends (not (ps/org-avail--weekend-p day)))
          (let* ((month      (nth 0 day))
                 (mday       (nth 1 day))
                 (year       (nth 2 day))
                 (base       (list (cons work-start work-end)))
                 ;; Events matching this calendar day
                 (day-events (cl-remove-if-not
                              (lambda (e)
                                (and (= (nth 0 e) month)
                                     (= (nth 1 e) mday)
                                     (= (nth 2 e) year)))
                              events))
                 (busy       (mapcar (lambda (e) (cons (nth 3 e) (nth 4 e)))
                                     day-events))
                 (busy       (if (and (not raw) (> buffer-mins 0))
                                 (ps/org-avail--apply-buffer busy buffer-mins)
                               busy))
                 (free       (ps/org-avail--subtract-intervals base busy))
                 (free       (if (and (not raw) (> min-dur 0))
                                 (ps/org-avail--filter-min-duration free min-dur)
                               free))
                 (line       (if (string= layout "inline")
                                 (ps/org-avail--format-day-inline  day free use-12h)
                               (ps/org-avail--format-day-bullets day free use-12h))))
            (push line lines)))))
    (mapconcat #'identity (nreverse lines) "\n")))

;;; Interactive buffer mode

(defvar-local ps/org-avail--cur-directory nil
  "Org Plans directory for the current availability buffer session.")
(defvar-local ps/org-avail--cur-days nil)
(defvar-local ps/org-avail--cur-weekends nil)
(defvar-local ps/org-avail--cur-time-fmt nil)
(defvar-local ps/org-avail--cur-layout nil)

(define-derived-mode ps-availability-mode special-mode "Availability"
  "Major mode for the *Org Availability* buffer.
\\{ps-availability-mode-map}")

(let ((map ps-availability-mode-map))
  (define-key map (kbd "g") #'ps/org-avail--buffer-refresh)
  (define-key map (kbd "r") #'ps/org-avail--buffer-refresh)
  (define-key map (kbd "+") #'ps/org-avail--buffer-more-days)
  (define-key map (kbd "-") #'ps/org-avail--buffer-fewer-days)
  (define-key map (kbd "w") #'ps/org-avail--buffer-toggle-weekends)
  (define-key map (kbd "t") #'ps/org-avail--buffer-toggle-time-fmt)
  (define-key map (kbd "l") #'ps/org-avail--buffer-toggle-layout))

(defun ps/org-avail--buffer-refresh ()
  "Re-render the availability buffer with current settings."
  (interactive)
  (ps/org-avail--buffer-render))

(defun ps/org-avail--buffer-more-days ()
  "Increase the day count by 1 and refresh."
  (interactive)
  (setq ps/org-avail--cur-days (1+ ps/org-avail--cur-days))
  (ps/org-avail--buffer-render))

(defun ps/org-avail--buffer-fewer-days ()
  "Decrease the day count by 1 (minimum 1) and refresh."
  (interactive)
  (setq ps/org-avail--cur-days (max 1 (1- ps/org-avail--cur-days)))
  (ps/org-avail--buffer-render))

(defun ps/org-avail--buffer-toggle-weekends ()
  "Toggle weekend inclusion and refresh."
  (interactive)
  (setq ps/org-avail--cur-weekends (not ps/org-avail--cur-weekends))
  (ps/org-avail--buffer-render))

(defun ps/org-avail--buffer-toggle-time-fmt ()
  "Toggle between 12h and 24h time format and refresh."
  (interactive)
  (setq ps/org-avail--cur-time-fmt
        (if (string= ps/org-avail--cur-time-fmt "12h") "24h" "12h"))
  (ps/org-avail--buffer-render))

(defun ps/org-avail--buffer-toggle-layout ()
  "Toggle between bullets and inline layout and refresh."
  (interactive)
  (setq ps/org-avail--cur-layout
        (if (string= ps/org-avail--cur-layout "bullets") "inline" "bullets"))
  (ps/org-avail--buffer-render))

(defun ps/org-avail--insert-header (buf)
  "Insert an interactive header row into BUF using current buffer-local settings."
  (with-current-buffer buf
    (insert "  Days: ")
    (insert-button "[-]"
                   'action (lambda (_b)
                             (with-current-buffer buf
                               (setq ps/org-avail--cur-days
                                     (max 1 (1- ps/org-avail--cur-days)))
                               (ps/org-avail--buffer-render)))
                   'follow-link t)
    (insert (format " %d " ps/org-avail--cur-days))
    (insert-button "[+]"
                   'action (lambda (_b)
                             (with-current-buffer buf
                               (setq ps/org-avail--cur-days
                                     (1+ ps/org-avail--cur-days))
                               (ps/org-avail--buffer-render)))
                   'follow-link t)
    (insert "   Weekends: ")
    (insert-button (if ps/org-avail--cur-weekends "[on] " "[off]")
                   'action (lambda (_b)
                             (with-current-buffer buf
                               (setq ps/org-avail--cur-weekends
                                     (not ps/org-avail--cur-weekends))
                               (ps/org-avail--buffer-render)))
                   'follow-link t)
    (insert "   Time: ")
    (insert-button (format "[%s]" ps/org-avail--cur-time-fmt)
                   'action (lambda (_b)
                             (with-current-buffer buf
                               (setq ps/org-avail--cur-time-fmt
                                     (if (string= ps/org-avail--cur-time-fmt "12h") "24h" "12h"))
                               (ps/org-avail--buffer-render)))
                   'follow-link t)
    (insert "   Layout: ")
    (insert-button (format "[%s]" ps/org-avail--cur-layout)
                   'action (lambda (_b)
                             (with-current-buffer buf
                               (setq ps/org-avail--cur-layout
                                     (if (string= ps/org-avail--cur-layout "bullets") "inline" "bullets"))
                               (ps/org-avail--buffer-render)))
                   'follow-link t)
    (insert "   ")
    (insert-button "[Refresh]"
                   'action (lambda (_b)
                             (with-current-buffer buf
                               (ps/org-avail--buffer-render)))
                   'follow-link t)
    (insert "\n")
    (insert (make-string 78 ?─))
    (insert "\n")))

(defun ps/org-avail--buffer-render ()
  "Redraw the *Org Availability* buffer with current session settings."
  (let ((buf (current-buffer)))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (ps/org-avail--insert-header buf)
      (insert (ps/org-avail-generate
               ps/org-avail--cur-directory
               :days     ps/org-avail--cur-days
               :weekends ps/org-avail--cur-weekends
               :time-fmt ps/org-avail--cur-time-fmt
               :layout   ps/org-avail--cur-layout))
      (insert "\n"))
    (goto-char (point-min))))

(defun ps/org-show-availability ()
  "Show availability from the org Plans directory in a dedicated buffer.
Settings persist for the session; use the header buttons or keybindings
to adjust: +/- days, w weekends, t time format, l layout, g refresh."
  (interactive)
  (let ((buf (get-buffer-create "*Org Availability*")))
    (with-current-buffer buf
      (unless (eq major-mode 'ps-availability-mode)
        (ps-availability-mode))
      ;; Initialize session settings from defcustoms on first call only
      (unless ps/org-avail--cur-directory
        (setq ps/org-avail--cur-days     ps/org-avail-days
              ps/org-avail--cur-weekends ps/org-avail-include-weekends
              ps/org-avail--cur-time-fmt ps/org-avail-time-format
              ps/org-avail--cur-layout   ps/org-avail-layout))
      (setq ps/org-avail--cur-directory
            (concat my-org-base-directory "Plans/"))
      (ps/org-avail--buffer-render))
    (display-buffer buf)))

;;; CLI argument parser

(defun ps/org-avail--parse-cli-args (args)
  "Parse CLI argument list ARGS into a plist.
The first non-option argument is returned as :directory."
  (let ((result '())
        (rest args))
    (while rest
      (let ((arg (car rest)))
        (setq rest (cdr rest))
        (cond
         ((string= arg "--days")
          (setq result (plist-put result :days (string-to-number (pop rest)))))
         ((string= arg "--start")
          (setq result (plist-put result :start (pop rest))))
         ((string= arg "--end")
          (setq result (plist-put result :end (pop rest))))
         ((string= arg "--buffer")
          (setq result (plist-put result :buffer-mins (string-to-number (pop rest)))))
         ((string= arg "--min-duration")
          (setq result (plist-put result :min-dur (string-to-number (pop rest)))))
         ((string= arg "--include-weekends")
          (setq result (plist-put result :weekends t)))
         ((string= arg "--raw")
          (setq result (plist-put result :raw t)))
         ((string= arg "--time-format")
          (setq result (plist-put result :time-fmt (pop rest))))
         ((string= arg "--layout")
          (setq result (plist-put result :layout (pop rest))))
         ((string= arg "--start-date")
          (setq result (plist-put result :start-date (pop rest))))
         ((not (string-prefix-p "--" arg))
          (setq result (plist-put result :directory arg))))))
    result))

;;; CLI entry point (for batch/script usage)

(defun ps/org-avail-cli ()
  "Batch entry point; reads arguments after -- from command-line-args-left."
  (let* ((raw-args (cdr (member "--" command-line-args-left))))
    (setq command-line-args-left nil)
    (let* ((opts      (ps/org-avail--parse-cli-args raw-args))
           (directory (plist-get opts :directory))
           (kw-args   '()))
      (unless directory
        (princ "Usage: org_availability.sh DIRECTORY [--days N] [--start HH:MM]\n")
        (princ "  [--end HH:MM] [--buffer N] [--min-duration N]\n")
        (princ "  [--include-weekends] [--raw] [--time-format 12h|24h]\n")
        (princ "  [--layout bullets|inline] [--start-date YYYY-MM-DD]\n")
        (kill-emacs 1))
      ;; Build keyword arg list, excluding :directory
      (let ((rest opts))
        (while rest
          (let ((k (car rest))
                (v (cadr rest)))
            (unless (eq k :directory)
              (setq kw-args (append kw-args (list k v))))
            (setq rest (cddr rest)))))
      (let ((output (apply #'ps/org-avail-generate directory kw-args)))
        (princ output)
        (terpri)))))

(provide 'ps-availability)
;;; ps-availability.el ends here
