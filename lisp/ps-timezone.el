;;; ps-timezone.el --- Org timestamp timezone shifting -*- lexical-binding: t; -*-

(require 'cl-lib)

;;; Faces

(defface ps/timezone--button-face
  '((t :inherit custom-button))
  "Face for buttons in the *Timezone Shift* buffer.")

;;; Constants

(defconst ps/timezone--timestamp-re
  (concat "<\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)"   ; group 1: date
          "\\(?: [A-Za-z]+\\)?"                                ; weekday (non-capturing)
          "\\(?: \\([0-9]\\{2\\}:[0-9]\\{2\\}\\)"             ; group 2: start time
          "\\(?:-\\([0-9]\\{2\\}:[0-9]\\{2\\}\\)\\)?"         ; group 3: end time
          "\\( [^>]+\\)?\\)?"                                  ; group 4: extra (repeater etc.)
          ">")
  "Regexp matching org timestamps.
Groups: 1=date (YYYY-MM-DD), 2=start time (HH:MM), 3=end time (HH:MM),
4=extra content (e.g. \" +1w\"). Groups 2-4 optional; only timestamps
with group 2 are shifted.")

(defconst ps/timezone--zone-base "/usr/share/zoneinfo/"
  "Root directory of the IANA timezone database.")

(defconst ps/timezone--zone-skip
  '("posix" "right" "SystemV" "leapseconds" "leap-seconds.list"
    "README" "Theory" "+VERSION" "SECURITY" "posixrules")
  "Entries in /usr/share/zoneinfo/ that are not canonical zone names.")

;;; Zone list

(defvar ps/timezone--zones-cache nil
  "Cached list of IANA timezone names, populated on first use.")

(defun ps/timezone--list-zones ()
  "Return a sorted list of IANA timezone names."
  (or ps/timezone--zones-cache
      (setq ps/timezone--zones-cache (ps/timezone--compute-zones))))

(defun ps/timezone--compute-zones ()
  "Enumerate IANA zone names from `ps/timezone--zone-base'."
  (let ((base ps/timezone--zone-base)
        (result '()))
    (when (file-directory-p base)
      (let ((base-len (length base)))
        (dolist (entry (directory-files base nil "^[A-Za-z]"))
          (let ((path (expand-file-name entry base)))
            (cond
             ((member entry ps/timezone--zone-skip) nil)
             ;; Region directory (America/, Europe/, US/, Etc/, ...)
             ((file-directory-p path)
              (dolist (child (directory-files-recursively path "^[^.+]"))
                (let ((rel (substring child base-len)))
                  (unless (string-match "\\." rel)
                    (push rel result)))))
             ;; Root-level zone file (UTC, GMT, CET, ...)
             ((not (string-match "\\." entry))
              (push entry result)))))))
    (sort result #'string<)))

;;; File scanning

(defun ps/timezone--read-file-timezone (path)
  "Return the #+TIMEZONE value from the org file at PATH, or nil."
  (with-temp-buffer
    (insert-file-contents path nil 0 4096)
    (goto-char (point-min))
    (when (re-search-forward "^#\\+TIMEZONE:[ \t]+\\(\\S-+\\)" nil t)
      (match-string-no-properties 1))))

(defun ps/timezone--find-files (directory)
  "Return alist of (path . tz) for org files in DIRECTORY that have #+TIMEZONE."
  (let ((result '()))
    (dolist (file (directory-files-recursively directory "\\.org$"))
      (let ((tz (ps/timezone--read-file-timezone file)))
        (when tz
          (push (cons file tz) result))))
    (nreverse result)))

;;; Timezone utilities

(defun ps/timezone--utc-offset-label (tz)
  "Return a UTC offset string like \"UTC+1\" or \"UTC-5\" for TZ."
  (let* ((offset-str (format-time-string "%z" (current-time) tz))
         ;; "%z" returns e.g. "+0100" or "-0500"
         (sign  (substring offset-str 0 1))
         (hours (string-to-number (substring offset-str 1 3)))
         (mins  (string-to-number (substring offset-str 3 5))))
    (if (= mins 0)
        (format "UTC%s%d" (if (string= sign "+") "+" "-") hours)
      (format "UTC%s%d:%02d" (if (string= sign "+") "+" "-") hours mins))))

(defun ps/timezone--most-common (list)
  "Return the most frequently occurring element in LIST, or nil."
  (when list
    (let ((counts (make-hash-table :test #'equal))
          (best nil) (best-count 0))
      (dolist (x list) (puthash x (1+ (gethash x counts 0)) counts))
      (maphash (lambda (k v)
                 (when (> v best-count) (setq best k best-count v)))
               counts)
      best)))

;;; Timestamp conversion

(defun ps/timezone--parse-date-parts (date-str)
  "Return (year month day) from YYYY-MM-DD string DATE-STR."
  (let ((parts (split-string date-str "-")))
    (list (string-to-number (nth 0 parts))
          (string-to-number (nth 1 parts))
          (string-to-number (nth 2 parts)))))

(defun ps/timezone--parse-time-parts (time-str)
  "Return (hour min) from HH:MM string TIME-STR."
  (let ((parts (split-string time-str ":")))
    (list (string-to-number (car parts))
          (string-to-number (cadr parts)))))

(defun ps/timezone--convert-time (year month day hour min from-tz to-tz)
  "Convert YEAR MONTH DAY HOUR MIN from FROM-TZ to TO-TZ.
Returns (new-year new-month new-day new-hour new-min)."
  (let* ((encoded (encode-time 0 min hour day month year from-tz))
         (decoded (decode-time encoded to-tz)))
    ;; decode-time returns (sec min hour day month year dow dst utcoff)
    (list (nth 5 decoded)
          (nth 4 decoded)
          (nth 3 decoded)
          (nth 2 decoded)
          (nth 1 decoded))))

(defun ps/timezone--shift-timestamp (date-str start-str end-str extra from-tz to-tz)
  "Shift a single org timestamp from FROM-TZ to TO-TZ.
DATE-STR is YYYY-MM-DD, START-STR is HH:MM, END-STR is HH:MM or nil,
EXTRA is any trailing content inside the timestamp (e.g. \" +1w\") or nil."
  (let* ((dp    (ps/timezone--parse-date-parts date-str))
         (sp    (ps/timezone--parse-time-parts start-str))
         (ns    (ps/timezone--convert-time
                 (nth 0 dp) (nth 1 dp) (nth 2 dp)
                 (nth 0 sp) (nth 1 sp)
                 from-tz to-tz))
         (ny (nth 0 ns)) (nm (nth 1 ns)) (nd (nth 2 ns))
         (nh (nth 3 ns)) (nmin (nth 4 ns))
         (encoded    (encode-time 0 nmin nh nd nm ny to-tz))
         (new-date   (format "%04d-%02d-%02d" ny nm nd))
         (new-wday   (format-time-string "%a" encoded to-tz))
         (new-start  (format "%02d:%02d" nh nmin))
         (extra-str  (or extra "")))
    (if end-str
        (let* ((ep   (ps/timezone--parse-time-parts end-str))
               (ne   (ps/timezone--convert-time
                      (nth 0 dp) (nth 1 dp) (nth 2 dp)
                      (nth 0 ep) (nth 1 ep)
                      from-tz to-tz))
               (new-end (format "%02d:%02d" (nth 3 ne) (nth 4 ne))))
          (format "<%s %s %s-%s%s>" new-date new-wday new-start new-end extra-str))
      (format "<%s %s %s%s>" new-date new-wday new-start extra-str))))

(defun ps/timezone--count-timed-timestamps (content)
  "Count timed org timestamps in CONTENT string."
  (let ((count 0)
        (pos 0))
    (while (string-match ps/timezone--timestamp-re content pos)
      (setq pos (match-end 0))
      (when (match-string 2 content)
        (cl-incf count)))
    count))

(defun ps/timezone--shift-content (content from-tz to-tz)
  "Return CONTENT with all timed timestamps shifted from FROM-TZ to TO-TZ.
Also update the #+TIMEZONE: header."
  (with-temp-buffer
    (insert content)
    ;; Shift timestamps
    (goto-char (point-min))
    (while (re-search-forward ps/timezone--timestamp-re nil t)
      (let* ((date-str  (match-string-no-properties 1))
             (start-str (match-string-no-properties 2))
             (end-str   (match-string-no-properties 3))
             (extra     (match-string-no-properties 4))
             (beg       (match-beginning 0))
             (end       (match-end 0)))
        (when start-str
          (let ((new-ts (ps/timezone--shift-timestamp
                         date-str start-str end-str extra from-tz to-tz)))
            (goto-char beg)
            (delete-region beg end)
            (insert new-ts)))))
    ;; Update #+TIMEZONE: header
    (goto-char (point-min))
    (when (re-search-forward "^#\\+TIMEZONE:.*$" nil t)
      (replace-match (concat "#+TIMEZONE: " to-tz) t t))
    (buffer-string)))

(defun ps/timezone--apply-all (file-tz-alist to-tz)
  "Shift timestamps in all files in FILE-TZ-ALIST to TO-TZ.
Returns the number of files written."
  (let ((count 0))
    (dolist (entry file-tz-alist)
      (let* ((path     (car entry))
             (from-tz  (cdr entry))
             (content  (with-temp-buffer
                         (insert-file-contents path)
                         (buffer-string)))
             (new-content (ps/timezone--shift-content content from-tz to-tz)))
        (unless (string= content new-content)
          (with-temp-file path
            (insert new-content))
          (cl-incf count))))
    count))

;;; Interactive buffer

(defvar-local ps/timezone--cur-to-tz nil
  "Target timezone for the current *Timezone Shift* session.")
(defvar-local ps/timezone--cur-files nil
  "Alist of (path . from-tz) for the current session.")

(define-derived-mode ps-timezone-mode special-mode "Timezone Shift"
  "Major mode for the *Timezone Shift* preview buffer.
\\{ps-timezone-mode-map}")

(let ((map ps-timezone-mode-map))
  (define-key map (kbd "a")   #'ps/timezone--buffer-apply)
  (define-key map (kbd "RET") #'ps/timezone--buffer-apply)
  (define-key map (kbd "q")   #'quit-window))

(defun ps/timezone--buffer-apply ()
  "Apply the timezone shift shown in the preview buffer."
  (interactive)
  (ps/timezone--do-apply))

(defun ps/timezone--insert-button (label action)
  "Insert a styled button with LABEL that calls ACTION when pressed."
  (insert-button label
                 'face       'ps/timezone--button-face
                 'mouse-face 'custom-button-pressed-unraised
                 'action     action
                 'follow-link t))

(defun ps/timezone--do-apply ()
  "Apply the shift and replace buffer contents with a completion summary."
  (let* ((n (ps/timezone--apply-all ps/timezone--cur-files ps/timezone--cur-to-tz))
         (inhibit-read-only t))
    (erase-buffer)
    (insert (format "Done: %d file%s updated.\n" n (if (= n 1) "" "s")))
    (goto-char (point-min)))
  (message "Timezone shift complete."))

(defun ps/timezone--render ()
  "Redraw the *Timezone Shift* preview buffer."
  (let ((buf (current-buffer))
        (inhibit-read-only t))
    (erase-buffer)
    (let* ((from-tzs (mapcar #'cdr ps/timezone--cur-files))
           (common-from (ps/timezone--most-common from-tzs))
           (from-label (if common-from
                           (format "%s (%s)" common-from
                                   (ps/timezone--utc-offset-label common-from))
                         "(mixed)"))
           (to-label (format "%s (%s)" ps/timezone--cur-to-tz
                             (ps/timezone--utc-offset-label ps/timezone--cur-to-tz)))
           (base-dir (if (boundp 'my-org-base-directory)
                         my-org-base-directory
                       "")))
      (insert "Shift Org Timezones\n\n")
      (insert (format "From: %s  →  To: %s\n\n" from-label to-label))
      (insert (format "Files to update (%d):\n" (length ps/timezone--cur-files)))
      (dolist (entry ps/timezone--cur-files)
        (let* ((path    (car entry))
               (tz      (cdr entry))
               (rel     (if (and (> (length base-dir) 0)
                                 (string-prefix-p base-dir path))
                            (substring path (length base-dir))
                          (file-name-nondirectory path)))
               (content (with-temp-buffer
                          (insert-file-contents path)
                          (buffer-string)))
               (n-ts   (ps/timezone--count-timed-timestamps content)))
          (insert (format "  %-42s %-20s (%d timestamp%s)\n"
                          rel tz n-ts (if (= n-ts 1) "" "s")))))
      (insert "\n")
      (insert (make-string 78 ?─))
      (insert "\n ")
      (ps/timezone--insert-button
       " Apply "
       (lambda (_b) (with-current-buffer buf (ps/timezone--do-apply))))
      (insert "   ")
      (ps/timezone--insert-button
       " Cancel "
       (lambda (_b) (quit-window)))
      (insert "\n"))
    (goto-char (point-min))))

;;; Public entry point

(defun ps/shift-timezone ()
  "Shift timestamps in all org files with #+TIMEZONE: to a new timezone.
Scans `my-org-base-directory' for org files with #+TIMEZONE: headers,
prompts for the target timezone, then shows a preview buffer.
Press a or RET (or click Apply) to apply; q to cancel."
  (interactive)
  (unless (boundp 'my-org-base-directory)
    (user-error "my-org-base-directory is not set"))
  (let ((files (ps/timezone--find-files my-org-base-directory)))
    (if (null files)
        (message "No org files with #+TIMEZONE: found in %s" my-org-base-directory)
      (let* ((from-tzs    (mapcar #'cdr files))
             (common-from (ps/timezone--most-common from-tzs))
             (prompt      (if common-from
                              (format "Shift from %s to timezone: " common-from)
                            "Shift to timezone: "))
             (zones  (ps/timezone--list-zones))
             (to-tz  (completing-read prompt (or zones '()) nil nil nil nil common-from)))
        (let ((buf (get-buffer-create "*Timezone Shift*")))
          (with-current-buffer buf
            (unless (eq major-mode 'ps-timezone-mode)
              (ps-timezone-mode))
            (setq ps/timezone--cur-to-tz  to-tz
                  ps/timezone--cur-files  files)
            (ps/timezone--render))
          (display-buffer buf))))))

(provide 'ps-timezone)
;;; ps-timezone.el ends here
