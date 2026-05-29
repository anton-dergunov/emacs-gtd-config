;;; test-ps-timezone.el --- ERT tests for ps-timezone -*- lexical-binding: t; -*-

(require 'ert)
(add-to-list 'load-path "lisp")
(require 'ps-timezone)

;;; Test helpers

(defmacro ps/timezone-test--with-org-dir (files &rest body)
  "Evaluate BODY with a temp dir containing FILES.
FILES is an alist of (filename . content). Binds `dir'. Cleans up afterward."
  (declare (indent 1))
  `(let ((dir (make-temp-file "ps-timezone-test-" t)))
     (unwind-protect
         (progn
           (dolist (entry ,files)
             (with-temp-file (expand-file-name (car entry) dir)
               (insert (cdr entry))))
           ,@body)
       (delete-directory dir t))))

;;; Fixtures

(defconst ps/timezone-test--london-org
  "#+TITLE: Test
#+TIMEZONE: Europe/London

* Morning meeting
SCHEDULED: <2026-01-15 Thu 10:00-11:00>

* All-day event
SCHEDULED: <2026-01-15 Thu>

* Point appointment
SCHEDULED: <2026-01-15 Thu 14:00>

* Repeating task
SCHEDULED: <2026-01-15 Thu 09:00-09:30 +1w>
"
  "Sample org content with Europe/London timezone.
January dates used to avoid DST ambiguity (London = UTC+0 in winter).")

(defconst ps/timezone-test--ny-org
  "#+TITLE: New York
#+TIMEZONE: America/New_York

* Meeting
SCHEDULED: <2026-01-15 Thu 10:00-11:00>
"
  "Org file with America/New_York timezone.")

(defconst ps/timezone-test--no-tz-org
  "#+TITLE: No timezone

* Task
SCHEDULED: <2026-01-15 Thu 10:00-11:00>
"
  "Org file with no #+TIMEZONE: header.")

(defconst ps/timezone-test--date-only-org
  "#+TIMEZONE: UTC

* Deadline
DEADLINE: <2026-01-15 Thu>
* Task
SCHEDULED: <2026-01-20 Tue>
"
  "Org file with only date-only timestamps (no times to shift).")

;;; -------------------------------------------------------
;;; list-zones
;;; -------------------------------------------------------

(ert-deftest ps/timezone--list-zones-non-empty ()
  "list-zones returns a non-empty list when zoneinfo is present."
  (skip-unless (file-directory-p "/usr/share/zoneinfo/"))
  (let ((zones (ps/timezone--list-zones)))
    (should (> (length zones) 0))))

(ert-deftest ps/timezone--list-zones-contains-known-zones ()
  "list-zones contains well-known IANA zone names."
  (skip-unless (file-directory-p "/usr/share/zoneinfo/"))
  (let ((zones (ps/timezone--list-zones)))
    (should (member "America/New_York" zones))
    (should (member "Europe/London" zones))
    (should (member "Asia/Tokyo" zones))))

(ert-deftest ps/timezone--list-zones-no-posix-right ()
  "list-zones excludes posix/ and right/ duplicates."
  (skip-unless (file-directory-p "/usr/share/zoneinfo/"))
  (let ((zones (ps/timezone--list-zones)))
    (should-not (cl-some (lambda (z) (string-prefix-p "posix/" z)) zones))
    (should-not (cl-some (lambda (z) (string-prefix-p "right/" z)) zones))))

(ert-deftest ps/timezone--list-zones-sorted ()
  "list-zones returns zones in alphabetical order."
  (skip-unless (file-directory-p "/usr/share/zoneinfo/"))
  (let ((zones (ps/timezone--list-zones)))
    (should (equal zones (sort (copy-sequence zones) #'string<)))))

;;; -------------------------------------------------------
;;; read-file-timezone
;;; -------------------------------------------------------

(ert-deftest ps/timezone--read-file-timezone-present ()
  "read-file-timezone extracts the IANA zone name from #+TIMEZONE:."
  (ps/timezone-test--with-org-dir
   `(("test.org" . ,ps/timezone-test--london-org))
   (let ((tz (ps/timezone--read-file-timezone (expand-file-name "test.org" dir))))
     (should (equal tz "Europe/London")))))

(ert-deftest ps/timezone--read-file-timezone-missing ()
  "read-file-timezone returns nil when #+TIMEZONE: is absent."
  (ps/timezone-test--with-org-dir
   `(("test.org" . ,ps/timezone-test--no-tz-org))
   (let ((tz (ps/timezone--read-file-timezone (expand-file-name "test.org" dir))))
     (should (null tz)))))

(ert-deftest ps/timezone--read-file-timezone-new-york ()
  "read-file-timezone returns the correct zone for America/New_York."
  (ps/timezone-test--with-org-dir
   `(("test.org" . ,ps/timezone-test--ny-org))
   (let ((tz (ps/timezone--read-file-timezone (expand-file-name "test.org" dir))))
     (should (equal tz "America/New_York")))))

;;; -------------------------------------------------------
;;; find-files
;;; -------------------------------------------------------

(ert-deftest ps/timezone--find-files-finds-tz-files ()
  "find-files returns an alist entry for files with #+TIMEZONE:."
  (ps/timezone-test--with-org-dir
   `(("a.org" . ,ps/timezone-test--london-org)
     ("b.org" . ,ps/timezone-test--no-tz-org))
   (let ((result (ps/timezone--find-files dir)))
     (should (= (length result) 1))
     (should (equal (cdr (car result)) "Europe/London")))))

(ert-deftest ps/timezone--find-files-skips-no-tz ()
  "find-files omits files without #+TIMEZONE:."
  (ps/timezone-test--with-org-dir
   `(("a.org" . ,ps/timezone-test--no-tz-org)
     ("b.org" . ,ps/timezone-test--no-tz-org))
   (let ((result (ps/timezone--find-files dir)))
     (should (null result)))))

(ert-deftest ps/timezone--find-files-multiple ()
  "find-files returns all files with #+TIMEZONE: headers."
  (ps/timezone-test--with-org-dir
   `(("a.org" . ,ps/timezone-test--london-org)
     ("b.org" . ,ps/timezone-test--ny-org)
     ("c.org" . ,ps/timezone-test--no-tz-org))
   (let ((result (ps/timezone--find-files dir)))
     (should (= (length result) 2)))))

;;; -------------------------------------------------------
;;; utc-offset-label
;;; -------------------------------------------------------

(ert-deftest ps/timezone--utc-offset-label-utc ()
  "UTC zone gives UTC+0."
  (should (equal (ps/timezone--utc-offset-label "UTC") "UTC+0")))

(ert-deftest ps/timezone--utc-offset-label-format ()
  "utc-offset-label returns a string starting with UTC."
  (let ((label (ps/timezone--utc-offset-label "America/New_York")))
    (should (string-prefix-p "UTC" label))))

;;; -------------------------------------------------------
;;; convert-time
;;; -------------------------------------------------------

;; All tests use January dates to avoid DST ambiguity.
;; America/New_York in January = UTC-5 (EST).
;; Europe/London in January = UTC+0 (GMT).
;; UTC always = UTC+0.

(ert-deftest ps/timezone--convert-time-utc-to-utc ()
  "Converting from UTC to UTC is a no-op."
  (let ((result (ps/timezone--convert-time 2026 1 15 10 0 "UTC" "UTC")))
    (should (equal result '(2026 1 15 10 0)))))

(ert-deftest ps/timezone--convert-time-utc-to-ny ()
  "UTC 10:00 → America/New_York (UTC-5 in January) = 05:00, same day."
  (let ((result (ps/timezone--convert-time 2026 1 15 10 0 "UTC" "America/New_York")))
    (should (= (nth 3 result) 5))
    (should (= (nth 4 result) 0))
    (should (= (nth 2 result) 15))))

(ert-deftest ps/timezone--convert-time-ny-to-utc ()
  "America/New_York 10:00 → UTC = 15:00 in January."
  (let ((result (ps/timezone--convert-time 2026 1 15 10 0 "America/New_York" "UTC")))
    (should (= (nth 3 result) 15))
    (should (= (nth 4 result) 0))
    (should (= (nth 2 result) 15))))

(ert-deftest ps/timezone--convert-time-date-rollover ()
  "Late-night conversion can roll date forward."
  ;; America/New_York 22:00 → UTC = next day 03:00
  (let ((result (ps/timezone--convert-time 2026 1 15 22 0 "America/New_York" "UTC")))
    (should (= (nth 3 result) 3))
    (should (= (nth 4 result) 0))
    (should (= (nth 2 result) 16))     ; Jan 16
    (should (= (nth 1 result) 1))))    ; January

(ert-deftest ps/timezone--convert-time-preserves-minutes ()
  "Minutes are correctly converted."
  ;; UTC 10:30 → America/New_York = 05:30
  (let ((result (ps/timezone--convert-time 2026 1 15 10 30 "UTC" "America/New_York")))
    (should (= (nth 3 result) 5))
    (should (= (nth 4 result) 30))))

;;; -------------------------------------------------------
;;; shift-timestamp
;;; -------------------------------------------------------

(ert-deftest ps/timezone--shift-timestamp-start-and-end ()
  "Timestamp with start and end times is shifted correctly."
  ;; UTC 10:00-11:00 → America/New_York = 05:00-06:00
  (let ((result (ps/timezone--shift-timestamp
                 "2026-01-15" "10:00" "11:00" nil "UTC" "America/New_York")))
    (should (string-match "<2026-01-15 Thu 05:00-06:00>" result))))

(ert-deftest ps/timezone--shift-timestamp-start-only ()
  "Timestamp with start time only (no end) is shifted correctly."
  (let ((result (ps/timezone--shift-timestamp
                 "2026-01-15" "10:00" nil nil "UTC" "America/New_York")))
    (should (string-match "<2026-01-15 Thu 05:00>" result))
    (should-not (string-match "-" (substring result 16)))))

(ert-deftest ps/timezone--shift-timestamp-weekday-updated ()
  "Weekday is recomputed after date rollover."
  ;; NY 22:00 → UTC rolls to next day (Jan 16 = Friday)
  (let ((result (ps/timezone--shift-timestamp
                 "2026-01-15" "22:00" nil nil "America/New_York" "UTC")))
    (should (string-match "2026-01-16 Fri" result))))

(ert-deftest ps/timezone--shift-timestamp-extra-preserved ()
  "Repeater and other extra content is preserved."
  (let ((result (ps/timezone--shift-timestamp
                 "2026-01-15" "09:00" "09:30" " +1w" "UTC" "America/New_York")))
    (should (string-match " \\+1w>" result))))

;;; -------------------------------------------------------
;;; DST handling (summer dates)
;;; -------------------------------------------------------

(ert-deftest ps/timezone--convert-time-dst-summer ()
  "Europe/London in July is BST (UTC+1); London 10:00 → UTC 09:00."
  (skip-unless (file-directory-p "/usr/share/zoneinfo/"))
  (let ((result (ps/timezone--convert-time 2026 7 15 10 0 "Europe/London" "UTC")))
    (should (= (nth 3 result) 9))      ; hour
    (should (= (nth 4 result) 0))      ; minute
    (should (= (nth 2 result) 15))))   ; same day

(ert-deftest ps/timezone--shift-timestamp-dst-summer ()
  "A July Europe/London timestamp shifts to UTC one hour earlier (BST = UTC+1).
Contrast with the January tests where London = UTC+0 (no shift)."
  (skip-unless (file-directory-p "/usr/share/zoneinfo/"))
  (let ((result (ps/timezone--shift-timestamp
                 "2026-07-15" "10:00" "11:00" nil "Europe/London" "UTC")))
    (should (string-match "<2026-07-15 [A-Za-z]+ 09:00-10:00>" result))))

;;; -------------------------------------------------------
;;; count-timed-timestamps
;;; -------------------------------------------------------

(ert-deftest ps/timezone--count-timed-timestamps-mixed ()
  "Counts only timed timestamps, ignores date-only."
  ;; london-org has: 10:00-11:00, 14:00, 09:00-09:30 (timed) + 1 date-only
  (should (= (ps/timezone--count-timed-timestamps ps/timezone-test--london-org) 3)))

(ert-deftest ps/timezone--count-timed-timestamps-date-only ()
  "Returns 0 when all timestamps are date-only."
  (should (= (ps/timezone--count-timed-timestamps ps/timezone-test--date-only-org) 0)))

(ert-deftest ps/timezone--count-timed-timestamps-empty ()
  "Returns 0 for empty string."
  (should (= (ps/timezone--count-timed-timestamps "") 0)))

;;; -------------------------------------------------------
;;; shift-content
;;; -------------------------------------------------------

(ert-deftest ps/timezone--shift-content-updates-timezone-header ()
  "#+TIMEZONE: header is updated to the target zone."
  (let ((result (ps/timezone--shift-content
                 ps/timezone-test--london-org "Europe/London" "UTC")))
    (should (string-match "^#\\+TIMEZONE: UTC$" result))))

(ert-deftest ps/timezone--shift-content-shifts-timestamps ()
  "Timed timestamps are shifted; date-only timestamps are unchanged."
  ;; Europe/London in January = UTC+0, so shifting to UTC = no time change.
  ;; Use America/New_York → UTC to get a visible shift.
  (let* ((content "#+TIMEZONE: America/New_York\n\n* Meeting\nSCHEDULED: <2026-01-15 Thu 10:00-11:00>\n")
         (result (ps/timezone--shift-content content "America/New_York" "UTC")))
    ;; NY 10:00-11:00 → UTC 15:00-16:00
    (should (string-match "<2026-01-15 Thu 15:00-16:00>" result))))

(ert-deftest ps/timezone--shift-content-leaves-date-only-unchanged ()
  "Date-only timestamps are not modified."
  (let ((result (ps/timezone--shift-content
                 ps/timezone-test--date-only-org "UTC" "America/New_York")))
    (should (string-match "<2026-01-15 Thu>" result))
    (should (string-match "<2026-01-20 Tue>" result))))

(ert-deftest ps/timezone--shift-content-repeater-preserved ()
  "Repeater suffix is preserved in shifted timestamps."
  (let* ((content "#+TIMEZONE: UTC\n\n* Task\nSCHEDULED: <2026-01-15 Thu 09:00-09:30 +1w>\n")
         (result (ps/timezone--shift-content content "UTC" "America/New_York")))
    (should (string-match " \\+1w>" result))))

(ert-deftest ps/timezone--shift-content-no-change-when-same-zone ()
  "Shifting from a zone to itself leaves timestamps unchanged."
  (let ((result (ps/timezone--shift-content
                 ps/timezone-test--london-org "Europe/London" "Europe/London")))
    (should (string-match "<2026-01-15 Thu 10:00-11:00>" result))))

;;; -------------------------------------------------------
;;; apply-all
;;; -------------------------------------------------------

(ert-deftest ps/timezone--apply-all-writes-files ()
  "apply-all rewrites files with shifted timestamps."
  (ps/timezone-test--with-org-dir
   `(("test.org" . "#+TIMEZONE: America/New_York\n\n* Meeting\nSCHEDULED: <2026-01-15 Thu 10:00-11:00>\n"))
   (let* ((path  (expand-file-name "test.org" dir))
          (alist (list (cons path "America/New_York")))
          (n     (ps/timezone--apply-all alist "UTC")))
     (should (= n 1))
     (let ((content (with-temp-buffer
                      (insert-file-contents path)
                      (buffer-string))))
       (should (string-match "#\\+TIMEZONE: UTC" content))
       (should (string-match "<2026-01-15 Thu 15:00-16:00>" content))))))

(ert-deftest ps/timezone--apply-all-returns-zero-when-unchanged ()
  "apply-all returns 0 when no file content changes."
  (ps/timezone-test--with-org-dir
   `(("test.org" . "#+TIMEZONE: UTC\n\n* Meeting\nSCHEDULED: <2026-01-15 Thu 10:00-11:00>\n"))
   (let* ((path  (expand-file-name "test.org" dir))
          (alist (list (cons path "UTC")))
          (n     (ps/timezone--apply-all alist "UTC")))
     (should (= n 0)))))

(ert-deftest ps/timezone--apply-all-multiple-files ()
  "apply-all processes all files in the alist."
  (ps/timezone-test--with-org-dir
   `(("a.org" . "#+TIMEZONE: America/New_York\n\n* A\nSCHEDULED: <2026-01-15 Thu 10:00-11:00>\n")
     ("b.org" . "#+TIMEZONE: America/New_York\n\n* B\nSCHEDULED: <2026-01-15 Thu 12:00-13:00>\n"))
   (let* ((path-a (expand-file-name "a.org" dir))
          (path-b (expand-file-name "b.org" dir))
          (alist  (list (cons path-a "America/New_York")
                        (cons path-b "America/New_York")))
          (n      (ps/timezone--apply-all alist "UTC")))
     (should (= n 2)))))

;;; -------------------------------------------------------
;;; most-common
;;; -------------------------------------------------------

(ert-deftest ps/timezone--most-common-single ()
  "most-common returns the sole element."
  (should (equal (ps/timezone--most-common '("a")) "a")))

(ert-deftest ps/timezone--most-common-majority ()
  "most-common returns the most frequent element."
  (should (equal (ps/timezone--most-common '("a" "b" "a" "c" "a")) "a")))

(ert-deftest ps/timezone--most-common-nil-for-empty ()
  "most-common returns nil for an empty list."
  (should (null (ps/timezone--most-common '()))))

;;; -------------------------------------------------------
;;; Interactive buffer
;;; -------------------------------------------------------

(defun ps/timezone-test--make-buffer (files to-tz)
  "Create a *Timezone Shift* buffer with FILES alist and TO-TZ target."
  (let ((buf (get-buffer-create "*Timezone Shift Test*")))
    (with-current-buffer buf
      (unless (eq major-mode 'ps-timezone-mode)
        (ps-timezone-mode))
      (setq ps/timezone--cur-files  files
            ps/timezone--cur-to-tz  to-tz))
    buf))

(ert-deftest ps/timezone--mode-is-special-mode ()
  "ps-timezone-mode derives from special-mode."
  (with-temp-buffer
    (ps-timezone-mode)
    (should (derived-mode-p 'special-mode))))

(ert-deftest ps/timezone--buffer-has-locals ()
  "Buffer-local variables are initialized correctly."
  (ps/timezone-test--with-org-dir
   `(("test.org" . ,ps/timezone-test--london-org))
   (let* ((path (expand-file-name "test.org" dir))
          (buf  (ps/timezone-test--make-buffer
                 (list (cons path "Europe/London")) "UTC")))
     (with-current-buffer buf
       (should (equal ps/timezone--cur-to-tz "UTC"))
       (should (= (length ps/timezone--cur-files) 1)))
     (kill-buffer buf))))

(ert-deftest ps/timezone--buffer-render-has-heading ()
  "Rendered buffer contains the 'Shift Org Timezones' heading."
  (ps/timezone-test--with-org-dir
   `(("test.org" . ,ps/timezone-test--london-org))
   (let* ((path (expand-file-name "test.org" dir))
          (buf  (ps/timezone-test--make-buffer
                 (list (cons path "Europe/London")) "UTC")))
     (with-current-buffer buf
       (ps/timezone--render)
       (goto-char (point-min))
       (should (search-forward "Shift Org Timezones" nil t)))
     (kill-buffer buf))))

(ert-deftest ps/timezone--buffer-render-shows-from-to ()
  "Rendered buffer shows the from and to timezone info."
  (ps/timezone-test--with-org-dir
   `(("test.org" . ,ps/timezone-test--london-org))
   (let* ((path (expand-file-name "test.org" dir))
          (buf  (ps/timezone-test--make-buffer
                 (list (cons path "Europe/London")) "UTC")))
     (with-current-buffer buf
       (ps/timezone--render)
       (let ((content (buffer-string)))
         (should (string-match "Europe/London" content))
         (should (string-match "UTC" content))))
     (kill-buffer buf))))

(ert-deftest ps/timezone--buffer-render-shows-file ()
  "Rendered buffer lists the files to be updated."
  (ps/timezone-test--with-org-dir
   `(("myfile.org" . ,ps/timezone-test--london-org))
   (let* ((path (expand-file-name "myfile.org" dir))
          (buf  (ps/timezone-test--make-buffer
                 (list (cons path "Europe/London")) "UTC")))
     (with-current-buffer buf
       (ps/timezone--render)
       (let ((content (buffer-string)))
         (should (string-match "myfile\\.org" content))))
     (kill-buffer buf))))

(ert-deftest ps/timezone--buffer-render-has-separator ()
  "Rendered buffer has a horizontal separator line."
  (ps/timezone-test--with-org-dir
   `(("test.org" . ,ps/timezone-test--london-org))
   (let* ((path (expand-file-name "test.org" dir))
          (buf  (ps/timezone-test--make-buffer
                 (list (cons path "Europe/London")) "UTC")))
     (with-current-buffer buf
       (ps/timezone--render)
       (should (string-match "─" (buffer-string))))
     (kill-buffer buf))))

(ert-deftest ps/timezone--buffer-render-has-buttons ()
  "Rendered buffer contains Apply and Cancel buttons."
  (ps/timezone-test--with-org-dir
   `(("test.org" . ,ps/timezone-test--london-org))
   (let* ((path (expand-file-name "test.org" dir))
          (buf  (ps/timezone-test--make-buffer
                 (list (cons path "Europe/London")) "UTC")))
     (with-current-buffer buf
       (ps/timezone--render)
       (let ((content (buffer-string)))
         (should (string-match "Apply" content))
         (should (string-match "Cancel" content))))
     (kill-buffer buf))))

(ert-deftest ps/timezone--buffer-keybindings ()
  "a, RET, and q are bound in ps-timezone-mode."
  (with-temp-buffer
    (ps-timezone-mode)
    (should (key-binding (kbd "a")))
    (should (key-binding (kbd "RET")))
    (should (key-binding (kbd "q")))))

(ert-deftest ps/timezone--buffer-apply-updates-file ()
  "Pressing apply (via do-apply) rewrites the file and shows Done message."
  (ps/timezone-test--with-org-dir
   `(("test.org" . "#+TIMEZONE: America/New_York\n\n* Meeting\nSCHEDULED: <2026-01-15 Thu 10:00-11:00>\n"))
   (let* ((path (expand-file-name "test.org" dir))
          (buf  (ps/timezone-test--make-buffer
                 (list (cons path "America/New_York")) "UTC")))
     (with-current-buffer buf
       (ps/timezone--render)
       (ps/timezone--do-apply)
       (should (string-match "Done:" (buffer-string))))
     (let ((content (with-temp-buffer
                      (insert-file-contents path)
                      (buffer-string))))
       (should (string-match "#\\+TIMEZONE: UTC" content))
       (should (string-match "<2026-01-15 Thu 15:00-16:00>" content)))
     (kill-buffer buf))))

(ert-deftest ps/timezone--buffer-timestamp-count-shown ()
  "Rendered buffer shows the count of timed timestamps per file."
  (ps/timezone-test--with-org-dir
   `(("test.org" . ,ps/timezone-test--london-org))
   ;; london-org has 3 timed timestamps
   (let* ((path (expand-file-name "test.org" dir))
          (buf  (ps/timezone-test--make-buffer
                 (list (cons path "Europe/London")) "UTC")))
     (with-current-buffer buf
       (ps/timezone--render)
       (should (string-match "3 timestamps" (buffer-string))))
     (kill-buffer buf))))
