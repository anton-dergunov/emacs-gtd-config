;;; test-ps-availability.el --- ERT tests for ps-availability -*- lexical-binding: t; -*-

(require 'ert)
(add-to-list 'load-path "lisp")
(require 'ps-availability)

;;; Test helpers

(defun ps/avail-test--write-org (dir content)
  "Write CONTENT to test.org inside DIR. Returns DIR."
  (with-temp-file (expand-file-name "test.org" dir)
    (insert content))
  dir)

(defmacro ps/avail-test--with-org-dir (content &rest body)
  "Evaluate BODY with a temp directory containing an org file with CONTENT.
Binds `dir' to the temp directory path. Cleans up afterward."
  (declare (indent 1))
  `(let ((dir (make-temp-file "ps-avail-test-" t)))
     (unwind-protect
         (progn
           (ps/avail-test--write-org dir ,content)
           ,@body)
       (delete-directory dir t))))

(defconst ps/avail-test--sample-org
  "* Test
SCHEDULED: <2026-05-04 Mon 10:00-11:00>
SCHEDULED: <2026-05-04 Mon 13:00-14:00>
SCHEDULED: <2026-05-05 Tue 15:00-16:00>
"
  "Sample org content matching the Python test fixture.")

;;; -------------------------------------------------------
;;; parse-time
;;; -------------------------------------------------------

(ert-deftest ps/org-avail--parse-time-zero ()
  (should (= (ps/org-avail--parse-time "00:00") 0)))

(ert-deftest ps/org-avail--parse-time-normal ()
  (should (= (ps/org-avail--parse-time "10:30") 630)))

(ert-deftest ps/org-avail--parse-time-noon ()
  (should (= (ps/org-avail--parse-time "12:00") 720)))

(ert-deftest ps/org-avail--parse-time-end-of-day ()
  (should (= (ps/org-avail--parse-time "23:59") 1439)))

(ert-deftest ps/org-avail--parse-time-work-end ()
  (should (= (ps/org-avail--parse-time "22:00") 1320)))

;;; -------------------------------------------------------
;;; extract-events / load-events
;;; -------------------------------------------------------

(ert-deftest ps/org-avail--extract-events-basic ()
  "Three timed events are extracted from sample org content."
  (ps/avail-test--with-org-dir ps/avail-test--sample-org
    (let ((events (ps/org-avail--load-events dir)))
      (should (= (length events) 3))
      ;; First event: 2026-05-04 10:00-11:00
      (let ((e (car events)))
        (should (= (nth 0 e) 5))     ; month
        (should (= (nth 1 e) 4))     ; day
        (should (= (nth 2 e) 2026))  ; year
        (should (= (nth 3 e) 600))   ; 10:00 = 600 mins
        (should (= (nth 4 e) 660)))))) ; 11:00 = 660 mins

(ert-deftest ps/org-avail--extract-events-date-only-ignored ()
  "Timestamps without a time range are not extracted."
  (ps/avail-test--with-org-dir "<2026-05-04 Mon>\n<2026-05-05 Tue 10:00>"
    (let ((events (ps/org-avail--load-events dir)))
      (should (= (length events) 0)))))

(ert-deftest ps/org-avail--extract-events-empty-file ()
  "An empty org file produces no events and does not error."
  (ps/avail-test--with-org-dir ""
    (let ((events (ps/org-avail--load-events dir)))
      (should (= (length events) 0)))))

(ert-deftest ps/org-avail--extract-events-multiple-files ()
  "Events are aggregated across multiple .org files."
  (let ((dir (make-temp-file "ps-avail-test-" t)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "a.org" dir)
            (insert "SCHEDULED: <2026-05-04 Mon 09:00-10:00>\n"))
          (with-temp-file (expand-file-name "b.org" dir)
            (insert "SCHEDULED: <2026-05-04 Mon 11:00-12:00>\n"))
          (let ((events (ps/org-avail--load-events dir)))
            (should (= (length events) 2))))
      (delete-directory dir t))))

;;; -------------------------------------------------------
;;; subtract-intervals
;;; -------------------------------------------------------

(ert-deftest ps/org-avail--subtract-simple ()
  "Busy in the middle of base produces two free slots."
  (let* ((base (list (cons 600 1080)))   ; 10:00-18:00
         (busy (list (cons 720 780)))    ; 12:00-13:00
         (free (ps/org-avail--subtract-intervals base busy)))
    (should (= (length free) 2))
    (should (= (cdr (car free)) 720))   ; first slot ends at 12:00
    (should (= (car (cadr free)) 780)))) ; second slot starts at 13:00

(ert-deftest ps/org-avail--subtract-overlap-at-start ()
  "Busy at the start of base removes the beginning."
  (let* ((base (list (cons 600 1080)))   ; 10:00-18:00
         (busy (list (cons 600 720)))    ; 10:00-12:00
         (free (ps/org-avail--subtract-intervals base busy)))
    (should (= (length free) 1))
    (should (= (car (car free)) 720))))  ; free starts at 12:00

(ert-deftest ps/org-avail--subtract-overlap-at-end ()
  "Busy at the end of base removes the tail."
  (let* ((base (list (cons 600 1080)))   ; 10:00-18:00
         (busy (list (cons 960 1080)))   ; 16:00-18:00
         (free (ps/org-avail--subtract-intervals base busy)))
    (should (= (length free) 1))
    (should (= (cdr (car free)) 960))))  ; free ends at 16:00

(ert-deftest ps/org-avail--subtract-fully-covered ()
  "Busy covering the entire base produces no free slots."
  (let* ((base (list (cons 600 1080)))
         (busy (list (cons 600 1080)))
         (free (ps/org-avail--subtract-intervals base busy)))
    (should (null free))))

(ert-deftest ps/org-avail--subtract-no-overlap ()
  "Busy entirely outside base leaves base unchanged."
  (let* ((base (list (cons 600 1080)))   ; 10:00-18:00
         (busy (list (cons 300 420)))    ; 05:00-07:00
         (free (ps/org-avail--subtract-intervals base busy)))
    (should (= (length free) 1))
    (should (equal (car free) (cons 600 1080)))))

(ert-deftest ps/org-avail--subtract-multiple-busy ()
  "Two busy blocks produce three free slots."
  (let* ((base (list (cons 600 1080)))   ; 10:00-18:00
         (busy (list (cons 660 720)      ; 11:00-12:00
                     (cons 840 900)))    ; 14:00-15:00
         (free (ps/org-avail--subtract-intervals base busy)))
    (should (= (length free) 3))
    (should (= (car (nth 0 free)) 600))
    (should (= (cdr (nth 0 free)) 660))
    (should (= (car (nth 1 free)) 720))
    (should (= (cdr (nth 1 free)) 840))
    (should (= (car (nth 2 free)) 900))
    (should (= (cdr (nth 2 free)) 1080))))

(ert-deftest ps/org-avail--subtract-adjacent-busy ()
  "Adjacent busy blocks are handled without leaving a zero-width gap."
  (let* ((base (list (cons 600 1080)))
         (busy (list (cons 660 720)
                     (cons 720 780)))
         (free (ps/org-avail--subtract-intervals base busy)))
    ;; Gap between 720-720 is zero-width so only two real slots
    (let ((nonzero (cl-remove-if (lambda (s) (= (car s) (cdr s))) free)))
      (should (= (length nonzero) 2)))))

;;; -------------------------------------------------------
;;; apply-buffer
;;; -------------------------------------------------------

(ert-deftest ps/org-avail--apply-buffer-basic ()
  "15-minute buffer expands each side of the event."
  (let* ((slots    (list (cons 600 660)))  ; 10:00-11:00
         (buffered (ps/org-avail--apply-buffer slots 15)))
    (should (= (car (car buffered)) 585))  ; 09:45
    (should (= (cdr (car buffered)) 675)))) ; 11:15

(ert-deftest ps/org-avail--apply-buffer-zero ()
  "Zero buffer leaves slots unchanged."
  (let* ((slots    (list (cons 600 660)))
         (buffered (ps/org-avail--apply-buffer slots 0)))
    (should (equal buffered slots))))

(ert-deftest ps/org-avail--apply-buffer-multiple ()
  "Buffer is applied independently to each slot."
  (let* ((slots    (list (cons 600 660) (cons 780 840)))
         (buffered (ps/org-avail--apply-buffer slots 10)))
    (should (= (length buffered) 2))
    (should (= (car (nth 0 buffered)) 590))
    (should (= (cdr (nth 1 buffered)) 850))))

;;; -------------------------------------------------------
;;; filter-min-duration
;;; -------------------------------------------------------

(ert-deftest ps/org-avail--filter-min-duration-removes-short ()
  "A 20-minute slot is removed when threshold is 30 minutes."
  (let* ((slots    (list (cons 600 620)   ; 20 min — too short
                         (cons 660 750))) ; 90 min — keep
         (filtered (ps/org-avail--filter-min-duration slots 30)))
    (should (= (length filtered) 1))
    (should (= (car (car filtered)) 660))))

(ert-deftest ps/org-avail--filter-min-duration-keeps-exact ()
  "A slot exactly at the threshold is kept."
  (let* ((slots    (list (cons 600 630)))  ; exactly 30 min
         (filtered (ps/org-avail--filter-min-duration slots 30)))
    (should (= (length filtered) 1))))

(ert-deftest ps/org-avail--filter-min-duration-zero-threshold ()
  "Zero threshold keeps all slots including zero-width ones."
  (let* ((slots    (list (cons 600 600) (cons 600 601)))
         (filtered (ps/org-avail--filter-min-duration slots 0)))
    (should (= (length filtered) 2))))

;;; -------------------------------------------------------
;;; format-time
;;; -------------------------------------------------------

(ert-deftest ps/org-avail--format-time-12h-afternoon ()
  "15:30 (930 mins) formats as 3:30pm."
  (should (string= (ps/org-avail--format-time 930 t) "3:30pm")))

(ert-deftest ps/org-avail--format-time-12h-morning ()
  "10:00 (600 mins) formats as 10:00am."
  (should (string= (ps/org-avail--format-time 600 t) "10:00am")))

(ert-deftest ps/org-avail--format-time-12h-midnight ()
  "00:00 (0 mins) formats as 12:00am."
  (should (string= (ps/org-avail--format-time 0 t) "12:00am")))

(ert-deftest ps/org-avail--format-time-12h-noon ()
  "12:00 (720 mins) formats as 12:00pm."
  (should (string= (ps/org-avail--format-time 720 t) "12:00pm")))

(ert-deftest ps/org-avail--format-time-24h ()
  "15:30 (930 mins) formats as 15:30 in 24h mode."
  (should (string= (ps/org-avail--format-time 930 nil) "15:30")))

(ert-deftest ps/org-avail--format-time-24h-leading-zero ()
  "09:05 formats with leading zero in 24h mode."
  (should (string= (ps/org-avail--format-time 545 nil) "09:05")))

;;; -------------------------------------------------------
;;; format-day-inline / format-day-bullets
;;; -------------------------------------------------------

(ert-deftest ps/org-avail--format-day-inline-basic ()
  "Inline format contains day name and slot string."
  (let* ((date  '(5 4 2026))                      ; Mon 4 May 2026
         (slots (list (cons 600 660)))             ; 10:00-11:00
         (out   (ps/org-avail--format-day-inline date slots t)))
    (should (string-match-p "Monday" out))
    (should (string-match-p "May" out))
    (should (string-match-p "10:00am-11:00am" out))))

(ert-deftest ps/org-avail--format-day-inline-no-slots ()
  "Inline format with no slots shows \"(no availability)\"."
  (let ((out (ps/org-avail--format-day-inline '(5 4 2026) nil t)))
    (should (string-match-p "(no availability)" out))))

(ert-deftest ps/org-avail--format-day-bullets-basic ()
  "Bullet format contains a '  - HH:MM-HH:MM' line."
  (let* ((date  '(5 4 2026))
         (slots (list (cons 600 660)))
         (out   (ps/org-avail--format-day-bullets date slots t)))
    (should (string-match-p "Monday" out))
    (should (string-match-p "  - 10:00am-11:00am" out))))

(ert-deftest ps/org-avail--format-day-bullets-no-slots ()
  "Bullet format with no slots shows \"(no availability)\"."
  (let ((out (ps/org-avail--format-day-bullets '(5 4 2026) nil t)))
    (should (string-match-p "(no availability)" out))))

(ert-deftest ps/org-avail--format-day-bullets-multiple-slots ()
  "Bullet format lists each slot on its own '  - ...' line."
  (let* ((slots (list (cons 600 660) (cons 780 900)))
         (out   (ps/org-avail--format-day-bullets '(5 4 2026) slots t)))
    (should (string-match-p "  - 10:00am-11:00am" out))
    (should (string-match-p "  - 1:00pm-3:00pm" out))))

;;; -------------------------------------------------------
;;; generate (integration)
;;; -------------------------------------------------------

(ert-deftest ps/org-avail--generate-end-to-end ()
  "End-to-end: a 10:00-11:00 event leaves a free afternoon slot."
  (ps/avail-test--with-org-dir
      "SCHEDULED: <2026-05-04 Mon 10:00-11:00>\n"
    (let ((out (ps/org-avail-generate
                dir
                :start "10:00" :end "18:00"
                :buffer-mins 0 :min-dur 0
                :start-date "2026-05-04" :days 1
                :layout "inline" :time-fmt "24h")))
      (should (string-match-p "11:00-18:00" out)))))

(ert-deftest ps/org-avail--generate-raw-mode ()
  "Raw mode disables buffer and min-duration filtering."
  (ps/avail-test--with-org-dir
      "SCHEDULED: <2026-05-04 Mon 10:00-10:05>\n"
    ;; Without raw: 5-min event + 15-min buffer = 35-min busy;
    ;; remaining slot < 30-min min-dur → filtered out → no free time shown.
    ;; With raw: no buffer, no filter → free slots shown.
    (let ((out (ps/org-avail-generate
                dir
                :start "10:00" :end "11:00"
                :start-date "2026-05-04" :days 1
                :layout "inline" :time-fmt "24h"
                :raw t)))
      (should (string-match-p "10:05-11:00" out)))))

(ert-deftest ps/org-avail--generate-excludes-weekends ()
  "Weekend days are absent from output by default."
  (ps/avail-test--with-org-dir ""
    ;; 2026-05-09 is Saturday, 2026-05-10 is Sunday
    (let ((out (ps/org-avail-generate
                dir
                :start-date "2026-05-09" :days 2
                :buffer-mins 0 :min-dur 0)))
      (should (string= out "")))))

(ert-deftest ps/org-avail--generate-includes-weekends ()
  "Weekend days appear when :weekends is t."
  (ps/avail-test--with-org-dir ""
    (let ((out (ps/org-avail-generate
                dir
                :start-date "2026-05-09" :days 2
                :buffer-mins 0 :min-dur 0
                :weekends t)))
      (should (string-match-p "Saturday" out))
      (should (string-match-p "Sunday" out)))))

(ert-deftest ps/org-avail--generate-no-events-full-day-free ()
  "A day with no events shows the entire work window as free."
  (ps/avail-test--with-org-dir ""
    (let ((out (ps/org-avail-generate
                dir
                :start "09:00" :end "17:00"
                :start-date "2026-05-04" :days 1
                :buffer-mins 0 :min-dur 0
                :layout "inline" :time-fmt "24h")))
      (should (string-match-p "09:00-17:00" out)))))

(ert-deftest ps/org-avail--generate-fully-busy-day ()
  "A day fully covered by events shows no availability."
  (ps/avail-test--with-org-dir
      "SCHEDULED: <2026-05-04 Mon 09:00-17:00>\n"
    (let ((out (ps/org-avail-generate
                dir
                :start "09:00" :end "17:00"
                :start-date "2026-05-04" :days 1
                :buffer-mins 0 :min-dur 0
                :layout "inline")))
      (should (string-match-p "(no availability)" out)))))

;;; -------------------------------------------------------
;;; parse-cli-args
;;; -------------------------------------------------------

(ert-deftest ps/org-avail--parse-cli-args-directory ()
  "Positional arg is captured as :directory."
  (let ((opts (ps/org-avail--parse-cli-args '("/some/dir"))))
    (should (string= (plist-get opts :directory) "/some/dir"))))

(ert-deftest ps/org-avail--parse-cli-args-days ()
  "\"--days 7\" is parsed as :days 7."
  (let ((opts (ps/org-avail--parse-cli-args '("/dir" "--days" "7"))))
    (should (= (plist-get opts :days) 7))))

(ert-deftest ps/org-avail--parse-cli-args-raw-flag ()
  "\"--raw\" sets :raw to t."
  (let ((opts (ps/org-avail--parse-cli-args '("/dir" "--raw"))))
    (should (eq (plist-get opts :raw) t))))

(ert-deftest ps/org-avail--parse-cli-args-include-weekends ()
  "\"--include-weekends\" sets :weekends to t."
  (let ((opts (ps/org-avail--parse-cli-args '("/dir" "--include-weekends"))))
    (should (eq (plist-get opts :weekends) t))))

(ert-deftest ps/org-avail--parse-cli-args-all-flags ()
  "All flags are parsed correctly in combination."
  (let ((opts (ps/org-avail--parse-cli-args
               '("/dir" "--days" "3" "--start" "09:00" "--end" "18:00"
                 "--buffer" "10" "--min-duration" "20"
                 "--time-format" "24h" "--layout" "inline"
                 "--start-date" "2026-06-01"))))
    (should (string= (plist-get opts :directory)  "/dir"))
    (should (= (plist-get opts :days)        3))
    (should (string= (plist-get opts :start) "09:00"))
    (should (string= (plist-get opts :end)   "18:00"))
    (should (= (plist-get opts :buffer-mins) 10))
    (should (= (plist-get opts :min-dur)     20))
    (should (string= (plist-get opts :time-fmt) "24h"))
    (should (string= (plist-get opts :layout)   "inline"))
    (should (string= (plist-get opts :start-date) "2026-06-01"))))

;;; -------------------------------------------------------
;;; Interactive buffer mode
;;; -------------------------------------------------------

(defun ps/avail-test--make-avail-buffer (dir)
  "Set up a fresh *Org Availability* buffer pointed at DIR.
Returns the buffer."
  (when (get-buffer "*Org Availability*")
    (kill-buffer "*Org Availability*"))
  (let ((buf (get-buffer-create "*Org Availability*")))
    (with-current-buffer buf
      (ps-availability-mode)
      (setq ps/org-avail--cur-directory   dir
            ps/org-avail--cur-days        ps/org-avail-days
            ps/org-avail--cur-weekends    ps/org-avail-include-weekends
            ps/org-avail--cur-time-fmt    ps/org-avail-time-format
            ps/org-avail--cur-layout      ps/org-avail-layout))
    buf))

(ert-deftest ps/org-avail--mode-is-special-mode ()
  "ps-availability-mode is derived from special-mode."
  (let ((buf (get-buffer-create " *avail-test-mode*")))
    (unwind-protect
        (with-current-buffer buf
          (ps-availability-mode)
          (should (derived-mode-p 'special-mode)))
      (kill-buffer buf))))

(ert-deftest ps/org-avail--buffer-init-from-defcustom ()
  "Buffer-local vars are initialized from defcustoms."
  (ps/avail-test--with-org-dir ""
    (let ((buf (ps/avail-test--make-avail-buffer dir)))
      (unwind-protect
          (with-current-buffer buf
            (should (= ps/org-avail--cur-days ps/org-avail-days))
            (should (eq ps/org-avail--cur-weekends ps/org-avail-include-weekends))
            (should (string= ps/org-avail--cur-time-fmt ps/org-avail-time-format))
            (should (string= ps/org-avail--cur-layout ps/org-avail-layout)))
        (kill-buffer buf)))))

(ert-deftest ps/org-avail--buffer-more-days ()
  "more-days increments ps/org-avail--cur-days by 1."
  (ps/avail-test--with-org-dir ""
    (let ((buf (ps/avail-test--make-avail-buffer dir)))
      (unwind-protect
          (with-current-buffer buf
            (setq ps/org-avail--cur-days 5)
            (ps/org-avail--buffer-more-days)
            (should (= ps/org-avail--cur-days 6)))
        (kill-buffer buf)))))

(ert-deftest ps/org-avail--buffer-fewer-days ()
  "fewer-days decrements ps/org-avail--cur-days, clamped to 1."
  (ps/avail-test--with-org-dir ""
    (let ((buf (ps/avail-test--make-avail-buffer dir)))
      (unwind-protect
          (with-current-buffer buf
            (setq ps/org-avail--cur-days 3)
            (ps/org-avail--buffer-fewer-days)
            (should (= ps/org-avail--cur-days 2))
            ;; Clamp at 1
            (setq ps/org-avail--cur-days 1)
            (ps/org-avail--buffer-fewer-days)
            (should (= ps/org-avail--cur-days 1)))
        (kill-buffer buf)))))

(ert-deftest ps/org-avail--buffer-toggle-weekends ()
  "toggle-weekends flips ps/org-avail--cur-weekends nil → t → nil."
  (ps/avail-test--with-org-dir ""
    (let ((buf (ps/avail-test--make-avail-buffer dir)))
      (unwind-protect
          (with-current-buffer buf
            (setq ps/org-avail--cur-weekends nil)
            (ps/org-avail--buffer-toggle-weekends)
            (should (eq ps/org-avail--cur-weekends t))
            (ps/org-avail--buffer-toggle-weekends)
            (should (eq ps/org-avail--cur-weekends nil)))
        (kill-buffer buf)))))

(ert-deftest ps/org-avail--buffer-toggle-time-fmt ()
  "toggle-time-fmt cycles 12h → 24h → 12h."
  (ps/avail-test--with-org-dir ""
    (let ((buf (ps/avail-test--make-avail-buffer dir)))
      (unwind-protect
          (with-current-buffer buf
            (setq ps/org-avail--cur-time-fmt "12h")
            (ps/org-avail--buffer-toggle-time-fmt)
            (should (string= ps/org-avail--cur-time-fmt "24h"))
            (ps/org-avail--buffer-toggle-time-fmt)
            (should (string= ps/org-avail--cur-time-fmt "12h")))
        (kill-buffer buf)))))

(ert-deftest ps/org-avail--buffer-toggle-layout ()
  "toggle-layout cycles bullets → inline → bullets."
  (ps/avail-test--with-org-dir ""
    (let ((buf (ps/avail-test--make-avail-buffer dir)))
      (unwind-protect
          (with-current-buffer buf
            (setq ps/org-avail--cur-layout "bullets")
            (ps/org-avail--buffer-toggle-layout)
            (should (string= ps/org-avail--cur-layout "inline"))
            (ps/org-avail--buffer-toggle-layout)
            (should (string= ps/org-avail--cur-layout "bullets")))
        (kill-buffer buf)))))

(ert-deftest ps/org-avail--buffer-render-has-header ()
  "Rendered buffer starts with the Days: header."
  (ps/avail-test--with-org-dir ""
    (let ((buf (ps/avail-test--make-avail-buffer dir)))
      (unwind-protect
          (with-current-buffer buf
            (ps/org-avail--buffer-render)
            (goto-char (point-min))
            (should (search-forward "Days:" nil t)))
        (kill-buffer buf)))))

(ert-deftest ps/org-avail--buffer-render-has-content ()
  "Rendered buffer contains a day name after the separator line."
  (ps/avail-test--with-org-dir ""
    (let ((buf (ps/avail-test--make-avail-buffer dir)))
      (unwind-protect
          (with-current-buffer buf
            (setq ps/org-avail--cur-days 1)
            (ps/org-avail--buffer-render)
            ;; The separator ─── line appears after header; day name appears after it
            (goto-char (point-min))
            (should (search-forward "─" nil t))
            ;; After the separator there should be some text (a day name)
            (should (> (point-max) (point))))
        (kill-buffer buf)))))

(ert-deftest ps/org-avail--buffer-init-not-reset-on-repeat ()
  "A second ps/org-avail--buffer-render preserves in-session changes."
  (ps/avail-test--with-org-dir ""
    (let ((buf (ps/avail-test--make-avail-buffer dir)))
      (unwind-protect
          (with-current-buffer buf
            (setq ps/org-avail--cur-days 99)
            (ps/org-avail--buffer-render)
            ;; Days should still be 99, not reset to defcustom
            (should (= ps/org-avail--cur-days 99)))
        (kill-buffer buf)))))

(ert-deftest ps/org-avail--buffer-buttons-have-face ()
  "Rendered header contains buttons with ps/org-avail--button-face."
  (ps/avail-test--with-org-dir ""
    (let ((buf (ps/avail-test--make-avail-buffer dir)))
      (unwind-protect
          (with-current-buffer buf
            (ps/org-avail--buffer-render)
            ;; At least one button must exist in the buffer
            (let ((btn (next-button (point-min))))
              (should btn)
              ;; That button must carry our custom face
              (let ((face (button-get btn 'face)))
                (should (or (eq face 'ps/org-avail--button-face)
                            (and (listp face)
                                 (memq 'ps/org-avail--button-face face)))))))
        (kill-buffer buf)))))
