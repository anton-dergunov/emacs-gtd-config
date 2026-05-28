;;; test-ps-org-conflicts.el --- ERT tests for ps-org-conflicts -*- lexical-binding: t; -*-

(require 'ert)
(add-to-list 'load-path "lisp")
(require 'ps-org-conflicts)

;;; Test helpers

(defconst ps/conflicts-test--sample-org
  "* Task A
SCHEDULED: <2026-05-04 Mon 10:00-11:00>

* Task B
SCHEDULED: <2026-05-04 Mon 10:30-11:30>

* Task C
SCHEDULED: <2026-05-04 Mon 11:35-12:00>

* Task D
SCHEDULED: <2026-05-04 Mon 12:10-13:00>
"
  "Sample org content matching the Python test fixture.
A and B overlap. B→C gap is 5 min. C→D gap is 10 min. Both < 15 min threshold.")

(defmacro ps/conflicts-test--with-org-dir (content &rest body)
  "Evaluate BODY with a temp dir containing test.org written with CONTENT.
Binds `dir' to the temp directory path. Cleans up afterward."
  (declare (indent 1))
  `(let ((dir (make-temp-file "ps-conflicts-test-" t)))
     (unwind-protect
         (progn
           (with-temp-file (expand-file-name "test.org" dir)
             (insert ,content))
           ,@body)
       (delete-directory dir t))))

(defmacro ps/conflicts-test--with-multi-org-dir (files &rest body)
  "Evaluate BODY with a temp dir containing multiple org files.
FILES is an alist of (filename . content). Binds `dir'."
  (declare (indent 1))
  `(let ((dir (make-temp-file "ps-conflicts-test-" t)))
     (unwind-protect
         (progn
           (dolist (entry ,files)
             (with-temp-file (expand-file-name (car entry) dir)
               (insert (cdr entry))))
           ,@body)
       (delete-directory dir t))))

;;; -------------------------------------------------------
;;; parse-time
;;; -------------------------------------------------------

(ert-deftest ps/conflicts--parse-time-zero ()
  (should (= (ps/conflicts--parse-time "00:00") 0)))

(ert-deftest ps/conflicts--parse-time-normal ()
  (should (= (ps/conflicts--parse-time "10:30") 630)))

(ert-deftest ps/conflicts--parse-time-noon ()
  (should (= (ps/conflicts--parse-time "12:00") 720)))

(ert-deftest ps/conflicts--parse-time-end-of-day ()
  (should (= (ps/conflicts--parse-time "23:59") 1439)))

(ert-deftest ps/conflicts--parse-time-round-hours ()
  (should (= (ps/conflicts--parse-time "11:00") 660))
  (should (= (ps/conflicts--parse-time "13:00") 780)))

;;; -------------------------------------------------------
;;; parse-date
;;; -------------------------------------------------------

(ert-deftest ps/conflicts--parse-date-basic ()
  (let ((d (ps/conflicts--parse-date "2026-05-04")))
    (should (= (nth 0 d) 5))     ; month
    (should (= (nth 1 d) 4))     ; day
    (should (= (nth 2 d) 2026)))) ; year

(ert-deftest ps/conflicts--parse-date-january ()
  (let ((d (ps/conflicts--parse-date "2025-01-01")))
    (should (= (nth 0 d) 1))
    (should (= (nth 1 d) 1))
    (should (= (nth 2 d) 2025))))

;;; -------------------------------------------------------
;;; format-time
;;; -------------------------------------------------------

(ert-deftest ps/conflicts--format-time-zero ()
  (should (string= (ps/conflicts--format-time 0) "00:00")))

(ert-deftest ps/conflicts--format-time-noon ()
  (should (string= (ps/conflicts--format-time 720) "12:00")))

(ert-deftest ps/conflicts--format-time-with-minutes ()
  (should (string= (ps/conflicts--format-time 630) "10:30")))

(ert-deftest ps/conflicts--format-time-leading-zeros ()
  (should (string= (ps/conflicts--format-time 65) "01:05")))

;;; -------------------------------------------------------
;;; date<
;;; -------------------------------------------------------

(ert-deftest ps/conflicts--date<-year ()
  (should (ps/conflicts--date< '(1 1 2025) '(1 1 2026)))
  (should-not (ps/conflicts--date< '(1 1 2026) '(1 1 2025))))

(ert-deftest ps/conflicts--date<-month ()
  (should (ps/conflicts--date< '(3 1 2026) '(5 1 2026)))
  (should-not (ps/conflicts--date< '(5 1 2026) '(3 1 2026))))

(ert-deftest ps/conflicts--date<-day ()
  (should (ps/conflicts--date< '(5 3 2026) '(5 5 2026)))
  (should-not (ps/conflicts--date< '(5 5 2026) '(5 3 2026))))

(ert-deftest ps/conflicts--date<-equal ()
  (should-not (ps/conflicts--date< '(5 4 2026) '(5 4 2026))))

;;; -------------------------------------------------------
;;; extract-events / load-events
;;; -------------------------------------------------------

(ert-deftest ps/conflicts--load-events-count ()
  "Four timed events are extracted from the sample fixture."
  (ps/conflicts-test--with-org-dir ps/conflicts-test--sample-org
    (let ((events (ps/conflicts--load-events dir)))
      (should (= (length events) 4)))))

(ert-deftest ps/conflicts--load-events-first-title ()
  "First event has title 'Task A'."
  (ps/conflicts-test--with-org-dir ps/conflicts-test--sample-org
    (let ((events (ps/conflicts--load-events dir)))
      (should (string= (ps/conflict--event-title (car events)) "Task A")))))

(ert-deftest ps/conflicts--load-events-date-fields ()
  "First event has correct date fields (2026-05-04)."
  (ps/conflicts-test--with-org-dir ps/conflicts-test--sample-org
    (let ((e (car (ps/conflicts--load-events dir))))
      (should (= (ps/conflict--event-month e) 5))
      (should (= (ps/conflict--event-day   e) 4))
      (should (= (ps/conflict--event-year  e) 2026)))))

(ert-deftest ps/conflicts--load-events-time-fields ()
  "First event has start=600 (10:00) and end=660 (11:00)."
  (ps/conflicts-test--with-org-dir ps/conflicts-test--sample-org
    (let ((e (car (ps/conflicts--load-events dir))))
      (should (= (ps/conflict--event-start e) 600))
      (should (= (ps/conflict--event-end   e) 660)))))

(ert-deftest ps/conflicts--load-events-line-number ()
  "Line number is recorded for each event."
  (ps/conflicts-test--with-org-dir ps/conflicts-test--sample-org
    (let ((e (car (ps/conflicts--load-events dir))))
      (should (> (ps/conflict--event-line e) 0)))))

(ert-deftest ps/conflicts--load-events-file-path ()
  "File path is stored as a non-empty string."
  (ps/conflicts-test--with-org-dir ps/conflicts-test--sample-org
    (let ((e (car (ps/conflicts--load-events dir))))
      (should (stringp (ps/conflict--event-file e)))
      (should (> (length (ps/conflict--event-file e)) 0)))))

(ert-deftest ps/conflicts--load-events-date-only-ignored ()
  "Timestamps without a time range are ignored."
  (ps/conflicts-test--with-org-dir
      "* Task\n<2026-05-04 Mon>\n<2026-05-05 Tue 10:00>\n"
    (let ((events (ps/conflicts--load-events dir)))
      (should (= (length events) 0)))))

(ert-deftest ps/conflicts--load-events-empty-file ()
  "An empty org file produces no events."
  (ps/conflicts-test--with-org-dir ""
    (let ((events (ps/conflicts--load-events dir)))
      (should (= (length events) 0)))))

(ert-deftest ps/conflicts--load-events-multiple-files ()
  "Events are aggregated across multiple .org files."
  (ps/conflicts-test--with-multi-org-dir
      '(("a.org" . "* A\nSCHEDULED: <2026-05-04 Mon 09:00-10:00>\n")
        ("b.org" . "* B\nSCHEDULED: <2026-05-04 Mon 11:00-12:00>\n"))
    (let ((events (ps/conflicts--load-events dir)))
      (should (= (length events) 2)))))

(ert-deftest ps/conflicts--load-events-no-heading ()
  "Timestamps before any heading are ignored."
  (ps/conflicts-test--with-org-dir
      "SCHEDULED: <2026-05-04 Mon 10:00-11:00>\n* Task\nsome text\n"
    (let ((events (ps/conflicts--load-events dir)))
      (should (= (length events) 0)))))

;;; -------------------------------------------------------
;;; group-by-day
;;; -------------------------------------------------------

(ert-deftest ps/conflicts--group-by-day-single-day ()
  "All events on the same day form one group."
  (ps/conflicts-test--with-org-dir ps/conflicts-test--sample-org
    (let* ((events (ps/conflicts--load-events dir))
           (by-day (ps/conflicts--group-by-day events)))
      (should (= (length by-day) 1))
      (should (= (length (cdar by-day)) 4)))))

(ert-deftest ps/conflicts--group-by-day-two-days ()
  "Events on two different days produce two groups."
  (ps/conflicts-test--with-org-dir
      "* A\nSCHEDULED: <2026-05-04 Mon 10:00-11:00>\n* B\nSCHEDULED: <2026-05-05 Tue 10:00-11:00>\n"
    (let* ((events (ps/conflicts--load-events dir))
           (by-day (ps/conflicts--group-by-day events)))
      (should (= (length by-day) 2)))))

(ert-deftest ps/conflicts--group-by-day-sorted ()
  "Groups are sorted chronologically."
  (ps/conflicts-test--with-org-dir
      "* B\nSCHEDULED: <2026-05-05 Tue 10:00-11:00>\n* A\nSCHEDULED: <2026-05-04 Mon 10:00-11:00>\n"
    (let* ((events (ps/conflicts--load-events dir))
           (by-day (ps/conflicts--group-by-day events))
           (first-day (caar by-day)))
      ;; First group should be May 4, not May 5
      (should (= (nth 1 first-day) 4)))))

;;; -------------------------------------------------------
;;; find-overlaps
;;; -------------------------------------------------------

(ert-deftest ps/conflicts--find-overlaps-one-overlap ()
  "Task A (10:00-11:00) and Task B (10:30-11:30) produce exactly one overlap."
  (ps/conflicts-test--with-org-dir ps/conflicts-test--sample-org
    (let* ((events (ps/conflicts--load-events dir))
           (by-day (ps/conflicts--group-by-day events))
           (overlaps (ps/conflicts--find-overlaps (cdar by-day))))
      (should (= (length overlaps) 1)))))

(ert-deftest ps/conflicts--find-overlaps-titles ()
  "The overlapping pair is Task A and Task B."
  (ps/conflicts-test--with-org-dir ps/conflicts-test--sample-org
    (let* ((events (ps/conflicts--load-events dir))
           (by-day (ps/conflicts--group-by-day events))
           (pair   (car (ps/conflicts--find-overlaps (cdar by-day)))))
      (should (string= (ps/conflict--event-title (nth 0 pair)) "Task A"))
      (should (string= (ps/conflict--event-title (nth 1 pair)) "Task B")))))

(ert-deftest ps/conflicts--find-overlaps-no-overlap ()
  "Non-overlapping events produce no overlaps."
  (let* ((e1 (ps/conflict--make-event :file "" :line 1 :title "A"
                                           :month 5 :day 4 :year 2026
                                           :start 600 :end 660))
         (e2 (ps/conflict--make-event :file "" :line 2 :title "B"
                                           :month 5 :day 4 :year 2026
                                           :start 720 :end 780))
         (overlaps (ps/conflicts--find-overlaps (list e1 e2))))
    (should (null overlaps))))

(ert-deftest ps/conflicts--find-overlaps-adjacent ()
  "Adjacent events (B starts exactly when A ends) do not overlap."
  (let* ((e1 (ps/conflict--make-event :file "" :line 1 :title "A"
                                           :month 5 :day 4 :year 2026
                                           :start 600 :end 660))
         (e2 (ps/conflict--make-event :file "" :line 2 :title "B"
                                           :month 5 :day 4 :year 2026
                                           :start 660 :end 720))
         (overlaps (ps/conflicts--find-overlaps (list e1 e2))))
    (should (null overlaps))))

(ert-deftest ps/conflicts--find-overlaps-total-containment ()
  "An event fully contained within another counts as an overlap."
  (let* ((outer (ps/conflict--make-event :file "" :line 1 :title "Outer"
                                              :month 5 :day 4 :year 2026
                                              :start 600 :end 780))
         (inner (ps/conflict--make-event :file "" :line 2 :title "Inner"
                                              :month 5 :day 4 :year 2026
                                              :start 660 :end 720))
         (overlaps (ps/conflicts--find-overlaps (list outer inner))))
    (should (= (length overlaps) 1))))

(ert-deftest ps/conflicts--find-overlaps-multiple ()
  "Three mutually overlapping events produce the correct number of pairs."
  (let* ((e1 (ps/conflict--make-event :file "" :line 1 :title "A"
                                           :month 5 :day 4 :year 2026
                                           :start 600 :end 780))
         (e2 (ps/conflict--make-event :file "" :line 2 :title "B"
                                           :month 5 :day 4 :year 2026
                                           :start 660 :end 840))
         (e3 (ps/conflict--make-event :file "" :line 3 :title "C"
                                           :month 5 :day 4 :year 2026
                                           :start 720 :end 900))
         (overlaps (ps/conflicts--find-overlaps (list e1 e2 e3))))
    ;; A-B, A-C, B-C → 3 pairs
    (should (= (length overlaps) 3))))

;;; -------------------------------------------------------
;;; find-small-gaps
;;; -------------------------------------------------------

(ert-deftest ps/conflicts--find-small-gaps-b-to-c ()
  "Task B ends 11:30, Task C starts 11:35: gap = 5 min < 15."
  (ps/conflicts-test--with-org-dir ps/conflicts-test--sample-org
    (let* ((events (ps/conflicts--load-events dir))
           (by-day (ps/conflicts--group-by-day events))
           (gaps   (ps/conflicts--find-small-gaps (cdar by-day) 15)))
      ;; B→C (5 min) and C→D (10 min) both qualify
      (should (> (length gaps) 0))
      (should (cl-some (lambda (g) (< (nth 2 g) 15)) gaps)))))

(ert-deftest ps/conflicts--find-small-gaps-exact-count ()
  "In the sample: B→C (5 min) and C→D (10 min) are both flagged."
  (ps/conflicts-test--with-org-dir ps/conflicts-test--sample-org
    (let* ((events (ps/conflicts--load-events dir))
           (by-day (ps/conflicts--group-by-day events))
           (gaps   (ps/conflicts--find-small-gaps (cdar by-day) 15)))
      (should (= (length gaps) 2)))))

(ert-deftest ps/conflicts--find-small-gaps-none ()
  "Events with large gaps produce no flagged gaps."
  (let* ((e1 (ps/conflict--make-event :file "" :line 1 :title "A"
                                           :month 5 :day 4 :year 2026
                                           :start 600 :end 660))
         (e2 (ps/conflict--make-event :file "" :line 2 :title "B"
                                           :month 5 :day 4 :year 2026
                                           :start 720 :end 780))
         (gaps (ps/conflicts--find-small-gaps (list e1 e2) 15)))
    (should (null gaps))))

(ert-deftest ps/conflicts--find-small-gaps-at-threshold ()
  "A gap exactly at the threshold is not flagged."
  (let* ((e1 (ps/conflict--make-event :file "" :line 1 :title "A"
                                           :month 5 :day 4 :year 2026
                                           :start 600 :end 660))
         (e2 (ps/conflict--make-event :file "" :line 2 :title "B"
                                           :month 5 :day 4 :year 2026
                                           :start 675 :end 720)) ; gap = 15 min
         (gaps (ps/conflicts--find-small-gaps (list e1 e2) 15)))
    (should (null gaps))))

(ert-deftest ps/conflicts--find-small-gaps-just-below-threshold ()
  "A gap one minute below the threshold is flagged."
  (let* ((e1 (ps/conflict--make-event :file "" :line 1 :title "A"
                                           :month 5 :day 4 :year 2026
                                           :start 600 :end 660))
         (e2 (ps/conflict--make-event :file "" :line 2 :title "B"
                                           :month 5 :day 4 :year 2026
                                           :start 674 :end 720)) ; gap = 14 min
         (gaps (ps/conflicts--find-small-gaps (list e1 e2) 15)))
    (should (= (length gaps) 1))
    (should (= (nth 2 (car gaps)) 14))))

(ert-deftest ps/conflicts--find-small-gaps-overlapping-events ()
  "Negative gaps (overlapping events) are not flagged by find-small-gaps."
  (let* ((e1 (ps/conflict--make-event :file "" :line 1 :title "A"
                                           :month 5 :day 4 :year 2026
                                           :start 600 :end 720))
         (e2 (ps/conflict--make-event :file "" :line 2 :title "B"
                                           :month 5 :day 4 :year 2026
                                           :start 660 :end 780)) ; starts before A ends
         (gaps (ps/conflicts--find-small-gaps (list e1 e2) 15)))
    (should (null gaps))))

(ert-deftest ps/conflicts--find-small-gaps-zero-threshold ()
  "A zero threshold never flags any gap."
  (let* ((e1 (ps/conflict--make-event :file "" :line 1 :title "A"
                                           :month 5 :day 4 :year 2026
                                           :start 600 :end 660))
         (e2 (ps/conflict--make-event :file "" :line 2 :title "B"
                                           :month 5 :day 4 :year 2026
                                           :start 661 :end 720))
         (gaps (ps/conflicts--find-small-gaps (list e1 e2) 0)))
    (should (null gaps))))

;;; -------------------------------------------------------
;;; find-all (integration)
;;; -------------------------------------------------------

(ert-deftest ps/conflicts--find-all-returns-plist ()
  "find-all returns a plist with :overlaps and :gaps keys."
  (ps/conflicts-test--with-org-dir ps/conflicts-test--sample-org
    (let ((result (ps/conflicts--find-all dir 15 t)))
      (should (plist-member result :overlaps))
      (should (plist-member result :gaps)))))

(ert-deftest ps/conflicts--find-all-with-include-past ()
  "include-past=t returns conflicts even for past-dated events."
  (ps/conflicts-test--with-org-dir ps/conflicts-test--sample-org
    (let* ((result (ps/conflicts--find-all dir 15 t))
           (n (ps/conflicts--count result)))
      (should (> n 0)))))

(ert-deftest ps/conflicts--find-all-exclude-past ()
  "include-past=nil filters out events before today, so no conflicts found
for the sample (which is dated 2026-05-04, well in the past from today's view
during tests — unless today IS 2026-05-04)."
  ;; Use a far-future date to guarantee the events are in the past
  (let ((org-content
         "* A\nSCHEDULED: <2000-01-01 Sat 10:00-11:00>\n* B\nSCHEDULED: <2000-01-01 Sat 10:30-11:30>\n"))
    (ps/conflicts-test--with-org-dir org-content
      (let* ((result (ps/conflicts--find-all dir 15 nil))
             (n (ps/conflicts--count result)))
        (should (= n 0))))))

(ert-deftest ps/conflicts--find-all-no-events ()
  "A directory with no timed events produces zero conflicts."
  (ps/conflicts-test--with-org-dir "* Task\nJust some text.\n"
    (let* ((result (ps/conflicts--find-all dir 15 t))
           (n (ps/conflicts--count result)))
      (should (= n 0)))))

(ert-deftest ps/conflicts--count-sums-both-types ()
  "count sums overlaps and gaps across all days."
  (ps/conflicts-test--with-org-dir ps/conflicts-test--sample-org
    (let* ((result (ps/conflicts--find-all dir 15 t))
           (n (ps/conflicts--count result)))
      ;; 1 overlap (A-B) + 2 gaps (B→C, C→D)
      (should (= n 3)))))

;;; -------------------------------------------------------
;;; format-event-label
;;; -------------------------------------------------------

(ert-deftest ps/conflicts--format-event-label-contains-title ()
  "Event label contains the task title."
  (let ((e (ps/conflict--make-event :file "/dir/foo.org" :line 5
                                         :title "My Task"
                                         :month 5 :day 4 :year 2026
                                         :start 600 :end 660)))
    (should (string-match-p "My Task" (ps/conflicts--format-event-label e)))))

(ert-deftest ps/conflicts--format-event-label-contains-times ()
  "Event label contains start and end times."
  (let ((e (ps/conflict--make-event :file "/dir/foo.org" :line 5
                                         :title "T"
                                         :month 5 :day 4 :year 2026
                                         :start 600 :end 660)))
    (let ((label (ps/conflicts--format-event-label e)))
      (should (string-match-p "10:00" label))
      (should (string-match-p "11:00" label)))))

(ert-deftest ps/conflicts--format-event-label-basename-only ()
  "Event label uses the file basename, not the full path."
  (let ((e (ps/conflict--make-event :file "/long/path/to/foo.org" :line 5
                                         :title "T"
                                         :month 5 :day 4 :year 2026
                                         :start 600 :end 660)))
    (let ((label (ps/conflicts--format-event-label e)))
      (should (string-match-p "foo\\.org" label))
      (should-not (string-match-p "/long/path" label)))))

(ert-deftest ps/conflicts--format-date-basic ()
  "Date (5 4 2026) formats as 2026-05-04."
  (should (string= (ps/conflicts--format-date '(5 4 2026)) "2026-05-04")))

(ert-deftest ps/conflicts--format-date-leading-zeros ()
  "Single-digit month and day are zero-padded."
  (should (string= (ps/conflicts--format-date '(1 3 2026)) "2026-01-03")))

;;; -------------------------------------------------------
;;; Interactive buffer mode
;;; -------------------------------------------------------

(defun ps/conflicts-test--make-buffer (dir)
  "Set up a fresh *Org Conflicts* test buffer pointing at DIR.
Returns the buffer."
  (when (get-buffer "*Org Conflicts*")
    (kill-buffer "*Org Conflicts*"))
  (let ((buf (get-buffer-create "*Org Conflicts*")))
    (with-current-buffer buf
      (ps-conflicts-mode)
      (setq ps/conflicts--cur-directory   dir
            ps/conflicts--cur-gap         ps/conflicts-gap-minutes
            ps/conflicts--cur-include-past ps/conflicts-include-past))
    buf))

(ert-deftest ps/conflicts--mode-is-special-mode ()
  "ps-conflicts-mode is derived from special-mode."
  (let ((buf (get-buffer-create " *conflicts-test-mode*")))
    (unwind-protect
        (with-current-buffer buf
          (ps-conflicts-mode)
          (should (derived-mode-p 'special-mode)))
      (kill-buffer buf))))

(ert-deftest ps/conflicts--buffer-init-from-defcustom ()
  "Buffer-local vars are initialized from ps/conflicts-gap-minutes."
  (ps/conflicts-test--with-org-dir ""
    (let ((buf (ps/conflicts-test--make-buffer dir)))
      (unwind-protect
          (with-current-buffer buf
            (should (= ps/conflicts--cur-gap ps/conflicts-gap-minutes))
            (should (eq ps/conflicts--cur-include-past ps/conflicts-include-past)))
        (kill-buffer buf)))))

(ert-deftest ps/conflicts--buffer-more-gap ()
  "more-gap increments ps/conflicts--cur-gap by 5."
  (ps/conflicts-test--with-org-dir ""
    (let ((buf (ps/conflicts-test--make-buffer dir)))
      (unwind-protect
          (with-current-buffer buf
            (setq ps/conflicts--cur-gap 15)
            (ps/conflicts--buffer-more-gap)
            (should (= ps/conflicts--cur-gap 20)))
        (kill-buffer buf)))))

(ert-deftest ps/conflicts--buffer-less-gap ()
  "less-gap decrements ps/conflicts--cur-gap by 5, clamped at 0."
  (ps/conflicts-test--with-org-dir ""
    (let ((buf (ps/conflicts-test--make-buffer dir)))
      (unwind-protect
          (with-current-buffer buf
            (setq ps/conflicts--cur-gap 10)
            (ps/conflicts--buffer-less-gap)
            (should (= ps/conflicts--cur-gap 5))
            ;; Clamp at zero
            (setq ps/conflicts--cur-gap 3)
            (ps/conflicts--buffer-less-gap)
            (should (= ps/conflicts--cur-gap 0))
            (ps/conflicts--buffer-less-gap)
            (should (= ps/conflicts--cur-gap 0)))
        (kill-buffer buf)))))

(ert-deftest ps/conflicts--buffer-toggle-past ()
  "toggle-past flips ps/conflicts--cur-include-past nil → t → nil."
  (ps/conflicts-test--with-org-dir ""
    (let ((buf (ps/conflicts-test--make-buffer dir)))
      (unwind-protect
          (with-current-buffer buf
            (setq ps/conflicts--cur-include-past nil)
            (ps/conflicts--buffer-toggle-past)
            (should (eq ps/conflicts--cur-include-past t))
            (ps/conflicts--buffer-toggle-past)
            (should (eq ps/conflicts--cur-include-past nil)))
        (kill-buffer buf)))))

(ert-deftest ps/conflicts--buffer-render-has-header ()
  "Rendered buffer starts with the Gap: header line."
  (ps/conflicts-test--with-org-dir ""
    (let ((buf (ps/conflicts-test--make-buffer dir)))
      (unwind-protect
          (with-current-buffer buf
            (ps/conflicts--buffer-render)
            (goto-char (point-min))
            (should (search-forward "Gap:" nil t)))
        (kill-buffer buf)))))

(ert-deftest ps/conflicts--buffer-render-has-separator ()
  "Rendered buffer contains the ─── separator line."
  (ps/conflicts-test--with-org-dir ""
    (let ((buf (ps/conflicts-test--make-buffer dir)))
      (unwind-protect
          (with-current-buffer buf
            (ps/conflicts--buffer-render)
            (goto-char (point-min))
            (should (search-forward "─" nil t)))
        (kill-buffer buf)))))

(ert-deftest ps/conflicts--buffer-render-no-conflicts-message ()
  "Buffer shows 'No conflicts found.' when there are none."
  (ps/conflicts-test--with-org-dir ""
    (let ((buf (ps/conflicts-test--make-buffer dir)))
      (unwind-protect
          (with-current-buffer buf
            (ps/conflicts--buffer-render)
            (goto-char (point-min))
            (should (search-forward "No conflicts found" nil t)))
        (kill-buffer buf)))))

(ert-deftest ps/conflicts--buffer-render-shows-conflicts ()
  "Buffer shows conflict entries when conflicts exist."
  (ps/conflicts-test--with-org-dir ps/conflicts-test--sample-org
    (let ((buf (ps/conflicts-test--make-buffer dir)))
      (unwind-protect
          (with-current-buffer buf
            (setq ps/conflicts--cur-include-past t)
            (ps/conflicts--buffer-render)
            (goto-char (point-min))
            (should (search-forward "Overlapping" nil t)))
        (kill-buffer buf)))))

(ert-deftest ps/conflicts--buffer-render-shows-date-header ()
  "Buffer shows the === YYYY-MM-DD === section header for conflicting days."
  (ps/conflicts-test--with-org-dir ps/conflicts-test--sample-org
    (let ((buf (ps/conflicts-test--make-buffer dir)))
      (unwind-protect
          (with-current-buffer buf
            (setq ps/conflicts--cur-include-past t)
            (ps/conflicts--buffer-render)
            (goto-char (point-min))
            (should (search-forward "2026-05-04" nil t)))
        (kill-buffer buf)))))

(ert-deftest ps/conflicts--buffer-render-has-buttons ()
  "Rendered buffer contains buttons in the header."
  (ps/conflicts-test--with-org-dir ""
    (let ((buf (ps/conflicts-test--make-buffer dir)))
      (unwind-protect
          (with-current-buffer buf
            (ps/conflicts--buffer-render)
            (let ((btn (next-button (point-min))))
              (should btn)
              (let ((face (button-get btn 'face)))
                (should (or (eq face 'ps/conflicts--button-face)
                            (and (listp face)
                                 (memq 'ps/conflicts--button-face face)))))))
        (kill-buffer buf)))))

(ert-deftest ps/conflicts--buffer-render-event-buttons ()
  "Conflict entries are rendered as clickable buttons."
  (ps/conflicts-test--with-org-dir ps/conflicts-test--sample-org
    (let ((buf (ps/conflicts-test--make-buffer dir)))
      (unwind-protect
          (with-current-buffer buf
            (setq ps/conflicts--cur-include-past t)
            (ps/conflicts--buffer-render)
            ;; Skip past the header buttons to find event buttons
            (goto-char (point-min))
            (search-forward "─" nil t)  ; skip past separator
            (let ((btn (next-button (point))))
              (should btn)))
        (kill-buffer buf)))))

(ert-deftest ps/conflicts--buffer-gap-preserved-on-render ()
  "A second render does not reset the gap value."
  (ps/conflicts-test--with-org-dir ""
    (let ((buf (ps/conflicts-test--make-buffer dir)))
      (unwind-protect
          (with-current-buffer buf
            (setq ps/conflicts--cur-gap 42)
            (ps/conflicts--buffer-render)
            (should (= ps/conflicts--cur-gap 42)))
        (kill-buffer buf)))))

(ert-deftest ps/conflicts--buffer-keybindings ()
  "Mode map has keybindings for g, r, +, -, p, and RET."
  (let ((map ps-conflicts-mode-map))
    (should (lookup-key map (kbd "g")))
    (should (lookup-key map (kbd "r")))
    (should (lookup-key map (kbd "+")))
    (should (lookup-key map (kbd "-")))
    (should (lookup-key map (kbd "p")))
    (should (lookup-key map (kbd "RET")))))
