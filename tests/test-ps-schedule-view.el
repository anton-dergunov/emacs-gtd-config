;;; test-ps-schedule-view.el --- ERT tests for ps-schedule-view.el -*- lexical-binding: t; -*-

(add-to-list 'load-path (expand-file-name "../lisp" (file-name-directory load-file-name)))

;; Stub out ps-agenda-layout so we can load ps-schedule-view in isolation.
(unless (featurep 'ps-agenda-layout)
  (defun ps/agenda-layout--columns () '(:cat 1 :state 5 :pri 12 :emoji 17 :title 20))
  (defun ps/agenda-layout--window-cols () 80)
  (defvar ps/agenda-layout-left-margin-cols 1)
  (defvar ps/agenda-layout-right-margin-cols 2)
  (defvar ps/agenda-layout-truncate t)
  (defvar ps/agenda-layout-schedule-group "Schedule")
  (defvar ps/agenda-layout-emoji-face nil)
  (provide 'ps-agenda-layout))

(require 'ps-schedule-view)
(require 'ert)

;;; ------------------------------------------------------------------
;;; ps/schedule-view--to-mins

(ert-deftest ps/schedule-view--to-mins/midnight ()
  (should (= 0 (ps/schedule-view--to-mins 0))))

(ert-deftest ps/schedule-view--to-mins/morning ()
  (should (= 570 (ps/schedule-view--to-mins 930))))

(ert-deftest ps/schedule-view--to-mins/noon ()
  (should (= 720 (ps/schedule-view--to-mins 1200))))

(ert-deftest ps/schedule-view--to-mins/late ()
  (should (= 1320 (ps/schedule-view--to-mins 2200))))

;;; ------------------------------------------------------------------
;;; ps/schedule-view--end-mins

(ert-deftest ps/schedule-view--end-mins/with-duration ()
  (should (= 630 (ps/schedule-view--end-mins 900 90))))

(ert-deftest ps/schedule-view--end-mins/zero-duration ()
  (should (= 570 (ps/schedule-view--end-mins 930 0))))

(ert-deftest ps/schedule-view--end-mins/nil-duration ()
  (should (= 570 (ps/schedule-view--end-mins 930 nil))))

(ert-deftest ps/schedule-view--end-mins/fractional-duration ()
  (should (= 585 (ps/schedule-view--end-mins 930 15))))

;;; ------------------------------------------------------------------
;;; ps/schedule-view--fmt-tod

(ert-deftest ps/schedule-view--fmt-tod/zero-padded-hour ()
  (should (equal "08:00" (ps/schedule-view--fmt-tod 800))))

(ert-deftest ps/schedule-view--fmt-tod/zero-padded-minute ()
  (should (equal "13:05" (ps/schedule-view--fmt-tod 1305))))

(ert-deftest ps/schedule-view--fmt-tod/noon ()
  (should (equal "12:00" (ps/schedule-view--fmt-tod 1200))))

;;; ------------------------------------------------------------------
;;; ps/schedule-view--time-range-str

(ert-deftest ps/schedule-view--time-range-str/with-duration ()
  (should (equal "08:00-08:15" (ps/schedule-view--time-range-str 800 15))))

(ert-deftest ps/schedule-view--time-range-str/hour-crossing ()
  (should (equal "09:30-10:00" (ps/schedule-view--time-range-str 930 30))))

(ert-deftest ps/schedule-view--time-range-str/no-duration ()
  ;; No duration: "HH:MM" padded to 11 chars with spaces.
  (let ((s (ps/schedule-view--time-range-str 1500 nil)))
    (should (equal 11 (length s)))
    (should (string-prefix-p "15:00" s))
    (should (string-suffix-p "      " s))))

(ert-deftest ps/schedule-view--time-range-str/zero-duration ()
  (let ((s (ps/schedule-view--time-range-str 800 0)))
    (should (equal 11 (length s)))
    (should (string-prefix-p "08:00" s))))

(ert-deftest ps/schedule-view--time-range-str/always-11-chars ()
  (should (= 11 (length (ps/schedule-view--time-range-str 800 15))))
  (should (= 11 (length (ps/schedule-view--time-range-str 1730 90)))))

;;; ------------------------------------------------------------------
;;; ps/schedule-view--grid-str

(ert-deftest ps/schedule-view--grid-str/zero-padded ()
  (let ((s (ps/schedule-view--grid-str 800)))
    (should (string-prefix-p "08:00" s))
    (should (= 11 (length s)))))

(ert-deftest ps/schedule-view--grid-str/always-11-chars ()
  (should (= 11 (length (ps/schedule-view--grid-str 0))))
  (should (= 11 (length (ps/schedule-view--grid-str 1200))))
  (should (= 11 (length (ps/schedule-view--grid-str 2359)))))

;;; ------------------------------------------------------------------
;;; ps/schedule-view--find-overlaps

(ert-deftest ps/schedule-view--find-overlaps/no-items ()
  (should (null (ps/schedule-view--find-overlaps '()))))

(ert-deftest ps/schedule-view--find-overlaps/no-overlap ()
  ;; 08:00-09:00 and 09:00-10:00 — touching but not overlapping
  (let* ((m1 (cons 'marker 1))
         (m2 (cons 'marker 2))
         (items (list (list 480 540 m1) (list 540 600 m2))))
    (should (null (ps/schedule-view--find-overlaps items)))))

(ert-deftest ps/schedule-view--find-overlaps/overlap ()
  ;; 08:00-09:30 and 09:00-10:00 — overlap from 09:00-09:30
  (let* ((m1 (cons 'marker 1))
         (m2 (cons 'marker 2))
         (items (list (list 480 570 m1) (list 540 600 m2)))
         (result (ps/schedule-view--find-overlaps items)))
    (should (memq m1 result))
    (should (memq m2 result))))

(ert-deftest ps/schedule-view--find-overlaps/contained ()
  ;; 08:00-10:00 contains 08:30-09:00
  (let* ((m1 (cons 'marker 1))
         (m2 (cons 'marker 2))
         (items (list (list 480 600 m1) (list 510 540 m2)))
         (result (ps/schedule-view--find-overlaps items)))
    (should (memq m1 result))
    (should (memq m2 result))))

(ert-deftest ps/schedule-view--find-overlaps/no-self-overlap ()
  ;; A single item cannot overlap with itself.
  (let* ((m1 (cons 'marker 1))
         (items (list (list 480 540 m1))))
    (should (null (ps/schedule-view--find-overlaps items)))))

(ert-deftest ps/schedule-view--find-overlaps/non-overlapping-pair ()
  (let* ((m1 (cons 'marker 1))
         (m2 (cons 'marker 2))
         ;; 08:00-08:30 and 09:00-09:30 — gap between
         (items (list (list 480 510 m1) (list 540 570 m2))))
    (should (null (ps/schedule-view--find-overlaps items)))))

;;; ------------------------------------------------------------------
;;; ps/schedule-view--cols

(ert-deftest ps/schedule-view--cols/shifts-by-prefix-width ()
  ;; delta = prefix-width (not prefix-width - left-margin) because left-margin
  ;; is physically prepended to the rendered string, so :cat lands at
  ;; left-margin + prefix-width = 15 (with stub left-margin=1, prefix-width=14).
  (let* ((cols (ps/schedule-view--cols))
         (expected (+ ps/schedule-view--prefix-width ps/agenda-layout-left-margin-cols)))
    (should (= expected (plist-get cols :cat)))))

;;; ------------------------------------------------------------------
;;; ps/schedule-view--now-line-str

(ert-deftest ps/schedule-view--now-line-str/contains-time ()
  (let ((s (ps/schedule-view--now-line-str 1347 80)))
    (should (string-match-p "13:47" s))))

(ert-deftest ps/schedule-view--now-line-str/bar-at-col ()
  ;; ┆ is at column (left-margin + time-col-width + 1).
  ;; With stub left-margin=1 and time-col-width=11, expected col = 13.
  (let* ((s (ps/schedule-view--now-line-str 900 80))
         (plain (substring-no-properties s))
         (expected-col (+ ps/agenda-layout-left-margin-cols
                          (1+ ps/schedule-view--time-col-width))))
    (should (= ?┆ (aref plain expected-col)))))

(ert-deftest ps/schedule-view--now-line-str/narrow-window ()
  ;; Should not error even with a very narrow window.
  (should (stringp (ps/schedule-view--now-line-str 900 10))))

(provide 'test-ps-schedule-view)
;;; test-ps-schedule-view.el ends here
