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
(require 'cl-lib)
(require 'org-agenda)

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

(ert-deftest ps/schedule-view--time-range-str/nil-tod-is-blank ()
  ;; Regression: an untimed item (time-of-day = nil) must yield a blank,
  ;; correctly-padded time column instead of signalling
  ;; "Wrong type argument: number-or-marker-p, nil".
  (let ((s (ps/schedule-view--time-range-str nil nil)))
    (should (equal 11 (length s)))
    (should (string-blank-p s))))

(ert-deftest ps/schedule-view--time-range-str/nil-tod-ignores-duration ()
  ;; A nil time-of-day stays blank even if a duration is somehow present.
  (let ((s (ps/schedule-view--time-range-str nil 60)))
    (should (equal 11 (length s)))
    (should (string-blank-p s))))

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
  ;; 08:00-09:30 (m1) and 09:00-10:00 (m2).
  ;; m2 starts during m1 → m2 flagged; m1 is already running → not flagged.
  (let* ((m1 (cons 'marker 1))
         (m2 (cons 'marker 2))
         (items (list (list 480 570 m1) (list 540 600 m2)))
         (result (ps/schedule-view--find-overlaps items)))
    (should-not (memq m1 result))
    (should     (memq m2 result))))

(ert-deftest ps/schedule-view--find-overlaps/contained ()
  ;; 08:00-10:00 (m1) contains 08:30-09:00 (m2).
  ;; m2 starts during m1 → m2 flagged; m1 not flagged.
  (let* ((m1 (cons 'marker 1))
         (m2 (cons 'marker 2))
         (items (list (list 480 600 m1) (list 510 540 m2)))
         (result (ps/schedule-view--find-overlaps items)))
    (should-not (memq m1 result))
    (should     (memq m2 result))))

(ert-deftest ps/schedule-view--find-overlaps/three-way ()
  ;; 10:00-12:00 (mA), 10:15-10:30 (mB), 11:00-11:15 (mC).
  ;; B and C start during A; A is not flagged.
  ;; B and C do not overlap each other (B ends before C starts).
  (let* ((mA (cons 'marker 'A))
         (mB (cons 'marker 'B))
         (mC (cons 'marker 'C))
         (items (list (list 600 720 mA) (list 615 630 mB) (list 660 675 mC)))
         (result (ps/schedule-view--find-overlaps items)))
    (should-not (memq mA result))
    (should     (memq mB result))
    (should     (memq mC result))))

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
;;; ps/schedule-view--is-past-midnight-p

(ert-deftest ps/schedule-view--is-past-midnight-p/nil-when-not-set ()
  (let ((org-extend-today-until nil))
    (should-not (ps/schedule-view--is-past-midnight-p 6))))

(ert-deftest ps/schedule-view--is-past-midnight-p/nil-when-zero ()
  (let ((org-extend-today-until 0))
    (should-not (ps/schedule-view--is-past-midnight-p 6))))

(ert-deftest ps/schedule-view--is-past-midnight-p/nil-past-window ()
  ;; 05:00 (HHMM=500) is not in the window when today-until=4 (threshold=400).
  (let ((org-extend-today-until 4))
    (should-not (ps/schedule-view--is-past-midnight-p 500))))

(ert-deftest ps/schedule-view--is-past-midnight-p/t-in-window ()
  ;; 00:06 (HHMM=6) is inside the window (6 < 400).
  (let ((org-extend-today-until 4))
    (should (ps/schedule-view--is-past-midnight-p 6))))

;;; ------------------------------------------------------------------
;;; ps/schedule-view--cols

(ert-deftest ps/schedule-view--cols/shifts-by-prefix-width ()
  ;; :cat lands at left-margin + prefix-width + extra-margin (the schedule block
  ;; is indented by the extra margin on top of the time/bar prefix).
  (let* ((cols (ps/schedule-view--cols))
         (expected (+ ps/schedule-view--prefix-width
                      ps/agenda-layout-left-margin-cols
                      ps/schedule-view-extra-margin-cols)))
    (should (= expected (plist-get cols :cat)))))

;;; ------------------------------------------------------------------
;;; ps/schedule-view--now-line-str

(ert-deftest ps/schedule-view--now-line-str/contains-time ()
  (let ((s (ps/schedule-view--now-line-str 1347 80)))
    (should (string-match-p "13:47" s))))

(ert-deftest ps/schedule-view--now-line-str/bar-at-col ()
  ;; ┆ is at column (left-margin + extra-margin + time-col-width + 1).
  (let* ((s (ps/schedule-view--now-line-str 900 80))
         (plain (substring-no-properties s))
         (expected-col (+ ps/agenda-layout-left-margin-cols
                          ps/schedule-view-extra-margin-cols
                          (1+ ps/schedule-view--time-col-width))))
    (should (= ?┆ (aref plain expected-col)))))

(ert-deftest ps/schedule-view--now-line-str/narrow-window ()
  ;; Should not error even with a very narrow window.
  (should (stringp (ps/schedule-view--now-line-str 900 10))))

;;; ------------------------------------------------------------------
;;; ps/schedule-view--refresh-agenda

(ert-deftest ps/schedule-view--refresh-agenda/skips-when-no-window ()
  "Regression test: must not call `org-agenda-redo' (and must leave any other
buffer untouched) when the *Org Agenda* buffer has no live window -- doing
so previously corrupted whatever buffer the user had selected (see
`lisp/ps-window.el''s `ps/window--split-if-alone-advice')."
  (let ((agenda-buf (generate-new-buffer "*Org Agenda*"))
        (redo-calls 0))
    (unwind-protect
        (progn
          (with-current-buffer agenda-buf
            (setq-local ps/agenda-layout--view-kind 'agenda))
          (cl-letf (((symbol-function 'org-agenda-redo)
                     (lambda (&rest _) (cl-incf redo-calls))))
            (ps/schedule-view--refresh-agenda))
          (should (= redo-calls 0)))
      (kill-buffer agenda-buf))))

(ert-deftest ps/schedule-view--refresh-agenda/redoes-when-window-present ()
  "Calls `org-agenda-redo' when the Agenda view has a live window."
  (let ((agenda-buf (generate-new-buffer "*Org Agenda*"))
        (redo-calls 0))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (switch-to-buffer agenda-buf)
          (setq-local ps/agenda-layout--view-kind 'agenda)
          (cl-letf (((symbol-function 'org-agenda-redo)
                     (lambda (&rest _) (cl-incf redo-calls))))
            (ps/schedule-view--refresh-agenda))
          (should (= redo-calls 1)))
      (kill-buffer agenda-buf))))

(provide 'test-ps-schedule-view)
;;; test-ps-schedule-view.el ends here
