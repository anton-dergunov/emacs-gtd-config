;;; test-ps-freeze-log.el --- ERT tests for ps-freeze-log -*- lexical-binding: t; -*-

(require 'ert)
(add-to-list 'load-path "lisp")
(require 'ps-freeze-log)

;;; Timestamp formatting

(ert-deftest ps/freeze-log--timestamp-shape ()
  "Timestamp is `YYYY-MM-DD HH:MM:SS.mmm' for a known epoch time."
  ;; Use an explicit time so the test is clock-independent.  Compare only the
  ;; structural shape (local TZ affects the digits, not the format).
  (let ((s (ps/freeze-log--timestamp '(24000 0 0 0))))
    (should (string-match-p
             "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}\\.[0-9]\\{3\\}\\'"
             s))))

;;; Line formatting

(ert-deftest ps/freeze-log--format-line-basic ()
  "A line is `TS [category] message\\n'."
  (should (equal (ps/freeze-log--format-line "2026-07-16 21:00:00.123"
                                             'scrollbar "redisplay BEGIN")
                 "2026-07-16 21:00:00.123 [scrollbar] redisplay BEGIN\n")))

(ert-deftest ps/freeze-log--format-line-accepts-string-category ()
  (should (equal (ps/freeze-log--format-line "T" "focus" "x")
                 "T [focus] x\n")))

;;; Rotation decision (pure)

(ert-deftest ps/freeze-log--should-rotate ()
  (should (ps/freeze-log--should-rotate-p 101 100))
  (should-not (ps/freeze-log--should-rotate-p 100 100))
  (should-not (ps/freeze-log--should-rotate-p 50 100))
  ;; Non-positive / non-integer limits disable rotation.
  (should-not (ps/freeze-log--should-rotate-p 1000 0))
  (should-not (ps/freeze-log--should-rotate-p 1000 -1))
  (should-not (ps/freeze-log--should-rotate-p nil 100)))

;;; End-to-end append + marker (temp files)

(ert-deftest ps/freeze-log--writes-and-rotates ()
  "`ps/freeze-log' appends a line, and rotates once past the size cap."
  (let* ((dir (make-temp-file "ps-freeze-test" t))
         (ps/freeze-log-file (expand-file-name "log" dir))
         (ps/freeze-log-enabled t)
         (ps/freeze-log-max-bytes 200))
    (unwind-protect
        (progn
          (ps/freeze-log 'test "hello %d" 1)
          (should (file-exists-p ps/freeze-log-file))
          (with-temp-buffer
            (insert-file-contents ps/freeze-log-file)
            (should (string-match-p "\\[test\\] hello 1" (buffer-string))))
          ;; Grow past the cap, then the next write should rotate the old file
          ;; aside to `.1' and start fresh.
          (with-temp-file ps/freeze-log-file
            (insert (make-string 300 ?x)))
          (ps/freeze-log 'test "after-rotate")
          (should (file-exists-p (concat ps/freeze-log-file ".1")))
          (with-temp-buffer
            (insert-file-contents ps/freeze-log-file)
            (should (string-match-p "after-rotate" (buffer-string)))
            (should-not (string-match-p "xxxx" (buffer-string)))))
      (delete-directory dir t))))

(ert-deftest ps/freeze-log--op-marker-set-and-clear ()
  "`ps/freeze-log-op-begin' names the op; `-op-end' clears the marker."
  (let* ((dir (make-temp-file "ps-freeze-test" t))
         (ps/freeze-log-marker-file (expand-file-name "op" dir))
         (ps/freeze-log-enabled t))
    (unwind-protect
        (progn
          (ps/freeze-log-op-begin "risky-thing")
          (with-temp-buffer
            (insert-file-contents ps/freeze-log-marker-file)
            (should (string-match-p "risky-thing" (buffer-string))))
          (ps/freeze-log-op-end)
          (with-temp-buffer
            (insert-file-contents ps/freeze-log-marker-file)
            (should (equal (buffer-string) ""))))
      (delete-directory dir t))))

(ert-deftest ps/freeze-log--disabled-writes-nothing ()
  "With `ps/freeze-log-enabled' nil, nothing is written."
  (let* ((dir (make-temp-file "ps-freeze-test" t))
         (ps/freeze-log-file (expand-file-name "log" dir))
         (ps/freeze-log-enabled nil))
    (unwind-protect
        (progn
          (ps/freeze-log 'test "should not appear")
          (should-not (file-exists-p ps/freeze-log-file)))
      (delete-directory dir t))))

;;; Heartbeat scrollbar-state suffix

(ert-deftest ps/freeze-log--scrollbar-state-string-pure ()
  "Suffix formatting: absent module, first sample, and subsequent deltas."
  ;; Module not loaded (nil count) -> no suffix at all.
  (should (equal (ps/freeze-log--scrollbar-state-string nil nil) ""))
  (should (equal (ps/freeze-log--scrollbar-state-string nil 50) ""))
  ;; First sample -> count only, no delta.
  (should (equal (ps/freeze-log--scrollbar-state-string 100 nil) " sb-ticks=100"))
  ;; Subsequent sample -> delta against the previous count.
  (should (equal (ps/freeze-log--scrollbar-state-string 113 100)
                 " sb-ticks=113 (+13)"))
  ;; A stalled timer shows +0 -- the signature of a paused/torn-down tick,
  ;; which is what distinguishes "timer died" from "timer ran into the wedge".
  (should (equal (ps/freeze-log--scrollbar-state-string 113 113)
                 " sb-ticks=113 (+0)")))

(ert-deftest ps/freeze-log--scrollbar-state-tracks-last-count ()
  "The stateful wrapper records the previous count so deltas advance."
  (defvar ps/scrollbar--tick-count)
  (let ((ps/scrollbar--tick-count 100)
        (ps/freeze-log--last-tick-count nil))
    (should (equal (ps/freeze-log--scrollbar-state) " sb-ticks=100"))
    (setq ps/scrollbar--tick-count 113)
    (should (equal (ps/freeze-log--scrollbar-state) " sb-ticks=113 (+13)"))))

;;; Frame-count reporting (exposure multiplier)

(ert-deftest ps/freeze-log--frame-state-string-pure ()
  "Frame counts render as `frames=N child=M'."
  (should (equal (ps/freeze-log--frame-state-string '(1 . 0)) " frames=1 child=0"))
  (should (equal (ps/freeze-log--frame-state-string '(2 . 1)) " frames=2 child=1")))

(ert-deftest ps/freeze-log--frame-counts-separates-children ()
  "Child frames (those with a `parent-frame') are counted separately."
  (let ((counts (ps/freeze-log--frame-counts)))
    (should (consp counts))
    (should (integerp (car counts)))
    (should (integerp (cdr counts)))
    ;; Every live frame is classified as exactly one of the two.
    (should (= (+ (car counts) (cdr counts)) (length (frame-list))))
    ;; Batch mode still has the initial frame.
    (should (>= (car counts) 1))))

(ert-deftest ps/freeze-log--frame-change-skips-child-frames ()
  "Child-frame churn must not flood the log; only top-level frames log."
  (let* ((dir (make-temp-file "ps-freeze-test" t))
         (ps/freeze-log-file (expand-file-name "log" dir))
         (ps/freeze-log-enabled t))
    (unwind-protect
        (progn
          ;; A frame reporting a parent-frame is skipped entirely.
          (cl-letf (((symbol-function 'frame-parameter)
                     (lambda (&rest _) 'some-parent)))
            (ps/freeze-log--on-frame-change 'fake "created"))
          (should-not (file-exists-p ps/freeze-log-file))
          ;; A top-level frame (no parent) is logged.
          (cl-letf (((symbol-function 'frame-parameter)
                     (lambda (&rest _) nil)))
            (ps/freeze-log--on-frame-change 'fake "created"))
          (should (file-exists-p ps/freeze-log-file))
          (with-temp-buffer
            (insert-file-contents ps/freeze-log-file)
            (should (string-match-p "top-level frame created" (buffer-string)))))
      (delete-directory dir t))))

(ert-deftest ps/freeze-log--setup-teardown-manage-timer ()
  "Setup starts a heartbeat timer and focus hook; teardown removes both."
  (let ((ps/freeze-log-enabled t)
        (ps/freeze-log--heartbeat-timer nil)
        (ps/freeze-log-file (make-temp-file "ps-freeze-setup")))
    (unwind-protect
        (progn
          (ps/freeze-log-setup)
          (should (timerp ps/freeze-log--heartbeat-timer))
          (should (advice-function-member-p #'ps/freeze-log--on-focus-change
                                            after-focus-change-function))
          (ps/freeze-log-teardown)
          (should (null ps/freeze-log--heartbeat-timer))
          (should-not (advice-function-member-p #'ps/freeze-log--on-focus-change
                                                after-focus-change-function)))
      (ps/freeze-log-teardown)
      (ignore-errors (delete-file ps/freeze-log-file)))))

(provide 'test-ps-freeze-log)
;;; test-ps-freeze-log.el ends here
