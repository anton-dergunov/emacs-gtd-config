;;; test-ps-window.el --- ERT tests for ps-window -*- lexical-binding: t; -*-

(require 'ert)
(add-to-list 'load-path "lisp")
(require 'ps-window)

;; Window-parameter side effects: `window-side' does more than tag a single
;; window -- Emacs tracks side windows at the frame level, so a frame must
;; never be collapsed down to ONLY a side window (even transiently), and the
;; parameter must be cleared again before the test ends, or later tests in
;; this same batch process see a corrupted frame ("Cannot make side window
;; the only window").  Every test below keeps at least one non-side window
;; alive at all times and clears `window-side' in an `unwind-protect'.

;;; -------------------------------------------------------
;;; ps/window--side-window-p / ps/window--content-windows / ps/window--alone-p
;;; -------------------------------------------------------

(ert-deftest ps/window--side-window-p-detects-window-side-parameter ()
  "A window with a `window-side' parameter is a side window; a plain one isn't."
  (save-window-excursion
    (delete-other-windows)
    (let ((side-win (split-window)))
      (unwind-protect
          (progn
            (should-not (ps/window--side-window-p side-win))
            (set-window-parameter side-win 'window-side 'left)
            (should (ps/window--side-window-p side-win)))
        (set-window-parameter side-win 'window-side nil)))))

(ert-deftest ps/window--content-windows-excludes-side-windows ()
  "Side windows (file tree, Claude Code) are excluded from the content list."
  (save-window-excursion
    (delete-other-windows)
    (let* ((content-win (selected-window))
           (side-win (split-window)))
      (unwind-protect
          (progn
            (set-window-parameter side-win 'window-side 'left)
            (should (equal (ps/window--content-windows) (list content-win))))
        (set-window-parameter side-win 'window-side nil)))))

(ert-deftest ps/window--alone-p-true-with-one-content-window ()
  "Alone means exactly one content window, regardless of side windows present."
  (save-window-excursion
    (delete-other-windows)
    (let* ((content-win (selected-window))
           (side-win (split-window)))
      (unwind-protect
          (progn
            (set-window-parameter side-win 'window-side 'left)
            (select-window content-win)
            (should (ps/window--alone-p)))
        (set-window-parameter side-win 'window-side nil)))))

(ert-deftest ps/window--alone-p-false-with-two-content-windows ()
  "Not alone when two (or more) content windows are open."
  (save-window-excursion
    (delete-other-windows)
    (split-window)
    (should-not (ps/window--alone-p))))

;;; -------------------------------------------------------
;;; ps/window-show-here
;;; -------------------------------------------------------

(ert-deftest ps/window-show-here-takes-over-selected-window-when-not-alone ()
  "With 2+ content windows, only the selected one is replaced; the other is untouched."
  (let ((buf-a (generate-new-buffer "a"))
        (buf-b (generate-new-buffer "b"))
        (buf-new (generate-new-buffer "new")))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (set-window-buffer (selected-window) buf-a)
          (let ((win-a (selected-window))
                (win-b (split-window)))
            (set-window-buffer win-b buf-b)
            (select-window win-a)
            (ps/window-show-here buf-new)
            (should (eq (window-buffer win-a) buf-new))
            (should (eq (window-buffer win-b) buf-b))))
      (mapc #'kill-buffer (list buf-a buf-b buf-new)))))

(ert-deftest ps/window-show-here-splits-when-alone ()
  "With exactly one content window, it splits first so the original buffer
stays visible alongside the new one."
  (let ((buf-a (generate-new-buffer "a"))
        (buf-new (generate-new-buffer "new"))
        (split-width-threshold 0)
        (split-height-threshold 0))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (set-window-buffer (selected-window) buf-a)
          (let ((win-a (selected-window)))
            (ps/window-show-here buf-new)
            (should (= (length (ps/window--content-windows)) 2))
            (should (eq (window-buffer win-a) buf-a))
            (should (member buf-new (mapcar #'window-buffer (ps/window--content-windows))))))
      (mapc #'kill-buffer (list buf-a buf-new)))))

;;; -------------------------------------------------------
;;; ps/window--split-if-alone-advice
;;; -------------------------------------------------------

(ert-deftest ps/window--split-if-alone-advice-splits-when-alone ()
  "Splits first, then calls the wrapped function, when alone."
  (let ((buf-a (generate-new-buffer "a"))
        (split-width-threshold 0)
        (split-height-threshold 0)
        (called nil))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (set-window-buffer (selected-window) buf-a)
          (ps/window--split-if-alone-advice (lambda (&rest _) (setq called t)))
          (should called)
          (should (= (length (ps/window--content-windows)) 2)))
      (kill-buffer buf-a))))

(ert-deftest ps/window--split-if-alone-advice-noop-when-not-alone ()
  "Does not split when there are already 2+ content windows."
  (let ((called nil))
    (save-window-excursion
      (delete-other-windows)
      (split-window)
      (ps/window--split-if-alone-advice (lambda (&rest _) (setq called t)))
      (should called)
      (should (= (length (ps/window--content-windows)) 2)))))

(provide 'test-ps-window)
;;; test-ps-window.el ends here
