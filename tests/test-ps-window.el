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
;;; ps/window--current-buffer-visible-p
;;; -------------------------------------------------------

(ert-deftest ps/window--current-buffer-visible-p-true-when-displayed ()
  "True for a buffer actually shown in a window (real interactive navigation)."
  (let ((buf (generate-new-buffer "a")))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (switch-to-buffer buf)
          (should (ps/window--current-buffer-visible-p)))
      (kill-buffer buf))))

(ert-deftest ps/window--current-buffer-visible-p-false-when-windowless ()
  "False for a buffer pinned via `with-current-buffer' that has no window --
the exact signature of a background `org-agenda-redo' on a closed Agenda."
  (let ((buf (generate-new-buffer "a")))
    (unwind-protect
        (with-current-buffer buf
          (should-not (ps/window--current-buffer-visible-p)))
      (kill-buffer buf))))

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
          ;; `switch-to-buffer' (not `set-window-buffer') so `current-buffer'
          ;; is actually synced to the window, like real interactive
          ;; navigation -- required for `ps/window--current-buffer-visible-p'.
          (switch-to-buffer buf-a)
          (let ((win-a (selected-window)))
            (ps/window-show-here buf-new)
            (should (= (length (ps/window--content-windows)) 2))
            (should (eq (window-buffer win-a) buf-a))
            (should (member buf-new (mapcar #'window-buffer (ps/window--content-windows))))))
      (mapc #'kill-buffer (list buf-a buf-new)))))

(ert-deftest ps/window-show-here-never-takes-over-a-side-window ()
  "Regression test: pressing a view key while point is in the file tree used
to replace the tree with the view, inside the tree's own narrow slot -- a side
window is dedicated with the value `side', not t, and `switch-to-buffer'
refuses only the latter.  On screen the command looked like it had done
nothing at all, and the next invocation, now with a content window selected,
worked.  The view must land in a content window and leave the side window's
buffer alone."
  (let ((buf-tree (generate-new-buffer "tree"))
        (buf-main (generate-new-buffer "main"))
        (buf-new (generate-new-buffer "new")))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (switch-to-buffer buf-main)
          (let ((win-main (selected-window))
                (win-side (split-window)))
            (unwind-protect
                (progn
                  (set-window-buffer win-side buf-tree)
                  (set-window-parameter win-side 'window-side 'left)
                  (select-window win-side)
                  (ps/window-show-here buf-new)
                  (should (eq (window-buffer win-side) buf-tree))
                  (should (eq (window-buffer win-main) buf-new))
                  (should-not (ps/window--side-window-p (selected-window))))
              (set-window-parameter win-side 'window-side nil))))
      (mapc #'kill-buffer (list buf-tree buf-main buf-new)))))

(ert-deftest ps/window--select-main-leaves-a-content-window-selected ()
  "Selecting from a side window moves to a content window; from a content
window it changes nothing."
  (let ((buf-main (generate-new-buffer "main")))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (switch-to-buffer buf-main)
          (let ((win-main (selected-window))
                (win-side (split-window)))
            (unwind-protect
                (progn
                  (set-window-parameter win-side 'window-side 'left)
                  (select-window win-side)
                  (should (eq (ps/window--select-main) win-main))
                  (should (eq (selected-window) win-main))
                  ;; Already on a content window: a no-op.
                  (should (eq (ps/window--select-main) win-main)))
              (set-window-parameter win-side 'window-side nil))))
      (kill-buffer buf-main))))

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
          (switch-to-buffer buf-a)
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

(ert-deftest ps/window--split-if-alone-advice-noop-when-buffer-windowless ()
  "Regression test: a background `org-agenda-redo' pinning a windowless
buffer via `with-current-buffer' must not split/select a window -- doing so
previously corrupted whatever buffer the user was actually looking at (see
`ps/schedule-view--refresh-agenda').  The wrapped function still runs; only
the window manipulation is skipped."
  (let ((agenda-buf (generate-new-buffer "*fake agenda*"))
        (other-buf (generate-new-buffer "other"))
        (called nil))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (switch-to-buffer other-buf)
          (let ((other-win (selected-window))
                (other-size (with-current-buffer other-buf (point-max))))
            (with-current-buffer agenda-buf
              (ps/window--split-if-alone-advice (lambda (&rest _) (setq called t))))
            (should called)
            ;; No new window, and the buffer the user was looking at is untouched.
            (should (= (length (ps/window--content-windows)) 1))
            (should (eq (window-buffer other-win) other-buf))
            (should (= (with-current-buffer other-buf (point-max)) other-size))))
      (mapc #'kill-buffer (list agenda-buf other-buf)))))

(ert-deftest ps/window--split-if-alone-advice-noop-while-inhibited ()
  "Regression test: with `ps/window-inhibit-split' bound (an agenda redo is
in progress) the view is rebuilt in the window it already occupies -- no
second window appears.  The wrapped function still runs."
  (let ((buf-a (generate-new-buffer "a"))
        (split-width-threshold 0)
        (split-height-threshold 0)
        (called nil))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (switch-to-buffer buf-a)
          (let ((ps/window-inhibit-split t))
            (ps/window--split-if-alone-advice (lambda (&rest _) (setq called t))))
          (should called)
          (should (= (length (ps/window--content-windows)) 1)))
      (kill-buffer buf-a))))

(ert-deftest ps/window--inhibit-split-advice-binds-the-flag ()
  "The redo wrapper binds `ps/window-inhibit-split' for the wrapped call only."
  (let (seen)
    (should-not ps/window-inhibit-split)
    (ps/window--inhibit-split-advice
     (lambda (&rest _) (setq seen ps/window-inhibit-split)))
    (should seen)
    (should-not ps/window-inhibit-split)))

(ert-deftest ps/window--split-if-alone-advice-noop-inside-a-block-agenda ()
  "Regression test: the `todo'/`tags-todo' blocks of a block agenda are run
through `org-todo-list', which is advised to split when alone.  While
`org-agenda-multi' is set the series is mid-assembly, so those blocks must
not each split off another window."
  (let ((buf-a (generate-new-buffer "a"))
        (split-width-threshold 0)
        (split-height-threshold 0)
        (called nil))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (switch-to-buffer buf-a)
          (let ((org-agenda-multi t))
            (ps/window--split-if-alone-advice (lambda (&rest _) (setq called t))))
          (should called)
          (should (= (length (ps/window--content-windows)) 1)))
      (kill-buffer buf-a))))

(provide 'test-ps-window)
;;; test-ps-window.el ends here
