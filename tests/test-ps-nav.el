;;; test-ps-nav.el --- ERT tests for ps-nav -*- lexical-binding: t; -*-

;; Window-parameter side effects: `window-side' does more than tag a single
;; window -- Emacs tracks side windows at the frame level, so a frame must never
;; be collapsed down to ONLY a side window, and the parameter must be cleared
;; again before the test ends, or later tests in this same batch process see a
;; corrupted frame.  The tests below that touch it keep a non-side window alive
;; throughout and clear the parameter in an `unwind-protect'.

(require 'ert)
(add-to-list 'load-path "lisp")
(require 'ps-window)
(require 'ps-nav)

;;; -------------------------------------------------------
;;; ps/nav--push -- the stack, as pure list surgery
;;; -------------------------------------------------------

(ert-deftest ps/nav--push-collapses-a-repeat-of-the-same-target ()
  "Re-rendering the agenda in place, or reverting a file, is not a step in a
trail -- and without this, pressing back once after a revert goes nowhere."
  (let ((places (ps/nav--push '("/a.org" . 10) '(("/a.org" . 3) ("/b.org" . 1)))))
    (should (equal places '(("/a.org" . 10) ("/b.org" . 1))))))

(ert-deftest ps/nav--push-keeps-a-different-target ()
  (should (equal (ps/nav--push '("/b.org" . 1) '(("/a.org" . 3)))
                 '(("/b.org" . 1) ("/a.org" . 3)))))

(ert-deftest ps/nav--push-caps-the-history ()
  "An unbounded history in a window parameter is a leak that survives every
buffer it names."
  (let ((ps/nav-history-limit 3)
        (places nil))
    (dotimes (i 10)
      (setq places (ps/nav--push (cons (format "/%d.org" i) 1) places)))
    (should (= (length places) 3))
    (should (equal (car places) '("/9.org" . 1)))))

(ert-deftest ps/nav--push-ignores-a-nil-place ()
  "An untrackable buffer contributes nothing rather than a nil entry that
every later reader would have to guard against."
  (should (equal (ps/nav--push nil '(("/a.org" . 1))) '(("/a.org" . 1)))))

;;; -------------------------------------------------------
;;; ps/nav--place -- what is remembered, and why by name
;;; -------------------------------------------------------

(ert-deftest ps/nav--place-remembers-a-directory-by-path ()
  "Dired buffers are killed as you descend out of them
\(`dired-kill-when-opening-new-dired-buffer'), so a place naming the buffer
would be unreachable by the time back is pressed."
  (let ((buffer (dired-noselect temporary-file-directory)))
    (unwind-protect
        (let ((place (ps/nav--place buffer)))
          (should (stringp (car place)))
          (should (equal (expand-file-name (car place))
                         (expand-file-name temporary-file-directory))))
      (kill-buffer buffer))))

(ert-deftest ps/nav--place-falls-back-to-the-buffer-itself ()
  "A generated planning view has no file behind it and must still be a place."
  (with-temp-buffer
    (rename-buffer "*Availability*" t)
    (let ((place (ps/nav--place (current-buffer))))
      (should (bufferp (car place))))))

(ert-deftest ps/nav--place-refuses-a-transient-buffer ()
  (with-temp-buffer
    (rename-buffer " *hidden work*" t)
    (should-not (ps/nav--place (current-buffer)))))

(ert-deftest ps/nav--reachable-p-drops-a-place-whose-file-is-gone ()
  "Dropping an item deletes its directory, and its place must not strand back."
  (should-not (ps/nav--reachable-p '("/nowhere/at/all/index.md" . 1)))
  (should (ps/nav--reachable-p (cons temporary-file-directory 1))))

;;; -------------------------------------------------------
;;; ps/nav--trackable-window-p
;;; -------------------------------------------------------

(ert-deftest ps/nav--trackable-window-p-excludes-a-side-window ()
  "A dock's buffer is the point of it, so there is nothing to go back to."
  (let* ((main (selected-window))
         (side (split-window main)))
    (unwind-protect
        (progn
          (set-window-parameter side 'window-side 'right)
          (should (ps/nav--trackable-window-p main))
          (should-not (ps/nav--trackable-window-p side)))
      (set-window-parameter side 'window-side nil)
      (delete-window side))))

;;; -------------------------------------------------------
;;; The mode-line segment
;;; -------------------------------------------------------

(ert-deftest ps/nav--button-is-inert-when-that-direction-is-empty ()
  "Two buttons that sometimes silently do nothing are worse than one greyed
pair that says which way you can actually go."
  (set-window-parameter (selected-window) 'ps/nav-back nil)
  (let ((glyph (ps/nav--button 'back)))
    (should (eq (get-text-property 0 'face glyph) 'shadow))
    (should-not (get-text-property 0 'local-map glyph))))

(ert-deftest ps/nav--button-is-live-and-named-when-there-is-somewhere-to-go ()
  (unwind-protect
      (progn
        (set-window-parameter (selected-window) 'ps/nav-back
                              (list (cons temporary-file-directory 1)))
        (let ((glyph (ps/nav--button 'back)))
          (should (get-text-property 0 'local-map glyph))
          (should (string-match-p "mouse-1: back to"
                                  (get-text-property 0 'help-echo glyph)))))
    (set-window-parameter (selected-window) 'ps/nav-back nil)))

(ert-deftest ps/nav-mode-line-add-is-idempotent ()
  "Reloading config.org must not stack a second pair of arrows."
  (let* ((once (ps/nav-mode-line-add '("%b")))
         (twice (ps/nav-mode-line-add once)))
    (should (equal once twice))
    (should (eq (car once) ps/nav-mode-line-element))))

(ert-deftest ps/nav-mode-line-add-accepts-a-non-list-format ()
  "`mode-line-format' is allowed to be a bare string."
  (should (equal (ps/nav-mode-line-add "%b")
                 (list ps/nav-mode-line-element "%b"))))

;;; -------------------------------------------------------
;;; Recording and moving
;;; -------------------------------------------------------

(ert-deftest ps/nav--record-pushes-the-outgoing-buffer-and-clears-forward ()
  "Going somewhere new abandons the forward trail, as in a browser."
  (let* ((window (selected-window))
         (first (find-file-noselect (expand-file-name "lisp/ps-nav.el")))
         (second (find-file-noselect (expand-file-name "lisp/ps-open.el"))))
    (unwind-protect
        (progn
          (set-window-parameter window 'ps/nav-back nil)
          (set-window-parameter window 'ps/nav-forward (list '("/x.org" . 1)))
          (set-window-buffer window first)
          (set-window-parameter window 'ps/nav--current first)
          (set-window-buffer window second)
          (ps/nav--record window)
          (should (equal (car (car (ps/nav--stack window 'back)))
                         (buffer-file-name first)))
          (should-not (ps/nav--stack window 'forward)))
      (set-window-parameter window 'ps/nav-back nil)
      (set-window-parameter window 'ps/nav-forward nil)
      (set-window-parameter window 'ps/nav--current nil)
      (kill-buffer first)
      (kill-buffer second))))

(ert-deftest ps/nav-back-returns-and-leaves-a-way-forward ()
  (let* ((window (selected-window))
         (origin (current-buffer))
         (target (expand-file-name "lisp/ps-nav.el"))
         (visited (find-file-noselect target)))
    (unwind-protect
        (progn
          (set-window-buffer window visited)
          (set-window-parameter window 'ps/nav--current visited)
          (set-window-parameter window 'ps/nav-back (list (cons target 1)))
          (set-window-parameter window 'ps/nav-forward nil)
          (ps/nav-back)
          (should (equal (buffer-file-name (window-buffer window)) target))
          (should-not (ps/nav--stack window 'back))
          (should (ps/nav--stack window 'forward))
          ;; The arrival is claimed here rather than left to the recorder, which
          ;; runs at redisplay and would otherwise push this step straight back.
          (should (eq (window-parameter window 'ps/nav--current)
                      (window-buffer window))))
      (set-window-parameter window 'ps/nav-back nil)
      (set-window-parameter window 'ps/nav-forward nil)
      (set-window-parameter window 'ps/nav--current nil)
      (set-window-buffer window origin)
      (kill-buffer visited))))

(ert-deftest ps/nav-note-departure-records-without-waiting-for-redisplay ()
  "`window-buffer-change-functions' only runs when Emacs next draws, so the
navigation helpers record as they go instead of depending on that timing."
  (let* ((window (selected-window))
         (visited (find-file-noselect (expand-file-name "lisp/ps-nav.el"))))
    (unwind-protect
        (progn
          (set-window-buffer window visited)
          (set-window-parameter window 'ps/nav-back nil)
          (set-window-parameter window 'ps/nav-forward (list '("/x.org" . 1)))
          (ps/nav-note-departure window)
          (should (equal (car (car (ps/nav--stack window 'back)))
                         (buffer-file-name visited)))
          (should-not (ps/nav--stack window 'forward)))
      (set-window-parameter window 'ps/nav-back nil)
      (set-window-parameter window 'ps/nav-forward nil)
      (kill-buffer visited))))

(ert-deftest ps/nav-note-departure-is-silent-while-moving ()
  "`ps/nav--go' restores through the same helpers a click uses, so without
this the step it just popped would be pushed straight back on."
  (let ((window (selected-window)))
    (unwind-protect
        (progn
          (set-window-parameter window 'ps/nav-back nil)
          (let ((ps/nav--in-transit t))
            (ps/nav-note-departure window))
          (should-not (ps/nav--stack window 'back)))
      (set-window-parameter window 'ps/nav-back nil))))

(ert-deftest ps/nav-back-says-so-when-there-is-nowhere-to-go ()
  (set-window-parameter (selected-window) 'ps/nav-back nil)
  (should-error (ps/nav-back) :type 'user-error))

;;; test-ps-nav.el ends here
