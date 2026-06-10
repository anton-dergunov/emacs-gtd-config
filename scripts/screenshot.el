;;; screenshot.el --- Capture a PNG of the running frame -*- lexical-binding: t; -*-

;; Loaded by scripts/screenshot_theme.sh AFTER the normal config has finished
;; loading.  It builds a two-window layout (a chosen org file on top, the Super
;; Agenda below, split evenly), waits for a redisplay, then captures the
;; current graphical frame to the file named in the PS_SCREENSHOT_OUT env var
;; using the macOS `screencapture' tool against the frame's outer edges
;; (`frame-edges'), so the whole Emacs window including the title bar is saved.
;;
;; If PS_SCREENSHOT_FAKE_DATE is set (e.g. "2026-05-21 10:00"), Emacs/Org is
;; tricked into believing "now" is that time for this session, so the fixed
;; SCHEDULED/DEADLINE dates in samples/realistic/ show up as overdue/today/
;; upcoming as intended without editing the sample files.
;;
;; This is macOS-only and needs a graphical frame, so the launching script must
;; NOT use -nw.  The controlling terminal must have Screen Recording permission,
;; otherwise `screencapture' produces a blank/desktop-only image. It also needs
;; Accessibility/Automation permission (to control "System Events"), used here
;; to bring this Emacs window to the front before capturing.

(defun ps/screenshot--capture ()
  "Capture the selected frame to PS_SCREENSHOT_OUT via `screencapture'."
  (let ((out (getenv "PS_SCREENSHOT_OUT")))
    (unless out
      (message "PS_SCREENSHOT_OUT is not set")
      (kill-emacs 1))
    ;; Make sure the frame is frontmost and fully drawn before the capture.
    ;; `raise-frame' alone isn't enough on macOS when another Emacs window is
    ;; already on screen: `screencapture -R' grabs whatever is visually on top
    ;; at those coordinates, so explicitly activate this process by PID.
    (raise-frame)
    (select-frame-set-input-focus (selected-frame))
    (call-process "osascript" nil nil nil "-e"
                  (format "tell application \"System Events\" to set frontmost of (first process whose unix id is %d) to true"
                          (emacs-pid)))
    (redisplay t)
    (pcase-let ((`(,left ,top ,right ,bottom) (frame-edges nil 'outer-edges)))
      (let* ((region (format "%d,%d,%d,%d" left top (- right left) (- bottom top)))
             (status (call-process "screencapture" nil nil nil
                                   "-x" "-o" (concat "-R" region) out)))
        (if (and (eq status 0) (file-exists-p out))
            (progn (message "Screenshot written: %s" out)
                   (kill-emacs 0))
          (message "screencapture failed (status %S) for %s" status out)
          (kill-emacs 1))))))

(setq ps/git-sync-paused t)
(set-frame-size (selected-frame) 120 40)

;; Optional time travel: if PS_SCREENSHOT_FAKE_DATE is set, make Org believe
;; "today" is that date for this session, so the fixed dates in
;; samples/realistic/ show up as overdue/today/upcoming as intended.
;; `org-today' is the single source of truth org-agenda uses for both the
;; agenda's start date and all today/overdue comparisons (see
;; `org-agenda-list' and `org-agenda-today-p'), so overriding it alone
;; suffices -- no need to fake `current-time' itself.
(when-let ((fake-date (getenv "PS_SCREENSHOT_FAKE_DATE")))
  (let ((fake-day (time-to-days (date-to-time fake-date))))
    (advice-add 'org-today :override (lambda () fake-day))))

(defvar ps/screenshot-org-file "Plans/Career.org"
  "Org file (relative to `my-org-base-directory') shown in the top window.")

;; Build a deterministic two-window layout: chosen org file on top, agenda
;; below, split evenly. `org-agenda-window-setup' must be 'current-window so
;; org-agenda doesn't reorganize/replace this layout (its default does).
(setq org-agenda-window-setup 'current-window)
(delete-other-windows)
(find-file (expand-file-name ps/screenshot-org-file my-org-base-directory))
(let ((bottom (split-window-below)))
  (select-window bottom)
  (ignore-errors (org-agenda nil "c")))
(balance-windows)
(redisplay t)
;; Give org-modern / fontification a moment to settle, then capture.
(run-with-timer 2 nil #'ps/screenshot--capture)

;;; screenshot.el ends here
