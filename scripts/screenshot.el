;;; screenshot.el --- Capture a PNG of the running frame -*- lexical-binding: t; -*-

;; Loaded by scripts/screenshot_theme.sh AFTER the normal config has finished
;; loading.  It opens the Super Agenda, waits for a redisplay, then exports the
;; current graphical frame to the file named in the PS_SCREENSHOT_OUT env var
;; using `x-export-frames' (PNG when the Emacs build has cairo, else SVG).
;;
;; This needs a graphical frame, so the launching script must NOT use -nw.

(defun ps/screenshot--capture ()
  "Export the selected frame to PS_SCREENSHOT_OUT and quit Emacs."
  (let* ((out (getenv "PS_SCREENSHOT_OUT"))
         (png (ignore-errors (x-export-frames nil 'png))))
    (cond
     ((and out png)
      (let ((coding-system-for-write 'binary))
        (with-temp-file out (insert png)))
      (message "Screenshot written: %s" out)
      (kill-emacs 0))
     (out
      ;; PNG unavailable (no cairo) — fall back to SVG next to the target.
      (let ((svg (ignore-errors (x-export-frames nil 'svg)))
            (alt (concat (file-name-sans-extension out) ".svg")))
        (if svg
            (progn
              (with-temp-file alt (insert svg))
              (message "PNG unavailable; wrote SVG: %s" alt)
              (kill-emacs 0))
          (message "Frame export unavailable in this Emacs build")
          (kill-emacs 1))))
     (t
      (message "PS_SCREENSHOT_OUT is not set")
      (kill-emacs 1)))))

(setq ps/git-sync-paused t)
(set-frame-size (selected-frame) 120 40)
(ignore-errors (org-agenda nil "c"))
(redisplay t)
;; Give org-modern / fontification a moment to settle, then capture.
(run-with-timer 2 nil #'ps/screenshot--capture)

;;; screenshot.el ends here
