;;; ps-schedule-icons.el --- Replace SCHEDULED/DEADLINE labels with icons -*- lexical-binding: t; -*-

;;; Customization

(defcustom ps/schedule-icons-scheduled "⏱ "
  "Icon used to visually replace the SCHEDULED: keyword."
  :type 'string)

(defcustom ps/schedule-icons-deadline "⚑ "
  "Icon used to visually replace the DEADLINE: keyword."
  :type 'string)

(defcustom ps/schedule-icons-face '(:foreground "#000000" :weight bold)
  "Face spec applied to the SCHEDULED:/DEADLINE: icons.
The DONE-fade overlay will override this color for completed tasks."
  :type 'sexp)

;;; Font-lock

(defun ps/schedule-icons--compose (limit)
  "Visually replace SCHEDULED: and DEADLINE: up to LIMIT with icons."
  ;; 'while' ensures every instance in the buffer is caught
  (while (re-search-forward "\\(SCHEDULED:\\|DEADLINE:\\)" limit t)
    (let* ((start (match-beginning 1))
           (end (match-end 1))
           (label (match-string 1))
           (icon (if (string= label "SCHEDULED:")
                     ps/schedule-icons-scheduled
                   ps/schedule-icons-deadline)))
      (add-text-properties start end
                           `(display ,icon
                                     face ,ps/schedule-icons-face))))
  nil)

(defun ps/schedule-icons-enable ()
  "Register the SCHEDULED:/DEADLINE: icon composition in the current buffer.
The keyword is appended (APPEND=t) so it runs after Org's own font-lock and
its `face' wins over `org-special-keyword'."
  (font-lock-add-keywords
   nil
   '((ps/schedule-icons--compose 0 t))
   t))

(provide 'ps-schedule-icons)
;;; ps-schedule-icons.el ends here
