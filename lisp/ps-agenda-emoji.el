;;; ps-agenda-emoji.el --- Append semantic emojis to org agenda tasks -*- lexical-binding: t; -*-

(require 'json)

;;; Customization

(defcustom ps/agenda-emoji-matcher-path
  (expand-file-name "scripts/org_emoji_matcher.py" user-emacs-directory)
  "Path to the org_emoji_matcher.py script."
  :type 'file)

;;; Internal state

(defvar ps/agenda-emoji--timer nil
  "Idle timer used to debounce emoji updates in the agenda.")

(defconst ps/agenda-emoji--task-re
  "^[[:space:]]*\\(?:[^:]+:\\)?[[:space:]]*\\(?:Scheduled:\\|Deadline:\\|In[[:space:]]+[0-9]+[[:space:]]+d.:\\)?[[:space:]]*\\(TODO\\|NEXT\\|IN-PROGRESS\\|WAITING\\|SOMEDAY\\)[[:space:]]+\\(.*\\)$"
  "Regexp matching a TODO-like task line in the agenda.
Group 1 is the keyword; group 2 is the task text.")

;;; Buffer scraping

(defun ps/agenda-emoji--extract-tasks ()
  "Extract all visible TODO-like task strings from the current buffer."
  (let (tasks)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward ps/agenda-emoji--task-re nil t)
        (let ((task (match-string 2)))
          (setq task (replace-regexp-in-string "\\s-+:[^:]+:$" "" task))
          (push task tasks))))
    (nreverse tasks)))

(defun ps/agenda-emoji--apply (emoji-map)
  "Append emojis from EMOJI-MAP (alist) to matching tasks in the current buffer."
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward ps/agenda-emoji--task-re nil t)
        (let* ((task (match-string 2))
               (emojis (alist-get task emoji-map nil nil #'string=)))
          (when emojis
            (end-of-line)
            (insert
             (concat " "
                     (mapconcat
                      (lambda (e)
                        (propertize e 'face '(:height 0.8)))
                      emojis " ")))))))))

;;; Async matcher process

(defun ps/agenda-emoji--run-matcher (tasks callback)
  "Run the Python emoji matcher on TASKS asynchronously, then call CALLBACK."
  (let* ((buf (generate-new-buffer " *emoji-matcher-output*"))
         (python-exec (or (executable-find "python3")
                          (executable-find "python")
                          "/usr/bin/python3")))
    (make-process
     :name "emoji-matcher-proc"
     :buffer buf
     :command (list python-exec ps/agenda-emoji-matcher-path)
     :noquery t
     :connection-type 'pipe
     :sentinel
     (lambda (p _event)
       (when (eq (process-status p) 'exit)
         (when (buffer-live-p (process-buffer p))
           (with-current-buffer (process-buffer p)
             (let* ((lines (split-string (string-trim (buffer-string)) "\n" t "[ \t\r]*"))
                    (last-line (car (last lines)))
                    (parsed (ignore-errors (json-read-from-string (or last-line "")))))
               (if parsed
                   (funcall callback parsed)
                 (message "[emoji-matcher] Failed to parse output:\n%s" last-line))))
           (kill-buffer (process-buffer p))))))
    (process-send-string "emoji-matcher-proc" (concat (json-encode tasks) "\n"))
    (process-send-eof "emoji-matcher-proc")))

;;; Agenda integration

(defun ps/agenda-emoji--update ()
  "Run the matcher and update the *Org Agenda* buffer with emojis."
  (when (get-buffer "*Org Agenda*")
    (with-current-buffer "*Org Agenda*"
      (let ((tasks (ps/agenda-emoji--extract-tasks)))
        (ps/agenda-emoji--run-matcher
         tasks
         (lambda (emoji-map)
           (with-current-buffer "*Org Agenda*"
             (ps/agenda-emoji--apply emoji-map))))))))

(defun ps/agenda-emoji--append (&rest _)
  "Debounce an emoji update after the agenda renders."
  (when ps/agenda-emoji--timer
    (cancel-timer ps/agenda-emoji--timer))
  (setq ps/agenda-emoji--timer
        (run-with-idle-timer
         0.4 nil
         (lambda ()
           (setq ps/agenda-emoji--timer nil)
           (ps/agenda-emoji--update)))))

;;; Public API

(defun ps/agenda-emoji-setup ()
  "Enable emoji decoration of agenda tasks after each agenda render."
  (add-hook 'org-agenda-finalize-hook #'ps/agenda-emoji--append))

(provide 'ps-agenda-emoji)
;;; ps-agenda-emoji.el ends here
