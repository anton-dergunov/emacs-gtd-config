;;; ps-agenda-emoji.el --- Append semantic emojis to org agenda tasks -*- lexical-binding: t; -*-

;;; Commentary:
;; Decorates org-agenda task lines with a semantic emoji chosen by
;; `scripts/org_emoji_matcher.py'.  Task titles are read from org-agenda's own
;; per-line text properties (robust against every prefix format, including
;; overdue lines and titles containing colons).  Results are kept in a
;; persistent on-disk cache keyed by title, so a warm agenda render reuses
;; cached emojis and never spawns Python.  Emojis are shown as right-aligned
;; overlays, so they line up in a column and never modify the buffer text.

;;; Code:

(require 'json)

(declare-function org-get-at-bol "org" (property))
(declare-function org-get-heading "org" (&optional no-tags no-todo no-priority no-comment))
(defvar org-agenda-finalize-hook)

;;; Customization

(defcustom ps/agenda-emoji-enabled t
  "When non-nil, decorate agenda tasks with semantic emojis."
  :type 'boolean
  :group 'ps-agenda-emoji)

(defcustom ps/agenda-emoji-matcher-path
  (expand-file-name "scripts/org_emoji_matcher.py" user-emacs-directory)
  "Path to the org_emoji_matcher.py script."
  :type 'file
  :group 'ps-agenda-emoji)

(defcustom ps/agenda-emoji-cache-file
  (expand-file-name "emoji_matcher/task_cache.json"
                    (expand-file-name ".cache" "~"))
  "File holding the persistent title->emoji cache."
  :type 'file
  :group 'ps-agenda-emoji)

(defcustom ps/agenda-emoji-cache-tag "bge-small-en-v1.5@0.50"
  "Identity tag for cached emoji results.
Change this whenever the matcher model or threshold changes; a mismatch
invalidates the on-disk cache so emojis are recomputed."
  :type 'string
  :group 'ps-agenda-emoji)

(defcustom ps/agenda-emoji-right-margin 6
  "Column, counted from the right window edge, where the emoji column starts."
  :type 'integer
  :group 'ps-agenda-emoji)

(defcustom ps/agenda-emoji-face '(:height 0.8)
  "Face spec applied to the appended agenda emojis."
  :type 'sexp
  :group 'ps-agenda-emoji)

;;; Internal state

(defvar ps/agenda-emoji--timer nil
  "Idle timer used to debounce emoji updates in the agenda.")

(defvar ps/agenda-emoji--cache nil
  "In-memory hash table mapping task title -> list of emoji strings.
Nil until loaded from `ps/agenda-emoji-cache-file'.")

(defvar ps/agenda-emoji--cache-loaded-tag nil
  "Value of `ps/agenda-emoji-cache-tag' that `ps/agenda-emoji--cache' was built for.")

;;; Buffer scraping (org property based)

(defun ps/agenda-emoji--line-title ()
  "Return the clean heading title for the agenda line at point, or nil.
A line is a task line when it carries an `org-marker' text property; the title
is read from the heading itself, with TODO keyword, priority, tags and comment
stripped.  This is the only org-touching primitive in this file."
  (let ((marker (org-get-at-bol 'org-marker)))
    (when (and marker (marker-buffer marker))
      (let ((title (org-with-point-at marker (org-get-heading t t t t))))
        (when (and (stringp title) (> (length (string-trim title)) 0))
          (string-trim title))))))

(defun ps/agenda-emoji--collect-titles ()
  "Return the list of task titles on task lines of the current buffer."
  (let (titles)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((title (ps/agenda-emoji--line-title)))
          (when title (push title titles)))
        (forward-line 1)))
    (nreverse titles)))

;;; Persistent cache

(defun ps/agenda-emoji--map-get (map title)
  "Look TITLE up in MAP, which may be a hash table or a string-keyed alist."
  (if (hash-table-p map)
      (gethash title map)
    (alist-get title map nil nil #'equal)))

(defun ps/agenda-emoji--cache-load ()
  "Return the title->emoji cache hash table, loading it from disk on first use.
The on-disk cache is ignored when its tag does not match
`ps/agenda-emoji-cache-tag' (model/threshold change)."
  (when (and ps/agenda-emoji--cache
             (not (equal ps/agenda-emoji--cache-loaded-tag ps/agenda-emoji-cache-tag)))
    (setq ps/agenda-emoji--cache nil))
  (or ps/agenda-emoji--cache
      (setq ps/agenda-emoji--cache
            (let ((tbl (make-hash-table :test 'equal)))
              (setq ps/agenda-emoji--cache-loaded-tag ps/agenda-emoji-cache-tag)
              (when (file-exists-p ps/agenda-emoji-cache-file)
                (ignore-errors
                  (let* ((json-object-type 'hash-table)
                         (json-array-type 'list)
                         (json-key-type 'string)
                         (data (json-read-file ps/agenda-emoji-cache-file)))
                    (when (and (hash-table-p data)
                               (equal (gethash "tag" data) ps/agenda-emoji-cache-tag))
                      (let ((map (gethash "map" data)))
                        (when (hash-table-p map)
                          (maphash (lambda (k v) (puthash k v tbl)) map)))))))
              tbl))))

(defun ps/agenda-emoji--cache-save ()
  "Persist the in-memory cache to `ps/agenda-emoji-cache-file'."
  (when ps/agenda-emoji--cache
    (ignore-errors
      (make-directory (file-name-directory ps/agenda-emoji-cache-file) t)
      (let ((data (make-hash-table :test 'equal))
            (map (make-hash-table :test 'equal)))
        ;; Store emoji lists as vectors so an empty list round-trips as `[]'
        ;; rather than JSON null (which `json-encode' would emit for nil).
        (maphash (lambda (k v) (puthash k (vconcat v) map)) ps/agenda-emoji--cache)
        (puthash "tag" ps/agenda-emoji-cache-tag data)
        (puthash "map" map data)
        (with-temp-file ps/agenda-emoji-cache-file
          (insert (json-encode data)))))))

(defun ps/agenda-emoji--partition (titles)
  "Split TITLES against the cache.
Return a cons (CACHED . MISSING): CACHED is an alist title->emoji-list for
titles present in the cache, MISSING is the list of titles absent from it."
  (let ((cache (ps/agenda-emoji--cache-load))
        cached missing)
    (dolist (title (delete-dups (copy-sequence titles)))
      (if (eq (gethash title cache 'ps/none) 'ps/none)
          (push title missing)
        (push (cons title (gethash title cache)) cached)))
    (cons (nreverse cached) (nreverse missing))))

(defun ps/agenda-emoji--cache-put (title emojis)
  "Store EMOJIS (a list, possibly empty) for TITLE in the cache."
  (puthash title (append emojis nil) (ps/agenda-emoji--cache-load)))

;;; Display (right-aligned overlays)

(defun ps/agenda-emoji--clear-overlays ()
  "Remove emoji overlays previously placed in the current buffer."
  (remove-overlays (point-min) (point-max) 'ps/agenda-emoji t))

(defun ps/agenda-emoji--apply (emoji-map)
  "Draw right-aligned emoji overlays for EMOJI-MAP on the current buffer.
EMOJI-MAP maps a title to its list of emoji strings (hash table or alist).
Existing emoji overlays are cleared first, so this is idempotent."
  (ps/agenda-emoji--clear-overlays)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (let ((title (ps/agenda-emoji--line-title)))
        (when title
          (let ((emojis (ps/agenda-emoji--map-get emoji-map title)))
            (when emojis
              (let* ((eol (line-end-position))
                     (ov (make-overlay eol eol))
                     (glyphs (mapconcat
                              (lambda (e) (propertize e 'face ps/agenda-emoji-face))
                              emojis " "))
                     (spacer (propertize
                              " " 'display
                              `(space :align-to (- right ,ps/agenda-emoji-right-margin)))))
                (overlay-put ov 'ps/agenda-emoji t)
                (overlay-put ov 'after-string (concat spacer glyphs)))))))
      (forward-line 1))))

;;; Async matcher process

(defun ps/agenda-emoji--run-matcher (tasks callback)
  "Run the Python emoji matcher on TASKS asynchronously, then call CALLBACK.
CALLBACK receives a hash table mapping each task title to its emoji list."
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
                    (json-object-type 'hash-table)
                    (json-array-type 'list)
                    (json-key-type 'string)
                    (parsed (ignore-errors (json-read-from-string (or last-line "")))))
               (if (hash-table-p parsed)
                   (funcall callback parsed)
                 (message "[emoji-matcher] Failed to parse output:\n%s" last-line))))
           (kill-buffer (process-buffer p))))))
    (process-send-string "emoji-matcher-proc" (concat (json-encode tasks) "\n"))
    (process-send-eof "emoji-matcher-proc")))

;;; Agenda integration

(defun ps/agenda-emoji--update ()
  "Update the *Org Agenda* buffer with emojis, using the cache then Python."
  (when (get-buffer "*Org Agenda*")
    (with-current-buffer "*Org Agenda*"
      (let* ((titles (ps/agenda-emoji--collect-titles))
             (part (ps/agenda-emoji--partition titles))
             (cached (car part))
             (missing (cdr part)))
        ;; Draw what we already know immediately; if everything is cached we
        ;; never spawn Python.
        (ps/agenda-emoji--apply cached)
        (when missing
          (ps/agenda-emoji--run-matcher
           missing
           (lambda (emoji-map)
             (dolist (title missing)
               (ps/agenda-emoji--cache-put
                title (ps/agenda-emoji--map-get emoji-map title)))
             (ps/agenda-emoji--cache-save)
             (when (get-buffer "*Org Agenda*")
               (with-current-buffer "*Org Agenda*"
                 (ps/agenda-emoji--apply (ps/agenda-emoji--cache-load)))))))))))

(defun ps/agenda-emoji--append (&rest _)
  "Debounce an emoji update after the agenda renders, when enabled."
  (when ps/agenda-emoji-enabled
    (when ps/agenda-emoji--timer
      (cancel-timer ps/agenda-emoji--timer))
    (setq ps/agenda-emoji--timer
          (run-with-idle-timer
           0.4 nil
           (lambda ()
             (setq ps/agenda-emoji--timer nil)
             (ps/agenda-emoji--update))))))

;;; Public API

(defun ps/agenda-emoji-setup ()
  "Enable emoji decoration of agenda tasks after each agenda render."
  (add-hook 'org-agenda-finalize-hook #'ps/agenda-emoji--append))

(provide 'ps-agenda-emoji)
;;; ps-agenda-emoji.el ends here
