;;; ps-links.el --- Org link management (Obsidian + async URL insertion) -*- lexical-binding: t; -*-

(require 'subr-x)
(require 'url)
(require 'ps-utils)

;;; Customization

(defcustom ps/obsidian-link-icon "✎ "
  "Display icon for obsidian: link prefixes."
  :type 'string)

;;; Obsidian link type

(defun ps/obsidian--open-note (path)
  "Open the Obsidian note at PATH using the Obsidian URL scheme."
  (let ((vault "obsidian"))
    (browse-url
     (format "obsidian://open?vault=%s&file=%s"
             vault
             (url-hexify-string path)))))

(defun ps/org-compose-obsidian-prefix (limit)
  "Replace all visible `obsidian:` prefixes with icon composition."
  (while (re-search-forward "\\(\\bobsidian:\\)" limit t)
    (let ((start (match-beginning 1))
          (end   (match-end 1)))
      ;; Remove any stale composition first
      (remove-text-properties start end '(composition nil))
      ;; Apply fresh composition
      (compose-region start end ps/obsidian-link-icon)))
  nil)

(defun ps/org-enable-obsidian-links ()
  "Register the obsidian: link composition rule in the current buffer."
  (font-lock-add-keywords
   nil
   '((ps/org-compose-obsidian-prefix 0 t))))

;;; Obsidian link insertion

(defun ps/insert-obsidian-link-from-clipboard ()
  "Insert [[obsidian:<title>]]. Title is taken from system clipboard."
  (interactive)
  (let* ((clip (string-trim (or (gui-get-selection 'CLIPBOARD)
                                (gui-get-selection 'PRIMARY)
                                (when (executable-find "pbpaste")
                                  (with-temp-buffer (call-process "pbpaste" nil t) (buffer-string)))
                                ""))))
    (if (string-empty-p clip)
        (user-error "Clipboard is empty — cannot form Obsidian link")
      (insert (format "[[obsidian:%s]]" clip)))))

(defun ps/insert-obsidian-link-prompt ()
  "Ask user for Obsidian note title in minibuffer, then insert [[obsidian:<title>]]."
  (interactive)
  (let ((title (read-string "Obsidian note title: ")))
    (unless (string-empty-p (string-trim title))
      (insert (format "[[obsidian:%s]]" title)))))

;;; Async URL link insertion

;; org-appear internals, referenced defensively below. Declared so byte-compile
;; doesn't warn about free variables when org-appear isn't loaded.
(defvar org-appear--prev-elem)
(defvar org-appear--elem-toggled)

(defun ps/org--system-clipboard-url ()
  "Try to return a URL from GUI selection or pbpaste, or nil."
  (or
   (when (fboundp 'gui-get-selection)
     (let ((sel (or (gui-get-selection 'CLIPBOARD)
                    (gui-get-selection 'PRIMARY))))
       (and sel (string-trim sel) (when (string-match-p "^https?://" sel) sel))))
   (when (executable-find "pbpaste")
     (let ((s (string-trim (with-temp-buffer
                             (call-process "pbpaste" nil t)
                             (buffer-string)))))
       (and (not (string-empty-p s)) (when (string-match-p "^https?://" s) s))))))

(defun ps/org--replace-marked-region-with (start-marker end-marker text)
  "Replace region between START-MARKER and END-MARKER with TEXT.
Runs from an async `url-retrieve' callback that deletes a placeholder link
and inserts the final one outside the command loop. That invalidates
org-appear's cached at-point element (`org-appear--prev-elem'), which still
points at the deleted placeholder; the `ps/org-appear--reassert-reveal'
advice on `org-activate-links' would otherwise re-reveal that stale region
during refontification, leaving the new link half-folded (only the trailing
\"]]\" hidden) until the cursor moves. Clear that cache first, then force the
new link's fontification (Org link folding) and an immediate redisplay.
Clears markers afterward to prevent double-execution bugs."
  (when (and (marker-position start-marker) (marker-position end-marker))
    (let ((inhibit-read-only t)
          (beg (marker-position start-marker))
          (end (marker-position end-marker)))
      (save-excursion
        (goto-char beg)
        (delete-region beg end)
        (insert text))
      ;; Drop org-appear's stale cached element before refontifying so its
      ;; reveal-reassert advice no-ops; the next post-command resyncs cleanly.
      (when (bound-and-true-p org-appear-mode)
        (setq org-appear--prev-elem nil
              org-appear--elem-toggled nil))
      (font-lock-flush beg (+ beg (length text)))
      (font-lock-ensure beg (+ beg (length text)))
      (redisplay t)
      ;; Nullify markers to prevent the url.el double-callback bug
      (set-marker start-marker nil)
      (set-marker end-marker nil))))

(defun ps/org--extract-title (html)
  "Extract a page title from HTML string.
Tries the og:title meta tag, then a name=title meta tag, then the <title>
element, returning the first match or nil."
  (or
   (when (string-match "<meta[^>]*property=[\"']og:title[\"'][^>]*content=[\"']\\([^\"']+\\)[\"']" html)
     (match-string 1 html))
   (when (string-match "<meta[^>]*name=[\"']title[\"'][^>]*content=[\"']\\([^\"']+\\)[\"']" html)
     (match-string 1 html))
   (when (string-match "<title[^>]*>\\([^<]+\\)</title>" html)
     (match-string 1 html))))

;;; The interactive command (must be defined before the key binding)
(defun ps/org-insert-link-async ()
  "Insert an Org link from URL found at point/clipboard or prompted.
Spoofs User-Agent to bypass bot detection."
  (interactive)
  (let* ((url-at-point-bounds (bounds-of-thing-at-point 'url))
         (url-at-point (when url-at-point-bounds
                         (buffer-substring-no-properties (car url-at-point-bounds)
                                                         (cdr url-at-point-bounds))))
         (maybe-url (or url-at-point
                        (ps/org--system-clipboard-url)
                        (read-string "URL: ")))
         (url-user-agent "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36"))
    (unless (and maybe-url (string-match-p "^https?://" maybe-url))
      (user-error "No valid URL found or provided"))

    ;; If there was a URL at point, delete it
    (when url-at-point-bounds
      (delete-region (car url-at-point-bounds) (cdr url-at-point-bounds)))

    ;; Insert placeholder and set markers
    (let ((start-marker (set-marker (make-marker) (point))))
      (insert (format "[[%s][Fetching title...]]" maybe-url))
      (let ((end-marker (set-marker (make-marker) (point)))
            (orig-buf (current-buffer)))
        (condition-case err
            (url-retrieve
             maybe-url
             (lambda (status &rest _)
               (let ((error-desc (plist-get status :error)))
                 (unwind-protect
                     (if error-desc
                         (progn
                           (when (buffer-live-p orig-buf)
                             (with-current-buffer orig-buf
                               ;; Fallback to RAW URL
                               (ps/org--replace-marked-region-with start-marker end-marker maybe-url)))
                           (message "[org-link] Failed to fetch %s: %s" maybe-url error-desc))
                       (goto-char (point-min))
                       (when (search-forward "\n\n" nil t)
                         (let* ((html (buffer-substring-no-properties (point) (point-max)))
                                (title (ps/org--extract-title html)))
                           (setq title (ps/org--clean-title title))
                           (if (and title (not (string-empty-p title)))
                               (let ((final (ps/org--shorten title)))
                                 (when (buffer-live-p orig-buf)
                                   (with-current-buffer orig-buf
                                     (ps/org--replace-marked-region-with start-marker end-marker (format "[[%s][%s]]" maybe-url final)))))
                             ;; Fallback to RAW URL
                             (when (buffer-live-p orig-buf)
                               (with-current-buffer orig-buf
                                 (ps/org--replace-marked-region-with start-marker end-marker maybe-url)))
                             (message "[org-link] No title found for %s" maybe-url)))))
                   (when (buffer-live-p (current-buffer))
                     (kill-buffer (current-buffer)))))))
          (error
           (when (buffer-live-p orig-buf)
             (with-current-buffer orig-buf
               ;; Fallback to RAW URL
               (ps/org--replace-marked-region-with start-marker end-marker maybe-url)))
           (message "[org-link] Error fetching %s: %s" maybe-url (error-message-string err))))))))

(provide 'ps-links)
;;; ps-links.el ends here
