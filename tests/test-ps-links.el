;;; test-ps-links.el --- ERT tests for ps-links -*- lexical-binding: t; -*-

(require 'ert)
(require 'org)
(require 'cl-lib)
(add-to-list 'load-path "lisp")
(require 'ps-links)

;; Mirrors the registration in config.org, needed here so `org-open-at-point'
;; recognizes obsidian: links (otherwise it treats them as fuzzy heading
;; searches).
(org-link-set-parameters "obsidian" :follow #'ps/obsidian--open-note :keymap ps/obsidian-link-map)

;; org-appear is not loaded in the batch test env; declare the internals the
;; stale-cache regression test fakes so they are special (dynamically bound).
(defvar org-appear-mode)
(defvar org-appear--elem-toggled)
(defvar org-appear--prev-elem)

(ert-deftest ps/obsidian-links-all-composed ()
  "All obsidian: prefixes in a buffer receive font-lock composition."
  (with-temp-buffer
    (org-mode)
    (insert "[[obsidian:One]]\n[[obsidian:Two]]\n[[obsidian:Three]]")
    (ps/org-enable-obsidian-links)
    (font-lock-ensure)
    (goto-char (point-min))
    (let ((count 0))
      (while (re-search-forward "obsidian:" nil t)
        (when (get-text-property (match-beginning 0) 'composition)
          (setq count (1+ count))))
      (should (= count 3)))))

(ert-deftest ps/obsidian-links-survive-link-display-toggle ()
  "Obsidian icon composition is unaffected by toggling `org-link-descriptive',
the mechanism org-appear uses to reveal/hide link syntax at point."
  (with-temp-buffer
    (org-mode)
    (insert "[[obsidian:Note]]")
    (ps/org-enable-obsidian-links)
    (font-lock-ensure)
    (let ((org-link-descriptive nil))  ; simulate org-appear revealing the link
      (font-lock-flush)
      (font-lock-ensure)
      (goto-char (point-min))
      (re-search-forward "obsidian:")
      (should (get-text-property (match-beginning 0) 'composition)))))

;;; -------------------------------------------------------
;;; Async URL link insertion
;;; -------------------------------------------------------

(ert-deftest ps/links--async-api-defined ()
  "The async URL command and its helpers are defined; the command is interactive."
  (should (fboundp 'ps/org-insert-link-async))
  (should (commandp 'ps/org-insert-link-async))
  (should (fboundp 'ps/org--system-clipboard-url))
  (should (fboundp 'ps/org--replace-marked-region-with)))

(ert-deftest ps/links--replace-marked-region-basic ()
  "Replacing a marked region swaps the placeholder text for the final text."
  (with-temp-buffer
    (insert "before PLACEHOLDER after")
    (goto-char (point-min))
    (search-forward "PLACEHOLDER")
    (let ((start (set-marker (make-marker) (match-beginning 0)))
          (end   (set-marker (make-marker) (match-end 0))))
      (ps/org--replace-marked-region-with start end "[[url][Title]]")
      (should (equal (buffer-string) "before [[url][Title]] after")))))

(ert-deftest ps/links--replace-marked-region-nullifies-markers ()
  "After replacement the markers are cleared, preventing a double callback."
  (with-temp-buffer
    (insert "x PLACEHOLDER y")
    (goto-char (point-min))
    (search-forward "PLACEHOLDER")
    (let ((start (set-marker (make-marker) (match-beginning 0)))
          (end   (set-marker (make-marker) (match-end 0))))
      (ps/org--replace-marked-region-with start end "Z")
      (should (null (marker-position start)))
      (should (null (marker-position end)))
      ;; A second call is a no-op now that the markers are nil.
      (ps/org--replace-marked-region-with start end "SHOULD-NOT-APPEAR")
      (should-not (string-match-p "SHOULD-NOT-APPEAR" (buffer-string))))))

(ert-deftest ps/links--replace-marked-region-folds-link-immediately ()
  "After replacement, the inserted org link is folded without an explicit
external font-lock-ensure call — the function must fontify it itself."
  (with-temp-buffer
    (org-mode)
    (insert "before PLACEHOLDER after")
    (goto-char (point-min))
    (search-forward "PLACEHOLDER")
    (let ((start (set-marker (make-marker) (match-beginning 0)))
          (end   (set-marker (make-marker) (match-end 0))))
      (ps/org--replace-marked-region-with
       start end "[[https://example.com][Example]]")
      (goto-char (point-min))
      (search-forward "[[")
      (should (eq (get-text-property (match-beginning 0) 'invisible)
                  'org-link)))))

(ert-deftest ps/links--replace-clears-stale-org-appear-cache ()
  "The async replace clears org-appear's cached toggled element so the
reveal-reassert advice cannot un-fold the freshly inserted link."
  (with-temp-buffer
    (org-mode)
    (insert "x PLACEHOLDER y")
    (goto-char (point-min))
    (search-forward "PLACEHOLDER")
    (let ((org-appear-mode t)
          (org-appear--elem-toggled t)
          (org-appear--prev-elem 'stale)
          (start (set-marker (make-marker) (match-beginning 0)))
          (end   (set-marker (make-marker) (match-end 0))))
      (ps/org--replace-marked-region-with start end "[[u][d]]")
      (should (null org-appear--elem-toggled))
      (should (null org-appear--prev-elem)))))

(ert-deftest ps/links--replace-marked-region-noop-on-unset ()
  "With unset markers, replacement does nothing and leaves the buffer intact."
  (with-temp-buffer
    (insert "untouched")
    (let ((start (make-marker))
          (end   (make-marker)))
      (ps/org--replace-marked-region-with start end "X")
      (should (equal (buffer-string) "untouched")))))

;;; -------------------------------------------------------
;;; extract-title
;;; -------------------------------------------------------

(ert-deftest ps/links--extract-title-og ()
  "The og:title meta tag is preferred."
  (should (equal (ps/org--extract-title
                  "<meta property=\"og:title\" content=\"OG Title\"><title>Tag Title</title>")
                 "OG Title")))

(ert-deftest ps/links--extract-title-name-meta ()
  "Falls back to the name=title meta tag when og:title is absent."
  (should (equal (ps/org--extract-title
                  "<meta name=\"title\" content=\"Name Title\"><title>Tag Title</title>")
                 "Name Title")))

(ert-deftest ps/links--extract-title-title-tag ()
  "Falls back to the <title> element when no meta title is present."
  (should (equal (ps/org--extract-title "<head><title>Just A Title</title></head>")
                 "Just A Title")))

(ert-deftest ps/links--extract-title-none ()
  "Returns nil when no title can be found."
  (should (null (ps/org--extract-title "<html><body>no title here</body></html>"))))

;;; -------------------------------------------------------
;;; obsidian:// URL construction
;;; -------------------------------------------------------

(ert-deftest ps/links--open-note-builds-url ()
  "open-note browses an obsidian:// URL with the hexified path."
  (let (captured)
    (cl-letf (((symbol-function 'browse-url)
               (lambda (url &rest _) (setq captured url))))
      (ps/obsidian--open-note "My Note")
      (should (equal captured "obsidian://open?vault=obsidian&file=My%20Note")))))

;;; -------------------------------------------------------
;;; obsidian: link mouse handling
;;; -------------------------------------------------------

(ert-deftest ps/links--mouse-open-invokes-follow-without-moving-point ()
  "Clicking an obsidian link opens it without moving point or a live window."
  (let (captured-path)
    (cl-letf (((symbol-function 'ps/obsidian--open-note)
               (lambda (path) (setq captured-path path))))
      (with-temp-buffer
        (org-mode)
        (insert "before [[obsidian:Note]] after")
        (goto-char (point-min))
        (let ((link-pos (progn (goto-char (point-min))
                                (search-forward "obsidian:Note")
                                (match-beginning 0)))
              (start-point (point-min)))
          (goto-char start-point)
          (save-window-excursion
            (set-window-buffer (selected-window) (current-buffer))
            (let ((event (list 'down-mouse-1
                                (list (selected-window) link-pos '(0 . 0) 0))))
              (ps/obsidian-mouse-open event))
            (should (equal captured-path "Note"))
            (should (= (point) start-point))))))))

(ert-deftest ps/links--obsidian-map-binds-down-mouse-1 ()
  "The link-local keymap opens on mouse-down rather than the default up-click."
  (should (eq (lookup-key ps/obsidian-link-map [down-mouse-1])
              #'ps/obsidian-mouse-open)))

;;; -------------------------------------------------------
;;; obsidian link insertion commands
;;; -------------------------------------------------------

(ert-deftest ps/links--insert-from-clipboard ()
  "Clipboard text becomes an [[obsidian:...]] link."
  (cl-letf (((symbol-function 'gui-get-selection)
             (lambda (&rest _) "Clip Title")))
    (with-temp-buffer
      (ps/insert-obsidian-link-from-clipboard)
      (should (equal (buffer-string) "[[obsidian:Clip Title]]")))))

(ert-deftest ps/links--insert-from-clipboard-empty-errors ()
  "An empty clipboard signals a user-error and inserts nothing."
  (cl-letf (((symbol-function 'gui-get-selection) (lambda (&rest _) ""))
            ((symbol-function 'executable-find) (lambda (&rest _) nil)))
    (with-temp-buffer
      (should-error (ps/insert-obsidian-link-from-clipboard) :type 'user-error)
      (should (equal (buffer-string) "")))))

(ert-deftest ps/links--insert-from-prompt ()
  "A prompted title becomes an [[obsidian:...]] link."
  (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "Typed Title")))
    (with-temp-buffer
      (ps/insert-obsidian-link-prompt)
      (should (equal (buffer-string) "[[obsidian:Typed Title]]")))))

(ert-deftest ps/links--insert-from-prompt-blank-noop ()
  "A blank/whitespace title inserts nothing."
  (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "   ")))
    (with-temp-buffer
      (ps/insert-obsidian-link-prompt)
      (should (equal (buffer-string) "")))))

;;; -------------------------------------------------------
;;; system-clipboard-url
;;; -------------------------------------------------------

(ert-deftest ps/links--system-clipboard-url-returns-url ()
  "A clipboard holding an http(s) URL is returned."
  (cl-letf (((symbol-function 'gui-get-selection)
             (lambda (&rest _) "https://example.com/x")))
    (should (equal (ps/org--system-clipboard-url) "https://example.com/x"))))

(ert-deftest ps/links--system-clipboard-url-nil-for-non-url ()
  "Non-URL clipboard contents yield nil."
  (cl-letf (((symbol-function 'gui-get-selection) (lambda (&rest _) "just text"))
            ((symbol-function 'executable-find) (lambda (&rest _) nil)))
    (should (null (ps/org--system-clipboard-url)))))
