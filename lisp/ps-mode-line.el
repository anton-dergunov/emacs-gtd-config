;;; ps-mode-line.el --- Planning-focused mode line -*- lexical-binding: t; -*-

;;; Commentary:
;; A compact, planning-oriented mode line for Org buffers and the agenda.
;;
;; Org buffers show:   <file> · <pct>% · <heading breadcrumb>
;; Agenda buffers show: <view title> [· <pct>%]
;;
;; The filename drops its ".org" extension; the position is a percentage only
;; (the line-number gutter already shows the line); the breadcrumb is the
;; ancestor + current heading TITLES (no TODO keyword, priority, tags, or
;; cookies).  When the line overflows the window, breadcrumb segments are
;; ellipsized individually, longest first, while the filename and position are
;; always preserved.
;;
;; Save state, minor-mode lighters, and the git-sync indicator are intentionally
;; omitted from Org/agenda windows (git-sync lives in the file-tree mode line).

;;; Code:

(require 'subr-x)

;; Provided by other modules / Org; declared so this file loads and its pure
;; helpers are testable in isolation.
(declare-function ps/file-tree--strip-org-extension "ps-file-tree" (name))
(declare-function org-before-first-heading-p "org" ())
(declare-function org-back-to-heading "org" (&optional invisible-ok))
(declare-function org-up-heading-safe "org" ())
(declare-function org-get-heading "org" (&optional no-tags no-todo no-priority no-comment))
;; Defined by ps-agenda-emoji (a defcustom); set buffer-locally per agenda view.
(defvar ps/agenda-emoji-enabled)
;; Set by org-agenda before `org-agenda-finalize'; identifies the built view.
(defvar org-agenda-redo-command)
;; Let-bound by the Calendar custom command; the displayed span.  Used to label
;; the agenda mode line.
(defvar ps/agenda-layout-view-kind)
(defvar org-agenda-current-span)

;;; Customization

(defgroup ps-mode-line nil
  "Planning-focused mode line."
  :group 'ps)

(defcustom ps/mode-line-separator " · "
  "Separator placed between mode-line components."
  :type 'string
  :group 'ps-mode-line)

;;; Filename

(defun ps/mode-line--buffer-name ()
  "Return the current buffer's name without a trailing \".org\"."
  (let ((name (buffer-name)))
    (if (fboundp 'ps/file-tree--strip-org-extension)
        (ps/file-tree--strip-org-extension name)
      name)))

;;; Position

(defun ps/mode-line--percent ()
  "Return point's position through the buffer as an integer percent string."
  (let ((max (point-max)))
    (if (<= max 1)
        "0%"
      (format "%d%%" (/ (* 100 (1- (point))) (1- max))))))

;;; Heading breadcrumb

(defun ps/mode-line--outline-titles ()
  "Return ancestor+current heading titles (top-to-bottom), or nil if none.
Each title is cleaned of TODO keyword, priority, tags, and comment markers."
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (save-restriction
        (widen)
        (unless (org-before-first-heading-p)
          (org-back-to-heading t)
          (let ((titles (list (substring-no-properties (org-get-heading t t t t)))))
            (while (org-up-heading-safe)
              (push (substring-no-properties (org-get-heading t t t t)) titles))
            titles))))))

(defun ps/mode-line--join-titles (titles)
  "Join TITLES into a \" > \"-separated breadcrumb string."
  (mapconcat #'identity titles " > "))

;;; Truncation (pure)

(defun ps/mode-line--seg-trimmable-p (seg)
  "Return non-nil when SEG can lose another visible character.
A segment is kept to at least one visible char plus an ellipsis."
  (let ((base (if (string-suffix-p "…" seg) (substring seg 0 -1) seg)))
    (> (length base) 1)))

(defun ps/mode-line--seg-trim (seg)
  "Return SEG with one visible character removed and a trailing ellipsis."
  (let* ((base (if (string-suffix-p "…" seg) (substring seg 0 -1) seg))
         (trimmed (substring base 0 (max 1 (1- (length base))))))
    (concat trimmed "…")))

(defun ps/mode-line--longest-trimmable-index (segs)
  "Return the index of the widest trimmable segment in SEGS, or nil."
  (let ((best nil) (best-w -1) (i 0))
    (dolist (s segs)
      (when (and (ps/mode-line--seg-trimmable-p s)
                 (> (string-width s) best-w))
        (setq best i best-w (string-width s)))
      (setq i (1+ i)))
    best))

(defun ps/mode-line--truncate-segments (titles width)
  "Join TITLES with \" > \", ellipsizing segments to fit WIDTH columns.
The widest trimmable segment is shortened first; each segment keeps at
least one visible character.  When WIDTH is non-positive the full
breadcrumb is returned unchanged."
  (let ((segs (copy-sequence titles)))
    (if (null segs)
        ""
      (let ((result (ps/mode-line--join-titles segs)))
        (while (and (> width 0)
                    (> (string-width result) width)
                    (ps/mode-line--longest-trimmable-index segs))
          (let ((idx (ps/mode-line--longest-trimmable-index segs)))
            (setf (nth idx segs) (ps/mode-line--seg-trim (nth idx segs))))
          (setq result (ps/mode-line--join-titles segs)))
        result))))

;;; Org-buffer mode line

(defun ps/mode-line--escape (s)
  "Escape % in S so it survives mode-line %-construct expansion.
A `:eval' result is itself processed for %-constructs, so a literal % must
be doubled or it (and the following character) is swallowed."
  (replace-regexp-in-string "%" "%%" s))

(defun ps/mode-line--render ()
  "Return the Org-buffer mode-line string.
Computed live (no cache) so it tracks point on every redisplay."
  (let* ((sep ps/mode-line-separator)
         (name (ps/mode-line--buffer-name))
         (pct (ps/mode-line--percent))
         (titles (ps/mode-line--outline-titles))
         (prefix (concat " "
                         (propertize (ps/mode-line--escape name)
                                     'face 'mode-line-emphasis)
                         sep (ps/mode-line--escape pct))))
    (if titles
        ;; Width math uses the unescaped strings (escaping does not widen).
        (let* ((used (+ (string-width (concat " " name sep pct)) (string-width sep)))
               (avail (- (window-body-width) used))
               (crumb (ps/mode-line--escape
                       (ps/mode-line--truncate-segments titles avail))))
          (concat prefix sep crumb))
      prefix)))

;;; Agenda mode line

(defvar-local ps/mode-line--agenda-title nil
  "Mode-line title for this agenda buffer, set by `ps/mode-line--agenda-finalize'.")

(defvar-local ps/mode-line--agenda-show-position nil
  "When non-nil, the agenda mode line appends point's percentage.")

(defun ps/mode-line--agenda-render ()
  "Return the agenda mode-line string."
  (let ((title (propertize (or ps/mode-line--agenda-title "Agenda")
                           'face 'mode-line-emphasis)))
    (if ps/mode-line--agenda-show-position
        (concat " " title ps/mode-line-separator
                (ps/mode-line--escape (ps/mode-line--percent)))
      (concat " " title))))

(defun ps/mode-line--span-label (span)
  "Return a human label for an agenda SPAN symbol or day count."
  (pcase span
    ('day "Day") ('week "Week") ('month "Month") ('year "Year")
    ('fortnight "Fortnight")
    ((and (pred integerp) n) (if (= n 1) "Day" (format "%d days" n)))
    (_ "Day")))

(defun ps/mode-line--agenda-finalize ()
  "Apply per-view mode line/chrome to the agenda buffer on every build.
Runs from `org-agenda-finalize-hook' at a negative depth, before the
emoji/layout hooks, so the emoji toggle takes effect for this render.

The view is derived intrinsically: `org-agenda-redo-command' is `org-todo-list'
for the Tasks view; the Calendar custom command let-binds
`ps/agenda-layout-view-kind' to `calendar' (in scope here, during finalize);
otherwise it is the Agenda.  Robust regardless of how the build was triggered
\(wrapper, dispatcher, `g'/redo, a date-stamp click)."
  (when (derived-mode-p 'org-agenda-mode)
    (let* ((tasks (eq (car-safe org-agenda-redo-command) 'org-todo-list))
           (calendar (and (boundp 'ps/agenda-layout-view-kind)
                          (eq ps/agenda-layout-view-kind 'calendar))))
      (setq-local ps/mode-line--agenda-title
                  (cond (tasks "Tasks")
                        (calendar
                         (concat "Calendar"
                                 ps/mode-line-separator
                                 (ps/mode-line--span-label
                                  (and (boundp 'org-agenda-current-span)
                                       org-agenda-current-span))))
                        (t "Agenda")))
      (setq-local ps/mode-line--agenda-show-position tasks)
      ;; Disable the semantic-emoji decoration in the (long) Tasks view.
      (setq-local ps/agenda-emoji-enabled (not tasks))
      ;; Line-number gutter only in Tasks; re-applied so a redo can't drop it.
      (display-line-numbers-mode (if tasks 1 0))
      ;; Agenda buffers are regenerated — never accumulate undo data.
      (setq buffer-undo-list t)
      (setq-local mode-line-format '((:eval (ps/mode-line--agenda-render))))
      ;; Refresh on navigation so the Tasks percentage tracks point (see
      ;; `ps/mode-line--org-setup' for why a full redraw must be forced).
      (add-hook 'post-command-hook #'force-mode-line-update nil t))))

;;; Frame title

(defun ps/mode-line--frame-title ()
  "Return the current buffer's name without a trailing \".org\"."
  (ps/mode-line--buffer-name))

;;; Mouse

(defun ps/mode-line--disable-destructive-mouse ()
  "Disable the old-school destructive mode-line mouse clicks (global).
By default mouse-2 closes the other windows and mouse-3 closes the
window — both are easy to trigger by accident in a planning UI.  Left
click (select window) and drag-to-resize are left untouched."
  (define-key global-map [mode-line mouse-2] #'ignore)
  (define-key global-map [mode-line mouse-3] #'ignore))

;;; Setup

(defvar-local ps/mode-line--last-line nil
  "Line number at the last mode-line refresh in this buffer.
Used by `ps/mode-line--refresh-on-line-change' to skip per-keystroke updates.")

(defun ps/mode-line--refresh-on-line-change ()
  "Force a mode-line update only when point moved to a different line.
Emacs's optimized cursor-movement redisplay does not re-evaluate a custom
`:eval' mode line, so the live percentage/breadcrumb would otherwise look
stale on keyboard navigation.  Refreshing on every command (including each
self-insert) is visually noisy, so refresh only when the line changes."
  (let ((line (line-number-at-pos)))
    (unless (eql line ps/mode-line--last-line)
      (setq ps/mode-line--last-line line)
      (force-mode-line-update))))

(defun ps/mode-line--org-setup ()
  "Install the planning mode line in the current Org buffer."
  (setq-local mode-line-format '((:eval (ps/mode-line--render))))
  (add-hook 'post-command-hook #'ps/mode-line--refresh-on-line-change nil t))

;;;###autoload
(defun ps/mode-line-setup ()
  "Enable the planning-focused mode line and frame title."
  (add-hook 'org-mode-hook #'ps/mode-line--org-setup)
  ;; Negative depth: run before the emoji/layout finalize hooks.
  (add-hook 'org-agenda-finalize-hook #'ps/mode-line--agenda-finalize -90)
  (ps/mode-line--disable-destructive-mouse)
  (setq frame-title-format '(:eval (ps/mode-line--frame-title))))

(provide 'ps-mode-line)
;;; ps-mode-line.el ends here
