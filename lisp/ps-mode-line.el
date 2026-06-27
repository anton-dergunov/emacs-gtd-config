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
(declare-function org-link-display-format "ol" (s))
;; The view-switcher commands are defined later, in config.org's "Menu Bar
;; and Keybindings" section — always loaded well before a user can click the
;; agenda mode line.
(declare-function ps/show-agenda "config" ())
(declare-function ps/show-calendar-day "config" ())
(declare-function ps/show-calendar-week "config" ())
(declare-function ps/show-calendar-month "config" ())
(declare-function ps/show-calendar-year "config" ())
(declare-function ps/show-tasks "config" ())
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

(defun ps/mode-line--clean-markup (s)
  "Return S with Org link and emphasis markup reduced to plain text.
Bracket links collapse to their description (or path); a leading
\"obsidian:\" link-type prefix is dropped; emphasis markers
(* / _ = ~ +) wrapping a word are removed.  A light cleanup for display
only -- not a full Org renderer."
  (let ((s (if (fboundp 'org-link-display-format)
               (org-link-display-format s)
             s)))
    ;; Drop the obsidian link-type prefix left on description-less links.
    (setq s (replace-regexp-in-string "\\bobsidian:" "" s))
    ;; Strip simple emphasis markers, keeping the wrapped text.
    (replace-regexp-in-string
     "\\([*/_=~+]\\)\\([^ \t\n*/_=~+][^*/_=~+]*\\)\\1"
     "\\2" s)))

(defun ps/mode-line--heading-title ()
  "Return the current heading's cleaned, plain-text title."
  (ps/mode-line--clean-markup
   (substring-no-properties (org-get-heading t t t t))))

(defun ps/mode-line--outline-titles ()
  "Return ancestor+current heading titles (top-to-bottom), or nil if none.
Each title is cleaned of TODO keyword, priority, tags, comment markers,
and Org link/emphasis markup."
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (save-restriction
        (widen)
        (unless (org-before-first-heading-p)
          (org-back-to-heading t)
          (let ((titles (list (ps/mode-line--heading-title))))
            (while (org-up-heading-safe)
              (push (ps/mode-line--heading-title) titles))
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
  "Return the Org-buffer mode-line string for the current point/buffer state.
Called by `ps/mode-line--update-now' when the cache
(`ps/mode-line--cached-string') needs updating -- not evaluated directly by
`mode-line-format' on every redisplay."
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

(defconst ps/mode-line--agenda-view-items
  '(("Agenda" . ps/show-agenda)
    ("Calendar: Day" . ps/show-calendar-day)
    ("Calendar: Week" . ps/show-calendar-week)
    ("Calendar: Month" . ps/show-calendar-month)
    ("Calendar: Year" . ps/show-calendar-year)
    ("Tasks" . ps/show-tasks))
  "Views offered by the agenda mode-line's view switcher.
An alist of (LABEL . COMMAND), shown as a flat popup menu by
`ps/mode-line--agenda-click' — the same combo-box interaction as the file
tree's file-set selector (see `ps/file-tree--modeline-click').")

(defun ps/mode-line--agenda-click (event)
  "Show a popup menu of planning views and switch to the one EVENT selects."
  (interactive "e")
  (let* ((menu (list "View" (cons "Views" ps/mode-line--agenda-view-items)))
         (choice (x-popup-menu event menu)))
    (when choice (call-interactively choice))))

(defun ps/mode-line--agenda-render ()
  "Return the agenda mode-line string.
The title is clickable (mouse-1 switches views), matching the file tree's
file-set selector."
  (let ((title (propertize (concat (or ps/mode-line--agenda-title "Agenda") " ▾")
                           'face 'mode-line-emphasis
                           'mouse-face 'mode-line-highlight
                           'help-echo "mouse-1: switch view"
                           'local-map
                           (let ((map (make-sparse-keymap)))
                             (define-key map [mode-line mouse-1] #'ps/mode-line--agenda-click)
                             map))))
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

(defvar ps/mode-line-refresh-idle-delay 0.2
  "Idle seconds before the Org mode line recomputes after point moves.
The recompute is debounced onto idle time, so held-key scrolling (during which
Emacs is never idle) does not pay the per-line cost; the mode line refreshes
once movement pauses for this long.")

(defvar-local ps/mode-line--last-bol nil
  "Beginning-of-line position at the last mode-line refresh in this buffer.
Used by `ps/mode-line--update-now' to skip recomputing while point stays on the
same line.  A position compares in O(line length); the displayed info is a
percentage, never the absolute line number, so counting lines is unnecessary.")

(defvar-local ps/mode-line--update-timer nil
  "Pending idle timer that will recompute this buffer's mode line, or nil.")

(defvar-local ps/mode-line--cached-string nil
  "Last-rendered mode-line string for this buffer.
`mode-line-format' displays this value directly rather than calling
`ps/mode-line--render' on every redisplay.  Gating the *content* this way
(rather than just gating the `force-mode-line-update' call) matters because
self-insert and other editing commands need a real window redisplay anyway
\(to show the new character), and Emacs re-evaluates a custom `:eval' form
during any such redisplay regardless of whether `force-mode-line-update'
was called -- so editing on the same line would otherwise still change the
displayed text on every keystroke.")

(defun ps/mode-line--update-now (buffer)
  "Recompute and cache BUFFER's mode-line string if point changed lines.
Emacs's optimized cursor-movement redisplay does not re-evaluate a custom
`:eval' mode line on its own, so the live percentage/breadcrumb would otherwise
look stale on navigation without an explicit refresh.  Recomputing is gated on a
line change (see `ps/mode-line--last-bol') because the render is content-
sensitive and need not run while point stays on one line.  This is the
synchronous worker; `ps/mode-line--schedule-refresh' defers it to idle time."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq ps/mode-line--update-timer nil)
      (let ((bol (pos-bol)))
        (unless (eql bol ps/mode-line--last-bol)
          (setq ps/mode-line--last-bol bol
                ps/mode-line--cached-string (ps/mode-line--render))
          (force-mode-line-update))))))

(defun ps/mode-line--schedule-refresh ()
  "Debounce a mode-line recompute onto idle time (a `post-command-hook').
While point moves rapidly (e.g. held-key scrolling) Emacs is never idle, so
the content-sensitive `ps/mode-line--render' is deferred until movement pauses
for `ps/mode-line-refresh-idle-delay' -- keeping scrolling fast -- then runs
once and updates the mode line accurately.  Arming only when no timer is
pending keeps the per-command cost negligible."
  (unless ps/mode-line--update-timer
    (setq ps/mode-line--update-timer
          (run-with-idle-timer ps/mode-line-refresh-idle-delay nil
                               #'ps/mode-line--update-now (current-buffer)))))

(defun ps/mode-line--org-setup ()
  "Install the planning mode line in the current Org buffer."
  (setq ps/mode-line--last-bol (pos-bol))
  (setq ps/mode-line--cached-string (ps/mode-line--render))
  (setq-local mode-line-format '((:eval ps/mode-line--cached-string)))
  (add-hook 'post-command-hook #'ps/mode-line--schedule-refresh nil t))

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
