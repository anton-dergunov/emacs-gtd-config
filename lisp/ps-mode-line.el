;;; ps-mode-line.el --- Planning-focused mode line -*- lexical-binding: t; -*-

;;; Commentary:
;; A compact, planning-oriented mode line for Org buffers and the agenda.
;;
;; Org buffers show:   <file> · <pct>% · <heading breadcrumb>
;; Agenda buffers show: <view title> [· <pct>%]
;;
;; The filename drops its ".org" extension, and gains its folder as a prefix
;; ("Personal/Inbox") when two open files share a name; the position is a
;; percentage only (the line-number gutter already shows the line); the
;; breadcrumb is the ancestor + current heading TITLES (no TODO keyword,
;; priority, tags, or cookies).  When the line overflows, breadcrumb segments are
;; ellipsized individually, longest first, while the filename and position are
;; always preserved.
;;
;; Save state, minor-mode lighters, and the git-sync indicator are intentionally
;; omitted from Org/agenda windows (git-sync lives in the file-tree mode line).

;;; Code:

(require 'subr-x)

;; Provided by other modules / Org; declared so this file loads and its pure
;; helpers are testable in isolation.
(declare-function ps/file-tree--normalize-display-name "ps-file-tree" (name))
(declare-function ps/claude--session-buffer-p "ps-claude" (buffer-or-name))
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
(declare-function ps/show-conflicts "ps-conflicts" ())
(declare-function ps/org-show-availability "ps-availability" ())
(declare-function ps/show-situation "ps-situations" (key))
(declare-function ps/situations--menu-filter "ps-situations" (&optional items))
(declare-function ps/situations--name "ps-situations" (situation))
(declare-function ps/show-calendar "config" (&optional span))
(declare-function easy-menu-create-menu "easymenu" (menu-name menu-items))
(declare-function ps/situations-find "ps-situations" (key))
;; Set by `ps/situations--stash' on every agenda build; nil outside a Situation view.
(defvar ps/situations-current-key)
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

(defun ps/mode-line--strip-org (name)
  "Return NAME with a trailing \".org\" removed and \"_\" replaced by a space."
  (if (fboundp 'ps/file-tree--normalize-display-name)
      (ps/file-tree--normalize-display-name name)
    name))

(defun ps/mode-line--display-name (name)
  "Return buffer NAME formatted for display: \".org\" dropped, folder prefixed.

When two open files share a name, `uniquify' renames one of them by
appending a directory qualifier in angle brackets -- \"Work.org<mydir>\"
\(see `uniquify-buffer-name-style', pinned in config.org).  That suffix
defeats a plain extension strip, and it reads better as a path anyway, so
the qualifier is moved to the front: \"mydir/Work\".  This matches how the
File Tree presents the same file.  Deeper qualifiers already contain \"/\"
\(\"Work.org<a/b>\" -> \"a/b/Work\"), so they need no special handling.

An all-digit qualifier is Emacs's own fallback for buffers with no file
\(\"Work.org<2>\"); it is not a directory, so it is left where it is."
  (if (string-match "\\`\\(.*\\)<\\([^<>]+\\)>\\'" name)
      (let ((base (match-string 1 name))
            (qualifier (match-string 2 name)))
        (if (string-match-p "\\`[0-9]+\\'" qualifier)
            (concat (ps/mode-line--strip-org base) "<" qualifier ">")
          (concat qualifier "/" (ps/mode-line--strip-org base))))
    (ps/mode-line--strip-org name)))

(defun ps/mode-line--buffer-name ()
  "Return the current buffer's name formatted for display.
See `ps/mode-line--display-name'."
  (ps/mode-line--display-name (buffer-name)))

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

(defvar-local ps/mode-line--task-count-open nil
  "Count of this buffer's open (non-DONE) TODO-state headings, or nil.
Nil means either the file is out of `ps/org-files-in-scope-p', or it has no
headings with a recognized TODO state -- either way, no segment is shown.
Note 0 is a valid value (every recognized heading is DONE) and is distinct
from nil.  Owned by `ps/task-count--recompute' in lisp/ps-task-count.el.")

(defvar-local ps/mode-line--task-count-tooltip nil
  "Per-state task-count breakdown shown on hover, or nil alongside
`ps/mode-line--task-count-open' being nil.  Owned by
`ps/task-count--recompute' in lisp/ps-task-count.el.")

(defvar-local ps/mode-line--task-count-gen 0
  "Generation counter bumped every time the task count is recomputed.
Folded into the per-window render cache key (see `ps/mode-line--cache-valid-p')
so an idle-timer-driven update is not masked by a cache that only otherwise
keys on point/buffer-name.  Owned by `ps/task-count--recompute' in
lisp/ps-task-count.el.")

(defun ps/mode-line--escape (s)
  "Escape % in S so it survives mode-line %-construct expansion.
A `:eval' result is itself processed for %-constructs, so a literal % must
be doubled or it (and the following character) is swallowed."
  (replace-regexp-in-string "%" "%%" s))

(defun ps/mode-line--task-count-segment ()
  "Return the propertized task-count segment, or nil when there is none.
No `mouse-face'/`local-map': this segment isn't clickable, unlike the
agenda title and conflict-count segments -- a bare `help-echo' is enough
for a hover tooltip without implying an action that doesn't exist."
  (when ps/mode-line--task-count-open
    (propertize (ps/mode-line--escape (number-to-string ps/mode-line--task-count-open))
                'help-echo ps/mode-line--task-count-tooltip)))

(defun ps/mode-line--render ()
  "Return the Org-buffer mode-line string for the current point/buffer state."
  (let* ((sep ps/mode-line-separator)
         (name (ps/mode-line--buffer-name))
         (pct (ps/mode-line--percent))
         (titles (ps/mode-line--outline-titles))
         (task-str (and ps/mode-line--task-count-open
                        (number-to-string ps/mode-line--task-count-open)))
         (task-seg (ps/mode-line--task-count-segment))
         (task-width (if task-str (+ (string-width sep) (string-width task-str)) 0))
         (prefix (concat " "
                         (propertize (ps/mode-line--escape name)
                                     'face 'mode-line-emphasis)
                         (if task-seg (concat sep task-seg) "")
                         sep (ps/mode-line--escape pct))))
    (if titles
        ;; Width math uses the unescaped strings (escaping does not widen).
        (let* ((used (+ (string-width (concat " " name sep pct)) task-width (string-width sep)))
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

(defvar-local ps/mode-line--agenda-conflict-count nil
  "Scheduling-conflict count for this agenda buffer's Agenda view.
Set by `ps/conflicts--agenda-check' (lisp/ps-conflicts.el); cleared by
`ps/mode-line--agenda-finalize' on every non-Agenda rebuild so a stale count
never leaks into the Calendar or Tasks mode line.")

(defun ps/mode-line-view-menu-items ()
  "Return the planning views as an easymenu item list.

The single definition of the view menu, shared by two places so they cannot
drift apart: the mode-line view switcher pops it up verbatim
\(`ps/mode-line--view-click'), and config.org's Productivity → Plan & Review
submenu is built from it, adding only its own \"Full Agenda…\" entry.  Both
therefore show the same entries, nested the same way, in the same order.

The Situations submenu is generated on open from `ps/situations' (see
`ps/situations--menu-filter'), and omitted when that module is absent."
  (append
   (list ["Agenda" ps/show-agenda :keys "C-c p a"]
         (list "Calendar"
               ["Open (Day)" ps/show-calendar       :keys "C-c p c c"]
               ["Day"        ps/show-calendar-day   :keys "C-c p c d"]
               ["Week"       ps/show-calendar-week  :keys "C-c p c w"]
               ["Month"      ps/show-calendar-month :keys "C-c p c m"]
               ["Year"       ps/show-calendar-year  :keys "C-c p c y"]))
   (when (fboundp 'ps/situations--menu-filter)
     (list (list "Situations" :filter #'ps/situations--menu-filter)))
   (list ["Tasks"        ps/show-tasks            :keys "C-c p n   /   F9"]
         ["Availability" ps/org-show-availability :keys "C-c p v"]
         ["Conflicts"    ps/show-conflicts        :keys "C-c p x"])))

(defun ps/mode-line--view-click (event)
  "Show a popup menu of planning views and switch to the one EVENT selects.

Built as a real menu keymap rather than an alist of panes, so the popup can
carry the nested Calendar and Situations submenus and match the Productivity
menu exactly."
  (interactive "e")
  (require 'easymenu)
  (let* ((menu (easy-menu-create-menu "View" (ps/mode-line-view-menu-items)))
         (choice (x-popup-menu event menu))
         (cmd (and choice (lookup-key menu (apply #'vector choice)))))
    (when (commandp cmd) (call-interactively cmd))))

(defun ps/mode-line--view-title (label)
  "Return a clickable \"LABEL ▾\" mode-line segment.
Mouse-1 pops the planning-views menu (see `ps/mode-line--view-click')."
  (propertize (concat label " ▾")
              'face 'mode-line-emphasis
              'mouse-face 'mode-line-highlight
              'help-echo "mouse-1: switch view"
              'local-map
              (let ((map (make-sparse-keymap)))
                (define-key map [mode-line mouse-1] #'ps/mode-line--view-click)
                map)))

(defun ps/mode-line--simple-view-render (label)
  "Return a mode-line string showing LABEL as a clickable view-switcher title.
Used by buffers (Availability, Conflicts) that need nothing beyond the
clickable title itself."
  (concat " " (ps/mode-line--view-title label)))

(defun ps/mode-line--agenda-conflicts-click (event)
  "Open the dedicated Conflicts buffer in response to EVENT."
  (interactive "e")
  (call-interactively #'ps/show-conflicts))

(defun ps/mode-line--agenda-render ()
  "Return the agenda mode-line string.
The title is clickable (mouse-1 switches views), matching the file tree's
file-set selector.  In the Agenda view, a clickable conflict count follows it
when there are scheduling conflicts — in the mode line's default face, not a
warning face."
  (let ((title (ps/mode-line--view-title (or ps/mode-line--agenda-title "Agenda"))))
    (cond
     (ps/mode-line--agenda-show-position
      (concat " " title ps/mode-line-separator
              (ps/mode-line--escape (ps/mode-line--percent))))
     ((and ps/mode-line--agenda-conflict-count
           (> ps/mode-line--agenda-conflict-count 0))
      (concat " " title ps/mode-line-separator
              (propertize (format "⚠ %d conflict%s" ps/mode-line--agenda-conflict-count
                                  (if (= ps/mode-line--agenda-conflict-count 1) "" "s"))
                          'mouse-face 'mode-line-highlight
                          'help-echo "mouse-1: show conflicts"
                          'local-map
                          (let ((map (make-sparse-keymap)))
                            (define-key map [mode-line mouse-1]
                              #'ps/mode-line--agenda-conflicts-click)
                            map))))
     (t (concat " " title)))))

(defun ps/mode-line--span-label (span)
  "Return a human label for an agenda SPAN symbol or day count."
  (pcase span
    ('day "Day") ('week "Week") ('month "Month") ('year "Year")
    ('fortnight "Fortnight")
    ((and (pred integerp) n) (if (= n 1) "Day" (format "%d days" n)))
    (_ "Day")))

(defun ps/mode-line--situation-label (key)
  "Return the display name of situation KEY, falling back to KEY itself."
  (or (and (fboundp 'ps/situations-find)
           (let ((s (ps/situations-find key)))
             (and s (ps/situations--name s))))
      key))

(defun ps/mode-line--agenda-finalize ()
  "Apply per-view mode line/chrome to the agenda buffer on every build.
Runs from `org-agenda-finalize-hook' at a negative depth, before the
emoji/layout hooks, so the emoji toggle takes effect for this render.

The view is derived intrinsically: `org-agenda-redo-command' is `org-todo-list'
for the Tasks view; the Calendar custom command let-binds
`ps/agenda-layout-view-kind' to `calendar' (in scope here, during finalize); a
generated situation command binds it to `situation' and records which one in
`ps/situations-current-key' (stashed just before this hook); otherwise it is the
Agenda.  Robust regardless of how the build was triggered \(wrapper, dispatcher,
`g'/redo, a date-stamp click)."
  (when (derived-mode-p 'org-agenda-mode)
    (let* ((tasks (eq (car-safe org-agenda-redo-command) 'org-todo-list))
           (calendar (and (boundp 'ps/agenda-layout-view-kind)
                          (eq ps/agenda-layout-view-kind 'calendar)))
           (situation (and (boundp 'ps/situations-current-key)
                           ps/situations-current-key)))
      (setq-local ps/mode-line--agenda-title
                  (cond (tasks "Tasks")
                        (calendar
                         (concat "Calendar"
                                 ps/mode-line-separator
                                 (ps/mode-line--span-label
                                  (and (boundp 'org-agenda-current-span)
                                       org-agenda-current-span))))
                        (situation
                         (concat "Situation" ps/mode-line-separator
                                 (ps/mode-line--situation-label situation)))
                        (t "Agenda")))
      (setq-local ps/mode-line--agenda-show-position tasks)
      ;; The conflict count is Agenda-only (see `ps/conflicts--agenda-schedule-check');
      ;; clear it on every other view so a stale count never leaks into Calendar/Tasks.
      (when (or tasks calendar situation)
        (setq-local ps/mode-line--agenda-conflict-count nil))
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
  "Return the frame title for the current buffer.
The Claude Code session buffer shows as \"Claude Code\"; everything else
falls back to `ps/mode-line--buffer-name'."
  (if (and (fboundp 'ps/claude--session-buffer-p)
           (ps/claude--session-buffer-p (current-buffer)))
      "Claude Code"
    (ps/mode-line--buffer-name)))

;;; Mouse

(defun ps/mode-line--disable-destructive-mouse ()
  "Disable the old-school destructive mode-line mouse clicks (global).
By default mouse-2 closes the other windows and mouse-3 closes the
window — both are easy to trigger by accident in a planning UI.  Left
click (select window) and drag-to-resize are left untouched."
  (define-key global-map [mode-line mouse-2] #'ignore)
  (define-key global-map [mode-line mouse-3] #'ignore))

;;; Setup

(defun ps/mode-line--cache-valid-p ()
  "Non-nil when this window's cached mode-line string is still current.
The line and the buffer name are checked: `uniquify' renames an
already-open buffer the moment a second file of the same name is opened,
and without the name in the key the renamed buffer would keep showing its
old title until point happened to move to another line.  The task-count
generation is checked too: it updates asynchronously off an idle timer
(see lisp/ps-task-count.el), with neither point nor the buffer name
necessarily changing, so without this the cache would keep showing a
stale count until the next line move."
  (and (eql (pos-bol) (window-parameter nil 'ps-ml-bol))
       (equal (buffer-name) (window-parameter nil 'ps-ml-name))
       (eql ps/mode-line--task-count-gen (window-parameter nil 'ps-ml-task-gen))))

(defun ps/mode-line--render-window-cached ()
  "Return the Org-buffer mode-line string using a per-window cache.
Recomputes only when the cache key changes (see
`ps/mode-line--cache-valid-p'); otherwise returns the cached result.
Stored in window parameters (`ps-ml-bol', `ps-ml-name', `ps-ml-task-gen',
`ps-ml-str') so two windows showing the same buffer each track their own
position."
  (if (ps/mode-line--cache-valid-p)
      (or (window-parameter nil 'ps-ml-str) "")
    (let ((str (ps/mode-line--render)))
      (set-window-parameter nil 'ps-ml-bol (pos-bol))
      (set-window-parameter nil 'ps-ml-name (buffer-name))
      (set-window-parameter nil 'ps-ml-task-gen ps/mode-line--task-count-gen)
      (set-window-parameter nil 'ps-ml-str str)
      str)))

(defun ps/mode-line--maybe-refresh ()
  "Call `force-mode-line-update' when this window's cached line goes stale.
A `post-command-hook'; compares the current key against the
window-parameter cache set by `ps/mode-line--render-window-cached'."
  (unless (ps/mode-line--cache-valid-p)
    (force-mode-line-update)))

(defun ps/mode-line--org-setup ()
  "Install the planning mode line in the current Org buffer."
  (set-window-parameter nil 'ps-ml-bol (pos-bol))
  (set-window-parameter nil 'ps-ml-name (buffer-name))
  (set-window-parameter nil 'ps-ml-task-gen ps/mode-line--task-count-gen)
  (set-window-parameter nil 'ps-ml-str (ps/mode-line--render))
  (setq-local mode-line-format '((:eval (ps/mode-line--render-window-cached))))
  (add-hook 'post-command-hook #'ps/mode-line--maybe-refresh nil t))

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
