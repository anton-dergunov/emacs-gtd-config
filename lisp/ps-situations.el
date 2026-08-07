;;; ps-situations.el --- Context tags and the situation queries over them -*- lexical-binding: t; -*-

;;; Commentary:
;; Free time arrives in awkward shapes -- a minute between gym sets, a walk, a
;; plane with no signal.  This module answers "what is worth doing right now,
;; with what I have in my hands" with two declarations, and derives everything
;; else from them:
;;
;;   `ps/context-tags' -- the fixed tag vocabulary.  Each tag is an *affordance*:
;;     a promise that the task survives with less than a desk.  From it we derive
;;     `org-tag-alist' (names, fast-selection keys, and the `(:newline)' breaks
;;     between kinds) and the tag table in the generated AI context.
;;
;;   `ps/situations' -- saved `tags-todo' searches named by circumstance.  From
;;     it we derive the `org-agenda-custom-commands' entries, the `C-c p S'
;;     keymap, the Productivity menu entries, the mode-line switcher and the
;;     situation table in the generated AI context.
;;
;; Situations are not tags: a situation is a bundle of capabilities, so tagging
;; them directly would mean re-tagging everything whenever a new situation
;; appears.  The tags describe requirements; the situations are queries over
;; them, and a new situation costs one line and no re-tagging.
;;
;; Note the name: `ps-tags.el' is a *display* module (it keeps org-modern tag
;; pills whole when a line wraps) and has nothing to do with this one.
;;
;; Two facts about org-agenda drive the shape of the generated commands, and
;; both are load-bearing:
;;
;; 1. The commands are generated in *block* form -- ((tags-todo QUERY LPROPS))
;;    with command-wide GPROPS -- even though each holds a single block.  Only
;;    `org-agenda-run-series' records the whole series as
;;    `org-agenda-redo-command'; a single-type command lets `org-tags-view'
;;    overwrite redo with itself, which would drop the settings below and lose
;;    the view's identity on every `g'.
;;
;; 2. `org-agenda-overriding-header' is deliberately NOT set, so `org-tags-view'
;;    emits its own "Headlines with TAGS match: ..." structural header --
;;    the line `ps-agenda-layout' rewrites into the centred plate.
;;
;; `ps/situations-apply' must run *after* config.org's own
;; `(setq org-agenda-custom-commands ...)', which lives in a
;; `(use-package org-super-agenda :after org-agenda :init ...)' block and
;; therefore fires on the `org-agenda' load event.  Registering earlier -- from
;; workspace.org, say -- silently loses every entry: `eval-after-load' forms run
;; in registration order and that `setq' replaces the whole list.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

;; org / org-agenda are loaded by the time any of this runs interactively.
(declare-function org-agenda "org-agenda" (&optional arg org-keys restriction))
(declare-function org-get-at-bol "org" (property))
(declare-function ps/material-icons-image "ps-material-icons" (name &optional ascent height))
(defvar org-agenda-custom-commands)
(defvar org-agenda-finalize-hook)
(defvar org-tag-alist)
;; Let-bound by the generated commands; read during `org-agenda-finalize'.
(defvar ps/agenda-layout-view-kind)

;;; Customization

(defgroup ps-situations nil
  "Context tags and the situation queries over them."
  :group 'ps)

(defcustom ps/context-tags nil
  "The fixed context-tag vocabulary, or nil to leave tags free-form.

Each entry is a plist:

  (:name \"audio\" :key ?a :kind \"affordance\"
   :means \"Works with ears only -- walking, hiking, cardio, chores.\")

`:name' is the tag as written in the file.  `:key' is the character that
selects it in the `C-c C-q' fast-selection interface (`?a' is Emacs Lisp
character syntax for the character `a').  `:kind' groups the tags: entries are
kept in the order given and a `(:newline)' is emitted between kinds, so the
selection buffer reads as blocks.  `:means' is the one-line promise the tag
makes, and is what an AI assistant is told the tag is for.

A bare cons cell (NAME . KEY) is also accepted.

When non-nil this becomes `org-tag-alist' (see
`ps/context-tags-org-tag-alist'); when nil, `org-tag-alist' is left alone so
it can still be set by hand."
  :type '(repeat sexp)
  :group 'ps-situations)

(defcustom ps/situations nil
  "Saved tag searches named by circumstance, or nil for none.

Each entry is a plist:

  (:key \"m\" :name \"A spare minute\" :hint \"gym sets, a queue, waiting\"
   :icon \"hourglass_top\" :query \"think|micro\")

`:key' is the single character that follows `ps/situations-key-prefix' in the
agenda dispatcher, and the same character under `C-c p S'.  `:name' titles the
view.  `:hint' names the circumstance it fires in (tooltips, and the generated
AI context).  `:icon' is an optional Material Symbols name drawn on the plate.
`:query' is an Org tag-match string.

A bare list (KEY NAME QUERY) is also accepted.

Org's tag-match syntax has no parentheses -- `|' separates whole clauses -- so
a mixed query must be distributed by hand: (tablet or phone) and not online is
written \"tablet-online|phone-online\"."
  :type '(repeat sexp)
  :group 'ps-situations)

(defcustom ps/situations-key-prefix "s"
  "Agenda-dispatcher prefix owned by the situation commands.

`C-c a s m' opens the situation whose `:key' is \"m\".  Claiming a prefix
shadows the dispatcher's built-in key of the same name -- with the default
\"s\" that is \"Search for keywords\", still reachable as
\\[org-search-view].  Every key under this prefix belongs to this module:
`ps/situations-apply' strips them before re-adding its own."
  :type 'string
  :group 'ps-situations)

(defcustom ps/situations-empty-message
  "Nothing matches this situation right now."
  "Message shown in place of an empty situation view."
  :type 'string
  :group 'ps-situations)

;;; State

(defvar ps/situations-current nil
  "Key of the situation being built, let-bound by the generated command.
In scope during `org-agenda-finalize'; stashed into the buffer-local
`ps/situations-current-key' so it survives a resize re-layout.")

(defvar-local ps/situations-current-key nil
  "Key of the situation this agenda buffer shows, or nil for any other view.")

;;; Context tags (pure)

(defun ps/context-tags--normalize (entry)
  "Return ENTRY as a canonical context-tag plist, or nil if unusable.
Accepts the plist form and the (NAME . KEY) cons shorthand."
  (cond
   ((and (consp entry) (keywordp (car entry)))
    (let ((name (plist-get entry :name)))
      (and (stringp name) (not (string-empty-p name)) entry)))
   ((and (consp entry) (stringp (car entry)))
    ;; (NAME . KEY) or (NAME KEY)
    (let ((key (if (consp (cdr entry)) (cadr entry) (cdr entry))))
      (list :name (car entry) :key (and (characterp key) key))))
   (t nil)))

(defun ps/context-tags-all ()
  "Return `ps/context-tags' as a list of canonical plists."
  (delq nil (mapcar #'ps/context-tags--normalize ps/context-tags)))

(defun ps/context-tags-org-tag-alist (&optional tags)
  "Return an `org-tag-alist' value derived from TAGS (default `ps/context-tags').
Entries keep their given order.  A `(:newline)' is inserted wherever the
`:kind' changes, so the fast-selection buffer shows one block per kind.  A tag
with no `:key' contributes a bare name string, which Org accepts."
  (let ((tags (or tags (ps/context-tags-all)))
        (prev-kind nil)
        (first t)
        result)
    (dolist (tag tags)
      (let ((kind (plist-get tag :kind))
            (key (plist-get tag :key))
            (name (plist-get tag :name)))
        (unless (or first (equal kind prev-kind))
          (push '(:newline) result))
        (push (if key (cons name key) name) result)
        (setq prev-kind kind first nil)))
    (nreverse result)))

(defun ps/context-tags--substring-collisions (names)
  "Return (A . B) pairs from NAMES where A is a substring of a different B.
Orgzly matches tags by substring, so such a pair makes one tag unselectable on
its own.  Comparison is case-sensitive, and a name is never paired with itself."
  (let (pairs)
    (dolist (a names)
      (dolist (b names)
        (when (and (not (equal a b)) (string-match-p (regexp-quote a) b))
          (push (cons a b) pairs))))
    (nreverse pairs)))

(defun ps/context-tags-lint ()
  "Report context tags that are substrings of other context tags.
Orgzly matches tags by substring, so a colliding pair cannot be queried apart."
  (interactive)
  (let* ((names (mapcar (lambda (tg) (plist-get tg :name)) (ps/context-tags-all)))
         (pairs (ps/context-tags--substring-collisions names)))
    (if (null pairs)
        (message "Context tags: %d tag%s, no substring collisions."
                 (length names) (if (= (length names) 1) "" "s"))
      (message "Context tags: %s"
               (mapconcat (lambda (p) (format "\"%s\" is inside \"%s\"" (car p) (cdr p)))
                          pairs "; ")))))

;;; Situations (pure)

(defun ps/situations--normalize (entry)
  "Return ENTRY as a canonical situation plist, or nil if unusable.
Accepts the plist form and the (KEY NAME QUERY) list shorthand."
  (cond
   ((and (consp entry) (keywordp (car entry)))
    (let ((key (plist-get entry :key))
          (query (plist-get entry :query)))
      (and (stringp key) (not (string-empty-p key))
           (stringp query) (not (string-empty-p query))
           entry)))
   ((and (consp entry) (stringp (car entry)) (listp (cdr entry)))
    (let ((key (nth 0 entry)) (name (nth 1 entry)) (query (nth 2 entry)))
      (and (stringp key) (stringp query)
           (list :key key :name (or name key) :query query))))
   (t nil)))

(defun ps/situations-all ()
  "Return `ps/situations' as a list of canonical plists."
  (delq nil (mapcar #'ps/situations--normalize ps/situations)))

(defun ps/situations-find (key)
  "Return the situation plist whose `:key' is KEY, or nil."
  (seq-find (lambda (s) (equal (plist-get s :key) key)) (ps/situations-all)))

(defun ps/situations--name (situation)
  "Return the display name of SITUATION."
  (or (plist-get situation :name) (plist-get situation :key) ""))

(defun ps/situations--description (situation)
  "Return \"Name (hint)\" for SITUATION, or just the name when it has no hint."
  (let ((name (ps/situations--name situation))
        (hint (plist-get situation :hint)))
    (if (and (stringp hint) (not (string-empty-p hint)))
        (format "%s (%s)" name hint)
      name)))

(defun ps/situations--command-key (key)
  "Return the agenda-dispatcher key string for situation KEY."
  (concat ps/situations-key-prefix key))

(defun ps/situations--custom-commands (&optional situations)
  "Return `org-agenda-custom-commands' entries for SITUATIONS.

Each entry is a one-block *series* -- see this file's Commentary for why the
block form is required and why `org-agenda-overriding-header' is left unset.
The command-wide settings tag the view for `ps-agenda-layout' and record which
situation built it."
  (mapcar
   (lambda (s)
     (let ((key (plist-get s :key))
           (query (plist-get s :query)))
       (list (ps/situations--command-key key)
             (ps/situations--description s)
             `((tags-todo ,query ((org-super-agenda-groups nil))))
             `((ps/agenda-layout-view-kind 'situation)
               (ps/situations-current ,key)))))
   (or situations (ps/situations-all))))

(defun ps/situations--register (existing &optional situations)
  "Return EXISTING custom commands with the situation entries refreshed.
Anything under `ps/situations-key-prefix' is dropped first, so re-running this
can neither duplicate entries nor reverse their order, and the entries defined
by config.org keep the top of the dispatcher."
  (append
   (seq-remove (lambda (cmd)
                 (and (consp cmd) (stringp (car cmd))
                      (string-prefix-p ps/situations-key-prefix (car cmd))))
               existing)
   (ps/situations--custom-commands situations)))

(defun ps/situations--menu-vectors (&optional situations)
  "Return easymenu vectors, one per situation."
  (mapcar (lambda (s)
            (let ((key (plist-get s :key)))
              (vector (ps/situations--description s)
                      `(ps/show-situation ,key)
                      :keys (format "C-c p S %s" key))))
          (or situations (ps/situations-all))))

(defun ps/situations--menu-filter (&optional _items)
  "Easymenu `:filter' for the Situations submenu.
Rebuilt from `ps/situations' each time the menu opens, so a situation added to
workspace.org appears after a reload with no menu edit."
  (or (ps/situations--menu-vectors)
      (list (vector "No situations defined" #'ignore :active nil))))

;;; Opening and switching

(defun ps/situations--read ()
  "Prompt for a situation with completion and return its plist."
  (let* ((all (ps/situations-all))
         (_ (unless all (user-error "No situations defined (see `ps/situations')")))
         (names (mapcar #'ps/situations--description all))
         (choice (completing-read "Situation: " names nil t)))
    (seq-find (lambda (s) (equal (ps/situations--description s) choice)) all)))

(defun ps/show-situation (key)
  "Open the situation whose `:key' is KEY."
  (interactive (list (plist-get (ps/situations--read) :key)))
  (require 'org-agenda)
  (if (ps/situations-find key)
      (org-agenda nil (ps/situations--command-key key))
    (user-error "No situation with key \"%s\"" key)))

(defun ps/situations-switch (&optional event)
  "Switch to another situation.
Called with the mouse (EVENT non-nil) this pops a menu at the click; from the
keyboard it completes over the situation names."
  (interactive (list last-nonmenu-event))
  (let ((all (ps/situations-all)))
    (unless all (user-error "No situations defined (see `ps/situations')"))
    (if (and event (not (integerp event)) (fboundp 'x-popup-menu))
        ;; A flat pane is right here: this is the in-buffer switcher, offering
        ;; only situations (the mode line's menu is the one that also carries
        ;; the other views, and is built as a nested keymap).
        (let ((choice (x-popup-menu
                       event
                       (list "Situations"
                             (cons "Situations"
                                   (mapcar (lambda (s)
                                             (cons (ps/situations--description s)
                                                   (plist-get s :key)))
                                           all))))))
          (when (stringp choice) (ps/show-situation choice)))
      (ps/show-situation (plist-get (ps/situations--read) :key)))))

;;; Agenda-view integration
;;
;; The plate itself is drawn by `ps-agenda-layout' (it owns the centring and the
;; button styling); these two functions are all it needs from us.

(defun ps/situations-plate-label (&optional key)
  "Return the plate label for situation KEY (default: this buffer's).
The name alone — the icon is returned separately by
`ps/situations-plate-icon', so that the *name* is what gets centred on the
plate rather than the name-plus-icon pair."
  (let ((s (ps/situations-find (or key ps/situations-current-key))))
    (if s (ps/situations--name s) "Situation")))

(defun ps/situations-plate-icon (&optional key)
  "Return situation KEY's icon as a one-character image string, or nil.
Nil when the situation declares no `:icon', the frame cannot show images, or
the Material Symbols font is unavailable."
  (let* ((s (ps/situations-find (or key ps/situations-current-key)))
         (icon (and s (plist-get s :icon)))
         (img (and icon (display-graphic-p)
                   (fboundp 'ps/material-icons-image)
                   (ps/material-icons-image icon))))
    (and img (propertize " " 'display img))))

(defun ps/situations--stash ()
  "Record which situation built this agenda buffer.
On `org-agenda-finalize-hook' ahead of every other view hook: a fresh build
always overwrites the buffer-local value -- nil for the Agenda, Calendar and
Tasks -- so a stale situation can never leak across views.  A resize re-layout
runs no finalize hook, so the stashed value is what survives it."
  (when (derived-mode-p 'org-agenda-mode)
    (setq-local ps/situations-current-key
                (and (boundp 'ps/situations-current) ps/situations-current))))

(defun ps/situations--tidy-header ()
  "Repair the header line of a Situation view before the plate is drawn on it.

Two fixes, both for things `org-tags-view' leaves behind, and both done here
because `ps/agenda-layout--apply' turns this line into the plate immediately
afterwards:

1. Strip the stray face off the line's trailing newline.  Inside a block series
   `org-agenda-multi' is non-nil, so `org-tags-view' skips the \"Press C-u r to
   search again\" line — but it still runs the `add-text-properties' meant for
   it, over a now-reversed range that Emacs normalises onto the preceding
   character.  `org-agenda-structure-secondary' therefore lands on the newline,
   which draws as a one-character coloured box past the end of the plate.

2. Add a real blank line beneath.  Org leaves one after the Calendar's block
   header, but a tag search runs straight into its first match, so without this
   the plate sits flush against the list.  A real newline, not an overlay
   string: those are not displayed at either end of this line (`after-string'
   here, `before-string' on the next were both tried), and a real line also
   survives a resize re-layout, which runs no finalize hook.  It is inserted
   with no text properties on purpose — inheriting the header's
   `org-agenda-structural-header' would make the layout and fold passes read
   the blank line as a second header."
  (when (and ps/situations-current-key (derived-mode-p 'org-agenda-mode))
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char (point-min))
        (let ((found nil))
          (while (and (not found) (not (eobp)))
            (if (org-get-at-bol 'org-agenda-structural-header)
                (setq found t)
              (forward-line 1)))
          (when found
            (let ((eol (line-end-position)))
              (when (< eol (point-max))
                (remove-text-properties eol (1+ eol) '(face nil))))
            (forward-line 1)
            (unless (or (eobp) (looking-at-p "^$"))
              (insert "\n"))))))))

(defun ps/situations--item-count ()
  "Return the number of real agenda item lines in the current buffer."
  (let ((n 0))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when (org-get-at-bol 'org-marker) (setq n (1+ n)))
        (forward-line 1)))
    n))

(defun ps/situations--empty-notice ()
  "Say so, in place of a bare header, when a situation view matches nothing.
An overlay `after-string' rather than inserted text, so it cannot disturb the
layout pass or org-agenda's own line markers.

Any previous notice is removed first: org-agenda rebuilds a view by erasing the
buffer, which leaves overlays behind (collapsed to position 1), so without this
a notice would follow the buffer into the next, non-empty view."
  (when (derived-mode-p 'org-agenda-mode)
    (remove-overlays (point-min) (point-max) 'ps/situations t)
    (when (and ps/situations-current-key
               (= (ps/situations--item-count) 0))
      (let ((ov (make-overlay (point-max) (point-max))))
        (overlay-put ov 'ps/situations t)
        (overlay-put ov 'after-string
                     (concat "\n  "
                             (propertize ps/situations-empty-message 'face 'shadow)
                             "\n"))))))

;;; Keymap

(defvar ps/situations-keymap (make-sparse-keymap)
  "Prefix keymap for the situation views, bound at `C-c p S'.
Rebuilt from `ps/situations' by `ps/situations-apply'.")

(defun ps/situations--rebuild-keymap ()
  "Repopulate `ps/situations-keymap' from `ps/situations'."
  (setcdr ps/situations-keymap nil)
  (dolist (s (ps/situations-all))
    (let ((key (plist-get s :key)))
      (define-key ps/situations-keymap (kbd key)
                  (lambda () (interactive) (ps/show-situation key)))))
  (define-key ps/situations-keymap (kbd "?") #'ps/situations-switch)
  ps/situations-keymap)

;;; Setup

;;;###autoload
(defun ps/situations-apply ()
  "Apply the context-tag and situation declarations.

Sets `org-tag-alist' from `ps/context-tags' (when non-nil), registers the
generated agenda commands, and rebuilds `ps/situations-keymap'.  Idempotent,
so it is also the workspace-reload entry point.

The agenda registration is deferred to the `org-agenda' load event so it lands
*after* config.org's own `setq' of `org-agenda-custom-commands' -- see this
file's Commentary."
  (when ps/context-tags
    (setq org-tag-alist (ps/context-tags-org-tag-alist)))
  (ps/situations--rebuild-keymap)
  (with-eval-after-load 'org-agenda
    (setq org-agenda-custom-commands
          (ps/situations--register org-agenda-custom-commands))))

;;;###autoload
(defun ps/situations-setup ()
  "Install the situation view's agenda hooks."
  ;; Ahead of `ps/mode-line--agenda-finalize' (-90), which reads the stash.
  (add-hook 'org-agenda-finalize-hook #'ps/situations--stash -95)
  ;; After the stash (it needs the key), before the layout pass turns the header
  ;; line into the plate.
  (add-hook 'org-agenda-finalize-hook #'ps/situations--tidy-header -80)
  ;; After the layout pass, so the control row exists before we measure.
  (add-hook 'org-agenda-finalize-hook #'ps/situations--empty-notice 90))

(provide 'ps-situations)
;;; ps-situations.el ends here
