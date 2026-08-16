;;; ps-nav.el --- Browser-style back and forward, per window -*- lexical-binding: t; -*-

;;; Commentary:
;; Emacs has no "back".  It has `pop-global-mark', `org-mark-ring-goto', Dired's
;; `^', a per-mode history in eww and Info -- each of which knows about one kind
;; of thing and none of which knows about the others.  Following the Info Triage
;; queue into an item's index, into its directory, into a file inside it crosses
;; three of those in four keystrokes, so this is the one that covers the trail.
;;
;; The trail is recorded twice over, deliberately.  `ps-window.el's navigation
;; helpers call `ps/nav-note-departure' as they go, which is synchronous with
;; the navigation; `window-buffer-change-functions' catches everything else, but
;; only at redisplay, so it alone would make the trail depend on when Emacs next
;; got round to drawing.  `ps/nav--push' collapses a repeat, so the overlap
;; costs nothing.
;;
;; The history lives in *window parameters*, so two windows keep independent
;; trails and closing a window forgets its own.  A place records where to reopen
;; something, not what was open: a file path, a directory, or -- for a generated
;; buffer with no file behind it -- the buffer itself.  Recording the path is
;; load-bearing rather than tidy, because `dired-kill-when-opening-new-dired-buffer'
;; (config.org) kills a Dired buffer as you descend out of it, so by the time
;; you press back the buffer that place named is gone.
;;
;; Two things about the mode line are easy to get wrong.  The segment must be
;; installed BOTH into the default `mode-line-format' and into the buffer-local
;; format `ps-mode-line.el' gives Org and agenda buffers -- there are two mode
;; lines in this config and a segment in one of them is a button that works in
;; half the frame.  And in Org buffers it must stay *outside*
;; `ps/mode-line--render-window-cached': that renderer caches per window on a key
;; that has nothing to do with navigation, so a cached arrow would keep pointing
;; at the buffer you already left.

;;; Code:

(require 'seq)

(declare-function ps/window--side-window-p "ps-window")
(declare-function ps/window-visit-here "ps-window")

(defgroup ps/nav nil
  "Back and forward navigation across windows."
  :group 'ps)

(defcustom ps/nav-history-limit 30
  "How many places back each window remembers."
  :type 'integer
  :group 'ps/nav)

(defcustom ps/nav-ignore-buffers
  '("\\` " "\\`\\*Echo Area" "\\`\\*Minibuf" "\\`\\*Completions\\*\\'"
    "\\`\\*claude" "\\`\\*Async-native-compile-log\\*\\'")
  "Buffers that never enter a navigation history, as name regexps.
Transient and terminal buffers only: a trail whose steps you cannot mean to
return to is worse than no trail.  `*Help*' and the planning views are
deliberately absent -- going back out of them is exactly the point."
  :type '(repeat regexp)
  :group 'ps/nav)

(defcustom ps/nav-back-glyph "‹"
  "Glyph for the mode line's back button."
  :type 'string
  :group 'ps/nav)

(defcustom ps/nav-forward-glyph "›"
  "Glyph for the mode line's forward button."
  :type 'string
  :group 'ps/nav)

;;; Places

(defun ps/nav--trackable-buffer-p (buffer)
  "Non-nil when BUFFER is worth remembering having visited."
  (and (buffer-live-p buffer)
       (let ((name (buffer-name buffer)))
         (not (seq-some (lambda (rx) (string-match-p rx name))
                        ps/nav-ignore-buffers)))))

(defun ps/nav--trackable-window-p (window)
  "Non-nil when WINDOW is a content window that keeps a history.
Side windows -- the file tree, the Claude Code panel -- are docks: their
buffer is the point of them, so there is nothing to go back to."
  (and (window-live-p window)
       (not (window-minibuffer-p window))
       (not (ps/window--side-window-p window))))

(defun ps/nav--place (buffer)
  "Return a place for BUFFER: (TARGET . POSITION), or nil if it is not trackable.

TARGET is a file name, a directory, or the buffer itself when neither exists.
Preferring the name over the buffer is what lets a place outlive the buffer --
Dired buffers in particular are killed as you descend out of them."
  (when (ps/nav--trackable-buffer-p buffer)
    (with-current-buffer buffer
      (cons (cond (buffer-file-name)
                  ((derived-mode-p 'dired-mode) (expand-file-name default-directory))
                  (t buffer))
            (point)))))

(defun ps/nav--place-name (place)
  "Return a short human name for PLACE, for a tooltip."
  (let ((target (car place)))
    (cond ((bufferp target) (buffer-name target))
          ((directory-name-p target)
           (file-name-nondirectory (directory-file-name target)))
          (t (file-name-nondirectory target)))))

(defun ps/nav--reachable-p (place)
  "Non-nil when PLACE can still be returned to."
  (let ((target (car place)))
    (if (bufferp target) (buffer-live-p target) (file-exists-p target))))

(defun ps/nav--restore (place)
  "Show PLACE in the selected window and move point into it."
  (let ((target (car place)))
    (if (bufferp target)
        (switch-to-buffer target)
      (ps/window-visit-here target)))
  (when (<= (cdr place) (point-max))
    (goto-char (cdr place))))

;;; History

(defun ps/nav--stack (window direction)
  "Return WINDOW's history stack for DIRECTION, one of `back' or `forward'."
  (window-parameter window (if (eq direction 'back) 'ps/nav-back 'ps/nav-forward)))

(defun ps/nav--set-stack (window direction places)
  (set-window-parameter window
                        (if (eq direction 'back) 'ps/nav-back 'ps/nav-forward)
                        places))

(defun ps/nav--push (place places)
  "Return PLACES with PLACE on top, deduplicated and capped.
Consecutive visits to the same target collapse: re-rendering the agenda in
place, or saving and reverting a file, is not a step in a trail."
  (if (null place)
      places
    (let ((rest (if (and places (equal (car (car places)) (car place)))
                    (cdr places)
                  places)))
      (seq-take (cons place rest) ps/nav-history-limit))))

(defvar ps/nav--in-transit nil
  "Non-nil while `ps/nav--go' is moving, so a move is not recorded as a step.")

;;;###autoload
(defun ps/nav-note-departure (&optional window)
  "Record WINDOW's current buffer as somewhere to come back to.

Called by the navigation helpers in `ps-window.el' *before* they replace the
buffer.  `window-buffer-change-functions' alone would be enough in principle,
but it runs at redisplay rather than when the navigation happens, so the trail
would depend on when Emacs next got round to drawing.  The two overlap
harmlessly: `ps/nav--push' collapses a repeat of the same target."
  (let ((window (or window (selected-window))))
    (when (and (not ps/nav--in-transit)
               (ps/nav--trackable-window-p window))
      (when-let* ((place (ps/nav--place (window-buffer window))))
        (ps/nav--set-stack window 'back
                           (ps/nav--push place (ps/nav--stack window 'back)))
        ;; Going somewhere new abandons the forward trail, as in a browser.
        (ps/nav--set-stack window 'forward nil)))))

(defun ps/nav--record (window-or-frame)
  "Note that a window's buffer changed, for `window-buffer-change-functions'.
That hook passes a window when a window's buffer changed and a frame when
windows were created or deleted, so the argument is checked rather than
assumed."
  (when (and (windowp window-or-frame)
             (ps/nav--trackable-window-p window-or-frame))
    (let ((previous (window-parameter window-or-frame 'ps/nav--current))
          (current (window-buffer window-or-frame)))
      (unless (eq previous current)
        (when-let* ((place (and (buffer-live-p previous) (ps/nav--place previous))))
          (ps/nav--set-stack window-or-frame 'back
                             (ps/nav--push place (ps/nav--stack window-or-frame 'back)))
          ;; Going somewhere new abandons the forward trail, as in a browser.
          (ps/nav--set-stack window-or-frame 'forward nil))
        (set-window-parameter window-or-frame 'ps/nav--current current)))))

(defun ps/nav--go (direction)
  "Move DIRECTION (`back' or `forward') in the selected window's history."
  (let ((window (selected-window)))
    (unless (ps/nav--trackable-window-p window)
      (user-error "This window keeps no navigation history"))
    ;; Drop places whose file was deleted or whose buffer was killed, rather
    ;; than failing on them: a dropped item's directory is exactly such a place.
    (let ((stack (seq-drop-while (lambda (place) (not (ps/nav--reachable-p place)))
                                 (ps/nav--stack window direction))))
      (ps/nav--set-stack window direction stack)
      (unless stack
        (user-error "Nothing to go %s to" direction))
      (let ((here (ps/nav--place (window-buffer window)))
            (opposite (if (eq direction 'back) 'forward 'back)))
        (ps/nav--set-stack window direction (cdr stack))
        (ps/nav--set-stack window opposite
                           (ps/nav--push here (ps/nav--stack window opposite)))
        ;; The restore goes through the same helpers a click does, and those
        ;; call `ps/nav-note-departure' -- which would push this step straight
        ;; back onto the stack it just came off.
        (let ((ps/nav--in-transit t))
          (ps/nav--restore (car stack)))
        ;; Claim the arrival ourselves, so the recorder -- which runs later, at
        ;; redisplay -- sees no change and does not push this step back on.
        (set-window-parameter window 'ps/nav--current (window-buffer window))))))

;;;###autoload
(defun ps/nav-back ()
  "Return to the previous place in this window."
  (interactive)
  (ps/nav--go 'back))

;;;###autoload
(defun ps/nav-forward ()
  "Go forward again to the place `ps/nav-back' came from."
  (interactive)
  (ps/nav--go 'forward))

;;; Mode line

(defvar ps/nav--back-map
  (let ((map (make-sparse-keymap)))
    ;; mouse-1 only: mouse-2 and mouse-3 are disabled across the mode line by
    ;; `ps/mode-line--disable-destructive-mouse'.
    (define-key map [mode-line mouse-1] #'ps/nav-back)
    map)
  "Keymap on the mode line's back button.")

(defvar ps/nav--forward-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] #'ps/nav-forward)
    map)
  "Keymap on the mode line's forward button.")

(defun ps/nav--button (direction)
  "Return the propertized mode-line button for DIRECTION.
Dimmed and inert when that direction is empty, so the pair reads as a state
rather than as two buttons that sometimes do nothing."
  (let ((glyph (if (eq direction 'back) ps/nav-back-glyph ps/nav-forward-glyph))
        (place (car (seq-drop-while (lambda (p) (not (ps/nav--reachable-p p)))
                                    (ps/nav--stack (selected-window) direction)))))
    (if (null place)
        (propertize glyph 'face 'shadow)
      (propertize glyph
                  'mouse-face 'mode-line-highlight
                  'help-echo (format "mouse-1: %s to %s"
                                     (if (eq direction 'back) "back" "forward")
                                     (ps/nav--place-name place))
                  'local-map (if (eq direction 'back)
                                 ps/nav--back-map
                               ps/nav--forward-map)))))

(defun ps/nav--segment ()
  "Return the back/forward pair for the mode line, or \"\" where it makes no sense."
  (if (not (ps/nav--trackable-window-p (selected-window)))
      ""
    (concat " " (ps/nav--button 'back) (ps/nav--button 'forward))))

(defconst ps/nav-mode-line-element '(:eval (ps/nav--segment))
  "The mode-line element that draws the back/forward pair.
A `defconst' so `ps/nav-mode-line-add' can recognise its own work and stay
idempotent across a config reload.")

(defun ps/nav-mode-line-add (format)
  "Return mode-line FORMAT with the navigation buttons in front of it.
Idempotent: reloading config.org must not stack a second pair of arrows."
  (if (and (listp format) (member ps/nav-mode-line-element format))
      format
    (cons ps/nav-mode-line-element (if (listp format) format (list format)))))

;;;###autoload
(defun ps/nav-setup ()
  "Start recording navigation history and put the buttons on the mode line.
Idempotent, so it survives a config reload."
  (add-hook 'window-buffer-change-functions #'ps/nav--record)
  (setq-default mode-line-format (ps/nav-mode-line-add (default-value 'mode-line-format))))

(provide 'ps-nav)
;;; ps-nav.el ends here
