;;; ps-prose-width.el --- Centred reading-width margins for Org prose -*- lexical-binding: t; -*-

;;; Commentary:

;; Org buffers wrap with `visual-line-mode' at the window edge, which is
;; comfortable for editing but hard to read on a maximized window, unlike
;; Obsidian's centred, max-width prose column.  This adds the same effect via
;; `visual-fill-column-mode': text is capped at `ps/prose-width-columns'
;; characters and centred, with margins that shrink back to zero once the
;; window itself is narrower than that value.
;;
;; Scope is deliberately narrow: only the actual GTD/plan files the agenda
;; itself scans (`ps/org-files-in-scope-p') get margins -- not `config.org',
;; `workspace.org', the journal/archive, or any generated view (Agenda,
;; Tasks, Calendar, Schedule/Timeline all live in the `org-agenda-mode'
;; buffer, a different major mode).  The blank-line-recovery review (and any
;; other Ediff session) is excluded explicitly, since splitting the frame
;; only usually leaves each pane narrower than the threshold, not always.

;;; Code:

(require 'ps-org-files)

;; Provided by visual-fill-column; declared here so this file loads (and its
;; pure logic is testable) without the package installed.
(defvar visual-fill-column-width)
(defvar visual-fill-column-center-text)
(declare-function visual-fill-column-mode "visual-fill-column")

;; Provided by ediff; declared here for the same reason.
(defvar ediff-buffer-A)
(defvar ediff-buffer-B)
(defvar ediff-prepare-buffer-hook)
(defvar ediff-quit-hook)

;;; Customization

(defcustom ps/prose-width-columns 120
  "Maximum text width, in characters, for Org prose buffers.
`visual-fill-column-mode' centres the text within this width and pads the
rest with window margins; the margins shrink to zero once the window itself
is narrower than this value, so nothing is lost on a split or a narrow
frame.  Set to 0 to disable margins entirely."
  :type 'natnum)

;;; Enable/disable

(defun ps/prose-width--enabled-p ()
  "Non-nil if margins should be applied to the current buffer.
True when `ps/prose-width-columns' is positive and the visited file is one
of the actual GTD/plan files the agenda scans, per
`ps/org-files-in-scope-p'."
  (and (> ps/prose-width-columns 0)
       (ps/org-files-in-scope-p)))

(defun ps/prose-width-enable ()
  "Enable centred-width margins in the current buffer, if in scope."
  (when (ps/prose-width--enabled-p)
    (setq-local visual-fill-column-width ps/prose-width-columns
                visual-fill-column-center-text t)
    (visual-fill-column-mode 1)))

;;; Ediff

(defun ps/prose-width--ediff-disable ()
  "Turn off margins in the current Ediff participant buffer, if on."
  (when (bound-and-true-p visual-fill-column-mode)
    (visual-fill-column-mode -1)))

(defun ps/prose-width--ediff-restore ()
  "Re-enable margins in the Ediff session's file buffers, if in scope.
Runs once per session rather than per buffer, since `ediff-quit-hook' does
not run in each participant buffer the way `ediff-prepare-buffer-hook' does."
  (when ediff-buffer-A
    (with-current-buffer ediff-buffer-A (ps/prose-width-enable)))
  (when ediff-buffer-B
    (with-current-buffer ediff-buffer-B (ps/prose-width-enable))))

;;;###autoload
(defun ps/prose-width-setup ()
  "Keep margins off for the duration of any Ediff session."
  (add-hook 'ediff-prepare-buffer-hook #'ps/prose-width--ediff-disable)
  (add-hook 'ediff-quit-hook #'ps/prose-width--ediff-restore))

(provide 'ps-prose-width)
;;; ps-prose-width.el ends here
