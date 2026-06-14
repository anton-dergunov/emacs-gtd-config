;;; ps-agenda-fold.el --- Collapsible sections in the org agenda -*- lexical-binding: t; -*-

;;; Commentary:
;; Lets agenda section headers (the org-super-agenda group headers like
;; "Overdue:" and the structural headers like "High-priority:" / "In progress:")
;; be collapsed and expanded.  A header line gets a ▾/▸ indicator; TAB or a
;; mouse click on it toggles the body below via an `invisible' overlay.  Section
;; boundaries are detected from the same header text properties org-super-agenda
;; uses (`org-super-agenda-header' / `org-agenda-structural-header').
;;
;; Which sections are collapsed is remembered per agenda buffer and re-applied on
;; every render (so it survives `org-agenda-redo'), keyed by header text.

;;; Code:

(require 'subr-x)

(declare-function org-get-at-bol "org" (property))
(declare-function org-agenda-goto "org-agenda" (&optional highlight))
(declare-function mouse-set-point "mouse" (event &optional promote-to-region))
(defvar org-agenda-finalize-hook)
(defvar org-agenda-mode-map)
(defvar org-super-agenda-header-map)

(defgroup ps-agenda-fold nil
  "Collapsible sections in the org agenda."
  :group 'ps)

(defcustom ps/agenda-fold-default-collapsed nil
  "List of section header labels (e.g. \"In progress:\") collapsed on open."
  :type '(repeat string)
  :group 'ps-agenda-fold)

(defface ps/agenda-fold-indicator-face
  '((t :inherit shadow))
  "Face for the ▾/▸ collapse indicator on agenda headers."
  :group 'ps-agenda-fold)

(defvar-local ps/agenda-fold--collapsed nil
  "List of header labels currently collapsed in this agenda buffer.")

(defvar ps/agenda-fold--orig-tab nil
  "Command TAB was bound to in `org-agenda-mode-map' before our binding.")

(defvar ps/agenda-fold-header-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") #'ps/agenda-fold-toggle)
    (define-key map (kbd "<tab>") #'ps/agenda-fold-toggle)
    (define-key map [mouse-1] #'ps/agenda-fold-mouse-toggle)
    map)
  "Keymap applied as a text property to every collapsible header line.
Covers both org-super-agenda group headers and the plain
`org-agenda-structural-header' lines (e.g. \"High-priority:\"), which carry no
keymap of their own.")

;;; Header detection

(defun ps/agenda-fold--header-p ()
  "Non-nil when the line at point is a collapsible section header."
  (and (not (org-get-at-bol 'org-agenda-date-header))
       (or (org-get-at-bol 'org-super-agenda-header)
           (org-get-at-bol 'org-agenda-structural-header))))

(defun ps/agenda-fold--header-label ()
  "Return the trimmed visible text of the header line at point."
  (string-trim (buffer-substring-no-properties
                (line-beginning-position) (line-end-position))))

(defun ps/agenda-fold--body-region ()
  "Return (START . END) of the body following the header at point, or nil.
The body runs from the next line up to the next header or end of buffer.  A
trailing run of blank line(s) immediately before the next header (or eobp) is
excluded from the body, so it stays visible as a separator and the invisible
region never ends at the same position as the next header's indicator."
  (save-excursion
    (let ((start (min (point-max) (1+ (line-end-position))))
          (blank-start nil))
      (forward-line 1)
      (while (and (not (eobp)) (not (ps/agenda-fold--header-p)))
        (if (= (line-beginning-position) (line-end-position))
            (unless blank-start (setq blank-start (line-beginning-position)))
          (setq blank-start nil))
        (forward-line 1))
      (let ((end (or blank-start (point))))
        (and (> end start) (cons start end))))))

;;; Overlays

(defun ps/agenda-fold--set-indicator (bol collapsed)
  "Place/update the ▾/▸ indicator overlay at header line start BOL."
  (let ((ov (make-overlay bol bol)))
    (overlay-put ov 'ps/agenda-fold-indicator t)
    (overlay-put ov 'before-string
                 (propertize (if collapsed "▸ " "▾ ")
                             'face 'ps/agenda-fold-indicator-face))))

(defun ps/agenda-fold--collapse-here ()
  "Hide the body of the header at point with a `display'-overriding overlay."
  (when-let ((region (ps/agenda-fold--body-region)))
    (let ((ov (make-overlay (car region) (cdr region))))
      (overlay-put ov 'ps/agenda-fold t)
      (overlay-put ov 'evaporate t)
      ;; `invisible' alone does not hide characters whose own `display' property
      ;; is an `image' spec (e.g. the category-icon and pill images from
      ;; `ps/agenda-layout--apply'), leaving fragments of the first hidden item
      ;; visible.  Overriding `display' for the whole range hides everything,
      ;; images included.
      (overlay-put ov 'display ""))))

;;; Apply / toggle

(defun ps/agenda-fold--mark-clickable ()
  "Put the fold keymap and a hover highlight on the header line at point."
  (let ((bol (line-beginning-position))
        (eol (line-end-position))
        (inhibit-read-only t))
    (add-text-properties bol eol
                          (list 'keymap ps/agenda-fold-header-map
                                'mouse-face 'highlight))))

(defun ps/agenda-fold--apply ()
  "Add header indicators and re-collapse remembered sections."
  (when (derived-mode-p 'org-agenda-mode)
    (remove-overlays (point-min) (point-max) 'ps/agenda-fold t)
    (remove-overlays (point-min) (point-max) 'ps/agenda-fold-indicator t)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when (ps/agenda-fold--header-p)
          (let ((collapsed (member (ps/agenda-fold--header-label)
                                   ps/agenda-fold--collapsed)))
            (ps/agenda-fold--mark-clickable)
            (ps/agenda-fold--set-indicator (line-beginning-position) collapsed)
            (when collapsed (ps/agenda-fold--collapse-here))))
        (forward-line 1)))))

(defun ps/agenda-fold-toggle ()
  "Toggle the section under point; off a header, fall back to TAB's old command."
  (interactive)
  (if (ps/agenda-fold--header-p)
      (let ((label (ps/agenda-fold--header-label)))
        (if (member label ps/agenda-fold--collapsed)
            (setq ps/agenda-fold--collapsed
                  (delete label ps/agenda-fold--collapsed))
          (push label ps/agenda-fold--collapsed))
        (ps/agenda-fold--apply))
    (let ((cmd (or ps/agenda-fold--orig-tab #'org-agenda-goto)))
      (when (commandp cmd) (call-interactively cmd)))))

(defun ps/agenda-fold-mouse-toggle (event)
  "Toggle the section header clicked in EVENT."
  (interactive "e")
  (mouse-set-point event)
  (ps/agenda-fold-toggle))

(defun ps/agenda-fold--all-labels ()
  "Return the list of all section header labels in the buffer."
  (let (labels)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when (ps/agenda-fold--header-p)
          (push (ps/agenda-fold--header-label) labels))
        (forward-line 1)))
    (nreverse labels)))

(defun ps/agenda-fold-collapse-all ()
  "Collapse every section in the agenda."
  (interactive)
  (setq ps/agenda-fold--collapsed (ps/agenda-fold--all-labels))
  (ps/agenda-fold--apply))

(defun ps/agenda-fold-expand-all ()
  "Expand every section in the agenda."
  (interactive)
  (setq ps/agenda-fold--collapsed nil)
  (ps/agenda-fold--apply))

;;; Setup

(defun ps/agenda-fold--init-buffer ()
  "Seed the per-buffer collapsed set from the default on first render."
  (when (and (null ps/agenda-fold--collapsed)
             ps/agenda-fold-default-collapsed)
    (setq ps/agenda-fold--collapsed
          (copy-sequence ps/agenda-fold-default-collapsed))))

(defun ps/agenda-fold-setup ()
  "Enable collapsible agenda sections: bind keys and register the render hook.
Keymaps are bound through `with-eval-after-load' so this is safe to call before
org-agenda / org-super-agenda are loaded."
  (with-eval-after-load 'org-agenda
    (unless ps/agenda-fold--orig-tab
      (setq ps/agenda-fold--orig-tab (lookup-key org-agenda-mode-map (kbd "TAB"))))
    (define-key org-agenda-mode-map (kbd "TAB") #'ps/agenda-fold-toggle)
    (define-key org-agenda-mode-map (kbd "<tab>") #'ps/agenda-fold-toggle))
  (with-eval-after-load 'org-super-agenda
    (define-key org-super-agenda-header-map (kbd "TAB") #'ps/agenda-fold-toggle)
    (define-key org-super-agenda-header-map (kbd "<tab>") #'ps/agenda-fold-toggle)
    (define-key org-super-agenda-header-map [mouse-1] #'ps/agenda-fold-mouse-toggle))
  (add-hook 'org-agenda-finalize-hook #'ps/agenda-fold--init-buffer t)
  (add-hook 'org-agenda-finalize-hook #'ps/agenda-fold--apply t))

(provide 'ps-agenda-fold)
;;; ps-agenda-fold.el ends here
