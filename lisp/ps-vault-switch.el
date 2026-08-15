;;; ps-vault-switch.el --- Switching between vaults at runtime -*- lexical-binding: t; -*-

;;; Commentary:

;; Changing `my-org-base-directory' is one `setq'.  Making the rest of the
;; system agree with it is this file.  A dozen things are derived from the vault
;; directory when Emacs starts and then never reconsidered -- the journal folder,
;; `default-directory', the treemacs project root and icon theme, the git-sync
;; working directory -- and a further set comes from the vault's own
;; workspace.org.  `ps/vault-apply' is the ordered sequence that redoes all of
;; it, in place, without restarting Emacs.
;;
;; Three things about that sequence are load-bearing:
;;
;; - Teardown must *reset* the vault-scoped globals, not merely let the incoming
;;   vault overwrite them.  `ps/material-icons-add' merges rather than replaces,
;;   and workspace.org's plain `setq's only run if the new vault happens to set
;;   the same variable -- so without `ps/vault-restore-defaults' the old vault's
;;   categories, file sets, tags and situations survive into the new one.  See
;;   `ps/vault-scoped-variables'.
;;
;; - Generated buffers (the agenda and friends) must be killed *before* the file
;;   buffers they hold markers into, or the kill signals partway through and
;;   leaves the switch half-done.
;;
;; - Pending one-shot timers must be cancelled before the directory moves.  The
;;   AI-context timer is the dangerous one: it renders from whatever
;;   `my-org-base-directory' holds when it *fires*, so a timer armed by the
;;   outgoing vault would write that vault's content into the incoming vault's
;;   .claude/generated-context.md.
;;
;; Each step runs inside `ps/vault--step', which demotes an error to a message.
;; A switch that reports one broken step is recoverable; one that aborts halfway
;; leaves an Emacs whose agenda, tree and journal disagree about where they are.
;;
;; The treemacs re-root is asynchronous -- treemacs renders directories in
;; subprocesses, so the setup is re-run over a few seconds to catch late
;; updates.  Those timers carry a generation number, so switching twice in quick
;; succession cannot let the first switch's ladder re-add the old project.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'ps-vault)

(defvar my-org-base-directory)
(defvar org-agenda-buffer-name)
(defvar org-journal-dir)

(declare-function treemacs-canonical-path "treemacs")
(declare-function treemacs-get-local-buffer "treemacs")
(declare-function treemacs-select-window "treemacs")
(declare-function ps/file-tree-init "ps-file-tree")
(declare-function ps/file-tree-set-projects "ps-file-tree")
(declare-function ps/file-tree--ensure-valid-set "ps-file-tree")
(declare-function ps/file-tree-window-exists-p "ps-file-tree")
(declare-function ps/git-sync-stop "ps-git-sync")
(declare-function ps/git-sync-maybe-start "ps-git-sync")
(declare-function ps/situations-apply "ps-situations")
(declare-function ps/ai-context-sync "ps-ai-context")

(defcustom ps/vault-file-tree-init-delays '(0 0.3 0.8 1.5 3)
  "Seconds after a vault switch at which the file tree setup is re-run.
treemacs renders directories asynchronously, so one pass at the moment of the
switch loses to updates that arrive afterwards.  The setup is idempotent, so
repeating it simply settles."
  :type '(repeat number)
  :group 'ps-vault)

(defcustom ps/vault-generated-buffers
  '("*Org Availability*" "*Org Conflicts*" "*Org Blank Lines*"
    "*Org Blank Line Tree*" "*Org Git Sync*")
  "Buffers rendered *from* a vault, killed when leaving it.
Their content describes the outgoing vault and their markers point into its
files, so keeping them would be worse than losing them -- every one of them is
one keystroke to regenerate."
  :type '(repeat string)
  :group 'ps-vault)

;;; Step runner

(defvar ps/vault--failed-steps nil
  "Steps that signalled during the switch in progress.")

(defmacro ps/vault--step (name &rest body)
  "Run BODY, demoting any error to a message naming step NAME.
A vault switch has a dozen steps and no useful rollback: finishing the other
eleven and saying which one broke beats stopping in the middle."
  (declare (indent 1))
  `(condition-case error
       (progn ,@body)
     (error
      (push (cons ,name (error-message-string error)) ps/vault--failed-steps)
      (message "Vault switch: %s failed (%s)" ,name (error-message-string error))
      nil)))

;;; Teardown

(defun ps/vault--cancel-timers ()
  "Cancel pending one-shot timers that would act on the wrong vault."
  (dolist (symbol '(ps/ai-context--save-timer
                    ps/file-tree--follow-timer
                    ps/conflicts--agenda-timer
                    ps/agenda-emoji--timer))
    (when (and (boundp symbol) (timerp (symbol-value symbol)))
      (cancel-timer (symbol-value symbol))
      (set symbol nil)))
  (ps/vault--file-tree-init-cancel)
  ;; `ps/done--refade-timer' is buffer-local; the buffers holding one are about
  ;; to be killed, but a timer outliving its buffer errors on every tick.
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and (boundp 'ps/done--refade-timer)
                 (timerp ps/done--refade-timer))
        (cancel-timer ps/done--refade-timer)
        (setq ps/done--refade-timer nil)))))

(defun ps/vault--vault-buffers (root)
  "Return the file-visiting buffers under ROOT."
  (seq-filter (lambda (buffer)
                (when-let* ((file (buffer-file-name buffer)))
                  (ps/vault-path-under-p file root)))
              (buffer-list)))

(defun ps/vault--save-buffers (root)
  "Save modified files under ROOT.  Returns how many were saved.
Saving rather than asking matches how the rest of the system treats Org
buffers -- they are already saved on focus loss and on exit -- and it is the
only option here that cannot lose an edit."
  (let ((modified (seq-filter #'buffer-modified-p (ps/vault--vault-buffers root))))
    (dolist (buffer modified)
      (with-current-buffer buffer
        (let ((inhibit-message t))
          (save-buffer))))
    (length modified)))

(defun ps/vault--kill-generated-buffers ()
  "Kill the agenda and the other views rendered from the outgoing vault.
Runs before the file buffers are killed: these hold markers into those files,
and killing them in the other order signals partway through."
  (dolist (buffer (buffer-list))
    (let ((name (buffer-name buffer)))
      (when (and name
                 (or (member name ps/vault-generated-buffers)
                     ;; Sticky agenda buffers are named "*Org Agenda(c)*" etc.
                     (string-prefix-p "*Org Agenda" name)
                     (string-prefix-p "*proposed: " name)))
        (kill-buffer buffer)))))

(defun ps/vault--kill-vault-buffers (root)
  "Kill the file buffers under ROOT, which also releases their file watches."
  (dolist (buffer (ps/vault--vault-buffers root))
    (with-current-buffer buffer
      (set-buffer-modified-p nil))
    (kill-buffer buffer)))

;;; The treemacs ladder

(defvar ps/vault--file-tree-generation 0
  "Incremented per switch, so a previous switch's timers can tell they are stale.")

(defvar ps/vault--file-tree-timers nil
  "Pending timers of the current file-tree settle ladder.")

(defun ps/vault--file-tree-init-cancel ()
  "Cancel any pending file-tree settle timers."
  (dolist (timer ps/vault--file-tree-timers)
    (when (timerp timer) (cancel-timer timer)))
  (setq ps/vault--file-tree-timers nil))

(defun ps/vault--file-tree-init-now (root open)
  "Re-root the file tree at ROOT, opening its window when OPEN is non-nil."
  (when (require 'treemacs nil t)
    (let ((base (treemacs-canonical-path root)))
      (cond
       ((or open (ps/file-tree-window-exists-p))
        (save-selected-window
          (treemacs-select-window)
          (ps/file-tree-init base)))
       ((treemacs-get-local-buffer)
        (with-current-buffer (treemacs-get-local-buffer)
          (ps/file-tree-init base)))
       ;; Tree never opened: re-root the workspace anyway, or opening it later
       ;; would show the vault we just left.
       (t (ps/file-tree-set-projects base))))))

;;;###autoload
(defun ps/vault-file-tree-init-later (&optional root open)
  "Re-root the file tree at ROOT over `ps/vault-file-tree-init-delays'.
Cancels any ladder still running, so two switches in quick succession cannot
have the first one re-add the vault the second one left.  OPEN non-nil opens
the tree window, which is what startup wants and a switch does not.

treemacs is loaded when a timer *fires*, not when the ladder is scheduled: the
switch itself must not depend on a package that may not be installed."
  (ps/vault--file-tree-init-cancel)
  (when (ps/vault-configured-p)
    (let ((generation (cl-incf ps/vault--file-tree-generation))
          (base (or root my-org-base-directory)))
      (setq ps/vault--file-tree-timers
            (mapcar
             (lambda (delay)
               (run-with-timer
                delay nil
                (lambda ()
                  (when (= generation ps/vault--file-tree-generation)
                    (ignore-errors (ps/vault--file-tree-init-now base open))))))
             ps/vault-file-tree-init-delays)))))

;;; Apply

(defun ps/vault--rederive ()
  "Re-run everything derived from the vault directory, in dependency order."
  (ps/vault--step "workspace.org"
    (when (fboundp 'ps/load-workspace-config) (ps/load-workspace-config)))
  ;; After workspace.org, because the valid file sets are what it just defined.
  (ps/vault--step "file set"
    (when (fboundp 'ps/file-tree--ensure-valid-set) (ps/file-tree--ensure-valid-set)))
  (ps/vault--step "icons"
    (when (fboundp 'ps/icons-reapply) (ps/icons-reapply)))
  (ps/vault--step "situations"
    (when (fboundp 'ps/situations-apply) (ps/situations-apply)))
  (ps/vault--step "agenda files"
    (when (fboundp 'ps/agenda-files-refresh) (ps/agenda-files-refresh)))
  (ps/vault--step "file tree"
    (ps/vault-file-tree-init-later))
  (ps/vault--step "git sync"
    (when (fboundp 'ps/git-sync-maybe-start)
      (ps/git-sync-maybe-start my-org-base-directory)))
  (ps/vault--step "AI context"
    (when (fboundp 'ps/ai-context-sync) (ps/ai-context-sync))))

;;;###autoload
(defun ps/vault-apply (root)
  "Make ROOT the open vault, re-pointing everything derived from the old one.
The whole switch, in order: save and remember the outgoing vault, tear down
its buffers, timers and sync, reset the vault-scoped globals, move
`my-org-base-directory', then re-derive.  Returns ROOT."
  (let* ((root (ps/vault--normalize-path root))
         (previous (ps/vault-current))
         (ps/vault--failed-steps nil)
         (saved 0))
    (unless root (user-error "No vault given"))
    (unless (file-directory-p root)
      (user-error "%s is not a directory" root))

    ;; Leaving the outgoing vault.
    (when (ps/vault-configured-p)
      (ps/vault--step "saving files" (setq saved (ps/vault--save-buffers previous)))
      (ps/vault--step "vault state" (ps/vault-state-save previous)))

    (ps/vault--step "timers" (ps/vault--cancel-timers))
    (ps/vault--step "git sync teardown"
      (when (fboundp 'ps/git-sync-stop) (ps/git-sync-stop)))
    (ps/vault--step "views" (ps/vault--kill-generated-buffers))
    (when previous
      (ps/vault--step "buffers" (ps/vault--kill-vault-buffers previous)))
    (ps/vault--step "settings reset" (ps/vault-restore-defaults))

    ;; Entering the new vault.  These three mirror the config.org blocks that
    ;; set them at startup; they are the derivations with nowhere else to live.
    (setq my-org-base-directory root)
    (setq default-directory root)
    (setq org-journal-dir (expand-file-name "Journal/" root))
    (ps/vault--step "registry"
      (ps/vault-registry-update
       (lambda (registry) (ps/vault--registry-set-current registry root))))
    (ps/vault--step "vault state" (ps/vault-state-apply root))

    (ps/vault--rederive)

    (force-mode-line-update t)
    (message "Vault: %s%s%s"
             (or (ps/vault-name) root)
             (if (> saved 0) (format " (saved %d file%s)" saved
                                     (if (= saved 1) "" "s"))
               "")
             (if ps/vault--failed-steps
                 (format " — %s did not complete"
                         (string-join (mapcar #'car (reverse ps/vault--failed-steps))
                                      ", "))
               ""))
    root))

;;; Commands

(defun ps/vault--read (prompt)
  "Read a known vault path with PROMPT, offering its display names."
  (let* ((vaults (ps/vault-known))
         (labels (ps/vault-menu-labels vaults))
         (table (cl-mapcar (lambda (label entry)
                             (cons label (ps/vault--entry-path entry)))
                           labels vaults)))
    (unless table (user-error "No vaults yet — use `ps/vault-create' first"))
    (cdr (assoc (completing-read prompt table nil t) table))))

;;;###autoload
(defun ps/vault-switch (path)
  "Switch to the vault at PATH, re-initializing everything that reads from it."
  (interactive (list (ps/vault--read "Switch to vault: ")))
  (if (equal (ps/vault--normalize-path path) (ps/vault-current))
      (message "Already in %s" (or (ps/vault-name) path))
    (ps/vault-apply path)))

;;;###autoload
(defun ps/vault-create (directory name)
  "Create a vault in DIRECTORY called NAME, scaffold it, and switch to it."
  (interactive
   (let* ((directory (read-directory-name "New vault directory: "))
          (name (read-string "Vault name: "
                             (ps/vault--directory-name directory))))
     (list directory name)))
  (let ((root (ps/vault--normalize-path directory)))
    (unless (file-directory-p root)
      (make-directory root t))
    (when-let* ((problem (ps/vault-validate root)))
      (user-error "%s" problem))
    (ps/vault-scaffold root name)
    (ps/vault-registry-update
     (lambda (registry) (ps/vault--registry-add registry root name)))
    (ps/vault-apply root)))

;;;###autoload
(defun ps/vault-open-existing (directory)
  "Add the existing folder DIRECTORY as a vault and switch to it."
  (interactive (list (read-directory-name "Open folder as vault: ")))
  (let ((root (ps/vault--normalize-path directory)))
    (when-let* ((problem (ps/vault-validate root)))
      (user-error "%s" problem))
    (when (and (ps/vault-path-under-p root user-emacs-directory)
               (not (yes-or-no-p
                     (format "%s is inside the Emacs config directory.  Use it anyway? "
                             root))))
      (user-error "Cancelled"))
    (ps/vault-registry-update
     (lambda (registry) (ps/vault--registry-add registry root)))
    (ps/vault-apply root)))

;;;###autoload
(defun ps/vault-rename (name)
  "Rename the current vault to NAME in the vault list.
Renames the label only -- the folder on disk keeps its name.  A blank NAME
goes back to showing the folder's own name."
  (interactive
   (progn
     (unless (ps/vault-configured-p) (user-error "No vault is open"))
     (list (read-string "Vault name: " (ps/vault-name)))))
  (let ((root (ps/vault-current)))
    (ps/vault-registry-update
     (lambda (registry) (ps/vault--registry-rename registry root name)))
    (force-mode-line-update t)
    (message "Vault renamed to %s" (ps/vault-name))))

;;;###autoload
(defun ps/vault-forget (path)
  "Remove the vault at PATH from the vault list, leaving the folder untouched.
Forgetting the open vault switches to another one, or leaves none open when it
was the last."
  (interactive (list (ps/vault--read "Remove vault from list: ")))
  (let* ((root (ps/vault--normalize-path path))
         (current (equal root (ps/vault-current)))
         (registry (ps/vault-registry-update
                    (lambda (registry) (ps/vault--registry-remove registry root)))))
    (cond
     ((not current)
      (message "Removed %s from the vault list" root))
     ((ps/vault--registry-fallback registry)
      (ps/vault-apply (ps/vault--registry-fallback registry)))
     (t
      (message "Removed %s — no vaults left" root)
      (when (fboundp 'ps/vault-welcome) (ps/vault-welcome))))))

;;; Header-line chip

(defun ps/vault--chip-menu-items ()
  "Return the (LABEL . VALUE) rows of the vault popup.
The open vault is marked with a bullet rather than a radio button, because
`x-popup-menu' has no radio style."
  (let* ((vaults (ps/vault-known))
         (current (ps/vault-current))
         (labels (ps/vault-menu-labels vaults)))
    (append
     (cl-mapcar (lambda (label entry)
                  (let ((path (ps/vault--entry-path entry)))
                    (cons (format "%s %s" (if (equal path current) "●" " ") label)
                          path)))
                labels vaults)
     (list (cons "--" nil)
           (cons "Create New Vault…" :create)
           (cons "Open Existing Folder…" :open)
           (cons "Rename This Vault…" :rename)
           (cons "Remove From List…" :forget)))))

(defun ps/vault--chip-click (event)
  "Show the vault popup for EVENT and act on what it selects."
  (interactive "e")
  (let ((choice (x-popup-menu event
                              (list "Vault" (cons "Vaults" (ps/vault--chip-menu-items))))))
    (pcase choice
      ((pred stringp) (ps/vault-switch choice))
      (:create (call-interactively #'ps/vault-create))
      (:open (call-interactively #'ps/vault-open-existing))
      (:rename (call-interactively #'ps/vault-rename))
      (:forget (call-interactively #'ps/vault-forget))
      (_ nil))))

(defun ps/vault--chip ()
  "Return the clickable vault name for the file tree's header line.
Shaped like the file-set chip in the tree's mode line, but bound on
`header-line' rather than `mode-line' -- the wrong one leaves a chip that
looks live and does nothing when clicked."
  (propertize (format " %s ▾" (ps/vault-chip-label (or (ps/vault-entry)
                                                       (ps/vault-current))))
              'face 'mode-line-emphasis
              'mouse-face 'mode-line-highlight
              'help-echo "mouse-1: switch vault"
              'local-map
              (let ((map (make-sparse-keymap)))
                (define-key map [header-line mouse-1] #'ps/vault--chip-click)
                map)))

;;; Menu

(defun ps/vault--menu-filter (&optional _items)
  "Return the Vault menu, rebuilt from the vault list each time it opens."
  (let ((vaults (ps/vault-known))
        (current (ps/vault-current)))
    (append
     (or (cl-mapcar
          (lambda (label entry)
            (let ((path (ps/vault--entry-path entry)))
              (vector label `(ps/vault-switch ,path)
                      :style 'radio
                      :selected (equal path current))))
          (ps/vault-menu-labels vaults) vaults)
         (list (vector "No vaults yet" #'ignore :active nil)))
     (list "---"
           (vector "Create New Vault…" #'ps/vault-create t)
           (vector "Open Existing Folder…" #'ps/vault-open-existing t)
           (vector "Rename This Vault…" #'ps/vault-rename
                   :active '(ps/vault-configured-p))
           (vector "Remove From List…" #'ps/vault-forget
                   :active '(ps/vault-known))))))

(provide 'ps-vault-switch)
;;; ps-vault-switch.el ends here
