;;; ps-vault-welcome.el --- First-run screen when no vault is open -*- lexical-binding: t; -*-

;;; Commentary:

;; Emacs can now start with no vault at all -- on a first install, or when the
;; folder a vault pointed at has gone.  Rather than an empty frame that looks
;; like something failed, this offers the two things there are to do: create a
;; vault, or open a folder that already holds Org files.
;;
;; Two details are borrowed from the blank-lines report, where they were learned
;; the hard way.  The screen is shown through `ps/window-show-here', which
;; selects a main window first -- rendering into the file tree's side window
;; makes the command look like it did nothing.  And every button carries its
;; target in a button property rather than reading it back from point, because
;; `push-button' does not move point on a mouse click, so a screen that reads
;; point would act on whichever row the keyboard happened to leave it at.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'ps-vault)

(declare-function ps/vault-create "ps-vault-switch")
(declare-function ps/vault-open-existing "ps-vault-switch")
(declare-function ps/vault-switch "ps-vault-switch")
(declare-function ps/window-show-here "ps-window")

(defconst ps/vault-welcome-buffer "*Welcome*"
  "Name of the first-run buffer.")

(defvar ps/vault-welcome-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'ps/vault-welcome-create)
    (define-key map (kbd "o") #'ps/vault-welcome-open)
    (define-key map (kbd "g") #'ps/vault-welcome-refresh)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `ps/vault-welcome-mode'.")

(define-derived-mode ps/vault-welcome-mode special-mode "Welcome"
  "Major mode for the first-run vault screen."
  (setq-local cursor-type nil)
  (setq-local truncate-lines nil))

;;; Rendering (pure)

(defun ps/vault-welcome--missing (registry)
  "Return the known vault paths in REGISTRY whose folder is not there."
  (seq-remove #'file-directory-p
              (delq nil (mapcar #'ps/vault--entry-path
                                (ps/vault--registry-vaults registry)))))

(defun ps/vault-welcome--intro (registry)
  "Return the opening lines of the screen for REGISTRY.
Which story the screen tells depends on whether this is a genuinely first run
or a vault that has gone missing -- the second needs saying, or the vault list
looks like it lost the notes rather than the path to them."
  (let ((missing (ps/vault-welcome--missing registry)))
    (concat
     "No vault is open.\n\n"
     "A vault is a folder of Org files. Your agenda, file tree, journal and\n"
     "searches all come from one vault at a time, and you can keep as many as\n"
     "you like — work and personal, say — and switch between them.\n\n"
     (when missing
       (concat
        "These vaults are in your list but their folders are not there:\n"
        (mapconcat (lambda (path) (format "    %s\n" path)) missing "")
        "If one is on a drive or sync folder that has not mounted yet, open it\n"
        "again once it has.\n\n")))))

;;; Rendering (impure)

(defun ps/vault-welcome--insert-button (label help action &optional path)
  "Insert a button reading LABEL with tooltip HELP, running ACTION.
PATH, when given, is stored on the button and passed to ACTION -- never read
back from point, which a mouse click does not move."
  (insert "  ")
  (insert-button label
                 'action (lambda (button)
                           (funcall action (button-get button 'ps-vault-path)))
                 'ps-vault-path path
                 'help-echo help
                 'follow-link t)
  (insert "\n"))

(defun ps/vault-welcome--render (registry)
  "Insert the whole screen for REGISTRY into the current buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (ps/vault-welcome--intro registry))
    (when-let* ((vaults (ps/vault--registry-vaults registry)))
      (insert "Vaults you have used:\n\n")
      (cl-loop for entry in vaults
               for label in (ps/vault-menu-labels vaults)
               for path = (ps/vault--entry-path entry)
               for there = (file-directory-p path)
               do (ps/vault-welcome--insert-button
                   (format "%-24s %s" label
                           (if there path (concat path "  (missing)")))
                   (if there "Open this vault" "This folder is not there")
                   (lambda (path)
                     (if (file-directory-p path)
                         (ps/vault-switch path)
                       (user-error "%s is not there" path)))
                   path))
      (insert "\n"))
    (insert "What would you like to do?\n\n")
    (ps/vault-welcome--insert-button
     "Create a new vault…   (n)" "Pick an empty folder and set it up"
     (lambda (_) (call-interactively #'ps/vault-create)))
    (ps/vault-welcome--insert-button
     "Open an existing folder…   (o)" "Use a folder that already holds Org files"
     (lambda (_) (call-interactively #'ps/vault-open-existing)))
    (insert "\nOnce a vault is open you can switch between them from the name at\n"
            "the top of the file tree, from Productivity → Vault, or with C-c p V.\n")
    (goto-char (point-min))))

;;; Commands

(defun ps/vault-welcome-create (&optional _)
  "Create a new vault from the welcome screen."
  (interactive)
  (call-interactively #'ps/vault-create))

(defun ps/vault-welcome-open (&optional _)
  "Open an existing folder as a vault from the welcome screen."
  (interactive)
  (call-interactively #'ps/vault-open-existing))

(defun ps/vault-welcome-refresh ()
  "Redraw the welcome screen, e.g. after a missing folder has appeared."
  (interactive)
  (when (get-buffer ps/vault-welcome-buffer)
    (with-current-buffer ps/vault-welcome-buffer
      (ps/vault-welcome--render (ps/vault-registry-load)))))

;;;###autoload
(defun ps/vault-welcome ()
  "Show the first-run screen offering to create or open a vault."
  (interactive)
  (let ((buffer (get-buffer-create ps/vault-welcome-buffer)))
    (with-current-buffer buffer
      (ps/vault-welcome-mode)
      (ps/vault-welcome--render (ps/vault-registry-load)))
    (if (fboundp 'ps/window-show-here)
        (ps/window-show-here buffer)
      (switch-to-buffer buffer))))

;;;###autoload
(defun ps/vault-welcome-maybe-show ()
  "Show the welcome screen if startup finished without a vault."
  (when ps/vault--needs-welcome
    (ps/vault-welcome)))

(provide 'ps-vault-welcome)
;;; ps-vault-welcome.el ends here
