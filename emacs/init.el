;; Installation instructions:
;; 1. Create file ~/.emacs and put a line to load this config. For example, for Windows:
;;   (load "C:\\MyWork\\Dropbox\\config\\emacs\\init.el")
;; 2. Refresh packages:
;;   M-x package-refresh-contents RET
;; 3. Install the required packages:
;;   M-x package-install RET magit RET
;;   M-x package-install RET org-superstar RET
;;   M-x package-install RET solarized-theme RET
;;   M-x package-install RET org-journal RET
;;   M-x package-install RET neotree RET

;; https://emacs.stackexchange.com/questions/233/how-to-proceed-on-package-el-signature-check-failure
(setq package-check-signature nil)

;; This is a fix for "???" displayed in agenda view columns
;; https://www.reddit.com/r/emacs/comments/s9hl74/mystery_question_marks_on_my_agenda/
;; https://emacs.stackexchange.com/questions/42006/trouble-with-org-mode-cache-find-error
(setq org-element-use-cache nil)

;; Disable the splash screen
(setq inhibit-splash-screen t)

;; Disable tooltips
(tooltip-mode -1)

;; Disable the toolbar
(tool-bar-mode -1)

;; Show column number in mode line
(column-number-mode)

;; Display line numbers
(global-display-line-numbers-mode t)

;; Wrap text (but only in text mode, not in agenda)
;; https://www.inmotionhosting.com/support/edu/software/how-to-wrap-text-in-emacs/
(add-hook 'text-mode-hook 'visual-line-mode)

;; Disable line numbers for some modes
(dolist (mode '(term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(if (eq system-type 'darwin)
  (set-face-attribute 'default nil :font "Monaco-14"))
(if (eq system-type 'windows-nt)
  (set-face-attribute 'default nil :font "Consolas-11"))

(require 'org-superstar)
(add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))
;; https://www.reddit.com/r/orgmode/comments/pfgcql/comment/hb4g8j0/
(setq org-superstar-leading-bullet ?\s)

;; https://stackoverflow.com/questions/77332358/how-can-i-adjust-the-fonts-and-sizes-of-bullets-in-org-superstar
;; Increase font slightly for headers
(custom-set-faces
  '(org-level-1 ((t (:inherit outline-1 :height 1.04))))
  '(org-level-2 ((t (:inherit outline-2 :height 1.03))))
  '(org-level-3 ((t (:inherit outline-3 :height 1.02))))
  '(org-level-4 ((t (:inherit outline-4 :height 1.01))))
  '(org-level-5 ((t (:inherit outline-5 :height 1.0))))
  )

;; https://emacs.stackexchange.com/questions/639/how-can-i-restart-emacs-and-preserve-my-open-buffers-and-interactive-history
(desktop-save-mode 1)
(savehist-mode 1)
(add-to-list 'savehist-additional-variables 'kill-ring)
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Saving-Emacs-Sessions.html#:~:text=You%20can%20avoid%20the%20question,load%20the%20desktop%20without%20asking.
;; Reload desktop without asking
(setq desktop-load-locked-desktop t)

(if (eq system-type 'darwin)
  (setq default-directory "/Users/anton/mywork/Dropbox/notes/org/Plans/"))
(if (eq system-type 'windows-nt)
  (setq default-directory "C:/MyWork/Dropbox/notes/org/Plans/"))
(if (eq system-type 'gnu/linux)
  (setq default-directory "/home/anton/Dropbox/notes/org/Plans/"))

;; https://stackoverflow.com/questions/11384516/how-to-make-all-org-files-under-a-folder-added-in-agenda-list-automatically
(if (eq system-type 'darwin)
  (setq org-agenda-files (directory-files-recursively "/Users/anton/Dropbox/notes/org/" "\\.org$")))
(if (eq system-type 'windows-nt)
  (setq org-agenda-files (directory-files-recursively "C:/MyWork/Dropbox/notes/org/" "\\.org$")))
(if (eq system-type 'gnu/linux)
  (setq org-agenda-files (directory-files-recursively "/home/anton/Dropbox/notes/org/" "\\.org$")))

(define-key global-map "\C-ca" 'org-agenda)

(setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "IN-PROGRESS(i)" "WAITING(w)" "SOMEDAY(s)" "|" "DONE(d)")))

;; https://christiantietze.de/posts/2019/03/sync-emacs-org-files/
(add-hook 'auto-save-hook 'org-save-all-org-buffers)
(global-auto-revert-mode t)

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t))
(package-initialize)

;; Initialize package sources
;;(require 'package)

;;(add-to-list 'package-archives
;;             '("melpa" . "http://melpa.org/packages/") t)

;;(setq package-archives '(("melpa" . "https://melpa.org/packages/")))
;;                         ; ("org" . "https://orgmode.org/elpa/")))
;;                         ; ("elpa" . "https://elpa.gnu.org/packages/")))

;; (package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; make the fringe stand out from the background
(setq solarized-distinct-fringe-background nil)

;; Don't change the font for some headings and titles
(setq solarized-use-variable-pitch nil)

;; make the modeline high contrast
(setq solarized-high-contrast-mode-line nil)

;; Use less bolding
(setq solarized-use-less-bold t)

;; Use more italics
(setq solarized-use-more-italic t)

;; Use less colors for indicators such as git:gutter, flycheck and similar
(setq solarized-emphasize-indicators nil)

;; Don't change size of org-mode headlines (but keep other size-changes)
(setq solarized-scale-org-headlines nil)

;; Avoid all font-size changes
(setq solarized-height-minus-1 1.0)
(setq solarized-height-plus-1 1.0)
(setq solarized-height-plus-2 1.0)
(setq solarized-height-plus-3 1.0)
(setq solarized-height-plus-4 1.0)

;; https://emacs.stackexchange.com/questions/15120/how-do-i-install-solarized-theme
(load-theme 'solarized-light t)

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(if (eq system-type 'darwin)
  (setq org-journal-dir "/Users/anton/Dropbox/org/Journal/"))
(if (eq system-type 'windows-nt)
  (setq org-journal-dir "C:/MyWork/Dropbox/org/Journal/"))
(if (eq system-type 'gnu/linux)
  (setq org-journal-dir "/home/anton/Dropbox/org/Journal/"))
(setq org-journal-file-format "%Y-%m-%d.org")
(setq org-extend-today-until 4)
(setq org-journal-file-type 'weekly)
(setq org-journal-date-format "%A, %d %B %Y")
(require 'org-journal)

;; Keep folders clean
;; https://github.com/daviwil/emacs-from-scratch/blob/master/show-notes/Emacs-Tips-Cleaning.org
;; or use no-littering package:
;; https://github.com/emacscollective/no-littering

;; Store backup files (init.el~) in ~/.emacs/tmp/backups
(setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))

;; Store auto save files (#init.el#) in ~/.emacs/tmp/auto-saves
;; auto-save-mode doesn't create the path automatically!
(make-directory (expand-file-name "tmp/auto-saves/" user-emacs-directory) t)
(setq auto-save-list-file-prefix (expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory)
  auto-save-file-name-transforms `((".*" ,(expand-file-name "tmp/auto-saves/" user-emacs-directory) t)))

;; Disable lock files (.#init.el)
(setq create-lockfiles nil)

;; https://stackoverflow.com/questions/2010539/how-can-i-show-the-org-mode-agenda-on-emacs-start-up
(message "Loading agenda view...")
(org-todo-list "NEXT")
(delete-other-windows)
;; (org-journal-new-entry nil)

(defun my-show-agenda ()
  (interactive)
  (org-todo-list "NEXT")
  (delete-other-windows))
(global-set-key [f9] 'my-show-agenda)

;; (calendar) 
;; (other-window 1) 
;; (split-window-horizontally) 
;; (other-window 1) 

(require 'neotree)
;; https://github.com/jaypei/emacs-neotree/issues/164
(global-set-key [f8] 'neotree-toggle)
(if (eq system-type 'darwin)
  (neotree-dir "/Users/anton/Dropbox/notes/org/"))
(if (eq system-type 'windows-nt)
  (neotree-dir "C:/MyWork/Dropbox/org/"))
(if (eq system-type 'gnu/linux)
  (neotree-dir "/home/anton/Dropbox/notes/org/"))
(call-interactively 'other-window)

;; https://emacs.stackexchange.com/questions/15093/how-to-add-an-item-to-the-menu-bar
;; Add GTD menu to the end of the menu bar
(defvar my-menu-bar-menu (make-sparse-keymap "GTD"))
(define-key global-map [menu-bar my-menu] (cons "GTD" my-menu-bar-menu))

;; Insert GTD before the Help menu
(setq menu-bar-final-items (append menu-bar-final-items '(my-menu)))

;; Define the menu items
(define-key my-menu-bar-menu [my-cmd1]
  '(menu-item "Toggle explorer" neotree-toggle :help "Toggle explorer"))
(define-key my-menu-bar-menu [my-cmd2]
  '(menu-item "Show agenda" my-show-agenda :help "Show agenda"))

