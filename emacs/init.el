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

;; Disable line numbers for some modes
(dolist (mode '(term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(if (eq system-type 'darwin)
  (set-face-attribute 'default nil :font "Monaco-14"))
(if (eq system-type 'windows-nt)
  (set-face-attribute 'default nil :font "Consolas-11"))

(if (eq system-type 'darwin)
  (setq default-directory "/Users/anton/mywork/Dropbox/notes/Plans/Org/"))
(if (eq system-type 'windows-nt)
  (setq default-directory "C:/MyWork/Dropbox/notes/Plans/Org/"))

(define-key global-map "\C-ca" 'org-agenda)

(setq org-todo-keywords '((sequence "TODO(t)" "INPR(i)" "WAIT(w)" "SMDY(s)" "|" "DONE(d)")))

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

;; Install it first using M-x package-install solarized-theme
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


