;; Ensure package management is initialized
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")))

(package-initialize)

;; Install `use-package` if not already installed
(unless (package-installed-p 'use-package)
  (unless (assoc 'use-package package-archive-contents)
    (package-refresh-contents))
  (package-install 'use-package))

(require 'use-package)

;; Keep Customize's auto-generated settings out of init.el / config.org.
;; Without this, `custom-file' is nil and Emacs appends `custom-set-variables'
;; and `custom-set-faces' to the init file whenever you change a setting.
;; Point it at a separate, gitignored file instead.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Disable the org-element cache BEFORE tangling config.org. The startup
;; tangle parses config.org before any of our config has loaded, so the
;; cache is otherwise at its default (on) — the same path that throws
;; "Invalid search bound (wrong side of point)" and produces a broken,
;; partially-tangled config.el (load errors or silently-missing changes).
;; The matching setting inside config.org loads too late to protect this
;; step: it can only take effect after config.el has already been tangled
;; and loaded. This is the authoritative tangle-time guard.
(require 'org)
(setq org-element-use-cache nil)

;; Load the main configuration from `config.org`
(org-babel-load-file (expand-file-name "config.org" user-emacs-directory))
