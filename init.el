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

;; Load the main configuration from `config.org`
(org-babel-load-file (expand-file-name "config.org" user-emacs-directory))
