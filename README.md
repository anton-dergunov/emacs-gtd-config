This repository contains the minimalistic Emacs config to use [Org Mode](https://orgmode.org/) for "[Getting Things Done](https://en.wikipedia.org/wiki/Getting_Things_Done)"

## Installation Instructions

1. Install `ripgrep` (used by `counsel-projectile` for searching all files)
```bash
> brew install ripgrep       # MacOs
> sudo apt install ripgrep   # Ubuntu
```

2. Create file ~/.emacs and load this config:

```elisp
(package-initialize)
(defvar my-base-directory "~/<org-path>")
(org-babel-load-file "~/<config-path>/config.org")
```

3. Refresh packages:
   `M-x package-refresh-contents RET`

4. Install the required packages:
  - `M-x package-install RET magit RET`
  - `M-x package-install RET org-superstar RET`
  - `M-x package-install RET solarized-theme RET`
  - `M-x package-install RET org-journal RET`
  - `M-x package-install RET neotree RET`
  - `M-x package-install RET projectile RET`
  - `M-x package-install RET counsel-projectile RET`
