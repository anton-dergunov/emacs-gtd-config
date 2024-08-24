This repository contains Emacs config to use [Org Mode](https://orgmode.org/) for "[Getting Things Done](https://en.wikipedia.org/wiki/Getting_Things_Done)"

## Installation Instructions

1. Create file ~/.emacs and load this config:

```elisp
(package-initialize)
(defvar my-base-directory "~/<org-path>")
(org-babel-load-file "~/<config-path>/config.org")
```

2. Refresh packages:
   `M-x package-refresh-contents RET`

3. Install the required packages:
  - `M-x package-install RET magit RET`
  - `M-x package-install RET org-superstar RET`
  - `M-x package-install RET solarized-theme RET`
  - `M-x package-install RET org-journal RET`
  - `M-x package-install RET neotree RET`

