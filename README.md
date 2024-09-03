This repository contains the minimalistic Emacs config to use [Org Mode](https://orgmode.org/) for "[Getting Things Done](https://en.wikipedia.org/wiki/Getting_Things_Done)"

The main configuration file is [config.org](config.org).

## Installation Instructions

1. Install [Emacs](https://www.gnu.org/software/emacs/download.html).

2. Install `ripgrep` (used by `counsel-projectile` for searching all files).

```bash
brew install ripgrep       # MacOs
sudo apt install ripgrep   # Ubuntu
```

3. Clone this repository to `~/.emacs.d`.

```bash
git clone https://github.com/anton-dergunov/emacs-gtd-config.git ~/.emacs.d
```

4. Copy `local.el.template` to `local.el` and adjust `my-org-base-directory` to point to the base directory for org mode files. I like to keep them in Dropbox so that they are synchronized across different devices. To get you started, you can create a directory with a file `Example.org`. See "[Org for GTD and other Task managment systems](https://orgmode.org/worg/org-gtd-etc.html)" for more details.

```elisp
* My project
** TODO [#A] Task 1
** IN-PROGRESS Task 2
** TODO Overdue task 3
DEADLINE: <2024-01-01 Mon>
** TODO Overdue task 4
SCHEDULED: <2024-01-01 Mon 13:30-15:00>
```

5. (Optional) Ensure that all Emacs packages are initialized. This step can be skipped.

```bash
emacs --batch -l ~/.emacs.d/init.el
```

6. Run Emacs and enjoy.

