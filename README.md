This repository contains a minimalistic Emacs configuration to use [Org Mode](https://orgmode.org/) for implementing [Getting Things Done](https://en.wikipedia.org/wiki/Getting_Things_Done).

The main configuration file is [config.org](config.org).

## Installation Instructions

1. **Install [Emacs](https://www.gnu.org/software/emacs/download.html).**

2. **Install `ripgrep`** (used by `counsel-projectile` for searching through files).

   ```bash
   # macOS
   brew install ripgrep

   # Ubuntu
   sudo apt install ripgrep
   ```

   [Instructions for Windows](https://stackoverflow.com/questions/76666894/how-to-install-ripgrep-on-windows).

3. **Clone this repository** to `~/.emacs.d`.

   ```bash
   git clone https://github.com/anton-dergunov/emacs-gtd-config.git ~/.emacs.d
   ```

   Note: on Windows the location is usually the following `C:\Users\<CURRENT_USER_NAME>\AppData\Roaming\.emacs.d`

4. **Configure `local.el`**:

   - Copy `local.el.template` to `local.el`.
   - Adjust the `my-org-base-directory` variable in `local.el` to point to the base directory for your Org mode files. I recommend using Dropbox or another sync service to keep your files synchronized across devices.

   To get started, you can create a directory with a file named `Example.org`. See "[Org for GTD and other Task Management Systems](https://orgmode.org/worg/org-gtd-etc.html)" for more details.

   **Example `Example.org` file:**

   ```org
   * My Project
   ** TODO [#A] Task 1
   ** IN-PROGRESS Task 2
   ** TODO Overdue Task 3
      DEADLINE: <2024-01-01 Mon>
   ** TODO Overdue Task 4
      SCHEDULED: <2024-01-01 Mon 13:30-15:00>
   ```

5. **(Optional) Initialize Emacs packages**:

   You can skip this step, since Emacs will initialize them during first start instead.

   ```bash
   emacs --batch -l ~/.emacs.d/init.el
   ```

6. **Run Emacs and enjoy** your setup.
