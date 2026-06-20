# Org-mode planning system for Emacs

[![Tests](https://github.com/anton-dergunov/emacs-gtd-config/actions/workflows/tests.yml/badge.svg)](https://github.com/anton-dergunov/emacs-gtd-config/actions/workflows/tests.yml)

A minimalist Emacs configuration for planning your work and life in plain-text
[Org Mode](https://orgmode.org/) files. The workflow is inspired by
[Getting Things Done](https://en.wikipedia.org/wiki/Getting_Things_Done): you
keep tasks and notes in a handful of Org files, and a clean, visual **agenda**
shows what matters today — schedule, deadlines, high-priority and in-progress
work.

It doubles as a complete `~/.emacs.d`, so you can use it as your whole Emacs
setup or borrow pieces for your own.

<p align="center">
  <img src="screenshots/solarized-light.png" alt="The agenda under the Solarized Light theme" width="820">
</p>

## Highlights

- **A visual agenda** — your day at a glance: a real timeline, deadlines,
  overdue items, high-priority and in-progress tasks, with category icons and
  compact status/priority/date pills.
- **A schedule view** — see the day as a timeline or a compact event list, with
  a live now-indicator that refreshes every minute.
- **Planning tools** — find free slots (availability), detect scheduling
  conflicts, and shift timestamps between timezones.
- **A file tree with icons** — browse your Org areas, switch between named file
  sets, and optionally scope the agenda to the set you're viewing.
- **Capture and link** — quick capture, journaling, Obsidian-style links, and
  one-key insertion of a web link with its page title fetched automatically.
- **Many themes** — switch the entire look with one setting, or audition themes
  live.
- **Quiet quality-of-life touches** — multilingual typo checking, faded/folded
  DONE tasks, live-preview markup that hides `*`/`/`/`[[]]` until you edit it,
  and automatic background Git sync of your Org files.

## Quick start

> Full, per-OS instructions are in [docs/Installation.org](docs/Installation.org).

1. Install [Emacs](https://www.gnu.org/software/emacs/download.html) (29+) and
   [ripgrep](https://github.com/BurntSushi/ripgrep) (used for searching).
2. Clone this repo as your Emacs config directory:
   ```bash
   git clone https://github.com/anton-dergunov/emacs-gtd-config.git ~/.emacs.d
   ```
   (On Windows this is usually `C:\Users\<USER>\AppData\Roaming\.emacs.d`.)
3. Tell it where your Org files live: copy `local.el.template` to `local.el` and
   set `my-org-base-directory`. To explore first, point it at the bundled
   `samples/realistic/` example.
4. Start Emacs. Packages download on first launch; then press **`C-c p a`** to
   open the agenda.

For the prettiest result, also install the **Material Symbols** icon font — see
[docs/Installation.org](docs/Installation.org).

## Documentation

Start here: **[docs/Index.org](docs/Index.org)** — a guided table of contents.

Jump straight to:

- [Installation](docs/Installation.org) — set it up on macOS, Linux, or Windows
- [Emacs basics](docs/Emacs-basics.org) — new to Emacs? essential editing and
  navigation keys
- [Planning setup](docs/Planning-setup.org) — your Org files, task states, and
  the keys for editing tasks
- [The Agenda](docs/Agenda.org) — the heart of the system
- [Customization & appearance](docs/Customization.org) — themes, fonts, icons,
  and settings

## Developing

Run Emacs straight from this repo (no install), run the tests, and extend it
with new modules — see [docs/Developing.org](docs/Developing.org).
