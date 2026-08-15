# Org-mode planning system for Emacs

[![Tests](https://github.com/anton-dergunov/emacs-gtd-config/actions/workflows/tests.yml/badge.svg)](https://github.com/anton-dergunov/emacs-gtd-config/actions/workflows/tests.yml)

A minimalist Emacs configuration for planning your work and life in plain-text
[Org Mode](https://orgmode.org/) files. The workflow is inspired by
[Getting Things Done](https://en.wikipedia.org/wiki/Getting_Things_Done): you
keep tasks and notes in a handful of Org files, and a clean, visual **agenda**
shows what matters today — schedule, deadlines, high-priority work, what's in
progress, and what to pick up next.

It doubles as a complete `~/.emacs.d`, so you can use it as your whole Emacs
setup or borrow pieces for your own.

<p align="center">
  <img src="screenshots/solarized-light.png" alt="The agenda under the Solarized Light theme" width="820">
</p>

## Highlights

- **A visual agenda** — your day at a glance: a real timeline, deadlines,
  overdue items, high-priority, in-progress and next-up tasks, with category
  icons and compact status/priority/date pills.
- **A schedule view** — see the day as a timeline or a compact event list, with
  a live now-indicator that refreshes every minute.
- **Situations** — saved searches named by circumstance ("a spare minute", "on
  foot", "screen in hand, offline"), so an awkward gap in the day has an answer
  ready instead of turning into a scroll.
- **Planning tools** — find free slots (availability), detect scheduling
  conflicts, and shift timestamps between timezones.
- **A file tree with icons** — browse your Org areas, switch between named file
  sets, and optionally scope the agenda to the set you're viewing.
- **Vaults** — keep several Org folders (work, personal, a shared project) and
  switch between them from the file tree, Obsidian-style. Each vault carries its
  own icons, file sets and situations, and syncs to its own Git remote.
- **Capture and link** — quick capture, journaling, Obsidian-style links, and
  one-key insertion of a web link with its page title fetched automatically.
- **Many themes** — switch the entire look with one setting, or audition themes
  live.
- **Modern scrollbars** — an auto-hiding, theme-coloured scroll-position
  indicator, plus smooth pixel scrolling.
- **Blank-line recovery** — mobile Org apps throw away the blank lines you put
  in your files. One command finds the last version that still had them, works
  out where they belong in the file as it is now, and shows you what it would
  restore and why. Accept a file with one key, or open it side by side to take
  the changes one at a time — only what you accept is saved, and never a
  character of your text.
- **Quiet quality-of-life touches** — multilingual typo checking, faded/folded
  DONE tasks, live-preview markup that hides `*`/`/`/`[[]]` until you edit it,
  a centred reading-width column for your plan files on wide windows, and
  automatic background Git sync of your Org files.
- **An optional AI assistant** — a side-window helper (on your Claude
  subscription) that reads and edits your notes, aware of what you have selected,
  with its changes shown as diffs. Guide it with an `AGENTS.md` in your Org
  folder; your TODO keywords and what each plan file is for are passed to it
  automatically.

## Quick start

> Full, per-OS instructions are in [docs/Installation.org](docs/Installation.org).

1. Install [Emacs](https://www.gnu.org/software/emacs/download.html) (29+) and
   [ripgrep](https://github.com/BurntSushi/ripgrep) (used for searching). On
   macOS, read [the note below](#macos-build-emacs-with-the-freeze-fix) first.
2. Clone this repo as your Emacs config directory:
   ```bash
   git clone https://github.com/anton-dergunov/emacs-gtd-config.git ~/.emacs.d
   ```
   (On Windows this is usually `C:\Users\<USER>\AppData\Roaming\.emacs.d`.)
3. Start Emacs. Packages download on first launch, then it offers to create a
   **vault** — the folder your Org files live in. To explore first, open the
   bundled `samples/realistic/` example instead.
4. Press **`C-c p a`** to open the agenda.

For the prettiest result, also install the **Material Symbols** icon font — see
[docs/Installation.org](docs/Installation.org).

### macOS: build Emacs with the freeze fix

On macOS, Emacs 30 can freeze outright: the window stops accepting keyboard and
mouse input and never recovers, so the only way out is Force Quit. Minimising
the window while a background task is running is enough to trigger it — and this
config's automatic Git sync counts, so it can happen several times a day.

The bug is in Emacs itself, not in this configuration. Two patches fix it, and
[emacs-plus](https://github.com/d12frosted/homebrew-emacs-plus) can build Emacs
with both applied. You need **both** — the first one alone still leaves a
window in which Emacs can freeze:

- [`patches/emacs-30-ns-appdefined-windownumber.patch`](patches/emacs-30-ns-appdefined-windownumber.patch)
  stops Emacs from addressing its own wake-up event to a window that does not
  exist.
- [`patches/emacs-30-ns-appdefined-retry.patch`](patches/emacs-30-ns-appdefined-retry.patch)
  makes Emacs retry that wake-up, so a single lost one no longer hangs it
  permanently.

A third patch is optional and purely cosmetic:

- [`patches/emacs-30-ns-resize-title.patch`](patches/emacs-30-ns-resize-title.patch)
  keeps the window title alone while you drag a window edge, instead of
  replacing it with the frame's size for the duration of the drag.

```bash
brew tap d12frosted/emacs-plus

mkdir -p ~/.config/emacs-plus
cat > ~/.config/emacs-plus/build.yml <<'YAML'
patches:
  - ns-appdefined-windownumber:
      url: ~/.emacs.d/patches/emacs-30-ns-appdefined-windownumber.patch
      sha256: 319013a5587df554f81ef07ee25d678dcc4d169349d938b4164673b71d340d58
  - ns-appdefined-retry:
      url: ~/.emacs.d/patches/emacs-30-ns-appdefined-retry.patch
      sha256: 48c4577d5e49a74a40effe217cdd392bd30f6b5ca7139ef5e10cfa2c53a4c0fe
  - ns-resize-title:
      url: ~/.emacs.d/patches/emacs-30-ns-resize-title.patch
      sha256: c37fc4260551c3f380d0a81603cf5302fd6611d3822dc0f2893002197f48e873
YAML

brew install --build-from-source d12frosted/emacs-plus/emacs-plus@30
open -n /opt/homebrew/opt/emacs-plus@30/Emacs.app
```

`build.yml` is emacs-plus's own extension point, so the patches are re-applied
automatically every time you rebuild.

To confirm the Emacs you are running actually has both fixes:

```bash
EMACS=/opt/homebrew/opt/emacs-plus@30/Emacs.app/Contents/MacOS/Emacs
lldb --batch -o "disassemble -n ns_send_appdefined" -o quit "$EMACS" | grep -c keyWindow
lldb --batch -o "disassemble -n ns_read_socket_1"  -o quit "$EMACS" | grep -c scheduledTimer
```

Each command should print a non-zero count; `0` means that fix is not in.

Two things worth knowing when you rebuild later:

- **Use `brew uninstall` then `brew install`, not `brew reinstall`.** If the
  final linking step fails — which it does when symlinks from an older Emacs are
  still in `/opt/homebrew/bin` — `brew reinstall` quietly restores the previous
  build, so you keep running an unpatched Emacs that looks freshly installed.
- Homebrew builds with `-Os` (optimised for size). For `-O2` instead, add
  `cflags << "-O2"` near the top of the `cflags` list in
  `$(brew --repository)/Library/Taps/d12frosted/homebrew-emacs-plus/Formula/emacs-plus@30.rb`.
  Homebrew emits its own flags first, so this one wins — but `brew update`
  silently reverts the edit.

## Documentation

Start here: **[docs/Index.org](docs/Index.org)** — a guided table of contents.

Jump straight to:

- [Installation](docs/Installation.org) — set it up on macOS, Linux, or Windows
- [Emacs basics](docs/Emacs-basics.org) — new to Emacs? essential editing and
  navigation keys
- [Planning setup](docs/Planning-setup.org) — your Org files, task states, and
  the keys for editing tasks
- [The Agenda](docs/Agenda.org) — the heart of the system
- [Situations](docs/Situations.org) — context tags and the saved searches over
  them
- [Vaults](docs/Vaults.org) — several Org folders, switching between them, and
  which settings belong to which
- [Customization & appearance](docs/Customization.org) — themes, fonts, icons,
  and settings
- [AI integration (Claude Code)](docs/AI-integration.org) — the optional
  assistant and how to guide it with `AGENTS.md`

## Developing

Run Emacs straight from this repo (no install), run the tests, and extend it
with new modules — see [docs/Developing.org](docs/Developing.org).
