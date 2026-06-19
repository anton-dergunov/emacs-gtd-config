# Minimalist Emacs configuration for Getting Things Done (GTD) using Org Mode

[![Tests](https://github.com/anton-dergunov/emacs-gtd-config/actions/workflows/tests.yml/badge.svg)](https://github.com/anton-dergunov/emacs-gtd-config/actions/workflows/tests.yml)

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
   ** INPR Task 2
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


## Directory Structure

`my-org-base-directory` (set in `local.el`) is the root for your org files.
The agenda scans `Areas/` recursively for `.org` files; `Vision/` and
`Current/` are for your own use and are not scanned automatically.

```
<my-org-base-directory>/
├── Areas/      org files scanned recursively for the agenda
│                 (e.g. Career.org, Health.org, Financial.org, ...)
├── Vision/     long-term vision docs
└── Current/    current focus / weekly review docs
```

See `samples/realistic/` for a working example of this layout.


## Icons (Material Symbols)

Category icons — in the agenda (next to each task) and the file tree — are drawn
directly from Google's **Material Symbols Outlined** font. There are no per-icon
SVG files to download or maintain; each icon is rendered from a font glyph, sized
automatically to your font so it stays aligned across fonts and platforms.

### Install the font

1. Download **Material Symbols Outlined** from
   [Google Fonts](https://fonts.google.com/icons) ("Download family") or the
   [google/material-design-icons](https://github.com/google/material-design-icons/tree/master/variablefont)
   repo.
2. From the archive, install only
   `Material_Symbols_Outlined/static/MaterialSymbolsOutlined-Regular.ttf`
   (the plain "Regular" static file — not a "Filled" or `*pt-*` variant).
3. Install it:
   - **macOS:** double-click the `.ttf` and click **Install Font** (or drop it in `~/Library/Fonts/`).
   - **Linux:** copy it into `~/.local/share/fonts/`, then run `fc-cache -f`.
   - **Windows:** right-click the `.ttf` and choose **Install**.

If the font is not installed, the file tree still works: project folders and files
fall back to the `folder`/`folder_open`/`draft` SVGs in `icons/` (each named after
the Material Symbols glyph it stands in for).

### Assign icons to your files

Icons are assigned declaratively, mapping a `<Category>.org` file's basename to a
[Material Symbols name](https://fonts.google.com/icons), e.g. `("Blog" . "edit_square")`.
`config.org` ships **no** mappings — every file shows the generic `File` icon until
you provide a map. You normally do that per Org folder in `workspace.org` (below);
see `samples/realistic/workspace.org` for a complete example. Whole folders (e.g.
`Current/`, `Vision/`) can be icon-mapped too via `ps/material-icons-folder-map`.

To fine-tune size/alignment, adjust `ps/material-icons-height-scale` and
`ps/file-tree-icon-ascent` in the **Settings** section of `config.org`.

Icon names are resolved to font codepoints via `icons/material-symbols.codepoints`,
the official list Google ships with the font (Apache-2.0). Codepoints for existing
icons are stable, so you never need to touch it; only if you want an icon Google has
*added* since, refresh it with `scripts/update_material_symbols_codepoints.sh`.


## Workspace config (per-Org-folder settings)

`config.org` is the shared, public configuration (like VS Code's *User* settings).
Settings tied to a specific Org **data** folder — your personal category→icon map
and your file-tree file sets — live instead in a small **`workspace.org`** in that
folder, beside `init.org` (like *Workspace* settings). It is loaded after
`config.org` and overrides it, so:

- your personal categories never enter the public repo, and
- pulling upstream changes to `config.org` never conflicts with your customizations.

Open or reload it from **Productivity → Config → Workspace** (`C-c p W` / `C-c p w`).
If the file doesn't exist yet, it is simply ignored; "Open" starts a fresh one.
Copy `samples/realistic/workspace.org` into your Org base directory as a starting
point.


## Typo / Spell Checking

Org buffers get a quiet, multilingual typo checker. A word is underlined with a
subtle wavy line **only when none of your configured languages recognise it**,
so terms that are valid in any language you write in are never flagged — for
example Spanish `calor` or Cyrillic words inside English text stay clean. Code,
links, file paths and `=verbatim=`/`~code~` markup are skipped automatically.
Press `M-$` on an underlined word to pick a correction or add the word to your
personal dictionary. Only individual words are checked — there is no grammar
checking and no network/LLM use.

The accepted languages default to English, Russian and Spanish; edit
`ps/typo-languages` in the **Settings → Typo / spell checking** block of
`config.org` to match the languages you write in. See
[docs/typo-checker.md](docs/typo-checker.md) for the design rationale.

This feature uses [Jinx](https://github.com/minad/jinx), which needs the
`enchant` library plus a dictionary for each language.

**macOS:**

```bash
brew install enchant          # also pulls in hunspell
xcode-select --install        # C compiler for Jinx's module (skip if installed)
```

English and several other languages work out of the box via the macOS system
speller (AppleSpell). Note that AppleSpell is fairly permissive — it accepts
some misspellings (e.g. `teh`) — which keeps false positives low but also lets a
few typos through. For stricter checking, install Hunspell dictionary files
(`<locale>.aff` and `<locale>.dic`, e.g. `en_US.*`, `es_ES.*`, `ru_RU.*`) into
`~/.config/enchant/hunspell/`; `enchant` prefers them over AppleSpell. Russian
is not provided by AppleSpell, so a `ru_RU` Hunspell dictionary is required for
it. List what `enchant` can see with:

```bash
enchant-2 -list-dicts
```

Dictionary files are available from the
[LibreOffice dictionaries](https://github.com/LibreOffice/dictionaries) repo.

**Other systems:** `enchant` is cross-platform — on Linux install it with your
package manager (e.g. `sudo apt install enchant-2`, plus `hunspell-es`,
`hunspell-ru`, …); on Windows it is available via MSYS2 and some Emacs builds
bundle it. Only the dictionary-install step differs per OS.

If `enchant` is not installed, the rest of the config still loads normally —
typo checking simply stays off until the library and dictionaries are present.


## Changing the Color Theme

Everybody has strong opinions about colors, so switching the whole look is a
one-line change. Open `config.org`, find the **Settings → Appearance** section,
and set `ps/color-theme`:

```elisp
(defvar ps/color-theme 'solarized-light  ; <- change this
  "Color theme loaded by `Editor & UI / Color theme'.")
```

Recommended values (external-package themes are **installed automatically** on
first load):

| Kind        | Source         | Examples                                                                         |
| ----------- | -------------- | -------------------------------------------------------------------------------- |
| Solarized   | package        | `solarized-light` (default) `solarized-dark`                                     |
| modus       | built-in (28+) | `modus-operandi` `modus-vivendi`                                                 |
| ef-themes   | package        | `ef-day` `ef-elea-dark` `ef-winter` `ef-autumn`                                  |
| standard    | package        | `standard-light` `standard-dark`                                                 |
| doric       | package        | `doric-light` `doric-dark`                                                       |
| doom        | package        | `doom-one` `doom-one-light` `doom-nord` `doom-dracula` `doom-gruvbox`            |
| Catppuccin  | package        | `batppuccin-latte` `batppuccin-mocha` `batppuccin-macchiato` `batppuccin-frappe` |
| Tokyo Night | package        | `tokyo-night` `tokyo-night-storm` `tokyo-night-moon` `tokyo-night-day`           |
| Gruvbox     | package        | `gruvbox-dark-medium` `gruvbox-light-medium`                                     |
| Nord        | package        | `nord`                                                                           |

The config's own color tweaks (faded DONE tasks, timestamp pills, metadata, the
SCHEDULED/DEADLINE icons) adapt to the chosen theme automatically — the original
hand-tuned grays are kept for Solarized, and every other theme gets
theme-relative equivalents so it looks right on light *and* dark backgrounds.

**Audition a theme without editing the file:** `M-x ps/preview-theme` (or
`C-c p T`) loads any theme instantly. Set `ps/color-theme` to keep it.

### Gallery

> Generate these locally with `scripts/screenshot_all_themes.sh` (or
> `scripts/screenshot_theme.sh <theme>`), which captures the agenda under each
> theme into `screenshots/`. macOS-only: it uses the `screencapture` tool, so
> the terminal running it must have Screen Recording permission.

The agenda under the default theme, **Solarized Light**:

<p align="center">
  <img src="screenshots/solarized-light.png" alt="Solarized Light (default)" width="820">
</p>

<details>
  <summary><b>Solarized Dark</b></summary>
  <p align="center"><img src="screenshots/solarized-dark.png" alt="Solarized Dark" width="820"></p>
</details>

<details>
  <summary><b>Modus Operandi</b> — light, accessible</summary>
  <p align="center"><img src="screenshots/modus-operandi.png" alt="Modus Operandi" width="820"></p>
</details>

<details>
  <summary><b>ef-day</b> — light, warm</summary>
  <p align="center"><img src="screenshots/ef-day.png" alt="ef-day" width="820"></p>
</details>

<details>
  <summary><b>Doom One Light</b></summary>
  <p align="center"><img src="screenshots/doom-one-light.png" alt="Doom One Light" width="820"></p>
</details>

<details>
  <summary><b>Catppuccin Latte</b> — light, pastel</summary>
  <p align="center"><img src="screenshots/batppuccin-latte.png" alt="Catppuccin Latte" width="820"></p>
</details>

<details>
  <summary><b>Modus Vivendi</b> — dark, accessible</summary>
  <p align="center"><img src="screenshots/modus-vivendi.png" alt="Modus Vivendi" width="820"></p>
</details>

<details>
  <summary><b>Doom One</b> — dark, the modern classic</summary>
  <p align="center"><img src="screenshots/doom-one.png" alt="Doom One" width="820"></p>
</details>

<details>
  <summary><b>Tokyo Night</b> — dark</summary>
  <p align="center"><img src="screenshots/tokyo-night.png" alt="Tokyo Night" width="820"></p>
</details>

<details>
  <summary><b>Catppuccin Mocha</b> — dark, pastel</summary>
  <p align="center"><img src="screenshots/batppuccin-mocha.png" alt="Catppuccin Mocha" width="820"></p>
</details>

<details>
  <summary><b>ef-elea-dark</b> — dark</summary>
  <p align="center"><img src="screenshots/ef-elea-dark.png" alt="ef-elea-dark" width="820"></p>
</details>

<details>
  <summary><b>Gruvbox Dark</b></summary>
  <p align="center"><img src="screenshots/gruvbox-dark-medium.png" alt="Gruvbox Dark" width="820"></p>
</details>

<details>
  <summary><b>Doom Nord</b></summary>
  <p align="center"><img src="screenshots/doom-nord.png" alt="Doom Nord" width="820"></p>
</details>

<details>
  <summary><b>Doom Dracula</b></summary>
  <p align="center"><img src="screenshots/doom-dracula.png" alt="Doom Dracula" width="820"></p>
</details>


## Running Unit Tests

To run python tests locally:

```bash
cd ~/.emacs.d
PYTHONPATH=. pytest
```

To run elisp tests locally:

```bash
EMACS_BIN="/Applications/Emacs.app/Contents/MacOS/Emacs" ./scripts/org_test.sh
```


## Running Emacs for Local Testing

To run Emacs directly from this repository (without installing it to `~/.emacs.d`), use:

```bash
./scripts/run_emacs_dev.sh
```

This uses `--init-directory` to point Emacs at this repo, and automatically creates `local.el` on first run with org files pointing at `samples/realistic/`. Packages are downloaded into `elpa/` inside the repo on first launch.

> **Note:** The script defaults to `/Applications/Emacs.app/Contents/MacOS/Emacs`. If your Emacs is installed elsewhere, override the path:
> ```bash
> EMACS_BIN=/path/to/emacs ./scripts/run_emacs_dev.sh
> ```

### Trying other Emacs builds

Pass `--emacs <variant>` to run against a different Emacs build installed side-by-side:

```bash
./scripts/run_emacs_dev.sh --emacs default  # /Applications/Emacs.app (default)
./scripts/run_emacs_dev.sh --emacs plus     # emacs-plus@30 (Homebrew formula)
./scripts/run_emacs_dev.sh --emacs latest   # latest emacsformacosx.com build (~/Applications/Emacs-latest)
```

### Sandboxed runs

Pass `--sandbox` to copy the repo (excluding `.git`) to a temp directory under `/tmp` and run from there. This avoids any risk of `ps-git-sync` committing to this repo's git history during testing. The temp directory is removed when Emacs exits. Combine with `--emacs`, e.g. `./scripts/run_emacs_dev.sh --emacs plus --sandbox`.
