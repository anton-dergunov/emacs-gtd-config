#!/usr/bin/env bash
# Capture a screenshot of the agenda under a given color theme, for the README
# gallery. Temporarily points local.el at the requested theme + sample org files
# (restored on exit), launches a GUI Emacs, and exports the frame to a PNG.
#
# Usage:   scripts/screenshot_theme.sh THEME [OUT.png]
# Example: scripts/screenshot_theme.sh wombat
#          scripts/screenshot_theme.sh solarized-light docs/img/solarized.png
#
# Env:
#   EMACS_BIN       Emacs binary (default: /Applications/Emacs.app/...)
#   PS_SAMPLE_DIR   sample org dir relative to repo (default: samples/realistic/)

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_DIR="$(dirname "$SCRIPT_DIR")"
EMACS_BIN="${EMACS_BIN:-/Applications/Emacs.app/Contents/MacOS/Emacs}"
SAMPLE_DIR="${PS_SAMPLE_DIR:-samples/realistic/}"

THEME="${1:?Usage: screenshot_theme.sh THEME [OUT.png]}"
OUT="${2:-$REPO_DIR/screenshots/$THEME.png}"
mkdir -p "$(dirname "$OUT")"

# Swap in a temporary local.el so the theme is chosen *before* config loads
# (the per-theme face tweaks are computed at load time). Restore on exit.
LOCAL_EL="$REPO_DIR/local.el"
BACKUP=""
if [ -f "$LOCAL_EL" ]; then
  BACKUP="$(mktemp)"
  cp "$LOCAL_EL" "$BACKUP"
fi
cleanup() {
  if [ -n "$BACKUP" ]; then mv "$BACKUP" "$LOCAL_EL"; else rm -f "$LOCAL_EL"; fi
}
trap cleanup EXIT

cat > "$LOCAL_EL" <<EOF
(setq my-org-base-directory
      (expand-file-name "$SAMPLE_DIR" user-emacs-directory))
(setq ps/color-theme '$THEME)
EOF

echo "Capturing $THEME -> $OUT"
PS_SCREENSHOT_OUT="$OUT" "$EMACS_BIN" --init-directory "$REPO_DIR" \
  -l "$SCRIPT_DIR/screenshot.el"
