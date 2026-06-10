#!/usr/bin/env bash
# Capture a screenshot of the agenda under a given color theme, for the README
# gallery. Temporarily points local.el at the requested theme + sample org files
# (restored on exit), then runs scripts/run_emacs_dev.sh -- which disables git
# sync -- to launch a GUI Emacs and capture the frame to a PNG via the macOS
# `screencapture' tool (see scripts/screenshot.el). macOS-only; the terminal
# running this needs Screen Recording and Accessibility/Automation permission.
#
# Usage:   scripts/screenshot_theme.sh THEME [OUT.png]
# Example: scripts/screenshot_theme.sh wombat
#          scripts/screenshot_theme.sh solarized-light docs/img/solarized.png
#
# Env:
#   EMACS_BIN       Emacs binary (passed through to run_emacs_dev.sh)
#   PS_SAMPLE_DIR   sample org dir relative to repo (default: samples/realistic/)

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_DIR="$(dirname "$SCRIPT_DIR")"
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
PS_SCREENSHOT_OUT="$OUT" "$SCRIPT_DIR/run_emacs_dev.sh" -l "$SCRIPT_DIR/screenshot.el"
