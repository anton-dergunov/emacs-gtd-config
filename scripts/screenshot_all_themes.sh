#!/usr/bin/env bash
# Capture screenshots for a curated set of themes into screenshots/, for the
# README gallery. Wraps scripts/screenshot_theme.sh. Override the list with
# positional args: scripts/screenshot_all_themes.sh wombat leuven ef-day
#
# Env: EMACS_BIN, PS_SAMPLE_DIR (passed through to screenshot_theme.sh)

set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

if [ "$#" -gt 0 ]; then
  THEMES=("$@")
else
  THEMES=(
    solarized-light solarized-dark
    leuven wombat tango-dark
    modus-operandi modus-vivendi
    ef-day ef-winter
    doom-one
  )
fi

for t in "${THEMES[@]}"; do
  echo "== $t =="
  "$SCRIPT_DIR/screenshot_theme.sh" "$t" || echo "  (failed: $t)"
done
