#!/usr/bin/env bash
#
# update_material_symbols_codepoints.sh
#
# Refreshes icons/material-symbols.codepoints — the NAME -> hex-codepoint lookup
# table that lets config.org/workspace.org name icons (e.g. "edit_square") instead
# of raw codepoints. It is the official list published by Google alongside the
# Material Symbols variable font (Apache-2.0), and is identical for the Outlined,
# Rounded, and Sharp styles.
#
# You normally never need to run this: codepoints for existing icons are stable.
# Run it only to pick up icons Google has ADDED since this file was last fetched.

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
DEST="$REPO_ROOT/icons/material-symbols.codepoints"
URL="https://raw.githubusercontent.com/google/material-design-icons/master/variablefont/MaterialSymbolsOutlined%5BFILL%2CGRAD%2Copsz%2Cwght%5D.codepoints"

echo "Downloading Material Symbols codepoints…"
curl -fsSL "$URL" -o "$DEST"
echo "Wrote $DEST ($(wc -l < "$DEST" | tr -d ' ') icons)."
