#!/usr/bin/env bash
# Run org availability computation via Emacs Lisp in batch mode.
# Accepts the same arguments as the former org_availability.py:
#
#   org_availability.sh DIRECTORY [--days N] [--start HH:MM] [--end HH:MM]
#     [--buffer N] [--min-duration N] [--include-weekends] [--raw]
#     [--time-format 12h|24h] [--layout bullets|inline]
#     [--start-date YYYY-MM-DD]

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_DIR="$(dirname "$SCRIPT_DIR")"
EMACS_BIN="${EMACS_BIN:-/Applications/Emacs.app/Contents/MacOS/Emacs}"

exec "$EMACS_BIN" -Q --batch \
  -L "$REPO_DIR/lisp" \
  -l ps-availability \
  --eval "(ps/org-avail-cli)" \
  -- "$@"
