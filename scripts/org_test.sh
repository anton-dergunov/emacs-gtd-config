#!/usr/bin/env bash

set -euo pipefail

if [ -z "${EMACS_BIN:-}" ]; then
  if command -v emacs >/dev/null 2>&1; then
    EMACS_BIN="emacs"
  elif [ -x "/Applications/Emacs.app/Contents/MacOS/Emacs" ]; then
    EMACS_BIN="/Applications/Emacs.app/Contents/MacOS/Emacs"
  else
    echo "Could not find an Emacs binary. Set EMACS_BIN explicitly." >&2
    exit 1
  fi
fi

echo "Using Emacs: $EMACS_BIN"

TEST_FILES=()

while IFS= read -r file; do
  TEST_FILES+=("-l" "$file")
done < <(find tests -name 'test-*.el' | sort)

"$EMACS_BIN" -Q --batch \
  -L lisp \
  -L tests \
  "${TEST_FILES[@]}" \
  -f ert-run-tests-batch-and-exit
