#!/usr/bin/env bash

set -euo pipefail

EMACS_BIN="${EMACS_BIN:-emacs}"

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
