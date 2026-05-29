#!/usr/bin/env bash
# Run Emacs with this repo as the config directory (no install needed).
# Uses --init-directory (Emacs 29+) so user-emacs-directory points here.
# On first run, packages are downloaded into elpa/ inside this repo.
# local.el in the repo root points org files at samples/realistic/.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_DIR="$(dirname "$SCRIPT_DIR")"
EMACS_BIN="${EMACS_BIN:-/Applications/Emacs.app/Contents/MacOS/Emacs}"
LOCAL_EL="$REPO_DIR/local.el"

if [ ! -f "$LOCAL_EL" ]; then
  echo "Creating $LOCAL_EL pointing to samples/realistic/"
  cat > "$LOCAL_EL" <<'EOF'
(setq my-org-base-directory
      (expand-file-name "samples/realistic/" user-emacs-directory))
EOF
fi

exec "$EMACS_BIN" --init-directory "$REPO_DIR" \
  --eval "(setq ps/git-sync-paused t)" \
  "$@"
