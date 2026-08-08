#!/usr/bin/env bash
# Run Emacs with this repo as the config directory (no install needed).
# Uses --init-directory (Emacs 29+) so user-emacs-directory points here.
# On first run, packages are downloaded into elpa/ inside this repo.
# local.el in the repo root points org files at samples/realistic/.
#
# Pass --emacs <variant> to pick which Emacs build to run (default: plus):
#   plus    - emacs-plus@30 (Homebrew formula) -- the build this config is
#             used with day to day, so it is what development should test
#   default - /Applications/Emacs.app (override with EMACS_BIN). Note this
#             build ships its own copy of glib inside the bundle, which clashes
#             with Homebrew's when both end up loaded ("Class
#             GNotificationCenterDelegate is implemented in both ...").
#   latest  - official latest build from emacsformacosx.com,
#             installed to ~/Applications/Emacs-latest
#
# Pass --sandbox to copy the repo (without .git) to a temp dir under /tmp and
# run from there instead. Useful for testing without risking commits to this
# repo's git history. The temp dir is removed when Emacs exits.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_DIR="$(dirname "$SCRIPT_DIR")"

EMACS_VARIANT="plus"
SANDBOX=false

while [[ $# -gt 0 ]]; do
  case "$1" in
    --emacs)
      EMACS_VARIANT="$2"
      shift 2
      ;;
    --sandbox)
      SANDBOX=true
      shift
      ;;
    *)
      break
      ;;
  esac
done

case "$EMACS_VARIANT" in
  default)
    EMACS_BIN="${EMACS_BIN:-/Applications/Emacs.app/Contents/MacOS/Emacs}"
    ;;
  plus)
    EMACS_BIN="${EMACS_BIN:-$(brew --prefix)/opt/emacs-plus@30/Emacs.app/Contents/MacOS/Emacs}"
    if [ ! -x "$EMACS_BIN" ]; then
      echo "emacs-plus@30 not found at $EMACS_BIN" >&2
      echo "Install it with:  brew install emacs-plus@30" >&2
      echo "Or run another build:  $0 --emacs default" >&2
      exit 1
    fi
    ;;
  latest)
    EMACS_BIN="$HOME/Applications/Emacs-latest/Emacs.app/Contents/MacOS/Emacs"
    ;;
  *)
    echo "Unknown --emacs variant: $EMACS_VARIANT (expected default|plus|latest)" >&2
    exit 1
    ;;
esac

LOCAL_EL="$REPO_DIR/local.el"
if [ ! -f "$LOCAL_EL" ]; then
  echo "Creating $LOCAL_EL pointing to samples/realistic/"
  cat > "$LOCAL_EL" <<'EOF'
(setq my-org-base-directory
      (expand-file-name "samples/realistic/" user-emacs-directory))
EOF
fi

# If --sandbox, copy the repo (without .git) to a temp dir under /tmp and run
# from there instead. elpa/ is symlinked rather than copied so packages don't
# need re-downloading.
if $SANDBOX; then
  SANDBOX_DIR="$(mktemp -d /tmp/emacs-sandbox-XXXXXX)"
  echo "Sandbox: $SANDBOX_DIR (removed on exit; no .git, so auto-sync can't commit here)"
  rsync -a --exclude='.git' --exclude='elpa' "$REPO_DIR/" "$SANDBOX_DIR/"
  [ -d "$REPO_DIR/elpa" ] && ln -s "$REPO_DIR/elpa" "$SANDBOX_DIR/elpa"
  INIT_DIR="$SANDBOX_DIR"
else
  INIT_DIR="$REPO_DIR"
fi

# Belt-and-suspenders: PS_GIT_SYNC_DISABLE (checked in config.org) prevents
# the sync timer from ever starting; ps/git-sync-paused is a runtime fallback.
export PS_GIT_SYNC_DISABLE=1

# PS_ORG_BASE points the session at a different org directory than local.el
# names.  Needed to try anything that reads the org files' *git history*: by
# default my-org-base-directory is samples/realistic/, which lives inside this
# repo, so history there is this config repo's own.  See
# scripts/make_blank_line_playground.sh.
#
# Note: the always-present --eval seeds EMACS_ARGS, so the array is never
# empty -- macOS ships bash 3.2, where "${arr[@]}" on an empty array counts as
# an unset variable and trips `set -u'.
EMACS_ARGS=(--eval "(setq ps/git-sync-paused t)")
if [ -n "${PS_ORG_BASE:-}" ]; then
  if [ ! -d "$PS_ORG_BASE" ]; then
    echo "PS_ORG_BASE is not a directory: $PS_ORG_BASE" >&2
    exit 1
  fi
  EMACS_ARGS+=(--eval "(setq my-org-base-directory \"$PS_ORG_BASE/\")")
  echo "Org base: $PS_ORG_BASE"
fi

if $SANDBOX; then
  "$EMACS_BIN" --init-directory "$INIT_DIR" "${EMACS_ARGS[@]}" "$@"
  rm -rf "$SANDBOX_DIR"
else
  exec "$EMACS_BIN" --init-directory "$INIT_DIR" "${EMACS_ARGS[@]}" "$@"
fi
