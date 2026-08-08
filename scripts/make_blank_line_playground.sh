#!/usr/bin/env bash
#
# Build a throwaway org repo with a history that contains blank-line damage,
# for trying out `ps/blank-lines-recover' (F7) by hand.
#
# Why this exists: local.el points my-org-base-directory at samples/realistic/,
# which lives *inside this config repo*. Blank-line recovery reads the org
# files' git history, so pointed there it would read this repo's history and
# report nonsense. This copies the samples out, gives them their own repo, and
# synthesises the damage the mobile apps actually cause.
#
# No remote is ever configured, so nothing here can reach a real repository.
#
# Usage:
#   ./scripts/make_blank_line_playground.sh [DEST]
#   PS_ORG_BASE=<DEST> ./scripts/run_emacs_dev.sh    # then press F7

set -euo pipefail

REPO_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
DEST="${1:-${TMPDIR:-/tmp}/ps-blank-lines-playground}"

if [ ! -d "$REPO_DIR/samples/realistic" ]; then
  echo "Cannot find samples/realistic in $REPO_DIR" >&2
  exit 1
fi

echo "Building playground in $DEST"
rm -rf "$DEST"
mkdir -p "$DEST"
rsync -a --exclude='.git' --exclude='.claude' "$REPO_DIR/samples/realistic/" "$DEST/"

git -C "$DEST" init -q -b main

commit() {
  git -C "$DEST" add -A
  git -C "$DEST" \
    -c user.name="Playground" \
    -c user.email="playground@example.invalid" \
    -c commit.gpgsign=false \
    commit -q --no-verify -m "$1"
}

commit "Initial import"

# A desktop edit: blank lines intact, content changes only.
python3 - "$DEST" <<'PY'
import pathlib, sys, re
root = pathlib.Path(sys.argv[1])
p = root / "Work" / "Career.org"
text = p.read_text(encoding="utf-8")
text = text.replace("** TODO ", "** DONE ", 1)
p.write_text(text, encoding="utf-8")
PY
commit "Desktop edit: mark one task done"

# Beorg: strips the boundary blanks AND reinserts one before every level-1
# heading from its own level settings — both halves of the damage.
python3 - "$DEST" <<'PY'
import pathlib, re, sys

def strip_boundaries(lines):
    """Drop blanks after a heading and before the next heading, keeping
    blank lines that sit strictly inside a body — which is exactly what the
    mobile apps do, since a body is one content string to them."""
    out, i = [], 0
    heading = re.compile(r"^\*+ ")
    while i < len(lines):
        line = lines[i]
        if line.strip() == "":
            run_end = i
            while run_end < len(lines) and lines[run_end].strip() == "":
                run_end += 1
            prev = out[-1] if out else None
            nxt = lines[run_end] if run_end < len(lines) else None
            after_heading = prev is not None and heading.match(prev)
            before_heading = nxt is not None and heading.match(nxt)
            if after_heading or before_heading or nxt is None:
                i = run_end          # drop the whole run
                continue
            out.extend(lines[i:run_end])
            i = run_end
            continue
        out.append(line)
        i += 1
    return out

def beorg_reinsert(lines):
    out = []
    for line in lines:
        if line.startswith("* ") and out and out[-1].strip() != "":
            out.append("")
        out.append(line)
    return out

root = pathlib.Path(sys.argv[1])
for name in ["Work/Programming.org", "ML/ML.org", "Mind/Productivity.org"]:
    p = root / name
    lines = p.read_text(encoding="utf-8").split("\n")
    p.write_text("\n".join(beorg_reinsert(strip_boundaries(lines))), encoding="utf-8")
PY
commit "Beorg sync: blank lines stripped and re-added by rule"

# Orgzly: strips and reinserts nothing, and reorders two siblings in one file.
python3 - "$DEST" <<'PY'
import pathlib, re, sys
sys.path.insert(0, "")

def strip_all_boundaries(lines):
    out, i = [], 0
    heading = re.compile(r"^\*+ ")
    while i < len(lines):
        line = lines[i]
        if line.strip() == "":
            run_end = i
            while run_end < len(lines) and lines[run_end].strip() == "":
                run_end += 1
            prev = out[-1] if out else None
            nxt = lines[run_end] if run_end < len(lines) else None
            if (prev is not None and heading.match(prev)) or \
               (nxt is not None and heading.match(nxt)) or nxt is None:
                i = run_end
                continue
            out.extend(lines[i:run_end])
            i = run_end
            continue
        out.append(line)
        i += 1
    return out

root = pathlib.Path(sys.argv[1])
for name in ["Body/Health.org", "Inbox.org"]:
    p = root / name
    lines = p.read_text(encoding="utf-8").split("\n")
    p.write_text("\n".join(strip_all_boundaries(lines)), encoding="utf-8")

# Swap the first two level-2 siblings under some level-1 heading, so the
# seam-resolution path gets exercised too.
p = root / "Body" / "Health.org"
lines = p.read_text(encoding="utf-8").split("\n")
starts = [i for i, l in enumerate(lines) if l.startswith("** ")]
if len(starts) >= 3:
    a, b, c = starts[0], starts[1], starts[2]
    lines = lines[:a] + lines[b:c] + lines[a:b] + lines[c:]
    p.write_text("\n".join(lines), encoding="utf-8")
PY
commit "Orgzly sync: blank lines stripped, two tasks reordered"

echo
echo "Done. $(git -C "$DEST" rev-list --count HEAD) commits, working tree left damaged."
git -C "$DEST" log --oneline
echo
echo "Try it:"
echo "  PS_ORG_BASE=$DEST ./scripts/run_emacs_dev.sh"
echo "  then press F7"
