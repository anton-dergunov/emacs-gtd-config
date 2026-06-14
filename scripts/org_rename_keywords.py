#!/usr/bin/env python3
"""
org_rename_keywords.py

One-off migration: rename TODO keywords in org headlines to the new 4-char
convention (see CLAUDE.md):

  IN-PROGRESS -> INPR
  WAITING     -> WAIT
  SOMEDAY     -> MAYB

Only the headline keyword position (right after the leading stars) is
replaced; occurrences of these words elsewhere in titles, tags, or body text
are left untouched.

Usage:
  # Rename keywords in every *.org file under samples/ (default)
  python scripts/org_rename_keywords.py

  # Rename keywords in specific files or directories (recursed for *.org)
  python scripts/org_rename_keywords.py path/to/file.org path/to/dir
"""

import re
import sys
from pathlib import Path

KEYWORD_MAP = {
    "IN-PROGRESS": "INPR",
    "WAITING": "WAIT",
    "SOMEDAY": "MAYB",
}

KEYWORD_PATTERN = re.compile(
    r"^(\*+\s+)(" + "|".join(re.escape(k) for k in KEYWORD_MAP) + r")(\s)",
    re.MULTILINE,
)

REPO_ROOT = Path(__file__).resolve().parent.parent
DEFAULT_DIRS = [REPO_ROOT / "samples"]


def rename_keywords_text(text: str) -> str:
    return KEYWORD_PATTERN.sub(
        lambda m: m.group(1) + KEYWORD_MAP[m.group(2)] + m.group(3), text
    )


def rename_file(path: Path) -> None:
    text = path.read_text(encoding="utf-8")
    renamed = rename_keywords_text(text)
    if renamed != text:
        path.write_text(renamed, encoding="utf-8")


def default_files():
    for directory in DEFAULT_DIRS:
        if directory.is_dir():
            yield from sorted(directory.rglob("*.org"))


def collect_files(paths):
    for arg in paths:
        path = Path(arg)
        if path.is_dir():
            yield from sorted(path.rglob("*.org"))
        else:
            yield path


def main(argv):
    files = list(collect_files(argv)) if argv else list(default_files())
    for path in files:
        rename_file(path)


if __name__ == "__main__":
    main(sys.argv[1:])
