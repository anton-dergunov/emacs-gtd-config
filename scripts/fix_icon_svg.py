#!/usr/bin/env python3
"""
fix_icon_svg.py

Normalizes Material Design Icon SVGs (https://fonts.google.com/icons,
default size=24) to the attributes expected by `lisp/ps-agenda-icons.el`:

  height="20px" viewBox="0 -960 960 960" width="24px" fill="#5f6368"

Usage:
  # Fix every *.svg under icons/stock/ and icons/custom/ (repo root)
  python scripts/fix_icon_svg.py

  # Fix specific files
  python scripts/fix_icon_svg.py path/to/Icon.svg [more.svg ...]
"""

import re
import sys
from pathlib import Path

ATTRIBUTES = {
    "height": "20px",
    "viewBox": "0 -960 960 960",
    "width": "24px",
    "fill": "#5f6368",
}

REPO_ROOT = Path(__file__).resolve().parent.parent
DEFAULT_DIRS = [REPO_ROOT / "icons" / "stock", REPO_ROOT / "icons" / "custom"]


def fix_svg_text(svg_text: str) -> str:
    """Set/override height, viewBox, width, and fill on the root <svg> tag."""
    match = re.search(r"<svg\b[^>]*>", svg_text)
    if not match:
        return svg_text

    tag = match.group(0)
    for name, value in ATTRIBUTES.items():
        pattern = rf'{name}="[^"]*"'
        replacement = f'{name}="{value}"'
        if re.search(pattern, tag):
            tag = re.sub(pattern, replacement, tag)
        else:
            tag = tag[:-1] + f' {replacement}>'

    return svg_text[: match.start()] + tag + svg_text[match.end():]


def fix_file(path: Path) -> None:
    text = path.read_text(encoding="utf-8")
    fixed = fix_svg_text(text)
    if fixed != text:
        path.write_text(fixed, encoding="utf-8")


def default_files():
    for directory in DEFAULT_DIRS:
        if directory.is_dir():
            yield from sorted(directory.glob("*.svg"))


def main(argv):
    files = [Path(arg) for arg in argv] if argv else list(default_files())
    for path in files:
        fix_file(path)


if __name__ == "__main__":
    main(sys.argv[1:])
