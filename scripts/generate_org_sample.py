#!/usr/bin/env python3
"""Generate a large, realistic Org file by repeating an existing sample.

A one-off helper for performance testing: it takes a real Org sample and
duplicates its content until the output reaches a target size (or a given
number of copies), so you can find at what file size editing/scrolling starts
to feel slow.

Each copy is wrapped in its own top-level heading and the sample's own headings
are demoted one level so they nest underneath, keeping the result well-formed
Org. A single ``#+TITLE`` is written at the top.

Examples
--------
  # ~1 MB file from the default sample
  python3 scripts/generate_org_sample.py --size 1MB --output /tmp/big.org

  # exactly 50 copies of a chosen sample
  python3 scripts/generate_org_sample.py \\
      --source samples/realistic/Areas/Career.org \\
      --copies 50 --output /tmp/big.org

  # a bit over 1 MB, flat (no added headings) to mimic an inbox of paragraphs
  python3 scripts/generate_org_sample.py --size 1.2MB --flat --output /tmp/inbox.org
"""

from __future__ import annotations

import argparse
import math
import re
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent
DEFAULT_SOURCE = REPO_ROOT / "samples" / "realistic" / "Areas" / "Career.org"

_SIZE_UNITS = {"": 1, "B": 1, "KB": 1024, "MB": 1024 ** 2, "GB": 1024 ** 3}
_HEADING_RE = re.compile(r"^(\*+)(\s)", re.MULTILINE)
_LEADING_KEYWORD_RE = re.compile(r"^[ \t]*#\+[A-Za-z_]+:.*\n", re.IGNORECASE)


def parse_size(text: str) -> int:
    """Parse a human size like '1MB', '500KB', '1.5mb', or a plain byte count."""
    m = re.fullmatch(r"\s*([0-9]*\.?[0-9]+)\s*([KMG]?B?)\s*", text, re.IGNORECASE)
    if not m:
        raise argparse.ArgumentTypeError(
            f"invalid size {text!r} (use e.g. 1MB, 500KB, 1.5MB, or a byte count)"
        )
    value, unit = float(m.group(1)), m.group(2).upper()
    return int(value * _SIZE_UNITS[unit])


def strip_leading_keywords(text: str) -> str:
    """Drop a leading block of ``#+KEYWORD:`` lines (e.g. #+TITLE, #+TIMEZONE)."""
    while True:
        m = _LEADING_KEYWORD_RE.match(text)
        if not m:
            return text.lstrip("\n")
        text = text[m.end():]


def demote_headings(text: str) -> str:
    """Add one leading star to every Org heading so the body nests one level deeper."""
    return _HEADING_RE.sub(r"*\1\2", text)


def build_unit(body: str, index: int, flat: bool) -> str:
    """Return one repeated unit: a wrapper heading plus the (demoted) sample body."""
    if flat:
        # No added heading; just a horizontal-rule separator between copies.
        return f"\n-----\n\n{body.strip()}\n"
    return f"\n* Sample copy {index}\n\n{demote_headings(body).strip()}\n"


def generate(body: str, title: str, *, target_bytes: int | None,
             copies: int | None, flat: bool) -> tuple[str, int]:
    """Build the output text. Returns (text, copies_written)."""
    header = f"#+TITLE: {title}\n"
    if copies is not None:
        units = [build_unit(body, i + 1, flat) for i in range(copies)]
        return header + "".join(units), copies

    # Size-driven: estimate, then top up until we cross the target.
    assert target_bytes is not None
    unit_bytes = len(build_unit(body, 1, flat).encode("utf-8")) or 1
    est = max(1, math.ceil((target_bytes - len(header.encode("utf-8"))) / unit_bytes))
    parts = [header]
    size = len(header.encode("utf-8"))
    i = 0
    # Cap iterations defensively (target / unit can't realistically exceed this).
    while size < target_bytes and i < est * 4 + 100:
        i += 1
        unit = build_unit(body, i, flat)
        parts.append(unit)
        size += len(unit.encode("utf-8"))
    return "".join(parts), i


def main(argv: list[str]) -> int:
    p = argparse.ArgumentParser(
        description=__doc__,
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )
    p.add_argument("--source", type=Path, default=DEFAULT_SOURCE,
                   help=f"Org file to duplicate (default: {DEFAULT_SOURCE.relative_to(REPO_ROOT)})")
    p.add_argument("--output", "-o", type=Path, required=True,
                   help="Path to write the generated Org file")
    group = p.add_mutually_exclusive_group(required=True)
    group.add_argument("--size", type=parse_size,
                       help="Target size, e.g. 1MB, 500KB, 1.5MB, or a byte count")
    group.add_argument("--copies", type=int,
                       help="Exact number of copies of the source to emit")
    p.add_argument("--title", default="Large Sample", help="Value for the #+TITLE line")
    p.add_argument("--flat", action="store_true",
                   help="Concatenate copies with '-----' separators and no added "
                        "headings (mimics a long heading-free inbox)")
    args = p.parse_args(argv)

    if not args.source.is_file():
        p.error(f"source not found: {args.source}")
    if args.copies is not None and args.copies < 1:
        p.error("--copies must be >= 1")

    body = strip_leading_keywords(args.source.read_text(encoding="utf-8"))
    if not body.strip():
        p.error(f"source has no content after stripping keywords: {args.source}")

    text, copies = generate(
        body, args.title,
        target_bytes=args.size, copies=args.copies, flat=args.flat,
    )
    args.output.write_text(text, encoding="utf-8")

    data = text.encode("utf-8")
    print(
        f"Wrote {args.output}\n"
        f"  source : {args.source}\n"
        f"  copies : {copies}\n"
        f"  size   : {len(data):,} bytes ({len(data) / 1024 ** 2:.2f} MB)\n"
        f"  lines  : {text.count(chr(10)):,}"
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
