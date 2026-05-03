#!/usr/bin/env python3

"""
org_check_conflicts.py
======================

Scan Org-mode files and detect:

1. Overlapping tasks
2. Tasks with insufficient gap between them

Only considers timestamps with explicit time ranges.

Output includes:
    - file name
    - line number
    - task title
    - time range

Features
--------
- Reads all .org files in a directory (non-recursive)
- Ignores date-only timestamps
- Detects overlaps
- Detects small gaps between tasks
- Configurable gap threshold (default: 15 minutes)

Example
-------
python org_check_conflicts.py ./org
python org_check_conflicts.py ./org --gap 10
"""

import os
import re
import argparse
from datetime import datetime, timedelta
from typing import List, Tuple


TIMESTAMP_RE = re.compile(
    r"<(\d{4}-\d{2}-\d{2})\s+\w+\s+(\d{2}:\d{2})-(\d{2}:\d{2})>"
)

HEADING_RE = re.compile(r"^(\*+)\s+(.*)")


# ------------------------------------------------
# Data structure
# ------------------------------------------------

class Event:
    def __init__(self, file, line, title, date, start, end):
        self.file = file
        self.line = line
        self.title = title
        self.date = date
        self.start = start
        self.end = end

    def start_dt(self):
        return datetime.combine(self.date, self.start)

    def end_dt(self):
        return datetime.combine(self.date, self.end)

    def __repr__(self):
        return f"{self.file}:{self.line} | {self.title} | {self.start}-{self.end}"


# ------------------------------------------------
# Parsing
# ------------------------------------------------

def parse_file(path):
    events = []

    current_title = None

    with open(path) as f:
        for idx, line in enumerate(f, start=1):

            h = HEADING_RE.match(line)
            if h:
                current_title = h.group(2).strip()

            matches = TIMESTAMP_RE.findall(line)

            for date_str, start_str, end_str in matches:
                if not current_title:
                    continue

                date = datetime.strptime(date_str, "%Y-%m-%d").date()
                start = datetime.strptime(start_str, "%H:%M").time()
                end = datetime.strptime(end_str, "%H:%M").time()

                events.append(
                    Event(
                        file=os.path.basename(path),
                        line=idx,
                        title=current_title,
                        date=date,
                        start=start,
                        end=end,
                    )
                )

    return events


def load_events(directory):
    all_events = []

    for fname in os.listdir(directory):
        if fname.endswith(".org"):
            path = os.path.join(directory, fname)
            all_events.extend(parse_file(path))

    return all_events


# ------------------------------------------------
# Conflict detection
# ------------------------------------------------

def group_by_day(events):
    result = {}

    for e in events:
        result.setdefault(e.date, []).append(e)

    return result


def find_overlaps(events: List[Event]):
    overlaps = []

    events = sorted(events, key=lambda e: e.start_dt())

    for i in range(len(events)):
        for j in range(i + 1, len(events)):
            a = events[i]
            b = events[j]

            if b.start_dt() >= a.end_dt():
                break

            overlaps.append((a, b))

    return overlaps


def find_small_gaps(events: List[Event], gap_minutes: int):
    small_gaps = []

    events = sorted(events, key=lambda e: e.start_dt())

    for i in range(len(events) - 1):
        a = events[i]
        b = events[i + 1]

        gap = (b.start_dt() - a.end_dt()).total_seconds() / 60

        if 0 <= gap < gap_minutes:
            small_gaps.append((a, b, gap))

    return small_gaps


# ------------------------------------------------
# Formatting
# ------------------------------------------------

def fmt_event(e: Event):
    return f"{e.file}:{e.line} | {e.title} | {e.start.strftime('%H:%M')}-{e.end.strftime('%H:%M')}"


def print_results(events, gap_minutes):
    by_day = group_by_day(events)

    for day in sorted(by_day.keys()):
        day_events = by_day[day]

        overlaps = find_overlaps(day_events)
        small_gaps = find_small_gaps(day_events, gap_minutes)

        if not overlaps and not small_gaps:
            continue

        print(f"\n=== {day} ===")

        if overlaps:
            print("\nOverlapping tasks:")
            for a, b in overlaps:
                print(f"  - {fmt_event(a)}")
                print(f"    {fmt_event(b)}")

        if small_gaps:
            print(f"\nTasks with gap < {gap_minutes} min:")
            for a, b, gap in small_gaps:
                print(f"  - ({gap:.0f} min)")
                print("    ", fmt_event(a))
                print("    ", fmt_event(b))


# ------------------------------------------------
# Main
# ------------------------------------------------

def main(argv=None):
    parser = argparse.ArgumentParser(
        description="Detect overlapping tasks in Org-mode files"
    )

    parser.add_argument("directory")

    parser.add_argument(
        "--gap",
        type=int,
        default=15,
        help="Minimum gap between tasks in minutes (default: 15)"
    )

    parser.add_argument(
        "--from-date",
        help="Start checking from this date (YYYY-MM-DD)"
    )

    parser.add_argument(
        "--include-past",
        action="store_true",
        help="Include past events"
    )

    args = parser.parse_args(argv)

    events = load_events(args.directory)

    if args.from_date:
        start_date = datetime.strptime(args.from_date, "%Y-%m-%d").date()
    elif not args.include_past:
        start_date = datetime.now().date()
    else:
        start_date = None

    if start_date:
        events = [e for e in events if e.date >= start_date]

    print_results(events, args.gap)


if __name__ == "__main__":
    main()
