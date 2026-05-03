#!/usr/bin/env python3

"""
org_availability.py
===================

Generate availability windows from Org-mode agenda files.

Reads all .org files in a directory, extracts scheduled timestamps with time
ranges, and subtracts them from a default availability window.

Assumptions (by design):
- Local time only (no timezone handling)
- Only timestamps with explicit time ranges are considered
- Non-recursive directory scan

Features
--------
- Reads multiple org files (like org-agenda)
- Ignores date-only tasks
- Configurable working hours
- Configurable number of days
- Optional weekend exclusion
- Buffer time around meetings
- Minimum slot duration filtering
- "Raw" mode (no buffers, no filtering)
- 12h (AM/PM) and 24h formatting

Example
-------
python org_availability.py ./org \
    --days 15 \
    --start 10:00 \
    --end 22:00 \
    --buffer 15 \
    --min-duration 30

"""

import os
import re
import argparse
from datetime import datetime, timedelta, time
from typing import List, Tuple


TIMESTAMP_RE = re.compile(
    r"<(\d{4}-\d{2}-\d{2})\s+\w+\s+(\d{2}:\d{2})-(\d{2}:\d{2})>"
)


# ------------------------------------------------
# Helpers
# ------------------------------------------------

def parse_time(t: str) -> time:
    return datetime.strptime(t, "%H:%M").time()


def combine(dt, t):
    return datetime.combine(dt, t)


def daterange(start_date, days):
    for i in range(days):
        yield start_date + timedelta(days=i)


def is_weekend(d):
    return d.weekday() >= 5


# ------------------------------------------------
# Org parsing
# ------------------------------------------------

def extract_events_from_file(path):
    events = []

    with open(path) as f:
        for line in f:
            matches = TIMESTAMP_RE.findall(line)
            for date_str, start_str, end_str in matches:
                date = datetime.strptime(date_str, "%Y-%m-%d").date()
                start = parse_time(start_str)
                end = parse_time(end_str)

                events.append((date, start, end))

    return events


def load_events(directory):
    all_events = []

    for fname in os.listdir(directory):
        if fname.endswith(".org"):
            path = os.path.join(directory, fname)
            all_events.extend(extract_events_from_file(path))

    return all_events


# ------------------------------------------------
# Core logic
# ------------------------------------------------

def subtract_intervals(base: List[Tuple[datetime, datetime]],
                       busy: List[Tuple[datetime, datetime]]):
    """
    Subtract busy intervals from base intervals.
    """

    result = []

    for start, end in base:
        current = [(start, end)]

        for b_start, b_end in busy:
            new_current = []

            for c_start, c_end in current:
                # no overlap
                if b_end <= c_start or b_start >= c_end:
                    new_current.append((c_start, c_end))
                    continue

                # overlap -> split
                if b_start > c_start:
                    new_current.append((c_start, b_start))
                if b_end < c_end:
                    new_current.append((b_end, c_end))

            current = new_current

        result.extend(current)

    return result


def apply_buffer(events, buffer_minutes):
    buffered = []

    delta = timedelta(minutes=buffer_minutes)

    for start, end in events:
        buffered.append((start - delta, end + delta))

    return buffered


def filter_min_duration(slots, min_minutes):
    result = []
    for s, e in slots:
        if (e - s).total_seconds() >= min_minutes * 60:
            result.append((s, e))
    return result


# ------------------------------------------------
# Formatting
# ------------------------------------------------

def format_time(dt, use_12h):
    if use_12h:
        return dt.strftime("%-I:%M%p").lower()
    else:
        return dt.strftime("%H:%M")


def format_slot(s, e, use_12h):
    return f"{format_time(s, use_12h)}-{format_time(e, use_12h)}"


def format_day(date, slots, use_12h):
    day_str = date.strftime("%A, %-d %B")
    if not slots:
        return f"- {day_str}: (no availability)"

    slot_str = ", ".join(format_slot(s, e, use_12h) for s, e in slots)
    return f"- {day_str}: {slot_str}"


# ------------------------------------------------
# Main
# ------------------------------------------------

def main():
    parser = argparse.ArgumentParser(
        description="Generate availability from Org-mode files"
    )

    parser.add_argument("directory", help="Directory with .org files")

    parser.add_argument("--days", type=int, default=15)
    parser.add_argument("--start", default="10:00")
    parser.add_argument("--end", default="22:00")

    parser.add_argument("--buffer", type=int, default=15)
    parser.add_argument("--min-duration", type=int, default=30)

    parser.add_argument("--include-weekends", action="store_true")

    parser.add_argument(
        "--raw",
        action="store_true",
        help="Disable buffer and minimum duration filtering"
    )

    parser.add_argument(
        "--format",
        choices=["12h", "24h"],
        default="12h",
        help="Time format"
    )

    args = parser.parse_args()

    use_12h = args.format == "12h"

    work_start = parse_time(args.start)
    work_end = parse_time(args.end)

    events = load_events(args.directory)

    today = datetime.now().date()

    for day in daterange(today, args.days):

        if not args.include_weekends and is_weekend(day):
            continue

        base = [
            (
                combine(day, work_start),
                combine(day, work_end),
            )
        ]

        # collect events for this day
        busy = []

        for d, s, e in events:
            if d == day:
                busy.append((
                    combine(day, s),
                    combine(day, e)
                ))

        # apply buffer unless raw
        if not args.raw:
            busy = apply_buffer(busy, args.buffer)

        free = subtract_intervals(base, busy)

        # filter minimum duration unless raw
        if not args.raw:
            free = filter_min_duration(free, args.min_duration)

        print(format_day(day, free, use_12h))


if __name__ == "__main__":
    main()
