#!/usr/bin/env python3

"""
org_shift_timezone.py
=====================

Convert Org-mode timestamps between timezones.

The script updates all timestamps in-place and keeps a per-file
timezone header so travelling between regions becomes effortless.

Supported timezone input:
    • IANA timezone name      → Europe/London
    • City / place name       → London, Berlin, Bali, Buenos Aires
    • Abbreviation            → CET, EST, WITA, WIB, JST, etc.
    • UTC offset              → UTC+2, GMT-5
    • Automatic system detection

Features
--------
✓ Offline-first timezone resolution
✓ Online geolocation fallback (Nominatim + TimezoneFinder)
✓ Correct date rollover across midnight
✓ Updates Org headers automatically
✓ Batch processing of multiple files
✓ No hardcoded timezone aliases
✓ Cross-platform (macOS / Linux)

Org headers used:
    #+TIMEZONE: Europe/London
    #+TZ_LABEL: Europe London

Examples
--------
Auto-detect system timezone:

    python org_shift_timezone.py notes.org

Convert explicitly:

    python org_shift_timezone.py -t London notes.org

Convert multiple files:

    python org_shift_timezone.py -t CET *.org

Requirements
------------
    pip install geopy timezonefinder
"""


import os
import re
import difflib
import time
import subprocess
import platform
import argparse
from typing import Optional, cast
from functools import lru_cache
from datetime import datetime, UTC, timedelta
from zoneinfo import ZoneInfo, available_timezones
from geopy.geocoders import Nominatim
from geopy.exc import GeocoderTimedOut, GeocoderServiceError
from geopy.location import Location
from timezonefinder import TimezoneFinder


HEADER_TZ = "#+TIMEZONE:"
HEADER_LABEL = "#+TZ_LABEL:"


TIMESTAMP_RE = re.compile(
    r"<(\d{4}-\d{2}-\d{2})(?:\s+(\w+))?(?:\s+(\d{2}:\d{2})(?:-(\d{2}:\d{2}))?)?>"
)


# ------------------------------------------------
# Timezone helpers
# ------------------------------------------------

def system_timezone():
    """
    Return system IANA timezone name.
    Works reliably on macOS, Linux, and fallback environments.
    """

    system = platform.system()

    # ------------------------------------------------
    # macOS — BEST METHOD
    # ------------------------------------------------
    if system == "Darwin":
        try:
            link = os.readlink("/etc/localtime")
            # example:
            # /var/db/timezone/zoneinfo/Asia/Makassar
            if "zoneinfo/" in link:
                return link.split("zoneinfo/")[-1]
        except Exception:
            pass

    # ------------------------------------------------
    # Linux timedatectl
    # ------------------------------------------------
    if system == "Linux":
        try:
            out = subprocess.check_output(
                ["timedatectl"],
                stderr=subprocess.DEVNULL,
                text=True,
            )
            for line in out.splitlines():
                if "Time zone:" in line:
                    return line.split()[2]
        except Exception:
            pass

        try:
            with open("/etc/timezone") as f:
                return f.read().strip()
        except Exception:
            pass

    # ------------------------------------------------
    # Python fallback
    # ------------------------------------------------
    try:
        tz = datetime.now().astimezone().tzinfo
        if isinstance(tz, ZoneInfo):
            return tz.key
    except Exception:
        pass

    raise RuntimeError("Could not detect system timezone automatically.")


def extract_city(tz):
    return tz.split("/")[-1].replace("_", " ").lower()


@lru_cache
def canonical_timezones():
    return sorted(available_timezones())


@lru_cache
def build_abbreviation_map():
    """
    Build mapping like:
        EST -> America/New_York
        CET -> Europe/Berlin
        WITA -> Asia/Makassar
    using CURRENT timezone abbreviations.
    """
    now = datetime.now(UTC)

    mapping = {}

    for tz in canonical_timezones():
        try:
            abbr = now.astimezone(ZoneInfo(tz)).tzname()
            if not abbr:
                continue

            # keep first occurrence only
            mapping.setdefault(abbr.upper(), tz)

        except Exception:
            continue

    return mapping


OFFSET_RE = re.compile(r"(utc|gmt)\s*([+-]\d{1,2})", re.I)


def parse_offset(user_input):
    """UTC / GMT offset parsing"""
    m = OFFSET_RE.fullmatch(user_input.strip())
    if not m:
        return None

    offset = int(m.group(2))

    # IANA GMT sign is reversed
    sign = "+" if offset >= 0 else "-"
    return f"Etc/GMT{sign}{-offset}"


def resolve_timezone_offline(user_input):
    user_input = user_input.strip()
    user_lower = user_input.lower()

    tzs = canonical_timezones()

    # 1. Exact timezone name
    for tz in tzs:
        if tz.lower() == user_lower:
            return tz

    # 2. UTC/GMT offset
    offset = parse_offset(user_input)
    if offset:
        return offset

    # 3. Abbreviation (EST, CET, WITA, MSK, ...)
    abbr_map = build_abbreviation_map()
    abbr = user_input.upper()

    if abbr in abbr_map:
        return abbr_map[abbr]

    # 4. Exact city match
    city_matches = [
        tz for tz in tzs
        if extract_city(tz) == user_lower
    ]

    if len(city_matches) == 1:
        return city_matches[0]

    # 5. Strong fuzzy match
    scored = []

    for tz in tzs:
        city = extract_city(tz)
        score = difflib.SequenceMatcher(None, user_lower, city).ratio()
        scored.append((score, tz))

    scored.sort(reverse=True)

    best_score, best_match = scored[0]

    if best_score >= 0.9:
        print(f"Using best match: {best_match}")
        return best_match

    return None


def resolve_timezone_online(place):
    """
    Resolve human place name to IANA timezone.
    Robust against network timeout.
    """

    geolocator = Nominatim(user_agent="org-timezone-tool")

    tf = TimezoneFinder()

    print(f"Resolving location: {place}")

    location: Optional[Location] = None

    for attempt in range(3):
        try:
            result = cast(Optional[Location], geolocator.geocode(place))
            if result is not None:
                location = result
                break
        except (GeocoderTimedOut, GeocoderServiceError):
            print("Geocoder timeout — retrying...")
            time.sleep(1)

    if location is None:
        raise RuntimeError(
            "Could not resolve location (network timeout or unknown place)."
        )

    lat, lon = location.latitude, location.longitude

    tz = tf.timezone_at(lat=lat, lng=lon)

    if tz is None:
        raise RuntimeError("Could not determine timezone.")

    print(f"Location found: {location.address}")

    return tz


def resolve_timezone(user_input):
    # ---- OFFLINE FIRST ----
    tz = resolve_timezone_offline(user_input)

    if tz:
        print(f"Resolved locally: {tz}")
        return tz

    # ---- ONLINE FALLBACK ----
    print("No local match — trying online lookup...")
    try:
        tz = resolve_timezone_online(user_input)
    except Exception as e:
        raise RuntimeError(f"Online lookup failed: {e}")

    print(f"Resolved via geolocation: {tz}")

    confirm = input("Use this timezone? [Y/n] ").strip().lower()

    if confirm not in ("", "y", "yes"):
        raise RuntimeError("Aborted.")

    return tz


# ------------------------------------------------
# Header parsing
# ------------------------------------------------

def parse_header(lines):
    tz = None
    label = None

    for line in lines:
        if line.startswith(HEADER_TZ):
            tz = line.split(":", 1)[1].strip()
        if line.startswith(HEADER_LABEL):
            label = line.split(":", 1)[1].strip()

    return tz, label


def update_header(lines, new_tz):
    label = new_tz.replace("_", " ")

    new_lines = []
    tz_written = False
    label_written = False

    for line in lines:
        if line.startswith(HEADER_TZ):
            new_lines.append(f"{HEADER_TZ} {new_tz}\n")
            tz_written = True
        elif line.startswith(HEADER_LABEL):
            new_lines.append(f"{HEADER_LABEL} {label}\n")
            label_written = True
        else:
            new_lines.append(line)

    if not tz_written:
        new_lines.insert(1, f"{HEADER_TZ} {new_tz}\n")

    if not label_written:
        new_lines.insert(2, f"{HEADER_LABEL} {label}\n")

    return new_lines


# ------------------------------------------------
# Timestamp conversion
# ------------------------------------------------

def convert_timestamp(date, start, end, old_tz, new_tz):
    start_dt = datetime.fromisoformat(f"{date} {start}").replace(tzinfo=ZoneInfo(old_tz))
    start_dt = start_dt.astimezone(ZoneInfo(new_tz))

    new_date = start_dt.strftime("%Y-%m-%d")
    weekday = start_dt.strftime("%a")
    new_start = start_dt.strftime("%H:%M")

    new_end = None
    if end:
        if end == "24:00":
            end_dt = datetime.fromisoformat(f"{date} 00:00") + timedelta(days=1)
            end_dt = end_dt.replace(tzinfo=ZoneInfo(old_tz))
        else:
            end_dt = datetime.fromisoformat(f"{date} {end}").replace(tzinfo=ZoneInfo(old_tz))

        end_dt = end_dt.astimezone(ZoneInfo(new_tz))
        new_end = end_dt.strftime("%H:%M")

    if new_end:
        return f"<{new_date} {weekday} {new_start}-{new_end}>"

    return f"<{new_date} {weekday} {new_start}>"


def convert_file(lines, old_tz, new_tz):
    new_lines = []

    for line in lines:
        def repl(match):
            date = match.group(1)
            weekday = match.group(2)
            start = match.group(3)
            end = match.group(4)

            # If no time component => leave timestamp unchanged
            if start is None:
                return match.group(0)

            return convert_timestamp(
                date,
                start,
                end,
                old_tz,
                new_tz,
            )

        newline = TIMESTAMP_RE.sub(repl, line)
        new_lines.append(newline)

    return new_lines


# ------------------------------------------------
# Main
# ------------------------------------------------

def main(argv=None):
    parser = argparse.ArgumentParser(
        description="Convert Org-mode timestamps between timezones."
    )

    parser.add_argument(
        "-t",
        "--to",
        dest="target_tz",
        help="Target timezone (city, timezone name, abbreviation, or UTC offset)",
    )

    parser.add_argument(
        "files",
        nargs="+",
        help="Org files to convert",
    )

    args = parser.parse_args(argv)

    # ------------------------------------------------
    # Resolve target timezone
    # ------------------------------------------------

    if args.target_tz:
        new_tz = resolve_timezone(args.target_tz)
    else:
        detected = system_timezone()
        print(f"Detected timezone: {detected}")

        confirm = input("Use this timezone? [Y/n] ").strip().lower()

        if confirm.startswith("n"):
            user = input("Enter timezone/city: ")
            new_tz = resolve_timezone(user)
        else:
            new_tz = detected

    # ------------------------------------------------
    # Process files
    # ------------------------------------------------

    for filename in args.files:
        with open(filename) as f:
            lines = f.readlines()

        old_tz, _ = parse_header(lines)

        if not old_tz:
            print(f"{filename}: missing #+TIMEZONE header — skipped")
            continue

        if new_tz == old_tz:
            print(f"{filename}: timezone unchanged")
            continue

        print(f"{filename}: {old_tz} → {new_tz}")

        lines = convert_file(lines, old_tz, new_tz)
        lines = update_header(lines, new_tz)

        with open(filename, "w") as f:
            f.writelines(lines)

    print("Done.")


if __name__ == "__main__":
    main()

