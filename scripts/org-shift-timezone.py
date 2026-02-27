#!/usr/bin/env python3

"""
org-shift-timezone.py — Convert Org-mode timestamps between timezones.

Features:
- Per-file timezone header
- Automatic system timezone detection
- Human-friendly timezone names
- Converts dates when crossing midnight
- Updates file in place
"""

import os
import re
import sys
from datetime import datetime
from zoneinfo import ZoneInfo, available_timezones
import difflib
import subprocess
import platform


HEADER_TZ = "#+TIMEZONE:"
HEADER_LABEL = "#+TZ_LABEL:"


TIMESTAMP_RE = re.compile(
    r"<(\d{4}-\d{2}-\d{2})\s+\w+\s+(\d{2}:\d{2})(?:-(\d{2}:\d{2}))?>"
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


def canonical_timezones():
    """
    Remove duplicate/linked zones.
    Keep one representative per real timezone.
    """
    groups = {}

    for tz in available_timezones():
        try:
            key = ZoneInfo(tz).key
            groups.setdefault(key, []).append(tz)
        except Exception:
            continue

    # choose shortest readable name as canonical
    canonical = []
    for key, names in groups.items():
        canonical.append(sorted(names, key=len)[0])

    return sorted(canonical)


def extract_city(tz):
    return tz.split("/")[-1].replace("_", " ").lower()


def resolve_timezone(user_input):
    user_input = user_input.strip().lower()

    tzs = canonical_timezones()

    # ------------------------------------------------
    # 1. Exact timezone match
    # ------------------------------------------------
    for tz in tzs:
        if user_input == tz.lower():
            return tz

    # ------------------------------------------------
    # 2. Exact city match
    # ------------------------------------------------
    city_matches = [
        tz for tz in tzs
        if user_input == extract_city(tz)
    ]

    if len(city_matches) == 1:
        return city_matches[0]

    # ------------------------------------------------
    # 3. Substring match (semantic)
    # ------------------------------------------------
    substring_matches = [
        tz for tz in tzs
        if user_input in extract_city(tz)
    ]

    if len(substring_matches) == 1:
        return substring_matches[0]

    # ------------------------------------------------
    # 4. Fuzzy match on CITY ONLY
    # ------------------------------------------------
    scored = []

    for tz in tzs:
        city = extract_city(tz)
        score = difflib.SequenceMatcher(None, user_input, city).ratio()
        scored.append((score, tz))

    scored.sort(reverse=True)

    best_score, best_match = scored[0]

    # Only auto-accept VERY strong matches
    if best_score >= 0.88:
        print(f"Using best match: {best_match}")
        return best_match

    # ------------------------------------------------
    # 5. Suggest reasonable options
    # ------------------------------------------------
    suggestions = [tz for score, tz in scored if score >= 0.6][:5]

    if not suggestions:
        raise ValueError(
            f"No reasonable timezone match for '{user_input}'."
        )

    print("Possible matches:")
    for i, tz in enumerate(suggestions):
        print(f"{i+1}. {tz}")

    choice = int(input("Choose timezone: "))
    return suggestions[choice - 1]


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
    dt = datetime.fromisoformat(f"{date} {start}")
    dt = dt.replace(tzinfo=ZoneInfo(old_tz))
    dt2 = dt.astimezone(ZoneInfo(new_tz))

    new_date = dt2.strftime("%Y-%m-%d")
    new_start = dt2.strftime("%H:%M")

    if end:
        dt_end = datetime.fromisoformat(f"{date} {end}")
        dt_end = dt_end.replace(tzinfo=ZoneInfo(old_tz))
        dt_end = dt_end.astimezone(ZoneInfo(new_tz))
        new_end = dt_end.strftime("%H:%M")
    else:
        new_end = None

    weekday = dt2.strftime("%a")

    if new_end:
        return f"<{new_date} {weekday} {new_start}-{new_end}>"
    else:
        return f"<{new_date} {weekday} {new_start}>"


def convert_file(lines, old_tz, new_tz):
    new_lines = []

    for line in lines:
        def repl(match):
            return convert_timestamp(
                match.group(1),
                match.group(2),
                match.group(3),
                old_tz,
                new_tz,
            )

        newline = TIMESTAMP_RE.sub(repl, line)
        new_lines.append(newline)

    return new_lines


# ------------------------------------------------
# Main
# ------------------------------------------------

def main():
    if len(sys.argv) < 2:
        print("Usage: python org-shift-timezone.py FILE [NEW_TIMEZONE]")
        sys.exit(1)

    filename = sys.argv[1]

    with open(filename) as f:
        lines = f.readlines()

    old_tz, _ = parse_header(lines)

    if not old_tz:
        print("ERROR: File has no #+TIMEZONE header.")
        sys.exit(1)

    if len(sys.argv) >= 3:
        new_tz = resolve_timezone(sys.argv[2])
    else:
        detected = system_timezone()
        print(f"Detected timezone: {detected}")
        confirm = input("Use this timezone? [Y/n] ")

        if confirm.lower().startswith("n"):
            user = input("Enter timezone/city: ")
            new_tz = resolve_timezone(user)
        else:
            new_tz = detected

    if new_tz == old_tz:
        print("Timezone unchanged.")
        return

    print(f"Converting {old_tz} → {new_tz}")

    lines = convert_file(lines, old_tz, new_tz)
    lines = update_header(lines, new_tz)

    with open(filename, "w") as f:
        f.writelines(lines)

    print("Done.")


if __name__ == "__main__":
    main()

