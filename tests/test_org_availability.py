import scripts.org_availability as avail
from datetime import datetime, date, time


# ------------------------------------------------
# Helpers
# ------------------------------------------------

ORG_SAMPLE = """* Test
SCHEDULED: <2026-05-04 Mon 10:00-11:00>
SCHEDULED: <2026-05-04 Mon 13:00-14:00>
SCHEDULED: <2026-05-05 Tue 15:00-16:00>
"""


def write_org(tmp_path, content=ORG_SAMPLE):
    d = tmp_path / "org"
    d.mkdir()
    file = d / "test.org"
    file.write_text(content)
    return d


# ------------------------------------------------
# Parsing
# ------------------------------------------------

def test_extract_events(tmp_path):
    d = write_org(tmp_path)

    events = avail.load_events(d)

    assert len(events) == 3
    assert events[0][0] == date(2026, 5, 4)


def test_ignore_date_only(tmp_path):
    content = """<2026-05-04 Mon>"""
    d = write_org(tmp_path, content)

    events = avail.load_events(d)

    assert len(events) == 0


# ------------------------------------------------
# Interval logic
# ------------------------------------------------

def test_subtract_simple():
    base = [(datetime(2026, 5, 4, 10), datetime(2026, 5, 4, 18))]
    busy = [(datetime(2026, 5, 4, 12), datetime(2026, 5, 4, 13))]

    free = avail.subtract_intervals(base, busy)

    assert len(free) == 2
    assert free[0][1].hour == 12
    assert free[1][0].hour == 13


def test_subtract_overlap_edges():
    base = [(datetime(2026, 5, 4, 10), datetime(2026, 5, 4, 18))]
    busy = [(datetime(2026, 5, 4, 10), datetime(2026, 5, 4, 12))]

    free = avail.subtract_intervals(base, busy)

    assert len(free) == 1
    assert free[0][0].hour == 12


# ------------------------------------------------
# Buffer logic
# ------------------------------------------------

def test_apply_buffer():
    events = [(datetime(2026, 5, 4, 10), datetime(2026, 5, 4, 11))]

    buffered = avail.apply_buffer(events, 15)

    assert buffered[0][0].minute == 45  # 09:45
    assert buffered[0][1].minute == 15  # 11:15


# ------------------------------------------------
# Filtering
# ------------------------------------------------

def test_min_duration_filter():
    slots = [
        (datetime(2026, 5, 4, 10), datetime(2026, 5, 4, 10, 20)),
        (datetime(2026, 5, 4, 11), datetime(2026, 5, 4, 12)),
    ]

    filtered = avail.filter_min_duration(slots, 30)

    assert len(filtered) == 1
    assert filtered[0][0].hour == 11


# ------------------------------------------------
# Formatting
# ------------------------------------------------

def test_format_12h():
    dt = datetime(2026, 5, 4, 15, 30)

    assert avail.format_time(dt, True) == "3:30pm"


def test_format_24h():
    dt = datetime(2026, 5, 4, 15, 30)

    assert avail.format_time(dt, False) == "15:30"


def test_inline_format():
    slots = [
        (datetime(2026, 5, 4, 10), datetime(2026, 5, 4, 11))
    ]

    out = avail.format_day_inline(date(2026, 5, 4), slots, True)

    assert "10:00am-11:00am" in out


def test_bullet_format():
    slots = [
        (datetime(2026, 5, 4, 10), datetime(2026, 5, 4, 11))
    ]

    out = avail.format_day_bullets(date(2026, 5, 4), slots, True)

    assert "- 10:00am-11:00am" in out


# ------------------------------------------------
# End-to-end CLI
# ------------------------------------------------

def test_end_to_end(tmp_path, capsys):
    d = write_org(tmp_path)

    avail.main([
        str(d),
        "--days", "2",
        "--layout", "inline",
        "--start-date", "2026-05-04",
    ])

    out = capsys.readouterr().out

    assert "May" in out
    assert "-" in out


def test_raw_mode(tmp_path, capsys):
    d = write_org(tmp_path)

    avail.main([
        str(d),
        "--days", "2",
        "--raw",
        "--start-date", "2026-05-04",
    ])

    out = capsys.readouterr().out

    assert out  # just ensure it runs


def test_exclude_weekends(tmp_path, capsys):
    d = write_org(tmp_path)

    avail.main([
        str(d),
        "--days", "7",
        "--start-date", "2026-05-04",
    ])

    out = capsys.readouterr().out

    # should not crash and should print something
    assert out
