import scripts.org_check_conflicts as cc
from datetime import date


# ------------------------------------------------
# Helpers
# ------------------------------------------------

ORG_SAMPLE = """* Task A
SCHEDULED: <2026-05-04 Mon 10:00-11:00>

* Task B
SCHEDULED: <2026-05-04 Mon 10:30-11:30>

* Task C
SCHEDULED: <2026-05-04 Mon 11:35-12:00>

* Task D
SCHEDULED: <2026-05-04 Mon 12:10-13:00>
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

def test_parse_events(tmp_path):
    d = write_org(tmp_path)

    events = cc.load_events(d)

    assert len(events) == 4
    assert events[0].title == "Task A"


# ------------------------------------------------
# Overlap detection
# ------------------------------------------------

def test_find_overlaps(tmp_path):
    d = write_org(tmp_path)
    events = cc.load_events(d)

    by_day = cc.group_by_day(events)
    day_events = by_day[date(2026, 5, 4)]

    overlaps = cc.find_overlaps(day_events)

    assert len(overlaps) == 1
    a, b = overlaps[0]

    assert "Task A" in a.title
    assert "Task B" in b.title


# ------------------------------------------------
# Gap detection
# ------------------------------------------------

def test_small_gaps(tmp_path):
    d = write_org(tmp_path)
    events = cc.load_events(d)

    by_day = cc.group_by_day(events)
    day_events = by_day[date(2026, 5, 4)]

    gaps = cc.find_small_gaps(day_events, 15)

    # Task C (11:35) follows Task B (ends 11:30) → 5 min gap
    assert any(g[2] < 15 for g in gaps)


# ------------------------------------------------
# CLI
# ------------------------------------------------

def test_cli_runs(tmp_path, capsys):
    d = write_org(tmp_path)

    cc.main([str(d), "--include-past"])

    out = capsys.readouterr().out

    assert "Overlapping" in out or "gap" in out
