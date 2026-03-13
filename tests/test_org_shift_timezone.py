import scripts.org_shift_timezone as tz


# ------------------------------------------------
# Helpers
# ------------------------------------------------

ORG_SAMPLE = """#+TITLE: Test
#+TIMEZONE: Asia/Makassar

Meeting
<2026-02-27 Fri 23:30-23:45>
"""


def write_org(tmp_path, content=ORG_SAMPLE):
    file = tmp_path / "test.org"
    file.write_text(content)
    return file


# ------------------------------------------------
# Header tests
# ------------------------------------------------

def test_parse_header():
    lines = ORG_SAMPLE.splitlines(True)
    tz_name, label = tz.parse_header(lines)
    assert tz_name == "Asia/Makassar"


def test_update_header():
    lines = ORG_SAMPLE.splitlines(True)
    new = tz.update_header(lines, "Europe/London")

    text = "".join(new)
    assert "#+TIMEZONE: Europe/London" in text
    assert "#+TZ_LABEL: Europe/London" in text


# ------------------------------------------------
# Timestamp conversion
# ------------------------------------------------

def test_convert_timestamp_simple():
    result = tz.convert_timestamp(
        "2026-02-27",
        "10:00",
        None,
        "UTC",
        "Europe/London",
    )

    assert "10:00" in result


def test_cross_midnight():
    result = tz.convert_timestamp(
        "2026-02-27",
        "23:30",
        None,
        "Asia/Makassar",
        "Europe/London",
    )

    # London is earlier → date changes
    assert "2026-02-27" in result or "2026-02-26" in result


def test_convert_file_multiple():
    content = """#+TIMEZONE: UTC
<2026-01-01 Thu 10:00>
<2026-01-01 Thu 12:00>
"""

    lines = content.splitlines(True)

    out = tz.convert_file(lines, "UTC", "Europe/Berlin")

    assert len(out) == 3
    assert "<" in out[1]
    assert "<" in out[2]


def test_date_only_timestamp_unchanged():
    content = """#+TIMEZONE: UTC
<2026-03-15 Sun>
"""

    lines = content.splitlines(True)

    out = tz.convert_file(lines, "UTC", "Europe/Berlin")

    assert "<2026-03-15 Sun>" in "".join(out)


def test_date_without_weekday_unchanged():
    content = """#+TIMEZONE: UTC
<2026-03-15>
"""

    lines = content.splitlines(True)

    out = tz.convert_file(lines, "UTC", "Europe/Berlin")

    assert "<2026-03-15>" in "".join(out)


def test_midnight_24_hour_range():
    content = """#+TIMEZONE: Asia/Makassar
<2026-02-27 Fri 22:30-24:00>
"""

    lines = content.splitlines(True)

    out = tz.convert_file(lines, "Asia/Makassar", "Europe/London")

    text = "".join(out)

    assert "<" in text
    assert "24:00" not in text


def test_time_range_over_midnight_kept_single_timestamp():
    result = tz.convert_timestamp(
        "2026-03-15",
        "23:00",
        "01:00",
        "UTC",
        "UTC",
    )

    assert "<2026-03-15 Sun 23:00-01:00>" == result


def test_cross_midnight_becomes_same_day_backward():
    result = tz.convert_timestamp(
        "2026-03-15",
        "23:00",
        "01:00",
        "Asia/Makassar",
        "Europe/London",
    )

    assert result == "<2026-03-15 Sun 15:00-17:00>"


def test_cross_midnight_forward_timezone():
    result = tz.convert_timestamp(
        "2026-03-15",
        "23:00",
        "01:00",
        "Europe/London",
        "Asia/Makassar",
    )

    assert result == "<2026-03-16 Mon 07:00-09:00>"


def test_same_day_becomes_midnight_cross_backward():
    result = tz.convert_timestamp(
        "2026-03-15",
        "06:00",
        "08:00",
        "Asia/Makassar",
        "Europe/London",
    )

    assert result == "<2026-03-14 Sat 22:00-00:00>"


def test_same_day_forward_timezone():
    result = tz.convert_timestamp(
        "2026-03-15",
        "18:00",
        "20:00",
        "Europe/London",
        "Asia/Makassar",
    )

    assert result == "<2026-03-16 Mon 02:00-04:00>"


# ------------------------------------------------
# Timezone resolution (offline)
# ------------------------------------------------

def test_resolve_exact_timezone():
    assert tz.resolve_timezone_offline("Europe/London") == "Europe/London"


def test_resolve_city():
    assert tz.resolve_timezone_offline("London") == "Europe/London"


def test_resolve_abbreviation():
    result = tz.resolve_timezone_offline("UTC")
    assert result == "UTC"


def test_resolve_offset():
    assert tz.resolve_timezone_offline("UTC+2").startswith("Etc/GMT")


def test_bad_timezone_returns_none():
    assert tz.resolve_timezone_offline("Blablah") is None


# ------------------------------------------------
# End-to-end file update
# ------------------------------------------------

def test_full_conversion(tmp_path):
    file = write_org(tmp_path)

    tz.main(["-t", "Europe/London", str(file)])

    text = file.read_text()
    assert "Europe/London" in text
    assert "<" in text


# ------------------------------------------------
# CLI behaviour
# ------------------------------------------------


def test_cli_without_timezone(tmp_path, monkeypatch):
    file = write_org(tmp_path)

    monkeypatch.setattr("builtins.input", lambda _: "")

    tz.main([str(file)])

    assert file.exists()


def test_missing_header_skipped(tmp_path):
    file = tmp_path / "bad.org"
    file.write_text("<2026-01-01 Thu 10:00>")

    tz.main(["-t", "UTC", str(file)])

    assert file.read_text().startswith("<")
