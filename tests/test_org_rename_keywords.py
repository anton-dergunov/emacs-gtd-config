import scripts.org_rename_keywords as org_rename_keywords


def test_headline_keyword_replaced():
    text = "* IN-PROGRESS Finish report\n"
    assert org_rename_keywords.rename_keywords_text(text) == "* INPR Finish report\n"


def test_all_mapped_keywords_replaced():
    text = (
        "* IN-PROGRESS Task A\n"
        "** WAITING Task B\n"
        "*** SOMEDAY Task C\n"
    )
    expected = (
        "* INPR Task A\n"
        "** WAIT Task B\n"
        "*** MAYB Task C\n"
    )
    assert org_rename_keywords.rename_keywords_text(text) == expected


def test_non_headline_occurrences_untouched():
    text = (
        "* TODO Something about SOMEDAY plans\n"
        "  This task is currently IN-PROGRESS but not started.\n"
        "  :PROPERTIES:\n"
        "  :NOTES: WAITING on review\n"
        "  :END:\n"
    )
    assert org_rename_keywords.rename_keywords_text(text) == text


def test_fast_select_letters_untouched():
    text = "#+TODO: TODO NEXT IN-PROGRESS(i) WAITING(w) SOMEDAY(s) | DONE\n"
    # Not a headline (no leading stars), so left as-is.
    assert org_rename_keywords.rename_keywords_text(text) == text


def test_rename_file_rewrites_in_place(tmp_path):
    org_path = tmp_path / "Example.org"
    org_path.write_text("* IN-PROGRESS Task\n** WAITING Task 2\n", encoding="utf-8")

    org_rename_keywords.rename_file(org_path)

    assert org_path.read_text(encoding="utf-8") == "* INPR Task\n** WAIT Task 2\n"


def test_main_with_explicit_file(tmp_path):
    org_path = tmp_path / "Example.org"
    org_path.write_text("* SOMEDAY Task\n", encoding="utf-8")

    org_rename_keywords.main([str(org_path)])

    assert org_path.read_text(encoding="utf-8") == "* MAYB Task\n"


def test_main_with_directory_recurses(tmp_path):
    sub = tmp_path / "Areas"
    sub.mkdir()
    org_path = sub / "Career.org"
    org_path.write_text("* IN-PROGRESS Task\n", encoding="utf-8")

    org_rename_keywords.main([str(tmp_path)])

    assert org_path.read_text(encoding="utf-8") == "* INPR Task\n"
