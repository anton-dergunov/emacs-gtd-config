import scripts.fix_icon_svg as fix_icon_svg


RAW_DOWNLOAD = (
    '<svg xmlns="http://www.w3.org/2000/svg" height="24px" '
    'viewBox="0 -960 960 960" width="24px" fill="#000000">'
    '<path d="M1 2"/></svg>'
)

ALREADY_FIXED = (
    '<svg xmlns="http://www.w3.org/2000/svg" height="20px" '
    'viewBox="0 -960 960 960" width="24px" fill="#5f6368">'
    '<path d="M1 2"/></svg>'
)

NO_ATTRIBUTES = '<svg xmlns="http://www.w3.org/2000/svg"><path d="M1 2"/></svg>'


def test_fix_svg_text_normalizes_raw_download():
    fixed = fix_icon_svg.fix_svg_text(RAW_DOWNLOAD)
    assert 'height="20px"' in fixed
    assert 'width="24px"' in fixed
    assert 'fill="#5f6368"' in fixed
    assert 'viewBox="0 -960 960 960"' in fixed
    assert '<path d="M1 2"/>' in fixed


def test_fix_svg_text_is_idempotent():
    assert fix_icon_svg.fix_svg_text(ALREADY_FIXED) == ALREADY_FIXED


def test_fix_svg_text_inserts_missing_attributes():
    fixed = fix_icon_svg.fix_svg_text(NO_ATTRIBUTES)
    assert 'height="20px"' in fixed
    assert 'width="24px"' in fixed
    assert 'fill="#5f6368"' in fixed
    assert 'viewBox="0 -960 960 960"' in fixed
    assert 'xmlns="http://www.w3.org/2000/svg"' in fixed
    assert '<path d="M1 2"/>' in fixed


def test_fix_file_rewrites_in_place(tmp_path):
    svg_path = tmp_path / "Icon.svg"
    svg_path.write_text(RAW_DOWNLOAD, encoding="utf-8")

    fix_icon_svg.fix_file(svg_path)

    assert 'height="20px"' in svg_path.read_text(encoding="utf-8")
    assert 'fill="#5f6368"' in svg_path.read_text(encoding="utf-8")


def test_main_with_explicit_files(tmp_path):
    svg_path = tmp_path / "Icon.svg"
    svg_path.write_text(RAW_DOWNLOAD, encoding="utf-8")

    fix_icon_svg.main([str(svg_path)])

    assert 'fill="#5f6368"' in svg_path.read_text(encoding="utf-8")
