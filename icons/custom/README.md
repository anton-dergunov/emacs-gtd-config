This directory is for your own agenda category icons — additions for
categories not covered by `icons/stock/`, or overrides of stock icons (a
file here with the same name as one in `icons/stock/` wins, silently). See
the "Agenda category icons" section of the main README for details.

`icons/custom/` is gitignored by default, so anything you drop here stays out
of this repo's git history unless you explicitly `git add -f` it.

To add a new icon, you can download it (default size=24) from the
[Google Material Design Icons collection](https://fonts.google.com/icons)
under Apache 2.0. After downloading a new icon, run
`python scripts/fix_icon_svg.py` to normalize it to this config's expected
`height`/`viewBox`/`width`/`fill` attributes.
