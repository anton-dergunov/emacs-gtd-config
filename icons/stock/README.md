This directory holds the icons shipped with this config, used by
`lisp/ps-agenda-icons.el` for agenda category icons. Each `.svg` is matched
to an `Areas/<Category>.org` file by name (e.g. `Career.svg` ↔ `Career.org`).

The icons originate from the [Google Material Design Icons collection](https://fonts.google.com/icons) under Apache 2.0.

When adding new icons from this source for other categories:

1. Download the icon at default size=24.

2. Run `python scripts/fix_icon_svg.py` (with no arguments, it normalizes
   every `.svg` under `icons/stock/` and `icons/custom/`), or pass the
   downloaded file's path directly. This sets:

```xml
<svg xmlns="http://www.w3.org/2000/svg" height="20px" viewBox="0 -960 960 960" width="24px" fill="#5f6368">
```

To override or add to these icons without affecting this repo's git history,
put your own `.svg` files in `icons/custom/` instead.
