#!/usr/bin/env python3
"""Render markdown with rich, forcing tables to use the full console width.

Rich's TableElement creates Table(expand=False) by default, so markdown
tables render at their natural content width and leave the right side of
the screen blank. This wrapper monkey-patches TableElement.__rich_console__
to set expand=True.

Console width comes from $COLUMNS (set by the caller).
"""
import os
import sys
import glob

try:
    import rich  # noqa: F401
except ImportError:
    # rich-cli is typically installed via pipx; add its venv site-packages.
    for path in glob.glob(os.path.expanduser(
            "~/.local/share/pipx/venvs/rich-cli/lib/python*/site-packages")):
        sys.path.insert(0, path)
    import rich  # noqa: F401

from rich.console import Console
from rich.markdown import Markdown, TableElement
from rich.table import Table
from rich import box


def _expanded_table(self, console, options):
    table = Table(box=box.SIMPLE, pad_edge=False,
                  style="markdown.table.border",
                  show_edge=True, collapse_padding=True,
                  expand=True)
    if self.header is not None and self.header.row is not None:
        for column in self.header.row.cells:
            heading = column.content.copy()
            heading.stylize("markdown.table.header")
            table.add_column(heading)
    if self.body is not None:
        for row in self.body.rows:
            table.add_row(*[e.content for e in row.cells])
    yield table


TableElement.__rich_console__ = _expanded_table


def main():
    if len(sys.argv) != 2:
        sys.stderr.write("usage: render-md.py FILE\n")
        sys.exit(2)
    with open(sys.argv[1]) as f:
        Console(force_terminal=True).print(Markdown(f.read()))


if __name__ == "__main__":
    main()
