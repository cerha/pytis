# -*- coding: utf-8 -*-

# Copyright (C) 2018-2026 Tomáš Cerha <t.cerha@gmail.com>
# Copyright (C) 2007-2017 OUI Technology Ltd.
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

from __future__ import print_function
from __future__ import unicode_literals

import pytis
import pytis.data
import pytis.form
import pytis.presentation as pp
import pytis.util
import sys

from pytis.api import app


def dump_spec(spec_name, todo=None, path=None):
    pytis.util.log(pytis.util.OPERATIONAL, "Dumping specification:", spec_name)
    if spec_name in ("Nemovitosti.Smlouva", "Nemovitosti.SmlouvaSpotreba", "Nemovitosti.SumSkRok",
                     "UctoUzaverky.KurzRozdilDoklad"):
        return []

    try:
        specification = pytis.config.resolver.specification(spec_name)
        view_spec = pytis.config.resolver.get(spec_name, 'view_spec')
    except pytis.util.ResolverError as e:
        return [str(e)]

    output = []

    table = specification.table
    if isinstance(table, str):
        dbdef, sa_table, key, kind = None, None, None, 'tabulka'
    else:
        dbdef = table
        if issubclass(dbdef, pytis.data.gensqlalchemy.SQLView):
            kind = 'view'
        elif issubclass(dbdef, pytis.data.gensqlalchemy.SQLFunctional):
            kind = 'funkce'
        else:
            kind = 'tabulka'
        with pytis.data.gensqlalchemy.local_search_path(dbdef.default_search_path()):
            sa_table = pytis.data.gensqlalchemy.object_by_class(dbdef)
        table = f"{sa_table.schema}.{sa_table.name}"
        key = sa_table.primary_key.columns

    for label, info in (
            ("Náhled:", view_spec.title()),
            ("Položka menu:", path and f" → ".join(m.title for m in path)),
            ("Specifikace náhledu:", f"`{spec_name}`"),
            ("DB specifikace:", dbdef and f"`{dbdef.__module__}.{dbdef.__name__}`"),
            (f"DB {kind}:", table and f"`{table}`"),
            ("Primární klíč:", key and ', '.join(f"`{c.name}`" for c in key)),
            ("Podmínka:", specification.condition and f"`{specification.condition}`"),
    ):
        if info:
            output.append(f"| {label:>20} | {info:<52} |")

    description = view_spec.description()
    if description:
        output.append('\n> ' + description)

    help = view_spec.help()
    if help:
        output.append('\n>     ' + '\n> '.join(help.strip().splitlines()))

    output.append("\n**Políčka**:")
    for f in view_spec.fields():
        label = f.column_label()
        if not label:
            label = f.label() or f.id()
        elif f.label() and f.label() != label:
            label += ' (' + f.label() + ')'
        colname = f.dbcolumn()
        if sa_table is not None and hasattr(sa_table.c, colname):
            col = getattr(sa_table.c, colname)
            coltype = f" `{col.type}`"
        else:
            col = None
            coltype = ''
        output.append(f"* {label}: `{colname}`{coltype}")
        if col is not None:
            for fk in col.foreign_keys:
                output.append(f"  - cizí klíč: `{fk.target_fullname}`")
        cb = f.codebook()
        if cb:
            output.append(f"  - číselník: `{cb}`")

    profiles = view_spec.profiles().unnest()
    if profiles:
        output.append("\n**Profily**:")
        for profile in profiles:
            output.append(f"* {profile.title()}: {profile.descr() or ''}")
            for attr in ('filter', 'sorting', 'grouping', 'columns', 'aggregations',
                         'folding', 'column_widths', 'errors'):
                v = getattr(profile, attr)()
                if v:
                    output.append(f"  - {attr}: `{v}`")


    actions = view_spec.actions(unnest=True)
    if actions:
        output.append("\n**Akce**:")
        for action in actions:
            h = action.handler()
            obj = getattr(h, '__self__', None)
            qualname = getattr(h, '__qualname__', None)
            if obj:
                handler = f"{obj.__class__.__module__}.{obj.__class__.__name__}.{h.__name__}"
            elif qualname:
                handler = f"{h.__module__}.{qualname}"
            else:
                handler = f"{h.__module__}.{h.__name__}"
            output.append(f"* {action.title()}: `{handler}()`")

    bindings = [(b.name(), b.title()) for b in view_spec.bindings() if b.name()]
    if bindings:
        output.append("\n**Vedlejší formuláře**:")
        for name, title in bindings:
            output.append(f"* {title}: `{name}`")
            if todo is not None:
                todo.add(name)
    return output


def dump_item(output, todo, done, path, item):
    if isinstance(item, pp.MenuSeparator):
        return
    output.append(f"\n\n{'#' * (len(path) + 1)} {item.title}\n")
    if isinstance(item, pp.Menu):
        for i in item.items:
            dump_item(output, todo, done, path + (item,), i)
    else:
        cmd = item.command
        spec_name = cmd.args.get('specification', cmd.args.get('name'))
        if spec_name:
            if cmd.name in ('Application.new_record', 'Application.api_new_record'):
                output.append(f"Vloží nový záznam do {spec_name}")
            elif cmd.name in ('Application.run_form', 'Application.api_run_form'):
                done.add(spec_name)
                output.extend(dump_spec(spec_name, todo, path=path + (item,)))


def dump_menu():
    output, todo, done = [], set(), set()
    if pytis.form.app:
        menu = pytis.form.app.menu
    else:
        try:
            pytis.form.Application(headless=True)
            menu = pytis.form.app.menu
        finally:
            # Release the pytis.form.Application instance from pytis.api.app to make sure
            # the tests outside this class don't use the previously created instance.
            pytis.form.app.ExitMainLoop()
    for item in menu:
        dump_item(output, todo, done, (), item)
    missing = (todo - done)
    if missing:
        output.append(f"\n# Nezařazeno\n")
        for spec_name in sorted(missing):
            output.append(f"\n\n## {spec_name}\n")
            output.extend(dump_spec(spec_name))
    return output


def main():
    if len(sys.argv) == 2:
        config, spec_name = sys.argv[1], None
    else:
        config, spec_name = sys.argv[1:]
    pytis.config.add_command_line_options(('pytis', '--config', config))
    if spec_name:
        output = dump_spec(spec_name)
    else:
        output = dump_menu()
    print("\n".join(output))


if __name__ == '__main__':
    main()
