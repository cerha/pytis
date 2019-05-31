# -*- coding: utf-8 -*-

# Formátovací funkce pro datové objekty
#
# Copyright (C) 2002-2011, 2013, 2014 Brailcom, o.p.s.
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
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

"""Formátovací funkce pro datové objekty."""

from past.builtins import basestring
from lcg import UFont
import pytis.presentation
import pytis.util
import pytis.output

_ = pytis.util.translations('pytis-wx')

P_NAME = 'P_NAME'
"""Output parameter storing the specification."""
P_KEY = 'P_KEY'
"""Output parameter storing the data key."""
P_ROW = 'P_ROW'
"""Output parameter storing the data row."""
P_CONDITION = 'P_CONDITION'
"""Output parameter storing the condition for 'pytis.data.Data.select()' call."""
P_ARGUMENTS = 'P_ARGUMENTS'
"""Output parameter storing the arguments of the data object function."""
P_SORTING = 'P_SORTING'
"""Output parameter storing the sorting for 'pytis.data.Data.select()' call."""
P_DATA = 'P_DATA'
"""Output parameter storing the data object."""
P_LANGUAGE = 'P_LANGUAGE'
"""Output parameter storing the language code."""


def data_table(view, data, condition=None, sorting=None, transaction=None, **long_table_args):
    """Jednoduchý tisk tabulky dat.

    Zadaná tabulka bude jednoduchým způsobem zformátována na výstup.  Výběr dat
    je specifikován argumenty předanými konstruktoru použitého formátovače.
    Pro prezentační podobu tabulky jsou přiměřeně použity specifikace v jejím
    view.

    Argumenty:

      view -- presentation specification of the table;
        'pytis.presentation.Specification' instance
      data -- data object to use for querying the database; 'pytis.data.Data'
        instance
      condition -- podmínka výběru řádků tabulky ve formátu argumentu
        'condition' metody 'pytis.util.Data.select()'
      sorting -- specifikace třídění řádků tabulky ve formátu argumentu 'sort'
        metody 'pytis.util.Data.select()'
      transaction -- transaction object to use for database operations
      long_table_args -- dodatečné argumenty předané konstruktoru třídy
        'LongTable'

    """
    import pytis.data
    import pytis.form
    import pytis.presentation
    assert isinstance(view, pytis.presentation.ViewSpec), view
    assert isinstance(data, pytis.data.Data), data
    # Prezentace
    presented_row = pytis.presentation.PresentedRow(view.fields(), data, None, singleline=True)
    columns = []
    for cid in view.columns():
        f = view.field(cid)
        width = f.column_width()
        if width == 0:
            continue
        label = f.column_label()
        if isinstance(presented_row[cid].type(), pytis.data.Number):
            alignment = pytis.output.LongTable.Column.ALIGN_RIGHT
        else:
            alignment = pytis.output.LongTable.Column.ALIGN_LEFT
        tc = pytis.output.LongTable.Column(label, width, alignment=alignment)
        tc.id = cid  # fuj, viz `table_row' níže
        columns.append(tc)
    # Data
    data.select(condition=condition, sort=sorting, transaction=transaction)
    # Formátování

    def table_row(*args, **kwargs):
        row = data.fetchone()
        if row is None:
            return None
        presented_row.set_row(row)
        return [presented_row.format(c.id, secure=True) for c in columns]
    long_table_args['separator_margin'] = \
        long_table_args['line_separator_margin'] = UFont(0.2)
    return pytis.output.LongTable(columns, table_row, **long_table_args)


def data_item(view, data, column, key=None, row=None):
    """Exportovaná hodnota položky záznamu datového objektu.

    Jinak řečeno, tato funkce způsobí vložení příslušné \"buňky\" dat na
    výstup.  Aby toto bylo možno udělat, musí být známa příslušná datová buňka.
    Ta je identifikována přímo záznamem nebo jeho klíčem a identifikátorem
    sloupce.

    Argumenty:

      view -- presentation specification of the table;
        'pytis.presentation.Specification' instance
      data -- data object to use for querying the database; 'pytis.data.Data'
        instance
      column -- stringový identifikátor sloupce datového objektu, jehož
        hodnota bude vložena na výstup
      key -- klíč řádku (ve formě parametru pro 'pytis.data.Data.row()')
      row -- the row containing the required value; 'pytis.data.Row' instance

    """
    assert isinstance(view, pytis.presentation.Specification), view
    assert isinstance(data, pytis.data.Data), data
    assert isinstance(column, basestring)
    assert row is None or isinstance(row, pytis.data.Row), row
    if row is None and key is not None:
        row = data.row(key)
    if row is None:
        return ''
    fields = view.fields()
    presented_row = pytis.presentation.PresentedRow(fields, data, row)
    try:
        value = presented_row.format(column, secure=True)
    except KeyError:
        raise pytis.output.TemplateException(_("Invalid column reference"), view.title(), column)
    return value
