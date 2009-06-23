# -*- coding: iso-8859-2 -*-

# Formátovací funkce pro datové objekty
# 
# Copyright (C) 2002-2009 Brailcom, o.p.s.
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

"""Formátovací funkce pro datové objekty.

"""

from pytis.output import *
import pytis.presentation


P_NAME = 'P_NAME'
"""Parametr resolveru identifikující specifikační soubor."""
P_KEY = 'P_KEY'
"""Parametr resolveru identifikující datový klíč."""
P_ROW = 'P_ROW'
"""Parametr resolveru identifikující datový řádek."""
P_CONDITION = 'P_CONDITION'
"""Parametr resolveru identifikující podmínku pro 'pytis.data.Data.select()'."""
P_SORTING = 'P_SORTING'
"""Parametr resolveru identifikující třídění pro 'pytis.data.Data.select()'."""
P_DATA= 'P_DATA'
"""Parametr resolveru identifikující datový objekt."""


def data_table(resolver, name, condition=None, sorting=None,
               **long_table_args):
    """Jednoduchý tisk tabulky dat.

    Zadaná tabulka bude jednoduchým způsobem zformátována na výstup.  Výběr dat
    je specifikován argumenty předanými konstruktoru použitého formátovače.
    Pro prezentační podobu tabulky jsou přiměřeně použity specifikace v jejím
    view.

    Argumenty:

      resolver -- resolver specifikací, instance třídy 'pytis.util.Resolver'
      name -- identifikátor datového objektu a view pro resolver (jako
        string), stejný jako například ve formulářích
      condition -- podmínka výběru řádků tabulky ve formátu argumentu
        'condition' metody 'pytis.util.Data.select()'; může být též 'None',
        v kterémžto případě je podmínka získána z parametru resolveru
        '(name, P_CONDITION)'
      sorting -- specifikace třídění řádků tabulky ve formátu argumentu 'sort'
        metody 'pytis.util.Data.select()'; může být též 'None', v kterémžto
        případě je specifikace získána z parametru resolveru
        '(name, P_SORTING)'
      long_table_args -- dodatečné argumenty předané konstruktoru třídy
        'LongTable'
    
    """
    assert isinstance(name, basestring), repr(name)
    if condition is None:
        try:
            condition = resolver.p((name, P_CONDITION))
        except ResolverError:
            pass
    if sorting is None:
        sorting = resolver.p((name, P_SORTING))
    # Prezentace
    view = resolver.get(name, 'view_spec')
    data_spec = resolver.get(name, 'data_spec')
    import config
    data = data_spec.create(dbconnection_spec=config.dbconnection)
    import pytis.form
    columns = []
    for cid in view.columns():
        f = view.field(cid)
        width = f.column_width()
        if width == 0:
            continue
        label = f.column_label()
        if isinstance(f.type(data), pytis.data.Number):
            alignment = LongTable.Column.ALIGN_RIGHT
        else:
            alignment = LongTable.Column.ALIGN_LEFT
        tc = LongTable.Column(label, width, alignment=alignment)
        tc.id = cid                     # fuj, viz `table_row' níže
        columns.append(tc)
    # Data
    data.select(condition=condition, sort=sorting)
    # Formátování
    presented_row = \
      pytis.presentation.PresentedRow(view.fields(), data, None,
                                    singleline=True)
    def table_row(*args, **kwargs):
        row = data.fetchone()
        if row is None:
            return None
        presented_row.set_row(row)
        return [presented_row.format(c.id, secure=True) for c in columns]
    long_table_args['separator_margin'] = \
      long_table_args['line_separator_margin'] = UFont(0.2)
    return LongTable(columns, table_row, **long_table_args)
    

def data_item(resolver, name, column, key=None):
    """Exportovaná hodnota položky záznamu datového objektu.

    Jinak řečeno, tato funkce způsobí vložení příslušné \"buňky\" dat na
    výstup.  Aby toto bylo možno udělat, musí být známa příslušná datová buňka.
    Ta je identifikována přímo záznamem nebo jeho klíčem a identifikátorem
    sloupce.

    Argumenty:

      resolver -- resolver specifikací, instance třídy 'pytis.util.Resolver'
      name -- identifikátor datového objektu a view pro resolver (jako
        string), stejný jako například ve formulářích
      column -- stringový identifikátor sloupce datového objektu, jehož
        hodnota bude vložena na výstup
      key -- klíč řádku (ve formě parametru pro 'pytis.data.Data.row()'); může
        být též 'None', v kterémžto případě je klíč získán z parametru
        resolveru '(name, P_KEY)' nebo je získán přímo datový řádek z parametru
        resolveru '(name, P_ROW)'

    """
    assert type(name) == type('')
    assert type(column) == type('')
    if key is None:
        try:
            key = resolver.p((name, P_KEY))
        except ResolverError:
            pass
    data_spec = resolver.get(name, 'data_spec')
    import config
    data = data_spec.create(dbconnection_spec=config.dbconnection)
    if key is None:
        row = resolver.p((name, P_ROW))
    else:
        row = data.row(key)
    if row is None:
        return ''
    view = resolver.get(name, 'view_spec')
    fields = view.fields()
    presented_row = pytis.presentation.PresentedRow(fields, data, row)
    try:
        value = presented_row.format(column, secure=True)
    except KeyError:
        raise TemplateException(_("Chybný odkaz na sloupec"),
                                name, column)
    return value
