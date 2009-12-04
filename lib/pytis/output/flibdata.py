# -*- coding: iso-8859-2 -*-

# Form�tovac� funkce pro datov� objekty
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

"""Form�tovac� funkce pro datov� objekty.

"""

from pytis.output import *
import pytis.presentation


P_NAME = 'P_NAME'
"""Parametr resolveru identifikuj�c� specifika�n� soubor."""
P_KEY = 'P_KEY'
"""Parametr resolveru identifikuj�c� datov� kl��."""
P_ROW = 'P_ROW'
"""Parametr resolveru identifikuj�c� datov� ��dek."""
P_CONDITION = 'P_CONDITION'
"""Parametr resolveru identifikuj�c� podm�nku pro 'pytis.data.Data.select()'."""
P_SORTING = 'P_SORTING'
"""Parametr resolveru identifikuj�c� t��d�n� pro 'pytis.data.Data.select()'."""
P_DATA= 'P_DATA'
"""Parametr resolveru identifikuj�c� datov� objekt."""


def data_table(resolver, name, condition=None, sorting=None,
               **long_table_args):
    """Jednoduch� tisk tabulky dat.

    Zadan� tabulka bude jednoduch�m zp�sobem zform�tov�na na v�stup.  V�b�r dat
    je specifikov�n argumenty p�edan�mi konstruktoru pou�it�ho form�tova�e.
    Pro prezenta�n� podobu tabulky jsou p�im��en� pou�ity specifikace v�jej�m
    view.

    Argumenty:

      resolver -- resolver specifikac�, instance t��dy 'pytis.util.Resolver'
      name -- identifik�tor datov�ho objektu a view pro resolver (jako
        string), stejn� jako nap��klad ve formul���ch
      condition -- podm�nka v�b�ru ��dk� tabulky ve form�tu argumentu
        'condition' metody 'pytis.util.Data.select()'; m��e b�t t� 'None',
        v�kter�m�to p��pad� je podm�nka z�sk�na z�parametru resolveru
        '(name, P_CONDITION)'
      sorting -- specifikace t��d�n� ��dk� tabulky ve form�tu argumentu 'sort'
        metody 'pytis.util.Data.select()'; m��e b�t t� 'None', v�kter�m�to
        p��pad� je specifikace z�sk�na z�parametru resolveru
        '(name, P_SORTING)'
      long_table_args -- dodate�n� argumenty p�edan� konstruktoru t��dy
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
        tc.id = cid                     # fuj, viz `table_row' n�e
        columns.append(tc)
    # Data
    data.select(condition=condition, sort=sorting)
    # Form�tov�n�
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
    """Exportovan� hodnota polo�ky z�znamu datov�ho objektu.

    Jinak �e�eno, tato funkce zp�sob� vlo�en� p��slu�n� \"bu�ky\" dat na
    v�stup.  Aby toto bylo mo�no ud�lat, mus� b�t zn�ma p��slu�n� datov� bu�ka.
    Ta je identifikov�na p��mo z�znamem nebo jeho kl��em a identifik�torem
    sloupce.

    Argumenty:

      resolver -- resolver specifikac�, instance t��dy 'pytis.util.Resolver'
      name -- identifik�tor datov�ho objektu a view pro resolver (jako
        string), stejn� jako nap��klad ve formul���ch
      column -- stringov� identifik�tor sloupce datov�ho objektu, jeho�
        hodnota bude vlo�ena na v�stup
      key -- kl�� ��dku (ve form� parametru pro 'pytis.data.Data.row()'); m��e
        b�t t� 'None', v�kter�m�to p��pad� je kl�� z�sk�n z�parametru
        resolveru '(name, P_KEY)' nebo je z�sk�n p��mo datov� ��dek z�parametru
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
        raise TemplateException(_("Chybn� odkaz na sloupec"),
                                name, column)
    return value
