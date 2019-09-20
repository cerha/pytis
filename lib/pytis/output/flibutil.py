# -*- coding: utf-8 -*-

# Copyright (C) 2019-2020 Tomáš Cerha <t.cerha@gmail.com>
# Copyright (C) 2002-2013 OUI Technology Ltd.
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

"""Různé formátovací funkce.

Pro jména veřejných funkcí platí konvence, že začínají prefixem 'f_'.

"""

import pytis.output

_FONT_STEP = 1.2


def f_larger(*contents):
    """Nechť 'contents' je vysázeno fontem větší velikosti vzhledem k okolí."""
    return pytis.output.FontSize(_FONT_STEP, *contents)


def f_smaller(*contents):
    """Nechť 'contents' je vysázeno fontem menší velikosti vzhledem k okolí."""
    return pytis.output.FontSize(1.0 / _FONT_STEP, *contents)


def f_table(*data, **kwargs):
    """Jednoduchá tabulka.

    Argumenty:

      data -- neprázdná sekvence neprázdných sekvencí, odpovídá řádkům (vnější
        sekvence) formátovaným do sloupců (vnitřní sekvence); všechny vnitřní
        sekvence musí mít stejnou délku
      kwargs -- argumenty předané konstruktoru třídy 'Table'

    Jedná se o zcela jednoduchou tabulku s automaticky zarovnanými sloupci,
    nepřesahující velikost stránky.

    """
    column_spec = pytis.output.Table.Column(alignment=pytis.output.Table.Column.ALIGN_LEFT)
    return pytis.output.Table((column_spec,) * len(data[0]), *data, **kwargs)
