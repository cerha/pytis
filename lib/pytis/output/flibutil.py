# -*- coding: iso-8859-2 -*-

# R�zn� form�tovac� funkce
# 
# Copyright (C) 2002, 2003, 2005 Brailcom, o.p.s.
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

"""R�zn� form�tovac� funkce.

Pro jm�na ve�ejn�ch funkc� plat� konvence, �e za��naj� prefixem 'f_'.

"""

from pytis.output import *


_FONT_STEP = 1.2

def f_larger(*contents):
    """Nech� 'contents' je vys�zeno fontem v�t�� velikosti vzhledem k�okol�."""
    return FontSize(_FONT_STEP, *contents)

def f_smaller(*contents):
    """Nech� 'contents' je vys�zeno fontem men�� velikosti vzhledem k�okol�."""
    return FontSize(1.0/_FONT_STEP, *contents)


def f_table(*data, **kwargs):
    """Jednoduch� tabulka.

    Argumenty:

      data -- nepr�zdn� sekvence nepr�zdn�ch sekvenc�, odpov�d� ��dk�m (vn�j��
        sekvence) form�tovan�m do sloupc� (vnit�n� sekvence); v�echny vnit�n�
        sekvence mus� m�t stejnou d�lku
      kwargs -- argumenty p�edan� konstruktoru t��dy 'Table'

    Jedn� se o�zcela jednoduchou tabulku s�automaticky zarovnan�mi sloupci,
    nep�esahuj�c� velikost str�nky.

    """
    column_spec = Table.Column(alignment=Table.Column.ALIGN_LEFT)
    return Table((column_spec,) * len(data[0]), *data, **kwargs)
