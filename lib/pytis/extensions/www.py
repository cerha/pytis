# -*- coding: iso-8859-2 -*-
#
# Copyright (C) 2005, 2006 Brailcom, o.p.s.
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

"""N�stroje pro generov�n� HTML na z�klad� dat z pytisu."""

import pytis.form
from pytis.util import *
import config
resolver = pytis.util.FileResolver(config.def_dir)
pytis.form.NullApplication(resolver)
import HyperText
from HyperText.HTML import TABLE, TR, TD, Select
from pytis.extensions import dbselect, data_create


def DBTable(spec, columns,
            condition=None, sort=(), **attrs):
    """Generuje HTML tabulku.

    Argumenty:

      spec -- n�zev specifikace
      columns -- seznam sloupc�, kter� se pou�ij� v HTML tabulce
      condition -- podm�nka odpov�daj�c� argumentu vol�n� pytis.data.select()
      sort -- �azen� odpov�daj�c� argumentu vol�n� pytis.data.select()    

    Vrac� instanci HyperText.TABLE.
    """
    t = apply(TABLE, (), attrs)
    dbrows = dbselect(spec, condition=condition, sort=sort)
    rows = [[r[c].export() for c in columns] for r in dbrows]
    if len(rows) > 0:
        for row in rows:
            r = apply(TR, tuple(map(TD, row)))
            t.append(r)
    return t        


def form_validate(spec, prefill):
    # Sestav�me datov� objekt
    success, data = data_create(spec)
    if not data:
        return None, None
    failed = []
    row = []
    for c in data.columns():
        if prefill.has_key(c.id()):
            value, error = c.type().validate(prefill[c.id()])
            if error:
                failed.append(c.id())
                continue
            else:
                row.append((c.id(), value))
    if len(failed) > 0:
        return None, failed
    else:
        return pytis.data.Row(row), None

        
def PopupCB(spec, name, column, returned_column,
            condition=None, sort=(), **attrs):
    """Generuje popup list na z�klad� hodnot z ��seln�ku.

    Argumenty:

      spec -- n�zev specifikace
      name -- jm�no pro HTML widget
      label -- popis pro HTML widget
      column -- n�zev sloupce ��seln�ku, jeho� hodnoty se budou zobrazovat
      returned_column -- n�zev sloupce ��seln�ku, jeho� hodnoty se budou vracet
      sort -- �azen� odpov�daj�c� argumentu vol�n� pytis.data.select()    
      condition -- podm�nka odpov�daj�c� argumentu vol�n� pytis.data.select()

    Vrac� instanci HyperText.Select nebo None v p��pad� ne�sp�chu.
    """
    dbrows = dbselect(spec, condition=condition, sort=sort)
    if len(dbrows) == 0:
        return None
    options = [(r[column].value(), r[returned_column].value())
               for r in dbrows]
    return Select(options, name=name, **attrs)
