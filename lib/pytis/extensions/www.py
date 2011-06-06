# -*- coding: iso-8859-2 -*-
#
# Copyright (C) 2005, 2006, 2009, 2011 Brailcom, o.p.s.
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

import pytis.data
from pytis.util import *
import HyperText
from HyperText.HTML import TABLE, TR, TD, TH, Select, Href, URL, nbsp
from pytis.extensions import dbselect, data_object
import config
pytis.util.set_resolver(pytis.util.FileResolver(config.def_dir))

class BaseDBTable(object):

    def __init__(self, spec, columns, condition=None, sort=(),
                 show_headers=True, klass=None,
                 **attrs):
        """Vytvo� tabulku, jej� obsah je z�sk�n z datab�zov�ho objektu.

        Argumenty:

        spec -- n�zev specifikace
        columns -- seznam sloupc�, kter� se pou�ij� v HTML tabulce
        condition -- podm�nka odpov�daj�c� argumentu vol�n�
                     pytis.data.select()
        sort -- �azen� odpov�daj�c� argumentu vol�n� pytis.data.select()
        show_headers -- pokud je True, zobraz� se z�hlav� sloupc�, na z�klad�
                        'label' z jednotliv�ch pol��ek
        klass -- None nebo funkce jednoho argumentu, kter�m je ��dek tabulky
                 a kter� vrac� None nebo odpov�daj�c� styl
        attrs -- atributy pro HyperText.TABLE
        
        Vrac� instanci HyperText.TABLE.
        """        
        self._spec = spec
        self._columns = columns
        self._condition = condition
        self._sort = sort
        self._view = pytis.util.resolver().get(self._spec, 'view_spec')
        self._data = data_object(self._spec)
        self._fields = self._get_fields()
        self._klass = klass
        self._table = TABLE(**attrs)
        if show_headers:
            self._headers = self._get_headers()
            self._append_headers()

    def _get_headers(self):
        return [f.label() for f in self._fields]

    def _append_headers(self):
        headerline = TR(*[TH(h) for h in self._headers])
        self._table.append(headerline)

    def _get_fields(self):
        return [self._view.field(c) for c in self._columns]

    def _col_aligns(self):
        def align(f):
            column = self._data.find_column(f.id())
            if column and isinstance(column.type(), pytis.data.Number):
                return 'right'
            else:
                return 'left'
        return [align(f) for f in self._fields]

    def spec(self):
        return self._spec

    def columns(self):
        return self._columns

    def condition(self):
        return self._condition

    def sort(self):
        return self._sort

    def dbrows(self):
        rows = dbselect(self._spec, condition=self._condition,
                        sort=self._sort)
        return [pytis.data.Row([(c, r[c]) for c in self._columns])
                for r in rows]
        
    def table(self):
        secret_columns = [c for c in self._columns
                          if not self._data.permitted(c, pytis.data.Permission.VIEW)]
        aligns = self._col_aligns()
        rows = self.dbrows()
        if len(rows) > 0:
            for row in rows:
                r = TR()
                for i, c in enumerate(row):
                    if c in secret_columns:
                        val = c.type().secret_export()
                    else:
                        val = c.export()
                    if self._klass:
                        style = self._klass(row, self._columns[i])
                        if style:
                            r.append(TD(val, align=aligns[i],
                                klass=style))
                        else:    
                            r.append(TD(val, align=aligns[i]))
                    else:
                        r.append(TD(val, align=aligns[i]))
                self._table.append(r)
        return self._table
    

class BrowsableDBTable(BaseDBTable):

    def __init__(self, spec, columns, uri=None, pageno=1, limit=20,
                 **kwargs):
        self._uri = uri
        self._pageno = pageno
        self._limit = limit
        self._row_count = 0
        super(BrowsableDBTable, self).__init__(spec, columns, **kwargs)

    def dbrows(self):
        self._row_count = self._data.select(condition=self._condition,
                                      sort=self._sort)        
        offset = (self._pageno - 1) * self._limit
        self._data.skip(offset)
        rows = []
        i = 0
        while True:
            row = self._data.fetchone()
            i = i + 1
            if row is None or i >= self._limit or i > self._row_count:
                self._data.close()
                break
            rows.append(pytis.data.Row([(c,row[c])
                                        for c in self._columns]))
        return rows    

    def _labelfirst(self):
        return "|<<"

    def _labelprevious(self):
        return "<"

    def _labelnext(self):
        return ">"

    def _labellast(self):
        return ">>|"

    def lastpageno(self):
        return int(self._row_count / self._limit)        
           
    def first(self):
        return Href(URL(self._uri), self._labelfirst())

    def previous(self):
        previousno = max(self._pageno - 1, 1)
        return Href(URL(self._uri, pageno=str(previousno)),
                    self._labelprevious())
   
    def next(self):
        nextno = min(self._pageno + 1, self.lastpageno())
        return Href(URL(self._uri, pageno=str(nextno)),
                    self._labelnext())
    
    def last(self):
        return Href(URL(self._uri, pageno=str(self.lastpageno())),
                    self._labellast())

    def controls(self):
        f = self.first()
        p = self.previous()
        n = self.next()
        l = self.last()
        return f, nbsp, p, nbsp, n, nbsp, l

    def table(self):
        super(BrowsableDBTable, self).table()
        if self._uri is not None:
            controls = self.controls()
            rcontrols = TR(TD(colspan=len(self.columns()), *controls))
            self._table.append(rcontrols)
        return self._table            

def base_db_table(*args, **kwargs):
    t = BaseDBTable(*args, **kwargs)
    return t.table()

def browsable_db_table(*args, **kwargs):
    t = BrowsableDBTable(*args, **kwargs)
    return t.table()   


def form_validate(spec, prefill):
    # Sestav�me datov� objekt
    data = data_object(spec)
    if not data:
        return None, None
    failed = []
    row = []
    for c in data.columns():
        if c.id() in prefill:
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
            selected=None,
            condition=None, sort=(), **attrs):
    """Generuje popup list na z�klad� hodnot z ��seln�ku.

    Argumenty:

      spec -- n�zev specifikace
      name -- jm�no pro HTML widget
      label -- popis pro HTML widget
      column -- n�zev sloupce ��seln�ku, jeho� hodnoty se budou zobrazovat
      returned_column -- n�zev sloupce ��seln�ku, jeho� hodnoty se budou vracet
      selected -- None nebo seznam hodnot, kter� budou m�t v HTML hodnotu
                  selected
      sort -- �azen� odpov�daj�c� argumentu vol�n� pytis.data.select()    
      condition -- podm�nka odpov�daj�c� argumentu vol�n� pytis.data.select()

    Vrac� instanci HyperText.Select nebo None v p��pad� ne�sp�chu.
    """
    dbrows = dbselect(spec, condition=condition, sort=sort)
    if len(dbrows) == 0:
        return None
    options = [(r[column].value(), r[returned_column].value())
               for r in dbrows]
    if selected:
        if isinstance(selected, types.StringTypes):
            selected = xtuple(selected)
        if len(selected) > 1:
            attrs['multiple'] = 1
    else:
        selected = []
    return Select(options, selected=selected, name=name, **attrs)
