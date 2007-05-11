#!/usr/bin/env python
# -*- coding: iso-8859-2 -*-

# Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2007 Brailcom, o.p.s.
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

import unittest

import pytis
from pytis.form import * 

tests = pytis.util.test.TestSuite()

############
# field.py #
############

class PresentedRow_(unittest.TestCase):
    def setUp(self):
        key = pytis.data.ColumnSpec('a', pytis.data.Integer())
        columns = (
            key,
            pytis.data.ColumnSpec('b', pytis.data.Integer()),
            pytis.data.ColumnSpec('c', pytis.data.Integer()),
            pytis.data.ColumnSpec('d', pytis.data.Integer()))
        self._data = pytis.data.Data(columns, key)
        def twice(row):
            return row['c'].value() * 2
        def sum(row):
            b, c = (row['b'].value(), row['c'].value())
            if b is None or c is None:
                return None
            return b + c
        def inc(row):
            return row['sum'].value() + 1
        def gt5(row, key):
            return row['sum'].value() > 5
        self._fields = (FieldSpec('a'),
                        FieldSpec('b'),
                        FieldSpec('c', default=lambda : 5),
                        FieldSpec('d', 
                                  computer=Computer(twice, depends=('c',)),
                                  editable=Computer(gt5, depends=('sum',))),
                        FieldSpec('e', type=pytis.data.Integer(),
                                  default=88),
                        FieldSpec('sum', type=pytis.data.Integer(),
                                  computer=Computer(sum, depends=('b','c'))),
                        FieldSpec('inc', type=pytis.data.Integer(),
                                  computer=Computer(inc, depends=('sum',))))
        
    def _check_values(self, row, pairs):
        for k, v in pairs:
            assert row[k].value() == v, (k, v, row[k].value())
    def test_init(self):
        row = PresentedRow(self._fields, self._data, None, new=True)
        self._check_values(row, (('a', None),
                                 ('b', None),
                                 ('c', 5),
                                 ('d', 10),
                                 ('e', 88)))
        data_row = pytis.data.Row((
            ('a', pytis.data.Value(pytis.data.Integer(), 'xx')),
            ('b', pytis.data.Value(pytis.data.Integer(), 100)),
            ('c', pytis.data.Value(pytis.data.Integer(), 77)),
            ('d', pytis.data.Value(pytis.data.Integer(), 18))))
        row = PresentedRow(self._fields, self._data, data_row)
        self._check_values(row, (('a', 'xx'),
                                 ('b', 100),
                                 ('c', 77),
                                 ('d', 18)))
        row['c'] = pytis.data.Value(pytis.data.Integer(), 88)
        #row2 = PresentedRow(self._fields, self._data, row)
        self._check_values(row, (('a', 'xx'),
                                 ('b', 100),
                                 ('c', 88),
                                 ('d', 176)))
        # TODO: dodělat
    def test_prefill(self):
        row = PresentedRow(self._fields, self._data, None, new=True,
                           prefill={'a': 'xx', 'b': 3, 'd': 77})
        self._check_values(row, (('a', 'xx'),
                                 ('b', 3),
                                 ('c', 5),
                                 ('d', 77),
                                 ('e', 88)))
    def test_prefill_default(self):
        row = PresentedRow(self._fields, self._data, None, new=True,
                           prefill={'b': 22, 'c': 33, 'd': 44, 'e': 55})
        self._check_values(row, (('b', 22),
                                 ('c', 33),
                                 ('d', 44),
                                 ('e', 55)))
    def test_computer(self):
        row = PresentedRow(self._fields, self._data, None, new=True,
                           prefill={'b': 3})
        assert row.get('sum', lazy=True).value() == None
        assert row['sum'].value() == 8
        assert row.get('sum', lazy=True).value() == 8
        self._check_values(row, (('d', 10), ('sum', 8), ('inc', 9)))
        row['c'] = pytis.data.Value(pytis.data.Integer(), 100)
        self._check_values(row, (('d', 200), ('sum', 103), ('inc', 104)))
    def test_prefill_computer(self):
        row = PresentedRow(self._fields, self._data, None, new=True,
                           prefill={'b': 2, 'c': 2, 'sum': 88})
        self._check_values(row, (('b', 2),
                                 ('c', 2),
                                 ('sum', 88),
                                 ('inc', 89)))
    def test_editable(self):
        row = PresentedRow(self._fields, self._data, None,
                           prefill={'b': 2, 'c': 1})
        assert row.editable('a')
        assert not row.editable('d')
        row['b'] = pytis.data.Value(pytis.data.Integer(), 5)
        assert row.editable('d')
    def test_callback(self):
        row = PresentedRow(self._fields, self._data, None, new=True, prefill={'b': 3})
        changed = []
        def callback(id):
            def cb():
                x = row[id].value()
                changed.append(id)
            return cb
        for id in ('a', 'b', 'c', 'd', 'sum', 'inc'):
            row.register_callback(row.CALL_CHANGE, id, callback(id))
        #self._check_values(row, (('d', 10), ('sum', 8), ('inc', 9)))
        #assert 'd' in changed and 'sum' in changed and 'inc' in changed, changed
        #del changed[0:len(changed)]
        row['c'] = pytis.data.Value(pytis.data.Integer(), 100)
        #self._check_values(row, (('d', 200), ('sum', 103), ('inc', 104)))
        assert 'd' in changed and 'sum' in changed and 'inc' in changed, changed
        del changed[0:len(changed)]
        data_row = pytis.data.Row((
            ('a', pytis.data.Value(pytis.data.Integer(), 'xx')),
            ('b', pytis.data.Value(pytis.data.Integer(), 10)),
            ('c', pytis.data.Value(pytis.data.Integer(), 20)),
            ('d', pytis.data.Value(pytis.data.Integer(), 30))))
        row.set_row(data_row)
        assert 'a' in changed and 'b' in changed and 'c' in changed and 'd' in changed \
               and 'sum' in changed and 'inc' in changed, changed
        
    def test_editability_callbacks(self):
        enabled = [None] # we need a mutable object...
        row = PresentedRow(self._fields, self._data, None, prefill={'a': 6})
        def callback():
            enabled[0] = row.editable('d')
        row.register_callback(row.CALL_EDITABILITY_CHANGE, 'd', callback)
        assert enabled[0] is None, enabled[0]
        row['a'] = pytis.data.Value(pytis.data.Integer(), 8)
        assert enabled[0] is None, enabled[0]
        row['c'] = pytis.data.Value(pytis.data.Integer(), 3)
        assert enabled[0] is False
        row['b'] = pytis.data.Value(pytis.data.Integer(), 2)
        assert enabled[0] is False
        row['b'] = pytis.data.Value(pytis.data.Integer(), 3)
        assert enabled[0] is True
        row['c'] = pytis.data.Value(pytis.data.Integer(), 2)
        assert enabled[0] is False
    def test_has_key(self):
        row = PresentedRow(self._fields, self._data, None)
        assert row.has_key('a')
        assert row.has_key('inc')
        assert not row.has_key('blabla')
    def test_changed(self):
        row = PresentedRow(self._fields, self._data, None)
        assert not row.changed()
        row['b'] = pytis.data.Value(pytis.data.Integer(), 333)
        assert row.changed()
    def test_field_changed(self):
        row = PresentedRow(self._fields, self._data, None,
                           prefill={'b': 3, 'c': 8})
        assert not row.field_changed('a')
        assert not row.field_changed('b')
        assert not row.field_changed('c')
        row['b'] = pytis.data.Value(pytis.data.Integer(), 333)
        assert not row.field_changed('a')
        assert row.field_changed('b')
        assert not row.field_changed('c')
    def test_keys(self):
        row = PresentedRow(self._fields, self._data, None)
        assert row.keys().sort() == map(lambda f: f.id(), self._fields).sort()
    def test_display(self):
        C = pytis.data.ColumnSpec
        S = pytis.data.String
        V = pytis.data.Value
        rows = [pytis.data.Row((('x', V(S(), x)), ('y', V(S(), y))))
                for x,y in (('1','FIRST'), ('2','SECOND'), ('3','THIRD'))]
        edata = pytis.data.DataFactory(pytis.data.MemData,
                                       (C('x', S()), C('y', S())), data=rows)
        enum = pytis.data.DataEnumerator(edata)
        key = C('a', pytis.data.Integer())
        columns = (key,
                   C('b', S(enumerator=enum)),
                   C('c', S(enumerator=enum)),
                   C('d', S(enumerator=enum)))
        data = pytis.data.Data(columns, key)
        fields = (FieldSpec('a'),
                  FieldSpec('b', display='y'),
                  FieldSpec('c', display=lambda x: '-'+x+'-'),
                  FieldSpec('d', display=(lambda x: x.lower(), 'y')),
                  )
        row = PresentedRow(fields, data, None,
                           prefill={'b': '2', 'c': '3', 'd': '1'})
        assert row.display('a') == '', row.display('a')
        assert row.display('b') == 'SECOND', row.display('b')
        assert row.display('c') == '-3-', row.display('c')
        assert row.display('d') == 'first', row.display('d')
        
        
tests.add(PresentedRow_)

def get_tests():
    return tests

if __name__ == '__main__':
    unittest.main() #defaultTest='get_tests')
