#!/usr/bin/env python
# -*- coding: iso-8859-2 -*-

# Copyright (C) 2001, 2002, 2003, 2004, 2005 Brailcom, o.p.s.
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
            return row['b'].value() + row['c'].value()
        def inc(row):
            return row['sum'].value() + 1
        def gt5(row, key):
            return row['a'].value() > 5
        self._fields = (FieldSpec('a'),
                        FieldSpec('b'),
                        FieldSpec('c', default=lambda : 5),
                        FieldSpec('d', 
                                  computer=Computer(twice, depends=('c',)),
                                  editable=Computer(gt5, depends=('a',))),
                        FieldSpec('sum', type_=pytis.data.Integer(),
                                  computer=Computer(sum, depends=('b','c'))),
                        FieldSpec('inc', type_=pytis.data.Integer(),
                                  computer=Computer(inc, depends=('sum',))))
    def test_init(self):
        row = PresentedRow(self._fields, self._data, None)
        assert row['a'].value() == None
        assert row['b'].value() == None
        assert row['c'].value() == 5
        assert row['d'].value() == 10
        data_row = pytis.data.Row((
            ('a', pytis.data.Value(pytis.data.Integer(), 'xx')),
            ('b', pytis.data.Value(pytis.data.Integer(), 100)),
            ('c', pytis.data.Value(pytis.data.Integer(), 77)),
            ('d', pytis.data.Value(pytis.data.Integer(), 18))))
        row = PresentedRow(self._fields, self._data, data_row)
        assert row['a'].value() == 'xx'
        assert row['b'].value() == 100
        assert row['c'].value() == 77
        assert row['d'].value() == 18
        row['c'] = pytis.data.Value(pytis.data.Integer(), 88)
        row2 = PresentedRow(self._fields, self._data, row)
        assert row['a'].value() == 'xx'
        assert row['b'].value() == 100
        assert row['c'].value() == 88
        assert row['d'].value() == 176
        # TODO: dodělat
    def test_prefill(self):
        row = PresentedRow(self._fields, self._data, None,
                           prefill={'a': 'xx', 'b': 3, 'c': 99, 'd': 77})
        assert row['a'].value() == 'xx'
        assert row['b'].value() == 3
        assert row['c'].value() == 99
        assert row['d'].value() == 77
    def test_computer(self):
        row = PresentedRow(self._fields, self._data, None,
                                 prefill={'b': 3})
        assert row['d'].value() == 10
        assert row['sum'].value() == 8
        assert row['inc'].value() == 9
        row['c'] = pytis.data.Value(pytis.data.Integer(), 100)
        assert row['d'].value() == 200
        assert row['sum'].value() == 103
        assert row['inc'].value() == 104
    def test_callback(self):
        changed = []
        def cb(id):
            changed.append(id)
        row = PresentedRow(self._fields, self._data, None,
                           prefill={'b': 3}, change_callback=cb)
        assert row['d'].value() == 10
        assert row['sum'].value() == 8
        assert row['inc'].value() == 9
        assert 'd' in changed and 'sum' in changed and 'inc' in changed
        del changed[0:len(changed)]
        row['c'] = pytis.data.Value(pytis.data.Integer(), 100)
        assert row['d'].value() == 200
        assert row['sum'].value() == 103
        assert row['inc'].value() == 104
        assert 'd' in changed and 'sum' in changed and 'inc' in changed
    def test_editable(self):
        row = PresentedRow(self._fields, self._data, None,
                                 prefill={'a': 3, 'b': 3})
        assert row.editable('a')
        assert not row.editable('d')
        row['a'] = pytis.data.Value(pytis.data.Integer(), 8)
        row['a'] = pytis.data.Value(pytis.data.Integer(), 10)
        assert row.editable('d')
    def test_editability_callbacks(self):
        enabled = ['--'] # we need a mutable object...
        def editability_change(id, editable):
            enabled[0] = editable and 'yes' or 'no'
        row = PresentedRow(self._fields, self._data, None,
                                 prefill={'a': 6},
                                 editability_change_callback=editability_change)
        assert enabled[0] == '--'
        row['a'] = pytis.data.Value(pytis.data.Integer(), 7)
        assert enabled[0] == '--'
        row['a'] = pytis.data.Value(pytis.data.Integer(), 3)
        assert enabled[0] == 'no'
        row['a'] = pytis.data.Value(pytis.data.Integer(), 8)
        assert enabled[0] == 'yes'
        
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
    def test_keys(self):
        row = PresentedRow(self._fields, self._data, None)
        assert row.keys().sort() == map(lambda f: f.id(), self._fields).sort()
        
tests.add(PresentedRow_)

def get_tests():
    return tests

if __name__ == '__main__':
    unittest.main() #defaultTest='get_tests')
