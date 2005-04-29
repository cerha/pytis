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
import pytis.util.test
import pytis.data

import field

tests = pytis.util.test.TestSuite()

############
# field.py #
############

class PresentedRow(unittest.TestCase):
    def setUp(self):
        key = pytis.data.ColumnSpec('a', pytis.data.String.make())
        columns = (
            key,
            pytis.data.ColumnSpec('b', pytis.data.Integer.make()),
            pytis.data.ColumnSpec('c', pytis.data.Integer.make()),
            pytis.data.ColumnSpec('d', pytis.data.Integer.make()))
        self._data = pytis.data.Data(columns, key)
        def twice(row):
            return row['c'].value() * 2
        def sum(row):
            return row['b'].value() + row['c'].value()
        def inc(row):
            return row['sum'].value() + 1
        def gt5(row):
            return row['a'].value() > 5
        Field = field.FieldSpec
        Comp = field.Computer
        self._fields = (Field('a'),
                        Field('b'),
                        Field('c', default=lambda : 5),
                        Field('d', 
                              computer=Comp(twice, depends=('c',)),
                              editable=Comp(gt5, depends=('a',))),
                        Field('sum', type_=pytis.data.Integer.make(),
                              computer=Comp(sum, depends=('b','c'))),
                        Field('inc', type_=pytis.data.Integer.make(),
                              computer=Comp(inc, depends=('sum',))))
    def test_init(self):
        row = field.PresentedRow(self._fields, self._data, None)
        assert row['a'].value() == None
        assert row['b'].value() == None
        assert row['c'].value() == 5
        assert row['d'].value() == 10
        data_row = pytis.data.Row((
            ('a', pytis.data.Value(pytis.data.Integer.make(), 'xx')),
            ('b', pytis.data.Value(pytis.data.Integer.make(), 100)),
            ('c', pytis.data.Value(pytis.data.Integer.make(), 77)),
            ('d', pytis.data.Value(pytis.data.Integer.make(), 18))))
        row = field.PresentedRow(self._fields, self._data, data_row)
        assert row['a'].value() == 'xx'
        assert row['b'].value() == 100
        assert row['c'].value() == 77
        assert row['d'].value() == 18
        row['c'] = pytis.data.Value(pytis.data.Integer.make(), 88)
        row2 = field.PresentedRow(self._fields, self._data, row)
        assert row['a'].value() == 'xx'
        assert row['b'].value() == 100
        assert row['c'].value() == 88
        assert row['d'].value() == 176
        # TODO: dodělat
    def test_prefill(self):
        row = field.PresentedRow(self._fields, self._data, None,
                                 prefill={'a': 'xx', 'b': 3, 'c': 99, 'd': 77})
        assert row['a'].value() == 'xx'
        assert row['b'].value() == 3
        assert row['c'].value() == 99
        assert row['d'].value() == 77
    def test_computer(self):
        row = field.PresentedRow(self._fields, self._data, None,
                                 prefill={'b': 3})
        assert row['d'].value() == 10
        assert row['sum'].value() == 8
        assert row['inc'].value() == 9
        row['c'] = pytis.data.Value(pytis.data.Integer.make(), 100)
        assert row['d'].value() == 200
        assert row['sum'].value() == 103
        assert row['inc'].value() == 104
    def test_callback(self):
        changed = []
        def cb(id):
            changed.append(id)
        row = field.PresentedRow(self._fields, self._data, None,
                                 prefill={'b': 3}, change_callback=cb)
        assert row['d'].value() == 10
        assert row['sum'].value() == 8
        assert row['inc'].value() == 9
        assert 'd' in changed and 'sum' in changed and 'inc' in changed
        del changed[0:len(changed)]
        row['c'] = pytis.data.Value(pytis.data.Integer.make(), 100)
        assert row['d'].value() == 200
        assert row['sum'].value() == 103
        assert row['inc'].value() == 104
        assert 'd' in changed and 'sum' in changed and 'inc' in changed
    def test_editable(self):
        row = field.PresentedRow(self._fields, self._data, None,
                                 prefill={'a': 3, 'b': 3})
        assert row.editable('a')
        assert not row.editable('d')
        row['a'] = pytis.data.Value(pytis.data.Integer.make(), 8)
        row['a'] = pytis.data.Value(pytis.data.Integer.make(), 10)
        assert row.editable('d')
    def test_editability_callbacks(self):
        enabled = ['--'] # we need a mutable object...
        def enable(id):
            enabled[0] = 'yes'
        def disable(id):
            enabled[0] = 'no'
        row = field.PresentedRow(self._fields, self._data, None,
                                 prefill={'a': 6},
                                 enable_field_callback=enable,
                                 disable_field_callback=disable)
        assert enabled[0] == '--'
        row['a'] = pytis.data.Value(pytis.data.Integer.make(), 7)
        assert enabled[0] == '--'
        row['a'] = pytis.data.Value(pytis.data.Integer.make(), 3)
        assert enabled[0] == 'no'
        row['a'] = pytis.data.Value(pytis.data.Integer.make(), 8)
        assert enabled[0] == 'yes'
        
    def test_has_key(self):
        row = field.PresentedRow(self._fields, self._data, None)
        assert row.has_key('a')
        assert row.has_key('inc')
        assert not row.has_key('blabla')
    def test_changed(self):
        row = field.PresentedRow(self._fields, self._data, None)
        assert not row.changed()
        row['b'] = pytis.data.Value(pytis.data.Integer.make(), 333)
        assert row.changed()
    def test_keys(self):
        row = field.PresentedRow(self._fields, self._data, None)
        assert row.keys().sort() == map(lambda f: f.id(), self._fields).sort()
        
tests.add(PresentedRow)

def get_tests():
    return tests

if __name__ == '__main__':
    unittest.main() #defaultTest='get_tests')
