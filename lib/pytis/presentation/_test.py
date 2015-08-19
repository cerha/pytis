#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (C) 2001-2015 Brailcom, o.p.s.
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

import pytis.data as pd
import pytis.presentation as pp
from pytis.form import *

tests = pytis.util.test.TestSuite()

############
# field.py #
############

class PresentedRow(unittest.TestCase):
    def setUp(self):
        key = pd.ColumnSpec('a', pd.Integer())
        self._columns = (
            key,
            pd.ColumnSpec('b', pd.Integer()),
            pd.ColumnSpec('c', pd.Integer()),
            pd.ColumnSpec('d', pd.Integer()),
            pd.ColumnSpec('r', pd.IntegerRange()))
        self._data = pd.Data(self._columns, key)
        def twice(row):
            # Just try if it is possible to access the original row.
            row.original_row()['c'].value()
            c = row['c'].value()
            return c is not None and c * 2 or None
        def sum(row):
            b, c = (row['b'].value(), row['c'].value())
            if b is None or c is None:
                return 0
            return b + c
        def inc(row):
            sum = row['sum'].value()
            return sum is not None and sum + 1 or None
        def gt5(row):
            return row['sum'].value() > 5
        self._fields = (
            pp.Field('a'),
            pp.Field('b'),
            pp.Field('c', default=lambda: 5),
            pp.Field('d', editable=pp.Computer(gt5, depends=('sum',)),
                     computer=pp.Computer(twice, depends=('c',))),
            pp.Field('e', type=pd.Integer(), virtual=True,
                     default=88),
            pp.Field('sum', type=pd.Integer(), virtual=True,
                     computer=pp.Computer(sum, depends=('b', 'c'))),
            pp.Field('inc', type=pd.Integer(), virtual=True,
                     computer=pp.Computer(inc, depends=('sum',))),
            pp.Field('r'),
        )
        
    def _check_values(self, row, pairs):
        for k, v in pairs:
            row_value = row[k].value()
            if isinstance(v, tuple):
                for v1, v2 in zip(v, row_value):
                    assert v1 == v2, (k, v, row_value)
            else:
                assert row_value == v, (k, v, row_value)
    def _value(self, key, value):
        col = find(key, self._columns, key=lambda c: c.id())
        return pd.Value(col.type(), value)
    def _data_row(self, **values):
        return pd.Row([(c.id(), pd.Value(c.type(), values.get(c.id())))
                       for c in self._columns])
    def test_init(self):
        row = pp.PresentedRow(self._fields, self._data, None, new=True)
        self._check_values(row, (('a', None),
                                 ('b', None),
                                 ('c', 5),
                                 ('d', 10),
                                 ('e', 88),
                                 ('sum', 0),
                                 ('r', None)))
        row = pp.PresentedRow(self._fields, self._data, None, new=False)
        self._check_values(row, (('a', None),
                                 ('b', None),
                                 ('c', None),
                                 ('d', None),
                                 ('sum', None)))
        data_row = self._data_row(a=4, b=100, c=77, d=18, r=(1, 8))
        row = pp.PresentedRow(self._fields, self._data, data_row, new=True)
        self._check_values(row, (('a', 4),
                                 ('b', 100),
                                 ('c', 77),
                                 ('d', 18),
                                 ('e', 88),
                                 ('sum', 177),
                                 ('r', (1, 8))))
        row = pp.PresentedRow(self._fields, self._data, data_row, new=False)
        self._check_values(row, (('a', 4),
                                 ('b', 100),
                                 ('c', 77),
                                 ('d', 18),
                                 ('e', None),
                                 ('sum', 177)))
        row['c'] = self._value('c', 88)
        row['r'] = self._value('r', (8, 9))
        self._check_values(row, (('a', 4),
                                 ('b', 100),
                                 ('c', 88),
                                 ('d', 176),
                                 ('sum', 188)))
        # TODO: dodělat
    def test_prefill(self):
        row = pp.PresentedRow(self._fields, self._data, None, new=True,
                           prefill={'a': 1, 'b': 3, 'd': 77})
        self._check_values(row, (('a', 1),
                                 ('b', 3),
                                 ('c', 5),
                                 ('d', 77),
                                 ('e', 88)))
    def test_prefill_default(self):
        row = pp.PresentedRow(self._fields, self._data, None, new=True,
                           prefill={'b': 22, 'c': 33, 'd': 44, 'e': 55})
        self._check_values(row, (('b', 22),
                                 ('c', 33),
                                 ('d', 44),
                                 ('e', 55)))
    def test_computer(self):
        row = pp.PresentedRow(self._fields, self._data, None, new=True,
                           prefill={'b': 3})
        assert row.get('sum', lazy=True).value() is None
        assert row['sum'].value() == 8
        assert row.get('sum', lazy=True).value() == 8
        self._check_values(row, (('d', 10), ('sum', 8), ('inc', 9)))
        row['c'] = self._value('c', 100)
        self._check_values(row, (('d', 200), ('sum', 103), ('inc', 104)))
    def test_prefill_computer(self):
        row = pp.PresentedRow(self._fields, self._data, None, new=True,
                           prefill={'b': 2, 'c': 2, 'sum': 88})
        self._check_values(row, (('b', 2),
                                 ('c', 2),
                                 ('sum', 88),
                                 ('inc', 89)))
    def test_validation(self):
        row = pp.PresentedRow(self._fields, self._data, None)
        assert row.validate('a', '2') is None
        assert row.validate('b', '2.3') is not None
        assert row.validate('c', '8') is None
        assert row.validate('d', '12') is None
        self._check_values(row, (('a', 2),
                                 ('b', None),
                                 ('c', 8),
                                 ('d', 12),
                                 ('sum', 0)))
        assert row.invalid_string('a') is None
        assert row.invalid_string('b') == '2.3'
        assert row.validate('b', '12') is None
        assert row.invalid_string('b') is None
        assert row.validate('r', ('2', '12')) is None
        self._check_values(row, (('b', 12),
                                 ('c', 8),
                                 ('sum', 20),
                                 ('r', (2, 12))))
        assert row.validate('r', ('2', 'x12')) is not None
        self._check_values(row, (('r', (2, 12)),))
        assert row.invalid_string('r') == ('2', 'x12')
    def test_set_row(self):
        row = pp.PresentedRow(self._fields, self._data, None, new=True)
        self._check_values(row, (('a', None),
                                 ('b', None),
                                 ('c', 5),
                                 ('sum', 0),
                                 ('inc', 1)))
        row.set_row(self._data_row(b=10, c=20))
        self._check_values(row, (('a', None),
                                 ('b', 10),
                                 ('c', 20),
                                 ('sum', 30),
                                 ('inc', 31)))
        row.set_row(None)
        self._check_values(row, (('a', None),
                                 ('b', None),
                                 ('c', 5),
                                 ('sum', 0),
                                 ('inc', 1)))
        row = pp.PresentedRow(self._fields, self._data, None, new=False)
        self._check_values(row, (('a', None),
                                 ('b', None),
                                 ('c', None),
                                 ('sum', None),
                                 ('inc', None)))
        row.set_row(self._data_row(b=10, c=20))
        self._check_values(row, (('a', None),
                                 ('b', 10),
                                 ('c', 20),
                                 ('sum', 30),
                                 ('inc', 31)))
        row.set_row(None)
        self._check_values(row, (('a', None),
                                 ('b', None),
                                 ('c', None),
                                 ('sum', None),
                                 ('inc', None)))

    def test_editable(self):
        row = pp.PresentedRow(self._fields, self._data, None,
                           prefill={'b': 2, 'c': 1})
        assert row.editable('a')
        assert not row.editable('d')
        row['b'] = self._value('b', 5)
        assert row.editable('d')
    def test_callback(self):
        row = pp.PresentedRow(self._fields, self._data, None, new=True, prefill={'b': 3})
        changed = []
        def callback(id):
            def cb():
                row[id].value()
                changed.append(id)
            return cb
        for id in ('a', 'b', 'c', 'd', 'sum', 'inc'):
            row.register_callback(row.CALL_CHANGE, id, callback(id))
        #self._check_values(row, (('d', 10), ('sum', 8), ('inc', 9)))
        #assert 'd' in changed and 'sum' in changed and 'inc' in changed, changed
        #del changed[0:len(changed)]
        row['c'] = self._value('c', 100)
        #self._check_values(row, (('d', 200), ('sum', 103), ('inc', 104)))
        assert 'd' in changed and 'sum' in changed and 'inc' in changed, changed
        del changed[0:len(changed)]
        row.set_row(self._data_row(a=1, b=10, c=20, d=30))
        assert 'a' in changed and 'b' in changed and 'c' in changed and 'd' in changed \
               and 'sum' in changed and 'inc' in changed, changed
        
    def test_editability_callbacks(self):
        enabled = [None] # we need a mutable object...
        row = pp.PresentedRow(self._fields, self._data, None, prefill={'a': 6})
        def callback():
            enabled[0] = row.editable('d')
        row.register_callback(row.CALL_EDITABILITY_CHANGE, 'd', callback)
        assert enabled[0] is None, enabled[0]
        row['a'] = self._value('a', 8)
        assert enabled[0] is None, enabled[0]
        row['c'] = self._value('c', 3)
        assert enabled[0] is False
        row['b'] = self._value('b', 2)
        assert enabled[0] is False
        row['b'] = self._value('b', 3)
        assert enabled[0] is True
        row['c'] = self._value('c', 2)
        assert enabled[0] is False
    def test_has_key(self):
        row = pp.PresentedRow(self._fields, self._data, None)
        assert 'a' in row
        assert 'inc' in row
        assert 'blabla' not in row
    def test_changed(self):
        row = pp.PresentedRow(self._fields, self._data, None)
        assert not row.changed()
        row['b'] = self._value('b', 333)
        assert row.changed()
    def test_field_changed(self):
        row = pp.PresentedRow(self._fields, self._data, None, prefill={'b': 3, 'c': 8})
        assert not row.field_changed('a')
        assert not row.field_changed('b')
        assert not row.field_changed('c')
        row['b'] = self._value('b', 333)
        assert not row.field_changed('a')
        assert row.field_changed('b')
        assert not row.field_changed('c')
    def test_keys(self):
        row = pp.PresentedRow(self._fields, self._data, None)
        assert row.keys().sort() == map(lambda f: f.id(), self._fields).sort()
    def test_format(self):
        row = pp.PresentedRow(self._fields, self._data, None, singleline=True)
        row['r'] = self._value('r', (8, 9))
        r1 = row.format('r')
        r2 = row.format('r', single=False)
        assert r1 == u'8 — 9', r1
        assert r2 == ('8', '9'), r2
    def test_display(self):
        C = pd.ColumnSpec
        S = pd.String
        V = pd.Value
        rows = [pd.Row((('x', V(S(), x)), ('y', V(S(), y))))
                for x, y in (('1', 'FIRST'), ('2', 'SECOND'), ('3', 'THIRD'))]
        edata = pd.DataFactory(pd.MemData, (C('x', S()), C('y', S())), data=rows)
        enum = pd.DataEnumerator(edata)
        key = C('a', pd.Integer())
        columns = (key,
                   C('b', S(enumerator=enum)),
                   C('c', S(enumerator=enum)),
                   C('d', S(enumerator=enum)))
        data = pd.Data(columns, key)
        fields = (Field('a'),
                  Field('b', display='y'),
                  Field('c', display=lambda x: '-' + x + '-'),
                  Field('d', display=lambda row: row['y'].value().lower()),
                  )
        row = pp.PresentedRow(fields, data, None,
                           prefill={'b': '2', 'c': '3', 'd': '1'})
        assert row.display('a') == '', row.display('a')
        assert row.display('b') == 'SECOND', row.display('b')
        assert row.display('c') == '-3-', row.display('c')
        assert row.display('d') == 'first', row.display('d')
    def test_depends(self):
        row = pp.PresentedRow(self._fields, self._data, None)
        any = ('a', 'b', 'c', 'd', 'e', 'sum', 'inc')
        assert not row.depends('a', any)
        assert not row.depends('b', ('a', 'b', 'c'))
        assert row.depends('b', ('d',))
        assert row.depends('b', ('a', 'b', 'c', 'd'))
        assert row.depends('c', ('d',))
        assert not row.depends('d', any)
        assert not row.depends('e', any)
        assert row.depends('sum', ('inc', 'd'))
        assert not row.depends('sum', ('a', 'b', 'c', 'e', 'sum'))
        assert not row.depends('inc', any)
        
tests.add(PresentedRow)


class PrettyTypes(unittest.TestCase):
    class CustomFoldable(pp.PrettyFoldable, pd.String):
        def _init(self, **kwargs):
            super(PrettyTypes.CustomFoldable, self)._init(tree_column_id='tree_order',
                                                          subcount_column_id='tree_nsub',
                                                          **kwargs)
    def test_instance(self):
        t = PrettyTypes.CustomFoldable(maxlen=5)
        assert t.maxlen() == 5
        assert t.tree_column_id() == 'tree_order'

tests.add(PrettyTypes)


def get_tests():
    return tests

if __name__ == '__main__':
    unittest.main() #defaultTest='get_tests')
