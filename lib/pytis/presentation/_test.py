#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (C) 2001-2015, 2017 Brailcom, o.p.s.
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
        self.longMessage = True
        key = pd.ColumnSpec('a', pd.Integer())
        self._columns = (
            key,
            pd.ColumnSpec('b', pd.Integer()),
            pd.ColumnSpec('c', pd.Integer()),
            pd.ColumnSpec('d', pd.Integer()),
            pd.ColumnSpec('r', pd.IntegerRange()))
        self._data = pd.Data(self._columns, key)
        @pp.row_function
        def twice(row, c):
            # Just try if it is possible to access the original row.
            row.original_row()['c'].value()
            return c is not None and c * 2 or None
        @pp.row_function
        def total(row, b, c):
            if b is None or c is None:
                return 0
            return b + c
        @pp.row_function
        def inc(row, total):
            return total is not None and total + 1 or None
        @pp.row_function
        def gt5(row, total):
            return total > 5
        self._fields = (
            pp.Field('a'),
            pp.Field('b'),
            pp.Field('c', default=lambda: 5),
            pp.Field('d', editable=pp.Computer(gt5, depends=('total',)),
                     computer=pp.Computer(twice, depends=('c',))),
            pp.Field('e', type=pd.Integer(), virtual=True,
                     default=88),
            pp.Field('total', type=pd.Integer(), virtual=True, editable=pp.Editable.NEVER,
                     computer=pp.Computer(total, depends=('b', 'c'))),
            pp.Field('inc', type=pd.Integer(), virtual=True, editable=pp.Editable.NEVER,
                     computer=pp.Computer(inc, depends=('total',))),
            pp.Field('r'),
        )

    def _data_row(self, **values):
        return pd.Row([(c.id(), pd.Value(c.type(), values.get(c.id())))
                       for c in self._columns])

    def _row(self, new=False, row=None, singleline=False, **prefill):
        return pp.PresentedRow(self._fields, self._data, row=row, new=new,
                               singleline=singleline, prefill=prefill)

    def _check_values(self, row, **kwargs):
        for key, value in kwargs.items():
            rvalue = row[key].value()
            if isinstance(row[key].type(), pd.Range) and rvalue is not None:
                rvalue = tuple(rvalue)
            self.assertEqual(rvalue, value, key)

    def test_init(self):
        row = self._row(new=True)
        self._check_values(row, a=None, b=None, c=5, d=10, e=88, total=0, r=None)
        row = self._row()
        self._check_values(row, a=None, b=None, c=None, d=None, total=None)
        data_row = self._data_row(a=4, b=100, c=77, d=18, r=(1, 8))
        row = self._row(row=data_row, new=True)
        self._check_values(row, a=4, b=100, c=77, d=18, e=88, total=177, r=(1, 8))
        row = self._row(row=data_row)
        self._check_values(row, a=4, b=100, c=77, d=18, e=None, total=177)
        row['c'] = pd.ival(88)
        row['r'] = pd.Value(pd.IntegerRange(), (8, 9))
        self._check_values(row, a=4, b=100, c=88, d=176, total=188)
        # TODO: dodělat

    def test_prefill(self):
        row = self._row(new=True, a=1, b=3, d=77)
        self._check_values(row, a=1, b=3, c=5, d=77, e=88)

    def test_prefill_default(self):
        row = self._row(new=True, b=22, c=33, d=44, e=55)
        self._check_values(row, b=22, c=33, d=44, e=55)

    def test_computer(self):
        row = self._row(new=True, b=3)
        self.assertIsNone(row.get('total', lazy=True).value())
        self.assertEqual(row['total'].value(), 8)
        self.assertEqual(row.get('total', lazy=True).value(), 8)
        self._check_values(row, d=10, total=8, inc=9)
        row['c'] = pd.ival(100)
        self._check_values(row, d=200, total=103, inc=104)

    def test_prefill_computer(self):
        row = self._row(new=True, b=2, c=2, total=88)
        self._check_values(row, b=2, c=2, total=88, inc=89)

    def test_validation(self):
        row = self._row()
        self.assertIsNone(row.validate('a', '2'))
        self.assertIsNotNone(row.validate('b', '2.3'))
        self.assertIsNone(row.validate('c', '8'))
        self.assertIsNone(row.validate('d', '12'))
        self._check_values(row, a=2, b=None, c=8, d=12, total=0)
        self.assertIsNone(row.invalid_string('a'))
        self.assertEqual(row.invalid_string('b'), '2.3')
        self.assertIsNone(row.validate('b', '12'))
        self.assertIsNone(row.invalid_string('b'))
        self.assertIsNone(row.validate('r', ('2', '12')))
        self._check_values(row, b=12, c=8, total=20, r=(2, 12))
        self.assertIsNotNone(row.validate('r', ('2', 'x12')))
        self._check_values(row, r=(2, 12))
        self.assertEqual(row.invalid_string('r'), ('2', 'x12'))

    def test_set_row(self):
        row = self._row(new=True)
        self._check_values(row, a=None, b=None, c=5, total=0, inc=1)
        row.set_row(self._data_row(b=10, c=20))
        self._check_values(row, a=None, b=10, c=20, total=30, inc=31)
        row.set_row(None)
        self._check_values(row, a=None, b=None, c=5, total=0, inc=1)
        row = self._row()
        self._check_values(row, a=None, b=None, c=None, total=None, inc=None)
        row.set_row(self._data_row(b=10, c=20))
        self._check_values(row, a=None, b=10, c=20, total=30, inc=31)
        row.set_row(None)
        self._check_values(row, a=None, b=None, c=None, total=None, inc=None)

    def test_editable(self):
        row = self._row(b=2, c=1)
        self.assertTrue(row.editable('a'))
        self.assertFalse(row.editable('d'))
        row['b'] = pd.ival(5)
        self.assertTrue(row.editable('d'))

    def test_callback(self):
        row = self._row(new=True, b=3)
        changed = []
        def callback(id):
            def cb():
                row[id].value()
                changed.append(id)
            return cb
        for id in ('a', 'b', 'c', 'd', 'total', 'inc'):
            row.register_callback(row.CALL_CHANGE, id, callback(id))
        # self._check_values(row, d=10, total=8, inc=9)
        # assert 'd' in changed and 'total' in changed and 'inc' in changed, changed
        # del changed[0:len(changed)]
        row['c'] = pd.ival(100)
        # self._check_values(row, d=200, total=103, inc=104)
        self.assertIn('d', changed)
        self.assertIn('total', changed)
        self.assertIn('inc', changed)
        del changed[0:len(changed)]
        row.set_row(self._data_row(a=1, b=10, c=20, d=30))
        self.assertIn('a', changed)
        self.assertIn('b', changed)
        self.assertIn('c', changed)
        self.assertIn('d', changed)
        self.assertIn('total', changed)
        self.assertIn('inc', changed)

    def test_editability_callbacks(self):
        enabled = [None] # we need a mutable object...
        row = self._row(a=6)
        def callback():
            enabled[0] = row.editable('d')
        row.register_callback(row.CALL_EDITABILITY_CHANGE, 'd', callback)
        self.assertIsNone(enabled[0])
        row['a'] = pd.ival(8)
        self.assertIsNone(enabled[0])
        row['c'] = pd.ival(3)
        self.assertFalse(enabled[0])
        row['b'] = pd.ival(2)
        self.assertFalse(enabled[0])
        row['b'] = pd.ival(3)
        self.assertTrue(enabled[0])
        row['c'] = pd.ival(2)
        self.assertFalse(enabled[0])
    def test_has_key(self):
        row = self._row()
        self.assertIn('a', row)
        self.assertIn('inc', row)
        self.assertNotIn('blabla', row)
    def test_changed(self):
        row = self._row()
        self.assertFalse(row.changed())
        row['b'] = pd.ival(333)
        self.assertTrue(row.changed())
    def test_field_changed(self):
        row = self._row(b=3, c=8)
        self.assertFalse(row.field_changed('a'))
        self.assertFalse(row.field_changed('b'))
        self.assertFalse(row.field_changed('c'))
        row['b'] = pd.ival(333)
        self.assertFalse(row.field_changed('a'))
        self.assertTrue(row.field_changed('b'))
        self.assertFalse(row.field_changed('c'))
    def test_keys(self):
        row = self._row()
        self.assertItemsEqual(row.keys(), map(lambda f: f.id(), self._fields))
    def test_format(self):
        row = self._row(singleline=True, r=(8, 9))
        r1 = row.format('r')
        r2 = row.format('r', single=False)
        self.assertEqual(r1, u'8 — 9')
        self.assertEqual(r2, ('8', '9'))
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
        row = pp.PresentedRow(fields, data, None, prefill=dict(b='2', c='3', d='1'))
        self.assertEqual(row.display('a'), '')
        self.assertEqual(row.display('b'), 'SECOND')
        self.assertEqual(row.display('c'), '-3-')
        self.assertEqual(row.display('d'), 'first')
    def test_depends(self):
        row = self._row()
        any = ('a', 'b', 'c', 'd', 'e', 'total', 'inc')
        self.assertFalse(row.depends('a', any))
        self.assertFalse(row.depends('b', ('a', 'b', 'c')))
        self.assertTrue(row.depends('b', ('d',)))
        self.assertTrue(row.depends('b', ('a', 'b', 'c', 'd')))
        self.assertTrue(row.depends('c', ('d',)))
        self.assertFalse(row.depends('d', any))
        self.assertFalse(row.depends('e', any))
        self.assertTrue(row.depends('total', ('inc', 'd')))
        self.assertFalse(row.depends('total', ('a', 'b', 'c', 'e', 'total')))
        self.assertFalse(row.depends('inc', any))

tests.add(PresentedRow)


class PrettyTypes(unittest.TestCase):
    class CustomFoldable(pp.PrettyFoldable, pd.String):
        def _init(self, **kwargs):
            super(PrettyTypes.CustomFoldable, self)._init(tree_column_id='tree_order',
                                                          subcount_column_id='tree_nsub',
                                                          **kwargs)
    def test_instance(self):
        t = PrettyTypes.CustomFoldable(maxlen=5)
        self.assertEqual(t.maxlen(), 5)
        self.assertEqual(t.tree_column_id(), 'tree_order')

tests.add(PrettyTypes)

class DocTest(unittest.TestCase):
    def test_field_computations(self):
        import doctest
        doctest.testfile('../../../doc/tutorials/Fields.txt')


def get_tests():
    return tests

if __name__ == '__main__': # pragma: no cover
    unittest.main() #defaultTest='get_tests')
