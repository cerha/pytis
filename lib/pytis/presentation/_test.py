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
        self._columns = (
            pd.ColumnSpec('a', pd.Integer(not_null=True)),
            pd.ColumnSpec('b', pd.Integer(not_null=True,
                                          enumerator=pd.FixedEnumerator(range(101))),),
            pd.ColumnSpec('c', pd.Integer(not_null=True)),
            pd.ColumnSpec('d', pd.Integer()),
            pd.ColumnSpec('r', pd.IntegerRange()))
        self._data = pd.Data(self._columns, self._columns[0])
        @pp.computer
        def twice(row, c):
            return c * 2
        @pp.computer(fallback=0)
        def total(row, b, c):
            return b + c
        @pp.computer
        def inc(row, total):
            return total is not None and total + 1 or None
        @pp.computer
        def gt5(row, total):
            return total > 5
        def visible(row, a):
            return a != 0
        self._fields = (
            pp.Field('a'),
            pp.Field('b', runtime_filter=pp.computer(lambda r, a: lambda x: x % a == 0)),
            pp.Field('c', default=lambda: 5),
            pp.Field('d', editable=gt5, computer=twice),
            pp.Field('e', type=pd.Integer(), virtual=True, default=88),
            pp.Field('total', type=pd.Integer(), virtual=True, editable=pp.Editable.NEVER,
                     computer=total),
            pp.Field('inc', type=pd.Integer(), virtual=True, editable=pp.Editable.NEVER,
                     computer=inc),
            pp.Field('r', visible=pp.computer(visible)),#lambda r, a: a != 0)),
        )

    def _data_row(self, **values):
        return pd.Row([(c.id(), pd.Value(c.type(), values.get(c.id())))
                       for c in self._columns])

    def _row(self, new=False, row=None, singleline=False, **prefill):
        return pp.PresentedRow(self._fields, self._data, row=row, new=new,
                               singleline=singleline, prefill=prefill)

    def _set(self, row, **kwargs):
        for key, value in kwargs.items():
            row[key] = value

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

    def test_unicode(self):
        row = self._row(new=True, a=1, b=3, d=77)
        self.assertEqual(unicode(row),
                         '<PresentedRow: a=1, b=3, c=5, d=77, e=88, total=8, inc=9, r=None>')

    def test_prefill(self):
        row = self._row(new=True, a=1, b=pd.ival(3), d=77)
        self._check_values(row, a=1, b=3, c=5, d=77, e=88)

    def test_prefill_default(self):
        row = self._row(new=True, b=22, c=33, d=44, e=55)
        self._check_values(row, b=22, c=33, d=44, e=55)

    def test_setitem(self):
        row = self._row()
        self._check_values(row, a=None, b=None, c=None, d=None)
        row['a'] = 5
        self._check_values(row, a=5, b=None, c=None, d=None)
        row['b'] = 10
        self._check_values(row, a=5, b=10, c=None, d=None)
        row['c'] = 20
        self._check_values(row, a=5, b=10, c=20, d=40, total=30)
        row['d'] = 30
        self._check_values(row, a=5, b=10, c=20, d=30, total=30)
        row['total'] = 3
        self._check_values(row, a=5, b=10, c=20, d=30, total=3)
        def assign_invalid():
            row['c'] = 'x'
        self.assertRaises(TypeError, assign_invalid)
        def assign_invalid_value():
            row['c'] = pd.sval('x')
        self.assertRaises(TypeError, assign_invalid_value)

    def test_computer(self):
        row = self._row(new=True, b=3)
        self.assertIsNone(row.get('total', lazy=True).value())
        self._check_values(row, d=10, total=8, inc=9)
        self._set(row, c=100)
        self._check_values(row, d=200, total=103, inc=104)
        self._set(row, a=4, b=100, c=88, r=(8, 9))
        self.assertEqual(row.get('total', lazy=True).value(), 103)
        self._check_values(row, a=4, b=100, c=88, total=188)
        self._set(row, b=None)
        self._check_values(row, total=0, d=176)
        self._set(row, c=None)
        self._check_values(row, total=0, d=176)
        self._set(row, c=2)
        self._check_values(row, total=0, d=4)
        self._set(row, b=1)
        self._check_values(row, total=3, d=4)

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
        self._set(row, b=5)
        self.assertTrue(row.editable('d'))

    def test_visible(self):
        row = self._row(a=0, new=True)
        self.assertTrue(row.visible('a'))
        self.assertFalse(row.visible('r'))
        row['a'] = 1
        self.assertTrue(row.visible('r'))

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
        self._set(row, c=100)
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
        self._set(row, a=8)
        self.assertIsNone(enabled[0])
        self._set(row, c=3)
        self.assertFalse(enabled[0])
        self._set(row, b=2)
        self.assertFalse(enabled[0])
        self._set(row, b=3)
        self.assertEqual(row['total'].value(), 6)
        self.assertTrue(enabled[0])
        self._set(row, c=2)
        self.assertEqual(row['total'].value(), 5)
        self.assertFalse(enabled[0])

    def test_runtime_filter(self):
        def enum(row, key):
            return tuple(x for x, display in row.enumerate('b'))
        row = self._row(a=20, b=0, c=5)
        self.assertEqual(enum(row, 'b'), (0, 20, 40, 60, 80, 100))

    def test_enumeration_callbacks(self):
        pass

    def test_has_key(self):
        row = self._row()
        self.assertIn('a', row)
        self.assertIn('inc', row)
        self.assertNotIn('blabla', row)

    def test_changed(self):
        row = self._row()
        self.assertFalse(row.changed())
        self._set(row, b=333)
        self.assertTrue(row.changed())

    def test_field_changed(self):
        row = self._row(b=3, c=8)
        self.assertFalse(row.field_changed('a'))
        self.assertFalse(row.field_changed('b'))
        self.assertFalse(row.field_changed('c'))
        self._set(row, b=7)
        self.assertFalse(row.field_changed('a'))
        self.assertTrue(row.field_changed('b'))
        self.assertFalse(row.field_changed('c'))
        self.assertIsNotNone(row.validate('a', '3.4'))
        self.assertTrue(row.field_changed('a'))
        self.assertIsNotNone(row.validate('a', ''))
        self.assertFalse(row.field_changed('a'))

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
        enumerator = pd.DataEnumerator(pd.DataFactory(
            pd.MemData,
            [pd.ColumnSpec(c, pd.String()) for c in ('x', 'y', 'z')],
            data=[pd.Row(zip(('x', 'y', 'z'), [pd.sval(x) for x in values]))
                  for values in (('1', 'FIRST', 'A'), ('2', 'SECOND', 'B'), ('3', 'THIRD', 'C'))],
        ))
        columns = (
            pd.ColumnSpec('a', pd.Integer()),
            pd.ColumnSpec('b', pd.String(enumerator=enumerator)),
            pd.ColumnSpec('c', pd.String(enumerator=enumerator)),
            pd.ColumnSpec('d', pd.String(enumerator=enumerator)),
        )
        fields = (
            Field('a'),
            Field('b', display='y'),
            Field('c', display=lambda x: '-' + x + '-'),
            Field('d', display=lambda row: row['y'].value().lower()),
            Field('e', virtual=True, computer=pp.CbComputer('b', 'z')),
        )
        row = pp.PresentedRow(fields, pd.Data(columns, columns[0]), None, new=True,
                              prefill=dict(b='2', c='3', d='1'))
        self.assertEqual(row.display('a'), '')
        self.assertEqual(row.display('b'), 'SECOND')
        self.assertEqual(row.display('c'), '-3-')
        self.assertEqual(row.display('d'), 'first')
        self.assertEqual(row['e'].value(), 'B')

    def test_depends(self):
        row = self._row()
        self.assertFalse(row.depends('a', (x for x in row.keys() if x != 'b')))
        self.assertFalse(row.depends('b', ('a', 'b', 'c')))
        self.assertFalse(row.depends('c', ('b',)))
        self.assertFalse(row.depends('b', ('c',)))
        self.assertTrue(row.depends('b', ('d',)))
        self.assertTrue(row.depends('b', ('a', 'b', 'c', 'd')))
        self.assertTrue(row.depends('c', ('d',)))
        self.assertFalse(row.depends('d', row.keys()))
        self.assertFalse(row.depends('e', row.keys()))
        self.assertTrue(row.depends('total', ('inc', 'd')))
        self.assertFalse(row.depends('total', ('a', 'b', 'c', 'e', 'total')))
        self.assertFalse(row.depends('inc', any))

    def test_filename(self):
        row = pp.PresentedRow((
            Field('a', default=1),
            Field('x', virtual=True, default='aaa'),
            Field('y', virtual=True, filename='x'),
            Field('z', virtual=True, filename=lambda r: 'file%d.pdf' % r['a'].value()),
        ), self._data, None, new=True)
        self.assertEqual(row.filename('x'), None)
        self.assertEqual(row.filename('y'), 'aaa')
        self.assertEqual(row.filename('z'), 'file1.pdf')



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
