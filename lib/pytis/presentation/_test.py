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
import pytis.util
import config

tests = pytis.util.test.TestSuite()

############
# field.py #
############

class PresentedRow(unittest.TestCase):
    def setUp(self):

        class BigString(pd.String, pd.Big):
            pass

        class SpecialEnumerator(pd.FixedEnumerator):
            # This class is overriden just to allow definition of runtime_filter
            # and runtime_arguments for the same field (which is only important
            # to improve test coverage)
            def values(self, a=None):
                # Accepts argument a as returned by runtime_arguments.
                return super(SpecialEnumerator, self).values()

        class TestSpecification(pytis.presentation.Specification):
            data_cls = pd.Data
            def _twice(self, row, c):
                return c * 2
            def _total(self, row, b, c):
                return b + c
            def _half_total(self, row, total):
                return total / 2 if total is not None else None
            def fields(self):
                return (
                    pp.Field('a', type=pd.Integer(not_null=True)),
                    pp.Field('b', type=pd.Integer(not_null=True,
                                                  enumerator=SpecialEnumerator(range(101))),
                             runtime_filter=pp.computer(lambda r, a: lambda x: x % a == 0),
                             runtime_arguments=pp.computer(lambda r, a: dict(a=a))),
                    pp.Field('c', type=pd.Integer(not_null=True), default=lambda: 5),
                    pp.Field('d', type=pd.Integer(),
                             editable=pp.computer(lambda r, total: total > 5),
                             computer=pp.computer(self._twice)),
                    pp.Field('fruit', type=pd.String(), codebook='Fruits', display='title',
                             null_display='none'),
                    pp.Field('fruit_code', virtual=True, computer=pp.CbComputer('fruit', 'code')),
                    pp.Field('range', type=pd.IntegerRange(),
                             visible=pp.computer(lambda r, a: a != 0)),
                    pp.Field('total', type=pd.Integer(), virtual=True,
                             editable=pp.Editable.NEVER,
                             computer=pp.computer(self._total, fallback=0)),
                    pp.Field('half_total', type=pd.Integer(), virtual=True,
                             editable=pp.Editable.NEVER,
                             computer=pp.computer(self._half_total)),
                    pp.Field('x', type=pd.Integer(), virtual=True, default=88),
                    pp.Field('password', type=pd.Password(), virtual=True),
                    pp.Field('big', type=BigString(), virtual=True),
                    pp.Field('array', type=pd.Array, inner_type=pd.String,
                             codebook='Fruits', display='title', virtual=True),
                )

        class Fruits(pytis.presentation.Specification):
            fields = (
                pp.Field('id'),
                pp.Field('title'),
                pp.Field('code', type=pd.Integer()),
                pp.Field('tropical', type=pd.Boolean()),
            )
            data_cls = pd.MemData
            data = (('apl', 'Apple', 123, False),
                    ('ban', 'Banana', 234, True),
                    ('str', 'Strawberry', 234, False),
                    ('org', 'Orange', 456, True))

        class TestResolver(pytis.util.Resolver):
            _specifications = {'TestSpecification': TestSpecification,
                               'Fruits': Fruits}
            def _get_object_by_name(self, name):
                try:
                    return self._specifications[name]
                except KeyError:
                    return super(TestResolver, self)._get_object_by_name(name)

        self.longMessage = True
        config.resolver = resolver = TestResolver()
        self._fields = resolver.get('TestSpecification', 'view_spec').fields()
        self._data = resolver.get('TestSpecification', 'data_spec').create()

    def _data_row(self, **values):
        return pd.Row([(f.id(), pd.Value(f.type(), values.get(f.id())))
                       for f in self._fields if not f.virtual()])

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
        self._check_values(row, a=None, b=None, c=5, d=10, total=0, x=88, range=None)
        row = self._row()
        self._check_values(row, a=None, b=None, c=None, d=None, total=None)
        data_row = self._data_row(a=4, b=100, c=77, d=18, range=(1, 8))
        row = self._row(row=data_row, new=True)
        self._check_values(row, a=4, b=100, c=77, d=18, x=88, total=177, range=(1, 8))
        row = self._row(row=data_row)
        self._check_values(row, a=4, b=100, c=77, d=18, x=None, total=177)

    def test_unicode(self):
        row = self._row(new=True, a=1, b=3, d=77, password='secret', big=1024 * 'x')
        self.assertEqual(unicode(row), ('<PresentedRow: a=1, b=3, c=5, d=77, '
                                        'fruit=None, fruit_code=None, '
                                        'range=None, total=8, half_total=4, x=88, '
                                        'password=***, big=<BigString 1 kB>, '
                                        'array=None>'))
        delattr(row, '_row')
        self.assertRegexpMatches(unicode(row), r'<PresentedRow: [0-9a-h]+>')

    def test_prefill(self):
        row = self._row(new=True, a=1, b=pd.ival(3), d=77)
        self._check_values(row, a=1, b=3, c=5, d=77, x=88)

    def test_new(self):
        row = self._row()
        self.assertFalse(row.new())
        row = self._row(new=True)
        self.assertTrue(row.new())

    def test_key(self):
        row = self._row()
        self.assertEqual(row.key(), (pd.Value(pd.Integer(not_null=True), None),))
        row = self._row(a=1)
        self.assertEqual(row.key(), (pd.Value(pd.Integer(not_null=True), 1),))

    def test_keys(self):
        row = self._row()
        self.assertItemsEqual(row.keys(), map(lambda f: f.id(), self._fields))

    def test_fields(self):
        row = self._row()
        self.assertEqual(row.fields(), self._fields)

    def test_resolver(self):
        row = self._row()
        self.assertEqual(row.resolver(), config.resolver)

    def test_data(self):
        row = self._row()
        self.assertEqual(row.data(), self._data)

    def test_set_transaction(self):
        row = self._row()
        self.assertEqual(row.transaction(), None)
        # TODO: This whole test file tries to avoid using a real database connection,
        # so we can't create a real transaction here.  Using 'x' is invalid for
        # real use but it stil verifies that set_transaction works as long as
        # set_transaction doesn't validate its argument.
        transaction = 'x' #pd.DBTransactionDefault(config.dbconnection)
        row.set_transaction(transaction)
        self.assertEqual(row.transaction(), transaction)

    def test_original_row(self):
        row = self._row(new=True, row=self._data_row(b=6, d=3), b=22, c=33, d=44)
        r1 = row.original_row()
        self._check_values(r1, a=None, b=22, c=33, d=44)
        r2 = row.original_row(initialized=False)
        self._check_values(r2, a=None, b=6, c=None, d=3)

    def test_prefill_default(self):
        row = self._row(new=True, b=22, c=33, d=44, x=55)
        self._check_values(row, b=22, c=33, d=44, x=55)

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
        row['array'] = ('apl', 'str')
        self.assertEqual([v.value() for v in row['array'].value()], ['apl', 'str'])
        def assign_invalid():
            row['c'] = 'x'
        self.assertRaises(TypeError, assign_invalid)
        def assign_invalid_value():
            row['c'] = pd.sval('x')
        self.assertRaises(TypeError, assign_invalid_value)
        def assign_invalid_array():
            row['array'] = (1, 2)
        self.assertRaises(TypeError, assign_invalid_array)


        self.assertRaises(TypeError, assign_invalid_value)
    def test_get(self):
        row = self._row(new=True, a=4)
        self.assertEqual(row.get('a').value(), 4)
        self.assertIsNone(row.get('aaa'))
        self.assertEqual(row.get('aaa', default=10), 10)

    def test_computer(self):
        row = self._row(new=True, b=3)
        self.assertIsNone(row.get('total', lazy=True).value())
        self._check_values(row, d=10, total=8, half_total=4)
        self._set(row, c=100)
        self._check_values(row, d=200, total=103, half_total=51)
        self._set(row, a=4, b=100, c=88, range=(8, 9))
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

    def test_cb_computer(self):
        row = self._row(new=True, fruit='str')
        self.assertTrue(isinstance(row.type('fruit_code'), pytis.data.Integer))
        self.assertEqual(row['fruit_code'].value(), 234)

    def test_prefill_computer(self):
        row = self._row(new=True, b=2, c=2, total=88)
        self._check_values(row, b=2, c=2, total=88, half_total=44)

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
        self.assertIsNone(row.validate('range', ('2', '12')))
        self._check_values(row, b=12, c=8, total=20, range=(2, 12))
        self.assertIsNotNone(row.validate('range', ('2', 'x12')))
        self._check_values(row, range=(2, 12))
        self.assertEqual(row.invalid_string('range'), ('2', 'x12'))

    def test_set_row(self):
        row = self._row(new=True)
        self._check_values(row, a=None, b=None, c=5, total=0, half_total=0)
        row.set_row(self._data_row(b=10, c=20))
        self._check_values(row, a=None, b=10, c=20, total=30, half_total=15)
        row.set_row(None)
        self._check_values(row, a=None, b=None, c=5, total=0, half_total=0)
        row = self._row()
        self._check_values(row, a=None, b=None, c=None, total=None, half_total=None)
        row.set_row(self._data_row(b=10, c=20))
        self._check_values(row, a=None, b=10, c=20, total=30, half_total=15)
        row.set_row(None)
        self._check_values(row, a=None, b=None, c=None, total=None, half_total=None)

    def test_editable(self):
        row = self._row(b=2, c=1)
        self.assertTrue(row.editable('a'))
        self.assertFalse(row.editable('d'))
        self._set(row, b=5)
        self.assertTrue(row.editable('d'))

    def test_visible(self):
        row = self._row(a=0, new=True)
        self.assertTrue(row.visible('a'))
        self.assertFalse(row.visible('range'))
        row['a'] = 1
        self.assertTrue(row.visible('range'))

    def test_callback(self):
        row = self._row(new=True, b=3)
        changed = []
        def callback(id):
            def cb():
                row[id].value()
                changed.append(id)
            return cb
        for id in ('a', 'b', 'c', 'd', 'total', 'half_total'):
            row.register_callback(row.CALL_CHANGE, id, callback(id))
        # self._check_values(row, d=10, total=8, half_total=4)
        # assert 'd' in changed and 'total' in changed and 'half_total' in changed, changed
        # del changed[0:len(changed)]
        self._set(row, c=100)
        # self._check_values(row, d=200, total=103, half_total=51.5)
        self.assertIn('d', changed)
        self.assertIn('total', changed)
        self.assertIn('half_total', changed)
        del changed[0:len(changed)]
        row.set_row(self._data_row(a=1, b=10, c=20, d=30))
        self.assertIn('a', changed)
        self.assertIn('b', changed)
        self.assertIn('c', changed)
        self.assertIn('d', changed)
        self.assertIn('total', changed)
        self.assertIn('half_total', changed)

    def test_editability_callbacks(self):
        enabled = [None] # we need a mutable object...
        row = self._row(a=6)
        def callback():
            enabled[0] = row.editable('d')
        row.register_callback(row.CALL_EDITABILITY_CHANGE, 'd', callback)
        self.assertRaises(
            pytis.util.ProgramError,
            lambda: row.register_callback(row.CALL_EDITABILITY_CHANGE, 'd', lambda: None),
        )
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

    def test_enumerate(self):
        row = self._row()
        enum = [('apl', u'Apple'), ('ban', u'Banana'), ('str', u'Strawberry'), ('org', u'Orange')]
        self.assertEqual(row.enumerate('fruit'), enum)
        self.assertEqual(row.enumerate('c'), None)
        self.assertEqual(row.enumerate('array'), enum)

    def test_enumeration_callbacks(self):
        called = []
        def callback():
            called.append(True)
        row = self._row(a=0)
        row.register_callback(row.CALL_ENUMERATION_CHANGE, 'b', callback)
        row['a'] = 5
        self.assertEqual(len(called), 1)

    def test_has_key(self):
        row = self._row()
        self.assertIn('a', row)
        self.assertIn('half_total', row)
        self.assertNotIn('blabla', row)
        self.assertTrue(row.has_key('a'))
        self.assertFalse(row.has_key('xxx'))

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

    def test_format(self):
        row = self._row(singleline=True, range=(8, 9))
        r1 = row.format('range')
        r2 = row.format('range', single=False)
        self.assertEqual(r1, u'8 — 9')
        self.assertEqual(r2, ('8', '9'))

    def test_codebook(self):
        row = self._row()
        self.assertIsNone(row.codebook('a'))
        self.assertIsNone(row.codebook('b'))
        self.assertEqual(row.codebook('fruit'), 'Fruits')

    def test_cb_value(self):
        row = self._row(fruit='apl')
        self.assertEqual(row.cb_value('fruit', 'title').value(), 'Apple')
        self.assertEqual(row.cb_value('fruit', 'code').value(), 123)
        row['fruit'] = 'ban'
        self.assertEqual(row.cb_value('fruit', 'title').value(), 'Banana')

    def test_display(self):
        row = self._row(fruit='str')
        self.assertEqual(row.display('a'), '')
        self.assertEqual(row.display('fruit'), 'Strawberry')
        row['fruit'] = 'apl'
        self.assertEqual(row.display('fruit'), 'Apple')
        row['fruit'] = None
        self.assertEqual(row.display('fruit'), 'none')
        row['array'] = ('apl', 'str')
        self.assertEqual(row.display('array'), 'Apple, Strawberry')

    def test_prefer_display(self):
        row = self._row()
        self.assertFalse(row.prefer_display('a'))

    def test_display_functions(self):
        enumerator = pd.DataEnumerator(pd.DataFactory(
            pd.MemData,
            [pd.ColumnSpec(c, pd.String()) for c in ('id', 'title', 'letter')],
            data=(('1', 'First', 'A'), ('2', 'Second', 'B')),
        ))
        class Specification(pytis.presentation.Specification):
            data_cls = pd.MemData
            fields = (
                pp.Field('a'),
                pp.Field('b', type=pd.String(enumerator=enumerator),
                         display=lambda x: '-' + x + '-'),
                pp.Field('c', type=pd.String(enumerator=enumerator),
                         display=lambda row: row['title'].value().lower()),
            )
        row = pp.PresentedRow(Specification.fields, Specification().data_spec().create(),
                              None, new=True, prefill=dict(b='1', c='2'))
        self.assertEqual(row.display('b'), '-1-')
        self.assertEqual(row.display('c'), 'second')

    def test_inline_display(self):
        enumerator = pd.DataEnumerator(pd.DataFactory(
            pd.MemData,
            [pd.ColumnSpec(c, pd.String()) for c in ('id', 'title', 'letter')],
            data=(('1', 'First', 'A'), ('2', 'Second', 'B')),
        ))
        class Specification(pytis.presentation.Specification):
            data_cls = pd.MemData
            fields = (
                pp.Field('a', type=pd.String(enumerator=enumerator), display='title',
                         inline_display='b', null_display='-'),
                pp.Field('b', type=pd.String()),
            )
        row = pp.PresentedRow(Specification.fields, Specification().data_spec().create(),
                              None, new=True, prefill=dict(a='1', b='FIRST'))
        self.assertEqual(row.display('a'), 'FIRST')
        self.assertEqual(row.display('a', export=lambda x: x.value().lower()), 'first')
        row['b'] = None
        self.assertEqual(row.display('a'), '-')

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
        self.assertFalse(row.depends('x', row.keys()))
        self.assertTrue(row.depends('total', ('half_total', 'd')))
        self.assertFalse(row.depends('total', ('a', 'b', 'c', 'x', 'total')))
        self.assertFalse(row.depends('half_total', any))
        self.assertTrue(row.depends('fruit', ('fruit_code',)))

    def test_filename(self):
        row = pp.PresentedRow((
            pp.Field('a', default=1),
            pp.Field('x', virtual=True, default='aaa'),
            pp.Field('y', virtual=True, filename='x'),
            pp.Field('z', virtual=True, filename=lambda r: 'file%d.pdf' % r['a'].value()),
        ), self._data, None, new=True)
        self.assertEqual(row.filename('x'), None)
        self.assertEqual(row.filename('y'), 'aaa')
        self.assertEqual(row.filename('z'), 'file1.pdf')

    def test_virtual_field_type_class(self):
        row = pp.PresentedRow((
            pp.Field('a'),
            pp.Field('x', virtual=True, type=pytis.data.Integer),
        ), self._data, None)
        self.assertTrue(isinstance(row.type('x'), pytis.data.Integer))

    def test_invalid_codebook_field_type(self):
        row = pp.PresentedRow((
            pp.Field('a'),
            pp.Field('b', codebook='InvalidName'),
        ), self._data, None)
        self.assertTrue(isinstance(row.type('b'), pytis.data.Integer))

    def test_permissions(self):
        data = pd.RestrictedMemData(
            [pd.ColumnSpec(c, pd.String()) for c in ('x', 'y', 'z')],
            access_rights=pd.AccessRights(
                ('x', (None, pd.Permission.ALL)),
                ('y', (None, pd.Permission.VIEW)),
                ('z', ((), pd.Permission.ALL))),
        )
        row = pp.PresentedRow((
            pp.Field('x'),
            pp.Field('y'),
            pp.Field('z'),
            pp.Field('zz', virtual=True, computer=pp.computer(lambda r, z: '-' + z + '-')),
        ), data, None, new=True)
        self.assertTrue(row.permitted('x', pd.Permission.VIEW))
        self.assertTrue(row.permitted('x', True))
        self.assertTrue(row.permitted('y', pd.Permission.VIEW))
        self.assertFalse(row.permitted('y', pd.Permission.UPDATE))
        self.assertFalse(row.permitted('z', pd.Permission.VIEW))
        protected_row = row.protected()
        self.assertRaises(protected_row.ProtectionError, lambda: protected_row['z'].value())
        self.assertRaises(protected_row.ProtectionError, lambda: protected_row['zz'].value())
        # Cover special cases for a non-permitted field in various methods.
        self.assertIsNone(row.get('z', secure=True))
        self.assertEqual(row.display('z'), '')
        self.assertEqual(row.enumerate('z'), [])
        self.assertFalse(row.editable('z'))


    def test_completer(self):
        completer = pd.DataEnumerator(pd.DataFactory(
            pd.MemData, (pd.ColumnSpec('x', pd.String()),),
            data=(('Apple',), ('Bananas',), ('Basil',), ('Bacardi',), ('Cinamon',)),
        ))
        fields = (
            pp.Field('a', type=pd.Integer(not_null=True)),
            pp.Field('x0', type=pd.String(enumerator=pd.FixedEnumerator(('a', 'b', 'c')))),
            pp.Field('x1', type=pd.String(), completer=('yes', 'no', 'maybe')),
            pp.Field('x2', type=pd.String(), completer=completer,
                     runtime_filter=pp.computer(lambda r, a:
                                                pd.NE('x', pd.sval('Bacardi'))
                                                if a == 1 else None)),
        )
        data = pd.MemData([pd.ColumnSpec(f.id(), f.type()) for f in fields])
        row = pp.PresentedRow(fields, data, None, new=True)
        self.assertFalse(row.has_completer('a'))
        self.assertTrue(row.has_completer('x0'))
        self.assertTrue(row.has_completer('x1', static=True))
        self.assertTrue(row.has_completer('x2'))
        self.assertFalse(row.has_completer('x2', static=True))
        self.assertEqual(row.completions('a'), [])
        self.assertEqual(row.completions('x0'), ['a', 'b', 'c'])
        self.assertEquals(row.completions('x1'), ['maybe', 'no', 'yes'])
        self.assertEquals(row.completions('x1', prefix='y'), ['yes'])
        self.assertEquals(row.completions('x2'), ('Apple', 'Bananas', 'Basil', 'Bacardi',
                                                  'Cinamon'))
        self.assertEquals(row.completions('x2', prefix='ba'), ('Bananas', 'Basil', 'Bacardi'))
        row['a'] = 1
        self.assertEquals(row.completions('x2', prefix='ba'), ('Bananas', 'Basil'))

    def test_unique(self):
        data = pd.MemData(
            (pd.ColumnSpec('a', pd.String(not_null=True, unique=True)),),
            data=(('1',), ('2',), ('3',)),
        )
        row = pp.PresentedRow((pp.Field('a'),), data, None, new=True)
        self.assertTrue(row.validate('a', '1') is not None)
        self.assertTrue(row.validate('a', '4') is None)

    def test_attachment_storage(self):
        storage = pytis.presentation.AttachmentStorage()
        fields = (
            pp.Field('a', type=pd.String()),
            pp.Field('b', type=pd.String(), attachment_storage=storage),
            pp.Field('c', type=pd.String(), attachment_storage=lambda row: storage),
        )
        data = pd.MemData([pd.ColumnSpec(f.id(), f.type()) for f in fields])
        row = pp.PresentedRow(fields, data, None, new=True)
        self.assertIsNone(row.attachment_storage('a'))
        self.assertEquals(row.attachment_storage('b'), storage)
        self.assertEquals(row.attachment_storage('c'), storage)


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
