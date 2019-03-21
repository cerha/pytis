#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (C) 2018-2019 Tomáš Cerha <t.cerha@gmail.com>
# Copyright (C) 2001-2017 Brailcom, o.p.s.
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

############
# field.py #
############


class PresentedRow(unittest.TestCase):

    def setUp(self):
        self.longMessage = True

    def _enumerator(self, columns, data=()):
        return pd.DataEnumerator(pd.DataFactory(
            pd.MemData,
            [pd.ColumnSpec(c, pd.String()) for c in columns],
            data=data,
        ))

    def _row(self, fields=(), row=None, **kwargs):
        class Specification(pp.Specification):
            data_cls = pd.MemData
        Specification.fields = fields
        return pp.PresentedRow(fields, Specification().data_spec().create(), row, **kwargs)

    def _mega_row(self, new=False, row=None, singleline=False, **prefill):
        # TODO: This all-in-one row was historically used for all tests
        # but it proved to be problematic.  You can not see the definition
        # of the fields from paticular tests so it is hard to guess the test
        # logic.  Morover the specification is getting too complicated
        # and hard to maintain usable in all situations.  Thus new tests should
        # Define their own fields directly just for their purpose.

        class BigString(pd.String, pd.Big):
            pass

        class SpecialEnumerator(pd.FixedEnumerator):
            # This class is overriden just to allow definition of runtime_filter
            # and runtime_arguments for the same field (which is only important
            # to improve test coverage)

            def values(self, a=None):
                # Accepts argument a as returned by runtime_arguments.
                return super(SpecialEnumerator, self).values()

        fields = (
            pp.Field('a', type=pd.Integer(not_null=True)),
            pp.Field('b', type=pd.Integer(not_null=True,
                                          enumerator=SpecialEnumerator(range(101))),
                     runtime_filter=pp.computer(lambda r, a: lambda x: x % a == 0, validate=True),
                     runtime_arguments=pp.computer(lambda r, a: dict(a=a))),
            pp.Field('c', type=pd.Integer(not_null=True), default=lambda: 5),
            pp.Field('d', type=pd.Integer(),
                     editable=pp.computer(lambda r, total: total > 5),
                     computer=pp.computer(lambda r, c: c * 2, validate=True)),
            pp.Field('fruit', type=pd.String(), codebook='Fruits', display='title',
                     null_display='none'),
            pp.Field('fruit_code', virtual=True, computer=pp.CbComputer('fruit', 'code')),
            pp.Field('range', type=pd.IntegerRange(),
                     visible=pp.computer(lambda r, a: a != 0)),
            pp.Field('total', type=pd.Integer(), virtual=True,
                     editable=pp.Editable.NEVER,
                     computer=pp.computer(lambda r, b, c: b + c, validate=True, fallback=0)),
            pp.Field('half_total', type=pd.Integer(), virtual=True,
                     editable=pp.Editable.NEVER,
                     computer=pp.computer(lambda r, total:
                                          total / 2 if total is not None else None)),
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
            _specifications = {'Fruits': Fruits}

            def _get_object_by_name(self, name):
                try:
                    return self._specifications[name]
                except KeyError:
                    return super(TestResolver, self)._get_object_by_name(name)

        config.resolver = TestResolver()
        return self._row(fields, row=row, new=new, singleline=singleline, prefill=prefill)

    def _data_row(self, row, **values):
        return pd.Row([(f.id(), pd.Value(f.type(), values.get(f.id())))
                       for f in row.fields() if not f.virtual()])

    def _check_values(self, row, **kwargs):
        for key, value in kwargs.items():
            rvalue = row[key].value()
            if isinstance(row[key].type(), pd.Range) and rvalue is not None:
                rvalue = tuple(rvalue)
            self.assertEqual(rvalue, value, key)

    def test_init(self):
        row = self._mega_row(new=True)
        self._check_values(row, a=None, b=None, c=5, d=10, total=0, x=88, range=None)
        row = self._mega_row()
        self._check_values(row, a=None, b=None, c=None, d=None, total=None)
        data_row = self._data_row(row, a=4, b=100, c=77, d=18, range=(1, 8))
        row = self._mega_row(row=data_row, new=True)
        self._check_values(row, a=4, b=100, c=77, d=18, x=88, total=177, range=(1, 8))
        row = self._mega_row(row=data_row)
        self._check_values(row, a=4, b=100, c=77, d=18, x=None, total=177)

    def test_unicode(self):
        class BigString(pd.String, pd.Big):
            pass
        row = self._row((
            pp.Field('x', type=pd.Integer(not_null=True)),
            pp.Field('y', type=pd.Integer(), default=88),
            pp.Field('passwd', type=pd.Password()),
            pp.Field('data', type=BigString()),
        ), new=True, prefill=dict(x=1, y=3, passwd='secret', data=1024 * 'x'))
        self.assertEqual(unicode(row), ('<PresentedRow: x=1, y=3, '
                                        'passwd=***, data=<BigString 1 kB>>'))

    def test_unicode_uninitialized(self):
        row = self._row((pp.Field('x'), pp.Field('y')))
        delattr(row, '_row')
        self.assertRegexpMatches(unicode(row), r'<PresentedRow: [0-9a-h]+>')

    def test_prefill(self):
        row = self._mega_row(new=True, a=1, b=pd.ival(3), d=77)
        self._check_values(row, a=1, b=3, c=5, d=77, x=88)

    def test_new(self):
        row = self._row((pp.Field('x'), pp.Field('y')))
        self.assertFalse(row.new())
        row = self._row((pp.Field('x'), pp.Field('y')), new=True)
        self.assertTrue(row.new())

    def test_key(self):
        row = self._row((pp.Field('x', type=pd.Integer(not_null=True)), pp.Field('y')))
        self.assertEqual(row.key(), (pd.Value(pd.Integer(not_null=True), None),))
        row['x'] = 1
        self.assertEqual(row.key(), (pd.Value(pd.Integer(not_null=True), 1),))

    def test_fields(self):
        fields = (pp.Field('x'), pp.Field('y'))
        row = self._row(fields)
        self.assertEqual(row.fields(), fields)

    def test_keys(self):
        row = self._row((pp.Field('x'), pp.Field('y')))
        self.assertItemsEqual(row.keys(), ['x', 'y'])

    def test_resolver(self):
        row = self._row((pp.Field('x'), pp.Field('y')))
        self.assertEqual(row.resolver(), config.resolver)

    def test_data(self):
        row = self._row((pp.Field('x'), pp.Field('y')))
        self.assertTrue(isinstance(row.data(), pd.MemData))

    def test_set_transaction(self):
        row = self._row((pp.Field('x'), pp.Field('y')))
        self.assertEqual(row.transaction(), None)
        # TODO: This whole test file tries to avoid using a real database connection,
        # so we can't create a real transaction here.  Using 'x' is invalid for
        # real use but it stil verifies that set_transaction works as long as
        # set_transaction doesn't validate its argument.
        transaction = 'x'  # pd.DBTransactionDefault(config.dbconnection)
        row.set_transaction(transaction)
        self.assertEqual(row.transaction(), transaction)

    def test_original_row(self):
        r = pd.Row([(k, pd.Value(pd.Integer(), v)) for k, v in
                    dict(a=None, b=6, c=None, d=3).items()])
        row = self._mega_row(new=True, row=r, b=22, c=33, d=44)
        r1 = row.original_row()
        self._check_values(r1, a=None, b=22, c=33, d=44)
        r2 = row.original_row(initialized=False)
        self._check_values(r2, a=None, b=6, c=None, d=3)

    def test_prefill_default(self):
        row = self._mega_row(new=True, b=22, c=33, d=44, x=55)
        self._check_values(row, b=22, c=33, d=44, x=55)

    def test_setitem(self):
        row = self._mega_row()
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

    def test_get(self):
        row = self._mega_row(new=True, a=4)
        self.assertEqual(row.get('a').value(), 4)
        self.assertIsNone(row.get('aaa'))
        self.assertEqual(row.get('aaa', default=10), 10)

    def test_computer(self):
        row = self._mega_row(new=True, b=3)
        self.assertIsNone(row.get('total', lazy=True).value())
        self._check_values(row, d=10, total=8, half_total=4)
        row['c'] = 100
        self._check_values(row, d=200, total=103, half_total=51)
        row['a'] = 4
        row['b'] = 100
        row['c'] = 88
        row['range'] = (8, 9)
        self.assertEqual(row.get('total', lazy=True).value(), 103)
        self._check_values(row, a=4, b=100, c=88, total=188)
        row['b'] = None
        self._check_values(row, total=0, d=176)
        row['c'] = None
        self._check_values(row, total=0, d=176)
        row['c'] = 2
        self._check_values(row, total=0, d=4)
        row['b'] = 1
        self._check_values(row, total=3, d=4)

    def test_binary_computer(self):
        a = '\x04\x00\x00\x05\x00\x19\x00'
        b = '\x00\x05\x04\xa4\xbb\x10\x00'
        row = self._row((
            pp.Field('x', type=pd.String()),
            pp.Field('data', type=pd.Binary(not_null=True),
                     computer=pp.computer(lambda r, x: x == 'a' and a or x == 'b' and b or None)),
        ))
        self.assertEqual(row['data'].value(), None)
        row['x'] = pd.sval('a')
        self.assertEqual(row['data'].value(), a)
        self.assertIsInstance(row['data'].value(), pd.Binary.Data)
        row['x'] = pd.sval('b')
        self.assertEqual(row['data'].value(), b)

    def test_recursive_computer_validation(self):
        fields = (
            pp.Field('a', type=pd.Integer(not_null=True),
                     computer=pp.computer(lambda r, b: 2 * b, validate=True)),
            pp.Field('b', type=pd.Integer(not_null=True,
                                          enumerator=pd.FixedEnumerator(range(101))),
                     runtime_filter=pp.computer(lambda r, a: lambda x: x % a == 0, validate=True)),
        )
        # The computer for 'a' is called to compute the initial value and will lead to recursion
        # because it requires validation of 'b' which needs the value of 'a'...
        self.assertRaises(RuntimeError, lambda: self._row(fields, new=True))

        class Specification(pp.Specification):
            pass
        Specification.fields = fields
        # ViewSpec initialization should detect the cyclic dependency.
        self.assertRaises(AssertionError, lambda: Specification().view_spec())

        class Specification2(Specification):

            def _customize_fields(self, fields):
                fields.modify('b', runtime_filter=pp.computer(lambda r, a: lambda x: x % a == 0,
                                                              validate=True, novalidate=('a',)))
        row = self._row(Specification2().view_spec().fields(), new=True)
        # 'a' is None so runtime_filter will try to compute x % None (because 'a' is not validated).
        self.assertRaises(TypeError, lambda: row.enumerate('b'))

    def test_cb_computer(self):
        row = self._mega_row(new=True, fruit='str')
        self.assertTrue(isinstance(row.type('fruit_code'), pytis.data.Integer))
        self.assertEqual(row['fruit_code'].value(), 234)

    def test_prefill_computer(self):
        row = self._mega_row(new=True, b=2, c=2, total=88)
        self._check_values(row, b=2, c=2, total=88, half_total=44)

    def test_validation(self):
        row = self._mega_row()
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
        row = self._mega_row(new=True)
        self._check_values(row, a=None, b=None, c=5, total=0, half_total=0)
        row.set_row(self._data_row(row, b=10, c=20))
        self._check_values(row, a=None, b=10, c=20, total=30, half_total=15)
        row.set_row(None)
        self._check_values(row, a=None, b=None, c=5, total=0, half_total=0)
        row = self._mega_row()
        self._check_values(row, a=None, b=None, c=None, total=None, half_total=None)
        row.set_row(self._data_row(row, b=10, c=20))
        self._check_values(row, a=None, b=10, c=20, total=30, half_total=15)
        row.set_row(None)
        self._check_values(row, a=None, b=None, c=None, total=None, half_total=None)

    def test_editable(self):
        row = self._mega_row(b=2, c=1)
        self.assertTrue(row.editable('a'))
        self.assertFalse(row.editable('d'))
        row['b'] = 5
        self.assertTrue(row.editable('d'))

    def test_visible(self):
        row = self._mega_row(a=0, new=True)
        self.assertTrue(row.visible('a'))
        self.assertFalse(row.visible('range'))
        row['a'] = 1
        self.assertTrue(row.visible('range'))

    def test_callback(self):
        row = self._mega_row(new=True, b=3)
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
        row['c'] = 100
        # self._check_values(row, d=200, total=103, half_total=51.5)
        self.assertIn('d', changed)
        self.assertIn('total', changed)
        self.assertIn('half_total', changed)
        del changed[0:len(changed)]
        row.set_row(self._data_row(row, a=1, b=10, c=20, d=30))
        self.assertIn('a', changed)
        self.assertIn('b', changed)
        self.assertIn('c', changed)
        self.assertIn('d', changed)
        self.assertIn('total', changed)
        self.assertIn('half_total', changed)

    def test_editability_callbacks(self):
        enabled = [None]  # we need a mutable object...
        row = self._mega_row(a=6)

        def callback():
            enabled[0] = row.editable('d')
        row.register_callback(row.CALL_EDITABILITY_CHANGE, 'd', callback)
        self.assertRaises(
            pytis.util.ProgramError,
            lambda: row.register_callback(row.CALL_EDITABILITY_CHANGE, 'd', lambda: None),
        )
        self.assertIsNone(enabled[0])
        row['a'] = 8
        self.assertIsNone(enabled[0])
        row['c'] = 3
        self.assertFalse(enabled[0])
        row['b'] = 2
        self.assertFalse(enabled[0])
        row['b'] = 3
        self.assertEqual(row['total'].value(), 6)
        self.assertTrue(enabled[0])
        row['c'] = 2
        self.assertEqual(row['total'].value(), 5)
        self.assertFalse(enabled[0])

    def test_runtime_filter(self):
        def enum(row, key):
            return tuple(x for x, display in row.enumerate('b'))
        row = self._mega_row(a=20, b=0, c=5)
        self.assertEqual(enum(row, 'b'), (0, 20, 40, 60, 80, 100))

    def test_enumerate(self):
        row = self._mega_row()
        enum = [('apl', u'Apple'), ('ban', u'Banana'), ('str', u'Strawberry'), ('org', u'Orange')]
        self.assertEqual(row.enumerate('fruit'), enum)
        self.assertEqual(row.enumerate('c'), None)
        self.assertEqual(row.enumerate('array'), enum)

    def test_enumeration_callbacks(self):
        called = []

        def callback():
            called.append(True)
        row = self._mega_row(a=0)
        row.register_callback(row.CALL_ENUMERATION_CHANGE, 'b', callback)
        row['a'] = 5
        self.assertEqual(len(called), 1)

    def test_has_key(self):
        row = self._mega_row()
        self.assertIn('a', row)
        self.assertIn('half_total', row)
        self.assertNotIn('blabla', row)
        self.assertTrue(row.has_key('a'))  # noqa: W601
        self.assertFalse(row.has_key('xxx'))  # noqa: W601

    def test_changed(self):
        row = self._mega_row()
        self.assertFalse(row.changed())
        row['b'] = 333
        self.assertTrue(row.changed())

    def test_field_changed(self):
        row = self._mega_row(b=3, c=8)
        self.assertFalse(row.field_changed('a'))
        self.assertFalse(row.field_changed('b'))
        self.assertFalse(row.field_changed('c'))
        row['b'] = 7
        self.assertFalse(row.field_changed('a'))
        self.assertTrue(row.field_changed('b'))
        self.assertFalse(row.field_changed('c'))
        self.assertIsNotNone(row.validate('a', '3.4'))
        self.assertTrue(row.field_changed('a'))
        self.assertIsNotNone(row.validate('a', ''))
        self.assertFalse(row.field_changed('a'))

    def test_format(self):
        row = self._mega_row(singleline=True, range=(8, 9))
        r1 = row.format('range')
        r2 = row.format('range', single=False)
        self.assertEqual(r1, u'8 — 9')
        self.assertEqual(r2, ('8', '9'))

    def test_codebook(self):
        row = self._mega_row()
        self.assertIsNone(row.codebook('a'))
        self.assertIsNone(row.codebook('b'))
        self.assertEqual(row.codebook('fruit'), 'Fruits')

    def test_cb_value(self):
        row = self._mega_row(fruit='apl')
        self.assertEqual(row.cb_value('fruit', 'title').value(), 'Apple')
        self.assertEqual(row.cb_value('fruit', 'code').value(), 123)
        row['fruit'] = 'ban'
        self.assertEqual(row.cb_value('fruit', 'title').value(), 'Banana')

    def test_display(self):
        row = self._mega_row(fruit='str')
        self.assertEqual(row.display('a'), '')
        self.assertEqual(row.display('fruit'), 'Strawberry')
        row['fruit'] = 'apl'
        self.assertEqual(row.display('fruit'), 'Apple')
        row['fruit'] = None
        self.assertEqual(row.display('fruit'), 'none')
        row['array'] = ('apl', 'str')
        self.assertEqual(row.display('array'), 'Apple, Strawberry')

    def test_prefer_display(self):
        row = self._mega_row()
        self.assertFalse(row.prefer_display('a'))

    def test_display_functions(self):
        enumerator = self._enumerator(('id', 'title', 'letter'),
                                      data=(('1', 'First', 'A'), ('2', 'Second', 'B')))
        fields = (
            pp.Field('a'),
            pp.Field('b', type=pd.String(enumerator=enumerator),
                     display=lambda x: '-' + x + '-'),
            pp.Field('c', type=pd.String(enumerator=enumerator),
                     display=lambda row: row['title'].value().lower()),
        )
        row = self._row(fields, None, new=True, prefill=dict(b='1', c='2'))
        self.assertEqual(row.display('b'), '-1-')
        self.assertEqual(row.display('c'), 'second')

    def test_inline_display(self):
        enumerator = self._enumerator(('id', 'title', 'letter'),
                                      data=(('1', 'First', 'A'), ('2', 'Second', 'B')))
        fields = (
            pp.Field('a', type=pd.String(enumerator=enumerator), display='title',
                     inline_display='b', null_display='-'),
            pp.Field('b', type=pd.String()),
        )
        row = self._row(fields, new=True, prefill=dict(a='1', b='FIRST'))
        self.assertEqual(row.display('a'), 'FIRST')
        self.assertEqual(row.display('a', export=lambda x: x.value().lower()), 'first')
        row['b'] = None
        self.assertEqual(row.display('a'), '-')

    def test_depends(self):
        row = self._mega_row()
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
        row = self._row((
            pp.Field('x', default='aaa'),
            pp.Field('y', filename='x'),
            pp.Field('z', filename=lambda r: 'file_%s.pdf' % r['x'].value()),
        ), new=True)
        self.assertEqual(row.filename('x'), None)
        self.assertEqual(row.filename('y'), 'aaa')
        self.assertEqual(row.filename('z'), 'file_aaa.pdf')

    def test_virtual_field_type_class(self):
        row = self._row((
            pp.Field('a', type=pytis.data.Integer),
        ))
        self.assertTrue(isinstance(row.type('a'), pytis.data.Integer))

    def test_invalid_codebook_field_type(self):
        row = self._row((
            pp.Field('a', codebook='InvalidName'),
        ))
        self.assertTrue(isinstance(row.type('a'), pytis.data.String))

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
        completer = self._enumerator(
            ('x',),
            data=(('Apple',), ('Bananas',), ('Basil',), ('Bacardi',), ('Cinamon',)),
        )
        fields = (
            pp.Field('a', type=pd.Integer(not_null=True)),
            pp.Field('x0', type=pd.String(enumerator=pd.FixedEnumerator(('a', 'b', 'c')))),
            pp.Field('x1', type=pd.String(), completer=('yes', 'no', 'maybe')),
            pp.Field('x2', type=pd.String(), completer=completer,
                     runtime_filter=pp.computer(lambda r, a:
                                                pd.NE('x', pd.sval('Bacardi'))
                                                if a == 1 else None)),
        )
        row = self._row(fields, new=True)
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
        row = self._row((
            pp.Field('a', type=pd.String()),
            pp.Field('b', type=pd.String(), attachment_storage=storage),
            pp.Field('c', type=pd.String(), attachment_storage=lambda row: storage),
        ))
        self.assertIsNone(row.attachment_storage('a'))
        self.assertEquals(row.attachment_storage('b'), storage)
        self.assertEquals(row.attachment_storage('c'), storage)

    # Tests for previously existing bugs.  These tests help fixing the bugs
    # and prevent their future re-appearance.

    def test_validation_order(self):
        # This test reproduces a previously existing bug in computer input
        # validation dependencies.
        fields = (
            pp.Field('a', type=pd.String(not_null=True)),
            pp.Field('b', type=pd.String(not_null=True, maxlen=1),
                     computer=pp.computer(lambda r, a: a[0].upper(), validate=True)),
            pp.Field('c', type=pd.String(enumerator=pd.FixedEnumerator(range(10)), not_null=True),
                     computer=pp.computer(lambda r, b: str(ord(b) % 10), validate=True)),
        )
        row = self._row(fields, new=True)
        row.validate('a', 'foo')
        self.assertEqual(row['b'].value(), 'F')
        self.assertEqual(row['c'].value(), '0')
        # Set 'b' to an invalid value (violates maxlen=1).
        row.validate('b', 'xxx')

        def cb():
            # This used to fail when the computer for 'c' was not called
            # due to invalid value of 'b' (when 'b' validity was not refreshed
            # correctly).
            self.assertEqual(row['c'].value(), '6')
        row.register_callback(row.CALL_CHANGE, 'c', cb)
        row.validate('a', 'bar')

    def test_validation_cache(self):
        # This test reproduces a previously existing bug in validation result caching.
        enumerator = self._enumerator(('id', 'title'), data=(('1', 'First'), ('2', 'Second')))
        row = self._row((
            pp.Field('a', type=pd.String(not_null=True, enumerator=enumerator)),
        ))
        self.assertFalse(row.validate('a', '3') is None)
        data = enumerator._data  # There is currently no need to make this public elsewhere.
        data.insert(pd.Row((('id', pd.sval('3')), ('title', pd.sval('Third')))))
        self.assertEqual(row.validate('a', '3'), None)

    def test_cbcomputer_display(self):
        enumerator = self._enumerator(('id', 'title'), data=(('1', 'First'), ('2', 'Second')))
        fields = (
            pp.Field('a', type=pd.String(enumerator=enumerator), display='title'),
            pp.Field('b', computer=pp.CbComputer('a', 'title')),
        )
        row = self._row(fields)
        row['a'] = '1'
        self.assertEqual(row.display('a'), 'First')
        self.assertEqual(row.display('b'), '')

    def test_editable_always_bool(self):
        row = self._row((
            pp.Field('a', type=pd.String()),
            pp.Field('b', type=pd.Integer()),
            pp.Field('c', editable=pp.computer(lambda r, a, b: a or b)),
        ))
        for a, b, editable in (
                (None, None, False),
                ('', None, False),
                (None, 0, False),
                ('', 0, False),
                (None, 1, True),
                ('', 5, True),
                ('x', None, True),
                ('y', 8, True),
        ):
            row['a'] = a
            row['b'] = b
            self.assertIs(row.editable('c'), editable, (a, b))

    def test_prefill_untouched(self):
        row = self._row((
            pp.Field('a', type=pd.Integer()),
            pp.Field('b', type=pd.Integer()),
        ))
        prefill = dict(a=5, b=8)
        row.set_row(None, reset=True, prefill=prefill)
        self.assertEqual(prefill, dict(a=5, b=8))


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


class Style(unittest.TestCase):

    def test_colors(self):
        self.assertRaises(TypeError, lambda: pp.Style(background=20))
        self.assertRaises(TypeError, lambda: pp.Style(background={}))
        self.assertRaises(TypeError, lambda: pp.Style(background=('a', 10, 20)))
        self.assertRaises(ValueError, lambda: pp.Style(background='#1g2h3i'))
        self.assertRaises(ValueError, lambda: pp.Style(background='fff'))
        self.assertRaises(ValueError, lambda: pp.Style(background=(300, 10, 20)))
        self.assertEqual(pp.Style(background='#fff'), pp.Style(background='#FFFFFF'))
        self.assertEqual(pp.Style(background='#fff'), pp.Style(background=(255, 255, 255)))
        self.assertEqual(pp.Style(background='#fff'), pp.Style(background=pp.Color.WHITE))
        self.assertEqual(pp.Style(background='#002e6a'), pp.Style(background=[0, 46, 106]))
        self.assertEqual(pp.Style(background='#e00020'), pp.Style(background=(0xe0, 0x00, 0x20)))

    def test_cmp(self):
        self.assertEqual(pp.Style(), pp.Style())
        self.assertEqual(pp.Style(slanted=True), pp.Style(slanted=True))
        self.assertEqual(pp.Style(slanted=True, foreground='#fff'),
                         pp.Style(slanted=True, foreground='#FFF'))
        self.assertEqual(pp.Style(slanted=False), pp.Style())
        self.assertNotEqual(pp.Style(), pp.Orientation())
        self.assertNotEqual(pp.Orientation(), pp.Style())
        self.assertNotEqual(pp.Style(slanted=True), pp.Style(foreground='#fff'))


class DocTest(unittest.TestCase):

    def test_field_computations(self):
        import doctest
        doctest.testfile('../../../doc/tutorials/Fields.txt')


if __name__ == '__main__':
    unittest.main()
