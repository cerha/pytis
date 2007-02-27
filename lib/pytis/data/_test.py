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

import string
import time

from mx import DateTime as DT
import unittest

from pytis.util import *
import pytis.data
import dbdata

_connection_data = {'database': 'test'}

tests = TestSuite()

import sys
# Ošklivý hack kvůli ošetření defaultního kódování
# protože site.py maže ze 'sys' metodu setdefaultencoding
reload(sys)
sys.setdefaultencoding('iso-8859-2')



#############
# types_.py #
#############


class ValidationError(unittest.TestCase):
    MESSAGE = 'test message'
    def test_it(self):
        ValidationError.e = \
          pytis.data.ValidationError(ValidationError.MESSAGE)
        assert ValidationError.e.message() == ValidationError.MESSAGE
tests.add(ValidationError)


class Value(unittest.TestCase):
    def test_values(self):
        t = pytis.data.Type()
        v1 = pytis.data.Value(t, None)
        v2 = pytis.data.Value(t, 1)
        v3 = pytis.data.Value(t, t)
        assert v1.type() == t and v2.type() == t and v3.type() == t, \
               'type lost'
        assert v1.value() == None and v2.value() == 1 and v3.value() == t, \
               'value lost'
    def test_cmp(self):
        t = pytis.data.Type()
        v1 = pytis.data.Value(t, 1)
        v2 = pytis.data.Value(t, 1)
        v3 = pytis.data.Value(t, 2)
        assert v1 == v2, 'equal objects not equal'
        assert v1 != v3, 'non-equal objects equal'
tests.add(Value)


class _TypeCheck(unittest.TestCase):
    def _test_validity(self, type_, value, expected_value,
                       check_value=True, check_export=True, kwargs={},
                       ekwargs={}):
        if type_ == None:
            type_ = self._test_instance
        v, e = type_.validate(value, **kwargs)
        if check_value and expected_value == None:
            assert v == None, ('value returned on error', str(v))
            assert isinstance(e, pytis.data.ValidationError), \
                   ('invalid error instance', e)
        else:
            assert e == None, \
                   ('proper value generated error', value, e.message())
            assert isinstance(v.type(), type_.__class__), \
                   ('invalid value type', v.type())
            if check_value:
                assert v.value() == expected_value, \
                       ('invalid value', v.value(), expected_value)
        if check_export and e == None:
            result, error = type_.validate(type_.export(v.value(),
                                                        **ekwargs),
                                           **kwargs)
            assert result == v, ('export failed', str(v), str(result))
        return v, e
    def _test_null_validation(self):
        v, e = self._test_instance.validate('')
        assert e == None, ('Null validation failed', e)
        assert v.value() == None, ('Non-empty value', v.value())
        assert v.type() == self._test_instance, ('Invalid type', v.type())
        return v

    def test_cmp(self):
        c = self._test_instance.__class__
        assert c() == c(), 'comparison failed'


class Type(_TypeCheck):
    _test_instance = pytis.data.Type()
    def test_validation(self):
        self._test_null_validation()
    def test_noncmp(self):
        assert self._test_instance != pytis.data.Integer(), \
               'different types equal'
        assert pytis.data.Integer(not_null=True) != pytis.data.Integer(), \
               'different types equal'
        assert pytis.data.String(maxlen=2) != pytis.data.String(maxlen=3), \
               'different types equal'
tests.add(Type)


class Integer(_TypeCheck):
    _test_instance = pytis.data.Integer()
    def test_validation(self):
        self._test_validity(None, '1', 1)
        self._test_validity(None, '-11111111111111111111',
                            -11111111111111111111L)
        self._test_validity(None, '+0L', 0L)
        self._test_validity(None, '1.1', None)
        self._test_validity(None, 'foo', None)
tests.add(Integer)


class Float(_TypeCheck):
    _test_instance = pytis.data.Float()
    def test_validation(self):
        self._test_validity(None, '3', 3.0)
        self._test_validity(None, '3.14', 3.14)
        self._test_validity(None, '-3.14', -3.14)
        self._test_validity(None, '0.0', 0.0)
        self._test_validity(None, 'foo', None)
    def test_precision(self):
        PRECISION = 3
        t = pytis.data.Float(precision=PRECISION)
        v, _ = self._test_validity(t, '3.14159265', 3.14159265,
                                   check_export=False)
        assert v.type().precision() == PRECISION, 'wrong precision value'
        assert v.export() == '3.142', 'invalid export result'
    def test_rounding(self):
        self._test_validity(None, '3.1415', 3.14, kwargs={'precision': 2})
        self._test_validity(None, '3.1415', 3.142, kwargs={'precision': 3})
        self._test_validity(None, '2.71', 3, kwargs={'precision': 0})
        F = pytis.data.Float.FLOOR
        C = pytis.data.Float.CEILING
        self._test_validity(None, '3.14159', 3.141, kwargs={'precision': 3,
                                                            'rounding': F})
        self._test_validity(None, '3.14159', 3.15, kwargs={'precision': 2,
                                                           'rounding': C})
        self._test_validity(None, '3.14', 3.14, kwargs={'precision': 2,
                                                        'rounding': F})
        self._test_validity(None, '3.14', 3.14, kwargs={'precision': 2,
                                                        'rounding': C})
tests.add(Float)


class String(_TypeCheck):
    _test_instance = pytis.data.String()
    def test_validation_limited(self):
        MAXLEN = 5
        t = pytis.data.String(maxlen=MAXLEN)
        v, _ = self._test_validity(t, 'abcde', 'abcde')
        assert v.type().maxlen() == MAXLEN, 'wrong maxlen value'
        self._test_validity(t, 'abcdef', None)
    def test_validation_unlimited(self):
        v = self._test_null_validation()
        assert v.type().maxlen() == None, 'wrong maxlen value'
        self._test_validity(None, 'abcdefghi', 'abcdefghi')
        t = pytis.data.String(None)
        assert self._test_validity(t, 'abcdefghi', 'abcdefghi')
    def test_cmp(self):
        MAXLEN = 1
        _TypeCheck.test_cmp(self)
        t = pytis.data.String(maxlen=MAXLEN)
        assert t == pytis.data.String(maxlen=MAXLEN), 'comparison failed'
        assert t != self._test_instance, 'invalid comparison'
        assert t != pytis.data.String(maxlen=MAXLEN+1), \
               'invalid comparison'
tests.add(String)

class Color(_TypeCheck):
    _test_instance = pytis.data.Color()
    def test_validation(self):
        self._test_validity(None, '#0030ab', '#0030ab')
        self._test_validity(None, '0030ab', None)
        self._test_validity(None, '#h030ab', None)
        v, e = self._test_validity(None, '', None, check_value=False)
        assert v.value() == None, ('invalid value', v)
    def test_cmp(self):
        _TypeCheck.test_cmp(self)
        t = pytis.data.Color()
        assert t != pytis.data.String(), 'comparison failed'
        assert t == self._test_instance, 'invalid comparison'
tests.add(Color)

class DateTime(_TypeCheck):
    _test_instance = pytis.data.DateTime(format='%Y-%m-%d %H:%M:%S')
    def test_validation(self):
        vkwargs = {'local': False}
        self._test_validity(None, '2001-02-28 12:14:59',
                            DT.DateTime(2001,2,28,12,14,59), kwargs=vkwargs,
                            ekwargs=vkwargs)
        self._test_validity(None, '2999-12-31 0:0:0',
                            DT.DateTime(2999,12,31,0,0,0), kwargs=vkwargs,
                            ekwargs=vkwargs)
        self._test_validity(None, '  1999-01-01    23:59:59    ',
                            DT.DateTime(1999,1,1,23,59,59), kwargs=vkwargs,
                            ekwargs=vkwargs)
        self._test_validity(None, '1999-01-01 23:59', None, kwargs=vkwargs,
                            ekwargs=vkwargs)
        self._test_validity(None, '1999-01-01 23:59:00 +0200', None,
                            kwargs=vkwargs, ekwargs=vkwargs)
        self._test_validity(None, '99-01-01 0:0:0', None, kwargs=vkwargs,
                            ekwargs=vkwargs)
        self._test_validity(None, '2000-13-01 0:0:0', None, kwargs=vkwargs,
                            ekwargs=vkwargs)
        self._test_validity(None, '2001-02-29 0:0:0', None, kwargs=vkwargs,
                            ekwargs=vkwargs)
        self._test_validity(None, '2001-02-28 24:00:00', None, kwargs=vkwargs,
                            ekwargs=vkwargs)
    def test_export(self):
        vkwargs = {'local': False}
        v, e = self._test_validity(None, '2100-02-05 01:02:03',
                                   DT.DateTime(2100,2,5,1,2,3),
                                   kwargs=vkwargs, ekwargs=vkwargs,
                                   check_export=False)
        exp = v.type().export
        val = v.value()
        result = exp(val, **vkwargs)
        assert result == '2100-02-05 01:02:03', ('Invalid date export', result)
tests.add(DateTime)


class Date(_TypeCheck):
    _test_instance = pytis.data.Date(format=pytis.data.Date.DEFAULT_FORMAT)
    def test_validation(self):
        self._test_validity(None, '2001-02-28', DT.Date(2001,2,28))
        self._test_validity(None, '2999-12-31', DT.Date(2999,12,31))
        self._test_validity(None, '  1999-01-01    ', DT.Date(1999,1,1))
        self._test_validity(None, '1999-01-01', DT.Date(1999,1,1))
        self._test_validity(None, '1999-01-01 23:59', None)
        self._test_validity(None, '1999-01-01 23:59:00', None)
        self._test_validity(None, '01-02-29', None)
        self._test_validity(None, '2000-13-01', None)
        self._test_validity(None, '2001-02-29', None)
tests.add(Date)

class Boolean(_TypeCheck):
    _test_instance = pytis.data.Boolean()
    def test_validation(self):
        v, _ = self._test_validity(None, 'T', None, check_value=False)
        assert v.value(), 'T not mapped to true'
        v, _ = self._test_validity(None, 'F', None, check_value=False)
        assert not v.value(), 'F not mapped to false'
        self._test_validity(None, 't', None)
        self._test_validity(None, '0', None)
    def test_noncmp(self):
        assert self._test_instance != pytis.data.String(), \
               'invalid comparison'
tests.add(Boolean)


class Enumerator(_TypeCheck):
    # Netestováno, neboť třída není používána přímo, stačí testovat potomky
    pass

class DataEnumerator(unittest.TestCase):
    def setUp(self):
        C = pytis.data.ColumnSpec
        S = pytis.data.String()
        B = pytis.data.Boolean()
        V = pytis.data.Value
        data = [pytis.data.Row((('x', V(S, x)), ('y', V(S, y)), ('z', V(B, z))))
                for x,y,z in (('1','a',True), ('2','b',True), ('3','c',False))]
        d = pytis.data.DataFactory(pytis.data.MemData,
                                   (C('x', S), C('y', S), C('z', B)),
                                   data=data)
        e1 = pytis.data.DataEnumerator(d)
        e2 = pytis.data.DataEnumerator(d, value_column='y')
        e3 = pytis.data.DataEnumerator(d, validity_column='z')
        self.cb1 = pytis.data.String(enumerator=e1)
        self.cb2 = pytis.data.String(enumerator=e2, not_null=True)
        self.cb3 = pytis.data.String(enumerator=e3)
    def _test_validate(self, cb, value, expected=None, invalid=False):
        v, e = cb.validate(value)
        if invalid:
            assert e is not None
        else:
            assert e is None, e
            assert v.value() == expected
    def _test_export(self, cb, value, expected):
        result = self.cb1.export(value)
        assert result == expected, ('Invalid exported value:', result)
    def test_validate(self):
        self._test_validate(self.cb1, '1', '1')
        self._test_validate(self.cb1, '',  None)
        self._test_validate(self.cb1, '8', None, invalid=True)
        self._test_validate(self.cb2, 'b', 'b')
        self._test_validate(self.cb2, '',  None, invalid=True)
        self._test_validate(self.cb2, 'd', None, invalid=True)
        self._test_validate(self.cb2, None, None, invalid=True)
        self._test_validate(self.cb3, '1', '1')
        self._test_validate(self.cb3, '3', None, invalid=True)
        self._test_validate(self.cb3, None, None)
    def test_export(self):
        self._test_export(self.cb1, '2', '2')
        self._test_export(self.cb2, '8', '8')
        self._test_export(self.cb2, '', '')
        self._test_export(self.cb2, None, '')
    def test_values(self):
        v = self.cb1.enumerator().values()
        assert v == ('1', '2', '3'), v
    def test_get(self):
        e = self.cb1.enumerator()
        b = e.get('2', 'y')
        assert b.value() == 'b', ('Unexpected value', b)
tests.add(DataEnumerator)
        
class FixedEnumerator(unittest.TestCase):
    _values = (1,3,5,7,9)
    _enumerator = pytis.data.FixedEnumerator(_values)
    def test_check(self):
        e = self._enumerator
        for i in range (100):
            if i in self._values:
                assert e.check(i)
            else:
                assert not e.check(i)
        assert not e.check('1')
    def test_values(self):
        assert self._enumerator.values() == self._values
tests.add(FixedEnumerator)
        

###########
# data.py #
###########


class ReversedSorting(unittest.TestCase):
    def test_it(self):
        A = pytis.data.ASCENDENT
        D = pytis.data.DESCENDANT
        assert () == pytis.data.reversed_sorting(())
        assert (('foo', A),) == pytis.data.reversed_sorting((('foo', D),))
        assert (('foo', D), ('bar', A)) == \
               pytis.data.reversed_sorting((('foo', A), ('bar', D)))

class ColumnSpec(unittest.TestCase):
    _test_instance = pytis.data.ColumnSpec('foo', pytis.data.Integer())
    def test_class_(self):
        assert ColumnSpec._test_instance.id() == 'foo', 'invalid id'
        assert ColumnSpec._test_instance.type() == pytis.data.Integer(),\
               'invalid type'
    def test_cmp(self):
        x = pytis.data.ColumnSpec('foo', pytis.data.Integer())
        y = pytis.data.ColumnSpec('bar', pytis.data.Integer())
        z = pytis.data.ColumnSpec('foo', pytis.data.String())
        assert self._test_instance == x, 'comparison error'
        assert self._test_instance != y, 'comparison error'
        assert self._test_instance != z, 'comparison error'
tests.add(ColumnSpec)


class Row(unittest.TestCase):
    def test_empty(self):
        r = pytis.data.Row()
        assert len(r) == 0, 'invalid length'
    def test_nonempty(self):
        v1 = pytis.data.Value(pytis.data.Integer(), 1)
        v2 = pytis.data.Value(pytis.data.String(), 'prvni prvek')
        v3 = pytis.data.Value(pytis.data.Integer(), 2)
        r = pytis.data.Row((('poradi', v1), ('popis', v2)))
        assert len(r) == 2, 'invalid length'
        assert r[0] == v1 and r[1] == v2, 'numeric indexing failed'
        assert r[-2] == v1 and r[-1] == v2, 'numeric indexing failed'
        assert r['poradi'] == v1 and r['popis'] == v2, 'string indexing failed'
        for key in (-3, 2, '', 'pop', None, self):
            try:
                r[key]
            except:
                pass
            else:
                fail(('exception not thrown', key))
        r[0] = r['popis'] = v3
        assert r['poradi'] == r[1] == v3, 'value not set'
        r[0:2] = (v2, v1)
        assert r[0] == v2 and r[1] == v1, 'set slice not working'
        x1, x2 = r[0:2]
        assert x1 == v2 and x2 == v1, 'get slice not working'
        assert r[0:1][0] == v2 and r[1:2][0] == v1, 'get slice not working'
    def test_columns(self):
        v1 = pytis.data.Value(pytis.data.Integer(), 1)
        v2 = pytis.data.Value(pytis.data.String(), 'prvni prvek')
        v3 = pytis.data.Value(pytis.data.Integer(), 2)
        r = pytis.data.Row((('poradi', v1), ('popis', v2), ('cislo', v3)))
        assert r.columns(()) == ()
        assert r.columns(('poradi', 'cislo')) == (v1, v3)
    def test_update(self):
        v1 = pytis.data.Value(pytis.data.Integer(), 1)
        v2 = pytis.data.Value(pytis.data.String(), 'prvni prvek')
        r = pytis.data.Row((('poradi', v1), ('popis', v2)))
        u1 = pytis.data.Value(pytis.data.Integer(), 8)
        r2 = pytis.data.Row((('poradi', u1),))
        r.update(r2)
        assert r[0] == u1 and r[1] == v2, 'row update failed'
    def test_append(self):
        V = pytis.data.Value
        I = pytis.data.Integer()
        r = pytis.data.Row((('x', V(I, 1)), ('y', V(I,2))))
        r.append('z', V(I, 3))
        assert r['x'].value() == 1
        assert r['y'].value() == 2
        assert r['z'].value() == 3
tests.add(Row)


class Data(unittest.TestCase):
    def setUp(self):
        c1 = self._column1 = pytis.data.ColumnSpec('foo',
                                                 pytis.data.Integer())
        c2 = self._column2 = pytis.data.ColumnSpec('bar',
                                                 pytis.data.String())
        self._value = pytis.data.Value(pytis.data.Type(), None)
        self._row = pytis.data.Row()
        self._data = pytis.data.Data((c1, c2), c1)
    def test_it(self):
        c1, c2 = self._column1, self._column2
        r = self._row
        v = self._value
        d = self._data
        assert d.columns() == (c1, c2), 'columns lost'
        assert d.find_column('bar') == c2, 'column lost'
        assert d.find_column('foobar') == None, 'imaginary column'
        assert d.key() == (c1,), 'key lost'
        assert d.row(v) == None, 'row not working'
        assert d.select() == 0, 'select not working'
        assert d.fetchone() == None, 'fetchone not working'
        assert d.insert(r) == (None, False), 'insert not working'
        assert d.update(v, r) == (None, False), 'update not working'
        assert d.delete(v) == 0, 'delete not working'
    def test_row_key(self):
        v1 = pytis.data.Value(pytis.data.Integer(), 1)
        v2 = pytis.data.Value(pytis.data.String(), 'xxx')
        row = pytis.data.Row((('foo', v1), ('bar', v2)))
        assert self._data.row_key(row) == (v1,)
tests.add(Data)


class MemData(unittest.TestCase):
    def setUp(self):
        columns = (pytis.data.ColumnSpec('a', pytis.data.String()),
                   pytis.data.ColumnSpec('b', pytis.data.String()),
                   pytis.data.ColumnSpec('x', pytis.data.Integer()),
                   pytis.data.ColumnSpec('y', pytis.data.Integer()))
        data = [pytis.data.Row([(c.id(), pytis.data.Value(c.type(), v))
                                for c,v in zip(columns, values)])
                for values in (('aa','Bob',   1, 10),
                               ('bb','John',  5, 27),
                               ('cc','Will',  3, 2),
                               ('dd','Bill',  3, 42),
                               ('ee','John',  5, 12),
                               ('ff','Joe',   5, 31),
                               ('gg','Eddie', 12, 10))]
        d = pytis.data.DataFactory(pytis.data.MemData, columns, data=data)
        self._data = d.create()
    def _check_condition(self, cond, count):
        c = self._data.select(condition=cond)
        assert c == count, "Expected %d, got %d" % (count, c)
    def test_conditions(self):
        i = lambda v: pytis.data.Value(pytis.data.Integer(), v)
        s = lambda v: pytis.data.Value(pytis.data.String(), v)
        self._check_condition(pytis.data.EQ('a', s('AA')), 0)
        self._check_condition(pytis.data.EQ('a', s('AA'), ignore_case=True), 1)
        self._check_condition(pytis.data.NE('x', i(5)), 4)
        self._check_condition(pytis.data.GT('x', i(3)), 4)
        self._check_condition(pytis.data.LE('x', i(3)), 3)
        self._check_condition(pytis.data.GE('x', 'y'), 2)
    def test_fetch(self):
        v = pytis.data.Value(pytis.data.Integer(), 3)
        c = self._data.select(pytis.data.EQ('x', v))
        assert c == 2, c
        rows = []
        while True:
            row = self._data.fetchone()
            if row is None:
                break
            rows.append(row)
        assert len(rows) == 2, len(rows)
        assert rows[0]['b'].value() == 'Will', rows[0]['b'].value()
        assert rows[1]['b'].value() == 'Bill', rows[1]['b'].value()
tests.add(MemData)


class DataFactory(unittest.TestCase):
    def setUp(self):
        c1 = self._column1 = pytis.data.ColumnSpec('foo',
                                                 pytis.data.Integer())
        c2 = self._column2 = pytis.data.ColumnSpec('bar',
                                                 pytis.data.String())
        self._columns = (c1, c2)
    def test_basic(self):
        columns = self._columns
        key = (columns[1],)
        factory = pytis.data.DataFactory(pytis.data.Data, columns, key)
        factory2 = pytis.data.DataFactory(pytis.data.Data, columns, key=key)
        data1 = factory.create()
        data2 = factory.create()
        data3 = factory2.create()
        assert data1 is not data2
        for d in data1, data2, data3:
            assert d.columns() == columns
            assert d.key() == key, ("Key doesn't match", d.key(), key)
    def test_create(self):
        columns = self._columns
        key = (columns[0],)
        factory = pytis.data.DataFactory(pytis.data.Data, columns,
                                       key=(columns[1],))
        data = factory.create(key=key)
        assert data.columns() == columns
        assert data.key() == key
tests.add(DataFactory)



#############
# dbdata.py #
#############


class DBConnection(unittest.TestCase):
    def setUp(self):
        C = pytis.data.DBConnection
        self._connection = C(user='login', password='heslo',
                             host='localhost', port=1234, database='db')
        self._connection2 = C(user='login', password='heslo',
                              host='localhost', port=1234, database='db')
        self._connection3 = C(user='login', password='heslo',
                              host='localhost', port=1234, database='xxx')
    def test_it(self):
        c = self._connection
        assert c.user() == 'login', 'invalid user'
        assert c.password() == 'heslo', 'invalid password'
        assert c.host() == 'localhost', 'invalid host'
        assert c.port() == 1234, 'invalid port'
        assert c.database() == 'db', 'invalid database'
    def test_cmp(self):
        assert self._connection == self._connection2, \
               'connections don\'t equal'
        assert self._connection != self._connection3, 'connections equal'
    def test_modified(self):
        c = self._connection
        cc = c.modified(host='remotehost')
        assert c.user() == cc.user()
        assert c.password() == cc.password()
        assert c.port() == cc.port()
        assert c.database() == cc.database()
        assert c.host() == 'localhost'
        assert cc.host() == 'remotehost'
tests.add(DBConnection)


class DBBinding(unittest.TestCase):
    def test_it(self):
        b = pytis.data.DBBinding('foo')
        assert b.id() == 'foo', 'id lost'
tests.add(DBBinding)


class DBColumnBinding(unittest.TestCase):
    def test_defaults(self):
        b = pytis.data.DBColumnBinding('bar', 'tabulka', 'sloupec')
        assert b.id() == 'bar', 'id lost'
        assert b.table() == 'tabulka', 'table lost'
        assert b.column() == 'sloupec', 'column lost'
        assert b.related_to() == None, 'intruding relation'
        assert not b.is_hidden(), 'secret column'
    def test_specified(self):
        b1 = pytis.data.DBColumnBinding('', 'ciselnik', 'id')
        assert b1.id() == '', 'id lost'
        assert b1.table() == 'ciselnik', 'table lost'
        assert b1.column() == 'id', 'column lost'
        assert b1.related_to() == None, 'intruding relation'
        assert b1.is_hidden(), 'public column'
        b2 = pytis.data.DBColumnBinding('foo', 'tabulka', 'sloupec',
                                    related_to=b1)
        assert b2.id() == 'foo', 'id lost'
        assert b2.table() == 'tabulka', 'table lost'
        assert b2.column() == 'sloupec', 'column lost'
        assert b2.related_to() == b1, 'relation lost'
        assert not b2.is_hidden(), 'secret column'
tests.add(DBColumnBinding)


class DBExceptions(unittest.TestCase):
    def test_constructors(self):
        e = Exception()
        de = pytis.data.DBException('message', e, 'bla bla', 4)
        assert de.message() == 'message'
        assert de.exception() == e
        de = pytis.data.DBUserException('message')
        assert de.exception() == None
        de = pytis.data.DBSystemException(None)
        m = de.message()
        assert type(m) == type('') and len(m) > 0, ('Invalid message', m)
        de = pytis.data.DBLoginException()
        m = de.message()
        assert type(m) == type('') and len(m) > 0, ('Invalid message', m)
tests.add(DBExceptions)


class DBData(unittest.TestCase):
    def test_it(self):
        b1 = pytis.data.DBBinding('foo')
        b2 = pytis.data.DBBinding('bar')
        bindings = (b1, b2)
        d = pytis.data.DBData(bindings)
        assert map(lambda c: c.id(), d.columns()) == ['foo', 'bar'], \
               ('invalid columns', d.columns())
        assert len(d.key()) == 1, ('invalid number of keys', d.key())
        assert d.key()[0].id() == 'foo', ('invalid key', d.key()[0])
tests.add(DBData)
    

class _DBBaseTest(unittest.TestCase):
    def __init__(self, *args, **kwargs):
        super(_DBBaseTest, self).__init__(*args, **kwargs)
        self._dconnection = pytis.data.DBConnection(**_connection_data)
        import psycopg2
        self._connector = psycopg2.connect(**_connection_data)
    def _sql_command(self, command):
        cursor = self._connector.cursor()
        try:
            cursor.execute(command)
        except Exception, e:
            try:
                self._connector.rollback()
                cursor.close()
            except:
                pass
            raise e
        try:
            result = cursor.fetchall()
        except:
            result = ()
        self._connector.commit()
        cursor.close()
        return result
    def setUp(self):
        pass
    def tearDown(self):
        pass

class _DBTest(_DBBaseTest):
    def setUp(self):
        _DBBaseTest.setUp(self)
        c = self._connector
        for q in ("create table cstat (stat char(2) PRIMARY KEY, nazev varchar(40) UNIQUE NOT NULL) with oids",
                  "create table cosnova (id serial PRIMARY KEY, synte char(3), anal char(3), popis varchar(40), druh char(1) NOT NULL CHECK (druh IN ('X','Y')), stat char(2) REFERENCES cstat, danit boolean NOT NULL DEFAULT 'TRUE') with oids",
                  "create table denik (id int PRIMARY KEY, datum date NOT NULL DEFAULT now(), castka decimal(15,2) NOT NULL, madati int NOT NULL DEFAULT 1 REFERENCES cosnova) with oids",
                  "create table xcosi(id int, popis varchar(12)) with oids",
                  "create table bin(id int, data bytea) with oids",
                  "insert into cstat values('us', 'U.S.A.')",
                  "insert into cstat values('cz', 'Czech Republic')",
                  "insert into cosnova values(1, '100', '007', 'abcd', 'X', 'us', 'FALSE')",
                  "insert into cosnova values(2, '100', '008', 'ijkl', 'X', 'cz', 'FALSE')",
                  "insert into cosnova values(3, '101', '   ', 'efgh', 'Y', 'us')",
                  "insert into denik (id, datum, castka, madati) values(1, '2001-01-02', '1000.00', 1)",
                  "insert into denik (id, datum, castka, madati) values(2, '2001-01-02', '1000.00', 1)",
                  "insert into denik (id, datum, castka, madati) values(3, '2001-01-02', '2000.00', 2)",
                  "insert into denik (id, datum, castka, madati) values(4, '2001-01-04', '3000.00', 3)",
                  "insert into xcosi values(2, 'specialni')",
                  "insert into xcosi values(3, 'zvlastni')",
                  "insert into xcosi values(5, 'nove')",
                  "insert into xcosi values(999, NULL)",
                  "create table viewtest2 (x int) with oids",
                  "create view viewtest1 as select oid, * from viewtest2",
                  "create rule viewtest1_update as on update to viewtest1 do instead update viewtest2 set x=new.x;",
                  "insert into viewtest2 values (1)",
                  "insert into viewtest2 values (2)",
                  ):
            try:
                self._sql_command(q)
            except:
                self.tearDown()
                raise 
    def tearDown(self):
        c = self._connector
        for t in ('viewtest1',):
            try:
                self._sql_command('drop view %s' % t)
            except:
                pass            
        for t in ('bin', 'xcosi', 'denik', 'cosnova', 'cstat', 'viewtest2'):
            try:
                self._sql_command('drop table %s' % t)
            except:
                pass
        _DBBaseTest.tearDown(self)        

class DBDataPostgreSQL(_DBTest):
    # Testujeme v rámci testování potomka 'DBDataDefault'.
    pass

class DBDataPyPgSQL(_DBTest):
    # Testujeme v rámci testování potomka 'DBDataDefault'.
    pass

class PostgreSQLStandardBindingHandler(_DBTest):
    # Testujeme v rámci testování potomka 'DBDataDefault'.
    pass

class DBDataDefault(_DBTest):
    ROW1 = (2, DT.DateTimeFrom('2001-01-02'), 1000.0, 'U.S.A.', 'specialni')
    ROW2 = (3, DT.DateTimeFrom('2001-01-02'), 2000.0, 'Czech Republic',
            'zvlastni')
    ROW3 = ('5', '2001-07-06', '9.9', 'U.S.A.', 'nove')
    NEWROW = ('5', '2001-07-06', '9.90', 'U.S.A.', 'specialni')
    
    def setUp(self):
        _DBTest.setUp(self)
        B = pytis.data.DBColumnBinding
        conn = self._dconnection
        # stat
        key = B('stat', 'cstat', 'stat')
        dstat_spec = pytis.data.DataFactory(
            pytis.data.DBDataDefault,
            (key, (B('nazev', 'cstat', 'nazev'))),
            key)
        dstat = dstat_spec.create(connection_data=conn)
        dstat1_spec = pytis.data.DataFactory(
            pytis.data.DBDataDefault,
            (key, (B('nazev', 'cstat', 'nazev'))),
            key)
        dstat1 = dstat_spec.create(connection_data=conn)
        # osnova
        key = B('id', 'cosnova', 'id')
        dosnova_spec = pytis.data.DataFactory(
            pytis.data.DBDataDefault,
            (key,
             B('synt', 'cosnova', 'synte'), B('anal', 'cosnova', 'anal'),
             B('popis', 'cosnova', 'popis'),
             B('druh', 'cosnova', 'druh'),
             B('stat', 'cosnova', 'stat', enumerator=dstat_spec),
             B('danit', 'cosnova', 'danit')),
            key)
        dosnova = dosnova_spec.create(connection_data=conn)
        # denik
        cosi = B('', 'xcosi', 'id')
        key = B('cislo', 'denik', 'id', related_to=cosi)
        madati = B('', 'cosnova', 'id')
        stat = B('', 'cstat', 'stat')
        d = pytis.data.DBDataDefault(
            (key,
             B('datum', 'denik', 'datum',
               type_=pytis.data.Date(format=pytis.data.Date.DEFAULT_FORMAT)),
             B('castka', 'denik', 'castka'),
             B('', 'denik', 'madati',
               related_to=madati, enumerator=dosnova_spec),
             B('', 'cosnova', 'stat', related_to=stat, enumerator=dstat_spec),
             B('stat-nazev', 'cstat', 'nazev'),
             B('cosi-popis', 'xcosi', 'popis'),
             madati,
             stat,
             cosi),
            key,
            conn)
        key = B('id', 'xcosi', 'id')
        dcosi = pytis.data.DBDataDefault(
            (key,
             B('popis', 'xcosi', 'popis')),
            key,
            conn)
        # bin
        key = B('id', 'bin', 'id')
        dbin = pytis.data.DBDataDefault(
            (key,
             B('data', 'bin', 'data'),),
            key,
            conn)
        # view
        key = B('x', 'viewtest1', 'x')
        view = pytis.data.DBDataDefault((key,), key, conn)
        # atributy
        self.data = d
        #self.mdata = md
        self.dstat = dstat
        self.dstat1 = dstat1
        self.dosnova = dosnova
        self.dcosi = dcosi
        self.dbin = dbin
        self.view = view
        #self._to_kill = [d, md, dstat, dstat1, dosnova, dcosi, view]
        self._to_kill = [d, dstat, dstat1, dosnova, dcosi, view]
        # row data
        row = []
        for c, v in zip(self.data.columns(), self.NEWROW):
            v, e = c.type().validate(v)
            if e is not None:
                raise e
            row.append((c.id(), v))
        self.newrow = pytis.data.Row(row)
    def tearDown(self):
        if hasattr(self, '_to_kill'):
            for d in self._to_kill:
                d.sleep()
        _DBTest.tearDown(self)
    def test_constructor(self):
        # Již otestováno v setUp
        pass
    def test_row(self):
        I = pytis.data.Integer()
        for x in ('0', '1'):
            assert self.data.row((I.validate(x)[0],)) is None,\
                   'nonselectable row selected'
        for x, r in (('2', self.ROW1), ('3', self.ROW2)):
            result = self.data.row((I.validate(x)[0],))
            for i in range(len(result) - 1):
                v = result[i].value()
                assert v == r[i], ('row doesn\'t match', v, r[i])
        result = self.data.row((I.validate('2')[0],),
                               columns=('castka', 'stat-nazev',))
        assert len(result) == 2, ('invalid number of columns', len(result),)
        for i, j in ((0, 2,), (1, 3,)):
            assert result[i] != self.ROW1[j],\
                ('invalid response', i, result[i], self.ROW1[j])
    def test_select_fetch(self):
        self.data.select()
        for r in (self.ROW1, self.ROW2):
            result = self.data.fetchone()
            assert result != None, 'missing lines'
            for i in range(len(r)):
                assert r[i] == result[i].value(), \
                       ('invalid value', r[i], result[i].value())
        assert self.data.fetchone() == None, 'too many lines'
        assert self.data.fetchone() == None, 'data reincarnation'
        self.data.close()
    def test_limited_select(self):
        self.data.select(columns=('castka', 'stat-nazev',))
        for r in (self.ROW1, self.ROW2):
            result = self.data.fetchone()
            assert result != None, 'missing lines'
            for orig_col, result_col in ((2, 0,), (3, 1,),):
                assert r[orig_col] == result[result_col].value(), \
                       ('invalid value', r[orig_col],
                        result[result_col].value())
        assert self.data.fetchone() == None, 'too many lines'
        assert self.data.fetchone() == None, 'data reincarnation'
        self.data.close()
        # Search in limited select OK?
        self.dosnova.select(columns=('id', 'synt', 'anal', 'danit',))
        result = self.dosnova.search(pytis.data.EQ(
                'popis', pytis.data.Value(pytis.data.String(), 'efgh')))
        assert result == 3, ('Invalid search result', result)
        self.dosnova.close()
        # .row in limited search still working?
        self.data.select(columns=('castka', 'stat-nazev',))
        result = self.data.row((pytis.data.Integer().validate('2')[0],))
        for i in range(len(result) - 1):
            v = result[i].value()
            assert v == self.ROW1[i], ('row doesn\'t match', v, r[i])
        self.data.close()
    def test_select_map(self):
        result = self.data.select_map(lambda row: (row, 'foo'))
        for r, x in zip((self.ROW1, self.ROW2), result):
            assert x[1] == 'foo'
            xx = x[0]
            for i in range(len(r)):
                assert r[i] == xx[i].value(), \
                       ('invalid value', r[i], xx[i].value())
    def test_select_fetch_direction(self):
        self.data.select()
        F, B = pytis.data.FORWARD, pytis.data.BACKWARD
        R1, R2 = self.ROW1, self.ROW2
        n = 0
        for d, r in ((B, None), (F, R1), (B, None), (F, R1), (F, R2),
                     (B, R1), (F, R2), (F, None), (F, None), (B, R2), (B, R1),
                     (B, None)):
            result = self.data.fetchone(direction=d)
            if r:
                assert result is not None, ('line not received', n)
                for i in range(len(r)):
                    assert r[i] == result[i].value(), \
                           ('invalid value', r[i], result[i].value(), n)
            else:
                assert result is None, ('data reincarnation', str(result),
                                        d, r, n)
            n = n + 1
        self.data.close()
    def test_select_condition(self):
        v = pytis.data.Value(pytis.data.Integer(), 2)
        condition = pytis.data.AND(pytis.data.EQ('cislo', v))
        self.data.select(condition)
        for r in (self.ROW1,):
            result = self.data.fetchone()
            assert result != None, 'missing lines'
            for i in range(len(r)):
                assert r[i] == result[i].value(), \
                       ('invalid value', r[i], result[i].value())
        assert self.data.fetchone() == None, 'too many lines'
        assert self.data.fetchone() == None, 'data reincarnation'
        self.data.close()
        self.data.select(pytis.data.GT('castka', 'cislo'))
        rows = []
        while True:
            row = self.data.fetchone()
            if row is None:
                break
            rows.append(row)
        self.data.close()
        assert len(rows) == 2, len(rows)
        # NULL test
        condition = pytis.data.EQ('popis',
                                pytis.data.Value(pytis.data.String(), None))
        self.dcosi.select(condition)
        n = 0
        while self.dcosi.fetchone():
            n = n + 1
        self.dcosi.close()
        assert n == 1
    def test_select_sorting(self):
        A = pytis.data.ASCENDENT
        D = pytis.data.DESCENDANT
        d = self.dosnova
        TESTS = ([(('synt', D), ('stat',A)),
                  (('101', '   '), ('100', '008'), ('100', '007'))],
                 [(('stat', A), 'synt'),
                  (('100', '008'), ('100', '007'), ('101', '   '))],
                 [('anal', 'synt'),
                  (('101', '   '), ('100', '007'), ('100', '008'))])
        for spec, result in TESTS:
            d.select(sort=spec)
            for r in result:
                row = d.fetchone()
                assert row, 'missing lines'
                k1, k2 = r
                synt, anal = row['synt'].value(), row['anal'].value()
                assert synt == k1, ('bad value', synt, k1, spec)
                assert anal == k2, ('bad value', anal, k2, spec)
            assert not d.fetchone(), 'too many lines'
    def test_select_sorting_limited(self):
        A = pytis.data.ASCENDENT
        D = pytis.data.DESCENDANT
        d = self.dosnova
        limited_columns=('synt', 'anal', 'popis', 'stat',)
        TESTS = ([(('synt', D), ('stat',A)),
                  (('101', '   '), ('100', '008'), ('100', '007'))],
                 [(('stat', A), 'synt'),
                  (('100', '008'), ('100', '007'), ('101', '   '))],
                 [('anal', 'synt'),
                  (('101', '   '), ('100', '007'), ('100', '008'))])
        for spec, result in TESTS:
            d.select(sort=spec, columns=limited_columns)
            for r in result:
                row = d.fetchone()
                assert row, 'missing lines'
                k1, k2 = r
                synt, anal = row['synt'].value(), row['anal'].value()
                assert synt == k1, ('bad value', synt, k1, spec)
                assert anal == k2, ('bad value', anal, k2, spec)
            assert not d.fetchone(), 'too many lines'
    def test_select_aggregate(self):
        d = self.data
        result = d.select_aggregate((d.AGG_MIN, 'castka')).value()
        assert result == 1000, result
        result = d.select_aggregate((d.AGG_MAX, 'castka')).value()
        assert result == 2000, result
        condition = pytis.data.GT('castka',
                                  pytis.data.Value(pytis.data.Float(), 1500))
        result = d.select_aggregate((d.AGG_AVG, 'castka'),
                                    condition=condition).value()
        assert result == 2000, result
        result = d.select_aggregate((d.AGG_COUNT, 'castka')).value()
        assert result == 2, result
        result = d.select_aggregate((d.AGG_SUM, 'castka')).value()
        assert result == 3000, result
    def test_insert(self):
        row = self.newrow
        result, success = self.data.insert(row)
        assert success
        eresult = []
        for c, v in zip(self.data.columns(), self.ROW3):
            eresult.append((c.id(), c.type().validate(v)[0]))
        eresult = pytis.data.Row(eresult)
        assert result[:-1] == eresult, 'insertion failed'
        result2 = self.data.insert(row)
        assert result2[1] is False, 'invalid insertion succeeded'
        assert (result2[0] is None or type(result2[0]) == type('') or
                type(result2[0]) == type(u'')),\
               'invalid failed insertion result'
    def test_update(self):
        row = self.newrow
        row1 = []
        for c, v in zip(self.data.columns(), self.ROW1):
            row1.append((c.id(), pytis.data.Value(c.type(), v)))
        row1 = pytis.data.Row(row1)
        k1 = row1[0]
        k2 = pytis.data.Value(self.data.columns()[0].type(), self.ROW2[0])
        result, success = self.data.update(k1, row)
        assert success
        eresult = []
        for c, v in zip(self.data.columns(), self.ROW3):
            eresult.append((c.id(), c.type().validate(v)[0]))
        eresult = pytis.data.Row(eresult)
        assert result[:-1] == eresult, 'update failed'
        for k in k1, k2:
            result2 = self.data.update(k, row)
            assert result2[1] is False, 'invalid update succeeded'
            assert (result2[0] is None or type(result2[0]) == type('') or
                    type(result2[0]) == type(u'')),\
                    'invalid failed update result'
            result2 = self.data.update(k, row)
        assert self.data.update(row[0], row1)[0][:-1] == row1, 'update failed'
    def test_view_update(self):
        I = pytis.data.Integer()
        key = I.validate('2')[0]
        row = pytis.data.Row((('x', I.validate('3')[0]),))
        result, success = self.view.update(key, row)
        assert result and success, 'view update failed'
    def test_update_many(self):
        row = self.newrow
        row1 = []
        for c, v in zip(self.data.columns(), self.ROW1):
            row1.append((c.id(), pytis.data.Value(c.type(), v)))
        row1 = pytis.data.Row(row1)
        k1 = row1[0]
        k2 = pytis.data.Value(self.data.columns()[0].type(), self.ROW2[0])
        result = self.data.update_many(pytis.data.EQ('cislo', k1), row)
        assert result == 1, 'update failed'
        assert self.data.update_many(pytis.data.EQ('cislo', k1), row) == 0, \
               'invalid update succeeded'
        try:
            ok = True
            self.data.update_many(pytis.data.EQ('cislo', k2), row)
            ok = False
        except pytis.data.DBException:
            pass
        assert ok, 'invalid update succeeded'
        assert self.data.update_many(pytis.data.EQ('cislo', row[0]), row1) \
               == 1, \
               'update failed'
    def test_delete(self):
        def lines(keys, self=self):
            n = len(keys)
            result = self._sql_command('select id from denik order by id')
            assert len(result) == n, ('invalid number of rows', len(result), n)
            for i in range(n):
                v = result[i][0]
                assert keys[i] == v, ('nonmatching key', keys[i], v)
        I = pytis.data.Integer()
        assert self.data.delete(I.validate('0')[0]) == 0, \
               'nonexistent row deleted'
        lines((1, 2, 3, 4))
        assert self.data.delete(I.validate('1')[0]) == 1, \
               'row not deleted'
        lines((2, 3, 4))
        assert self.data.delete(I.validate('1')[0]) == 0, \
               'row deleted twice'
        lines((2, 3, 4))
        assert self.data.delete(I.validate('4')[0]) == 1, \
               'row not deleted'
        lines((2, 3))
    def test_delete_many(self):
        def lines(keys, self=self):
            n = len(keys)
            result = self._sql_command('select id from denik order by id')
            assert len(result) == n, ('invalid number of rows', len(result), n)
            for i in range(n):
                v = result[i][0]
                assert keys[i] == v, ('nonmatching key', keys[i], v)
        x999 = pytis.data.Float().validate('999')[0]
        x1000 = pytis.data.Float().validate('1000')[0]
        x3000 = pytis.data.Float().validate('3000')[0]
        assert self.data.delete_many(pytis.data.EQ('castka', x999)) == 0, \
               'nonexistent row deleted'
        lines((1, 2, 3, 4))
        assert self.data.delete_many(pytis.data.EQ('castka', x1000)) == 2, \
               'rows not deleted'
        lines((3, 4))
        assert self.data.delete_many(pytis.data.EQ('castka', x1000)) == 0, \
               'rows deleted twice'
        lines((3, 4))
        assert self.data.delete_many(pytis.data.EQ('castka', x3000)) == 1, \
               'row not deleted'
        lines((3,))
    def test_binary(self):
        B = pytis.data.Binary()
        I = pytis.data.Integer()
        R = pytis.data.Row
        null_data, error = B.validate(None)
        assert not error, ('Null Binary validation failed', error,)
        assert null_data.value() is None, \
               ('Invalid null binary value', null_data.value())
        data = [chr(i) for i in range(256)]
        data1, error = B.validate(buffer(string.join(data, '')))
        assert not error, ('Binary validation failed', error,)
        assert isinstance(data1.value(), pytis.data.Binary.Buffer), \
               ('Invalid binary value', data1.value(),)
        data.reverse()
        data2, error = B.validate(buffer(string.join(data, '')))
        assert not error, ('Binary validation failed', error,)
        assert isinstance(data2.value(), pytis.data.Binary.Buffer), \
               ('Invalid binary value', data2.value(),)
        key, _error = I.validate('1')
        row1 = R([('id', key,), ('data', data1,)])
        row2 = R([('id', key,), ('data', data2,)])
        result, success = self.dbin.insert(row1)
        assert success, 'Binary insertion failed'
        assert str(result[1].value().buffer()) == str(data1.value().buffer()), \
              ('Invalid inserted binary data', str(result[1].value().buffer()))
        result = str(self.dbin.row(key)[1].value().buffer())
        assert result == str(data1.value().buffer()), ('Invalid binary data', result,)
        result, succes = self.dbin.update(key, row2)
        assert success, 'Binary update failed'
        assert str(result[1].value().buffer()) == str(data2.value().buffer()),\
            ('Invalid updated binary data', str(result[1].value().buffer()),)
        result = str(self.dbin.row(key)[1].value().buffer())
        assert result == str(data2.value().buffer()), \
               ('Invalid binary data', result,)
        assert self.dbin.delete(key) == 1, 'Binary deletion failed'
    def test_lock(self):
        us = pytis.data.String().validate('us')[0]
        cz = pytis.data.String().validate('cz')[0]
        t1, t2 = self.dstat, self.dstat1
        transaction_1 = t1.begin_transaction()
        transaction_2 = t2.begin_transaction()
        try:
            assert t1.lock_row(us, transaction_1) is None, 'lock failed'
            result = t2.lock_row(us, transaction_2)
            assert type(result) == type(''), 'unlocked record locked'
            assert t2.lock_row(cz, transaction_2) is None, 'lock failed'
            t2.rollback_transaction(transaction_2)
            transaction_2 = t2.begin_transaction()
            assert type(t2.lock_row(us, transaction_2)) == type(''), \
                'unlocked record locked'
            t1.commit_transaction(transaction_1)
            transaction_1 = t1.begin_transaction()
            assert t2.lock_row(us, transaction_2) is None, 'lock failed'
            t1.rollback_transaction(transaction_1)
            t2.commit_transaction(transaction_2)
        finally:
            try:
                t1.rollback_transaction(transaction_1)
            except:
                pass
            try:
                t2.rollback_transaction(transaction_2)
            except:
                pass
    def _perform_transaction(self, transaction):
        d = self.dstat
        d1 = self.dstat1
        def v(s):
            return pytis.data.String().validate(s)[0]
        i_row0 = pytis.data.Row((('stat', v('cs'),), ('nazev', v('Cesko'),)))
        i_row00 = pytis.data.Row((('stat', v('cc'),), ('nazev', v('CC'),)))
        d.insert(i_row0)
        d.insert(i_row00)
        i_row1 = pytis.data.Row((('stat', v('xx'),), ('nazev', v('Xaxa'),)))
        i_row2 = pytis.data.Row((('stat', v('yy'),), ('nazev', v('Yaya'),)))
        u_key1 = i_row2[0]
        u_row1 = pytis.data.Row((('nazev', v('Gaga'),),))
        u_condition_2 = pytis.data.EQ('stat', v('cz'))
        u_row2 = pytis.data.Row((('nazev', v ('Plesko'),),))
        d_key = i_row1[0]
        d_condition = pytis.data.EQ('nazev', v('CC'))
        d.insert(i_row1, transaction=transaction)
        d1.insert(i_row2, transaction=transaction)
        d.lock_row(u_key1, transaction)
        d.update(u_key1, u_row1, transaction=transaction)
        d1.update_many(u_condition_2, u_row2, transaction=transaction)
        d.delete(d_key, transaction=transaction)
        d1.delete_many(d_condition, transaction=transaction)
        d.select(sort=('stat',), transaction=transaction)
        for k in ('cs', 'cz', 1, 'yy',):
            if type(k) == type(0):
                d.skip(k)
            else:
                value = d.fetchone(transaction=transaction)[0].value()
                assert value == k, ('invalid select value', k, value,)
        d.close()
    def test_transaction_commit(self):
        d = self.dstat
        transaction = d.begin_transaction()
        try:
            self._perform_transaction(transaction)
        finally:
            d.commit_transaction(transaction)
        for k, v in (('cs', 'Cesko',), ('xx', None,), ('yy', 'Gaga',),
                     ('cz', 'Plesko',), ('cc', None,),):
            result = d.row(pytis.data.String().validate(k)[0])
            if v is None:
                assert result is None, ('deleted value present', k,)
            else:
                assert result is not None, ('value not present', k,)
                assert result['nazev'].value() == v, \
                    ('invalid value', k, result['nazev'].value(),)
    def test_transaction_rollback(self):
        d = self.dstat
        transaction = d.begin_transaction()
        try:
            self._perform_transaction(transaction)
        finally:
            d.rollback_transaction(transaction)
        for k, v in (('cs', 'Cesko',), ('xx', None,), ('yy', None,),
                     ('cz', 'Czech Republic',), ('cc', 'CC',),):
            result = d.row(pytis.data.String().validate(k)[0])
            if v is None:
                assert result is None, ('deleted value present', k,)
            else:
                assert result is not None, ('value not present', k,)
                assert result['nazev'].value() == v, \
                    ('invalid value', k, result['nazev'].value(),)
    def test_partial_transaction(self):
        d = self.dstat
        def v(s):
            return pytis.data.String().validate(s)[0]
        row1 = pytis.data.Row((('stat', v('xx'),), ('nazev', v('Xaxa'),),))
        row2 = pytis.data.Row((('stat', v('yy'),), ('nazev', v('Yaya'),),))
        row3 = pytis.data.Row((('stat', v('zz'),), ('nazev', v('Zaza'),),))
        transaction = d.begin_transaction()
        d.set_transaction_point(transaction, 'xxx')
        d.insert(row1, transaction=transaction)
        d.set_transaction_point(transaction, 'yyy')
        d.insert(row2, transaction=transaction)
        d.set_transaction_point(transaction, 'zzz')
        d.cut_transaction(transaction, 'yyy')
        d.insert(row3, transaction=transaction)
        d.set_transaction_point(transaction, 'ooo')
        d.commit_transaction(transaction=transaction)
        assert d.row(row1['stat']), 'missing row'
        assert d.row(row2['stat']) is None, 'extra row'
        assert d.row(row3['stat']), 'missing row'
tests.add(DBDataDefault)


class DBMultiData(DBDataDefault):
    ROW1 = (2, DT.DateTimeFrom('2001-01-02'), 1000.0, ('100','007'),
            'U.S.A.', 'specialni')
    ROW2 = (3, DT.DateTimeFrom('2001-01-02'), 2000.0, ('100','008'),
            'Czech Republic', 'zvlastni')
    ROW3 = ('5', '2001-07-06', '9.9', ('100', '007'), 'U.S.A.', 'nove')
    def test_row(self):
        for x, r in (('2', self.ROW1), ('3', self.ROW2)):
            result = self.mdata.row(pytis.data.Integer().validate(x)[0])
            for i in range(len(result) - 1):
                v = result[i].value()
                if type(v) == type(()):
                    for j in range(len(v)):
                        assert v[j] == r[i][j], \
                               ('row doesn\'t match', v[i][j], r[i][j])
                else:
                    assert v == r[i], ('row doesn\'t match', v, r[i])
    def test_select_fetch(self):
        d = self.mdata
        d.select()
        for r in (self.ROW1, self.ROW2):
            result = d.fetchone()
            assert result != None, 'missing lines'
            for i in range(len(r)):
                assert r[i] == result[i].value(), \
                       ('invalid value', r[i], result[i].value())
        d.close()
    def test_select_condition(self):
        d = self.mdata
        v = pytis.data.Value(pytis.data.Integer(), 2)
        condition = pytis.data.AND(pytis.data.EQ('cislo', v))
        d.select(condition)
        for r in (self.ROW1,):
            result = d.fetchone()
            assert result != None, 'missing lines'
            for i in range(len(r)):
                assert r[i] == result[i].value(), \
                       ('invalid value', r[i], result[i].value())
        assert d.fetchone() == None, 'too many lines'
        d.close()
    def test_select_fetch_direction(self):
        dat = self.mdata
        dat.select()
        F, B = pytis.data.FORWARD, pytis.data.BACKWARD
        R1, R2 = self.ROW1, self.ROW2
        n = 0
        for d, r in ((B, None), (F, R1), (B, None), (F, R1), (F, R2),
                     (B, R1), (F, R2), (F, None), (F, None), (B, R2), (B, R1),
                     (B, None)):
            result = dat.fetchone(direction=d)
            if r:
                assert result != None, ('line not received', n)
                for i in range(len(r)):
                    assert r[i] == result[i].value(), \
                           ('invalid value', r[i], result[i].value(), n)
            else:
                assert result == None, 'data reincarnation'
            n = n + 1
        dat.close()
    def test_search(self):
        E = pytis.data.EQ
        V = pytis.data.Value
        S = pytis.data.String()
        d = self.dosnova
        d.select()
        res = d.search(E('popis', V(S, 'efgh')))
        assert res == 3, ('Invalid search result', res)
        res = d.search(E('popis', V(S, 'abcd')), direction=pytis.data.FORWARD)
        assert res == 1, ('Invalid search result', res)
        res = d.search(E('popis', V(S, 'foo')))
        assert res == 0, ('Invalid search result', res)
        d.fetchone()
        res = d.search(E('popis', V(S, 'efgh')))
        assert res == 2, ('Invalid search result', res)
        res = d.search(E('popis', V(S, 'abcd')))
        assert res == 0, ('Invalid search result', res)
        res = d.search(E('popis', V(S, 'foo')))
        assert res == 0, ('Invalid search result', res)
        d.fetchone()
        res = d.search(E('popis', V(S, 'abcd')), direction=pytis.data.FORWARD)
        assert res == 0, ('Invalid search result', res)
        res = d.search(E('popis', V(S, 'abcd')),
                       direction=pytis.data.BACKWARD)
        assert res == 1, ('Invalid search result', res)
        while d.fetchone() is not None:
            pass
        res = d.search(E('popis', V(S, 'abcd')), direction=pytis.data.FORWARD)
        assert res == 0, ('Invalid search result', res)
        res = d.search(E('popis', V(S, 'abcd')),
                       direction=pytis.data.BACKWARD)
        assert res == 3, ('Invalid search result', res)        
        d.close()
    def test_search_key(self):
        V = pytis.data.Value
        S = pytis.data.String()
        d = self.dosnova
        d.select()
        res = d.search_key((V(S, '100'), V(S, '008')))
        assert res == 2, ('Invalid search result', res)
        d.close()
    def test_insert(self):
        d = self.mdata
        row = self.newrow
        result, success = d.insert(row)
        assert success
        eresult = []
        for c, v in zip(d.columns(), self.ROW3):
            eresult.append((c.id(), c.type().validate(v)[0]))
        eresult = pytis.data.Row(eresult)
        assert result[:-1] == eresult, 'insertion failed'
        assert d.insert(row) == (None, False), 'invalid insertion succeeded'
    def test_update(self):
        d = self.mdata
        newrow = ('5', '2001-07-06', '9.90', ('100', '008'), 'Czech Republic',
                  'nove')
        rowdata = []
        for c, v in zip(d.columns(), newrow):
            rowdata.append((c.id(), c.type().validate(v)[0]))
        row = pytis.data.Row(rowdata)
        row1 = []
        for c, v in zip(d.columns(), self.ROW1):
            row1.append((c.id(), pytis.data.Value(c.type(), v)))
        row1 = pytis.data.Row(row1)
        k1 = row1[0]
        k2 = pytis.data.Value(d.columns()[0].type(), self.ROW2[0])
        result, success = d.update(k1, row)
        assert success
        eresult = []
        for c, v in zip(d.columns(), newrow):
            eresult.append((c.id(), c.type().validate(v)[0]))
        eresult = pytis.data.Row(eresult)
        assert result[:-1] == eresult, 'update failed'
        assert d.update(k1, row) == (None, False), 'invalid update succeeded'
        assert d.update(k2, row) == (None, False), 'invalid update succeeded'
        assert d.update(row[0], row1)[0][:-1] == row1, 'update failed'
    def test_delete(self):
        d = self.mdata
        def lines(keys, self=self):
            n = len(keys)
            result = self._sql_command('select id from denik order by id')
            assert len(result) == n, ('invalid number of rows', len(result), n)
            for i in range(n):
                v = result[i][0]
                assert keys[i] == v, ('nonmatching key', keys[i], v)
        assert d.delete(pytis.data.Integer().validate('0')[0]) == 0, \
               'nonexistent column deleted'
        lines((1, 2, 3, 4))
        assert d.delete(pytis.data.Integer().validate('1')[0]) == 1, \
               'column not deleted'
        lines((2, 3, 4))
        assert d.delete(pytis.data.Integer().validate('1')[0]) == 0, \
               'column deleted twice'
        lines((2, 3, 4))
        assert d.delete(pytis.data.Integer().validate('4')[0]) == 1, \
               'column not deleted'
        lines((2, 3))
#tests.add(DBMultiData)


class DBDataFetchBuffer(_DBBaseTest):
    def setUp(self):
        _DBBaseTest.setUp(self)
        c = self._connector
        import config
        try:
            self._sql_command("create table big (x int) with oids")
            table_size = config.initial_fetch_size + config.fetch_size + 10
            self._table_size = table_size
            for i in range(table_size):
                self._sql_command("insert into big values(%d)" % i)
            self._sql_command("create table small (x int) with oids")
            for i in range(4):
                self._sql_command('insert into "small" values(%d)' % i)
        except:
            self.tearDown()
            raise
        key = pytis.data.DBColumnBinding('x', 'big', 'x')
        self.data = \
          pytis.data.DBDataDefault((key,), key, self._dconnection)
        key2 = pytis.data.DBColumnBinding('x', 'small', 'x')
        self.data2 = \
          pytis.data.DBDataDefault((key2,), key2, self._dconnection)
    def tearDown(self):
        try:
            self.data.sleep()
        except:
            pass
        try:
            self._sql_command('drop table "big"')
            self._sql_command('drop table "small"')
        except:
            pass
        _DBBaseTest.tearDown(self)
    def _check_skip_fetch(self, d, spec, noresult=False):
        d.select()
        n = 0
        for op, count in spec:
            n = n + count
            if count >= 0:
                direction = pytis.data.FORWARD
            else:
                direction = pytis.data.BACKWARD
                count = -count
            if op == 'f':
                for i in range(count):
                    d.fetchone(direction=direction)
            elif op == 's':
                d.skip(count, direction=direction)
            else:
                raise Exception('Invalid op', op)
        row = d.fetchone()
        if noresult:
            assert row == None, ('Extra result', str(row))
        else:
            assert row, ('Missing row', n)
            assert row['x'].value() == n, ('Invalid result', str(row), n)
    def test_skip_fetch(self):
        import config
        fsize = config.initial_fetch_size
        fsize2 = fsize + config.fetch_size
        tsize = self._table_size
        d1 = self.data
        d2 = self.data2
        self._check_skip_fetch(d1, (('s', tsize-1), ('f', 1), ('s', -2),
                                    ('f',-1)))
        self._check_skip_fetch(d1, (('f',12), ('s',42)))
        self._check_skip_fetch(d1, (('f',12), ('s',42), ('f',10)))
        self._check_skip_fetch(d1, (('f',12), ('s',fsize)))
        self._check_skip_fetch(d1, (('f',12), ('s',fsize+1), ('f',5)))
        self._check_skip_fetch(d1, (('f',12), ('s',-6), ('f',2)))
        self._check_skip_fetch(d1, (('f',fsize+10), ('s',-16)))
        self._check_skip_fetch(d1, (('f',fsize+10), ('s',-16), ('s',20),
                                    ('f',-10), ('f',15)))
        self._check_skip_fetch(d1, (('s',fsize2+3),))
        self._check_skip_fetch(d1, (('s',10*fsize2),), noresult=True)
        # small table
        # Z neznámého důvodu to při ukončení vytuhne (testy ale proběhnou bez
        # problémů...  TODO: Co s tím??
        #self._check_skip_fetch(d2, (('s', 3), ('f', 1), ('s', -2), ('f',-1)))
        #self._check_skip_fetch(d2, (('s', 4), ('f', 1), ('s', -2), ('f',-1)))
tests.add(DBDataFetchBuffer)


class DBDataReuse(DBDataFetchBuffer):
    def test_it(self):
        d = self.data
        d.select()
        import config
        skip = config.initial_fetch_size
        d.skip(skip)
        for i in range(3):
            d.fetchone()
        d.select(reuse=True)
        d.skip(skip)
        row = d.fetchone()
        assert row, ('Missing row', skip)
        assert row['x'].value() == skip, ('Invalid result', str(row), skip)
        d.select(reuse=True)
        row = d.fetchone()
        assert row['x'].value() == 0, ('Invalid result', str(row), 0)
tests.add(DBDataReuse)


class DBDataOrdering(_DBTest):
    def setUp(self):
        super_(DBDataOrdering).setUp(self)
        B = pytis.data.DBColumnBinding
        key = B('id', 'xcosi', 'id')
        self.data = pytis.data.DBDataDefault(
            (key, B('popis', 'xcosi', 'popis')),
            key,
            self._dconnection,
            ordering='id')
    def tearDown(self):
        try:
            self.data.sleep()
        except:
            pass
        super_(DBDataOrdering).tearDown(self)
    def test_insert(self):
        row = pytis.data.Row((('popis',
                         pytis.data.Value(pytis.data.String(), 'bla bla')),))
        d = self.data
        key = (pytis.data.Value(pytis.data.Integer(), 3),)
        assert d.insert(row, after=key)[1], 'Insert failed'
        d.select()
        d.fetchone()
        result = d.fetchone()
        assert result['popis'].value() == 'zvlastni', \
               ('Unexpected value', result['popis'].value())
        result = d.fetchone()
        assert result['popis'].value() == 'bla bla', \
               ('Unexpected value', result['popis'].value())
        value = result['id'].value()
        assert value > 3 and value < 6, ('Invalid ordering value', value)
        assert d.insert(row, before=key)[1], 'Insert failed'
        d.select()
        d.fetchone()
        result = d.fetchone()
        assert result['popis'].value() == 'bla bla', \
               ('Unexpected value', result['popis'].value())
        result = d.fetchone()
        assert result['popis'].value() == 'zvlastni', \
               ('Unexpected value', result['popis'].value())
        result = d.fetchone()
        assert result['popis'].value() == 'bla bla', \
               ('Unexpected value', result['popis'].value())
tests.add(DBDataOrdering)


class DBDataNotification(DBDataDefault):
    def setUp(self):
        DBDataDefault.setUp(self)
        self.data.add_callback_on_change(self._ddn_callback_1)
        self.data.add_callback_on_change(self._ddn_callback_2)
        self.data.add_callback_on_change(self._ddn_callback_3)
        self.data.remove_callback_on_change(self._ddn_callback_2)
        self._ddn_1 = False
        self._ddn_2 = False
        self._ddn_3 = False
    def _ddn_callback_1(self):
        self._ddn_1 = True
    def _ddn_callback_2(self):
        self._ddn_2 = True
    def _ddn_callback_3(self):
        self._ddn_3 = True
    def _ddn_check_result(self):
        time.sleep(1)                   # hmm
        assert self._ddn_1, 'failure of callback 1'
        assert not self._ddn_2, 'failure of callback 2'
        assert self._ddn_3, 'failure of callback 3'        
    def test_notification(self):
        d = self.data
        assert d.change_number() == 0
        d.insert(self.newrow)
        self._ddn_check_result()
        assert d.change_number() == 1
    def test_side_notification(self):
        d = self.dstat
        assert d.change_number() == 0
        d.insert(pytis.data.Row(
            (('stat', d.columns()[0].type().validate('at')[0]),
             ('nazev', d.columns()[1].type().validate('Austria')[0]))))
        self._ddn_check_result()
        assert d.change_number() == 1
        assert self.data.change_number() == 1
tests.add(DBDataNotification)


class DBCounter(_DBBaseTest):
    def setUp(self):
        _DBBaseTest.setUp(self)
        c = self._connector
        for q in ("create sequence foo",):
            try:
                self._sql_command(q)
            except:
                self.tearDown()
                raise
        self._counter = pytis.data.DBCounterDefault('foo', self._dconnection)
    def tearDown(self):
        c = self._connector
        for q in ("drop sequence foo",):
            try:
                self._sql_command(q)
            except:
                pass
        _DBBaseTest.tearDown(self)
    def test_next(self):
        assert self._counter.next() == 1
        assert self._counter.next() == 2
tests.add(DBCounter)


class DBFunction(_DBBaseTest):
    def setUp(self):
        _DBBaseTest.setUp(self)
        c = self._connector
        try:
            self._sql_command("create table tab (x int)")
            for q in ("foo1(int) returns int as 'select $1+1'",
                      "foo2(text,text) returns text as 'select $1 || $2'",
                      "foo3() returns int as 'select min(x) from tab'"):
                self._sql_command("create function %s language sql " % q)
        except:
            self.tearDown()
            raise
    def tearDown(self):
        c = self._connector
        try:
            self._sql_command("drop table tab")
        except:
            pass
        for q in ("foo1(int)", "foo2(text,text)", "foo3()"):
            try:
                self._sql_command("drop function %s" % q)
            except:
                pass
        _DBBaseTest.tearDown(self)
    def test_int(self):
        function = pytis.data.DBFunctionDefault('foo1', self._dconnection)
        row = pytis.data.Row((('arg1',
                             pytis.data.Integer().validate('41')[0]),
                            ))
        result = function.call(row)[0][0].value()
        assert result == 42, ('Invalid result', result)
    def test_string(self):
        function = pytis.data.DBFunctionDefault('foo2', self._dconnection)
        row = pytis.data.Row((('arg1',
                             pytis.data.String().validate('foo')[0]),
                            ('arg2',
                             pytis.data.String().validate('bar')[0])))
        result = function.call(row)[0][0].value()
        assert result == 'foobar', ('Invalid result', result)
    def test_empty(self):
        function = pytis.data.DBFunctionDefault('foo3', self._dconnection)
        row = pytis.data.Row(())
        result = function.call(row)[0][0].value()
        assert result is None, ('Invalid result', result)
tests.add(DBFunction)



###################
# Complex DB test #
###################


class TutorialTest(_DBBaseTest):
    def setUp(self):
        for q in ("CREATE TABLE cis (x varchar(10) PRIMARY KEY, y text) with oids",
                  "CREATE TABLE tab (a int PRIMARY KEY, b varchar(30), "+\
                  "c varchar(10) REFERENCES cis) with oids",
                  "INSERT INTO cis VALUES ('1', 'raz')",
                  "INSERT INTO cis VALUES ('2', 'dva')",
                  "INSERT INTO cis VALUES ('3', 'tri')",
                  "INSERT INTO cis VALUES ('9', 'devet')",
                  "INSERT INTO tab VALUES (1, 'one', '1')",
                  "INSERT INTO tab VALUES (2, 'two', '2')",
                  ):
            self._sql_command(q)
    def tearDown(self):
        c = self._connector
        self._sql_command("DROP TABLE tab")
        self._sql_command("DROP TABLE cis")
    def test_it(self):
        # set up
        connection = pytis.data.DBConnection(**_connection_data)
        def get_connection(connection=connection):
            return connection
        C = pytis.data.DBColumnBinding
        D = pytis.data.DBDataDefault
        cis_key = C('id', 'cis', 'x')
        cis_columns = (cis_key,
                       C('popis', 'cis', 'y'))
        cis_data_spec = pytis.data.DataFactory(D, cis_columns, cis_key)
        cis_data = cis_data_spec.create(connection_data=connection)
        tab_key = C('klic', 'tab', 'a')
        tab_columns = (tab_key,
                       C('popis', 'tab', 'b'),
                       C('id', 'tab', 'c',
                         enumerator=cis_data_spec, value_column='popis'))
        tab_data = D(tab_columns, tab_key, get_connection)
        try:
            # go
            tab_data.select()
            n = 0
            while 1:
                row = tab_data.fetchone()
                if not row:
                    break
                n = n + 1
            assert n == 2, ('invalid number of rows', n)
            tab_data.close()
            old_key = tab_data.columns()[0].type().validate('1')[0]
            assert old_key, 'validation not working'
            new_key = tab_data.columns()[0].type().validate('9')[0]
            assert new_key, 'validation not working'
            new_row_data = []
            for c, v in zip(tab_data.columns(),
                            ('9', 'pěkný řádek', 'devet')):
                new_row_data.append ((c.id(), c.type().validate(v)[0]))
            new_row = pytis.data.Row(new_row_data)
            # TODO: Momenálně nechodí.  Opravit.
            #assert tab_data.insert(new_row)[1], 'line not inserted'
            #assert tab_data.delete(new_key), 'line not deleted'
            #result, success = tab_data.update(old_key, new_row)
            #assert result and success, 'line not updated'
            #assert tab_data.row(new_key), 'new line not found'
        finally:
            # shut down
            cis_data.sleep()
            tab_data.sleep()
tests.add(TutorialTest)


class ThreadTest(_DBBaseTest):
    # This is a non-regular test trying to detect bugs resulting from
    # insufficient thread safety
    def setUp(self):
        _DBBaseTest.setUp(self)
        c = self._connector
        try:
            self._sql_command("create table tab (x int, y int) with oids")
        except:
            self.tearDown()
            raise
    def tearDown(self):
        c = self._connector
        try:
            self._sql_command("drop table tab")
        except:
            pass
        _DBBaseTest.tearDown(self)
    def test_it(self):
        import thread
        B = pytis.data.DBColumnBinding
        key = B('x', 'tab', 'x')
        d = pytis.data.DataFactory(
            pytis.data.DBDataDefault,
            (key, (B('y', 'tab', 'y'))),
            key)
        c = pytis.data.DBConnection(**_connection_data)
        d1 = d.create(connection_data=c)
        d2 = d.create(connection_data=c)
        I = pytis.data.Integer()
        yvalue = I.validate('1')[0]
        nrepeat = 100
        thr = []
        for i in xrange(10):
            thr.append(False)
        def go1(n, startx, thr=thr):
            for i in xrange(nrepeat):
                key = I.validate('%d' % (i+startx,))[0]
                row = pytis.data.Row([('x', key), ('y', yvalue)])
                d1.insert(row)
                d1.delete(key)
            thr[n] = True
        def go2(n, startx, thr=thr):
            for i in xrange(nrepeat):
                key = I.validate('%d' % (i+startx,))[0]
                row = pytis.data.Row([('x', key), ('y', yvalue)])
                d2.insert(row)
                d2.delete(key)
            thr[n] = True
        for i in xrange(5):
            thread.start_new_thread(go1, (i, i*nrepeat,))
        for i in xrange(5):
            thread.start_new_thread(go2, (i+5, (i+5)*nrepeat,))
        end = False
        while not end:
            for i in xrange(10):
                if thr[i] == False:
                    break
            else:
                end = True
            time.sleep(1)
#tests.add(ThreadTest)


################


def get_tests():
    return tests


def go():
    unittest.main(defaultTest='get_tests',
                  argv=pytis.util.test.transform_args())

if __name__ == '__main__':
    go()
