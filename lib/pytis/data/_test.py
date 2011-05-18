#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (C) 2001-2011 Brailcom, o.p.s.
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

import copy
import datetime
import string
import time

import unittest

from pytis.util import *
import pytis.data
from pytis.data import bval, fval, ival, sval
import dbdata

_connection_data = {'database': 'test'}

tests = TestSuite()

import sys


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
        limited = pytis.data.Integer(minimum=5, maximum=8)
        self._test_validity(limited, '3', None)
        self._test_validity(limited, '5', 5)
        self._test_validity(limited, '10', None)
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
        MINLEN = 3
        MAXLEN = 5
        t = pytis.data.String(minlen=MINLEN, maxlen=MAXLEN)
        v, _ = self._test_validity(t, 'abcde', 'abcde')
        assert v.type().minlen() == MINLEN, 'wrong minlen value'
        assert v.type().maxlen() == MAXLEN, 'wrong maxlen value'
        self._test_validity(t, 'ab', None)
        self._test_validity(t, 'abcdef', None)
        self._test_validity(t, 'abcd', 'abcd')
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

class Password(_TypeCheck):
    _test_instance = pytis.data.Password(minlen=4)
    def test_validation(self):
        self._test_validity(None, 'abcdef', 'abcdef')
        self._test_validity(None, 'abcdef', 'abcdef', kwargs={'verify': 'abcdef'})
        self._test_validity(None, 'abcdef', None, kwargs={'verify': ''})
        self._test_validity(None, 'abcdef', None, kwargs={'verify': 'abcef'})
        self._test_validity(None, 'abc', None)
        v, e = self._test_validity(None, '', None, check_value=False)
        assert v and v.value() is None, v
        v, e = self._test_validity(None, '', None, check_value=False, kwargs={'verify': ''})
        assert v and v.value() is None, v
        t2 = pytis.data.Password(not_null=True)
        self._test_validity(t2, '', None)
        self._test_validity(t2, None, None)
        self._test_validity(t2, 'x', 'x')
        self._test_validity(t2, '', None, kwargs={'verify': ''})
        t3 = pytis.data.Password(md5=True, minlen=4)
        from hashlib import md5
        hashed = md5(u'abcčdef'.encode('utf-8')).hexdigest()
        self._test_validity(t3, hashed, hashed)
        self._test_validity(t3, 'xxx', None)
        self._test_validity(t3, hashed, None, kwargs={'verify': ''})
        self._test_validity(t3, 'abc', None, kwargs={'verify': 'abc'})
        self._test_validity(t3, u'abcčdef', hashed, kwargs={'verify': u'abcčdef'},
                            check_export=False)
        t4 = pytis.data.Password(md5=True, minlen=4, not_null=True)
        self._test_validity(t4, 'xxx', None)
        self._test_validity(t4, '', None, kwargs={'verify': ''})
        hashed = md5('abcd').hexdigest()
        self._test_validity(t4, 'abcd', hashed, kwargs={'verify': 'abcd'}, check_export=False)
        t5 = pytis.data.Password(strength=None)
        self._test_validity(t5, 'x', 'x')
        t6 = pytis.data.Password(strength=True)
        self._test_validity(t6, 'x', None)
        self._test_validity(t6, 'abcABC', None)
        self._test_validity(t6, '123456', None)
        self._test_validity(t6, 'abc123', 'abc123')
        self._test_validity(t6, 'abc abc', 'abc abc')
        def strength(password):
            if password and password[0] != 'X':
                return "Not an eXtreme password!"
        t7 = pytis.data.Password(strength=strength)
        self._test_validity(t7, 'abc', None)
        self._test_validity(t7, 'Xabc', 'Xabc')
        
tests.add(Password)

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
        tzinfo = pytis.data.DateTime.UTC_TZINFO
        vkwargs = {'local': False}
        self._test_validity(None, '2001-02-28 12:14:59',
                            datetime.datetime(2001,2,28,12,14,59,tzinfo=tzinfo),
                            kwargs=vkwargs, ekwargs=vkwargs)
        self._test_validity(None, '2999-12-31 0:0:0',
                            datetime.datetime(2999,12,31,0,0,0,tzinfo=tzinfo),
                            kwargs=vkwargs, ekwargs=vkwargs)
        self._test_validity(None, '  1999-01-01    23:59:59    ',
                            datetime.datetime(1999,1,1,23,59,59,tzinfo=tzinfo),
                            kwargs=vkwargs, ekwargs=vkwargs)
        self._test_validity(None, '1999-01-01 23:59', None,
                            kwargs=vkwargs, ekwargs=vkwargs)
        self._test_validity(None, '1999-01-01 23:59:00 +0200', None,
                            kwargs=vkwargs, ekwargs=vkwargs)
        self._test_validity(None, '99-01-01 0:0:0', None,
                            kwargs=vkwargs, ekwargs=vkwargs)
        self._test_validity(None, '2000-13-01 0:0:0', None,
                            kwargs=vkwargs, ekwargs=vkwargs)
        self._test_validity(None, '2001-02-29 0:0:0', None,
                            kwargs=vkwargs, ekwargs=vkwargs)
        self._test_validity(None, '2001-02-28 24:00:00', None,
                            kwargs=vkwargs, ekwargs=vkwargs)
    def test_export(self):
        tzinfo = pytis.data.DateTime.UTC_TZINFO
        vkwargs = {'local': False}
        v, e = self._test_validity(None, '2100-02-05 01:02:03',
                                   datetime.datetime(2100,2,5,1,2,3,tzinfo=tzinfo),
                                   kwargs=vkwargs, ekwargs=vkwargs,
                                   check_export=False)
        exp = v.type().export
        val = v.value()
        result = exp(val, **vkwargs)
        assert result == '2100-02-05 01:02:03', ('Invalid date export', result)
        assert v.primitive_value() == '2100-02-05 01:02:03', v.primitive_value()
tests.add(DateTime)

class Date(_TypeCheck):
    _test_instance = pytis.data.Date(format=pytis.data.Date.DEFAULT_FORMAT)
    def test_validation(self):
        self._test_validity(None, '2001-02-28', datetime.date(2001,2,28))
        self._test_validity(None, '2999-12-31', datetime.date(2999,12,31))
        self._test_validity(None, '  1999-01-01    ', datetime.date(1999,1,1))
        self._test_validity(None, '1999-01-01', datetime.date(1999,1,1))
        self._test_validity(None, '1999-01-01 23:59', None)
        self._test_validity(None, '1999-01-01 23:59:00', None)
        self._test_validity(None, '01-02-29', None)
        self._test_validity(None, '2000-13-01', None)
        self._test_validity(None, '2001-02-29', None)
    def test_date_and_time(self):
        date_value = pytis.data.Value(self._test_instance, datetime.date(2001, 2, 3))
        time_value = pytis.data.Value(pytis.data.Time(utc=True), datetime.time(12, 34, 56))
        value = pytis.data.date_and_time(date_value, time_value)
        assert value == datetime.datetime(2001, 2, 3, 12, 34, 56, tzinfo=pytis.data.DateTime.UTC_TZINFO)
        time_value = pytis.data.Value(pytis.data.Time(utc=False), datetime.time(2, 4, 6))
        value = pytis.data.date_and_time(date_value, time_value)
        assert value == datetime.datetime(2001, 2, 3, 2, 4, 6, tzinfo=pytis.data.DateTime.LOCAL_TZINFO)
tests.add(Date)

class Time(_TypeCheck):
    _test_instance = pytis.data.Time(format='%H:%M:%S')
    def test_validation(self):
        tzinfo = pytis.data.DateTime.UTC_TZINFO
        vkwargs = {'local': False}
        self._test_validity(None, '12:14:59',
                            datetime.time(12,14,59,tzinfo=tzinfo),
                            kwargs=vkwargs, ekwargs=vkwargs)
        self._test_validity(None, '0:0:0',
                            datetime.time(0,0,0,tzinfo=tzinfo),
                            kwargs=vkwargs, ekwargs=vkwargs)
        self._test_validity(None, '    23:59:59    ',
                            datetime.time(23,59,59,tzinfo=tzinfo),
                            kwargs=vkwargs, ekwargs=vkwargs)
        self._test_validity(None, '23:59', None,
                            kwargs=vkwargs, ekwargs=vkwargs)
        self._test_validity(None, '23:59:00 +0200', None,
                            kwargs=vkwargs, ekwargs=vkwargs)
        self._test_validity(None, '24:00:00', None,
                            kwargs=vkwargs, ekwargs=vkwargs)
    def test_export(self):
        tzinfo = pytis.data.DateTime.UTC_TZINFO
        vkwargs = {'local': False}
        v, e = self._test_validity(None, '01:02:03',
                                   datetime.time(1,2,3,tzinfo=tzinfo),
                                   kwargs=vkwargs, ekwargs=vkwargs,
                                   check_export=False)
        exp = v.type().export
        val = v.value()
        result = exp(val, **vkwargs)
        assert result == '01:02:03', ('Invalid time export', result)
tests.add(Time)

class TimeInterval(_TypeCheck):
    _test_instance = pytis.data.TimeInterval()
    def test_validation(self):
        self._test_validity(None, '0:15:01', datetime.timedelta(0, 901))
        self._test_validity(None, '24:00:00', datetime.timedelta(1, 0))
        self._test_validity(None, '1 day 1:00:00', datetime.timedelta(1, 3600), check_export=False)
        self._test_validity(None, '1000 days 1:00:00', datetime.timedelta(1000, 3600), check_export=False)
    def test_export(self):
        value = pytis.data.Value(self._test_instance, datetime.timedelta(1, 3600))
        exported = value.export()
        assert exported == '25:00:00', (value, exported,)
        assert value.primitive_value() == exported, (value.primitive_value(), exported,)
tests.add(TimeInterval)


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


class Array(_TypeCheck):
    _test_instance = pytis.data.Array(inner_type=pytis.data.Integer(not_null=True), maxlen=3)
    def test_validation(self):
        self._test_validity(None, (), ())
        value, _ = self._test_validity(None, ('1', '2', '3'), None, check_value=False)
        assert [v.value() for v in value.value()] == [1, 2, 3]
        assert value.export() == ('1', '2', '3'), value.export()
        assert value.primitive_value() == [1, 2, 3], value.primitive_value()
    def test_cmp(self):
        cls = self._test_instance.__class__
        inner_type = self._test_instance.inner_type()
        assert cls(inner_type=inner_type) == cls(inner_type=inner_type), 'comparison failed'
tests.add(Array)


class Enumerator(_TypeCheck):
    # Netestováno, neboť třída není používána přímo, stačí testovat potomky
    pass

class DataEnumerator(unittest.TestCase):
    def setUp(self):
        C = pytis.data.ColumnSpec
        S = pytis.data.String()
        B = pytis.data.Boolean()
        V = pytis.data.Value
        data = [pytis.data.Row((('x', sval(x)), ('y', sval(y)), ('z', bval(z))))
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
        r = e.row('2')
        assert r['y'].value() == 'b', ('Unexpected value', b)
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
        v1 = ival(1)
        v2 = sval('prvni prvek')
        v3 = ival(2)
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
        v1 = ival(1)
        v2 = sval('prvni prvek')
        v3 = ival(2)
        r = pytis.data.Row((('poradi', v1), ('popis', v2), ('cislo', v3)))
        assert r.columns(()) == ()
        assert r.columns(('poradi', 'cislo')) == (v1, v3)
    def test_update(self):
        v1 = ival(1)
        v2 = sval('prvni prvek')
        r = pytis.data.Row((('poradi', v1), ('popis', v2)))
        u1 = ival(8)
        r2 = pytis.data.Row((('poradi', u1),))
        r.update(r2)
        assert r[0] == u1 and r[1] == v2, 'row update failed'
    def test_append(self):
        r = pytis.data.Row((('x', ival(1)), ('y', ival(2))))
        r.append('z', ival(3))
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
        v1 = ival(1)
        v2 = sval('xxx')
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
        self._check_condition(pytis.data.EQ('a', sval('AA')), 0)
        self._check_condition(pytis.data.EQ('a', sval('AA'), ignore_case=True), 1)
        self._check_condition(pytis.data.NE('x', ival(5)), 4)
        self._check_condition(pytis.data.GT('x', ival(3)), 4)
        self._check_condition(pytis.data.LE('x', ival(3)), 3)
        self._check_condition(pytis.data.GE('x', 'y'), 2)
    def test_fetch(self):
        v = ival(3)
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
                             host='localhost', port=1234, database='db',
                             alternatives=dict(remote=dict(user='login2',
                                                           host='remotehost',
                                                           database='db2')))
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
    def test_select(self):
        c = self._connection
        c1 = c.select('remote')
        assert c1.user() == 'login2'
        assert c1.password() == None
        assert c1.host() == 'remotehost'
        assert c1.port() == None
        assert c1.database() == 'db2'
        c2 = c1.select(None)
        assert c.user() == c2.user(), (c.user(), c2.user())
        assert c.password() == c2.password()
        assert c.host() == c2.host()
        assert c.port() == c2.port()
        assert c.database() == c2.database()
        assert c == c2
        c3 = c.select(None)
        assert c == c3
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
        assert isinstance(m, basestring) and len(m) > 0, ('Invalid message', m)
        de = pytis.data.DBLoginException()
        m = de.message()
        assert isinstance(m, basestring) and len(m) > 0, ('Invalid message', m)
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
        except Exception as e:
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
        for q in ("create table cstat (stat char(2) PRIMARY KEY, nazev varchar(40) UNIQUE NOT NULL)",
                  "create table cosnova (id serial PRIMARY KEY, synte char(3), anal char(3), popis varchar(40), druh char(1) NOT NULL CHECK (druh IN ('X','Y')), stat char(2) REFERENCES cstat, danit boolean NOT NULL DEFAULT 'TRUE')",
                  "create table denik (id int PRIMARY KEY, datum date NOT NULL DEFAULT now(), castka decimal(15,2) NOT NULL, madati int NOT NULL DEFAULT 1 REFERENCES cosnova)",
                  "create table xcosi(id int, popis varchar(12))",
                  "create table dist (x int, y int)",
                  "create table bin(id int, data bytea)",
                  "create table fulltext(id int, text1 varchar(256), text2 text, index tsvector)",
                  "create trigger textindexupdate before update or insert on fulltext for each row execute procedure tsvector_update_trigger(index,'pg_catalog.simple',text1,text2)",
                  "insert into fulltext (id, text1, text2) values(1, 'Hello, world!', 'bear')",
                  "insert into fulltext (id, text1, text2) values(2, 'The quick brown fox jumps over the lazy dog.', 'cat')",
                  "insert into fulltext (id, text1, text2) values(3, 'GNU''s Not Unix', 'lazy fox and lazy dog')",
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
                  "insert into dist values (1, 1)",
                  "insert into dist values (2, 1)",
                  "insert into dist values (3, 2)",
                  "insert into dist values (4, 2)",
                  "insert into dist values (5, 3)",
                  "create table viewtest2 (x int)",
                  "insert into viewtest2 values (1)",
                  "insert into viewtest2 values (2)",
                  "create view viewtest1 as select * from viewtest2 where true",
                  "create rule viewtest1_update as on update to viewtest1 do instead update viewtest2 set x=new.x;",
                  "create view viewtest3 as select * from viewtest1",
                  "create rule viewtest3_insert as on insert to viewtest3 do instead insert into viewtest2 values (new.x)",
                  "create table viewtest0 (x int, y int) with oids",
                  "create view viewtest4 as select * from viewtest0",
                  "create rule viewtest4_insert as on insert to viewtest4 do instead insert into viewtest0 values (new.x)",
                  "create view viewtest7 as select *, oid from viewtest0",
                  "create rule viewtest7_insert as on insert to viewtest7 do instead insert into viewtest0 (y) values (new.y)",
                  "create table viewtest6 (x serial primary key, y int)",
                  "create view viewtest5 as select * from viewtest6",
                  "create rule viewtest5_insert as on insert to viewtest5 do instead insert into viewtest6 (y) values (new.y)",
                  "create view rudeview as select * from viewtest2 union select * from viewtest2",
                  "create type typ_xcosi as (id int, popis varchar(12))",
                  "create function tablefunc(int) returns setof typ_xcosi language 'sql' as $$ select * from xcosi where id > $1 $$",
                  ):
            try:
                self._sql_command(q)
            except:
                self.tearDown()
                raise 
    def tearDown(self):
        c = self._connector
        for t in ('tablefunc(int)',):
            try:
                self._sql_command('drop function %s' % (t,))
            except:
                pass
        for t in ('viewtest3', 'viewtest4', 'viewtest5', 'viewtest7',
                  'viewtest1', 'rudeview',):
            try:
                self._sql_command('drop view %s' % (t,))
            except:
                pass            
        for t in ('bin', 'fulltext', 'dist', 'xcosi', 'denik', 'cosnova', 'cstat', 'viewtest2',
                  'viewtest0', 'viewtest6',):
            try:
                self._sql_command('drop table %s' % (t,))
            except:
                pass
        for t in ('typ_xcosi',):
            try:
                self._sql_command('drop type %s' % (t,))
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
    ROW1 = (2, datetime.date(2001, 1, 2), 1000.0, 'U.S.A.', 'specialni')
    ROW2 = (3, datetime.date(2001, 1, 2), 2000.0, 'Czech Republic',
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
             B('stat', 'cosnova', 'stat', enumerator=pytis.data.DataEnumerator(dstat_spec)),
             B('danit', 'cosnova', 'danit')),
            key)
        dosnova = dosnova_spec.create(connection_data=conn)
        # denik
        cosi = B('', 'xcosi', 'id')
        key = B('cislo', 'denik', 'id', related_to=cosi)
        madati = B('', 'cosnova', 'id')
        stat = B('', 'cstat', 'stat')
        denik_spec = (key,
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
                      cosi)
        d = pytis.data.DBDataDefault(
            denik_spec,
            key,
            conn)
        key = B('id', 'xcosi', 'id')
        dcosi = pytis.data.DBDataDefault(
            (key,
             B('popis', 'xcosi', 'popis')),
            key,
            conn)
        self._dcosi_condition = pytis.data.DBDataDefault(
            (key,
             B('popis', 'xcosi', 'popis')),
            key, conn,
            condition=pytis.data.AND(
                pytis.data.GE('id', ival(3)),
                pytis.data.LT('id', ival(6))))
        # dist
        key = B('x', 'dist', 'x')
        dist = pytis.data.DBDataDefault(
            (key,
             B('y', 'dist', 'y')),
            key, conn, distinct_on=('y',))
        dist1 = pytis.data.DBDataDefault(
            (key,
             B('y', 'dist', 'y')),
            key, conn, distinct_on=('x',))
        # bin
        key = B('id', 'bin', 'id')
        dbin = pytis.data.DBDataDefault(
            (key,
             B('data', 'bin', 'data'),),
            key,
            conn)
        # fulltext
        key = B('id', 'fulltext', 'id')
        fulltext = pytis.data.DBDataDefault(
            (key,
             B('text1', 'fulltext', 'text1'),
             B('text2', 'fulltext', 'text2'),
             B('index', 'fulltext', 'index'),),
            key,
            conn)
        fulltext1 = pytis.data.DBDataDefault(
            (key,
             B('text1', 'fulltext', 'text1'),
             B('text2', 'fulltext', 'text2'),
             B('index', 'fulltext', 'index', type_=pytis.data.FullTextIndex(columns=('text1','text2',))),),
            key,
            conn)
        # views
        key = B('x', 'viewtest1', 'x')
        view = pytis.data.DBDataDefault((key,), key, conn)
        key = B('x', 'viewtest3', 'x')
        view3 = pytis.data.DBDataDefault((key,), key, conn)
        key = B('x', 'viewtest4', 'x')
        view4 = pytis.data.DBDataDefault((key,), key, conn)
        key = B('x', 'viewtest5', 'x')
        col = B('y', 'viewtest5', 'y')
        view5 = pytis.data.DBDataDefault((key, col,), key, conn)
        key = B('x', 'viewtest7', 'x')
        col = B('y', 'viewtest7', 'y')
        view7 = pytis.data.DBDataDefault((key, col,), key, conn)
        key = B('x', 'rudeview', 'x')
        rudeview = pytis.data.DBDataDefault((key,), key, conn)
        # atributy
        self.data = d
        #self.mdata = md
        self.dstat = dstat
        self.dstat1 = dstat1
        self.dosnova = dosnova
        self.dcosi = dcosi
        self.dist = dist
        self.dist1 = dist1
        self.dbin = dbin
        self.fulltext = fulltext
        self.fulltext1 = fulltext1
        self.view = view
        self.view3 = view3
        self.view4 = view4
        self.view5 = view5
        self.view7 = view7
        self.rudeview = rudeview
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
    def test_unique(self):
        assert self.dstat.find_column('stat').type().unique()
        assert self.dstat.find_column('nazev').type().unique()
        assert self.dosnova.find_column('id').type().unique()
        for colname in 'popis', 'druh', 'stat',:
            assert not self.dosnova.find_column(colname).type().unique(), colname
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
        result = self.dosnova.search(pytis.data.EQ('popis', sval('efgh')))
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
        v = ival(2)
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
        condition = pytis.data.EQ('popis', sval(None))
        self.dcosi.select(condition)
        n = 0
        while self.dcosi.fetchone():
            n = n + 1
        self.dcosi.close()
        assert n == 1
        # Function test
        condition = pytis.data.GT(pytis.data.OpFunction('pow', 'id', ival(2)), ival(10))
        self.dcosi.select(condition)
        n = 0
        while self.dcosi.fetchone():
            n = n + 1
        self.dcosi.close()
        assert n == 2        
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
    def test_select_distinct_on(self):
        def check(d, condition, result):
            d.select(condition=condition)
            try:
                for r in result:
                    row = d.fetchone()
                    assert row is not None, ('missing lines', condition,)
                    x, y = r
                    assert x == row['x'].value() and y == row['y'].value(), \
                        ('unexpected result', condition, (x, y,), (row['x'].value(), row['y'].value(),),)
                assert d.fetchone() is None, ('extra row', condition,)
            finally:
                d.close()
        check(self.dist, None, ((1, 1,), (3, 2,), (5, 3,),))
        check(self.dist, pytis.data.GT('x', ival(3)), ((4, 2,), (5, 3,),))
        check(self.dist1, None, ((1, 1,), (2, 1,), (3, 2,), (4, 2,), (5, 3,),))
        self.dist.select(sort=('x',))
        self.dist.close()
        self.dist.select(sort=('y',))
        self.dist.close()
        self.dist.select(sort=(('x', pytis.data.ASCENDENT,),))
        try:
            result = self.dist.search(pytis.data.GT('x', ival(3)))
        finally:
            self.dist.close()
        assert result == 2, ('distinct on search failed', result,)
    def test_select_aggregate(self):
        d = self.data
        result = d.select_aggregate((d.AGG_MIN, 'castka')).value()
        assert result == 1000, result
        result = d.select_aggregate((d.AGG_MAX, 'castka')).value()
        assert result == 2000, result
        condition = pytis.data.GT('castka', fval(1500.0))
        result = d.select_aggregate((d.AGG_AVG, 'castka'),
                                    condition=condition).value()
        assert result == 2000, result
        result = d.select_aggregate((d.AGG_COUNT, 'castka')).value()
        assert result == 2, result
        result = d.select_aggregate((d.AGG_SUM, 'castka')).value()
        assert result == 3000, result
    def test_select_and_aggregate(self):
        d = self.data
        select_result, aggregate_result = d.select_and_aggregate(d.AGG_SUM)
        assert select_result == 2, select_result
        assert aggregate_result[0].value() == 5, aggregate_result[0].value()
        assert aggregate_result[1].value() == None, aggregate_result[1].value()
        assert aggregate_result[2].value() == 3000, aggregate_result[2].value()
        assert aggregate_result[3].value() == None, aggregate_result[3].value()
        value = fval(2000.0)
        select_result, aggregate_result = \
            d.select_and_aggregate(d.AGG_MAX, columns=('castka',),
                                   condition=pytis.data.GE('castka', value))
        assert select_result == 1, select_result
        assert aggregate_result[0].value() == 2000
        select_result, aggregate_result = d.select_and_aggregate(d.AGG_COUNT)
        assert select_result == 2, select_result
        assert aggregate_result[0].value() == 2, aggregate_result[0].value()
    def test_constructor_condition(self):
        d = self._dcosi_condition
        assert d.row(ival(2)) is None, 'Excluded row found in limited data object'
        assert d.row(ival(3)) is not None, 'Row not found in limited data object'
        def test_select(condition, n):
            d.select(condition=condition)
            try:
                i = 0
                while d.fetchone() is not None:
                    i = i + 1
                assert i == n, ('Invalid number of rows in a limited select',
                                condition, n, i,)
            finally:
                d.close()
        test_select(None, 2)
        test_select(pytis.data.LT('id', ival(5)), 1)
        test_select(pytis.data.GT('id', ival(6)), 0)
    def test_async_select(self):
        self.data.select(async_count=True)
        for r in (self.ROW1, self.ROW2):
            result = self.data.fetchone()
            assert result != None, 'missing lines'
            for i in range(len(r)):
                assert r[i] == result[i].value(), \
                       ('invalid value', r[i], result[i].value())
        assert self.data.fetchone() == None, 'too many lines'
        assert self.data.fetchone() == None, 'data reincarnation'
        self.data.close()
    def test_insert(self):
        row = self.newrow
        result, success = self.data.insert(row)
        assert success
        eresult = []
        for c, v in zip(self.data.columns(), self.ROW3):
            eresult.append((c.id(), c.type().validate(v)[0]))
        eresult = pytis.data.Row(eresult)
        assert result == eresult, 'insertion failed'
        result2 = self.data.insert(row)
        assert result2[1] is False, 'invalid insertion succeeded'
        assert (result2[0] is None or type(result2[0]) == type('') or
                type(result2[0]) == type(u'')),\
               'invalid failed insertion result'
    def test_insert_view(self):
        row = pytis.data.Row((('x', ival(5),),))
        result, success = self.view3.insert(row)
        assert success
        assert result['x'].value() == 5, \
            ('unexpected insert result', result['x'].value(),)
        result, success = self.view4.insert(row)
        assert success
        assert result['x'].value() == 5, \
            ('unexpected insert result', result['x'].value(),)
        row = pytis.data.Row((('y', ival(5),),))
        result, success = self.view7.insert(row)
        assert success
        assert result is None, ('unexpected insert result', result,)
        result, success = self.view5.insert(row)
        assert success
        assert result is None, ('unexpected insert result', result,)
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
        assert result == eresult, 'update failed'
        for k in k1, k2:
            result2 = self.data.update(k, row)
            assert result2[1] is False, 'invalid update succeeded'
            assert (result2[0] is None or type(result2[0]) == type('') or
                    type(result2[0]) == type(u'')),\
                    'invalid failed update result'
            result2 = self.data.update(k, row)
        assert self.data.update(row[0], row1)[0] == row1, 'update failed'
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
    def test_table_function(self):
        id_value = ival(3)
        assert self.data.select(arguments=dict(id=id_value)) == 2
        assert self.data.fetchone() is not None
        assert self.data.fetchone() is not None
        assert self.data.fetchone() is None
        self.data.close()        
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
    def test_full_text_select(self):
        def check(query, result_set):
            condition = pytis.data.FT('index', query)
            self.fulltext.select(condition=condition, sort=('index',))
            result_ids = []
            while True:
                row = self.fulltext.fetchone()
                if row is None:
                    break
                result_ids.append(row[0].value())
            self.fulltext.close()
            assert result_set == result_ids, ('wrong full text result', result_ids,)
        def check1(query, result_set):
            condition = pytis.data.FT('index', query)
            self.fulltext1.select(condition=condition, sort=('index',))
            result_samples = []
            while True:
                row = self.fulltext1.fetchone()
                if row is None:
                    break
                result_samples.append(row[3].value())
            self.fulltext1.close()
            assert result_samples == result_set, ('wrong full text sample', result_samples,)
        check('nobody&likes&me', [])
        check('lazy&fox', [3, 2])
        check1('lazy&fox', ["The quick brown <b>fox</b> jumps over the <b>lazy</b> dog. * cat",
                            "GNU's Not Unix * <b>lazy</b> <b>fox</b> and <b>lazy</b> dog"])
        check1('world', ["Hello, <b>world</b>! * bear"])
    def test_backslash(self):
        data = self.dstat
        backslash = 'back\\012slash'
        row_template = ('xx', backslash,)
        row_data = []
        for c, v in zip(data.columns(), row_template):
            v, e = c.type().validate(v)
            if e is not None:
                raise e
            row_data.append((c.id(), v))
        row = pytis.data.Row(row_data)
        result, success = data.insert(row)
        assert success
        assert result[1].value() == backslash, ('invalid inserted value', result[1].value(), backslash,)
        assert data.delete(row[0]) == 1, 'row not deleted'
    def test_lock(self):
        us = pytis.data.String().validate('us')[0]
        cz = pytis.data.String().validate('cz')[0]
        t1, t2 = self.dstat, self.dstat1
        transaction_1 = \
            pytis.data.DBTransactionDefault(connection_data=self._dconnection)
        transaction_2 = \
            pytis.data.DBTransactionDefault(connection_data=self._dconnection)
        try:
            assert t1.lock_row(us, transaction_1) is None, 'lock failed'
            result = t2.lock_row(us, transaction_2)
            assert type(result) == type(''), 'unlocked record locked'
            assert t2.lock_row(cz, transaction_2) is None, 'lock failed'
            transaction_2.rollback()
            transaction_2 = \
                pytis.data.DBTransactionDefault(
                connection_data=self._dconnection)
            assert type(t2.lock_row(us, transaction_2)) == type(''), \
                'unlocked record locked'
            transaction_1.commit()
            transaction_1 = \
                pytis.data.DBTransactionDefault(
                connection_data=self._dconnection)
            assert t2.lock_row(us, transaction_2) is None, 'lock failed'
            transaction_1.rollback()
            transaction_2.commit()
        finally:
            try:
                transaction_1.rollback()
            except:
                pass
            try:
                transaction_2.rollback()
            except:
                pass
    def test_lock_view(self):
        v = self.view
        v.select()
        row = v.fetchone()
        key = row[0]
        row3 = v.fetchone()
        key3 = row3[0]
        v.close()
        v2 = self.rudeview
        v2.select()
        row2 = v2.fetchone()
        key2 = row2[0]
        v2.close()
        transaction = \
            pytis.data.DBTransactionDefault(connection_data=self._dconnection)
        transaction_2 = \
            pytis.data.DBTransactionDefault(connection_data=self._dconnection)
        try:
            result = v.lock_row(key, transaction=transaction)
            assert result is None, 'lock failed'
            result = v.lock_row(key, transaction=transaction_2)
            assert type(result) == type(''), 'locked record locked'
            result = v.lock_row(key3, transaction=transaction_2)
            assert result is None, 'additional row locking failed'
            result = v2.lock_row(key2, transaction=transaction)
            assert result is None, 'lock failed'
            result = v2.lock_row(key2, transaction=transaction_2)
            assert result is None, 'unlockable view locked'
        finally:
            for t in transaction, transaction_2:
                try:
                    t.rollback()
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
                value = d.fetchone()[0].value()
                assert value == k, ('invalid select value', k, value,)
        d.close()
    def test_transaction_commit(self):
        d = self.dstat
        transaction = \
            pytis.data.DBTransactionDefault(connection_data=self._dconnection)
        try:
            self._perform_transaction(transaction)
        finally:
            transaction.commit()
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
        transaction = \
            pytis.data.DBTransactionDefault(connection_data=self._dconnection)
        try:
            self._perform_transaction(transaction)
        finally:
            transaction.rollback()
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
        transaction = \
            pytis.data.DBTransactionDefault(connection_data=self._dconnection)
        try:
            transaction.set_point('xxx')
            d.insert(row1, transaction=transaction)
            transaction.set_point('yyy')
            d.insert(row2, transaction=transaction)
            transaction.set_point('zzz')
            transaction.cut('yyy')
            d.insert(row3, transaction=transaction)
            transaction.set_point('ooo')
        finally:
            transaction.commit()
        assert d.row(row1['stat']), 'missing row'
        assert d.row(row2['stat']) is None, 'extra row'
        assert d.row(row3['stat']), 'missing row'
tests.add(DBDataDefault)


class DBMultiData(DBDataDefault):
    ROW1 = (2, datetime.datetime(2001, 1, 2, tzinfo=pytis.data.DateTime.UTC_TZINFO), 1000.0, ('100','007'),
            'U.S.A.', 'specialni')
    ROW2 = (3, datetime.datetime(2001, 1, 2, tzinfo=pytis.data.DateTime.UTC_TZINFO), 2000.0, ('100','008'),
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
        v = ival(2)
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
        d = self.dosnova
        d.select()
        res = d.search(E('popis', sval('efgh')))
        assert res == 3, ('Invalid search result', res)
        res = d.search(E('popis', sval('abcd')), direction=pytis.data.FORWARD)
        assert res == 1, ('Invalid search result', res)
        res = d.search(E('popis', sval('foo')))
        assert res == 0, ('Invalid search result', res)
        d.fetchone()
        res = d.search(E('popis', sval('efgh')))
        assert res == 2, ('Invalid search result', res)
        res = d.search(E('popis', sval('abcd')))
        assert res == 0, ('Invalid search result', res)
        res = d.search(E('popis', sval('foo')))
        assert res == 0, ('Invalid search result', res)
        d.fetchone()
        res = d.search(E('popis', sval('abcd')), direction=pytis.data.FORWARD)
        assert res == 0, ('Invalid search result', res)
        res = d.search(E('popis', sval('abcd')),
                       direction=pytis.data.BACKWARD)
        assert res == 1, ('Invalid search result', res)
        while d.fetchone() is not None:
            pass
        res = d.search(E('popis', sval('abcd')), direction=pytis.data.FORWARD)
        assert res == 0, ('Invalid search result', res)
        res = d.search(E('popis', sval('abcd')),
                       direction=pytis.data.BACKWARD)
        assert res == 3, ('Invalid search result', res)        
        d.close()
    def test_search_key(self):
        d = self.dosnova
        d.select()
        res = d.search_key((sval('100'), sval('008')))
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
            self._sql_command("create table big (x int)")
            table_size = config.initial_fetch_size + config.fetch_size + 10
            self._table_size = table_size
            for i in range(table_size):
                self._sql_command("insert into big values(%d)" % i)
            self._sql_command("create table small (x int)")
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
            assert row['x'].value() == n, ('Invalid result', row['x'].value(), n)
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
        row = pytis.data.Row((('popis', sval('bla bla')),))
        d = self.data
        key = (ival(3),)
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
        d.close()
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
        d.close()
tests.add(DBDataOrdering)


class DBDataAggregated(DBDataDefault):
    def _aggtest(self, test_result, columns=None, condition=None, operation=None, key=None,
                 filter_condition=None, distinct_on=None, group_only=False, sort=()):
        D = pytis.data.DBDataDefault
        B = pytis.data.DBColumnBinding
        denik_spec = (B('cislo', 'denik', 'id'),
                      B('datum', 'denik', 'datum',
                        type_=pytis.data.Date(format=pytis.data.Date.DEFAULT_FORMAT)),
                      B('castka', 'denik', 'castka'),
                      B('madati', 'denik', 'madati'),
                      )
        if group_only:
            operations = ()
        else:
            operations = ((D.AGG_SUM, 'madati', 'madatisum',), (D.AGG_COUNT, 'cislo', 'count',),)
        if group_only:
            column_groups = ()
        else:
            column_groups = ('datum', 'castka',)
        column_groups = column_groups + (('mesic', pytis.data.Float(), 'date_part', sval('month'), 'datum',),)
        data = D(denik_spec,
                 denik_spec[0],
                 self._dconnection,
                 operations=operations,
                 column_groups=column_groups,
                 condition=filter_condition,
                 distinct_on=distinct_on)
        if not group_only:
            for column_id in ('madatisum', 'count'):
                column = data.find_column(column_id)
                assert column is not None, ('Aggregation column not found', column_id,)
                assert isinstance(column.type(), pytis.data.Integer), column.type()
        try:
            if key is not None:
                row = data.row(key=ival(key), columns=columns)
                for k, v in test_result:
                    assert k in row, ('Missing column', k,)
                    assert row[k].value() == v, ('Invalid value', v,)
            elif operation is None:
                count = data.select(columns=columns, condition=condition, sort=sort)
                assert count == len(test_result), ('Unexpected number of aggregate rows', count)
                for expected_result in test_result:
                    items = data.fetchone().items()
                    items_dict = dict(items)
                    if columns is None:
                        assert len(items) == len(column_groups) + len(operations), \
                               ('Invalid number of columns', items,)
                    else:
                        assert len(items) == len(columns), ('Invalid number of columns', items,)
                    for k, v in expected_result:
                        assert items_dict[k].value() == v, \
                               ('Unexpected result', (k, v, items_dict[k].value(),),)
                assert data.fetchone() is None, 'Extra row'
            elif isinstance(operation, tuple):
                value = data.select_aggregate(operation, condition=condition)
                assert value.value() == test_result, ('Invalid aggregate result', value.value(),)
            else:
                count, row = data.select_and_aggregate(operation, condition=condition, columns=columns)
                assert count == test_result[0], ('Invalid aggregate count', count,)
                for k, v in row.items():
                    value = v.value()
                    if value is None:
                        continue
                    test_result = test_result[1:]
                    assert test_result, ('Extra items in aggregate row', k, value,)
                    assert value == test_result[0], ('Invalid aggregate value', k, value,)
                assert len(test_result) <= 1, ('Missing aggregate row item', test_result,)
        finally:
            data.close()
    def test_basic(self, **kwargs):
        test_result = ((('castka', 1000.0), ('madatisum', 2), ('count', 2),),
                       (('castka', 2000.0), ('madatisum', 2), ('count', 1),),
                       (('castka', 3000.0), ('madatisum', 3), ('count', 1),),
                       )
        self._aggtest(test_result, **kwargs)
    def test_columns(self):
        self.test_basic(columns=('castka', 'madatisum', 'count'))
    def test_columns_groupby_function(self):
        test_result = ((('castka', 1000.0), ('madatisum', 2), ('count', 2), ('mesic', 1.0),),
                       (('castka', 2000.0), ('madatisum', 2), ('count', 1), ('mesic', 1.0),),
                       (('castka', 3000.0), ('madatisum', 3), ('count', 1), ('mesic', 1.0),),
                       )
        self._aggtest(test_result)
    def test_condition(self):
        test_result = ((('castka', 2000.0), ('madatisum', 2), ('count', 1),),
                       (('castka', 3000.0), ('madatisum', 3), ('count', 1),),
                       )
        condition = pytis.data.EQ('count', ival(1))
        self._aggtest(test_result, condition=condition)
    def test_double_aggregated(self):
        D = pytis.data.DBDataDefault
        self._aggtest(3, operation=(D.AGG_COUNT, 'madatisum',))
        self._aggtest(4, operation=(D.AGG_SUM, 'count',))
        self._aggtest((3, 6000, 7, 4,), operation=D.AGG_SUM, columns=('castka', 'madatisum', 'count',))
        self._aggtest((3, 6000, 7, 4,), operation=D.AGG_SUM)
    def test_row(self):
        self._aggtest((('castka', 2000.0), ('madatisum', 2), ('count', 1),), key=3)
    def test_group_only_row(self):
        self._aggtest(((('mesic', 1.0),),), group_only=True)
        self._aggtest(((('mesic', 1.0),),), group_only=True, sort=(('mesic', pytis.data.ASCENDENT,),))
    def test_aggregated_filter(self):
        D = pytis.data.DBDataDefault
        condition = pytis.data.EQ('cislo', ival(2))
        self._aggtest(((('castka', 1000.0), ('madatisum', 1), ('count', 1),),),
                      filter_condition=condition)
        condition = pytis.data.EQ('cislo', ival(3))
        self._aggtest(1, operation=(D.AGG_COUNT, 'madatisum',), filter_condition=condition)
    def test_distinct(self):
        D = pytis.data.DBDataDefault
        B = pytis.data.DBColumnBinding
        denik_spec = (B('cislo', 'denik', 'id'),
                      B('datum', 'denik', 'datum',
                        type_=pytis.data.Date(format=pytis.data.Date.DEFAULT_FORMAT)),
                      B('castka', 'denik', 'castka'),
                      B('madati', 'denik', 'madati'),
                      )
        operations = ((D.AGG_COUNT, 'cislo', 'count',),)
        column_groups = ('castka',)
        data = D(denik_spec,
                 denik_spec[0],
                 self._dconnection,
                 operations=operations,
                 column_groups=column_groups,
                 distinct_on=('datum',))
        data.select(columns=('count',), sort=('count',))
        data.fetchone()
        data.close()
tests.add(DBDataAggregated)


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
        cnumber_1 = d.change_number()
        cnumber_2 = self.data.change_number()
        assert cnumber_1 >= 0
        d.insert(pytis.data.Row(
            (('stat', d.columns()[0].type().validate('at')[0]),
             ('nazev', d.columns()[1].type().validate('Austria')[0]))))
        self._ddn_check_result()
        assert d.change_number() == cnumber_1 + 1, (cnumber_1, d.change_number(),)
        assert self.data.change_number() == cnumber_2 + 1, (cnumber_2, self.data.change_number(),)
# Notification tests don't work for long time for unknown reason,
# so they are disabled to not confuse test results.
#tests.add(DBDataNotification)


class DBCounter(_DBBaseTest):
    def setUp(self):
        _DBBaseTest.setUp(self)
        c = self._connector
        for q in ("create sequence fooseq",):
            try:
                self._sql_command(q)
            except:
                self.tearDown()
                raise
        self._counter = pytis.data.DBCounterDefault('fooseq', self._dconnection)
    def tearDown(self):
        c = self._connector
        for q in ("drop sequence fooseq",):
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
            self._sql_command("create table tab1 (x int)")
            self._sql_command("insert into tab1 values(10)")
            self._sql_command("insert into tab1 values(20)")
            self._sql_command("insert into tab1 values(30)")
            for q in ("foo1(int) returns int as 'select $1+1'",
                      "foo2(text,text) returns text as 'select $1 || $2'",
                      "foo3() returns int as 'select min(x) from tab'",
                      "foo4() returns tab1 as 'select * from tab1'",
                      ("foo5(int) returns setof tab1 as 'select * from tab1 "
                       "where x >= $1 order by x'"),
                      "foo6(int) returns void as 'insert into tab values ($1)'",
                      "foo7(int, out int, out int) as 'select $1, $1+2'",
                      ):
                self._sql_command("create function %s language sql " % q)
        except:
            self.tearDown()
            raise
    def tearDown(self):
        c = self._connector
        for q in ("foo1(int)",
                  "foo2(text,text)",
                  "foo3()",
                  "foo4()",
                  "foo5(int)",
                  "foo6(int)",
                  "foo7(int, out int, out int)",):
            try:
                self._sql_command("drop function %s" % q)
            except:
                pass
        for table in ('tab', 'tab1',):
            try:
                self._sql_command("drop table %s" % (table,))
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
    def test_row_result(self):
        function = pytis.data.DBFunctionDefault('foo4', self._dconnection)
        row = pytis.data.Row(())
        result = function.call(row)
        assert len(result) == 1, ('Invalid number of rows', result)
        values = [col.value() for col in result[0]]
        assert values == [10] or values == [20] or values == [30], \
            ('Invalid result', values)
    def test_setof_result(self):
        function = pytis.data.DBFunctionDefault('foo5', self._dconnection)
        row = pytis.data.Row((('arg1',
                             pytis.data.Integer().validate('20')[0]),))
        result = function.call(row)
        assert len(result) == 2, ('Invalid number of rows', result)
        value = result[0][0].value()
        assert value == 20, ('Invalid result', value)
        value = result[1][0].value()
        assert value == 30, ('Invalid result', value)
    def test_void(self):
        function = pytis.data.DBFunctionDefault('foo6', self._dconnection)
        row = pytis.data.Row((('arg1',
                             pytis.data.Integer().validate('1000')[0]),))
        function.call(row)
        data = self._sql_command("select count(*) from tab where x = 1000")
        assert data[0][0] == 1, ('Invalid data', data)
    def test_complex_result(self):
        C = pytis.data.ColumnSpec
        I = pytis.data.Integer()
        columns = [C('result1', I), C('result2', I)]
        function = pytis.data.DBFunctionDefault('foo7', self._dconnection, result_columns=columns)
        row = pytis.data.Row((('arg1',
                             pytis.data.Integer().validate('10')[0]),))
        result = [col.value() for col in function.call(row)[0]]
        assert result == [10, 12], ('Invalid result', result)
tests.add(DBFunction)


class DBSearchPath(_DBTest):
    def setUp(self):
        _DBTest.setUp(self)
        for q in ("create schema special",
                  "create table special.cstat(stat char(2) PRIMARY KEY, nazev varchar(40) UNIQUE NOT NULL)",
                  "insert into special.cstat values ('sk', 'Slovakia')",):
            self._sql_command(q)
    def tearDown(self):
        try:
            for q in ("drop table special.cstat",
                      "drop schema special",):
                self._sql_command(q)
        except:
            pass
        _DBTest.tearDown(self)
    def _retrieve(self, schemas):
        connection_data = copy.copy(_connection_data)
        connection_data['schemas'] = schemas
        name = 'schemas_' + string.join((schemas or ['default']), '_')
        connection = pytis.data.DBConnection(alternatives={name: connection_data},
                                             **_connection_data)
        B = pytis.data.DBColumnBinding
        key = B('stat', 'cstat', 'stat')
        dstat_spec = pytis.data.DataFactory(
            pytis.data.DBDataDefault,
            (key, (B('nazev', 'cstat', 'nazev'))),
            key)
        dstat = dstat_spec.create(connection_data=connection, connection_name=name)
        return dstat.select_map(lambda row: row[0].value())
    def test_default_path(self):
        def test(schemas):
            keys = self._retrieve(schemas)
            assert len(keys) > 1 and keys[0] != 'sk', ('Invalid result', keys,)
        test(None)
        test([])
        test(['public'])
        test(['public', 'special'])
    def test_special_path(self):
        def test(schemas):
            keys = self._retrieve(schemas)
            assert len(keys) == 1 and keys[0] == 'sk', ('Invalid result', keys,)
        test(['special'])
        test(['special', 'public'])
tests.add(DBSearchPath)


###################
# Complex DB test #
###################


class TutorialTest(_DBBaseTest):
    def setUp(self):
        for q in ("CREATE TABLE cis (x varchar(10) PRIMARY KEY, y text)",
                  "CREATE TABLE tab (a int PRIMARY KEY, b varchar(30), "+\
                  "c varchar(10) REFERENCES cis)",
                  "INSERT INTO cis VALUES ('1', 'raz')",
                  "INSERT INTO cis VALUES ('2', 'dva')",
                  "INSERT INTO cis VALUES ('3', 'tri')",
                  "INSERT INTO cis VALUES ('9', 'devet')",
                  "INSERT INTO tab VALUES (1, 'one', '1')",
                  "INSERT INTO tab VALUES (2, 'two', '2')",
                  ):
            try:
                self._sql_command(q)
            except:
                self.tearDown()
                raise
    def tearDown(self):
        c = self._connector
        for t in ('tab', 'cis'):
            try:
                self._sql_command("DROP TABLE %s" % (t,))
            except:
                pass
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
        cis_enumerator = pytis.data.DataEnumerator(cis_data_spec, value_column='popis', connection_data=connection)
        cis_data = cis_data_spec.create(connection_data=connection)
        tab_key = C('klic', 'tab', 'a')
        tab_columns = (tab_key,
                       C('popis', 'tab', 'b'),
                       C('id', 'tab', 'c',
                         enumerator=cis_enumerator))
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
                            ('9', u'pěkný řádek', 'devet')):
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


class AccessRightsTest(_DBBaseTest):
    def setUp(self):
        P = pytis.data.Permission
        for item in (('table1', 'column1', 'group1', P.VIEW,),
                     ('table1', 'column1', 'group2', P.VIEW,),
                     ('table1', 'column1', 'group3', P.INSERT,),
                     ('table2', 'column1', 'group4', P.VIEW,),
                     ('table1', 'column2', 'group3', P.VIEW,),
                     ('table1', 'column1', 'group1', P.UPDATE,),
                     ('table1', None, 'group3', P.UPDATE,),
                     ('table1', 'column3', 'group2', P.INSERT,),
                     ('table1', 'column3', None, P.INSERT,),
                     ('table1', 'column4', 'group1', P.INSERT,),
                     ('table1', 'column4', 'group2', P.ALL,),
                     ):
            args = tuple([x and ("'%s'" % (x,)) or 'NULL' for x in item])
            self._sql_command("INSERT INTO pytis.access_rights (object, column_, group_, permission) VALUES (%s, %s, %s, %s)" % args)
        connection_data = pytis.data.DBConnection(**_connection_data)
        self._access_rights = pytis.data.DBAccessRights(
            'table1', connection_data=connection_data)
    def tearDown(self):
        self._sql_command("DELETE FROM pytis.access_rights")
    def test_permitted_groups(self):
        P = pytis.data.Permission
        a = self._access_rights
        groups = a.permitted_groups(P.VIEW, 'column1')
        assert groups == ('group1', 'group2',), ('Invalid groups', groups,)
        groups = a.permitted_groups(P.INSERT, 'column2')
        assert groups == (), ('Invalid groups', groups,)
        groups = a.permitted_groups(P.INSERT, 'column2')
        assert groups == (), ('Invalid groups', groups,)
        groups = a.permitted_groups(P.UPDATE, 'column1')
        assert groups == ('group1', 'group3',), ('Invalid groups', groups,)
        groups = a.permitted_groups(P.UPDATE, None)
        assert groups == ('group3',), ('Invalid groups', groups,)
        groups = a.permitted_groups(P.INSERT, 'column4')
        assert groups == ('group1', 'group2',), ('Invalid groups', groups,)
    def test_permitted(self):
        P = pytis.data.Permission
        a = self._access_rights
        assert a.permitted(P.INSERT, ('group1', 'group3',), column='column1'),\
            'Invalid permission'
        assert not a.permitted(P.INSERT, ('group1', 'group2',), column='column1'),\
            'Invalid permission'
        assert a.permitted(P.UPDATE, ('group3',), column='column5'),\
            'Invalid permission'
        assert not a.permitted(P.UPDATE, ('group1',), column='column5'),\
            'Invalid permission'
        assert a.permitted(P.UPDATE, ('group3',)),\
            'Invalid permission'
        assert not a.permitted(P.VIEW, ('group3',)),\
            'Invalid permission'
tests.add(AccessRightsTest)


class ThreadTest(_DBBaseTest):
    # This is a non-regular test trying to detect bugs resulting from
    # insufficient thread safety
    def setUp(self):
        _DBBaseTest.setUp(self)
        c = self._connector
        try:
            self._sql_command("create table tab (x int, y int)")
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

class OperatorTest(unittest.TestCase):
    def test_it(self):
        a = pytis.data.EQ('a', sval('a'))
        b = pytis.data.EQ('b', sval('a'))
        c = pytis.data.EQ('a', pytis.data.Value(pytis.data.String(maxlen=5), 'a'))
        d = pytis.data.EQ('d', pytis.data.Value(pytis.data.DateTime(), pytis.data.DateTime.now().value()))
        e = pytis.data.EQ('d', pytis.data.Value(pytis.data.DateTime(), None))
        assert a != b
        assert a == c
        assert b != c
        assert a != d
        assert d != e
tests.add(OperatorTest)

################


def get_tests():
    return tests


def go():
    unittest.main(defaultTest='get_tests',
                  argv=pytis.util.test.transform_args())

if __name__ == '__main__':
    go()
