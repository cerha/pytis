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

import copy
import datetime
import decimal
import string
import time
import StringIO

import pytest
import unittest

from pytis.util import super_, DEBUG, OPERATIONAL, ACTION, EVENT
import pytis.data as pd
from pytis.data import bval, fval, ival, sval

import config

_connection_data = {'database': 'test'}

config.log_exclude = [DEBUG, OPERATIONAL, ACTION, EVENT]


#############
# types_.py #
#############


class ValidationError(unittest.TestCase):
    MESSAGE = 'test message'

    def test_it(self):
        ValidationError.e = pd.ValidationError(ValidationError.MESSAGE)
        self.assertEqual(ValidationError.e.message(), ValidationError.MESSAGE)


class Value(unittest.TestCase):

    def test_values(self):
        t = pd.Type()
        v1 = pd.Value(t, None)
        v2 = pd.Value(t, 1)
        v3 = pd.Value(t, t)
        self.assertTrue(v1.type() == t and v2.type() == t and v3.type() == t, 'type lost')
        self.assertTrue(v1.value() is None and v2.value() == 1 and v3.value() == t, 'value lost')

    def test_cmp(self):
        t = pd.Type()
        v1 = pd.Value(t, 1)
        v2 = pd.Value(t, 1)
        v3 = pd.Value(t, 2)
        self.assertEqual(v1, v2)
        self.assertNotEqual(v1, v3)


class _TypeCheck:  # (unittest.TestCase):  Temporarily disabled

    def _test_validity(self, type_, value, expected_value,
                       check_value=True, check_export=True, kwargs={},
                       ekwargs={}):
        if type_ is None:
            type_ = self._test_instance
        v, e = type_.validate(value, **kwargs)
        if check_value and expected_value is None:
            self.assertIsNone(v, ('value returned on error', str(v)))
            self.assertIsInstance(e, pd.ValidationError), ('invalid error instance', e)
        else:
            self.assertIsNone(e, ('proper value generated error', value, e,))
            self.assertIsInstance(v.type(), type_.__class__, ('invalid value type', v.type()))
            if check_value:
                self.assertEqual(v.value(), expected_value)
        if check_export and e is None:
            result, error = type_.validate(type_.export(v.value(), **ekwargs), **kwargs)
            self.assertEqual(result, v, ('export failed', str(v), str(result)))
        return v, e

    def _test_null_validation(self):
        v, e = self._test_instance.validate('')
        self.assertIsNone(e, ('Null validation failed', e,))
        self.assertIsNone(v.value(), ('Non-empty value', v.value(),))
        self.assertEqual(v.type(), self._test_instance, ('Invalid type', v.type()))
        return v

    def test_cmp(self):
        c = self._test_instance.__class__
        self.assertEqual(c(), c())


class Type(_TypeCheck):
    _test_instance = pd.Type()

    def test_validation(self):
        self._test_null_validation()

    def test_noncmp(self):
        self.assertNotEqual(self._test_instance, pd.Integer())
        self.assertNotEqual(pd.Integer(not_null=True), pd.Integer())
        self.assertNotEqual(pd.String(maxlen=2), pd.String(maxlen=3))

    def test_cloning(self):
        i1 = pd.Integer()
        i2 = pd.Integer(not_null=True)
        i12 = i1.clone(i2)
        self.assertTrue(i1.not_null() is False and i12.not_null() is True)
        i21 = i2.clone(i1)
        self.assertTrue(i21.not_null())
        i3 = pd.Integer(not_null=False)
        i23 = i2.clone(i3)
        self.assertFalse(i23.not_null())
        i31 = i3.clone(i1)
        self.assertFalse(i31.not_null())
        s1 = pd.String(maxlen=4)
        s2 = pd.RegexString(regex=r'\d-\d+')
        s12 = s1.clone(s2)
        self.assertIsInstance(s12, pd.RegexString)
        self.assertEqual(s12.maxlen(), 4)


class Integer(_TypeCheck):
    _test_instance = pd.Integer()

    def test_validation(self):
        self._test_validity(None, '1', 1)
        self._test_validity(None, '-11111111111111111111',
                            -11111111111111111111L)
        self._test_validity(None, '+0L', 0L)
        self._test_validity(None, '1.1', None)
        self._test_validity(None, 'foo', None)
        limited = pd.Integer(minimum=5, maximum=8)
        self._test_validity(limited, '3', None)
        self._test_validity(limited, '5', 5)
        self._test_validity(limited, '10', None)


class Float(_TypeCheck):
    _test_instance = pd.Float()

    def test_validation(self):
        self._test_validity(None, '3', 3.0)
        self._test_validity(None, '3.14', 3.14)
        self._test_validity(None, '-3.14', -3.14)
        self._test_validity(None, '0.0', 0.0)
        self._test_validity(None, 'foo', None)

    def test_precision(self):
        PRECISION = 3
        t = pd.Float(precision=PRECISION)
        d = decimal.Decimal
        v, _ = self._test_validity(t, '3.14159265', d('3.142'),
                                   check_export=False)
        self.assertEqual(v.type().precision(), PRECISION)
        self.assertEqual(v.export(), '3.142')

    def test_rounding(self):
        self._test_validity(None, '3.1415', 3.14, kwargs={'precision': 2})
        self._test_validity(None, '3.1415', 3.142, kwargs={'precision': 3})
        self._test_validity(None, '2.71', 3, kwargs={'precision': 0})
        F = pd.Float.FLOOR
        C = pd.Float.CEILING
        self._test_validity(None, '3.14159', 3.141, kwargs={'precision': 3,
                                                            'rounding': F})
        self._test_validity(None, '3.14159', 3.15, kwargs={'precision': 2,
                                                           'rounding': C})
        self._test_validity(None, '3.14', 3.14, kwargs={'precision': 2,
                                                        'rounding': F})
        self._test_validity(None, '3.14', 3.14, kwargs={'precision': 2,
                                                        'rounding': C})

    def test_value(self):
        f = 3.14
        d = decimal.Decimal('3.14')
        T = pd.Float()
        self.assertIsInstance(pd.Value(T, f).value(), float)
        self.assertIsInstance(pd.Value(T, d).value(), float)
        T = pd.Float(precision=2)
        self.assertIsInstance(pd.Value(T, f).value(), decimal.Decimal)
        self.assertIsInstance(pd.Value(T, d).value(), decimal.Decimal)
        T = pd.Float(digits=8)
        self.assertIsInstance(pd.Value(T, f).value(), decimal.Decimal)
        self.assertIsInstance(pd.Value(T, d).value(), decimal.Decimal)

    def test_fval(self):
        self.assertIsInstance(fval(decimal.Decimal('3.14')).value(), decimal.Decimal)
        self.assertIsInstance(fval(3.14, precision=2).value(), decimal.Decimal)
        self.assertIsInstance(fval(3.14).value(), float)


class String(_TypeCheck):
    _test_instance = pd.String()

    def test_validation_limited(self):
        MINLEN = 3
        MAXLEN = 5
        t = pd.String(minlen=MINLEN, maxlen=MAXLEN)
        v, _ = self._test_validity(t, 'abcde', 'abcde')
        self.assertEqual(v.type().minlen(), MINLEN)
        self.assertEqual(v.type().maxlen(), MAXLEN)
        self._test_validity(t, 'ab', None)
        self._test_validity(t, 'abcdef', None)
        self._test_validity(t, 'abcd', 'abcd')

    def test_validation_unlimited(self):
        v = self._test_null_validation()
        self.assertIsNone(v.type().maxlen())
        self._test_validity(None, 'abcdefghi', 'abcdefghi')
        t = pd.String(maxlen=None)
        self._test_validity(t, 'abcdefghi', 'abcdefghi')

    def test_cmp(self):
        MAXLEN = 1
        _TypeCheck.test_cmp(self)
        t = pd.String(maxlen=MAXLEN)
        self.assertEqual(t, pd.String(maxlen=MAXLEN))
        self.assertNotEqual(t, self._test_instance)
        self.assertNotEqual(t, pd.String(maxlen=(MAXLEN + 1)))


class Password(_TypeCheck):
    _test_instance = pd.Password(minlen=4)

    def test_validation(self):
        self._test_validity(None, 'abcdef', 'abcdef')
        self._test_validity(None, 'abcdef', 'abcdef', kwargs={'verify': 'abcdef'})
        self._test_validity(None, 'abcdef', None, kwargs={'verify': ''})
        self._test_validity(None, 'abcdef', None, kwargs={'verify': 'abcef'})
        self._test_validity(None, 'abc', None)
        v, e = self._test_validity(None, '', None, check_value=False)
        self.assertTrue(v and v.value() is None, v)
        v, e = self._test_validity(None, '', None, check_value=False, kwargs={'verify': ''})
        self.assertTrue(v and v.value() is None, v)
        t2 = pd.Password(not_null=True)
        self._test_validity(t2, '', None)
        self._test_validity(t2, None, None)
        self._test_validity(t2, 'x', 'x')
        self._test_validity(t2, '', None, kwargs={'verify': ''})
        t3 = pd.Password(md5=True, minlen=4)
        from hashlib import md5
        hashed = md5(u'abcčdef'.encode('utf-8')).hexdigest()
        self._test_validity(t3, hashed, hashed)
        self._test_validity(t3, 'xxx', None)
        self._test_validity(t3, hashed, None, kwargs={'verify': ''})
        self._test_validity(t3, 'abc', None, kwargs={'verify': 'abc'})
        self._test_validity(t3, u'abcčdef', hashed, kwargs={'verify': u'abcčdef'},
                            check_export=False)
        t4 = pd.Password(md5=True, minlen=4, not_null=True)
        self._test_validity(t4, 'xxx', None)
        self._test_validity(t4, '', None, kwargs={'verify': ''})
        hashed = md5('abcd').hexdigest()
        self._test_validity(t4, 'abcd', hashed, kwargs={'verify': 'abcd'}, check_export=False)
        t5 = pd.Password(strength=None)
        self._test_validity(t5, 'x', 'x')
        t6 = pd.Password(strength=True)
        self._test_validity(t6, 'x', None)
        self._test_validity(t6, 'abcABC', None)
        self._test_validity(t6, '123456', None)
        self._test_validity(t6, 'abc123', 'abc123')
        self._test_validity(t6, 'abc abc', 'abc abc')

        def strength(password):
            if password and password[0] != 'X':
                return "Not an eXtreme password!"
        t7 = pd.Password(strength=strength)
        self._test_validity(t7, 'abc', None)
        self._test_validity(t7, 'Xabc', 'Xabc')


class Color(_TypeCheck):
    _test_instance = pd.Color()

    def test_validation(self):
        self._test_validity(None, '#0030ab', '#0030ab')
        self._test_validity(None, '0030ab', None)
        self._test_validity(None, '#h030ab', None)
        v, e = self._test_validity(None, '', None, check_value=False)
        self.assertIsNone(v.value(), ('invalid value', v))

    def test_cmp(self):
        _TypeCheck.test_cmp(self)
        t = pd.Color()
        self.assertNotEqual(t, pd.String())
        self.assertEqual(t, self._test_instance)


class DateTime(_TypeCheck):
    _test_instance = pd.DateTime(format='%Y-%m-%d %H:%M:%S')

    def test_validation(self):
        tzinfo = pd.DateTime.UTC_TZINFO
        vkwargs = {'local': False}
        self._test_validity(None, '2001-02-28 12:14:59',
                            datetime.datetime(2001, 2, 28, 12, 14, 59, tzinfo=tzinfo),
                            kwargs=vkwargs, ekwargs=vkwargs)
        self._test_validity(None, '2999-12-31 0:0:0',
                            datetime.datetime(2999, 12, 31, 0, 0, 0, tzinfo=tzinfo),
                            kwargs=vkwargs, ekwargs=vkwargs)
        self._test_validity(None, '  1999-01-01    23:59:59    ',
                            datetime.datetime(1999, 1, 1, 23, 59, 59, tzinfo=tzinfo),
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
        tzinfo = pd.DateTime.UTC_TZINFO
        vkwargs = {'local': False}
        v, e = self._test_validity(None, '2100-02-05 01:02:03',
                                   datetime.datetime(2100, 2, 5, 1, 2, 3, tzinfo=tzinfo),
                                   kwargs=vkwargs, ekwargs=vkwargs,
                                   check_export=False)
        exp = v.type().export
        val = v.value()
        result = exp(val, **vkwargs)
        self.assertEqual(result, '2100-02-05 01:02:03', ('Invalid date export', result))
        self.assertEqual(v.primitive_value(), '2100-02-05 01:02:03')
        val2 = datetime.datetime(1841, 7, 2, 1, 2, 3, tzinfo=tzinfo)
        result2 = exp(val2, format='%d.%m.%Y')
        self.assertEqual(result2, '02.07.1841')


class ISODateTime(_TypeCheck):
    _test_instance = pd.ISODateTime()

    def test_validation(self):
        tzinfo = pd.DateTime.UTC_TZINFO
        vkwargs = dict(local=False, format=pd.ISODateTime.SQL_FORMAT)
        self._test_validity(None, '2012-01-23 11:14:39.23104+01:00',
                            datetime.datetime(2012, 1, 23, 10, 14, 39, 231040, tzinfo=tzinfo),
                            kwargs=vkwargs, ekwargs=vkwargs)
        self._test_validity(None, '2012-01-23 11:14:39+01:00',
                            datetime.datetime(2012, 1, 23, 10, 14, 39, 0, tzinfo=tzinfo),
                            kwargs=vkwargs, ekwargs=vkwargs)
        self._test_validity(None, '2999-12-31 00:00:00.124',
                            datetime.datetime(2999, 12, 31, 0, 0, 0, 124000, tzinfo=tzinfo),
                            kwargs=vkwargs, ekwargs=vkwargs)
        self._test_validity(None, '2999-12-31 0:0:0',
                            datetime.datetime(2999, 12, 31, 0, 0, 0, 0, tzinfo=tzinfo),
                            kwargs=vkwargs, ekwargs=vkwargs)
        self._test_validity(None, '2999-12-31 25:0:0', None,
                            kwargs=vkwargs, ekwargs=vkwargs)

    def test_export(self):
        tzinfo = pd.DateTime.UTC_TZINFO
        vkwargs = dict(local=False, format=pd.ISODateTime.SQL_FORMAT)
        v, e = self._test_validity(None, '2012-01-23 11:14:39.023104+01:00',
                                   datetime.datetime(2012, 1, 23, 10, 14, 39,
                                                     23104, tzinfo=tzinfo),
                                   kwargs=vkwargs, ekwargs=vkwargs,
                                   check_export=False)
        exp = v.type().export
        val = v.value()
        result = exp(val, **vkwargs)
        self.assertEqual(result, '2012-01-23 10:14:39.023104+00:00',
                         ('Invalid date export', result))
        self.assertEqual(v.primitive_value(), '2012-01-23 10:14:39.023104+00:00')


class Date(_TypeCheck):
    _test_instance = pd.Date(format=pd.Date.DEFAULT_FORMAT)

    def test_validation(self):
        self._test_validity(None, '2001-02-28', datetime.date(2001, 2, 28))
        self._test_validity(None, '2999-12-31', datetime.date(2999, 12, 31))
        self._test_validity(None, '  1999-01-01    ', datetime.date(1999, 1, 1))
        self._test_validity(None, '1999-01-01', datetime.date(1999, 1, 1))
        self._test_validity(None, '1841-07-02', datetime.date(1841, 7, 2))
        self._test_validity(None, '1999-01-01 23:59', None)
        self._test_validity(None, '1999-01-01 23:59:00', None)
        self._test_validity(None, '01-02-29', None)
        self._test_validity(None, '2000-13-01', None)
        self._test_validity(None, '2001-02-29', None)

    def test_date_and_time(self):
        date_value = pd.Value(self._test_instance, datetime.date(2001, 2, 3))
        time_value = pd.Value(pd.Time(utc=True), datetime.time(12, 34, 56))
        value = pd.date_and_time(date_value, time_value)
        self.assertEqual(value, datetime.datetime(2001, 2, 3, 12, 34, 56,
                                                  tzinfo=pd.DateTime.UTC_TZINFO))
        time_value = pd.Value(pd.Time(utc=False), datetime.time(2, 4, 6))
        value = pd.date_and_time(date_value, time_value)
        self.assertEqual(value, datetime.datetime(2001, 2, 3, 2, 4, 6,
                                                  tzinfo=pd.DateTime.LOCAL_TZINFO))


class Time(_TypeCheck):
    _test_instance = pd.Time(format='%H:%M:%S')

    def test_validation(self):
        tzinfo = pd.DateTime.UTC_TZINFO
        vkwargs = {'local': False}
        self._test_validity(None, '12:14:59',
                            datetime.time(12, 14, 59, tzinfo=tzinfo),
                            kwargs=vkwargs, ekwargs=vkwargs)
        self._test_validity(None, '0:0:0',
                            datetime.time(0, 0, 0, tzinfo=tzinfo),
                            kwargs=vkwargs, ekwargs=vkwargs)
        self._test_validity(None, '    23:59:59    ',
                            datetime.time(23, 59, 59, tzinfo=tzinfo),
                            kwargs=vkwargs, ekwargs=vkwargs)
        self._test_validity(None, '23:59', None,
                            kwargs=vkwargs, ekwargs=vkwargs)
        self._test_validity(None, '23:59:00 +0200', None,
                            kwargs=vkwargs, ekwargs=vkwargs)
        self._test_validity(None, '24:00:00', None,
                            kwargs=vkwargs, ekwargs=vkwargs)

    def test_export(self):
        tzinfo = pd.DateTime.UTC_TZINFO
        vkwargs = {'local': False}
        v, e = self._test_validity(None, '01:02:03',
                                   datetime.time(1, 2, 3, tzinfo=tzinfo),
                                   kwargs=vkwargs, ekwargs=vkwargs,
                                   check_export=False)
        exp = v.type().export
        val = v.value()
        result = exp(val, **vkwargs)
        self.assertEqual(result, '01:02:03', ('Invalid time export', result))


class TimeInterval(_TypeCheck):
    _test_instance = pd.TimeInterval()

    def test_validation(self):
        self._test_validity(None, '0:15:01', datetime.timedelta(0, 901))
        self._test_validity(None, '24:00:00', datetime.timedelta(1, 0))
        self._test_validity(None, '1 day 1:00:00', datetime.timedelta(1, 3600), check_export=False)
        self._test_validity(None, '1000 days 1:00:00', datetime.timedelta(1000, 3600),
                            check_export=False)

    def test_export(self):
        value = pd.Value(self._test_instance, datetime.timedelta(1, 3600))
        exported = value.export()
        self.assertEqual(exported, '25:00:00', (value, exported,))
        self.assertEqual(value.primitive_value(), exported, (value.primitive_value(), exported,))
        exported = value.export(format='%M:%S')
        self.assertEqual(exported, '00:00', (value, exported,))
        exported = value.export(format='%H')
        self.assertEqual(exported, '25', (value, exported,))


class TimeInterval2(_TypeCheck):
    _test_instance = pd.TimeInterval(format='%H:%M')

    def test_validation(self):
        self._test_validity(None, '01:02', datetime.timedelta(0, 3720))

    def test_export(self):
        value = pd.Value(self._test_instance, datetime.timedelta(1, 3600))
        exported = value.export()
        self.assertEqual(exported, '25:00', (value, exported,))
        self.assertEqual(value.primitive_value(), exported, (value.primitive_value(), exported,))
        exported = value.export(format='%M:%S')
        self.assertEqual(exported, '00:00', (value, exported,))
        exported = value.export(format='%H')
        self.assertEqual(exported, '25', (value, exported,))


class Boolean(_TypeCheck):
    _test_instance = pd.Boolean()

    def test_validation(self):
        v, _ = self._test_validity(None, 'T', None, check_value=False)
        self.assertTrue(v.value(), 'T not mapped to true')
        v, _ = self._test_validity(None, 'F', None, check_value=False)
        self.assertFalse(v.value(), 'F not mapped to false')
        self._test_validity(None, 't', None)
        self._test_validity(None, '0', None)

    def test_noncmp(self):
        self.assertNotEqual(self._test_instance, pd.String())


class Array(_TypeCheck):
    _test_instance = pd.Array(inner_type=pd.Integer(not_null=True), maxlen=3)

    def test_validation(self):
        self._test_validity(None, (), ())
        value, _ = self._test_validity(None, ('1', '2', '3'), None, check_value=False)
        self.assertEqual([v.value() for v in value.value()], [1, 2, 3])
        self.assertEqual(value.export(), ('1', '2', '3'), value.export())
        self.assertEqual(value.primitive_value(), [1, 2, 3], value.primitive_value())

    def test_cmp(self):
        cls = self._test_instance.__class__
        inner_type = self._test_instance.inner_type()
        self.assertEqual(cls(inner_type=inner_type), cls(inner_type=inner_type))


class Binary(_TypeCheck):
    _test_instance = pd.Binary()

    def test_validation(self):
        self._test_validity(None, '', '')
        self._test_validity(None, '0123456789', '0123456789')
        self._test_validity(None, buffer('abc'), 'abc')
        self._test_validity(None, open(__file__), open(__file__).read())
        self._test_validity(pd.Binary(maxlen=300), open(__file__), None)
        self._test_validity(pd.Binary(maxlen=300), 400 * 'x', None)
        self._test_validity(pd.Binary(maxlen=300), 300 * 'x', bytes(300 * 'x'))

    def test_typeerror(self):
        self.assertRaises(TypeError, lambda: pd.Value(pd.Binary(), u'abc'))

    def test_len(self):
        v = pd.Value(pd.Binary(), '0123456789')
        self.assertEqual(len(v.value()), 10)

    def test_cmp(self):
        self.assertEqual(pd.Binary(), pd.Binary())
        self.assertNotEqual(pd.Binary(), pd.Binary(maxlen=3))


class Image(_TypeCheck):
    _test_instance = pd.Image()
    _icon = ('\x89PNG\r\n\x1a\n\x00\x00\x00\rIHDR\x00\x00\x00\x0c\x00\x00\x00\x05\x04'
             '\x03\x00\x00\x00\x83T\x10\x1c\x00\x00\x00\x12PLTE\xef\xef\xef366rttSUU'
             '\x91\x92\x92\xff\xff\xff\xa6\xcc.k\x00\x00\x00\x01tRNS\x00@\xe6\xd8f'
             '\x00\x00\x00\x01bKGD\x00\x88\x05\x1dH\x00\x00\x00\tpHYs\x00\x00\x0b'
             '\x13\x00\x00\x0b\x13\x01\x00\x9a\x9c\x18\x00\x00\x00\x07tIME\x07\xdd'
             '\x04\x1a\r\x13:\x07k\xbf\x19\x00\x00\x00\x1fIDAT\x08\xd7c`\x10d\x00!'
             '\x06A\x01F\x10\xc5(((\xc0\x00\xe2\x828@.\x90\x03\x00\x0e:\x00\xbc\xe3'
             '\xfd\xe4\x96\x00\x00\x00\x00IEND\xaeB`\x82')

    def test_validation(self):
        self._test_validity(None, self._icon, self._icon)
        self._test_validity(None, StringIO.StringIO(self._icon), self._icon)
        self._test_validity(pd.Image(minsize=(100, 100)), self._icon, None)
        self._test_validity(pd.Image(maxsize=(10, 10)), self._icon, None)

    def test_image(self):
        value = pd.Value(self._test_instance, self._icon)
        image = value.value().image()
        self.assertEqual(image.size, (12, 5))


class DataEnumerator(unittest.TestCase):

    def setUp(self):
        C = pd.ColumnSpec
        S = pd.String()
        B = pd.Boolean()
        data = [pd.Row((('x', sval(x)), ('y', sval(y)), ('z', bval(z))))
                for x, y, z in (('1', 'a', True), ('2', 'b', True), ('3', 'c', False))]
        d = pd.DataFactory(pd.MemData,
                                   (C('x', S), C('y', S), C('z', B)),
                                   data=data)
        e1 = pd.DataEnumerator(d)
        e2 = pd.DataEnumerator(d, value_column='y')
        e3 = pd.DataEnumerator(d, validity_column='z')
        self.cb1 = pd.String(enumerator=e1)
        self.cb2 = pd.String(enumerator=e2, not_null=True)
        self.cb3 = pd.String(enumerator=e3)

    def _test_validate(self, cb, value, expected=None, invalid=False):
        v, e = cb.validate(value)
        if invalid:
            self.assertIsNotNone(e)
        else:
            self.assertIsNone(e)
            self.assertEqual(v.value(), expected)

    def _test_export(self, cb, value, expected):
        result = self.cb1.export(value)
        self.assertEqual(result, expected, ('Invalid exported value:', result))

    def test_validate(self):
        self._test_validate(self.cb1, '1', '1')
        self._test_validate(self.cb1, '', None)
        self._test_validate(self.cb1, '8', None, invalid=True)
        self._test_validate(self.cb2, 'b', 'b')
        self._test_validate(self.cb2, '', None, invalid=True)
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
        self.assertEqual(v, ('1', '2', '3'))

    def test_get(self):
        e = self.cb1.enumerator()
        r = e.row('2')
        self.assertEqual(r['y'].value(), 'b', ('Unexpected value', r['y'].value()))


class FixedEnumerator(unittest.TestCase):
    _values = (1, 3, 5, 7, 9,)
    _enumerator = pd.FixedEnumerator(_values)

    def test_check(self):
        e = self._enumerator
        for i in range(100):
            if i in self._values:
                self.assertTrue(e.check(i))
            else:
                self.assertFalse(e.check(i))
        self.assertFalse(e.check('1'))

    def test_values(self):
        self.assertEqual(self._enumerator.values(), self._values)


###########
# data.py #
###########


class ReversedSorting(unittest.TestCase):

    def test_it(self):
        A = pd.ASCENDENT
        D = pd.DESCENDANT
        self.assertEqual((), pd.reversed_sorting(()))
        self.assertEqual((('foo', A),), pd.reversed_sorting((('foo', D),)))
        self.assertEqual((('foo', D), ('bar', A)),
                         pd.reversed_sorting((('foo', A), ('bar', D))))


class ColumnSpec(unittest.TestCase):
    _test_instance = pd.ColumnSpec('foo', pd.Integer())

    def test_class_(self):
        self.assertEqual(ColumnSpec._test_instance.id(), 'foo')
        self.assertEqual(ColumnSpec._test_instance.type(), pd.Integer())

    def test_cmp(self):
        x = pd.ColumnSpec('foo', pd.Integer())
        y = pd.ColumnSpec('bar', pd.Integer())
        z = pd.ColumnSpec('foo', pd.String())
        self.assertEqual(self._test_instance, x)
        self.assertNotEqual(self._test_instance, y)
        self.assertNotEqual(self._test_instance, z)


class TestRow(object):

    def test_empty(self):
        row = pd.Row()
        assert len(row) == 0
        assert row.keys() == []
        assert row.values() == []

    def test_nonempty(self):
        v1 = ival(1)
        v2 = sval('prvni prvek')
        v3 = ival(2)
        row = pd.Row((('poradi', v1), ('popis', v2)))
        assert len(row) == 2
        assert row[0] == v1 and row[1] == v2
        assert row[-2] == v1 and row[-1] == v2
        assert row['poradi'] == v1 and row['popis'] == v2
        assert row.values() == [v1, v2]
        for key in (-3, 2, '', 'pop', None, self):
            with pytest.raises((IndexError, KeyError)):
                row[key]
        row[0] = row['popis'] = v3
        assert row['poradi'] == row[1] == v3
        row[0:2] = (v2, v1)
        assert row[0] == v2 and row[1] == v1
        x1, x2 = row[0:2]
        assert x1 == v2 and x2 == v1
        assert row[0:1][0] == v2 and row[1:2][0] == v1

    def test_columns(self):
        v1 = ival(1)
        v2 = sval('prvni prvek')
        v3 = ival(2)
        r = pd.Row((('poradi', v1), ('popis', v2), ('cislo', v3)))
        assert r.columns(()) == ()
        assert r.columns(('poradi', 'cislo')) == (v1, v3)

    def test_update(self):
        v1 = ival(1)
        v2 = sval('prvni prvek')
        r = pd.Row((('poradi', v1), ('popis', v2)))
        u1 = ival(8)
        r2 = pd.Row((('poradi', u1),))
        r.update(r2)
        assert r[0] == u1 and r[1] == v2

    def test_append(self):
        r = pd.Row((('x', ival(1)), ('y', ival(2))))
        r.append('z', ival(3))
        assert r['x'].value() == 1
        assert r['y'].value() == 2
        assert r['z'].value() == 3


class Data(unittest.TestCase):

    def setUp(self):
        c1 = self._column1 = pd.ColumnSpec('foo',
                                                   pd.Integer())
        c2 = self._column2 = pd.ColumnSpec('bar',
                                                   pd.String())
        self._value = pd.Value(pd.Type(), None)
        self._row = pd.Row()
        self._data = pd.Data((c1, c2), c1)

    def test_it(self):
        c1, c2 = self._column1, self._column2
        r = self._row
        v = self._value
        d = self._data
        self.assertEqual(d.columns(), (c1, c2), 'columns lost')
        self.assertEqual(d.find_column('bar'), c2, 'column lost')
        self.assertIsNone(d.find_column('foobar'), 'imaginary column')
        self.assertEqual(d.key(), (c1,), 'key lost')
        self.assertIsNone(d.row(v), 'row not working')
        self.assertEqual(d.select(), 0, 'select not working')
        self.assertIsNone(d.fetchone(), 'fetchone not working')
        self.assertEqual(d.insert(r), (None, False), 'insert not working')
        self.assertEqual(d.update(v, r), (None, False), 'update not working')
        self.assertEqual(d.delete(v), 0, 'delete not working')

    def test_row_key(self):
        v1 = ival(1)
        v2 = sval('xxx')
        row = pd.Row((('foo', v1), ('bar', v2)))
        self.assertEqual(self._data.row_key(row), (v1,))


class MemData(unittest.TestCase):

    def setUp(self):
        columns = (pd.ColumnSpec('a', pd.String()),
                   pd.ColumnSpec('b', pd.String()),
                   pd.ColumnSpec('x', pd.Integer()),
                   pd.ColumnSpec('y', pd.Integer()))
        data = [pd.Row([(c.id(), pd.Value(c.type(), v))
                                for c, v in zip(columns, values)])
                for values in (('a', 'Bob', 1, 10),
                               ('b', 'John', 5, 27),
                               ('c', 'Will', 3, 2),
                               ('d', 'Bill', 3, 42),
                               ('e', 'John', 5, 12),
                               ('f', 'Joe', 5, 31),
                               ('g', 'Eddie', 12, 10))]
        d = pd.DataFactory(pd.MemData, columns, data=data)
        self._data = d.create()

    def _check_condition(self, condition, keys):
        rows = []
        self._data.select(condition=condition)
        while True:
            row = self._data.fetchone()
            if row is None:
                break
            rows.append(row)
        self.assertEqual(tuple([r['a'].value() for r in rows]), keys)

    def test_conditions(self):
        self._check_condition(pd.EQ('a', sval('A')), ())
        self._check_condition(pd.EQ('a', sval('A'), ignore_case=True), ('a',))
        self._check_condition(pd.NE('x', ival(5)), ('a', 'c', 'd', 'g'))
        self._check_condition(pd.GT('x', ival(3)), ('b', 'e', 'f', 'g'))
        self._check_condition(pd.LE('x', ival(3)), ('a', 'c', 'd'))
        self._check_condition(pd.GE('x', 'y'), ('c', 'g'))

    def test_wildcards(self):
        def wm(pattern):
            return pd.WMValue(pd.String(), pattern)
        self._check_condition(pd.WM('b', wm('a*')), ())
        self._check_condition(pd.WM('b', wm('B*')), ('a', 'd'))
        self._check_condition(pd.WM('b', wm('*o*')), ('a', 'b', 'e', 'f'))
        self._check_condition(pd.WM('b', wm('?o?')), ('a', 'f'))
        self._check_condition(pd.WM('b', wm('^B*')), ())
        self._check_condition(pd.WM('b', wm('.+')), ())

    def test_fetch(self):
        v = ival(3)
        c = self._data.select(pd.EQ('x', v))
        self.assertEqual(c, 2)
        rows = []
        while True:
            row = self._data.fetchone()
            if row is None:
                break
            rows.append(row)
        self.assertEqual(len(rows), 2)
        self.assertEqual(rows[0]['b'].value(), 'Will')
        self.assertEqual(rows[1]['b'].value(), 'Bill')


class DataFactory(unittest.TestCase):

    def setUp(self):
        c1 = self._column1 = pd.ColumnSpec('foo',
                                                   pd.Integer())
        c2 = self._column2 = pd.ColumnSpec('bar',
                                                   pd.String())
        self._columns = (c1, c2)

    def test_basic(self):
        columns = self._columns
        key = (columns[1],)
        factory = pd.DataFactory(pd.Data, columns, key)
        factory2 = pd.DataFactory(pd.Data, columns, key=key)
        data1 = factory.create()
        data2 = factory.create()
        data3 = factory2.create()
        self.assertIsNot(data1, data2)
        for d in data1, data2, data3:
            self.assertEqual(d.columns(), columns)
            self.assertEqual(d.key(), key, ("Key doesn't match", d.key(), key))

    def test_create(self):
        columns = self._columns
        key = (columns[0],)
        factory = pd.DataFactory(pd.Data, columns,
                                         key=(columns[1],))
        data = factory.create(key=key)
        self.assertEqual(data.columns(), columns)
        self.assertEqual(data.key(), key)


#############
# dbdata.py #
#############


class DBConnection(unittest.TestCase):

    def setUp(self):
        C = pd.DBConnection
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
        self.assertEqual(c.user(), 'login')
        self.assertEqual(c.password(), 'heslo')
        self.assertEqual(c.host(), 'localhost')
        self.assertEqual(c.port(), 1234)
        self.assertEqual(c.database(), 'db')

    def test_cmp(self):
        self.assertEqual(self._connection, self._connection2)
        self.assertNotEqual(self._connection, self._connection3)

    def test_modified(self):
        c = self._connection
        cc = c.modified(host='remotehost')
        self.assertEqual(c.user(), cc.user())
        self.assertEqual(c.password(), cc.password())
        self.assertEqual(c.port(), cc.port())
        self.assertEqual(c.database(), cc.database())
        self.assertEqual(c.host(), 'localhost')
        self.assertEqual(cc.host(), 'remotehost')

    def test_select(self):
        c = self._connection
        c1 = c.select('remote')
        self.assertEqual(c1.user(), 'login2')
        self.assertEqual(c1.password(), 'heslo')
        self.assertEqual(c1.host(), 'remotehost')
        self.assertIsNone(c1.port())
        self.assertEqual(c1.database(), 'db2')
        c2 = c1.select(None)
        self.assertEqual(c.user(), c2.user())
        self.assertEqual(c.password(), c2.password())
        self.assertEqual(c.host(), c2.host())
        self.assertEqual(c.port(), c2.port())
        self.assertEqual(c.database(), c2.database())
        self.assertEqual(c, c2)
        c3 = c.select(None)
        self.assertEqual(c, c3)


class DBBinding(unittest.TestCase):

    def test_it(self):
        b = pd.DBBinding('foo')
        self.assertEqual(b.id(), 'foo')


class DBColumnBinding(unittest.TestCase):

    def test_defaults(self):
        b = pd.DBColumnBinding('bar', 'tabulka', 'sloupec')
        self.assertEqual(b.id(), 'bar')
        self.assertEqual(b.table(), 'tabulka')
        self.assertEqual(b.column(), 'sloupec')
        self.assertIsNone(b.related_to(), 'intruding relation')
        self.assertFalse(b.is_hidden(), 'secret column')

    def test_specified(self):
        b1 = pd.DBColumnBinding('', 'ciselnik', 'id')
        self.assertEqual(b1.id(), '')
        self.assertEqual(b1.table(), 'ciselnik')
        self.assertEqual(b1.column(), 'id')
        self.assertIsNone(b1.related_to(), 'intruding relation')
        self.assertTrue(b1.is_hidden(), 'public column')
        b2 = pd.DBColumnBinding('foo', 'tabulka', 'sloupec',
                                        related_to=b1)
        self.assertEqual(b2.id(), 'foo')
        self.assertEqual(b2.table(), 'tabulka')
        self.assertEqual(b2.column(), 'sloupec')
        self.assertEqual(b2.related_to(), b1)
        self.assertFalse(b2.is_hidden(), 'secret column')


class DBExceptions(unittest.TestCase):

    def test_constructors(self):
        e = Exception()
        de = pd.DBException('message', e, 'bla bla', 4)
        self.assertEqual(de.message(), 'message')
        self.assertEqual(de.exception(), e)
        de = pd.DBUserException('message')
        self.assertIsNone(de.exception())
        de = pd.DBSystemException(None)
        m = de.message()
        self.assertTrue(isinstance(m, basestring) and len(m) > 0, ('Invalid message', m))
        de = pd.DBLoginException()
        m = de.message()
        self.assertTrue(isinstance(m, basestring) and len(m) > 0, ('Invalid message', m))


class DBData(unittest.TestCase):

    def test_it(self):
        b1 = pd.DBBinding('foo')
        b2 = pd.DBBinding('bar')
        bindings = (b1, b2)
        d = pd.DBData(bindings)
        self.assertEqual(map(lambda c: c.id(), d.columns()), ['foo', 'bar'])
        self.assertEqual(len(d.key()), 1, ('invalid number of keys', d.key()))
        self.assertEqual(d.key()[0].id(), 'foo', ('invalid key', d.key()[0]))


class TestFetchBuffer(object):

    @pytest.fixture
    def buf(self):
        return pd.FetchBuffer(1000)

    def test_init(self, buf):
        assert buf.position() == -1
        assert buf.current() is None

    def test_empty_fetch(self, buf):
        assert buf.fetch(pd.FORWARD) is None
        assert buf.position() == 0
        assert buf.fetch(pd.BACKWARD) is None
        assert buf.position() == -1
        assert buf.fetch(pd.BACKWARD) is None
        assert buf.position() == -2

    def test_edge_fetch(self, buf):
        buf.fill(0, ['A', 'B', 'C'])
        assert buf.fetch(pd.BACKWARD) is None
        assert buf.fetch(pd.BACKWARD) is None
        assert buf.fetch(pd.FORWARD) is None
        assert buf.fetch(pd.FORWARD) is None
        assert buf.fetch(pd.FORWARD) == 'A'
        assert buf.fetch(pd.BACKWARD) is None
        assert buf.fetch(pd.FORWARD) == 'A'
        assert buf.fetch(pd.FORWARD) == 'B'
        assert buf.fetch(pd.FORWARD) == 'C'
        assert buf.fetch(pd.FORWARD) is None
        assert buf.fetch(pd.BACKWARD) == 'C'
        assert buf.fetch(pd.FORWARD) is None
        assert buf.fetch(pd.FORWARD) is None
        assert buf.fetch(pd.BACKWARD) is None
        assert buf.fetch(pd.BACKWARD) == 'C'

    def test_fill(self, buf):
        buf.fill(0, ['A', 'B', 'C', 'D', 'E', 'F'])
        assert buf.fetch(pd.FORWARD) == 'A'
        assert buf.fetch(pd.BACKWARD) is None
        assert buf.fetch(pd.FORWARD) == 'A'

    def test_skip(self, buf):
        buf.fill(0, [chr(i) for i in range(ord('A'), ord('Z') + 1)])
        assert buf.fetch(pd.FORWARD) == 'A'
        buf.skip(8, pd.FORWARD)
        buf.skip(8, pd.BACKWARD)
        assert buf.fetch(pd.FORWARD) == 'B'
        buf.skip(8, pd.FORWARD)
        assert buf.fetch(pd.FORWARD) == 'K'
        assert buf.fetch(pd.FORWARD) == 'L'
        buf.skip(11, pd.FORWARD)
        assert buf.fetch(pd.FORWARD) == 'X'
        buf.skip(14, pd.BACKWARD)
        assert buf.fetch(pd.FORWARD) == 'K'

    def test_skip_outside(self, buf):
        buf.fill(0, ['A', 'B', 'C', 'D', 'E', 'F'])
        buf.skip(20, pd.FORWARD)
        assert buf.position() == 19
        buf.skip(32, pd.BACKWARD)
        assert buf.position() == -13

    def test_refill(self, buf):
        buf.fill(0, ['A', 'B', 'C', 'D', 'E', 'F'])
        buf.fill(10, ['K', 'L', 'M', 'N'])
        assert buf.fetch(pd.FORWARD) == 'K'
        assert buf.fetch(pd.FORWARD) == 'L'

    def test_append(self, buf):
        buf.fill(0, ['A', 'B', 'C', 'D', 'E', 'F'])
        buf.fill(6, ['G', 'H', 'I', 'J'])
        assert buf.fetch(pd.FORWARD) == 'G'
        buf.skip(4, pd.BACKWARD)
        assert buf.fetch(pd.FORWARD) == 'D'


class _DBBaseTest(unittest.TestCase):

    def _sql_command(self, command):
        cursor = self._connector.cursor()
        try:
            cursor.execute(command)
        except Exception as e:
            try:
                self._connector.rollback()
                cursor.close()
            except Exception:
                pass
            raise e
        try:
            result = cursor.fetchall()
        except Exception:
            result = ()
        self._connector.commit()
        cursor.close()
        return result

    def setUp(self):
        self._dconnection = pd.DBConnection(**_connection_data)
        import psycopg2
        self._connector = psycopg2.connect(**_connection_data)

    def tearDown(self):
        self._connector.close()


class DBTest(object):
    """Pytest alternative to _DBBaseTest.

    All tests derived from '_DBBaseTest' should be gradually overwritten to
    derive from this class and make better use of pytest.

    """

    class DataStream(object):
        """Pretend a file-like object for given generator of row data.

        Used as the first argument for cursor.copy_from() to insert data
        efficiently.

        TODO: Currently only used internally in tests, but might be generally
        useful.

        """
        def __init__(self, generator):
            if isinstance(generator, (tuple, list)):
                generator = (x for x in generator)
            self._generator = generator
            self._buffer = ''

        def read(self, n=None):
            while n is None or len(self._buffer) < n:
                line = self.readline()
                if not line:
                    break
                self._buffer += line
            if not self._buffer:
                result = ''
            elif n is None:
                result = self._buffer
                self._buffer = ''
            else:
                result = self._buffer[:n]
                self._buffer = self._buffer[n:]
            return result

        def readline(self):
            try:
                row = next(self._generator)
                return '\t'.join('\\N' if x is None else str(x) for x in row) + '\n'
            except StopIteration:
                return None

    @pytest.fixture
    def dbconnection(self):
        return pd.DBConnection(**_connection_data)

    @pytest.fixture(autouse=True)
    def connector(self):
        import psycopg2
        self._connector = psycopg2.connect(**_connection_data)
        yield None
        self._connector.close()

    def sql(self, command):
        cursor = self._connector.cursor()
        try:
            cursor.execute(command)
        except Exception as e:
            try:
                self._connector.rollback()
                cursor.close()
            except Exception:
                pass
            raise e
        try:
            result = cursor.fetchall()
        except Exception:
            result = ()
        self._connector.commit()
        cursor.close()
        return result

    def insert(self, data, rows):
        cursor = self._connector.cursor()
        cursor.copy_from(self.DataStream(rows), data.table(data.columns()[0].id()))
        self._connector.commit()


def test_data_stream():
    # Test testing infrastructure.
    s = DBTest.DataStream((('a', 1), ('b', '2'), ('c', 3)))
    assert s.readline() == 'a\t1\n'
    assert s.read(2) == 'b\t'
    assert s.read() == '2\nc\t3\n'
    assert DBTest.DataStream(()).read() == ''
    s = DBTest.DataStream((('a', 1), ('b', '2'), ('c', 3)))
    assert s.read(4096) == 'a\t1\nb\t2\nc\t3\n'


class _DBTest(_DBBaseTest):

    def setUp(self):
        _DBBaseTest.setUp(self)
        for q in ("create table cstat (stat char(2) PRIMARY KEY, "
                  "nazev varchar(40) UNIQUE NOT NULL)",
                  "create table cosnova (id serial PRIMARY KEY, synte char(3), anal char(3), "
                  "popis varchar(40), druh char(1) NOT NULL CHECK (druh IN ('X','Y')), "
                  "stat char(2) REFERENCES cstat, danit boolean NOT NULL DEFAULT 'TRUE')",
                  "create table denik (id int PRIMARY KEY, "
                  "datum date NOT NULL DEFAULT now(), "
                  "castka decimal(15,2) NOT NULL, "
                  "madati int NOT NULL DEFAULT 1 REFERENCES cosnova)",
                  "create table xcosi(id int, popis varchar(12))",
                  "create table dist (x int, y int)",
                  "create table bin(id int, data bytea)",
                  "create table fulltext(id int, text1 varchar(256), text2 text, index tsvector)",
                  "create table dateformats (id int primary key, "
                  "datetime timestamptz default now())",
                  "create table timezones(id serial primary key, "
                  "dt timestamp, dttz timestamptz, t time, ttz timetz)",
                  "create trigger textindexupdate before update or insert on fulltext "
                  "for each row execute procedure "
                  "tsvector_update_trigger(index,'pg_catalog.simple',text1,text2)",
                  "insert into fulltext (id, text1, text2) values(1, 'Hello, world!', 'bear')",
                  "insert into fulltext (id, text1, text2) "
                  "values(2, 'The quick brown fox jumps over the lazy dog.', 'cat')",
                  "insert into fulltext (id, text1, text2) "
                  "values(3, 'GNU''s Not Unix', 'lazy fox and lazy dog')",
                  "insert into cstat values('us', 'U.S.A.')",
                  "insert into cstat values('cz', 'Czech Republic')",
                  "insert into cosnova values(1, '100', '007', 'abcd', 'X', 'us', 'FALSE')",
                  "insert into cosnova values(2, '100', '008', 'ijkl', 'X', 'cz', 'FALSE')",
                  "insert into cosnova values(3, '101', '   ', 'efgh', 'Y', 'us')",
                  "insert into denik (id, datum, castka, madati) "
                  "values(1, '2001-01-02', '1000.00', 1)",
                  "insert into denik (id, datum, castka, madati) "
                  "values(2, '2001-01-02', '1000.00', 1)",
                  "insert into denik (id, datum, castka, madati) "
                  "values(3, '2001-01-02', '2000.00', 2)",
                  "insert into denik (id, datum, castka, madati) "
                  "values(4, '2001-01-04', '3000.00', 3)",
                  "insert into xcosi values(2, 'specialni')",
                  "insert into xcosi values(3, 'zvlastni')",
                  "insert into xcosi values(5, 'nove')",
                  "insert into xcosi values(999, NULL)",
                  "insert into dist values (1, 1)",
                  "insert into dist values (2, 1)",
                  "insert into dist values (3, 2)",
                  "insert into dist values (4, 2)",
                  "insert into dist values (5, 3)",
                  "insert into dateformats (id) values (1)",
                  "create table viewtest2 (x int)",
                  "insert into viewtest2 values (1)",
                  "insert into viewtest2 values (2)",
                  ("create table rangetable (x int, r int4range, r2 int4range, rdt tsrange)",
                   90200,),
                  (("insert into rangetable values "
                    "(1, '[10, 20)', '[10, 20)', '[2014-01-01 00:00:00, 2014-01-01 00:00:02)')"),
                   90200,),
                  "create table arraytable (x int primary key, a int[], b text[])",
                  "insert into arraytable values (1, '{2, 3}', '{hello, world}')",
                  "insert into arraytable values (99, NULL, '{}')",
                  "create view viewtest1 as select *, x||'%s%s'::text as foo "
                  "from viewtest2 where true",
                  "create rule viewtest1_update as on update to viewtest1 "
                  "do instead update viewtest2 set x=new.x;",
                  "create view viewtest3 as select * from viewtest1",
                  "create rule viewtest3_insert as on insert to viewtest3 "
                  "do instead insert into viewtest2 values (new.x)",
                  "create table viewtest0 (x int, y int) with oids",
                  "create view viewtest4 as select * from viewtest0",
                  "create rule viewtest4_insert as on insert to viewtest4 "
                  "do instead insert into viewtest0 values (new.x)",
                  "create view viewtest7 as select *, oid from viewtest0",
                  "create rule viewtest7_insert as on insert to viewtest7 "
                  "do instead insert into viewtest0 (y) values (new.y)",
                  "create table viewtest6 (x serial primary key, y int)",
                  "create view viewtest5 as select * from viewtest6",
                  "create rule viewtest5_insert as on insert to viewtest5 "
                  "do instead insert into viewtest6 (y) values (new.y)",
                  "create view rudeview as select * from viewtest2 union select * from viewtest2",
                  "create type typ_xcosi as (id int, popis varchar(12))",
                  "create function tablefunc(int) returns setof typ_xcosi language 'sql' as "
                  "$$ select * from xcosi where id > $1 $$",
                  ):
            try:
                if len(q) == 2:
                    cmd = q[0]
                    min_version = q[1]
                else:
                    cmd = q
                    min_version = None
                if min_version is None or min_version <= self._connector.server_version:
                    self._sql_command(cmd)
            except Exception:
                self.tearDown()
                raise

    def tearDown(self):
        for t in ('tablefunc(int)',):
            try:
                self._sql_command('drop function %s' % (t,))
            except Exception:
                pass
        for t in ('viewtest3', 'viewtest4', 'viewtest5', 'viewtest7',
                  'viewtest1', 'rudeview',):
            try:
                self._sql_command('drop view %s' % (t,))
            except Exception:
                pass
        tables = ['bin', 'arraytable', 'dateformats', 'timezones', 'fulltext',
                  'dist', 'xcosi', 'denik',
                  'cosnova', 'cstat', 'viewtest2', 'viewtest0', 'viewtest6']
        if self._connector.server_version >= 90200:
            tables.append('rangetable')
        for t in tables:
            try:
                self._sql_command('drop table %s' % (t,))
            except Exception:
                pass
        for t in ('typ_xcosi',):
            try:
                self._sql_command('drop type %s' % (t,))
            except Exception:
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
        B = pd.DBColumnBinding
        conn = self._dconnection
        # stat
        key = B('stat', 'cstat', 'stat')
        dstat_spec = pd.DataFactory(
            pd.DBDataDefault,
            (key, (B('nazev', 'cstat', 'nazev'))),
            key)
        dstat = dstat_spec.create(connection_data=conn)
        dstat1_spec = pd.DataFactory(
            pd.DBDataDefault,
            (key, (B('nazev', 'cstat', 'nazev'))),
            key)
        dstat1 = dstat1_spec.create(connection_data=conn)
        # osnova
        key = B('id', 'cosnova', 'id')
        dosnova_spec = pd.DataFactory(
            pd.DBDataDefault,
            (key,
             B('synt', 'cosnova', 'synte'), B('anal', 'cosnova', 'anal'),
             B('popis', 'cosnova', 'popis'),
             B('druh', 'cosnova', 'druh'),
             B('stat', 'cosnova', 'stat', enumerator=pd.DataEnumerator(dstat_spec)),
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
                        type_=pd.Date(format=pd.Date.DEFAULT_FORMAT)),
                      B('castka', 'denik', 'castka'),
                      B('', 'denik', 'madati',
                        related_to=madati, enumerator=dosnova_spec),
                      B('', 'cosnova', 'stat', related_to=stat, enumerator=dstat_spec),
                      B('stat-nazev', 'cstat', 'nazev'),
                      B('cosi-popis', 'xcosi', 'popis'),
                      madati,
                      stat,
                      cosi)
        d = pd.DBDataDefault(
            denik_spec,
            key,
            conn)
        key = B('id', 'xcosi', 'id')
        dcosi = pd.DBDataDefault(
            (key,
             B('popis', 'xcosi', 'popis')),
            key,
            conn)
        self._dcosi_condition = pd.DBDataDefault(
            (key,
             B('popis', 'xcosi', 'popis')),
            key, conn,
            condition=pd.AND(
                pd.GE('id', ival(3)),
                pd.LT('id', ival(6))))
        # dist
        key = B('x', 'dist', 'x')
        dist = pd.DBDataDefault(
            (key,
             B('y', 'dist', 'y')),
            key, conn, distinct_on=('y',))
        dist1 = pd.DBDataDefault(
            (key,
             B('y', 'dist', 'y')),
            key, conn, distinct_on=('x',))
        # bin
        key = B('id', 'bin', 'id')
        dbin = pd.DBDataDefault(
            (key,
             B('data', 'bin', 'data'),),
            key,
            conn)
        # fulltext
        key = B('id', 'fulltext', 'id')
        fulltext = pd.DBDataDefault(
            (key,
             B('text1', 'fulltext', 'text1'),
             B('text2', 'fulltext', 'text2'),
             B('index', 'fulltext', 'index'),),
            key,
            conn)
        fulltext1 = pd.DBDataDefault(
            (key,
             B('text1', 'fulltext', 'text1'),
             B('text2', 'fulltext', 'text2'),
             B('index', 'fulltext', 'index',
               type_=pd.FullTextIndex(columns=('text1', 'text2',))),),
            key,
            conn)
        # dateformats
        key = B('id', 'dateformats', 'id')
        dateformats = pd.DBDataDefault(
            (key,
             B('datetime', 'dateformats', 'datetime', type_=pd.ISODateTime()),),
            key,
            conn)
        # timezones
        key = B('id', 'timezones', 'id')
        timezones = pd.DBDataDefault(
            (key,
             B('dt', 'timezones', 'dt', type_=pd.DateTime(without_timezone=True)),
             B('dttz', 'timezones', 'dttz', type_=pd.DateTime()),
             B('t', 'timezones', 't', type_=pd.Time(without_timezone=True)),
             B('ttz', 'timezones', 'ttz', type_=pd.Time()),),
            key,
            conn)
        # ranges
        if self._connector.server_version >= 90200:
            key = B('x', 'rangetable', 'x')
            ranges = pd.DBDataDefault(
                (key,
                 B('r', 'rangetable', 'r', type_=pd.IntegerRange()),
                 B('r2', 'rangetable', 'r2',
                   type_=pd.IntegerRange(lower_inc=False, upper_inc=True)),
                 B('rdt', 'rangetable', 'rdt',
                   type_=pd.DateTimeRange(without_timezone=True)),),
                key,
                conn)
        # arrays
        key = B('x', 'arraytable', 'x')
        arrays = pd.DBDataDefault(
            (key,
             B('a', 'arraytable', 'a', type_=pd.Array(inner_type=pd.Integer())),
             B('b', 'arraytable', 'b', type_=pd.Array(inner_type=pd.String())),),
            key,
            conn)
        # views
        key = B('x', 'viewtest1', 'x')
        view = pd.DBDataDefault((key,), key, conn)
        key = B('x', 'viewtest3', 'x')
        view3 = pd.DBDataDefault((key,), key, conn)
        key = B('x', 'viewtest4', 'x')
        view4 = pd.DBDataDefault((key,), key, conn)
        key = B('x', 'viewtest5', 'x')
        col = B('y', 'viewtest5', 'y')
        view5 = pd.DBDataDefault((key, col,), key, conn)
        key = B('x', 'viewtest7', 'x')
        col = B('y', 'viewtest7', 'y')
        view7 = pd.DBDataDefault((key, col,), key, conn)
        key = B('x', 'rudeview', 'x')
        rudeview = pd.DBDataDefault((key,), key, conn)
        key = B('id', 'tablefunc', 'id', type_=pd.Integer())
        col = B('popis', 'tablefunc', 'popis', type_=pd.String())
        funcdata = pd.DBDataDefault((key, col,), key, conn, arguments=(key,))
        # atributy
        self.data = d
        self.dstat = dstat
        self.dstat1 = dstat1
        self.dosnova = dosnova
        self.dcosi = dcosi
        self.dist = dist
        self.dist1 = dist1
        self.dbin = dbin
        self.fulltext = fulltext
        self.fulltext1 = fulltext1
        self.dateformats = dateformats
        self.timezones = timezones
        if self._connector.server_version >= 90200:
            self.ranges = ranges
        self.arrays = arrays
        self.view = view
        self.view3 = view3
        self.view4 = view4
        self.view5 = view5
        self.view7 = view7
        self.rudeview = rudeview
        self.funcdata = funcdata
        self._to_kill = [d, dstat, dstat1, dosnova, dcosi, view]
        # row data
        row = []
        for c, v in zip(self.data.columns(), self.NEWROW):
            v, e = c.type().validate(v)
            if e is not None:
                raise e
            row.append((c.id(), v))
        self.newrow = pd.Row(row)

    def tearDown(self):
        if hasattr(self, '_to_kill'):
            for d in self._to_kill:
                d.sleep()
        _DBTest.tearDown(self)

    def test_constructor(self):
        # Již otestováno v setUp
        pass

    def test_row(self):
        t = pd.Integer()
        for x in ('0', '1'):
            self.assertIsNone(self.data.row((t.validate(x)[0],)), 'nonselectable row selected')
        for x, r in (('2', self.ROW1), ('3', self.ROW2)):
            result = self.data.row((t.validate(x)[0],))
            for i in range(len(result) - 1):
                v = result[i].value()
                self.assertEqual(v, r[i], ('row doesn\'t match', v, r[i]))
        result = self.data.row((t.validate('2')[0],),
                               columns=('castka', 'stat-nazev',))
        self.assertEqual(len(result), 2, ('invalid number of columns', len(result),))
        for i, j in ((0, 2,), (1, 3,)):
            self.assertNotEqual(result[i], self.ROW1[j],
                                ('invalid response', i, result[i], self.ROW1[j]))

    def test_unique(self):
        self.assertTrue(self.dstat.find_column('stat').type().unique())
        self.assertTrue(self.dstat.find_column('nazev').type().unique())
        self.assertTrue(self.dosnova.find_column('id').type().unique())
        for colname in ('popis', 'druh', 'stat',):
            self.assertFalse(self.dosnova.find_column(colname).type().unique(), colname)

    def test_select_fetch(self, arguments={}):
        self.data.select(arguments=arguments)
        for r in (self.ROW1, self.ROW2):
            result = self.data.fetchone()
            self.assertIsNotNone(result, 'missing lines')
            for i in range(len(r)):
                self.assertEqual(r[i], result[i].value(),
                                 ('invalid value', r[i], result[i].value()))
        self.assertIsNone(self.data.fetchone(), 'too many lines')
        self.assertIsNone(self.data.fetchone(), 'data reincarnation')
        self.data.close()
        self.data.select(arguments=arguments, limit=1)
        self.assertIsNotNone(self.data.fetchone())
        self.assertIsNone(self.data.fetchone())
        self.data.close()

    def test_limited_select(self):
        self.data.select(columns=('castka', 'stat-nazev',))
        for r in (self.ROW1, self.ROW2):
            result = self.data.fetchone()
            self.assertIsNotNone(result, 'missing lines')
            for orig_col, result_col in ((2, 0,), (3, 1,),):
                self.assertEqual(r[orig_col], result[result_col].value())
        self.assertIsNone(self.data.fetchone(), 'too many lines')
        self.assertIsNone(self.data.fetchone(), 'data reincarnation')
        self.data.close()
        # Search in limited select OK?
        self.dosnova.select(columns=('id', 'synt', 'anal', 'danit',))
        result = self.dosnova.search(pd.EQ('popis', sval('efgh')))
        self.assertEqual(result, 3, ('Invalid search result', result))
        self.dosnova.close()
        # .row in limited search still working?
        self.data.select(columns=('castka', 'stat-nazev',))
        result = self.data.row((pd.Integer().validate('2')[0],))
        for i in range(len(result) - 1):
            v = result[i].value()
            self.assertEqual(v, self.ROW1[i], ('row doesn\'t match', v, r[i]))
        self.data.close()

    def test_select_map(self):
        result = self.data.select_map(lambda row: (row, 'foo'))
        for r, x in zip((self.ROW1, self.ROW2), result):
            self.assertEqual(x[1], 'foo')
            xx = x[0]
            for i in range(len(r)):
                self.assertEqual(r[i], xx[i].value(), ('invalid value', r[i], xx[i].value()))

    def test_select_fetch_direction(self):
        self.data.select()
        F, B = pd.FORWARD, pd.BACKWARD
        R1, R2 = self.ROW1, self.ROW2
        for d, r in ((B, None), (F, R1), (B, None), (F, R1), (F, R2),
                     (B, R1), (F, R2), (F, None), (F, None), (B, R2), (B, R1),
                     (B, None)):
            print d, r and r[0]
            result = self.data.fetchone(direction=d)
            print result and result[0].value()
            if r:
                assert result is not None
                assert r == tuple(v.value() for v in result.values())
            else:
                assert result is None
        self.data.close()

    def test_select_condition(self):
        v = ival(2)
        condition = pd.AND(pd.EQ('cislo', v))
        self.data.select(condition)
        for r in (self.ROW1,):
            result = self.data.fetchone()
            self.assertIsNotNone(result, 'missing lines')
            for i in range(len(r)):
                self.assertEqual(r[i], result[i].value(),
                                 ('invalid value', r[i], result[i].value()))
        self.assertIsNone(self.data.fetchone(), 'too many lines')
        self.assertIsNone(self.data.fetchone(), 'data reincarnation')
        self.data.close()
        self.data.select(pd.GT('castka', 'cislo'))
        rows = []
        while True:
            row = self.data.fetchone()
            if row is None:
                break
            rows.append(row)
        self.data.close()
        self.assertEqual(len(rows), 2)

        def nrows_test(condition, nrows):
            self.dcosi.select(condition)
            n = 0
            while self.dcosi.fetchone():
                n = n + 1
            self.dcosi.close()
            self.assertEqual(n, nrows)
        # NULL test
        nrows_test(pd.EQ('popis', sval(None)), 1)
        # Function test
        nrows_test(pd.GT(pd.OpFunction('pow', 'id', ival(2)), ival(10)), 2)
        # ANY_OF
        nrows_test(pd.ANY_OF('popis', sval('specialni'), sval('zvlastni'),
                                     sval('podivny'), sval(None)), 3)

    def test_select_special_characters(self):
        d = self.dcosi
        for v in ("'...", "\\'...", "'...\x00", "'...\n"):
            condition = pd.AND(pd.EQ('popis', pd.sval(v)))
            try:
                d.select(condition)
                row = d.fetchone()
            except ValueError as e:
                # psycopg2 doesn't allow null bytes since version 2.7.
                if str(e) != 'A string literal cannot contain NUL (0x00) characters.':
                    raise
            self.assertIsNone(row, 'too many lines')
            d.close()

    def test_select_sorting(self):
        A = pd.ASCENDENT
        D = pd.DESCENDANT
        d = self.dosnova
        TESTS = ([(('synt', D), ('stat', A)),
                  (('101', '   '), ('100', '008'), ('100', '007'))],
                 [(('stat', A), 'synt'),
                  (('100', '008'), ('100', '007'), ('101', '   '))],
                 [('anal', 'synt'),
                  (('101', '   '), ('100', '007'), ('100', '008'))])
        for spec, result in TESTS:
            d.select(sort=spec)
            for r in result:
                row = d.fetchone()
                self.assertIsNotNone(row, 'missing lines')
                k1, k2 = r
                synt, anal = row['synt'].value(), row['anal'].value()
                self.assertEqual(synt, k1, ('bad value', synt, k1, spec))
                self.assertEqual(anal, k2, ('bad value', anal, k2, spec))
            self.assertIsNone(d.fetchone(), 'too many lines')

    def test_select_sorting_limited(self):
        A = pd.ASCENDENT
        D = pd.DESCENDANT
        d = self.dosnova
        limited_columns = ('synt', 'anal', 'popis', 'stat',)
        TESTS = ([(('synt', D), ('stat', A)),
                  (('101', '   '), ('100', '008'), ('100', '007'))],
                 [(('stat', A), 'synt'),
                  (('100', '008'), ('100', '007'), ('101', '   '))],
                 [('anal', 'synt'),
                  (('101', '   '), ('100', '007'), ('100', '008'))])
        for spec, result in TESTS:
            d.select(sort=spec, columns=limited_columns)
            for r in result:
                row = d.fetchone()
                self.assertIsNotNone(row, 'missing lines')
                k1, k2 = r
                synt, anal = row['synt'].value(), row['anal'].value()
                self.assertEqual(synt, k1, ('bad value', synt, k1, spec))
                self.assertEqual(anal, k2, ('bad value', anal, k2, spec))
            self.assertIsNone(d.fetchone(), 'too many lines')
            d.close()

    def test_select_distinct_on(self):
        def check(d, condition, result):
            d.select(condition=condition)
            try:
                for r in result:
                    row = d.fetchone()
                    self.assertIsNotNone(row, ('missing lines', condition,))
                    x, y = r
                    self.assertTrue(x == row['x'].value() and y == row['y'].value(),
                                    ('unexpected result', condition, (x, y,),
                                     (row['x'].value(), row['y'].value(),),))
                self.assertIsNone(d.fetchone(), ('extra row', condition,))
            finally:
                d.close()
        check(self.dist, None, ((1, 1,), (3, 2,), (5, 3,),))
        check(self.dist, pd.GT('x', ival(3)), ((4, 2,), (5, 3,),))
        check(self.dist1, None, ((1, 1,), (2, 1,), (3, 2,), (4, 2,), (5, 3,),))
        self.dist.select(sort=('x',))
        self.dist.close()
        self.dist.select(sort=('y',))
        self.dist.close()
        self.dist.select(sort=(('x', pd.ASCENDENT,),))
        try:
            result = self.dist.search(pd.GT('x', ival(3)))
        finally:
            self.dist.close()
        self.assertEqual(result, 2, ('distinct on search failed', result,))

    def test_select_aggregate(self):
        d = self.data
        result = d.select_aggregate((d.AGG_MIN, 'castka')).value()
        self.assertEqual(result, 1000)
        result = d.select_aggregate((d.AGG_MAX, 'castka')).value()
        self.assertEqual(result, 2000)
        condition = pd.GT('castka', fval(1500.0))
        result = d.select_aggregate((d.AGG_AVG, 'castka'),
                                    condition=condition).value()
        self.assertEqual(result, 2000)
        result = d.select_aggregate((d.AGG_COUNT, 'castka')).value()
        self.assertEqual(result, 2)
        result = d.select_aggregate((d.AGG_SUM, 'castka')).value()
        self.assertEqual(result, 3000)

    def test_select_and_aggregate(self):
        d = self.data
        select_result, aggregate_result = d.select_and_aggregate(d.AGG_SUM)
        self.assertEqual(select_result, 2)
        self.assertEqual(aggregate_result[0].value(), 5)
        self.assertIsNone(aggregate_result[1].value())
        self.assertEqual(aggregate_result[2].value(), 3000)
        self.assertIsNone(aggregate_result[3].value())
        value = fval(2000.0)
        select_result, aggregate_result = \
            d.select_and_aggregate(d.AGG_MAX, columns=('castka',),
                                   condition=pd.GE('castka', value))
        self.assertEqual(select_result, 1)
        self.assertEqual(aggregate_result[0].value(), 2000)
        select_result, aggregate_result = d.select_and_aggregate(d.AGG_COUNT)
        self.assertEqual(select_result, 2)
        self.assertEqual(aggregate_result[0].value(), 2)

    def test_constructor_condition(self):
        d = self._dcosi_condition
        self.assertIsNone(d.row(ival(2)), 'Excluded row found in limited data object')
        self.assertIsNotNone(d.row(ival(3)), 'Row not found in limited data object')

        def test_select(condition, n):
            d.select(condition=condition)
            try:
                i = 0
                while d.fetchone() is not None:
                    i = i + 1
                self.assertEqual(i, n, ('Invalid number of rows in a limited select',
                                        condition, n, i,))
            finally:
                d.close()
        test_select(None, 2)
        test_select(pd.LT('id', ival(5)), 1)
        test_select(pd.GT('id', ival(6)), 0)

    def test_async_count(self, arguments={}):
        count = self.data.select(async_count=True, arguments=arguments)
        assert hasattr(count, 'count')
        assert count.count(1)[0] > 1
        assert count.count()[0] == 2
        self.data.close()

    def test_async_select(self, arguments={}):
        self.data.select(async_count=True, arguments=arguments)
        for r in (self.ROW1, self.ROW2):
            result = self.data.fetchone()
            assert result is not None
            assert tuple(v.value() for v in result.values()) == r
        assert self.data.fetchone() is None
        assert self.data.fetchone() is None
        self.data.close()

    def test_dummy_select(self):
        UNKNOWN_ARGUMENTS = self.data.UNKNOWN_ARGUMENTS
        self.test_select_fetch(arguments=UNKNOWN_ARGUMENTS)
        self.test_async_select(arguments=UNKNOWN_ARGUMENTS)
        self.assertEqual(self.funcdata.select(arguments=UNKNOWN_ARGUMENTS), 0)
        self.assertIsNone(self.funcdata.fetchone())
        count = self.funcdata.select(arguments=UNKNOWN_ARGUMENTS, async_count=True)
        result = count.count()
        self.assertEqual(result[0], 0)
        self.assertIsNone(self.funcdata.fetchone())
        self.assertEqual(self.funcdata.select(arguments=UNKNOWN_ARGUMENTS), 0)
        self.assertEqual(self.funcdata.search(None, arguments=UNKNOWN_ARGUMENTS), 0)

    def test_restore_select(self):
        d = self.dcosi
        condition = pd.EQ('id', pd.ival(3))
        d.select()
        result = d.search(condition)
        self.assertEqual(result, 2)
        d.skip(result)
        d.close()
        result = d.search(condition)
        self.assertEqual(result, 0)
        d.close()

    def test_insert(self):
        row = self.newrow
        result, success = self.data.insert(row)
        self.assertTrue(success)
        eresult = []
        for c, v in zip(self.data.columns(), self.ROW3):
            eresult.append((c.id(), c.type().validate(v)[0]))
        eresult = pd.Row(eresult)
        self.assertEqual(result, eresult, 'insertion failed')
        result2 = self.data.insert(row)
        self.assertIs(result2[1], False, 'invalid insertion succeeded')
        self.assertTrue(result2[0] is None or isinstance(result2[0], basestring),
                        'invalid failed insertion result')

    def test_insert_view(self):
        row = pd.Row((('x', ival(5),),))
        result, success = self.view3.insert(row)
        self.assertTrue(success)
        self.assertEqual(result['x'].value(), 5)
        result, success = self.view4.insert(row)
        self.assertTrue(success)
        self.assertEqual(result['x'].value(), 5)
        row = pd.Row((('y', ival(5),),))
        result, success = self.view7.insert(row)
        self.assertTrue(success)
        self.assertIsNone(result, ('unexpected insert result', result,))
        result, success = self.view5.insert(row)
        self.assertTrue(success)
        self.assertIsNone(result, ('unexpected insert result', result,))

    def test_update(self):
        row = self.newrow
        row1 = []
        for c, v in zip(self.data.columns(), self.ROW1):
            row1.append((c.id(), pd.Value(c.type(), v)))
        row1 = pd.Row(row1)
        k1 = row1[0]
        k2 = pd.Value(self.data.columns()[0].type(), self.ROW2[0])
        result, success = self.data.update(k1, row)
        self.assertTrue(success)
        eresult = []
        for c, v in zip(self.data.columns(), self.ROW3):
            eresult.append((c.id(), c.type().validate(v)[0]))
        eresult = pd.Row(eresult)
        self.assertEqual(result, eresult, 'update failed')
        for k in k1, k2:
            result2 = self.data.update(k, row)
            self.assertIs(result2[1], False, 'invalid update succeeded')
            self.assertTrue(result2[0] is None or isinstance(result2[0], basestring),
                            'invalid failed update result')
            result2 = self.data.update(k, row)
        self.assertEqual(self.data.update(row[0], row1)[0], row1, 'update failed')

    def test_view_update(self):
        t = pd.Integer()
        key = t.validate('2')[0]
        row = pd.Row((('x', t.validate('3')[0]),))
        result, success = self.view.update(key, row)
        self.assertTrue(result and success, 'view update failed')

    def test_update_many(self):
        row = self.newrow
        row1 = []
        for c, v in zip(self.data.columns(), self.ROW1):
            row1.append((c.id(), pd.Value(c.type(), v)))
        row1 = pd.Row(row1)
        k1 = row1[0]
        k2 = pd.Value(self.data.columns()[0].type(), self.ROW2[0])
        result = self.data.update_many(pd.EQ('cislo', k1), row)
        self.assertEqual(result, 1, 'update failed')
        self.assertEqual(self.data.update_many(pd.EQ('cislo', k1), row), 0,
                         'invalid update succeeded')
        try:
            ok = True
            self.data.update_many(pd.EQ('cislo', k2), row)
            ok = False
        except pd.DBException:
            pass
        self.assertTrue(ok, 'invalid update succeeded')
        self.assertEqual(self.data.update_many(pd.EQ('cislo', row[0]), row1), 1,
                         'update failed')

    def test_delete(self):
        def lines(keys, self=self):
            n = len(keys)
            result = self._sql_command('select id from denik order by id')
            self.assertEqual(len(result), n, ('invalid number of rows', len(result), n))
            for i in range(n):
                v = result[i][0]
                self.assertEqual(keys[i], v, ('nonmatching key', keys[i], v))
        t = pd.Integer()
        self.assertEqual(self.data.delete(t.validate('0')[0]), 0, 'nonexistent row deleted')
        lines((1, 2, 3, 4))
        self.assertEqual(self.data.delete(t.validate('1')[0]), 1, 'row not deleted')
        lines((2, 3, 4))
        self.assertEqual(self.data.delete(t.validate('1')[0]), 0, 'row deleted twice')
        lines((2, 3, 4))
        self.assertEqual(self.data.delete(t.validate('4')[0]), 1, 'row not deleted')
        lines((2, 3))

    def test_delete_many(self):
        def lines(keys, self=self):
            n = len(keys)
            result = self._sql_command('select id from denik order by id')
            self.assertEqual(len(result), n, ('invalid number of rows', len(result), n))
            for i in range(n):
                v = result[i][0]
                self.assertEqual(keys[i], v, ('nonmatching key', keys[i], v))
        F = pd.Float(digits=17, precision=2)
        x999 = F.validate('999')[0]
        x1000 = F.validate('1000')[0]
        x3000 = F.validate('3000')[0]
        self.assertEqual(self.data.delete_many(pd.EQ('castka', x999)), 0,
                         'nonexistent row deleted')
        lines((1, 2, 3, 4))
        self.assertEqual(self.data.delete_many(pd.EQ('castka', x1000)), 2,
                         'rows not deleted')
        lines((3, 4))
        self.assertEqual(self.data.delete_many(pd.EQ('castka', x1000)), 0,
                         'rows deleted twice')
        lines((3, 4))
        self.assertEqual(self.data.delete_many(pd.EQ('castka', x3000)), 1,
                         'row not deleted')
        lines((3,))

    def test_table_function(self):
        id_value = ival(3)
        try:
            self.assertEqual(self.funcdata.select(arguments=dict(id=id_value)), 2)
            self.assertIsNotNone(self.funcdata.fetchone())
            self.assertIsNotNone(self.funcdata.fetchone())
            self.assertIsNone(self.funcdata.fetchone())
        finally:
            self.funcdata.close()
        try:
            result = [v.value() for v in self.funcdata.distinct('id', arguments=dict(id=id_value))]
            self.assertEqual(len(result), 2)
            self.assertIn(5, result)
            self.assertIn(999, result)
        finally:
            self.funcdata.close()

    def test_binary(self):
        t = pd.Binary()
        null_data, error = t.validate(None)
        self.assertIsNone(error, ('Null Binary validation failed', error,))
        self.assertIsNone(null_data.value(), ('Invalid null binary value', null_data.value()))
        data = [chr(i) for i in range(256)]
        data1, error = t.validate(buffer(string.join(data, '')))
        self.assertIsNone(error, ('Binary validation failed', error,))
        self.assertIsInstance(data1.value(), pd.Binary.Buffer,
                              ('Invalid binary value', data1.value(),))
        self.assertEqual(str(data1.value()), string.join(data, ''),
                         ('Invalid binary value', data1.value(),))
        data.reverse()
        key = pd.ival(1)
        row1 = pd.Row([('id', key,), ('data', data1,)])
        result, success = self.dbin.insert(row1)
        self.assertTrue(success, 'Binary insertion failed')
        self.assertEqual(str(result[1].value()), str(data1.value()),
                         ('Invalid inserted binary data', str(result[1].value())))
        result = str(self.dbin.row(key)[1].value())
        self.assertEqual(result, str(data1.value()), ('Invalid binary data', result,))
        data2, error = t.validate(buffer(string.join(data, '')))
        self.assertIsNone(error, ('Binary validation failed', error,))
        self.assertIsInstance(data2.value(), pd.Binary.Buffer,
                              ('Invalid binary value', data2.value(),))
        row2 = pd.Row([('id', key,), ('data', data2,)])
        result, succes = self.dbin.update(key, row2)
        self.assertTrue(success, 'Binary update failed')
        self.assertEqual(str(result[1].value()), str(data2.value()),
                         ('Invalid updated binary data', str(result[1].value()),))
        result = str(self.dbin.row(key)[1].value())
        self.assertEqual(result, str(data2.value()), ('Invalid binary data', result,))
        self.assertEqual(self.dbin.delete(key), 1, 'Binary deletion failed')

    def test_full_text_select(self):
        ts_config = self._sql_command("select get_current_ts_config()")[0][0]
        self.assertEqual(ts_config, 'simple',
                         "Wrong ts_config for full text search tests.\n"
                         "Use the following SQL command as a database owner to fix it:\n"
                         "ALTER DATABASE ... SET default_text_search_config to 'simple';")

        def check(query, result_set):
            condition = pd.FT('index', query)
            self.fulltext.select(condition=condition, sort=('index',))
            result_ids = []
            while True:
                row = self.fulltext.fetchone()
                if row is None:
                    break
                result_ids.append(row[0].value())
            self.fulltext.close()
            self.assertEqual(result_set, result_ids)

        def check1(query, result_set):
            condition = pd.FT('index', query)
            self.fulltext1.select(condition=condition, sort=('index',))
            result_samples = []
            while True:
                row = self.fulltext1.fetchone()
                if row is None:
                    break
                result_samples.append(row[3].value())
            self.fulltext1.close()
            self.assertEqual(result_samples, result_set)
        check('nobody&likes&me', [])
        check('lazy&fox', [3, 2])
        check1('lazy&fox', ["The quick brown <b>fox</b> jumps over the <b>lazy</b> dog. * cat",
                            "GNU's Not Unix * <b>lazy</b> <b>fox</b> and <b>lazy</b> dog"])
        check1('world', ["Hello, <b>world</b>! * bear"])

    def test_dateformats(self):
        data = self.dateformats
        row = data.row(pd.ival(1))
        self.assertIsNotNone(row)
        value = row[1].value()
        delta = datetime.datetime.now(pd.DateTime.UTC_TZINFO) - value
        self.assertGreaterEqual(delta, datetime.timedelta(), value)
        self.assertLess(delta, datetime.timedelta(seconds=3600), value)

    def test_timezones(self):
        data = self.timezones
        V = pd.Value
        moment = datetime.datetime(2015, 7, 1, 12, 0, 0)
        moment_tz = moment.replace(tzinfo=pd.DateTime.LOCAL_TZINFO)
        dt_val = V(pd.DateTime(without_timezone=True), moment)
        dttz_val = V(pd.DateTime(), moment_tz)
        t_val = V(pd.Time(without_timezone=True), moment.time())
        ttz_val = V(pd.Time(), moment_tz.timetz())
        key_val = pd.ival(1)

        def check_row():
            row = data.row(key_val)
            self.assertEqual(row['dt'], dt_val)
            self.assertEqual(row['dttz'], dttz_val)
            self.assertEqual(row['t'], t_val)
            self.assertIsNone(row['ttz'].value())
        self.assertRaises(pd.DBUserException,
                          data.insert, pd.Row((('id', key_val),
                                                       ('dt', dttz_val), ('dttz', dt_val),
                                                       ('t', ttz_val), ('ttz', t_val),)))
        data.insert(pd.Row((('id', key_val),
                                    ('dt', dttz_val), ('dttz', dt_val),
                                    ('t', ttz_val), ('ttz', pd.tval(None)),)))
        check_row()
        self.assertRaises(pd.DBUserException,
                          data.update, key_val, pd.Row((('dt', dttz_val), ('dttz', dt_val),
                                                                ('t', ttz_val), ('ttz', t_val),)))
        data.update(key_val, pd.Row((('dt', dttz_val), ('dttz', dt_val),
                                             ('t', ttz_val), ('ttz', pd.tval(None)),)))
        check_row()

    def test_ranges(self):
        if self._connector.server_version < 90200:
            return
        IR = pd.IntegerRange()
        IR2 = pd.IntegerRange(lower_inc=False, upper_inc=True)
        IR3 = pd.IntegerRange(lower_inc=False, upper_inc=False)
        DR = pd.DateTimeRange(without_timezone=True)
        # Basic tests
        data = self.ranges
        row = data.row(pd.ival(1))
        self.assertIsNotNone(row)
        value = row[1].value()
        self.assertEqual(value[0], 10)
        self.assertEqual(value[1], 20)
        value = row[2].value()
        self.assertEqual(value.lower(), 9)
        self.assertEqual(value.upper(), 19)
        new_value, err = IR.validate(('20', '30',))
        self.assertIsNone(err)
        new_value_2, err = IR2.validate(('19', '29',))
        self.assertIsNone(err)
        self.assertEqual(new_value.value(), new_value_2.value())
        rdt_value, err = pd.DateTimeRange(without_timezone=True)\
                                   .validate(('2014-02-01 00:00:00', '2014-02-01 00:00:02',))
        self.assertIsNone(err)
        data.insert(pd.Row((('x', pd.ival(2),), ('r', new_value,),
                                    ('r2', new_value_2,), ('rdt', rdt_value,),)))
        for column in ('r', 'r2',):
            for value in (new_value, new_value_2,):
                n = data.select(pd.EQ(column, value))
                try:
                    self.assertEqual(n, 1)
                    row = data.fetchone()
                finally:
                    data.close()
        self.assertEqual(row['r'], new_value)
        self.assertEqual(row['r2'].value(), new_value.value())
        self.assertEqual(row['r2'].value(), new_value_2.value())
        self.assertEqual(row['rdt'], rdt_value)
        new_value, err = IR.validate(('40', '50',))
        self.assertIsNone(err)
        rdt_value, err = pd.DateTimeRange(without_timezone=True)\
                                   .validate(('2014-03-01 00:00:00', '2014-03-01 00:00:02',))
        self.assertIsNone(err)
        data.update(pd.ival(2), pd.Row((('r', new_value,), ('r2', new_value,),
                                                        ('rdt', rdt_value,),)))
        new_value, err = IR.validate(('', '',))
        self.assertIsNone(err)
        self.assertIsNone(new_value.value())
        data.insert(pd.Row((('x', pd.ival(3),), ('r', new_value,),
                                    ('r2', new_value,), ('rdt', new_value,),)))
        row = data.row(pd.ival(3))
        self.assertIsNotNone(row)
        value = row[1].value()
        self.assertIsNone(value)

        # Range operators
        def test_condition(n_rows, condition):
            try:
                self.assertEqual(n_rows, data.select(condition))
            finally:
                data.close()

        def irange(x, y):
            return pd.Value(IR, IR.Range(x, y))

        def irange2(x, y):
            return pd.Value(IR2, IR2.Range(x, y))

        def drange(x, y):
            return pd.Value(DR, (datetime.datetime(*x), datetime.datetime(*y),))
        test_condition(1, pd.RangeContains('r', irange(15, 18)))
        test_condition(1, pd.RangeContained('r', irange(30, 60)))
        test_condition(2, pd.RangeOverlap('r', irange(0, 100)))
        test_condition(0, pd.RangeOverlap('r', irange(25, 35)))
        test_condition(1, pd.RangeOverlap('rdt', drange((2014, 2, 1, 0, 0, 0,),
                                                                (2014, 4, 1, 0, 0, 0,))))
        # Inclusive / non-inclusive bounds
        test_condition(0, pd.RangeOverlap('r', irange(20, 30)))
        test_condition(1, pd.RangeOverlap('r', irange(19, 30)))
        test_condition(0, pd.RangeOverlap('r', irange2(19, 30)))
        test_condition(1, pd.RangeOverlap('r', irange2(18, 30)))
        test_condition(0, pd.RangeOverlap('r', irange(0, 10)))
        test_condition(1, pd.RangeOverlap('r', irange2(0, 10)))
        # Unbound values
        value, err = IR.validate(('1', '',))
        self.assertIsNone(err)
        self.assertEqual(value.value().lower(), 1)
        self.assertEqual(value.value().upper(), None)
        test_condition(1, pd.RangeOverlap('r', irange(30, None)))
        test_condition(1, pd.RangeOverlap('r', irange(None, 30)))
        # Other checks
        value, err = IR.validate(('2', '2'))
        self.assertIsNone(err)
        value, err = IR3.validate(('2', '2'))
        self.assertIsNone(err)
        value, err = IR.validate(('2', '1'))
        self.assertIsNotNone(err)
        value, err = DR.validate(('2015-01-01 12:00:00', '2015-01-01 12:00:00'))
        self.assertIsNone(err)
        value, err = DR.validate(('2015-01-01 12:00:01', '2015-01-01 12:00:00'))
        self.assertIsNotNone(err)
        pd.Value(IR, IR.Range(1, 1))
        self.assertRaises(TypeError, pd.Value, IR, IR.Range(1, 0))

    def test_arrays(self):
        int_array_type = pd.Array(inner_type=pd.Integer())
        str_array_type = pd.Array(inner_type=pd.String())
        data = self.arrays
        row = data.row(pd.ival(1))
        self.assertIsNotNone(row)
        value = row[1].value()
        self.assertTrue(value[0].value() == 2 and value[1].value() == 3, value)
        value = row[2].value()
        self.assertTrue(value[0].value() == 'hello' and value[1].value() == 'world', value)
        new_value_a, err = int_array_type.validate(('20', '30',))
        self.assertIsNone(err)
        new_value_b, err = str_array_type.validate(('bye', 'world',))
        self.assertIsNone(err)
        data.insert(pd.Row((('x', pd.ival(2),), ('a', new_value_a,),
                                    ('b', new_value_b,),)))
        n = data.select(pd.EQ('a', new_value_a))
        self.assertEqual(n, 1)
        row = data.fetchone()
        data.close()
        self.assertEqual(row['a'], new_value_a)
        self.assertEqual(row['b'], new_value_b)
        n = data.select(pd.EQ('b', new_value_b))
        self.assertEqual(n, 1)
        row = data.fetchone()
        data.close()
        self.assertEqual(row['a'], new_value_a)
        self.assertEqual(row['b'], new_value_b)
        new_value, err = int_array_type.validate(('40', '50',))
        self.assertIsNone(err)
        data.update(pd.ival(2), pd.Row((('b', new_value,),)))
        new_value, err = int_array_type.validate(())
        self.assertIsNone(err)
        self.assertEqual(new_value.value(), ())
        row = data.row(pd.ival(99))
        self.assertIsNotNone(row)
        self.assertEqual(row[1].value(), ())
        self.assertEqual(row[2].value(), ())
        empty_a, err = int_array_type.validate(())
        self.assertIsNone(err)
        empty_b, err = str_array_type.validate(())
        self.assertIsNone(err)
        data.insert(pd.Row((('x', pd.ival(100)), ('a', empty_a), ('b', empty_b),)))
        row = data.row(pd.ival(100))
        self.assertIsNotNone(row)
        self.assertEqual(row[1].value(), ())
        self.assertEqual(row[2].value(), ())

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
        row = pd.Row(row_data)
        result, success = data.insert(row)
        self.assertTrue(success)
        self.assertEqual(result[1].value(), backslash,
                         ('invalid inserted value', result[1].value(), backslash,))
        self.assertEqual(data.delete(row[0]), 1, 'row not deleted')

    def test_lock(self):
        us = pd.String().validate('us')[0]
        cz = pd.String().validate('cz')[0]
        t1, t2 = self.dstat, self.dstat1
        transaction_1 = \
            pd.DBTransactionDefault(connection_data=self._dconnection)
        transaction_2 = \
            pd.DBTransactionDefault(connection_data=self._dconnection)
        try:
            self.assertIsNone(t1.lock_row(us, transaction_1), 'lock failed')
            result = t2.lock_row(us, transaction_2)
            self.assertIsInstance(result, str, 'unlocked record locked')
            self.assertIsNone(t2.lock_row(cz, transaction_2), 'lock failed')
            transaction_2.rollback()
            transaction_2 = pd.DBTransactionDefault(connection_data=self._dconnection)
            self.assertIsInstance(t2.lock_row(us, transaction_2), str, 'unlocked record locked')
            transaction_1.commit()
            transaction_1 = pd.DBTransactionDefault(connection_data=self._dconnection)
            self.assertIsNone(t2.lock_row(us, transaction_2), 'lock failed')
            transaction_1.rollback()
            transaction_2.commit()
        finally:
            try:
                transaction_1.rollback()
            except Exception:
                pass
            try:
                transaction_2.rollback()
            except Exception:
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
            pd.DBTransactionDefault(connection_data=self._dconnection)
        transaction_2 = \
            pd.DBTransactionDefault(connection_data=self._dconnection)
        try:
            result = v.lock_row(key, transaction=transaction)
            self.assertIsNone(result, 'lock failed')
            result = v.lock_row(key, transaction=transaction_2)
            self.assertIsInstance(result, str, 'locked record locked')
            result = v.lock_row(key3, transaction=transaction_2)
            self.assertIsNone(result, 'additional row locking failed')
            result = v2.lock_row(key2, transaction=transaction)
            self.assertIsNone(result, 'lock failed')
            result = v2.lock_row(key2, transaction=transaction_2)
            self.assertIsNone(result, 'unlockable view locked')
        finally:
            for t in transaction, transaction_2:
                try:
                    t.rollback()
                except Exception:
                    pass

    def _perform_transaction(self, transaction):
        d = self.dstat
        d1 = self.dstat1

        def v(s):
            return pd.String().validate(s)[0]
        i_row0 = pd.Row((('stat', v('cs'),), ('nazev', v('Cesko'),)))
        i_row00 = pd.Row((('stat', v('cc'),), ('nazev', v('CC'),)))
        d.insert(i_row0)
        d.insert(i_row00)
        i_row1 = pd.Row((('stat', v('xx'),), ('nazev', v('Xaxa'),)))
        i_row2 = pd.Row((('stat', v('yy'),), ('nazev', v('Yaya'),)))
        u_key1 = i_row2[0]
        u_row1 = pd.Row((('nazev', v('Gaga'),),))
        u_condition_2 = pd.EQ('stat', v('cz'))
        u_row2 = pd.Row((('nazev', v('Plesko'),),))
        d_key = i_row1[0]
        d_condition = pd.EQ('nazev', v('CC'))
        d.insert(i_row1, transaction=transaction)
        d1.insert(i_row2, transaction=transaction)
        d.lock_row(u_key1, transaction)
        d.update(u_key1, u_row1, transaction=transaction)
        d1.update_many(u_condition_2, u_row2, transaction=transaction)
        d.delete(d_key, transaction=transaction)
        d1.delete_many(d_condition, transaction=transaction)
        d.select(sort=('stat',), transaction=transaction)
        for k in ('cs', 'cz', 1, 'yy',):
            if isinstance(k, int):
                d.skip(k)
            else:
                value = d.fetchone()[0].value()
                self.assertEqual(value, k, ('invalid select value', k, value,))
        d.close()

    def test_transaction_commit(self):
        d = self.dstat
        transaction = \
            pd.DBTransactionDefault(connection_data=self._dconnection)
        try:
            self._perform_transaction(transaction)
        finally:
            transaction.commit()
        for k, v in (('cs', 'Cesko',), ('xx', None,), ('yy', 'Gaga',),
                     ('cz', 'Plesko',), ('cc', None,),):
            result = d.row(pd.String().validate(k)[0])
            if v is None:
                self.assertIsNone(result, ('deleted value present', k,))
            else:
                self.assertIsNotNone(result, ('value not present', k,))
                self.assertEqual(result['nazev'].value(), v,
                                 ('invalid value', k, result['nazev'].value(),))

    def test_transaction_rollback(self):
        d = self.dstat
        transaction = \
            pd.DBTransactionDefault(connection_data=self._dconnection)
        try:
            self._perform_transaction(transaction)
        finally:
            transaction.rollback()
        for k, v in (('cs', 'Cesko',), ('xx', None,), ('yy', None,),
                     ('cz', 'Czech Republic',), ('cc', 'CC',),):
            result = d.row(pd.String().validate(k)[0])
            if v is None:
                self.assertIsNone(result, ('deleted value present', k,))
            else:
                self.assertIsNotNone(result, ('value not present', k,))
                self.assertEqual(result['nazev'].value(), v,
                                 ('invalid value', k, result['nazev'].value(),))

    def test_partial_transaction(self):
        d = self.dstat

        def v(s):
            return pd.String().validate(s)[0]
        row1 = pd.Row((('stat', v('xx'),), ('nazev', v('Xaxa'),),))
        row2 = pd.Row((('stat', v('yy'),), ('nazev', v('Yaya'),),))
        row3 = pd.Row((('stat', v('zz'),), ('nazev', v('Zaza'),),))
        transaction = \
            pd.DBTransactionDefault(connection_data=self._dconnection)
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
        self.assertIsNotNone(d.row(row1['stat']), 'missing row')
        self.assertIsNone(d.row(row2['stat']), 'extra row')
        self.assertIsNotNone(d.row(row3['stat']), 'missing row')


class DBMultiData:  # (DBDataDefault): Temporarily disabled
    ROW1 = (2, datetime.datetime(2001, 1, 2, tzinfo=pd.DateTime.UTC_TZINFO), 1000.0,
            ('100', '007'),
            'U.S.A.', 'specialni')
    ROW2 = (3, datetime.datetime(2001, 1, 2, tzinfo=pd.DateTime.UTC_TZINFO), 2000.0,
            ('100', '008'),
            'Czech Republic', 'zvlastni')
    ROW3 = ('5', '2001-07-06', '9.9', ('100', '007'), 'U.S.A.', 'nove')

    def setUp(self):
        DBDataDefault.setUp(self)
        # TODO: 'self.mdata' needs to be initialized in order to make this test work.

    def test_row(self):
        for x, r in (('2', self.ROW1), ('3', self.ROW2)):
            result = self.mdata.row(pd.Integer().validate(x)[0])
            for i in range(len(result) - 1):
                v = result[i].value()
                if isinstance(v, tuple):
                    for j in range(len(v)):
                        self.assertEqual(v[j], r[i][j], ("row doesn't match", v[i][j], r[i][j]))
                else:
                    self.assertEqual(v, r[i], ("row doesn't match", v, r[i]))

    def test_select_fetch(self):
        d = self.mdata
        d.select()
        for r in (self.ROW1, self.ROW2):
            result = d.fetchone()
            self.assertIsNotNone(result, 'missing lines')
            for i in range(len(r)):
                self.assertEqual(r[i], result[i].value(),
                                 ('invalid value', r[i], result[i].value()))
        d.close()

    def test_select_condition(self):
        d = self.mdata
        v = ival(2)
        condition = pd.AND(pd.EQ('cislo', v))
        d.select(condition)
        for r in (self.ROW1,):
            result = d.fetchone()
            self.assertIsNotNone(result, 'missing lines')
            for i in range(len(r)):
                self.assertEqual(r[i], result[i].value(),
                                 ('invalid value', r[i], result[i].value()))
        self.assertIsNone(d.fetchone(), 'too many lines')
        d.close()

    def test_select_fetch_direction(self):
        dat = self.mdata
        dat.select()
        F, B = pd.FORWARD, pd.BACKWARD
        R1, R2 = self.ROW1, self.ROW2
        n = 0
        for d, r in ((B, None), (F, R1), (B, None), (F, R1), (F, R2),
                     (B, R1), (F, R2), (F, None), (F, None), (B, R2), (B, R1),
                     (B, None)):
            result = dat.fetchone(direction=d)
            if r:
                self.assertIsNotNone(result, ('line not received', n))
                for i in range(len(r)):
                    self.assertEqual(r[i], result[i].value(),
                                     ('invalid value', r[i], result[i].value(), n))
            else:
                self.assertIsNone(result, 'data reincarnation')
            n = n + 1
        dat.close()

    def test_search(self):
        E = pd.EQ
        d = self.dosnova
        d.select()
        res = d.search(E('popis', sval('efgh')))
        self.assertEqual(res, 3)
        res = d.search(E('popis', sval('abcd')), direction=pd.FORWARD)
        self.assertEqual(res, 1)
        res = d.search(E('popis', sval('foo')))
        self.assertEqual(res, 0)
        d.fetchone()
        res = d.search(E('popis', sval('efgh')))
        self.assertEqual(res, 2)
        res = d.search(E('popis', sval('abcd')))
        self.assertEqual(res, 0)
        res = d.search(E('popis', sval('foo')))
        self.assertEqual(res, 0)
        d.fetchone()
        res = d.search(E('popis', sval('abcd')), direction=pd.FORWARD)
        self.assertEqual(res, 0)
        res = d.search(E('popis', sval('abcd')),
                       direction=pd.BACKWARD)
        self.assertEqual(res, 1)
        while d.fetchone() is not None:
            pass
        res = d.search(E('popis', sval('abcd')), direction=pd.FORWARD)
        self.assertEqual(res, 0)
        res = d.search(E('popis', sval('abcd')),
                       direction=pd.BACKWARD)
        self.assertEqual(res, 3)
        d.close()

    def test_search_key(self):
        d = self.dosnova
        d.select()
        res = d.search_key((sval('100'), sval('008')))
        self.assertEqual(res, 2)
        d.close()

    def test_insert(self):
        d = self.mdata
        row = self.newrow
        result, success = d.insert(row)
        self.assertTrue(success)
        eresult = []
        for c, v in zip(d.columns(), self.ROW3):
            eresult.append((c.id(), c.type().validate(v)[0]))
        eresult = pd.Row(eresult)
        self.assertEqual(result[:-1], eresult, 'insertion failed')
        self.assertEqual(d.insert(row), (None, False), 'invalid insertion succeeded')

    def test_update(self):
        d = self.mdata
        newrow = ('5', '2001-07-06', '9.90', ('100', '008'), 'Czech Republic',
                  'nove')
        rowdata = []
        for c, v in zip(d.columns(), newrow):
            rowdata.append((c.id(), c.type().validate(v)[0]))
        row = pd.Row(rowdata)
        row1 = []
        for c, v in zip(d.columns(), self.ROW1):
            row1.append((c.id(), pd.Value(c.type(), v)))
        row1 = pd.Row(row1)
        k1 = row1[0]
        k2 = pd.Value(d.columns()[0].type(), self.ROW2[0])
        result, success = d.update(k1, row)
        self.assertTrue(success)
        eresult = []
        for c, v in zip(d.columns(), newrow):
            eresult.append((c.id(), c.type().validate(v)[0]))
        eresult = pd.Row(eresult)
        self.assertEqual(result[:-1], eresult, 'update failed')
        self.assertEqual(d.update(k1, row), (None, False), 'invalid update succeeded')
        self.assertEqual(d.update(k2, row), (None, False), 'invalid update succeeded')
        self.assertEqual(d.update(row[0], row1)[0][:-1], row1, 'update failed')

    def test_delete(self):
        d = self.mdata

        def lines(keys, self=self):
            n = len(keys)
            result = self._sql_command('select id from denik order by id')
            self.assertEqual(len(result), n, ('invalid number of rows', len(result), n))
            for i in range(n):
                v = result[i][0]
                self.assertEqual(keys[i], v, ('nonmatching key', keys[i], v))
        self.assertEqual(d.delete(pd.Integer().validate('0')[0]), 0,
                         'nonexistent column deleted')
        lines((1, 2, 3, 4))
        self.assertEqual(d.delete(pd.Integer().validate('1')[0]), 1, 'column not deleted')
        lines((2, 3, 4))
        self.assertEqual(d.delete(pd.Integer().validate('1')
                                  [0]), 0, 'column deleted twice')
        lines((2, 3, 4))
        self.assertEqual(d.delete(pd.Integer().validate('4')[0]), 1, 'column not deleted')
        lines((2, 3))


class DBSessionVariables(_DBBaseTest):

    def setUp(self):
        _DBBaseTest.setUp(self)
        import config
        config.session_variables = {'myvar.test': 'value'}
        # Session variables are only initialized when a new connection is
        # created.  When other tests run before this on, connections are
        # already initialized in the pool and session variables don't get
        # applied.  Calling reload_session_variables() solves that...
        pd.reload_session_variables(_connection_data)
        try:
            self._sql_command("create function foo() "
                              "  returns text as "
                              "  $$select current_setting('myvar.test') as result$$ "
                              "  language sql")
        except Exception:
            self.tearDown()
            raise

    def test_it(self):
        function = pd.DBFunctionDefault('foo', self._dconnection)
        row = pd.Row()
        result = function.call(row)[0][0].value()
        self.assertEqual(result, 'value', 'session variable not set properly')
        try:
            pd.reload_session_variables(config.dbconnection)
        except Exception:
            self.tearDown()
            raise
        self.tearDown()

    def tearDown(self):
        try:
            self._sql_command("drop function foo()")
        except Exception:
            pass
        _DBBaseTest.tearDown(self)


class TestFetchSelect(DBTest):
    """Test fetching rows from DB select."""

    @pytest.fixture
    def table(self):
        try:
            self.sql("create table test (x int)")
            yield 'test'
        finally:
            try:
                self.sql('drop table test')
            except Exception:
                pass

    @pytest.fixture
    def data(self, table, dbconnection):
        try:
            key = pd.DBColumnBinding('x', table, 'x')
            data = pd.DBDataDefault((key,), key, dbconnection)
            yield data
        finally:
            try:
                data.sleep()
            except Exception:
                pass

    def _check_skip_fetch(self, d, spec, noresult=False):
        print
        d.select()
        n = 0
        for op, count in spec:
            n = n + count
            if count >= 0:
                direction = pd.FORWARD
            else:
                direction = pd.BACKWARD
                count = -count
            print 'fetch' if op == 'f' else 'skip', count, direction
            if op == 'f':
                for i in range(count):
                    d.fetchone(direction=direction)
            elif op == 's':
                d.skip(count, direction=direction)
            else:
                raise Exception('Invalid op', op)
        row = d.fetchone()
        if noresult:
            assert row is None
        else:
            assert row is not None
            assert row['x'].value() == n

    @pytest.fixture
    def rows(self, data):
        import config
        table_size = config.initial_fetch_size + config.fetch_size + 10
        self.insert(data, ((i,) for i in range(table_size)))
        return table_size

    def test_skip_fetch(self, data, rows):
        import config
        self._check_skip_fetch(data, (('f', 1),))
        self._check_skip_fetch(data, (('f', 12), ('s', 42)))
        self._check_skip_fetch(data, (('f', 12), ('s', 42), ('f', 10)))
        self._check_skip_fetch(data, (('f', 1), ('s', rows - 1),
                                      ('f', 1), ('s', -2), ('f', -1)))
        self._check_skip_fetch(data, (('f', 12), ('s', config.initial_fetch_size)))
        self._check_skip_fetch(data, (('f', 12), ('s', config.initial_fetch_size + 1), ('f', 5)))
        self._check_skip_fetch(data, (('f', 12), ('s', -6), ('f', 2)))
        self._check_skip_fetch(data, (('f', config.initial_fetch_size + 10), ('s', -16)))
        self._check_skip_fetch(data, (('f', config.initial_fetch_size + 10), ('s', -16), ('s', 20),
                                      ('f', -10), ('f', 15)))
        self._check_skip_fetch(data, (('s', config.initial_fetch_size + config.fetch_size + 3),))
        self._check_skip_fetch(data, (('s', 10 * config.initial_fetch_size + config.fetch_size),),
                               noresult=True)

    def test_small_table_skip_fetch(self, data):
        self.insert(data, ((i,) for i in range(4)))
        self._check_skip_fetch(data, (('s', 3), ('f', 1), ('s', -2), ('f', -1)))
        self._check_skip_fetch(data, (('s', 4), ('f', 1), ('s', -2), ('f', -1)))

    def test_rewind(self, data):
        self.insert(data, ((i,) for i in range(10)))
        data.select()
        for n in (0, 1, 6, 10, 12):
            for i in range(n):
                data.fetchone()
            data.rewind()
            row = data.fetchone()
            assert row is not None
            assert row['x'].value() == 0

    def test_reuse_select(self, data, rows):
        import config
        skip = config.initial_fetch_size
        data.select()
        data.skip(skip)
        for i in range(3):
            data.fetchone()
        data.select(reuse=True)
        data.skip(skip)
        row = data.fetchone()
        assert row is not None
        assert row['x'].value() == skip
        data.select(reuse=True)
        row = data.fetchone()
        assert row is not None
        assert row['x'].value() == 0

    def test_skip_result(self, data):
        self.insert(data, ((i,) for i in range(10)))
        data.select()
        assert data.skip(14) == 11
        assert data.skip(12, pd.BACKWARD) == 11
        row = data.fetchone()
        assert row is not None
        assert row['x'].value() == 0
        assert data.skip(11) == 10
        assert data.skip(10, pd.BACKWARD) == 10
        row = data.fetchone()
        assert row is not None
        assert row['x'].value() == 1
        assert data.skip(2, pd.BACKWARD) == 2
        row = data.fetchone()
        assert row is not None
        assert row['x'].value() == 0


class DBDataOrdering(_DBTest):

    def setUp(self):
        super_(DBDataOrdering).setUp(self)
        B = pd.DBColumnBinding
        key = B('id', 'xcosi', 'id')
        self.data = pd.DBDataDefault(
            (key, B('popis', 'xcosi', 'popis')),
            key,
            self._dconnection,
            ordering='id')

    def tearDown(self):
        try:
            self.data.sleep()
        except Exception:
            pass
        super_(DBDataOrdering).tearDown(self)

    def test_insert(self):
        row = pd.Row((('popis', sval('bla bla')),))
        d = self.data
        key = (ival(3),)
        self.assertTrue(d.insert(row, after=key)[1], 'Insert failed')
        d.select()
        d.fetchone()
        result = d.fetchone()
        self.assertEqual(result['popis'].value(), 'zvlastni',
                         ('Unexpected value', result['popis'].value()))
        result = d.fetchone()
        self.assertEqual(result['popis'].value(), 'bla bla',
                         ('Unexpected value', result['popis'].value()))
        value = result['id'].value()
        d.close()
        self.assertTrue(value > 3 and value < 6, ('Invalid ordering value', value))
        self.assertTrue(d.insert(row, before=key)[1], 'Insert failed')
        d.select()
        d.fetchone()
        result = d.fetchone()
        self.assertEqual(result['popis'].value(), 'bla bla',
                         ('Unexpected value', result['popis'].value()))
        result = d.fetchone()
        self.assertEqual(result['popis'].value(), 'zvlastni',
                         ('Unexpected value', result['popis'].value()))
        result = d.fetchone()
        self.assertEqual(result['popis'].value(), 'bla bla',
                         ('Unexpected value', result['popis'].value()))
        d.close()


class DBDataAggregated(DBDataDefault):

    def _aggtest(self, test_result, columns=None, condition=None, operation=None, key=None,
                 filter_condition=None, distinct_on=None, group_only=False, sort=()):
        D = pd.DBDataDefault
        B = pd.DBColumnBinding
        denik_spec = (B('cislo', 'denik', 'id'),
                      B('datum', 'denik', 'datum',
                        type_=pd.Date(format=pd.Date.DEFAULT_FORMAT)),
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
        column_groups = column_groups + (('mesic', pd.Float(digits=17, precision=2),
                                          'date_part', sval('month'), 'datum',),)
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
                self.assertIsNotNone(column, ('Aggregation column not found', column_id,))
                self.assertIsInstance(column.type(), pd.Integer)
        try:
            if key is not None:
                row = data.row(key=ival(key), columns=columns)
                for k, v in test_result:
                    self.assertIn(k, row, ('Missing column', k,))
                    self.assertEqual(row[k].value(), v, ('Invalid value', v,))
            elif operation is None:
                count = data.select(columns=columns, condition=condition, sort=sort)
                self.assertEqual(count, len(test_result),
                                 ('Unexpected number of aggregate rows', count))
                for expected_result in test_result:
                    items = data.fetchone().items()
                    items_dict = dict(items)
                    if columns is None:
                        self.assertTrue(len(items) == len(column_groups) + len(operations),
                                        ('Invalid number of columns', items,))
                    else:
                        self.assertEqual(len(items), len(columns),
                                         ('Invalid number of columns', items,))
                    for k, v in expected_result:
                        self.assertEqual(items_dict[k].value(), v,
                                         ('Unexpected result', (k, v, items_dict[k].value(),),))
                self.assertIsNone(data.fetchone(), 'Extra row')
            elif isinstance(operation, tuple):
                value = data.select_aggregate(operation, condition=condition)
                self.assertEqual(value.value(), test_result,
                                 ('Invalid aggregate result', value.value(),))
            else:
                count, row = data.select_and_aggregate(operation, condition=condition,
                                                       columns=columns)
                self.assertEqual(count, test_result[0], ('Invalid aggregate count', count,))
                for k, v in row.items():
                    value = v.value()
                    if value is None:
                        continue
                    test_result = test_result[1:]
                    self.assertTrue(test_result, ('Extra items in aggregate row', k, value,))
                    self.assertEqual(value, test_result[0], ('Invalid aggregate value', k, value,))
                self.assertLessEqual(len(test_result), 1,
                                     ('Missing aggregate row item', test_result,))
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
        condition = pd.EQ('count', ival(1))
        self._aggtest(test_result, condition=condition)

    def test_double_aggregated(self):
        D = pd.DBDataDefault
        self._aggtest(3, operation=(D.AGG_COUNT, 'madatisum',))
        self._aggtest(4, operation=(D.AGG_SUM, 'count',))
        self._aggtest((3, 6000, 7, 4,), operation=D.AGG_SUM,
                      columns=('castka', 'madatisum', 'count',))
        self._aggtest((3, 6000, 7, 4,), operation=D.AGG_SUM)

    def test_row(self):
        self._aggtest((('castka', 2000.0), ('madatisum', 2), ('count', 1),), key=3)

    def test_group_only_row(self):
        self._aggtest(((('mesic', 1.0),),), group_only=True)
        self._aggtest(((('mesic', 1.0),),), group_only=True,
                      sort=(('mesic', pd.ASCENDENT,),))

    def test_aggregated_filter(self):
        D = pd.DBDataDefault
        condition = pd.EQ('cislo', ival(2))
        self._aggtest(((('castka', 1000.0), ('madatisum', 1), ('count', 1),),),
                      filter_condition=condition)
        condition = pd.EQ('cislo', ival(3))
        self._aggtest(1, operation=(D.AGG_COUNT, 'madatisum',), filter_condition=condition)

    def test_distinct(self):
        D = pd.DBDataDefault
        B = pd.DBColumnBinding
        denik_spec = (B('cislo', 'denik', 'id'),
                      B('datum', 'denik', 'datum',
                        type_=pd.Date(format=pd.Date.DEFAULT_FORMAT)),
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

    def test_table_function(self):
        D = pd.DBDataDefault
        B = pd.DBColumnBinding
        func_spec = (B('id', 'tablefunc', 'id', type_=pd.Integer()),
                     B('popis', 'tablefunc', 'popis', type_=pd.String()),
                     )
        data = D(func_spec,
                 func_spec[0],
                 self._dconnection,
                 arguments=(func_spec[0],))
        try:
            count = data.select_aggregate((D.AGG_COUNT, 'id',), arguments=dict(id=ival(2))).value()
            self.assertEqual(count, 3, ('Unexpected number of aggregate rows', count))
        finally:
            data.close()


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
        self.assertTrue(self._ddn_1, 'failure of callback 1')
        self.assertFalse(self._ddn_2, 'failure of callback 2')
        self.assertTrue(self._ddn_3, 'failure of callback 3')

    def test_notification(self):
        d = self.data
        self.assertEqual(d.change_number(), 0)
        d.insert(self.newrow)
        self._ddn_check_result()
        self.assertEqual(d.change_number(), 1)

    def test_side_notification(self):
        d = self.dstat
        cnumber_1 = d.change_number()
        cnumber_2 = self.data.change_number()
        self.assertGreaterEqual(cnumber_1, 0)
        d.insert(pd.Row(
            (('stat', d.columns()[0].type().validate('at')[0]),
             ('nazev', d.columns()[1].type().validate('Austria')[0]))))
        self._ddn_check_result()
        self.assertEqual(d.change_number(), cnumber_1 + 1, (cnumber_1, d.change_number(),))
        self.assertEqual(self.data.change_number(), cnumber_2 + 1,
                         (cnumber_2, self.data.change_number(),))


class DBCounter(_DBBaseTest):

    def setUp(self):
        _DBBaseTest.setUp(self)
        for q in ("create sequence fooseq",):
            try:
                self._sql_command(q)
            except Exception:
                self.tearDown()
                raise
        self._counter = pd.DBCounterDefault('fooseq', self._dconnection)

    def tearDown(self):
        for q in ("drop sequence fooseq",):
            try:
                self._sql_command(q)
            except Exception:
                pass
        _DBBaseTest.tearDown(self)

    def test_next(self):
        self.assertEqual(self._counter.next(), 1)
        self.assertEqual(self._counter.next(), 2)


class DBFunction(_DBBaseTest):

    def setUp(self):
        _DBBaseTest.setUp(self)
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
                      "foo8() returns numeric(3,2) as 'select 3.14'",
                      "foo9() returns float as 'select 3.14::float'",
                      ):
                self._sql_command("create function %s language sql " % q)
        except Exception:
            self.tearDown()
            raise

    def tearDown(self):
        for q in ("foo1(int)",
                  "foo2(text,text)",
                  "foo3()",
                  "foo4()",
                  "foo5(int)",
                  "foo6(int)",
                  "foo7(int, out int, out int)",
                  "foo8()",
                  "foo9()",
                  ):
            try:
                self._sql_command("drop function %s" % q)
            except Exception:
                pass
        for table in ('tab', 'tab1',):
            try:
                self._sql_command("drop table %s" % (table,))
            except Exception:
                pass
        _DBBaseTest.tearDown(self)

    def test_int(self):
        function = pd.DBFunctionDefault('foo1', self._dconnection)
        row = pd.Row((('arg1',
                               pd.Integer().validate('41')[0]),
                              ))
        result = function.call(row)[0][0].value()
        self.assertEqual(result, 42, ('Invalid result', result))

    def test_string(self):
        function = pd.DBFunctionDefault('foo2', self._dconnection)
        row = pd.Row((('arg1', pd.String().validate('foo')[0]),
                              ('arg2', pd.String().validate('bar')[0])))
        result = function.call(row)[0][0].value()
        self.assertEqual(result, 'foobar', ('Invalid result', result))

    def test_numeric(self):
        function = pd.DBFunctionDefault('foo8', self._dconnection)
        row = pd.Row(())
        result = function.call(row)[0][0].value()
        self.assertIsInstance(result, decimal.Decimal)
        function = pd.DBFunctionDefault('foo9', self._dconnection)
        row = pd.Row(())
        result = function.call(row)[0][0].value()
        self.assertIsInstance(result, float)

    def test_empty(self):
        function = pd.DBFunctionDefault('foo3', self._dconnection)
        row = pd.Row(())
        result = function.call(row)[0][0].value()
        self.assertIsNone(result, ('Invalid result', result))

    def test_row_result(self):
        function = pd.DBFunctionDefault('foo4', self._dconnection)
        row = pd.Row(())
        result = function.call(row)
        self.assertEqual(len(result), 1, ('Invalid number of rows', result))
        values = [col.value() for col in result[0]]
        self.assertTrue(values == [10] or values == [20] or values == [30],
                        ('Invalid result', values))

    def test_setof_result(self):
        function = pd.DBFunctionDefault('foo5', self._dconnection)
        row = pd.Row((('arg1', pd.Integer().validate('20')[0]),))
        result = function.call(row)
        self.assertEqual(len(result), 2, ('Invalid number of rows', result))
        value = result[0][0].value()
        self.assertEqual(value, 20, ('Invalid result', value))
        value = result[1][0].value()
        self.assertEqual(value, 30, ('Invalid result', value))

    def test_void(self):
        function = pd.DBFunctionDefault('foo6', self._dconnection)
        row = pd.Row((('arg1', pd.Integer().validate('1000')[0]),))
        function.call(row)
        data = self._sql_command("select count(*) from tab where x = 1000")
        self.assertEqual(data[0][0], 1, ('Invalid data', data))

    def test_complex_result(self):
        t = pd.Integer()
        columns = [pd.ColumnSpec('result1', t),
                   pd.ColumnSpec('result2', t)]
        function = pd.DBFunctionDefault('foo7', self._dconnection, result_columns=columns)
        row = pd.Row((('arg1', pd.Integer().validate('10')[0]),))
        result = [col.value() for col in function.call(row)[0]]
        self.assertEqual(result, [10, 12], ('Invalid result', result))


class DBSearchPath(_DBTest):

    def setUp(self):
        _DBTest.setUp(self)
        for q in ("create schema special",
                  "create table special.cstat(stat char(2) PRIMARY KEY, "
                  "nazev varchar(40) UNIQUE NOT NULL)",
                  "insert into special.cstat values ('sk', 'Slovakia')",):
            self._sql_command(q)

    def tearDown(self):
        try:
            for q in ("drop table special.cstat",
                      "drop schema special",):
                self._sql_command(q)
        except Exception:
            pass
        _DBTest.tearDown(self)

    def _retrieve(self, schemas):
        connection_data = copy.copy(_connection_data)
        connection_data['schemas'] = schemas
        name = 'schemas_' + string.join((schemas or ['default']), '_')
        connection = pd.DBConnection(alternatives={name: connection_data},
                                             **_connection_data)
        B = pd.DBColumnBinding
        key = B('stat', 'cstat', 'stat')
        dstat_spec = pd.DataFactory(
            pd.DBDataDefault,
            (key, (B('nazev', 'cstat', 'nazev'))),
            key)
        dstat = dstat_spec.create(connection_data=connection, connection_name=name)
        return dstat.select_map(lambda row: row[0].value())

    def test_default_path(self):
        def test(schemas):
            keys = self._retrieve(schemas)
            self.assertTrue(len(keys) > 1 and keys[0] != 'sk', ('Invalid result', keys,))
        test(None)
        test([])
        test(['public'])
        test(['public', 'special'])

    def test_special_path(self):
        def test(schemas):
            keys = self._retrieve(schemas)
            self.assertTrue(len(keys) == 1 and keys[0] == 'sk', ('Invalid result', keys,))
        test(['special'])
        test(['special', 'public'])


class DBCrypto(_DBBaseTest):
    # Note: requires pgcrypto: (CREATE EXTENSION pgcrypto)

    def setUp(self):
        _DBBaseTest.setUp(self)
        # TODO: We can't initialize DB objects from gensql as outlined below,
        # because it contains plpythonu functions and thus requires admin
        # priveleges.  We don't want to run tests with DB admin privileges...
        #from pytis.data.gensqlalchemy import gsql_module, capture
        #db_pytis_crypto = capture(gsql_module, 'pytis.dbdefs.db_pytis_crypto')
        #db_pytis_crypto = db_pytis_crypto.replace("INT GENERATED BY DEFAULT AS IDENTITY", "SERIAL")
        #self._sql_command(db_pytis_crypto)
        for q in ("insert into c_pytis_crypto_names (name) values ('test')",
                  "insert into e_pytis_crypto_keys (name, username, key) "
                  "values ('test', current_user, "
                  "pytis_crypto_store_key('somekey', 'somepassword'))",
                  "create table cfoo (id serial, x bytea, y bytea, z bytea)",
        ):
            try:
                self._sql_command(q)
            except Exception:
                self.tearDown()
                raise
        import config
        config.dbconnection.set_crypto_password('somepassword')
        B = pd.DBColumnBinding
        key = B('id', 'cfoo', 'id')
        spec = pd.DataFactory(
            pd.DBDataDefault,
            (key,
             B('x', 'cfoo', 'x', type_=pd.Integer(), crypto_name='test'),
             B('y', 'cfoo', 'y', type_=pd.Float(), crypto_name='test'),
             B('z', 'cfoo', 'z', type_=pd.String(), crypto_name='test'),
             ),
            key)
        self._data = spec.create(connection_data=self._dconnection)
        self._data._pg_flush_connections()

    def tearDown(self):
        for q in ("delete from e_pytis_crypto_keys",
                  "delete from c_pytis_crypto_names",
                  "drop table cfoo",):
            try:
                self._sql_command(q)
            except Exception:
                pass
        _DBBaseTest.tearDown(self)

    def test_basic(self):
        data = self._data

        def check(expected, **kwargs):
            n = data.select(**kwargs)
            try:
                self.assertEqual(n, len(expected), ('Invalid row count', n,))
                for e in expected:
                    row = data.fetchone()
                    if row is None:
                        raise Exception('Missing row')
                    if e is None:
                        continue
                    x, y, z = e
                    self.assertEqual(row['x'].value(), x)
                    self.assertEqual(row['y'].value(), y)
                    self.assertEqual(row['z'].value(), z)
            finally:
                try:
                    data.close()
                except Exception:
                    pass
        data.insert(pd.Row((('x', ival(1),), ('y', fval(-1.10),), ('z', sval('abc'),),)))
        data.insert(pd.Row((('x', ival(2),), ('y', fval(2.22),), ('z', sval('def'),),)))
        data.insert(pd.Row((('x', ival(3),), ('y', fval(-3.33),), ('z', sval('gh'),),)))
        data.insert(pd.Row((('x', ival(4),), ('y', fval(4.44),), ('z', sval('ijkl'),),)))
        data.insert(pd.Row((('x', ival(-5),), ('y', fval(5.50),), ('z', sval('m'),),)))
        data.insert(pd.Row((('x', ival(0),), ('y', fval(0.00),), ('z', sval(''),),)))
        data.update(ival(1), pd.Row((('z', sval('xabc'),),)))
        data.delete(ival(2))
        check(((-5, 5.5, 'm',),
               (0, 0.0, '',),
               (1, -1.1, 'xabc',),
               (3, -3.33, 'gh',),
               (4, 4.44, 'ijkl',),
               ),
              sort=('x',))
        data.delete_many(pd.LE('x', ival(1)))
        check(((3, -3.33, 'gh',),
               (4, 4.44, 'ijkl',),
               ),
              sort=('y',))
        data.delete_many(pd.GT('y', fval(-10.0)))
        check(())


###################
# Complex DB test #
###################


class TutorialTest(_DBBaseTest):

    def setUp(self):
        _DBBaseTest.setUp(self)
        for q in ("CREATE TABLE cis (x varchar(10) PRIMARY KEY, y text)",
                  "CREATE TABLE tab (a int PRIMARY KEY, b varchar(30), "
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
            except Exception:
                self.tearDown()
                raise

    def tearDown(self):
        for t in ('tab', 'cis'):
            try:
                self._sql_command("DROP TABLE %s" % (t,))
            except Exception:
                pass
        _DBBaseTest.tearDown(self)

    def test_it(self):
        # set up
        connection = pd.DBConnection(**_connection_data)

        def get_connection(connection=connection):
            return connection
        C = pd.DBColumnBinding
        D = pd.DBDataDefault
        cis_key = C('id', 'cis', 'x')
        cis_columns = (cis_key,
                       C('popis', 'cis', 'y'))
        cis_data_spec = pd.DataFactory(D, cis_columns, cis_key)
        cis_enumerator = pd.DataEnumerator(cis_data_spec, value_column='popis',
                                                   connection_data=connection)
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
            self.assertEqual(n, 2, ('invalid number of rows', n))
            tab_data.close()
            old_key = tab_data.columns()[0].type().validate('1')[0]
            self.assertTrue(old_key, 'validation not working')
            new_key = tab_data.columns()[0].type().validate('9')[0]
            self.assertTrue(new_key, 'validation not working')
            new_row_data = []
            for c, v in zip(tab_data.columns(),
                            ('9', u'pěkný řádek', 'devet')):
                new_row_data.append((c.id(), c.type().validate(v)[0]))
            # TODO: Momenálně nechodí.  Opravit.
            if False:
                new_row = pd.Row(new_row_data)
                self.assertTrue(tab_data.insert(new_row)[1], 'line not inserted')
                self.assertTrue(tab_data.delete(new_key), 'line not deleted')
                result, success = tab_data.update(old_key, new_row)
                self.assertTrue(result and success, 'line not updated')
                self.assertTrue(tab_data.row(new_key), 'new line not found')
        finally:
            # shut down
            cis_data.sleep()
            tab_data.sleep()


class AccessRightsTest(_DBBaseTest):

    def setUp(self):
        _DBBaseTest.setUp(self)
        for q in ("CREATE SCHEMA pytis",
                  ("CREATE TABLE pytis.access_rights (id serial, object varchar(32), "
                   "column_ varchar(32), group_ name, permission varchar(32))"),
                  ):
            try:
                self._sql_command(q)
            except Exception:
                self.tearDown()
                raise
        P = pd.Permission
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
            self._sql_command(("INSERT INTO pytis.access_rights "
                               "(object, column_, group_, permission) VALUES (%s, %s, %s, %s)") %
                              args)
        connection_data = pd.DBConnection(**_connection_data)
        self._access_rights = pd.DBAccessRights(
            'table1', connection_data=connection_data)

    def tearDown(self):
        for q in ("DROP TABLE pytis.access_rights",
                  "DROP SCHEMA pytis",):
            try:
                self._sql_command(q)
            except Exception:
                pass
        _DBBaseTest.tearDown(self)

    def test_permitted_groups(self):
        P = pd.Permission
        a = self._access_rights
        groups = a.permitted_groups(P.VIEW, 'column1')
        self.assertEqual(groups, ['group1', 'group2'], ('Invalid groups', groups,))
        groups = a.permitted_groups(P.INSERT, 'column2')
        self.assertEqual(groups, [], ('Invalid groups', groups,))
        groups = a.permitted_groups(P.INSERT, 'column2')
        self.assertEqual(groups, [], ('Invalid groups', groups,))
        groups = a.permitted_groups(P.UPDATE, 'column1')
        self.assertEqual(groups, ['group1', 'group3'], ('Invalid groups', groups,))
        groups = a.permitted_groups(P.UPDATE, None)
        self.assertEqual(groups, ['group1', 'group2', 'group3'], ('Invalid groups', groups,))
        groups = a.permitted_groups(P.INSERT, 'column4')
        self.assertEqual(groups, ['group1', 'group2'], ('Invalid groups', groups,))

    def test_permitted(self):
        P = pd.Permission
        a = self._access_rights
        self.assertTrue(a.permitted(P.INSERT, ('group1', 'group3',), column='column1'),
                        'Invalid permission')
        self.assertTrue(not a.permitted(P.INSERT, ('group1', 'group2',), column='column1'),
                        'Invalid permission')
        self.assertTrue(a.permitted(P.UPDATE, ('group3',), column='column5'), 'Invalid permission')
        self.assertTrue(not a.permitted(P.UPDATE, ('group1',), column='column5'),
                        'Invalid permission')
        self.assertTrue(a.permitted(P.UPDATE, ('group3',)), 'Invalid permission')
        self.assertTrue(a.permitted(P.VIEW, ('group3',)), 'Invalid permission')
        self.assertTrue(not a.permitted(P.VIEW, ('group4',)), 'Invalid permission')


class _ThreadTest(): #_DBBaseTest):
    # This is a non-regular test trying to detect bugs resulting from
    # insufficient thread safety

    def setUp(self):
        _DBBaseTest.setUp(self)
        try:
            self._sql_command("create table tab (x int, y int)")
        except Exception:
            self.tearDown()
            raise

    def tearDown(self):
        try:
            self._sql_command("drop table tab")
        except Exception:
            pass
        _DBBaseTest.tearDown(self)

    def test_it(self):
        import thread
        B = pd.DBColumnBinding
        key = B('x', 'tab', 'x')
        d = pd.DataFactory(
            pd.DBDataDefault,
            (key, (B('y', 'tab', 'y'))),
            key)
        c = pd.DBConnection(**_connection_data)
        d1 = d.create(connection_data=c)
        d2 = d.create(connection_data=c)
        t = pd.Integer()
        yvalue = t.validate('1')[0]
        nrepeat = 100
        thr = []
        for i in xrange(10):
            thr.append(False)

        def go1(n, startx, thr=thr):
            for i in xrange(nrepeat):
                key = t.validate('%d' % (i + startx,))[0]
                row = pd.Row([('x', key), ('y', yvalue)])
                d1.insert(row)
                d1.delete(key)
            thr[n] = True

        def go2(n, startx, thr=thr):
            for i in xrange(nrepeat):
                key = t.validate('%d' % (i + startx,))[0]
                row = pd.Row([('x', key), ('y', yvalue)])
                d2.insert(row)
                d2.delete(key)
            thr[n] = True
        for i in xrange(5):
            thread.start_new_thread(go1, (i, i * nrepeat,))
        for i in xrange(5):
            thread.start_new_thread(go2, (i + 5, (i + 5) * nrepeat,))
        end = False
        while not end:
            for i in xrange(10):
                if thr[i] is False:
                    break
            else:
                end = True
            time.sleep(1)


class OperatorTest(_DBBaseTest):

    def setUp(self):
        _DBBaseTest.setUp(self)
        try:
            self._sql_command("create table a (a text, b int)")
            self._sql_command("create type t as (m int, n int)")
            self._sql_command("create or replace function f(x int) returns setof t as $$\n"
                              "begin\n"
                              "return query select unnest(ARRAY[x, x+1, x+2]), "
                              "unnest(ARRAY[(x^2)::int, ((x+1)^2)::int, ((x+2)^2)::int]);\n"
                              "end;\n"
                              "$$ language plpgsql stable")
            self._sql_command("insert into a values ('A', 1)")
            self._sql_command("insert into a values ('B', 2)")
            self._sql_command("insert into a values ('C', 3)")
            self._sql_command("insert into a values ('D', 4)")
        except Exception:
            self.tearDown()
            raise

    def tearDown(self):
        try:
            self._sql_command("drop table a")
            self._sql_command("drop type t cascade")
        except Exception:
            pass
        _DBBaseTest.tearDown(self)

    def test_in(self):
        a = pd.dbtable('a', ('a', 'b'),
                               pd.DBConnection(**_connection_data))
        f = pd.dbtable('f', (('m', pd.Integer()), ('n', pd.Integer())),
                               pd.DBConnection(**_connection_data),
                               arguments=(pd.DBColumnBinding('x', '', 'x',
                                                                     type_=pd.Integer()),))
        for condition, values in (
            (None, ['A', 'B', 'C', 'D']),
            (pd.GT('b', pd.ival(2)), ['C', 'D']),
            (pd.LE('b', pd.ival(2)), ['A', 'B']),
            (pd.IN('b', f, 'n', None,
                           table_arguments={'x': pd.ival(1)}), ['A', 'D']),
            (pd.IN('b', f, 'n', None,
                           table_arguments={'x': pd.ival(2)}), ['D']),
        ):
            result = []
            a.select(condition=condition)
            while 1:
                row = a.fetchone()
                if not row:
                    break
                result.append(row['a'].value())
            a.close()
            self.assertEqual(result, values,
                             '%s: expected %r, got %r' % (condition, values, result))

    def test_equality(self):
        a = pd.EQ('a', sval('a'))
        b = pd.EQ('b', sval('a'))
        c = pd.EQ('a', pd.Value(pd.String(maxlen=5), 'a'))
        d = pd.EQ('d', pd.Value(pd.DateTime(),
                                pd.DateTime.now().value()))
        e = pd.EQ('d', pd.Value(pd.DateTime(), None))
        self.assertNotEqual(a, b)
        self.assertEqual(a, c)
        self.assertNotEqual(b, c)
        self.assertNotEqual(a, d)
        self.assertNotEqual(d, e)
