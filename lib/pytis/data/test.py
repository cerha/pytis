#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (C) 2018-2022 Tomáš Cerha <t.cerha@gmail.com>
# Copyright (C) 2001-2017 OUI Technology Ltd.
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

from __future__ import print_function
from past.builtins import basestring
from builtins import range

import copy
import datetime
import decimal
import io
import sys
import time

import pytest
import unittest

import pytis
from pytis.util import super_, DEBUG, OPERATIONAL, ACTION, EVENT
import pytis.data as pd
from pytis.data import bval, fval, ival, sval


_connection_data = {'database': 'test'}

pytis.config.log_exclude = [DEBUG, OPERATIONAL, ACTION, EVENT]


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

    def test_equality(self):
        t = pd.Type()
        v1 = pd.Value(t, 1)
        v2 = pd.Value(t, 1)
        v3 = pd.Value(t, 2)
        assert v1 == v2
        assert v1 != v3


class _TestType(object):

    _type = pd.Type

    def _validate(self, t, value, check_export=True, **kwargs):
        v, e = t.validate(value, **kwargs)
        assert isinstance(v, pd.Value)
        assert v.type() == t
        assert e is None
        if check_export:
            # Check that validating the exported value produces the same result.
            # Only works in simple cases.  When export needs extra arguments,
            # pass check_export=False and check manually...
            v2, e2 = t.validate(t.export(v.value()), **kwargs)
            assert v2 == v
        return v.value()

    def _check_not_valid(self, t, value, **kwargs):
        v, e = t.validate(value, **kwargs)
        assert v is None
        assert isinstance(e, pd.ValidationError)

    def test_null_validation(self):
        t1 = self._type(not_null=False)
        assert self._validate(t1, '') is None
        t2 = self._type(not_null=True)
        self._check_not_valid(t2, '')

    def test_equality(self):
        assert self._type() == self._type()
        assert self._type(not_null=True) != self._type(not_null=False)

    def test_str(self):
        t = self._type(not_null=True)
        assert t.__class__.__name__ in str(t)
        assert ' not_null=True' in str(t)
        assert str(t) == repr(t)


class TestType(_TestType):

    def test_non_equality(self):
        assert pd.Type() != pd.Integer()
        assert pd.Integer(not_null=True) != pd.Integer()
        assert pd.String(maxlen=2) != pd.String(maxlen=3)

    def test_cloning(self):
        i1 = pd.Integer()
        i2 = pd.Integer(not_null=True)
        i12 = i1.clone(i2)
        assert not i1.not_null()
        assert i12.not_null()
        i21 = i2.clone(i1)
        assert i21.not_null()
        i3 = pd.Integer(not_null=False)
        i23 = i2.clone(i3)
        assert not i23.not_null()
        i31 = i3.clone(i1)
        assert not i31.not_null()
        s1 = pd.String(maxlen=4)
        s2 = pd.RegexString(regex=r'\d-\d+')
        s12 = s1.clone(s2)
        assert isinstance(s12, pd.RegexString)
        assert s12.maxlen() == 4

    def test_str(self):
        class String(pd.String):
            pass
        t1 = pd.String(minlen=3, not_null=True)
        t2 = pd.String(maxlen=3, not_null=False)
        t3 = pd.String(maxlen=3)
        t4 = String(maxlen=3, not_null=False)
        assert str(t1) == '<String minlen=3 not_null=True>'
        assert str(t2) == '<String maxlen=3 not_null=False>'
        assert str(t3) == '<String maxlen=3>'
        assert str(t4) == '<pytis.data.test.String maxlen=3 not_null=False>'
        assert str(t1) == repr(t1)
        assert str(t2) == repr(t2)
        assert str(t3) == repr(t3)
        assert str(t4) == repr(t4)

    def test_constraints(self):
        t = pd.String(minlen=3, constraints=(lambda v: None if v.startswith('x')
                                             else "Must start with x",))
        value, error = t.validate('xaf')
        assert not error
        value, error = t.validate('uaf')
        assert error


class TestInteger(_TestType):
    _type = pd.Integer

    def test_validation(self):
        t = pd.Integer()
        assert self._validate(t, '1') == 1
        assert self._validate(t, '-11111111111111111111') == -11111111111111111111
        assert self._validate(t, '+0') == 0
        self._check_not_valid(t, '1.1')
        self._check_not_valid(t, 'foo')

    def test_limits(self):
        t = pd.Integer(minimum=5, maximum=8)
        assert t.minimum() == 5
        assert t.maximum() == 8
        self._check_not_valid(t, '3')
        assert self._validate(t, '5') == 5
        self._check_not_valid(t, '10')

    def test_equality(self):
        assert pd.Integer() == pd.Integer()


class TestFloat(_TestType):
    _type = pd.Float

    def test_validation(self):
        t = pd.Float()
        assert self._validate(t, '3') == 3.0
        assert self._validate(t, '3.14') == 3.14
        assert self._validate(t, '-3.14') == -3.14
        assert self._validate(t, '0.0') == 0.0
        self._check_not_valid(t, 'foo')

    def test_precision(self):
        t = pd.Float(precision=3)
        assert self._validate(t, '3.14159265') == decimal.Decimal('3.142')

    def test_export(self):
        t = pd.Float(precision=3)
        assert t.export(decimal.Decimal('3.142')) == '3.142'

    def test_rounding(self):
        t = pd.Float()
        F = pd.Float.FLOOR
        C = pd.Float.CEILING
        assert self._validate(t, '3.1415', precision=2) == 3.14
        assert self._validate(t, '3.1415', precision=3) == 3.142
        assert self._validate(t, '2.71', precision=0) == 3
        assert self._validate(t, '3.14159', precision=3, rounding=F) == 3.141
        assert self._validate(t, '3.14159', precision=2, rounding=C) == 3.15
        assert self._validate(t, '3.14', precision=2, rounding=F) == 3.14
        assert self._validate(t, '3.14', precision=2, rounding=C) == 3.14

    def test_value(self):
        f = 3.14
        d = decimal.Decimal('3.14')
        t1 = pd.Float()
        assert isinstance(pd.Value(t1, f).value(), float)
        assert isinstance(pd.Value(t1, d).value(), float)
        t2 = pd.Float(precision=2)
        assert isinstance(pd.Value(t2, f).value(), decimal.Decimal)
        assert isinstance(pd.Value(t2, d).value(), decimal.Decimal)
        t3 = pd.Float(digits=8)
        assert isinstance(pd.Value(t3, f).value(), decimal.Decimal)
        assert isinstance(pd.Value(t3, d).value(), decimal.Decimal)

    def test_fval(self):
        assert isinstance(fval(decimal.Decimal('3.14')).value(), decimal.Decimal)
        assert isinstance(fval(3.14, precision=2).value(), decimal.Decimal)
        assert isinstance(fval(3.14).value(), float)

    def test_equality(self):
        assert pd.Float() == pd.Float()
        assert pd.Float(precision=5) != pd.Float()
        assert pd.Float(precision=3) != pd.Float(precision=2)


class TestString(_TestType):
    _type = pd.String

    def test_validation(self):
        assert self._validate(pd.String(), 'abcdefghi') == 'abcdefghi'
        assert self._validate(pd.String(maxlen=None), 'abcdefghi') == 'abcdefghi'

    def test_limits(self):
        t = pd.String(minlen=3, maxlen=5)
        assert t.minlen() == 3
        assert t.maxlen() == 5
        assert self._validate(t, 'abcde') == 'abcde'
        self._check_not_valid(t, 'ab')
        self._check_not_valid(t, 'abcdef')
        assert self._validate(t, 'abcd') == 'abcd'

    def test_null_validation(self):
        super(TestString, self).test_null_validation()
        assert pd.String().validate('')[0].type().maxlen() is None

    def test_equality(self):
        super(TestString, self).test_equality()
        assert pd.String(maxlen=2) == pd.String(maxlen=2)
        assert pd.String(maxlen=2) != pd.String(maxlen=3)
        assert pd.String(minlen=3) != pd.String(minlen=5)

    def test_non_equality(self):
        assert pd.String(maxlen=2) != pd.String()
        assert pd.String(maxlen=2) != pd.String(maxlen=3)


class TestPassword(_TestType):
    _type = pd.Password

    def test_validation(self):
        t1 = pd.Password(minlen=4)
        assert self._validate(t1, 'abcdef') == 'abcdef'
        assert self._validate(t1, 'abcdef', verify='abcdef') == 'abcdef'
        self._check_not_valid(t1, 'abcdef', verify='')
        self._check_not_valid(t1, 'abcdef', verify='abcef')
        self._check_not_valid(t1, 'abc')
        assert self._validate(t1, '', verify='') is None
        t2 = pd.Password(not_null=True)
        self._check_not_valid(t2, '')
        self._check_not_valid(t2, None)
        self._check_not_valid(t2, '', verify='')
        assert self._validate(t2, 'x') == 'x'
        t3 = pd.Password(minlen=4, not_null=True)
        self._check_not_valid(t3, 'xxx')
        self._check_not_valid(t3, '', verify='')
        assert self._validate(t3, 'abcd', verify='abcd') == 'abcd'

    def test_strength(self):
        t1 = pd.Password(strength=None)
        assert self._validate(t1, 'x') == 'x'
        t2 = pd.Password(strength=True)
        self._check_not_valid(t2, 'x')
        self._check_not_valid(t2, 'abcABC')
        self._check_not_valid(t2, '123456')
        assert self._validate(t2, 'abc123') == 'abc123'
        assert self._validate(t2, 'abc abc') == 'abc abc'

        def strength(password):
            if password and password[0] != 'X':
                return "Not an eXtreme password!"

        t3 = pd.Password(strength=strength)
        self._check_not_valid(t3, 'abc')
        assert self._validate(t3, 'Xabc') == 'Xabc'


class TestColor(_TestType):
    _type = pd.Color

    def test_validation(self):
        t = pd.Color()
        assert self._validate(t, '#0030ab') == '#0030ab'
        self._check_not_valid(t, '0030ab')
        self._check_not_valid(t, '#h030ab')

    def test_non_equality(self):
        assert pd.Color() != pd.String()


class TestBoolean(_TestType):
    _type = pd.Boolean

    def test_validation(self):
        t = pd.Boolean()
        assert self._validate(t, 'T') is True
        assert self._validate(t, 'F') is False
        self._check_not_valid(t, 't')
        self._check_not_valid(t, '0')

    def test_non_equality(self):
        assert pd.Boolean() != pd.String()


class TestUuid(_TestType):
    _type = pd.Uuid

    def test_validation(self):
        import uuid
        t = pd.Uuid()
        s_uuid = '{a0eebc99-9c0b-4ef8-bb6d-6bb9bd380a11}'
        assert self._validate(s_uuid) == uuid.Uuid(s_uuid)

    def test_non_equality(self):
        assert pd.Uuid() != pd.String()



class TestDateTime(_TestType):
    _type = pd.DateTime

    def test_validation(self):
        t = pd.DateTime(format='%Y-%m-%d %H:%M:%S')
        tzinfo = pd.DateTime.UTC_TZINFO
        assert self._validate(t, '2001-02-28 12:14:59', local=False) == \
            datetime.datetime(2001, 2, 28, 12, 14, 59, tzinfo=tzinfo)
        assert self._validate(t, '2999-12-31 0:0:0', local=False) == \
            datetime.datetime(2999, 12, 31, 0, 0, 0, tzinfo=tzinfo)
        assert self._validate(t, '  1999-01-01    23:59:59    ', local=False) == \
            datetime.datetime(1999, 1, 1, 23, 59, 59, tzinfo=tzinfo)
        self._check_not_valid(t, '1999-01-01 23:59', local=False)
        self._check_not_valid(t, '1999-01-01 23:59:00 +0200', local=False)
        self._check_not_valid(t, '99-01-01 0:0:0', local=False)
        self._check_not_valid(t, '2000-13-01 0:0:0', local=False)
        self._check_not_valid(t, '2001-02-29 0:0:0', local=False)
        self._check_not_valid(t, '2001-02-28 24:00:00', local=False)

    def test_export(self):
        t = pd.DateTime(format='%Y-%m-%d %H:%M:%S')
        tzinfo = pd.DateTime.UTC_TZINFO
        dt1 = datetime.datetime(2100, 2, 5, 1, 2, 3, tzinfo=tzinfo)
        assert t.export(dt1, local=False) == '2100-02-05 01:02:03'
        dt2 = datetime.datetime(1841, 7, 2, 1, 2, 3, tzinfo=tzinfo)
        assert t.export(dt2, format='%d.%m.%Y') == '02.07.1841'


class TestISODateTime(_TestType):
    _type = pd.ISODateTime

    def test_validation(self):
        t = pd.ISODateTime()
        tzinfo = pd.DateTime.UTC_TZINFO
        kwargs = dict(local=False, format=pd.ISODateTime.SQL_FORMAT)
        v = self._validate(t, '2012-01-23 11:14:39.23104+01:00', check_export=False, **kwargs)
        assert v == datetime.datetime(2012, 1, 23, 10, 14, 39, 231040, tzinfo=tzinfo)
        assert t.validate(t.export(v, **kwargs), **kwargs)[0].value() == v
        v = self._validate(t, '2012-01-23 11:14:39+01:00', **kwargs)
        assert v == datetime.datetime(2012, 1, 23, 10, 14, 39, 0, tzinfo=tzinfo)
        v = self._validate(t, '2999-12-31 00:00:00.124', check_export=False, **kwargs)
        assert v == datetime.datetime(2999, 12, 31, 0, 0, 0, 124000, tzinfo=tzinfo)
        assert t.validate(t.export(v, **kwargs), **kwargs)[0].value() == v
        v = self._validate(t, '2999-12-31 0:0:0', **kwargs)
        assert v == datetime.datetime(2999, 12, 31, 0, 0, 0, 0, tzinfo=tzinfo)
        self._check_not_valid(t, '2999-12-31 25:0:0', **kwargs)

    def test_export(self):
        t = pd.ISODateTime()
        tzinfo = pd.DateTime.UTC_TZINFO
        dt = datetime.datetime(2012, 1, 23, 10, 14, 39, 23104, tzinfo=tzinfo)
        result = t.export(dt, local=False, format=pd.ISODateTime.SQL_FORMAT)
        assert result == '2012-01-23 10:14:39.023104+00:00'


class TestDate(_TestType):
    _type = pd.Date

    def test_validation(self):
        t = pd.Date(format=pd.Date.DEFAULT_FORMAT)
        assert self._validate(t, '2001-02-28') == datetime.date(2001, 2, 28)
        assert self._validate(t, '2999-12-31') == datetime.date(2999, 12, 31)
        assert self._validate(t, '  1999-01-01    ') == datetime.date(1999, 1, 1)
        assert self._validate(t, '1999-01-01') == datetime.date(1999, 1, 1)
        assert self._validate(t, '1841-07-02') == datetime.date(1841, 7, 2)
        self._check_not_valid(t, '1999-01-01 23:59')
        self._check_not_valid(t, '1999-01-01 23:59:00')
        self._check_not_valid(t, '01-02-29')
        self._check_not_valid(t, '2000-13-01')
        self._check_not_valid(t, '2001-02-29')

    def test_date_and_time(self):
        t = pd.Date(format=pd.Date.DEFAULT_FORMAT)
        date_value = pd.Value(t, datetime.date(2001, 2, 3))
        time_value = pd.Value(pd.Time(utc=True), datetime.time(12, 34, 56))
        value = pd.date_and_time(date_value, time_value)
        assert value == datetime.datetime(2001, 2, 3, 12, 34, 56, tzinfo=pd.DateTime.UTC_TZINFO)
        time_value = pd.Value(pd.Time(utc=False), datetime.time(2, 4, 6))
        value = pd.date_and_time(date_value, time_value)
        assert value == datetime.datetime(2001, 2, 3, 2, 4, 6, tzinfo=pd.DateTime.LOCAL_TZINFO)


class TestTime(_TestType):

    _type = pd.Time

    def test_validation(self):
        t = pd.Time(format='%H:%M:%S')
        tzinfo = pd.DateTime.UTC_TZINFO
        kwargs = {'local': False}
        assert self._validate(t, '12:14:59', **kwargs) == \
            datetime.time(12, 14, 59, tzinfo=tzinfo)
        assert self._validate(t, '0:0:0', **kwargs) == \
            datetime.time(0, 0, 0, tzinfo=tzinfo)
        assert self._validate(t, '    23:59:59    ', **kwargs) == \
            datetime.time(23, 59, 59, tzinfo=tzinfo)
        self._check_not_valid(t, '23:59', **kwargs)
        self._check_not_valid(t, '23:59:00 +0200', **kwargs)
        self._check_not_valid(t, '24:00:00', **kwargs)

    def test_export(self):
        t = pd.Time(format='%H:%M:%S')
        tzinfo = pd.DateTime.UTC_TZINFO
        assert t.export(datetime.time(1, 2, 3, tzinfo=tzinfo), local=False) == '01:02:03'


class TestTimeInterval(_TestType):
    _type = pd.TimeInterval

    def test_validation(self):
        t = pd.TimeInterval()
        assert self._validate(t, '0:15:01') == datetime.timedelta(0, 901)
        assert self._validate(t, '24:00:00') == datetime.timedelta(1, 0)
        assert self._validate(t, '1 day 1:00:00') == datetime.timedelta(1, 3600)
        assert self._validate(t, '1000 days 1:00:00') == datetime.timedelta(1000, 3600)
        self._check_not_valid(t, '0:15')

    def test_validation_with_format(self):
        t = pd.TimeInterval(format='%H:%M')
        assert self._validate(t, '01:02') == datetime.timedelta(0, 3720)

    def test_export(self):
        t = pd.TimeInterval(format='%H:%M')
        value = pd.Value(t, datetime.timedelta(1, 3600))
        assert value.export() == '25:00'
        assert value.export(format='%M:%S') == '00:00'
        assert value.export(format='%H') == '25'
        t2 = pd.TimeInterval()
        value = pd.Value(t2, datetime.timedelta(1, 3600))
        assert value.export() == '25:00:00'
        assert value.export(format='%M:%S') == '00:00'
        assert value.export(format='%H') == '25'

    def test_non_equality(self):
        assert pd.TimeInterval() != pd.TimeInterval(format='%H:%M')


class TestArray(_TestType):
    _type = pd.Array

    def test_validation(self):
        t = pd.Array(inner_type=pd.Integer(not_null=True), maxlen=3)
        assert self._validate(t, ()) == ()
        assert self._validate(t, ('1', '2', '3')) == \
            tuple([pd.Value(t.inner_type(), x) for x in (1, 2, 3)])

    def test_export(self):
        t = pd.Array(inner_type=pd.Integer(not_null=True), maxlen=3)
        assert t.export([pd.ival(x) for x in (1, 2, 3)]) == ('1', '2', '3')

    def test_equality(self):
        t = pd.Array(inner_type=pd.Integer(not_null=True), maxlen=3)
        assert t == pd.Array(inner_type=pd.Integer(not_null=True), maxlen=3)

    def test_non_equality(self):
        t = pd.Array(inner_type=pd.Integer(not_null=True), maxlen=3)
        assert t != pd.Array(inner_type=pd.String())
        assert t != pd.Array(inner_type=pd.Integer(not_null=True))
        assert t != pd.Array(inner_type=pd.Integer(), maxlen=3)

    def test_null_validation(self):
        t1 = pd.Array(inner_type=pd.Integer(not_null=True), maxlen=3, not_null=False)
        v, e = t1.validate('')
        assert e is None
        assert v.type() == t1
        # Well, the status quo is that it returns () in this class, but is this really correct?
        assert v.value() == ()
        # Also with not_null=True it validates '' ok, which seems, well, not ok.
        # t2 = pd.Array(inner_type=pd.Integer(not_null=True), maxlen=3, not_null=True)
        # v, e = t2.validate('')
        # assert v is None
        # assert e is not None

    def test_str(self):
        t = pd.Array(inner_type=pd.Integer(not_null=True), maxlen=3, not_null=False)
        assert str(t) == '<Array inner_type=<Integer not_null=True> maxlen=3 not_null=False>'


class TestBinary(_TestType):
    _type = pd.Binary

    def test_validation(self):
        t = pd.Binary()
        assert self._validate(t, b'') == b''
        assert self._validate(t, b'0123456789') == b'0123456789'
        if sys.version_info[0] == 2:
            assert self._validate(t, buffer(b'abc')) == b'abc'
        else:
            assert self._validate(t, memoryview(b'abc')) == b'abc'
        assert self._validate(t, open(__file__, 'rb')) == open(__file__, 'rb').read()

    def test_maxlen(self):
        t = pd.Binary(maxlen=300)
        self._check_not_valid(t, open(__file__, 'rb'))
        self._check_not_valid(t, 400 * b'x')
        assert self._validate(t, 300 * b'x') == 300 * b'x'

    def test_typeerror(self):
        with pytest.raises(TypeError):
            pd.Value(pd.Binary(), u'abc')

    def test_len(self):
        v = pd.Value(pd.Binary(), b'0123456789')
        assert len(v.value()) == 10

    def test_non_equality(self):
        assert pd.Binary() != pd.Binary(maxlen=3)

    def test_null_validation(self):
        t1 = pd.Binary(not_null=False)
        v, e = t1.validate(b'')
        assert e is None
        assert v.type() == t1
        assert v.value() == b''


class TestJSON(_TestType):
    _type = pd.JSON

    def test_validation(self):
        t = pd.JSON()
        assert self._validate(t, '{"a": "A", "b": "B"}') == {'a': 'A', 'b': 'B'}
        assert self._validate(t, '["foo", 10, 3.14, true, null]') == ['foo', 10, 3.14, True, None]
        self._check_not_valid(t, "{'foo': 'bar'}")
        self._check_not_valid(t, "{foo: 'bar'}")
        self._check_not_valid(t, "{2: 3}")
        self._check_not_valid(t, '"foo"')
        self._check_not_valid(t, 'foo')
        self._check_not_valid(t, 'false')
        self._check_not_valid(t, '1')

    def test_adjust(self):
        t = pd.JSON()
        for v in ({'a': 'A', 'b': 'B'}, ['foo', 10, 3.14, True, None]):
            value = pd.Value(t, v)
            assert value.value() == v
        for v in ("foo", False, 1, {1: 2}, [pd.Integer()]):
            with pytest.raises(TypeError):
                pd.Value(t, v)

    def test_schema(self):
        schema = {
            "$schema": "http://json-schema.org/draft-07/schema#",
            "type": "object",
            "properties": {
                "description": {"type": "string"},
                "active": {"type": "boolean"},
                "age": {"type": "number"},
                "height": {"type": "number"},
            },
            "additionalProperties": False,
        }
        t = pd.JSON(schema=schema)
        self._validate(t, '{"description": "Fluffy jellyfish", "age": 12, "active": true}')
        self._validate(t, '{}')
        self._check_not_valid(t, '{"description": 10}')
        self._check_not_valid(t, '{"age": "12"}')
        self._check_not_valid(t, '{"active": null}')
        self._check_not_valid(t, '{"foo": "bar"}')


class TestImage(_TestType):
    _type = pd.Image
    _icon = (b'\x89PNG\r\n\x1a\n\x00\x00\x00\rIHDR\x00\x00\x00\x0c\x00\x00\x00\x05\x04'
             b'\x03\x00\x00\x00\x83T\x10\x1c\x00\x00\x00\x12PLTE\xef\xef\xef366rttSUU'
             b'\x91\x92\x92\xff\xff\xff\xa6\xcc.k\x00\x00\x00\x01tRNS\x00@\xe6\xd8f'
             b'\x00\x00\x00\x01bKGD\x00\x88\x05\x1dH\x00\x00\x00\tpHYs\x00\x00\x0b'
             b'\x13\x00\x00\x0b\x13\x01\x00\x9a\x9c\x18\x00\x00\x00\x07tIME\x07\xdd'
             b'\x04\x1a\r\x13:\x07k\xbf\x19\x00\x00\x00\x1fIDAT\x08\xd7c`\x10d\x00!'
             b'\x06A\x01F\x10\xc5(((\xc0\x00\xe2\x828@.\x90\x03\x00\x0e:\x00\xbc\xe3'
             b'\xfd\xe4\x96\x00\x00\x00\x00IEND\xaeB`\x82')

    def test_validation(self):
        assert self._validate(pd.Image(), self._icon) == self._icon
        assert self._validate(pd.Image(), io.BytesIO(self._icon)) == self._icon

    def test_minsize(self):
        self._check_not_valid(pd.Image(minsize=(100, 100)), self._icon)

    def test_maxsize(self):
        self._check_not_valid(pd.Image(maxsize=(10, 10)), self._icon)

    def test_image(self):
        value = pd.Value(pd.Image(), self._icon)
        image = value.value().image()
        assert image.size == (12, 5)

    def test_null_validation(self):
        t = pd.Image(not_null=False)
        assert self._validate(t, None) is None


class TestRange(_TestType):

    def test_int_range(self):
        t = pd.IntegerRange()
        t2 = pd.IntegerRange(lower_inc=False, upper_inc=True)
        t3 = pd.IntegerRange(lower_inc=False, upper_inc=False)

        value, err = t.validate(('10', '20'))
        assert err is None
        v = value.value()
        assert isinstance(v, pd.IntegerRange.Range)
        assert v[0] == 10
        assert v[1] == 20
        assert v.lower() == 10
        assert v.upper() == 20
        assert v == t.adjust_value((10, 20))
        value2, err = t2.validate(('9', '19',))
        assert err is None
        assert value2.value() == v
        # Empty value
        value, err = t.validate(('', '',))
        assert err is None
        assert value.value() is None
        # Unbound values
        value, err = t.validate(('1', '',))
        assert err is None
        assert value.value().lower() == 1
        assert value.value().upper() is None
        # Number of values
        with pytest.raises(ValueError):
            t.validate(('2',))
        with pytest.raises(ValueError):
            t.validate(('2', '2', '2'))
        # Other checks
        value, err = t.validate(('2', '2'))
        assert err is None
        value, err = t3.validate(('2', '2'))
        assert err is None
        value, err = t.validate(('2', '1'))
        assert err is not None
        value, err = t.validate(('x', 'y'))
        assert err is not None

        value, err = t.validate('22')
        assert err is None
        pd.Value(t, t.Range(1, 1))
        with pytest.raises(TypeError):
            pd.Value(t, t.Range(1, 0))

    def test_datetime_range(self):
        value, err = pd.DateTimeRange(without_timezone=True).validate(
            ('2014-02-01 00:00:00', '2014-02-01 00:00:02')
        )
        assert err is None
        value, err = pd.DateTimeRange(without_timezone=True).validate(
            ('2015-01-01 12:00:00', '2015-01-01 12:00:00')
        )
        assert err is None
        value, err = pd.DateTimeRange(without_timezone=True).validate(
            ('2015-01-01 12:00:01', '2015-01-01 12:00:00')
        )
        assert err is not None

    def test_export(self):
        t = pd.IntegerRange()
        v = t.adjust_value((1, 8))
        assert t.export(v) == ('1', '8')
        assert t.export(None) == ('', '')

    def test_eq(self):
        assert pd.IntegerRange() != pd.DateTimeRange()
        assert pd.DateTimeRange(without_timezone=True) != pd.DateTimeRange(without_timezone=False)

        r = pd.IntegerRange().adjust_value((1, 5))
        assert r != 1

    def test_base_type(self):
        assert pd.IntegerRange().base_type() == pd.Integer()
        assert pd.DateTimeRange().base_type() == pd.DateTime()

    def test_adjust_value(self):
        t = pd.IntegerRange()
        with pytest.raises(TypeError):
            t.adjust_value(20)
        with pytest.raises(TypeError):
            t.adjust_value((30, 20))
        t2 = pd.IntegerRange(lower_inc=False, upper_inc=True)
        v2 = t2.adjust_value((1, 5))
        assert t.adjust_value(v2) == t2.adjust_value((1, 5))


class DataEnumerator(unittest.TestCase):

    def setUp(self):
        C = pd.ColumnSpec
        S = pd.String()
        B = pd.Boolean()
        data = [pd.Row((('x', sval(x)), ('y', sval(y)), ('z', bval(z))))
                for x, y, z in (('1', 'a', True), ('2', 'b', True), ('3', 'c', False))]
        d = pd.DataFactory(pd.MemData, (C('x', S), C('y', S), C('z', B)), data=data)
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
        v = tuple(self.cb1.enumerator().values())
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
        self.assertEqual(tuple(self._enumerator.values()), self._values)


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

    def test_equality(self):
        x = pd.ColumnSpec('foo', pd.Integer())
        y = pd.ColumnSpec('bar', pd.Integer())
        z = pd.ColumnSpec('foo', pd.String())
        assert self._test_instance == x
        assert self._test_instance != y
        assert self._test_instance != z


class TestRow(object):

    def test_empty(self):
        row = pd.Row()
        assert len(row) == 0
        assert list(row.keys()) == []
        assert list(row.values()) == []

    def test_nonempty(self):
        v1 = ival(1)
        v2 = sval('prvni prvek')
        v3 = ival(2)
        row = pd.Row((('poradi', v1), ('popis', v2)))
        assert len(row) == 2
        assert row[0] == v1 and row[1] == v2
        assert row[-2] == v1 and row[-1] == v2
        assert row['poradi'] == v1 and row['popis'] == v2
        assert list(row.values()) == [v1, v2]
        for key in (-3, 2, '', 'pop', None, self):
            with pytest.raises((IndexError, KeyError)):
                row[key]
        row[0] = row['popis'] = v3
        assert row['poradi'] == row[1] == v3
        row[0:2] = (v2, v1)
        with pytest.raises(IndexError):
            row[0:2] = (v2, v1, v3)
        with pytest.raises(IndexError):
            row[0:2] = (v3,)
        with pytest.raises(TypeError):
            row[0:2] = v1
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

    def test_compare(self):
        r1 = pd.Row((('a', pd.ival(1)), ('b', pd.sval('foo'))))
        r2 = pd.Row((('a', pd.ival(1)), ('b', pd.sval('bar'))))
        r3 = pd.Row((('x', pd.ival(1)),))
        r4 = pd.Row((('a', pd.ival(1)), ('b', pd.Value(pd.String(), 'foo'))))
        r5 = pd.Row((('a', pd.ival(1)), ('b', pd.Value(pd.String(maxlen=4), 'foo'))))
        assert r1 != r2
        assert r2 != r3
        assert r3 != r4
        assert r4 == r1
        assert r5 != r4

    def test_append(self):
        r = pd.Row((('x', ival(1)), ('y', ival(2))))
        r.append('z', ival(3))
        assert r['x'].value() == 1
        assert r['y'].value() == 2
        assert r['z'].value() == 3


class TestData(object):

    @pytest.fixture
    def columns(self):
        return (
            pd.ColumnSpec('foo', pd.Integer()),
            pd.ColumnSpec('bar', pd.String()),
        )

    @pytest.fixture
    def data(self, columns):
        return pd.Data(columns, columns[0])

    def test_it(self, data, columns):
        assert data.columns() == columns
        assert data.find_column('bar') == columns[1]
        assert data.find_column('foobar') is None
        assert data.key() == (columns[0],)
        assert data.row(ival(None)) is None
        assert data.select() == 0
        assert data.fetchone() is None
        assert data.insert(pd.Row()) == (None, False)
        assert data.update(ival(None), pd.Row()) == (None, False)
        assert data.delete(ival(None)) == 0

    def test_row_key(self, data):
        v1 = ival(1)
        v2 = sval('xxx')
        row = pd.Row((('foo', v1), ('bar', v2)))
        assert data.row_key(row) == (v1,)


class MemData(unittest.TestCase):

    def setUp(self):
        columns = (pd.ColumnSpec('a', pd.String()),
                   pd.ColumnSpec('b', pd.String()),
                   pd.ColumnSpec('x', pd.Integer()),
                   pd.ColumnSpec('y', pd.Integer()))
        data = [pd.Row([(c.id(), pd.Value(c.type(), v)) for c, v in zip(columns, values)])
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
        c1 = self._column1 = pd.ColumnSpec('foo', pd.Integer())
        c2 = self._column2 = pd.ColumnSpec('bar', pd.String())
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
        factory = pd.DataFactory(pd.Data, columns, key=(columns[1],))
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

    def test_equality(self):
        assert self._connection == self._connection2
        assert self._connection != self._connection3

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
        b2 = pd.DBColumnBinding('foo', 'tabulka', 'sloupec', related_to=b1)
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
        assert [c.id() for c in d.columns()] == ['foo', 'bar']
        assert len(d.key()) == 1
        assert d.key()[0].id() == 'foo'


class TestFetchBuffer(object):

    @pytest.fixture
    def buf(self):
        def retrieve(position, count):
            start = position + ord('A')
            end = min(ord('Z') + 1, start + count)
            items = [chr(i) for i in range(start, end)]
            print('  +', position, count, items)
            return items
        return pd.FetchBuffer(retrieve, limit=20, initial_fetch_size=10, fetch_size=6)

    def test_init(self, buf):
        assert buf.position() == -1
        assert len(buf) == 0

    def test_rewind(self, buf):
        assert buf.fetch(22) == 'W'
        assert buf.position() == 22
        buf.rewind()
        assert buf.position() == -1
        assert buf.fetch(pd.FORWARD) == 'A'

    def test_fetch_direction(self, buf):
        F, B = pd.FORWARD, pd.BACKWARD
        for d, x in ((F, 'A'), (F, 'B'), (F, 'C'), (F, 'D'), (B, 'C'), (B, 'B')):
            assert buf.fetch(d) == x

    def test_fetch_position(self, buf):
        for position, char in ((0, 'A'), (10, 'K'), (14, 'O'), (25, 'Z'), (26, None), (-1, None)):
            print(position)
            assert buf.fetch(position) == char

    def test_skip(self, buf):
        buf.skip(8, pd.FORWARD)
        assert buf.fetch(pd.FORWARD) == 'I'
        buf.skip(8, pd.BACKWARD)
        assert buf.fetch(pd.FORWARD) == 'B'
        buf.skip(8, pd.FORWARD)
        assert buf.fetch(pd.FORWARD) == 'K'
        buf.skip(1, pd.FORWARD)
        assert buf.fetch(pd.FORWARD) == 'M'
        buf.skip(11, pd.FORWARD)
        assert buf.fetch(pd.FORWARD) == 'Y'
        buf.skip(14, pd.BACKWARD)
        assert buf.fetch(pd.FORWARD) == 'L'
        buf.skip(13, pd.FORWARD)
        assert buf.fetch(pd.FORWARD) == 'Z'

    def test_fetch_from_size(self, buf):
        # Test corner cases of the implementation.
        # First try fetching the last item in the reach of the initial fetch size.
        buf.skip(10, pd.FORWARD)
        assert buf.fetch(pd.FORWARD) == 'K'
        # Now try fetching the most distant item in the reach of a subsequent
        # fetch when adding to the buffer from left (fetching L fills G..P,
        # then fetching A is 6 items to the left from G (and 6 is fetch_size)).
        buf.reset()
        buf.skip(11, pd.FORWARD)
        assert buf.fetch(pd.FORWARD) == 'L'
        buf.skip(12, pd.BACKWARD)
        assert buf.fetch(pd.FORWARD) == 'A'

    def test_edge_fetch(self, buf):
        assert buf.fetch(pd.BACKWARD) is None
        assert buf.position() == -2
        assert buf.fetch(pd.BACKWARD) is None
        assert buf.position() == -3
        assert buf.fetch(pd.FORWARD) is None
        assert buf.position() == -2
        assert buf.fetch(pd.FORWARD) is None
        assert buf.position() == -1
        assert buf.fetch(pd.FORWARD) == 'A'
        buf.skip(24, pd.FORWARD)
        assert buf.fetch(pd.FORWARD) == 'Z'
        assert buf.position() == 25
        assert buf.fetch(pd.FORWARD) is None
        assert buf.position() == 26
        assert buf.fetch(pd.FORWARD) is None
        assert buf.position() == 27
        assert buf.fetch(pd.BACKWARD) is None
        assert buf.position() == 26
        assert buf.fetch(pd.BACKWARD) == 'Z'
        assert buf.position() == 25

    def test_skip_outside(self, buf):
        buf.skip(30, pd.FORWARD)
        assert buf.position() == 29
        assert buf.fetch(pd.FORWARD) is None
        assert buf.position() == 30
        buf.skip(34, pd.BACKWARD)
        assert buf.position() == -4
        assert buf.fetch(pd.FORWARD) is None
        assert buf.position() == -3
        assert buf.fetch(pd.FORWARD) is None

    def test_fetch_size(self, buf):
        assert buf.fetch(pd.FORWARD) == 'A'
        assert len(buf) == 10  # initial_fetch_size
        buf.skip(10, pd.FORWARD)
        assert buf.fetch(pd.FORWARD) == 'L'
        assert len(buf) == 16  # initial_fetch_size + fetch_size
        buf.skip(6, pd.FORWARD)
        assert buf.fetch(pd.FORWARD) == 'S'
        assert len(buf) == 20  # limit


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

    def _sql_commands(self, *commands):
        for command in commands:
            self._sql_command(command)

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
                  "create table viewtest0 (x int, y int) without oids",
                  "create view viewtest4 as select * from viewtest0",
                  "create rule viewtest4_insert as on insert to viewtest4 "
                  "do instead insert into viewtest0 values (new.x)",
                  "create view viewtest7 as select * from viewtest0",
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
            value, error = c.type().validate(v)
            assert error is None, (c.id(), c.type(), v, error)
            row.append((c.id(), value))
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
            print(d, r and r[0])
            result = self.data.fetch(d)
            print(result and result[0].value())
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
        assert error is None
        assert null_data.value() is None
        data = bytes(range(256))
        data1, error = t.validate(data)
        assert error is None
        assert isinstance(data1.value(), pd.Binary.Buffer)
        assert data1.value() == data
        revdata = bytes(reversed(range(256)))
        key = pd.ival(1)
        row1 = pd.Row([('id', key,), ('data', data1,)])
        result, success = self.dbin.insert(row1)
        assert success
        assert result[1].value() == data1.value()
        assert self.dbin.row(key)[1].value() == data1.value()
        data2, error = t.validate(revdata)
        assert error is None
        assert isinstance(data2.value(), pd.Binary.Buffer)
        row2 = pd.Row([('id', key,), ('data', data2,)])
        result, succes = self.dbin.update(key, row2)
        assert success
        assert result[1].value() == data2.value()
        result = self.dbin.row(key)[1].value()
        assert result == data2.value()
        assert self.dbin.delete(key) == 1

    def test_full_text_select(self):
        ts_config = self._sql_command("select get_current_ts_config()")[0][0]
        assert ts_config == 'simple', (
            "Wrong ts_config for full text search tests.\n"
            "Use the following SQL command as a database owner to fix it:\n"
            "ALTER DATABASE ... SET default_text_search_config to 'simple';"
        )

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
            assert result_set == result_ids

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
            assert result_samples == result_set
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
        def irange(x, y):
            t = pd.IntegerRange()
            return pd.Value(t, t.Range(x, y))

        def irange2(x, y):
            t = pd.IntegerRange(lower_inc=False, upper_inc=True)
            return pd.Value(t, t.Range(x, y))

        def drange(x, y):
            return pd.Value(pd.DateTimeRange(without_timezone=True),
                            (datetime.datetime(*x), datetime.datetime(*y),))

        if self._connector.server_version < 90200:
            return
        # Basic tests
        data = self.ranges
        row = data.row(pd.ival(1))
        assert row is not None
        value = row[1].value()
        assert value[0] == 10
        assert value[1] == 20
        value = row[2].value()
        assert value.lower() == 9
        assert value.upper() == 19
        # Insert / update
        v1 = irange(20, 30)
        v2 = irange2(19, 29)
        v3 = drange((2014, 2, 1, 0, 0, 0), (2014, 2, 1, 0, 0, 2))
        assert v1.value() == v2.value()
        data.insert(pd.Row((('x', pd.ival(2)), ('r', v1), ('r2', v2), ('rdt', v3))))
        for column in ('r', 'r2',):
            for value in (v1, v2):
                n = data.select(pd.EQ(column, value))
                try:
                    assert n == 1
                    row = data.fetchone()
                finally:
                    data.close()
        assert row['r'] == v1
        assert row['r2'].value() == v2.value()
        assert row['rdt'] == v3
        data.update(pd.ival(2), pd.Row((('r', irange(40, 50)),
                                        ('r2', irange(40, 50)),
                                        ('rdt', drange((2014, 2, 1, 0, 0, 0),
                                                       (2014, 3, 1, 0, 0, 2))))))
        data.insert(pd.Row((('x', pd.ival(3),),
                            ('r', pd.Value(pd.IntegerRange(), None)),
                            ('r2', pd.Value(pd.IntegerRange(), None)),
                            ('rdt', pd.Value(pd.IntegerRange(), None)))))
        row = data.row(pd.ival(3))
        assert row is not None
        value = row[1].value()
        assert value is None

        # Range operators
        def test_condition(n_rows, condition):
            try:
                assert n_rows == data.select(condition)
            finally:
                data.close()

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
        test_condition(1, pd.RangeOverlap('r', irange(30, None)))
        test_condition(1, pd.RangeOverlap('r', irange(None, 30)))

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
        assert success
        assert result[1].value() == backslash
        assert data.delete(row[0]) == 1

    def test_lock(self):
        us = pd.String().validate('us')[0]
        cz = pd.String().validate('cz')[0]
        t1, t2 = self.dstat, self.dstat1
        transaction_1 = pd.DBTransactionDefault(connection_data=self._dconnection)
        transaction_2 = pd.DBTransactionDefault(connection_data=self._dconnection)
        try:
            assert t1.lock_row(us, transaction_1) is None
            assert isinstance(t2.lock_row(us, transaction_2), basestring)
            assert t2.lock_row(cz, transaction_2) is None
            transaction_2.rollback()
            transaction_2 = pd.DBTransactionDefault(connection_data=self._dconnection)
            assert isinstance(t2.lock_row(us, transaction_2), basestring)
            transaction_1.commit()
            transaction_1 = pd.DBTransactionDefault(connection_data=self._dconnection)
            assert t2.lock_row(us, transaction_2) is None
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
        transaction = pd.DBTransactionDefault(connection_data=self._dconnection)
        transaction_2 = pd.DBTransactionDefault(connection_data=self._dconnection)
        try:
            assert v.lock_row(key, transaction=transaction) is None
            assert isinstance(v.lock_row(key, transaction=transaction_2), basestring)
            assert v.lock_row(key3, transaction=transaction_2) is None
            assert v2.lock_row(key2, transaction=transaction) is None
            assert v2.lock_row(key2, transaction=transaction_2) is None
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


class DBMultiData(object):  # (DBDataDefault): Temporarily disabled
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
            result = dat.fetch(d)
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
        pytis.config.session_variables = {'myvar.test': 'value'}
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
            pd.reload_session_variables(pytis.config.dbconnection)
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
        print()
        d.select()
        n = 0
        for op, count in spec:
            n = n + count
            if count >= 0:
                direction = pd.FORWARD
            else:
                direction = pd.BACKWARD
                count = -count
            print('fetch' if op == 'f' else 'skip', count, direction)
            if op == 'f':
                for i in range(count):
                    d.fetch(direction)
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
        table_size = pytis.config.initial_fetch_size + pytis.config.fetch_size + 10
        self.insert(data, ((i,) for i in range(table_size)))
        return table_size

    def test_skip_fetch(self, data, rows):
        fetch_size, initial_fetch_size = pytis.config.fetch_size, pytis.config.initial_fetch_size
        self._check_skip_fetch(data, (('f', 1),))
        self._check_skip_fetch(data, (('f', 12), ('s', 42)))
        self._check_skip_fetch(data, (('f', 12), ('s', 42), ('f', 10)))
        self._check_skip_fetch(data, (('f', 1), ('s', rows - 1),
                                      ('f', 1), ('s', -2), ('f', -1)))
        self._check_skip_fetch(data, (('f', 12), ('s', initial_fetch_size)))
        self._check_skip_fetch(data, (('f', 12), ('s', initial_fetch_size + 1), ('f', 5)))
        self._check_skip_fetch(data, (('f', 12), ('s', -6), ('f', 2)))
        self._check_skip_fetch(data, (('f', initial_fetch_size + 10), ('s', -16)))
        self._check_skip_fetch(data, (('f', initial_fetch_size + 10), ('s', -16), ('s', 20),
                                      ('f', -10), ('f', 15)))
        self._check_skip_fetch(data, (('s', initial_fetch_size + fetch_size + 3),))
        self._check_skip_fetch(data, (('s', 10 * initial_fetch_size + fetch_size),), noresult=True)

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
        skip = pytis.config.initial_fetch_size
        data.select()
        data.skip(skip)
        for i in range(3):
            assert data.fetchone() is not None
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
                assert column is not None
                assert isinstance(column.type(), pd.Integer)
        try:
            if key is not None:
                row = data.row(key=ival(key), columns=columns)
                for k, v in test_result:
                    assert k in row
                    assert row[k].value() == v
            elif operation is None:
                count = data.select(columns=columns, condition=condition, sort=sort)
                assert count == len(test_result)
                for expected_result in test_result:
                    items = list(data.fetchone().items())
                    items_dict = dict(items)
                    if columns is None:
                        assert len(items) == len(column_groups) + len(operations)
                    else:
                        assert len(items) == len(columns)
                    for k, v in expected_result:
                        assert items_dict[k].value() == v
                assert data.fetchone() is None
            elif isinstance(operation, tuple):
                value = data.select_aggregate(operation, condition=condition)
                assert value.value() == test_result
            else:
                count, row = data.select_and_aggregate(operation, condition=condition,
                                                       columns=columns)
                assert count == test_result[0]
                for k, v in row.items():
                    value = v.value()
                    if value is None:
                        continue
                    test_result = test_result[1:]
                    assert test_result
                    assert value == test_result[0]
                assert len(test_result) <= 1
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
        time.sleep(1)
        assert self._ddn_1
        assert not self._ddn_2
        assert self._ddn_3

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
        d.insert(pd.Row((
            ('stat', d.columns()[0].type().validate('at')[0]),
            ('nazev', d.columns()[1].type().validate('Austria')[0]),
        )))
        self._ddn_check_result()
        assert d.change_number() == cnumber_1 + 1
        assert self.data.change_number() == cnumber_2 + 1


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
            self._sql_commands(
                "create table tab (x int)",
                "create table tab1 (x int)",
                "insert into tab1 values(10)",
                "insert into tab1 values(20)",
                "insert into tab1 values(30)",
            )
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
                      "foo10(bytea) returns int as 'select length($1)'",
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
                  "foo10(bytea)",
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
        row = pd.Row((('arg1', pd.Integer().validate('41')[0]),))
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
        assert isinstance(result, decimal.Decimal)
        function = pd.DBFunctionDefault('foo9', self._dconnection)
        row = pd.Row(())
        result = function.call(row)[0][0].value()
        assert isinstance(result, float)

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

    def test_binary(self):
        function = pd.DBFunctionDefault('foo10', self._dconnection)
        result = function.call(pd.Row((('arg1', pd.Value(pd.Binary(), b'abc')),)))
        assert len(result) == 1
        assert result[0][0].value() == 3
        result = function.call(pd.Row((('arg1', pd.Value(pd.Binary(),
                                                         bytes(bytearray(range(255))))),)))
        assert len(result) == 1
        assert result[0][0].value() == 255

    def test_dbfunction_direct(self):
        pytis.config.dbconnection = self._dconnection
        assert pd.dbfunction('foo1', 5) == 6
        assert pd.dbfunction('foo2', 'abc', 'def') == 'abcdef'
        assert pd.dbfunction('foo10', pd.Value(pd.Binary(), b'abc')) == 3
        assert pd.dbfunction('reverse', 'Hello World') == 'dlroW olleH'

    def test_dbfunction_dbdefs(self):
        # There is also a multirow dbfunction test in Pytis Demo tests
        # (see doc/tutorials/testing.org for more info).
        import pytis.dbdefs.db_pytis_common as common
        import functools
        pytis.config.dbconnection = self._dconnection
        only_digits = functools.partial(pd.dbfunction, common.OnlyDigits)
        assert only_digits('123') is True
        assert only_digits(string='456') is True
        assert only_digits('123abc') is False
        with pytest.raises(TypeError):
            only_digits(value='123')
        with pytest.raises(TypeError):
            only_digits(1)
        with pytest.raises(TypeError):
            only_digits(pd.ival(1))
        with pytest.raises(TypeError):
            only_digits('123', 'abc')


class DBSearchPath(_DBTest):

    def setUp(self):
        _DBTest.setUp(self)
        self._sql_commands(
            "create schema special",
            "create table special.cstat(stat char(2) PRIMARY KEY, "
            "nazev varchar(40) UNIQUE NOT NULL)",
            "insert into special.cstat values ('sk', 'Slovakia')",
        )

    def tearDown(self):
        try:
            self._sql_commands(
                "drop table special.cstat",
                "drop schema special",
            )
        except Exception:
            pass
        _DBTest.tearDown(self)

    def _retrieve(self, schemas):
        connection_data = copy.copy(_connection_data)
        connection_data['schemas'] = schemas
        name = 'schemas_' + '_'.join((schemas or ['default']))
        connection = pd.DBConnection(alternatives={name: connection_data}, **_connection_data)
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
    # Note: requires pgcrypto and db_pytis_crypto:
    # psql test -c 'create extension pgcrypto;'
    # gsql pytis.dbdefs.db_pytis_crypto | psql test

    def setUp(self):
        _DBBaseTest.setUp(self)
        # TODO: We can't initialize DB objects from gensql as outlined below,
        # because it contains plpythonu functions and thus requires admin
        # priveleges.  We don't want to run tests with DB admin privileges...
        # from pytis.data.gensqlalchemy import gsql_module, capture
        # db_pytis_crypto = capture(gsql_module, 'pytis.dbdefs.db_pytis_crypto')
        # edb_pytis_crypto = db_pytis_crypto.replace("INT GENERATED BY DEFAULT AS IDENTITY",
        #                                            "SERIAL")
        # self._sql_command(db_pytis_crypto)
        for q in ("insert into c_pytis_crypto_names (name) values ('test')",
                  "insert into e_pytis_crypto_keys (name, username, key) "
                  "values ('test', current_user, "
                  "pytis_crypto_store_key('somekey', 'somepassword'))",
                  "create table cfoo (id serial, x bytea, y bytea, z bytea)"):
            try:
                self._sql_command(q)
            except Exception:
                self.tearDown()
                raise
        self._dconnection.set_crypto_password('somepassword')
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


class JSONDBTest(_DBBaseTest):

    def setUp(self):
        _DBBaseTest.setUp(self)
        try:
            self._sql_commands(
                "create table json_test (id serial primary key, data jsonb not null)",
                "insert into json_test values (1, '{\"bar\": 8, \"foo\": true}')",
            )
        except Exception:
            self.tearDown()
            raise
        key = pd.DBColumnBinding('id', 'json_test', 'id')
        self._data = pd.DBDataDefault(
            (key, pd.DBColumnBinding('data', 'json_test', 'data', type_=pd.JSONB),), key, self._dconnection
        )

    def tearDown(self):
        try:
            self._sql_command("drop table json_test")
        except Exception:
            pass
        _DBBaseTest.tearDown(self)

    def _get(self, id):
        row = self._data.row(pd.ival(id))
        if row:
            return row['data'].value()
        else:
            return None

    def _insert(self, id, data):
        self._data.insert(pd.Row((
            ('id', pd.ival(id)),
            ('data', pd.Value(pd.JSON(), data)),
        )))

    def test_retrieve(self):
        assert self._get(1) == {'bar': 8, 'foo': True}

    def test_insert(self):
        for id, value in (
                (2, {'foo': False, 'bar': 10}),
                (3, ['foo', 1, 3.14, True, None]),
        ):
            assert self._get(id) is None
            self._insert(id, value)
            assert self._get(id) == value
        self._insert(4, (True, False))
        assert self._get(4) == [True, False]




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

    def _permitted_groups(self, permission, column):
        return sorted(self._access_rights.permitted_groups(permission, column))

    def test_permitted_groups(self):
        P = pd.Permission
        assert self._permitted_groups(P.VIEW, 'column1') == ['group1', 'group2']
        assert self._permitted_groups(P.INSERT, 'column2') == []
        assert self._permitted_groups(P.INSERT, 'column2') == []
        assert self._permitted_groups(P.UPDATE, 'column1') == ['group1', 'group3']
        assert self._permitted_groups(P.UPDATE, None) == ['group1', 'group2', 'group3']
        assert self._permitted_groups(P.INSERT, 'column4') == ['group1', 'group2']

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


class _ThreadTest(object):  # _DBBaseTest):
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
        import _thread
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
        for i in range(10):
            thr.append(False)

        def go1(n, startx, thr=thr):
            for i in range(nrepeat):
                key = t.validate('%d' % (i + startx,))[0]
                row = pd.Row([('x', key), ('y', yvalue)])
                d1.insert(row)
                d1.delete(key)
            thr[n] = True

        def go2(n, startx, thr=thr):
            for i in range(nrepeat):
                key = t.validate('%d' % (i + startx,))[0]
                row = pd.Row([('x', key), ('y', yvalue)])
                d2.insert(row)
                d2.delete(key)
            thr[n] = True
        for i in range(5):
            _thread.start_new_thread(go1, (i, i * nrepeat,))
        for i in range(5):
            _thread.start_new_thread(go2, (i + 5, (i + 5) * nrepeat,))
        end = False
        while not end:
            for i in range(10):
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
        a = pd.dbtable('a', ('a', 'b'), pd.DBConnection(**_connection_data))
        f = pd.dbtable('f', (('m', pd.Integer()), ('n', pd.Integer())),
                       pd.DBConnection(**_connection_data),
                       arguments=(pd.DBColumnBinding('x', '', 'x', type_=pd.Integer()),))
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
        assert a != b
        assert a == c
        assert b != c
        assert a != d
        assert d != e
