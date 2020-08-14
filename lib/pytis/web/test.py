#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (C) 2018-2020 Tomáš Cerha <t.cerha@gmail.com>
# Copyright (C) 2001-2016 OUI Technology Ltd.
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

import datetime
import lcg
import pytest
import pytis.data as pd
import pytis.presentation as pp
import pytis.util
import pytis.web as pw
from xml.sax import saxutils


class Timezone(datetime.tzinfo):
    _ZERO_DIFF = datetime.timedelta(0)

    def utcoffset(self, dt):
        return self._ZERO_DIFF

    def tzname(self, dt):
        return "XXX"

    def dst(self, dt):
        return self._ZERO_DIFF


tests = (
    (pp.Field('numeric', type=pd.Integer()),
     (5, '5')),
    (pp.Field('string', type=pd.String()),
     ('x', 'x')),
    (pp.Field('multiline', type=pd.String(), height=4),
     ('xxx\nxxx', 'xxx\nxxx')),
    (pp.Field('date', type=pd.Date()),
     (datetime.date(2016, 8, 30), '30.08.2016')),
    (pp.Field('datetime', type=pd.DateTime()),
     (datetime.datetime(2016, 8, 30, 12, 40, tzinfo=Timezone()), '30.08.2016 12:40:00')),
    (pp.Field('daterange', type=pd.DateRange()),
     (pd.DateRange().adjust_value((datetime.date(1975, 8, 30),
                                   datetime.date(2016, 8, 30))),
      ('30.08.1975', '30.08.2016'))),
    (pp.Field('boolean', type=pd.Boolean()),
     (True, 'T'),
     (False, 'F')),
    (pp.Field('checklist', type=pd.Array(inner_type=pd.Integer()),
              enumerator=pd.FixedEnumerator(range(10)),
              selection_type=pp.SelectionType.CHECKLIST),
     ((pd.ival(1), pd.ival(4)), ('1', '4'))),
    (pp.Field('choice', type=pd.Integer(),
              enumerator=pd.FixedEnumerator(range(10)),
              selection_type=pp.SelectionType.CHOICE),
     (5, '5')),
    (pp.Field('radio', type=pd.Integer(),
              enumerator=pd.FixedEnumerator(range(10)),
              selection_type=pp.SelectionType.RADIO),
     (5, '5')),
)


class Request:
    """Minimal request representation for testing purposes."""

    def __init__(self, params=None):
        self._params = params or {}

    def param(self, name, default=None):
        return self._params.get(name, default)


def export_context(lang):
    exporter = lcg.Html5Exporter(sorted_attributes=True)
    node = lcg.ContentNode('test')
    return exporter.context(node, lang=lang, timezone=Timezone())


@pytest.fixture(params=tests)
def field_params(request):
    fspec, *values = request.param
    columns = [pd.ColumnSpec(fspec.id(), fspec.type())]
    data = pd.Data(columns, columns[0])
    row = pp.PresentedRow((fspec,), data, None, new=True)
    field = pw.Field.create(row, fspec, None, None)
    context = export_context('cs')
    return row, field, context, values


def field_test(f):
    """Decorator running given test func for all fields defined in 'tests' and their test data."""
    def x(field_params):
        row, field, context, values = field_params
        for value, exported in values:
            f(row, field, context, value, exported)
    return x


@field_test
def test_validation(row, field, context, value, exported):
    req = Request(params={field.id: exported})
    assert field.validate(req, context.locale_data()) is None
    assert row[field.id].value() == value


@field_test
def test_hidden(row, field, context, value, exported):
    row[field.id] = pd.Value(field.type, value)
    result = context.localize(field.hidden(context))
    expected = ''.join(
        '<input name="{}" type="hidden" value={}/>'.format(field.id, saxutils.quoteattr(v))
        for v in pytis.util.xtuple(exported)
    )
    assert result == expected
