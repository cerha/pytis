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

import pytis.data as pd
import pytis.presentation as pp
import pytis.util
import pytis.web as pw
import lcg
import datetime
from xml.sax import saxutils


class Request:
    """Minimal request representation for testing purposes."""

    def __init__(self, params=None):
        self._params = params or {}

    def param(self, name, default=None):
        return self._params.get(name, default)


def make_fields(fields):
    columns = [pd.ColumnSpec(f.id(), f.type()) for f in fields]
    data = pd.Data(columns, columns[0])
    row = pp.PresentedRow(fields, data, None, new=True)
    return row, [pw.Field.create(row, f, None, None) for f in fields]


def export_context(lang):
    class Timezone(datetime.tzinfo):
        _ZERO_DIFF = datetime.timedelta(0)

        def utcoffset(self, dt):
            return self._ZERO_DIFF

        def tzname(self, dt):
            return "XXX"

        def dst(self, dt):
            return self._ZERO_DIFF
    exporter = lcg.Html5Exporter(sorted_attributes=True)
    node = lcg.ContentNode('test')
    return exporter.context(node, lang=lang, timezone=Timezone())


fieldspec = (
    pp.Field('numeric', type=pd.Integer()),
    pp.Field('string', type=pd.String()),
    pp.Field('multiline', type=pd.String(), height=4),
    pp.Field('date', type=pd.Date()),
    pp.Field('datetime', type=pd.DateTime()),
    pp.Field('boolean', type=pd.Boolean()),
    pp.Field('checklist', type=pd.Array(inner_type=pd.Integer()),
             enumerator=pd.FixedEnumerator(range(10)),
             selection_type=pp.SelectionType.CHECKLIST),
    pp.Field('choice', type=pd.Integer(),
             enumerator=pd.FixedEnumerator(range(10)),
             selection_type=pp.SelectionType.CHOICE),
    pp.Field('radio', type=pd.Integer(),
             enumerator=pd.FixedEnumerator(range(10)),
             selection_type=pp.SelectionType.RADIO),
)


def test_hidden():
    row, fields = make_fields(fieldspec)
    context = export_context('cs')
    tests = (
        ('numeric', 5, '5'),
        ('string', 'x', 'x'),
        ('multiline', 'xxx\nxxx', 'xxx\nxxx'),
        ('date', datetime.date(2016, 8, 30), '30.08.2016'),
        ('datetime', datetime.datetime(2016, 8, 30, 12, 40, tzinfo=context.timezone()),
         '30.08.2016 12:40:00'),
        ('boolean', True, 'T'),
        ('boolean', False, 'F'),
        ('checklist', (pd.ival(1), pd.ival(4)), ('1', '4')),
        ('choice', 5, '5'),
        ('radio', 5, '5'),
    )
    for fid, value, exported in tests:
        field = pytis.util.find(fid, fields, key=lambda f: f.id)
        row[fid] = pd.Value(row.type(fid), value)
        html = ''.join('<input name="%s" type="hidden" value=%s/>' %
                       (fid, saxutils.quoteattr(v))
                       for v in pytis.util.xtuple(exported))
        assert context.localize(field.hidden(context)) == html

        error = field.validate(Request(params={fid: exported}), context.locale_data())
        assert error is None
        assert row[fid].value() == value
