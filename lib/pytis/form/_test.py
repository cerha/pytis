#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (C) 2018, 2019 Tomáš Cerha <t.cerha@gmail.com>
# Copyright (C) 2001-2005 Brailcom, o.p.s.
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

import pytest
import pytis.form._grid as grid
import pytis.presentation as pp
import pytis.data as pd
import pytis.data._test as dtest


class TestDataTable(dtest.DBTest):

    @pytest.fixture
    def table(self, connector):
        try:
            self.sql('create table grid_test'
                     '(id int, name text, price decimal(10, 2), flag bool)')
            yield 'grid_test'
        finally:
            try:
                self.sql('drop table grid_test')
            except Exception:
                pass

    @pytest.fixture
    def spec(self):
        class Specification(pp.Specification):
            table = 'grid_test'
            fields = (
                pp.Field('id', type=pd.Integer()),
                pp.Field('name', type=pd.String()),
                pp.Field('price', type=pd.Float(precision=2)),
                pp.Field('flag', type=pd.Boolean()),
            )
        return Specification()

    @pytest.fixture
    def data(self, table, spec, dbconnection):
        return spec.data_spec().create(connection_data=dbconnection)

    def grid_table(self, spec, data, row_style=None, field_style={}, sorting=None):
        fields = [f.clone(pp.Field(f.id(), style=field_style.get(f.id()))) for f in spec.fields]
        row = pp.PresentedRow(fields, data, None)
        count = data.select(sort=sorting or ())  # , async_count=True)
        return grid.DataTable(data, row, row.fields(), count, row_style=row_style, sorting=sorting)

    def test_cell_value(self, spec, data):
        self.insert(data, (
            (1, 'Apple', 12.3, False),
            (2, 'Banana', 23.4, True),
            (3, 'Strawberry', 2.34, False),
            (4, 'Orange', None, True),
        ))
        t = self.grid_table(spec, data)
        try:
            assert t.cell_value(0, 0) == '1'
            assert t.cell_value(1, 2) == '23.40'
            assert t.cell_value(3, 2) == ''
            assert t.cell_value(3, 3) == 'T'
            assert t.cell_value(4, 2) is None
            assert t.cell_value(3, 4) is None
        finally:
            data.close()

    def test_cell_style(self, spec, data):
        self.insert(data, (
            (1, 'Apple', 12.3, False),
            (2, 'Banana', 23.4, True),
            (3, 'Strawberry', 2.34, True),
            (4, 'Orange', None, True),
        ))
        t = self.grid_table(
            spec, data,
            row_style=lambda r: pp.Style(background='#ffa') if r['id'].value() % 2 else None,
            field_style={'price': lambda r: pp.Style(bold=True) if r['flag'].value() else None},
        )
        try:
            assert t.cell_style(4, 1) is None
            assert t.cell_style(3, 4) is None
            assert t.cell_style(0, 2) == pp.Style(background='#ffa')
            assert t.cell_style(1, 3) == pp.Style()
            assert t.cell_style(1, 2) == pp.Style(bold=True)
            assert t.cell_style(2, 2) == pp.Style(bold=True, background='#ffa')
        finally:
            data.close()

    def test_big_table_performance(self, spec, data):
        # This test can be used to examine row buffer filling strategy
        # and database performance on bigger jumps.  It is practical to
        # add something like:
        #
        # print '  ', query.format()[:140]
        #
        # into PostgreSQLConnector._pg_query() in postgresql.py and
        # run pytest with -s to see how the buffer is filled in response
        # to row requests.
        def rows(count):
            i = 0
            while i < count:
                i += 1
                yield (i, 'Row number %d' % i, 1.86 * i, i % 3 == 0)
        self.insert(data, rows(9500))

        t = self.grid_table(spec, data, sorting=(('id', pd.ASCENDENT),))
        try:
            for n in (30, 29, 28, 27, 26, 9440, 9439, 9438, 9437, 9436,
                      50, 199, 200, 299, 220, 160, 120, 80, 40, 0,
                      4444, 0, 2453, 3890, 9499, 99, 140, 98, 30):
                print n
                assert t.cell_value(n, 0) == str(n + 1)
        finally:
            data.close()
