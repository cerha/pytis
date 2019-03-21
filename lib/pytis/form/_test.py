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

import unittest
import pytis.form._grid as grid
import pytis.presentation as pp
import pytis.data as pd
import pytis.data._test as dtest


class DataTable(dtest._DBBaseTest):

    def setUp(self):
        self._sql_command('create table grid_test'
                          '(id int, name text, price decimal(10, 2), flag bool)')

    def tearDown(self):
        self._sql_command('drop table grid_test')

    def _table(self, rows, row_style=None, name_style=None, price_style=None):
        class Specification(pp.Specification):
            table = 'grid_test'
            fields = (
                pp.Field('id', type=pd.Integer()),
                pp.Field('name', type=pd.String(), style=name_style),
                pp.Field('price', type=pd.Float(precision=2), style=price_style),
                pp.Field('flag', type=pd.Boolean()),
            )
        data = Specification().data_spec().create(connection_data=self._dconnection)
        row = pp.PresentedRow(Specification.fields, data, None)

        for r in rows:
            data.insert(pd.Row([(f.id(), pd.Value(f.type(), v)) for v, f in zip(r, row.fields())]))

        count = data.select()
        return grid.DataTable(data, row, row.fields(), count, row_style=row_style), data

    def test_cell_value(self):
        t, data = self._table((
            (1, 'Apple', 12.3, False),
            (2, 'Banana', 23.4, True),
            (3, 'Strawberry', 2.34, False),
            (4, 'Orange', None, True),
        ))
        try:
            self.assertEqual(t.cell_value(0, 0), '1')
            self.assertEqual(t.cell_value(1, 2), '23.40')
            self.assertEqual(t.cell_value(3, 2), '')
            self.assertEqual(t.cell_value(3, 3), 'T')
            self.assertEqual(t.cell_value(4, 2), None)
            self.assertEqual(t.cell_value(3, 4), None)
        finally:
            data.close()

    def test_cell_style(self):
        t, data = self._table(
            rows=(
                (1, 'Apple', 12.3, False),
                (2, 'Banana', 23.4, True),
                (3, 'Strawberry', 2.34, True),
                (4, 'Orange', None, True),
            ),
            row_style=lambda r: pp.Style(background='#ffa') if r['id'].value() % 2 else None,
            price_style=lambda r: pp.Style(bold=True) if r['flag'].value() else None,
        )
        try:
            self.assertEqual(t.cell_style(4, 1), None)
            self.assertEqual(t.cell_style(3, 4), None)
            self.assertEqual(t.cell_style(0, 2), pp.Style(background='#ffa'))
            self.assertEqual(t.cell_style(1, 3), pp.Style())
            self.assertEqual(t.cell_style(1, 2), pp.Style(bold=True))
            self.assertEqual(t.cell_style(2, 2), pp.Style(bold=True, background='#ffa'))
        finally:
            data.close()


if __name__ == '__main__':
    unittest.main()
