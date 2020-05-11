# -*- coding: utf-8 -*-

# Copyright (C) 2018-2019 Tomáš Cerha <t.cerha@gmail.com>
# Copyright (C) 2008-2014 OUI Technology Ltd.
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
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

from __future__ import unicode_literals

from lcg import Document, UFont, UMm
from pytis.output import Center, Group, LongTable, Null, PageNumber, Table, VSpace, \
    f_smaller, \
    PAGE_WIDTH, PAGE_HEIGHT, PAGE_LANDSCAPE_MODE, PAGE_TOP_MARGIN, PAGE_BOTTOM_MARGIN, \
    PAGE_LEFT_MARGIN, PAGE_RIGHT_MARGIN
import pytis.output

TC = Table.Column


class ContinentCountries(pytis.output.PrintSpecification):

    def page_layout(self):
        return {PAGE_WIDTH: UMm(210),
                PAGE_HEIGHT: UMm(297),
                PAGE_LANDSCAPE_MODE: False,
                PAGE_TOP_MARGIN: UMm(10),
                PAGE_BOTTOM_MARGIN: UMm(20),
                PAGE_LEFT_MARGIN: UMm(10),
                PAGE_RIGHT_MARGIN: UMm(10),
                }

    def page_header(self):
        return Null()

    def page_footer(self):
        return PageNumber()

    def body(self):
        spec = self._parameter(pytis.output.P_NAME)
        parts = []
        row = self._parameter((spec, pytis.output.P_ROW))
        id_continent = row['id']
        pcond = pytis.data.EQ('continent', id_continent)

        parts.append(Group(Center(row['name'].export()), boxed=True))

        data_countries_spec = pytis.config.resolver.get('cb.Countries', 'data_spec')
        data_countries = data_countries_spec.create(dbconnection_spec=pytis.config.dbconnection)
        columns = (
            TC('Alpha-2', UMm(10), label_alignment=TC.ALIGN_LEFT, alignment=TC.ALIGN_LEFT),
            TC('Alpha-3', UMm(10), label_alignment=TC.ALIGN_LEFT, alignment=TC.ALIGN_LEFT),
            TC('Numeric', UMm(10), label_alignment=TC.ALIGN_LEFT, alignment=TC.ALIGN_LEFT),
            TC('Continent', UMm(10), label_alignment=TC.ALIGN_LEFT, alignment=TC.ALIGN_LEFT),
            TC('Short name', UMm(60), label_alignment=TC.ALIGN_LEFT, alignment=TC.ALIGN_LEFT),
            TC('Full name', UMm(70), label_alignment=TC.ALIGN_LEFT, alignment=TC.ALIGN_LEFT),
        )

        def data_table(condition):
            def generator_init():
                data_countries.select(condition=condition)

            def generator():
                row = data_countries.fetchone()
                if row is None:
                    return None
                id_ = row['id'].export()
                id3 = row['id3'].export()
                num = row['num'].export()
                continent = row['continent'].export()
                name = row['name'].export()
                fullname = row['fullname'].export()
                return (id_, id3, num, continent, name, fullname)

            table = LongTable(columns, generator,
                              row_generator_init=generator_init,
                              separator_height=1.2, separator_margin=2)
            return table

        table_countries = data_table(pcond)
        parts.append(VSpace(UFont(1)))
        parts.append(f_smaller(table_countries))
        return Document(Group(*parts, vertical=True))
