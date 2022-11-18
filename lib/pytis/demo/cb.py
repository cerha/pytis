# -*- coding: utf-8 -*-

# Copyright (C) 2018-2022 Tomáš Cerha <t.cerha@gmail.com>
# Copyright (C) 2007-2018 OUI Technology Ltd.
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
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

from __future__ import unicode_literals

import lcg
import pytis.data as pd
import pytis.output
import pytis.form

from pytis.api import app
from pytis.presentation import (
    Binding, computer, CbComputer, CodebookSpec, Field, Editable,
    PostProcess, PrintAction, Specification, TextFilter,
)
from pytis.util import translations

import pytis.dbdefs.demo as dbdefs

_ = translations('pytis-demo')


class Continents(Specification):
    """Codebook of continents and their codes.

    Codebook is a regular view.  Any view can be used as a codebook.  It is,
    however, possible to define several properties of a view, which influence
    its use in the codebook context.  They are all covered by the
    'CodebookSpec' instance passed as the 'cb' argument of the specification.
    See the 'CodebookSpec' documentation for more information about the
    available options.

    Using a view as a codebook for a particular field is than done by passing
    its name as the 'codebook' argument in the field specification.  See the
    specification of 'Countries' below for an example.

    """
    public = True
    title = _("Continents")
    table = dbdefs.Continents
    fields = (
        Field('id', width=3, column_width=6, fixed=True,
              filter=TextFilter.ALPHANUMERIC, post_process=PostProcess.UPPER),
        Field('name', width=40),
        Field('smallest', codebook='cb.Countries'),
    )
    cb = CodebookSpec(display='name')
    bindings = (
        Binding('all-countries', _("All Countries"), 'cb.Countries', 'continent',
                search=lambda r: r['id'].value() == 'EU' and pd.sval('CZ') or None),
        Binding('islands', _("Islands"), 'cb.Countries', 'continent',
                condition=lambda r: pd.WM('name', pd.WMValue(pd.String(), '*island*'))),
        Binding('disabled-countries', _("Disabled Countries"),
                'cb.DisabledCountries', 'continent'),
        Binding('random-numbers', _("Random numbers"), 'misc.RandomNumbers',
                arguments=lambda r: {}),
    )
    prints = (
        (_("Default"), 'output/None'),  # This spec doen't exist, so defaults are used.
        (_("Countries of the current continent"), 'cb.PrintContinentCountries'),
        PrintAction('p_countries_continents',
                    _("Countries of the current continent as specification"),
                    'cb.PrintContinentCountries'),
        PrintAction('p_overflow',
                    _("PDF LayoutError"),
                    'cb.PrintOverflow'),
        (_("Template based"), 'output/Continents'),
        (_("Database template"), 'ContinentsDB'),
        (_("Iterated table"), 'IteratedContinents'),
    )


class Countries(Specification):
    description = "Codebook of country codes and names."
    help = """

    This view is a codebook and it also uses another codebook -- the field
    'continent' refers to the codebook of continents.

    Link: [form:cb.Continents?select_row=EU]
    Link: [call:pytis.demo.cb.pokus?message=Hello&x=5&y=3]

    """
    public = True
    title = _("Countries")
    table = dbdefs.Countries
    _continents = 'cb.Continents'  # To be overriden in www/demo.py

    def _customize_fields(self, fields):
        fields.modify_many(('id', 'id3', 'num', 'continent',), column_width=6)
        fields.modify_many(('id', 'id3', 'num',), fixed=True)
        fields.modify_many(('id', 'id3',), filter=TextFilter.ALPHANUMERIC,
                           post_process=PostProcess.UPPER)
        fields.append(Field('cname', _("Continent name"), editable=Editable.NEVER,
                            computer=CbComputer('continent', 'name'),
                            virtual=True, type=pd.String()))
        fields.set_property('width', id=2, num=3, continent=2, name=30,
                            fullname=40, cname=20)
    columns = ('id', 'id3', 'continent', 'name')
    cb = CodebookSpec(display='name', columns=('id', 'id3', 'name'),
                      sorting=(('id', pd.ASCENDENT),))
    sorting = (('id', pd.ASCENDENT),)

    def bindings(self):
        return (
            Binding('continent-details', _("Continent details"), self._continents, 'continent',
                    single=True,
                    descr=_("Shows the details of the currently selected main table record.")),
            Binding('other-countries', _("Other countries of the same continent"), 'cb.Countries',
                    condition=lambda r: pd.EQ('continent', r['continent'],),
                    prefill=lambda r: {'name': r['name'].value()},
                    descr=_("Displays a table of all countries from the same continent as the "
                            "currently selected main form country.")),
            Binding('webform', _("Wikipedia"), uri=self._continent_uri,
                    descr=_("Shows Wikipedia information about the currently selected country.")),
        )

    def _continent_uri(self, record):
        """This method demonstrates usage of web form bindings.

        It returns an URI displayed in the side form browser window for the
        currently selected main form record.  In this case the result is a
        wikipedia article about given country.  The article name may be set
        explicitly through the mapping or it is guessed from the country name.

        """
        mapping = {'BV': 'Bouvet_Island',
                   'CC': 'Coco_Islands',
                   'FK': 'Falkland_Islands',
                   }
        country_id = record['id'].value()
        try:
            article_name = mapping[country_id]
        except KeyError:
            article_name = record['name'].value().replace(' ', '_')
        return 'https://en.wikipedia.org/wiki/' + article_name


@pytis.form.help_proc
def pokus(message, x=None, y=None):
    app.message("%s\nx=%d, y=%d" % (message, x, y))


class DisabledCountries(Countries):
    """Just for testing the behavior of bindings with insufficient acceess rights."""
    public = True
    access_rights = pd.AccessRights((None, (['xxx'], pd.Permission.VIEW)),)


class Insurance(Specification):
    """Using function table as a codebook."""
    public = True
    title = _("Function Table")
    table = dbdefs.Insurance
    columns = ('description', 'value', 'fee',)
    sorting = (('id', pd.ASCENDENT),)

    def _customize_fields(self, fields):
        fields.modify_many(('value', 'fee',), fixed=True)
        fields.modify('fee', editable=computer(lambda row, value: value is not None),
                      runtime_arguments=computer(lambda row, value: dict(value=pd.ival(value))),
                      codebook='cb.InsuranceFee')


class InsuranceFee(Specification):
    """Function table codebook."""
    public = True
    title = _("Fees")
    table = dbdefs.InsuranceFees
    cb = CodebookSpec(display='risk')

    def _customize_fields(self, fields):
        fields.modify_except([], fixed=True)


class PrintContinentCountries(pytis.output.PrintSpecification):

    def init(self):
        if not app.question(_("Really print?")):
            return None
        result = pytis.form.run_form(pytis.form.InputForm, title=_("Document title"), fields=(
            Field('title', _("Title"), not_null=True),
        ))
        if result:
            self._add_parameter('title', result['title'].value())
            data_countries_spec = pytis.config.resolver.get('cb.Countries', 'data_spec')
            self._data = data_countries_spec.create(dbconnection_spec=pytis.config.dbconnection)
            return True

    def cleanup(self):
        self._data.close()
        app.message(_("Printed!"))

    def page_layout(self):
        return {pytis.output.PAGE_WIDTH: lcg.UMm(210),
                pytis.output.PAGE_HEIGHT: lcg.UMm(297),
                pytis.output.PAGE_LANDSCAPE_MODE: False,
                pytis.output.PAGE_TOP_MARGIN: lcg.UMm(10),
                pytis.output.PAGE_BOTTOM_MARGIN: lcg.UMm(20),
                pytis.output.PAGE_LEFT_MARGIN: lcg.UMm(10),
                pytis.output.PAGE_RIGHT_MARGIN: lcg.UMm(10),
                }

    def page_header(self):
        return pytis.output.Null()

    def page_footer(self):
        return pytis.output.PageNumber()

    def body(self):
        spec = self._parameter(pytis.output.P_NAME)
        parts = []
        row = self._parameter((spec, pytis.output.P_ROW,))
        id_continent = row['id']
        pcond = pd.EQ('continent', id_continent)

        parts.append(pytis.output.Center(self._parameter('title')))

        columns = (
            pytis.output.Table.Column(
                'Alpha-2', lcg.UMm(10), label_alignment=pytis.output.Table.Column.ALIGN_LEFT,
                alignment=pytis.output.Table.Column.ALIGN_LEFT,
            ),
            pytis.output.Table.Column(
                'Alpha-3', lcg.UMm(10), label_alignment=pytis.output.Table.Column.ALIGN_LEFT,
                alignment=pytis.output.Table.Column.ALIGN_LEFT,
            ),
            pytis.output.Table.Column(
                'Numeric', lcg.UMm(10), label_alignment=pytis.output.Table.Column.ALIGN_LEFT,
                alignment=pytis.output.Table.Column.ALIGN_LEFT,
            ),
            pytis.output.Table.Column(
                'Continent', lcg.UMm(10), label_alignment=pytis.output.Table.Column.ALIGN_LEFT,
                alignment=pytis.output.Table.Column.ALIGN_LEFT,
            ),
            pytis.output.Table.Column(
                'Short name', lcg.UMm(60), label_alignment=pytis.output.Table.Column.ALIGN_LEFT,
                alignment=pytis.output.Table.Column.ALIGN_LEFT,
            ),
            pytis.output.Table.Column(
                'Full name', lcg.UMm(70), label_alignment=pytis.output.Table.Column.ALIGN_LEFT,
                alignment=pytis.output.Table.Column.ALIGN_LEFT,
            ),
        )

        def data_table(condition):
            def generator_init():
                self._data.select(condition=condition)

            def generator():
                row = self._data.fetchone()
                if row is None:
                    return None
                id_ = row['id'].export()
                id3 = row['id3'].export()
                num = row['num'].export()
                continent = row['continent'].export()
                name = row['name'].export()
                fullname = row['fullname'].export()
                return (id_, id3, num, continent, name, fullname)

            table = pytis.output.LongTable(columns, generator,
                                           row_generator_init=generator_init,
                                           separator_height=1.2, separator_margin=2)
            return table

        table_countries = data_table(pcond)
        parts.append(pytis.output.VSpace(lcg.UFont(1)))
        parts.append(pytis.output.f_smaller(table_countries))
        return pytis.output.Document(pytis.output.Group(*parts, vertical=True))


class PrintOverflow(pytis.output.PrintSpecification):

    def cleanup(self):
        app.message(_("Printed!"))

    def body(self):
        content_1 = lcg.coerce('xxx')
        content_2 = lcg.VSpace(lcg.UMm(500))
        return pytis.output.Document(lcg.Container((content_1, content_2, content_1,)))
