# -*- coding: utf-8 -*-

# Copyright (C) 2019-2023 Tomáš Cerha <t.cerha@gmail.com>
# Copyright (C) 2010-2015 OUI Technology Ltd.
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

import pytis.util
import pytis.data
import pytis.extensions
import pytis.form
import pytis.presentation
from pytis.api import app
from pytis.presentation import Field, Editable, procedure
from pytis.util import nextval

_ = pytis.util.translations('pytis-defs')


class GlobalOutputTemplates(pytis.presentation.Specification):
    public = True
    table = 'ev_pytis_global_output_templates'
    title = _("Předdefinované tiskové šablony")
    fields = (
        Field('id', _("Identifikátor řádku"), editable=Editable.NEVER),
        Field('module', _("Formulář"), editable=Editable.NEVER),
        Field('specification', _("Název šablony")),
        Field('template', _("Šablona"), text_format=pytis.presentation.TextFormat.LCG,
              width=80, height=20, compact=True),
        Field('rowtemplate', _("Šablona pro jednotlivé řádky"),
              text_format=pytis.presentation.TextFormat.LCG,
              width=80, height=20, compact=True),
        Field('help', _("Help"), virtual=True, editable=Editable.NEVER,
              computer=pytis.presentation.computer(pytis.output.Formatter.template_help),
              width=80, height=20, compact=True),
    )
    columns = ('module', 'specification', 'template',)
    layout = ('module', 'specification', 'template', 'rowtemplate', 'help',)


class UserOutputTemplates(pytis.presentation.Specification):
    public = True
    table = 'ev_pytis_user_output_templates'
    title = _("Uživatelské tiskové šablony")
    fields = (
        Field('id', _("Identifikátor řádku"), default=nextval('e_pytis_output_templates_id_seq'),
              editable=Editable.NEVER),
        Field('module', _("Formulář")),
        Field('specification', _("Název šablony")),
        Field('template', _("Šablona"), text_format=pytis.presentation.TextFormat.LCG,
              width=80, height=25, compact=True),
        Field('rowtemplate', _("Šablona pro jednotlivé řádky"),
              text_format=pytis.presentation.TextFormat.LCG,
              width=80, height=25, compact=True),
        Field('header', _("Hlavička"),
              text_format=pytis.presentation.TextFormat.LCG,
              width=80, height=10, compact=True),
        Field('first_page_header', _("Odlišná hlavička první stránky"),
              text_format=pytis.presentation.TextFormat.LCG,
              width=80, height=10, compact=True),
        Field('footer', _("Patička"),
              text_format=pytis.presentation.TextFormat.LCG,
              width=80, height=10, compact=True),
        Field('style', _("Styl"),
              width=80, height=25, compact=True),
        Field('username', _("User")),
        Field('help', _("Help"), virtual=True, editable=Editable.NEVER,
              computer=pytis.presentation.computer(pytis.output.Formatter.template_help),
              width=80, height=25, compact=True),
    )
    columns = ('module', 'specification', 'username', 'template',)
    layout = pytis.presentation.TabGroup((_("Šablona"), ('module', 'specification', 'template',)),
                                         (_("Řádková šablona"), ('rowtemplate',)),
                                         (_("Hlavičky a patičky"),
                                          ('header', 'first_page_header', 'footer',)),
                                         (_("Styl"), ('style',)),
                                         (_("Help"), ('help',)))

    def on_delete_record(self, row):
        if not row['username'].value():
            app.warning(_("Můžete mazat pouze své vlastní záznamy."))
            return None
        template = row['specification'].value()
        if not app.question(_("Opravdu chcete vymazat tiskovou sestavu %s?") % (template,)):
            return None
        return pytis.data.EQ(row.keys()[0], row.key()[0])

    def check(self, row):
        condition = pytis.data.AND(pytis.data.EQ('specification', row['specification']),
                                   pytis.data.EQ('module', row['module']),
                                   pytis.data.NE('id', row['id']))
        if pytis.extensions.dbselect('printing.UserOutputTemplates', condition=condition):
            app.echo(_("Tento název šablony již existuje"))
            return 'specification'


class DirectUserOutputTemplates(UserOutputTemplates):

    def fields(self):
        overridde = (pytis.presentation.Field('module', editable=Editable.NEVER),
                     )
        return self._inherited_fields(DirectUserOutputTemplates, override=overridde)

    @procedure
    def delete_template(self, module, specification):
        def s(value):
            return pytis.data.Value(pytis.data.String(), value)
        data_spec = self.data_spec()
        view_spec = self.view_spec()
        data = data_spec.create(dbconnection_spec=pytis.config.dbconnection)
        condition = pytis.data.AND(pytis.data.EQ('module', s(module)),
                                   pytis.data.EQ('specification', s(specification)))
        if not data.select(condition):
            app.error(_("Tisková sestava neexistuje: ") + specification)
            return
        row = data.fetchone()
        if data.fetchone() is not None:
            app.error(_("Tisková sestava se vyskytuje ve více exemplářích: ") + specification)
            return
        record = pytis.presentation.PresentedRow(view_spec.fields(), data, row)
        if pytis.form.delete_record(view_spec, data, None, record):
            # To update the printing button of the current form
            pytis.form.refresh()
