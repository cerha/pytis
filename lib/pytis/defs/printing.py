# -*- coding: utf-8 -*-

# Copyright (C) 2019-2026 Tomáš Cerha <t.cerha@gmail.com>
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
from __future__ import print_function

import pytis.util
import pytis.data
import pytis.extensions
import pytis.output
import pytis.presentation
from pytis.api import app
from pytis.presentation import Field, Editable, procedure
from pytis.util import nextval

_ = pytis.util.translations('pytis-defs')


class GlobalOutputTemplates(pytis.presentation.Specification):
    public = True
    table = 'ev_pytis_global_output_templates'
    title = _("Predefined print templates")
    fields = (
        Field('id', _("Row identifier"), editable=Editable.NEVER),
        Field('module', _("Form"), editable=Editable.NEVER),
        Field('specification', _("Template name")),
        Field('template', _("Template"), text_format=pytis.presentation.TextFormat.LCG,
              width=80, height=20, compact=True),
        Field('rowtemplate', _("Row template"),
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
    title = _("User print templates")
    fields = (
        Field('id', _("Row identifier"), default=nextval('e_pytis_output_templates_id_seq'),
              editable=Editable.NEVER),
        Field('module', _("Form")),
        Field('specification', _("Template name")),
        Field('template', _("Template"), text_format=pytis.presentation.TextFormat.LCG,
              width=80, height=25, compact=True),
        Field('rowtemplate', _("Row template"),
              text_format=pytis.presentation.TextFormat.LCG,
              width=80, height=25, compact=True),
        Field('header', _("Header"),
              text_format=pytis.presentation.TextFormat.LCG,
              width=80, height=10, compact=True),
        Field('first_page_header', _("First-page header (different)"),
              text_format=pytis.presentation.TextFormat.LCG,
              width=80, height=10, compact=True),
        Field('footer', _("Footer"),
              text_format=pytis.presentation.TextFormat.LCG,
              width=80, height=10, compact=True),
        Field('style', _("Style"),
              width=80, height=25, compact=True),
        Field('username', _("User")),
        Field('help', _("Help"), virtual=True, editable=Editable.NEVER,
              computer=pytis.presentation.computer(pytis.output.Formatter.template_help),
              width=80, height=25, compact=True),
    )
    columns = ('module', 'specification', 'username', 'template',)
    layout = pytis.presentation.TabGroup((_("Template"), ('module', 'specification', 'template',)),
                                         (_("Row template"), ('rowtemplate',)),
                                         (_("Headers and footers"),
                                          ('header', 'first_page_header', 'footer',)),
                                         (_("Style"), ('style',)),
                                         (_("Help"), ('help',)))

    def on_delete_record(self, row):
        if not row['username'].value():
            app.warning(_("You can delete only your own records."))
            return None
        template = row['specification'].value()
        if not app.question(_("Do you really want to delete the print report %s?") % (template,)):
            return None
        return pytis.data.EQ(row.keys()[0], row.key()[0])

    def check(self, row):
        condition = pytis.data.AND(pytis.data.EQ('specification', row['specification']),
                                   pytis.data.EQ('module', row['module']),
                                   pytis.data.NE('id', row['id']))
        if pytis.extensions.dbselect('printing.UserOutputTemplates', condition=condition):
            app.echo(_("This template name already exists"))
            return 'specification'


class DirectUserOutputTemplates(UserOutputTemplates):

    def fields(self):
        overridde = (pytis.presentation.Field('module', editable=Editable.NEVER),)
        return self._inherited_fields(DirectUserOutputTemplates, override=overridde)

    @procedure
    def delete_template(self, module, specification):
        def s(value):
            return pytis.data.Value(pytis.data.String(), value)
        data = self.data_spec().create(dbconnection_spec=pytis.config.dbconnection)
        condition = pytis.data.AND(pytis.data.EQ('module', s(module)),
                                   pytis.data.EQ('specification', s(specification)))
        if not data.select(condition):
            app.error(_("Print report does not exist: ") + specification)
            return
        row = data.fetchone()
        if data.fetchone() is not None:
            app.error(_("Print report exists in multiple copies: ") + specification)
            return
        if app.delete_record(self, row):
            # Update the printing button of the current form
            app.refresh()
