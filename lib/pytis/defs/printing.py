# -*- coding: utf-8 -*-

# Copyright (C) 2010, 2011, 2013 Brailcom, o.p.s.
#
# COPYRIGHT NOTICE
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
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
from pytis.util import nextval
import pytis.data
import pytis.extensions
from pytis.extensions import Field
import pytis.form
import pytis.presentation

_ = pytis.util.translations('pytis-defs')

class GlobalOutputTemplates(pytis.presentation.Specification):
    public = True
    table = 'ev_pytis_global_output_templates'
    title = _("Předdefinované tiskové šablony")
    fields = (
        Field('id', _("Identifikátor řádku"), editable=pytis.presentation.Editable.NEVER),
        Field('module', _("Formulář"), editable=pytis.presentation.Editable.NEVER),
        Field('specification', _("Název šablony")),
        Field('template', _("Šablona"), text_format=pytis.presentation.TextFormat.LCG,
              width=80, height=20, compact=True),
        Field('rowtemplate', _("Šablona pro jednotlivé řádky"),
              text_format=pytis.presentation.TextFormat.LCG,
              width=80, height=20, compact=True),
        Field('help', _("Nápověda"), virtual=True,
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
              editable=pytis.presentation.Editable.NEVER),
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
        Field('username', _("Uživatel")),
        Field('help', _("Nápověda"), virtual=True,
              computer=pytis.presentation.computer(pytis.output.Formatter.template_help),
              width=80, height=25, compact=True),
        )
    columns = ('module', 'specification', 'username', 'template',)
    layout = pytis.presentation.TabGroup((_("Šablona"), ('module', 'specification', 'template',)),
                                         (_("Řádková šablona"), ('rowtemplate',)),
                                         (_("Hlavičky a patičky"), ('header', 'first_page_header', 'footer',)),
                                         (_("Styl"), ('style',)),
                                         (_("Nápověda"), ('help',)))
    def on_delete_record(self, row):
        if not row['username'].value():
            pytis.form.run_dialog(pytis.form.Warning, _("Můžete mazat pouze své vlastní záznamy."))
            return None
        template = row['specification'].value()
        question = _("Opravdu chcete vymazat tiskovou sestavu %s?") % (template,)
        if not pytis.form.run_dialog(pytis.form.Question, question):
            return None
        return pytis.data.EQ(row.keys()[0], row.key()[0])
    def check(self, row):
        condition = pytis.data.AND(pytis.data.EQ('specification', row['specification']),
                                   pytis.data.EQ('module', row['module']),
                                   pytis.data.NE('id', row['id']))
        if pytis.extensions.dbselect('printing.UserOutputTemplates', condition=condition):
            pytis.form.message(_("Tento název šablony již existuje"))
            return 'specification'

class DirectUserOutputTemplates(UserOutputTemplates):
    def fields(self):
        overridde = (pytis.presentation.Field('module', editable=pytis.presentation.Editable.NEVER),
                     )
        return self._inherited_fields(DirectUserOutputTemplates, override=overridde)
    
    def proc_spec(self):
        def delete_template(module, specification):
            def s(value):
                return pytis.data.Value(pytis.data.String(), value)
            data_spec = self.data_spec()
            view_spec = self.view_spec()
            import config
            data = data_spec.create(dbconnection_spec=config.dbconnection)
            condition = pytis.data.AND(pytis.data.EQ('module', s(module)),
                                       pytis.data.EQ('specification', s(specification)))
            if not data.select(condition):
                message =  _("Tisková sestava neexistuje: ") + specification
                pytis.form.run_dialog(pytis.form.Error, message)
                return
            row = data.fetchone()
            if data.fetchone() is not None:
                message =  _("Tisková sestava se vyskytuje ve více exemplářích: ") + specification
                pytis.form.run_dialog(pytis.form.Error, message)
                return
            record = pytis.presentation.PresentedRow(view_spec.fields(), data, row)
            if pytis.form.delete_record(view_spec, data, None, record):
                # To update the printing button of the current form
                pytis.form.refresh()
        return {'delete_template': delete_template}
