# -*- coding: iso-8859-2 -*-

# Copyright (C) 2010 Brailcom, o.p.s.
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

import pytis.data
import pytis.form
import pytis.presentation

from pytis.presentation import Editable
from pytis.extensions import Field, nextval

class GlobalOutputTemplates(pytis.presentation.Specification):
    public = True
    table = 'ev_pytis_global_output_templates'
    title = _("Předdefinované tiskové šablony")
    fields = (
        Field('id', _("Identifikátor řádku"), editable=pytis.presentation.Editable.NEVER),
        Field('module', _("Formulář"), editable=pytis.presentation.Editable.NEVER),
        Field('specification', _("Název šablony")),
        Field('template', _("Šablona"), type=pytis.data.StructuredText(),
              width=80, height=20, compact=True),
        Field('rowtemplate', _("Šablona pro jednotlivé řádky"), type=pytis.data.StructuredText(),
              width=80, height=20, compact=True),
        )
    columns = ('module', 'specification', 'template',)
    layout = ('module', 'specification', 'template', 'rowtemplate',)

class UserOutputTemplates(pytis.presentation.Specification):
    public = True
    table = 'ev_pytis_user_output_templates'
    title = _("Uživatelské tiskové šablony")
    fields = (
        Field('id', _("Identifikátor řádku"), default=nextval('e_pytis_output_templates_id_seq'),
              editable=pytis.presentation.Editable.NEVER),
        Field('module', _("Formulář")),
        Field('specification', _("Název šablony")),
        Field('template', _("Šablona"), type=pytis.data.StructuredText(),
              width=80, height=20, compact=True),
        Field('rowtemplate', _("Šablona pro jednotlivé řádky"), type=pytis.data.StructuredText(),
              width=80, height=20, compact=True),
        Field('username', _("Uživatel")),
        )
    columns = ('module', 'specification', 'username', 'template',)
    layout = ('module', 'specification', 'template', 'rowtemplate',)
    def on_delete_record(self, row):
        if not row['username'].value():
            pytis.form.run_dialog(pytis.form.Warning, _("Můžete mazat pouze své vlastní záznamy."))
            return None
        template = row['specification'].value()
        question = _("Opravdu chcete vymazat tiskovou sestavu %s?") % (template,)
        if not pytis.form.run_dialog(pytis.form.Question, question):
            return None
        return pytis.data.EQ(row.keys()[0], row.key()[0])

class DirectUserOutputTemplates(UserOutputTemplates):
    def fields(self):
        fields = pytis.presentation.Fields(UserOutputTemplates.fields)
        overridden = [pytis.presentation.Field(inherit=fields['module'],
                                               editable=pytis.presentation.Editable.NEVER)]
        return fields.fields(override=overridden)
    
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
