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

import pytis.form
import pytis.presentation

from pytis.presentation import Editable
from pytis.extensions import Field, nextval

class GlobalOutputTemplates(pytis.presentation.Specification):
    public = True
    table = 'ev_pytis_global_output_templates'
    title = _("Tiskové šablony")
    fields = (
        Field('id', _("Identifikátor řádku"), editable=pytis.presentation.Editable.NEVER),
        Field('module', _("Formulář"), editable=pytis.presentation.Editable.NEVER),
        Field('specification', _("Název šablony")),
        Field('data', _("Šablona"), width=80, height=20, compact=True),
        )
    columns = ('module', 'specification', 'data',)
    layout = ('module', 'specification', 'data',)

class UserOutputTemplates(pytis.presentation.Specification):
    public = True
    table = 'ev_pytis_user_output_templates'
    title = _("Tiskové šablony")
    fields = (
        Field('id', _("Identifikátor řádku"), default=nextval('e_pytis_output_templates_id_seq'),
              editable=pytis.presentation.Editable.NEVER),
        Field('module', _("Formulář")),
        Field('specification', _("Název šablony")),
        Field('template', _("Šablona"), width=80, height=20, compact=True),
        Field('rowtemplate', _("Šablona pro jednotlivé řádky"), width=80, height=20, compact=True),
        Field('username', _("Uživatel")),
        )
    columns = ('module', 'specification', 'username', 'template',)
    layout = ('module', 'specification', 'template', 'rowtemplate',)
    def on_delete_record(self, row):
        if not row['username'].value():
            pytis.form.run_dialog(pytis.form.Warning, _("Můžete mazat pouze své vlastní záznamy."))
            return None
        if not pytis.form.run_dialog(pytis.form.Question, _("Opravdu chcete záznam zcela vymazat?")):
            return None
        return pytis.data.EQ(row.keys()[0], row.key()[0])
