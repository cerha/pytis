# -*- coding: utf-8 -*-

# Copyright (C) 2009, 2010, 2011 Brailcom, o.p.s.
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

import pytis.data as pd
from pytis.presentation import Specification, Field, CodebookSpec, Editable, Profile, computer

class FormActionLog(Specification):
    public = False
    table = 'e_pytis_action_log'
    title = _(u"Log uživatelských akcí")
    fields = (
        Field('id', editable=Editable.NEVER),
        Field('timestamp', _(u"Čas"), width=25, editable=Editable.NEVER),
        Field('username', _(u"Uživatel"), codebook='statistics.FormUserList',
              value_column='login', editable=Editable.NEVER),
        Field('spec_name', _(u"Název specifikace"),
              width=50, column_width=30, editable=Editable.NEVER),
        Field('form_name', _(u"Třída formuláře"),
              width=50, column_width=30, editable=Editable.NEVER),
        Field('action', _(u"Akce"), width=25, editable=Editable.NEVER),
        Field('info', _("Informace"), editable=Editable.NEVER, height=10, width=70),
        )
    sorting = (('timestamp', pd.ASCENDENT),)
    columns = ('timestamp', 'username', 'spec_name', 'form_name', 'action')
    layout = ('timestamp', 'username', 'spec_name', 'form_name', 'action', 'info')

class FormActionLogView(Specification):
    public = True

