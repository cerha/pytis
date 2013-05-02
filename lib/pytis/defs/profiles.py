# -*- coding: utf-8 -*-

# Copyright (C) 2009, 2010, 2011, 2013 Brailcom, o.p.s.
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

import pytis.data as pd, pytis.util
from pytis.presentation import Specification, Field, CodebookSpec, Editable, HGroup, Profile, \
    computer

_ = pytis.util.translations('pytis-defs')

class FormProfiles(Specification):
    public = True
    table = 'ev_pytis_form_profiles'
    title = _(u"Profily formulářů")
    fields = (
        Field('id', _(u"Identifikátor"), width=25, editable=Editable.NEVER),
        Field('title', _(u"Název"), width=25, editable=Editable.NEVER),
        Field('username', _(u"Uživatel"), codebook='statistics.FormUserList', value_column='login', editable=Editable.NEVER),
        Field('fullname', _(u"Fullname"), codebook='menu.ApplicationMenuM',
              width=80, column_width=30, editable=Editable.NEVER),
        Field('spec_name', _(u"Název specifikace"),
              width=50, column_width=30, editable=Editable.NEVER),
        Field('form_name', _(u"Třída formuláře"),
              width=50, column_width=30, editable=Editable.NEVER),
        Field('profile_id', _(u"Id profilu"), width=25, editable=Editable.NEVER),
        Field('pickled_filter', editable=Editable.NEVER),
        Field('pickled_params', editable=Editable.NEVER),
        Field('dump', _(u"Obsah"), width=80, height=8, editable=Editable.NEVER),
        Field('errors', _(u"Chyby"), width=80, height=8, editable=Editable.NEVER),
        Field('invalid', _(u"Neplatný"), type=pd.Boolean, virtual=True, width=1,
              computer=computer(lambda r, errors: errors is not None), editable=Editable.NEVER),
        )
    cb = CodebookSpec(display='title')
    columns = ('title', 'profile_id', 'username', 'spec_name', 'form_name', 'invalid')
    layout = HGroup(('title', 'profile_id', 'username'),
                    ('spec_name', 'form_name', 'dump', 'errors'))
    profiles = (Profile('invalid-profiles', _("Neplatné profily"),
                        filter=pd.NE('errors', pd.sval(None))),
                Profile('user-profiles', _("Uživatelské profily"),
                        filter=pd.WM('profile_id', pd.WMValue(pd.String(), '_user_profile_*'))),
                Profile('system-profiles', _("Systémové profily"),
                        filter=pd.NW('profile_id', pd.WMValue(pd.String(), '_user_profile_*'))),
                )

class FormSettings(Specification):
    public = True
    table = 'e_pytis_form_settings'
    title = _(u"Nastavení formulářů")
    fields = (
        Field('id', _(u"Identifikátor"), width=20, editable=Editable.NEVER),
        Field('username', _(u"Uživatel"), codebook='statistics.FormUserList', value_column='login', editable=Editable.NEVER),
        Field('spec_name', _(u"Název specifikace"), width=50, editable=Editable.NEVER),
        Field('form_name', _(u"Typ formuláře"), width=50, editable=Editable.NEVER),
        Field('pickle', editable=Editable.NEVER),
        Field('dump', _(u"Obsah"), width=40, height=5, editable=Editable.NEVER),
        )
    columns = ('username', 'spec_name', 'form_name')
    layout = HGroup(('username', 'spec_name', 'form_name'),
                    ('dump'))
