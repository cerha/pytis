# -*- coding: utf-8 -*-

# Copyright (C) 2019-2026 Tomáš Cerha <t.cerha@gmail.com>
# Copyright (C) 2010-2014 OUI Technology Ltd.
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

from __future__ import print_function
import pytis.data
import pytis.util
from pytis.presentation import Binding, Field, Specification

_ = pytis.util.translations('pytis-defs')


class FormShortStatistics(Specification):
    public = True
    table = 'ev_pytis_form_short_summary'
    title = _("Overview of used forms")
    fields = (
        Field('form', _("Form name")),
        Field('class', _("Form Class")),
        Field('n_users', _("Number of users")),
        Field('n_open', _("Number of opens")),
        Field('avg_start', _("Average startup time")),
        Field('last_used', _("Last used")),
    )
    bindings = (
        Binding('users', _("Users"), 'statistics.FormUsers',
                condition=(lambda row: pytis.data.AND(
                    pytis.data.EQ('form', row['form']),
                    pytis.data.EQ('class', row['class'])
                ))),
    )


class FormStatistics(Specification):
    public = True
    table = 'ev_pytis_form_summary'
    title = _("Detailed overview of used forms")
    fields = (
        Field('form', _("Form name")),
        Field('class', _("Form Class")),
        Field('info', _("Form parameters")),
        Field('n_users', _("Number of users")),
        Field('n_open', _("Number of opens")),
        Field('avg_start', _("Average startup time")),
        Field('last_used', _("Last used")),
    )
    bindings = (Binding('users', _("Users"), 'statistics.FormUsers',
                        condition=(lambda row: pytis.data.AND(pytis.data.EQ('form', row['form']),
                                                              pytis.data.EQ('class', row['class']),
                                                              pytis.data.EQ('info', row['info'])))),
                )


class FormUsers(Specification):
    public = True
    table = 'ev_pytis_form_users'
    title = _("Form users")
    fields = (
        Field('login', _("Login")),
        Field('form', _("Form name")),
        Field('class', _("Form Class")),
        Field('info', _("Form parameters")),
        Field('n_open', _("Number of opens")),
        Field('last_used', _("Last used")),
    )
    columns = ('login', 'info', 'n_open', 'last_used',)


class FormUserList(Specification):
    public = True
    table = 'ev_pytis_form_user_list'
    title = _("Users")
    fields = (
        Field('login', _("Login")),
    )
    bindings = (Binding('users', _("Forms"), 'statistics.FormUserStatistics',
                        condition=(lambda row: pytis.data.EQ('login', row['login']))),
                Binding('profiles', _("Profiles"), 'profiles.FormProfiles', 'username'),
                )


class FormUserStatistics(Specification):
    public = True
    table = 'ev_pytis_form_users'
    title = _("User forms")
    fields = (
        Field('login', _("Login")),
        Field('form', _("Form name")),
        Field('class', _("Form Class")),
        Field('info', _("Form parameters")),
        Field('n_open', _("Number of opens")),
        Field('last_used', _("Last used")),
    )
    columns = ('form', 'class', 'info', 'n_open', 'last_used',)


class FormUserStatisticsNoinfo(Specification):
    public = True
    table = 'ev_pytis_form_users_noinfo'
    title = _("User forms")
    fields = (
        Field('shortname', _("Action")),
        Field('login', _("Login")),
        Field('form', _("Form name")),
        Field('class', _("Form Class")),
        Field('n_open', _("Number of opens")),
        Field('last_used', _("Last used")),
    )
    columns = ('login', 'class', 'n_open', 'last_used',)
