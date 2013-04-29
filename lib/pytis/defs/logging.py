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

from pytis.util import translate as _

import datetime

import config
import pytis.data as pd
from pytis.presentation import Specification, Field, CodebookSpec, Editable, \
    Profile, QueryFields, computer, HGroup, VGroup
from pytis.form import BrowseForm, run_form

class FormActionLog(Specification):
    # This specification is used for insertion of log record by pytis
    # internally, so it is not public.  The derived specification
    # FormActionLogView is used for viewing the logs through admin forms.
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


class FormActionLogView(FormActionLog):
    # This specification is used for viewing the logs through admin forms.
    public = True
    access_rights = pd.AccessRights((None, (['dmp_view'], pd.Permission.VIEW,)))


class ChangesLog(Specification):
    # This specification is used for viewing the logs from log_trigger on db table.
    public = True

    title = _("Přehled změn")
    table = 'f_view_log'
    columns = layout = ('timestamp', 'username', 'operation', 'schemaname',
                        'tablename', 'key_column', 'key_value', 'detail')
    sorting = (('id', pd.DESCENDANT),)

    def _df_search_path_(self):
        return config.dbschemas

    def arguments(self):
        return (Field('date_from', _("Od"), type=pd.Date, not_null=True,
                      default=lambda: pd.Date.now().value()),
                Field('date_to', _("Do"), type=pd.Date, not_null=True,
                      default=lambda: pd.Date.now().value()),
                Field('username_', _("Uživatel"), type=pd.String),
                Field('tablename_', _("Tabulka"), type=pd.String),
                Field('key_value_', _("Klíč"), type=pd.String),
                Field('detail_', _("Řádek obsahuje"), type=pd.String),
                Field('search_path_', _("Schema"), type=pd.String, not_null=True,
                      default=self._df_search_path_)
                )
    
    def fields(self): return (
        Field("id", _("ID"), width=10, type=pd.Integer),
        Field("timestamp", _("Datum a čas"), width=17, type=pd.DateTime),
        Field("username", _("Uživatel"), width=20, type=pd.String),
        Field("schemaname", _("Schema"), width=20, type=pd.String),
        Field("tablename", _("Tabulka"), width=20, type=pd.String),
        Field("operation", _("Operace"), width=10, type=pd.String),
        Field("key_column", _("ID klíč"), width=20, type=pd.String),
        Field("key_value", _("Klíč"), width=20, type=pd.String),
        Field("detail", _("Řádek"), width=40, height=20, type=pd.String),
        )


class ChangesLogUser(ChangesLog):
    """Specification providing query fields for users"""
    public = True
    
    def query_fields(self):
        return QueryFields(self.arguments,
                           layout=HGroup(VGroup('date_from', 'date_to'),
                                         VGroup('username_', 'search_path_'),
                                         VGroup('tablename_', 'key_value_'),
                                         'detail_',))
    
    def argument_provider(self, value_dict, query_fields):
        if query_fields:
            date_from   = query_fields['date_from'] 
            date_to     = query_fields['date_to'] 
            username_   = query_fields['username_']  
            tablename_  = query_fields['tablename_'] 
            key_value_  = query_fields['key_value_'] 
            detail_  = query_fields['detail_']
            search_path_ = query_fields['search_path_']
        else:
            date_from  = pd.dval(None)
            date_to    = pd.dval(None)
            username_  = pd.sval(None)
            tablename_ = pd.sval(None)
            key_value_ = pd.sval(None)
            detail_ = pd.sval(None)
            search_path_ = pd.sval(None)
        return {
            'date_from':  date_from, 
            'date_to':    date_to,   
            'username_':  username_, 
            'tablename_': tablename_,
            'key_value_': key_value_,
            'detail_': detail_,
            'search_path_': search_path_}
    
def proc_spec():

    def show_changes_in_row(key_value, tablename=None):
        CHANGE_LOG_SPEC = 'logging.ChangesLog'
        arguments = {"date_from": pd.dval(datetime.date(year=2000, month=1, day=1)),
                     "date_to": pd.dval(datetime.date(year=2100, month=1, day=1)),
                     "key_value_": pd.sval(key_value.export()),
                     "schemaname_": pd.sval(config.dbschemas)}
        if tablename is not None:
            arguments["tablename_"] = pd.sval(tablename)
        return run_form(BrowseForm, CHANGE_LOG_SPEC, arguments=arguments)

    return {"show_changes_in_row": show_changes_in_row}
