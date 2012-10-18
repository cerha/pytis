# -*- coding: utf-8 -*-

# Copyright (C) 2012 Brailcom, o.p.s.
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
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

"""Classes for management and generation of help for Pytis applications.

Help texts are stored in the database and edited by help administrator.

This module includes the following classes:

  'HelpUpdater' -- Create/update initial help texts in the database according
    to specifications.
   
The last important part of the pytis help system is the user interface for
managment of the help texts in the database by help administrators.  This is
defined in 'pytis.defs.help.Help' (and can be used as 'help.Help' when
'pytis.defs' is in resolver search path).
  
""" 

import sys, os, lcg, pytis.data as pd, pytis.form, pytis.util, config

class HelpUpdater(object):
    """Create/update initial help texts in the database according to specifications.

    The initial help contents is created automatically based on the information
    available in specifications.  It is supposed to be further edited and
    maintained by help administrator.  The contents of the database must be
    regularly synchronized with the specifications after changes in the
    application, so that new objects (views, fields, profiles etc.) get visible
    in the help database, old objects are removed and changed or renamed
    objects are updated.

    """
    def __init__(self):
        self._menu_help_data = pd.dbtable('e_pytis_help_menu', ('fullname', 'content', 'changed', 'removed'),
                                          config.dbconnection)
        self._spec_help_data = pd.dbtable('e_pytis_help_spec', ('spec_name', 'description', 'help', 'changed', 'removed'),
                                          config.dbconnection)
        self._spec_help_items_data = pd.dbtable('e_pytis_help_spec_items',
                                                ('item_id', 'spec_name', 'kind', 'identifier',
                                                 'content', 'changed', 'removed'), config.dbconnection)
        self._done = {}

    def _row(self, data, **kwargs):
        data.select(condition=pd.AND(*[pd.EQ(k, v) for k, v in self._values(data, **kwargs)]))
        row = data.fetchone()
        data.close()
        return row

    def _values(self, data, **kwargs):
        return [(k, pd.Value(data.find_column(k).type(), v)) for k, v in kwargs.items()]

    def _update(self, data, key, **kwargs):
        row = self._row(data, **key)
        rowdata = self._values(data, changed=False, removed=False, **kwargs)
        if row is None:
            data.insert(pd.Row(self._values(data, **key) + rowdata))
        elif not row['changed'].value() and any([row[k].value() != v.value() for k, v in rowdata]):
            data.update((row[0],), pd.Row(rowdata))

    def _update_spec_help(self, spec_name):
        if self._done.get(spec_name):
            return
        self._done[spec_name] = True
        resolver = pytis.util.resolver()
        try:
            view_spec = resolver.get(spec_name, 'view_spec')
        except pytis.util.ResolverError as e:
            print e
            return
        description = view_spec.description()
        help_text = (view_spec.help() or '').strip() or None
        self._update(self._spec_help_data, dict(spec_name=spec_name),
                     description=description, help=help_text)
        data = self._spec_help_items_data
        for kind, items in (('field', view_spec.fields()),
                            ('profile', view_spec.profiles()),
                            ('binding', view_spec.bindings()),
                            ('action', view_spec.actions(linear=True))):
            for item in items:
                self._update(data, dict(spec_name=spec_name, kind=kind,
                                        identifier=item.id()),
                             content=item.descr())
            # Items which were modified (content changed by hand) are kept with
            # the 'removed' flag set, since the texts may be reused for other
            # items in case of identifier change or other rearrangements.  It
            # will also automatically resurrect texts for items which are
            # removed temporarily (eg. commented out) which is quite common
            # during development.  Items which were not ever modified or have
            # no content may be safely deleted (they contain no hand-edited
            # data).
            conds = [pd.EQ('spec_name', pd.sval(spec_name)),
                     pd.EQ('kind', pd.sval(kind)),
                     pd.NOT(pd.ANY_OF('identifier', *[pd.sval(item.id()) for item in items])),
                     ]
            data.delete_many(pd.AND(*(conds + [pd.OR(pd.EQ('changed', pd.bval(False)),
                                                     pd.EQ('content', pd.sval(None)))])))
            data.update_many(pd.AND(*(conds + [pd.EQ('removed', pd.bval(False))])),
                             pd.Row(self._values(data, removed=True)))
        
    def _update_menu_item_help(self, fullname, spec_name):
        kind = fullname.split('/', 1)[0]
        if kind == 'menu':
            content = None
        elif kind == 'form' and spec_name:
            form_class = fullname.split('/')[1]
            content = self._generate_form_help(form_class, spec_name)
        elif kind == 'proc':
            procname = fullname.strip('/').split('/')[1]
            content = self._generate_proc_help(procname, spec_name)
        elif kind == 'handle':
            action = fullname.strip('/').split('/')[1]
            content = self._generate_action_help(action)
        elif kind == 'NEW_RECORD':
            if not fullname.endswith('/'):
                # This is probably possible also for other fullname kinds...
                command_name = spec_name
                spec_name = None
            else:
                command_name = None
            content = self._generate_new_record_help(spec_name, command_name)
        elif kind == 'RELOAD_RIGHTS':
            content = self._generate_reload_rights_help()
        elif kind == 'RUN_FORM':
            content = self._generate_run_form_help(spec_name)
        elif kind == 'EXIT':
            content = self._generate_exit_help()
        else:
            print "Ignoring menu item of unknown type:", (fullname, spec_name)
            return
        self._update(self._menu_help_data, dict(fullname=fullname), content=content)
        if spec_name and kind != 'handle' and spec_name not in ('ui', 'export'):
            self._update_spec_help(spec_name)

    def _spec_link(self, spec_name):
        resolver = pytis.util.resolver()
        try:
            view_spec = resolver.get(spec_name, 'view_spec')
        except pytis.util.ResolverError as e:
            print e
            title = spec_name
        else:
            title = view_spec.title()
        return '[help:spec/%s %s]' % (spec_name, title)
        
    def _generate_form_help(self, form_class, spec_name):
        if form_class.endswith('.ConfigForm'):
            if spec_name == 'ui':
                return _(u"Vyvolá formulář pro přizpůsobení uživatelského rozhraní aplikace.")
            if spec_name == 'export':
                return _(u"Vyvolá formulář uživatelského nastavení exportu dat.")
            else:
                return None
        elif '::' in spec_name:
            resolver = pytis.util.resolver()
            mainname, sidename = spec_name.split('::')
            try:
                bspec = resolver.get(mainname, 'binding_spec')
            except pytis.util.ResolverError as e:
                print e
                return None
            if not isinstance(bspec, dict):
                print "Can't create dual form '%s': %s.binding_spec() is not a dictionary" % \
                    (spec_name, mainname)
                return None
            b = bspec[sidename]
            description = b.help() or b.description()
            if description is None:
                def clabel(cid, name):
                    try:
                        c = resolver.get(name, 'view_spec').field(cid)
                    except pytis.util.ResolverError as e:
                        print e
                        return cid
                    return c.label()
                description = (_(u"Formuláře jsou propojeny přes shodu hodnot sloupce '%s' "
                                 u"hlavního formuláře a sloupce '%s' vedlejšího formuláře.") % 
                               (clabel(mainname, b.binding_column()),
                                clabel(sidename, b.side_binding_column())))
            return '%s\n\n:%s::%s\n:%s::%s\n\n%s' % (
                _(u"Otevře duální formulář:"),
                _(u"Hlavní formulář"),
                self._spec_link(mainname),
                _(u"Vedlejší formulář"),
                self._spec_link(sidename),
                description)
        else:
            if form_class.startswith('pytis.form.'):
                form_name = form_class[11:]
                if form_name.startswith('dualform.'):
                    form_name = form_name[9:]
                form_type = getattr(pytis.form, form_name).DESCR
            else:
                form_type = form_class
            return _(u"Otevře %s pro %s") % (form_type, self._spec_link(spec_name))

    def _generate_new_record_help(self, spec_name, command_name):
        if spec_name:
            return _(u"Vyvolá formulář pro vložení nového záznamu do náhledu %s.") % self._spec_link(spec_name)
        else:
            #resolver = pytis.util.resolver()
            #try:
            #    command = resolver.get('app_commands', command_name)
            #except pytis.util.ResolverError as e:
            #    print e
            #    command = None
            #if command and command.__doc__:
            #    return command.__doc__
            #else:
            return _(u"Vyvolá příkaz %s.") % command_name
    
    def _generate_proc_help(self, procname, spec_name):
        return _(u"Vyvolá proceduru %s z %s.") % (procname, spec_name)

    def _generate_action_help(self, action):
        return _(u"Vyvolá funkci %s.") % action

    def _generate_reload_rights_help(self):
        return _(u"Vyvolá přenačtení práv.")

    def _generate_run_form_help(self, formname):
        return _(u"Vyvolá formulář %s.") % formname

    def _generate_exit_help(self):
        return _(u"Ukončí běh aplikace.")

    def update(self):
        """(Re)generate help for all menu items."""
        data = pd.dbtable('ev_pytis_help', ('help_id', 'fullname', 'spec_name', 'position'),
                          config.dbconnection)
        data.select(sort=(('position', pd.ASCENDENT),))
        while True:
            row = data.fetchone()
            if row is None:
                break
            if row['fullname'].value():
                self._update_menu_item_help(row['fullname'].value(), row['spec_name'].value())
