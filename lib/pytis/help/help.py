# -*- coding: utf-8 -*-

# Copyright (C) 2018-2020 Tomáš Cerha <t.cerha@gmail.com>
# Copyright (C) 2012-2017 OUI Technology Ltd.
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

  'HelpGenerator' -- Generate a final help page contents according to the texts
    defined in the database.

The last important part of the pytis help system is the user interface for
managment of the help texts in the database by help administrators.  This is
defined in 'pytis.defs.help.Help' (and can be used as 'help.Help' when
'pytis.defs' is in resolver search path).

"""
from __future__ import print_function
from __future__ import unicode_literals

import sys
import os
import lcg
import pytis.data as pd
import pytis.form
import pytis.util

from pytis.util import log, OPERATIONAL, translations, ProgramError

_ = translations('pytis-wx')

unistr = type(u'')  # Python 2/3 transition hack.


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
        self._menu_help_data = pd.dbtable(
            'e_pytis_help_menu',
            ('fullname', 'content', 'changed', 'removed'),
            pytis.config.dbconnection,
        )
        self._spec_help_data = pd.dbtable(
            'e_pytis_help_spec',
            ('spec_name', 'description', 'help', 'changed', 'removed'),
            pytis.config.dbconnection,
        )
        self._spec_help_items_data = pd.dbtable(
            'e_pytis_help_spec_items',
            ('item_id', 'spec_name', 'kind', 'identifier', 'content', 'changed', 'removed'),
            pytis.config.dbconnection,
        )
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
            print(e)
            return
        description = view_spec.description()
        help_text = (view_spec.help() or '').strip() or None
        self._update(self._spec_help_data, dict(spec_name=spec_name),
                     description=description, help=help_text)
        data = self._spec_help_items_data
        for kind, items in (('field', view_spec.fields()),
                            ('profile', view_spec.profiles().unnest()),
                            ('binding', view_spec.bindings()),
                            ('action', view_spec.actions(unnest=True))):
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
            print("Ignoring menu item of unknown type:", (fullname, spec_name))
            return
        self._update(self._menu_help_data, dict(fullname=fullname), content=content)
        if spec_name and kind != 'handle' and spec_name not in ('ui', 'export'):
            self._update_spec_help(spec_name)

    def _spec_link(self, spec_name):
        resolver = pytis.util.resolver()
        try:
            view_spec = resolver.get(spec_name, 'view_spec')
        except pytis.util.ResolverError as e:
            print(e)
            title = spec_name
        else:
            title = view_spec.title()
        return '[help:spec/%s %s]' % (spec_name, title)

    def _generate_form_help(self, form_class, spec_name):
        if form_class.endswith('.ConfigForm'):
            if spec_name == 'ui':
                return _("Opens a form for customizing the application user interface.")
            if spec_name == 'export':
                return _("Opens a form for customizing data export.")
            else:
                return None
        elif '::' in spec_name:
            resolver = pytis.util.resolver()
            mainname, sidename = spec_name.split('::')
            try:
                bspec = resolver.get(mainname, 'binding_spec')
            except pytis.util.ResolverError as e:
                print(e)
                return None
            if not isinstance(bspec, dict):
                print("Can't create dual form '%s': %s.binding_spec() is not a dictionary" %
                      (spec_name, mainname))
                return None
            b = bspec[sidename]
            description = b.help() or b.description()
            if description is None:
                def clabel(cid, name):
                    try:
                        c = resolver.get(name, 'view_spec').field(cid)
                    except pytis.util.ResolverError as e:
                        print(e)
                        return cid
                    return c.label()
                description = _("Forms are connected through the equality of values "
                                "in column '%(main_form_column)s' of the main form and "
                                "column '%(side_form_column)s' of the side form.",
                                main_form_column=clabel(mainname, b.binding_column()),
                                side_form_column=clabel(sidename, b.side_binding_column()))
            return '%s\n\n:%s::%s\n:%s::%s\n\n%s' % (
                _("Opens a dual form:"),
                _("Main form"),
                self._spec_link(mainname),
                _("Side form"),
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
            return _("Opens %(form_type)s for %(spec_name)s",
                     form_type=form_type, spec_name=self._spec_link(spec_name))

    def _generate_new_record_help(self, spec_name, command_name):
        if spec_name:
            return _("Opens a form for new record insertion into %s.",
                     self._spec_link(spec_name))
        else:
            # Maybe retrieve custom application command here?
            # command, args = pytis.form.custom_command(command_name)
            # if command and command.__doc__:
            #     return command.__doc__
            # else:
            return _("Invokes command %s.") % command_name

    def _generate_proc_help(self, procname, spec_name):
        return _("Invokes procedure %s from %s.") % (procname, spec_name)

    def _generate_action_help(self, action):
        return _("Invokes %s.") % action

    def _generate_reload_rights_help(self):
        return _("Reloads access rights.")

    def _generate_run_form_help(self, formname):
        return _("Invokes form %s.") % formname

    def _generate_exit_help(self):
        return _("Exits the application.")

    def update(self):
        """(Re)generate help for all menu items."""
        data = pd.dbtable('ev_pytis_help', ('help_id', 'fullname', 'spec_name', 'position'),
                          pytis.config.dbconnection)
        data.select(sort=(('position', pd.ASCENDENT),))
        while True:
            row = data.fetchone()
            if row is None:
                break
            if row['fullname'].value():
                self._update_menu_item_help(row['fullname'].value(), row['spec_name'].value())


generator = None


def help_page(uri):
    """Return a help page for given URI as a 'lcg.ContentNode' instance."""
    global generator
    if not generator:
        for cls in (DmpHelpGenerator, SpecHelpGenerator):
            try:
                generator = cls()
                break
            except HelpGenerator.NotAvailable:
                continue
        else:
            raise ProgramError("No usable help generator available.")
    return generator.help_page(uri)


class HelpGenerator(object):
    """Generate help page content and menu structure for the help browser.

    Generate the help as lcg.ContentNode structure for the current application
    from the available source of help texts.

    This base class defines the common parts but is abstract regarding the
    source of help texts.  The derived classes must implement the methods for
    obtaining the texts from the relevant source (see the derived classes
    below).

    """
    class NotAvailable(Exception):
        pass

    class ContentNode(lcg.ContentNode):

        def append_child(self, node):
            self._children += (node,)
            node._set_parent(self)

        def update(self, content):
            self._default_variant = lcg.Variant('--', content=content)

    class EmptyContent(lcg.Content):
        pass

    def __init__(self):
        self._resource_provider = lcg.ResourceProvider(dirs=pytis.config.resource_path)
        self._root_node = None

    def _application_help_nodes(self):
        pass

    def _application_help_page_content(self, node, uri):
        pass

    def _pytis_help_nodes(self):
        def clone(node, id):
            return self.ContentNode(
                id=id,
                title=node.title(),
                descr=node.descr(),
                content=node.content(),
                hidden=node.hidden(),
                children=[clone(n, 'help:pytis/' + n.id()) for n in node.children()],
                resource_provider=self._resource_provider,
                foldable=True,
            )
        directory = os.path.join(pytis.config.help_dir, 'src')
        reader = lcg.reader(directory, 'pytis', ext='txt',
                            resource_provider=self._resource_provider)
        try:
            node = reader.build()
        except IOError as e:
            log(OPERATIONAL, "Unable to read Pytis help files from '%s':" % directory, e)
            node = self.ContentNode('help:pytis', title=_("Pytis User Guide"),
                                    content=lcg.p(_("Help files not found!")),
                                    hidden=True, resource_provider=self._resource_provider)
        else:
            node = clone(node, 'help:pytis')
        return node

    def _spec_link(self, spec_name, title=None):
        if title is None:
            resolver = pytis.util.resolver()
            try:
                view_spec = resolver.get(spec_name, 'view_spec')
            except pytis.util.ResolverError:
                title = spec_name
            else:
                title = view_spec.title()
        return lcg.link('help:spec/%s' % spec_name, title)

    def _description(self, kind, spec_name, identifier, default):
        if kind in ('description', 'help'):
            return default
        else:
            return default or _("Description not available.")

    def _spec_help_resources(self, spec_name):
        return ()

    def _spec_help_content(self, spec_name):
        if not pytis.form.has_access(spec_name):
            return (_("Access Denied"),
                    lcg.p(_("You don't have permissions for specification „%s“.") % spec_name))
        resolver = pytis.util.resolver()
        try:
            view_spec = resolver.get(spec_name, 'view_spec')
        except pytis.util.ResolverError:
            return None, None

        def field_label(f):
            label = f.column_label()
            if not label:
                label = f.label() or f.id()
            elif f.label() and f.label() != label:
                label += ' (' + f.label() + ')'
            return label

        def field_description(f):
            result = self._description('field', spec_name, f.id(), f.descr())
            related_specnames = [name for name in
                                 [f.codebook()] + [link.name() for link in f.links()] if name]
            if related_specnames:
                pytis.util.remove_duplicates(related_specnames)
                result = (result,
                          lcg.p(_("Related views:")),
                          lcg.ul([self._spec_link(name) for name in related_specnames]))
            return result

        parser = lcg.Parser()
        content = lcg.Container(
            [lcg.TableOfContents(title=_("Contents"))] + [
                lcg.Section(title=title, content=func(content)) for title, func, content in (
                    (_("Overview"), lcg.p,
                     self._description('description', spec_name, None, view_spec.description())),
                    (_("Description"), parser.parse,
                     self._description('help', spec_name, None, view_spec.help())),
                    (_("Form fields"), lcg.dl,
                     sorted([(field_label(f), field_description(f)) for f in view_spec.fields()],
                            key=lambda x: x[0])),
                    (_("Profiles"), lcg.dl,
                     [(p.title(), self._description('profile', spec_name, p.id(), p.descr()))
                      for p in view_spec.profiles().unnest()]),
                    (_("Context menu actions"), lcg.dl,
                     [(a.title(), (self._description('action', spec_name, a.id(), a.descr()),
                                   (' ', _("Access Rights"), ': ', ', '.join(a.access_groups()))
                                   if a.access_groups() else ''))
                      for a in view_spec.actions(unnest=True)]),
                    (_("Side forms"), lcg.dl,
                     [(self._spec_link(b.name(), b.title()),
                       self._description('binding', spec_name, b.id(), b.descr()))
                      for b in view_spec.bindings() if b.name()]),
                    (_("Access Rights"), lcg.dl,
                     self._access_rights(spec_name, view_spec)),
                ) if content
            ],
            resources=self._spec_help_resources(spec_name)
        )
        return view_spec.title(), content

    def _access_rights(self, spec_name, view_spec):
        return None

    def _root(self):
        if self._root_node:
            node = self._root_node
        else:
            node = self.ContentNode('help:', content=lcg.Content(), hidden=True, children=(
                self.ContentNode('help:application',
                                 title=_("%s application help") % pytis.config.application_name,
                                 content=lcg.NodeIndex(),
                                 foldable=True,
                                 resource_provider=self._resource_provider,
                                 children=self._application_help_nodes()),
                self._pytis_help_nodes(),
            ))
            self._root_node = node
        return node

    def help_page(self, uri):
        """Return the complete help structure of LCG nodes with content for given uri.

        The returned value is an 'lcg.ContentNode' instance, which is a part of
        the complete menu structure (it refers to other 'lcg.ContentNode'
        instances through the parent/child relationships).  Only the returned
        node, however, actually has content (page to be displayed in the help
        browser).  Other nodes have empty content as they are only important as
        navigation menu items.

        """
        root = self._root()
        if uri.startswith('help:spec/'):
            # Separate specification descriptions are not part of the
            # static node structure generated by _root().
            title, content = self._spec_help_content(uri[10:])
            if content:
                node = self.ContentNode(
                    uri,
                    title=title,
                    hidden=True,
                    content=content,
                    resource_provider=self._resource_provider,
                )
                root.append_child(node)
            else:
                node = None
        else:
            # Try to find the node in the statically
            node = root.find_node(uri)
            if node and isinstance(node.content(), self.EmptyContent):
                node.update(self._application_help_page_content(node, uri))
        if not node:
            node = root.find_node('NotFound')
            if not node:
                node = self.ContentNode('NotFound', title=_("Not Found"), hidden=True,
                                        content=(), resource_provider=self._resource_provider)
                root.append_child(node)
            node.update(lcg.p(_("The requested help page not found: %s", uri)))
        return node


class SpecHelpGenerator(HelpGenerator):
    """Generate help page contents directly from the application specifications."""

    def _application_help_nodes(self):
        counter = pytis.util.Counter()
        app = pytis.config.resolver.specification('Application')

        def node(item):
            if isinstance(item, pytis.form.Menu):
                content = lcg.NodeIndex()
                children = [node(i) for i in item.items()
                            if not isinstance(i, pytis.form.MSeparator)]
                globs = {}
            else:
                content = self.EmptyContent()
                children = ()
                globs = dict(command=item.command(), args=item.args())
            return self.ContentNode(
                'help:application/%d' % counter.next(), title=item.title(),
                content=content, children=children, foldable=True, globals=globs,
                resource_provider=self._resource_provider,
            )
        return [
            self.ContentNode('help:application/menu', title=_("Application menu"),
                             content=lcg.NodeIndex(), foldable=True,
                             children=[node(item) for item in app.menu()],
                             resource_provider=self._resource_provider),
            # TODO: Read the static part of application help from the filesystem?
        ]

    def _application_help_page_content(self, node, uri):
        globs = node.globals()
        command, args = globs['command'], globs['args']
        if command in (pytis.form.Application.COMMAND_RUN_FORM,
                       pytis.form.Application.COMMAND_NEW_RECORD):
            content = self._spec_help_content(args['name'])[1]
            if not content:
                if command == pytis.form.Application.COMMAND_RUN_FORM:
                    text = _("Opens the %s on specification", args['form_class'].descr())
                else:
                    text = _("Inserts a new record into")
                content = lcg.p(text, ' ', self._spec_link(args['name']), '.')
            return content
        return lcg.Container((
            lcg.p(_("Runs the command %s with the following arguments:", command.id())),
            lcg.fieldset([(k, unistr(v)) for k, v in args.items()])
        ))

    def _access_rights(self, spec_name, view_spec):
        access_rights = pytis.config.resolver.get(spec_name, 'access_spec')
        if not access_rights:
            return None
        permissions = (
            (_("View"), pd.Permission.VIEW),
            (_("Insert"), pd.Permission.INSERT),
            (_("Update"), pd.Permission.UPDATE),
            (_("Delete"), pd.Permission.DELETE),
            (_("Call"), pd.Permission.CALL),
            (_("Export"), pd.Permission.EXPORT),
            (_("Print"), pd.Permission.PRINT),
        )
        result = []
        for label, permission in permissions:
            groups = tuple(access_rights.permitted_groups(permission, None))
            if groups:
                result.append((label, ', '.join(groups)))
            field_access = {}
            for field in view_spec.fields():
                field_groups = tuple(access_rights.permitted_groups(permission, field.id()))
                if field_groups != groups:
                    field_access.setdefault(field_groups, []).append(field)
            for groups, fields in field_access.items():
                result.append(((label, ' (', ', '.join(f.label() for f in fields), ')'),
                               ', '.join(groups)))
        return result


class DmpHelpGenerator(HelpGenerator):
    """Generate help page contents according to the texts in DMP database."""

    def __init__(self):
        try:
            data = pytis.data.dbtable(
                'ev_pytis_user_help',
                ('help_id', 'fullname', 'spec_name', 'page_id', 'position',
                 'title', 'description', 'menu_help', 'content',),
                pytis.config.dbconnection,
            )
        except pd.DBException:
            log(OPERATIONAL, "Not using DMP help: DMP help tables not found in database.")
            raise self.NotAvailable()
        count = data.select(condition=pd.NE('help_id', pd.sval('menu/')))
        data.close()
        if count == 0:
            log(OPERATIONAL, "Not using DMP help: DMP help tables are empty.")
            raise self.NotAvailable()
        self._data = data
        self._cached_descriptions = {}
        super(DmpHelpGenerator, self).__init__()

    def _load_descriptions(self, spec_name):
        data = pytis.data.dbtable('e_pytis_help_spec_items',
                                  ('spec_name', 'kind', 'identifier', 'content'),
                                  pytis.config.dbconnection)
        data.select(condition=pytis.data.EQ('spec_name', pytis.data.sval(spec_name)))
        descriptions = dict([(x, {}) for x in
                             ('help', 'description', 'field', 'action', 'binding', 'profile')])
        while True:
            row = data.fetchone()
            if row is None:
                break
            content = row['content'].value()
            if content:
                descriptions[row['kind'].value()][row['identifier'].value()] = content
        data.close()
        data = pytis.data.dbtable('e_pytis_help_spec', ('spec_name', 'description', 'help'),
                                  pytis.config.dbconnection)
        row = data.row((pytis.data.Value(data.find_column('spec_name').type(), spec_name),))
        if row:
            for kind in ('help', 'description'):
                value = row[kind].value()
                if value:
                    descriptions[kind][None] = value
        return descriptions

    def _description(self, kind, spec_name, identifier, default):
        try:
            descriptions = self._cached_descriptions[spec_name]
        except KeyError:
            descriptions = self._cached_descriptions[spec_name] = self._load_descriptions(spec_name)
        try:
            return descriptions[kind][identifier]
        except KeyError:
            return super(DmpHelpGenerator, self)._description(kind, spec_name, identifier, default)

    def _spec_help_resources(self, spec_name):
        storage = pytis.presentation.DbAttachmentStorage('e_pytis_help_spec_attachments',
                                                         'spec_name', spec_name)
        return storage.resources()

    def _application_help_nodes(self):
        def node(row, children):
            return self.ContentNode('help:' + row['help_id'].value(), title=row['title'].value(),
                                    descr=row['description'].value(), foldable=True,
                                    content=self.EmptyContent(),
                                    resource_provider=self._resource_provider,
                                    children=[node(r, children) for r in
                                              children.get(row['position'].value(), ())])
        self._data.select(sort=(('position', pytis.data.ASCENDENT),))
        children = {}
        while True:
            row = self._data.fetchone()
            if not row:
                break
            parent = '.'.join(row['position'].value().split('.')[:-1]) or None
            children.setdefault(parent, []).append(row)
        self._data.close()
        return [node(r, children) for r in children[None]]

    def _application_help_page_content(self, node, uri):
        self._data.select(condition=pd.EQ('help_id', pd.sval(uri[5:])))
        row = self._data.fetchone()
        self._data.close()
        if row:
            if row['page_id'].value():
                storage = pytis.presentation.DbAttachmentStorage(
                    'e_pytis_help_pages_attachments', 'page_id', row['page_id'].value(),
                )
                return lcg.Container(lcg.Parser().parse(row['content'].value() or ''),
                                     resources=storage.resources())
            if row['menu_help'].value():
                return lcg.Parser().parse(row['menu_help'].value())
            else:
                spec_name = row['spec_name'].value()
                # TODO: Use the default descriptions from HelpUpdater.
                if spec_name:
                    content = self._spec_help_content(spec_name)[1]
                    if content:
                        return content
        return lcg.Content()


class HelpExporter(pytis.form.Browser.Exporter):

    _STYLES = ('default.css', 'pytis-help.css')
    _PAGE_STRUCTURE = (
        pytis.form.Browser.Exporter.Part('menu'),
        pytis.form.Browser.Exporter.Part('heading'),
        pytis.form.Browser.Exporter.Part('content'),
    )

    def _menu(self, context):
        g = self._generator
        tree = lcg.FoldableTree(tooltip=_("Expand/collapse complete menu hierarchy"))
        return g.div((g.div(_("Navigation"), cls='menu-heading'),
                      tree.export(context)),
                     cls='menu-panel')
