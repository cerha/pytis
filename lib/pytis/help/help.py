# -*- coding: utf-8 -*-

# Copyright (C) 2012, 2013, 2014 Brailcom, o.p.s.
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

import sys, os, lcg, pytis.data as pd, pytis.form, pytis.util, config
from pytis.util import current_language, log, OPERATIONAL, translations

_ = translations('pytis-wx')

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
            #resolver = pytis.util.resolver()
            #try:
            #    command = resolver.get('app_commands', command_name)
            #except pytis.util.ResolverError as e:
            #    print e
            #    command = None
            #if command and command.__doc__:
            #    return command.__doc__
            #else:
            return _("Invokes command %s.") % command_name
    
    def _generate_proc_help(self, procname, spec_name):
        return _("Invokes procedure %s from %s.") % (procname, spec_name)

    def _generate_action_help(self, action):
        return _("Invokes  %s.") % action

    def _generate_reload_rights_help(self):
        return _("Reloads access rights.")

    def _generate_run_form_help(self, formname):
        return _("Invokes form %s.") % formname

    def _generate_exit_help(self):
        return _("Exits the application.")

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
                

class HelpGenerator(object):
    """Generate help page contents according to the current texts in the database."""
    
    def _spec_items_descriptions(self, spec_name):
        data = pytis.data.dbtable('e_pytis_help_spec_items',
                                        ('spec_name', 'kind', 'identifier', 'content'),
                                        config.dbconnection)
        data.select(condition=pytis.data.EQ('spec_name', pytis.data.sval(spec_name)))
        descriptions = dict([(x, {}) for x in ('field', 'action', 'binding', 'profile')])
        while True:
            row = data.fetchone()
            if row is None:
                break
            if row['content'].value():
                descriptions[row['kind'].value()][row['identifier'].value()] = row['content'].value()
        data.close()
        return descriptions

    def _spec_help_content(self, spec_name):
        from pytis.form import has_access
        if not has_access(spec_name):
            return (_("Access denied"),
                    lcg.p(_("You don't have permissions for specification „%s“.") % spec_name))
        resolver = pytis.util.resolver()
        def spec_link(spec_name, title=None):
            if title is None:
                try:
                    view_spec = resolver.get(spec_name, 'view_spec')
                except pytis.util.ResolverError as e:
                    title = spec_name
                else:
                    title = view_spec.title()
            return lcg.link('help:spec/%s' % spec_name, title)
        descriptions = self._spec_items_descriptions(spec_name)
        def description(kind, identifier, default):
            return descriptions[kind].get(identifier, default or _("Description not available."))
        def field_label(f):
            label = f.column_label()
            if not label:
                label = f.label() or f.id()
            elif f.label() and f.label() != label:
                label += ' ('+ f.label()+')'
            return label
        def field_description(f):
            result = description('field', f.id(), f.descr())
            related_specnames = [name for name in
                                 [f.codebook()] + [link.name() for link in f.links()] if name]
            if related_specnames:
                pytis.util.remove_duplicates(related_specnames)
                result = (result,
                          lcg.p(_("Related views:")),
                          lcg.ul([spec_link(name) for name in related_specnames]))
            return result
        try:
            view_spec = resolver.get(spec_name, 'view_spec')
        except pytis.util.ResolverError as e:
            return None, None
        data = pytis.data.dbtable('e_pytis_help_spec', ('spec_name', 'description', 'help'),
                                  config.dbconnection)
        row = data.row((pytis.data.Value(data.find_column('spec_name').type(), spec_name),))
        if row:
            spec_description = row['description'].value()
            spec_help = row['help'].value()
        else:
            spec_description = view_spec.description()
            spec_help = view_spec.help()
        parser = lcg.Parser()
        sections = [
            lcg.Section(title=title, content=f(content))
            for title, f, content in (
                (_("Overview"), lcg.p, spec_description),
                (_("Description"), parser.parse, spec_help),
                (_("Form fields"), lcg.dl,
                 sorted([(field_label(f), field_description(f)) for f in view_spec.fields()],
                        key=lambda x: x[0])),
                (_("Profiles"), lcg.dl,
                 [(p.title(), description('profile', p.id(), p.descr()))
                  for p in view_spec.profiles()]),
                (_("Context menu actions"), lcg.dl,
                 [(a.title(), description('action', a.id(), a.descr()))
                  for a in view_spec.actions(linear=True)]),
                (_("Side forms"), lcg.dl,
                 [(spec_link(b.name(), b.title()), description('binding', b.id(), b.descr()))
                  for b in view_spec.bindings() if b.name()]),
                )
            if content]
        #data_spec = resolver.get(specname, 'data_spec')
        #rights = data_spec.access_rights()
        #if rights:
        #    content += "\n\n== %s ==\n\n" % _(u"Přístupová práva")
        #    for perm in (pd.Permission.VIEW,
        #                 pd.Permission.INSERT,
        #                 pd.Permission.UPDATE,
        #                 pd.Permission.DELETE,
        #                 pd.Permission.EXPORT):
        #        groups = [g for g in rights.permitted_groups(perm, None) if g]
        #        content += ":%s:: %s\n" % (perm, ', '.join(map(str, groups)) or _(u"Nedefinováno"))
        storage = pytis.presentation.DbAttachmentStorage('e_pytis_help_spec_attachments',
                                                         'spec_name', spec_name,
                                                         base_uri='resource:')
        return view_spec.title(), lcg.Container(sections, resources=storage.resources())

    def _pytis_help(self, resource_dirs):
        image_dir = os.path.join(config.help_dir, 'img')
        resource_provider = lcg.ResourceProvider(dirs=[image_dir] + resource_dirs)
        def clone(node, node_id):
            # Clone the content node, but force `id' to help URI and `foldable' to True.
            return lcg.ContentNode(node_id, title=node.title(), descr=node.descr(),
                                   content=node.content(), hidden=node.hidden(),
                                   children=[clone(n, 'help:pytis/'+n.id()) for n in node.children()],
                                   resource_provider=resource_provider,
                                   foldable=True)
        try:
            node = self._pytis_help_root_node
        except AttributeError:
            directory = os.path.join(config.help_dir, 'src')
            reader = lcg.reader(directory, 'pytis', ext='txt')
            try:
                node = self._pytis_help_root_node = reader.build()
            except IOError as e:
                log(OPERATIONAL, "Unable to read Pytis help files from '%s':" % directory, e)
                node = lcg.ContentNode('pytis:', title=_("Pytis User Guide"),
                                       content=lcg.p(_("Help files not found!")),
                                       hidden=True, resource_provider=resource_provider)
        # We need to clone on every request because set_parent() is called on
        # the result when added to the help tree and set_parent may not be
        # called multiple times.
        return clone(node, 'help:pytis')

    def help_page(self, topic):
        """Return the help page for given topic as lcg.ContentNode instance."""
        resource_dirs = [d[:-3]+'resources' for d in sys.path
                         if d.endswith('/pytis/lib') or d.endswith('/lcg/lib')]
        resource_provider = lcg.ResourceProvider(dirs=resource_dirs)
        def make_node(row, children):
            if row['help_id'].value() == topic:
                # If this is the currently displayed node, create the content.
                # Other nodes are only generated for their presence in the
                # menu.
                parser = lcg.Parser()
                if row['page_id'].value():
                    storage = pytis.presentation.DbAttachmentStorage(
                        'e_pytis_help_pages_attachments',
                        'page_id', row['page_id'].value(),
                        base_uri='resource:')
                    content = lcg.Container(parser.parse(row['content'].value()),
                                            resources=storage.resources())
                else:
                    content = [lcg.TableOfContents(title=_("Contents"))]
                    fullname, spec_name = row['fullname'].value(), row['spec_name'].value()
                    if row['menu_help'].value():
                        content.extend(parser.parse(row['menu_help'].value()))
                    if fullname and fullname.startswith('handle/'):
                        pass
                    elif fullname and fullname.startswith('proc/'):
                        pass
                    elif spec_name == 'export':
                        pass
                    elif spec_name == 'ui':
                        pass
                    elif spec_name:
                        spec_help_content = self._spec_help_content(spec_name)[1]
                        if spec_help_content:
                            content.append(spec_help_content)
            else:
                content = ()
            return lcg.ContentNode('help:'+row['help_id'].value(), title=row['title'].value(),
                                   descr=row['description'].value(), foldable=True,
                                   content=lcg.Container(content),
                                   resource_provider=resource_provider,
                                   children=[make_node(r, children) for r in
                                             children.get(row['position'].value(), ())])
        #data = pytis.data.dbtable('ev_pytis_user_help',
        #                          ('help_id', 'fullname', 'spec_name', 'page_id', 'position',
        #                           'title', 'description', 'menu_help', 'content', 'language',),
        #                          config.dbconnection)
        #data.select(condition=pytis.data.EQ('language', pytis.data.sval(current_language())),
        #            sort=(('position', pytis.data.ASCENDENT),))
        data = pytis.data.dbtable('ev_pytis_user_help',
                                  ('help_id', 'fullname', 'spec_name', 'page_id', 'position',
                                   'title', 'description', 'menu_help', 'content',),
                                  config.dbconnection)
        data.select(sort=(('position', pytis.data.ASCENDENT),))
        children = {}
        while True:
            row = data.fetchone()
            if not row:
                break
            parent = '.'.join(row['position'].value().split('.')[:-1])
            children.setdefault(parent, []).append(row)
        data.close()
        nodes = [lcg.ContentNode('help:application',
                                 title=_("%s application help") % config.application_name,
                                 content=lcg.NodeIndex(), resource_provider=resource_provider,
                                 children=[make_node(r, children) for r in children['']]),
                 self._pytis_help(resource_dirs),
                 lcg.ContentNode('NotFound', title=_("Not Found"), hidden=True,
                                 content=lcg.p(_("The requested help page not found: %s",
                                                 topic)),
                                 resource_provider=resource_provider)]
        if topic.startswith('spec/'):
            # Separate specification descriptions are not in the menu generated
            # from ev_pytis_help.  If specification description page is
            # requested, we generate one and add it to the menu here.
            spec_name = topic[5:]
            title, content = self._spec_help_content(spec_name)
            if title and content:
                content = lcg.Container((lcg.TableOfContents(title=_("Contents")), content))
                node = lcg.ContentNode('help:'+topic, title=title, hidden=True, content=content,
                                       resource_provider=resource_provider)
                nodes.append(node)
        root = lcg.ContentNode('help:', content=lcg.Content(), hidden=True, children=nodes)
        return root.find_node('help:'+topic) or root.find_node('NotFound')


class HelpExporter(lcg.StyledHtmlExporter, lcg.HtmlExporter):
    _BODY_PARTS = ('menu',
                   'heading',
                   'content')

    def _uri_resource(self, context, resource):
        if resource.uri() is not None:
            return resource.uri()
        else:
            return 'resource:' +resource.filename()

    def _menu(self, context):
        g = self._generator
        tree = lcg.FoldableTree(tooltip=_("Expand/collapse complete menu hierarchy"))
        return g.div((g.h(g.a(_("Navigation"), accesskey="3"), 3),
                      tree.export(context)),
                     cls='menu-panel')

