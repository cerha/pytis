# -*- coding: utf-8 -*-

# Copyright (C) 2002, 2003, 2005, 2006, 2007, 2008, 2010, 2011, 2012 Brailcom, o.p.s.
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

"""Classes used for generating help for Pytis applications.

Help texts are stored in the database and edited by help administrator.  This
module includes scripts intended to create initial help texts for new
specifications.  These texts are supposed to be further edited by help
administrator before published to be visible to application users.

""" 

import sys, os, lcg, pytis.data as pd, pytis.form, pytis.util, config

class HelpGenerator(object):
    """Generate initial help page contents for application menu items from specification.

    The initial help contents is created automatically based on the information
    available in specifications.  It is supposed to be further edited and
    maintained by help administrator.  The output should be used just as a
    skeleton for the actual man-made help pages.

    """
    def __init__(self, directory):
        self._diretory = directory
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
            print "**", key
            data.insert(pd.Row(self._values(data, **key) + rowdata))
        elif not row['changed'].value() and any([row[k].value() != v.value() for k, v in rowdata]):
            print "==", key
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
        filename = os.path.join(self._diretory, spec_name + '.cs.txt')
        if os.path.exists(filename):
            help_text = open(filename).read()
        else:
            help_text = (view_spec.help() or '').strip() or None
        self._update(self._spec_help_data, dict(spec_name=spec_name),
                     description=description, help=help_text)
        data = self._spec_help_items_data
        for kind, items in (('field', view_spec.fields()),
                            ('profile', view_spec.profiles()),
                            ('binding', view_spec.bindings()),
                            ('action', view_spec.actions(linear=True))):
            for item in items:
                self._update(data, dict(spec_name=spec_name, kind=kind, identifier=item.id()),
                             content=item.descr())
            conds = [pd.EQ('spec_name', pd.sval(spec_name)),
                     pd.EQ('kind', pd.sval(kind)),
                     pd.NOT(pd.ANY_OF('identifier', *[pd.sval(item.id()) for item in items])),
                     ]
            # Items which were modified (content changed by hand) are kept with
            # the 'removed' flag set, since the texts may be reused for other
            # items in case of identifier change or other rearrangements.  It
            # will also automatically resurrect texts for items which are
            # removed temporarily (eg. commented out) which is quite common
            # during development.  Items which were not ever modified or have
            # no content may be safely deleted (they contain no hand-edited
            # data).
            deleted = data.delete_many(pd.AND(*(conds + [pd.OR(pd.EQ('changed', pd.bval(False)),
                                                               pd.EQ('content', pd.sval(None)))])))
            updated = data.update_many(pd.AND(*(conds + [pd.EQ('removed', pd.bval(False))])),
                                       pd.Row(self._values(data, removed=True)))
            if deleted or updated:
                print "xx", spec_name, kind, deleted, updated
        
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

    def update_menu_help(self):
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
                


# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# All the following code is a relict of the previous statical help system.
# It is currently used just for reference and inspitration and will be removed soon.
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


global _used_defs, _menu_items, _refered_defs
_used_defs = []
_menu_items = {}
_refered_defs = {}
        
def _refered_names(name):
    global _refered_defs
    try:
        names = _refered_defs[name]
    except KeyError:
        spec = pytis.util.resolver().get(name, 'view_spec')
        names = [f.codebook() for f in spec.fields() if f.codebook()]
        for f in spec.fields():
            names.extend([link.name() for link in f.links()])
        names = _refered_defs[name] = tuple(pytis.util.remove_duplicates(names))
    return names
    
def _related_names(name):
    # Return related names and also all dual forms using given name
    global _refered_defs
    names = list(_refered_names(name))
    if name.find('::') == -1:
        global _used_defs
        for n in _used_defs:
            if n.startswith(name+'::') or n.endswith('::'+name):
                names.append(n)
    return names

class _MenuItemReader(lcg.Reader):
    """Generate an LCG document from a Pytis menu item."""
    
    def __init__(self, id, item, **kwargs):
        self._name = id
        self._item = item
        super(_MenuItemReader, self).__init__(id, **kwargs)

    def _title(self):
        # TODO: This is a workaround until the specifications use 8-bit strings.
        return unicode(self._item.title(), 'utf-8')

    def _menu_path(self):
        if self._parent is None or self._parent.id() == 'menu':
            return (self,)
        else:
            return self._parent._menu_path() + (self,)

    def item(self):
        return self._item
    
    def menu_path(self):
        return lcg.WikiText(' -> '.join(['[%s]' % r.id() for r in self._menu_path()]))
        
    def menu_path_title(self):
        return ' -> '.join([r.item().title() for r in self._menu_path()])

    def _content(self):
        command, args = (self._item.command(), self._item.args())
        hotkey = self._item.hotkey()
        if hotkey and hotkey[0] is not None:
            key = ' '.join(hotkey)
        else:
            key = _(u"Není definována")
        if command.doc():
            cmd = "%s (%s)" % (command.doc(), command.name())
        else:
            cmd = command.name()
        info = [("Klávesová zkratka", key),
                ("Příkaz", cmd)]
        if command in (pytis.form.Application.COMMAND_RUN_FORM,
                       pytis.form.Application.COMMAND_NEW_RECORD):
            name = args['name']
            if command == pytis.form.Application.COMMAND_RUN_FORM:
                form =  args['form_class']
            else:
                form = pytis.form.PopupEditForm
            info.append(("Typ formuláře",
                         '%s (%s)' % (form.DESCR.capitalize(), form.__name__)))
            if not issubclass(form, pytis.form.ConfigForm):
                if name.find('::') != -1:
                    main, side = name.split('::')
                    names = (name, main, side) + _refered_names(main) + _refered_names(side)
                else:
                    names = (name,) + _refered_names(name)
                global _used_defs, _menu_items
                if name in _menu_items:
                    _menu_items[name].append(self)
                else:
                    _menu_items[name] = [self]
                info.append(("Náhled", "[%s] (%s)" % (name, name)))
                if issubclass(form, pytis.form.MultiBrowseDualForm):
                    view = pytis.util.resolver().get(name, 'view_spec')
                    bindings = []
                    for b in view.bindings():
                        _menu_items[b.name()] = [self]
                        bindings.append("[%s %s]" % (b.name(), b.title()))
                        names += (b.name(),)
                    info.append(("Postranní formuláře", ', '.join(bindings)))
                for n in names:
                    if n not in _used_defs:
                        _used_defs.append(n)
        else:
            a = ', '.join(['%s=%r' % x for x in args.items()])
            info.append(("Argumenty příkazu", a or _(u"Žádné")))
        return lcg.fieldset(info, formatted=True)


class _MenuReader(_MenuItemReader):
    """Generate an LCG document from a submenu within a menu hierarchy."""
    def _content(self):
        return lcg.NodeIndex()

    def _children(self):
        children = []
        for n, item in enumerate(self._item.items()):
            if isinstance(item, (pytis.form.MItem, pytis.form.Menu)):
                if isinstance(item, pytis.form.MItem):
                    cls = _MenuItemReader
                else:
                    cls = _MenuReader
                id = '%s-%d' % (self._id, n+1)
                children.append(cls(id, item, parent=self))
        return children
    
        
class MenuReader(_MenuReader):
    """Root generator of Pytis application menu descriptions.

    This reader reads the main menu of the current pytis application and generates LCG document
    hierarchy corresponding to this menu.

    """
    def __init__(self, id, *args, **kwargs):
        if '..' not in sys.path:
            sys.path.append('..')
        menu = pytis.util.resolver().get('application', 'menu')
        item = pytis.form.Menu(_(u"Přehled menu"), menu)
        super(MenuReader, self).__init__(id, item, *args, **kwargs)


class MenuOverviewReader(MenuReader):
    """Generate menu overview as one document with sections and subsections for menu hierarchy."""

    def _create_section(self, item):
        if isinstance(item, pytis.form.Menu):
            content = [self._create_section(subitem) for subitem in item.items()
                       if not isinstance(subitem, pytis.form.MSeparator)]
        else:
            command, args = (item.command(), item.args())
            if command == pytis.form.Application.COMMAND_NEW_RECORD or \
                   command == pytis.form.Application.COMMAND_RUN_FORM \
                   and not issubclass(args['form_class'], pytis.form.ConfigForm):
                name = args['name']
                resolver = pytis.util.resolver()
                if name.find('::') != -1:
                    main, side = name.split('::')
                    binding = resolver.get(main, 'binding_spec')[side]
                    title = binding.title()
                    if not title:
                        title = ' / '.join((resolver.get(main, 'view_spec').title(),
                                            resolver.get(side, 'view_spec').title()))
                    help_src = binding.help() or binding.description()
                    actions = []
                else:
                    spec = resolver.get(name, 'view_spec')
                    help_src = spec.help()
                    title = spec.title()
                    if command == pytis.form.Application.COMMAND_RUN_FORM:
                        actions = [(a.title(), a.descr() or '') for a in spec.actions(linear=True)]
                    else:
                        actions = []
                if help_src:
                    help = lcg.Container(lcg.Parser().parse(help_src))
                else:
                    help = lcg.p(_(u"Popis není k dispozici."))
                content = lcg.coerce((lcg.fieldset((('Titulek formuláře', title),
                                                    ('Název specifikace', name))),
                                      lcg.p(help)))
                if actions:
                    content = lcg.coerce((content, "*Akce kontextového menu:*", lcg.dl(actions)),
                                         formatted=True)
            else:
                content = lcg.p("Není obyčejný náhled.")
        return lcg.Section(item.title(), content)
                
    def _content(self):
        sections = [self._create_section(menu) for menu in self._item.items()]
        return lcg.Container([lcg.TableOfContents(title="Obsah")] + sections)

    def _children(self):
        return []

        
################################################################################

        
class _DescrReader(lcg.StructuredTextReader):
    """Generates a description of one view within a pytis application."""

    def __init__(self, id, **kwargs):
        self._read_spec(pytis.util.resolver(), id)
        self._name = id
        super(_DescrReader, self).__init__(id, **kwargs)

    def _read_spec(self, resolver, name):
        self._view_spec = resolver.get(name, 'view_spec')
        self._data_spec = resolver.get(name, 'data_spec')

    def _title(self):
        return self._view_spec.title()

    def _descr(self):
        global _menu_items
        if id in _menu_items:
            return ", ".join([i.menu_path_title() for i in _menu_items[id]])
        else:
            return None
    
    def _info(self):
        # Create a list of relevant menu items
        global _menu_items
        if self._name in _menu_items:
            links = [i.menu_path() for i in _menu_items[self._name]]
            if len(links) > 1:
                menu_items = lcg.ul(links)
            else:
                menu_items = links[0]
        else:
            menu_items = _(u"Žádné")
        return ((_(u"Název specifikace"), self._name),
                (_(u"Menu"), menu_items))
        
    def _default_description_text(self):
        help = self._view_spec.help()
        if help:
            return lcg.Container(lcg.Parser().parse(help))
        else:
            return lcg.p(_(u"Popis není k dispozici."))

    def _content(self):
        content = [lcg.Section("Základní informace",
                               lcg.fieldset(self._info(), formatted=True))]
        if os.path.exists(self._input_file(self._name, lang='cs', ext='txt')):
            text = self._read_file(self._name, lang='cs', ext='txt')
            descr = self._parse_source_text(text)
        else:
            # The file does not exist.  Let's read the specification.
            descr = lcg.p(self._default_description_text(), formatted=True)
        content.append(lcg.Section("Popis", descr))
        return content
            


class _SingleDescrReader(_DescrReader):

    def _content(self):
        content = super(_SingleDescrReader, self)._content()
        view = self._view_spec
        actions = [(a.title(), a.descr() or '') for a in view.actions(linear=True)]
        fields = [(f.label(), f.descr() or '')  for f in
                  [view.field(cid) for cid in view.layout().order()]]
        related = [lcg.WikiText("[%s]" % name) for name in _related_names(self._name)]
        bindings = [(lcg.WikiText("[%s %s]" % (b.name(), b.title())), lcg.WikiText(b.descr() or ''))
                    for b in view.bindings()]
        for (title, items, f) in (
            ("Akce kontextového menu", actions,  lcg.dl),
            ("Políčka formuláře",      fields,   lcg.dl),
            ("Postranní formuláře",    bindings, lcg.dl),
            ("Související náhledy",    related,  lcg.ul)):
            c = items and f(items, formatted=True) or lcg.TextContent("Žádné")
            content.append(lcg.Section(title, c))
        if not self._data_spec.access_rights():
            return content
        rights = self._data_spec.access_rights()
        perms = [(perm, ', '.join([str(g) for g in rights.permitted_groups(perm, None)]))
                 for perm in (pd.Permission.VIEW,
                              pd.Permission.INSERT,
                              pd.Permission.UPDATE,
                              pd.Permission.DELETE,
                              pd.Permission.EXPORT)]
        content.append(lcg.Section("Přístupová práva", lcg.fieldset(perms, formatted=True)))
        return content
    

class _DualDescrReader(_DescrReader):

    def _read_spec(self, resolver, name):
        main, side = name.split('::')
        self._main_spec = resolver.get(main, 'view_spec')
        self._side_spec = resolver.get(side, 'view_spec')
        self._main_data_spec = resolver.get(main, 'data_spec')
        self._side_data_spec = resolver.get(side, 'data_spec')
        self._binding = resolver.get(main, 'binding_spec')[side]

    def _title(self):
        return self._binding.title() or \
               ' / '.join((self._main_spec.title(), self._side_spec.title()))

    def _info(self):
        main, side = self._name.split('::')
        return super(_DualDescrReader, self)._info() + \
               ((_(u"Horní formulář"), "[%s]" % main),
                (_(u"Dolní formulář"), "[%s]" % side))
    
    def _content(self):
        content = super(_DualDescrReader, self)._content()
        main_rights = self._main_data_spec.access_rights()
        side_rights = self._side_data_spec.access_rights()
        main_groups = main_rights.permitted_groups(pd.Permission.VIEW, None)
        groups = [str(g) for g in side_rights.permitted_groups(pd.Permission.VIEW, None)
                  if g in main_groups]
        content.append(lcg.Section("Přístupová práva", lcg.p(', '.join(groups or '-'))))
        return content
        
    def _default_description_text(self):
        def clabel(cid, view):
            c = view.field(cid)
            return c and c.column_label() or cid
        b = self._binding
        help = b.help() or b.description()
        if help:
            return lcg.Container(lcg.Parser().parse(help))
        else:
            main, side = self._name.split('::')
            text = _(u"Hlavním formulářem tohoto duálního formuláře je [%s].  V dolní části "
                     "se zobrazují související záznamy formuláře [%s].  Oba formuláře jsou "
                     "propojeny přes shodu hodnot sloupečků '%s' = '%s'.") % \
                     (main, side, clabel(b.binding_column(), self._main_spec),
                      clabel(b.side_binding_column(), self._side_spec))
            return lcg.p(text, formatted=True)
    

class DescrReader(lcg.FileReader):
    """Root generator of Pytis view descriptions.

    This reader generates LCG documents with descriptions of all views used within the current
    Pytis application.  The list of used views is generated dynamically according to the
    application main menu.  The menu definitions are analyzed by the 'MenuReader' class defined
    above and it is necessary to include the menu reader within the document structure prior to
    including this class.

    """

    def _title(self):
        return u"Nápověda k jednotlivým náhledům"

    def _content(self):
        return lcg.NodeIndex(depth=1)

    def _children(self):
        global _used_defs
        _used_defs.sort()
        children = []
        dir = os.path.join(self.dir(), self._id)
        for name in _used_defs:
            if name.find('::') != -1:
                cls = _DualDescrReader
            else:
                cls = _SingleDescrReader
            children.append(cls(name, parent=self, dir=dir, encoding=self.encoding()))
        import glob
        stray = [file for file in glob.glob(os.path.join(dir, '*.txt'))
                 if os.path.split(file)[1][:-4] not in _used_defs]
        if stray:
            lcg.log("Unused description files:")
            for file in stray:
                lcg.log("   "+file)
        return children

# Backwards compatibility aliases.
MenuIndex = MenuReader
DescrIndex = DescrReader
    
