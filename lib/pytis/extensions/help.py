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
        self._menu_help_data = pd.dbtable('e_pytis_menu_help', ('menuid', 'content'),
                                                  config.dbconnection)

    def _save_menu_help(self, menuid, content):
        data = self._menu_help_data
        key = (pd.ival(menuid),)
        row = data.row(key)
        if row:
            data.update(key, pd.Row((('content', pd.sval(content)),)))
        else:
            data.insert(pd.Row((('menuid', pd.ival(menuid)), ('content', pd.sval(content)))))

    def _generate_menu_item_help(self, shortname, fullname, title):
        kind, specname = shortname.split('/', 1)
        if kind == 'menu':
            return None
        elif kind == 'form':
            form_class = fullname.split('/')[1]
            if form_class.endswith('.ConfigForm'):
                return self._generate_config_help(specname)
            elif '::' in specname:
                mainname, sidename = specname.split('::')
                return "= %s =\n\n%s\n\n= %s =\n\n%s" % (
                    _(u"Hlavní formulář"),
                    self._generate_form_help('mainform', mainname),
                    _(u"Vedlejší formulář"),
                    self._generate_form_help('sideform', sidename))
            else:
                return self._generate_form_help(form_class, specname)
        elif kind == 'proc':
            procname, specname = specname.strip('/').split('/')
            return self._generate_proc_help(procname, specname)
        elif kind == 'handle':
            action = specname.strip('/').split('/')[0]
            return self._generate_action_help(action)
        elif kind == 'NEW_RECORD':
            return self._generate_new_record_help(specname)
        elif kind == 'RELOAD_RIGHTS':
            return self._generate_reload_rights_help()
        elif kind == 'RUN_FORM':
            return self._generate_run_form_help(specname)
        elif kind == 'EXIT':
            return self._generate_exit_help()
        else:
            print "Ignoring menu item of unknown type:", shortname, fullname, title

    def _generate_config_help(self, name):
        if name == 'ui':
            return _(u"Vyvolá formulář pro přizpůsobení uživatelského rozhraní aplikace.")
        if name == 'export':
            return _(u"Vyvolá formulář uživatelského nastavení exportu dat.")
        else:
            return None

    def _generate_form_help(self, form_class, specname):
        resolver = pytis.util.resolver()
        try:
            view_spec = resolver.get(specname, 'view_spec')
        except pytis.util.ResolverError as e:
            print e
            return _(u"Neznámá specifikace %s.") % specname
        #content = [lcg.Section("Základní informace",
        #                       lcg.fieldset(self._info(), formatted=True))]
        filename = os.path.join(self._diretory, 'descr', specname + '.cs.txt')
        if os.path.exists(filename):
            content = '\n'.join([line for line in open(filename).read().splitlines()
                                 if not line.startswith('#')]).strip()
        else:
            content = (view_spec.help() or '').strip()
        related_specnames = []
        for f in view_spec.fields():
            for name in [f.codebook()] + [link.name() for link in f.links()]:
                if name and name not in related_specnames:
                    related_specnames.append(name)
        sections = [
            (_(u"Akce kontextového menu"),
             ["%s\n  %s\n\n" % (a.title(), a.descr() or '')
              for a in view_spec.actions(linear=True)] or [_(u"Žádné")]),
            (_(u"Políčka formuláře"),
             ["%s\n  %s\n\n" % (f.label(), f.descr() or '')
              for f in [view_spec.field(fid) for fid in view_spec.layout().order()]] or [_(u"Žádná")]),
            (_(u"Vedlejší formuláře"),
             ["[help:%s %s]\n  %s\n\n" % (b.name(), b.title(), b.descr() or '')
              for b in view_spec.bindings()]),
            (_(u"Související náhledy"),
             ["* [help:%s]\n" % name for name in related_specnames]),
            ]
        for title, items in sections:
            content += '\n\n== %s ==\n\n%s' % (title, ''.join(items) or _(u"Žádné"))
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
        return content

    def _generate_new_record_help(self, specname):
        return _(u"Vyvolá formulář pro vložení nového záznamu do náhledu [help:%s].") % specname
    
    def _generate_proc_help(self, procname, specname):
        return _(u"Vyvolá proceduru %s nad náhledem [help:%s].") % (procname, specname)

    def _generate_action_help(self, action):
        return _(u"Vyvolá funkci %s.") % action

    def _generate_reload_rights_help(self):
        return _(u"Vyvolá přenačtení práv.")

    def _generate_run_form_help(self, formname):
        return _(u"Vyvolá formulář %s.") % formname

    def _generate_exit_help(self):
        return _(u"Ukončí běh aplikace.")

    def create_menu_help(self):
        """Generate help pages for all menu items."""
        data = pd.dbtable('ev_pytis_menu', ('menuid', 'title', 'shortname', 'fullname', 'position'),
                                  config.dbconnection)
        data.select(sort=(('position', pd.ASCENDENT),))
        while True:
            row = data.fetchone()
            if row is None:
                break
            if row['shortname'].value():
                content = self._generate_menu_item_help(row['shortname'].value(), row['fullname'].value(), row['title'].value())
                self._save_menu_help(row['menuid'].value(), content)



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
    
