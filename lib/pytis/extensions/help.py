# -*- coding: iso-8859-2 -*-

# Copyright (C) 2002, 2003, 2005, 2006, 2007, 2008, 2010, 2011 Brailcom, o.p.s.
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

Help generation is implemented using the LCG Python librarry.  The classes defined below
automatically create the hierarchy of documents according to the application specification.

The classes 'MenuReader' and 'DescrReader', can be used within the help source files.  If they
define the class named 'Reader', this class is automatically used by LCG to build the document
hierarchy subtree.  The 'MenuReader' defines

So the typical usage is to include the following line in the python source file:

from pytis.extensions.help import MenuReader as Reader

See the LCG documentation for more information.

""" 

import sys, os, lcg, pytis.form, pytis.util

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
        return unicode(self._item.title(), 'iso-8859-2')

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
            key = _("Nen� definov�na")
        if command.doc():
            cmd = "%s (%s)" % (command.doc(), command.name())
        else:
            cmd = command.name()
        info = [("Kl�vesov� zkratka", key),
                ("P��kaz", cmd)]
        if command in (pytis.form.Application.COMMAND_RUN_FORM,
                       pytis.form.Application.COMMAND_NEW_RECORD):
            name = args['name']
            if command == pytis.form.Application.COMMAND_RUN_FORM:
                form =  args['form_class']
            else:
                form = pytis.form.PopupEditForm
            info.append(("Typ formul��e",
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
                info.append(("N�hled", "[%s] (%s)" % (name, name)))
                if issubclass(form, pytis.form.MultiBrowseDualForm):
                    view = pytis.util.resolver().get(name, 'view_spec')
                    bindings = []
                    for b in view.bindings():
                        _menu_items[b.name()] = [self]
                        bindings.append("[%s %s]" % (b.name(), b.title()))
                        names += (b.name(),)
                    info.append(("Postrann� formul��e", ', '.join(bindings)))
                for n in names:
                    if n not in _used_defs:
                        _used_defs.append(n)
        else:
            a = ', '.join(['%s=%r' % x for x in args.items()])
            info.append(("Argumenty p��kazu", a or _("��dn�")))
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
        pytis.util.set_resolver(pytis.util.FileResolver('../defs'))
        if '..' not in sys.path:
            sys.path.append('..')
        menu = pytis.util.resolver().get('application', 'menu')
        item = pytis.form.Menu(_("P�ehled menu"), menu)
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
                    help = lcg.p(_("Popis nen� k dispozici."))
                content = lcg.coerce((lcg.fieldset((('Titulek formul��e', title),
                                                    ('N�zev specifikace', name))),
                                      lcg.p(help)))
                if actions:
                    content = lcg.coerce((content, "*Akce kontextov�ho menu:*", lcg.dl(actions)),
                                         formatted=True)
            else:
                content = lcg.p("Nen� oby�ejn� n�hled.")
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
            menu_items = _("��dn�")
        return ((_("N�zev specifikace"), self._name),
                (_("Menu"), menu_items))
        
    def _default_description_text(self):
        help = self._view_spec.help()
        if help:
            return lcg.Container(lcg.Parser().parse(help))
        else:
            return lcg.p(_("Popis nen� k dispozici."))

    def _content(self):
        content = [lcg.Section("Z�kladn� informace",
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
            ("Akce kontextov�ho menu", actions,  lcg.dl),
            ("Pol��ka formul��e",      fields,   lcg.dl),
            ("Postrann� formul��e",    bindings, lcg.dl),
            ("Souvisej�c� n�hledy",    related,  lcg.ul)):
            c = items and f(items, formatted=True) or lcg.TextContent("��dn�")
            content.append(lcg.Section(title, c))
        if not self._data_spec.access_rights():
            return content
        rights = self._data_spec.access_rights()
        perms = [(perm, ', '.join([str(g) for g in rights.permitted_groups(perm, None)]))
                 for perm in (pytis.data.Permission.VIEW,
                              pytis.data.Permission.INSERT,
                              pytis.data.Permission.UPDATE,
                              pytis.data.Permission.DELETE,
                              pytis.data.Permission.EXPORT)]
        content.append(lcg.Section("P��stupov� pr�va", lcg.fieldset(perms, formatted=True)))
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
               ((_("Horn� formul��"), "[%s]" % main),
                (_("Doln� formul��"), "[%s]" % side))
    
    def _content(self):
        content = super(_DualDescrReader, self)._content()
        main_rights = self._main_data_spec.access_rights()
        side_rights = self._side_data_spec.access_rights()
        main_groups = main_rights.permitted_groups(pytis.data.Permission.VIEW, None)
        groups = [str(g) for g in side_rights.permitted_groups(pytis.data.Permission.VIEW, None)
                  if g in main_groups]
        content.append(lcg.Section("P��stupov� pr�va", lcg.p(', '.join(groups or '-'))))
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
            text = _("Hlavn�m formul��em tohoto du�ln�ho formul��e je [%s].  V doln� ��sti "
                     "se zobrazuj� souvisej�c� z�znamy formul��e [%s].  Oba formul��e jsou "
                     "propojeny p�es shodu hodnot sloupe�k� '%s' = '%s'.") % \
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
        return u"N�pov�da k jednotliv�m n�hled�m"

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
    
