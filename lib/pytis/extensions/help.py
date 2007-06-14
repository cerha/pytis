# -*- coding: iso-8859-2 -*-

# Copyright (C) 2002, 2003, 2005, 2006, 2007 Brailcom, o.p.s.
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

global _used_defs, _menu_items
_used_defs = []
_menu_items = {}
        

def _refered_names(view):
    names = [f.codebook() for f in view.fields() if f.codebook()]
    for  f in view.fields():
        names.extend([link.name() for link in f.links()])
    return tuple(pytis.util.remove_duplicates(names))
    


class _MenuItemReader(lcg.Reader):
    """Generate an LCG document from a Pytis menu item."""
    
    def __init__(self, id, item, **kwargs):
        self._name = id
        self._item = item
        super(_MenuItemReader, self).__init__(id, title=item.title(), **kwargs)

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

    def _create_content(self):
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
                def get(name, spec='view_spec'):
                    return pytis.util.resolver().get(name, spec)
                if issubclass(form, pytis.form.DualForm) and \
                       not issubclass(form, pytis.form.DescriptiveDualForm):
                    main, side = name.split('::')
                    names = (name, main, side) + \
                          _refered_names(get(main)) + _refered_names(get(side))
                else:
                    names = (name,) + _refered_names(get(name))
                global _used_defs, _menu_items
                for n in names:
                    if n not in _used_defs:
                        _used_defs.append(n)
                if _menu_items.has_key(name):
                    _menu_items[name].append(self)
                else:
                    _menu_items[name] = [self]
                info.append(("N�hled", "[%s] (%s)" % (name, name)))
        else:
            a = ', '.join(['%s=%r' % x for x in args.items()])
            info.append(("Argumenty p��kazu", a or _("��dn�")))
        return lcg.fieldset(info, formatted=True)


class _MenuReader(_MenuItemReader):
    """Generate an LCG document from a submenu within a menu hierarchy."""
    def _create_content(self):
        return lcg.NodeIndex(depth=99)

    def _create_children(self):
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

    
################################################################################

        
class _DescrReader(lcg.StructuredTextReader):
    """Generates a description of a one view within a pytis application."""

    def __init__(self, id, **kwargs):
        self._read_spec(pytis.util.resolver(), id)
        global _menu_items
        if _menu_items.has_key(id):
            items = [i.menu_path_title() for i in _menu_items[id]]
            descr = ", ".join(items)
        else:
            descr = None
        self._name = id
        super(_DescrReader, self).__init__(id, title=self._title(), descr=descr, **kwargs)

    def _read_spec(self, resolver, name):
        self._view_spec = resolver.get(name, 'view_spec')
        self._data_spec = resolver.get(name, 'data_spec')

    def _title(self):
        return self._view_spec.title()
    
    def _info(self):
        # Create a list of relevant menu items
        global _menu_items
        if _menu_items.has_key(self._name):
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
        return self._view_spec.help() or self._view_spec.description() or \
               _("Popis nen� k dispozici.")
    
    def _create_content(self):
        content = [lcg.Section("Z�kladn� informace",
                               lcg.fieldset(self._info(), formatted=True))]
        if os.path.exists(self._input_file(self._name, lang=self._language, ext='txt')):
            text = self._read_file(self._name, lang=self._language, ext='txt')
            descr = self._parse_source_text(text)
        else:
            # The file does not exist.  Let's read the specification.
            descr = lcg.p(self._default_description_text(), formatted=True)
        content.append(lcg.Section("Popis", descr))
        return content
            


class _SingleDescrReader(_DescrReader):

    def _create_content(self):
        content = super(_SingleDescrReader, self)._create_content()
        view = self._view_spec
        actions = [(a.title(), a.descr() or '') for a in view.actions(linear=True)]
        fields = [(f.label(), f.descr() or "")  for f in
                  [view.field(cid) for cid in view.layout().order()]]
        links = [lcg.WikiText("[%s]" % name) for name in _refered_names(view)]
        for (title, items, f) in (
            ("Akce kontextov�ho menu", actions, lcg.dl),
            ("Pol��ka formul��e",      fields,  lcg.fieldset),
            ("Souvisej�c� n�hledy",    links,   lcg.ul)):
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
        self._binding = resolver.get(main, 'binding_spec')[side]

    def _title(self):
        return self._binding.title() or \
               ' / '.join((self._main_spec.title(), self._side_spec.title()))

    def _info(self):
        main, side = self._name.split('::')
        return super(_DualDescrReader, self)._info() + \
               ((_("Horn� formul��"), "[%s]" % main),
                (_("Doln� formul��"), "[%s]" % side))
    
    def _default_description_text(self):
        def clabel(cid, view):
            c = view.field(cid)
            return c and c.column_label() or cid
        b = self._binding
        text = _("Hlavn�m formul��em tohoto du�ln�ho formul��e je '%s'.  "
                 "V doln� ��sti ze zobrazuj� souvisej�c� z�znamy formul��e "
                 "'%s'.  Oba formul��e jsou propojeny p�es shodu hodnot "
                 "sloupe�k� '%s' = '%s'.") % \
                 (self._main_spec.title(), self._side_spec.title(),
                  clabel(b.binding_column(), self._main_spec),
                  clabel(b.side_binding_column(), self._side_spec))
        return text
    

class DescrReader(lcg.FileReader):
    """Root generator of Pytis view descriptions.

    This reader generates LCG documents with descriptions of all views used within the current
    Pytis application.  The list of used views is generated dynamically according to the
    application main menu.  The menu definitions are analyzed by the 'MenuReader' class defined
    above and it is necessary to include the menu reader within the document structure prior to
    including this class.

    """
    def __init__(self, id, **kwargs):
        super(DescrReader, self).__init__(id, title="N�pov�da k jednotliv�m n�hled�m", **kwargs)

    def _create_content(self):
        return lcg.NodeIndex(depth=1)

    def _create_children(self):
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
    
