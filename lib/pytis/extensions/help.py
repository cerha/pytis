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

"""Třídy a funkce používané při generování nápovědy k Pytis aplikaci.

K vytváření nápovědy je využíváno LCG.  Níže definované třídy automaticky
vytvářejí strukturu LCG dokumentů odpovídající specifikaci aktuální aplikace.
Před použitím je třeba inicializovat konfiguraci Pytisu.

Třídy 'MenuIndex' a 'DescrIndex', mohou být použity v rámci nápovědy aplikace
formou Pythonových zdrojových souborů, které LCG automaticky detekuje a využije
k dynamickému vytvoření větve v hierarchii dokumentů.  Více viz dokumentace
LCG.

""" 

import sys, os, lcg, pytis.form, pytis.util

global _used_defs, _menu_items
_used_defs = []
_menu_items = {}
        
def _fieldset(pairs, title=None):
    """Vrať instanci 'lcg.FieldSet' sestavenou ze seznamu dvojic řetězců."""
    fields = [lcg.Field(lcg.TextContent(l),
                        isinstance(v, lcg.Content) and v or \
                        lcg.WikiText(v)) for l,v in pairs]
    return lcg.FieldSet(fields, title=title)

def _refered_names(view):
    names = [f.codebook() for f in view.fields() if f.codebook()]
    for  f in view.fields():
        names.extend([link.name() for link in f.links()])
    return tuple(pytis.util.remove_duplicates(names))
    


class ItemNode(lcg.ContentNode):
    """Stránka s popisem koncové položky menu."""
    
    def __init__(self, parent, id, item, **kwargs):
        self._name = id
        self._item = item
        super(ItemNode, self).__init__(parent, id, title=self._item.title(),
                                       content=self._create_content(),
                                       **kwargs)

    def _menu_node_path(self):
        path = self._node_path()
        menu = pytis.util.find('menu', self._node_path(), lambda n: n.id())
        i = list(path).index(menu)
        return path[i+1:]
    
    def menu_path(self):
        path = []
        for n in self._menu_node_path():
            if path:
                path.append(lcg.TextContent(" -> "))
            path.append(lcg.Link(n))
        return lcg.Container(path)
        
    def menu_path_title(self):
        return ' -> '.join([n.title() for n in self._menu_node_path()])

    def _create_content(self):
        command, args = (self._item.command(), self._item.args())
        hotkey = self._item.hotkey()
        if hotkey and hotkey[0] is not None:
            key = ' '.join(hotkey)
        else:
            key = _("Není definována")
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
                info.append(("Náhled", "[%s] (%s)" % (name, name)))
        else:
            a = ', '.join(['%s=%r' % x for x in args.items()])
            info.append(("Argumenty příkazu", a or _("Žádné")))
        return _fieldset(info)


class MenuNode(ItemNode):
    """Stránka s popisem položky menu, která obsahuje podmenu.

    Položka obsahující podmenu sama o sobě nevyvolává žádný příkaz, takže
    stránka obsahuje pouze seznam odkazů na jednotlivé položky podmenu.
    
    """
    def _create_content(self):
        return lcg.NodeIndex(depth=99)

    def _create_children(self):
        cls = lambda i: isinstance(i, pytis.form.MItem) \
              and ItemNode or MenuNode
        return [cls(item)(self, '%s-%d' % (self._id, n+1), item)
                for n, item in enumerate(self._item.items())
                if isinstance(item, (pytis.form.MItem, pytis.form.Menu))]
    
        
class MenuIndex(MenuNode):
    """Kořenová stránka hierarchie menu aplikace.

    Tato stránka nápovědy vytvoří hierarchii stránek popisujících jednotlivé
    položky menu aktuální aplikace.

    """
    def __init__(self, parent, id, *args, **kwargs):
        pytis.util.set_resolver(pytis.util.FileResolver('../defs'))
        if '..' not in sys.path:
            sys.path.append('..')
        if kwargs.has_key('input_encoding'):
            del kwargs['input_encoding']
        menu = pytis.util.resolver().get('application', 'menu')
        item = pytis.form.Menu(_("Přehled menu"), menu)
        super(MenuIndex, self).__init__(parent, id, item, *args, **kwargs)

    
################################################################################

        
class DescrNode(lcg.ContentNode, lcg.FileNodeMixin):
    """Stránka s popisem náhledu."""

    def __init__(self, parent, id, subdir=None, input_encoding=None, **kwargs):
        self._read_spec(pytis.util.resolver(), id)
        global _menu_items
        if _menu_items.has_key(id):
            items = [i.menu_path_title() for i in _menu_items[id]]
            descr = ", ".join(items)
        else:
            descr = None
        lcg.FileNodeMixin._init(self, parent, subdir=subdir,
                                input_encoding=input_encoding)
        self._name = id
        super_ = super(DescrNode, self).__init__
        super_(parent, id, title=self._title(), descr=descr,
               content=self._create_content(), **kwargs)

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
                menu_items = lcg.ItemizedList(links)
            else:
                menu_items = links[0]
        else:
            menu_items = _("Žádné")
        return ((_("Název specifikace"), self._name),
                (_("Menu"), menu_items))
        
    def _create_content(self):
        content = [lcg.Section("Základní informace",
                               _fieldset(self._info()))]
        if os.path.exists(self._input_file(self._name, ext='txt')):
            descr = self.parse_wiki_file(self._name, ext='txt')
        else:
            # The file does not exist.  Let's read the specification.
            text = lcg.WikiText(self._default_description_text())
            descr = lcg.Paragraph(text)
        content.append(lcg.Section("Popis", descr))
        return content
            
    def _default_description_text(self):
        return self._view_spec.help() or self._view_spec.description() or \
               _("Popis není k dispozici.")


class SingleDescrNode(DescrNode):

    def _create_content(self):
        content = super(SingleDescrNode, self)._create_content()
        view = self._view_spec
        actions = [lcg.Definition(lcg.TextContent(a.title()),
                                  lcg.WikiText(a.descr() or ''))
                   for a in view.actions(linear=True)]
        fields = [(f.label(), f.descr() or "")
                  for f in [view.field(cid) for cid in view.layout().order()]]
        links = [lcg.WikiText("[%s]" % name) for name in _refered_names(view)]
        for (title, items, f) in (
            ("Akce kontextového menu", actions, lcg.DefinitionList),
            ("Políčka formuláře",      fields,  _fieldset),
            ("Související náhledy",    links,   lcg.ItemizedList)):
            c = items and f(items) or lcg.TextContent("Žádné")
            content.append(lcg.Section(title, c))
        if not self._data_spec.access_rights():
            return content
        rights = self._data_spec.access_rights()
        perms = [(perm, ', '.join([str(g) for g in
                                   rights.permitted_groups(perm, None)]))
                 for perm in (pytis.data.Permission.VIEW,
                              pytis.data.Permission.INSERT,
                              pytis.data.Permission.UPDATE,
                              pytis.data.Permission.DELETE,
                              pytis.data.Permission.EXPORT)]
        content.append(lcg.Section("Přístupová práva", _fieldset(perms)))
        return content
    

class DualDescrNode(DescrNode):
    """Stránka s popisem duálního náhledu."""

    def _read_spec(self, resolver, name):
        main, side = name.split('::')
        self._main_spec = resolver.get(main, 'view_spec')
        self._side_spec = resolver.get(side, 'view_spec')
        self._binding = resolver.get(main, 'binding_spec')[side]

    def _title(self):
        return self._binding.title()

    def _info(self):
        main, side = self._name.split('::')
        return super(DualDescrNode, self)._info() + \
               ((_("Horní formulář"), "[%s]" % main),
                (_("Dolní formulář"), "[%s]" % side))
    
    def _default_description_text(self):
        def clabel(cid, view):
            c = view.field(cid)
            return c and c.column_label() or cid
        b = self._binding
        text = _("Hlavním formulářem tohoto duálního formuláře je '%s'.  "
                 "V dolní části ze zobrazují související záznamy formuláře "
                 "'%s'.  Oba formuláře jsou propojeny přes shodu hodnot "
                 "sloupečků '%s' = '%s'.") % \
                 (self._main_spec.title(), self._side_spec.title(),
                  clabel(b.binding_column(), self._main_spec),
                  clabel(b.side_binding_column(), self._side_spec))
        return text
    

class DescrIndex(lcg.ContentNode, lcg.FileNodeMixin):
    """Kořenová stránka popisu použitých náhledů aplikace.

    Tato stránka nápovědy vytvoří jednotlivé podřízené stránky s popisem všech
    použitých náhledů aktuální aplikace.  Seznam použitých náhledů je vytvářen
    při konstrukci nápovědy hierarchie menu (třída 'MenuIndex').  Vytvoření
    nápovědy k menu ještě před vytvářením této stránky je tedy nezbytnou
    podmínkou funkčnosti této třídy.

    """

    def __init__(self, parent, id, subdir=None, input_encoding=None, **kwargs):
        lcg.FileNodeMixin._init(self, parent, subdir=subdir,
                                input_encoding=input_encoding)
        super_ = super(DescrIndex, self).__init__
        super_(parent, id, title="Nápověda k jednotlivým náhledům",
               content=lcg.NodeIndex(depth=1), **kwargs)

    def _create_children(self):
        #_split_menu_descriptions(self._read_file('descr'), 'src/descr')
        global _used_defs
        _used_defs.sort()
        cls = lambda name: name.find('::') != -1 \
              and DualDescrNode or SingleDescrNode
        return [cls(name)(self, name, subdir='descr') for name in _used_defs]

