# -*- coding: iso-8859-2 -*-

# Copyright (C) 2002, 2003, 2005, 2006 Brailcom, o.p.s.
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

import sys, os
import lcg, pytis.form, pytis.util

global _used_defs, _menu_items
_used_defs = []
_menu_items = {}
        
def _fieldset(parent, pairs, title=None):
    """Vrať instanci 'lcg.FieldSet' sestavenou ze seznamu dvojic řetězců."""
    fields = [lcg.Field(parent, lcg.TextContent(parent, l),
                        isinstance(v, lcg.Content) and v or \
                        lcg.WikiText(parent, v)) for l,v in pairs]
    return lcg.FieldSet(parent, fields, title=title)


class ItemNode(lcg.ContentNode):
    """Stránka s popisem koncové položky menu."""
    
    def __init__(self, parent, id, item, **kwargs):
        self._item = item
        super(ItemNode, self).__init__(parent, id, **kwargs)
        
    def _title(self):
        return self._item.title()

    def _menu_node_path(self):
        path = self._node_path()
        menu = pytis.util.find('menu', self._node_path(), lambda n: n.id())
        i = list(path).index(menu)
        return path[i+1:]
    
    def menu_path(self):
        path = []
        for n in self._menu_node_path():
            if path:
                path.append(lcg.TextContent(self, " -> "))
            path.append(lcg.Link(self, n))
        return lcg.Container(self, path)
        
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
                if issubclass(form, pytis.form.DualForm) and \
                       not issubclass(form, pytis.form.DescriptiveDualForm):
                    resolver = pytis.form.resolver()
                    dual = resolver.get(name, 'dual_spec')
                    node_name = name + '-dual'
                    names = (node_name, dual.main_name(), dual.side_name())
                else:
                    node_name = name
                    names = (name,)
                global _used_defs, _menu_items
                for n in names:
                    if n not in _used_defs:
                        _used_defs.append(n)
                if _menu_items.has_key(node_name):
                    _menu_items[node_name].append(self)
                else:
                    _menu_items[node_name] = [self]
                info.append(("Náhled", "[%s] (%s)" % (node_name, name)))
        else:
            a = ', '.join(['%s=%r' % x for x in args.items()])
            info.append(("Argumenty příkazu", a or _("Žádné")))
        return _fieldset(self, info)


class MenuNode(ItemNode):
    """Stránka s popisem položky menu, která obsahuje podmenu.

    Položka obsahující podmenu sama o sobě nevyvolává žádný příkaz, takže
    stránka obsahuje pouze seznam odkazů na jednotlivé položky podmenu.
    
    """
    def _create_content(self):
        return lcg.TableOfContents(self, depth=99)

    def _create_children(self):
        return [self._create_child(isinstance(item, pytis.form.MItem) and \
                                   ItemNode or MenuNode,
                                   '%s-%d' % (self._id, n+1), item)
                for n, item in enumerate(self._item.items())
                if isinstance(item, (pytis.form.MItem, pytis.form.Menu))]
    
        
class MenuIndex(MenuNode):
    """Kořenová stránka hierarchie menu aplikace.

    Tato stránka nápovědy vytvoří hierarchii stránek popisujících jednotlivé
    položky menu aktuální aplikace.

    """
    def __init__(self, parent, id, *args, **kwargs):
        resolver = pytis.util.FileResolver('../defs')
        pytis.form.NullApplication(resolver)
        if '..' not in sys.path:
            sys.path.append('..')
        item = pytis.form.Menu(_("Přehled menu"),
                               resolver.get('application', 'menu'))
        super(MenuIndex, self).__init__(parent, id, item, *args, **kwargs)

    
        
################################################################################

        
class DescrNode(lcg.ContentNode):
    """Stránka s popisem jednoduchého náhledu."""

    def __init__(self, parent, id, *args, **kwargs):
        resolver = pytis.form.resolver()
        self._read_spec(resolver, id)
        super(DescrNode, self).__init__(parent, id, *args, **kwargs)

    def _name(self, id):
        return id

    def _read_spec(self, resolver, id):
        self._view_spec = resolver.get(id, 'view_spec')
        self._data_spec = resolver.get(id, 'data_spec')
        
    def _title(self):
        return self._view_spec.title()

    def _descr(self):
        global _menu_items
        if _menu_items.has_key(self._id):
            items = [i.menu_path_title() for i in _menu_items[self._id]]
            return ", ".join(items)
        else:
            return None

    def output_file(self):
        return self.id().replace(':', '-') + '.html'
        
    def _info(self):
        # Create the list of relevant menu items
        global _menu_items
        if _menu_items.has_key(self._id):
            links = [i.menu_path() for i in _menu_items[self._id]]
            if len(links) > 1:
                menu_items = lcg.ItemizedList(self, links)
            else:
                menu_items = links[0]
        else:
            menu_items = _("Žádné")
        return ((_("Název specifikace"), self._name(self._id)),
                (_("Menu"), menu_items))
        
    def _create_content(self):
        content = [_fieldset(self, self._info())]
        if os.path.exists(self._input_file(self._id, ext='txt')):
            content.extend(self.parse_wiki_file(self._id, ext='txt'))
        else:
            # The file does not exist.  Let's read the specification.
            text = lcg.WikiText(self, self._default_description_text())
            content.append(lcg.Paragraph(self, text))
        content.extend(self._access_rights())
        return content
            
    def _default_description_text(self):
        return self._view_spec.help() or self._view_spec.description() or \
               _("Popis není k dispozici.")

    def _access_rights(self):
        if not self._data_spec.access_rights():
            return []
        rights = self._data_spec.access_rights()
        perms = [(perm, ', '.join([str(g) for g in
                                   rights.permitted_groups(perm, None)]))
                 for perm in (pytis.data.Permission.VIEW,
                              pytis.data.Permission.INSERT,
                              pytis.data.Permission.UPDATE,
                              pytis.data.Permission.DELETE,
                              pytis.data.Permission.EXPORT)]
        return [lcg.Section(self, "Přístupová práva",
                            _fieldset(self, perms))]

class DualDescrNode(DescrNode):
    """Stránka s popisem duálního náhledu."""

    def _name(self, id):
        assert id.endswith('-dual')
        return id[:-5]
    
    def _read_spec(self, resolver, id):
        self._dual_spec = dual = resolver.get(self._name(id), 'dual_spec')
        self._main_spec = resolver.get(dual.main_name(), 'view_spec')
        self._side_spec = resolver.get(dual.side_name(), 'view_spec')

    def _title(self):
        return self._main_spec.title() +' / '+ self._side_spec.title()

    def _info(self):
        return super(DualDescrNode, self)._info() + \
               ((_("Horní formulář"), "[%s]" % self._dual_spec.main_name()),
                (_("Dolní formulář"), "[%s]" % self._dual_spec.side_name()))
    
    def _default_description_text(self):
        def clabel(cid, view):
            c = view.field(cid)
            return c and c.column_label() or cid
        dual, main, side = (self._dual_spec, self._main_spec, self._side_spec)
        text = _("Hlavním formulářem tohoto duálního formuláře je '%s'.  "
                 "V dolní části ze zobrazují související záznamy formuláře "
                 "'%s'.  Oba formuláře jsou propojeny přes shodu hodnot "
                 "sloupečků '%s' = '%s'.") % \
                 (main.title(), side.title(),
                  clabel(dual.binding_column(), main),
                  clabel(dual.side_binding_column(), side))
        return text
    
    def _access_rights(self):
        return []
    

class DescrIndex(lcg.ContentNode):
    """Kořenová stránka popisu použitých náhledů aplikace.

    Tato stránka nápovědy vytvoří jednotlivé podřízené stránky s popisem všech
    použitých náhledů aktuální aplikace.  Seznam použitých náhledů je vytvářen
    při konstrukci nápovědy hierarchie menu (třída 'MenuIndex').  Vytvoření
    nápovědy k menu ještě před vytvářením této stránky je tedy nezbytnou
    podmínkou funkčnosti této třídy.

    """

    def _title(self):
        return "Nápověda k jednotlivým náhledům"
    
    def _create_content(self):
        return lcg.TableOfContents(self, depth=1)

    def _create_children(self):
        #_split_menu_descriptions(self._read_file('descr'), 'src/descr')
        global _used_defs
        _used_defs.sort()
        cls = lambda name: name.endswith('-dual') and DualDescrNode or DescrNode
        return [self._create_child(cls(name), name, subdir='descr')
                for name in _used_defs]


def _split_menu_descriptions(text, dir, strayfile='_stray.txt'):
    """Rozdělí dokument s popisky formulářů do jednotlivých souborů.

    Jednotlivé sekce dokumnetu musí být pojmenovány jako odpovídající položky
    menu.  Soubory budou uloženy do daného adresáře.

    Nepřiřazené položky budou zapsány do souboru 
    
    """
    global _menu_items
    menu = {}
    for name, items in _menu_items.items():
        for item in items:
            menu[item.title()] = (item, name)
    import re
    sections = re.split("(?m)^=+\s+(?:[\d\.]+\s+)?(.*)\s+=+$", text)
    modeline = '# -*- coding: utf-8; mode: structured-text -*-\n'
    stray = modeline
    for title, text in zip(sections[1::2], sections[2::2]):
        text = text.strip()
        if menu.has_key(title):
            item, name = menu[title]
            filename = fn = os.path.join(dir, name+'.txt')
            n = 0
            while os.path.exists(fn):
                n+=1
                fn = '%s.%d' % (filename, n)
            print "**", title, fn
            #fh = file(fn, 'w')
            #fh.write(modeline +"\n" + text.encode('utf-8'))
            #fh.close()
        else:
            print "--", title
            #stray += "\n== %s ==\n\n%s\n" % (title, text)
    #sfh = file(strayfile, 'w')
    #sfh.write(stray.encode('utf-8'))
    #sfh.close()

