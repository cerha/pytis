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

"""T��dy a funkce pou��van� p�i generov�n� n�pov�dy k Pytis aplikaci.

K vytv��en� n�pov�dy je vyu��v�no LCG.  N�e definovan� t��dy automaticky
vytv��ej� strukturu LCG dokument� odpov�daj�c� specifikaci aktu�ln� aplikace.
P�ed pou�it�m je t�eba inicializovat konfiguraci Pytisu.

T��dy 'MenuIndex' a 'DescrIndex', mohou b�t pou�ity v r�mci n�pov�dy aplikace
formou Pythonov�ch zdrojov�ch soubor�, kter� LCG automaticky detekuje a vyu�ije
k dynamick�mu vytvo�en� v�tve v hierarchii dokument�.  V�ce viz dokumentace
LCG.

""" 

import sys, os, lcg, pytis.form, pytis.util

global _used_defs, _menu_items
_used_defs = []
_menu_items = {}
        
def _fieldset(pairs, title=None):
    """Vra� instanci 'lcg.FieldSet' sestavenou ze seznamu dvojic �et�zc�."""
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
    """Str�nka s popisem koncov� polo�ky menu."""
    
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
        return _fieldset(info)


class MenuNode(ItemNode):
    """Str�nka s popisem polo�ky menu, kter� obsahuje podmenu.

    Polo�ka obsahuj�c� podmenu sama o sob� nevyvol�v� ��dn� p��kaz, tak�e
    str�nka obsahuje pouze seznam odkaz� na jednotliv� polo�ky podmenu.
    
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
    """Ko�enov� str�nka hierarchie menu aplikace.

    Tato str�nka n�pov�dy vytvo�� hierarchii str�nek popisuj�c�ch jednotliv�
    polo�ky menu aktu�ln� aplikace.

    """
    def __init__(self, parent, id, *args, **kwargs):
        pytis.util.set_resolver(pytis.util.FileResolver('../defs'))
        if '..' not in sys.path:
            sys.path.append('..')
        if kwargs.has_key('input_encoding'):
            del kwargs['input_encoding']
        menu = pytis.util.resolver().get('application', 'menu')
        item = pytis.form.Menu(_("P�ehled menu"), menu)
        super(MenuIndex, self).__init__(parent, id, item, *args, **kwargs)

    
################################################################################

        
class DescrNode(lcg.ContentNode, lcg.FileNodeMixin):
    """Str�nka s popisem n�hledu."""

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
            menu_items = _("��dn�")
        return ((_("N�zev specifikace"), self._name),
                (_("Menu"), menu_items))
        
    def _create_content(self):
        content = [lcg.Section("Z�kladn� informace",
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
               _("Popis nen� k dispozici.")


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
            ("Akce kontextov�ho menu", actions, lcg.DefinitionList),
            ("Pol��ka formul��e",      fields,  _fieldset),
            ("Souvisej�c� n�hledy",    links,   lcg.ItemizedList)):
            c = items and f(items) or lcg.TextContent("��dn�")
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
        content.append(lcg.Section("P��stupov� pr�va", _fieldset(perms)))
        return content
    

class DualDescrNode(DescrNode):
    """Str�nka s popisem du�ln�ho n�hledu."""

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
    

class DescrIndex(lcg.ContentNode, lcg.FileNodeMixin):
    """Ko�enov� str�nka popisu pou�it�ch n�hled� aplikace.

    Tato str�nka n�pov�dy vytvo�� jednotliv� pod��zen� str�nky s popisem v�ech
    pou�it�ch n�hled� aktu�ln� aplikace.  Seznam pou�it�ch n�hled� je vytv��en
    p�i konstrukci n�pov�dy hierarchie menu (t��da 'MenuIndex').  Vytvo�en�
    n�pov�dy k menu je�t� p�ed vytv��en�m t�to str�nky je tedy nezbytnou
    podm�nkou funk�nosti t�to t��dy.

    """

    def __init__(self, parent, id, subdir=None, input_encoding=None, **kwargs):
        lcg.FileNodeMixin._init(self, parent, subdir=subdir,
                                input_encoding=input_encoding)
        super_ = super(DescrIndex, self).__init__
        super_(parent, id, title="N�pov�da k jednotliv�m n�hled�m",
               content=lcg.NodeIndex(depth=1), **kwargs)

    def _create_children(self):
        #_split_menu_descriptions(self._read_file('descr'), 'src/descr')
        global _used_defs
        _used_defs.sort()
        cls = lambda name: name.find('::') != -1 \
              and DualDescrNode or SingleDescrNode
        return [cls(name)(self, name, subdir='descr') for name in _used_defs]

