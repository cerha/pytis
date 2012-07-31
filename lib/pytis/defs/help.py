# -*- coding: utf-8 -*-

# Copyright (C) 2009, 2010, 2011, 2012 Brailcom, o.p.s.
#
# COPYRIGHT NOTICE
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

import pytis.data as pd, pytis.presentation as pp, pytis.util
from pytis.presentation import Specification, Field, CodebookSpec, Editable, HGroup, Profile, \
    Binding, computer

class _TreeOrderLTree(pp.PrettyFoldable, pd.String):
    pass


class HelpParents(Specification):
    # Codebook of parent items for Help (to prevent recursion).
    public = True
    title = _(u"Nadřízená položka")
    table = 'ev_pytis_help'
    def fields(self): return (
        Field('page_id'),
        Field('position'),
        Field('title', _(u"Název")),
        )
    sorting = ('position', pd.ASCENDENT),
    columns = ('title',)
    cb = CodebookSpec(display='title', prefer_display=True)

class Help(Specification):
    public = True
    table = 'ev_pytis_help'
    title = _(u"Nápověda")
    def fields(self):
        return (
            Field('help_id'),
            Field('fullname', _(u"Fullname"), width=50, editable=Editable.NEVER),
            Field('spec_name', _("Název specifikace"), width=50, editable=Editable.NEVER),
            Field('page_id'),
            Field('position'),
            Field('position_nsub'),
            Field('title', _(u"Název"), width=20, editable=computer(self._is_page),
                  type=_TreeOrderLTree(tree_column_id='position', subcount_column_id='position_nsub'),
                  ),
            Field('description', _(u"Popis"), width=70, editable=computer(self._is_page),),
            Field('content', _(u"Obsah"), width=80, height=20, compact=True,
                  text_format=pp.TextFormat.LCG, attachment_storage=self._attachment_storage),
            Field('short_menu_help', _(u"Popis položky menu"), dbcolumn='menu_help',
                  width=80, height=2, compact=True, text_format=pp.TextFormat.LCG),
            Field('menu_help', _(u"Popis položky menu"), width=80, height=20, compact=True,
                  text_format=pp.TextFormat.LCG, attachment_storage=self._attachment_storage),
            Field('spec_description', _(u"Stručný popis náhledu"), width=80, height=3, compact=True),
            Field('spec_help', _(u"Podrobná nápověda náhledu"), width=80, height=20, compact=True,
                  text_format=pp.TextFormat.LCG, attachment_storage=self._attachment_storage),
            Field('parent', _("Nadřízená položka"), not_null=False,
                  codebook='help.HelpParents', value_column='page_id',
                  editable=computer(self._is_page),
                  runtime_filter=computer(self._parent_filter),
                  descr=_("Vyberte bezprostředně nadřízenou položku v hierarchii menu.  Ponechte "
                          "prázdné pro stránky na nejvyšší úrovni menu.")),
            Field('ord', _("Pořadí"), width=8, fixed=True, type=pd.Integer, maximum=999998,
                  editable=computer(self._is_page),
                  descr=_("Zadejte číslo určující pořadí položky v menu (mezi stránkami na stejné "
                          "úrovni hierarchie).  Pokud nevyplníte, stránka bude automaticky "
                          "zařazena na konec.")),
            )

    def _is_page(self, record, page_id):
        return record.new() or page_id is not None

    def _parent_filter(self, page_id):
        return pd.NE('page_id', pd.ival(None))

    def _attachment_storage(self, record):
        if record['page_id'].value():
            table, ref = ('e_pytis_help_pages_attachments', 'page_id')
        elif record['spec_name'].value():
            table, ref = ('e_pytis_help_spec_attachments', 'spec_name')
        else:
            # The attachments are not allowed for some special pages, such as the menu root page.
            return None
        return pp.DbAttachmentStorage(table, ref, record[ref].value(), base_uri='resource:')
    
    def _content(self, record):
        content = '\n\n'.join([record[f].value()
                               for f in ('menu_help', 'spec_description', 'spec_help', 'content')
                               if record[f].value() is not None])
        storage = self._attachment_storage(record)
        return pytis.util.parse_lcg_text(content, resources=storage and storage.resources() or ())
        
    def redirect(self, record):
        if record['page_id'].value() is not None:
            return None
        elif record['spec_name'].value() is not None:
            return 'help.SpecHelp'
        elif record['fullname'].value() is not None:
            return 'help.MenuHelp'
        else:
            return 'help.NoHelp'
    def bindings(self):
        return (
            Binding('content', _("Obsah"), content=self._content),
            Binding('fields', _("Políčka"), 'help.FieldItemsHelp', 'spec_name'),
            Binding('profiles', _("Profily"), 'help.ProfileItemsHelp', 'spec_name'),
            Binding('actions', _("Akce"), 'help.ActionItemsHelp', 'spec_name'),
            Binding('bindings', _("Vedlejší formuláře"), 'help.BindingItemsHelp', 'spec_name'),
            )

    layout = ('title', 'description', 'parent', 'ord', 'content')
    cb = CodebookSpec(display='title')
    columns = ('title', 'description', 'spec_name')
    sorting = ('position', pd.ASCENDENT),
    

class SpecHelp(Help):
    layout = ('title', 'fullname', 'short_menu_help', 'spec_name', 'spec_description', 'spec_help')

    
class MenuHelp(Help):
    layout = ('title', 'fullname', 'menu_help')

    
class NoHelp(Help):
    layout = ('title', pp.Text(_(u"Tato položka nemá editovatelnou nápovědu.")))


class ItemsHelp(Specification):
    _ITEM_KIND = None
    public = True
    table = 'e_pytis_help_spec_items'
    def fields(self):
        return (
            Field('item_id'),
            Field('kind'),
            Field('spec_name', codebook='help.Help', value_column='spec_name'),
            Field('identifier', _(u"Identifikátor"), width=30, editable=Editable.NEVER),
            Field('content', _(u"Popis"), width=80, height=15, compact=True,
                  text_format=pp.TextFormat.LCG, attachment_storage=self._attachment_storage),
            Field('label', _(u"Název"), width=30, virtual=True, computer=computer(self._label)),
            )
    def _attachment_storage(self, record):
        return pp.DbAttachmentStorage('e_pytis_help_spec_attachments', 'spec_name',
                                      record['spec_name'].value(), base_uri='resource:')
    def _label(self, record, spec_name, kind, identifier):
        if not kind or not identifier:
            return None
        resolver = pytis.util.resolver()
        try:
            view_spec = resolver.get(spec_name, 'view_spec')
        except pytis.util.ResolverError:
            return identifier
        items = getattr(view_spec, kind+'s')()
        item = pytis.util.find(identifier, items, key=lambda x: x.id())
        if item:
            if kind in ('field',):
                return item.label()
            elif kind in ('action', 'binding'):
                return item.title()
            else:
                return item.name()
        else:
            return identifier
        
    def condition(self):
        return pd.EQ('kind', pd.sval(self._ITEM_KIND))
    columns = ('identifier', 'label')
    layout = ('identifier', 'label', 'content')


class FieldItemsHelp(ItemsHelp):
    _ITEM_KIND = 'field'


class BindingItemsHelp(ItemsHelp):
    _ITEM_KIND = 'binding'


class ProfileItemsHelp(ItemsHelp):
    _ITEM_KIND = 'profile'


class ActionItemsHelp(ItemsHelp):
    _ITEM_KIND = 'action'
    
