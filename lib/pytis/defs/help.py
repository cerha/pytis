# -*- coding: utf-8 -*-

# Copyright (C) 2009-2015 Brailcom, o.p.s.
#
# COPYRIGHT NOTICE
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
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

import pytis.data as pd, pytis.presentation as pp, pytis.util
from pytis.presentation import Specification, Field, CodebookSpec, Editable, HGroup, Profile, \
    Binding, computer
from pytis.util import nextval

class _TreeOrderLTree(pp.PrettyFoldable, pd.String):
    pass

_ = pytis.util.translations('pytis-defs')

class HelpParents(Specification):
    # Codebook of parent items for Help (to prevent recursion).
    public = True
    title = _("Parent item")
    table = 'ev_pytis_help'
    def fields(self): return (
        Field('page_id'),
        Field('position'),
        Field('title', _("Title")),
        )
    sorting = ('position', pd.ASCENDENT),
    columns = ('title',)
    cb = CodebookSpec(display='title', prefer_display=True)

class Help(Specification):
    public = True
    table = 'ev_pytis_help'
    title = _("Help")
    def fields(self):
        return (
            Field('help_id',
                  # The computer is only used for help pages (, editable=pp.Editable.NEVER, editable=pp.Editable.NEVER, editable=pp.Editable.NEVER, editable=pp.Editable.NEVER, editable=Editable.NEVERwith page_id) so
                  # we don't need to care about other kinds of help_id.  New
                  # record is always a new page.
                  computer=computer(lambda r, page_id: 'page/%d' % page_id)),
            Field('fullname', _("Fullname"), width=50, editable=Editable.NEVER),
            Field('spec_name', _("Specification Name"), width=50, editable=Editable.NEVER),
            Field('page_id', default=nextval('e_pytis_help_pages_page_id_seq')),
            Field('position'),
            Field('position_nsub'),
            Field('title', _("Title"), width=20, editable=computer(self._is_page),
                  type=_TreeOrderLTree(tree_column_id='position', subcount_column_id='position_nsub'),
                  ),
            Field('description', _("Description"), width=70, editable=computer(self._is_page),),
            Field('content', _("Content"), width=80, height=20, compact=True,
                  text_format=pp.TextFormat.LCG, attachment_storage=self._attachment_storage),
            Field('menu_help', _(u"Menu item description"), width=80, height=20, compact=True,
                  text_format=pp.TextFormat.LCG, attachment_storage=self._attachment_storage),
            Field('spec_description', _("Brief form description"), width=80, height=3, compact=True),
            Field('spec_help', _("Detailed form help"), width=80, height=20, compact=True,
                  text_format=pp.TextFormat.LCG, attachment_storage=self._attachment_storage),
            Field('parent', _("Parent item"), not_null=False,
                  codebook='help.HelpParents', value_column='page_id',
                  editable=computer(self._is_page),
                  runtime_filter=computer(self._parent_filter),
                  descr=_("Choose the directly superordinate item in menu hierarchy. Leave "
                          "empty for pages at the top level menu.")),
            Field('ord', _("Ordering"), width=8, fixed=True, type=pd.Integer, maximum=999998,
                  editable=computer(self._is_page),
                  descr=_("Enter a number denoting the order of the item in menu between "
                          "pages of the same hierarchy level.  Leave empty to put the item "
                          "automatically to bottom.")),
            Field('removed', _("Removed"), editable=Editable.NEVER),
            Field('changed', _("Changed"), editable=Editable.NEVER),
            )
    def row_style(self, row):
        return not row['changed'].value() and pp.Style(background='#ffd') or None

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
        return pp.DbAttachmentStorage(table, ref, record[ref].value())

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
            Binding('content', _("Content"), uri=lambda r: 'help:'+r['help_id'].value()),
            Binding('fields', _("Fields"), 'help.FieldItemsHelp', 'spec_name'),
            Binding('profiles', _("Profiles"), 'help.ProfileItemsHelp', 'spec_name'),
            Binding('actions', _("Actions"), 'help.ActionItemsHelp', 'spec_name'),
            Binding('bindings', _("Side Forms"), 'help.BindingItemsHelp', 'spec_name'),
            )

    layout = ('title', 'description', 'parent', 'ord', 'content')
    cb = CodebookSpec(display='title')
    columns = ('title', 'description', 'spec_name', 'changed', 'removed')
    sorting = ('position', pd.ASCENDENT),
    profiles = pp.Profiles((pp.Profile('active', _("Active"),
                                       filter=pd.EQ('removed', pd.bval(False)),
                                       columns=('title', 'description', 'spec_name', 'removed')),),
                           default='active')


class SpecHelp(Help):
    def fields(self):
        return self._inherited_fields(SpecHelp, override=(Field('menu_help', height=2),))
    layout = ('title', 'fullname', 'menu_help', 'spec_name', 'spec_description', 'spec_help')


class MenuHelp(Help):
    layout = ('title', 'fullname', 'menu_help')


class NoHelp(Help):
    layout = ('title', pp.Text(_("This item has no editable help.")))


class ItemsHelp(Specification):
    _ITEM_KIND = None
    public = True
    table = 'e_pytis_help_spec_items'
    def fields(self):
        return (
            Field('item_id'),
            Field('kind'),
            Field('spec_name', not_null=True, codebook='help.Help', value_column='spec_name'),
            Field('identifier', _("Identifier"), width=30, editable=Editable.NEVER),
            Field('content', _("Description"), width=80, height=15, compact=True,
                  text_format=pp.TextFormat.LCG, attachment_storage=self._attachment_storage),
            Field('label', _("Title"), width=30, virtual=True, editable=pp.Editable.NEVER, computer=computer(self._label)),
            Field('removed', _("Removed"), editable=Editable.NEVER),
            Field('changed', _("Changed"), editable=Editable.NEVER,
                  computer=computer(lambda r, content: True)),
            )

    def row_style(self, row):
        return not row['changed'].value() and pp.Style(background='#ffd') or None

    def _attachment_storage(self, record):
        return pp.DbAttachmentStorage('e_pytis_help_spec_attachments', 'spec_name',
                                      record['spec_name'].value())
    def _label(self, record, spec_name, kind, identifier):
        if not kind or not identifier:
            return None
        resolver = pytis.util.resolver()
        try:
            view_spec = resolver.get(spec_name, 'view_spec')
        except pytis.util.ResolverError:
            return identifier
        kwargs = dict(unnest=True) if kind == 'action' else {}
        items = getattr(view_spec, kind+'s')(**kwargs)
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
    columns = ('identifier', 'label', 'changed', 'removed')
    layout = ('identifier', 'label', 'content')
    profiles = pp.Profiles((pp.Profile('active', _("Active"),
                                       filter=pd.EQ('removed', pd.bval(False)),
                                       columns=('identifier', 'label', 'changed')),),
                           default='active')


class FieldItemsHelp(ItemsHelp):
    _ITEM_KIND = 'field'


class BindingItemsHelp(ItemsHelp):
    _ITEM_KIND = 'binding'


class ProfileItemsHelp(ItemsHelp):
    _ITEM_KIND = 'profile'


class ActionItemsHelp(ItemsHelp):
    _ITEM_KIND = 'action'
