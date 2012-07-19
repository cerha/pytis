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

import pytis.data as pd, pytis.presentation as pp
from pytis.presentation import Specification, Field, CodebookSpec, Editable, HGroup, Profile, \
    Binding, computer
from pytis.util import lcg_to_html

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
            Field('menuid'),
            Field('page_id'),
            Field('position'),
            Field('position_nsub'),
            Field('title', _(u"Název"), width=20, editable=computer(self._is_page),
                  type=_TreeOrderLTree(tree_column_id='position', subcount_column_id='position_nsub'),
                  ),
            Field('description', _(u"Popis"), width=70, editable=computer(self._is_page),),
            Field('content', _(u"Obsah"), width=80, height=20, text_format=pp.TextFormat.LCG, 
                  compact=True, attachment_storage=self._attachment_storage),
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
            Field('fullname', _(u"Fullname"), width=50),
            )

    def _is_page(self, record, page_id):
        return record.new() or page_id is not None

    def _parent_filter(self, page_id):
        return pd.NE('page_id', pd.ival(None))

    def _attachment_storage(self, record):
        if record['page_id'].value():
            table, ref = ('e_pytis_help_pages_attachments', 'page_id')
        elif record['menuid'].value():
            table, ref = ('e_pytis_menu_help_attachments', 'menuid')
        else:
            # The attachments are not allowed for some special pages, such as the menu root page.
            return None
        return pp.DbAttachmentStorage(table, ref, record[ref].value(), base_uri='resource:')

    cb = CodebookSpec(display='title')
    columns = ('title', 'description')
    layout = ('title', 'description', 'parent', 'ord', 'content')
    sorting = ('position', pd.ASCENDENT),
    bindings = (
        Binding('content', _("Obsah"),
                content=lambda r: lcg_to_html(r['content'].value() or ''),),
        )
    
