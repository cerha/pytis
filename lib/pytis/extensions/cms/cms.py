# -*- coding: iso-8859-2 -*-
#
# Copyright (C) 2009 Brailcom, o.p.s.
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

"""Pytis CMS specifications.

Pytis CMS is a fat client application for management of database structures, which define the
behavior of a web application.

Wiking modules which implement a Wiking application, which reads these database structures and
behaves according to them can be found in the 'web' submodule of this module.

"""

import lcg
import pytis.data as pd, pytis.presentation as pp
from pytis.presentation import Specification, FieldSpec as Field, Fields, \
     HGroup, VGroup, Binding, CodebookSpec, Computer, CbComputer, computer
from pytis.extensions import nextval, ONCE, NEVER, ALWAYS
ASC = pd.ASCENDENT
DESC = pd.DESCENDANT


class Languages(Specification):
    title = _("Jazyky")
    help = _("Spr�va jazyk� dostupn�ch v CMS.")
    table = 'cms_languages'
    def fields(self): return (
        Field('lang_id', default=nextval('cms_languages_lang_id_seq')),
        Field('lang', _("K�d"), width=2, column_width=6, fixed=True,
              filter=pp.TextFilter.ALPHANUMERIC, post_process=pp.PostProcess.LOWER),
        Field('name', _("N�zev"), virtual=True,
              computer=computer(lambda record, lang: self._language_name(lang))),
        )
    def _language_name(self, lang):
        t = lcg.GettextTranslator('cs', path=(lcg.config.translation_dir,), fallback=True)
        return t.translate(lcg.language_name(lang))
    def cb(self):
        return CodebookSpec(display=lambda lang: self._language_name(lang), prefer_display=True)
    sorting = ('lang', ASC),
    layout = ('lang',)
    columns = ('lang', 'name')

class Modules(Specification):
    title = _("Moduly")
    help = _("Spr�va roy�i�uj�c�ch modul� pou�iteln�ch ve str�nk�ch.")
    table = 'cms_modules'
    def fields(self): return (
        Field('mod_id', default=nextval('cms_modules_mod_id_seq')),
        Field('modname', _("N�zev"), width=32),
        Field('descr', _("Popis"), width=64, virtual=True,
              computer=computer(self._modtitle))
        )
    def _modtitle(self, row, modname):
        return None
    sorting = ('modname', ASC),
    cb = CodebookSpec(display='modname', prefer_display=True)
    layout = ('modname', 'descr')
    columns = ('modname', 'descr')
    

class MenuParents(Specification):
    # Codebook of parent items for Menu (to prevent recursion).
    title = _("Menu")
    table = 'cms_menu'
    def fields(self): return (
        Field('menu_id'),
        Field('menu_item_id'),
        Field('tree_order', type=pd.TreeOrder()),
        Field('lang'),
        Field('title_or_identifier', _("N�zev"), width=32),
        Field('identifier', _("Identifik�tor"), width=20),
        Field('modname', _("Modul")),
        #Field('description', _("Popis"), width=64),
        )
    sorting = ('tree_order', ASC),
    columns = ('title_or_identifier', 'identifier', 'modname')
    cb = CodebookSpec(display='title_or_identifier', prefer_display=True)
    

class Menu(Specification):
    title = _("Menu")
    help = _("Spr�va polo�ek hlavn�ho menu, jejich hierarchie a obsahu.")
    table = 'cms_menu'
    def _cbname(self, name):
        # Hack to allow namespaced spec names in wx app and plain module names in Wiking (the
        # specification is inherited by the Wiking Module).
        return 'cms.'+ name
    def _parent_filter(self, record, lang):
        return pd.EQ('lang', pd.Value(pd.String(), lang))
    def fields(self): return (
        Field('menu_id'),
        Field('menu_item_id'),
        Field('tree_order', type=pd.TreeOrder()),
        Field('identifier', _("Identifik�tor"), width=20, fixed=True, editable=ONCE,
              type=pd.RegexString(maxlen=32, not_null=True, regex='^[a-zA-Z][0-9a-zA-Z_-]*$'),
              descr=_("Identifik�tor bude vystupovat jako vn�j�� adresa str�nky.  M��e b�t pou�it "
                      "v odkazech na tuto str�nku v r�mci textu jin�ch str�nek. Platn� "
                      "identifik�tor m��e obsahovat pouze p�smena bez diakritiky, ��slice, "
                      "poml�ky a podtr��tka a mus� za��nat p�smenem.")),
        Field('lang', _("Jazyk"), editable=ONCE, codebook=self._cbname('Languages'),
              value_column='lang', selection_type=pp.SelectionType.CHOICE),
        Field('title_or_identifier', _("N�zev"), width=30),
        Field('title', _("N�zev"), width=32, not_null=True),
        Field('description', _("Popis"), width=64,
              descr=_("Stru�n� popis str�nky (zobrazen v menu jako tooltip).")),
        Field('content', _("Obsah"), type=pd.StructuredText(), compact=True, height=20, width=80,
              descr=_("Text str�nky form�tovan� jako LCG strukturovan� text (wiki)")),
        Field('mod_id', _("Modul"), not_null=False,
              codebook=self._cbname('Modules'),
              descr=_("Vyberte roz�i�uj�c� modul zobrazen� uvnit� str�nky.  Ponechte pr�zdn� pro "
                      "prostou textovou str�nku.")),
        Field('modname', _("Modul")),
        Field('parent', _("Nad��zen� polo�ka"), not_null=False,
              codebook=self._cbname('MenuParents'), value_column='menu_item_id',
              runtime_filter=computer(self._parent_filter),
              descr=_("Vyberte bezprost�edn� nad��zenou polo�ku v hierarchii menu.  Ponechte "
                      "pr�zdn� pro str�nky na nejvy��� �rovni menu.")),
        Field('published', _("Zve�ejn�no"), width=6, default=True, fixed=True, 
              descr=_("Umo��uje ��dit dostupnost dan� polo�ky nez�visle pro ka�ou jazykovou "
                      "verzi.")),
        Field('ord', _("Po�ad�"), width=8, editable=ALWAYS, fixed=True, 
              descr=_("Zadejte ��slo ur�uj�c� po�ad� polo�ky v menu (mezi str�nkami na stejn� "
                      "�rovni hierarchie.  Pokud nevypln�te, str�nka bude automaticky za�azena "
                      "na konec.")),
        )
    def row_style(self, record):
        if not record['published'].value():
            return pp.Style(foreground='#777', background='#eee')
        else:
            return None
    sorting = ('tree_order', ASC),
    layout = (VGroup('identifier', 'parent', 'mod_id', 'ord',
                     label=_("Glob�ln� vlastnosti (spole�n� pro v�echny jazyky)")),
              VGroup('lang', 'title', 'description', 'content', 'published',
                     label=_("Text str�nky (pro aktu�ln� jazykovou verzi)")))
    columns = ('title_or_identifier', 'identifier', 'modname', 'ord', 'published')
