# -*- coding: iso-8859-2 -*-

# Copyright (C) 2001, 2002, 2003, 2004, 2005 Brailcom, o.p.s.
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

"""Formuláře pro editaci konfiguračních voleb v uživatelském rozhraní.

Formuláře nejsou vázány na žádnou specifikaci.  Datový objekt i prezentační
specifikace jsou generovány automaticky při vytvoření formuláře.  Layout
formuláře je získáván z konstanty '_LAYOUT' definované níže.  Ta je slovníkem
jednotlivých Layoutů, kde klíčem je arguemnt 'name' konstruktoru formuláře.
Takže výběr specifikace z resolveru, běžný u jiných formulářů je zde nahrazen
výběrem layoutu ze slovníku a vygenerováním zbylých specifikačních parametrů
podle vlastností konfiguračních voleb obsažených v tomto layoutu.

"""

from pytis.form import *

_LAYOUT = {
    'ui': LayoutSpec(_("Nastavení uživatelského rozhraní"), VGroup(
    LVGroup(_("Barvy"),
            HGroup(LHGroup(_("Aktivní řádek"),
                           'row_focus_fg_color','row_focus_bg_color'),
                   LHGroup(_("Neaktivní řádek"),
                           'row_nofocus_fg_color', 'row_nofocus_bg_color'),
                   LHGroup(_("Editovaný řádek"),
                           'row_edit_fg_color','row_edit_bg_color'),
                   ),
            'cell_highlight_color',
            'grid_line_color',
            #'grouping_background_downgrade',
            ),
    #LVGroup(_("Písma"),
    #        'edit_form_caption_font'),
    LVGroup(_("Chování"),
            'show_splash', 'show_tooltips', 'cache_spec_onstart'),
    ))
    }

_LABELS = {'row_focus_fg_color':            _("Text"),
           'row_focus_bg_color':            _("Pozadí"),
           'row_nofocus_fg_color':          _("Text"),
           'row_nofocus_bg_color':          _("Pozadí"),
           'row_edit_fg_color':             _("Text"),
           'row_edit_bg_color':             _("Pozadí"),
           'cell_highlight_color':          _("Zvýraznění aktivní buňky"),
           'grid_line_color':               _("Mřížka tabulky"),
           #'grouping_background_downgrade': _("Ztmavení seskupených řádků"),
           #'edit_form_caption_font':        _("Font"),
           'show_splash':                   _("Zobrazovat úvodní dialog"),
           'show_tooltips':                 _("Zobrazovat bublinovou nápovědu"),
           'cache_spec_onstart':            _("Načítat specifikace při startu"),
           }


class _MemData(pytis.data.MemData, pytis.data.RestrictedData):
    def __init__(self, bindings):
        access_rights=pytis.data.AccessRights((None, (None, pytis.data.Permission.ALL)))
        pytis.data.RestrictedData.__init__(self, access_rights)
        pytis.data.MemData.__init__(self, bindings)


class ConfigForm(PopupEditForm):
    """Formulář pro editaci konfiguračních voleb.

    Argument 'name' konstruktoru zde nemá obvyklý význam.  Slouží jako klíč do
    slovníku layoutů definovaného výše (konstanta '_LAYOUT').  Datová i
    prezentační specifikace pro tento layout jsou vytvořeny automaticky.
    
    Formulář po svém ukončení automaticky aktualizuje konfiguraci novými
    hodnotami.

    """

    def _layout(self):
        return _LAYOUT[self._name]
    
    def _create_view_spec(self, **kwargs):
        def cleanup(row):
            # Update konfiguračních voleb po odeslání formuláře.
            for option in row.keys():
                setattr(config, option, row[option].value())
                
        fields = [FieldSpec(option, _LABELS.get(option, option),
                            descr=config.description(option),
                            default=lambda o=option: getattr(config, o),
                            )
                  for option in self._layout().order()]

        return ViewSpec(_("Nastavení uživatelského rozhraní"),
                        fields, layout=self._layout(), cleanup=cleanup)

    def _create_data_object(self, **kwargs):
        columns = [pytis.data.ColumnSpec(option, config.type(option))
                   for option in self._layout().order()]
        return pytis.data.DataFactory(_MemData, columns).create()

    def _create_print_menu(self):
        return None
