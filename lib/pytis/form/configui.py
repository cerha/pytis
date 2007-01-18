# -*- coding: iso-8859-2 -*-

# Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2007 Brailcom, o.p.s.
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
specifikace jsou generovány automaticky při vytvoření formuláře.  Layout je
zvolen podle argumentu 'name' konstruktoru formuláře jako jedna z položek
konstanty '_LAYOUT' definované níže.  Takže výběr specifikace z resolveru,
běžný u jiných formulářů je zde nahrazen výběrem layoutu z předdefinovaných
layoutů a vygenerováním zbylých specifikačních parametrů podle vlastností
konfiguračních voleb obsažených v tomto layoutu.

"""

from pytis.form import *

_LAYOUT = (
    ('ui', LayoutSpec(_("Nastavení uživatelského rozhraní"), VGroup(
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
            'grouping_background_downgrade',
            ),
    LVGroup(_("Chování"),
            'stretch_tables',
            'show_tooltips',
            'show_splash',
            'cache_spec_onstart'),
    ))),
    ('export', LayoutSpec(_("Nastavení exportu"),
                          VGroup('export_directory','export_encoding')),
     ))

_LABELS = {'row_focus_fg_color':    _("Text"),
           'row_focus_bg_color':    _("Pozadí"),
           'row_nofocus_fg_color':  _("Text"),
           'row_nofocus_bg_color':  _("Pozadí"),
           'row_edit_fg_color':     _("Text"),
           'row_edit_bg_color':     _("Pozadí"),
           'cell_highlight_color':  _("Zvýraznění aktivní buňky"),
           'grid_line_color':       _("Mřížka tabulky"),
           'grouping_background_downgrade': _("Ztmavení řádků při seskupování"),
           'show_splash':           _("Zobrazovat úvodní dialog"),
           'show_tooltips':         _("Zobrazovat bublinovou nápovědu"),
           'stretch_tables':        _("Rozpínat tabulky na šířku okna"),
           'cache_spec_onstart':    _("Načítat specifikace při startu"),
           'export_directory':      _("Výchozí adresář"),
           'export_encoding':       _("Kódování exportovaných dat"),
}

def config_menu_items(hotkeys={}):
    """Vrať seznam položek menu pro otevření konfiguračních formulářů.

    Vrací tuple instancí 'MItem', z nichž každá otevírá jeden ze standardně
    definovaných formulářů pro editaci konfiguračních voleb.  Použitím této
    funkce v definici menu aplikace budou automaticky do menu přidávány položky
    standardních konfiguračních formulářů bez nutnosti změn v aplikaci při
    aktualizaci systému Pytis.

    """
    items = [MItem(layout.caption(),
                   command=pytis.form.Application.COMMAND_RUN_FORM,
                   args=dict(form_class=ConfigForm, name=name),
                   hotkey=hotkeys.get(name),
                   help=(_('Otevřít konfigurační formulář "%s"') % \
                         layout.caption()),
                   icon='config-'+name)
             for name, layout in _LAYOUT]
    return tuple(items)

def configurable_options():
    """Vrať seznam všech voleb nastavitelných pomocí konfiguračních formulářů.

    Vrací tuple řetězců odpovídajících názvům konfiguračních voleb.

    """
    return tuple(reduce(lambda all, layout: all + tuple(layout.order()),
                        [layout for id, layout in _LAYOUT], ()))

class _ConfigData(pytis.data.RestrictedData):
    """Falešná datová třída."""
    
    def __init__(self, columns):
        super(_ConfigData, self).__init__(columns=columns, key=columns[0])
        self._giveone = False

    def select(self, condition=None, sort=None, reuse=False):
        self._giveone = True
        return 1

    def fetchone(self, direction=pytis.data.FORWARD):
        if direction != pytis.data.FORWARD or not self._giveone:
            return None
        self._giveone = False
        row_data = [(o, pytis.data.Value(config.type(o), getattr(config, o)))
                    for o in [c.id() for c in self.columns()]]
        return pytis.data.Row(row_data)

    def update(self, key, row):
        options = [c.id() for c in self.columns()]
        for option in options:
            setattr(config, option, row[option].value())
        return row, True
        
        

class ConfigForm(PopupEditForm):
    """Formulář pro editaci konfiguračních voleb.

    Argument 'name' konstruktoru zde nemá obvyklý význam.  Slouží jako klíč do
    seznamu layoutů definovaného výše (konstanta '_LAYOUT').  Datová i
    prezentační specifikace pro tento layout jsou vytvořeny automaticky.
    
    Formulář po svém ukončení automaticky aktualizuje konfiguraci novými
    hodnotami.

    """
    DESCR = _("konfigurační formulář")
    
    def __init__(self, *args, **kwargs):
        kwargs['mode'] = self.MODE_EDIT
        super(ConfigForm, self).__init__(*args, **kwargs)
        self._init_select()
        self.select_row(0)
        
    def _layout(self):
        return dict(_LAYOUT)[self._name]

    def _create_view_spec(self, **kwargs):
        fields = [FieldSpec(option, _LABELS.get(option, option),
                            descr=config.description(option, full=True))
                  for option in self._layout().order()]
        return ViewSpec(_("Nastavení uživatelského rozhraní"),
                        fields, layout=self._layout())

    def _create_data_object(self, **kwargs):
        columns = [pytis.data.ColumnSpec(option, config.type(option))
                   for option in self._layout().order()]
        return pytis.data.DataFactory(_ConfigData, columns).create()
    
    def _create_print_menu(self):
        return None
    
    def _on_apply(self):
        self._commit_form(close=False)
        refresh()

    def _buttons(self):
        apply = {'id': wx.ID_APPLY,
                 'toottip': _("Uplatnit změny bez uzavření formuláře"),
                 'handler': lambda e: self._on_apply()}
        buttons = super(ConfigForm, self)._buttons()
        return (buttons[0], apply) + buttons[1:]
    
