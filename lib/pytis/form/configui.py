# -*- coding: iso-8859-2 -*-

# Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011 Brailcom, o.p.s.
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
            HGroup(LVGroup(_("Aktivní řádek"),
                           'row_focus_fg_color','row_focus_bg_color'),
                   LVGroup(_("Neaktivní řádek"),
                           'row_nofocus_fg_color', 'row_nofocus_bg_color'),
                   LVGroup(_("Editovaný řádek"),
                           'row_edit_fg_color','row_edit_bg_color'),
                   ),
            'cell_highlight_color',
            'grid_line_color',
            'grouping_background_downgrade',
            'field_disabled_color',
            'field_denied_color',
            'field_invalid_color',
            ),
    LVGroup(_("Chování"),
            'stretch_tables',
            'show_tooltips',
            'auto_menu_accel',
            'show_splash',
            'cache_spec_onstart'),
    LVGroup(_("Ostatní"),
            'sender_address'),
    ))),
    ('export', LayoutSpec(_("Nastavení exportu"),
                          VGroup('export_directory','export_encoding')),
     ))

_LABELS = {
    'row_focus_fg_color':    _("Text"),
    'row_focus_bg_color':    _("Pozadí"),
    'row_nofocus_fg_color':  _("Text"),
    'row_nofocus_bg_color':  _("Pozadí"),
    'row_edit_fg_color':     _("Text"),
    'row_edit_bg_color':     _("Pozadí"),
    'cell_highlight_color':  _("Zvýraznění aktivní buňky"),
    'grid_line_color':       _("Mřížka tabulky"),
    'grouping_background_downgrade': _("Ztmavení řádků při seskupování"),
    'field_disabled_color':  _("Neaktivní vstupní políčko"),
    'field_denied_color':    _("Zakázané vstupní políčko"),
    'field_invalid_color':   _("Nevalidní vstupní políčko"),
    'show_splash':           _("Zobrazovat úvodní dialog"),
    'show_tooltips':         _("Zobrazovat bublinovou nápovědu"),
    'auto_menu_accel':       _("Automaticky číslovat menu akceleračními klávesami"),
    'stretch_tables':        _("Rozpínat tabulky na šířku okna"),
    'cache_spec_onstart':    _("Načítat specifikace při startu"),
    'sender_address':        _("E-mailová adresa"),
    'export_directory':      _("Výchozí adresář"),
    'export_encoding':       _("Kódování exportovaných dat"),
}

_FIELDSPEC_KWARGS = {
    'export_directory': dict(width=45),
    'sender_address': dict(width=45),
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
    options = ()
    for id, layout in _LAYOUT:
        options = options + tuple(layout.order())
    return options

class _ConfigData(pytis.data.RestrictedData):
    """Falešná datová třída."""
    
    def __init__(self, columns, **kwargs):
        super(_ConfigData, self).__init__(columns=columns, key=columns[0], **kwargs)
        self._giveone = False

    def select(self, *args, **kwargs):
        self._giveone = True
        return 1

    def fetchone(self, direction=pytis.data.FORWARD, transaction=None):
        if direction != pytis.data.FORWARD or not self._giveone:
            return None
        self._giveone = False
        row_data = [(o, pytis.data.Value(config.option(o).type(), getattr(config, o)))
                    for o in [c.id() for c in self.columns()]]
        return pytis.data.Row(row_data)

    def update(self, key, row, transaction=None):
        for option in row.keys():
            setattr(config, option, row[option].value())
        wx.ToolTip.Enable(config.show_tooltips)
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
        super(ConfigForm, self).__init__(*args, **dict(kwargs, mode=self.MODE_EDIT, select_row=0))
        
    def _layout(self):
        return dict(_LAYOUT)[self._name]

    def _create_view_spec(self):
        def descr(name):
            option = config.option(name)
            descr = option.description()
            doc = option.documentation()
            if doc:
                descr += "\n" + doc
            return descr
        fields = [Field(option, _LABELS.get(option, option), descr=descr(option),
                        **_FIELDSPEC_KWARGS.get(option, {}))
                  for option in self._layout().order()]
        return ViewSpec(_("Nastavení uživatelského rozhraní"),
                        fields, layout=self._layout(), **self._spec_kwargs)

    def _create_data_object(self):
        columns = [pytis.data.ColumnSpec(option, config.option(option).type())
                   for option in self._layout().order()]
        return pytis.data.DataFactory(_ConfigData, columns).create()
    
    def _print_menu(self):
        return None
    
    def _buttons(self):
        button = dict(id=wx.ID_APPLY,
                      tooltip=_("Uplatnit změny bez uzavření formuláře"),
                      command=self.COMMAND_COMMIT_RECORD(close=False))
        buttons = super(ConfigForm, self)._buttons()
        return (buttons[0], button) + buttons[1:]
    
