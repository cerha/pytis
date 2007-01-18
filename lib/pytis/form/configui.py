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

"""Formul��e pro editaci konfigura�n�ch voleb v u�ivatelsk�m rozhran�.

Formul��e nejsou v�z�ny na ��dnou specifikaci.  Datov� objekt i prezenta�n�
specifikace jsou generov�ny automaticky p�i vytvo�en� formul��e.  Layout je
zvolen podle argumentu 'name' konstruktoru formul��e jako jedna z polo�ek
konstanty '_LAYOUT' definovan� n�e.  Tak�e v�b�r specifikace z resolveru,
b�n� u jin�ch formul��� je zde nahrazen v�b�rem layoutu z p�eddefinovan�ch
layout� a vygenerov�n�m zbyl�ch specifika�n�ch parametr� podle vlastnost�
konfigura�n�ch voleb obsa�en�ch v tomto layoutu.

"""

from pytis.form import *

_LAYOUT = (
    ('ui', LayoutSpec(_("Nastaven� u�ivatelsk�ho rozhran�"), VGroup(
    LVGroup(_("Barvy"),
            HGroup(LHGroup(_("Aktivn� ��dek"),
                           'row_focus_fg_color','row_focus_bg_color'),
                   LHGroup(_("Neaktivn� ��dek"),
                           'row_nofocus_fg_color', 'row_nofocus_bg_color'),
                   LHGroup(_("Editovan� ��dek"),
                           'row_edit_fg_color','row_edit_bg_color'),
                   ),
            'cell_highlight_color',
            'grid_line_color',
            'grouping_background_downgrade',
            ),
    LVGroup(_("Chov�n�"),
            'stretch_tables',
            'show_tooltips',
            'show_splash',
            'cache_spec_onstart'),
    ))),
    ('export', LayoutSpec(_("Nastaven� exportu"),
                          VGroup('export_directory','export_encoding')),
     ))

_LABELS = {'row_focus_fg_color':    _("Text"),
           'row_focus_bg_color':    _("Pozad�"),
           'row_nofocus_fg_color':  _("Text"),
           'row_nofocus_bg_color':  _("Pozad�"),
           'row_edit_fg_color':     _("Text"),
           'row_edit_bg_color':     _("Pozad�"),
           'cell_highlight_color':  _("Zv�razn�n� aktivn� bu�ky"),
           'grid_line_color':       _("M��ka tabulky"),
           'grouping_background_downgrade': _("Ztmaven� ��dk� p�i seskupov�n�"),
           'show_splash':           _("Zobrazovat �vodn� dialog"),
           'show_tooltips':         _("Zobrazovat bublinovou n�pov�du"),
           'stretch_tables':        _("Rozp�nat tabulky na ���ku okna"),
           'cache_spec_onstart':    _("Na��tat specifikace p�i startu"),
           'export_directory':      _("V�choz� adres��"),
           'export_encoding':       _("K�dov�n� exportovan�ch dat"),
}

def config_menu_items(hotkeys={}):
    """Vra� seznam polo�ek menu pro otev�en� konfigura�n�ch formul���.

    Vrac� tuple instanc� 'MItem', z nich� ka�d� otev�r� jeden ze standardn�
    definovan�ch formul��� pro editaci konfigura�n�ch voleb.  Pou�it�m t�to
    funkce v definici menu aplikace budou automaticky do menu p�id�v�ny polo�ky
    standardn�ch konfigura�n�ch formul��� bez nutnosti zm�n v aplikaci p�i
    aktualizaci syst�mu Pytis.

    """
    items = [MItem(layout.caption(),
                   command=pytis.form.Application.COMMAND_RUN_FORM,
                   args=dict(form_class=ConfigForm, name=name),
                   hotkey=hotkeys.get(name),
                   help=(_('Otev��t konfigura�n� formul�� "%s"') % \
                         layout.caption()),
                   icon='config-'+name)
             for name, layout in _LAYOUT]
    return tuple(items)

def configurable_options():
    """Vra� seznam v�ech voleb nastaviteln�ch pomoc� konfigura�n�ch formul���.

    Vrac� tuple �et�zc� odpov�daj�c�ch n�zv�m konfigura�n�ch voleb.

    """
    return tuple(reduce(lambda all, layout: all + tuple(layout.order()),
                        [layout for id, layout in _LAYOUT], ()))

class _ConfigData(pytis.data.RestrictedData):
    """Fale�n� datov� t��da."""
    
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
    """Formul�� pro editaci konfigura�n�ch voleb.

    Argument 'name' konstruktoru zde nem� obvykl� v�znam.  Slou�� jako kl�� do
    seznamu layout� definovan�ho v��e (konstanta '_LAYOUT').  Datov� i
    prezenta�n� specifikace pro tento layout jsou vytvo�eny automaticky.
    
    Formul�� po sv�m ukon�en� automaticky aktualizuje konfiguraci nov�mi
    hodnotami.

    """
    DESCR = _("konfigura�n� formul��")
    
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
        return ViewSpec(_("Nastaven� u�ivatelsk�ho rozhran�"),
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
                 'toottip': _("Uplatnit zm�ny bez uzav�en� formul��e"),
                 'handler': lambda e: self._on_apply()}
        buttons = super(ConfigForm, self)._buttons()
        return (buttons[0], apply) + buttons[1:]
    
