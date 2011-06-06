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
            HGroup(LVGroup(_("Aktivn� ��dek"),
                           'row_focus_fg_color','row_focus_bg_color'),
                   LVGroup(_("Neaktivn� ��dek"),
                           'row_nofocus_fg_color', 'row_nofocus_bg_color'),
                   LVGroup(_("Editovan� ��dek"),
                           'row_edit_fg_color','row_edit_bg_color'),
                   ),
            'cell_highlight_color',
            'grid_line_color',
            'grouping_background_downgrade',
            'field_disabled_color',
            'field_denied_color',
            'field_invalid_color',
            ),
    LVGroup(_("Chov�n�"),
            'stretch_tables',
            'show_tooltips',
            'auto_menu_accel',
            'show_splash',
            'cache_spec_onstart'),
    LVGroup(_("Ostatn�"),
            'sender_address'),
    ))),
    ('export', LayoutSpec(_("Nastaven� exportu"),
                          VGroup('export_directory','export_encoding')),
     ))

_LABELS = {
    'row_focus_fg_color':    _("Text"),
    'row_focus_bg_color':    _("Pozad�"),
    'row_nofocus_fg_color':  _("Text"),
    'row_nofocus_bg_color':  _("Pozad�"),
    'row_edit_fg_color':     _("Text"),
    'row_edit_bg_color':     _("Pozad�"),
    'cell_highlight_color':  _("Zv�razn�n� aktivn� bu�ky"),
    'grid_line_color':       _("M��ka tabulky"),
    'grouping_background_downgrade': _("Ztmaven� ��dk� p�i seskupov�n�"),
    'field_disabled_color':  _("Neaktivn� vstupn� pol��ko"),
    'field_denied_color':    _("Zak�zan� vstupn� pol��ko"),
    'field_invalid_color':   _("Nevalidn� vstupn� pol��ko"),
    'show_splash':           _("Zobrazovat �vodn� dialog"),
    'show_tooltips':         _("Zobrazovat bublinovou n�pov�du"),
    'auto_menu_accel':       _("Automaticky ��slovat menu akcelera�n�mi kl�vesami"),
    'stretch_tables':        _("Rozp�nat tabulky na ���ku okna"),
    'cache_spec_onstart':    _("Na��tat specifikace p�i startu"),
    'sender_address':        _("E-mailov� adresa"),
    'export_directory':      _("V�choz� adres��"),
    'export_encoding':       _("K�dov�n� exportovan�ch dat"),
}

_FIELDSPEC_KWARGS = {
    'export_directory': dict(width=45),
    'sender_address': dict(width=45),
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
    options = ()
    for id, layout in _LAYOUT:
        options = options + tuple(layout.order())
    return options

class _ConfigData(pytis.data.RestrictedData):
    """Fale�n� datov� t��da."""
    
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
    """Formul�� pro editaci konfigura�n�ch voleb.

    Argument 'name' konstruktoru zde nem� obvykl� v�znam.  Slou�� jako kl�� do
    seznamu layout� definovan�ho v��e (konstanta '_LAYOUT').  Datov� i
    prezenta�n� specifikace pro tento layout jsou vytvo�eny automaticky.
    
    Formul�� po sv�m ukon�en� automaticky aktualizuje konfiguraci nov�mi
    hodnotami.

    """
    DESCR = _("konfigura�n� formul��")
    
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
        return ViewSpec(_("Nastaven� u�ivatelsk�ho rozhran�"),
                        fields, layout=self._layout(), **self._spec_kwargs)

    def _create_data_object(self):
        columns = [pytis.data.ColumnSpec(option, config.option(option).type())
                   for option in self._layout().order()]
        return pytis.data.DataFactory(_ConfigData, columns).create()
    
    def _print_menu(self):
        return None
    
    def _buttons(self):
        button = dict(id=wx.ID_APPLY,
                      tooltip=_("Uplatnit zm�ny bez uzav�en� formul��e"),
                      command=self.COMMAND_COMMIT_RECORD(close=False))
        buttons = super(ConfigForm, self)._buttons()
        return (buttons[0], button) + buttons[1:]
    
