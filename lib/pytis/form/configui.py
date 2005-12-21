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

"""Formul��e pro editaci konfigura�n�ch voleb v u�ivatelsk�m rozhran�.

Formul��e nejsou v�z�ny na ��dnou specifikaci.  Datov� objekt i prezenta�n�
specifikace jsou generov�ny automaticky p�i vytvo�en� formul��e.  Layout
formul��e je z�sk�v�n z konstanty '_LAYOUT' definovan� n�e.  Ta je slovn�kem
jednotliv�ch Layout�, kde kl��em je arguemnt 'name' konstruktoru formul��e.
Tak�e v�b�r specifikace z resolveru, b�n� u jin�ch formul��� je zde nahrazen
v�b�rem layoutu ze slovn�ku a vygenerov�n�m zbyl�ch specifika�n�ch parametr�
podle vlastnost� konfigura�n�ch voleb obsa�en�ch v tomto layoutu.

"""

from pytis.form import *

_LAYOUT = {
    'ui': LayoutSpec(_("Nastaven� u�ivatelsk�ho rozhran�"), VGroup(
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
            #'grouping_background_downgrade',
            ),
    #LVGroup(_("P�sma"),
    #        'edit_form_caption_font'),
    LVGroup(_("Chov�n�"),
            'show_splash', 'show_tooltips', 'cache_spec_onstart'),
    ))
    }

_LABELS = {'row_focus_fg_color':            _("Text"),
           'row_focus_bg_color':            _("Pozad�"),
           'row_nofocus_fg_color':          _("Text"),
           'row_nofocus_bg_color':          _("Pozad�"),
           'row_edit_fg_color':             _("Text"),
           'row_edit_bg_color':             _("Pozad�"),
           'cell_highlight_color':          _("Zv�razn�n� aktivn� bu�ky"),
           'grid_line_color':               _("M��ka tabulky"),
           #'grouping_background_downgrade': _("Ztmaven� seskupen�ch ��dk�"),
           #'edit_form_caption_font':        _("Font"),
           'show_splash':                   _("Zobrazovat �vodn� dialog"),
           'show_tooltips':                 _("Zobrazovat bublinovou n�pov�du"),
           'cache_spec_onstart':            _("Na��tat specifikace p�i startu"),
           }


class _MemData(pytis.data.MemData, pytis.data.RestrictedData):
    def __init__(self, bindings):
        access_rights=pytis.data.AccessRights((None, (None, pytis.data.Permission.ALL)))
        pytis.data.RestrictedData.__init__(self, access_rights)
        pytis.data.MemData.__init__(self, bindings)


class ConfigForm(PopupEditForm):
    """Formul�� pro editaci konfigura�n�ch voleb.

    Argument 'name' konstruktoru zde nem� obvykl� v�znam.  Slou�� jako kl�� do
    slovn�ku layout� definovan�ho v��e (konstanta '_LAYOUT').  Datov� i
    prezenta�n� specifikace pro tento layout jsou vytvo�eny automaticky.
    
    Formul�� po sv�m ukon�en� automaticky aktualizuje konfiguraci nov�mi
    hodnotami.

    """

    def __init__(self, *args, **kwargs):
        super_(ConfigForm).__init__(self, *args, **kwargs)
        values = [(o, pytis.data.Value(config.type(o), getattr(config, o)))
                  for o in self._layout().order()]
        self._select_row(pytis.data.Row(values))
    
    def _layout(self):
        return _LAYOUT[self._name]
    
    def _create_view_spec(self, **kwargs):
        fields = [FieldSpec(option, _LABELS.get(option, option),
                            descr=config.description(option))
                  for option in self._layout().order()]

        return ViewSpec(_("Nastaven� u�ivatelsk�ho rozhran�"),
                        fields, layout=self._layout())

    def _create_data_object(self, **kwargs):
        columns = [pytis.data.ColumnSpec(option, config.type(option))
                   for option in self._layout().order()]
        return pytis.data.DataFactory(_MemData, columns).create()

    def _commit_form(self, close=True):
        # Update konfigura�n�ch voleb po odesl�n� formul��e.
        if not self._validate_fields():
            return False
        for field in self._fields:
            option = field.id()
            setattr(config, option, self._row[option].value())
            field.init(field.get_value())
        if close:    
            self._parent.Close()
        else:
            refresh()
        return True

    def _create_additional_buttons(self):
        b = wx.Button(self, wx.ID_APPLY)
        wx_callback(wx.EVT_BUTTON, self, b.GetId(),
                    lambda e: self._commit_form(close=False))
        return (b,)
    
    def _create_print_menu(self):
        return None
