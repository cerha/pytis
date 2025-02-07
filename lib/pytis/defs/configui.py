# -*- coding: utf-8 -*-

# Copyright (C) 2018-2025 Tomáš Cerha <t.cerha@gmail.com>
# Copyright (C) 2001-2016 OUI Technology Ltd.
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

"""Defines User Interface for modification of selected Pytis configuration options."""

from __future__ import print_function

from pytis.presentation import Field, FieldSet, VGroup, procedure
import pytis.util
from pytis.api import app

_ = pytis.util.translations('pytis-wx')

FIELDS = (
    Field('row_highlight_color', _("Default")),
    Field('row_highlight_edited_color', _("During inline editation")),
    Field('row_highlight_unfocused_color', _("Inactive form")),
    Field('row_highlight_width', _("Border width"), width=2, slider=True),
    Field('cell_highlight_color', _("Current cell highlight")),
    Field('grid_line_color', _("Table grid")),
    Field('grouping_background_downgrade', _("Grouping background change")),
    Field('show_splash', _("Show start-up dialog")),
    Field('show_tooltips', _("Show tooltips")),
    Field('auto_menu_accel', _("Automatically numbered menus with accellerator keys")),
    Field('stretch_tables', _("Automatically stretch tables to full window width")),
    Field('sender_address', _("E-mail address"), width=45),
    Field('export_encoding', _("Character encoding")),
)

def edit_config(title, layout):
    """Open a form for modification of given set of Pytis configuration options.

    Configuration options are updated on form submission.

    """
    def descr(name):
        option = pytis.config.option(name)
        descr = option.description()
        doc = option.documentation()
        if doc:
            descr += "\n" + doc
        return descr

    options = layout.order()
    fdict = {f.id(): f for f in FIELDS}
    fields = [
        fdict[option].clone(Field(option, descr=descr(option),
                                  type=pytis.config.option(option).type()))
        for option in options
    ]
    prefill = dict((o, getattr(pytis.config, o)) for o in options)
    row = app.input_form(title, fields=fields, layout=layout, prefill=prefill)
    if row:
        for option in row.keys():
            setattr(pytis.config, option, row[option].value())
        app.refresh()
        import wx
        wx.ToolTip.Enable(pytis.config.show_tooltips)


@procedure
def ui_settings():
    """Edit configuration options related to user interface.

    BEWARE: This procedure is referred from DMP identifiers of existing applications:
    'proc/export_settings/configui/',

    """
    return edit_config(_("User interface settings"), VGroup(
        FieldSet(_("Colors"), (
            FieldSet(_("Current table row highlight"), (
                'row_highlight_color',
                'row_highlight_edited_color',
                'row_highlight_unfocused_color',
                'row_highlight_width',
            )),
            'cell_highlight_color',
            'grid_line_color',
            'grouping_background_downgrade',
        )),
        FieldSet(_("Behavior"), (
            'stretch_tables',
            'show_tooltips',
            'auto_menu_accel',
            'show_splash',
        )),
        FieldSet(_("Other"), (
            'sender_address',
        )),
    ))


@procedure
def export_settings():
    """Edit configuration options related to export.

    BEWARE: This procedure is referred from DMP identifiers of existing applications:
    'proc/export_settings/configui/',

    """
    return edit_config(_("Export settings"), VGroup('export_encoding'))
