# -*- coding: utf-8 -*-

# Copyright (C) 2018-2025 Tomáš Cerha <t.cerha@gmail.com>
# Copyright (C) 2001-2017 OUI Technology Ltd.
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

"""Práce s formuláři se seznamovým zobrazením.

Modul jednak interpretuje specifikaci formulářů (viz modul 'spec') pro
seznamové zobrazení a jednak zajišťuje práci s ní prostřednictvím objektů
wxWidgets.

"""
# Terminologická poznámka: Proměnné s názvem `row' obvykle značí číslo řádku
# (číslováno od 0).  Jedná-li se o obsah řádku, nazývá se příslušná proměnná
# obvykle `the_row'.  Matoucí jméno `row' bylo převzato z wxWidgets.

from __future__ import unicode_literals
from __future__ import print_function

from past.builtins import basestring
from builtins import range

import copy
import datetime
import functools
import io
import tempfile
import time

import wx
import wx.grid

import pytis.data
import pytis.form
import pytis.output
import pytis.util

from pytis.api import app
from pytis.presentation import (
    Action, ActionGroup, AggregatedView, CodebookSpec, Field, Editable,
    FormType, SelectionType, Link, TextFormat, ViewSpec, ActionContext,
    PrettyFoldable, Menu, MenuItem, MenuSeparator, Command
)
from pytis.util import (
    ACTION, DEBUG, EVENT, OPERATIONAL, UNDEFINED,
    Attribute, ProgramError, ResolverError, SimpleCache, Structure, find, log,
)

import pytis.remote

from .event import UserBreakException, wx_callback
from .dialog import AggregationSetupDialog, CheckListDialog
from .form import (
    BrowsableShowForm, Form, LookupForm, PopupEditForm, PopupForm,
    QueryFieldsForm, RecordForm, Refreshable, ShowForm,
)
from .screen import (
    KeyHandler, busy_cursor, make_in_operator,
    copy_to_clipboard, dlg2px, file_menu_items, get_icon, is_busy_cursor,
    microsleep, wx_button, wx_checkbox, wx_choice, wx_text_ctrl,
)
from .search import sfs_columns
from .application import Application, run_form
from .grid import TableRowIterator, GridTable


_ = pytis.util.translations('pytis-wx')

unistr = type(u'')  # Python 2/3 transition hack.


# Forms

class ListForm(RecordForm, Refreshable):
    """Společná nadtřída pro formuláře se seznamovým zobrazením.

    Tyto formuláře zobrazují seznam řádků, rozdělených do několika sloupců,
    tedy v podstatě tabulku.  Třída definuje společné vlastnosti, jako možnosti
    navigace, vyhledávání, řazení apod.

    Třída je 'CallbackHandler' a jako argument callbackové funkce předává
    slovník, jehož klíče jsou id sloupců (stringy) a hodnoty jsou hodnoty
    těchto sloupců (opět stringy) řádku, jehož se callback týká.

    Tato třída obvykle není používána přímo, nýbrž slouží jako základ pro
    specializované třídy.

    """
    CALL_ACTIVATION = 'CALL_ACTIVATION'
    """Konstanta callbacku aktivace řádku."""
    CALL_MODIFICATION = 'CALL_MODIFICATION'
    """Konstanta callbacku modifikace řádku."""

    _REFRESH_PERIOD = 60   # sekund
    _SELECTION_CALLBACK_DELAY = 3   # desítky milisekund
    _ROW_LABEL_WIDTH = 85
    _ALLOW_TOOLBAR = False
    _SINGLE_LINE = True

    _AGGREGATIONS = ((pytis.data.Data.AGG_SUM, _("Sum"), 'agg-sum', _("Sum:")),
                     (pytis.data.Data.AGG_AVG, _("Average"), 'agg-avg', _("Avg:")),
                     (pytis.data.Data.AGG_MIN, _("Minimum"), 'agg-min', _("Min:")),
                     (pytis.data.Data.AGG_MAX, _("Maximum"), 'agg-max', _("Max:")))

    DESCR = _("row form")

    def _full_init(self, *args, **kwargs):
        self._grid = None
        super(ListForm, self)._full_init(*args, **kwargs)
        # Nastav klávesové zkratky z kontextových menu.
        for action in self._view.actions(unnest=True):
            if action.hotkey():
                self.define_key(action.hotkey(), Command(self.context_action, action=action))
        # Závěrečné akce
        self._data.add_callback_on_change(self.on_data_change)
        wx_callback(wx.EVT_SIZE, self, self._on_size)
        self._select_cell(row=self._get_row_number(self._row.row()))
        self.set_callback(ListForm.CALL_ACTIVATION, self._on_activation)
        # Setting a minimal size here is a hack to avoid wx/gtk hanging when
        # the form size becomes too small.  The trigger of the hang seems to be
        # the moment, when there doesn't remain any space for scrollbar "body"
        # (the arrow buttons touch each other...).
        self.SetMinSize((80, 80))

    def _init_attributes(self, **kwargs):
        self._aggregation_results = SimpleCache(self._get_aggregation_result)
        super(ListForm, self)._init_attributes(**kwargs)
        self._fields = self._view.fields()
        self._selection_candidate = None
        self._selection_callback_candidate = None
        self._selection_callback_tick = None
        self._in_select_cell = False
        self._last_reshuffle_request = self._reshuffle_request = 0
        self._column_to_move = None
        self._column_move_target = None
        self._mouse_dragged = False
        self._search_panel = None
        self._last_updated_row_count = 0
        self._grid = None
        self._group_by_columns = ()
        self._aggregation_columns = ()
        self._last_grid_mouse_position = (None, None, None)
        self._last_cell_tooltip_position = (None, None)
        self._list_position = None

    def _default_columns(self):
        """Return the default form columns as a sequence of field identifiers (strings)."""
        return self._view.columns()

    def _init_select(self, async_count=False, grid_update=True):
        self._aggregation_results.reset()
        result = super(ListForm, self)._init_select(async_count=async_count)
        if grid_update and self._grid is not None:
            self._update_grid()
        return result

    def _aggregation_valid(self, operation, type):
        allowed_types = (pytis.data.Number,)
        if operation in (pytis.data.Data.AGG_MIN, pytis.data.Data.AGG_MAX,
                         pytis.data.Data.AGG_COUNT):
            allowed_types += (pytis.data.String, pytis.data.DateTime)
        return isinstance(type, allowed_types)

    def _get_aggregation_result(self, key):
        cid, operation = key
        c = self._data.find_column(cid)
        if ((c is not None and
             self._aggregation_valid(operation, c.type()) and
             self._row.permitted(cid, pytis.data.Permission.VIEW))):
            return self._data.select_aggregate((operation, cid),
                                               condition=self._current_condition(),
                                               transaction=self._open_transaction(),
                                               arguments=self._current_arguments())
        return None

    def _init_columns(self, columns=None):
        if not columns:
            columns = self._default_columns()
        self._columns = [self._view.field(id) for id in columns]

    def _init_grouping(self, grouping=None):
        if grouping is None:
            grouping = self._view.grouping()
        self._grouping = grouping

    def _init_aggregations(self, aggregations):
        if aggregations is None:
            aggregations = self._view.aggregations()
        self._aggregations = list(aggregations)

    def _apply_profile_parameters(self, profile):
        super(ListForm, self)._apply_profile_parameters(profile)
        self._init_columns(profile.columns())
        self._init_grouping(profile.grouping())
        self._init_aggregations(profile.aggregations())
        self._column_widths = dict(profile.column_widths() or {})

    def _apply_profile(self, profile, refresh=True):
        self._apply_profile_parameters(profile)
        self._update_label_height()
        self._update_grid(init_columns=True)
        self._refresh()
        # Force to count at least one line.  This is to allow user
        # break in case the first cursor operation is very slow.  We
        # must do it here, otherwise the delay happens later in
        # _on_idle where we can't handle it.
        self._table.number_of_rows(min_value=1)

    def _profile_parameters_to_save(self):
        return dict(super(ListForm, self)._profile_parameters_to_save(),
                    columns=tuple([c.id() for c in self._columns]),
                    grouping=self._grouping,
                    aggregations=tuple(self._aggregations),
                    column_widths=dict(self._column_widths))

    def _select_columns(self):
        return [c.id() for c in self._data.columns()
                if not isinstance(c.type(), pytis.data.Big)]

    def _ideal_column_width(self, column):
        """Return the ideal column width.

        If the user previously explicitly resized the column, the resized width
        is returned.  Otherwise the ideal width is determined by the
        specification.

        """
        try:
            return self._column_widths[column.id()]
        except KeyError:
            width = max(column.column_width(), len(column.column_label()))
            return dlg2px(self._grid, 4 * width + 8)

    def _update_label_height(self):
        height = self._label_height
        if self._aggregations:
            height += 1 + len(self._aggregations) * self._row_height
        g = self._grid
        g.SetColLabelSize(height)
        g.FitInside()

    def _create_form_parts(self):
        if self._ALLOW_TOOLBAR:
            self.Sizer.Add(self._create_toolbar(), 0, wx.EXPAND)
        self.Sizer.Add(self._create_grid(), 1, wx.EXPAND)
        self._create_query_fields_panel()

    def _create_grid(self):
        # Create the grid and table.  Initialize the data select.
        self._table = None
        self._grid = g = wx.grid.Grid(self)
        self._table = table = GridTable(
            self, self._data, self._row, self._columns, self._lf_select_count_,
            sorting=self._lf_sorting,
            grouping=self._grouping,
            row_style=self._view.row_style(),
        )
        g.SetTable(table, True)
        g.SetRowLabelSize(0)
        # g.SetColLabelAlignment(wx.CENTER, wx.CENTER)
        g.SetMargins(0, 0)
        g.DisableDragGridSize()
        g.DisableDragRowSize()
        if hasattr(wx.grid.Grid, 'wxGridSelectRows'):
            mode = wx.grid.Grid.wxGridSelectRows  # for wxPython 4.0.4
        else:
            mode = wx.grid.Grid.GridSelectionModes.GridSelectRows  # for wxPython 4.1.0
        g.SetSelectionMode(mode)
        g.SetLabelBackgroundColour(wx.SystemSettings.GetColour(wx.SYS_COLOUR_BACKGROUND))
        g.SetLabelFont(g.GetFont())  # Use standard font instead of bold.
        self._row_height = row_height = dlg2px(g, 0, 10)[1]
        self._label_height = dlg2px(g, 0, 12)[1]
        self._init_col_attr()
        self._update_colors()
        self._update_label_height()
        g.SetDefaultRowSize(row_height)
        # Event handlery
        labels = g.GetGridColLabelWindow()
        corner = g.GetGridCornerLabelWindow()
        wx_callback(wx.grid.EVT_GRID_SELECT_CELL, g, self._on_select_cell)
        wx_callback(wx.grid.EVT_GRID_COL_SIZE, g, self._on_label_drag_size)
        wx_callback(wx.grid.EVT_GRID_CELL_RIGHT_CLICK, g, self._on_right_click)
        wx_callback(wx.grid.EVT_GRID_CELL_LEFT_CLICK, g, self._on_left_click)
        wx_callback(wx.EVT_MOTION, g.GetGridWindow(), self._on_mouse_move)
        wx_callback(wx.EVT_MOUSEWHEEL, g, self._on_wheel)
        wx_callback(wx.EVT_IDLE, g, self._on_idle)
        wx_callback(wx.EVT_KEY_DOWN, g, self.on_key_down)
        wx_callback(wx.EVT_LEFT_DOWN, labels, self._on_label_left_down)
        wx_callback(wx.EVT_LEFT_UP, labels, self._on_label_left_up)
        wx_callback(wx.EVT_RIGHT_DOWN, labels, self._on_label_right_down)
        wx_callback(wx.EVT_MOTION, labels, self._on_label_mouse_move)
        wx_callback(wx.EVT_ENTER_WINDOW, labels, self._on_label_mouse_enter)
        wx_callback(wx.EVT_PAINT, labels, self._on_label_paint)
        wx_callback(wx.EVT_LEFT_DOWN, corner, self._on_corner_left_down)
        wx_callback(wx.EVT_RIGHT_DOWN, corner, self._on_corner_right_down)
        wx_callback(wx.EVT_PAINT, corner, self._on_corner_paint)
        return g

    def _update_grid(self, data_init=False, delete_column=None, insert_column=None,
                     inserted_column_index=None, init_columns=False, retain_row=False):
        g = self._grid
        notify = self._notify_grid
        current_row_number = self._current_cell()[0]
        if current_row_number not in (0, -1):
            original_row_number = current_row_number
        else:
            original_row_number = None
        if retain_row and original_row_number is not None:
            original_key = self._current_key()
        else:
            original_key = None
        # Uprav velikost gridu
        g.BeginBatch()
        try:
            if init_columns:
                notify(wx.grid.GRIDTABLE_NOTIFY_COLS_DELETED, 0, g.GetNumberCols())
                notify(wx.grid.GRIDTABLE_NOTIFY_COLS_INSERTED, 0, len(self._columns))
            if delete_column is not None:
                i = self._columns.index(delete_column)
                del self._columns[i]
                notify(wx.grid.GRIDTABLE_NOTIFY_COLS_DELETED, i, 1)
            if insert_column is not None:
                i = inserted_column_index
                if i is None or not (0 <= i <= len(self._columns)):
                    i = len(self._columns)
                self._columns.insert(i, insert_column)
                notify(wx.grid.GRIDTABLE_NOTIFY_COLS_INSERTED, i, 1)
            if data_init:
                row_count = self._init_select(async_count=True, grid_update=False)
            else:
                row_count = self._lf_count(timeout=0)
                self._data.rewind()
            self._table.update(columns=self._columns,
                               row_count=self._lf_select_count_,
                               sorting=self._lf_sorting,
                               grouping=self._grouping)
            old_row_count = g.GetNumberRows()
            self._update_grid_length(g, row_count, original_row_number)
            if insert_column is not None or delete_column is not None or init_columns:
                self._init_col_attr()
        finally:
            g.EndBatch()
        # Skip to the former line
        if retain_row and original_key is not None:
            if self._current_key() != original_key:
                self.select_row(original_key, quiet=True)
            # Pokud se nepodařilo nastavit pozici na předchozí klíč,
            # pokusíme se nastavit pozici na předchozí číslo řádku v gridu.
            if self._current_key() != original_key:
                row = original_row_number
                if ((row is not None and
                     row < self._table.number_of_rows(min_value=(row + 1)) and row >= 0)):
                    self._select_cell(row=row)
                else:
                    self._select_cell(row=0)
        else:
            self._select_cell(row=original_row_number)
        # Závěrečné úpravy
        self._update_colors()
        self._resize_columns()
        if row_count != old_row_count or insert_column is not None or delete_column is not None \
                or init_columns:
            # Force scrollbar update by generating a size event.
            # g.SetSize(g.GetSize())
            g.FitInside()

    def _update_grid_length(self, g, row_count, current_row):
        notify = self._notify_grid
        self._last_updated_row_count = row_count
        old_row_count = g.GetNumberRows()
        row_count_diff = row_count - old_row_count
        if row_count_diff < 0:
            if row_count == 0 or current_row is None:
                current_row = 1
            notify(wx.grid.GRIDTABLE_NOTIFY_ROWS_DELETED, current_row, -row_count_diff)
        elif row_count_diff > 0:
            notify(wx.grid.GRIDTABLE_NOTIFY_ROWS_APPENDED, row_count_diff)
        notify(wx.grid.GRIDTABLE_REQUEST_VIEW_GET_VALUES)
        if row_count != old_row_count:
            g.FitInside()

    def _notify_grid(self, message_id, *args):
        msg = wx.grid.GridTableMessage(self._table, message_id, *args)
        self._grid.ProcessTableMessage(msg)

    def _init_col_attr(self):
        for i, c in enumerate(self._columns):
            attr = wx.grid.GridCellAttr()
            attr.SetReadOnly()
            ctype = self._row.type(c.id())
            if isinstance(ctype, pytis.data.Number):
                alignment = wx.ALIGN_RIGHT
            elif isinstance(ctype, pytis.data.Boolean):
                alignment = wx.ALIGN_CENTER
            else:
                alignment = wx.ALIGN_LEFT
            attr.SetAlignment(alignment, wx.CENTER)
            self._grid.SetColAttr(i, attr)

    def _query_fields_row(self):
        try:
            form = self._query_fields_form
        except AttributeError:
            # The first argument provider call is made during form
            # initialization before the initial select when the form user
            # interface is not created yet.  Thus query fields are not yet
            # available as well.
            if self._view.query_fields().autoinit() or self._query_field_values:
                # If autoinit is on, we will construct the row from default
                # field values.
                return super(ListForm, self)._query_fields_row()
            else:
                # If autoinit is off, we return None, which avoids calling
                # argument provider at all.  The form will appear initially
                # empty and the user will need to submit query fields to see
                # any data.
                return None
        else:
            if form is None:
                return None
            # Append query field values as the second argument provider
            # argument.
            return form.row()

    def _create_query_fields_panel(self):
        query_fields = self._view.query_fields()
        if query_fields:
            panel = wx.Panel(self, -1, style=wx.SUNKEN_BORDER)
            self._query_fields_form = form = QueryFieldsForm(
                panel, pytis.config.resolver, None,
                query_fields=query_fields,
                callback=self._on_query_fields_refresh,
                prefill=self._query_field_values,
                materialized_view=self._data if query_fields.materialized_view() else None,
            )
            self._query_fields_panel_buttons = buttons = (
                wx_button(panel, icon='minimize-down', noborder=True, size=(26, 20),
                          tooltip=_("Minimize/maximize query panel."),
                          callback=self._on_minimize_query_fields),
                wx_button(panel, icon='move-up', noborder=True, size=(26, 20),
                          tooltip=_("Move the query panel to the top/bottom edge of the form."),
                          callback=self._on_move_query_fields))
            sizer = wx.BoxSizer()
            sizer.Add(form, 0, wx.EXPAND | wx.FIXED_MINSIZE)
            sizer.Add((0, 0), 1)
            for button in buttons:
                sizer.Add(button, 0, wx.FIXED_MINSIZE, 1)
            panel.SetSizer(sizer)
            sizer.Fit(panel)
            panel.SetAutoLayout(1)
            position = self._saved_settings.get('query-fields-position', 'up')
            if position == 'up':
                i = self.Sizer.GetChildren().index(self.Sizer.GetItem(self._grid))
                self.Sizer.Insert(i, panel, 0, wx.EXPAND)
            else:
                self.Sizer.Add(panel, 0, wx.EXPAND)
            if self._saved_settings.get('query-fields-minimized', False):
                self._toggle_query_fields_minimization()
            self._update_query_fields_panel_button_bitmaps()
        else:
            self._query_fields_form = None

    def _on_query_fields_refresh(self, row):
        self.refresh(interactive=True, reload_query_fields=False)
        self._grid.SetFocus()

    def refresh(self, reload_query_fields=True, **kwargs):
        query_fields = self._view.query_fields()
        if query_fields and query_fields.load() and reload_query_fields:
            row = self._query_fields_row()
            if row is not None:
                query_fields.load()(row)
        return super(ListForm, self).refresh(**kwargs)

    def restore(self):
        if not self._restored and self._query_fields_form:
            self._query_fields_form.restore()
        return super(ListForm, self).restore()

    def _update_query_fields_panel_button_bitmaps(self):
        sizer = self.Sizer
        children = sizer.GetChildren()
        grid_position = children.index(sizer.GetItem(self._grid))
        panel_position = children.index(sizer.GetItem(self._query_fields_form.GetParent()))
        on_top = panel_position < grid_position
        for button in self._query_fields_panel_buttons:
            if button is self._query_fields_panel_buttons[0]:
                if self._query_fields_minimized():
                    icon = 'maximize-down' if on_top else 'maximize-up'
                else:
                    icon = 'minimize-up' if on_top else 'minimize-down'
            else:
                icon = 'move-down' if on_top else 'move-up'
            bitmap = get_icon(icon, type=wx.ART_TOOLBAR)
            button.SetBitmapLabel(bitmap)

    def _on_move_query_fields(self, event):
        sizer = self.Sizer
        target_position = sizer.GetChildren().index(sizer.GetItem(self._grid))
        panel = event.GetEventObject().GetParent()
        sizer.Detach(panel)
        sizer.Insert(target_position, panel, 0, wx.EXPAND)
        sizer.Layout()
        self._update_query_fields_panel_button_bitmaps()
        position = ('up' if sizer.GetItem(panel).Position.y < sizer.GetItem(self._grid).Position.y
                    else 'down')
        self._saved_settings.set('query-fields-position', position)

    def _on_minimize_query_fields(self, event):
        self._toggle_query_fields_minimization()
        self._update_query_fields_panel_button_bitmaps()
        self._saved_settings.set('query-fields-minimized', self._query_fields_minimized())

    def _query_fields_minimized(self):
        return not self._query_fields_form.IsShown()

    def _toggle_query_fields_minimization(self):
        # Minimize/Deminimize the panel.
        form = self._query_fields_form
        panel = form.GetParent()
        for child in panel.GetChildren():
            if child not in self._query_fields_panel_buttons:
                child.Show(not child.IsShown())
        self.Sizer.Layout()
        if self._query_fields_minimized():
            row = form.row()
            if row:
                values = ['%s: %s' % (field.spec().label(), row.format(field.id()))
                          for field in form.fields()]
                panel.SetToolTip(', '.join(values))
        elif panel.GetToolTip():
            panel.SetToolTip(' ')
            panel.GetToolTip().Enable(False)  # Doesn't seem to work, thus the line above...

    def _context_menu(self):
        """Vrať specifikaci \"kontextového\" popup menu vybrané buňky seznamu.

        Vrací: Sekvenci instancí 'MenuItem'.

        Tuto metodu nechť odvozené třídy předefinují, pokud chtějí zobrazovat
        kontextové menu.

        """
        return ()

    def _lf_sfs_columns(self):
        def labelfunc(c):
            label = c.column_label()
            if c not in self._columns and label:
                label += _(" (hidden)")
            return label
        return sfs_columns(self._fields, self._data, labelfunc=labelfunc)

    def _create_search_panel(self, full=False, prefill=None):
        HEIGHT = 27
        self._search_panel = panel = wx.Panel(self, -1, style=wx.SUNKEN_BORDER)
        self._incremental_search_last_direction = pytis.data.FORWARD
        self._incremental_search_results = []
        columns = [(c.column_label(), c) for c in self._columns
                   if isinstance(self._row.type(c.id()), pytis.data.String)]
        self._search_panel_controls = controls = (
            wx_choice(panel, columns, selected=self._columns[self._current_cell()[1]],
                      tooltip=_("Choose the column to search in (incremental search " +
                                "is only possible in text columns)."),
                      height=HEIGHT),
            wx_text_ctrl(panel, tooltip=_("Enter the text search for."),
                         on_text=lambda e: self._incremental_search(newtext=True),
                         on_key_down=self._on_incremental_search_key_down, height=HEIGHT),
            wx_button(panel, icon=wx.ART_GO_BACK, height=HEIGHT, tooltip=_("Find previous"),
                      command=Command(self.search, next=True, back=True)),
            wx_button(panel, icon=wx.ART_GO_FORWARD, height=HEIGHT, tooltip=_("Find next"),
                      command=Command(self.search, next=True)),
            wx_checkbox(panel, label=_("search also inside values"),
                        tooltip=_("Check if you want to search anywhere inside the value. "
                                  "Otherwise the substring is only searched at the start "
                                  "of the string."),
                        checked=full),
            wx_checkbox(panel, label=_("case sensitive"),
                        tooltip=_("Check if you want to distinguish "
                                  "lower and upper case letters."),
                        checked=False),
            wx_button(panel, icon='close', noborder=True,
                      tooltip=_("Hide the search panel"),
                      callback=lambda e: self._exit_incremental_search()),
        )
        sizer = wx.BoxSizer()
        for i, ctrl in enumerate(controls[:-1]):
            sizer.Add(ctrl, 0, wx.LEFT, i >= 4 and 10 or 0)
        sizer.Add((0, 0), 1)
        sizer.Add(controls[-1])
        panel.SetSizer(sizer)
        panel.SetAutoLayout(True)
        self.Sizer.Add(panel, 0, wx.EXPAND)
        self.Sizer.Layout()
        if prefill:
            controls[1].SetValue(prefill)
        else:
            controls[1].SetFocus()

    def _on_incremental_search_key_down(self, event):
        code = event.GetKeyCode()
        if code in (wx.WXK_RETURN, wx.WXK_NUMPAD_ENTER):
            self._exit_incremental_search()
        elif code == wx.WXK_BACK:
            if self._incremental_search_results:
                row, text = self._incremental_search_results.pop()
                self.select_row(row)
            event.Skip()
        elif code == wx.WXK_ESCAPE or code == ord('G') and event.ControlDown():
            self._exit_incremental_search(rollback=True)
        else:
            event.Skip()

    def _incremental_search(self, direction=None, newtext=False):
        if direction is None:
            direction = self._incremental_search_last_direction
        else:
            self._incremental_search_last_direction = direction
        choice, ctrl, b1, b2, full, case, b3 = self._search_panel_controls
        text = ctrl.GetValue()
        if newtext:
            oldtext = text[:-1]
        else:
            oldtext = text
        row = self._current_cell()[0]
        self._incremental_search_results.append((row, oldtext))
        column = choice.GetClientData(choice.GetSelection())
        stext = text + '*'
        if full.IsChecked():
            stext = '*' + stext
        wmvalue = pytis.data.WMValue(pytis.data.String(), stext)
        condition = pytis.data.WM(column.id(), wmvalue, ignore_case=not case.IsChecked())
        # TODO: Předhledání v aktuálním selectu
        found = self._search(condition, direction, row_number=row, report_failure=False,
                             initial_shift=newtext)
        if found is None:
            app.echo(_("No next matching record"), kind='error')
        else:
            if direction == pytis.data.FORWARD:
                new_row = row + found
            else:
                new_row = row - found
            self._select_cell(row=new_row)

    def _exit_incremental_search(self, rollback=False):
        panel = self._search_panel
        self._search_panel = None
        self._search_panel_controls = None
        if rollback and self._incremental_search_results:
            self.select_row(self._incremental_search_results[0][0])
        panel.Enable(False)
        self.Sizer.Detach(panel)
        panel.Destroy()
        self.Sizer.Layout()
        self._grid.SetFocus()
        if not rollback:
            row = self._table.current_row()
            if row is not None:
                the_row = self._table.row(row)
                self._run_callback(self.CALL_SELECTION, the_row)

    # Pomocné metody

    def _current_cell(self):
        """Vrať dvojici souřadnic (ROW, COL) aktuální buňky."""
        g = self._grid
        return g.GetGridCursorRow(), g.GetGridCursorCol()

    def _current_key(self):
        the_row = self.current_row()
        if the_row is None:
            return None
        else:
            return the_row.row().columns([c.id() for c in self._data.key()])

    def current_row(self):
        result = None
        row = self._current_cell()[0]
        if row >= 0 and row < self._table.number_of_rows(min_value=(row + 1)):
            try:
                result = self._table.row(row)
            except Exception:
                # It sometimes happens, under unknown circumstances, that the
                # data select gets changed without updating GridTable selection
                # data.  Then `row' may actually be outside the reported number
                # of rows limit and the table row call above may crash.  In
                # such a case, it's probably better to return an unknown row
                # instead of raising an error.
                pass
        return result

    def selected_rows(self):
        return TableRowIterator(self._table, self._grid.GetSelectedRows())

    def unselect_selected_rows(self):
        self._grid.ClearSelection()

    def _select_cell(self, row=None, col=None, invoke_callback=True):
        # Vrací pravdu, pokud může být událost provedena (viz _on_select_cell).
        if self._in_select_cell:
            return True
        self._in_select_cell = True
        if __debug__:
            log(DEBUG, 'Grid cell move:', (row, col))
        self._table.number_of_rows(min_value=1)
        try:
            g = self._grid
            current_row = g.GetGridCursorRow()
            current_col = g.GetGridCursorCol()
            if row is not None:
                assert isinstance(row, int)
                if row >= g.GetNumberRows():
                    row_count = self._table.number_of_rows(min_value=(row + 1))
                    if row < row_count:
                        self._update_grid_length(g, row_count, current_row)
                if row < 0 or row >= g.GetNumberRows():
                    row = 0
                else:
                    if col is None:
                        col = max(0, current_col)
                    g.SetGridCursor(row, col)
                    g.MakeCellVisible(row, col)
                    self._selection_candidate = (row, col)
                    if invoke_callback:
                        # Delay callback invocation after the end of scrolling.
                        self._selection_callback_candidate = row
                        delay = self._SELECTION_CALLBACK_DELAY
                        self._selection_callback_tick = delay
                # TODO: tady to způsobuje špatné zobrazování pozice v
                #       dualform. Nahrazeno voláním _update_list_position v
                #       _post_selection_hook.
                #       Jiné řešení?
                # self._update_list_position()
            elif col is not None and col != current_col:
                g.SetGridCursor(current_row, col)
                g.MakeCellVisible(current_row, col)
            if __debug__:
                log(DEBUG, 'Cell selected:', (row, col))
            return True
        finally:
            self._in_select_cell = False

    def _search_adjust_data_position(self, row_number):
        if row_number is None:
            row_number = self._current_cell()[0]
        self._table.rewind(position=row_number)

    def _search_skip(self, skip, direction):
        if direction == pytis.data.BACKWARD:
            skip = -skip
        row, col = self._current_cell()
        new_row = row + skip
        self._table.rewind(position=new_row)
        self._select_cell(row=new_row)

    def _apply_filter(self, condition):
        self._lf_filter = condition
        self._refresh()
        # Force to count at least one line.  This is to allow user
        # break in case the first cursor operation is very slow.  We
        # must do it here, otherwise the delay happens later in
        # _on_idle where we can't handle it.
        self._table.number_of_rows(min_value=1)

    def _current_column_id(self):
        col = self._current_cell()[1]
        return self._columns[col].id()

    # Callbacky

    def on_data_change(self):
        """Callback, který lze zavolat při změně dat v datovém zdroji.

        Metoda je určena pro registraci pomocí metody
        'pytis.data.Data.add_callback_on_change'.

        Metoda naopak není určena pro žádost o okamžitý update, protože pouze
        zadá požadavek na update, který je zpracován až za blíže neurčenou
        dobu.  K přímým updatům slouží metody 'reset()' a 'refresh()'.

        """
        log(EVENT, 'List data change notification')
        now = time.time()
        maybe_future = self._last_reshuffle_request + self._REFRESH_PERIOD
        self._reshuffle_request = max(now, maybe_future)
        self._update_data_status()

    def _update_cell_tooltip(self, row, col):
        value = self._get_cell_tooltip_string(row, col)
        gw = self._grid.GetGridWindow()
        if value:
            tooltip = gw.GetToolTip()
            if tooltip is None:
                tooltip = wx.ToolTip(value)
                gw.SetToolTip(tooltip)
            elif tooltip.GetTip() != value:
                tooltip.SetTip(value)
        else:
            gw.SetToolTip(None)

    def _on_idle(self, event):
        super(ListForm, self)._on_idle(event)
        if is_busy_cursor() or pytis.form.app.current_form() is not self:
            # Prevent blocking the idle method of a popup form opened on top of the current form,
            # such as PopupEditForm or Codebook.
            return
        if self._selection_candidate is not None:
            row, col = self._selection_candidate
            self._selection_candidate = None
            self._grid.MakeCellVisible(row, col)
            self._grid.Refresh()
        x, y, timestamp = self._last_grid_mouse_position
        if timestamp is not None:
            now = 1000 * int(time.time()) + int(datetime.datetime.now().microsecond // 1000)
            delay = now - timestamp
            if delay > 10:
                # Adding 10ms delay helps to keep the tooltip close to the current
                # mouse position.  Without it the tooltip stays at the same place
                # where it first appeared and only changes the contents when moving
                # the mouse over cells.  The unpleasant effect is that tooltips
                # take longer to appear (the tooltip delay adds to the 10ms delay
                # and also to the idle event delay), which is sometimes noticable.
                # The delay might be also usefull when implementing tooltips by a
                # custom widget...
                row, col = self._grid.XYToCell(x, y)
                if (row, col) != self._last_cell_tooltip_position:
                    wx.CallAfter(self._update_cell_tooltip, row, col)
                    self._last_cell_tooltip_position = row, col
            else:
                self._grid.GetGridWindow().SetToolTip(None)
                self._last_cell_tooltip_position = (None, None)
        if self._selection_callback_candidate is not None:
            if self._selection_callback_tick > 0:
                self._selection_callback_tick -= 1
                microsleep(100)
                event.RequestMore()
            else:
                row = self._selection_callback_candidate
                self._selection_callback_candidate = None
                wx.CallAfter(self._run_selection_callback, row)
        # Update grid when lazy row count computation is in progress; we
        # mustn't do it much often otherwise row count computation gets disrupted
        # and slowed down significantly.  Hence the timeout value below.
        if self._last_updated_row_count != self._table.number_of_rows(timeout=0.3):
            self._update_grid()
            self._update_list_position()
        # V budoucnu by zde mohlo být přednačítání dalších řádků nebo dat
        event.Skip()

    def _get_cell_tooltip_string(self, row, col):
        # Return the tooltip string for given grid cell.
        if (((self._transaction is None and not self._data.select_active()) or
             (self._transaction is not None and not self._transaction.open()))):
            self.refresh()
            if row >= self._lf_count(min_value=(row + 1)):
                return None
        try:
            # Despite the above checks we've seen "Not within select" tracebacks so the
            # checks are probably insufficient or there is a race condition.  The question
            # is whether it is not enough to handle the exception here and remove the
            # checks alltogether.
            record = self._table.row(row)
        except pytis.data.NotWithinSelect:
            return True
        if record:
            cid = self._columns[col].id()
            tooltip = record.display(cid) or None
            if not tooltip and isinstance(record.type(cid), pytis.data.String):
                value = record[cid].value()
                if value:
                    width, height = self._grid.GetTextExtent(value)
                    one_line_height = self._grid.GetTextExtent('X')[1]
                    if width > self._grid.GetColSize(col) or height > one_line_height:
                        # If the value doesn't fit the cell, show it in tooltip.
                        tooltip = value
            return tooltip
        else:
            return None

    def _run_selection_callback(self, row):
        the_row = self._table.row(row)
        if the_row is not None:
            self._run_callback(self.CALL_SELECTION, the_row)
            self._post_selection_hook(the_row)

    def _post_selection_hook(self, the_row):
        if Form.focused_form() is self:
            # TODO: viz poznámka v _select_cell.
            self._update_list_position()
            # Show display value in status line (this is also shown in tooltips,
            # but tooltips are not useful when using keyboard only).
            row, col = self._current_cell()
            if row >= 0 and col >= 0:
                app.echo(self._table.row(row).display(self._columns[col].id()))

    def _on_select_cell(self, event):
        if not self._in_select_cell and self._grid.GetBatchCount() == 0:
            # GetBatchCount zjišťujeme proto, aby nedhocházelo k volání
            # callbacku při změnách v rámci _update_grid(), které nejsou
            # interaktivní.
            self._run_callback(self.CALL_USER_INTERACTION)
            if self._last_updated_row_count != self._table.number_of_rows(timeout=0):
                self._update_grid()
        if self._select_cell(row=max(0, event.GetRow()), col=event.GetCol()):
            # SetGridCursor vyvolá tento handler.  Aby SetGridCursor mělo
            # vůbec nějaký účinek, musíme zde zavolat originální handler, který
            # požadované nastavení buňky zajistí.
            event.Skip()
        else:
            event.Veto()

    def _on_activation(self, alternate=False):
        if alternate:
            f = pytis.form.DescriptiveDualForm
        else:
            f = BrowsableShowForm
        kwargs = self._new_form_kwargs()
        if self._view.query_fields():
            row = self._query_fields_row()
            kwargs['query_field_values'] = [(k, row[k].value()) for k in row.keys()]
        run_form(f, self._name, select_row=self._current_key(), **kwargs)

    def _scroll_x_offset(self):
        g = self._grid
        return g.GetViewStart()[0] * g.GetScrollPixelsPerUnit()[0]

    def _available_columns(self):
        select_columns = self._select_columns()
        if select_columns is None:
            columns = self._fields
        else:
            columns = [self._view.field(cid) for cid in select_columns]
        return sorted([c for c in columns if c and not c.disable_column()],
                      key=lambda col: pytis.util.strxfrm(col.column_label()))

    def _displayed_columns_menu(self, column_index):
        menu = [MenuItem(_("Display row headings"), command=Command(ListForm.toggle_row_labels),
                         state=lambda: self._grid.GetRowLabelSize() != 0)]
        if column_index is not None:
            cid = self._columns[column_index].id()
            menu.append(
                MenuItem(_("Hide this column"),
                         command=Command(ListForm.toggle_column, column_id=cid, position=None))
            )

        hidden_columns = [c for c in self._available_columns() if c not in self._columns]
        if hidden_columns:
            pos = column_index + 1 if column_index is not None else len(self._columns)
            menu.append(Menu(_("Add column"), [
                MenuItem(c.column_label() or c.id(), help=c.descr(),
                         command=Command(ListForm.toggle_column, column_id=c.id(), position=pos))
                for c in hidden_columns
            ]))
        else:
            menu.append(MenuItem(_("Add column"),
                                 command=Command(Application.nothing, enabled=False)))
        menu.append(MenuItem(_("Displayed columns"), command=Command(ListForm.toggle_columns)))
        return menu

    def _aggregation_menu(self):
        menu = [MenuItem(title, command=Command(ListForm.toggle_aggregation, operation=op),
                         state=lambda op=op: op in self._aggregations)
                for op, title, icon, label in self._AGGREGATIONS]
        menu.extend((MenuSeparator(),
                     MenuItem(_("Show all"), command=Command(ListForm.aggregate)),
                     MenuItem(_("Hide all"), command=Command(ListForm.unaggregate)),
                     ))
        predefined_aggregated_views = self._view.aggregated_views()
        if predefined_aggregated_views:
            menu.append(MenuSeparator())
            menu.extend([
                MenuItem(v.name(),
                         command=Command(ListForm.aggregated_view, aggregated_view_id=v.id()),
                         help=_("Open predefined aggregated view"))
                for v in predefined_aggregated_views
            ])
        manager = pytis.form.app.aggregated_views_manager
        aggregated_views = [manager.load(self._name, aggregated_view_id)
                            for aggregated_view_id in manager.list(self._name)]
        if aggregated_views:
            menu.append(MenuSeparator())
            menu.extend([
                MenuItem(v.name(),
                         command=Command(ListForm.aggregated_view, aggregated_view_id=v.id()),
                         help=_("Open user defined aggregated view"))
                for v in aggregated_views
            ])
        menu.extend((MenuSeparator(),
                     MenuItem(_("Define new aggregated view"),
                              command=Command(ListForm.aggregated_view, aggregated_view_id=None))))
        if aggregated_views:
            menu.append(Menu(_("Remove agregated view"), [MenuItem(
                v.name(),
                command=Command(ListForm.delete_aggregated_view, aggregated_view_id=v.id()),
            ) for v in aggregated_views]))
        return menu

    def _column_context_menu(self, col):
        return (
            Menu(_("Primary Sorting"), (
                MenuItem(_("Sort Ascending"),
                         command=Command(LookupForm.sort, col=col, primary=True,
                                         direction=pytis.data.ASCENDENT)),
                MenuItem(_("Sort Descending"),
                         command=Command(LookupForm.sort, col=col, primary=True,
                                         direction=pytis.data.DESCENDANT)),
            )),
            Menu(_("Secondary Sorting"), (
                MenuItem(_("Sort Ascending"),
                         command=Command(LookupForm.sort, col=col,
                                         direction=pytis.data.ASCENDENT)),
                MenuItem(_("Sort Descending"),
                         command=Command(LookupForm.sort, col=col,
                                         direction=pytis.data.DESCENDANT)),
            )),
            MenuItem(_("Omit this column from sorting"),
                     command=Command(LookupForm.sort, col=col, direction=self.UNSORT)),
            MenuItem(_("Cancel sorting completely"),
                     command=Command(LookupForm.sort, direction=self.UNSORT)),
            MenuSeparator(),
            MenuItem(_("Group up to this column"),
                     command=Command(ListForm.set_grouping_column, col=col)),
            MenuItem(_("Cancel visual grouping"),
                     command=Command(ListForm.set_grouping_column, col=None)),
            MenuSeparator(),
            MenuItem(_("Autofilter"), command=Command(ListForm.autofilter, col=col)),
            MenuItem(_("Cancel filtering"), command=Command(LookupForm.unfilter)),
            MenuSeparator(),
        ) + tuple(self._displayed_columns_menu(col))

    def _aggregation_info_by_position(self, y):
        if y > self._label_height and self._aggregations:
            i = max(0, min(len(self._aggregations) - 1,
                           (y - self._label_height) // self._row_height))
            return [x for x in self._AGGREGATIONS if x[0] in self._aggregations][i]
        else:
            return None

    def _on_label_right_down(self, event):
        # The menu must be constructed here since it depends on the mouse event position.
        self._run_callback(self.CALL_USER_INTERACTION)
        col = self._grid.XToCol(event.GetX() + self._scroll_x_offset())
        aggregation = self._aggregation_info_by_position(event.GetY())
        if aggregation is not None:
            menu = self._aggregation_menu()
            if col != -1:
                menu[0:0] = (
                    MenuItem(_("Copy the Result"),
                             command=Command(self.copy_aggregation_result,
                                             cid=self._columns[col].id(),
                                             operation=aggregation[0])),
                    MenuSeparator(),
                )
        elif col == -1:
            menu = self._displayed_columns_menu(None)
        else:
            menu = self._column_context_menu(col)
        self._popup_menu(menu)
        event.Skip()

    def _on_label_left_down(self, event):
        g = self._grid
        if event.GetY() > self._label_height:
            return
        x = event.GetX() + self._scroll_x_offset()
        col = g.XToCol(x)
        if col != -1:
            x1 = functools.reduce(lambda x, i: x + g.GetColSize(i), range(col), 0)
            x2 = x1 + g.GetColSize(col)
            if x > x1 + 2 and x < x2 - 2:
                self._column_to_move = col
        self._mouse_dragged = False
        # We don't call event.Skip() since we want to suppress the default bahavior
        # (eg. range selection when Shift is down).

    def _on_label_left_up(self, event):
        if self._column_move_target is not None:
            old_index = self._column_to_move
            new_index = self._column_move_target
            if new_index > old_index:
                new_index -= 1
            if old_index is not None and old_index != new_index:
                c = self._columns[old_index]
                self._update_grid(delete_column=c, insert_column=c,
                                  inserted_column_index=new_index)
        elif not self._mouse_dragged:
            col = self._grid.XToCol(event.GetX() + self._scroll_x_offset())
            if col != -1:
                self._run_callback(self.CALL_USER_INTERACTION)
                cid = self._columns[col].id()
                primary = not event.ShiftDown()
                position = self._sorting_position(cid)
                if primary and position != 0:
                    direction = pytis.data.ASCENDENT
                else:
                    current_direction = self._sorting_direction(cid) or self.UNSORT
                    cycle = [pytis.data.ASCENDENT, pytis.data.DESCENDANT, self.UNSORT]
                    direction = cycle[(cycle.index(current_direction) + 1) % 3]
                self.sort(col=col, primary=primary, direction=direction)
        self._column_move_target = None
        self._column_to_move = None
        event.GetEventObject().Refresh()
        event.Skip()

    def _on_mouse_move(self, event):
        x, y = self._grid.CalcUnscrolledPosition(event.GetX(), event.GetY())
        last_x, last_y, last_time = self._last_grid_mouse_position
        if last_x != x or last_y != y:
            timestamp = 1000 * int(time.time()) + int(datetime.datetime.now().microsecond // 1000)
            self._last_grid_mouse_position = x, y, timestamp
        # Don't call event.Skip() here to prevent grid rows selection on mouse drag
        # (require using Shift/Ctrl to create/extend the selection).

    def _on_label_mouse_move(self, event):
        def nearest_column(x):
            g = self._grid
            n = g.GetNumberCols()
            pos = 0
            lastwidth = 0
            for col in range(n + 1):
                if col < n:
                    width = g.GetColSize(col)
                else:
                    width = 0
                if pos - lastwidth // 2 <= x <= pos + width // 2:
                    return col
                lastwidth = width
                pos += width
            return g.GetNumberCols()
        if self._column_to_move is not None:
            self._column_move_target = nearest_column(event.GetX() + self._scroll_x_offset())
            event.GetEventObject().Refresh()
        else:
            self._update_label_tooltips(event)
        self._mouse_dragged = True
        event.Skip()

    def _on_label_mouse_enter(self, event):
        self._update_label_tooltips(event)
        event.Skip()

    def _update_label_tooltips(self, event):
        col = self._grid.XToCol(event.GetX() + self._scroll_x_offset())
        if col != -1:
            column = self._columns[col]
            aggregation = self._aggregation_info_by_position(event.GetY())
            if aggregation is not None:
                descr = _("Result of aggregation function %(aggregation)s "
                          "for column %(column)s",
                          aggregation=aggregation[1],
                          column=column.label())
            else:
                descr = column.descr() or column.label() or ''
        else:
            descr = ''
        window = event.GetEventObject()
        # Testing the attribute is here to work around strange errors, where
        # we got a MenuItem instance from GetEventObject.  We didn't manage
        # to reproduce it, but we received such tracebacks.
        if hasattr(window, 'GetToolTip'):
            tip = window.GetToolTip()
            # Setting to None doesn't remove the tooltip, so we at least set
            # the tooltip to an ampty string.
            if tip is None:
                tip = wx.ToolTip(descr)
                window.SetToolTip(tip)
            elif tip.GetTip() != descr:
                tip.SetTip(descr)

    def _on_label_drag_size(self, event):
        self._remember_column_width(event.GetRowOrCol())
        self._grid.FitInside()
        # Mohli bychom rozšířit poslední sloupec, ale jak ho potom zase zúžit?
        # if pytis.config.stretch_tables:
        #     g = self._grid
        #     n = g.GetNumberCols()
        #     w = functools.reduce(lambda x, i: x + g.GetColSize(i), range(n), 0)
        #     x = g.GetSize().x
        #     if w < x:
        #         col = n-1
        #         g.SetColSize(col, g.GetColSize(col) + (x - w))
        #         self._remember_column_width(col)
        event.Skip()

    def _remember_column_width(self, col):
        self._column_widths[self._columns[col].id()] = self._grid.GetColSize(col)

    def _on_label_paint(self, event):
        def triangle(x, y, r=4, reversed=True):
            # Return polygon coordinates for a triangle.
            if reversed:
                return ((x, y), (x + 2 * r, y), (x + r, y + r))
            else:
                return ((x + r, y), (x + 2 * r, y + r), (x, y + r))

        def arrow(x, y, r=5, length=4):
            # Return polygon coordinates for an arrow.
            return ((x, y), (x - r, y - r), (x - r // 2, y - r), (x - r // 2, y - r - length),
                    (x + r // 2, y - r - length), (x + r // 2, y - r), (x + r, y - r))

        def funnel(x, y, r=3, length=8):
            # Return polygon coordinates for a funnel.
            return ((x, y), (x + r, y + r), (x + r, y + length), (x + r + 2, y + length),
                    (x + r + 2, y + r), (x + r * 2 + 2, y), (x, y))
        g = self._grid
        dc = wx.PaintDC(g.GetGridColLabelWindow())
        dc.SetTextForeground(wx.BLACK)
        # dc.SetFont(g.GetFont())
        x = - self._scroll_x_offset()
        row_height = self._row_height
        total_width = dc.GetSize().GetWidth()
        total_height = g.GetColLabelSize()
        label_height = self._label_height
        filtered_columns = self._filtered_columns()
        for i, column in enumerate(self._columns):
            if i >= g.GetNumberCols():
                # This should not happen, but we have seen wxAssertionError
                # tracebacks on g.GetColSize(i).
                log(OPERATIONAL, "Unexpected situation in _on_label_paint:",
                    (g.GetNumberCols(), i, self._columns))
                continue
            width = g.GetColSize(i)
            cid = column.id()
            y = 0
            if i == 0:
                dx = 0
            else:
                dx = 1
            if self._lf_filter is not None and pytis.config.filter_color:
                dc.SetBrush(wx.Brush(pytis.config.filter_color, wx.BRUSHSTYLE_SOLID))
            else:
                dc.SetBrush(wx.Brush('GRAY', wx.BRUSHSTYLE_TRANSPARENT))
            # Draw the rectangle around.
            dc.DrawRectangle(x - dx, y, width + dx, label_height)
            # Indicate when the column is being moved (before we clip the active refion).
            move_target = self._column_move_target
            if self._column_to_move is not None and move_target is not None:
                if i == move_target:
                    ax = x - dx + (i == 0 and 5 or 0)
                elif i == move_target - 1 and i == len(self._columns) - 1:
                    ax = x + width - 5
                else:
                    ax = None
                if ax is not None:
                    dc.SetBrush(wx.Brush('GREEN', wx.BRUSHSTYLE_SOLID))
                    dc.DrawPolygon(arrow(ax, label_height - 2))
            dc.SetClippingRegion(x, 0, total_width - x, total_height)
            # Draw the label itself.
            label = column.column_label()
            while dc.GetTextExtent(label)[0] > width and len(label):
                label = label[:-1]  # Shrink the label to fit the column width.
            dc.DrawLabel(label, (x, y, width, label_height), wx.ALIGN_CENTER)
            # Draw the sorting sign.
            pos = self._sorting_position(cid)
            if pos is not None:
                left = x + width - 12
                top = y + 3
                r = self._sorting_direction(cid) == pytis.data.ASCENDENT
                if cid in self._grouping:
                    color = 'GREEN'
                else:
                    color = 'CORAL'
                dc.SetBrush(wx.Brush(color, wx.BRUSHSTYLE_SOLID))
                for i in range(pos):
                    dc.DrawLine(left, top + 2 * i, left + 9, top + 2 * i)
                dc.DrawPolygon(triangle(left, top + pos * 2, reversed=r))
            # Draw the filter sign.
            if cid in filtered_columns:
                dc.SetBrush(wx.Brush('GOLD', wx.BRUSHSTYLE_SOLID))
                dc.DrawPolygon(funnel(x + 2, y + 3))
            # Draw the aggregation results.
            y += label_height
            if self._aggregations:
                for op, title, icon_id, label in self._AGGREGATIONS:
                    if op in self._aggregations:
                        rect = (x - dx, y - 1, width + dx, row_height + 1)
                        dc.SetBrush(wx.Brush('GRAY', wx.BRUSHSTYLE_TRANSPARENT))
                        dc.DrawRectangle(*rect)
                        value = self._aggregation_results[(cid, op)]
                        if value is not None:
                            icon = get_icon(icon_id)
                            if isinstance(value.type(), pytis.data.Number):
                                align = wx.ALIGN_CENTER_VERTICAL | wx.ALIGN_RIGHT
                            else:
                                align = wx.ALIGN_CENTER_VERTICAL | wx.ALIGN_LEFT
                            text = ' ' + value.export()
                            if not icon:
                                text = label + text
                            dc.SetClippingRegion(x, y, width, row_height)
                            dc.DrawLabel(text, bitmap=icon, alignment=align,
                                         rect=(x - dx + 2, y + 1, width + dx - 4, row_height))
                            dc.DestroyClippingRegion()
                        y += row_height
                dc.DrawLine(x - dx, y, x + width, y)
            x += width

    def _on_corner_paint(self, event):
        if self._aggregations:
            dc = wx.PaintDC(self._grid.GetGridCornerLabelWindow())
            dc.SetTextForeground(wx.BLACK)
            dc.SetBrush(wx.Brush('GRAY', wx.BRUSHSTYLE_TRANSPARENT))
            width = dc.GetSize().GetWidth()
            row_height = self._row_height
            y = self._label_height
            for op, title, icon_id, label in self._AGGREGATIONS:
                if op in self._aggregations:
                    rect = (1, y - 1, width, row_height + 1)
                    dc.DrawRectangle(*rect)
                    dc.DrawLabel(title, rect, wx.ALIGN_CENTER)
                    y += row_height
            dc.DrawLine(0, y, width, y)

    def _on_corner_left_down(self, event):
        # Suppress the default behavior - selection of all grid cells.
        pass

    def _on_corner_right_down(self, event):
        self._run_callback(self.CALL_USER_INTERACTION)
        if event.GetY() > self._label_height:
            menu = self._aggregation_menu()
        else:
            menu = (
                MenuItem(_("Hide row headings"), command=Command(ListForm.toggle_row_labels)),
            )
        self._popup_menu(menu)
        event.Skip()

    def _on_right_click(self, event):
        self._run_callback(self.CALL_USER_INTERACTION)
        row, col = event.GetRow(), event.GetCol()
        self._select_cell(row, col)
        self._grid.Refresh()
        self.context_menu(position=event.GetPosition())

    def _on_left_click(self, event):
        self._run_callback(self.CALL_USER_INTERACTION)
        if event.ShiftDown() or event.ControlDown():
            # Allow default behavior (clicked row selection) only when the user
            # wants to make a selection.
            event.Skip()
        else:
            # Otherwise only move the cursor to the clicked cell.
            row, col = event.GetRow(), event.GetCol()
            self._select_cell(row, col)
            self.focus()

    def _on_wheel(self, event):
        g = self._grid
        delta = event.GetWheelDelta()
        linesPer = event.GetLinesPerAction()
        pxx, pxy = g.GetScrollPixelsPerUnit()
        rot = event.GetWheelRotation()
        lines = rot // delta
        if lines != 0:
            vsx, vsy = g.GetViewStart()
            lines = lines * linesPer
            scrollTo = vsy - pxy // lines
            g.Scroll(-1, scrollTo)

    def _update_list_position(self):
        row = self._current_cell()[0]
        total, finished = self._table.number_of_rows(timeout=0, full_result=True)
        if not finished:
            total = '%s?' % (total,)
        self._list_position = "%d/%s" % (row + 1, total)

    def list_position(self):
        # list_position() may be called on idle from application.py
        # _refresh_list_position() before the form is fully initialized.
        # Note that BrowsableShowForm defines the same method, but
        # The _list_position attribute is managed differently and
        # there is no common base class that recognizes _list_position.
        return getattr(self, '_list_position', None)

    def _update_data_status(self):
        if self._reshuffle_request > self._last_reshuffle_request:
            self._data_changed = False
        else:
            self._data_changed = True
        # TODO: the status is currently unused but it was designed
        # to be displayed in a status bar field.

    def _initial_select_row(self):
        if self._select_row_argument is not None:
            self.select_row(self._select_row_argument)
        else:
            self.select_row(0, quiet=True)

    def select_row(self, position, **kwargs):
        # Během editace může `position' obsahovat nevyhledatelná data.
        if ((isinstance(position, int) and
             position < self._table.number_of_rows(min_value=(position + 1)))):
            # Pro číslo voláme rovnou _select_cell a nezdržujeme se převodem na
            # row a zpět, který probíhá v rodičovské metodě...
            self._select_cell(row=position)
            return True
        else:
            return super(ListForm, self).select_row(position, **kwargs)

    def _select_row(self, row):
        if row:
            row_number = self._get_row_number(row)
        else:
            row_number = -1
        if row_number is not None:
            self._select_cell(row=row_number)
            return True
        else:
            return False

    def _refresh(self, interactive=False):
        if __debug__:
            log(DEBUG, 'Refresh request')
        if interactive:
            self._apply_providers()
        self._last_reshuffle_request = self._reshuffle_request = time.time()
        self._update_grid(data_init=True, retain_row=True)
        self._update_data_status()
        self._print_menu_ = UNDEFINED
        return True

    def _update_colors(self):
        # Wx only supports highlighting of the current cell, not the current
        # row.  Thus we highlight the current row ourselves in
        # CustomCellRenderer (defined in _grid.py).
        if pytis.config.cell_highlight_color is not None:
            self._grid.SetCellHighlightColour(pytis.config.cell_highlight_color)
        if pytis.config.grid_line_color is not None:
            self._grid.SetGridLineColour(pytis.config.grid_line_color)

    def _resize_columns(self, size=None):
        # Recompute column widths to fit the current form size.
        g = self._grid
        if size is None:
            size = g.Parent.GetClientSize()
        available_width = size.width - g.GetRowLabelSize()
        flexible_columns = []
        flexible_width = 0
        try:
            # The batch here is important to prevent major slowdown in large
            # tables, where SetColumn outside batch results in calling GetAttr
            # for *all* table cells since wxPython 4.1.0.
            g.BeginBatch()
            for i, column in enumerate(self._columns):
                column_width = self._ideal_column_width(column)
                if not column.fixed() and pytis.config.stretch_tables:
                    flexible_columns.append((i, column_width))
                    flexible_width += column_width
                else:
                    available_width -= column_width
                g.SetColSize(i, column_width)
            if flexible_columns and available_width > flexible_width > 0:
                coef = float(available_width) / float(flexible_width)
                for i, column_width in flexible_columns:
                    if i == flexible_columns[-1][0]:
                        # Avoid rounding inaccuracy shift for the last column.
                        column_width = max(0, available_width)
                    else:
                        column_width = int(column_width * coef)
                    g.SetColSize(i, column_width)
                    available_width -= column_width
        finally:
            g.EndBatch()

    def _on_size(self, event):
        size = event.GetSize()
        if size.width != self._grid.Size.width:
            self._resize_columns(size)
        event.Skip()

    def _cleanup(self):
        super(ListForm, self)._cleanup()
        if self._grid is not None:
            # Musíme tabulce zrušit datový objekt, protože jinak do něj bude šahat
            # i po kompletním uzavření starého gridu (!!) a rozhodí nám tak data
            # v novém gridu.
            if self._table is not None:
                self._table.close()

    def _cleanup_data(self):
        self._data.remove_callback_on_change(self.on_data_change)
        super(ListForm, self)._cleanup_data()

    # Commands

    @Command.define
    def delete_record(self):
        with Refreshable.block_refresh():
            result = super(ListForm, self).delete_record()
        if result:
            r = self._current_cell()[0]
            if r < self._table.number_of_rows(min_value=(r + 2)) - 1:
                self._select_cell(row=r)
            elif r > 0:
                self._select_cell(row=(r - 1))
            # Uděláme raději refresh celé aplikace, protože jinak se
            # nerefreshne horní formulář po vymazání záznamu ze sideformu.
            app.refresh()

    @Command.define
    def activate(self, alternate=False):
        self._run_callback(self.CALL_ACTIVATION, alternate=alternate)

    @Command.define
    def first_column(self):
        self._select_cell(col=0)

    @Command.define
    def last_column(self):
        self._select_cell(col=(len(self._columns) - 1))

    @Command.define
    def move_column(self, diff=1):
        col = self._grid.GetGridCursorCol()
        newcol = col + diff
        if 0 <= newcol < len(self._columns):
            c = self._columns[col]
            self._update_grid(delete_column=c, insert_column=c,
                              inserted_column_index=newcol)
            self._select_cell(col=newcol)
        else:
            log(OPERATIONAL, "Invalid column move command:", (col, newcol))

    def _can_move_column(self, diff=1):
        col = self._grid.GetGridCursorCol()
        return 0 <= col + diff < len(self._columns)

    @Command.define
    def toggle_column(self, column_id, position=None):
        c = find(column_id, self._columns, key=lambda c: c.id())
        if c:
            self._update_grid(delete_column=c)
        else:
            self._update_grid(insert_column=self._view.field(column_id),
                              inserted_column_index=position)

    @Command.define
    def toggle_columns(self):
        columns = self._available_columns()
        result = pytis.form.app.run_dialog(
            CheckListDialog, title=_("Displayed columns"),
            message=_("Select the columns to be displayed:"),
            items=[(c in self._columns, c.column_label()) for c in columns]
        )
        if result:
            for i, (c, checked) in enumerate(zip(columns, result)):
                present = c in self._columns
                if present and not checked:
                    self._update_grid(delete_column=c)
                elif not present and checked:
                    self._update_grid(insert_column=self._view.field(c.id()),
                                      inserted_column_index=result[:i].count(True))

    @Command.define
    def toggle_row_labels(self):
        g = self._grid
        if g.GetRowLabelSize() == 0:
            widht = self._ROW_LABEL_WIDTH
        else:
            widht = 0
        g.SetRowLabelSize(widht)
        g.FitInside()
        self.refresh()

    @Command.define
    def resize_column(self, diff=5):
        # diff can be positive or negative integer in pixels.
        g = self._grid
        col = g.GetGridCursorCol()
        newsize = g.GetColSize(col) + diff
        if newsize > 0:
            g.SetColSize(col, newsize)
            g.SetSize(g.GetSize())
            g.Refresh()
            self._remember_column_width(col)

    @Command.define
    def sort(self, col=None, direction=None, primary=False):
        if col is not None:
            col = self._columns[col].id()
        old_sorting = self._lf_sorting
        sorting = super(ListForm, self).sort(col=col, direction=direction, primary=primary)
        if sorting is not None and sorting != old_sorting:
            try:
                # Update grouping first.
                cols = self._sorting_columns()
                minlen = min(len(cols), len(self._grouping))
                self._grouping = tuple(cols[:minlen])
                self._lf_sorting = sorting
                # Make the changes visible.
                self._refresh()
                # Force to count at least one line.  This is to allow user
                # break in case the first cursor operation is very slow.  We
                # must do it here, otherwise the delay happens later in
                # _on_idle where we can't handle it.
                self._table.number_of_rows(min_value=1)
            except UserBreakException:
                self._lf_sorting = old_sorting
                cols = self._sorting_columns()
                minlen = min(len(cols), len(self._grouping))
                self._grouping = tuple(cols[:minlen])
                # Make the changes visible.
                self._refresh()
        return sorting

    def _can_sort(self, **kwargs):
        col = kwargs.get('col')
        if col is not None:
            kwargs['col'] = self._columns[col].id()
        return super(ListForm, self)._can_sort(**kwargs)

    @Command.define
    def toggle_aggregation(self, operation):
        if operation in self._aggregations:
            command = self.unaggregate
        else:
            command = self.aggregate
        command(operation=operation)

    @Command.define
    def aggregate(self, operation=None):
        if operation is None:
            self._aggregations = [op for op, title, icon, label in self._AGGREGATIONS]
        else:
            self._aggregations.append(operation)
        self._update_label_height()
        self.refresh()

    def _can_aggregate(self, operation=None):
        if operation is None:
            return len(self._aggregations) != len(self._AGGREGATIONS)
        else:
            return operation not in self._aggregations

    @Command.define
    def unaggregate(self, operation=None):
        if operation is None:
            self._aggregations = []
        else:
            self._aggregations.remove(operation)
        self._update_label_height()
        self.refresh()

    def _can_unaggregate(self, operation=None):
        if operation is None:
            return bool(self._aggregations)
        else:
            return operation in self._aggregations

    def _available_aggregations(self):
        return ([(op, title) for op, title, icon, label in self._AGGREGATIONS] +
                [(pytis.data.Data.AGG_COUNT, _("Count"))])

    @Command.define
    def aggregated_view(self, aggregated_view_id):
        grouping_functions = self._view.grouping_functions()
        for v in self._view.aggregated_views():
            if v.id() == aggregated_view_id:
                name = v.name()
                group_by_columns = v.group_by_columns()
                aggregation_columns = v.aggregation_columns()
                break
        else:
            manager = pytis.form.app.aggregated_views_manager
            if aggregated_view_id:
                view = manager.load(self._name, aggregated_view_id)
                name = view.name()
                group_by_columns = view.group_by_columns()
                aggregation_columns = view.aggregation_columns()
            else:
                name = ''
                group_by_columns = ()
                aggregation_columns = ()
            result = pytis.form.app.run_dialog(
                AggregationSetupDialog,
                aggregation_functions=self._available_aggregations(),
                grouping_functions=grouping_functions,
                columns=[(c.id(), c.label(), c.type())
                         for c in self._lf_sfs_columns()],
                aggregation_valid=self._aggregation_valid,
                name=name, group_by_columns=group_by_columns,
                aggregation_columns=aggregation_columns,
            )
            if result is None:
                return
            name, group_by_columns, aggregation_columns = result
            if name:
                if aggregated_view_id is None:
                    prefix = 'user-'
                    numbers = [int(aid[len(prefix):]) for aid in manager.list(self._name)
                               if aid.startswith(prefix) and aid[len(prefix):].isdigit()]
                    aggregated_view_id = prefix + str(max(numbers + [0]) + 1)
                aggregated_view = AggregatedView(aggregated_view_id,
                                                 name, group_by_columns, aggregation_columns)
                manager.save(self._name, aggregated_view)
        # Compose the aggregated data object inner condition from the
        # current user filter and the hardcoded condition from
        # specification.
        condition = self._current_condition()
        arguments = self._current_arguments()
        spec_condition = self._data.condition()
        if spec_condition:
            if condition:
                condition = pytis.data.AND(condition, spec_condition)
            else:
                condition = spec_condition
        run_form(pytis.form.AggregationDualForm, self._name,
                 aggregated_view_name=name,
                 group_by_columns=group_by_columns,
                 grouping_functions=grouping_functions,
                 aggregation_columns=aggregation_columns,
                 aggregation_condition=condition,
                 aggregation_arguments=arguments)

    @Command.define
    def delete_aggregated_view(self, aggregated_view_id):
        pytis.form.app.aggregated_views_manager.drop(self._name, aggregated_view_id)

    @Command.define
    def filter_by_cell(self):
        row, col = self._current_cell()
        id = self._columns[col].id()
        value = self._table.row(row)[id]
        self.filter_by_value(column_id=id, value=value)

    def _can_filter_by_cell(self):
        row, col = self._current_cell()
        return self._data.find_column(self._columns[col].id()) is not None

    @Command.define
    def autofilter(self, col=None, position=None):
        busy_cursor(True)
        try:
            if col is None:
                col = self._current_cell()[1]
            cid = self._columns[col].id()
            if not self._data.permitted(cid, pytis.data.Permission.VIEW):
                return
            ctype = self._data.find_column(self._columns[col].id()).type()
            cond = pytis.data.AND(
                self._current_condition(),
                pytis.data.NE(cid, pytis.data.Value(ctype, None)))
            arguments = self._current_arguments()
            limit = 60
            distinct = self._autofilter_values(cid, limit, condition=cond, arguments=arguments)
            if len(distinct) > limit:
                app.echo(_("Too many items for autofilter."), kind='error')
                return
            items = [MenuItem(v.export(),
                              command=Command(ListForm.filter_by_value, column_id=cid, value=v))
                     for v in distinct]
        finally:
            busy_cursor(False)
        self._popup_menu(items, position=position)

    def _autofilter_values(self, cid, limit, **kwargs):
        return self._data.distinct(cid, **kwargs)

    def _can_autofilter(self, col=None, position=None):
        if col is None:
            col = self._current_cell()[1]
        return self._data.find_column(self._columns[col].id()) is not None

    @Command.define
    def filter_contained(self, name, column_id, my_column_id, binding=None, not_in=False):
        if self._current_profile_changed():
            asave, aquit = _("Save"), _("Cancel")
            if app.question(_("Can not filter as long as the current profile is unsaved!"),
                            answers=(asave, aquit), default=asave) != asave:
                return
            self.update_profile()
        if self._current_profile.id() == self._default_profile.id():
            profile_id = None
        else:
            profile_id = self._current_profile.id()
        condition = make_in_operator(column_id, self.name(), my_column_id, profile_id)
        if not_in:
            select_row = None
            condition = pytis.data.NOT(condition)
        else:
            row = self.current_row()
            select_row = {column_id: row[my_column_id]}
        if binding:
            form_class = pytis.form.MultiBrowseDualForm
            kwargs = dict(binding=binding)
        else:
            form_class = BrowseForm
            kwargs = {}
        run_form(form_class, name, select_row=select_row, filter=condition, **kwargs)

    def _can_filter_contained(self, name, *args, **kwargs):
        return app.has_access(name) and self._current_profile.id() != '__constructor_profile__'

    @Command.define
    def context_menu(self, position=None):
        menu = self._context_menu()
        if menu:
            if position is None:
                g = self._grid
                row, col = self._current_cell()
                rect = g.CellToRect(row, col)
                pos = (rect.GetX() + rect.GetWidth() // 3,
                       rect.GetY() + rect.GetHeight() // 2 + g.GetColLabelSize())
                position = self._grid.CalcScrolledPosition(pos)
            self._popup_menu(menu, position=position)

    def _can_context_menu(self, position=None):
        return self._grid.IsSelection() or self._table.current_row() is not None

    def _popup_menu(self, items, position=None):
        pytis.form.app.popup_menu(self._grid, items, keymap=self._get_keymap(), position=position)

    @Command.define
    def set_grouping_column(self, col=None):
        if col is not None:
            cid = self._columns[col].id()
            pos = self._sorting_position(cid)
            if pos is not None:
                cols = self._sorting_columns()
                self._grouping = tuple(cols[:pos + 1])
            else:
                log(OPERATIONAL, "Invalid grouping column:", cid)
                return
        else:
            self._grouping = ()
        self._update_grid()

    def _can_set_grouping_column(self, col=None):
        if col is not None:
            return self._columns[col].id() in self._sorting_columns()
        else:
            return bool(self._grouping)

    @Command.define
    def incremental_search(self, full=False, prefill=None):
        row, col = self._current_cell()
        col_id = self._columns[col].id()
        if ((not isinstance(self._row.type(col_id), pytis.data.String) or
             not self._data.permitted(col_id, pytis.data.Permission.VIEW))):
            app.echo(_("Unable to search incrementally in this column."), kind='error')
            return
        if self._search_panel is None:
            self._create_search_panel(full=full, prefill=prefill)
        else:
            self._search_panel_controls[1].SetFocus()
        # self._selection_callback = self.get_callback(self.CALL_SELECTION)
        # self.set_callback(self.CALL_SELECTION, None)

    @Command.define
    def search(self, next=False, back=False):
        if next and self._search_panel is not None:
            self._incremental_search(direction=pytis.data.BACKWARD
                                     if back else pytis.data.FORWARD)
        else:
            return super(ListForm, self).search(next=next, back=back)

    @Command.define
    def copy_cell(self):
        row, col = self._current_cell()
        presented_row = self._table.row(row)
        if presented_row:
            cid = self._columns[col].id()
            copy_to_clipboard(presented_row.format(cid, secure=True))

    @Command.define
    def copy_aggregation_result(self, operation, cid):
        copy_to_clipboard(self._aggregation_results[(cid, operation)].export())

    def _can_copy_aggregation_result(self, operation, cid):
        return self._aggregation_results[(cid, operation)] is not None

    @Command.define
    def export_file(self):
        log(EVENT, 'Called export to file')
        if not all(self._data.permitted(c.id(), pytis.data.Permission.EXPORT)
                   for c in self._columns):
            problem = _("You don't have permissions to export this table.")
        else:
            number_of_rows = self._table.number_of_rows()
            if number_of_rows == 0:
                problem = _("The table has no rows!")
            elif number_of_rows > 100000:
                problem = _("The table has too many rows! Use a filter.")
            else:
                problem = None
        if problem:
            app.warning(problem + '\n' + _("Export aborted."))
            return
        import pkgutil
        xls_available = pkgutil.find_loader('xlsxwriter') is not None
        selection_active = self._grid.IsSelection()
        answers = app.input_form(title=_("Export options"), fields=(
            Field('scope', _("Scope"), enumerator=('all', 'selection'),
                  default='selection' if selection_active else 'all',
                  not_null=True, selection_type=SelectionType.RADIO,
                  editable=Editable.ALWAYS if selection_active else Editable.NEVER,
                  display=lambda x: (_("All rows") if x == 'all' else
                                     _("Selected rows only")),
                  descr=_("Select the extent of rows to be exported.")),
            Field('format', _("File format"), enumerator=('XLSX', 'CSV'),
                  default='CSV', not_null=True, selection_type=SelectionType.RADIO,
                  descr=_("Choose the export file format")),
            Field('action', _("Action"), enumerator=('save', 'launch'),
                  default='save', not_null=True, selection_type=SelectionType.RADIO,
                  display=lambda x: (_("Save to disk") if x == 'save' else
                                     _("Open in spreadsheet")),
                  descr=_("Select what to do with the exported file")),
        ), check=(lambda r: _("XLSX support not installed.  Ask your administrator.")
                  if r['format'].value() == 'XLSX' and not xls_available else None))
        if not answers:
            return
        scope, fileformat, action = (answers['scope'].value(), answers['format'].value(),
                                     answers['action'].value())

        log(ACTION, "Export action:", (self.name(), self._form_name(), pytis.config.dbschemas,
                                       "Filter: %s" % str(self._lf_filter),
                                       scope, fileformat, action))
        suffix = '.' + fileformat.lower()
        if action == 'save':
            if fileformat == 'XLSX':
                filetypes = ('xlsx',)
            else:
                filetypes = ('txt', 'csv')
                suffix = '.txt'
            filename = 'export'
            if not pytis.remote.client_available():
                filename += '-' + pytis.config.dbconnection.user()
            try:
                export_file = app.make_selected_file(filename=filename + suffix,
                                                     mode='wb', filetypes=filetypes,
                                                     context='export')
                if not export_file:
                    return
            except (IOError, OSError):
                app.error(_("Unable to open the file for writing!"))
                return
            after_export = None
        else:
            cmode = pytis.form.app.client_mode()
            if cmode == 'remote':
                export_file = pytis.remote.make_temporary_file(mode='wb', suffix=suffix)
                after_export = pytis.remote.launch_file
            elif cmode == 'local':
                export_file = tempfile.NamedTemporaryFile(mode='wb', suffix=suffix)
                after_export = app.launch_file
            else:
                return
        if fileformat == 'XLSX':
            export_method = self._export_xlsx
        else:
            export_method = self._export_csv
        try:
            # We prefer exporting all data into memory and write to the file in the
            # end in order to prevent numerous rpc calls in case of remote export.
            data = app.run(export_method, kwargs=dict(only_selected=(scope == 'selection')))
            pytis.form.app.log_user_action(self._name, self.__class__.__name__, 'export', dict(
                condition=str(self._current_condition()) if self._current_condition() else None,
                columns=[c.id() for c in self._columns],
                total_rows=self._table.number_of_rows(),
                selected_rows=len(self._grid.GetSelectedRows()) if scope == 'selection' else None,
                filename=export_file.name,
                size=len(data),
            ))
            export_file.write(data)
            if after_export:
                export_file.flush()
                after_export(export_file.name)
        finally:
            export_file.close()

    def _export_csv(self, update, only_selected=False):
        columns = [(c.id(), (dict(locale_format=False)
                             if isinstance(self._row.type(c.id()), pytis.data.Float)
                             else dict()))
                   for c in self._columns]
        number_of_rows = self._table.number_of_rows()
        result = '\t'.join(c.column_label() for c in self._columns) + '\n'
        for r in range(0, number_of_rows):
            if not update(int(float(r) / number_of_rows * 100)):
                break
            if only_selected and not self._grid.IsInSelection(r, 0):
                continue
            presented_row = self._table.row(r)
            result += '\t'.join(
                ';'.join(presented_row.format(cid, secure=True, **kwargs).splitlines())
                for cid, kwargs in columns
            ) + '\n'
        try:
            encoded = result.encode(pytis.config.export_encoding)
        except (LookupError, UnicodeEncodeError) as e:
            msg = (_("Encoding %s not supported.", pytis.config.export_encoding)
                   if isinstance(e, LookupError) else
                   _("Unable to encode data to %s.", pytis.config.export_encoding))
            app.error(msg + '\n' + _("Using UTF-8 instead."))
            encoded = result.encode('utf-8')
        return encoded

    def _export_xlsx(self, update, only_selected=False):
        def writefunc(cid, ctype, vfunc=pytis.util.identity):
            """Return a closure writing formatted value of given column to the spreadcheet.

            Returns a function of arguments (x, y, r, v), where:
              x -- worksheet column number
              y -- worksheet row number
              r -- PresentedRow instance representing the current row
              v -- internal Python value of the worksheet cell

            This function allows to perform all decision making once and reuse the created
            closure repeatedly during export for all rows.  One closure is created for each
            spreadsheet column according to its data type.

            """
            if isinstance(ctype, (pytis.data.Float, pytis.data.Integer)):
                if isinstance(ctype, pytis.data.Integer):
                    num_format = '0'
                else:
                    precision = ctype.precision()
                    if precision and precision > 0:
                        num_format = '0.' + '0' * precision
                    else:
                        num_format = 'General'
                fmt = writer.add_format({'num_format': num_format})
                return lambda x, y, r, v: worksheet.write_number(x, y, vfunc(v), fmt)
            elif isinstance(ctype, pytis.data.DateTime) and not isinstance(ctype, pytis.data.Range):
                if isinstance(ctype, pytis.data.Date):
                    num_format = 'dd/mm/yyyy'
                elif isinstance(ctype, pytis.data.Time):
                    num_format = 'hh:mm:ss'
                else:
                    num_format = 'dd/mm/yyyy hh:mm:ss'
                fmt = writer.add_format({'num_format': num_format, 'align': 'left'})
                return lambda x, y, r, v: worksheet.write_datetime(x, y, vfunc(v), fmt)
            elif vfunc != pytis.util.identity and isinstance(ctype, pytis.data.String):
                # Special case for String range fields (does that ever happen?).
                return lambda x, y, r, v: worksheet.write(x, y, vfunc(v))
            else:
                return lambda x, y, r, v: \
                    worksheet.write_string(x, y, ';'.join(r.format(cid, secure=True).split('\n')))

        import xlsxwriter
        MINIMAL_COLUMN_WIDTH = 12
        columns = []
        output = io.BytesIO()
        writer = xlsxwriter.Workbook(output, {'remove_timezone': True})
        if pytis.config.xlsx_export_author:
            writer.set_properties({"author": pytis.config.xlsx_export_author})
        worksheet = writer.add_worksheet('Export')
        # Setup columns
        bold = writer.add_format({'bold': True})
        bold_centered = writer.add_format({'align': 'center', 'bold': True})
        for col in self._columns:
            cid = col.id()
            position = len(columns)
            ctype = self._row.type(cid)
            label = unistr(col.column_label() or col.label())
            width = max(col.width(), len(label), MINIMAL_COLUMN_WIDTH)
            columns.append((position, cid, writefunc(cid, ctype)))
            worksheet.write(0, position, label, bold)
            worksheet.set_column(position, position, width)
        # Process rows
        number_of_rows = self._table.number_of_rows()
        output_row_number = 2  # Actually start at third row (one for headings, one empty).
        for r in range(0, number_of_rows):
            if not update(int(float(r) / number_of_rows * 100)):
                break
            if only_selected and not self._grid.IsInSelection(r, 0):
                continue
            presented_row = self._table.row(r)
            for position, cid, write in columns:
                value = presented_row.get(cid, secure=True)
                if value and value.value() is not None:
                    write(output_row_number, position, presented_row, value.value())
            output_row_number += 1
        writer.close()
        return output.getvalue()

    @Command.define
    def clear_selection(self):
        self.unselect_selected_rows()

    def _can_clear_selection(self):
        return self._grid.IsSelection()

    @Command.define
    def add_row_to_selection(self):
        self._grid.SelectRow(self._grid.GetGridCursorRow(), True)

    def _can_add_row_to_selection(self):
        return not self._grid.IsInSelection(self._grid.GetGridCursorRow(), 0)

    @Command.define
    def remove_row_from_selection(self):
        self._grid.DeselectRow(self._grid.GetGridCursorRow())

    def _can_remove_row_from_selection(self):
        return self._grid.IsInSelection(self._grid.GetGridCursorRow(), 0)

    # Public methods

    def on_key_down(self, event, dont_skip=True):
        if self._search_panel is None:
            # Running this callback in dual-form would lead to focus loss on Shift, Ctrl etc.
            self._run_callback(self.CALL_USER_INTERACTION)
        if KeyHandler.on_key_down(self, event, dont_skip=dont_skip):
            return True
        event.Skip()
        return False

    def focus(self):
        super(ListForm, self).focus()
        self._update_list_position()
        self._update_data_status()
        self._grid.SetFocus()
        self._grid.Refresh()

    def defocus(self):
        super(ListForm, self).defocus()
        self._grid.Refresh()


class FoldableForm(ListForm):
    """Form able to expand or collapse sets of rows.

    Its typical use is for tree structured data such as menus or file system
    objects.

    The folding facilities are automatically enabled when a column of type
    'PrettyFoldable' is present.  At most one such column may be present.  The
    tree order column referred by the PrettyFoldable column must have unique
    not-NULL values (typically it is the primary key in the corresponding
    database table).

    """

    class _FoldingState(Structure):
        _attributes = (Attribute('level'),
                       Attribute('subnodes', dict),
                       )

    class Folding(object):
        """Representation of current folding state.

        Initial folding level can be specified in the constructor.  Then
        folding can be changed using 'expand()' method.

        """
        def __init__(self, level=1):
            """
            Arguments:

              level -- initial folding level, integer (0=everything folded,
                1=single level unfolded, etc.) or 'None' (everything unfolded)

            """
            self._folding = FoldableForm._FoldingState(level=level, subnodes={})

        def __eq__(self, other):
            if pytis.util.sameclass(self, other):
                return self._folding == other._folding
            else:
                return NotImplemented

        def __ne__(self, other):
            # Implied automatically in Python 3 so can be removed when dropping Python 2 support.
            return not self == other

        def _find_node(self, node):
            state = self._folding
            labels = (node or '').split('.')
            while labels and state is not None:
                label = labels.pop(0)
                state = state.subnodes().get(label)
            return state

        def _folding_level(self, node):
            state = self._folding
            labels = (node or '').split('.')
            while labels and state is not None:
                level = state.level()
                label = labels.pop(0)
                state = state.subnodes().get(label)
            if state is None:
                if level is None:
                    folding_level = None
                else:
                    folding_level = max(level - len(labels) - 1, 0)
            else:
                folding_level = state.level()
            return folding_level

        def _expand(self, node, level):
            def fold(state, path, level):
                state_level = state.level()
                if path:
                    next_key = path[0]
                    state_subnodes = state.subnodes()
                    if state_level is None and level is None:
                        if next_key in state_subnodes:
                            new_state = state.copy(level=state_level,
                                                   subnodes=copy.copy(state_subnodes))
                            del new_state.subnodes()[next_key]
                        else:
                            new_state = state
                    else:
                        new_state = state.copy(level=state_level,
                                               subnodes=copy.copy(state_subnodes))
                        next_state = new_state.subnodes().get(next_key)
                        if next_state is None:
                            if state_level:
                                next_level = state_level - 1
                            else:  # state_level == 0 or state_level == None -> inherit it
                                next_level = state_level
                            next_state = FoldableForm._FoldingState(level=next_level, subnodes={})
                        new_next_state = fold(next_state, path[1:], level)
                        new_state.subnodes()[next_key] = new_next_state
                        if state_level is None:
                            redundant_level = None
                        else:
                            redundant_level = state_level - 1
                        for key, key_state in new_state.subnodes().items():
                            if not key_state.subnodes() and key_state.level() == redundant_level:
                                del new_state.subnodes()[key]
                else:
                    if state_level == level:
                        new_state = state
                    else:
                        new_state = state.copy(level=level, subnodes={})
                return new_state
            path = (node or '').split('.')
            state = self._folding
            new_state = fold(state, path, level)
            self._folding = new_state
            return state is not new_state

        def expand(self, node, level=None):
            """Set folding level of 'node' to 'level'.

            Arguments:

              node -- tree column value of the node
              level -- new folding level of the node, integer or 'None', see
                '__init__()' for more details

            """
            # level==None => expand whole subtree
            # level==0 => collapse whole subtree
            # level==1 => expand just the next level
            # level==N, N>0 => expand up to level N
            return self._expand(node, level)

        def collapse(self, node):
            return self._expand(node, 0)

        def expand_or_collapse(self, node, level):
            if self._folding_level(node) == 0:
                result = self.expand(node, level=level)
            else:
                result = self.collapse(node)
            return result

        def condition(self, column_id):
            queries = []

            def add(path, state):
                state_level = state.level()
                subnodes = state.subnodes()
                if subnodes:
                    if path:
                        queries.append(path[:-1])
                    if state_level != 0:
                        q = path + '!' + '|'.join(subnodes.keys()) + '.*'
                        if state_level is not None:
                            q = '%s{0,%d}' % (q, state_level - 1,)
                        queries.append(q)
                    for label, next_state in subnodes.items():
                        next_path = path + label + '.'
                        add(next_path, next_state)
                else:
                    q = path + '*'
                    if state_level is not None:
                        q = '%s{0,%d}' % (q, state_level,)
                    queries.append(q)
            add('', self._folding)
            condition = pytis.data.OR(*[pytis.data.LTreeMatch(column_id, q) for q in queries])
            return condition

        def level(self, node):
            return self._folding_level(node)

        def folding_state(self):
            def transform(folding):
                level = folding.level()
                subnodes = [[k, transform(v)] for k, v in folding.subnodes().items()]
                return [level, subnodes]
            return transform(self._folding)

        def set_folding_state(self, state):
            def transform(state):
                level, subnodes_state = state
                subnodes = [(k, transform(v),) for k, v in subnodes_state]
                return FoldableForm._FoldingState(level=level, subnodes=dict(subnodes))
            self._folding = transform(state)

    def _full_init(self, *args, **kwargs):
        self._folding_column_id = None
        super(FoldableForm, self)._full_init(*args, **kwargs)
        self._folding_column_id = self._find_folding_column()
        if self._folding_column_id is not None:
            # Make the current row visible (unfolded)
            node = self._row[self._folding_column_id].value()
            path = (node or '').split('.')
            if len(path) > 1:
                parent = '.'.join(path[:-1])
                level = self._folding.level(parent)
                if level == 0:
                    self._folding.expand(parent, level=1)
        # Any better way to display the form with initial folding than to
        # refresh it?
        self._refresh_folding()

    def _init_folding(self, folding_state=None):
        self._folding = self._default_folding()
        if folding_state is None:
            folding_state = self._view.folding()
        if folding_state is not None:
            self._folding.set_folding_state(folding_state)

    def _default_folding(self):
        return self.Folding()

    def _find_folding_column(self):
        if self._folding_column_id is not None:
            return self._folding_column_id
        folding_column_id = None
        for c in self._data.columns():
            if isinstance(c.type(), PrettyFoldable):
                folding_column_id = c.type().tree_column_id()
                break
        return folding_column_id

    def _apply_profile_parameters(self, profile):
        super(FoldableForm, self)._apply_profile_parameters(profile)
        folding_state = profile.folding()
        if folding_state is None and profile.filter():
            # Don't interpret None as the default folding, but as unfolded,
            # because the default (initial) folding would conflict with
            # filters.
            folding_state = self.Folding(level=None).folding_state()
        self._init_folding(folding_state)

    def _profile_parameters_to_save(self):
        return dict(super(FoldableForm, self)._profile_parameters_to_save(),
                    folding=self._folding.folding_state())

    def _profile_parameter_changed(self, param, current_value, original_value):
        if ((param == 'folding' and self._current_profile.filter() and
             not self._current_profile.is_user_defined_profile())):
            # Avoid indication of changed profile in case of system profiles
            # with filter and no folding in specifications with initial
            # 'folding' defined.  The default folding is actually different
            # depending on the profile's filter as implemented in
            # _apply_profile_parameters().
            original_value = self.Folding(level=None).folding_state()
        return super(FoldableForm, self)._profile_parameter_changed(param, current_value,
                                                                    original_value)

    def _apply_filter(self, condition):
        def is_in(operator):
            if operator is None:
                return False
            elif operator.name() in ('AND', 'OR', 'NOT'):
                for op in operator.args():
                    if is_in(op):
                        return True
                return False
            elif operator.name() == 'IN':
                return True
            else:
                return False
        if self._folding_enabled() and is_in(condition):
            if not app.question(_("You attempted to use a filter with the IN operator\n"
                                  "on a foldable form. This operation may potentially\n"
                                  "take several minutes. You can greatly speed up the\n"
                                  "operation by turning the folding off (sorting by\n"
                                  "another column).\n"
                                  "Really continue?"), default=True):
                # This should abort profile selection in apply_profile() and
                # return the previously selected profile.
                raise UserBreakException()
        if condition is not None and condition != self._lf_filter:
            self._folding = self.Folding(level=None)
        return super(FoldableForm, self)._apply_filter(condition)

    def _default_sorting(self):
        sorting = self._view.sorting()
        if sorting is None:
            folding_column_id = self._find_folding_column()
            if folding_column_id is not None:
                sorting = ((folding_column_id, pytis.data.ASCENDENT,),)
            else:
                sorting = super(FoldableForm, self)._default_sorting()
        return sorting

    def _determine_sorting(self, col, direction, primary):
        sorting = super(FoldableForm, self)._determine_sorting(col, direction, primary)
        if sorting == () and self._folding_column_id is not None:
            sorting = tuple(self._default_sorting())
        return sorting

    def _current_condition(self, filter=None, display=False):
        condition = super(FoldableForm, self)._current_condition(filter=filter, display=display)
        if display and self._folding_enabled():
            condition = pytis.data.AND(condition, self._folding.condition(self._folding_column_id))
        return condition

    def _folding_enabled(self):
        return (self._folding_column_id is not None and
                (not self._lf_sorting or
                 (len(self._lf_sorting) == 1 and
                  self._lf_sorting[0][0] == self._folding_column_id)))

    def _search(self, condition, direction, row_number=None, report_failure=True,
                initial_shift=False):
        if not self._folding_enabled():
            return super(FoldableForm, self)._search(condition, direction, row_number=row_number,
                                                     report_failure=report_failure,
                                                     initial_shift=initial_shift)
        if row_number is not None:
            self._table.rewind(position=row_number)
        orig_folding = self._folding
        self._folding = self.Folding(level=None)
        self._refresh_folding()
        if row_number is None:
            unfolded_row_number = row_number
        else:
            # self._table.current_row() returns None on the first row of a foldable tree table.
            unfolded_row_number = self._table.current_row() or 0
        try:
            result = super(FoldableForm, self)._search(condition, direction,
                                                       row_number=unfolded_row_number,
                                                       report_failure=report_failure,
                                                       initial_shift=initial_shift)
            row = self.current_row()
        finally:
            self._folding = orig_folding
        if result is None:
            self._refresh_folding()
            return None
        node = row[self._folding_column_id].value()
        if self._folding.level(node) == 0:
            self._folding.expand(node, level=0)
        self._refresh_folding()
        return super(FoldableForm, self)._search(condition, direction, row_number=row_number,
                                                 report_failure=report_failure)

    def _on_left_click(self, event):
        if self._folding_enabled():
            col = event.GetCol()
            column = self._columns[col]
            if isinstance(self._row.type(column.id()), PrettyFoldable):
                row = event.GetRow()
                value = self._table.row(row).format(column.id(), pretty=True, form=self)
                pos = value.find(PrettyFoldable.FOLDED_MARK)
                if pos == -1:
                    pos = value.find(PrettyFoldable.UNFOLDED_MARK)
                if pos != -1:
                    x1 = self._grid.GetTextExtent(value[:pos])[0]
                    x2 = self._grid.GetTextExtent(value[:pos + 1])[0]
                    x = event.GetPosition()[0] - 2  # don't count grid padding.
                    offset = self._grid.CellToRect(row, col)[0]
                    if x1 - 2 < x - offset < x2 + 2:  # enlarge active area by 2px for easier hit
                        if event.ControlDown():
                            level = None
                        else:
                            level = 1
                        self._expand_or_collapse(row, level=level)
                        return
        super(FoldableForm, self)._on_left_click(event)

    def _expand_or_collapse(self, row, level=None):
        node = self._table.row(row)[self._folding_column_id].value()
        if self._folding.expand_or_collapse(node, level=level):
            self._refresh_folding()

    def _refresh_folding(self):
        if self._folding_column_id is None:
            return
        self.refresh()

    @Command.define
    def expand_or_collapse_subtree(self, level=None):
        row = self._current_cell()[0]
        self._expand_or_collapse(row, level=level)

    def _can_expand_or_collapse_subtree(self, level=None):
        return self._folding_enabled()

    @Command.define
    def expand_or_collapse(self):
        row = self._current_cell()[0]
        self._expand_or_collapse(row, level=1)

    def _can_expand_or_collapse(self):
        return self._folding_enabled()

    @Command.define
    def expand_all(self):
        self._folding = self.Folding(level=None)
        self._refresh_folding()

    @Command.define
    def collapse_all(self):
        self._folding = self.Folding()
        self._refresh_folding()

    @Command.define
    def select_folding_level(self):
        if self._folding_enabled():
            result = app.input_form(title=_("Select folding level"), fields=(
                Field('level', _("Folding level"), width=2,
                      type=pytis.data.Integer(not_null=True, minimum=1),
                      descr=_("Enter the number denoting the folding level to fold/unfold")),
            ))
            if result:
                self._folding = self.Folding(level=result['level'].value())
                self._refresh_folding()

    def folding_level(self, row):
        """Return current folding level of 'row'.

        If 'None' is returned then the node is completely expanded.  If 0 is
        returned then the node is completely folded.  If N, N > 0, is returned
        then the node is expanded up to level N.  If an empty string is
        returned, folding is not enabled.

        Arguments:

          row -- 'Row' or 'PresentedRow' instance

        """
        if row is not None and self._folding_enabled():
            node = row[self._folding_column_id].value()
            level = self._folding.level(node)
        else:
            level = ''
        return level


class CodebookForm(PopupForm, FoldableForm, KeyHandler):
    """Formulář pro zobrazení výběrového seznamu (číselníku).

    Výběrový seznam zobrazuje řádky dat, z nichž uživatel některý řádek
    vybere.  Uživatel kromě výběru a listování nemůže s řádky nijak
    manipulovat.

    Formulář je zobrazen jako modální okno pomocí metody 'run()', která skončí
    po výběru položky a vrátí instanci PresentedRow pro vybraný řádek.  Pokud
    byl formulář ukončen jinak než výběrem záznamu, je vrácena hodnota 'None'.

    """
    DESCR = _("codebook")

    _DEFAULT_WINDOW_HEIGHT = 500
    _ALLOW_TOOLBAR = True

    def __init__(self, parent, *args, **kwargs):
        parent = self._popup_frame(parent)
        super(CodebookForm, self).__init__(parent, *args, **kwargs)

    def _full_init(self, *args, **kwargs):
        super(CodebookForm, self)._full_init(*args, **kwargs)
        self._set_real_size()
        wx_callback(wx.grid.EVT_GRID_CELL_LEFT_DCLICK, self._grid, self._on_dclick)

    def _init_attributes(self, begin_search=None, **kwargs):
        """Arguments:

          begin_search -- If not None, the form will automatically start
            incremental search on startup. If the value is a string, it is the
            column identifier in which to search.  Otherwise the search is
            performed on the column with primary sorting.

        """
        try:
            self._cb_spec = self._resolver.get(self._name, 'cb_spec')
        except ResolverError:
            self._cb_spec = CodebookSpec()
        self._begin_search = begin_search
        super(CodebookForm, self)._init_attributes(**kwargs)

    def _set_real_size(self):
        g = self._grid
        if self._folding_enabled():
            height = self._DEFAULT_WINDOW_HEIGHT
        else:
            height = g.GetColLabelSize()
            rows = g.GetNumberRows()
            if rows:
                height += rows * g.GetRowSize(0) + 50
            height = min(self._DEFAULT_WINDOW_HEIGHT, height)
        if self._search_panel:
            height += 30
        if self._query_fields_form and self._query_fields_form.IsShown():
            height += self._query_fields_form.GetSize().y
        grid_width = functools.reduce(lambda x, c: x + self._ideal_column_width(c),
                                      self._columns,
                                      g.GetRowLabelSize())
        # 660 seems to be the minimal width to make the profile selector in the toolbar visible.
        width = max(grid_width + 30, 660)
        self.SetSize((width, height))

    def _popup_frame_style(self):
        return super(CodebookForm, self)._popup_frame_style() | wx.RESIZE_BORDER

    def _current_arguments(self):
        return self._arguments

    def _default_folding(self):
        return self.Folding(level=None)

    def _on_idle(self, event):
        ListForm._on_idle(self, event)
        if not hasattr(self, '_focus_forced_to_grid'):
            self._grid.SetFocus()
            self._focus_forced_to_grid = True
        # Starting incremental on a table with just one row freezes the
        # application, thus the aditional condition below as a work around.
        if self._begin_search and self._table.number_of_rows(min_value=1) > 1:
            begin_search = self._begin_search
            self._begin_search = None
            prefill = None
            if isinstance(begin_search, basestring):
                col_id = begin_search
            elif isinstance(begin_search, tuple):
                col_id, prefill = begin_search
            else:
                cols = self._sorting_columns()
                if cols:
                    col_id = cols[0]
                else:
                    app.echo(_("Unable to search incrementally. No sortable column found!"),
                             kind='error')
            col = find(col_id, self._columns, key=lambda c: c.id())
            if col is not None:
                self._select_cell(row=0, col=self._columns.index(col))
                self.incremental_search(prefill=prefill)
                self._set_real_size()
                self.GetParent().SetSize(self.GetSize())
            else:
                log(OPERATIONAL, "Invalid search column:", col_id)

    def _default_columns(self):
        return (self._cb_spec.columns() or
                super(CodebookForm, self)._default_columns())

    def _default_sorting(self):
        sorting = self._cb_spec.sorting()
        if sorting is not None:
            return sorting
        else:
            return super(CodebookForm, self)._default_sorting()

    def _context_menu(self):
        return (MenuItem(_("Select"), command=Command(ListForm.activate)),)

    def _on_activation(self, alternate=False):
        """Nastav návratovou hodnotu a ukonči modální dialog."""
        self._result = self.current_row()
        self.Parent.EndModal(1)
        return True

    def _on_dclick(self, event):
        return self.activate()


class SelectRowsForm(CodebookForm):
    """Řádkový pop-up formulář vracející tuple všech vybraných řádků."""

    def _on_activation(self, alternate=False):
        selected_rows = tuple(self.selected_rows())
        if len(selected_rows) == 0:
            selected_rows = (self.current_row(),)
        self._result = selected_rows
        self.Parent.EndModal(1)
        return True


class BrowseForm(FoldableForm):
    """Formulář pro prohlížení dat s možností editace."""
    def _init_attributes(self, **kwargs):
        super(BrowseForm, self)._init_attributes(**kwargs)
        menu = (
            MenuItem(_("Filter by cell"),
                     command=Command(ListForm.filter_by_cell),
                     help=_("Filter the rows containing the same value in this column.")),
            MenuItem(_("Copy cell value"),
                     command=Command(ListForm.copy_cell),
                     help=_("Copy the contents of the cell into the clipboard.")),
            MenuSeparator(),
            MenuItem(_("Edit record"),
                     command=Command(BrowseForm.edit_record),
                     help=_("Open a separate edit form for this record.")),
            MenuItem(_("Copy record"),
                     command=Command(BrowseForm.new_record, copy=True),
                     help=_("Open a separate insert form with a copy of this record.")),
            MenuItem(_("Delete record"),
                     command=Command(RecordForm.delete_record),
                     help=_("Delete the record parmanently from the database.")),
            MenuItem(_("Preview form"),
                     command=Command(ListForm.activate),
                     help=_("Open the preview form for browsing all records."),
                     icon='show-record'),
            MenuItem(_("Dual preview"),
                     command=Command(ListForm.activate, alternate=True),
                     help=_("Open a dual form with a table up and preview at the bottom."),
                     icon='show-record'),
        )
        structured_text_fields = [f for f in self._fields if f.text_format() == TextFormat.LCG]
        if structured_text_fields:
            menu += (MenuSeparator(),)
        for f in structured_text_fields:
            menu += (
                MenuItem(_("Text editor for field %s") % f.label(),
                         command=Command(ListForm.open_editor, field_id=f.id()),
                         help=_("Open a structured text editor.")),
                MenuItem(_("PDF preview of %s") % f.label(),
                         command=Command(ListForm.view_field_pdf, field_id=f.id()),
                         help=_("Open PDF preview of field contents.")),
            )
        action_items = self._action_mitems(self._view.actions())
        if action_items:
            menu += (MenuSeparator(),) + tuple(action_items)
        self._context_menu_static_part = menu

        # The dynamic part of the menu is created based on the links.
        def spec_title(name, binding=None):
            if name.find('::') != -1:
                name1, name2 = name.split('::')
                title = pytis.config.resolver.get(name1, 'binding_spec')[name2].title() or \
                    ' / '.join((pytis.config.resolver.get(name1, 'view_spec').title(),
                                pytis.config.resolver.get(name2, 'view_spec').title()))
            else:
                spec = pytis.config.resolver.get(name, 'view_spec')
                title = spec.title()
                if binding is not None:
                    b = find(binding, spec.bindings(), key=lambda b: b.id())
                    assert b is not None, "Unknown binding for %s: %s" % (name, binding)
                    title += ' / ' + pytis.config.resolver.get(b.name(), 'view_spec').title()
            return title

        def link_label(title, type=FormType.BROWSE):
            mapping = {FormType.BROWSE: _("Link - %s"),
                       FormType.EDIT: _("Edit %s"),
                       FormType.VIEW: _("Show %s"),
                       FormType.INSERT: _("Insert for %s"),
                       FormType.BROWSABLE_VIEW: _("Preview %s")}
            return mapping[type] % title
        # Create links lists as accepted by _link_mitems()
        self._explicit_links = []
        automatic_links = {}
        self._explicit_in_operator_links = []
        self._automatic_in_operator_links = []
        for f in self._fields:
            fid = f.id()
            # Use explicitly defined links from specification.
            for link in f.links():
                label = link.label()
                if link.label() is None:
                    label = link_label(spec_title(link.name(), link.binding()), link.type())
                self._explicit_links.append((label, [(f, link)]))
                item = (link.name(), link.column(), f, spec_title(link.name()), link.binding())
                if item not in self._explicit_in_operator_links:
                    self._explicit_in_operator_links.append(item)
            # Create automatic links for codebook fields.
            enumerator = self._row.type(fid).enumerator()
            codebook = self._row.codebook(fid)
            if enumerator and codebook:
                if codebook in automatic_links:
                    links = automatic_links[codebook][1]
                else:
                    bindings = pytis.config.resolver.get(codebook, 'view_spec').bindings()
                    binding = bindings[0].id() if bindings else None
                    links = []
                    automatic_links[codebook] = (binding, links)
                if self._row.runtime_arguments(fid):
                    kwargs = dict(arguments=lambda row, fid=fid: row.runtime_arguments(fid))
                else:
                    kwargs = dict()
                item = (codebook, enumerator.value_column(), f, kwargs)
                if item not in links:
                    links.append(item)
                in_item = (codebook, enumerator.value_column(), f, spec_title(codebook), None)
                if in_item not in self._automatic_in_operator_links:
                    self._automatic_in_operator_links.append(in_item)
        self._automatic_links = [
            (link_label(spec_title(name)),
             [(f, Link(name, column, binding=binding_, **kwargs_))
              for name, column, f, kwargs_ in links_])
            for name, (binding_, links_) in sorted(automatic_links.items())
        ]
        self._explicit_in_operator_links.sort()
        self._automatic_in_operator_links.sort()

    def _formatter_parameters(self):
        prefix = self._name + '/'
        return {(prefix + pytis.output.P_CONDITION): pytis.data.AND(self._current_condition()),
                (prefix + pytis.output.P_ARGUMENTS): self._arguments,
                (prefix + pytis.output.P_SORTING): self._lf_sorting,
                (prefix + pytis.output.P_KEY): self._current_key(),
                (prefix + pytis.output.P_DATA): copy.copy(self._data)}

    def _action_mitems(self, spec, context=None):
        items = []
        for x in spec:
            if isinstance(x, Action):
                if context and x.context() != context:
                    continue
                items.append(MenuItem(x.title(raw=True),
                                      command=Command(self.context_action, action=x),
                                      help=x.descr()))
            elif isinstance(x, ActionGroup):
                group_actions = self._action_mitems(x.items(), context=context)
                if group_actions:
                    items.append(Menu(x.title(), group_actions))
            elif isinstance(x, (tuple, list)):
                separated_items = self._action_mitems(x, context=context)
                if items and separated_items:
                    items.append(MenuSeparator())
                items.extend(separated_items)
            else:
                raise ProgramError("Invalid action specification: %s" % x)
        return items

    def _link_mitems(self, row, linkspec):
        def mitem(f, link, row, force_title=None):
            type, name, enabled = link.type(), link.name(), link.enabled()
            if type == FormType.INSERT:
                cmd = Command(Application.new_record,
                              name=name,
                              prefill={link.column(): row[f.id()]})
                hlp = _("Insert a new record for value '%(value)s' of column %(column)s.")
                icon = 'link-new-record'
            else:
                kwargs = {}
                if type == FormType.BROWSE:
                    if name.find('::') != -1:
                        cls = pytis.form.BrowseDualForm
                    elif link.binding():
                        cls = pytis.form.MultiBrowseDualForm
                        kwargs['binding'] = link.binding()
                    else:
                        cls = BrowseForm
                    title = _("Search value of column %s") % f.label()
                    hlp = _("Search the record for value '%(value)s' of column %(column)s.")
                    filter_func = link.filter()
                    if filter_func:
                        kwargs['filter'] = filter_func(row)
                    arg_func = link.arguments()
                    if arg_func:
                        kwargs['arguments'] = arg_func(row)
                elif type == FormType.EDIT:
                    cls = PopupEditForm
                    title = _("Edit the record of column %s") % f.label()
                    hlp = _("Edit the record for value '%(value)s' of column %(column)s.")
                elif type in (FormType.VIEW, FormType.BROWSABLE_VIEW):
                    if type == FormType.BROWSABLE_VIEW:
                        cls = BrowsableShowForm
                    else:
                        cls = ShowForm
                    title = _("Show the record of column %s") % f.label()
                    hlp = _("Show the record for value '%(value)s' of column %(column)s.")
                cmd = Command(Application.run_form,
                              name=name, form_class=cls,
                              select_row={link.column(): row[f.id()]},
                              **kwargs)
                icon = 'link'
            if callable(enabled):
                enabled = enabled(row)
            if not enabled:
                cmd = Command(Application.nothing, enabled=False)
            return MenuItem(force_title or title,
                            command=cmd,
                            help=hlp % dict(value=row.format(f.id(), secure=''),
                                            column=f.column_label()),
                            icon=icon)
        items = []
        for title, links in linkspec:
            links = [(f, link) for f, link in links if row[f.id()].value() is not None]
            if len(links) == 1:
                f, link = links[0]
                items.append(mitem(f, link, row, title))
            elif len(links) != 0:
                items.append(Menu(title, [mitem(f, link, row) for f, link in links]))
        return items

    def _in_operator_mitems(self, linkspec, not_in=False):
        return [MenuItem((_('Filter "%(view_title)s" for rows not contained in '
                            'column "%(column)s" of the current form.')
                          if not_in else
                          _('Filter "%(view_title)s" for rows contained in '
                            'column "%(column)s" of the current form.')
                          ) % dict(view_title=title, column=field.column_label()),
                         command=Command(self.filter_contained, name, column_id, field.id(),
                                         binding=binding, not_in=not_in))
                for name, column_id, field, title, binding in linkspec]

    def _context_menu(self):
        if self._grid.IsSelection():
            menu = (
                MenuItem(_("Cancel selection"),
                         command=Command(ListForm.clear_selection),
                         icon='selection-cancel',
                         help=_("Cancel the selection of rows for bulk operations.")),
                MenuItem(_("Add row to selection"),
                         command=Command(ListForm.add_row_to_selection),
                         icon='selection-add',
                         help=_("Add this row to the current selection of rows for "
                                "bulk operations.")),
                MenuItem(_("Remove row from selection"),
                         icon='selection-remove',
                         command=Command(ListForm.remove_row_from_selection),
                         help=_("Remove this row from the current selection of rows "
                                "for bulk operations.")),
            )
            actions = self._action_mitems(self._view.actions(), context=ActionContext.SELECTION)
            if actions:
                menu += (MenuSeparator(),) + tuple(actions)
        else:
            menu = self._context_menu_static_part
            row = self.current_row()
            select_arguments = self._current_arguments()
            file_open_mitems = file_menu_items(self._fields, row, select_arguments)
            if file_open_mitems:
                menu += (MenuSeparator(),) + tuple(file_open_mitems)
            if self._explicit_links or self._automatic_links or self._explicit_in_operator_links \
               or self._automatic_in_operator_links:
                menu += (MenuSeparator(),)
            if self._explicit_links:
                menu += tuple(self._link_mitems(row, self._explicit_links))
            if self._automatic_links:
                menu += (Menu(_("Links"), self._link_mitems(row, self._automatic_links)),)
            if self._explicit_in_operator_links or self._automatic_in_operator_links:
                if self._explicit_in_operator_links and self._automatic_in_operator_links:
                    separator = [MenuSeparator()]
                else:
                    separator = []
                menu += (
                    Menu(_("Filter existing values (operator IN)"),
                         (self._in_operator_mitems(self._explicit_in_operator_links) +
                          separator +
                          self._in_operator_mitems(self._automatic_in_operator_links))),
                    Menu(_("Filter missing values (operator NOT IN)"),
                         (self._in_operator_mitems(self._explicit_in_operator_links, True) +
                          separator +
                          self._in_operator_mitems(self._automatic_in_operator_links, True))),
                )
            dual = self._dualform()
            if self._view.bindings() and not (isinstance(dual, pytis.form.MultiBrowseDualForm) and
                                              dual.main_form() == self):
                menu += (MenuSeparator(),
                         MenuItem(_("Open with side forms"),
                                  command=Command(
                                      Application.run_form,
                                      name=self._name,
                                      form_class=pytis.form.MultiBrowseDualForm,
                                      select_row=self._current_key(),
                                  ),
                                  help=_("Open the current record in a main form with "
                                         "related data in side forms."),
                                  icon='link'))
        return menu

    @Command.define
    def printout(self, spec=None):
        if not spec:
            # Handle Ctrl-p which invokes print without arguments for
            # the default print (the first print specification) if defined.
            try:
                prints = self._resolver.get(self._name, 'print_spec')
            except ResolverError:
                prints = ()
            if not prints:
                log(EVENT, 'No print specification for:', self._name)
                return
            spec = prints[0]
        if spec.handler():
            handler = spec.handler()
            args = self._context_action_args(Action('x', '-', context=spec.context()))
            return handler(*args)
        else:
            app.printout(self._name, spec.name(),
                         parameters=self._formatter_parameters(),
                         row=copy.copy(self._table.row(self._current_cell()[0])),
                         form=self,
                         language=spec.language())


class SideBrowseForm(BrowseForm):
    """Form displaying records depending on other form's current row."""

    def _init_attributes(self, main_form, binding_column=None, side_binding_column=None,
                         hide_binding_column=True, condition=None, arguments=None,
                         prefill=None, search=None, **kwargs):
        """Process constructor arguments and initialize attributes.

        Arguments:

          main_form -- the main form instance.

        """
        assert isinstance(main_form, Form), main_form
        self._binding_column = binding_column
        self._side_binding_column = side_binding_column
        self._hide_binding_column = hide_binding_column
        self._binding_condition = condition
        self._xarguments = arguments
        self._selection_arguments = {}
        self._side_prefill = prefill
        self._side_search = search
        if binding_column:

            def column_condition(row):
                return pytis.data.EQ(side_binding_column, row[binding_column])
            if condition is not None:
                cond = condition

                def condition(row):
                    return pytis.data.AND(column_condition(row), cond(row))
            else:
                condition = column_condition
        self._main_form = main_form
        self._selection_condition = condition
        self._main_form_row = None
        kwargs['condition'] = pytis.data.OR()  # The form will be empty after initialization.
        super(SideBrowseForm, self)._init_attributes(**kwargs)

    def _current_arguments(self):
        arguments = self._arguments
        if arguments == self._data.UNKNOWN_ARGUMENTS:
            return self._data.UNKNOWN_ARGUMENTS
        return dict(self._selection_arguments, **(arguments or {}))

    def _get_aggregation_result(self, key):
        if self._main_form_row is None:
            # Avoid calling select_aggregate with invalid function arguments
            # before 'on_selection' is called.  This may happen because
            # _get_aggregation_result() is called asynchronously from label
            # EVT_PAINT event handler.
            return None
        else:
            return super(SideBrowseForm, self)._get_aggregation_result(key)

    def _provider_kwargs(self):
        kwargs = super(SideBrowseForm, self)._provider_kwargs()
        kwargs['main_form_row'] = self._main_form_row
        return kwargs

    def on_selection(self, row):
        """Update form after main form selection.

        Arguments:

          row -- main form selected row as a PresentedRow instance.

        """
        # log(EVENT, 'Filtrace obsahu formuláře:', (self._name, row))
        self._main_form_row = row
        if self._xarguments is not None:
            self._selection_arguments = self._xarguments(row)
        if self._side_prefill:
            prefill = self._side_prefill(row)
            if prefill:
                def value_instance(cid, value):
                    # Beware, we can not cast all values to their row types here, because
                    # some fields may not exist in the row.  This happens when side form
                    # prefill (specified for given Binding) contains values for another
                    # form than for itself when it redirects new record insertion to
                    # the other form through on_new_record.
                    if isinstance(value, pytis.data.Value):
                        return value
                    else:
                        return pytis.data.Value(self._row.type(cid), value)
                self._prefill = dict([(cid, value_instance(cid, val))
                                      for cid, val in prefill.items()])
        elif self._binding_column:
            bcol, sbcol = self._binding_column, self._side_binding_column
            self._prefill = {sbcol: row[bcol].retype(self._row.type(sbcol))}
        if self._selection_condition is not None:
            self._lf_condition = self._selection_condition(row)
        elif self._xarguments is not None:
            self._lf_condition = None
        self._refresh(interactive=True)
        query_fields = self._view.query_fields()
        if query_fields:
            callback = query_fields.on_main_form_selection()
            if callback:
                callback(row, self._query_fields_row())
        if self._side_search:
            search = self._side_search(row)
            if search is not None:
                if isinstance(search, pytis.data.Value):
                    search = pytis.data.EQ(self._data.key()[0].id(), search)
                elif isinstance(search, dict):
                    search = pytis.data.AND(*[pytis.data.EQ(k, v) for k, v in search.items()])
                else:
                    assert isinstance(search, pytis.data.Operator)
                self._select_cell(0)
                self._search(search, pytis.data.FORWARD, row_number=0, report_failure=False)

    def side_form_in_condition(self):
        """Return pytis.form.IN condition for filtering mainform records by this sideform.

        The main form will be filtered to contain only rows which have non-zero
        side form rows with the current binding condition for the current side
        form profile's filter.

        """
        if not self.initialized():
            self.full_init()
        bcol, sbcol = self._binding_column, self._side_binding_column
        if ((bcol is None or self._binding_condition is not None or
             self._current_profile.id() == '__constructor_profile__')):
            return None
        else:
            if self._current_profile.id() == self._default_profile.id():
                profile_id = None
            else:
                profile_id = self._current_profile.id()
            return make_in_operator(bcol, self.name(), sbcol, profile_id,
                                    arguments=self._arguments)

    def _default_columns(self):
        columns = super(SideBrowseForm, self)._default_columns()
        if self._hide_binding_column:
            return tuple([c for c in columns if c != self._side_binding_column])
        else:
            return columns

    def _formatter_parameters(self):
        parameters = super(SideBrowseForm, self)._formatter_parameters()
        parameters.update({self._main_form.name() + '/' + pytis.output.P_ROW:
                           copy.copy(self._main_form.current_row().row())})
        return parameters

    def main_form(self):
        """Return main form instance corresponding to this side form."""
        return self._main_form


class AggregationForm(BrowseForm):

    def _full_init(self, *args, **kwargs):
        # We can't process these arguments in _init_attributes() since they are
        # needed in _create_view_spec() and _create_data_object() which are
        # called before _init_attributes().
        self._af_name = kwargs.pop('aggregated_view_name')
        self._af_group_by_columns = tuple(kwargs.pop('group_by_columns'))
        self._af_aggregation_columns = kwargs.pop('aggregation_columns')
        self._af_aggregation_condition = kwargs.pop('aggregation_condition', None)
        self._af_aggregation_arguments = kwargs.pop('aggregation_arguments', None)
        self._af_grouping_functions = tuple(kwargs.pop('grouping_functions'))
        super(AggregationForm, self)._full_init(*args, **kwargs)

    def _create_view_spec(self):
        view = super(AggregationForm, self)._create_view_spec()
        fields = list(view.fields())
        labels = dict([(f.id(), f.label()) for f in fields])
        agg_labels = dict(self._available_aggregations())
        column_groups = []
        for column_id, function in self._af_group_by_columns:
            if function is None:
                column_groups.append(column_id)
            else:
                spec = find(function, self._af_grouping_functions, key=lambda x: x[0])
                label, input_type, return_type = spec[1:]
                func_column_id = self._group_by_column_id(column_id, function)
                label = labels[column_id] + ' :: ' + label
                column_groups.append((func_column_id, return_type, function, column_id))
                fields.append(Field(func_column_id, label, type=return_type))
        operations = []
        for column_id, op in self._af_aggregation_columns:
            agg_column_id = self._aggregation_column_id(column_id, op)
            label = labels[column_id] + '/' + agg_labels[op]
            operations.append((op, column_id, agg_column_id))
            fields.append(Field(agg_column_id, label))
        self._data_kwargs = dict(
            operations=tuple(operations),
            column_groups=tuple(column_groups),
            condition=self._af_aggregation_condition,
        )
        return ViewSpec(view.title(), fields)

    def _create_data_object(self):
        return pytis.util.data_object(self._name, kwargs=self._data_kwargs)

    def _current_arguments(self):
        return self._af_aggregation_arguments

    def _saved_settings_accessor(self):
        # We need to have unique names for different column configurations
        # because profiles for one configuration may not be valid in the other.
        spec_name = self._name + '/' + ':'.join(self._select_columns())
        return self._SavedSettingsAccessor(spec_name, self._form_name(), self._view, self._data)

    def _can_aggregated_view(self, aggregated_view_id):
        return False

    def _autofilter_values(self, cid, limit, **kwargs):
        # 'self._data.distinct()' doesn't work in aggregation forms, so we have
        # to perform the distinct operation manually here.
        seen = set()
        distinct = []

        def process(row):
            if len(distinct) > limit:
                return
            v = row[cid]
            value = v.value()
            if value not in seen:
                distinct.append(v)
                seen.add(value)
        self._data.select_map(process, **kwargs)
        return distinct

    def _aggregation_column_id(self, column_id, op):
        return '_' + column_id + '_' + str(op)

    def _group_by_column_id(self, column_id, function):
        if function is None:
            return column_id
        else:
            return str('_' + column_id + '_' + function)

    def _group_by_column_ids(self):
        return [self._group_by_column_id(column_id, function)
                for column_id, function in self._af_group_by_columns]

    def _init_columns(self, columns=None):
        if columns is None:
            columns = self._select_columns()
        return super(AggregationForm, self)._init_columns(columns=columns)

    def _default_sorting(self):
        return tuple([(column_id, pytis.data.ASCENDENT)
                      for column_id in self._group_by_column_ids()])

    def _select_columns(self):
        aggregation_columns = [self._aggregation_column_id(column_id, op)
                               for column_id, op in self._af_aggregation_columns]
        return tuple(self._group_by_column_ids() + aggregation_columns)

    def _lf_sfs_columns(self):
        return [c for c in super(AggregationForm, self)._lf_sfs_columns()
                if c.id() in self._select_columns()]

    def title(self):
        return (_("Aggregated View") + ' :: ' +
                super(AggregationForm, self).title() +
                ' :: ' + self._af_name)

    def group_by_columns(self):
        return self._group_by_column_ids()

    def side_form_condition(self, row):
        """Return the side form filtering condition for given main form row."""
        conditions = [self._af_aggregation_condition]
        for column_id, function in self._af_group_by_columns:
            if function is None:
                condition = pytis.data.EQ(column_id, row[column_id])
            else:
                value = row[self._group_by_column_id(column_id, function)]
                op_function = pytis.data.OpFunction(function, column_id)
                condition = pytis.data.EQ(op_function, value)
            conditions.append(condition)
        return pytis.data.AND(*conditions)

    def side_form_arguments(self):
        return self._af_aggregation_arguments
