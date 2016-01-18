# -*- coding: utf-8 -*-

# Copyright (C) 2001-2016 Brailcom, o.p.s.
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
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

from __future__ import unicode_literals

"""Práce s formuláři se seznamovým zobrazením.

Modul jednak interpretuje specifikaci formulářů (viz modul 'spec') pro
seznamové zobrazení a jednak zajišťuje práci s ní prostřednictvím objektů
wxWindows.

"""

# Terminologická poznámka: Proměnné s názvem `row' obvykle značí číslo řádku
# (číslováno od 0).  Jedná-li se o obsah řádku, nazývá se příslušná proměnná
# obvykle `the_row'.  Matoucí jméno `row' bylo převzato z wxWindows.

import collections
import copy
import cStringIO
import datetime
import functools
import string
import time
import types

import wx
import wx.grid

import pytis.data
import pytis.form
import pytis.output
import pytis.presentation
from pytis.presentation import Action, ActionGroup, AggregatedView, \
    CodebookSpec, Computer, Editable, Field, \
    FormType, Link, TextFormat, ViewSpec
from pytis.util import ACTION, DEBUG, EVENT, OPERATIONAL, \
    Attribute, ProgramError, ResolverError, SimpleCache, Structure, \
    UNDEFINED, compare_objects, find, form_view_data, log, sameclass
import pytis.remote
from dialog import AggregationSetupDialog, Error, InputNumeric, MultiQuestion, Question
from event import UserBreakException, wx_callback
from form import BrowsableShowForm, Form, LookupForm, PopupEditForm, PopupForm, \
    QueryFieldsForm, \
    RecordForm, Refreshable, ShowForm, TitledForm
from output import print_form
from screen import CheckItem, KeyHandler, Menu, MItem, MSeparator, WxKey, \
    busy_cursor, copy_to_clipboard, dlg2px, file_menu_items, focused_window, get_icon, \
    is_busy_cursor, microsleep, popup_menu, wx_button, wx_checkbox, wx_choice, wx_text_ctrl
from search import sfs_columns
from application import Application, \
    aggregated_views_manager, block_refresh, current_form, db_operation, \
    message, refresh, run_dialog, run_form, set_status
import _grid
import config

_ = pytis.util.translations('pytis-wx')


# Forms


class ListForm(RecordForm, TitledForm, Refreshable):
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

    _REFRESH_PERIOD = 60 # sekund
    _SELECTION_CALLBACK_DELAY = 3 # desítky milisekund
    _ROW_LABEL_WIDTH = 85
    _ALLOW_TITLE_BAR = True
    _ALLOW_TOOLBAR = False

    _STATUS_FIELDS = ('list-position', 'data-changed')

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
                self.define_key(action.hotkey(), self.COMMAND_CONTEXT_ACTION, dict(action=action))
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

    def _init_attributes(self, select_row=0, **kwargs):
        self._aggregation_results = SimpleCache(self._get_aggregation_result)
        super(ListForm, self)._init_attributes(_singleline=True, select_row=select_row, **kwargs)
        self._fields = self._view.fields()
        self._selection_candidate = None
        self._selection_callback_candidate = None
        self._selection_callback_tick = None
        self._in_select_cell = False
        self._last_reshuffle_request = self._reshuffle_request = 0
        self._current_editor = None
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
        self._refresh(when=self.DOIT_IMMEDIATELY)
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

    def _column_width(self, column):
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

    def _create_form_parts(self, sizer):
        if self.title() is not None and self._ALLOW_TITLE_BAR:
            self._title_bar = self._create_title_bar()
            sizer.Add(self._title_bar, 0, wx.EXPAND | wx.FIXED_MINSIZE)
        else:
            self._title_bar = None
        if self._ALLOW_TOOLBAR:
            sizer.Add(self._create_toolbar(), 0, wx.EXPAND | wx.FIXED_MINSIZE)
        sizer.Add(self._create_grid(), 1, wx.EXPAND | wx.FIXED_MINSIZE)
        self._create_query_fields_panel(sizer)

    def _create_grid(self):
        # Create the grid and table.  Initialize the data select.
        self._editors = []
        self._table = None
        self._grid = g = wx.grid.Grid(self)
        self._table = table = \
            _grid.ListTable(self, self._data, self._row, self._columns, self._lf_select_count_,
                            sorting=self._lf_sorting, grouping=self._grouping,
                            prefill=self._prefill,
                            row_style=self._view.row_style())
        g.SetTable(table, True)
        g.SetRowLabelSize(0)
        # g.SetColLabelAlignment(wx.CENTER, wx.CENTER)
        g.SetMargins(0, 0)
        g.DisableDragGridSize()
        g.DisableDragRowSize()
        g.SetSelectionMode(wx.grid.Grid.wxGridSelectRows)
        g.SetLabelBackgroundColour(wx.SystemSettings.GetColour(wx.SYS_COLOUR_BACKGROUND))
        g.SetLabelFont(g.GetFont()) # Use standard font instead of bold.
        self._row_height = row_height = dlg2px(g, 0, 10).GetHeight()
        self._label_height = dlg2px(g, 0, 12).GetHeight()
        self._init_col_attr()
        self._update_colors()
        self._update_label_height()
        g.SetDefaultRowSize(row_height)
        # Event handlery
        labels = g.GetGridColLabelWindow()
        corner = g.GetGridCornerLabelWindow()
        wx_callback(wx.grid.EVT_GRID_SELECT_CELL, g, self._on_select_cell)
        wx_callback(wx.grid.EVT_GRID_COL_SIZE, g, self._on_label_drag_size)
        wx_callback(wx.grid.EVT_GRID_EDITOR_SHOWN, g, self._on_editor_shown)
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

    def _update_grid(self, data_init=False, inserted_row_number=None, inserted_row_prefill=None,
                     delete_column=None, insert_column=None, inserted_column_index=None,
                     init_columns=False, retain_row=False):
        g = self._grid
        t = self._table
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
            if inserted_row_number is not None:
                row_count = row_count + 1
            t.update(columns=self._columns, row_count=self._lf_select_count_,
                     sorting=self._lf_sorting,
                     grouping=self._grouping, inserted_row_number=inserted_row_number,
                     inserted_row_prefill=inserted_row_prefill, prefill=self._prefill)
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
            r = original_row_number if inserted_row_number is None else inserted_row_number
            self._select_cell(row=r)
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
        if message_id == wx.grid.GRIDTABLE_NOTIFY_COLS_DELETED:
            self._close_editors()
        msg = wx.grid.GridTableMessage(self._table, message_id, *args)
        self._grid.ProcessTableMessage(msg)

    def _init_col_attr(self):
        # (Re)inicializuj atributy sloupců gridu.
        def registration(editor):
            self._current_editor = editor
        if self._editors:
            self._close_editors()
        for i, c in enumerate(self._columns):
            # zarovnání
            attr = wx.grid.GridCellAttr()
            if isinstance(self._row.type(c.id()), pytis.data.Number):
                alignment = wx.ALIGN_RIGHT
            else:
                alignment = wx.ALIGN_LEFT
            attr.SetAlignment(alignment, wx.CENTER)
            # editor
            if ((c.editable() in (Editable.ALWAYS, Editable.ONCE) or
                 isinstance(c.editable(), Computer))):
                editing = self._table.editing()
                if editing:
                    e = _grid.InputFieldCellEditor(self._parent, editing.the_row, c.id(),
                                                   self, registration)
                    self._editors.append(e)
                    # self._grid.SetCellEditor(row, col, e)
                    attr.SetEditor(e)
            else:
                attr.SetReadOnly()
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

    def _create_query_fields_panel(self, sizer):
        query_fields = self._view.query_fields()
        if query_fields:
            panel = wx.Panel(self, -1, style=wx.SUNKEN_BORDER)
            form = QueryFieldsForm(panel, config.resolver, None, prefill=self._query_field_values,
                                   **query_fields.view_spec_kwargs())
            self._query_fields_form = form
            if not query_fields.autoapply():
                self._query_fields_apply_button = apply_button = wx_button(
                    panel, label=_("Apply"),
                    tooltip=_("Reload form data with current query field values."),
                    enabled=not query_fields.autoinit(),
                    callback=lambda e: self._apply_query_fields()
                )
            else:
                self._query_fields_apply_button = apply_button = None
            self._query_fields_panel_buttons = panel_buttons = (
                wx_button(panel, label=_("Minimize"),
                          tooltip=_("Minimize/maximize query panel."),
                          icon='minimize-down', noborder=True,
                          callback=self._on_minimize_query_fields),
                wx_button(panel, label=_("Move up"),
                          tooltip=_("Move the query panel to the top/bottom edge of the form."),
                          icon='move-up', noborder=True,
                          callback=self._on_move_query_fields))
            panel_sizer = wx.BoxSizer()
            panel_sizer.Add(form, 0, wx.EXPAND | wx.FIXED_MINSIZE)
            if apply_button:
                panel_sizer.Add((0, 0), 1)
                panel_sizer.Add(apply_button, 0, wx.ALL | wx.ALIGN_BOTTOM | wx.FIXED_MINSIZE, 4)
            for button in panel_buttons:
                panel_sizer.Add(button, 0, wx.FIXED_MINSIZE, 1)
            panel.SetSizer(panel_sizer)
            panel.SetAutoLayout(1)
            position = self._get_saved_setting('query-fields-position', 'up')
            if position == 'up':
                sizer.Insert(sizer.GetItemIndex(self._grid), panel, 0, wx.EXPAND)
            else:
                sizer.Add(panel, 0, wx.EXPAND)
            if self._get_saved_setting('query-fields-minimized', False):
                self._toggle_query_fields_minimization()
            self._update_query_fields_panel_button_bitmaps()
            load = self._view.query_fields().load()
            if load:
                load(self._query_fields_row())
            form.set_callback(form.CALL_QUERY_FIELDS_CHANGED, self._on_query_fields_changed)
        else:
            self._query_fields_form = None
            self._query_fields_apply_button = None
        self._unapplied_query_field_changes = False

    def _on_query_fields_changed(self):
        if self._query_fields_apply_button:
            self._query_fields_apply_button.Enable(True)
        else:
            self._apply_query_fields()

    def _apply_query_fields(self):
        save = self._view.query_fields().save()
        if save:
            save(self._query_fields_row())
        self.refresh(interactive=True, reload_query_fields=False)
        if self._query_fields_apply_button:
            self._query_fields_apply_button.Enable(False)
            self._grid.SetFocus()

    def refresh(self, reload_query_fields=True, **kwargs):
        query_fields = self._view.query_fields()
        if query_fields and query_fields.load() and reload_query_fields:
            row = self._query_fields_row()
            if row is not None:
                query_fields.load()(row)
        return super(ListForm, self).refresh(**kwargs)

    def restore(self):
        if self._query_fields_apply_button and self._query_fields_apply_button.Enabled:
            self._unapplied_query_field_changes = True
        return super(ListForm, self).restore()

    def _update_query_fields_panel_button_bitmaps(self):
        sizer = self._top_level_sizer
        grid_position = sizer.GetItemIndex(self._grid)
        panel_position = sizer.GetItemIndex(self._query_fields_form.GetParent())
        for button in self._query_fields_panel_buttons:
            if button is self._query_fields_panel_buttons[0]:
                if self._query_fields_minimized():
                    icon = panel_position > grid_position and 'maximize-up' or 'maximize-down'
                else:
                    icon = panel_position > grid_position and 'minimize-down' or 'minimize-up'
            else:
                icon = panel_position > grid_position and 'move-up' or 'move-down'
            bitmap = get_icon(icon, type=wx.ART_TOOLBAR)
            button.SetBitmapLabel(bitmap)

    def _on_move_query_fields(self, event):
        sizer = self._top_level_sizer
        target_position = sizer.GetItemIndex(self._grid)
        panel = event.GetEventObject().GetParent()
        sizer.Detach(panel)
        sizer.Insert(target_position, panel, 0, wx.EXPAND)
        sizer.Layout()
        self._update_query_fields_panel_button_bitmaps()
        position = sizer.GetItemIndex(panel) < sizer.GetItemIndex(self._grid) and 'up' or 'down'
        self._set_saved_setting('query-fields-position', position)

    def _on_minimize_query_fields(self, event):
        self._toggle_query_fields_minimization()
        self._update_query_fields_panel_button_bitmaps()
        self._set_saved_setting('query-fields-minimized', self._query_fields_minimized())

    def _query_fields_minimized(self):
        return not self._query_fields_form.IsShown()

    def _toggle_query_fields_minimization(self):
        # Minimize/Deminimize the panel.
        form = self._query_fields_form
        panel = form.GetParent()
        for child in panel.GetChildren():
            if child not in self._query_fields_panel_buttons:
                child.Show(not child.IsShown())
        self._top_level_sizer.Layout()
        if self._query_fields_minimized():
            row = form.row()
            values = ['%s: %s' % (field.spec().label(), row.format(field.id()))
                      for field in form.fields()]
            panel.SetToolTipString(', '.join(values))
        elif panel.GetToolTip():
            panel.SetToolTipString(' ')
            panel.GetToolTip().Enable(False) # Doesn't seem to work, thus the line above...

    def _context_menu(self):
        """Vrať specifikaci \"kontextového\" popup menu vybrané buňky seznamu.

        Vrací: Sekvenci instancí 'MItem'.

        Tuto metodu nechť odvozené třídy předefinují, pokud chtějí zobrazovat
        kontextové menu.

        """
        return ()

    def _edit_menu(self):
        return (
            MItem(_("Edit cell"),
                  command=ListForm.COMMAND_EDIT,
                  help=_("Enter the input field for this value.")),
            MItem(_("Save Record"),
                  command=ListForm.COMMAND_LINE_COMMIT,
                  help=_("Save the record and leave editation.")),
            MItem(_("Quit editation"),
                  command=ListForm.COMMAND_FINISH_EDITING,
                  help=_("Leave editation without saving the record.")),
            MSeparator(),
            MItem(_("Copy cell content"),
                  command=ListForm.COMMAND_COPY_CELL,
                  help=_("Copy the value into the clipboard.")),
            # MItem("", command = ListForm.COMMAND_LINE_ROLLBACK),
        )

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
        columns = [(c.label(), c) for c in self._columns
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
                      command=self.COMMAND_SEARCH(next=True, back=True)),
            wx_button(panel, icon=wx.ART_GO_FORWARD, height=HEIGHT, tooltip=_("Find next"),
                      command=self.COMMAND_SEARCH(next=True)),
            wx_checkbox(panel, label=_("search also inside values"),
                        tooltip=_("Check if you want to search anywhere inside the value. "
                                  "Otherwise the substring is only searched at the start "
                                  "of the string."),
                        checked=full),
            wx_checkbox(panel, label=_("case sensitive"),
                        tooltip=_("Check if you want to distinguish "
                                  "lower and upper case letters."),
                        checked=False),
            wx_button(panel, label=_("Hide"), tooltip=_("Hide the search panel"),
                      icon='close', noborder=True,
                      callback=lambda e: self._exit_incremental_search()),
        )
        sizer = wx.BoxSizer()
        for i, ctrl in enumerate(controls[:-1]):
            sizer.Add(ctrl, 0, wx.LEFT, i >= 4 and 10 or 0)
        sizer.Add((0, 0), 1)
        sizer.Add(controls[-1])
        panel.SetSizer(sizer)
        panel.SetAutoLayout(True)
        self._top_level_sizer.Add(panel, 0, wx.EXPAND)
        self._top_level_sizer.Layout()
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
            message(_("No next matching record"), beep_=True)
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
        sizer = self._top_level_sizer
        panel.Enable(False)
        sizer.Detach(panel)
        panel.Destroy()
        sizer.Layout()
        self._grid.SetFocus()
        if not rollback:
            the_row = self._table.row(self._table.current_row())
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
            except:
                # It sometimes happens, under unknown circumstances, that the
                # data select gets changed without updating ListTable selection
                # data.  Then `row' may actually be outside the reported number
                # of rows limit and the table row call above may crash.  In
                # such a case, it's probably better to return an unknown row
                # instead of raising an error.
                pass
        return result

    def _selected_rows(self):
        g = self._grid
        # It was observed at "certain" configurations, that the methods
        # g.GetSelectionBlockTopLeft() and g.GetSelectionBlockBottomRight()
        # don't report a selection made from the keyboard using Shift + Arrow
        # keys.  Such selection is ignored until the user clicks to the grid
        # with a mouse.  It is also not reported by EVT_GRID_RANGE_SELECT.  The
        # only working solution in this case was querying all existing grid
        # rows by g.IsInSelection().  The problem is, that this method is
        # inefficient for very long tables, but in practise the delay seems
        # acceptable up to one milion rows in a table (around 500ms).  The
        # whole problem, however, only appears only at some systems (TCs Debian
        # development workstation and Ubuntu notebook) and didn't apear at any
        # other tested system.  So the work around below is left commanted out
        # just in case the problem re-appears somewhere...
        # return filter(lambda row: g.IsInSelection(row, 0), range(g.GetNumberRows()))
        #
        # One would say that g.GetSelectedRows() is what we want, but it always
        # returns an empty list, so we must construct the list ourselves using
        # g.GetSelectionBlockTopLeft() and g.GetSelectionBlockBottomRight().
        rows = []
        for start, end in zip([r for r, c in g.GetSelectionBlockTopLeft()],
                              [r for r, c in g.GetSelectionBlockBottomRight()]):
            rows.extend([r for r in range(start, end + 1) if r not in rows])
        rows.sort()
        return rows

    def selected_rows(self):
        return _grid.TableRowIterator(self._table, self._selected_rows())

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
                assert isinstance(row, types.IntType)
                # Zkontroluj případné opuštění editace
                if not self._finish_editing(row=row):
                    log(EVENT, 'Quitting editation not confirmed')
                    return False
                else:
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
                    #       dualform. Nahrazeno voláním _show_position v
                    #       _post_selection_hook.
                    #       Jiné řešení?
                    # self._show_position()
            elif col is not None and col != current_col:
                g.SetGridCursor(current_row, col)
                g.MakeCellVisible(current_row, col)
            if __debug__:
                log(DEBUG, 'Cell selected:', (row, col))
            return True
        finally:
            self._in_select_cell = False

    def _edit_cell(self):
        # Activate the editor of the current field.
        # TODO: Cell editors must be recreated for the current edited row.  Reinitializing columns
        # solves that, but some more gentle solution would be desirable...
        if self._current_editor is None:
            self._init_col_attr()
        row, col = self._current_cell()
        if self._view.on_edit_record() is not None:
            message(_("In-line editation disabled.  Use a standalone form (F5)."), beep_=True)
            return False
        if not self._is_editable_cell(row, col):
            message(_("Field not editable."), kind=ACTION, beep_=True)
            return False
        self._grid.EnableCellEditControl()
        log(EVENT, 'Grid field editor started:', (row, col))
        return True

    def _on_editor_shown(self, event):
        if self._table.editing():
            event.Skip()
        else:
            event.Veto()
            self._select_cell(row=max(0, event.GetRow()), col=event.GetCol())

    def _finish_editing(self, question=None, row=None):
        # Vrací pravdu, právě když nejsou akce blokovány editací řádku.
        table = self._table
        editing = table.editing()
        if not editing:
            return True
        if editing.row == row:
            return True
        if not editing.the_row.changed():
            if __debug__:
                log(DEBUG, 'Quit unchanged row editation enabled.')
            self._on_line_rollback()
            finish = True
        else:
            log(EVENT, 'Quit unsaved edited record attempted.')
            if question is None:
                question = _("Revoke your changes?")
            buttons = bcancel, bsave, bcontinue = \
                _("Revoke"), _("Save"), _("Continue editation")
            result = run_dialog(MultiQuestion, question, buttons=buttons, default=bsave)
            finish = (result != bcontinue)
            if result == bcancel:
                log(EVENT, 'Quit confirmed by user')
                self._on_line_rollback()
                finish = True
            elif result == bsave:
                log(EVENT, 'Quit saving row data')
                finish = self._on_line_commit()
            elif result is None or result == bcontinue:
                log(EVENT, 'Quit not confirmed')
                finish = False
            else:
                raise ProgramError('Unexpected dialog result', result)
        return finish

    def _on_line_commit(self):
        # The return value matters here, as it is used by _cmd_cell_commit().
        log(EVENT, 'Saving changed form row to the database')
        # Pick the new data.
        table = self._table
        editing = table.editing()
        if not editing:
            return False
        row = editing.row
        the_row = editing.the_row
        # Check record integrity.
        failed_id = self._check_record(the_row)
        if failed_id:
            col = find(failed_id, self._columns, key=lambda c: c.id())
            if col is not None:
                i = self._columns.index(col)
                self._select_cell(row=row, col=i, invoke_callback=False)
                self._edit_cell()
            return True
        # Determine the operation and the key.
        newp = editing.the_row.new()
        if newp:
            permission = pytis.data.Permission.INSERT
        else:
            permission = pytis.data.Permission.UPDATE
        rdata = self._record_data(the_row, permission=permission, updated=(not newp))
        kc = [c.id() for c in self._data.key()]
        if newp:
            if row > 0:
                after = table.row(row - 1).row().columns(kc)
                before = None
            elif row < table.number_of_rows(min_value=(row + 2)) - 1:
                after = None
                before = table.row(row + 1).row().columns(kc)
            else:
                after = before = None
            op, args, kwargs = self._data.insert, (rdata,), dict(after=after, before=before)
        else:
            key = editing.orig_row.columns(kc)
            op, args, kwargs = self._data.update, (key, rdata,), {}
        # Perform the operation.
        success, result = db_operation(op, *args, **dict(kwargs,
                                                         transaction=self._open_transaction()))
        if self._governing_transaction is None and self._transaction is not None:
            self._transaction.commit()
        self._transaction = self._governing_transaction
        if success and result[1]:
            cleanup = self._view.cleanup()
            if cleanup is not None:
                original_row = copy.copy(the_row)
                new_row = result[0]
                if new_row is None:
                    new_row = the_row.row()
                the_row.set_row(new_row, reset=True)
            table.edit_row(None)
            message(_("Record saved to the database."), ACTION)
            self.refresh()
            self._run_callback(self.CALL_MODIFICATION)
            if cleanup is not None:
                cleanup(the_row, original_row)
            self.focus()
        elif success:
            log(EVENT, 'Zamítnuto pro chybu klíče')
            if editing.the_row.new():
                msg = _("Row with this key already exists or adjacent row changed.")
            else:
                msg = _("Row with this key already exists or the original row "
                        "doesn't exist anymore.")
            run_dialog(Warning, msg)
            return False
        else:
            log(EVENT, 'Chyba databázové operace')
            return False
        return True

    def _on_line_rollback(self, soft=False):
        log(EVENT, 'Zrušení editace řádku')
        if self._transaction is not None:
            if self._governing_transaction is None:
                self._transaction.rollback()
            else:
                self._transaction.cut('inline')
            self._transaction = self._governing_transaction
        editing = self._table.editing()
        if not editing:
            return False
        if soft and editing.the_row.changed():
            return True
        row = editing.row
        if editing.the_row.new():
            self._update_grid()
        else:
            self._table.edit_row(None)
            self._grid.Refresh()
        self._select_cell(row=row, invoke_callback=False)
        self.refresh()
        return True

    def _is_editable_cell(self, row, col):
        # Vrať pravdu, pokud je buňka daného řádku a sloupca editovatelná.
        editing = self._table.editing()
        if editing and row == editing.row:
            the_row = editing.the_row
        else:
            the_row = self._table.row(row)
        cid = self._columns[col].id()
        return the_row.editable(cid)

    def _find_next_editable_cell(self):
        # Vrať pravdu, pokud bylo pohybem vpravo nalezeno editovatelné políčko.
        row, col = self._current_cell()
        while self._grid.MoveCursorRight(False):
            col += 1
            if self._is_editable_cell(row, col):
                self._edit_cell()
                return True

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
        self._refresh(when=self.DOIT_IMMEDIATELY, reset={'filter': condition})
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
        log(EVENT, 'Notifikace o změně dat řádkového seznamu')
        now = time.time()
        maybe_future = self._last_reshuffle_request + self._REFRESH_PERIOD
        self._reshuffle_request = max(now, maybe_future)
        self._show_data_status()

    def _on_idle(self, event):
        if super(ListForm, self)._on_idle(event):
            return True
        if is_busy_cursor() or current_form() is not self:
            # Prevent blocking the idle method of a popup form opened on top of the current form,
            # such as PopupEditForm or Codebook.
            return False
        if self._selection_candidate is not None:
            row, col = self._selection_candidate
            self._selection_candidate = None
            self._grid.MakeCellVisible(row, col)
            self._grid.Refresh()
        x, y, timestamp = self._last_grid_mouse_position
        if timestamp is not None:
            now = 1000 * int(time.time()) + int(datetime.datetime.now().microsecond / 1000)
            delay = now - timestamp
            gw = self._grid.GetGridWindow()
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
                    value = self._get_cell_tooltip_string(row, col)
                    if value:
                        tooltip = gw.GetToolTip()
                        if tooltip is None:
                            tooltip = wx.ToolTip(value)
                            gw.SetToolTip(tooltip)
                        elif tooltip.GetTip() != value:
                            tooltip.SetTip(value)
                    else:
                        gw.SetToolTip(None)
                    self._last_cell_tooltip_position = row, col
            else:
                gw.SetToolTip(None)
                self._last_cell_tooltip_position = (None, None)
        if self._selection_callback_candidate is not None:
            if self._selection_callback_tick > 0:
                self._selection_callback_tick -= 1
                microsleep(100)
                event.RequestMore()
            else:
                row = self._selection_callback_candidate
                self._selection_callback_candidate = None
                the_row = self._table.row(row)
                if the_row is not None:
                    self._run_callback(self.CALL_SELECTION, the_row)
                    self._post_selection_hook(the_row)
        # Update grid when lazy row count computation is in progress; we
        # mustn't do it much often otherwise row count computation gets disrupted
        # and slowed down significantly.  Hence the timeout value below.
        if ((self._table.editing() is None and
             self._last_updated_row_count != self._table.number_of_rows(timeout=0.3))):
            self._update_grid()
            self._show_position()
        if self._unapplied_query_field_changes:
            self._unapplied_query_field_changes = False
            if run_dialog(Question, _("Query fields contain unapplied changes. Apply now?"), True):
                self._apply_query_fields()
        # V budoucnu by zde mohlo být přednačítání dalších řádků nebo dat
        event.Skip()
        return False

    def _get_cell_tooltip_string(self, row, col):
        # Return the tooltip string for given grid cell.
        if (((self._transaction is None and not self._data.select_active()) or
             (self._transaction is not None and not self._transaction.open()))):
            self.refresh()
            if row >= self._lf_count(min_value=(row + 1)):
                return None
        record = self._table.row(row)
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

    def _post_selection_hook(self, the_row):
        if focused_window() is self:
            # TODO: viz poznámka v _select_cell.
            self._show_position()
            # Show display value in status line (this is also shown in tooltips,
            # but tooltips are not useful when using keyboard only).
            row, col = self._current_cell()
            if row >= 0 and col >= 0:
                message(self._table.row(row).display(self._columns[col].id()))

    def _on_select_cell(self, event):
        if not self._in_select_cell and self._grid.GetBatchCount() == 0:
            # GetBatchCount zjišťujeme proto, aby nedhocházelo k volání
            # callbacku při změnách v rámci _update_grid(), které nejsou
            # interaktivní.
            self._run_callback(self.CALL_USER_INTERACTION)
            if ((self._table.editing() is None and
                 self._last_updated_row_count != self._table.number_of_rows(timeout=0))):
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

    def _displayed_columns_menu(self, col):
        select_columns = self._select_columns()
        if select_columns is None:
            columns = self._fields
        else:
            columns = [self._view.field(cid) for cid in select_columns]
        import locale
        columns.sort(key=lambda c: locale.strxfrm(c.column_label()))
        return [CheckItem(_("Row headings"), command=ListForm.COMMAND_TOGGLE_ROW_LABELS,
                          state=lambda: self._grid.GetRowLabelSize() != 0)] + \
               [CheckItem(c.column_label(),
                          command=ListForm.COMMAND_TOGGLE_COLUMN(column_id=c.id(), col=col),
                          state=lambda c=c: c in self._columns)
                for c in columns if c and not c.disable_column()]

    def _aggregation_menu(self):
        menu = [CheckItem(title, command=ListForm.COMMAND_TOGGLE_AGGREGATION(operation=op),
                          state=lambda op=op: op in self._aggregations)
                for op, title, icon, label in self._AGGREGATIONS]
        menu.extend((MSeparator(),
                     MItem(_("Show all"), command=ListForm.COMMAND_AGGREGATE),
                     MItem(_("Hide all"), command=ListForm.COMMAND_UNAGGREGATE),
                     ))
        predefined_aggregated_views = self._view.aggregated_views()
        if predefined_aggregated_views:
            menu.append(MSeparator())
            menu.extend([MItem(v.name(),
                               command=ListForm.COMMAND_AGGREGATED_VIEW(aggregated_view_id=v.id()),
                               help=_("Open predefined aggregated view"))
                         for v in predefined_aggregated_views])
        manager = aggregated_views_manager()
        aggregated_views = [manager.load(self._name, aggregated_view_id)
                            for aggregated_view_id in manager.list(self._name)]
        if aggregated_views:
            menu.append(MSeparator())
            menu.extend([MItem(v.name(),
                               command=ListForm.COMMAND_AGGREGATED_VIEW(aggregated_view_id=v.id()),
                               help=_("Open user defined aggregated view"))
                         for v in aggregated_views])
        menu.extend((MSeparator(),
                     MItem(_("Define new aggregated view"),
                           command=ListForm.COMMAND_AGGREGATED_VIEW(aggregated_view_id=None))))
        if aggregated_views:
            command = ListForm.COMMAND_DELETE_AGGREGATED_VIEW
            menu.append(Menu(_("Remove agregated view"),
                             [MItem(v.name(), command=command(aggregated_view_id=v.id()))
                              for v in aggregated_views]))
        return menu

    def _column_context_menu(self, col):
        M = Menu
        I = MItem
        ________ = MSeparator()
        ASC = LookupForm.SORTING_ASCENDENT
        DESC = LookupForm.SORTING_DESCENDANT
        c = self._columns[col]
        items = (M(_("Primary Sorting"),
                   (I(_("Sort Ascending"),
                      command=LookupForm.COMMAND_SORT(direction=ASC, col=col, primary=True)),
                    I(_("Sort Descending"),
                      command=LookupForm.COMMAND_SORT(direction=DESC, col=col, primary=True)),)),
                 M(_("Secondary Sorting"),
                   (I(_("Sort Ascending"),
                      command=LookupForm.COMMAND_SORT(direction=ASC, col=col)),
                    I(_("Sort Descending"),
                      command=LookupForm.COMMAND_SORT(direction=DESC, col=col)),)),
                 I(_("Omit this column from sorting"),
                   command=LookupForm.COMMAND_SORT(direction=LookupForm.SORTING_NONE, col=col)),
                 I(_("Cancel sorting completely"),
                   command=LookupForm.COMMAND_SORT(direction=LookupForm.SORTING_NONE)),
                 ________,
                 I(_("Group up to this column"),
                   command=ListForm.COMMAND_SET_GROUPING_COLUMN(col=col)),
                 I(_("Cancel visual grouping"),
                   command=ListForm.COMMAND_SET_GROUPING_COLUMN(col=None)),
                 ________,
                 I(_("Autofilter"), command=ListForm.COMMAND_AUTOFILTER(col=col)),
                 I(_("Cancel filtering"), command=LookupForm.COMMAND_UNFILTER),
                 ________,
                 I(_("Hide this column"),
                   command=ListForm.COMMAND_TOGGLE_COLUMN(column_id=c.id(), col=None)),
                 M(_("Displayed columns"),
                   self._displayed_columns_menu(col=col)),
                 )
        return items

    def _aggregation_info_by_position(self, y):
        if y > self._label_height and self._aggregations:
            i = max(0, min(len(self._aggregations) - 1,
                           (y - self._label_height) / self._row_height))
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
                cmd = self.COMMAND_COPY_AGGREGATION_RESULT(cid=self._columns[col].id(),
                                                           operation=aggregation[0])
                menu[0:0] = (MItem(_("Copy the Result"), command=cmd), MSeparator())
        elif col == -1:
            menu = self._displayed_columns_menu(len(self._columns))
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
                pos = self._sorting_position(cid)
                if primary and pos != 0:
                    direction = self.SORTING_ASCENDENT
                else:
                    dir = self._sorting_direction(cid) or self.SORTING_NONE
                    cycle = [self.SORTING_ASCENDENT,
                             self.SORTING_DESCENDANT,
                             self.SORTING_NONE]
                    direction = cycle[(cycle.index(dir) + 1) % 3]
                LookupForm.COMMAND_SORT.invoke(col=col, primary=primary,
                                               direction=direction)
        self._column_move_target = None
        self._column_to_move = None
        event.GetEventObject().Refresh()
        event.Skip()

    def _on_mouse_move(self, event):
        x, y = self._grid.CalcUnscrolledPosition(event.GetX(), event.GetY())
        last_x, last_y, last_time = self._last_grid_mouse_position
        if last_x != x or last_y != y:
            timestamp = 1000 * int(time.time()) + int(datetime.datetime.now().microsecond / 1000)
            self._last_grid_mouse_position = x, y, timestamp
        event.Skip()

    def _on_label_mouse_move(self, event):
        def nearest_column(x):
            g = self._grid
            n = g.GetNumberCols()
            pos = 0
            lastwidth = 0
            for col in range(n + 1):
                if col <= n:
                    width = g.GetColSize(col)
                else:
                    width = 0
                if pos - lastwidth / 2 <= x <= pos + width / 2:
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
        tip = window.GetToolTip()
        # Setting to None doesn't remove the tooltip, so we at least set the tooltip to an ampty
        # string.
        if tip is None:
            tip = wx.ToolTip(descr)
            window.SetToolTip(tip)
        elif tip.GetTip() != descr:
            tip.SetTip(descr)

    def _on_label_drag_size(self, event):
        self._remember_column_size(event.GetRowOrCol())
        self._grid.FitInside()
        # Mohli bychom rozšířit poslední sloupec, ale jak ho potom zase zúžit?
        # if config.stretch_tables:
        #     g = self._grid
        #     n = g.GetNumberCols()
        #     w = functools.reduce(lambda x, i: x + g.GetColSize(i), range(n), 0)
        #     x = g.GetSize().x
        #     if w < x:
        #         col = n-1
        #         g.SetColSize(col, g.GetColSize(col) + (x - w))
        #         self._remember_column_size(col)
        event.Skip()

    def _remember_column_size(self, col):
        self._column_widths[self._columns[col].id()] = self._grid.GetColSize(col)

    def _on_label_paint(self, event):
        def triangle(x, y, r=4, reversed=True):
            # Return polygon coordinates for a triangle.
            if reversed:
                return ((x, y), (x + 2 * r, y), (x + r, y + r))
            else:
                return ((x + r, y), (x + 2 * r, y + r), (x, y + r))
        def arrow(x, y, r=5, l=4):
            # Return polygon coordinates for an arrow.
            return ((x, y), (x - r, y - r), (x - r / 2, y - r), (x - r / 2, y - r - l),
                    (x + r / 2, y - r - l), (x + r / 2, y - r), (x + r, y - r))
        def funnel(x, y, r=3, l=8):
            # Return polygon coordinates for a funnel.
            return ((x, y), (x + r, y + r), (x + r, y + l), (x + r + 2, y + l),
                    (x + r + 2, y + r), (x + r * 2 + 2, y), (x, y))
        g = self._grid
        # t = self._table
        dc = wx.PaintDC(g.GetGridColLabelWindow())
        dc.SetTextForeground(wx.BLACK)
        # dc.SetFont(g.GetFont())
        x = - self._scroll_x_offset()
        row_height = self._row_height
        total_width = dc.GetSize().GetWidth()
        total_height = g.GetColLabelSize()
        label_height = self._label_height
        filtered_columns = self._filtered_columns()
        for col, c in enumerate(self._columns):
            y = 0
            id = c.id()
            width = g.GetColSize(col)
            if col == 0:
                d = 0
            else:
                d = 1
            if self._lf_filter is not None and config.filter_color:
                dc.SetBrush(wx.Brush(config.filter_color, wx.SOLID))
            else:
                dc.SetBrush(wx.Brush('GRAY', wx.TRANSPARENT))
            # Draw the rectangle around.
            dc.DrawRectangle(x - d, y, width + d, label_height)
            # Indicate when the column is being moved (before we clip the active refion).
            move_target = self._column_move_target
            if self._column_to_move is not None and move_target is not None:
                if col == move_target:
                    ax = x - d + (col == 0 and 5 or 0)
                elif col == move_target - 1 and col == len(self._columns) - 1:
                    ax = x + width - 5
                else:
                    ax = None
                if ax is not None:
                    dc.SetBrush(wx.Brush('GREEN', wx.SOLID))
                    dc.DrawPolygon(arrow(ax, label_height - 2))
            dc.SetClippingRegion(x, 0, total_width - x, total_height)
            # Draw the label itself.
            label = c.column_label()
            while dc.GetTextExtent(label)[0] > width and len(label):
                label = label[:-1] # Shrink the label to fit the column width.
            dc.DrawLabel(label, (x, y, width, label_height), wx.ALIGN_CENTER)
            # Draw the sorting sign.
            pos = self._sorting_position(id)
            if pos is not None:
                left = x + width - 12
                top = y + 3
                r = self._sorting_direction(id) == LookupForm.SORTING_ASCENDENT
                if id in self._grouping:
                    color = 'GREEN'
                else:
                    color = 'CORAL'
                dc.SetBrush(wx.Brush(color, wx.SOLID))
                for i in range(pos):
                    dc.DrawLine(left, top + 2 * i, left + 9, top + 2 * i)
                dc.DrawPolygon(triangle(left, top + pos * 2, reversed=r))
            # Draw the filter sign.
            if id in filtered_columns:
                dc.SetBrush(wx.Brush('GOLD', wx.SOLID))
                dc.DrawPolygon(funnel(x + 2, y + 3))
            # Draw the aggregation results.
            y += label_height
            if self._aggregations:
                for op, title, icon_id, label in self._AGGREGATIONS:
                    if op in self._aggregations:
                        rect = (x - d, y - 1, width + d, row_height + 1)
                        dc.SetBrush(wx.Brush('GRAY', wx.TRANSPARENT))
                        dc.DrawRectangle(*rect)
                        value = self._aggregation_results[(id, op)]
                        if value is not None:
                            icon = get_icon(icon_id)
                            if isinstance(value.type(), pytis.data.Number):
                                align = wx.ALIGN_CENTER_VERTICAL | wx.ALIGN_RIGHT
                            else:
                                align = wx.ALIGN_CENTER_VERTICAL | wx.ALIGN_LEFT
                            label_rect = (x - d + 2, y, width + d, row_height)
                            exported_value = value.export()
                            if icon:
                                dc.DrawImageLabel(' ' + exported_value, icon, label_rect, align)
                            else:
                                dc.DrawLabel(label + ' ' + exported_value, label_rect, align)
                        y += row_height
                dc.DrawLine(x - d, y, x + width, y)
            x += width

    def _on_corner_paint(self, event):
        if self._aggregations:
            dc = wx.PaintDC(self._grid.GetGridCornerLabelWindow())
            dc.SetTextForeground(wx.BLACK)
            dc.SetBrush(wx.Brush('GRAY', wx.TRANSPARENT))
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
            menu = (MItem(_("Hide row headings"), command=ListForm.COMMAND_TOGGLE_ROW_LABELS),)
        self._popup_menu(menu)
        event.Skip()

    def _on_right_click(self, event):
        self._run_callback(self.CALL_USER_INTERACTION)
        row, col = event.GetRow(), event.GetCol()
        self._select_cell(row, col)
        self._grid.Refresh()
        self.COMMAND_CONTEXT_MENU.invoke(position=event.GetPosition())

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
        lines = rot / delta
        if lines != 0:
            vsx, vsy = g.GetViewStart()
            lines = lines * linesPer
            scrollTo = vsy - pxy / lines
            g.Scroll(-1, scrollTo)

    def _show_position(self):
        row = self._current_cell()[0]
        total, finished = self._table.number_of_rows(timeout=0, full_result=True)
        if not finished:
            total = '%s?' % (total,)
        set_status('list-position', "%d/%s" % (row + 1, total))

    def _show_data_status(self):
        if self._reshuffle_request > self._last_reshuffle_request:
            status = _("Data changed")
        else:
            status = _("Data ok")
        set_status('data-changed', status)

    def _is_changed(self):
        editing = self._table.editing()
        return editing and editing.the_row.changed()

    def _exit_check(self):
        # Opuštění formuláře je umožněno vždy, ale před opuštěním během editace
        # je nutné provést dodatečné akce.
        editing = self._table.editing()
        if editing:
            log(EVENT, 'Attempt to leave row form during editation')
            if ((editing.the_row.changed() and
                 run_dialog(Question, _("Save the editted record?"), True))):
                log(EVENT, 'Saving confirmed')
                self._on_line_commit()
            else:
                log(EVENT, 'Saving denied')
                self._on_line_rollback()
        return True

    def select_row(self, position, quiet=False):
        # Během editace může `position' obsahovat nevyhledatelná data.
        if position is not None and self._table.editing():
            position = self._table.editing().row
        if ((isinstance(position, int) and
             position < self._table.number_of_rows(min_value=(position + 1)))):
            # Pro číslo voláme rovnou _select_cell a nezdržujeme se převodem na
            # row a zpět, který probíhá v rodičovské metodě...
            self._select_cell(row=position)
            return True
        else:
            return super(ListForm, self).select_row(position, quiet=quiet)

    def _select_row(self, row, quiet=False):
        if row:
            row_number = self._get_row_number(row)
            if row_number is None:
                if not quiet:
                    if self._search_again_unfiltered():
                        self._apply_profile(self._profiles[0])
                        return self._select_row(row)
                return False
        else:
            row_number = -1
        self._select_cell(row=row_number)
        return True

    def _refresh(self, when=None, reset=None, key_update=True, interactive=False):
        """Aktualizuj data seznamu z datového zdroje.

        Překresli celý seznam v okamžiku daném argumentem 'when' se zachováním
        parametrů dle argumentu 'reset'.

        Argumenty:

          when -- určuje, zda a kdy má být aktualizace provedena, musí to být
            jedna z 'DOIT_*' konstant třídy.  Implicitní hodnota je
            'DOIT_AFTEREDIT', je-li 'reset' 'None', 'DOIT_IMMEDIATELY' jinak.
          reset -- určuje, které parametry zobrazení mají být zachovány a které změněny.  Hodnotou
            je buď 'None', nebo dictionary.  Je-li hodnotou 'None', zůstane zachována filtrovací
            podmínka i třídění.  Jinak jsou resetovány právě ty parametry, pro něž
            v dictionary existuje klíč (jeden z řetězců 'sorting', 'filter'), a to na hodnotou
            z dictionary pro daný klíč.
          key_update -- if true, try to select the previously selected row
          interactive -- indicates whether the refresh was invoked by explicit
            user request

        Vrací: Pravdu, právě když byla aktualizace provedena.

        """
        assert when in (None, # internal ONLY!
                        self.DOIT_IMMEDIATELY, self.DOIT_AFTEREDIT, self.DOIT_IFNEEDED), when
        assert reset is None or isinstance(reset, dict), reset
        if when is None:
            if reset is None:
                when = self.DOIT_AFTEREDIT
            else:
                when = self.DOIT_IMMEDIATELY
        # Jdeme na to
        if __debug__:
            log(DEBUG, 'Refresh request:', (when, reset))
        if interactive:
            self._apply_providers()
        if when is self.DOIT_IFNEEDED:
            if ((self._reshuffle_request == self._last_reshuffle_request or
                 self._reshuffle_request > time.time())):
                return False
        if when is self.DOIT_IMMEDIATELY:
            QUESTION = _("Cancel changes in row editation and refresh the form?")
            delay = not self._finish_editing(question=QUESTION)
        else:
            delay = (self._table.editing() is not None) # nechceme držet info
        if delay:
            if __debug__:
                log(DEBUG, 'Refresh postponed until end of editation.')
            return False
        if reset:
            for k, v in reset.items():
                if k == 'filter':
                    self._lf_filter = v
                elif k == 'sorting':
                    self._lf_sorting = v
                else:
                    raise ProgramError('Invalid refresh parameter', k)
        self._last_reshuffle_request = self._reshuffle_request = time.time()
        self._update_grid(data_init=True, retain_row=True)
        self._show_data_status()
        self._print_menu_ = UNDEFINED
        return True

    def _update_colors(self):
        # Wx only supports highlighting of the current cell, not the current
        # row.  Thus we highlight the current row ourselves in
        # CustomCellRenderer (defined in _grid.py).
        if config.cell_highlight_color is not None:
            self._grid.SetCellHighlightColour(config.cell_highlight_color)
        if config.grid_line_color is not None:
            self._grid.SetGridLineColour(config.grid_line_color)

    def _total_width(self):
        total = self._grid.GetRowLabelSize()
        for c in self._columns:
            total += self._column_width(c)
        return total

    def _total_height(self):
        g = self._grid
        height = g.GetColLabelSize()
        rows = self._grid.GetNumberRows()
        if rows:
            height += rows * g.GetRowSize(0)
        if self._title_bar:
            height += self._title_bar.GetSize().height
        return height

    def _resize_columns(self, size=None):
        g = self._grid
        if size is None:
            size = g.GetSize()
        width = size.width
        height = size.height
        if height < self._total_height():
            width = width - wx.SystemSettings.GetMetric(wx.SYS_VSCROLL_X) - 1
        total_width = self._total_width()
        if width > total_width > 0:
            coef = float(width) / (total_width - g.GetRowLabelSize())
        else:
            coef = 1
        total = g.GetRowLabelSize()
        last = None
        # Přenastav šířky sloupců
        for i, c in enumerate(self._columns):
            w = self._column_width(c)
            if not c.fixed() and config.stretch_tables:
                w = int(w * coef)
                last = i
            g.SetColSize(i, w)
            total += w
        if coef != 1 and total != width and last is not None:
            g.SetColSize(last, g.GetColSize(last) + (width - total))

    def _on_size(self, event):
        size = event.GetSize()
        if size.width != self._grid.GetSize().width:
            self._resize_columns(size)
        event.Skip()

    def _close_editors(self):
        for e in self._editors:
            if e:
                e.close()
        self._editors = []

    def _release_data(self):
        if not self._table.editing():
            super(ListForm, self)._release_data()

    def _cleanup(self):
        super(ListForm, self)._cleanup()
        if self._grid is not None:
            # Musíme ručně zrušit editory, jinak se dočkáme segmentation fault.
            self._close_editors()
            # Musíme tabulce zrušit datový objekt, protože jinak do něj bude šahat
            # i po kompletním uzavření starého gridu (!!) a rozhodí nám tak data
            # v novém gridu.
            if self._table is not None:
                self._table.close()

    def _cleanup_data(self):
        self._data.remove_callback_on_change(self.on_data_change)
        super(ListForm, self)._cleanup_data()

    # Zpracování příkazů

    def can_command(self, command, **kwargs):
        # Příkazy platné i během editace, pokud není aktivní editor.
        UNIVERSAL_COMMANDS = (ListForm.COMMAND_COPY_CELL,
                              ListForm.COMMAND_RESIZE_COLUMN,
                              ListForm.COMMAND_EDIT,
                              ListForm.COMMAND_FIRST_COLUMN,
                              ListForm.COMMAND_LAST_COLUMN,
                              ListForm.COMMAND_CONTEXT_MENU)
        # Příkazy platné pouze během editace řádku.
        EDIT_COMMANDS = (ListForm.COMMAND_LINE_COMMIT,
                         ListForm.COMMAND_LINE_ROLLBACK,
                         ListForm.COMMAND_FINISH_EDITING,
                         ListForm.COMMAND_CELL_COMMIT,
                         ListForm.COMMAND_CELL_ROLLBACK)
        if not self.initialized():
            self.full_init()
        if self._table.editing():
            allowed = EDIT_COMMANDS
            if not self._grid.IsCellEditControlEnabled():
                allowed += UNIVERSAL_COMMANDS
            if command not in allowed:
                return False
        elif command in EDIT_COMMANDS:
            return False
        return super(ListForm, self).can_command(command, **kwargs)

    def _cmd_delete_record(self):
        def blocked_code():
            deleted = super(ListForm, self)._cmd_delete_record()
            self._table.edit_row(None)
            return deleted
        if block_refresh(blocked_code):
            r = self._current_cell()[0]
            if r < self._table.number_of_rows(min_value=(r + 2)) - 1:
                self._select_cell(row=r)
            elif r > 0:
                self._select_cell(row=(r - 1))
            # Uděláme raději refresh celé aplikace, protože jinak se
            # nerefreshne horní formulář po vymazání záznamu ze sideformu.
            refresh()

    def _cmd_activate(self, alternate=False):
        self._run_callback(self.CALL_ACTIVATION, alternate=alternate)

    def _cmd_first_column(self):
        self._select_cell(col=0)

    def _cmd_last_column(self):
        self._select_cell(col=(len(self._columns) - 1))

    def _can_move_column(self, diff=1):
        col = self._grid.GetGridCursorCol()
        return 0 <= col + diff < len(self._columns)

    def _cmd_move_column(self, diff=1):
        col = self._grid.GetGridCursorCol()
        newcol = col + diff
        if 0 <= newcol < len(self._columns):
            c = self._columns[col]
            self._update_grid(delete_column=c, insert_column=c,
                              inserted_column_index=newcol)
            self._select_cell(col=newcol)
        else:
            log(OPERATIONAL, "Invalid column move command:", (col, newcol))

    def _cmd_toggle_column(self, column_id, col=None):
        c = find(column_id, self._columns, key=lambda c: c.id())
        if c:
            self._update_grid(delete_column=c)
        else:
            self._update_grid(insert_column=self._view.field(column_id),
                              inserted_column_index=col)

    def _cmd_toggle_row_labels(self):
        g = self._grid
        if g.GetRowLabelSize() == 0:
            widht = self._ROW_LABEL_WIDTH
        else:
            widht = 0
        g.SetRowLabelSize(widht)
        g.FitInside()
        self.refresh()

    def _cmd_resize_column(self, diff=5):
        # diff can be positive or negative integer in pixels.
        g = self._grid
        col = g.GetGridCursorCol()
        newsize = g.GetColSize(col) + diff
        if newsize > 0:
            g.SetColSize(col, newsize)
            g.SetSize(g.GetSize())
            g.Refresh()
            self._remember_column_size(col)
            if g.IsCellEditControlEnabled():
                row = g.GetGridCursorRow()
                self._current_editor.SetSize(g.CellToRect(row, col))

    def _can_sort(self, **kwargs):
        col = kwargs.get('col')
        if col is not None:
            kwargs['col'] = self._columns[col].id()
        return super(ListForm, self)._can_sort(**kwargs)

    def _cmd_sort(self, col=None, direction=None, primary=False):
        if not self._finish_editing():
            return
        if col is not None:
            col = self._columns[col].id()
        old_sorting = self._lf_sorting
        sorting = super(ListForm, self)._cmd_sort(col=col, direction=direction, primary=primary)
        if sorting is not None and sorting != old_sorting:
            try:
                # Update grouping first.
                cols = self._sorting_columns()
                l = min(len(cols), len(self._grouping))
                self._grouping = tuple(cols[:l])
                # Make the changes visible.
                self._refresh(when=self.DOIT_IMMEDIATELY, reset={'sorting': sorting})
                # Force to count at least one line.  This is to allow user
                # break in case the first cursor operation is very slow.  We
                # must do it here, otherwise the delay happens later in
                # _on_idle where we can't handle it.
                self._table.number_of_rows(min_value=1)
            except UserBreakException:
                self._lf_sorting = old_sorting
                cols = self._sorting_columns()
                l = min(len(cols), len(self._grouping))
                self._grouping = tuple(cols[:l])
                # Make the changes visible.
                self._refresh(when=self.DOIT_IMMEDIATELY, reset={'sorting': self._lf_sorting})
        return sorting

    def _cmd_toggle_aggregation(self, operation):
        if operation in self._aggregations:
            command = self.COMMAND_UNAGGREGATE
        else:
            command = self.COMMAND_AGGREGATE
        command.invoke(operation=operation)

    def _can_aggregate(self, operation=None):
        if operation is None:
            return len(self._aggregations) != len(self._AGGREGATIONS)
        else:
            return operation not in self._aggregations

    def _cmd_aggregate(self, operation=None):
        if operation is None:
            self._aggregations = [op for op, title, icon, label in self._AGGREGATIONS]
        else:
            self._aggregations.append(operation)
        self._update_label_height()
        self.refresh()

    def _can_unaggregate(self, operation=None):
        if operation is None:
            return bool(self._aggregations)
        else:
            return operation in self._aggregations

    def _cmd_unaggregate(self, operation=None):
        if operation is None:
            self._aggregations = []
        else:
            self._aggregations.remove(operation)
        self._update_label_height()
        self.refresh()

    def _available_aggregations(self):
        return ([(op, title) for op, title, icon, label in self._AGGREGATIONS] +
                [(pytis.data.Data.AGG_COUNT, _("Count"))])

    def _cmd_aggregated_view(self, aggregated_view_id):
        grouping_functions = self._view.grouping_functions()
        for v in self._view.aggregated_views():
            if v.id() == aggregated_view_id:
                name = v.name()
                group_by_columns = v.group_by_columns()
                aggregation_columns = v.aggregation_columns()
                break
        else:
            manager = aggregated_views_manager()
            if aggregated_view_id:
                view = manager.load(self._name, aggregated_view_id)
                name = view.name()
                group_by_columns = view.group_by_columns()
                aggregation_columns = view.aggregation_columns()
            else:
                name = ''
                group_by_columns = ()
                aggregation_columns = ()
            result = run_dialog(AggregationSetupDialog,
                                aggregation_functions=self._available_aggregations(),
                                grouping_functions=grouping_functions,
                                columns=[(c.id(), c.label(), c.type())
                                         for c in self._lf_sfs_columns()],
                                aggregation_valid=self._aggregation_valid,
                                name=name, group_by_columns=group_by_columns,
                                aggregation_columns=aggregation_columns)
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
                 aggregation_arguments=arguments,
                 )

    def _cmd_delete_aggregated_view(self, aggregated_view_id):
        manager = aggregated_views_manager()
        manager.drop(self._name, aggregated_view_id)

    def _cmd_filter_by_cell(self):
        row, col = self._current_cell()
        id = self._columns[col].id()
        value = self._table.row(row)[id]
        self.COMMAND_FILTER_BY_VALUE.invoke(column_id=id, value=value)

    def _can_filter_by_cell(self):
        row, col = self._current_cell()
        return self._data.find_column(self._columns[col].id()) is not None

    def _cmd_autofilter(self, col=None, position=None):
        busy_cursor(True)
        try:
            if col is None:
                col = self._current_cell()[1]
            cid = self._columns[col].id()
            if not self._data.permitted(cid, pytis.data.Permission.VIEW):
                return
            cond = self._current_condition()
            arguments = self._current_arguments()
            limit = 60
            distinct = self._autofilter_values(cid, limit, condition=cond, arguments=arguments)
            if len(distinct) > limit:
                message(_("Too many items for autofilter."), beep_=True)
                return
            items = [MItem(v.export(), command=ListForm.COMMAND_FILTER_BY_VALUE,
                           args=dict(column_id=cid, value=v))
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

    def _cmd_context_menu(self, position=None):
        if self._table.editing():
            menu = self._edit_menu()
        else:
            menu = self._context_menu()
        if menu:
            if position is None:
                g = self._grid
                row, col = self._current_cell()
                rect = g.CellToRect(row, col)
                pos = (rect.GetX() + rect.GetWidth() / 3,
                       rect.GetY() + rect.GetHeight() / 2 + g.GetColLabelSize())
                position = self._grid.CalcScrolledPosition(pos)
            self._popup_menu(menu, position=position)

    def _popup_menu(self, items, position=None):
        popup_menu(self._grid, items, keymap=self._get_keymap(), position=position)

    def _can_set_grouping_column(self, col=None):
        if col is not None:
            return self._columns[col].id() in self._sorting_columns()
        else:
            return bool(self._grouping)

    def _cmd_set_grouping_column(self, col=None):
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

    def _cmd_incremental_search(self, full=False, prefill=None):
        row, col = self._current_cell()
        col_id = self._columns[col].id()
        if ((not isinstance(self._row.type(col_id), pytis.data.String) or
             not self._data.permitted(col_id, pytis.data.Permission.VIEW))):
            message(_("Unable to search incrementally in this column."), beep_=True)
            return
        if self._search_panel is None:
            self._create_search_panel(full=full, prefill=prefill)
        else:
            self._search_panel_controls[1].SetFocus()
        # self._selection_callback = self.get_callback(self.CALL_SELECTION)
        # self.set_callback(self.CALL_SELECTION, None)

    def _cmd_search(self, next=False, back=False):
        if next and self._search_panel is not None:
            self._incremental_search(direction=back and pytis.data.BACKWARD or pytis.data.FORWARD)
        else:
            return super(ListForm, self)._cmd_search(next=next, back=back)

    def _cmd_copy_cell(self):
        row, col = self._current_cell()
        presented_row = self._table.row(row)
        if presented_row:
            cid = self._columns[col].id()
            copy_to_clipboard(presented_row.format(cid, secure=True))

    def _cmd_copy_aggregation_result(self, operation, cid):
        copy_to_clipboard(self._aggregation_results[(cid, operation)].export())

    def _can_copy_aggregation_result(self, operation, cid):
        return self._aggregation_results[(cid, operation)] is not None

    def _can_edit(self):
        return self._current_key() is not None

    def _cmd_edit(self):
        table = self._table
        if self._transaction is None:
            self._transaction = pytis.data.DBTransactionDefault(config.dbconnection)
        self._transaction.set_point('inline')
        if not table.editing():
            if not self._lock_record(self._current_key()):
                self._transaction.cut('inline')
                self._transaction = self._governing_transaction
                return False
            table.edit_row(self._current_cell()[0])
            self._grid.Refresh()
        if not self._edit_cell():
            self._on_line_rollback()
        return True

    def _can_cmd_export(self):
        number_rows = self._table.number_of_rows()
        if number_rows == 0:
            problem = _("The table has no rows!")
        elif number_rows > 100000:
            problem = _("The table has too many rows! Use a filter.")
        elif False in [self._data.permitted(c.id(), pytis.data.Permission.EXPORT)
                       for c in self._columns]:
            problem = _("You don't have permissions to export this table.")
        else:
            return True
        run_dialog(Warning, problem + '\n' + _("Export aborted."))
        return False

    def _cmd_export_file(self):
        log(EVENT, 'Called export to file')
        if not self._can_cmd_export():
            return
        try:
            import pyExcelerator as pyxls
            pyxls               # to make flake8 happy
            xls_possible = True
        except:
            xls_possible = False
        wildcards = [_("Files %s", "TXT (*.txt)"), "*.txt",
                     _("Files %s", "CSV (*.csv)"), "*.csv"]
        username = config.dbconnection.user()
        if username is None:
            username = ''
        default_filename = 'export_%s.txt' % username
        if xls_possible:
            msg = "\n\n".join((_("Data may be exported into one of the following file formats."),
                              _("Choose the desired format.")))
            fileformat = run_dialog(MultiQuestion, msg, ('CSV', 'XLS'), default='CSV')
            if not fileformat:
                return
            if fileformat == 'XLS':
                wildcards = [_("Files %s", "XLS (*.xls)"), "*.xls"]
                default_filename = 'export_%s.xls' % username
        else:
            fileformat = 'CSV'

        filename = None
        remote = False
        if pytis.remote.client_available():
            client_ip = pytis.remote.client_ip()
            log(EVENT, 'Windows service on %s available' % client_ip)
            try:
                filename = pytis.remote.make_temporary_file(suffix=('.' + fileformat.lower()))
            except:
                pass
            export_file = filename
            if export_file is not None:
                remote = True
        else:
            log(EVENT, 'Windows service not available')
        if filename is None:
            export_dir = config.export_directory
            filename = pytis.form.run_dialog(pytis.form.FileDialog, title=_("Export to file"),
                                             dir=export_dir, file=default_filename, mode='SAVE',
                                             wildcards=tuple(wildcards))
            if not filename:
                return
            mode = 'w'
            if fileformat == 'XLS':
                mode += 'b'
            try:
                export_file = open(filename, mode)
                export_file.write('')
            except:
                msg = _("Unable to open the file for writing!")
                run_dialog(Error, msg)
                return
        if fileformat == 'XLS':
            export_function = self._cmd_export_xls
        else:
            export_function = self._cmd_export_csv
        log(ACTION, "Export action:", (self.name(), self._form_name(), config.dbschemas,
                                       "Filter: %s\n" % str(self._lf_filter)))
        if export_function(export_file) and remote:
            pytis.remote.launch_file(filename.name())

    def _cmd_export_csv(self, filename):
        log(EVENT, 'Called CSV export')
        column_list = [(c.id(), self._row.type(c.id())) for c in self._columns]
        export_encoding = config.export_encoding
        db_encoding = 'utf-8'
        try:
            u"test".encode(export_encoding)
        except:
            msg = '\n'.join(_("Encoding %s not supported.", export_encoding),
                            _("Exported data will not be recoded."))
            export_encoding = None
            run_dialog(Error, msg)
        if isinstance(filename, basestring):
            try:
                export_file = open(filename, 'w')
            except:
                msg = _("Unable to open the file for writing!")
                run_dialog(Error, msg)
                return False
        else:
            export_file = filename
        number_rows = self._table.number_of_rows()
        def _process_table(update):
            # We buffer exported data before writing them to the file in order
            # to prevent numerous rpc calls in case of remote export.
            csv_buffer = cStringIO.StringIO()
            for column in self._columns:
                label = column.column_label()
                if label is None:
                    label = column.label()
                if export_encoding and export_encoding != db_encoding:
                    if not isinstance(label, unicode):
                        label = unicode(label, db_encoding)
                    label = label.encode(export_encoding)
                csv_buffer.write(label + '\t')
            csv_buffer.write('\n')
            for r in range(0, number_rows):
                if not update(int(float(r) / number_rows * 100)):
                    break
                for cid, ctype in column_list:
                    presented_row = self._table.row(r)
                    if isinstance(ctype, pytis.data.Float):
                        s = presented_row.format(cid, secure=True, locale_format=False)
                    else:
                        s = presented_row.format(cid, secure=True)
                    if export_encoding and export_encoding != db_encoding:
                        if not isinstance(s, unicode):
                            s = unicode(s, db_encoding)
                        s = s.encode(export_encoding)
                    csv_buffer.write(';'.join(s.split('\n')) + '\t')
                csv_buffer.write('\n')
            export_file.write(csv_buffer.getvalue())
            export_file.close()
        pytis.form.run_dialog(pytis.form.ProgressDialog, _process_table)
        return True

    def _cmd_export_xls(self, filename):
        log(EVENT, 'Called XLS export')
        column_list = [(c.id(), self._row.type(c.id())) for c in self._columns]
        import datetime
        import pyExcelerator as pyxls
        number_rows = self._table.number_of_rows()
        def _process_table(update):
            w = pyxls.Workbook()
            ws = w.add_sheet('Export')
            # Export labelů
            for i, column in enumerate(self._columns):
                label = column.column_label()
                if label is None:
                    label = column.label()
                ws.write(0, i, unicode(label))
            default_style = pyxls.XFStyle()
            w.add_style(default_style)
            # Styles
            column_styles = {}
            for cid, ctype in column_list:
                st = pyxls.XFStyle()
                if isinstance(ctype, pytis.data.Float):
                    precision = ctype.precision()
                    if precision and precision > 0:
                        fmt = "0." + "0" * precision
                    else:
                        fmt = "general"
                    st.num_format_str = fmt
                elif isinstance(ctype, pytis.data.Date):
                    st.num_format_str = "D.M.YYYY"
                elif isinstance(ctype, pytis.data.Time):
                    st.num_format_str = "h:mm:ss"
                elif isinstance(ctype, pytis.data.DateTime):
                    st.num_format_str = "D.M.YYYY h:mm:ss"
                column_styles[cid] = st
                w.add_style(st)
            for r in range(0, number_rows):
                if not update(int(float(r) / number_rows * 100)):
                    break
                presented_row = self._table.row(r)
                for j, (cid, ctype) in enumerate(column_list):
                    value = presented_row.get(cid, secure=True)
                    if value and value.value() is not None:
                        v = value.value()
                        if isinstance(ctype, pytis.data.Float):
                            s = float(v)
                        else:
                            if isinstance(ctype, pytis.data.Date):
                                s = datetime.date(v.year, v.month, v.day)
                            elif isinstance(ctype, pytis.data.Time):
                                s = datetime.time(v.hour, v.minute, int(v.second))
                            elif isinstance(ctype, pytis.data.DateTime):
                                s = v.strftime(pytis.data.DateTime.CZECH_FORMAT)
                            else:
                                s = ';'.join(presented_row.format(cid, secure=True).split('\n'))
                        ws.write(r + 1, j, s, column_styles[cid])
            w.save(filename)
            if not isinstance(filename, basestring):
                # This is necessary to prevent truncation of the file in some
                # cases as pyxls doesn't close the file.
                filename.close()
        pytis.form.run_dialog(pytis.form.ProgressDialog, _process_table)
        return True

    def _cmd_insert_line(self, before=False, copy=False):
        row = self._current_cell()[0]
        log(EVENT, 'Inserting new table row:', (row, before, copy))
        if not self._data.permitted(True, pytis.data.Permission.INSERT):
            message(_("You don't have insert permissions for this table!"), beep_=True)
            return False
        if self._view.on_new_record() is not None:
            message(_("In-line insertion not allowed. Use a standalone form (F6)."), beep_=True)
            return False
        cols = [c.id() for c in self._columns]
        for col in self._data.columns():
            if ((col.type().not_null() and col.id() not in cols and
                 (self._prefill is None or col.id() not in self._prefill) and
                 self._view.field(col.id()) is not None)):
                # We silently presume, that when a not null column is not in
                # fields, it probably has a default value (if not, it would be
                # an error anyway), so we can continue.
                msg = _("Mandatory column '%s' not shown.\n"
                        "Unable to insert new rows in in-line mode.\n"
                        "Unhide the column or insert the record in a standalone form (F6).")
                label = self._view.field(col.id()).column_label()
                run_dialog(Warning, msg % label)
                return False
        table = self._table
        if table.editing():
            log(EVENT, 'Attempted to insert a new line while editting.')
            return False
        self._last_insert_copy = copy
        oldg = self._grid
        oldempty = (oldg.GetNumberRows() == 0)
        if not copy or oldempty:
            inserted_row_prefill = None
        else:
            inserted_row_prefill = self._row_copy_prefill(table.row(row))
        if not before and not oldempty:
            row = row + 1
        if row == -1:
            row = 0
        self._update_grid(inserted_row_number=row, inserted_row_prefill=inserted_row_prefill)
        self._select_cell(row=row, col=0, invoke_callback=False)
        if ((not self._is_editable_cell(row, 0) and
             not self._find_next_editable_cell())):
            log(EVENT, 'No editable column')
            return False
        if self._transaction is not None:
            self._transaction.set_point('inline')
        self._edit_cell()
        log(EVENT, 'Row inserted')
        return True

    def _can_line_commit(self):
        return self._is_changed()

    def _cmd_line_commit(self):
        return self._on_line_commit()

    def _can_line_rollback(self):
        return self._is_changed()

    def _cmd_line_rollback(self):
        return self._on_line_rollback()

    def _can_cell_commit(self):
        return self._grid.IsCellEditControlEnabled()

    def _cmd_finish_editing(self):
        return self._finish_editing()

    def _cmd_cell_commit(self):
        row, col = self._current_cell()
        id = self._columns[col].id()
        log(EVENT, 'Cell commit in inline editation:', id)
        self._grid.DisableCellEditControl()
        editing = self._table.editing()
        if not editing:
            return
        the_row = editing.the_row
        if the_row.invalid_string(id) is None:
            if not self._find_next_editable_cell():
                if the_row.new():
                    q = _("Save the record?")
                    if run_dialog(Question, q, True):
                        return self._on_line_commit()
                    else:
                        log(EVENT, "Line commit refused by the user.")
                self._grid.SetGridCursor(row, 0)
        if the_row.invalid_string(id) is not None or the_row.new():
            log(EVENT, "Returning to in-line editation.")
            self._edit_cell()

    def _can_cell_rollback(self):
        return self._grid.IsCellEditControlEnabled()

    def _cmd_cell_rollback(self):
        log(EVENT, 'Cell rollback in inline editation.')
        self._current_editor.Reset()
        row, col = self._current_cell()
        self._grid.DisableCellEditControl()
        self._current_editor = None

    def _can_clear_selection(self):
        return len(self.selected_rows()) > 0

    def _cmd_clear_selection(self):
        self.unselect_selected_rows()

    # Public methods

    def is_edited(self):
        """Return True, iff the table currently is in in-line edit mode."""
        return self._table.editing()

    def status_fields(self):
        # TODO: zatím je podoba statusbaru určena specifikací, ale bylo by
        # rozumné to celé předělat, aby se statusbar dynamicky měnil podle
        # aktuálního formuláře (s využitím této metody).
        return (('list-position', 7),)

    # Other public methods

    def on_key_down(self, event, dont_skip=True):
        if self._search_panel is None:
            # Running this callback in dual-form would lead to focus loss on Shift, Ctrl etc.
            self._run_callback(self.CALL_USER_INTERACTION)
        if KeyHandler.on_key_down(self, event, dont_skip=dont_skip):
            return True
        def evil_key(event):
            # Tato věc je tu kvůli eliminaci vstupu do editace políčka
            # libovolnou klávesou.  Není mi znám jiný způsob, jak této
            # eliminace dosáhnout.
            # Nelze použít hasModifiers ani test MetaDown kvůli NumLocku.
            if event.AltDown() or event.ControlDown():
                return False
            code = event.GetKeyCode()
            return code not in(wx.WXK_PRIOR, wx.WXK_NEXT, wx.WXK_LEFT,
                               wx.WXK_RIGHT, wx.WXK_DOWN, wx.WXK_UP,
                               wx.WXK_HOME, wx.WXK_END, wx.WXK_TAB,
                               wx.WXK_ESCAPE)
        if (evil_key(event) or
            (self._grid.IsCellEditControlEnabled() and
             WxKey().is_event_of_key(event, '\t'))):
            return False
        else:
            event.Skip()
            return False

    def focus(self):
        super(ListForm, self).focus()
        self._show_position()
        self._show_data_status()
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
        def __cmp__(self, other):
            if sameclass(self, other):
                return cmp(self._folding, other._folding)
            else:
                return compare_objects(self, other)
        def _find_node(self, node):
            state = self._folding
            labels = (node or '').split('.')
            while labels and state is not None:
                l = labels.pop(0)
                state = state.subnodes().get(l)
            return state
        def _folding_level(self, node):
            state = self._folding
            labels = (node or '').split('.')
            while labels and state is not None:
                level = state.level()
                l = labels.pop(0)
                state = state.subnodes().get(l)
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
                            else: # state_level == 0 or state_level == None -> inherit it
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
                        q = path + '!' + string.join(subnodes.keys(), '|') + '.*'
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
                subnodes = [(k, transform(v),) for k, v in folding.subnodes().items()]
                return (level, subnodes,)
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
            view_folding = self._view.folding()
            if view_folding is not None:
                folding_state = view_folding.folding_state()
        if folding_state is not None:
            self._folding.set_folding_state(folding_state)

    def _default_folding(self):
        return self.Folding()

    def _find_folding_column(self):
        if self._folding_column_id is not None:
            return self._folding_column_id
        folding_column_id = None
        for c in self._data.columns():
            if isinstance(c.type(), pytis.presentation.PrettyFoldable):
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
        elif isinstance(folding_state, self.Folding):
            # HACK: Profiles from specification contain a 'Folding'
            # instance, while saved profiles contain a
            # 'Folding._FoldingState' instance.  A single folding
            # specification should be used in both cases and it should be
            # defined as a public specification class in pytis.presentation
            # (spec.py).
            folding_state = folding_state.folding_state()
        self._init_folding(folding_state)

    def _profile_parameters_to_save(self):
        return dict(super(FoldableForm, self)._profile_parameters_to_save(),
                    folding=self._folding.folding_state())

    def _profile_parameter_changed(self, param, current_value, original_value):
        if ((param == 'folding' and self._current_profile.filter() and
             not self._current_profile.is_user_defined_profile())):
            # Avoid indication of changed profile in case of system profiles
            # with filter and no folding in specifications with
            # initial_folding.  The default folding is actually different
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
            if not run_dialog(Question, _("You attempted to use a filter with the IN operator\n"
                                          "on a foldable form. This operation may potentially\n"
                                          "take several minutes. You can greatly speed up the\n"
                                          "operation by turning the folding off (sorting by\n"
                                          "another column).\n"
                                          "Really continue?"), True):
                # This should abort profile selection in _cmd_apply_profile and
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
        if sorting is () and self._folding_column_id is not None:
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
            if isinstance(self._row.type(column.id()), pytis.presentation.PrettyFoldable):
                row = event.GetRow()
                value = self._table.row(row).format(column.id(), pretty=True, form=self)
                pos = value.find(pytis.presentation.PrettyFoldable.FOLDED_MARK)
                if pos == -1:
                    pos = value.find(pytis.presentation.PrettyFoldable.UNFOLDED_MARK)
                if pos != -1:
                    x1 = self._grid.GetTextExtent(value[:pos])[0]
                    x2 = self._grid.GetTextExtent(value[:pos + 1])[0]
                    x = event.GetPosition()[0] - 2 # don't count grid padding.
                    offset = self._grid.CellToRect(row, col)[0]
                    if x1 - 2 < x - offset < x2 + 2: # enlarge the active area by 2px for easier hit
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

    def _can_expand_or_collapse_subtree(self, level=None):
        return self._folding_enabled()

    def _cmd_expand_or_collapse_subtree(self, level=None):
        row = self._current_cell()[0]
        self._expand_or_collapse(row, level=level)

    def _can_expand_or_collapse(self):
        return self._folding_enabled()

    def _cmd_expand_or_collapse(self):
        row = self._current_cell()[0]
        self._expand_or_collapse(row, level=1)

    def _cmd_expand_all(self):
        self._folding = self.Folding(level=None)
        self._refresh_folding()

    def _cmd_collapse_all(self):
        self._folding = self.Folding()
        self._refresh_folding()

    def _cmd_folding_level(self):
        if self._folding_enabled():
            result = run_dialog(InputNumeric,
                                message=_("Folding/unfolding form nodes to a particular level."),
                                prompt=_("Folding level (1-...):"),
                                min_value=1,
                                integer_width=2)
            level = result.value()
            if level is not None:
                self._folding = self.Folding(level=level)
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
    _ALLOW_TITLE_BAR = False

    def __init__(self, parent, *args, **kwargs):
        parent = self._popup_frame(parent)
        super(CodebookForm, self).__init__(parent, *args, **kwargs)

    def _full_init(self, parent, *args, **kwargs):
        parent = self._popup_frame(parent)
        super(CodebookForm, self)._full_init(parent, *args, **kwargs)
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
        if self._folding_enabled():
            height = self._DEFAULT_WINDOW_HEIGHT
        else:
            height = min(self._DEFAULT_WINDOW_HEIGHT, self._total_height() + 50)
        if self._search_panel:
            height += 30
        size = (self._total_width() + 30, height)
        self.SetSize(size)

    def _popup_frame_style(self):
        return super(CodebookForm, self)._popup_frame_style() | wx.RESIZE_BORDER

    def _total_width(self):
        # 630 seems to be the minimal width to make the profile selector in the toolbar visible.
        return max(super(CodebookForm, self)._total_width(), 630)

    def _current_arguments(self):
        return self._arguments

    def _default_folding(self):
        return self.Folding(level=None)

    def _on_idle(self, event):
        if ListForm._on_idle(self, event):
            return True
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
                    message(_("Unable to search incrementally. No sortable column found!"),
                            beep_=True)
            col = find(col_id, self._columns, key=lambda c: c.id())
            if col is not None:
                self._select_cell(row=0, col=self._columns.index(col))
                self.COMMAND_INCREMENTAL_SEARCH.invoke(prefill=prefill)
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
        return (MItem(_("Select"), command=ListForm.COMMAND_ACTIVATE),)

    def _on_activation(self, alternate=False):
        """Nastav návratovou hodnotu a ukonči modální dialog."""
        self._result = self.current_row()
        self._parent.EndModal(1)
        return True

    def _on_dclick(self, event):
        return self.COMMAND_ACTIVATE.invoke()

    def size(self):
        """Return the prefered form size in pixels as a tuple of two integers (width, height).

        In this class, the returned size is always the total size of the form
        (wide to fit all displayed columns and high to fit all rows).  The
        calling side is responsible to limit the returned size to fit the
        current context (screen).

        """
        return (self._total_width(), self._total_height())


class SelectRowsForm(CodebookForm):
    """Řádkový pop-up formulář vracející tuple všech vybraných řádků."""

    def _on_activation(self, alternate=False):
        selected_rows = tuple(self.selected_rows())
        if len(selected_rows) == 0:
            selected_rows = (self.current_row(),)
        self._result = selected_rows
        self._parent.EndModal(1)
        return True

class BrowseForm(FoldableForm):
    """Formulář pro prohlížení dat s možností editace."""

    class _PrintResolver (pytis.output.OutputResolver):
        P_NAME = 'P_NAME'
        class _Spec:
            # This class has to emulate a specification module as well as a
            # (new style) specification class.
            def __init__(self, resolver):
                self._resolver = resolver
            def body(self, resolver=None, variant=None, **kwargs):
                if config.fallback_table_print:
                    table_id = self._resolver.p(BrowseForm._PrintResolver.P_NAME)
                    view, data = form_view_data(self._resolver, table_id)
                    result = pytis.output.data_table(view, data)
                    return result
                else:
                    run_dialog(Error, _("Print specification not found!"))
            def doc_header(self, resolver=None, variant=None, **kwargs):
                return None
            def doc_footer(self, resolver=None, variant=None, **kwargs):
                return None

        def _get_module(self, name):
            # Supply a default specification module (old style spec).
            try:
                x = super(BrowseForm._PrintResolver, self)._get_module(name)
            except ResolverError:
                x = self._Spec(self)
            return x

        def _get_instance(self, key):
            # Supply a default specification class (new style spec).
            try:
                x = super(BrowseForm._PrintResolver, self)._get_instance(key)
            except ResolverError:
                x = self._Spec(self)
            return x

    class _PlainPrintResolver(pytis.output.PlainFileResolver):

        def get(self, *args, **kwargs):
            result = pytis.output.PlainFileResolver.get(self, *args, **kwargs)
            if result and isinstance(result, basestring):
                result = pytis.output.StructuredText(result)
            return result

    class _DBPrintResolver(pytis.output.DatabaseResolver):

        def __init__(self, db_table):
            pytis.output.DatabaseResolver.__init__(self, db_table,
                                                   ('template', 'rowtemplate', 'header',
                                                    'first_page_header', 'footer', 'style',))

        def get(self, module_name, spec_name, **kwargs):
            specs = ('body', 'row', 'page_header', 'first_page_header', 'page_footer',)
            plain_specs = ('style',)
            try:
                result_index = (specs + plain_specs).index(spec_name)
            except ValueError:
                raise ResolverError(module_name, spec_name)
            module_parts = module_name.split('/')
            if module_parts[0] == 'output':
                del module_parts[0]
            if len(module_parts) > 1:
                module_name = string.join(module_parts[:-1], '/')
                last_spec_name = module_parts[-1]
            else:
                module_name = string.join(module_parts, '/')
                last_spec_name = ''
            result = pytis.output.DatabaseResolver.get(self, module_name, last_spec_name,
                                                       **kwargs)[result_index]
            if result and isinstance(result, basestring) and spec_name in specs:
                result = pytis.output.StructuredText(result)
            return result

    def _init_attributes(self, **kwargs):
        super(BrowseForm, self)._init_attributes(**kwargs)
        menu = (
            MItem(_("Edit cell"),
                  command=ListForm.COMMAND_EDIT,
                  help=_("Edit the value in in-line mode.")),
            MItem(_("Filter by cell"),
                  command=ListForm.COMMAND_FILTER_BY_CELL,
                  help=_("Filter the rows containing the same value in this column.")),
            MItem(_("Copy cell value"),
                  command=ListForm.COMMAND_COPY_CELL,
                  help=_("Copy the contents of the cell into the clipboard.")),
            MSeparator(),
            MItem(_("Edit record"),
                  command=BrowseForm.COMMAND_EDIT_RECORD,
                  help=_("Open a separate edit form for this record.")),
            MItem(_("Copy record"),
                  command=BrowseForm.COMMAND_NEW_RECORD(copy=True),
                  help=_("Open a separate insert form with a copy of this record.")),
            MItem(_("Delete record"),
                  command=RecordForm.COMMAND_DELETE_RECORD,
                  help=_("Delete the record parmanently from the database.")),
            MItem(_("Preview form"),
                  command=ListForm.COMMAND_ACTIVATE,
                  help=_("Open the preview form for browsing all records."),
                  icon='show-record'),
            MItem(_("Dual preview"),
                  command=ListForm.COMMAND_ACTIVATE(alternate=True),
                  help=_("Open a dual form with a table up and preview at the bottom."),
                  icon='show-record'),
            MItem(_("Cancel selection"),
                  command=ListForm.COMMAND_CLEAR_SELECTION(),
                  help=_("Cancel the selection of rows for bulk operations.")),
        )
        editor_items = [MItem(_("Text editor for field %s") % f.label(),
                              command=ListForm.COMMAND_OPEN_EDITOR(field_id=f.id()),
                              help=_("Open a structured text editor."))
                        for f in self._fields if f.text_format() == TextFormat.LCG]
        if editor_items:
            menu += (MSeparator(),) + tuple(editor_items)
        action_items = self._action_mitems(self._view.actions())
        if action_items:
            menu += (MSeparator(),) + tuple(action_items)
        self._context_menu_static_part = menu
        # The dynamic part of the menu is created based on the links.
        def spec_title(name, binding=None):
            if name.find('::') != -1:
                name1, name2 = name.split('::')
                title = config.resolver.get(name1, 'binding_spec')[name2].title() or \
                    ' / '.join((config.resolver.get(name1, 'view_spec').title(),
                                config.resolver.get(name2, 'view_spec').title()))
            else:
                spec = config.resolver.get(name, 'view_spec')
                title = spec.title()
                if binding is not None:
                    b = find(binding, spec.bindings(), key=lambda b: b.id())
                    assert b is not None, "Unknown binding for %s: %s" % (name, binding)
                    title += ' / ' + config.resolver.get(b.name(), 'view_spec').title()
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
            enumerator = self._row.type(f.id()).enumerator()
            codebook = self._row.codebook(f.id())
            if enumerator and codebook:
                links = automatic_links.setdefault(codebook, [])
                item = (codebook, enumerator.value_column(), f)
                if item not in links:
                    links.append(item)
                in_item = (codebook, enumerator.value_column(), f, spec_title(codebook), None)
                if in_item not in self._automatic_in_operator_links:
                    self._automatic_in_operator_links.append(in_item)
        self._automatic_links = [(link_label(spec_title(name)),
                                  [(f, Link(name, column)) for name, column, f in items])
                                 for name, items in sorted(automatic_links.items())]
        self._explicit_in_operator_links.sort()
        self._automatic_in_operator_links.sort()

    def _formatter_parameters(self):
        prefix = self._name + '/'
        return {(prefix + pytis.output.P_CONDITION): pytis.data.AND(self._current_condition()),
                (prefix + pytis.output.P_SORTING): self._lf_sorting,
                (prefix + pytis.output.P_KEY): self._current_key(),
                (prefix + pytis.output.P_ROW): copy.copy(self._table.row(self._current_cell()[0])),
                (prefix + pytis.output.P_DATA): copy.copy(self._data)}

    def _action_mitems(self, spec):
        items = []
        for x in spec:
            if isinstance(x, Action):
                cmd = self.COMMAND_CONTEXT_ACTION(action=x)
                items.append(MItem(x.title(raw=True), command=cmd, help=x.descr()))
            elif isinstance(x, ActionGroup):
                items.append(Menu(x.title(), self._action_mitems(x.items())))
            elif isinstance(x, (types.TupleType, types.ListType)):
                if items:
                    items.append(MSeparator())
                items.extend(self._action_mitems(x))
            else:
                raise ProgramError("Invalid action specification: %s" % x)
        return items

    def _link_mitems(self, row, linkspec):
        def mitem(f, link, row, force_title=None):
            type, name, enabled = link.type(), link.name(), link.enabled()
            if type == FormType.INSERT:
                cmd = Application.COMMAND_NEW_RECORD(name=name,
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
                cmd = Application.COMMAND_RUN_FORM(name=name, form_class=cls,
                                                   select_row={link.column(): row[f.id()]},
                                                   **kwargs)
                icon = 'link'
            if isinstance(enabled, collections.Callable):
                enabled = enabled(row)
            if not enabled:
                cmd = Application.COMMAND_NOTHING(enabled=False)
            return MItem(force_title or title, command=cmd,
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

    def _in_operator_mitems(self, row, linkspec, not_in=False):
        def mitem(name, column, f, title, binding):
            if self._current_profile.id() == self._default_profile.id():
                profile_id = None
            else:
                profile_id = self._current_profile.id()
            column_label = f.column_label()
            if not_in:
                select_row = None
                ititle = _('Filter "%(view_title)s" for rows not contained in '
                           'column "%(column)s" of the current form.')
            else:
                select_row = {column: row[f.id()]}
                ititle = _('Filter "%(view_title)s" for rows contained in '
                           'column "%(column)s" of the current form.')
            def handler(form_class, name, **kwargs):
                # The main reason for wrapping COMMAND_RUN_FORM in a function
                # run through COMMAND_HANDLED_ACTION here is to postpone the
                # time consuming IN operator construction until the menu item
                # is actually selected.
                if self._current_profile_changed():
                    msg = _("Can not filter as long as the current profile is unsaved!")
                    bsave, bquit = _("Save"), _("Cancel")
                    if run_dialog(MultiQuestion, msg, buttons=(bsave, bquit),
                                  default=bsave) != bsave:
                        return
                    self._cmd_update_profile()
                filter = pytis.form.IN(column, self.name(), f.id(), profile_id)
                if not_in:
                    filter = pytis.data.NOT(filter)
                run_form(form_class, name, select_row=select_row, filter=filter, **kwargs)
            if binding:
                form_class = pytis.form.MultiBrowseDualForm
                kwargs = dict(binding=binding)
            else:
                form_class = BrowseForm
                kwargs = {}
            cmd = Application.COMMAND_HANDLED_ACTION(handler=handler,
                                                     enabled=Application.COMMAND_RUN_FORM.enabled,
                                                     form_class=form_class, name=name, **kwargs)
            return MItem(ititle % dict(view_title=title, column=column_label), command=cmd)
        return [mitem(*args) for args in linkspec]

    def _context_menu(self):
        menu = self._context_menu_static_part
        row = self.current_row()
        select_arguments = self._current_arguments()
        file_open_mitems = file_menu_items(self._fields, row, select_arguments)
        if file_open_mitems:
            menu += (MSeparator(),) + tuple(file_open_mitems)
        if self._explicit_links or self._automatic_links or self._explicit_in_operator_links \
                or self._automatic_in_operator_links:
            menu += (MSeparator(),)
        if self._explicit_links:
            menu += tuple(self._link_mitems(row, self._explicit_links))
        if self._automatic_links:
            menu += (Menu(_("Links"), self._link_mitems(row, self._automatic_links)),)
        if self._explicit_in_operator_links or self._automatic_in_operator_links:
            if self._explicit_in_operator_links and self._automatic_in_operator_links:
                separator = [MSeparator()]
            else:
                separator = []
            menu += (Menu(_("Filter existing values (operator IN)"),
                          (self._in_operator_mitems(row, self._explicit_in_operator_links)
                           + separator +
                           self._in_operator_mitems(row, self._automatic_in_operator_links))),
                     Menu(_("Filter missing values (operator NOT IN)"),
                          (self._in_operator_mitems(row, self._explicit_in_operator_links, True)
                           + separator +
                           self._in_operator_mitems(row, self._automatic_in_operator_links, True))),
                     )
        dual = self._dualform()
        if self._view.bindings() and not (isinstance(dual, pytis.form.MultiBrowseDualForm)
                                          and dual.main_form() == self):
            command = Application.COMMAND_RUN_FORM(name=self._name,
                                                   form_class=pytis.form.MultiBrowseDualForm,
                                                   select_row=self._current_key())
            menu += (MSeparator(),
                     MItem(_("Open with side forms"),
                           command=command,
                           help=_("Open the current record in a main form with "
                                  "related data in side forms."),
                           icon='link'))
        return menu

    def _cmd_print(self, print_spec_path=None, language=None):
        log(EVENT, 'Print form invocation:', print_spec_path)
        name = self._name
        if not print_spec_path:
            try:
                spec = self._resolver.get(name, 'print_spec')
            except ResolverError:
                spec = None
            if spec:
                print_spec_path = spec[0].name()
                language = spec[0].language()
            else:
                print_spec_path = name
        parameters = self._formatter_parameters()
        parameters.update({self._PrintResolver.P_NAME: name})
        print_file_resolver = pytis.output.FileResolver(config.print_spec_dir)
        print_resolver = self._PrintResolver(print_file_resolver, self._resolver)
        wiki_template_resolver = self._PlainPrintResolver(config.print_spec_dir, extension='text')
        db_template_resolver = self._DBPrintResolver('ev_pytis_user_output_templates')
        resolvers = (db_template_resolver, wiki_template_resolver, print_resolver,)
        try:
            formatter = pytis.output.Formatter(config.resolver, resolvers, print_spec_path,
                                               form=self, parameters=parameters,
                                               language=language,
                                               translations=pytis.util.translation_path(),
                                               **self._print_form_kwargs())
        except pytis.output.AbortOutput:
            return
        run_form(print_form(), name, formatter=formatter)

    def _print_form_kwargs(self):
        return {}


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
        self._xarguments = arguments
        self._selection_arguments = {}
        self._side_prefill = prefill
        self._side_search = search
        if binding_column:
            column_condition = lambda row: pytis.data.EQ(side_binding_column, row[binding_column])
            if condition is not None:
                cond = condition
                condition = lambda row: pytis.data.AND(column_condition(row), cond(row))
            else:
                condition = column_condition
        self._main_form = main_form
        self._selection_condition = condition
        self._main_form_row = None
        kwargs['condition'] = pytis.data.OR() # The form will be empty after initialization.
        super(SideBrowseForm, self)._init_attributes(**kwargs)

    def _current_arguments(self):
        arguments = self._arguments
        if arguments == self._data.UNKNOWN_ARGUMENTS:
            return self._data.UNKNOWN_ARGUMENTS
        return dict(self._selection_arguments, **(arguments or {}))

    def _provider_kwargs(self):
        return dict(super(SideBrowseForm, self)._provider_kwargs(),
                    main_form_row=self._main_form_row)

    def on_selection(self, row):
        """Update form after main form selection.

        Arguments:

          row -- main form selected row as a PresentedRow instance.

        """
        def inner_value(value):
            if isinstance(value, pytis.data.Value):
                return value.value()
            else:
                return value
        # log(EVENT, 'Filtrace obsahu formuláře:', (self._name, row))
        self._main_form_row = row
        if self._xarguments is not None:
            self._selection_arguments = self._xarguments(row)
        if self._side_prefill:
            prefill = self._side_prefill(row)
        elif self._binding_column:
            prefill = {self._side_binding_column: row[self._binding_column].value()}
        else:
            prefill = {}
        if prefill:
            self._prefill = dict([(cid, pytis.data.Value(self._row.type(cid), inner_value(value)))
                                  for cid, value in prefill.items()])
        if self._selection_condition is not None:
            self._lf_condition = self._selection_condition(row)
        elif self._xarguments is not None:
            self._lf_condition = None
        self._refresh(key_update=False, interactive=True)
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
        if bcol is None or self._current_profile.id() == '__constructor_profile__':
            return None
        else:
            if self._current_profile.id() == self._default_profile.id():
                profile_id = None
            else:
                profile_id = self._current_profile.id()
            return pytis.form.IN(bcol, self.name(), sbcol, profile_id, self._arguments)

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
        self._data_kwargs['operations'] = tuple(operations)
        self._data_kwargs['column_groups'] = tuple(column_groups)
        self._data_kwargs['condition'] = self._af_aggregation_condition
        return ViewSpec(view.title(), fields)

    def _current_arguments(self):
        return self._af_aggregation_arguments

    def _profile_spec_name(self):
        # We need to have unique names for different column configurations
        # because profiles for one configuration may not be valid in the other.
        return (super(AggregationForm, self)._profile_spec_name() +
                '/' + ':'.join(self._select_columns()))

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
