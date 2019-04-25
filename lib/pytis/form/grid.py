# -*- coding: utf-8 -*-

# Copyright (C) 2018, 2019 Tomáš Cerha <t.cerha@gmail.com>
# Copyright (C) 2001-2018 Brailcom, o.p.s.
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

"""Pomocné třídy pro seznamové formuláře."""

# Note on terminology: Identifiers named `row' usually refer to row number
# (starting from zero) as this is what wxWidgets use.  When refering to
# data rows, we usually use 'the_row' for 'PresentedRow' instances.

import collections
import copy
import types
import cachetools

import wx
import wx.grid

import pytis.data
import pytis.output
import pytis.presentation
from pytis.presentation import PresentedRow
from pytis.util import DEBUG, ProgramError, log

from .application import db_operation
from .form import Form
from .screen import Color, get_icon
from .event import top_level_exception

import config


class DataTable(object):
    """Access database table as a grid of numbered rows and columns.

    This class allows accessing table data as a visual grid of cells with
    numeric row and column numbers.  Grid requests are translated into database
    requests.  It doesn't cache database rows, but it caches displayed values
    and cell styles for a limited number of rows (a window roughly matching the
    currently visible portion of the table).

    """
    class _CurrentRow:
        def __init__(self, row, the_row):
            assert isinstance(row, int)
            assert isinstance(the_row, PresentedRow)
            self.row = row
            self.the_row = the_row

    class _Column(object):
        def __init__(self, id_, type_, label, style):
            self.id = id_
            self.type = type_
            self.label = label
            self.style = style

    def __init__(self, data, presented_row, columns, row_count,
                 sorting=(), grouping=(), prefill=None, row_style=None):
        self._data = data
        self._presented_row = copy.copy(presented_row)
        self._row_style = row_style
        self._plain_style = pytis.presentation.Style()
        self.update(row_count=row_count,
                    sorting=sorting,
                    grouping=grouping,
                    prefill=prefill,
                    columns=columns)

    def _update_columns(self, columns):
        self._columns = [self._Column(c.id(),
                                      self._presented_row.type(c.id()),
                                      c.column_label() or '',
                                      c.style())
                         for c in columns]
        self._column_count = len(self._columns)
        self._secret_columns = [c.id() for c in columns
                                if not self._data.permitted(c.id(), pytis.data.Permission.VIEW)]

    def _panic(self):
        if __debug__:
            log(DEBUG, 'Zpanikaření gridové tabulky')

    def _retrieve_row(self, row):
        current = self._current_row
        if not current or current.row != row:
            data = self._data
            success, data_row = db_operation(data.fetch, row)
            if not success:
                self._panic()
            if data_row:
                self._presented_row.set_row(data_row)
                current = self._current_row = self._CurrentRow(row, copy.copy(self._presented_row))
            else:
                if 0 <= row < self.number_of_rows(min_value=(row + 1)):
                    log(DEBUG, "Missing grid row:", row)
                return None
        return current.the_row

    def _format(self, the_row, cid):
        return the_row.format(cid, secure=True)

    def _cached_value(self, row, col_id, style=False):
        # Return the cached value for given row and column id.
        #
        # The value returned is the formatted cell value by default or a
        # computed style, when the keyword argument style is true.
        # This is a little tricky, but the reason is to cache everithing
        # once we read the row value, because we can not cache the rows
        # inside the 'row()' method.
        #
        try:
            value_dict, style_dict = self._cache[row]
        except KeyError:
            the_row = self.row(row)
            if the_row is None:
                return None
            # If row_style is defined, lets compute it.
            row_style = self._row_style
            if isinstance(row_style, collections.Callable):
                protected_row = the_row.protected()
                try:
                    row_style = row_style(protected_row)
                except protected_row.ProtectionError:
                    row_style = None
            style_dict = {}
            value_dict = {}
            # Cache the values and styles for all columns at once.
            for c in self._columns:
                cid = c.id
                value_dict[cid] = self._format(the_row, cid)
                if cid in self._secret_columns:
                    field_style = self._plain_style
                else:
                    field_style = c.style
                    if isinstance(field_style, collections.Callable):
                        field_style = field_style(the_row)
                style_dict[cid] = (field_style or self._plain_style) + row_style
            # Grouping column may not be in self._columns.
            for gcol in self._grouping:
                if gcol not in value_dict:
                    value_dict[gcol] = self._format(the_row, gcol)
            self._cache[row] = value_dict, style_dict
        if style:
            return style_dict[col_id]
        else:
            return value_dict[col_id]

    # Public methods

    def column_id(self, col):
        return self._columns[col].id

    def column_label(self, col):
        return self._columns[col].label

    def current_row(self):
        """Vrať číslo aktuálního řádku datového objektu tabulky.

        Řádky jsou číslovány od 0.  Pokud číslo aktuálního řádku není
        známo, vrať 'None'.

        """
        current = self._current_row
        return current and current.row

    def number_of_rows(self, min_value=None, timeout=None, full_result=False):
        if isinstance(self._row_count, int):
            count, finished = self._row_count, True
        else:
            count, finished = self._row_count.count(min_value=min_value, timeout=timeout)
            if finished:
                self._row_count = count
        if full_result:
            return count, finished
        else:
            return count

    def row(self, row):
        """Return the row number 'row' as a 'PresentedRow' instance.

        Arguments:

          row -- row number within the *database select*, starting from 0

        Jestliže řádek daného čísla neexistuje, vrať 'None'.

        """
        return self._retrieve_row(row)

    def cell_value(self, row, col):
        """Return the formatted value for table cell at given 'row' and 'col'.

        'row' and 'col' are numbered from 0.

        None is returned when given cell does not exist in the data table

        """
        if self._data is not None and col < self._column_count:
            return self._cached_value(row, self._columns[col].id)
        else:
            return None

    def cell_style(self, row, col):
        """Return style for table cell at given 'row' and 'col' as 'Style' instance.

        'row' and 'col' are numbered from 0.

        None is returned when given cell does not exist in the data table.

        The returned style doesn't take grouping highlighting into account.
        Grouping highlighting may be queried using the method 'group()' and
        applied to the cell style afterwards.  This is necessary because
        grouping is not deterministic while cell style is (it always returns
        the same values for the same row and col).

        """
        if row >= self.number_of_rows(min_value=(row + 1)) or col >= self._column_count:
            return None
        else:
            return self._cached_value(row, self._columns[col].id, style=True)

    def rewind(self, position):
        """Move data pointer to given position."""
        if self._current_row is not None and -1 <= position < self.number_of_rows() - 1:
            # Rely on _retrieve_row() side effect setting self._current_row.
            if not self._retrieve_row(position):
                raise Exception('Missing row', position)

    def group(self, row):
        """Return true, if given row belongs to a highlighted group or False otherwise.

        The returned values are deterministic only for a limited range of most
        recently queried rows.  Group (True/False) assignment may be be
        restarted for a more distant range of rows and getting back to a
        previously queried row may thus return different results next time.

        """
        # Return true, if given row belongs to a highlighted group.
        def cached_values(row, cols):
            return tuple([self._cached_value(row, cid) for cid in cols])
        grouping = self._grouping
        if not grouping:
            return False
        try:
            return self._group_cache[row]
        except KeyError:
            values = cached_values(row, grouping)
            try:
                result = self._group_value_cache[values]
                self._group_cache[row] = result
                return result
            except KeyError:
                cached = self._group_cache.keys()
                lower = filter(lambda k: k < row, cached)
                if len(lower) and (row < 100 or row - max(lower) < 80):
                    prev_values = cached_values(row - 1, grouping)
                    prev_group = self.group(row - 1)
                    if values == prev_values:
                        result = prev_group
                    else:
                        result = not prev_group
                    self._group_value_cache[values] = result
                    self._group_cache[row] = result
                    return result
                higher = filter(lambda k: k > row, cached)
                if len(higher) and min(higher) - row < 80:
                    next_values = cached_values(row + 1, grouping)
                    next_group = self.group(row + 1)
                    if values == next_values:
                        result = next_group
                    else:
                        result = not next_group
                    self._group_cache[row] = result
                    return result
                # There is no cached group within nearest rows, so start
                # again with an empty cache.
                self._group_cache = {row: False}
                self._group_value_cache = {values: False}
                return False

    def update(self, columns, row_count, sorting, grouping, prefill):
        assert isinstance(grouping, tuple)
        self._data.rewind()
        self._current_row = None
        self._row_count = row_count
        self._sorting = sorting
        self._grouping = grouping
        self._prefill = prefill
        self._update_columns(columns)
        # (re)create caches
        self._cache = cachetools.LRUCache(maxsize=config.cache_size)
        self._group_cache = {0: False}
        self._group_value_cache = {}

    def close(self):
        # This method is necessary because of some wierd wxWidgets behavior,
        # where the table is being accessed even after the form is closed.
        self._data = None
        # TODO: The following (and maybe the previous too) operations are in
        # principal useless, but we try to remove all data of the instance
        # manually because some data seem not to be destroyed when the form
        # is closed.
        self._fields = None
        self._columns = None
        self._cache = None
        self._group_cache = None
        self._group_value_cache = None
        self._presented_row = None
        self._current_row = None
        self._row_style = None
        self._group_bg_downgrade = None
        self._group_bg_color = None


class GridTable(wx.grid.GridTableBase, DataTable):

    _DEFAULT_FOREGROUND_COLOR = pytis.presentation.Color.BLACK
    _DEFAULT_BACKGROUND_COLOR = pytis.presentation.Color.WHITE

    def __init__(self, form, *args, **kwargs):
        assert isinstance(form, Form)
        self._form = form
        self._attr_cache = {}
        self._font_cache = {}
        wx.grid.GridTableBase.__init__(self)
        DataTable.__init__(self, *args, **kwargs)

    def _panic(self):
        DataTable._panic(self)
        Form.COMMAND_LEAVE_FORM.invoke()

    def _format(self, the_row, cid):
        return the_row.format(cid, pretty=True, form=self._form, secure=True)

    def _make_attr(self, style):
        fg = style.foreground() or self._DEFAULT_FOREGROUND_COLOR
        bg = style.background() or self._DEFAULT_BACKGROUND_COLOR
        key = (style.slanted(), style.bold(), style.overstrike(), style.underline())
        try:
            font = self._font_cache[key]
        except KeyError:
            size = self._form.GetFont().GetPointSize()
            flags = wx.FONTFLAG_DEFAULT
            if style.slanted():
                flags |= wx.FONTFLAG_ITALIC
            if style.bold():
                flags |= wx.FONTFLAG_BOLD
            if style.overstrike():
                flags |= wx.FONTFLAG_STRIKETHROUGH
            if style.underline():
                flags |= wx.FONTFLAG_UNDERLINED
            font = self._font_cache[key] = font = wx.FFont(size, wx.FONTFAMILY_DEFAULT, flags)
        return (Color(fg), Color(bg), font)

    def form(self):
        return self._form

    def update(self, *args, **kwargs):
        super(GridTable, self).update(*args, **kwargs)
        c = wx.Colour(config.grouping_background_downgrade)
        self._group_bg_downgrade = (255 - c.Red(), 255 - c.Green(), 255 - c.Blue())

    def close(self):
        self._form = None
        self._attr_cache = None
        self._font_cache = None
        super(GridTable, self).close()

    # Mandatory wx grid methods.

    def GetNumberRows(self):
        # We have to get only approximate number of rows here.  The reason is
        # that wx functions call this method on form creation.  Hopefully this
        # doesn't break anything.  Our code should use `number_of_rows'
        # directly anyway.
        return self.number_of_rows(timeout=0)

    def GetNumberCols(self):
        return self._column_count

    def IsEmptyCell(self, row, col):
        return False

    def GetValue(self, row, col):
        return self.cell_value(row, col) or ''

    # Optional wx grid methods.

    # def GetColLabelValue(self, col):
    # Now implemented in `ListForm._on_column_header_paint()'.

    def GetTypeName(self, row, col):
        # We pretend that everyting is a string in order to use our own
        # formatting for floats, dates, etc.  Also using wx.grid.GRID_VALUE_BOOL
        # causes a segfault on doubleclicking.
        return wx.grid.GRID_VALUE_STRING

    def GetAttr(self, row, col, kind):
        try:
            style = self.cell_style(row, col)
            if style:
                try:
                    fg, bg, font = self._attr_cache[style]
                except KeyError:
                    fg, bg, font = self._attr_cache[style] = self._make_attr(style)
                if self.group(row):
                    bg = wx.Colour(*[max(0, x - y) for x, y in zip(
                        (bg.Red(), bg.Green(), bg.Blue()),
                        self._group_bg_downgrade,
                    )])
                provider = self.GetAttrProvider()
                if provider:
                    attr = provider.GetAttr(row, col, kind)
                    if attr:
                        attr.SetTextColour(fg)
                        attr.SetBackgroundColour(bg)
                        attr.SetFont(font)
                        if self._columns[col].type.__class__ == pytis.data.Boolean:
                            attr.SetRenderer(CustomBooleanCellRenderer(self))
                        else:
                            attr.SetRenderer(CustomCellRenderer(self))
                        return attr
            return None
        except Exception:
            top_level_exception()


class TableRowIterator(object):
    """Create a table iterator which gradually returns given rows.

    Argumenty konstruktoru:
      table -- GridTable instance.
      row_numbers -- sequence of integers containing the row numbers to be iterated.

    The iteration returns PresentedRow instances corresponding to given row
    numbers.

    """
    def __init__(self, table, row_numbers):
        self._pointer = -1
        self._table = table
        self._row_numbers = row_numbers

    def __iter__(self):
        return self

    def __len__(self):
        return len(self._row_numbers)

    def next(self):
        self._pointer += 1
        if self._pointer >= len(self._row_numbers):
            raise StopIteration
        else:
            return self._table.row(self._row_numbers[self._pointer])

    def form(self):
        """Return the current form instance.

        This method is designed to be used in application code to get to the
        current form methods from the action handler function (which receives
        this iterator as an argument when action context is
        ActionContext.SELECTION).

        """
        return self._table.form()


class CustomCellRenderer(wx.grid.GridCellRenderer):
    """Custom renderer which highlights the current row using a rectangle.

    The base class doesn't allow using the default behavior and only add the
    rectangle, so we must drow everything ourselves as the default renderer
    would draw it.  Than we additionally drow the rectangle around the current
    row (where the grid cursor is located).

    """

    def __init__(self, table):
        self._table = table
        super(CustomCellRenderer, self).__init__()

    def _draw_value(self, value, dc, rect, align):
        label_rect = wx.Rect(rect.x + 1, rect.y, rect.width - 2, rect.height)
        dc.DrawLabel(value, label_rect, wx.ALIGN_CENTER_VERTICAL | align)

    def Draw(self, grid, attr, dc, rect, row, col, isSelected):
        "Customisation: Draw the data from grid in the rectangle with attributes using the dc."
        dc.SetClippingRegion(rect.x, rect.y, rect.width, rect.height)
        try:
            if isSelected:
                fg = wx.SystemSettings.GetColour(wx.SYS_COLOUR_HIGHLIGHTTEXT)
                bg = wx.SystemSettings.GetColour(wx.SYS_COLOUR_HIGHLIGHT)
            else:
                fg = attr.GetTextColour()
                bg = attr.GetBackgroundColour()
            dc.SetBrush(wx.Brush(bg, wx.BRUSHSTYLE_SOLID))
            dc.SetPen(wx.TRANSPARENT_PEN)
            dc.DrawRectangle(rect.x, rect.y, rect.width, rect.height)
            if grid.GetGridCursorRow() == row:
                original_pen = dc.GetPen()
                try:
                    border_width = config.row_highlight_width
                    if border_width != 0:
                        if grid.GetParent() is not Form.focused_form():
                            color = config.row_highlight_unfocused_color
                        else:
                            color = config.row_highlight_color
                        dc.SetPen(wx.Pen(color, border_width, wx.PENSTYLE_SOLID))
                        r, mod = divmod(border_width, 2)
                        x, y, width, height = rect
                        left, right, top, bottom = x, x + width, y + r, y + height - r - mod
                        dc.DrawLine(left, top, right, top)
                        dc.DrawLine(left, bottom, right, bottom)
                        if col == 0:
                            leftx = left + r + mod - 1
                            dc.DrawLine(leftx, top, leftx, bottom)
                        if col + 1 == grid.GetNumberCols():
                            dc.DrawLine(right - r - mod, top, right - r - mod, bottom)

                finally:
                    dc.SetPen(original_pen)
            dc.SetBackgroundMode(wx.TRANSPARENT)
            dc.SetTextForeground(fg)
            dc.SetFont(attr.GetFont())
            self._draw_value(grid.GetCellValue(row, col), dc, rect, attr.GetAlignment()[0])
        finally:
            dc.DestroyClippingRegion()


class CustomBooleanCellRenderer(CustomCellRenderer):

    def _draw_value(self, value, dc, rect, align):
        icon = get_icon(value == 'T' and 'checkbox-checked' or 'checkbox-unchecked')
        if icon:
            value = ''
        dc.DrawLabel(value, bitmap=icon, rect=rect, alignment=wx.ALIGN_CENTER_VERTICAL | align)
