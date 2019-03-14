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

# Terminologická poznámka: Proměnné s názvem `row' obvykle značí číslo řádku
# (číslováno od 0).  Jedná-li se o obsah řádku, nazývá se příslušná proměnná
# obvykle `the_row'.  Matoucí jméno `row' bylo převzato z wxWidgets.

import collections
import copy
import types

import wx
import wx.grid

import pytis.data
import pytis.output
import pytis.presentation
from pytis.presentation import PresentedRow
from pytis.util import DEBUG, EVENT, ProgramError, log

from .application import db_operation
from .form import Form
from .screen import color2wx, get_icon
from .event import top_level_exception

import config


class DataTable(object):
    # Požadavky na ni jsou následující:
    # - základní práce s tabulkovými daty
    # - schopnost práce s počítanými sloupci

    # Uvnitř třídy pracujeme zásadně s vnitřními hodnotami, nikoliv
    # hodnotami uživatelskými.  *Jediné* metody, které pracují
    # s uživatelskou reprezentací, jsou `GetValue' a `SetValue'.

    # Necachujeme žádná data, udržujeme pouze poslední řádek; cachování
    # většího množství dat vzhledem ke způsobu použití tabulky nedává
    # příliš velký smysl.

    class _CurrentRow:
        def __init__(self, row, the_row):
            assert isinstance(row, int)
            assert isinstance(the_row, PresentedRow)
            self.row = row
            self.the_row = the_row

    class _DisplayCache:
        def __init__(self, size=1000):
            self._size = size
            self._start_row = 0
            self._cache = self._allocate(size)

        def _allocate(self, size):
            return [None for i in range(size)]

        def __getitem__(self, row):
            try:
                index = row - self._start_row
                if index >= 0:
                    return self._cache[index]
                else:
                    raise IndexError()
            except IndexError:
                return None

        def __setitem__(self, row, the_row):
            start = self._start_row
            try:
                index = row - start
                if index >= 0:
                    self._cache[index] = the_row
                else:
                    raise IndexError()
            except IndexError:
                size = self._size
                end = start + size
                new_start = max(row - size / 2, 0)
                new_end = new_start + size
                if end <= new_start or new_end <= start:
                    cache = self._allocate(size)
                elif end < new_end:
                    diff = new_end - end
                    cache = self._cache[diff:] + self._allocate(diff)
                elif start > new_start:
                    diff = start - new_start
                    cache = self._allocate(diff) + self._cache[:-diff]
                else:
                    raise ProgramError(start, end, new_start, new_end)
                assert len(cache) == self._size, len(cache)
                self._start_row = new_start
                cache[row - new_start] = the_row
                self._cache = cache

    class _Column(object):
        def __init__(self, id_, type_, label, style):
            self.id = id_
            self.type = type_
            self.label = label
            self.style = style

    _DEFAULT_FOREGROUND_COLOR = pytis.presentation.Color.BLACK
    _DEFAULT_BACKGROUND_COLOR = pytis.presentation.Color.WHITE

    def __init__(self, form, data, presented_row, columns, row_count,
                 sorting=(), grouping=(), prefill=None, row_style=None):
        assert isinstance(form, Form)
        assert isinstance(grouping, types.TupleType)
        self._form = form
        self._data = data
        self._presented_row = copy.copy(presented_row)
        self._row_count = row_count
        self._sorting = sorting
        self._grouping = grouping
        self._prefill = prefill
        self._current_row = None
        self._row_style = row_style
        self._plain_style = pytis.presentation.Style()
        self._update_columns(columns)
        # Create caches
        self._cache = self._DisplayCache()
        self._attr_cache = {}
        self._font_cache = {}
        self._group_cache = {0: False}
        self._group_value_cache = {}
        self.rewind()

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

    def _get_row(self, row, require=True):
        """Return the row number 'row' from the database as a 'PresentedRow' instance.

        Arguments:

          row -- row number within the *database select*, starting from 0
          require -- when true, the requested row must exist, otherwise 'None'
            is returned if the given row doesn't exist

        """
        success, result = db_operation(self._retrieve_row, row, require=require)
        if not success:
            self._panic()
        return result

    def _retrieve_row(self, row, require=True):
        data = self._data

        def fetch(row, direction=pytis.data.FORWARD):
            result = self._data.fetchone(direction=direction)
            if result is None:
                # In theory this shouldn't happen but it actually happens so we
                # have to attempt some workaround here.
                data.rewind()
                if row > 0:
                    data.skip(row - 1, direction=pytis.data.FORWARD)
                result = data.fetchone(direction=pytis.data.FORWARD)
                if result is None:
                    # This shouldn't happen at all but it still happens.
                    log(DEBUG, "Missing grid row")
                    if require:
                        raise Exception('Missing row', row)
                    return None
                else:
                    log(DEBUG, "Grid data synchronization error")
            self._presented_row.set_row(result)
            the_row = copy.copy(self._presented_row)
            self._current_row = self._CurrentRow(row, the_row)
        current = self._current_row
        if not current:
            data.rewind()
            if row > 0:
                # Tento fetch pouze zabezpečí přednačtení bufferu v dopředném
                # směru od začátku dat.  To je potřeba, protože grid
                # načítá řádky od konce a bez tohoto hacku by buffer obsahoval
                # pouze zobrazené řádky.  Lépe by to však bylo ošetřit lepší
                # strategií plnění bufferu v dbdata.py ...
                fetch(0)
                data.skip(row - 1, direction=pytis.data.FORWARD)
            fetch(row)
        elif row != current.row:
            last = data.last_row_number()
            if row > last:
                skip = row - last - 1
                direction = pytis.data.FORWARD
            elif row == last:
                data.skip(1, pytis.data.BACKWARD)
                skip = 0
                direction = pytis.data.FORWARD
            else:
                skip = last - row - 1
                direction = pytis.data.BACKWARD
            data.skip(skip, direction=direction)
            fetch(row, direction)
        return self._current_row.the_row

    def _group(self, row):
        # Return true, if given row belongs to a highlighted group
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
                    prev_group = self._group(row - 1)
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
                    next_group = self._group(row + 1)
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

    # Public methods

    def row(self, row):
        """Vrať řádek číslo 'row' jako instanci třídy 'PresentedRow'.

        Vrácený řádek zahrnuje změny provedené případnou editací a
        obsahuje pouze sloupce datového objektu (nepočítané i počítané),
        takže je možné jej přímo použít v databázových operacích.

        Jestliže řádek daného čísla neexistuje, vrať 'None'.

        Argumenty:

        row -- nezáporný integer, první řádek má číslo 0

        """
        if row >= 0 and row < self.number_of_rows(min_value=(row + 1)):
            return self._get_row(row, require=False)
        else:
            return None

    def rewind(self, position=None):
        """Přesuň datové ukazovátko na začátek dat.

        Jestliže 'position' není 'None', přesuň ukazovátko na 'position'.

        """
        if self._current_row is None:
            return
        if position is None:
            self._data.rewind()
            self._cache = self._DisplayCache()
            self._current_row = None
        elif position < -1 or position >= self.number_of_rows() - 1:
            pass
        else:
            row = position
            success, result = db_operation(self._retrieve_row, row)
            if not success:
                self._panic()
            self._presented_row.set_row(result)
            self._current_row = self._CurrentRow(row, copy.copy(self._presented_row))

    def form(self):
        return self._form

    def update(self, columns, row_count, sorting, grouping, prefill):
        assert isinstance(grouping, tuple)
        self._row_count = row_count
        self._sorting = sorting
        self._grouping = grouping
        self._prefill = prefill
        self._update_columns(columns)
        # Smaž cache
        self._group_cache = {0: False}
        self._group_value_cache = {}
        # Nastav řádek
        self.rewind()

    def close(self):
        # Tato metoda je nutná kvůli jistému podivnému chování wxWidgets,
        # kdy wxWidgets s tabulkou pracuje i po jejím zrušení.
        self._data = None
        # TODO: Následující (a možná i ta předcházející) operace jsou
        # jsou v principu zbytečné, ale protože z neznámých důvodů
        # nedochází při uzavření formuláře k likvidaci nějakých blíže
        # neurčených dat, patrně i z této tabulky, tak raději významná
        # data instance mažeme ručně...
        self._form = None
        self._fields = None
        self._columns = None
        self._cache = None
        self._attr_cache = None
        self._font_cache = None
        self._group_cache = None
        self._group_value_cache = None
        self._presented_row = None
        self._current_row = None
        self._row_style = None
        self._group_bg_downgrade = None
        self._group_bg_color = None

    def _cached_value(self, row, col_id, style=False):
        # Return the cached value for given row and column id.
        #
        # The value returned is the formatted cell value by default or a
        # computed style, when the keyword argument style is true.
        # This is a little tricky, but the reason is to cache everithing
        # once we read the row value, because we can not cache the rows
        # inside the 'row()' method.
        #
        cached_things = self._cache[row]
        if cached_things is None:
            the_row = self.row(row)
            style_dict = {}
            value_dict = {}
            # Cache the values and styles for all columns at once.
            for c in self._columns:
                cid = c.id
                s = c.style
                if isinstance(s, collections.Callable):
                    style_dict[cid] = s(the_row)
                value_dict[cid] = the_row.format(cid, pretty=True, form=self._form, secure=True)
            # Grouping column may not be in self._columns.
            for gcol in self._grouping:
                if gcol not in value_dict:
                    value_dict[gcol] = the_row.format(gcol, pretty=True, form=self._form,
                                                      secure=True)
            # If row_style is defined, lets compute it.
            if isinstance(self._row_style, collections.Callable):
                protected_row = the_row.protected()
                try:
                    row_style = self._row_style(protected_row)
                except protected_row.ProtectionError:
                    row_style = None
                style_dict[None] = row_style
            self._cache[row] = cached_things = [value_dict, style_dict]
        cached_row = cached_things[style and 1 or 0]
        return cached_row[col_id]

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

    def number_of_columns(self):
        return self._column_count


class ListTable(wx.grid.GridTableBase, DataTable):

    def __init__(self, form, data, presented_row, columns, row_count,
                 sorting=(), grouping=(), prefill=None, row_style=None):
        assert isinstance(form, Form)
        assert isinstance(grouping, types.TupleType)
        wx.grid.GridTableBase.__init__(self)
        DataTable.__init__(self, form, data, presented_row, columns, row_count,
                           sorting=sorting, grouping=grouping, prefill=prefill, row_style=row_style)
        self._init_group_bg_downgrade()

    # Pomocné metody

    def _panic(self):
        DataTable._panic(self)
        Form.COMMAND_LEAVE_FORM.invoke()

    def _make_attr(self, style):
        flags = wx.FONTFLAG_DEFAULT
        fg, bg = (self._DEFAULT_FOREGROUND_COLOR, self._DEFAULT_BACKGROUND_COLOR)
        if style:
            if style.slanted():
                flags |= wx.FONTFLAG_ITALIC
            if style.bold():
                flags |= wx.FONTFLAG_BOLD
            if style.overstrike():
                flags |= wx.FONTFLAG_STRIKETHROUGH
            if style.underline():
                flags |= wx.FONTFLAG_UNDERLINED
            if style.foreground():
                fg = style.foreground()
            if style.background():
                bg = style.background()
        flags |= wx.FONTFLAG_STRIKETHROUGH
        try:
            font = self._font_cache[flags]
        except KeyError:
            size = self._form.GetFont().GetPointSize()
            font = self._font_cache[flags] = font = wx.FFont(size, wx.FONTFAMILY_DEFAULT, flags)
        return (color2wx(fg), color2wx(bg), font)

    def _init_group_bg_downgrade(self):
        c = wx.Colour(config.grouping_background_downgrade)
        self._group_bg_downgrade = (255 - c.Red(), 255 - c.Green(), 255 - c.Blue())

    def update(self, *args, **kwargs):
        super(ListTable, self).update(*args, **kwargs)
        self._init_group_bg_downgrade()

    # Povinné wx gridové metody

    def GetNumberRows(self):
        # We have to get only approximate number of rows here.  The reason is
        # that wx functions call this method on form creation.  Hopefully this
        # doesn't break anything.  Our code should use `number_of_rows'
        # directly anyway.
        return self.number_of_rows(timeout=0)

    def GetNumberCols(self):
        return self.number_of_columns()

    def IsEmptyCell(self, row, col):
        return False

    def GetValue(self, row, col):
        # `row' and `col' are numbered from 0.
        if self._data is not None and col < self.GetNumberCols():
            return self._cached_value(row, self._columns[col].id)
        else:
            return ''

    # Nepovinné wx gridové metody

    # def GetColLabelValue(self, col):
    # Nyní implementováno pomocí `ListForm._on_column_header_paint()'.

    def GetTypeName(self, row, col):
        # wx.grid.GRID_VALUE_BOOL causes segfault on doubleclicking a column
        # and float is avoided in favor of our own numeric value formatting, so
        # we rather blaim the grid that everyting is a string.  Bool values are
        # rendered using a custom renderer...
        return wx.grid.GRID_VALUE_STRING

    def GetAttr(self, row, col, kind):
        try:
            if row >= self.number_of_rows(min_value=(row + 1)) or col >= self.number_of_columns():
                # it may happen
                return None
            column = self._columns[col]
            if column.id in self._secret_columns:
                style = self._plain_style
            else:
                style = column.style
                if isinstance(style, collections.Callable):
                    style = self._cached_value(row, column.id, style=True)
            row_style = self._row_style
            if isinstance(row_style, collections.Callable):
                row_style = self._cached_value(row, None, style=True)
            if row_style:
                style += row_style
            try:
                fg, bg, font = self._attr_cache[style]
            except KeyError:
                fg, bg, font = self._attr_cache[style] = self._make_attr(style)
            if self._group(row):
                rgb = (bg.Red(), bg.Green(), bg.Blue())
                bg = wx.Colour(*[max(0, x - y) for x, y in zip(rgb, self._group_bg_downgrade)])
            provider = self.GetAttrProvider()
            if provider:
                attr = provider.GetAttr(row, col, kind)
                if attr:
                    attr.SetTextColour(fg)
                    attr.SetBackgroundColour(bg)
                    attr.SetFont(font)
                    if column.type.__class__ == pytis.data.Boolean:
                        attr.SetRenderer(CustomBooleanCellRenderer(self))
                    else:
                        attr.SetRenderer(CustomCellRenderer(self))
                    return attr
            return None
        except Exception:
            top_level_exception()


class TableRowIterator(object):
    """Vytvoří iterátor nad tabulkou, který postupně vrací určené řádky.

    Argumenty konstruktoru:
      table -- instance ListTable
      rows -- sekvence celých čísel určujících jednotlivé řádky

    """
    def __init__(self, table, rows):
        self._pointer = -1
        self._table = table
        self._rows = rows

    def __iter__(self):
        return self

    def __len__(self):
        return len(self._rows)

    def next(self):
        self._pointer += 1
        if self._pointer >= len(self._rows):
            raise StopIteration
        else:
            return self._table.row(self._rows[self._pointer])

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
