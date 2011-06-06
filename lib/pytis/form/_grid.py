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

"""Pomocn� t��dy pro seznamov� formul��e."""

# Terminologick� pozn�mka: Prom�nn� s�n�zvem `row' obvykle zna�� ��slo ��dku
# (��slov�no od�0).  Jedn�-li se o�obsah ��dku, naz�v� se p��slu�n� prom�nn�
# obvykle `the_row'.  Matouc� jm�no `row' bylo p�evzato z�wxWindows.

import collections
import copy
import time
import types

import wx
import wx.grid

from pytis.form import *
import pytis.data
import pytis.output
import pytis.presentation
from pytis.presentation import PresentedRow

import config

class DataTable(object):
    # Tato t��da nen� a� tak trivi�ln�, jak bychom si mo�n� p��li.
    # Po�adavky na ni jsou n�sleduj�c�:
    # - z�kladn� pr�ce s�tabulkov�mi daty
    # - mo�nost p��tomnosti jednoho ��dku nav�c, nep��tomn�ho v�datech, na
    #   n�jak�m m�st� v�tabulce
    # - znalost p�vodn�ch dat editovan�ho ��dku
    # - znalost aktu�ln�ch (zeditovan�ch) dat editovan�ho ��dku
    # - znalost a� t�� hodnot pr�v� editovan�ho pol��ka: hodnota p�vodn�
    #   (z�datab�ze), platn� hodnota zeditovan�, odeslan� nezvalidovan�
    #   hodnota ur�en� k�opravn� editaci u�ivatelem
    # - schopnost pr�ce s�po��tan�mi sloupci
    
    # Uvnit� t��dy pracujeme z�sadn� s�vnit�n�mi hodnotami, nikoliv
    # hodnotami u�ivatelsk�mi.  *Jedin�* metody, kter� pracuj�
    # s�u�ivatelskou reprezentac�, jsou `GetValue' a `SetValue'.
    
    # Necachujeme ��dn� data, udr�ujeme pouze posledn� ��dek a data
    # o�editaci jednoho ��dku; cachov�n� v�t��ho mno�stv� dat vzhledem ke
    # zp�sobu pou�it� tabulky ned�v� p��li� velk� smysl.
    
    class _CurrentRow:
        def __init__(self, row, the_row):
            assert type(row) == type(0)
            assert isinstance(the_row, PresentedRow)
            self.row = row
            self.the_row = the_row
            
    class _EditedRow(_CurrentRow):
        def __init__(self, row, data_row, record):
            assert data_row is None or isinstance(data_row, pytis.data.Row)
            DataTable._CurrentRow.__init__(self, row, record)
            self.orig_row = copy.copy(data_row)
        def update(self, colid, value):
            self.the_row[colid] = value    

    class EditInfo:
        def __init__(self, row, the_row, orig_row):
            self.row = row
            self.the_row = the_row
            self.orig_row = orig_row

    class _DisplayCache:
        def __init__(self, size=1000):
            self._size = size
            self._start_row = 0
            self._cache = self._allocate(size)
        def _allocate(self, size):
            return map(lambda __: None, range(size))
        def __getitem__(self, row):
            try:
                index = row-self._start_row
                if index >= 0:
                    return self._cache[index]
                else:
                    raise IndexError()
            except IndexError:
                return None
        def __setitem__(self, row, the_row):
            start = self._start_row
            try:
                index = row-start
                if index >= 0:
                    self._cache[index] = the_row
                else:
                    raise IndexError()
            except IndexError:
                size = self._size
                end = start + size
                new_start = max(row - size/2, 0)
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
                cache[row-new_start] = the_row
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
        # Zpracuj sloupce
        self._update_columns(columns)
        # Vytvo� cache
        self._cache = self._DisplayCache()
        self._attr_cache = {}
        self._font_cache = {}
        self._group_cache = {0: False}
        self._group_value_cache = {}
        # Nastav ��dek
        self.rewind()
        self._edited_row = None

    def _init_edited_row(self, row_number, data_row=None, prefill=None, new=False):
        assert data_row is None or isinstance(data_row, pytis.data.Row)
        if prefill is None:
            prefill = self._prefill
        record = self._form.record(data_row, new=new, singleline=True, prefill=prefill)
        return self._EditedRow(row_number, data_row, record)

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
            log(DEBUG, 'Zpanika�en� gridov� tabulky')

    def _get_row(self, row, autoadjust=False):
        """Return the row number 'row' from the database as a 'PresentedRow' instance.
        
        Arguments:
        
        row -- row number within the *database select*, starting from�0
        autoadjust -- when true, 'row' is decreased by one if it is located behind an edited *new*
          row.  Also when the 'row', equals to the number of the currently edited row (whether new
          or existing), this edited row is returned.

        """
        edited = self._edited_row
        if edited and row == edited.row:
            return edited.the_row
        elif edited and autoadjust and edited.the_row.new() and \
                 row > edited.row:
            row_ = row - 1
        else:
            row_ = row
        success, result = db_operation(self._retrieve_row, row_)
        if not success:
            self._panic()
        return result

    def _retrieve_row(self, row):
        def fetch(row, direction=pytis.data.FORWARD):
            result = self._data.fetchone(direction=direction)
            assert result, ('Missing row', row)
            self._presented_row.set_row(result)
            the_row = copy.copy(self._presented_row)
            self._current_row = self._CurrentRow(row, the_row)
        current = self._current_row
        if not current:
            data = self._data
            data.rewind()
            if row > 0:
                # Tento fetch pouze zabezpe�� p�edna�ten� bufferu v dop�edn�m
                # sm�ru od za��tku dat.  To je pot�eba, proto�e grid 
                # na��t� ��dky od konce a bez tohoto hacku by buffer obsahoval
                # pouze zobrazen� ��dky.  L�pe by to v�ak bylo o�et�it lep��
                # strategi� pln�n� bufferu v dbdata.py ...
                fetch(0)
                data.skip(row-1, direction=pytis.data.FORWARD)
            fetch(row)
        elif row != current.row:
            data = self._data
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
                    prev_values = cached_values(row-1, grouping)
                    prev_group = self._group(row-1)
                    if values == prev_values:
                        result = prev_group
                    else:
                        result = not prev_group
                    self._group_value_cache[values] = result
                    self._group_cache[row] = result
                    return result
                higher = filter(lambda k: k > row, cached)
                if len(higher) and min(higher) - row < 80:
                    next_values = cached_values(row+1, grouping)
                    next_group = self._group(row+1)
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

    ## Public methods

    def row(self, row):
        """Vra� ��dek ��slo 'row' jako instanci t��dy 'PresentedRow'.
        
        Vr�cen� ��dek zahrnuje zm�ny proveden� p��padnou editac� a
        obsahuje pouze sloupce datov�ho objektu (nepo��tan� i�po��tan�),
        tak�e je mo�n� jej p��mo pou��t v�datab�zov�ch operac�ch.
        
        Jestli�e ��dek dan�ho ��sla neexistuje, vra� 'None'.
        
        Argumenty:
        
        row -- nez�porn� integer, prvn� ��dek m� ��slo�0
        
        """
        if row < 0 or row >= self.number_of_rows(min_value=row+1):
            return None
        return self._get_row(row, autoadjust=True)

    def rewind(self, position=None):
        """P�esu� datov� ukazov�tko na za��tek dat.
        
        Jestli�e 'position' nen� 'None', p�esu� ukazov�tko na 'position'.
        
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
    
    def update(self, columns, row_count, sorting, grouping, inserted_row_number,
               inserted_row_prefill, prefill):
        assert isinstance(grouping, tuple)
        self._row_count = row_count
        self._sorting = sorting
        self._grouping = grouping
        self._prefill = prefill
        self._update_columns(columns)
        # Sma� cache
        self._group_cache = {0: False}
        self._group_value_cache = {}
        # Nastav ��dek
        self.rewind()
        if inserted_row_number is None:
            self._edited_row = None
        else:
            self._edited_row = self._init_edited_row(inserted_row_number, prefill=inserted_row_prefill, new=True)
        
    def close(self):
        # Tato metoda je nutn� kv�li jist�mu podivn�mu chov�n� wxWindows,
        # kdy wxWindows s�tabulkou pracuje i�po jej�m zru�en�.
        self._data = None
        # TODO: N�sleduj�c� (a�mo�n� i�ta p�edch�zej�c�) operace jsou
        # jsou v principu zbyte�n�, ale proto�e z�nezn�m�ch d�vod�
        # nedoch�z� p�i uzav�en� formul��e k�likvidaci n�jak�ch bl�e
        # neur�en�ch dat, patrn� i�z�t�to tabulky, tak rad�ji v�znamn�
        # data instance ma�eme ru�n�...
        self._form = None
        self._fields = None
        self._columns = None
        self._cache = None
        self._attr_cache = None
        self._font_cache = None
        self._group_cache = None
        self._group_value_cache = None
        self._edited_row = None
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
                    value_dict[gcol] = the_row.format(gcol, pretty=True, form=self._form, secure=True)
            # If row_style is defined, lets compute it.
            if isinstance(self._row_style, collections.Callable):
                style_dict[None] = self._row_style(the_row)
            self._cache[row] = cached_things = [value_dict, style_dict]
        cached_row = cached_things[style and 1 or 0]
        return cached_row[col_id]

    def edit_row(self, row):
        """Zahaj editaci ��dku ��slo 'row'.
        
        Pokud ji� n�jak� ��dek editov�n je, jeho editace je zru�ena a obsah
        vr�cen do p�vodn�ho stavu (ov�em bez p�ekreslen�, to mus� b�t
        zaji�t�no jinak!).
        
        Argumenty:
        
          row -- nez�porn� integer ur�uj�c� ��slo editovan�ho ��dku
            po��naje od�0, nebo 'None' zna��c� �e nem� b�t editov�n ��dn�
            ��dek

        """
        if row is None:
            self._edited_row = None
        else:
            assert row >= 0 and row < self.number_of_rows(min_value=row), ('Invalid row number', row)
            self._edited_row = self._init_edited_row(row, data_row=self._get_row(row).row())

    def editing(self):
        """Vra� informaci o�editovan�m ��dku nebo 'None'.
        
        Pokud nen� editov�n ��dn� ��dek, vra� 'None'.  Jinak vra� instanci
        t��dy 'EditInfo' s�informacemi o���sle editovan�ho ��dku, zda je
        tento ��dek nov�, zda je jeho aktu�ln� obsah r�zn� od jeho
        p�vodn�ho obsahu, zda jsou v�echny sloupce validn�, p�vodn� obsah
        ��dku (jako instanci 'pytis.data.Row') a aktu�ln� obsah ��dku (jako
        instanci 'PresentedRow').

        """
        edited = self._edited_row
        if edited:
            the_row = edited.the_row
            if the_row.new():
                orig_row = None
            else:
                orig_row = edited.orig_row
            return self.EditInfo(edited.row, the_row, orig_row)
        else:
            return None

    def column_id(self, col):
        return self._columns[col].id
        
    def column_label(self, col):
        return self._columns[col].label

    def current_row(self):
        """Vra� ��slo aktu�ln�ho ��dku datov�ho objektu tabulky.
        
        ��dky jsou ��slov�ny od�0.  Pokud ��slo aktu�ln�ho ��dku nen�
        zn�mo, vra� 'None'.

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
            

class ListTable(wx.grid.PyGridTableBase, DataTable):    
            
    class _Column(DataTable._Column):
        
        _TYPE_MAPPING = None
        
        def __init__(self, id_, type_, label, style):
            DataTable._Column.__init__(self, id_, type_, label, style)
            self.wxtype = self._wx_type(type_)
            
        def _wx_type(self, t):
            if self._TYPE_MAPPING is None:
                # Mus�me inicializovat a� zde kv�li neXov�mu serveru.
                # Nepou��v�me mapov�n� pro Float, proto�e to by n�m zru�ilo
                # na�e form�tov�n� ��sel.
                self.__class__._TYPE_MAPPING = {pytis.data.Boolean: wx.grid.GRID_VALUE_BOOL}
            return self._TYPE_MAPPING.get(t.__class__, wx.grid.GRID_VALUE_STRING)
                            
    def __init__(self, form, data, presented_row, columns, row_count,
                 sorting=(), grouping=(), prefill=None, row_style=None):
        assert isinstance(form, Form)
        assert isinstance(grouping, types.TupleType)
        wx.grid.PyGridTableBase.__init__(self)
        DataTable.__init__(self, form, data, presented_row, columns, row_count,
                           sorting=sorting, grouping=grouping, prefill=prefill, row_style=row_style)
        self._init_group_bg_downgrade()
        
    # Pomocn� metody
        
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
            font = self._font_cache[flags] = font = wx.FFont(size, wx.DEFAULT, flags)
        return (color2wx(fg), color2wx(bg), font)

    def _init_group_bg_downgrade(self):
        c = wx.NamedColor(config.grouping_background_downgrade)
        self._group_bg_downgrade = (255-c.Red(), 255-c.Green(), 255-c.Blue())
        
    def update(self, *args, **kwargs):
        super(ListTable, self).update(*args, **kwargs)
        self._init_group_bg_downgrade()

    # Povinn� wx gridov� metody
    
    def GetNumberRows(self):
        ## We have to get only approximate number of rows here.  The reason is
        ## that wx functions call this method on form creation.  Hopefully this
        ## doesn't break anything.  Our code should use `number_of_rows'
        ## directly anyway.
        return self.number_of_rows(timeout=0)
    
    def GetNumberCols(self):
        return self.number_of_columns()
    
    def IsEmptyCell(self, row, col):
        return False
    
    def GetValue(self, row, col, inputfield=False):
        # `row' a `col' jsou ��slov�ny od 0.
        # Je tabulka ji� uzav�ena?
        if not self._data or col >= self.GetNumberCols():
            return ''
        column = self._columns[col]
        if self._edited_row and row == self._edited_row.row:
            the_row = self._edited_row.the_row
            if the_row is None:
                return ''
            value = the_row.format(column.id, secure=True)
        else:
            value = self._cached_value(row, column.id)
        if not inputfield and column.wxtype == wx.grid.GRID_VALUE_BOOL:
            # wx pro boolean sloupce rozezn�v� pouze n�sleduj�c� *stringov�* hodnoty:
            if value == 'T':
                value = '1'
            else:
                value = ''
        return value

    def SetValue(self, row, col, value):
        # Tato metoda neodpov�d� specifikaci gridu, ale to nevad�, proto�e
        # pol��ka editujeme v�hradn� p�es na�e editory.
        assert isinstance(value, pytis.data.Value), ('Value not a value', value)
        edited = self._edited_row
        if edited == None:
            # K�t�to situaci doch�z�, kdy� se kliknut�m my�i opou�t�
            # rozeditovan� pol��ko ��dku, jemu� je�t� nebyla zm�n�na ��dn�
            # hodnota.  V�takov�m p��pad� na�e metody editaci nakr�sno
            # ukon�� a wxWindows po proveden� zm�n� ��dku vesele zavol�
            # SetValue�...
            return
        # Nastav hodnotu editovan�mu sloupci
        cid = self._columns[col].id
        edited.update(cid, value)
        log(EVENT, 'Nastavena hodnota editovan�ho pol��ka:',
            (row, col, value))

    # Nepovinn� wx gridov� metody

    #def GetColLabelValue(self, col):
    # Nyn� implementov�no pomoc� `ListForm._on_column_header_paint()'.

    def GetTypeName(self, row, col):
        # wx.grid.GRID_VALUE_BOOL causes segfault on doubleclicking a column, so we rather blaim 
        # the grid that everyting is a string and use a custom renderer for boolean columns...
        #if col < self.GetNumberCols():
        #    return self._columns[col].wxtype
        #else:
        return wx.grid.GRID_VALUE_STRING
    
    def GetAttr(self, row, col, kind):
        if row >= self.number_of_rows(min_value=row+1) or col >= self.number_of_columns(): # m��e se st�t...
            return None
        column = self._columns[col]
        if column.id in self._secret_columns:
            style = self._plain_style
        else:
            row_style = self._row_style
            if isinstance(row_style, collections.Callable):
                row_style = self._cached_value(row, None, style=True)
            style = column.style
            if isinstance(style, collections.Callable):
                style = self._cached_value(row, column.id, style=True)
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
                if column.wxtype == wx.grid.GRID_VALUE_BOOL:
                    attr.SetRenderer(wx.grid.GridCellBoolRenderer())
                return attr
        return None


class TableRowIterator(object):
    """Vytvo�� iter�tor nad tabulkou, kter� postupn� vrac� ur�en� ��dky.

    Argumenty konstruktoru:
      table -- instance ListTable
      rows -- sekvence cel�ch ��sel ur�uj�c�ch jednotliv� ��dky
    
    """
    def __init__(self, table, rows):
        self._pointer = -1
        self._table = table
        self._rows = rows
        
    def __iter__(self):
        return self

    def next(self):
        self._pointer += 1
        if self._pointer >= len(self._rows):
            raise StopIteration
        else:
            return self._table.row(self._rows[self._pointer])
        
    

class InputFieldCellEditor(wx.grid.PyGridCellEditor):

    def __init__(self, parent, row, id, guardian, registration):
        wx.grid.PyGridCellEditor.__init__(self)
        self._parent = parent
        self._row = row
        self._id = id
        self._guardian = guardian
        self._registration = registration
        self._field = None

    # Povinn� metody

    def Create(self, parent, id, evt_handler):
        self._field = InputField.create(parent, self._row, self._id,
                                        guardian=self._guardian, inline=True)
        control = self._field.widget()
        self.SetControl(control)
        return control

    def BeginEdit(self, row, col, grid):
        self._registration(self)
        field = self._field
        field.widget().Enable(True)
        field.set_focus()
        try:
            field.widget().SetInsertionPointEnd()
        except AttributeError:
            pass
        
    def EndEdit(self, row, col, grid):
        field = self._field
        field.validate(interactive=False)
        field.widget().Enable(False)
        self._parent.SetFocus()
        self._registration(None)
        return True

    def Reset(self):
        self._field.reset()
        
    def Clone(self):
        return InputFieldCellEditor()

    # Ostatn� metody
    
    def field(self):
        """Vra� sv�j 'InputField'."""
        return self._field
        
    def IsAcceptedKey(self, event):
        # TODO/wx: Z�nezn�m�ch d�vod� nen� vol�no.
        if __debug__: log(DEBUG, 'Neuv��iteln� -- vol�no IsAcceptedKey')
        return False
    
    def close(self):
        """Prove� ukon�ovac� akce.

        Tuto metodu je nutno volat explicitn�, nebo� definov�n� metod
        `Close()' a `Destroy()' nem� ��dn� ��inek.

        """
        self.SetControl(None)

    
