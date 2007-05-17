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

"""Pomocn� t��dy pro seznamov� formul��e."""

# Terminologick� pozn�mka: Prom�nn� s�n�zvem `row' obvykle zna�� ��slo ��dku
# (��slov�no od�0).  Jedn�-li se o�obsah ��dku, naz�v� se p��slu�n� prom�nn�
# obvykle `the_row'.  Matouc� jm�no `row' bylo p�evzato z�wxWindows.

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

class ListTable(wx.grid.PyGridTableBase):
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
        def __init__(self, row, the_row, fieldspec, data, new=False, prefill=None):
            if __debug__ and config.server:
                import pytis.remote
            # TODO: Pro� tu nen� pytis.data, i kdy� je naho�e import pytis.data?
            # Tak to importneme tady...
            import pytis.data    
            assert type(row) == type(0)
            assert the_row is None or \
                   isinstance(the_row, pytis.data.Row) or \
                   isinstance(the_row, PresentedRow)
            assert is_sequence(fieldspec)
            assert isinstance(data, pytis.data.Data) or \
                   isinstance (data, pytis.remote.RemoteData)
            p_row = PresentedRow(fieldspec, data, the_row,
                                 prefill=prefill, singleline=True, new=new)
            ListTable._CurrentRow.__init__(self, row, p_row)
            self.orig_row = copy.copy(the_row)
        def update(self, colid, value):
            self.the_row[colid] = value
            
    class _Column:
        def __init__(self, id, wxtype, label, style):
            self.id = id
            self.wxtype = wxtype
            self.label = label
            self.style = style

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
                    
    _TYPE_MAPPING = None
        
    def __init__(self, frame, presented_row, columns, row_count, sorting=(), grouping=(),
                 inserted_row_number=None, inserted_row=None, prefill=None, row_style=None):
        assert isinstance(grouping, types.TupleType)
        wx.grid.PyGridTableBase.__init__(self)
        self._frame = frame
        self._presented_row = presented_row
        self._data = presented_row.data()
        self._fields = presented_row.fields()
        self._row_count = row_count
        self._sorting = sorting
        self._grouping = grouping
        self._prefill = prefill
        self._current_row = None
        self._row_style = row_style
        # Zpracuj sloupce
        self._update_columns(columns)
        # Vytvo� cache
        self._cache = self._DisplayCache()
        self._attr_cache = {}
        self._font_cache = {}
        self._group_cache = {0: False}
        self._group_value_cache = {}
        self._grouping_background_downgrade = self._group_bg_downgrade()
        # Nastav ��dek
        self.rewind()
        if inserted_row_number is None:
            self._edited_row = None
        else:
            self._edited_row = self._init_edited_row(inserted_row_number, inserted_row)

    def _init_edited_row(self, row, data_row, new=False):
        return self._EditedRow(row, data_row, self._fields, self._data, new=new,
                               prefill=self._prefill)
        
    # Pomocn� metody

    def _wx_type(self, t):
        if self._TYPE_MAPPING is None:
            # Mus�me inicializovat a� zde kv�li neXov�mu serveru.
            # Nepou��v�me mapov�n� pro Float, proto�e to by n�m zru�ilo
            # na�e form�tov�n� ��sel.
            self.__class__._TYPE_MAPPING = \
                    {pytis.data.Boolean: wx.grid.GRID_VALUE_BOOL}
        return self._TYPE_MAPPING.get(t.__class__, wx.grid.GRID_VALUE_STRING)

    def _update_columns(self, columns):
        self._columns = [self._Column(c.id(),
                                      self._wx_type(c.type(self._data)),
                                      c.column_label() or '',
                                      c.style())
                         for c in columns]
        self._column_count = len(self._columns)
        
    def _panic(self):
        if __debug__: log(DEBUG, 'Zpanika�en� gridov� tabulky')
        Form.COMMAND_LEAVE_FORM.invoke()

    def _get_row(self, row, autoadjust=False):
        """Vra� ��dek ��slo 'row' z�datab�ze jako instanci 'pytis.data.Row'.
        
        Argumenty:
        
        row -- po�adov� ��slo ��dku *v�datab�zov�m selectu*, po��naje�0
        autoadjust -- pr�v� kdy� je pravdiv�, je 'row' sn��eno
          o�jedni�ku, nach�z�-li se za editovan�m *nov�m* ��dkem, a�pokud
          je shodno s���slem editovan�ho ��dku (a� u� nov�ho �i pouze
          modifikovan�ho), je vr�cen editovan� ��dek

        Pozor: Pokud je po�adovan� ��dek pr�v� editovan�m ��dkem, je vr�cen
        jako instance 'PresentedRow'.

        """
        edited = self._edited_row
        if edited and row == edited.row:
            return edited.the_row
        elif edited and autoadjust and edited.the_row.new() and \
                 row > edited.row:
            row_ = row - 1
        else:
            row_ = row
        success, result = db_operation(lambda: self._retrieve_row(row_))
        if not success:
            self._panic()
        return result

    def _retrieve_row(self, row):
        def fetch(row, direction=pytis.data.FORWARD):
            result = self._data.fetchone(direction=direction,
                                         transaction=self._presented_row.transaction())
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
        
    def _make_attr(self, style):
        if style.slanted():
            slant = wx.ITALIC
        else:
            slant = wx.NORMAL
        if style.bold():
            weight = wx.BOLD
        else:
            weight = wx.NORMAL
        font_key = (slant, weight)
        try:
            font = self._font_cache[font_key]
        except KeyError:
            size = self._frame.GetFont().GetPointSize()
            font = self._font_cache[font_key] = \
                   font = wx.Font(size, wx.DEFAULT, slant, weight)
        return (color2wx(style.foreground()), color2wx(style.background()),font)

    # Na�e ve�ejn� metody

    def _group_bg_downgrade(self):
        d = wx.NamedColor(config.grouping_background_downgrade)
        return (255-d.Red(), 255-d.Green(), 255-d.Blue())
    
    def update(self, columns, row_count, sorting, grouping,
               inserted_row_number, inserted_row, prefill):
        assert isinstance(grouping, types.TupleType)
        self._update_columns(columns)
        self._row_count = row_count
        self._sorting = sorting
        self._grouping = grouping
        self._prefill = prefill
        # Sma� cache
        self._group_cache = {0: False}
        self._group_value_cache = {}
        self._grouping_background_downgrade = self._group_bg_downgrade()
        # Nastav ��dek
        self.rewind()
        if inserted_row_number is None:
            self._edited_row = None
        else:
            self._edited_row = self._init_edited_row(inserted_row_number, inserted_row, new=True)
        
    def close(self):
        # Tato metoda je nutn� kv�li jist�mu podivn�mu chov�n� wxWindows,
        # kdy wxWindows s�tabulkou pracuje i�po jej�m zru�en�.
        self._data = None
        # TODO: N�sleduj�c� (a�mo�n� i�ta p�edch�zej�c�) operace jsou
        # jsou v principu zbyte�n�, ale proto�e z�nezn�m�ch d�vod�
        # nedoch�z� p�i uzav�en� formul��e k�likvidaci n�jak�ch bl��e
        # neur�en�ch dat, patrn� i�z�t�to tabulky, tak rad�ji v�znamn�
        # data instance ma�eme ru�n�...
        self._frame = None
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
        self._grouping_background_downgrade = None

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
                if callable(s):
                    style_dict[cid] = s(cid, the_row)
                value_dict[cid] = the_row.format(cid)
            # Grouping column may not be in self._columns.
            for gcol in self._grouping:
                if not value_dict.has_key(gcol):
                    value_dict[gcol] = the_row.format(gcol)
            # When row_style was defined, lets compute it.
            if callable(self._row_style):
                style_dict[None] = self._row_style(the_row)
            self._cache[row] = cached_things = [value_dict, style_dict]
        cached_row = cached_things[style and 1 or 0]
        return cached_row[col_id]

    def row(self, row):
        """Vra� ��dek ��slo 'row' jako instanci t��dy 'PresentedRow'.
        
        Vr�cen� ��dek zahrnuje zm�ny proveden� p��padnou editac� a
        obsahuje pouze sloupce datov�ho objektu (nepo��tan� i�po��tan�),
        tak�e je mo�n� jej p��mo pou��t v�datab�zov�ch operac�ch.
        
        Jestli�e ��dek dan�ho ��sla neexistuje, vra� 'None'.
        
        Argumenty:
        
        row -- nez�porn� integer, prvn� ��dek m� ��slo�0
        
        """
        if row < 0 or row >= self.GetNumberRows():
            return None
        r = self._get_row(row, autoadjust=True)
        if isinstance(r, PresentedRow):
            return r
        else:
            self._presented_row.set_row(r)
            return self._presented_row

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
            assert row >= 0 and row < self._row_count, ('Invalid row number', row)
            self._edited_row = self._init_edited_row(row, self._get_row(row))

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
        elif position < -1 or position >= self.GetNumberRows() - 1:
            pass
        else:
            row = position
            success, result = \
                     db_operation(lambda : self._retrieve_row(row))
            if not success:
                self._panic()
            self._presented_row.set_row(result)
            self._current_row = \
                  self._CurrentRow(row, copy.copy(self._presented_row))

    def current_row(self):
        """Vra� ��slo aktu�ln�ho ��dku datov�ho objektu tabulky.
        
        ��dky jsou ��slov�ny od�0.  Pokud ��slo aktu�ln�ho ��dku nen�
        zn�mo, vra� 'None'.

        """
        current = self._current_row
        return current and current.row
        
    # Povinn� gridov� metody
    
    def GetNumberRows(self):
        return self._row_count
    
    def GetNumberCols(self):
        return self._column_count
    
    def IsEmptyCell(self, row, col):
        return False
    
    def GetValue(self, row, col, inputfield=False):
        # `row' a `col' jsou ��slov�ny od 0.
        # Je tabulka ji� uzav�ena?
        if not self._data or col >= self.GetNumberCols():
            return ''
        col_id = self._columns[col].id
        if self._edited_row and row == self._edited_row.row:
            the_row = self._edited_row.the_row
            if the_row is None:
                return ''
            value = the_row.format(col_id)
        else:
            value = self._cached_value(row, col_id)
        # Vyt�hni hodnotu sloupce
        if not inputfield and \
               self._columns[col].wxtype == wx.grid.GRID_VALUE_BOOL and \
               value == 'F':
            # V�t�to podob� gridu je 0 pova�ov�na za pravdu.
            # Mo�n� to souvis� s�C++ p�ij�maj�c�m zde pouze strings.
            value = ''
        return value

    def SetValue(self, row, col, value):
        # Tato metoda neodpov�d� specifikaci gridu, ale to nevad�, proto�e
        # pol��ka editujeme v�hradn� p�es na�e editory.
        assert isinstance(value, pytis.data.Value), \
               ('Value not a value', value)
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

    # Nepovinn� gridov� metody

    #def GetColLabelValue(self, col):
    # Nyn� implementov�no pomoc� `ListForm._on_column_header_paint()'.

    def GetTypeName(self, row, col):
        if col >= self.GetNumberCols():
            return wx.grid.GRID_VALUE_STRING
        else:
            return self._columns[col].wxtype
    
    def GetAttr(self, row, col, kind):
        if row >= self.GetNumberRows() or col >= self.GetNumberCols(): # m��e se st�t...
            return None
        column = self._columns[col]
        style = column.style
        if style is None:
            style = self._row_style
            style_column = None
        else:
            style_column = column.id
        if callable(style):
            style = self._cached_value(row, style_column, style=True)
        try:
            fg, bg, font = self._attr_cache[style]
        except KeyError:
            fg, bg, font = self._attr_cache[style] = self._make_attr(style)
        if self._group(row):
            rgb = [max(0, x - y)
                   for x, y in zip((bg.Red(), bg.Green(), bg.Blue()),
                                   self._grouping_background_downgrade)]
            bg = wx.Colour(*rgb)
        provider = self.GetAttrProvider()
        if provider:
            attr = provider.GetAttr(row, col, kind)
            if attr:
                attr.SetTextColour(fg)
                attr.SetBackgroundColour(bg)
                attr.SetFont(font)
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

class IncrementalSearch:
    TEXT_CONTROL_NAME = 'incremental-search-text-control'
    def __init__(self, listform, full):
        self._listform = listform
        self._full = full
        self._key = WxKey()
        self._rows = []
        self._last_direction = pytis.data.FORWARD
        self._on_text_blocked = False
        self._exiting = False
        self._widget = w = wx.TextCtrl(listform.GetParent(), -1,
                                       name=self.TEXT_CONTROL_NAME)
        wx_callback(wx.EVT_KILL_FOCUS, w, self._on_kill_focus)
        wx_callback(wx.EVT_KEY_DOWN, w, self._on_key_down)
        wx_callback(wx.EVT_TEXT, w, w.GetId(), self._on_text)
        cb = self._listform.get_callback(ListForm.CALL_SELECTION)
        self._selection_callback = cb
        self._listform.set_callback(ListForm.CALL_SELECTION, None)
        w.SetFocus()
            
    def run(self, prefill=None):
        self._widget.Show(True)
        self._widget.Enable(True)
        if prefill:
            self._widget.SetValue(prefill)
            self._widget.SetInsertionPoint(len(prefill))
            
    def _on_key_down(self, event):
        key = self._key.event_key(event)
        if key == 'Enter':
            self._exit(False)
        elif key == 'Backspace':
            self._back()
        elif key == 'Ctrl-s':
            self._search(direction=pytis.data.FORWARD)
        elif key == 'Ctrl-r':
            self._search(direction=pytis.data.BACKWARD)
        elif key == 'Ctrl-g' or key == 'Escape':
            self._exit(True)
        else:
            event.Skip()
    
    def _on_text(self, event):
        if self._on_text_blocked:
            return
        self._search(newtext=True)

    def _on_kill_focus(self, event):
        if self._listform:
            self._listform.set_callback(ListForm.CALL_SELECTION, self._selection_callback)
        if not self._exiting:
            self._exit(True)
            
    def _exit(self, rollback):
        self._exiting = True
        if rollback and self._rows and self._listform:
            self._set_row(self._rows[0][0])
        w = self._widget
        w.Enable(False)
        w.Show(False)
        w.Destroy()
        # Mus�me po��tat s mo�nost�, �e n�kdo zav�el listform
        # p�ed opu�t�n�m inkrement�ln�ho vyhled�v�n�
        # (nap�. kliknut�m my�i u codebooku s vyhled�v�n�m)
        if self._listform:
            form = self._listform
            form.focus()
            if not rollback:
                the_row = form._table.row(form._table.current_row())
                form._run_callback(form.CALL_SELECTION, the_row)

    def _back(self):
        if self._rows:
            row, text = self._rows.pop()
            try:
                self._on_text_blocked = True
                w = self._widget
                w.SetValue(text)
                w.SetInsertionPointEnd()
            finally:
                self._on_text_blocked = False
            self._set_row(row)
        else:
            beep()

    def _search(self, direction=None, newtext=False):
        if direction is None:
            direction = self._last_direction
        else:
            self._last_direction = direction
        text = self._widget.GetValue()
        form = self._listform
        row, col = form._current_cell()
        if newtext:
            oldtext = text[:-1]
        else:
            oldtext = text
        self._rows.append((row, oldtext))
        colid = form._columns[col].id()
        stext = text + '*'
        if self._full:
            stext = '*' + stext
        wmvalue = pytis.data.WMValue(pytis.data.String(), stext)
        condition = pytis.data.WM(colid, wmvalue)
        if newtext:
            if direction == pytis.data.FORWARD:
                start_row = max(row-1, 0)
            else:
                start_row = min(row+1, form._table.GetNumberRows())
        else:
            start_row = row
        # TODO: P�edhled�n� v�aktu�l~~n�m selectu
        found = form._search(condition, direction, row_number=start_row, report_failure=False)
        if found is None:
            message(_("Dal�� z�znam nenalezen"), beep_=True)
        else:
            if direction == pytis.data.FORWARD:
                new_row = start_row + found
            else:    
                new_row = start_row - found
            form._select_cell(row=new_row)

    def _set_row(self, row):
        current = self._listform._table.current_row()
        self._listform.select_row(row)
        self._widget.SetFocus()
        return current
    
