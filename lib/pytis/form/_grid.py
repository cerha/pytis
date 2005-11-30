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
        def __init__(self, row, the_row, fieldspec, data, new=False,
                     prefill=None):
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
            self.invalid_col = None # ��slo sloupce
            self.invalid_string = None
        def update(self, colid, value):
            self.the_row[colid] = value
            
    class _Column:
        def __init__(self, id, wxtype, label, style):
            self.id = id
            self.wxtype = wxtype
            self.label = label
            self.style = style

    class EditInfo:
        def __init__(self, row, the_row, new, changed, valid, orig_content):
            self.row = row
            self.the_row = the_row
            self.new = new
            self.changed = changed
            self.valid = valid
            self.orig_content = orig_content

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
        
    def __init__(self, frame, data, fields, columns, row_count,
                 sorting=(), grouping=None, inserted_row_number=None,
                 inserted_row=None, prefill=None):
        wx.grid.PyGridTableBase.__init__(self)
        if self._TYPE_MAPPING is None:
            # Mus�me inicializovat a� zde kv�li neXov�mu serveru.
            # Nepou��v�me mapov�n� pro Float, proto�e to by n�m zru�ilo
            # na�e form�tov�n� ��sel.
            self.__class__._TYPE_MAPPING = \
                  {pytis.data.Boolean: wx.grid.GRID_VALUE_BOOL}
        self._frame = frame
        self._data = data
        self._fields = fields
        self._row_count = row_count
        self._sorting = sorting
        self._grouping = grouping
        self._prefill = prefill
        self._current_row = None
        # Zpracuj sloupce
        self._columns = columns
        self._column_count = len(filter (lambda c: c.column_width(),
                                         columns))
        self._column_info = column_info = []
        for c in columns:
            cid = c.id()
            label = c.column_label() or ''
            style = c.style()
            assert isinstance(style, pytis.presentation.FieldStyle) \
                   or callable(style), \
                   ('Invalid field style', cid, style)
            t = c.type(data)
            try:
                wxtype = self._TYPE_MAPPING[t.__class__]
            except KeyError:
                wxtype = wx.grid.GRID_VALUE_STRING
            cc = self._Column(cid, wxtype, label, style)
            column_info.append(cc)
        # Vytvo� cache
        self._cache = self._DisplayCache()
        self._attr_cache = {}
        self._font_cache = {}
        self._group_cache = {0: False}
        self._group_value_cache = {}
        # Nastav ��dek
        self.rewind()
        if inserted_row_number is None:
            self._edited_row = None
        else:
            self._edited_row = self._EditedRow(inserted_row_number,
                                               inserted_row, fields, data,
                                               new=True, prefill=prefill)
        self._presented_row = PresentedRow(fields, data, None,
                                           singleline=True,
                                           prefill=prefill)

    # Pomocn� metody
        
    def _panic(self):
        if __debug__: log(DEBUG, 'Zpanika�en� gridov� tabulky')
        leave_form()

    def _get_row(self, row, autoadjust=False):
        """Vra� ��dek ��slo 'row' z�datab�ze jako instanci 'pytis.data.Row'.
        
        Argumenty:
        
        row -- po�adov� ��slo ��dku *v�datab�zov�m selectu*, po��naje�0
        autoadjust -- pr�v� kdy� je pravdiv�, je 'row' sn�eno
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
        if self._grouping is None:
            return False
        try:
            return self._group_cache[row]
        except KeyError:
            grouping_column = self._grouping
            this_value = self._cached_value(row, grouping_column)
            try:
                result = self._group_value_cache[this_value]
                self._group_cache[row] = result
                return result
            except KeyError:
                cached = self._group_cache.keys()
                lower = filter(lambda k: k < row, cached)
                if len(lower) and (row < 100 or row - max(lower) < 80):
                    prev_value = self._cached_value(row-1, grouping_column)
                    prev_group = self._group(row-1)
                    if this_value == prev_value:
                        result = prev_group
                    else:
                        result = not prev_group
                    self._group_value_cache[this_value] = result
                    self._group_cache[row] = result
                    return result
                higher = filter(lambda k: k > row, cached)
                if len(higher) and min(higher) - row < 80:
                    next_value = self._cached_value(row+1, grouping_column)
                    next_group = self._group(row+1)
                    if this_value == next_value:
                        result = next_group
                    else:
                        result = not next_group
                    self._group_cache[row] = result
                    return result
                # There is no cached group within nearest rows, so start
                # again with an empty cache.
                self._group_cache = {row: False}
                self._group_value_cache = {this_value: False}
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
        
    def update(self, row_count, sorting, grouping, inserted_row_number,
               inserted_row, prefill):
        self._row_count = row_count
        self._sorting = sorting
        self._grouping = grouping
        self._prefill = prefill
        # Sma� cache
        self._group_cache = {0: False}
        self._group_value_cache = {}
        # Nastav ��dek
        self.rewind()
        if inserted_row_number is None:
            self._edited_row = None
        else:
            self._edited_row = self._EditedRow(inserted_row_number,
                                               inserted_row, self._fields,
                                               self._data,
                                               new=True, prefill=prefill)
        self._presented_row = PresentedRow(self._fields, self._data,
                                           None, singleline=True,
                                           prefill=prefill)
        
    def close(self):
        # Tato metoda je nutn� kv�li jist�mu podivn�mu chov�n� wxWindows,
        # kdy wxWindows s�tabulkou pracuje i�po jej�m zru�en�.
        self._data = None
        # TODO: N�sleduj�c� (a�mo�n� i�ta p�edch�zej�c�) operace jsou
        # jsou v principu zbyte�n�, ale proto�e z�nezn�m�ch d�vod�
        # nedoch�z� p�i uzav�en� formul��e k�likvidaci n�jak�ch bl�e
        # neur�en�ch dat, patrn� i�z�t�to tabulky, tak rad�ji v�znamn�
        # data instance ma�eme ru�n�...
        self._frame = None
        self._fields = None
        self._columns = None
        self._column_info = None
        self._cache = None
        self._attr_cache = None
        self._font_cache = None
        self._group_cache = None
        self._group_value_cache = None
        self._edited_row = None
        self._presented_row = None

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
            for c in self._column_info:
                cid = c.id
                s = c.style
                if callable(s):
                    style_dict[cid] = s(cid, the_row)
                value_dict[cid] = the_row.format(cid)
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
            assert row >= 0 and row < self._row_count, \
                   ('Invalid row number', row)
            data_row = self._get_row(row)
            self._edited_row = \
                  self._EditedRow(row, data_row, self._fields, self._data,
                                  prefill=self._prefill)

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
            row = edited.row
            new = the_row.new()
            if new:
                the_orig = None
            else:
                the_orig = edited.orig_row
            valid = (edited.invalid_col == None)
            changed = the_row.changed()
            return self.EditInfo(row, the_row, new, changed, valid, the_orig)
        else:
            return None

    def column_id(self, col):
        return self._column_info[col].id
        
    def column_label(self, col):
        return self._column_info[col].label

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

    def set_invalid_string(self, row, col, string):
        """Zapamatuj si nevalidn� hodnotu bu�ky.
        
        Tato metoda by m�la b�t vol�na, pokud byla u�ivatelem vlo�ena
        nevalidn� hodnota.  Nastaven� metodou 'SetValue()' p�ij�m�
        instanci 'pytis.data.Value', a tud� pouze validn� hodnoty.  Takto
        d�v�me gridov� tabulce mo�nost dozv�d�t se o nevalidn�m vstupu a
        pat�i�n� na n�j reagovat.

        Po �sp�n� validaci mus� b�t zapamatovan� �et�zec vymaz�n vol�n�m
        t�to metody s argumentem 'string' rovn�m 'None'.
        
        Sou�asn� implementace se omezuje na mo�nost nastaven� nevalidn�
        hodnoty pouze pro jeden sloupec pr�v� editovan�ho ��dku.
        
        """
        edited = self._edited_row
        if edited and edited.row == row:
            if string is None:
                edited.invalid_col = None
                edited.invalid_string = None
            else:
                edited.invalid_col = col
                edited.invalid_string = string
                
    def get_invalid_string(self, row, col):
        """Vra� nevalidn� hodnotu bu�ky zadanou u�ivatelem jako �et�zec.
        
        Pokud nebyla do dan� bu�ky vlo�ena nevalidn� hodnota, vra� 'None'.
        
        Plat� zde omezen� popsan� v 'set_invalid_string()'.
        
        """
        edited = self._edited_row
        if edited and edited.row == row and edited.invalid_col == col:
            return edited.invalid_string
        return None

            
        
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
        if not self._data:
            return ''
        col_id = self._column_info[col].id
        if self._edited_row and row == self._edited_row.row:
            the_row = self._edited_row.the_row
            if the_row is None:
                return ''
            value = the_row.format(col_id)
        else:
            value = self._cached_value(row, col_id)
        # Vyt�hni hodnotu sloupce
        if not inputfield and \
               self._column_info[col].wxtype == wx.grid.GRID_VALUE_BOOL and \
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
        cid = self._column_info[col].id
        edited.update(cid, value)
        log(EVENT, 'Nastavena hodnota editovan�ho pol��ka:',
            (row, col, value))

    # Nepovinn� gridov� metody

    #def GetColLabelValue(self, col):
    # Nyn� implementov�no pomoc� `ListForm._on_column_header_paint()'.

    def GetTypeName(self, row, col):
        return self._column_info[col].wxtype
    
    def GetAttr(self, row, col, _something):
        if row >= self.GetNumberRows(): # m��e se st�t...
            return None
        column = self._column_info[col]
        style = column.style
        if callable(style):
            style = self._cached_value(row, column.id, style=True)
        try:
            fg, bg, font = self._attr_cache[style]
        except KeyError:
            fg, bg, font = self._attr_cache[style] = self._make_attr(style)
        if self._group(row):
            rgb = [max(0, x - y)
                   for x, y in zip((bg.Red(), bg.Green(), bg.Blue()),
                                   ListForm._GROUPING_BACKGROUND_DOWNGRADE)]
            bg = wx.Colour(*rgb)
        provider = self.GetAttrProvider()
        if provider:
            attr = provider.GetAttr(row, col, _something)
            if attr:
                attr.SetTextColour(fg)
                attr.SetBackgroundColour(bg)
                attr.SetFont(font)
                return attr
        return None


class InputFieldCellEditor(wx.grid.PyGridCellEditor):

    def __init__(self, parent, table, guardian, field_spec, data,
                 registration):
        wx.grid.PyGridCellEditor.__init__(self)
        self._parent = parent
        self._table = table
        self._guardian = guardian
        self._field_spec = field_spec
        self._data = data
        self._registration = registration
        self._field = None
        self._callbacks = []

    # Povinn� metody

    def Create(self, parent, id, evt_handler):
        self._field = InputField.create(parent, self._field_spec, self._data,
                                        guardian=self._guardian, inline=True)
        for type,function in self._callbacks:
            self._field.set_callback(type, function)
        control = self._field.widget()
        self.SetControl(control)
        return control

    def BeginEdit(self, row, col, grid):
        self._registration(self)
        field = self._field
        value = self._table.GetValue(row, col, inputfield=field)
        field.init(value)
        invalid_string = self._table.get_invalid_string(row, col)
        if invalid_string is not None:
            field.set_value(invalid_string)
        field.widget().Enable(True)
        field.set_focus()
        try:
            field.widget().SetInsertionPointEnd()
        except AttributeError:
            pass
        
    def EndEdit(self, row, col, grid):
        field = self._field
        field.widget().Enable(False)
        self._parent.SetFocus()
        if not field.is_modified():
            self._table.set_invalid_string(row, col, None)
            return False
        value, error = field.validate(interactive=False)
        if error:
            self._table.set_invalid_string(row, col, field.get_value())
            return True
        else:
            self._table.set_invalid_string(row, col, None)
            self._table.SetValue(row, col, value)
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
    
    def set_callback(self, type, function):
        if self._field is not None:
            self._field.set_callback(type, function)
        else:
            self._callbacks.append((type, function))

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
        w.SetFocus()
        wx_callback(wx.EVT_KEY_DOWN, w, self._on_key_down)
        wx_callback(wx.EVT_TEXT, w, w.GetId(), self._on_text)
        cb = self._listform.get_callback(ListForm.CALL_SELECTION)
        self._selection_callback = cb
        self._listform.set_callback(ListForm.CALL_SELECTION, None)
            
    def run(self):
        self._widget.Show(True)
        self._widget.Enable(True)

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
            self._listform.set_callback(ListForm.CALL_SELECTION,
                                        self._selection_callback)
        if not self._exiting:
            self._exit(True)
            
    def _exit(self, rollback):
        self._exiting = True
        if rollback and self._rows:
            self._set_row(self._rows[0][0])
        w = self._widget
        w.Enable(False)
        w.Show(False)
        w.Destroy()
        # Mus�me po��tat s mo�nost�, �e n�kdo zav�el listform
        # p�ed opu�t�n�m inkrement�ln�ho vyhled�v�n�
        # (nap�. kliknut�m my�i u codebooku s vyhled�v�n�m)
        if self._listform:
            self._listform.focus()
            if not rollback:
                l = self._listform
                the_row = l._table.row(l._table.current_row())
                l._run_callback(l.CALL_SELECTION, (the_row,))

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
        l = self._listform
        row, col = l._current_cell()
        if newtext:
            oldtext = text[:-1]
        else:
            oldtext = text
        self._rows.append((row, oldtext))
        colid = l._columns[col].id()
        stext = text + '*'
        if self._full:
            stext = '*' + stext
        wmvalue = pytis.data.WMValue(pytis.data.String(), stext)
        condition = pytis.data.WM(colid, wmvalue)
        if newtext:
            if direction == pytis.data.FORWARD:
                start_row = max(row-1, 0)
            else:
                start_row = min(row+1, l._table.GetNumberRows())
        else:
            start_row = row
        # TODO: P�edhled�n� v�aktu�l~~n�m selectu
        found = l._search(condition, direction, row_number=start_row,
                          report_failure=False)
        if found is None:
            message(_("Dal�� z�znam nenalezen"), beep_=True)
        else:
            if direction == pytis.data.FORWARD:
                new_row = start_row + found
            else:    
                new_row = start_row - found
            l._select_cell(row=new_row)

    def _set_row(self, row):
        current = self._listform._table.current_row()
        self._listform.select_row(row)
        self._widget.SetFocus()
        return current
    
