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

"""Práce s formuláři se seznamovým zobrazením.

Modul jednak interpretuje specifikaci formulářů (viz modul 'spec') pro
seznamové zobrazení a jednak zajišťuje práci s ní prostřednictvím objektů
wxWindows.

"""

# Terminologická poznámka: Proměnné s názvem `row' obvykle značí číslo řádku
# (číslováno od 0).  Jedná-li se o obsah řádku, nazývá se příslušná proměnná
# obvykle `the_row'.  Matoucí jméno `row' bylo převzato z wxWindows.

import copy
import string
import time
import types

import wx
import wx.grid

from pytis.form import *
import pytis.data
import pytis.output
import pytis.presentation
from pytis.presentation import PresentedRow

import _grid

import config
    
### Formuláře


class ListForm(LookupForm, TitledForm, Refreshable):
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
    CALL_ALTERNATE_ACTIVATION = 'CALL_ALTERNATE_ACTIVATION'
    """Konstanta callbacku alternativní aktivace řádku."""
    CALL_MODIFICATION = 'CALL_MODIFICATION'
    """Konstanta callbacku modifikace řádku."""
    CALL_USER_INTERACTION = 'CALL_USER_INTERACTION'
    """Konstanta callbacku interakce uživatele."""

    _REFRESH_PERIOD = 60 # sekund
    _SELECTION_CALLBACK_DELAY = 3 # desítky milisekund
    _GROUPING_BACKGROUND_DOWNGRADE = (18, 16, 14) # snížení složek jasu RGB
    _TITLE_FOREGROUND_COLOR = WxColor(210, 210, 210)
    
    _STATUS_FIELDS = ['list-position']


    def __init__(self, *args, **kwargs):
        super_(ListForm).__init__(self, *args, **kwargs)
        # Nastav klávesové zkratky z kontextových menu.
        for item in self._context_menu() + self._edit_menu():
            if isinstance(item, MItem):
                if item.hotkey() != (None,):
                    self.define_key(item.hotkey(), item.command(), item.args())
        # Závěrečné akce
        self._data.add_callback_on_change(self.on_data_change)
        wx_callback(wx.EVT_SIZE, self, self._on_size)
        self.select_row(self._position)

    def _init_attributes(self, columns=None, **kwargs):
        """Zpracuj klíčové argumenty konstruktoru a inicializuj atributy.

        Argumenty:

          columns -- pokud není None, bude formulář používat dané sloupce.
            Jinak je použit seznam sloupců daný specifikací.  Hodnotou je
            sekvence identifikátorů sloupců obsaženýh ve specifikaci.
          kwargs -- argumenty předané konstruktoru předka.

        """
        super_(ListForm)._init_attributes(self, **kwargs)
        assert columns is None or is_sequence(columns)
        self._orig_columns = columns or self._view.columns()
        # Inicializace atributů
        self._fields = self._view.fields()
        self._enable_inline_insert = self._view.enable_inline_insert()
        self._selection_candidate = None
        self._selection_callback_candidate = None
        self._selection_callback_tick = None
        self._in_select_cell = False
        self._last_reshuffle_request = self._reshuffle_request = 0
        self._current_editor = None
        self._block_refresh = False
        # Parametry zobrazení
        self._initial_position = self._position = 0

    def _default_grouping(self):
        return self._view.grouping()

    def _default_sorting(self):
        view = self._view
        sorting = view.sorting()
        if sorting is None:
            key = filter(lambda k: view.field(k.id()) is not None,
                         self._data.key())
            sorting = tuple(map(lambda k: (k.id(),
                                           LookupForm.SORTING_DESCENDANT),
                                key))
        return sorting
        
    def _create_form_parts(self, sizer):
        title = self.title()
        if title is not None:
            description = self._view.description()
            self._title_bar = self._create_title_bar(title,
                                                     description=description)
            sizer.Add(self._title_bar, 0, wx.EXPAND|wx.FIXED_MINSIZE)
        else:
            self._title_bar = None
        self._grid = self._create_grid()
        sizer.Add(self._grid, 1, wx.EXPAND|wx.FIXED_MINSIZE)

    def _column_width(self, grid, column):
        column_width = max(column.column_width(), len(column.column_label()))
        return dlg2px(grid, 4*column_width+8)
        
    def _create_grid(self):
        if __debug__: log(DEBUG, 'Vytváření nového gridu')
        # Uprav sloupce
        visible_columns = []
        hidden_columns = []
        for c in [self._view.field(id) for id in self._orig_columns]:
            if c.column_width():
                visible_columns.append(c)
            else:
                hidden_columns.append(c)
        self._columns = columns = visible_columns + hidden_columns
        # Vytvoř grid a tabulku
        g = wx.grid.Grid(self, wx.NewId())
        # Inicializuj datový select
        row_count = self._init_select()
        self._table = table = \
          _grid.ListTable(self._parent, self._data, self._fields, columns,
                          row_count, sorting=self._lf_sorting,
                          grouping=self._lf_grouping, prefill=self._prefill)
        g.SetTable(table, True)
        g.SetRowLabelSize(0)
        g.SetColLabelSize(dlg2px(g, 0, 12).GetHeight())
        g.SetColLabelAlignment(wx.CENTER, wx.CENTER)
        g.SetMargins(0,0)
        g.DisableDragGridSize()
        labelfont = g.GetLabelFont()
        labelfont.SetWeight(wx.NORMAL)
        g.SetLabelFont(labelfont)
        if config.cell_highlight_color is not None:
            g.SetCellHighlightColour(config.cell_highlight_color)
        if config.grid_line_color is not None:
            g.SetGridLineColour(config.grid_line_color)
        g.SetDefaultRowSize(dlg2px(g, 0, 10).GetHeight())
        # (Re)inicializuj atributy instance a gridu
        self._editors = []
        def registration(editor):
            self._current_editor = editor
        editable = False
        total_width = 0
        for i in range(len(columns)):
            c = columns[i]
            # šířka
            if not c.width():
                continue
            w = self._column_width(g, c)
            g.SetColSize(i, w)
            total_width += w
            # typ sloupce
            t = c.type(self._data)
            # zarovnání
            attr = wx.grid.GridCellAttr()
            if isinstance(t, pytis.data.Number):
                alignment = wx.ALIGN_RIGHT
            else:
                alignment = wx.ALIGN_LEFT
            attr.SetAlignment(alignment, wx.CENTER)
            # editor
            editable = c.editable()
            if editable or editable in (Editable.ALWAYS, Editable.ONCE):
                editable = True
                editor = _grid.InputFieldCellEditor(self._parent, table, self,
                                                    c, self._data, registration)
                editor.set_callback(InputField.CALL_LEAVE_FIELD,
                                    self._on_cell_rollback)
                editor.set_callback(InputField.CALL_COMMIT_FIELD, 
                                    self._on_cell_commit)
                self._editors.append(editor)
                attr.SetEditor(editor)
            else:
                attr.SetReadOnly()
            g.SetColAttr(i, attr)
        self._total_width = total_width
        self.editable = editable
        # Event handlery
        wx_callback(wx.grid.EVT_GRID_SELECT_CELL, g, self._on_select_cell)
        # CLICK automaticky vovolá SELECT_CELL, takže by následující řádka
        # neměla být třeba.  Odkomentováním bude metoda volána dokonce třikrát.
        #wx_callback(wx.grid.EVT_GRID_CELL_LEFT_CLICK, g, self._on_select_cell)
        wx_callback(wx.grid.EVT_GRID_CELL_RIGHT_CLICK, g, self._on_context_menu)
        wx_callback(wx.grid.EVT_GRID_LABEL_LEFT_CLICK, g, self._on_label_left)
        wx_callback(wx.grid.EVT_GRID_LABEL_RIGHT_CLICK, g, self._on_label_right)
        wx_callback(wx.grid.EVT_GRID_EDITOR_SHOWN, g, self._on_editor_shown)
        wx_callback(wx.EVT_MOUSEWHEEL, g, self._on_wheel)
        wx_callback(wx.EVT_IDLE, g, self._on_idle)
        wx_callback(wx.EVT_KEY_DOWN, g, self.on_key_down)
        wx_callback(wx.EVT_PAINT, g.GetGridColLabelWindow(),
                    self._on_column_header_paint)
        self._update_label_colors(g)
        if __debug__: log(DEBUG, 'Nový grid vytvořen')
        return g

    def _on_editor_shown(self, event):
        if self._table.editing():
            event.Skip()
        else:
            event.Veto()
            self._select_cell(row=max(0, event.GetRow()), col=event.GetCol())
    
    def _update_grid(self, data_init=False, inserted_row_number=None, inserted_row=None):
        current_row = self._table.current_row()
        if data_init:
            row_count = self._init_select()
        else:
            row_count = self._lf_select_count
            self._data.rewind()
        if inserted_row_number is not None:
            row_count = row_count + 1
        # Uprav velikost gridu
        old_row_count = self._table.GetNumberRows()
        new_row_count = row_count
        t = self._table
        t.update(row_count=row_count, sorting=self._lf_sorting,
                 grouping=self._lf_grouping,
                 inserted_row_number=inserted_row_number,
                 inserted_row=inserted_row, prefill=self._prefill)
        g = self._grid
        g.BeginBatch()
        ndiff = new_row_count - old_row_count
        if new_row_count < old_row_count:
            if new_row_count == 0:
                current_row = 1
            gmessage_id = wx.grid.GRIDTABLE_NOTIFY_ROWS_DELETED
            ndiff = -ndiff
            gmargs = (current_row, ndiff,)
        elif new_row_count > old_row_count:
            gmessage_id = wx.grid.GRIDTABLE_NOTIFY_ROWS_APPENDED
            gmargs = (ndiff,)
        else:
            gmessage_id = None
        if gmessage_id is not None:
            gmessage = wx.grid.GridTableMessage(t, gmessage_id, *gmargs)
            g.ProcessTableMessage(gmessage) 
        gmessage = wx.grid.GridTableMessage(
            t, wx.grid.GRIDTABLE_REQUEST_VIEW_GET_VALUES)
        g.ProcessTableMessage(gmessage)
        g.EndBatch()
        # Závěrečné úpravy
        self.select_row(self._position)
        self._update_label_colors(g)
        g.SetFocus()

    def _update_label_colors(self, grid):
        color = self._lf_indicate_filter and config.filter_color or \
                self._TITLE_FOREGROUND_COLOR
        grid.SetLabelBackgroundColour(color)

    def _context_menu(self):
        """Vrať specifikaci \"kontextového\" popup menu vybrané buňky seznamu.

        Vrací: Sekvenci instancí 'MItem'.

        Tuto metodu nechť odvozené třídy předefinují, pokud chtějí zobrazovat
        kontextové menu.
        
        """
        return ()

    def _edit_menu(self):
        return (
            MItem("Editovat buňku",
                  command = ListForm.COMMAND_EDIT),
            MItem("Uložit záznam",
                  command = ListForm.COMMAND_LINE_COMMIT),
            MItem("Opustit editaci",
                  command = ListForm.COMMAND_FINISH_EDITING),
            MSeparator(),
            MItem("Kopírovat obsah buňky",
                  command = ListForm.COMMAND_COPY_CELL),
            #MItem("", command = ListForm.COMMAND_LINE_ROLLBACK),
            )

    # Pomocné metody

    def _current_cell(self):
        """Vrať dvojici souřadnic (ROW, COL) aktuální buňky."""
        g = self._grid
        return g.GetGridCursorRow(), g.GetGridCursorCol()

    def current_row(self):
        row = self._current_cell()[0]
        if row < 0 or row >= self._grid.GetNumberRows():
            # Při prázdné tabulce má wxGrid nastaven řádek 0.
            return None
        else:
            return self._table.row(row)
        
    def _select_cell(self, row=None, col=None, invoke_callback=True):
        if self._in_select_cell:
            return
        self._in_select_cell = True
        if __debug__: log(DEBUG, 'Přechod na buňku gridu:', (row, col))
        try:
            g = self._grid
            current_row = g.GetGridCursorRow()
            current_col = g.GetGridCursorCol()
            if row is not None:
                assert isinstance(row, types.IntType)
                # Zkontroluj případné opuštění editace
                if not self._finish_editing(row=row):
                    log(EVENT, 'Zamítnuto opuštění editace řádku')
                else:
                    if row < 0 or row >= g.GetNumberRows():
                        if g.IsSelection():
                            g.ClearSelection()
                        row = 0
                    else:
                        if col is None:
                            col = max(0, current_col)
                        g.SetGridCursor(row, col)
                        g.MakeCellVisible(row, col)
                        self._selection_candidate = (row, col)
                        if invoke_callback:
                            # Nevoláme callback ihned, stačí až po
                            # zastavení scrolování...
                            self._selection_callback_candidate = row
                            delay = self._SELECTION_CALLBACK_DELAY
                            self._selection_callback_tick = delay
                    self._position = row
                    self.show_position()
            elif col is not None and col != current_col:
                g.SetGridCursor(current_row, col)
                g.MakeCellVisible(current_row, col)
            if __debug__: log(DEBUG, 'Výběr buňky proveden:', (row, col))
        finally:
            self._in_select_cell = False

    def _edit_cell(self):
        """Spusť editor aktuálního políčka."""
        row, col = self._current_cell()
        table = self._table
        cid = self._columns[col].id()
        if not table.row(table.editing().row).editable(cid):
            message(_("Políčko je needitovatelné"), kind=ACTION, beep_=True)
            return False
        self._grid.EnableCellEditControl()       
        log(EVENT, 'Spuštěn editor políčka:', (row, col))
        return True
    
    def _finish_editing(self, question=None, row=None):
        # Vrací pravdu, právě když nejsou akce blokovány editací řádku.
        table = self._table
        editing = table.editing()
        if not editing:
            return True
        if editing.row == row:
            return True
        if not editing.changed:
            if __debug__: log(DEBUG, 'Odchod z needitovaného řádku povolen')
            self._on_line_rollback()
            finish = True 
        else:
            log(EVENT, 'Pokus o odchod z rozeditovaného řádku seznamu')
            if question == None:
                question = _("Zrušit změny záznamu?")
            buttons = bcancel, bsave, bcontinue = \
                      _("Zrušit"), _("Uložit"), _("Pokračovat v editaci")
            result = run_dialog(MultiQuestion, question, buttons=buttons,
                                default=bsave)
            finish = (result != bcontinue)
            if result is None or result == bcancel:
                log(EVENT, 'Odchod uživatelem povolen')
                self._on_line_rollback()
                finish = True
            elif result == bsave:
                log(EVENT, 'Odchod s uložením řádku')
                finish = self._on_line_commit()
            elif result == bcontinue:
                log(EVENT, 'Odchod uživatelem zamítnut')
                finish = False
            else:
                raise ProgramError('Unexpected dialog result', result)
        return finish

    def _update_selection_colors(self):
        if focused_window() is self:
            if self._table.editing():
                foreground = config.row_edit_fg_color
                background = config.row_edit_bg_color
            else:
                foreground = config.row_focus_fg_color
                background = config.row_focus_bg_color
                if background is None:
                    c = wx.SYS_COLOUR_HIGHLIGHT
                    background = wx.SystemSettings.GetColour(c)

        else:
            foreground = config.row_nofocus_fg_color
            background = config.row_nofocus_bg_color
        g = self._grid
        if foreground is not None and foreground != g.GetSelectionForeground():
            g.SetSelectionForeground(foreground)
        if background is not None and background != g.GetSelectionBackground():
            g.SetSelectionBackground(background)
        # Musíme vynutit překreslení celé selection
        if g.IsSelection():
            g.ClearSelection()
            g.SelectRow(g.GetGridCursorRow())
        
    def _is_editable_cell(self, row, col):
        # Vrať pravdu, pokud je buňka daného řádku a sloupca editovatelná.
        the_row = self._table.row(row)
        id = self._columns[col].id()
        return the_row.editable(id)
    
    def _find_next_editable_cell(self):
        # Vrať pravdu, pokud bylo pohybem vpravo nalezeno editovatelné políčko.
        row, col = self._current_cell()
        while True:
            if not self._grid.MoveCursorRight(False):
                return False
            col = col + 1
            if self._is_editable_cell(row, col):
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

    def _filter(self, condition):
        # TODO: Jak to bylo myšleno?
        # if self._lf_initial_condition:
        #      xcondition = pytis.data.AND(condition, self._lf_initial_condition)
        # else:
        #      xcondition = condition
        log(EVENT, 'Uživatelský filtr:', condition)
        self.refresh(reset={'condition': condition,
                            'filter_flag': condition},
                     when=self.DOIT_IMMEDIATELY)

    def _filter_by_cell(self, cancel=False):
        row, col = self._current_cell()
        id = self._columns[col].id()
        sf_dialog = self._lf_sf_dialog('_lf_filter_dialog', FilterDialog)
        sf_dialog.append_condition(id, self._table.row(row)[id])
        self._on_filter(show_dialog=False)
        
    def _on_sort_column(self, col=None, direction=None, primary=False):
        if not self._finish_editing():
            return
        if col is not None:
            col = self._columns[col].id()
            if not self._data.find_column(col):
                message(_("Podle tohoto sloupce nelze třídit"),
                        beep_=True)
                return
        old_sorting = self._lf_sorting
        sorting = super_(ListForm)._on_sort_column(self, col=col,
                                                   direction=direction,
                                                   primary=primary)
        if sorting is not None and sorting != old_sorting:
            self.refresh(reset={'condition':self._lf_condition,
                                'sorting':sorting},
                         when=self.DOIT_IMMEDIATELY)
        return sorting

    def can_sort_column(self, **kwargs):
        col = kwargs.get('col')
        if col is not None:
            kwargs['col'] = self._columns[col].id()
        return super(ListForm, self).can_sort_column(**kwargs)
            

    
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

    def _on_idle(self, event):
        if self._selection_candidate is not None:
            row, col = self._selection_candidate
            self._selection_candidate = None
            self._grid.SelectRow(row)
            self._update_selection_colors()
            self._grid.MakeCellVisible(row, col)
        if self._selection_callback_candidate is not None:
            if self._selection_callback_tick > 0:
                self._selection_callback_tick -= 1
                microsleep(100)
                event.RequestMore()
            else:
                row = self._selection_callback_candidate
                self._selection_callback_candidate = None
                the_row = self._table.row(row)
                self._run_callback(self.CALL_SELECTION, (the_row,))
                self._post_selection_hook(the_row)
        # V budoucnu by zde mohlo být přednačítání dalších řádků nebo dat
        event.Skip()
        return False

    def _post_selection_hook(self, the_row):
        if focused_window() is self:
            # Zobraz hodnotu displeje z číselníku ve stavové řádce.
            column = self._columns[self._current_cell()[1]]
            value = the_row[column.id()]
            enumerator = value.type().enumerator()
            display_value = ''
            if enumerator and column.codebook():
                try:
                    cb_spec = resolver().get(column.codebook(), 'cb_spec')
                except ResolverError:
                    cb_spec = None
                except AttributeError:
                    cb_spec = None
                if cb_spec and cb_spec.display():
                    try:
                        v = enumerator.get(value.value(), cb_spec.display())
                        if v:
                            display_value = v.export()
                    except DataAccessException:
                        pass
            message(display_value)
    
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

    def _on_jump(self):
        row = super_(ListForm)._on_jump(self)
        if row:
            self.select_row(row-1)
        else:
            message(_("Neplatné číslo záznamu"), beep_=True)                    

    def _on_label_left(self, event):
        col = event.GetCol()
        self._run_callback(self.CALL_USER_INTERACTION)
        invoke_command(LookupForm.COMMAND_SORT_COLUMN, col=col,
                       direction=LookupForm.SORTING_CYCLE_DIRECTION)
        return True

    def _on_select_cell(self, event):
        if not self._in_select_cell:
            self._run_callback(self.CALL_USER_INTERACTION)
        self._select_cell(row=max(0, event.GetRow()), col=event.GetCol())
        # SetGridCursor vyvolá tento handler.  Aby SetGridCursor mělo
        # vůbec nějaký účinek, musíme zde zavolat originální handler, který
        # požadované nastavení buňky zajistí.
        event.Skip()

    def _on_column_header_paint(self, event):
        def triangle (x, y, reversed=True):
            if reversed:
                return ((x, y), (x+6, y), (x+3, y+4))
            else:
                return ((x+3, y), (x+6, y+4), (x, y+4))
        g = self._grid
        t = self._table
        dc = wx.PaintDC(g.GetGridColLabelWindow())
        x = - g.GetViewStart()[0] * g.GetScrollPixelsPerUnit()[0]
        y = 0
        height = g.GetColLabelSize()
        for col in range(g.GetNumberCols()):
            id = t.column_id(col)
            width = g.GetColSize(col)
            # Draw the rectangle around.
            dc.SetBrush(wx.Brush("GRAY", wx.TRANSPARENT))
            dc.SetTextForeground(wx.BLACK)
            d = col == 0 and 0 or 1
            dc.DrawRectangle(x-d, y, width+d, height)
            # Draw the sorting sign.
            pos = position(id, self._lf_sorting, key=lambda x: x[0])
            if pos is not None:
                left = x+width-10
                top = y+3
                a = self._lf_sorting[pos][1] == LookupForm.SORTING_ASCENDENT
                dc.SetBrush(wx.Brush("CORAL", wx.SOLID))
                for i in range(pos):
                    dc.DrawLine(left, top+2*i, left+7, top+2*i)
                dc.DrawPolygon(triangle(left, top+pos*2, reversed=a))
            # Draw the grouping sign.
            if self._lf_grouping == id:
                dc.SetBrush(wx.Brush("CORAL", wx.SOLID))
                dc.DrawCircle(x+5, y+5, 2)
            # Draw the label itself.
            label = t.column_label(col)
            while dc.GetTextExtent(label)[0] > width and len(label):
                label = label[:-1] # Don't allow the label to extend the width.
            dc.DrawLabel(label, (x,y,width,height), wx.ALIGN_CENTER|wx.CENTER)
            x += width
        
    def _on_label_right(self, event):
        self._run_callback(self.CALL_USER_INTERACTION)
        g = self._grid
        col = event.GetCol()
        # Menu musíme zkonstruovat až zde, protože argumentem příkazů je číslo
        # sloupce, které zjistím až z eventu.
        items = (Menu(_("Primární řazení"),
                      (MItem(_("Řadit vzestupně"),
                             command = LookupForm.COMMAND_SORT_COLUMN,
                             args = {'direction':LookupForm.SORTING_ASCENDENT,
                               'col': col, 'primary': True}),
                       MItem(_("Řadit sestupně"),
                             command = LookupForm.COMMAND_SORT_COLUMN,
                             args = {'direction':
                                     LookupForm.SORTING_DESCENDANT,
                                     'col': col, 'primary': True}),)),
                 Menu(_("Dodatečné řazení"),
                      (MItem(_("Řadit vzestupně"),
                             command = LookupForm.COMMAND_SORT_COLUMN,
                             args = {'direction':LookupForm.SORTING_ASCENDENT,
                                     'col': col}),
                       MItem(_("Řadit sestupně"),
                             command = LookupForm.COMMAND_SORT_COLUMN,
                             args = {'direction':
                                     LookupForm.SORTING_DESCENDANT,
                                     'col': col}),
                       )),
                 MSeparator(),
                 MItem(_("Neřadit podle tohoto sloupce"),
                       command = LookupForm.COMMAND_SORT_COLUMN,
                       args = {'direction': LookupForm.SORTING_NONE,
                               'col': col}),
                 MItem(_("Zrušit řazení úplně"),
                       command = LookupForm.COMMAND_SORT_COLUMN,
                       args = {'direction': LookupForm.SORTING_NONE}),
                 MSeparator(),
                 MItem(_("Seskupit podle tohoto sloupce"),
                       command = ListForm.COMMAND_SET_GROUPING_COLUMN,
                       args = {'column_id': self._columns[col].id()}),
                 MItem(_("Zrušit seskupování"),
                       command = ListForm.COMMAND_SET_GROUPING_COLUMN,
                       args = {'column_id': None}),
                 )
        menu = Menu('', items).create(g, self)
        pos = event.GetPosition()
        # Od wxWin 2.3 je vrácena pozice včetně výšky záhlaví, což je v
        # případě záhlaví špatně, tekže musíme opět přepočítávat...
        pos.y = pos.y - g.GetColLabelSize()
        g.PopupMenu(menu, pos)
        menu.Destroy()
        event.Skip()
        return True

    def _on_context_menu(self, event):
        # Popup menu pro vybraný řádek gridu
        self._run_callback(self.CALL_USER_INTERACTION)
        row, col = event.GetRow(), event.GetCol()
        self._select_cell(row=row, col=col)
        self.show_context_menu(position=event.GetPosition())
        event.Skip()
        return True

    def show_context_menu(self, position=None):
        if self._table.editing():
            menu = self._edit_menu()
        else:
            menu = self._context_menu()
        g = self._grid
        if menu:
            keymap = global_keymap()
            for item in menu:
                if isinstance(item, MItem):
                    hotkey = keymap.lookup_command(item.command(), item.args())
                    if hotkey is not None:
                        item.set_hotkey(hotkey)
            if position is None:
                row, col = self._current_cell()
                rect = g.CellToRect(row, col)
                pos = (rect.GetX() + rect.GetWidth()/3 ,
                       rect.GetY() + rect.GetHeight()/2 + g.GetColLabelSize())
                position = self._grid.CalcScrolledPosition(pos)
            menu = Menu('', menu).create(g, self)
            g.PopupMenu(menu, position)
            menu.Destroy()

    def show_popup_menu(self):
        self.show_context_menu()

    def show_position(self):
        row = self._current_cell()[0]
        total = self._table.GetNumberRows()
        set_status('list-position', "%d/%d" % (row + 1, total))

    def on_key_down(self, event, dont_skip=True):
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
        if evil_key(event) or \
           (self._grid.IsCellEditControlEnabled() and
            WxKey().is_event_of_key(event, '\t')):
            return False
        else:
            event.Skip()
            return False

    def _is_changed(self):
        editing = self._table.editing()
        return editing and editing.changed

    def can_line_commit(self):
        return self._is_changed()

    def can_line_rollback(self):
        return self._is_changed()
            
    def on_command(self, command, **kwargs):
        # Univerzální příkazy
        if command.handler() is not None:
            return self._on_handled_command(command, **kwargs)
        elif command == ListForm.COMMAND_COPY_CELL:
            self._on_copy_cell()
            return True
        elif command == ListForm.COMMAND_FILTER_BY_CELL:
            self._filter_by_cell()
            return True
        elif command == ListForm.COMMAND_EDIT:
            self._on_edit()
            return True
        elif command == ListForm.COMMAND_EXPORT_CSV:
            self._on_export_csv()
            return True
        elif command == LookupForm.COMMAND_SORT_COLUMN:
            self._on_sort_column(**kwargs)
            return True
        elif command == ListForm.COMMAND_SET_GROUPING_COLUMN:
            self.refresh(reset={'grouping': kwargs['column_id']})
            return True
        elif command == ListForm.COMMAND_SELECT_CELL:
            self._select_cell(**kwargs)
            return True
        # Příkazy během editace řádku
        elif self._table.editing():
            if command == ListForm.COMMAND_LINE_COMMIT:
                return self._on_line_commit()
            elif command == ListForm.COMMAND_LINE_ROLLBACK:
                return self._on_line_rollback()
            elif command == ListForm.COMMAND_LINE_SOFT_ROLLBACK:
                return self._on_line_rollback(soft=True)
            elif command == ListForm.COMMAND_FINISH_EDITING:
                self._finish_editing()
                return True
            # Příkazy vztahující se pouze k editaci políčka
            elif self._grid.IsCellEditControlEnabled():
                if command == ListForm.COMMAND_CELL_COMMIT:
                    return self._on_cell_commit()
                elif command == ListForm.COMMAND_CELL_ROLLBACK:
                    return self._on_cell_rollback()
                else:
                    try:
                        field = self._current_editor.field()
                        if field.on_command(command, **kwargs):
                            return True
                    except:
                        pass
        # Příkazy mimo editaci
        else:
            if command == ListForm.COMMAND_FIRST_COLUMN:
                self._select_cell(col=0)
            elif command == ListForm.COMMAND_LAST_COLUMN:
                self._select_cell(col=len(self._columns)-1)
            elif command == ListForm.COMMAND_ACTIVATE:
                self._on_activation()
            elif command == ListForm.COMMAND_ACTIVATE_ALTERNATE:
                self._on_alternate_activation()
            elif command == ListForm.COMMAND_SHOW_CELL_CODEBOOK:
                self._on_show_cell_codebook()
            elif command == LookupForm.COMMAND_SEARCH:
                self._on_search()
            elif command == LookupForm.COMMAND_SEARCH_NEXT:
                self._on_search(show_dialog=False, direction=pytis.data.FORWARD)
            elif command == LookupForm.COMMAND_SEARCH_PREVIOUS:
                self._on_search(show_dialog=False,
                                direction=pytis.data.BACKWARD)
            elif command == LookupForm.COMMAND_FILTER:
                self._on_filter()
            elif command == ListForm.COMMAND_INCREMENTAL_SEARCH:
                self._on_incremental_search(full=False)
            elif command == ListForm.COMMAND_FULL_INCREMENTAL_SEARCH:
                self._on_incremental_search(full=True)
            elif command == ListForm.COMMAND_NEW_LINE_AFTER:
                self._on_insert_line()
            elif command == ListForm.COMMAND_NEW_LINE_AFTER_COPY:
                self._on_insert_line(copy=True)
            elif command == ListForm.COMMAND_NEW_LINE_BEFORE:
                self._on_insert_line(after=False)
            elif command == ListForm.COMMAND_NEW_LINE_BEFORE_COPY:
                self._on_insert_line(after=False, copy=True)
            else:
                return super_(ListForm).on_command(self, command, **kwargs)
            return True
        return super_(ListForm).on_command(self, command, **kwargs)
            
    def can_set_grouping(cls, appl, cmd, args):
        f = appl.current_form()
        cid = args.get('column_id')
        if f and isinstance(f, LookupForm):
            grp = f._lf_grouping
            return (cid and cid != grp or grp and not cid)
        else:
            return False
    can_set_grouping = classmethod(can_set_grouping)

    # Metody volané přímo z callbackových metod
                                   
    def _on_activation(self):
        log(EVENT, 'Aktivace řádku řádkového seznamu')
        key = self._current_key()
        self._run_callback(self.CALL_ACTIVATION, (key,))

    def _on_alternate_activation(self):
        log(EVENT, 'Aktivace řádku řádkového seznamu')
        key = self._current_key()
        self._run_callback(self.CALL_ALTERNATE_ACTIVATION, (key,))

    def _on_show_cell_codebook(self):
        column = self._columns[self._current_cell()[1]]
        cb_name = column.codebook()
        if cb_name:
            the_row = self._table.row(self._current_cell()[0])
            v = the_row[column.id()]
            e = v.type().enumerator()
            run_form(BrowseForm, cb_name, select_row={e.value_column(): v})

    def can_show_cell_codebook(self):
        column = self._columns[self._current_cell()[1]]
        return column.codebook() is not None

    def _on_handled_command(self, command, **kwargs):
        log(EVENT, 'Vyvolávám uživatelský handler příkazu:', command)
        handler = command.handler()
        refresh = not kwargs.has_key('norefresh')
        if not refresh:
            del kwargs['norefresh']
        # Zjistíme, jaké má uživatelský handler argumenty.
        import inspect
        allargs, varargs, varkw, defaults = inspect.getargspec(handler)
        if allargs:
            if defaults:
                posargs = len(allargs) - len(defaults)
            else:
                posargs = len(allargs)
        else:
            posargs = 0
        the_row = self.current_row()
        if posargs == 0:
            args = ()
        if posargs == 1:
            args = (the_row,)
        else:
            args = (self._data, the_row)
        if not varkw:
            kwargs = {}
        handler(*args, **kwargs)
        if refresh:
            self.refresh()
        return True

    def _on_incremental_search(self, full):
        row, col = self._current_cell()
        column = self._columns[col]
        if not isinstance(column.type(self._data), pytis.data.String):
            message(_("V tomto sloupci nelze vyhledávat inkrementálně"),
                    beep_=True)
            return
        search_field = _grid.IncrementalSearch(self, full)
        search_field.run()

    def _on_filter(self, show_dialog=True):
        row, col = self._current_cell()
        super_(ListForm)._on_filter(self, row=self._table.row(row),
                                    col=col, show_dialog=show_dialog)

    def _on_copy_cell(self):
        row, col = self._current_cell()
        cid = self._columns[col].id()
        clptext = self._table.row(row).format(cid)
        # set_clipboard_text(clptext)
        # TODO: wxClipboard nefunguje, jak má, tak to vyřešíme
        #       hackem, kdy využijeme toho, že wxTextCtrl.Copy()
        #       dělá to, co má.
        tc = wx.TextCtrl(self, -1, clptext)
        tc.SetSelection(0,len(clptext))
        tc.Copy()
        tc.Destroy()

    def _on_export_csv(self):
        log(EVENT, 'Vyvolání CSV exportu')
        table = self._table
        data = self._data
        # Kontrola počtu řádků
        number_rows = self._table.GetNumberRows()
        if number_rows == 0:
            msg = _("Tabulka neobsahuje žádné řádky! Export nebude proveden.")
            run_dialog(Warning, msg)
            return
        # Seznam sloupců
        column_list = []
        for column in self._columns:
            column_list.append((column.id(), column.type(data)))
        allowed = True
        # Kontrola práv        
        for cid, ctype in column_list:
            if not data.accessible(cid, pytis.data.Permission.EXPORT):
                allowed = False
                break
        if not allowed:
            msg = _("Nemáte právo exportu k této tabulce.\n")
            msg = msg + _("Export nebude proveden.")
            run_dialog(Warning, msg)
            return            
        export_dir = config.export_directory
        export_encoding = config.export_encoding
        db_encoding = config.db_encoding
        try:
            u"test".encode(export_encoding)
        except:
            msg = _("Kódování %s není podporováno.\n" % export_encoding)
            msg = msg + _("Export se provede bez překódování.")
            export_encoding = None
            run_dialog(Error, msg)
        try:
            u"test".encode(db_encoding)
        except:
            msg = _("Kódování %s není podporováno.\n" % db_encoding)
            msg = msg + _("Export se neprovede.")
            run_dialog(Error, msg)
            return
        filename = pytis.form.run_dialog(pytis.form.FileDialog,
                                       title="Zadat exportní soubor",
                                       dir=export_dir, file='export.txt',
                                       mode='SAVE',
                                       wildcards=("Soubory TXT (*.txt)",
                                                  "*.txt",
                                                  "Soubory CSV (*.csv)",
                                                  "*.csv"))
        if not filename:
            return
        try:       
            export_file = open(filename,'w')
        except:
            msg = _("Nepodařilo se otevřít soubor " + filename + \
                    " pro zápis!\n")
            run_dialog(Error, msg)
            return
        def _process_table(update):
            # Export labelů
            for column in self._columns:
                export_file.write(column.label()+'\t')
            export_file.write('\n')
            for r in range(0,number_rows):
                if not update(int(float(r)/number_rows*100)):
                    break                
                for cid, ctype in column_list:
                    if isinstance(ctype, pytis.data.Float):
                        s = self._table.row(r)[cid].export(locale_format=False)
                    else:
                        s = self._table.row(r)[cid].export()                        
                    if export_encoding and export_encoding != db_encoding:
                        if not is_unicode(s):
                            s = unicode(s, db_encoding)
                        s = s.encode(export_encoding)
                    export_file.write(';'.join(s.split('\n'))+'\t')
                export_file.write('\n')
            export_file.close()
        pytis.form.run_dialog(pytis.form.ProgressDialog, _process_table)       
        
    def _on_edit(self):
        if not self.editable:
            log(EVENT, 'Pokus o editaci needitovatelné tabulky')
            return False
        table = self._table
        if not table.editing():
            row = self._current_cell()[0]
            the_row = table.row(row).row()
            key = the_row.columns(map(lambda c: c.id(), self._data.key()))
            success, locked = db_operation(lambda : self._data.lock_row(key),
                                           quiet=True)
            if success and locked != None:
                log(EVENT, 'Záznam je zamčen', locked)
                run_dialog(Message, _("Záznam je zamčen: %s") % locked)
                return False
            table.edit_row(row)
            self._update_selection_colors()
        if not self._edit_cell():
            self._on_line_rollback()
        return True

    def _on_insert_line(self, after=True, copy=False):
        """Vlož nový řádek do seznamu.

        Argumenty:

          after -- je-li pravda, nový řádek se vloží za aktuální řádek, jinak
            se vloží před aktuální řádek
          copy -- je-li pravda a seznam není prázdný, obsahem nového řádku bude
            obsah aktuálního řádku, v opačném případě bude nový řádek prázdný

        Vložení nového řádku do seznamu je možné jen tehdy, pokud není zrovna
        žádný řádek editován, ať už nový nebo stávající.  Při pokusu o vložení
        nového řádku během editace jiného řádku je chování metody nedefinováno.

        Vložení nového řádku může být také zakázáno pro konkrétní formulář v
        jeho specifikaci (viz argument 'enable_inline_insert' konstruktoru
        třídy 'ViewSpec').
        
        Po vložení nového řádku seznam automaticky přejde do režimu editace
        tohoto řádku a spustí editaci první editovatelné buňky řádku.

        """
        row = self._current_cell()[0]
        log(EVENT, 'Vložení nového řádku:', (row, after, copy))
        if not self._data.accessible(None, pytis.data.Permission.INSERT):
            message('Nemáte přístupová práva pro vkládání záznamů do této ' + \
                    'tabulky!', beep_=True)
            return False
        if not self.editable:
            message('Needitovatelná tabulka!', beep_=True)
            return False
        if not self._enable_inline_insert:
            message('Není možné vkládat řádky v in-line editaci. ' +
                    'Použijte editační formulář.', beep_=True)
            return False
        table = self._table
        if table.editing():
            log(EVENT, 'Pokus o vložení nového řádku během editace')
            return False
        self._last_insert_copy = copy
        oldg = self._grid
        oldempty = (oldg.GetNumberRows() == 0)
        if not copy or oldempty:
            the_row = None
        else:
            the_row = table.row(row)
            # TODO: možná půjde vyřešit čistěji
            # Jde o to vytvořit kopii řádku, ale klíč nekopíro
            prefill = {}
            keys = [c.id() for c in the_row.data().key()]
            for k in the_row.keys():
                if k not in keys:
#                    prefill[k] = the_row[k].value()
                    prefill[k] = the_row[k]
            fields = the_row.fields()
            data = the_row.data()
            the_row = PresentedRow(fields, data, None, prefill=prefill,
                                   new=True)
            for k in the_row.keys():
                the_row[k]
        if after and not oldempty:
            row = row + 1
        self._update_grid(inserted_row_number=row, inserted_row=the_row)
        self._select_cell(row=row, col=0, invoke_callback=False)
        if not self._is_editable_cell(row, 0) \
               and not self._find_next_editable_cell():
            log(EVENT, 'Žádný sloupec není editovatelný')
            return False
        self._edit_cell()
        self._update_selection_colors()
        log(EVENT, 'Řádek vložen')
        return True

    def _on_delete_record(self, key):
        if not self.editable:
            message('Needitovatelná tabulka!', beep_=True)
            return False
        self._block_refresh = True
        try:
            key = self._current_key()
            deleted = super(ListForm, self)._on_delete_record(key)
            self._table.edit_row(None)
        finally:
            self._block_refresh = False
        if deleted:
            r = self._current_cell()[0]
            n = self._table.GetNumberRows()
            if r < n - 1:
                self._select_cell(row=r+1)
            elif r > 0:
                self._select_cell(row=r-1)
            self.refresh()

    def _on_line_commit(self):
        # Zde záleží na návratové hodnotě, protože ji využívá _on_cell_commit.
        log(EVENT, 'Pokus o uložení řádku seznamu do databáze')
        # Vytažení nových dat
        table = self._table
        editing = table.editing()
        if not editing:
            return False
        data = self._data
        row = editing.row
        the_row = editing.the_row
        error = None
        check = self._view.check()
        if check is not None:
            error = check(the_row)
        if error is None:
            error = the_row.check()
        if error is not None:        
            if is_sequence(error):
                failed_id, msg = error                
                message(msg)
            else:
                failed_id = error
            log(EVENT, 'Kontrola integrity selhala:', failed_id)
            col = find(failed_id, self._columns, key=lambda c: c.id())
            if col is not None:
                i = self._columns.index(col)
                self._select_cell(row=row, col=i, invoke_callback=False)
                self._edit_cell()
            return True
        # Určení operace a klíče
        kc = [c.id() for c in data.key()]
        if editing.new:
            table = self._table
            if row > 0:
                after = table.row(row-1).row().columns(kc)
                before = None
            elif row < table.GetNumberRows() - 1:
                after = None
                before = table.row(row+1).row().columns(kc)
            else:
                after = before = None
            rdata = []
            for field in the_row.fields():
                rdata.append((field.id(), the_row[field.id()]))
            new_row = pytis.data.Row(rdata)
            op = (data.insert, (new_row,), {'after': after, 'before': before})
        else:
            key = editing.orig_content.row().columns(kc)
            op = (data.update, (key, the_row.row()))
        # Provedení operace
        success, result = db_operation(op)
        if success and result[1]:
            table.edit_row(None)
            if data.locked_row():
                data.unlock_row()
                message('Řádek uložen do databáze', ACTION)
            self.refresh()
            self._run_callback(self.CALL_MODIFICATION)
            on_line_commit = self._view.on_line_commit()
            if on_line_commit is not None:
                on_line_commit(the_row)
            self.focus()
        elif success:
            log(EVENT, 'Zamítnuto pro chybu klíče')
            if editing.new:
                msg = _("Řádek s tímto klíčem již existuje nebo změna "+\
                            "sousedního řádku")
            else:
                msg = _("Řádek s tímto klíčem již existuje nebo původní "+\
                            "řádek již neexistuje")
            run_dialog(Warning, msg)
            return False
        else:
            log(EVENT, 'Chyba databázové operace')
            return False
        return True
        
    def _on_line_rollback(self, soft=False):
        log(EVENT, 'Zrušení editace řádku')
        editing = self._table.editing()
        if not editing:
            return False
        if soft and editing.changed:
            return True
        if self._data.locked_row():
            self._data.unlock_row()
        row = editing.row
        if editing.new:
            self._update_grid()
        else:
            self._table.edit_row(None)
            self._update_selection_colors()
            # Tento SelectRow je zde nutný pro vynucení překreslení řádku se
            # staronovými hodnotami.
            self._grid.SelectRow(row)
        self._select_cell(row=row, invoke_callback=False)
        self.refresh()
        return True

    def _on_cell_commit(self):
        row, col = self._current_cell()
        log(EVENT, 'Odeslání obsahu políčka gridu', (row, col))
        self._grid.DisableCellEditControl()
        editing = self._table.editing()
        if not editing:
            return True
        if editing.valid:
            if not self._find_next_editable_cell():
                if editing.new:
                    q = _("Uložit řádek?")
                    if run_dialog(Question, q, True):
                        log(EVENT, 'Kladná odpověď na dotaz o uložení řádku')
                        if self._on_line_commit():
                            # TODO: voláním následující metody v těle této
                            # metody, která ošetřuje příkaz, dojde k
                            # zablokování zpracování příkazů v rámci jejího
                            # zpracování.  Než bude následující volání opět
                            # odkomentováno, je třeba zajistit neblokující
                            # zpracování příkazů...
                            # self._on_insert_line(copy=self._last_insert_copy)
                            pass
                        return True
                    else:
                        log(EVENT, 'Záporná odpověď na dotaz o uložení řádku')
                self._grid.SetGridCursor(row, 0)
        if not editing.valid or editing.new:
            log(EVENT, 'Návrat do editace políčka')
            self._edit_cell()
        return True
        
    def _on_cell_rollback(self):
        log(EVENT, 'Opuštění políčka gridu beze změny hodnoty')
        self._current_editor.Reset()
        self._grid.DisableCellEditControl()
        self._current_editor = None
        return True

    # Veřejné metody

    def is_edited(self):
        """Vrať pravdu, právě když je List ve stavu řádkové editace."""
        return self._table.editing()

    def exit_check(self):
        """Proveď kontrolu ukončení editace řádku před opuštěním seznamu.

        Metoda nic nevrací, pouze sama provede, co je potřeba.

        """
        editing = self._table.editing()
        if editing:
            log(EVENT, 'Pokus o odchod z řádkového formuláře během editace')
            if editing.changed and  \
                   run_dialog(Question, _("Uložit zeditovaný řádek?"), True):
                log(EVENT, 'Vyžádáno uložení')
                self._on_line_commit()
            else:
                log(EVENT, 'Uložení zamítnuto')
                self._on_line_rollback()

    def _total_height(self):
        g = self._grid
        height = g.GetColLabelSize()
        rows = self._grid.GetNumberRows()
        if rows:
            height += rows * g.GetRowSize(0)
        if self._title_bar:
            height += self._title_bar.GetSize().height
        return height

    def _find_row_by_values(self, cols, values):
        """Vrať číslo řádku v gridu odpovídající daným hodnotám.

        Arguemnty:

          cols -- sekvence názvů sloupců, které mají být prohledávány.
          values -- sekvence hodnot sloupců jako instancí 'pytis.data.Value' v
            pořadí odpovídajícím 'cols'.

        Pro obě sekvence platí, že pokud jsou jednoprvkové, mohou být hodnoty
        předány i přímo, bez obalení do sekvenčního typu.

        """
        cols = xtuple(cols)
        values = xtuple(values)
        assert len(cols) == len(values)
        condition = apply(pytis.data.AND, map(pytis.data.EQ, cols, values))
        data = self._data
        data.rewind()
        success, result = db_operation(lambda: data.search(condition))
        if not success:
            row = None
        elif result == 0:
            row = 0
        else:
            row = result - 1
        return row

    
    def select_row(self, position, _invoke_callback=True):
        """Zvýrazni řádek dle 'position'.

        Argument 'position' může mít některou z následujících hodnot:

          None -- nebude vysvícen žádný řádek.
          Nezáporný integer -- bude vysvícen řádek příslušného pořadí, přičemž
            řádky jsou číslovány od 0.
          Datový klíč -- bude vysvícen řádek s tímto klíčem, kterým je tuple
            instancí třídy 'pytis.data.Value'.
          Slovník hodnot -- bude vysvícen první nalezený řádek obsahující
            hodnoty slovníku (instance 'pytis.data.Value') v sloupcích určených
            klíči slovníku.
          Instance třídy 'pytis.data.Row', kompatibilní s datovým objektem
            seznamu -- bude převeden na datový klíč.

        Pokud 'position' neodpovídá žádnému řádku, nebude žádný řádek vysvícen.
        Pokud je editován nějaký řádek a 'position' není 'None', vysviť
        editovaný řádek bez ohledu na hodnotu 'position'.

        Vrací: Číslo vysvíceného řádku nebo 'None', pokud nebyl žádný řádek
        vysvícen.

        """
        # Během editace může `position' obsahovat nevyhledatelná data.
        if position is not None and self._table.editing():
            position = self._table.editing().row
        if isinstance(position, pytis.data.Row):
            position = self._data.row_key(position)
        if isinstance(position, types.TupleType):
            cols = [c.id() for c in self._data.key()]
            position = self._find_row_by_values(cols, position)
        if isinstance(position, types.DictType):
            position = self._find_row_by_values(position.keys(),
                                                position.values())
        if position is None:
            position = -1
        return self._select_cell(row=position, invoke_callback=_invoke_callback)

    def refresh(self, reset=None, when=None):
        """Aktualizuj data seznamu z datového zdroje.

        Překresli celý seznam v okamžiku daném argumentem 'when' se zachováním
        parametrů dle argumentu 'reset'.

        Argumenty:

          reset -- určuje, které parametry zobrazení mají být zachovány a které
            změněny.  Hodnotou je buď 'None', nebo dictionary.  Je-li hodnotou
            'None', zůstane zachována filtrovací podmínka, třídění i vybraný
            řádek (vzhledem k jeho obsahu, ne pořadí), je-li to možné.  Je-li
            hodnotou prázdné dictionary, jsou naopak všechny tyto parametry
            resetovány na své počáteční hodnoty.  Jinak jsou resetovány právě
            ty parametry, pro něž v dictionary existuje klíč (jeden z řetězců
            'sorting', 'grouping', 'condition', 'position' a 'filter_flag'),
            a to na hodnotou z dictionary pro daný klíč.  Parametr
            'filter_flag' udává, zda má být zobrazena indikace filtru.
          when -- určuje, zda a kdy má být aktualizace provedena, musí to být
            jedna z 'DOIT_*' konstant třídy.  Implicitní hodnota je
            'DOIT_AFTEREDIT', je-li 'reset' 'None', 'DOIT_IMMEDIATELY' jinak.

        Vrací: Pravdu, právě když byla aktualizace provedena.

        """
        if self._block_refresh:
            return
        assert when in (None,           # to je pouze interní hodnota
                        self.DOIT_IMMEDIATELY, self.DOIT_AFTEREDIT,
                        self.DOIT_IFNEEDED), \
                        ("Invalid argument 'when'", when)
        assert reset is None or type(reset) == type({}), reset
        if when is None:
            if reset is None:
                when = self.DOIT_AFTEREDIT
            else:
                when = self.DOIT_IMMEDIATELY
        if reset == None:
            reset = {}
        elif reset == {}:
            reset = {'sorting': self._lf_initial_sorting,
                     'grouping': self._lf_initial_grouping,
                     'condition': self._lf_initial_condition,
                     'position': self._initial_position,
                     'filter_flag': False}
        # Jdeme na to
        if __debug__: log(DEBUG, 'Požadavek na refresh:', (reset, when))
        if when is self.DOIT_IFNEEDED:
            if self._reshuffle_request == self._last_reshuffle_request or \
                   self._reshuffle_request > time.time():
                if __debug__: log(DEBUG, 'Refresh není třeba provádět nyní')
                return False
        if when is self.DOIT_IMMEDIATELY:
            QUESTION = _("Zrušit změny záznamu a aktualizovat seznam?")
            delay = not self._finish_editing(question=QUESTION)
        else:
            delay = (self._table.editing() is not None) # nechceme držet info
        if delay:
            if __debug__: log(DEBUG, 'Refresh odložen do ukončení editace')
            return False
        # Refresh nyní bude skutečně proveden
        for k, v in reset.items():
            if k == 'condition':
                self._lf_condition = v
            elif k == 'sorting':
                self._lf_sorting = v
            elif k == 'grouping':
                self._lf_grouping = v
            elif k == 'position':
                self._position = v
            elif k == 'filter_flag':
                self._lf_indicate_filter = v
            else:
                raise ProgramError('Invalid refresh parameter', k)
        key = self._current_key()
        row = max(0, self._current_cell()[0])
        self._last_reshuffle_request = self._reshuffle_request = time.time()
        
        self._update_grid(data_init=True)
        
        if key is not None:
            self.select_row(key)
            # Pokud se nepodařilo nastavit pozici na předchozí klíč,
            # pokusíme se nastavit pozici na předchozí číslo řádku v gridu.
            if self._current_key() != key and \
                   row < self._table.GetNumberRows() and row >= 0:
                self.select_row(row)
        else:
            self.select_row(row)
        return True

    def status_fields(self):
        # TODO: zatím je podoba statusbaru určena specifikací, ale bylo by
        # rozumné to celé předělat, aby se statusbar dynamicky měnil podle
        # aktuálního formuláře (s využitím této metody).
        return (('list-position', 7),)

    # wx metody

    def _on_size(self, event):
        size = event.GetSize()
        g = self._grid
        oldsize = g.GetSize()
        width = size.width
        height = size.height
        if width == oldsize.width:
            event.Skip()
            return False
        if height < self._total_height():
            width = width - wx.SystemSettings.GetMetric(wx.SYS_VSCROLL_X)
        if width > self._total_width:
            coef = float(width) / self._total_width
        else:
            coef = 1
        total = 0
        last = None
        # Přenastav šířky sloupců
        for i, c in enumerate(self._columns):
            if not c.column_width():
                continue
            if c.fixed():
                w = g.GetColSize(i)
            else:
                w = int(self._column_width(g, c)*coef)
                g.SetColSize(i, w)
                last = i
            total += w
        if coef != 1 and total != width and last is not None:
            g.SetColSize(last, g.GetColSize(last) + (width - total))
        event.Skip()

    def Close(self):
        self._data.remove_callback_on_change(self.on_data_change)
        try:
            self._data.close()
        except pytis.data.DBException:
            pass
        # Musíme ručně zrušit editory, jinak se dočkáme segmentation fault.
        for e in self._editors:
            e.close()
        # Musíme tabulce zrušit datový objekt, protože jinak do něj bude šahat
        # i po kompletním uzavření starého gridu (!!) a rozhodí nám tak data
        # v novém gridu.
        self._table.close()    
        return super_(ListForm).Close(self)

    def Show(self, show):
        if not show:
            self.exit_check()
        return super_(ListForm).Show(self, show)
    
    # Ostatní veřejné metody

    def focus(self):
        super_(ListForm).focus(self)
        self.show_position()
        self._update_selection_colors()
        self._grid.SetFocus()
        
    def defocus(self):
        super_(ListForm).defocus(self)
        self._update_selection_colors()



class CodebookForm(ListForm, PopupForm, KeyHandler):
    """Formulář pro zobrazení výběrového seznamu (číselníku).

    Výběrový seznam zobrazuje řádky dat, z nichž uživatel některý řádek
    vybere.  Uživatel kromě výběru a listování nemůže s řádky nijak
    manipulovat.

    Třída se od svého předka liší následujícími vlastnostmi:

    - Jsou zobrazeny pouze sloupce, jejichž identifikátory jsou v množině
      'columns' předané jako argument konstruktoru (pokud není None).

    - Formulář je zobrazen jako modální okno pomocí metody 'run()', která
      skončí po výběru položky a vrátí instanci PresentedRow pro vybraný
      řádek. 
      Pokud byl formulář ukončen jinak než výběrem záznamu, je vrácena hodnota
      'None'.

    """

    _DEFAULT_WINDOW_HEIGHT = 500

    def __init__(self, parent, *args, **kwargs):
        parent = self._popup_frame(parent)
        super_(CodebookForm).__init__(self, parent, *args, **kwargs)
        self.set_callback(ListForm.CALL_ACTIVATION, self._on_activation)
        h = min(self._DEFAULT_WINDOW_HEIGHT, self._total_height()+50)
        self.SetSize((self._total_width+30, h))
        wx_callback(wx.grid.EVT_GRID_CELL_LEFT_DCLICK, self._grid,
                    lambda e: self._on_activation())

    def _init_attributes(self, condition=None, begin_search=None, **kwargs):
        """Zpracuj klíčové argumenty konstruktoru a inicializuj atributy.

        Argumenty:

          condition -- podmínka filtrující řádky číselníku jako instance
            'pytis.data.Operator'.
          begin_search -- Pokud není None, bude po otevření formuláře
            automaticky nastartováno inkrementální vyhledávání. Pokud
            je hodnota řetězec, je chápán jako identifikátor
            sloupce, ve kterém se má provádět vyhledávání. Není-li ho            
            hodnota řetězec, nebo neodpovídá-li žádnému sloupci,
            je vyhledávání prováděno automaticky nad sloupečkem s
            primárním tříděním.
            
        """
        super_(CodebookForm)._init_attributes(self, **kwargs)
        self._begin_search = begin_search
        if condition is not None:
            condition = pytis.data.AND(self._lf_initial_condition, condition)
            self._lf_initial_condition = condition
          
    def _on_idle(self, event):
        ListForm._on_idle(self, event)
        if not hasattr(self, '_focus_forced_to_grid'):
            self._grid.SetFocus()
            self._focus_forced_to_grid = True
        if self._begin_search:
            begin_search = self._begin_search
            self._begin_search = None
            if isinstance(begin_search, types.StringType):
                col_id = begin_search
            elif self._lf_sorting is not None:
                col_id = self._lf_sorting[0][0]
            else:
                message(_("Nelze začít inkrementální vyhledávání. " +
                          "Číselník neobsahuje žádný setříděný sloupec!"),
                        beep_=True)
            col = find(col_id, self._columns, key=lambda c:c.id())
            assert col is not None, "Invalid column: %s" % col_id
            self._select_cell(row=0, col=self._columns.index(col))
            self._on_incremental_search(False)
                
        
    def _context_menu(self):
        return (MItem(_("Vybrat"),
                      command = ListForm.COMMAND_ACTIVATE),
                )

    def on_command(self, command, **kwargs):
        if command == Application.COMMAND_LEAVE_FORM:
            self._leave_form()
            return True
        return super_(CodebookForm).on_command(self, command, **kwargs)

    def _on_activation(self):
        """Nastav návratovou hodnotu a ukonči modální dialog."""
        self._result = self.current_row()
        self._parent.EndModal(1)
        return True

    
class BrowseForm(ListForm):
    """Formulář pro prohlížení dat s možností editace.

    Formulář je CallbackHandler a argumenty callbacků jsou shodné, jako je
    tomu pro třídu 'List'.
    
    """

    CALL_EDIT_RECORD = 'CALL_EDIT_RECORD'
    """Voláno při požadavku na editaci akt. záznamu."""
    CALL_NEW_RECORD = 'CALL_NEW_RECORD'
    """Voláno při požadavku na vytvoření nového záznamu.

    Může mít jeden nepovinný argument.  Pokud je pravdivý, bude nový záznam
    předvyplněn zkopírováním dat aktuálního řádku.

    """

    class _PrintResolver (pytis.output.OutputResolver):
        P_NAME = 'P_NAME'
        class _Spec:
            def body(self, resolver, variant=None):
                table_id = resolver.p(BrowseForm._PrintResolver.P_NAME)
                result = pytis.output.data_table(resolver, table_id)
                return result
            def doc_header(self, resolver, variant=None):
                return None
            def doc_footer(self, resolver, variant=None):
                return None
            def coding(self, resolver, variant=None):
                if wx.Font_GetDefaultEncoding() == \
                   wx.FONTENCODING_ISO8859_2:
                    result = pytis.output.Coding.LATIN2
                else:
                    result = pytis.output.Coding.ASCII
                return result
        def _get_module(self, module_name):
            try:
                result = pytis.output.OutputResolver._get_module(self,
                                                                 module_name)
            except ResolverModuleError:
                result = self._Spec()
            return result

    def __init__(self, *args, **kwargs):
        super_(BrowseForm).__init__(self, *args, **kwargs)
        self.set_callback(self.CALL_NEW_RECORD,  self._on_new_record)
        self.set_callback(self.CALL_EDIT_RECORD,
                          lambda k: self._on_edit_record(k)),
        self.set_callback(ListForm.CALL_ACTIVATION,
                          lambda key: self._run_form(BrowsableShowForm, key))
        self.set_callback(ListForm.CALL_ALTERNATE_ACTIVATION,
                          lambda key: self._run_form(DescriptiveDualForm, key))
        
    def _run_form(self, form, key):
        name = self._redirected_name(key)
        if not name:
            name = self._name
        if issubclass(form, EditForm):
            kwargs = {'key': key}
        else:
            kwargs = {'select_row': key}
        run_form(form, name, condition=self._lf_condition,
                 sorting=self._lf_sorting, **kwargs)

    def _formatter_parameters(self):
        name = self._name
        return {(name+'/'+pytis.output.P_CONDITION):
                pytis.data.AND(self._lf_initial_condition, self._lf_condition),
                (name+'/'+pytis.output.P_SORTING):
                self._lf_translated_sorting(),
                (name+'/'+pytis.output.P_KEY):
                self._current_key(),
                (name+'/'+pytis.output.P_ROW):
                copy.copy(self._table.row(self._current_cell()[0])),
                (name+'/'+pytis.output.P_DATA):
                copy.copy(self._data)
                }

    def _context_menu(self):
        # Sestav specifikaci kontextového menu
        menu = super_(BrowseForm)._context_menu(self) + (
            MItem(_("Editovat buňku"),
                  command=ListForm.COMMAND_EDIT),
            MItem(_("Filtrovat podle buňky"),
                  command=ListForm.COMMAND_FILTER_BY_CELL),
            MItem(_("Zkopírovat obsah buňky"),
                  command=ListForm.COMMAND_COPY_CELL),
            MSeparator(),
            MItem(_("Editovat záznam"),
                  command=BrowseForm.COMMAND_EDIT_RECORD),
            MItem(_("Smazat záznam"),
                  command=RecordForm.COMMAND_DELETE_RECORD),
            MItem(_("Náhled"),
                  command=ListForm.COMMAND_ACTIVATE),
            MItem(_("Náhled v druhém formuláři"),
                  command=ListForm.COMMAND_ACTIVATE_ALTERNATE),
            MItem(_("Zobrazit související číselník"),
                  command=ListForm.COMMAND_SHOW_CELL_CODEBOOK),
            )
        custom_menu = self._view.popup_menu()
        if custom_menu:
            menu += (MSeparator(),) + custom_menu
        return menu

    def _redirected_name(self, key):
        redirect = self._view.redirect()
        if redirect is not None:
            success, row = db_operation(lambda : self._data.row(key))
            if not success:
                raise ProgramError('Row read failure')
            name = redirect(row)
            if name is not None:
                assert isinstance(name, types.StringType)
                return name
        return None

    def _on_import_interactive(self):
        if not self._data.accessible(None, pytis.data.Permission.INSERT):
            msg = _("Nemáte práva pro vkládání záznamů do této tabulky.")
            message(msg, beep_=True)
            return False
        msg = _("Nejprve vyberte soubor obsahující importovaná data. "
                "Poté budete moci zkontrolovat a potvrdit každý záznam.\n\n"
                "*Formát vstupního souboru:*\n\n"
                "Každý řádek obsahuje seznam hodnot oddělených zvoleným "
                "znakem, nebo skupinou znaků (vyplňte níže). "
                "Tabelátor zapište jako ='\\t'=.\n\n"
                "První řádek obsahuje identifikátory sloupců a určuje tedy "
                "význam a pořadí hodnot v následujících (datových) řádcích.\n\n"
                "Identifikátory jednotlivých sloupců jsou následující:\n\n" + \
                "\n".join(["|*%s*|=%s=|" % \
                           (c.column_label(), c.id().replace('_', '!_'))
                           for c in self._columns]))
        separator = run_dialog(InputDialog, 
                               title=_("Hromadné vkládání dat"),
                               report=msg, report_format=TextFormat.WIKI,
                               prompt="Oddělovač", value='|')
        if not separator:
            if separator is not None:
                message(_("Nebyl zadán oddělovač."), beep_=True)
            return False
        separator.replace('\\t', '\t')
        while 1:
            filename = run_dialog(FileDialog)
            if filename is None:
                message(_("Nebyl zadán soubor.  Proces ukončen."), beep_=True)
                return False
            try:
                f = open(filename)
            except IOError, e:
                msg = _("Nebylo možno otevřít soubor '%s': %s")
                run_dialog(Error, msg % (filename, str(e)))
                continue
            break
        try:
            columns = []
            for key in f.readline().split(separator):
                col = find(key.strip(), self._columns, key=lambda c: c.id())
                if col:
                    columns.append(col)
                else:
                    msg = _("Chybný identifikátor sloupce: %s")
                    run_dialog(Error, msg % key.strip())
                    return False
            i = 1 # aktuální číslo řádku (jeden už byl přečten)
            n = 0 # počet skutečně vložených záznamů
            prefill = self.prefill()
            for line in f:
                i += 1
                values = line.rstrip('\r\n').split(separator)
                if len(values) != len(columns):
                    msg = _("Chyba na řádku %d: "
                            "Počet hodnot neodpovídá počtu sloupců.\n"
                            "Chcete přesto pokračovat (dalším záznamem)?")
                    if run_dialog(Question, msg % i,
                                  title=_("Chyba vstupních dat"),
                                  icon=Question.ICON_ERROR):
                        continue
                    else:
                        break
                data = prefill and copy.copy(prefill) or {}
                for col, val in zip(columns, values):
                    type = col.type(self._data)
                    value, error = type.validate(val)
                    if error:
                        msg = _("Chybná hodnota sloupce '%s' na řádku %d: %s\n"
                                "Chcete přesto záznam vložit?")
                        msg = msg % (col.id(), i, error.message())
                        if not run_dialog(Question, msg,
                                          title=_("Chyba vstupních dat"),
                                          icon=Question.ICON_ERROR):
                            break
                    data[col.id()] = value
                else:
                    result = new_record(self._name, prefill=data)
                    if result:
                        n += 1
                    else:
                        msg = _("Ještě nebyly zpracovány všechny řádky "
                                "vstupních dat.\n"
                                "Chcete pokračovat ve vkládání?")
                        if not run_dialog(Question, msg):
                            break
        finally:
            f.close()
        run_dialog(Message, _("%d/%d záznamů bylo vloženo.") % (n, i),
                   title=_("Hromadné vkládání dat dokončeno"))
    
    def _on_new_record(self, copy=False):
        if not self._data.accessible(None, pytis.data.Permission.INSERT):
            msg = _("Nemáte práva pro vkládání záznamů do této tabulky.")
            message(msg, beep_=True)
            return False
        if copy:
            key = self.current_key()
        else:
            key = None
        result = new_record(self._name, key=key, prefill=self.prefill())
        if isinstance(result, PresentedRow):
            self.select_row(result.row())

    def _on_edit_record(self, key):
        on_edit_record = self._view.on_edit_record()
        if on_edit_record is not None:
            row = self._table.row(self._current_cell()[0])
            on_edit_record(row=row)
            self.refresh()
            # TODO: tento refresh je tu jen pro případ, že byla uživatelská
            # procedura ošetřena jinak než vyvoláním formuláře.  Protože to
            # samo už je hack, tak ať si raději také tvůrce provádí refresh
            # sám, protože tady je volán ve všech ostatních případech zbytečně
            # a zdržuje.
        else:
            self._run_form(PopupEditForm, key)


    def _on_print_(self, spec_path):
        log(EVENT, 'Vyvolání tiskového formuláře')
        name = self._name
        if not spec_path:
            try:
                spec_paths = self._resolver.get(name, 'print_spec')
            except ResolverError:
                spec_paths = None
            if spec_paths:
                spec_path = spec_paths[0][1]
            else:
                spec_path = os.path.join('output', name)
        P = self._PrintResolver
        parameters = self._formatter_parameters()
        parameters.update({P.P_NAME: name})
        print_resolver = P(self._resolver, parameters=parameters)
        resolvers = (print_resolver,)
        formatter = pytis.output.Formatter(resolvers, spec_path)
        run_form(PrintForm, name, formatter=formatter)

    def on_command(self, command, **kwargs):
        if command == Form.COMMAND_PRINT:
            self._on_print_(kwargs.get('print_spec_path'))
        elif command == BrowseForm.COMMAND_IMPORT_INTERACTIVE:
            self._on_import_interactive()
        elif command == BrowseForm.COMMAND_NEW_RECORD:
            self._run_callback(self.CALL_NEW_RECORD)
        elif command == BrowseForm.COMMAND_NEW_RECORD_COPY:
            self._run_callback(self.CALL_NEW_RECORD, (True,))
        elif command == BrowseForm.COMMAND_EDIT_RECORD:
            key = self.current_key()
            if key is not None:
                self._run_callback(self.CALL_EDIT_RECORD, (key,))
        else:
            return super_(BrowseForm).on_command(self, command, **kwargs)
        return True


class FilteredBrowseForm(BrowseForm):
    """Prohlížecí formulář s filtrovaným obsahem.

    Oproti obyčejnému prohlížecímu formuláři zobrazuje filtrovaný formulář
    pouze řádky splňující podmínku zadanou v konstruktoru, případně
    kombinovanou se zadaným datovým klíčem.

    """

    def __init__(self, *args, **kwargs):
        super_(FilteredBrowseForm).__init__(self, *args, **kwargs)
        self._init_filter()
    
    def _init_attributes(self, condition=None, **kwargs):
        """Zpracuj klíčové argumenty konstruktoru a inicializuj atributy.
        
        Argumenty:

          condition -- hodnotou je podmínkový výraz pro datové dotazy, tak jak
            je definovaný v metodě 'pytis.data.Data.select()'.  Tento výraz může
            být i funkcí jednoho argumentu vracející příslušný podmínkový
            výraz, předaným argumentem je dictionary id sloupců (strings) a
            jejich hodnot (instancí třídy 'pytis.data.Value').

        Má-li podmínka podobu statického operátoru, je filtrování provedeno
        ihned a nelze je změnit.  Má-li naopak podmínka podobu funkce, jsou
        zobrazena všechna data až do prvního zavolání metody `filter()';
        následnými voláními této funkce pak lze výběr upravovat.

        """
        super_(FilteredBrowseForm)._init_attributes(self, **kwargs)
        assert callable(condition) or isinstance(condition, pytis.data.Operator)
        self._condition = condition

    def _init_filter(self):
        self.filter()

    def filter(self, data=None):
        """Filtruj data dle podmínky z konstruktoru a 'data'.

        Argumenty:

          data -- 'None' nebo dictionary, jehož klíče jsou ids sloupců
            (strings) a hodnoty jim příslušné hodnoty (instance třídy
            'pytis.data.Value').  Jestliže je dictionary prázdné, budou
            odfiltrovány všechny záznamy; jestliže je 'None', budou naopak
            všechny záznamy zobrazeny.  Tento volitelný argument není nutno
            v zápisu volání funkce klíčovat.

        Je-li podmínka zadaná v konstruktoru statickým operátorem, metoda pouze
        zavolá 'refresh()' na obalený list.  Je-li naopak podmínka zadaná
        v konstruktoru funkcí, je provedeno přefiltrování v závislosti na
        argumentu 'data' -- je-li 'None', tak jsou zobrazena všechna data bez
        omezení výběru, jinak jsou zobrazena data dle podmínky vrácené voláním
        podmínkové funkce.

        """
        #log(EVENT, 'Filtrace obsahu formuláře:', (self._name, data))
        if data is None:
            condition = None
        elif data == {}:
            condition = pytis.data.OR()
            data = None
        elif callable(self._condition):
            condition = self._condition(data)
        else:
            condition = self._condition
        self._lf_initial_condition = condition
        self.refresh(reset={'condition': None})


class SideBrowseForm(FilteredBrowseForm):
    """Formulář zobrazující podmnožnu řádků závislých na jiném řádku dat.

    Data tohoto formuláře jsou filtrována v závislosti na aktuálním řádku v
    jiném formuláři.
    
    Filtrovací podmínka je vytvářena automaticky jako ekvivalence hodnot
    vazebních sloupců v hlavním a vedlejším formuláři.  Identifikátory
    příslušných sloupců jsou dány argumenty `binding_column' a
    `sibling_binding_column'.

    """

    def _init_attributes(self, sibling_name, sibling_row,
                         sibling_binding_column, binding_column,
                         hide_binding_column, append_condition=None,
                         title=None, **kwargs):
        """Zpracuj klíčové argumenty konstruktoru a inicializuj atributy.
        
        Argumenty:

          title -- ???
          sibling_name -- jméno specifikace hlavního formuláře; řetězec
          sibling_row -- funkce bez argumentů, která vrátí aktuální datový
            řádek hlavního formuláře.
          sibling_binding_column -- identifikátor vazebního sloupce v hlavním
            formuláři; řetězec
          binding_column -- identifikátor vazebního sloupce ve vedlejším
            formuláři; řetězec
          hide_binding_column -- pravdivá hodnota způsobí, že vazební sloupec
            nebude zobrazen.
          append_condition -- None nebo funkce jednoho argumentu, kterým je
            aktuální řádek hlavního formuláře. V tomto případě musí funkce
            vrátit instanci Operator, která se připojí k implicitní
            podmínce provazující vazební sloupce.

        """
        column_condition = lambda row: pytis.data.EQ(binding_column,
                                                   row[sibling_binding_column])
        if append_condition:
            condition = lambda row: pytis.data.AND(column_condition(row),
                                                 append_condition(row))
        else:
            condition = column_condition
        super_(SideBrowseForm)._init_attributes(self, condition, **kwargs)
        self._sibling_name = sibling_name
        self._sibling_row = sibling_row
        self._title = title
        if hide_binding_column:
            self._orig_columns = filter(lambda c: c != binding_column,
                                        self._orig_columns)
        
    def _init_filter(self):
        self.filter({})

    def title(self):
        if self._title is not None:
            return self._title
        return super_(SideBrowseForm).title(self)
        
    def _formatter_parameters(self):
        parameters = FilteredBrowseForm._formatter_parameters(self)
        extra_parameters = {self._sibling_name+'/'+pytis.output.P_ROW:
                            self._sibling_row()}
        parameters.update(extra_parameters)
        return parameters

    def _update_selection_colors(self):
        g = self._grid
        if focused_window() is not self:
            g.ClearSelection()
        else:
            g.SelectRow(g.GetGridCursorRow())
            super_(SideBrowseForm)._update_selection_colors(self)
