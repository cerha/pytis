# -*- coding: iso-8859-2 -*-

# Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006 Brailcom, o.p.s.
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

"""Pr�ce s�formul��i se seznamov�m zobrazen�m.

Modul jednak interpretuje specifikaci formul��� (viz modul 'spec') pro
seznamov� zobrazen� a jednak zaji��uje pr�ci s�n� prost�ednictv�m objekt�
wxWindows.

"""

# Terminologick� pozn�mka: Prom�nn� s�n�zvem `row' obvykle zna�� ��slo ��dku
# (��slov�no od�0).  Jedn�-li se o�obsah ��dku, naz�v� se p��slu�n� prom�nn�
# obvykle `the_row'.  Matouc� jm�no `row' bylo p�evzato z�wxWindows.

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
    
### Formul��e


class ListForm(LookupForm, TitledForm, Refreshable):
    """Spole�n� nadt��da pro formul��e se seznamov�m zobrazen�m.

    Tyto formul��e zobrazuj� seznam ��dk�, rozd�len�ch do n�kolika sloupc�,
    tedy v�podstat� tabulku.  T��da definuje spole�n� vlastnosti, jako mo�nosti
    navigace, vyhled�v�n�, �azen� apod.

    T��da je 'CallbackHandler' a jako argument callbackov� funkce p�ed�v�
    slovn�k, jeho� kl��e jsou id sloupc� (stringy) a hodnoty jsou hodnoty
    t�chto sloupc� (op�t stringy) ��dku, jeho� se callback t�k�.

    Tato t��da obvykle nen� pou��v�na p��mo, n�br� slou�� jako z�klad pro
    specializovan� t��dy.

    """
    CALL_ACTIVATION = 'CALL_ACTIVATION'
    """Konstanta callbacku aktivace ��dku."""
    CALL_MODIFICATION = 'CALL_MODIFICATION'
    """Konstanta callbacku modifikace ��dku."""
    CALL_USER_INTERACTION = 'CALL_USER_INTERACTION'
    """Konstanta callbacku interakce u�ivatele."""

    _REFRESH_PERIOD = 60 # sekund
    _SELECTION_CALLBACK_DELAY = 3 # des�tky milisekund
    _TITLE_FOREGROUND_COLOR = WxColor(210, 210, 210)
    
    _STATUS_FIELDS = ('list-position', 'data-changed')


    def __init__(self, *args, **kwargs):
        super_(ListForm).__init__(self, *args, **kwargs)
        # Nastav kl�vesov� zkratky z kontextov�ch menu.
        for item in self._context_menu() + self._edit_menu():
            if isinstance(item, MItem):
                if item.hotkey() != (None,):
                    self.define_key(item.hotkey(), item.command(), item.args())
        # Z�v�re�n� akce
        self._data.add_callback_on_change(self.on_data_change)
        wx_callback(wx.EVT_SIZE, self, self._on_size)
        self._select_cell(row=self._position)
        self.set_callback(ListForm.CALL_ACTIVATION, self._on_activation)

    def _init_attributes(self, columns=None, **kwargs):
        """Zpracuj kl��ov� argumenty konstruktoru a inicializuj atributy.

        Argumenty:

          columns -- pokud nen� None, bude formul�� pou��vat dan� sloupce.
            Jinak je pou�it seznam sloupc� dan� specifikac�.  Hodnotou je
            sekvence identifik�tor� sloupc� obsa�en�h ve specifikaci.
          kwargs -- argumenty p�edan� konstruktoru p�edka.

        """
        super_(ListForm)._init_attributes(self, **kwargs)
        self._default_columns_changed = False
        assert columns is None or is_sequence(columns)
        if not columns:
            default = self._default_columns()
            columns = self._get_state_param('columns', default)
            if default != self._get_state_param('default_columns', default):
                self._default_columns_changed = True
                columns = [id for id in columns
                           if self._view.field(id) is not None]
        self._columns = [self._view.field(id) for id in columns]
        # Inicializace atribut�
        self._fields = self._view.fields()
        self._enable_inline_insert = self._view.enable_inline_insert()
        self._selection_candidate = None
        self._selection_callback_candidate = None
        self._selection_callback_tick = None
        self._in_select_cell = False
        self._last_reshuffle_request = self._reshuffle_request = 0
        self._current_editor = None
        self._column_to_move = None
        self._column_move_target = None
        self._mouse_dragged = False
        # Parametry zobrazen�
        self._initial_position = self._position = 0

    def _default_columns(self):
        return self._view.columns()
        
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
        self._update_colors()
        sizer.Add(self._grid, 1, wx.EXPAND|wx.FIXED_MINSIZE)

    def _column_width(self, column):
        try:
            #TODO: Column widths should be saved/restored in dialog units.
            return self._get_state_param('column_width', {})[column.id()]
        except KeyError:
            width = max(column.column_width(), len(column.column_label()))
            return dlg2px(self._grid, 4*width + 8)

    def _create_grid(self):
        if __debug__: log(DEBUG, 'Vytv��en� nov�ho gridu')
        # Vytvo� grid a tabulku
        g = wx.grid.Grid(self, wx.NewId())
        # Inicializuj datov� select
        row_count = self._init_select()
        self._table = table = \
          _grid.ListTable(self._parent, self._data, self._fields,
                          self._columns, row_count, sorting=self._lf_sorting,
                          grouping=self._lf_grouping, prefill=self._prefill,
                          row_style=self._view.row_style())
        g.SetTable(table, True)
        g.SetRowLabelSize(0)
        g.SetColLabelSize(dlg2px(g, 0, 12).GetHeight())
        g.SetColLabelAlignment(wx.CENTER, wx.CENTER)
        g.SetMargins(0,0)
        g.DisableDragGridSize()
        g.SetSelectionMode(wx.grid.Grid.wxGridSelectRows)
        labelfont = g.GetLabelFont()
        labelfont.SetWeight(wx.NORMAL)
        g.SetLabelFont(labelfont)
        g.SetDefaultRowSize(dlg2px(g, 0, 10).GetHeight())
        # (Re)inicializuj atributy instance a gridu
        self._editors = []
        def registration(editor):
            self._current_editor = editor
        editable = False
        for i, c in enumerate(self._columns):
            # typ sloupce
            t = c.type(self._data)
            # zarovn�n�
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
        self.editable = editable
        labels = g.GetGridColLabelWindow()
        # Event handlery
        wx_callback(wx.grid.EVT_GRID_SELECT_CELL,   g, self._on_select_cell)
        wx_callback(wx.grid.EVT_GRID_COL_SIZE,      g, self._on_label_drag_size)
        wx_callback(wx.grid.EVT_GRID_EDITOR_SHOWN,  g, self._on_editor_shown)
        wx_callback(wx.grid.EVT_GRID_CELL_RIGHT_CLICK, g, self._on_context_menu)
        wx_callback(wx.EVT_MOUSEWHEEL, g,      self._on_wheel)
        wx_callback(wx.EVT_IDLE,       g,      self._on_idle)
        wx_callback(wx.EVT_KEY_DOWN,   g,      self.on_key_down)
        wx_callback(wx.EVT_LEFT_DOWN,  labels, self._on_label_left_down)
        wx_callback(wx.EVT_LEFT_UP,    labels, self._on_label_left_up)
        wx_callback(wx.EVT_RIGHT_DOWN, labels, self._on_label_right_down)
        wx_callback(wx.EVT_MOTION,     labels, self._on_label_mouse_move)
        wx_callback(wx.EVT_PAINT,      labels, self._on_label_paint)
        if __debug__: log(DEBUG, 'Nov� grid vytvo�en')
        return g

    def _on_editor_shown(self, event):
        if self._table.editing():
            event.Skip()
        else:
            event.Veto()
            self._select_cell(row=max(0, event.GetRow()), col=event.GetCol())
    
    def _update_grid(self, data_init=False, inserted_row_number=None,
                     inserted_row=None, delete_column=None, insert_column=None,
                     inserted_column_index=None, reset_columns=False):
        g = self._grid
        t = self._table
        def notify(id, *args):
            msg = wx.grid.GridTableMessage(t, id, *args)
            g.ProcessTableMessage(msg)
        current_row = self._table.current_row()
        if data_init:
            row_count = self._init_select()
        else:
            row_count = self._lf_select_count
            self._data.rewind()
        if inserted_row_number is not None:
            row_count = row_count + 1
        old_row_count = self._table.GetNumberRows()
        new_row_count = row_count
        old_columns = tuple([c.id() for c in self._columns])
        # Uprav velikost gridu
        g.BeginBatch()
        if reset_columns:
            deleted = len(self._columns)
            self._columns = [self._view.field(id)
                             for id in self._default_columns()]
            inserted = len(self._columns)
            notify(wx.grid.GRIDTABLE_NOTIFY_COLS_DELETED, 0, deleted)
            notify(wx.grid.GRIDTABLE_NOTIFY_COLS_INSERTED, 0, inserted)
        if delete_column is not None:
            i = self._columns.index(delete_column)
            del self._columns[i]
            notify(wx.grid.GRIDTABLE_NOTIFY_COLS_DELETED, i, 1)
        if insert_column is not None:
            if inserted_column_index is None:
                i = len(self._columns)
            else:
                i = inserted_column_index
            self._columns.insert(i, insert_column)
            notify(wx.grid.GRIDTABLE_NOTIFY_COLS_INSERTED, i, 1)
        new_columns = tuple([c.id() for c in self._columns])
        if reset_columns:
            self._unset_state_param('columns')
            self._unset_state_param('default_columns')
        elif new_columns != old_columns:
            self._set_state_param('columns', new_columns)
            self._set_state_param('default_columns', self._default_columns())
        t.update(columns=self._columns,
                 row_count=row_count, sorting=self._lf_sorting,
                 grouping=self._lf_grouping,
                 inserted_row_number=inserted_row_number,
                 inserted_row=inserted_row, prefill=self._prefill)
        ndiff = new_row_count - old_row_count
        if new_row_count < old_row_count:
            if new_row_count == 0:
                current_row = 1
            notify(wx.grid.GRIDTABLE_NOTIFY_ROWS_DELETED, current_row, -ndiff)
        elif new_row_count > old_row_count:
            notify(wx.grid.GRIDTABLE_NOTIFY_ROWS_APPENDED, ndiff)
        notify(wx.grid.GRIDTABLE_REQUEST_VIEW_GET_VALUES)
        g.EndBatch()
        # Z�v�re�n� �pravy
        if new_row_count != old_row_count:
            # This is a workaround of a wxWidgets bug.  The scrollbars are not
            # shown or hidden properly, until a size event is received by the
            # grid.  Thus we generate one artificially...
            g.SetSize(g.GetSize())
        # Tento _select_cell() zde nem��e b�t, proto�e vyvol� ukon�en� editace
        # p�i vkl�d�n� ��dku.  Pokud to je n�kdy pot�eba, bude nutn� volat
        # _select_cell() zvlṻ po _update_grid().  Zat�m to ale sp� vypad�,
        # �e je to tady zbyte�n� (kostlivec).  TC 2005-12-28
        #self._select_cell(row=self._position)
        self._update_colors()
        self._resize_columns()


    def _update_label_colors(self):
        color = self._lf_indicate_filter and config.filter_color or \
                self._TITLE_FOREGROUND_COLOR
        self._grid.SetLabelBackgroundColour(color)

    def _context_menu(self):
        """Vra� specifikaci \"kontextov�ho\" popup menu vybran� bu�ky seznamu.

        Vrac�: Sekvenci instanc� 'MItem'.

        Tuto metodu nech� odvozen� t��dy p�edefinuj�, pokud cht�j� zobrazovat
        kontextov� menu.
        
        """
        return ()

    def _edit_menu(self):
        return (
            MItem("Editovat bu�ku",
                  command = ListForm.COMMAND_EDIT),
            MItem("Ulo�it z�znam",
                  command = ListForm.COMMAND_LINE_COMMIT),
            MItem("Opustit editaci",
                  command = ListForm.COMMAND_FINISH_EDITING),
            MSeparator(),
            MItem("Kop�rovat obsah bu�ky",
                  command = ListForm.COMMAND_COPY_CELL),
            #MItem("", command = ListForm.COMMAND_LINE_ROLLBACK),
            )

    def _lf_sfs_columns(self):
        shown = tuple(self._columns)
        hidden = tuple([c for c in self._view.fields()
                        if c not in shown and c.column_label()])
        def labelfunc(c):
            label = c.column_label()
            if c in hidden:
                return "(" + label + ")"
            else:
                return label
        return sfs_columns(shown + hidden, self._data, labelfunc=labelfunc)
    
    # Pomocn� metody

    def _current_cell(self):
        """Vra� dvojici sou�adnic (ROW, COL) aktu�ln� bu�ky."""
        g = self._grid
        return g.GetGridCursorRow(), g.GetGridCursorCol()

    def current_row(self):
        row = self._current_cell()[0]
        if row < 0 or row >= self._grid.GetNumberRows():
            # P�i pr�zdn� tabulce m� wxGrid nastaven ��dek�0.
            return None
        else:
            return self._table.row(row)
        
    def _select_cell(self, row=None, col=None, invoke_callback=True):
        # Vrac� pravdu, pokud m��e b�t ud�lost provedena (viz _on_select_cell).
        if self._in_select_cell:
            return True
        self._in_select_cell = True
        if __debug__: log(DEBUG, 'P�echod na bu�ku gridu:', (row, col))
        try:
            g = self._grid
            current_row = g.GetGridCursorRow()
            current_col = g.GetGridCursorCol()
            if row is not None:
                assert isinstance(row, types.IntType)
                # Zkontroluj p��padn� opu�t�n� editace
                if not self._finish_editing(row=row):
                    log(EVENT, 'Zam�tnuto opu�t�n� editace ��dku')
                    return False
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
                            # Nevol�me callback ihned, sta�� a� po
                            # zastaven� scrolov�n�...
                            self._selection_callback_candidate = row
                            delay = self._SELECTION_CALLBACK_DELAY
                            self._selection_callback_tick = delay
                    self._position = row
                    # TODO: tady to zp�sobuje �patn� zobrazov�n� pozice v
                    #       dualform. Nahrazeno vol�n�m show_position v
                    #       _post_selection_hook.
                    #       Jin� �e�en�?
                    # self.show_position()
            elif col is not None and col != current_col:
                g.SetGridCursor(current_row, col)
                g.MakeCellVisible(current_row, col)
            if __debug__: log(DEBUG, 'V�b�r bu�ky proveden:', (row, col))
            return True
        finally:
            self._in_select_cell = False

    def _edit_cell(self):
        """Spus� editor aktu�ln�ho pol��ka."""
        row, col = self._current_cell()
        table = self._table
        cid = self._columns[col].id()
        if not table.row(table.editing().row).editable(cid):
            message(_("Pol��ko je needitovateln�"), kind=ACTION, beep_=True)
            return False
        self._grid.EnableCellEditControl()       
        log(EVENT, 'Spu�t�n editor pol��ka:', (row, col))
        return True
    
    def _finish_editing(self, question=None, row=None):
        # Vrac� pravdu, pr�v� kdy� nejsou akce blokov�ny editac� ��dku.
        
        table = self._table
        editing = table.editing()
        if not editing:
            return True
        if editing.row == row:
            return True
        if not editing.changed:
            if __debug__: log(DEBUG, 'Odchod z�needitovan�ho ��dku povolen')
            self._on_line_rollback()
            finish = True 
        else:
            log(EVENT, 'Pokus o�odchod z�rozeditovan�ho ��dku seznamu')
            if question == None:
                question = _("Zru�it zm�ny z�znamu?")
            buttons = bcancel, bsave, bcontinue = \
                      _("Zru�it"), _("Ulo�it"), _("Pokra�ovat v editaci")
            result = run_dialog(MultiQuestion, question, buttons=buttons,
                                default=bsave)
            finish = (result != bcontinue)
            if result == bcancel:
                log(EVENT, 'Odchod u�ivatelem povolen')
                self._on_line_rollback()
                finish = True
            elif result == bsave:
                log(EVENT, 'Odchod s�ulo�en�m ��dku')
                finish = self._on_line_commit()
            elif result is None or result == bcontinue:
                log(EVENT, 'Odchod u�ivatelem zam�tnut')
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
        # Mus�me vynutit p�ekreslen� cel� selection
        if g.IsSelection():
            g.ClearSelection()
            g.SelectRow(g.GetGridCursorRow())
        
    def _is_editable_cell(self, row, col):
        # Vra� pravdu, pokud je bu�ka dan�ho ��dku a sloupca editovateln�.
        editing = self._table.editing()
        if row == editing.row:
            the_row = editing.the_row
        else:
            the_row = self._table.row(row)
        id = self._columns[col].id()
        return the_row.editable(id)
    
    def _find_next_editable_cell(self):
        # Vra� pravdu, pokud bylo pohybem vpravo nalezeno editovateln� pol��ko.
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

    def _filter(self, condition):
        # TODO: Jak to bylo my�leno?
        # if self._lf_initial_condition:
        #      xcondition = pytis.data.AND(condition, self._lf_initial_condition)
        # else:
        #      xcondition = condition
        log(EVENT, 'U�ivatelsk� filtr:', condition)
        self._refresh(reset={'condition': condition,
                             'filter_flag': condition},
                      when=self.DOIT_IMMEDIATELY)

    def _filter_by_cell(self, cancel=False):
        row, col = self._current_cell()
        id = self._columns[col].id()
        sf_dialog = self._lf_sf_dialog('_lf_filter_dialog', FilterDialog)
        if sf_dialog.append_condition(id, self._table.row(row)[id]):
            self._on_filter(show_dialog=False)
        else:
            message(_("Podle tohoto sloupce nelze filtrovat."), beep_=True)

    def _resize_column(self, diff=5):
        # diff can be positive or negative integer in pixels.
        g = self._grid
        col = g.GetGridCursorCol()
        newsize = g.GetColSize(col) + diff
        if newsize > 0:
            g.SetColSize(col, newsize)
            g.SetSize(g.GetSize())
            g.Refresh()
            self._remember_column_size(col)

    def _move_column(self, diff=1):
        col = self._grid.GetGridCursorCol()
        newcol = col + diff
        if 0 <= newcol < len(self._columns):
            c = self._columns[col]
            self._update_grid(delete_column=c, insert_column=c,
                              inserted_column_index=newcol)
            self._select_cell(col=newcol)
        else:
            log(OPERATIONAL, "Invalid column move command:", (col, newcol))

    def can_move_column(self, diff=1):
        col = self._grid.GetGridCursorCol()
        return 0 <= col + diff < len(self._columns)
        
    def _on_sort_column(self, col=None, direction=None, primary=False):
        if not self._finish_editing():
            return
        if col is not None:
            col = self._columns[col].id()
            if not self._data.find_column(col):
                message(_("Podle tohoto sloupce nelze t��dit"),
                        beep_=True)
                return
        old_sorting = self._lf_sorting
        sorting = super_(ListForm)._on_sort_column(self, col=col,
                                                   direction=direction,
                                                   primary=primary)
        if sorting is not None and sorting != old_sorting:
            self._refresh(reset={'condition':self._lf_condition,
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
        """Callback, kter� lze zavolat p�i zm�n� dat v�datov�m zdroji.

        Metoda je ur�ena pro registraci pomoc� metody
        'pytis.data.Data.add_callback_on_change'.

        Metoda naopak nen� ur�ena pro ��dost o�okam�it� update, proto�e pouze
        zad� po�adavek na update, kter� je zpracov�n a� za bl�e neur�enou
        dobu.  K�p��m�m updat�m slou�� metody 'reset()' a 'refresh()'.

        """
        log(EVENT, 'Notifikace o�zm�n� dat ��dkov�ho seznamu')
        now = time.time()
        maybe_future = self._last_reshuffle_request + self._REFRESH_PERIOD
        self._reshuffle_request = max(now, maybe_future)
        self._show_data_status()

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
                if the_row is not None:
                    self._run_callback(self.CALL_SELECTION, (the_row,))
                    self._post_selection_hook(the_row)
        if self._default_columns_changed:
            self._default_columns_changed = False
            msg = _("Specifikace sloupc� formul��e byla zm�n�na.\n"
                    "Va�e u�ivatelsk� nastaven� sloupc� je ji� zastaral�.\n"
                    "Chcete pou��t nov� v�choz� nastaven� sloupc�?")
            if run_dialog(Question, msg):
                self._on_reset_columns()
            else:
                self._set_state_param('default_columns',self._default_columns())
        # V�budoucnu by zde mohlo b�t p�edna��t�n� dal��ch ��dk� nebo dat
        event.Skip()
        return False

    def _post_selection_hook(self, the_row):
        if focused_window() is self:
            # TODO: viz pozn�mka v _select_cell.
            self.show_position()
            # Zobraz hodnotu displeje z ��seln�ku ve stavov� ��dce.
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
                    except pytis.data.DataAccessException:
                        pass
            message(display_value)
    
    def _on_select_cell(self, event):
        if not self._in_select_cell and self._grid.GetBatchCount() == 0:
            # GetBatchCount zji��ujeme proto, aby nedhoch�zelo k vol�n�
            # callbacku p�i zm�n�ch v r�mci _update_grid(), kter� nejsou
            # interaktivn�.
            self._run_callback(self.CALL_USER_INTERACTION)
        if self._select_cell(row=max(0, event.GetRow()), col=event.GetCol()):
            # SetGridCursor vyvol� tento handler.  Aby SetGridCursor m�lo
            # v�bec n�jak� ��inek, mus�me zde zavolat origin�ln� handler, kter�
            # po�adovan� nastaven� bu�ky zajist�.
            event.Skip()
        else:
            event.Veto()
            self._grid.SelectRow(self._grid.GetGridCursorRow())

    def _on_activation(self, key, alternate=False):
        if alternate:
            f = DescriptiveDualForm
        else:
            f = BrowsableShowForm
        self._run_form(f, key)

    def _scroll_x_offset(self):
        g = self._grid
        return g.GetViewStart()[0] * g.GetScrollPixelsPerUnit()[0]
            
    def _on_label_right_down(self, event):
        self._run_callback(self.CALL_USER_INTERACTION)
        g = self._grid
        col = g.XToCol(event.GetX() + self._scroll_x_offset())
        # Menu mus�me zkonstruovat a� zde, proto�e argumentem p��kaz� je ��slo
        # sloupce, kter� zjist�m a� z eventu.
        items = (Menu(_("Prim�rn� �azen�"),
                      (MItem(_("�adit vzestupn�"),
                             command=LookupForm.COMMAND_SORT_COLUMN,
                             args=dict(direction=LookupForm.SORTING_ASCENDENT,
                                       col=col, primary=True)),
                       MItem(_("�adit sestupn�"),
                             command=LookupForm.COMMAND_SORT_COLUMN,
                             args=dict(direction=LookupForm.SORTING_DESCENDANT,
                                       col=col, primary=True)),)),
                 Menu(_("Dodate�n� �azen�"),
                      (MItem(_("�adit vzestupn�"),
                             command=LookupForm.COMMAND_SORT_COLUMN,
                             args=dict(direction=LookupForm.SORTING_ASCENDENT,
                                       col=col)),
                       MItem(_("�adit sestupn�"),
                             command=LookupForm.COMMAND_SORT_COLUMN,
                             args=dict(direction=LookupForm.SORTING_DESCENDANT,
                                       col=col)),)),
                 MSeparator(),
                 MItem(_("Ne�adit podle tohoto sloupce"),
                       command=LookupForm.COMMAND_SORT_COLUMN,
                       args=dict(direction=LookupForm.SORTING_NONE, col=col)),
                 MItem(_("Zru�it �azen� �pln�"),
                       command=LookupForm.COMMAND_SORT_COLUMN,
                       args=dict(direction=LookupForm.SORTING_NONE)),
                 MSeparator(),
                 MItem(_("Seskupit podle tohoto sloupce"),
                       command=ListForm.COMMAND_SET_GROUPING_COLUMN,
                       args=dict(column_id=self._columns[col].id())),
                 MItem(_("Zru�it seskupov�n�"),
                       command=ListForm.COMMAND_SET_GROUPING_COLUMN,
                       args=dict(column_id=None)),
                 MSeparator(),
                 MItem(_("Skr�t tento sloupec"),
                       command=ListForm.COMMAND_TOGGLE_COLUMN,
                       args=dict(column_id=self._columns[col].id())),
                 Menu(_("Zobrazen� sloupce"),
                      [CheckItem(c.label(),
                                 state=lambda a, c=c: c in self._columns,
                                 command=ListForm.COMMAND_TOGGLE_COLUMN,
                                 args=dict(column_id=c.id(), col=col))
                       for c in self._view.fields()] + \
                      [MSeparator(),
                       MItem(_("Vr�tit v�choz� sloupce"),
                             command=ListForm.COMMAND_RESET_COLUMNS)]),
                 )
        menu = Menu('', items).create(g, self)
        g.PopupMenu(menu)
        menu.Destroy()
        event.Skip()

    def _on_context_menu(self, event):
        # Popup menu pro vybran� ��dek gridu
        self._run_callback(self.CALL_USER_INTERACTION)
        row, col = event.GetRow(), event.GetCol()
        self._select_cell(row=row, col=col)
        self.show_context_menu(position=event.GetPosition())
        event.Skip()

    def _on_label_left_down(self, event):
        g = self._grid
        x = event.GetX() + self._scroll_x_offset()
        col = g.XToCol(x)
        x1 = reduce(lambda x, i: x + g.GetColSize(i), range(col), 0)
        x2 = x1 + g.GetColSize(col)
        if x > x1+2 and x < x2-2:
            self._column_to_move = col
        self._mouse_dragged = False
        event.Skip()
        
    def _on_label_left_up(self, event):
        col = self._grid.XToCol(event.GetX() + self._scroll_x_offset())
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
            self._run_callback(self.CALL_USER_INTERACTION)
            invoke_command(LookupForm.COMMAND_SORT_COLUMN, col=col,
                           direction=LookupForm.SORTING_CYCLE_DIRECTION)
        self._column_move_target = None
        self._column_to_move = None
        event.GetEventObject().Refresh()
        event.Skip()
        
    def _on_label_mouse_move(self, event):
        def nearest_column(x):
            g = self._grid
            n = g.GetNumberCols()
            pos = 0
            lastwidth = 0
            for col in range(n+1):
                if col <= n:
                    width = g.GetColSize(col)
                else:
                    width = 0
                if pos - lastwidth/2 <= x <= pos + width/2:
                    return col
                lastwidth = width
                pos += width
            return g.GetNumberCols()
        if self._column_to_move is not None:
            x = event.GetX() + self._scroll_x_offset()
            self._column_move_target = nearest_column(x)
            event.GetEventObject().Refresh()
        self._mouse_dragged = True
        event.Skip()

    def _on_label_drag_size(self, event):
        self._remember_column_size(event.GetRowOrCol())
        # Mohli bychom roz���it posledn� sloupec, ale jak ho potom zase z��it?
        #if config.stretch_tables:
        #    g = self._grid
        #    n = g.GetNumberCols()
        #    w = reduce(lambda x, i: x + g.GetColSize(i), range(n), 0)
        #    x = g.GetSize().x
        #    if w < x:
        #        col = n-1
        #        g.SetColSize(col, g.GetColSize(col) + (x - w))
        #        self._remember_column_size(col)
        event.Skip()

    def _remember_column_size(self, col):
        stored = self._get_state_param('column_width', {})
        stored[self._columns[col].id()] = self._grid.GetColSize(col)
        self._set_state_param('column_width', stored)
        
    def _on_label_paint(self, event):
        def triangle(x, y, r=4, reversed=True):
            # Return polygon coordinates for a triangle.
            if reversed:
                return ((x, y), (x+2*r, y), (x+r, y+r))
            else:
                return ((x+r, y), (x+2*r, y+r), (x, y+r))
        def arrow(x, y, r=5, l=4):
            # Return polygon coordinates for an arrow.
            return ((x, y), (x-r, y-r), (x-r/2, y-r), (x-r/2, y-r-l),
                    (x+r/2, y-r-l), (x+r/2, y-r), (x+r, y-r))
        g = self._grid
        #t = self._table
        dc = wx.PaintDC(g.GetGridColLabelWindow())
        x = - self._scroll_x_offset()
        y = 0
        height = g.GetColLabelSize()
        for col, c in enumerate(self._columns):
            id = c.id()
            width = g.GetColSize(col)
            if col == 0:
                d = 0
            else:
                d = 1
            dc.SetBrush(wx.Brush("GRAY", wx.TRANSPARENT))
            dc.SetTextForeground(wx.BLACK)
            # Draw the rectangle around.
            dc.DrawRectangle(x-d, y, width+d, height)
            # Draw the label itself.
            label = c.column_label()
            while dc.GetTextExtent(label)[0] > width and len(label):
                label = label[:-1] # Don't allow the label to extend the width.
            dc.DrawLabel(label, (x,y,width,height), wx.ALIGN_CENTER|wx.CENTER)
            # Draw the sorting sign.
            pos = position(id, self._lf_sorting, key=lambda x: x[0])
            if pos is not None:
                left = x+width-12
                top = y+3
                r = self._lf_sorting[pos][1] == LookupForm.SORTING_ASCENDENT
                dc.SetBrush(wx.Brush("CORAL", wx.SOLID))
                for i in range(pos):
                    dc.DrawLine(left, top+2*i, left+9, top+2*i)
                dc.DrawPolygon(triangle(left, top+pos*2, reversed=r))
            # Draw the grouping sign.
            if self._lf_grouping == id:
                dc.SetBrush(wx.Brush("CORAL", wx.SOLID))
                dc.DrawCircle(x+5, y+5, 2)
            # Indicate when the column is being moved.
            move_target = self._column_move_target
            if self._column_to_move is not None and move_target is not None:
                if col == move_target:
                    ax = x - d + (col == 0 and 5 or 0)
                elif col == move_target-1 and col == len(self._columns)-1:
                    ax = x + width - 5
                else:
                    ax = None
                if ax is not None:
                    dc.SetBrush(wx.Brush("GREEN", wx.SOLID))
                    dc.DrawPolygon(arrow(ax, height-2))
            x += width

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

    def _on_toggle_column(self, column_id, col=None):
        c = find(column_id, self._columns, key=lambda c: c.id())
        if c:
            if len(self._columns) == 1:
                message(_("Posledn� sloupec"), beep_=True)
                return
            self._update_grid(delete_column=c)
        else:
            self._update_grid(insert_column=self._view.field(column_id),
                              inserted_column_index=col)

    def _on_reset_columns(self):
        self._update_grid(reset_columns=True)
        
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

    def _show_data_status(self):
        if self._reshuffle_request > self._last_reshuffle_request:
            status = _("Data zm�n�na")
        else:
            status = _("Data ok")
        set_status('data-changed', status)
        
    def on_key_down(self, event, dont_skip=True):
        self._run_callback(self.CALL_USER_INTERACTION)
        if KeyHandler.on_key_down(self, event, dont_skip=dont_skip):
            return True
        def evil_key(event):
            # Tato v�c je tu kv�li eliminaci vstupu do editace pol��ka
            # libovolnou kl�vesou.  Nen� mi zn�m jin� zp�sob, jak t�to
            # eliminace dos�hnout.
            # Nelze pou��t hasModifiers ani test MetaDown kv�li NumLocku.
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
        # Univerz�ln� p��kazy
        if command.handler() is not None:
            return self._on_handled_command(command, **kwargs)
        elif command == ListForm.COMMAND_CONTEXT_MENU_ACTION:
            self._on_context_menu_action(**kwargs)
            return True
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
            self._refresh(reset={'grouping': kwargs['column_id']})
            return True
        elif command == ListForm.COMMAND_TOGGLE_COLUMN:
            self._on_toggle_column(**kwargs)
            return True
        elif command == ListForm.COMMAND_RESET_COLUMNS:
            self._on_reset_columns(**kwargs)
            return True
        elif command == ListForm.COMMAND_SELECT_CELL:
            self._select_cell(**kwargs)
            return True
        elif command == ListForm.COMMAND_RESIZE_COLUMN:
            self._resize_column(**kwargs)
            return True
        elif command == ListForm.COMMAND_MOVE_COLUMN:
            self._move_column(**kwargs)
            return True
        # P��kazy b�hem editace ��dku
        elif self._table.editing():
            if command == ListForm.COMMAND_LINE_COMMIT:
                return self._on_line_commit()
            elif command == ListForm.COMMAND_LINE_ROLLBACK:
                return self._on_line_rollback(**kwargs)
            elif command == ListForm.COMMAND_FINISH_EDITING:
                self._finish_editing()
                return True
            # P��kazy vztahuj�c� se pouze k�editaci pol��ka
            elif self._grid.IsCellEditControlEnabled():
                if command == ListForm.COMMAND_CELL_COMMIT:
                    return self._on_cell_commit()
                elif command == ListForm.COMMAND_CELL_ROLLBACK:
                    return self._on_cell_rollback()
                else:
                    field = self._current_editor.field()
                    if field.on_command(command, **kwargs):
                        return True
        # P��kazy mimo editaci
        else:
            if command == ListForm.COMMAND_FIRST_COLUMN:
                self._select_cell(col=0)
            elif command == ListForm.COMMAND_LAST_COLUMN:
                self._select_cell(col=len(self._columns)-1)
            elif command == ListForm.COMMAND_ACTIVATE:
                key = self._current_key()
                self._run_callback(self.CALL_ACTIVATION, (key,), kwargs)
            elif command == ListForm.COMMAND_SHOW_CELL_CODEBOOK:
                self._on_show_cell_codebook()
            elif command == LookupForm.COMMAND_FILTER:
                self._on_filter()
            elif command == ListForm.COMMAND_INCREMENTAL_SEARCH:
                self._on_incremental_search(**kwargs)
            elif command == ListForm.COMMAND_NEW_LINE:
                self._on_insert_line(**kwargs)
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

    # Metody volan� p��mo z�callbackov�ch metod
                                   
    def _on_show_cell_codebook(self):
        row, col = self._current_cell()
        column = self._columns[col]
        cb_name = column.codebook()
        if cb_name:
            the_row = self._table.row(row)
            v = the_row[column.id()]
            e = v.type().enumerator()
            run_form(BrowseForm, cb_name, select_row={e.value_column(): v})

    def can_show_cell_codebook(self):
        column = self._columns[self._current_cell()[1]]
        return column.codebook() is not None

    def _on_handled_command(self, command, norefresh=False, **kwargs):
        log(EVENT, 'Vyvol�v�m u�ivatelsk� handler p��kazu:', command)
        # TODO: P��kazy s handlerem by nem�ly b�t v�bec pou��v�ny.  Nam�sto
        # nich nech� je vyu��v�n p��kaz COMMAND_CONTEXT_MENU_ACTION,
        # kde se handler definuje jako sou��st argument� a ne jako argument
        # konstruktoru Command.  U�ivatel zkr�tka nem� co vytv��et vlastn�
        # instance t��dy Command...  Tato metoda a p��slu�n� ��st v on_command
        # jsou zde jen kv�li zp�tn� kompatibilit� a a� se v aplikac�ch
        # v�echy u�ivatelsk� p��kazy nahrad�, bude mo�n� ji zru�it.
        handler = command.handler()
        args, kwargs = self._context_menu_handler_args(handler, **kwargs)
        handler(*args, **kwargs)
        if not norefresh:
            self.refresh()
        return True

    def _context_menu_handler_args(self, handler, **kwargs):
        # Zjist�me, jak� m� u�ivatelsk� handler argumenty.
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
        elif posargs == 1:
            args = (the_row,)
        else:
            args = (self._data, the_row)
        if not varkw:
            if defaults is not None:
                kwnames = allargs[-len(defaults):]
                kwargs = dict([(k,v) for k,v in kwargs.items() if k in kwnames])
            else:
                kwargs = {}
        return args, kwargs
    
    def _on_context_menu_action(self, handler=None, enabled=None, **kwargs):
        log(EVENT, 'Vyvol�v�m u�ivatelsk� handler akce kontextov�ho menu.')
        if not callable(handler):
            raise ProgramError("Nep��pustn� handler akce konetxtov�ho menu:",
                               handler)
        args, kwargs = self._context_menu_handler_args(handler, **kwargs)
        handler(*args, **kwargs)
        #if not norefresh:
        self.refresh()
        return True
    
    def can_context_menu_action(self, handler=None, enabled=None, **kwargs):
        if enabled:
            args, kwargs = self._context_menu_handler_args(enabled, **kwargs)
            return enabled(*args, **kwargs)
        else:
            return True

    def _on_incremental_search(self, full=False):
        row, col = self._current_cell()
        column = self._columns[col]
        if not isinstance(column.type(self._data), pytis.data.String):
            message(_("V�tomto sloupci nelze vyhled�vat inkrement�ln�"),
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
        # TODO: wxClipboard nefunguje, jak m�, tak to vy�e��me
        #       hackem, kdy vyu�ijeme toho, �e wxTextCtrl.Copy()
        #       d�l� to, co m�.
        tc = wx.TextCtrl(self, -1, clptext)
        tc.SetSelection(0,len(clptext))
        tc.Copy()
        tc.Destroy()

    def _on_export_csv(self):
        log(EVENT, 'Vyvol�n� CSV exportu')
        data = self._data
        # Kontrola po�tu ��dk�
        number_rows = self._table.GetNumberRows()
        if number_rows == 0:
            msg = _("Tabulka neobsahuje ��dn� ��dky! Export nebude proveden.")
            run_dialog(Warning, msg)
            return
        # Seznam sloupc�
        column_list = []
        for column in self._columns:
            column_list.append((column.id(), column.type(data)))
        allowed = True
        # Kontrola pr�v        
        for cid, ctype in column_list:
            if not data.accessible(cid, pytis.data.Permission.EXPORT):
                allowed = False
                break
        if not allowed:
            msg = _("Nem�te pr�vo exportu k t�to tabulce.\n")
            msg = msg + _("Export nebude proveden.")
            run_dialog(Warning, msg)
            return            
        export_dir = config.export_directory
        export_encoding = config.export_encoding
        db_encoding = config.db_encoding
        try:
            u"test".encode(export_encoding)
        except:
            msg = _("K�dov�n� %s nen� podporov�no.\n" % export_encoding)
            msg = msg + _("Export se provede bez p�ek�dov�n�.")
            export_encoding = None
            run_dialog(Error, msg)
        try:
            u"test".encode(db_encoding)
        except:
            msg = _("K�dov�n� %s nen� podporov�no.\n" % db_encoding)
            msg = msg + _("Export se neprovede.")
            run_dialog(Error, msg)
            return
        filename = pytis.form.run_dialog(pytis.form.FileDialog,
                                       title="Zadat exportn� soubor",
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
            msg = _("Nepoda�ilo se otev��t soubor " + filename + \
                    " pro z�pis!\n")
            run_dialog(Error, msg)
            return
        def _process_table(update):
            # Export label�
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
            log(EVENT, 'Pokus o�editaci needitovateln� tabulky')
            return False
        table = self._table
        if not table.editing():
            if not self._lock_record(self._current_key()):
                return False
            table.edit_row(self._current_cell()[0])
            self._update_selection_colors()
        if not self._edit_cell():
            self._on_line_rollback()
        return True

    def _on_insert_line(self, before=False, copy=False):
        """Vlo� nov� ��dek do seznamu.

        Argumenty:

          before -- je-li pravda, nov� ��dek se vlo�� p�ed aktu�ln� ��dek, jinak
            se vlo�� za aktu�ln� ��dek
          copy -- je-li pravda a seznam nen� pr�zdn�, obsahem nov�ho ��dku bude
            obsah aktu�ln�ho ��dku, v�opa�n�m p��pad� bude nov� ��dek pr�zdn�

        Vlo�en� nov�ho ��dku do seznamu je mo�n� jen tehdy, pokud nen� zrovna
        ��dn� ��dek editov�n, a� u� nov� nebo st�vaj�c�.  P�i pokusu o vlo�en�
        nov�ho ��dku b�hem editace jin�ho ��dku je chov�n� metody nedefinov�no.

        Vlo�en� nov�ho ��dku m��e b�t tak� zak�z�no pro konkr�tn� formul�� v
        jeho specifikaci (viz argument 'enable_inline_insert' konstruktoru
        t��dy 'ViewSpec').
        
        Po vlo�en� nov�ho ��dku seznam automaticky p�ejde do re�imu editace
        tohoto ��dku a spust� editaci prvn� editovateln� bu�ky ��dku.

        """
        row = self._current_cell()[0]
        log(EVENT, 'Vlo�en� nov�ho ��dku:', (row, before, copy))
        if not self._data.accessible(None, pytis.data.Permission.INSERT):
            message('Nem�te p��stupov� pr�va pro vkl�d�n� z�znam� do t�to ' + \
                    'tabulky!', beep_=True)
            return False
        if not self.editable:
            message('Needitovateln� tabulka!', beep_=True)
            return False
        if not self._enable_inline_insert:
            message('Nen� mo�n� vkl�dat ��dky v in-line editaci. ' +
                    'Pou�ijte edita�n� formul��.', beep_=True)
            return False
        table = self._table
        if table.editing():
            log(EVENT, 'Pokus o�vlo�en� nov�ho ��dku b�hem editace')
            return False
        self._last_insert_copy = copy
        oldg = self._grid
        oldempty = (oldg.GetNumberRows() == 0)
        if not copy or oldempty:
            the_row = None
        else:
            the_row = table.row(row)
            # TODO: mo�n� p�jde vy�e�it �ist�ji
            # Jde o to vytvo�it kopii ��dku, ale kl�� nekop�rovat.
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
        if not before and not oldempty:
            row = row + 1
        if row == -1:
            row = 0
        self._update_grid(inserted_row_number=row, inserted_row=the_row)
        self._select_cell(row=row, col=0, invoke_callback=False)
        if not self._is_editable_cell(row, 0) \
               and not self._find_next_editable_cell():
            log(EVENT, '��dn� sloupec nen� editovateln�')
            return False
        self._edit_cell()
        self._update_selection_colors()
        log(EVENT, '��dek vlo�en')
        return True

    def _on_line_commit(self):
        # Zde z�le�� na n�vratov� hodnot�, proto�e ji vyu��v� _on_cell_commit.
        log(EVENT, 'Pokus o�ulo�en� ��dku seznamu do datab�ze')
        # Vyta�en� nov�ch dat
        table = self._table
        editing = table.editing()
        if not editing:
            return False
        row = editing.row
        the_row = editing.the_row
        # Ov��en� integrity z�znamu (funkce check).
        failed_id = self._check_record(the_row)
        if failed_id:
            col = find(failed_id, self._columns, key=lambda c: c.id())
            if col is not None:
                i = self._columns.index(col)
                self._select_cell(row=row, col=i, invoke_callback=False)
                self._edit_cell()
            return True
        # Ur�en� operace a kl��e
        rdata = self._record_data(the_row)
        kc = [c.id() for c in self._data.key()]
        if editing.new:
            if row > 0:
                after = table.row(row-1).row().columns(kc)
                before = None
            elif row < table.GetNumberRows() - 1:
                after = None
                before = table.row(row+1).row().columns(kc)
            else:
                after = before = None
            op = (self._data.insert, (rdata,), dict(after=after, before=before))
        else:
            key = editing.orig_content.row().columns(kc)
            op = (self._data.update, (key, rdata))
        # Proveden� operace
        success, result = db_operation(op)
        if success and result[1]:
            table.edit_row(None)
            self._unlock_record()
            message('��dek ulo�en do datab�ze', ACTION)
            self.refresh()
            self._run_callback(self.CALL_MODIFICATION)
            on_line_commit = self._view.on_line_commit()
            if on_line_commit is not None:
                on_line_commit(the_row)
            self.focus()
        elif success:
            log(EVENT, 'Zam�tnuto pro chybu kl��e')
            if editing.new:
                msg = _("��dek s�t�mto kl��em ji� existuje nebo zm�na "
                        "sousedn�ho ��dku")
            else:
                msg = _("��dek s�t�mto kl��em ji� existuje nebo p�vodn� "
                        "��dek ji� neexistuje")
            run_dialog(Warning, msg)
            return False
        else:
            log(EVENT, 'Chyba datab�zov� operace')
            return False
        return True

    def _on_line_rollback(self, soft=False):
        log(EVENT, 'Zru�en� editace ��dku')
        editing = self._table.editing()
        if not editing:
            return False
        if soft and editing.changed:
            return True
        self._unlock_record()
        row = editing.row
        if editing.new:
            self._update_grid()
        else:
            self._table.edit_row(None)
            self._update_selection_colors()
            # Tento SelectRow je zde nutn� pro vynucen� p�ekreslen� ��dku se
            # staronov�mi hodnotami.
            self._grid.SelectRow(row)
        self._select_cell(row=row, invoke_callback=False)
        self.refresh()
        return True

    def _on_cell_commit(self):
        row, col = self._current_cell()
        log(EVENT, 'Odesl�n� obsahu pol��ka gridu', (row, col))
        self._grid.DisableCellEditControl()
        editing = self._table.editing()
        if not editing:
            return True
        if editing.valid:
            if not self._find_next_editable_cell():
                if editing.new:
                    q = _("Ulo�it ��dek?")
                    if run_dialog(Question, q, True):
                        log(EVENT, 'Kladn� odpov�� na dotaz o�ulo�en� ��dku')
                        if self._on_line_commit():
                            # TODO: vol�n�m n�sleduj�c� metody v t�le t�to
                            # metody, kter� o�et�uje p��kaz, dojde k
                            # zablokov�n� zpracov�n� p��kaz� v r�mci jej�ho
                            # zpracov�n�.  Ne� bude n�sleduj�c� vol�n� op�t
                            # odkomentov�no, je t�eba zajistit neblokuj�c�
                            # zpracov�n� p��kaz�...
                            # self._on_insert_line(copy=self._last_insert_copy)
                            pass
                        return True
                    else:
                        log(EVENT, 'Z�porn� odpov�� na dotaz o�ulo�en� ��dku')
                self._grid.SetGridCursor(row, 0)
        if not editing.valid or editing.new:
            log(EVENT, 'N�vrat do editace pol��ka')
            self._edit_cell()
        return True
        
    def _on_cell_rollback(self):
        log(EVENT, 'Opu�t�n� pol��ka gridu beze zm�ny hodnoty')
        self._current_editor.Reset()
        self._grid.DisableCellEditControl()
        self._current_editor = None
        return True

    def _on_delete_record(self, key):
        if not self.editable:
            message('Needitovateln� tabulka!', beep_=True)
            return False
        def blocked_code():
            key = self._current_key()
            deleted = super(ListForm, self)._on_delete_record(key)
            self._table.edit_row(None)
            return deleted
        if block_refresh(blocked_code):
            r = self._current_cell()[0]
            n = self._table.GetNumberRows()
            if r < n - 1:
                self._select_cell(row=r+1)
            elif r > 0:
                self._select_cell(row=r-1)
            # Ud�l�me rad�ji refresh cel� aplikace, proto�e jinak se
            # nerefreshne horn� formul�� po vymaz�n� z�znamu ze sideformu.
            refresh()

    # Ve�ejn� metody

    def is_edited(self):
        """Vra� pravdu, pr�v� kdy� je List ve stavu ��dkov� editace."""
        return self._table.editing()

    def exit_check(self):
        """Prove� kontrolu ukon�en� editace ��dku p�ed opu�t�n�m seznamu.

        Metoda nic nevrac�, pouze sama provede, co je pot�eba.

        """
        editing = self._table.editing()
        if editing:
            log(EVENT, 'Pokus o�odchod z���dkov�ho formul��e b�hem editace')
            if editing.changed and  \
                   run_dialog(Question, _("Ulo�it zeditovan� ��dek?"), True):
                log(EVENT, 'Vy��d�no ulo�en�')
                self._on_line_commit()
            else:
                log(EVENT, 'Ulo�en� zam�tnuto')
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

    def _find_row_by_number(self, row_number):
        # Nutno p�edefinovat, proto�e metoda rodi�. t��dy n�m rozhod� kurzor.
        # Krom toho je toto rychlej��...
        return self._table.row(row_number).row()

    def _find_row_by_values(self, cols, values):
        # Nutno p�edefinovat, proto�e metoda rodi�. t��dy n�m rozhod� kurzor.
        cols = xtuple(cols)
        values = xtuple(values)
        assert len(cols) == len(values)
        condition = apply(pytis.data.AND, map(pytis.data.EQ, cols, values))
        data = self._data
        data.rewind()
        success, result = db_operation(lambda: data.search(condition))
        if not success:
            row = 0
        elif result == 0:
            row = 0
        else:
            row = result - 1
        prow = self._table.row(row)
        if prow:
            return prow.row()
        else:
            return None

    def select_row(self, position, quiet=False):
        # B�hem editace m��e `position' obsahovat nevyhledateln� data.
        if position is not None and self._table.editing():
            position = self._table.editing().row
        if isinstance(position, types.IntType):
            # Pro ��slo vol�me rovnou _select_cell a nezdr�ujeme se p�evodem na
            # row a zp�t, kter� prob�h� v rodi�ovsk� metod�...
            self._select_cell(row=position)
        else:
            super(ListForm, self).select_row(position, quiet=quiet)
    
    def _select_row(self, row):
        if row is None:
            row_number = -1
        else:
            row_number = self._get_row_number(row)
        self._select_cell(row=row_number)


    # Ve�ejn� metody
        
    def _refresh(self, reset=None, when=None):
        """Aktualizuj data seznamu z�datov�ho zdroje.

        P�ekresli cel� seznam v�okam�iku dan�m argumentem 'when' se zachov�n�m
        parametr� dle argumentu 'reset'.

        Argumenty:

          reset -- ur�uje, kter� parametry zobrazen� maj� b�t zachov�ny a kter�
            zm�n�ny.  Hodnotou je bu� 'None', nebo dictionary.  Je-li hodnotou
            'None', z�stane zachov�na filtrovac� podm�nka, t��d�n� i�vybran�
            ��dek (vzhledem k�jeho obsahu, ne po�ad�), je-li to mo�n�.  Je-li
            hodnotou pr�zdn� dictionary, jsou naopak v�echny tyto parametry
            resetov�ny na sv� po��te�n� hodnoty.  Jinak jsou resetov�ny pr�v�
            ty parametry, pro n� v�dictionary existuje kl�� (jeden z��et�zc�
            'sorting', 'grouping', 'condition', 'position' a 'filter_flag'),
            a�to na hodnotou z�dictionary pro dan� kl��.  Parametr
            'filter_flag' ud�v�, zda m� b�t zobrazena indikace filtru.

          when -- ur�uje, zda a kdy m� b�t aktualizace provedena, mus� to b�t
            jedna z�'DOIT_*' konstant t��dy.  Implicitn� hodnota je
            'DOIT_AFTEREDIT', je-li 'reset' 'None', 'DOIT_IMMEDIATELY' jinak.

        Vrac�: Pravdu, pr�v� kdy� byla aktualizace provedena.

        """
        assert when in (None,           # to je pouze intern� hodnota
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
        if __debug__: log(DEBUG, 'Po�adavek na refresh:', (reset, when))
        if when is self.DOIT_IFNEEDED:
            if self._reshuffle_request == self._last_reshuffle_request or \
                   self._reshuffle_request > time.time():
                if __debug__: log(DEBUG, 'Refresh nen� t�eba prov�d�t nyn�')
                return False
        if when is self.DOIT_IMMEDIATELY:
            QUESTION = _("Zru�it zm�ny z�znamu a aktualizovat seznam?")
            delay = not self._finish_editing(question=QUESTION)
        else:
            delay = (self._table.editing() is not None) # nechceme dr�et info
        if delay:
            if __debug__: log(DEBUG, 'Refresh odlo�en do ukon�en� editace')
            return False
        # Refresh nyn� bude skute�n� proveden
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
            self.select_row(key, quiet=True)
            # Pokud se nepoda�ilo nastavit pozici na p�edchoz� kl��,
            # pokus�me se nastavit pozici na p�edchoz� ��slo ��dku v gridu.
            if self._current_key() != key and \
                   row < self._table.GetNumberRows() and row >= 0:
                self._select_cell(row=row)
        else:
            self._select_cell(row=row)
        self._show_data_status()
        return True

    def status_fields(self):
        # TODO: zat�m je podoba statusbaru ur�ena specifikac�, ale bylo by
        # rozumn� to cel� p�ed�lat, aby se statusbar dynamicky m�nil podle
        # aktu�ln�ho formul��e (s vyu�it�m t�to metody).
        return (('list-position', 7),)

    def _update_colors(self):
        self._update_selection_colors()
        self._update_label_colors()
        if config.cell_highlight_color is not None:
            self._grid.SetCellHighlightColour(config.cell_highlight_color)
        if config.grid_line_color is not None:
            self._grid.SetGridLineColour(config.grid_line_color)

    def _total_width(self):
        total = 0
        for c in self._columns:
            total += self._column_width(c)
        return total

    def _resize_columns(self, size=None):
        g = self._grid
        if size is None:
            size = g.GetSize()
        width = size.width
        height = size.height
        if height < self._total_height():
            width = width - wx.SystemSettings.GetMetric(wx.SYS_VSCROLL_X) - 1
        total_width = self._total_width()
        if width > total_width:
            coef = float(width) / total_width
        else:
            coef = 1
        total = 0
        last = None
        # P�enastav ���ky sloupc�
        for i, c in enumerate(self._columns):
            w = self._column_width(c)
            if not c.fixed() and config.stretch_tables:
                w = int(w*coef)
                last = i
            g.SetColSize(i, w)
            total += w
        if coef != 1 and total != width and last is not None:
            g.SetColSize(last, g.GetColSize(last) + (width - total))

    # wx metody

    def _on_size(self, event):
        size = event.GetSize()
        if size.width != self._grid.GetSize().width:
            self._resize_columns(size)
        event.Skip()
            
    def Close(self):
        self._data.remove_callback_on_change(self.on_data_change)
        try:
            self._data.close()
        except pytis.data.DBException:
            pass
        # Mus�me ru�n� zru�it editory, jinak se do�k�me segmentation fault.
        for e in self._editors:
            if e:
                e.close()
        # Mus�me tabulce zru�it datov� objekt, proto�e jinak do n�j bude �ahat
        # i�po kompletn�m uzav�en� star�ho gridu (!!) a rozhod� n�m tak data
        # v�nov�m gridu.
        self._table.close()    
        return super_(ListForm).Close(self)

    def Show(self, show):
        if not show:
            self.exit_check()
        return super_(ListForm).Show(self, show)
    
    # Ostatn� ve�ejn� metody

    def focus(self):
        super_(ListForm).focus(self)
        self.show_position()
        self._show_data_status()
        self._update_selection_colors()
        self._grid.SetFocus()
        
    def defocus(self):
        super_(ListForm).defocus(self)
        self._update_selection_colors()



class CodebookForm(ListForm, PopupForm, KeyHandler):
    """Formul�� pro zobrazen� v�b�rov�ho seznamu (��seln�ku).

    V�b�rov� seznam zobrazuje ��dky dat, z�nich� u�ivatel n�kter� ��dek
    vybere.  U�ivatel krom� v�b�ru a listov�n� nem��e s���dky nijak
    manipulovat.

    Formul�� je zobrazen jako mod�ln� okno pomoc� metody 'run()', kter� skon��
    po v�b�ru polo�ky a vr�t� instanci PresentedRow pro vybran� ��dek.  Pokud
    byl formul�� ukon�en jinak ne� v�b�rem z�znamu, je vr�cena hodnota 'None'.

    """

    _DEFAULT_WINDOW_HEIGHT = 500

    def __init__(self, parent, *args, **kwargs):
        parent = self._popup_frame(parent)
        super_(CodebookForm).__init__(self, parent, *args, **kwargs)
        h = min(self._DEFAULT_WINDOW_HEIGHT, self._total_height()+50)
        self.SetSize((self._total_width()+30, h))
        wx_callback(wx.grid.EVT_GRID_CELL_LEFT_DCLICK, self._grid,
                    lambda e: self._on_activation())

    def _init_attributes(self, condition=None, begin_search=None, **kwargs):
        """Zpracuj kl��ov� argumenty konstruktoru a inicializuj atributy.

        Argumenty:

          condition -- podm�nka filtruj�c� ��dky ��seln�ku jako instance
            'pytis.data.Operator'.
          begin_search -- Pokud nen� None, bude po otev�en� formul��e
            automaticky nastartov�no inkrement�ln� vyhled�v�n�. Pokud
            je hodnota �et�zec, je ch�p�n jako identifik�tor
            sloupce, ve kter�m se m� prov�d�t vyhled�v�n�. Nen�-li ho
            hodnota �et�zec, nebo neodpov�d�-li ��dn�mu sloupci,
            je vyhled�v�n� prov�d�no automaticky nad sloupe�kem s
            prim�rn�m t��d�n�m.
            
        """
        try:
            self._cb_spec = self._resolver.get(self._name, 'cb_spec')
        except ResolverError:
            self._cb_spec = CodebookSpec()
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
                message(_("Nelze za��t inkrement�ln� vyhled�v�n�. "
                          "��seln�k neobsahuje ��dn� set��d�n� sloupec!"),
                        beep_=True)
            col = find(col_id, self._columns, key=lambda c:c.id())
            if col is not None:
                self._select_cell(row=0, col=self._columns.index(col))
                self._on_incremental_search(full=False)
            else:
                log(OPERATIONAL, "Invalid search column:", col_id)

    def _default_columns(self):
        return self._cb_spec.columns() \
               or super(CodebookForm, self)._default_columns()

    def _default_sorting(self):
        sorting = self._cb_spec.sorting()
        if sorting is not None:
            return sorting
        else:
            return super(CodebookForm, self)._default_sorting()
        
    def _context_menu(self):
        return (MItem(_("Vybrat"),
                      command = ListForm.COMMAND_ACTIVATE),
                )

    def on_command(self, command, **kwargs):
        if command == Application.COMMAND_LEAVE_FORM:
            self._leave_form()
            return True
        return super_(CodebookForm).on_command(self, command, **kwargs)

    def _on_activation(self, key=None, alternate=False):
        """Nastav n�vratovou hodnotu a ukon�i mod�ln� dialog."""
        self._result = self.current_row()
        self._parent.EndModal(1)
        return True

    
class BrowseForm(ListForm):
    """Formul�� pro prohl�en� dat s mo�nost� editace."""

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
        # Sestav specifikaci kontextov�ho menu
        menu = super_(BrowseForm)._context_menu(self) + (
            MItem(_("Editovat bu�ku"),
                  command=ListForm.COMMAND_EDIT),
            MItem(_("Filtrovat podle bu�ky"),
                  command=ListForm.COMMAND_FILTER_BY_CELL),
            MItem(_("Zkop�rovat obsah bu�ky"),
                  command=ListForm.COMMAND_COPY_CELL),
            MSeparator(),
            MItem(_("Editovat z�znam"),
                  command=BrowseForm.COMMAND_EDIT_RECORD),
            MItem(_("Smazat z�znam"),
                  command=RecordForm.COMMAND_DELETE_RECORD),
            MItem(_("N�hled"),
                  command=ListForm.COMMAND_ACTIVATE),
            MItem(_("N�hled v druh�m formul��i"),
                  command=ListForm.COMMAND_ACTIVATE, args=dict(alternate=True)),
            MItem(_("Zobrazit souvisej�c� ��seln�k"),
                  command=ListForm.COMMAND_SHOW_CELL_CODEBOOK),
            )
        custom_menu = self._view.popup_menu()
        if custom_menu:
            menu += (MSeparator(),) + custom_menu
        return menu

    def _on_import_interactive(self):
        if not self._data.accessible(None, pytis.data.Permission.INSERT):
            msg = _("Nem�te pr�va pro vkl�d�n� z�znam� do t�to tabulky.")
            message(msg, beep_=True)
            return False
        msg = _("Nejprve vyberte soubor obsahuj�c� importovan� data. "
                "Pot� budete moci zkontrolovat a potvrdit ka�d� z�znam.\n\n"
                "*Form�t vstupn�ho souboru:*\n\n"
                "Ka�d� ��dek obsahuje seznam hodnot odd�len�ch zvolen�m "
                "znakem, nebo skupinou znak� (vypl�te n�e). "
                "Tabel�tor zapi�te jako ='\\t'=.\n\n"
                "Prvn� ��dek obsahuje identifik�tory sloupc� a ur�uje tedy "
                "v�znam a po�ad� hodnot v n�sleduj�c�ch (datov�ch) ��dc�ch.\n\n"
                "Identifik�tory jednotliv�ch sloupc� jsou n�sleduj�c�:\n\n" + \
                "\n".join(["|*%s*|=%s=|" % \
                           (c.column_label(), c.id().replace('_', '!_'))
                           for c in self._columns]))
        separator = run_dialog(InputDialog, 
                               title=_("Hromadn� vkl�d�n� dat"),
                               report=msg, report_format=TextFormat.WIKI,
                               prompt="Odd�lova�", value='|')
        if not separator:
            if separator is not None:
                message(_("Nebyl zad�n odd�lova�."), beep_=True)
            return False
        separator.replace('\\t', '\t')
        while 1:
            filename = run_dialog(FileDialog)
            if filename is None:
                message(_("Nebyl zad�n soubor.  Proces ukon�en."), beep_=True)
                return False
            try:
                f = open(filename)
            except IOError, e:
                msg = _("Nebylo mo�no otev��t soubor '%s': %s")
                run_dialog(Error, msg % (filename, str(e)))
                continue
            break
        try:
            columns = []
            for key in f.readline().split(separator):
                col = find(key.strip(), self._fields, key=lambda c: c.id())
                if col:
                    columns.append(col)
                else:
                    msg = _("Chybn� identifik�tor sloupce: %s")
                    run_dialog(Error, msg % key.strip())
                    return False
            i = 0 # aktu�ln� ��slo ��dku dat (nepo��taje z�hlav�)
            n = 0 # po�et skute�n� vlo�en�ch z�znam�
            prefill = self.prefill()
            for line in f:
                i += 1
                values = line.rstrip('\r\n').split(separator)
                if len(values) != len(columns):
                    msg = _("Chyba na ��dku %d: "
                            "Po�et hodnot neodpov�d� po�tu sloupc�.\n"
                            "Chcete p�esto pokra�ovat (dal��m z�znamem)?")
                    if run_dialog(Question, msg % i+1,
                                  title=_("Chyba vstupn�ch dat"),
                                  icon=Question.ICON_ERROR):
                        continue
                    else:
                        break
                data = prefill and copy.copy(prefill) or {}
                for col, val in zip(columns, values):
                    type = col.type(self._data)
                    value, error = type.validate(val)
                    if error:
                        msg = _("Chybn� hodnota sloupce '%s' na ��dku %d: %s\n"
                                "Chcete p�esto z�znam vlo�it?")
                        msg = msg % (col.id(), i+1, error.message())
                        if not run_dialog(Question, msg,
                                          title=_("Chyba vstupn�ch dat"),
                                          icon=Question.ICON_ERROR):
                            break
                    data[col.id()] = value
                else:
                    result = new_record(self._name, prefill=data)
                    if result:
                        n += 1
                    else:
                        msg = _("Je�t� nebyly zpracov�ny v�echny ��dky "
                                "vstupn�ch dat.\n"
                                "Chcete pokra�ovat ve vkl�d�n�?")
                        if not run_dialog(Question, msg):
                            break
        finally:
            f.close()
        run_dialog(Message, _("%d/%d z�znam� bylo vlo�eno.") % (n, i),
                   title=_("Hromadn� vkl�d�n� dat dokon�eno"))

    def _on_print_(self, spec_path):
        log(EVENT, 'Vyvol�n� tiskov�ho formul��e')
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
        if command == Form.COMMAND_PRINT and \
               (not kwargs.has_key('form') or kwargs['form'] is self):
            self._on_print_(kwargs.get('print_spec_path'))
        elif command == BrowseForm.COMMAND_IMPORT_INTERACTIVE:
            self._on_import_interactive()
        else:
            return super_(BrowseForm).on_command(self, command, **kwargs)
        return True


class FilteredBrowseForm(BrowseForm):
    """Prohl�ec� formul�� s�filtrovan�m obsahem.

    Oproti oby�ejn�mu prohl�ec�mu formul��i zobrazuje filtrovan� formul��
    pouze ��dky spl�uj�c� podm�nku zadanou v�konstruktoru, p��padn�
    kombinovanou se zadan�m datov�m kl��em.

    """

    def __init__(self, *args, **kwargs):
        super_(FilteredBrowseForm).__init__(self, *args, **kwargs)
        self._init_filter()
    
    def _init_attributes(self, condition=None, **kwargs):
        """Zpracuj kl��ov� argumenty konstruktoru a inicializuj atributy.
        
        Argumenty:

          condition -- hodnotou je podm�nkov� v�raz pro datov� dotazy, tak jak
            je definovan� v�metod� 'pytis.data.Data.select()'.  Tento v�raz m��e
            b�t i�funkc� jednoho argumentu vracej�c� p��slu�n� podm�nkov�
            v�raz, p�edan�m argumentem je dictionary id sloupc� (strings) a
            jejich hodnot (instanc� t��dy 'pytis.data.Value').

        M�-li podm�nka podobu statick�ho oper�toru, je filtrov�n� provedeno
        ihned a nelze je zm�nit.  M�-li naopak podm�nka podobu funkce, jsou
        zobrazena v�echna data a� do prvn�ho zavol�n� metody `filter()';
        n�sledn�mi vol�n�mi t�to funkce pak lze v�b�r upravovat.

        """
        super_(FilteredBrowseForm)._init_attributes(self, **kwargs)
        assert callable(condition) or isinstance(condition, pytis.data.Operator)
        self._condition = condition

    def _init_filter(self):
        self.filter()

    def filter(self, data=None):
        """Filtruj data dle podm�nky z�konstruktoru a 'data'.

        Argumenty:

          data -- 'None' nebo dictionary, jeho� kl��e jsou ids sloupc�
            (strings) a hodnoty jim p��slu�n� hodnoty (instance t��dy
            'pytis.data.Value').  Jestli�e je dictionary pr�zdn�, budou
            odfiltrov�ny v�echny z�znamy; jestli�e je 'None', budou naopak
            v�echny z�znamy zobrazeny.  Tento voliteln� argument nen� nutno
            v�z�pisu vol�n� funkce kl��ovat.

        Je-li podm�nka zadan� v�konstruktoru statick�m oper�torem, metoda pouze
        zavol� 'refresh()' na obalen� list.  Je-li naopak podm�nka zadan�
        v�konstruktoru funkc�, je provedeno p�efiltrov�n� v�z�vislosti na
        argumentu 'data' -- je-li 'None', tak jsou zobrazena v�echna data bez
        omezen� v�b�ru, jinak jsou zobrazena data dle podm�nky vr�cen� vol�n�m
        podm�nkov� funkce.

        """
        #log(EVENT, 'Filtrace obsahu formul��e:', (self._name, data))
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
        self._refresh(reset={'condition': None})


class SideBrowseForm(FilteredBrowseForm):
    """Formul�� zobrazuj�c� podmno�nu ��dk� z�visl�ch na jin�m ��dku dat.

    Data tohoto formul��e jsou filtrov�na v z�vislosti na aktu�ln�m ��dku v
    jin�m formul��i.
    
    Filtrovac� podm�nka je vytv��ena automaticky jako ekvivalence hodnot
    vazebn�ch sloupc� v hlavn�m a vedlej��m formul��i.  Identifik�tory
    p��slu�n�ch sloupc� jsou d�ny argumenty `binding_column' a
    `sibling_binding_column'.

    """

    def _init_attributes(self, sibling_name, sibling_row,
                         sibling_binding_column, binding_column,
                         hide_binding_column, append_condition=None,
                         title=None, **kwargs):
        """Zpracuj kl��ov� argumenty konstruktoru a inicializuj atributy.
        
        Argumenty:

          title -- ???
          sibling_name -- jm�no specifikace hlavn�ho formul��e; �et�zec
          sibling_row -- funkce bez argument�, kter� vr�t� aktu�ln� datov�
            ��dek hlavn�ho formul��e.
          sibling_binding_column -- identifik�tor vazebn�ho sloupce v hlavn�m
            formul��i; �et�zec
          binding_column -- identifik�tor vazebn�ho sloupce ve vedlej��m
            formul��i; �et�zec
          hide_binding_column -- pravdiv� hodnota zp�sob�, �e vazebn� sloupec
            nebude zobrazen.
          append_condition -- None nebo funkce jednoho argumentu, kter�m je
            aktu�ln� ��dek hlavn�ho formul��e. V tomto p��pad� mus� funkce
            vr�tit instanci Operator, kter� se p�ipoj� k implicitn�
            podm�nce provazuj�c� vazebn� sloupce.

        """
        column_condition = lambda row: pytis.data.EQ(binding_column,
                                                   row[sibling_binding_column])
        if append_condition:
            condition = lambda row: pytis.data.AND(column_condition(row),
                                                   append_condition(row))
        else:
            condition = column_condition
        self._sibling_name = sibling_name
        self._sibling_row = sibling_row
        self._title = title
        self._hide_binding_column = hide_binding_column
        self._binding_column = binding_column
        super_(SideBrowseForm)._init_attributes(self, condition, **kwargs)

    def _default_columns(self):
        columns = super(SideBrowseForm, self)._default_columns()
        if self._hide_binding_column:
            return tuple([c for c in columns if c != self._binding_column])
        else:
            return columns
        
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
