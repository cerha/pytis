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
    CALL_MODIFICATION = 'CALL_MODIFICATION'
    """Konstanta callbacku modifikace řádku."""
    CALL_USER_INTERACTION = 'CALL_USER_INTERACTION'
    """Konstanta callbacku interakce uživatele."""

    _REFRESH_PERIOD = 60 # sekund
    _SELECTION_CALLBACK_DELAY = 3 # desítky milisekund
    _TITLE_FOREGROUND_COLOR = WxColor(210, 210, 210)
    
    _STATUS_FIELDS = ('list-position', 'data-changed')

    DESCR = _("řádkový formulář")

    def __init__(self, *args, **kwargs):
        super(ListForm, self).__init__(*args, **kwargs)
        # Nastav klávesové zkratky z kontextových menu.
        for action in self._view.actions(linear=True):
            if action.hotkey():
                self.define_key(action.hotkey(),
                                ListForm.COMMAND_CONTEXT_ACTION,
                                dict(action=action))
        # Závěrečné akce
        self._data.add_callback_on_change(self.on_data_change)
        wx_callback(wx.EVT_SIZE, self, self._on_size)
        self._select_cell(row=self._position)
        self.set_callback(ListForm.CALL_ACTIVATION, self._on_activation)

    def _init_attributes(self, columns=None, grouping=None, **kwargs):
        """Zpracuj klíčové argumenty konstruktoru a inicializuj atributy.

        Argumenty:

          columns -- pokud není None, bude formulář používat dané sloupce.
            Jinak je použit seznam sloupců daný specifikací.  Hodnotou je
            sekvence identifikátorů sloupců obsaženýh ve specifikaci.

          grouping -- id sloupce vizuálního seskupování nebo None.
            
          kwargs -- argumenty předané konstruktoru předka.

        """
        super(ListForm, self)._init_attributes(**kwargs)
        assert columns is None or is_sequence(columns)
        # Inicializace atributů závislých na uživatelském nastavení.
        self._init_columns(columns)
        self._init_grouping(grouping)
        self._init_column_widths()
        # Inicializace ostatních atributů.
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
        self._check_default_columns = not columns
        # Parametry zobrazení.
        self._initial_position = self._position = 0


    def _default_columns(self):
        return self._view.columns()

    def _init_columns(self, columns=None):
        if not columns:
            default = self._default_columns()
            columns = [id for id in self._get_state_param('columns', default,
                                                          types.TupleType)
                       if self._view.field(id)] or default
        self._columns = [self._view.field(id) for id in columns]
    
    def _init_grouping(self, grouping=None):
        if grouping is None:
            grouping = self._get_state_param('grouping', None, types.TupleType)
            if grouping and None in [self._view.field(cid) for cid in grouping]:
                grouping = None
        if grouping is None:
            grouping = self._view.grouping()
        self._grouping = grouping
            
    def _init_column_widths(self):
        widths = self._get_state_param('column_width', (), types.TupleType)
        try:
            #TODO: Should column widths be saved/restored in dialog units?
            self._column_widths = dict([(id, width) for id, width in widths
                                        if self._view.field(id) is not None \
                                        and isinstance(width, types.IntType)])
        except ValueError:
            self._column_widths = {}
    
    def _select_columns(self):
        return [c.id() for c in self._data.columns() 
                if not isinstance(c.type(), pytis.data.Big)]
    
    def _create_form_parts(self, sizer):
        if self.title() is not None:
            description = self._view.description()
            self._title_bar = self._create_title_bar(description=description)
            sizer.Add(self._title_bar, 0, wx.EXPAND|wx.FIXED_MINSIZE)
        else:
            self._title_bar = None
        self._create_grid()
        self._update_colors()
        sizer.Add(self._grid, 1, wx.EXPAND|wx.FIXED_MINSIZE)

    def _column_width(self, column):
        try:
            return self._column_widths[column.id()]
        except KeyError:
            width = max(column.column_width(), len(column.column_label()))
            return dlg2px(self._grid, 4*width + 8)

    def _create_grid(self):
        if __debug__: log(DEBUG, 'Vytváření nového gridu')
        # Vytvoř grid a tabulku
        self._grid = g = wx.grid.Grid(self, wx.NewId())
        # Inicializuj datový select
        row_count = self._init_select()
        self._table = table = \
          _grid.ListTable(self._parent, self._data, self._fields,
                          self._columns, row_count, sorting=self._lf_sorting,
                          grouping=self._grouping, prefill=self._prefill,
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
        labels = g.GetGridColLabelWindow()
        self._editors = []
        self._init_col_attr()
        # Event handlery
        wx_callback(wx.grid.EVT_GRID_SELECT_CELL,   g, self._on_select_cell)
        wx_callback(wx.grid.EVT_GRID_COL_SIZE,      g, self._on_label_drag_size)
        wx_callback(wx.grid.EVT_GRID_EDITOR_SHOWN,  g, self._on_editor_shown)
        wx_callback(wx.grid.EVT_GRID_CELL_RIGHT_CLICK, g, self._on_right_click)
        wx_callback(wx.EVT_MOUSEWHEEL, g,      self._on_wheel)
        wx_callback(wx.EVT_IDLE,       g,      self._on_idle)
        wx_callback(wx.EVT_KEY_DOWN,   g,      self.on_key_down)
        wx_callback(wx.EVT_LEFT_DOWN,  labels, self._on_label_left_down)
        wx_callback(wx.EVT_LEFT_UP,    labels, self._on_label_left_up)
        wx_callback(wx.EVT_RIGHT_DOWN, labels, self._on_label_right_down)
        wx_callback(wx.EVT_MOTION,     labels, self._on_label_mouse_move)
        wx_callback(wx.EVT_PAINT,      labels, self._on_label_paint)
        if __debug__: log(DEBUG, 'Nový grid vytvořen')

    def _on_editor_shown(self, event):
        if self._table.editing():
            event.Skip()
        else:
            event.Veto()
            self._select_cell(row=max(0, event.GetRow()), col=event.GetCol())
    
    def _update_grid(self, data_init=False, inserted_row_number=None,
                     inserted_row=None, delete_column=None, insert_column=None,
                     inserted_column_index=None, init_columns=False):
        g = self._grid
        t = self._table
        def notify(id, *args):
            if id == wx.grid.GRIDTABLE_NOTIFY_COLS_DELETED:
                self._close_editors()
            msg = wx.grid.GridTableMessage(t, id, *args)
            g.ProcessTableMessage(msg)
        current_row = self._table.current_row()
        old_columns = tuple([c.id() for c in self._columns])
        # Uprav velikost gridu
        g.BeginBatch()
        if init_columns:
            deleted = len(self._columns)
            self._init_columns()
            inserted = len(self._columns)
            notify(wx.grid.GRIDTABLE_NOTIFY_COLS_DELETED, 0, deleted)
            notify(wx.grid.GRIDTABLE_NOTIFY_COLS_INSERTED, 0, inserted)
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
        new_columns = tuple([c.id() for c in self._columns])
        if new_columns != old_columns and not init_columns:
            default_columns = self._default_columns()
            if new_columns == default_columns:
                self._unset_state_param('columns')
                self._unset_state_param('default_columns')
            else:
                self._set_state_param('columns', new_columns)
                self._set_state_param('default_columns', default_columns)
        if data_init:
            row_count = self._init_select()
        else:
            row_count = self._lf_select_count
            self._data.rewind()
        if inserted_row_number is not None:
            row_count = row_count + 1
        old_row_count = self._table.GetNumberRows()
        new_row_count = row_count
        t.update(columns=self._columns,
                 row_count=row_count, sorting=self._lf_sorting,
                 grouping=self._grouping,
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
        if new_columns != old_columns or init_columns:
            self._init_col_attr()
        g.EndBatch()
        # Závěrečné úpravy
        self._update_colors()
        self._resize_columns()
        if new_row_count != old_row_count or new_columns != old_columns \
               or init_columns:
            # This is a workaround of a wxWidgets bug.  The scrollbars are not
            # shown or hidden properly, until a size event is received by the
            # grid.  Thus we generate one artificially...
            g.SetSize(g.GetSize())
        # Tento _select_cell() zde nemůže být, protože vyvolá ukončení editace
        # při vkládání řádku.  Pokud to je někdy potřeba, bude nutné volat
        # _select_cell() zvlášť po _update_grid().  Zatím to ale spíš vypadá,
        # že je to tady zbytečně (kostlivec).  TC 2005-12-28
        #self._select_cell(row=self._position)

    def _init_col_attr(self):
        # (Re)inicializuj atributy sloupců gridu.
        def registration(editor):
            self._current_editor = editor
        self.editable = False
        if self._editors:
            self._close_editors()
        for i, c in enumerate(self._columns):
            # zarovnání
            attr = wx.grid.GridCellAttr()
            if isinstance(c.type(self._data), pytis.data.Number):
                alignment = wx.ALIGN_RIGHT
            else:
                alignment = wx.ALIGN_LEFT
            attr.SetAlignment(alignment, wx.CENTER)
            # editor
            if c.editable() in (Editable.ALWAYS, Editable.ONCE) \
                   or isinstance(c.editable(), Computer):
                self.editable = True
                e = _grid.InputFieldCellEditor(self._parent, self._table, self,
                                               c, self._data, registration)
                # TODO:
                #e.set_callback(InputField.CALL_FIELD_CHANGE,
                #               ...)
                self._editors.append(e)
                attr.SetEditor(e)
            else:
                attr.SetReadOnly()
            self._grid.SetColAttr(i, attr)
        
    def _update_label_colors(self):
        color = self._lf_filter is not None and config.filter_color or \
                self._TITLE_FOREGROUND_COLOR
        self._grid.SetLabelBackgroundColour(color)

    def _context_menu(self):
        """Vrať specifikaci \"kontextového\" popup menu vybrané buňky seznamu.

        Vrací: Sekvenci instancí 'MItem'.

        Tuto metodu nechť odvozené třídy předefinují, pokud chtějí zobrazovat
        kontextové menu.
        
        """
        return ()

    def _edit_menu(self):
        return (
            MItem(_("Editovat buňku"),
                  command = ListForm.COMMAND_EDIT,
                  help=_("Otevřít vstupní políčko pro tuto hodnotu.")),
            MItem(_("Uložit záznam"),
                  command = ListForm.COMMAND_LINE_COMMIT,
                  help=_("Ukončit editaci s uložením záznamu.")),
            MItem(_("Opustit editaci"),
                  command = ListForm.COMMAND_FINISH_EDITING,
                  help=_("Ukončit editaci bez uložení záznamu.")),
            MSeparator(),
            MItem(_("Kopírovat obsah buňky"),
                  command = ListForm.COMMAND_COPY_CELL,
                  help=_("Zkopírovat hodnotu do schránky.")),
            #MItem("", command = ListForm.COMMAND_LINE_ROLLBACK),
            )

    def _lf_sfs_columns(self):
        shown = tuple(self._columns)
        hidden = tuple([c for c in self._fields
                        if c not in shown and c.column_label()])
        def labelfunc(c):
            label = c.column_label()
            if c in hidden:
                return "(" + label + ")"
            else:
                return label
        return sfs_columns(shown + hidden, self._data, labelfunc=labelfunc)
    
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

    def _selected_rows(self):
        g = self._grid
        # g.SelectedRows() nefunguje, proto následující hrůza...
        rows = []
        if g.IsInSelection(*self._current_cell()):
            rows.append(g.GetGridCursorRow())
        for start, end in zip([r for r, c in g.GetSelectionBlockTopLeft()],
                              [r for r, c in g.GetSelectionBlockBottomRight()]):
            rows.extend([r for r in range(start, end+1) if r not in rows])
        rows.sort()
        return rows

    def selected_rows(self):
        return _grid.TableRowIterator(self._table, self._selected_rows())
    
    def _select_cell(self, row=None, col=None, invoke_callback=True):
        # Vrací pravdu, pokud může být událost provedena (viz _on_select_cell).
        if self._in_select_cell:
            return True
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
                            # Nevoláme callback ihned, stačí až po
                            # zastavení scrolování...
                            self._selection_callback_candidate = row
                            delay = self._SELECTION_CALLBACK_DELAY
                            self._selection_callback_tick = delay
                    self._position = row
                    # TODO: tady to způsobuje špatné zobrazování pozice v
                    #       dualform. Nahrazeno voláním _show_position v
                    #       _post_selection_hook.
                    #       Jiné řešení?
                    # self._show_position()
            elif col is not None and col != current_col:
                g.SetGridCursor(current_row, col)
                g.MakeCellVisible(current_row, col)
            if __debug__: log(DEBUG, 'Výběr buňky proveden:', (row, col))
            return True
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
        log(EVENT, 'Spuštěn editor políčka:', (row, cid))
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
            if result == bcancel:
                log(EVENT, 'Odchod uživatelem povolen')
                self._on_line_rollback()
                finish = True
            elif result == bsave:
                log(EVENT, 'Odchod s uložením řádku')
                finish = self._on_line_commit()
            elif result is None or result == bcontinue:
                log(EVENT, 'Odchod uživatelem zamítnut')
                finish = False
            else:
                raise ProgramError('Unexpected dialog result', result)
        return finish

    def _on_line_commit(self):
        # Zde záleží na návratové hodnotě, protože ji využívá _cmd_cell_commit.
        log(EVENT, 'Pokus o uložení řádku seznamu do databáze')
        # Vytažení nových dat
        table = self._table
        editing = table.editing()
        if not editing:
            return False
        row = editing.row
        the_row = editing.the_row
        # Ověření integrity záznamu (funkce check).
        failed_id = self._check_record(the_row)
        if failed_id:
            col = find(failed_id, self._columns, key=lambda c: c.id())
            if col is not None:
                i = self._columns.index(col)
                self._select_cell(row=row, col=i, invoke_callback=False)
                self._edit_cell()
            return True
        # Určení operace a klíče
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
            op = (self._data.insert,
                  (rdata,),
                  dict(after=after, before=before,
                       transaction=self._transaction))
        else:
            key = editing.orig_content.row().columns(kc)
            op = (self._data.update,
                  (key, rdata,),
                  dict(transaction=self._transaction))
        # Provedení operace
        success, result = db_operation(op)
        if self._transaction is not None:
            self._transaction.commit()
            self._transaction = None
        if success and result[1]:
            table.edit_row(None)
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
                msg = _("Řádek s tímto klíčem již existuje nebo změna "
                        "sousedního řádku")
            else:
                msg = _("Řádek s tímto klíčem již existuje nebo původní "
                        "řádek již neexistuje")
            run_dialog(Warning, msg)
            return False
        else:
            log(EVENT, 'Chyba databázové operace')
            return False
        return True

    def _on_line_rollback(self, soft=False):
        log(EVENT, 'Zrušení editace řádku')
        if self._transaction is not None:
            self._transaction.rollback()
            self._transaction = None
        editing = self._table.editing()
        if not editing:
            return False
        if soft and editing.changed:
            return True
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
        editing = self._table.editing()
        if row == editing.row:
            the_row = editing.the_row
        else:
            the_row = self._table.row(row)
        id = self._columns[col].id()
        return the_row.editable(id)
    
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

    def _filter_refresh(self):
        self._refresh(when=self.DOIT_IMMEDIATELY)

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
                    self._run_callback(self.CALL_SELECTION, the_row)
                    self._post_selection_hook(the_row)
        if self._check_default_columns:
            self._check_default_columns = False
            columns = self._default_columns()
            if columns != self._get_state_param('default_columns', columns):
                msg = _("Specifikace sloupců formuláře byla změněna.\n"
                        "Vaše uživatelské nastavení sloupců je již zastaralé.\n"
                        "Chcete použít nové výchozí nastavení sloupců?")
                if run_dialog(Question, msg):
                    self.COMMAND_RESET_FORM_STATE.invoke()
                else:
                    self._set_state_param('default_columns', columns)
        # V budoucnu by zde mohlo být přednačítání dalších řádků nebo dat
        event.Skip()
        return False

    def _post_selection_hook(self, the_row):
        if focused_window() is self:
            # TODO: viz poznámka v _select_cell.
            self._show_position()
            # Zobraz hodnotu displeje z číselníku ve stavové řádce.
            row, col = self._current_cell()
            message(self._table.row(row).display(self._columns[col].id()))
    
    def _on_select_cell(self, event):
        if not self._in_select_cell and self._grid.GetBatchCount() == 0:
            # GetBatchCount zjišťujeme proto, aby nedhocházelo k volání
            # callbacku při změnách v rámci _update_grid(), které nejsou
            # interaktivní.
            self._run_callback(self.CALL_USER_INTERACTION)
        if self._select_cell(row=max(0, event.GetRow()), col=event.GetCol()):
            # SetGridCursor vyvolá tento handler.  Aby SetGridCursor mělo
            # vůbec nějaký účinek, musíme zde zavolat originální handler, který
            # požadované nastavení buňky zajistí.
            event.Skip()
        else:
            event.Veto()
            self._grid.SelectRow(self._grid.GetGridCursorRow())

    def _on_activation(self, alternate=False):
        if alternate:
            f = DescriptiveDualForm
        else:
            f = BrowsableShowForm
        kwargs = self._new_form_kwargs()
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
        return [CheckItem(c.column_label(),
                          command=ListForm.COMMAND_TOGGLE_COLUMN,
                          args=dict(column_id=c.id(), col=col),
                          state=lambda c=c: c in self._columns)
                for c in columns if c and not c.disable_column()] + \
                [MSeparator(),
                 MItem(_("Vrátit výchozí nastavení formuláře"),
                       command=InnerForm.COMMAND_RESET_FORM_STATE),
                 ]
    
    def _column_context_menu(self, col):
        M = Menu
        I = MItem
        ________ = MSeparator()
        ASC = LookupForm.SORTING_ASCENDENT
        DESC = LookupForm.SORTING_DESCENDANT
        c = self._columns[col]
        items = (M(_("Primární řazení"),
                   (I(_("Řadit vzestupně"),
                      command=LookupForm.COMMAND_SORT,
                      args=dict(direction=ASC, col=col, primary=True)),
                    I(_("Řadit sestupně"),
                      command=LookupForm.COMMAND_SORT,
                      args=dict(direction=DESC, col=col, primary=True)),)),
                 M(_("Dodatečné řazení"),
                   (I(_("Řadit vzestupně"),
                      command=LookupForm.COMMAND_SORT,
                      args=dict(direction=ASC, col=col)),
                    I(_("Řadit sestupně"),
                      command=LookupForm.COMMAND_SORT,
                      args=dict(direction=DESC, col=col)),)),
                 ________,
                 I(_("Neřadit podle tohoto sloupce"),
                   command=LookupForm.COMMAND_SORT,
                   args=dict(direction=LookupForm.SORTING_NONE, col=col)),
                 I(_("Zrušit řazení úplně"),
                   command=LookupForm.COMMAND_SORT,
                   args=dict(direction=LookupForm.SORTING_NONE)),
                 ________,
                 I(_("Seskupovat až po tento sloupec"),
                   command=ListForm.COMMAND_SET_GROUPING_COLUMN,
                   args=dict(col=col)),
                 I(_("Zrušit vizuální seskupování"),
                   command=ListForm.COMMAND_SET_GROUPING_COLUMN,
                   args=dict(col=None)),
                 ________,
                 I(_("Autofiltr"), command=ListForm.COMMAND_AUTOFILTER,
                   args=dict(col=col)),
                 I(_("Zruš filtr"), command=LookupForm.COMMAND_UNFILTER),
                 ________,
                 I(_("Skrýt tento sloupec"),
                   command=ListForm.COMMAND_TOGGLE_COLUMN,
                   args=dict(column_id=c.id(), col=None)),
                 M(_("Zobrazené sloupce"),
                   self._displayed_columns_menu(col=col))
                 )
        return items
    
    def _on_label_right_down(self, event):
        self._run_callback(self.CALL_USER_INTERACTION)
        col = self._grid.XToCol(event.GetX() + self._scroll_x_offset())
        # Menu musíme zkonstruovat až zde, protože je pro každý sloupec jiné.
        if col == -1:
            menu = self._displayed_columns_menu(len(self._columns))
        else:
            menu = self._column_context_menu(col)
        self._popup_menu(menu)
        event.Skip()

    def _on_label_left_down(self, event):
        g = self._grid
        x = event.GetX() + self._scroll_x_offset()
        col = g.XToCol(x)
        if col != -1:
            x1 = reduce(lambda x, i: x + g.GetColSize(i), range(col), 0)
            x2 = x1 + g.GetColSize(col)
            if x > x1+2 and x < x2-2:
                self._column_to_move = col
        self._mouse_dragged = False
        # We don't call event.Skip() since we want to suppress default bahavior
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
                    dir = self._sorting_direction(cid) \
                          or self.SORTING_NONE
                    cycle = [self.SORTING_ASCENDENT,
                             self.SORTING_DESCENDANT,
                             self.SORTING_NONE]
                    direction = cycle[(cycle.index(dir)+1)%3]
                LookupForm.COMMAND_SORT.invoke(col=col, primary=primary,
                                               direction=direction)
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
        self._grid.SetSize(self._grid.GetSize())
        # Mohli bychom rozšířit poslední sloupec, ale jak ho potom zase zúžit?
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
        id = self._columns[col].id()
        self._column_widths[id] = self._grid.GetColSize(col)
        saved_value = tuple(self._column_widths.items())
        self._set_state_param('column_width', saved_value)
        
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
        def funnel(x, y, r=3, l=8):
            # Return polygon coordinates for a funnel.
            return ((x, y), (x+r, y+r), (x+r, y+l), (x+r+2, y+l),
                    (x+r+2, y+r), (x+r*2+2, y), (x, y))
        g = self._grid
        #t = self._table
        dc = wx.PaintDC(g.GetGridColLabelWindow())
        x = - self._scroll_x_offset()
        y = 0
        height = g.GetColLabelSize()
        filtered_columns = self._filtered_columns()
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
            pos = self._sorting_position(id)
            if pos is not None:
                left = x+width-12
                top = y+3
                r = self._sorting_direction(id) == LookupForm.SORTING_ASCENDENT
                if id in self._grouping:
                    color = 'GREEN'
                else:
                    color = 'CORAL'
                dc.SetBrush(wx.Brush(color, wx.SOLID))
                for i in range(pos):
                    dc.DrawLine(left, top+2*i, left+9, top+2*i)
                dc.DrawPolygon(triangle(left, top+pos*2, reversed=r))
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
            # Draw the filter sign.
            if id in filtered_columns:
                dc.SetBrush(wx.Brush('GOLD', wx.SOLID))
                dc.DrawPolygon(funnel(x+2, y+3))
            x += width

    def _on_form_state_change(self):
        super(ListForm, self)._on_form_state_change()
        self._init_grouping()
        self._init_column_widths() 
        self._update_grid(init_columns=True)
        
    def _on_right_click(self, event):
        self._run_callback(self.CALL_USER_INTERACTION)
        selected = len(self._selected_rows())
        if selected == 1 and self._grid.IsInSelection(*self._current_cell()) \
               or selected == 0:
            row, col = event.GetRow(), event.GetCol()
            self._select_cell(row, col)
            self.COMMAND_CONTEXT_MENU.invoke(position=event.GetPosition())
        event.Skip()
            
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
        total = self._table.GetNumberRows()
        set_status('list-position', "%d/%d" % (row + 1, total))

    def _show_data_status(self):
        if self._reshuffle_request > self._last_reshuffle_request:
            status = _("Data změněna")
        else:
            status = _("Data ok")
        set_status('data-changed', status)
        
    def _is_changed(self):
        editing = self._table.editing()
        return editing and editing.changed

    def _dualform(self):
        # Pokud je formulář součástí duálního formuláře, vrať jej, jinak None.
        top = top_window()
        if isinstance(top, DualForm) and \
               self in (top.active_form(), top.inactive_form()):
            return top
        else:
            return None
    
    def _context_action_args(self, action):
        if action.context() == ActionContext.CURRENT_ROW:
            args = (self.current_row(),)
        elif action.context() == ActionContext.SELECTION:
            args = (self.selected_rows(),)
        else:
            raise ProgramError("Invalid action context:", action.context())
        if action.secondary_context() is not None:
            args += (self._secondary_context(action.secondary_context()),)
        return args
    
    def _secondary_context(self, context):
        dual = self._dualform()
        if dual:
            if dual.active_form() is self:
                form = dual.inactive_form()
            else:
                form = dual.active_form()
            if context == ActionContext.CURRENT_ROW:
                return form.current_row()
            elif context == ActionContext.SELECTION:
                return form.selected_rows()
            else:
                raise ProgramError("Invalid action secondary_context:", context)
        else:
            return None
    
    def _on_delete_record(self):
        if not self.editable:
            message('Needitovatelná tabulka!', beep_=True)
            return False
        def blocked_code():
            deleted = super(ListForm, self)._on_delete_record()
            self._table.edit_row(None)
            return deleted
        if block_refresh(blocked_code):
            r = self._current_cell()[0]
            n = self._table.GetNumberRows()
            if r < n - 1:
                self._select_cell(row=r+1)
            elif r > 0:
                self._select_cell(row=r-1)
            # Uděláme raději refresh celé aplikace, protože jinak se
            # nerefreshne horní formulář po vymazání záznamu ze sideformu.
            refresh()
            
    def _exit_check(self):
        # Opuštění formuláře je umožněno vždy, ale před opuštěním během editace
        # je nutné provést dodatečné akce.
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
        return True

    def _find_row_by_number(self, row_number):
        # Nutno předefinovat, protože metoda rodič. třídy nám rozhodí kurzor.
        # Krom toho je toto rychlejší...
        if row_number < self._table.GetNumberRows():
            return self._table.row(row_number).row()
        else:
            return None

    def _find_row_by_values(self, cols, values):
        # Nutno předefinovat, protože metoda rodič. třídy nám rozhodí kurzor.
        cols = xtuple(cols)
        values = xtuple(values)
        assert len(cols) == len(values)
        cond = apply(pytis.data.AND, map(pytis.data.EQ, cols, values))
        condition = pytis.data.AND(cond, self._current_condition())
        data = self._data
        data.rewind()
        def dbop():
            return data.search(condition)
        success, result = db_operation(dbop)
        if not success:
            row = -1
        elif result == 0:
            row = -1
        else:
            row = result - 1
        prow = self._table.row(row)
        if prow:
            return prow.row()
        else:
            return None
        
    def select_row(self, position, quiet=False):
        # Během editace může `position' obsahovat nevyhledatelná data.
        if position is not None and self._table.editing():
            position = self._table.editing().row
        if isinstance(position, types.IntType) \
               and position < self._table.GetNumberRows():
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
                    run_dialog(Warning, _("Záznam nenalezen"))
                return False
        else:
            row_number = -1
        self._select_cell(row=row_number)
        return True

    def _refresh(self, reset=None, when=None):
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
            'sorting', 'filter' a 'position'), a to na hodnotou z dictionary
            pro daný klíč.

          when -- určuje, zda a kdy má být aktualizace provedena, musí to být
            jedna z 'DOIT_*' konstant třídy.  Implicitní hodnota je
            'DOIT_AFTEREDIT', je-li 'reset' 'None', 'DOIT_IMMEDIATELY' jinak.

        Vrací: Pravdu, právě když byla aktualizace provedena.

        """
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
                     'filter': None,
                     'position': self._initial_position}
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
            if k == 'filter':
                self._lf_filter = v
            elif k == 'sorting':
                self._lf_sorting = v
            elif k == 'position':
                self._position = v
            else:
                raise ProgramError('Invalid refresh parameter', k)
        key = self._current_key()
        row = max(0, self._current_cell()[0])
        self._last_reshuffle_request = self._reshuffle_request = time.time()
        self._update_grid(data_init=True)
        if key is not None:
            self.select_row(key, quiet=True)
            # Pokud se nepodařilo nastavit pozici na předchozí klíč,
            # pokusíme se nastavit pozici na předchozí číslo řádku v gridu.
            if self._current_key() != key:
                if row < self._table.GetNumberRows() and row >= 0:
                    self._select_cell(row=row)
                else:
                    self._select_cell(row=0)
        else:
            self._select_cell(row=row)
        self._show_data_status()
        return True

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

    def size(self):
        return wx.Size(self._total_width(), self._total_height())

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
            coef = float(width) / total_width
        else:
            coef = 1
        total = 0
        last = None
        # Přenastav šířky sloupců
        for i, c in enumerate(self._columns):
            w = self._column_width(c)
            if not c.fixed() and config.stretch_tables:
                w = int(w*coef)
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
        self._data.remove_callback_on_change(self.on_data_change)
        try:
            self._data.close()
        except pytis.data.DBException:
            pass
        # Musíme ručně zrušit editory, jinak se dočkáme segmentation fault.
        self._close_editors()
        # Musíme tabulce zrušit datový objekt, protože jinak do něj bude šahat
        # i po kompletním uzavření starého gridu (!!) a rozhodí nám tak data
        # v novém gridu.
        self._table.close()

    # Zpracování příkazů
    
    def can_command(self, command, **kwargs):
        # Příkazy platné i během editace, pokud není aktivní editor.
        UNIVERSAL_COMMANDS = (ListForm.COMMAND_COPY_CELL,
                              ListForm.COMMAND_RESIZE_COLUMN,
                              ListForm.COMMAND_EDIT,
                              ListForm.COMMAND_FIRST_COLUMN,
                              ListForm.COMMAND_LAST_COLUMN)
        # Příkazy platné pouze během editace řádku.
        EDIT_COMMANDS = (ListForm.COMMAND_LINE_COMMIT,
                         ListForm.COMMAND_LINE_ROLLBACK,
                         ListForm.COMMAND_FINISH_EDITING,
                         ListForm.COMMAND_CELL_COMMIT,
                         ListForm.COMMAND_CELL_ROLLBACK)
        if self._table.editing():
            allowed = EDIT_COMMANDS
            if not self._grid.IsCellEditControlEnabled():
                allowed += UNIVERSAL_COMMANDS
            if command not in allowed:
                return False
        elif command in EDIT_COMMANDS:
            return False
        return super(ListForm, self).can_command(command, **kwargs)
    
    def _cmd_activate(self, alternate=False):
        self._run_callback(self.CALL_ACTIVATION, alternate=alternate)
            
    def _cmd_first_column(self):
        self._select_cell(col=0)
            
    def _cmd_last_column(self):
        self._select_cell(col=len(self._columns)-1)
    
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
            # Update grouping first.
            cols = self._sorting_columns()
            l = min(len(cols), len(self._grouping))
            self._grouping = tuple(cols[:l])
            self._set_state_param('grouping', self._grouping)
            # Make the changes visible.
            self._refresh(reset={'sorting': sorting},
                          when=self.DOIT_IMMEDIATELY)
        return sorting

    def _cmd_filter_by_cell(self):
        row, col = self._current_cell()
        id = self._columns[col].id()
        value = self._table.row(row)[id]
        self.COMMAND_FILTER_BY_VALUE.invoke(column_id=id, value=value)
            
    def _cmd_autofilter(self, col=None, position=None):
        busy_cursor(True)
        try:
            if col is None:
                col = self._current_cell()[1]
            cid = self._columns[col].id()
            cond = self._current_condition()
            distinct = self._data.distinct(cid, condition=cond)
            if len(distinct) > 60:
                message(_("Příliš mnoho položek pro autofilter."), beep_=True)
                return
            items = [MItem(v.export(), command=ListForm.COMMAND_FILTER_BY_VALUE,
                           args=dict(column_id=cid, value=v))
                     for v in distinct]
        finally:
            busy_cursor(False)
        self._popup_menu(items, position=position)

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
                pos = (rect.GetX() + rect.GetWidth()/3,
                       rect.GetY() + rect.GetHeight()/2 + g.GetColLabelSize())
                position = self._grid.CalcScrolledPosition(pos)
            self._popup_menu(menu, position=position)

    def _popup_menu(self, items, position=None):
        popup_menu(self._grid, items, keymap=self._get_keymap(),
                   position=position)

    def _can_context_action(self, action):
        if action.context() == ActionContext.SELECTION and \
           len(self._selected_rows()) < 1:
            return False
        if action.secondary_context() is not None and \
               self._secondary_context(action.secondary_context()) is None:
            return False
        if not pytis.data.is_in_groups(action.access_groups()):
            return False
        enabled = action.enabled()
        if callable(enabled):
            args = self._context_action_args(action)
            kwargs = action.kwargs()
            return enabled(*args, **kwargs)
        else:
            return enabled

    def _cmd_context_action(self, action):
        args = self._context_action_args(action)
        kwargs = action.kwargs()
        log(EVENT, 'Vyvolávám handler kontextové akce.', (args, kwargs))
        apply(action.handler(), args, kwargs)
        # Hack: Pokud jsme součástí duálního formuláře, chceme refreshnout celý
        # dualform.  Jinak refreshujeme jen sebe sama.
        dual = self._dualform()
        if dual:
            dual.refresh()
        else:
            self.refresh()
        return True

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
                self._grouping = tuple(cols[:pos+1])
            else:
                log(OPERATIONAL, "Invalid grouping column:", cid)
                return
        else:
            self._grouping = ()
        self._set_state_param('grouping', self._grouping)
        self._update_grid()
    
    def _cmd_incremental_search(self, full=False, prefill=None):
        row, col = self._current_cell()
        column = self._columns[col]
        if not isinstance(column.type(self._data), pytis.data.String):
            message(_("V tomto sloupci nelze vyhledávat inkrementálně"),
                    beep_=True)
            return
        search_field = _grid.IncrementalSearch(self, full)
        search_field.run(prefill=prefill)

    def _cmd_copy_cell(self):
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

    def _can_edit(self):
        return self._current_key() is not None
        
    def _cmd_edit(self):
        if not self.editable:
            log(EVENT, 'Pokus o editaci needitovatelné tabulky')
            return False
        table = self._table
        self._transaction = pytis.data.DBTransactionDefault(config.dbconnection)
        if not table.editing():
            if not self._lock_record(self._current_key()):
                self._transaction.rollback()
                return False
            table.edit_row(self._current_cell()[0])
            self._update_selection_colors()
        if not self._edit_cell():
            self._on_line_rollback()
        return True
    
    def _cmd_export_csv(self):
        log(EVENT, 'Vyvolání CSV exportu')
        data = self._data
        # Kontrola počtu řádků
        number_rows = self._table.GetNumberRows()
        if number_rows == 0:
            msg = _("Tabulka neobsahuje žádné řádky! Export nebude proveden.")
            run_dialog(Warning, msg)
            return
        # Seznam sloupců
        column_list = [(c.id(), c.type(data)) for c in self._columns]
        allowed = True
        # Kontrola práv        
        for cid, ctype in column_list:
            if not data.permitted(cid, pytis.data.Permission.EXPORT):
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

    def _cmd_insert_line(self, before=False, copy=False):
        row = self._current_cell()[0]
        log(EVENT, 'Vložení nového řádku:', (row, before, copy))
        if not self._data.permitted(None, pytis.data.Permission.INSERT):
            message('Nemáte přístupová práva pro vkládání záznamů do této ' + \
                    'tabulky!', beep_=True)
            return False
        if not self.editable:
            message('Needitovatelná tabulka!', beep_=True)
            return False

        cols = [c.id() for c in self._columns]
        for col in self._data.columns():
            if col.type().not_null() and col.id() not in cols \
                   and (self._prefill is None \
                        or not self._prefill.has_key(col.id())) \
                   and self._view.field(col.id()) is not None:
                # We silently presume, that when a not null column is not in
                # fields, it probably has a default value (if not, it would be
                # an error anyway), so we can continue.
                msg = _('Povinný sloupec "%s" není zobrazen.\n'
                        'Není možné vkládat řádky v in-line editaci.\n'
                        'Přidejte sloupec nebo použijte editační formulář.')
                label = self._view.field(col.id()).column_label()
                run_dialog(Warning, msg % label)
                return False
        table = self._table
        if table.editing():
            log(EVENT, 'Pokus o vložení nového řádku během editace')
            return False
        self._last_insert_copy = copy
        oldg = self._grid
        oldempty = (oldg.GetNumberRows() == 0)
        if not copy or oldempty:
            inserted_row = None
        else:
            the_row = table.row(row)
            inserted_row = PresentedRow(the_row.fields(), the_row.data(), None,
                                        prefill=self._row_copy_prefill(the_row),
                                        new=True)
        if not before and not oldempty:
            row = row + 1
        if row == -1:
            row = 0
        self._update_grid(inserted_row_number=row, inserted_row=inserted_row)
        self._select_cell(row=row, col=0, invoke_callback=False)
        if not self._is_editable_cell(row, 0) \
               and not self._find_next_editable_cell():
            log(EVENT, 'Žádný sloupec není editovatelný')
            return False
        self._edit_cell()
        self._update_selection_colors()
        log(EVENT, 'Řádek vložen')
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
        log(EVENT, 'Odeslání obsahu políčka gridu', (row, col))
        self._grid.DisableCellEditControl()
        editing = self._table.editing()
        if not editing:
            return
        if editing.valid:
            if not self._find_next_editable_cell():
                if editing.new:
                    q = _("Uložit řádek?")
                    if run_dialog(Question, q, True):
                        return self._on_line_commit()
                    else:
                        log(EVENT, 'Záporná odpověď na dotaz o uložení řádku')
                self._grid.SetGridCursor(row, 0)
        if not editing.valid or editing.new:
            log(EVENT, 'Návrat do editace políčka')
            self._edit_cell()
        
    def _can_cell_rollback(self):
        return self._grid.IsCellEditControlEnabled()

    def _cmd_cell_rollback(self):
        log(EVENT, 'Opuštění políčka gridu beze změny hodnoty')
        self._current_editor.Reset()
        self._grid.DisableCellEditControl()
        self._current_editor = None

    # Veřejné metody
        
    def is_edited(self):
        """Vrať pravdu, právě když je List ve stavu řádkové editace."""
        return self._table.editing()

    def status_fields(self):
        # TODO: zatím je podoba statusbaru určena specifikací, ale bylo by
        # rozumné to celé předělat, aby se statusbar dynamicky měnil podle
        # aktuálního formuláře (s využitím této metody).
        return (('list-position', 7),)
        
    # Ostatní veřejné metody

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

    def focus(self):
        if not self.is_edited():
            self._update_grid(data_init=True)
        super(ListForm, self).focus()
        self._show_position()
        self._show_data_status()
        self._update_selection_colors()
        self._grid.SetFocus()
        
    def defocus(self):
        super(ListForm, self).defocus()
        self._update_selection_colors()



class CodebookForm(PopupForm, ListForm, KeyHandler):
    """Formulář pro zobrazení výběrového seznamu (číselníku).

    Výběrový seznam zobrazuje řádky dat, z nichž uživatel některý řádek
    vybere.  Uživatel kromě výběru a listování nemůže s řádky nijak
    manipulovat.

    Formulář je zobrazen jako modální okno pomocí metody 'run()', která skončí
    po výběru položky a vrátí instanci PresentedRow pro vybraný řádek.  Pokud
    byl formulář ukončen jinak než výběrem záznamu, je vrácena hodnota 'None'.

    """
    DESCR = _("číselník")

    _DEFAULT_WINDOW_HEIGHT = 500

    def __init__(self, parent, *args, **kwargs):
        parent = self._popup_frame(parent)
        super(CodebookForm, self).__init__(parent, *args, **kwargs)
        h = min(self._DEFAULT_WINDOW_HEIGHT, self._total_height()+50)
        self.SetSize((self._total_width()+30, h))
        wx_callback(wx.grid.EVT_GRID_CELL_LEFT_DCLICK, self._grid,
                    lambda e: self.COMMAND_ACTIVATE.invoke())

    def _init_attributes(self, begin_search=None, **kwargs):
        """Zpracuj klíčové argumenty konstruktoru a inicializuj atributy.

        Argumenty:

          begin_search -- Pokud není None, bude po otevření formuláře
            automaticky nastartováno inkrementální vyhledávání. Pokud
            je hodnota řetězec, je chápán jako identifikátor
            sloupce, ve kterém se má provádět vyhledávání. Není-li ho
            hodnota řetězec, nebo neodpovídá-li žádnému sloupci,
            je vyhledávání prováděno automaticky nad sloupečkem s
            primárním tříděním.
            
        """
        try:
            self._cb_spec = self._resolver.get(self._name, 'cb_spec')
        except ResolverError:
            self._cb_spec = CodebookSpec()
        self._begin_search = begin_search
        super(CodebookForm, self)._init_attributes(**kwargs)
          
    def _on_idle(self, event):
        ListForm._on_idle(self, event)
        if not hasattr(self, '_focus_forced_to_grid'):
            self._grid.SetFocus()
            self._focus_forced_to_grid = True
        if self._begin_search:
            begin_search = self._begin_search
            self._begin_search = None
            prefill = None
            if isinstance(begin_search, str):
                col_id = begin_search
            elif isinstance(begin_search, tuple):
                col_id, prefill = begin_search
            else:
                cols = self._sorting_columns()
                if cols:
                    col_id = cols[0]
                else:
                    message(_("Nelze začít inkrementální vyhledávání. "
                              "Číselník neobsahuje žádný setříděný sloupec!"),
                            beep_=True)
            col = find(col_id, self._columns, key=lambda c:c.id())
            if col is not None:
                self._select_cell(row=0, col=self._columns.index(col))
                self.COMMAND_INCREMENTAL_SEARCH.invoke(prefill=prefill)
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

    def _on_activation(self, alternate=False):
        """Nastav návratovou hodnotu a ukonči modální dialog."""
        self._result = self.current_row()
        self._parent.EndModal(1)
        return True

class SelectRowsForm(CodebookForm):
    """Řádkový pop-up formulář vracející tuple všech vybraných řádků."""

    def _on_activation(self, alternate=False):
        self._result = tuple(self.selected_rows())
        self._parent.EndModal(1)
        return True


class BrowseForm(ListForm):
    """Formulář pro prohlížení dat s možností editace."""

    class _PrintResolver (pytis.output.OutputResolver):
        P_NAME = 'P_NAME'
        class _Spec:
            # This class has to emulete a specification module as well as a
            # (new style) specification class.
            def __init__(self, resolver):
                self._resolver = resolver
            def body(self, resolver=None, variant=None):
                table_id = self._resolver.p(BrowseForm._PrintResolver.P_NAME)
                result = pytis.output.data_table(self._resolver, table_id)
                return result
            def doc_header(self, resolver=None, variant=None):
                return None
            def doc_footer(self, resolver=None, variant=None):
                return None
            def coding(self, resolver=None, variant=None):
                if wx.Font_GetDefaultEncoding() == \
                   wx.FONTENCODING_ISO8859_2:
                    result = pytis.output.Coding.LATIN2
                else:
                    result = pytis.output.Coding.ASCII
                return result
            
        def _get_module(self, name):
            # Supply a default specification module (old style spec).
            try:
                x = super(BrowseForm._PrintResolver, self)._get_module(name)
            except ResolverModuleError:
                x = self._Spec(self)
            return x

        def _get_instance(self, key):
            # Supply a default specification class (new style spec).
            try:
                x = super(BrowseForm._PrintResolver, self)._get_instance(key)
            except ResolverSpecError:
                x = self._Spec(self)
            return x
        
    def _init_attributes(self, **kwargs):
        super(BrowseForm, self)._init_attributes(**kwargs)
        menu = (
            MItem(_("Editovat buňku"),
                  command=ListForm.COMMAND_EDIT,
                  help=_("Upravit hodnotu v režimu inline editace")),
            MItem(_("Filtrovat podle buňky"),
                  command=ListForm.COMMAND_FILTER_BY_CELL,
                  help=_("Vyfiltrovat řádky obsahující v tomto sloupci "
                         "stejnou hodnotu")),
            MItem(_("Zkopírovat obsah buňky"),
                  command=ListForm.COMMAND_COPY_CELL,
                  help=_("Zkopírovat hodnotu do schránky.")),
            MSeparator(),
            MItem(_("Editovat záznam"),
                  command=BrowseForm.COMMAND_EDIT_RECORD,
                  help=_("Otevřít editační formulář pro tento záznam.")),
            MItem(_("Smazat záznam"),
                  command=RecordForm.COMMAND_DELETE_RECORD,
                  help=_("Odstranit záznam z databáze.")),
            MItem(_("Náhled"),
                  command=ListForm.COMMAND_ACTIVATE,
                  help=_("Otevřít náhledový formulář s možností procházení "
                         "záznamů"), icon='show-record'),
            MItem(_("Duální náhled"),
                  command=ListForm.COMMAND_ACTIVATE(alternate=True),
                  help=_("Otevřít formulář s tabulkou nahoře a náhledem "
                         "v dolní části."), icon='show-record'),
            )
        actions = self._action_mitems(self._view.actions())
        if actions:
            menu += (MSeparator(),) + tuple(actions)
        self._context_menu_static_part = menu
        # The dynamic part of the menu is created based on the links.
        def link_title(type, name):
            if name.find('::') != -1:
                name1, name2 = name.split('::')
                title = resolver().get(name1, 'binding_spec')[name2].title() or \
                        ' / '.join((resolver().get(name1, 'view_spec').title(),
                                    resolver().get(name2, 'view_spec').title()))
            else:
                title = resolver().get(name, 'view_spec').title()
            mapping = {FormType.BROWSE: _("Odskok - %s"),
                       FormType.EDIT:   _("Editovat %s"),
                       FormType.VIEW:   _("Náhled %s"),
                       FormType.INSERT: _("Nový záznam pro %s")}
            return mapping[type] % title
        # Create automatic links for codebook fields.
        links = [(f, Link(cb, col))  for f, cb, col in
                 remove_duplicates([(f, cb, e.value_column()) for f, cb, e in
                                    [(f, f.codebook(self._data),
                                      f.type(self._data).enumerator())
                                     for f in self._fields] if e and cb])]
        # Add explicit links from FieldSpec.
        for  f in self._fields:
            links.extend([(f, link) for link in f.links()])
        # Now we group all links by the target spec name.
        linkdict = {}
        self._links = []
        for f, link in links:
            if link.label():
                self._links.append((link.label(), f, link))
                continue
            key = (link.type(), link.name(),)
            try:
                a = linkdict[key]
            except KeyError:
                a = linkdict[key] = []
            item = (f,link)
            if item not in a:
                a.append(item)
        # Create the links list as accepted by _link_mitems()
        linklist = linkdict.items()
        linklist.sort()
        for key, items in linklist:
            title = link_title(*key)
            if len(items) == 1:
                f, link = items[0]
                item = (title, f, link)
            else:
                item = (title, [(_("Přes hodnotu sloupce '%s'") % f.label(),
                                 f, link) for f, link in items])
            self._links.append(item)
        
    def _formatter_parameters(self):
        name = self._name
        return {(name+'/'+pytis.output.P_CONDITION):
                pytis.data.AND(self._current_condition()),
                (name+'/'+pytis.output.P_SORTING):
                self._data_sorting(),
                (name+'/'+pytis.output.P_KEY):
                self._current_key(),
                (name+'/'+pytis.output.P_ROW):
                copy.copy(self._table.row(self._current_cell()[0])),
                (name+'/'+pytis.output.P_DATA):
                copy.copy(self._data)
                }

    def _action_mitems(self, spec):
        items = []
        for x in spec:
            if isinstance(x, Action):
                cmd = ListForm.COMMAND_CONTEXT_ACTION(action=x)
                items.append(MItem(x.title(raw=True), command=cmd,
                                   help=x.descr()))
            elif isinstance(x, ActionGroup):
                items.append(Menu(x.title(raw=True),
                                  self._action_mitems(x.actions())))
            elif isinstance(x, (types.TupleType, types.ListType)):
                if items:
                    items.append(MSeparator())
                items.extend(self._action_mitems(x))
            else:
                raise ProgramError("Invalid action specification: %s" % x)
        return items

    def _link_mitems(self, row, spec):
        items = []
        for item in spec:
            if len(item) == 2:
                title, links = item
                subitems = self._link_mitems(row, links)
                if len(subitems) == 1:
                    i = subitems[0]
                    items.append(MItem(title, command=i.command(), icon='link',
                                       args=i.args(), help=i.help()))
                elif subitems:
                    items.append(Menu(title, subitems))
                continue
            title, f, link = item
            if row[f.id()].value() is not None:
                type, name, enabled = link.type(), link.name(), link.enabled()
                pair = {link.column(): row[f.id()]}
                if type == FormType.INSERT:
                    cmd = Application.COMMAND_NEW_RECORD(name=name,prefill=pair)
                    hlp = _("Vložit záznam pro hodnotu '%s' sloupce '%s'.") \
                          % (row.format(f.id()), f.column_label())
                    icon = 'link-new-record'
                else:
                    if name.find('::') != -1:
                        assert type == FormType.BROWSE
                        cls = BrowseDualForm
                    else:
                        mapping = {FormType.BROWSE: BrowseForm,
                                   FormType.EDIT:   PopupEditForm,
                                   FormType.VIEW:   ShowForm}
                        cls = mapping[type]
                    cmd = Application.COMMAND_RUN_FORM(name=name,form_class=cls,
                                                       select_row=pair)
                    hlp = _("Vyhledat záznam pro hodnotu '%s' sloupce '%s'.") \
                          % (row.format(f.id()), f.column_label())
                    icon = 'link'
                if callable(enabled):
                    enabled = enabled(row)
                if not enabled:
                    cmd = Application.COMMAND_NOTHING(enabled=False)
                items.append(MItem(title, command=cmd, help=hlp, icon=icon))
        return items
                           
        
    def _context_menu(self):
        menu = self._context_menu_static_part
        links = list(self._links)
        if links:
            menu += (MSeparator(),) + \
                    tuple(self._link_mitems(self.current_row(), links))
        return menu

    def _cmd_print(self, print_spec_path=None):
        log(EVENT, 'Vyvolání tiskového formuláře:', print_spec_path)
        name = self._name
        if not print_spec_path:
            try:
                spec = self._resolver.get(name, 'print_spec')
            except ResolverError:
                spec = None
            if spec:
                print_spec_path = spec[0][1]
            else:
                print_spec_path = os.path.join('output', name)
        P = self._PrintResolver
        parameters = self._formatter_parameters()
        parameters.update({P.P_NAME: name})
        print_resolver = P(self._resolver, parameters=parameters)
        resolvers = (print_resolver,)
        formatter = pytis.output.Formatter(resolvers, print_spec_path)
        run_form(print_form(), name, formatter=formatter)


class SideBrowseForm(BrowseForm):
    """Form displaying records depending on other form's current row."""

    def _init_attributes(self, main_form, selection_condition, hide_columns=(), **kwargs):
        """Process constructor arguments and initialize attributes.
        
        Arguments:

          main_form -- the main form instance.
          hide_columns -- a list of column names which should be hidden by default
          selection_condition -- function of one agument (PresentedRow instance) returning a
            filtering condition for the current main form row.

        """
        assert isinstance(main_form, Form), main_form
        assert isinstance(hide_columns, (list, tuple)), hide_columns
        assert callable(selection_condition), selection_condition
        self._main_form = main_form
        self._selection_condition = selection_condition
        self._hide_columns = hide_columns
        kwargs['condition'] = pytis.data.OR() # The form will be empty after initialization.
        super(SideBrowseForm, self)._init_attributes(**kwargs)

    def on_selection(self, row):
        """Update current filter condition

        Arguments:

          row -- main form selected row as a PresentedRow instance.

        """
        #log(EVENT, 'Filtrace obsahu formuláře:', (self._name, row))
        self._lf_condition = self._selection_condition(row)
        self._refresh(reset={'filter': None})

    def _default_columns(self):
        columns = super(SideBrowseForm, self)._default_columns()
        if self._hide_columns:
            return tuple([c for c in columns if c not in self._hide_columns])
        else:
            return columns

    def _formatter_parameters(self):
        parameters = super(SideBrowseForm, self)._formatter_parameters()
        parameters.update({self._main_form.name()+'/'+pytis.output.P_ROW:
                           copy.copy(self._main_form.current_row().row())})
        return parameters

    def _update_selection_colors(self):
        g = self._grid
        if focused_window() is not self:
            g.ClearSelection()
        else:
            g.SelectRow(g.GetGridCursorRow())
            super(SideBrowseForm, self)._update_selection_colors()
