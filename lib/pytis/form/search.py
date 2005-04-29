# -*- coding: iso-8859-2 -*-

# Prvky uživatelského rozhraní související s vyhledáváním
# 
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

"""Prvky uživatelského rozhraní související s vyhledáváním.

Modul obsahuje podporu dialogů s funkcionalitou více či méně odpovídající
základnímu vyhledávacímu dialogu.  Základem je společná třída 'Dialog', která
je míněna jako abstraktní.  Z ní jsou pak odvozeny dialogové třídy pro
konkrétní použití.

"""

import pytis.data
from pytis.form import *
import wx


class SFSColumn:
    """Pomocná třída pro specifikaci sloupců do selectoru."""

    def __init__(self, id, type, label):
        """Inicializuj specifikaci sloupce.

        Argumenty:

          id -- datový identifikátor sloupce jako string
          type -- datový typ sloupce jako instance třídy
            'pytis.data.types_.Type'
          label -- jméno sloupce pro uživatele jako string

        """
        self._id = id
        self._type = type
        self._label = label

    def id(self):
        """Vrať id zadané v konstruktoru."""
        return self._id

    def type(self):
        """Vrať type zadaný v konstruktoru."""
        return self._type

    def label(self):
        """Vrať label zadané v konstruktoru."""
        return self._label

    
class SFSDialog(GenericDialog):
    """Společný základ všech vyhledávacích, filtrovacích a třídících dialogů."""

    _FIELD_HEIGHT = 24

    def _create_button(self, label, callback, tip=None):
        b = wx.Button(self._dialog, -1, unicode(label))
        b.SetSize((b.GetSize().width, self._FIELD_HEIGHT))
        wx_callback(wx.EVT_BUTTON, self._dialog, b.GetId(), callback)
        if tip is not None and config.show_tooltips:
            b.SetToolTipString(unicode(tip))
        return b
        
    def _create_choice(self, choices, tip=None, enlarge=True):
        ch = wx.Choice(self._dialog, -1, choices=choices)
        correction = enlarge and 22 or 0 # longer texts may not fit...
        ch.SetSize((ch.GetSize().width+correction, self._FIELD_HEIGHT))
        if tip is not None and config.show_tooltips:
            ch.SetToolTipString(unicode(tip))
        return ch

    def _create_text_ctrl(self, size, value=None, tip=None, readonly=False):
        style = readonly and wx.TE_READONLY or 0
        t = wx.TextCtrl(self._dialog, -1, style=style)
        t.SetSize((dlg2px(t, 4*size), self._FIELD_HEIGHT))
        if value is not None:
            t.SetValue(value)
        if tip is not None and config.show_tooltips:
            t.SetToolTipString(unicode(tip))
        return t

    
class SFDialog(SFSDialog):
    """Společný základ všech vyhledávacích a filtrovacích dialogů."""

    _NO_COLUMN = '---'
    _OPERATORS = (('=',   pytis.data.WM, pytis.data.EQ),
                  ('=<',  pytis.data.LE, pytis.data.LE),
                  ('>=',  pytis.data.GE, pytis.data.GE),
                  ('<',   pytis.data.LT, pytis.data.LT),
                  ('>',   pytis.data.GT, pytis.data.GT),
                  ('=/=', pytis.data.NW, pytis.data.NE))
    _LOGICAL_OPERATORS = ((_("a zároveň"), pytis.data.AND),
                          (_("nebo"), pytis.data.OR))

    _TITLE = ''
    _BUTTONS = (_("Zavřít"),)
    _COMMIT = None

    def __init__(self, parent, columns):
        """Inicializuj dialog.

        Dialog není zobrazen ihned voláním konstruktoru, nýbrž až metodou
        'run()'.  Pokud mají být zohledněny předchozí zadané hodnoty, je nutno
        opakovaně volat metodu 'run()' jedné a téže instance.

        Argumenty:

          parent -- rodič dialogu
          columns -- sekvence instancí třídy 'SFSColumn'

        """
        # Argumenty
        self._row = None
        self._columns = columns
        # Ostatní atributy
        self._selectors = {}
        self._logical_selectors = {}
        self._default_item = (-1, 0, '', True)
        self._defaults = {}
        self._condition = None
        self._number_of_conditions = 1
        # Předek
        super_(SFDialog).__init__(self, parent, self._TITLE, self._BUTTONS)

    def _create_content(self, number_of_conditions=None):
        self._selectors = {}
        self._logical_selectors = {}
        if number_of_conditions is None:
            number_of_conditions = self._number_of_conditions
        parent = self._dialog
        # Prvky
        def condition(id):
            column = self._create_choice(map(SFSColumn.label, self._columns),
                                         tip=_("Zvolte sloupec tabulky"))
            try:
                wcol, wop, wval, __ = self._defaults[id]
            except KeyError:
                wcol, wop, wval, __ = self._defaults[id] = self._default_item
            if wcol == -1:
                if id == 0:
                    sel = self._colnum
                else:
                    sel = self._defaults[id-1][0]
                column.SetSelection(sel)
            else:
                column.SetSelection(wcol)
            op = self._create_choice(map(lambda o: o[0], self._OPERATORS),
                                     tip=_("Zvolte operátor"), enlarge=False)
            op.SetSelection(wop)
            value = self._create_text_ctrl(18, wval,
                                   tip=_("Zapište hodnotu podmínkového výrazu"))
            self._selectors[id] = (column, op, value)
            clear  = self._create_button(_("Smazat"),
                                         lambda e: self._on_clear(e, id),
                                         tip=_("Vymazat obsah podmínky"))
            suck   = self._create_button(_("Nasát"),
                                         lambda e: self._on_suck(e, id),
                                         tip=_("Načíst hodnotu aktivní buňky"))
            remove = self._create_button(_("Odebrat"),
                                         lambda e: self._on_remove(e, id),
                                         tip=_("Zrušit tuto podmínku"))
            sizer = wx.BoxSizer()
            for x in (column, op, value, clear, suck, remove):
                sizer.Add(x)
            return sizer
        def and_or_separator(id):
            try:
                defaults = self._defaults[id]
            except KeyError:
                selection = 0
            else:
                selection = defaults[3]
            op = self._create_choice(map(lambda o: o[0],
                                         self._LOGICAL_OPERATORS),
                                     tip=_("Zvolte typ spojení podmínek"))
            op.SetSelection(selection)
            self._logical_selectors[id] = op
            return op
        # Vytvoření prvků a sizerů
        sizer = wx.BoxSizer(wx.VERTICAL)
        sizer.Add(condition(0))
        for i in range(1, number_of_conditions):
            and_sizer = wx.BoxSizer()
            and_sizer.Add(and_or_separator(i-1))
            sizer.Add(and_sizer)
            sizer.Add(condition(i))
        b1 = self._create_button(_('Přidat "a zároveň"'), self._on_add,
                                 tip=_("Přidat novou podmínku v konjunkci"))
        b2 = self._create_button(_('Přidat "nebo"'), self._on_add_or_,
                                 tip=_("Přidat novou podmínku v disjunkci"))
        button_sizer = wx.BoxSizer(wx.HORIZONTAL)
        button_sizer.Add(b1)
        button_sizer.Add(b2, 0, wx.LEFT, 20)
        # Hotovo
        return (sizer, button_sizer)

    def _selected_condition(self):
        condition = None
        selectors = self._selectors
        logical_selectors = self._logical_selectors
        keys = selectors.keys()
        keys.sort()
        for k in keys:
            wcol, wop, wval = selectors[k]
            vcol = wcol.GetStringSelection()
            for c in self._columns:
                if c.label() == vcol:
                    column_id = c.id()
                    column_type = c.type()
                    break
            else:
                raise ProgramError('Column disappeared', vcol)
            vop = wop.GetStringSelection()
            vval = wval.GetValue()
            for o in self._OPERATORS:
                if o[0] == vop:
                    if (o[1] is not o[2] and
                        vval.find('*') >= 0 or vval.find('?') >= 0):
                        op = o[1]
                        value, err = column_type.wm_validate(vval)
                    elif isinstance(column_type, pytis.data.Boolean):
                        op = o[2]
                        value, err = column_type.validate(vval, extended=True)
                    else:
                        op = o[2]
                        value, err = column_type.validate(vval)
                    if err:
                        message = _("Chybná hodnota v podmínce %d:\n%s") % \
                                  (k+1, err)
                        run_dialog(Error, message)
                        return
                    break
            else:
                raise ProgramError('Operator disappeared', vop)
            subcondition = op(column_id, value)
            if k > 0:
                lopindex = logical_selectors[k-1].GetSelection()
                lop = self._LOGICAL_OPERATORS[lopindex][1]
                condition = lop(condition, subcondition)
            else:
                condition = subcondition
        return condition

    def _on_clear(self, event, id):
        column, op, value = self._selectors[id]
        column.SetSelection(0)
        op.SetSelection(0)
        value.SetValue('')

    def _on_suck(self, event, id):
        column, __, value = self._selectors[id]
        label = column.GetStringSelection()
        for c in self._columns:
            if c.label() == label:
                v = self._row[c.id()].export()
                break
        else:
            raise ProgramError('Unknown column selected', label)
        if is_sequence(v):
            v = v[0]
        value.SetValue(v)

    def _on_add(self, event, logical_selection=0):
        self._number_of_conditions = self._number_of_conditions + 1
        self._save_values(logical_selection=logical_selection)
        self.rebuild()
    
    def _on_add_or_(self, event):
        self._on_add(event, logical_selection=1)
    
    def _on_remove(self, event, id):
        if self._number_of_conditions > 1:
            self._number_of_conditions = self._number_of_conditions - 1
            self._save_values(exclude=id)
            self.rebuild()
    
    def _on_cancel(self, event):
        self._condition = None
        self._widget.EndModal(0)

    def _commit_dialog(self):
        return self._end_modal(self._button_id(self._COMMIT))

    def _customize_result(self, button_wid):
        self._condition = None
        return self._condition

    def _save_values(self, logical_selection=0, exclude=None):
        # _logical_selectors[i] váže _selectors[i] a _selectors[i+1]
        self._defaults = {}
        for k, v in self._selectors.items():
            if exclude is not None:
                if k == exclude:
                    del self._selectors[k]
                    try:
                        del self._logical_selectors[k]
                    except KeyError:
                        pass
                    continue
                elif k > exclude:
                    self._selectors[k-1] = v
                    del self._selectors[k]
                    try:
                        self._logical_selectors[k-1] = \
                          self._logical_selectors[k]
                        del self._logical_selectors[k]
                    except KeyError:
                        pass
                    k = k - 1
            wcol, wop, wval = v
            try:
                logsel = self._logical_selectors[k].GetSelection()
            except KeyError:
                logsel = logical_selection
            defaults = (wcol.GetSelection(), wop.GetSelection(),
                        wval.GetValue(), logsel)
            self._defaults[k] = defaults
            
    def append_condition(self, col_id, value, and_=True):
        """Přidej filtrovací podmínku neinteraktivně.

        Metoda neinteraktivně přidá do dialogu podmínku rovnosti sloupce
        'col_id' na hodnotu 'value'.  Pokud je argument 'and_' pravdivý, bude
        podmínka přidána v konjunkci se stávajícími podmínkami, jinak v
        disjunkci.

        Metoda je určena k použití ve chvíli, kdy není zobrazen dialog.  Při
        jiném použití je chování nedefinováno.
        
        """
        logop = and_ and pytis.data.AND or pytis.data.OR
        op = pytis.data.EQ
        condition = op(col_id, value)
        if self._condition is None:
            self._condition = condition
        else:
            self._condition = logop(self._condition, condition)
        # Protože celý způsob ukládání defaults je dost čuňárna (vázaný na
        # widgety), tak i toto je čuňárna...
        cols = filter(lambda c: c.id() == col_id, self._columns)
        assert len(cols) == 1, 'Column not found: %s' % col_id
        col = self._columns.index(cols[0])
        defaults = (col, 0, value.export(), and_ and 1 or 0)
        if self._number_of_conditions != 1 or len(self._defaults.keys()) != 0:
            self._number_of_conditions += 1
        self._defaults[self._number_of_conditions-1] = defaults
            
    def _finish_dialog(self):
        self._save_values()

    def condition(self):
        """Vrať aktuální podmínku nebo 'None', pokud žádná není."""
        return self._condition

    def run(self, current_row, col):
        """Zobraz formulář a po jeho ukončení vrať zvolenou podmínku.

        Argumenty:
        
          current_row -- aktuální řádek formuláře jako instance třídy
            'pytis.data.Row'; pokud ve formuláři není zvolen žádný řádek, tak
            'None'
          col -- číslo aktuálního sloupce, počínaje od 0

        Vrácená podmínka je instance podmínkového operátoru používaného
        metodami třídy 'pytis.data.Data'.  Pokud je dialog opuštěn bez zadání
        podmínky (typicky stiskem tlačítka \"Zrušit\"), vrať 'None'.

        """
        self._row = current_row
        self._colnum = col
        return super_(SFDialog).run(self)


class SearchDialog(SFDialog):
    """Dialog pro vyhledávání v řádkových seznamech."""

    _NEXT_BUTTON = _("Další")
    _PREVIOUS_BUTTON = _("Předchozí")
    _BUTTONS = (_NEXT_BUTTON, _PREVIOUS_BUTTON) + SFDialog._BUTTONS
    _COMMIT = _NEXT_BUTTON    
    _TITLE = _("Hledání")

    def _search(self, direction):
        self._condition = self._selected_condition()
        self._direction = direction

    def _customize_result(self, button_wid):
        label = self._button_label(button_wid)
        if label == self._NEXT_BUTTON:
            self._search(pytis.data.FORWARD)
        elif label == self._PREVIOUS_BUTTON:
            self._search(pytis.data.BACKWARD)
        else:
            return None, None
        result = self._condition, self._direction
        return result

    def run(self, current_row, col=None):
        """Zobraz formulář a po jeho ukončení vrať zvolené parametry hledání.

        Argumenty:
        
          row -- aktuální řádek formuláře jako instance třídy 'pytis.data.Row';
            pokud ve formuláři není zvolen žádný řádek, tak 'None'
          col -- číslo aktuálního sloupce, počínaje od 0; může být též 'None',
            v kterémžto případě je použito implicitní číslo sloupce

        Vrací: Dvojici (CONDITION, DIRECTION).  CONDITION je instance
        podmínkového operátoru používaného metodami třídy 'pytis.data.Data'.
        DIRECTION je požadovaný směr hledání, jedna z konstant
        'pytis.data.FORWARD' a 'pytis.data.BACKWARD'.  Pokud je dialog opuštěn bez
        zadání podmínky (typicky stiskem tlačítka \"Zrušit\"), je CONDITION
        'None' a hodnota DIRECTION je nedefinována.

        """
        self._direction = None
        if col is None:
            col = 0
        return super_(SearchDialog).run(self, current_row, col)


class FilterDialog(SFDialog):
    """Dialog pro filtrování v řádkových seznamech."""

    _FILTER_BUTTON = _("Filtrovat")
    _UNFILTER_BUTTON = _("Zrušit filtr")
    _BUTTONS = (_FILTER_BUTTON, _UNFILTER_BUTTON) + SFDialog._BUTTONS
    _COMMIT = _FILTER_BUTTON
    _AGG_OPERATORS = ((_("Počet"), pytis.data.Data.AGG_COUNT),
                      (_("Minimum"), pytis.data.Data.AGG_MIN),
                      (_("Maximum"), pytis.data.Data.AGG_MAX),
                      (_("Součet"), pytis.data.Data.AGG_SUM),
                      (_("Průměr"), pytis.data.Data.AGG_AVG))
    _TITLE = _("Filtrování")

    def __init__(self, parent, columns, data, filter=None):
        self._data = data
        self._data_filter = filter
        super_(FilterDialog).__init__(self, parent, columns)
        
    def _create_content(self, **kwargs):
        content = super_(FilterDialog)._create_content(self, **kwargs)
        column_choices = map(SFSColumn.label, self._columns)
        op_choices = map(lambda o: o[0], self._AGG_OPERATORS)
        self._agg_column = self._create_choice(column_choices,
                                          tip=_("Zvolte sloupec pro agregaci"))
        self._agg_operator = self._create_choice(op_choices,
                                          tip=_("Zvolte agregační funkci"))
        self._agg_result = self._create_text_ctrl(24, readonly=True,
                                   tip=_("Zobrazení výsledku agregační funkce"))
        go = self._create_button(_("Zjistit"), self._on_compute_aggregate,
                                 _("Zobraz výsledek zvolené agrekační funkce"))
        computer = wx.BoxSizer(wx.HORIZONTAL)
        for w in self._agg_column, self._agg_operator, self._agg_result, go:
            computer.Add(w)
        return content + (computer,)

    def _on_filter(self):
        self._condition = self._selected_condition()
        self._perform = True

    def _on_reset_filter(self):
        self._condition = None
        self._perform = True

    def _on_compute_aggregate(self, event):
        operator = self._AGG_OPERATORS[self._agg_operator.GetSelection()][1]
        colvalue = self._agg_column.GetSelection()
        column = self._columns[colvalue].id()
        operation = (operator, column)
        self._on_filter()
        condition = self._condition
        if self._data_filter is not None:
            condition = pytis.data.AND(condition, self._data_filter)
        result = self._data.select_aggregate(operation, condition)
        if result is not None:
            self._agg_result.SetValue(result.export())

    def _customize_result(self, button_wid):
        label = self._button_label(button_wid)
        if label == self._FILTER_BUTTON:
            self._on_filter()
        elif label == self._UNFILTER_BUTTON:
            self._on_reset_filter()
        else:
            return None, None
        result = self._perform, self._condition
        return result

    def run(self, current_row, col=None):
        """Zobraz formulář a po jeho ukončení vrať parametry filtrování.
        
        Argumenty:
                  current_row -- aktuální řádek formuláře jako instance třídy
            'pytis.data.Row'; pokud ve formuláři není zvolen žádný řádek, tak
            'None'
          col -- číslo aktuálního sloupce, počínaje od 0; je-li 'None', bude
            vybrán nějaký implicitní sloupec

        Vrací: Dvojici (FILTER, CONDITION).  CONDITION je instance
        podmínkového operátoru používaného metodami třídy 'pytis.data.Data'.
        FILTER je flag udávající, zda má být filtrování provedeno nebo zda byl
        dialog uživatelem zrušen bez požadavku na nové filtrování.

        """
        self._perform = False
        if col is None:
            col = 0
        return super_(FilterDialog).run(self, current_row, col)


class SortingDialog(SFSDialog):
    """Dialog pro volbu parametrů třídění.

    Metoda 'run()' vrací specifikaci třídění, kterou používá formulář
    `pytis.form.LookupForm'.  Je-li dialog opuštěn jiným způsobem než stiskem
    tlačítka, je vráceno 'None'.

    """

    _OK_BUTTON = _("Setřídit")
    _CANCEL_BUTTON = _("Resetovat třídění")
    _ESCAPE_BUTTON = _("Zavřít")
    
    _ASCENDENT = _("Vzestupně")
    _DESCENDANT = _("Sestupně")
    _NOSORTING = _("Netřídit")
    
    def __init__(self, parent, columns, sorting, col=None, direction=None):
        """Inicializuj dialog.

        Argumenty:

          parent -- rodič dialogu
          columns -- sekvence instancí třídy 'SFSColumn'
          sorting -- specifikace aktuálního třídění, ve tvaru argumentu 'sort'
            metody 'pytis.data.Data.select()'
          col -- id implicitního sloupce jako string
          direction -- implicitní směr třídění, jedna ze směrových konstant
            modulu 'pytis.data'

        """
        self._parent = parent
        self._columns = columns
        self._sorting = sorting
        self._col = col
        self._direction = direction
        buttons = (self._OK_BUTTON, self._CANCEL_BUTTON, self._ESCAPE_BUTTON)
        super_(SortingDialog).__init__(self, parent, _("Třídění"), buttons)

    def _create_content(self):
        columns = self._columns
        column_choices = map(lambda c: c.label(), columns)
        direction_choices = [self._ASCENDENT, self._DESCENDANT,
                             self._NOSORTING]
        self._selections = []
        big_sizer = wx.BoxSizer(wx.VERTICAL)
        if self._sorting:
            for col, dir in self._sorting:
                # Sloupce
                colsel = self._create_choice(column_choices,
                     tip=_("Zvolte sloupec tabulky, podle nějž chcete třídit"))
                colpos = position(col, columns, key=lambda c: c.id())
                if colpos:
                    colsel.SetSelection(colpos)
                # Směr třídění
                dirsel = self._create_choice(direction_choices,
                                             tip=_("Zvolte směr třídění"))
                if dir == LookupForm.SORTING_DESCENDANT:
                    dirsel.SetSelection(1)
                self._selections.append((colsel, dirsel))
                # Sizer
                sizer = wx.BoxSizer()
                for w in colsel, dirsel:
                    sizer.Add(w)
                big_sizer.Add(sizer)
        else:
            # Sloupce
            colsel = self._create_choice(column_choices,
                     tip=_("Zvolte sloupec tabulky, podle nějž chcete třídit"))
            col = self._col
            if col is not None:
                for i in range(len(columns)):
                    if columns[i].id() == col:
                        colsel.SetSelection(i)
                        break
            # Směr třídění
            dirsel = self._create_choice(direction_choices,
                                         tip=_("Zvolte směr třídění"))
            if self._direction == LookupForm.SORTING_DESCENDANT:
                dirsel.SetSelection(1)
            self._selections.append((colsel, dirsel))
            # Sizer
            sizer = wx.BoxSizer()
            for w in colsel, dirsel:
                sizer.Add(w)
            big_sizer.Add(sizer)
        add_button = self._create_button(_("Přidat"), self._on_add,
                                  tip=_("Přidat sloupec sekundárního třídění"))
        big_sizer.Add(add_button)
        return big_sizer

    def _commit_dialog(self):
        return self._end_modal(self._button_id(self._OK_BUTTON))
        
    def _customize_result(self, button_wid):
        label = self._button_label(button_wid)
        if label == self._CANCEL_BUTTON:
            return ()
        elif label != self._OK_BUTTON:
            return None
        return self._customize_result_sorting()

    def _customize_result_sorting(self):
        sorting = []
        for colsel, dirsel in self._selections:
            # Sloupec
            colvalue = colsel.GetSelection()
            colid = self._columns[colvalue].id()
            # Směr
            dirvalue = dirsel.GetSelection()
            if dirvalue == 0:
                direction = LookupForm.SORTING_ASCENDENT
            elif dirvalue == 1:
                direction = LookupForm.SORTING_DESCENDANT
            elif dirvalue == 2:
                continue
            else:
                raise ProgramError('Invalid direction selection', dirvalue)
            sorting.append((colid, direction))
        return tuple(sorting)

    def _on_add(self, event):
        new = (self._columns[0].id(), LookupForm.SORTING_DESCENDANT)
        self._sorting = self._customize_result_sorting() + (new,)
        self.rebuild()


def sfs_columns(columns, data, labelfunc=FieldSpec.label,
                widthfunc=FieldSpec.width):
    """Vrať sloupce vhodné k použití v konstruktorech SFS dialogů.

    (SFS = Search, Filter, Sort)

    Argumenty:

      columns -- sekvence instancí třídy 'Column' obsahující sloupce
      data -- datový objekt, na nějž jsou sloupce navázány
      labelfunc -- funkce jednoho argumentu (instance 'Column') vracející
        návěští sloupce v dialogu
      widthfunc -- funkce jednoho argumentu (instance 'Column') vracející
        šířku sloupce (obecnou, slouží k rozpoznání skrytých políček)

    """
    sfs_columns = []
    for c in columns:
        if data.find_column(c.id()) is None or \
           not widthfunc(c):
            continue
        id = c.id()
        label = labelfunc(c)
        type_ = c.type(data)
        sfs_columns.append(SFSColumn(id, type_, label))
    return sfs_columns
