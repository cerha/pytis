# -*- coding: iso-8859-2 -*-

# Prvky u�ivatelsk�ho rozhran� souvisej�c� s�vyhled�v�n�m
# 
# Copyright (C) 2001-2006 Brailcom, o.p.s.
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

"""Prvky u�ivatelsk�ho rozhran� souvisej�c� s�vyhled�v�n�m.

Modul obsahuje podporu dialog� s�funkcionalitou v�ce �i m�n� odpov�daj�c�
z�kladn�mu vyhled�vac�mu dialogu.  Z�kladem je spole�n� t��da 'Dialog', kter�
je m�n�na jako abstraktn�.  Z�n� jsou pak odvozeny dialogov� t��dy pro
konkr�tn� pou�it�.

"""

import pytis.data
from pytis.form import *
from pytis.presentation import *
import wx


class SFSColumn:
    """Pomocn� t��da pro specifikaci sloupc� do selectoru."""

    def __init__(self, id, type, label):
        """Inicializuj specifikaci sloupce.

        Argumenty:

          id -- datov� identifik�tor sloupce jako string
          type -- datov� typ sloupce jako instance t��dy
            'pytis.data.types_.Type'
          label -- jm�no sloupce pro u�ivatele jako string

        """
        self._id = id
        self._type = type
        self._label = label

    def id(self):
        """Vra� id zadan� v�konstruktoru."""
        return self._id

    def type(self):
        """Vra� type zadan� v�konstruktoru."""
        return self._type

    def label(self):
        """Vra� label zadan� v�konstruktoru."""
        return self._label

    
class SFSDialog(GenericDialog):
    """Spole�n� z�klad v�ech vyhled�vac�ch, filtrovac�ch a t��d�c�ch dialog�."""

    _FIELD_HEIGHT = 24

    def _create_button(self, label, callback, tip=None):
        b = wx.Button(self._dialog, -1, unicode(label))
        b.SetMinSize((b.GetSize().width, self._FIELD_HEIGHT))
        wx_callback(wx.EVT_BUTTON, self._dialog, b.GetId(), callback)
        if tip is not None and config.show_tooltips:
            b.SetToolTipString(unicode(tip))
        return b
        
    def _create_choice(self, choices, tip=None, enlarge=True):
        ch = wx.Choice(self._dialog, -1, choices=choices)
        ch.SetSelection(0)
        correction = enlarge and 22 or 0 # longer texts may not fit...
        ch.SetMinSize((ch.GetSize().width+correction, self._FIELD_HEIGHT))
        if tip is not None and config.show_tooltips:
            ch.SetToolTipString(unicode(tip))
        return ch

    def _create_text_ctrl(self, size, value=None, tip=None, readonly=False):
        style = readonly and wx.TE_READONLY or 0
        t = wx.TextCtrl(self._dialog, -1, style=style)
        t.SetMinSize((dlg2px(t, 4*size), self._FIELD_HEIGHT))
        if value is not None:
            t.SetValue(value)
        if tip is not None and config.show_tooltips:
            t.SetToolTipString(unicode(tip))
        return t

    
class SFDialog(SFSDialog):
    """Spole�n� z�klad v�ech vyhled�vac�ch a filtrovac�ch dialog�."""

    _NO_COLUMN = '---'
    _OPERATORS = (('=',   pytis.data.WM, pytis.data.EQ),
                  ('=<',  pytis.data.LE, pytis.data.LE),
                  ('>=',  pytis.data.GE, pytis.data.GE),
                  ('<',   pytis.data.LT, pytis.data.LT),
                  ('>',   pytis.data.GT, pytis.data.GT),
                  ('=/=', pytis.data.NW, pytis.data.NE))
    _LOGICAL_OPERATORS = ((_("a z�rove�"), pytis.data.AND),
                          (_("nebo"), pytis.data.OR))

    _TITLE = ''
    _BUTTONS = (_("Zav��t"),)

    def __init__(self, parent, columns):
        """Inicializuj dialog.

        Dialog nen� zobrazen ihned vol�n�m konstruktoru, n�br� a� metodou
        'run()'.  Pokud maj� b�t zohledn�ny p�edchoz� zadan� hodnoty, je nutno
        opakovan� volat metodu 'run()' jedn� a t�e instance.

        Argumenty:

          parent -- rodi� dialogu
          columns -- sekvence instanc� t��dy 'SFSColumn'

        """
        # Argumenty
        self._row = None
        self._columns = columns
        # Ostatn� atributy
        self._selectors = {}
        self._logical_selectors = {}
        self._default_item = (-1, 0, '', 0)
        self._defaults = {}
        self._condition = None
        self._number_of_conditions = 1
        # P�edek
        super_(SFDialog).__init__(self, parent, self._TITLE, self._BUTTONS)

    def _create_content(self, number_of_conditions=None):
        self._selectors = {}
        self._logical_selectors = {}
        if number_of_conditions is None:
            number_of_conditions = self._number_of_conditions
        parent = self._dialog
        # Prvky
        def condition(id):
            column = self._create_choice([c.label() for c in self._columns],
                                         tip=_("Zvolte sloupec tabulky"))
            try:
                wcol, wop, wval, __ = self._defaults[id]
            except KeyError:
                wcol, wop, wval, __ = self._defaults[id] = self._default_item
            if wcol == -1 and self._field_id:
                c = find(self._field_id, self._columns, key=lambda c: c.id())
                if c is not None:
                    wcol = self._columns.index(c)
            if wcol == -1:
                if id > 0 and self._defaults[id-1][0] != -1:
                    wcol = self._defaults[id-1][0]
                else:
                    wcol = 0
            column.SetSelection(wcol)
            op = self._create_choice(map(lambda o: o[0], self._OPERATORS),
                                     tip=_("Zvolte oper�tor"), enlarge=False)
            op.SetSelection(wop)
            value = self._create_text_ctrl(18, wval,
                                   tip=_("Zapi�te hodnotu podm�nkov�ho v�razu"))
            self._selectors[id] = (column, op, value)
            buttons = [self._create_button(label, handler, tip=tooltip)
                       for label, tooltip, handler in
                       ((_("Smazat"), _("Vymazat obsah podm�nky"),
                         lambda e: self._on_clear(e, id)),
                        (_("Nas�t"), _("Na��st hodnotu aktivn� bu�ky"),
                         lambda e: self._on_suck(e, id)),
                        (_("Odebrat"), _("Zru�it tuto podm�nku"),
                         lambda e: self._on_remove(e, id)))]
            sizer = wx.BoxSizer()
            for x in (column, op, value) + tuple(buttons):
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
                                     tip=_("Zvolte typ spojen� podm�nek"))
            op.SetSelection(selection)
            self._logical_selectors[id] = op
            return op
        # Vytvo�en� prvk� a sizer�
        sizer = wx.BoxSizer(wx.VERTICAL)
        sizer.Add(condition(0))
        for i in range(1, number_of_conditions):
            and_sizer = wx.BoxSizer()
            and_sizer.Add(and_or_separator(i-1))
            sizer.Add(and_sizer)
            sizer.Add(condition(i))
        b1 = self._create_button(_('P�idat "a z�rove�"'), self._on_add,
                                 tip=_("P�idat novou podm�nku v konjunkci"))
        b2 = self._create_button(_('P�idat "nebo"'), self._on_add_or_,
                                 tip=_("P�idat novou podm�nku v disjunkci"))
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
            column = self._columns[wcol.GetSelection()]
            vop = wop.GetStringSelection()
            vval = wval.GetValue()
            for o in self._OPERATORS:
                if o[0] == vop:
                    if (o[1] is not o[2] and
                        vval.find('*') >= 0 or vval.find('?') >= 0):
                        op = o[1]
                        value, err = column.type().wm_validate(vval)
                    elif isinstance(column.type(), pytis.data.Boolean):
                        op = o[2]
                        value, err = column.type().validate(vval, extended=True)
                    else:
                        op = o[2]
                        value, err = column.type().validate(vval, strict=False)
                    if err:
                        msg = _("Chybn� hodnota v podm�nce %d:\n%s") % \
                              (k+1, err.message())
                        run_dialog(Error, msg)
                        raise err
                    break
            else:
                raise ProgramError('Operator disappeared', vop)
            subcondition = op(column.id(), value)
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
        wcol, __, value = self._selectors[id]
        column = self._columns[wcol.GetSelection()]
        v = self._row[column.id()].export()
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

    def _customize_result(self, button_wid):
        self._condition = None
        return self._condition

    def _save_values(self, logical_selection=0, exclude=None):
        # _logical_selectors[i] v�e _selectors[i] a _selectors[i+1]
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
        keys = self._defaults.keys()
        if len(keys) == 1 and self._defaults[keys[0]][2] == '':
            # We don't want to remember an empty condition.
            self._defaults = {}
            
    def append_condition(self, col_id, value):
        """P�idej filtrovac� podm�nku neinteraktivn�.

        Metoda neinteraktivn� p�id� do dialogu podm�nku rovnosti sloupce
        'col_id' na hodnotu 'value'.  Podm�nka je p�id�na v�dy v konjunkci se
        st�vaj�c�mi podm�nkami.

        Metoda je ur�ena k pou�it� ve chv�li, kdy nen� zobrazen dialog.  P�i
        jin�m pou�it� je chov�n� nedefinov�no.
        
        """
        c = find(col_id, self._columns, key=lambda c: c.id())
        if c is None:
            return False
        condition = pytis.data.EQ(col_id, value)
        if self._condition is None:
            self._condition = condition
        else:
            self._condition = pytis.data.AND(self._condition, condition)
        # Proto�e cel� zp�sob ukl�d�n� defaults je dost �u��rna (v�zan� na
        # widgety), tak i toto je �u��rna...
        defaults = (self._columns.index(c), 0, value.export(), 0)
        if self._number_of_conditions != 1 or len(self._defaults.keys()) != 0:
            self._number_of_conditions += 1
        self._defaults[self._number_of_conditions-1] = defaults
        if self._number_of_conditions > 1:
            d = self._defaults[self._number_of_conditions-2]
            d = d[:3] + (0,)
            self._defaults[self._number_of_conditions-2] = d
        return True
            
    def _finish_dialog(self):
        self._save_values()

    def condition(self):
        """Vra� aktu�ln� podm�nku nebo 'None', pokud ��dn� nen�."""
        return self._condition

    def run(self, current_row, current_field):
        """Zobraz formul�� a po jeho ukon�en� vra� zvolenou podm�nku.

        Argumenty:
        
          current_row -- aktu�ln� ��dek formul��e jako instance t��dy
            'pytis.data.Row'; pokud ve formul��i nen� zvolen ��dn� ��dek, tak
            'None'
          current_field -- identifik�tor aktu�ln�ho pol��ka/sloupce.  Je-li
            'None', bude vybr�n n�jak� implicitn� sloupec.

        Vr�cen� podm�nka je instance podm�nkov�ho oper�toru pou��van�ho
        metodami t��dy 'pytis.data.Data'.  Pokud je dialog opu�t�n bez zad�n�
        podm�nky (typicky stiskem tla��tka \"Zru�it\"), vra� 'None'.

        """
        self._row = current_row
        self._field_id = current_field
        return super_(SFDialog).run(self)


class SearchDialog(SFDialog):
    """Dialog pro vyhled�v�n� v���dkov�ch seznamech."""

    _NEXT_BUTTON = _("Dal��")
    _PREVIOUS_BUTTON = _("P�edchoz�")
    _BUTTONS = (_NEXT_BUTTON, _PREVIOUS_BUTTON) + SFDialog._BUTTONS
    _COMMIT_BUTTON = _NEXT_BUTTON    
    _TITLE = _("Hled�n�")
    _HELP_TOPIC = 'searching'
    
    def _search(self, direction):
        try:
            self._condition = self._selected_condition()
        except pytis.data.ValidationError, e:
            return False
        self._direction = direction
        return True

    def _on_button(self, event):
        label = self._button_label(event.GetId())
        if label == self._NEXT_BUTTON:
            direction = pytis.data.FORWARD
        elif label == self._PREVIOUS_BUTTON:
            direction = pytis.data.BACKWARD
        else:
            direction = None
        if direction is None or self._search(direction):
            return super(SearchDialog, self)._on_button(event)

    def _customize_result(self, button_wid):
        label = self._button_label(button_wid)
        if label in (self._NEXT_BUTTON, self._PREVIOUS_BUTTON):
            return self._condition, self._direction
        else:
            return None, None

    def run(self, current_row, current_field):
        """Zobraz formul�� a po jeho ukon�en� vra� zvolen� parametry hled�n�.

        Argumenty:
        
          row -- aktu�ln� ��dek formul��e jako instance t��dy 'pytis.data.Row';
            pokud ve formul��i nen� zvolen ��dn� ��dek, tak 'None'
          current_field -- identifik�tor aktu�ln�ho pol��ka/sloupce.  Je-li
            'None', bude vybr�n n�jak� implicitn� sloupec.

        Vrac�: Dvojici (CONDITION, DIRECTION).  CONDITION je instance
        podm�nkov�ho oper�toru pou��van�ho metodami t��dy 'pytis.data.Data'.
        DIRECTION je po�adovan� sm�r hled�n�, jedna z�konstant
        'pytis.data.FORWARD' a 'pytis.data.BACKWARD'.  Pokud je dialog opu�t�n bez
        zad�n� podm�nky (typicky stiskem tla��tka \"Zru�it\"), je CONDITION
        'None' a hodnota DIRECTION je nedefinov�na.

        """
        self._direction = None
        return super_(SearchDialog).run(self, current_row, current_field)


class FilterDialog(SFDialog):
    """Dialog pro filtrov�n� v���dkov�ch seznamech."""

    _FILTER_BUTTON = _("Filtrovat")
    _UNFILTER_BUTTON = _("Zru�it filtr")
    _BUTTONS = (_FILTER_BUTTON, _UNFILTER_BUTTON) + SFDialog._BUTTONS
    _COMMIT_BUTTON = _FILTER_BUTTON
    _AGG_OPERATORS = ((_("Po�et"), pytis.data.Data.AGG_COUNT),
                      (_("Minimum"), pytis.data.Data.AGG_MIN),
                      (_("Maximum"), pytis.data.Data.AGG_MAX),
                      (_("Sou�et"), pytis.data.Data.AGG_SUM),
                      (_("Pr�m�r"), pytis.data.Data.AGG_AVG))
    _TITLE = _("Filtrov�n�")
    _HELP_TOPIC = 'filtering'

    def _create_content(self, **kwargs):
        content = super_(FilterDialog)._create_content(self, **kwargs)
        column_choices = map(SFSColumn.label, self._columns)
        op_choices = map(lambda o: o[0], self._AGG_OPERATORS)
        self._agg_column = self._create_choice(column_choices,
                                          tip=_("Zvolte sloupec pro agregaci"))
        self._agg_operator = self._create_choice(op_choices,
                                          tip=_("Zvolte agrega�n� funkci"))
        self._agg_result = self._create_text_ctrl(24, readonly=True,
                                   tip=_("Zobrazen� v�sledku agrega�n� funkce"))
        go = self._create_button(_("Zjistit"), self._on_compute_aggregate,
                                 _("Zobraz v�sledek zvolen� agreka�n� funkce"))
        computer = wx.BoxSizer(wx.HORIZONTAL)
        for w in self._agg_column, self._agg_operator, self._agg_result, go:
            computer.Add(w)
        return content + (computer,)

    def _on_filter(self):
        try:
            self._condition = self._selected_condition()
        except pytis.data.ValidationError, e:
            return False
        self._perform = True
        return True

    def _on_button(self, event):
        label = self._button_label(event.GetId())
        if label == self._FILTER_BUTTON:
            if not self._on_filter():
                return False
        elif label == self._UNFILTER_BUTTON:
            self._on_reset_filter()
        return super(FilterDialog, self)._on_button(event)
        
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
        if label in (self._FILTER_BUTTON, self._UNFILTER_BUTTON):
            return self._perform, self._condition
        else:
            return None, None

    def run(self, data, filter, current_row, current_field):
        """Zobraz formul�� a po jeho ukon�en� vra� parametry filtrov�n�.
        
        Argumenty:

          data -- datov� objekt nad n�m� budou prov�d�ny p��padn� agrega�n�
            funkce.
          filter -- p��padn� dopl�uj�c� podm�nka filtruj�c� data datov�ho
            objektu p�i v�po�tu agrega�n�ch funkc�.  Tato podm�nka bude
            uplatn�ja sou�asn� s aktu�ln� podm�nkou nastavenou v dialogu.
            Instance pytis.data.Operator, nebo None.
          current_row -- aktu�ln� ��dek formul��e jako instance t��dy
            'pytis.data.Row'; pokud ve formul��i nen� zvolen ��dn� ��dek, tak
            'None'
          current_field -- identifik�tor aktu�ln�ho pol��ka/sloupce.  Je-li
            'None', bude vybr�n n�jak� implicitn� sloupec.

        Vrac�: Dvojici (FILTER, CONDITION).  CONDITION je instance
        podm�nkov�ho oper�toru pou��van�ho metodami t��dy 'pytis.data.Data'.
        FILTER je flag ud�vaj�c�, zda m� b�t filtrov�n� provedeno nebo zda byl
        dialog u�ivatelem zru�en bez po�adavku na nov� filtrov�n�.

        """
        self._data = data
        self._data_filter = filter
        self._perform = False
        return super_(FilterDialog).run(self, current_row, current_field)


class SortingDialog(SFSDialog):
    """Dialog pro volbu parametr� t��d�n�.

    Metoda 'run()' vrac� specifikaci t��d�n�, kterou pou��v� formul��
    `pytis.form.LookupForm'.  Je-li dialog opu�t�n jin�m zp�sobem ne� stiskem
    tla��tka, je vr�ceno 'None'.

    """

    _OK_BUTTON = _("Set��dit")
    _CANCEL_BUTTON = _("Resetovat t��d�n�")
    _ESCAPE_BUTTON = _("Zav��t")

    _COMMIT_BUTTON = _OK_BUTTON
    
    _ASCENDENT = _("Vzestupn�")
    _DESCENDANT = _("Sestupn�")
    _NOSORTING = _("Net��dit")
    _HELP_TOPIC = 'sorting'
    
    def __init__(self, parent, columns, sorting, col=None, direction=None):
        """Inicializuj dialog.

        Argumenty:

          parent -- rodi� dialogu
          columns -- sekvence instanc� t��dy 'SFSColumn'
          sorting -- specifikace aktu�ln�ho t��d�n�, ve tvaru argumentu 'sort'
            metody 'pytis.data.Data.select()'
          col -- id implicitn�ho sloupce jako string
          direction -- implicitn� sm�r t��d�n�, jedna ze sm�rov�ch konstant
            modulu 'pytis.data'

        """
        self._parent = parent
        self._columns = columns
        self._sorting = sorting
        self._col = col
        self._direction = direction
        buttons = (self._OK_BUTTON, self._CANCEL_BUTTON, self._ESCAPE_BUTTON)
        super_(SortingDialog).__init__(self, parent, _("T��d�n�"), buttons)

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
                     tip=_("Zvolte sloupec tabulky, podle n�j� chcete t��dit"))
                colpos = position(col, columns, key=lambda c: c.id())
                if colpos:
                    colsel.SetSelection(colpos)
                # Sm�r t��d�n�
                dirsel = self._create_choice(direction_choices,
                                             tip=_("Zvolte sm�r t��d�n�"))
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
                     tip=_("Zvolte sloupec tabulky, podle n�j� chcete t��dit"))
            col = self._col
            if col is not None:
                for i in range(len(columns)):
                    if columns[i].id() == col:
                        colsel.SetSelection(i)
                        break
            # Sm�r t��d�n�
            dirsel = self._create_choice(direction_choices,
                                         tip=_("Zvolte sm�r t��d�n�"))
            if self._direction == LookupForm.SORTING_DESCENDANT:
                dirsel.SetSelection(1)
            self._selections.append((colsel, dirsel))
            # Sizer
            sizer = wx.BoxSizer()
            for w in colsel, dirsel:
                sizer.Add(w)
            big_sizer.Add(sizer)
        add_button = self._create_button(_("P�idat"), self._on_add,
                                  tip=_("P�idat sloupec sekund�rn�ho t��d�n�"))
        big_sizer.Add(add_button)
        return big_sizer

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
            # Sm�r
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


def sfs_columns(columns, data, labelfunc=FieldSpec.label):
    """Vra� sloupce vhodn� k�pou�it� v�konstruktorech SFS dialog�.

    (SFS = Search, Filter, Sort)

    Argumenty:

      columns -- sekvence instanc� t��dy 'Column' obsahuj�c� sloupce
      data -- datov� objekt, na n�j� jsou sloupce nav�z�ny
      labelfunc -- funkce jednoho argumentu (instance 'Column') vracej�c�
        n�v�t� sloupce v�dialogu

    """
    sfs_columns = []
    for c in columns:
        label = labelfunc(c)
        if data.find_column(c.id()) is None or not label:
            continue
        id = c.id()
        type_ = c.type(data)
        sfs_columns.append(SFSColumn(id, type_, label))
    return sfs_columns
