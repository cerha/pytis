# -*- coding: utf-8 -*-

# Prvky uživatelského rozhraní související s vyhledáváním
# 
# Copyright (C) 2001-2011 Brailcom, o.p.s.
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

import functools
import pytis.data
from pytis.form import *
from pytis.presentation import *
import wx


class SFSColumn(object):
    """Column specification for dialog selectors."""

    def __init__(self, id, type, label):
        """Initialize column specification.

        Arguments:

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
        """Vrať label zadaný v konstruktoru."""
        return self._label

    
class SFSDialog(GenericDialog):
    """Common ancestor of all sorting/filtering/searching dialogs."""

    _FIELD_HEIGHT = 27
    _TITLE = None
    _ESCAPE_BUTTON = _(u"Zavřít")
    _BUTTONS = (_ESCAPE_BUTTON,)
    def __init__(self, parent, columns, col=None):
        """Initialize the dialog.

        Arguments:

          parent -- wx parent of the dialog window
          columns -- a sequence of 'SFSColumn' instances
          col -- current column identifier as a string

        """
        self._parent = parent
        self._columns = tuple(columns)
        self._col = col
        super(SFSDialog, self).__init__(parent, self._TITLE, self._BUTTONS)

    def _find_column(self, cid):
        return find(cid, self._columns, key=lambda c: c.id())

    def _create_ctrl(self, constructor, *args, **kwargs):
        ctrl = constructor(self._dialog, *args, **kwargs)
        self._handle_keys(ctrl)
        return ctrl
    
    def _create_button(self, label, callback, tooltip=None, **kwargs):
        return self._create_ctrl(wx_button, label=label, callback=callback,
                                 tooltip=tooltip, height=self._FIELD_HEIGHT, **kwargs)
        
    def _create_choice(self, choices, tooltip=None, **kwargs):
        return self._create_ctrl(wx_choice, choices, height=self._FIELD_HEIGHT, **kwargs)

    def _create_text_ctrl(self, value, **kwargs):
        return self._create_ctrl(wx_text_ctrl, value, height=self._FIELD_HEIGHT, **kwargs)

    def _create_spin_ctrl(self, value, **kwargs):
        return self._create_ctrl(wx_spin_ctrl, value, height=self._FIELD_HEIGHT, **kwargs)
        
    def _create_label(self, label, **kwargs):
        panel = wx.Panel(self._dialog, -1)
        sizer = wx.BoxSizer(wx.HORIZONTAL)
        label_ctrl = wx.StaticText(panel, -1, label)
        sizer.Add(label_ctrl, border=12, flag=wx.ALIGN_CENTER_VERTICAL|wx.LEFT)
        panel.SetSizer(sizer)
        panel.SetMinSize((label_ctrl.GetSize().width+12, self._FIELD_HEIGHT))
        return panel

    def _create_content(self, sizer):
        self._controls = []
        self._create_controls()
        for i, ctrls in enumerate(self._controls):
            row = wx.BoxSizer()
            for x in ctrls:
                if x:
                    row.Add(x)
            flags = wx.ALIGN_LEFT|wx.LEFT|wx.RIGHT
            if i == 0:
                flags |= wx.TOP
            sizer.Add(row, 0, flags, 8)

    def _create_controls(self):
        pass
    
    
class SortingDialog(SFSDialog):
    """Dialog pro volbu parametrů řazení.

    Metoda 'run()' vrací specifikaci řazení, kterou používá formulář
    `pytis.form.LookupForm'.  Je-li dialog opuštěn jiným způsobem než stiskem
    tlačítka, je vráceno 'None'.

    """

    _TITLE = _(u"Řazení")
    _SORT_BUTTON = _(u"Seřadit")
    _RESET_BUTTON = _(u"Obnovit výchozí řazení")
    _BUTTONS = (_SORT_BUTTON, _RESET_BUTTON) + SFSDialog._BUTTONS
    _COMMIT_BUTTON = _SORT_BUTTON
    
    _DIRECTIONS = (pytis.data.ASCENDENT, pytis.data.DESCENDANT, None)
    _LABELS = {pytis.data.ASCENDENT: _(u"Vzestupně"),
               pytis.data.DESCENDANT: _(u"Sestupně"),
               None: _(u"Neřadit")}
    
    _HELP_TOPIC = 'sorting'
    
    def __init__(self, parent, columns, sorting, direction=None, **kwargs):
        """Initialize the dialog.

        Arguments:

          parent -- wx parent of the dialog window
          columns -- a sequence of 'SFSColumn' instances
          sorting -- current sorting specification in the form of the `sort'
            argument of 'pytis.data.Data.select()'
          direction -- default sorting direction, one of
            'pytis.data.ASCENDENT', 'pytis.data.DESCENDANT'
          kwargs -- passed to the parent class constructor

        """
        self._sorting = sorting
        self._direction = direction
        super(SortingDialog, self).__init__(parent, columns, **kwargs)

    def _create_controls(self):
        choice = self._create_choice
        for cid, dir in self._sorting or ((self._col, self._direction),):
            self._controls.append((
                choice([(c.label(), c) for c in self._columns], selected=self._find_column(cid),
                       tooltip=_(u"Zvolte sloupec, podle nějž chcete řadit")),
                choice([(self._LABELS[d], d) for d in self._DIRECTIONS], selected=dir,
                       tooltip=_(u"Zvolte směr řazení"))))

    def _create_content(self, sizer):
        super(SortingDialog, self)._create_content(sizer)
        button = self._create_button(_(u"Přidat"), self._on_add,
                                     _(u"Přidat sloupec sekundárního řazení"))
        sizer.Add(button, 0, wx.ALL|wx.CENTER, 5)

    def _customize_result(self, button_wid):
        label = self._button_label(button_wid)
        if label == self._RESET_BUTTON:
            return ()
        elif label != self._SORT_BUTTON:
            return None
        return self._selected_sorting()

    def _selected_sorting(self):
        sorting = []
        for colsel, dirsel in self._controls:
            cid = self._columns[colsel.GetSelection()].id()
            direction = self._DIRECTIONS[dirsel.GetSelection()]
            if direction is not None:
                sorting.append((cid, direction))
        return tuple(sorting)

    def _on_add(self, event):
        new = (self._columns[0].id(), pytis.data.DESCENDANT)
        self._sorting = self._selected_sorting() + (new,)
        self.rebuild()

    
class SFDialog(SFSDialog):
    """Společný základ všech vyhledávacích a filtrovacích dialogů."""

    _OPERATORS = (pytis.data.EQ,
                  pytis.data.NE,
                  pytis.data.LE,
                  pytis.data.GE,
                  pytis.data.LT,
                  pytis.data.GT)
    _LOGICAL_OPERATORS = (pytis.data.AND, pytis.data.OR)
    _WM_OPERATORS = {pytis.data.EQ: pytis.data.WM,
                     pytis.data.NE: pytis.data.NW}
    _LABELS = {pytis.data.EQ: '=',
               pytis.data.NE: '=/=', #u'\u2260',
               pytis.data.LE: '=<',
               pytis.data.GE: '>=',
               pytis.data.LT: '<',
               pytis.data.GT: '>',
               pytis.data.AND: _(u"AND"),
               pytis.data.OR:  _(u"OR")}
    # WM and EQ have the same UI ctrl, so we ignore the difference.
    _RELATIONAL_OPERATORS_MAP = {'EQ': pytis.data.EQ,
                                 'NE': pytis.data.NE,
                                 'WM': pytis.data.EQ,
                                 'NW': pytis.data.NE,
                                 'LT': pytis.data.LT,
                                 'LE': pytis.data.LE,
                                 'GT': pytis.data.GT,
                                 'GE': pytis.data.GE}
    _LOGICAL_OPERATORS_MAP = {'AND': pytis.data.AND,
                              'OR': pytis.data.OR}
    _TEXT_CTRL_SIZE = 18
    _NO_COLUMN = SFSColumn('--sfs-dlg-no-column--', pytis.data.String(),
                           '* '+_(u"hodnota")+' *')
    
    class SFConditionError(Exception):
        def __init__(self, i, ctrl, msg):
            msg = _(u"Chyba v podmínce č. %d: %s") % (i+1, msg)
            run_dialog(Error, msg)
            #ctrl.SetFocus()
            #self.focus()
            super(SFDialog.SFConditionError, self).__init__(msg)
            
    def __init__(self, parent, columns, row, condition=None, **kwargs):
        """Initialize the dialog.

        Arguments:

          parent -- wx parent of the dialog window
          columns -- a sequence of 'SFSColumn' instances
          row -- current row as a 'pytis.data.Row' instance or 'None'
          condition -- currently displayed condition as a 'pytis.data.Operator' instance.
          kwargs -- passed to the parent class constructor

        """
        self._row = row
        self._condition = condition
        self._col2_columns = (self._NO_COLUMN,) + tuple(columns)
        super(SFDialog, self).__init__(parent, columns, **kwargs)

    def _strop(self, operator, ):
        if operator.logical():
            op = ' '+ operator.name() +' '
            return '('+ op.join([self._strop(arg) for arg in operator.args()]) + ')'
        else:
            arg1, arg2 = operator.args()
            if isinstance(arg2, (pytis.data.Value, pytis.data.WMValue)):
                if isinstance(arg2.value(), unicode):
                    # Avoid the initial u for unicode strings...
                    arg2 = repr(arg2.value())[1:]
                else:
                    arg2 = repr(arg2.value())
            op = self._LABELS[self._RELATIONAL_OPERATORS_MAP[operator.name()]]
            return arg1 +' '+ op +' '+ arg2

    def _decompose_condition(self, operator, level=1):
        # Decompose nested conditions into a linear list of corresponding relational and logical
        # operators in infix notation.  Hierarchy is represented by the level of logical operators.
        if not isinstance(operator, pytis.data.Operator):
            raise Exception("Invalid condition: "+ repr(operator))
        name, args = operator.name(), operator.args()
        if name in self._LOGICAL_OPERATORS_MAP:
            op = self._LOGICAL_OPERATORS_MAP[name]
            conds = [self._decompose_condition(arg, level=level+1) for arg in args]
            return functools.reduce(lambda a, b: (a + ((op, level),)) + b, conds)
        elif name in self._RELATIONAL_OPERATORS_MAP:
            if len(args) != 2:
                raise Exception("Wrong number of arguments: "+ str(args))
            arg1, arg2 = args
            op = self._RELATIONAL_OPERATORS_MAP[name]
            col1 = self._find_column(arg1)
            if col1 is None:
                raise Exception("Invalid column: "+ arg1)
            if isinstance(arg2, basestring):
                col2 = self._find_column(arg2)
                if col2 is None:
                    raise Exception("Invalid column: "+ arg2)
                value = None
            elif isinstance(arg2, (pytis.data.WMValue, pytis.data.Value)):
                col2 = None
                value = isinstance(arg2, pytis.data.WMValue) \
                        and arg2.value() or arg2.export()
            else:
                raise Exception("Invalid operand type: "+ repr(arg))
            return (op, col1, col2, value),
        else:
            raise Exception("Unsupported operator: "+ name)

    def _create_controls(self):
        choice, spin, label, field, button = self._create_choice, self._create_spin_ctrl, \
            self._create_label, self._create_text_ctrl, self._create_button
        # Construct the ui controls based on the current condition.
        def create_logical_operator(i, n, operator, level):
            return (
                choice([(self._LABELS[op], op) for op in self._LOGICAL_OPERATORS],
                       selected=operator,
                       tooltip=_(u"Zvolte způsob spojení s předchozími podmínkami")),
                label(_(u"Váha operátoru:")),
                spin(level, length=4, tooltip=_(u"Zvolte váhu logického operátoru.")))
        def create_relational_operator(i, n, operator, col1, col2, value):
            return (
                choice([(c.label(), c) for c in self._columns], selected=col1,
                       on_change=lambda e: self._on_selection_change(i),
                       tooltip=_(u"Zvolte sloupec tabulky")),
                choice([(self._LABELS[op], op) for op in self._OPERATORS], selected=operator,
                       tooltip=_(u"Zvolte operátor")),
                choice([(c.label(), c) for c in self._col2_columns], selected=col2,
                       on_change=lambda e: self._on_selection_change(i),
                       tooltip=_(u"Zvolte s čím má být hodnota porovnávána")),
                field(value, length=self._TEXT_CTRL_SIZE,
                      tooltip=_(u"Zapište hodnotu podmínkového výrazu")),
                button(_(u"Nasát"), lambda e: self._on_suck(i),
                       _(u"Načíst hodnotu aktivní buňky"),
                       enabled=self._row is not None),
                button(_(u"Vymazat"), lambda e: self._on_clear(i),
                       _(u"Vymazat obsah podmínky")),
                button(_(u"Odebrat"), lambda e: self._on_remove(i),
                       _(u"Zrušit tuto podmínku"), enabled=n > 1))
        c = self._find_column(self._col) or self._columns[0]
        empty = pytis.data.EQ(c.id(), pytis.data.Value(c.type(), None))
        #print "===", self._strop(self._condition or empty)
        try:
            operators = self._decompose_condition(self._condition or empty)
        except Exception as e:
            run_dialog(Warning, _(u"Nepodařilo se rozložit podmínkový výraz:") +" "+ str(e))
            operators = self._decompose_condition(empty)
        for i, items in enumerate(operators):
            if len(items) == 2:
                self._controls.append(create_logical_operator(i, len(operators), *items))
            else:
                self._controls.append(create_relational_operator(i, len(operators), *items))
                self._on_selection_change(i)
        wval = self._controls[-1][3]
        if wval.IsEnabled():
            self._want_focus = wval

    def _create_content(self, sizer):
        super(SFDialog, self)._create_content(sizer)
        choice, button = self._create_choice, self._create_button
        buttons = [
            button(_(u"Přidat AND"), lambda e: self._on_add(),
                   tooltip=_(u"Přidat novou podmínku v konjunkci (a zároveň)")),
            button(_(u"Přidat OR"), lambda e: self._on_add(or_=True),
                   tooltip=_(u"Přidat novou podmínku v disjunkci (a nebo)")),
            button(_(u"Odebrat vše"), lambda e: self._on_reset(),
                   tooltip=_(u"Zrušit všechny stávající podmínky"))]
        bsizer = wx.BoxSizer(wx.HORIZONTAL)
        for b in buttons:
            bsizer.Add(b, 0, wx.RIGHT, 10)
        sizer.Add(bsizer, 0, wx.ALL|wx.CENTER, 5)

    def _selected_condition(self, omit=None):
        # Construct the operator from the current dialog ui controls.
        def logical_operator(i):
            wop, wlabel, wweight = self._controls[i]
            op = self._LOGICAL_OPERATORS[wop.GetSelection()]
            weight = wweight.GetValue()
            return (op, weight)
        def relational_operator(i):
            wcol1, wop, wcol2, wval, b1, b2, b3 = self._controls[i]
            col1 = self._columns[wcol1.GetSelection()]
            op = self._OPERATORS[wop.GetSelection()]
            col2 = self._col2_columns[wcol2.GetSelection()]
            if col2 is not self._NO_COLUMN:
                arg2 = col2.id()
                for basetype in (pytis.data.String, pytis.data.Number, pytis.data.DateTime,
                                 pytis.data.Boolean, pytis.data.Binary):
                    if isinstance(col1.type(), basetype) and not isinstance(col2.type(), basetype):
                        raise self.SFConditionError(i, wcol2, _(u"Neslučitelné typy %s a %s") %
                                                    (col1.type().__class__.__name__,
                                                     col2.type().__class__.__name__))
            elif isinstance(col1.type(), pytis.data.Binary):
                if wval.GetValue():
                    raise self.SFConditionError(i, wval,
                                     _(u"Binární sloupec lze testovat pouze na prázdnou hodnotu"))
                elif op not in (pytis.data.EQ, pytis.data.NE):
                    raise self.SFConditionError(i, wop,
                                     _(u"Binární sloupec lze testovat pouze na rovnost či nerovnost"))
                arg2 = pytis.data.Value(col1.type(), None)
            else:
                val = wval.GetValue()
                if op in self._WM_OPERATORS and (val.find('*') >= 0 or val.find('?') >= 0):
                    op = self._WM_OPERATORS[op]
                    value, err = col1.type().wm_validate(val)
                else:
                    kwargs = dict(strict=False)
                    if isinstance(col1.type(), pytis.data.Boolean):
                        kwargs['extended'] = True
                    value, err = col1.type().validate(val, **kwargs)
                if err:
                    raise self.SFConditionError(i, wval, err.message())
                arg2 = value
            return op(col1.id(), arg2)
        def apply_logical_operator(operator, operators, level):
            # Apply the logical operators at given level to its operands and return the reduced
            # list of top-level operators.
            result = []
            operands = []
            for i in range(1, len(operators), 2):
                op, weight = operators[i]
                operand = operators[i-1]
                if weight == level and op == operator:
                    operands.append(operand)
                else:
                    if operands:
                        operands.append(operand)
                        operand = operator(*operands)
                        operands = []
                    result.extend([operand, (op, weight)])
            op, weight = operators[-2]
            operand = operators[-1]
            if op == operator and weight == level:
                operands.append(operand)
            else:
                result.append(operand)
            if operands:
                result.append(operator(*operands))
            return result
        operators = []
        weights = []
        for i in range(len(self._controls)):
            # Omit the relational operator and the logical operator above, or below for the first
            # operator.
            if omit is None or i not in (omit, (omit == 0 and 1 or omit-1)):
                if i % 2 == 1:
                    op, weight = logical_operator(i)
                    if weight not in weights:
                        weights.append(weight)
                    operators.append((op, weight))
                else:
                    operators.append(relational_operator(i))
        weights.sort()
        weights.reverse()
        for weight in weights:
            operators = apply_logical_operator(pytis.data.AND, operators, weight)
            if len(operators) > 1:
                operators = apply_logical_operator(pytis.data.OR, operators, weight)
        #print "***", self._strop(operators[0])
        assert len(operators) == 1
        return operators[0]

    def _on_selection_change(self, i):
        wcol1, wop, wcol2, wval, bsuck, bclear, bremove = self._controls[i]
        enabled = wcol2.GetSelection() == 0
        wval.Enable(enabled)
        bsuck.Enable(enabled and self._row is not None)

    def _on_clear(self, i):
        wcol1, wop, wcol2, wval = self._controls[i][:4]
        wop.SetSelection(0)
        wcol2.SetSelection(0)
        wval.SetValue('')
        self._on_selection_change(i)

    def _on_suck(self, i):
        wcol1, wop, wcol2, wval = self._controls[i][:4]
        col = self._columns[wcol1.GetSelection()]
        v = self._row[col.id()].export()
        if is_sequence(v):
            v = v[0]
        wval.SetValue(v)

    def _on_remove(self, i):
        try:
            condition = self._selected_condition(omit=i)
        except self.SFConditionError:
            pass
        else:
            self._condition = condition
            self.rebuild()
    
    def _on_add(self, or_=False):
        try:
            condition = self._selected_condition()
        except self.SFConditionError:
            pass
        else:
            op = or_ and pytis.data.OR or pytis.data.AND
            c = self._find_column(self._col) or self._columns[0]
            v = pytis.data.Value(c.type(), None)
            self._condition = op(condition, pytis.data.EQ(c.id(), v))
            self.rebuild()

    def _on_reset(self):
        self._condition = None
        self.rebuild()

            
class SearchDialog(SFDialog):
    """Dialog for manipulation of the current searching condition.

    The 'run()' method of this dialog returns a pair (DIRECTION, CONDITION).
    
    DIRECTION is the selected search direction.  The value can be either
    'pytis.data.FORWARD', 'pytis.data.BACKWARD' or 'None'.  'None' means that
    the search should not be performed (the dialog was escaped), the other two
    values indicate, that next record should be located in given direction.
    
    CONDITION is the selected search condition as a 'pytis.data.Operator'
    instance.

    """
    _NEXT_BUTTON = _(u"Další")
    _PREVIOUS_BUTTON = _(u"Předchozí")
    _BUTTONS = (_NEXT_BUTTON, _PREVIOUS_BUTTON) + SFSDialog._BUTTONS
    _COMMIT_BUTTON = _NEXT_BUTTON    
    _TITLE = _(u"Hledání")
    _HELP_TOPIC = 'searching'

    def __init__(self, *args, **kwargs):
        self._direction = None
        return super(SearchDialog, self).__init__(*args, **kwargs)
    
    def _on_button(self, event):
        mapping = {self._NEXT_BUTTON: pytis.data.FORWARD,
                   self._PREVIOUS_BUTTON: pytis.data.BACKWARD}
        direction = mapping.get(self._button_label(event.GetId()))
        if direction is not None:
            try:
                self._condition = self._selected_condition()
            except self.SFConditionError:
                return
        if direction is not None:
            self._direction = direction
        return super(SearchDialog, self)._on_button(event)
        
    def _customize_result(self, button_wid):
        return self._direction, self._condition


class FilterDialog(SFDialog):
    """Dialog for manipulation of the filtering condition and aggregations.

    This dialog edits the current filtering condition.  In addition it has a
    simple aggregation panel, where the user can display the result of a
    selected aggregation function.  These aggregations work with the data
    filtered by the current selected condition without the need to actually
    perform the filter to the underlying form.

    The 'run()' method of this dialog returns a pair (PERFORM, CONDITION).
    
    PERFORM is a boolean flag indicating whether the CONDITION should be
    applied to the underlying form or not.  It is True when the user presses
    the ``Filter'' or ``Unfilter'' button and False if the user cancels the
    dialog.
    
    CONDITION is the current selected search condition as a
    'pytis.data.Operator' instance or None.  'None' is used when the user
    wishes to unfilter the underlying form.
    
    """
    _FILTER_BUTTON = _(u"Filtrovat")
    _UNFILTER_BUTTON = _(u"Zrušit filtr")
    _BUTTONS = (_FILTER_BUTTON, _UNFILTER_BUTTON) + SFSDialog._BUTTONS
    _COMMIT_BUTTON = _FILTER_BUTTON
    _AGG_OPERATORS = (pytis.data.Data.AGG_COUNT,
                      pytis.data.Data.AGG_MIN,
                      pytis.data.Data.AGG_MAX,
                      pytis.data.Data.AGG_SUM,
                      pytis.data.Data.AGG_AVG)
    _AGG_LABELS = {pytis.data.Data.AGG_COUNT: _(u"Počet"),
                   pytis.data.Data.AGG_MIN:   _(u"Minimum"),
                   pytis.data.Data.AGG_MAX:   _(u"Maximum"),
                   pytis.data.Data.AGG_SUM:   _(u"Součet"), 
                   pytis.data.Data.AGG_AVG:   _(u"Průměr")}
    _TITLE = _(u"Filtrování")
    _HELP_TOPIC = 'filtering'

    def __init__(self, parent, columns, row, compute_aggregate, **kwargs):
        """Initialize the dialog.

        Arguments:

          parent -- wx parent of the dialog window
          columns -- a sequence of 'SFSColumn' instances
          row -- current row as a 'pytis.data.Row' instance or 'None'
          compute_aggregate -- a callable object which takes three arguments
            (OPERATION, COLUMN_ID, CONDITION) and returns the result of the
            aggregation OPERATION on COLUMN_ID with given CONDITION as a
            'pytis.data.Value' instance.
          kwargs -- passed to the parent class constructor

        """
        self._compute_aggregate = compute_aggregate
        self._perform = False
        super(FilterDialog, self).__init__(parent, columns, row, **kwargs)

    def _create_content(self, sizer):
        super(FilterDialog, self)._create_content(sizer)
        choice, field, button = self._create_choice, self._create_text_ctrl, self._create_button
        self._agg_controls = (
            choice([(c.label(), c) for c in self._columns],
                   tooltip=_(u"Zvolte sloupec pro agregaci")),
            choice([(self._AGG_LABELS[op], op) for op in self._AGG_OPERATORS],
                   tooltip=_(u"Zvolte agregační funkci")),
            field(None, length=24, readonly=True,
                  tooltip=_(u"Zobrazení výsledku agregační funkce")),
            button(_(u"Zjistit"), self._on_compute_aggregate,
                   tooltip=_(u"Zobraz výsledek zvolené agrekační funkce")))
        box = wx.StaticBox(self._dialog, -1, _(u"Agregační funkce:"))
        boxsizer = wx.StaticBoxSizer(box, wx.HORIZONTAL)
        for ctrl in self._agg_controls:
            boxsizer.Add(ctrl)
        sizer.Add(boxsizer, 0, wx.ALL|wx.CENTER, 5)


    def _on_compute_aggregate(self, event):
        try:
            condition = self._selected_condition()
        except self.SFConditionError:
            pass
        else:
            wcol, wop, wresult, wbutton = self._agg_controls
            op = self._AGG_OPERATORS[wop.GetSelection()]
            col = self._columns[wcol.GetSelection()]
            if op != pytis.data.Data.AGG_COUNT and not isinstance(col.type(), pytis.data.Number):
                # TODO: We should also support Date and maybe other types, but first it must be
                # implemented in the data interface.
                run_dialog(Error, _(u"Tato operaca není pro daný typ sloupce podporována."))
                v = ''
            else:
                result = self._compute_aggregate(op, col.id(), condition)
                if result is not None:
                    v = result.export()
                else:
                    v = ''
            wresult.SetValue(v)

    def _on_button(self, event):
        label = self._button_label(event.GetId())
        if label == self._FILTER_BUTTON:
            try:
                self._condition = self._selected_condition()
            except self.SFConditionError:
                return
            self._perform = True
        elif label == self._UNFILTER_BUTTON:
            self._perform = True
            self._condition = None
        return super(FilterDialog, self)._on_button(event)
        
    def _customize_result(self, button_wid):
        return self._perform, self._condition


def sfs_columns(fields, data, labelfunc=Field.label):
    """Return a list of 'SFSColumn' instances for SFS dialog constructor.

    (SFS = Search, Filter, Sort)

    Argumenty:

      fields -- sequence of 'Field' instances from specification
      data -- related data object
      labelfunc -- function of one argument ('Field' instance) returning the
        column label used in the dialog or None to exclude the field from the
        dialog.

    """
    columns = []
    for f in fields:
        id = f.id()
        label = labelfunc(f)
        column = data.find_column(id)
        if column is not None and label and data.permitted(id, pytis.data.Permission.VIEW):
            columns.append(SFSColumn(id, column.type(), label))
    columns.sort(key=lambda c: c.label())
    return columns
