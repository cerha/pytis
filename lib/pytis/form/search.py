# -*- coding: iso-8859-2 -*-

# Prvky uživatelského rozhraní související s vyhledáváním
# 
# Copyright (C) 2001-2007 Brailcom, o.p.s.
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
    _ESCAPE_BUTTON = _("Zavřít")
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
    
    def _create_button(self, label, callback, tooltip=None, **kwargs):
        return wx_button(self._dialog, label=label, callback=callback,
                         tooltip=tooltip, height=self._FIELD_HEIGHT, **kwargs)
        
    def _create_choice(self, choices, tooltip=None, label=identity,
                       selected=None, on_change=None):
        ch = wx.Choice(self._dialog, -1, choices=[label(x) for x in choices])
        ch.SetSelection(0)
        ch.SetMinSize((ch.GetSize().width, self._FIELD_HEIGHT))
        if tooltip is not None:
            ch.SetToolTipString(unicode(tooltip))
        if selected:
            ch.SetSelection(list(choices).index(selected))
        if on_change:
            wx_callback(wx.EVT_CHOICE, ch, ch.GetId(), on_change)
        return ch

    def _create_text_ctrl(self, size, value=None, tooltip=None,
                          readonly=False, enabled=None):
        style = readonly and wx.TE_READONLY or 0
        t = wx.TextCtrl(self._dialog, -1, style=style)
        t.SetMinSize((dlg2px(t, 4*size), self._FIELD_HEIGHT))
        if value is not None:
            t.SetValue(value)
        if tooltip is not None:
            t.SetToolTipString(unicode(tooltip))
        if enabled is not None:
            t.Enable(enabled)
        return t

    def _create_content(self, sizer):
        self._controls = []
        self._create_controls()
        for ctrls in self._controls:
            row = wx.BoxSizer()
            for x in ctrls:
                if x:
                    row.Add(x)
            sizer.Add(row, 0, wx.ALIGN_RIGHT|wx.ALL, 5)

    def _create_controls(self):
        pass
    
    
class SortingDialog(SFSDialog):
    """Dialog pro volbu parametrů řazení.

    Metoda 'run()' vrací specifikaci řazení, kterou používá formulář
    `pytis.form.LookupForm'.  Je-li dialog opuštěn jiným způsobem než stiskem
    tlačítka, je vráceno 'None'.

    """

    _TITLE = _("Řazení")
    _SORT_BUTTON = _("Seřadit")
    _RESET_BUTTON = _("Obnovit výchozí řazení")
    _BUTTONS = (_SORT_BUTTON, _RESET_BUTTON) + SFSDialog._BUTTONS
    _COMMIT_BUTTON = _SORT_BUTTON
    
    _DIRECTIONS = (pytis.data.ASCENDENT, pytis.data.DESCENDANT, None)
    _LABELS = {pytis.data.ASCENDENT: _("Vzestupně"),
               pytis.data.DESCENDANT: _("Sestupně"),
               None: _("Neřadit")}
    
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
            # Sloupce
            self._controls.append((
                choice(self._columns, selected=self._find_column(cid),
                       label=lambda c: c.label(),
                       tooltip=_("Zvolte sloupec, podle nějž chcete řadit")),
                choice(self._DIRECTIONS, selected=dir,
                       label=lambda d: self._LABELS[d],
                       tooltip=_("Zvolte směr řazení"))))

    def _create_content(self, sizer):
        super(SortingDialog, self)._create_content(sizer)
        button = self._create_button(_("Přidat"), self._on_add,
                                     _("Přidat sloupec sekundárního řazení"))
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
                  pytis.data.LE,
                  pytis.data.GE,
                  pytis.data.LT,
                  pytis.data.GT,
                  pytis.data.NE)
    _LOGICAL_OPERATORS = (pytis.data.AND, pytis.data.OR)
    _WM_OPERATORS = {pytis.data.EQ: pytis.data.WM,
                     pytis.data.NE: pytis.data.NW}
    _LABELS = {pytis.data.EQ: '=',
               pytis.data.LE: '=<',
               pytis.data.GE: '>=',
               pytis.data.LT: '<',
               pytis.data.GT: '>',
               pytis.data.NE: '=/=',
               pytis.data.AND: _("AND"),
               pytis.data.OR:  _("OR")}
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
                           '* '+_("hodnota")+' *')
    
    class SFConditionError(Exception):
        pass

    def __init__(self, parent, columns, row, condition=None,
                 conditions=(), **kwargs):
        """Initialize the dialog.

        Arguments:

          parent -- wx parent of the dialog window
          columns -- a sequence of 'SFSColumn' instances
          row -- current row as a 'pytis.data.Row' instance or 'None'
          condition -- current condition as a 'pytis.data.Operator' instance.
            This condition will be preselected in the dialog.  The current
            implementation, can only display a certainly structured condition.
            It is safe to use a condition obtained from the previous dialog
            call.
          conditions -- predefined search/filtering conditions as a sequence of
            'Condition' instances which may be loaded into a dialog.  Same
            limitations as for 'condition' apply.
          kwargs -- passed to the parent class constructor

        """
        assert isinstance(conditions, tuple)
        self._row = row
        self._condition = condition
        self._conditions = list(conditions)
        self._loaded_condition = None
        self._col2_columns = (self._NO_COLUMN,) + tuple(columns)
        super(SFDialog, self).__init__(parent, columns, **kwargs)

    def _create_controls(self):
        # Construct the ui controls based on the current condition.
        def decompose_condition(operator, logical_operation=None):
            # Decompose nested conditions into a list of corresponding operator
            # functions and their logical pairing.
            if not isinstance(operator, pytis.data.Operator):
                raise Exception("Invalid condition: "+ repr(operator))
            name, args = operator.name(), operator.args()
            if len(args) != 2:
                # This really applies also for logical operators!
                raise Exception("Wrong number of arguments: "+ str(args))
            arg1, arg2 = args
            if self._LOGICAL_OPERATORS_MAP.has_key(name):
                op = self._LOGICAL_OPERATORS_MAP[name]
                return decompose_condition(arg1, logical_operation) + \
                       decompose_condition(arg2, op)
            elif self._RELATIONAL_OPERATORS_MAP.has_key(name):
                op = self._RELATIONAL_OPERATORS_MAP[name]
                col1 = self._find_column(arg1)
                if col1 is None:
                    raise Exception("Invalid column: "+ arg1)
                if isinstance(arg2, str):
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
                return ((logical_operation, op, col1, col2, value), )
            else:
                raise Exception("Unsupported operator: "+ name)
        def create_controls(i, n, log_op, operator, col1, col2, value):
            choice, field, button = self._create_choice, \
                                    self._create_text_ctrl, self._create_button
            return (
                log_op and \
                choice(self._LOGICAL_OPERATORS, selected=log_op,
                       label=lambda o: self._LABELS[o],
                  tooltip=_("Zvolte způsob spojení s předchozími podmínkami")),
                choice(self._columns, selected=col1,
                       label=lambda c: c.label(),
                       on_change=lambda e: self._on_selection_change(i),
                       tooltip=_("Zvolte sloupec tabulky")),
                choice(self._OPERATORS, selected=operator,
                       label=lambda o: self._LABELS[o],
                       tooltip=_("Zvolte operátor")),
                choice(self._col2_columns, selected=col2,
                       label=lambda c: c.label(),
                       on_change=lambda e: self._on_selection_change(i),
                       tooltip=_("Zvolte s čím má být hodnota porovnávána")),
                field(self._TEXT_CTRL_SIZE, value,
                      tooltip=_("Zapište hodnotu podmínkového výrazu")),
                button(_("Nasát"), lambda e: self._on_suck(i),
                       _("Načíst hodnotu aktivní buňky"),
                       enabled=self._row is not None),
                button(_("Vymazat"), lambda e: self._on_clear(i),
                       _("Vymazat obsah podmínky")),
                button(_("Odebrat"), lambda e: self._on_remove(i),
                       _("Zrušit tuto podmínku"), enabled=n > 1))
        c = self._find_column(self._col) or self._columns[0]
        empty = pytis.data.EQ(c.id(), pytis.data.Value(c.type(), None))
        try:
            conditions = decompose_condition(self._condition  or empty)
        except Exception, e:
            run_dialog(Warning, (_("Nepdařilo se rozložit podmínkový výraz:")+
                                 " "+str(e)))
            conditions = decompose_condition(empty)
        for i, items in enumerate(conditions):
            self._controls.append(create_controls(i, len(conditions), *items))
            self._on_selection_change(i)
        wval = self._controls[-1][4]
        if wval.IsEnabled():
            self._want_focus = wval

    def _create_content(self, sizer):
        super(SFDialog, self)._create_content(sizer)
        choice, button = self._create_choice, self._create_button
        buttons = [
            button(_("Přidat AND"), lambda e: self._on_add(),
                   tooltip=_("Přidat novou podmínku v konjunkci")),
            button(_("Přidat OR"), lambda e: self._on_add(or_=True),
                   tooltip=_("Přidat novou podmínku v disjunkci")),
            button(_("Odebrat vše"), lambda e: self._on_reset(),
                   tooltip=_("Přidat novou podmínku v disjunkci"))]
        if self._conditions:
            buttons.append(
                button(_("Uložit"), lambda e: self._on_save(),
                       tooltip=_("Uložit stávající výběr jako pojmenovanou "
                                 "podmínku")))
            self._saved_condition_controls = (
                choice(self._conditions, label=lambda c: c.name(),
                       selected=self._loaded_condition,
                       on_change=lambda e: self._on_saved_selection_change(),
                       tooltip=_("Vyberte jednu z pojmenovaných podmínek.")),
                button(_("Načíst"), lambda e: self._on_load(),
                       tooltip=_("Nahradit stávající výběr zvolenou "
                                 "podmínkou")),
                button(_("Smazat"), lambda e: self._on_remove_saved(),
                       tooltip=_("Smazat zvolenou podmínkou")),
                button(_("Aktualizovat"), lambda e: self._on_update_saved(),
                       tooltip=_("Uložit stávající výběr pod zvoleným názvem")))
            self._on_saved_selection_change()
        else:
            self._saved_condition_controls = ()
        bsizer = wx.BoxSizer(wx.HORIZONTAL)
        for b in buttons:
            bsizer.Add(b, 0, wx.RIGHT, 10)
        for x in self._saved_condition_controls:
            bsizer.Add(x)
        sizer.Add(bsizer, 0, wx.ALL|wx.CENTER, 5)

    def _selected_condition(self, omit=None):
        # Construct the operator from the current dialog ui controls.
        def quit(i, ctrl, msg):
            msg = _("Chyba v podmínce č. %d: %s") % (i+1, msg)
            run_dialog(Error, msg)
            #ctrl.SetFocus()
            #self.focus()
            raise self.SFConditionError(msg)
        condition = None
        for i, controls in enumerate(self._controls):
            if i == omit:
                continue
            wlop, wcol1, wop, wcol2, wval, b1, b2, b3 = controls
            lop = wlop and self._LOGICAL_OPERATORS[wlop.GetSelection()]
            col1 = self._columns[wcol1.GetSelection()]
            op = self._OPERATORS[wop.GetSelection()]
            col2 = self._col2_columns[wcol2.GetSelection()]
            if col2 is not self._NO_COLUMN:
                arg2 = col2.id()
                for basetype in (pytis.data.String, pytis.data.Number,
                                 pytis.data.DateTime, pytis.data.Boolean,
                                 pytis.data.Binary):
                    if isinstance(col1.type(), basetype) \
                           and not isinstance(col2.type(), basetype):
                        quit(i, wcol2, _("Neslučitelné typy %s a %s") %
                             (col1.type().__class__.__name__,
                              col2.type().__class__.__name__))
            else:
                val = wval.GetValue()
                if self._WM_OPERATORS.has_key(op) and \
                       (val.find('*') >= 0 or val.find('?') >= 0):
                    op = self._WM_OPERATORS[op]
                    value, err = col1.type().wm_validate(val)
                else:
                    kwargs = dict(strict=False)
                    if isinstance(col1.type(), pytis.data.Binary):
                        if val:
                            quit(i, wval, _("Binární sloupec lze testovat "
                                            "pouze na prázdnou hodnotu"))
                        else:
                            val = None
                        if op not in (pytis.data.EQ, pytis.data.NE):
                            quit(i, wop, _("Binární sloupec lze testovat "
                                           "pouze na rovnost či nerovnost"))
                    elif isinstance(col1.type(), pytis.data.Boolean):
                        kwargs = dict(extended=True)
                    value, err = col1.type().validate(val, **kwargs)
                if err:
                    quit(i, wval, err.message())
                arg2 = value
            subcondition = op(col1.id(), arg2)
            if condition is None:
                condition = subcondition
            else:
                condition = lop(condition, subcondition)
        return condition

    def _on_selection_change(self, i):
        wcol1, wop, wcol2, wval, bsuck, bclear = self._controls[i][1:7]
        enabled = wcol2.GetSelection() == 0
        wval.Enable(enabled)
        bsuck.Enable(enabled and self._row is not None)
        
    def _on_clear(self, i):
        wcol1, wop, wcol2, wval = self._controls[i][1:5]
        wop.SetSelection(0)
        wcol2.SetSelection(0)
        wval.SetValue('')
        self._on_selection_change(i)

    def _on_suck(self, i):
        wcol1, wop, wcol2, wval = self._controls[i][1:5]
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
        self._loaded_condition = None
        self.rebuild()

    def _on_save(self):
        try:
            self._condition = condition = self._selected_condition()
        except SFConditionError:
            return
        name = run_dialog(InputDialog, title=_("Uložit podmínku pod jménem"),
                          prompt="Název:", input_width=40)
        if name:
            self._conditions.append(Condition(name, condition, fixed=False))
            self._loaded_condition = self._conditions[-1]
            self.rebuild()
        
    def _on_saved_selection_change(self):
        sel, bload, bremove, bupdate = self._saved_condition_controls
        cond = self._conditions[sel.GetSelection()]
        for b in (bremove, bupdate):
            b.Enable(not cond.fixed())
        
    def _on_load(self):
        i = self._saved_condition_controls[0].GetSelection()
        self._loaded_condition = cond = self._conditions[i]
        self._condition = cond.condition()
        self.rebuild()

    def _on_remove_saved(self):
        i = self._saved_condition_controls[0].GetSelection()
        cond = self._conditions[i]
        if cond.fixed():
            return
        msg = _("Opravdu chcete smazat uloženou podmínku '%s'?") % cond.name()
        if run_dialog(Question, msg, title=_("Mazání uložené podmínky")):
            self._loaded_condition = None
            del self._conditions[i]
            self.rebuild()
        
    def _on_update_saved(self):
        i = self._saved_condition_controls[0].GetSelection()
        cond = self._conditions[i]
        if cond.fixed():
            return
        try:
            self._condition = self._selected_condition()
        except SFConditionError:
            return
        msg = _("Opravdu přepsat uloženou podmínku\n"
                "'%s' aktuálním výběrem?") % cond.name()
        if run_dialog(Question, msg, title=_("Přepis uložené podmínky")):
            self._conditions[i] = self._loaded_condition = \
                     Condition(cond.name(), self._condition, fixed=False)
            self.rebuild()
        
            
class SearchDialog(SFDialog):
    """Dialog for manipulation of the current searching condition.

    The 'run()' method of this dialog returns a triple
    (DIRECTION, CONDITION, CONDITIONS).
    
    DIRECTION is the selected search direction.  The value can be either
    'pytis.data.FORWARD', 'pytis.data.BACKWARD' or 'None'.  'None' means that
    the search should not be performed (the dialog was escaped), the other two
    values indicate, that next record should be located in given direction.
    
    CONDITION is the selected search condition as a 'pytis.data.Operator'
    instance.

    CONDITIONS is the sequence of named conditions as 'Condition' instances.
    This list may have been modified by the user and thus may be different than
    the list passed to the constructor.  Passing it back allows the application
    to save it.
    
    """
    _NEXT_BUTTON = _("Další")
    _PREVIOUS_BUTTON = _("Předchozí")
    _BUTTONS = (_NEXT_BUTTON, _PREVIOUS_BUTTON) + SFSDialog._BUTTONS
    _COMMIT_BUTTON = _NEXT_BUTTON    
    _TITLE = _("Hledání")
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
        return self._direction, self._condition, self._conditions


class FilterDialog(SFDialog):
    """Dialog for manipulation of the filtering condition and aggregations.

    This dialog edits the current filtering condition.  In addition it has a
    simple aggregation panel, where the user can display the result of a
    selected aggregation function.  These aggregations work with the data
    filtered by the current selected condition without the need to actually
    perform the filter to the underlying form.

    The 'run()' method of this dialog returns a triple
    (PERFORM, CONDITION, CONDITIONS).
    
    PERFORM is a boolean flag indicating whether the CONDITION should be
    applied to the underlying form or not.  It is True when the user presses
    the ``Filter'' or ``Unfilter'' button and False if the user cancels the
    dialog.
    
    CONDITION is the current selected search condition as a
    'pytis.data.Operator' instance or None.  'None' is used when the user
    wishes to unfilter the underlying form.
    
    CONDITIONS is the sequence of named conditions as 'Condition' instances.
    This list may have been modified byt the user and thus may be different
    than the list passed to the constructor.  Passing it back allows the
    application to save it.

    """
    _FILTER_BUTTON = _("Filtrovat")
    _UNFILTER_BUTTON = _("Zrušit filtr")
    _BUTTONS = (_FILTER_BUTTON, _UNFILTER_BUTTON) + SFSDialog._BUTTONS
    _COMMIT_BUTTON = _FILTER_BUTTON
    _AGG_OPERATORS = (pytis.data.Data.AGG_COUNT,
                      pytis.data.Data.AGG_MIN,
                      pytis.data.Data.AGG_MAX,
                      pytis.data.Data.AGG_SUM,
                      pytis.data.Data.AGG_AVG)
    _AGG_LABELS = {pytis.data.Data.AGG_COUNT: _("Počet"),
                   pytis.data.Data.AGG_MIN:   _("Minimum"),
                   pytis.data.Data.AGG_MAX:   _("Maximum"),
                   pytis.data.Data.AGG_SUM:   _("Součet"), 
                   pytis.data.Data.AGG_AVG:   _("Průměr")}
    _TITLE = _("Filtrování")
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
        choice, field, button = self._create_choice, \
                                self._create_text_ctrl, self._create_button
        self._agg_controls = (
            choice(self._columns, label=lambda c: c.label(),
                   tooltip=_("Zvolte sloupec pro agregaci")),
            choice(self._AGG_OPERATORS, label=lambda o: self._AGG_LABELS[o],
                   tooltip=_("Zvolte agregační funkci")),
            field(24, readonly=True,
                  tooltip=_("Zobrazení výsledku agregační funkce")),
            button(_("Zjistit"), self._on_compute_aggregate,
                   tooltip=_("Zobraz výsledek zvolené agrekační funkce")))
        box = wx.StaticBox(self._dialog, -1, _("Agregační funkce:"))
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
        return self._perform, self._condition, self._conditions


def sfs_columns(columns, data, labelfunc=FieldSpec.label):
    """Vrať sloupce vhodné k použití v konstruktorech SFS dialogů.

    (SFS = Search, Filter, Sort)

    Argumenty:

      columns -- sekvence instancí třídy 'Column' obsahující sloupce
      data -- datový objekt, na nějž jsou sloupce navázány
      labelfunc -- funkce jednoho argumentu (instance 'Column') vracející
        návěští sloupce v dialogu

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
