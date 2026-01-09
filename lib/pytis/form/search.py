# -*- coding: utf-8 -*-

# Copyright (C) 2018-2026 Tomáš Cerha <t.cerha@gmail.com>
# Copyright (C) 2001-2018 OUI Technology Ltd.
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
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

"""User interface elements related to searching.

The module provides support for dialogs with functionality more or less
corresponding to a basic search dialog.  The core is the common base class
'Dialog', which is intended to be abstract.  Concrete dialog classes for
specific use cases are derived from it.

"""
from __future__ import print_function
from past.builtins import basestring
from builtins import range

import functools
import wx

import pytis.data
from pytis.api import app
from pytis.presentation import Field
from pytis.util import find

from .dialog import GenericDialog
from .screen import wx_button, wx_choice, wx_spin_ctrl, wx_text_ctrl, field_size

_ = pytis.util.translations('pytis-wx')

unistr = type(u'')  # Python 2/3 transition hack.


class SFSColumn(object):
    """Column specification for dialog selectors."""

    def __init__(self, id, type, label):
        """Initialize column specification.

        Arguments:

          id -- column data identifier as a string
          type -- column data type as an instance of
            'pytis.data.types_.Type'
          label -- column name presented to the user as a string

        """
        self._id = id
        self._type = type
        self._label = label

    def id(self):
        """Return the id given in the constructor."""
        return self._id

    def type(self):
        """Return the type given in the constructor."""
        return self._type

    def label(self):
        """Return the label given in the constructor."""
        return self._label


class SFSDialog(GenericDialog):
    """Common ancestor of all sorting, filtering, and searching dialogs."""

    _TITLE = None
    BUTTON_CLOSE = GenericDialog.Button(_("Close"), icon='cancel', value=None)
    _BUTTONS = (BUTTON_CLOSE,)

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
        super(SFSDialog, self).__init__(parent, self._TITLE)

    def _find_column(self, cid):
        return find(cid, self._columns, key=lambda c: c.id())

    def _create_ctrl(self, constructor, *args, **kwargs):
        parent = kwargs.pop('parent', self._panel)
        kwargs['height'] = kwargs.get('height', field_size(parent, 1, 1)[1])
        ctrl = constructor(parent, *args, **kwargs)
        self._handle_keys(ctrl)
        return ctrl

    def _create_button(self, label, callback, **kwargs):
        return self._create_ctrl(wx_button, label=label, callback=callback, **kwargs)

    def _create_choice(self, choices, tooltip=None, **kwargs):
        return self._create_ctrl(wx_choice, choices, **kwargs)

    def _create_text_ctrl(self, value, **kwargs):
        return self._create_ctrl(wx_text_ctrl, value, **kwargs)

    def _create_spin_ctrl(self, value, **kwargs):
        return self._create_ctrl(wx_spin_ctrl, value, **kwargs)

    def _create_label(self, label, **kwargs):
        panel = wx.Panel(self._panel, -1)
        sizer = wx.BoxSizer(wx.HORIZONTAL)
        height = kwargs.get('height', field_size(panel, 1, 1)[1])
        label_ctrl = wx.StaticText(panel, -1, label)
        sizer.Add(label_ctrl, border=12, flag=wx.ALIGN_CENTER_VERTICAL | wx.LEFT)
        panel.SetSizer(sizer)
        panel.SetMinSize((label_ctrl.GetSize().width + 12, height))
        return panel

    def _create_main_content(self, sizer):
        self._panel = panel = wx.ScrolledWindow(self._dialog, style=wx.TAB_TRAVERSAL)
        panel_sizer = wx.BoxSizer(wx.VERTICAL)
        self._create_scrolled_panel_content(panel_sizer)
        sizer.Add(panel, 1, wx.EXPAND)
        panel.SetSizer(panel_sizer)
        panel_sizer.Fit(panel)
        size = panel_sizer.GetMinSize()
        panel.SetMinSize((size.width, min(size.height, max(200, wx.DisplaySize()[1] - 400))))
        panel.SetScrollRate(0, 20)

    def _create_scrolled_panel_content(self, sizer):
        self._controls = []
        self._create_controls()
        for ctrls in self._controls:
            row = wx.BoxSizer()
            for x in ctrls:
                if x:
                    row.Add(x)
            sizer.AddSpacer(3)
            sizer.Add(row, 0, wx.ALIGN_LEFT | wx.LEFT | wx.RIGHT, 8)

    def _create_controls(self):
        pass


class SortingDialog(SFSDialog):
    """Dialog for selecting sorting parameters.

    The 'run()' method returns a sorting specification used by
    `pytis.form.LookupForm`.  If the dialog is closed in any way other than by
    clicking a button, 'None' is returned.

    """

    _TITLE = _("Sorting")
    BUTTON_SORT = GenericDialog.Button(_("Sort"), icon='submit', value='sort')
    BUTTON_RESET = GenericDialog.Button(_("Reset to default sorting"), icon='undo', value='reset')
    _BUTTONS = (BUTTON_SORT, BUTTON_RESET) + SFSDialog._BUTTONS
    _DEFAULT_BUTTON = BUTTON_SORT

    _DIRECTIONS = (pytis.data.ASCENDENT, pytis.data.DESCENDANT, None)
    _LABELS = {pytis.data.ASCENDENT: _("Ascending"),
               pytis.data.DESCENDANT: _("Descending"),
               None: _("No sorting")}

    _HELP_TOPIC = 'sorting'

    def __init__(self, parent, columns, sorting, direction=None, **kwargs):
        """Initialize the dialog.

        Arguments:

          parent -- wx parent of the dialog window
          columns -- a sequence of 'SFSColumn' instances
          sorting -- current sorting specification in the form of the 'sort'
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
                       tooltip=_("Choose the sorting column.")),
                choice([(self._LABELS[d], d) for d in self._DIRECTIONS], selected=dir,
                       tooltip=_("Choose the sorting direction."))))

    def _create_scrolled_panel_content(self, sizer):
        super(SortingDialog, self)._create_scrolled_panel_content(sizer)
        button = self._create_button(_("Add"), self._on_add, icon='plus',
                                     tooltip=_("Add secondary sorting column."))
        sizer.Add(button, 0, wx.ALL | wx.CENTER, 5)

    def _customize_result(self, wxid):
        button = self._button(wxid)
        if button == self.BUTTON_RESET:
            return ()
        elif button != self.BUTTON_SORT:
            return None
        else:
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
        self._rebuild()


class SFDialog(SFSDialog):
    """Common base class for all searching and filtering dialogs."""

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
               pytis.data.NE: '=/=',  # u'\u2260',
               pytis.data.LE: '=<',
               pytis.data.GE: '>=',
               pytis.data.LT: '<',
               pytis.data.GT: '>',
               pytis.data.AND: _(u"AND"),
               pytis.data.OR: _(u"OR")}
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
                           '* ' + _("value") + ' *')

    class SFConditionError(Exception):

        def __init__(self, i, ctrl, msg):
            app.error(_("Error in condition no. %d: %s", i + 1, msg))
            # ctrl.SetFocus()
            # self.focus()
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
            op = ' ' + operator.name() + ' '
            return '(' + op.join([self._strop(arg) for arg in operator.args()]) + ')'
        else:
            arg1, arg2 = operator.args()
            if isinstance(arg2, (pytis.data.Value, pytis.data.WMValue)):
                if isinstance(arg2.value(), unistr):
                    # Avoid the initial u for unicode strings...
                    arg2 = repr(arg2.value())[1:]
                else:
                    arg2 = repr(arg2.value())
            op = self._LABELS[self._RELATIONAL_OPERATORS_MAP[operator.name()]]
            return arg1 + ' ' + op + ' ' + arg2

    def _decompose_condition(self, operator, level=1):
        # Decompose nested conditions into a linear list of corresponding relational and logical
        # operators in infix notation.  Hierarchy is represented by the level of logical operators.
        if not isinstance(operator, pytis.data.Operator):
            raise Exception("Invalid condition: " + repr(operator))
        name, args = operator.name(), operator.args()
        if name in self._LOGICAL_OPERATORS_MAP:
            op = self._LOGICAL_OPERATORS_MAP[name]
            conds = [self._decompose_condition(arg, level=(level + 1)) for arg in args]
            return functools.reduce(lambda a, b: (a + ((op, level),)) + b, conds)
        elif name in self._RELATIONAL_OPERATORS_MAP:
            if len(args) != 2:
                raise Exception("Wrong number of arguments: " + str(args))
            arg1, arg2 = args
            op = self._RELATIONAL_OPERATORS_MAP[name]
            col1 = self._find_column(arg1)
            if col1 is None:
                raise Exception("Invalid column: " + arg1)
            if isinstance(arg2, basestring):
                col2 = self._find_column(arg2)
                if col2 is None:
                    raise Exception("Invalid column: " + arg2)
                value = None
            elif isinstance(arg2, (pytis.data.WMValue, pytis.data.Value)):
                col2 = None
                if isinstance(arg2, pytis.data.WMValue):
                    value = arg2.value()
                elif isinstance(col1.type(), pytis.data.Range):
                    value = ' - '.join(arg2.export())
                else:
                    value = arg2.export()
            else:
                raise Exception("Invalid operand type: " + repr(arg))
            return (op, col1, col2, value),
        elif name == 'IN' and isinstance(operator, pytis.presentation.IN):
            return ((operator,),)
        elif name == 'NOT' and len(operator.args()) == 1 \
                and isinstance(operator.args()[0], pytis.presentation.IN):
            return ((operator,),)
        else:
            raise Exception("Unsupported operator: " + name)

    def _create_controls(self):
        choice, spin, label, field, button = self._create_choice, self._create_spin_ctrl, \
            self._create_label, self._create_text_ctrl, self._create_button
        # Construct the UI controls based on the current condition.

        def create_logical_operator(i, n, operator, level):
            # Make all controls in this row slightly higher to avoid GTK warnings
            # on GtkSpinButton creation.
            height = 34
            return (
                choice([(self._LABELS[op], op) for op in self._LOGICAL_OPERATORS],
                       selected=operator, height=height,
                       tooltip=_("Choose the logical operator to join with previous conditions.")),
                label(_("Operator weight:"), height=height),
                spin(level, length=4, height=height,
                     tooltip=_("Choose the level of precedence of the logical operator.")))

        def create_relational_operator(i, n, operator, col1, col2, value):
            height = field_size(self._panel, 1, 1)[1]
            return (
                choice([(c.label(), c) for c in self._columns], selected=col1,
                       on_change=lambda e: self._on_selection_change(i),
                       tooltip=_("Choose the table column (first operand).")),
                choice([(self._LABELS[op], op) for op in self._OPERATORS], selected=operator,
                       tooltip=_("Choose the operator.")),
                choice([(c.label(), c) for c in self._col2_columns], selected=col2,
                       on_change=lambda e: self._on_selection_change(i),
                       tooltip=_("Choose the second operand.")),
                field(value, length=self._TEXT_CTRL_SIZE,
                      tooltip=_("Enter the operand value.")),
                button(_("Gather"), lambda e: self._on_suck(i),
                       icon='picker', height=height, enabled=self._row is not None,
                       tooltip=_("Use the value from the current form row.")),
                button(_("Clear"), lambda e: self._on_clear(i),
                       icon='clear', height=height, tooltip=_("Clear the condition.")),
                button(_("Remove"), lambda e: self._on_remove(i), enabled=n > 1,
                       icon='delete', height=height, tooltip=_("Remove this condition.")))

        def create_in_operator(i, n, operator):
            if operator.name() == 'NOT':
                op = operator.args()[0]
                op_name = _(u"NOT IN")
            else:
                op = operator
                op_name = _(u"IN")
            col = self._find_column(op.args()[0])
            ctrl = label(" ".join((col.label(), op_name, op.label())))
            ctrl._pytis_in_operator = operator
            return (ctrl,
                    button(_("Remove"), lambda e: self._on_remove(i),
                           enabled=n > 1,
                           tooltip=_("Remove this condition.")))

        c = self._find_column(self._col) or self._columns[0]
        empty = pytis.data.EQ(c.id(), pytis.data.Value(c.type(), None))
        # print "===", self._strop(self._condition or empty)
        try:
            operators = self._decompose_condition(self._condition or empty)
        except Exception as e:
            app.warning(_("Failed to decompose the conditional expression:") + " " + str(e))
            operators = self._decompose_condition(empty)
        for i, items in enumerate(operators):
            if len(items) == 1:
                self._controls.append(create_in_operator(i, len(operators), *items))
            elif len(items) == 2:
                self._controls.append(create_logical_operator(i, len(operators), *items))
            else:
                self._controls.append(create_relational_operator(i, len(operators), *items))
                self._on_selection_change(i)
        if len(self._controls[-1]) > 2:
            wval = self._controls[-1][3]
            if wval.IsEnabled():
                self._want_focus = wval

    def _create_scrolled_panel_content(self, sizer):
        super(SFDialog, self)._create_scrolled_panel_content(sizer)
        button = self._create_button
        buttons = (
            button(_("Add AND"), lambda e: self._on_add(), icon='plus',
                   tooltip=_("Add a new condition in conjunction.")),
            button(_("Add OR"), lambda e: self._on_add(or_=True), icon='plus',
                   tooltip=_("Add a new condition in disjunction.")),
            button(_("Remove all"), lambda e: self._on_reset(), icon='delete',
                   enabled=len(self._controls) > 1,
                   tooltip=_("Remove all current conditions.")),
        )
        bsizer = wx.BoxSizer(wx.HORIZONTAL)
        for b in buttons:
            if bsizer.ItemCount != 0:
                bsizer.AddSpacer(14)
            bsizer.Add(b)
        sizer.AddSpacer(12)
        sizer.Add(bsizer, 0, wx.CENTER)

    def _selected_condition(self, omit=None):
        # Construct the operator from the current dialog UI controls.

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
                        raise self.SFConditionError(i, wcol2,
                                                    _("Incompatible types %s and %s",
                                                      col1.type().__class__.__name__,
                                                      col2.type().__class__.__name__))
            elif isinstance(col1.type(), pytis.data.Binary):
                if wval.GetValue():
                    msg = _("Binary column can only be compared with value.")
                    raise self.SFConditionError(i, wval, msg)
                elif op not in (pytis.data.EQ, pytis.data.NE):
                    msg = _("Binary column can only be tested for equality/inequality.")
                    raise self.SFConditionError(i, wop, msg)
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
                    elif isinstance(col1.type(), pytis.data.Range):
                        val = tuple(x.strip() for x in val.split('-'))
                        if len(val) != 2:
                            raise self.SFConditionError(i, wval, _("Range must consist of two "
                                                                   "values separated by a dash."))
                    value, err = col1.type().validate(val, **kwargs)
                if err:
                    raise self.SFConditionError(i, wval, err.message())
                arg2 = value
            return op(col1.id(), arg2)

        def in_operator(i):
            ctrl = self._controls[i][0]
            return ctrl._pytis_in_operator

        def apply_logical_operator(operator, operators, level):
            # Apply the logical operators at given level to its operands and return the reduced
            # list of top-level operators.
            result = []
            operands = []
            for i in range(1, len(operators), 2):
                op, weight = operators[i]
                operand = operators[i - 1]
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
            if omit is None or i not in (omit, (omit == 0 and 1 or omit - 1)):
                if i % 2 == 1:
                    op, weight = logical_operator(i)
                    if weight not in weights:
                        weights.append(weight)
                    operators.append((op, weight))
                elif len(self._controls[i]) == 2:
                    operators.append(in_operator(i))
                else:
                    operators.append(relational_operator(i))
        weights.sort()
        weights.reverse()
        for weight in weights:
            operators = apply_logical_operator(pytis.data.AND, operators, weight)
            if len(operators) > 1:
                operators = apply_logical_operator(pytis.data.OR, operators, weight)
        # print "***", self._strop(operators[0])
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
        if isinstance(col.type(), pytis.data.Range):
            v = ' - '.join(v)
        elif isinstance(v, (tuple, list)):
            v = v[0]
        wval.SetValue(v)

    def _on_remove(self, i):
        try:
            condition = self._selected_condition(omit=i)
        except self.SFConditionError:
            pass
        else:
            self._condition = condition
            self._rebuild()

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
            self._rebuild()

    def _on_reset(self):
        self._condition = None
        self._rebuild()

    def _recompute_condition_for_button(self, button):
        return False

    def _on_button(self, event):
        # Cancel button action on error.
        if self._recompute_condition_for_button(self._button(event.Id)):
            try:
                self._condition = self._selected_condition()
            except self.SFConditionError:
                return
        return super(SFDialog, self)._on_button(event)


class SearchDialog(SFDialog):
    """Dialog for manipulation of the current search condition.

    The 'run()' method of this dialog returns a pair (DIRECTION, CONDITION).

    DIRECTION is the selected search direction.  The value can be either
    'pytis.data.FORWARD', 'pytis.data.BACKWARD' or 'None'.  'None' means that
    the search should not be performed (the dialog was escaped), the other two
    values indicate, that next record should be located in given direction.

    CONDITION is the selected search condition as a 'pytis.data.Operator'
    instance.

    """
    BUTTON_NEXT = GenericDialog.Button(_("Next"), icon='search-forward', value='next')
    BUTTON_PREVIOUS = GenericDialog.Button(_("Previous"), icon='search-backwards', value='prev')
    _BUTTONS = (BUTTON_NEXT, BUTTON_PREVIOUS) + SFSDialog._BUTTONS
    _DEFAULT_BUTTON = BUTTON_NEXT
    _TITLE = _("Search")
    _HELP_TOPIC = 'searching'

    def _recompute_condition_for_button(self, button):
        return button in (self.BUTTON_NEXT, self.BUTTON_PREVIOUS)

    def _customize_result(self, wxid):
        button = self._button(wxid)
        if button == self.BUTTON_NEXT:
            direction = pytis.data.FORWARD
        elif button == self.BUTTON_PREVIOUS:
            direction = pytis.data.BACKWARD
        else:
            return None, None
        return direction, self._condition


class FilterDialog(SFDialog):
    """Dialog for manipulation of the filter condition and aggregations.

    This dialog edits the current filter condition.  In addition it has a
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
    BUTTON_FILTER = GenericDialog.Button(_("Filter"), icon='filter', value='filter')
    BUTTON_UNFILTER = GenericDialog.Button(_("Unfilter"), icon='unfilter', value='unfilter')
    _BUTTONS = (BUTTON_FILTER, BUTTON_UNFILTER) + SFSDialog._BUTTONS
    _DEFAULT_BUTTON = BUTTON_FILTER
    _AGG_OPERATORS = (pytis.data.Data.AGG_COUNT,
                      pytis.data.Data.AGG_MIN,
                      pytis.data.Data.AGG_MAX,
                      pytis.data.Data.AGG_SUM,
                      pytis.data.Data.AGG_AVG)
    _AGG_LABELS = {pytis.data.Data.AGG_COUNT: _("Count"),
                   pytis.data.Data.AGG_MIN: _("Minimum"),
                   pytis.data.Data.AGG_MAX: _("Maximum"),
                   pytis.data.Data.AGG_SUM: _("Sum"),
                   pytis.data.Data.AGG_AVG: _("Average")}
    _TITLE = _("Filter")
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
        super(FilterDialog, self).__init__(parent, columns, row, **kwargs)
        self._compute_aggregate = compute_aggregate

    def _create_main_content(self, sizer):
        super(FilterDialog, self)._create_main_content(sizer)
        cp = wx.CollapsiblePane(self._dialog, label=_("Aggregation functions"))
        self._handle_keys(cp)
        pane = cp.GetPane()
        choice, field, button = self._create_choice, self._create_text_ctrl, self._create_button
        self._agg_controls = (
            choice([(c.label(), c) for c in self._columns], parent=pane,
                   tooltip=_("Select the column for aggregation.")),
            choice([(self._AGG_LABELS[op], op) for op in self._AGG_OPERATORS], parent=pane,
                   tooltip=_("Select the aggregation function.")),
            field(None, length=24, readonly=True, parent=pane,
                  tooltip=_("Displays the aggregation function result.")),
            button(_("Compute"), self._on_compute_aggregate, parent=pane,
                   tooltip=_("Compute the aggregation function result.")))
        boxsizer = wx.BoxSizer(wx.HORIZONTAL)
        for ctrl in self._agg_controls:
            boxsizer.Add(ctrl)
        pane.SetSizer(boxsizer)
        sizer.Add(cp, 0, wx.LEFT | wx.TOP | wx.RIGHT, 8)

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
                app.error(_("Operation not supported for the selected column type."))
                v = ''
            else:
                result = self._compute_aggregate(op, col.id(), condition)
                if result is not None:
                    v = result.export()
                else:
                    v = ''
            wresult.SetValue(v)

    def _recompute_condition_for_button(self, button):
        return button == self.BUTTON_FILTER

    def _customize_result(self, wxid):
        button = self._button(wxid)
        if button == self.BUTTON_FILTER:
            return True, self._condition
        elif button == self.BUTTON_UNFILTER:
            return True, None
        else:
            return False, None


def sfs_columns(fields, data, labelfunc=Field.label):
    """Return a list of 'SFSColumn' instances for SFS dialog construction.

    (SFS = Search, Filter, Sort)

    Arguments:

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
    return sorted(columns, key=lambda c: pytis.util.strxfrm(c.label()))
