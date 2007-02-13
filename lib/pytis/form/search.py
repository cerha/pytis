# -*- coding: iso-8859-2 -*-

# Prvky u�ivatelsk�ho rozhran� souvisej�c� s�vyhled�v�n�m
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

    def label(self, default=""):
        """Vra� label zadan� v�konstruktoru."""
        return self._label or default

    
class SFSDialog(GenericDialog):
    """Spole�n� z�klad v�ech vyhled�vac�ch, filtrovac�ch a t��d�c�ch dialog�."""

    _FIELD_HEIGHT = 26

    def _create_button(self, label, callback, tooltip=None, **kwargs):
        return wx_button(self._dialog, label=label, callback=callback,
                         tooltip=tooltip, height=self._FIELD_HEIGHT, **kwargs)
        
    def _create_choice(self, choices, tooltip=None, label=identity,
                       selected=None, on_change=None):
        ch = wx.Choice(self._dialog, -1, choices=[label(x) for x in choices])
        ch.SetSelection(0)
        ch.SetMinSize((ch.GetSize().width, self._FIELD_HEIGHT))
        if tooltip is not None and config.show_tooltips:
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
        if tooltip is not None and config.show_tooltips:
            t.SetToolTipString(unicode(tooltip))
        if enabled is not None:
            t.Enable(enabled)
        return t


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
                 tooltip=_("Zvolte sloupec tabulky, podle n�j� chcete t��dit"))
                colpos = position(col, columns, key=lambda c: c.id())
                if colpos:
                    colsel.SetSelection(colpos)
                # Sm�r t��d�n�
                dirsel = self._create_choice(direction_choices,
                                             tooltip=_("Zvolte sm�r t��d�n�"))
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
                 tooltip=_("Zvolte sloupec tabulky, podle n�j� chcete t��dit"))
            col = self._col
            if col is not None:
                for i in range(len(columns)):
                    if columns[i].id() == col:
                        colsel.SetSelection(i)
                        break
            # Sm�r t��d�n�
            dirsel = self._create_choice(direction_choices,
                                         tooltip=_("Zvolte sm�r t��d�n�"))
            if self._direction == LookupForm.SORTING_DESCENDANT:
                dirsel.SetSelection(1)
            self._selections.append((colsel, dirsel))
            # Sizer
            sizer = wx.BoxSizer()
            for w in colsel, dirsel:
                sizer.Add(w)
            big_sizer.Add(sizer)
        button = self._create_button(_("P�idat"), self._on_add,
                                     _("P�idat sloupec sekund�rn�ho t��d�n�"))
        big_sizer.Add(button)
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

    
class SFDialog(SFSDialog):
    """Spole�n� z�klad v�ech vyhled�vac�ch a filtrovac�ch dialog�."""

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
               pytis.data.AND: _("a z�rove�"),
               pytis.data.OR:  _("nebo")}
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
    _TITLE = ''
    _BUTTONS = (_("Zav��t"),)
    _TEXT_CTRL_SIZE = 18
    _NO_COLUMN = SFSColumn('--sfs-dlg-no-column--', pytis.data.String(), None)
    
    class SFConditionError(Exception):
        pass

    def __init__(self, parent, columns, row, col=None, condition=None):
        """Initialize the dialog.

        Arguments:

          parent -- wx parent of the dialog window
          columns -- a sequence of 'SFSColumn' instances
          row -- current row as a 'pytis.data.Row' instance or 'None'
          col -- current column identifier as a string
          condition -- search/filtering condition as a 'pytis.data.Operator'
            instance.  This condition will be preselected in the dialog.  The
            current implementation, however, can only display a certainly
            structured condition.  It is safe to use a condition obtained from
            the previous dialog call.

        """
        self._columns = (self._NO_COLUMN,) + tuple(columns)
        self._row = row
        self._col = col
        self._condition = condition
        super_(SFDialog).__init__(self, parent, self._TITLE, self._BUTTONS)

    def _create_content(self):
        # Construct the ui controls based on the current condition.
        def decompose_condition(operator, logical_operation=None):
            # Decompose nested conditions into a list of corresponding operator
            # functions and their logical pairing.
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
                return ((logical_operation, op, arg1, arg2), )
            else:
                raise Exception("Unsupported operator: "+ name)
        def create_controls(i, n, logical_operator, operator, arg1, arg2):
            col1 = find(arg1, self._columns, key=lambda c: c.id())
            if isinstance(arg2, str):
                col2 = find(arg2, self._columns, key=lambda c: c.id())
                if col2 is None:
                    raise Exception("Invalid operand: "+ repr(arg2))
                value = None
            else:
                col2 = None
                value = isinstance(arg2, pytis.data.WMValue) \
                        and arg2.value() or arg2.export()
            choice, field, button = self._create_choice, \
                                    self._create_text_ctrl, self._create_button
            return (
                logical_operator and \
                choice(self._LOGICAL_OPERATORS, selected=logical_operator,
                       label=lambda o: self._LABELS[o],
                  tooltip=_("Zvolte zp�sob spojen� s p�edchoz�mi podm�nkami")),
                choice(self._columns, selected=col1,
                       label=lambda c: c.label('- '+_("zvolte sloupec")+' -'),
                       on_change=lambda e: self._on_selection_change(i),
                       tooltip=_("Zvolte sloupec tabulky")),
                choice(self._OPERATORS, selected=operator,
                       label=lambda o: self._LABELS[o],
                       tooltip=_("Zvolte oper�tor")),
                choice(self._columns, selected=col2,
                       label=lambda c: c.label('* '+_("hodnota")+' *'),
                       on_change=lambda e: self._on_selection_change(i),
                       tooltip=_("Zvolte s ��m m� b�t hodnota porovn�v�na")),
                field(self._TEXT_CTRL_SIZE, value,
                      tooltip=_("Zapi�te hodnotu podm�nkov�ho v�razu")),
                button(_("Nas�t"), lambda e: self._on_suck(i),
                       _("Na��st hodnotu aktivn� bu�ky")),
                button(_("Vymazat"), lambda e: self._on_clear(i),
                       _("Vymazat obsah podm�nky")),
                button(_("Odebrat"), lambda e: self._on_remove(i),
                       _("Zru�it tuto podm�nku"), enabled=n > 1))
        condition = self._condition
        if condition is None:
            c = self._NO_COLUMN
            condition = pytis.data.EQ(c.id(), pytis.data.Value(c.type(), None))
        conditions = decompose_condition(condition)
        self._controls = []
        for i, items in enumerate(conditions):
            self._controls.append(create_controls(i, len(conditions), *items))
            self._on_selection_change(i)
        b1 = self._create_button(_('P�idat "a z�rove�"'),
                                 lambda e: self._on_add(),
                                 _("P�idat novou podm�nku v konjunkci"))
        b2 = self._create_button(_('P�idat "nebo"'),
                                 lambda e: self._on_add(or_=True),
                                 _("P�idat novou podm�nku v disjunkci"))

        # Put all the controlls into sizers.
        sizer = wx.BoxSizer(wx.VERTICAL)
        for ctrls in self._controls:
            row = wx.BoxSizer()
            for x in ctrls:
                if x:
                    row.Add(x)
            sizer.Add(row, 0, wx.ALIGN_RIGHT)
        bsizer = wx.BoxSizer(wx.HORIZONTAL)
        bsizer.Add(b1)
        bsizer.Add(b2, 0, wx.LEFT, 20)
        # Hotovo
        return (sizer, bsizer)

    def _selected_condition(self, omit=None, allow_no_column=False):
        # Construct the operator from the current dialog ui controls.
        def quit(i, ctrl, msg):
            msg = _("Chyba v podm�nce �. %d: %s") % (i+1, msg)
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
            col2 = self._columns[wcol2.GetSelection()]
            if col1 is self._NO_COLUMN and not allow_no_column:
                quit(i, wcol1, _("Nen� zvolen sloupec"))
            if col2 is not self._NO_COLUMN:
                arg2 = col2.id()
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
                            quit(i, wval, _("Bin�rn� sloupec lze testovat "
                                            "pouze na pr�zdnou hodnotu"))
                        else:
                            val = None
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
        enabled = wcol1.GetSelection() != 0
        for ctrl in (wop, wcol2,wval, bclear, bsuck):
            ctrl.Enable(enabled)
        if enabled:
            e2 = wcol2.GetSelection() == 0
            for ctrl in (wval, bsuck):
                ctrl.Enable(e2)
        
    def _on_clear(self, i):
        wcol1, wop, wcol2, wval = self._controls[i][1:5]
        wcol1.SetSelection(0)
        wop.SetSelection(0)
        wval.SetValue('')
        self._on_selection_change(i)

    def _on_suck(self, i):
        wcol1, wop, wcol2, wval = self._controls[i][1:5]
        col = self._columns[self._controls[i][1].GetSelection()]
        v = self._row[col.id()].export()
        if is_sequence(v):
            v = v[0]
        wval.SetValue(v)

    def _on_remove(self, i):
        try:
            condition = self._selected_condition(omit=i, allow_no_column=True)
        except self.SFConditionError:
            pass
        else:
            self._condition = condition
            self.rebuild()
    
    def _on_add(self, or_=False):
        try:
            condition = self._selected_condition(allow_no_column=True)
        except self.SFConditionError:
            pass
        else:
            op = or_ and pytis.data.OR or pytis.data.AND
            c = self._NO_COLUMN
            v = pytis.data.Value(c.type(), None)
            self._condition = op(condition, pytis.data.EQ(c.id(), v))
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
    _NEXT_BUTTON = _("Dal��")
    _PREVIOUS_BUTTON = _("P�edchoz�")
    _BUTTONS = (_NEXT_BUTTON, _PREVIOUS_BUTTON) + SFDialog._BUTTONS
    _COMMIT_BUTTON = _NEXT_BUTTON    
    _TITLE = _("Hled�n�")
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
    _FILTER_BUTTON = _("Filtrovat")
    _UNFILTER_BUTTON = _("Zru�it filtr")
    _BUTTONS = (_FILTER_BUTTON, _UNFILTER_BUTTON) + SFDialog._BUTTONS
    _COMMIT_BUTTON = _FILTER_BUTTON
    _AGG_OPERATORS = (pytis.data.Data.AGG_COUNT,
                      pytis.data.Data.AGG_MIN,
                      pytis.data.Data.AGG_MAX,
                      pytis.data.Data.AGG_SUM,
                      pytis.data.Data.AGG_AVG)
    _AGG_LABELS = {pytis.data.Data.AGG_COUNT: _("Po�et"),
                   pytis.data.Data.AGG_MIN:   _("Minimum"),
                   pytis.data.Data.AGG_MAX:   _("Maximum"),
                   pytis.data.Data.AGG_SUM:   _("Sou�et"), 
                   pytis.data.Data.AGG_AVG:   _("Pr�m�r")}
    _TITLE = _("Filtrov�n�")
    _HELP_TOPIC = 'filtering'

    def __init__(self, parent, columns, row, compute_aggregate, **kwargs):
        self._compute_aggregate = compute_aggregate
        self._perform = False
        super(FilterDialog, self).__init__(parent, columns, row, **kwargs)

    def _create_content(self):
        content = super_(FilterDialog)._create_content(self)
        choice, field, button = self._create_choice, \
                                self._create_text_ctrl, self._create_button
        self._agg_controls = (
            choice(self._columns[1:], label=lambda c: c.label(),
                   tooltip=_("Zvolte sloupec pro agregaci")),
            choice(self._AGG_OPERATORS, label=lambda o: self._AGG_LABELS[o],
                   tooltip=_("Zvolte agrega�n� funkci")),
            field(24, readonly=True,
                  tooltip=_("Zobrazen� v�sledku agrega�n� funkce")),
            button(_("Zjistit"), self._on_compute_aggregate,
                   tooltip=_("Zobraz v�sledek zvolen� agreka�n� funkce")))
        box = wx.StaticBox(self._dialog, -1, _("Agrega�n� funkce:"))
        sizer = wx.StaticBoxSizer(box, wx.HORIZONTAL)
        for w in self._agg_controls:
            sizer.Add(w)
        return content + (sizer,)

    def _on_compute_aggregate(self, event):
        if self._on_filter():
            wcol, wop, wresult, wbutton = self._agg_controls
            op = self._AGG_OPERATORS[wop.GetSelection()]
            col = self._columns[wcol.GetSelection()+1]
            result = self._compute_aggregate(op, col.id(), self._condition)
            if result is not None:
                wresult.SetValue(result.export())

    def _on_filter(self):
        try:
            condition = self._selected_condition()
        except self.SFConditionError:
            return False
        else:
            self._condition = condition
            self._perform = True
            return True

    def _on_unfilter(self):
        self._perform = True
        self._condition = None

    def _on_button(self, event):
        label = self._button_label(event.GetId())
        if label == self._FILTER_BUTTON:
            if not self._on_filter():
                return False
        elif label == self._UNFILTER_BUTTON:
            self._on_unfilter()
        return super(FilterDialog, self)._on_button(event)
        
    def _customize_result(self, button_wid):
        return self._perform, self._condition


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
