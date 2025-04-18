# -*- coding: utf-8 -*-

# Copyright (C) 2018-2025 Tomáš Cerha <t.cerha@gmail.com>
# Copyright (C) 2001-2017 OUI Technology Ltd.
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

"""Input field abstraction.

The input field is basically the user interface control (widget or a set of widgets) which is
directly bound to the PresentedRow instance representing the edited record.  The changes are
automatically propagated between the PresentedRow instance and the user interface.

The actual class representing each field is determined by its specification and data type.

"""
from __future__ import print_function

from past.builtins import basestring
from builtins import range
from future.utils import python_2_unicode_compatible

import io
import datetime
import os
import re
import sys
import textwrap

import wx.lib.colourselect

import lcg
import pytis.api
import pytis.data
import pytis.form
import pytis.util
from pytis.api import app
from pytis.presentation import (
    AttachmentStorage, Button, CodebookSpec, Editable, Enumeration,
    Field, HGroup, MenuItem, Orientation, PostProcess, PresentedRow, Command,
    SelectionType, TextFilter, TextFormat, MenuSeparator, computer
)
from pytis.util import (
    ProgramError, ResolverError, find, format_byte_size,
)

from .application import Application, run_form
from .command import CommandHandler, UICommand
from .dialog import Calendar, ColorSelector, Error
from .event import wx_callback, top_level_exception
from .form import RecordForm
from .screen import (
    CallbackHandler, InfoWindow, KeyHandler, TextHeadingSelector,
    char2px, dlg2px, file_menu_items, get_icon, uicommand_mitem,
    paste_from_clipboard, wx_button, wx_focused_window,
    copy_to_clipboard, field_size, wx_toolbar, darkmode
)


_ = pytis.util.translations('pytis-wx')


class _Completer(wx.TextCompleterSimple):
    """wx autocompletion API implementation on top of 'PresentedRow.completions()'."""

    def __init__(self, row, field_id):
        super(_Completer, self).__init__()
        self._row = row
        self._field_id = field_id

    def GetCompletions(self, prefix):
        return self._row.completions(self._field_id, prefix=prefix)


@pytis.api.implements(pytis.api.Field)
@python_2_unicode_compatible
class InputField(KeyHandler, CommandHandler):
    """Abstract base class for input fields.

    Subclasses of this class implement input fields for particular field types
    according to their specification.  Instances of InputField subclasses are
    not themselves wx widgets, but they create widgets and return them through
    the methods 'label()' and 'widget()'.

    """
    _DEFAULT_WIDTH = 13
    _DEFAULT_HEIGHT = 1

    DEFAULT_FIELD_LABEL_COLOR = wx.WHITE if darkmode else wx.BLACK
    DISABLED_FIELD_LABEL_COLOR = '#606060' if darkmode else '#606060'
    INVALID_FIELD_LABEL_COLOR = '#ff546f' if darkmode else '#ba344f'

    DEFAULT_FIELD_BACKGROUND_COLOR = wx.BLACK if darkmode else wx.WHITE
    DISABLED_FIELD_BACKGROUND_COLOR = '#202020' if darkmode else '#f0f0f0'

    # 'wx.TextCtrl' has its own system color in the disabled state, but we don't
    # use the disabled state because of its side effects and make the field read
    # only and change the color manually.  This color should match the system
    # color, particularly we use the default color used by GTK+ 3 on Linux as
    # this is our primary target platform.  To support other platforms or themes,
    # we would need to do some decision making here.

    _last_focused_field = None
    _icon_cache = {}

    @classmethod
    def command_handler_instance(cls):
        return InputField.last_focused_field()

    @classmethod
    def create(cls, parent, row, id, **kwargs):
        """Create an instance of the class corresponding to the field specification.

        The arguments are the same as for the 'InputField' constructor.

        """
        # assert isinstance(parent, wx.Window)
        assert isinstance(row, PresentedRow)
        assert isinstance(id, basestring)
        spec = find(id, row.fields(), key=lambda f: f.id())
        ftype = row.type(id)
        if ftype.enumerator() is not None:
            selection_type = spec.selection_type()
            if isinstance(ftype, pytis.data.Boolean) and selection_type is None:
                field = CheckBoxField
            else:
                if selection_type is None:
                    if row.codebook(id) is not None:
                        selection_type = SelectionType.CODEBOOK
                    else:
                        selection_type = SelectionType.CHOICE
                mapping = {
                    SelectionType.CODEBOOK: CodebookField,
                    SelectionType.LIST: ListField,
                    SelectionType.CHOICE: ChoiceField,
                    SelectionType.LISTBOX: ListBoxField,
                    SelectionType.RADIO: RadioBoxField,
                }
                field = mapping[selection_type]
        elif isinstance(ftype, pytis.data.IntegerRange):
            field = NumericRangeField
        elif isinstance(ftype, pytis.data.LargeIntegerRange):
            field = NumericRangeField
        elif isinstance(ftype, pytis.data.DateRange):
            field = DateRangeField
        elif isinstance(ftype, pytis.data.DateTimeRange):
            field = DateTimeRangeField
        elif isinstance(ftype, pytis.data.Image):
            field = ImageField
        elif isinstance(ftype, pytis.data.Binary):
            field = FileField
        elif isinstance(ftype, pytis.data.Date):
            field = DateField
        elif isinstance(ftype, pytis.data.Time):
            field = TimeField
        elif isinstance(ftype, pytis.data.DateTime):
            field = DateTimeField
        elif isinstance(ftype, pytis.data.Color):
            field = ColorSelectionField
        elif isinstance(ftype, pytis.data.Password):
            field = PasswordField
        elif isinstance(ftype, pytis.data.String):
            if spec.text_format() == TextFormat.LCG:
                field = StructuredTextField
            else:
                field = StringField
        elif isinstance(ftype, pytis.data.Number):
            field = NumericField
        else:
            field = TextField
        return field(parent, row, id, **kwargs)

    @classmethod
    def set_last_focused_field(cls, field):
        cls._last_focused_field = field

    @classmethod
    def last_focused_field(cls):
        field = cls._last_focused_field
        if field is not None and field._alive():
            return field
        else:
            return None

    @classmethod
    def icon(cls, icon):
        try:
            result = cls._icon_cache[icon]
        except KeyError:
            result = cls._icon_cache[icon] = get_icon(icon)
        return result

    # Instance methods

    def __init__(self, parent, row, id, guardian=None, readonly=False):
        """Initialize the input field according to its specification.

        Arguments:

          parent -- wx parent for the created widgets.
          row -- 'PresentedRow' instance.
          id -- field specification as a 'Field' instance.
          guardian -- parent 'KeyHandler'.
          readonly --

        This method should not be overriden by derived classes.  All field
        specific initialization should be done in the methods
        '_create_widget()' and '_create_label'.

        """
        assert isinstance(row, PresentedRow)
        assert isinstance(id, basestring)
        assert isinstance(guardian, KeyHandler)
        spec = find(id, row.fields(), key=lambda f: f.id())
        self._row = row
        self._type = row.type(id)
        self._spec = spec
        self._guardian = guardian
        self._id = id = spec.id()
        self._want_focus = False
        self._last_focused_ctrl = None
        self._connection_closed = False
        if row.new():
            permission = pytis.data.Permission.INSERT
        else:
            permission = pytis.data.Permission.UPDATE
        self._denied = denied = not row.permitted(id, permission)
        self._hidden = not row.permitted(id, pytis.data.Permission.VIEW)
        encrypted = (spec.crypto_name() and spec.crypto_name() not in app.decrypted_areas())
        readonly = readonly or encrypted
        self._readonly = readonly or denied or row.hidden_codebook(id)
        self._enabled = not readonly and row.editable(id)
        self._callback_registered = False
        self._skipped_controls = {}
        self._needs_validation = True
        self._last_validation_error = None
        self._needs_check = False
        self._last_check_result = None
        self._had_focus = False
        self._init_attributes()
        self._call_on_idle = []
        self._status_icon = wx.StaticBitmap(parent, bitmap=InputField.icon('field-ok'))
        self._ctrl = ctrl = self._create_ctrl(parent)
        self._controls = [(ctrl, self._set_ctrl_editable)]
        self._init_ctrl(ctrl)
        label = self._create_label(parent)
        widget = self._create_widget(parent, ctrl)
        if label and spec.compact():
            label = self._hbox(*([label] + self._icons(parent)))
        else:
            widget = self._hbox(*([widget] + self._icons(parent)))
        self._label = label
        self._widget = widget
        self._set_editable(self._enabled)
        row.register_callback(row.CALL_CHANGE, id, self._change_callback)
        row.register_callback(row.CALL_EDITABILITY_CHANGE, id,
                              self._editability_change_callback)
        row.register_callback(row.CALL_CHECK, id, self._check_callback)
        value = row.invalid_string(id)
        if value is None:
            value = row.format(id, single=False, secure='')
        self._set_value(value)
        self._call_on_idle.append(self._update_field_state)

    def __str__(self):
        try:
            return "<%s id='%s'>" % (self.__class__.__name__, self.id())
        except AttributeError:
            return "<%s (uninitialized)>" % self.__class__.__name__

    def _init_attributes(self):
        pass

    def _init_ctrl(self, ctrl):
        KeyHandler.__init__(self, ctrl)
        wx_callback(wx.EVT_IDLE, ctrl, self._on_idle)
        wx_callback(wx.EVT_RIGHT_DOWN, ctrl, lambda e: self._on_context_menu(ctrl))
        wx_callback(wx.EVT_NAVIGATION_KEY, ctrl, self._on_navigation(ctrl))

    def _on_navigation(self, widget, skip=False):
        def cb(e):
            e.Skip()
            if skip or widget in self._skipped_controls:
                flag = e.GetDirection() and wx.NavigationKeyEvent.IsForward or 0
                wx.CallAfter(lambda: widget.Navigate(flag))
        return cb

    def _box(self, orientation, content):
        # Helper function to group wx widgets into a horizontal box (sizer).
        box = wx.BoxSizer(orientation)
        for x in content:
            if isinstance(x, tuple):
                box.Add(*x)
            else:
                minsize = x.MinSize if hasattr(x, 'MinSize') else None
                box.Add(x, 0, wx.FIXED_MINSIZE)
                if hasattr(x, 'MinSize') and minsize != x.MinSize:
                    # This hack prevents the previously set minsize to drop
                    # unexpectedly, which happened for example with the ListBoxField
                    # present in pe.Commands.run_any_form() input form.
                    x.SetMinSize(minsize)
        return box

    def _hbox(self, *content):
        return self._box(wx.HORIZONTAL, content)

    def _vbox(self, *content):
        return self._box(wx.VERTICAL, content)

    def _create_label(self, parent):
        # Return field label as 'wx.StaticText' instance.
        label = self.spec().label()
        if label:
            return wx.StaticText(parent, -1, label + ':', style=wx.ALIGN_RIGHT)
        else:
            return None

    def _create_ctrl(self, parent):
        # Return the actual control element for this field.
        raise ProgramError("This method must be overriden!")

    def _create_widget(self, parent, ctrl):
        # Create additional UI elements for the field control.  Return a wx
        # widget containing all UI elements for given field.  This class simply
        # returns the actual control, but derived classes may add extra buttons
        # etc. to create more sophisticated user interface.
        return ctrl

    def _icons(self, parent):
        icons = [(self._status_icon, 0, wx.LEFT, 4)]
        descr = self._spec.descr()
        if descr:
            help_icon = wx.StaticBitmap(parent, bitmap=InputField.icon('info'))
            help_icon.SetToolTip(textwrap.fill(descr, 60))
            icons.append((help_icon, 0, wx.LEFT, 1))
        return icons

    def _get_value(self):
        # Return the external (string) representation of the current field value from the field UI
        # control.  This value must be validatable by the field data type.
        raise ProgramError("This method must be overriden!")

    def _set_value(self, value):
        # Set the field control according to given external (string) value.
        raise ProgramError("This method must be overriden!")

    # Other private methods.

    def _menu(self):
        # Return a tuple of popup menu items ('MenuItem' instances).
        menu = (UICommand(Command(InputField.reset),
                          _("Restore the original value"),
                          _("Discard all changes.")),)
        file_open_mitems = file_menu_items([self._spec], self._row, {})
        if file_open_mitems:
            menu += tuple(file_open_mitems)
        return menu

    def _on_context_menu(self, ctrl, position=None):
        menu = []
        for item in self._menu():
            if item is None:
                item = MenuSeparator()
            elif isinstance(item, UICommand):
                if issubclass(item.command().definer, (InputField, Invocable)):
                    item = MenuItem(item.title(),
                                    command=item.command().bind(self),
                                    help=item.descr())
                else:
                    item = uicommand_mitem(item)
            menu.append(item)
        pytis.form.app.popup_menu(self._current_ctrl(), menu, position=position,
                                  keymap=pytis.form.app.keymap)

    def _validate(self):
        return self._row.validate(self.id(), self._get_value())

    def _valid(self):
        return self._last_validation_error is None

    def _check(self):
        return self._row.check(self.id())

    def _do_validation(self, do_validation, do_check):
        try:
            transaction = self._row.transaction()
            if transaction and not transaction.open() or not all(x[0] for x in self._controls):
                # Don't validate when the transaction is already closed.  Also abort if the C++
                # controls are already destroyed (as we may get called later due to wx.CallAfter).
                return
            if do_validation:
                try:
                    error = self._validate()
                except (pytis.data.DBRetryException, pytis.data.DBSystemException):
                    error = None
                    self._connection_closed = True
                self._last_validation_error = error
                self._on_change_hook()
            if do_check:
                self._last_check_result = self._check()
            self._update_field_state()
            if self._last_validation_error:
                app.echo(self._last_validation_error.message())
            elif self._last_check_result:
                app.echo(self._last_check_result)
        except:
            # _do_validation is called outside the main thread so we need to handle top
            # level exceptions here.  Otherwise errors in runtime filters and check
            # functions are silently ignored.
            wx.CallAfter(top_level_exception, sys.exc_info())

    def _on_idle(self, event):
        w = wx_focused_window()
        if w in [x[0] for x in self._controls]:
            self._last_focused_ctrl = w
            InputField.set_last_focused_field(self)
        elif self._want_focus and self.enabled():
            self._set_focus(self._current_ctrl())
        self._want_focus = False
        if self._needs_validation or self._needs_check:
            wx.CallAfter(self._do_validation, self._needs_validation, self._needs_check)
            self._needs_validation = self._needs_check = False
        while self._call_on_idle:
            callback = self._call_on_idle.pop()
            callback()
        event.Skip()

    def _on_change_hook(self):
        """Handle field value changes.

        Overriding this method allows any additional actions after each change of the field value.

        """
        pass

    def _current_ctrl(self):
        """

        Note, there may be several active controls, for example in range fields...

        """
        return self._last_focused_ctrl or self._controls[0][0]

    def _change_callback(self):
        # Field value change signalization from PresentedRow.
        value = self._row.format(self.id(), single=False, secure=True)
        if self._get_value() != value:
            self._set_value(value)

    def _editability_change_callback(self):
        # Field editability change signalization from PresentedRow.
        if not self._denied and not self._readonly:
            enabled = self._row.editable(self.id())
            if enabled != self._enabled:
                self._enabled = enabled
                self._set_editable(enabled)
                # The change won't take effect for certain fields if we do it directly!
                self._call_on_idle.append(self._update_field_state)

    def _check_callback(self):
        self._needs_check = True

    def _on_change(self, event=None):
        # Called on user interaction (editation, selection).  The actual processing of the event
        # is postponed to the idle thread to avoid user interface hangs on time-consuming
        # operations (such as complicated field recomputations).
        if not self._readonly:
            self._needs_validation = True
            self._update_field_state()
        if event:
            event.Skip()

    def _set_editable(self, editable):
        for ctrl, set_editable in self._controls:
            set_editable(ctrl, editable)
            if editable:
                # ctrl.Disconnect(-1, -1, wx.wxEVT_NAVIGATION_KEY)
                # The disconnect above doeasn't work, so here is a nasty workaround.
                if ctrl in self._skipped_controls:
                    del self._skipped_controls[ctrl]
            else:
                self._skipped_controls[ctrl] = True

    def _set_ctrl_editable(self, ctrl, editable):
        ctrl.Enable(editable)

    def _update_field_state(self):
        """Update UI field state indication.

        The field state is primarily indicated by an icon which is normally on
        the upper right edge of the field.  The field states such as disabled,
        locked, invalid, ...) have distinct icons and some of them may also by
        indicated by different label or background color.

        """
        if self._denied:
            icon = 'field-locked'
            tooltip = _("The field is ineditable due to insufficient permissions.")
            color = self.DISABLED_FIELD_LABEL_COLOR
        elif self._hidden and not self._modified():
            icon = 'field-hidden'
            tooltip = _("The field value is not visible due to insufficient permissions.")
            color = self.DISABLED_FIELD_LABEL_COLOR
        elif not self._row.editable(self._id):
            icon = 'field-disabled'
            tooltip = _("The field is not editable.")
            color = self.DISABLED_FIELD_LABEL_COLOR
        elif not self.valid():
            icon = 'field-invalid'
            error = self._last_validation_error
            if error:
                tooltip = _("Validation error:") + ' ' + error.message()
                if self._type.not_null() and self._row[self._id].value() is None:
                    tooltip += ' (' + _("this field is mandatory") + ')'
            else:
                tooltip = self._last_check_result
            color = self.INVALID_FIELD_LABEL_COLOR
        else:
            icon = 'field-ok'
            tooltip = _("The current field value is valid.")
            color = self.DEFAULT_FIELD_LABEL_COLOR
        self._status_icon.SetBitmap(InputField.icon(icon))
        self._status_icon.SetToolTip(tooltip)
        label = self._label
        if isinstance(label, wx.Sizer):
            label = label.GetItem(0).Window  # When 'compact', the label includes icons.
        if label:
            label.SetForegroundColour(color)

    def _modified(self):
        # Returns always false for virtual fields
        return self._row.field_changed(self.id())

    def _set_focus(self, ctrl):
        parent = ctrl.GetParent()
        nb = parent.GetParent()
        if isinstance(nb, wx.Notebook) and nb.GetCurrentPage() != parent:
            for i in range(nb.GetPageCount()):
                if nb.GetPage(i) == parent:
                    nb.SetSelection(i)
        ctrl.SetFocus()

    def _alive(self):
        try:
            for ctrl, set_editable in self._controls:
                ctrl.GetId()
            return True
        except RuntimeError:
            return False

    # Commands

    @Command.define
    def reset(self):
        """Reset the field to its original value."""
        self._row[self._id] = self._row.original_row()[self._id]

    def _can_reset(self):
        return self._modified() and self._enabled

    @Command.define
    def context_menu(self):
        for ctrl, set_editable in self._controls:
            size = ctrl.GetSize()
            self._on_context_menu(ctrl, position=(size.x // 3, size.y // 2))

    # Public methods

    def width(self):
        """Return field width in characters."""
        return self.spec().width(self._DEFAULT_WIDTH)

    def height(self):
        """Return field height in characters."""
        return self.spec().height(self._DEFAULT_HEIGHT)

    def id(self):
        """Return the field identifier as a string."""
        return self._id

    def guardian(self):
        return self._guardian

    def spec(self):
        """Return field specification as a 'Field' instance."""
        return self._spec

    def type(self):
        """Return the data type as a 'pytis.data.Type' instance."""
        return self._type

    def widget(self):
        """Return the complete widget as a 'wx.Window' instance."""
        return self._widget

    def label(self):
        """Return the field label as a 'wx.StaticText' instance."""
        return self._label

    def validate(self, interactive=True):
        """Invoke field validation and propagate current user input to the underlying PresentedRow.

        Arguments:

          interactive -- controls how the validation error is announced if the current field value
            is not valid.  If true, the error is announced by a popup dialog.  If false the error
            message will appear in the status line.

        The side effect of calling this method is propagation of the current user input to the
        underlying PresentedRow instance.  If the value is valid, it will be stored in the row.  If
        not, the row will recognize its state as changed but invalid.

        Returns: True if the field value is valid and False otherwise.

        """
        error = self._validate()
        if error:
            errmsg = error.message()
        else:
            errmsg = self._check()
        if errmsg:
            if interactive:
                colname = self.spec().label() or self.spec().column_label() or self.spec().id()
                app.error(colname + ": " + errmsg, title=_("Invalid value"))
            else:
                app.echo(errmsg, kind='error')
        return error is None

    def valid(self):
        """Return True if the current field value is valid and False otherwise.

        The return value reflects both, the data type validation as well as the
        per field check function.  If either of these fails, False is returned.

        """
        return self._valid() and self._last_check_result is None

    def set_focus(self):
        """Make the field active for user input.

        Passing initial=True will suppress showing completer popup which is not
        desired when the field is initially autofocused on form creation.

        """
        self._want_focus = True

    def enabled(self):
        """Return true if the field is editable by the user.

        The field may be disabled for several reasons:
          * the field was created as read-only,
          * the user doesn't have sufficient permissions,
          * the field is not editable from definition (permanently or because of an editability
            condition, typically dependence on values of some other fields in the form).

        """
        return self._enabled

    def insert_text(self, text):
        """Insert given text into the field in the current place of the cursor."""
        self._controls[0][0].WriteText(text)

    def connection_closed(self):
        """Return True if closed transaction was detected in background threads.

        True is returned if a background database operation, such as
        validation, caught 'pytis.data.DBRetryException' or
        'pytis.data.DBSystemException'.  The parent form should watch for this
        situation and react properly if necessary.

        """
        return self._connection_closed

    def tab_navigated_widgets(self):
        """Return a tuple of 'wx.Window' subclasses present in form tab navigation."""
        return (self._controls[0][0],)

    # Implementation of Public API 'pytis.api.Field'.

    def api_refresh(self):
        pass

    def api_write(self, text):
        self._controls[0][0].WriteText(text)

    def api_on_list_change(self, callback):
        raise TypeError("List change callback not supported by %s." % self.__class__.__name__)



class Unlabeled(object):
    """Mix-in třída pro políčka .

    Některé prvky mají label spojen přímo s controlem, takže label zobrazený
    v gridu musí být prázdný.

    """
    def _create_label(self, parent):
        return None


class TextField(InputField):
    """Textové vstupní políčko."""

    class TextValidator(wx.Validator):

        def __init__(self, control, filter):
            wx.Validator.__init__(self)
            self._control = control
            self._filter = filter
            wx_callback(wx.EVT_CHAR, self, self._on_char)

        def Clone(self):
            return TextField.TextValidator(self._control, self._filter)

        def _on_char(self, event):
            key = event.GetKeyCode()
            if ((self._filter is not None and
                 key >= wx.WXK_SPACE and key != wx.WXK_DELETE and key <= 255 and
                 not self._filter(chr(key)))):
                app.echo(_("Invalid character!"), kind='error')
                return True
            else:
                event.Skip()
                return True

    NUMBERS = [str(x) for x in range(10)]
    SIGNS = ['-', '+']
    DECIMAL_POINTS = ['.', ',']
    FLOAT = [str(x) for x in range(10)] + SIGNS + DECIMAL_POINTS
    ASCII = [chr(x) for x in range(127)]
    LETTERS = [chr(x) for x in (list(range(ord('a'), ord('z') + 1)) +
                                list(range(ord('A'), ord('Z') + 1)))]

    def _create_ctrl(self, parent):
        control = wx.TextCtrl(parent, -1, '', style=self._text_ctrl_style(),
                              size=field_size(parent, self.width(), self.height()))
        maxlen = self._maxlen()
        if maxlen is not None and self.height() == 1:
            # Setting max length on multiline TextCtrl fields is not supported
            # on wx 3.x and later.  Thus we limit the length only on single line
            # fields and instead of handling wx.EVT_TEXT_MAXLEN we implement our
            # own maxlen check in _on_change().
            control.SetMaxLength(maxlen)
        filter = self._filter()
        control.SetValidator(self.TextValidator(control, filter=filter))
        wx_callback(wx.EVT_TEXT, control, self._on_change)
        wx_callback(wx.EVT_TEXT_ENTER, control, self._on_enter_key)
        if not self._denied and not self._readonly and self._row.has_completer(self.id()):
            control.AutoComplete(_Completer(self._row, self.id()))
        return control

    def _set_ctrl_editable(self, ctrl, editable):
        ctrl.SetEditable(editable)
        if editable:
            color = self.DEFAULT_FIELD_BACKGROUND_COLOR
            validator = self.TextValidator(ctrl, filter=self._filter())
        else:
            color = self.DISABLED_FIELD_BACKGROUND_COLOR
            validator = wx.DefaultValidator
        ctrl.SetValidator(validator)
        ctrl.SetOwnBackgroundColour(color)

    def _text_ctrl_style(self):
        style = wx.TE_PROCESS_ENTER
        if self.height() and self.height() > 1:
            style |= wx.TE_MULTILINE
        return style

    def _maxlen(self):
        """Vrať maximální délku zadaného textu."""
        return None

    def _on_enter_key(self, event):
        if self.height() and self.height() > 1:
            event.Skip()
        else:
            event.GetEventObject().Navigate()

    def _on_change(self, event=None):
        value = self._get_value()
        post_process = self._post_process_func()
        if post_process:
            value = post_process(value)
            if value != self._ctrl.GetValue():
                selection = self._ctrl.GetSelection()
                self._ctrl.SetValue(value)
                self._ctrl.SetSelection(*selection)
        maxlen = self._maxlen()
        if maxlen is not None and len(value) > maxlen:
            # wx 3.x and later does not support wx.EVT_TEXT_MAXLEN on multiline TextCtrl fields.
            # Thus we handle maxlen ourselves here without using wx.EVT_TEXT_MAXLEN altogether.
            app.echo(_("Maximal length exceeded."), kind='error')
        super(TextField, self)._on_change(event=event)

    def _post_process_func(self):
        """Vrať funkci odpovídající specifikaci postprocessingu políčka.

        Vrací: Funkci jednoho argumentu (původní text), která vrací
        řetězec (změněný text).

        """
        try:
            return self._stored_post_process_func
        except Exception:
            pp_spec = self.spec().post_process()
            if callable(pp_spec):
                self._stored_post_process_func = pp_spec
            else:
                mapping = {
                    None: None,
                    PostProcess.UPPER: lambda s: s.upper(),
                    PostProcess.LOWER: lambda s: s.lower(),
                }
                assert pp_spec in mapping.keys()
                self._stored_post_process_func = mapping[pp_spec]
            return self._stored_post_process_func

    def _filter(self):
        """Vrať filtrační funkci odpovídající specifikaci políčka.

        Vrací: Funkci jednoho argumentu, která vrací pravdu, pokud znak
        odpovídá specifikaci filtru pro dané políčko, nepravdu v opačném
        případě.

        Pokud políčko nemá nastavenu filtraci, vrací None.

        """
        filter_spec = self.spec().filter()
        if filter_spec is None:
            return None
        if filter_spec == TextFilter.EXCLUDE_LIST:
            return lambda char, list=self.spec().filter_list(): char not in list
        mapping = {
            TextFilter.ASCII: self.ASCII,
            TextFilter.ALPHA: self.LETTERS,
            TextFilter.FLOAT: self.FLOAT,
            TextFilter.ALPHANUMERIC: self.LETTERS + self.NUMBERS,
            TextFilter.NUMERIC: self.NUMBERS,
            TextFilter.INCLUDE_LIST: self.spec().filter_list(),
        }
        assert filter_spec in mapping.keys()
        return lambda char, list=mapping[filter_spec]: char in list

    def _get_value(self):
        return self._ctrl.GetValue()

    def _set_value(self, value):
        assert isinstance(value, basestring), value
        self._ctrl.SetValue(value)
        self._on_change()  # call manually, since SetValue() doesn't emit an event.

    def _menu(self):
        return super(TextField, self)._menu() + \
            (None,
             UICommand(Command(TextField.cut), _("Cut"),
                       _("Cut the selected text to clipboard.")),
             UICommand(Command(TextField.copy), _("Copy"),
                       _("Copy the selected text to clipboard.")),
             UICommand(Command(TextField.paste), _("Paste"),
                       _("Paste text from clipboard.")),
             UICommand(Command(TextField.select_all), _("Select All"),
                       _("Select entire field value.")))

    # Commands

    @Command.define
    def cut(self):
        ctrl = self._current_ctrl()
        selection = ctrl.GetSelection()
        self.copy()
        ctrl.Remove(*selection)
        self._on_change()

    def _can_cut(self):
        ctrl = self._current_ctrl()
        return hasattr(ctrl, 'CanCut') and ctrl.CanCut()

    @Command.define
    def copy(self):
        ctrl = self._current_ctrl()
        if self.height() and self.height() > 1:
            # Calling Copy on a multiline field raises: "wxAssertionError:
            # C++ assertion "IsSingleLine()" failed at
            # .../wxWidgets/src/gtk/textctrl.cpp(853)
            # in GetEditable(): shouldn't be called for multiline"
            # since wxPython 4.0.4.  Thus we need to work around:
            copy_to_clipboard(ctrl.GetStringSelection())
        else:
            ctrl.Copy()

    def _can_copy(self):
        ctrl = self._current_ctrl()
        return hasattr(ctrl, 'CanCopy') and ctrl.CanCopy()

    @Command.define
    def paste(self):
        paste_from_clipboard(self._current_ctrl())
        self._on_change()

    def _can_paste(self):
        ctrl = self._current_ctrl()
        if hasattr(ctrl, 'CanPaste'):
            try:
                return ctrl.CanPaste()
            except wx.wxAssertionError as e:
                # This error has been observed on Linux when attempting to paste text
                # by clicking the middle mouse button.  Ignoring it looks safe here.
                if not str(e).endswith('reentrancy in clipboard code'):
                    raise
        return False

    @Command.define
    def select_all(self):
        return self._current_ctrl().SetSelection(-1, -1)

    def _can_select_all(self):
        ctrl = self._current_ctrl()
        return hasattr(ctrl, 'SetSelection') and ctrl.GetValue()


class StringField(TextField):
    """Textové vstupní políčko pro data typu 'pytis.data.String'."""

    def _maxlen(self):
        return self._type.maxlen()


class PasswordField(StringField):
    _ORIGINAL_VALUE = u'\u2024' * 8

    def _text_ctrl_style(self):
        return super(PasswordField, self)._text_ctrl_style() | wx.TE_PASSWORD

    def _create_widget(self, parent, ctrl):
        widget = super(PasswordField, self)._create_widget(parent, ctrl)
        if self._type.verify():
            self._ctrl2 = ctrl2 = self._create_ctrl(parent)
            self._controls.append((ctrl2, self._set_ctrl_editable))
            widget = self._vbox(widget, ctrl2)
        else:
            self._ctrl2 = None
        return widget

    def _icons(self, parent):
        icons = super(PasswordField, self)._icons(parent)
        if not self._readonly:
            self._toggle = toggle = wx.StaticBitmap(parent, bitmap=InputField.icon('eye'))
            toggle.SetToolTip(_("Show/hide password"))
            wx_callback(wx.EVT_LEFT_DOWN, toggle, self._on_toggle)
            icons.insert(0, (toggle, 0, wx.LEFT, 4))
        return icons

    def _on_toggle(self, event):
        if self._ctrl.IsEditable():
            style = self._ctrl.GetWindowStyleFlag() ^ wx.TE_PASSWORD
            self._toggle.SetBitmap(InputField.icon('eye' if style & wx.TE_PASSWORD
                                                   else 'eye-stroke'))
            for ctrl in (self._ctrl, self._ctrl2):
                if ctrl:
                    ctrl.SetWindowStyle(style)
        event.Skip()

    def _set_value(self, value):
        if value:
            value = self._ORIGINAL_VALUE
        super(PasswordField, self)._set_value(value)
        if self._ctrl2:
            self._ctrl2.SetValue(value)

    def _validate(self):
        value = self._get_value()
        if value == self._ORIGINAL_VALUE:
            return None
        if self._ctrl2:
            verify = self._ctrl2.GetValue()
        else:
            verify = value
        return self._row.validate(self.id(), value, verify=verify)

    def tab_navigated_widgets(self):
        widgets = super(PasswordField, self).tab_navigated_widgets()
        if self._ctrl2:
            widgets += (self._ctrl2,)
        return widgets


class SpinnableField(InputField):
    """Field capable of spinning its value up/down (incrementing/decrementing)."""

    _SPIN_STEP = None
    """Value incremented/decremented on each spin step.

    This constant must be set by derived classes to the value which is incremented/decremented
    to/from the current field value on each spin command.  Thus the value must be compatible for
    addition/subtraction with the internal value of the field's type.

    """
    def _spin(self, value, up=True):
        """Return the incremented (if 'up' is true) or decremented (if 'up' is false) 'value'.

        The derived classes will usually just define '_SPIN_STEP' value, but for more complicated
        spinning logic, it is possible to override this method as well.

        """
        if value is None:
            return None
        elif up:
            value += self._SPIN_STEP
        else:
            value -= self._SPIN_STEP
        return value

    @Command.define
    def spin(self, up=True):
        value = self._row[self._id].value()
        new_value = self._spin(value, up=up)
        ctrl = self._ctrl
        if isinstance(ctrl, wx.TextCtrl) and ctrl.GetStringSelection() == ctrl.GetValue() != '':
            select_all = True
        else:
            select_all = False
        self._row[self._id] = pytis.data.Value(self.type(), new_value)
        if select_all:
            ctrl.SetSelection(-1, -1)

    def _can_spin(self, up=True):
        return self._valid()


class NumericField(TextField, SpinnableField):
    """Textové vstupní políčko pro data typu 'pytis.data.Number'."""
    _SPIN_STEP = 1

    def _text_ctrl_style(self):
        return super(NumericField, self)._text_ctrl_style() | wx.TE_RIGHT

    def _create_widget(self, parent, ctrl):
        widget = super(NumericField, self)._create_widget(parent, ctrl)
        if self._spec.slider():
            slider = wx.Slider(parent, -1, style=wx.SL_HORIZONTAL,
                               minValue=self._type.minimum() or 0,
                               maxValue=(self._type.maximum() is None and
                                         100 or self._type.maximum()),
                               size=(200, 25))
            self._controls.append((slider, lambda c, e: c.Enable(e)))

            def on_slider(event):
                ctrl.SetValue(str(slider.GetValue()))
                self._on_change()
            wx_callback(wx.EVT_SCROLL, slider, on_slider)

            def on_idle(event):
                try:
                    position = int(ctrl.GetValue())
                except (ValueError, TypeError):
                    position = slider.GetMin()
                slider.SetValue(position)
            wx_callback(wx.EVT_IDLE, slider, on_idle)
            widget = self._hbox(widget, slider)
        return widget


class CheckBoxField(Unlabeled, InputField):
    """Boolean control implemented using 'wx.CheckBox'."""

    class ReadOnlyValidator(wx.Validator):

        def __init__(self):
            wx.Validator.__init__(self)
            # Eat all interaction events without calling e.Skip()
            # so no default event processing takes place.
            wx_callback(wx.EVT_KEY_DOWN, self, lambda e: None)
            wx_callback(wx.EVT_LEFT_DOWN, self, lambda e: None)
            wx_callback(wx.EVT_LEFT_DCLICK, self, lambda e: None)

        def Clone(self):
            return CheckBoxField.ReadOnlyValidator()

    def _create_ctrl(self, parent):
        """Vrať instanci 'wx.CheckBox'."""
        control = wx.CheckBox(parent, -1, self.spec().label())
        wx_callback(wx.EVT_CHECKBOX, control, self._on_change)
        return control

    def _get_value(self):
        return self._ctrl.GetValue() and 'T' or 'F'

    def _set_value(self, value):
        assert value in ('T', 'F', ''), ('Invalid value', value)
        self._ctrl.SetValue(value == 'T')
        self._on_change()  # call manually, since SetValue() doesn't emit an event.

    def _set_ctrl_editable(self, ctrl, editable):
        if self._readonly:
            # Avoid graying the field out in read only forms.
            ctrl.SetValidator(self.ReadOnlyValidator())
        else:
            ctrl.Enable(editable)


class GenericEnumerationField(InputField):

    def _init_attributes(self):
        super(GenericEnumerationField, self)._init_attributes()
        self._enumeration_changed = False
        self._row.register_callback(self._row.CALL_ENUMERATION_CHANGE, self._id,
                                    self._on_enumeration_change)

    def _on_enumeration_change(self):
        # Callback může být volán i když už je list mrtev.
        if not self._readonly:
            self._needs_validation = True
            self._enumeration_changed = True

    def _on_idle(self, event):
        super(GenericEnumerationField, self)._on_idle(event)
        transaction = self._row.transaction()
        if self._enumeration_changed and (not transaction or transaction.open()):
            self._enumeration_changed = False
            self._reload_enumeration()

    def _reload_enumeration(self):
        raise ProgramError("Runtime enumeration changes not supported for %s." % self.__class__)

    def reload_enumeration(self):
        """Force reloading of field's enumeration data.

        Public method to be used by application code in specific cases,
        typically when the enumeration depends on some external condition which
        is not detected by the PresentedRow row automatic callbacks.

        """
        self._reload_enumeration()

    # Implementation of Public API 'pytis.api.Field'.

    def api_refresh(self):
        try:
            self._reload_enumeration()
        except ProgramError:
            pass


class RadioBoxField(Unlabeled, GenericEnumerationField):
    """Field with a fixed enumeration represented by 'wx.RadioBox'.

    Field specification interpretation details:

      orientation -- the individual radio buttons will be aligned horizontaly or vertically.
      width -- max number of columns (if the orientation is horizontal)
      height -- max number of rows (if the orientation is vertical)

    """
    _DEFAULT_WIDTH = 0
    _DEFAULT_HEIGHT = 0

    def _create_ctrl(self, parent):
        spec = self._spec
        if spec.orientation() == Orientation.HORIZONTAL:
            style = wx.RA_SPECIFY_COLS
            dimension = self.width()
        else:
            style = wx.RA_SPECIFY_ROWS
            dimension = self.height()
        label = spec.label()
        if label:
            label = label + ':'
        # Radio Box enumeration is STATIC.
        choices = self._row.enumerate(self.id())
        default = spec.default()
        if default is None:
            default = self._type.default_value().value()
        if not self._type.not_null() or default is None or not choices:
            # Even NOT NULL fields may initially have NULL value when
            # 'default' is not set.  We also add a null choice in case
            # the choices are empty (empty radiobox is not allowed in wx).
            choices.insert(0, (None, spec.null_display() or ''))
        self._radio_values = [self._type.export(value) for value, choice in choices]
        control = wx.RadioBox(parent, -1, label, style=style, majorDimension=dimension,
                              choices=[choice for value, choice in choices])
        wx_callback(wx.EVT_RADIOBOX, control, self._on_change)
        return control

    def _get_value(self):
        i = self._ctrl.GetSelection()
        if i == wx.NOT_FOUND:
            value = None
        else:
            value = self._radio_values[i]
        return value

    def _set_value(self, value):
        assert isinstance(value, basestring), value
        try:
            selection = self._radio_values.index(value)
        except ValueError:
            pass
        else:
            self._ctrl.SetSelection(selection)
            self._on_change()  # call manually, since SetSelection() doesn't emit an event.


class EnumerationField(GenericEnumerationField):
    """Common base class for fields based on 'wx.ControlWithItems'.

    'wx.ControlWithItems' allows the items to be updated dynamically (as
    opposed to static enumeration controls, such as 'wx.RadioBox').

    """
    _INVALID_SELECTION = wx.NOT_FOUND

    def _enumeration(self):
        return self._row.enumerate(self.id())

    def _append_items(self, ctrl):
        for value, label in self._enumeration():
            ctrl.Append(label, self._type.export(value))

    def _get_value(self):
        i = self._ctrl.GetSelection()
        if i == wx.NOT_FOUND:
            value = self._type.export(None)
        else:
            value = self._ctrl.GetClientData(i)
        return value

    def _set_value(self, value):
        if self._enumeration_changed:
            self._enumeration_changed = False
            self._reload_enumeration()
        assert isinstance(value, basestring), value
        for i in range(self._ctrl.GetCount()):
            if self._ctrl.GetClientData(i) == value:
                selection = i
                break
        else:
            selection = self._INVALID_SELECTION
        self._ctrl.SetSelection(selection)
        self._on_change()  # call manually, since SetSelection() doesn't emit an event.

    def _reload_enumeration(self):
        orig_value = self._get_value()
        self._ctrl.Clear()
        self._append_items(self._ctrl)
        self._set_value(orig_value)
        self._update_size(self._ctrl)

    def _update_size(self, ctrl):
        ctrl.SetSize(ctrl.GetBestSize())


class ChoiceField(EnumerationField):
    """Field with a fixed enumeration represented by 'wx.Choice'."""
    _INVALID_SELECTION = 0

    def _enumeration(self):
        return [(None, self._spec.null_display() or '')] + super(ChoiceField, self)._enumeration()

    def _create_ctrl(self, parent):
        control = wx.Choice(parent)
        self._append_items(control)
        wx_callback(wx.EVT_CHOICE, control, self._on_change)
        self._update_size(control, initial=True)
        return control

    def _update_size(self, ctrl, initial=False):
        ctrl.SetSize((ctrl.GetBestSize().width, 30))
        if not initial:
            # Necessary to move the help icon properly when the control size changes.
            ctrl.SetMinSize(ctrl.Size)
            ctrl.Parent.Sizer.Layout()


class ListBoxField(EnumerationField):
    """Field with a fixed enumeration represented by 'wx.ListBox'."""
    _DEFAULT_HEIGHT = None

    def _create_ctrl(self, parent):
        control = wx.ListBox(parent, style=wx.LB_SINGLE | wx.LB_NEEDED_SB)
        self._append_items(control)
        self._update_size(control)
        wx_callback(wx.EVT_LISTBOX, control, self._on_change)
        return control

    def _update_size(self, ctrl):
        width = ctrl.GetBestSize().width
        min_char_width = self.spec().width(None)
        if min_char_width:
            min_width = dlg2px(ctrl, 4 * min_char_width)
            # When width is specified, it is used as minimal field width (used
            # when the list is empty or contains only short values) and also
            # the maximal field size is restricted to 3x the specified width to
            # avoid very wide field when values are too long.
            width = min(max(width, min_width), 3 * min_width)
        height = int(char2px(ctrl, 1, 1)[1] * (self.height() or ctrl.GetCount()) * 1.46)
        ctrl.SetMinSize((width, height))

    def _set_value(self, value):
        super(ListBoxField, self)._set_value(value)
        selection = self._ctrl.GetSelection()
        if selection != self._INVALID_SELECTION:
            self._ctrl.SetFirstItem(selection)


class Invocable(CommandHandler):
    """Mix-in class for fields capable to invoke a selection.

    The selection can be an enumeration (such as codebook selection) or just a dialog with some
    specific representation of the selected value (such as color selection or date selection).  The
    selection dialog is usually modal.

    The input control will be accompanied with an invocation button and will also handle the
    INVOKE_SELECTION command.

    """
    _INVOKE_TITLE = _("Select")
    _INVOKE_HELP = None
    _INVOKE_ICON = 'invoke-selection'

    @classmethod
    def command_handler_instance(cls):
        return InputField.command_handler_instance()

    def _create_widget(self, parent, ctrl):
        widget = super(Invocable, self)._create_widget(parent, ctrl)
        button = self._create_invocation_button(parent)
        if button:
            self._controls.append((button, lambda c, e: c.Enable(e)))
            wx_callback(wx.EVT_NAVIGATION_KEY, button, self._on_navigation(button, skip=True))
            wx_callback(wx.EVT_BUTTON, button, lambda e: self._on_invoke_selection(ctrl))
            widget = self._hbox(widget, button)
        self._invocation_button = button
        return widget

    def _button_size(self, parent):
        return field_size(parent, 1, 1)

    def _create_invocation_button(self, parent):
        return wx_button(parent, icon=self._INVOKE_ICON,
                         label='...' if not self._INVOKE_ICON else None,
                         size=self._button_size(parent), tooltip=self._INVOKE_TITLE)

    def _menu(self):
        return super(Invocable, self)._menu() + \
            (None,
             UICommand(Command(self.invoke_selection), self._INVOKE_TITLE, self._INVOKE_HELP))

    def _on_invoke_selection(self, ctrl, alternate=False):
        raise ProgramError("This method must be overriden!")

    @Command.define
    def invoke_selection(self, **kwargs):
        self._on_invoke_selection(self._ctrl, **kwargs)

    def _can_invoke_selection(self, **kwargs):
        return self.enabled()


class DateField(Invocable, TextField, SpinnableField):
    """Input field for values of type 'pytis.data.Date'.

    The field implements selection invocation using a calendar widget.

    The field also supports spinning (see 'SpinnableField') by one day per one step.

    """

    _DEFAULT_WIDTH = 10
    _INVOKE_TITLE = _("Select from Calendar")
    _INVOKE_HELP = _("Show the calendar for date selection.")
    _SPIN_STEP = datetime.timedelta(days=1)

    def _date_type(self):
        return self._type

    def _on_invoke_selection(self, ctrl, alternate=False):
        t = self._date_type()
        value, error = t.validate(ctrl.GetValue())
        date = pytis.form.app.run_dialog(Calendar, value and value.value())
        if date is not None:
            ctrl.SetValue(t.export(date))


class DateTimeField(DateField):
    """Input field for values of type 'pytis.data.DateTime'.

    The date part can be changed using the calendar widget.

    Spinning changes the date by one day per one step.

    """
    _DEFAULT_WIDTH = 19

    def _on_invoke_selection(self, ctrl, alternate=False):
        t = self._date_type()
        value, error = t.validate(ctrl.GetValue())
        dt = value and value.value()
        date = pytis.form.app.run_dialog(Calendar, dt)
        if date is not None:
            if dt:
                kwargs = dict(hour=dt.hour, minute=dt.minute, second=dt.second)
            else:
                kwargs = {}
            dt = datetime.datetime(year=date.year, month=date.month, day=date.day,
                                   tzinfo=t.timezone(), **kwargs)
            ctrl.SetValue(t.export(dt))


class TimeField(TextField, SpinnableField):
    """Input field for values of type 'pytis.data.Time'.

    The field also supports spinning (see 'SpinnableField') by one hour per one step.

    """
    _SPIN_STEP = datetime.timedelta(hours=1)

    def _spin(self, value, up=True):
        if value is None:
            return None
        date = datetime.date.today()
        # datetime.time doesn't support addition/subtraction of datetime.timedelta, so
        # we need to convert it to a datetime first and extract the time part afterwards.
        dt = super(TimeField, self)._spin(datetime.datetime.combine(date, value), up=up)
        return dt.time() if dt.date() == date else value


class ColorSelectionField(Invocable, TextField):
    """Vstupní pole pro výběr barvy."""

    _DEFAULT_WIDTH = 9  # Value is max. 7 chars, but may include wide letters, such as D.
    _INVOKE_TITLE = _("Select Color")
    _INVOKE_HELP = _("Show the color selection dialog.")

    def _on_invoke_selection(self, ctrl, alternate=False):
        color = pytis.form.app.run_dialog(ColorSelector, self._get_value())
        if color is not None:
            self._set_value(color)

    def _create_invocation_button(self, parent):
        return wx.lib.colourselect.ColourSelect(parent, -1, size=self._button_size(parent))

    def _set_value(self, value):
        self._invocation_button.SetColour(value or '#e8e8e8')
        return super(ColorSelectionField, self)._set_value(value)


class GenericCodebookField(GenericEnumerationField):
    """Společná nadtřída číselníkových políček."""

    def _init_attributes(self):
        cb_name = self._row.codebook(self._id)
        assert cb_name is not None
        try:
            cb_spec = pytis.config.resolver.get(cb_name, 'cb_spec')
        except ResolverError:
            cb_spec = CodebookSpec()
        self._cb_name = cb_name
        self._cb_spec = cb_spec
        super(GenericCodebookField, self)._init_attributes()

    def _reload_enumeration(self):
        pass

    def _cb_key(self):
        value = self._row[self._id]
        if self._valid() and value.value():
            return {self._type.enumerator().value_column(): value}
        else:
            return None

    def _codebook_arguments(self):
        return self._row.runtime_arguments(self.id())

    def _run_codebook_form(self, begin_search=None):
        """Zobraz číselník a po jeho skončení nastav hodnotu políčka."""
        enumerator = self._type.enumerator()
        validity_condition = enumerator.validity_condition()
        runtime_filter_condition = self._row.runtime_filter(self.id())
        if validity_condition and runtime_filter_condition:
            condition = pytis.data.AND(validity_condition, runtime_filter_condition)
        else:
            condition = validity_condition or runtime_filter_condition
        result = run_form(pytis.form.CodebookForm, self._cb_name, begin_search=begin_search,
                          select_row=self._cb_key(), transaction=self._row.transaction(),
                          condition=condition, arguments=self._codebook_arguments())
        if result:  # may be None or False!
            self._set_value(result.format(enumerator.value_column()))
        self.set_focus()

    def _codebook_insert(self):
        value_column = self._type.enumerator().value_column()
        fspec = self.spec()
        prefill_function = fspec.codebook_insert_prefill()
        if prefill_function:
            prefill = prefill_function(self._row)
        else:
            prefill = {}
        if not self._valid() and self._modified():
            value, error = self._type.validate(self._get_value(), strict=False)
            if not error:
                prefill[value_column] = value
        spec_name = fspec.codebook_insert_spec() or self._cb_name
        result = app.new_record(spec_name, prefill=prefill, transaction=self._row.transaction())
        if result and value_column in result:
            self._set_value(result[value_column].export())

    @Command.define
    def invoke_codebook_form(self):
        self._run_codebook_form()


class CodebookField(Invocable, GenericCodebookField, TextField):
    """Vstupní pole pro data navázaná na číselník.

    Bude použito v případě, že datový typ definuje enumerátor typu
    'pytis.data.DataEnumerator' a prezentační specifikace políčka definuje
    navázaný číselník (viz. argument 'codebook' konstruktoru 'Field').

    Jako akci pro vyvolání výběru definuje zobrazení formuláře
    'pytis.form.CodebookForm'.  Název specifikace číselníku je dán výše
    zmíněným specifikátorem 'codebook'.  Další vlastnosti číselníkového
    formuláře jsou dány jednak specifikací 'cb_spec' v odkazované specifikaci a
    jednak přímo specifikací 'view_spec' tamtéž.

    K políčku může být volitelně přidružen displej, který slouží k zobrazení
    popisu vybrané (aktuální) hodnoty číselníku.

    """
    _INVOKE_TITLE = _("Select from Codebook")
    _INVOKE_HELP = _("Show the selection of available codebook values.")

    def _init_attributes(self):
        self._display = None
        super(CodebookField, self)._init_attributes()

    def _create_widget(self, parent, ctrl):
        """Zavolej '_create_widget()' třídy Invocable a přidej displej."""
        widget = super(CodebookField, self)._create_widget(parent, ctrl)
        spec = self.spec()
        cb_spec = self._cb_spec
        if cb_spec.display() is None and not spec.allow_codebook_insert():
            return widget
        added_controls = []
        if cb_spec.display():
            display_size = spec.display_size()
            if display_size is None:
                display_size = cb_spec.display_size()
            if display_size:
                size = field_size(parent, display_size, 1)
                display = wx.TextCtrl(parent, style=wx.TE_READONLY, size=size)
                display.SetOwnBackgroundColour(self.DISABLED_FIELD_BACKGROUND_COLOR)
                self._display = display
                wx_callback(wx.EVT_NAVIGATION_KEY, display, self._on_navigation(display, skip=True))
                self._controls.append((display, lambda c, e: None))
                added_controls.append(display)
        if spec.allow_codebook_insert():
            button = wx_button(parent, icon='new-record', size=self._button_size(parent),
                               tooltip=_("Insert a new codebook value."),
                               callback=lambda e: self._codebook_insert())
            wx_callback(wx.EVT_NAVIGATION_KEY, button, self._on_navigation(button, skip=True))
            self._controls.append((button, lambda b, e: b.Enable(e)))
            added_controls.append(button)
        if added_controls:
            widget = self._hbox(widget, *added_controls)
        return widget

    def _menu(self):
        return super(CodebookField, self)._menu() + \
            (UICommand(Command(self.invoke_selection, alternate=True),
                       _("Search in codebook"),
                       _("Show the codebook and start incremental search.")),)

    def _maxlen(self):
        try:
            return self._type.maxlen()
        except AttributeError:
            return None

    def _set_value(self, value):
        super(CodebookField, self)._set_value(value)
        self._update_display()

    def _on_change_hook(self):
        super(CodebookField, self)._on_change_hook()
        self._update_display()

    def _update_display(self):
        if self._display:
            if self._readonly or self._valid():
                display = self._row.display(self.id())
            else:
                display = ''
            self._display.SetValue(display)

    def _on_invoke_selection(self, ctrl, alternate=False):
        value_column = self._type.enumerator().value_column()
        value = self._get_value()
        if ((not self._valid() and value and self._modified() and
             isinstance(self.type(), pytis.data.String))):
            begin_search = (value_column, value)
        elif alternate:
            begin_search = value_column
        else:
            begin_search = self._cb_spec.begin_search()
        self._run_codebook_form(begin_search=begin_search)


class ListField(GenericCodebookField, CallbackHandler):
    """Číselníkové políčko zobrazující data číselníku jako součást formuláře.

    Pokud je 'selection_type' číselníkového políčka ve specifikaci určen jako 'LIST', bude ve
    formuláři použit tento typ vstupního pole.

    """
    _DEFAULT_WIDTH = 30
    _DEFAULT_HEIGHT = 6

    CALL_LIST_CHANGE = 'CALL_LIST_CHANGE'
    """Callback called on list modification (codebook values inserted/edited/deleted)."""

    def __init__(self, parent, row, id, **kwargs):
        GenericCodebookField.__init__(self, parent, row, id, **kwargs)
        CallbackHandler.__init__(self)

    def _create_ctrl(self, parent):
        style = wx.LC_REPORT | wx.LC_SINGLE_SEL
        self._list = listctrl = wx.ListCtrl(parent, -1, style=style)
        # Set up column headings according to specification.
        view_spec = pytis.config.resolver.get(self._cb_name, 'view_spec')
        self._columns = columns = self._cb_spec.columns() or view_spec.columns()
        total_width = 0
        for i, cid in enumerate(columns):
            col = view_spec.field(cid)
            if isinstance(col.type(), pytis.data.Number):
                attr = wx.LIST_FORMAT_RIGHT
            else:
                attr = wx.LIST_FORMAT_LEFT
            listctrl.InsertColumn(i, col.column_label(), attr)
            width = col.column_width()
            if width < len(col.column_label()):
                width = len(col.column_label())
            listctrl.SetColumnWidth(i, dlg2px(listctrl, 4 * (width + 1)))
            total_width = total_width + width
        height = listctrl.GetCharHeight() * self.height() * 1.4 + 26  # TODO: any better?
        self._DEFAULT_WIDTH = total_width + 3
        listctrl.SetMinSize((dlg2px(listctrl, 4 * (self.width() + 1)), height))
        wx_callback(wx.EVT_LIST_ITEM_SELECTED, listctrl, self._on_select_or_activate)
        wx_callback(wx.EVT_LIST_ITEM_ACTIVATED, listctrl, self._on_select_or_activate)
        wx_callback(wx.EVT_MOUSEWHEEL, listctrl, lambda e: e.Skip())
        wx_callback(wx.EVT_LIST_KEY_DOWN, listctrl, self._on_key_down)
        self._selected_item = None
        self._enumeration_changed = True
        self._list_data = []
        self._last_set_invalid_value = None
        return listctrl

    def _set_ctrl_editable(self, ctrl, editable):
        # Disabling the control also disables scrolling.
        # Instead we simply don't perform selection changes when disabled.
        if editable:
            ctrl.SetBackgroundColour(InputField.DEFAULT_FIELD_BACKGROUND_COLOR)
        else:
            ctrl.SetBackgroundColour(InputField.DISABLED_FIELD_BACKGROUND_COLOR)

    def _change_callback(self):
        self._reload_enumeration()
        super(ListField, self)._change_callback()

    def _on_select_or_activate(self, event):
        # We want to ACTIVATE the item on single mouse click, but wx
        # default behavior is to focus on single click and activate
        # on double click.
        i = event.GetIndex()
        if i != self._selected_item:
            if self._enabled:
                self._set_selection(i)
            else:
                # Force return previous selection as wx has already selected the
                # clicked item before calling this event handler (but without
                # changing self._selected_item).
                self._set_selection(self._selected_item, ensure_visible=False)
                self._list.Focus(i)

    def _on_key_down(self, event):
        code = event.GetKeyCode()
        i = self._list.GetFocusedItem() or 0
        if code in (wx.WXK_RETURN, wx.WXK_SPACE, wx.WXK_NUMPAD_ENTER) and self._enabled:
            self._set_selection(i)
        elif code == wx.WXK_UP and i > 0:
            self._list.Focus(i - 1)
        elif code == wx.WXK_DOWN and i + 1 < self._list.GetItemCount():
            self._list.Focus(i + 1)
        elif code == wx.WXK_PAGEUP and i > 0:
            self._list.Focus(max(i - self.height(), 0))
        elif code == wx.WXK_PAGEDOWN and i + 1 < self._list.GetItemCount():
            self._list.Focus(min(i + self.height(), self._list.GetItemCount()))

    def _reload_enumeration(self):
        if self._last_set_invalid_value is not None:
            current = self._last_set_invalid_value
        else:
            current = self._get_value()
        list = self._list
        list.DeleteAllItems()
        self._list_data = []
        select_item = None
        enumerator = self.type().enumerator()
        value_column = enumerator.value_column()
        sorting = self._cb_spec.sorting()
        if sorting is None:
            sorting = pytis.config.resolver.get(self._cb_name, 'view_spec').sorting()
        rows = enumerator.rows(condition=self._row.runtime_filter(self._id),
                               transaction=self._row.transaction(), sort=sorting or (),
                               arguments=self._codebook_arguments())
        for i, row in enumerate(rows):
            list.InsertItem(i, "")
            v = row[value_column]
            self._list_data.append(v)
            if v.export() == current:
                select_item = i
            for j, id in enumerate(self._columns):
                value = row[id]
                t = value.type()
                if not enumerator.permitted(id):
                    exported_value = t.secret_export()
                elif isinstance(t, pytis.data.Boolean):
                    exported_value = value.value() and _("Yes") or _("No")
                elif isinstance(t, pytis.data.Range):
                    # TODO: This duplicates the logic in PresentedRow.format()!!!
                    exported_value = u' — '.join(x or _("unlimited") for x in value.export())
                else:
                    exported_value = value.export().replace("\n", ";")
                list.SetItem(i, j, exported_value)
        self._set_selection(select_item)

    def _set_selection(self, i, ensure_visible=True):
        list = self._list
        if self._selected_item is not None and self._selected_item < list.GetItemCount():
            # Deselect the old item.
            fgcolor = wx.SystemSettings.GetColour(wx.SYS_COLOUR_WINDOWTEXT)
            bgcolor = wx.SystemSettings.GetColour(wx.SYS_COLOUR_WINDOW)
            list.SetItemTextColour(self._selected_item, fgcolor)
            list.SetItemBackgroundColour(self._selected_item, bgcolor)
        self._selected_item = i
        if i is not None:
            # Select the new item.
            assert i >= 0
            fgcolor = wx.SystemSettings.GetColour(wx.SYS_COLOUR_HIGHLIGHTTEXT)
            bgcolor = wx.SystemSettings.GetColour(wx.SYS_COLOUR_HIGHLIGHT)
            list.SetItemTextColour(i, fgcolor)
            list.SetItemBackgroundColour(i, bgcolor)
            list.SetItemState(i, wx.LIST_STATE_FOCUSED, wx.LIST_STATE_FOCUSED)
            if ensure_visible:
                list.EnsureVisible(i)
        self._on_change()

    def _set_value(self, value):
        self._last_set_invalid_value = None
        if value:
            for i, v in enumerate(self._list_data):
                if v.export() == value:
                    self._set_selection(i)
                    return True
            else:
                # Not in list.
                self._set_selection(None)
                self._last_set_invalid_value = value
                return False
        else:
            # Empty value.
            self._set_selection(None)
            return True

    def _get_value(self):
        i = self._selected_item
        if i is not None:
            return self._list_data[i].export()
        else:
            return ''

    def _menu(self):
        return (UICommand(Command(self.select), _("Select"),
                          _("Select this item as active.")),
                UICommand(Command(self.show_selected), _("Show selected item"),
                          _("Locate the currently selected item.")),
                None,
                UICommand(Command(self.invoke_codebook_form), _("Show codebook"),
                          _("Open the codebook form.")),
                UICommand(Command(self.edit_selected), _("Edit selected item"),
                          _("Open the selected item in edit form.")),
                UICommand(Command(self.delete_selected), _("Remove selected item"),
                          _("Remove the selected item from the codebook.")),
                UICommand(Command(self.new_codebook_record), _("Insert new item"),
                          _("Open form for new codebook record insertion.")),
                UICommand(Command(Application.run_form,
                                  form_class=pytis.form.BrowseForm,
                                  name=self._cb_name,
                                  select_row=self._cb_key()),
                          _("Show the entire table"),
                          _("Open the codebook in a standalone form.")),
                )

    def _selected_item_index(self):
        i = self._list.GetNextItem(-1, state=wx.LIST_STATE_FOCUSED)
        if i == -1:
            return None
        else:
            return i

    # Command handling

    @Command.define
    def select(self):
        self._set_selection(self._selected_item_index())

    def _can_select(self):
        if not self.enabled():
            return False
        else:
            return self._selected_item_index() is not None

    @Command.define
    def show_selected(self):
        self._set_selection(self._selected_item)

    def _can_show_selected(self):
        return self._selected_item is not None

    @Command.define
    def edit_selected(self):
        prefill_function = self.spec().codebook_update_prefill()
        if prefill_function:
            set_values = prefill_function(self._row)
        else:
            set_values = None
        transaction = self._row.transaction()
        row = self._type.enumerator().row(self._row[self._id].value(), transaction=transaction)
        app.edit_record(self._cb_name, row, set_values=set_values, transaction=transaction)
        self._reload_enumeration()
        self._run_callback(self.CALL_LIST_CHANGE, self._row)
        self.set_focus()

    def _can_edit_selected(self, **kwargs):
        return self.enabled() and self._selected_item is not None

    @Command.define
    def delete_selected(self):
        transaction = self._row.transaction()
        row = self._type.enumerator().row(self._row[self._id].value(), transaction=transaction)
        app.delete_record(self._cb_name, row,
                          question=_("Really remove the item %s from the codebook permanently?",
                                     self._row[self._id].export()),
                          transaction=transaction)
        self._reload_enumeration()
        self._run_callback(self.CALL_LIST_CHANGE, self._row)
        self.set_focus()

    def _can_delete_selected(self):
        return self.enabled() and self._selected_item is not None

    @Command.define
    def new_codebook_record(self):
        self._codebook_insert()
        self._reload_enumeration()
        self._run_callback(self.CALL_LIST_CHANGE, self._row)
        self.set_focus()

    def _can_new_codebook_record(self):
        return self.enabled()

    @Command.define
    def invoke_codebook_form(self):
        super(ListField, self).invoke_codebook_form()
        self._reload_enumeration()
        self._run_callback(self.CALL_LIST_CHANGE, self._row)
        self.set_focus()

    def _can_invoke_codebook_form(self):
        return self.enabled()

    def api_on_list_change(self, callback):
        self.set_callback(self.CALL_LIST_CHANGE, callback)


class FileField(Invocable, InputField):
    """Input field for manipulating generic binary data."""

    _INVOKE_TITLE = _("Select File")
    _INVOKE_HELP = _("Show a dialog to browse files in the file system.")
    _INVOKE_ICON = wx.ART_FILE_OPEN

    def _init_attributes(self):
        self._value = None
        super(FileField, self)._init_attributes()

    def _create_ctrl(self, parent):
        if self._spec.filename():
            size = 50
        else:
            size = 10
        ctrl = wx.TextCtrl(parent, -1, '', size=field_size(parent, size, 1))
        ctrl.SetEditable(False)
        ctrl.SetOwnBackgroundColour(InputField.DISABLED_FIELD_BACKGROUND_COLOR)
        wx_callback(wx.EVT_LEFT_DCLICK, ctrl, self._on_filename_dclick)
        return ctrl

    def _button_size(self, parent):
        return field_size(parent, 2, 1)

    def _validate(self):
        return self._row.validate(self.id(), self._value,
                                  filename=self._value and self._value.filename())

    def _get_value(self):
        return self._value

    def _set_value(self, value):
        self._value = self._type.adjust_value(value)
        if self._readonly:
            # _on_change() will not trigger _on_change_hook() for readonly
            # fields, so we need to run it manually here.
            self._on_file_changed()
        else:
            self._on_change()

    def _on_change_hook(self):
        super(FileField, self)._on_change_hook()
        self._on_file_changed()

    def _on_file_changed(self):
        filename = self._row.filename(self._id)
        display = filename or ''
        if self._value is not None:
            bytesize = format_byte_size(len(self._value))
            if display:
                display = '%s (%s)' % (display, bytesize)
            else:
                display = bytesize
        self._ctrl.SetValue(display)

    def _on_filename_dclick(self, event):
        Command(self.open).invoke()

    def _on_invoke_selection(self, ctrl, alternate=False):
        Command(self.load).invoke()

    def _filename_extension(self):
        if self._value:
            filename = self._row.filename(self._id)
            if filename:
                return os.path.splitext(filename)[1]
            else:
                return None
        else:
            return None

    def _menu(self):
        # We really want to use Invocable's super method, since we don't
        # want the Invocable menu items.
        return super(Invocable, self)._menu() + \
            (None,
             UICommand(Command(FileField.open), _("Open"),
                       _("Open the file in a preferred application.")),
             UICommand(Command(FileField.load), _("Load from file"),
                       _("Set the field value from a file.")),
             UICommand(Command(FileField.save), _("Save to file"),
                       _("Save the current field value as file.")),
             UICommand(Command(FileField.clear), _("Clear value"),
                       _("Set the field to an ampty value.")),
             )

    @Command.define
    def open(self):
        app.launch_file(data=self._value, suffix=self._filename_extension())

    def _can_open(self):
        return self._enabled and self._value is not None and self._filename_extension()

    @Command.define
    def load(self):
        # title = _("Select the file for field '%s'", self.spec().label())
        fh = app.open_selected_file(mode='rb', filetypes=self._spec.filename_extensions(),
                                    context='file-field')
        if fh:
            with fh:
                filename = app.splitpath(fh.name)[1]
                try:
                    self._value = self._type.Data(fh, filename=filename)
                except pytis.data.ValidationError as e:
                    app.echo(e.message(), kind='error')
                except IOError as e:
                    app.echo(_("Error reading file:") + ' ' + str(e), kind='error')
                else:
                    self._on_change()
                    app.echo(_("File loaded."))
        else:
            app.echo(_("Loading file canceled."))

    def _can_load(self):
        return self._enabled

    @Command.define
    def save(self):
        # msg = _("Save value of %s") % self.spec().label()
        try:
            path = app.write_selected_file(self._value, mode='wb',
                                           filename=self._row.filename(self._id),
                                           context='file-field')
        except IOError as e:
            app.echo(_("Error writing file to disk:") + ' ' + str(e), kind='error')
        else:
            if path:
                app.echo(_("File saved."))
            else:
                app.echo(_("Saving file canceled."))

    def _can_save(self):
        return self._value is not None

    @Command.define
    def clear(self):
        self._set_value(None)

    def _can_clear(self):
        return self._enabled and self._value is not None


class ImageField(FileField):
    """Input field for bitmap images showing a thumbnail within the control."""

    _DEFAULT_WIDTH = _DEFAULT_HEIGHT = 80

    def _create_ctrl(self, parent):
        return wx_button(parent, bitmap=self._bitmap(),
                         size=(self.width() + 10, self.height() + 10),
                         callback=lambda e: self._on_button())

    def _create_invocation_button(self, parent):
        if self._spec.editable() is False:
            # Hide the button only when the field is statically
            # ineditable (may not become dynamically editable).
            # The button looks odd when image field is used as
            # read only image preview.
            return None
        return super(ImageField, self)._create_invocation_button(parent)

    def _set_ctrl_editable(self, ctrl, editable):
        # Ineditable button will gray out the image displayed on it.
        # We sometimes use image fields as read only image preview.
        pass

    def _bitmap(self):
        if self._value is not None:
            import PIL.Image
            img = self._value.image().copy()
            img.thumbnail((self.width(), self.height()), PIL.Image.ANTIALIAS)
            stream = io.BytesIO()
            img.save(stream, 'PNG')
            stream.seek(0)
            return wx.Bitmap(wx.Image(stream, type=wx.BITMAP_TYPE_PNG))
        return wx.Bitmap(1, 1)

    def _on_button(self):
        Command(self.open).invoke()

    def _filename_extension(self):
        if self._value:
            return "." + self._value.image().format.lower()
        else:
            return None

    def _on_file_changed(self):
        self._ctrl.SetBitmapLabel(self._bitmap())


class StructuredTextField(TextField):

    class AttachmentEnumerator(pytis.data.Enumerator, pytis.data.TransactionalEnumerator):

        def __init__(self, storage, images=True):
            self._storage = storage
            self._images = images
            super(StructuredTextField.AttachmentEnumerator, self).__init__()

        def values(self, transaction=None):
            try:
                return [r.filename() for r in self._storage.resources(transaction=transaction)
                        if isinstance(r, lcg.Image) ^ (not self._images)]
            except AttachmentStorage.StorageError as e:
                app.error(title=_("Error accessing attachment storrage"),
                          message=_("Error accessing attachment storrage") + ':\n' + e)
                return []

    class ImageAlignments(Enumeration):
        enumeration = (('inline', _("Inline")),
                       ('left', _("Left")),
                       ('right', _("Right")))
        default = 'left'

    class ImageSizes(Enumeration):
        SMALL_THUMBNAIL_SIZE = 200
        LARGE_THUMBNAIL_SIZE = 350
        enumeration = (('small-thumbnail', _("Small preview (%d px), click to enlarge" %
                                             SMALL_THUMBNAIL_SIZE)),
                       ('large-thumbnail', _("Larger preview (%d px), click to enlarge" %
                                             LARGE_THUMBNAIL_SIZE)),
                       # ('custom-thumbnail', _("Vlastní velikost náhledu")),
                       ('full-size', _("Full size (appropriate for screenshot etc.)")))
        default = 'full-size'

        @classmethod
        def matching_size(cls, thumbnail):
            if thumbnail:
                size = max(*thumbnail.size())
                if size == cls.SMALL_THUMBNAIL_SIZE:
                    return 'small-thumbnail'
                elif size == cls.LARGE_THUMBNAIL_SIZE:
                    return 'large-thumbnail'
                else:
                    return 'custom-thumbnail'
            else:
                return 'full-size'

        @classmethod
        def thumbnail_size_bounds(cls, size, custom_size):
            if size == 'small-thumbnail':
                return (cls.SMALL_THUMBNAIL_SIZE, cls.SMALL_THUMBNAIL_SIZE)
            elif size == 'large-thumbnail':
                return (cls.LARGE_THUMBNAIL_SIZE, cls.LARGE_THUMBNAIL_SIZE)
            elif size == 'custom-thumbnail':
                return (custom_size, custom_size)
            elif size == 'full-size':
                return None

        @classmethod
        def preview_size(cls, size, custom_size, orig_size):
            if size == 'small-thumbnail':
                size = cls.SMALL_THUMBNAIL_SIZE
            elif size == 'large-thumbnail':
                size = cls.LARGE_THUMBNAIL_SIZE
            elif size == 'custom-thumbnail':
                size = custom_size
            elif size == 'full-size':
                return tuple(orig_size)
            scale = float(size) / float(max(*orig_size))
            return (round(scale * orig_size[0]), round(scale * orig_size[1]))

    class LCGLink(object):
        """Common manipulations with LCG Structured Text links.

        Reads current link properties from the field source text, provides the
        current values to the UI dialog and writes the user edited values back
        to the source text after the UI dialog is closed.

        The UI dialog is created independently.  This class only handles common
        text manipulations.  Dialogs are specific for each link type.

        """
        def __init__(self, ctrl):
            self._ctrl = ctrl
            self._target = None
            self._title = None
            self._tooltip = None
            self._align = 'inline'
            # Find out whether the current cursor position is within an
            # existing attachment link.
            self._position = position = ctrl.GetInsertionPoint()
            success, column_number, line_number = ctrl.PositionToXY(position)
            self._column_number = column_number
            line_text = ctrl.GetLineText(line_number)
            self._start = start = line_text[:column_number].rfind('[')
            self._end = end = line_text[column_number:].find(']')
            if start != -1 and end != -1:
                # If we are inside the link, read the current link properties.
                link_text = line_text[start + 1:column_number + end]
                if link_text.startswith('<'):
                    link_text = link_text[1:]
                    self._align = 'left'
                elif link_text.startswith('>'):
                    link_text = link_text[1:]
                    self._align = 'right'
                if '|' in link_text:
                    link_text, self._tooltip = [x.strip() for x in link_text.split('|', 1)]
                if ' ' in link_text:
                    link_text, self._title = link_text.split(' ', 1)
                self._target = link_text

        def target(self):
            return self._target

        def title(self):
            return self._title

        def tooltip(self):
            return self._tooltip

        def align(self):
            return self._align

        def update(self, target, title, tooltip, align=None):
            """Update link source text with values from a UI dialog."""
            link_text = target
            if title:
                link_text += ' ' + title
            if tooltip:
                if not title:
                    link_text += ' ' + target
                link_text += ' | ' + tooltip
            if align == 'left':
                link_text = '<' + link_text
            if align == 'right':
                link_text = '>' + link_text
            start, end, pos, col = self._start, self._end, self._position, self._column_number
            if start != -1 and end != -1:
                self._ctrl.Remove(pos - col + start + 1, pos + end)
                self._ctrl.WriteText(link_text)
            else:
                self._ctrl.WriteText('[' + link_text + ']')

    _HEADING_MATCHER = re.compile(r'^(?P<level>=+) (?P<title>.*) (?P=level)' +
                                  r'(?:[\t ]+(?:\*|(?P<anchor>[\w\d_-]+)))? *$')

    def _toolbar_commands(self):
        commands = ()
        if isinstance(self._guardian, pytis.form.StructuredTextEditor):
            # Add form commands only in a standalone editor, not in ordinary forms.
            commands += (
                (UICommand(Command(pytis.form.EditForm.commit_record, close=False),
                           _("Save"),
                           _("Save the record without closing the form.")),
                 ),
            )
        if isinstance(self._guardian, pytis.form.ResizableInputForm):
            # ResizableInputForm is used within open_in_editor().  Commit
            # will only return the current value (it is a virtual form), not
            # save anything to the database.
            commands += (
                (UICommand(Command(pytis.form.EditForm.commit_record),
                           _("Confirm and leave"),
                           _("Confirm the changes and leave editation.")),
                 ),
            )
        commands += (
            # (UICommand(Command(self.undo),
            #            _(u"Zpět"),
            #            _(u"Vrátit zpět poslední akci.")),
            #  UICommand(Command(self.redo),
            #            _(u"Znovu"),
            #            _(u"Provést znovu poslední akci vzatou zpět.")),
            # ),

            (UICommand(Command(self.cut),
                       _("Cut"),
                       _("Cut the selected text to clipboard.")),
             UICommand(Command(self.copy),
                       _("Copy"),
                       _("Copy the selected text to clipboard.")),
             UICommand(Command(self.paste),
                       _("Paste"),
                       _("Paste text from clipboard.")),
             ),
            # (UICommand(Command(self.search),
            #            _(u"Hledat"),
            #            _(u"Vyhledat řetězec v textu políčka.")),
            #  UICommand(Command(self.search_and_replace),
            #            _(u"Hledat a nahradit"),
            #            _(u"Vyhledat na nahradit řetězec v textu políčka.")),
            # ),
            (UICommand(Command(self.heading),
                       _("Heading level"),
                       _("Insert markup for heading of given level."),
                       ctrl=(TextHeadingSelector, dict(size=(150, None)))),
             ),
            (UICommand(Command(self.strong),
                       _("Bold text"),
                       _("Insert markup for bold text.")),
             UICommand(Command(self.emphasized),
                       _("Slanted"),
                       _("Insert markup for text emphasized by slanted font.")),
             UICommand(Command(self.underlined),
                       _("Underlined text"),
                       _("Insert markup for underlined text.")),
             ),
            (UICommand(Command(self.link),
                       _("Hypertext link"),
                       _("Insert markup hypertext link.")),
             ) +
            (UICommand(Command(self.image),
                       _("Image"),
                       _("Insert image.")),
             UICommand(Command(self.attachment),
                       _("Attachment"),
                       _("Attach file.")),
             ) if self._storage else () +
            (UICommand(Command(self.itemize, style='bullet'),
                       _("Itemized list"),
                    _("Create a bullet list item.")),
             UICommand(Command(self.itemize, style='numbered'),
                       _("Numbered list"),
                       _("Create a numbered list item.")),
             UICommand(Command(self.verbatim),
                       _("Preformatted text"),
                       _("Insert markup for preformatted text.")),
             UICommand(Command(self.linebreak),
                       _("Line break"),
                       _("Insert markup for explicit line break.")),
             ),
            (UICommand(Command(self.preview),
                       _("Show HTML preview"),
                       _("Show preview of the text formatted as HTML.")),
             UICommand(Command(self.export_pdf),
                       _("Show PDF preview"),
                       _("Show preview of the text formatted as PDF.")),
             ),
        )
        return commands

    def _menu(self):
        menu = super(StructuredTextField, self)._menu()
        if not isinstance(self._guardian,
                          (pytis.form.StructuredTextEditor, pytis.form.ResizableInputForm)):
            menu += (None,
                     UICommand(Command(self.open_in_editor),
                               _("Edit in a standalone window"),
                               ""),
                     )
        return menu

    def _create_ctrl(self, parent):
        import wx.stc

        class TextCtrl(wx.stc.StyledTextCtrl):
            """StyledTextCtrl implementing the TextCtrl API used by parent classes.

            This allows us to use StyledTextCtrl (which is normally not API
            compatible with TextCtrl) as a drop-in replacement for the TextCtrl
            widget and keep the parent classes happy.

            """
            def SetValue(self, text):
                self.ClearAll()
                self.AppendText(text)
                self.EmptyUndoBuffer()

            def GetValue(self):
                return self.GetText()

            def CanCut(self):
                start, end = self.GetSelection()
                return start != end

            def CanCopy(self):
                return self.CanCut()
        # TODO: We currently use a standard wx.TextCtrl instead of
        # wx.stc.StyledTextCtrl as it has some strange bugs in caret
        # positioning etc.  Once this is resolved, we can re-enable usiong the
        # derived TextCtrl class defined above.
        # ctrl = TextCtrl(parent, -1, style=self._text_ctrl_style())
        # wx_callback(wx.stc.EVT_STC_MODIFIED, ctrl, self._on_change)
        ctrl = wx.TextCtrl(parent, -1, style=self._text_ctrl_style(),
                           size=field_size(parent, self.width(), self.height()))
        # Set a monospace font
        ctrl.SetFont(wx.Font(ctrl.GetFont().GetPointSize(), wx.FONTFAMILY_MODERN,
                             wx.FONTSTYLE_NORMAL, wx.FONTWEIGHT_NORMAL))
        wx_callback(wx.EVT_TEXT, ctrl, self._on_change)
        self._storage = self._row.attachment_storage(self._id)
        return ctrl

    def _create_widget(self, parent, ctrl):
        widget = super(StructuredTextField, self)._create_widget(parent, ctrl)
        toolbar = wx_toolbar(parent, self._toolbar_commands())
        sizer = wx.BoxSizer(wx.VERTICAL)
        sizer.Add(toolbar, 0, wx.EXPAND)
        sizer.Add(widget, 1, wx.EXPAND)
        return sizer

    def _insert_markup(self, markup):
        ctrl = self._ctrl
        start, end = ctrl.GetSelection()
        selection = ctrl.GetRange(start, end)
        prior = next = None
        if start > 0:
            prior = ctrl.GetRange(start - 1, start)
        if end < ctrl.GetLastPosition():
            next = ctrl.GetRange(end, end + 1)
        if prior not in (None, ' ', '\t', '\n', '\r', '/', '*', '_'):
            ctrl.WriteText(' ')
        if start == end:
            ctrl.WriteText(markup + markup)
            step_back = 1
        else:
            ctrl.WriteText(markup + selection + markup)
            step_back = None
        if next not in (None, ' ', '\t', '\n', '\r', '/', '*', '_'):
            ctrl.WriteText(' ')
            if step_back:
                step_back += 1
        if step_back:
            ctrl.SetInsertionPoint(ctrl.GetInsertionPoint() - step_back)

    def _storage_op(self, method_name, *args, **kwargs):
        method = getattr(self._storage, method_name)
        try:
            return method(*args, **kwargs)
        except AttachmentStorage.InvalidImageFormat as e:
            app.echo(_("Invalid image format!"), kind='error')
        except AttachmentStorage.StorageError as e:
            app.error(title=_("Error accessing attachment storrage"),
                      message=_("Error accessing attachment storrage") + ":\n" + e)

    @Command.define
    def search(self):
        pass

    @Command.define
    def search_and_replace(self):
        pass

    @Command.define
    def undo(self):
        self._ctrl.Undo()

    def _can_undo(self):
        return self._ctrl.CanUndo()

    @Command.define
    def redo(self):
        self._ctrl.Redo()

    def _can_redo(self):
        return self._ctrl.CanRedo()

    @Command.define
    def preview(self):
        text = self._get_value()
        if self._storage:
            resources = self._storage_op('resources', transaction=self._row.transaction()) or ()
        else:
            resources = ()
        InfoWindow(_(u"Preview"), text=text, format=TextFormat.LCG, resources=resources)

    @Command.define
    def export_pdf(self):
        Command(RecordForm.view_field_pdf, field_id=self._id).invoke()

    @Command.define
    def strong(self):
        self._insert_markup('*')
        self.set_focus()

    @Command.define
    def emphasized(self):
        self._insert_markup('/')
        self.set_focus()

    @Command.define
    def underlined(self):
        self._insert_markup('_')
        self.set_focus()

    @Command.define
    def itemize(self, style='bullet'):
        if style == 'bullet':
            markup = '*'
        elif style == 'numbered':
            markup = '#'
        else:
            raise ProgramError("Invalid list style: %r" % style)
        ctrl = self._ctrl
        selection = ctrl.GetRange(*ctrl.GetSelection())
        if selection:
            if '\n' in selection:
                selection = textwrap.fill(selection, 80, subsequent_indent='  ')
            new_text = markup + ' ' + selection.strip() + '\n'
        else:
            new_text = markup + ' '
        column_number = ctrl.PositionToXY(ctrl.GetInsertionPoint())[1]
        if column_number != 0:
            new_text = '\n' + new_text
        ctrl.WriteText(new_text)
        self.set_focus()

    @Command.define
    def verbatim(self):
        ctrl = self._ctrl
        start, end = ctrl.GetSelection()
        selection = ctrl.GetRange(start, end)
        if selection:
            verbatim_text = selection
            if not verbatim_text.endswith('\n'):
                verbatim_text += '\n'
            position = start
        else:
            verbatim_text = '\n'
            position = ctrl.GetInsertionPoint()
        new_text = '-----\n' + verbatim_text + '-----\n'
        column_number = ctrl.PositionToXY(position)[1]
        if column_number != 0:
            new_text = '\n' + new_text
        ctrl.WriteText(new_text)
        if not selection:
            ctrl.SetInsertionPoint(position + 6)
        self.set_focus()

    def _load_new_file(self, row):
        fh = app.open_selected_file(mode='rb', context='attachments')
        if fh:
            with fh:
                filename = app.splitpath(fh.name)[1]
                if ' ' in filename or any(ord(c) > 127 for c in filename):
                    app.error(
                        title=_("Invalid file name"),
                        message="{}: {}".format(filename, _("Invalid characters in file name.")),
                    )
                    return
                if 'size' in row:
                    size = self.ImageSizes.thumbnail_size_bounds(row['size'].value(), None)
                    values = dict(has_thumbnail=(size is not None), thumbnail_size=size)
                else:
                    values = dict()
                self._storage_op('insert', filename, fh, values,
                                 transaction=self._row.transaction())
            row.form.field.filename.refresh()
            row['filename'] = pytis.data.Value(row.type('filename'), filename)

    def _check_file_selected(self, row, filename):
        if not filename:
            return ('filename', _("File not selected!"))

    def _image_preview_computer(self, row, filename):
        if filename:
            f = self._storage_op('retrieve', filename, transaction=self._row.transaction())
            if f:
                try:
                    return pytis.data.Image.Data(f, filename=filename)
                except ValueError as e:
                    app.error(title=_("Invalid value"),
                              message="{}: {}".format(filename, e))
                finally:
                    f.close()
        return None

    def _retrieve_attachment(self, filename):
        if filename:
            return self._storage_op('resource', filename, transaction=self._row.transaction())
        else:
            return None

    def _size_computer(self, row, filename):
        thumbnail = None
        resource = self._retrieve_attachment(filename)
        if resource:
            thumbnail = resource.thumbnail()
        return self.ImageSizes.matching_size(thumbnail)

    def _preview_size_computer(self, row, filename, size):
        resource = self._retrieve_attachment(filename)
        if resource:
            orig_size = resource.size()
            if orig_size:
                return "%dx%d px" % self.ImageSizes.preview_size(size, None, orig_size)
        return None

    def _orig_size_computer(self, row, filename, size):
        resource = self._retrieve_attachment(filename)
        if resource:
            orig_size = resource.size()
            if orig_size:
                return "%dx%d px" % tuple(orig_size)
        return None

    def _resize_computer(self, row, filename):
        thumbnail = None
        resource = self._retrieve_attachment(filename)
        if resource and resource.size():
            thumbnail = resource.thumbnail()
            if thumbnail and thumbnail.size():
                resize = thumbnail.size()[0] // resource.size()[0] * 100
                return str(resize) + '%'
            return thumbnail
        return None

    @Command.define
    def image(self):
        if not self._storage:
            return
        link = self.LCGLink(self._ctrl)
        enumerator = self.AttachmentEnumerator(self._storage, images=True)
        transaction = self._row.transaction()
        if link.target() in enumerator.values(transaction=transaction):
            filename = link.target()
        else:

            # TODO: Warn the user?
            filename = None
        fields = (
            Field('filename', _("Available files"),
                  compact=True, width=40, height=7, enumerator=enumerator,
                  selection_type=SelectionType.LISTBOX),
            Field('preview', _("Preview"), compact=True, width=200, height=200,
                  computer=computer(self._image_preview_computer),
                  editable=Editable.NEVER,
                  type=pytis.data.Image(not_null=True, maxlen=5 * 1024 * 1024),
                  descr=_("Choose one of available files "
                          "or insert a new file from your computer.")),
            Field('size', _("Display in page as"), enumerator=self.ImageSizes,
                  selection_type=SelectionType.RADIO,
                  editable=Editable.ALWAYS, not_null=True,
                  computer=computer(self._size_computer),
                  descr=_("Choose the size and behavior of the image within the page.")),
            Field('orig_size', _("Original size"), editable=Editable.NEVER,
                  computer=computer(self._orig_size_computer)),
            Field('preview_size', _("Preview size"),
                  editable=Editable.NEVER,
                  computer=computer(self._preview_size_computer)),
            Field('resize', _("Resize ratio"),
                  editable=Editable.NEVER,
                  computer=computer(self._resize_computer)),
            Field('align', _("Alignment"), not_null=True,
                  enumerator=self.ImageAlignments),
            # Field('title', _(u"Název"), width=50,
            #       descr=_(u"Zadejte název zobrazený v textu dokumentu.  Ponechte\n"
            #               u"prázdné, pokud chcete zobrazit přímo URL zadané v \n"
            #               u"předchozím políčku.")),
            Field('tooltip', _("Tooltip"), width=50,
                  descr=_("Enter the text displayed in baloon help above "
                          "the image when the mouse moves over.")),
        )
        button = Button(_("Insert new"), self._load_new_file)
        row = app.input_form(title=_("Insert Image"), fields=fields,
                             prefill=dict(filename=filename,
                                          align=link.align(),
                                          tooltip=link.tooltip()),
                             layout=(HGroup(('filename', button), ('preview',)),
                                     'align', 'size', 'orig_size', 'preview_size', 'tooltip'),
                             check=(self._check_file_selected,),
                             transaction=transaction)
        if row:
            filename = row['filename'].value()
            if row['size'].value() != self._size_computer(row, filename):
                size = self.ImageSizes.thumbnail_size_bounds(row['size'].value(), None)
                self._storage_op('update', filename, dict(has_thumbnail=(size is not None),
                                                          thumbnail_size=size),
                                 transaction=transaction)
            link.update(target=filename,
                        title=None,
                        tooltip=row['tooltip'].value(),
                        align=row['align'].value())
        self.set_focus()

    @Command.define
    def attachment(self):
        if not self._storage:
            return
        link = self.LCGLink(self._ctrl)
        enumerator = self.AttachmentEnumerator(self._storage, images=False)
        if link.target() in enumerator.values(transaction=self._row.transaction()):
            filename = link.target()
        else:
            # TODO: Warn the user?
            filename = None
        fields = (
            Field('filename', _("Available files"),
                  compact=True, width=40, height=7, enumerator=enumerator,
                  selection_type=SelectionType.LISTBOX),
            Field('title', _(u"Title"), width=50,
                  descr=_("Enter the link label displayed within document text. "
                          "Leave empty if you want to dispaly the file name directly.")),
            Field('tooltip', _(u"Tooltip"), width=50,
                  descr=_("Enter the text displayed in baloon help above the "
                          "link when the mouse moves over.")),
        )
        button = Button(_("Insert new"), self._load_new_file)
        row = app.input_form(title=_("Insert attachment"), fields=fields,
                             prefill=dict(filename=filename,
                                          title=link.title(),
                                          tooltip=link.tooltip()),
                             layout=('filename', button, 'title', 'tooltip'),
                             check=(self._check_file_selected,))
        if row:
            link.update(target=row['filename'].value(),
                        title=row['title'].value(),
                        tooltip=row['tooltip'].value())
        self.set_focus()

    @Command.define
    def link(self):
        link = self.LCGLink(self._ctrl)
        prefill=dict(
            target=link.target(),
            title=link.title(),
            tooltip=link.tooltip(),
        )
        row = app.input_form(title=_("Link properties"), prefill=prefill, fields=(
            Field('target', _("Target"), width=50, not_null=True,
                  descr=_("Enter the absolute URL, such as "
                          "http://www.mycompany.com or "
                          "a local link, such as an identifier of another page in the CMS.")),
            Field('title', _("Title"), width=50,
                  descr=_("Enter the link label displayed within the document text. "
                          "Leave empty if you want to dispaly the target URL directly.")),
            Field('tooltip', _(u"Tooltip"), width=50,
                  descr=_("Enter the text displayed in baloon help above the link "
                          "when the mouse moves over.")),
        ))
        if row:
            link.update(target=row['target'].value(),
                        title=row['title'].value(),
                        tooltip=row['tooltip'].value())
        self.set_focus()

    @Command.define
    def linebreak(self):
        self._ctrl.WriteText('//\n')
        self.set_focus()

    @Command.define
    def heading(self, level):
        ctrl = self._ctrl
        position = ctrl.GetInsertionPoint()
        success, column_number, line_number = ctrl.PositionToXY(position)
        line_text = ctrl.GetLineText(line_number)
        match = self._HEADING_MATCHER.match(line_text)
        line_beginning = position - column_number
        if match:
            title = match.group('title')
            anchor = match.group('anchor')
            if anchor:
                anchor = ' ' + anchor
            else:
                anchor = ''
        else:
            title = line_text.strip()
            anchor = ''
        if level > 0:
            new_text = '=' * level + ' ' + title + ' ' + '=' * level + anchor
        else:
            new_text = title
        ctrl.SetSelection(line_beginning, line_beginning + len(line_text))
        ctrl.WriteText(new_text)
        self.set_focus()

    @Command.define
    def open_in_editor(self):
        result = run_form(pytis.form.ResizableInputForm, name='x', title=self._spec.label(),
                          fields=(self._spec,),
                          prefill={self._id: self._ctrl.GetValue()})
        if result is not None:
            self._ctrl.SetValue(result[self._id].value())

    def current_heading_level(self):
        ctrl = self._ctrl
        position = ctrl.GetInsertionPoint()
        line_number = ctrl.PositionToXY(position)[2]
        line_text = ctrl.GetLineText(line_number)
        match = self._HEADING_MATCHER.match(line_text)
        if match:
            return len(match.group('level'))
        else:
            return 0


class RangeField(InputField):

    def _create_widget(self, parent, ctrl):
        w1 = super(RangeField, self)._create_widget(parent, ctrl)
        ctrl2 = self._create_ctrl(parent)
        self._controls.append((ctrl2, self._set_ctrl_editable))
        self._init_ctrl(ctrl2)
        w2 = super(RangeField, self)._create_widget(parent, ctrl2)
        self._inputs = (ctrl, ctrl2)
        return self._hbox(w1, w2)

    def _set_value(self, value):
        for val, ctrl in zip(value, self._inputs):
            ctrl.SetValue(val)
        self._on_change()

    def _validate(self):
        value = tuple([ctrl.GetValue() for ctrl in self._inputs])
        return self._row.validate(self.id(), value)

    def tab_navigated_widgets(self):
        return self._inputs


class NumericRangeField(RangeField, NumericField):
    pass


class DateRangeField(RangeField, DateField):

    def _date_type(self):
        return self._type.base_type()


class DateTimeRangeField(RangeField, DateTimeField):

    def _date_type(self):
        return self._type.base_type()
