# -*- coding: utf-8 -*-

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

"""Input field abstraction.

The input field is basically the user interface control (widget or a set of widgets) which is
directly bound to the PresentedRow instance representing the edited record.  The changes are
automatically propagated between the PresentedRow instance and the user interface.

The actual class representing each field is determined by its specification and data type.

"""

import collections
import pytis.data
from pytis.form import *
import wx.lib.colourselect
from cStringIO import StringIO
import datetime
#from wxPython.pytis.maskededit import wxMaskedTextCtrl


class _TextValidator(wx.PyValidator):
    def __init__(self, control, filter):
        wx.PyValidator.__init__(self)
        self._control = control
        self._filter = filter
        wx_callback(wx.EVT_CHAR, self, self._on_char)

    def Clone(self): 
        return _TextValidator(self._control, self._filter)
    
    def _on_char(self, event):
        key = event.GetKeyCode()
        if self._filter is not None \
               and key >= wx.WXK_SPACE and key != wx.WXK_DELETE and key <= 255 \
               and not self._filter(chr(key)):
            message(_(u"Nepovolený znak!"), beep_=True)
            return True
        else: 
            event.Skip()
            return True

class _Completer(wx.PopupWindow):
    """Autocompletion selection control."""
    
    def __init__(self, ctrl):
        """Initialize the selectrol for given `wx.TextCtrl' instance."""
        super(_Completer, self).__init__(ctrl.GetParent())
        self._ctrl = ctrl
        self._last_insertion_point = 0
        style = wx.LC_REPORT | wx.LC_SINGLE_SEL | wx.LC_NO_HEADER| wx.SIMPLE_BORDER
        self._list = listctrl = wx.ListCtrl(self, pos=wx.Point(0, 0), style=style)
        self.update(ctrl.GetValue(), False)
        wx_callback(wx.EVT_KILL_FOCUS, ctrl, self._on_close)
        wx_callback(wx.EVT_LEFT_DOWN,  ctrl, self._on_toggle_down)
        wx_callback(wx.EVT_LEFT_UP,    ctrl, self._on_toggle_up)
        wx_callback(wx.EVT_LISTBOX,     listctrl, listctrl.GetId(), self._on_list_item_selected)
        wx_callback(wx.EVT_LEFT_DOWN,   listctrl, self._on_list_click)
        wx_callback(wx.EVT_LEFT_DCLICK, listctrl, self._on_list_dclick)

    def _on_close(self, event):
        self._show(False)
        event.Skip()

    def _on_toggle_down(self, event):
        self._last_insertion_point = self._ctrl.GetInsertionPoint()
        event.Skip()

    def _on_toggle_up(self, event):
        if self._ctrl.GetInsertionPoint() == self._last_insertion_point:
            self._show(not self.IsShown())
        event.Skip()

    def _on_list_item_selected(self, event):
        self._set_value_from_selected()
        event.Skip()

    def _on_list_click(self, event):
        sel, flag = self._list.HitTest(event.GetPosition())
        if sel != -1:
            self._list.Select(sel)
            self._set_value_from_selected()

    def _on_list_dclick(self, event):
        self._set_value_from_selected()

    def _set_value_from_selected(self):
         sel = self._list.GetFirstSelected()
         if sel > -1:
            text = self._list.GetItem(sel, 0).GetText()
            self._ctrl.SetValue(text)
            self._ctrl.SetInsertionPointEnd()
            self._ctrl.SetSelection(-1, -1)
            self._show(False)

    def _show(self, show=True):
        if show and self._list.GetItemCount() > 0:
            size = self.GetSize()
            height = self._ctrl.GetSizeTuple()[1]
            x, y = self._ctrl.ClientToScreenXY(0, height)
            if (y + size.GetHeight()) >= wx.SystemSettings.GetMetric(wx.SYS_SCREEN_Y):
                y = y - height - size.GetHeight()
            self.SetPosition(wx.Point(x, y))
            self.Show(True)
        else:
            self.Show(False)

    def on_key_down(self, event):
        """Handle TextCtrl keypresses if they belong to the completer.

        Returns True if the event was processed and False when it should be passed on.
        
        """
        listctrl = self._list
        if listctrl.GetItemCount() == 0:
            return False
        code = event.GetKeyCode()
        if code in (wx.WXK_DOWN, wx.WXK_UP):
            sel = listctrl.GetFirstSelected()
            if code == wx.WXK_DOWN and sel < (listctrl.GetItemCount() - 1):
                listctrl.Select(sel+1)
            elif code == wx.WXK_UP and sel > 0 :
                listctrl.Select(sel-1)
            listctrl.EnsureVisible(sel)
            self._show()
            return True
        if self.IsShown():
            if code == wx.WXK_RETURN :
                self._set_value_from_selected()
                return True
            if code == wx.WXK_ESCAPE :
                self._show(False)
                return True
        return False
    
    def update(self, completions, show):
        """Update the list of available completions."""
        self._show(False)
        listctrl = self._list
        listctrl.ClearAll()
        listctrl.InsertColumn(0, "")
        height_limit = None
        listctrl.SetSize((17,17)) # Needed for GetViewRect to work consistently. 
        for i, choice in enumerate(completions):
            listctrl.InsertStringItem(i, "")
            listctrl.SetStringItem(i, 0, choice)
            if i == 10:
                height_limit = listctrl.GetViewRect()[3] + 5
        listctrl.SetColumnWidth(0, wx.LIST_AUTOSIZE)
        width, height = listctrl.GetViewRect()[2:]
        if height_limit:
            height = height_limit
        else:
            # Vertical scrollbars are not displayed in this case.
            width -= wx.SystemSettings.GetMetric(wx.SYS_VSCROLL_X) + 1
            # Actually, horizontal scrollbars are not needed too, but if we reduce the height,
            # they show up.  Grin...
        listctrl.SetSize((width, height))
        self.SetClientSize((width, height))
        if completions and show:
            self._show(True)
            listctrl.Select(0)
            listctrl.EnsureVisible(0)


class InputField(object, KeyHandler, CallbackHandler, CommandHandler):
    """Abstraktní třída vstupního pole.

    Vstupní políčko není samo o sobě wx prvkem. Odpovídající prvky
    uživatelského rozhraní lze získat metodami 'label()' a 'widget()'.
    Políčko je rozděleno na části widget a label, aby mohly být tyto dvě části
    umístěny do gridu...

    Tato třída není sama o sobě instanciovatelná! Odvozením další
    třídy a předefinováním dále popsaných metod však lze vytvořit políčka
    s libvolným chováním realizovaná libovolným UI prvkem.

    Třída je 'CallbackHandler'. Argument callbackové funkce závisí na typu
    callbacku a je zdokumentován v dokumentaci callbackové konstanty.
    
    """

    _DEFAULT_WIDTH = 13
    _DEFAULT_HEIGHT = 1
    _DEFAULT_BACKGROUND_COLOR = None

    CALL_FIELD_CHANGE = 'CALL_FIELD_CHANGE'
    """Callback volaný při každé změně hodnoty. Argumentem je instance políčka.

    Callback je volán pouze při interaktivní (uživatelem vyvolané) změně
    hodnoty a při inicializaci hodnoty políčka.  Ostatní programové nastavování
    hodnoty callback nevyvolává.
    
    """

    _focused_field = None
    _last_focused_field = None
    
    def _get_command_handler_instance(cls):
        return InputField.focused()
    _get_command_handler_instance = classmethod(_get_command_handler_instance)
    
    def create(cls, parent, row, id, inline=False, **kwargs):
        """Create an instance of the class corresponding to the field specification.

        The arguments are the same as for the 'InputField' constructor.
        
        """
        #assert isinstance(parent, wx.Window)
        assert isinstance(row, PresentedRow)
        assert isinstance(id, basestring)
        assert isinstance(inline, bool)
        spec = find(id, row.fields(), key=lambda f: f.id())
        type = row.type(id)
        if type.enumerator() is not None:
            codebook = row.codebook(id)
            selection_type = spec.selection_type()
            if isinstance(type, pytis.data.Boolean) and selection_type is None:
                field = CheckBoxField
            elif inline:
                if codebook:
                    field = CodebookField
                else:
                    field = ChoiceField 
            else:
                if selection_type is None:
                    if codebook is not None:
                        selection_type = SelectionType.CODEBOOK
                    else:
                        selection_type = SelectionType.CHOICE
                mapping = {
                    SelectionType.CODEBOOK:  CodebookField,
                    SelectionType.LIST:      ListField,
                    SelectionType.CHOICE:    ChoiceField,
                    SelectionType.LIST_BOX:  ListBoxField,
                    SelectionType.RADIO: RadioBoxField,
                    }
                field = mapping[selection_type]
        elif isinstance(type, pytis.data.Image):
            field = ImageField
        elif isinstance(type, pytis.data.Binary):
            field = FileField
        elif isinstance(type, pytis.data.Date):
            field = DateField
        elif isinstance(type, pytis.data.Time):
            field = TimeField
        elif isinstance(type, pytis.data.Color):
            field = ColorSelectionField
        elif isinstance(type, pytis.data.Password):
            field = PasswordField
        elif isinstance(type, pytis.data.String):
            field = StringField
        elif isinstance(type, pytis.data.Number):
            field = NumericField
        else:
            field = TextField
        return field(parent, row, id, inline=inline, **kwargs)
    create = classmethod(create)

    def _defocus(cls, field):
        if cls._focused_field is field:
            cls._last_focused_field = cls._focused_field
            cls._focused_field = None
    _defocus = classmethod(_defocus)

    def _focus(cls, field):
        #import weakref
        current = cls.focused()
        cls._focused_field = field #weakref.ref(field)
        if current is not None:
            cls._last_focused_field = current
    _focus   = classmethod(_focus)
    
    def _last_focused(cls):
        field = cls._last_focused_field
        cls._last_focused_field = None
        if field is not None and field._alive():
            return field
        return None
    _last_focused = classmethod(_last_focused)

    def focused(cls):
        field = cls._focused_field
        if field is not None and field._alive():
            return field
        return None
    focused = classmethod(focused)

    # Instance methods
    
    def __init__(self, parent, row, id, inline=False, guardian=None, readonly=False):
        """Initialize the input field according to its specification.
        
        Arguments:

          parent -- wx parent for the created widgets.
            
          row -- 'PresentedRow' instance.

          id -- field specification as a 'Field' instance.
            
          inline -- if true, only the basic input widget is created.  The label and all surrounding
            widgets are omitted, so that the widget can be used in the inline editation mode in of
            a table cell.
          
          guardian -- parent 'KeyHandler'.
          
          readonly -- 

        This method should not be overriden by derived classes.  All field specific initialization
        should be done in the methods '_create_widget()' and '_create_label'.

        """
        assert isinstance(row, PresentedRow)
        assert isinstance(id, basestring)
        assert isinstance(guardian, KeyHandler)
        assert isinstance(inline, bool)
        CallbackHandler.__init__(self)
        spec = find(id, row.fields(), key=lambda f: f.id())
        self._parent = parent
        self._row = row
        self._type = row.type(id)
        self._spec = spec
        self._guardian = guardian
        self._id = id = spec.id()
        self._inline = inline
        self._want_focus = False
        if row.new():
            permission = pytis.data.Permission.INSERT
        else:
            permission = pytis.data.Permission.UPDATE
        self._denied = denied = not row.permitted(id, permission)
        self._hidden = not row.permitted(id, pytis.data.Permission.VIEW)
        self._readonly = readonly or denied or row.hidden_codebook(id)
        self._enabled = not readonly and row.editable(id)
        self._callback_registered = False
        self._unregistered_widgets = {}
        self._needs_validation = False
        self._valid = False
        self._init_attributes()
        self._ctrl = ctrl = self._create_ctrl()
        if inline:
            self._widget = ctrl
        else:
            self._label = self._create_label()
            self._widget = self._create_widget()
        KeyHandler.__init__(self, ctrl)
        wx_callback(wx.EVT_IDLE,       ctrl, self._on_idle)
        wx_callback(wx.EVT_KILL_FOCUS, ctrl, self._on_kill_focus)
        wx_callback(wx.EVT_SET_FOCUS,  ctrl, self._on_set_focus)
        wx_callback(wx.EVT_RIGHT_DOWN, ctrl, self._on_context_menu)
        if self._spec.descr() is not None:
            ctrl.SetToolTipString(self._spec.descr())
        if not self._enabled:
            self._disable()
            self._register_skip_navigation_callback()
        if not inline:
            row.register_callback(row.CALL_CHANGE, id, self._change_callback)
            row.register_callback(row.CALL_EDITABILITY_CHANGE, id, self._editability_change_callback)
        value = self._row.invalid_string(id)
        if value is None:
            value = row.format(id, secure='')
        self._set_value(value)
        self._call_on_idle = self._update_background_color
        
    def _init_attributes(self):
        pass
        
    def __str__(self):
        try:
            return "<%s id='%s'>" % (self.__class__.__name__, self.id())
        except AttributeError:
            return "<%s (uninitialized)>" % self.__class__.__name__
        
    def _skip_navigation_callback(self, widget):
        def cb(e):
            if widget not in self._unregistered_widgets:
                e.Skip()
                flag = e.GetDirection() and wx.NavigationKeyEvent.IsForward or 0
                wx.CallAfter(lambda : widget.Navigate(flag))
            else:
                e.Skip()
        return cb
            
    def _create_label(self):
        # Return field label as 'wx.StaticText' instance.
        label = self.spec().label()
        if label:
            label = label + ':'            
        return wx.StaticText(self._parent, -1, label, style=wx.ALIGN_RIGHT)

    def _create_ctrl(self):
        # Return the actual control element for this field.
        raise ProgramError("This method must be overriden!")

    def _create_widget(self):
        # Return the complete widget containing all control elements.
        # For simple fields that's the actual control, but some more
        # sophisticated classes may add additional buttons etc.
        return self._ctrl

    def _get_value(self):
        # Return the external (string) representation of the current field value from the field UI
        # control.  This value must be validatable by the field data type.
        raise ProgramError("This method must be overriden!")

    def _set_value(self, value):
        # Set the field control according to given external (string) value.
        raise ProgramError("This method must be overriden!")
    
    # Other private methods.

    def _menu(self):
        # Return a tuple of popup menu items ('MItem' instances).
        return (UICommand(InputField.COMMAND_RESET(),
                          _(u"Vrátit původní hodnotu"),
                          _(u"Vrátit veškeré provedené změny.")),)
                        
    def _on_context_menu(self, event=None):
        def handler(uicmd):
            if issubclass(uicmd.command().handler(), (InputField, Invocable)):
                return self
            else:
                return None
        self._set_focus()
        menu = [uicmd and mitem(uicmd.clone(_command_handler=handler(uicmd))) or MSeparator()
                for uicmd in self._menu()]
        if event:
            position = None
        else:
            size = self._ctrl.GetSize()
            position = (size.x/3, size.y/2)
        popup_menu(self._ctrl, menu, position=position, keymap=global_keymap())

    def _validate(self):
        return self._row.validate(self.id(), self._get_value())
        
    def _on_idle(self, event):
        if self._needs_validation:
            transaction = self._row.transaction()
            # Don't validate when the transaction is already closed.
            if transaction is None or transaction.open():
                self._needs_validation = False
                valid = self._validate() is None
                if valid != self._valid:
                    self._valid = valid
                    self._on_validity_change()
                self._on_change_hook()
        if self._want_focus and not self._has_focus():
            self._set_focus()
        if hasattr(self, '_call_on_idle') and self._call_on_idle is not None:
            self._call_on_idle()
            self._call_on_idle = None
        event.Skip()
        return True

    def _on_change_hook(self):
        """Handle field value changes.
        
        Overriding this method allows any additional actions after each change of the field value.
        
        """
        pass
        
    def _on_validity_change(self):
        """Handle field validity changes.
        
        Overriding this method allows any additional actions after field validity changes, such as
        highlighting this state in the UI.
        
        """
        self._update_background_color()
        
    def _on_set_focus(self, event):
        self._want_focus = False
        last = InputField._last_focused()
        # TODO: Zkusit to přes `wx.Window.SetFocusFromKbd()'
        if last is not None and last is not self and last.enabled() and last._modified():
            if not last.validate(interactive=False):
                last.set_focus()
                return True
        InputField._focus(self)
        event.Skip()
        return True

    def _on_kill_focus(self, event):
        InputField._defocus(self)
        event.Skip()
        return True

    def _enable_event_handlers(self):
        self._ctrl.SetEvtHandlerEnabled(True)

    def _disable_event_handlers(self):
        self._ctrl.SetEvtHandlerEnabled(False)

    def _register_skip_navigation_callback(self):
        control = self._ctrl
        if not self._callback_registered:
            wx_callback(wx.EVT_NAVIGATION_KEY, control, self._skip_navigation_callback(control))
            self._callback_registered = True
        if control in self._unregistered_widgets:
            del(self._unregistered_widgets[control])

    def _unregister_skip_navigation_callback(self):
        #self._ctrl.Disconnect(-1, -1, wx.wxEVT_NAVIGATION_KEY)
        # The disconnect above doeasn't work, so here is a nasty workaround.
        self._unregistered_widgets[self._ctrl] = True

    def _change_callback(self):
        # Field value change signalization from PresentedRow.
        value = self._row.format(self.id())
        if self._get_value() != value:
            self._set_value(value)

    def _editability_change_callback(self):
        # Field editability change signalization from PresentedRow.
        if not self._denied and not self._readonly:
            if self._row.editable(self.id()):
                self._enabled = True
                self._enable()
                self._unregister_skip_navigation_callback()
            else:
                self._enabled = False
                self._disable()
                self._register_skip_navigation_callback()

    def _on_change(self, event=None):
        # Called on user interaction (editation, selection).  The actual processing of the event
        # is postponed to the idle thread to avoid user interface hangs on time-consuming
        # operations (such as complicated field recomputations).
        if not self._readonly:
            self._needs_validation = True
            self._update_background_color()
        if event:
            event.Skip()

    def _enable(self):
        self._ctrl.Enable(True)

    def _disable(self):
        #if not self._readonly:
        # There is currently no way to disable a wx control without graying it out, which we don't
        # want in show forms, so we leave it editable relying on the fact, that there is no way to
        # save the changed in a readonly form.  This situation is fixed for TextFields below.
        self._ctrl.Enable(False)

    def _update_background_color(self):
        if self._denied:
            color = config.field_denied_color
        elif self._readonly:
            color = config.field_disabled_color
        elif not self._enabled:
            color = config.field_disabled_color
        elif self._hidden and not self._modified():
            color = config.field_hidden_color
        elif not self._valid:
            color = config.field_invalid_color
        else:
            color = self._DEFAULT_BACKGROUND_COLOR
        self._set_background_color(color)

    def _set_background_color(self, color):
        self._ctrl.SetOwnBackgroundColour(color)
        self._ctrl.Refresh()

    def _modified(self):
        # Returns always false for virtual fields
        return self._row.field_changed(self.id())

    def _px_size(self, width, height):
        size = dlg2px(self._parent, 4*(width+1)+2, 8*height+4.5)
        return (size.width, size.height)
    
    def _set_focus(self):
        parent = self._ctrl.GetParent()
        nb = parent.GetParent()
        if isinstance(nb, wx.Notebook) and nb.GetCurrentPage() != parent:
            for i in range(nb.GetPageCount()):
                if nb.GetPage(i) == parent:
                    nb.SetSelection(i)
        self._ctrl.SetFocus()

    def _has_focus(self):
        """Return true if the field currently has keyboard focus."""
        return InputField.focused() is self

    def _alive(self):
        try:
            self._ctrl.GetId()
            return True
        except wx.PyDeadObjectError:
            return False

    # Command processing
    
    def _can_reset(self):
        return self._modified() and self._enabled

    def _cmd_reset(self):
        self.reset()

    def _cmd_context_menu(self):
        self._on_context_menu()

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
            if interactive:
                log(EVENT, 'Invalid field:', self.id())
                run_dialog(Error, title=_(u"Chyba validace"),
                           message=_('Chyba validace políčka!\n\n%s: %s') % \
                           (self.spec().label(), error.message()))
            else:
                message(error.message(), beep_=True)
        return error is None

    def set_focus(self, reset=False):
        """Make the field active for user input."""
        InputField._last_focused() # Focus set programatically - forget the last focused field.
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
    
    def reset(self):
        """Reset the field to its original value."""
        id = self.id()
        value = self._row.original_row()[id]
        if self._row.permitted(id, pytis.data.Permission.VIEW):
            exported_value = value.export()
        else:
            exported_value = value.type().secret_export()
        self._set_value(exported_value)

    def insert_text(self, text):
        """Insert given text into the field in the current place of the cursor."""
        self._ctrl.WriteText(text)
        
        
class Unlabeled:
    """Mix-in třída pro políčka .

    Některé prvky mají label spojen přímo s controlem, takže label zobrazený
    v gridu musí být prázdný.

    """
    def _create_label(self):
        # Return an empty label as 'wx.StaticText' instance.
        return wx.StaticText(self._parent, -1, '')


class TextField(InputField):
    """Textové vstupní políčko."""
    
    NUMBERS = map(str, range(10))
    SIGNS = ['-', '+']
    DECIMAL_POINTS = ['.', ',']
    FLOAT = map(str, range(10)) + SIGNS + DECIMAL_POINTS
    ASCII   = map(chr, range(127))
    LETTERS = map(chr, range(ord('a'),ord('z')+1) + range(ord('A'),ord('Z')+1))

    def _create_ctrl(self):
        if not self._inline:
            size = self._px_size(self.width(), self.height())
        else:
            size = None
        control = wx.TextCtrl(self._parent, -1, '', style=self._ctrl_style(), size=size)
        wxid = control.GetId()
        maxlen = self._maxlen()
        if maxlen is not None:
            control.SetMaxLength(maxlen)
            wx_callback(wx.EVT_TEXT_MAXLEN, control, wxid,
                        lambda e: message(_(u"Překročena maximální délka."), beep_=True))
        filter = self._filter()
        control.SetValidator(_TextValidator(control, filter=filter))
        wx_callback(wx.EVT_TEXT, control, wxid, self._on_change)
        wx_callback(wx.EVT_TEXT_ENTER, control, wxid, self._on_enter_key)
        if not self._denied and not self._readonly and self._row.has_completer(self.id()):
            self._completer = _Completer(control)
        else:
            self._completer = None
        self._update_completions = None
        return control

    def on_key_down(self, event):
        if self._completer and self._completer.on_key_down(event):
            return
        super(TextField, self).on_key_down(event)

    def _ctrl_style(self):
        style = wx.TE_PROCESS_ENTER
        if self.height() > 1:
            style |= wx.TE_MULTILINE
        return style
    
    def _maxlen(self):
        """Vrať maximální délku zadaného textu."""
        return None

    def _on_enter_key(self, event):
        if self.height() > 1:
            event.Skip()
        else:
            event.GetEventObject().Navigate()

    def _on_idle(self, event):
        text = self._update_completions
        if text is not None:
            self._update_completions = None
            # If the field has no focus, the change is *most likely* not originated by the user, so
            # we we don't popup the selection (the second argument to update()).
            self._completer.update(self._row.completions(self.id(), prefix=text),
                                   self._enabled and self._has_focus())
        return super(TextField, self)._on_idle(event)
        
    def _on_change(self, event=None):
        post_process = self._post_process_func()
        if post_process:
            value = post_process(self._get_value())
            if value != self._get_value():
                self._set_value(value)
        if event and self._completer:
            self._update_completions = event.GetString()
        super(TextField, self)._on_change(event=event)

    def _post_process_func(self):
        """Vrať funkci odpovídající specifikaci postprocessingu políčka.

        Vrací: Funkci jednoho argumentu (původní text), která vrací
        řetězec (změněný text).
        
        """
        try:
            return self._stored_post_process_func
        except:            
            pp_spec = self.spec().post_process()
            if isinstance(pp_spec, collections.Callable):
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
            return lambda char, list=self.spec().filter_list(): \
                                      char not in list
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
        self._on_change() # call manually, since SetValue() doesn't emit an event.

    def _enable(self):
        control = self._ctrl
        control.SetEditable(True)
        control.SetValidator(_TextValidator(control, filter=self._filter()))
        self._update_background_color()

    def _disable(self):
        self._ctrl.SetEditable(False)
        self._ctrl.SetValidator(wx.DefaultValidator)
        # The change won't take effect for certain fields if we do it directly!
        self._call_on_idle = self._update_background_color

    def _menu(self):
        return super(TextField, self)._menu() + \
               (None,
                UICommand(TextField.COMMAND_CUT(), _(u"Vyjmout"),
                          _(u"Vyjmout označený text a uložit jej do schránky.")),
                UICommand(TextField.COMMAND_COPY(), _(u"Kopírovat"),
                          _(u"Zkopírovat označený text do schránky.")),
                UICommand(TextField.COMMAND_PASTE(), _(u"Vložit"),
                          _(u"Vložit text ze schránky do políčka.")),
                UICommand(TextField.COMMAND_SELECT_ALL(), _(u"Vybrat vše"),
                          _(u"Označit celou hodnotu.")))

    # Zpracování příkazů
    
    def _can_cut(self):
        return self._ctrl.CanCut()
        
    def _cmd_cut(self):
        self._ctrl.Cut()
        
    def _can_copy(self):
        return self._ctrl.CanCopy()

    def _cmd_copy(self):
        self._ctrl.Copy()
        
    def _can_paste(self):
        return self._ctrl.CanPaste()
        
    def _cmd_paste(self):
        self._ctrl.Paste()
        
    def _can_select_all(self):
        return bool(self._ctrl.GetValue())

    def _cmd_select_all(self):
        self._ctrl.SetSelection(-1, -1)
        

class StringField(TextField):
    """Textové vstupní políčko pro data typu 'pytis.data.String'."""

    def _maxlen(self):
        return self._type.maxlen()

class PasswordField(StringField):
    _ORIGINAL_VALUE = u'\u2024'*8
    
    def _ctrl_style(self):
        return super(PasswordField, self)._ctrl_style() | wx.TE_PASSWORD

    def _create_widget(self):
        result = super(PasswordField, self)._create_widget()
        if self._type.verify():
            self._ctrl2 = self._create_ctrl()
            sizer = wx.BoxSizer()
            sizer.Add(result,  0, wx.FIXED_MINSIZE)
            sizer.Add(self._ctrl2, 0, wx.FIXED_MINSIZE)
            result = sizer
        else:
            self._ctrl2 = None
        return result
    
    def _set_value(self, value):
        if value:
            value = self._ORIGINAL_VALUE
        super(PasswordField, self)._set_value(value)
        if self._ctrl2:
            self._ctrl2.SetValue(value)

    def _enable(self):
        super(PasswordField, self)._enable()
        if self._ctrl2:
            self._ctrl2.SetEditable(True)

    def _disable(self):
        super(PasswordField, self)._disable()
        if self._ctrl2:
            self._ctrl2.SetEditable(False)
        
    def _set_background_color(self, color):
        super(PasswordField, self)._set_background_color(color)
        if self._ctrl2:
            self._ctrl2.SetOwnBackgroundColour(color)
            self._ctrl2.Refresh()

    def _validate(self):
        value = self._get_value()
        if value == self._ORIGINAL_VALUE:
            return None
        if self._ctrl2:
            verify = self._ctrl2.GetValue()
        else:
            verify = value
        return self._row.validate(self.id(), value, verify=verify)

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
        if up:
            value += self._SPIN_STEP
        else:
            value -= self._SPIN_STEP
        return value
    
    def _cmd_spin(self, up=True):
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
        return self._valid
        
    
class NumericField(TextField, SpinnableField):
    """Textové vstupní políčko pro data typu 'pytis.data.Number'."""
    _SPIN_STEP = 1

    def _create_widget(self):
        result = super(NumericField, self)._create_widget()
        if self._spec.slider() and not self._inline:
            box = wx.BoxSizer()
            slider = wx.Slider(self._parent, -1, style=wx.SL_HORIZONTAL,
                                     minValue=self._type.minimum() or 0,
                                     maxValue=self._type.maximum() is None and 100 or self._type.maximum(),
                                     size=(200, 25))
            wx_callback(wx.EVT_SCROLL, slider, self._on_slider)
            box.Add(result)
            box.Add(slider)
            result = box
        else:
            slider = None
        self._slider = slider
        return result
    
    def _on_slider(self, event):
        self._set_value(str(self._slider.GetValue()))
        
    def _on_change(self, event=None):
        super(NumericField, self)._on_change(event=event)
        value = self._get_value()
        if self._slider:
            try:
                position = int(value)
            except ValueError:
                position = self._slider.GetMin()
            self._slider.SetValue(position)

   
class CheckBoxField(Unlabeled, InputField):
    """Boolean control implemented using 'wx.CheckBox'."""

    def _create_ctrl(self):
        """Vrať instanci 'wx.CheckBox'."""
        if self._inline:
            label = ''
        else:
            label = self.spec().label()
        control = wx.CheckBox(self._parent, -1, label)
        wx_callback(wx.EVT_CHECKBOX, control, control.GetId(), self._on_change)
        return control
                    
    def _get_value(self):
        return self._ctrl.GetValue() and 'T' or 'F'

    def _set_value(self, value):
        assert value in ('T','F',''), ('Invalid value', value)
        wxvalue = value == 'T' and True or False
        self._ctrl.SetValue(wxvalue)
        self._on_change() # call manually, since SetValue() doesn't emit an event.


class EnumerationField(InputField):
    """Common base class for fields with fixed enumerations.
    
    All the derived input fields are represented with a control which contains some sort of fixed
    enumeration of values, such as combo boxes, radio buttons etc.

    """
    _INVALID_SELECTION = wx.NOT_FOUND

    def _enumeration(self):
        return self._row.enumerate(self.id())
    
    def _choices(self):
        return [label for value, label in self._enumeration()]

    def _get_value(self):
        i = self._ctrl.GetSelection()
        if i == wx.NOT_FOUND:
            value = None
        else:
            value = self._enumeration()[i][0]
        return self._type.export(value)

    def _set_value(self, value):
        assert isinstance(value, basestring), value
        values = [self._type.export(v) for v, l in self._enumeration()]
        try:
            selection = values.index(value)
        except ValueError:
            selection = self._INVALID_SELECTION
        self._ctrl.SetSelection(selection)
        self._on_change() # call manually, since SetSelection() doesn't emit an event.


class ChoiceField(EnumerationField):
    """Field with a fixed enumeration represented by 'wx.Choice'."""
    _INVALID_SELECTION = 0
    
    def _enumeration(self):
        return [(None, self._spec.null_display() or '')] + super(ChoiceField, self)._enumeration()
    
    def _create_ctrl(self):
        control = wx.Choice(self._parent, choices=self._choices())
        wx_callback(wx.EVT_CHOICE, control, control.GetId(), self._on_change)
        return control
    

class RadioBoxField(Unlabeled, EnumerationField):
    """Field with a fixed enumeration represented by 'wx.RadioBox'.

    Field specification interpretation details:

      orientation -- the individual radio buttons will be aligned horizontaly or vertically.
      width -- max number of columns (if the orientation is horizontal)
      height -- max number of rows (if the orientation is vertical)

    """
    _DEFAULT_WIDTH = 1

    def _create_ctrl(self):
        if self._spec.orientation() == Orientation.VERTICAL:
            style = wx.RA_SPECIFY_COLS
            dimension = self.width()
        else:
            style = wx.RA_SPECIFY_ROWS
            dimension = self.height()
        label = self.spec().label()
        if label:
            label = label + ':'
        control = wx.RadioBox(self._parent, -1, label,
                              choices=self._choices(), style=style,
                              majorDimension=dimension)
        wx_callback(wx.EVT_RADIOBOX, control, control.GetId(), self._on_change)
        return control


class ListBoxField(EnumerationField):
    """Field with a fixed enumeration represented by 'wx.ListBox'."""
    _DEFAULT_HEIGHT = None
    _DEFAULT_BACKGROUND_COLOR = wx.WHITE
    
    def _create_ctrl(self):
        control = wx.ListBox(self._parent, choices=self._choices(),
                             style=wx.LB_SINGLE|wx.LB_NEEDED_SB)
        if self.height() is not None:
            height = char2px(control, 1, float(10)/7).height * self.height()
            control.SetMinSize((control.GetSize().width, height))
        wx_callback(wx.EVT_LISTBOX, control, control.GetId(), self._on_change)
        return control
    
    def _set_value(self, value):
        super(ListBoxField, self)._set_value(value)
        self._ctrl.SetFirstItem(self._ctrl.GetSelection())
        

class Invocable(object, CommandHandler):
    """Mix-in class for fields capable to invoke a selection.

    The selection can be an enumeration (such as codebook selection) or just a dialog with some
    specific representation of the selected value (such as color selection or date selection).  The
    selection dialog is usually modal.

    The input control will be accompanied with an invocation button and will also handle the
    INVOKE_SELECTION command.
    
    """
    _INVOKE_TITLE = _(u"Vybrat hodnotu")
    _INVOKE_HELP = None
    _INVOKE_ICON = 'invoke-selection'
    
    def _get_command_handler_instance(cls):
        return InputField._get_command_handler_instance()
    _get_command_handler_instance = classmethod(_get_command_handler_instance)
    
    def _create_widget(self):
        widget = super(Invocable, self)._create_widget()
        button = self._create_button('...', icon=self._INVOKE_ICON)
        button.SetToolTipString(self._INVOKE_TITLE)
        self._invocation_button = button
        sizer = wx.BoxSizer()
        sizer.Add(widget, 0, wx.FIXED_MINSIZE)
        sizer.Add(button, 0, wx.FIXED_MINSIZE)
        wx_callback(wx.EVT_BUTTON, button, button.GetId(),
                    lambda e: self._on_invoke_selection())
        wx_callback(wx.EVT_NAVIGATION_KEY, button,
                    self._skip_navigation_callback(button))
        return sizer

    def _button_size(self):
        x = self._px_size(1, 1)[1]
        return (x, x)

    def _create_button(self, label, icon=None):
        return wx_button(self._parent, label=label, icon=icon, size=self._button_size())

    def _disable(self):
        if not self._inline:
            self._invocation_button.Enable(False)
        super(Invocable, self)._disable()
    
    def _enable(self):
        if not self._inline:
            self._invocation_button.Enable(True)
        super(Invocable, self)._enable()
    
    def _menu(self):
        return super(Invocable, self)._menu() + \
               (None,
                UICommand(self.COMMAND_INVOKE_SELECTION(), self._INVOKE_TITLE, self._INVOKE_HELP))
    
    def _on_invoke_selection(self, alternate=False):
        raise ProgramError("This method must be overriden!")
    
    def _cmd_invoke_selection(self, **kwargs):
        self._on_invoke_selection(**kwargs)
        
    def _can_invoke_selection(self, **kwargs):
        return self.enabled()

    
class DateField(Invocable, TextField, SpinnableField):
    """Input field for values of type 'pytis.data.Date'.

    The field implements selection invocation using a calendar widget.

    The field also supports spinning (see 'SpinnableField') by one day per one step.

    """

    _DEFAULT_WIDTH = 10
    _INVOKE_TITLE = _(u"Vybrat z kalendáře")
    _INVOKE_HELP = _(u"Zobrazit kalendář pro výběr datumu.")
    _SPIN_STEP = datetime.timedelta(days=1)
    
    def _on_invoke_selection(self, alternate=False):
        if self._valid:
            d = self._row[self._id].value()
        else:
            d = None
        date = run_dialog(Calendar, d)
        if date != None:
            self._set_value(self._type.export(date))


class TimeField(TextField, SpinnableField):
    """Input field for values of type 'pytis.data.Time'.
    
    The field also supports spinning (see 'SpinnableField') by one hour per one step.

    """
    _SPIN_STEP = datetime.timedelta(hours=1)
    

class ColorSelectionField(Invocable, TextField):
    """Vstupní pole pro výběr barvy."""

    _DEFAULT_WIDTH = 7
    _INVOKE_TITLE = _(u"Vybrat barvu")
    _INVOKE_HELP = _(u"Zobrazit dialog pro výběr barev.")
    
    def _on_invoke_selection(self, alternate=False):
        color = run_dialog(ColorSelector, self._get_value())
        if color != None:
            self._set_value(color)

    def _create_button(self, label, **kwargs):
        size = self._button_size()
        return wx.lib.colourselect.ColourSelect(self._parent, -1, size=size)
    
    def _set_value(self, value):
        if not self._inline:
            self._invocation_button.SetColour(value)
        return super(ColorSelectionField, self)._set_value(value)

    
class GenericCodebookField(InputField):
    """Společná nadtřída číselníkových políček."""

    def _init_attributes(self):
        cb_name = self._row.codebook(self._id)
        assert cb_name is not None
        try:
            cb_spec = resolver().get(cb_name, 'cb_spec')
        except ResolverError:
            cb_spec = CodebookSpec()
        self._cb_name = cb_name
        self._cb_spec = cb_spec
        super(GenericCodebookField, self)._init_attributes()
        self._enumeration_changed = False
        self._row.register_callback(self._row.CALL_ENUMERATION_CHANGE, self._id,
                                    self._on_enumeration_change)
        
    def _on_enumeration_change(self):
        # Callback může být volán i když už je list mrtev.
        if not self._readonly:
            self._needs_validation = True
            self._enumeration_changed = True

    def _on_idle(self, event):
        if self._enumeration_changed:
            self._reload_enumeration()
            self._enumeration_changed = False
        return super(GenericCodebookField, self)._on_idle(event)

    def _reload_enumeration(self):
        pass

    def reload_enumeration(self):
        """Reload enumeration data."""
        self._reload_enumeration()
        
    def _select_row_arg(self):
        """Return the value for RecordForm 'select_row' argument."""
        value = self._row[self.id()]
        if self._valid and value.value():
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
        result = run_form(CodebookForm, self._cb_name, begin_search=begin_search,
                          select_row=self._select_row_arg(), transaction=self._row.transaction(),
                          condition=condition, arguments=self._codebook_arguments())
        if result: # may be None or False!
            self._set_value(result.format(enumerator.value_column()))
        self.set_focus()

    def _codebook_insert(self):
        value_column = self._type.enumerator().value_column()
        if not self._valid and self._modified():
            prefill = {value_column: self._get_value()}
        else:
            prefill = {}
        spec = self.spec().codebook_insert_spec() or self._cb_name
        result = new_record(spec, prefill=prefill, transaction=self._row.transaction())
        if result and value_column in result:
            self._set_value(result[value_column].export())
        
    def _cmd_invoke_codebook_form(self):
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
    _INVOKE_TITLE = _(u"Vybrat z číselníku")
    _INVOKE_HELP = _(u"Zobrazit číselník hodnot s možností výběru.")

    def _init_attributes(self):
        self._insert_button = None
        self._display = None
        super(CodebookField, self)._init_attributes()
        
    def _create_widget(self):
        """Zavolej '_create_widget()' třídy Invocable a přidej displej."""
        widget = super(CodebookField, self)._create_widget()
        spec = self.spec()
        cb_spec = self._cb_spec
        if cb_spec.display() is None and not spec.allow_codebook_insert():
            return widget
        sizer = wx.BoxSizer()
        sizer.Add(widget, 0, wx.FIXED_MINSIZE)
        if cb_spec.display():
            display_size = spec.display_size()
            if display_size is None:
                display_size = cb_spec.display_size()
            if display_size:
                size = self._px_size(display_size, 1)
                display = wx.TextCtrl(self._parent, style=wx.TE_READONLY,
                                      size=size)
                display.SetOwnBackgroundColour(config.field_disabled_color)
                self._display = display
                wx_callback(wx.EVT_NAVIGATION_KEY, display,
                            self._skip_navigation_callback(display))
                sizer.Add(display, 0, wx.FIXED_MINSIZE)
        if spec.allow_codebook_insert():
            button = self._create_button('+', icon='new-record')
            button.SetToolTipString(_(u"Vložit nový záznam do číselníku"))
            wx_callback(wx.EVT_BUTTON, button, button.GetId(), lambda e: self._codebook_insert())
            wx_callback(wx.EVT_NAVIGATION_KEY, button, self._skip_navigation_callback(button))
            sizer.Add(button, 0, wx.FIXED_MINSIZE)
            self._insert_button = button
        return sizer

    def _menu(self):
        return super(CodebookField, self)._menu() + \
               (UICommand(self.COMMAND_INVOKE_SELECTION(alternate=True),
                          _(u"Vyhledávat v číselníku"),
                          _(u"Zobrazit číselník se zapnutým inkrementálním vyhledáváním.")),)

    def _maxlen(self):
        try:
            return self._type.maxlen()
        except AttributeError:
            return None

    def _disable(self):
        if self._insert_button:
            self._insert_button.Enable(False)
        super(CodebookField, self)._disable()        
    
    def _enable(self):
        if self._insert_button:
            self._insert_button.Enable(True)
        super(CodebookField, self)._enable()
        
    def _set_value(self, value):
        super(CodebookField, self)._set_value(value)
        self._update_display()

    def _on_change_hook(self):
        super(CodebookField, self)._on_change_hook()
        self._update_display()

    def _update_display(self):
        if self._display:
            if self._readonly or self._valid:
                display = self._row.display(self.id())
            else:
                display = ''
            self._display.SetValue(display)
        
    def _on_invoke_selection(self, alternate=False):
        value_column = self._type.enumerator().value_column()
        value = self._get_value()
        if not self._valid and value and self._modified() \
               and isinstance(self.type(), pytis.data.String):
            begin_search = (value_column, value)
        elif alternate:
            begin_search = value_column
        else:
            begin_search = self._cb_spec.begin_search()
        self._run_codebook_form(begin_search=begin_search)
    

class ListField(GenericCodebookField):
    """Číselníkové políčko zobrazující data číselníku jako součást formuláře.

    Pokud je 'selection_type' číselníkového políčka ve specifikaci určen jako 'LIST', bude ve
    formuláři použit tento typ vstupního pole.

    """
    _DEFAULT_WIDTH = 30
    _DEFAULT_HEIGHT = 6
    _DEFAULT_BACKGROUND_COLOR = wx.WHITE

    def _create_ctrl(self):
        # Načtu specifikace.
        view_spec = resolver().get(self._cb_name, 'view_spec')
        self._columns = columns = self._cb_spec.columns() or view_spec.columns()
        # Vytvořím vlastní seznamový widget.
        style=wx.LC_REPORT|wx.SIMPLE_BORDER|wx.LC_SINGLE_SEL
        list = wx.ListCtrl(self._parent, -1, style=style)
        # Nastavím záhlaví sloupců.
        total_width = 0
        for i, id in enumerate(columns):
            col = view_spec.field(id)
            list.InsertColumn(i, col.column_label())
            width = col.column_width()
            if width < len(col.column_label()):
                width = len(col.column_label())
            list.SetColumnWidth(i, dlg2px(list, 4*(width+1)))
            total_width = total_width + width
        height = list.GetCharHeight() * 5/4 * (self.height()+ 1) + 10 # TODO: something better?
        self._DEFAULT_WIDTH = total_width + 3
        list.SetMinSize((dlg2px(list, 4*(self.width()+1)), height))
        self._list =  list
        wxid = list.GetId()
        wx_callback(wx.EVT_LIST_ITEM_SELECTED, list, wxid, self._on_select)
        wx_callback(wx.EVT_LIST_ITEM_ACTIVATED, list, wxid, self._on_activation)
        wx_callback(wx.EVT_MOUSEWHEEL, list, lambda e: e.Skip())
        self._selected_item = None
        self._enumeration_changed = True
        self._list_data = []
        self._last_set_invalid_value = None
        return list

    def _on_select(self, event):
        self._list.SetItemState(event.GetIndex(), 0, wx.LIST_STATE_SELECTED)

    def _on_activation(self, event):
        event.Skip()
        i = event.GetIndex()
        if self._enabled and i != self._selected_item:
            self._set_selection(i)
            
    #def _on_kill_focus(self, event):
    #    if self._selected_item is not None:
    #        self._list.EnsureVisible(self._selected_item)
    #    return super(ListField, self)._on_kill_focus(event)
        
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
            sorting = resolver().get(self._cb_name, 'view_spec').sorting()
        rows = enumerator.rows(condition=self._row.runtime_filter(self._id),
                               transaction=self._row.transaction(), sort=sorting or (),
                               arguments=self._codebook_arguments())
        for i, row in enumerate(rows):
            list.InsertStringItem(i, "")
            v = row[value_column]
            self._list_data.append(v)
            if v.export() == current:
                select_item = i
            for j, id in enumerate(self._columns):
                value = row[id]
                if isinstance(value.type(), pytis.data.Boolean):
                    exported_value = value.value() and _(u"Ano") or _(u"Ne")
                else:
                    exported_value = value.export().replace("\n", ";")
                list.SetStringItem(i, j, exported_value)
        self._set_selection(select_item)

    def _disable(self):
        self._update_background_color()
    
    def _set_selection(self, i):
        list = self._list
        if self._selected_item is not None:
            # Deselect the old item.
            fgcolor = wx.SystemSettings.GetColour(wx.SYS_COLOUR_WINDOWTEXT)
            bgcolor = wx.SystemSettings.GetColour(wx.SYS_COLOUR_WINDOW)
            list.SetItemTextColour(self._selected_item, fgcolor)
            list.SetItemBackgroundColour(self._selected_item, bgcolor)
        self._selected_item = i
        if i is not None:
            # Select the new item.
            fgcolor = wx.SystemSettings.GetColour(wx.SYS_COLOUR_HIGHLIGHTTEXT)
            bgcolor = wx.SystemSettings.GetColour(wx.SYS_COLOUR_HIGHLIGHT)
            list.SetItemTextColour(i, fgcolor)
            list.SetItemBackgroundColour(i, bgcolor)
            list.SetItemState(i, wx.LIST_STATE_FOCUSED, wx.LIST_STATE_FOCUSED)
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
        return (UICommand(self.COMMAND_SELECT(), _(u"Vybrat"),
                          _(u"Zvolit tuto položku jako aktivní.")),
                UICommand(self.COMMAND_SHOW_SELECTED(), _(u"Najít vybranou položku"),
                          _(u"Nalistovat v seznamu vybranou položku.")),
                None,
                UICommand(self.COMMAND_INVOKE_CODEBOOK_FORM(), _(u"Zobrazit číselník"),
                          _(u"Otevřít číselníkový formulář.")),
                UICommand(self.COMMAND_EDIT_SELECTED(), _(u"Upravit vybraný záznam"),
                          _(u"Otevřít vybraný záznam v editačním formuláři.")),
                UICommand(self.COMMAND_DELETE_SELECTED(), _(u"Smazat vybraný záznam"),
                          _(u"Vymazat vybraný záznam z číselníku.")),
                UICommand(self.COMMAND_NEW_CODEBOOK_RECORD(), _(u"Vložit nový záznam do číselníku"),
                          _(u"Otevřít formulář pro vložení nového záznamu do číselníku.")),
                UICommand(Application.COMMAND_RUN_FORM(form_class=BrowseForm,
                                                       name=self._cb_name,
                                                       select_row=self._select_row_arg()),
                          _(u"Zobrazit celou tabulku"),
                          _(u"Otevřít náhled číselníku v samostatném řádkovém formuláři.")),
                )

    def _current_row(self):
        view = resolver().get(self._cb_name, 'view_spec')
        data = create_data_object(self._cb_name)
        row = self._type.enumerator().row(self._row[self._id].value(),
                                          transaction=self._row.transaction())
        return PresentedRow(view.fields(), data, row, transaction=self._row.transaction())
    
    # Command handling
    
    def _can_select(self):
        return self.enabled()
    
    def _cmd_select(self):
        i = self._list.GetNextItem(-1, state=wx.LIST_STATE_FOCUSED)
        self._set_selection(i)
        
    def _cmd_show_selected(self):
        self._set_selection(self._selected_item)

    def _can_edit_selected(self, **kwargs):
        return self._selected_item is not None

    def _cmd_edit_selected(self):
        view = resolver().get(self._cb_name, 'view_spec')
        on_edit_record = view.on_edit_record()
        if on_edit_record is not None:
            on_edit_record(row=self._current_row())
        else:
            run_form(PopupEditForm, self._cb_name, select_row=self._select_row_arg(),
                     transaction=self._row.transaction())
        self._reload_enumeration()

    def _can_delete_selected(self):
        return self._selected_item is not None
        
    def _cmd_delete_selected(self):
        view = resolver().get(self._cb_name, 'view_spec')
        data = create_data_object(self._cb_name)
        transaction = self._row.transaction()
        row = self._current_row()
        question = _(u"Opravdu chcete položku %s zcela vymazat z číselníku?") % self._row[self._id].export()
        delete_record(view, data, transaction, row, question=question)
        self._reload_enumeration()
        
    def _cmd_new_codebook_record(self):
        self._codebook_insert()
        self._reload_enumeration()

    def _cmd_invoke_codebook_form(self):
        super(ListField, self)._cmd_invoke_codebook_form()
        self._reload_enumeration()
        

class FileField(Invocable, InputField):
    """Input field for manipulating generic binary data."""
    
    _INVOKE_TITLE = _(u"Vybrat soubor")
    _INVOKE_HELP = _(u"Zobrazit dialog pro procházení systému souborů.")
    _INVOKE_ICON = wx.ART_FILE_OPEN

    _last_load_dir = None
    _last_save_dir = None

    def _init_attributes(self):
        self._buffer = None
        super(FileField, self)._init_attributes()
        
    def _create_ctrl(self):
        ctrl = wx.TextCtrl(self._parent, -1, '', size=self._px_size(8, 1))
        ctrl.SetEditable(False)
        ctrl.SetOwnBackgroundColour(config.field_disabled_color)
        return ctrl

    def _button_size(self):
        x = self._px_size(1, 1)[1]
        return (x+5, x+2)
    
    def _validate(self):
        filename = self._buffer and self._buffer.filename()
        return self._row.validate(self.id(), self._get_value(), filename=filename)
        
    def _get_value(self):
        return self._buffer and self._buffer.buffer()

    def _set_value(self, value):
        assert value is None or isinstance(value, buffer)
        self._buffer = value and self._type.Buffer(value) or None
        self._on_set_value()
        self._on_change()

    def _on_set_value(self):
        if self._buffer is None:
            display = ""
        else:
            display = format_byte_size(len(self._buffer))
        self._ctrl.SetValue(display)

    def _on_invoke_selection(self, alternate=False):
        FileField.COMMAND_LOAD.invoke(_command_handler=self)

    def _menu(self):
        # We really want to use Invocable's super method, since we don't
        # want the Invocable menu items.
        return super(Invocable, self)._menu() + \
               (None,
                UICommand(FileField.COMMAND_LOAD(), _(u"Nastavit ze souboru"),
                          _(u"Nahradit hodnotu políčka daty ze souboru. ")),
                UICommand(FileField.COMMAND_SAVE(), _(u"Uložit do souboru"),
                          _(u"Uložit objekt z databáze jako soubor.")),
                UICommand(FileField.COMMAND_CLEAR(), _(u"Vynulovat"),
                          _(u"Nastavit prázdnou hodnotu.")),
                )

    def _can_load(self):
        return self._enabled
        
    def _cmd_load(self):
        msg = _(u"Vyberte soubor pro políčko '%s'") % self.spec().label()
        dir = FileField._last_load_dir or FileField._last_save_dir or ''
        dlg = wx.FileDialog(self._parent, message=msg, style=wx.OPEN,
                            defaultDir=dir)
        if dlg.ShowModal() == wx.ID_OK:
            path = dlg.GetPath()
            filename = os.path.split(path)[1]
            FileField._last_load_dir = os.path.dirname(path)
            try:
                if self._buffer:
                    self._buffer.load(path, filename=filename)
                else:
                    self._buffer = self._type.Buffer(path, filename=filename)
            except pytis.data.ValidationError as e:
                message(e.message(), beep_=True)
            except IOError as e:
                message(_(u"Chyba při čtení souboru:")+' '+str(e), beep_=True)
            else:
                self._on_set_value()
                self._on_change()
                message(_(u"Soubor načten."))
        
    def _can_save(self):
        return self._buffer is not None
        
    def _cmd_save(self):
        msg = _(u"Uložit hodnotu políčka '%s'") % self.spec().label()
        dir = FileField._last_save_dir or FileField._last_load_dir or ''
        dlg = wx.FileDialog(self._parent, style=wx.SAVE, message=msg,
                            defaultDir=dir)
        if dlg.ShowModal() == wx.ID_OK:
            path = dlg.GetPath()
            FileField._last_save_dir = os.path.dirname(path)
            try:
                self._buffer.save(path)
            except IOError as e:
                message(_(u"Chyba při zápisu souboru:")+' '+str(e), beep_=True)
            else:
                message(_(u"Soubor uložen."))
        
    def _can_clear(self):
        return self._enabled and self._buffer is not None
        
    def _cmd_clear(self):
        self._set_value(None)


class ImageField(FileField):
    """Input field for bitmap images showing a thumbnail within the control."""

    _DEFAULT_WIDTH = _DEFAULT_HEIGHT = 80
    _DEFAULT_BACKGROUND_COLOR = wx.WHITE
    
    def _create_ctrl(self):
        return wx_button(self._parent, bitmap=self._bitmap(),
                         size=(self.width()+10, self.height()+10),
                         callback=lambda e: self._on_button())

    def _disable(self):
        super(ImageField, self)._disable()
        self._ctrl.Enable(True)
        
    def _button_size(self):
        x = self._px_size(1, 1)[1]
        return (x+4, x+2)
    
    def _bitmap(self):
        if self._buffer is not None:
            import PIL.Image
            img = self._buffer.image().copy()
            img.thumbnail((self.width(), self.height()), PIL.Image.ANTIALIAS)
            stream = StringIO()
            img.save(stream, 'PNG')
            stream.seek(0)
            wximg = wx.EmptyImage()
            wximg.LoadStream(stream, type=wx.BITMAP_TYPE_PNG)
            return wx.BitmapFromImage(wximg)
        return wx.EmptyBitmap(1, 1, depth=1)
    
    def _on_button(self):
        if self.COMMAND_VIEW.enabled():
            self.COMMAND_VIEW.invoke()
    
    def _on_set_value(self):
        self._ctrl.SetBitmapLabel(self._bitmap())
    
    def _can_view(self):
        return self._buffer is not None 
        
    def _cmd_view(self):
        path = os.tempnam()+"."+self._buffer.image().format.lower()
        command = config.image_viewer
        if command.find('%f') != -1:
            command = command.replace('%f', path)
        else:
            command += " "+path
        log(OPERATIONAL, "Running external viewer:", command)
        try:
            self._buffer.save(path)
            os.system(command)
        finally:
            os.remove(path)
