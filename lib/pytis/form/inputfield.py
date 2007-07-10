# -*- coding: iso-8859-2 -*-

# Copyright (C) 2001-2006, 2007 Brailcom, o.p.s.
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

import pytis.data
from pytis.form import *
import wx.lib.colourselect
from cStringIO import StringIO
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
            message(_("Nepovolen� znak!"), beep_=True)
            return True
        else: 
            event.Skip()
            return True

        
class InputField(object, KeyHandler, CallbackHandler, CommandHandler):
    """Abstraktn� t��da vstupn�ho pole.

    Vstupn� pol��ko nen� samo o sob� wx prvkem. Odpov�daj�c� prvky
    u�ivatelsk�ho rozhran� lze z�skat metodami 'label()' a 'widget()'.
    Pol��ko je rozd�leno na ��sti widget a label, aby mohly b�t tyto dv� ��sti
    um�st�ny do gridu...

    Tato t��da nen� sama o sob� instanciovateln�! Odvozen�m dal��
    t��dy a p�edefinov�n�m d�le popsan�ch metod v�ak lze vytvo�it pol��ka
    s libvoln�m chov�n�m realizovan� libovoln�m UI prvkem.

    T��da je 'CallbackHandler'. Argument callbackov� funkce z�vis� na typu
    callbacku a je zdokumentov�n v dokumentaci callbackov� konstanty.
    
    """

    _DEFAULT_WIDTH = 13
    _DEFAULT_HEIGHT = 1
    _DEFAULT_BACKGROUND_COLOR = None

    CALL_FIELD_CHANGE = 'CALL_FIELD_CHANGE'
    """Callback volan� p�i ka�d� zm�n� hodnoty. Argumentem je instance pol��ka.

    Callback je vol�n pouze p�i interaktivn� (u�ivatelem vyvolan�) zm�n�
    hodnoty a p�i inicializaci hodnoty pol��ka.  Ostatn� programov� nastavov�n�
    hodnoty callback nevyvol�v�.
    
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
        assert isinstance(id, str)
        assert isinstance(inline, bool)
        spec = find(id, row.fields(), key=lambda f: f.id())
        type = row[id].type()
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
        elif isinstance(type, pytis.data.Color):
            field = ColorSelectionField
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

          id -- field specification as a 'FieldSpec' instance.
            
          inline -- if true, only the basic input widget is created.  The label and all surrounding
            widgets are omitted, so that the widget can be used in the inline editation mode in of
            a table cell.
          
          guardian -- parent 'KeyHandler'.
          
          readonly -- 

        This method should not be overriden by derived classes.  All field specific initialization
        should be done in the methods '_create_widget()' and '_create_label'.

        """
        assert isinstance(row, PresentedRow)
        assert isinstance(id, str)
        assert isinstance(guardian, KeyHandler)
        assert isinstance(inline, bool)
        CallbackHandler.__init__(self)
        spec = find(id, row.fields(), key=lambda f: f.id())
        self._parent = parent
        self._row = row
        self._type = row[id].type()
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
        self._readonly = readonly
        self._enabled = not denied and not readonly and row.editable(id)
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
            value = row.format(id)
        self._set_value(value)
        self._call_on_idle = self._update_background_color
        
    def _init_attributes(self):
        pass
        
    def __str__(self):
        return "<%s id='%s'>" % (self.__class__.__name__, self.id())
        
    def _skip_navigation_callback(self, widget):
        def cb(e):
            if not self._unregistered_widgets.has_key(widget):
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
        return ((InputField.COMMAND_RESET,
                 _("Vr�tit p�vodn� hodnotu"),
                 _("Vr�tit ve�ker� proveden� zm�ny.")),)

    def _mitem(self, command, title=None, help=None):
        if command is None:
            return MSeparator()
        else:
            if isinstance(command, tuple):
                command, kwargs = command
            else:
                kwargs = {}
            if issubclass(command.handler(), (InputField, Invocable)):
                kwargs['_command_handler'] = self
            return MItem(title, command=command(**kwargs), help=help)
                        
    def _on_context_menu(self, event=None):
        control = self._ctrl
        if event:
            position = None
        else:
            size = control.GetSize()
            position = (size.x/3, size.y/2)
        items = [self._mitem(*args) for args in self._menu()]
        self._set_focus()
        menu = Menu('', items).create(control, global_keymap())
        control.PopupMenu(menu, position)
        menu.Destroy()
        #event.Skip()

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
        # TODO: Zkusit to p�es `wx.Window.SetFocusFromKbd()'
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
            wx_callback(wx.EVT_NAVIGATION_KEY, control,
                        self._skip_navigation_callback(control))
            self._callback_registered = True
        if self._unregistered_widgets.has_key(control):
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
        self._needs_validation = True
        if event:
            event.Skip()

    def _enable(self):
        self._ctrl.Enable(True)

    def _disable(self):
        if not self._readonly and not self._denied:
            self._ctrl.Enable(False)
        else:
            # Here we rely on a simple hack in InputField._on_change() which
            # resets field value after each attempt to chnge it, so the field
            # is in fact editable, but it is not possible to change it
            # effectively.
            pass

    def _update_background_color(self):
        if self._readonly:
            return
        elif self._denied:
            color = config.field_denied_color
        elif not self._enabled:
            color = config.field_disabled_color
        elif not self._valid:
            color = config.field_invalid_color
        else:
            color = self._DEFAULT_BACKGROUND_COLOR
        self._ctrl.SetOwnBackgroundColour(color)
        self._ctrl.Refresh()

    def _modified(self):
        # Returns always false for virtual fields
        return self._row.field_changed(self.id())

    def _px_size(self, width, height):
        return dlg2px(self._parent, 4*(width+1)+2, 8*height+4.5)
    
    def _set_focus(self):
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
        """Return field specification as a 'FieldSpec' instance."""
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
                run_dialog(Error, title=_("Chyba validace"),
                           message=_('Chyba validace pol��ka!\n\n%s: %s') % \
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
        self._set_value(self._row.original_row()[self.id()].export())

        
class Unlabeled:
    """Mix-in t��da pro pol��ka .

    N�kter� prvky maj� label spojen p��mo s controlem, tak�e label zobrazen�
    v gridu mus� b�t pr�zdn�.

    """
    def _create_label(self):
        # Return an empty label as 'wx.StaticText' instance.
        return wx.StaticText(self._parent, -1, '')


class TextField(InputField):
    """Textov� vstupn� pol��ko."""
    
    NUMBERS = map(str, range(10))
    SIGNS = ['-', '+']
    DECIMAL_POINTS = ['.', ',']
    FLOAT = map(str, range(10)) + SIGNS + DECIMAL_POINTS
    ASCII   = map(chr, range(127))
    LETTERS = map(chr, range(ord('a'),ord('z')+1) + \
                  range(ord('A'),ord('Z')+1))

    def _create_ctrl(self):
        style = wx.TE_PROCESS_ENTER
        if self.height() > 1:
            style |= wx.TE_MULTILINE
        if not self._inline:
            size = self._px_size(self.width(), self.height())
        else:
            size = None
        control = wx.TextCtrl(self._parent, -1, '', style=style, size=size)
        wxid = control.GetId()
        maxlen = self._maxlen()
        if maxlen is not None:
            control.SetMaxLength(maxlen)
            wx_callback(wx.EVT_TEXT_MAXLEN, control, wxid,
               lambda e: message(_("P�ekro�ena maxim�ln� d�lka."), beep_=True))
        filter = self._filter()
        control.SetValidator(_TextValidator(control, filter=filter))
        wx_callback(wx.EVT_TEXT, control, wxid, self._on_change)
        wx_callback(wx.EVT_TEXT_ENTER, control, wxid, self._on_enter_key)
        return control

    def _maxlen(self):
        """Vra� maxim�ln� d�lku zadan�ho textu."""
        return None

    def _on_enter_key(self, event):
        if self.height() > 1:
            event.Skip()
        else:
            event.GetEventObject().Navigate()

    def _on_change(self, event=None):
        post_process = self._post_process_func()
        if post_process:
            value = post_process(self._get_value())
            if value != self._get_value():
                self._set_value(value)
        super(TextField, self)._on_change(event=event)

    def _post_process_func(self):
        """Vra� funkci odpov�daj�c� specifikaci postprocessingu pol��ka.

        Vrac�: Funkci jednoho argumentu (p�vodn� text), kter� vrac�
        �et�zec (zm�n�n� text).
        
        """
        try:
            return self._stored_post_process_func
        except:            
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
        """Vra� filtra�n� funkci odpov�daj�c� specifikaci pol��ka.
        
        Vrac�: Funkci jednoho argumentu, kter� vrac� pravdu, pokud znak
        odpov�d� specifikaci filtru pro dan� pol��ko, nepravdu v opa�n�m
        p��pad�.

        Pokud pol��ko nem� nastavenu filtraci, vrac� None.
        
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

    def _set_value(self, value):
        assert isinstance(value, (str, unicode)), value
        self._ctrl.SetValue(value)
        self._on_change() # call manually, since SetValue() doesn't emit an event.

    def _menu(self):
        return super(TextField, self)._menu() + \
               ((None,),
                (TextField.COMMAND_CUT,
                 _("Vyjmout"),
                 _("Vyjmout ozna�en� text a ulo�it jej do schr�nky.")),
                (TextField.COMMAND_COPY,
                 _("Kop�rovat"),
                 _("Zkop�rovat ozna�en� text do schr�nky.")),
                (TextField.COMMAND_PASTE,
                 _("Vlo�it"),
                 _("Vlo�it text ze schr�nky do pol��ka.")),
                (TextField.COMMAND_SELECT_ALL,
                 _("Vybrat v�e"),
                 _("Ozna�it celou hodnotu.")))

    # Zpracov�n� p��kaz�
    
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
    """Textov� vstupn� pol��ko pro data typu 'pytis.data.String'."""

    def _maxlen(self):
        return self._type.maxlen()

    
class NumericField(TextField):
    """Textov� vstupn� pol��ko pro data typu 'pytis.data.Number'."""
    pass


class CheckBoxField(Unlabeled, InputField):
    """Boolean control implemented using 'wx.CheckBox'."""

    def _create_ctrl(self):
        """Vra� instanci 'wx.CheckBox'."""
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
    def _choices(self):
        return [x[1] for x in self._row.enumerate(self.id())]

    def _get_value(self):
        i = self._ctrl.GetSelection()
        value = self._type.enumerator().values()[i]
        return self._type.export(value)

    def _set_value(self, value):
        assert isinstance(value, (str, unicode)), value
        values = [self._type.export(v) for v in self._type.enumerator().values()]
        try:
            selection = values.index(value)
        except ValueError:
            selection = wx.NOT_FOUND
        self._ctrl.SetSelection(selection)
        self._on_change() # call manually, since SetSelection() doesn't emit an event.


class ChoiceField(EnumerationField):
    """Field with a fixed enumeration represented by 'wx.Choice'."""

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
    _INVOKE_TITLE = _("Vybrat hodnotu")
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
        return wx_button(self._parent, label=label, icon=icon,
                         size=self._button_size())

    def _disable(self):
        self._invocation_button.Enable(False)
        super(Invocable, self)._disable()
    
    def _enable(self):
        self._invocation_button.Enable(True)
        super(Invocable, self)._enable()
    
    def _menu(self):
        return super(Invocable, self)._menu() + \
               ((None,),
                (self.COMMAND_INVOKE_SELECTION,
                 self._INVOKE_TITLE, self._INVOKE_HELP))
    
    def _on_invoke_selection(self, alternate=False):
        raise ProgramError("This method must be overriden!")
    
    def _cmd_invoke_selection(self, **kwargs):
        self._on_invoke_selection(**kwargs)
        
    def _can_invoke_selection(self, **kwargs):
        return self.enabled()

    
class DateField(Invocable, TextField):
    """Vstupn� pole pro datov� typ 'pytis.data.Date'.

    Jako akci pro vyvol�n� v�b�ru definuje zobrazen� dialogu s kalend��em,
    kter� je nastaven na datum odpov�daj�c� hodnot� pol��ka a po ukon�en�
    nastav� hodnotu pol��ka na vybran� datum.

    """

    _DEFAULT_WIDTH = 10
    _INVOKE_TITLE = _("Vybrat z kalend��e")
    _INVOKE_HELP = _("Zobrazit kalend�� pro v�b�r datumu.")
    
    def _on_invoke_selection(self, alternate=False):
        if self._valid:
            d = self._row[self._id].value()
        else:
            d = None
        date = run_dialog(Calendar, d)
        if date != None:
            self._set_value(self._type.export(date))


class ColorSelectionField(Invocable, TextField):
    """Vstupn� pole pro v�b�r barvy."""

    _DEFAULT_WIDTH = 7
    _INVOKE_TITLE = _("Vybrat barvu")
    _INVOKE_HELP = _("Zobrazit dialog pro v�b�r barev.")
    
    def _on_invoke_selection(self, alternate=False):
        color = run_dialog(ColorSelector, self._get_value())
        if color != None:
            self._set_value(color)

    def _create_button(self, label, **kwargs):
        size = self._button_size()
        return wx.lib.colourselect.ColourSelect(self._parent, -1, size=size)
    
    def _set_value(self, value):
        self._invocation_button.SetColour(value)
        return super(ColorSelectionField, self)._set_value(value)

    
class GenericCodebookField(InputField):
    """Spole�n� nadt��da ��seln�kov�ch pol��ek."""

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
        # Callback m��e b�t vol�n i kdy� u� je list mrtev.
        self._needs_validation = True
        self._enumeration_changed = True

    def _on_idle(self, event):
        if self._enumeration_changed:
            self._reload_enumeration()
            self._enumeration_changed = False
        return super(GenericCodebookField, self)._on_idle(event)

    def _reload_enumeration(self):
        pass
        
    def _select_row_arg(self):
        """Return the value for RecordForm 'select_row' arguemnt."""
        value = self._row[self.id()]
        if self._valid and value.value():
            return {self._type.enumerator().value_column(): value}
        else:
            return None
    
    def _run_codebook_form(self, begin_search=None):
        """Zobraz ��seln�k a po jeho skon�en� nastav hodnotu pol��ka."""
        enumerator = self._type.enumerator()
        validity_condition = enumerator.validity_condition()
        runtime_filter_condition = self._row.runtime_filter(self.id())
        if validity_condition and runtime_filter_condition:
            condition = pytis.data.AND(validity_condition, runtime_filter_condition)
        else:    
            condition = validity_condition or runtime_filter_condition            
        result = run_form(CodebookForm, self._cb_name, begin_search=begin_search,
                          select_row=self._select_row_arg(), transaction=self._row.transaction(),
                          condition=condition)
        if result: # may be None or False!
            self._set_value(result.format(enumerator.value_column()))
        self.set_focus()

    def _cmd_invoke_codebook_form(self):
        self._run_codebook_form()
    

class CodebookField(Invocable, GenericCodebookField, TextField):
    """Vstupn� pole pro data nav�zan� na ��seln�k.

    Bude pou�ito v p��pad�, �e datov� typ definuje enumer�tor typu
    'pytis.data.DataEnumerator' a prezenta�n� specifikace pol��ka definuje
    nav�zan� ��seln�k (viz. argument 'codebook' konstruktoru 'FieldSpec').

    Jako akci pro vyvol�n� v�b�ru definuje zobrazen� formul��e
    'pytis.form.CodebookForm'.  N�zev specifikace ��seln�ku je d�n v��e
    zm�n�n�m specifik�torem 'codebook'.  Dal�� vlastnosti ��seln�kov�ho
    formul��e jsou d�ny jednak specifikac� 'cb_spec' v odkazovan� specifikaci a
    jednak p��mo specifikac� 'view_spec' tamt�.

    K pol��ku m��e b�t voliteln� p�idru�en displej, kter� slou�� k zobrazen�
    popisu vybran� (aktu�ln�) hodnoty ��seln�ku. 

    """
    _INVOKE_TITLE = _("Vybrat z ��seln�ku")
    _INVOKE_HELP = _("Zobrazit ��seln�k hodnot s mo�nost� v�b�ru.")

    def _init_attributes(self):
        self._insert_button = None
        self._display = None
        super(CodebookField, self)._init_attributes()
    
    def _create_widget(self):
        """Zavolej '_create_widget()' t��dy Invocable a p�idej displej."""
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
            button.SetToolTipString(_("Vlo�it nov� z�znam do ��seln�ku"))
            wx_callback(wx.EVT_BUTTON, button, button.GetId(),
                        self._on_codebook_insert)
            wx_callback(wx.EVT_NAVIGATION_KEY, button,
                        self._skip_navigation_callback(button))
            sizer.Add(button, 0, wx.FIXED_MINSIZE)
            self._insert_button = button
        return sizer

    def _menu(self):
        return super(CodebookField, self)._menu() + \
               ((self.COMMAND_INVOKE_SELECTION(alternate=True),
                 _("Vyhled�vat v ��seln�ku"),
                 _("Zobrazit ��seln�k se zapnut�m inkrement�ln�m "
                   "vyhled�v�n�m.")),)

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

    def _on_change_hook(self):
        super(CodebookField, self)._on_change_hook()
        if self._display:
            display = self._valid and self._row.display(self.id()) or ''
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

    def _on_codebook_insert(self, event):
        value_column = self._type.enumerator().value_column()
        if not self._valid and self._modified():
            prefill = {value_column: self._get_value()}
        else:
            prefill = {}
        spec = self.spec().codebook_insert_spec() or self._cb_name
        result = new_record(spec, prefill=prefill, transaction=self._row.transaction())
        if result and result.has_key(value_column):
            self._set_value(result[value_column].export())
        return True
    

class ListField(GenericCodebookField):
    """��seln�kov� pol��ko zobrazuj�c� data ��seln�ku jako sou��st formul��e.

    Pokud je 'selection_type' ��seln�kov�ho pol��ka ve specifikaci ur�en jako 'LIST', bude ve
    formul��i pou�it tento typ vstupn�ho pole.

    """
    _DEFAULT_WIDTH = 30
    _DEFAULT_HEIGHT = 6
    _DEFAULT_BACKGROUND_COLOR = wx.WHITE

    def _create_ctrl(self):
        # Na�tu specifikace.
        view_spec = resolver().get(self._cb_name, 'view_spec')
        self._columns = columns = self._cb_spec.columns() or view_spec.columns()
        # Vytvo��m vlastn� seznamov� widget.
        style=wx.LC_REPORT|wx.SUNKEN_BORDER|wx.LC_SINGLE_SEL
        list = wx.ListCtrl(self._parent, -1, style=style)
        # Nastav�m z�hlav� sloupc�.
        total_width = 0
        for i, id in enumerate(columns):
            col = view_spec.field(id)
            list.InsertColumn(i, col.label())
            width = col.column_width()
            if width < len(col.label()):
                width = len(col.label())
            list.SetColumnWidth(i, dlg2px(list, 4*(width+1)))
            total_width = total_width + width
        # TODO/wx: N�jak spo��tat skute�nou v��ku z�hlav� a ��dku.
        # Tohle jsou "empirick�" vzorce!!!
        header_height = char2px(list, 1, float(9)/4).GetHeight()
        row_height = char2px(list, 1, float(10)/7).GetHeight()
        height = header_height + row_height * self.height()
        self._DEFAULT_WIDTH = total_width + 3
        list.SetMinSize((dlg2px(list, 4*(self.width()+1)), height))
        self._list =  list
        wxid = list.GetId()
        wx_callback(wx.EVT_LIST_ITEM_SELECTED, list, wxid, self._on_select)
        wx_callback(wx.EVT_LIST_ITEM_ACTIVATED, list, wxid, self._on_activation)
        wx_callback(wx.EVT_MOUSEWHEEL, list, lambda e: e.Skip())
        self._selected_item = None
        self._reload_enumeration()
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
        current = self._get_value()
        list = self._list
        list.DeleteAllItems()
        self._list_data = []
        select_item = None
        enumerator = self.type().enumerator()
        value_column = enumerator.value_column()
        rows = enumerator.rows(condition=self._row.runtime_filter(self._id))
        for i, row in enumerate(rows):
            list.InsertStringItem(i, "")
            v = row[value_column]
            self._list_data.append(v)
            if v.export() == current:
                select_item = i
            for j, id in enumerate(self._columns):
                list.SetStringItem(i, j, row[id].export().replace("\n", ";"))
        self._set_selection(select_item)

    def _disable(self):
        self._update_background_color()
    
    def _set_selection(self, i):
        list = self._list
        if self._selected_item is not None:
            # Deselect the old item.
            list.SetItemBackgroundColour(self._selected_item, None)
        self._selected_item = i
        if i is not None:
            bgcolor = wx.SystemSettings.GetColour(wx.SYS_COLOUR_HIGHLIGHT)
            # TODO: This doesn't work correctly.  Only the later call has an
            # effect (regardless which one it is).  Probably a wx problem...
            # If solved, the text color should be also restored above.
            # list.SetItemTextColour(i, wx.WHITE)
            list.SetItemBackgroundColour(i, bgcolor)
            list.SetItemState(i, wx.LIST_STATE_FOCUSED, wx.LIST_STATE_FOCUSED)
            list.EnsureVisible(i)
        self._on_change()

    def _set_value(self, value):
        if value:
            for i, v in enumerate(self._list_data):
                if v.export() == value:
                    self._set_selection(i)
                    return True
            else:
                # Not in list.
                self._set_selection(None)
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
        return ((self.COMMAND_SELECT,
                 _("Vybrat"),
                 _("Zvolit tuto polo�ku jako aktivn�.")),
                (self.COMMAND_SHOW_SELECTED,
                 _("Naj�t vybranou polo�ku"),
                 _("Nalistovat v seznamu vybranou polo�ku.")),
                (None,),
                (self.COMMAND_INVOKE_CODEBOOK_FORM,
                 _("Zobrazit ��seln�k"),
                 _("Otev��t odpov�daj�c� ��seln�kov� formul��.")),
                (self.COMMAND_INVOKE_EDIT_FORM,
                 _("Editovat vybran� z�znam"),
                 _("Otev��t vybran� z�znam v edita�n�m formul��i.")),
                (Application.COMMAND_RUN_FORM(form_class=BrowseForm,
                                              name=self._cb_name,
                                              select_row=self._select_row_arg()),
                 _("Zobrazit celou tabulku"),
                 _("Otev��t n�hled ��seln�ku v ��dkov�m formul��i.")),
                )

    # Command handling
    
    def _can_select(self):
        return self.enabled()
    
    def _cmd_select(self):
        i = self._list.GetNextItem(-1, state=wx.LIST_STATE_FOCUSED)
        self._set_selection(i)
        
    def _cmd_show_selected(self):
        self._set_selection(self._selected_item)

    def _can_invoke_edit_form(self, **kwargs):
        return self._selected_item is not None

    def _cmd_invoke_edit_form(self):
        run_form(PopupEditForm, self._cb_name, select_row=self._select_row_arg())


class FileField(Invocable, InputField):
    """Input field for manipulating generic binary data."""
    
    _INVOKE_TITLE = _("Vybrat soubor")
    _INVOKE_HELP = _("Zobrazit dialog pro proch�zen� syst�mu soubor�.")
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
               ((None,),
                (FileField.COMMAND_LOAD,
                 _("Nastavit ze souboru"),
                 _("Nahradit hodnotu pol��ka daty ze souboru. ")),
                (FileField.COMMAND_SAVE,
                 _("Ulo�it do souboru"),
                 _("Ulo�it objekt z datab�ze jako soubor.")),
                (FileField.COMMAND_CLEAR,
                 _("Vynulovat"),
                 _("Nastavit pr�zdnou hodnotu.")),
                )

    def _can_load(self):
        return self._enabled
        
    def _cmd_load(self):
        msg = _("Vyberte soubor pro pol��ko '%s'") % self.spec().label()
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
            except pytis.data.ValidationError, e:
                message(e.message(), beep_=True)
            except IOError, e:
                message(_("Chyba p�i �ten� souboru:")+' '+str(e), beep_=True)
            else:
                self._on_set_value()
                self._on_change()
                message(_("Soubor na�ten."))
        
    def _can_save(self):
        return self._buffer is not None
        
    def _cmd_save(self):
        msg = _("Ulo�it hodnotu pol��ka '%s'") % self.spec().label()
        dir = FileField._last_save_dir or FileField._last_load_dir or ''
        dlg = wx.FileDialog(self._parent, style=wx.SAVE, message=msg,
                            defaultDir=dir)
        if dlg.ShowModal() == wx.ID_OK:
            path = dlg.GetPath()
            FileField._last_save_dir = os.path.dirname(path)
            try:
                self._buffer.save(path)
            except IOError, e:
                message(_("Chyba p�i z�pisu souboru:")+' '+str(e), beep_=True)
            else:
                message(_("Soubor ulo�en."))
        
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
