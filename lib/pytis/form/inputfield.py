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
            message(_("Nepovolený znak!"), beep_=True)
            return True
        else: 
            event.Skip()
            return True

        
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
        assert isinstance(id, str)
        assert isinstance(inline, bool)
        spec = find(id, row.fields(), key=lambda f: f.id())
        type = row[id].type()
        if type.enumerator() is not None:
            codebook = spec.codebook(row.data())
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
                    SelectionType.RADIO_BOX: RadioBoxField,
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
        data = row.data()
        if data.find_column(id) is not None:
            if row.new():
                permission = pytis.data.Permission.INSERT
            else:
                permission = pytis.data.Permission.UPDATE
            denied = not data.permitted(id, permission)
        else:
            denied = False
        spec = find(id, row.fields(), key=lambda f: f.id())
        self._row = row
        self._parent = parent
        self._type = row[id].type()
        self._type = spec.type(data)
        self._spec = spec
        self._guardian = guardian
        self._id = id = spec.id()
        self._inline = inline
        self._want_focus = False
        self._denied = denied
        self._readonly = readonly
        self._enabled = not denied and not readonly and row.editable(id)
        self._callback_registered = False
        self._unregistered_widgets = {}
        self._is_changed = False
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
        if self._spec.descr() is not None and config.show_tooltips:
            ctrl.SetToolTipString(self._spec.descr())
        if not self._enabled:
            self._disable()
            self._register_skip_navigation_callback()
        if not inline:
            row.register_callback(row.CALL_CHANGE, id, self._change_callback)
            row.register_callback(row.CALL_EDITABILITY_CHANGE, id, self._editability_change_callback)
        #self._disable_event_handlers()
        self.set_value(row.format(id))
        self._on_change()
        #self._enable_event_handlers()
        
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

    def _menu(self):
        # Return a tuple of popup menu items ('MItem' instances).
        return ((InputField.COMMAND_RESET,
                 _("Vrátit původní hodnotu"),
                 _("Vrátit veškeré provedené změny.")),)

    def guardian(self):
        return self._guardian

    # Zpracování příkazů
    
    def _can_reset(self):
        return self.is_modified() and self.is_enabled()

    def _cmd_reset(self):
        self.reset()

    def _cmd_context_menu(self):
        self._on_context_menu()

    # Ostatní neveřejné metody.

    def _mitem(self, command, title=None, help=None):
        if command is None:
            return MSeparator()
        else:
            if isinstance(command, types.TupleType):
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

    def _on_idle(self, event):
        if self._is_changed:
            self._is_changed = False
            value = self._value()
            if value and value != self._row[self.id()]:
                self._row[self.id()] = value
            self._on_change_hook()
        if self._want_focus and not self._has_focus():
            self._set_focus()
        if hasattr(self, '_call_on_idle') and self._call_on_idle is not None:
            self._call_on_idle()
            self._call_on_idle = None
        event.Skip()
        return True

    def _on_change_hook(self):
        """Ošetři změny textu políčka.
        
        Předefinováním této metody lze provádět libovolné doplňující akce
        při každé změně textu políčka.
        
        """
        pass
        
    def _on_set_focus(self, event):
        self._want_focus = False
        last = InputField._last_focused()
        # TODO: Zkusit to přes `wx.Window.SetFocusFromKbd()'
        if last is not None and last is not self and last.is_enabled() and last.is_modified():
            value, error = last.validate(interactive=False)
            if error:
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
        if self.get_value() != value:
            self.set_value(value)

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
        # Called on user interaction (editation, selection).  The actual processing of the change
        # is postponed to the idle thread to avoid user interface hangs on time-consuming
        # operations (such as complicated field recomputations).
        self._is_changed = True
        if event:
            event.Skip()

    def _px_size(self, width, height):
        return dlg2px(self._parent, 4*(width+1)+2, 8*height+4.5)
    
    def _has_focus(self):
        """Vrať pravdu právě když je políčko zaostřeno pro uživ. vstup."""
        return InputField.focused() is self

    def width(self):
        """Vrať šířku políčka danou specifikací; počet znaků."""
        return self.spec().width(self._DEFAULT_WIDTH)

    def height(self):
        """Vrať výšku políčka danou specifikací; počet znaků."""
        return self.spec().height(self._DEFAULT_HEIGHT)

    def id(self):
        """Vrať identifikátor políčka (string)."""
        return self._id

    def spec(self):
        """Vrať prezentační specifikaci políčka jako 'FieldSpec'."""
        return self._spec

    def type(self):
        """Vrať datový typ políčka jako instanci 'pytis.data.Type'."""
        return self._type

    def widget(self):
        """Vrať ovládací prvek jako instanci 'wx.Window'."""
        return self._widget

    def label(self):
        """Vrať nadpis políčka jako 'wx.StaticText'."""
        return self._label

    def validate(self, quiet=False, interactive=True, **kwargs):
        """Zvaliduj hodnotu políčka a vrať instanci 'Value' a popis chyby.

        Argumenty:
        
          quiet -- v případě pravdivé hodnoty je výsledek validace metodou
            pouze vrácen a chyba není nijak ohlašována.  V opačném případě je
            chyba ohlášena způsobem, který závísí na argumentu `interactive'.
          interactive -- pokud je pravdivý, dojde k ohlášení chyby vyskočivším
            dialogem s popisem chyby.  V opačném případě je pouze zobrazena
            zpráva ve stavové řádce.
          **kwargs -- klíčové argumenty, které mají být předány metodě
            'pytis.data.Type.validate()'.

        Vrací: Tuple (value, error), tak, jak ho vrátí
        'pytis.data.Type.validate()' příslušného datového typu pro hodnotu
        zadanou v políčku.

        """
        value, error = self._type.validate(self.get_value(), transaction=self._row.transaction(),
                                           **kwargs)
        if error and not quiet:
            if interactive:
                msg = _('Chyba validace políčka!\n\n%s: %s') % \
                      (self.spec().label(), error.message())
                run_dialog(Error, msg, title=_("Chyba validace"))
            else:
                message(error.message(), beep_=True)
        return value, error

    def _value(self, **kwargs):
        value, error = self.validate(quiet=True, **kwargs)
        return value
    
    def _is_valid(self, **kwargs):
        value, error = self.validate(quiet=True, **kwargs)
        return error is None

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

    def _set_disabled_color(self):
        if self._readonly:
            return
        elif self._denied:
            color = config.field_denied_color
        else:
            color = config.field_disabled_color 
        self._ctrl.SetOwnBackgroundColour(color)
        self._ctrl.Refresh()
            
    def set_focus(self):
        """Učiň toto políčko aktivním pro vstup z klávesnice."""
        self._want_focus = True

    def _set_focus(self):
        self._ctrl.SetFocus()

    def get_value(self):
        """Vrať hodnotu políčka jako string.
        
        Tuto metodu je třeba předefinovat v odvozené třídě.
        
        """
        raise ProgramError("This method must be overriden!")

    def set_value(self, value):
        """Nastav hodnotu políčka na 'value'.

        Argumenty:

          value -- hodnota políčka, string (pokud datový typ políčka nevyžaduje
            jinak)

        Vrací: Pravdu, jestliže hodnota byla úspěšně nastavena, nepravdu
        v opačném případě.

        Pokud je hodnota None, nebude provedeno nic, pouze vráceno False.

        Odvozené třídy nechť tuto metodu nepředefinovávájí, nechť předefinují
        metodu '_set_value()'.

        """
        # TODO: Is this really what we want?  Why do we ignore None values?
        # This requires a workaround in FileField, where None is a correct
        # value.
        if value is not None:
            return self._set_value(value)
        else:
            return False

    def _set_value(self, value):
        raise ProgramError("This method must be overriden!")

    def is_modified(self):
        """Vrať pravdu, právě pokud byla hodnota políčka změněna uživatelem.

        Políčko je nastaveno do počátečního stavu po každém volání metody
        'init()'. Metoda vrátí pravdu právě když je současná hodnota políčka
        rozdílná od hodnoty v počátečním stavu.
        
        """
        # Returns always false for virtual fields
        return self._row.field_changed(self.id()) or self.get_value() != self._row.format(self.id())

    def is_enabled(self):
        """Return true if the field is editable by the user.

        The field may be disabled for several reasons:
          * the field was created as read-only,
          * the user doesn't have sufficient permissions,
          * the field is not editable from definition (permanently or because of an editability
            condition, typically dependence on values of some other fields in the form).

        """
        return self._enabled
    
    def reset(self):
        """Nastav hodnotu políčka na původní hodnotu.

        Původní hodnotou je myšlena hodnota po posledním volání metody
        'init()'. Pokud motoda 'init()' nebyla doposud volána, je chování
        metody nespecifikováno.
        
        """
        self._set_value(self._row.original_row()[self.id()].export())

    def _alive(self):
        try:
            self._ctrl.GetId()
            return True
        except wx.PyDeadObjectError:
            return False   
        
    # Class methods

    def _defocus(cls, field):
        if cls._focused_field is field:
            cls._last_focused_field = cls._focused_field
            cls._focused_field = None

    def _focus(cls, field):
        #import weakref
        current = cls.focused()
        cls._focused_field = field #weakref.ref(field)
        if current is not None:
            cls._last_focused_field = current
    
    def _last_focused(cls):
        field = cls._last_focused_field
        cls._last_focused_field = None
        if field is not None and field._alive():
            return field
        return None

    def focused(cls):
        field = cls._focused_field
        if field is not None and field._alive():
            return field
        return None
         
    _focus   = classmethod(_focus)
    _defocus = classmethod(_defocus)
    _last_focused = classmethod(_last_focused)
    focused = classmethod(focused)
    
        
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
               lambda e: message(_("Překročena maximální délka."), beep_=True))
        filter = self._filter()
        control.SetValidator(_TextValidator(control, filter=filter))
        wx_callback(wx.EVT_TEXT, control, wxid, self._on_change)
        wx_callback(wx.EVT_TEXT_ENTER, control, wxid, self._on_enter_key)
        return control

    def _maxlen(self):
        """Vrať maximální délku zadaného textu."""
        return None

    def _on_enter_key(self, event):
        if self.height() > 1:
            event.Skip()
        else:
            event.GetEventObject().Navigate()

    def _on_change(self, event=None):
        post_process = self._post_process_func()
        if post_process:
            val = post_process(self.get_value())
            if val != self.get_value():
                self._set_value(val)
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

    def get_value(self):
        return self._ctrl.GetValue()

    def _enable(self):
        control = self._ctrl
        control.SetEditable(True)
        control.SetOwnBackgroundColour(None)
        control.SetValidator(_TextValidator(control, filter=self._filter()))

    def _disable(self):
        self._ctrl.SetEditable(False)
        self._ctrl.SetValidator(wx.DefaultValidator)
        # Pokud to udělám přímo, u některých políček se změna neprojeví!
        self._call_on_idle = self._set_disabled_color

    def _set_value(self, value):
        assert isinstance(value, types.StringTypes), \
               ('String or Unicode expected', value)
        self._ctrl.SetValue(value)
        return True

    def _menu(self):
        return super(TextField, self)._menu() + \
               ((None,),
                (TextField.COMMAND_CUT,
                 _("Vyjmout"),
                 _("Vyjmout označený text a uložit jej do schránky.")),
                (TextField.COMMAND_COPY,
                 _("Kopírovat"),
                 _("Zkopírovat označený text do schránky.")),
                (TextField.COMMAND_PASTE,
                 _("Vložit"),
                 _("Vložit text ze schránky do políčka.")),
                (TextField.COMMAND_SELECT_ALL,
                 _("Vybrat vše"),
                 _("Označit celou hodnotu.")))

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
        return bool(self.get_value())

    def _cmd_select_all(self):
        self._ctrl.SetSelection(-1, -1)
        

class StringField(TextField):
    """Textové vstupní políčko pro data typu 'pytis.data.String'."""

    def _maxlen(self):
        return self._type.maxlen()

    
class NumericField(TextField):
    """Textové vstupní políčko pro data typu 'pytis.data.Number'."""
    pass


class CheckBoxField(Unlabeled, InputField):
    """Vstupní pole pro typ Boolean realizované pomocí 'wx.CheckBox'."""

    def _create_ctrl(self):
        """Vrať instanci 'wx.CheckBox'."""
        if self._inline:
            label = ''
        else:
            label = self.spec().label()
        control = wx.CheckBox(self._parent, -1, label)
        wx_callback(wx.EVT_CHECKBOX, control, control.GetId(), self._on_change)
        return control
                    
    def get_value(self):
        """Vrať hodnotu políčka jako string.

        Je vrácen string 'T', je-li políčko zatrženo, string 'F' jinak.

        """
        return self._ctrl.GetValue() and 'T' or 'F'

    def _set_value(self, value):
        """Nastav hodnotu políčka na 'value'.

        Argumenty:

            value -- hodnota políčka, string 'T' (pravda) nebo 'F' (nepravda)
              nebo prázdný řetězec (nepravda)

        Vrací: Pravdu, jestliže hodnota byla úspěšně nastavena, nepravdu
        v opačném případě.

        """
        assert value in ('T','F',''), ('Invalid argument', value)
        wxvalue = value == 'T' and True or False
        self._ctrl.SetValue(wxvalue)
        # _on_change musíme volat ručně, protože SetValue() nevyvolá událost.
        self._on_change()
        return True


class EnumerationField(InputField):
    """Abstrakce vstupního pole pro výčtový typ.
    
    Tento typ vstupního pole je reprezentován pomocí výběru z pevně dané
    množiny hodnot.  Množinu hodnot určuje enumerátor datového typu (viz metoda
    'pytis.data.FixedEnumerator.values()').

    Tato třída není určena k přímému použití. Je to rodičivská třída pro
    vstupní pole nad výčtovým typem dat.
    
    """
    def _choices(self):
        return [x[1] for x in self._row.enumerate(self.id())]

    def get_value(self):
        i = self._ctrl.GetSelection()
        value = self._type.enumerator().values()[i]
        return self._type.export(value)

    def _set_value(self, value):
        assert isinstance(value, types.StringTypes), ('Invalid value', value)
        t = self._type
        values = [t.export(v) for v in t.enumerator().values()]
        try:
            i = values.index(value)
        except ValueError:
            i = wx.NOT_FOUND
        result = self._ctrl.SetSelection(i)
        # _on_change must be called here, because SetSelection() doesn't emit an event.
        self._on_change()
        return result


class ChoiceField(EnumerationField):
    """Vstupní pole pro výčtový typ reprezentované pomocí 'wx.Choice'."""

    def _create_ctrl(self):
        """Vrať instanci 'wx.Choice' podle specifikace."""
        control = wx.Choice(self._parent, choices=self._choices())
        wx_callback(wx.EVT_CHOICE, control, control.GetId(), self._on_change)
        return control

    
class RadioBoxField(Unlabeled, EnumerationField):
    """Vstupní pole pro výčtový typ reprezentované pomocí 'wx.RadioBox'.

    Interpretace specifikace:

      orientation -- tento specifikační atribut udává hlavní orientaci skládání
        jednotlivých prvků. Hodnotou je konstanta 'spec.Orientation'.
      width -- v případě horizontální orientace udává maximální počet sloupců
        prvků vedle sebe.
      height -- v případě vertikální orientace udává maximální počet řad
        prvků nad sebou.

    """

    _DEFAULT_WIDTH = 1

    def _create_ctrl(self):
        """Vrať instanci 'wx.RadioBox' podle specifikace."""
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
    """Vstupní pole pro výčtový typ reprezentované pomocí 'wx.ListBox'."""

    def _create_ctrl(self):
        """Vrať instanci 'wx.ListBox' podle specifikace."""
        control = wx.ListBox(self._parent, choices=self._choices(),
                             style=wx.LB_SINGLE|wx.LB_NEEDED_SB)
        wx_callback(wx.EVT_LISTBOX, control, control.GetId(), self._on_change)
        return control
    

class Invocable(object, CommandHandler):
    """Mix-in třída pro políčka s možností vyvolání výběru.

    Abstraktní třída pro políčka, která umožňují vyvolat pro výběr hodnoty
    nějakou akci (většinou v podobě modálního popup okna).

    Vstupní políčko (vytvořené metodou '_create_widget()' základní třídy) bude
    doplněno o tlačítko pro vyvolání výběru.

    Výběr lze vyvolat také klávesou příkazu
    'Invocable.COMMAND_INVOKE_SELECTION'.

    """
    _INVOKE_TITLE = _("Vybrat hodnotu")
    _INVOKE_HELP = None
    _INVOKE_ICON = 'invoke-selection'
    
    def _get_command_handler_instance(cls):
        return InputField._get_command_handler_instance()
    _get_command_handler_instance = classmethod(_get_command_handler_instance)
    
    def _create_widget(self):
        """Zavolej '_create_widget()' odvozené třídy a přidej tlačítko.

        Více informací viz. dokumentace třídy 'Invocable'.
        
        """
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
        return self.is_enabled()

    
class DateField(Invocable, TextField):
    """Vstupní pole pro datový typ 'pytis.data.Date'.

    Jako akci pro vyvolání výběru definuje zobrazení dialogu s kalendářem,
    který je nastaven na datum odpovídající hodnotě políčka a po ukončení
    nastaví hodnotu políčka na vybraný datum.

    """

    _DEFAULT_WIDTH = 10
    _INVOKE_TITLE = _("Vybrat z kalendáře")
    _INVOKE_HELP = _("Zobrazit kalendář pro výběr datumu.")
    
    def _on_invoke_selection(self, alternate=False):
        value = self._value()
        if value is not None:
            d = value.value()
        else:
            d = None
        date = run_dialog(Calendar, d)
        if date != None:
            self.set_value(self._type.export(date))


class ColorSelectionField(Invocable, TextField):
    """Vstupní pole pro výběr barvy."""

    _DEFAULT_WIDTH = 7
    _INVOKE_TITLE = _("Vybrat barvu")
    _INVOKE_HELP = _("Zobrazit dialog pro výběr barev.")
    
    def _on_invoke_selection(self, alternate=False):
        color = run_dialog(ColorSelector, self.get_value())
        if color != None:
            self.set_value(color)

    def _create_button(self, label, **kwargs):
        size = self._button_size()
        return wx.lib.colourselect.ColourSelect(self._parent, -1, size=size)
    
    def _set_value(self, value):
        self._invocation_button.SetColour(value)
        return super(ColorSelectionField, self)._set_value(value)

    
class GenericCodebookField(InputField):
    """Společná nadtřída číselníkových políček."""

    def _init_attributes(self):
        cb_name = self._spec.codebook(self._row.data())
        assert cb_name is not None
        try:
            cb_spec = resolver().get(cb_name, 'cb_spec')
        except ResolverError:
            cb_spec = CodebookSpec()
        self._cb_name = cb_name
        self._cb_spec = cb_spec
        self._type.enumerator().add_hook_on_update(self._on_enumerator_change)
        super(GenericCodebookField, self)._init_attributes()

    def _select_row_arg(self):
        """Return the value for RecordForm 'select_row' arguemnt."""
        value = self._value()
        if value and value.value():
            return {self._type.enumerator().value_column(): value}
        else:
            return None
    
    def _run_codebook_form(self, begin_search=None):
        """Zobraz číselník a po jeho skončení nastav hodnotu políčka."""
        enumerator = self._type.enumerator()
        result = run_form(CodebookForm, self._cb_name, begin_search=begin_search,
                          select_row=self._select_row_arg(), transaction=self._row.transaction(),
                          condition=enumerator.validity_condition())
        if result: # may be None or False!
            self.set_value(result.format(enumerator.value_column()))
        self.set_focus()

    def _on_enumerator_change(self):
        pass

    def _cmd_invoke_codebook_form(self):
        self._run_codebook_form()
    

class CodebookField(Invocable, GenericCodebookField, TextField):
    """Vstupní pole pro data navázaná na číselník.

    Bude použito v případě, že datový typ definuje enumerátor typu
    'pytis.data.DataEnumerator' a prezentační specifikace políčka definuje
    navázaný číselník (viz. argument 'codebook' konstruktoru 'FieldSpec').

    Jako akci pro vyvolání výběru definuje zobrazení formuláře
    'pytis.form.CodebookForm'.  Název specifikace číselníku je dán výše
    zmíněným specifikátorem 'codebook'.  Další vlastnosti číselníkového
    formuláře jsou dány jednak specifikací 'cb_spec' v odkazované specifikaci a
    jednak přímo specifikací 'view_spec' tamtéž.

    K políčku může být volitelně přidružen displej, který slouží k zobrazení
    popisu vybrané (aktuální) hodnoty číselníku. 

    """
    _INVOKE_TITLE = _("Vybrat z číselníku")
    _INVOKE_HELP = _("Zobrazit číselník hodnot s možností výběru.")

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
            button.SetToolTipString(_("Vložit nový záznam do číselníku"))
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
                 _("Vyhledávat v číselníku"),
                 _("Zobrazit číselník se zapnutým inkrementálním "
                   "vyhledáváním.")),)

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
            display = self._value() and self._row.display(self.id()) or ''
            self._display.SetValue(display)
        
    def _on_invoke_selection(self, alternate=False):
        value_column = self._type.enumerator().value_column()
        if not self._is_valid() and self.get_value() and self.is_modified() \
               and isinstance(self.type(), pytis.data.String):
            begin_search = (value_column, self.get_value())
        elif alternate:
            begin_search = value_column
        else:
            begin_search = self._cb_spec.begin_search()
        self._run_codebook_form(begin_search=begin_search)

    def _on_codebook_insert(self, event):
        value_column = self._type.enumerator().value_column()
        if not self._is_valid() and self.is_modified():
            prefill = {value_column: self.get_value()}
        else:
            prefill = {}
        spec = self.spec().codebook_insert_spec() or self._cb_name
        result = new_record(spec, prefill=prefill, transaction=self._row.transaction())
        if result and result.has_key(value_column):
            self.set_value(result[value_column].export())
        return True
    
    
class ListField(GenericCodebookField):
    """Číselníkové políčko zobrazující data číselníku jako součást formuláře.

    Pokud je 'selection_type' číselníkového políčka ve specifikaci určen jako 'LIST', bude ve
    formuláři použit tento typ vstupního pole.

    """
    _DEFAULT_WIDTH = 30
    _DEFAULT_HEIGHT = 6

    def _create_ctrl(self):
        # Načtu specifikace.
        view_spec = resolver().get(self._cb_name, 'view_spec')
        self._columns = columns = self._cb_spec.columns() or view_spec.columns()
        # Vytvořím vlastní seznamový widget.
        style=wx.LC_REPORT|wx.SUNKEN_BORDER|wx.LC_SINGLE_SEL
        list = wx.ListCtrl(self._parent, -1, style=style)
        # Nastavím záhlaví sloupců.
        total_width = 0
        for i, id in enumerate(columns):
            col = view_spec.field(id)
            list.InsertColumn(i, col.label())
            width = col.column_width()
            if width < len(col.label()):
                width = len(col.label())
            list.SetColumnWidth(i, dlg2px(list, 4*(width+1)))
            total_width = total_width + width
        # TODO/wx: Nějak spočítat skutečnou výšku záhlaví a řádku.
        # Tohle jsou "empirické" vzorce!!!
        header_height = char2px(list, 1, float(9)/4).GetHeight()
        row_height = char2px(list, 1, float(10)/7).GetHeight()
        height = header_height + row_height * self.height()
        self._DEFAULT_WIDTH = total_width + 3
        list.SetMinSize((dlg2px(list, 4*(self.width()+1)), height))
        self._list =  list
        self._data_dirty = True
        wxid = list.GetId()
        wx_callback(wx.EVT_LIST_ITEM_SELECTED, list, wxid, self._on_select)
        wx_callback(wx.EVT_LIST_ITEM_ACTIVATED, list, wxid, self._on_activation)
        wx_callback(wx.EVT_MOUSEWHEEL, list, lambda e: e.Skip())
        self._selected_item = None
        return list

    def _on_select(self, event):
        self._list.SetItemState(event.GetIndex(), 0, wx.LIST_STATE_SELECTED)

    def _on_activation(self, event):
        event.Skip()
        i = event.GetIndex()
        if self._enabled and i != self._selected_item:
            self._set_selection(i)
            self._on_change()
            
    def _on_enumerator_change(self):
        # Callback může být volán i když už je list mrtev.
        self._data_dirty = True

    def _on_idle(self, event):
        if self._data_dirty:
            self._load_list_data()
        return super(ListField, self)._on_idle(event)

    #def _on_kill_focus(self, event):
    #    if self._selected_item is not None:
    #        self._list.EnsureVisible(self._selected_item)
    #    return super(ListField, self)._on_kill_focus(event)
        
    def _load_list_data(self):
        current = self.get_value()
        list = self._list
        enumerator = self.type().enumerator()
        list.DeleteAllItems()
        self._list_data = []
        select_item = None
        for i, row in enumerate(enumerator.iter()):
            list.InsertStringItem(i, "")
            v = row[enumerator.value_column()]
            self._list_data.append(v)
            if v.export() == current:
                select_item = i
            for j, id in enumerate(self._columns):
                list.SetStringItem(i, j, row[id].export().replace("\n", ";"))
        self._set_selection(select_item)
        self._data_dirty = False

    def _disable(self):
        self._set_disabled_color()
        
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
        

    def _set_value(self, value):
        if self._data_dirty:
            self._load_list_data()
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
        
    def get_value(self):
        """Vrať aktuální vnitřní hodnotu políčka."""
        i = self._selected_item
        if i is not None:
            return self._list_data[i].export()
        else:
            return None

    def _menu(self):
        return ((self.COMMAND_SELECT,
                 _("Vybrat"),
                 _("Zvolit tuto položku jako aktivní.")),
                (self.COMMAND_SHOW_SELECTED,
                 _("Najít vybranou položku"),
                 _("Nalistovat v seznamu vybranou položku.")),
                (None,),
                (self.COMMAND_INVOKE_CODEBOOK_FORM,
                 _("Zobrazit číselník"),
                 _("Otevřít odpovídající číselníkový formulář.")),
                (self.COMMAND_INVOKE_EDIT_FORM,
                 _("Editovat vybraný záznam"),
                 _("Otevřít vybraný záznam v editačním formuláři.")),
                (Application.COMMAND_RUN_FORM(form_class=BrowseForm,
                                              name=self._cb_name,
                                              select_row=self._select_row_arg()),
                 _("Zobrazit celou tabulku"),
                 _("Otevřít náhled číselníku v řádkovém formuláři.")),
                )

    # Zpracování příkazů
    
    def on_command(self, command, **kwargs):
        if command == self.COMMAND_SELECT:
            i = self._list.GetNextItem(-1, state=wx.LIST_STATE_FOCUSED)
            self._set_selection(i)
        elif command == self.COMMAND_SHOW_SELECTED:
            self._set_selection(self._selected_item)
        elif command == self.COMMAND_INVOKE_EDIT_FORM:
            run_form(PopupEditForm, self._cb_name,
                     select_row=self._select_row_arg())
        else:            
            return super(ListField, self).on_command(command, **kwargs)
        return True

    def _can_select(self):
        return self.is_enabled()
    
    def _can_invoke_edit_form(self, **kwargs):
        return self._selected_item is not None


class FileField(Invocable, InputField):
    """Input field for manipulating generic binary data."""
    
    _INVOKE_TITLE = _("Vybrat soubor")
    _INVOKE_HELP = _("Zobrazit dialog pro procházení systému souborů.")
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
    
    def get_value(self):
        return self._buffer and self._buffer.buffer()

    def set_value(self, value):
        # This is a workargound. None values are ignored in the parent method!
        return self._set_value(value)
        
    def _set_value(self, value):
        assert value is None or isinstance(value, buffer)
        self._buffer = value and self._type.Buffer(value) or None
        self._on_set_value()
        self._on_change()
        return True

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
                 _("Nahradit hodnotu políčka daty ze souboru. ")),
                (FileField.COMMAND_SAVE,
                 _("Uložit do souboru"),
                 _("Uložit objekt z databáze jako soubor.")),
                (FileField.COMMAND_CLEAR,
                 _("Vynulovat"),
                 _("Nastavit prázdnou hodnotu.")),
                )

    def _can_load(self):
        return self._enabled
        
    def _cmd_load(self):
        msg = _("Vyberte soubor pro políčko '%s'") % self.spec().label()
        dir = FileField._last_load_dir or FileField._last_save_dir or ''
        dlg = wx.FileDialog(self._parent, message=msg, style=wx.OPEN,
                            defaultDir=dir)
        if dlg.ShowModal() == wx.ID_OK:
            path = dlg.GetPath()
            FileField._last_load_dir = os.path.dirname(path)
            try:
                if self._buffer:
                    self._buffer.load(path)
                else:
                    self._buffer = self._type.Buffer(path)
            except pytis.data.ValidationError, e:
                message(e.message(), beep_=True)
            except IOError, e:
                message(_("Chyba při čtení souboru:")+' '+str(e), beep_=True)
            else:
                self._on_set_value()
                self._on_change()
                message(_("Soubor načten."))
        
    def _can_save(self):
        return self._buffer is not None 
        
    def _cmd_save(self):
        msg = _("Uložit hodnotu políčka '%s'") % self.spec().label()
        dir = FileField._last_save_dir or FileField._last_load_dir or ''
        dlg = wx.FileDialog(self._parent, style=wx.SAVE, message=msg,
                            defaultDir=dir)
        if dlg.ShowModal() == wx.ID_OK:
            path = dlg.GetPath()
            FileField._last_save_dir = os.path.dirname(path)
            try:
                self._buffer.save(path)
            except IOError, e:
                message(_("Chyba při zápisu souboru:")+' '+str(e), beep_=True)
            else:
                message(_("Soubor uložen."))
        
    def _can_clear(self):
        return self._enabled and self._buffer is not None
        
    def _cmd_clear(self):
        self.set_value(None)


class ImageField(FileField):
    """Input field for bitmap images showing a thumbnail within the control."""

    _DEFAULT_WIDTH = _DEFAULT_HEIGHT = 80
    
    def _create_ctrl(self):
        return wx_button(self._parent, bitmap=self._bitmap(),
                         size=(self.width()+10, self.height()+10),
                         callback=lambda e: self._on_button())

    def _disable(self):
        super(ImageField, self)._disable(self)
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
