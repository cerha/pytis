# -*- coding: iso-8859-2 -*-

# Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006 Brailcom, o.p.s.
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

"""Abstrakce vstupních políček pro použití ve formulářích.

Každé vstupní políčko má nápis (label) a vlastní UI přípravek obsluhující vstup
a hodnoty (widget).

Třída, která bude pro které vstupní políčko použita je dána datovým typem
hodnoty, pro kterou je políčko vytvářeno - ten je zjištěn z datového objektu.
Vytvoření patřičné třídy.

"""

import pytis.data
from pytis.form import *
import wx.lib.colourselect
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
    
    def create(cls, parent, spec, data, guardian=None, inline=False,
               accessible=True):
        """Vrať instanci políčka odpovídajícího typu.
        
        Argumewnty jsou totožné, jako u metody 'InputField.__init__()'.
        
        """
        type = spec.type(data)
        codebook = spec.codebook(data)
        if isinstance(type, pytis.data.Date):
            field = DateField
        elif isinstance(type, pytis.data.Boolean):
            field = CheckBoxField
        elif isinstance(type, pytis.data.Color):
            field = ColorSelectionField
        elif isinstance(type, (pytis.data.Number, pytis.data.String)) \
                 and type.enumerator() is not None and codebook is not None \
                 and not isinstance(spec.computer(), CbComputer):
            if inline:
                if codebook:
                    field = CodebookField
                else:
                    field = ChoiceField 
            else:
                selection_type = spec.selection_type()
                if selection_type is None:
                    if codebook is not None:
                        selection_type = SelectionType.CODEBOOK
                    else:
                        selection_type = SelectionType.CHOICE
                #cbtypes = (SelectionType.CODEBOOK, SelectionType.LIST)
                #assert selection_type not in cbtypes or codebook is not None
                mapping = {
                    SelectionType.CODEBOOK:  CodebookField,
                    SelectionType.LIST:      ListField,
                    SelectionType.CHOICE:    ChoiceField,
                    SelectionType.LIST_BOX:  ListBoxField,
                    SelectionType.RADIO_BOX: RadioBoxField,
                    }
                field = mapping[selection_type]
        elif isinstance(type, pytis.data.String):
            field = StringField
        elif isinstance(type, pytis.data.Number):
            field = NumericField
        else:
            field = TextField
        return field(parent, spec, data, guardian=guardian, inline=inline,
                     accessible=accessible)

    create = classmethod(create)

    def __init__(self, parent, spec, data, guardian=None, inline=False,
                 accessible=True):
        """Vytvoř vstupní políčko, podle specifikace.

        Argumenty:

          parent -- instance 'wx.Window', která má být používána jako wx rodič
            všech vytvářených wx prvků
            
          spec -- specifikace prezentačních vlastností, instance třídy
            'spec.FieldSpec'
            
          data -- datový objekt, instance třídy 'pytis.data.Data'

          guardian -- nadřazený 'KeyHandler'.
          
          inline -- pokud je pravda, bude vytvořen pouze vlastní vstupní
            prvek.  Label a veškeré blbinky kolem budou vynechány.  To je
            vhodné při použití políčka pro in-line editaci v řádkovém
            formuláři.
            
          accessible -- pravda, pokud má uživatel mít právo editace políčka.
            Takto znepřístupněné políčko již nelze zpřístupnit a vzhled je
            jiný, než v případě políčka zakázaného voláním metody 'disable()'.

        Metodu '__init__()' nechť odvozené třídy nepředefinovávají. Nechť
        předefinovávají metody '_create_widget()' a '_create_label'.

        """
        assert self != None
        #assert isinstance(parent, wx.Window)
        assert isinstance(guardian, KeyHandler)
        assert isinstance(spec, FieldSpec)
        CallbackHandler.__init__(self)
        self._parent = parent
        self._type = spec.type(data)
        self._spec = spec
        self._data = data
        self._guardian = guardian
        self._id = id = spec.id()
        self._inline = inline
        self._initial_value = None
        self._want_focus = False
        self._is_changed = False
        self._initialized = False
        self._accessible = self._enabled = accessible
        self._ctrl = self._create_ctrl()
        self._callback_registered = False
        self._unregistered_widgets = {}
        if inline:
            self._widget = self._ctrl
        else:
            self._label = self._create_label()
            self._widget = self._create_widget()
        self._init_ctrl()
        if not self._enabled:
            self._disable(True)
            self._register_skip_navigation_callback()

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
    
    def _init_ctrl(self):
        c = self._ctrl
        KeyHandler.__init__(self, c)
        wx_callback(wx.EVT_IDLE,       c, self._on_idle)
        wx_callback(wx.EVT_KILL_FOCUS, c, self._on_kill_focus)
        wx_callback(wx.EVT_SET_FOCUS,  c, self._on_set_focus)
        wx_callback(wx.EVT_RIGHT_DOWN, c, self._on_context_menu)
        if self._spec.descr() is not None and config.show_tooltips:
            c.SetToolTipString(self._spec.descr())
            
    def _create_label(self):
        # Return field label as 'wx.StaticText' instance.
        label = self.spec().label()
        if label:
            label = label + ':'            
        return wx.StaticText(self._parent, -1, label,
                             style=wx.ALIGN_RIGHT)

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
            # Pokud je hodnota validní, dej o změně vědět formuláři.
            self._run_callback(self.CALL_FIELD_CHANGE, self)
            self._is_changed = False
        if self._want_focus and not self.has_focus():
            self._set_focus()
        if hasattr(self, '_call_on_idle') and self._call_on_idle is not None:
            self._call_on_idle()
            self._call_on_idle = None
        event.Skip()
        return True
    
    def _on_set_focus(self, event):
        self._want_focus = False
        last = InputField._last_focused()
        # TODO: Zkusit to přes `wx.Window.SetFocusFromKbd()'
        if last is not None and last is not self and last.enabled() \
               and last.is_modified():
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
        # Výše uvedený Disconnect nefunguje, takže si to ubastlíme po svém...
        self._unregistered_widgets[self._ctrl] = True

    def _on_change(self, event=None):
        """Event handler volaný při jakékoliv změně hodnoty políčka."""
        # Toto je hack aby bylo možné vytvářet zakázaná políčka, která nejsou
        # zašedivěna a vypadají tedy stejně jako editovatelná, ale nelze je
        # změnit.  Zde tedy po každém pokusu o změnu vrátíme původní hodnotu a
        # hotovo.  V jiných případech by nemělo dojít k tomu, že políčko, které
        # je `self._enabled' změní hodnotu, takže by to snad nemuselo ničemu
        # vadit...
        #if self._enabled:  Dělá to problémy s dopočítávanými políčky, takže
        # je to zatím vyřazeno.  Nejlepší by bylo to vymyslet úplně jinak...
        self._disable_event_handlers()
        self._post_process()
        self._is_changed = True
        if event:
            event.Skip()
        self._enable_event_handlers()
        #else:
        #    self.reset()
        #return True

    def _post_process(self):
        """Aplikuj postprocessing.
        
        Voláno po každé změně hodnoty políčka.
        
        """
        pass
    
    def has_focus(self):
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
        value, error = self._type.validate(self.get_value(), **kwargs)
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
    
    def enabled(self):
        """Vrať pravdu, pokud je políčko editovatelné."""
        return self._enabled
    
    def enable(self):
        """Povol uživatelský vstup do políčka."""
        if self._accessible:
            self._enabled = True
            self._enable()
            self._unregister_skip_navigation_callback()
    
    def _enable(self):
        self._ctrl.Enable(True)

    def disable(self, change_appearance=True):
        """Zakaž uživatelský vstup do políčka.

        Voláním této metody se políčko stane read-only.  Nebude tedy aktivní na
        uživatelský vstup.  Pokud není nastaven argument `change_appearance' na
        nepravdivou hodnotu, bude také změněn vzhled políčka (zašedivění).

        Odvozené třídy nechť tuto metodu nepředefinovávájí, nechť raději
        předefinují metodu '_disable()'.

        """
        if self._accessible:
            self._enabled = False
            self._disable(change_appearance)
            self._register_skip_navigation_callback()

    def _disable(self, change_appearance):
        if change_appearance:
            self._ctrl.Enable(False)
        else:
            # Here we rely on a simple hack in InputField._on_change() which
            # resets field value after each attempt to chnge it, so the field
            # is in fact editable, but it is not possible to change it
            # effectively.
            pass

    def _set_disabled_color(self):
        if self._accessible:
            color = config.field_disabled_color 
        else:
            color = config.field_inaccessible_color
        self._ctrl.SetBackgroundColour(color)
        self._ctrl.Refresh()
            
    def set_focus(self):
        """Učiň toto políčko aktivním pro vstup z klávesnice."""
        self._want_focus = True

    def _set_focus(self):
        self._ctrl.SetFocus()

    def initialized(self):
        """Vrať pravdu právě když políčko již bylo inicializováno.""" 
        return self._initialized
        

    def init(self, value):
        """Nastav počáteční hodnotu políčka na 'value'. 

        Argumenty:

          value -- počáteční hodnota políčka (stejná, jako pro metodu
            'set_value()').

        Takto nastavenou počáteční hodnotu lze kdykoliv vrátit metodou
        'reset()'.  Metodou 'is_modified()' lze potom zjistit, zda se současná
        hodnota políčka liší od této počáteční hodnoty.

        """
        self._disable_event_handlers()
        self.set_value(value)
        self._initial_value = self.get_value()
        self._is_changed = True
        self._enable_event_handlers()
        self._initialized = True

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
        if __debug__:
            log(DEBUG, 'Nastavení hodnoty políčka:', (self.id(), value))
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
        return self._initial_value != self.get_value()

    def is_enabled(self):
        return self._enabled
    
    def reset(self):
        """Nastav hodnotu políčka na původní hodnotu.

        Původní hodnotou je myšlena hodnota po posledním volání metody
        'init()'. Pokud motoda 'init()' nebyla doposud volána, je chování
        metody nespecifikováno.
        
        """
        if __debug__: log(DEBUG, 'Reset hodnoty políčka', self.id())
        self._set_value(self._initial_value)

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
        control = wx.TextCtrl(self._parent, -1, '', style=style)
        wxid = control.GetId()
        if not self._inline:
            width, height = self.width(), self.height()
            size = dlg2px(control, 4*(width+1)+2, 8*height+4.5)
            control.SetMinSize(size)
            control.SetSize(size)
        maxlen = self._maxlen()
        if maxlen is not None:
            control.SetMaxLength(maxlen)
            wx_callback(wx.EVT_TEXT_MAXLEN, control, wxid, self._on_maxlen)
        filter = self._filter()
        control.SetValidator(_TextValidator(control, filter=filter))
        wx_callback(wx.EVT_TEXT, control, wxid, self._on_change)
        wx_callback(wx.EVT_TEXT_ENTER, control, wxid, self._on_enter_key)
        return control

    def _maxlen(self):
        """Vrať maximální délku zadaného textu."""
        return None

    def _on_maxlen(self, event):
        # User tried to enter more text into the control than the limit
        beep()
        message(_("Překročena maximální délka."))
    
    def _on_enter_key(self, event):
        if self.height() > 1:
            event.Skip()
        else:
            event.GetEventObject().Navigate()

    def _post_process_func(self):
        """Vrať funkci odpovídající specifikaci postprocessingu políčka.

        Vrací: Funkci je funkcí jednoho argumentu (původní text), která vrací
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
        control.SetBackgroundColour(wx.WHITE)
        control.SetValidator(_TextValidator(control, filter=self._filter()))

    def _post_process(self):
        f = self._post_process_func()
        if f:
            oldval = self.get_value()
            args = (oldval,)
            val = f(*args)
            if val != oldval:
                self._set_value(val)

    def _disable(self, change_appearance):
        self._ctrl.SetEditable(False)
        self._ctrl.SetValidator(wx.DefaultValidator)
        if change_appearance:
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
        # Return a sequence of string representations of all type's values.
        # We don't have access to the PresentedRow here, so we create a fake
        # one just for this field to be able to use its `enumerate' method.
        r = PresentedRow((self._spec,), self._data, None)
        return [x[1] for x in r.enumerate(self.id())]

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
        # _on_change must be called here, because SetSelection() doesn't emit
        # an event.
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
    _INVOKE_SELECTION_MENU_TITLE = _("Vybrat hodnotu")
    _INVOKE_SELECTION_MENU_HELP = None
    
    def _get_command_handler_instance(cls):
        return InputField._get_command_handler_instance()
    _get_command_handler_instance = classmethod(_get_command_handler_instance)
    
    def _call_next_method(self, name, *args, **kwargs):
        # Will not work in derived classes!
        for base in self.__class__.__bases__:
            if hasattr(base, name) and base != Invocable:
                method = getattr(base, name)
                return method(self, *args, **kwargs)
        else:
            raise ProgramError(repr(self) + " has no next method '%s'" % name)
    
    def _create_widget(self):
        """Zavolej '_create_widget()' odvozené třídy a přidej tlačítko.

        Více informací viz. dokumentace třídy 'Invocable'.
        
        """
        widget = self._call_next_method('_create_widget')
        if self._inline:
            return widget
        height = self._ctrl.GetSize().GetHeight()
        self._invocation_button = button = self._create_button(height)
        button.SetToolTipString(self._INVOKE_SELECTION_MENU_TITLE)
        sizer = wx.BoxSizer()
        sizer.Add(widget, 0, wx.FIXED_MINSIZE)
        sizer.Add(button, 0, wx.FIXED_MINSIZE)
        wx_callback(wx.EVT_BUTTON, button, button.GetId(),
                    lambda e: self._on_invoke_selection())
        wx_callback(wx.EVT_NAVIGATION_KEY, button,
                    self._skip_navigation_callback(button))
        return sizer

    def _create_button(self, height):
        button = wx.Button(self._parent, -1, "...")
        button.SetSize((dlg2px(button, 12), height))
        return button

    def _disable(self, change_appearance):
        self._invocation_button.Enable(False)
        self._call_next_method('_disable', change_appearance)
    
    def _enable(self):
        self._invocation_button.Enable(True)
        self._call_next_method('_enable')
    
    def _menu(self):
        return TextField._menu(self) + \
               ((None,),
                (self.COMMAND_INVOKE_SELECTION,
                 self._INVOKE_SELECTION_MENU_TITLE,
                 self._INVOKE_SELECTION_MENU_HELP))
    
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
    _INVOKE_SELECTION_MENU_TITLE = _("Vybrat z kalendáře")
    _INVOKE_SELECTION_MENU_HELP = _("Zobrazit kalendář pro výběr datumu.")
    
    def _on_invoke_selection(self, alternate=False):
        value = self._value()
        if value is not None:
            d = value.value()
        else:
            d = None
        date = run_dialog(Calendar, d)
        if date != None:
            self.set_value(self._type.export(date))
        return True


class ColorSelectionField(Invocable, TextField):
    """Vstupní pole pro výběr barvy."""

    _DEFAULT_WIDTH = 7
    _INVOKE_SELECTION_MENU_TITLE = _("Vybrat barvu")
    _INVOKE_SELECTION_MENU_HELP = _("Zobrazit dialog pro výběr barev.")
    
    def _on_invoke_selection(self, alternate=False):
        color = run_dialog(ColorSelector, self.get_value())
        if color != None:
            self.set_value(color)
        return True

    def _create_button(self, height):
        button = wx.lib.colourselect.ColourSelect(self._parent, -1,
                                                  size=(height, height))
        return button
    def _set_value(self, value):
        self._invocation_button.SetColour(value)
        return super(ColorSelectionField, self)._set_value(value)

    
class GenericCodebookField(InputField):
    """Společná nadtřída číselníkových políček."""

    def __init__(self, parent, spec, data, *args, **kwargs):
        self._codebook_name = codebook = spec.codebook(data)
        assert codebook is not None
        try:
            self._cb_spec = resolver().get(codebook, 'cb_spec')
        except ResolverError:
            self._cb_spec = CodebookSpec()
        super(GenericCodebookField, self).__init__(parent, spec, data, *args,
                                                   **kwargs)
        self._type.enumerator().add_hook_on_update(self._on_enumerator_change)

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
        result = run_form(CodebookForm, self._codebook_name,
                          begin_search=begin_search,
                          select_row=self._select_row_arg(),
                          condition=enumerator.validity_condition())
        if result != None:
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
    _INVOKE_SELECTION_MENU_TITLE = _("Vybrat z číselníku")
    _INVOKE_SELECTION_MENU_HELP = _("Zobrazit číselník přípustných hodnot "
                                    "s možností výběru.")

    def _create_widget(self):
        """Zavolej '_create_widget()' třídy Invocable a přidej displej."""
        widget = Invocable._create_widget(self)
        self._insert_button = None
        spec = self.spec()
        cb_spec = self._cb_spec
        self._display = None
        if self._inline or cb_spec.display() is None and \
               not spec.allow_codebook_insert():
            return widget
        sizer = wx.BoxSizer()
        sizer.Add(widget, 0, wx.FIXED_MINSIZE)
        height = self._ctrl.GetSize().GetHeight()
        if cb_spec.display():
            display_size = spec.display_size()
            if display_size is None:
                display_size = cb_spec.display_size()
            if display_size:
                display = wx.TextCtrl(self._parent, style=wx.TE_READONLY)
                size = char2px(display, display_size, 1)
                size.SetHeight(height)
                display.SetSize(size)
                display.SetBackgroundColour(wx.Colour(213, 213, 213))
                self._display = display
                wx_callback(wx.EVT_NAVIGATION_KEY, display,
                            self._skip_navigation_callback(display))
                sizer.Add(display, 0, wx.FIXED_MINSIZE)
        if spec.allow_codebook_insert():
            self._insert_button = button = wx.Button(self._parent, -1, "+")
            button.SetSize((dlg2px(button, 10), height))
            button.SetToolTipString(_("Vložit nový záznam do číselníku"))
            wx_callback(wx.EVT_BUTTON, button, button.GetId(),
                        self._on_codebook_insert)
            wx_callback(wx.EVT_NAVIGATION_KEY, button,
                        self._skip_navigation_callback(button))
            sizer.Add(button, 0, wx.FIXED_MINSIZE)
        return sizer

    def _menu(self):
        return Invocable._menu(self) + \
               ((self.COMMAND_INVOKE_SELECTION(alternate=True),
                 _("Vyhledávat v číselníku"),
                 _("Zobrazit číselník se zapnutým inkrementálním "
                   "vyhledáváním.")),)

    def _maxlen(self):
        try:
            return self._type.maxlen()
        except AttributeError:
            return None

    def _disable(self, change_appearance):
        if self._insert_button:
            self._insert_button.Enable(False)
        super_(CodebookField)._disable(self, change_appearance)        
    
    def _enable(self):
        if self._insert_button:
            self._insert_button.Enable(True)
        super_(CodebookField)._enable(self)        

    def set_display(self, value):
        if self._display:
            self._display.SetValue(value)

    def _on_invoke_selection(self, alternate=False):
        value_column = self._type.enumerator().value_column()
        if self._value() is None and self.get_value() \
               and isinstance(self.type(), pytis.data.String):
            begin_search = (value_column, self.get_value())
        elif alternate:
            begin_search = value_column
        else:
            begin_search = self._cb_spec.begin_search()
        self._run_codebook_form(begin_search=begin_search)
        return True

    def _on_codebook_insert(self, event):
        value_column = self._type.enumerator().value_column()
        if not self._is_valid() and self.is_modified():
            prefill = {value_column: self.get_value()}
        else:
            prefill = {}
        spec = self.spec().codebook_insert_spec() or self._codebook_name
        result = run_form(PopupEditForm, spec, prefill=prefill)
        if result and result.has_key(value_column):
            self.set_value(result[value_column].export())
        return True
    
    
class ListField(GenericCodebookField):
    """Číselníkové políčko zobrazující data číselníku jako součást formuláře.

    Pokud je 'selection_type' číselníkového políčka ve specifikaci určen jako
    'LIST_FIELD', bude ve formuláři použit tento typ vstupního pole.

    """
    _DEFAULT_WIDTH = 30
    _DEFAULT_HEIGHT = 6

    def _create_ctrl(self):
        # Načtu specifikace.
        view_spec = resolver().get(self._codebook_name, 'view_spec')
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
            self._is_changed = True
            
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
    #    super(ListField, self)._on_kill_focus(event)
        
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

    def _disable(self, change_appearance):
        if change_appearance:
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
                                            name=self._codebook_name,
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
            run_form(PopupEditForm, self._codebook_name,
                     select_row=self._select_row_arg())
        else:            
            return super(ListField, self).on_command(command, **kwargs)
        return True

    def _can_select(self):
        return self.is_enabled()
    
    def _can_invoke_edit_form(self, **kwargs):
        return self._selected_item is not None


    
