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

"""Abstrakce vstupn�ch pol��ek pro pou�it� ve formul���ch.

Ka�d� vstupn� pol��ko m� n�pis (label) a vlastn� UI p��pravek obsluhuj�c� vstup
a hodnoty (widget).

T��da, kter� bude pro kter� vstupn� pol��ko pou�ita je d�na datov�m typem
hodnoty, pro kterou je pol��ko vytv��eno - ten je zji�t�n z datov�ho objektu.
Vytvo�en� pat�i�n� t��dy.

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
    
    def create(cls, parent, spec, data, guardian=None, inline=False,
               accessible=True):
        """Vra� instanci pol��ka odpov�daj�c�ho typu.
        
        Argumewnty jsou toto�n�, jako u metody 'InputField.__init__()'.
        
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
        """Vytvo� vstupn� pol��ko, podle specifikace.

        Argumenty:

          parent -- instance 'wx.Window', kter� m� b�t pou��v�na jako wx rodi�
            v�ech vytv��en�ch wx prvk�
            
          spec -- specifikace prezenta�n�ch vlastnost�, instance t��dy
            'spec.FieldSpec'
            
          data -- datov� objekt, instance t��dy 'pytis.data.Data'

          guardian -- nad�azen� 'KeyHandler'.
          
          inline -- pokud je pravda, bude vytvo�en pouze vlastn� vstupn�
            prvek.  Label a ve�ker� blbinky kolem budou vynech�ny.  To je
            vhodn� p�i pou�it� pol��ka pro in-line editaci v ��dkov�m
            formul��i.
            
          accessible -- pravda, pokud m� u�ivatel m�t pr�vo editace pol��ka.
            Takto znep��stupn�n� pol��ko ji� nelze zp��stupnit a vzhled je
            jin�, ne� v p��pad� pol��ka zak�zan�ho vol�n�m metody 'disable()'.

        Metodu '__init__()' nech� odvozen� t��dy nep�edefinov�vaj�. Nech�
        p�edefinov�vaj� metody '_create_widget()' a '_create_label'.

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
                 _("Vr�tit p�vodn� hodnotu"),
                 _("Vr�tit ve�ker� proveden� zm�ny.")),)

    def guardian(self):
        return self._guardian

    # Zpracov�n� p��kaz�
    
    def _can_reset(self):
        return self.is_modified() and self.is_enabled()

    def _cmd_reset(self):
        self.reset()

    def _cmd_context_menu(self):
        self._on_context_menu()

    # Ostatn� neve�ejn� metody.

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
            # Pokud je hodnota validn�, dej o zm�n� v�d�t formul��i.
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
        # TODO: Zkusit to p�es `wx.Window.SetFocusFromKbd()'
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
        # V��e uveden� Disconnect nefunguje, tak�e si to ubastl�me po sv�m...
        self._unregistered_widgets[self._ctrl] = True

    def _on_change(self, event=None):
        """Event handler volan� p�i jak�koliv zm�n� hodnoty pol��ka."""
        # Toto je hack aby bylo mo�n� vytv��et zak�zan� pol��ka, kter� nejsou
        # za�ediv�na a vypadaj� tedy stejn� jako editovateln�, ale nelze je
        # zm�nit.  Zde tedy po ka�d�m pokusu o zm�nu vr�t�me p�vodn� hodnotu a
        # hotovo.  V jin�ch p��padech by nem�lo doj�t k tomu, �e pol��ko, kter�
        # je `self._enabled' zm�n� hodnotu, tak�e by to snad nemuselo ni�emu
        # vadit...
        #if self._enabled:  D�l� to probl�my s dopo��t�van�mi pol��ky, tak�e
        # je to zat�m vy�azeno.  Nejlep�� by bylo to vymyslet �pln� jinak...
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
        
        Vol�no po ka�d� zm�n� hodnoty pol��ka.
        
        """
        pass
    
    def has_focus(self):
        """Vra� pravdu pr�v� kdy� je pol��ko zaost�eno pro u�iv. vstup."""
        return InputField.focused() is self

    def width(self):
        """Vra� ���ku pol��ka danou specifikac�; po�et znak�."""
        return self.spec().width(self._DEFAULT_WIDTH)

    def height(self):
        """Vra� v��ku pol��ka danou specifikac�; po�et znak�."""
        return self.spec().height(self._DEFAULT_HEIGHT)

    def id(self):
        """Vra� identifik�tor pol��ka (string)."""
        return self._id

    def spec(self):
        """Vra� prezenta�n� specifikaci pol��ka jako 'FieldSpec'."""
        return self._spec

    def type(self):
        """Vra� datov� typ pol��ka jako instanci 'pytis.data.Type'."""
        return self._type

    def widget(self):
        """Vra� ovl�dac� prvek jako instanci 'wx.Window'."""
        return self._widget

    def label(self):
        """Vra� nadpis pol��ka jako 'wx.StaticText'."""
        return self._label

    def validate(self, quiet=False, interactive=True, **kwargs):
        """Zvaliduj hodnotu pol��ka a vra� instanci 'Value' a popis chyby.

        Argumenty:
        
          quiet -- v p��pad� pravdiv� hodnoty je v�sledek validace metodou
            pouze vr�cen a chyba nen� nijak ohla�ov�na.  V opa�n�m p��pad� je
            chyba ohl�ena zp�sobem, kter� z�v�s� na argumentu `interactive'.
          interactive -- pokud je pravdiv�, dojde k ohl�en� chyby vysko�iv��m
            dialogem s popisem chyby.  V opa�n�m p��pad� je pouze zobrazena
            zpr�va ve stavov� ��dce.
          **kwargs -- kl��ov� argumenty, kter� maj� b�t p�ed�ny metod�
            'pytis.data.Type.validate()'.

        Vrac�: Tuple (value, error), tak, jak ho vr�t�
        'pytis.data.Type.validate()' p��slu�n�ho datov�ho typu pro hodnotu
        zadanou v pol��ku.

        """
        value, error = self._type.validate(self.get_value(), **kwargs)
        if error and not quiet:
            if interactive:
                msg = _('Chyba validace pol��ka!\n\n%s: %s') % \
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
        """Vra� pravdu, pokud je pol��ko editovateln�."""
        return self._enabled
    
    def enable(self):
        """Povol u�ivatelsk� vstup do pol��ka."""
        if self._accessible:
            self._enabled = True
            self._enable()
            self._unregister_skip_navigation_callback()
    
    def _enable(self):
        self._ctrl.Enable(True)

    def disable(self, change_appearance=True):
        """Zaka� u�ivatelsk� vstup do pol��ka.

        Vol�n�m t�to metody se pol��ko stane read-only.  Nebude tedy aktivn� na
        u�ivatelsk� vstup.  Pokud nen� nastaven argument `change_appearance' na
        nepravdivou hodnotu, bude tak� zm�n�n vzhled pol��ka (za�ediv�n�).

        Odvozen� t��dy nech� tuto metodu nep�edefinov�v�j�, nech� rad�ji
        p�edefinuj� metodu '_disable()'.

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
        """U�i� toto pol��ko aktivn�m pro vstup z kl�vesnice."""
        self._want_focus = True

    def _set_focus(self):
        self._ctrl.SetFocus()

    def initialized(self):
        """Vra� pravdu pr�v� kdy� pol��ko ji� bylo inicializov�no.""" 
        return self._initialized
        

    def init(self, value):
        """Nastav po��te�n� hodnotu pol��ka na 'value'. 

        Argumenty:

          value -- po��te�n� hodnota pol��ka (stejn�, jako pro metodu
            'set_value()').

        Takto nastavenou po��te�n� hodnotu lze kdykoliv vr�tit metodou
        'reset()'.  Metodou 'is_modified()' lze potom zjistit, zda se sou�asn�
        hodnota pol��ka li�� od t�to po��te�n� hodnoty.

        """
        self._disable_event_handlers()
        self.set_value(value)
        self._initial_value = self.get_value()
        self._is_changed = True
        self._enable_event_handlers()
        self._initialized = True

    def get_value(self):
        """Vra� hodnotu pol��ka jako string.
        
        Tuto metodu je t�eba p�edefinovat v odvozen� t��d�.
        
        """
        raise ProgramError("This method must be overriden!")

    def set_value(self, value):
        """Nastav hodnotu pol��ka na 'value'.

        Argumenty:

          value -- hodnota pol��ka, string (pokud datov� typ pol��ka nevy�aduje
            jinak)

        Vrac�: Pravdu, jestli�e hodnota byla �sp�n� nastavena, nepravdu
        v�opa�n�m p��pad�.

        Pokud je hodnota None, nebude provedeno nic, pouze vr�ceno False.

        Odvozen� t��dy nech� tuto metodu nep�edefinov�v�j�, nech� p�edefinuj�
        metodu '_set_value()'.

        """
        if __debug__:
            log(DEBUG, 'Nastaven� hodnoty pol��ka:', (self.id(), value))
        if value is not None:
            return self._set_value(value)
        else:
            return False

    def _set_value(self, value):
        raise ProgramError("This method must be overriden!")

    def is_modified(self):
        """Vra� pravdu, pr�v� pokud byla hodnota pol��ka zm�n�na u�ivatelem.

        Pol��ko je nastaveno do po��te�n�ho stavu po ka�d�m vol�n� metody
        'init()'. Metoda vr�t� pravdu pr�v� kdy� je sou�asn� hodnota pol��ka
        rozd�ln� od hodnoty v po��te�n�m stavu.
        
        """
        return self._initial_value != self.get_value()

    def is_enabled(self):
        return self._enabled
    
    def reset(self):
        """Nastav hodnotu pol��ka na p�vodn� hodnotu.

        P�vodn� hodnotou je my�lena hodnota po posledn�m vol�n� metody
        'init()'. Pokud motoda 'init()' nebyla doposud vol�na, je chov�n�
        metody nespecifikov�no.
        
        """
        if __debug__: log(DEBUG, 'Reset hodnoty pol��ka', self.id())
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
        """Vra� maxim�ln� d�lku zadan�ho textu."""
        return None

    def _on_maxlen(self, event):
        # User tried to enter more text into the control than the limit
        beep()
        message(_("P�ekro�ena maxim�ln� d�lka."))
    
    def _on_enter_key(self, event):
        if self.height() > 1:
            event.Skip()
        else:
            event.GetEventObject().Navigate()

    def _post_process_func(self):
        """Vra� funkci odpov�daj�c� specifikaci postprocessingu pol��ka.

        Vrac�: Funkci je funkc� jednoho argumentu (p�vodn� text), kter� vrac�
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
            # Pokud to ud�l�m p��mo, u n�kter�ch pol��ek se zm�na neprojev�!
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
        return bool(self.get_value())

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
    """Vstupn� pole pro typ Boolean realizovan� pomoc� 'wx.CheckBox'."""

    def _create_ctrl(self):
        """Vra� instanci 'wx.CheckBox'."""
        if self._inline:
            label = ''
        else:
            label = self.spec().label()
        control = wx.CheckBox(self._parent, -1, label)
        wx_callback(wx.EVT_CHECKBOX, control, control.GetId(), self._on_change)
        return control
                    
    def get_value(self):
        """Vra� hodnotu pol��ka jako string.

        Je vr�cen string 'T', je-li pol��ko zatr�eno, string 'F' jinak.

        """
        return self._ctrl.GetValue() and 'T' or 'F'

    def _set_value(self, value):
        """Nastav hodnotu pol��ka na 'value'.

        Argumenty:

            value -- hodnota pol��ka, string 'T' (pravda) nebo 'F' (nepravda)
              nebo pr�zdn� �et�zec (nepravda)

        Vrac�: Pravdu, jestli�e hodnota byla �sp�n� nastavena, nepravdu
        v�opa�n�m p��pad�.

        """
        assert value in ('T','F',''), ('Invalid argument', value)
        wxvalue = value == 'T' and True or False
        self._ctrl.SetValue(wxvalue)
        # _on_change mus�me volat ru�n�, proto�e SetValue() nevyvol� ud�lost.
        self._on_change()
        return True


class EnumerationField(InputField):
    """Abstrakce vstupn�ho pole pro v��tov� typ.
    
    Tento typ vstupn�ho pole je reprezentov�n pomoc� v�b�ru z pevn� dan�
    mno�iny hodnot.  Mno�inu hodnot ur�uje enumer�tor datov�ho typu (viz metoda
    'pytis.data.FixedEnumerator.values()').

    Tato t��da nen� ur�ena k p��m�mu pou�it�. Je to rodi�ivsk� t��da pro
    vstupn� pole nad v��tov�m typem dat.
    
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
    """Vstupn� pole pro v��tov� typ reprezentovan� pomoc� 'wx.Choice'."""

    def _create_ctrl(self):
        """Vra� instanci 'wx.Choice' podle specifikace."""
        control = wx.Choice(self._parent, choices=self._choices())
        wx_callback(wx.EVT_CHOICE, control, control.GetId(), self._on_change)
        return control

    
class RadioBoxField(Unlabeled, EnumerationField):
    """Vstupn� pole pro v��tov� typ reprezentovan� pomoc� 'wx.RadioBox'.

    Interpretace specifikace:

      orientation -- tento specifika�n� atribut ud�v� hlavn� orientaci skl�d�n�
        jednotliv�ch prvk�. Hodnotou je konstanta 'spec.Orientation'.
      width -- v p��pad� horizont�ln� orientace ud�v� maxim�ln� po�et sloupc�
        prvk� vedle sebe.
      height -- v p��pad� vertik�ln� orientace ud�v� maxim�ln� po�et �ad
        prvk� nad sebou.

    """

    _DEFAULT_WIDTH = 1

    def _create_ctrl(self):
        """Vra� instanci 'wx.RadioBox' podle specifikace."""
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
    """Vstupn� pole pro v��tov� typ reprezentovan� pomoc� 'wx.ListBox'."""

    def _create_ctrl(self):
        """Vra� instanci 'wx.ListBox' podle specifikace."""
        control = wx.ListBox(self._parent, choices=self._choices(),
                             style=wx.LB_SINGLE|wx.LB_NEEDED_SB)
        wx_callback(wx.EVT_LISTBOX, control, control.GetId(), self._on_change)
        return control
    

class Invocable(object, CommandHandler):
    """Mix-in t��da pro pol��ka s mo�nost� vyvol�n� v�b�ru.

    Abstraktn� t��da pro pol��ka, kter� umo��uj� vyvolat pro v�b�r hodnoty
    n�jakou akci (v�t�inou v podob� mod�ln�ho popup okna).

    Vstupn� pol��ko (vytvo�en� metodou '_create_widget()' z�kladn� t��dy) bude
    dopln�no o tla��tko pro vyvol�n� v�b�ru.

    V�b�r lze vyvolat tak� kl�vesou p��kazu
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
        """Zavolej '_create_widget()' odvozen� t��dy a p�idej tla��tko.

        V�ce informac� viz. dokumentace t��dy 'Invocable'.
        
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
    """Vstupn� pole pro datov� typ 'pytis.data.Date'.

    Jako akci pro vyvol�n� v�b�ru definuje zobrazen� dialogu s kalend��em,
    kter� je nastaven na datum odpov�daj�c� hodnot� pol��ka a po ukon�en�
    nastav� hodnotu pol��ka na vybran� datum.

    """

    _DEFAULT_WIDTH = 10
    _INVOKE_SELECTION_MENU_TITLE = _("Vybrat z kalend��e")
    _INVOKE_SELECTION_MENU_HELP = _("Zobrazit kalend�� pro v�b�r datumu.")
    
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
    """Vstupn� pole pro v�b�r barvy."""

    _DEFAULT_WIDTH = 7
    _INVOKE_SELECTION_MENU_TITLE = _("Vybrat barvu")
    _INVOKE_SELECTION_MENU_HELP = _("Zobrazit dialog pro v�b�r barev.")
    
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
    """Spole�n� nadt��da ��seln�kov�ch pol��ek."""

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
        """Zobraz ��seln�k a po jeho skon�en� nastav hodnotu pol��ka."""
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
    _INVOKE_SELECTION_MENU_TITLE = _("Vybrat z ��seln�ku")
    _INVOKE_SELECTION_MENU_HELP = _("Zobrazit ��seln�k p��pustn�ch hodnot "
                                    "s mo�nost� v�b�ru.")

    def _create_widget(self):
        """Zavolej '_create_widget()' t��dy Invocable a p�idej displej."""
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
            button.SetToolTipString(_("Vlo�it nov� z�znam do ��seln�ku"))
            wx_callback(wx.EVT_BUTTON, button, button.GetId(),
                        self._on_codebook_insert)
            wx_callback(wx.EVT_NAVIGATION_KEY, button,
                        self._skip_navigation_callback(button))
            sizer.Add(button, 0, wx.FIXED_MINSIZE)
        return sizer

    def _menu(self):
        return Invocable._menu(self) + \
               ((self.COMMAND_INVOKE_SELECTION(alternate=True),
                 _("Vyhled�vat v ��seln�ku"),
                 _("Zobrazit ��seln�k se zapnut�m inkrement�ln�m "
                   "vyhled�v�n�m.")),)

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
    """��seln�kov� pol��ko zobrazuj�c� data ��seln�ku jako sou��st formul��e.

    Pokud je 'selection_type' ��seln�kov�ho pol��ka ve specifikaci ur�en jako
    'LIST_FIELD', bude ve formul��i pou�it tento typ vstupn�ho pole.

    """
    _DEFAULT_WIDTH = 30
    _DEFAULT_HEIGHT = 6

    def _create_ctrl(self):
        # Na�tu specifikace.
        view_spec = resolver().get(self._codebook_name, 'view_spec')
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
        # Callback m��e b�t vol�n i kdy� u� je list mrtev.
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
        """Vra� aktu�ln� vnit�n� hodnotu pol��ka."""
        i = self._selected_item
        if i is not None:
            return self._list_data[i].export()
        else:
            return None

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
                                            name=self._codebook_name,
                                            select_row=self._select_row_arg()),
                 _("Zobrazit celou tabulku"),
                 _("Otev��t n�hled ��seln�ku v ��dkov�m formul��i.")),
                )

    # Zpracov�n� p��kaz�
    
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


    
