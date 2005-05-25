# -*- coding: iso-8859-2 -*-

# Copyright (C) 2001, 2002, 2003, 2004, 2005 Brailcom, o.p.s.
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

        
class InputField(object, KeyHandler, CallbackHandler):
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

    UI_IS_MODIFIED = 'UI_IS_MODIFIED'
    """Povolen� ud�losti, pouze pokud je obsah pol��ka zm�n�n."""
    UI_IS_ENABLED =  'UI_IS_ENABLED'
    """Povolen� ud�losti, pouze pokud je pol��ko editovateln�."""

    CALL_LEAVE_FIELD = 'CALL_LEAVE_FIELD'
    """Callback volan� p�i po�adavku na opu�t�n� pol��ka. Bez argument�."""
    CALL_COMMIT_FIELD = 'CALL_COMMIT_FIELD'
    """Vol�no p�i opu�t�n� pol��ka s potvrzen�m hodnoty. Bez argument�."""
    CALL_FIELD_CHANGE = 'CALL_FIELD_CHANGE'
    """Callback volan� p�i zm�n� pol��ka, pokud je nov� hodnota validn�.

    Argumenty:

      fid -- id pol��ka, string.
      value -- nov� hodnota pol��ka, instance 'pytis.data.Value'.
    
    """
    CALL_SKIP_NAVIGATION = 'CALL_SKIP_NAVIGATION'
    """Vol�no, pokud m� b�t n�jak� ui prvek p�esko�en p�i navigaci.

    To je vhodn� nap��klad pro tla��tka pol��ek typu 'Invocable', nebo display
    u 'CodebookField'.  Navigaci v�ak zaji��uje nad��zen� formul��.

    Argumenty:

      object -- ui prvek kter� m� b�t p�esko�en.
      dir -- sm�r pohybu, boolean, pravda p�i pohybu vp�ed.

    """

    _focused_field = None
    _last_focused_field = None
    
    def create(cls, parent, fspec, data, guardian=None, inline=False,
               accessible=True):
        """Vra� instanci pol��ka odpov�daj�c�ho typu.
        
        Argumewnty jsou toto�n�, jako u metody 'InputField.__init__()'.
        
        """
        field = TextField # default
        type = fspec.type(data)
        if fspec.width() == 0: 
            field = HiddenField
        elif fspec.references() is not None: 
            field = ListField
        elif isinstance(type, pytis.data.Date):
            field = DateField
        elif isinstance(type, pytis.data.Boolean):
            field = CheckBoxField
        elif isinstance(type, pytis.data.Color):
            field = ColorSelectionField
        elif isinstance(type, (pytis.data.Number, pytis.data.String)) \
                 and type.enumerator():
            if fspec.codebook():
                field = CodebookField
            else:
                field = ChoiceField # default
                if not inline:
                    selection_type = fspec.selection_type()
                    if selection_type == SelectionType.LIST_BOX:
                        field = ListBoxField
                    elif selection_type == spec.SelectionType.RADIO_BOX:
                        field = RadioBoxField
        elif isinstance(type, pytis.data.String):
            field = StringField
        elif isinstance(type, pytis.data.Number):
            field = NumericField
        return field(parent, fspec, data, guardian=guardian, inline=inline,
                     accessible=accessible)

    create = classmethod(create)

    def __init__(self, parent, fspec, data, guardian=None, inline=False,
                 accessible=True):
        """Vytvo� vstupn� pol��ko, podle specifikace a typu dat.

        Argumenty:

          parent -- libovoln� instance 'wx.Window', kter� m� b�t pou��v�na
            jako wx rodi� v�ech vytv��en�ch wx prvk�
          fspec -- specifikace prezenta�n�ch vlastnost�, instance t��dy
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
        assert isinstance(fspec, FieldSpec)
        CallbackHandler.__init__(self)
        self._parent = parent
        self._type = fspec.type(data)
        self._spec = fspec
        self._guardian = guardian
        self._id = id = fspec.id()
        self._inline = inline
        self._initial_value = None
        self._want_focus = False
        self._is_changed = False
        self._initialized = False
        self._accessible = self._enabled = accessible
        self._ctrl = self._create_ctrl()
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
                self._run_callback(self.CALL_SKIP_NAVIGATION,
                                   (widget, e.GetDirection()))
                return True
            else:
                e.Skip()
                return True
        return cb
    
    def _init_ctrl(self):
        c = self._ctrl
        KeyHandler.__init__(self, c)
        wx_callback(wx.EVT_IDLE,       c, self._on_idle)
        wx_callback(wx.EVT_KILL_FOCUS, c, self._on_kill_focus)
        wx_callback(wx.EVT_SET_FOCUS,  c, self._on_set_focus)
        wx_callback(wx.EVT_RIGHT_UP,   c, self._on_popup_menu)
        wx_callback(wx.EVT_COMMAND_RIGHT_CLICK, c, c.GetId(),
                    self._on_popup_menu)
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
        # Return the tuple of popup menu items ('MItem' instances).
        return (MItem("Vr�tit p�vodn� hodnotu",
                      command = InputField.COMMAND_RESET_FIELD,
                      uievent_id = self.UI_IS_MODIFIED),
                )

    def guardian(self):
        return self._guardian
    
    def on_ui_event(self, event, id):
        if id == self.UI_IS_MODIFIED:
            if self.is_modified():
                event.Enable(True)
            else:
                event.Enable(False)
            return True
        elif id == self.UI_IS_ENABLED:
            if self._enabled:
                event.Enable(True)
            else:
                event.Enable(False)
            return True
        else:
            return False

    def on_command(self, command, **kwargs):
        if command == self.COMMAND_RESET_FIELD:
            self.reset()
            return True
        elif command == self.COMMAND_COMMIT_FIELD:
            return self._run_callback(self.CALL_COMMIT_FIELD, ())
        elif command == self.COMMAND_LEAVE_FIELD:
            return self._run_callback(self.CALL_LEAVE_FIELD, ())
        return False
    
    def show_popup_menu(self, position=None):
        """Zobraz kontextov� menu vstupn�ho pol��ka.

        Argumenty:
        
          position -- tuple (x, y) ud�vaj�c� pozici relativn� k vstupn�mu
            prvku tohoto pol��ka.
        
        """
        control = self._ctrl
        if position is None:
            size = control.GetSize()
            position = (size.x/3, size.y/2)
        menu = Menu('', self._menu()).create(control, self)
        control.PopupMenu(menu, position)
        menu.Destroy()

    def _on_popup_menu(self, event):
        self.show_popup_menu(position=(event.GetX(), event.GetY()))
        event.Skip()

    def _validation_error_handler(self):
        # O�et�i valida�n� chybu.
        
        # Tato metoda je vol�na, pokud u�ivatel opou�t� pol��ko, ve kter�m
        # zm�nil hodnotu, ale nov� hodnota nen� validn�.  Vrac� pravdu pokud
        # byla situace n�jak o�et�ena, nebo nepravdu, pokud m� b�t situace
        # o�et�ena standardn�m zp�sobem (zobrazen�m chybov� zpr�vy ve stavov�m
        # ��dku).
        
        # V t�to t��d� ned�l� nic, ale odvozen� t��da m��e p�edefinov�n�m
        # vyvolat nap��klad n�jakou interaktivn� akci apod.

        return False
        
    def _on_idle(self, event):
        if self._is_changed:
            # Pokud je hodnota validn�, dej o zm�n� v�d�t formul��i.
            value, error = self.validate(quiet=True)
            if not error:
                self._run_callback(self.CALL_FIELD_CHANGE, (self.id(), value))
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
        wx_callback(wx.EVT_NAVIGATION_KEY, control,
                    self._skip_navigation_callback(control))
        if self._unregistered_widgets.has_key(control):
            del(self._unregistered_widgets[control])

    def _unregister_skip_navigation_callback(self):
        #self._ctrl.Disconnect(-1, -1, wx.wxEVT_NAVIGATION_KEY)
        # V��e uveden� Disconnect nefunguje, tak�e si to ubastl�me po sv�m...
        self._unregistered_widgets[self._ctrl] = 1

    def _on_change(self, event=None):
        """Event handler volan� p�i jak�koliv zm�n� hodnoty pol��ka.
        
        Odvozen� t��dy by tuto metodu nem�ly p�edefinov�vat. Nech� rad�ji
        p�edefiuj� metodu '_on_change_hook()'. Ka�d� t��da by m�la zabezpe�it
        o�et�en� edita�n�ch ud�lost� vol�n�m t�to metody.

        """
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
        self._on_change_hook()
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
    
    def _on_change_hook(self):
        """O�et�i zm�ny textu pol��ka.
        
        P�edefinov�n�m t�to metody lze prov�d�t libovoln� dopl�uj�c� akce
        p�i ka�d� zm�n� textu pol��ka.
        
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

    def validate(self, quiet=False, interactive=True):
        """Zvaliduj hodnotu pol��ka a vra� instanci 'Value' a popis chyby.

        Argumenty:
        
          quiet -- v p��pad� pravdiv� hodnoty je v�sledek validace metodou
            pouze vr�cen a chyba nen� nijak ohla�ov�na.  V opa�n�m p��pad� je
            chyba ohl�ena zp�sobem, kter� z�v�s� na argumentu `interactive'.
          interactive -- pokud je pravdiv�, dojde k ohl�en� chyby vysko�iv��m
            dialogem s popisem chyby.  V opa�n�m p��pad� je pouze zobrazena
            zpr�va ve stavov� ��dce.

        Vrac�: Tuple (value, error), tak, jak ho vr�t�
        'pytis.data.Type.validate()' p��slu�n�ho datov�ho typu pro hodnotu
        zadanou v pol��ku.

        """

        value, error = self._type.validate(self.get_value())
        if error and not quiet:
            if not self._validation_error_handler():
                if interactive:
                    msg = _('Chyba validace pol��ka!\n\n%s: %s') % \
                          (self.spec().label(), error.message())
                    run_dialog(Error, msg, title=_("Chyba validace"))
                else:
                    message(error.message(), beep_=True)
            else:
                # Revalidate after succesful handler invocation.
                value, error = self._type.validate(self.get_value())
        return value, error

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
            # Here we rely on a simple hack in InputField._on_change() that
            # resets field value after each attempt to chnge it, so the field
            # is in fact editable, but it is not possible to change it
            # effectively.
            pass

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
        self._on_change_hook()
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

    def _set_value(self):
        raise ProgramError("This method must be overriden!")

    def is_modified(self):
        """Vra� pravdu, pr�v� pokud byla hodnota pol��ka zm�n�na u�ivatelem.

        Pol��ko je nastaveno do po��te�n�ho stavu po ka�d�m vol�n� metody
        'init()'. Metoda vr�t� pravdu pr�v� kdy� je sou�asn� hodnota pol��ka
        rozd�ln� od hodnoty v po��te�n�m stavu.
        
        """
        return self._initial_value != self.get_value()

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
        control = wx.TextCtrl(self._parent, -1, '', style=self._style())
        if not self._inline:
            width, height = self.width(), self.height()
            size = dlg2px(control, 4*(width+1)+2, 8*height+4)
            control.SetMinSize(size)
        maxlen = self._maxlen()
        if maxlen is not None:
            control.SetMaxLength(maxlen)
            wx_callback(wx.EVT_TEXT_MAXLEN, control, control.GetId(),
                        self._on_maxlen)
        filter = self._filter()
        control.SetValidator(_TextValidator(control, filter=filter))
        wx_callback(wx.EVT_TEXT, control, control.GetId(), self._on_change)
        if self.height() > 1: # For multiline fields add a custom key handler
            wx_callback(wx.EVT_KEY_DOWN, control,
                        lambda e: self._on_key_down_multiline(e, control))
        return control
    
    def _maxlen(self):
        """Vra� maxim�ln� d�lku zadan�ho textu."""
        return None

    def _style(self):
        # Return the style for created text control (to be redefined).
        style = 0 #wx.NO_BORDER
        if self.height() > 1:
            style = style | wx.TE_MULTILINE
        return style

    def _on_set_focus(self, event):
        super_(TextField)._on_set_focus(self, event)
        if self._enabled:
            event.GetEventObject().SetSelection(-1, -1)
        
    def _on_maxlen(self, event):
        # User tried to enter more text into the control than the limit
        beep()
        message(_("P�ekro�ena maxim�ln� d�lka."))
    
    def _on_key_down_multiline(self, event, control):
        # Used only for multiline fields,
        if event.GetKeyCode() == wx.WXK_RETURN and self._enabled:
            control.WriteText("\n");
            self._on_change()
            return True
        else:
            # Other keys are processed in a standard way.
            event.Skip()
            return True

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
        oldval = self.get_value()
        if f:
            args = (oldval,)
            val = f(*args)
            if val != oldval:
                self._set_value(val)

    def _disable(self, change_appearance):
        self._ctrl.SetEditable(False)
        self._ctrl.SetValidator(wx.DefaultValidator)
        if change_appearance:
            if self._accessible:
                color = config.field_disabled_color 
            else:
                color = config.field_inaccessible_color
            def call_on_idle():
                self._ctrl.SetBackgroundColour(color)
                self._ctrl.Refresh()
            # Pokud to ud�l�m p��mo, u n�kter�ch pol��ek se zm�na neprojev�!
            self._call_on_idle = call_on_idle
            
    def _set_value(self, value):
        """Nastav hodnotu pol��ka na 'value'.

        Argumenty:

            value -- hodnota pol��ka, string

        Vrac�: Pravdu, jestli�e hodnota byla �sp�n� nastavena, nepravdu
        v�opa�n�m p��pad�.

        """
        assert isinstance(value, types.StringTypes), \
               ('String or Unicode expected', value)
        self._ctrl.SetValue(value)
        return True


class StringField(TextField):
    """Textov� vstupn� pol��ko pro data typu 'pytis.data.String'."""

    def _maxlen(self):
        return self._type.maxlen()

class NumericField(TextField):
    """Textov� vstupn� pol��ko pro data typu 'pytis.data.Number'."""

    def _style(self):
        return super(NumericField, self)._style()#| wx.TE_RIGHT


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
        return True


class EnumerationField(InputField):
    """Abstrakce vstupn�ho pole pro v��tov� typ.
    
    Tento typ vstupn�ho pole je reprezentov�n pomoc� v�b�ru z pevn� dan�
    mno�iny hodnot.  Mno�inu hodnot ur�uje enumer�tor datov�ho typu (viz metoda
    'pytis.data.FixedEnumerator.values()').

    Tato t��da nen� ur�ena k p��m�mu pou�it�. Je to rodi�ivsk� t��da pro
    vstupn� pole nad v��tov�m typem dat.
    
    """
    def _values(self):
        # Return a sequence of string representations of all type's values.
        t = self._type
        return [t.export(v) for v in t._enumerator().values()]

    def get_value(self):
        return self._ctrl.GetStringSelection()

    def _set_value(self, value):
        assert isinstance(value, types.StringTypes), ('Invalid value', value)
        self._ctrl.SetStringSelection(value)
        # TODO: Tento test nefunguje pro pol��ka, kter� vzniknou z Codebook�
        # if self._ctrl.GetStringSelection() != value:
        #     raise ProgramError("Setting field value failed!",
        #                        self.id(), value[0])
        return True
        


class ChoiceField(EnumerationField):
    """Vstupn� pole pro v��tov� typ reprezentovan� pomoc� 'wx.Choice'."""

    def _create_ctrl(self):
        """Vra� instanci 'wx.Choice' podle specifikace."""
        control = wx.Choice(self._parent, -1, (-1,-1), (-1,-1),
                            choices=self._values())
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
                              choices=self._values(), style=style,
                              majorDimension=dimension)
        wx_callback(wx.EVT_RADIOBOX, control, control.GetId(), self._on_change)
        return control


class ListBoxField(EnumerationField):
    """Vstupn� pole pro v��tov� typ reprezentovan� pomoc� 'wx.ListBox'."""

    def _create_ctrl(self):
        """Vra� instanci 'wx.ListBox' podle specifikace."""
        control = wx.ListBox(self._parent, choices=self._values(),
                             style=wx.LB_SINGLE|wx.LB_NEEDED_SB)
        wx_callback(wx.EVT_LISTBOX, control, control.GetId(), self._on_change)
        return control
    

class Invocable:
    """Mix-in t��da pro pol��ka s mo�nost� vyvol�n� v�b�ru.

    Abstraktn� t��da pro pol��ka, kter� umo��uj� vyvolat pro v�b�r hodnoty
    n�jakou akci (v�t�inou v podob� mod�ln�ho popup okna).

    Vstupn� pol��ko (vytvo�en� metodou '_create_widget()' z�kladn� t��dy) bude
    dopln�no o tla��tko pro vyvol�n� v�b�ru.

    V�b�r lze vyvolat tak� kl�vesou p��kazu
    'Invocable.COMMAND_INVOKE_SELECTION'.

    """
    _INVOKE_SELECTION_MENU_TITLE = _("Vybrat hodnotu")
    
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
        button_height = self._ctrl.GetSize().GetHeight()
        self._invocation_button = button = self._create_button(button_height)
        sizer = wx.BoxSizer()
        sizer.Add(widget, 0, wx.RIGHT, 4)
        sizer.Add(button)
        wx_callback(wx.EVT_BUTTON, button, button.GetId(),
                    lambda e: self._on_invoke_selection())
        wx_callback(wx.EVT_NAVIGATION_KEY, button,
                    self._skip_navigation_callback(button))
        return sizer

    def _create_button(self, height):
        button = wx.Button(self._parent, -1, "...")
        button.SetMinSize((dlg2px(button, 12), height))
        return button

    def _disable(self, change_appearance):
        self._invocation_button.Enable(False)
        self._call_next_method('_disable', change_appearance)
    
    def _enable(self):
        self._invocation_button.Enable(True)
        self._call_next_method('_enable')
    
    def _on_invoke_selection(self, **kwargs):
        """Callback pro akci vyvol�n� v�b�ru."""
        raise ProgramError("This method must be overriden!")

    def _menu(self):
        return InputField._menu(self) + \
               (MSeparator(),
                MItem(self._INVOKE_SELECTION_MENU_TITLE,
                      command=self.COMMAND_INVOKE_SELECTION,
                      uievent_id=self.UI_IS_ENABLED,
                      args={'originator': self}))
    
    def on_command(self, command, **kwargs):
        if self._enabled:
            if command == Invocable.COMMAND_INVOKE_SELECTION:
                return self._on_invoke_selection()
            if command == Invocable.COMMAND_INVOKE_SELECTION_ALTERNATE:
                return self._on_invoke_selection(alternate=True)
        return InputField.on_command(self, command, **kwargs)


class DateField(Invocable, TextField):
    """Vstupn� pole pro datov� typ 'pytis.data.Date'.

    Jako akci pro vyvol�n� v�b�ru definuje zobrazen� dialogu s kalend��em,
    kter� je nastaven na datum odpov�daj�c� hodnot� pol��ka a po ukon�en�
    nastav� hodnotu pol��ka na vybran� datum.

    """

    _DEFAULT_WIDTH = 10
    _INVOKE_SELECTION_MENU_TITLE = _("Vybrat z kalend��e")
    
    def _on_invoke_selection(self, **kwargs):
        """Zobraz kalend�� a po jeho skon�en� nastav hodnotu pol��ka."""
        d = pytis.data.Date.make()
        result, error = d.validate(self.get_value())
        if result is not None:
            date = result.value()
        else:
            date = None
        date = run_dialog(Calendar, date)
        if date != None:
            self.set_value(d.export(date))
        return True


class ColorSelectionField(Invocable, TextField):
    """Vstupn� pole pro v�b�r barvy."""

    _DEFAULT_WIDTH = 7
    _INVOKE_SELECTION_MENU_TITLE = _("Vybrat barvu")
    
    def _on_invoke_selection(self, **kwargs):
        """Zobraz kalend�� a po jeho skon�en� nastav hodnotu pol��ka."""
        color = run_dialog(ColorSelector, self.get_value())
        if color != None:
            self.set_value(color)
        return True

    def _create_button(self, height):
        button = wx.Button(self._parent, -1, "")
        button.SetMinSize((height, height))
        return button

    def _set_value(self, value):
        super(ColorSelectionField, self)._set_value(value)
        self._invocation_button.SetBackgroundColour(value)
        self._invocation_button.SetForegroundColour(value)


class CodebookField(Invocable, TextField):
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
    
    def _create_widget(self):
        """Zavolej '_create_widget()' t��dy Invocable a p�idej displej."""
        widget = Invocable._create_widget(self)
        try:
            cb_spec = resolver().get(self.spec().codebook(), 'cb_spec')
        except ResolverError:
            cb_spec = CodebookSpec()
        except AttributeError:
            cb_spec = CodebookSpec()
        self._cb_spec = cb_spec
        if self._inline or cb_spec.display() is None:
            return widget
        self._display_column = cb_spec.display()
        display_size = self.spec().display_size() or cb_spec.display_size()
        display = wx.TextCtrl(self._parent, style=wx.TE_READONLY)
        size = char2px(display, display_size, 1)
        size.SetHeight(self._ctrl.GetSize().GetHeight())
        display.SetMinSize(size)
        display.SetBackgroundColour(wx.Colour(213, 213, 213))
        sizer = wx.BoxSizer()
        sizer.Add(widget, 0, wx.RIGHT, 4)
        sizer.Add(display)
        self._display = display
        wx_callback(wx.EVT_NAVIGATION_KEY, display,
                    self._skip_navigation_callback(display))
        return sizer

    def _menu(self):
        return Invocable._menu(self) + \
               (MItem("Vyhled�vat v ��seln�ku",
                      command=self.COMMAND_INVOKE_SELECTION_ALTERNATE,
                      uievent_id=self.UI_IS_ENABLED,
                      args={'originator': self}),)

    def _maxlen(self):
        try:
            return self._type.maxlen()
        except AttributeError:
            return None

    def _on_change_hook(self):
        if hasattr(self, '_display_column'):
            v = self.get_value()
            dv = self._type.enumerator().get(v, self._display_column)
            d = dv and dv.export() or ''
            self._display.SetValue(d)

    def _on_invoke_selection(self, alternate=False, **kwargs):
        """Zobraz ��seln�k a po jeho skon�en� nastav hodnotu pol��ka."""
        value, error = self.validate(quiet=True)
        enumerator = self._type.enumerator()
        begin_search = alternate or self._cb_spec.begin_search() or None
        result = run_form(CodebookForm, self.spec().codebook(),
                          columns=self._cb_spec.columns(),
                          begin_search=begin_search,
                          select_row=value and (value,),
                          condition=enumerator.validity_condition())
        if result != None:
            self.set_value(result.format(enumerator.value_column()))
        self.set_focus()
        return True

    def _validation_error_handler(self):
        value, error = self._type.validate(self.get_value(), strict=False)
        if error or not self._cb_spec.insert_unknown_values():
            return False
        msg = _("��seln�k neobsahuje hodnotu %s") % value +"\n"+ \
              _("Chcete do ��seln�ku p�idat nov� z�znam?")
        if (run_dialog(Question, msg)):
            prefill = {self._type.enumerator().value_column(): value}
            result = run_form(PopupEditForm, self.spec().codebook(),
                              prefill=prefill)
            #TODO: Update datov�ho objektu ��seln�ku?
            self._on_change_hook()
            return result is not None
        return False
    
    
class ListField(InputField):
    """Seznamov� pol��ko pro zobrazen� z�znam� ze z�visl� tabulky

    Pol��ko slou�� k zobrazen� mno�niny z�znam� z jin� tabulky (datov�ho
    objektu) vztahuj�c�ch se k nastaven� vnit�n� hodnot� pol��ka (viz tak�
    metoda '_set_value()').  Typick� vyu�it� je p�i vazb� p�es cyz� kl�� v
    rela�n� datab�zi.

    Vzhled a chov�n� je ur�eno specifik�torem 'references' ve specifikaci
    pol��ka (viz. 'pytis.form.FieldSpec').  Datov� objekt nav�zan� tabulky je
    z�sk�n pomoc� resolveru podle jm�na ur�en�ho specifikac�. 

    """
    _DEFAULT_WIDTH = 30
    _DEFAULT_HEIGHT = 6

    UI_LIST_ITEM_SELECTED = 'UI_LIST_ITEM_SELECTED'

    def _create_ctrl(self):
        self._ref_spec = refspec = self.spec().references()
        # Na�tu specifikace.
        resolver = pytis.form.application._application.resolver()
        data_spec = resolver.get(refspec.name(), 'data_spec')
        view_spec = resolver.get(refspec.name(), 'view_spec')
        assert isinstance(data_spec, pytis.data.DataFactory)
        assert isinstance(view_spec, ViewSpec)
        self._list_data = \
            data_spec.create(dbconnection_spec=config.dbconnection)
        # Vytvo��m vlastn� seznamov� widget.
        style=wx.LC_REPORT|wx.SUNKEN_BORDER|wx.LC_SINGLE_SEL
        list = wx.ListCtrl(self._parent, -1, style=style)
        # Specifikace sloupc� ulo��m do dictionary.
        colspec = {}
        for id in view_spec.columns():
            colspec[id] = view_spec.field(id)
        self._colspec = colspec
        # Nastav�m z�hlav� sloupc�.
        total_width = 0
        columns = refspec.columns()
        for i in range(len(columns)):
            col = colspec[columns[i]]
            list.InsertColumn(i, col.label())
            width = col.column_width()
            if width < len(col.label()):
                width = len(col.label())
            list.SetColumnWidth(i, dlg2px(list, 4*(width+1)))
            total_width = total_width + width
        # Spo��t�m celkovou v��ku.
        #list.InsertStringItem(0, 'XXX')
        #rect = list.GetItemRect(0) # to vrac� (0,0,0,0)  :-(
        #list.DeleteAllItems()
        # TODO/wx: N�jak spo��tat skute�nou v��ku z�hlav� a ��dku.
        # Tohle jsou "empirick�" vzorce!!!
        header_height = char2px(list, 1, float(9)/4).GetHeight()
        row_height = char2px(list, 1, float(10)/7).GetHeight()
        height = header_height + row_height * self.height()
        self._DEFAULT_WIDTH = total_width + 3
        list.SetMinSize((dlg2px(list, 4*(self.width()+1)), height))
        #list.SetMargins(0,0)
        self._list =  list
        self._get_item = None
        return list

    def _selected_item(self):
        item = self._list.GetNextItem(-1, wx.LIST_NEXT_ALL,
                                      wx.LIST_STATE_SELECTED)
        if item == -1:
            return None
        return item

    def _choose_returned_column(self, column):
        """Vra� hodnotu vybran�ho sloupce."""        
        i = self._selected_item()
        columns = self._ref_spec.columns()
        self._get_item = None
        for j in range(len(columns)):
            if columns[j] == column:
                ctype = self._colspec[column].type(self._list_data)
                itemtext = self._list.GetItem(i,j).GetText()
                val, error = ctype._validate(itemtext)
                if not error:
                    self._get_item = val
                else:
                    self._get_item = None
                break            
        
    def _enable(self):
        pass # ListControl se stejn� ned� nijak editovat
    
    def _disable(self, change_appearance):
        pass # ListControl se stejn� ned� nijak editovat

    def get_item(self):
        """Vra� aktu�ln� hodnotu vybran�ho pol��ka."""
        return self._get_item

    def get_value(self):
        """Vra� aktu�ln� vnit�n� hodnotu pol��ka."""
        return self._value

    def _set_value(self, value):
        """Nastav vnit�n� hodnotu pol��ka a p�ena�ti seznam z�visl�ch z�znam�.
        
        Ka�d� nastaven� hodnotu vyvol� v�b�r z nav�zan�ho datov�ho objektu.
        V�b�r vyfiltruje z nav�zan� tabulky ty z�znamy jejich� sloupe�ek ur�en�
        specifikac� 'references' (instance 'RefSpec') jako 'key' se svou
        hodnotou shoduje s hodnotou p�edanou argumentem 'value'.
        
        """
        self._value = value
        list = self._list
        list.DeleteAllItems()
        v = pytis.data.Value(self.type(), value)
        self._list_data.select(pytis.data.EQ(self._ref_spec.key(), v),
                               sort=self._ref_spec.sorting())
        n = 0
        self._list_rows = []
        while 1:
            row = self._list_data.fetchone()
            if row is None: break
            self._list_rows.append(row)
            columns = self._ref_spec.columns()
            list.InsertStringItem(n, row[columns[0]].export())
            for i in range(1, len(columns)):
                list.SetStringItem(n, i, row[columns[i]].export())
            n = n + 1

        return True

    def _selected_item_key(self):
        i = self._selected_item()
        return self._list_data.row_key(self._list_rows[i])

    def _menu(self):
        menu = (MItem("Editovat",
                      command=self.COMMAND_INVOKE_EDIT_FORM,
                      uievent_id=self.UI_LIST_ITEM_SELECTED,
                      args={'originator': self}),
                MItem("Cel� tabulka",
                      command=self.COMMAND_INVOKE_BROWSE_FORM,
                      uievent_id=self.UI_LIST_ITEM_SELECTED,
                      args={'originator': self})
                )
        if self._ref_spec.returned_columns():
            user_popup = [MItem("Vybrat " + m,
                                command=self.COMMAND_CHOOSE_KEY,
                                args={'id': self.id(),
                                      'returned_key': k,
                                      'originator': self},
                                uievent_id=self.UI_LIST_ITEM_SELECTED)
                          for m, k in self._ref_spec.returned_columns()]
            menu = menu + tuple(user_popup)
        return menu   

    def on_ui_event(self, event, id):
        if id == self.UI_LIST_ITEM_SELECTED:
            if self._selected_item() is not None:
                event.Enable(True)
            else:
                event.Enable(False)
            return True
        else:
            return InputField.on_ui_event(self, event, id)

    def on_command(self, command, **kwargs):
        if command == self.COMMAND_INVOKE_EDIT_FORM:
            run_form(PopupEditForm, self._ref_spec.name(),
                     key=self._selected_item_key())
            return True
        elif command == self.COMMAND_INVOKE_BROWSE_FORM:
            if isinstance(current_form(), PopupForm):
                run_dialog(Warning, _("Celou tabulku nem��ete zobrazit, " + \
                                   "pokud je otev�eno mod�ln� okno formul��e!"))
                return True
            run_form(BrowseForm, self._ref_spec.name(),
                          select_row=self._selected_item_key())
            return True
        elif command == self.COMMAND_CHOOSE_KEY:
            if kwargs.has_key('returned_key'):
                self._choose_returned_column(kwargs['returned_key'])
            return True
        else:            
            return InputField.on_command(self, command, **kwargs)


class HiddenField(InputField):
    """Skryt� (virtu�ln�) pol��ko. Pol��ko pouze dr�� nastavenou hodnotu."""

    def _init_ctrl(self):
        pass
    
    def _enable(self):
        pass

    def _disable(self, change_appearance):
        pass
    
    def _set_focus(self):
        pass
    
    def _create_ctrl(self):
        return None

    def _enable_event_handlers(self):
        pass
    
    def _disable_event_handlers(self):
        pass

    def _register_skip_navigation_callback(self):
        pass
        
    def _create_label(self):
        return None
    
    def get_value(self):
        """Vra� d��ve ulo�enou hodnotu pol��ka."""
        try:
            return self._value
        except:
            return None
        
    def _set_value(self, value):
        """Nastav hodnotu pol��ka na 'value'."""
        self._value = value
        return True

