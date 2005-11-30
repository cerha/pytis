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

"""Dialogová okna.

Dialogová okna slouží jako nepřehlédnutelná upozornění nebo otázky pro
uživatele.  Uživateli je znemožněno pokračovat práci až do doby, než potvrdí
zobrazené hlášení nebo odpoví na otázku.

Všechny dialogy vychází z abstraktní třídy 'Dialog'.

Modul dále obsahuje několik pomocných funkcí využívajících dialogy pro vícekrát
se vyskytující dialogové operace.

"""

import types
import pytis.data
from pytis.form import *
import config
from wx import calendar
from wx.lib import masked     

class Dialog(KeyHandler, CommandHandler, object):
    """Abstraktní třída, která je základem všech dialogů.

    Všechny dialogy musí být potomky této třídy.  Vytvoření instance dialogu
    ještě neznamená jeho vyvolání, pro to slouží metoda 'run()'.  Metodu
    'run()' lze na jednu instanci volat teoreticky i vícekrát.  Instance
    dialogu však sama o sobě neobsahuje žádné objekty uživatelského rozhraní,
    pouze si pamatuje jejich vlastnosti.  K vytvoření okna a jeho prvků dochází
    až při volání metody 'run()'.
    
    Tato třída pouze definuje abstraktní metodu 'run()'.
    
    """
    def get_command_handler_instance(cls, application):
        return application.top_window()
    get_command_handler_instance = classmethod(get_command_handler_instance)
    
    def __init__(self, parent):
        self._parent = parent
        KeyHandler.__init__(self)
        self._key_guardian = None
        
    def run(self):
        """Vyvolej dialog a počkej na odpověď.

        Vrací: Hodnotu závislou na typu dialogu.

        V této třídě metoda nedělá nic a vždy vrací pravdu.

        """
        return True

class GenericDialog(Dialog):
    """Obecný dialog s tlačítky.

    Univerzální dialogová třída, od které je možno odvodit specializované třídy
    konkrétních dialogů pomocí předefinování některých metod.
    
    """    
        
    def __init__(self, parent, title, buttons, default=None, report=None,
                 report_format=TextFormat.PLAIN):
        """Inicializuj dialog.

        Argumenty:

          parent -- wx rodič; instance 'wx.Frame' nebo 'wx.Dialog'
          title -- titulek dialogového okna jako string
          buttons -- sekvence názvů tlačítek dialogu, strings
          default -- název předvoleného tlačítka (string obsažený v 'buttons',
            nebo 'None')
          report -- Text reportu, který má být zobrazen v okně dialogu.  Jedná
            se o delší text, který bude automaticky scrollovatelný.  Je možné
            zobrazit také komplexní text s HTML či Wiki formátováním.  V
            takovém případě je nutné toto indikovat argumentem 'report_format'.
            Pro vstupní formát platí stejná pravidla, jako v případě třídy
            'InfoWindow'.
          report_format -- konstanta třídy 'TextFormat' určující jak má být
            nakládáno se vstupním textem argumentu 'report'.  V případě, že
            není žádný report specifikován, je tento argument itrelevantní.
            
        """
        assert is_sequence(buttons)
        assert isinstance(title, types.StringTypes)
        assert default is None or default in buttons
        assert report is None or isinstance(report, types.StringTypes)
        assert report_format in public_attributes(TextFormat)
        super_(GenericDialog).__init__(self, parent)
        self._title = unicode(title)
        self._buttons = buttons
        self._default = default
        self._report = report
        self._report_format = report_format
        self._want_focus = None
        self._shown = False
        
    def _create_dialog(self):
        """Vytvoř celý dialog (postupně okno, jeho obsah a tlačítka).
        
        Nejprve je vytvořeno okno dialogu jako takové ('wx.Dialog') a potom
        je zavolána metoda `_create_dialog_elements'.

        Tuto metodu by nemělo být třeba předefinovávat. Ve většině případů by
        mělo stačit předefinovat metodu '_create_content()'.

        """
        style = (wx.CAPTION | wx.CLOSE_BOX | wx.MINIMIZE_BOX |
                 wx.SYSTEM_MENU | wx.STAY_ON_TOP)
        if self._report is not None:
            style |= wx.RESIZE_BORDER
        self._dialog = dialog = wx.Dialog(self._parent, title=self._title,
                                          style=style)
        self._create_dialog_elements(dialog)
        self._handle_keys(dialog)
        

    def _create_dialog_elements(self, dialog):
        """Vlož do dialogu jeho vnitřní prvky.
        
        Pomocí sizerů je do dialogu vložen hlavní obsah (výsledek metody
        '_create_content()') a tlačítka (výsledek metody
        '_create_buttons()').

        Tuto metodu by nemělo být třeba předefinovávat. Ve většině případů by
        mělo stačit předefinovat metodu '_create_content()' nebo
        '_create_buttons()'.

        """
        # vytvoř obsah (vnitřek) dialogu
        content = xtuple(self._create_content())
        # vytvoř tlačítka a poskládej je vedle sebe
        button_sizer = wx.BoxSizer()
        for b in self._create_buttons():
            button_sizer.Add(b, 0, wx.ALL, 8)
            # registruj handlery událostí
            wx_callback(wx.EVT_BUTTON, dialog, b.GetId(), self._on_button)
            self._handle_keys(b)
        # poskládej obsah a tlačítka do top-level sizeru (nad sebe)
        sizer = wx.BoxSizer(wx.VERTICAL)
        if self._report is not None:
            report = wx_text_view(dialog, self._report,
                                  format=self._report_format)
            sizer.Add(report, 1, wx.EXPAND)
        for part in content:
            sizer.Add(part, 0, wx.ALL|wx.CENTER, 5)
        sizer.Add(button_sizer, 0, wx.CENTER)
        wx_callback(wx.EVT_IDLE, self._dialog, self._on_idle)
        # dokonči ...
        sizer.SetSizeHints(dialog)
        dialog.SetAutoLayout(True)
        dialog.SetSizer(sizer)
        sizer.Fit(dialog)

    def _create_content(self):
        """Abstraktní metoda - nutno předefinovat v odvozených třídách.

        Vrací: Libovolný wx objekt, který má být vložen do těla dialogu.
        
        """
        pass
    
    def _create_buttons(self):
        """Vytvoř tlačítka a vrať je jako sekvenci."""
        buttons = []
        self._button_labels = {}
        for label in self._buttons:
            id = wx.NewId()
            self._button_labels[id] = label
            button = wx.Button(self._dialog, id, unicode(label))
            buttons.append(button)
            if self._default == label:
                button.SetDefault()
                self._want_focus = button
        return tuple(buttons)

    def _create_icon(self, artid):
        bitmap = wx.ArtProvider_GetBitmap(artid, wx.ART_MESSAGE_BOX, (48,48))
        if bitmap.Ok():
            return wx.StaticBitmap(self._dialog, -1, bitmap)
        else:
            return None

    def _can_commit(self, widget):
        return widget.GetId() in self._button_labels.keys()

    def _on_idle(self, event):
        event.Skip()
        if self._want_focus is not None:
            self._want_focus.SetFocus()
            self._want_focus = None
        if not self._shown and self._dialog.IsShown():
            self._shown = True
            self._on_show()

    def _on_show(self):
        pass
    
    def _navigate(self):
        nav = wx.NavigationKeyEvent()
        nav.SetDirection(True)        
        nav.SetCurrentFocus(self._dialog)
        self._dialog.GetEventHandler().ProcessEvent(nav)

    def _end_modal(self, result):
        self._dialog.EndModal(result)
        return True
    
    def _on_button(self, event):
        return self._end_modal(event.GetId())
    
    def _button_label(self, id):
        # Vrať nápis tlačítka s daným id.
        try:
            return self._button_labels[id]
        except KeyError:
            return None

    def _button_id(self, label):
        # Vrať id tlačítka s daným nápisem.
        for id in self._button_labels.keys():
            if self._button_labels[id] == label:
                return id
        return None

    def _customize_result(self, result):
        """Vrať návratovou hodnotu podle výsledku ukončeného dialogu.

        V této třídě jednoduše vrací nápis tlačítka, kterým byl dialog ukončen
        ('None' v případě, že byl ukončen jiným způsobem než tlačítkem).

        """
        return self._button_label(result)

    def _run_dialog(self):
        return self._dialog.ShowModal()

    def _close_dialog(self):
        return self._end_modal(wx.ID_CANCEL)
        
    def _commit_dialog(self):
        focused = wx_focused_window()
        if focused and self._can_commit(focused):
            self._end_modal(focused.GetId())
        else:
            self._navigate()
        return True
        
    def on_command(self, command, **kwargs):
        if command == Dialog.COMMAND_CLOSE_DIALOG:
            return self._close_dialog()
        if command == Dialog.COMMAND_COMMIT_DIALOG:
            return self._commit_dialog()
        return False

    def run(self):
        """Zobraz dialog a po jeho ukončení vrať jeho návratovou hodnotu.

        Návratová hodnota závisí na typu dialogu, resp. na jeho metodě
        '_customize_result()'.

        """
        self._create_dialog()
        self._dialog.SetFocus()
        result = self._customize_result(self._run_dialog())
        self._dialog.Destroy()
        return result

    def rebuild(self):
        """Znovu vytvoř obsah dialogu bez jeho uzavření.

        Tato metoda je určena k použití v případech, kdy je během zobrazení
        modálního dialogu nutno změnit jeho obsah, například přidat nebo
        odebrat (nikoliv pouze zapnout nebo vypnout) některé prvky.

        """
        dialog = self._dialog
        if wx is 'working':
            dialog.DestroyChildren()
        else:
            for c in dialog.GetChildren():
                c.Show(False)
                c.Enable(False)
                c.Close()
                c.Destroy()
        assert not dialog.GetChildren()
        self._create_dialog_elements(dialog)
        dialog.SetFocus()


class Message(GenericDialog):
    """Dialog zobrazující zprávu a vracející odpověď.

    Tato třída pouze zobrazuje zprávu a tlačítko pro akceptování dialogu.

    Vrácená hodnota metody 'run()' je jednoduše nápis tlačítka, kterým byl
    dialog ukončen (None v případě, že byl ukončen jiným způsobem než
    tlačítkem).

    """
    ICON_INFO = wx.ART_INFORMATION
    "Ikona pro informativní zprávy (žárovka)"
    ICON_QUESTION =  wx.ART_QUESTION
    "Ikona s otazníkem."
    ICON_WARNING = wx.ART_WARNING
    "Ikona s vykřičníkem."
    ICON_ERROR = wx.ART_ERROR
    "Ikona pro chybové zprávy."
    ICON_TIP = wx.ART_TIP
    "Ikona pro tipy, rady apod."

    BUTTON_OK = _('Ok')
    "Nápis pro potvrzovací tlačítko." 
    BUTTON_CANCEL = _('Zrušit')
    "Nápis pro opouštěcí tlačítko." 
    BUTTON_YES = _('Ano')
    "Nápis pro tlačítko souhlasu." 
    BUTTON_NO = _('Ne')
    "Nápis pro tlačítko nesouhlasu." 
    
    _icons = (ICON_INFO, ICON_QUESTION, ICON_WARNING, ICON_ERROR, ICON_TIP)
    
    def __init__(self, parent, message, icon=ICON_INFO, title=_('Zpráva'),
                 buttons=(BUTTON_OK,), default=_(BUTTON_OK), report=None,
                 report_format=TextFormat.PLAIN):
        """Inicializuj dialog.

        Argumenty:

          'parent', 'buttons', 'title' a 'default' -- jako u 'GenericDialog'.
          'message' -- Zpráva zobrazená v těle dialogu (string).
          'icon' -- Jedna z ICON_* konstant třídy ('ICON_INFO' atd.).
          
        """
        super_(Message).__init__(self, parent, title, buttons,
                                 default=default, report=report, report_format=report_format)
        assert icon in self._icons + (None,)
        if message:
            self._message = unicode(message)
        else:
            self._message = None
        self._icon = icon

    def _create_content(self):
        """Vytvoř obsah - to co bude vyplňovat plochu okna nad tlačítky."""
        sizer = wx.BoxSizer()
        if self._icon is not None:
            icon = self._create_icon(self._icon)
            if icon is not None:
                sizer.Add(icon, 0, wx.ALL|wx.ALIGN_CENTER_VERTICAL, 5)
        t = wx.StaticText(self._dialog, -1, self._message)
        sizer.Add(t, 1, wx.EXPAND|wx.ALL|wx.ALIGN_CENTER_VERTICAL, 12)
        return sizer

    
class Warning(Message):
    """Dialog pro zobrazení varovné zprávy."""

    def __init__(self, parent, message, title=_('Varování')):
        """Inicializuj dialog.

        Argumenty:


          Odpovídají stejným argumentům rodičovské třídy s tím, že následující
          argumenty tato třída definuje vždy napevno:

            icon = 'Message.ICON_WARNING'
            buttons = ('Message.BUTTON_OK',)
            default = 'Message.BUTTON_OK'

        """
        super_(Warning).__init__(self, parent, message, title=title,
                                 icon=Message.ICON_WARNING,
                                 buttons=(Message.BUTTON_OK,),
                                 default=Message.BUTTON_OK)


class Error(Message):
    """Dialog pro zobrazení chybové zprávy."""

    def __init__(self, parent, message, title=_('Chyba')):
        """Inicializuj dialog.
        
        Argumenty:


          Odpovídají stejným argumentům rodičovské třídy s tím, že následující
          argumenty tato třída definuje vždy napevno:

            icon = 'Message.ICON_ERROR'
            buttons = ('Message.BUTTON_OK',)
            default = 'Message.BUTTON_OK'

        """
        super_(Error).__init__(self, parent, message, title=title,
                               icon=Message.ICON_ERROR,
                               buttons=(Message.BUTTON_OK,),
                               default=Message.BUTTON_OK)


class MultiQuestion(Message):
    """Dialog vyžadující odpověď na otázku výběrem z tlačítek.

    Jedná se o jednoduché přizpůsobení třídy message, s přednastavením ikony a
    titulku.
    
    """
    def __init__(self, parent, message, buttons, default=None,
                 title=_("Otázka"), icon=Message.ICON_QUESTION,
                 report=None, report_format=TextFormat.PLAIN):
        super_(MultiQuestion).__init__(self, parent, message, title=title,
                                       buttons=buttons, default=default,
                                       icon=icon, report=report, report_format=report_format)
    

class Question(MultiQuestion):
    """Dialog vyžadující odpověď ano/ne na zobrazenou zprávu (otázku).

    Metoda 'run()' vrací: Pravdu, právě když uživatel odpoví na danou
    otázku kladně - stiskne tlačítko s nápisem 'Message.BUTTON_YES'.

    """
    def __init__(self, parent, message, default=True,
                 title=_("Otázka"), icon=Message.ICON_QUESTION,
                 report=None, report_format=TextFormat.PLAIN):
        """Inicializuj dialog.
        
        Argumenty:

          default -- pokud je pravda, bude předvoleným tlačítkem tlačítko
            'Message.BUTTON_YES'. Jinak je předvolená odpověď
            'Message.BUTTON_NO' (implicitně).
        
          Ostatní argumenty odpovídají stejným argumentům rodičovské třídy s
          tím, že následující argumenty tato třída definuje vždy napevno:

            buttons = ('Message.BUTTON_YES', 'Message.BUTTON_NO')

        Klíčový argument 'default' může být uváděn i bez explicitního
        pojmenování, takže musí být do budoucna zaručeno jeho zachování včetně
        pořadí.
            
        """
        if default:
            default = Message.BUTTON_YES
        else:
            default = Message.BUTTON_NO
        super_(Question).__init__(self, parent, message, title=title,
                                  buttons=(Message.BUTTON_YES,
                                           Message.BUTTON_NO),
                                  default=default, icon=icon,
                                  report=report, report_format=report_format)
        
    def _customize_result(self, result):
        if self._button_label(result) == Message.BUTTON_YES:
            return True
        else:
            return False

        
class InputDialog(Message):
    """Dialog pro zadání textu.

    Dialog kromě zprávy obsahuje i textové vstupní pole a tlačítka 'Ok' a
    'Zrušit'.  Návratovou hodnotou dialogu je zadaný string, byl-li odeslán
    tlačítkem 'OK' či stiskem klávesy Enter, nebo 'None', byl-li dialog opuštěn
    jinak (tlačítko 'Zrušit', klávesa Escape apod.).

    """
    def __init__(self, parent, message=None, value=None, prompt=None,
                 title=_("Zadejte hodnotu"), icon=None, passwd=False,
                 report=None, report_format=TextFormat.PLAIN,
                 input_width=None, input_height=1, allow_empty=True):
        """Inicializuj dialog.

        Argumenty:

          prompt -- výzva připojená před políčko (string) nebo 'None'
          value -- přednastavená hodnota textového vstupního políčka (string)
          passwd -- pokud má pravdivou hodnotu, bude se textové políčko
            chovat joko políčko pro vstup hesla (vepsané znaky budou
            zobrazovány jako hvězdičky)
          input_width -- šířka vstupního prvku ve znacích nebo
            'None' (v kterémžto případě se použije implicitní velikost)
          input_height -- výška vstupního políčka ve znacích
          allow_empty -- pokud je pravda (implicitně ano), je prázdný vstup
            akceptován, jinak dialog vyžaduje zadání nějaké hodnoty

          Klíčové argumenty jsou předány konstruktoru třídy
          wx.pytis.masked.TextCtrl.
          Zbývající argumenty odpovídají stejným argumentům rodičovské třídy
          s tím, že následující argumenty tato třída definuje vždy napevno:

            buttons = ('Message.BUTTON_OK', 'Message.BUTTON_CANCEL')
            default = None

        """
        self._input_height = input_height
        self._input_width = input_width
        super_(InputDialog).__init__(self, parent, message, title=title,
                                     buttons=(Message.BUTTON_OK,
                                              Message.BUTTON_CANCEL),
                                     default=None, icon=icon,
                                     report=report, report_format=report_format)
        assert value is None or isinstance(value, types.StringTypes)
        assert prompt is None or isinstance(prompt, types.StringTypes)
        assert isinstance(allow_empty, types.BooleanType)
        self._value = value and unicode(value)
        self._prompt = unicode(prompt)
        self._passwd = passwd
        self._allow_empty = allow_empty
        self._mask = ''
        self._autoformat = None

    def _create_content(self):
        sizer = wx.BoxSizer()
        if self._prompt:
            prompt = wx.StaticText(self._dialog, -1, self._prompt)
            sizer.Add(prompt, 1, wx.ALL|wx.ALIGN_CENTER_VERTICAL, 3)
        style = self._passwd and wx.TE_PASSWORD or 0
        if self._input_width is None:
            width = wx.DefaultSize.width
        else:
            width = 4*(self._input_width+1)+2
        size = dlg2px(self._parent, width, 8*self._input_height+4)
        if self._input_height > 1:
            style = style | wx.TE_MULTILINE            
        control = masked.TextCtrl(self._dialog, -1, "", style=style,
                                  size=size, emptyInvalid=not self._allow_empty,
                                  mask=self._mask, autoformat=self._autoformat)
        if self._value is not None:
            control.SetValue(self._value)
        self._control = control
        wx_callback(wx.EVT_KILL_FOCUS, self._control,
                    self._on_kill_control_focus)        
        sizer.Add(control, 0, wx.ALL|wx.ALIGN_CENTER_VERTICAL, 3)
        self._handle_keys(control)
        if self._message is not None:
            result = Message._create_content(self), sizer
        else:
            result = sizer
        return result

    def _on_kill_control_focus(self, e):
        if not self._control.IsValid(self._control.GetValue()):
            self._control.SetFocus()
        e.Skip()

    def _customize_result(self, result):
        if self._button_label(result) == Message.BUTTON_OK:
            return self._control.GetValue()
        else:
            return None

        
class Password(InputDialog):
    """Dialog pro zadání hesla.

    Speciální případ dialogu 'InputDialog' určený pro zadávání hesla.
    
    """
    def __init__(self, parent, message=None, title=_("Zadejte heslo"),
                 prompt=_('Heslo:'), icon=None):
        """Inicializuj dialog.

        Argumenty:

          Argumenty odpovídají stejným argumentům rodičovské třídy s tím, že
          argument 'passwd' je nastaven vždy na pravdivou hodnotu.
          
        """
        super_(Password).__init__(self, parent, message=message, title=title,
                                  prompt=prompt, passwd=True, icon=icon)

class Login(InputDialog):
    """Dialog pro zadání uživatelského jména a hesla.

    """
    def __init__(self, parent, message=None, title=_(u"Zadejte heslo"),
                 login='', icon=None, login_prompt=_(u"Uživatelské jméno:"), 
                 passwd_prompt=_(u"Heslo:")):
        """Inicializuj dialog.

        Speciální argumenty:

          login -- předvyplněná hodnota uživatelského jména.
          login_prompt -- výzva pro zadání uživatelského jména.
          passwd_prompt -- výzva pro zadání hesla.
          
        """
        super_(Password).__init__(self, parent, message=message, title=title,
                                  icon=icon)
        self._login_prompt = login_prompt
        self._passwd_prompt = passwd_prompt
        self._value = login
        
    def _create_content(self):
        grid = wx.FlexGridSizer(2, 2, 2, 5)
        login_label  = wx.StaticText(self._dialog, -1, self._login_prompt)
        passwd_label = wx.StaticText(self._dialog, -1, self._passwd_prompt)
        self._login  = wx.TextCtrl(self._dialog, -1, self._value)
        self._passwd = wx.TextCtrl(self._dialog, -1,
                                   style=wx.TE_PASSWORD)
        style = wx.ALIGN_RIGHT|wx.ALIGN_CENTER_VERTICAL
        grid.Add(login_label, 0, style, 2)
        grid.Add(self._login)
        grid.Add(passwd_label, 0, style, 2)
        grid.Add(self._passwd)
        self._handle_keys(self._login, self._passwd)
        if self._value != '':
            self._want_focus = self._passwd
        if self._message is not None:
            return (Message._create_content(self), grid)
        else:
            return grid

    def _customize_result(self, result):
        if self._button_label(result) == Message.BUTTON_OK:
            return (self._login.GetValue(), self._passwd.GetValue())
        else:
            return None


class InputDate(InputDialog):
    """Dialog pro zadání datumu.

    Speciální případ dialogu 'InputDialog' určený pro zadávání datumu.
    
    """    
    def __init__(self, *args, **kwargs):
        kwargs['allow_empty'] = kwargs.get('allow_empty', False)
        super_(InputDate).__init__(self, *args, **kwargs)
        import config
        format = config.date_format.lower().replace('%d','dd')
        format = format.replace('%m','mm').replace('%y','yyyy')
        for tag in masked.masktags.keys():
            if masked.masktags[tag]['description'].lower() == format:
                self._autoformat = tag
                break
        else:
            self._autoformat = 'EUDATEDDMMYYYY/'

    def _customize_result(self, result):
        if self._button_label(result) == Message.BUTTON_OK:
            if not self._control.IsValid(self._control.GetValue()):
                return pytis.data.Value(pytis.data.Date(), None)
            value, error  = pytis.data.Date().validate(self._control.GetValue())
            if error:
                raise ProgramError("Chyba validace vstupu!")                
            return value
        else:
            return pytis.data.Value(pytis.data.Date(), None)


class InputNumeric(InputDialog):
    """Dialog pro zadávání čísel.

    Speciální případ dialogu 'InputDialog' určený pro zadávání čísel.
    
    """
  
    def __init__(self, parent, message=None, value=None, prompt=None,
                 title=_("Zadejte hodnotu"), integer_width=10,
                 allow_empty=False, decimal_width=0,
                 min_value=None, max_value=None, allow_negative=True,
                 select_on_entry=False, signed_colour="Red"
                 ):
        super_(InputNumeric).__init__(self, parent, message=message,
                                      value=None, prompt=prompt,
                                      title=title, 
                                      allow_empty=allow_empty)
        self._decimal_width = decimal_width
        self._min_value = min_value
        self._max_value = max_value
        self._allow_negative = allow_negative
        self._select_on_entry = select_on_entry
        self._signed_colour = signed_colour
        self._value = value
        self._integer_width = integer_width
        # Zjištění desetinného oddělovače a oddělovače tísíců
        import locale
        self._decimal_point = locale.localeconv()['decimal_point']
        self._thousands_sep = locale.localeconv()['thousands_sep']
        self._limited = not (self._min_value is None and self._max_value is None)

    def _create_content(self):
        sizer = wx.BoxSizer()
        if self._prompt:
            prompt = wx.StaticText(self._dialog, -1, self._prompt)
            sizer.Add(prompt, 1, wx.ALL|wx.ALIGN_CENTER_VERTICAL, 3)           
        control = masked.NumCtrl(self._dialog, -1, 0,
                                 allowNegative=self._allow_negative,
                                 allowNone=self._allow_empty,
                                 selectOnEntry=self._select_on_entry,
                                 integerWidth=self._integer_width,
                                 fractionWidth=self._decimal_width,
                                 groupChar=self._thousands_sep,
                                 groupDigits=True,
                                 decimalChar=self._decimal_point,
                                 signedForegroundColour=self._signed_colour
                                 )
        if self._value is not None:
            control.SetValue(self._value)
        if self._min_value:
            control.SetMin(self._min_value)
        if self._max_value:
            control.SetMax(self._max_value)
        if self._limited:
            control.SetLimited(True)            
        self._control = control
        wx_callback(wx.EVT_KILL_FOCUS, self._control,
                    self._on_kill_control_focus)        
        sizer.Add(control, 0, wx.ALL|wx.ALIGN_CENTER_VERTICAL, 3)
        self._handle_keys(control)
        if self._message is not None:
            result = Message._create_content(self), sizer
        else:
            result = sizer
        return result
        
    def _on_kill_control_focus(self, e):
        if not self._control.IsInBounds(self._control.GetValue()):
            beep()
            self._control.SetFocus()
        e.Skip()
        
    def _customize_result(self, result):
        if self._button_label(result) == Message.BUTTON_OK:
            if not self._control.IsInBounds(self._control.GetValue()):
                if self._decimal_width == 0:
                    return pytis.data.Value(pytis.data.Integer(), None)
                else:
                    return pytis.data.Value(pytis.data.Float(), None)
            if self._decimal_width == 0:
                value = pytis.data.Value(pytis.data.Integer(),
                                       self._control.GetValue())
            else:    
                value = pytis.data.Value(pytis.data.Float(),
                                       self._control.GetValue())
            return value
        else:
            if self._decimal_width == 0:
                return pytis.data.Value(pytis.data.Integer(), None)
            else:
                return pytis.data.Value(pytis.data.Float(), None)



class RunFormDialog(InputDialog):
    """Dialog pro spuštění formuláře.

    Umožní uživateli vybrat třídu formuláře a zadat název specifikace.  Ty
    potom vrátí v tuplu jako výsledek volání své metody 'run()'.
    
    """
    _BROWSE_FORM = "BrowseForm"
    _EDIT_FORM = "EditForm"
    _BROWSE_DUAL_FORM = "BrowseDualForm"
    _CODEBOOK_FORM = "CodebookForm"

    def __init__(self, parent, title=_("Zobrazit formulář")):
        """Inicializuj dialog.

        Argumenty:

          Argumenty odpovídají stejným argumentům rodičovské třídy.
          
        """
        super_(RunFormDialog).__init__(self, parent, message=None,
                                       title=title, input_width=25,
                                       prompt=_("Název specifikace:"),
                                       icon=self.ICON_TIP)
        self._FORM_CLASS_MAPPING = {
            self._BROWSE_DUAL_FORM: pytis.form.BrowseDualForm,
            self._BROWSE_FORM: BrowseForm,
            self._EDIT_FORM: PopupEditForm,
            self._CODEBOOK_FORM: CodebookForm,
        }


    def _create_content(self):
        base_content = InputDialog._create_content(self)
        sizer = wx.BoxSizer()
        label = wx.StaticText(self._dialog, -1, _("Třída formuláře:"))
        sizer.Add(label, 1, wx.ALL|wx.ALIGN_CENTER_VERTICAL, 3)
        ch = [self._BROWSE_FORM, self._EDIT_FORM, self._BROWSE_DUAL_FORM,
              self._CODEBOOK_FORM]
        control = wx.Choice(self._dialog, -1, (-1,-1), (-1,-1), choices=ch)
        control.SetSelection(0)
        self._form_class_choice = control
        sizer.Add(control, 0, wx.ALL|wx.ALIGN_CENTER_VERTICAL, 3)
        self._handle_keys(control)
        return (base_content, sizer)

    def _customize_result(self, result):
        if self._button_label(result) == Message.BUTTON_OK:
            selection = self._form_class_choice.GetStringSelection()
            selection = self._form_class_choice.GetStringSelection()
            return (self._FORM_CLASS_MAPPING[selection],
                    self._control.GetValue())
        else:
            return None

        
class OperationDialog(Message):
    """Dialog pro spuštění dlouhotrvající operace.
    
    Spuštěním dialogu metodou 'run()' bude zobrazen modální dialog a spuštěna
    funkce zadaná v konstruktoru.  Dialog je ukončen automaticky po skončení
    funkce.  Do té doby uživatel nemůže dělat nic, než čekat...
    
    """
    def __init__(self, parent, function, args=(), kwargs={},
                 title=_("Provádí se operace"),
                 message=_("Čekejte prosím...")):
        """Inicializuj dialog.

        Argumenty:

          parent, title, message -- odpovídají stejným argumentům rodič. třídy.
          function -- funkce, která má být spuštěna.
          args -- argumenty spouštěné funkce jako tuple.
          kwargs -- klíčové argumenty spouštěné funkce jako dictionary.
          
        """
        super_(OperationDialog).__init__(self, parent, message=message,
                                         title=title, icon=self.ICON_TIP,
                                         buttons=(), default=None)
        assert callable(function)
        assert is_sequence(args)
        assert is_dictionary(kwargs)
        self._function = function
        self._args = args
        self._kwargs = kwargs

    def _on_show(self):
        self._result = self._function(*self._args, **self._kwargs)
        wx_yield_(full=True)
        self._end_modal(wx.ID_OK)
        
    def _customize_result(self, result):
        return self._result


class ProgressDialog(OperationDialog):
    """Dialog pro spuštění dlouhotrvající operace s ProgressBarem.
    
    Spuštěním dialogu metodou 'run()' bude zobrazen modální dialog a spuštěna
    funkce zadaná v konstruktoru s argumenty předanými konstruktoru.

    Navíc bude funkci vždy předán první argument, kterým je funkce
    aktualizující stav progress baru.  Za její volání v průběhu operace je
    funkce vykonávající operaci zodpovědná.  Aktualizační funkce vyžaduje jeden
    argument, kterým je stav operace v procentech (integer).  Aktualizační
    funkce také vrací nepravdivou hodnotu, pokud má být operace ukončena
    (uživatelské přerušení, je-li povoleno).  Za ukončení je však opět
    zodpovědná funkce vykonávající operaci.  Druhým nepovinným argumentem je
    klíčový argument 'newmsg'.  Pokud je předán neprázdný řetězec, bude také
    aktualizována zpráva zobrazená na dialogu (tato zpráva nahradí původní
    zprávu zadanou v konstruktoru).

    Po ukončení dialogu (ať už z důvodu uživatelského přerušení, či dokončení
    operace) je vrácena návratová hodnota funkce vykonávající operaci.

    """
    def __init__(self, parent, function, args=(), kwargs={},
                 title=_("Provádí se operace"), message=_("Čekejte prosím..."),
                 elapsed_time=False, estimated_time=False,
                 remaining_time=False, can_abort=False):
        """Inicializuj dialog.

        Argumenty:

          parent, function, args, kwargs, message, title -- stejné, jako u
            rodičovské třídy, pouze 'function' musí navíc přijímat odkaz na
            aktualizační funkci jako první argument (viz dokumentace třídy).
          elapsed_time -- Pokud je 'True', zobrazí se uběhlý čas
          estimated_time -- Pokud je 'True', zobrazí se předpokládaný čas
          remaining_time -- Pokud je 'True', zobrazí se zbývající čas
          can_abort -- Pokud je 'True', bude možno vykonávání funkce přerušit,
            pokud to funkce vykonávající operaci umožňuje (viz. docstring
            třídy).
          
        """

        super_(ProgressDialog).__init__(self, parent, function, args=args,
                                        kwargs=kwargs, message=message,
                                        title=title)
        style = wx.PD_APP_MODAL
        if elapsed_time:
            style = style|wx.PD_ELAPSED_TIME
        if estimated_time:
            style = style|wx.PD_ESTIMATED_TIME
        if remaining_time:
            style = style|wx.PD_REMAINING_TIME
        if can_abort:
            style = style|wx.PD_CAN_ABORT
        self._style = style

    def _create_dialog(self):
        self._dialog = wx.ProgressDialog(self._title, unicode(self._message),
                                         maximum=100, parent=self._parent,
                                         style=self._style)

    def _run_dialog(self):
        return self._function(lambda n, newmsg='':
                              self._dialog.Update(n, newmsg=newmsg),
                              *self._args, **self._kwargs)

    def _customize_result(self, result):
        return result


class RepeatedOperationDialog(ProgressDialog):
    """Dialog pro opakované spouštění operace nad seznamem argumentů.

    Tento dialog je speciálním případem použití 'ProgressDialog' pro cyklické
    spouštění operace nad seznamem argumentů.  Uživatel pouze nemusí psát
    funkci provádějící cyklus a aktualizující ProgressBar.
    
    """
    def __init__(self, parent, function, args=(), step=None, **kwargs):
        """Inicializuj dialog.

        Argumenty:

          function --
          args --
          kwargs --
          step -- celé číslo, udávající počet procent, po kterých je
            progressbar aktualizován.  Pokud je step

          Ostatní argumenty jsou shodné jako u rodičovské třídy.
          
        """
        assert step is None or \
               isinstance(step, types.IntType) and step in range(1, 100)
        def do(update, *args_list):
            total = len(args_list)
            last_status = 0
            for n, arg in enumerate(args_list):
                status = int(float(n)/total*100)
                if step is None or status/step != last_status/step:
                    last_status = status
                    try:
                        msg = self._message % arg
                    except TypeError:
                        msg = ''
                    if not update(status, newmsg=msg):
                        break
                function(arg)
            
        super_(RepeatedOperationDialog).__init__(self, parent, do, args=args,
                                                 **kwargs)
    
    
    
class Calendar(GenericDialog):
    """Dialog zobrazující kalendář, umožňující výběr dne.

    Datum na kalendáři může být přednastaven parametrem konstruktoru. Metoda
    'run()' vrací vybraný datum jako instanci 'mx.mxDateTime', nebo None, pokud
    byl dialog opuštěn.
    
    """

    def __init__(self, parent, date, title=_("Kalendář"),
                 enable_year=True, enable_month=True, monday_first=True):
        """Inicializuj dialog.

        Argumenty:

          parent -- wx rodič; instance 'wx.Frame' nebo 'wx.Dialog'
          date -- přednastavený datum jako instance 'mxDateTime'.
          title -- titulek dialogového okna jako string
          enable_year -- když je pravda, zobrazí výběr roku; boolean
          enable_month -- když je pravda, zobrazí výběr měsíce; boolean
          monday_first -- když je pravda, bude pondělí prvním dnem v týdnu;
            boolean

        Pokud argument date neobsahuje řetězec, který je možné zpracovat pomocí
        'wx.DateTime.ParseDate()', bude datum nastaven na dnešní datum. 

        """
        super_(Calendar).__init__(self, parent, title=title,
                                  buttons=(Message.BUTTON_OK,
                                           Message.BUTTON_CANCEL))
        # vytvoř kalendář
        style = wx.DIALOG_MODAL| \
                calendar.CAL_SHOW_HOLIDAYS| \
                calendar.CAL_SHOW_SURROUNDING_WEEKS 
        if not enable_year:  style = style | calendar.CAL_NO_YEAR_CHANGE
        if not enable_month: style = style | calendar.CAL_NO_MONTH_CHANGE
        if monday_first:     style = style | calendar.CAL_MONDAY_FIRST
        else:                style = style | calendar.CAL_SUNDAY_FIRST
        self._style = style
        from mx import DateTime as DT
        if date is None:
            self._date = DT.now()
        else:
            assert type(date) == type(DT.DateTimeFrom('2001-01-01'))
            self._date = date
        
    def _create_content(self):
        cal = calendar.CalendarCtrl(self._dialog, -1, style=self._style)
        if wx.MAJOR_VERSION == 2 and wx.MINOR_VERSION < 6:
            size = cal.GetSize()
            cal.SetMinSize((size.GetWidth()+150, size.GetHeight()))
        wx_date = wx.DateTime()
        if wx_date.ParseDate(str(self._date.date)) is None:
            wx_date = wx.DateTime_Today()
        wx_callback(calendar.EVT_CALENDAR, cal, cal.GetId(), self._on_calendar)
        self._handle_keys(cal)
        cal.SetDate(wx_date)
        self._cal = cal
        self._want_focus = cal
        return cal

    def _can_commit(self, widget):
        return super(Calendar, self)._can_commit(widget) or widget == self._cal
    
    def _customize_result(self, result):
        if result == self._cal.GetId() \
               or self._button_label(result) == Message.BUTTON_OK:
            from mx import DateTime as DT
            return DT.DateTimeFrom(str(self._cal.GetDate().FormatISODate()))
        return None

    def _on_calendar(self, event):
        return self._end_modal(self._button_id(Message.BUTTON_OK))

    
class ColorSelector(GenericDialog):
    """Dialog umožňující výběr barvy.

    Výchozí barva může být přednastavena parametrem konstruktoru.  Metoda
    'run()' vrací barvu jako řetězec '#RRGGBB', nebo None, pokud byl dialog
    opuštěn.
    
    """

    def __init__(self, parent, color=None, title=_("Výběr barvy")):
        """Inicializuj dialog.

        Argumenty:

          parent -- wx rodič; instance 'wx.Frame' nebo 'wx.Dialog'
          color -- přednastavená barva, jako řetězec '#RRGGBB'.
          title -- titulek dialogového okna jako string

        """
        super_(ColorSelector).__init__(self, parent, title=title, buttons=())
        assert isinstance(color, types.StringTypes) or color is None
        self._color = color
        
    def _create_dialog(self):
        data = None
        if self._color is not None:
            data = wx.ColourData()
            data.SetColour(self._color)
        self._dialog = dialog = wx.ColourDialog(self._parent, data)
        self._handle_keys(dialog)

    def _customize_result(self, result):
        if result == wx.ID_OK:
            c = self._dialog.GetColourData().GetColour()
            return '#%02x%02x%02x' % (c.Red(), c.Green(), c.Blue())
        return None


class BugReport(GenericDialog):
    """Dialog pro zobrazení neočekávané výjimky.

    Dialog nabízí uživateli možnost výběru reakce na výjimku, včetně možnosti
    odeslání oznámení o chybě.

    Dialog vrací jednu z následujících hodnot:

      None -- požaduje-li uživatel ukončení aplikace
      prázdný string -- požaduje-li uživatel chybu ignorovat
      neprázdný string -- požaduje-li uživatel poslat oznámení o chybě s textem
        stringu
    
    """
    # Existuje sice wxPython.pytis.ErrorDialogs, ale to vypadá jako těžký a
    # nepříliš funkční hack.

    _IGNORE_LABEL = _("Ignorovat")
    _REPORT_LABEL = _("Poslat oznámení o chybě")
    _EXIT_LABEL = _("Ukončit aplikaci")
    
    def __init__(self, parent, einfo):
        """Inicializuj instanci.

        Argumenty:

          parent -- wx rodič; instance 'wx.Frame' nebo 'wx.Dialog'
          einfo -- informace o výjimce ve tvaru vraceném funkcí
            'sys.exc_info()'

        """
        super_(BugReport).__init__(self, parent, _("Neočekávaná chyba"),
                                   buttons=(self._IGNORE_LABEL,
                                            self._REPORT_LABEL,
                                            self._EXIT_LABEL),
                                   default=self._IGNORE_LABEL)
        self._einfo = einfo

    def _create_content(self):
        self._sizer = sizer = wx.BoxSizer(wx.VERTICAL)
        label = wx.StaticText(self._dialog, -1, _("Nevyšlo to"))
        font = wx.Font(18, wx.DEFAULT, wx.NORMAL, wx.BOLD,
                         encoding=wx.FONTENCODING_DEFAULT)
        label.SetFont(font)
        icon = self._create_icon(Message.ICON_ERROR)
        if icon is not None:
            s2 = wx.BoxSizer(wx.HORIZONTAL)
            s2.Add(label, 1, wx.ALL|wx.EXPAND|wx.CENTER, 10)
            s2.Add(icon, 0, wx.BOTTOM, 5)
            sizer.Add(s2, 0, wx.EXPAND|wx.ALL)
        else:
            sizer.Add(label, 0, wx.ALL, 5)
        # store the traceback text    
        import config
        if 0:
            # Fancy HTML traceback
            import cgitb
            from wx import html
            display = html.HtmlWindow(self._dialog, -1)
            text = "<html>"+cgitb.html(self._einfo)+"</html>"
            step = 3000
            pointer = 0
            while (pointer < len(text)):
                display.AppendToPage(text[pointer:min(pointer+step,len(text))])
                pointer += step
                display.SetSize(char2px(display, 140, 35))
            #display.SetFonts('Arial', 'Fixed', sizes=(6,7,8,9,10,11,12))
        else:
            style = wx.TE_MULTILINE|wx.TE_DONTWRAP #|wx.TE_READONLY
            display = wx.TextCtrl(self._dialog, -1, style=style,
                                  size=wx.Size(600, 400))
            for line in exception_info(self._einfo).splitlines():
                # Příliš "dlouhý" text se nemusí povést do políčka vložit...
                display.AppendText(line+'\n')
            font = wx.Font(display.GetFont().GetPointSize(),
                           wx.MODERN, wx.NORMAL, wx.NORMAL)
            display.SetFont(font)
        sizer.Add(display, 0, wx.EXPAND|wx.CENTER)
        self._display = display
        self._want_focus = display
        return sizer

    def _customize_result(self, result):
        label = self._button_label(result)
        if label == self._EXIT_LABEL:
            result = None
        elif label == self._IGNORE_LABEL:
            result = ''
        elif label == self._REPORT_LABEL:
            if isinstance(self._display, wx.TextCtrl):
                result = self._display.GetValue()
            else:
                result = exception_info(self._einfo)
        else:
            raise ProgramError('Unknown BugReport dialog result', label)
        return result

    def _close_dialog(self, command, **kwargs):
        self._end_modal(self._button_id(self._IGNORE_LABEL))

        
class FileDialog(Dialog):
    """Dialog pro výběr souboru.

    Zobrazí dialog s možností procházení adresářů a výběru souboru.

    """
    OPEN = 'OPEN'
    """Konstanta určující dialog pro otevření existujícího souboru."""  
    SAVE = 'SAVE'
    """Konstanta určující dialog pro zadání jména souboru pro uložení."""  

    _last_directory = {}

    def __init__(self, parent, title=None, dir=None, file=None, mode=OPEN,
                 wildcards=(_("Všechny soubory")+" (*.*)|*.*",),
                 multi=False, overwrite_prompt=True):
        """Inicializuj dialog.

        Argumenty:

          parent -- wx rodič; instance 'wx.Frame' nebo 'wx.Dialog'
          title -- titulek dialogového okna jako string; pokud je None, bude
            doplněn výchozí titulek v závislosti na argumentu 'mode'.
          dir -- přednastavená cesta; řetězec, nebo None.
          file -- přednastavený název souboru; řetězec, nebo None.
          mode -- typ dialogu; jedna z konstant 'OPEN' a 'CLOSE' třídy.
          wildcards -- seznam masek souborů a popisů, podle kterých bude možno
            filtrovat; jedná se o sekvenci, kde každý lichý prvek určuje popis
            a každý sudý prvek je wildcard řetězcem, podle kterého budou
            soubory filtrovány, pokud je zvolen; výchozí filtrování je podle
            první dvojice. příklad: ("BMP soubory (*.bmp)", "*.bmp",
                                     "GIF soubory (*.gif)", "*.gif")
          multi -- pokud je pravda, bude možno vybrat více souborů najednou;
            relevantní poouze pro 'mode'='OPEN'.
          overwrite_prompt -- pokud je pravda, bude při výběru existujícího
            souboru pro ukládání zobrazena otázka, zda má být soubor přepsán;
            relevantní poouze pro 'mode'='SAVE'; pokud je pravda, bude
            návratovou hodnotou metody 'run()' tuple.

        """
        super_(FileDialog).__init__(self, parent)
        assert mode in (FileDialog.OPEN, FileDialog.SAVE)
        if title is None:
            title = {FileDialog.OPEN: _("Otevřít soubor"),
                     FileDialog.SAVE: _("Uložit soubor")}[mode]
        assert dir is None or isinstance(dir, types.StringTypes)
        assert file is None or isinstance(file, types.StringTypes)
        self._title = unicode(title)
        self._dir = dir
        self._file = file
        self._mode = mode
        self._wildcards = wildcards
        self._multi = multi
        self._overwrite_prompt = overwrite_prompt

    def on_command(self, command, **kwargs):
        if command == Dialog.COMMAND_CLOSE_DIALOG:
            return self._end_modal(wx.ID_CANCEL)
        return False

    def run(self):
        """Zobraz dialog a vrať cestu k vybranému souboru jeko řetězec.

        Pokud je argument konstruktoru 'multi' pravdivý, bude vrácen tuple
        řetězců.

        """
        dir = self._dir or FileDialog._last_directory.get(self._mode, '')
        file = self._file or ''
        style = {FileDialog.OPEN: wx.OPEN,
                 FileDialog.SAVE: wx.SAVE}[self._mode]
        if self._multi and self._mode == FileDialog.OPEN:
            style = style | wx.MULTIPLE
        if self._overwrite_prompt and self._mode == FileDialog.SAVE:
            style = style | wx.OVERWRITE_PROMPT
        self._dialog = d = wx.FileDialog(self._parent,
                                         message=self._title,
                                         defaultDir=dir, defaultFile=file,
                                         wildcard='|'.join(self._wildcards),
                                         style=style)
        result = d.ShowModal()
        FileDialog._last_directory[self._mode] = d.GetDirectory()
        if self._multi:
            path = tuple(d.GetPaths())
        else:
            path = d.GetPath()
        d.Destroy()
        if result == wx.ID_OK:
            return path
        else:
            return None


# Pomocné funkce využívající dialogy


def db_operation(operation, quiet=False):
    """Proveď databázovou 'operation' s ošetřením případných chyb.

    Funkce volá funkci 'operation' s odchytáváním výjimek typu
    'pytis.data.dbdata.DBException' (a jen těchto výjimek).  Dojde-li k výjimce,
    funkce zobrazí uživateli dialog s (pokud možno) lidsky srozumitelnou
    zprávou a zeptá se jej, zda má zkusit operaci zopakovat.  Postup se opakuje
    tak dlouho, dokud uživatel na tuto otázku odpovídá kladně.  U výjimek typu
    'DBLoginException' je uživatel dotázán na heslo, které je před
    zopakováním operace nastaveno.

    Dojde-li k úspěšnému provedení 'operation', ať už na první pokus nebo
    později, je vrácen její výsledek.

    Argumenty:

      operation -- funkce bez argumentů, provádějící databázovou operaci a
        vracející její výsledek; může to být též tuple o dvou nebo třech
        prvcích odpovídajících argumentům funkce 'apply'
      quiet -- právě když je pravda, nezobrazují se při chybě žádné dialogy
        kromě přihlašovacího dialogu (je-li třeba) a dojde ihned k návratu

    Vrací: Dvojici (SUCCESS, RESULT), kde SUCCESS je flag indikující zda
    databázová operace v konečném výsledku uspěla (pravda) nebo ne (nepravda) a
    RESULT je výsledek volání 'operation' (je-li SUCCESS nepravda, není hodnota
    RESULT specifikována).

    """
    FAILURE = None, False
    while True:
        try:
            if type(operation) == type(()):
                result = apply(apply, operation) # wow!
            else:
                result = operation()
            success = True
            break
        except pytis.data.DataAccessException, e:
            run_dialog(Error, _("Přístup odmítnut"))
            return FAILURE
        except pytis.data.DBLoginException, e:
            import config
            prompt = _("Zadejte heslo pro přístup do databáze")
            login, password = run_dialog(Login, prompt, login=config.dbuser)
            if password == None:
                return FAILURE
            config.dbconnection = \
                   config.dbconnection.modified(user=login, password=password)
        except pytis.data.DBException, e:
            log(OPERATIONAL, "Databázová chyba v db_operation",
                format_traceback())
            if quiet:
                return FAILURE
            else:
                message = e.message()
                if e.exception():
                    message += '\n' + str(e.exception())
                message += '\n' + _("Zkusit znovu?")
                if not run_dialog(Question, message, 
                                  title='Databázová chyba',
                                  icon=Question.ICON_ERROR):
                    return FAILURE
    return success, result


def delete_record_question(msg=None):
    """Zeptej se uživatele, zda má být opravdu smazán záznam.

    Vrať pravdu, právě když uživatel odpoví kladně.
    
    """
    log(EVENT, 'Dialog mazání řádku')
    if msg == None:
        msg = _("Opravdu chcete záznam zcela vymazat?")        
    if not run_dialog(Question, msg):
        log(EVENT, 'Mazání řádku uživatelem zamítnuto')
        return False
    log(EVENT, 'Mazání řádku uživatelem potvrzeno')
    return True
