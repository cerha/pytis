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

"""Dialogová okna.

Dialogová okna slouží jako nepřehlédnutelná upozornění nebo otázky pro
uživatele.  Uživateli je znemožněno pokračovat práci až do doby, než potvrdí
zobrazené hlášení nebo odpoví na otázku.

Všechny dialogy vychází z abstraktní třídy 'Dialog'.

Modul dále obsahuje několik pomocných funkcí využívajících dialogy pro vícekrát
se vyskytující dialogové operace.

"""

import collections
import types
import pytis.data
from pytis.form import *
import config
from wx import calendar
from wx.lib import masked
import wx.lib.mixins.listctrl

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
    def _get_command_handler_instance(cls):
        return top_window()
    _get_command_handler_instance = classmethod(_get_command_handler_instance)
    
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
    _COMMIT_BUTTON = None
    _HELP_TOPIC = 'dialog'
    _STYLE = wx.CAPTION | wx.CLOSE_BOX | wx.MINIMIZE_BOX | wx.SYSTEM_MENU
    
    def __init__(self, parent, title, buttons, default=None, report=None,
                 report_format=TextFormat.PLAIN, report_size=(None, None)):
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
            není žádný report specifikován, je tento argument irelevantní.
          report_size -- report window size as a pair of integers (width,
            height) in characters.  If any of the numbers is 'None' given size
            will be will automatically accommodate to the size of the contents
            (for plain text) or use a default value.
            
        """
        assert isinstance(title, basestring), title
        assert isinstance(buttons, (list, tuple)), buttons
        assert default is None or default in buttons, default
        assert report is None or isinstance(report, basestring), report
        assert report_format in public_attributes(TextFormat), report_format
        assert isinstance(report_size, (list, tuple)) and len(report_size) == 2, report_size
        super_(GenericDialog).__init__(self, parent)
        self._title = unicode(title)
        self._button_labels = buttons
        self._default = default
        self._report = report
        self._report_format = report_format
        self._report_size = report_size
        self._want_focus = None
        self._shown = False

    def _create_dialog(self):
        """Vytvoř celý dialog (postupně okno, jeho obsah a tlačítka).
        
        Nejprve je vytvořeno okno dialogu jako takové ('wx.Dialog') a potom
        je zavolána metoda `_create_dialog_elements'.

        Tuto metodu by nemělo být třeba předefinovávat. Ve většině případů by
        mělo stačit předefinovat metodu '_create_content()'.

        """
        style = self._STYLE
        if self._report is not None:
            style |= wx.RESIZE_BORDER
        self._dialog = dialog = wx.Dialog(self._parent, title=self._title, style=style)
        self._create_dialog_elements(dialog)
        self._handle_keys(dialog)

    def _create_dialog_elements(self, dialog):
        """Vlož do dialogu jeho vnitřní prvky.
        
        Pomocí sizerů je do dialogu vložen hlavní obsah (výsledek metody
        '_create_content()') a tlačítka (výsledek metody '_create_buttons()').

        Tuto metodu by nemělo být třeba předefinovávat. Ve většině případů by
        mělo stačit předefinovat metodu '_create_content()' a/nebo
        '_create_buttons()'.

        """
        sizer = wx.BoxSizer(wx.VERTICAL)
        self._create_content(sizer)
        # vytvoř tlačítka a poskládej je vedle sebe
        button_sizer = wx.BoxSizer()
        for b in self._create_buttons():
            button_sizer.Add(b, 0, wx.ALL, 8)
            # registruj handlery událostí
            wx_callback(wx.EVT_BUTTON, dialog, b.GetId(), self._on_button)
            self._handle_keys(b)
        # poskládej obsah a tlačítka do top-level sizeru (nad sebe)
        if self._report is not None:
            report = wx_text_view(dialog, self._report,
                                  format=self._report_format,
                                  width=self._report_size[0], height=self._report_size[1])
            sizer.Add(report, 1, wx.EXPAND)
        sizer.Add(button_sizer, 0, wx.CENTER)
        wx_callback(wx.EVT_IDLE, self._dialog, self._on_idle)
        # dokonči ...
        sizer.SetSizeHints(dialog)
        dialog.SetAutoLayout(True)
        dialog.SetSizer(sizer)
        sizer.Fit(dialog)

    def _create_content(self, sizer):
        """Create the main dialog content and add it to the top level sizer.

        This method must be defined by all derived classes.  The base class
        implementation does nothing.
        
        """
        pass
    
    def _create_buttons(self):
        """Create dialog buttons and return them as a sequence of wx widgets."""
        self._buttons = []
        self._button_label_dict = {}
        for label in self._button_labels:
            id = wx.NewId()
            self._button_label_dict[id] = label
            button = wx.Button(self._dialog, id, unicode(label))
            self._buttons.append(button)
            if self._default == label:
                button.SetDefault()
                self._want_focus = button
        return self._buttons

    def _create_icon(self, artid):
        bitmap = wx.ArtProvider_GetBitmap(artid, wx.ART_MESSAGE_BOX, (48,48))
        if bitmap.Ok():
            return wx.StaticBitmap(self._dialog, -1, bitmap)
        else:
            return None

    def _can_commit(self, widget):
        # Override to allow certain widgets to commit the whole dialog, when
        # COMMIT_DIALOG command is invoked (from the keyboard).
        return False

    def _on_idle(self, event):
        event.Skip()
        if self._dialog.IsShown():
            if self._want_focus is not None:
                self._want_focus.SetFocus()
                self._want_focus.SetFocusFromKbd()
                self._want_focus = None
            if not self._shown:
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
    
    def _on_button(self, event):
        self._end_modal(event.GetId())
    
    def _button_label(self, id):
        # Vrať nápis tlačítka s daným id.
        try:
            return self._button_label_dict[id]
        except KeyError:
            return None

    def _button_id(self, label):
        # Vrať id tlačítka s daným nápisem.
        for id, l in self._button_label_dict.items():
            if l == label:
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

    def _cmd_commit_dialog(self, force=False):
        if force and self._COMMIT_BUTTON is not None:
            id = self._button_id(self._COMMIT_BUTTON)
            widget = wx.FindWindowById(id, self._parent)
        else:
            widget = wx_focused_window()
        if widget in self._buttons:
            # Simulate a click on the commit button.
            widget.Command(wx.CommandEvent(wx.wxEVT_COMMAND_BUTTON_CLICKED,
                                           widget.GetId()))
        elif widget and self._can_commit(widget):
            self._end_modal(widget.GetId())
        else:
            self._navigate()
        
    def _cmd_close_dialog(self):
        self._end_modal(wx.ID_CANCEL)
        
    def _cmd_help(self):
        help(topic=self._HELP_TOPIC)

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

    def focus(self):
        self._dialog.SetFocus()
        
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
    ICON_QUIT = wx.ART_QUIT
    "Ikona opuštění aplikace."

    BUTTON_OK = _(u"Ok")
    "Nápis pro potvrzovací tlačítko." 
    BUTTON_CANCEL = _(u"Zrušit")
    "Nápis pro opouštěcí tlačítko." 
    BUTTON_YES = _(u"Ano")
    "Nápis pro tlačítko souhlasu." 
    BUTTON_NO = _(u"Ne")
    "Nápis pro tlačítko nesouhlasu." 
    
    _icons = (ICON_INFO, ICON_QUESTION, ICON_WARNING, ICON_ERROR, ICON_TIP, ICON_QUIT)
    
    def __init__(self, parent, message, icon=ICON_INFO, title=_(u"Zpráva"),
                 buttons=(BUTTON_OK,), default=_(BUTTON_OK), **kwargs):
        """Inicializuj dialog.

        Argumenty:

          'parent', 'buttons', 'title' a 'default' -- jako u 'GenericDialog'.
          'message' -- Zpráva zobrazená v těle dialogu (string).
          'icon' -- Jedna z ICON_* konstant třídy ('ICON_INFO' atd.).
          
        """
        super_(Message).__init__(self, parent, title, buttons,
                                 default=default, **kwargs)
        assert icon in self._icons + (None,)
        if message:
            self._message = unicode(message)
        else:
            self._message = None
        self._icon = icon

    def _create_content(self, sizer):
        """Vytvoř obsah - to co bude vyplňovat plochu okna nad tlačítky."""
        message = wx.StaticText(self._dialog, -1, self._message)
        icon = self._icon and self._create_icon(self._icon)
        if icon is not None:
            content = wx.BoxSizer()
            content.Add(icon, 0, wx.ALL|wx.ALIGN_CENTER_VERTICAL, 5)
            content.Add(message, 1, wx.EXPAND|wx.ALL|wx.ALIGN_CENTER_VERTICAL, 12)
        else:
            content = message
        sizer.Add(content, 0, wx.ALL|wx.CENTER, 5)

    
class Warning(Message):
    """Dialog pro zobrazení varovné zprávy."""

    def __init__(self, parent, message, title=_(u"Varování"), **kwargs):
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
                                 default=Message.BUTTON_OK,
                                 **kwargs)


class Error(Message):
    """Dialog pro zobrazení chybové zprávy."""

    def __init__(self, parent, message, title=_(u"Chyba"), **kwargs):
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
                               default=Message.BUTTON_OK,
                               **kwargs)


class MultiQuestion(Message):
    """Dialog vyžadující odpověď na otázku výběrem z tlačítek."""
    def __init__(self, parent, message, buttons, default=None,
                 title=_(u"Otázka"), icon=Message.ICON_QUESTION, **kwargs):
        super_(MultiQuestion).__init__(self, parent, message, title=title, buttons=buttons,
                                       default=default, icon=icon, **kwargs)
    

class Question(MultiQuestion):
    """Dialog vyžadující odpověď ano/ne na zobrazenou zprávu (otázku).

    Metoda 'run()' vrací: Pravdu, právě když uživatel odpoví na danou
    otázku kladně - stiskne tlačítko s nápisem 'Message.BUTTON_YES'.

    """
    def __init__(self, parent, message, default=True,
                 title=_(u"Otázka"), icon=Message.ICON_QUESTION,
                 **kwargs):
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
            default = self.BUTTON_YES
        else:
            default = self.BUTTON_NO
        self._COMMIT_BUTTON = default
        super_(Question).__init__(self, parent, message, title=title,
                                  buttons=(self.BUTTON_YES, self.BUTTON_NO),
                                  default=default, icon=icon, **kwargs)
        
    def _customize_result(self, result):
        if self._button_label(result) == self.BUTTON_YES:
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
    _COMMIT_BUTTON = Message.BUTTON_OK

    
    def __init__(self, parent, message=None, value=None, prompt=None,
                 title=_(u"Zadejte hodnotu"), passwd=False,
                 input_width=None, input_height=1, allow_empty=True, **kwargs):
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
                                     default=None, **kwargs)
        assert value is None or isinstance(value, basestring)
        assert prompt is None or isinstance(prompt, basestring)
        assert isinstance(allow_empty, types.BooleanType)
        self._value = value and unicode(value)
        self._prompt = unicode(prompt)
        self._passwd = passwd
        self._allow_empty = allow_empty
        self._mask = ''
        self._autoformat = None

    def _create_content(self, sizer):
        if self._message is not None:
            Message._create_content(self, sizer)
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
        wx_callback(wx.EVT_KILL_FOCUS, self._control, self._on_kill_control_focus)
        self._handle_keys(control)
        if self._prompt:
            prompt = wx.StaticText(self._dialog, -1, self._prompt)
            content = wx.BoxSizer()
            content.Add(prompt, 1, wx.ALL|wx.ALIGN_CENTER_VERTICAL, 3)
            content.Add(control, 0, wx.ALL|wx.ALIGN_CENTER_VERTICAL, 3)
        else:
            content = control
        sizer.Add(content, 0, wx.ALL|wx.CENTER, 5)
        

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
    def __init__(self, parent, message=None, title=_(u"Zadejte heslo"),
                 prompt=_(u"Heslo:"), icon=None):
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
        
    def _create_content(self, sizer):
        if self._message is not None:
            Message._create_content(self, sizer)
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
        sizer.Add(grid, 0, wx.ALL|wx.CENTER, 5)

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
                raise ProgramError("Input validation error!")                
            return value
        else:
            return pytis.data.Value(pytis.data.Date(), None)


class InputNumeric(InputDialog):
    """Dialog pro zadávání čísel.

    Speciální případ dialogu 'InputDialog' určený pro zadávání čísel.
    
    """
  
    def __init__(self, parent, message=None, value=None, prompt=None,
                 title=_(u"Zadejte hodnotu"), integer_width=10,
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
        encoding = locale.getpreferredencoding()
        self._decimal_point = locale.localeconv()['decimal_point'].decode(encoding)
        self._thousands_sep = locale.localeconv()['thousands_sep'].decode(encoding)
        self._limited = not (self._min_value is None and self._max_value is None)

    def _create_content(self, sizer):
        if self._message is not None:
            Message._create_content(self, sizer)
        kwargs = {}
        if self._thousands_sep:
            kwargs['groupChar'] = self._thousands_sep
        control = masked.NumCtrl(self._dialog, -1, 0,
                                 allowNegative=self._allow_negative,
                                 allowNone=self._allow_empty,
                                 selectOnEntry=self._select_on_entry,
                                 integerWidth=self._integer_width,
                                 fractionWidth=self._decimal_width,
                                 groupDigits=True,
                                 decimalChar=self._decimal_point,
                                 signedForegroundColour=self._signed_colour,
                                 **kwargs
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
        if self._prompt:
            prompt = wx.StaticText(self._dialog, -1, self._prompt)
            content = wx.BoxSizer()
            content.Add(prompt,  1, wx.ALL|wx.ALIGN_CENTER_VERTICAL, 3)           
            content.Add(control, 0, wx.ALL|wx.ALIGN_CENTER_VERTICAL, 3)
        else:
            content = control
        self._handle_keys(control)
        sizer.Add(content, 0, wx.ALL|wx.CENTER, 5)
        
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
                    return pytis.data.Value(
                        pytis.data.Float(precision=self._decimal_width), None)
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
    _EDIT_FORM = "PopupEditForm"
    _INSERT_FORM = "PopupInsertForm"
    _BROWSE_DUAL_FORM = "BrowseDualForm"
    _MULTI_BROWSE_DUAL_FORM = "MultiBrowseDualForm"
    _CODEBOOK_FORM = "CodebookForm"

    def __init__(self, parent, title=_(u"Zobrazit formulář")):
        """Inicializuj dialog.

        Argumenty:

          Argumenty odpovídají stejným argumentům rodičovské třídy.
          
        """
        super_(RunFormDialog).__init__(self, parent, message=None,
                                       title=title, input_width=25,
                                       prompt=_(u"Název specifikace:"),
                                       icon=self.ICON_TIP)
        self._FORM_CLASS_MAPPING = {
            self._BROWSE_DUAL_FORM: pytis.form.BrowseDualForm,
            self._MULTI_BROWSE_DUAL_FORM: pytis.form.MultiBrowseDualForm,
            self._BROWSE_FORM: BrowseForm,
            self._EDIT_FORM: PopupEditForm,
            self._INSERT_FORM: PopupInsertForm,
            self._CODEBOOK_FORM: CodebookForm,
        }


    def _create_content(self, sizer):
        super(RunFormDialog, self)._create_content(sizer)
        label = wx.StaticText(self._dialog, -1, _(u"Třída formuláře:"))
        choices = [self._BROWSE_FORM, self._EDIT_FORM, self._INSERT_FORM,
                   self._BROWSE_DUAL_FORM, self._MULTI_BROWSE_DUAL_FORM, self._CODEBOOK_FORM]
        control = wx.Choice(self._dialog, -1, (-1,-1), (-1,-1), choices=choices)
        control.SetSelection(0)
        self._handle_keys(control)
        self._form_class_choice = control
        vsizer = wx.BoxSizer()
        vsizer.Add(label, 1, wx.ALL|wx.ALIGN_CENTER_VERTICAL, 3)
        vsizer.Add(control, 0, wx.ALL|wx.ALIGN_CENTER_VERTICAL, 3)
        sizer.Add(vsizer, 0, wx.ALL|wx.CENTER, 5)

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
                 title=_(u"Provádí se operace"),
                 message=_(u"Čekejte prosím...")):
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
        assert isinstance(function, collections.Callable)
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
                 title=_(u"Provádí se operace"), message=_(u"Čekejte prosím..."),
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
    'run()' vrací vybraný datum jako instanci 'datetime.datetime', nebo None, pokud
    byl dialog opuštěn.
    
    """
    _COMMIT_BUTTON = Message.BUTTON_OK

    def __init__(self, parent, date, title=_(u"Kalendář"),
                 enable_year=True, enable_month=True, monday_first=True):
        """Inicializuj dialog.

        Argumenty:

          parent -- wx rodič; instance 'wx.Frame' nebo 'wx.Dialog'
          date -- přednastavený datum jako instance 'datetime.datetime'.
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
        if date is None:
            self._date = pytis.data.Date.datetime()
        else:
            assert isinstance(date, datetime.date), date
            self._date = date
        
    def _create_content(self, sizer):
        cal = calendar.CalendarCtrl(self._dialog, -1, style=self._style)
        if wx.MAJOR_VERSION == 2 and wx.MINOR_VERSION < 6:
            size = cal.GetSize()
            cal.SetMinSize((size.GetWidth()+150, size.GetHeight()))
        wx_date = wx.DateTime()
        if wx_date.ParseDate(str(self._date)) is None:
            wx_date = wx.DateTime_Today()
        wx_callback(calendar.EVT_CALENDAR, cal, cal.GetId(), self._on_calendar)
        self._handle_keys(cal)
        cal.SetDate(wx_date)
        self._cal = cal
        self._want_focus = cal
        sizer.Add(cal, 0, wx.ALL|wx.CENTER, 5)

    def _can_commit(self, widget):
        return super(Calendar, self)._can_commit(widget) or widget == self._cal
    
    def _customize_result(self, result):
        if result == self._cal.GetId() \
               or self._button_label(result) == Message.BUTTON_OK:
            date_string = str(self._cal.GetDate().FormatISODate())
            return pytis.data.Date(format=pytis.data.Date.DEFAULT_FORMAT).validate(date_string)[0].value()
        return None

    def _on_calendar(self, event):
        return self._end_modal(self._button_id(Message.BUTTON_OK))

    
class ColorSelector(GenericDialog):
    """Dialog umožňující výběr barvy.

    Výchozí barva může být přednastavena parametrem konstruktoru.  Metoda
    'run()' vrací barvu jako řetězec '#RRGGBB', nebo None, pokud byl dialog
    opuštěn.
    
    """

    def __init__(self, parent, color=None, title=_(u"Výběr barvy")):
        """Inicializuj dialog.

        Argumenty:

          parent -- wx rodič; instance 'wx.Frame' nebo 'wx.Dialog'
          color -- přednastavená barva, jako řetězec '#RRGGBB'.
          title -- titulek dialogového okna jako string

        """
        super_(ColorSelector).__init__(self, parent, title=title, buttons=())
        assert isinstance(color, basestring) or color is None
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

    _IGNORE_LABEL = _(u"Ignorovat")
    _REPORT_LABEL = _(u"Poslat oznámení o chybě")
    _EXIT_LABEL = _(u"Ukončit aplikaci")
    _COMMIT_BUTTON = _EXIT_LABEL
    _STYLE = GenericDialog._STYLE | wx.RESIZE_BORDER
    
    def __init__(self, parent, einfo):
        """Inicializuj instanci.

        Argumenty:

          parent -- wx rodič; instance 'wx.Frame' nebo 'wx.Dialog'
          einfo -- informace o výjimce ve tvaru vraceném funkcí
            'sys.exc_info()'

        """
        super_(BugReport).__init__(self, parent, _(u"Neočekávaná chyba"),
                                   buttons=(self._IGNORE_LABEL,
                                            self._REPORT_LABEL,
                                            self._EXIT_LABEL),
                                   default=self._IGNORE_LABEL)
        self._einfo = einfo

    def _create_content(self, sizer):
        dialog = self._dialog
        label = wx.StaticText(dialog, -1, _(u"Nevyšlo to"))
        font = wx.Font(18, wx.DEFAULT, wx.NORMAL, wx.BOLD, encoding=wx.FONTENCODING_DEFAULT)
        label.SetFont(font)
        icon = self._create_icon(Message.ICON_ERROR)
        if icon is not None:
            vsizer = wx.BoxSizer(wx.HORIZONTAL)
            vsizer.Add(label, 1, wx.ALIGN_CENTER_VERTICAL)
            vsizer.Add(icon, 0, wx.ALL, 5)
            label = vsizer
        # store the traceback text    
        import config
        if 0:
            # Fancy HTML traceback
            import cgitb
            from wx import html
            traceback = html.HtmlWindow(dialog, -1)
            text = "<html>"+cgitb.html(self._einfo)+"</html>"
            step = 3000
            pointer = 0
            while (pointer < len(text)):
                traceback.AppendToPage(text[pointer:min(pointer+step,len(text))])
                pointer += step
                traceback.SetSize(char2px(traceback, 140, 35))
            #traceback.SetFonts('Arial', 'Fixed', sizes=(6,7,8,9,10,11,12))
        else:
            style = wx.TE_MULTILINE|wx.TE_DONTWRAP #|wx.TE_READONLY
            traceback = wx.TextCtrl(dialog, -1, style=style, size=wx.Size(600, 360))
            font = wx.Font(traceback.GetFont().GetPointSize(), wx.MODERN, wx.NORMAL, wx.NORMAL)
            traceback.SetFont(font)
            for line in exception_info(self._einfo).splitlines():
                # Příliš "dlouhý" text se nemusí povést do políčka vložit...
                traceback.AppendText(line+'\n')
        self._traceback = traceback
        self._want_focus = traceback
        sizer.Add(label, 0, wx.EXPAND|wx.ALL|wx.CENTER, 5)
        sizer.Add(traceback, 1, wx.EXPAND|wx.ALL, 5)


    def _customize_result(self, result):
        label = self._button_label(result)
        if label == self._EXIT_LABEL:
            result = None
        elif label == self._IGNORE_LABEL or label is None:
            result = ''
        elif label == self._REPORT_LABEL:
            if isinstance(self._traceback, wx.TextCtrl):
                result = self._traceback.GetValue()
            else:
                result = exception_info(self._einfo)
        else:
            raise ProgramError('Unknown BugReport dialog result', label)
        return result

    def _cmd_close_dialog(self):
        self._end_modal(self._button_id(self._IGNORE_LABEL))


class _CheckListCtrl(wx.ListCtrl, wx.lib.mixins.listctrl.CheckListCtrlMixin):
    def __init__(self, parent, columns, items):
        wx.ListCtrl.__init__(self, parent, -1, style=wx.LC_REPORT)
        wx.lib.mixins.listctrl.CheckListCtrlMixin.__init__(self)
        wx_callback(wx.EVT_LIST_ITEM_ACTIVATED, self, self.GetId(),
                    lambda e: self.ToggleItem(e.m_itemIndex))
        for i, label in enumerate(columns):
            self.InsertColumn(i, label)
        for i, item in enumerate(items):
            self.InsertStringItem(i, item[1])
            self.CheckItem(i, item[0])
            for j, value in enumerate(item[1:]):
                self.SetStringItem(i, j, value)
        for i in range(len(columns)):
            self.SetColumnWidth(i, wx.LIST_AUTOSIZE)
        self.SetMinSize((0, max(80, min(300, len(items)*20+30))))


class CheckListDialog(Message):
    """A question dialog with a list of checkable items.

    The dialog displays a question with a list of items and a checkbox for each of the items.
    Items can 

    The result returned by the `run()' method is a sequence of boolean values, one for each item of
    'items' passed to the constructor.  The value is True for items which were checked and False
    for unchecked items.

    """
    _STYLE = GenericDialog._STYLE | wx.RESIZE_BORDER
    
    def __init__(self, parent, columns=(), items=(), **kwargs):
        """Arguments:
             columns -- sequence of column labels (strings).
             items -- a sequence of checkable items.  Each item is a sequence.  The first value
               in this sequence is a boolean flag indicating the initial checkbox state for this
               item.  The following values are textual fields describing the item.  The number of
               textual fields must be the same as the numer of column labels passed in
               'columns'.  These fields are presented in a table-like list.
        """
        super(CheckListDialog, self).__init__(parent, buttons=(Message.BUTTON_OK,
                                                               Message.BUTTON_CANCEL), **kwargs)
        assert isinstance(columns, (list, tuple))
        assert isinstance(items, (list, tuple))
        self._columns = columns
        self._items = items

    def _create_content(self, sizer):
        super(CheckListDialog, self)._create_content(sizer)
        self._checklist = _CheckListCtrl(self._dialog, self._columns, self._items)
        sizer.Add(self._checklist, 1, wx.EXPAND|wx.ALL, 5)
        
    def _customize_result(self, result):
        if self._button_label(result) == self.BUTTON_OK:
            return [self._checklist.IsChecked(i) for i, triple in enumerate(self._items)]
        else:
            return None

        
class CheckMatrixDialog(Message):
    """A dialog with a matrix of checkable items.

    The dialog displays a question with a matrix of checkboxes arranged within
    labeled rows and columns.

    The result returned by the `run()' method is a sequence of sequences of
    boolean values, one sequence for each matrix row and one boolean value for
    each column.  The order of rows and columns corresponds to the order passed
    to the constructor.  The value is True for items which were checked and
    False for unchecked items.

    """
    _STYLE = GenericDialog._STYLE | wx.RESIZE_BORDER
    
    def __init__(self, parent, columns=(), rows=(), values=None, enabled=None, **kwargs):
        """Arguments:
             columns -- sequence of column labels (strings).
             rows --  sequence of row labels (strings).
             values -- a sequence of sequences of boolean values.  The outer
               sequence corresponds to matrix rows and the inner sequences
               correcpond to matrix columns.  The length of the outer sequence
               must match the length of 'rows', the lengths of inner sequences
               must match the lengths of 'columns'.  The values control the
               initial state of the corresponding checkboxes.
             enabled -- None or a function of two arguments (ROW, COLUMN)
               returning a boolean value indicating the availability of the
               corresponding checkbox in the matrix through numeric row/column
               indices.  When False is returned the checkbox at given row and
               column will be inactive (the user will not be able to change its
               state from its initial state given by values).
        """
        super(CheckMatrixDialog, self).__init__(parent, buttons=(Message.BUTTON_OK,
                                                                 Message.BUTTON_CANCEL), **kwargs)
        assert isinstance(columns, (list, tuple)), columns
        assert isinstance(rows, (list, tuple)), rows
        assert values is None or isinstance(values, (list, tuple)), values
        self._columns = columns
        self._rows = rows
        self._values = values
        self._enabled = enabled

    def _create_content(self, sizer):
        super(CheckMatrixDialog, self)._create_content(sizer)
        panel = wx.ScrolledWindow(self._dialog, style=wx.TAB_TRAVERSAL)
        panel.SetScrollRate(20, 20)
        grid = wx.FlexGridSizer(len(self._rows)+1, len(self._columns)+1, 2, 6)
        self._controls = []
        grid.Add(wx.StaticText(panel, -1, ""))
        for column in self._columns:
            label = wx.StaticText(panel, -1, column)
            grid.Add(label)
        for i, row_label in enumerate(self._rows):
            label = wx.StaticText(panel, -1, row_label)
            grid.Add(label)
            controls = []
            for j, column_label in enumerate(self._columns):
                control = wx.CheckBox(panel, -1)
                if self._values:
                    value = self._values[i][j]
                    control.SetValue(value)
                if self._enabled:
                    control.Enable(self._enabled(i, j))
                grid.Add(control)
                controls.append(control)
            self._controls.append(controls)
        panel.SetSizer(grid)
        self._matrix_size = grid.CalcMin()
        sizer.Add(panel, 1, wx.EXPAND|wx.ALL, 5)

    def _run_dialog(self):
        sizer_size = self._dialog.GetSizer().CalcMin()
        size = wx.Size(max(sizer_size.width, self._matrix_size.width + 40),
                       sizer_size.height + self._matrix_size.height)
        size.DecTo(wx.GetDisplaySize() - wx.Size(50, 80))
        self._dialog.SetClientSize(size)
        return super(CheckMatrixDialog, self)._run_dialog()

        
    def _customize_result(self, result):
        if self._button_label(result) == self.BUTTON_OK:
            return [[ctrl.IsChecked() for ctrl in controls]
                    for controls in self._controls]
        else:
            return None


class AggregationSetupDialog(Message):
    """A dialog for setting up an aggregated form.

    The result returned by the `run()' is a tuple of two tuples
    (group_by_columns, aggregation_columns).

    group_by_columns -- selected group by columns as a sequence of pairs
      (column_id, function), where function is the name of the grouping
      function from 'grouping_functions' constructor argument or None if the
      column is used directly with no function applied.
               
    aggregation_columns -- preselected aggregation columns as a sequence of
      pairs (column_id, operation), where operation is the name of the
      aggregation function from 'aggregation_functions' constructor argument.
    
    """
    _STYLE = GenericDialog._STYLE | wx.RESIZE_BORDER
    
    def __init__(self, parent, aggregation_functions, grouping_functions, columns,
                 group_by_columns, aggregation_columns, aggregation_valid,
                 title=_(u"Zvolte sloupce..."),
                 message=_(u"Zvolte sloupce agregačního náhledu")):
        """Arguments:
             aggregation_functions -- specification of available aggregation
               functions as a sequence of pairs (operation, label), where
               operation is one of `pytis.data.AGG_*' constants and label is
               the string title of given function.
             grouping_functions -- specification of available functions
               aplicable to group by columns in the same format as the
               'ViewSpec' argument 'grouping_functions'.
             columns -- sequence of available columns as tuples (column_id,
               column_label, column_type).
             aggregation_valid -- function of two arguments (operation,
               column_type) returning true if given aggregation operation is
               valid for given column type and false otherwise.
             group_by_columns -- preselected group by columns in the same
               format as in the result of run() as described in the class
               docstring.
             aggregation_columns -- preselected aggregation columns in the same
               format as in the result of run() as described in the class
               docstring.
        """
        super(AggregationSetupDialog, self).__init__(parent, title=title, message=message,
                                                     buttons=(Message.BUTTON_OK,
                                                              Message.BUTTON_CANCEL))
        self._aggregation_functions = aggregation_functions
        self._grouping_functions = grouping_functions
        self._columns = columns
        self._aggregation_valid = aggregation_valid
        self._group_by_columns = group_by_columns
        self._aggregation_columns = aggregation_columns
        
    def _create_content(self, sizer):
        super(AggregationSetupDialog, self)._create_content(sizer)
        panel = wx.ScrolledWindow(self._dialog, style=wx.TAB_TRAVERSAL)
        panel.SetScrollRate(20, 20)
        self._grid = grid = wx.FlexGridSizer(len(self._columns)+1,
                                             len(self._aggregation_functions)+2, 2, 6)
        self._grouping_controls = []
        self._aggregation_controls = []
        for label in ['', _(u"Seskupování")] + [x[1] for x in self._aggregation_functions]:
            grid.Add(wx.StaticText(panel, -1, label))
        for (column_id, column_label, column_type) in self._columns:
            grid.Add(wx.StaticText(panel, -1, column_label))
            checkbox = wx.CheckBox(panel, -1)
            checkbox.SetValue((column_id, None) in self._group_by_columns)
            self._grouping_controls.append(((column_id, None), checkbox))
            functions = [x for x in self._grouping_functions if isinstance(column_type, x[2])]
            if functions:
                fsizer = wx.BoxSizer(wx.VERTICAL)
                fsizer.Add(checkbox)
                cp = wx.CollapsiblePane(panel, label=_(u"Funkce"), style=wx.CP_DEFAULT_STYLE)
                panel.Bind(wx.EVT_COLLAPSIBLEPANE_CHANGED, self._on_collapsiblepane_changed, cp)
                pane = cp.GetPane()
                cpsizer = wx.BoxSizer(wx.VERTICAL)
                collapse = True
                for function, label, input_type, return_type in functions:
                    checkbox = wx.CheckBox(pane, -1, label=label)
                    checked = (column_id, function) in self._group_by_columns
                    checkbox.SetValue(checked)
                    if checked:
                        collapse = False
                    self._grouping_controls.append(((column_id, function), checkbox))
                    cpsizer.Add(checkbox)
                pane.SetSizer(cpsizer)
                cp.Collapse(collapse)
                fsizer.Add(cp)
                grid.Add(fsizer)
            else:
                grid.Add(checkbox)
            for operation, title in self._aggregation_functions:
                checkbox = wx.CheckBox(panel, -1)
                checkbox.SetValue((column_id, operation) in self._aggregation_columns)
                checkbox.Enable(self._aggregation_valid(operation, column_type))
                grid.Add(checkbox)
                self._aggregation_controls.append(((column_id, operation), checkbox))
        panel.SetSizer(grid)
        sizer.Add(panel, 1, wx.EXPAND|wx.ALL, 5)

    def _on_collapsiblepane_changed(self, event):
        self._grid.Layout()
        self._resize()

    def _resize(self):
        sizer_size = self._dialog.GetSizer().CalcMin()
        grid_size = self._grid.CalcMin()
        size = wx.Size(max(sizer_size.width, grid_size.width + 20),
                       sizer_size.height + grid_size.height)
        size.DecTo(wx.GetDisplaySize() - wx.Size(50, 80))
        self._dialog.SetClientSize(size)
        
    def _run_dialog(self):
        self._resize()
        return super(AggregationSetupDialog, self)._run_dialog()

    def _on_button(self, event):
        if self._button_label(event.GetId()) == self.BUTTON_OK:
            self._group_by_columns = [spec for spec, checkbox in self._grouping_controls
                                      if checkbox.IsChecked()]
            self._aggregation_columns = [spec for spec, checkbox in self._aggregation_controls
                                         if checkbox.IsChecked()]
            if not self._group_by_columns:
                run_dialog(Warning, _(u"Musíte zvolit alespoň jeden sloupec pro seskupování"))
                return
        return super(AggregationSetupDialog, self)._on_button(event)

    def _customize_result(self, result):
        if self._button_label(result) == self.BUTTON_OK:
            return tuple(self._group_by_columns), tuple(self._aggregation_columns)
        else:
            return None
        
        
class ExitDialog(Question):
    """Application exit question with a choice of items to save for next startup.

    The dialog lets the user to save the application state by checking the items (forms, documents)
    which should be opened automatically on next startup.

    The result returned by the `run()' method is a pair (EXIT, ITEMS).  EXIT is True if the user
    really wants to quit the application or False otherwise.  ITEMS is a sequence of boolean
    values, one for each item of 'save_items' passed to the constructor.  The value is True for
    items which were checked and False for unchecked items.  ITEMS is None if the user doesn't want
    to save the current state (so the prevoius saved state should be used).

    """
    _STYLE = GenericDialog._STYLE | wx.RESIZE_BORDER
    
    def __init__(self, parent, title=_(u"Ukončit aplikaci"),
                 message=_(u"Opravdu chcete ukončit aplikaci?"), icon=Message.ICON_QUIT,
                 save_label=_(u"Zapamatovat označené formuláře pro příští spuštění"),
                 save_state=True, save_columns=(), save_items=()):
        """Arguments:

           save_label -- save state checkbox label as a string or unicode.  This checkbox
             indicates, whether the current application state should be saved or not.
           save_state -- initial state of the save checkbox as a boolean value.
           save_columns -- sequence of column labels for the list of items to save.
           save_items -- a sequence of checkable items.  Each item is a sequence.  The first value
             in this sequence is a boolean flag indicating the initial checkbox state for this
             item.  The following values are textual fields describing the item.  The number of
             textual fields must be the same as the numer of column labels passed in
             'save_columns'.  These fields are presented in a table-like list.
    
        """
        super(ExitDialog, self).__init__(parent, message, title=title, default=True, icon=icon)
        assert isinstance(save_state, bool)
        assert isinstance(save_columns, (list, tuple))
        assert isinstance(save_items, (list, tuple))
        assert isinstance(save_label, basestring)
        self._save_state = save_state
        self._save_label = save_label
        self._save_columns = save_columns
        self._save_items = save_items

    def _create_content(self, sizer):
        super(ExitDialog, self)._create_content(sizer)
        if self._save_items:
            self._checklist = _CheckListCtrl(self._dialog, self._save_columns, self._save_items)
            self._checkbox = wx.CheckBox(self._dialog, -1, self._save_label)
            self._checkbox.SetValue(self._save_state)
            sizer.Add(self._checklist, 1, wx.EXPAND|wx.ALL, 5)
            sizer.Add(self._checkbox, 0, wx.ALL|wx.ALIGN_LEFT, 5)
        
    def _customize_result(self, result):
        exit = super(ExitDialog, self)._customize_result(result)
        if self._save_items and self._checkbox.IsChecked():
            items = [self._checklist.IsChecked(i) for i, triple in enumerate(self._save_items)]
        else:
            items = None
        return (exit, items)

    
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
                 wildcards=(_(u"Všechny soubory")+" (*.*)|*.*",),
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
            title = {FileDialog.OPEN: _(u"Otevřít soubor"),
                     FileDialog.SAVE: _(u"Uložit soubor")}[mode]
        assert dir is None or isinstance(dir, basestring)
        assert file is None or isinstance(file, basestring)
        self._title = unicode(title)
        self._dir = dir
        self._file = file
        self._mode = mode
        self._wildcards = wildcards
        self._multi = multi
        self._overwrite_prompt = overwrite_prompt

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

