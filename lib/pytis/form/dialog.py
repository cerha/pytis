# -*- coding: utf-8 -*-

# Copyright (C) 2019-2021 Tomáš Cerha <t.cerha@gmail.com>
# Copyright (C) 2001-2018 OUI Technology Ltd.
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
from __future__ import unicode_literals
from __future__ import division
from past.builtins import basestring
from builtins import range
from future import standard_library

import cgitb
import datetime
import email.utils
import os
import sys
import wx.adv

import pytis.data
import pytis.form
import pytis.util

from pytis.presentation import TextFormat
from pytis.util import ProgramError, super_, send_mail

from .command import CommandHandler
from .event import wx_callback
from .screen import KeyHandler, wx_focused_window, wx_text_ctrl, wx_text_view

# Needed for subprocess.getstatusoutput (commands.getstatusoutput in Python 2).
standard_library.install_aliases()

_ = pytis.util.translations('pytis-wx')

unistr = type(u'')  # Python 2/3 transition hack.


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
    @classmethod
    def _get_command_handler_instance(cls):
        return pytis.form.top_window()

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

    BUTTON_OK = _("Ok")
    "Nápis pro potvrzovací tlačítko."
    BUTTON_CANCEL = _("Cancel")
    "Nápis pro opouštěcí tlačítko."
    BUTTON_YES = _("Yes")
    "Nápis pro tlačítko souhlasu."
    BUTTON_NO = _("No")
    "Nápis pro tlačítko nesouhlasu."

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
        assert report_format in pytis.util.public_attr_values(TextFormat), report_format
        assert isinstance(report_size, (list, tuple)) and len(report_size) == 2, report_size
        super_(GenericDialog).__init__(self, parent)
        self._title = unistr(title)
        self._button_labels = buttons
        self._default = default
        self._report = report
        self._report_format = report_format
        self._report_size = report_size
        self._want_focus = None
        self._shown = False

    def _create_dialog(self):
        """Create the dialog wx instance and build its contents.

        The goal is of this method is to assign the wx instance to
        'self._dialog' and initialize its contents.

        The base class creates a 'wx.Dialog' and calls
        '_create_dialog_elements()'.  Most subclasses will only need to
        override the methods used by '_create_dialog_elements()' to build
        specific dialog contents.  Overriding this method may be necessary when
        another wx class is to be used instead of 'wx.Dialog'.

        """
        style = self._STYLE
        if self._report is not None:
            style |= wx.RESIZE_BORDER
        self._dialog = dialog = wx.Dialog(self._parent, title=self._title, style=style)
        self._create_dialog_elements()
        self._handle_keys(dialog)

    def _rebuild(self):
        self._dialog.DestroyChildren()
        self._create_dialog_elements()
        self.focus()

    def _create_dialog_elements(self):
        """Vlož do dialogu jeho vnitřní prvky.

        Pomocí sizerů je do dialogu vložen hlavní obsah (výsledek metody
        '_create_content()') a tlačítka (výsledek metody '_create_buttons()').

        Tuto metodu by nemělo být třeba předefinovávat. Ve většině případů by
        mělo stačit předefinovat metodu '_create_content()' a/nebo
        '_create_buttons()'.

        """
        dialog = self._dialog
        sizer = wx.BoxSizer(wx.VERTICAL)
        self._create_content(sizer)
        # vytvoř tlačítka a poskládej je vedle sebe
        button_sizer = wx.BoxSizer()
        for b in self._create_buttons():
            button_sizer.Add(b, 0, wx.ALL, 8)
            # registruj handlery událostí
            wx_callback(wx.EVT_BUTTON, b, self._on_button)
            self._handle_keys(b)
        # poskládej obsah a tlačítka do top-level sizeru (nad sebe)
        if self._report is not None:
            report = wx_text_view(dialog, self._report,
                                  format=self._report_format,
                                  width=self._report_size[0], height=self._report_size[1])
            # Set the min size to be respected by the sizer (but unset again below).
            report.SetMinSize(report.GetSize())
            sizer.Add(report, 1, wx.EXPAND)
        sizer.Add(button_sizer, 0, wx.CENTER)
        wx_callback(wx.EVT_IDLE, self._dialog, self._on_idle)
        dialog.SetSizer(sizer)
        sizer.Fit(dialog)
        if self._report is not None:
            # Unset report's min size to allow manually sizing the dialog to a smaller size.
            report.SetMinSize((100, 100))

    def _create_content(self, sizer):
        """Create the main dialog content and add it to the top level sizer.

        The main content is the area above the dialog buttons, which is
        constructed specifically for each derived class.

        This method must be defined by all derived classes.  The base class
        implementation does nothing.

        """
        pass

    def _create_buttons(self):
        """Create dialog buttons and return them as a sequence of wx widgets."""
        self._buttons = []
        for label in self._button_labels:
            button = wx.Button(self._dialog, -1, label)
            self._buttons.append(button)
            if self._default == label:
                button.SetDefault()
                self._want_focus = button
        return self._buttons

    def _create_icon(self, artid):
        bitmap = wx.ArtProvider.GetBitmap(artid, wx.ART_MESSAGE_BOX, (48, 48))
        if bitmap.IsOk():
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
            # Note, self._want_focus may be set (not None), but dead (evaluate to False).
            if self._want_focus:
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
        button = pytis.util.find(id, self._buttons, key=lambda b: b.GetId())
        return button and button.GetLabel()

    def _button_id(self, label):
        # Vrať id tlačítka s daným nápisem.
        button = pytis.util.find(label, self._buttons, key=lambda b: b.GetLabel())
        return button and button.GetId()

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
        pytis.form.Application.COMMAND_HELP.invoke(topic='pytis/' + self._HELP_TOPIC)

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
    ICON_QUESTION = wx.ART_QUESTION
    "Ikona s otazníkem."
    ICON_WARNING = wx.ART_WARNING
    "Ikona s vykřičníkem."
    ICON_ERROR = wx.ART_ERROR
    "Ikona pro chybové zprávy."
    ICON_TIP = wx.ART_TIP
    "Ikona pro tipy, rady apod."
    ICON_QUIT = wx.ART_QUIT
    "Ikona opuštění aplikace."

    _icons = (ICON_INFO, ICON_QUESTION, ICON_WARNING, ICON_ERROR, ICON_TIP, ICON_QUIT)

    def __init__(self, parent, message, icon=ICON_INFO, title=_("Message"),
                 buttons=(GenericDialog.BUTTON_OK,), default=GenericDialog.BUTTON_OK, **kwargs):
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
            self._message = unistr(message)
        else:
            self._message = None
        self._icon = icon

    def _create_content(self, sizer):
        """Vytvoř obsah - to co bude vyplňovat plochu okna nad tlačítky."""
        message = wx.StaticText(self._dialog, -1, self._message)
        icon = self._icon and self._create_icon(self._icon)
        if icon is not None:
            content = wx.BoxSizer()
            content.Add(icon, 0, wx.ALL | wx.ALIGN_CENTER_VERTICAL, 5)
            content.Add(message, 1, wx.EXPAND | wx.ALL, 12)
        else:
            content = message
        sizer.Add(content, 0, wx.ALL | wx.CENTER, 5)


class Warning(Message):
    """Dialog pro zobrazení varovné zprávy."""

    def __init__(self, parent, message, title=_("Warning"), **kwargs):
        """Inicializuj dialog.

        Argumenty:


          Odpovídají stejným argumentům rodičovské třídy s tím, že následující
          argumenty tato třída definuje vždy napevno:

            icon = 'Message.ICON_WARNING'
            buttons = ('GenericDialog.BUTTON_OK',)
            default = 'GenericDialog.BUTTON_OK'

        """
        super_(Warning).__init__(self, parent, message, title=title,
                                 icon=Message.ICON_WARNING,
                                 buttons=(GenericDialog.BUTTON_OK,),
                                 default=GenericDialog.BUTTON_OK,
                                 **kwargs)


class Error(Message):
    """Dialog pro zobrazení chybové zprávy."""

    def __init__(self, parent, message, title=_("Error"), **kwargs):
        """Inicializuj dialog.

        Argumenty:


          Odpovídají stejným argumentům rodičovské třídy s tím, že následující
          argumenty tato třída definuje vždy napevno:

            icon = 'Message.ICON_ERROR'
            buttons = ('GenericDialog.BUTTON_OK',)
            default = 'GenericDialog.BUTTON_OK'

        """
        super_(Error).__init__(self, parent, message, title=title,
                               icon=Message.ICON_ERROR,
                               buttons=(GenericDialog.BUTTON_OK,),
                               default=GenericDialog.BUTTON_OK,
                               **kwargs)


class MultiQuestion(Message):
    """Dialog vyžadující odpověď na otázku výběrem z tlačítek."""

    def __init__(self, parent, message, buttons, default=None,
                 title=_("Question"), icon=Message.ICON_QUESTION, **kwargs):
        super_(MultiQuestion).__init__(self, parent, message, title=title, buttons=buttons,
                                       default=default, icon=icon, **kwargs)


class Question(MultiQuestion):
    """Dialog vyžadující odpověď ano/ne na zobrazenou zprávu (otázku).

    Metoda 'run()' vrací: Pravdu, právě když uživatel odpoví na danou
    otázku kladně - stiskne tlačítko s nápisem 'GenericDialog.BUTTON_YES'.

    """
    def __init__(self, parent, message, default=True,
                 title=_("Question"), icon=Message.ICON_QUESTION, timeout=None,
                 **kwargs):
        """Inicializuj dialog.

        Argumenty:

          default -- pokud je pravda, bude předvoleným tlačítkem tlačítko
            'GenericDialog.BUTTON_YES'. Jinak je předvolená odpověď
            'GenericDialog.BUTTON_NO' (implicitně).
          timeout -- dialog timeout in seconds; integer.  When the dialog is
            shown for more than the given time, it gets automatically closed
            and 'None' is returned as the answer.  If the argument value is
            'None' then the dialog is shown until user chooses an answer.

          Ostatní argumenty odpovídají stejným argumentům rodičovské třídy s
          tím, že následující argumenty tato třída definuje vždy napevno:

            buttons = ('GenericDialog.BUTTON_YES', 'GenericDialog.BUTTON_NO')

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
        self._timeout_limit = timeout

    def _create_dialog(self):
        super(Question, self)._create_dialog()
        if self._timeout_limit is not None:
            def destroy():
                try:
                    self._dialog.EndModal(-1000)
                except Exception:
                    # The wx instance of `self' may already be inactive
                    pass
            wx.FutureCall(self._timeout_limit * 1000, destroy)

    def _customize_result(self, result):
        if result == -1000:
            return None
        elif self._button_label(result) == self.BUTTON_YES:
            return True
        else:
            return False


class OperationDialog(Message):
    """Dialog pro spuštění dlouhotrvající operace.

    Spuštěním dialogu metodou 'run()' bude zobrazen modální dialog a spuštěna
    funkce zadaná v konstruktoru.  Dialog je ukončen automaticky po skončení
    funkce.  Do té doby uživatel nemůže dělat nic, než čekat...

    """
    def __init__(self, parent, function, args=(), kwargs={},
                 title=_("Operation in progress"),
                 message=_("Please wait...")):
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
        assert isinstance(args, (tuple, list))
        assert isinstance(kwargs, dict)
        self._function = function
        self._args = args
        self._kwargs = kwargs

    def _on_show(self):
        self._result = self._function(*self._args, **self._kwargs)
        pytis.form.wx_yield_(full=True)
        self._end_modal(wx.ID_OK)

    def _customize_result(self, result):
        return self._result


class ProgressDialog(OperationDialog):
    """Dialog pro spuštění dlouhotrvající operace s ProgressBarem.

    Spuštěním dialogu metodou 'run()' bude zobrazen modální dialog a spuštěna
    funkce zadaná v konstruktoru s argumenty předanými konstruktoru.  Navíc je
    funkci jako první argument předán callback sloužící k aktualizaci stavu
    progress baru.  Za jeho volání v průběhu operace je funkce vykonávající
    operaci zodpovědná.  Tento callback vyžaduje jeden argument, kterým je stav
    operace v procentech (integer).  Je li namísto číselné hodnoty předána
    hodnota None, bude ukazatel průběhu změněn na pulzující pruh bez zobrazení
    konkrétní hodnoty.  Návratová hodnota callbacku bude pravdivá, pokud má být
    operace ukončena (uživatelské přerušení, je-li povoleno).  Za ukončení je
    však opět zodpovědná funkce vykonávající operaci.  Druhým nepovinným
    argumentem je klíčový argument 'newmsg'.  Pokud je předán neprázdný
    řetězec, bude také aktualizována zpráva zobrazená na dialogu (tato zpráva
    nahradí původní zprávu zadanou v konstruktoru).

    Po ukončení dialogu (ať už z důvodu uživatelského přerušení, či dokončení
    operace) je vrácena návratová hodnota funkce vykonávající operaci.

    """
    def __init__(self, parent, function, args=(), kwargs={},
                 title=_("Operation in progress"), message=_("Please wait..."),
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
            style = style | wx.PD_ELAPSED_TIME
        if estimated_time:
            style = style | wx.PD_ESTIMATED_TIME
        if remaining_time:
            style = style | wx.PD_REMAINING_TIME
        if can_abort:
            style = style | wx.PD_CAN_ABORT
        self._style = style

    def _create_dialog(self):
        self._dialog = wx.ProgressDialog(self._title, unistr(self._message),
                                         maximum=100, parent=self._parent,
                                         style=self._style)

    def _update(self, progress, newmsg=''):
        # progress is a number in range 1..100.
        font = self._dialog.GetFont()
        new_width = min(self._dialog.GetFullTextExtent(newmsg, font)[0] + 30,
                        wx.DisplaySize()[0] - 50)
        current_size = self._dialog.GetSize()
        if new_width > current_size.width:
            self._dialog.SetSize((new_width, current_size.height))
        if progress is None:
            return self._dialog.UpdatePulse(newmsg=newmsg)
        else:
            return self._dialog.Update(progress, newmsg=newmsg)

    def _run_dialog(self):
        return self._function(self._update, *self._args, **self._kwargs)

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
        assert step is None or isinstance(step, int) and 1 <= step <= 99, step

        def do(update, *args_list):
            total = len(args_list)
            last_status = 0
            for n, arg in enumerate(args_list):
                status = int(n / total * 100)
                if step is None or status // step != last_status // step:
                    last_status = status
                    try:
                        msg = self._message % arg
                    except TypeError:
                        msg = ''
                    if not update(status, newmsg=msg):
                        break
                function(arg)

        super_(RepeatedOperationDialog).__init__(self, parent, do, args=args, **kwargs)


class Calendar(GenericDialog):
    """Dialog zobrazující kalendář, umožňující výběr dne.

    Datum na kalendáři může být přednastaven parametrem konstruktoru. Metoda
    'run()' vrací vybraný datum jako instanci 'datetime.datetime', nebo None, pokud
    byl dialog opuštěn.

    """
    _COMMIT_BUTTON = GenericDialog.BUTTON_OK

    def __init__(self, parent, date, title=_("Calendar"),
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
                                  buttons=(GenericDialog.BUTTON_OK,
                                           GenericDialog.BUTTON_CANCEL))
        # vytvoř kalendář
        style = (wx.adv.CAL_SHOW_HOLIDAYS |
                 wx.adv.CAL_SHOW_SURROUNDING_WEEKS)
        if not enable_year:
            style = style | wx.adv.CAL_NO_YEAR_CHANGE
        if not enable_month:
            style = style | wx.adv.CAL_NO_MONTH_CHANGE
        if monday_first:
            style = style | wx.adv.CAL_MONDAY_FIRST
        else:
            style = style | wx.adv.CAL_SUNDAY_FIRST
        self._style = style
        if date is None:
            self._date = pytis.data.Date.datetime()
        else:
            assert isinstance(date, datetime.date), date
            self._date = date

    def _create_content(self, sizer):
        cal = wx.adv.GenericCalendarCtrl(self._dialog, -1, style=self._style)
        # This makes year +/- buttons visible, but the calendar is not centered (not nice).
        cal.SetMinSize((cal.Size.width + 40, cal.Size.height))
        wx_date = wx.DateTime()
        if wx_date.ParseDate(str(self._date)) is None:
            wx_date = wx.DateTime_Today()
        wx_callback(wx.adv.EVT_CALENDAR, cal, self._on_calendar)
        self._handle_keys(cal)
        cal.SetDate(wx_date)
        self._cal = cal
        self._want_focus = cal
        sizer.Add(cal, 0, wx.ALL | wx.CENTER, 5)

    def _can_commit(self, widget):
        return super(Calendar, self)._can_commit(widget) or widget == self._cal

    def _customize_result(self, result):
        if result == self._cal.GetId() or self._button_label(result) == GenericDialog.BUTTON_OK:
            date_string = str(self._cal.GetDate().FormatISODate())
            return pytis.data.Date(format=pytis.data.Date.DEFAULT_FORMAT).\
                validate(date_string)[0].value()
        return None

    def _on_calendar(self, event):
        return self._end_modal(self._button_id(GenericDialog.BUTTON_OK))


class ColorSelector(GenericDialog):
    """Dialog umožňující výběr barvy.

    Výchozí barva může být přednastavena parametrem konstruktoru.  Metoda
    'run()' vrací barvu jako řetězec '#RRGGBB', nebo None, pokud byl dialog
    opuštěn.

    """

    def __init__(self, parent, color=None, title=_("Color selection")):
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
    """Dialog displaying information about unhandled exception.

    It is possible to send a bug report by email before closing the dialog.

    The user may close the dialog by choosing between two options:
      - Ignore the exception and try continuing running the program
        (which may not always work).
      - Exit the application

    The return value is True if exit is requested or False otherwise.

    """
    _IGNORE_LABEL = _("Ignore")
    _EXIT_LABEL = _("Exit application")
    _COMMIT_BUTTON = _EXIT_LABEL
    _STYLE = GenericDialog._STYLE | wx.RESIZE_BORDER

    def __init__(self, parent, einfo):
        """Arguments:

          parent -- wx parent window; 'wx.Frame' or 'wx.Dialog' instance
          einfo -- exception information as returned by 'sys.exc_info()'

        """
        super_(BugReport).__init__(self, parent, _("Unhandled exception"),
                                   buttons=(self._IGNORE_LABEL, self._EXIT_LABEL),
                                   default=self._IGNORE_LABEL)
        self._einfo = einfo

    def _create_content(self, sizer):
        dialog = self._dialog
        label = wx.StaticText(dialog, -1, _("Program Error"))
        label.SetFont(wx.Font(18, wx.FONTFAMILY_DEFAULT, wx.FONTSTYLE_NORMAL, wx.FONTWEIGHT_BOLD,
                              encoding=wx.FONTENCODING_DEFAULT))
        icon = self._create_icon(Message.ICON_ERROR)
        self._sent = False
        if icon is not None:
            hsizer = wx.BoxSizer(wx.HORIZONTAL)
            hsizer.Add(label, 1, wx.ALIGN_CENTER_VERTICAL)
            hsizer.Add(icon, 0, wx.ALL, 5)
            label = hsizer
        sizer.Add(label, 0, wx.EXPAND | wx.ALL | wx.CENTER, 6)
        sizer.Add(wx.StaticText(dialog, -1, _(
            "Unhandled exception caught. Please, use the button below to report the problem."
        )), 0, wx.EXPAND | wx.ALL | wx.CENTER, 6)

        nb = wx.Notebook(dialog)
        html = cgitb.html(self._einfo)
        if isinstance(html, bytes):
            # Python 2 hack to avoid ASCII decoding error in unicode concatenation on next line.
            html = html.decode('utf-8')
        nb.AddPage(wx_text_view(nb, "<html>" + html + "</html>",
                                format=TextFormat.HTML, width=74, height=14),
                   _("Exception details"))
        nb.AddPage(wx.TextCtrl(nb, value='', name='message', size=(740, 200),
                               style=wx.TE_MULTILINE),
                   _("Your message (optional)"))
        sizer.Add(nb, 1, wx.EXPAND | wx.ALL, 6)

        if not pytis.config.sender_address:
            import subprocess
            status, domain = subprocess.getstatusoutput('hostname -f')
            if not status and domain != 'localhost':
                addr = '%s@%s' % (pytis.config.dbconnection.user(), domain)
            else:
                addr = ''
            email_ctrl = wx.TextCtrl(dialog, value=addr or '', name='from')  # size=(740, 30),
            email_ctrl.SetToolTip(_('Set your address in form "%s" to '
                                    'avoid being asked next time.',
                                    _("User interface settings")))
            sizer.Add(wx.StaticText(dialog, -1, _("Your email address:")), 0,
                      wx.TOP | wx.LEFT | wx.RIGHT, 6)
            sizer.Add(email_ctrl, 0, wx.EXPAND | wx.ALL, 6)

        button = wx.Button(dialog, -1, label=_("Send error report"))
        button.Bind(wx.EVT_BUTTON, self._on_send_bug_report)
        button.Bind(wx.EVT_UPDATE_UI, lambda e: e.Enable(bool(
            not self._sent and (pytis.config.sender_address or
                                dialog.FindWindowByName('from').GetValue() != '')
        )))
        hsizer = wx.BoxSizer(wx.HORIZONTAL)
        bitmap = wx.ArtProvider.GetBitmap(wx.ART_TICK_MARK, wx.ART_MESSAGE_BOX, (16, 16))
        icon = wx.StaticBitmap(dialog, -1, bitmap, name='icon')
        icon.Show(False)
        hsizer.Add(icon, 0, wx.ALIGN_CENTER_VERTICAL | wx.RIGHT | wx.LEFT, 6)
        hsizer.Add(wx.StaticText(dialog, -1, "", name='feedback'), 1, wx.ALIGN_CENTER_VERTICAL)
        hsizer.Add(button, 0)
        sizer.Add(hsizer, 0, wx.EXPAND | wx.ALL, 6)
        self._want_focus = button

    def _on_send_bug_report(self, event):
        to = pytis.config.bug_report_address
        if not to:
            pytis.form.run_dialog(pytis.form.Message,
                                  _("Destination address not known. The configuration option "
                                    "`bug_report_address' must be set."))
            return
        sender = pytis.config.sender_address
        if not sender:
            sender = self._dialog.FindWindowByName('from').GetValue()

        tb = self._einfo[2]
        while tb.tb_next is not None:
            tb = tb.tb_next
        subject = '{}: {} at {} line {}'.format(
            pytis.config.bug_report_subject or _("Error"),
            self._einfo[0].__name__,
            os.path.split(tb.tb_frame.f_code.co_filename)[-1],  # file name
            tb.tb_lineno,
        )

        message = self._dialog.FindWindowByName('message').GetValue().strip()
        if message:
            message += "\n\n"
        message += pytis.util.exception_info(self._einfo)

        try:
            send_mail(subject, message, to, sender,
                      message_id=email.utils.make_msgid('pytis_bugs'))
        except Exception as e:
            pytis.form.run_dialog(Error, _("Failed sending error report:") + "\n" + unistr(e))
        else:
            self._dialog.FindWindowByName('feedback').SetLabel(
                _("The report has been sent succesfully.")
            )
            self._dialog.FindWindowByName('icon').Show()
            self._dialog.Sizer.Layout()
            self._sent = True

    def _customize_result(self, result):
        label = self._button_label(result)
        if label == self._EXIT_LABEL:
            result = True
        elif label == self._IGNORE_LABEL or label is None:
            result = False
        else:
            raise ProgramError('Unknown BugReport dialog result', label)
        return result

    def _cmd_close_dialog(self):
        self._end_modal(self._button_id(self._IGNORE_LABEL))


class CheckListDialog(Message):
    """A question dialog with a list of checkable items.

    The dialog displays a question with a list of items and a checkbox for each
    of the items.

    The result returned by the `run()' method is a sequence of boolean values,
    one for each item of 'items' passed to the constructor.  The value is True
    for items which were checked and False for unchecked items.

    """
    _STYLE = GenericDialog._STYLE | wx.RESIZE_BORDER

    def __init__(self, parent, columns=(), items=(), **kwargs):
        """Arguments:
             items -- a sequence of checkable items.  Each item is a pair of
               (bool, unicode).  The bool value in indicates the initial
               checkbox state for this item.  The unicode value is the textual
               label for the item.

        """
        super(CheckListDialog, self).__init__(parent, buttons=(GenericDialog.BUTTON_OK,
                                                               GenericDialog.BUTTON_CANCEL),
                                              **kwargs)
        assert isinstance(columns, (list, tuple))
        assert isinstance(items, (list, tuple))
        self._columns = columns
        self._items = items

    def _create_content(self, sizer):
        super(CheckListDialog, self)._create_content(sizer)
        self._checklist = box = wx.CheckListBox(self._dialog,
                                                choices=[label for state, label in self._items])
        box.SetCheckedItems([i for i, (state, label) in enumerate(self._items) if state])
        sizer.Add(box, 1, wx.EXPAND | wx.ALL, 5)

    def _customize_result(self, result):
        if self._button_label(result) == self.BUTTON_OK:
            return [self._checklist.IsChecked(i) for i in range(len(self._items))]
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
        super(CheckMatrixDialog, self).__init__(parent, buttons=(GenericDialog.BUTTON_OK,
                                                                 GenericDialog.BUTTON_CANCEL),
                                                **kwargs)
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
        grid = wx.FlexGridSizer(len(self._rows) + 1, len(self._columns) + 1, 2, 6)
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
        sizer.Add(panel, 1, wx.EXPAND | wx.ALL, 5)

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


class AggregationSetupDialog(GenericDialog):
    """A dialog for setting up an aggregated form.

    The result returned by the `run()' is a tuple of two tuples
    (name, group_by_columns, aggregation_columns).

    name -- user supplied human readable title of the aggregated view for
      further reference (the values of group_by_columns and aggregation_columns
      may be stored and further used under this title).

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
                 name, group_by_columns, aggregation_columns, aggregation_valid,
                 title=_("Aggregated view parameters")):
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
             name -- user supplied human readable name as in the result of
               run() described in the class docstring.
             group_by_columns -- preselected group by columns in the same
               format as in the result of run() as described in the class
               docstring.
             aggregation_columns -- preselected aggregation columns in the same
               format as in the result of run() as described in the class
               docstring.
        """
        super(AggregationSetupDialog, self).__init__(parent, title=title,
                                                     buttons=(GenericDialog.BUTTON_OK,
                                                              GenericDialog.BUTTON_CANCEL))
        self._aggregation_functions = aggregation_functions
        self._grouping_functions = grouping_functions
        self._columns = columns
        self._aggregation_valid = aggregation_valid
        self._name = name
        self._group_by_columns = group_by_columns
        self._aggregation_columns = aggregation_columns

    def _create_content(self, sizer):
        super(AggregationSetupDialog, self)._create_content(sizer)
        self._name_control = wx_text_ctrl(self._dialog, value=self._name, length=50,
                                          tooltip=_("Enter the name for saving the view, or "
                                                    "leave empty, if you prefer not to save it."))
        box = wx.BoxSizer(wx.HORIZONTAL)
        box.Add(wx.StaticText(self._dialog, -1, _("Title") + ':'), wx.ALL, 3)
        box.Add(self._name_control)
        sizer.Add(box, 0, wx.EXPAND | wx.ALL, 5)
        panel = wx.ScrolledWindow(self._dialog, style=wx.TAB_TRAVERSAL)
        panel.SetScrollRate(20, 20)
        self._grid = grid = wx.FlexGridSizer(len(self._columns) + 1,
                                             len(self._aggregation_functions) + 2, 2, 6)
        self._grouping_controls = []
        self._aggregation_controls = []
        for label in ['', _("Group by")] + [x[1] for x in self._aggregation_functions]:
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
                cp = wx.CollapsiblePane(panel, label=_("Function"), style=wx.CP_DEFAULT_STYLE)
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
        sizer.Add(panel, 1, wx.EXPAND | wx.ALL, 5)

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
            self._name = self._name_control.GetValue()
            self._group_by_columns = [spec for spec, checkbox in self._grouping_controls
                                      if checkbox.IsChecked()]
            self._aggregation_columns = [spec for spec, checkbox in self._aggregation_controls
                                         if checkbox.IsChecked()]
            if not self._group_by_columns:
                pytis.form.run_dialog(Warning,
                                      _("You need to select at least one grouping column."))
                return
        return super(AggregationSetupDialog, self)._on_button(event)

    def _customize_result(self, result):
        if self._button_label(result) == self.BUTTON_OK:
            return self._name, tuple(self._group_by_columns), tuple(self._aggregation_columns)
        else:
            return None


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
                 wildcards=(_("All files") + " (*.*)|*.*",),
                 multi=False, overwrite_prompt=True):
        """Inicializuj dialog.

        Argumenty:

          parent -- wx rodič; instance 'wx.Frame' nebo 'wx.Dialog'
          title -- titulek dialogového okna jako string; pokud je None, bude
            doplněn výchozí titulek v závislosti na argumentu 'mode'.
          dir -- přednastavená cesta; řetězec, nebo None.
          file -- přednastavený název souboru; řetězec, nebo None.
          mode -- typ dialogu; jedna z konstant 'OPEN' a 'SAVE' třídy.
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
            title = {(FileDialog.OPEN, False): _("Open file"),
                     (FileDialog.OPEN, True): _("Open files"),
                     (FileDialog.SAVE, False): _("Save file"),
                     (FileDialog.SAVE, True): _("Save files")}[(mode, multi)]
        assert dir is None or isinstance(dir, basestring)
        assert file is None or isinstance(file, basestring)
        self._title = unistr(title)
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
        directory = self._dir or FileDialog._last_directory.get(self._mode, '')
        style = {FileDialog.OPEN: wx.FD_OPEN,
                 FileDialog.SAVE: wx.FD_SAVE}[self._mode]
        if self._multi and self._mode == FileDialog.OPEN:
            style = style | wx.FD_MULTIPLE
        if self._overwrite_prompt and self._mode == FileDialog.SAVE:
            style = style | wx.FD_OVERWRITE_PROMPT
        self._dialog = d = wx.FileDialog(self._parent,
                                         message=self._title,
                                         defaultDir=directory,
                                         defaultFile=self._file or '',
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


class DirDialog(Dialog):
    """Dialog for directory selection.

    Displays a dialog to browse existing directories and also allows creation
    of a new directory.

    """

    _last_directory = None

    def __init__(self, parent, title=_("Directory selection"), path=None):
        """Arguments:

          parent -- wx parent; 'wx.Frame' or 'wx.Dialog' instance
          title -- Title to show in the dialog title bar.
          path -- initial derectory or None to use the last selected directory.

        """
        super_(FileDialog).__init__(self, parent)
        assert isinstance(title, basestring)
        assert path is None or isinstance(path, basestring)
        self._title = unistr(title)
        self._path = path

    def run(self):
        """Zobraz dialog a vrať cestu k vybranému souboru jeko řetězec.

        Pokud je argument konstruktoru 'multi' pravdivý, bude vrácen tuple
        řetězců.

        """
        self._dialog = d = wx.DirDialog(self._parent,
                                        message=self._title,
                                        defaultPath=self._path or DirDialog._last_directory or '',
                                        style=wx.DD_DEFAULT_STYLE)
        result = d.ShowModal()
        path = d.GetPath()
        d.Destroy()
        DirDialog._last_directory = path
        if result == wx.ID_OK:
            return path
        else:
            return None
