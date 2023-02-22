# -*- coding: utf-8 -*-

# Copyright (C) 2018-2023 Tomáš Cerha <t.cerha@gmail.com>
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

"""Specifikace a zpracování uživatelských prvků hlavní obrazovky.

Modul zavádí prvky společné pro všechny formuláře: menu, stavový řádek,
nápovědu, atd.  Tyto prvky se mohou vyskytovat ve všech formulářích, avšak
v každém mohou mít jiný obsah.

Modul definuje jak třídy sloužící ke specifikaci konkrétních prvků, tak
i třídy, které tyto specifikace následně zpracovávají.

"""
from past.builtins import basestring
from builtins import range, str
from future import standard_library
from future.utils import python_2_unicode_compatible

import sys
import copy
import os
import string
import types
import wx
import wx.html2
import http.server
import socketserver
import io
import threading
import time
import mimetypes
import fitz

import lcg

import wx.lib.pdfviewer
import wx.lib.agw.supertooltip as supertooltip

import pytis
import pytis.api
import pytis.form
import pytis.output
import pytis.presentation
import pytis.util
from pytis.api import app
from pytis.presentation import Orientation, TextFormat, StatusField
from pytis.util import find, xtuple, log, DEBUG, EVENT, OPERATIONAL, ProgramError
from .event import wx_callback, top_level_exception
from .command import Command, CommandHandler, UICommand, command_icon
from .managers import FormProfileManager


# Needed for urllib.parse (urlparse in Python 2).
standard_library.install_aliases()
unistr = type(u'')  # Python 2/3 transition hack.

if pytis.config.http_proxy is not None:
    # On Linux, proxy can be set through webkitgtk library (used by the GTK wx.html2.WebView
    # implementation).  Setting up http_proxy will currently fail on other platforms.
    import ctypes
    try:
        libgobject = ctypes.CDLL('libgobject-2.0.so.0')
        libsoup = ctypes.CDLL('libsoup-2.4.so.1')
        libwebkitgtk = ctypes.CDLL('libwebkitgtk-1.0.so.0')
    except OSError as e:
        raise ProgramError("HTTP proxy is only supported on Linux with specific libraries. "
                           "Install the missing libraries or unset http_proxy: %s" % e)
    else:
        proxy_uri = libsoup.soup_uri_new(pytis.config.http_proxy)
        session = libwebkitgtk.webkit_get_default_session()
        libgobject.g_object_set(session, "proxy-uri", proxy_uri, None)

for const in ('EVT_WEBVIEW_NAVIGATING', 'EVT_WEBVIEW_NAVIGATED', 'EVT_WEBVIEW_LOADED',
              'EVT_WEBVIEW_ERROR', 'EVT_WEBVIEW_TITLE_CHANGED', 'WEBVIEW_RELOAD_NO_CACHE'):
    # wxPython on MAC OS X uses a different naming scheme.  We don't want
    # to care about it later, so we just duplicate the used identifiers here.
    if not hasattr(wx.html2, const):
        setattr(wx.html2, const, getattr(wx.html2, const.replace('WEBVIEW', 'WEB_VIEW')))


_ = pytis.util.translations('pytis-wx')

# TODO: The background color should be taken from system settings, but
# wx.SYS_COLOUR_WINDOW returns white and wx.SYS_COLOUR_BACKGROUND returns
# gray which is darker than it should be, so we rather hard code the color
# which looks fine with the default GTK+ color theme.
# DEFAULT_WINDOW_BACKGROUND_COLOUR = wx.SystemSettings.GetColour(wx.SYS_COLOUR_BACKGROUND)
DEFAULT_WINDOW_BACKGROUND_COLOUR = '#e8e8e8'

FIELD_PADDING = (24, 11)
"""Text field padding as a tuple of (x-padding, y-padding) in pixels.

This padding is used to compute the size of a wx.TextCtrl in 'field_size()'.

"""
if wx.version().split(' ')[1] == 'gtk2':
    FIELD_PADDING = (24, 17)


def field_size(parent, width, height):
    """Return pixel size of a wx.TextCtrl fitting text of given character width/height.

    Text fields are the most common fields so some other widgets, such as
    buttons etc. may also use 'field_size()' to match text field sizes and
    align with them nicely.

    Note, that the width computation can not exactly suit to any text when
    proportional fonts are used.  It usually suits an average text with even
    occurence of wide and narrow letters.  The computation is tuned to make
    small fields (up to three chars) wide enough to fit exactly the given
    number of letters even if these are the widest possible capital letters,
    such as G.  With higher width values (4 and above) the wide letters will
    stop to fit but the field size will match well for numbers.  So if wide
    letters are expected in field values, their width must be proportionally
    set higher than the number of characters intended to fit.

    Returns: Pixel size as a tuple of two integers (width, height)

    """
    return tuple(a + b for a, b in zip(char2px(parent, width, height), FIELD_PADDING))


# Utility functions


def beep():
    """Pípni."""
    wx.Bell()


def microsleep(miliseconds=100):
    """Čekej a nic nedělej po dobu 'miliseconds'."""
    wx.MicroSleep(miliseconds)


def busy_cursor(enable):
    """Zapni nebo vypni busy cursor.

    Argumenty:

      enable -- je-li pravdivé, bude kurzor zapnut, v opačném případě bude
        vypnut

    Poznámka: Hlavní okno aplikace automaticky busy cursor vypíná v idle
    handleru.

    """
    if enable:
        if not wx.IsBusy():
            wx.BeginBusyCursor()
    else:
        if wx.IsBusy():
            wx.EndBusyCursor()


def is_busy_cursor():
    """Vrať pravdu, právě když je nastaven busy cursor."""
    return wx.IsBusy()


def modal(window):
    """Vrať pravdu právě když je 'window' modálním oknem.

    'window' je považováno za modální, jestliže je instancí třídy 'PopupForm'
    nebo 'Dialog'.

    """
    return (window and
            (isinstance(window, pytis.form.Dialog) or isinstance(window, pytis.form.PopupForm)))


def copy_to_clipboard(text):
    """Copy given text into system clipboard."""
    assert isinstance(text, basestring)
    log(EVENT, 'Copy text to system clipboard.')
    # Using the wx clipboard doesn't work depending on how the application is
    # run.  It works when run natively on linux and also with newer versions of
    # nx, but it doesn't work when pytis is used on Windows through an X
    # server or on Cygwin.
    # clipboard = wx.TheClipboard
    # if clipboard.Open():
    #     clipboard.SetData(wx.TextDataObject(text))
    #     clipboard.Flush()
    #     clipboard.Close()
    # The following solution is is quite a hack, but it works consistently...
    ctrl = wx.TextCtrl(pytis.form.wx_frame(), -1, text)
    ctrl.SetSelection(0, len(text))
    ctrl.Copy()
    ctrl.Destroy()


def paste_from_clipboard(ctrl):
    """Paste text from clipboard to 'wx.TextCtrl' widget.

    The text will be pasted from the current preferred clipboard -- the remote
    windows clipboard if it is available or the local system clipboard
    otherwise.  Except for the source of the text, it should behave exactly as
    'wx.TextCtrl.Paste()'.

    """
    assert isinstance(ctrl, wx.TextCtrl)
    log(EVENT, 'Paste from clipboard')
    if not wx.TheClipboard.IsOpened():  # may crash, otherwise
        do = wx.TextDataObject()
        wx.TheClipboard.Open()
        success = wx.TheClipboard.GetData(do)
        wx.TheClipboard.Close()
        if success:
            text = do.GetText()
            text_length = len(text)
            orig_text = ctrl.GetValue()
            if orig_text:
                from_, to_ = ctrl.GetSelection()
                if from_ != to_:
                    text = orig_text[:from_] + text + orig_text[to_:]
                    point = from_ + text_length
                else:
                    point = ctrl.GetInsertionPoint()
                    text = orig_text[:point] + text + orig_text[point:]
                    point = point + text_length
            else:
                point = text_length
            ctrl.ChangeValue(text)
            ctrl.SetInsertionPoint(point)


def hotkey_string(hotkey):
    """Return the human readable hotkey representation of Keymap.lookup_command() result."""
    return ' '.join([k.replace(' ', _("Space")) for k in hotkey])


def file_menu_items(fields, row, select_arguments):

    from .application import Application
    data = row.data(init_select=False)

    def field_not_null(field_id):
        value = row[field_id]
        t = value.type()
        if isinstance(t, pytis.data.Big) and data.find_column(field_id):
            # Big values are not included in list form select.
            key_id = data.key()[0].id()
            data.select(
                condition=pytis.data.AND(pytis.data.EQ(key_id, row[key_id]),
                                         pytis.data.NE(field_id, pytis.data.Value(t, None))),
                arguments=select_arguments, columns=(key_id,), transaction=row.transaction(),
            )
            result = data.fetchone() is not None
            data.close()
        else:
            result = value.value() is not None
        return result

    def field_data(field_id):
        value = row[field_id]
        if isinstance(value.type(), pytis.data.Big) and data.find_column(field_id):
            # Big values are not included in list form select.
            key_id = data.key()[0].id()
            data.select(condition=pytis.data.EQ(key_id, row[key_id]),
                        columns=(field_id,), transaction=row.transaction(),
                        arguments=select_arguments)
            complete_row = data.fetchone()
            data.close()
            if complete_row:
                value = complete_row[field_id]
        if isinstance(value.type(), pytis.data.Binary):
            result = value.value()
        elif value.value() is not None:
            result = value.export()
        else:
            result = None
        return result

    def open_file(field_id, filename):
        app.launch_file(data=field_data(field_id), suffix=os.path.splitext(filename)[1])

    def can_open(fspec):
        crypto_name = fspec.crypto_name()

        def can_open_file(field_id, filename):
            return crypto_name is None or crypto_name in app.decrypted_areas()
        return can_open_file

    mitems = []
    for f in fields:
        fid = f.id()
        filename = row.filename(fid)
        if filename is not None and field_not_null(fid):
            command = Application.COMMAND_HANDLED_ACTION(handler=open_file,
                                                         field_id=fid,
                                                         filename=filename,
                                                         enabled=can_open(f))
            mitems.append(MItem(_('Open file "%s"', filename), command=command,
                                help=_('Open the value of field "%s" as a file.', f.label())))
    return mitems


# Utility classes


class WxKey(object):
    """Práce s reprezentací kláves.

    Třída umožňuje porovnat událost s definicí klávesy.  V budoucnu mohou být
    její funkce rozšířeny.

    """
    _M_ALT = 'ALT'
    _M_CTRL = 'CTRL'

    _TRANS_TABLE = None
    _RTRANS_TABLE = None

    def __init__(self):
        # Musí to být až tady, kvůli (ne)importům wx na serveru
        table = (
            ('Insert', wx.WXK_INSERT),
            ('Delete', wx.WXK_DELETE),
            ('Backspace', wx.WXK_BACK),
            ('Home', wx.WXK_HOME),
            ('End', wx.WXK_END),
            ('Prior', wx.WXK_PAGEUP),
            ('Next', wx.WXK_PAGEDOWN),
            ('Up', wx.WXK_UP),
            ('Down', wx.WXK_DOWN),
            ('Left', wx.WXK_LEFT),
            ('Right', wx.WXK_RIGHT),
            ('Escape', wx.WXK_ESCAPE),
            ('Tab', wx.WXK_TAB),
            ('Enter', wx.WXK_RETURN),
            ('Enter', wx.WXK_NUMPAD_ENTER),
            ('Home', wx.WXK_NUMPAD_HOME),
            ('Left', wx.WXK_NUMPAD_LEFT),
            ('Up', wx.WXK_NUMPAD_UP),
            ('Right', wx.WXK_NUMPAD_RIGHT),
            ('Down', wx.WXK_NUMPAD_DOWN),
            ('Prior', wx.WXK_NUMPAD_PAGEUP),
            ('Next', wx.WXK_NUMPAD_PAGEDOWN),
            ('End', wx.WXK_NUMPAD_END),
            ('Home', wx.WXK_NUMPAD_BEGIN),
            ('Insert', wx.WXK_NUMPAD_INSERT),
            ('=', wx.WXK_NUMPAD_EQUAL),
            ('*', wx.WXK_NUMPAD_MULTIPLY),
            ('+', wx.WXK_NUMPAD_ADD),
            ('-', wx.WXK_NUMPAD_SUBTRACT),
            ('.', wx.WXK_NUMPAD_DECIMAL),
            ('/', wx.WXK_NUMPAD_DIVIDE),
            ('0', wx.WXK_NUMPAD0),
            ('1', wx.WXK_NUMPAD1),
            ('2', wx.WXK_NUMPAD2),
            ('3', wx.WXK_NUMPAD3),
            ('4', wx.WXK_NUMPAD4),
            ('5', wx.WXK_NUMPAD5),
            ('6', wx.WXK_NUMPAD6),
            ('7', wx.WXK_NUMPAD7),
            ('8', wx.WXK_NUMPAD8),
            ('9', wx.WXK_NUMPAD9),
            ('F1', wx.WXK_F1),
            ('F2', wx.WXK_F2),
            ('F3', wx.WXK_F3),
            ('F4', wx.WXK_F4),
            ('F5', wx.WXK_F5),
            ('F6', wx.WXK_F6),
            ('F7', wx.WXK_F7),
            ('F8', wx.WXK_F8),
            ('F9', wx.WXK_F9),
            ('F10', wx.WXK_F10),
            ('F11', wx.WXK_F11),
            ('F12', wx.WXK_F12),
        )
        if self._TRANS_TABLE is None:
            self.__class__._TRANS_TABLE = dict(table)
            self.__class__._RTRANS_TABLE = dict([(v, k) for k, v in table])
        self._cache = {}

    def _key2wx(self, key):
        try:
            return self._cache[key]
        except KeyError:
            pass
        if key.find('Alt-') == 0:
            modifier = self._M_ALT
            skey = key[len('Alt-'):]
        elif key.find('Ctrl-') == 0:
            modifier = self._M_CTRL
            skey = key[len('Ctrl-'):]
        else:
            modifier = None
            skey = key
        try:
            code = self._TRANS_TABLE[skey]
        except KeyError:
            code = ord(skey.upper())
        if modifier == self._M_CTRL:
            if len(skey) == 1 and skey in string.letters:
                code = 1 + (code - ord('A'))
            elif skey == ' ':
                code = 0
        result = modifier, code
        self._cache[key] = result
        return result

    def is_true_key(self, event):
        """Vrať pravdu, práve když 'event' neodpovídá jen modifikátoru."""
        code = event.GetKeyCode()
        # Chybí symboly pro Meta a Alt, takže natvrdo 307...
        return code not in (wx.WXK_SHIFT, wx.WXK_CONTROL, 307)

    def is_event_of_key(self, event, key):
        """Vrať pravdu, právě když 'event' byla vyvolána 'key'.

        Argument:

          event -- instance wx.Event
          key -- string definující klávesu dle specifikace v modulu 'command'

        """
        if not isinstance(event, wx.KeyEvent) or key is None:
            return False
        modifier, code = self._key2wx(key)
        if modifier:
            if modifier == self._M_ALT:
                if not event.AltDown():
                    return False
            elif modifier == self._M_CTRL:
                if not event.ControlDown():
                    return False
            else:
                raise ProgramError('Unhandled modifier', modifier)
        else:
            # zde nepoužívat event.HasModifiers(), protože ta vrací
            # při zapnutém NumLocku vždy pravdu.
            if event.AltDown() or event.ControlDown():
                return False
        return code == event.GetKeyCode()

    def event_key(self, event):
        """Vrať stringovou podobu klávesové události 'event'.

        Ne všechny události musí vracet rozumnou nebo správnou stringovou
        podobu, podporovány jsou pouze rozeznávané klávesové události.

        """
        prefix = ''
        if event.ControlDown():
            prefix = 'Ctrl-'
        if event.AltDown():
            prefix = prefix + 'Alt-'
        if event.ShiftDown():
            prefix = prefix + 'Shift-'
        code = event.GetKeyCode()
        try:
            key = self._RTRANS_TABLE[code]
        except KeyError:
            if code >= 1 and code <= 26:
                key = chr(code + 96)
            else:
                try:
                    key = chr(code).lower()
                except Exception:
                    key = '???'
        return prefix + key


# Common handlers


@python_2_unicode_compatible
class Keymap(object):
    """Klávesová mapa.

    Klávesová mapa umožňuje definovat přiřazení příkazů a případně jejich
    argumentů klávesám.

    """
    def __init__(self, parent=None):
        """Inicializuj instanci.

        Argumenty:

          parent -- rodičovská klávesová mapa; buď 'None' (pak klávesová mapa
            je čistá), nebo instance 'Keymap' (pak se podědí všechny klávesy
            z dané klávesové mapy, pokud nejsou předefinovány).  Argument není
            nutno klíčovat.

        """
        if parent is None:
            keymap = {}
        else:
            keymap = dict([(key, copy.copy(keydef))
                           for key, keydef in parent._keymap.items()])
        self._keymap = keymap

    def __str__(self):
        return '<Keymap: %s>' % (self._keymap,)

    def _define_key(self, key, command, args):
        prefix, rest = key[0], key[1:]
        try:
            keydef = self._keymap[prefix]
        except KeyError:
            keydef = []
        if rest and not isinstance(keydef, Keymap):
            if keydef:
                log(OPERATIONAL, "Key '%s' is already used as non-prefix key." % (prefix,))
                return
            keydef = Keymap(None)
        self._keymap[prefix] = keydef
        if isinstance(keydef, list):
            keydef[0:0] = [(command, args)]
        elif rest:
            keydef._define_key(rest, command, args)
        else:
            log(OPERATIONAL, "Key '%s' is already used as a prefix key." % (prefix,))

    def define_key(self, key, command, args={}):
        """Přiřaď klávese 'key' příkaz 'command' s argumenty '**kwargs'.

        Argumenty:

          key -- řetězec, resp. sekvence řetězců, definující klávesu,
            resp. sekvenci kláves; popis viz níže

          command -- přiřazený příkaz, instance třídy 'Command'

          args -- parametry příkazu, předané při jeho vyvolání obslužné metodě

        Přiřazované klávesy jsou řetězce sestavené dle následujících pravidel:

        - Klávesa odpovídající znaku anglické abecedy je reprezentována
          řetězcem rovným tomuto znaku.  Velikost písmen je brána v potaz (viz.
          také dále modifikátor Shift).

        - Funkční klávesy F1 až F12 se zapisují řetězci 'F1' až 'F12'.

        - Šipky se zapisují řetězci 'Up', 'Down', 'Left', 'Right'.

        - Klávesy 'Escape', 'Enter', 'Tab', 'Insert', 'Delete', 'Backspace',
          'Home', 'End', 'Prior' a 'Next' se zapisují stejnojmennými řetězci.

        - Klávesa s modifikátorem Control je zapsána ve formátu 'Ctrl-<KEY>',
          kde '<KEY>' je zápis klávesy bez tohoto modifikátoru.

        - Klávesa s modifikátorem Alt je zapsána ve formátu 'Alt-<KEY>', kde
          '<KEY>' je zápis klávesy bez tohoto modifikátoru.

        - Klávesa s modifikátorem Shift je zapsána ve formátu 'Shift-<KEY>'.

        - Lze používat více modifikátorů současně.  Potom jsou modifikátory
          zapisovány vždy v pořadí Ctrl, Alt, Shift (např. tedy Ctrl-Alt-s nebo
          Alt-Shift-Tab, nikoliv potom Alt-Ctrl-x).

        """
        key = xtuple(key)
        if key != (None,):
            self._define_key(key, command, args)

    def lookup_key(self, key):
        """Vrať definici asociovanou s klávesou 'key'.

        Argumenty:

          key -- string popisující klávesu v notaci uvedené v docstringu třídy

        Vrací: Je-li na klávesu napojen příkaz, vrať dvojici (COMMAND, ARGS),
          kde COMMAND je instance třídy 'Command' a ARGS jsou jeho argumenty
          jako dictionary pro předání obslužné metodě.  Je-li na klávesu
          připojena klávesová mapa (v případě víceklávesových definic), je
          vrácena tato mapa jako instance třídy 'Keymap'.  Není-li klávesa
          definována, vrať 'None'.

        """
        try:
            return self._keymap[key]
        except KeyError:
            return None

    def lookup_command(self, command, args={}):
        """Vrať klávesovou zkratku asociovanou s daným příkazem a argumenty.

        Argumenty:

          command -- příkaz, instance třídy 'Command'
          args -- argumenty příkazu jako dictionary.

        Vrací: Je-li příkaz s danými argumenty napojen na nějakou klávesu, vrať
          definici klávesové zkratky jako tuple (vždy, byť jednoprvkový).
          Není-li pro příkaz s danými argumenty klávesa definována (předchozím
          voláním metody 'define_key()'), vrať 'None'.

        """
        for key, keydef in self._keymap.items():
            if isinstance(keydef, Keymap):
                k = keydef.lookup_command(command, args)
                if k is not None:
                    return (key,) + k
            else:
                if (command, args) in keydef:
                    return (key,)
        return None

    def keys(self):
        """Vrať seznam všech platných kláves jako řetězců."""
        return self._keymap.keys()


class KeyHandler(object):
    """Třída schopná převádět klávesové události na příkazy.

    Třída v konstruktoru registruje pro zpracování kláves metodu
    'on_key_down()', která zajišťuje převod na klávesy na příkaz a vyvolání
    jeho obslužné metody.  Ve třídě se vytvoří klávesová mapa poskládaná
    z kláves příkazů instance oné třídy plus všech jejích poručníků.  Při
    konfliktu kláves mají přednost ty bližší dané třídě.

    Třída je určena k \"přidědění\" ve všech třídách, které chtějí samy
    odchytávat klávesové události.

    """

    def __init__(self, widgets=None):
        """Inicializuj instanci.

        Argumenty:

          widgets -- wx widget, nebo jejich sekvence, pro který má být
            definován handler klávesy; může být i 'None', v kterémžto případě
            bude oním widgetem 'self'

        """
        if widgets is None:
            widgets = isinstance(self, wx.Window) and (self,) or ()
        self._handle_keys(*xtuple(widgets))
        self._wx_key = WxKey()
        self._prefix_key_sequence = []
        self._commands = None
        if not hasattr(self, 'keymap'):
            self.keymap = None
        self._current_keymap = self.keymap
        try:
            key_guardian = self.guardian()
            while not isinstance(key_guardian, KeyHandler):
                key_guardian = key_guardian.guardian()
        except Exception:
            key_guardian = None
        self._key_guardian = key_guardian

    def _handle_keys(self, *widgets):
        """Registruj se pro ošetření klávesových událostí daných UI prvků."""
        for widget in widgets:
            wx_callback(wx.EVT_KEY_DOWN, widget, self.on_key_down)

    def _init_commands(self):
        # Nemůžeme `_commands' inicializovat hned v konstruktoru, protože
        # tou dobou ještě nemusí být všechny příkazy ve třídě definovány.
        commands = []
        for attrname in pytis.util.public_attributes(self.__class__):
            if attrname.startswith('COMMAND_'):
                command = getattr(self.__class__, attrname)
                if isinstance(command, Command):
                    commands.append(command)
        # Do atributu přiřazujeme až nyní, aby to bylo odolnější vláknům
        self._commands = commands

    def _maybe_invoke_command(self, key_commands):
        for command, kwargs in key_commands:
            if self._commands is None:
                self._init_commands()
            if command in self._commands and command.enabled(**kwargs):
                if __debug__:
                    log(DEBUG, 'Nalezen příkaz klávesy', (command, kwargs))
                command.invoke(**kwargs)
                return True

        else:
            guardian = self._key_guardian
            if guardian is None:
                if __debug__:
                    log(DEBUG, 'Žádný další poručník')
                return False
            else:
                if __debug__:
                    log(DEBUG, 'Předání poručníkovi:', guardian)
                return guardian._maybe_invoke_command(key_commands)

    def _get_keymap(self):
        if self.keymap is None:
            guardian = self._key_guardian
            if guardian is None:
                gkeymap = pytis.form.global_keymap()
            else:
                gkeymap = guardian._get_keymap()
            self.keymap = Keymap(gkeymap)
            if __debug__:
                log(DEBUG, 'Vytvořena klávesová mapa', (self, self.keymap))
        return self.keymap

    def define_key(self, key, command, args):
        """Definuj klávesovou zkratku v klávesové mapě této instance.

        Klávesová mapa nemusí být dostupná v době inicializace instance, takže
        není možné definovat klávesové zkratky přímo.  Tato metoda zaručuje, že
        předané klávesové zkratky budou dříve nebo později správně uplatněny.

        Argumenty jsou shodé jako v metodě 'Keymap.define_key()'.

        """
        keymap = self._get_keymap()
        keymap.define_key(key, command, args)

    def on_key_down(self, event, dont_skip=False):
        """Zpracuj klávesovou událost 'event'.

        Pokud existuje v instanci příkaz napojený na danou klávesu, zavolej
        jeho obslužnou metodu.  Pokud takový příkaz neexistuje nebo pokud
        obslužná metoda odmítne příkaz zpracovat (vrátí nepravdu), ponech
        'event' k dalšímu zpracování.

        Argumenty:

          event -- klávesová wx událost
          dont_skip -- právě když je pravdivé, není proveden skip události,
            i když neodpovídá žádnému příkazu

        Vrací: Pravdu, právě když událost byla úspěšně převedena na příkaz.

        """
        if __debug__:
            log(DEBUG, 'Stisk klávesy:', event)
        wk = self._wx_key
        if not wk.is_true_key(event):
            return
        app.echo('')
        if __debug__:
            log(DEBUG, 'Událost zpracovává:', str(self))
        guardian = self._key_guardian
        if self._commands is None:
            self._init_commands()
        if ((self._current_keymap is None or
             not isinstance(pytis.form.last_user_event(), wx.KeyEvent))):
            self._current_keymap = self._get_keymap()
        if __debug__:
            log(DEBUG, 'Aktuální klávesová mapa:', str(self._current_keymap))
        key = wk.event_key(event)
        keydef = self._current_keymap.lookup_key(key)
        if isinstance(keydef, Keymap):
            if __debug__:
                log(DEBUG, 'Prefixová klávesa', keydef)
            self._prefix_key_sequence.append(key)
            app.echo(_("Prefix key: %s", '%s (%s)' %
                       (' '.join(self._prefix_key_sequence), ', '.join(keydef.keys()),)))
            self._current_keymap = keydef
            return True
        else:
            # Pozor, wxWidgets je debilní a ne vždy předává události rodičům!
            self._current_keymap = None
            self._prefix_key_sequence = []
            if keydef is not None:
                result = self._maybe_invoke_command(keydef)
                if result:
                    return result
            if guardian:
                if __debug__:
                    log(DEBUG, 'Klávesa předána výše')
                return guardian.on_key_down(event, dont_skip)
            if dont_skip:
                if __debug__:
                    log(DEBUG, 'Klávesa ignorována')
            else:
                if __debug__:
                    log(DEBUG, 'Klávesová událost přeskočena')
                event.Skip()
        return False


class CallbackHandler(object):
    """Mixin třída pro prvky podporující nastavování a spouštění callbacků.

    Třída, která podědí 'CallbackHandler', získá metody 'set_callback()' a
    '_run_callback()'.  Metoda 'set_callback()' je určena pro použití ze
    strany uživatele odvozené třídy -- nastavuje funkci, která má být vyvolána
    pro ošetření určité akce.  Naproti tomu metoda '_run_callback()' je určena
    pro použití uvnitř odvozené třídy v místech, kde má být tato funkce
    vyvolána.

    Odvozená třída musí definovat konstanty s prefixem 'CALL_', jejichž hodnoty
    slouží k rozlišení jednotlivých druhů callbacků.

    """
    def __init__(self):
        self._callbacks = {}

    def set_callback(self, kind, function):
        # Toto musí být samostatná metoda a nejen parametr konstruktoru mimo
        # jiné kvůli cyklickým callbackovým závislostem v duálním formuláři.
        """Nastav 'function' pro callback 'kind'.

        Pokud byla pro callback 'kind' předtím nastavena jiná funkce, je toto
        předchozí nastavení zrušeno.

        Argumenty:

          kind -- druh callbacku, jedna z 'CALL_*' konstant třídy
          function -- funkce, která má být vyvolána.  Počet a význam argumentů
            je dán odvozenou třídou a měl by být zdokumentovám v rámci
            její dokumentace.

        """
        assert kind[:5] == 'CALL_' and hasattr(self, kind), kind
        assert function is None or callable(function), function
        self._callbacks[kind] = function
        if __debug__:
            log(DEBUG, 'Callback registered:', (kind, function))

    def get_callback(self, kind):
        return self._callbacks.get(kind)

    def _run_callback(self, kind, *args, **kwargs):
        """Vyvolej funkci pro ošetření callbacku 'kind'.

        Pokud nebyla funkce pro ošetření daného callbacku předtím nastavena
        metodou 'set_callback()', nedělej nic a vrať 'False', jinak vracej
        'True'.

        Argumenty:

          kind -- druh callbacku, jedna z 'CALL_*' konstant třídy

          args, kwargs -- argumenty volané funkce.  Počet a význam argumentů je
            dán odvozenou třídou a měl by být zdokumentovám v rámci dokumentace
            callbackové konstanty.

        """
        try:
            callback = self._callbacks[kind]
        except KeyError:
            return False
        if callback:
            if __debug__:
                log(DEBUG, 'Invoking callback:', (kind, callback))
            callback(*args, **kwargs)
            return True

# Specialized gadgets


# Menu


class _MenuObject(object):
    """Společný předek všech tříd specifikujících strukturu menu."""


class MSeparator(_MenuObject):
    """Oddělovač položek menu.

    Pokud se mezi položkami menu vyskytne instance této třídy, bude na jejím
    místě vytvořen vizuální oddělovač.

    """


class _TitledMenuObject(_MenuObject):

    def __init__(self, title):
        """Initializuj instanci.

        Argumenty:

          title -- název menu, string

        'title' je vždy považován za jazykově závislý text a tudíž automaticky
        podléhá jazykové konverzi.

        """
        assert isinstance(title, basestring)
        self._title = title

    def title(self, raw=False):
        """Vrať titulek menu zadaný v konstruktoru jako string."""
        if raw:
            return self._title
        else:
            return self._title.replace('&', '')


class Menu(_TitledMenuObject):
    """Specifikace menu.

    Menu je dáno svým popisem a položkami.  Položky mohou být buď vlastní
    aktivační položky (instance třídy 'MItem'), oddělovače (instance třídy
    'MSeparator') nebo vnořená menu (instance třídy 'Menu').  U této třídy
    nerozlišujeme, zda se jedná o pull-down menu, pop-up menu nebo vnořené
    menu, specifikace je stejná pro všechny typy menu.

    Z vytvořené instance této třídy lze potom vytvořit instanci wxMenu pomocí
    metody 'create()'.

    """
    def __init__(self, title, items, allow_autoindex=True):
        """Initialize menu specification.

        Arguments:

          title -- menu title as a string
          items -- sequence of menu items as 'Menu', 'MItem' and 'MSeparator' instances
          allow_autoindex -- allow automatic keyboard access index numbers on this menu

        """
        assert isinstance(items, (tuple, list))
        if __debug__:
            for i in items:
                # Empty tuple is possible for items like 'recent_forms_menu' by generating help
                assert isinstance(i, _MenuObject) or i == ()
        self._items = tuple(items)
        self._allow_autoindex = allow_autoindex
        self._wx_menu = None
        super(Menu, self).__init__(title)

    def items(self):
        """Vrať sekvenci položek menu zadanou v konstruktoru."""
        return self._items

    def _on_highlight_item(self, menu, event):
        if event.GetMenuId() != -1:
            item = menu.FindItemById(event.GetMenuId())
            if item:
                app.echo(item.GetHelp())
        event.Skip()

    def create(self, parent, keymap=None):
        """Vytvoř menu dle specifikace a vrať instanci 'wx.Menu'.

        Tato metoda zkonstruuje menu včetně všech vnořených podmenu, přičemž
        zabezpečí veškeré navázání událostí apod.

        Argumenty:

          parent -- wx rodič vytvářené instance 'wx.Menu'
          keymap -- klávesová mapa (instance 'Keymap'), která má být
             synchronizována s klávesovými zkratkami položek menu.

        """
        self._wx_menu = menu = wx.Menu()
        wx_callback(wx.EVT_MENU_HIGHLIGHT_ALL, menu,
                    lambda event: self._on_highlight_item(menu, event))
        # At first, compute the maximal width of hotkey string in this menu.
        max_hotkey_width = 0
        hotkey_str = {}
        for i in self._items:
            if isinstance(i, MItem):
                hotkey, command, args = i.hotkey(), i.command(), i.args()
                if hotkey == (None,) and keymap is not None:
                    real_args = dict([(k, v) for k, v in args.items()
                                      if k != '_command_handler'])
                    hotkey = xtuple(keymap.lookup_command(command, real_args))
                elif keymap is not None:
                    keymap.define_key(hotkey, command, args)
                if hotkey != (None,):
                    s = hotkey_str[i] = '    ' + hotkey_string(hotkey)
                    hotkey_width = parent.GetTextExtent(s)[0]
                    max_hotkey_width = max(hotkey_width, max_hotkey_width)
        # Now create the items and remember max. width of whole item label
        hotkey_items = []
        max_label_width = 0
        i = 0
        for item in self._items:
            if isinstance(item, MSeparator):
                menu.AppendSeparator()
            else:
                title, wx_title = item.title(), item.title(raw=True)
                if self._allow_autoindex and pytis.config.auto_menu_accel:
                    prefix = acceskey_prefix(i)
                    wx_title = prefix + title
                    title = prefix[1:] + title
                i += 1
                width = parent.GetTextExtent(title)[0] + 20
                if isinstance(item, MItem):
                    wxitem = item.create(parent, menu)
                    wxitem.SetItemLabel(wx_title)
                    menu.Append(wxitem)
                    if isinstance(item, (RadioItem, CheckItem)):
                        wxitem.Check(item.state())
                    if item in hotkey_str:
                        hotkey_items.append((item, wxitem, wx_title, width))
                    max_label_width = max(width + max_hotkey_width, max_label_width)
                elif isinstance(item, Menu):
                    menu.AppendSubMenu(item.create(parent, keymap), wx_title)
                    max_label_width = max(width + 20, max_label_width)
                else:
                    raise ProgramError('Invalid menu item type', item)
        # Append hotkey description string to the item labels.
        # Fill with spaces to justify hotkeys on the right edge.
        space_width = parent.GetTextExtent(' ')[0]
        for i, wxitem, wx_title, width in hotkey_items:
            fill_width = max_label_width - width - max_hotkey_width
            n = round(float(fill_width) / float(space_width))
            fill = "%%%ds" % n % ''
            wxitem.SetItemLabel(wx_title + fill + hotkey_str[i])
        return menu

    def wx_menu(self):
        """Return the most recently created 'wxMenu' instance or None if create() was not called."""
        return self._wx_menu


class MItem(_TitledMenuObject):
    """Menu item specification.

    The class is only for specification purposes, it has no connection to any
    UI elements.

    """
    _WX_KIND = wx.ITEM_NORMAL
    _used_titles = {}

    def __init__(self, title, command, args=None, help=None, hotkey=None, icon=None):
        """Arguments:

          title -- menu item title, non-empty basestring

          command -- defines the command invoked when this menu item is
            activated as a 'pytis.form.Command' instance.  It can be also
            defined as a pair of (COMMAND, ARGS), where the first item is the
            command itself and the later item replaces the argument 'args'
            described below (in this case, the argument 'args' must be None).
            Finally, this argument may be passed as a string, in which case
            this string, together with a 'cmd_' prefix, denotes the name of a
            method in the application specification and this method is called
            to retrieve the pair (COMMANDS, ARGS).  For example when command is
            'my_form', the application specification must define a method named
            'cmd_my_form' (with no arguments) which returns the command
            specification.

          args -- dictionary of 'command' arguemnts.

          help -- basestring describing the menu item's action in more detail,
             but still not longer than one line.  May be displayed for example
             in status line or as a tooltip.

          hotkey -- string or a sequence of strings defining the shortcut to
            invoke this menu item from keyboard.  The form of the specification
            is described in the module 'command'.

          icon -- explicit icon for this menu item.  Must be a valid argument
            of function 'get_icon()'.  If not defined, default command icon may
            be used if defined by pytis.

        """
        if isinstance(command, basestring):
            application = pytis.config.resolver.specification('Application')
            method = getattr(application, 'cmd_' + command)
            command, args = method()
        elif isinstance(command, (tuple, list)):
            assert len(command) == 2, command
            assert args is None, args
            command, args = command
        assert isinstance(command, Command), command
        assert args is None or isinstance(args, dict)
        assert help is None or isinstance(help, basestring)
        assert hotkey is None or isinstance(hotkey, (basestring, tuple, list))
        assert icon is None or isinstance(icon, basestring)
        self._command = command
        self._args = args or {}
        self._help = help
        self._hotkey = xtuple(hotkey)
        self._icon = icon
        self._action_id = self._make_action_id(command)
        super(MItem, self).__init__(title)

    def _on_ui_event(self, event):
        event.Enable(self._command.enabled(**self._args))

    def _make_action_id(self, command_spec):
        def modulify(obj, name):
            module_name = str(obj.__module__)
            if module_name == 'pytis.form.list':
                # Well, not a very good idea to name a Python file `list'
                module_name = 'pytis.form'
            name = '%s.%s' % (module_name, name,)
            return name
        args = copy.copy(self.args())
        if isinstance(command_spec, basestring):
            command_proc = command_spec
            command_spec = self._command
        else:
            command_proc = ''
        command = command_spec.name()
        appstring = 'Application.'
        if command[:len(appstring)] == appstring:
            command = command[len(appstring):]
        if command == 'RUN_FORM':
            form_class = args.pop('form_class', None)
            form_name = args.pop('name', None)
            extra = []
            if 'binding' in args:
                extra.append('binding=%s' % (args['binding'],))
                del args['binding']
            if not args:
                class_name = modulify(form_class, form_class.__name__)
                return ('form/%s/%s/%s/%s' %
                        (class_name, form_name, '&'.join(extra), command_proc,))
        elif command == 'NEW_RECORD' and args:
            form_name = args.pop('name', '')
            if form_name is not None and not args:
                return ('%s/%s/%s' % (command, form_name, command_proc,))
        elif command == 'HANDLED_ACTION':
            handler = args.pop('handler', None)
            if not args and isinstance(handler, types.FunctionType):
                name = modulify(handler, handler.__name__)
                return ('handle/%s/%s' % (name, command_proc,))
        elif command == 'RUN_PROCEDURE':
            proc_name = args.pop('proc_name')
            spec_name = args.pop('spec_name')
            if not args or (len(args) == 1 and 'enabled' in args and not callable(args['enabled'])):
                return ('proc/%s/%s/%s' % (proc_name, spec_name, command_proc,))
        if args and not command_proc:
            return None
        return ('%s/%s' % (command, command_proc,))

    @classmethod
    def parse_action(class_, action):
        """Parse action id back to command and its arguments.

        Arguments:

          action -- the action id, string
          globals -- dictionary of global name space
          locals -- dictionary of local name space

        Return pair COMMAND, ARGUMENTS corresponding to the given action id.
        If the action id is invalid, behavior of this method is undefined.

        """
        components = action.split('/')
        kind = components[0]

        def find_symbol(symbol):
            # temporary hack to not crash on special situations to be solved
            # later
            try:
                return eval(symbol)
            except AttributeError:
                sys.stderr.write("Can't find object named `%s'\n" % (symbol,))
                return None
        if components[-1]:
            return components[-1]
        elif kind == 'form':
            command = pytis.form.Application.COMMAND_RUN_FORM
            class_name, form_name = components[1], components[2]
            arguments = dict(form_class=find_symbol(class_name), name=form_name)
            if components[3]:
                for extra in components[3].split('&'):
                    if extra[:len('binding=')] == 'binding=':
                        arguments['binding'] = extra[len('binding='):]
                        break
        elif kind == 'handle':
            command = pytis.form.Application.COMMAND_HANDLED_ACTION
            function_name = components[1]
            arguments = dict(handler=find_symbol(function_name),
                             enabled=lambda: pytis.form.app.action_has_access(action))
        elif kind == 'proc':
            command = pytis.form.Application.COMMAND_RUN_PROCEDURE
            proc_name, spec_name = components[1], components[2]
            arguments = dict(proc_name=proc_name, spec_name=spec_name,
                             enabled=lambda: pytis.form.app.action_has_access(action))
        elif kind == 'NEW_RECORD':
            command = pytis.form.Application.COMMAND_NEW_RECORD
            arguments = dict(name=components[1])
        else:
            command = pytis.form.Command.command(kind)
            arguments = None
        return command, arguments

    def _create_icon(self, item):
        icon = get_icon(self._icon or command_icon(self._command, self._args))
        if icon:
            item.SetBitmap(icon)

    def _on_invoke_command(self, event):
        # Invoke the command through CallAfter to finish menu event processing before
        # command invocation.  Calling dirrectly for example resulted in disfunctional
        # TAB traversal in a popup form which was opended through a popup menu command
        # due to FindFocus() returning a wrong window.  Using CallAfter fixes this.
        wx.CallAfter(self._command.invoke, **self._args)

    def create(self, parent, parent_menu):
        item = wx.MenuItem(parent_menu, -1, self._title, self._help or "", kind=self._WX_KIND)
        wx_callback(wx.EVT_MENU, parent, self._on_invoke_command, source=item)
        wx_callback(wx.EVT_UPDATE_UI, parent, self._on_ui_event, source=item)
        self._create_icon(item)
        return item

    def command(self):
        """Vrať command zadaný v konstruktoru."""
        return self._command

    def args(self):
        """Vrať argumenty command zadané v konstruktoru."""
        return self._args

    def hotkey(self):
        """Vrať horkou klávesu položky jako tuple řetězců.

        Pokud nemá položka přiřazenu horkou klávesu, vrať tuple '(None,)'.

        """
        return self._hotkey

    def help(self):
        """Vrať text nápovědy položky zadaný v konstruktoru."""
        return self._help

    def icon(self):
        """Return icon given in the constructor."""
        return self._icon

    def action_id(self):
        """Return action id string of the menu item or 'None' if it is unavailable."""
        return self._action_id


class CheckItem(MItem):
    """Položka menu, která může být ve stavu ON/OFF."""
    _WX_KIND = wx.ITEM_CHECK

    def __init__(self, title, command, state=None, **kwargs):
        """Inicializuj instanci.

        Arguemnty:

          state -- funkce (volaná bez argumentů), která vrací True/False podle
            toho, zda je stav této položky 'zapnuto', nebo 'vypnuto'.

          Všechny ostatní arguemnty jsou sthodné jako v konstruktoru předka.

        """
        assert callable(state)
        self._state = state
        super(CheckItem, self).__init__(title, command, **kwargs)

    def _create_icon(self, item):
        pass

    def _on_ui_event(self, event):
        event.Check(self.state())
        super(CheckItem, self)._on_ui_event(event)

    def state(self):
        return self._state()


class RadioItem(CheckItem):
    """Položka menu tvořící přepínatelnou skupinu."""
    # wx.ITEM_RADIO způsobuje SEGFAULT.  CheckItem se však, zdá se, chová úplně
    # stejně, takže to vlastně vůbec nevadí...
    # _WX_KIND = wx.ITEM_RADIO


class MenuBar(wx.MenuBar):
    """Wx implementace pull-down menu hlavního aplikačního okna.

    Třída zkonstruuje menubar a vloží jej do zadaného framu.  Menubar je
    zkonstruován na základě specifikace, která se skládá z instancí 'Menu'
    určujících jednotlivé položky menubaru.

    Menubar je jednotný pro celou aplikaci.

    """

    def __init__(self, parent, menus, keymap=None):
        """Vytvoř menubar na základě sekvence 'menus' a vlož do 'parent'.

        Argumenty:

          parent -- instance třídy 'wxFrame', do které má být menubar vložen
          menus -- sekvence instancí třídy 'Menu' definující jednotlivá
            menu v menu baru; menu se v menu baru vytvoří ve stejném pořadí,
            v jakém jsou v této sekvenci uvedena
          keymap -- klávesová mapa (instance 'Keymap'), která má být
            synchronizována s klávesovými zkratkami položek menu.

        """
        wx.MenuBar.__init__(self)
        self._parent = parent
        if __debug__:
            self._keys = {}
            for m in menus:
                self._check_duplicate_keys(m)
        for menu in menus:
            self.Append(menu.create(self._parent, keymap), menu.title(raw=True))
        parent.SetMenuBar(self)

    def _check_duplicate_keys(self, menu):
        if isinstance(menu, Menu):
            for m in menu.items():
                self._check_duplicate_keys(m)
        elif isinstance(menu, MItem):
            k = xtuple(menu.hotkey())
            if k != (None,) and k != (u'',):
                cmd = (menu.command(), menu.args())
                if k in self._keys and self._keys[k] != cmd:
                    log(OPERATIONAL, "Duplicate menu shortcut:", (k, menu.title(), cmd))
                    log(OPERATIONAL, "Colliding command:", self._keys[k])
                else:
                    self._keys[k] = cmd


# StatusBar tooltips use the SuperToolTip class, but it is not very
# customizable, thus the hacks below.

class ToolTipWindow(supertooltip.ToolTipWindow):

    def OnPaint(self, event):
        # The superclass is a mess - OnPaint is used (apart from
        # actually painting) also to compute the tooltip size.
        # Here we just need to enlarge the window vertically.
        result = super(ToolTipWindow, self).OnPaint(event)
        if event is None:
            return result[0], result[1] + 8

    def CalculateBestPosition(self, widget):
        size = self.GetSize()
        screen = wx.ClientDisplayRect()[2:]
        position = wx.GetMousePosition()
        if position.x + size.x > screen[0]:
            position.x -= size.x
        # Y position must be always above the pointer, otherwise the tooltip doesn't
        # show at all when it gets below the main application frame (which is always
        # the case for status bar tooltips).
        self.SetPosition((position.x, position.y - size.y))


supertooltip.ToolTipWindow = ToolTipWindow


class ToolTip(supertooltip.SuperToolTip):

    def __init__(self, window):
        super(ToolTip, self).__init__('')
        self.SetTarget(window)
        self.SetDropShadow(False)
        self.ApplyStyle("Yellow")
        self._label = None
        self._content = None

    def OnStartTimer(self):
        label = self._label
        content = self._content() if self._content else None
        if label is not None or content is not None:
            self.SetHeader(label or '')
            self.SetDrawHeaderLine(label is not None)
            self.SetMessage(content or '')
            super(ToolTip, self).OnStartTimer()

    def SetContent(self, label, content):
        # Here we rely on the fact, that this method is not called twice
        # for the same field.  Thus we know that the content has changed
        # (we are above a different field) and thus we need to hide the
        # old content (if shown) and restart timers.
        assert callable(content), content
        self._label = label
        self._content = content
        if self.GetTipWindow():
            self.OnEndTimer()
        if not self._startTimer.IsRunning():
            self._startTimer.Start(self._startDelayTime * 1000)


# Status bar

class StatusBar(object):
    """Pytis application main frame status bar.

    The status bar is located at the bottom of the main application window and
    may be used to display global application status information in a set of
    dedicated fields.  The application defines the available status bar fields
    through the specification method 'Application.status_fields()'.

    """
    @pytis.api.implements(pytis.api.StatusField)
    class Field(object):
        class _Timer(wx.Timer):
            def Notify(self):
                self.Stop()
            def Start(self, timeout):
                if self.IsRunning():
                    self.Stop()
                super(StatusBar.Field._Timer, self).Start(timeout, True)

        def __init__(self, sb, spec, index, count, on_size_change):
            self._sb = sb
            self._spec = spec
            self._index = index
            self._on_size_change = on_size_change
            self._timer = self._Timer() if spec.refresh_interval() else None
            self._text = ''
            self._icon = None
            self._tooltip = None
            self._bitmap = None
            width = dlg2px(sb, spec.width() * 4) if spec.width() is not None else None
            if width and index == count - 1:
                # Wx hack: Extend the last field to fit also the dragging triangle.
                width += 22
            self._width = self._min_width = width

        def _on_click(self, event):
            handler = self._spec.on_click()
            if handler:
                handler()

        def update_bitmap_position(self):
            if self._bitmap:
                position = self._spec.icon_position()
                rect = self._sb.GetFieldRect(self._index)
                if position == StatusField.ICON_LEFT:
                    x = rect.x + 4
                else:
                    x = rect.x + rect.width - 2 - self._bitmap.GetSize().width
                self._bitmap.SetPosition((x, rect.y + 4))

        @property
        def spec(self):
            return self._spec

        @property
        def width(self):
            return self._width

        @property
        def timer(self):
            return self._timer

        # Implementation of Public API 'pytis.api.StatusField'.

        def api_update(self, text=None, icon=None, tooltip=None):
            # Prevent status bar blinking by checking against the current value.
            text = unistr(text or '')
            if text != self._text:
                self._text = text
                if text and icon:
                    if self._spec.icon_position() == StatusField.ICON_LEFT:
                        text = '     ' + text
                    else:
                        text += '      '
                if self._width is not None:
                    # Adjust the field width to fit the new text (for fixed width
                    # fields only).  The "fixed" fields don't change their width
                    # as a percentage of the application frame width, but are not
                    # completely fixed...
                    width = max(self._sb.GetTextExtent(text)[0] + 8, self._min_width)
                    if width != self._width:
                        self._width = width
                        self._on_size_change()
                self._sb.SetStatusText(text, self._index)
            if icon != self._icon:
                self._icon = icon
                if self._bitmap is not None:
                    self._bitmap.Destroy()
                if icon is not None:
                    bitmap = get_icon(icon)
                    if bitmap:
                        self._bitmap = bmp = wx.StaticBitmap(self._sb, bitmap=bitmap)
                        wx_callback(wx.EVT_LEFT_DOWN, bmp, self._on_click)
                        self.update_bitmap_position()
                    else:
                        self._bitmap = None
                else:
                    self._bitmap = None
            self._tooltip = tooltip

        def api_refresh(self):
            refresh = self._spec.refresh()
            if refresh:
                status = refresh()
                if status is not None:
                    if not isinstance(status, (tuple, list)):
                        text, icon, tooltip = status, None, None
                    elif len(status) == 2:
                        text, icon, tooltip = status[0], status[1], None
                    else:
                        text, icon, tooltip = status
                    self.api_update(text, icon=icon, tooltip=tooltip)
                return True
            else:
                return False

        @property
        def api_text(self):
            return self._text

        @property
        def api_icon(self):
            return self._icon

        @property
        def api_tooltip(self):
            tooltip = self._tooltip
            if callable(tooltip):
                tooltip = self._tooltip = tooltip()
            return tooltip

    def __init__(self, parent, fields):
        """Arguments:

          parent -- parent wx Window.
          fields -- sequence of 'pytis.presentation.StatusField' instances.

        """
        self._sb = sb = wx.StatusBar(parent, -1)
        self._fields = [self.Field(sb, f, i, len(fields),
                                   on_size_change=self._on_field_size_change)
                        for i, f in enumerate(fields)]
        self._tooltip = ToolTip(sb)
        self._last_tooltip_field = None
        self._initial_refresh_called = False
        sb.SetOwnBackgroundColour(DEFAULT_WINDOW_BACKGROUND_COLOUR)
        sb.SetMinHeight(22)
        sb.SetFieldsCount(len(fields))
        SB_SUNKEN = 3  # This wx constant is missing in wx Python???
        sb.SetStatusStyles([SB_SUNKEN for f in fields])
        self._update_widths()
        parent.SetStatusBar(sb)
        wx_callback(wx.EVT_IDLE, sb, self._on_idle)
        wx_callback(wx.EVT_LEFT_DOWN, sb, self._on_click)
        wx_callback(wx.EVT_MOTION, sb, self._on_motion)
        wx_callback(wx.EVT_SIZE, sb, self._on_size)

    def _on_idle(self, event):
        for field in self._fields:
            interval = field.spec.refresh_interval()
            if interval:
                if field.timer.IsRunning():
                    continue
                field.timer.Start(interval)
            if interval == 0 and self._initial_refresh_called:
                continue
            field.api_refresh()
        self._initial_refresh_called = True

    def _on_click(self, event):
        field = self._field_on_position(event.GetX())
        if field:
            field._on_click(event)

    def _on_motion(self, event):
        field = self._field_on_position(event.GetX())
        if field and field != self._last_tooltip_field:
            self._last_tooltip_field = field
            self._tooltip.SetContent(
                field.spec.label(),
                lambda: field.api_tooltip or field.api_text or None
            )
        event.Skip()

    def _on_size(self, event):
        self._update_bitmap_positions()
        event.Skip()

    def _on_field_size_change(self):
        self._update_widths()
        self._update_bitmap_positions()

    def _field_on_position(self, x):
        for i, field in enumerate(self._fields):
            rect = self._sb.GetFieldRect(i)
            if x >= rect.x and x <= rect.x + rect.width:
                return field
        return None

    def _update_widths(self):
        self._sb.SetStatusWidths([-1 if f.width is None else f.width for f in self._fields])

    def _update_bitmap_positions(self):
        for field in self._fields:
            field.update_bitmap_position()

    @property
    def fields(self):
        return self._fields


class InfoWindow(object):
    """Nemodální okno pro zobrazení textových informací."""

    def __init__(self, title, text, format=TextFormat.PLAIN, parent=None, _name='info', **kwargs):
        """Display information window in a standalone frame.

        Arguments:

          title -- Frame title as a basestring.
          parent -- parent wx Frame or None to use the main application frame
          text, format, **kwargs -- passed to 'wx_text_view()' ('text' as 'content').

        """
        frame = wx.Dialog(parent or pytis.form.wx_frame(), title=title, name=_name,
                          style=wx.DEFAULT_DIALOG_STYLE | wx.RESIZE_BORDER)
        # Temporarily use a modal dialog instead an ordinary frame to work
        # around the problem of closing a frame whose parent is a modal dialog
        # in StructuredTextField._cmd_preview().  Once that is sorted out, a
        # non-modal frame would be better.
        view = wx_text_view(frame, text, format, **kwargs)
        frame.SetSize(view.GetSize())
        frame.ShowModal()


class ProfileSelectorPopup(wx.ComboPopup):
    """Profile selection menu implemented using wx.ListCtrl.

    This class implements the 'wx.ComboPopup' API and thus can be used as
    a popup selection of the 'ProfileSelector' control, which is derived form
    'wx.ComboCtrl'.

    """
    def __init__(self):
        wx.ComboPopup.__init__(self)
        self._selected_profile_index = None
        self._listctrl = None

    def _on_motion(self, event):
        ctrl = self._listctrl
        item, flags = ctrl.HitTest(event.GetPosition())
        if item >= 0:
            profile_index = ctrl.GetItemData(item)
            if profile_index != -1:
                ctrl.Select(item)
                self._selected_profile_index = profile_index

    def _on_left_down(self, event):
        self.Dismiss()
        if self._selected_profile_index is not None:
            # We explicitly pass _command_handler to ensure that the
            # command is handled by the form for which the profile
            # selection menu was constructed.  This is because we've
            # seen tracebacks indicating that handliong gets to a
            # form with fewer profiles than _selected_profile_index.
            # Probably the current form may change for some reason
            # between the popup invocation and the click?
            pytis.form.LookupForm.COMMAND_APPLY_PROFILE.invoke(index=self._selected_profile_index,
                                                               _command_handler=self._current_form)

    def _append_label(self, label, toplevel=True):
        ctrl = self._listctrl
        i = ctrl.GetItemCount()
        ctrl.InsertItem(i, label)
        ctrl.SetItemBackgroundColour(i, wx.Colour(225, 225, 225))
        if toplevel:
            ctrl.SetItemFont(i, wx.Font(ctrl.GetFont().GetPointSize(),
                                        wx.FONTFAMILY_DEFAULT, wx.FONTSTYLE_NORMAL,
                                        wx.FONTWEIGHT_BOLD))
        ctrl.SetItemData(i, -1)

    def _append_profile(self, profile, index, select=False, indent=''):
        ctrl = self._listctrl
        title = indent + profile.title()
        if profile.errors():
            title += ' ' + _("(invalid)")
        i = ctrl.GetItemCount()
        ctrl.InsertItem(i, title)
        ctrl.SetItemData(i, index)
        if select:
            ctrl.Select(i)

    # The following methods implement the ComboPopup API.

    def Create(self, parent):
        # Create the popup child control. Return True for success.
        self._listctrl = ctrl = wx.ListCtrl(parent, style=(wx.LC_REPORT | wx.LC_SINGLE_SEL |
                                                           wx.SIMPLE_BORDER | wx.LC_NO_HEADER))
        ctrl.InsertColumn(0, 'profile')
        ctrl.Bind(wx.EVT_LEFT_DOWN, self._on_left_down)
        ctrl.Bind(wx.EVT_MOTION, self._on_motion)
        return True

    def GetControl(self):
        # Return the widget that is to be used for the popup.
        return self._listctrl

    def GetAdjustedSize(self, minWidth, prefHeight, maxHeight):
        # Called just prior to displaying the popup.
        # Fill menu items before each popup and delete them on dismiss to
        # avoid having to update the menu during form profile list update.
        self._current_form = form = pytis.form.current_form()
        profiles = form.profiles()
        current = form.current_profile()

        def append_system_profiles(items, level=0):
            indent = 3 * level * ' '
            for item in items:
                if isinstance(item, pytis.presentation.Profile):
                    profile = find(item.id(), profiles, key=lambda p: p.id())
                    self._append_profile(profile, profiles.index(profile), profile is current,
                                         indent=indent)
                else:
                    self._append_label(indent + item.title(), False)
                    append_system_profiles(item.items(), level=level + 1)
        self._append_label(_("System Profiles"))
        # Default profile is always the first in form.profiles().
        self._append_profile(profiles[0], 0, profiles[0] is current)
        append_system_profiles(form.view().profiles())
        self._append_label(_("User Profiles"))
        for i, profile in enumerate(profiles):
            if profile.id().startswith(FormProfileManager.USER_PROFILE_PREFIX):
                self._append_profile(profile, i, profile is current)
        ctrl = self._listctrl
        ctrl.SetColumnWidth(0, minWidth)
        ctrl.SetSize((1000, 3000))  # Needed for GetViewRect to work consistently.
        width, height = ctrl.GetViewRect()[2:]
        return wx.Size(max(width - 1, minWidth), min(height - 1, maxHeight))

    def SetStringValue(self, value):
        # Called just prior to displaying the popup, but after GetAdjustedSize.
        # As it is more practical to select the current item there, we don't
        # need to do anything here.
        pass

    def GetStringValue(self):
        # Return a string representation of the current item.
        selected = self._listctrl.GetFirstSelected()
        if selected != -1:
            return self._listctrl.GetItemText(selected)
        else:
            return ''

    def OnPopup(self):
        # Called immediately after the popup is shown.
        wx.ComboPopup.OnPopup(self)

    def OnDismiss(self):
        # Called when popup is dismissed.
        ctrl = self._listctrl
        wx.ComboPopup.OnDismiss(self)
        ctrl.Select(wx.NOT_FOUND)
        ctrl.DeleteAllItems()


class ProfileSelector(wx.ComboCtrl):
    """Toolbar control for form profile selection and management."""

    def __init__(self, parent, uicmd, size):
        wx.ComboCtrl.__init__(self, parent, style=wx.TE_PROCESS_ENTER, size=size)
        self._popup = ProfileSelectorPopup()
        self.SetPopupControl(self._popup)
        self._on_enter_perform = None
        # SetButtonPosition works around incorrect initial text control sizing in
        # codebook form, which starts with size 10x35 and resizes to the correct
        # size only after resizing the parent Dialog manually.
        self.SetButtonPosition(width=20, height=size[1] - 1)
        ctrl = self.GetTextCtrl()
        ctrl.SetEditable(False)
        wx_callback(wx.EVT_UPDATE_UI, self, self._on_ui_event)
        wx_callback(wx.EVT_RIGHT_DOWN, self, self._on_context_menu)
        wx_callback(wx.EVT_RIGHT_DOWN, ctrl, self._on_context_menu)
        wx_callback(wx.EVT_TEXT_ENTER, ctrl, self._on_enter)
        wx_callback(wx.EVT_KEY_DOWN, self, self._on_key_down)
        wx_callback(wx.EVT_KEY_DOWN, ctrl, self._on_key_down)

    def _on_ui_event(self, event):
        enabled = pytis.form.LookupForm.COMMAND_PROFILE_MENU.enabled()
        event.Enable(enabled)
        ctrl = self.GetTextCtrl()
        if enabled:
            if not ctrl.IsEditable():
                form = pytis.form.current_form()
                current_profile = form.current_profile()
                if current_profile and ctrl.GetValue() != current_profile.title():
                    ctrl.SetValue(current_profile.title())
            if pytis.form.LookupForm.COMMAND_UPDATE_PROFILE.enabled():
                # Indicate changed profile by color (update is enabled for changed profiles).
                color = wx.Colour(200, 0, 0)
            else:
                color = wx.Colour(0, 0, 0)
            ctrl.SetForegroundColour(color)
        elif pytis.form.top_window() is None and ctrl.GetValue() != '':
            ctrl.SetValue('')

    def _on_context_menu(self, event):
        menu = (
            MItem(_("Save"),
                  pytis.form.LookupForm.COMMAND_UPDATE_PROFILE(),
                  help=_("Update the saved profile according to the current form setup.")),
            MItem(_("Save as new"),
                  pytis.form.Application.COMMAND_HANDLED_ACTION(
                      # Name must be edited first and 'cmd' will be invoked after confirmation.
                      handler=self._edit_profile_title,
                      enabled=self._edit_profile_title_enabled,
                      cmd=pytis.form.LookupForm.COMMAND_SAVE_NEW_PROFILE,
                      clear=True),
                  help=_("Create a new profile according to the current form setup.")),
            MItem(_("Rename"),
                  pytis.form.Application.COMMAND_HANDLED_ACTION(
                      # Name must be edited first and 'cmd' will be invoked after confirmation.
                      handler=self._edit_profile_title,
                      enabled=self._edit_profile_title_enabled,
                      cmd=pytis.form.LookupForm.COMMAND_RENAME_PROFILE),
                  help=_("Change the name of the current profile and save it.")),
            MItem(_("Delete"),
                  pytis.form.LookupForm.COMMAND_DELETE_PROFILE(),
                  help=_("Delete the selected saved profile.")),
            MItem(("Use automatically on form startup"),
                  pytis.form.LookupForm.COMMAND_SET_INITIAL_PROFILE(),
                  help=_("Automatically switch to this profile "
                         "when this form is opened next time.")),
            MSeparator(),
            MItem(_("Restore to previously saved form settings"),
                  pytis.form.LookupForm.COMMAND_RELOAD_PROFILE,
                  help=_("Discard changes in form settings since the profile was last saved.")),
            MItem(_("Restore to default form settings"),
                  command=pytis.form.LookupForm.COMMAND_RESET_PROFILE,
                  help=_("Discard all user changes in form settings.")),
        )
        popup_menu(self, menu)

    def _edit_profile_title(self, cmd, clear=False):
        ctrl = self.GetTextCtrl()

        def perform():
            title = self.GetValue()
            ctrl.SetEditable(False)
            cmd.invoke(title=title)
        ctrl.SetEditable(True)
        if clear:
            ctrl.SetValue('')
        else:
            ctrl.SelectAll()
        ctrl.SetFocus()
        app.echo(_("Enter the profile name and press ENTER when done."))
        self._on_enter_perform = perform

    def _edit_profile_title_enabled(self, cmd, clear=False):
        return cmd.enabled(title=self.GetValue())

    def _on_enter(self, event):
        func = self._on_enter_perform
        if func:
            self._on_enter_perform = None
            func()
        else:
            event.Skip()

    def _on_key_down(self, event):
        event.Skip()
        code = event.GetKeyCode()
        if code in (wx.WXK_ESCAPE, wx.WXK_TAB, wx.WXK_RETURN, wx.WXK_NUMPAD_ENTER):
            pytis.form.current_form().focus()


class TextHeadingSelector(wx.Choice):
    """Toolbar control for structured text heading level selection."""
    _CHOICES = ("Běžný text",
                "Nadpis úrovně 1",
                "Nadpis úrovně 2",
                "Nadpis úrovně 3",
                "Nadpis úrovně 4",
                "Nadpis úrovně 5",
                "Nadpis úrovně 6",
                )

    def __init__(self, parent, uicmd, size=None):
        self._uicmd = uicmd
        wx.Choice.__init__(self, parent, choices=self._CHOICES, size=size or wx.DefaultSize)
        wx_callback(wx.EVT_UPDATE_UI, self, self._on_ui_event)
        wx_callback(wx.EVT_CHOICE, self, self._on_selection)

    def _on_ui_event(self, event):
        cmd, kwargs = self._uicmd.command(), self._uicmd.args()
        enabled = cmd.enabled(**kwargs)
        event.Enable(enabled)
        if enabled:
            ctrl = kwargs['_command_handler']
            self.SetSelection(ctrl.current_heading_level())

    def _on_selection(self, event):
        cmd, kwargs = self._uicmd.command(), self._uicmd.args()
        cmd.invoke(level=event.GetSelection(), **kwargs)


class FormStateToolbarControl(wx.BitmapButton):
    """Special toolbar control for form commands with current state indication.

    A custom toolbar control indicating the current form state by changing the
    visible icon.  Available icons are defined by '_ICONS' and the currently
    displayed icon is returned by '_current_icon_index()'.

    """
    _ICONS = ()
    """Sequence of all possible icons as values for the first argument to 'get_icon()'."""

    def __init__(self, parent, uicmd):
        self._toolbar = parent
        self._uicmd = uicmd
        self._bitmaps = [get_icon(icon, type=wx.ART_TOOLBAR) or
                         get_icon(wx.ART_ERROR, type=wx.ART_TOOLBAR)
                         for icon in self._ICONS]
        self._current_bitmap = self._bitmaps[0]
        wx.BitmapButton.__init__(self, parent, -1, self._current_bitmap,
                                 style=wx.BU_EXACTFIT | wx.NO_BORDER)
        wx_callback(wx.EVT_BUTTON, self, self._on_click)
        wx_callback(wx.EVT_UPDATE_UI, self, self._on_update_ui)

    def _on_click(self, event):
        cmd, kwargs = self._uicmd.command(), self._uicmd.args()
        cmd.invoke(**kwargs)

    def _on_update_ui(self, event):
        cmd, kwargs = self._uicmd.command(), self._uicmd.args()
        enabled = cmd.enabled(**kwargs)
        event.Enable(enabled)
        if enabled:
            form = pytis.form.current_form(inner=False)
            new_bitmap = self._bitmaps[self._current_icon_index(form)]
            if self._current_bitmap != new_bitmap:
                self._current_bitmap = new_bitmap
                self.SetBitmapLabel(new_bitmap)
                self._toolbar.Realize()

    def _current_icon_index(self, form):
        """Implement this method to return the index of the active icon in _ICONS."""


class KeyboardSwitcher(wx.BitmapButton):
    """Special toolbar control for keyboard layout switching and indication.


    """
    _ICONS = ()
    """Sequence of all possible icons as values for the first argument to 'get_icon()'."""

    def __init__(self, parent, uicmd):
        self._toolbar = parent
        layouts = pytis.config.keyboard_layouts
        self._bitmaps = dict([(icon, get_icon(icon, type=wx.ART_TOOLBAR) or
                               get_icon(wx.ART_ERROR, type=wx.ART_TOOLBAR))
                              for title, icon, command in layouts])
        self._menu = [MItem(title,
                            command=pytis.form.Application.COMMAND_HANDLED_ACTION(
                                handler=self._switch_layout,
                                system_command=system_command,
                                icon=icon,
                            ))
                      for title, icon, system_command in layouts]
        layout = find(pytis.config.initial_keyboard_layout, layouts, lambda x: x[2]) or layouts[0]
        icon, system_command = layout[1:]
        os.system(system_command)
        wx.BitmapButton.__init__(self, parent, -1, self._bitmaps[icon],
                                 style=wx.BU_EXACTFIT | wx.NO_BORDER)
        wx_callback(wx.EVT_BUTTON, self, self._on_click)

    def _on_click(self, event):
        popup_menu(self._toolbar, self._menu)

    def _switch_layout(self, system_command, icon):
        os.system(system_command)
        pytis.config.initial_keyboard_layout = system_command
        self.SetBitmapLabel(self._bitmaps[icon])
        self._toolbar.Realize()


class DualFormSwitcher(FormStateToolbarControl):
    """Special toolbar control for DualForm.COMMAND_OTHER_FORM.

    The current icon indicates whether the current form is the main form or the
    side form and takes the dual form split mode into account.

    """
    _ICONS = ('dual-form-active-up',
              'dual-form-active-down',
              'dual-form-active-left',
              'dual-form-active-right',)

    def _current_icon_index(self, form):
        if form.active_form() == form.main_form():
            i = 0
        else:
            i = 1
        if form.is_vertical():
            i += 2
        return i


class DualFormResplitter(FormStateToolbarControl):
    """Special toolbar control for DualForm.COMMAND_RESPLIT.

    The current icon indicates whether the form is currently split vertically or horizontally.

    """

    _ICONS = ('dual-form-split-horizontally',
              'dual-form-split-vertically',)

    def _current_icon_index(self, form):
        if form.is_vertical():
            return 0
        else:
            return 1


class LocationBar(wx.TextCtrl):
    """Toolbar control for browser location display and entry."""

    def __init__(self, parent, uicmd, editable=True, size=None):
        self._toolbar = parent
        self._uicmd = uicmd
        wx.TextCtrl.__init__(self, parent, -1, size=size or wx.DefaultSize,
                             style=wx.TE_PROCESS_ENTER, name='location-bar')
        self.SetEditable(editable)
        wx_callback(wx.EVT_UPDATE_UI, self, self._on_update_ui)
        if editable:
            wx_callback(wx.EVT_TEXT_ENTER, self, self._on_enter)
        else:
            from .inputfield import TextField
            self.SetOwnBackgroundColour(TextField.FIELD_DISABLED_COLOR)
            self.Refresh()
        browser = uicmd.args()['_command_handler']
        browser.set_callback(browser.CALL_URI_CHANGED, self.SetValue)
        self._want_focus = 0

    def _on_update_ui(self, event):
        cmd, kwargs = self._uicmd.command(), self._uicmd.args()
        enabled = cmd.enabled(**kwargs)
        event.Enable(enabled)
        if self._want_focus:
            self.SetFocus()
            # Nasty hack - see set_focus for explanation.
            self._want_focus -= 1

    def _on_enter(self, event):
        cmd, kwargs = self._uicmd.command(), self._uicmd.args()
        uri = self.GetValue()
        cmd.invoke(uri=uri, **kwargs)

    def set_focus(self):
        # This is a total hack - calling SetFocus() is ignored at form startup, but
        # calling it repeatedly in _on_update_ui seems to help finally.  Three times
        # seems to work, so rather repeat it four times to be sure...
        self._want_focus = 4


class HelpProc(object):
    """Special class to mark procedures allowed to be called from pytis help.

    Don't use directly, just mark the procedure using the 'help_proc' decorator.

    """
    def __init__(self, func):
        self._func = func

    def __call__(self, **kwargs):
        action = 'proc/%s/%s/' % (self._func.__name__, self._func.__module__)
        if not pytis.form.app.action_has_access(action):
            app.error(_(u"You don't have priviledges to invoke the action '%s'.\n"
                        u"Please contact the access rights administrator.") % (action,))
        else:
            self._func(**kwargs)


def help_proc(func):
    """Decorator to mark functions allowed to be run from Pytis help.

    Use this decorator on Python functions which are allowed to be called from
    within the Pytis help content using the link in the form:

    [call:module_name.procedure_name?argument=value Link label]

    Note that module_name is a full Python module name and not a specification
    name and thus it is not limited to Pytis resolver modules.  Thus help
    procedures can be located in any Python module, they just must be marked by
    this decorator.

    """
    return HelpProc(func)


class Browser(wx.Panel, CommandHandler, CallbackHandler, KeyHandler):
    """Web Browser widget.

    The widget can be embedded into other wx widgets as an ordinary wx.Panel.
    The public methods may be used then to manipulate the browser content.

    """

    CALL_TITLE_CHANGED = 'CALL_TITLE_CHANGED'
    """Callback called when the document title changes (called with the title as the argument)."""
    CALL_URI_CHANGED = 'CALL_URI_CHANGED'
    """Callback called when the current uri changes (called with the uri as the argument)."""

    class ResourceServer(socketserver.TCPServer):
        """HTTP server to handle external resources for the current browser document.

        An instance of HTTP server is run for each browser instance.  The
        server runs in a separate thread and is shutdown when the browser is
        deallocated (see Browser.__del__).  Its purpose is to serve external
        resources (images, scripts, css, ...) for the current document loaded
        within the browser.  The resources are part of the 'lcg.Content'
        instance when the document is loaded throuch 'load_content()' or may be
        passed separately through 'resource_provider' argument when the
        document is loaded through 'load_html'.  Resources are not handled when
        a document is loaded through 'load_uri()' as it is assumed that network
        document's resources are loaded through their network URIs.

        Note, it would seem reasonable to load resources through a custom
        scheme handler derived from 'wx.html2.WebViewHandler' (registered by
        'wx.html2.WebView.RegisterHandler'), but it is not the best fit,
        because wx uses a single global handler instance shared by all webview
        instances. This complicates our use case because we keep a separate set
        of resources for every loaded document so we need a separate handler
        for each browser instance.

        """
        def __init__(self):
            self._resource_provider = None
            socketserver.TCPServer.__init__(self, ('', 0), Browser.ResourceHandler)

        def load_resources(self, resource_provider):
            """Make resources of given 'lcg.ResourceProvider' available on the server.

            Use given 'lcg.RecourceProvider' instance as the source of
            currently available resources for the related browser instance.

            The browser should call this method after every document load.
            This allows the server to serve the related resources included
            within the LCG document.  Pass None to unload.

            """
            self._resource_provider = resource_provider

        def find_resource(self, uri):
            """Return the 'lcg.Resource' instance for given URI.

            Searches the resources of the 'lcg.ResourceProvider' instance most
            recently loaded using the 'load_resources()' method.

            Returns None when there is no matching resource.

            """
            if self._resource_provider:
                # Try searching the existing resources by URI first.
                for resource in self._resource_provider.resources():
                    if resource.filename() == uri:
                        return resource
                    if isinstance(resource, lcg.Image):
                        thumbnail = resource.thumbnail()
                        if thumbnail and thumbnail.filename() == uri:
                            return thumbnail
                # If URI doesn't match any existing resource, try locating the
                # resource using the standard resource provider's algorithm
                # (including searching resource directories).
                return self._resource_provider.resource(uri)
            return None


    class ResourceHandler(http.server.SimpleHTTPRequestHandler):

        def do_GET(self):
            uri = self.path
            if uri != '/favicon.ico':
                resource = self.server.find_resource(uri[1:])
                if resource:
                    mime_type, encoding = mimetypes.guess_type(uri)
                    path = resource.src_file()
                    if path:
                        return self._send_data(mime_type, open(path, 'rb'))
                    content = resource.get()
                    if content:
                        return self._send_data(mime_type, io.BytesIO(content))
            self.send_error(404, 'Not found: %s' % uri)

        def _send_data(self, mime_type, f):
            with f:
                self.send_response(200)
                self.send_header("Content-type", mime_type or 'application/octet-stream')
                self.end_headers()
                while True:
                    # Read the file in 0.5MB chunks.
                    data = f.read(524288)
                    if not data:
                        break
                    self.wfile.write(data)

    class Exporter(lcg.StyledHtmlExporter, lcg.HtmlExporter):

        _STYLES = ('default.css',)

        def __init__(self, *args, **kwargs):
            self._resource_base_uri = kwargs.pop('resource_base_uri')
            kwargs['styles'] = self._STYLES
            super(Browser.Exporter, self).__init__(*args, **kwargs)

        def _uri_resource(self, context, resource):
            return resource.uri() or self._resource_base_uri + resource.filename()

    def __init__(self, parent, guardian=None):
        wx.Panel.__init__(self, parent)
        CallbackHandler.__init__(self)
        self._restricted_navigation_uri = None
        self._guardian = guardian
        self._webview = webview = wx.html2.WebView.New(self)
        sizer = wx.BoxSizer(wx.VERTICAL)
        sizer.Add(webview, 1, wx.EXPAND)
        self.SetSizer(sizer)
        self._custom_scheme_handlers = {
            'help': self._help_handler,
            'form': self._form_handler,
            'call': self._call_handler,
        }
        self._exporter_instance = {}
        wx_callback(wx.html2.EVT_WEBVIEW_NAVIGATING, webview, self._on_navigating)
        wx_callback(wx.html2.EVT_WEBVIEW_NAVIGATED, webview, self._on_navigated)
        wx_callback(wx.html2.EVT_WEBVIEW_LOADED, webview, self._on_loaded)
        wx_callback(wx.html2.EVT_WEBVIEW_ERROR, webview, self._on_error)
        wx_callback(wx.html2.EVT_WEBVIEW_TITLE_CHANGED, webview, self._on_title_changed)
        self.Bind(wx.EVT_WINDOW_DESTROY, self._on_destroy)
        KeyHandler.__init__(self, webview)
        self._resource_server = server = self.ResourceServer()
        threading.Thread(target=server.serve_forever).start()
        self._navigation_timeout = time.time()

    def _on_destroy(self, event):
        self._resource_server.shutdown()
        self._resource_server.socket.close()
        event.Skip()

    def _exporter(self, exporter_class):
        try:
            exporter = self._exporter_instance[exporter_class]
        except KeyError:
            exporter = self._exporter_instance[exporter_class] = exporter_class(
                resource_base_uri=('http://localhost:%d/' %
                                   self._resource_server.socket.getsockname()[1]),
                # SVG plots don't display well in the embedded browser.  LinePlot doesn't
                # display grid and constant plot lines, even though the same SVG displays
                # well in ordinary browsers.  Thus we rather convert SVG to PNG.
                allow_svg=False,
                translations=pytis.util.translation_path(),
            )
        return exporter

    def _on_loaded(self, event):
        busy_cursor(False)
        app.echo(_("Document loaded."))

    def _on_error(self, event):
        busy_cursor(False)
        log(OPERATIONAL, "Failed loading '%s':" % event.GetURL(), event.GetString())
        app.echo(_("Loading document failed."))

    def _on_title_changed(self, event):
        self._run_callback(self.CALL_TITLE_CHANGED, self._webview.GetCurrentTitle())

    def _on_navigated(self, event):
        self._run_callback(self.CALL_URI_CHANGED, event.GetURL())

    def _on_navigating(self, event):
        uri = event.GetURL()
        if uri.startswith('#'):
            script = ("var x = document.getElementById('%s'); "
                      "if (x) { x.scrollIntoView() };") % uri[1:]
            self._webview.RunScript(script)
            event.Veto()
            return
        # The navigation timeout hack below prevents endless navigation loop when
        # EVT_WEBVIEW_NAVIGATING is fired again after SetPage() is called within
        # the handler.  This started to happen with some recent wxPython versions,
        # probably 4.1.
        if ':' in uri and (uri != self._webview.GetCurrentURL()
                           or time.time() > self._navigation_timeout):
            # TODO: It would seem nice to implement custom schemes using wx.WebView
            # sheme handlers (WebView.RegisterHandler()), but they don't allow
            # handling the action without actually loading some content into the
            # browser (which we need in form: and call: handlers).
            scheme, path, kwargs = self._parse_uri(uri)
            if scheme in self._custom_scheme_handlers:
                handler = self._custom_scheme_handlers[scheme]
                handler(uri, path, **kwargs)
                event.Veto()
                self._navigation_timeout = time.time() + 0.1
                return
        if ((self._restricted_navigation_uri is not None and
             not uri.startswith(self._restricted_navigation_uri) and
             not uri.startswith('about:'))):
            app.echo(_("External URL navigation denied: %s") % uri, kind='error')
            log(OPERATIONAL, "Restricted to :", self._restricted_navigation_uri)
            event.Veto()
        event.Skip()

    def _help_handler(self, uri, path):
        from pytis.help import help_page, HelpExporter
        self.load_content(help_page(uri), base_uri=uri, exporter_class=HelpExporter)

    def _form_handler(self, uri, path, **kwargs):
        view_spec = pytis.config.resolver.get(path, 'view_spec')
        if view_spec.bindings():
            cls = pytis.form.MultiBrowseDualForm
        else:
            cls = pytis.form.BrowseForm
        pytis.form.run_form(cls, path, **kwargs)

    def _call_handler(self, uri, path, **kwargs):
        try:
            module_name, proc_name = path.rsplit('.', 1)
            module = __import__(module_name)
            for component in module_name.split('.')[1:]:
                module = getattr(module, component)
            proc = getattr(module, proc_name)
            if not isinstance(proc, HelpProc):
                raise ProgramError("Unable to call '%s' from help. "
                                   "Use the 'pytis.form.help_proc' decorator!" % uri[5:])
            proc(**kwargs)
        except Exception:
            top_level_exception()

    def _parse_uri(self, uri):
        def value(v):
            v = [int(x) if x.isdigit() else x for x in v]
            if len(v) == 1:
                v = v[0]
            return v
        scheme, path = uri.split(':', 1)
        if '?' in path:
            import urllib.parse
            path, query = path.split('?', 1)
            kwargs = dict((k, value(v)) for k, v in urllib.parse.parse_qs(query).items())
        else:
            kwargs = {}
        return scheme, path, kwargs

    def _can_go_forward(self):
        return self._webview.CanGoForward()

    def _cmd_go_forward(self):
        self._webview.GoForward()

    def _can_go_back(self):
        return self._webview.CanGoBack()

    def _cmd_go_back(self):
        self._webview.GoBack()

    def _can_stop_loading(self):
        self._webview.IsBusy()

    def _cmd_stop_loading(self):
        self._webview.Stop()

    def _cmd_reload(self):
        self.reload()

    def _cmd_load_uri(self, uri):
        self._webview.LoadURL(uri)

    def guardian(self):
        return self._guardian

    def toolbar(self, parent):
        """Create browser toolbar and return it as a 'wx.ToolBar' instance.

        The toolbar is not part of the browser pane.  If a toolbar is desired,
        it must be created separately using this method and added to the user
        interface together with the main browser pane (the 'Browser' instance
        itself).

        """
        toolbar = wx.ToolBar(parent)
        for uicmd in (UICommand(Browser.COMMAND_GO_BACK(_command_handler=self),
                                _("Back"),
                                _("Go back to the previous location in browser history.")),
                      UICommand(Browser.COMMAND_GO_FORWARD(_command_handler=self),
                                _("Forward"),
                                _("Go forward to the following location in browser history.")),
                      UICommand(Browser.COMMAND_RELOAD(_command_handler=self),
                                _("Reload"),
                                _("Reload the current document.")),
                      UICommand(Browser.COMMAND_STOP_LOADING(_command_handler=self),
                                _("Stop"),
                                _("Stop loading the document.")),
                      UICommand(Browser.COMMAND_LOAD_URI(_command_handler=self),
                                _("Location"),
                                _("Current browser URI."),
                                ctrl=(LocationBar, dict(size=(600, 25), editable=False))),
                      ):
            uicmd.create_toolbar_ctrl(toolbar)
        toolbar.Realize()
        return toolbar

    def reload(self):
        """Reload the current browser document from its original source."""
        self._webview.Reload(wx.html2.WEBVIEW_RELOAD_NO_CACHE)

    def load_uri(self, uri, restrict_navigation=None):
        """Load browser content from given URL.

        Arguments:
          uri -- URI of the document to load.
          restrict_navigation -- URI prefix (string) to restrict further
            navigation.  None means no restriction.  If a string is passed, the
            user will not be able to navigate to URIs not matching given
            prefix.

        """
        self._resource_server.load_resources(None)
        self._restricted_navigation_uri = restrict_navigation
        self._webview.LoadURL(uri)

    def load_html(self, html, base_uri='', restrict_navigation=None, resource_provider=None):
        """Load browser content from given HTML string.

        Arguments:
          html -- HTML document to be loaded into the browser.
          base_uri -- base URI of the document.  Relative URIs within the
            document are relative to this URI.  Browser policies may also
            restrict loading further resources according to this URI.
          restrict_navigation -- URI prefix (string) to restrict further
            navigation.  None means no restriction.  If a string is passed, the
            user will not be able to navigate to URIs not matching given
            prefix.
          resource_provider -- 'lcg.ResourceProvider' instance providing
            external resources for the loaded document (images, scripts,
            stylesheets).  The HTML may refer to these resources and the
            browser will be able to load them if they can be obtained from the
            provider.

        """
        if sys.version_info[0] == 2 and isinstance(html, unistr):
            html = html.encode('utf-8')
        self._resource_server.load_resources(resource_provider)
        self._restricted_navigation_uri = restrict_navigation
        self._webview.SetPage(html, base_uri)

    def load_content(self, content, base_uri='', exporter_class=None):
        """Load browser content from 'lcg.ContentNode' instance.

        Arguments:

          content -- 'lcg.ContentNode' instance representing the document to be
            loaded into the browser.  The node content will be exported into
            HTML and displayed.  Can also be 'lcg.Content' which will be
            automatically wrapped by untitled 'lcg.ContentNode'.
          base_uri -- base uri of the document.  Relative URIs within the
            document are relative to this URI.  Browser policies may also
            restrict loading further resources according to this URI.
          exporter_class -- 'pytis.form.Browser.Exporter' is used by default
            for exporting the node's contents into HTML.  You may pass another
            exporter class derived from the default one if you want to
            customize the export.

        """
        if isinstance(content, lcg.ContentNode):
            node = content
        else:
            node = lcg.ContentNode('content', content=content, title='')
        exporter = self._exporter(exporter_class or self.Exporter)
        context = exporter.context(node, pytis.util.environment_language())
        html = exporter.export(context)
        self.load_html(html, base_uri=base_uri, resource_provider=node.resource_provider())


class mupdfProcessor(wx.lib.pdfviewer.viewer.mupdfProcessor):
    """Overriden to allow passing fitz.Document instances directly."""

    def __init__(self, parent, pdf_file):
        self.parent = parent
        if isinstance(pdf_file, basestring):
            # a filename/path string, pass the name to fitz.open
            pathname = pdf_file
            self.pdfdoc = fitz.open(pathname)
        if isinstance(pdf_file, fitz.Document):
            pathname = 'fileobject.pdf'
            self.pdfdoc = pdf_file
        else:
            # assume it is a file-like object, pass the stream content to fitz.open
            # and a '.pdf' extension in pathname to identify the stream type
            pathname = 'fileobject.pdf'
            if pdf_file.tell() > 0:     # not positioned at start
                pdf_file.seek(0)
            stream = bytearray(pdf_file.read())
            self.pdfdoc = fitz.open(pathname, stream)

        self.numpages = self.pdfdoc.pageCount
        page = self.pdfdoc.loadPage(0)
        self.pagewidth = page.bound().width
        self.pageheight = page.bound().height
        self.page_rect = page.bound()
        self.zoom_error = False     # set if memory errors during render

    def RenderPage(self, gc, pageno, scale=1.0):
        # The change in PyMuPDF=1.14.17 ("Changed methods Page.getPixmap,
        # Document.getPagePixmap to now use alpha=False as default"),
        # broke the rendering method of the page in wx.lib.pdfviewer.
        # So we have to override also this method and specify the keyword
        # argument "alpha" explicitly.
        #
        # The change in PyMuPDF>=1.15.0 made also previous fix
        # unusable. Now it is necessary to make another fix:
        # (see https://github.com/wxWidgets/Phoenix/issues/1350)
        """Render the set of pagedrawings into gc for specified page """
        page = self.pdfdoc.loadPage(pageno)
        matrix = fitz.Matrix(scale, scale)
        try:
            pix = page.getPixmap(matrix=matrix)   # MUST be keyword arg(s)
            if [int(v) for v in fitz.version[1].split('.')] >= [1,15,0]:
                # See https://github.com/wxWidgets/Phoenix/issues/1350
                bmp = wx.Bitmap.FromBuffer(pix.width, pix.height, pix.samples)
            else:
                bmp = wx.Bitmap.FromBufferRGBA(pix.width, pix.height, pix.samples)
            gc.DrawBitmap(bmp, 0, 0, pix.width, pix.height)
            self.zoom_error = False
        except (RuntimeError, MemoryError):
            if not self.zoom_error:     # report once only
                self.zoom_error = True
                dlg = wx.MessageDialog(self.parent, 'Out of memory. Zoom level too high?',
                                       'pdf viewer', wx.OK | wx.ICON_EXCLAMATION)
                dlg.ShowModal()
                dlg.Destroy()


class FileViewerButtonPanel(wx.lib.pdfviewer.pdfButtonPanel):
    """Button panel for FileViewer"""

    def CreateButtons(self):
        import wx.lib.pdfviewer.images as images
        import wx.lib.agw.buttonpanel as bp
        self.disabled_controls = []
        self.pagelabel = wx.StaticText(self, -1, 'Page')
        self.page = wx.TextCtrl(self, -1, size=(46, -1), style=wx.TE_CENTRE | wx.TE_PROCESS_ENTER)
        self.page.Enable(False)
        self.disabled_controls.append(self.page)
        self.page.Bind(wx.EVT_KILL_FOCUS, self.OnPage)
        self.Bind(wx.EVT_TEXT_ENTER, self.OnPage, self.page)
        self.maxlabel = wx.StaticText(self, -1, '          ')
        self.zoom = wx.ComboBox(self, -1, style=wx.CB_DROPDOWN | wx.TE_PROCESS_ENTER)
        self.zoom.Enable(False)
        self.disabled_controls.append(self.zoom)
        self.comboval = (('Actual size', 1.0), ('Fit width', -1), ('Fit page', -2),
                         ('25%', 0.25), ('50%', 0.5), ('75%', 0.75), ('100%', 1.0),
                         ('125%', 1.25), ('150%', 1.5), ('200%', 2.0), ('400%', 4.0),
                         ('800%', 8.0), ('1000%', 10.0))
        for item in self.comboval:
            self.zoom.Append(item[0], item[1])      # string value and client data
        self.Bind(wx.EVT_COMBOBOX, self.OnZoomSet, self.zoom)
        self.Bind(wx.EVT_TEXT_ENTER, self.OnZoomSet, self.zoom)
        panelitems = [
            ('btn', images.First.GetBitmap(), wx.ITEM_NORMAL, "First page", self.OnFirst),
            ('btn', images.Prev.GetBitmap(), wx.ITEM_NORMAL, "Previous page", self.OnPrev),
            ('btn', images.Next.GetBitmap(), wx.ITEM_NORMAL, "Next page", self.OnNext),
            ('btn', images.Last.GetBitmap(), wx.ITEM_NORMAL, "Last page", self.OnLast),
            ('ctrl', self.pagelabel),
            ('ctrl', self.page),
            ('ctrl', self.maxlabel),
            ('sep',),
            ('btn', images.ZoomOut.GetBitmap(), wx.ITEM_NORMAL, "Zoom out", self.OnZoomOut),
            ('btn', images.ZoomIn.GetBitmap(), wx.ITEM_NORMAL, "Zoom in", self.OnZoomIn),
            ('ctrl', self.zoom),
            ('btn', images.Width.GetBitmap(), wx.ITEM_NORMAL, "Fit page width", self.OnWidth),
            ('btn', images.Height.GetBitmap(), wx.ITEM_NORMAL, "Fit page height", self.OnHeight),
        ]
        self.Freeze()
        for item in panelitems:
            if item[0].lower() == 'btn':
                x_type, image, kind, popup, handler = item
                btn = bp.ButtonInfo(self, wx.ID_ANY, image, kind=kind,
                                    shortHelp=popup, longHelp='')
                self.AddButton(btn)
                btn.Enable(False)
                self.disabled_controls.append(btn)
                self.Bind(wx.EVT_BUTTON, handler, id=btn.GetId())
            elif item[0].lower() == 'sep':
                self.AddSeparator()
            elif item[0].lower() == 'ctrl':
                self.AddControl(item[1])
        self.Thaw()
        self.DoLayout()

    def SetProperties(self):
        import wx.lib.agw.buttonpanel as bp
        art = self.GetBPArt()
        art.SetGradientType(bp.BP_GRADIENT_VERTICAL)
        art.SetColor(bp.BP_GRADIENT_COLOUR_FROM, wx.Colour(225, 225, 225))
        art.SetColor(bp.BP_GRADIENT_COLOUR_TO, wx.Colour(235, 235, 235))
        art.SetColor(bp.BP_BORDER_COLOUR, wx.Colour(225, 225, 225))
        art.SetColor(bp.BP_BUTTONTEXT_COLOUR, wx.Colour(0, 0, 0))
        art.SetColor(bp.BP_SEPARATOR_COLOUR,
                     bp.BrightenColour(wx.Colour(220, 222, 224), 0.85))
        art.SetColor(bp.BP_SELECTION_PEN_COLOUR,
                     wx.SystemSettings.GetColour(wx.SYS_COLOUR_ACTIVECAPTION))


class FileViewer(wx.lib.pdfviewer.viewer.pdfViewer):
    """File viewer widget.

    Wx window displaying file preview in its main content area.

    Only PDF files are currently supported (using 'wx.lib.pdfviewer') but the
    intention is to possibly have one such widget supporting multiple file
    formats, such as images.

    """

    def __init__(self, parent):
        wx.lib.pdfviewer.viewer.pdfViewer.__init__(
            self, parent, -1, wx.DefaultPosition, wx.DefaultSize, wx.HSCROLL | wx.VSCROLL,
        )
        self.ShowLoadProgress = False

    def LoadFile(self, pdf_file):
        # Override this method use the overriden version of 'mupdfProcessor' (defined above).
        if isinstance(pdf_file, basestring):
            # a filename/path string, save its name
            self.pdfpathname = pdf_file
        else:
            self.pdfpathname = ''
        self.pdfdoc = mupdfProcessor(self, pdf_file)
        self.numpages = self.pdfdoc.numpages
        self.pagewidth = self.pdfdoc.pagewidth
        self.pageheight = self.pdfdoc.pageheight
        self.page_buffer_valid = False
        self.Scroll(0, 0)               # in case this is a re-LoadFile
        self.CalculateDimensions()      # to get initial visible page range
        # draw and display the minimal set of pages
        self.pdfdoc.DrawFile(self.frompage, self.topage)
        self.have_file = True
        # now draw full set of pages
        wx.CallAfter(self.pdfdoc.DrawFile, 0, self.numpages - 1)

    def load_file(self, data):
        """Display preview of given document in the viewer.

        'data' is either a file-like object or a 'fitz.Document' instance.

        If 'data' is None, empty, or of unsupported format, display an empty
        (gray) area.

        """
        if not data:
            self.Show(False)
        else:
            if hasattr(data, 'read'):
                import magic
                if hasattr(magic, 'detect_from_content'):
                    # Hack for temporary compatibility with both python 'magic' modules...
                    mime_type = magic.detect_from_content(data.read(1024)).mime_type
                else:
                    mime_type = magic.from_buffer(data.read(1024), mime=True)
                data.seek(0)
            else:
                assert isinstance(data, fitz.Document), data
                mime_type = 'application/pdf'
            try:
                if mime_type != 'application/pdf':
                    raise Exception(_("Unsupported file type: %s", mime_type))
                else:
                    self.Show(True)
                    self.LoadFile(data)
            except Exception as e:
                self.Show(False)
                app.echo(_("Loading document failed: %s", e), kind='error')

    def buttons(self):
        buttons = FileViewerButtonPanel(self.GetParent(), wx.ID_ANY,
                                        wx.DefaultPosition, wx.DefaultSize, 0)
        buttons.viewer = self
        self.buttonpanel = buttons
        return buttons


class FileViewerFrame(wx.Frame):
    """Standalone frame displaying file content preview."""

    def __init__(self, title, data, size=(800, 600)):
        """
        Arguments:
          title -- Frame title (string)
          data -- File-like object to show preview of
          size -- Initial frame size as a tuple of pixels

        """
        wx.Frame.__init__(self, None, title=title)
        sizer = wx.BoxSizer(wx.VERTICAL)
        viewer = FileViewer(self)
        sizer.Add(viewer.buttons(), 0, wx.EXPAND)
        sizer.Add(viewer, 1, wx.EXPAND)
        self.SetSizer(sizer)
        sizer.Fit(self)
        viewer.load_file(data)
        self.SetSize(size)
        self.Show()
        self.Raise()


def make_in_operator(column_id, spec_name, table_column_id, profile_id,
                     profile_name=None, condition=None, arguments=None):
    """Return pytis.presentation.IN operator instance for given args.

    Arguemnts match the arguments of 'pytis.presentation.IN' constructor.

    Profiles managed by 'FormProfileManager' (recognized by 'profile_id'
    prefix) are resolved to 'profile_name' and 'condition' and passed on.
    Other profiles are left to be resolved by 'pytis.presentation.IN'
    constructor.

    """
    if profile_id and profile_id.startswith(FormProfileManager.USER_PROFILE_PREFIX):
        manager = pytis.form.app.profile_manager
        data_object = pytis.util.data_object(spec_name)
        profile_condition, profile_name = manager.load_filter(spec_name, data_object, profile_id)
        if condition:
            condition = pytis.data.AND(condition, profile_condition)
        else:
            condition = profile_condition
    return pytis.presentation.IN(column_id, spec_name, table_column_id, profile_id,
                                 profile_name=profile_name, condition=condition,
                                 arguments=arguments)


# Převodní funkce

def char2px(window, x, y):
    """Return dimension in pixels for given dimension in characters.

    The input dimension is the number of "common" characters displayed in given
    widget.

    Arguments:

      window -- wx.Window instance inside which the dimension applies
      x -- width as an integer (number of characters)
      y -- height as an integer (number of characters)

    Returns: Pixel size as a tuple of two integers (width, height).

    """
    return dlg2px(window, 4 * x, 8 * y)


def dlg2px(window, x, y=None):
    """Return dimension in pixels for given dimension in "dialog units".

    The "dialog units" are defined by wxWidgets as 1/4 of a single character
    width horizontally and 1/8 of a single character height vertically, where
    the character is one "common" character displayed in given widget.

    Arguments:

      window -- wx.Window instance inside which the dimension applies
      x -- width as an integer (number of 1/4 characters)
      y -- height as an integer (number of 1/8 characters)

    Returns: Pixel size as a tuple of two integers (width, height) if y was
    passed (not None) or width directly as a single integer in the other case.

    """
    if y is None:
        y = 0
        single = True
    else:
        single = False
    dlgsize = (x, y)
    pxsize = wx.DLG_UNIT(window, dlgsize)
    if single:
        return pxsize[0]
    else:
        return pxsize


def acceskey_prefix(i):
    pad = {'f': '  ', 'i': '  ', 'j': '  ', 'l': '  ', 'm': '', 't': '  ', 'r': '  ', 'w': ''}
    if i < 26:
        index = chr(i + 97)
    else:
        index = str(i - 25)
    return '&' + index + '. ' + pad.get(index, ' ')


def orientation2wx(orientation):
    """Převeď konstantu třídy 'Orientation' na wx reprezentaci."""
    if orientation == Orientation.VERTICAL:
        return wx.VERTICAL
    elif orientation == Orientation.HORIZONTAL:
        return wx.HORIZONTAL
    else:
        raise ProgramError("Neplatná hodnota Orientation:", orientation)

# Pomocné funkce


def make_fullname(form_class, spec_name):
    """Return a fullname string for given form class and specification name.

    Arguments:
      form_class -- particular form class derived from 'pytis.form.Form'
      spec_name -- string name of a pytis specification (for resolver)

    The fullname string is used to refer to a form in DMP and other places
    where python objects (form instances) can not be refered directly (the
    reference is stored in the database).

    """
    module_name = form_class.__module__
    if module_name.startswith('pytis.form.'):
        # Make sure the module name is always the same (depending on how
        # modules were imported, it may be sometimes 'pytis.form' and sometimes
        # 'pytis.form.list').
        module_name = 'pytis.form'
    return 'form/%s.%s/%s//' % (module_name, form_class.__name__, spec_name)


def mitem(uicmd):
    """Return a 'MItem' instance for given 'UICommand' instance."""
    return MItem(uicmd.title(), command=uicmd.command(), args=uicmd.args(), help=uicmd.descr())


def popup_menu(parent, items, keymap=None, position=None):
    """Pop-up a wx context menu.

    Arguments:
       parent -- wx parent window of the menu
       items -- sequence of MItem, MSeparator or Menu instances
       keymap -- keymap to use for determination of menu command keyboard shortcuts
       position -- optional position of the menu as a tuple (x, y).  The menu is normally
         positioned automatically under the pointer so position may be needed when displaying menu
         in response to a keyboard command.

    """
    menu = Menu('', items).create(parent, keymap)
    parent.PopupMenu(menu, position or wx.DefaultPosition)
    menu.Destroy()


def get_icon(icon_id, type=wx.ART_MENU, size=(16, 16)):
    """Get icon by id and return the corresponding 'wx.Bitmap' instance.

    Arguments:

      icon_id -- string icon identifier.  It may be either one of 'wx.ART_*'
        constants or a filename.  The icons returned for wx constants should
        honour the pre-defined symbol in the current user interface theme.
        Identifiers not corresponding to wx constants are used as icon file
        names within the 'pytis.config.icon_dir'.  The '.png' suffix is added
        automatically, so the identifier consists just of a base name.
      type, size -- only relevant when icon_id as a 'wx.ART_*' constant.

    If an icon for given identifier is not found, returns None.

    """
    if icon_id is None:
        bitmap = None
    elif (sys.version_info[0] == 2 and icon_id.startswith('wx') or
          sys.version_info[0] > 2 and isinstance(icon_id, bytes)):
        bitmap = wx.ArtProvider.GetBitmap(icon_id, type, size)
    else:
        filename = os.path.join(pytis.config.icon_dir, icon_id)
        if os.path.exists(filename + '.png'):
            img = wx.Image(filename + '.png', type=wx.BITMAP_TYPE_PNG)
        elif os.path.exists(filename + '.svg'):
            import cairosvg
            with open(filename + '.svg') as f:
                data = f.read()
            png = cairosvg.svg2png(data, parent_width=size[0], parent_height=size[1])
            img = wx.Image(io.BytesIO(png), wx.BITMAP_TYPE_PNG)
            img = img.Scale(size[0], size[1], wx.IMAGE_QUALITY_HIGH)
        else:
            log(OPERATIONAL, "Could not find icon file:", filename + '.(svg|png)')
            return None
        bitmap = wx.Bitmap(img)
    if bitmap and bitmap.IsOk():
        return bitmap
    else:
        return None


def wx_focused_window():
    """Vrať aktuálně zaostřené wx okno, jako instanci 'wx.Window'."""
    return wx.Window.FindFocus()


def _init_wx_ctrl(ctrl, tooltip=None, update=False, enabled=True, width=None, height=None):
    if tooltip:
        ctrl.SetToolTip(tooltip)
    if update:
        # Bug: 'parent' is undefined!
        # wx_callback(wx.EVT_UPDATE_UI, ctrl, update)
        pass
    if not enabled:
        ctrl.Enable(False)
    if width:
        ctrl.SetSize((width, ctrl.GetSize().height))
        ctrl.SetMinSize((width, ctrl.GetSize().height))
    if height:
        ctrl.SetSize((ctrl.GetSize().width, height))
        ctrl.SetMinSize((ctrl.GetSize().width, height))


def wx_button(parent, label=None, icon=None, bitmap=None, id=-1, noborder=False,
              fullsize=False, command=None, callback=None, enabled=True, update=False,
              tooltip=None, size=None, width=None, height=None):
    """Create and setup a button.

    This is a convenience helper to allow simple button creation and setup in one step.

    Arguments:

      parent -- wx parent window
      label -- button label; the label is only used when no icon was specified or if the specified
        icon was not found.  Label should be always specified when using named icons (not stock
        icons), since it must be available in case the icon can not be found
      icon -- button icon identifier as used with 'get_icon'; if the icon is found, the label is
        ignored
      bitmap -- button bitmap; overrides both label and icon
      id -- wx id of the button; integer; may be useful for creating ``stock buttons''
      noborder -- if true, the button will not have the visible border
      fullsize -- the button is created with exact fit by default (smallest possible).  Pass true
        if you want normal size buttons (irrelevant when size is defined explicitly)
      command -- pytis command to invoke when button is pressed.  If defined, 'callback' must
        be None
      callback -- if specified, the given function will be associated with button press event.  The
        function will be called with `wx.Event' instance as first argument
      enabled -- if false, the button will be disabled
      update -- CURRENTLY IGNORED!
        if true the button availability will be periodically checked and the changes will
        be reflected by enabling/disabling the button when the 'command' becomes enabled/disabled.
        Currently not supported if 'callback' is used instead of 'command'.
      tooltip -- tooltip string
      size -- button size in pixels as a two-tuple (width, height)
      width -- width in pixels, overrides the width given by 'size'
      height -- height in pixels, overrides the height given by 'size'

    Returns a 'wx.Button' or 'wx.BitmapButton' instance.

    """
    if not bitmap and icon:
        bitmap = get_icon(icon, type=wx.ART_TOOLBAR)
    style = 0
    if not fullsize:
        style |= wx.BU_EXACTFIT
    if noborder:
        style |= wx.BORDER_NONE
    if size is None:
        size = wx.DefaultSize
    if bitmap:
        button = wx.BitmapButton(parent, id, bitmap, size=size, style=style)
    else:
        button = wx.Button(parent, id, label=label or '', size=size, style=style)
        if label and not fullsize and size == wx.DefaultSize:
            # Add small left and right padding to the label, otherwise
            # the label is edge to edge with BU_EXACTFIT.
            button.SetMinSize((button.Size.width + 18, button.Size.height))
    button._pytis_in_button_handler = False
    if command:
        assert callback is None
        cmd, args = command

        def callback(e, args=args, button=button):
            if button._pytis_in_button_handler:
                # The handler may get invoked recursively and break things,
                # e.g. prevent a PopupEditForm from closing.
                # The recursive invocation may happen at least in wx 2.9 on OS X:
                # Button press or form key action to close a form invokes this
                # callback, form closing generates a close event and that event
                # (for unknown reasons) generates this button event again.
                e.Skip()
            else:
                button._pytis_in_button_handler = True
                try:
                    if cmd.invoke(**args):
                        if wx.VERSION >= (2, 9):
                            # I don't know whether it behaves well in 2.8,
                            # so let's be on the safe side.
                            e.Skip()
                finally:
                    button._pytis_in_button_handler = False
        if tooltip:
            hotkey = pytis.form.global_keymap().lookup_command(cmd, args)
            if hotkey:
                tooltip += ' (' + hotkey_string(hotkey) + ')'
        # TODO: This causes the whole application to freeze when a dialog is closed.
        # if update:
        #     wx_callback(wx.EVT_UPDATE_UI, button, lambda e: e.Enable(cmd.enabled(**args)))
    if callback:
        wx_callback(wx.EVT_BUTTON, button, callback)
    _init_wx_ctrl(button, tooltip=tooltip, enabled=enabled, width=width, height=height)
    return button


def wx_choice(parent, choices, selected=None, tooltip=None, on_change=None,
              enabled=True, size=None, width=None, height=None, _combo=False):
    """Create and setup a choice control.

    This is just a convenience helper to allow choice creation and setup in one step.

    Arguments:

      parent -- wx parent window
      choices -- a sequence of available choices; the items may be directly strings or you can pass
        a sequence of (LABEL, VALUE) pairs, where VALUE is an arbitrary object.  VALUE will be used
        as choice data and may be later retrieved using the control's 'GetClientData()' method.
      selected -- the initially selected item; one of the 'choices' items.  If (LABEL, VALUE) pairs
        were used as 'choices', 'selected' refers to VALUE, otherwise directly to the choice
        string.
      tooltip -- tooltip string
      on_change -- if specified, the given function will be associated with the change event.  The
        function will be called with `wx.Event' instance as first argument.
      enabled -- if false, the control will be disabled.
      size -- button size in pixels as a two-tuple (width, height).
      width -- width in pixels, overrides the width given by 'size'.
      height -- height in pixels, overrides the height given by 'size'.

    Returns a 'wx.Choice' instance.

    """
    if _combo:
        cls = wx.ComboBox
        evt = wx.EVT_COMBOBOX
        style = 0
    else:
        cls = wx.Choice
        evt = wx.EVT_CHOICE
        style = wx.CB_READONLY
    if choices and isinstance(choices[0], (tuple, list)):
        labels = [label for label, value in choices]
        values = [value for label, value in choices]
    else:
        assert not choices or isinstance(choices[0], basestring)
        labels = choices
        values = None
    if size is None:
        size = wx.DefaultSize
    ctrl = cls(parent, -1, choices=labels, size=size, style=style)
    ctrl.SetSelection(0)
    if values:
        for i, value in enumerate(values):
            ctrl.SetClientData(i, value)
            if value == selected:
                ctrl.SetSelection(i)
    elif selected:
        ctrl.SetSelection(labels.index(selected))
    if on_change:
        wx_callback(evt, ctrl, on_change)
    _init_wx_ctrl(ctrl, tooltip=tooltip, enabled=enabled, width=width, height=height)
    return ctrl


def wx_combo(parent, choices, **kwargs):
    """Create and setup a combo box control.

    This is just a convenience helper to allow combo box creation and setup in one step.

    Arguments are the same as for 'wx_choice()'.

    Returns a 'wx.ComboBox' instance.

    """
    return wx_choice(parent, choices, _combo=True, **kwargs)


def wx_text_ctrl(parent, value=None, tooltip=None, on_key_down=None, on_text=None,
                 length=None, width=None, height=None, readonly=False, enabled=True, _spin=False):
    if _spin:
        cls = wx.SpinCtrl
    else:
        cls = wx.TextCtrl
    ctrl = cls(parent, -1, style=(readonly and wx.TE_READONLY or 0))
    if on_key_down:
        wx_callback(wx.EVT_KEY_DOWN, ctrl, on_key_down)
    if on_text:
        wx_callback(wx.EVT_TEXT, ctrl, on_text)
    wx_callback(wx.EVT_TEXT_PASTE, ctrl, lambda e: paste_from_clipboard(ctrl))
    if value is not None:
        ctrl.SetValue(value)
    if length:
        assert width is None
        width = dlg2px(ctrl, 4 * length + 12)
        if _spin:
            width += 60  # Add space for the spin buttons...
    _init_wx_ctrl(ctrl, tooltip=tooltip, enabled=enabled, width=width, height=height)
    return ctrl


def wx_spin_ctrl(parent, value=None, **kwargs):
    return wx_text_ctrl(parent, value=value, _spin=True, **kwargs)


def wx_checkbox(parent, label=None, tooltip=None, checked=False):
    checkbox = wx.CheckBox(parent, -1, label=label)
    if tooltip is not None:
        checkbox.SetToolTip(unistr(tooltip))
    checkbox.SetValue(checked)
    return checkbox


def wx_text_view(parent, content, format=None, width=None, height=None, resources=()):
    """Return a wx widget displaying given text content.

    Arguments:

      content -- text content to be displayed as a string or a 'lcg.Content'
        instance.  If string, its formatting must be further specified by the
        arguemnt 'format' (see below).
      format -- input format of the text content as one of 'TextFormat'
        constants.  In case of plain text ('TextFormat.PLAIN'), the text is
        displayed as is in a 'wx.TextCtrl' widget.  In case of
        'TextFormat.HTML', the HTML content is displayed within an embedded
        HTML 'Browser' window.  The content can be either a complete document
        (including <html>, <head> and <body> tags) or an HTML fragment (the
        surrounding tags will be added automatically).  In case of
        'TextFormat.LCG', the text is first processed by LCG Parser and
        exported into HTML and further treated as HTML.
     width -- window width in characters.  If None, the width is automatically
       set to fit the text width (the longest line length) when 'format' is
       TextFormat.PLAIN or 90 characters for other formats.
     height -- window height in characters.  If None, the width is
       automatically set to fit the text height (number of lines with maximum
       of 20 characters) when 'format' is TextFormat.PLAIN or 30 characters for
       other formats.
     resources -- sequence of statically defined 'lcg.Resource' instances
       representing external files (such as images) used within the content.
       Only applicable when 'format' is 'TextFormat.LCG'.

    """
    import wx
    if format == TextFormat.PLAIN:
        assert isinstance(content, basestring)
        lines = content.splitlines()
        if width is None:
            width = min(max([len(l) for l in lines]), 80)
        if height is None:
            height = min(len(lines), 20)
        style = wx.TE_MULTILINE | wx.TE_DONTWRAP | wx.TE_READONLY
        ctrl = wx.TextCtrl(parent, style=style)
        ctrl.SetValue(content)
        px_width, px_height = char2px(ctrl, width, height)
        # Slightly enlarge the size to avoid scrollbars when not necessary (for small sizes).
        ctrl.SetInitialSize((px_width + 30, px_height + 2))
        return ctrl
    else:
        content = pytis.util.content(content, format=format, resources=resources)
        browser = Browser(parent)
        browser.load_content(content)
        # We can' adjust the default size according to the content size, but since
        browser.SetInitialSize(char2px(browser, width or 90, height or 30))
        return browser
