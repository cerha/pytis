# -*- coding: utf-8 -*-

# Copyright (C) 2018-2025 Tomáš Cerha <t.cerha@gmail.com>
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
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

"""Implementation of common UI elements for the wx Widgets application."""

from __future__ import print_function

from past.builtins import basestring
from builtins import str
from future import standard_library
from future.utils import python_2_unicode_compatible

import sys
import copy
import os
import string
import types
import http.server
import socketserver
import io
import threading
import time
import mimetypes
import fitz

import wx
import wx.html2
import wx.lib.agw.supertooltip as supertooltip
import wx.lib.pdfviewer

import lcg

import pytis
import pytis.api
import pytis.form
import pytis.output
import pytis.presentation
import pytis.util
from pytis.api import app
from pytis.presentation import (
    Orientation, TextFormat, StatusField, MenuItem, MenuSeparator, Command,
)
from pytis.util import find, xtuple, log, DEBUG, EVENT, OPERATIONAL, ProgramError
from .event import wx_callback, top_level_exception
from .command import CommandHandler, UICommand
from .managers import FormProfileManager

try:
    import darkdetect
    darkmode = darkdetect.isDark()
except ImportError:
    darkmode = False

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
DEFAULT_WINDOW_BACKGROUND_COLOUR = '#181818' if darkmode else '#e8e8e8'

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
    ctrl = wx.TextCtrl(pytis.form.app.GetTopWindow(), -1, text)
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
            mitems.append(MenuItem(_('Open file "%s"', filename),
                                   command=Command(Application.handled_action,
                                                   handler=open_file,
                                                   field_id=fid,
                                                   filename=filename,
                                                   enabled=can_open(f)),
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
    """Assignment of keyboard shortcuts to commands."""

    def __init__(self, parent=None):
        """Arguments:

          parent -- parent key map as a 'Keymap' instance or None.  If not
            None, all assignments from the parent map are inherited if not
            overriden.

        """
        if parent is None:
            keymap = {}
        else:
            keymap = dict([(key, copy.copy(keydef))
                           for key, keydef in parent._keymap.items()])
        self._keymap = keymap

    def __str__(self):
        return '<Keymap: %s>' % (self._keymap,)

    def _define_key(self, key, command):
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
            keydef[0:0] = [command]
        elif rest:
            keydef._define_key(rest, command)
        else:
            log(OPERATIONAL, "Key '%s' is already used as a prefix key." % (prefix,))

    def define_key(self, key, command):
        """Assign given 'command' to given 'key'.

        Arguments:

          key -- string or a sequence of strings defining the keyboard shortcut
            or a sequence of keyboard shortcuts (see below).
          command -- the assigned command as a 'Command' instance.

        Keyboard shortcut strings are constructed as follows:

        - Keys corresponding to the characters of English alphabet are
          represented by strings of that character.  Characters are case
          sensitive (see also the Shift modifier below).

        - Function keys F1 to F12 are represented by strings 'F1' to 'F12'.

        - Arrow keys are represented by strings 'Up', 'Down', 'Left', 'Right'.

        - Keys 'Escape', 'Enter', 'Tab', 'Insert', 'Delete', 'Backspace',
          'Home', 'End', 'Prior' and 'Next' are represented by these strings.

        - Key with the Control modifier is written as 'Ctrl-<KEY>', where
          '<KEY>' is the representation of the key without this modifier.

        - Key with the Alt modifier is written as 'Alt-<KEY>'.

        - Key with the Shift modifier is written as 'Shift-<KEY>'.

        - Modifiers can be combined.  In this case they are always written in
          the order Ctrl, Alt, Shift (for example 'Ctrl-Alt-s' or 'Alt-Shift-Tab',
          not 'Alt-Ctrl-x').

        """
        key = xtuple(key)
        if key != (None,):
            self._define_key(key, command)

    def lookup_key(self, key):
        """Return the command associated with given 'key'.

        Arguments:

          key -- key string representation in notation described by
          'define_key()' docstring.

        Returns the 'Command' instance if key defines a command or a 'Keymap'
        instance in case of a multi-key definition or 'None' if given key
        doesn't define anything.

        """
        try:
            return self._keymap[key]
        except KeyError:
            return None

    def lookup_command(self, command):
        """Return the kyeboard shortcut associated with given 'command'.

        Arguments:

          command -- the 'Command' instance

        Returns a keyboard shortcut definition if given command with its
        arguments is bound to a keyboard shortcut.  The returned keyboard
        shortcut definition is always returned as a tuple (even if it contains
        just a single key).  Returns none if there is no key defined for given
        command (after previously calling the 'define_key()').

        """
        for key, keydef in self._keymap.items():
            if isinstance(keydef, Keymap):
                k = keydef.lookup_command(command)
                if k is not None:
                    return (key,) + k
            else:
                if command in keydef:
                    return (key,)
        return None

    def keys(self):
        """Return the sequence of all valid keys."""
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
        """Arguments:

          widgets -- wx widget, or their sequence, for which key handling
            should be initialized.  If None, 'self' is used as the widget.

        """
        if widgets is None:
            widgets = isinstance(self, wx.Window) and (self,) or ()
        self._handle_keys(*xtuple(widgets))
        self._wx_key = WxKey()
        self._prefix_key_sequence = []
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
        """Register 'self.on_key_down' as key event handler for given 'wx.Window' instances."""
        for widget in widgets:
            wx_callback(wx.EVT_KEY_DOWN, widget, self.on_key_down)

    def _maybe_invoke_command(self, key_commands):
        for command in key_commands:
            if command.handler is self and command.enabled:
                if __debug__:
                    log(DEBUG, 'Found command for the key:', command)
                command.invoke()
                return True

        else:
            guardian = self._key_guardian
            if guardian is None:
                if __debug__:
                    log(DEBUG, 'No guardian remaining.')
                return False
            else:
                if __debug__:
                    log(DEBUG, 'Passing on to guardian:', guardian)
                return guardian._maybe_invoke_command(key_commands)

    def _get_keymap(self):
        if self.keymap is None:
            guardian = self._key_guardian
            if guardian is None:
                gkeymap = pytis.form.app.keymap
            else:
                gkeymap = guardian._get_keymap()
            self.keymap = Keymap(gkeymap)
            if __debug__:
                log(DEBUG, 'Keymap created:', (self, self.keymap))
        return self.keymap

    def define_key(self, key, command):
        """Definuj klávesovou zkratku v klávesové mapě této instance.

        Klávesová mapa nemusí být dostupná v době inicializace instance, takže
        není možné definovat klávesové zkratky přímo.  Tato metoda zaručuje, že
        předané klávesové zkratky budou dříve nebo později správně uplatněny.

        Argumenty jsou shodé jako v metodě 'Keymap.define_key()'.

        """
        keymap = self._get_keymap()
        keymap.define_key(key, command)

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
            log(DEBUG, 'Key event:', event)
        wk = self._wx_key
        if not wk.is_true_key(event):
            return
        app.echo('')
        if __debug__:
            log(DEBUG, 'Key event processed by:', str(self))
        guardian = self._key_guardian
        if ((self._current_keymap is None or
             not isinstance(pytis.form.last_user_event(), wx.KeyEvent))):
            self._current_keymap = self._get_keymap()
        if __debug__:
            log(DEBUG, 'Current keymap:', str(self._current_keymap))
        key = wk.event_key(event)
        keydef = self._current_keymap.lookup_key(key)
        if isinstance(keydef, Keymap):
            if __debug__:
                log(DEBUG, 'Prefix key:', keydef)
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
                    log(DEBUG, 'Keypress passed on to guardian:', guardian)
                return guardian.on_key_down(event, dont_skip)
            if dont_skip:
                if __debug__:
                    log(DEBUG, 'Keypress ignored')
            else:
                if __debug__:
                    log(DEBUG, 'Key event skipped')
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

        def _update_text(self, text):
            self._text = text
            if text and self._icon:
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
            self.api_text = text
            self.api_icon = icon
            self.api_tooltip = tooltip

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

        @api_text.setter
        def api_text(self, text):
            # Prevent status bar blinking by checking against the current value.
            text = unistr(text or '')
            if text != self._text:
                self._update_text(text)

        @property
        def api_icon(self):
            return self._icon

        @api_icon.setter
        def api_icon(self, icon):
            if icon != self._icon:
                self._icon = icon
                if self._bitmap is not None:
                    self._bitmap.Destroy()
                if icon is not None:
                    # Make sure text padding is added to make space for the icon.
                    self._update_text(self._text)
                    bitmap = get_icon(icon)
                    if bitmap:
                        self._bitmap = bmp = wx.StaticBitmap(self._sb, bitmap=bitmap)
                        wx_callback(wx.EVT_LEFT_DOWN, bmp, self._on_click)
                        self.update_bitmap_position()
                    else:
                        self._bitmap = None
                else:
                    self._bitmap = None

        @property
        def api_tooltip(self):
            tooltip = self._tooltip
            if callable(tooltip):
                tooltip = self._tooltip = tooltip()
            return tooltip

        @api_tooltip.setter
        def api_tooltip(self, tooltip):
            self._tooltip = tooltip


    def __init__(self, parent, fields):
        """Arguments:

          parent -- parent wx Window.
          fields -- list of 'pytis.presentation.StatusField' instances.

        """
        # Insert a dummy first field because wx uses the first field
        # internally and overwrites our contents unexpectedly.
        fields.insert(0, pytis.presentation.StatusField('__dummy__', width=0))
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
        # Don't expose the dummy field - see __init__().
        return self._fields[1:]


class InfoWindow(object):
    """Nemodální okno pro zobrazení textových informací."""

    def __init__(self, title, text, format=TextFormat.PLAIN, parent=None, _name='info', **kwargs):
        """Display information window in a standalone frame.

        Arguments:

          title -- Frame title as a basestring.
          parent -- parent wx Frame or None to use the main application frame
          text, format, **kwargs -- passed to 'wx_text_view()' ('text' as 'content').

        """
        frame = wx.Dialog(parent or pytis.form.app.GetTopWindow(), title=title, name=_name,
                          style=wx.DEFAULT_DIALOG_STYLE | wx.RESIZE_BORDER)
        # Temporarily use a modal dialog instead an ordinary frame to work
        # around the problem of closing a frame whose parent is a modal dialog
        # in StructuredTextField.preview().  Once that is sorted out, a
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
        self._selected_profile_id = None
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
            profile = self._current_form.profiles()[self._selected_profile_index]
            self._current_form.apply_profile(profile_id=profile.id())

    def _append_label(self, label, toplevel=True):
        ctrl = self._listctrl
        i = ctrl.GetItemCount()
        ctrl.InsertItem(i, label)
        ctrl.SetItemBackgroundColour(i, wx.Colour(60, 60, 60) if darkmode
                                     else wx.Colour(225, 225, 225))
        if toplevel:
            ctrl.SetItemFont(i, wx.Font(ctrl.GetFont().GetPointSize(),
                                        wx.FONTFAMILY_DEFAULT, wx.FONTSTYLE_NORMAL,
                                        wx.FONTWEIGHT_BOLD))
        ctrl.SetItemData(i, -1)

    def _append_profile(self, profile, index, select=False, indent='', initial=False):
        ctrl = self._listctrl
        title = indent + profile.title()
        if profile.errors():
            title += ' ' + _("(invalid)")
        i = ctrl.GetItemCount()
        ctrl.InsertItem(i, title)
        ctrl.SetItemData(i, index)
        if initial:
            # 0 is the star icon index in the image list created by ctrl.AssignImageList() below.
            ctrl.SetItemImage(i, 0)
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
        images = wx.ImageList(16, 16)
        images.Add(get_icon('star'))
        ctrl.AssignImageList(images, wx.IMAGE_LIST_SMALL)
        return True

    def GetControl(self):
        # Return the widget that is to be used for the popup.
        return self._listctrl

    def GetAdjustedSize(self, minWidth, prefHeight, maxHeight):
        # Called just prior to displaying the popup.
        # Fill menu items before each popup and delete them on dismiss to
        # avoid having to update the menu during form profile list update.
        self._current_form = form = pytis.form.app.current_form()
        profiles = form.profiles()
        current = form.current_profile()
        initial = form.initial_profile()

        def append_system_profiles(items, level=0):
            indent = 3 * level * ' '
            for item in items:
                if isinstance(item, pytis.presentation.Profile):
                    profile = find(item.id(), profiles, key=lambda p: p.id())
                    self._append_profile(profile, profiles.index(profile), profile is current,
                                         indent=indent, initial=profile is initial)
                else:
                    self._append_label(indent + item.title(), False)
                    append_system_profiles(item.items(), level=level + 1)
        self._append_label(_("System Profiles"))
        # Default profile is always the first in form.profiles().
        self._append_profile(profiles[0], 0, profiles[0] is current, initial=profiles[0] is initial)
        append_system_profiles(form.view().profiles())
        self._append_label(_("User Profiles"))
        for i, profile in enumerate(profiles):
            if profile.id().startswith(FormProfileManager.USER_PROFILE_PREFIX):
                self._append_profile(profile, i, profile is current, initial=profile is initial)
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


class ProfileSelector(wx.ComboCtrl, CommandHandler):
    """Toolbar control for form profile selection and management."""

    def __init__(self, parent, command, size):
        wx.ComboCtrl.__init__(self, parent, style=wx.TE_PROCESS_ENTER, size=size)
        self._popup = ProfileSelectorPopup()
        self.SetPopupControl(self._popup)
        self._on_enter_invoke = None
        # SetButtonPosition works around incorrect initial text control sizing in
        # codebook form, which starts with size 10x35 and resizes to the correct
        # size only after resizing the parent Dialog manually.
        self.SetButtonPosition(width=20, height=size[1] - 1)
        ctrl = self.GetTextCtrl()
        ctrl.SetEditable(False)
        wx_callback(wx.EVT_UPDATE_UI, self, self._on_ui_event)
        wx_callback(wx.EVT_RIGHT_DOWN, self, self._on_context_menu)
        wx_callback(wx.EVT_RIGHT_DOWN, ctrl, self._on_context_menu)
        wx_callback(wx.EVT_TEXT_ENTER, ctrl, self._on_enter) # Does not work on macOS...
        wx_callback(wx.EVT_KEY_DOWN, self, self._on_key_down)
        wx_callback(wx.EVT_KEY_DOWN, ctrl, self._on_key_down)
        # TODO: Add button for context menu invocation.

    def _on_ui_event(self, event):
        ctrl = self.GetTextCtrl()
        if pytis.form.app.top_window() is None:
            event.Enable(False)
            if ctrl.GetValue() != '':
                ctrl.SetValue('')
        else:
            event.Enable(True)
            if not ctrl.IsEditable() and app.form:
                current_profile = app.form.profile
                if current_profile and ctrl.GetValue() != current_profile.title():
                    ctrl.SetValue(current_profile.title())
            if Command(pytis.form.LookupForm.update_profile).enabled:
                # Indicate changed profile by color (update is enabled for changed profiles).
                color = wx.Colour(255 if darkmode else 200, 0, 0)
            else:
                color = wx.Colour(255, 255, 255) if darkmode else wx.Colour(0, 0, 0)
            ctrl.SetForegroundColour(color)

    def _on_context_menu(self, event):
        menu = (
            MenuItem(_("Save current profile settings"),
                     command=Command(pytis.form.LookupForm.update_profile),
                     help=_("Update the saved profile according to the current form setup.")),
            MenuItem(_("Duplicate current profile"),
                     command=Command(self.edit_profile_title, new=True),
                     help=_("Create a new profile according to the current form setup.")),
            MenuItem(_("Rename current profile"),
                     command=Command(self.edit_profile_title),
                     help=_("Change the name of the current profile and save it.")),
            MenuItem(_("Delete current profile"),
                     command=Command(pytis.form.LookupForm.delete_profile),
                     help=_("Delete the selected saved profile.")),
            MenuItem(_("Use this profile automatically on form starup"),
                     command=Command(pytis.form.LookupForm.set_initial_profile),
                     help=_("Automatically switch to this profile "
                            "when this form is opened next time.")),
            MenuSeparator(),
            MenuItem(_("Restore to previously saved form settings"),
                     command=Command(pytis.form.LookupForm.reload_profile),
                     help=_("Discard changes in form settings since the profile was last saved.")),
            MenuItem(_("Restore to default form settings"),
                     command=Command(pytis.form.LookupForm.reset_profile),
                     help=_("Discard all user changes in form settings.")),
            MenuItem(_("Export user profiles to file"),
                     command=Command(pytis.form.LookupForm.export_profiles),
                     help=_("Export user defined form profiles to a file.")),
            MenuItem(_("Import profiles from file"),
                     command=Command(pytis.form.LookupForm.import_profiles),
                     help=_("Import form profiles from a file.")),
        )
        pytis.form.app.popup_menu(self, menu)

    @Command.define
    def edit_profile_title(self, new=True):
        ctrl = self.GetTextCtrl()
        if new:
            ctrl.SetValue('')
            self._on_enter_invoke = pytis.form.LookupForm.save_new_profile
        else:
            ctrl.SelectAll()
            self._on_enter_invoke = pytis.form.LookupForm.rename_profile
        ctrl.SetFocus()
        app.echo(_("Enter the profile name and press ENTER when done."))

    def _can_edit_profile_title(self, new=True):
        return (self._on_enter_invoke is None or
                Command(self._on_enter_invoke, title=self.GetValue()).enabled)

    def _on_enter(self, event):
        command_method = self._on_enter_invoke
        if command_method:
            self._on_enter_invoke = None
            self.GetTextCtrl().SetEditable(False)
            command = Command(command_method, title=self.GetValue())
            command.invoke()
        else:
            event.Skip()

    def _on_key_down(self, event):
        event.Skip()
        code = event.GetKeyCode()
        if code in (wx.WXK_ESCAPE, wx.WXK_TAB, wx.WXK_RETURN, wx.WXK_NUMPAD_ENTER):
            pytis.form.app.current_form().focus()


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

    def __init__(self, parent, command, size=None):
        self._command = command
        wx.Choice.__init__(self, parent, choices=self._CHOICES, size=size or wx.DefaultSize)
        wx_callback(wx.EVT_UPDATE_UI, self, self._on_ui_event)
        wx_callback(wx.EVT_CHOICE, self, self._on_selection)

    def _on_ui_event(self, event):
        if self._command.enabled:
            event.Enable(True)
            self.SetSelection(self._command.handler.current_heading_level())
        else:
            event.Enable(False)

    def _on_selection(self, event):
        self._command.handler.heading(level=event.GetSelection())


class FormStateToolbarControl(wx.BitmapButton):
    """Special toolbar control for form commands with current state indication.

    A custom toolbar control indicating the current form state by changing the
    visible icon.  Available icons are defined by '_ICONS' and the currently
    displayed icon is returned by '_current_icon_index()'.

    """
    _ICONS = ()
    """Sequence of all possible icons as values for the first argument to 'get_icon()'."""

    def __init__(self, parent, command, size=None):
        self._toolbar = parent
        self._command = command
        self._bitmaps = [get_icon(icon, type=wx.ART_TOOLBAR) or
                         get_icon(wx.ART_ERROR, type=wx.ART_TOOLBAR)
                         for icon in self._ICONS]
        self._current_bitmap = self._bitmaps[0]
        wx.BitmapButton.__init__(self, parent, -1, self._current_bitmap,
                                 size=size or wx.DefaultSize,
                                 style=wx.BU_EXACTFIT | wx.NO_BORDER)
        wx_callback(wx.EVT_BUTTON, self, self._on_click)
        wx_callback(wx.EVT_UPDATE_UI, self, self._on_update_ui)

    def _on_click(self, event):
        self._command.invoke()

    def _on_update_ui(self, event):
        if self._command.enabled:
            event.Enable(True)
            form = pytis.form.app.current_form(inner=False)
            new_bitmap = self._bitmaps[self._current_icon_index(form)]
            if self._current_bitmap != new_bitmap:
                self._current_bitmap = new_bitmap
                self.SetBitmapLabel(new_bitmap)
                self._toolbar.Realize()
        else:
            event.Enable(False)

    def _current_icon_index(self, form):
        """Implement this method to return the index of the active icon in _ICONS."""
        pass


class KeyboardSwitcher(wx.BitmapButton):
    """Special toolbar control for keyboard layout switching and indication.

    """
    _ICONS = ()
    """Sequence of all possible icons as values for the first argument to 'get_icon()'."""

    def __init__(self, parent, command, size=None):
        self._toolbar = parent
        layouts = pytis.config.keyboard_layouts
        self._bitmaps = dict([(icon, get_icon(icon, type=wx.ART_TOOLBAR) or
                               get_icon(wx.ART_ERROR, type=wx.ART_TOOLBAR))
                              for title, icon, command in layouts])
        self._menu = [MenuItem(title,
                               command=Command(pytis.form.Application.handled_action,
                                               handler=self._switch_layout,
                                               system_command=system_command,
                                               icon=icon,
                               ))
                      for title, icon, system_command in layouts]
        layout = find(pytis.config.initial_keyboard_layout, layouts, lambda x: x[2]) or layouts[0]
        icon, system_command = layout[1:]
        os.system(system_command)
        wx.BitmapButton.__init__(self, parent, -1, self._bitmaps[icon],
                                 size=size or wx.DefaultSize,
                                 style=wx.BU_EXACTFIT | wx.NO_BORDER)
        wx_callback(wx.EVT_BUTTON, self, self._on_click)

    def _on_click(self, event):
        pytis.form.app.popup_menu(self._toolbar, self._menu)

    def _switch_layout(self, system_command, icon):
        os.system(system_command)
        pytis.config.initial_keyboard_layout = system_command
        self.SetBitmapLabel(self._bitmaps[icon])
        self._toolbar.Realize()


class DualFormSwitcher(FormStateToolbarControl):
    """Special toolbar control for DualForm.other_form.

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
    """Special toolbar control for DualForm.resplit.

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

    def __init__(self, parent, command, editable=True, size=None):
        self._toolbar = parent
        self._command = command
        wx.TextCtrl.__init__(self, parent, -1, size=size or wx.DefaultSize,
                             style=wx.TE_PROCESS_ENTER, name='location-bar')
        self.SetEditable(editable)
        wx_callback(wx.EVT_UPDATE_UI, self, self._on_update_ui)
        if editable:
            wx_callback(wx.EVT_TEXT_ENTER, self, self._on_enter)
        else:
            from .inputfield import InputField
            self.SetOwnBackgroundColour(InputField.DISABLED_FIELD_BACKGROUND_COLOR)
            self.Refresh()
        browser = command.handler
        browser.set_callback(browser.CALL_URI_CHANGED, self.SetValue)
        self._want_focus = 0

    def _on_update_ui(self, event):
        event.Enable(self._command.enabled)
        if self._want_focus:
            self.SetFocus()
            # Nasty hack - see set_focus for explanation.
            self._want_focus -= 1

    def _on_enter(self, event):
        self._command.handler.load_uri(self.GetValue())

    def set_focus(self):
        # This is a total hack - calling SetFocus() is ignored at form startup, but
        # calling it repeatedly in _on_update_ui seems to help finally.  Three times
        # seems to work, so rather repeat it four times to be sure...
        self._want_focus = 4


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
        self._reload = None
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
        elif uri != 'about:blank':
            self._reload = None
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
            if not isinstance(proc, pytis.presentation.HelpProc):
                raise ProgramError("Unable to call '%s' from help. "
                                   "Use the 'pytis.presentation.help_proc' decorator!" % uri[5:])
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

    @Command.define
    def go_forward(self):
        """Go to the next page in browser history"""
        self._webview.GoForward()

    def _can_go_forward(self):
        return self._webview.CanGoForward()

    @Command.define
    def go_back(self):
        """Go to the previous page in browser history"""
        self._webview.GoBack()

    def _can_go_back(self):
        return self._webview.CanGoBack()

    @Command.define
    def stop_loading(self):
        """Stop loading"""
        self._webview.Stop()

    def _can_stop_loading(self):
        return self._webview.IsBusy()

    @Command.define
    def reload(self):
        """Reload the current browser document from its original source."""
        self._webview.Reload(wx.html2.WEBVIEW_RELOAD_NO_CACHE)

    @Command.define
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

    def guardian(self):
        return self._guardian

    def toolbar(self, parent):
        """Create browser toolbar and return it as a 'wx.ToolBar' instance.

        The toolbar is not part of the browser pane.  If a toolbar is desired,
        it must be created separately using this method and added to the user
        interface together with the main browser pane (the 'Browser' instance
        itself).

        """
        return wx_toolbar(parent, (
            UICommand(Command(self.go_back),
                      _("Back"),
                      _("Go back to the previous location in browser history.")),
            UICommand(Command(self.go_forward),
                      _("Forward"),
                      _("Go forward to the following location in browser history.")),
            UICommand(Command(self.reload),
                      _("Reload"),
                      _("Reload the current document.")),
            UICommand(Command(self.stop_loading),
                      _("Stop"),
                      _("Stop loading the document.")),
            UICommand(Command(self.load_uri),
                      _("Location"),
                      _("Current browser URI."),
                      ctrl=(LocationBar, dict(size=(600, None), editable=False))),
        ))

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
        # Avoid dismissing the page contents on reload (form refresh in web form).
        # This way it works at least until we navigate away (when self._reload
        # is cleared).  To fix it also when getting back in history, we would
        # need to remember the context for each history item, which now seems
        # an overkill.
        self._reload = lambda: self.load_html(html,
                                              base_uri=base_uri,
                                              restrict_navigation=restrict_navigation,
                                              resource_provider=resource_provider)
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
        # TODO:
        # New versions of PyMuPDF have a lot of differences
        # to old versions, but they are not available for
        # Python2. In order to support Python2 and Python3
        # we have to make some adjustments here.
        # These adjustments can be removed as soon as
        # we completely switch to Python3
        if hasattr(fitz.Document, "is_pdf"):
            self.fitz_obsolete = False
        else:
            self.fitz_obsolete = True
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

        if self.fitz_obsolete:
            self.pdfdoc.page_count = self.pdfdoc.pageCount
            self.pdfdoc.load_page = self.pdfdoc.loadPage
        self.numpages = self.pdfdoc.page_count
        page = self.pdfdoc.load_page(0)
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
        page = self.pdfdoc.load_page(pageno)
        if self.fitz_obsolete:
            page.get_pixmap = page.getPixmap
        matrix = fitz.Matrix(scale, scale)
        try:
            pix = page.get_pixmap(matrix=matrix)   # MUST be keyword arg(s)
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

    User defined profiles (managed by 'FormProfileManager', recognized by
    'profile_id' prefix) are resolved to 'profile_name' and 'condition' and
    passed on.  Other profiles are left to be resolved by
    'pytis.presentation.IN' constructor.

    """
    if profile_id and profile_id.startswith(FormProfileManager.USER_PROFILE_PREFIX):
        manager = pytis.form.app.form_profile_manager
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


def uicommand_mitem(uicmd):
    """Return a 'MenuItem' instance for given 'UICommand' instance."""
    return MenuItem(uicmd.title(), command=uicmd.command(), help=uicmd.descr())


def uicommand_toolbar_ctrl(toolbar, uicmd):
    """Add a toolbar control for given UICommand into given wx 'toolbar'.

    This method adds command control into the toolbar.  The default control
    is a simple button which invokes the command on click.  More
    sophisticated controls may be specified using the 'ctrl' constructor
    argument.

    """
    title = uicmd.title()
    descr = uicmd.descr()
    ctrl_cls = uicmd.ctrl()
    if ctrl_cls:
        if isinstance(ctrl_cls, tuple):
            ctrl_cls, kwargs = ctrl_cls
        else:
            kwargs = {}
        ctrl = ctrl_cls(toolbar, uicmd, **kwargs)
        ctrl.SetToolTip(title)
        tool = toolbar.AddControl(ctrl)
        toolbar.SetToolLongHelp(tool.GetId(), descr)  # Doesn't work...
    else:
        command = uicmd.command()
        assigned_icon = command_icon(command)
        if assigned_icon is None:
            raise Exception("No icon assigned for command: {}".format(command))
        icon = get_icon(assigned_icon, type=wx.ART_TOOLBAR)
        if icon is None:
            icon = get_icon(wx.ART_ERROR, type=wx.ART_TOOLBAR)
        tool = toolbar.AddTool(-1, title, icon, wx.NullBitmap, shortHelp=title, longHelp=descr)
        parent = toolbar.GetParent()
        wx_callback(wx.EVT_TOOL, parent, lambda event: command.invoke(), source=tool)
        wx_callback(wx.EVT_UPDATE_UI, parent,
                    lambda event: event.Enable(command.enabled), source=tool)


_command_icons = None

def command_icon(command):
    """Return the icon identifier for given command and its arguments.

    Arguments:

      command -- 'Command' instance.

    The icon which best matches with given command with its arguments is searched
    within 'COMMAND_ICONS' specification.

    The returned value is the icon identifier as accepted by 'get_icon()'.

    """
    global _command_icons
    if _command_icons is None:
        _command_icons = {}
        from .defaults import COMMAND_ICONS
        for cmd, icon in COMMAND_ICONS:
            icons = _command_icons.setdefault(cmd.name, [])
            icons.append((cmd.args, icon))
    try:
        icons = _command_icons[command.name]
    except KeyError:
        pass
    else:
        for iargs, icon in icons:
            for k, v in iargs.items():
                if k not in command.args or command.args[k] != v:
                    break
            else:
                return icon
    if ((command.name == 'RecordForm.context_action'
         and command.args['action'].context() == 'SELECTION')):
        # Use 'selection' icon as the default for actions which operate on selection
        # in order to distingush these actions for the user.  This is not perfect, as
        # the distinction will disapear for actions which define their icons, but
        # icons are actually asigned quite rarely so it mostly works in practice.
        return 'selection'
    return None


def get_icon(icon_id, type=wx.ART_MENU, size=(16, 16)):
    """Get icon by id and return the corresponding 'wx.Bitmap' instance.

    Arguments:

      icon_id -- icon identifier.  It may be either one of 'wx.ART_*' constants
        resolved by 'wx.ArtProvider' (refers to icons in the current user
        interface theme) or a string identifier used to search for the icon
        file within 'pytis.config.icon_path' with either '.png' or '.svg' file
        name extension (added automatically, so the identifier is just the the
        base file name without extension).
      type -- only relevant for 'wx.ArtProvider' icons.
      size -- unused for PNG icons, selects the preferred size (if available)
        for 'wx.ArtProvider' icons and used to scale SVG icons to given target
        bitmap size.

    Returns None if an icon for given identifier is not found.

    """
    def find_file(icon_id):
        for directory in pytis.config.icon_path:
            for ext in ('.png', '.svg'):
                filename = os.path.join(directory, icon_id) + ext
                if os.path.exists(filename):
                    return filename
        return None

    if icon_id is None:
        bitmap = None
    elif (sys.version_info[0] == 2 and icon_id.startswith('wx') or
          sys.version_info[0] > 2 and isinstance(icon_id, bytes)):
        bitmap = wx.ArtProvider.GetBitmap(icon_id, type, size)
    else:
        filename = find_file(icon_id)
        if not filename:
            log(OPERATIONAL, "Could not find icon file {}.(svg|png):".format(icon_id),
                pytis.config.icon_path)
            return None
        if filename.endswith('.png'):
            img = wx.Image(filename, type=wx.BITMAP_TYPE_PNG)
        else:
            import cairosvg
            with open(filename) as f:
                data = f.read()
            png = cairosvg.svg2png(data, parent_width=size[0], parent_height=size[1])
            img = wx.Image(io.BytesIO(png), wx.BITMAP_TYPE_PNG
                           ).Scale(size[0], size[1], wx.IMAGE_QUALITY_HIGH)
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
              command=None, callback=None, enabled=True, update=False, tooltip=None,
              size=None, width=None, height=None):
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
    if noborder:
        style |= wx.BORDER_NONE
    if size is None:
        size = wx.DefaultSize
    kwargs = dict(size=size, style=style)
    if bitmap:
        if label:
            # wx Widgets don't have a standard button with both text and icon.  There is
            # wx.lib.buttons.ThemedGenBitmapTextButton, but is does not work in many ways
            # (does not indicate keyboerd focus, does not set the default button, ...)
            # So we rather draw the icon and the label into a new bitmap and use it
            # with BitmapButton.
            hmargin, vmargin, gap = 14, 6, 4
            label_size = parent.GetTextExtent(label)
            total_width = hmargin + bitmap.Size.width + gap + label_size.width + hmargin
            total_height = 2 * vmargin + max(bitmap.Size.height, label_size.height)
            bitmap_with_label = wx.Bitmap.FromRGBA(total_width, total_height)
            dc = wx.MemoryDC(bitmap_with_label)
            dc.SetBackground(wx.Brush(wx.TRANSPARENT_BRUSH))
            dc.Clear()
            dc.DrawBitmap(bitmap, hmargin, (total_height - bitmap.Size.height) // 2, True)
            dc.SetFont(parent.GetFont())
            dc.SetTextForeground(wx.Colour('white' if darkmode else 'black'))
            dc.DrawText(label, hmargin + bitmap.Size.width + gap, (total_height - label_size.height) // 2)
            del dc
            bitmap = bitmap_with_label
        button = wx.BitmapButton(parent, id, bitmap, **kwargs)
    else:
        button = wx.Button(parent, id, label=label or '', **kwargs)
    if command:
        assert callback is None
        button._pytis_in_button_callback = False
        def callback(e, button=button):
            if button._pytis_in_button_callback:
                # The handler may get invoked recursively and break things,
                # e.g. prevent a PopupEditForm from closing.
                # The recursive invocation may happen at least in wx 2.9 on OS X:
                # Button press or form key action to close a form invokes this
                # callback, form closing generates a close event and that event
                # (for unknown reasons) generates this button event again.
                e.Skip()
            else:
                button._pytis_in_button_callback = True
                try:
                    if command.invoke():
                        if wx.VERSION >= (2, 9):
                            # I don't know whether it behaves well in 2.8,
                            # so let's be on the safe side.
                            e.Skip()
                finally:
                    button._pytis_in_button_callback = False
        if tooltip:
            hotkey = pytis.form.app.keymap.lookup_command(command)
            if hotkey:
                tooltip += ' (' + hotkey_string(hotkey) + ')'
        # TODO: This causes the whole application to freeze when a dialog is closed.
        # if update:
        #     wx_callback(wx.EVT_UPDATE_UI, button, lambda e: e.Enable(command.enabled))
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


def wx_toolbar(parent, items):
    """Return wx.ToolBar instance created from given specification of items.

    Arguments:
      parent -- parent wx.Window instance
      items -- sequence of toolbar items as 'UICommand' instances or such
        sequences (representing groups to be delimited by a separator).

    """
    if isinstance(parent, wx.Frame):
        toolbar = parent.CreateToolBar(wx.NO_BORDER | wx.TB_DOCKABLE)
    else:
        toolbar = wx.ToolBar(parent)
    if items and not isinstance(items[0], (tuple, list)):
        items = (items,)
    for i, group in enumerate(items):
        if i != 0:
            toolbar.AddSeparator()
        for item in group:
            command = item.command()
            icon = command_icon(command)
            ctrl = item.ctrl()
            if isinstance(ctrl, tuple):
                ctrl, kwargs = (ctrl[0], copy.copy(ctrl[1]))
            elif ctrl:
                kwargs = {}
            else:
                assert icon is not None, command
                ctrl, kwargs = wx_button, dict(icon=icon, noborder=True)
            size = tuple(x or 32 for x in kwargs.pop('size', (None, None)))
            tool = toolbar.AddControl(ctrl(toolbar, command=command, size=size, **kwargs))
            toolbar.SetToolShortHelp(tool.GetId(), item.title())
            toolbar.SetToolLongHelp(tool.GetId(), item.descr())  # Doesn't seem to have effect...
            wx_callback(wx.EVT_UPDATE_UI, toolbar.Parent,
                        lambda e, command=command: e.Enable(command.enabled), source=tool)
    toolbar.Realize()
    return toolbar
