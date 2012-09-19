# -*- coding: utf-8 -*-

# Copyright (C) 2001-2012 Brailcom, o.p.s.
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

import collections
import copy
import string
import gtk, gtk.gdk
import webkit
import lcg

from pytis.form import *
import wx, wx.combo
import pytis.presentation

import config
if config.http_proxy is not None:
    # Nasty way to set the proxy used by the webkit browser.  This should be
    # much more elegant with newer pywebkitgtk versions as they will have a new
    # method set_proxy(), but this version was not released yet..
    import ctypes
    libgobject = ctypes.CDLL('/usr/lib/libgobject-2.0.so.0')
    libsoup = ctypes.CDLL('/usr/lib/libsoup-2.4.so.1')
    libwebkit = ctypes.CDLL('/usr/lib/libwebkit-1.0.so.2')
    proxy_uri = libsoup.soup_uri_new(config.http_proxy)
    session = libwebkit.webkit_get_default_session()
    libgobject.g_object_set(session, "proxy-uri", proxy_uri, None)


_WX_COLORS = {}
_WX_COLOR_DB = {}

def init_colors():
    global _WX_COLORS, _WX_COLOR_DB
    from pytis.presentation import Color
    colors = [getattr(Color, name) for name in public_attributes(Color)]
    _WX_COLORS = dict([(c, WxColor(*c)) for c in colors])
    from wx.lib import colourdb
    colourdb.updateColourDB()
    for name in colourdb.getColourList():
        wxcolour = wx.TheColourDatabase.FindColour(name)
        _WX_COLOR_DB[name] = WxColor(wxcolour.Red(), wxcolour.Green(), wxcolour.Blue())
    
### Pomocné funkce

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
    return window and \
           (isinstance(window, Dialog) or isinstance(window, PopupForm))

def copy_to_clipboard(text):
    """Copy given text into system clipboard.

    Even though this function itself doesn't handle windows clipboard, the text
    will be automatically propagated if pytis.windows is available thanks to
    the 'Application._on_clipboard_copy' callback.

    """
    assert isinstance(text, basestring)
    log(EVENT, 'Copy text to system clipboard.')
    # Using the wx clipboard doesn't work depending on how the application is
    # run.  It works when run natively on linux and also with newer versions of
    # nx, but it doesn't work when pytis is used on Windows through an X
    # server or on Cygwin.
    #clipboard = wx.TheClipboard
    #if clipboard.Open():
    #    clipboard.SetData(wx.TextDataObject(text))
    #    clipboard.Flush()
    #    clipboard.Close()
    # The following solution is is quite a hack, but it works consistently...
    ctrl = wx.TextCtrl(wx_frame(), -1, text)
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
    if pytis.windows.windows_available():
        nx_ip  = pytis.windows.nx_ip()
        log(EVENT, 'Paste text from windows clipboard on %s' % (nx_ip,))
        text = pytis.windows.get_clipboard_text()
    else:
        log(EVENT, 'Paste from clipboard')
        if not wx.TheClipboard.IsOpened():  # may crash, otherwise
            do = wx.TextDataObject()
            wx.TheClipboard.Open()
            success = wx.TheClipboard.GetData(do)
            wx.TheClipboard.Close()
            if success:
                text = do.GetText()
            else:
                text = None
    if text:
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
    return ' '.join([k.replace(' ', _(u"Mezerník")) for k in hotkey])


### Pomocné třídy


class Restorable:
    """Prvek s obnovitelným stavem.

    Potomci této třídy se považují za objekty schopné popsat svůj stav a
    později tento stav dle onoho popisu obnovit.  K získání popisu stavu,
    resp. obnovení stavu, slouží metoda 'save()', resp. 'restore()'.

    Tuto třídu by měly dědit všechny přímé statické děti aplikačního okna.
    
    """
    def save(self):
        """Vrať informaci o aktuálním stavu prvku pro jeho pozdější obnovení.

        Vrácená hodnota může být cokoliv, co je schopna zpracovat metoda
        'restore()'.

        V této třídě metoda vrací 'None'.
        
        """
        return None
        
    def restore(self, state):
        """Obnov stav prvku na základě 'state'.

        Argumenty:

          state -- informace o stavu vrácená dříve metodou 'save()'

        V této třídě metoda nedělá nic.
        
        """
        pass


class Window(wx.Panel, Restorable):
    """Vyměnitelné okno.

    Tato třída by měla podporovat všechny akce, které jsou nutné k umístění
    okna do aplikace a poskytovat pro tyto akce jednotné rozhraní.

    """

    _focused_window = None

    def __init__(self, parent):
        """Inicializuj instanci.

        Argumenty:

          parent -- rodičovské okno.  Instance 'wx.Window'.
        
        """
        assert isinstance(parent, wx.Window), parent
        wx.Panel.__init__(self, parent, wx.NewId())
        self._parent = parent
        self._hide_form_requested = False
        
    def _exit_check(self):
        "Proveď kontrolu před uzavřením a vrať pravdu, je-li možno pokračovat."
        return True

    def _cleanup(self):
        "Proveď úklidové akce před uzavřením okna."
        pass
    
    def close(self, force=False):
        return self._close(force=force)

    def _close(self, force=False):
        """Definitivně uzavři okno a zruš veškerý jeho obsah."""
        if force or self._exit_check():
            self.hide()
            try:
                self._cleanup()
            finally:
                self.Close()
                self.Destroy()
            return True
        else:
            return False
    
    def parent(self):
        """Vrať rodičovské okno zadané v konstruktoru."""
        return self._parent

    def resize(self, size=None):
        """Nastav velikost okna na velikost danou jako tuple (x, y).

        Pokud není velikost udána, je okno automaticky nastaveno na velikost
        svého rodičovského okna.

        """
        if size is None:
            size = self._parent.GetClientSize()
        self.SetSize(size)
        
    def show(self):
        """Zobraz (vykresli) toto okno a učiň jej aktivním."""
        self.Enable(True)
        self.Show(True)
        self.focus()

    def hide(self):
        """Učiň toto okno neaktivním a skryj jej."""
        orig_hide_form_requested = self._hide_form_requested
        self._hide_form_requested = True
        try:
            self.defocus()
            self.Enable(False)
            self.Show(False) # nutné i před uzavřením
        finally:
            self._hide_form_requested = orig_hide_form_requested
            
    def focus(self):
        """Nastav focus tomuto oknu."""
        if Window._focused_window:
            Window._focused_window.defocus()
        Window._focused_window = self
        self.SetFocus()
        
    def defocus(self):
        """Zruš focus tomuto oknu."""
        if Window._focused_window is self:
            Window._focused_window = None

            
def focused_window():
    """Vrať zaostřené okno nebo 'None', není-li jaké."""
    return Window._focused_window


class WxKey:
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
        table = (('Insert',    wx.WXK_INSERT),
                 ('Delete',    wx.WXK_DELETE),
                 ('Backspace', wx.WXK_BACK),
                 ('Home',      wx.WXK_HOME),
                 ('End',       wx.WXK_END),
                 ('Prior',     wx.WXK_PRIOR),
                 ('Next',      wx.WXK_NEXT),
                 ('Up',        wx.WXK_UP),
                 ('Down',      wx.WXK_DOWN),
                 ('Left',      wx.WXK_LEFT),
                 ('Right',     wx.WXK_RIGHT),
                 ('Enter',     wx.WXK_NUMPAD_ENTER),
                 ('Enter',     wx.WXK_RETURN),
                 ('Escape',    wx.WXK_ESCAPE),
                 ('Tab',       wx.WXK_TAB),
                 ('F1',  wx.WXK_F1),
                 ('F2',  wx.WXK_F2),
                 ('F3',  wx.WXK_F3),
                 ('F4',  wx.WXK_F4),
                 ('F5',  wx.WXK_F5),
                 ('F6',  wx.WXK_F6),
                 ('F7',  wx.WXK_F7),
                 ('F8',  wx.WXK_F8),
                 ('F9',  wx.WXK_F9),
                 ('F10', wx.WXK_F10),
                 ('F11', wx.WXK_F11),
                 ('F12', wx.WXK_F12),
                 )
        if self._TRANS_TABLE is None:
            self.__class__._TRANS_TABLE = dict(table)
            self.__class__._RTRANS_TABLE = dict([(v,k) for k,v in table])
        self._cache = {}
        
    def _key2wx(self, key):
        try:
            return self._cache[key]
        except KeyError:
            pass
        if string.find(key, 'Alt-') == 0:
            modifier = self._M_ALT
            skey = key[len('Alt-'):]
        elif string.find(key, 'Ctrl-') == 0:
            modifier = self._M_CTRL
            skey = key[len('Ctrl-'):]
        else:
            modifier = None
            skey = key
        try:
            code = self._TRANS_TABLE[skey]
        except KeyError:
            code = ord(string.upper(skey))
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
                except:
                    key = '???'
        return prefix + key
    

class WxColor(wx.Colour):
    """Stejné jako předek, avšak definuje rozumné porovnání."""
    
    def __cmp__(self, other):
        """Vrať shodu, právě když 'self' a 'other' mají shodné RGB složky."""
        try:
            result = cmp(self.Red(), other.Red()) or \
                     cmp(self.Green(), other.Green()) or \
                     cmp(self.Blue(), other.Blue())
        except AttributeError:
            # Je-li `other' barvou, může být ve wxWindows ledacos, proto nelze
            # zařadit nějaký rozumný test na třídu instance.
            result = compare_objects(self, other)
        return result


def color2wx(color):
    """Vrať barvu ve formě akceptované wxWindows.

    Pokud odpovídající barva není známa, vrať 'None'.
    
    Argumenty:

      color -- požadovaná barva, jedna z konstant třídy
        'pytis.presentation.Color' nebo název barvy z databáze barev
        (instance wxTheColourDatabase)

    """
    return _WX_COLORS.get(color, None) or _WX_COLOR_DB.get(color, None) or wx.NamedColor(color)

### Univerzální handlery


class Keymap:
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

    def _define_key(self, key, command, args):
        prefix, rest = key[0], key[1:]
        try:
            keydef = self._keymap[prefix]
        except KeyError:
            keydef = []
        if rest and not isinstance(keydef, Keymap):
            if keydef:
                log(OPERATIONAL, "Key '%s' is already used as non-prefix key."%\
                    prefix)
                return
            keydef = Keymap(None)
        self._keymap[prefix] = keydef
        if type(keydef) == type([]):
            keydef[0:0] = [(command, args)]
        elif rest:
            keydef._define_key(rest, command, args)
        else:
            log(OPERATIONAL, "Key '%s' is already used as a prefix key." % \
                prefix)

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
        """Vrať seznam všech platných kláves, jako tuple řetězců."""
        return self._keymap.keys()
    
    def __str__(self):
        return '<Keymap: %s>' % str(self._keymap)
    

class KeyHandler:
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
        except Exception as e:
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
        for attrname in public_attributes(self.__class__):
            if starts_with(attrname, 'COMMAND_'):
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
                if __debug__: log(DEBUG, 'Žádný další poručník')
                return False
            else:
                if __debug__: log(DEBUG, 'Předání poručníkovi:', guardian)
                return guardian._maybe_invoke_command(key_commands)

    def _get_keymap(self):
        if self.keymap is None:
            guardian = self._key_guardian
            if guardian is None:
                gkeymap = global_keymap()
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
        if __debug__: log(DEBUG, 'Stisk klávesy:', event)
        wk = self._wx_key
        if not wk.is_true_key(event):
            return
        message(None)
        if __debug__: log(DEBUG, 'Událost zpracovává:', str(self))
        guardian = self._key_guardian
        if self._commands is None:
            self._init_commands()
        if self._current_keymap is None or \
           not isinstance(last_user_event(), wx.KeyEvent):
            self._current_keymap = self._get_keymap()
        if __debug__:
            log(DEBUG, 'Aktuální klávesová mapa:', str(self._current_keymap))
        key = wk.event_key(event)
        keydef = self._current_keymap.lookup_key(key)
        if isinstance(keydef, Keymap):
            if __debug__: log(DEBUG, 'Prefixová klávesa', keydef)
            self._prefix_key_sequence.append(key)
            message('Prefixová klávesa: %s (%s)' % \
                    (' '.join(self._prefix_key_sequence),
                     ', '.join(keydef.keys())))
            self._current_keymap = keydef
            return True
        else:
            # Pozor, wxWindows je debilní a ne vždy předává události rodičům!
            self._current_keymap = None
            self._prefix_key_sequence = []
            if keydef is not None:
                result = self._maybe_invoke_command(keydef)
                if result:
                    return result
            if guardian:
                if __debug__: log(DEBUG, 'Klávesa předána výše')
                return guardian.on_key_down(event, dont_skip)
            if dont_skip:
                if __debug__: log(DEBUG, 'Klávesa ignorována')
            else:
                if __debug__: log(DEBUG, 'Klávesová událost přeskočena')
                event.Skip()
        return False


class CallbackHandler:
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
        assert kind[:5] == 'CALL_' and hasattr(self, kind), ('Invalid callback kind', kind)
        assert function is None or isinstance(function, collections.Callable), ('Invalid callback function', function)
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
            


#=============================#
# Specializované prvky        #
#=============================#


### Menu


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
        assert is_sequence(items)
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
        if event.GetMenuId() == -1:
            msg = ""
        else:
            msg = menu.FindItemById(event.GetMenuId()).GetHelp()
        message(msg, log_=False)
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
                    real_args = dict([(k,v) for k,v in args.items()
                                      if k!= '_command_handler'])
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
                if self._allow_autoindex and config.auto_menu_accel:
                    prefix = acceskey_prefix(i)
                    wx_title = prefix + title
                    title = prefix[1:] + title
                i += 1
                width = parent.GetTextExtent(title)[0] + 20
                if isinstance(item, MItem):
                    wxitem = item.create(parent, menu)
                    wxitem.SetText(wx_title)
                    menu.AppendItem(wxitem)
                    # Toto je zde zejména kvůli nakešování datových specifikací
                    # pro výpočet 'Command.enabled()' při startu aplikace.  Položky
                    # jsou správně aktivovány i bez toho, ale první zobrazení menu
                    # je pomalejší.
                    menu.Enable(wxitem.GetId(), item.command().enabled(**item.args()))
                    if __debug__:
                        if config.debug:
                            log(DEBUG, 'Menu item:', (item.title(),
                                                      item.command().enabled(**item.args()),))
                    if isinstance(item, (RadioItem, CheckItem)):
                        wxitem.Check(item.state())
                    if item in hotkey_str:
                        hotkey_items.append((item, wxitem, wx_title, width))
                    max_label_width = max(width + max_hotkey_width, max_label_width)
                elif isinstance(item, Menu):
                    menu.AppendMenu(wx.NewId(), wx_title, item.create(parent, keymap))
                    max_label_width = max(width+20, max_label_width)
                else:
                    raise ProgramError('Invalid menu item type', item)
        # Append hotkey description string to the item labels.
        # Fill with spaces to justify hotkeys on the right edge.
        space_width = parent.GetTextExtent(' ')[0]
        for i, wxitem, wx_title, width in hotkey_items:
            fill_width = max_label_width - width - max_hotkey_width
            n = round(float(fill_width) / float(space_width))
            fill = "%%%ds" % n % ''
            wxitem.SetText(wx_title + fill + hotkey_str[i]) 
        return menu

    def wx_menu(self):
        """Return the most recently created 'wxMenu' instance or None if create() was not called."""
        return self._wx_menu


class MItem(_TitledMenuObject):
    """Specifikace položky menu.

    Třída nic nevytváří, pouze si pamatuje parametry a předává je třídě Menu,
    která provádí tvorbu menu.
    
    """
    _WX_KIND = wx.ITEM_NORMAL
    _used_titles = {}
    
    def __init__(self, title, command, args=None, help=None, hotkey=None,
                 icon=None):
        """Uschovej parametry.

        Argumenty:

          title -- titulek menu, neprázdný řetězec
          
          command -- instance třídy 'Command' odpovídající příkazu, který má
            být při aktivaci této položky menu vyvolán.  Zde může být předána
            také dvojice (COMMAND, ARGS).  V tom případě je instance příkazu
            prvním prvkem této dvojice a druhý prvek nahrazuje argument 'args',
            který tímto již nesmí být předán.  Nakonec může být tímto
            argumentem string, který je pak identifikátorem specifikace ze
            specifikačního modulu 'app_commands'; tato specifikace je funkcí
            vracející kýženou dvojici (COMMANDS, ARGS).
            
          args -- dictionary argumentů příkazu 'command'.
          
          help -- řetězec obsahující jednořádkovou nápovědu, zobrazovaný ve
            stavovém řádku při průchodu přes položku; může být prázdný
            
          hotkey -- horká klávesa, která má být s daným příkazem a argumenty
            spojena, string nebo sekvence stringů dle specifikace v modulu
            'command'
            
          icon -- explicitně definovaná ikona položky menu.  Jedná se o
            identifikátor ikony použitelný jako argument funkce 'get_icon'.
            Pokud není určena, bude automaticky použita ikona podle typu
            příkazu (je-li pro příkaz definována).
            
        Je-li uveden argument 'hotkey' a nejsou předávány žádné 'args', je
        'command' automaticky nastavena tato klávesa.
          
        'title' a 'help' jsou vždy považovány za jazykově závislé texty
        a tudíž automaticky podléhají jazykové konverzi.

        """
        if is_sequence(command):
            command_spec = command[0]
        else:
            command_spec = command            
        if isinstance(command, basestring):
            command = resolver().get('app_commands', command)
        if is_sequence(command):
            assert len(command) == 2
            assert args is None
            command, args = command
        assert isinstance(command, Command), (command, command_spec,)
        assert args is None or isinstance(args, types.DictType)
        assert help is None or isinstance(help, basestring)
        assert hotkey is None or isinstance(hotkey, (basestring,
                                                     types.TupleType,
                                                     types.ListType))
        assert icon is None or isinstance(icon, basestring)
        self._command = command
        self._args = args or {}
        self._help = help
        self._hotkey = xtuple(hotkey)
        self._icon = icon
        self._action_id = self._make_action_id(command_spec)
        super(MItem, self).__init__(title)

    def _on_ui_event(self, event):
        event.Enable(self._command.enabled(**self._args))

    def _make_action_id(self, command_spec):
        def modulify(obj, name):
            module = obj.__module__
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
                extra.append('binding=%s' % (args['binding'],)); del args['binding']
            if not args:
                class_name = modulify(form_class, form_class.__name__)
                return ('form/%s/%s/%s/%s' % (class_name, form_name, string.join(extra, '&'), command_proc,))
        elif command == 'NEW_RECORD' and args:
            form_name = args.pop('name', '')
            if form_name is not None and not args:
                return ('%s/%s/%s' % (command, form_name, command_proc,))
        elif command == 'HANDLED_ACTION':
            handler = args.pop('handler', None)
            if not args and type(handler) == types.FunctionType:
                name = modulify(handler, handler.func_name)
                return ('handle/%s/%s' % (name, command_proc,))
        elif command == 'RUN_PROCEDURE':
            proc_name = args.pop('proc_name')
            spec_name = args.pop('spec_name')
            if (not args or
                (len(args) == 1 and
                 'enabled' in args and
                 not isinstance(args['enabled'], collections.Callable))):
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
                import sys
                sys.stderr.write("Can't find object named `%s'\n" % (symbol,))
                return None
        if components[-1]:
            return components[-1]
        elif kind == 'form':
            command = pytis.form.Application.COMMAND_RUN_FORM
            class_name, form_name = components[1], components[2]
            arguments = dict(form_class=find_symbol(class_name), name=form_name)
            if components[3]:
                for extra in string.split(components[3], '&'):
                    if extra[:len('binding=')] == 'binding=':
                        arguments['binding'] = extra[len('binding='):]
                        break
        elif kind == 'handle':
            command = pytis.form.Application.COMMAND_HANDLED_ACTION
            function_name = components[1]
            arguments = dict(handler=find_symbol(function_name),
                             enabled=lambda: action_has_access(action))
        elif kind == 'proc':
            command = pytis.form.Application.COMMAND_RUN_PROCEDURE
            proc_name, spec_name = components[1], components[2]
            arguments = dict(proc_name=proc_name, spec_name=spec_name,
                             enabled=lambda: action_has_access(action))
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
        
    def create(self, parent, parent_menu):
        item = wx.MenuItem(parent_menu, -1, self._title, self._help or "", kind=self._WX_KIND)
        wx_callback(wx.EVT_MENU, parent, item.GetId(), lambda e: self._command.invoke(**self._args))
        wx_callback(wx.EVT_UPDATE_UI, parent, item.GetId(), self._on_ui_event)
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
        assert isinstance(state, collections.Callable)
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
    #_WX_KIND = wx.ITEM_RADIO
    pass


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
                    log(OPERATIONAL, _(u"Duplicitní klávesa položky menu:"),
                        (k, menu.title(), cmd))
                    log(OPERATIONAL, _(u"Kolidující příkaz:"), self._keys[k])
                else:
                    self._keys[k] = cmd
                    
### Status bar


class StatusBar(wx.StatusBar):
    """Třída realizující stavový řádek.

    Stavový řádek slouží pro zobrazování krátkých zpráv uživateli a zobrazování
    stavových informací.  Může být rozdělen do více polí, z nichž každé slouží
    k zobrazení samostatné informace.  Tato třída rozšiřuje třídu
    'wxStatusBar' z wxWindows, měly by však být volány pouze její vlastní
    metody a ne metody poděděné z třídy 'wxStatusBar'.

    Stavový řádek je definován svým rozdělením na pole, z nichž každé má své
    jméno a šířku.  V těchto polích lze zobrazovat zprávy metodou 'message()'.
    
    Příklad použití:
    
       frame = wx.Frame(None, title='Pokus')

       sb = StatusBar(frame, (('main', -1), ('a', 80), ('b', 20)))
       sb.message('message', 'ahoj')
       sb.message('a', 'nazdar')

    """
    class _Timer(wx.Timer):
        def __init__(self, statusbar, field_number):
            self._statusbar = statusbar
            self._field_number = field_number
            wx.Timer.__init__(self, None, field_number)

        def Notify(self):
            self._statusbar.SetStatusText('', self._field_number)
            self.Stop()

        def start(self, timeout):
            if self.IsRunning():
                self.Stop()
            self.Start(1000 * timeout, True)
    
    def __init__(self, parent, fields):
        """Inicializuj StatusBar a vytvoř pojmenovaná pole.

        Argumenty:
        
          parent -- rodičovské okno
          fields -- sekvence specifikací polí

        Každá položka sekvence 'fields' je dvojice (ID, WIDTH), kde ID je jméno
        pole jako neprázdný string a WIDTH je jeho šířka v dialogových
        jednotkách.  Je-li WIDTH rovno None, pole je roztaženo na maximální
        možnou šířku po odečtení šířek ostatních polí; WIDTH smí být None pouze
        pro jediné pole.  ID pole jednoznačně identifikuje a k poli se jeho
        prostřednictvím přistupuje v metodách této třídy.  Žádná dvě pole nesmí
        mít stejné ID.
        
        """
        super(StatusBar, self).__init__(parent, -1)
        parent.SetStatusBar(self)
        self.SetFieldsCount(len(fields))
        self._field_numbers = {}
        self._timer = {}
        self._widths = widths = []
        for i, field in enumerate(fields):
            id, width = field
            self._field_numbers[id] = i
            # Each field has its own timer...
            self._timer[id] = self._Timer(self, i)
            if width is None:
                width = -1
            elif width > 0:
                width = dlg2px(self, width*4)
            widths.append(width)
        if widths and widths[-1] > 0:
            # Wx hack: Extend the last field to fit also the dragging triangle.
            widths[-1] += 22
        self._orig_widths = widths[:]
        self.SetStatusWidths(widths)

    def _field_number(self, id):
        """Vrať pořadové číslo pole 'id' pro wxWindows.

        Argumenty:
        
           id -- id pole jako string

        Pokud pole pojmenované 'id' neexistuje, vyvolej výjimku 'KeyError'.

        """
        return self._field_numbers[id]
    
    def message(self, id, message, timeout=None):
        """Nastav text pole 'id' na 'message'.

        Argumenty:
        
          id -- id pole jako string
          message -- string, který má být novým obsahem pole; může být
            i 'None', v kterémžto případě bude předchozí hlášení smazáno
          timeout -- pokud je zadáno, zpráva zmizí po zadaném počtu sekund
          
        Pokud stavová řádka dané pole neobsahuje, vrať nepravdu, jinak vracej
        pravdu.
           
        """
        if message is None:
            message = ''
        else:
            message = unicode(message)
        try:
            i = self._field_number(id)
        except KeyError:
            return False
        # Prevent status bar blinking by checking against the current value.
        if message != self.GetStatusText(i):
            # Adjust the field width to fit the new text (for fixed width fields only).  The
            # "fixed" fields don't change their width as a percentage of the application frame
            # width, but are not completely fixed...
            if self._widths[i] > 0:
                # Add 6 pixels below for the borders.
                width = max(self.GetTextExtent(message)[0] + 6, self._orig_widths[i])
                if width != self._widths[i]:
                    self._widths[i] = width
                    self.SetStatusWidths(self._widths)
            self.SetStatusText(message, i)
        if timeout is not None:
            self._timer[id].start(timeout)
        return True

    def get_message(self, id):
        """Vrať text pole 'id' jako string.

        Argumenty:
        
           id -- identifikátor pole jako řetězec

        Pokud pole daného 'id' neexistuje, je vrácena hodnota None.

        """
        try:
            return self.GetStatusText(self._field_number(id))
        except KeyError:
            return None


class InfoWindow(object):
    """Nemodální okno pro zobrazení textových informací."""
    
    def __init__(self, title, text, format=TextFormat.PLAIN, parent=None,
                 _name='info', **kwargs):
        """Display information window in a standalone frame.
        
        Arguments:
        
          title -- Frame title as a basestring.
          parent -- parent wx Frame or None to use the main application frame
          text, format, **kwargs -- passed to 'wx_text_view()' ('text' as 'content').

        """
        frame = wx.Dialog(parent or wx_frame(), title=title, name=_name,
                          style=wx.DEFAULT_DIALOG_STYLE|wx.RESIZE_BORDER)
        # Temporarily use a modal dialog instead an ordinary frame to work
        # around the problem of closing a frame whose parent is a modal dialog
        # in StructuredTextField._cmd_preview().  Once that is sorted out, a
        # non-modal frame would be better.
        view = wx_text_view(frame, text, format, **kwargs)
        frame.SetSize(view.GetSize())
        frame.ShowModal()
        

class ProfileSelectorPopup(wx.ListCtrl, wx.combo.ComboPopup):
    """Profile selection menu implemented using wx.ListCtrl.

    This class implements the 'wx.combo.ComboPopup' API and thus can be used as
    a popup selection of the 'ProfileSelector' control, which is derived form
    'wx.combo.ComboCtrl'.

    """
    def __init__(self):
        self.PostCreate(wx.PreListCtrl())
        wx.combo.ComboPopup.__init__(self)
        self._selected_profile_index = None

    def _on_motion(self, event):
        item, flags = self.HitTest(event.GetPosition())
        if item >= 0:
            profile_index = self.GetItemData(item)
            if profile_index != -1:
                self.Select(item)
                self._selected_profile_index = profile_index

    def _on_left_down(self, event):
        self.Dismiss()
        if self._selected_profile_index is not None:
            LookupForm.COMMAND_APPLY_PROFILE.invoke(index=self._selected_profile_index)

    # The following methods implement the ComboPopup API.

    def Create(self, parent):
        # Create the popup child control. Return True for success.
        wx.ListCtrl.Create(self, parent,
                           style=wx.LC_REPORT|wx.LC_SINGLE_SEL|wx.SIMPLE_BORDER|wx.LC_NO_HEADER)
        self.InsertColumn(0, 'profile')
        self.Bind(wx.EVT_LEFT_DOWN, self._on_left_down)
        self.Bind(wx.EVT_MOTION, self._on_motion)
        return True

    def GetControl(self):
        # Return the widget that is to be used for the popup.
        return self

    def GetAdjustedSize(self, minWidth, prefHeight, maxHeight):
        # Called just prior to displaying the popup.
        # Fill menu items before each popup and delete them on dismiss to
        # avoid having to update the menu during form profile list update.
        form = current_form()
        profiles = form.profiles()
        current = form.current_profile()
        first_user_profile = None
        for i, profile in enumerate(profiles):
            if profile.id().startswith(FormProfileManager.USER_PROFILE_PREFIX) and first_user_profile is None:
                first_user_profile = i
            title = profile.title()
            if profile.errors():
                title += ' '+ _(u"(neplatný)")
            self.InsertStringItem(i, title)
            self.SetItemData(i, i)
            if profile is current:
                self.Select(i)
        if first_user_profile:
            for i, label in ((0, _(u"Systémové profily")),
                             (first_user_profile + 1, _(u"Uživatelské profily"))):
                self.InsertStringItem(i, '- ' + label + ' -')
                self.SetItemBackgroundColour(i, wx.Colour(225, 225, 225))
                self.SetItemData(i, -1)
        self.SetColumnWidth(0, minWidth)
        self.SetSize((1,1)) # Needed for GetViewRect to work consistently. 
        width, height = self.GetViewRect()[2:] # Returned sizes are 16 px greater than the reality.
        return wx.Size(max(width-16, minWidth), min(height-16, maxHeight))

    def SetStringValue(self, value):
        # Called just prior to displaying the popup, but after GetAdjustedSize.
        # As it is more practical to select the current item there, we don't
        # need to do anything here.
        pass

    def GetStringValue(self):
        # Return a string representation of the current item.
        selected = self.GetFirstSelected()
        if selected != -1:
            return self.GetItemText(selected)
        else:
            return ''

    def OnPopup(self):
        # Called immediately after the popup is shown.
        wx.combo.ComboPopup.OnPopup(self)

    def OnDismiss(self):
        # Called when popup is dismissed.
        wx.combo.ComboPopup.OnDismiss(self)
        self.Select(wx.NOT_FOUND)
        self.DeleteAllItems()


class ProfileSelector(wx.combo.ComboCtrl):
    """Toolbar control for form profile selection and management."""
    
    def __init__(self, parent, uicmd, size):
        wx.combo.ComboCtrl.__init__(self, parent, style=wx.TE_PROCESS_ENTER, size=size)
        self._popup = ProfileSelectorPopup()
        self.SetPopupControl(self._popup)
        self._on_enter_perform = None
        ctrl = self.GetTextCtrl()
        ctrl.SetEditable(False)
        wx_callback(wx.EVT_UPDATE_UI, self, self.GetId(), self._on_ui_event)
        wx_callback(wx.EVT_RIGHT_DOWN, self, self._on_context_menu)
        wx_callback(wx.EVT_RIGHT_DOWN, ctrl, self._on_context_menu)
        wx_callback(wx.EVT_TEXT_ENTER, ctrl, ctrl.GetId(), self._on_enter)
        wx_callback(wx.EVT_KEY_DOWN, self, self._on_key_down)
        wx_callback(wx.EVT_KEY_DOWN, ctrl, self._on_key_down)
            
    def _on_ui_event(self, event):
        enabled = LookupForm.COMMAND_PROFILE_MENU.enabled()
        event.Enable(enabled)
        ctrl = self.GetTextCtrl()
        popup = self._popup
        if enabled:
            if not ctrl.IsEditable():
                form = current_form()
                current_profile = form.current_profile()
                if current_profile and ctrl.GetValue() != current_profile.title():
                    ctrl.SetValue(current_profile.title())
            if LookupForm.COMMAND_UPDATE_PROFILE.enabled():
                # Indicate changed profile by color (update is enabled for changed profiles).
                color = wx.Color(200, 0, 0)
            else:
                color = wx.Color(0, 0, 0)
            ctrl.SetForegroundColour(color)
        elif top_window() is None and ctrl.GetValue() != '':
            ctrl.SetValue('')

    def _on_context_menu(self, event):
        menu = (
            MItem(_(u"Uložit"),
                  LookupForm.COMMAND_UPDATE_PROFILE(),
                  help=_(u"Aktualizovat uložený profil podle současného nastavení formuláře")),
            MItem(_(u"Uložit jako nový"),
                  Application.COMMAND_HANDLED_ACTION(
                    # Name must be edited first and 'cmd' will be invoked after confirmation.
                    handler=self._edit_profile_title,
                    enabled=self._edit_profile_title_enabled,
                    cmd=LookupForm.COMMAND_SAVE_NEW_PROFILE,
                    clear=True),
                  help=_(u"Vytvořit nový profil podle současného nastavení formuláře")),
            MItem(_(u"Přejmenovat"),
                  Application.COMMAND_HANDLED_ACTION(
                    # Name must be edited first and 'cmd' will be invoked after confirmation.
                    handler=self._edit_profile_title,
                    enabled=self._edit_profile_title_enabled,
                    cmd=LookupForm.COMMAND_RENAME_PROFILE),
                  help=_(u"Upravit a uložit název aktuálního profilu")),
            MItem(_(u"Smazat"), 
                  LookupForm.COMMAND_DELETE_PROFILE(),
                  help=_(u"Smazat zvolený uložený profil")),
            MItem(_(u"Použít automaticky při otevření formuláře"), 
                  LookupForm.COMMAND_SET_INITIAL_PROFILE(),
                  help=_(u"Automaticky přepnout na současný profil při příštím otevření formuláře.")),
            MSeparator(),
            MItem(_(u"Vrátit poslední uložené nastavení"),
                  LookupForm.COMMAND_RELOAD_PROFILE,
                  help=_(u"Zahodit změny nastavení formuláře provedené "
                         u"od posledního uložení profilu.")),
            MItem(_(u"Vrátit výchozí nastavení aplikace"),
                  command=LookupForm.COMMAND_RESET_PROFILE,
                  help=_(u"Zahodit všechny uložené uživatelské změny nastavení formuláře.")),
            )
        popup_menu(self, menu)

    def _edit_profile_title(self, cmd, clear=False):
        ctrl = self.GetTextCtrl()
        def perform():
            title=self.GetValue()
            ctrl.SetEditable(False)
            cmd.invoke(title=title)
        ctrl.SetEditable(True)
        if clear:
            ctrl.SetValue('')
        else:
            ctrl.SelectAll()
        ctrl.SetFocus()
        message(_(u"Zadejte název profilu a potvrďte stiskem ENTER."))
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
            current_form().focus()
            

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
        wx.Choice.__init__(self, parent, choices=self._CHOICES, size=size)
        wx_callback(wx.EVT_UPDATE_UI, self, self.GetId(), self._on_ui_event)
        wx_callback(wx.EVT_CHOICE, self, self.GetId(), self._on_selection)
            
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

            
class DualFormSwitcher(wx.BitmapButton):
    """Special toolbar control for DualForm.COMMAND_OTHER_FORM.

    The only reason for implementing a custom toolbar control is the ability to
    switch the bitmap displayed on the button according to the current form to
    indicate whether the upper or the lower form in the dual form is active..

    """
    
    def __init__(self, parent, uicmd):
        self._toolbar = parent
        self._uicmd = uicmd
        self._bitmaps = (get_icon('dual-form-active-up', type=wx.ART_TOOLBAR),
                         get_icon('dual-form-active-down', type=wx.ART_TOOLBAR))
        self._current_bitmap = self._bitmaps[0]
        wx.BitmapButton.__init__(self, parent, -1, self._current_bitmap,
                                 style=wx.BU_EXACTFIT|wx.NO_BORDER)
        wx_callback(wx.EVT_BUTTON, self, self.GetId(), self._on_click)
        wx_callback(wx.EVT_UPDATE_UI, parent, self.GetId(), self._on_update_ui)

    def _on_click(self, event):
        cmd, kwargs = self._uicmd.command(), self._uicmd.args()
        cmd.invoke(**kwargs)
        
    def _on_update_ui(self, event):
        cmd, kwargs = self._uicmd.command(), self._uicmd.args()
        enabled = cmd.enabled(**kwargs)
        event.Enable(enabled)
        if enabled:
            dualform = current_form(inner=False)
            if dualform.active_form() == dualform.main_form():
                i = 0
            else:
                i = 1
            new_bitmap = self._bitmaps[i]
            if self._current_bitmap != new_bitmap:
                self._current_bitmap = new_bitmap
                self.SetBitmapLabel(new_bitmap)
                self._toolbar.Realize()
            

class LocationBar(wx.TextCtrl):
    """Toolbar control for browser location display and entry."""
    
    def __init__(self, parent, uicmd, editable=True, size=None):
        self._toolbar = parent
        self._uicmd = uicmd
        wx.TextCtrl.__init__(self, parent, -1, size=size, style=wx.TE_PROCESS_ENTER)
        self.SetEditable(editable)
        wx_callback(wx.EVT_UPDATE_UI, parent, self.GetId(), self._on_update_ui)
        if editable:
            wx_callback(wx.EVT_TEXT_ENTER, parent, self.GetId(), self._on_enter)
        else:
            self.SetOwnBackgroundColour(config.field_disabled_color)
            self.Refresh()
        browser = uicmd.args()['_command_handler']
        browser.set_uri_change_callback(lambda uri: self.SetValue(uri))

    def _on_update_ui(self, event):
        cmd, kwargs = self._uicmd.command(), self._uicmd.args()
        enabled = cmd.enabled(**kwargs)
        event.Enable(enabled)
                
    def _on_enter(self, event):
        cmd, kwargs = self._uicmd.command(), self._uicmd.args()
        uri = self.GetValue()
        cmd.invoke(uri=uri, **kwargs)


class HelpExporter(lcg.StyledHtmlExporter, lcg.HtmlExporter):
    _BODY_PARTS = ('menu',
                   'heading',
                   'content')

    def _uri_resource(self, context, resource):
        if resource.uri() is not None:
            return resource.uri()
        else:
            return 'resource:' +resource.filename()

    def _menu(self, context):
        # TODO: This is copied from Wiking! Move to LCG.
        g = self._generator
        current = context.node()
        while current.parent() is not None and current.hidden():
            current = current.parent()
        path = current.path()
        def is_foldable(node):
            return False
            if node.foldable():
                for item in node.children():
                    if not item.hidden():
                        return True
            return False
        def li_cls(node):
            if is_foldable(node):
                cls = 'foldable'
                if node not in path:
                    cls += ' folded'
                return cls
            return None
        def item(node):
            cls = []
            if node is current:
                cls.append('current')
            if not node.active():
                cls.append('inactive')
            # The inner span is necessary because MSIE doesn't fire on click events outside the A
            # tag, so we basically need to indent the link title inside and draw folding controls
            # in this space.  This is only needed for foldable trees, but we render also fixed
            # trees in the same manner for consistency.  The CSS class 'bullet' represents either
            # fixed tree items or leaves in foldable trees (where no further folding is possible).
            content = g.span(node.title(), cls=not is_foldable(node) and 'bullet' or None)
            return g.a(content, href='help:'+node.id(), title=node.descr(),
                       cls=' '.join(cls) or None)
        def menu(node, indent=0):
            spaces = ' ' * indent
            items = [lcg.concat(spaces, '  ',
                                g.li(lcg.concat(item(n),
                                                menu(n, indent+4)),
                                     cls=li_cls(n)),
                                '\n')
                     for n in node.children() if not n.hidden()]
            if items:
                return lcg.concat("\n", spaces,
                                g.ul(lcg.concat('\n', items, spaces)),
                                '\n', ' '*(indent-2))
            else:
                return ''
        return g.div((g.h(g.link(_("Navigace"), None, hotkey="3"), 3),
                      menu(context.node().root())),
                     cls='menu-panel')

        
class Browser(wx.Panel, CommandHandler):
    """Web Browser widget.
    
    The widget can be embedded into other wx widgets as an ordinary wx.Panel.
    The public methods may be used then to manipulate the browser content.

    Implemented using Webkit GTK (http://webkitgtk.org/reference/webkitgtk-webkitwebview.html).
    
    """
    def __init__(self, parent):
        wx.Panel.__init__(self, parent)
        self._webview = None
        self._async_queue = []
        self._resource_provider = None
        self._restricted_navigation_uri = None
        self._uri_change_callback = None
        self._loading_help = False
        wx_callback(wx.EVT_IDLE, self, self._on_idle)

    def _on_idle(self, event):
        if self._webview is None:
            # Show must be called first in order to be able to obtain the GtkPizza
            # widget below.  GtkPizza is a custom GTK widget implemented by
            # wxWidgets.
            self.GetParent().Show()
            handle = self.GetHandle()
            gtk_window = gtk.gdk.window_lookup(handle)
            # The GTK window lookup will return None when called *before* the
            # window is actually shown on the screen (because of some wx/GTK
            # magic).  In this case we must wait and try again in next idle
            # call.
            if gtk_window:
                # Reference to the GtkPizza widget must be kept to prevent a segfault.
                self._gtk_pizza = gtk_pizza = gtk_window.get_user_data()
                # GtkPizza's parant is a gtk.ScrolledWindow.
                gtk_scrolled_window = gtk_pizza.parent
                # Replace the GTK ScrolledWindow content by the webkit widget.
                gtk_scrolled_window.remove(gtk_pizza)
                self._webview = webview = webkit.WebView()
                gtk_scrolled_window.add(webview)
                gtk_scrolled_window.show_all()
                webview.connect('notify::load-status', self._on_load_status_changed)
                webview.connect('navigation-policy-decision-requested', self._on_navigation_request)
                webview.connect('resource-request-starting', self._on_resource_request)
                #webview.connect('load-error', self._on_load_error)
                settings = webview.get_settings()
                settings.props.user_agent += ' Pytis/%s (WebKit)' % pytis.__version__
                #settings.props.enable_developer_extras = True # Doesn't work...
                webview.set_settings(settings)
        if self._webview is not None:
            # Perform webview interaction asyncronously to avoid blocking the
            # main application.  This also allows the public methods load_uri
            # and load_html() to be called before the vebview is actually
            # created above.
            while self._async_queue:
                function = self._async_queue.pop(0)
                function()

    def _on_load_status_changed(self, webview, signal):
        status = webview.get_property('load-status')
        if status == webkit.LOAD_FINISHED:
            msg = _(u"Dokument byl načten.")
            busy = False
            if self._uri_change_callback:
                self._uri_change_callback(webview.get_property('uri'))
        elif status == webkit.LOAD_FAILED:
            msg = _(u"Načtení dokumentu se nezdařilo.")
            busy = False
        else:
            msg = _(u"Načítám dokument.")
            busy = True
        busy_cursor(busy)
        message(msg)

    def _on_load_error(self, webview, frame, uri, gerror):
        # For debugging only! (non portable hack)
        import ctypes
        ptr = int(str(gerror)[13:-1], 16)
        ptrtype = ctypes.POINTER(ctypes.c_char_p)
        msgptr = ctypes.cast(ptr + 8, ptrtype)
        message = msgptr[0]
        print "LOAD ERROR:", message
        return False
        
    def _on_navigation_request(self, webview, frame, req, action, decision):
        uri = req.get_uri()
        if uri == 'about:blank':
            # This URI gets loaded several times while other document is
            # loaded.  It is unknown what originates such request and how to
            # distinguish it form a real request to 'about:blank'.  But it
            # confuses the location bar and may also generate invalid
            # restriction messages so we beter ignore it now.
            return False
        restricted_navigation_uri = self._restricted_navigation_uri
        if not uri.startswith('resource:') and restricted_navigation_uri is not None and \
                not uri.startswith(restricted_navigation_uri):
            decision.ignore()
            message(_(u"Přechod na externí URL zamítnut: %s") % uri, beep_=True)
            return True
        elif uri.startswith('help:') and not self._loading_help:
            # We need to protect loading help by the flag self._loading_help
            # because the signal gets called recursively for unknown reason
            # when webview.load_string() is called.
            self._loading_help = True
            self._load_help_page(uri[5:])
            if action.get_reason() in (webkit.WEB_NAVIGATION_REASON_LINK_CLICKED,
                                       webkit.WEB_NAVIGATION_REASON_FORM_SUBMITTED,
                                       webkit.WEB_NAVIGATION_REASON_OTHER):
                history = self._webview.get_back_forward_list()
                history.add_item(webkit.WebHistoryItem(uri, uri))
            self._loading_help = False
            return True
        else:
            return False
        
    def _on_resource_request(self, webview, frame, resource, req, response):
        def redirect(resource):
            if resource and resource.src_file():
                # Redirect the request to load the resource file from filesystem.
                req.set_uri("file://" + resource.src_file())
        uri = resource.get_uri()
        # Note, when load_html() is performed, this method gets called with uri
        # equal to base_uri passed to load_html().
        if uri.startswith('resource:') and self._resource_provider is not None:
            # Try searching the existing resources by URI first.
            for resource in self._resource_provider.resources():
                if resource.uri() == uri:
                    return redirect(resource)
                if isinstance(resource, lcg.Image):
                    thumbnail = resource.thumbnail()
                    if thumbnail and thumbnail.uri() == uri:
                        return redirect(thumbnail)
            # If URI doesn't match any existing resource, try locating the
            # resource using the standard resource provider's algorithm
            # (including searching resource directories).
            return redirect(self._resource_provider.resource(uri[9:]))

    def _spec_items_descriptions(self, spec_name):
        data = pytis.data.dbtable('e_pytis_help_spec_items',
                                        ('spec_name', 'kind', 'identifier', 'content'),
                                        config.dbconnection)
        data.select(condition=pytis.data.EQ('spec_name', pytis.data.sval(spec_name)))
        descriptions = dict([(x, {}) for x in ('field', 'action', 'binding', 'profile')])
        while True:
            row = data.fetchone()
            if row is None:
                break
            if row['content'].value():
                descriptions[row['kind'].value()][row['identifier'].value()] = row['content'].value()
        data.close()
        return descriptions
        
    def _spec_help_content(self, spec_name):
        if not has_access(spec_name):
            return (_(u"Přístup odepřen"),
                    lcg.p(_(u"Nemáte přístup ke specifikaci „%s“.") % spec_name))
        resolver = pytis.util.resolver()
        def spec_link(spec_name, title=None):
            if title is None:
                try:
                    view_spec = resolver.get(spec_name, 'view_spec')
                except pytis.util.ResolverError as e:
                    title = spec_name
                else:
                    title = view_spec.title()
            return lcg.link('help:spec/%s' % spec_name, title)
        descriptions = self._spec_items_descriptions(spec_name)
        def description(kind, identifier, default):
            return descriptions[kind].get(identifier, default or _(u"Popis není k dispozici."))
        #content = [lcg.Section("Základní informace",
        #                       lcg.fieldset(self._info(), formatted=True))]
        try:
            view_spec = resolver.get(spec_name, 'view_spec')
        except pytis.util.ResolverError as e:
            return None, None
        data = pytis.data.dbtable('e_pytis_help_spec', ('spec_name', 'description', 'help'),
                                  config.dbconnection)
        row = data.row((pytis.data.Value(data.find_column('spec_name').type(), spec_name),))
        if row:
            spec_description = row['description'].value()
            spec_help = row['help'].value()
        else:
            spec_description = view_spec.description()
            spec_help = view_spec.help()
        parser = lcg.Parser()
        related_specnames = []
        for f in view_spec.fields():
            for name in [f.codebook()] + [link.name() for link in f.links()]:
                if name and name not in related_specnames:
                    related_specnames.append(name)
        sections = [
            lcg.Section(title=title, content=f(content))
            for title, f, content in (
                (_(u"Shrnutí"), lcg.p, spec_description),
                (_(u"Popis"), parser.parse, spec_help),
                (_(u"Políčka formuláře"), lcg.dl,
                 [(f.label(), description('field', f.id(), f.descr()))
                  for f in [view_spec.field(fid) for fid in view_spec.layout().order()]]),
                (_(u"Akce kontextového menu"), lcg.dl,
                 [(a.title(), description('action', a.id(), a.descr()))
                  for a in view_spec.actions(linear=True)]),
                (_(u"Vedlejší formuláře"), lcg.dl,
                 [(spec_link(b.name(), b.title()), description('binding', b.id(), b.descr()))
                  for b in view_spec.bindings() if b.name()]),
                (_(u"Související náhledy"), lcg.ul,
                 [spec_link(name) for name in related_specnames]),
                )
            if content]
        #data_spec = resolver.get(specname, 'data_spec')
        #rights = data_spec.access_rights()
        #if rights:
        #    content += "\n\n== %s ==\n\n" % _(u"Přístupová práva")
        #    for perm in (pd.Permission.VIEW,
        #                 pd.Permission.INSERT,
        #                 pd.Permission.UPDATE,
        #                 pd.Permission.DELETE,
        #                 pd.Permission.EXPORT):
        #        groups = [g for g in rights.permitted_groups(perm, None) if g]
        #        content += ":%s:: %s\n" % (perm, ', '.join(map(str, groups)) or _(u"Nedefinováno"))
        storage = pytis.presentation.DbAttachmentStorage('e_pytis_help_spec_attachments',
                                                         'spec_name', spec_name,
                                                         base_uri='resource:')
        return view_spec.title(), lcg.Container(sections, resources=storage.resources())
        
    def _load_help_page(self, topic):
        resource_dirs = [d[:-3]+'resources' for d in sys.path
                         if d.endswith('/pytis/lib') or d.endswith('/lcg/lib')]
        resource_provider = lcg.ResourceProvider(dirs=resource_dirs)
        def make_node(row, children):
            if row['help_id'].value() == topic:
                # If this is the currently displayed node, create the content.
                # Other nodes are only generated for their presence in the
                # menu.
                parser = lcg.Parser()
                if row['page_id'].value():
                    storage = pytis.presentation.DbAttachmentStorage('e_pytis_help_pages_attachments',
                                                                     'page_id', page_id,
                                                                     base_uri='resource:')
                    content = lcg.Container(parser.parse(row['content'].value()),
                                            resources=storage.resources())
                else:
                    content = [lcg.TableOfContents()]
                    if row['menu_help'].value():
                        content.extend(parser.parse(row['menu_help'].value()))
                    if row['spec_name'].value():
                        spec_help_content = self._spec_help_content(row['spec_name'].value())[1]
                        if spec_help_content:
                            content.append(spec_help_content)
            else:
                content = ()
            return lcg.ContentNode(row['help_id'].value(), title=row['title'].value(),
                                   descr=row['description'].value(),
                                   content=lcg.Container(content),
                                   resource_provider=resource_provider,
                                   children=[make_node(r, children) for r in
                                             children.get(row['position'].value(), ())])
        data = pytis.data.dbtable('ev_pytis_help', ('help_id', 'fullname', 'spec_name', 'page_id',
                                                    'position', 'title', 'description',
                                                    'menu_help', 'content'),
                                  config.dbconnection)
        data.select(sort=(('position', pytis.data.ASCENDENT),))
        children = {}
        while True:
            row = data.fetchone()
            if not row:
                break
            parent = '.'.join(row['position'].value().split('.')[:-1])
            children.setdefault(parent, []).append(row)
        data.close()
        nodes = [lcg.ContentNode('NotFound', title=_("Nenalezeno"), hidden=True,
                                 content=lcg.p(_(u"Požadovaná stránka nápovědy nenalezena: %s") % topic),
                                 resource_provider=resource_provider)]
        if topic.startswith('spec/'):
            # Separate specification descriptions are not in the menu generated
            # from ev_pytis_help.  If specification description page is
            # requested, we generate one and add it to the menu here.
            spec_name = topic[5:]
            title, content = self._spec_help_content(spec_name)
            if title and content:
                node = lcg.ContentNode(topic, title=title, hidden=True,
                                       content=lcg.Container((lcg.TableOfContents(), content)),
                                       resource_provider=resource_provider)
                nodes.append(node)
        root = lcg.ContentNode('help', content=lcg.Content(), hidden=True,
                               children=nodes + [make_node(r, children) for r in children['']])
        current_node = root.find_node(topic) or root.find_node('NotFound')
        exporter = HelpExporter(styles=('default.css', 'pytis-help.css'))
        self.load_content(current_node, exporter=exporter)
                
    def _can_go_forward(self):
        return self._webview.can_go_forward()
        
    def _cmd_go_forward(self):
        self._webview.go_forward()
        
    def _can_go_back(self):
        return self._webview.can_go_back()
        
    def _cmd_go_back(self):
        self._webview.go_back()
        
    def _can_stop_loading(self):
        status = self._webview.get_property('load-status')
        return status not in (webkit.LOAD_FINISHED, webkit.LOAD_FAILED)
        
    def _cmd_stop_loading(self):
        self._webview.stop_loading()
        
    def _cmd_reload(self):
        self._webview.reload()

    def _cmd_load_uri(self, uri):
        self._webview.load_uri(uri)

    def can_command(self, command, **kwargs):
        if self._webview is not None:
            return super(Browser, self).can_command(command, **kwargs)
        else:
            return False
        
    def toolbar(self, parent):
        toolbar = wx.ToolBar(parent)
        for uicmd in (UICommand(Browser.COMMAND_GO_BACK(_command_handler=self),
                                _(u"Zpět"),
                                _(u"Zobrazit předchozí položku historie prohlížení")),
                      UICommand(Browser.COMMAND_GO_FORWARD(_command_handler=self),
                                _(u"Vpřed"),
                                _(u"Zobrazit následující položku historie prohlížení")),
                      UICommand(Browser.COMMAND_RELOAD(_command_handler=self),
                                _(u"Obnovit"),
                                _(u"Načíst aktuální dokument znovu")),
                      UICommand(Browser.COMMAND_STOP_LOADING(_command_handler=self),
                                _(u"Zastavit"),
                                _(u"Zastavit načítání dokumentu")),
                      UICommand(Browser.COMMAND_LOAD_URI(_command_handler=self),
                                _(u"Adresa"),
                                _(u"Aktuální adresa prohlížeče"),
                                ctrl=(LocationBar, dict(size=(600, 25), editable=False))),
                      ):
            uicmd.create_toolbar_ctrl(toolbar)
        toolbar.Realize()
        return toolbar

    def set_uri_change_callback(self, callback):
        self._uri_change_callback = callback

    def load_uri(self, uri, restrict_navigation=None):
        def f():
            self._resource_provider = None
            self._restricted_navigation_uri = restrict_navigation
            self._webview.load_uri(uri)
        self._async_queue.append(f)

    def load_html(self, html, base_uri='', restrict_navigation=None, resource_provider=None):
        def f():
            self._resource_provider = resource_provider
            self._restricted_navigation_uri = restrict_navigation
            self._webview.load_html_string(html, base_uri)
        self._async_queue.append(f)

    def load_content(self, node, exporter=None):
        """Load browser content from lcg.ContentNode instance."""
        if exporter is None:
            class Exporter(lcg.StyledHtmlExporter, lcg.HtmlExporter):
                pass
            exporter = Exporter(styles=('default.css',), inlinestyles=True)
        context = exporter.context(node, None)
        html = exporter.export(context)
        self.load_html(html.encode('utf-8'), resource_provider=node.resource_provider())


class HelpBrowserFrame(wx.Frame):

    def __init__(self):
        wx.Frame.__init__(self, None)
        self._browser = browser = Browser(self)
        sizer = wx.BoxSizer(wx.VERTICAL)
        sizer.Add(browser.toolbar(self), proportion=0, flag=wx.EXPAND)
        sizer.Add(browser, proportion=1, flag=wx.EXPAND)
        self.SetSizer(sizer)
        self.SetSize((800,600))
        self.SendSizeEvent()

    def load_uri(self, uri):
        self._browser.load_uri(uri)


class IN(pytis.data.Operator):
    """Symbolic specification of pytis.data.IN operator.
    
    This class creates a pytis data operator instance based on symbolic
    arguments and translates these symbolic arguments into real arguments
    accepted by pytis.data.IN.  This makes it possible to display this operator
    in the user interface and save it within user profiles, because the data
    object and filter condition is defined by name rather than directly.

    See constructor arguments for details how they differ from pytis.data.IN
    arguments.

    """

    def __init__(self, column_id, spec_name, table_column_id, profile_id):
        """Arguments:

          column_id -- string identifier of the column which should belong to
            the set; existence of its value is checked in the other table; same
            as 'column_id' argument of 'pytis.data.IN'.
            
          spec_name -- string name of the specification defining the set; data
            object of this specification will be created and passed to
            'pytis.data.IN' as 'data'.
          
          table_column_id -- string identifier of the column in 'spec_name' used to
            search for the value of 'column_id'; same as 'table_column_id'
            argument of 'pytis.data.IN'.

          profile_id -- string identifier of an existing profile within
            'spec_name'.  It can be either one of profiles defined by the
            specification or a user defined profile saved through the profile
            manager by the current user.  The profile's filter determines the
            'condition' passed to 'pytis.data.IN'.  Can be also None if no
            condition shall be applied.

        """
        self._column_id = column_id
        self._spec_name = spec_name
        self._table_column_id = table_column_id
        self._profile_id = profile_id
        import config
        resolver = pytis.util.resolver()
        view_spec = resolver.get(spec_name, 'view_spec')
        data_factory = resolver.get(spec_name, 'data_spec')
        data_object = data_factory.create(connection_data=config.dbconnection)
        if profile_id is not None:
            if profile_id.startswith(FormProfileManager.USER_PROFILE_PREFIX):
                manager = profile_manager()
                condition, profile_name = manager.load_filter(spec_name, data_object, profile_id)
            else:
                profile = find(profile_id, view_spec.profiles(), lambda p: p.id())
                if not profile:
                    raise Exception("Profile %s of %s doesn't exist!" % (profile_id, spec_name))
                condition = profile.filter()
                profile_name = profile.title()
        else:
            profile_name = None
            condition = None
        self._profile_name = profile_name
        self._spec_title = view_spec.title()
        self._table_column_label = view_spec.field(table_column_id).label()
        pytis.data.Operator.__init__(self, 'IN', column_id, data_object, table_column_id, condition)

    def column_id(self):
        return self._column_id
    
    def spec_name(self):
        return self._spec_name

    def spec_title(self):
        return self._spec_title

    def table_column_id(self):
        return self._table_column_id
    
    def table_column_label(self):
        return self._table_column_label
    
    def profile_id(self):
        return self._profile_id
    
    def profile_name(self):
        return self._profile_name


# Převodní funkce
        
def char2px(window, x, y):
    """Přepočítej znakový rozměr na pixely a vrať instanci 'wx.Size'.

    Vstupní rozměr je chápán jako šířka a výška \"běžného\" znaku.

    Argumenty:
    
      window -- okno, podle jehož fontu má být rozměr vypočítán.
      x -- šířka; počet znaků
      y -- výška; počet znaků

    Vrací: Rozměry v pixelech jako instanci 'wx.Size'.
    
    """

    return dlg2px(window, 4*x, 8*y)

def dlg2px(window, x, y=None):
    """Přepočítej znakový rozměr na pixely.

    Vstupní rozměr je chápán jako šířka a výška \"běžného\" znaku.

    Tento přepočet by nás měl odstínit od závislosti na použitém výstupním
    zařízení / systémových nastaveních. Poskytuje možnost získat rozměry
    v pixelech odpovídající aktuálním rozměrům znaků v daném okně.
    
    Funkčnost je v této implementaci založená na tzv. \"dialog units\"
    z wxWindows, takže případnou další dokumentaci je nutno hledat tam.

    Argumenty:
    
      window -- okno, podle jehož fontu má být rozměr vypočítán.
      x -- šířka; jednotkou je 1/4 šířky znaku; integer
      y -- výška; jednotkou je 1/8 výšky znaku; integer

    Vrací: Rozměry v pixelech jako instanci wxSize, pokud byl zadán i y-ový
      rozměr.  Pokud je volán pouze s x-ovým rozměrem, vrátí rovnou integer.
    
    """
    if y == None:
        y = 0
        single = True
    else:
        single = False
    dlgsize = (x,y)
    pxsize = wx.DLG_SZE(window, dlgsize)
    if single:
        return pxsize.GetWidth()
    else:
        return pxsize

def acceskey_prefix(i):
    pad = {'f': '  ', 'i': '  ', 'j': '  ', 'l': '  ', 'm': '', 't': '  ', 'r': '  ', 'w': ''}
    if i < 26:
        index = chr(i+97)
    else:
        index = str(i-25)
    return '&'+ index +'. '+ pad.get(index, ' ')


def orientation2wx(orientation):
    """Převeď konstantu třídy 'Orientation' na wx reprezentaci."""
    if orientation == spec.Orientation.VERTICAL:
        return wx.VERTICAL
    elif orientation == spec.Orientation.HORIZONTAL:
        return wx.HORIZONTAL
    else:
        raise ProgramError("Neplatná hodnota Orientation:", orientation)

def border_style2wx(style):
    """Převeď konstantu třídy 'BorderStyle' na wx reprezentaci."""
    mapping = {
        spec.BorderStyle.ALL: wx.ALL,
        spec.BorderStyle.TOP: wx.TOP,
        spec.BorderStyle.BOTTOM: wx.BOTTOM,
        spec.BorderStyle.LEFT: wx.LEFT,
        spec.BorderStyle.RIGHT: wx.RIGHT,
        }        
    try:
        return mapping[style]
    except KeyError:
        raise ProgramError("Neplatná hodnota BorderStyle:", orientation)

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

def open_data_as_file(data, suffix):
    remote_file = None
    if pytis.windows.windows_available():
        try:
            remote_file = pytis.windows.make_temporary_file(suffix=suffix)
        except:
            pass
    if remote_file:
        nxip = pytis.windows.nx_ip()
        log(OPERATIONAL, "Launching file on Windows at %s:" % nxip, remote_file.name())
        try:
            remote_file.write(data)
        finally:
            remote_file.close()
        pytis.windows.launch_file(remote_file.name())
    else:
        import mailcap, mimetypes
        path = os.tempnam() + suffix
        mime_type = mimetypes.guess_type(path)[0]
        if mime_type:
            match = mailcap.findmatch(mailcap.getcaps(), mime_type)[1]
            if match:
                command = match['view'] % path
                log(OPERATIONAL, "Running external file viewer:", command)
                try:
                    f = open(path, 'wb')
                    try:
                        f.write(data)
                    finally:
                        f.close()
                        os.system(command)
                finally:
                    os.remove(path)
                return
        run_dialog(Error, _("Nenalezen odpovídající prohlížeč pro '%s'." % suffix))


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
    parent.PopupMenu(menu, position)
    menu.Destroy()


def get_icon(icon_id, type=wx.ART_MENU, size=(16,16)):
    """Get icon by id and return the corresponding 'wx.Bitmap' instance.

    Arguments:

      icon_id -- string icon identifier.  It may be either one of 'wx.ART_*'
        constants or a filename.  The icons returned for wx constants should
        honour the pre-defined symbol in the current user interface theme.
        Identifiers not corresponding to wx constants are used as icon file
        names within the 'config.icon_dir'.  The '.png' suffix is added
        automatically, so the identifier consists just of a base name.
      type, size -- only relevant when icon_id as a 'wx.ART_*' constant.

    If an icon for given identifier is not found, returns None.

    """
    if icon_id is None:
        bitmap = None
    elif icon_id.startswith('wx'):
        bitmap = wx.ArtProvider_GetBitmap(icon_id, type, size)
    else:
        imgfile = os.path.join(config.icon_dir, icon_id + '.png')
        if os.path.exists(imgfile):
            img = wx.Image(imgfile, type=wx.BITMAP_TYPE_PNG)
            bitmap = wx.BitmapFromImage(img)
        else:
            log(OPERATIONAL, "Could not find icon file:", imgfile)
            bitmap = None
    if bitmap and bitmap.Ok():
        return bitmap
    else:
        return None

    
def wx_focused_window():
    """Vrať aktuálně zaostřené wx okno, jako instanci 'wx.Window'."""
    return wx.Window_FindFocus()

    
def _init_wx_ctrl(ctrl, tooltip=None, update=False, enabled=True, width=None, height=None):
    if tooltip:
        ctrl.SetToolTipString(tooltip)
    if update:
        wx_callback(wx.EVT_UPDATE_UI, parent, ctrl.GetId(), update)
    if not enabled:
        ctrl.Enable(False)
    if width:
        ctrl.SetSize((width, ctrl.GetSize().height))
        ctrl.SetMinSize((width, ctrl.GetSize().height))
    if height:
        ctrl.SetSize((ctrl.GetSize().width, height))
        ctrl.SetMinSize((ctrl.GetSize().width, height))


def wx_button(parent, label=None, icon=None, bitmap=None, id=-1, noborder=False, fullsize=False,
              command=None, callback=None, enabled=True, update=False, tooltip=None, size=None,
              width=None, height=None):
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
        style |= wx.NO_BORDER
    if bitmap:
        button = wx.BitmapButton(parent, id, bitmap, size=size, style=style)
    else:
        button = wx.Button(parent, id, label=label or '', size=size, style=style)
    if command:
        assert callback is None
        cmd, args = command
        callback = lambda e: cmd.invoke(**args)
        if tooltip:
            hotkey = global_keymap().lookup_command(cmd, args)
            if hotkey:
                tooltip += ' ('+ hotkey_string(hotkey) +')'
        # TODO: This causes the whole application to freeze when a dialog is closed.
        #if update:
        #    wx_callback(wx.EVT_UPDATE_UI, parent, button.GetId(),
        #                lambda e: e.Enable(cmd.enabled(**args)))
    if callback:
        wx_callback(wx.EVT_BUTTON, button, button.GetId(), callback)
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
        wx_callback(evt, ctrl, ctrl.GetId(), on_change)
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
        wx_callback(wx.EVT_TEXT, ctrl, ctrl.GetId(), on_text)
    wx_callback(wx.EVT_TEXT_PASTE, ctrl, lambda e: paste_from_clipboard(ctrl))
    if value is not None:
        ctrl.SetValue(value)
    if length:
        assert width is None
        width = dlg2px(ctrl, 4*length)
        if _spin:
            width += 20 # Add some space for spin buttons...
    _init_wx_ctrl(ctrl, tooltip=tooltip, enabled=enabled, width=width, height=height)
    return ctrl


def wx_spin_ctrl(parent, value=None, **kwargs):
    return wx_text_ctrl(parent, value=value, _spin=True, **kwargs)


def wx_checkbox(parent, label=None, tooltip=None, checked=False):
    checkbox = wx.CheckBox(parent, -1, label=label)
    if tooltip is not None:
        checkbox.SetToolTipString(unicode(tooltip))
    checkbox.SetValue(checked)
    return checkbox


def wx_text_view(parent, content, format=TextFormat.PLAIN, width=None, height=None, resources=()):
    """Return a wx widget displaying given text content.

    Arguments:
    
      content -- text content to be displayed.  The text is treated according
        to 'format' (see below).
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
    assert isinstance(content, basestring)
    assert format == TextFormat.LCG or not resources
    if format == TextFormat.PLAIN:
        lines = content.splitlines()
        if width is None:
            width = min(max([len(l) for l in lines]), 80)
        if height is None:
            height = min(len(lines), 20)
        style = wx.TE_MULTILINE | wx.TE_DONTWRAP | wx.TE_READONLY
        ctrl = wx.TextCtrl(parent, style=style)
        ctrl.SetValue(content)
        size = char2px(ctrl, width, height)
        # Slightly enlarge the size to avoid scrollbars when not necessary (for small sizes).
        ctrl.SetBestFittingSize((size[0]+30, size[1]+2))
        return ctrl
    else:
        browser = Browser(parent)
        if format == TextFormat.LCG:
            node = parse_lcg_text(content, resources=resources)
            browser.load_content(node)
        elif format == TextFormat.HTML:
            if isinstance(content, unicode):
                content = content.encode('utf-8')
            if '<html' not in content[:100].lower():
                cotnent = ('<html>'
                           '<head>'
                           '<meta content="text/html; charset=UTF-8" http-equiv="content-type">'
                           '</head>'
                           '<body>'
                           + content +
                           '</body>'
                           '</html>')
            browser.load_html(content)
        else:
            raise ProgramError("Unknown text format: %s" % format)
        # We can' adjust the default size according to the content size, but since
        # webkit 1.3.8, there should be a new method webview.get_viewport_attributes(),
        # which will allow it.
        if width is None:
            width = 90
        if height is None:
            height = 30
        browser.SetSize(char2px(parent, width, height))
    return browser

