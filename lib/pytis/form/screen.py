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

"""Specifikace a zpracování uživatelských prvků hlavní obrazovky.

Modul zavádí prvky společné pro všechny formuláře: menu, stavový řádek,
nápovědu, atd.  Tyto prvky se mohou vyskytovat ve všech formulářích, avšak
v každém mohou mít jiný obsah.

Modul definuje jak třídy sloužící ke specifikaci konkrétních prvků, tak
i třídy, které tyto specifikace následně zpracovávají.

"""

import copy


from pytis.form import *
import wx
import pytis.presentation

_WX_COLORS = {}
_WX_COLORS_DATABASE = {}

def init_colors():
    global _WX_COLORS, _WX_COLORS_DATABASE
    _WX_COLORS = {
        pytis.presentation.Color.WHITE:  WxColor(255, 255, 255),
        pytis.presentation.Color.BLACK:  WxColor(0, 0, 0),
        pytis.presentation.Color.RED:    WxColor(255, 0, 0),
        pytis.presentation.Color.RED20:  WxColor(255, 200, 200),
        pytis.presentation.Color.GREEN:  WxColor(0, 255, 0),
        pytis.presentation.Color.BLUE:   WxColor(0, 0, 255),
        pytis.presentation.Color.YELLOW: WxColor(255, 255, 160),
        pytis.presentation.Color.GRAY:   WxColor(50, 50, 50),
        pytis.presentation.Color.GRAY90: WxColor(25, 25, 25),
        pytis.presentation.Color.GRAY80: WxColor(50, 50, 50),
        pytis.presentation.Color.GRAY70: WxColor(75, 75, 75),
        pytis.presentation.Color.GRAY60: WxColor(100, 100, 100),
        pytis.presentation.Color.GRAY50: WxColor(125, 125, 125),
        pytis.presentation.Color.GRAY40: WxColor(150, 150, 150),
        pytis.presentation.Color.GRAY30: WxColor(175, 175, 175),
        pytis.presentation.Color.GRAY20: WxColor(200, 200, 200),
        pytis.presentation.Color.GRAY10: WxColor(225, 225, 225),
        pytis.presentation.Color.BLANCHETALMOND: WxColor(255, 235, 205),
        pytis.presentation.Color.LIGHTYELLOW:    WxColor(255, 255, 224),
        pytis.presentation.Color.PEACHPUFF2:     WxColor(238, 203, 173),
        pytis.presentation.Color.SLATEGRAY2:     WxColor(185, 211, 238),
        pytis.presentation.Color.LIGHTSALMON:    WxColor(255, 160, 122)
                  }
    _WX_COLORS_DATABASE = color_dictionary()
    
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

def set_clipboard_text(text):
    assert is_anystring(text)
    clipboard = wx.TheClipboard
    if clipboard.Open():
        clipboard.UsePrimarySelection(True)
        clipboard.SetData(wx.PyTextDataObject(text));
        clipboard.Close()
    #import stdwin
    #stdwin.setcutbuffer(0, text)

def color_dictionary():
    from wx.lib import colourdb
    colourdb.updateColourDB()
    clrdict = {}
    for clrname in colourdb.getColourList():
        wxcolour = wx.TheColourDatabase.FindColour(clrname)
        clrdict[clrname] = WxColor(wxcolour.Red(),
                                   wxcolour.Green(),
                                   wxcolour.Blue())
    return clrdict    
    


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


class Window(wx.ScrolledWindow, Restorable):
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
        wx.ScrolledWindow.__init__(self, parent, wx.NewId())
        self._parent = parent
        
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
        self.defocus()
        self.Show(False) # nutné i před uzavřením
        self.Enable(False)

    def close(self):
        """Definitivně uzavři okno a zruš veškerý jeho obsah."""
        self.hide()
        self.Close()
        self.Destroy()               
                        
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
    def _make_dictionary(self, items):
        dict = {}
        for i in items:
            dict[i[0]] = i[1]
        return dict
    _RTRANS_TABLE = None

    def __init__(self):
        if self._TRANS_TABLE is None:
            # Musí to být až tady, kvůli (ne)importům wx na serveru
            self.__class__._TRANS_TABLE = {'Insert'   : wx.WXK_INSERT,
                                           'Delete'   : wx.WXK_DELETE,
                                           'Backspace': wx.WXK_BACK,
                                           'Home'     : wx.WXK_HOME,
                                           'End'      : wx.WXK_END,
                                           'Prior'    : wx.WXK_PRIOR,
                                           'Next'     : wx.WXK_NEXT,
                                           'Up'       : wx.WXK_UP,
                                           'Down'     : wx.WXK_DOWN,
                                           'Left'     : wx.WXK_LEFT,
                                           'Right'    : wx.WXK_RIGHT,
                                           'Enter'    : wx.WXK_RETURN,
                                           'Escape'   : wx.WXK_ESCAPE,
                                           'Tab'      : wx.WXK_TAB,
                                           'F1':  wx.WXK_F1,
                                           'F2':  wx.WXK_F2,
                                           'F3':  wx.WXK_F3,
                                           'F4':  wx.WXK_F4,
                                           'F5':  wx.WXK_F5,
                                           'F6':  wx.WXK_F6,
                                           'F7':  wx.WXK_F7,
                                           'F8':  wx.WXK_F8,
                                           'F9':  wx.WXK_F9,
                                           'F10': wx.WXK_F10,
                                           'F11': wx.WXK_F11,
                                           'F12': wx.WXK_F12,
                                           }
            self.__class__._RTRANS_TABLE = \
              self._make_dictionary(map(lambda x: (x[1], x[0]),
                                        self._TRANS_TABLE.items()))
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
        code = event.GetKeyCode()
        try:
            body = self._RTRANS_TABLE[code]
            if event.ShiftDown():
                prefix = prefix + 'Shift-'
        except KeyError:
            if code == 13 or code == 372: # RETURN or KeyPad RETURN
                body = 'Enter'
            else:
                if code >= 1 and code <= 26:
                    body = chr(code + 96)
                else:
                    try:
                        body = chr(code).lower()
                    except:
                        body = '???'
                if event.ShiftDown():
                    body = body.upper()
        return prefix + body
    

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
    return _WX_COLORS.get(color, None) or _WX_COLORS_DATABASE.get(color, None)

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
            keymap = parent._keymap.copy()
        self._keymap = keymap

    def _define_key(self, key, command, args):
        prefix, rest = key[0], key[1:]
        try:
            keydef = self._keymap[prefix]
        except KeyError:
            keydef = []
        if rest and not isinstance(keydef, Keymap):
            keydef = Keymap(None)
        self._keymap[prefix] = keydef
        if type(keydef) == type([]):
            keydef[0:0] = [(command, args)]
        elif rest:
            keydef._define_key(rest, command, args)
        else:
            raise ProgramError("Key is already defined as a prefix key:",
                               prefix, str(command))

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

        - Klávesa s modifikátorem Shift je zapsána ve formátu 'Shift-<KEY>',
          pokud <KEY> není znak anglické abecedy -- ten je zapsán jako
          odpovídající velké písmeno.
          
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
        self._temporary_keymap = []
        if not hasattr(self, 'keymap'):
            self.keymap = None
        self._current_keymap = self.keymap
        try:
            key_guardian = self.guardian()
            while not isinstance(key_guardian, KeyHandler):
                key_guardian = key_guardian.guardian()
        except Exception, e:
            key_guardian = None
        self._key_guardian = key_guardian

    def _handle_keys(self, *widgets):
        """Registruj se pro ošetření klávesových událostí daných UI prvků."""
        for widget in widgets:
            wx_callback(wx.EVT_KEY_DOWN, widget, self.on_key_down)
        
    def _init_commands(self):
        # Nemůžeme `_commands' inicializovat hned v konstruktoru, protože
        # tou dobou ještě nemusí být všechny příkazy ve třídě definovány.
        keymap = self._get_keymap()
        commands = []
        for attrname in public_attributes(self.__class__):
            if starts_with(attrname, 'COMMAND_'):
                command = getattr(self.__class__, attrname)
                if isinstance(command, Command):
                    commands.append(command)
        # Do atributu přiřazujeme až nyní, aby to bylo odolnější vláknům
        for key, command, args in self._temporary_keymap:
            self.keymap.define_key(key, command, args)
            if command not in commands:
                commands.append(command)
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
        if self._commands is None:
            # `_init_commands()' was not yet called.
            self._temporary_keymap.append((key, command, args))
        else:
            self.keymap.define_key(key, command, args)
            if command not in self._commands:
                self._commands.append(command)
        
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
        assert kind[:5] == 'CALL_' and hasattr(self, kind), \
               ('Invalid callback kind', kind)
        assert function is None or callable(function), \
               ('Callback function not callable', function)
        if __debug__:
            log(DEBUG, 'Nastaven callback řádkového seznamu:', (kind, function))
        self._callbacks[kind] = function


    def get_callback(self, kind):
        if self._callbacks.has_key(kind):
            return self._callbacks[kind]
        return None

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
            if __debug__: log(DEBUG, 'Bude volán callback:', (kind, callback))
            apply(callback, args, kwargs)
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
        assert isinstance(title, types.StringTypes)
        self._title = gettext_(title)
        
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
    def __init__(self, title, items):
        """Uschovej specifikaci menu.

        Argumenty:

          title -- název menu, string
          items -- položky menu, sekvence instancí tříd 'Menu' a 'MItem';

        """
        assert is_sequence(items)
        if __debug__:
            for i in items:
                assert isinstance(i, _MenuObject)
        self._items = tuple(items)
        super(Menu, self).__init__(title)

    def items(self):
        """Vrať sekvenci položek menu zadanou v konstruktoru."""
        return self._items
        
    def create(self, parent, keyhandler):
        """Vytvoř menu dle specifikace a vrať instanci 'wx.Menu'.

        Tato metoda zkonstruuje menu včetně všech vnořených podmenu, přičemž
        zabezpečí veškeré navázání událostí apod.
        
        Argumenty:

          parent -- wx rodič vytvářené instance 'wx.Menu'
          keyhandler -- instance 'KeyHandler', jejíž klávesová mapa bude
            synchronizována s příkazy a horkými klávesami položek menu.
        
        """
        menu = wx.Menu()
        # At first, compute the maximal width of hotkey string in this menu.
        max_hotkey_width = 0
        hotkey_string = {}
        for i in self._items:
            if isinstance(i, MItem):
                hotkey, command, args = i.hotkey(), i.command(), i.args()
                if hotkey == (None,) and keyhandler.keymap is not None:
                    hotkey = keyhandler.keymap.lookup_command(command, args)
                    hotkey = xtuple(hotkey)
                elif keyhandler.keymap is not None:
                    keyhandler.keymap.define_key(hotkey, command, args)
                if hotkey != (None,):
                    s = hotkey_string[i] = '    ' + ' '.join(hotkey) 
                    hotkey_width = parent.GetTextExtent(s)[0]
                    max_hotkey_width = max(hotkey_width, max_hotkey_width)
        # Now create the items and remember max. width of whole item label
        hotkey_items = []
        max_label_width = 0
        for i in self._items:
            if isinstance(i, MItem):
                item = i.create(parent, menu)
                menu.AppendItem(item)
                # Toto je zde zejména kvůli nakešování datových specifikací
                # pro výpočet 'Command.enabled()' při startu aplikace.  Položky
                # jsou správně aktivovány i bez toho, ale první zobrazení menu
                # je pomalejší.
                menu.Enable(item.GetId(), i.command().enabled(**i.args()))
                if isinstance(i, (RadioItem, CheckItem)):
                    item.Check(i.state())
                width = parent.GetTextExtent(i.title())[0]
                if hotkey_string.has_key(i):
                    hotkey_items.append((i, item, width))
                    width = width + max_hotkey_width
                max_label_width = max(width, max_label_width)
            elif isinstance(i, MSeparator):
                menu.AppendSeparator()
            elif isinstance(i, Menu):
                menu.AppendMenu(wx.NewId(), i.title(raw=True),
                                i.create(parent, keyhandler))
                width = parent.GetTextExtent(i.title())[0] + 20
                max_label_width = max(width, max_label_width)
            else:
                raise ProgramError('Invalid menu item type', i)
        # Append hotkey description string to the item labels.
        # Fill with spaces to justify hotkeys on the right edge.
        space_width = parent.GetTextExtent(' ')[0]
        for i, item, width in hotkey_items:
            fill_width = max_label_width - width - max_hotkey_width
            n = round(float(fill_width) / float(space_width))
            fill = "%%%ds" % n % ''
            item.SetText(i.title(raw=True) + fill + hotkey_string[i]) 
        return menu


class MItem(_TitledMenuObject):
    """Specifikace položky menu.

    Třída nic nevytváří, pouze si pamatuje parametry a předává je třídě Menu,
    která provádí tvorbu menu.
    
    """
    _WX_KIND = wx.ITEM_NORMAL
    _used_titles = {}
    
    def __init__(self, title, command, args=None, help='', hotkey=None):
        """Uschovej parametry.

        Argumenty:

          title -- titulek menu, neprázdný řetězec
          command -- instance třídy 'Command' odpovídající příkazu, který má
            být při aktivaci této položky menu vyvolán.  Zde může být předána
            také dvojice (COMMAND, ARGS).  V tom případě je instance příkazu
            prvním prvkem této dvojice a druhý prvek nahrazuje argument
            `args', který tímto již nesmí být předán.
          args -- dictionary argumentů příkazu 'command'.
          help -- řetězec obsahující jednořádkovou nápovědu, zobrazovaný
            ve stavovém řádku při průchodu přes položku; může být prázdný
          hotkey -- horká klávesa, která má být s daným příkazem a argumenty
            spojena, string nebo sekvence stringů dle specifikace v modulu 'command'
            
        Je-li uveden argument 'hotkey' a nejsou předávány žádné 'args', je
        'command' automaticky nastavena tato klávesa.
          
        'title' a 'help' jsou vždy považovány za jazykově závislé texty
        a tudíž automaticky podléhají jazykové konverzi.

        """
        if is_sequence(command):
            assert len(command) == 2
            assert args is None
            command, args = command
        assert isinstance(command, Command)
        assert args is None or isinstance(args, types.DictType)
        assert help is None or isinstance(help, types.StringTypes)
        assert hotkey is None or isinstance(hotkey, (types.StringTypes,
                                                     types.TupleType,
                                                     types.ListType))
        self._command = command
        self._args = args or {}
        self._help = gettext_(help)
        self._hotkey = xtuple(hotkey)
        super(MItem, self).__init__(title)

    def _on_ui_event(self, event):
        event.Enable(self._command.enabled(**self._args))
        
    def create(self, parent, parent_menu):
        item = wx.MenuItem(parent_menu, -1, self._title, self._help,
                           kind=self._WX_KIND)
        wx_callback(wx.EVT_MENU, parent, item.GetId(),
                    lambda e: self._command.invoke(**self._args))
        wx_callback(wx.EVT_UPDATE_UI, parent, item.GetId(), self._on_ui_event)
        return item
        
    def set_hotkey(self, hotkey):
        """Nastav dodatečně klávesovou zkratku položky menu."""
        assert hotkey is None or isinstance(hotkey, (types.StringTypes,
                                                     types.TupleType,
                                                     types.ListType))
        self._hotkey = xtuple(hotkey)
    
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

    def state(self):
        return self._state()
        
    def _on_ui_event(self, event):
        event.Check(self.state())
        super(CheckItem, self)._on_ui_event(event)

        
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
    
    def __init__(self, parent, menus, keyhandler):
        """Vytvoř menubar na základě sekvence 'menus' a vlož do 'parent'.

        Argumenty:
        
          parent -- instance třídy 'wxFrame', do které má být menubar vložen
          menus -- sekvence instancí třídy 'Menu' definující jednotlivá
            menu v menu baru; menu se v menu baru vytvoří ve stejném pořadí,
            v jakém jsou v této sekvenci uvedena
          keyhandler -- instance 'KeyHandler', jejíž klávesová mapa bude
            synchronizována s příkazy a horkými klávesami položek menu.
            
        """
        wx.MenuBar.__init__(self)
        self._parent = parent
        if __debug__:
            self._keys = {}
            for m in menus:
                self._check_duplicate_keys(m)
        for menu in menus:
            self.Append(menu.create(self._parent, keyhandler),
                        menu.title(raw=True))
        parent.SetMenuBar(self)        
        
    def _check_duplicate_keys(self, menu):
        if isinstance(menu, Menu):
            for m in menu.items():
                self._check_duplicate_keys(m)
        elif isinstance(menu, MItem):
            k = xtuple(menu.hotkey())
            if k != (None,):
                cmd = (menu.command(), menu.args())
                if self._keys.has_key(k) and self._keys[k] != cmd:
                    log(OPERATIONAL, _("Duplicitní klávesa položky menu:"),
                        (k, menu.title(), cmd))
                    log(OPERATIONAL, _("Kolidující příkaz:"), self._keys[k])
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
        widths = []
        for i in range(len(fields)):
            id, width = fields[i]
            self._field_numbers[id] = i
            # each field has its own timer...
            self._timer[id] = self._Timer(self, i)
            widths.append(width is None and -1 or dlg2px(self, width*4))
        if len(widths) and widths[-1] != -1:
            widths[-1] += 22
        self.SetStatusWidths(widths)
        c = wx.SYS_COLOUR_HIGHLIGHT

        

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
            field = self._field_number(id)
        except KeyError:
            return False
        if message != self.GetStatusText(field):
            # Změnu provádíme jen při dané podmínce, aby nám status bar
            # zbytečně neblikal.
            self.SetStatusText(message, field)
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
    
    def __init__(self, title, text, format=TextFormat.PLAIN, _name='info'):
        """Zobraz nemodální okno nezávislé na hlavním okně aplikace.
        
        Argumenty:
        
          title -- titulek okna jako řetězec.
          text -- text, který bude zobrazen v okně.  Způsob zpravování je určen
            argumentem 'format'.
          format -- vstupní formát textu, jako konstanta 'TextFormat'.  V
            případě prostého textu ('TextFormat.PLAIN') zůstane řádkování i
            veškeré další formátování nedotčeno (je ponecháno na volající
            straně).  V případě formátu 'TextFormat.HTML' je vstupní text
            považován přímo za text s HTML zančkováním.  Text však není sám o
            sobě platným HTML dokumentem.  Neobsahuje hlavičku, ani značky
            <html> a <body>.  Jde jen o zformátovaný text, který bude vsazen do
            těla automaticky vytvořeného dokumentu.

        """
        assert isinstance(title, types.StringTypes)
        assert isinstance(text, types.StringTypes)
        assert format in public_attributes(TextFormat)
        frame = wx.Frame(wx_frame(), title=title, name=_name)
        view = wx_text_view(frame, text, format)
        frame.Show(True)
        
        
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


def wx_focused_window():
    """Vrať aktuálně zaostřené wx okno, jako instanci 'wx.Window'."""
    return wx.Window_FindFocus()


def wx_text_view(parent, content, format=TextFormat.PLAIN):
    import wx
    if format in (TextFormat.PLAIN, TextFormat.WIKI):
        lines = content.splitlines()
        width = min(max([len(l) for l in lines]), 80)
        height = min(len(lines), 20)
        size = (width, height)
    else:
        size = None
    if format == TextFormat.PLAIN:
        style = wx.TE_MULTILINE | wx.TE_DONTWRAP | wx.TE_READONLY
        w = wx.TextCtrl(parent, style=style)
        w.SetBestFittingSize(char2px(w, *size))
        w.SetValue(content)
    else:
        if format == TextFormat.WIKI:
            import lcg
            n = lcg.WikiNode('x', content, title='')
            html = n.content().export()
        else:
            html = content
        import wx.html
        size = size and char2px(parent, *[1.3 * x for x in size]) or (400, 300)
        w = wx.html.HtmlWindow(parent, size=size)
        #w.SetFonts('', '', sizes=(8,9,10,11,12,13,14))
        w.SetPage('<html><head><title></title></head>' + \
                  '<body><font size="-2">' + html + '</font></body></html>')
    return w

