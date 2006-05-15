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

"""Specifikace a zpracov�n� u�ivatelsk�ch prvk� hlavn� obrazovky.

Modul zav�d� prvky spole�n� pro v�echny formul��e: menu, stavov� ��dek,
n�pov�du, atd.  Tyto prvky se mohou vyskytovat ve v�ech formul���ch, av�ak
v�ka�d�m mohou m�t jin� obsah.

Modul definuje jak t��dy slou��c� ke specifikaci konkr�tn�ch prvk�, tak
i�t��dy, kter� tyto specifikace n�sledn� zpracov�vaj�.

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
    
### Pomocn� funkce

def beep():
    """P�pni."""
    wx.Bell()


def microsleep(miliseconds=100):
    """�ekej a nic ned�lej po dobu 'miliseconds'."""
    wx.MicroSleep(miliseconds)


def busy_cursor(enable):
    """Zapni nebo vypni busy cursor.

    Argumenty:

      enable -- je-li pravdiv�, bude kurzor zapnut, v�opa�n�m p��pad� bude
        vypnut

    Pozn�mka: Hlavn� okno aplikace automaticky busy cursor vyp�n� v�idle
    handleru.

    """
    if enable:
        if not wx.IsBusy():
            wx.BeginBusyCursor()
    else:
        if wx.IsBusy():
            wx.EndBusyCursor()

def is_busy_cursor():
    """Vra� pravdu, pr�v� kdy� je nastaven busy cursor."""
    return wx.IsBusy()


def modal(window):
    """Vra� pravdu pr�v� kdy� je 'window' mod�ln�m oknem.

    'window' je pova�ov�no za mod�ln�, jestli�e je instanc� t��dy 'PopupForm'
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
    


### Pomocn� t��dy


class Restorable:
    """Prvek s�obnoviteln�m stavem.

    Potomci t�to t��dy se pova�uj� za objekty schopn� popsat sv�j stav a
    pozd�ji tento stav dle onoho popisu obnovit.  K�z�sk�n� popisu stavu,
    resp. obnoven� stavu, slou�� metoda 'save()', resp. 'restore()'.

    Tuto t��du by m�ly d�dit v�echny p��m� statick� d�ti aplika�n�ho okna.
    
    """
    def save(self):
        """Vra� informaci o�aktu�ln�m stavu prvku pro jeho pozd�j�� obnoven�.

        Vr�cen� hodnota m��e b�t cokoliv, co je schopna zpracovat metoda
        'restore()'.

        V�t�to t��d� metoda vrac� 'None'.
        
        """
        return None
        
    def restore(self, state):
        """Obnov stav prvku na z�klad� 'state'.

        Argumenty:

          state -- informace o�stavu vr�cen� d��ve metodou 'save()'

        V�t�to t��d� metoda ned�l� nic.
        
        """
        pass


class Window(wx.ScrolledWindow, Restorable):
    """Vym�niteln� okno.

    Tato t��da by m�la podporovat v�echny akce, kter� jsou nutn� k um�st�n�
    okna do aplikace a poskytovat pro tyto akce jednotn� rozhran�.

    """

    
    _focused_window = None

    def __init__(self, parent):
        """Inicializuj instanci.

        Argumenty:

          parent -- rodi�ovsk� okno.  Instance 'wx.Window'.
        
        """
        wx.ScrolledWindow.__init__(self, parent, wx.NewId())
        self._parent = parent
        
    def parent(self):
        """Vra� rodi�ovsk� okno zadan� v�konstruktoru."""
        return self._parent

    def resize(self, size=None):
        """Nastav velikost okna na velikost danou jako tuple (x, y).

        Pokud nen� velikost ud�na, je okno automaticky nastaveno na velikost
        sv�ho rodi�ovsk�ho okna.

        """
        if size is None:
            size = self._parent.GetClientSize()
        self.SetSize(size)
        
    def show(self):
        """Zobraz (vykresli) toto okno a u�i� jej aktivn�m."""
        self.Enable(True)
        self.Show(True)
        self.focus()

    def hide(self):
        """U�i� toto okno neaktivn�m a skryj jej."""
        self.defocus()
        self.Show(False) # nutn� i�p�ed uzav�en�m
        self.Enable(False)

    def close(self):
        """Definitivn� uzav�i okno a zru� ve�ker� jeho obsah."""
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
        """Zru� focus tomuto oknu."""
        if Window._focused_window is self:
            Window._focused_window = None

def focused_window():
    """Vra� zaost�en� okno nebo 'None', nen�-li jak�."""
    return Window._focused_window


class WxKey:
    """Pr�ce s�reprezentac� kl�ves.

    T��da umo��uje porovnat ud�lost s�definic� kl�vesy.  V�budoucnu mohou b�t
    jej� funkce roz���eny.

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
            # Mus� to b�t a� tady, kv�li (ne)import�m wx na serveru
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
        """Vra� pravdu, pr�ve kdy� 'event' neodpov�d� jen modifik�toru."""
        code = event.GetKeyCode()
        # Chyb� symboly pro Meta a Alt, tak�e natvrdo 307...
        return code not in (wx.WXK_SHIFT, wx.WXK_CONTROL, 307)
        
    def is_event_of_key(self, event, key):
        """Vra� pravdu, pr�v� kdy� 'event' byla vyvol�na 'key'.

        Argument:

          event -- instance wx.Event
          key -- string definuj�c� kl�vesu dle specifikace v�modulu 'command'

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
            # zde nepou��vat event.HasModifiers(), proto�e ta vrac�
            # p�i zapnut�m NumLocku v�dy pravdu.
            if event.AltDown() or event.ControlDown():
                return False
        return code == event.GetKeyCode()

    def event_key(self, event):
        """Vra� stringovou podobu kl�vesov� ud�losti 'event'.

        Ne v�echny ud�losti mus� vracet rozumnou nebo spr�vnou stringovou
        podobu, podporov�ny jsou pouze rozezn�van� kl�vesov� ud�losti.
        
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
    """Stejn� jako p�edek, av�ak definuje rozumn� porovn�n�."""
    
    def __cmp__(self, other):
        """Vra� shodu, pr�v� kdy� 'self' a 'other' maj� shodn� RGB slo�ky."""
        try:
            result = cmp(self.Red(), other.Red()) or \
                     cmp(self.Green(), other.Green()) or \
                     cmp(self.Blue(), other.Blue())
        except AttributeError:
            # Je-li `other' barvou, m��e b�t ve wxWindows ledacos, proto nelze
            # za�adit n�jak� rozumn� test na t��du instance.
            result = compare_objects(self, other)
        return result


def color2wx(color):
    """Vra� barvu ve form� akceptovan� wxWindows.

    Pokud odpov�daj�c� barva nen� zn�ma, vra� 'None'.
    
    Argumenty:

      color -- po�adovan� barva, jedna z�konstant t��dy
        'pytis.presentation.Color' nebo n�zev barvy z datab�ze barev
        (instance wxTheColourDatabase)

    """
    return _WX_COLORS.get(color, None) or _WX_COLORS_DATABASE.get(color, None)

### Univerz�ln� handlery


class Keymap:
    """Kl�vesov� mapa.

    Kl�vesov� mapa umo��uje definovat p�i�azen� p��kaz� a p��padn� jejich
    argument� kl�ves�m.

    """
    def __init__(self, parent=None):
        """Inicializuj instanci.

        Argumenty:

          parent -- rodi�ovsk� kl�vesov� mapa; bu� 'None' (pak kl�vesov� mapa
            je �ist�), nebo instance 'Keymap' (pak se pod�d� v�echny kl�vesy
            z�dan� kl�vesov� mapy, pokud nejsou p�edefinov�ny).  Argument nen�
            nutno kl��ovat.

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
        """P�i�a� kl�vese 'key' p��kaz 'command' s�argumenty '**kwargs'.

        Argumenty:

          key -- �et�zec, resp. sekvence �et�zc�, definuj�c� kl�vesu,
            resp. sekvenci kl�ves; popis viz n�e
            
          command -- p�i�azen� p��kaz, instance t��dy 'Command'
          
          args -- parametry p��kazu, p�edan� p�i jeho vyvol�n� obslu�n� metod�

        P�i�azovan� kl�vesy jsou �et�zce sestaven� dle n�sleduj�c�ch pravidel:

        - Kl�vesa odpov�daj�c� znaku anglick� abecedy je reprezentov�na
          �et�zcem rovn�m tomuto znaku.  Velikost p�smen je br�na v potaz (viz.
          tak� d�le modifik�tor Shift).

        - Funk�n� kl�vesy F1 a� F12 se zapisuj� �et�zci 'F1' a� 'F12'.

        - �ipky se zapisuj� �et�zci 'Up', 'Down', 'Left', 'Right'.

        - Kl�vesy 'Escape', 'Enter', 'Tab', 'Insert', 'Delete', 'Backspace',
          'Home', 'End', 'Prior' a 'Next' se zapisuj� stejnojmenn�mi �et�zci.

        - Kl�vesa s�modifik�torem Control je zaps�na ve form�tu 'Ctrl-<KEY>',
          kde '<KEY>' je z�pis kl�vesy bez tohoto modifik�toru.

        - Kl�vesa s�modifik�torem Alt je zaps�na ve form�tu 'Alt-<KEY>', kde
          '<KEY>' je z�pis kl�vesy bez tohoto modifik�toru.

        - Kl�vesa s�modifik�torem Shift je zaps�na ve form�tu 'Shift-<KEY>',
          pokud <KEY> nen� znak anglick� abecedy -- ten je zaps�n jako
          odpov�daj�c� velk� p�smeno.
          
        - Lze pou��vat v�ce modifik�tor� sou�asn�.  Potom jsou modifik�tory
          zapisov�ny v�dy v po�ad� Ctrl, Alt, Shift (nap�. tedy Ctrl-Alt-s nebo
          Alt-Shift-Tab, nikoliv potom Alt-Ctrl-x).

        """
        key = xtuple(key)
        if key != (None,):
            self._define_key(key, command, args)

    def lookup_key(self, key):
        """Vra� definici asociovanou s�kl�vesou 'key'.

        Argumenty:

          key -- string popisuj�c� kl�vesu v�notaci uveden� v�docstringu t��dy

        Vrac�: Je-li na kl�vesu napojen p��kaz, vra� dvojici (COMMAND, ARGS),
          kde COMMAND je instance t��dy 'Command' a ARGS jsou jeho argumenty
          jako dictionary pro p�ed�n� obslu�n� metod�.  Je-li na kl�vesu
          p�ipojena kl�vesov� mapa (v�p��pad� v�cekl�vesov�ch definic), je
          vr�cena tato mapa jako instance t��dy 'Keymap'.  Nen�-li kl�vesa
          definov�na, vra� 'None'.

        """
        try:
            return self._keymap[key]
        except KeyError:
            return None

    def lookup_command(self, command, args={}):
        """Vra� kl�vesovou zkratku asociovanou s�dan�m p��kazem a argumenty.

        Argumenty:

          command -- p��kaz, instance t��dy 'Command'
          args -- argumenty p��kazu jako dictionary.

        Vrac�: Je-li p��kaz s dan�mi argumenty napojen na n�jakou kl�vesu, vra�
          definici kl�vesov� zkratky jako tuple (v�dy, by� jednoprvkov�).
          Nen�-li pro p��kaz s dan�mi argumenty kl�vesa definov�na (p�edchoz�m
          vol�n�m metody 'define_key()'), vra� 'None'.

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
        """Vra� seznam v�ech platn�ch kl�ves, jako tuple �et�zc�."""
        return self._keymap.keys()
    
    def __str__(self):
        return '<Keymap: %s>' % str(self._keymap)
    

class KeyHandler:
    """T��da schopn� p�ev�d�t kl�vesov� ud�losti na p��kazy.

    T��da v�konstruktoru registruje pro zpracov�n� kl�ves metodu
    'on_key_down()', kter� zaji��uje p�evod na kl�vesy na p��kaz a vyvol�n�
    jeho obslu�n� metody.  Ve t��d� se vytvo�� kl�vesov� mapa poskl�dan�
    z�kl�ves p��kaz� instance on� t��dy plus v�ech jej�ch poru�n�k�.  P�i
    konfliktu kl�ves maj� p�ednost ty bli��� dan� t��d�.

    T��da je ur�ena k�\"p�id�d�n�\" ve v�ech t��d�ch, kter� cht�j� samy
    odchyt�vat kl�vesov� ud�losti.

    """

    def __init__(self, widgets=None):
        """Inicializuj instanci.

        Argumenty:

          widgets -- wx widget, nebo jejich sekvence, pro kter� m� b�t
            definov�n handler kl�vesy; m��e b�t i�'None', v�kter�m�to p��pad�
            bude on�m widgetem 'self'

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
        """Registruj se pro o�et�en� kl�vesov�ch ud�lost� dan�ch UI prvk�."""
        for widget in widgets:
            wx_callback(wx.EVT_KEY_DOWN, widget, self.on_key_down)
        
    def _init_commands(self):
        # Nem��eme `_commands' inicializovat hned v�konstruktoru, proto�e
        # tou dobou je�t� nemus� b�t v�echny p��kazy ve t��d� definov�ny.
        keymap = self._get_keymap()
        commands = []
        for attrname in public_attributes(self.__class__):
            if starts_with(attrname, 'COMMAND_'):
                command = getattr(self.__class__, attrname)
                if isinstance(command, Command):
                    commands.append(command)
        # Do atributu p�i�azujeme a� nyn�, aby to bylo odoln�j�� vl�kn�m
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
                    log(DEBUG, 'Nalezen p��kaz kl�vesy', (command, kwargs))
                command.invoke(**kwargs)
                return True
                    
        else:
            guardian = self._key_guardian
            if guardian is None:
                if __debug__: log(DEBUG, '��dn� dal�� poru�n�k')
                return False
            else:
                if __debug__: log(DEBUG, 'P�ed�n� poru�n�kovi:', guardian)
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
                log(DEBUG, 'Vytvo�ena kl�vesov� mapa', (self, self.keymap))
        return self.keymap

    def define_key(self, key, command, args):
        """Definuj kl�vesovou zkratku v kl�vesov� map� t�to instance.

        Kl�vesov� mapa nemus� b�t dostupn� v dob� inicializace instance, tak�e
        nen� mo�n� definovat kl�vesov� zkratky p��mo.  Tato metoda zaru�uje, �e
        p�edan� kl�vesov� zkratky budou d��ve nebo pozd�ji spr�vn� uplatn�ny.

        Argumenty jsou shod� jako v metod� 'Keymap.define_key()'.
        
        """
        if self._commands is None:
            # `_init_commands()' was not yet called.
            self._temporary_keymap.append((key, command, args))
        else:
            self.keymap.define_key(key, command, args)
            if command not in self._commands:
                self._commands.append(command)
        
    def on_key_down(self, event, dont_skip=False):
        """Zpracuj kl�vesovou ud�lost 'event'.

        Pokud existuje v�instanci p��kaz napojen� na danou kl�vesu, zavolej
        jeho obslu�nou metodu.  Pokud takov� p��kaz neexistuje nebo pokud
        obslu�n� metoda odm�tne p��kaz zpracovat (vr�t� nepravdu), ponech
        'event' k�dal��mu zpracov�n�.

        Argumenty:

          event -- kl�vesov� wx ud�lost
          dont_skip -- pr�v� kdy� je pravdiv�, nen� proveden skip ud�losti,
            i�kdy� neodpov�d� ��dn�mu p��kazu

        Vrac�: Pravdu, pr�v� kdy� ud�lost byla �sp�n� p�evedena na p��kaz.

        """
        if __debug__: log(DEBUG, 'Stisk kl�vesy:', event)
        wk = self._wx_key
        if not wk.is_true_key(event):
            return
        message(None)
        if __debug__: log(DEBUG, 'Ud�lost zpracov�v�:', str(self))
        guardian = self._key_guardian
        if self._commands is None:
            self._init_commands()
        if self._current_keymap is None or \
           not isinstance(last_user_event(), wx.KeyEvent):
            self._current_keymap = self._get_keymap()
        if __debug__:
            log(DEBUG, 'Aktu�ln� kl�vesov� mapa:', str(self._current_keymap))
        key = wk.event_key(event)
        keydef = self._current_keymap.lookup_key(key)
        if isinstance(keydef, Keymap):
            if __debug__: log(DEBUG, 'Prefixov� kl�vesa', keydef)
            self._prefix_key_sequence.append(key)
            message('Prefixov� kl�vesa: %s (%s)' % \
                    (' '.join(self._prefix_key_sequence),
                     ', '.join(keydef.keys())))
            self._current_keymap = keydef
            return True
        else:
            # Pozor, wxWindows je debiln� a ne v�dy p�ed�v� ud�losti rodi��m!
            self._current_keymap = None
            self._prefix_key_sequence = []
            if keydef is not None:
                result = self._maybe_invoke_command(keydef)
                if result:
                    return result
            if guardian:
                if __debug__: log(DEBUG, 'Kl�vesa p�ed�na v��e')
                return guardian.on_key_down(event, dont_skip)
            if dont_skip:
                if __debug__: log(DEBUG, 'Kl�vesa ignorov�na')
            else:
                if __debug__: log(DEBUG, 'Kl�vesov� ud�lost p�esko�ena')
                event.Skip()
        return False


class CallbackHandler:
    """Mixin t��da pro prvky podporuj�c� nastavov�n� a spou�t�n� callback�.

    T��da, kter� pod�d� 'CallbackHandler', z�sk� metody 'set_callback()' a
    '_run_callback()'.  Metoda 'set_callback()' je ur�ena pro pou�it� ze
    strany u�ivatele odvozen� t��dy -- nastavuje funkci, kter� m� b�t vyvol�na
    pro o�et�en� ur�it� akce.  Naproti tomu metoda '_run_callback()' je ur�ena
    pro pou�it� uvnit� odvozen� t��dy v m�stech, kde m� b�t tato funkce
    vyvol�na.

    Odvozen� t��da mus� definovat konstanty s prefixem 'CALL_', jejich� hodnoty
    slou�� k rozli�en� jednotliv�ch druh� callback�.

    """
    def __init__(self):
        self._callbacks = {}

    def set_callback(self, kind, function):
        # Toto mus� b�t samostatn� metoda a nejen parametr konstruktoru mimo
        # jin� kv�li cyklick�m callbackov�m z�vislostem v�du�ln�m formul��i.
        """Nastav 'function' pro callback 'kind'.

        Pokud byla pro callback 'kind' p�edt�m nastavena jin� funkce, je toto
        p�edchoz� nastaven� zru�eno.

        Argumenty:

          kind -- druh callbacku, jedna z�'CALL_*' konstant t��dy
          function -- funkce, kter� m� b�t vyvol�na.  Po�et a v�znam argument�
            je d�n odvozenou t��dou a m�l by b�t zdokumentov�m v r�mci
            jej� dokumentace.
            
        """
        assert kind[:5] == 'CALL_' and hasattr(self, kind), \
               ('Invalid callback kind', kind)
        assert function is None or callable(function), \
               ('Callback function not callable', function)
        if __debug__:
            log(DEBUG, 'Nastaven callback ��dkov�ho seznamu:', (kind, function))
        self._callbacks[kind] = function


    def get_callback(self, kind):
        if self._callbacks.has_key(kind):
            return self._callbacks[kind]
        return None

    def _run_callback(self, kind, *args, **kwargs):
        """Vyvolej funkci pro o�et�en� callbacku 'kind'.

        Pokud nebyla funkce pro o�et�en� dan�ho callbacku p�edt�m nastavena
        metodou 'set_callback()', ned�lej nic a vra� 'False', jinak vracej
        'True'.

        Argumenty:
        
          kind -- druh callbacku, jedna z�'CALL_*' konstant t��dy
          
          args, kwargs -- argumenty volan� funkce.  Po�et a v�znam argument� je
            d�n odvozenou t��dou a m�l by b�t zdokumentov�m v r�mci dokumentace
            callbackov� konstanty.
            
        """
        try:
            callback = self._callbacks[kind]
        except KeyError:
            return False
        if callback:
            if __debug__: log(DEBUG, 'Bude vol�n callback:', (kind, callback))
            apply(callback, args, kwargs)
            return True
            


#=============================#
# Specializovan� prvky        #
#=============================#


### Menu


class _MenuObject(object):
    """Spole�n� p�edek v�ech t��d specifikuj�c�ch strukturu menu."""


class MSeparator(_MenuObject):
    """Odd�lova� polo�ek menu.

    Pokud se mezi polo�kami menu vyskytne instance t�to t��dy, bude na jej�m
    m�st� vytvo�en vizu�ln� odd�lova�.
    
    """


class _TitledMenuObject(_MenuObject):

    def __init__(self, title):
        """Initializuj instanci.

        Argumenty:

          title -- n�zev menu, string
          
        'title' je v�dy pova�ov�n za jazykov� z�visl� text a tud� automaticky
        podl�h� jazykov� konverzi.
          
        """
        assert isinstance(title, types.StringTypes)
        self._title = gettext_(title)
        
    def title(self, raw=False):
        """Vra� titulek menu zadan� v�konstruktoru jako string."""
        if raw:
            return self._title
        else:
            return self._title.replace('&', '')

    
class Menu(_TitledMenuObject):
    """Specifikace menu.

    Menu je d�no sv�m popisem a polo�kami.  Polo�ky mohou b�t bu� vlastn�
    aktiva�n� polo�ky (instance t��dy 'MItem'), odd�lova�e (instance t��dy
    'MSeparator') nebo vno�en� menu (instance t��dy 'Menu').  U�t�to t��dy
    nerozli�ujeme, zda se jedn� o�pull-down menu, pop-up menu nebo vno�en�
    menu, specifikace je stejn� pro v�echny typy menu.

    Z vytvo�en� instance t�to t��dy lze potom vytvo�it instanci wxMenu pomoc�
    metody 'create()'.

    """ 
    def __init__(self, title, items):
        """Uschovej specifikaci menu.

        Argumenty:

          title -- n�zev menu, string
          items -- polo�ky menu, sekvence instanc� t��d 'Menu' a 'MItem';

        """
        assert is_sequence(items)
        if __debug__:
            for i in items:
                assert isinstance(i, _MenuObject)
        self._items = tuple(items)
        super(Menu, self).__init__(title)

    def items(self):
        """Vra� sekvenci polo�ek menu zadanou v�konstruktoru."""
        return self._items
        
    def create(self, parent, keyhandler):
        """Vytvo� menu dle specifikace a vra� instanci 'wx.Menu'.

        Tato metoda zkonstruuje menu v�etn� v�ech vno�en�ch podmenu, p�i�em�
        zabezpe�� ve�ker� nav�z�n� ud�lost� apod.
        
        Argumenty:

          parent -- wx rodi� vytv��en� instance 'wx.Menu'
          keyhandler -- instance 'KeyHandler', jej� kl�vesov� mapa bude
            synchronizov�na s p��kazy a hork�mi kl�vesami polo�ek menu.
        
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
                # Toto je zde zejm�na kv�li nake�ov�n� datov�ch specifikac�
                # pro v�po�et 'Command.enabled()' p�i startu aplikace.  Polo�ky
                # jsou spr�vn� aktivov�ny i bez toho, ale prvn� zobrazen� menu
                # je pomalej��.
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
    """Specifikace polo�ky menu.

    T��da nic nevytv���, pouze si pamatuje parametry a p�ed�v� je t��d� Menu,
    kter� prov�d� tvorbu menu.
    
    """
    _WX_KIND = wx.ITEM_NORMAL
    _used_titles = {}
    
    def __init__(self, title, command, args=None, help='', hotkey=None):
        """Uschovej parametry.

        Argumenty:

          title -- titulek menu, nepr�zdn� �et�zec
          command -- instance t��dy 'Command' odpov�daj�c� p��kazu, kter� m�
            b�t p�i aktivaci t�to polo�ky menu vyvol�n.  Zde m��e b�t p�ed�na
            tak� dvojice (COMMAND, ARGS).  V tom p��pad� je instance p��kazu
            prvn�m prvkem t�to dvojice a druh� prvek nahrazuje argument
            `args', kter� t�mto ji� nesm� b�t p�ed�n.
          args -- dictionary argument� p��kazu 'command'.
          help -- �et�zec obsahuj�c� jedno��dkovou n�pov�du, zobrazovan�
            ve stavov�m ��dku p�i pr�chodu p�es polo�ku; m��e b�t pr�zdn�
          hotkey -- hork� kl�vesa, kter� m� b�t s dan�m p��kazem a argumenty
            spojena, string nebo sekvence string� dle specifikace v�modulu 'command'
            
        Je-li uveden argument 'hotkey' a nejsou p�ed�v�ny ��dn� 'args', je
        'command' automaticky nastavena tato kl�vesa.
          
        'title' a 'help' jsou v�dy pova�ov�ny za jazykov� z�visl� texty
        a tud� automaticky podl�haj� jazykov� konverzi.

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
        """Nastav dodate�n� kl�vesovou zkratku polo�ky menu."""
        assert hotkey is None or isinstance(hotkey, (types.StringTypes,
                                                     types.TupleType,
                                                     types.ListType))
        self._hotkey = xtuple(hotkey)
    
    def command(self):
        """Vra� command zadan� v�konstruktoru."""
        return self._command

    def args(self):
        """Vra� argumenty command zadan� v�konstruktoru."""
        return self._args

    def hotkey(self):
        """Vra� horkou kl�vesu polo�ky jako tuple �et�zc�.

        Pokud nem� polo�ka p�i�azenu horkou kl�vesu, vra� tuple '(None,)'.

        """
        return self._hotkey


class CheckItem(MItem):
    """Polo�ka menu, kter� m��e b�t ve stavu ON/OFF."""
    _WX_KIND = wx.ITEM_CHECK
    
    def __init__(self, title, command, state=None, **kwargs):
        """Inicializuj instanci.

        Arguemnty:

          state -- funkce (volan� bez argument�), kter� vrac� True/False podle
            toho, zda je stav t�to polo�ky 'zapnuto', nebo 'vypnuto'.

          V�echny ostatn� arguemnty jsou sthodn� jako v konstruktoru p�edka.

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
    """Polo�ka menu tvo��c� p�ep�natelnou skupinu."""
    # wx.ITEM_RADIO zp�sobuje SEGFAULT.  CheckItem se v�ak, zd� se, chov� �pln�
    # stejn�, tak�e to vlastn� v�bec nevad�...
    #_WX_KIND = wx.ITEM_RADIO
    pass



class MenuBar(wx.MenuBar):
    """Wx implementace pull-down menu hlavn�ho aplika�n�ho okna.

    T��da zkonstruuje menubar a vlo�� jej do zadan�ho framu.  Menubar je
    zkonstruov�n na z�klad� specifikace, kter� se skl�d� z�instanc� 'Menu'
    ur�uj�c�ch jednotliv� polo�ky menubaru.

    Menubar je jednotn� pro celou aplikaci.
    
    """
    
    def __init__(self, parent, menus, keyhandler):
        """Vytvo� menubar na z�klad� sekvence 'menus' a vlo� do 'parent'.

        Argumenty:
        
          parent -- instance t��dy 'wxFrame', do kter� m� b�t menubar vlo�en
          menus -- sekvence instanc� t��dy 'Menu' definuj�c� jednotliv�
            menu v�menu baru; menu se v�menu baru vytvo�� ve stejn�m po�ad�,
            v�jak�m jsou v�t�to sekvenci uvedena
          keyhandler -- instance 'KeyHandler', jej� kl�vesov� mapa bude
            synchronizov�na s p��kazy a hork�mi kl�vesami polo�ek menu.
            
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
                    log(OPERATIONAL, _("Duplicitn� kl�vesa polo�ky menu:"),
                        (k, menu.title(), cmd))
                    log(OPERATIONAL, _("Koliduj�c� p��kaz:"), self._keys[k])
                else:
                    self._keys[k] = cmd


### Status bar


class StatusBar(wx.StatusBar):
    """T��da realizuj�c� stavov� ��dek.

    Stavov� ��dek slou�� pro zobrazov�n� kr�tk�ch zpr�v u�ivateli a zobrazov�n�
    stavov�ch informac�.  M��e b�t rozd�len do v�ce pol�, z�nich� ka�d� slou��
    k�zobrazen� samostatn� informace.  Tato t��da roz�i�uje t��du
    'wxStatusBar' z�wxWindows, m�ly by v�ak b�t vol�ny pouze jej� vlastn�
    metody a ne metody pod�d�n� z�t��dy 'wxStatusBar'.

    Stavov� ��dek je definov�n sv�m rozd�len�m na pole, z�nich� ka�d� m� sv�
    jm�no a ���ku.  V�t�chto pol�ch lze zobrazovat zpr�vy metodou 'message()'.
    
    P��klad pou�it�:
    
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
        """Inicializuj StatusBar a vytvo� pojmenovan� pole.

        Argumenty:
        
          parent -- rodi�ovsk� okno
          fields -- sekvence specifikac� pol�

        Ka�d� polo�ka sekvence 'fields' je dvojice (ID, WIDTH), kde ID je jm�no
        pole jako nepr�zdn� string a WIDTH je jeho ���ka v dialogov�ch
        jednotk�ch.  Je-li WIDTH rovno None, pole je rozta�eno na maxim�ln�
        mo�nou ���ku po ode�ten� ���ek ostatn�ch pol�; WIDTH sm� b�t None pouze
        pro jedin� pole.  ID pole jednozna�n� identifikuje a k�poli se jeho
        prost�ednictv�m p�istupuje v�metod�ch t�to t��dy.  ��dn� dv� pole nesm�
        m�t stejn� ID.
        
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
        """Vra� po�adov� ��slo pole 'id' pro wxWindows.

        Argumenty:
        
           id -- id pole jako string

        Pokud pole pojmenovan� 'id' neexistuje, vyvolej v�jimku 'KeyError'.

        """
        return self._field_numbers[id]
    
    def message(self, id, message, timeout=None):
        """Nastav text pole 'id' na 'message'.

        Argumenty:
        
          id -- id pole jako string
          message -- string, kter� m� b�t nov�m obsahem pole; m��e b�t
            i�'None', v�kter�m�to p��pad� bude p�edchoz� hl�en� smaz�no
          timeout -- pokud je zad�no, zpr�va zmiz� po zadan�m po�tu sekund
          
        Pokud stavov� ��dka dan� pole neobsahuje, vra� nepravdu, jinak vracej
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
            # Zm�nu prov�d�me jen p�i dan� podm�nce, aby n�m status bar
            # zbyte�n� neblikal.
            self.SetStatusText(message, field)
        if timeout is not None:
            self._timer[id].start(timeout)
        return True

    def get_message(self, id):
        """Vra� text pole 'id' jako string.

        Argumenty:
        
           id -- identifik�tor pole jako �et�zec

        Pokud pole dan�ho 'id' neexistuje, je vr�cena hodnota None.

        """
        try:
            return self.GetStatusText(self._field_number(id))
        except KeyError:
            return None
      

class InfoWindow(object):
    """Nemod�ln� okno pro zobrazen� textov�ch informac�."""
    
    def __init__(self, title, text, format=TextFormat.PLAIN, _name='info'):
        """Zobraz nemod�ln� okno nez�visl� na hlavn�m okn� aplikace.
        
        Argumenty:
        
          title -- titulek okna jako �et�zec.
          text -- text, kter� bude zobrazen v okn�.  Zp�sob zpravov�n� je ur�en
            argumentem 'format'.
          format -- vstupn� form�t textu, jako konstanta 'TextFormat'.  V
            p��pad� prost�ho textu ('TextFormat.PLAIN') z�stane ��dkov�n� i
            ve�ker� dal�� form�tov�n� nedot�eno (je ponech�no na volaj�c�
            stran�).  V p��pad� form�tu 'TextFormat.HTML' je vstupn� text
            pova�ov�n p��mo za text s HTML zan�kov�n�m.  Text v�ak nen� s�m o
            sob� platn�m HTML dokumentem.  Neobsahuje hlavi�ku, ani zna�ky
            <html> a <body>.  Jde jen o zform�tovan� text, kter� bude vsazen do
            t�la automaticky vytvo�en�ho dokumentu.

        """
        assert isinstance(title, types.StringTypes)
        assert isinstance(text, types.StringTypes)
        assert format in public_attributes(TextFormat)
        frame = wx.Frame(wx_frame(), title=title, name=_name)
        view = wx_text_view(frame, text, format)
        frame.Show(True)
        
        
# P�evodn� funkce
        
def char2px(window, x, y):
    """P�epo��tej znakov� rozm�r na pixely a vra� instanci 'wx.Size'.

    Vstupn� rozm�r je ch�p�n jako ���ka a v��ka \"b�n�ho\" znaku.

    Argumenty:
    
      window -- okno, podle jeho� fontu m� b�t rozm�r vypo��t�n.
      x -- ���ka; po�et znak�
      y -- v��ka; po�et znak�

    Vrac�: Rozm�ry v pixelech jako instanci 'wx.Size'.
    
    """

    return dlg2px(window, 4*x, 8*y)

def dlg2px(window, x, y=None):
    """P�epo��tej znakov� rozm�r na pixely.

    Vstupn� rozm�r je ch�p�n jako ���ka a v��ka \"b�n�ho\" znaku.

    Tento p�epo�et by n�s m�l odst�nit od z�vislosti na pou�it�m v�stupn�m
    za��zen� / syst�mov�ch nastaven�ch. Poskytuje mo�nost z�skat rozm�ry
    v�pixelech odpov�daj�c� aktu�ln�m rozm�r�m znak� v dan�m okn�.
    
    Funk�nost je v t�to implementaci zalo�en� na tzv. \"dialog units\"
    z�wxWindows, tak�e p��padnou dal�� dokumentaci je nutno hledat tam.

    Argumenty:
    
      window -- okno, podle jeho� fontu m� b�t rozm�r vypo��t�n.
      x -- ���ka; jednotkou je 1/4 ���ky znaku; integer
      y -- v��ka; jednotkou je 1/8 v��ky znaku; integer

    Vrac�: Rozm�ry v pixelech jako instanci wxSize, pokud byl zad�n i y-ov�
      rozm�r.  Pokud je vol�n pouze s x-ov�m rozm�rem, vr�t� rovnou integer.
    
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
    """P�eve� konstantu t��dy 'Orientation' na wx reprezentaci."""
    if orientation == spec.Orientation.VERTICAL:
        return wx.VERTICAL
    elif orientation == spec.Orientation.HORIZONTAL:
        return wx.HORIZONTAL
    else:
        raise ProgramError("Neplatn� hodnota Orientation:", orientation)


def border_style2wx(style):
    """P�eve� konstantu t��dy 'BorderStyle' na wx reprezentaci."""
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
        raise ProgramError("Neplatn� hodnota BorderStyle:", orientation)


def wx_focused_window():
    """Vra� aktu�ln� zaost�en� wx okno, jako instanci 'wx.Window'."""
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

