# -*- coding: iso-8859-2 -*-

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

"""Specifikace a zpracov�n� u�ivatelsk�ch prvk� hlavn� obrazovky.

Modul zav�d� prvky spole�n� pro v�echny formul��e: menu, stavov� ��dek,
n�pov�du, atd.  Tyto prvky se mohou vyskytovat ve v�ech formul���ch, av�ak
v�ka�d�m mohou m�t jin� obsah.

Modul definuje jak t��dy slou��c� ke specifikaci konkr�tn�ch prvk�, tak
i�t��dy, kter� tyto specifikace n�sledn� zpracov�vaj�.

"""

import copy
import string

from pytis.form import *
import wx, wx.combo
import pytis.presentation

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

def copy_to_clipboard(text):
    """Copy the text into system clipboard."""
    assert isinstance(text, (str, unicode))
    clipboard = wx.TheClipboard
    if clipboard.Open():
        if config.clipboard_primary_selection:
            clipboard.UsePrimarySelection(True)
        clipboard.SetData(wx.TextDataObject(text))
        clipboard.Flush()
        clipboard.Close()
        
def hotkey_string(hotkey):
    """Return the human readable hotkey representation of Keymap.lookup_command() result."""
    return ' '.join([k.replace(' ', _("Mezern�k")) for k in hotkey])


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


class Window(wx.Panel, Restorable):
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
        assert isinstance(parent, wx.Window), parent
        wx.Panel.__init__(self, parent, wx.NewId())
        self._parent = parent
        
    def _exit_check(self):
        "Prove� kontrolu p�ed uzav�en�m a vra� pravdu, je-li mo�no pokra�ovat."
        return True

    def _cleanup(self):
        "Prove� �klidov� akce p�ed uzav�en�m okna."
        pass
    
    def close(self, force=False):
        return self._close(force=force)

    def _close(self, force=False):
        """Definitivn� uzav�i okno a zru� ve�ker� jeho obsah."""
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
        self.Enable(False)
        self.Show(False) # nutn� i�p�ed uzav�en�m

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
    _RTRANS_TABLE = None

    def __init__(self):
        # Mus� to b�t a� tady, kv�li (ne)import�m wx na serveru
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
    return _WX_COLORS.get(color, None) or _WX_COLOR_DB.get(color, None) or wx.NamedColor(color)

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

        - Kl�vesa s�modifik�torem Shift je zaps�na ve form�tu 'Shift-<KEY>'.
          
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
        commands = []
        for attrname in public_attributes(self.__class__):
            if starts_with(attrname, 'COMMAND_'):
                command = getattr(self.__class__, attrname)
                if isinstance(command, Command):
                    commands.append(command)
        # Do atributu p�i�azujeme a� nyn�, aby to bylo odoln�j�� vl�kn�m
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
        keymap = self._get_keymap()
        keymap.define_key(key, command, args)
        
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
        assert kind[:5] == 'CALL_' and hasattr(self, kind), ('Invalid callback kind', kind)
        assert function is None or callable(function), ('Invalid callback function', function)
        self._callbacks[kind] = function
        if __debug__:
            log(DEBUG, 'Callback registered:', (kind, function))

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
            if __debug__:
                log(DEBUG, 'Invoking callback:', (kind, callback))
            callback(*args, **kwargs)
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
        self._title = title
        
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
        """Vra� sekvenci polo�ek menu zadanou v�konstruktoru."""
        return self._items

    def _on_highlight_item(self, menu, event):
        if event.GetMenuId() == -1:
            msg = ""
        else:
            msg = menu.FindItemById(event.GetMenuId()).GetHelp()
        message(msg, log_=False)
        event.Skip()
        
    def create(self, parent, keymap=None):
        """Vytvo� menu dle specifikace a vra� instanci 'wx.Menu'.

        Tato metoda zkonstruuje menu v�etn� v�ech vno�en�ch podmenu, p�i�em�
        zabezpe�� ve�ker� nav�z�n� ud�lost� apod.
        
        Argumenty:

          parent -- wx rodi� vytv��en� instance 'wx.Menu'
          keymap -- kl�vesov� mapa (instance 'Keymap'), kter� m� b�t
             synchronizov�na s kl�vesov�mi zkratkami polo�ek menu.
        
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
                    # Toto je zde zejm�na kv�li nake�ov�n� datov�ch specifikac�
                    # pro v�po�et 'Command.enabled()' p�i startu aplikace.  Polo�ky
                    # jsou spr�vn� aktivov�ny i bez toho, ale prvn� zobrazen� menu
                    # je pomalej��.
                    menu.Enable(wxitem.GetId(), item.command().enabled(**item.args()))
                    if __debug__:
                        if config.debug:
                            log(DEBUG, 'Menu item:', (item.title(),
                                                      item.command().enabled(**item.args()),))
                    if isinstance(item, (RadioItem, CheckItem)):
                        wxitem.Check(item.state())
                    if hotkey_str.has_key(item):
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
    """Specifikace polo�ky menu.

    T��da nic nevytv���, pouze si pamatuje parametry a p�ed�v� je t��d� Menu,
    kter� prov�d� tvorbu menu.
    
    """
    _WX_KIND = wx.ITEM_NORMAL
    _used_titles = {}
    
    def __init__(self, title, command, args=None, help=None, hotkey=None,
                 icon=None):
        """Uschovej parametry.

        Argumenty:

          title -- titulek menu, nepr�zdn� �et�zec
          
          command -- instance t��dy 'Command' odpov�daj�c� p��kazu, kter� m�
            b�t p�i aktivaci t�to polo�ky menu vyvol�n.  Zde m��e b�t p�ed�na
            tak� dvojice (COMMAND, ARGS).  V tom p��pad� je instance p��kazu
            prvn�m prvkem t�to dvojice a druh� prvek nahrazuje argument 'args',
            kter� t�mto ji� nesm� b�t p�ed�n.  Nakonec m��e b�t t�mto
            argumentem string, kter� je pak identifik�torem specifikace ze
            specifika�n�ho modulu 'app_commands'; tato specifikace je funkc�
            vracej�c� k��enou dvojici (COMMANDS, ARGS).
            
          args -- dictionary argument� p��kazu 'command'.
          
          help -- �et�zec obsahuj�c� jedno��dkovou n�pov�du, zobrazovan� ve
            stavov�m ��dku p�i pr�chodu p�es polo�ku; m��e b�t pr�zdn�
            
          hotkey -- hork� kl�vesa, kter� m� b�t s dan�m p��kazem a argumenty
            spojena, string nebo sekvence string� dle specifikace v�modulu
            'command'
            
          icon -- explicitn� definovan� ikona polo�ky menu.  Jedn� se o
            identifik�tor ikony pou�iteln� jako argument funkce 'get_icon'.
            Pokud nen� ur�ena, bude automaticky pou�ita ikona podle typu
            p��kazu (je-li pro p��kaz definov�na).
            
        Je-li uveden argument 'hotkey' a nejsou p�ed�v�ny ��dn� 'args', je
        'command' automaticky nastavena tato kl�vesa.
          
        'title' a 'help' jsou v�dy pova�ov�ny za jazykov� z�visl� texty
        a tud� automaticky podl�haj� jazykov� konverzi.

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
        assert help is None or isinstance(help, types.StringTypes)
        assert hotkey is None or isinstance(hotkey, (types.StringTypes,
                                                     types.TupleType,
                                                     types.ListType))
        assert icon is None or isinstance(icon, str)
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
            if args.has_key('binding'):
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
            if not args or (len(args) == 1 and args.has_key('enabled') and not callable(args['enabled'])):
                return ('proc/%s/%s/%s' % (proc_name, spec_name, command_proc,))
        if args and not command_proc:
            return None
        return ('%s/%s' % (command, command_proc,))

    @classmethod
    def parse_action(class_, action):
        """Parse action id back to command and its argumets.

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
            arguments = dict(handler=find_symbol(function_name))
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

    def help(self):
        """Vra� text n�pov�dy polo�ky zadan� v�konstruktoru."""
        return self._help

    def icon(self):
        """Return icon given in the constructor."""
        return self._icon

    def action_id(self):
        """Return action id string of the menu item or 'None' if it is unavailable."""
        return self._action_id
    

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

    def _create_icon(self, item):
        pass
        
    def _on_ui_event(self, event):
        event.Check(self.state())
        super(CheckItem, self)._on_ui_event(event)

    def state(self):
        return self._state()
        
        
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
    
    def __init__(self, parent, menus, keymap=None):
        """Vytvo� menubar na z�klad� sekvence 'menus' a vlo� do 'parent'.

        Argumenty:
        
          parent -- instance t��dy 'wxFrame', do kter� m� b�t menubar vlo�en
          menus -- sekvence instanc� t��dy 'Menu' definuj�c� jednotliv�
            menu v�menu baru; menu se v�menu baru vytvo�� ve stejn�m po�ad�,
            v�jak�m jsou v�t�to sekvenci uvedena
          keymap -- kl�vesov� mapa (instance 'Keymap'), kter� m� b�t
            synchronizov�na s kl�vesov�mi zkratkami polo�ek menu.
         
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
        

class ProfileSelectorPopup(wx.ListCtrl, wx.combo.ComboPopup):
    """Profile selection menu implemented using wx.ListCtrl.

    This class implements the 'wx.combo.ComboPopup' API and thus can be used as
    a popup selection of the 'ProfileSelector' control, which is derived form
    'wx.combo.ComboCtrl'.

    """
    def __init__(self):
        self.PostCreate(wx.PreListCtrl())
        wx.combo.ComboPopup.__init__(self)
        self._current_item = -1

    def _on_motion(self, event):
        item, flags = self.HitTest(event.GetPosition())
        if item >= 0:
            self.Select(item)
            self._current_item = item

    def _on_left_down(self, event):
        self.Dismiss()
        if self._current_item >= 0:
            LookupForm.COMMAND_APPLY_PROFILE.invoke(index=self._current_item)

    # The following methods implement the ComboPopup API.

    def Create(self, parent):
        # Create the popup child control. Return True for success.
        wx.ListCtrl.Create(self, parent, style=wx.LC_LIST|wx.LC_SINGLE_SEL|wx.SIMPLE_BORDER)
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
        width, height = (0, 0)
        for i, profile in enumerate(profiles):
            self.InsertStringItem(i, profile.name())
            w, h = self.GetItemRect(i)[2:]
            height += h
            width = max(width, w)
        self.Select(profiles.index(form.current_profile()))
        # Leave 6 px vertically and 30 px horizontally for padding.
        return wx.Size(max(width+30, minWidth), min(height+6, maxHeight))

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
    
    def __init__(self, parent, size):
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
                if current_profile and ctrl.GetValue() != current_profile.name():
                    ctrl.SetValue(current_profile.name())
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
            MItem(_("Ulo�it"),
                  LookupForm.COMMAND_UPDATE_PROFILE(),
                  help=_("Aktualizovat ulo�en� profil podle sou�asn�ho nastaven� formul��e")),
            MItem(_("Ulo�it jako nov�"),
                  Application.COMMAND_HANDLED_ACTION(
                    # Name must be edited first and 'cmd' will be invoked after confirmation.
                    handler=self._edit_profile_name,
                    enabled=self._edit_profile_name_enabled,
                    cmd=LookupForm.COMMAND_SAVE_NEW_PROFILE,
                    clear=True),
                  help=_("Vytvo�it nov� profil podle sou�asn�ho nastaven�m formul��e")),
            MItem(_("P�ejmenovat"),
                  Application.COMMAND_HANDLED_ACTION(
                    # Name must be edited first and 'cmd' will be invoked after confirmation.
                    handler=self._edit_profile_name,
                    enabled=self._edit_profile_name_enabled,
                    cmd=LookupForm.COMMAND_RENAME_PROFILE),
                  help=_("Upravit a ulo�it n�zev aktu�ln�ho profilu")),
            MItem(_("Smazat"), 
                  LookupForm.COMMAND_DELETE_PROFILE(),
                  help=_("Smazat zvolen� ulo�en� profil")),
            MSeparator(),
            MItem(_("Vr�tit posledn� ulo�en� nastaven�"),
                  LookupForm.COMMAND_RELOAD_PROFILE,
                  help=_("Zahodit zm�ny nastaven� formul��e proveden� "
                         "od posledn�ho ulo�en� profilu.")),
            MItem(_("Vr�tit v�choz� nastaven� aplikace"),
                  command=LookupForm.COMMAND_RESET_PROFILE,
                  help=_("Zahodit v�echny ulo�en� u�ivatelsk� zm�ny nastaven� formul��e.")),
            )
        popup_menu(self, menu)

    def _edit_profile_name(self, cmd, clear=False):
        ctrl = self.GetTextCtrl()
        def perform():
            name=self.GetValue()
            ctrl.SetEditable(False)
            cmd.invoke(name=name)
        ctrl.SetEditable(True)
        if clear:
            ctrl.SetValue('')
        else:
            ctrl.SelectAll()
        ctrl.SetFocus()
        message(_("Zadejte n�zev profilu a potvr�te stiskem ENTER."))
        self._on_enter_perform = perform

    def _edit_profile_name_enabled(self, cmd, clear=False):
        return cmd.enabled(name=self.GetValue())
        
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

def acceskey_prefix(i):
    pad = {'f': '  ', 'i': '  ', 'j': '  ', 'l': '  ', 'm': '', 't': '  ', 'r': '  ', 'w': ''}
    if i < 26:
        index = chr(i+97)
    else:
        index = str(i-25)
    return '&'+ index +'. '+ pad.get(index, ' ')


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

# Pomocn� funkce

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
    parent.PopupMenu(menu, position)
    menu.Destroy()


def get_icon(icon_id, type=wx.ART_MENU, size=(16,16)):
    """Najdi vra� po�adovanou ikonu jako instanci 'wx.Bitmap'.

    Argumenty:

      icon_id -- m��e to b�t bu�to jedna z konstant 'wx.ART_*' nebo �et�zec.
        Pokud jde o wx konstantu, je vr�cena p��slu�n� syst�mov� ikona
        odpov�daj�c� aktu�ln� zvolen�mu t�matu.  Pokud jde o �et�zec, je
        vyhled�v�na ikona stejn�ho jm�na (k n�mu� je automaticky p�ipojena
        p��pona '.png') v adres��i ur�en�m konfigura�n� volbou
        'config.icon_dir'.

      type, size -- only relevant when icon_id as a 'wx.ART_*' constant.

    If the icon is not found, returns None.

    """
    
    bitmap = None
    if isinstance(icon_id, str):
        imgfile = os.path.join(config.icon_dir, icon_id + '.png')
        if os.path.exists(imgfile):
            img = wx.Image(imgfile, type=wx.BITMAP_TYPE_PNG)
            bitmap = wx.BitmapFromImage(img)
        else:
            log(OPERATIONAL, "Could not find icon file:", imgfile)
    elif icon_id is not None:
        bitmap = wx.ArtProvider_GetBitmap(icon_id, type, size)
    if bitmap and bitmap.Ok():
        return bitmap
    else:
        return None

    
def wx_focused_window():
    """Vra� aktu�ln� zaost�en� wx okno, jako instanci 'wx.Window'."""
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
    assert not isinstance(icon, str) or label is not None, \
           "Button with non-system icon must have a fallback label."
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


def wx_text_view(parent, content, format=TextFormat.PLAIN, width=None, height=None):
    import wx
    if format == TextFormat.PLAIN:
        lines = content.splitlines()
        if width is None:
            width = min(max([len(l) for l in lines]), 80)
        if height is None:
            height = min(len(lines), 20)
        style = wx.TE_MULTILINE | wx.TE_DONTWRAP | wx.TE_READONLY
        w = wx.TextCtrl(parent, style=style)
        w.SetValue(content)
    else:
        if width is None:
            width = 80
        if height is None:
            height = 20
        import wx.html
        w = wx.html.HtmlWindow(parent)
        if format == TextFormat.WIKI:
            import lcg
            n = lcg.ContentNode('', content=lcg.Container(lcg.Parser().parse(content)))
            html = n.content().export(lcg.HtmlExporter().context(n, None))
        else:
            html = content
        if (wx.MAJOR_VERSION, wx.MINOR_VERSION) == (2, 6):
            html = '<font size="-2">' + html + '</font>'
        w.SetPage('<html><head></head><body>'+ html +'</body></html>')
    w.SetBestFittingSize(char2px(w, width, height))
    return w

