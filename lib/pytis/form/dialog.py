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

"""Dialogov� okna.

Dialogov� okna slou�� jako nep�ehl�dnuteln� upozorn�n� nebo ot�zky pro
u�ivatele.  U�ivateli je znemo�n�no pokra�ovat pr�ci a� do doby, ne� potvrd�
zobrazen� hl�en� nebo odpov� na ot�zku.

V�echny dialogy vych�z� z�abstraktn� t��dy 'Dialog'.

Modul d�le obsahuje n�kolik pomocn�ch funkc� vyu��vaj�c�ch dialogy pro v�cekr�t
se vyskytuj�c� dialogov� operace.

"""

import types
import pytis.data
from pytis.form import *
import config
from wx import calendar
from wx.lib import masked     

class Dialog(KeyHandler, CommandHandler, object):
    """Abstraktn� t��da, kter� je z�kladem v�ech dialog�.

    V�echny dialogy mus� b�t potomky t�to t��dy.  Vytvo�en� instance dialogu
    je�t� neznamen� jeho vyvol�n�, pro to slou�� metoda 'run()'.  Metodu
    'run()' lze na jednu instanci volat teoreticky i v�cekr�t.  Instance
    dialogu v�ak sama o sob� neobsahuje ��dn� objekty u�ivatelsk�ho rozhran�,
    pouze si pamatuje jejich vlastnosti.  K vytvo�en� okna a jeho prvk� doch�z�
    a� p�i vol�n� metody 'run()'.
    
    Tato t��da pouze definuje abstraktn� metodu 'run()'.
    
    """
    def get_command_handler_instance(cls, application):
        return application.top_window()
    get_command_handler_instance = classmethod(get_command_handler_instance)
    
    def __init__(self, parent):
        self._parent = parent
        KeyHandler.__init__(self)
        self._key_guardian = None
        
    def run(self):
        """Vyvolej dialog a po�kej na odpov��.

        Vrac�: Hodnotu z�vislou na typu dialogu.

        V�t�to t��d� metoda ned�l� nic a v�dy vrac� pravdu.

        """
        return True

class GenericDialog(Dialog):
    """Obecn� dialog s tla��tky.

    Univerz�ln� dialogov� t��da, od kter� je mo�no odvodit specializovan� t��dy
    konkr�tn�ch dialog� pomoc� p�edefinov�n� n�kter�ch metod.
    
    """    
        
    def __init__(self, parent, title, buttons, default=None, report=None,
                 report_format=TextFormat.PLAIN):
        """Inicializuj dialog.

        Argumenty:

          parent -- wx rodi�; instance 'wx.Frame' nebo 'wx.Dialog'
          title -- titulek dialogov�ho okna jako string
          buttons -- sekvence n�zv� tla��tek dialogu, strings
          default -- n�zev p�edvolen�ho tla��tka (string obsa�en� v 'buttons',
            nebo 'None')
          report -- Text reportu, kter� m� b�t zobrazen v okn� dialogu.  Jedn�
            se o del�� text, kter� bude automaticky scrollovateln�.  Je mo�n�
            zobrazit tak� komplexn� text s HTML �i Wiki form�tov�n�m.  V
            takov�m p��pad� je nutn� toto indikovat argumentem 'report_format'.
            Pro vstupn� form�t plat� stejn� pravidla, jako v p��pad� t��dy
            'InfoWindow'.
          report_format -- konstanta t��dy 'TextFormat' ur�uj�c� jak m� b�t
            nakl�d�no se vstupn�m textem argumentu 'report'.  V p��pad�, �e
            nen� ��dn� report specifikov�n, je tento argument itrelevantn�.
            
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
        """Vytvo� cel� dialog (postupn� okno, jeho obsah a tla��tka).
        
        Nejprve je vytvo�eno okno dialogu jako takov� ('wx.Dialog') a potom
        je zavol�na metoda `_create_dialog_elements'.

        Tuto metodu by nem�lo b�t t�eba p�edefinov�vat. Ve v�t�in� p��pad� by
        m�lo sta�it p�edefinovat metodu '_create_content()'.

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
        """Vlo� do dialogu jeho vnit�n� prvky.
        
        Pomoc� sizer� je do dialogu vlo�en hlavn� obsah (v�sledek metody
        '_create_content()') a tla��tka (v�sledek metody
        '_create_buttons()').

        Tuto metodu by nem�lo b�t t�eba p�edefinov�vat. Ve v�t�in� p��pad� by
        m�lo sta�it p�edefinovat metodu '_create_content()' nebo
        '_create_buttons()'.

        """
        # vytvo� obsah (vnit�ek) dialogu
        content = xtuple(self._create_content())
        # vytvo� tla��tka a poskl�dej je vedle sebe
        button_sizer = wx.BoxSizer()
        for b in self._create_buttons():
            button_sizer.Add(b, 0, wx.ALL, 8)
            # registruj handlery ud�lost�
            wx_callback(wx.EVT_BUTTON, dialog, b.GetId(), self._on_button)
            self._handle_keys(b)
        # poskl�dej obsah a tla��tka do top-level sizeru (nad sebe)
        sizer = wx.BoxSizer(wx.VERTICAL)
        if self._report is not None:
            report = wx_text_view(dialog, self._report,
                                  format=self._report_format)
            sizer.Add(report, 1, wx.EXPAND)
        for part in content:
            sizer.Add(part, 0, wx.ALL|wx.CENTER, 5)
        sizer.Add(button_sizer, 0, wx.CENTER)
        wx_callback(wx.EVT_IDLE, self._dialog, self._on_idle)
        # dokon�i ...
        sizer.SetSizeHints(dialog)
        dialog.SetAutoLayout(True)
        dialog.SetSizer(sizer)
        sizer.Fit(dialog)

    def _create_content(self):
        """Abstraktn� metoda - nutno p�edefinovat v odvozen�ch t��d�ch.

        Vrac�: Libovoln� wx objekt, kter� m� b�t vlo�en do t�la dialogu.
        
        """
        pass
    
    def _create_buttons(self):
        """Vytvo� tla��tka a vra� je jako sekvenci."""
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
        # Vra� n�pis tla��tka s dan�m id.
        try:
            return self._button_labels[id]
        except KeyError:
            return None

    def _button_id(self, label):
        # Vra� id tla��tka s dan�m n�pisem.
        for id in self._button_labels.keys():
            if self._button_labels[id] == label:
                return id
        return None

    def _customize_result(self, result):
        """Vra� n�vratovou hodnotu podle v�sledku ukon�en�ho dialogu.

        V t�to t��d� jednodu�e vrac� n�pis tla��tka, kter�m byl dialog ukon�en
        ('None' v p��pad�, �e byl ukon�en jin�m zp�sobem ne� tla��tkem).

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
        """Zobraz dialog a po jeho ukon�en� vra� jeho n�vratovou hodnotu.

        N�vratov� hodnota z�vis� na typu dialogu, resp. na jeho metod�
        '_customize_result()'.

        """
        self._create_dialog()
        self._dialog.SetFocus()
        result = self._customize_result(self._run_dialog())
        self._dialog.Destroy()
        return result

    def rebuild(self):
        """Znovu vytvo� obsah dialogu bez jeho uzav�en�.

        Tato metoda je ur�ena k�pou�it� v�p��padech, kdy je b�hem zobrazen�
        mod�ln�ho dialogu nutno zm�nit jeho obsah, nap��klad p�idat nebo
        odebrat (nikoliv pouze zapnout nebo vypnout) n�kter� prvky.

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
    """Dialog zobrazuj�c� zpr�vu a vracej�c� odpov��.

    Tato t��da pouze zobrazuje zpr�vu a tla��tko pro akceptov�n� dialogu.

    Vr�cen� hodnota metody 'run()' je jednodu�e n�pis tla��tka, kter�m byl
    dialog ukon�en (None v p��pad�, �e byl ukon�en jin�m zp�sobem ne�
    tla��tkem).

    """
    ICON_INFO = wx.ART_INFORMATION
    "Ikona pro informativn� zpr�vy (��rovka)"
    ICON_QUESTION =  wx.ART_QUESTION
    "Ikona s otazn�kem."
    ICON_WARNING = wx.ART_WARNING
    "Ikona s vyk�i�n�kem."
    ICON_ERROR = wx.ART_ERROR
    "Ikona pro chybov� zpr�vy."
    ICON_TIP = wx.ART_TIP
    "Ikona pro tipy, rady apod."

    BUTTON_OK = _('Ok')
    "N�pis pro potvrzovac� tla��tko." 
    BUTTON_CANCEL = _('Zru�it')
    "N�pis pro opou�t�c� tla��tko." 
    BUTTON_YES = _('Ano')
    "N�pis pro tla��tko souhlasu." 
    BUTTON_NO = _('Ne')
    "N�pis pro tla��tko nesouhlasu." 
    
    _icons = (ICON_INFO, ICON_QUESTION, ICON_WARNING, ICON_ERROR, ICON_TIP)
    
    def __init__(self, parent, message, icon=ICON_INFO, title=_('Zpr�va'),
                 buttons=(BUTTON_OK,), default=_(BUTTON_OK), report=None,
                 report_format=TextFormat.PLAIN):
        """Inicializuj dialog.

        Argumenty:

          'parent', 'buttons', 'title' a 'default' -- jako u 'GenericDialog'.
          'message' -- Zpr�va zobrazen� v t�le dialogu (string).
          'icon' -- Jedna z ICON_* konstant t��dy ('ICON_INFO' atd.).
          
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
        """Vytvo� obsah - to co bude vypl�ovat plochu okna nad tla��tky."""
        sizer = wx.BoxSizer()
        if self._icon is not None:
            icon = self._create_icon(self._icon)
            if icon is not None:
                sizer.Add(icon, 0, wx.ALL|wx.ALIGN_CENTER_VERTICAL, 5)
        t = wx.StaticText(self._dialog, -1, self._message)
        sizer.Add(t, 1, wx.EXPAND|wx.ALL|wx.ALIGN_CENTER_VERTICAL, 12)
        return sizer

    
class Warning(Message):
    """Dialog pro zobrazen� varovn� zpr�vy."""

    def __init__(self, parent, message, title=_('Varov�n�')):
        """Inicializuj dialog.

        Argumenty:


          Odpov�daj� stejn�m argument�m rodi�ovsk� t��dy s t�m, �e n�sleduj�c�
          argumenty tato t��da definuje v�dy napevno:

            icon = 'Message.ICON_WARNING'
            buttons = ('Message.BUTTON_OK',)
            default = 'Message.BUTTON_OK'

        """
        super_(Warning).__init__(self, parent, message, title=title,
                                 icon=Message.ICON_WARNING,
                                 buttons=(Message.BUTTON_OK,),
                                 default=Message.BUTTON_OK)


class Error(Message):
    """Dialog pro zobrazen� chybov� zpr�vy."""

    def __init__(self, parent, message, title=_('Chyba')):
        """Inicializuj dialog.
        
        Argumenty:


          Odpov�daj� stejn�m argument�m rodi�ovsk� t��dy s t�m, �e n�sleduj�c�
          argumenty tato t��da definuje v�dy napevno:

            icon = 'Message.ICON_ERROR'
            buttons = ('Message.BUTTON_OK',)
            default = 'Message.BUTTON_OK'

        """
        super_(Error).__init__(self, parent, message, title=title,
                               icon=Message.ICON_ERROR,
                               buttons=(Message.BUTTON_OK,),
                               default=Message.BUTTON_OK)


class MultiQuestion(Message):
    """Dialog vy�aduj�c� odpov�� na ot�zku v�b�rem z�tla��tek.

    Jedn� se o�jednoduch� p�izp�soben� t��dy message, s�p�ednastaven�m ikony a
    titulku.
    
    """
    def __init__(self, parent, message, buttons, default=None,
                 title=_("Ot�zka"), icon=Message.ICON_QUESTION,
                 report=None, report_format=TextFormat.PLAIN):
        super_(MultiQuestion).__init__(self, parent, message, title=title,
                                       buttons=buttons, default=default,
                                       icon=icon, report=report, report_format=report_format)
    

class Question(MultiQuestion):
    """Dialog vy�aduj�c� odpov�� ano/ne na zobrazenou zpr�vu (ot�zku).

    Metoda 'run()' vrac�: Pravdu, pr�v� kdy� u�ivatel odpov� na danou
    ot�zku kladn� - stiskne tla��tko s n�pisem 'Message.BUTTON_YES'.

    """
    def __init__(self, parent, message, default=True,
                 title=_("Ot�zka"), icon=Message.ICON_QUESTION,
                 report=None, report_format=TextFormat.PLAIN):
        """Inicializuj dialog.
        
        Argumenty:

          default -- pokud je pravda, bude p�edvolen�m tla��tkem tla��tko
            'Message.BUTTON_YES'. Jinak je p�edvolen� odpov��
            'Message.BUTTON_NO' (implicitn�).
        
          Ostatn� argumenty odpov�daj� stejn�m argument�m rodi�ovsk� t��dy s
          t�m, �e n�sleduj�c� argumenty tato t��da definuje v�dy napevno:

            buttons = ('Message.BUTTON_YES', 'Message.BUTTON_NO')

        Kl��ov� argument 'default' m��e b�t uv�d�n i bez explicitn�ho
        pojmenov�n�, tak�e mus� b�t do budoucna zaru�eno jeho zachov�n� v�etn�
        po�ad�.
            
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
    """Dialog pro zad�n� textu.

    Dialog krom� zpr�vy obsahuje i�textov� vstupn� pole a tla��tka 'Ok' a
    'Zru�it'.  N�vratovou hodnotou dialogu je zadan� string, byl-li odesl�n
    tla��tkem 'OK' �i stiskem kl�vesy Enter, nebo 'None', byl-li dialog opu�t�n
    jinak (tla��tko 'Zru�it', kl�vesa Escape apod.).

    """
    def __init__(self, parent, message=None, value=None, prompt=None,
                 title=_("Zadejte hodnotu"), icon=None, passwd=False,
                 report=None, report_format=TextFormat.PLAIN,
                 input_width=None, input_height=1, allow_empty=True):
        """Inicializuj dialog.

        Argumenty:

          prompt -- v�zva p�ipojen� p�ed pol��ko (string) nebo 'None'
          value -- p�ednastaven� hodnota textov�ho vstupn�ho pol��ka (string)
          passwd -- pokud m� pravdivou hodnotu, bude se textov� pol��ko
            chovat joko pol��ko pro vstup hesla (vepsan� znaky budou
            zobrazov�ny jako hv�zdi�ky)
          input_width -- ���ka vstupn�ho prvku ve znac�ch nebo
            'None' (v kter�m�to p��pad� se pou�ije implicitn� velikost)
          input_height -- v��ka vstupn�ho pol��ka ve znac�ch
          allow_empty -- pokud je pravda (implicitn� ano), je pr�zdn� vstup
            akceptov�n, jinak dialog vy�aduje zad�n� n�jak� hodnoty

          Kl��ov� argumenty jsou p�ed�ny konstruktoru t��dy
          wx.pytis.masked.TextCtrl.
          Zb�vaj�c� argumenty odpov�daj� stejn�m argument�m rodi�ovsk� t��dy
          s t�m, �e n�sleduj�c� argumenty tato t��da definuje v�dy napevno:

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
    """Dialog pro zad�n� hesla.

    Speci�ln� p��pad dialogu 'InputDialog' ur�en� pro zad�v�n� hesla.
    
    """
    def __init__(self, parent, message=None, title=_("Zadejte heslo"),
                 prompt=_('Heslo:'), icon=None):
        """Inicializuj dialog.

        Argumenty:

          Argumenty odpov�daj� stejn�m argument�m rodi�ovsk� t��dy s t�m, �e
          argument 'passwd' je nastaven v�dy na pravdivou hodnotu.
          
        """
        super_(Password).__init__(self, parent, message=message, title=title,
                                  prompt=prompt, passwd=True, icon=icon)

class Login(InputDialog):
    """Dialog pro zad�n� u�ivatelsk�ho jm�na a hesla.

    """
    def __init__(self, parent, message=None, title=_(u"Zadejte heslo"),
                 login='', icon=None, login_prompt=_(u"U�ivatelsk� jm�no:"), 
                 passwd_prompt=_(u"Heslo:")):
        """Inicializuj dialog.

        Speci�ln� argumenty:

          login -- p�edvypln�n� hodnota u�ivatelsk�ho jm�na.
          login_prompt -- v�zva pro zad�n� u�ivatelsk�ho jm�na.
          passwd_prompt -- v�zva pro zad�n� hesla.
          
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
    """Dialog pro zad�n� datumu.

    Speci�ln� p��pad dialogu 'InputDialog' ur�en� pro zad�v�n� datumu.
    
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
    """Dialog pro zad�v�n� ��sel.

    Speci�ln� p��pad dialogu 'InputDialog' ur�en� pro zad�v�n� ��sel.
    
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
        # Zji�t�n� desetinn�ho odd�lova�e a odd�lova�e t�s�c�
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
    """Dialog pro spu�t�n� formul��e.

    Umo�n� u�ivateli vybrat t��du formul��e a zadat n�zev specifikace.  Ty
    potom vr�t� v tuplu jako v�sledek vol�n� sv� metody 'run()'.
    
    """
    _BROWSE_FORM = "BrowseForm"
    _EDIT_FORM = "EditForm"
    _BROWSE_DUAL_FORM = "BrowseDualForm"
    _CODEBOOK_FORM = "CodebookForm"

    def __init__(self, parent, title=_("Zobrazit formul��")):
        """Inicializuj dialog.

        Argumenty:

          Argumenty odpov�daj� stejn�m argument�m rodi�ovsk� t��dy.
          
        """
        super_(RunFormDialog).__init__(self, parent, message=None,
                                       title=title, input_width=25,
                                       prompt=_("N�zev specifikace:"),
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
        label = wx.StaticText(self._dialog, -1, _("T��da formul��e:"))
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
    """Dialog pro spu�t�n� dlouhotrvaj�c� operace.
    
    Spu�t�n�m dialogu metodou 'run()' bude zobrazen mod�ln� dialog a spu�t�na
    funkce zadan� v konstruktoru.  Dialog je ukon�en automaticky po skon�en�
    funkce.  Do t� doby u�ivatel nem��e d�lat nic, ne� �ekat...
    
    """
    def __init__(self, parent, function, args=(), kwargs={},
                 title=_("Prov�d� se operace"),
                 message=_("�ekejte pros�m...")):
        """Inicializuj dialog.

        Argumenty:

          parent, title, message -- odpov�daj� stejn�m argument�m rodi�. t��dy.
          function -- funkce, kter� m� b�t spu�t�na.
          args -- argumenty spou�t�n� funkce jako tuple.
          kwargs -- kl��ov� argumenty spou�t�n� funkce jako dictionary.
          
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
    """Dialog pro spu�t�n� dlouhotrvaj�c� operace s ProgressBarem.
    
    Spu�t�n�m dialogu metodou 'run()' bude zobrazen mod�ln� dialog a spu�t�na
    funkce zadan� v konstruktoru s argumenty p�edan�mi konstruktoru.

    Nav�c bude funkci v�dy p�ed�n prvn� argument, kter�m je funkce
    aktualizuj�c� stav progress baru.  Za jej� vol�n� v pr�b�hu operace je
    funkce vykon�vaj�c� operaci zodpov�dn�.  Aktualiza�n� funkce vy�aduje jeden
    argument, kter�m je stav operace v procentech (integer).  Aktualiza�n�
    funkce tak� vrac� nepravdivou hodnotu, pokud m� b�t operace ukon�ena
    (u�ivatelsk� p�eru�en�, je-li povoleno).  Za ukon�en� je v�ak op�t
    zodpov�dn� funkce vykon�vaj�c� operaci.  Druh�m nepovinn�m argumentem je
    kl��ov� argument 'newmsg'.  Pokud je p�ed�n nepr�zdn� �et�zec, bude tak�
    aktualizov�na zpr�va zobrazen� na dialogu (tato zpr�va nahrad� p�vodn�
    zpr�vu zadanou v konstruktoru).

    Po ukon�en� dialogu (a� u� z d�vodu u�ivatelsk�ho p�eru�en�, �i dokon�en�
    operace) je vr�cena n�vratov� hodnota funkce vykon�vaj�c� operaci.

    """
    def __init__(self, parent, function, args=(), kwargs={},
                 title=_("Prov�d� se operace"), message=_("�ekejte pros�m..."),
                 elapsed_time=False, estimated_time=False,
                 remaining_time=False, can_abort=False):
        """Inicializuj dialog.

        Argumenty:

          parent, function, args, kwargs, message, title -- stejn�, jako u
            rodi�ovsk� t��dy, pouze 'function' mus� nav�c p�ij�mat odkaz na
            aktualiza�n� funkci jako prvn� argument (viz dokumentace t��dy).
          elapsed_time -- Pokud je 'True', zobraz� se ub�hl� �as
          estimated_time -- Pokud je 'True', zobraz� se p�edpokl�dan� �as
          remaining_time -- Pokud je 'True', zobraz� se zb�vaj�c� �as
          can_abort -- Pokud je 'True', bude mo�no vykon�v�n� funkce p�eru�it,
            pokud to funkce vykon�vaj�c� operaci umo��uje (viz. docstring
            t��dy).
          
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
    """Dialog pro opakovan� spou�t�n� operace nad seznamem argument�.

    Tento dialog je speci�ln�m p��padem pou�it� 'ProgressDialog' pro cyklick�
    spou�t�n� operace nad seznamem argument�.  U�ivatel pouze nemus� ps�t
    funkci prov�d�j�c� cyklus a aktualizuj�c� ProgressBar.
    
    """
    def __init__(self, parent, function, args=(), step=None, **kwargs):
        """Inicializuj dialog.

        Argumenty:

          function --
          args --
          kwargs --
          step -- cel� ��slo, ud�vaj�c� po�et procent, po kter�ch je
            progressbar aktualizov�n.  Pokud je step

          Ostatn� argumenty jsou shodn� jako u rodi�ovsk� t��dy.
          
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
    """Dialog zobrazuj�c� kalend��, umo��uj�c� v�b�r dne.

    Datum na kalend��i m��e b�t p�ednastaven parametrem konstruktoru. Metoda
    'run()' vrac� vybran� datum jako instanci 'mx.mxDateTime', nebo None, pokud
    byl dialog opu�t�n.
    
    """

    def __init__(self, parent, date, title=_("Kalend��"),
                 enable_year=True, enable_month=True, monday_first=True):
        """Inicializuj dialog.

        Argumenty:

          parent -- wx rodi�; instance 'wx.Frame' nebo 'wx.Dialog'
          date -- p�ednastaven� datum jako instance 'mxDateTime'.
          title -- titulek dialogov�ho okna jako string
          enable_year -- kdy� je pravda, zobraz� v�b�r roku; boolean
          enable_month -- kdy� je pravda, zobraz� v�b�r m�s�ce; boolean
          monday_first -- kdy� je pravda, bude pond�l� prvn�m dnem v t�dnu;
            boolean

        Pokud argument date neobsahuje �et�zec, kter� je mo�n� zpracovat pomoc�
        'wx.DateTime.ParseDate()', bude datum nastaven na dne�n� datum. 

        """
        super_(Calendar).__init__(self, parent, title=title,
                                  buttons=(Message.BUTTON_OK,
                                           Message.BUTTON_CANCEL))
        # vytvo� kalend��
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
    """Dialog umo��uj�c� v�b�r barvy.

    V�choz� barva m��e b�t p�ednastavena parametrem konstruktoru.  Metoda
    'run()' vrac� barvu jako �et�zec '#RRGGBB', nebo None, pokud byl dialog
    opu�t�n.
    
    """

    def __init__(self, parent, color=None, title=_("V�b�r barvy")):
        """Inicializuj dialog.

        Argumenty:

          parent -- wx rodi�; instance 'wx.Frame' nebo 'wx.Dialog'
          color -- p�ednastaven� barva, jako �et�zec '#RRGGBB'.
          title -- titulek dialogov�ho okna jako string

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
    """Dialog pro zobrazen� neo�ek�van� v�jimky.

    Dialog nab�z� u�ivateli mo�nost v�b�ru reakce na v�jimku, v�etn� mo�nosti
    odesl�n� ozn�men� o�chyb�.

    Dialog vrac� jednu z�n�sleduj�c�ch hodnot:

      None -- po�aduje-li u�ivatel ukon�en� aplikace
      pr�zdn� string -- po�aduje-li u�ivatel chybu ignorovat
      nepr�zdn� string -- po�aduje-li u�ivatel poslat ozn�men� o�chyb� s�textem
        stringu
    
    """
    # Existuje sice wxPython.pytis.ErrorDialogs, ale to vypad� jako t�k� a
    # nep��li� funk�n� hack.

    _IGNORE_LABEL = _("Ignorovat")
    _REPORT_LABEL = _("Poslat ozn�men� o�chyb�")
    _EXIT_LABEL = _("Ukon�it aplikaci")
    
    def __init__(self, parent, einfo):
        """Inicializuj instanci.

        Argumenty:

          parent -- wx rodi�; instance 'wx.Frame' nebo 'wx.Dialog'
          einfo -- informace o�v�jimce ve tvaru vracen�m funkc�
            'sys.exc_info()'

        """
        super_(BugReport).__init__(self, parent, _("Neo�ek�van� chyba"),
                                   buttons=(self._IGNORE_LABEL,
                                            self._REPORT_LABEL,
                                            self._EXIT_LABEL),
                                   default=self._IGNORE_LABEL)
        self._einfo = einfo

    def _create_content(self):
        self._sizer = sizer = wx.BoxSizer(wx.VERTICAL)
        label = wx.StaticText(self._dialog, -1, _("Nevy�lo to"))
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
                # P��li� "dlouh�" text se nemus� pov�st do pol��ka vlo�it...
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
    """Dialog pro v�b�r souboru.

    Zobraz� dialog s mo�nost� proch�zen� adres��� a v�b�ru souboru.

    """
    OPEN = 'OPEN'
    """Konstanta ur�uj�c� dialog pro otev�en� existuj�c�ho souboru."""  
    SAVE = 'SAVE'
    """Konstanta ur�uj�c� dialog pro zad�n� jm�na souboru pro ulo�en�."""  

    _last_directory = {}

    def __init__(self, parent, title=None, dir=None, file=None, mode=OPEN,
                 wildcards=(_("V�echny soubory")+" (*.*)|*.*",),
                 multi=False, overwrite_prompt=True):
        """Inicializuj dialog.

        Argumenty:

          parent -- wx rodi�; instance 'wx.Frame' nebo 'wx.Dialog'
          title -- titulek dialogov�ho okna jako string; pokud je None, bude
            dopln�n v�choz� titulek v z�vislosti na argumentu 'mode'.
          dir -- p�ednastaven� cesta; �et�zec, nebo None.
          file -- p�ednastaven� n�zev souboru; �et�zec, nebo None.
          mode -- typ dialogu; jedna z konstant 'OPEN' a 'CLOSE' t��dy.
          wildcards -- seznam masek soubor� a popis�, podle kter�ch bude mo�no
            filtrovat; jedn� se o sekvenci, kde ka�d� lich� prvek ur�uje popis
            a ka�d� sud� prvek je wildcard �et�zcem, podle kter�ho budou
            soubory filtrov�ny, pokud je zvolen; v�choz� filtrov�n� je podle
            prvn� dvojice. p��klad: ("BMP soubory (*.bmp)", "*.bmp",
                                     "GIF soubory (*.gif)", "*.gif")
          multi -- pokud je pravda, bude mo�no vybrat v�ce soubor� najednou;
            relevantn� poouze pro 'mode'='OPEN'.
          overwrite_prompt -- pokud je pravda, bude p�i v�b�ru existuj�c�ho
            souboru pro ukl�d�n� zobrazena ot�zka, zda m� b�t soubor p�eps�n;
            relevantn� poouze pro 'mode'='SAVE'; pokud je pravda, bude
            n�vratovou hodnotou metody 'run()' tuple.

        """
        super_(FileDialog).__init__(self, parent)
        assert mode in (FileDialog.OPEN, FileDialog.SAVE)
        if title is None:
            title = {FileDialog.OPEN: _("Otev��t soubor"),
                     FileDialog.SAVE: _("Ulo�it soubor")}[mode]
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
        """Zobraz dialog a vra� cestu k vybran�mu souboru jeko �et�zec.

        Pokud je argument konstruktoru 'multi' pravdiv�, bude vr�cen tuple
        �et�zc�.

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


# Pomocn� funkce vyu��vaj�c� dialogy


def db_operation(operation, quiet=False):
    """Prove� datab�zovou 'operation' s�o�et�en�m p��padn�ch chyb.

    Funkce vol� funkci 'operation' s�odchyt�v�n�m v�jimek typu
    'pytis.data.dbdata.DBException' (a�jen t�chto v�jimek).  Dojde-li k�v�jimce,
    funkce zobraz� u�ivateli dialog s�(pokud mo�no) lidsky srozumitelnou
    zpr�vou a zept� se jej, zda m� zkusit operaci zopakovat.  Postup se opakuje
    tak dlouho, dokud u�ivatel na tuto ot�zku odpov�d� kladn�.  U�v�jimek typu
    'DBLoginException' je u�ivatel dot�z�n na heslo, kter� je p�ed
    zopakov�n�m operace nastaveno.

    Dojde-li k��sp�n�mu proveden� 'operation', a� u� na prvn� pokus nebo
    pozd�ji, je vr�cen jej� v�sledek.

    Argumenty:

      operation -- funkce bez argument�, prov�d�j�c� datab�zovou operaci a
        vracej�c� jej� v�sledek; m��e to b�t t� tuple o�dvou nebo t�ech
        prvc�ch odpov�daj�c�ch argument�m funkce 'apply'
      quiet -- pr�v� kdy� je pravda, nezobrazuj� se p�i chyb� ��dn� dialogy
        krom� p�ihla�ovac�ho dialogu (je-li t�eba) a dojde ihned k�n�vratu

    Vrac�: Dvojici (SUCCESS, RESULT), kde SUCCESS je flag indikuj�c� zda
    datab�zov� operace v�kone�n�m v�sledku usp�la (pravda) nebo ne (nepravda) a
    RESULT je v�sledek vol�n� 'operation' (je-li SUCCESS nepravda, nen� hodnota
    RESULT specifikov�na).

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
            run_dialog(Error, _("P��stup odm�tnut"))
            return FAILURE
        except pytis.data.DBLoginException, e:
            import config
            prompt = _("Zadejte heslo pro p��stup do datab�ze")
            login, password = run_dialog(Login, prompt, login=config.dbuser)
            if password == None:
                return FAILURE
            config.dbconnection = \
                   config.dbconnection.modified(user=login, password=password)
        except pytis.data.DBException, e:
            log(OPERATIONAL, "Datab�zov� chyba v�db_operation",
                format_traceback())
            if quiet:
                return FAILURE
            else:
                message = e.message()
                if e.exception():
                    message += '\n' + str(e.exception())
                message += '\n' + _("Zkusit znovu?")
                if not run_dialog(Question, message, 
                                  title='Datab�zov� chyba',
                                  icon=Question.ICON_ERROR):
                    return FAILURE
    return success, result


def delete_record_question(msg=None):
    """Zeptej se u�ivatele, zda m� b�t opravdu smaz�n z�znam.

    Vra� pravdu, pr�v� kdy� u�ivatel odpov� kladn�.
    
    """
    log(EVENT, 'Dialog maz�n� ��dku')
    if msg == None:
        msg = _("Opravdu chcete z�znam zcela vymazat?")        
    if not run_dialog(Question, msg):
        log(EVENT, 'Maz�n� ��dku u�ivatelem zam�tnuto')
        return False
    log(EVENT, 'Maz�n� ��dku u�ivatelem potvrzeno')
    return True
