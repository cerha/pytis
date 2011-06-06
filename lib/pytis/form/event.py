# -*- coding: iso-8859-2 -*-

# Zpracov�n� ud�lost�
# 
# Copyright (C) 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2011 Brailcom, o.p.s.
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

"""Zpracov�n� ud�lost�.

Hlavn�m c�lem tohoto modulu je umo�nit vy��zen� wx ud�losti b�hem zpracov�n�
jin� wx ud�losti.  Ud�losti ve wxWindows jsou blokuj�c�, b�hem zpracov�n�
ud�losti mus� dal�� ud�losti �ekat na jej� dokon�en�.  To m� n�kter� nep��jemn�
d�sledky, jako nap��klad nemo�nost pr�b�n� aktualizace obsahu oken nebo
mo�nost p�eru�en� dlouhotrvaj�c�ho zpracov�n� ud�losti u�ivatelem.  Modul se
sna�� tyto pot�e obej�t.

Aby modul plnil sv�j ��el, je nutno zajistit n�sleduj�c�:

- V�echna p�i�azen� callback� ('EVT_*' funkce) mus� b�t prov�d�na
  prost�ednictv�m funkce 'wx_callback()'.

- Na n�jak�m m�st�, nejl�pe v�top-level k�du u�ivatelsk�ho rozhran�, je nutno
  spustit hl�dac� vl�kno zavol�n�m funkce 'interrupt_watcher()'.

- Je nutno zajistit periodick� vol�n� funkce 'yield_()'.  Mnohdy je mo�no
  v�hodn� vyu��t h��ek ve funkci 'pytis.util.log()'.

"""

import os
import sys
import thread

from pytis.form import *
import wx

class UserBreakException(Exception):
    """V�jimka vyvol�van� p�i p�eru�en� zpracov�n� ud�losti.

    Tato v�jimka je pou��v�na jedno��elov� pro signalizaci p�eru�en� zpracov�n�
    ud�losti u�ivatelem.

    """
    def __init__(self, *args):
        super(UserBreakException, self).__init__(*args)
        message(_("Stop"), beep_=True)


_current_event = None
_interrupted = False
_main_thread_ident = None
_watcher_thread_ident = None
_wx_key = None


def top_level_exception():
    """Zpracuj aktu�ln� vyvolanou v�jimku aplikace."""
    einfo = sys.exc_info()
    if issubclass(einfo[0], SystemExit):
        sys.exit()
    tbstring = format_traceback()
    import cgitb
    try:
        tbstring = cgitb.text(einfo)
    except:
        import traceback
        tbstring = "\n".join(traceback.format_exception(*einfo))
    log(OPERATIONAL, 'Top-level exception caught', tbstring)
    text = run_dialog(BugReport, einfo)
    if text is None:
        sys.exit()
    elif text:
        to = config.bug_report_address
        if not to:
            run_dialog(Message, _("Nen� zn�ma c�lov� adresa. Je nutno nastavit konfigura�n� volbu "
                                  "`bug_report_address'."))
        else:
            tb = einfo[2]
            while tb.tb_next is not None:
                tb = tb.tb_next
            filename = os.path.split(tb.tb_frame.f_code.co_filename)[-1]
            buginfo = "%s at %s line %d" % (einfo[0].__name__, filename, tb.tb_lineno)
            address = config.sender_address
            if not address:
                import commands
                status, domain = commands.getstatusoutput('hostname -f')
                username = config.dbconnection.user()
                if status:
                    address = username
                else:    
                    address = '%s@%s' % (username, domain)
                while True:
                    address = run_dialog(InputDialog, prompt=_("Va�e e-mailov� adresa: "),
                                         value=address, input_width=30,
                                         message=_("Pokud svou adresu nastav�te ve formul��i "
                                                   "Nastaven� u�ivatelsk�ho rozhran�, nebudete "
                                                   "ji� p��t� dotazov�ni."))
                    if address is None or address and address.strip() != '':
                        break
            if address:
                import email.Header, email.Message, email.Utils, smtplib
                def header(value):
                    if isinstance(value, (str, unicode)):
                        try:
                            unicode(value, 'us-ascii')
                        except:
                            pass
                        else:
                            return value
                    return email.Header.Header(value, 'utf-8')
                msg = email.Message.Message()
                msg['From'] = header(address)
                msg['To'] = header(to)
                msg['Subject'] = header('%s: %s' % (config.bug_report_subject, buginfo))
                msg['Date'] = email.Utils.formatdate()
                msg.set_payload(text)
                try:
                    try:
                        server = smtplib.SMTP(config.smtp_server)
                        server.sendmail(address, to, msg.as_string())
                    finally:
                        try:
                            server.quit()
                        except:
                            pass
                except Exception, e:
                    run_dialog(Error, _("Ozn�men� se nezda�ilo odeslat:\n") + unicode(e))
                else:
                    run_dialog(Message, _("Ozn�men� o�chyb� odesl�no"))
    if config.debug_on_error:
        import pdb
        pdb.post_mortem(sys.exc_info()[2])
        return


_last_user_event = None
def last_user_event():
    """Vra� posledn� p�ijatou u�ivatelskou ud�lost jako instanci 'wx.Event'.

    P�ed prvn�m vol�n�m u�ivatelsk� ud�losti vra� 'None'.
    
    """
    return _last_user_event


def _is_user_event(event):
    # Sem nelze p�idat jen tak jakoukoliv ud�lost, proto�e n�kter� u�ivatelsk�
    # ud�losti nast�vaj� vesm�s b�hem jin�ch u�ivatelsk�ch ud�lost�, a�pak by
    # mohlo doj�t k�r�zn�m nep��jemn�m efekt�m.
    if isinstance(event, (wx.KeyEvent, wx.MenuEvent, wx.MouseEvent)):
        return True
    # Instance wxCommandEvent pova�ujeme za user_event, krom� ud�lost� gridu
    # a v�b�ru z menu (nap�. v�b�r z popup menu)
    if isinstance(event, wx.CommandEvent) and \
       not isinstance(event, (wx.grid.GridEvent, wx.UpdateUIEvent)) and \
       event.GetEventType() != wx.wxEVT_COMMAND_MENU_SELECTED:
        return True
    else:
        return False


_system_callback_lock = None
_system_callback_thread_ident = None
_system_callback_access_lock = thread.allocate_lock()
def wx_callback(evt_function, *args):
    """Obal wx callback hl�dac�m k�dem.

    Funkce zavol� 'evt_function' s�argumenty 'args'.  Posledn� prvek 'args'
    mus� b�t callback o�et�uj�c� p��slu�nou ud�lost.  Tento callback je obalen
    k�dem, kter� zaji��uje o�et�en� ud�losti i�v�p��pad�, kdy pr�v� prob�h�
    zpracov�n� jin� ud�losti.

    P��klad typick�ho vol�n� funkce:

      wx_callback(EVT_BUTTON, (self, button.GetId(), self.on_button))

    """
    evt_function_args, callback = args[:-1], args[-1]
    assert callable(evt_function)
    assert callable(callback)
    def process_event(event, callback=callback):
        def system_callback():
            # P�i zamyk�n� atd. se vyu��v� toho, �e v�existuj� jen dv� vl�kna
            # zpracov�vaj�c� ud�losti a �e v�r�mci jednoho vl�kna doch�z� pouze
            # k�sekven�n�mu nebo cibulovit�mu �azen� ud�lost�.
            STATE_CURRENT = 'STATE_CURRENT'
            STATE_FREE = 'STATE_FREE'
            STATE_BLOCKED = 'STATE_BLOCKED'
            global _system_callback_thread_ident, _system_callback_lock
            _system_callback_access_lock.acquire()
            try:
                ident = thread.get_ident()
                if _system_callback_thread_ident == ident:
                    # Jsme uvnit� vlastn� slupky, jsme v�pohod�
                    state = STATE_CURRENT
                elif _system_callback_thread_ident is None and \
                     _system_callback_lock is None:
                    # Nikdo jin� nem� z�jem, uzmeme to
                    _system_callback_thread_ident = ident
                    state = STATE_FREE
                else:
                    # M�me konkurenci -- vytvo��me si synchroniza�n� z�mek pro
                    # ozn�men� uvoln�n� cesty
                    _system_callback_lock = lock = thread.allocate_lock()
                    _system_callback_lock.acquire()
                    state = STATE_BLOCKED
            finally:
                _system_callback_access_lock.release()
            if state == STATE_BLOCKED:
                # �ek�me na uvoln�n� cesty
                lock.acquire()
                lock.release()
                _system_callback_access_lock.acquire()
                try:
                    # Je�t� st�le je to n� synchroniza�n� z�mek?  Uvolni jej!
                    if _system_callback_lock is lock:
                        _system_callback_lock = None
                    # Te� jsme na koni my
                    _system_callback_thread_ident = ident
                    state = STATE_FREE
                finally:
                    _system_callback_access_lock.release()
            try:
                # To hlavn�...
                result = callback(event)
            finally:
                _system_callback_access_lock.acquire()
                try:
                    # Jako prvn� usurp�to�i mus�me uvolnit informace o�sv�
                    # cibuli ...
                    if state == STATE_FREE:
                        _system_callback_thread_ident = None
                        if _system_callback_lock is not None:
                            while True:
                                try:
                                    # ... a poslat sign�l p��padn�mu �ekateli
                                    _system_callback_lock.release()
                                    break
                                except thread.error:
                                    # To je p��pad, kdy �ekatel je�t� nesta�il
                                    # na sv�j z�mek zavolat acquire
                                    pass
                finally:
                    _system_callback_access_lock.release()
            return result
        global _current_event, _interrupted, _last_user_event
        is_user = _is_user_event(event)
        if is_user:
            message('')
        if not isinstance(event, (wx.IdleEvent, wx.UpdateUIEvent)):
            if __debug__:
                log(DEBUG, 'Zpracov�n� ud�losti:', (event, event.__class__))
        try:
            if thread.get_ident() == _watcher_thread_ident or _current_event:
                # Ud�lost b�hem ud�losti
                if _wx_key and _wx_key.is_event_of_key(event, 'Ctrl-g'): # TODO: ne natvr.
                    _interrupted = True
                    result = True
                elif is_user:
                    result = True
                else:
                    result = system_callback()
            elif is_user and modal(top_window()):
                # Ud�lost vyvolan� u�ivatelsk�m p��kazem v�mod�ln�m okn�
                result = callback(event)
            elif is_user:
                # Ud�lost vyvolan� u�ivatelsk�m p��kazem
                _interrupted = False
                _current_event = event
                try:
                    result = callback(event)
                finally:
                    _interrupted = False # ud�lost kon�� -> nebude co p�eru�it
                    _current_event = None
                    _last_user_event = event
            else:
                # Standardn� "syst�mov�" ud�lost
                result = system_callback()
        except:
            top_level_exception()
            return
        return result
    evt_function(*(evt_function_args + (process_event,)))


def unlock_callbacks():
    """Uvolni uzam�en� u�ivatelsk�ch ud�lost� callbackem."""
    global _current_event
    _current_event = None


def yield_():
    """Zkontroluj, zda u�ivatel ne��d� p�eru�en� zpracov�n� ud�losti.

    ��d�-li, vyvolej 'UserBreakException'.

    """
    global _interrupted
    if _interrupted and _main_thread_ident == thread.get_ident():
        _interrupted = False
        raise UserBreakException()


def interrupt_watcher():
    """Spus� vl�kno sleduj�c� wx ud�losti b�hem zpracov�n� jin� wx ud�losti."""
    lock = thread.allocate_lock()
    lock.acquire()
    def watcher():
        interrupt_init(_watcher_thread_ident_=thread.get_ident())
        lock.release()
        last_event = None
        import application
        import time
        while application._application is not None:
            time.sleep(0.1)
            if _current_event is not None and _current_event is last_event:
                wx_yield_(full=True)
            else:
                last_event = _current_event
    thread.start_new_thread(watcher, ())
    # �ek�n� na dokon�en� inicializace watcheru
    lock.acquire()
    lock.release()
    
def interrupt_init(_main_thread_ident_=thread.get_ident(),
                   _watcher_thread_ident_=None):
    """Inicializuj zpracov�n� p�eru�en� ud�losti pro aktu�ln� thread."""
    global _wx_key, _main_thread_ident, _watcher_thread_ident
    _wx_key = WxKey()
    _main_thread_ident = _main_thread_ident_
    _watcher_thread_ident = _watcher_thread_ident_
