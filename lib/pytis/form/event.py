# -*- coding: utf-8 -*-

# Zpracování událostí
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

"""Zpracování událostí.

Hlavním cílem tohoto modulu je umožnit vyřízení wx události během zpracování
jiné wx události.  Události ve wxWindows jsou blokující, během zpracování
události musí další události čekat na její dokončení.  To má některé nepříjemné
důsledky, jako například nemožnost průběžné aktualizace obsahu oken nebo
možnost přerušení dlouhotrvajícího zpracování události uživatelem.  Modul se
snaží tyto potíže obejít.

Aby modul plnil svůj účel, je nutno zajistit následující:

- Všechna přiřazení callbacků ('EVT_*' funkce) musí být prováděna
  prostřednictvím funkce 'wx_callback()'.

- Na nějakém místě, nejlépe v top-level kódu uživatelského rozhraní, je nutno
  spustit hlídací vlákno zavoláním funkce 'interrupt_watcher()'.

- Je nutno zajistit periodické volání funkce 'yield_()'.  Mnohdy je možno
  výhodně využít háček ve funkci 'pytis.util.log()'.

"""

import collections
import os
import sys
import thread

from pytis.form import *
import wx

class UserBreakException(Exception):
    """Výjimka vyvolávaná při přerušení zpracování události.

    Tato výjimka je používána jednoúčelově pro signalizaci přerušení zpracování
    události uživatelem.

    """
    def __init__(self, *args):
        super(UserBreakException, self).__init__(*args)
        message(_(u"Stop"), beep_=True)


_current_event = None
_interrupted = False
_main_thread_ident = None
_watcher_thread_ident = None
_wx_key = None


def top_level_exception():
    """Zpracuj aktuálně vyvolanou výjimku aplikace."""
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
            run_dialog(Message, _(u"Není známa cílová adresa. Je nutno nastavit konfigurační volbu "
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
                    address = run_dialog(InputDialog, prompt=_(u"Vaše e-mailová adresa: "),
                                         value=address, input_width=30,
                                         message=_(u"Pokud svou adresu nastavíte ve formuláři "
                                                   "Nastavení uživatelského rozhraní, nebudete "
                                                   "již příště dotazováni."))
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
                except Exception as e:
                    run_dialog(Error, _(u"Oznámení se nezdařilo odeslat:\n") + unicode(e))
                else:
                    run_dialog(Message, _(u"Oznámení o chybě odesláno"))
    if config.debug_on_error:
        import pdb
        pdb.post_mortem(sys.exc_info()[2])
        return


_last_user_event = None
def last_user_event():
    """Vrať poslední přijatou uživatelskou událost jako instanci 'wx.Event'.

    Před prvním voláním uživatelské události vrať 'None'.
    
    """
    return _last_user_event


def _is_user_event(event):
    # Sem nelze přidat jen tak jakoukoliv událost, protože některé uživatelské
    # události nastávají vesměs během jiných uživatelských událostí, a pak by
    # mohlo dojít k různým nepříjemným efektům.
    if isinstance(event, (wx.KeyEvent, wx.MenuEvent, wx.MouseEvent)):
        return True
    # Instance wxCommandEvent považujeme za user_event, kromě událostí gridu
    # a výběru z menu (např. výběr z popup menu)
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
    """Obal wx callback hlídacím kódem.

    Funkce zavolá 'evt_function' s argumenty 'args'.  Poslední prvek 'args'
    musí být callback ošetřující příslušnou událost.  Tento callback je obalen
    kódem, který zajišťuje ošetření události i v případě, kdy právě probíhá
    zpracování jiné události.

    Příklad typického volání funkce:

      wx_callback(EVT_BUTTON, (self, button.GetId(), self.on_button))

    """
    evt_function_args, callback = args[:-1], args[-1]
    assert isinstance(evt_function, collections.Callable)
    assert isinstance(callback, collections.Callable)
    def process_event(event, callback=callback):
        def system_callback():
            # Při zamykání atd. se využívá toho, že v existují jen dvě vlákna
            # zpracovávající události a že v rámci jednoho vlákna dochází pouze
            # k sekvenčnímu nebo cibulovitému řazení událostí.
            STATE_CURRENT = 'STATE_CURRENT'
            STATE_FREE = 'STATE_FREE'
            STATE_BLOCKED = 'STATE_BLOCKED'
            global _system_callback_thread_ident, _system_callback_lock
            _system_callback_access_lock.acquire()
            try:
                ident = thread.get_ident()
                if _system_callback_thread_ident == ident:
                    # Jsme uvnitř vlastní slupky, jsme v pohodě
                    state = STATE_CURRENT
                elif _system_callback_thread_ident is None and \
                     _system_callback_lock is None:
                    # Nikdo jiný nemá zájem, uzmeme to
                    _system_callback_thread_ident = ident
                    state = STATE_FREE
                else:
                    # Máme konkurenci -- vytvoříme si synchronizační zámek pro
                    # oznámení uvolnění cesty
                    _system_callback_lock = lock = thread.allocate_lock()
                    _system_callback_lock.acquire()
                    state = STATE_BLOCKED
            finally:
                _system_callback_access_lock.release()
            if state == STATE_BLOCKED:
                # Čekáme na uvolnění cesty
                lock.acquire()
                lock.release()
                _system_callback_access_lock.acquire()
                try:
                    # Ještě stále je to náš synchronizační zámek?  Uvolni jej!
                    if _system_callback_lock is lock:
                        _system_callback_lock = None
                    # Teď jsme na koni my
                    _system_callback_thread_ident = ident
                    state = STATE_FREE
                finally:
                    _system_callback_access_lock.release()
            try:
                # To hlavní...
                result = callback(event)
            finally:
                _system_callback_access_lock.acquire()
                try:
                    # Jako první usurpátoři musíme uvolnit informace o své
                    # cibuli ...
                    if state == STATE_FREE:
                        _system_callback_thread_ident = None
                        if _system_callback_lock is not None:
                            while True:
                                try:
                                    # ... a poslat signál případnému čekateli
                                    _system_callback_lock.release()
                                    break
                                except thread.error:
                                    # To je případ, kdy čekatel ještě nestačil
                                    # na svůj zámek zavolat acquire
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
                log(DEBUG, 'Zpracování události:', (event, event.__class__))
        try:
            if thread.get_ident() == _watcher_thread_ident or _current_event:
                # Událost během události
                if _wx_key and _wx_key.is_event_of_key(event, 'Ctrl-g'): # TODO: ne natvr.
                    _interrupted = True
                    result = True
                elif is_user:
                    result = True
                else:
                    result = system_callback()
            elif is_user and modal(top_window()):
                # Událost vyvolaná uživatelským příkazem v modálním okně
                result = callback(event)
            elif is_user:
                # Událost vyvolaná uživatelským příkazem
                _interrupted = False
                _current_event = event
                try:
                    result = callback(event)
                finally:
                    _interrupted = False # událost končí -> nebude co přerušit
                    _current_event = None
                    _last_user_event = event
            else:
                # Standardní "systémová" událost
                result = system_callback()
        except:
            top_level_exception()
            return
        return result
    evt_function(*(evt_function_args + (process_event,)))


def unlock_callbacks():
    """Uvolni uzamčení uživatelských událostí callbackem."""
    global _current_event
    _current_event = None


def yield_():
    """Zkontroluj, zda uživatel nežádá přerušení zpracování události.

    Žádá-li, vyvolej 'UserBreakException'.

    """
    global _interrupted
    if _interrupted and _main_thread_ident == thread.get_ident():
        _interrupted = False
        raise UserBreakException()


def interrupt_watcher():
    """Spusť vlákno sledující wx události během zpracování jiné wx události."""
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
    # Čekání na dokončení inicializace watcheru
    lock.acquire()
    lock.release()
    
def interrupt_init(_main_thread_ident_=thread.get_ident(),
                   _watcher_thread_ident_=None):
    """Inicializuj zpracování přerušení události pro aktuální thread."""
    global _wx_key, _main_thread_ident, _watcher_thread_ident
    _wx_key = WxKey()
    _main_thread_ident = _main_thread_ident_
    _watcher_thread_ident = _watcher_thread_ident_
