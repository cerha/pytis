# -*- coding: utf-8 -*-

# Copyright (C) 2018, 2019 Tomáš Cerha <t.cerha@gmail.com>
# Copyright (C) 2002-2013 Brailcom, o.p.s.
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
jiné wx události.  Události ve wxWidgets jsou blokující, během zpracování
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
import sys
import thread
import time

import wx
import wx.aui

import pytis.form
from pytis.util import DEBUG, OPERATIONAL, format_traceback, log, translations


_ = translations('pytis-wx')


class UserBreakException(Exception):
    """Výjimka vyvolávaná při přerušení zpracování události.

    Tato výjimka je používána jednoúčelově pro signalizaci přerušení zpracování
    události uživatelem.

    """
    def __init__(self, *args):
        super(UserBreakException, self).__init__(*args)
        pytis.form.message(_("Stopped..."), beep_=True)


_current_event = None
_interrupted = False
_main_thread_ident = None
_watcher_thread_ident = None
_wx_key = None


_in_top_level_exception = False


def top_level_exception(einfo=None):
    """Zpracuj aktuálně vyvolanou výjimku aplikace."""
    global _in_top_level_exception
    if not _in_top_level_exception:
        _in_top_level_exception = True
        try:
            if not einfo:
                einfo = sys.exc_info()
            if issubclass(einfo[0], SystemExit):
                sys.exit()
            tbstring = format_traceback()
            import cgitb
            try:
                tbstring = cgitb.text(einfo)
            except Exception:
                import traceback
                tbstring = "\n".join(traceback.format_exception(*einfo))
            log(OPERATIONAL, 'Top-level exception caught', tbstring)
            if pytis.form.run_dialog(pytis.form.BugReport, einfo):
                sys.exit()
            if pytis.config.debug_on_error:
                import pdb
                pdb.post_mortem(sys.exc_info()[2])
        finally:
            _in_top_level_exception = False


_last_user_event = None


def last_user_event():
    """Vrať poslední přijatou uživatelskou událost jako instanci 'wx.Event'.

    Před prvním voláním uživatelské události vrať 'None'.

    """
    return _last_user_event


_last_user_event_time = None


def last_event_age():
    """Return the age of the last user event in seconds.

    Return -1 before the first user event is invoked.

    """
    if _last_user_event_time is None:
        return -1
    else:
        return time.time() - _last_user_event_time


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


def wx_callback(event_kind, handler, callback, **kwargs):
    """Obal wx callback hlídacím kódem.

    Funkce zavolá handler.Bind(event_kind, callback, **kwargs).  Callback je obalen
    kódem, který zajišťuje ošetření události i v případě, kdy právě probíhá
    zpracování jiné události.

    Příklad typického volání funkce:

      wx_callback(EVT_BUTTON, button, self.on_button)

    """
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
                elif (_system_callback_thread_ident is None and
                      _system_callback_lock is None):
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
        if isinstance(event, wx.IdleEvent) and idle_blocked():
            return
        global _current_event, _interrupted, _last_user_event, _last_user_event_time
        is_user = _is_user_event(event)
        if is_user:
            pytis.form.message('')
        if not isinstance(event, (wx.IdleEvent, wx.UpdateUIEvent)):
            if __debug__:
                log(DEBUG, 'Processing event:', (event, event.__class__))
        try:
            if thread.get_ident() == _watcher_thread_ident or _current_event:
                # Událost během události
                if _wx_key and _wx_key.is_event_of_key(event, 'Ctrl-g'):  # TODO: ne natvr.
                    _interrupted = True
                    result = True
                elif is_user:
                    result = True
                else:
                    result = system_callback()
            elif is_user and pytis.form.modal(pytis.form.top_window()):
                # Událost vyvolaná uživatelským příkazem v modálním okně
                result = callback(event)
            elif is_user:
                # Událost vyvolaná uživatelským příkazem
                _interrupted = False
                _current_event = event
                try:
                    result = callback(event)
                finally:
                    _interrupted = False  # událost končí -> nebude co přerušit
                    _current_event = None
                    _last_user_event = event
            else:
                # Standardní "systémová" událost
                result = system_callback()
        except SystemExit:
            raise
        except Exception:
            top_level_exception()
            return
        finally:
            if is_user:
                _last_user_event_time = time.time()
        return result
    if event_kind is wx.aui.EVT_AUINOTEBOOK_PAGE_CHANGING:
        # Hack: This particular event kind needs to be bound using the EVT_* call,
        # otherwise sideform tabs don't resize the contained form properly when
        # switching to another tab (start a multiuform and switch to another tab).
        # Strangely, this doesn't produce the depracation warning as the other events.
        event_kind(handler, handler.GetId(), process_event)
    else:
        handler.Bind(event_kind, process_event, **kwargs)


def unlock_callbacks():
    """Uvolni uzamčení uživatelských událostí callbackem."""
    global _current_event
    _current_event = None


def yield_():
    """Check for user break of the event.

    If the user requests a break, raise 'UserBreakException'.

    """
    global _interrupted
    if _interrupted and _main_thread_ident == thread.get_ident():
        _interrupted = False
        raise UserBreakException()


_idle_blocked = False


def idle_blocked():
    """Return whether idle events are blocked."""
    # We used to check for busy_cursor here as well.
    # But that didn't work: Message dialog didn't show its content.  Apparently
    # pytis dialogs depend on _on_idle action invocation, while the cursor is
    # set to busy, to finish their construction.
    return _idle_blocked


def block_idle(block):
    """Block or unblock idle events.

    Normally, idle event handlers should implement their own reasonable checks
    whether to run idle actions or not, e.g. by checking for busy cursor.  But
    in some rare cases it is necessary to block idle events completely.  In
    such a case you can use this method; but don't forget to unblock events in
    finally part!

    Arguments:

      block -- flag indicating whether to block (true) or unblock (false) idle
        events

    """
    global _idle_blocked
    _idle_blocked = block


_stop_check_running = False


def _stop_check(start_time, confirmed, command_number):
    global _stop_check_running
    if _stop_check_running:
        return False
    from pytis.form import CommandHandler
    running, number = CommandHandler.command_running()
    if not running or number != command_number:
        return False
    if not confirmed and time.time() > start_time + pytis.config.run_form_timeout:
        # We must block idle events before running the question dialog.  This
        # prevents recursive stop check dialog invocation from an idle method,
        # followed by a traceback.  Additionally stop check is typically
        # invoked during command processing when idle events shouldn't be run
        # yet.
        block_idle(True)
        _stop_check_running = True
        try:
            answer = pytis.form.run_dialog(pytis.form.Question,
                                           _("Form startup already takes long. "
                                             "Should it be interrupted?"))
        finally:
            _stop_check_running = False
            block_idle(False)
        if answer:
            raise UserBreakException()
        else:
            confirmed = True
    else:
        # Allow user break by Ctrl-g.  This has actually no real effect, mainly
        # because wxWidgets apparently doesn't pass key events until the run
        # form event finishes.  But maybe we find a workaround sometimes...
        yield_()
    return confirmed


def standard_stop_check_function():
    """Return standard stop check function.

    The returned function is suitable as 'stop_check' argument value in
    database 'select' operations.  It raises 'UserBreakException' when
    'run_form_timeout' given in configuration gets exceeded.

    """
    confirmed = [False]
    __, command_number = pytis.form.CommandHandler.command_running()

    def maybe_stop_check(start_time):
        confirmed[0] = _stop_check(start_time, confirmed[0], command_number)
    return maybe_stop_check


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
                pytis.form.wx_yield_(full=True)
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
    _wx_key = pytis.form.WxKey()
    _main_thread_ident = _main_thread_ident_
    _watcher_thread_ident = _watcher_thread_ident_
