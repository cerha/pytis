# -*- coding: utf-8 -*-

# Copyright (C) 2018-2024 Tomáš Cerha <t.cerha@gmail.com>
# Copyright (C) 2002-2013 OUI Technology Ltd.
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

"""Event handling.

The main goal of this module is to allow processing a wx event while another
wx event is being processed. wxWidgets events are blocking: while an event is
being handled, other events must wait until it finishes. This has some
unpleasant consequences, such as the inability to update window contents
incrementally, or the inability for the user to interrupt long-running event
processing. This module tries to work around these issues.

To make the module fulfill its purpose, you must ensure the following:

- All callback bindings (the 'EVT_*' functions) must be performed via
  'wx_callback()'.

- Somewhere, ideally in the top-level UI code, start the watcher thread by
  calling 'interrupt_watcher()'.

- Ensure periodic calls to 'yield_()'. Often it is convenient to use a hook in
  'pytis.util.log()'.

"""

from __future__ import print_function
import sys
import _thread
import time

import wx
import wx.aui

import pytis.form
from pytis.api import app
from pytis.util import DEBUG, OPERATIONAL, format_traceback, log, translations


_ = translations('pytis-wx')


class UserBreakException(Exception):
    """Exception raised when the user interrupts event processing.

    This exception is used solely as a signal that event processing was
    interrupted by the user.

    """
    def __init__(self, *args):
        super(UserBreakException, self).__init__(*args)
        app.message(_("Stopped..."), kind='error')


_current_event = None
_interrupted = False
_main_thread_ident = None
_watcher_thread_ident = None
_wx_key = None


_in_top_level_exception = False


def top_level_exception(einfo=None):
    """Handle the currently raised application exception."""
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
            if pytis.form.app.run_dialog(pytis.form.BugReport, einfo):
                app.exit(force=True)
            if pytis.config.debug_on_error:
                import pdb
                pdb.post_mortem(sys.exc_info()[2])
        finally:
            _in_top_level_exception = False


_last_user_event = None


def last_user_event():
    """Return the last received user event as a 'wx.Event' instance.

    Return None before the first user event is received.

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
    # You cannot just add any event type here, because some user events tend to
    # occur mostly during other user events, which could lead to unpleasant
    # effects.
    if isinstance(event, (wx.KeyEvent, wx.MenuEvent, wx.MouseEvent)):
        return True
    # Treat wx.CommandEvent instances as user events, except for grid events
    # and menu selection events (e.g. selection from a popup menu).
    if isinstance(event, wx.CommandEvent) and \
       not isinstance(event, (wx.grid.GridEvent, wx.UpdateUIEvent)) and \
       event.GetEventType() != wx.wxEVT_COMMAND_MENU_SELECTED:
        return True
    else:
        return False


_system_callback_lock = None
_system_callback_thread_ident = None
_system_callback_access_lock = _thread.allocate_lock()


def wx_callback(event_kind, handler, callback, **kwargs):
    """Wrap wx callback by a guard code.

    The function calls handler.Bind(event_kind, callback, **kwargs).  The
    callback is wrapped by a code which ensures event handling even in case
    that the event ocures during processing another event.

    Typical usage:

      wx_callback(EVT_BUTTON, button, self.on_button)

    """
    assert callable(callback)

    def process_event(event, callback=callback):
        def system_callback():
            # The locking relies on the fact that there are only two threads
            # processing events, and that within a single thread events are
            # nested/stacked in a purely sequential (onion-like) way.
            STATE_CURRENT = 'STATE_CURRENT'
            STATE_FREE = 'STATE_FREE'
            STATE_BLOCKED = 'STATE_BLOCKED'
            global _system_callback_thread_ident, _system_callback_lock
            _system_callback_access_lock.acquire()
            try:
                ident = _thread.get_ident()
                if _system_callback_thread_ident == ident:
                    # We are inside our own "onion layer", we're fine.
                    state = STATE_CURRENT
                elif (_system_callback_thread_ident is None and
                      _system_callback_lock is None):
                    # Nobody else is interested, take it.
                    _system_callback_thread_ident = ident
                    state = STATE_FREE
                else:
                    # We have competition: create a synchronization lock used to
                    # signal that the path has been released.
                    _system_callback_lock = lock = _thread.allocate_lock()
                    _system_callback_lock.acquire()
                    state = STATE_BLOCKED
            finally:
                _system_callback_access_lock.release()
            if state == STATE_BLOCKED:
                # Wait until the path is released.
                lock.acquire()
                lock.release()
                _system_callback_access_lock.acquire()
                try:
                    # Is this still our synchronization lock? Release it.
                    if _system_callback_lock is lock:
                        _system_callback_lock = None
                    # Now it's our turn.
                    _system_callback_thread_ident = ident
                    state = STATE_FREE
                finally:
                    _system_callback_access_lock.release()
            try:
                # The main work...
                result = callback(event)
            finally:
                _system_callback_access_lock.acquire()
                try:
                    # As the first usurper, we must clear the information about
                    # our "onion layer"...
                    if state == STATE_FREE:
                        _system_callback_thread_ident = None
                        if _system_callback_lock is not None:
                            while True:
                                try:
                                    # ... and send a signal to a potential waiter.
                                    _system_callback_lock.release()
                                    break
                                except _thread.error:
                                    # This happens when the waiter has not yet
                                    # managed to call acquire() on its lock.
                                    pass
                finally:
                    _system_callback_access_lock.release()
            return result
        if isinstance(event, wx.IdleEvent) and idle_blocked():
            return
        global _current_event, _interrupted, _last_user_event, _last_user_event_time
        is_user = _is_user_event(event)
        if is_user:
            app.echo('')
        if not isinstance(event, (wx.IdleEvent, wx.UpdateUIEvent)):
            if __debug__:
                log(DEBUG, 'Processing event:', (event, event.__class__))
        try:
            if _thread.get_ident() == _watcher_thread_ident or _current_event:
                # Event during event.
                if _wx_key and _wx_key.is_event_of_key(event, 'Ctrl-g'):  # TODO: avoid hardcoding.
                    _interrupted = True
                    result = True
                elif is_user:
                    result = True
                else:
                    result = system_callback()
            elif is_user and pytis.form.app and pytis.form.modal(pytis.form.app.top_window()):
                # Event triggered by a user command in a modal window.
                result = callback(event)
            elif is_user:
                # Event triggered by a user command.
                _interrupted = False
                _current_event = event
                try:
                    result = callback(event)
                finally:
                    _interrupted = False  # event ends -> nothing to interrupt anymore
                    _current_event = None
                    _last_user_event = event
            else:
                # Standard "system" event.
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
    handler.Bind(event_kind, process_event, **kwargs)


def unlock_callbacks():
    """Release the user event lock held by the callback wrapper."""
    global _current_event
    _current_event = None


def yield_():
    """Check for user break of the event.

    If the user requests a break, raise 'UserBreakException'.

    """
    global _interrupted
    if _interrupted and _main_thread_ident == _thread.get_ident():
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
            answer = app.question(_("Form startup already takes long. "
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
    """Start a thread which watches wx events during processing of another wx event."""
    lock = _thread.allocate_lock()
    lock.acquire()

    def watcher():
        interrupt_init(_watcher_thread_ident_=_thread.get_ident())
        lock.release()
        last_event = None
        import time
        while pytis.form and pytis.form.app is not None:
            time.sleep(0.1)
            if _current_event is not None and _current_event is last_event:
                pytis.form.app.wx_yield(full=True)
            else:
                last_event = _current_event
    _thread.start_new_thread(watcher, ())
    # Wait for watcher initialization to finish.
    lock.acquire()
    lock.release()


def interrupt_init(_main_thread_ident_=_thread.get_ident(),
                   _watcher_thread_ident_=None):
    """Initialize event interruption handling for the current thread."""
    global _wx_key, _main_thread_ident, _watcher_thread_ident
    _wx_key = pytis.form.WxKey()
    _main_thread_ident = _main_thread_ident_
    _watcher_thread_ident = _watcher_thread_ident_
