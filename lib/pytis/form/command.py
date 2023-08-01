# -*- coding: utf-8 -*-

# Copyright (C) 2018-2025 Tomáš Cerha <t.cerha@gmail.com>
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

"""Command handling extensions specific for the wx application."""

from __future__ import print_function

from future.utils import python_2_unicode_compatible
from future.utils import with_metaclass
from past.builtins import basestring

import wx
import pytis.util
import pytis.presentation

from pytis.presentation import Command, CommandHandlerMetaClass
from pytis.api import app

_ = pytis.util.translations('pytis-wx')


class WxCommandHandlerMetaClass(type(wx.Object), CommandHandlerMetaClass):
    def __init__(cls, name, bases, dict):
        type(wx.Object).__init__(cls, name, bases, dict)
        CommandHandlerMetaClass.__init__(cls, name, bases, dict)


class CommandHandler(with_metaclass(WxCommandHandlerMetaClass, pytis.presentation.CommandHandler)):

    _command_running = False
    _command_counter = pytis.util.Counter()

    def invoke_command(self, command):
        from .event import UserBreakException, top_level_exception
        from .screen import busy_cursor
        CommandHandler._command_counter.next()
        try:
            try:
                CommandHandler._command_running = True
                busy_cursor(True)
                return super(CommandHandler, self).invoke_command(command)
            finally:
                CommandHandler._command_running = False
                busy_cursor(False)
        except UserBreakException:
            pass
        except SystemExit:
            raise
        except Exception:
            top_level_exception()

    @classmethod
    def command_running(cls):
        """Return whether there is a running command.

        Return pair (RUNNING, NUMBER) where RUNNING is a boolean flag
        indicating whether some command is being handled and NUMBER is a
        sequential number of the last handled command.

        """
        return CommandHandler._command_running, CommandHandler._command_counter.current()


class UICommand(object):
    """User interface command specification.

    This class defines concrete command instances available within the user
    interface (as menu items, toolbar buttons) and their presentational
    properties such as title, description or icon which can be displayed in the
    user interface (as menu item title, button tooltip, etc).

    """
    def __init__(self, command, title, descr, icon=None, hotkey=None, ctrl=None):
        """Arguments:

          command -- The Command instance.
          title -- user interface title shown as menu item title, toolbar
            button tooltip, etc.
          descr -- brief description (longer than title) used in status-bar
            help or so.
          icon, hotkey -- currently unused, but planned to replace the
            'DEFAULT_KEYMAP' and 'icons' specifications.
          ctrl -- class of a wx widget representing the command control in the
            user interface (toolbar).  May also be a tuple (ctrl, kwargs),
            where kwargs are passed to ctrl constructor on its creation.  If
            None, the command is represented by a simple button which invokes
            the command when pressed.  If not None, the class must accept two
            positional constructor arguments (parent, uicmd), where parent is
            the parent wx widget (toolbar) and uicmd is this UICommand instance
            (plus any keyword arguments if defined as described above).

        """
        assert isinstance(command, Command), command
        assert isinstance(title, basestring), title
        assert isinstance(descr, basestring), descr
        assert icon is None or isinstance(icon, (int, basestring)), icon
        assert hotkey is None or isinstance(hotkey, basestring), hotkey
        self._command = command
        self._title = title
        self._descr = descr
        self._icon = icon
        self._hotkey = hotkey
        self._ctrl = ctrl

    def command(self):
        return self._command

    def title(self):
        return self._title

    def descr(self):
        return self._descr

    def icon(self):
        return self._icon

    def hotkey(self):
        return self._hotkey

    def ctrl(self):
        return self._ctrl
