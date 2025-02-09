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

"""Třídy sloužící k definici příkazů.

Tento modul definuje třídy sloužící k definici a zpracování příkazů.  Vlastní
definice všech dostupných příkazů aplikace je potom soustředěna centrálně v
modulu 'commands_'.

"""
from __future__ import print_function
from future.utils import python_2_unicode_compatible

from past.builtins import basestring
import pytis.util
from pytis.api import app

_ = pytis.util.translations('pytis-wx')


class CommandHandler(object):
    """Mix-in class for objects capable of handling user commands.

    This class adds the ability to define user commands and process them.

    Classes derived from this class are capable of handling user commands
    defined using the 'Command' class.  New commands are defined by command
    handler classes (classes derived from 'CommandHandler') and handled by
    their instances.  To locate the handler instance, the derived classes must
    implement the method '_get_command_handler_instance()'.  The handler
    instance is then queried for availability of the particular command using
    the 'can_command()' instance method and if the command is available, it may
    be invoked on the instance using the 'on_command()' instance method.

    The methods 'command_enabled()' and 'invoke_command()' are counterparts of
    the methods 'can_command()' and 'on_command(), but they are class methods
    and are capable to locate the active instance and call the instance method
    on it.

    """

    _command_running = False
    _command_counter = pytis.util.Counter()

    @classmethod
    def _get_command_handler_instance(cls):
        """Find an active CommandHandler instance capable to handle commands.

        Commands are bound to command handler classes, but are invoked on their
        instances.  This method is used to find an active instance of given
        command handler class.

        Each class derived from CommandHandler must define this method and
        return an instance of itself which is currently active to process its
        commands or None if no such instance is active in the application.

        """
        raise pytis.util.ProgramError("This method must be overriden in a derived class.")

    @classmethod
    def _command_handler(cls, command, _command_handler=None, **kwargs):
        if _command_handler is not None:
            handler = _command_handler
        else:
            handler = cls._get_command_handler_instance()
        if not isinstance(handler, cls):
            handler = None
        return handler, kwargs

    @classmethod
    def command_enabled(cls, command, **kwargs):
        """Return true if given command is active (can be invoked).

        The commands which are not compatible with the active object in the
        application ('CommandHandler' instance) are automatically inactive.  If
        there is an active handler compatible with the command (the command is
        defined by its 'CommandHandler' class), the command availability is
        determined by calling the method 'can_command()' of the instance (which
        normally futher calls a method '_can_<command-name>()' matching the
        command name).

        """
        handler, kwargs = cls._command_handler(command, **kwargs)
        if handler is None:
            return False
        if __debug__:
            name = 'COMMAND_' + command.name()
            assert hasattr(handler, name) and getattr(handler, name) == command,\
                "Invalid command '%s' for %s" % (name, handler)
        return handler.can_command(command, **kwargs)

    @classmethod
    def invoke_command(cls, command, **kwargs):
        """Invoke given comand in the currently active command handler instance.

        If there is an active handler compatible with the command (the command
        is defined by its 'CommandHandler' class), the command is invoked by
        calling the method 'on_command()' of the instance (which normally
        futher calls a method '_cmd_<command-name>()' matching the command
        name).

        Returns: The return value of the command handler method.

        """
        from .event import UserBreakException, top_level_exception
        from .screen import busy_cursor
        handler, kwargs = cls._command_handler(command, **kwargs)
        CommandHandler._command_counter.next()
        try:
            try:
                CommandHandler._command_running = True
                busy_cursor(True)
                return handler.on_command(command, **kwargs)
            finally:
                CommandHandler._command_running = False
                busy_cursor(False)
        except UserBreakException:
            pass
        except SystemExit:
            raise
        except Exception:
            top_level_exception()

    def on_command(self, command, **kwargs):
        """Zpracuj příkaz 'command' s parametry 'kwargs'.

        Argumenty:

          command -- instance 'Command'.

          kwargs -- argumenty, se kterámi byl příkaz 'command' vyvolán.

        Obsluždá rutina příkazu je nalezena automaticky podle názvu příkazu.
        Napříkad pro příkaz 'Application.COMMAND_RUN_FORM' je hledána metoda
        'Application._cmd_run_form()'.  Této metodě jsou předány všechny
        argumenty, se kterými byl příkaz vyvolán.

        Každá třída, pro kterou jsou definovány příkazy, by tak měla definovat
        všechny odpovídající obslužné rutiny.  Druhou možností je předefinování
        této metody a implementace vlastního mechanismu zpracování příkazů.

        Vrací: Návratovou hodnotu obslužné rutiny daného příkazu.

        """
        handler = getattr(self, '_cmd_' + command.name().lower())
        return handler(**kwargs)

    def can_command(self, command, **kwargs):
        """Vrať pravdu, pokud je příkaz aktivní a může být proveden.

        Pokud je příkaz aktivní, znamená to, že jeho provedení má v daném
        kontextu smysl, uživatel má dostatečná přístupová práva atd.

        Příkazy, pro něž je definována metoda 'can_<command_name>' a ta vrátí
        False, jsou neaktivní.

        """
        can_method_name = '_can_' + command.name().lower()
        if hasattr(self, can_method_name):
            can = getattr(self, can_method_name)
            if not can(**kwargs):
                return False
        return True

    @classmethod
    def command_running(cls):
        """Return whether there is a running command.

        Return pair (RUNNING, NUMBER) where RUNNING is a boolean flag
        indicating whether some command is being handled and NUMBER is a
        sequential number of the last handled command.

        """
        return CommandHandler._command_running, CommandHandler._command_counter.current()


@python_2_unicode_compatible
class Command(object):
    """Reprezentace obecného příkazu uživatelského rozhraní.

    Každý příkaz je vázán na určitý typ prvku uživatelského rozhraní aplikace
    (formulář, dialog, vstupní políčko), nad jehož instancí může být vyvolán.
    Třída každého takového prvku uživatelského rozhraní, která chce vlastní
    příkazy definovat, musí být odvozena od třídy 'CommandHandler'.

    """

    _commands = {}

    @classmethod
    def command(class_, name):
        """Return command instance named 'name'.

        If there is no command with that name, return 'None'.

        Arguments:

          name -- name of the command, string

        """
        return class_._commands.get(name)

    def __init__(self, handler, name, doc=None, log_=True):
        """Definuj příkaz.

        Argumenty:

          handler -- Třída prvku uživatelského rozhraní, který příkaz
            zpracovává.  Třída musí být potomkem třídy 'CommandHandler'.  Více
            také viz. výše (dokumentace třídy 'Command').
          name -- název příkazu.  Neprázdný řetězec, použitelný jako Pythonový
            identifikáítor, mezi názvy příkazů unikátní.  Název je použit pro
            vytvoření konstanty (viz. níže), takže dalším požadavkem je, aby
            veškerá písmena byla velká.
          doc -- dokumentační řetězec příkazu.  Stručný popis z pohledu vývojáře
            (ne pro zobrazení v uživatelském rozhraní).
          log_ -- právě když je pravdivé, je vyvolání příkazu logováno jako
            EVENT, jinak je logováno pouze jako DEBUG

        Po definici příkazu je každý příkaz automaticky dostupný jako veřejná
        konstanta své obslužné třídy (dané argumentem 'handler').  Název této
        konstanty je vždy COMMAND_ + 'name' ('name' je název příkazu zadaný v
        konstruktoru).  Naříklad tedy 'Application.COMMAND_EXIT', nebo
        'LookupForm.COMMAND_SORT'.

        """
        assert issubclass(handler, CommandHandler), handler
        assert isinstance(name, basestring) and name == name.upper(), (name, type(name))
        assert doc is None or isinstance(doc, basestring)
        self._handler = handler
        self._name = name
        self._doc = doc
        self._id = '.'.join((handler.__name__, name.lower().replace('_', '-')))
        self._log = log_
        assert not hasattr(handler, 'COMMAND_' + name), \
            "Command '%s' already defined for %s" % (name, handler.__name__)
        setattr(handler, 'COMMAND_' + name, self)
        Command._commands[name] = self

    def __str__(self):
        return '<Command: %s>' % (self._id,)

    def __hash__(self):
        return hash(self._id)

    def __eq__(self, other):
        if pytis.util.sameclass(self, other):
            return self._id == other._id
        else:
            return NotImplemented

    def __ne__(self, other):
        # Implied automatically in Python 3 so can be removed when dropping Python 2 support.
        return not self == other

    def __call__(self, **kwargs):
        """Umožňuje pohodlně vytvořit definici příkazu a jeho argumentů.

        Vrací dvojici (COMMAND, ARGS), kde COMMAND je instance příkazu a ARGS
        jsou jeho argumenty jako slovník.

        Této vlastnosti lze využít například pro zjednodušení zápisu
        klávesových map apod., kde příkaz a jeho argumenty tvoří nedílnou
        dvojici.

        """
        return (self, kwargs)

    def handler(self):
        """Vrať třídu uživatelského rozhraní, která tento příkaz zpracovává."""
        return self._handler

    def name(self):
        """Vrať název příkazu zadaný v konstruktoru."""
        return self._name

    def id(self):
        """Vrať identifikátor příkazu jako řetězec.

        Identifikátor je vhodný např. pro logování.  Příkazy jsou rozpoznávány
        dle konkrétních instancí, ne podle svého identifikátoru.

        """
        return self._id

    def doc(self):
        """Vrať dokumentační řetězec příkazu jako string, nebo None."""
        return self._doc

    def enabled(self, **kwargs):
        """Vrať pravdu, pokud je příkaz aktivní (smí být vyvolán).

        Zjištění dostupnosti příkazu je ponecháno na metodě 'command_enabled'
        třídy 'CommandHandler' pro kterou je tento příkaz definován.

        """
        return self._handler.command_enabled(self, **kwargs)

    def invoke(self, **kwargs):
        """Vyvolej v aplikaci zpracování příkazu s danými argumenty."""
        if self.enabled(**kwargs):
            if self._log:
                kind = pytis.util.EVENT
            else:
                kind = pytis.util.DEBUG
            pytis.util.log(kind, 'Invoking command:', (self, kwargs))
            return self._handler.invoke_command(self, **kwargs)
        else:
            app.echo(_(u"Command invocation refused: %s") % (self.id(),), kind='error')
            return False

class UICommand(object):
    """User interface command specification.

    This class defines concrete command variants available within the user interface (as menu
    items, toolbar buttons).  Technically, 'Command' instances can be invoked with arguments, but
    'UICommand' instance is a definition of a command and its arguments.  Each combination of a
    command and its arguments is accompanied by the corresponding title and description which can
    be displayed in the user interface (as a button tooltip, menu item title, etc).

    """
    def __init__(self, cmd, title, descr, icon=None, hotkey=None, ctrl=None):
        """Arguments:

          cmd -- The (command, args) pair as produced by calling a command instance.
          title -- user interface title shown as menu item title, toolbar button tooltip, etc.
          descr -- brief description (longer than title) used in status-bar help or so.
          icon, hotkey -- currently unused, but planned to replace the 'DEFAULT_KEYMAP' and
            'COMMAND_ICONS' specifications.
          ctrl -- class of a wx widget representing the command control in the
            user interface (toolbar).  May also be a tuple (ctrl, kwargs),
            where kwargs are passed to ctrl constructor on its creation.  If
            None, the command is represented by a simple button which invokes
            the command when pressed.  If not None, the class must accept two
            positional constructor arguments (parent, uicmd), where parent is
            the parent wx widget (toolbar) and uicmd is this UICommand instance
            (plus any keyword arguments if defined as described above).

        """
        command, args = cmd
        assert isinstance(command, Command), command
        assert isinstance(args, dict), args
        assert isinstance(title, basestring), title
        assert isinstance(descr, basestring), descr
        assert icon is None or isinstance(icon, (int, basestring)), icon
        assert hotkey is None or isinstance(hotkey, basestring), hotkey
        self._command = command
        self._args = args
        self._title = title
        self._descr = descr
        self._icon = icon
        self._hotkey = hotkey
        self._ctrl = ctrl

    def command(self):
        return self._command

    def args(self):
        return self._args

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

    def clone(self, **kwargs):
        """Return the same 'UICommand' instance with command arguments overriden by 'kwargs'."""
        return UICommand((self._command, dict(self._args, **kwargs)), self._title, self._descr,
                         icon=self._icon, hotkey=self._hotkey)


_command_icons = None


def command_icon(command, args):
    """Return the icon identifier for given command and its arguments.

    Arguments:

      command -- 'Command' instance.
      args -- command arguemnts as a dictionary.

    The icon which best matches with given command and arguments is searched
    within 'COMMAND_ICONS' specification (see the 'commands_' module).

    The returned value is the icon identifier as accepted by 'get_icon()'.

    """
    global _command_icons
    if _command_icons is None:
        from .commands_ import COMMAND_ICONS
        _command_icons = {}
        for item, icon in COMMAND_ICONS:
            if isinstance(item, tuple):
                cmd, iargs = item
            else:
                cmd, iargs = item, {}
            icons = _command_icons[cmd] = _command_icons.get(cmd, [])
            icons.append((iargs, icon))

    try:
        icons = _command_icons[command]
    except KeyError:
        pass
    else:
        for iargs, icon in icons:
            for k, v in iargs.items():
                if k not in args or args[k] != v:
                    break
            else:
                return icon
    if command.name() == 'CONTEXT_ACTION' and args['action'].context() == 'SELECTION':
        # Use 'selection' icon as the default for actions which operate on selection
        # in order to distingush these actions for the user.  This is not perfect, as
        # the distinction will disapear for actions which define their icons, but
        # icons are actually asigned quite rarely so it mostly works in practice.
        return 'selection'
    return None
