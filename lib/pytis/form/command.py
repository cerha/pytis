# -*- coding: utf-8 -*-

# Definice uživatelských příkazů
# 
# Copyright (C) 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2011 Brailcom, o.p.s.
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

"""Třídy sloužící k definici příkazů.

Tento modul definuje třídy sloužící k definici a zpracování příkazů.  Vlastní
definice všech dostupných příkazů aplikace je potom soustředěna centrálně v
modulu 'commands_'.

"""

from pytis.form import *

class CommandHandler:
    """Mix-in třída, kterou musí dědit třídy definující vlastní příkazy.

    Tato třída přidává schopnost zpracovat příkazy (instance 'Command') a
    zjistit, zda je konkrétní příkaz v danou chvíli dostupný.

    TODO: Doplnit přehled účelu jednotlivých metod a způsobu vyhledání instance
    handleru.

    """
    
    @classmethod
    def _get_command_handler_instance(cls):
        """Najdi v aplikaci aktivní prvek, který je schopen zpracovat příkaz."""
        raise ProgramError("This method must be overriden in a derived class.")

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
        """Vrať pravdu, pokud je daný příkaz aktivní (smí být vyvolán).
        
        Příkazy, které nejsou kompatibilní s aktivním prvkem aplikace (instancí
        'CommandHandler') jsou automaticky neaktivní.  Pokud je kompatibilní
        instance 'CommandHandler' nalezena, je dostupnost příkazu dále
        vyhodnocena voláním metody 'can_command' této instance.

        """
        handler, kwargs = cls._command_handler(command, **kwargs)
        if handler is None:
            return False
        if __debug__:
            name = 'COMMAND_' + command.name()
            assert hasattr(handler,name) and getattr(handler,name) == command,\
                   "Invalid command '%s' for %s" % (name, handler)
        return handler.can_command(command, **kwargs)

    @classmethod
    def invoke_command(cls, command, **kwargs):
        """Vyhledej instanci handleru příkazu a příkaz proveď.

        Vrací: Návratovou hodnotu obslužné rutiny daného příkazu.

        """
        handler, kwargs = cls._command_handler(command, **kwargs)
        try:
            try:
                busy_cursor(True)
                return handler.on_command(command, **kwargs)
            finally:
                busy_cursor(False)
        except UserBreakException:
            pass
        except:
            top_level_exception()

    @classmethod
    def add_toolbar_ctrl(cls, toolbar, uicmd):
        """Add a toolbar control for given 'uicmd' into 'toolbar'.

        This method adds a default command control into the toolbar.  The default control is a
        simple button which invokes the command on click.  Derived classes may override this method
        to create some more sophisticated controls for their specific commands.

        """
        cmd, kwargs = uicmd.command(), uicmd.args()
        ctrl_cls = uicmd.ctrl()
        if ctrl_cls:
            if isinstance(ctrl_cls, tuple):
                ctrl_cls, ctrl_kwargs = ctrl_cls
            else:
                ctrl_kwargs = {}
            ctrl = ctrl_cls(toolbar, uicmd, **ctrl_kwargs)
            ctrl.SetToolTipString(uicmd.title())
            tool = toolbar.AddControl(ctrl)
            toolbar.SetToolLongHelp(tool.GetId(), uicmd.descr()) # Doesn't work...
        else:
            assigned_icon = command_icon(cmd, kwargs)
            if assigned_icon is None:
                raise Exception("No icon assigned for command %s %s." % (cmd, kwargs))
            icon = get_icon(assigned_icon, type=wx.ART_TOOLBAR)
            tool = toolbar.AddTool(-1, icon,
                                   shortHelpString=uicmd.title(),
                                   longHelpString=uicmd.descr())
            parent = toolbar.GetParent()
            wx_callback(wx.EVT_TOOL, parent, tool.GetId(), lambda e: cmd.invoke(**kwargs))
            wx_callback(wx.EVT_UPDATE_UI, parent, tool.GetId(), lambda e: e.Enable(cmd.enabled(**kwargs)))

        
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
        assert issubclass(handler, CommandHandler), \
               "Not a CommandHandler subclass: %s" % handler
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
                kind = EVENT
            else:
                kind = DEBUG
            log(kind, 'Vyvolán příkaz:', (self, kwargs))
            return self._handler.invoke_command(self, **kwargs)
        else:
            message(_(u"Vyvolání příkazu zamítnuto: %s") % self.id(), beep_=True)
            return False
    
    def __cmp__(self, other):
        if sameclass(self, other):
            if self._id == other._id:
                return 0
            elif self._id < other._id:
                return -1
            else:
                return 1
        else:
            return compare_objects(self, other)
        
    def __str__(self):
        return '<Command: %s>' % self._id


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
    return None
