# -*- coding: iso-8859-2 -*-

# Definice u�ivatelsk�ch p��kaz�
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

"""T��dy slou��c� k�definici p��kaz�.

Tento modul definuje t��dy slou��c� k definici a zpracov�n� p��kaz�.  Vlastn�
definice v�ech dostupn�ch p��kaz� aplikace je potom soust�ed�na centr�ln� v
modulu 'commands_'.

"""

from pytis.form import *

class CommandHandler:
    """Mix-in t��da, kterou mus� d�dit t��dy definuj�c� vlastn� p��kazy.

    Tato t��da p�id�v� schopnost zpracovat p��kazy (instance 'Command') a
    zjistit, zda je konkr�tn� p��kaz v danou chv�li dostupn�.

    TODO: Doplnit p�ehled ��elu jednotliv�ch metod a zp�sobu vyhled�n� instance
    handleru.

    """
    
    @classmethod
    def _get_command_handler_instance(cls):
        """Najdi v aplikaci aktivn� prvek, kter� je schopen zpracovat p��kaz."""
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
        """Vra� pravdu, pokud je dan� p��kaz aktivn� (sm� b�t vyvol�n).
        
        P��kazy, kter� nejsou kompatibiln� s aktivn�m prvkem aplikace (instanc�
        'CommandHandler') jsou automaticky neaktivn�.  Pokud je kompatibiln�
        instance 'CommandHandler' nalezena, je dostupnost p��kazu d�le
        vyhodnocena vol�n�m metody 'can_command' t�to instance.

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
        """Vyhledej instanci handleru p��kazu a p��kaz prove�.

        Vrac�: N�vratovou hodnotu obslu�n� rutiny dan�ho p��kazu.

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
        id = wx.NewId()
        icon = get_icon(command_icon(cmd, kwargs), type=wx.ART_TOOLBAR)
        tool = toolbar.AddTool(id, icon,
                               shortHelpString=uicmd.title(),
                               longHelpString=uicmd.descr())
        frame = toolbar.GetParent()
        wx_callback(wx.EVT_TOOL, frame, id, lambda e: cmd.invoke(**kwargs))
        wx_callback(wx.EVT_UPDATE_UI, frame, id, lambda e: e.Enable(cmd.enabled(**kwargs)))
    
    def on_command(self, command, **kwargs):
        """Zpracuj p��kaz 'command' s�parametry 'kwargs'.

        Argumenty:

          command -- instance 'Command'.

          kwargs -- argumenty, se kter�mi byl p��kaz 'command' vyvol�n.

        Obslu�d� rutina p��kazu je nalezena automaticky podle n�zvu p��kazu.
        Nap��kad pro p��kaz 'Application.COMMAND_RUN_FORM' je hled�na metoda
        'Application._cmd_run_form()'.  T�to metod� jsou p�ed�ny v�echny
        argumenty, se kter�mi byl p��kaz vyvol�n.

        Ka�d� t��da, pro kterou jsou definov�ny p��kazy, by tak m�la definovat
        v�echny odpov�daj�c� obslu�n� rutiny.  Druhou mo�nost� je p�edefinov�n�
        t�to metody a implementace vlastn�ho mechanismu zpracov�n� p��kaz�.
        
        Vrac�: N�vratovou hodnotu obslu�n� rutiny dan�ho p��kazu.

        """
        handler = getattr(self, '_cmd_' + command.name().lower())
        return handler(**kwargs)

    def can_command(self, command, **kwargs):
        """Vra� pravdu, pokud je p��kaz aktivn� a m��e b�t proveden.

        Pokud je p��kaz aktivn�, znamen� to, �e jeho proveden� m� v dan�m
        kontextu smysl, u�ivatel m� dostate�n� p��stupov� pr�va atd.

        P��kazy, pro n� je definov�na metoda 'can_<command_name>' a ta vr�t�
        False, jsou neaktivn�.

        """
        can_method_name = '_can_' + command.name().lower()
        if hasattr(self, can_method_name):
            can = getattr(self, can_method_name)
            if not can(**kwargs):
                return False
        return True




    
class Command(object):
    """Reprezentace obecn�ho p��kazu u�ivatelsk�ho rozhran�.

    Ka�d� p��kaz je v�z�n na ur�it� typ prvku u�ivatelsk�ho rozhran� aplikace
    (formul��, dialog, vstupn� pol��ko), nad jeho� instanc� m��e b�t vyvol�n.
    T��da ka�d�ho takov�ho prvku u�ivatelsk�ho rozhran�, kter� chce vlastn�
    p��kazy definovat, mus� b�t odvozena od t��dy 'CommandHandler'.

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
        """Definuj p��kaz.

        Argumenty:

          handler -- T��da prvku u�ivatelsk�ho rozhran�, kter� p��kaz
            zpracov�v�.  T��da mus� b�t potomkem t��dy 'CommandHandler'.  V�ce
            tak� viz. v��e (dokumentace t��dy 'Command').
          name -- n�zev p��kazu.  Nepr�zdn� �et�zec, pou�iteln� jako Pythonov�
            identifik��tor, mezi n�zvy p��kaz� unik�tn�.  N�zev je pou�it pro
            vytvo�en� konstanty (viz. n�e), tak�e dal��m po�adavkem je, aby
            ve�ker� p�smena byla velk�.
          doc -- dokumenta�n� �et�zec p��kazu.  Stru�n� popis z pohledu v�voj��e
            (ne pro zobrazen� v u�ivatelsk�m rozhran�).
          log_ -- pr�v� kdy� je pravdiv�, je vyvol�n� p��kazu logov�no jako
            EVENT, jinak je logov�no pouze jako DEBUG

        Po definici p��kazu je ka�d� p��kaz automaticky dostupn� jako ve�ejn�
        konstanta sv� obslu�n� t��dy (dan� argumentem 'handler').  N�zev t�to
        konstanty je v�dy COMMAND_ + 'name' ('name' je n�zev p��kazu zadan� v
        konstruktoru).  Na��klad tedy 'Application.COMMAND_EXIT', nebo
        'LookupForm.COMMAND_SORT'.

        """
        assert issubclass(handler, CommandHandler), \
               "Not a CommandHandler subclass: %s" % handler
        assert isinstance(name, types.StringType) and name == name.upper(), \
               (name, type(name))
        assert doc is None or isinstance(doc, types.StringTypes)
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
        """Umo��uje pohodln� vytvo�it definici p��kazu a jeho argument�.

        Vrac� dvojici (COMMAND, ARGS), kde COMMAND je instance p��kazu a ARGS
        jsou jeho argumenty jako slovn�k.

        T�to vlastnosti lze vyu��t nap��klad pro zjednodu�en� z�pisu
        kl�vesov�ch map apod., kde p��kaz a jeho argumenty tvo�� ned�lnou
        dvojici.

        """
        return (self, kwargs)
        
    def handler(self):
        """Vra� t��du u�ivatelsk�ho rozhran�, kter� tento p��kaz zpracov�v�."""
        return self._handler

    def name(self):
        """Vra� n�zev p��kazu zadan� v konstruktoru."""
        return self._name
    
    def id(self):
        """Vra� identifik�tor p��kazu jako �et�zec.

        Identifik�tor je vhodn� nap�. pro logov�n�.  P��kazy jsou rozpozn�v�ny
        dle konkr�tn�ch instanc�, ne podle sv�ho identifik�toru.
    
        """
        return self._id

    def doc(self):
        """Vra� dokumenta�n� �et�zec p��kazu jako string, nebo None."""
        return self._doc
    
    def enabled(self, **kwargs):
        """Vra� pravdu, pokud je p��kaz aktivn� (sm� b�t vyvol�n).

        Zji�t�n� dostupnosti p��kazu je ponech�no na metod� 'command_enabled'
        t��dy 'CommandHandler' pro kterou je tento p��kaz definov�n.

        """
        return self._handler.command_enabled(self, **kwargs)

    def invoke(self, **kwargs):
        """Vyvolej v aplikaci zpracov�n� p��kazu s dan�mi argumenty."""
        if self.enabled(**kwargs):
            if self._log:
                kind = EVENT
            else:
                kind = DEBUG
            log(kind, 'Vyvol�n p��kaz:', (self, kwargs))
            return self._handler.invoke_command(self, **kwargs)
        else:
            message(_("Vyvol�n� p��kazu zam�tnuto: %s") % self.id(), beep_=True)
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
    def __init__(self, cmd, title, descr, icon=None, hotkey=None):
        """Arguments:

          cmd -- The (command, args) pair as produced by calling a command instance.
          title -- user interface title shown as menu item title, toolbar button tooltip, etc.
          descr -- brief description (longer than title) used in status-bar help or so.
          
          icon, hotkey -- currently unused, but planned to replace the 'DEFAULT_KEYMAP' and
            'COMMAND_ICONS' specifications.

        """
        command, args = cmd
        assert isinstance(command, Command), command
        assert isinstance(args, dict), args
        assert isinstance(title, (str, unicode)), title
        assert isinstance(descr, (str, unicode)), descr
        assert icon is None or isinstance(icon, (int, str)), icon
        assert hotkey is None or isinstance(hotkey, str), hotkey
        self._command = command
        self._args = args
        self._title = title
        self._descr = descr
        self._icon = icon
        self._hotkey = hotkey

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
