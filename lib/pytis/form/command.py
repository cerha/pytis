# -*- coding: iso-8859-2 -*-

# Definice u�ivatelsk�ch p��kaz�
# 
# Copyright (C) 2002, 2003, 2004, 2005, 2006 Brailcom, o.p.s.
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

    TODO: Doplnit p�ehled ��elu jednotliv�ch metod.

    """
    
    def _get_command_handler_instance(cls):
        """Najdi v aplikaci aktivn� prvek, kter� je schopen zpracovat p��kaz."""
        raise ProgramError("This method must be overriden in derived class.")
    _get_command_handler_instance = classmethod(_get_command_handler_instance)

    def _command_handler(cls, command, _command_handler=None, **kwargs):
        if _command_handler is not None:
            handler = _command_handler
        else:
            handler = cls._get_command_handler_instance()
        assert handler is None or isinstance(handler, cls), \
               (str(command), handler, cls)
        return handler, kwargs
    _command_handler = classmethod(_command_handler)
    
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
    command_enabled = classmethod(command_enabled)

    def invoke_command(cls, command, **kwargs):
        """Vyhledej instanci handleru p��kazu a p��kaz prove�.

        Vrac� pravdu, pokud je handler nalezen, p��kaz je zpracov�n a nemaj�
        tedy ji� b�t prov�d�ny dal�� pokusy o�jeho zpracov�n�.

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
    invoke_command = classmethod(invoke_command)

    def on_command(self, command, **kwargs):
        """Zpracuj p��kaz 'command' s�parametry 'kwargs'.

        Argumenty:

          command -- instance 'Command'
          kwargs -- argumenty p��kazu 'command'

        Vrac�: Pravdu, pr�v� kdy� p��kaz byl zpracov�n a nemaj� b�t ji�
        prov�d�ny dal�� pokusy o�jeho zpracov�n�.

        V�t�to t��d� metoda ned�l� nic a vrac� False.

        V�ka�d� odvozen� t��d�, kter� definuje vlastn� p��kazy, by tato metoda
        m�la b�t p�edefinov�na a m�la by o�et�ovat v�echny tyto p��kazy.  

        """
        return False

    def can_command(self, command, **kwargs):
        """Vra� pravdu, pokud je p��kaz aktivn� a m��e b�t proveden.

        Pokud je p��kaz aktivn�, znamen� to, �e jeho proveden� m� v dan�m
        kontextu smysl, u�ivatel m� dostate�n� p��stupov� pr�va atd.

        P��kazy, pro n� je definov�na metoda 'can_<command_name>' a ta vr�t�
        False, jsou neaktivn�.

        """
        can_method_name = 'can_' + command.name().lower()
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
          doc -- dokumenta�n� �et�zec p��kazu.  Stru�n� popis, kter� m��e b�t
            nap�. zobrazen v u�ivatelsk�m rozhran�.
          log_ -- pr�v� kdy� je pravdiv�, je vyvol�n� p��kazu logov�no jako
            EVENT, jinak je logov�no pouze jako DEBUG

        Po definici p��kazu je ka�d� p��kaz automaticky dostupn� jako ve�ejn�
        konstanta sv� obslu�n� t��dy (dan� argumentem 'handler').  N�zev t�to
        konstanty je v�dy COMMAND_ + 'name' ('name' je n�zev p��kazu zadan� v
        konstruktoru).  Na��klad tedy 'Application.COMMAND_EXIT', nebo
        'LookupForm.COMMAND_SORT_COLUMN'.

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
            log(EVENT, 'Zam�tnuto vyvol�n� p��kazu:', (self, kwargs))
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

            
        
def invoke_command(command, **kwargs):
    """Vyvolej glob�ln� zpracov�n� p��kazu 'command'.

    Argumenty:

      command -- instance t��dy 'Command'
      kwargs -- parametry p��kazu

    """
    return command.invoke(**kwargs)
