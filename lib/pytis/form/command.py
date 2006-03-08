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

Tento modul definuje t��dy slou��c� k definici p��kaz�.  Vlastn� definice v�ech
p��kaz� aplikace je potom soust�ed�na centr�ln� v modulu 'commands_'.

"""

from pytis.form import *


class CommandHandler:
    """Mix-in t��da, kterou mus� d�dit t��dy definuj�c� vlastn� p��kazy."""
    
    def get_command_handler_instance(cls, application):
        """Najdi v aplikaci aktivn� prvek, kter� je schopen zpracovat p��kaz."""
        raise ProgramError("This method must be overriden in derived class.")
    get_command_handler_instance = classmethod(get_command_handler_instance)
                           
    
class Command(object):
    """Reprezentace obecn�ho p��kazu u�ivatelsk�ho rozhran�.

    Ka�d� p��kaz je v�z�n na ur�it� typ prvku u�ivatelsk�ho rozhran� aplikace
    (formul��, dialog, vstupn� pol��ko), nad jeho� instanc� m��e b�t vyvol�n.
    T��da ka�d�ho takov�ho prvku u�ivatelsk�ho rozhran�, kter� chce vlastn�
    p��kazy definovat, mus� b�t odvozena od t��dy 'CommandHandler'.

    Metoda 'enabled()' ka�d�ho p��kazu potom nejprve zjist�, zda je aktivn�
    prvek aplikace (instance 'CommandHandler') kompatibiln� s dan�m p��kazem
    (p��kaz byl definov�n pro jeho t��du).  Pokud ne, metoda vrac� v�dy false
    bez testov�n� hodnoty, dan� arguemntem 'enabled' konstruktoru.  T�m jsou v
    d�sledku automaticky zneaktivn�ny i p��slu�n� polo�ky menu atd.

    Obslu�n� t��da (resp. jej� instance) ka�d� p��kaz zpracuje bu�to sama
    (p��kazy definovan� p��mo Pytisem), nebo jde o tzv. `u�ivatelsk� p��kaz' s
    vlastn� obslu�nou rutinou (definovanou u�ivatelem/tv�rcem aplikace).

    O�et�en� u�ivatelsk�ch p��kaz� je provedeno vyvol�n�m obslu�n� rutiny,
    specifikovan� argumentem konstruktoru 'handler'.  Parametry, se kter�mi
    bude obslu�n� rutina zavol�na z�vis� na typu t��dy 'CommandHandler'.  T��da
    'BrowseForm' tak nap��klad jako argument p�ed� data aktu�ln�ho ��dku
    seznamu apod.  V�ce v dokumentaci jednotliv�ch t��d.

    Terminologick� pozn�mka: N�zev arguemntu konstruktoru 'handler' (obslu�n�
    rutina p��kazu) nelze zam��ovat s pou�it�m ozna�en� handler pro prvek
    u�ivatelsk�ho rozhran�, kter� se o zpracov�n� p��kazu postar�
    ('CommandHandler').  Shodn� ozna�en� je pou�ito z historick�ch d�vod� a je
    t�eba m�t rozli�en� na pam�ti.
    
    """
    def __init__(self, cls, name, doc=None, handler=None,
                 enabled=True, access_groups=None, log_=True):
        """Definuj p��kaz.

        Argumenty:

          cls -- T��da prvku u�ivatelsk�ho rozhran�, kter� p��kaz zpracov�v�.
            T��da mus� b�t potomkem t��dy 'CommandHandler'.  V�ce tak�
            viz. v��e (dokumentace t��dy 'Command').
          name -- n�zev p��kazu.  Nepr�zdn� �et�zec, pou�iteln� jako Pythonov�
            identifik��tor, mezi n�zvy p��kaz� unik�tn�.  N�zev je pou�it pro
            vytvo�en� konstanty (viz. n�e), tak�e dal��m po�adavkem je, aby
            ve�ker� p�smena byla velk�.
          doc -- dokumenta�n� �et�zec p��kazu.  Stru�n� popis, kter� m��e b�t
            nap�. zobrazen v u�ivatelsk�m rozhran�.
          handler -- obslu�n� funkce volan� p�i zpracov�n� p��kazu.  M� v�znam
            p�i definici u�ivatelsk�ch p��kaz�.  Bl�e viz dokumentace t��dy.
            Hodnotou je callable object, nebo None.
          access_groups -- sekvence jmen skupin (strings), kter� maj� pr�vo
            p��kaz vyvolat; m��e b�t t� 'None', v�kter�m�to p��pad� p��kaz
            mohou vyvolat v�echny skupiny.  Toto opr�vn�n� je form�ln�,
            zohledn�n� jen v�u�ivatelsk�m rozhran�, nem� faktickou bezpe�nostn�
            roli.  P��tomnost u�ivatele ve skupin�ch je zji��ov�na pouze jednou
            p�i inicializaci instance p��kazu, co� je v�t�inou p�i startu
            aplikace.
          enabled -- bu�to p��mo boolean hodnota ur�uj�c�, zda je p��kaz
            aktivn�, nebo odkaz na funkci, kter� toto zjist� (a vr�t�
            odpov�daj�c� boolean hodnotu).  Jde o funkci t�� argument� (APPL,
            COMMAND, ARGS), kde APPL je instance aplikace (t��dy
            'Application'), COMMAND je instance p��kazu a ARGS je slovn�k
            arguemnt� s jak�mi bude p��kaz vol�n.
          log_ -- pr�v� kdy� je pravdiv�, je vyvol�n� p��kazu logov�no jako
            EVENT, jinak je logov�no pouze jako DEBUG

        Po definici p��kazu je ka�d� p��kaz automaticky dostupn� jako ve�ejn�
        konstanta sv� obslu�n� t��dy (dan� argumentem 'cls').  N�zev t�to
        konstanty je v�dy COMMAND_ + 'name' ('name' je n�zev p��kazu zadan� v
        konstruktoru).  Na��klad tedy 'Application.COMMAND_LEAVE_FORM', nebo
        'LookupForm.COMMAND_SORT_COLUMN'.

        """
        assert issubclass(cls, CommandHandler), \
               "Not a CommandHandler subclass: %s" % cls
        assert isinstance(name, types.StringType) and name == name.upper(), \
               (name, type(name))
        assert handler is None or callable(handler), handler
        assert doc is None or isinstance(doc, types.StringTypes)
        assert callable(enabled) or isinstance(enabled, types.BooleanType)
        assert access_groups is None or \
               isinstance(access_groups,
                          (types.StringType, types.TupleType, types.ListType))
        self._cls = cls
        self._name = name
        self._doc = doc
        self._id = id = '.'.join((cls.__name__, name.lower().replace('_', '-')))
        self._handler = handler
        self._log = log_
        self._has_access = pytis.data.is_in_groups(access_groups)
        self._enabled = enabled
        self._cache = {}
        assert not hasattr(cls, 'COMMAND_' + name), \
               "Command '%s' already defined for %s" % (name, cls.__name__)
        setattr(cls, 'COMMAND_' + name, self)
        

    def __call__(self, **kwargs):
        """Umo��uje pohodln� vytvo�it definici p��kazu a jeho argument�.

        Vrac� dvojici (COMMAND, ARGS), kde COMMAND je instance p��kazu a ARGS
        jsou jeho argumenty jako slovn�k.

        T�to vlastnosti lze vyu��t nap��klad pro zjednodu�en� z�pisu
        kl�vesov�ch map apod., kde p��kaz a jeho argumenty tvo�� ned�lnou
        dvojici.

        """
        return (self, kwargs)
        
    def cls(self):
        """Vra� t��du u�ivatelsk0ho rozhran�, kter� tento p��kaz zpracov�v�."""
        return self._cls

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
    
    def handler(self):
        """Vra� rutinu pro zpracov�n� p��kazu zadanou v konstruktoru."""
        return self._handler

    def enabled(self, args):
        """Vra� pravdu, pokud je p��kaz aktivn� (sm� b�t vyvol�n).

        Pokud u�ivatel nem� p��stupov� pr�va k dan�mu p��kazu (s dan�mi
        argumenty), je automaticky vr�ceno False.  P��kazy, kter� nejsou
        kompatibiln� s aktivn�m prvkem aplikace (instanc� 'CommandHandler')
        jsou rovn� automaticky neaktivn�.  Tak� p��kazy, pro n� aktivn� prvek
        aplikace definuje metodu 'can_<command_name>' a ta vr�t� False, jsou
        neaktivn�.  A� nakonec je testov�na hodnota (nebo v�sledek vol�n�
        funkce) argumentu 'enabled' konstruktoru.
        
        """
        appl = pytis.form.application._application
        
        if not self._has_access:
            return False
        handler = self._cls.get_command_handler_instance(appl)
        if handler is None or not hasattr(handler, 'COMMAND_'+self._name) \
               or not getattr(handler, 'COMMAND_'+self._name) == self:
            return False
        can_method_name = 'can_' + self._name.lower()
        if hasattr(handler, can_method_name):
            can = getattr(handler, can_method_name)
            if not can(**args):
                return False
        enabled = self._enabled
        if not isinstance(enabled, types.BooleanType):
            enabled = bool(enabled(appl, self, args))
        return enabled
    
    def log_kind(self):
        """Vra� druh logovac� hl�ky, pod kter�m m� b�t p��kaz logov�n."""
        if self._log:
            kind = EVENT
        else:
            kind = DEBUG
        return kind

    def has_access(self):
        return self._has_access

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
    # TODO: Zde vyvol�me on_command aplikace a ta je zodpov�dna za p�ed�n�
    # p��kazu formul��i, pokud to nen� jej� p��kaz.  Formul�� zase p�ed�v�
    # p��kazy pol��k�m.  To odpov�d� p�vodn�mu n�vrhu.  Nyn� v�ak m�me
    # CommandHandler a metodu get_command_handler_instance(), tak�e bychom
    # p��kazy mohli p�ed�vat rovnou instanci, kter� p��kaz n�le��.  Pozor,
    # mo�n� to tak� n�jak souvis� s metodou KeyHandler._maybe_invoke_command().
    if command.enabled(kwargs):
        appl = pytis.form.application._application
        return appl.on_command(command, **kwargs)
    else:
        return False
