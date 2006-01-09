# -*- coding: iso-8859-2 -*-

# Definice uživatelských příkazů
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

"""Třídy sloužící k definici příkazů.

Tento modul definuje třídy sloužící k definici příkazů.  Vlastní definice všech
příkazů aplikace je potom soustředěna centrálně v modulu 'commands_'.

"""

from pytis.form import *


class CommandHandler:
    """Mix-in třída, kterou musí dědit třídy definující vlastní příkazy."""
    
    def get_command_handler_instance(cls, application):
        """Najdi v aplikaci aktivní prvek, který je schopen zpracovat příkaz."""
        raise ProgramError("This method must be overriden in derived class.")
    get_command_handler_instance = classmethod(get_command_handler_instance)
                           
    
class Command(object):
    """Reprezentace obecného příkazu uživatelského rozhraní.

    Každý příkaz je vázán na určitý typ prvku uživatelského rozhraní aplikace
    (formulář, dialog, vstupní políčko), nad jehož instancí může být vyvolán.
    Třída každého takového prvku uživatelského rozhraní, která chce vlastní
    příkazy definovat, musí být odvozena od třídy 'CommandHandler'.

    Metoda 'enabled()' každého příkazu potom nejprve zjistí, zda je aktivní
    prvek aplikace (instance 'CommandHandler') kompatibilní s daným příkazem
    (příkaz byl definován pro jeho třídu).  Pokud ne, metoda vrací vždy false
    bez testování hodnoty, dané arguemntem 'enabled' konstruktoru.  Tím jsou v
    důsledku automaticky zneaktivněny i příslušné položky menu atd.

    Obslužná třída (resp. její instance) každý příkaz zpracuje buďto sama
    (příkazy definované přímo Pytisem), nebo jde o tzv. `uživatelský příkaz' s
    vlastní obslužnou rutinou (definovanou uživatelem/tvůrcem aplikace).

    Ošetření uživatelských příkazů je provedeno vyvoláním obslužné rutiny,
    specifikované argumentem konstruktoru 'handler'.  Parametry, se kterými
    bude obslužná rutina zavolána závisí na typu třídy 'CommandHandler'.  Třída
    'BrowseForm' tak například jako argument předá data aktuálního řádku
    seznamu apod.  Více v dokumentaci jednotlivých tříd.

    Terminologická poznámka: Název arguemntu konstruktoru 'handler' (obslužná
    rutina příkazu) nelze zaměňovat s použitím označení handler pro prvek
    uživatelského rozhraní, který se o zpracování příkazu postará
    ('CommandHandler').  Shodné označení je použito z historických důvodů a je
    třeba mít rozlišení na paměti.
    
    """
    def __init__(self, cls, name=None, doc=None, key=None, handler=None,
                 enabled=True, access_groups=None, static=False, log_=True):
        """Definuj příkaz.

        Argumenty:

          cls -- Třída prvku uživatelského rozhraní, který příkaz zpracovává.
            Třída musí být potomkem třídy 'CommandHandler'.  Více také
            viz. výše (dokumentace třídy 'Command').
          name -- název příkazu.  Neprázdný řetězec, použitelný jako Pythonový
            identifikáítor, mezi názvy příkazů unikátní.  Název je použit pro
            vytvoření konstanty (viz. níže), takže dalším požadavkem je, aby
            veškerá písmena byla velká.
          doc -- dokumentační řetězec příazu.  Pokud je příkaz určen k
            uživatelskému využití, měly by být zmíněny zejména argumenty
            příkazu.
          handler -- obslužná funkce volaná při zpracování příkazu.  Má význam
            při definici uživatelských příkazů.  Blíže viz dokumentace třídy.
            Hodnotou je callable object, nebo None.
          access_groups -- sekvence jmen skupin (strings), které mají právo
            příkaz vyvolat; může být též 'None', v kterémžto případě příkaz
            mohou vyvolat všechny skupiny.  Toto oprávnění je formální,
            zohledněné jen v uživatelském rozhraní, nemá faktickou bezpečnostní
            roli.  Přítomnost uživatele ve skupinách je zjišťována pouze jednou
            při inicializaci instance příkazu, což je většinou při startu
            aplikace.
          enabled -- buďto přímo boolean hodnota určující, zda je příkaz
            aktivní, nebo odkaz na funkci, která toto zjistí (a vrátí
            odpovídající boolean hodnotu).  Jde o funkci tří argumentů (APPL,
            COMMAND, ARGS), kde APPL je instance aplikace (třídy
            'Application'), COMMAND je instance příkazu a ARGS je slovník
            arguemntů s jakými bude příkaz volán.
          static -- pokud je předána pravdivá hodnota, bude hodnota vrácená
            funkcí `enabled' považována za neměnnou a výsledek tedy bude
            cachován.  V opačném případě (výchozí hodnota) bude funkce volána
            při každém požadavku na zjištění hodnoty enabled.
          log_ -- právě když je pravdivé, je vyvolání příkazu logováno jako
            EVENT, jinak je logováno pouze jako DEBUG

        Po definici příkazu je každý příkaz automaticky dostupný jako veřejná
        konstanta své obslužné třídy (dané argumentem 'cls').  Název této
        konstanty je vždy COMMAND_ + 'name' ('name' je název příkazu zadaný v
        konstruktoru).  Naříklad tedy 'Application.COMMAND_LEAVE_FORM', nebo
        'LookupForm.COMMAND_SORT_COLUMN'.

        """
        if isinstance(cls, types.StringType):
            # TODO: Až se to bude rušit, je třeba také udělat argument `name'
            # pozičním (povinným) argumentem.
            assert name is None
            name = cls.replace('user-command.', '').upper().replace('-', '_')
            cls = BrowseForm
            log(OPERATIONAL, "Konstruktor Command volán se starými argumenty!",
                stack_info(depth=2).splitlines()[0])
        assert issubclass(cls, CommandHandler), \
               "Not a CommandHandler subclass: %s" % cls
        assert isinstance(name, types.StringType) and name == name.upper()
        assert handler is None or callable(handler)
        assert doc is None or isinstance(doc, types.StringTypes)
        assert key is None or is_string(key) or is_sequence(key)
        assert callable(enabled) or isinstance(enabled, types.BooleanType)
        assert isinstance(static, types.BooleanType)
        assert access_groups is None or \
               isinstance(access_groups,
                          (types.StringType, types.TupleType, types.ListType))
        self._cls = cls
        self._name = name
        self._doc = doc
        self._id = id = '.'.join((cls.__name__, name.lower().replace('_', '-')))
        if key is not None:
            log(OPERATIONAL,
                "Použit potlačený argument `key' třídy `Command':", (key, id))
        self._handler = handler
        self._log = log_
        self._has_access = True
        if access_groups is not None:
            access_groups = xtuple(access_groups)
            dbconnection = config.dbconnection
            groups = pytis.data.DBDataDefault.class_access_groups(dbconnection)
            if groups is not None:
                self._has_access = some(lambda g: g in groups, access_groups)
        self._enabled = enabled
        self._static = static
        self._cache = {}
        assert not hasattr(cls, 'COMMAND_' + name), \
               "Command '%s' already defined for %s" % (name, cls.__name__)
        setattr(cls, 'COMMAND_' + name, self)
        

    def __call__(self, **kwargs):
        """Umožňuje pohodlně vytvořit definici příkazu a jeho argumentů.

        Vrací dvojici (COMMAND, ARGS), kde COMMAND je instance příkazu a ARGS
        jsou jeho argumenty jako slovník.

        Této vlastnosti lze využít například pro zjednodušení zápisu
        klávesových map apod., kde příkaz a jeho argumenty tvoří nedílnou
        dvojici.

        """
        return (self, kwargs)
        
    def cls(self):
        """Vrať třídu uživatelsk0ho rozhraní, která tento příkaz zpracovává."""
        return self._cls

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
    
    def handler(self):
        """Vrať rutinu pro zpracování příkazu zadanou v konstruktoru."""
        return self._handler

    def enabled(self, application, args):
        """Vrať pravdu, pokud je příkaz aktivní (smí být vyvolán).

        Pokud uživatel nemá přístupová práva k danému příkazu, je automaticky
        vráceno False.  Příkazy, které nejsou kompatibilní s aktivním prvkem
        aplikace (instancí 'CommandHandler') jsou rovněž automaticky neaktivní.
        Také příkazy, pro něž aktivní prvek aplikace definuje metodu
        'can_<command_name>' a ta vrátí False, jsou neaktivní.  Až nakonec je
        testována hodnota (nebo výsledek volání funkce) argumentu 'enabled'
        konstruktoru.
        
        """
        if not self._has_access:
            return False
        handler = self._cls.get_command_handler_instance(application)
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
            if self._static:
                cache_key = tuple(args.items())
                try:
                    enabled = self._cache[cache_key]
                except KeyError:
                    enabled = bool(enabled(application, self, args))
                    self._cache[cache_key] = enabled
            else:
                enabled = bool(enabled(application, self, args))
        return enabled
    
    def log_kind(self):
        """Vrať druh logovací hlášky, pod kterým má být příkaz logován."""
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

    def __setattr__(self, name, value):
        # TODO: Časem zrušit.
        if name == 'key':
            log(OPERATIONAL,
                "Nastaven potlačený atribut `key' třídy `Command':",
                (value, self._id))
        self.__dict__[name] = value
            
        
def invoke_command(command, **kwargs):
    """Vyvolej globální zpracování příkazu 'command'.

    Argumenty:

      command -- instance třídy 'Command'
      kwargs -- parametry příkazu

    """
    appl = pytis.form.application._application
    # TODO: Zde vyvoláme on_command aplikace a ta je zodpovědna za předání
    # příkazu formuláři, pokud to není její příkaz.  Formulář zase předává
    # příkazy políčkům.  To odpovídá původnímu návrhu.  Nyní však máme
    # CommandHandler a metodu get_command_handler_instance(), takže bychom
    # příkazy mohli předávat rovnou instanci, které příkaz náleží.  Pozor,
    # možná to také nějak souvisí s metodou KeyHandler._maybe_invoke_command().
    if command.enabled(appl, kwargs):
        return appl.on_command(command, **kwargs)
    else:
        return False
