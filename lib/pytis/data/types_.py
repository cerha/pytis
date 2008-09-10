# -*- coding: iso-8859-2 -*-

# Datové typy
#
# Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008 Brailcom, o.p.s.
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

"""Práce s datovými typy, jejich hodnotami a vnější reprezentací.

Základní ideou modulu je, že uvnitř programu vždy pracujeme s hodnotami
určitého, námi definovaného, typu.  Z důvodu datové abstrakce při práci s daty
nepoužíváme přímo standardní typy Pythonu a jeho knihoven, nýbrž naše vlastní
obálky okolo nich, které nám zajistí nezávislost vůči konkrétní reprezentaci
hodnot daného typu v různých částech programu (PostgreSQL, wxWindows, ...).
Kromě toho nám tyto typové obálky mohou také poskytovat některé doplňující
funkce související s typy dat, jako je například validace vstupní hodnoty
daného typu reprezentované stringem a její převod na interní reprezentaci, se
kterou dále v programu pracujeme.

Základem modulu je abstraktní třída 'Type', která je společným základem všech
typových tříd.  Jejím poděděním vznikají konkrétní typy nebo jejich společné
specializovanější základy.  Hodnoty daných typů jsou pak reprezentovány
instancemi samostatné třídy 'Value'.

"""

import math
import re
import string
from cStringIO import StringIO
import thread

from mx import DateTime as DT

from pytis.data import *


class _MType(type):

    def __call__ (self, *args, **kwargs):
        return self.make(*args, **kwargs)
    
    
class Type(object):
    """Abstraktní třída sloužící jako společný základ všech typů.

    Tuto třídu musí povinně dědit všechny typové třídy.

    Instance této třídy jsou považovány za immutable, nesmí být po své
    inicializaci modifikovány a mohou být neomezeně sdíleny.
    
    """
    __metaclass__ = _MType
    
    class _TypeTable:

        def __init__(self):
            self._id_counter = Counter()
            self._init_args_to_id = {}
            self._id_to_init_args = {}
            self._init_args_to_type = {}

        def _key(self, class_, args, kwargs):
            return (class_.__module__, class_.__name__) + \
                   args + tuple(kwargs.items())

        def id_of_initargs(self, class_, args, kwargs):
            table = self._init_args_to_id
            key = self._key(class_, args, kwargs)
            if table.has_key(key):
                result = table[key]
            else:
                result = table[key] = self._id_counter.next()
                self._id_to_init_args[result] = \
                  class_.__module__, class_.__name__, args, kwargs
            return result

        def get(self, id):
            if type(id) != type(0):
                raise Exception('Invalid id type', id)
            return self._id_to_init_args.get(id)

        def get_instance(self, class_, *args, **kwargs):
            table = self._init_args_to_type
            key = self._key(class_, args, kwargs)
            if table.has_key(key):
                result = table[key]
            else:
                result = table[key] = class_.__new__(class_)
                assert isinstance(result, class_)
                result.__init__(*args, **kwargs)
                result._id = self.id_of_initargs(class_, args, kwargs)
            assert result is not None
            return result

    _type_table = _TypeTable()
    _remote_type_table = None
    _remote_type_table_cache = {}

    VM_NULL_VALUE = 'VM_NULL_VALUE'
    VM_INVALID_VALUE =  'VM_INVALID_VALUE'
    _VM_NULL_VALUE_MSG = _("Prázdná hodnota")
    _VM_INVALID_VALUE_MSG = _("Nesprávná hodnota")
    
    _SPECIAL_VALUES = ()
    
    _VALIDATION_CACHE_LIMIT = 1000

    def _make(class_, *args, **kwargs):
        result = Type._type_table.get_instance(class_, *args, **kwargs)
        assert result is not None
        return result
    _make = staticmethod(_make)

    def make(class_, *args, **kwargs):
        """Pouze pro účely zpětné kompatibility a pro metatřídu.

        V novém kódu nepoužívat.

        """
        return class_._make(class_, *args, **kwargs)
    make = classmethod(make)

    def __init__(self, not_null=False, enumerator=None, constraints=(),
                 validation_messages=None, unique=False):
        """Inicializuj instanci.

        Argumenty:
        
          not_null -- příznak udávající, zda hodnoty tohoto typu smí být
            prázdné.  Za prázdnou hodnotu je považována hodnota None, nebo
            libovolná jiná hodnota na None mapovaná (viz. konstanta
            _SPECIAL_VALUES).  Pokud tento argument pravdivý, neprojde prázdná
            hodnota validací.
            
          enumerator -- specifikace enumerátoru, jako instance `Enumerator',
            nebo None.  Slouží k realizaci integritních omezení výčtového
            typu.  Více viz dokumentace třídy `Enumerator'.
            
          constraints -- sekvence validačních funkcí sloužících k realizaci
            libovolných integritních omezení.  Každá z těchto funkcí je funkcí
            jednoho argumentu, kterým je vnitřní hodnota typu.  Funkce pro tuto
            hodnotu musí vrátit buď 'None', je-li hodnota správná, nebo
            chybovou hlášku jako string v opačném případě.
            
          validation_messages -- dictionary identifikátorů a validačních
            hlášek.  Klíče jsou identifikátory validačních hlášek definované
            konstantami třídy s názvy začínajícími prefixem 'VM_' a hodnoty
            jsou hlášky coby řetězce.  Hlášky z tohoto argumentu, jsou-li pro
            daný identifikátor definovány, mají přednost před implicitními
            hláškami definovanými typem.

          unique -- flag saying the value must be unique within its column in a
            table

        """
        super(Type, self).__init__()
        assert isinstance(not_null, types.BooleanType)
        assert isinstance(unique, types.BooleanType)
        assert enumerator is None or isinstance(enumerator, Enumerator)
        assert isinstance(constraints, (types.ListType, types.TupleType))
        assert validation_messages is None or \
               isinstance(validation_messages, types.DictType) 
        self._not_null = not_null
        self._unique = unique
        self._enumerator = enumerator
        self._constraints = xtuple(constraints)
        vm = [(getattr(self, attr), getattr(self, '_'+attr+'_MSG'))
              for attr in dir(self) if attr.startswith('VM_')]
        self._validation_messages = dict(vm)
        if validation_messages is not None:
            self._validation_messages.update(validation_messages)
        self._fetched = True
        # Cachujeme na úrovni instancí, protože ty jsou stejně sdílené, viz
        # `__new__'.
        self._validation_cache = LimitedCache(self._validating_provider,
                                              limit=self._VALIDATION_CACHE_LIMIT)
        if isinstance(enumerator, DataEnumerator):
            # TODO: Jak se to bude chovat po smrti instance typu?
            def callback():
                self._validation_cache.reset()
            enumerator.add_callback_on_change(callback)

    def type_table(class_):
        """Vrať tabulku typů jako instanci '_TypeTable'.

        Jediný účel této metody je zpřístupnit tabulku typů pro vzdálené
        předávání typů ze serveru na klienta.  Pro jiné účely by tabulka typů
        neměla být používána.

        """
        return Type._type_table
    type_table = classmethod(type_table)
    
    def __cmp__(self, other):
        """Vrať 0, právě když 'self' a 'other' reprezentují tentýž typ."""
        if not sameclass(self, other):
            result = compare_objects(self, other)
        elif self._id == other._id:
            result = 0
        elif (self._constraints == other._constraints and
              self._not_null == other._not_null and
              self._unique == other.unique and
              cmp(self._enumerator, other._enumerator) == 0):
            result = 0
        else:
            result = -1
            #TODO: Volání compare_objects způsobuje rekurzi.  Existuje nějaký
            # případ, kdy by se instance mohly rovnat i přes nesplnění výše
            # uvedených podmínek?
            #result = compare_objects(self, other)
        return result

    def __hash__(self):
        return hash(self.__class__.__name__)

    def __getstate__(self):
        assert self._id is not None, ('Improper type instance', self)
        return self._id
    
    def __setstate__(self, state):
        if type(state) != type(0):
            raise InvalidAccessError('Invalid type identifier', state)
        self._id = state
        self._fetched = False

    def __getattr__(self, name):
        if self._fetched:
            raise AttributeError(name)
        assert self._id is not None, ('Improper type instance', self)
        id = self._id
        cache = self._remote_type_table_cache
        if cache.has_key(id):
            t = cache[id]
            if t.__class__ != self.__class__:
                raise ('Incorrect type class', self, t.__class__,
                       self.__class__)
        else:
            remote_table = Type._remote_type_table
            if remote_table is None:
                import config
                server = config.server
                if server is None:
                    raise AttributeError(name)
                import pytis.remote, Pyro.core
                uri = 'PYROLOC://%s/%s' % (server, pytis.remote.NAME_TYPE_TABLE)
                remote_table = Type._remote_type_table = \
                               Pyro.core.getProxyForURI(uri)
            class_module, class_name_, args, kwargs = \
               remote_table.get(id)
            if class_module != self.__class__.__module__ or \
                   class_name_ != self.__class__.__name__:
                raise Exception('Invalid type class', id, class_module,
                                class_name_, self.__class__.__name__)
            t = self.__class__(*args, **kwargs)
            cache[id] = t
        self.__dict__.update(t.__dict__)
        self._id = id
        self._fetched = True
        try:
            return self.__dict__[name]
        except KeyError:
            raise AttributeError(name)

    def validate(self, object, strict=True, transaction=None, condition=None, **kwargs):
        """Validate the 'object' and return a 'Value' instance and an error.

        Arguments:
        
          object -- an object to be converted to a value
          strict -- passing 'False' leads to a ``tolerant'' validation.  No
            constraints are checked and the method does its best to convert
            anything reasonable to a value.  It may be useful when the
            reason is not validation, but the conversion.
          transaction -- transaction for data operations.
          condition -- runtime filter condition for enumerator validation.
          kwargs -- type specific keyword arguments

        Returns: a pair (VALUE, ERROR).  VALUE is a 'Value' instance (for a
        valid 'object') or 'None' (for an invalid 'object').  ERROR is 'None'
        for a valid 'object' and a 'ValidationError' instance for an invalid
        'object'.

        Most types require the 'object' to be a string, most often representing
        user input or a value loaded from a file or process.  Certain more
        sophisticated types, however, may accept or require an 'object' of a
        different type.  All types should, if possible, accept a string and if
        not (eg. for excessive complications), they should accept an object,
        which may be simply constructed from data picked up from the user
        interface.  If the 'object' is not a string or another type explicitely
        allowed in the documentation of the corresponding type class, the
        behavior of this method is undefined.

        The 'kwargs' argument allows parametrized validation for particular
        types.  Each type may specify its set of options, which control the way
        how the user input is treated.

        Most types should validate an empty string to a 'Value' instance, which
        has 'None' as its value.

        Derived classes should not override this method.  They should override
        '_validate()' instead.
        
        """
        # Tato metoda je zároveň používána i pro převod hodnot získaných
        # z databázového rozhraní.  To není úplně ideální, ale je to zatím
        # postačující a rozlišení těchto dvou použití nestojí za komplikace
        # s tím spojené.  Pokud by bylo potřeba v budoucnu toto rozlišit, lze
        # přidat další metodu nebo argument.  Nyní je to částečně řešeno
        # argumentem 'strict'.
        try:
            key = (object, strict, transaction, condition, tuple(kwargs.items()))
            result = self._validation_cache[key], None
        except ValidationError, e:
            result = None, e
        return result

    def _validating_provider(self, key):
        object, strict, transaction, condition, kwargs_items = key
        special = rassoc(object, self._SPECIAL_VALUES)
        if special:
            value = Value(self, special[0])
        elif object is None:
            value = Value(self, None)
        else:
            value, error = self._validate(object, **dict(kwargs_items))
            if error:
                raise error
        if strict:
            self._check_constraints(value.value(), transaction=transaction, condition=condition)
        return value
    
    def _validate(self, object, **kwargs):
        return Value(self, None), None
    
    def wm_validate(self, object):
        """Zvaliduj objekt pro wildcard matching.

        Argumenty:

          object -- validovaný objekt, string

        Vrací: dvojici (VALUE, ERROR).  VALUE je instance třídy 'WMValue'
        (je-li 'object' správný) nebo je 'None' (je-li 'object' nesprávný).
        Je-li 'object' správný, je ERROR 'None', v opačném případě je instancí
        třídy 'ValidationError', která obsahuje popis chyby.

        Ne všechny typy musí tento druh validace podporovat.  Ty, které
        nepodporují, vrací dvojici (None, ERROR).

        """
        msg = _("Hvězdičkové výrazy nejsou podporovány pro hodnoty typu '%s'.")
        return None, ValidationError(msg % self.__class__.__name__)

    def _validation_error(self, id, **kwargs):
        message = self._validation_messages[id]
        if kwargs:
            message %= kwargs
        return ValidationError(message)
        
    def _check_constraints(self, value, transaction=None, condition=None, **kwargs):
        if value is None:
            if self._not_null:
                raise self._validation_error(self.VM_NULL_VALUE)
            else:
                return True
        if self._enumerator is not None:
            if isinstance(self._enumerator, DataEnumerator):
                if not self._enumerator.check(value, transaction, condition=condition):
                    raise self._validation_error(self.VM_INVALID_VALUE)
            else:
                if not self._enumerator.check(value):
                    raise self._validation_error(self.VM_INVALID_VALUE)                    
        for c in self._constraints:
            cresult = c(value)
            if cresult is not None:
                raise ValidationError(cresult)

    def not_null(self):
        """Return true if values of this type may not be empty."""
        return self._not_null

    def unique(self):
        """Return true if values of this type must be unique in a table."""
        return not not self._unique

    def enumerator(self):
        """Return the 'Enumerator' instance bound to this type or None."""
        return self._enumerator
            
    def export(self, value, *args, **kwargs):
        """Vrať stringovou reprezentaci 'value' schopnou validace.

        'value' je hodnota vrácená metodou 'Value.value' z objektu vytvořeného
        v metodě 'validate' na základě *string* parametru.  Pro objekty vzniklé
        z argumentu 'validate', který nebyl string, je chování této metody
        nedefinováno.  Výjimkou je hodnota 'None', pro kterou všechny typy,
        v jejichž dokumentaci není řečeno jinak, vrací prázdný string.

        V této třídě metoda vrací výsledek operátoru '``', pouze pro 'value'
        rovno 'None' vrací prázdný string.  Potomci třídy nechť tuto metodu
        nepředefinovávají, neboť metoda může provádět i různé doplňující akce;
        nechť potomci předefinovávají metodu `_export()'.

        """
        special = assoc(value, self._SPECIAL_VALUES)
        if special:
            return special[1]
        exported = apply(self._export, (value,)+args, kwargs)
        return exported

    def _export(self, value):
        return `value`

    def default_value(self):
        """Vrať implicitní hodnotu typu jako instanci 'Value'.

        Pokud taková hodnota neexistuje nebo nemá smysl, vrať 'None'.
        
        Vrácená hodnota může být využita například v inicializaci nových
        záznamů.

        """
        return Value(self, None)
  

class Number(Type):
    """Abstraktní typová třída, která je základem všech numerických typů.

    Třída víceméně nic nového nedefinuje, je určena pouze k podědění všemi
    numerickými typy, aby tyto byly jakožto číselné snadno rozpoznatelné.

    """
    _SPECIAL_VALUES = Type._SPECIAL_VALUES + ((None, ''),)


class Big(Type):
    """Mixin class denoting types with big values.

    Instances of this type are sometimes handled in Pytis in a special way,
    e.g. they are not printed to the terminal log.
    
    """

class Large(Big):
    """Mixin class denoting types with really large values.

    Instances of this type may be handled in a special way not only inside
    Pytis, but possibly inside databases as well.

    """

class Limited(Type):
    """Mixin class for types with possibly limited maximal lenght.

    Minimal and maximal length of a value of this type can be limited by passing the
    `minlen' and `maxlen' constructor arguments.
    
    """

    VM_MINLEN = 'VM_MINLEN'
    _VM_MINLEN_MSG = _("Nedodržena minimální délka %(minlen)s")
    VM_MAXLEN = 'VM_MAXLEN'
    _VM_MAXLEN_MSG = _("Překročena maximální délka %(maxlen)s")

    def __init__(self, minlen=None, maxlen=None, **kwargs):
        """Initialize the instance.
        
        Arguments:
        
          maxlen -- maximal length of a value of this type as integer or
            'None'; 'None' denotes unlimited lenght.
             
        Other arguments are passed to the parent constructor.

        """
        self._minlen = minlen
        self._maxlen = maxlen
        super(Limited, self).__init__(**kwargs)

    def __cmp__(self, other):
        """Return 0 if 'self' and 'other' are of the same class and maxlen."""
        result = super(Limited, self).__cmp__(other)
        if not result:
            result = cmp(self.maxlen(), other.maxlen())
        if not result:
            result = cmp(self.minlen(), other.minlen())
        return result

    def minlen(self):
        """Return the minimal lenght of the value as an integer or 'None'.

        'None' denotes unlimited minimal lenght.
        
        """
        return self._minlen

    def maxlen(self):
        """Return the maximal lenght of the value as an integer or 'None'.

        'None' denotes unlimited lenght.
        
        """
        return self._maxlen

    def _format_length(self, length):
        return str(length)

    def _check_constraints(self, value, **kwargs):
        super(Limited, self)._check_constraints(value, **kwargs)
        self._check_maxlen(value)

    def _check_maxlen(self, value):
        if value is not None:
            if self._minlen is not None and len(value) < self._minlen:
                raise self._validation_error(self.VM_MINLEN,
                                             minlen=self._format_length(self._minlen))
            if self._maxlen is not None and len(value) > self._maxlen:
                raise self._validation_error(self.VM_MAXLEN,
                                             maxlen=self._format_length(self._maxlen))

    
class Integer(Number):
    """Libovolný integer."""

    VM_NONINTEGER = 'VM_NONINTEGER'
    _VM_NONINTEGER_MSG = _("Není to celé číslo")
    
    def _validate(self, string):
        """Pokus se převést 'string' na plain nebo long integer.
        
        Pokud je 'string' možno převést na plain integer, je správný a vrácená
        instance třídy 'Value' obsahuje odpovídající hodnotu jako plain
        integer.  V jiném případě platí analogické pravidlo pro long integers.
        Pokud 'string' není možno převést na plain ani long integer, 'string'
        není správný a je vrácena chyba.

        Metoda validuje všechny zápisy integers akceptované Pythonem, zejména
        tedy i long integers ve tvaru '1L'.
        
        """
        assert isinstance(string, types.StringTypes), ('Not a string', string)
        error = None
        try:
            value = int(string)
        except:
            # Dokumentace Pythonu 1.5.2 neříká, že by `int' mohlo metat metat
            # nějakou výjimkou, ale evidentně by mělo, pokud `string' nelze
            # na obyčejný integer převést.
            try:
                value = long(string)
            except:
                # Podobně jako `int' i `long' by mělo v případě nemožnosti
                # převodu metat výjimku.
                value = None
        if value is not None:
            result = Value(self, value), None
        else:
            result = None, self._validation_error(self.VM_NONINTEGER)
        return result


class Serial(Integer):
    """Integer s automaticky generovanými hodnotami.

    Typ oproti 'Integer' nezavádí žádné nové rysy, jeho význam je čistě
    specifikační.  Například uživatelské rozhraní tak získává informaci, že
    není třeba ani žádoucí explicitně nastavovat hodnoty sloupců tohoto typu
    v řádku při vkládání nového záznamu.

    """
    pass


class Float(Number):
    """Číslo v pohyblivé řádové čárce v rozsahu podporovaném Pythonem."""

    CEILING = 'CEILING'
    """Konstanta pro typ zaokrouhlení ve 'validate'."""
    FLOOR = 'FLOOR'
    """Konstanta pro typ zaokrouhlení ve 'validate'."""

    VM_INVALID_NUMBER = 'VM_INVALID_NUMBER'
    _VM_INVALID_NUMBER_MSG = _("Není to povolené číslo")
    
    def __init__(self, precision=None, **kwargs):
        """Definuj typ reálného čísla.

        Argumenty:

          precision -- nezáporný integer udávající počet čísel za desetinnou
            čárkou uváděný při exportu, nebo 'None' (pak není přesnost uměle
            omezena)

        Ostatní klíčové argumenty jsou shodné, jako v předkovi.
             
        """
        super(Float, self).__init__(**kwargs)
        assert precision is None or precision >= 0, \
               ('Invalid precision', precision)
        if precision is None:
            format = '%f'
        else:
            format = '%%.%df' % precision
        self._format_string = format
        self._precision = precision

    def precision(self):
        """Vrať přesnost čísla zadanou v konstruktoru jako integer."""
        return self._precision
    
    def _validate(self, string, precision=None, rounding=None):
        """Pokus se převést 'string' na float.

        Pokud je 'string' možno převést na float, je správný a vrácená instance
        třídy 'Value' obsahuje odpovídající hodnotu jako plain integer.  Pokud
        'string' převést možno není, 'string' není správný a je vrácena chyba.

        Metoda validuje všechny zápisy floats akceptované Pythonem.

        Argumenty:

          precision -- nezáporný integer udávající počet čísel za desetinnou
            čárkou, na která má být zvalidované číslo zaokrouhleno, nebo 'None'
            (pak není přesnost uměle omezena)
          rounding -- specifikace zaokrouhlení při požadavku na omezenou
            přesnost; 'None' značí standardní zaokrouhlení, konstanta 'CEILING'
            zaokrouhlení směrem nahoru, konstanta 'FLOOR' zaokrouhlení směrem
            dolů (pozor na záporná čísla, platí to pro ně také přesně takto!)
        
        """
        assert isinstance(string, types.StringTypes), ('Not a string', string)
        assert precision is None or \
               type(precision) == type(0) and precision >=0, \
               ('Invalid precision', precision)
        error = None
        try:
            import locale
            # TODO: Odporný hack, z nějakého důvodu následující
            # metoda bez toho nezafunguje
            locale.getlocale(locale.LC_NUMERIC)
            value = locale.atof(string)
        except:
            # Dokumentace Pythonu 1.5.2 neříká, že by `float' mohlo metat metat
            # nějakou výjimkou, ale evidentně by mělo, pokud `string' nelze
            # na float převést.
            value = None
        if value is not None:
            if precision is not None:
                # Pozor na převody mezi binárními a dekadickými čísly!
                rvalue = round(value, precision)
                if rounding:
                    if rounding == self.CEILING:
                        if rvalue < value:
                            rvalue = rvalue + math.pow(10, -precision)
                    elif rounding == self.FLOOR:
                        if rvalue > value:
                            rvalue = rvalue - math.pow(10, -precision)
                    else:
                        raise ProgramError('Invalid rounding argument',
                                           rounding)
                value = rvalue
            result = Value(self, value), None
        else:
            result = None, self._validation_error(self.VM_INVALID_NUMBER)
        return result

    def _export(self, value, locale_format=True):
        if locale_format:
            import locale
            return unicode(locale.format(self._format_string, value, 1))
        else:
            return unicode(self._format_string % value)

        
class String(Limited):
    """Libovolný string.

    Lze také specifikovat, že řetězec může mít pouze omezenou délku, blíže viz
    metody '__init__' a 'maxlen'.

    """    

    _VM_MAXLEN_MSG = _("Řetězec přesahuje maximální délku %(maxlen)s")
    _SPECIAL_VALUES = Type._SPECIAL_VALUES + ((None, ''),)
    
    def _validate(self, string):
        """Vrať instanci třídy 'Value' s hodnotou 'string'.

        Pokud byla v konstruktoru specifikována maximální délka, 'string' je
        správný právě tehdy, není-li delší než tato délka.
        
        """
        assert isinstance(string, types.StringTypes), ('Not a string', string)
        return Value(self, unicode(string)), None

    def _export(self, value):
        # Pozor, na triviální funkci této metody se spoléhá Value.__init__ --
        # při změně zde je nutná změna i tam.
        assert isinstance(value, types.StringTypes), \
               ('Value not a string', value)
        return isinstance(value, unicode) and value or unicode(value)

    def wm_validate(self, object):
        assert isinstance(object, basestring)
        return WMValue(self, object), None

    
class Password(String):
    """Specialized string type for password fields.

    The user interface should handle password input differently from ordinary string input.

    1. The typed characters should never be visible on the screen.

    2. Also, if the constructor argument 'verify' is true (it is by default), the user should be
       required to type the new value twice to prevent typos (since there is no visual feedback).
       The user interface is responsible for creating two fields if the method 'verify()' returns
       true.  The value of the second field is passed as the 'verify' argument to the 'validate()'
       method.

    The validation argument 'verify' should be always passed when validating user input.  It may be
    omitted if validation is used just to convert an already validated string value (e.g. read from
    database) to a 'Value' instance.  When user input is validated, but the type doesn't require
    verification (the user enters the password just once), it is thus necessary to pass the same
    value twice (as the validated value and as the 'verify' argument).

    """
    VM_PASSWORD = 'VM_PASSWORD'
    _VM_PASSWORD_MSG = _("Zadejte heslo dvakrát pro vyloučení překlepů")
    VM_PASSWORD_VERIFY = 'VM_PASSWORD_VERIFY'
    _VM_PASSWORD_VERIFY_MSG = _("Kontrolní zadání hesla neodpovídá")
    VM_INVALID_MD5 = 'VM_INVALID_MD5'
    _VM_INVALID_MD5_MSG = _("Invalid MD5 hash")
    
    def __init__(self, md5=False, verify=True, strength=None, **kwargs):
        """Initialize the instance.
        
        Arguments:
        
          md5 -- boolean flag indicating, that the password is stored as a hexadeximal md5 hash.
            This will lead to automatic conversion of user input to its md5 hash, so the original
            password is no more visible anywhere after successful validation.  The conversion is
            only done when the 'verify' argument is passed to the 'validate()' method.  When
            'verify' is not used, the input string is not considered to be user input, but an
            already hashed value (eg. read from data source).

          verify -- boolean flag indicating, that user input should be verified by the user
            interface by presenting two controls for entering the password.  Both inputs must match
            to pass validation.

          strength -- specification of password strength checking.  If 'None',
            no special checks are performed.  If 'True', default checking
            implemented in the '_check_strength' method is performed.  If
            anything else, it must be a function of a single argument, the
            password string, that returns either 'None' when the password is
            strong enough or an error message if the password is weak.
             
        Other arguments are passed to the parent constructor.

        """
        super(Password, self).__init__(**kwargs)
        assert isinstance(md5, bool)
        assert isinstance(verify, bool)
        self._md5 = md5
        self._verify = verify
        if strength is True:
            self._strength = self._check_strength
        else:
            self._strength = strength

    def verify(self):
        """Return true if verification of user input is required."""
        return self._verify

    def _check_strength(self, string_):
        letters = non_letters = False
        for char in string_:
            if char in string.ascii_letters:
                letters = True
            else:
                non_letters = True
        if not letters or not non_letters:
            return _("Please use mix of letters and non-letters in your password")
        
    def _validate(self, string, verify=None, **kwargs):
        if verify is not None:
            if not verify:
                return None, self._validation_error(self.VM_PASSWORD)
            if string != verify:
                return None, self._validation_error(self.VM_PASSWORD_VERIFY)
        if self._strength is not None:
            error = self._strength(string)
            if error is not None:
                raise ValidationError(error)
        return super(Password, self)._validate(string, **kwargs)

    def validate(self, object, verify=None, **kwargs):
        if self._md5 and verify is None:
            # Strict checking applies to the original value.  Here we are validating the md5 sum,
            # so strict checking is forced to False.
            kwargs['strict'] = False
        value, error = super(Password, self).validate(object, verify=verify, **kwargs)
        if self._md5 and value and value.value() is not None:
            string = str(value.value())
            if verify is not None:
                # User input was valid, so let's turn it into its md5 hash.
                import md5
                value = Value(value.type(), md5.new(string).hexdigest())
            elif len(string) != 32 or not string.isalnum():
                return None, self._validation_error(self.VM_INVALID_MD5)
        return value, error

    
class RegexString(String):

    VM_FORMAT = 'VM_FORMAT'
    _VM_FORMAT_MSG = _("Neplatný formát.")
    _REGEX = None
    
    def __init__(self, regex=None, **kwargs):
        super(RegexString, self).__init__(**kwargs)
        self._regex = re.compile(regex or self._REGEX)
    
    def _validate(self, string, *args, **kwargs):
        # TODO: Shall we rather do the regexp check in _check_constraints?
        value, error = super(RegexString, self)._validate(string, *args,
                                                          **kwargs)
        if error is None and self._regex.match(string) is None:
            value, error = None, self._validation_error(self.VM_FORMAT)
        return value, error

    
class Color(RegexString):
    """Barva reprezentovaná řetězcem '#RRGGBB'."""

    _VM_FORMAT_MSG = _("Formát barvy neodpovídá ('#RGB' nebo '#RRGGBB')")
    _REGEX = re.compile('^\#[0-9a-fA-F]{3,3}([0-9a-fA-F]{3,3})?$')

    
class Inet(String):
    """IPv4 nebo IPv6 adresa."""

    VM_INET_FORMAT = 'VM_INET_FORMAT'
    VM_INET_MASK = 'VM_INET_MASK'
    VM_INET_ADDR = 'VM_INET_ADDR'
    _VM_INET_FORMAT_MSG = _("Chybný formát Inet adresy.")
    _VM_INET_MASK_MSG = _("Chybná maska Inet adresy: %(mask)s")
    _VM_INET_ADDR_MSG = _("Chybná hodnota Inet adresy %(addr)s")
    
    _INET4_FORMAT = re.compile('(\d{1,3}(\.\d{1,3}){0,3}([/]\d{1,2}){0,1})$')

    def _validate(self, string, *args, **kwargs):
        # TODO: Doplnit i validaci pro IPv6 formát
        if not self._INET4_FORMAT.match(string):
            raise self._validation_error(self.VM_INET_FORMAT)
        if string.find('/') != -1:
            addr, mask = string.split('/')
            if int(mask) > 32:
                raise self._validation_error(self.VM_INET_MASK, mask=mask)
        else:
            addr, mask = string, '32'
        numbers = addr.split('.')        
        for n in numbers:
            if n and int(n) > 255:
                raise self._validation_error(self.VM_INET_ADDR, addr=addr)
        for i in range(len(numbers), 4):
            numbers.append('0')
        value = '%s/%s' % ('.'.join(numbers), mask)
        return Value(self, unicode(value)), None


class Macaddr(String):
    """MAC adresa."""

    VM_MACADDR_FORMAT = 'VM_MACADDR_FORMAT'
    _VM_MACADDR_FORMAT_MSG = _("Chybný formát MAC adresy.")
    
    _MACADDR_FORMAT = re.compile('([0-9a-fA-F]{2}[-:]{0,1}){5}[0-9a-fA-F]{2}$')
    
    def _validate(self, string, *args, **kwargs):
        if not self._MACADDR_FORMAT.match(string):
            raise self._validation_error(self.VM_MACADDR_FORMAT)
        macaddr = string.replace(':','').replace('-','')
        value = ':'.join( [macaddr[x:x+2]
                            for x in range(0,len(macaddr),2)] )
        return Value(self, unicode(value)), None


class StructuredText(String):
    """Text formatted using the LCG structored text markup.

    Fields of this type may be handled specifically in the user interface.

    """
    pass

    
class TreeOrder(String):
    """Literal numeric value denoting the level of the item within the tree structure.

    The type itself does not implement any specific features.  It has strictly specificational
    meaning.  If such a column is detected within a list, the user interface may try to render the
    tree structure of the items according to the tree level value.
    
    """
    pass


class DateTime(Type):
    """Časový okamžik reprezentovaný instancí třídy 'DateTime.DateTime'.

    Třída je schopna pracovat pouze s absolutním časovým okamžikem.  Čas je uvažován v UTC nebo v
    lokálním čase podle parametru v konstruktoru, časové zóny nejsou podporovány.  

    Formát data a času je shodný pro import a export a je dán parametrem
    'format' metody '__init__()'.
    
    """
    VM_DT_FORMAT = 'VM_DT_FORMAT'
    VM_DT_VALUE = 'VM_DT_VALUE'
    VM_DT_AGE = 'VM_DT_AGE'
    _VM_DT_FORMAT_MSG = _("Chybný formát data nebo času")
    _VM_DT_VALUE_MSG = _("Chybné datum nebo čas")
    _VM_DT_AGE_MSG = _("Datum mimo povolený rozsah")
    
    _SPECIAL_VALUES = Type._SPECIAL_VALUES + ((None, ''),)

    DEFAULT_FORMAT = '%Y-%m-%d %H:%M:%S'
    """Implicitní formát data a času."""
    SQL_FORMAT = DEFAULT_FORMAT
    """Formát data a času používaný standardně SQL stroji."""
    CZECH_FORMAT = '%d.%m.%Y %H:%M:%S'
    """Český \"účetnický\" formát data a času."""

    if __debug__:
        _dt_type = type(DT.DateTimeFrom('2001-01-01'))

    def __init__(self, format=None, mindate=None, maxdate=None, utc=True, **kwargs):
        """Inicializuj instanci.

        Argumenty:

          format -- specifikace vstupního i výstupního formátu data a/nebo
            času, v podobě akceptované funkcí 'time.strftime()'; může být též
            'None', v kterémžto případě se použije konfigurační volba
            'config.date_time_format'.  Třída obsahuje předdefinované konstanty
            '*_FORMAT', které lze využít jako hodnotu tohoto parametru.
          mindate. maxdate -- omezení validity času
          utc -- specifies, if timestamp in database is in UTC

        """
        assert mindate is None or isinstance(mindate, types.StringTypes)
        assert maxdate is None or isinstance(maxdate, types.StringTypes)
        if format is None:
            import config
            format = config.date_time_format
        self._format = format
        self._mindate = mindate
        self._maxdate = maxdate
        self._utc = utc
        self._check_matcher = {}
        if mindate:
            try:
                self._mindate = DT.strptime(mindate, self.SQL_FORMAT)
            except:
                raise ProgramError('Bad value for mindate', mindate, self.SQL_FORMAT)
        if maxdate:
            try:
                self._maxdate = DT.strptime(maxdate, self.SQL_FORMAT)
            except:
                raise ProgramError('Bad value for maxdate', maxdate)                
        super(DateTime, self).__init__(**kwargs)
        
    def _check_format(self, format, string):
        try:
            matcher = self._check_matcher[format]
        except KeyError:
            special = {'%Y': r'\d\d\d\d', ' ': '\s+', '%p': '[AP]M'}
            def subst(match):
                m = match.group(1)
                try:
                    return special[m]
                except KeyError:
                    return m.startswith('%') and '\d?\d' or re.escape(m)
            regexp = re.sub('(\%[a-zA-Z]|.|\s+)', subst, format)
            self._check_matcher[format] = matcher = re.compile(regexp)
        return matcher.match(string)
        
    def _validate(self, string, format=None, local=True):
        """Stejné jako v předkovi až na klíčované argumenty.

        Argumenty:

          string -- stejné jako v předkovi
          format -- požadovaný formát hodnoty 'string', ve tvaru požadovaném
            metodou '__init__()'
          local -- pravdivé právě když zadaná hodnota je v lokálním čase;
            v opačném případě je v UTC
          
        """
        assert isinstance(string, types.StringTypes)
        if format is None:
            format = self._format
        # Využití `strptime' je nejjednodušší řešení.  GNU `strptime' je
        # dostatečně tolerantní vůči nadbytečným mezerám atd., takže by jeho
        # použitím neměl vzniknout problém, pokud nehodláme software provozovat
        # na ne-GNU systémech, které `strptime' řádně nepodporují.  Musíme
        # ovšem ořezat mezery zprava, protože v mx.DateTime vadí, je tam nějaká
        # chyba, standardní `time.strptime' funguje.
        string = string.strip()
        dt = None
        try:
            if not self._check_format(format, string):
                raise ValidationError(self.VM_DT_FORMAT)
            dt = DT.strptime(string, format)
            if local and self._utc:                
                dt = dt.gmtime()
            elif not local and not self._utc:
                dt = dt.localtime()
            if (self._mindate and dt < self._mindate) or \
                   (self._maxdate and dt > self._maxdate):
                result = None, self._validation_error(self.VM_DT_AGE)
            else:    
                result = Value(self, dt), None
        except Exception, e:
            result = None, self._validation_error(self.VM_DT_FORMAT)
        # TODO: zjistit, proč zde bylo uděláno toto omezení
        # Pro správnou funkčnost třídy Time je ale třeba ho zrušit
        #if dt is not None and dt.year < 1000:
        #    result = None, self._validation_error(self.VM_DT_AGE)
        return result
    
    def _export(self, value, local=True):
        """Stejné jako v předkovi až na klíčované argumenty.

        Argumenty:

          local -- pravdivé právě když zadaná hodnota je v lokálním čase;
            v opačném případě je v UTC
          
        """
        assert type(value) == self._dt_type, 'Value is not DateTime'
        if local and self._utc:
            value = value.localtime()
        elif not local and not self._utc:
            value = value.gmtime()
        return value.strftime(self._format)

    def format(self):
        return self._format
    
    def is_utc(self):
        return self._utc

    @classmethod
    def now(class_, **kwargs):
        """Vrať instanci 'Value' tohoto typu odpovídající aktuálnímu okamžiku.

        Argumenty:

          kwargs -- argumenty předané konstruktoru třídy typu
          
        """
        type = class_(**kwargs)
        return Value(type, DT.now())

    @staticmethod
    def current_gmtime():
        """Return current GM time suitable for use as this class value.
        """
        return DT.now().gmtime()


class Date(DateTime):
    """Datum bez časového údaje."""

    DEFAULT_FORMAT = '%Y-%m-%d'
    """Implicitní formát data."""
    SQL_FORMAT = DEFAULT_FORMAT
    """Formát data používaný standardně SQL stroji."""
    CZECH_FORMAT = '%d.%m.%Y'
    """Český \"účetnický\" formát data."""

    def __init__(self, format=None, **kwargs):
        """Inicializuj instanci.

        Argumenty:

          format -- specifikace vstupního i výstupního formátu data, v podobě
            akceptované funkcí 'time.strftime()'; může být též 'None',
            v kterémžto případě se použije konfigurační volba
            'config.date_format'.  Třída obsahuje předdefinované konstanty
            '*_FORMAT', které lze využít jako hodnotu tohoto parametru.

        """
        if format is None:
            import config
            format = config.date_format
        super(Date, self).__init__(format=format, **kwargs)

    def _validate(self, *args, **kwargs):
        kwargs['local'] = False
        return super(Date, self)._validate(*args, **kwargs)
        
    def _export(self, *args, **kwargs):
        kwargs['local'] = False
        return super(Date, self)._export(*args, **kwargs)


class Time(DateTime):
    """Čas bez datumového údaje."""

    DEFAULT_FORMAT = '%H:%M:%S'
    """Implicitní formát času."""
    SQL_FORMAT = DEFAULT_FORMAT
    """Formát času používaný standardně SQL stroji."""
    SHORT_FORMAT = '%H:%M'
    """Formát času bez zobrazení sekund."""

    def __init__(self, format=None, **kwargs):
        """Inicializuj instanci.

        Argumenty:

          format -- specifikace vstupního i výstupního formátu času, v podobě
            akceptované funkcí 'time.strftime()'; může být též 'None',
            v kterémžto případě se použije konfigurační volba
            'config.time_format'.  Třída obsahuje předdefinované konstanty
            '*_FORMAT', které lze využít jako hodnotu tohoto parametru.

        """
        if format is None:
            import config
            format = config.time_format
        super(Time, self).__init__(format=format, **kwargs)

    def _validate(self, *args, **kwargs):
        kwargs['local'] = False
        return super(Time, self)._validate(*args, **kwargs)
        
    def _export(self, *args, **kwargs):
        kwargs['local'] = False
        return super(Time, self)._export(*args, **kwargs)


class Boolean(Type):
    """Jednoduchý výčtový typ implementující hodnoty \"pravda\" a \"nepravda\".
    
    Za pravdu je považován string 'T', za nepravdu string 'F'; tyto stringy
    jsou uživatelskými hodnotami výčtu.  Odpovídající vnitřní hodnoty jsou
    blíže nespecifikované pythonové objekty s pythonovu sémantikou pravdy a
    nepravdy.

    Validační argument 'extended' umožňuje liberálnější kontrolu vstupu.  Je-li pravdivý, jsou
    kromě \"oficiálních\" hodnot 'object' zvalidovány i následující stringové hodnoty:
    
    \'t\', \'1\' -- jako reprezentace pravdivé hodnoty
    \'f\', \'0\' -- jako reprezentace nepravdivé hodnoty

    
    """

    _SPECIAL_VALUES = ((True, 'T'), (False, 'F'), (None, ''))
    
    def __init__(self, not_null=True):
        e = FixedEnumerator((True, False))
        super(Boolean, self).__init__(enumerator=e, not_null=not_null)

    def _validate(self, object, extended=False):
        print "->", object, extended
        if extended:
            if object in ('t', '1'):
                return Value(self, True), None
            elif object in ('f', '0'):
                return Value(self, False), None
        # Valid values are found in _SPECIAL_VALUES before _validate is called.
        return None, ValidationError(_("Neplatná vstupní hodnota typu boolean."))

    
    def default_value(self):
        return Value(self, False)

    
class Binary(Limited):
    """Binary data.

    External representation of a value of this type is either a Python 'buffer'
    object or 'None' (representing a null value).  Internal representation is
    an instance of the 'Buffer' class.  This is in general a wrapper of the
    Python buffer, which can add certain extended features, depending on the
    actual type of binary data contained within the buffer (such as report
    format or pixel size of an image, etc).  Thus each subclass of this type
    may define it's own 'Buffer' subclass with such extended features.

    Usage of binary data is limited only to certain situations.  They may be
    used only in non-key columns, they can be retrieved from a database (but
    they may not be used in search conditions with the exception of testing for
    NULL value) and they can be used as non-key values in insertions and
    updates.

    Values of this type are not cached as they may be large and their
    validation is trivial.

    """
    
    _VALIDATION_CACHE_LIMIT = 0
    _VM_MAXLEN_MSG = _("Překročena maximální velikost %(maxlen)s")
    
    class Buffer(object):
        """Wrapper of a buffer for internal representation of binary values.

        The primary purpose of this class is to provide further validation of
        binary data depending on their content.  This class accepts any data,
        but subclasses may exist, which only accept certain binary formats,
        such as images, documents, audio files etc.

        Methods for loading binary data from files or saving them are also
        provided, but these are mostly here for convenience.
        
        """
        def __init__(self, data, filename=None, type=None):
            """Initialize a new buffer instance and validate the input data.

            Arguemnts:
            
              data -- The buffer data.  It can be a Python 'buffer' object,
                input file path as a string or an open stream (a file-like
                object).  A buffer object is used directly, file path is opened
                and read and file-like object is just read (the caller is
                responsible for closing it).

              filename -- Filename for the buffer data as a string or None.
                This name does not include directory and does not refer to any
                actual file (has nothing to do with the input file for reading
                the data).  It may be used to suggest what the contents of the
                buffer is.  It is optional and its usage may be application
                specific.
                
              type -- MIME type of buffer data as a string or None.  It is
                optional and its usage may be application specific.

            Raises 'ValidationError' if the data don't conform to the binary
            format in use (depending on the actual 'Buffer' subclass).
            
            Raises 'IOError' if the input file can not be read.
            
            """
            self._path = None
            if isinstance(data, buffer):
                self._validate(data)
                self._buffer = data
            elif isinstance(data, (str, unicode)):
                self.load(data)
            elif isinstance(data, file):
                self._load(data)
            else:
                ProgramError("Invalid Buffer data:", data)
            assert filename is None or isinstance(filename, (str, unicode))
            assert type is None or isinstance(type, str)
            self._filename = filename
            self._type = type

        def __len__(self):
            return len(self._buffer)

        def _validate(self, data):
            if not isinstance(data, buffer):
                raise ValidationError(_("Not a buffer object: %r") % data)
            
        def _load(self, f):
            # Load and validate data from a file-like object.
            data = buffer(f.read())
            self._validate(data)
            self._buffer = data
                
        def buffer(self):
            """Return the binary data as a Python buffer instance."""
            return self._buffer

        def filename(self):
            """Return the suggested filename as passed to the constructor."""
            return self._filename

        def type(self):
            """Return the suggested filename as passed to the constructor."""
            return self._type

        def path(self):
            """Return the path to the input file or None.
            
            If the buffer was loaded from a file, the input path is returned as
            a string.  None is returned if the buffer was loaded from an input
            stream or buffer.  The application should not rely on the path to
            still exist nor should it operate the file.  The returned value may
            be used for preselecting the path in a dialog for saving the file
            or a similar purpos.  Note, that in the client-server environment,
            the path refers to the server filesystem, so might not be usable
            for the client at all.
            
            """
            return self._path

        def save(self, path):
            """Save the buffer data into a file.

            Arguemnts:
            
              path -- string path to the output file.
            
            Raises 'IOError' if the file can not be written.

            """
            f = open(path, 'wb')
            try:
                f.write(self._buffer)
            finally:
                f.close()
                
        def load(self, path, filename=None):
            """Try to load the buffer from a file replacing the current data.

            Arguemnts:
            
              path -- string path to the input file.
            
            Raises 'IOError' if the file can not be read.

            Raises 'ValidationError' if the data format is invalid.

            The original buffer contents remains unchanged in case of any error.
            
            """
            self._path = path
            f = open(path, 'rb')
            try:
                self._load(f)
            finally:
                f.close()
            if filename is not None:
                assert isinstance(filename, (str, unicode))
                self._filename = filename
            
                
    def __init__(self, enumerator=None, **kwargs):
        assert enumerator is None, ("Enumerators can not be used with binary data types")
        super(Binary, self).__init__(**kwargs)
        
    def _validate(self, object, filename=None, type=None, **kwargs):
        value = Value(self, self.Buffer(object, filename=filename, type=type))
        return value, None

    def _export(self, value):
        return value and value.buffer()

    def _format_length(self, length):
        return format_byte_size(length)
        
    
class Image(Binary, Big):
    """Binary type for generic bitmap images.

    The binary data of this type are represented by an 'Image.Buffer' instance.
    
    'Image.Buffer' validates the binary data to conform to one of input image
    formats supported by the Python Imaging Library.  It also provides the
    `image()' method, which returns the 'PIL.Image' instance corresponding to
    the image contained within the data.

    Image type can be further restricted to a list of allowed formats.  You may
    also restrict minimal/maximal pixel size of the image.

    Supported input formats include the most widely used formats such as 'PNG',
    'JPEG', 'TIFF', 'GIF', 'BMP', 'PCX', 'PPM', 'XBM' or 'IM'.  See Python
    Imaging Library documentation for the full list.

    The Python Imaging Library (PIL) must be installed when using this class.
    
    """
    
    VM_MAXSIZE = 'VM_MAXSIZE'
    _VM_MAXSIZE_MSG = _("Překročena maximální velikost %(maxsize)s pixelů")
    VM_MINSIZE = 'VM_MINSIZE'
    _VM_MINSIZE_MSG = _("Nedodržena minimální velikost %(minsize)s pixelů")
    VM_FORMAT = 'VM_FORMAT'
    _VM_FORMAT_MSG = _("Nepovolený formát %(format)s; povoleno: %(formats)s")
    
    class Buffer(Binary.Buffer):
        """A bufer for internal representation of bitmap image data.

        See the documentation of the 'Image' type for more information.

        """
        def _validate(self, data):
            super(Image.Buffer, self)._validate(data)
            import PIL.Image
            # The stream must stay open for the whole life of the Image object.
            f = StringIO(data)
            try:
                image = PIL.Image.open(f)
            except IOError:
                raise ValidationError(_("Neplatný grafický formát"))
            self._image = image
    
        def image(self):
            """Return the image as a 'PIL.Image' instance."""
            return self._image

    def __init__(self, minsize=(None, None), maxsize=(None, None),
                 formats=None, **kwargs):
        """Initialize the instance.
        
        Arguments:
        
          minsize -- maximal image size in pixels as a sequence of two integers
            (WIDTH, HEIGHT).  'None' in either value indicates an unlimited
            size in the corresponding direction...
          maxsize -- maximal image size in pixels; same rules as for 'minsize'
          formats -- list of allowed input formats as a sequence of strings,
            each string being one of PIL supported file formats, such as 'PNG',
            'JPEG', 'TIFF', 'GIF', 'BMP', 'PCX' etc.  Full list of the
            supported formats depends upon your PIL version.  If None, all
            formats supported by the Python Imaging Library are allowed.
          
        Other arguments are passed to the parent constructor.

        """
        if __debug__:
            for size in minsize, maxsize:
                assert isinstance(size, (tuple, list)) and len(size) == 2 and \
                       size[0] is None or isinstance(size[0], int) and \
                       size[1] is None or isinstance(size[1], int), size
            if formats is not None:
                assert isinstance(formats, (tuple, list)), formats
                for f in formats:
                    assert isinstance(f, str)
                    if f not in ('PNG', 'JPEG', 'TIFF', 'GIF', 'BMP',
                                 'PCX', 'PPM', 'XBM', 'IM'):
                        log(OPERATIONAL, "Suspicious image format:", f)
        self._minsize = tuple(minsize)
        self._maxsize = tuple(maxsize)
        self._formats = formats and tuple(formats)
        super(Image, self).__init__(**kwargs)

    def __cmp__(self, other):
        result = super(Image, self).__cmp__(other)
        if not result:
            result = cmp(self._minsize, other._minsize)
            if not result:
                result = cmp(self._maxsize, other._maxsize)
                if not result:
                    result = cmp(self._formats, other._formats)
        return result

    def minsize(self):
        """Return the minimal image size in pixels as a pair (WIDTH, HEIGHT).

        WIDTH and HEIGHT are integers or 'None' (denoting no limit).
        
        """
        return self._minsize

    def maxsize(self):
        """Return the maximal image size in pixels as a pair (WIDTH, HEIGHT).

        WIDTH and HEIGHT are integers or 'None' (denoting no limit).
        
        """
        return self._maxsize
    
    def formats(self):
        """Return the tuple of allowed input formats or None."""
        return self._formats
    
    def _check_constraints(self, value, **kwargs):
        super(Image, self)._check_constraints(value, **kwargs)
        if value is not None:
            image = value.image()
            for min,max,size in zip(self._minsize, self._maxsize, image.size):
                if min is not None and size < min:
                    raise self._validation_error(self.VM_MINSIZE,
                                               minsize='%sx%s' % self._minsize)
                if max is not None and size > max:
                    raise self._validation_error(self.VM_MAXSIZE,
                                               maxsize='%sx%s' % self._maxsize)
            if self._formats is not None and image.format not in self._formats:
                raise self._validation_error(self.VM_FORMAT,
                                             format=image.format,
                                             formats=', '.join(self._formats))
        

# Pomocné třídy

class Enumerator(object):
    """Realizace výčtu hodnot použitelného pro integritní omezení datového typu.

    Enumerátor je především poskytovatelem validace pro ověření, zda je nějaká
    hodnota přítomna v určité množině hodnot.  Způsob ověření a určení množiny
    hodnot je předmětem implementace různých tříd enumerátorů.  Ověřovaná
    hodnota je vždy vnitřní (Pythonovou) hodnotou typu, ve kterém je enumerátor
    použit.

    Instanci enumerátoru je potom možno předat konstruktoru datového typu a
    uvalit tak na daný typ příslušné integritní omezení.  Více informací také
    viz 'Type.__init__()'.
    
    Tato třída pouze definuje povinné rozhraní enumerátorů.  Kromě zde
    definovaných povinných metod mohou konkrétní třídy enumerátorů nabízet
    ještě další služby.

    Generally, enumerators must be thread-safe as thy can be used in shared 'Type' instances.  Any
    enumerator method which is not thread-safe must be clearly marked as such and it may not be
    used with enumerator instances used in types.
    
    """
    def check(self, value):
        """Vrať pravdu, pokud 'value' je prvkem množiny enumerátoru.

        Argumenty:
        
          value -- vnitřní (Pythonová) hodnota datového typu, pro který je
            enumerátor použit.
        
        """
        raise ProgramError('Not implemented', 'Enumerator.check()')

    def values(self):
        """Vrať sekvenci všech správných uživatelských hodnot typu."""
        raise ProgramError('Not implemented', 'Enumerator.values()')
    

class FixedEnumerator(Enumerator):
    """Enumerator with a fixed enumeration."""
    
    def __init__(self, enumeration):
        """Initialize the instance.
        
        Arguments:
        
          enumeration -- a sequence of values compatible with internal (Python) values of the type,
            for which the enumerator is used.
          
        """
        super(FixedEnumerator, self).__init__()
        self._enumeration = tuple(enumeration)

    def check(self, value):
        for v in self._enumeration:
            if v == value:
                return True
        return False

    def values(self):
        return self._enumeration
        
        
class DataEnumerator(Enumerator):
    """Enumerator retrieving the enumeration values from a data object.

    The enumerator uses one column of the data object to get the set of enumeration values.  This
    is typically the key column (by default), but it is possible to choose any other column by
    passing proper constructor arguments.

    """
    def __init__(self, data_factory, value_column=None, validity_column=None,
                 validity_condition=None):
        """Initialize the instance.
        
        Arguments:
        
          data_factory -- a 'DataFactory' instance for data object creation.

          value_column -- identifier of the column which provides the enumeration values.  If
            None, the key column is used.

          validity_column -- identifier of the column which determines valid rows (or None).  If
            defined, only rows with a true value in this column will be used for the enumeration
            (it must be a boolean column).  It is not possible to combine this argument with the
            'validity_condition' argument below.

          validity_condition -- a condition determining validity of data rows as a
            'pytis.data.Operator' instance (or None).  Only rows complying to this condition will
            be used for the enumeration.  This is a more general option than the 'validity_column'
            argument above.  It is not possible to combine these two arguments, but it is always
            possible to implement 'validity_column' within 'validity_condition'.
            
        """
        super(DataEnumerator, self).__init__()
        assert isinstance(data_factory, DataFactory), data_factory
        assert value_column is None or \
               isinstance(value_column, types.StringType)
        assert validity_column is None or \
               isinstance(validity_column, types.StringType) 
        assert validity_condition is None or \
               isinstance(validity_condition, pytis.data.Operator) \
               and validity_column is None
        self._data_factory = data_factory
        self._data_lock = thread.allocate_lock()
        self._value_column_ = value_column
        self._validity_column = validity_column
        if validity_column is not None:
            validity_condition = EQ(validity_column, Value(Boolean(), True))            
        self._validity_condition = validity_condition
        self._change_callbacks = []
        self._data_factory_kwargs = None

    def __getattr__(self, name):
        if name in ('_data', '_value_column'):
            self._complete()
            return self.__dict__[name]
        else:
            raise AttributeError(name)
        
    def _complete(self):
        # Finish the instance by data object creation.
        if self._data_factory_kwargs is None:
            import config
            self._data_factory_kwargs = dict(connection_data=config.dbconnection)
        self._data = data = self._data_factory.create(**self._data_factory_kwargs)
        if self._value_column_ is None:
            self._value_column = data.key()[0].id()
        else:
            self._value_column = self._value_column_
        c = data.find_column(self._value_column)
        assert c, ('Non-existent value column', self._value_column)
        self._value_column_type = c.type()
        for callback in self._change_callbacks:
            self._data.add_callback_on_change(callback)
        self._change_callbacks = []
        if __debug__:
            if self._validity_column is not None:
                c = data.find_column(self._validity_column)
                assert c, ('Non-existent validity column', self._validity_column)
                assert isinstance(c.type(), Boolean), ('Invalid validity column type', c)

    def _condition(self, condition=None):
        if self._validity_condition is not None:
            if condition is not None:
                return AND(condition, self._validity_condition)
            else:
                return self._validity_condition
        else:
            return condition

    def _retrieve(self, value, transaction=None, condition=None):
        the_condition = EQ(self._value_column, Value(self._value_column_type, value))
        validity_condition = self._condition(condition=condition)
        if validity_condition is not None:
            the_condition = AND(the_condition, validity_condition)
        def lfunction():
            data = self._data
            count = data.select(the_condition, transaction=transaction)
            if count > 1:
                raise ProgramError('Insufficient runtime filter for DataEnumerator',
                                   str(the_condition))
            row = data.fetchone(transaction=transaction)
            data.close()
            return row
        return with_lock(self._data_lock, lfunction)

    # Enumerator interface
    
    def check(self, value, transaction=None, condition=None):
        row = self._retrieve(value, transaction, condition)
        if row is None:
            result = False
        else:
            result = True
        return result

    def values(self, condition=None, transaction=None, sort=(), max=None):
        the_condition = self._condition(condition=condition)
        def lfunction():
            result = []
            count = self._data.select(condition=the_condition, transaction=transaction, sort=sort)
            if max is not None and count > max:
                self._data.close()
                return None
            while True:
                row = self._data.fetchone()
                if row is None:
                    self._data.close()
                    break
                result.append(row[self._value_column].value())
            return tuple(result)
        result = with_lock(self._data_lock, lfunction)
        return result
    
    # Extended interface.

    def add_callback_on_change(self, callback):
        # We don't want this co cause data object creation.  See also __getattr__().
        if self.__dict__.has_key('_data'):
            self._data.add_callback_on_change(callback)
        else:
            self._change_callbacks.append(callback)
        
    def data_factory(self):
        """Vrať specifikaci datového objektu enumerátoru jako instanci 'pytis.data.DataFactory'."""
        return self._data_factory
    
    def set_data_factory_kwargs(self, **kwargs):
        self._data_factory_kwargs = kwargs
    
    def value_column(self):
        """Vrať název sloupce datového objektu, který nese vnitřní hodnotu."""
        return self._value_column

    def validity_condition(self):
        """Return static condition determining validity of data rows."""
        return self._validity_condition
    
    def row(self, value, transaction=None, condition=None):
        """Return a *data* row corresponding to given codebook value.
        
        Arguments:
        
          value -- internal (Python) value of the enumerator's 'value_column'.  The row
            corresponding to this value is returned.
          transaction -- transaction for data operations.
          condition -- runtime filter condition for enumerator validation.
  
        Returns a 'pytis.data.Row' instance from the underlying data object.

        """
        return self._retrieve(value, transaction=transaction, condition=condition)
    
    def rows(self, transaction=None, condition=None):
        """Return sequence of rows of the underlying data object.

        Arguments:

          transaction -- transaction for data operations.
          condition -- runtime filter condition as an 'Operator' instance or None.

        """
        the_condition = self._condition(condition=condition)
        def lfunction():
            return self._data.select_map(identity, condition=the_condition, transaction=transaction)
        return with_lock(self._data_lock, lfunction)

    def type(self, column):
        """Vrať datový typ daného sloupce v datovém objektu enumerátoru."""
        return self._data.find_column(column).type()
    

class ValidationError(Exception):
    """Popis chyby při neúspěchu validace v 'Type.validate'.

    Popis lze získat veřejnou metodou 'message'.  Popisem je string a měl by
    být srozumitelný jako zpráva pro uživatele.
    
    """
    def __init__(self, message):
        """Inicializuj zprávu o chybě.

        Argumenty:
        
          message -- string obsahující zprávu o chybě pro uživatele
         
        """
        super_(ValidationError).__init__(self, message)
        self._message = message

    def __str__(self):
        return '<ValidationError: ' + self.message() + '>'

    def message(self):
        """Vrať zprávu o chybě jako string srozumitelný pro uživatele."""
        return self._message


class _Value(object):
    """Obecná reprezentace hodnoty daného typu.

    Každá hodnota se skládá z typu (instance třídy 'Type') a hodnoty samotné.
    Hodnota samotná může být cokoliv, bez ohledu na uvedený typ.

    """
    def __init__(self, type, value):
        """Inicializuj instanci.

        Argumenty:
        
          type -- instance třídy 'Type'
          value -- hodnota samotná, libovolný objekt

        """
        self._type = type
        self._value = value

    def __getstate__(self):
        return {'_type': self._type,
                '_value': self._value}

    def __setstate__(self, state):
        type = state['_type']
        if not isinstance(type, Type):
            raise InvalidAccessError('Invalid argument type', '_Value.type',
                                     type)
        self._type = type
        self._value = state['_value']
    
    def __unicode__(self):
        type = self.type()
        value = unicode (self.value())
        if isinstance(type, Big) and len(value) > 40:
            value = '%s...<<big value>>...' % (value[:10],)
        return '<%s: type=%s, value=%s>' % (self.__class__.__name__,
                                            type, value)

    def __cmp__(self, other):
        """Vrať 0, právě když 'self' a 'other' jsou shodné.

        'self' a 'other' jsou shodné, právě když se rovnají jejich typy a
        hodnoty.
        
        """
        if sameclass(self, other):
            t1, t2 = self.type(), other.type()
            if t1 == t2:
                res = 0
            else:
                res = compare_objects(t1, t2)
            if res:
                return res
            else:
                #return compare_objects(self.value(), other.value())
                return cmp(self.value(), other.value())
        else:
            return compare_objects(self, other)

    def __hash__(self):
        return hash(self._type) ^ hash(self._value)

    def type(self):
        """Vrať typ hodnoty jako instanci třídy 'Type' zadanou v '__init__()'.
        """
        return self._type

    def value(self):
        """Vrať hodnotu zadanou v '__init__()'."""
        return self._value


class Value(_Value):
    """Reprezentace hodnoty daného typu.

    Každá hodnota se skládá z typu (instance třídy 'Type') a hodnoty samotné.
    Hodnota samotná může být cokoliv, bez ohledu na uvedený typ.

    Pro zápis hodnoty do uživatelského rozhraní nebo databáze lze využít metodu
    'export()'.
    
    """
    _VOID = object()
    
    def __init__(self, type, value):
        """Inicializuj hodnotu daného typu.

        Argumenty:
        
          type -- instance třídy 'Type'
          value -- hodnota samotná, libovolný objekt
         
        """
        _Value.__init__(self, type, value)
        self._init()

    def __setstate__(self, state):
        super(Value, self).__setstate__(state)
        self._init()
        
    def _init(self):
        if self._type.__class__ == String:    # pozor, nebezpečná věc!
            e = self._value or ''
            self._exported = isinstance(e, unicode) and e or unicode(e)
        else:
            self._exported = self._VOID
        
    def export(self, *args, **kwargs):
        """Vrať stringovou reprezentaci hodnoty schopnou validace.

        Tato metoda je pouze zkratkou pro volání
        'self.type().export(self.value())'.  Pokud jsou metodě předány
        argumenty, je metoda `Type.export()' volána i s těmito argumenty.
        
        """
        # Abychom to zbytečně nekomplikovali, tak cachujeme pouze exporty bez
        # argumentů.
        if args or kwargs:
            exported = apply(self.type().export,
                             (self.value(),) + args, kwargs)
        else:
            exported = self._exported
            if exported is self._VOID:
                exported = self.type().export(self.value())
                self._exported = exported
        return exported


class WMValue(_Value):
    """Reprezentace specifikace pro wildcard match daného typu."""
