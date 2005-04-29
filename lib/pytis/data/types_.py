# -*- coding: iso-8859-2 -*-

# Datové typy
#
# Copyright (C) 2001, 2002, 2003, 2004, 2005 Brailcom, o.p.s.
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
    _VALIDATION_MESSAGES = {VM_NULL_VALUE: _("Prázdná hodnota")}
    _SPECIAL_VALUES = ()

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

    def __init__(self, not_null=False, constraints=(), validation_messages={}):
        """Inicializuj instanci.

        Argumenty:
        
          not_null -- příznak udávající, zda hodnoty tohoto typu smí být
            prázdné.  Za prázdnou hodnotu je považována hodnota None, nebo
            libovolná jiná hodnota na None mapovaná (viz. konstanta
            _SPECIAL_VALUES).  Pokud tento argument pravdivý, neprojde prázdná
            hodnota validací.
          constraints -- sekvence validačních funkcí.  Každá z těchto funkcí je
            funkcí jednoho argumentu, kterým je vnitřní hodnota typu.  Funkce
            pro tuto hodnotu musí vrátit buď 'None', je-li hodnota správná,
            nebo chybovou hlášku jako string v opačném případě.
          validation_messages -- dictionary identifikátorů a validačních
            hlášek.  Klíče jsou identifikátory validačních hlášek definované
            konstantami třídy s názvy začínajícími prefixem 'VM_' a hodnoty
            jsou hlášky coby stringy.  Hlášky z tohoto argumentu, jsou-li pro
            daný identifikátor definovány, mají přednost před implicitními
            hláškami definovanými typem.
          
        """
        super(Type, self).__init__()
        assert isinstance(not_null, types.BooleanType) 
        self._not_null = not_null
        self._constraints = xtuple(constraints)
        self._validation_messages = copy.copy(self._VALIDATION_MESSAGES)
        self._validation_messages.update(validation_messages)
        self._fetched = True
        # Cachujeme na úrovni instancí, protože ty jsou stejně sdílené, viz
        # `__new__'.
        self._validation_cache = LimitedCache(self._validating_provider)

    def _complete(self):
        """Dokonči všechny odložené inicializace instance.

        Po zavolání této metody musí být instance typu plně kompletní, včetně
        všech líných inicializací.

        """
        pass
        
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
        elif self._constraints == other._constraints \
                 and self._not_null == other._not_null:
            result = 0
        else:
            result = compare_objects(self, other)
        return result

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
                import config, pytis.remote, Pyro.core
                server = config.server
                if server is None:
                    raise AttributeError(name)
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
            t._complete()
            cache[id] = t
        self.__dict__.update(t.__dict__)
        self._id = id
        self._fetched = True
        try:
            return self.__dict__[name]
        except KeyError:
            raise AttributeError(name)
        
    def validate(self, object, strict=True, **kwargs):
        """Zvaliduj 'object' a vrať instanci třídy 'Value' a popis chyby.

        Argumenty:
        
          object -- objekt, který má být převeden na hodnotu
          strict -- pokud účelem validace není kontrola dat, ale pouhé
            převedení hodnoty na instanci Value, může být tento boolean příznak
            nastaven na nepravdivou hodnotu a validace tak bude tolerantnější.
            Nebodou například probíhat konroly constraintů apod.
          kwargs -- klíčové argumenty specifické pro konkrétní typ

        Vrací: dvojici (VALUE, ERROR).  VALUE je instance třídy 'Value' (je-li
        'object' správný) nebo je 'None' (je-li 'object' nesprávný).  Je-li
        'object' správný, je ERROR 'None', v opačném případě je instancí třídy
        'ValidationError', která obsahuje popis chyby.

        Většina typů vyžaduje, aby 'object' byl string, obvykle reprezentující
        vstup uživatele nebo hodnotu získanou ze souboru nebo od procesu.
        Některé sofistikovanější typy však akceptují nebo vyžadují 'object'
        jiného typu.  Všechny typy by měly, pokud možno, akceptovat string a
        pokud to nelze (například kvůli zbytečným komplikacím), měly by
        akceptovat objekt, který lze snadno zkonstruovat z dat převzatých
        z uživatelského rozhraní.  Pokud 'object' není string ani jiný typ
        explicitně povolený v dokumentaci příslušné typové třídy, je chování
        této metody nedefinováno.

        Argument 'kwargs' umožňuje konkrétnímu typu nabízet parametrizaci
        validace.  Například typ reprezentující reálná čísla může umožnit
        specifikovat parametry zaokrouhlení.  Takto lze upravovat validaci
        uživatelských vstupů různými parametry, které nejsou součástí
        specifikace typu.

        V této třídě metoda vrací vždy dvojici (VALUE, None), kde VALUE má za
        svoji hodnotu 'None'.  Potomci třídy nechť tuto metodu
        nepředefinovávají, neboť metoda může provádět i různé doplňující akce;
        nechť potomci předefinovávají metodu `_validate()'.

        Není-li v dokumentaci konkrétního typu řečeno jinak, akceptuje prázdný
        string stejně jako 'None' jako speciální prázdnou hodnotu, pro kterou
        vrací dvojici (VALUE, None), kde VALUE má za svoji hodnotu 'None'.
        
        """
        # Tato metoda je zároveň používána i pro převod hodnot získaných
        # z databázového rozhraní.  To není úplně ideální, ale je to zatím
        # postačující a rozlišení těchto dvou použití nestojí za komplikace
        # s tím spojené (použití pro databáze je omezeno na modul `dbdata').
        # Pokud by bylo potřeba v budoucnu toto rozlišit, lze přidat další
        # metodu nebo argument.  Nyní je to částečně řešeno argumentem
        # 'strict'.
        try:
            key = (object, strict, tuple(kwargs.items()))
            result = self._validation_cache[key], None
        except ValidationError, e:
            result = None, e
        return result

    def _validating_provider(self, key):
        object, strict, kwargs = key[0], key[1], dict(key[2])
        special = rassoc(object, self._SPECIAL_VALUES)
        if special:
            value = Value(self, special[0])
        elif object is None:
            value = Value(self, None)
        else:
            value, error = apply(self._validate, (object,), kwargs)
            if error: raise error
        if strict:
            self._check_constraints(value.value())
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
        assert isinstance(object, types.StringTypes)
        return WMValue(self, object), None

    def _validation_error(self, id, *args):
        message = self._validation_messages[id] % args
        return ValidationError(message)
        
    def _check_constraints(self, value):
        if self._not_null and value is None:
            raise self._validation_error(self.VM_NULL_VALUE)
        for c in self._constraints:
            cresult = c(value)
            if cresult is not None:
                raise ValidationError(cresult)

    def constraints(self):
        """Vrať constraints zadaná v konstruktoru."""
        return self._constraints

    def not_null(self):
        """Vrať pravdu, pokud hodnoty tohoto typu smí být prázdné."""
        return self._not_null

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
        nullsub = assoc(None, self._SPECIAL_VALUES)
        if nullsub is not None and is_sequence(exported):
            return tuple(map(lambda v, n=nullsub[1]: v or n, exported))
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


class MutableType(object):
    """Abstraktní typová třída, kterou povinně dědí všechny mutable typy.

    Mutable typ je takový, jehož validační a exportní metody mohou pro tutéž
    instanci dávat v čase rozdílné výsledky.

    """
    __metaclass__ = _MType
    
    def _update(self, force=False):
        """Updatuj data typu.

        Bezprostředně po zavolání této metody by měla validace a exporty
        pracovat s aktuálními daty.

        Argumenty:

          force -- právě když je pravda, je update vynucený, jinak může a
            nemusí být proveden, většinou v závislosti na náročnosti operace

        Vrací: Pravdu, právě když update dat byl skutečně proveden.

        """
        return True

    def _with_update(self, function, args=(), kwargs={}):
        updated = self._update()
        value, ok = apply(function, args, kwargs)
        if not ok and not updated:
            self._update(force=True)
            value, __ = apply(function, args, kwargs)
        return value
        

class Number(Type):
    """Abstraktní typová třída, která je základem všech numerických typů.

    Třída víceméně nic nového nedefinuje, je určena pouze k podědění všemi
    numerickými typy, aby tyto byly jakožto číselné snadno rozpoznatelné.

    """
    _SPECIAL_VALUES = Type._SPECIAL_VALUES + ((None, ''),)
    
class Integer(Number):
    """Libovolný integer."""

    VM_NONINTEGER = 'VM_NONINTEGER'
    _VALIDATION_MESSAGES = Number._VALIDATION_MESSAGES
    _VALIDATION_MESSAGES.update({VM_NONINTEGER: _("Není to celé číslo")})
    
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
    _VALIDATION_MESSAGES = Number._VALIDATION_MESSAGES
    _VALIDATION_MESSAGES.update(
        {VM_INVALID_NUMBER: _("Není to povolené číslo")})
    
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

class String(Type):
    """Libovolný string.

    Lze také specifikovat, že řetězec může mít pouze omezenou délku, blíže viz
    metody '__init__' a 'maxlen'.

    """    

    VM_STRING_TOO_LONG = 'VM_STRING_TOO_LONG'
    _VALIDATION_MESSAGES = Type._VALIDATION_MESSAGES
    _VALIDATION_MESSAGES.update(
      {VM_STRING_TOO_LONG: _("Řetězec je příliš dlouhý: (%s, %s)")})

    _SPECIAL_VALUES = Type._SPECIAL_VALUES + ((None, ''),)
    
    def __init__(self, maxlen=None, **kwargs):
        """Definuj stringový typ maximální délky.

        Argumenty:
        
          maxlen -- maximální délka stringu jako integer, nebo 'None';
            pokud je 'None', délka stringu není omezena
             
        Ostatní klíčové argumenty jsou shodné, jako v předkovi.

        """
        super(String, self).__init__(**kwargs)
        self._maxlen = maxlen

    def __cmp__(self, other):
        """Vrať 0, právě když 'self' a 'other' jsou téže třídy a max. délky."""
        result = super(String, self).__cmp__(other)
        if not result:
            result = cmp (self.maxlen(), other.maxlen())
        return result

    def maxlen(self):
        """Vrať maximální povolenou délku stringu jako integer, nebo 'None'.

        'None' značí, že délka řetězce není omezená.
        
        """
        return self._maxlen

    def _check_constraints(self, value):
        super(String, self)._check_constraints(value)
        self._check_maxlen(value)

    def _check_maxlen(self, value):
        if value is not None and self._maxlen is not None \
               and len(value) > self._maxlen:
            raise self._validation_error(self.VM_STRING_TOO_LONG,
                                         self._maxlen, value)
        
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
        return unicode(value)


class Color(String):
    """Barva reprezentovaná řetězcem '#RRGGBB'."""

    VM_COLOR_FORMAT = 'VM_COLOR_FORMAT'
    _VALIDATION_MESSAGES = Type._VALIDATION_MESSAGES
    _VALIDATION_MESSAGES.update(
        {VM_COLOR_FORMAT: _("Formát barvy neodpovídá masce '#RRGGBB'.")})
    
    _VALIDATION_REGEX = re.compile('^\#[0-9a-hA-H]{6,6}$')

    def _validate(self, string, *args, **kwargs):
        value, error = super(Color, self)._validate(string, *args, **kwargs)
        if error is None and self._VALIDATION_REGEX.match(string) is None:
            value, error = None, self._validation_error(self.VM_COLOR_FORMAT)
        return value, error
            
class DateTime(Type):
    """Časový okamžik reprezentovaný instancí třídy 'DateTime.DateTime'.

    Třída je schopna pracovat pouze s absolutním časovým okamžikem.  Čas je
    navíc vždy uvažován v UTC, časové zóny nejsou podporovány.  Datum je
    podporováno pouze od roku 1000 dále.

    Formát data a času je shodný pro import a export a je dán parametrem
    'format' metody '__init__()'.
    
    """
    VM_DT_FORMAT = 'VM_DT_FORMAT'
    VM_DT_VALUE = 'VM_DT_VALUE'
    VM_DT_AGE = 'VM_DT_AGE'
    _VALIDATION_MESSAGES = Type._VALIDATION_MESSAGES
    _VALIDATION_MESSAGES.update(
        {VM_DT_FORMAT: _("Chybný formát data nebo času"),
         VM_DT_VALUE: _("Chybné datum nebo čas"),
         VM_DT_AGE: _("Datum mimo povolený rozsah")})
    
    _SPECIAL_VALUES = Type._SPECIAL_VALUES + ((None, ''),)

    DEFAULT_FORMAT = '%Y-%m-%d %H:%M:%S'
    """Implicitní formát data a času."""
    SQL_FORMAT = DEFAULT_FORMAT
    """Formát data a času používaný standardně SQL stroji."""
    CZECH_FORMAT = '%d.%m.%Y %H:%M:%S'
    """Český \"účetnický\" formát data a času."""

    if __debug__:
        _dt_type = type(DT.DateTimeFrom('2001-01-01'))

    def __init__(self, format=None, mindate=None, maxdate=None, **kwargs):
        """Inicializuj instanci.

        Argumenty:

          format -- specifikace vstupního i výstupního formátu data a/nebo
            času, v podobě akceptované funkcí 'time.strftime()'; může být též
            'None', v kterémžto případě se použije konfigurační volba
            'config.date_time_format'.  Třída obsahuje předdefinované konstanty
            '*_FORMAT', které lze využít jako hodnotu tohoto parametru.
          mindate. maxdate -- omezení validity času

        """
        assert mindate is None or isinstance(mindate, types.StringTypes)
        assert maxdate is None or isinstance(maxdate, types.StringTypes)
        if format is None:
            import config
            format = config.date_time_format
        self._format = format
        self._mindate = mindate
        self._maxdate = maxdate
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
            special = {'%Y': r'\d\d\d\d', ' ': '\s+'}
            def subst(match):
                m = match.group(1)
                try:
                    return special[m]
                except KeyError:
                    return m.startswith('%') and '\d?\d' or re.escape(m)
            regexp = re.sub('(\%[a-zA-Z]|.|\s+)', subst, format)
            self._check_matcher[format] = matcher = re.compile(regexp)
        if not matcher.match(string):
            raise ValidationError(self.VM_DT_FORMAT)
        
    def _check_format(self, format, string):
        try:
            matcher = self._check_matcher[format]
        except KeyError:
            special = {'%Y': r'\d\d\d\d', ' ': '\s+'}
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
            if local:
                dt = dt.gmtime()
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
        if local:
            value = value.localtime()
        return value.strftime(self._format)

    def now(class_, **kwargs):
        """Vrať instanci 'Value' tohoto typu odpovídající aktuálnímu okamžiku.

        Argumenty:

          kwargs -- argumenty předané konstruktoru třídy typu
          
        """
        type = class_(**kwargs)
        return Value(type, DT.now())
    now = classmethod(now)


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


class Enumeration(Type):
    """Výčtový typ.

    Hodnoty tohoto typu jsou dány fixním nebo v čase variabilním výčtem.

    """

    VM_INVALID_VALUE =  'VM_INVALID_VALUE'
    _VALIDATION_MESSAGES = Type._VALIDATION_MESSAGES
    _VALIDATION_MESSAGES.update({VM_INVALID_VALUE: _("Nesprávná hodnota")})

    def values(self):
        """Vrať sekvenci všech správných uživatelských hodnot typu.

        V této třídě metoda vyvolá výjimku 'ProgramError', předpokládá se její
        předefinování v potomcích.
        
        """
        raise ProgramError('Not implemented', 'Enumeration.values')
    
    
class FixedEnumeration(Enumeration):
    """Výčtový typ.

    Hodnoty tohoto typu mohou být pouze prvky pevně stanovené množiny zadané
    jako parametr konstruktoru této třídy.  Blíže viz metoda '__init__'.
    
    """
    
    def __init__(self, enumeration, **kwargs):
        """Inicializuje výčtový typ dle specifikace 'enumeration'.

        Argumenty:
        
          enumeration -- specifikace výčtových hodnot
          
        Ostatní klíčové argumenty jsou shodné, jako v předkovi.

        Specifikace má podobu sekvence dvouprvkových sekvencí.  První prvek
        každé dvouprvkové sekvence je objekt (ne nutně string) odpovídající
        reprezentaci hodnoty v uživatelském rozhraní (\"uživatelská hodnota\")
        a druhý prvek je libovolný objekt reprezentující hodnotu interně
        (\"interní hodnota\").

        Korespondence mezi uživatelskými a interními hodnotami by měla být 1:1.
        Je-li tato korespondence 1:N, třída nemusí pracovat správně.  Naproti
        tomu korespondence N:1 je legální, je však třeba si uvědomit, že
        všechny uživatelské hodnoty pak mohou být nabízeny uživateli pro výběr
        v uživatelském rozhraní, což není vždy žádoucí.

        Typ zachovává konvenci, že prázdný string odpovídá nedefinované
        hodnotě.

        Příklad specifikace:
          (('Čeština', 'cs'), ('Angličtina', 'en'), ('Němčina', 'de'))
        
        """
        super(FixedEnumeration, self).__init__(**kwargs)
        self._enumeration = enumeration

    def __cmp__(self, other):
        """Vrať 0, právě když 'self' a 'other' jsou shodné.

        'self' a 'other' jsou shodné, právě když jsou téže třídy a jejich
        enumerations se rovnají.
        
        """
        result = super(FixedEnumeration, self).__cmp__(other)
        if not result:
            result = cmp(self._enumeration, other._enumeration)
        return result
        
    def values(self):
        """Vrať sekvenci všech správných uživatelských hodnot typu."""
        return map(lambda x: x[0], self._enumeration)
    
    def _validate(self, object):
        """Vrať instanci třídy 'Value' s hodnotou definovanou pro 'object'.

        'object' je správný právě tehdy, je-li jednou z uživatelských hodnot
        definovaných v metodě '__init__'.  Blíže o mapování objektů na hodnoty
        viz metoda '__init__'.

        """
        for s, v in self._enumeration:
            if s == object:
                value = v
                break
        else:
            return None, self._validation_error(self.VM_INVALID_VALUE)
        return Value(self, value), None

    def _export(self, value):
        for s, v in self._enumeration:
            if v == value:
                return s
        else:
            raise ProgramError('Invalid internal value', value)
        
    def default_value(self):
        if self._enumeration:
            return Value(self, self._enumeration[0][1])
        else:
            return super(FixedEnumeration, self).default_value()


class Boolean(FixedEnumeration):
    """Jednoduchý výčtový typ implementující hodnoty \"pravda\" a \"nepravda\".

    Za pravdu je považován string 'T', za nepravdu string 'F'; tyto stringy
    jsou uživatelskými hodnotami výčtu.  Odpovídající vnitřní hodnoty jsou
    blíže nespecifikované pythonové objekty s pythonovu sémantikou pravdy a
    nepravdy.
    
    """
    def __init__(self, **kwargs):
        super(Boolean, self).__init__((('F', False), ('T', True)), **kwargs)

    def _validate(self, object, extended=False):
        """Vrať instanci třídy 'Value' s hodnotou definovanou pro 'object'.

        Je-li argument 'extended' pravdivý, jsou kromě \"oficiálních\" hodnot
        'object' zvalidovány i následující stringové hodnoty:

          \'t\', \'1\' -- jako reprezentace pravdivé hodnoty
          \'f\', \'0\', \'\' -- jako reprezentace nepravdivé hodnoty
        
        """
        if not extended and isinstance(object, str):
            object = string.strip(object)
            if object in ('t', '1'):
                object = 'T'
            elif object in ('', 'f', '0'):
                object = 'F'
        return super(Boolean, self)._validate(object)


class Sequence(Type):
    """Typ, jehož hodnotou je tuple hodnot, instancí třídy 'Value'.

    Typ je charakterizován tuplem instancí třídy 'Type' definujícím typy
    v tuple hodnot.  Hodnoty musí typové specifikaci odpovídat.  Mezi
    jednotlivými typy v tuple typů nesmí být typ 'Sequence'.

    """
    
    VM_NONTUPLE = 'VM_NONTUPLE'
    VM_ARG_NUMBER = 'VM_ARG_NUMBER'
    _VALIDATION_MESSAGES = Type._VALIDATION_MESSAGES
    _VALIDATION_MESSAGES.update({VM_NONTUPLE: _("Objekt není tuple"),
                                 VM_ARG_NUMBER: _("Chybný počet argumentů")})
    
    def __init__(self, subtypes, **kwargs):
        """Vytvoř typ složený ze 'subtypes'.

        Argumenty:

          subtypes -- seznam instancí třídy 'Type', který definuje typy prvků
            hodnoty typu.
            
        Ostatní klíčové argumenty jsou shodné, jako v předkovi.

        """
        assert not filter(lambda t: isinstance(t, Sequence), subtypes), \
               'Sequence type within subtypes'
        super(Sequence, self).__init__(**kwargs)
        self._subtypes = subtypes

    def __cmp__(self, other):
        """Vrať 0, právě když 'self' a 'other' jsou shodné.

        'self' a 'other' jsou shodné, právě když jsou téže třídy a jejich
        tuples typů položek se rovnají.
        
        """
        result = super(Sequence, self).__cmp__(other)
        if not result:
            result = (self._subtypes == other._subtypes)
        return result

    def __len__(self):
        """Vrať počet prvků tuple typové specifikace."""
        return len(self._subtypes)
        
    def _validate(self, object):
        if type(object) != type(()):
            return None, self._validation_error(self.VM_NONTUPLE)
        subtypes = self._subtypes
        if len(object) != len(subtypes):
            return None, ValidationError(VM_ARG_NUMBER)
        vlist = []
        for t, o in zip(subtypes, object):
            v, e = t.validate(o)
            if e:
                return None, e
            vlist.append(v)
        return Value(self, tuple(vlist)), None

    def _export(self, value):
        """Vrať tuple stringů odpovídajících exportům hodnot 'value'."""
        assert type(value) == type(()), ('value not a tuple', value)
        vlist = map(lambda v: v.type().export(v.value()), value)
        return tuple(vlist)



# Pomocné třídy


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
        self._init()
    
    def __str__(self):
        return '<%s: type=%s, value=%s>' % (self.__class__.__name__,
                                            self.type(), self.value())

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
                return compare_objects(self.value(), other.value())
        else:
            return compare_objects(self, other)

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
        super(Value, self).__init__(type, value)
        self._init()

    def _init(self):
        if self._type.__class__ == String:    # pozor, nebezpečná věc!
            self._exported = unicode(self._value or '')
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
