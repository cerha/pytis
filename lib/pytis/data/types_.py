# -*- coding: iso-8859-2 -*-

# Datov� typy
#
# Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006 Brailcom, o.p.s.
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

"""Pr�ce s�datov�mi typy, jejich hodnotami a vn�j�� reprezentac�.

Z�kladn� ideou modulu je, �e uvnit� programu v�dy pracujeme s�hodnotami
ur�it�ho, n�mi definovan�ho, typu.  Z�d�vodu datov� abstrakce p�i pr�ci s�daty
nepou��v�me p��mo standardn� typy Pythonu a jeho knihoven, n�br� na�e vlastn�
ob�lky okolo nich, kter� n�m zajist� nez�vislost v��i konkr�tn� reprezentaci
hodnot dan�ho typu v�r�zn�ch ��stech programu (PostgreSQL, wxWindows,�...).
Krom� toho n�m tyto typov� ob�lky mohou tak� poskytovat n�kter� dopl�uj�c�
funkce souvisej�c� s�typy dat, jako je nap��klad validace vstupn� hodnoty
dan�ho typu reprezentovan� stringem a jej� p�evod na intern� reprezentaci, se
kterou d�le v�programu pracujeme.

Z�kladem modulu je abstraktn� t��da 'Type', kter� je spole�n�m z�kladem v�ech
typov�ch t��d.  Jej�m pod�d�n�m vznikaj� konkr�tn� typy nebo jejich spole�n�
specializovan�j�� z�klady.  Hodnoty dan�ch typ� jsou pak reprezentov�ny
instancemi samostatn� t��dy 'Value'.

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
    """Abstraktn� t��da slou��c� jako spole�n� z�klad v�ech typ�.

    Tuto t��du mus� povinn� d�dit v�echny typov� t��dy.

    Instance t�to t��dy jsou pova�ov�ny za immutable, nesm� b�t po sv�
    inicializaci modifikov�ny a mohou b�t neomezen� sd�leny.
    
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
    _VALIDATION_MESSAGES = {VM_NULL_VALUE: _("Pr�zdn� hodnota"),
                            VM_INVALID_VALUE: _("Nespr�vn� hodnota")}
    
    _SPECIAL_VALUES = ()

    def _make(class_, *args, **kwargs):
        result = Type._type_table.get_instance(class_, *args, **kwargs)
        assert result is not None
        return result
    _make = staticmethod(_make)

    def make(class_, *args, **kwargs):
        """Pouze pro ��ely zp�tn� kompatibility a pro metat��du.

        V�nov�m k�du nepou��vat.

        """
        return class_._make(class_, *args, **kwargs)
    make = classmethod(make)

    def __init__(self, not_null=False, enumerator=None, constraints=(),
                 validation_messages=None):
        """Inicializuj instanci.

        Argumenty:
        
          not_null -- p��znak ud�vaj�c�, zda hodnoty tohoto typu sm� b�t
            pr�zdn�.  Za pr�zdnou hodnotu je pova�ov�na hodnota None, nebo
            libovoln� jin� hodnota na None mapovan� (viz. konstanta
            _SPECIAL_VALUES).  Pokud tento argument pravdiv�, neprojde pr�zdn�
            hodnota validac�.
          enumerator -- specifikace enumer�toru, jako instance `Enumerator',
            nebo None.  Slou�� k realizaci integritn�ch omezen� v��tov�ho
            typu.  V�ce viz dokumentace t��dy `Enumerator'.
          constraints -- sekvence valida�n�ch funkc� slou��c�ch k realizaci
            libovoln�ch integritn�ch omezen�.  Ka�d� z�t�chto funkc� je funkc�
            jednoho argumentu, kter�m je vnit�n� hodnota typu.  Funkce pro tuto
            hodnotu mus� vr�tit bu� 'None', je-li hodnota spr�vn�, nebo
            chybovou hl�ku jako string v�opa�n�m p��pad�.
          validation_messages -- dictionary identifik�tor� a valida�n�ch
            hl�ek.  Kl��e jsou identifik�tory valida�n�ch hl�ek definovan�
            konstantami t��dy s�n�zvy za��naj�c�mi prefixem 'VM_' a hodnoty
            jsou hl�ky coby stringy.  Hl�ky z�tohoto argumentu, jsou-li pro
            dan� identifik�tor definov�ny, maj� p�ednost p�ed implicitn�mi
            hl�kami definovan�mi typem.


        """
        super(Type, self).__init__()
        assert isinstance(not_null, types.BooleanType)
        assert enumerator is None or isinstance(enumerator, Enumerator)
        assert isinstance(constraints, (types.ListType, types.TupleType))
        assert validation_messages is None or \
               isinstance(validation_messages, types.DictType) 
        self._not_null = not_null
        self._enumerator = enumerator
        self._constraints = xtuple(constraints)
        self._validation_messages = copy.copy(self._VALIDATION_MESSAGES)
        if validation_messages is not None:
            self._validation_messages.update(validation_messages)
        self._fetched = True
        # Cachujeme na �rovni instanc�, proto�e ty jsou stejn� sd�len�, viz
        # `__new__'.
        self._validation_cache = cache = LimitedCache(self._validating_provider)
        if isinstance(enumerator, MutableEnumerator):
            # TODO: Jak se to bude chovat po smrti instance typu?
            enumerator.add_hook_on_update(lambda : cache.reset())

    def type_table(class_):
        """Vra� tabulku typ� jako instanci '_TypeTable'.

        Jedin� ��el t�to metody je zp��stupnit tabulku typ� pro vzd�len�
        p�ed�v�n� typ� ze serveru na klienta.  Pro jin� ��ely by tabulka typ�
        nem�la b�t pou��v�na.

        """
        return Type._type_table
    type_table = classmethod(type_table)
    
    def __cmp__(self, other):
        """Vra� 0, pr�v� kdy� 'self' a 'other' reprezentuj� tent�� typ."""
        if not sameclass(self, other):
            result = compare_objects(self, other)
        elif self._id == other._id:
            result = 0
        elif self._constraints == other._constraints \
                 and self._not_null == other._not_null \
                 and cmp(self._enumerator, other._enumerator) == 0:
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
            cache[id] = t
        self.__dict__.update(t.__dict__)
        self._id = id
        self._fetched = True
        try:
            return self.__dict__[name]
        except KeyError:
            raise AttributeError(name)

    def validate(self, object, strict=True, **kwargs):
        """Zvaliduj 'object' a vra� instanci t��dy 'Value' a popis chyby.

        Argumenty:
        
          object -- objekt, kter� m� b�t p�eveden na hodnotu
          strict -- pokud ��elem validace nen� kontrola dat, ale pouh�
            p�eveden� hodnoty na instanci Value, m��e b�t tento boolean p��znak
            nastaven na nepravdivou hodnotu a validace tak bude tolerantn�j��.
            Nebodou nap��klad prob�hat konroly constraint� apod.
          kwargs -- kl��ov� argumenty specifick� pro konkr�tn� typ

        Vrac�: dvojici (VALUE, ERROR).  VALUE je instance t��dy 'Value' (je-li
        'object' spr�vn�) nebo je 'None' (je-li 'object' nespr�vn�).  Je-li
        'object' spr�vn�, je ERROR 'None', v�opa�n�m p��pad� je instanc� t��dy
        'ValidationError', kter� obsahuje popis chyby.

        V�t�ina typ� vy�aduje, aby 'object' byl string, obvykle reprezentuj�c�
        vstup u�ivatele nebo hodnotu z�skanou ze souboru nebo od procesu.
        N�kter� sofistikovan�j�� typy v�ak akceptuj� nebo vy�aduj� 'object'
        jin�ho typu.  V�echny typy by m�ly, pokud mo�no, akceptovat string a
        pokud to nelze (nap��klad kv�li zbyte�n�m komplikac�m), m�ly by
        akceptovat objekt, kter� lze snadno zkonstruovat z�dat p�evzat�ch
        z�u�ivatelsk�ho rozhran�.  Pokud 'object' nen� string ani jin� typ
        explicitn� povolen� v�dokumentaci p��slu�n� typov� t��dy, je chov�n�
        t�to metody nedefinov�no.

        Argument 'kwargs' umo��uje konkr�tn�mu typu nab�zet parametrizaci
        validace.  Nap��klad typ reprezentuj�c� re�ln� ��sla m��e umo�nit
        specifikovat parametry zaokrouhlen�.  Takto lze upravovat validaci
        u�ivatelsk�ch vstup� r�zn�mi parametry, kter� nejsou sou��st�
        specifikace typu.

        V�t�to t��d� metoda vrac� v�dy dvojici (VALUE, None), kde VALUE m� za
        svoji hodnotu 'None'.  Potomci t��dy nech� tuto metodu
        nep�edefinov�vaj�, nebo� metoda m��e prov�d�t i�r�zn� dopl�uj�c� akce;
        nech� potomci p�edefinov�vaj� metodu `_validate()'.

        Nen�-li v�dokumentaci konkr�tn�ho typu �e�eno jinak, akceptuje pr�zdn�
        string stejn� jako 'None' jako speci�ln� pr�zdnou hodnotu, pro kterou
        vrac� dvojici (VALUE, None), kde VALUE m� za svoji hodnotu 'None'.
        
        """
        # Tato metoda je z�rove� pou��v�na i�pro p�evod hodnot z�skan�ch
        # z�datab�zov�ho rozhran�.  To nen� �pln� ide�ln�, ale je to zat�m
        # posta�uj�c� a rozli�en� t�chto dvou pou�it� nestoj� za komplikace
        # s�t�m spojen� (pou�it� pro datab�ze je omezeno na modul `dbdata').
        # Pokud by bylo pot�eba v�budoucnu toto rozli�it, lze p�idat dal��
        # metodu nebo argument.  Nyn� je to ��ste�n� �e�eno argumentem
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

          object -- validovan� objekt, string

        Vrac�: dvojici (VALUE, ERROR).  VALUE je instance t��dy 'WMValue'
        (je-li 'object' spr�vn�) nebo je 'None' (je-li 'object' nespr�vn�).
        Je-li 'object' spr�vn�, je ERROR 'None', v�opa�n�m p��pad� je instanc�
        t��dy 'ValidationError', kter� obsahuje popis chyby.

        Ne v�echny typy mus� tento druh validace podporovat.  Ty, kter�
        nepodporuj�, vrac� dvojici (None, ERROR).

        """
        assert isinstance(object, types.StringTypes)
        return WMValue(self, object), None

    def _validation_error(self, id, *args):
        message = self._validation_messages[id] % args
        return ValidationError(message)
        
    def _check_constraints(self, value):
        if value is None:
            if self._not_null:
                raise self._validation_error(self.VM_NULL_VALUE)
            else:
                return True
        if self._enumerator is not None and not self._enumerator.check(value):
            raise self._validation_error(self.VM_INVALID_VALUE)
        for c in self._constraints:
            cresult = c(value)
            if cresult is not None:
                raise ValidationError(cresult)

    def not_null(self):
        """Vra� pravdu, pokud hodnoty tohoto typu sm� b�t pr�zdn�."""
        return self._not_null

    def enumerator(self):
        """Vra� enumer�tor sv�zan� s t�mto typem."""
        return self._enumerator
            
    def export(self, value, *args, **kwargs):
        """Vra� stringovou reprezentaci 'value' schopnou validace.

        'value' je hodnota vr�cen� metodou 'Value.value' z�objektu vytvo�en�ho
        v�metod� 'validate' na z�klad� *string* parametru.  Pro objekty vznikl�
        z�argumentu 'validate', kter� nebyl string, je chov�n� t�to metody
        nedefinov�no.  V�jimkou je hodnota 'None', pro kterou v�echny typy,
        v�jejich� dokumentaci nen� �e�eno jinak, vrac� pr�zdn� string.

        V�t�to t��d� metoda vrac� v�sledek oper�toru '``', pouze pro 'value'
        rovno 'None' vrac� pr�zdn� string.  Potomci t��dy nech� tuto metodu
        nep�edefinov�vaj�, nebo� metoda m��e prov�d�t i�r�zn� dopl�uj�c� akce;
        nech� potomci p�edefinov�vaj� metodu `_export()'.

        """
        special = assoc(value, self._SPECIAL_VALUES)
        if special:
            return special[1]
        exported = apply(self._export, (value,)+args, kwargs)
        return exported

    def _export(self, value):
        return `value`

    def default_value(self):
        """Vra� implicitn� hodnotu typu jako instanci 'Value'.

        Pokud takov� hodnota neexistuje nebo nem� smysl, vra� 'None'.
        
        Vr�cen� hodnota m��e b�t vyu�ita nap��klad v�inicializaci nov�ch
        z�znam�.

        """
        return Value(self, None)
  

class Number(Type):
    """Abstraktn� typov� t��da, kter� je z�kladem v�ech numerick�ch typ�.

    T��da v�cem�n� nic nov�ho nedefinuje, je ur�ena pouze k�pod�d�n� v�emi
    numerick�mi typy, aby tyto byly jako�to ��seln� snadno rozpoznateln�.

    """
    _SPECIAL_VALUES = Type._SPECIAL_VALUES + ((None, ''),)

    
class Integer(Number):
    """Libovoln� integer."""

    VM_NONINTEGER = 'VM_NONINTEGER'
    _VALIDATION_MESSAGES = Number._VALIDATION_MESSAGES
    _VALIDATION_MESSAGES.update({VM_NONINTEGER: _("Nen� to cel� ��slo")})
    
    def _validate(self, string):
        """Pokus se p�ev�st 'string' na plain nebo long integer.
        
        Pokud je 'string' mo�no p�ev�st na plain integer, je spr�vn� a vr�cen�
        instance t��dy 'Value' obsahuje odpov�daj�c� hodnotu jako plain
        integer.  V�jin�m p��pad� plat� analogick� pravidlo pro long integers.
        Pokud 'string' nen� mo�no p�ev�st na plain ani long integer, 'string'
        nen� spr�vn� a je vr�cena chyba.

        Metoda validuje v�echny z�pisy integers akceptovan� Pythonem, zejm�na
        tedy i�long integers ve tvaru '1L'.
        
        """
        assert isinstance(string, types.StringTypes), ('Not a string', string)
        error = None
        try:
            value = int(string)
        except:
            # Dokumentace Pythonu 1.5.2 ne��k�, �e by `int' mohlo metat metat
            # n�jakou v�jimkou, ale evidentn� by m�lo, pokud `string' nelze
            # na oby�ejn� integer p�ev�st.
            try:
                value = long(string)
            except:
                # Podobn� jako `int' i�`long' by m�lo v�p��pad� nemo�nosti
                # p�evodu metat v�jimku.
                value = None
        if value is not None:
            result = Value(self, value), None
        else:
            result = None, self._validation_error(self.VM_NONINTEGER)
        return result


class Serial(Integer):
    """Integer s�automaticky generovan�mi hodnotami.

    Typ oproti 'Integer' nezav�d� ��dn� nov� rysy, jeho v�znam je �ist�
    specifika�n�.  Nap��klad u�ivatelsk� rozhran� tak z�sk�v� informaci, �e
    nen� t�eba ani ��douc� explicitn� nastavovat hodnoty sloupc� tohoto typu
    v���dku p�i vkl�d�n� nov�ho z�znamu.

    """
    pass


class Float(Number):
    """��slo v�pohybliv� ��dov� ��rce v�rozsahu podporovan�m Pythonem."""

    CEILING = 'CEILING'
    """Konstanta pro typ zaokrouhlen� ve 'validate'."""
    FLOOR = 'FLOOR'
    """Konstanta pro typ zaokrouhlen� ve 'validate'."""

    VM_INVALID_NUMBER = 'VM_INVALID_NUMBER'
    _VALIDATION_MESSAGES = Number._VALIDATION_MESSAGES
    _VALIDATION_MESSAGES.update(
        {VM_INVALID_NUMBER: _("Nen� to povolen� ��slo")})
    
    def __init__(self, precision=None, **kwargs):
        """Definuj typ re�ln�ho ��sla.

        Argumenty:

          precision -- nez�porn� integer ud�vaj�c� po�et ��sel za desetinnou
            ��rkou uv�d�n� p�i exportu, nebo 'None' (pak nen� p�esnost um�le
            omezena)

        Ostatn� kl��ov� argumenty jsou shodn�, jako v p�edkovi.
             
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
        """Vra� p�esnost ��sla zadanou v�konstruktoru jako integer."""
        return self._precision
    
    def _validate(self, string, precision=None, rounding=None):
        """Pokus se p�ev�st 'string' na float.

        Pokud je 'string' mo�no p�ev�st na float, je spr�vn� a vr�cen� instance
        t��dy 'Value' obsahuje odpov�daj�c� hodnotu jako plain integer.  Pokud
        'string' p�ev�st mo�no nen�, 'string' nen� spr�vn� a je vr�cena chyba.

        Metoda validuje v�echny z�pisy floats akceptovan� Pythonem.

        Argumenty:

          precision -- nez�porn� integer ud�vaj�c� po�et ��sel za desetinnou
            ��rkou, na kter� m� b�t zvalidovan� ��slo zaokrouhleno, nebo 'None'
            (pak nen� p�esnost um�le omezena)
          rounding -- specifikace zaokrouhlen� p�i po�adavku na omezenou
            p�esnost; 'None' zna�� standardn� zaokrouhlen�, konstanta 'CEILING'
            zaokrouhlen� sm�rem nahoru, konstanta 'FLOOR' zaokrouhlen� sm�rem
            dol� (pozor na z�porn� ��sla, plat� to pro n� tak� p�esn� takto!)
        
        """
        assert isinstance(string, types.StringTypes), ('Not a string', string)
        assert precision is None or \
               type(precision) == type(0) and precision >=0, \
               ('Invalid precision', precision)
        error = None
        try:
            import locale
            # TODO: Odporn� hack, z n�jak�ho d�vodu n�sleduj�c�
            # metoda bez toho nezafunguje
            locale.getlocale(locale.LC_NUMERIC)
            value = locale.atof(string)
        except:
            # Dokumentace Pythonu 1.5.2 ne��k�, �e by `float' mohlo metat metat
            # n�jakou v�jimkou, ale evidentn� by m�lo, pokud `string' nelze
            # na float p�ev�st.
            value = None
        if value is not None:
            if precision is not None:
                # Pozor na p�evody mezi bin�rn�mi a dekadick�mi ��sly!
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
    """Libovoln� string.

    Lze tak� specifikovat, �e �et�zec m��e m�t pouze omezenou d�lku, bl�e viz
    metody '__init__' a 'maxlen'.

    """    

    VM_STRING_TOO_LONG = 'VM_STRING_TOO_LONG'
    _VALIDATION_MESSAGES = Type._VALIDATION_MESSAGES
    _VALIDATION_MESSAGES.update(
      {VM_STRING_TOO_LONG: _("�et�zec je p��li� dlouh�: (%s, %s)")})

    _SPECIAL_VALUES = Type._SPECIAL_VALUES + ((None, ''),)
    
    def __init__(self, maxlen=None, **kwargs):
        """Definuj stringov� typ maxim�ln� d�lky.

        Argumenty:
        
          maxlen -- maxim�ln� d�lka stringu jako integer, nebo 'None';
            pokud je 'None', d�lka stringu nen� omezena
             
        Ostatn� kl��ov� argumenty jsou shodn�, jako v p�edkovi.

        """
        super(String, self).__init__(**kwargs)
        self._maxlen = maxlen

    def __cmp__(self, other):
        """Vra� 0, pr�v� kdy� 'self' a 'other' jsou t�e t��dy a max. d�lky."""
        result = super(String, self).__cmp__(other)
        if not result:
            result = cmp (self.maxlen(), other.maxlen())
        return result

    def maxlen(self):
        """Vra� maxim�ln� povolenou d�lku stringu jako integer, nebo 'None'.

        'None' zna��, �e d�lka �et�zce nen� omezen�.
        
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
        """Vra� instanci t��dy 'Value' s�hodnotou 'string'.

        Pokud byla v�konstruktoru specifikov�na maxim�ln� d�lka, 'string' je
        spr�vn� pr�v� tehdy, nen�-li del�� ne� tato d�lka.
        
        """
        assert isinstance(string, types.StringTypes), ('Not a string', string)
        return Value(self, unicode(string)), None

    def _export(self, value):
        # Pozor, na trivi�ln� funkci t�to metody se spol�h� Value.__init__ --
        # p�i zm�n� zde je nutn� zm�na i�tam.
        assert isinstance(value, types.StringTypes), \
               ('Value not a string', value)
        return unicode(value)


class Color(String):
    """Barva reprezentovan� �et�zcem '#RRGGBB'."""

    VM_COLOR_FORMAT = 'VM_COLOR_FORMAT'
    _VALIDATION_MESSAGES = Type._VALIDATION_MESSAGES
    _VALIDATION_MESSAGES.update(
        {VM_COLOR_FORMAT: _("Form�t barvy neodpov�d� masce '#RRGGBB'.")})
    
    _VALIDATION_REGEX = re.compile('^\#[0-9a-fA-F]{6,6}$')

    def _validate(self, string, *args, **kwargs):
        value, error = super(Color, self)._validate(string, *args, **kwargs)
        if error is None and self._VALIDATION_REGEX.match(string) is None:
            value, error = None, self._validation_error(self.VM_COLOR_FORMAT)
        return value, error

    
class DateTime(Type):
    """�asov� okam�ik reprezentovan� instanc� t��dy 'DateTime.DateTime'.

    T��da je schopna pracovat pouze s�absolutn�m �asov�m okam�ikem.  �as je
    nav�c v�dy uva�ov�n v�UTC, �asov� z�ny nejsou podporov�ny.  Datum je
    podporov�no pouze od roku�1000 d�le.

    Form�t data a �asu je shodn� pro import a export a je d�n parametrem
    'format' metody '__init__()'.
    
    """
    VM_DT_FORMAT = 'VM_DT_FORMAT'
    VM_DT_VALUE = 'VM_DT_VALUE'
    VM_DT_AGE = 'VM_DT_AGE'
    _VALIDATION_MESSAGES = Type._VALIDATION_MESSAGES
    _VALIDATION_MESSAGES.update(
        {VM_DT_FORMAT: _("Chybn� form�t data nebo �asu"),
         VM_DT_VALUE: _("Chybn� datum nebo �as"),
         VM_DT_AGE: _("Datum mimo povolen� rozsah")})
    
    _SPECIAL_VALUES = Type._SPECIAL_VALUES + ((None, ''),)

    DEFAULT_FORMAT = '%Y-%m-%d %H:%M:%S'
    """Implicitn� form�t data a �asu."""
    SQL_FORMAT = DEFAULT_FORMAT
    """Form�t data a �asu pou��van� standardn� SQL stroji."""
    CZECH_FORMAT = '%d.%m.%Y %H:%M:%S'
    """�esk� \"��etnick�\" form�t data a �asu."""

    if __debug__:
        _dt_type = type(DT.DateTimeFrom('2001-01-01'))

    def __init__(self, format=None, mindate=None, maxdate=None, **kwargs):
        """Inicializuj instanci.

        Argumenty:

          format -- specifikace vstupn�ho i�v�stupn�ho form�tu data a/nebo
            �asu, v�podob� akceptovan� funkc� 'time.strftime()'; m��e b�t t�
            'None', v�kter�m�to p��pad� se pou�ije konfigura�n� volba
            'config.date_time_format'.  T��da obsahuje p�eddefinovan� konstanty
            '*_FORMAT', kter� lze vyu��t jako hodnotu tohoto parametru.
          mindate. maxdate -- omezen� validity �asu

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
        return matcher.match(string)
        
    def _validate(self, string, format=None, local=True):
        """Stejn� jako v�p�edkovi a� na kl��ovan� argumenty.

        Argumenty:

          string -- stejn� jako v�p�edkovi
          format -- po�adovan� form�t hodnoty 'string', ve tvaru po�adovan�m
            metodou '__init__()'
          local -- pravdiv� pr�v� kdy� zadan� hodnota je v�lok�ln�m �ase;
            v�opa�n�m p��pad� je v�UTC
          
        """
        assert isinstance(string, types.StringTypes)
        if format is None:
            format = self._format
        # Vyu�it� `strptime' je nejjednodu��� �e�en�.  GNU `strptime' je
        # dostate�n� tolerantn� v��i nadbyte�n�m mezer�m atd., tak�e by jeho
        # pou�it�m nem�l vzniknout probl�m, pokud nehodl�me software provozovat
        # na ne-GNU syst�mech, kter� `strptime' ��dn� nepodporuj�.  Mus�me
        # ov�em o�ezat mezery zprava, proto�e v�mx.DateTime vad�, je tam n�jak�
        # chyba, standardn� `time.strptime' funguje.
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
        # TODO: zjistit, pro� zde bylo ud�l�no toto omezen�
        # Pro spr�vnou funk�nost t��dy Time je ale t�eba ho zru�it
        #if dt is not None and dt.year < 1000:
        #    result = None, self._validation_error(self.VM_DT_AGE)
        return result
    
    def _export(self, value, local=True):
        """Stejn� jako v�p�edkovi a� na kl��ovan� argumenty.

        Argumenty:

          local -- pravdiv� pr�v� kdy� zadan� hodnota je v�lok�ln�m �ase;
            v�opa�n�m p��pad� je v�UTC
          
        """
        assert type(value) == self._dt_type, 'Value is not DateTime'
        if local:
            value = value.localtime()
        return value.strftime(self._format)

    def now(class_, **kwargs):
        """Vra� instanci 'Value' tohoto typu odpov�daj�c� aktu�ln�mu okam�iku.

        Argumenty:

          kwargs -- argumenty p�edan� konstruktoru t��dy typu
          
        """
        type = class_(**kwargs)
        return Value(type, DT.now())
    now = classmethod(now)


class Date(DateTime):
    """Datum bez �asov�ho �daje."""

    DEFAULT_FORMAT = '%Y-%m-%d'
    """Implicitn� form�t data."""
    SQL_FORMAT = DEFAULT_FORMAT
    """Form�t data pou��van� standardn� SQL stroji."""
    CZECH_FORMAT = '%d.%m.%Y'
    """�esk� \"��etnick�\" form�t data."""

    def __init__(self, format=None, **kwargs):
        """Inicializuj instanci.

        Argumenty:

          format -- specifikace vstupn�ho i�v�stupn�ho form�tu data, v�podob�
            akceptovan� funkc� 'time.strftime()'; m��e b�t t� 'None',
            v�kter�m�to p��pad� se pou�ije konfigura�n� volba
            'config.date_format'.  T��da obsahuje p�eddefinovan� konstanty
            '*_FORMAT', kter� lze vyu��t jako hodnotu tohoto parametru.

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
    """�as bez datumov�ho �daje."""

    DEFAULT_FORMAT = '%H:%M:%S'
    """Implicitn� form�t �asu."""
    SQL_FORMAT = DEFAULT_FORMAT
    """Form�t �asu pou��van� standardn� SQL stroji."""
    SHORT_FORMAT = '%H:%M'
    """Form�t �asu bez zobrazen� sekund."""

    def __init__(self, format=None, **kwargs):
        """Inicializuj instanci.

        Argumenty:

          format -- specifikace vstupn�ho i�v�stupn�ho form�tu �asu, v�podob�
            akceptovan� funkc� 'time.strftime()'; m��e b�t t� 'None',
            v�kter�m�to p��pad� se pou�ije konfigura�n� volba
            'config.time_format'.  T��da obsahuje p�eddefinovan� konstanty
            '*_FORMAT', kter� lze vyu��t jako hodnotu tohoto parametru.

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
    """Jednoduch� v��tov� typ implementuj�c� hodnoty \"pravda\" a \"nepravda\".
    
    Za pravdu je pova�ov�n string 'T', za nepravdu string 'F'; tyto stringy
    jsou u�ivatelsk�mi hodnotami v��tu.  Odpov�daj�c� vnit�n� hodnoty jsou
    bl�e nespecifikovan� pythonov� objekty s�pythonovu s�mantikou pravdy a
    nepravdy.
    
    """

    _SPECIAL_VALUES = ((True, 'T'), (False, 'F'))
    
    def __init__(self, not_null=True):
        e = FixedEnumerator((True, False))
        super(Boolean, self).__init__(enumerator=e, not_null=not_null)

    def _validate(self, object, extended=False):
        """Vra� instanci t��dy 'Value' s hodnotou definovanou pro 'object'.

        Je-li argument 'extended' pravdiv�, jsou krom� \"ofici�ln�ch\" hodnot
        'object' zvalidov�ny i�n�sleduj�c� stringov� hodnoty:

          \'t\', \'1\' -- jako reprezentace pravdiv� hodnoty
          \'f\', \'0\', \'\' -- jako reprezentace nepravdiv� hodnoty
        
        """
        if not extended and isinstance(object, str):
            object = string.strip(object)
            if object in ('t', '1'):
                object = 'T'
            elif object in ('', 'f', '0'):
                object = 'F'
        return super(Boolean, self)._validate(object)
    
    def default_value(self):
        return Value(self, False)


# Pomocn� t��dy

class Enumerator(object):
    """Realizace v��tu hodnot pou�iteln�ho pro integritn� omezen� datov�ho typu.

    Enumer�tor je p�edev��m poskytovatelem validace pro ov��en�, zda je n�jak�
    hodnota p��tomna v ur�it� mno�in� hodnot.  Zp�sob ov��en� a ur�en� mno�iny
    hodnot je p�edm�tem implementace r�zn�ch t��d enumer�tor�.  Ov��ovan�
    hodnota je v�dy vnit�n� (Pythonovou) hodnotou typu, ve kter�m je enumer�tor
    pou�it.

    Instanci enumer�toru je potom mo�no p�edat konstruktoru datov�ho typu a
    uvalit tak na dan� typ p��slu�n� integritn� omezen�.  V�ce informac� tak�
    viz 'Type.__init__()'.
    
    Tato t��da pouze definuje povinn� rozhran� enumer�tor�.  Krom� zde
    definovan�ch povinn�ch metod mohou konkr�tn� t��dy enumer�tor� nab�zet
    je�t� dal�� slu�by.
    
    """
    def check(self, value):
        """Vra� pravdu, pokud 'value' je prvkem mno�iny enumer�toru.

        Argumenty:
        
          value -- vnit�n� (Pythonov�) hodnota datov�ho typu, pro kter� je
            enumer�tor pou�it.
        
        """
        raise ProgramError('Not implemented', 'Enumerator.check()')

    def values(self):
        """Vra� sekvenci v�ech spr�vn�ch u�ivatelsk�ch hodnot typu."""
        raise ProgramError('Not implemented', 'Enumerator.values()')


class FixedEnumerator(Enumerator):
    """Enumer�tor pracuj�c� s fixn� mno�inou hodnot."""
    
    def __init__(self, enumeration):
        """Inicializuj instanci.
        
        Argumenty:
        
          enumeration -- sekvence hodnot kompatibiln�ch s vnit�n�mi
            (Pythonov�mi) hodnotami typu, pro kter� m� b�t enumer�tor pou�it.
          
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
        
        
class MutableEnumerator(Enumerator):
    """Abstraktn� t��da, kterou povinn� d�d� v�echny mutable enumer�tory.

    Mutable enumer�tor takov�, jeho� mno�ina hodnot se m��e v �ase m�nit.

    """
    def __init__(self):
        self._hooks = []
        self._running_hooks = False
    
    def _update(self, force=False):
        """Aktualizuj data enumer�toru.

        Bezprost�edn� po zavol�n� t�to metody by m�l enumer�tor pracovat
        s�aktu�ln�mi daty.

        Argumenty:

          force -- pr�v� kdy� je pravda, je update vynucen�, jinak m��e a
            nemus� b�t proveden, v�t�inou v�z�vislosti na n�ro�nosti operace

        Vrac�: Pravdu, pr�v� kdy� byl update skute�n� proveden.

        """
        if self._running_hooks:
            return True
        self._running_hooks = True
        try:
            for hook in self._hooks: hook()
        finally:
            self._running_hooks = False
        return True

    def add_hook_on_update(self, hook):
        self._hooks.append(hook)
        

class DataEnumerator(MutableEnumerator):
    """Enumer�tor z�sk�vaj�c� sv� hodnoty z�datov�ho objektu.

    Hodnoty v��tu jsou pythonov� hodnoty ur�it�ho sloupce datov�ho objektu.
    Typicky je to kl��ov� sloupec ale pomoc� argument� konstruktoru je mo�no
    zvolit libovoln� jin� sloupec.

    """
        
    def __init__(self, data_factory, data_factory_kwargs={},
                 value_column=None, validity_column=None):
        """Inicializuj instanci.
        
        Argumenty:
        
          data_factory -- instance t��dy 'DataFactory' slou��c� k vytvo�en�
            datov�ho objektu pou�it�ho k z�sk�n� v��tov�ch hodnot.
          data_factory_kwargs -- dictionary kl��ovan�ch argument� pro metodu
            'DataFactory.create()'.
          value_column -- id sloupce datov�ho objektu poskytuj�c�ho hodnoty
            enumer�toru.  Je-li None, bude pou�it kl��ov� sloupec.
          validity_column -- id sloupce, ur�uj�c�ho platnost ��dk� datov�ho
            zdroje.  Pokud je ur�en (nen� None), budou za hodnoty v��tu
            pova�ov�ny pouze ty ��dky, v nich� dan� sloupec nab�v� pravdiv�
            hodnoty (mus� j�t o Boolean sloupec).
          
        """
        super(DataEnumerator, self).__init__()
        assert isinstance(data_factory, DataFactory), data_factory
        assert isinstance(data_factory_kwargs,
                          (types.DictType, types.TupleType))
        assert value_column is None or \
               isinstance(value_column, types.StringType)
        assert validity_column is None or \
               isinstance(validity_column, types.StringType) 
        # Store the arguments.
        self._data_factory = data_factory
        if type(data_factory_kwargs) == type(()):
            data_factory_kwargs = dict(data_factory_kwargs)
        self._data_factory_kwargs = data_factory_kwargs
        self._value_column_ = value_column
        self._validity_column = validity_column
        if validity_column is None:
            validity_condition = None
        else:
            validity_condition = EQ(validity_column, Value(Boolean(), True))
        self._validity_condition = validity_condition
        # Initialize the runtime filter.
        self._runtime_filter = None
        self._runtime_filter_dirty = True
        self._runtime_filter_provider = None
        self._runtime_filter_args = None

    def __getattr__(self, name):
        if name in ('_data', '_value_column'):
            self._complete()
            return self.__dict__[name]
        else:
            return super(DataEnumerator, self).__getattr__(name)
        
    def _complete(self):
        # Dokon�i instanci vytvo�en�m datov�ho objektu.
        kwargs = self._data_factory_kwargs
        self._data = data = self._data_factory.create(**kwargs)
        self._data_changed = False
        def on_data_change():
            self._data_changed = True
        self._data.add_callback_on_change(on_data_change)
        if self._value_column_ is None:
            key = data.key()
            assert len(key) == 1, \
                   "Only single-column key is supported by DataEnumerator."
            self._value_column = key[0].id()
        else:
            self._value_column = self._value_column_
        c = data.find_column(self._value_column)
        assert c, ('Non-existent value column', self._value_column)
        self._value_column_type = c.type()
        if self._validity_column is not None:
            c = data.find_column(self._validity_column)
            assert c, ('Non-existent validity column', self._validity_column)
            assert isinstance(c.type(), Boolean), \
                   ('Invalid validity column type', c)

    def _retrieve(self, value):
        data = self._data
        v = Value(self._value_column_type, value)
        condition = EQ(self._value_column, v)
        validity_condition = self.validity_condition()
        if validity_condition is not None:
            condition = AND(condition, validity_condition)
        count = data.select(condition)
        if count > 1:
            raise ProgramError('Insufficient runtime filter for DataEnumerator',
                               str(condition))
        row = data.fetchone()
        data.close()
        return row

    def _update(self, force=False):
        if force or self._data_changed:
            self._data_changed = False
            result = super(DataEnumerator, self)._update(force=force)
        else:
            result = False
        return result

    # Enumerator interface
    
    def check(self, value):
        row = self._retrieve(value)
        if row is None:
            result = False
        else:
            result = True
        return result

    def values(self):
        result = self._data.select_map(lambda r: r[self._value_column].value(),
                                       condition=self.validity_condition())
        return tuple(result)
        

    # Extended interface.

    def iter(self):
        self._data.select(self.validity_condition())
        def fetchone():
            row = self._data.fetchone()
            if row is None:
                self._data.close()
            return row
        return iter(fetchone, None)
    
    def value_column(self):
        return self._value_column
    
    def get(self, value, column=None):
        """Z�skej z dat hodnotu dan�ho sloupce z ��dku odpov�daj�c�mu 'value'.

        Argumenty:
        
           value -- vnit�n� (Pythonov�) hodnota sloupce 'value_column' z
             datov�ho objektu.  Podle t�to hodnoty bude vyhled�n p��slu�n�
             ��dek.
           column -- identifik�tor sloupce datov�ho objektu, jeho� hodnota m�
             b�t vr�cena.

        Vrac� instanci 'Value', nebo None, pokud dan� ��dek nebyl nalezen.

        """
        row = self._retrieve(value)
        if row is None:
            result = None
        else:
            if column is None:
                column = self._value_column
            result = row[column]
        return result
    
    # Run-time filter interface.

    def set_runtime_filter_provider(self, provider, args):
        """Nastav poskytovatele run-time podm�nky filtruj�c� ��dky enumer�toru.

        Argumenty:
        
          provider -- None, nebo funkce, kter� vrac� instanci t��dy 'Operator'.
            Tato funkce bude vol�na v�dy, kdy� je t�eba zjistit dodate�nou
            filtrovac� podm�nku.
          args -- seznam argument� (tuple), kter� maj� b�t p�ed�ny t�to funkci.

        Run-time podm�nka umo��uje m�nit mno�inu platn�ch ��dk� enumer�toru za
        b�hu.  Po extern� zm�n� podm�nky je t�eba toto ozn�mit vol�n�m
        'notify_runtime_filter_change()'.  T�m je zaji�t�no, �e p�i v�ech
        n�sledn�ch operac�ch s enumer�torem bude podm�nka automaticky
        p�epo��t�na a mno�ina platn�ch hodnot enumer�toru bude aktualizov�na.

        """
        assert callable(provider) or provider is None
        self._runtime_filter_provider = provider
        self._runtime_filter_args = args

    def notify_runtime_filter_change(self):
        """Ohlas zm�nu run-time filtrovac� podm�nky.

        Tato metoda by m�la b�t vol�na v�dy, kdy� dojde k extern� zm�n�
        run-time filtrovac� podm�nky.  Enumer�tor se tak dozv�, �e si m� v
        p��pad� pot�eby zjistit novou hodnotu podm�nky (viz metoda
        'set_runtime_filter_provider()').
        
        """
        self._runtime_filter_dirty = True
        self._update(force=True)
        
    
    def validity_condition(self):
        """Vra� podm�nku ur�uj�c� platn� ��dky enumer�toru.
        
        Podm�nka zahrnuje jak statick� omezen� ��dk� enumer�toru, tak aktu�ln�
        hodnotu run-time podm�nky

        Vrac�: Instanci t��dy 'pytis.data.Operator'.

        """
        f = self._runtime_filter_provider
        if f is not None:
            if self._runtime_filter_dirty:
                self._runtime_filter = flt = apply(f, self._runtime_filter_args)
                assert isinstance(flt, Operator) or flt is None
                self._runtime_filter_dirty = False
                self._update(force=True)
            condition = self._runtime_filter
        else:
            condition = None
        if self._validity_condition:
            if condition:
                return AND(condition, self._validity_condition)
            else:
                return self._validity_condition
        else:
            return condition


class ValidationError(Exception):
    """Popis chyby p�i ne�sp�chu validace v�'Type.validate'.

    Popis lze z�skat ve�ejnou metodou 'message'.  Popisem je string a m�l by
    b�t srozumiteln� jako zpr�va pro u�ivatele.
    
    """
    def __init__(self, message):
        """Inicializuj zpr�vu o�chyb�.

        Argumenty:
        
          message -- string obsahuj�c� zpr�vu o�chyb� pro u�ivatele
         
        """
        super_(ValidationError).__init__(self, message)
        self._message = message

    def __str__(self):
        return '<ValidationError: ' + self.message() + '>'

    def message(self):
        """Vra� zpr�vu o�chyb� jako string srozumiteln� pro u�ivatele."""
        return self._message


class _Value(object):
    """Obecn� reprezentace hodnoty dan�ho typu.

    Ka�d� hodnota se skl�d� z�typu (instance t��dy 'Type') a hodnoty samotn�.
    Hodnota samotn� m��e b�t cokoliv, bez ohledu na uveden� typ.

    """
    def __init__(self, type, value):
        """Inicializuj instanci.

        Argumenty:
        
          type -- instance t��dy 'Type'
          value -- hodnota samotn�, libovoln� objekt

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
        """Vra� 0, pr�v� kdy� 'self' a 'other' jsou shodn�.

        'self' a 'other' jsou shodn�, pr�v� kdy� se rovnaj� jejich typy a
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

    def type(self):
        """Vra� typ hodnoty jako instanci t��dy 'Type' zadanou v�'__init__()'.
        """
        return self._type

    def value(self):
        """Vra� hodnotu zadanou v�'__init__()'."""
        return self._value


class Value(_Value):
    """Reprezentace hodnoty dan�ho typu.

    Ka�d� hodnota se skl�d� z�typu (instance t��dy 'Type') a hodnoty samotn�.
    Hodnota samotn� m��e b�t cokoliv, bez ohledu na uveden� typ.

    Pro z�pis hodnoty do u�ivatelsk�ho rozhran� nebo datab�ze lze vyu��t metodu
    'export()'.
    
    """
    _VOID = object()
    
    def __init__(self, type, value):
        """Inicializuj hodnotu dan�ho typu.

        Argumenty:
        
          type -- instance t��dy 'Type'
          value -- hodnota samotn�, libovoln� objekt
         
        """
        _Value.__init__(self, type, value)
        self._init()

    def _init(self):
        if self._type.__class__ == String:    # pozor, nebezpe�n� v�c!
            self._exported = unicode(self._value or '')
        else:
            self._exported = self._VOID
        
    def export(self, *args, **kwargs):
        """Vra� stringovou reprezentaci hodnoty schopnou validace.

        Tato metoda je pouze zkratkou pro vol�n�
        'self.type().export(self.value())'.  Pokud jsou metod� p�ed�ny
        argumenty, je metoda `Type.export()' vol�na i�s�t�mito argumenty.
        
        """
        # Abychom to zbyte�n� nekomplikovali, tak cachujeme pouze exporty bez
        # argument�.
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
    """Reprezentace specifikace pro wildcard match dan�ho typu."""
