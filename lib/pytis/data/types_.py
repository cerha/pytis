# -*- coding: utf-8 -*-

# Copyright (C) 2018-2022, 2024 Tomáš Cerha <t.cerha@gmail.com>
# Copyright (C) 2001-2017 OUI Technology Ltd.
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

"""Data types, their values and internal/external representation.

Základní ideou modulu je, že uvnitř programu vždy pracujeme s hodnotami
určitého, námi definovaného, typu.  Z důvodu datové abstrakce při práci s daty
nepoužíváme přímo standardní typy Pythonu a jeho knihoven, nýbrž naše vlastní
obálky okolo nich, které nám zajistí nezávislost vůči konkrétní reprezentaci
hodnot daného typu v různých částech programu (PostgreSQL, wxWidgets, ...).
Kromě toho nám tyto typové obálky mohou také poskytovat některé doplňující
funkce související s typy dat, jako je například validace vstupní hodnoty
daného typu reprezentované stringem a její převod na interní reprezentaci, se
kterou dále v programu pracujeme.

Základem modulu je abstraktní třída 'Type', která je společným základem všech
typových tříd.  Jejím poděděním vznikají konkrétní typy nebo jejich společné
specializovanější základy.  Hodnoty daných typů jsou pak reprezentovány
instancemi samostatné třídy 'Value'.

"""
from past.builtins import basestring, long
from builtins import range
from future.utils import with_metaclass, python_2_unicode_compatible

import datetime
import decimal
import io
import json
import re
import string
import sys
import _thread
import time
import uuid

from pytis.util import (
    Counter, InvalidAccessError, LimitedCache, OPERATIONAL, ProgramError,
    assoc, format_byte_size, identity, log, rassoc, sameclass, super_, Locked, xtuple,
)
import pytis.util

_ = pytis.util.translations('pytis-data')

unistr = type(u'')  # Python 2/3 transition hack.

try:
    import sqlalchemy
except Exception as e:
    # Ignore import error and re-raise it only when sqlalchemy is actually used
    # in run-time.
    class Dummy(object):

        def __getattr__(self, name):
            raise e
    sqlalchemy = Dummy()
else:
    import sqlalchemy.dialects.postgresql


class _MType(type):

    def __call__(self, *args, **kwargs):
        return self.make(*args, **kwargs)


@python_2_unicode_compatible
class Type(with_metaclass(_MType, object)):
    """Abstract base class for all data type specific classes.

    All type classes are required to derive from this class.

    Instances of this class are considered to be immutable.  They can not be
    modified during their life time and may be shared without a limitation.

    Constructor arguments:

      not_null -- flag saying the value must not be empty.  Empty value is
        None or any other value mapped to None in '_SPECIAL_VALUES'.
      enumerator -- enumerator specification as 'Enumerator' instance or
        None.  Enumerators are used to implement referential integrity or
        static enumeration checking.  See 'Enumerator' class documentation
        for more information.
      constraints -- sekvence validačních funkcí sloužících k realizaci
        libovolných integritních omezení.  Každá z těchto funkcí je funkcí
        jednoho argumentu, kterým je vnitřní hodnota typu.  Funkce pro tuto
        hodnotu musí vrátit buď 'None', je-li hodnota správná, nebo chybovou
        hlášku jako string v opačném případě.
      unique -- flag saying the value must be unique within its column in a
        table

    """
    class _TypeTable(object):

        def __init__(self):
            self._id_counter = Counter()
            self._init_args_to_id = {}
            self._id_to_init_args = {}
            self._init_args_to_type = {}

        def _key(self, class_, args, kwargs):
            return ((class_.__module__, class_.__name__) +
                    args + tuple(kwargs.items()))

        def id_of_initargs(self, class_, args, kwargs):
            table = self._init_args_to_id
            key = self._key(class_, args, kwargs)
            if key in table:
                result = table[key]
            else:
                result = table[key] = self._id_counter.next()
                self._id_to_init_args[result] = \
                    class_.__module__, class_.__name__, args, kwargs
            return result

        def get(self, id):
            if not isinstance(id, int):
                raise Exception('Invalid id type', id)
            return self._id_to_init_args.get(id)

        def get_instance(self, class_, *args, **kwargs):
            table = self._init_args_to_type
            key = self._key(class_, args, kwargs)
            if key in table:
                result = table[key]
            else:
                result = table[key] = class_.__new__(class_)
                assert isinstance(result, class_)
                result.__init__(*args, **kwargs)
                result._id = self.id_of_initargs(class_, args, kwargs)
                result._init_args = (args, kwargs,)
            assert result is not None
            return result

    _type_table = _TypeTable()

    _SPECIAL_VALUES = ()
    _VALIDATION_CACHE_LIMIT = 1000

    def _make(class_, *args, **kwargs):
        result = Type._type_table.get_instance(class_, *args, **kwargs)
        assert result is not None
        return result
    _make = staticmethod(_make)

    @classmethod
    def make(class_, *args, **kwargs):
        """Pouze pro účely zpětné kompatibility a pro metatřídu.

        V novém kódu nepoužívat.

        """
        return class_._make(class_, *args, **kwargs)

    @classmethod
    def type_table(class_):
        """Vrať tabulku typů jako instanci '_TypeTable'.

        Jediný účel této metody je zpřístupnit tabulku typů pro vzdálené
        předávání typů ze serveru na klienta.  Pro jiné účely by tabulka typů
        neměla být používána.

        """
        return Type._type_table

    def __init__(self, **kwargs):
        """Initialize the instance.

        See the class docstring for description of available arguments.  Don't
        override the constructor in derived classes, unless you really know
        what you are doing.  Normally you want to override the '_init()' method
        in derived classes to define specific type constructor arguments or
        default values of inherited arguments.

        The purpose of overriding '_init()' instead of '__init__()' is to be
        able to save the dictionary of explicitly passed constructor arguments
        here.  This allows type cloning (see 'clone()').

        """
        self._constructor_kwargs = kwargs
        self._init(**kwargs)
        super(Type, self).__init__()

    def _init(self, not_null=False, enumerator=None, constraints=(), unique=False):
        """Initialize the instance.

        Defines constructor arguments and their default values.  You typically
        want to override this method in derived classes to define type specific
        constructor arguments or default values of inherited arguments as you
        would normally do by overriding '__init__()', which has a special
        purpose in this class.

        """
        assert isinstance(not_null, bool), not_null
        assert isinstance(unique, bool), unique
        assert enumerator is None or isinstance(enumerator, Enumerator), enumerator
        assert isinstance(constraints, (list, tuple)), constraints
        self._not_null = not_null
        self._unique = unique
        self._enumerator = enumerator
        self._constraints = xtuple(constraints)
        # Cachujeme na úrovni instancí, protože ty jsou stejně sdílené, viz
        # `__new__'.
        self._validation_cache = LimitedCache(self._validating_provider,
                                              limit=self._VALIDATION_CACHE_LIMIT)
        if isinstance(enumerator, DataEnumerator):
            # TODO: Jak se to bude chovat po smrti instance typu?
            def callback():
                self._validation_cache.reset()
            enumerator.add_callback_on_change(callback)

    def __eq__(self, other):
        if not sameclass(self, other):
            return NotImplemented
        else:
            return (self._id == other._id or id(self) == id(other) or
                    self._comparison_key() == other._comparison_key())

    def _comparison_key(self):
        return (self._constraints, self._not_null, self._unique, self._enumerator)

    def __ne__(self, other):
        # Implied automatically in Python 3 so can be removed when dropping Python 2 support.
        return not self == other

    def __str__(self):
        name, module = self.__class__.__name__, self.__class__.__module__
        if module != 'pytis.data.types_':
            name = module + '.' + name
        args = [' %s=%s' % x for x in sorted(self._constructor_kwargs.items())]
        return "<%s%s>" % (name, ''.join(args))

    def __repr__(self):
        return str(self)

    def __hash__(self):
        return hash(self.__class__.__name__)

    def __getstate__(self):
        assert self._id is not None, ('Improper type instance', self)
        return self._id

    def __setstate__(self, state):
        if not isinstance(state, int):
            raise InvalidAccessError('Invalid type identifier', state)
        self._id = state

    def clone(self, other):
        """Clone this instance by another instance and return a merged instance.

        The returned instance will inherit all attributes of the cloned type
        instance as well as the cloning instance (the one passed as the
        argument), where the attributes of the cloning instance take
        precedence.  The cloning instance must be the same class or a subclass
        of the cloned instance.  The returned instance is of the cloning
        instance's class.

        """
        assert isinstance(other, self.__class__), '%s, %s' % (self.__class__, other)
        kwargs = dict(self._constructor_kwargs, **other._constructor_kwargs)
        return other.__class__(**kwargs)

    def validate(self, obj, strict=True, transaction=None, condition=None, arguments=None,
                 **kwargs):
        """Validate the 'obj' and return a 'Value' instance and an error.

        Arguments:

          obj -- an object to be converted to a value.  This is typically a
            string representation of the value from user input.  See below for
            more details.
          strict -- when True (by default), the input object is first converted
            to a Value instance (containing a proper internal Python
            representation of the value) and then this value is checkend
            against all constraints defined by the type instance.  Passing
            False disables constraints checking so it reduces validation just
            to conversion.  Note, that constraints may be checked individually
            later using 'check_constraints()'.
          transaction, condition, arguments -- used for constraints checking
            (only when 'strict' is False) with the same meaning as defined by
            'check_constraints()'.
          kwargs -- type specific keyword arguments

        Returns: a pair (VALUE, ERROR).  VALUE is a 'Value' instance (for a
        valid 'obj') or 'None' (for an invalid 'obj').  ERROR is 'None'
        for a valid 'obj' and a 'ValidationError' instance for an invalid
        'obj'.

        Most types require the 'obj' to be a string, most often representing
        user input or a value loaded from a file or process.  Certain more
        sophisticated types, however, may accept or require an 'obj' of a
        different type.  All types should, if possible, accept a string and if
        not (eg. for excessive complications), they should accept an object,
        which may be simply constructed from data picked up from the user
        interface.  If the 'obj' is not a string or another type explicitely
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
        if arguments:
            arguments = tuple(arguments.items())
        key = (obj, strict, transaction, condition, arguments, tuple(kwargs.items()))
        try:
            result = self._validation_cache[key], None
        except ValidationError as e:
            result = None, e
        return result

    def _validating_provider(self, key):
        obj, strict, transaction, condition, arguments, kwargs_items = key
        special = rassoc(obj, self._SPECIAL_VALUES)
        if special:
            value = Value(self, special[0])
        elif obj is None:
            value = Value(self, None)
        else:
            value = self._validate(obj, **dict(kwargs_items))
        if strict:
            if arguments:
                arguments = dict(arguments)
            self._check_constraints(value.value(), transaction=transaction, condition=condition,
                                    arguments=arguments)
        return value

    def _validate(self, obj, **kwargs):
        return Value(self, None)

    def wm_validate(self, obj):
        """Validate input object for wildcard matching.

        Arguments:

          obj -- validated object, string

        Returns: pair (VALUE, ERROR).  When given 'obj' is valid and can be
        used for wildcard matching, VALUE is a 'WMValue' instance () and ERROR
        is 'None'.  In the other case VALUE is None and ERROR is a
        'ValidationError' instance containing error description.

        """
        msg = _(u"Wildcard matching not supported for values of type '%s'.")
        return None, ValidationError(msg % self.__class__.__name__)

    def _check_constraints(self, value, transaction=None, condition=None, arguments=None):
        if value is None:
            if self._not_null:
                # Translators: User input validation error message.
                raise ValidationError(_(u"Empty value"))
            else:
                return True
        for c in self._constraints:
            cresult = c(value)
            if cresult is not None:
                raise ValidationError(cresult)
        if self._enumerator is not None:
            if isinstance(self._enumerator, DataEnumerator):
                kwargs = dict(condition=condition, arguments=arguments)
            else:
                kwargs = arguments or {}
            if isinstance(self._enumerator, TransactionalEnumerator):
                kwargs['transaction'] = transaction
            if not self._enumerator.check(value, **kwargs):
                # Translators: User input validation error message.
                raise ValidationError(_(u"Invalid value"))

    def check_constraints(self, value, transaction=None, condition=None, arguments=None):
        """Check if 'value' matches all constraints defined by this type instance.

        Arguments:

          value -- internal Python representation of a value of given type as
            returned by 'Value.value()'.
          transaction -- transaction for data operations.  Only needed if the
            type defines an enumerator, which is a 'TransactionalEnumerator'
            instance.
          condition -- runtime filter condition for enumerator validation.
            Only needed if the type defines an enumerator.
          arguments -- runtime table function arguments for enumerator
            validation.  Only needed if the type defines an enumerator which
            operates on a table function.

        """
        try:
            self._check_constraints(value, transaction=transaction,
                                    condition=condition, arguments=arguments)
        except ValidationError as error:
            return error
        else:
            return None

    def not_null(self):
        """Return true if values of this type may not be empty."""
        return self._not_null

    def unique(self):
        """Return true if values of this type must be unique in a table."""
        return not not self._unique

    def enumerator(self):
        """Return the 'Enumerator' instance bound to this type or None."""
        return self._enumerator

    def init_args(self):
        """Return pair (ARGS, KWARGS) containing constructor arguments.

        This is useful when you need to create a modified type instance.

        """
        return self._init_args

    def export(self, value, *args, **kwargs):
        """Return a valid string reprezentation of 'value'.

        'value' is the internal Python representation of a value of given
        type as returned by 'Value.value()'.

        When 'value' is None, most types (whose documentation doesnt't say
        otherwise) return an empty string.

        In any case, the returned value should always be a valid representation
        which can be converted back to an internal Python representation of the
        value using 'validate()'

        Derived classes should not override this method.  They should override
        '_export()' instead.

        """
        special = assoc(value, self._SPECIAL_VALUES)
        if special:
            return special[1]
        exported = self._export(value, *args, **kwargs)
        return exported

    def _export(self, value, **kwargs):
        return repr(value)

    def default_value(self):
        """Return the default value as a 'Value' instance.

        If no such value exists or doesn't make sense, return 'None'.

        The returned value may be used for example for initialization of a new
        record.

        """
        return Value(self, None)

    def secret_export(self):
        """Return string representation of a hidden value."""
        return '***'

    def adjust_value(self, value):
        """Return 'value' in the form to be used as type internal value.

        Typically, the return value is just 'value'.  But if the value is not
        of the proper internal value type it may be converted to it if
        possible.  If it is not possible then 'TypeError' is raised.

        """
        return value

    def sqlalchemy_type(self):
        """Return corresponding SQLAlchemy type, sqlalchemy.types.TypeEngine instance."""
        raise Exception("Not implemented", self)


class Number(Type):
    """Abstraktní typová třída, která je základem všech numerických typů.

    Třída víceméně nic nového nedefinuje, je určena pouze k podědění všemi
    numerickými typy, aby tyto byly jakožto číselné snadno rozpoznatelné.

    Constructor arguments:

      minimum -- minimal value; 'None' denotes no limit.
      maximum -- maximal value; 'None' denotes no limit.

    Other arguments are passed to the parent constructor.

    """
    _SPECIAL_VALUES = Type._SPECIAL_VALUES + ((None, ''),)

    def _init(self, minimum=None, maximum=None, **kwargs):
        self._minimum = minimum
        self._maximum = maximum
        super(Number, self)._init(**kwargs)

    def _comparison_key(self):
        return super(Number, self)._comparison_key() + (self._minimum, self._maximum)

    def minimum(self):
        """Return the minimal value.

        'None' denotes no limit.

        """
        return self._minimum

    def maximum(self):
        """Return the maximal value.

        'None' denotes no limit.

        """
        return self._maximum

    def _check_constraints(self, value, **kwargs):
        super(Number, self)._check_constraints(value, **kwargs)
        if value is not None:
            if self._minimum is not None and value < self._minimum:
                # Translators: User input validation error message.
                raise ValidationError(_(u"Minimal value is %(minimum)s", minimum=self._minimum))
            if self._maximum is not None and value > self._maximum:
                # Translators: User input validation error message.
                raise ValidationError(_(u"Maximal value is %(maximum)s", maximum=self._maximum))


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
    """Mixin class for types with possibly limited maximal and/or minimal length.

    Minimal and maximal length of a value of this type can be limited by passing the
    `minlen' and `maxlen' constructor arguments.

    Constructor arguments:

      minlen -- minimal length of a value of this type as integer or
        'None'; 'None' denotes unlimited minimal length.

      maxlen -- maximal length of a value of this type as integer or
        'None'; 'None' denotes unlimited maximal length.

    Other arguments are passed to the parent constructor.

    """

    # Translators: User input validation error message.
    _MSG_MINLEN = _(u"Minimal size %(minlen)s not satisfied")
    # Translators: User input validation error message.
    _MSG_MAXLEN = _(u"Maximal size %(maxlen)s exceeded")

    def _init(self, minlen=None, maxlen=None, **kwargs):
        self._minlen = minlen
        self._maxlen = maxlen
        super(Limited, self)._init(**kwargs)

    def _comparison_key(self):
        return super(Limited, self)._comparison_key() + (self._minlen, self._maxlen)

    def minlen(self):
        """Return the minimal length of the value as an integer or 'None'.

        'None' denotes unlimited minimal length.

        """
        return self._minlen

    def maxlen(self):
        """Return the maximal length of the value as an integer or 'None'.

        'None' denotes unlimited length.

        """
        return self._maxlen

    def _format_length(self, length):
        return str(length)

    def _check_constraints(self, value, **kwargs):
        super(Limited, self)._check_constraints(value, **kwargs)
        self._check_limits(value)

    def _check_limits(self, value):
        if value is not None:
            if self._minlen is not None and len(value) < self._minlen:
                # Translators: User input validation error message.
                raise ValidationError(self._MSG_MINLEN %
                                      dict(minlen=self._format_length(self._minlen)))
            if self._maxlen is not None and len(value) > self._maxlen:
                # Translators: User input validation error message.
                raise ValidationError(self._MSG_MAXLEN %
                                      dict(maxlen=self._format_length(self._maxlen)))


class Range(Type):
    """Representation of range types.

    Those types are available in PostgreSQL >= 9.2.

    This class should be inherited to corresponding base types to make new
    range types.  Validation accepts pairs of strings which are validated by
    calling the superclass.  Export returns 'Range.Range' instance.  NULL
    values are represented by single 'None' values in export and by a
    'Range.Range' instance of empty strings on validation.

    """
    class Range(object):

        _type = None
        _default_lower_inc = True
        _default_upper_inc = False

        def __init__(self, lower, upper, lower_inc=None, upper_inc=None):
            self._lower = lower
            self._upper = upper
            if lower_inc is None:
                lower_inc = self._default_lower_inc
            self._lower_inc = lower_inc
            if upper_inc is None:
                upper_inc = self._default_upper_inc
            self._upper_inc = upper_inc

        def lower(self):
            return self._lower

        def upper(self):
            return self._upper

        def lower_inc(self):
            return self._lower_inc

        def upper_inc(self):
            return self._upper_inc

        def __eq__(self, other):
            if not sameclass(self, other):
                return NotImplemented
            else:
                t = self._type
                if t:
                    r1, r2 = t.adjust_value(self), t.adjust_value(other)
                else:
                    r1, r2 = self, other
                return r1._lower == r2._lower and r1._upper == r2._upper

        def __ne__(self, other):
            # Implied automatically in Python 3 so can be removed when dropping Python 2 support.
            return not self == other

        def __len__(self):
            return 2

        def __getitem__(self, key):
            if key == 0:
                return self.lower()
            elif key == 1:
                return self.upper()
            else:
                raise IndexError(key)

    _NULL_RANGE_VALUE = ('', '')

    def _init(self, lower_inc=True, upper_inc=False, **kwargs):
        """
        Arguments:

          lower_inc -- indicates whether the lower bound is inclusive; boolean
          upper_inc -- indicates whether the upper bound is inclusive; boolean

        """
        self._lower_inc = lower_inc
        self._upper_inc = upper_inc

        class InstanceRange(Range.Range):
            _type = self
            _default_lower_inc = lower_inc
            _default_upper_inc = upper_inc
        self.Range = InstanceRange
        super(Range, self)._init(**kwargs)

    def _comparison_key(self):
        return super(Range, self)._comparison_key() + (self._lower_inc, self._upper_inc)

    def _validate(self, obj, **kwargs):
        if obj == self._NULL_RANGE_VALUE:
            return Value(self, None)
        if not len(obj) == 2:
            raise ValueError("need a sequence of 2 values to validate")
        base_type = self.base_type()
        values = []
        for o in obj:
            v, e = base_type.validate(o, **kwargs)
            if e is not None:
                raise e
            else:
                assert v is not None
                values.append(v)
        v1, v2 = [v.value() for v in values]
        if v1 is not None and v2 is not None and v1 > v2:
            # PostgreSQL accepts values under the opposite condition regardless of bound kinds.
            raise ValidationError(_("Lower range bound higher than the upper one"))
        return Value(self, self.Range(v1, v2))

    def export(self, value, *args, **kwargs):
        if value is None:
            return self._NULL_RANGE_VALUE
        elif not value:
            v1 = v2 = value
        else:
            v1, v2 = value
        return super(Range, self).export(v1, **kwargs), super(Range, self).export(v2, **kwargs)

    def lower_inc(self):
        return self._lower_inc

    def upper_inc(self):
        return self._upper_inc

    _LOWER_BOUND = 'LOWER_BOUND'
    _UPPER_BOUND = 'UPPER_BOUND'

    def _adjust_bound(self, bound, value, value_inc):
        assert isinstance(value_inc, bool), value_inc
        if bound is self._LOWER_BOUND:
            type_inc = self.lower_inc()
        elif bound is self._UPPER_BOUND:
            type_inc = self.upper_inc()
        else:
            raise Exception("Invalid bound", bound)
        if type_inc != value_inc:
            if (((bound is self._LOWER_BOUND and not value_inc) or
                 (bound is self._UPPER_BOUND and value_inc))):
                change_function = self._increase_bound
            else:
                change_function = self._decrease_bound
            value = change_function(value)
        return value

    def _increase_bound(self, value):
        raise Exception("Changing bounds not possible for this type")

    def _decrease_bound(self, value):
        raise Exception("Changing bounds not possible for this type")

    def adjust_value(self, value):
        if value is None:
            return None
        if not isinstance(value, (Range.Range, tuple, list)):
            raise TypeError("Value not a range", value)
        adjust = self.base_type().adjust_value
        adjusted = [adjust(v) for v in value]
        if isinstance(value, Range.Range):
            adjusted = [self._adjust_bound(self._LOWER_BOUND, adjusted[0], value.lower_inc()),
                        self._adjust_bound(self._UPPER_BOUND, adjusted[1], value.upper_inc())]
        if adjusted[0] is not None and adjusted[1] is not None and adjusted[0] > adjusted[1]:
            raise TypeError("Lower range bound higher than the upper one", adjusted)
        result = self.Range(*adjusted)
        return result

    def base_type(self):
        """Return instance of the underlying types of the range type.

        This is the type of the two values of the range pair.

        """
        raise Exception("Not implemented")


class Integer(Number):
    """Libovolný integer."""

    def _validate(self, obj):
        """Attempt to convert given 'obj' integer.

        Pokud je 'obj' možno převést integer, je správný a vrácená instance
        třídy 'Value' obsahuje odpovídající hodnotu jako integer.  Pokud
        'obj' není možno převést na integer, 'obj' není správný a je
        vrácena chyba.

        Metoda validuje všechny zápisy integers akceptované Pythonem, zejména
        tedy i long integers ve tvaru '1L'.

        """
        assert isinstance(obj, basestring), obj
        try:
            value = int(obj)
        except Exception:
            try:
                value = long(obj)
            except Exception:
                value = None
        if value is None:
            # Translators: User input validation error message.
            raise ValidationError(_(u"Not an integer"))
        return Value(self, value)

    def adjust_value(self, value):
        if value is None:
            return None
        if isinstance(value, decimal.Decimal):
            if value % 1 != 0:
                raise TypeError("Value not an integer", value)
            value = int(value)
        elif not isinstance(value, (int, long)):
            raise TypeError("Value not an integer", value)
        return value

    def _export(self, value, **kwargs):
        return '%d' % (value,)

    def sqlalchemy_type(self):
        return sqlalchemy.Integer()


class IntegerRange(Range, Integer):

    def sqlalchemy_type(self):
        import pytis.data.gensqlalchemy
        return pytis.data.gensqlalchemy.INT4RANGE()

    def base_type(self):
        return pytis.data.Integer()

    def _increase_bound(self, value):
        if value is None:
            return None
        return value + 1

    def _decrease_bound(self, value):
        if value is None:
            return None
        return value - 1


class SmallInteger(Integer):

    def sqlalchemy_type(self):
        return sqlalchemy.SmallInteger()


class LargeInteger(Integer):

    def sqlalchemy_type(self):
        return sqlalchemy.BigInteger()


class LargeIntegerRange(Range, Integer):

    def sqlalchemy_type(self):
        import pytis.data.gensqlalchemy
        return pytis.data.gensqlalchemy.INT8RANGE()

    def base_type(self):
        return pytis.data.LargeInteger()

    def _increase_bound(self, value):
        if value is None:
            return None
        return value + 1

    def _decrease_bound(self, value):
        if value is None:
            return None
        return value - 1


class Serial(Integer):
    """Integer s automaticky generovanými hodnotami.

    Typ oproti 'Integer' nezavádí žádné nové rysy, jeho význam je čistě
    specifikační.  Například uživatelské rozhraní tak získává informaci, že
    není třeba ani žádoucí explicitně nastavovat hodnoty sloupců tohoto typu
    v řádku při vkládání nového záznamu.

    """

    def _init(self, not_null=True, **kwargs):
        super(Serial, self)._init(not_null=not_null, **kwargs)

    def sqlalchemy_type(self):
        import pytis.data.gensqlalchemy
        return pytis.data.gensqlalchemy.SERIAL()


class LargeSerial(Integer):

    def sqlalchemy_type(self):
        import pytis.data.gensqlalchemy
        return pytis.data.gensqlalchemy.BIGSERIAL()


class Float(Number):
    """Floating point number.

    Constructor arguments:

      precision -- non-negative integer determining the number of digits after
        decimal point in the exported value, or 'None' (no explicit limit on
        the precision)
      digits -- maximum number of digits, integer, or 'True' (the number is
        precise, with arbitrary number of digits after decimal point; this is
        useful for database definitions of unqualified NUMERIC types), or
        'None' (unspecified value)

    Other keyword arguments are the same is in the superclass.

    """
    CEILING = decimal.ROUND_CEILING
    """Konstanta pro typ zaokrouhlení ve 'validate'."""
    FLOOR = decimal.ROUND_FLOOR
    """Konstanta pro typ zaokrouhlení ve 'validate'."""

    def _init(self, precision=None, digits=None, **kwargs):
        super(Float, self)._init(**kwargs)
        assert precision is None or precision >= 0, ('Invalid precision', precision,)
        assert digits is None or digits is True or isinstance(digits, int), digits
        if precision is None:
            format = '%f'
        else:
            format = '%%.%df' % (precision,)
        self._format_string = format
        self._precision = precision
        self._digits = digits

    def _comparison_key(self):
        return super(Float, self)._comparison_key() + (self._precision, self._digits)

    def precision(self):
        """Vrať přesnost čísla zadanou v konstruktoru jako integer."""
        return self._precision

    def digits(self):
        """Return number of digits given in the constructor."""
        return self._digits

    def _validate(self, obj, precision=None, rounding=None, locale_format=True):
        """Pokus se převést 'obj' na float.

        Pokud je 'obj' možno převést na float, je správný a vrácená instance
        třídy 'Value' obsahuje odpovídající hodnotu jako plain integer.  Pokud
        'obj' převést možno není, 'obj' není správný a je vrácena chyba.

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
        assert isinstance(obj, basestring), ('Not a string', obj)
        assert (precision is None or isinstance(precision, int) and precision >= 0), \
               ('Invalid precision', precision)
        try:
            if locale_format:
                import locale
                value = locale.atof(obj)
            else:
                value = decimal.Decimal(obj)
        except (ValueError, decimal.InvalidOperation):
            raise ValidationError(_(u"Invalid number"))
        if precision is None:
            precision = self._precision
        digits = self._digits
        # We have to handle floats (as produced by 'locale.atof') very
        # carefully.
        if digits is not None:
            with decimal.localcontext() as context:
                context.prec = len(str(value)) if digits is True else digits
                if rounding:
                    context.rounding = rounding
                if isinstance(value, float):
                    # Round the value -- any better way?
                    str_value = ('%%.%dg' % (context.prec,)) % (value,)
                    rvalue = float(str_value)
                    correction = 0
                    if rounding:
                        if rounding == self.CEILING:
                            if rvalue < value:
                                correction = 1
                        elif rounding == self.FLOOR:
                            if rvalue > value:
                                correction = -1
                        else:
                            raise ProgramError('Invalid rounding argument', rounding)
                    if precision is None:
                        format_ = '%%.%dg' % (context.prec,)
                    else:
                        format_ = '%%.%df' % (precision,)
                    value = decimal.Decimal(format_ % (rvalue,))
                    if correction:
                        rvalue = rvalue + correction * (10 ** -value.as_tuple().exponent)
                        value = decimal.Decimal(format_ % (rvalue,))
                else:
                    value = +decimal.Decimal(value)
        if precision is not None:
            if isinstance(value, float):
                rvalue = round(value, precision)
                if rounding:
                    if rounding == self.CEILING:
                        if rvalue < value:
                            rvalue = rvalue + 10 ** -precision
                    elif rounding == self.FLOOR:
                        if rvalue > value:
                            rvalue = rvalue - 10 ** -precision
                    else:
                        raise ProgramError('Invalid rounding argument', rounding)
                value = decimal.Decimal(('%%.%df' % (precision,)) % (rvalue,))
            else:
                with decimal.localcontext() as context:
                    context.prec = 100
                    if rounding:
                        context.rounding = rounding
                    quantizer = decimal.Decimal('1.' + '0' * precision)
                    value = decimal.Decimal(value).quantize(quantizer)
        if self._digits is None and self._precision is None:
            value = float(value)
        return Value(self, value)

    def _export(self, value, locale_format=True):
        if locale_format:
            import locale
            result = locale.format_string(self._format_string, value, 1)
            if sys.version_info[0] == 2:
                encoding = locale.getpreferredencoding() or 'UTF-8'
                result = result.decode(encoding)
        else:
            result = unistr(self._format_string % value)
        return result

    def adjust_value(self, value):
        if value is None:
            return None
        if self._precision is not None or self._digits is not None:
            cast = decimal.Decimal
        else:
            cast = float
        return cast(value)

    def sqlalchemy_type(self):
        if self._digits is True:
            alchemy_type = sqlalchemy.Numeric()
        elif self._precision is not None and self._digits is not None:
            alchemy_type = sqlalchemy.Numeric(precision=self.digits(), scale=self.precision())
        elif self._digits is not None:
            alchemy_type = sqlalchemy.Numeric(precision=self.digits())
        else:
            alchemy_type = sqlalchemy.Float(precision=self.precision())
        return alchemy_type


class DoublePrecision(Float):
    """Database double precision type.

    Useful in specifications.

    """

    def __init__(self, **kwargs):
        super(DoublePrecision, self).__init__(**kwargs)

    def adjust_value(self, value):
        if value is not None and not isinstance(value, float):
            raise TypeError("Value not a float", value)
        return value

    def sqlalchemy_type(self):
        return sqlalchemy.dialects.postgresql.DOUBLE_PRECISION()


class Monetary(Float):
    """Monetary type.

    Just identifies monetary fields and sets the default precision to 2.

    Web forms will be able to render such fields with locale specific monetary
    formatting.

    """

    def _init(self, precision=2, **kwargs):
        super(Monetary, self)._init(precision=precision, **kwargs)


class String(Limited):
    """Libovolný string.

    Lze také specifikovat, že řetězec může mít pouze omezenou délku, blíže viz
    metody '_init' a 'maxlen'.

    """
    _SPECIAL_VALUES = Type._SPECIAL_VALUES + ((None, ''),)
    # Translators: User input validation error message.
    _MSG_MINLEN = _(u"Minimal length %(minlen)s characters not satisfied")
    # Translators: User input validation error message.
    _MSG_MAXLEN = _(u"Maximal length %(maxlen)s characters exceeded")

    def _validate(self, obj):
        """Vrať instanci třídy 'Value' s hodnotou 'obj'.

        Pokud byla v konstruktoru specifikována maximální délka, 'obj' je
        správný právě tehdy, není-li delší než tato délka.

        """
        assert isinstance(obj, basestring), ('Not a string', obj)
        return Value(self, unistr(obj))

    def _export(self, value):
        # Pozor, na triviální funkci této metody se spoléhá Value.__init__ --
        # při změně zde je nutná změna i tam.
        assert isinstance(value, basestring), ('Value not a string', value)
        return value if isinstance(value, unistr) else unistr(value)

    def wm_validate(self, obj):
        assert isinstance(obj, basestring)
        return WMValue(self, obj), None

    def adjust_value(self, value):
        if value is not None and not isinstance(value, basestring):
            raise TypeError("Value not a string", value)
        return value

    def sqlalchemy_type(self):
        if self.minlen() is None and self.maxlen() is None:
            result = sqlalchemy.TEXT()
        elif self.minlen() and self.maxlen() and self.minlen() == self.maxlen():
            result = sqlalchemy.CHAR(length=self.minlen())
        else:
            result = sqlalchemy.String(length=self.maxlen())
        return result


class Name(String):
    """String type to be identified as 'name' in the database.

    Useful in database specifications.

    """

    def sqlalchemy_type(self):
        import pytis.data.gensqlalchemy
        return pytis.data.gensqlalchemy.NAME()


class Password(String):
    """Specialized string type for password fields.

    The user interface should handle passwords differently from ordinary
    strings.

    1. The values must be treated as sensitive information which should never
       appear on the screen as well as in logs etc.

    2. Also, if the constructor argument 'verify' is true (it is by default),
       the user should be required to type the new value twice to prevent typos
       (since there is no visual feedback).  Thus the user interface should
       create two input fields in this case.  The value of the second field
       must be passed as the 'verify' argument to the 'validate()' method.

    Constructor arguments:

      verify -- boolean flag indicating, that user input should be verified by
        the user interface by presenting two controls for entering the
        password.  Both inputs must match to pass validation.

      strength -- specification of password strength checking.  If 'None', no
        special checks are performed.  If 'True', default checking implemented
        in the '_check_strength' method is performed.  If anything else, it
        must be a function of a single argument, the password string, that
        returns either 'None' when the password is strong enough or an error
        message if the password is weak.

      md5 -- DEPRECATED; Use a virtual field for password entry and a computer
        function to create a hash; The built-in md5 hashing doesn't use salt so
        can not be considered secure; Original docstring: boolean flag
        indicating, that the password is stored as a hexadeximal md5 hash.
        This will lead to automatic conversion of user input to its md5 hash,
        so the original password is no more visible anywhere after successful
        validation.  The conversion is only done when the 'verify' argument is
        passed to the 'validate()' method.  When 'verify' is not used, the
        input string is not considered to be user input, but an already hashed
        value (eg. read from data source).

    Other arguments are passed to the parent constructor.

    The validation argument 'verify' should be always passed when validating
    user input.  It may be omitted if validation is used just to convert an
    already validated string value (e.g. read from database) to a 'Value'
    instance.  When user input is validated, but the type doesn't require
    verification (the user enters the password just once), it is thus necessary
    to pass the same value twice (as the validated value and as the 'verify'
    argument).

    """
    def _init(self, md5=False, verify=True, strength=None, **kwargs):
        super(Password, self)._init(**kwargs)
        assert isinstance(md5, bool)
        assert isinstance(verify, bool)
        self._md5 = md5
        self._verify = verify
        if strength is True:
            self._strength = self._check_strength
        else:
            self._strength = strength

    def _comparison_key(self):
        return super(Password, self)._comparison_key() + (self._verify, self._strength)

    def verify(self):
        """Return true if verification of user input is required."""
        return self._verify

    def _check_strength(self, obj):
        letters = non_letters = False
        for char in obj:
            if char in string.ascii_letters:
                letters = True
            else:
                non_letters = True
        if not letters or not non_letters:
            # Translators: User input validation error message.
            return _(u"Please use mix of letters and non-letters in your password")

    def _validate(self, obj, verify=None, **kwargs):
        if verify is not None:
            if not verify:
                # Translators: User input validation error message.
                raise ValidationError(_(u"Enter the password twice to eliminate typos"))
            if obj != verify:
                # Translators: User input validation error message.
                raise ValidationError(_(u"Passwords don't match"))
        if self._strength is not None:
            error = self._strength(obj)
            if error is not None:
                raise ValidationError(error)
        return super(Password, self)._validate(obj, **kwargs)

    def validate(self, obj, verify=None, **kwargs):
        if self._md5 and verify is None:
            # Strict checking applies to the original value.  Here we are validating the md5 sum,
            # so strict checking is forced to False.
            kwargs['strict'] = False
        value, error = super(Password, self).validate(obj, verify=verify, **kwargs)
        if self._md5 and value and value.value() is not None:
            obj = value.value()
            if isinstance(obj, unistr):
                obj = obj.encode('utf-8')
            if verify is not None:
                # User input was valid, so let's turn it into its md5 hash.
                from hashlib import md5
                value = Value(value.type(), md5(obj).hexdigest())
            elif len(obj) != 32 or not obj.isalnum():
                # Translators: User input validation error message.
                return None, self._validation_error(_(u"Invalid MD5 hash"))
        return value, error


class RegexString(String):

    # Translators: User input validation error message.
    _MSG_INVALID_FORMAT = _(u"Invalid format")
    _REGEX = None

    def _init(self, regex=None, **kwargs):
        super(RegexString, self)._init(**kwargs)
        if regex is None:
            self._regex = self._REGEX
        else:
            self._regex = re.compile(regex)

    def _comparison_key(self):
        return super(RegexString, self)._comparison_key() + (self._regex,)

    def _check_constraints(self, value, **kwargs):
        super(RegexString, self)._check_constraints(value, **kwargs)
        if value is not None and not self._regex.match(value):
            raise ValidationError(self._MSG_INVALID_FORMAT)


class Color(RegexString):
    """Barva reprezentovaná řetězcem '#RRGGBB'."""

    # Translators: User input validation error message.
    _MSG_INVALID_FORMAT = _(u"Invalid color format ('#RGB' or '#RRGGBB')")
    _REGEX = re.compile(r'^\#[0-9a-fA-F]{3,3}([0-9a-fA-F]{3,3})?$')

    def sqlalchemy_type(self):
        return sqlalchemy.String(length=7)


class Inet(String):
    """IPv4 nebo IPv6 adresa."""
    _INET4_FORMAT = re.compile(r'(\d{1,3}(\.\d{1,3}){0,3}([/]\d{1,2}){0,1})$')

    def _validate(self, obj, *args, **kwargs):
        # TODO: Doplnit i validaci pro IPv6 formát
        if not self._INET4_FORMAT.match(obj):
            # Translators: User input validation error message.
            raise ValidationError(_(u"Invalid format"))
        if obj.find('/') != -1:
            addr, mask = obj.split('/')
            if int(mask) > 32:
                # Translators: User input validation error message.
                raise ValidationError(_(u"Invalid inet address mask: %(mask)s", mask=mask))
        else:
            addr, mask = obj, '32'
        numbers = addr.split('.')
        for n in numbers:
            if n and int(n) > 255:
                # Translators: User input validation error message.
                raise ValidationError(_(u"Invalid inet address value %(addr)s", addr=addr))
        for i in range(len(numbers), 4):
            numbers.append('0')
        value = '%s/%s' % ('.'.join(numbers), mask)
        return Value(self, unistr(value))

    def sqlalchemy_type(self):
        return sqlalchemy.dialects.postgresql.INET()


class Macaddr(String):
    """MAC adresa."""

    _MACADDR_FORMAT = re.compile('([0-9a-fA-F]{2}[-:]{0,1}){5}[0-9a-fA-F]{2}$')

    def _validate(self, obj, *args, **kwargs):
        if not self._MACADDR_FORMAT.match(obj):
            # Translators: User input validation error message.
            raise ValidationError(_(u"Invalid format"))
        macaddr = obj.replace(':', '').replace('-', '')
        value = ':'.join([macaddr[x:x + 2] for x in range(0, len(macaddr), 2)])
        return Value(self, unistr(value))

    def sqlalchemy_type(self):
        return sqlalchemy.dialects.postgresql.MACADDR()


class Email(String):
    """E-mail address."""
    # Taken from HTML5.  Not compliant with RFC 5322 but it should be good
    # enough for practical purposes.
    _EMAIL_FORMAT = re.compile(r"^[a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+@"
                               r"[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?"
                               r"(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$")

    def _validate(self, obj, *args, **kwargs):
        if not self._EMAIL_FORMAT.match(obj):
            # Translators: User input validation error message.
            raise ValidationError(_(u"Invalid format"))
        return Value(self, unistr(obj))


class TreeOrderBase(Type):
    """Literal numeric value denoting the level of the item within the tree structure.

    The type itself does not implement any specific features.  It has strictly specificational
    meaning.  If such a column is detected within a list, the user interface may try to render the
    tree structure of the items according to the tree level value.

    """
    pass


class TreeOrder(String, TreeOrderBase):
    """String type with tree ordering property."""
    pass


class FullTextIndex(String):
    """Type representing full text index columns.

    This is a special type with limited use and promiscuous values.  Values of
    this type can't be directly read from the database and they can't be
    inserted at all into it.  All this type is able to do is to enable access
    to 'pytis.data.FT' operator and to enable access to full text search result
    headlines.

    Constructor arguments:

      columns -- tuple of column ids (strings), these columns will be included
        for the purpose of generating full text search headlines when a column
        of this type is included in the full text search

    """

    def _init(self, columns=(), **kwargs):
        assert isinstance(columns, (list, tuple)), ("Invalid argument type", columns,)
        super(FullTextIndex, self)._init(**kwargs)
        self._columns = columns

    def _comparison_key(self):
        return super(FullTextIndex, self)._comparison_key() + (self._columns,)

    def columns(self):
        """Return sequence of column ids given in constructor."""
        return self._columns


class _LocalTimezone(datetime.tzinfo):

    def __init__(self):
        self._offset = datetime.timedelta(seconds=-time.timezone)
        if time.daylight:
            self._dst_offset = datetime.timedelta(seconds=-time.altzone)
        else:
            self._dst_offset = self._offset
        self._offset_diff = self._dst_offset - self._offset
        self._zero_diff = datetime.timedelta(0)

    def __repr__(self):
        return '%s.%s()' % (self.__class__.__module__, self.__class__.__name__)

    def utcoffset(self, dt):
        if self._dst(dt):
            return self._dst_offset
        else:
            return self._offset

    def dst(self, dt):
        if self._dst(dt):
            return self._offset_diff
        else:
            return self._zero_diff

    def tzname(self, dt):
        return time.tzname[self._dst(dt)]

    def _dst(self, dt):
        try:
            # mktime is not guaranteed to work for all dates and overflows were
            # observed for reasonable dates on an 32-bit system
            stamp = time.mktime((dt.year, dt.month, dt.day, dt.hour, dt.minute, dt.second,
                                 dt.weekday(), 0, 0))
        except Exception:
            return False
        localtime = time.localtime(stamp)
        return localtime.tm_isdst > 0


class _UTCTimezone(datetime.tzinfo):

    def __init__(self):
        self._zero_diff = datetime.timedelta(0)

    def __repr__(self):
        return '%s.%s()' % (self.__class__.__module__, self.__class__.__name__)

    def utcoffset(self, dt):
        return self._zero_diff

    def tzname(self, dt):
        return "UTC"

    def dst(self, dt):
        return self._zero_diff


class _CommonDateTime(Type):
    """Common base class of all date and time types.

    All the derived classes use classes from Python 'datetime' module to
    represent the date and/or time values.

    Constructor arguments:

      format -- specification of both input and output format of date
        and/or time in the form accepted by `time.strftime()'.
      mindate, maxdate -- limits of acceptable date/time
      utc -- specifies whether timestamp in the database is in UTC
      without_timezone -- iff true then use WITHOUT TIMEZONE when declaring the
        type in the database.  This is only to support legacy tables, all new
        database objects should be created WITH TIMEZONE.
      precision -- optional precision value p which specifies the number
        of fractional digits retained in the seconds field, default 0

    """
    _SPECIAL_VALUES = Type._SPECIAL_VALUES + ((None, ''),)
    _MSG_INVALID_DT_FORMAT = _(u"Invalid date or time format")

    UTC_TZINFO = _UTCTimezone()
    LOCAL_TZINFO = _LocalTimezone()

    def _init(self, format, utc=True, without_timezone=False, precision=0, **kwargs):
        assert format is True or isinstance(format, basestring), format
        assert isinstance(utc, bool), utc
        self._format = format
        self._utc = utc
        if utc:
            self._timezone = self.UTC_TZINFO
        else:
            self._timezone = self.LOCAL_TZINFO
        self._check_matcher = {}
        self._without_timezone = without_timezone
        self._precision = precision
        super(_CommonDateTime, self)._init(**kwargs)

    def _comparison_key(self):
        return super(_CommonDateTime, self)._comparison_key() + (self._format, self._utc,
                                                                 self._without_timezone)

    def _check_format(self, format, obj):
        try:
            matcher = self._check_matcher[format]
        except KeyError:
            special = {'%Y': r'\d\d\d\d', ' ': r'\s+', '%p': '[AP]M'}

            def subst(match):
                m = match.group(1)
                try:
                    return special[m]
                except KeyError:
                    return m.startswith('%') and r'\d?\d' or re.escape(m)
            regexp = re.sub(r'(\%[a-zA-Z]|.|\s+)', subst, format)
            self._check_matcher[format] = matcher = re.compile(regexp)
        return matcher.match(obj)

    def format(self):
        """Return format given in the constructor, basestring."""
        return self._format

    def utc(self):
        """Return 'utc' flag value given in the constructor, boolean."""
        return self._utc

    def timezone(self):
        """Return 'datetime.tzinfo' object corresponding to the time zone."""
        return self._timezone

    def precision(self):
        return self._precision

    def is_utc(self):
        """Deprecated.  Use 'utc' instead."""
        return self.utc()

    def _validate(self, obj, format=None, local=None):
        """Stejné jako v předkovi až na klíčované argumenty.

        Argumenty:

          obj -- stejné jako v předkovi
          format -- požadovaný formát hodnoty 'obj', ve tvaru požadovaném
            metodou '_init()'
          local -- if true, handle the given value as a local time value; if
            false, handle it as a UTC value; if 'None' handle it according to
            utc flag of the type

        """
        assert isinstance(obj, basestring), obj
        if format is None:
            format = self._format
        if local is None:
            local = (not self._utc)
        # Využití `strptime' je nejjednodušší řešení.  GNU `strptime' je
        # dostatečně tolerantní vůči nadbytečným mezerám atd., takže by jeho
        # použitím neměl vzniknout problém, pokud nehodláme software provozovat
        # na ne-GNU systémech, které `strptime' řádně nepodporují.
        obj = obj.strip()
        dt = None
        if not self._check_format(format, obj):
            raise ValidationError(self._MSG_INVALID_DT_FORMAT)
        try:
            dt = datetime.datetime.strptime(obj, format)
            if local:
                dt = dt.replace(tzinfo=self.LOCAL_TZINFO)
            elif dt.tzinfo is None:
                dt = dt.replace(tzinfo=self.UTC_TZINFO)
            if self._utc:
                dt = dt.astimezone(self.UTC_TZINFO)
        except Exception:
            raise ValidationError(self._MSG_INVALID_DT_FORMAT)
        return Value(self, dt)

    def _export(self, value, local=None, format=None):
        return value.strftime(format or self._format)

    def without_timezone(self):
        return self._without_timezone

    @classmethod
    def now(class_, **kwargs):
        """Return 'Value' instance of this type of the current moment.

        Arguments:

          kwargs -- arguments passed to the class constructor

        """
        type_ = class_(**kwargs)
        if kwargs.get('without_timezone'):
            tz = False
        else:
            tz = type_._timezone
        return Value(type_, class_.datetime(tz=tz))

    @classmethod
    def datetime(class_, tz=None):
        """Return value corresponding to the current moment.

        The returned value is instance of a 'datetime' module class which is
        used to represent internal values of the given pytis type.

        Arguments:

          tz -- determines time zone of the value; if 'None', UTC is used, if
            'True', local time zone is used, if 'False', don't use any time
            zone; otherwise it must be a 'datetime.tzinfo' instance to use

        """
        if tz is None:
            tz = class_.UTC_TZINFO
        elif tz is True:
            tz = class_.LOCAL_TZINFO
        elif tz is False:
            tz = None
        return class_._datetime(tz=tz)

    @classmethod
    def _datetime(class_, tz):
        raise Exception("Not implemented")

    def sqlalchemy_type(self):
        return sqlalchemy.DateTime(timezone=True)


class DateTime(_CommonDateTime):
    """Time stamp represented by a 'datetime.datetime' instance.

    The class can work only with absolute times.  The time can be local or UTC,
    depending on the constructor parameter; each time value must contain time
    zone.

    The date and time format is the same for both import and export and is
    determined by constructor arguments.

    Constructor arguments:

      format -- specification of both input and output format of date
        and/or time in the form accepted by `time.strftime()'.
        May be also None in which case the configuration option
        'config.date_time_format' is used.  The class defines '*_FORMAT'
        constants which may be used as a value of this argument.
      mindate, maxdate -- limits of acceptable date/time
      precision -- optional precision value p which specifies the number
        of fractional digits retained in the seconds field, default 0
      utc -- specifies, if timestamp in database is in UTC

    """
    DEFAULT_FORMAT = '%Y-%m-%d %H:%M:%S'
    """Implicitní formát data a času."""
    SQL_FORMAT = DEFAULT_FORMAT
    """Formát data a času používaný standardně SQL stroji."""
    CZECH_FORMAT = '%d.%m.%Y %H:%M:%S'
    """Český \"účetnický\" formát data a času."""

    _ISO_TZ_MATCHER = re.compile('(?P<sign>[-+])(?P<hours>[0-9]+):(?P<minutes>[0-9]+)')

    def _init(self, format=None, mindate=None, maxdate=None, utc=True, precision=0, **kwargs):
        assert mindate is None or isinstance(mindate, basestring)
        assert maxdate is None or isinstance(maxdate, basestring)
        if format is None:
            format = pytis.config.date_time_format
        super(DateTime, self)._init(format=format, utc=utc, **kwargs)
        if mindate:
            try:
                self._mindate = datetime.datetime.strptime(mindate, self.DEFAULT_FORMAT)
            except Exception:
                raise ProgramError('Bad value for mindate', mindate, self.DEFAULT_FORMAT)
        else:
            self._mindate = None
        if maxdate:
            try:
                self._maxdate = datetime.datetime.strptime(maxdate, self.DEFAULT_FORMAT)
            except Exception:
                raise ProgramError('Bad value for maxdate', maxdate)
        else:
            self._maxdate = None

    def _comparison_key(self):
        return super(DateTime, self)._comparison_key() + (self._mindate, self._maxdate)

    def _validate(self, obj, format=None, **kwargs):
        if format is True:
            value = self._validate_iso(obj, **kwargs)
        else:
            value = super(DateTime, self)._validate(obj, format=format, **kwargs)
        if value is not None:
            dt = value.value()
            if (((self._mindate and dt < self._mindate) or
                 (self._maxdate and dt > self._maxdate))):
                raise ValidationError(_(u"Date outside the allowed range"))
        return value

    def _validate_iso(self, obj, local=None, **kwargs):
        common_string, shift_string = obj[:-6], obj[-6:]
        match = self._ISO_TZ_MATCHER.match(shift_string)
        if match is None:
            common_string = obj
            if local or (local is None and not self._utc):
                tzinfo = self.LOCAL_TZINFO
            else:
                tzinfo = self.UTC_TZINFO
            shift = 0
        else:
            groups = match.groupdict()
            shift = int(groups['hours']) * 3600 + int(groups['minutes']) * 60
            if groups['sign'] == '-':
                shift = -shift
            tzinfo = self.UTC_TZINFO
        if common_string.find('.') >= 0:
            format_ = '%Y-%m-%d %H:%M:%S.%f'
        else:
            format_ = '%Y-%m-%d %H:%M:%S'
        try:
            value = datetime.datetime.strptime(common_string, format_)
        except Exception:
            raise ValidationError(self._MSG_INVALID_DT_FORMAT)
        value = value - datetime.timedelta(seconds=shift)
        value = datetime.datetime(value.year, value.month, value.day, value.hour, value.minute,
                                  value.second, value.microsecond, tzinfo)
        return Value(self, value)

    def _export(self, value, local=None, format=None):
        """Stejné jako v předkovi až na klíčované argumenty.

        Arguments:

          local -- if true then the value is exported in local time, otherwise
            it is exported in UTC; if 'None' handle it according to utc flag of
            the type

        """
        assert isinstance(value, datetime.datetime), value
        if local is None:
            local = not self._utc
        if local:
            tzinfo = self.LOCAL_TZINFO
        else:
            tzinfo = self.UTC_TZINFO
        if not self._without_timezone:
            value = value.astimezone(tzinfo)
        if format is True:
            if sys.version_info[0] == 2:
                exported = value.isoformat(b' ')
            else:
                exported = value.isoformat(' ')
        else:
            exported = self._strftime(value, format or self._format)
        return exported

    def _strftime(self, value, format):
        # Python datetime.strftime() doesn't support dates before 1900-01-01.
        # The following code works around that.  The original author is Andrew
        # Dalke who posted this code to
        # http://code.activestate.com/\
        # recipes/306860-proleptic-gregorian-dates-and-strftime-before-1900/
        def findall(text, substr):
            # Also finds overlaps
            sites = []
            i = 0
            while 1:
                j = text.find(substr, i)
                if j == -1:
                    break
                sites.append(j)
                i = j + 1
            return sites
        # Every 28 years the calendar repeats, except through century leap
        # years where it's 6 years.  But only if you're using the Gregorian
        # calendar.  ;)
        # For every non-leap year century, advance by
        # 6 years to get into the 28-year repeat cycle
        year = value.year
        if year > 1900:
            return value.strftime(format)
        delta = 2000 - year
        off = 6 * (delta // 100 + delta // 400)
        year = year + off
        # Move to around the year 2000
        year = year + ((2000 - year) // 28) * 28
        timetuple = value.timetuple()
        s1 = time.strftime(format, (year,) + timetuple[1:])
        sites1 = findall(s1, str(year))
        s2 = time.strftime(format, (year + 28,) + timetuple[1:])
        sites2 = findall(s2, str(year + 28))
        sites = []
        for site in sites1:
            if site in sites2:
                sites.append(site)
        s = s1
        syear = "%4d" % (value.year,)
        for site in sites:
            s = s[:site] + syear + s[site + 4:]
        return s

    @classmethod
    def _datetime(class_, tz):
        return datetime.datetime.now(tz)

    @staticmethod
    def diff_seconds(dt1, dt2):
        """Return difference between d1 and d2 in seconds.

        Arguments:

          dt1 -- start datetime; 'Value' instance of 'DateTime' type
          dt2 -- end datetime; 'Value' instance of 'DateTime' type

        """
        diff = dt2.value() - dt1.value()
        return diff.days * 86400 + diff.seconds

    @classmethod
    def current_gmtime(class_):
        """Deprecated.  Use 'datetime' method instead.
        """
        return class_.datetime()

    def adjust_value(self, value):
        if value is not None and not isinstance(value, datetime.datetime):
            raise TypeError("Value not a datetime", value)
        timezone = self.timezone()
        if value is not None:
            if self._without_timezone:
                if value.tzinfo is not None:
                    value = datetime.datetime(value.year, value.month, value.day, value.hour,
                                              value.minute, value.second, value.microsecond)
            else:
                if value.tzinfo is None:
                    value = datetime.datetime(value.year, value.month, value.day, value.hour,
                                              value.minute, value.second, value.microsecond,
                                              timezone)
                else:
                    value = value.astimezone(timezone)
        return value

    def sqlalchemy_type(self):
        return sqlalchemy.dialects.postgresql.TIMESTAMP(timezone=(not self._without_timezone),
                                                        precision=self._precision)


class LocalDateTime(DateTime):
    "Datetime stored as UTC in database but presented as local time by default."

    def _export(self, value, local=None, format=None):
        if local is None:
            local = True
        return super(LocalDateTime, self)._export(value, local=local, format=format)

    def _validate(self, obj, format=None, local=None):
        if local is None:
            local = True
        return super(LocalDateTime, self)._validate(obj, format=format, local=local)


class DateTimeRange(Range, DateTime):

    def sqlalchemy_type(self):
        import pytis.data.gensqlalchemy
        if self._utc:
            sql_class = pytis.data.gensqlalchemy.TSRANGE
        else:
            sql_class = pytis.data.gensqlalchemy.TSTZRANGE
        return sql_class()

    def base_type(self):
        return pytis.data.DateTime(utc=self._utc, without_timezone=self._without_timezone)


class ISODateTime(DateTime):
    """Datetime represented by the ISO datetime format in the database.
    """
    SQL_FORMAT = True


class Date(DateTime):
    """Date without a time.

    Constructor arguments:

      format -- specification of both input and output format of date
        and/or time in the form accepted by `time.strftime()'.
         May be also None in which case the configuration option
        'pytis.config.date_time_format' is used.  The class defines '*_FORMAT'
         constants which may be used as a value of this argument.

    """

    DEFAULT_FORMAT = '%Y-%m-%d'
    """Implicitní formát data."""
    SQL_FORMAT = DEFAULT_FORMAT
    """Formát data používaný standardně SQL stroji."""
    CZECH_FORMAT = '%d.%m.%Y'
    """Český \"účetnický\" formát data."""

    def _init(self, format=None, **kwargs):
        if format is None:
            format = pytis.config.date_format
        super(Date, self)._init(format=format, utc=False, **kwargs)

    def _validate(self, *args, **kwargs):
        value = super(Date, self)._validate(*args, **kwargs)
        return Value(value.type(), value.value().date())

    def _export(self, value, local=None, format=None):
        assert isinstance(value, datetime.date), value
        return self._strftime(value, format or self._format)

    @classmethod
    def _datetime(class_, tz):
        dt = super(Date, class_)._datetime(tz)
        return dt.date()

    def adjust_value(self, value):
        if value is not None and not isinstance(value, datetime.date):
            raise TypeError("Value not a date", value)
        return value

    def sqlalchemy_type(self):
        return sqlalchemy.Date()


class DateRange(Range, Date):

    def sqlalchemy_type(self):
        import pytis.data.gensqlalchemy
        return pytis.data.gensqlalchemy.DATERANGE()

    def base_type(self):
        return pytis.data.Date()

    def _increase_bound(self, value):
        if value is None:
            return None
        return value + datetime.timedelta(1)

    def _decrease_bound(self, value):
        if value is None:
            return None
        return value - datetime.timedelta(1)


class Time(_CommonDateTime):
    """Time of day without the date part.

    It is strongly recommended to always use UTC as the only Time timezone to
    prevent problems with daylight saving time conversions.  If you need to use
    time with local timezones, either use 'DateTime' or don't mix timezones in
    constructor, validation and exports.

    Constructor arguments:

      format -- specification of both input and output format of date
        and/or time in the form accepted by `time.strftime()'.
        May be also None in which case the configuration option
        'pytis.config.date_time_format' is used.  The class defines '*_FORMAT'
        constants which may be used as a value of this argument.

    """

    DEFAULT_FORMAT = '%H:%M:%S'
    """Implicitní formát času."""
    SQL_FORMAT = DEFAULT_FORMAT
    """Formát času používaný standardně SQL stroji."""
    SHORT_FORMAT = '%H:%M'
    """Formát času bez zobrazení sekund."""

    def _init(self, format=None, **kwargs):
        if format is None:
            format = pytis.config.time_format
        super(Time, self)._init(format=format, **kwargs)

    def _validate(self, *args, **kwargs):
        value = super(Time, self)._validate(*args, **kwargs)
        dt = value.value()
        if self._without_timezone:
            v = dt.time()
        else:
            v = dt.timetz()
        return Value(value.type(), v)

    def _export(self, value, **kwargs):
        assert isinstance(value, datetime.time), value
        if __debug__:
            local = kwargs.get('local')
            if local is not None:
                if local and self._utc or not local and not self._utc:
                    raise Exception("Can't mix UTC and local time zones in Time type")
        if self._utc:
            kwargs['local'] = False
        else:
            kwargs['local'] = True
        return super(Time, self)._export(value, **kwargs)

    @classmethod
    def _datetime(class_, tz):
        dt = datetime.datetime.now(tz)
        if tz is None:
            result = dt.time()
        else:
            result = dt.astimezone(tz).timetz()
        return result

    def adjust_value(self, value):
        if value is not None and not isinstance(value, (datetime.datetime, datetime.time)):
            raise TypeError("Value not a time", value)
        return value

    def sqlalchemy_type(self):
        return sqlalchemy.Time(timezone=(not self._without_timezone), precision=self._precision)


class LocalTime(Time):
    "Time stored as UTC in database but presented as local time by default."

    def _export(self, value, local=None, format=None):
        if local is None:
            local = True
        return super(LocalTime, self)._export(value, local=local, format=format)

    def _validate(self, obj, format=None, local=None):
        if local is None:
            local = True
        return super(LocalTime, self)._validate(obj, format=format, local=local)


class TimeInterval(Type):
    """Amount of time between two moments.

    Constructor arguments:

      format -- specification of both input and output format of the time
        interval; only a limited set of specification constructs is
        supported

    """

    _MATCHER = re.compile('((?P<days>[0-9]+) days?,? )?'
                          '(?P<hours>[0-9]+):(?P<minutes>[0-9]+):(?P<seconds>[0-9]+)$')

    DEFAULT_FORMAT = '%H:%M:%S'
    SHORT_FORMAT = '%H:%M'
    SQL_FORMAT = True
    NATURAL_FORMAT = 'NATURAL_FORMAT'

    _SPECIAL_VALUES = Type._SPECIAL_VALUES + ((None, ''),)

    def _init(self, format=None, **kwargs):
        super(TimeInterval, self)._init(**kwargs)
        self._format = format
        if format is None:
            self._matcher = self._MATCHER
        else:
            self._matcher = self._make_matcher(format)

    def _comparison_key(self):
        return super(TimeInterval, self)._comparison_key() + (self._format,)

    def _make_matcher(self, format):
        re_hours = '(?P<hours>[0-9]+)'
        re_minutes = '(?P<minutes>[0-9]+)'
        re_seconds = '(?P<seconds>[0-9]+)'
        matcher_string = (format.replace('%H', re_hours).
                          replace('%M', re_minutes).replace('%S', re_seconds))
        return re.compile(matcher_string)

    def _validate(self, obj, format=None, **kwargs):
        assert isinstance(obj, basestring)
        if format is None:
            matcher = self._matcher
        elif format is True:
            matcher = self._MATCHER
        else:
            matcher = self._make_matcher(format)
        match = matcher.match(obj)
        if not match:
            raise ValidationError(_(u"Invalid format"))
        groups = match.groupdict()
        days = int(groups.get('days') or '0')
        seconds = (int(groups.get('hours') or '0') * 3600 +
                   int(groups.get('minutes') or '0') * 60 +
                   int(groups.get('seconds') or '0'))
        days += seconds // 86400
        seconds = seconds % 86400
        interval = datetime.timedelta(days, seconds)
        return Value(self, interval)

    def _export(self, value, format=None, **kwargs):
        assert isinstance(value, datetime.timedelta), value
        seconds = value.total_seconds()
        if format == self.NATURAL_FORMAT:
            def minutes(value):
                return _.ngettext("%d min", "%d min", int(value.seconds) % 3600 // 60)
            def hours(value):
                return _.ngettext("%d h", "%d h", int(value.seconds) // 3600)
            def days(value):
                return _.ngettext("%d day", "%d days", value.days)
            if seconds < 60:
                result = _("less than 1 min")
            elif seconds < 3600:
                result = minutes(value)
            elif value.days == 0:
                result = hours(value) + ', ' + minutes(value)
            elif value.days < 7:
                result = days(value) + ', ' + hours(value)
            else:
                result = days(value)
        else:
            if seconds < 0:
                seconds = -seconds
                sign = '-'
            else:
                sign = ''
            if format is None:
                format = self._format
                if format is None:
                    format = self.DEFAULT_FORMAT
            format_string = sign + (format.replace('%H', '%(hours)d').
                                    replace('%M', '%(minutes)02d').replace('%S', '%(seconds)02d'))
            result = format_string % dict(hours=(seconds // 3600),
                                          minutes=((seconds % 3600) // 60),
                                          seconds=(seconds % 60))
        return result

    def adjust_value(self, value):
        if value is not None and not isinstance(value, datetime.timedelta):
            raise TypeError("Value not a timedelta", value)
        return value

    def sqlalchemy_type(self):
        return sqlalchemy.dialects.postgresql.INTERVAL()


def date_and_time(date, time):
    """Combine given 'date' and 'time' 'Value's into a 'datetime.datetime' return value.

    Arguments:

      date -- 'Value' instance of type 'Date' containing the date value
      time -- 'Value' instance of type 'Time' containing the time value to
        add to the date value

    """
    assert isinstance(date, Value) and isinstance(date.type(), Date), date
    assert isinstance(time, Value) and isinstance(time.type(), Time), time
    date_value = date.value()
    time_value = time.value()
    if time_value.tzinfo is None:
        time_value = time_value.replace(tzinfo=time.type().timezone())
    value = datetime.datetime.combine(date_value, time_value)
    return value


def add_timedelta(value, timedelta):
    """Return information about 'DateTime' 'value' with 'timedelta' added.

    Arguments:

      value -- 'Value' instance of type 'DateTime'
      timedelta -- 'datetime.timedelta' instance

    This utility function takes the 'value', adds 'timedelta' to its Python
    value and returns the sequence (VALUE, DATETIME, DIFF_DATES) where:
    - VALUE is a new 'Value' instance created from the resulting datetime
    - DATETIME is the corresponding Python value
    - DIFF_DATES is the difference in days between the original and new
      dates (without considering times); it is positive iff the new date is
      higher than the old date

    """
    assert isinstance(value, Value) and isinstance(value.type(), DateTime), value
    assert isinstance(timedelta, datetime.timedelta), timedelta
    orig_value = value.value()
    dt = orig_value + timedelta
    value = Value(value.type(), dt)
    diff_dates = (dt.date() - orig_value.date()).days
    return value, dt, diff_dates


class Boolean(Type):
    """Jednoduchý výčtový typ implementující hodnoty \"pravda\" a \"nepravda\".

    Za pravdu je považován string 'T', za nepravdu string 'F'; tyto stringy
    jsou uživatelskými hodnotami výčtu.  Odpovídající vnitřní hodnoty jsou
    blíže nespecifikované pythonové objekty s pythonovu sémantikou pravdy a
    nepravdy.

    Validační argument 'extended' umožňuje liberálnější kontrolu vstupu.  Je-li pravdivý, jsou
    kromě \"oficiálních\" hodnot 'obj' zvalidovány i následující stringové hodnoty:

    \'t\', \'1\' -- jako reprezentace pravdivé hodnoty
    \'f\', \'0\' -- jako reprezentace nepravdivé hodnoty


    """

    _SPECIAL_VALUES = ((True, 'T'), (False, 'F'), (None, ''))

    def _init(self, not_null=True):
        e = FixedEnumerator((True, False))
        super(Boolean, self)._init(enumerator=e, not_null=not_null)

    def _validate(self, obj, extended=False):
        if extended:
            if obj in ('t', '1'):
                return Value(self, True)
            elif obj in ('f', '0'):
                return Value(self, False)
        # Valid values are found in _SPECIAL_VALUES before _validate is called.
        raise ValidationError(_(u"Invalid boolean type input value"))

    def default_value(self):
        return Value(self, False)

    def secret_export(self):
        return ''

    def adjust_value(self, value):
        if value is None:
            return None
        return bool(value)

    def sqlalchemy_type(self):
        return sqlalchemy.Boolean()


class Uuid(Type):
    """Uuid type.

    Uuid type is represented by python's immutable UUID object (UUID class).

    Constructor arguments:

      version -- optional argument; if given, the resulting UUID will have
        its variant and version number set according to RFC 4122, overriding bits
        in the given hex, bytes, bytes_le, fields, or int.

    """

    _SPECIAL_VALUES = Type._SPECIAL_VALUES + ((None, ''),)

    def _init(self, version=None, **kwargs):
        self._version = version
        super(Uuid, self)._init(**kwargs)

    def _validate(self, obj):
        if obj is None:
            value = None
        else:
            try:
                value = uuid.UUID(obj, version=self._version)
            except ValueError as e:
                raise ValidationError(*e.args)
        return Value(self, value)

    def _export(self, value):
        return unistr(value)

    def sqlalchemy_type(self):
        return sqlalchemy.dialects.postgresql.UUID(as_uuid=True)


class Binary(Limited):
    """Binary data.

    Binary values are represented by instances of Python 'bytes' type or 'None'
    (representing a null value).  Internal representation is an instance of the
    'Binary.Data' class (or <derived-class>.Data for derived classes).  This
    class adds optional possibility to keep file name and MIME type information
    together with the value instance.  Derived classes may extend the internal
    value representation to add more specific features, such as obtaining image
    format or pixel size, etc.

    Usage of binary data is limited only to certain situations.  They may be
    used only in non-key columns, they can be retrieved from a database (but
    they may not be used in search conditions with the exception of testing for
    NULL value) and they can be used as non-key values in insertions and
    updates.

    Values of this type are not cached as they may be large and their
    validation is trivial.

    """

    _VALIDATION_CACHE_LIMIT = 0

    class Data(bytes):
        """Internal representation of binary values.

        The primary purpose of this class is to provide verification od binary
        data depending on their content.  This class accepts any data, but
        subclasses may exist, which only accept certain binary formats, such as
        images, documents, audio files etc.

        """

        def __new__(cls, data, **kwargs):
            if ((sys.version_info[0] == 2 and isinstance(data, (str, bytes, buffer)) or
                 sys.version_info[0] > 2 and isinstance(data, (bytes, memoryview)))):
                pass
            elif hasattr(data, 'read') and callable(data.read):
                # TODO: Make sure the file was opened in binary mode?  Make a conversion if not?
                data = data.read()
            else:
                raise TypeError("Invalid type for binary data:", type(data))
            return bytes.__new__(cls, data)

        def __init__(self, data, filename=None, mime_type=None):
            """Initialize a new value instance.

            Arguments:

              data -- The binary data.  It can be a Python 'bytes', 'str' or
                 'buffer' object or an open stream (a file-like object).  A
                 'bytes' object is used directly, file-like object is read (the
                 caller is responsible for closing it) and the other types are
                 converted to 'bytes'.
              filename -- Filename as a string or None.  This name does not
                include directory and does not refer to any actual file (has
                nothing to do with the input file for reading the data).  It
                may be used to suggest what the content of the data is.  It is
                optional and its usage may be application specific.
              mime_type -- Data MIME type as a string or None.  It is optional
                and its usage may be application specific.

            Raises 'TypeError' if 'data' is a value of unsupported type.

            Raises 'IOError' if 'data' is an open stream which can not be read.

            Raises 'ValueError' if the data content does not conform to the
            expected binary format (depending on the actual 'Binary.Data'
            subclass).

            """
            assert filename is None or isinstance(filename, basestring), filename
            assert mime_type is None or isinstance(mime_type, basestring), mime_type
            self._filename = filename
            self._mime_type = mime_type

        def __repr__(self):
            if len(self) > 100:
                # Prevent printing huge binary data in logs, tracebacks etc.
                return '<binary value at 0x%x, %d bytes>' % (id(self), len(self))
            else:
                return super(Binary.Data, self).__repr__()

        def buffer(self):
            """Deprecated! The instance itself is a bytes instance now."""
            return buffer(self)

        def filename(self):
            """Return the suggested filename as passed to the constructor."""
            return self._filename

        def mime_type(self):
            """Return the suggested MIME type as passed to the constructor."""
            return self._mime_type

    # Temporary backwards compatibility alias.
    Buffer = Data

    def _init(self, enumerator=None, **kwargs):
        assert enumerator is None, ("Enumerators can not be used with binary data types")
        super(Binary, self)._init(**kwargs)

    def _validate(self, obj, filename=None, mime_type=None, **kwargs):
        try:
            value = self.Data(obj, filename=filename, mime_type=mime_type)
        except ValueError as e:
            raise ValidationError(*e.args)
        return Value(self, value)

    def _export(self, value):
        return value

    def _format_length(self, length):
        return format_byte_size(length)

    def adjust_value(self, value):
        if value is not None and not isinstance(value, self.Data):
            value = self.Data(value)
        return value

    def sqlalchemy_type(self):
        return sqlalchemy.dialects.postgresql.BYTEA()


class Image(Binary, Big):
    """Binary type for generic bitmap images.

    The binary data of this type are represented by an 'Image.Data' instance.

    'Image.Data' makes sure that the binary data represent an image in one of
    the input formats supported by the Python Imaging Library.  It also
    provides the `image()' method, which returns the 'PIL.Image' instance
    corresponding to the image contained within the data.

    Image type can be further restricted to a list of allowed formats.  You may
    also restrict minimal/maximal pixel size of the image.

    Supported input formats include the most widely used formats such as 'PNG',
    'JPEG', 'TIFF', 'GIF', 'BMP', 'PCX', 'PPM', 'XBM' or 'IM'.  See Python
    Imaging Library documentation for the full list.

    The Python Imaging Library (PIL) must be installed when using this type.

    Constructor arguments:

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
    class Data(Binary.Data):
        """A bufer for internal representation of bitmap image data.

        See the documentation of the 'Image' type for more information.

        """

        def __init__(self, *args, **kwargs):
            super(Image.Data, self).__init__(*args, **kwargs)
            import PIL.Image
            # The stream must stay open for the whole life of the Image object.
            f = io.BytesIO(self)
            try:
                image = PIL.Image.open(f)
            except IOError:
                raise ValueError(_(u"Invalid graphics format"))
            self._image = image

        def image(self):
            """Return the image as a 'PIL.Image' instance."""
            return self._image

    Buffer = Data

    def _init(self, minsize=(None, None), maxsize=(None, None),
              formats=None, **kwargs):
        if __debug__:
            for size in minsize, maxsize:
                assert (isinstance(size, (tuple, list)) and len(size) == 2 and
                        size[0] is None or isinstance(size[0], int) and
                        size[1] is None or isinstance(size[1], int)), size
            if formats is not None:
                assert isinstance(formats, (tuple, list)), formats
                for f in formats:
                    assert isinstance(f, basestring)
                    if f not in ('PNG', 'JPEG', 'TIFF', 'GIF', 'BMP',
                                 'PCX', 'PPM', 'XBM', 'IM'):
                        log(OPERATIONAL, "Suspicious image format:", f)
        self._minsize = tuple(minsize)
        self._maxsize = tuple(maxsize)
        self._formats = formats and tuple(formats)
        super(Image, self)._init(**kwargs)

    def _comparison_key(self):
        return super(Image, self)._comparison_key() + (self._minsize, self._maxsize, self._formats)

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
            for min, max, size in zip(self._minsize, self._maxsize, image.size):
                if min is not None and size < min:
                    raise ValidationError(_("Minimal pixel size %(minsize)s exceeded",
                                            minsize=('%sx%s' % self._minsize)))
                if max is not None and size > max:
                    raise ValidationError(_("Maximal pixel size %(maxsize)s exceeded",
                                            maxsize=('%sx%s' % self._maxsize)))
            if self._formats is not None and image.format not in self._formats:
                raise ValidationError(_("Unsupported format %(format)s; valid formats: %(formats)s",
                                        format=image.format,
                                        formats=', '.join(self._formats)))


class LTree(Type):
    """Type representing a hierarchical (tree) structure.

    It is similar to 'String', but there are some differences:

    - No length limits can be set in LTree.

    - Dots in LTree strings serve as item separators.  So LTree values are
      handled as lists of items in some situations, especially in sorting.

    - The items between dots may not be empty, may contain only alphanumeric
      characters and may be at most 255 characters long (each of them).

    Constructor arguments:

      text -- if true, handle the type values as text values, otherwise handle
        them as true ltree values.  This makes difference in sorting -- when
        true, sort textually, when false, sort as ltree.  This may be
        particularly useful when ltree values contain national characters,
        which are not sorted correctly by PostgreSQL ltree sorting.  Anyway,
        when true, the values must be padded to a fixed width of individual
        ltree components (such as '00010.00008.00124' instead of '10.8.124') in
        order to get the expected results with textual sorting.

    """
    _REGEX = re.compile(r'^\w+$', re.UNICODE)
    _SPECIAL_VALUES = Type._SPECIAL_VALUES + ((None, ''),)

    def _init(self, text=False, **kwargs):
        super(LTree, self)._init(**kwargs)
        self._text = text

    def _comparison_key(self):
        return super(LTree, self)._comparison_key() + (self._text,)

    def text(self):
        """Return value of 'text' constructor argument."""
        return self._text

    def _validate(self, obj):
        assert isinstance(obj, basestring), ('Not a string', obj)
        items = obj.split('.')
        for item in items:
            if not item:
                raise ValidationError(_(u"Invalid hierarchical value format"))
            elif len(item) > 255:
                raise ValidationError(_(u"One of hierarchical value items is too long"))
            elif self._REGEX.match(item) is None:
                raise ValidationError(_(u"One of hierarchical value items contains invalid "
                                        u"characters"))
        return Value(self, unistr(obj))

    def _export(self, value):
        assert isinstance(value, basestring), ('Value not a string', value)
        return value

    def wm_validate(self, obj):
        assert isinstance(obj, basestring)
        return WMValue(self, obj), None

    def adjust_value(self, value):
        if value is not None and not isinstance(value, basestring):
            raise TypeError("Value not a string", value)
        return value

    def sqlalchemy_type(self):
        import pytis.data.gensqlalchemy
        return pytis.data.gensqlalchemy.LTreeType()


class Array(Limited):
    """Sequence of values of some other type.

    The 'inner_type' constructor argument (mandatory) determines the pytis type
    of array items.  Validation expects a sequence of strings (python tuple,
    array or any iterable object containing strings) for validation.  These
    strings are validated against the inner type.  The internal value is a
    tuple of 'Value' instances of the inner type.  Export returns a tuple of
    strings corresponding to the exported array items.

    """
    _SPECIAL_VALUES = Limited._SPECIAL_VALUES + ((None, ''),)

    def _init(self, inner_type, **kwargs):
        assert isinstance(inner_type, Type)
        self._inner_type = inner_type
        super(Array, self)._init(**kwargs)

    def _comparison_key(self):
        return super(Array, self)._comparison_key() + (self._inner_type,)

    def validate(self, obj, strict=True, transaction=None, condition=None, arguments=None,
                 **kwargs):
        # We override `validate()' instead of `_validate()' here, which is
        # discouraged in `validate()' method docstring.  The reason is that we
        # need to pass arguments `transaction', `condition' and `arguments' to
        # the validation of inner values.  We also don't want to cache
        # validation results as it should be enough to cache the inner values.
        values = []
        for item in obj:
            value, error = self._inner_type.validate(item, strict=strict, transaction=transaction,
                                                     condition=condition, arguments=arguments,
                                                     **kwargs)
            if error:
                return value, error
            else:
                values.append(value)
        values = tuple(values)
        if strict:
            try:
                self._check_constraints(values, transaction=transaction,
                                        condition=condition, arguments=arguments)
            except ValidationError as e:
                return None, e
        return Value(self, values), None

    def _export(self, value):
        return tuple([val.export() for val in value])

    def inner_type(self):
        return self._inner_type

    def adjust_value(self, value):
        if value is None:
            value = ()
        else:
            if not isinstance(value, (tuple, list)):
                raise TypeError("Value not a sequence", value)
            if value and not isinstance(value[0], Value):
                inner_type = self.inner_type()
                value = tuple(Value(inner_type, v) for v in value)
            elif isinstance(value, list):
                value = tuple(value)
        return value

    def sqlalchemy_type(self):
        return sqlalchemy.dialects.postgresql.ARRAY(self.inner_type().sqlalchemy_type())


class JSON(Type):

    _SPECIAL_VALUES = Type._SPECIAL_VALUES + ((None, ''),)

    class _hashabledict(dict):
        def __key(self):
            def val(v):
                if isinstance(v, dict):
                    return self.__class__(v)
                elif isinstance(v, list):
                    return tuple(v)
                else:
                    return v
            return tuple((k, val(v)) for k, v in sorted(self.items()))
        def __hash__(self):
            return hash(self.__key())
        def __eq__(self, other):
            return self.__key() == other.__key()

    class JSONValue:
        """Wrapper type allowing recognition of JSON values in DB interface."""
        pass

    class JSONArray(list, JSONValue):
        pass

    class JSONObject(dict, JSONValue):
        pass

    def _make(class_, *args, **kwargs):
        if kwargs.get('schema') is not None:
            # Type arguments must be hashable.
            kwargs['schema'] = class_._hashabledict(kwargs['schema'])
        return Type._make(class_, *args, **kwargs)
    _make = staticmethod(_make)

    def _init(self, schema=None, **kwargs):
        super(JSON, self)._init(**kwargs)
        if schema:
            import jsonschema
            jsonschema.Validator.check_schema(schema)
            validator = jsonschema.validators.validator_for(schema)(schema)
        else:
            validator = None
        self._validator = validator

    def _adjust_value(self, x, allow_primitive_value=True):
        if allow_primitive_value and isinstance(x, (int, long, float, decimal.Decimal,
                                                    bool, basestring, type(None))):
            return x
        elif isinstance(x, (tuple, list)):
            return [self._adjust_value(v) for v in x]
        elif isinstance(x, dict) and all(isinstance(k, basestring) for k in x.keys()):
            return dict((k, self._adjust_value(v)) for k, v in x.items())
        else:
            raise TypeError("Unsupported JSON value", x)

    def adjust_value(self, value):
        if value is None:
            return None
        else:
            wrapper = self.JSONObject if isinstance(value, dict) else self.JSONArray
            return wrapper(self._adjust_value(value, allow_primitive_value=False))

    def sqlalchemy_type(self):
        import pytis.data.gensqlalchemy
        return pytis.data.gensqlalchemy.JSON()

    def _validate(self, obj):
        assert isinstance(obj, basestring), obj
        if sys.version_info[0] == 2:
            errcls = ValueError
        else:
            errcls = json.JSONDecodeError  # not defined in Python 2
        try:
            value = json.loads(obj)
        except errcls:
            # Translators: User input validation error message.
            raise ValidationError(_(u"Not a JSON string"))
        try:
            self._adjust_value(value, allow_primitive_value=False)
        except TypeError:
            raise ValidationError(_(u"Unsupported JSON value"))
        if self._validator:
            import jsonschema
            try:
                self._validator.validate(value)
            except jsonschema.ValidationError:
                raise ValidationError(_(u"JSON value doesn't match schema definition"))
        return Value(self, value)

    def _export(self, value):
        return json.dumps(value)


class JSONB(JSON):

    def sqlalchemy_type(self):
        import pytis.data.gensqlalchemy
        return pytis.data.gensqlalchemy.JSONB()


# Pomocné třídy

@python_2_unicode_compatible
class Enumerator(object):
    """Generic interface for enumerations of data type values for integrity constraints.

    The enumerator provides an interface for validation if a given value
    belongs to the set of valid values (the 'check()' method) and also a means
    to retrieve all valid enumeration values (the 'values()' method).  The
    means of retrieving the values and performing the check is the subject of
    implementation of particular derived classes.

    This class defines the mandatory enumerator API.  Derived classes may also
    offer extended interface for specific services.  An instance of an
    enumerator class (a derived class implementing the mandatory API) may be
    used as the 'enumerator' argument of a data 'Type' constructor.

    Generally, enumerators must be thread-safe as thy can be used in shared
    'Type' instances.  Any enumerator method which is not thread-safe must be
    clearly marked as such and it may not be used with enumerator instances
    used in types.

    """

    def check(self, value, **kwargs):
        """Return true, iff 'value' belongs to the enumeration.

        Arguments:

          value -- internal Python value of the corresponding data type, for
            which the enumerator is used.

          **kwargs -- keyword arguments as accepted by 'values()'.

        The default implementation simply retrieves all values and checks the
        presence of 'value' in the returned sequence.  This may be suboptimal
        for potentionaly large sets so it may be overriden in derived classes
        using a more sophisticated method.

        """
        return value in self.values(**kwargs)

    def __str__(self):
        return '<%s.%s>' % (self.__class__.__module__, self.__class__.__name__)

    def __repr__(self):
        return str(self)

    def values(self, **kwargs):
        """Return a sequence of all valid enumeration values."""
        raise ProgramError('Not implemented', 'Enumerator.values()')


class TransactionalEnumerator(object):
    """Mix-in class for enumerators which need a database transaction.

    The methods 'values()' and 'check()' of such enumerators accept one
    mandatory keyword argument 'transaction' representing the current database
    transaction.

    """


@python_2_unicode_compatible
class FixedEnumerator(Enumerator):
    """Enumerator with a fixed enumeration passed to the constructor.

    The method 'values()' (and thus 'check()' as well) accepts no keyword
    arguments.

    """

    def __init__(self, enumeration):
        """Initialize the instance.

        Arguments:

          enumeration -- a sequence of values compatible with internal (Python)
            values of the type, for which the enumerator is used.

        """
        super(FixedEnumerator, self).__init__()
        self._enumeration = tuple(enumeration)

    def __str__(self):
        return '<%s %r>' % (self.__class__.__name__, self._enumeration)

    def values(self):
        return self._enumeration


@python_2_unicode_compatible
class DataEnumerator(Enumerator, TransactionalEnumerator):
    """Enumerator retrieving the enumeration values from a data object.

    The enumerator uses one column of the data object to get the set of
    enumeration values.  This is typically the key column (by default), but it
    is possible to choose any other column by passing proper constructor
    arguments.

    One special thing about this enumerator is that it is internally handled
    specifically throughout Pytis.  It's instances are created automatically
    when a specification name is passed to a 'codebook' or 'enumerator'
    attribute of 'Field' constructor and its extended API is also used where
    appropriate.

    """

    def __init__(self, data_factory, value_column=None, validity_column=None,
                 validity_condition=None, connection_data=None):
        """Initialize the instance.

        Arguments:

          data_factory -- a 'DataFactory' instance for data object creation, or
            a string naming the specification of the data factory
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
          connection_data -- 'DBConnection' instance providing the database
            connection parameters; if 'None' then connection parameters are
            retrieved from the configuration

        """
        super(DataEnumerator, self).__init__()
        from pytis.data import DataFactory
        assert isinstance(data_factory, (DataFactory, basestring)), data_factory
        assert (value_column is None or
                isinstance(value_column, basestring))
        assert (validity_column is None or
                isinstance(validity_column, basestring))
        assert (validity_condition is None or
                isinstance(validity_condition, pytis.data.Operator) and
                validity_column is None)
        self._data_factory = data_factory
        self._data_lock = _thread.allocate_lock()
        self._value_column_ = value_column
        self._validity_column = validity_column
        if validity_column is not None:
            from pytis.data import EQ
            validity_condition = EQ(validity_column, Value(Boolean(), True))
        self._validity_condition = validity_condition
        self._change_callbacks = []
        self._connection_data = connection_data
        self._completed = False

    def __str__(self):
        factory = self._data_factory
        if ((isinstance(factory, pytis.data.DataFactory) and
             isinstance(factory._args[0], (tuple, list)) and
             isinstance(factory._args[0][0], pytis.data.DBColumnBinding))):
            factory = factory._args[0][0].table()
        # Avoid calling _complete() just because of stringification, such as in logging.
        if self._completed:
            value_column = self._value_column
        else:
            value_column = self._value_column_
        return '<%s %s %s>' % (self.__class__.__name__, factory, value_column)

    def __getattr__(self, name):
        if name in ('_data', '_value_column'):
            self._complete()
            return self.__dict__[name]
        else:
            raise AttributeError(name)

    def _complete(self):
        # Finish the instance by data object creation.
        if self._connection_data is None:
            connection_data = pytis.config.dbconnection
        else:
            connection_data = self._connection_data
        if isinstance(self._data_factory, basestring):
            self._data_factory = pytis.config.resolver.get(self._data_factory, 'data_spec')
        self._data = data = self._data_factory.create(connection_data=connection_data)
        if self._value_column_ is None:
            self._value_column = data.key()[0].id()
        else:
            self._value_column = self._value_column_
        column = data.find_column(self._value_column)
        assert column, ('Non-existent value column', self._value_column)
        self._value_column_type = column.type()
        for callback in self._change_callbacks:
            self._data.add_callback_on_change(callback)
        self._change_callbacks = []
        self._non_big_columns = [c.id() for c in data.columns()
                                 if not isinstance(c.type(), pytis.data.Big)]
        if __debug__:
            if self._validity_column is not None:
                c = data.find_column(self._validity_column)
                assert c, ('Non-existent validity column', self._validity_column)
                assert isinstance(c.type(), Boolean), ('Invalid validity column type', c)
        self._completed = True

    def _condition(self, condition=None):
        if self._validity_condition is not None:
            if condition is not None:
                from pytis.data import AND
                return AND(condition, self._validity_condition)
            else:
                return self._validity_condition
        else:
            return condition

    def _retrieve(self, value, transaction=None, condition=None, arguments=None):
        from pytis.data import AND, EQ
        if arguments is None:
            arguments = {}
        the_condition = EQ(self._value_column, Value(self._value_column_type, value))
        validity_condition = self._condition(condition=condition)
        if validity_condition is not None:
            the_condition = AND(the_condition, validity_condition)
        with Locked(self._data_lock):
            data = self._data
            try:
                count = data.select(the_condition, transaction=transaction, arguments=arguments,
                                    columns=self._non_big_columns)
                if count > 1:
                    raise ProgramError('Insufficient runtime filter for DataEnumerator',
                                       str(the_condition))
                row = data.fetchone()
            finally:
                try:
                    data.close()
                except Exception:
                    pass
            return row

    # Enumerator interface

    def check(self, value, transaction=None, condition=None, arguments=None):
        row = self._retrieve(value, transaction, condition, arguments=arguments)
        if row is None:
            result = False
        else:
            result = True
        return result

    def values(self, transaction=None, condition=None, arguments=None, sort=(), max=None):
        if arguments is None:
            arguments = {}
        the_condition = self._condition(condition=condition)
        with Locked(self._data_lock):
            result = []
            try:
                count = self._data.select(condition=the_condition, transaction=transaction,
                                          sort=sort, arguments=arguments,
                                          columns=(self._value_column,))
                if max is not None and count > max:
                    return None
                while True:
                    row = self._data.fetchone()
                    if row is None:
                        break
                    result.append(row[self._value_column].value())
            finally:
                try:
                    self._data.close()
                except Exception:
                    pass
            return tuple(result)

    # Extended interface.

    def add_callback_on_change(self, callback):
        # We don't want this co cause data object creation.  See also __getattr__().
        if '_data' in self.__dict__:
            self._data.add_callback_on_change(callback)
        else:
            self._change_callbacks.append(callback)

    def data_factory(self):
        """Vrať specifikaci datového objektu enumerátoru jako instanci 'pytis.data.DataFactory'."""
        return self._data_factory

    def value_column(self):
        """Vrať název sloupce datového objektu, který nese vnitřní hodnotu."""
        return self._value_column

    def validity_condition(self):
        """Return static condition determining validity of data rows."""
        return self._validity_condition

    def row(self, value, transaction=None, condition=None, arguments=None):
        """Return a *data* row corresponding to given codebook value.

        Arguments:

          value -- internal (Python) value of the enumerator's 'value_column'.  The row
            corresponding to this value is returned.
          transaction -- transaction for data operations.
          condition -- runtime filter condition for enumerator validation.
          arguments -- dictionary of table function call arguments, with
            function argument identifiers as keys and 'pytis.data.Value'
            instances as values.  Useful only when the table is actually a row
            returning database function, otherwise ignored.

        Returns a 'pytis.data.Row' instance from the underlying data object.

        """
        return self._retrieve(value, transaction=transaction, condition=condition,
                              arguments=arguments)

    def rows(self, transaction=None, condition=None, arguments=None, sort=()):
        """Return sequence of rows of the underlying data object.

        Arguments:

          transaction -- transaction for data operations.
          condition -- runtime filter condition as an 'Operator' instance or None.
          sort -- sorting specification as accepted by 'pytis.data.Data.select()'.
          arguments -- dictionary of table function call arguments, with
            function argument identifiers as keys and 'pytis.data.Value'
            instances as values.  Useful only when the table is actually a row
            returning database function, otherwise ignored.

        """
        if arguments is None:
            arguments = {}
        the_condition = self._condition(condition=condition)
        with Locked(self._data_lock):
            return self._data.select_map(identity, transaction=transaction,
                                         condition=the_condition, arguments=arguments,
                                         columns=self._non_big_columns,
                                         sort=sort)

    def type(self, column):
        """Vrať datový typ daného sloupce v datovém objektu enumerátoru."""
        return self._data.find_column(column).type()

    def permitted(self, column=None):
        """Return whether VIEW access to given enumerator column is permitted.

        If column is None, key column is used which should give information
        whether the enumerator itself is permitted.

        """
        if column is None:
            column = self._data.key()[0].id()
        if isinstance(self._data, pytis.data.RestrictedData):
            return self._data.permitted(column, pytis.data.Permission.VIEW)
        else:
            return True


@python_2_unicode_compatible
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


@python_2_unicode_compatible
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
        t = state['_type']
        if not isinstance(t, Type):
            raise InvalidAccessError('Invalid argument type', '_Value.type', t)
        self._type = t
        self._value = state['_value']

    def __str__(self):
        # Note: %r is important here in order to get (possibly long) binary values represented
        # reasonably (see Binary.Data.__repr__()).
        return '<%s type=%s, value=%r>' % (self.__class__.__name__, self.type(), self.value())

    def __eq__(self, other):
        if not sameclass(self, other):
            return NotImplemented
        elif self._type != other._type:
            return False
        else:
            # Beware, datetime instances can't be compared with None
            v1, v2 = self._value, other._value
            if v1 is None and v2 is None:
                return True
            elif v1 is None or v2 is None:
                return False
            else:
                return v1 == v2

    def __ne__(self, other):
        # Implied automatically in Python 3 so can be removed when dropping Python 2 support.
        return not self == other

    def __hash__(self):
        return hash(self._type) ^ hash(self._value)

    def type(self):
        """Vrať typ hodnoty jako instanci třídy 'Type' zadanou v '__init__()'.
        """
        return self._type

    def value(self):
        """Return value given in '__init__()'.

        In some derived classes the returned value may be modified, e.g. when
        it is necessary to hide the value.  If you need the unchanged value
        (and you are sure this is really what you want), use 'true_value()'.

        """
        return self._value

    def true_value(self):
        """Return true value of the instance.

        In some derived classes, the value returned by 'value()' may be
        transformed, e.g. when the true value should be hidden.  This method
        always returns the actual unchanged value.

        """
        return self._value


class Value(_Value):
    """Reprezentace hodnoty daného typu.

    Každá hodnota se skládá z typu (instance třídy 'Type') a hodnoty samotné.
    Hodnota samotná může být cokoliv, bez ohledu na uvedený typ.

    Pro zápis hodnoty do uživatelského rozhraní nebo databáze lze využít metodu
    'export()'.

    """
    _VOID = object()

    def __init__(self, type_, value):
        """Inicializuj hodnotu daného typu.

        Argumenty:

          type_ -- instance třídy 'Type'
          value -- hodnota samotná, libovolný objekt

        """
        # print '---', type_, type(value), repr(value)
        value = type_.adjust_value(value)
        # print '-->', type(value), repr(value)
        _Value.__init__(self, type_, value)
        self._init()

    def __setstate__(self, state):
        super(Value, self).__setstate__(state)
        self._init()

    def _init(self):
        if self._type.__class__ == String:    # pozor, nebezpečná věc!
            e = self._value or ''
            self._exported = e if isinstance(e, unistr) else unistr(e)
        else:
            self._exported = self._VOID

    def __hash__(self):
        return hash(self._type) ^ hash(self._value)

    def export(self, *args, **kwargs):
        """Vrať stringovou reprezentaci hodnoty schopnou validace.

        Tato metoda je pouze zkratkou pro volání
        'self.type().export(self.value())'.  Pokud jsou metodě předány
        argumenty, je metoda `Type.export()' volána i s těmito argumenty.

        """
        # Abychom to zbytečně nekomplikovali, tak cachujeme pouze exporty bez
        # argumentů.
        if args or kwargs:
            exported = self.type().export(self.value(), *args, **kwargs)
        else:
            exported = self._exported
            if exported is self._VOID:
                exported = self.type().export(self.value())
                self._exported = exported
        return exported

    def retype(self, type):
        """Return instance of the same class and value, but of diferent type.

        Arguments:

          type -- 'Type' instance to be used as the new value instance type

        It is responsibility of the caller to ensure that the new type is
        compatible with the type of the value.

        """
        assert isinstance(type, Type)
        return self.__class__(type, self._value)

    @classmethod
    def reconceal(class_, value):
        """Return 'class_' instance corresponding to 'value'.

        Arguments:

          value -- 'Value' instance to use as the base of the 'class_'
            instance

        """
        assert isinstance(value, Value)
        if value.__class__ == class_:
            instance = value
        else:
            instance = class_(type=value.type(), value=value.true_value())
        return instance


class WMValue(_Value):
    """Reprezentace specifikace pro wildcard match daného typu."""


# Shorthand functions for values


def sval(value, maxlen=None):
    """Return 'Value' instance of type 'String' with given value.

    Arguments:

      value -- internal value of the 'Value' instance, basestring

    """
    assert value is None or isinstance(value, basestring), value
    return Value(String(maxlen=maxlen), value)


def ival(value):
    """Return 'Value' instance of type 'Value' with given value.

    Arguments:

      value -- internal value of the 'Value' instance, integer

    """
    assert value is None or isinstance(value, int), value
    return Value(Integer(), value)


def fval(value, digits=None, precision=None):
    """Return 'Value' instance of type 'Float' with given value.

    Arguments:

      value -- internal value of the 'Value' instance, float
      digits -- maximum number of digits to pass to 'Float' constructor
      precision -- number of digits after decimal point to pass to 'Float'
        constructor

    """
    assert value is None or isinstance(value, (float, decimal.Decimal)), value
    if isinstance(value, decimal.Decimal) and digits is None and precision is None:
        precision = max(-value.as_tuple().exponent, 0)
    return Value(Float(digits=digits, precision=precision), value)


def bval(value):
    """Return 'Value' instance of type 'Boolean' with given value.

    Arguments:

      value -- internal value of the 'Value' instance, basestring

    """
    assert value is None or isinstance(value, bool), value
    return Value(Boolean(), value)


def dval(value):
    """Return 'Value' instance of type 'Date' with given value.

    Arguments:

      value -- internal value of the 'Value' instance, datetime.date

    """
    assert value is None or isinstance(value, datetime.date), value
    return Value(Date(), value)


def dtval(value, without_timezone=False):
    """Return 'Value' instance of type 'DateTime' with given value.

    Arguments:

      value -- internal value of the 'Value' instance, datetime.datetime
      without_timezone -- corresponding constructor argument of 'DateTime'

    """
    assert value is None or isinstance(value, datetime.datetime), value
    return Value(DateTime(without_timezone=without_timezone), value)


def tval(value, without_timezone=False):
    """Return 'Value' instance of type 'Time' with given value.

    Arguments:

      value -- internal value of the 'Value' instance, datetime.time
      without_timezone -- corresponding constructor argument of 'Time'

    """
    assert value is None or isinstance(value, datetime.time), value
    return Value(Time(without_timezone=without_timezone), value)


def binval(value):
    """Return 'Value' instance of type 'Binary' with given value.

    Arguments:

      value -- internal value of the 'Value' instance as 'bytes' or an open
        stream (a file-like object).

    """
    return Value(Binary(), value)


def wmval(value):
    """Return 'VMValue' instance of type 'String' with given value.

    Arguments:

      value -- internal value of the 'WMValue' instance, basestring

    """
    assert value is None or isinstance(value, basestring), value
    return WMValue(String(), value)
