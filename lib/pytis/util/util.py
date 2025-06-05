# -*- coding: utf-8 -*-

# Copyright (C) 2018-2025 Tomáš Cerha <t.cerha@gmail.com>
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
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

"""Různé užitečné pomůcky usnadňující psaní pythonových programů.

Modul obsahuje víceméně triviální funkce, které svým charakterem nepatří jinam
a které slouží primárně pro zjednodušení zápisu často užívaných konstrukcí.
Pokud se nějaký tématický okruh pomůcek rozmnoží, může být přesunut do
samostatného modulu.

Tento modul je výjimečný ve dvou směrech:

1. Vzhledem k triviálnímu charakteru zde obsažených funkcí a vzhledem k tomu,
   že jejich primárním účelem je zkrátit a zčitelnit kód, je povoleno jej
   importovat následujícím způsobem:

     from util import *

"""
from __future__ import print_function

from past.builtins import basestring
from builtins import range
from future.utils import python_2_unicode_compatible

import base64
import cgitb
import copy
import functools
import gc
import getopt
import inspect
import mimetypes
import operator
import os
import io
import re
import sys
import time
import tempfile
import _thread
import platform

unistr = type(u'')  # Python 2/3 transition hack.
try:
    from types import ClassType
except ImportError:
    ClassType = type  # ClassType is gone in Python 3.  Remove when dropping Python 2 support.


# Classes


class ProgramError(Exception):
    """Výjimka signalizující programovou chybu.

    Programová chyba je chyba, která by teoreticky neměla nikdy nastat.
    Vznikla chybou programu, ať už přímo v místě, kde je detekována, nebo
    chybným voláním kódu zvně (například nedodržení typů argumentů metody).

    Programovou chybou naopak není systémová chyba, jejíž vznik lze za určitých
    okolností očekávat, ani chyba způsobená akcemi uživatele (například chybně
    zadaná data na vstupu).

    Tato výjimka by neměla být odchytávána, s výjimkou funkcí pro ošetření
    havárie programu.  Její výskyt znamená, že program se dostal do
    nedefinovaného stavu a měl by být ukončen.  (V určitých, v dokumentačních
    řetězcích jasně definovaných, případech tento požadavek nemusí být striktní
    a může znamenat pouze lokální zhroucení týkající se určitého modulu,
    případně i s možností uzdravení reinicializací.)

    Výjimka pouze dědí obecnou výjimkovou třídu a nedefinuje nic nového.

    """
    pass


class InvalidAccessError(Exception):
    """Signalizace neautorizovaného přístupu.

    Tato výjimka je typicky vyvolávána na straně vzdáleného serveru, pokud se
    klient pokouší volat vzdálenou metodu bez potřebné dodatečné autorizace
    nebo s argumenty chybných typů.

    """

    def __init__(self, *args):
        import pytis.util
        pytis.util.log(pytis.util.OPERATIONAL, 'Invalid access:', args)
        super(InvalidAccessError, self).__init__(*args)


class FileError(Exception):
    """Výjimka vyvolávaná po chybě při práci se soubory.

    Nejedná se o duplikát 'os.OSError', používá se například pokud nelze
    z nějakého zvláštního důvodu vytvořit dočasný soubor.

    """
    pass


class NotImplementedException(Exception):
    """Exception raised on calling unimplemented methods.
    """
    pass


class Counter(object):
    """Jednoduchý čítač.

    Po svém vytvoření je inicializován na hodnotu 0 nebo hodnotu zadanou v konstruktoru
    a při každém čtení metodou 'next()' je tato hodnota zvýšena.

    Třída není thread-safe.

    """

    def __init__(self, value=0):
        """Inicializuj instanci."""
        self._value = value

    def next(self):
        """Zvyš hodnotu čítače o 1 a vrať ji."""
        self._value = self._value + 1
        return self._value

    def current(self):
        """Vrať aktuální hodnotu čítače bez jejího zvýšení."""
        return self._value

    def reset(self):
        """Nastav hodnotu čítače na 0."""
        self._value = 0


class Popen(object):
    """Třída umožňující spouštění programů a komunikaci s nimi.

    Při vytváření instance třídy je vytvořen nový proces, se kterým je možno
    komunikovat pomocí zadaných nebo nově vytvořených streamů, blíže viz metoda
    '__init__()'.

    Streamy pro komunikaci s procesem jsou dostupné prostřednictvím metod
    'from_child()' a 'to_child()'.  Process id spuštěného programu je
    dostupné přes metodu 'pid()'.

    """

    def __init__(self, command, to_child=None, from_child=None,
                 directory=None):
        """Spusť 'command' v samostatném procesu.

        Argumenty:

          command -- string nebo sekvence stringů definující spouštěný příkaz a
            jeho argumenty
          to_child -- file descriptor nebo file object obsahující file
            descriptor otevřené pro zápis, prostřednictvím kterého bude
            zapisováno na standardní vstup spuštěného procesu; může být též
            'None', v kterémžto případě bude pro tyto účely vytvořena nová
            roura
          from_child -- file descriptor nebo file object obsahující file
            descriptor otevřené pro čtení, prostřednictvím kterého bude
            dostupný standardní výstup spuštěného procesu; může být též 'None',
            v kterémžto případě bude pro tyto účely vytvořena nová roura
          directory -- existující adresář (coby string), ve kterém má být
            proces spuštěn, nebo 'None', v kterémžto případě je proces spuštěn
            v aktuálním adresáři

        """
        # Temporary hack for Python 2/3 compatibility.
        try:
            file_types = (file, io.IOBase)
        except NameError:  # 'file' not defined in Python 3.
            file_types = (io.IOBase,)
        if to_child is None:
            r_to_child, w_to_child = os.pipe()
        else:
            if isinstance(to_child, file_types):
                to_child = to_child.fileno()
            r_to_child, w_to_child = to_child, None
        if from_child is None:
            r_from_child, w_from_child = os.pipe()
        else:
            if isinstance(from_child, file_types):
                from_child = from_child.fileno()
            r_from_child, w_from_child = None, from_child
        pid = os.fork()
        if pid == 0:
            if directory is not None:
                os.chdir(directory)
            try:
                if w_to_child is not None:
                    os.close(w_to_child)
                if r_from_child is not None:
                    os.close(r_from_child)
                os.dup2(r_to_child, 0)
                os.dup2(w_from_child, 1)
                if isinstance(command, basestring):
                    command = ['/bin/sh', '-c', command]
                for i in range(3, 256):
                    try:
                        os.close(i)
                    except Exception:
                        pass
                os.execvp(command[0], command)
            finally:
                os._exit(1)
        if r_to_child is not None:
            try:
                os.close(r_to_child)
            except OSError:
                pass
        if w_to_child is None:
            self._to_child = None
        else:
            self._to_child = os.fdopen(w_to_child, 'w')
        if w_from_child is not None and isinstance(w_from_child, int):
            try:
                os.close(w_from_child)
            except OSError:
                pass
        if r_from_child is None:
            self._from_child = None
        else:
            self._from_child = os.fdopen(r_from_child, 'r')
        self._pid = pid

    def from_child(self):
        """Vrať file object pro čtení ze standardního výstupu procesu."""
        return self._from_child

    def to_child(self):
        """Vrať file object pro zápis na standardní vstup procesu."""
        return self._to_child

    def pid(self):
        """Vrať process id spuštěného programu."""
        return self._pid

    def wait(self):
        """Čekej na dokončení podprocesu."""
        os.waitpid(self.pid(), 0)


@python_2_unicode_compatible
class XStack(object):
    """Stack of items tracking insertion order and activation order.

    * Items are appended right after the currently active item.
    * Any existing item can be activated.
    * The order of activation is tracked and may be retrieved by 'mru()'.
    * Each distinct object may only be pushed once.

    """

    def __init__(self):
        self._active = None
        self._mru = []
        self._list = []

    def __str__(self):
        classname = str(self.__class__).split('.')[-1]
        contents = ', '.join(str(x) for x in self._list)
        return '<%s items=%s>' % (classname, contents)

    def __len__(self):
        return len(self._list)

    def items(self):
        """Return the tuple of all items in their order on the stack."""
        return tuple(self._list)

    def push(self, item):
        """Append the item right after the currently active item.

        The inserted item automatically becomes active.

        """
        if self._list:
            self._list.insert(self._list.index(self.active()), item)
        else:
            self._list.append(item)
        self.activate(item)

    def remove(self, item):
        """Remove the given 'item' from the stack.

        If 'item' is currently the active item, the following item is activated
        (or the preceding one when no such item exists).

        """
        if item is self._list[-1]:
            to_activate = self.prev()
        else:
            to_activate = self.next()
        self._list.remove(item)
        self._mru.remove(item)
        if item is self._active:
            self.activate(to_activate)

    def activate(self, item):
        """Activate given item."""
        assert item in self._list or item is None and not self._list
        self._active = item
        if item is not None:
            if item in self._mru:
                self._mru.remove(item)
            self._mru.insert(0, item)

    def active(self):
        """Return the currently active item."""
        assert self._active in self._list or (self._active is None and not self._list)
        return self._active

    def mru(self):
        """Return the tuple of items ordered by last activation (most recently used).

        The active item is first followed by the previously active item etc.

        """
        return tuple(self._mru)

    def next(self):
        """Return item just after the currently active item.

        If the active item is the only item on the stack or when the stack is
        empty, return 'None'.  Otherwise, if there is nothing after the
        currently active item, return the first item.

        """
        if len(self._list) <= 1:
            return None
        i = self._list.index(self._active)
        return self._list[(i + 1) % len(self._list)]

    def prev(self):
        """Return item just prior to the currently active item.

        If the active item is the only item on the stack or when the stack is
        empty, return 'None'.  Otherwise, if there is nothing prior to the
        currently active item, return the last item.

        """
        if len(self._list) <= 1:
            return None
        i = self._list.index(self._active)
        return self._list[i - 1]


class Attribute(object):
    """Definition of a 'Structure' attribute."""

    def __init__(self, name, type=object, default=None, mutable=False):
        """
        Arguments:

          name -- name of the attribute, string
          type -- Python type or a sequence of Python types of the attribute
          default -- default value of the attribute
          mutable -- whether the given attribute is mutable; if so, a setter
            function is defined for it

        """
        self._name = name
        self._type = type
        self._default = default
        self._mutable = mutable

    def name(self):
        return self._name

    def type(self):
        return self._type

    def default(self):
        return self._default

    def mutable(self):
        return self._mutable


@python_2_unicode_compatible
class Structure (object):
    """Simple data structures.

    Attribute names of the instance are listed in the sequence '_attributes'.
    Each element of '_attributes' is an 'Attribute' instance.

    """
    _attributes = ()

    def __init__(self, _template=None, **kwargs):
        self._init(kwargs, template=_template)

    def __str__(self):
        result = '<%s:' % (self.__class__.__name__,)
        for member in self._attributes:
            name = member.name()
            result = result + (' %s=%s;' % (name, getattr(self, name)()))
        result = result + '>'
        return result

    def _init(self, kwargs, nodefault=False, template=None):
        for member in self._attributes:
            name = member.name()
            value = UNDEFINED
            if name in kwargs:
                value = kwargs[name]
                assert isinstance(value, member.type()) or value == member.default(), \
                    ("Invalid attribute type", name, value, member.type())
                if __debug__:
                    del kwargs[name]
            else:
                if template is not None:
                    assert isinstance(self, template.__class__)
                    try:
                        value = getattr(template, name)()
                    except AttributeError:
                        pass
                if value is UNDEFINED and not nodefault:
                    value = member.default()
            if value is not UNDEFINED:
                setattr(self, name, lambda value=value: value)
            if member.mutable():
                setattr(self, 'set_' + name,
                        lambda value, name=name: self._replace_value(name, value))
        assert not kwargs, ("Extra initialization arguments", tuple(kwargs.keys()))

    def _replace_value(self, name, value):
        setattr(self, name, lambda value=value: value)

    def copy(self, **kwargs):
        result = copy.copy(self)
        result._init(kwargs, nodefault=True)
        return result

    def __eq__(self, other):
        if self.__class__ != other.__class__:
            return False
        for attribute in self._attributes:
            name = attribute.name()
            if getattr(self, name) != getattr(other, name):
                return False
        return True

    def __ne__(self, other):
        # Implied automatically in Python 3 so can be removed when dropping Python 2 support.
        return not self.__eq__(other)

    __hash__ = None


class DBParams(object):
    """Provides access to shared parameters.

    Shared parameters provide a way to share global and user specific values
    between the database and application code.  They are defined by
    'Application.params()' and accessed through 'pytis.api.app'.
    'app.param.foo' is a 'DBParams' instance for the 'SharedParams'
    specification named 'foo'.

    The 'DBParams' instance provides access to the parameter values through its
    public attributes.  Their names correspond to the names of the columns
    present in the data object represented by the instance.  When read, the
    attributes return the internal Python value of the column, when assigned,
    they update the value in the database.

    It is not possible to use a parameter named 'add_callback', 'cbvalue' and
    'reload' due to the presence of the public methods of the same name.

    """
    _lock = _thread.allocate_lock()

    def __init__(self, name, condition=None):
        self._name = name
        self._condition = condition
        self._callbacks = {}

    def __getattr__(self, name):
        if name in ('_row', '_data'):
            self._data = data_object(self._name)
            self._data.add_callback_on_change(self._on_change)
            self._select()
            return self.__dict__[name]
        elif name in self._row:
            return self._row[name].value()
        else:
            raise AttributeError("'%s' object for '%s' has no attribute '%s'" %
                                 (self.__class__.__name__, self._name, name))

    def __setattr__(self, name, value):
        if name.startswith('_'):
            super(DBParams, self).__setattr__(name, value)
        elif name in self._row:
            import pytis.data
            row = pytis.data.Row(((name, pytis.data.Value(self._row[name].type(), value)),))
            key = [self._row[c.id()] for c in self._data.key()]
            with Locked(self._lock):
                updated_row, success = self._data.update(key, row)
                if success and updated_row:
                    self._row = updated_row
                else:
                    raise ProgramError("Failed updating DBParams {} row {}: {}".format(
                        self._name, [v.value() for v in key], updated_row))
        else:
            raise AttributeError("'%s' object for '%s' has no attribute '%s'" %
                                 (self.__class__.__name__, self._name, name))

    def __contains__(self, key):
        return key in self._row

    def _select(self):
        data = self._data
        with Locked(self._lock):
            data.select(condition=self._condition)
            row = data.fetchone()
            if row:
                self._row = row
            else:
                raise ProgramError("Failed getting DBParams {} row for {}.".format(
                    self._name, self._condition))
            data.close()

    def _on_change(self):
        orig_row = self._row
        self._select()
        for name, callbacks in self._callbacks.items():
            if self._row[name].value() != orig_row[name].value():
                for callback in callbacks:
                    callback()

    def add_callback(self, name, callback):
        """Registger a callback called on given parameter change.

        The callback function is called without arguments whenever the value of
        given parameter changes.

        """
        assert name is None or name in self._row
        self._callbacks.setdefault(name, []).append(callback)

    def cbvalue(self, name, cbcolumn):
        """Return the value of given codebook column for given parameter.

        Arguments:

          name -- name of the parameter with a codebook (enumerator).
          cbcolumn -- codebook column name

        ValueError is raised if given column has no enunerator in the
        underlying data object.

        None is returned if the codebook doesn't include the record for the
        current value of parameter 'name'.

        """
        value = self._row[name]
        enumerator = value.type().enumerator()
        if not enumerator:
            raise ValueError("Column '%s' has no enumerator!" % name)
        row = enumerator.row(value.value())
        if not row:
            return None
        return row[cbcolumn].value()

    def reload(self):
        """Explicitly reload parameter values.

        Values normally reload automatically thanks to DB notifications.
        However it may be practical in certain situations to make sure you are
        working with up-to-date values.

        """
        self._on_change()


class object_2_5(object):
    """Base class emulating Python 2.5 'object' class.

    Unlike 'object' class in Python 2.6 it consumes any keyword arguments.
    This makes handling some multiple inheritance situations easier.

    """

    def __init__(self, **kwargs):
        object.__init__(self)


# Functions

def identity(x):
    """Vrať 'x'."""
    return x


def is_(x, y):
    """Vrať pravdu, právě když je 'x' identické s 'y' ve smyslu operátoru 'is'.

    'x' a 'y' mohou být libovolné objekty.

    """
    return x is y


def xor(x, y):
    """Vrať pravdivostní hodnotu exkluzivního OR výrazů 'x' a 'y'."""
    return (x and not y) or (not x and y)


def xtuple(x):
    """Vrať 'x' jako tuple.

    Je-li 'x' sekvence, vrať tuple, jehož prvky se shodují s prvky 'x'.  Jinak
    vrať tuple, jehož jediným prvkem je 'x'.

    """
    if is_sequence(x):
        return tuple(x)
    else:
        return (x,)


def xlist(x):
    """Vrať 'x' jako list.

    Je-li 'x' sekvence, vrať list, jehož prvky se shodují s prvky 'x'.  Jinak
    vrať list, jehož jediným prvkem je 'x'.

    """
    if is_sequence(x):
        return list(x)
    else:
        return [x]


def position(element, sequence, key=identity):
    """Vrať pozici 'element' v 'sequence'.

    Pokud se 'element' v 'sequence' nenachází, vrať 'None'.

    Porovnání prvků je prováděno operátorem '=='.  Hodnoty prvků 'sequence'
    jsou získávány funkcí 'key', která musí jako svůj jediný argument přijímat
    prvky 'sequence'.

    """
    for i in range(len(sequence)):
        if key(sequence[i]) == element:
            return i
    else:
        return None


def find(element, sequence, key=identity, test=operator.eq):
    """Vrať nejlevější prvek 'sequence' rovnající se 'element'.

    Pokud se 'element' v 'sequence' nenachází, vrať 'None'.

    Argumenty:

      key -- funkce jednoho argumentu, kterým je prvek 'sequence', vracející
        hodnotu pro porovnání s 'element'
      test -- funkce dvou argumentů, z nichž první je 'element' a druhý prvek
        'sequence' po aplikaci 'key'.  Je-li zadáno, provádí se porovnání touto
        funkcí, jinak se porovnání provádí operátorem '=='.

    """
    for elt in sequence:
        if test(element, key(elt)):
            return elt
    else:
        return None


def assoc(item, alist):
    """Vrať nejlevější prvek z 'alist', jehož první prvek se rovná 'item'.

    Pokud takový prvek neexistuje, vrať 'None'.  Porovnání se provádí
    operátorem '='.

    'alist' musí být sekvence neprázdných sekvencí.

    """
    return find(item, alist, key=(lambda x: x[0]))


def rassoc(item, alist):
    """Vrať nejlevější prvek z 'alist', jehož druhý prvek se rovná 'item'.

    Pokud takový prvek neexistuje, vrať 'None'.  Porovnání se provádí
    operátorem '='.

    'alist' musí být sekvence dvouprvkových sekvencí.

    """
    return find(item, alist, key=(lambda x: x[1]))


def remove_duplicates(items):
    """Return a list containing each item of 'items' just once."""
    return list(set(items))


def flatten(list):
    """Vrať 'list' bez vnořených sekvencí.

    Argumenty:

      list -- libovolná sekvence

    Vrací: Sekvenci tvořenou prvky sekvence 'list', přičemž každý prvek, který
      je sám sekvencí, je ve vrácené sekvenci rekurzivně nahrazen svými prvky.

    """
    result = []
    if is_sequence(list):
        result += functools.reduce(operator.add, [flatten(x) for x in list], [])
    else:
        result.append(list)
    return result


def nreverse(list):
    """Vrať prvky 'list' v opačném pořadí.

    Argumenty:

      list -- libovolný list

    Funkce je destruktivní, tj. hodnota 'list' je v ní změněna.

    """
    list.reverse()
    return list


def strxfrm(string):
    """Return a string transformation for locale aware sorting.

    Like, locale.strxfrm, but addresses some compatibility issues.

    """
    if sys.platform == 'darwin':
        # locale.strxfrm sorts incorrectly on macOS and for some strings it even
        # fails with "OSError: [Errno 22] Invalid argument".
        # Normalize is not a 100% replacement for strxfrm, but at least improves the
        # result sufficiently for the non-primary platform...
        import unicodedata
        return unicodedata.normalize('NFKD', string)
    else:
        import locale
        return locale.strxfrm(string)



def sameclass(o1, o2, strict=False):
    """Vrať pravdu, právě když 'o1' a 'o2' jsou instance téže třídy.

    Je-li argument 'strict' pravdivý, musí se rovnat třídy obou objektů 'o1' a
    'o2' ve smyslu operátoru '=='.  V opačném případě postačí rovnost jmen tříd
    a jejich modulů.

    """
    try:
        c1 = o1.__class__
        c2 = o2.__class__
    except Exception:
        return False
    if c1 == c2:
        return True
    else:
        if strict:
            return False
        else:
            return c1.__name__ == c2.__name__ and c1.__module__ == c2.__module__


def _mro(class_):
    def dfs(dfs, queue, found):
        if queue:
            head = queue[0]
            if head in found:
                result = dfs(dfs, queue[1:], found)
            else:
                found.append(head)
                result = dfs(dfs, list(head.__bases__) + queue[1:], found)
        else:
            result = found
        return result
    return dfs(dfs, [class_], [])


_public_attributes = {}


def public_attributes(class_, prefix=None):
    """Vrať tuple všech jmen veřejných atributů třídy 'class_'.

    Vrácená jména jsou strings a obsahují i poděděné atributy.  Nejsou mezi
    nimi však žádná jména začínající podtržítkem.  Jména atributů jsou ve
    vrácené sekvenci v pořadí dědičnosti počínaje od 'class_'.  Mohou se v nich
    vyskytovat duplicity.

    Dojde-li od posledního volání této funkce v 'class_' ke změně atributů,
    tato změna nemusí být zohledněna.

    """
    global _public_attributes
    try:
        return _public_attributes[(class_, prefix)]
    except KeyError:
        pass
    att = functools.reduce(operator.add, [[attr for attr in dir(c)
                                           if prefix is None or attr.startswith(prefix)]
                                          for c in _mro(class_)])
    result = tuple(remove_duplicates([s for s in att if not s or s[0] != '_']))
    _public_attributes[(class_, prefix)] = result
    return result


def public_attr_values(class_, prefix=None):
    """Return a tuple of values of all public attributes of class 'class_'.

    Just a shorthand to get the values of attributes returned by
    'public_attributes()'.

    """
    # Note: This function should actually be used in most assertions for use of
    # specification class constants.  They mostly use just public_attributes()
    # as they rely on the typical 1:1 mapping of attribute names and their
    # values.  But when this is not the case, public_attr_values() must be
    # used.
    return tuple(getattr(class_, attr) for attr in public_attributes(class_, prefix=prefix))


def argument_names(callable, var_positional=False, var_keyword=False):
    """Return names of all function/method arguments as a tuple of strings.

    The method argument 'self' is ignored.  The names are returned in the order in which the
    arguments are defined, including all keyword arguments.

    Only named arguments are taken into account by default.  Set
    'var_positional' to True to also return variable positional arguments such
    as '*args' and set 'var_keyword' to True to return also variable keyword
    arguments such as '**kwargs'.

    >>> def x(a, count, *names, **keys):
    ...     return count
    >>> argument_names(x)
    ('a', 'count')
    >>> argument_names(x, var_keyword=True)
    ('a', 'count', '**keys')
    >>> argument_names(x, var_positional=True)
    ('a', 'count', '*names')
    >>> argument_names(x, var_positional=True, var_keyword=True)
    ('a', 'count', '*names', '**keys')

    """
    if sys.version_info[0] == 2:
        spec = inspect.getargspec(callable)
        args = spec.args
        if var_positional and spec.varargs:
            args.append('*' + spec.varargs)
        if var_keyword and spec.keywords:
            args.append('**' + spec.keywords)
    else:
        args = [{a.VAR_POSITIONAL: '*', a.VAR_KEYWORD: '**'}.get(a.kind, '') + a.name
                for a in inspect.signature(callable).parameters.values()
                if (a.kind == a.VAR_POSITIONAL and var_positional or
                    a.kind == a.VAR_KEYWORD and var_keyword or
                    a.kind not in (a.VAR_POSITIONAL, a.VAR_KEYWORD))]
    if args and args[0] == 'self':
        args = args[1:]
    return tuple(args)


def direct_public_members(obj):
    """Vrať tuple všech přímých veřejných atributů a metod třídy objektu 'obj'.

    Přímými členy třídy jsou myšleny ty, které nejsou shodné se stejnojmenným
    členem některého předka třídy.  Veřejnými členy třídy jsou myšleny ty,
    jejichž název nezačíná podtržítkem.

    """
    if isinstance(obj, (ClassType, type)):
        cls = obj
    else:
        cls = obj.__class__

    def public_members(cls):
        return [(name, value) for name, value in inspect.getmembers(cls)
                if name and not name.startswith('_')]
    super_members = functools.reduce(operator.add,
                                     [public_members(b) for b in cls.__bases__],
                                     [])
    result = [name for name, value in public_members(cls)
              if find(value, [x[1] for x in super_members]) is None]
    return tuple(result)


def less(o1, o2):
    """Similar to '<' operator but handles 'None' values.

    Arguments:

      o1, o2 -- objects to compare

    If 'o2' is 'None', return False.
    Else if 'o1' is 'None', return True.
    Else return the result of 'o1 < o2'.

    """
    if o2 is None:
        return False
    if o1 is None:
        return True
    return o1 < o2


def less_equal(o1, o2):
    """Similar to '<=' operator but handles 'None' values.

    Arguments:

      o1, o2 -- objects to compare

    If 'o1' is None and 'o2' is 'None', return True.
    Else if 'o2' is 'None', return False.
    Else if 'o1' is 'None', return True.
    Else return the result of 'o1 <= o2'.

    """
    if o2 is None:
        return o1 is None
    if o1 is None:
        return True
    return o1 <= o2


def hash_attr(self, attributes):
    """Vrať hash-kód instance 'self'.

    Kód je vytvářen dle hodnot 'attributes' instance, v souladu s pythonovými
    pravidly pro hash kód.

    Argumenty:

      self -- instance třídy, pro níž má být hash kód vytvořen
      attributes -- sekvence jmen atributů (strings), jejichž hodnoty mají být
        při vytváření kódu uvažovány

    """
    dict = self.__dict__

    def h(obj):
        if isinstance(obj, list):
            obj = tuple(obj)
        return hash(obj)
    return functools.reduce(operator.xor, [h(dict[a]) for a in attributes])


def is_sequence(x):
    """Vrať pravdu, právě když 'x' je list nebo tuple."""
    return isinstance(x, (tuple, list))


def ecase(value, *settings):
    """Vrať hodnotu ze 'settings' odpovídající 'value'.

    Pokud 'value' není v 'settings' obsaženo, vyvolej výjimku 'ProgramError'.
    Je-li v 'settings' 'value' obsaženo vícekrát, je uvažován první výskyt.

    Argumenty:

      value -- libovolný objekt; je porovnáván s prvními prvky prvků 'settings'
        operátorem '='
      settings -- sekvence dvojic (KEY, VALUE), kde KEY odpovídá některé
        z možných hodnot 'value' a VALUE je hodnota, kterou má funkce vrátit
        v případě shody KEY a 'value' vrátit

    Vrací: VALUE z dvojice ze 'settings', jejíž KEY odpovídá 'value'.

    """
    s = assoc(value, settings)
    if s is None:
        raise ProgramError('Invalid ecase value', value)
    return s[1]


class Locked(object):
    """Context manager for code protected by locking.

    Usage:

       with Locked(my_lock):
           do_something()

    Constructor arguments:

      lock -- 'thread.lock' instance to be used for locking

    It is recommended to use this context manager instead of direct locking for
    the following reasons:

    - The calling locking code is somewhat shorter and safer.

    - locking is wrapped with some additional code for debugging and deadlock
      prevention.

    """

    _debug_lock = _thread.allocate_lock() if __debug__ else None
    _active_locks = {} if __debug__ else None

    def __init__(self, lock):
        self._lock = lock

    def __enter__(self):
        lock = self._lock
        if __debug__:
            self.__class__._debug_lock.acquire()
            try:
                self._thread_id = thread_id = _thread.get_ident()
                locks = self.__class__._active_locks.setdefault(thread_id, [])
                if lock in locks:
                    raise Exception('Deadlock detected')
                locks.append(lock)
            finally:
                self.__class__._debug_lock.release()
        lock.acquire()

    def __exit__(self, type, value, tb):
        lock = self._lock
        lock.release()
        if __debug__:
            self.__class__._debug_lock.acquire()
            try:
                self.__class__._active_locks[self._thread_id].remove(lock)
            finally:
                self.__class__._debug_lock.release()


def dev_null_stream(mode):
    """Vrať bezdatový stream.

    Vrácený stream je plnohodnotné file object a funguje jako zařízení
    '/dev/null' -- neposkytuje žádná data a všechna přijatá data zahazuje.

    Argumenty:

      mode -- jeden ze stringů 'r' (nechť je vrácený stream otevřen pro čtení)
        nebo 'w' (nechť je vrácený stream otevřen pro zápis)

    """
    assert mode in ('r', 'w')
    return open('/dev/null', mode)


_mktempdir_counter = None


def mktempdir(prefix='pytis'):
    """Vytvoř podadresář v adresáři pro dočasné soubory.

    Adresář pro dočasné soubory je dán konfigurací.  Jméno podadresáře se
    skládá ze zadaného 'prefix', kterým musí být string, a generované přípony.

    Podadresář je vytvořen s přístupovými právy 0o700.  Není-li možné adresář
    z nějakého důvodu vytvořit, je vyvolána výjimka 'FileError'.

    Vrací: Jméno vytvořeného adresáře včetně kompletní cesty.  Žádná dvě volání
    této funkce nevrátí stejné jméno; to však neplatí v případě použití
    threads, protože funkce není thread-safe.

    """
    import pytis
    global _mktempdir_counter
    if _mktempdir_counter is None:
        _mktempdir_counter = Counter()
    pattern = os.path.join(pytis.config.tmp_dir, '%s%d.%%d' % (prefix, os.getpid()))
    oldumask = os.umask(0o077)
    try:
        for i in range(1000):
            n = _mktempdir_counter.next()
            try:
                the_dir = pattern % n
                os.mkdir(the_dir, 0o700)
                break
            except OSError:
                pass
        else:
            raise FileError(pattern)
    finally:
        os.umask(oldumask)
    return the_dir


def format_byte_size(size):
    """Return a human readable string representing given int bytesize."""
    size = float(size)
    units = ('B', 'kB', 'MB', 'GB')
    i = 0
    while size >= 1024 and i < len(units) - 1:
        size /= 1024
        i += 1
    return '%.4g ' % size + units[i]


_CAMEL_CASE_WORD = re.compile(r'[A-Z][a-z\d]*')


def split_camel_case(string):
    """Return a lowercase string using 'separator' to concatenate words."""
    return _CAMEL_CASE_WORD.findall(string)


def camel_case_to_lower(string, separator='-'):
    """Return a lowercase string using 'separator' to concatenate words."""
    return separator.join([w.lower() for w in split_camel_case(string)])


def nextval(seq, connection_name=None):
    """Return a function generating next value from given DB sequence.

    The argument 'seq' is the string name of a database sequence object.  The
    returned function accepts one optional argument transaction and returns the
    next value from given sequence when called.

    Designed for convenient specification of 'default' argument in 'Field'
    constructor, such as default=nextval('my_table_id_seq').

    """
    import pytis
    import pytis.data

    def conn_spec():
        return pytis.config.dbconnection
    counter = pytis.data.DBCounterDefault(seq, conn_spec, connection_name=connection_name)
    return lambda transaction=None: counter.next(transaction=transaction)


def rsa_encrypt(key, text):
    """Return text encrypted using RSA 'key' and base64 encoded as 'bytes'.

    If key is 'None', return 'text'.

    Arguments:

      key -- public key to use for encryption as 'str' or 'None'
      text -- text to encrypt

    """
    if key:
        if isinstance(text, unistr):
            text = text.encode('utf-8')
        try:
            from Cryptodome.PublicKey import RSA
            from Cryptodome.Cipher import PKCS1_OAEP
        except ImportError as e:
            from Crypto.PublicKey import RSA
            from Crypto.Cipher import PKCS1_OAEP
        rsa_key = RSA.importKey(key)
        cipher = PKCS1_OAEP.new(rsa_key)
        encrypted = cipher.encrypt(text)
        return base64.b64encode(encrypted)
    else:
        return text


def load_module(module_name):
    """Load and return module named 'module_name'.

    The module is loaded including its parent modules.

    Arguments:

      module_name -- the module name, it may contain dots; basestring

    """
    module = __import__(module_name)
    components = module_name.split('.')[1:]
    while components:
        try:
            module = getattr(module, components.pop(0))
        except AttributeError:
            raise ImportError(module_name)
    return module

# TODO: kwargs -> **kwargs

def data_object(spec, kwargs=None):
    """Create a data object for given specification.

    Arguments:

      spec -- specification name as a string or a 'pytis.data.DataFactory'
        instance.
      kwargs -- a dictionary of keyword arguments passed to the data object
        constructor.  The argument 'connection_data' is added automatically
        if the data class is derived from 'pytis.data.DBData'.

    Raises 'ResolverError' or 'ProgramError' if data object creation fails.

    """
    import pytis
    import pytis.data
    if isinstance(spec, basestring):
        factory = pytis.config.resolver.get(spec, 'data_spec')
    else:
        factory = spec
    assert isinstance(factory, pytis.data.DataFactory)

    if kwargs is None:
        kwargs = {}
    if issubclass(factory.class_(), pytis.data.DBData):
        kwargs = dict(kwargs, connection_data=pytis.config.dbconnection)

    try:
        import pytis.form
    except Exception:
        return factory.create(**kwargs)
    else:
        success, data = pytis.form.db_operation(factory.create, **kwargs)
        if success:
            return data
        else:
            raise ProgramError("Unable to create data object:", spec)


def form_view_data(resolver, name, dbconnection_spec=None):
    """Return pair of specification objects (VIEW, DATA) for specification 'name'.

    VIEW is instance of view specification and DATA is instance of the
    specification data object related to specification named 'name'.

    Arguments:

      resolver -- resolver to use to find the given specification;
        'pytis.util.Resolver' instance
      name -- name of the specification; basestring

    """
    import pytis
    import pytis.util
    assert isinstance(resolver, pytis.util.Resolver), resolver
    assert isinstance(name, basestring), name
    if dbconnection_spec is None:
        dbconnection_spec = pytis.config.dbconnection
    view = resolver.get(name, 'view_spec')
    data_spec = resolver.get(name, 'data_spec')
    data = data_spec.create(dbconnection_spec=dbconnection_spec)
    return view, data


class Attachment:
    """Representation of  e-mail attachment for 'send_mail()' 'attachments' argument.

    Constructor arguments:

      filename -- file name of the attachment as a string (mandatory)
      data -- attachment data as 'bytes' instance or an open file to read the
        attachment data from it; may be also None in which case the data is
        read from the file given by 'filename' (which must be a full path which
        exists and is readable).
      mime_type -- MIME type of the attachment as a string; if None, the MIME
        type is automatically guessed from the 'filename' extension.

    """

    def __init__(self, filename, data=None, mime_type=None):
        assert filename is None or isinstance(filename, basestring), filename
        assert data is None or isinstance(data, bytes) or hasattr(data, 'read'), data
        assert mime_type is None or isinstance(mime_type, basestring), mime_type
        self._filename = filename
        self._data = data
        self._mime_type = mime_type

    @property
    def filename(self):
        return os.path.basename(self._filename)

    @property
    def data(self):
        if self._data is None:
            with open(self._filename, 'rb') as f:
                data = f.read()
        elif isinstance(self._data, bytes):
            data = self._data
        else:
            try:
                data = self._data.read()
            finally:
                self._data.close()
        return data

    @property
    def mime_type(self):
        mime_type = self._mime_type
        if mime_type is None:
            mime_type, encoding = mimetypes.guess_type(self._filename)
            if mime_type is None or encoding is not None:
                # No guess could be made, or the file is encoded (compressed).
                mime_type = 'application/octet-stream'
        return mime_type


def send_mail(subject, text, to, sender, sender_name=None, cc=(), bcc=(),
              html=False, attachments=(), encryption_key=None,
              message_id=None, headers=(), smtp_server=None, smtp_port=25):
    """Send a MIME e-mail message.

    Arguments:

      subject -- message subject as a string
      text -- message text as a string
      to -- recipient address(es) as a string or a sequence of strings
      sender -- sender address as a string
      sender_name -- optional human readable sender name as a string; if not
        None, the name is added to the 'From' header in the standard form:
        "sender name" <sender@email>.
      cc -- "carbon copy" recipient address(es) (string or their sequence)
      bcc -- "blind carbon copy" recipient address(es) (string or their sequence)
      html -- iff True, the message 'text' is considered HTML
      attachments -- sequence of 'Attachment' instances defining the files to
        attach to the message.  Items may also be strings which will be
        automatically converted to 'Attachment' instances passing the string as
        its 'filename'.
      encryption_key -- public PGP key used to encrypt the message in OpenPGP
        message format (don't encrypt when None)
      message_id -- message id (string) to be used for the Message-Id header.
        The format must follow the RFC 2822 specification.
      headers -- additional headers to insert into the mail; it must be a tuple
        of pairs (HEADER, VALUE) where HEADER is an ASCII string containing the
        header name (without the final colon) and value is a string containing
        the header value.
      smtp_server -- Specific SMTP server to use instead of the default given by
        configuration option 'smtp_server'.
      smtp_port -- Specific SMTP server to use instead of default 25.

    May raise the following exceptions:

    'EncryptionKeyError' ... Invalid encryption key.  The key can not be
      imported. The data may be corrupted or in an unsupported format.

    'EncryptionError' ... Encryption failed by given key.  Typically trying
      to use an expired key.

    'socket.gaierror', 'socket.herror' ... Invalid SMTP server host name or
      address.

    'socket.timeout', 'socket.error' ... SMTP server refused connection.
      Possibly invalid port or the server requires authentication or TLS.

    'smtplib.SMTPException' ... Error during SMTP communication.  The server
      may refuse the sender address of have some other problem.  The exception
      will actually be one of SMTPException subclasses (see smtplib
      documentation more details).

    """
    import smtplib
    import pytis
    msg = _compose_mail(subject, text, to, sender, sender_name=sender_name,
                        cc=cc, bcc=bcc, html=html, attachments=attachments,
                        encryption_key=encryption_key, message_id=message_id,
                        headers=headers)
    # Send the message.
    server = smtplib.SMTP(smtp_server or pytis.config.smtp_server, smtp_port)
    try:
        # TODO: May be just server.send_message(msg) on Python 2 support end.
        server.sendmail(sender, xtuple(to) + xtuple(cc) + xtuple(bcc), msg.as_string())
    finally:
        server.quit()


class SendMailError(Exception):
    """Base class for exceptions raised by 'send_mail()'."""
    def __init__(self, message, **kwargs):
        super(SendMailError, self).__init__(message)
        self.__dict__.update(kwargs)


class EncryptionKeyError(SendMailError):
    """Invalid encryption key.

    The key can not be imported.  The data may be corrupted or in an
    unsupported format.

    """
    pass


class EncryptionError(SendMailError):
    """Encryption failed by given key.

    Typically trying to use an expired key.

    """
    pass


def _compose_mail(subject, text, to, sender, sender_name=None, cc=(), bcc=(),
                  html=False, attachments=(), encryption_key=None,
                  message_id=None, headers=()):
    import email.message
    from email.header import Header
    from email.mime.multipart import MIMEMultipart
    from email.mime.audio import MIMEAudio
    from email.mime.base import MIMEBase
    from email.mime.image import MIMEImage
    from email.mime.text import MIMEText
    from email import encoders

    assert isinstance(subject, basestring), subject
    assert isinstance(text, basestring), text
    assert isinstance(to, (basestring, tuple, list)), to
    assert isinstance(sender, basestring), sender
    assert sender_name is None or isinstance(sender_name, basestring), sender_name
    assert isinstance(cc, (basestring, tuple, list)), cc
    assert isinstance(bcc, (basestring, tuple, list)), bcc
    assert isinstance(html, bool), html
    assert isinstance(attachments, (tuple, list)), attachments
    assert message_id is None or isinstance(message_id, basestring), message_id
    assert encryption_key is None or isinstance(encryption_key, basestring), encryption_key

    # Set up message headers.
    if encryption_key:
        multipart_type = 'encrypted'
    elif attachments:
        multipart_type = 'mixed'
    else:
        multipart_type = 'alternative'
    msg = MIMEMultipart(multipart_type)
    msg['Subject'] = subject
    if sender_name:
        # Encode the sender name (potentially containing non-ascii characters)
        # separately bacause if we let the message encode the whole string,
        # it will not match the desired "name" <email> format anymore and some
        # clients will not display it correctly and it will also often be
        # classified as spam.
        msg['From'] = '"{}" <{}>'.format(Header(sender_name, 'utf-8').encode(), sender)
    else:
        # Angle brackets seem to help some mail readers to display sender name from
        # their address book.
        msg['From'] = '<{}>'.format(sender)
    msg['To'] = ', '.join(xtuple(to))
    msg['Date'] = email.utils.formatdate(localtime=1)
    if cc:
        msg['Cc'] = ', '.join(xtuple(cc))
    if bcc:
        msg['Bcc'] = ', '.join(xtuple(bcc))
    if message_id:
        msg['Message-ID'] = message_id
    for header, value in headers:
        msg[header] = value

    if encryption_key:
        # Compose OpenPGP encrypted message.
        import gnupg
        try:
            # Work around gnupg > 2.1.19/Python gnupg module version incompatibility.
            gnupg._parsers.Verify.TRUST_LEVELS["ENCRYPTION_COMPLIANCE_MODE"] = 23
        except:
            pass
        message_to_encrypt = _compose_mail(subject, text, to, sender, sender_name=sender_name,
                                           cc=cc, bcc=bcc, html=html, attachments=attachments)
        msg['Content-transfer-encoding'] = '8bit'
        msg.preamble = 'This is an OpenPGP/MIME encrypted message (RFC 2440 and 3156)'
        submsg = email.message.Message()
        submsg['Content-Type'] = 'application/pgp-encrypted'
        submsg['Content-Description'] = 'PGP/MIME version identification'
        submsg.set_payload('Version: 1\n')
        msg.attach(submsg)
        content = email.message.Message()
        content.add_header('Content-Type', 'application/octet-stream', name='encrypted.asc')
        content.add_header('Content-Description', 'OpenPGP encrypted message')
        content.add_header('Content-Disposition', 'attachment', filename='encrypted.asc')
        with tempfile.NamedTemporaryFile() as tmp:
            gpg = gnupg.GPG(keyring=tmp.name, options=('--no-secmem-warning', '--always-trust',
                                                       '--no-default-keyring'))
            imported = gpg.import_keys(encryption_key)
            if not imported or not imported.fingerprints:
                raise EncryptionKeyError('No valid PGP key found in given encryption key')
            result = gpg.encrypt(message_to_encrypt.as_string(), imported.fingerprints[0])
            if not result.ok:
                raise EncryptionError('Encryption failed',
                                      status=result.status, detail=result.stderr)
        content.set_payload(str(result))
        msg.attach(content)
    else:
        # Compose message body.
        msg.attach(MIMEText(text, 'html' if html else 'plain', 'utf-8'))
        # Process the attachments.
        for attachment in attachments:
            if isinstance(attachment, basestring):
                attachment = Attachment(attachment)
            maintype, subtype = attachment.mime_type.split('/', 1)
            if maintype == 'text':
                submsg = MIMEText(attachment.data, subtype, 'utf-8')
            elif maintype == 'image':
                submsg = MIMEImage(attachment.data, subtype)
            elif maintype == 'audio':
                submsg = MIMEAudio(attachment.data, subtype)
            else:
                submsg = MIMEBase(maintype, subtype)
                submsg.set_payload(attachment.data)
                encoders.encode_base64(submsg)
            submsg.add_header('Content-Disposition', 'attachment', filename=attachment.filename)
            msg.attach(submsg)
    return msg

# Miscellaneous


UNDEFINED = object()
"""Object representing an undefined value.

Typically used as a default value of optional arguments to avoid the need to
define and check using **kwargs.

"""


# Debugging functions


def debugger():
    """Vyvolej interaktivní debugger.

    Užitečné pouze pro ladění.

    """
    import pdb
    pdb.set_trace()


_mem_info = None


def mem_info():
    """Vypiš na standardní chybový výstup informaci o paměti.

    Užitečné pouze pro ladění.

    """
    global _mem_info
    if _mem_info is None:
        class MemInfo(object):

            def __init__(self):
                self._length = 0
                self._report_length = 1
                self._count = Counter()

            def update(self):
                glen = len(gc.garbage)
                if glen != self._length:
                    nlen = gc.collect()
                    sys.stderr.write(
                        'Pending data length: %s; uncollectable: %s\n' %
                        (glen, nlen))
                    self._length = glen
                    if glen > self._report_length:
                        sys.stderr.write('Pending data: %s\n' % gc.garbage)
                        self._report_length = 2 * glen
        _mem_info = MemInfo()
    _mem_info.update()


def ipython():
    """Vyvolej embedded IPython."""
    try:
        from IPython.Shell import IPShellEmbed
    except ImportError:
        sys.stderr.write('IPython not available\n')
        return
    args = ['-pi1', 'In2<\\#>: ', '-pi2', '   .\\D.: ',
            '-po', 'Out<\\#>: ', '-nosep']
    ipshell = IPShellEmbed(
        args,
        banner='---\nEntering IPython, hit Ctrl-d to continue the program.',
        exit_msg='Leaving IPython.\n---')
    locals = inspect.currentframe().f_back.f_locals
    ipshell(locals)


def deepstr(obj):
    """Return a string form of 'obj'.

    If 'obj' is a sequence, apply the function on it recursively.  The function
    is intended to be primarily used in logging, for various purposes.

    """
    if is_sequence(obj):
        template = u'(%s,)' if isinstance(obj, tuple) else u'[%s]'
        transformed = template % (', '.join(deepstr(x) for x in obj),)
    elif isinstance(obj, unistr):
        transformed = u'"%s"' % (obj.replace('"', '\\"'),)
    elif isinstance(obj, str):
        transformed = '"%s"' % (obj.replace('"', '\\"'),)
    else:
        transformed = obj
    if sys.version_info[0] == 2:
        try:
            result = unistr(transformed)
        except UnicodeEncodeError:
            result = transformed.encode('unicode_escape')
        except Exception:
            try:
                result = unistr(repr(transformed))
            except Exception:
                result = '<<unicode conversion error>>'
    else:
        result = str(transformed)
    return result


def format_traceback():
    """Vrať zformátovaný traceback aktuální výjimky, jako string."""
    import traceback
    einfo = __, einstance, tb = sys.exc_info()
    tblist = traceback.format_exception(*einfo)
    tbstring = ''.join(tblist)
    return tbstring


def exception_info(einfo=None):
    """Vrať podrobný výpis informací o aktuální výjimce, jako string.

    Tento výpis je založen na funkcích modulu 'cgitb', avšak místo HTML vrací
    obyčejný textový string.

    Argumenty:

      einfo -- informace o výjimce ve tvaru vraceném funkcí 'sys.exc_info()',
        nebo 'None' (v kterémžto případě je tato informace získána automaticky)

    """
    # Inicializace
    etype, evalue, etb = einfo or sys.exc_info()
    context = 5
    import os
    import time
    import traceback
    import linecache
    import inspect
    # Sestavení hlavičky
    if inspect.isclass(etype):
        etype = etype.__name__
    date = time.ctime(time.time())
    head = '%s, %s\n' % (str(etype), date)
    indent = ' ' * 5
    # Frames
    frames = []
    records = inspect.getinnerframes(etb, context)
    for frame, file, lnum, func, lines, index in records:
        file = file and os.path.abspath(file) or '?'
        args, varargs, varkw, locals = inspect.getargvalues(frame)
        call = ''
        if func != '?':
            call = ('in ' + func +
                    inspect.formatargvalues(args, varargs, varkw, locals,
                                            formatvalue=lambda value: '=' + deepstr(value)))
            highlight = {}

        def reader(lnum=[lnum]):
            highlight[lnum[0]] = 1
            try:
                return linecache.getline(file, lnum[0])
            finally:
                lnum[0] = lnum[0] + 1
        vars = cgitb.scanvars(reader, frame, locals)
        rows = ['%s %s\n' % (file, call)]
        if index is not None:
            i = lnum - index
            for line in lines:
                num = ' ' * (5 - len(str(i))) + str(i) + ' '
                line = '%s%s' % (num, line)
                if i in highlight:
                    rows.append('=> ' + line)
                else:
                    rows.append('   ' + line)
                i = i + 1
        done, dump = {}, []
        for name, where, value in vars:
            if name in done:
                continue
            done[name] = True
            if value is not cgitb.__UNDEF__:
                if where == 'global':
                    name = 'global ' + name
                elif where == 'local':
                    pass
                else:
                    name = where + name.split('.')[-1]
                dump.append('%s = %s' % (name, deepstr(value)))
            else:
                dump.append(name + ' undefined')
        rows.append(', '.join(dump))
        frames.append(' '.join(rows) + '\n')
    exception = ['%s: %s' % (str(etype), str(evalue))]
    if not inspect.isclass(evalue):
        for name in dir(evalue):
            value = deepstr(getattr(evalue, name))
            exception.append('\n%s%s =\n%s' % (indent, name, value))
    return (head + '\n' + ' '.join(traceback.format_exception(etype, evalue, etb)) +
            '\n' + ' '.join(frames) +
            '\n' + ' '.join(exception))


def stack_info(depth=None):
    """Vrať obsah zásobníku volání, jako string.

    String je zformátovaný podobně jako Pythonový traceback.  Poslední volání
    je na konci.  Argument 'depth' může omezit hloubku jen na určitý počet
    frames.

    Funkce je typicky určena k ladění.

    """
    stack = inspect.stack()[1:]
    if depth is not None:
        stack = stack[:depth]
    stack.reverse()
    return "\n".join(['  File "%s", line %d, in %s' % frame[1:4] +
                      (frame[5] and ':\n    %s' % frame[5] or '')
                      for frame in stack])


def lcg_node(content, title=None, resource_path=(), resources=()):
    """Return lcg.ContentNode for given content with given resources.

    Arguments:

      content -- 'lcg.Content' instance or a sequence of such instances.
      resource_path -- sequence of filesystem directory names where resource
        files referred from the document (images, style sheets, scripts) are
        searched.  The LCG's resource directory is appended automatically.  The
        arrangement of files in resource directories must follow the standard
        expected by 'lcg.ResourceProvider'.
      resources -- list of 'lcg.Resource' instances or string resource file
        names.  The 'lcg.Resource' instances will be passed to the resource
        provider as statically defined resources.  The string names will be
        allocated through the resource provider (searched within
        'resource_path').

    The content is returned as an 'lcg.ContentNode' instance.

    """
    import lcg
    import os
    lcg_dir = os.path.dirname(os.path.dirname(os.path.dirname(lcg.__file__)))
    resource_path = list(resource_path)
    resource_path.append(os.path.join(lcg_dir, 'resources'))
    resource_provider = lcg.ResourceProvider(
        dirs=resource_path,
        resources=[r for r in resources if isinstance(r, lcg.Resource)],
    )
    for r in resources:
        if not isinstance(r, lcg.Resource):
            assert isinstance(r, basestring)
            resource_provider.resource(r)
    return lcg.ContentNode('', title=title, content=content, resource_provider=resource_provider)


def parse_lcg_text(text, resource_path=(), resources=()):
    """Return lcg.ContentNode created by parsing given LCG Structured Text.

    Arguments:

      text -- The source text in LCG structured text format.
      resource_path, resources -- as in 'lcg_node()' above.

    The content is returned as an 'lcg.ContentNode' instance.

    """
    import lcg
    content = lcg.Parser().parse(text)
    return lcg_node(content=content, resource_path=resource_path, resources=resources)

def content(content, format=None, resources=()):
    """Return lcg.ContentNode created from given content.

    Arguments:

      content -- content as a string, 'lcg.Content' instance or a sequence of
        'lcg.Content' instances.  'lcg.Content' instance is returned as is,
        sequence of 'lcg.Content' instances is wrapped in a newly created
        'lcg.Container' instance (passing it given 'resources') and a string is
        converted to 'lcg.Content' according to the 'format' argument value.
      format -- input format of the text content as one of 'TextFormat'
        constants.  'TextFormat.PLAIN' for preformatted plain text,
        'TextFormat.HTML' for HTML source fragment (excluding <html>, <head>
        and <body> tags) or 'TextFormat.LCG' for LCG structured text source to
        be processed by LCG Parser.  This argument is irrelevant if content is
        not given as a string.  If None, the default format is
        'TextFormat.LCG'.
      resources -- list of 'lcg.Resource' instances or string resource file
        names.  The 'lcg.Resource' instances will be passed to the resource
        provider as statically defined resources.  The string names will be
        allocated through the resource provider (searched within
        'resource_path').

    The content is returned as an 'lcg.ContentNode' instance.

    """
    import lcg
    if isinstance(content, basestring):
        from pytis.presentation import TextFormat
        if format is None or format == TextFormat.LCG:
            content = lcg.Container(lcg.Parser().parse(content), resources=resources)
        elif format == TextFormat.PLAIN:
            assert not resources, resources
            content = lcg.PreformattedText(content)
        elif format == TextFormat.HTML:
            assert not resources, resources
            content = lcg.HtmlContent(content)
        else:
            raise ProgramError('Invalid content format:', format)
    else:
        assert format is None, format
        if isinstance(content, (tuple, list)) or resources:
            content = lcg.Container(content, resources=resources)
        else:
            assert isinstance(content, lcg.Content), content
    return content


def lcg_to_html(text, styles=('default.css',), resource_path=()):
    """Return given LCG structured text converted to HTML.

    Arguments:

      text -- The source text in LCG structured text format.
      styles -- sequence of style sheet file names to be embedded as inline
        styles in the final document.  These files must be located in resource
        directories specified by 'resource_path'.
      resource_path -- sequence of filesystem directory names where resource
        files (style sheets) are searched.  The LCG's resource directory is
        appended automatically.  The arrangement of files in resource
        directories must follow the standard expected by
        'lcg.ResourceProvider'.

    The exported HTML is returned as UTF-8 encoded string.

    """
    import lcg

    class Exporter(lcg.StyledHtmlExporter, lcg.HtmlExporter):
        def _head(self, context):
            g = self._generator
            return super(Exporter, self)._head(context) + \
                [g.meta(http_equiv='Content-Type', content='text/html; charset=utf-8')]

    node = parse_lcg_text(text, resource_path=resource_path, resources=styles)
    exporter = Exporter(inlinestyles=True)
    context = exporter.context(node, None)
    html = exporter.export(context)
    if sys.version_info[0] == 2:
        html = html.encode('utf-8')
    return html


def html_diff(text1, text2, name1, name2, wrapcolumn=80, context=True, numlines=3):
    """Return a human readable overview of differences between given two texts.

    Arguments:
      text1 -- first text as a basestring
      text2 -- second text as a basestring
      name1 -- name of the first text as a basestring
      name2 -- name of the second text as a basestring
      wrapcolumn -- column to wrap longer lines in both texts as int or None
      context -- a context diff is returned if true, full diff otherwise
      numlines -- number of lines before and after change to show in context diff

    Returns a string containing a complete HTML document.

    """
    import difflib
    diff = difflib.HtmlDiff(wrapcolumn=wrapcolumn)
    result = diff.make_file(text1.splitlines(), text2.splitlines(), name1, name2,
                            context=context, numlines=numlines)

    _ = translations('pytis-wx')
    for src, dst, context in (
        # Localize some strings and hack the style sheet.
        ('Colors', _("Colors"), '<th> %s </th>'),
        ('Legends', _("Legends"), '> %s </th>'),
        ('&nbsp;Added&nbsp;', _("Added"), '<td class="diff_add">%s</td>'),
        ('Changed', _("Changed"), '<td class="diff_chg">%s</td>'),
        ('Deleted', _("Deleted"), '<td class="diff_sub">%s</td>'),
        ('font-family:Courier', 'font-size:0.9em;cell-padding:2px',
         'table.diff {%s; border:medium;}'),
        ('background-color:#c0c0c0', 'display:none', '.diff_next {%s}'),
    ):
        result = result.replace(context % src, context % dst)
    return re.sub('<td> <table border="" summary="Links">(.|[\r\n])*</table></td>', '', result)


_current_language = None


def current_language():
    """Return current language code as string.

    If current language is not set, set it to the current environment language.

    """
    if _current_language is None:
        set_current_language(environment_language())
    return _current_language


def set_current_language(language):
    """Set current language to 'language'.

    Arguments:

      language -- language code (without any variant), string

    """
    global _current_language
    _current_language = language


def environment_language(default=None):
    """Return code of the language of the current locale environment.

    Arguments:

      default -- default language code; string or 'None'

    Just the basic code, without any variant, is returned.

    """
    for env in ('LANGUAGE', 'LC_ALL', 'LC_MESSAGES', 'LANG'):
        locale = os.getenv(env)
        if locale:
            if locale != 'C':
                lang = locale.split('_')[0]
            else:
                lang = default
            break
    else:
        lang = default
    return lang


def translation_status():
    """Return the current status of translations in all available translation files.

    Returns a list of dictionaries, where each dictionary contains the
    following information:

       filename -- translation file name as a string (such as 'pytis-wx.en.po'),
       percent_translated -- percent of translated entries as integer
       count_untranslated -- number of untranslated entries as integer
       count_fuzzy -- number of fuzzy entriues as integer

    The list contains an entry for every PO file found within the current
    translation path (see 'translation_path()').  Note, that the actual
    translations visible within the application may not exactly correspond to
    returned information, because they are retrieved from MO files (compiled PO
    files) but the returned information is read from the PO files directly.

    """
    import glob
    import os
    import polib
    info = []
    for directory in translation_path():
        for path in glob.glob(os.path.join(directory, '*.*.po')):
            po = polib.pofile(path)
            info.append(dict(filename=os.path.split(path)[1],
                             percent_translated=po.percent_translated(),
                             count_untranslated=len(po.untranslated_entries()),
                             count_fuzzy=len(po.fuzzy_entries()),
                             ))
    return info


def translation_path():
    """Return the current translation path as a list of strings.

    Individual strings are names of directories containing translations.  The
    list is currently created from the environment variable
    PYTIS_TRANSLATION_PATH (which contains the directory names separated by
    colons).  When PYTIS_TRANSLATION_PATH is not set, the path contains the
    default path relative to the source files as they are organized within the
    source directory.

    """
    # Note: We can't make this a configuration option, because we need the value
    # sooner than the configuration options are initialized as translations() are
    # typically called at the top of each source file.
    path_env = os.getenv('PYTIS_TRANSLATION_PATH')
    if path_env:
        path = path_env.split(':')
    else:
        from os.path import dirname
        path = (os.path.join(dirname(dirname(dirname(dirname(__file__)))), 'translations'),)
    return path


def translations(domain, origin='en'):
    """Create 'lcg.TranslatedTextFactory' for the current locale.

    Used to define the '_' symbol in modules which define translatable user
    interface strings.

    The class 'lcg.TranslatedTextFactory' produces instances of strings, which
    are translated to the current locale, but may be also translated later into
    any of the other supported locales when used properly.  This is necessary
    for those parts of pytis, which define translatable strings which may be
    used both in web and desktop applications (desktop applications expect
    strings translated to the current locale, web applications need to
    translate the strings later when a particular client is served).

    """
    try:
        import lcg
    except ImportError:
        import gettext

        class Translator(object):
            """Implement 'lcg.TranslatedTextFactory' interface using pure gettext."""

            def __init__(self, domain, path):
                self._gettext = gettext.translation(domain, path[0], fallback=True)
                if sys.version_info[0] == 2:
                    self._gettext.gettext = self._gettext.ugettext
                    self._gettext.ngettext = self._gettext.ungettext

            def _interpolate(self, text, *args, **kwargs):
                values = args or kwargs
                if values:
                    text %= values
                return text

            def __call__(self, text, *args, **kwargs):
                return self._interpolate(self._gettext.gettext(text), *args, **kwargs)

            def ngettext(self, singular, plural, *args, **kwargs):
                if args:
                    n = args[0]
                else:
                    n = kwargs['n']
                return self._interpolate(self._gettext.ngettext(singular, plural, n),
                                         *args, **kwargs)

            def pgettext(self, context, text, *args, **kwargs):
                return self.__call__(context + '\x04' + text, *args, **kwargs).split('\x04', 1)[-1]

        return Translator(domain, translation_path())
    else:
        path = translation_path()
        lang = environment_language(default=origin)
        return lcg.TranslatedTextFactory(domain, origin=origin, lang=lang, translation_path=path)


def translate(text):
    """Return translation object for given text.

    This function is suitable for use as '_' to mark translatable texts.  It is
    now used in applications to quickly define the '_' function explicitly
    after it was removed from builtins in Pytis.  To really translate the
    texts, use the 'translations()' function defined above and set up creation
    of message catalogs.

    Arguments:

      text -- text to translate; basestring

    The function just returns 'text'.

    """
    return text


def on_windows():
    """Return True iff we are currently running on Windows."""
    return platform.system() == 'Windows'


def run_as_script(function):
    """Run given function as a script in Pytis application context.

    Process command line options, initialize configuration, connect to the
    database and run given function with positional arguments matching the
    command line options which remain after processing all known pytis options.

    Returns the value returned by the function.

    The first line of function's docstring is used in the usage information
    page when --help is passed or if invalid command line options are found.

    """
    def usage(msg=None):
        descr = function.__doc__.splitlines()[0]
        sys.stderr.write("%s\nUsage: %s [options] %s\n%s\n" % (
            descr,
            os.path.split(sys.argv[0])[-1],
            ' '.join(argument_names(function)),
            "Options are Pytis command line options, such as --config or --dbhost and --dbname."
        ))
        if msg:
            sys.stderr.write(msg)
            sys.stderr.write('\n')
        sys.exit(1)

    import pytis
    import pytis.data
    if '--help' in sys.argv:
        usage()
    try:
        pytis.config.add_command_line_options(sys.argv)
    except getopt.GetoptError as e:
        usage(e.msg)
    args = sys.argv[1:]
    if len(args) != len(argument_names(function)):
        usage()

    # Disable pytis logging and notification thread (may cause troubles when creating data objects).
    pytis.config.dblisten = False
    # Disable pytis logging of data operations etc.
    pytis.config.log_exclude = [pytis.util.ACTION, pytis.util.EVENT,
                                pytis.util.DEBUG, pytis.util.OPERATIONAL]
    while True:
        try:
            return function(*args)
        except pytis.data.DBLoginException as e:
            if pytis.config.dbconnection.password() is None:
                import getpass
                login = pytis.config.dbuser
                password = getpass.getpass("Enter database password for %s: " % login)
                pytis.config.dbconnection.update_login_data(user=login, password=password)
            else:
                sys.stderr.write(e.message())
                sys.exit(1)
