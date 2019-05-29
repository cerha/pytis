# -*- coding: utf-8 -*-

# Copyright (C) 2018-2019 Tomáš Cerha <t.cerha@gmail.com>
# Copyright (C) 2001-2017 Brailcom, o.p.s.
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

from builtins import range
import functools
import cgitb
import codecs
import copy
import gc
import inspect
import operator
import os
import re
import string
import sys
import tempfile
import _thread
import types as pytypes
import unicodedata
import platform


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
        pytis.util.log(pytis.util.OPERATIONAL, 'Neoprávněný přístup', args)
        super_(InvalidAccessError).__init__(self, *args)


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
        if to_child is None:
            r_to_child, w_to_child = os.pipe()
        else:
            if isinstance(to_child, file):
                to_child = to_child.fileno()
            r_to_child, w_to_child = to_child, None
        if from_child is None:
            r_from_child, w_from_child = os.pipe()
        else:
            if isinstance(from_child, file):
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


class Tmpdir(object):
    """Třída vytvářející pro dobu své existence dočasný adresář.

    Třída zajišťuje vytvoření dočasného adresáře při svém vzniku a jeho smazání
    včetně všech souborů v něm obsažených při svém zániku.

    """

    def __init__(self, prefix='pytis', *args, **kwargs):
        """Inicializuj instanci.

        Argumenty:

          prefix -- prefix jména adresáře, string

        """
        self._tmpdir = TemporaryDirectory(prefix=prefix)
        super(Tmpdir, self).__init__(*args, **kwargs)


class TemporaryDirectory(object):
    """Create a temporary directory and delete it together with the instance.

    The directory is deleted including all its contents on instance
    destruction, but only in the process of the same pid as the one that
    created the instance.

    You can get the name of the directory using 'name()' method.

    """

    def __init__(self, prefix='tmppytis', *args, **kwargs):
        """
        Arguments:

          prefix -- directory name prefix, basestring

        """
        self._directory = tempfile.mkdtemp(prefix=prefix)
        self._pid = os.getpid()

    def name(self):
        "Return the name of the temporary directory, basestring."
        return self._directory

    def __del__(self):
        self._cleanup()

    def _cleanup(self):
        if os.getpid() == self._pid:
            for dirpath, dirnames, filenames in os.walk(self._directory, topdown=False):
                for d in dirnames:
                    try:
                        os.rmdir(os.path.join(dirpath, d))
                    except Exception:
                        pass
                for f in filenames:
                    try:
                        os.remove(os.path.join(dirpath, f))
                    except Exception:
                        pass
            try:
                os.rmdir(self._directory)
            except Exception:
                pass


class TemporaryFile(object):
    """Just like 'tempfile.NamedTemporaryFile' but with different delete rules.

    The file is by default not deleted as soon as it is closed but only after
    instance of this class is destroyed and only the process of the same pid
    as the one that created the instance.

    """

    def __init__(self, delete=False, **kwargs):
        self._file = tempfile.NamedTemporaryFile(delete=delete, prefix='tmppytis', **kwargs)
        self._pid = os.getpid()

    def __getattr__(self, name):
        return getattr(self._file, name)

    def __del__(self):
        if not self._file.delete and os.getpid() == self._pid:
            try:
                os.remove(self._file.name)
            except Exception:
                pass


class Stack(object):
    """Obecný zásobník.

    Datová struktura typu LIFO, umožňující pracovat s prvky libovolného typu.

    """

    def __init__(self):
        self._list = []

    def __str__(self):
        classname = str(self.__class__).split('.')[-1]
        contents = ', '.join(map(str, self._list))
        return '<%s contents=%s>' % (classname, contents)

    def push(self, item):
        """Přidej prvek na vrchol zásobníku.

        Argumentem může být libovolný objekt.

        """
        self._list.append(item)

    def pop(self):
        """Odeber objekt z vrcholu zásobníku.

        Při pokusu o odebrání z prázdného zásobníku vyvolej `IndexError'.

        """
        return self._list.pop()

    def top(self):
        """Vrať nejvrchnější prvek ze zásobníku.

        Pokud je zásobník prázdný, vrať None.

        """
        if self.empty():
            return None
        return self._list[-1]

    def empty(self):
        """Vrať pravdu, je-li zásobník prázdný."""
        return len(self._list) == 0


class XStack(Stack):
    """Zásobník s aktivním prvkem a dalšími rozšířenými možnostmi.

    Rozšiřuje možnosti zásobníku o:

      * aktivaci libovolného prvku
      * zjištění aktivního prvku
      * zjištění seznamu všech prvků
      * vyjmutí libovolného prvku ze zásobníku
      * zjištění pořadí posledně aktivovaných prvků (MRU)

    Omezení: V zásobníku nesmí být přítomen jeden objekt současně vícekrát,
    resp. zásobník nesmí obsahovat dva ekvivalentní prvky.  V
    takovém případě není chování zásobníku definováno.

    New elements are pushed just below the currently active element on the
    stack, not to the top of the stack as in the superclass.

    """

    def __init__(self):
        self._active = None
        self._mru = []
        super(XStack, self).__init__()

    def push(self, item):
        """Push the element just below the currently active element.

        The inserted element automatically becomes active.

        """
        if self.empty():
            super(XStack, self).push(item)
        else:
            self._list.insert(self._list.index(self.active()), item)
        self.activate(item)

    def pop(self):
        """Odeber objekt z vrcholu zásobníku.

        Při odebrání aktivního prvku se stává aktivním prvkem vrchní prvek
        zásobníku.

        """
        item = self.top()
        self._mru.remove(item)
        super(XStack, self).pop()
        if item is self._active:
            self.activate(self.top())

    def remove(self, item):
        """Remove the given 'item' from the stack.

        If 'item' is currently the active element, the following element is
        activated (or the preceding one when no such element exists).

        """
        if item is self.top():
            to_activate = self.prev()
        else:
            to_activate = self.next()
        self._list.remove(item)
        self._mru.remove(item)
        if item is self._active:
            self.activate(to_activate)

    def items(self):
        """Vrať seznam všech prvků jako tuple.

        Prvek ``top'' je poslední.

        """
        return tuple(self._list)

    def mru(self):
        """Vrať seznam prvků seřazený podle poslední aktivace.

        Aktivní prvek je první, za ním následuje prvek, který byl aktivní před
        tím, než se aktivní prvek stal aktivním, atd.

        """
        return tuple(self._mru)

    def activate(self, item):
        """Aktivuj daný prvek."""
        assert item in self._list or item is None and self.empty()
        self._active = item
        if item is not None:
            if item in self._mru:
                self._mru.remove(item)
            self._mru.insert(0, item)

    def active(self):
        """Vrať právě aktivní prvek"""
        assert self._active in self._list or (self._active is None and self.empty())
        return self._active

    def next(self):
        """Return element just below the currently active element.

        If the active element is the only element on the stack or when the
        stack is empty, return 'None'.  Otherwise, if there is nothing below
        the currently active element, return the top element.

        """
        if len(self._list) <= 1:
            return None
        i = self._list.index(self._active)
        return self._list[(i + 1) % len(self._list)]

    def prev(self):
        """Return element just above the currently active element.

        If the active element is the only element on the stack or when the
        stack is empty, return 'None'.  Otherwise, if there is nothing above
        the currently active element, return the bottom element.

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


class Structure (object):
    """Simple data structures.

    Attribute names of the instance are listed in the sequence '_attributes'.
    Each element of '_attributes' is an 'Attribute' instance.

    """
    _attributes = ()

    def __init__(self, _template=None, **kwargs):
        self._init(kwargs, template=_template)

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

    def __str__(self):
        result = '<%s:' % (self.__class__.__name__,)
        for member in self._attributes:
            name = member.name()
            result = result + (' %s=%s;' % (name, str(getattr(self, name)()),))
        result = result + '>'
        return result

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
        return not self.__eq__(other)

    __hash__ = None


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


def remove_duplicates(list, keep_order=False):
    """Vrať prvky 'list', avšak bez jejich násobných výskytů.

    Násobnost se testuje porovnáním prvků pomocí operátoru '='.

    Argumenty:

      keep_order -- při výchozí hodnotě funkce nezachovává pořadí prvků, ale
        algoritmus je optimalizován.  Pokud potřebujeme pořadí zachovat, musíme
        očekávat vyšší náročnost algoritmu.

    """
    if not list:
        return list
    if keep_order:
        result = []
        for x in list:
            if x not in result:
                result.append(x)
        return result
    else:
        result = copy.copy(list)
        result.sort()
        last = result[0]
        i = 1
        for x in result[1:]:
            if x != last:
                result[i] = last = x
                i = i + 1
        return result[:i]


def flatten(list):
    """Vrať 'list' bez vnořených sekvencí.

    Argumenty:

      list -- libovolná sekvence

    Vrací: Sekvenci tvořenou prvky sekvence 'list', přičemž každý prvek, který
      je sám sekvencí, je ve vrácené sekvenci rekurzivně nahrazen svými prvky.

    """
    result = []
    if is_sequence(list):
        result = result + functools.reduce(operator.add, map(flatten, list), [])
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


def super_(class_):
    """Vrať prvního předka třídy 'class_'."""
    return class_.__bases__[0]


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


def public_attributes(class_):
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
        return _public_attributes[class_]
    except KeyError:
        pass
    att = functools.reduce(operator.add, map(dir, _mro(class_)))
    result = tuple(remove_duplicates([s for s in att if not s or s[0] != '_']))
    _public_attributes[class_] = result
    return result


def public_attr_values(class_):
    """Return a tuple of values of all public attributes of class 'class_'.

    Just a shorthand to get the values of attributes returned by
    'public_attributes()'.

    """
    # Note: This function should actually be used in most assertions for use of
    # specification class constants.  They mostly use just public_attributes()
    # as they rely on the typical 1:1 mapping of attribute names and their
    # values.  But when this is not the case, public_attr_values() must be
    # used.
    return tuple(getattr(class_, attr) for attr in public_attributes(class_))


def argument_names(callable):
    """Return names of all function/method arguments as a tuple of strings.

    The method argument 'self' is ignored.  The names are returned in the order in which the
    arguments are defined, including all keyword arguments.  Only named arguments are taken into
    account, so any `*' and `**' arguments are ignored.

    """
    args, __, __, __ = inspect.getargspec(callable)
    if args and args[0] == 'self':
        args = args[1:]
    return tuple(args)


def direct_public_members(obj):
    """Vrať tuple všech přímých veřejných atributů a metod třídy objektu 'obj'.

    Přímými členy třídy jsou myšleny ty, které nejsou shodné se stejnojmenným
    členem některého předka třídy.  Veřejnými členy třídy jsou myšleny ty,
    jejichž název nezačíná podtržítkem.

    """
    if isinstance(obj, (pytypes.ClassType, type)):
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
    t = type(x)
    return t == pytypes.TupleType or t == pytypes.ListType


def is_dictionary(x):
    """Vrať pravdu, právě když 'x' je dictionary."""
    return type(x) == pytypes.DictionaryType


def is_string(x):
    """Vrať pravdu, právě když 'x' je běžný řetězec."""
    return isinstance(x, str)


def is_unicode(x):
    """Vrať pravdu, právě když 'x' je unicode řetězec."""
    return isinstance(x, unicode)


def is_anystring(x):
    """Vrať pravdu, právě když 'x' je unicode řetězec nebo běžný řetězec."""
    return isinstance(x, basestring)


def unormalize(unicode_):
    """Return a normalized version of 'unicode_'.

    This useful when comparing or sorting unicode's received from external
    sources (e.g. from a Web browser) which may be canonically equivalent but
    represented in different ways.

    Arguments:

      unicode_ -- unicode to normalize

    """
    return unicodedata.normalize('NFC', unicode_)


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
    """Return text encrypted using RSA 'key' and base64 encoded.

    If key is 'None', return 'text'.

    Arguments:

      key -- public key to use for encryption; string or 'None'
      text -- text to encrypt

    """
    if key:
        import Crypto.PublicKey.RSA
        import base64
        rsa = Crypto.PublicKey.RSA.importKey(key)
        encrypted = rsa.encrypt(str(text), None)[0]
        return base64.encodestring(encrypted)
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


def data_object(spec, spec_kwargs=None, kwargs=None):
    """Create a data object for given specification.

    Arguments:

      spec -- specification name as a string or a 'pytis.data.DataFactory'
        instance.
      spec_kwargs -- a dictionary of keyword arguments passed to the
        specification.
      kwargs -- a dictionary of keyword arguments passed to the data object
        constructor.  The argument 'connection_data' is added automatically
        if the data class is derived from 'pytis.data.DBData'.

    Raises 'ResolverError' or 'ProgramError' if data object creation fails.

    """
    import pytis
    import pytis.data
    if isinstance(spec, basestring):
        if spec_kwargs is None:
            spec_kwargs = {}
        factory = pytis.config.resolver.get(spec, 'data_spec', **spec_kwargs)
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


# Miscellaneous


UNDEFINED = object()
"""Objekt reprezentující nedefinovanou hodnotu.

Typicky se používá jako implicitní hodnota volitelných argumentů, aby nebylo
nutno provádět jejich definici a zkoumání prostřednictvím **kwargs.

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
    """Return unicode form of 'obj'.

    If 'obj' is a sequence, apply the function on it recursively.

    The function is intended to be primarily used in logging, for various
    purposes.

    """
    if is_sequence(obj):
        template = u'(%s,)' if isinstance(obj, tuple) else u'[%s]'
        transformed = template % (string.join(map(deepstr, obj), ', '),)
    elif isinstance(obj, unicode):
        transformed = u'"%s"' % (obj.replace('"', '\\"'),)
    elif isinstance(obj, str):
        transformed = '"%s"' % (obj.replace('"', '\\"'),)
    else:
        transformed = obj
    try:
        result = unicode(transformed)
    except UnicodeEncodeError:
        result = transformed.encode('unicode_escape')
    except Exception:
        try:
            result = unicode(repr(transformed))
        except Exception:
            result = '<<unicode conversion error>>'
    return result


def format_traceback():
    """Vrať zformátovaný traceback aktuální výjimky, jako string."""
    import traceback
    einfo = __, einstance, tb = sys.exc_info()
    tblist = traceback.format_exception(*einfo)
    tbstring = string.join(tblist, '')
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
        frames.append(string.join(rows) + '\n')
    exception = ['%s: %s' % (str(etype), str(evalue))]
    if not inspect.isclass(evalue):
        for name in dir(evalue):
            value = deepstr(getattr(evalue, name))
            exception.append('\n%s%s =\n%s' % (indent, name, value))
    return (head + '\n' +
            string.join(traceback.format_exception(etype, evalue, etb)) +
            '\n' + string.join(frames) +
            '\n' + string.join(exception))


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
      resources -- list 'lcg.Resource' instances or string resource file names.
        The 'lcg.Resource' instances will be passed to the resource provider as
        statically defined resources.  The string names will be allocated
        through the resource provider (searched within 'resource_path').

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
    return html.encode('utf-8')


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
    path_env = os.getenv('PYTIS_TRANSLATION_PATH')
    if path_env:
        path = path_env.split(':')
    else:
        base_dir = os.path.normpath(os.path.dirname(__file__) + '/../../..')
        path = (os.path.join(base_dir, 'translations'),)
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

            def _interpolate(self, text, *args, **kwargs):
                values = args or kwargs
                if values:
                    text %= values
                return text

            def __call__(self, text, *args, **kwargs):
                return self._interpolate(self._gettext.ugettext(text), *args, **kwargs)

            def ngettext(self, singular, plural, *args, **kwargs):
                if args:
                    n = args[0]
                else:
                    n = kwargs['n']
                return self._interpolate(self._gettext.ungettext(singular, plural, n),
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
