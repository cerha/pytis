# -*- coding: utf-8 -*-

# Copyright (C) 2001-2011 Brailcom, o.p.s.
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

2. Definuje symbol '_' jako exportovanou proměnnou.  To je z podobných důvodů
   jako výše a s ohledem na běžně používané konvence gettextu.

"""

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
import thread
import types as pytypes

import __builtin__
if '_' not in __builtin__.__dict__:
    __builtin__.__dict__['_'] = lambda x: x


### Třídy

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

    
class Counter:
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


_emergency_encoder = codecs.getencoder('utf-8')
def safe_encoding_write(stream, string):
    try:
        stream.write(string)
    except UnicodeEncodeError:
        string, __ = _emergency_encoder(string, 'replace')
        stream.write(string)


class Pipe:
    """Jednoduchá roura umožňující zápis a čtení stringových dat.

    Typické použití této třídy je když jedna funkce si žádá stream pro zápis,
    druhá pro čtení a je zapotřebí tyto dvě funkce propojit rourou.  Třída
    neposkytuje žádný komfort, omezuje se pouze na nejzákladnější funkce.  Je
    však thread-safe.

    """
    # Implementace této třídy byla původně jednodušší, využívala Queue.Queue.
    # To v sobě ovšem skrývalo nepříjemný výkonnostní problém: Ve frontě se
    # může ocitnout spousta krátkých řetězců a jsou-li čteny na konci všechny
    # naráz, trvá to velmi dlouho.  Bylo tedy nutné použít mechanismus, kdy
    # se nepracuje se zámky při čtení každého vloženého stringu a nesčítá se
    # mnoho krátkých stringů do jednoho velkého (je zde nepříjemná kvadratická
    # časová složitost vzhledem k počtu stringů).

    # Invarianty:
    # - vždy pracuje nejvýše jeden čtenář
    # - manipulace s bufferem kdekoliv je vždy kryta zámkem _buffer_lock
    # - zámek _empty_lock je nastaven pouze na začátku do prvního zápisu
    #   nebo volání close, a pak už nikdy není nastaven na dobu delší než
    #   okamžik
    # - zámek _read_lock smí uvolnit pouze držitel zámku _read_lock_lock
    
    def __init__(self, cc=(), encoder=None, decoder=None):
        """Inicializuj rouru.

        Argumenty:

          cc -- stream nebo sekvence streamů, do kterých budou kopírována
            všechna do roury zapisovaná data; bude-li volána metoda 'close()',
            budou uzavřeny i tyto streamy
          encoder -- if not None, it is an encoder function for output
          decoder -- if not None, it is a decoder function for input

        """
        self._cc = xtuple(cc)
        self._closed = False
        self._buffer = []
        self._buffer_lock = thread.allocate_lock()
        self._read_lock = thread.allocate_lock()
        self._empty_lock = thread.allocate_lock()
        self._empty_lock.acquire()
        self._empty_lock_lock = thread.allocate_lock()
        self._encoder = encoder
        self._decoder = decoder

    def _free_empty_lock(self):
        self._empty_lock_lock.acquire()
        try:
            if self._empty_lock.locked():
                self._empty_lock.release()
        finally:
            self._empty_lock_lock.release()

    def write(self, string_):
        """Stejné jako v případě třídy 'file'.

        Zápis po zavolání metody 'close()' vyvolá výjimku 'ValueError'.
        K témuž může dojít, pokud byl v konstruktoru specifikován kopírovací
        stream a je již uzavřen.

        """
        if self._closed:
            raise ValueError, "I/O operation on closed file"
        if self._encoder is not None:
            string_ = self._encoder(string_)[0]
        def lfunction():
            buffer = self._buffer
            if not buffer or len(buffer[-1]) > 4096:
                buffer.append(string_)
            else:
                buffer[-1] = buffer[-1] + string_
            self._free_empty_lock()
            for s in self._cc:
                safe_encoding_write(s, string_)
        with_lock(self._buffer_lock, lfunction)

    def read(self, size=-1):
        """Stejné jako v případě třídy 'file'."""
        # TODO: Z nepochopitelných důvodů je zde předávání size nutné.
        def lfunction(size=size):
            result = ''
            buffer = self._buffer
            while True:
                self._buffer_lock.acquire()
                try:
                    while buffer:
                        first = buffer[0]
                        if size < 0:
                            result = result + first
                            del buffer[0]
                        else:
                            if len(first) <= size:
                                result = result + first
                                size = size - len(first)
                                del buffer[0]
                            else:
                                result = result + first[:size]
                                buffer[0] = first[size:]
                                return result
                finally:
                    self._buffer_lock.release()
                self._empty_lock.acquire()
                self._free_empty_lock()
                if self._closed:
                    break
            if not result and size != 0:
                return None
            else:
                return result
        result = with_lock(self._read_lock, lfunction)
        if self._decoder is not None:
            result = self._decoder(result)[0]
        return result

    def close(self):
        """Stejné jako v případě třídy 'file'.

        Tato metoda přitom uzavírá pouze zápisový konec roury a cc stream
        (byl-li v konstruktoru zadán) a ponechává data pro čtení.  Uvolnit data
        lze následným zavoláním metody 'read()' bez argumentů.

        """
        self._closed = True
        self._free_empty_lock()
        for s in self._cc:
            try:
                s.close()
            except:
                pass


class Popen:
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
                if type(command) == type(''):
                    command = ['/bin/sh', '-c', command]
                for i in range(3, 256):
                    try:
                        os.close(i)
                    except:
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
        self._tmpdir = mktempdir(prefix=prefix)
        super(Tmpdir, self).__init__(*args, **kwargs)

    def __del__(self):
        self._cleanup()
        
    def _cleanup(self):
        for file_name in os.listdir(self._tmpdir):
            try:
                os.remove(os.path.join(self._tmpdir, file_name))
            except:
                pass
        try:
            os.rmdir(self._tmpdir)
        except:
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

        If 'item' is currently the active element, 

        Pokud byl vyjmutý prvek aktivním prvkem, je aktivován následující prvek
        (pokud neexistuje, tak předcházející).
        
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
        assert self._active in self._list or \
               self._active is None and self.empty()
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
        return self._list[(i+1) % len(self._list)]

    def prev(self):
        """Return element just above the currently active element.

        If the active element is the only element on the stack or when the
        stack is empty, return 'None'.  Otherwise, if there is nothing above
        the currently active element, return the bottom element.

        """
        if len(self._list) <= 1:
            return None
        i = self._list.index(self._active)
        return self._list[i-1]


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

    def __init__ (self, _template=None, **kwargs):
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
                setattr (self, name, lambda value=value: value)
            if member.mutable():
                setattr(self, 'set_'+name,
                        lambda value, name=name: self._replace_value(name, value))
        assert not kwargs, ("Extra initialization arguments", kwargs.keys())

    def _replace_value(self, name, value):
        setattr(self, name, lambda value=value: value)

    def __str__ (self):
        result = '<%s:' % (self.__class__.__name__,)
        for member in self._attributes:
            name = member.name()
            result = result + (' %s=%s;' % (name, str (getattr (self, name)()),))
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


### Funkce

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


def some(predicate, *sequences):
    """Vrať pravdu, právě když nějaký prvek 'sequences' splňuje 'predicate'.

    Argumenty:

      predicate -- funkce s počtem argumentů rovným počtu prvků 'sequences'
        vracející pravdu nebo nepravdu
      sequences -- sekvence vzájemně stejně dlouhých sekvencí, jejichž
        zazipováním vzniknou sekvence argumentů pro volání funkce 'predicate'

    """
    for elt in zip(*sequences):
        if predicate(*elt):
            return True
    else:
        return False

        
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


def safedel(object, element):
    """Aplikuj operátor 'del' na 'element' of 'object' bez signalizace chyby.

    Provádí příkaz 'del object[element]', avšak odchytává případnou výjimku
    'KeyError', resp. 'IndexError', místo ní nedělá nic.
    
    Argumenty:

      object -- dictionary nebo list, ze kterého má být odstraněn 'element'
      element -- pro 'object' dictionary libovolný objekt, který je klíčem
        'object'; pro 'object' list libovolný nezáporný integer

    Vrací: 'object'.

    """
    if isinstance(object, dict):
        try:
            del object[element]
        except KeyError:
            pass
    elif isinstance(object, list):
        try:
            del object[element]
        except IndexError:
            pass
    return object
        
    
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


if hasattr(operator, 'eq'):
    _eq = operator.eq
else:
    _eq = (lambda x, y: x == y)

def find(element, sequence, key=identity, test=_eq):
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


def starts_with(string, prefix):
    """Vrať pravdu, právě když 'string' začíná 'prefix'.

    Argumenty:

      string -- string
      prefix -- string

    """
    return string[:len(prefix)] == prefix


def super_(class_):
    """Vrať prvního předka třídy 'class_'."""
    return class_.__bases__[0]


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


def next_subclass(class_, instance):
    """Vrať potomka následujícího 'class_' v hierarchii dědičnosti 'instance'.

    Pokud má třída 'instance' atribut '__mro__', je použit tento.  V opačném
    případě je tento atribut třídy vytvořen prohledáváním předků 'instance' do
    hloubky.  'instance' musí být instancí 'class_'.

    Vrací: Odpovídající třídu; pokud taková není tak 'None'.
    
    """
    iclass = instance.__class__
    try:
        mro = iclass.__mro__
    except AttributeError:
        mro = _mro(iclass)
        iclass.__mro__ = mro
    i = position(class_, mro)
    if i is None or i == len(mro) - 1:
        result = None
    else:
        result = mro[i+1]
    return result


def sameclass(o1, o2, strict=False):
    """Vrať pravdu, právě když 'o1' a 'o2' jsou instance téže třídy.

    Je-li argument 'strict' pravdivý, musí se rovnat třídy obou objektů 'o1' a
    'o2' ve smyslu operátoru '=='.  V opačném případě postačí rovnost jmen tříd
    a jejich modulů.

    """
    try:
        c1 = o1.__class__
        c2 = o2.__class__
    except:
        return False
    if c1 == c2:
        return True
    else:
        if strict:
            return False
        else:
            return c1.__name__ == c2.__name__ and \
                   c1.__module__ == c2.__module__


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
    attrs = map(dir, _mro(class_))
    att = functools.reduce(operator.add, attrs)
    public = tuple(filter(lambda s: not s or s[0] != '_', att))
    result = remove_duplicates(list(public))
    result = tuple(result)
    _public_attributes[class_] = result
    return result

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
    if isinstance(obj, (pytypes.ClassType, type,)):
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


def compare_objects(o1, o2):
    """Porovnej 'o1' a 'o2' a vrať výsledek.

    Výsledek odpovídá pravidlům pro special metodu '__cmp__'.

    Pro porovnání platí následující pravidla:

    - Jestliže jsou oba objekty 'None', rovnají se.

    - Jestliže jeden z objektů je instance třídy a druhý není instancí třídy,
      instance je větší.

    - Jestliže oba objekty jsou instance různých tříd, vrátí se výsledek
      porovnání 'id' jejich tříd.

    - Neplatí-li žádná z předchozích podmínek, vrátí se výsledek volání
      'cmp(o1, o2)'.
      
    """
    try:
        c1 = o1.__class__
    except:
        c1 = None
    try:
        c2 = o2.__class__
    except:
        c2 = None
    if c1:
        if c2:
            if c1 == c2:
                return cmp(o1, o2)
            elif id(c1) < id(c2):
                return -1
            else:
                return 1
        else:
            return 1
    else:
        if c2:
            return -1
        else:
            return cmp(o1, o2)


def compare_attr(self, other, attributes):
    """Vrať celkový výsledek porovnání atributů objektů 'self' a 'other'.

    Funkce porovnává třídy objektů 'self' a 'other' a v případě shody pak
    uvedené atributy.  Návratová hodnota se řídí pravidly pro funkci 'cmp'.

    Argumenty:

      self, other -- instance tříd
      attributes -- sekvence jmen atributů instancí (strings), které mají být
        porovnávány

    Funkce je typicky určena k použití v metodě '__cmp__'.

    """
    if sameclass(self, other):
        sdict = self.__dict__
        odict = other.__dict__
        for a in attributes:
            s, o = sdict[a], odict[a]
            if s < o:
                return -1
            elif s > o:
                return 1
        else:
            return 0
    else:
        return compare_objects(self, other)


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
    return functools.reduce(operator.xor, map(lambda a: h(dict[a]), attributes))


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


if __debug__:
    _active_locks = None
    _with_lock_lock = thread.allocate_lock()
def with_lock(lock, function):
    """Call 'function' as protected by 'lock'.

    Arguments:

      lock -- 'thread.lock' instance to be used for locking
      function -- function of no arguments, the function to be called

    The return value is the return value of the function call.

    It is recommended to use this function instead of direct locking for the
    following reasons:

    - The calling locking code is somewhat shorter and safer.

    - It is possible to wrap locking with other code in this function, as is
      useful e.g. when debugging.

    - This function may perform additional checks for deadlock prevention, etc.

    """
    if __debug__:
        _with_lock_lock.acquire()
        try:
            thread_id = thread.get_ident()
            global _active_locks
            if _active_locks is None:
                _active_locks = {}
            locks = _active_locks.get(thread_id, [])
            if lock in locks:
                raise Exception ('Deadlock detected')
            locks.append(lock)
            _active_locks[thread_id] = locks
        finally:
            _with_lock_lock.release()
    lock.acquire()
    try:
        return function()
    finally:
        lock.release()
        if __debug__:
            _with_lock_lock.acquire()
            try:
                _active_locks[thread_id].remove(lock)
            finally:
                _with_lock_lock.release()
            

def with_locks(locks, function):
    """The same as 'with_lock' except multiple locks are given.

    'locks' is a sequence of locks to be applied in the given order.
    """
    if not locks:
        return_value = function()
    else:
        lock = locks[0]
        def lfunction():
            return with_locks(locks[1:], function)
        return_value = with_lock(lock, lfunction)
    return return_value

    
class _Throw(Exception):
    """Výjimka pro nelokální přechody."""
    
    def __init__(self, tag, value):
        """Inicializuj instanci.

        Argumenty:

          tag -- string identifikující přechod
          value -- návratová hodnota přechodu, libovolný objekt

        """
        Exception.__init__(self)
        self._tag = tag
        self._value = value

    def tag(self):
        """Vrať tag zadané v konstruktoru."""
        return self._tag

    def value(self):
        """Vrať hodnotu 'value' zadanou v konstruktoru."""
        return self._value

def catch(tag, function, *args, **kwargs):
    """Volej 'function' s ošetřením nelokálního přechodu.

    Argumenty:

      tag -- string identifikující přechod
      function -- funkce, která má být zavolána
      args -- argumenty 'function'
      kwargs -- klíčované argumenty 'function'

    Jsou ošetřeny pouze přechody s tagem 'tag', ostatní odchyceny nejsou.
    
    Vrací: Nedošlo-li k přechodu, je vrácena návratová hodnota 'function'.
    Došlo-li k přechodu, je vrácena hodnota z přechodu předaná funkci 'throw_'.

    Viz též funkce 'throw_'.
      
    """
    try:
        result = function(*args, **kwargs)
    except _Throw as e:
        if e.tag() == tag:
            result = e.value()
        else:
            raise
    return result
    
def throw(tag, value=None):
    """Vyvolej nelokální přechod identifikovaný 'tag'.

    Argumenty:

      tag -- string identifikující přechod
      value -- návratová hodnota přechodu, libovolný objekt

    Viz též funkce 'catch'.

    """
    raise _Throw(tag, value)


def copy_stream(input, output, close=False, in_thread=False, _catch=False):
    """Zkopíruj data ze streamu 'input' do streamu 'output'.

    Počáteční pozice ve streamech nejsou nijak nastavovány, to je starostí
    volajícího.  Je-li argument 'close' pravdivý, je stream 'output' po
    ukončení kopírování uzavřen; v opačném případě není uzavřen žádný stream.

    """
    if in_thread:
        return thread.start_new_thread(copy_stream, (input, output),
                                       {'close': close, '_catch': True})
    try:
        try:
            import pytis.util
            DEBUG = pytis.util.DEBUG
            log = pytis.util.log
            if __debug__:
                log(DEBUG, 'Kopíruji stream:', (input, output))
            while True:
                data = input.read(4096)
                if not data:
                    break
                try:
                    if output.closed:
                        return
                except AttributeError:
                    pass
                safe_encoding_write(output, data)
            if __debug__:
                log(DEBUG, 'Stream zkopírován:', (input, output))
        except:
            if not _catch:
                raise
    finally:
        if close:
            try:
                output.close()
            except:
                pass


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
    import config
    global _mktempdir_counter
    if _mktempdir_counter is None:
        _mktempdir_counter = Counter()
    pattern = os.path.join(config.tmp_dir,
                           '%s%d.%%d' % (prefix, os.getpid()))
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


def in_x():
    """Vrať pravdu, právě když je k dispozici prostředí X Window."""
    return os.getenv('DISPLAY')


def format_byte_size(size):
    """Return a human readable string representing given int bytesize."""
    size = float(size)
    units = ('B', 'kB', 'MB', 'GB')
    i = 0
    while size >= 1024 and i < len(units)-1:
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



# Různé


UNDEFINED = object()
"""Objekt reprezentující nedefinovanou hodnotu.

Typicky se používá jako implicitní hodnota volitelných argumentů, aby nebylo
nutno provádět jejich definici a zkoumání prostřednictvím **kwargs.

"""


# Funkce pro ladění


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
        class MemInfo:
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
    args = ['-pi1','In2<\\#>: ','-pi2','   .\\D.: ',
            '-po','Out<\\#>: ','-nosep']
    ipshell = IPShellEmbed (
        args,
        banner='---\nEntering IPython, hit Ctrl-d to continue the program.',
        exit_msg='Leaving IPython.\n---')
    locals = inspect.currentframe().f_back.f_locals
    ipshell(locals)


def deepstr(obj):
    """Vrať stringovou podobu 'obj'.

    Je-li 'obj' sekvence, aplikuj se rekurzivně na její prvky.

    """
    if is_sequence(obj):
        result = map(deepstr, obj)
        if isinstance(obj, tuple):
            result = tuple(result)
        result = unicode(result)
    elif type(obj) is type(object) or type(obj) is pytypes.ClassType:
        try:
            result = unicode(obj)
        except:
            # If the class defines __unicode__ method, the call above throws an
            # exception
            result = '%s.%s' % (obj.__module__, obj.__name__,)
    else:
        result = unicode(obj)
    try:
        return str(result)
    except UnicodeEncodeError:
        return result.encode('unicode_escape')


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
    import os, types, time, traceback, linecache, inspect
    # Sestavení hlavičky
    if type(etype) is types.ClassType:
        etype = etype.__name__
    date = time.ctime(time.time())
    head =  '%s, %s\n' % (str(etype), date)
    indent = ' ' * 5
    # Frames
    frames = []
    records = inspect.getinnerframes(etb, context)
    for frame, file, lnum, func, lines, index in records:
        file = file and os.path.abspath(file) or '?'
        args, varargs, varkw, locals = inspect.getargvalues(frame)
        call = ''
        if func != '?':
            call = 'in ' + func + \
                inspect.formatargvalues(args, varargs, varkw, locals,
                    formatvalue=lambda value: '=' + deepstr(value))
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
                num = ' ' * (5-len(str(i))) + str(i) + ' '
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
    if type(evalue) is types.InstanceType:
        for name in dir(evalue):
            value = deepstr(getattr(evalue, name))
            exception.append('\n%s%s =\n%s' % (indent, name, value))
    return head + '\n' + \
           string.join(traceback.format_exception(etype, evalue, etb)) + \
           '\n' + string.join(frames) + \
           '\n' + string.join(exception)


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
    return "\n".join(['  File "%s", line %d, in %s' % frame[1:4] + \
                      (frame[5] and ':\n    %s' % frame[5] or '')
                      for frame in stack])

def positive_id(obj):
    """Return id(obj) as a non-negative integer."""
    result = id(obj)
    if result < 0:
        # This is a puzzle:  there's no way to know the natural width of
        # addresses on this box (in particular, there's no necessary
        # relation to sys.maxint).  Try 32 bits first (and on a 32-bit
        # box, adding 2**32 gives a positive number with the same hex
        # representation as the original result).
        result += 1L << 32
        if result < 0:
            # Undo that, and try 64 bits.
            result -= 1L << 32
            result += 1L << 64
            assert result >= 0 # else addresses are fatter than 64 bits
    return result

