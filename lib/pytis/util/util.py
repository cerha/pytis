# -*- coding: iso-8859-2 -*-

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

"""R�zn� u�ite�n� pom�cky usnad�uj�c� psan� pythonov�ch program�.

Modul obsahuje v�cem�n� trivi�ln� funkce, kter� sv�m charakterem nepat�� jinam
a kter� slou�� prim�rn� pro zjednodu�en� z�pisu �asto u��van�ch konstrukc�.
Pokud se n�jak� t�matick� okruh pom�cek rozmno��, m��e b�t p�esunut do
samostatn�ho modulu.

Tento modul je v�jime�n� ve dvou sm�rech:

1. Vzhledem k�trivi�ln�mu charakteru zde obsa�en�ch funkc� a vzhledem k�tomu,
   �e jejich prim�rn�m ��elem je zkr�tit a z�itelnit k�d, je povoleno jej
   importovat n�sleduj�c�m zp�sobem:
   
     from util import *

2. Definuje symbol '_' jako exportovanou prom�nnou.  To je z�podobn�ch d�vod�
   jako v��e a s�ohledem na b�n� pou��van� konvence gettextu.

"""

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


### T��dy

class ProgramError(Exception):
    """V�jimka signalizuj�c� programovou chybu.

    Programov� chyba je chyba, kter� by teoreticky nem�la nikdy nastat.
    Vznikla chybou programu, a� u� p��mo v�m�st�, kde je detekov�na, nebo
    chybn�m vol�n�m k�du zvn� (nap��klad nedodr�en� typ� argument� metody).

    Programovou chybou naopak nen� syst�mov� chyba, jej� vznik lze za ur�it�ch
    okolnost� o�ek�vat, ani chyba zp�soben� akcemi u�ivatele (nap��klad chybn�
    zadan� data na vstupu).

    Tato v�jimka by nem�la b�t odchyt�v�na, s�v�jimkou funkc� pro o�et�en�
    hav�rie programu.  Jej� v�skyt znamen�, �e program se dostal do
    nedefinovan�ho stavu a m�l by b�t ukon�en.  (V�ur�it�ch, v�dokumenta�n�ch
    �et�zc�ch jasn� definovan�ch, p��padech tento po�adavek nemus� b�t striktn�
    a m��e znamenat pouze lok�ln� zhroucen� t�kaj�c� se ur�it�ho modulu,
    p��padn� i�s�mo�nost� uzdraven� reinicializac�.)

    V�jimka pouze d�d� obecnou v�jimkovou t��du a nedefinuje nic nov�ho.
    
    """
    pass


class InvalidAccessError(Exception):
    """Signalizace neautorizovan�ho p��stupu.

    Tato v�jimka je typicky vyvol�v�na na stran� vzd�len�ho serveru, pokud se
    klient pokou�� volat vzd�lenou metodu bez pot�ebn� dodate�n� autorizace
    nebo s�argumenty chybn�ch typ�.

    """
    def __init__(self, *args):
        import pytis.util
        pytis.util.log(pytis.util.OPERATIONAL, 'Neopr�vn�n� p��stup', args)
        super_(InvalidAccessError).__init__(self, *args)


class FileError(Exception):
    """V�jimka vyvol�van� po chyb� p�i pr�ci se soubory.

    Nejedn� se o�duplik�t 'os.OSError', pou��v� se nap��klad pokud nelze
    z�n�jak�ho zvl�tn�ho d�vodu vytvo�it do�asn� soubor.

    """
    pass


class NotImplementedException(Exception):
    """Exception raised on calling unimplemented methods.
    """
    pass

    
class Counter:
    """Jednoduch� ��ta�.

    Po sv�m vytvo�en� je inicializov�n na hodnotu 0 nebo hodnotu zadanou v konstruktoru
    a p�i ka�d�m �ten� metodou 'next()' je tato hodnota zv��ena.

    T��da nen� thread-safe.

    """
    def __init__(self, value=0):
        """Inicializuj instanci."""
        self._value = value

    def next(self):
        """Zvy� hodnotu ��ta�e o�1 a vra� ji."""
        self._value = self._value + 1
        return self._value

    def current(self):
        """Vra� aktu�ln� hodnotu ��ta�e bez jej�ho zv��en�."""
        return self._value
        
    def reset(self):
        """Nastav hodnotu ��ta�e na 0."""
        self._value = 0


_emergency_encoder = codecs.getencoder('iso-8859-2')
def safe_encoding_write(stream, string):
    try:
        stream.write(string)
    except UnicodeEncodeError:
        string, __ = _emergency_encoder(string, 'replace')
        stream.write(string)


class Pipe:
    """Jednoduch� roura umo��uj�c� z�pis a �ten� stringov�ch dat.

    Typick� pou�it� t�to t��dy je kdy� jedna funkce si ��d� stream pro z�pis,
    druh� pro �ten� a je zapot�eb� tyto dv� funkce propojit rourou.  T��da
    neposkytuje ��dn� komfort, omezuje se pouze na nejz�kladn�j�� funkce.  Je
    v�ak thread-safe.

    """
    # Implementace t�to t��dy byla p�vodn� jednodu���, vyu��vala Queue.Queue.
    # To v�sob� ov�em skr�valo nep��jemn� v�konnostn� probl�m: Ve front� se
    # m��e ocitnout spousta kr�tk�ch �et�zc� a jsou-li �teny na konci v�echny
    # nar�z, trv� to velmi dlouho.  Bylo tedy nutn� pou��t mechanismus, kdy
    # se nepracuje se z�mky p�i �ten� ka�d�ho vlo�en�ho stringu a nes��t� se
    # mnoho kr�tk�ch string� do jednoho velk�ho (je zde nep��jemn� kvadratick�
    # �asov� slo�itost vzhledem k�po�tu string�).

    # Invarianty:
    # - v�dy pracuje nejv��e jeden �ten��
    # - manipulace s�bufferem kdekoliv je v�dy kryta z�mkem _buffer_lock
    # - z�mek _empty_lock je nastaven pouze na za��tku do prvn�ho z�pisu
    #   nebo vol�n� close, a�pak u� nikdy nen� nastaven na dobu del�� ne�
    #   okam�ik
    # - z�mek _read_lock sm� uvolnit pouze dr�itel z�mku _read_lock_lock
    
    def __init__(self, cc=()):
        """Inicializuj rouru.

        Argumenty:

          cc -- stream nebo sekvence stream�, do kter�ch budou kop�rov�na
            v�echna do roury zapisovan� data; bude-li vol�na metoda 'close()',
            budou uzav�eny i�tyto streamy

        """
        self._cc = xtuple(cc)
        self._closed = False
        self._buffer = []
        self._buffer_lock = thread.allocate_lock()
        self._read_lock = thread.allocate_lock()
        self._empty_lock = thread.allocate_lock()
        self._empty_lock.acquire()
        self._empty_lock_lock = thread.allocate_lock()

    def _free_empty_lock(self):
        self._empty_lock_lock.acquire()
        try:
            if self._empty_lock.locked():
                self._empty_lock.release()
        finally:
            self._empty_lock_lock.release()

    def write(self, string_):
        """Stejn� jako v�p��pad� t��dy 'file'.

        Z�pis po zavol�n� metody 'close()' vyvol� v�jimku 'ValueError'.
        K�t�mu� m��e doj�t, pokud byl v�konstruktoru specifikov�n kop�rovac�
        stream a je ji� uzav�en.

        """
        if self._closed:
            raise ValueError, "I/O operation on closed file"
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
        """Stejn� jako v�p��pad� t��dy 'file'."""
        # TODO: Z nepochopiteln�ch d�vod� je zde p�ed�v�n� size nutn�.
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
        return with_lock(self._read_lock, lfunction)

    def close(self):
        """Stejn� jako v�p��pad� t��dy 'file'.

        Tato metoda p�itom uzav�r� pouze z�pisov� konec roury a cc stream
        (byl-li v�konstruktoru zad�n) a ponech�v� data pro �ten�.  Uvolnit data
        lze n�sledn�m zavol�n�m metody 'read()' bez argument�.

        """
        self._closed = True
        self._free_empty_lock()
        for s in self._cc:
            try:
                s.close()
            except:
                pass


class Popen:
    """T��da umo��uj�c� spou�t�n� program� a komunikaci s�nimi.

    P�i vytv��en� instance t��dy je vytvo�en nov� proces, se kter�m je mo�no
    komunikovat pomoc� zadan�ch nebo nov� vytvo�en�ch stream�, bl�e viz metoda
    '__init__()'.

    Streamy pro komunikaci s�procesem jsou dostupn� prost�ednictv�m metod
    'from_child()' a 'to_child()'.  Process id spu�t�n�ho programu je
    dostupn� p�es metodu 'pid()'.
    
    """
    def __init__(self, command, to_child=None, from_child=None,
                 directory=None):
        """Spus� 'command' v�samostatn�m procesu.

        Argumenty:

          command -- string nebo sekvence string� definuj�c� spou�t�n� p��kaz a
            jeho argumenty
          to_child -- file descriptor nebo file object obsahuj�c� file
            descriptor otev�en� pro z�pis, prost�ednictv�m kter�ho bude
            zapisov�no na standardn� vstup spu�t�n�ho procesu; m��e b�t t�
            'None', v�kter�m�to p��pad� bude pro tyto ��ely vytvo�ena nov�
            roura
          from_child -- file descriptor nebo file object obsahuj�c� file
            descriptor otev�en� pro �ten�, prost�ednictv�m kter�ho bude
            dostupn� standardn� v�stup spu�t�n�ho procesu; m��e b�t t� 'None',
            v�kter�m�to p��pad� bude pro tyto ��ely vytvo�ena nov� roura
          directory -- existuj�c� adres�� (coby string), ve kter�m m� b�t
            proces spu�t�n, nebo 'None', v�kter�m�to p��pad� je proces spu�t�n
            v�aktu�ln�m adres��i

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
        """Vra� file object pro �ten� ze standardn�ho v�stupu procesu."""
        return self._from_child

    def to_child(self):
        """Vra� file object pro z�pis na standardn� vstup procesu."""
        return self._to_child

    def pid(self):
        """Vra� process id spu�t�n�ho programu."""
        return self._pid

    def wait(self):
        """�ekej na dokon�en� podprocesu."""
        os.waitpid(self.pid(), 0)


class Tmpdir(object):
    """T��da vytv��ej�c� pro dobu sv� existence do�asn� adres��.

    T��da zaji��uje vytvo�en� do�asn�ho adres��e p�i sv�m vzniku a jeho smaz�n�
    v�etn� v�ech soubor� v�n�m obsa�en�ch p�i sv�m z�niku.

    """

    def __init__(self, prefix='pytis', *args, **kwargs):
        """Inicializuj instanci.

        Argumenty:

          prefix -- prefix jm�na adres��e, string

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
    """Obecn� z�sobn�k.

    Datov� struktura typu LIFO, umo��uj�c� pracovat s prvky libovoln�ho typu.

    """

    def __init__(self):
        self._list = []
        
    def __str__(self):
        classname = str(self.__class__).split('.')[-1]
        contents = ', '.join(map(str, self._list))
        return '<%s contents=%s>' % (classname, contents)

    def push(self, item):
        """P�idej prvek na vrchol z�sobn�ku.

        Argumentem m��e b�t libovoln� objekt.

        """
        self._list.append(item)
        
    def pop(self):
        """Odeber objekt z vrcholu z�sobn�ku.

        P�i pokusu o odebr�n� z pr�zdn�ho z�sobn�ku vyvolej `IndexError'.
        
        """
        return self._list.pop()
            
    def top(self):
        """Vra� nejvrchn�j�� prvek ze z�sobn�ku.

        Pokud je z�sobn�k pr�zdn�, vra� None.
        
        """
        if self.empty():
            return None
        return self._list[-1]

    def empty(self):
        """Vra� pravdu, je-li z�sobn�k pr�zdn�."""
        return len(self._list) == 0
        

class XStack(Stack):
    """Z�sobn�k s aktivn�m prvkem a dal��mi roz���en�mi mo�nostmi.

    Roz�i�uje mo�nosti z�sobn�ku o:

      * aktivaci libovoln�ho prvku
      * zji�t�n� aktivn�ho prvku
      * zji�t�n� seznamu v�ech prvk�
      * vyjmut� libovoln�ho prvku ze z�sobn�ku
      * zji�t�n� po�ad� posledn� aktivovan�ch prvk� (MRU)

    Omezen�: V z�sobn�ku nesm� b�t p��tomen jeden objekt sou�asn� v�cekr�t,
    resp. z�sobn�k nesm� obsahovat dva ekvivalentn� prvky.  V
    takov�m p��pad� nen� chov�n� z�sobn�ku definov�no.

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
        """Odeber objekt z vrcholu z�sobn�ku.

        P�i odebr�n� aktivn�ho prvku se st�v� aktivn�m prvkem vrchn� prvek
        z�sobn�ku.
        
        """
        item = self.top()
        self._mru.remove(item)
        super(XStack, self).pop()
        if item is self._active:
            self.activate(self.top())

    def remove(self, item):
        """Remove the given 'item' from the stack.

        If 'item' is currently the active element, 

        Pokud byl vyjmut� prvek aktivn�m prvkem, je aktivov�n n�sleduj�c� prvek
        (pokud neexistuje, tak p�edch�zej�c�).
        
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
        """Vra� seznam v�ech prvk� jako tuple.

        Prvek ``top'' je posledn�.

        """
        return tuple(self._list)

    def mru(self):
        """Vra� seznam prvk� se�azen� podle posledn� aktivace.

        Aktivn� prvek je prvn�, za n�m n�sleduje prvek, kter� byl aktivn� p�ed
        t�m, ne� se aktivn� prvek stal aktivn�m, atd.
        
        """
        return tuple(self._mru)

    def activate(self, item):
        """Aktivuj dan� prvek."""
        assert item in self._list or item is None and self.empty()
        self._active = item
        if item is not None:
            if item in self._mru:
                self._mru.remove(item)
            self._mru.insert(0, item)            
        
    def active(self):
        """Vra� pr�v� aktivn� prvek"""
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

    def __hash__(self):
        value = 0
        for attribute in self._attributes:
            value = value ^ hash(gettattr(self, attribute.name()))
        return value


class object_2_5(object):
    """Base class emulating Python 2.5 'object' class.

    Unlike 'object' class in Python 2.6 it consumes any keyword arguments.
    This makes handling some multiple inheritance situations easier.
    
    """

    def __init__(self, **kwargs):
        object.__init__(self)


### Funkce

def identity(x):
    """Vra� 'x'."""
    return x


def is_(x, y):
    """Vra� pravdu, pr�v� kdy� je 'x' identick� s�'y' ve smyslu oper�toru 'is'.

    'x' a 'y' mohou b�t libovoln� objekty.

    """
    return x is y

def xor(x, y):
    """Vra� pravdivostn� hodnotu exkluzivn�ho OR v�raz� 'x' a 'y'."""
    return (x and not y) or (not x and y)


def some(predicate, *sequences):
    """Vra� pravdu, pr�v� kdy� n�jak� prvek 'sequences' spl�uje 'predicate'.

    Argumenty:

      predicate -- funkce s�po�tem argument� rovn�m po�tu prvk� 'sequences'
        vracej�c� pravdu nebo nepravdu
      sequences -- sekvence vz�jemn� stejn� dlouh�ch sekvenc�, jejich�
        zazipov�n�m vzniknou sekvence argument� pro vol�n� funkce 'predicate'

    """
    for elt in zip(*sequences):
        if predicate(*elt):
            return True
    else:
        return False

        
def xtuple(x):
    """Vra� 'x' jako tuple.

    Je-li 'x' sekvence, vra� tuple, jeho� prvky se shoduj� s�prvky 'x'.  Jinak
    vra� tuple, jeho� jedin�m prvkem je 'x'.
    
    """
    if is_sequence(x):
        return tuple(x)
    else:
        return (x,)


def xlist(x):
    """Vra� 'x' jako list.

    Je-li 'x' sekvence, vra� list, jeho� prvky se shoduj� s�prvky 'x'.  Jinak
    vra� list, jeho� jedin�m prvkem je 'x'.
    
    """
    if is_sequence(x):
        return list(x)
    else:
        return [x]


def safedel(object, element):
    """Aplikuj oper�tor 'del' na 'element' of 'object' bez signalizace chyby.

    Prov�d� p��kaz 'del object[element]', av�ak odchyt�v� p��padnou v�jimku
    'KeyError', resp. 'IndexError', m�sto n� ned�l� nic.
    
    Argumenty:

      object -- dictionary nebo list, ze kter�ho m� b�t odstran�n 'element'
      element -- pro 'object' dictionary libovoln� objekt, kter� je kl��em
        'object'; pro 'object' list libovoln� nez�porn� integer

    Vrac�: 'object'.

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
    """Vra� pozici 'element' v�'sequence'.

    Pokud se 'element' v�'sequence' nenach�z�, vra� 'None'.
    
    Porovn�n� prvk� je prov�d�no oper�torem '=='.  Hodnoty prvk� 'sequence'
    jsou z�sk�v�ny funkc� 'key', kter� mus� jako sv�j jedin� argument p�ij�mat
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
    """Vra� nejlev�j�� prvek 'sequence' rovnaj�c� se 'element'.
    
    Pokud se 'element' v�'sequence' nenach�z�, vra� 'None'.

    Argumenty:

      key -- funkce jednoho argumentu, kter�m je prvek 'sequence', vracej�c�
        hodnotu pro porovn�n� s�'element'
      test -- funkce dvou argument�, z�nich� prvn� je 'element' a druh� prvek
        'sequence' po aplikaci 'key'.  Je-li zad�no, prov�d� se porovn�n� touto
        funkc�, jinak se porovn�n� prov�d� oper�torem '=='.

    """
    for elt in sequence:
        if test(element, key(elt)):
            return elt
    else:
        return None


def assoc(item, alist):
    """Vra� nejlev�j�� prvek z�'alist', jeho� prvn� prvek se rovn� 'item'.

    Pokud takov� prvek neexistuje, vra� 'None'.  Porovn�n� se prov�d�
    oper�torem '='.
    
    'alist' mus� b�t sekvence nepr�zdn�ch sekvenc�.

    """
    return find(item, alist, key=(lambda x: x[0]))


def rassoc(item, alist):
    """Vra� nejlev�j�� prvek z�'alist', jeho� druh� prvek se rovn� 'item'.

    Pokud takov� prvek neexistuje, vra� 'None'.  Porovn�n� se prov�d�
    oper�torem '='.
    
    'alist' mus� b�t sekvence dvouprvkov�ch sekvenc�.

    """
    return find(item, alist, key=(lambda x: x[1]))


def remove_duplicates(list, keep_order=False):
    """Vra� prvky 'list', av�ak bez jejich n�sobn�ch v�skyt�.

    N�sobnost se testuje porovn�n�m prvk� pomoc� oper�toru '='.
    
    Argumenty:

      keep_order -- p�i v�choz� hodnot� funkce nezachov�v� po�ad� prvk�, ale
        algoritmus je optimalizov�n.  Pokud pot�ebujeme po�ad� zachovat, mus�me
        o�ek�vat vy��� n�ro�nost algoritmu.

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
    """Vra� 'list' bez vno�en�ch sekvenc�.

    Argumenty:

      list -- libovoln� sekvence

    Vrac�: Sekvenci tvo�enou prvky sekvence 'list', p�i�em� ka�d� prvek, kter�
      je s�m sekvenc�, je ve vr�cen� sekvenci rekurzivn� nahrazen sv�mi prvky.

    """
    result = []
    if is_sequence(list):
        result = result + reduce(operator.add, map(flatten, list), [])
    else:
        result.append(list)
    return result


def nreverse(list):
    """Vra� prvky 'list' v�opa�n�m po�ad�.

    Argumenty:

      list -- libovoln� list

    Funkce je destruktivn�, tj. hodnota 'list' je v�n� zm�n�na.

    """
    list.reverse()
    return list


def starts_with(string, prefix):
    """Vra� pravdu, pr�v� kdy� 'string' za��n� 'prefix'.

    Argumenty:

      string -- string
      prefix -- string

    """
    return string[:len(prefix)] == prefix


def super_(class_):
    """Vra� prvn�ho p�edka t��dy 'class_'."""
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
    """Vra� potomka n�sleduj�c�ho 'class_' v�hierarchii d�di�nosti 'instance'.

    Pokud m� t��da 'instance' atribut '__mro__', je pou�it tento.  V�opa�n�m
    p��pad� je tento atribut t��dy vytvo�en prohled�v�n�m p�edk� 'instance' do
    hloubky.  'instance' mus� b�t instanc� 'class_'.

    Vrac�: Odpov�daj�c� t��du; pokud takov� nen� tak 'None'.
    
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
    """Vra� pravdu, pr�v� kdy� 'o1' a 'o2' jsou instance t�e t��dy.

    Je-li argument 'strict' pravdiv�, mus� se rovnat t��dy obou objekt� 'o1' a
    'o2' ve smyslu oper�toru '=='.  V�opa�n�m p��pad� posta�� rovnost jmen t��d
    a jejich modul�.

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
    """Vra� tuple v�ech jmen ve�ejn�ch atribut� t��dy 'class_'.

    Vr�cen� jm�na jsou strings a obsahuj� i�pod�d�n� atributy.  Nejsou mezi
    nimi v�ak ��dn� jm�na za��naj�c� podtr��tkem.  Jm�na atribut� jsou ve
    vr�cen� sekvenci v�po�ad� d�di�nosti po��naje od 'class_'.  Mohou se v�nich
    vyskytovat duplicity.

    Dojde-li od posledn�ho vol�n� t�to funkce v�'class_' ke zm�n� atribut�,
    tato zm�na nemus� b�t zohledn�na.

    """
    global _public_attributes
    try:
        return _public_attributes[class_]
    except KeyError:
        pass
    attrs = map(dir, _mro(class_))
    att = reduce(operator.add, attrs)
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
    """Vra� tuple v�ech p��m�ch ve�ejn�ch atribut� a metod t��dy objektu 'obj'.

    P��m�mi �leny t��dy jsou my�leny ty, kter� nejsou shodn� se stejnojmenn�m
    �lenem n�kter�ho p�edka t��dy.  Ve�ejn�mi �leny t��dy jsou my�leny ty,
    jejich� n�zev neza��n� podtr��tkem.

    """
    if isinstance(obj, (pytypes.ClassType, type,)):
        cls = obj
    else:
        cls = obj.__class__
    def public_members(cls):
        return [(name, value) for name, value in inspect.getmembers(cls)
                if name and not name.startswith('_')]
    super_members = reduce(operator.add, [public_members(b)
                                          for b in cls.__bases__], [])
    result = [name for name, value in public_members(cls)
              if find(value, [x[1] for x in super_members]) is None]
    return tuple(result)


def compare_objects(o1, o2):
    """Porovnej 'o1' a 'o2' a vra� v�sledek.

    V�sledek odpov�d� pravidl�m pro special metodu '__cmp__'.

    Pro porovn�n� plat� n�sleduj�c� pravidla:

    - Jestli�e jsou oba objekty 'None', rovnaj� se.

    - Jestli�e jeden z�objekt� je instance t��dy a druh� nen� instanc� t��dy,
      instance je v�t��.

    - Jestli�e oba objekty jsou instance r�zn�ch t��d, vr�t� se v�sledek
      porovn�n� 'id' jejich t��d.

    - Neplat�-li ��dn� z�p�edchoz�ch podm�nek, vr�t� se v�sledek vol�n�
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
    """Vra� celkov� v�sledek porovn�n� atribut� objekt� 'self' a 'other'.

    Funkce porovn�v� t��dy objekt� 'self' a 'other' a v�p��pad� shody pak
    uveden� atributy.  N�vratov� hodnota se ��d� pravidly pro funkci 'cmp'.

    Argumenty:

      self, other -- instance t��d
      attributes -- sekvence jmen atribut� instanc� (strings), kter� maj� b�t
        porovn�v�ny

    Funkce je typicky ur�ena k�pou�it� v�metod� '__cmp__'.

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
    """Vra� hash-k�d instance 'self'.

    K�d je vytv��en dle hodnot 'attributes' instance, v�souladu s�pythonov�mi
    pravidly pro hash k�d.

    Argumenty:

      self -- instance t��dy, pro n� m� b�t hash k�d vytvo�en
      attributes -- sekvence jmen atribut� (strings), jejich� hodnoty maj� b�t
        p�i vytv��en� k�du uva�ov�ny
    
    """
    dict = self.__dict__
    def h(obj):
        if isinstance(obj, list):
            obj = tuple(obj)
        return hash(obj)
    return reduce (operator.xor, map(lambda a: h(dict[a]), attributes))


def is_sequence(x):
    """Vra� pravdu, pr�v� kdy� 'x' je list nebo tuple."""
    t = type(x)
    return t == pytypes.TupleType or t == pytypes.ListType


def is_dictionary(x):
    """Vra� pravdu, pr�v� kdy� 'x' je dictionary."""
    return type(x) == pytypes.DictionaryType

def is_string(x):
    """Vra� pravdu, pr�v� kdy� 'x' je b�n� �et�zec."""
    return isinstance(x, pytypes.StringType)

def is_unicode(x):
    """Vra� pravdu, pr�v� kdy� 'x' je unicode �et�zec."""
    return isinstance(x, pytypes.UnicodeType)

def is_anystring(x):
    """Vra� pravdu, pr�v� kdy� 'x' je unicode �et�zec nebo b�n� �et�zec."""
    return isinstance(x, pytypes.StringTypes)

def ecase(value, *settings):
    """Vra� hodnotu ze 'settings' odpov�daj�c� 'value'.

    Pokud 'value' nen� v�'settings' obsa�eno, vyvolej v�jimku 'ProgramError'.
    Je-li v�'settings' 'value' obsa�eno v�cekr�t, je uva�ov�n prvn� v�skyt.
    
    Argumenty:

      value -- libovoln� objekt; je porovn�v�n s�prvn�mi prvky prvk� 'settings'
        oper�torem '='
      settings -- sekvence dvojic (KEY, VALUE), kde KEY odpov�d� n�kter�
        z�mo�n�ch hodnot 'value' a VALUE je hodnota, kterou m� funkce vr�tit
        v�p��pad� shody KEY a 'value' vr�tit

    Vrac�: VALUE z�dvojice ze 'settings', jej� KEY odpov�d� 'value'.

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
        import pytis.util
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
    """V�jimka pro nelok�ln� p�echody."""
    
    def __init__(self, tag, value):
        """Inicializuj instanci.

        Argumenty:

          tag -- string identifikuj�c� p�echod
          value -- n�vratov� hodnota p�echodu, libovoln� objekt

        """
        Exception.__init__(self)
        self._tag = tag
        self._value = value

    def tag(self):
        """Vra� tag zadan� v�konstruktoru."""
        return self._tag

    def value(self):
        """Vra� hodnotu 'value' zadanou v�konstruktoru."""
        return self._value

def catch(tag, function, *args, **kwargs):
    """Volej 'function' s�o�et�en�m nelok�ln�ho p�echodu.

    Argumenty:

      tag -- string identifikuj�c� p�echod
      function -- funkce, kter� m� b�t zavol�na
      args -- argumenty 'function'
      kwargs -- kl��ovan� argumenty 'function'

    Jsou o�et�eny pouze p�echody s�tagem 'tag', ostatn� odchyceny nejsou.
    
    Vrac�: Nedo�lo-li k�p�echodu, je vr�cena n�vratov� hodnota 'function'.
    Do�lo-li k�p�echodu, je vr�cena hodnota z�p�echodu p�edan� funkci 'throw_'.

    Viz t� funkce 'throw_'.
      
    """
    try:
        result = function(*args, **kwargs)
    except _Throw, e:
        if e.tag() == tag:
            result = e.value()
        else:
            raise
    return result
    
def throw(tag, value=None):
    """Vyvolej nelok�ln� p�echod identifikovan� 'tag'.

    Argumenty:

      tag -- string identifikuj�c� p�echod
      value -- n�vratov� hodnota p�echodu, libovoln� objekt

    Viz t� funkce 'catch'.

    """
    raise _Throw(tag, value)


def copy_stream(input, output, close=False, in_thread=False, _catch=False):
    """Zkop�ruj data ze streamu 'input' do streamu 'output'.

    Po��te�n� pozice ve streamech nejsou nijak nastavov�ny, to je starost�
    volaj�c�ho.  Je-li argument 'close' pravdiv�, je stream 'output' po
    ukon�en� kop�rov�n� uzav�en; v�opa�n�m p��pad� nen� uzav�en ��dn� stream.

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
                log(DEBUG, 'Kop�ruji stream:', (input, output))
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
                log(DEBUG, 'Stream zkop�rov�n:', (input, output))
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
    """Vra� bezdatov� stream.

    Vr�cen� stream je plnohodnotn� file object a funguje jako za��zen�
    '/dev/null' -- neposkytuje ��dn� data a v�echna p�ijat� data zahazuje.

    Argumenty:

      mode -- jeden ze string� 'r' (nech� je vr�cen� stream otev�en pro �ten�)
        nebo 'w' (nech� je vr�cen� stream otev�en pro z�pis)

    """
    assert mode in ('r', 'w')
    return open('/dev/null', mode)


_mktempdir_counter = None
def mktempdir(prefix='pytis'):
    """Vytvo� podadres�� v�adres��i pro do�asn� soubory.

    Adres�� pro do�asn� soubory je d�n konfigurac�.  Jm�no podadres��e se
    skl�d� ze zadan�ho 'prefix', kter�m mus� b�t string, a�generovan� p��pony.

    Podadres�� je vytvo�en s�p��stupov�mi pr�vy 0700.  Nen�-li mo�n� adres��
    z�n�jak�ho d�vodu vytvo�it, je vyvol�na v�jimka 'FileError'.
    
    Vrac�: Jm�no vytvo�en�ho adres��e v�etn� kompletn� cesty.  ��dn� dv� vol�n�
    t�to funkce nevr�t� stejn� jm�no; to v�ak neplat� v�p��pad� pou�it�
    threads, proto�e funkce nen� thread-safe.
    
    """
    import config
    global _mktempdir_counter
    if _mktempdir_counter is None:
        _mktempdir_counter = Counter()
    pattern = os.path.join(config.tmp_dir,
                           '%s%d.%%d' % (prefix, os.getpid()))
    oldumask = os.umask(0077)
    try:
        for i in range(1000):
            n = _mktempdir_counter.next()
            try:
                the_dir = pattern % n
                os.mkdir(the_dir, 0700)
                break
            except OSError:
                pass
        else:
            raise FileError(pattern)
    finally:
        os.umask(oldumask)
    return the_dir


def in_x():
    """Vra� pravdu, pr�v� kdy� je k�dispozici prost�ed� X�Window."""
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



# R�zn�


UNDEFINED = object()
"""Objekt reprezentuj�c� nedefinovanou hodnotu.

Typicky se pou��v� jako implicitn� hodnota voliteln�ch argument�, aby nebylo
nutno prov�d�t jejich definici a zkoum�n� prost�ednictv�m **kwargs.

"""


# Funkce pro lad�n�


def debugger():
    """Vyvolej interaktivn� debugger.

    U�ite�n� pouze pro lad�n�.

    """
    import pdb
    pdb.set_trace()


_mem_info = None
def mem_info():
    """Vypi� na standardn� chybov� v�stup informaci o�pam�ti.

    U�ite�n� pouze pro lad�n�.

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
    """Vra� stringovou podobu 'obj'.

    Je-li 'obj' sekvence, aplikuj se rekurzivn� na jej� prvky.

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
    """Vra� zform�tovan� traceback aktu�ln� v�jimky, jako string."""
    import traceback
    einfo = __, einstance, tb = sys.exc_info()
    tblist = traceback.format_exception(*einfo)
    tbstring = string.join(tblist, '')
    return tbstring

    
def exception_info(einfo=None):
    """Vra� podrobn� v�pis informac� o�aktu�ln� v�jimce, jako string.

    Tento v�pis je zalo�en na funkc�ch modulu 'cgitb', av�ak m�sto HTML vrac�
    oby�ejn� textov� string.

    Argumenty:

      einfo -- informace o�v�jimce ve tvaru vracen�m funkc� 'sys.exc_info()',
        nebo 'None' (v�kter�m�to p��pad� je tato informace z�sk�na automaticky)

    """
    # Inicializace
    etype, evalue, etb = einfo or sys.exc_info()
    context = 5
    import os, types, time, traceback, linecache, inspect
    # Sestaven� hlavi�ky
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
                    name = name
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
    """Vra� obsah z�sobn�ku vol�n�, jako string.

    String je zform�tovan� podobn� jako Pythonov� traceback.  Posledn� vol�n�
    je na konci.  Argument 'depth' m��e omezit hloubku jen na ur�it� po�et
    frames.

    Funkce je typicky ur�ena k lad�n�.
    
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

