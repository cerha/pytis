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
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

"""Implementace tabulkov�ch dat pro rela�n� datab�zov� stroje.

Z�kladem modulu je abstraktn� t��da 'DBData'.  Ta je potomkem t��dy
'data.Data' a obohacuje ji o�specifikaci napojen� tabulkov�ch dat do rela�n�
datab�ze a roz�i�uje n�kter� ji metody.

Jednotliv� potomci t��dy 'DBData' implementuj� datab�zov� p��stup pro
jednotliv� datab�zov� stroje (na �rovni SQL) a jejich potomci pak pro
konkr�tn� pythonov� rozhran� k�t�mto stroj�m.

R�zn� datab�zov� t��dy pln� n�sleduj�c� role:

- P�eklad po�adavk� do SQL p��slu�n�ho datab�zov�ho stroje.

- Implementace komunikace s�datab�z� pomoc� pythonov�ho modulu.

- Interpretace column bindings.

Ka�d� z�t�chto rol� je realizov�na samostatnou t��dou.

Krom� toho modul obsahuje pomocnou t��du 'DBConnection' pro specifikaci
datab�zov�ho spojen�, pomocn� t��dy zalo�en� na 'DBBinding' pro specifikaci
napojen� uvnit� 'DBData' a datab�zov� v�jimky odvozen� ze t��dy
'DBException'.  T��d� 'DBBinding' a jej�m potomk�m je t�eba v�novat
obzvl�tn� pozornost, pomoc� nich se definuj� nejd�le�it�j�� v�ci t�kaj�c� se
tabulky.

"""


# TODO: V�implementaci se po��t� s t�m, �e z�datab�ze nedostaneme nesmysly
# (z�d�vodu jin�ho ne� program�torsk� chyby).  Tyto situace sice obecn�
# o�et�ujeme met�n�m DBException, to ale nesta��, proto�e takov� situace m��e
# vzniknout i�n�jak�m probl�mem na DB serveru.  Je tedy v�budoucnu
# bezpodm�ne�n� nutno tyto situace ��dn� o�et�ovat a pro�et�ovat.  Nejprve je
# ov�em nutno definovat n�jak� mechanismus o�et�ov�n� chyb.


import gc
import thread
import weakref

from pytis.data import *


### Obecn� t��dy


class DBData(Data):
    """Tabulkov� data mapovan� do rela�n� datab�ze.

    T��da roz�i�uje sv�ho p�edka o�specifikaci napojen� sloupc� tabulkov�ch dat
    do rela�n� datab�ze (bl�e viz metoda '__init__') a o�n�kter� argumenty
    jeho ve�ejn�ch metod.  Zav�d� tak� ve�ejnou metodu 'sleep'.

    Tato t��da je nez�visl� na konkr�tn�m pou�it�m datab�zov�m stroji a
    pythonov�m modulu.  Z�visl� v�ci jsou implementov�ny v�potomc�ch t�to
    t��dy, p�i�em� se doporu�uje v�nejbli��� podt��d� napsat potomky pro
    jednotliv� datab�zov� stroje (tj. realizovat metody na �rovni SQL a jin�ch
    vlastnost� dan�ho datab�zov�ho stroje) a teprve nad nimi definovat t��dy
    odpov�daj�c�m jednotliv�m pythonov�m modul�m.  Je t� mo�n� vyu��t
    v�cen�sobn� d�di�nosti a definovat nap��klad \"odbo�uj�c�\" t��du pro
    DB-SIG API.

    V�echny metody t�to t��dy p��stupuj�c� k�dat�m mohou metat 'DBException'.
    
    """
    def __init__(self, bindings, ordering=None, distinct_on=(), arguments=None,
                 **kwargs):
        """Inicializuj tabulku s�napojen�m do datab�ze.

        Argumenty:
        
          bindings -- sekvence instanc� t��dy 'DBBinding'
          ordering -- stejn� jako v�p�edkovi
          distinct_on -- sequence of column names to add as a DISTINCT TO part
            to SELECT commands
          arguments -- sequence of 'DBBinding' instances defining table
            arguments, when the table is actually a row returning function.
            Otherwise it must be 'None'.
          kwargs -- k p�ed�n� p�edkovi

        Sloupce datov� tabulky se ur�� automaticky na z�klad� 'bindings'.
        Jejich typy jsou ur�eny typy odpov�daj�c�ch dat v�datab�zi(p�esn�
        mapov�n� z�vis� na potomc�ch t��dy a nen� zde specifikov�no).  Kl��ov�m
        sloupcem tabulky je prvn� sloupec z�'bindings', kter� je kl��ov�m
        sloupcem v�datab�zi.

        ��dn� dva prvky 'bindings' by nem�ly m�t shodn� id, s�v�jimkou skryt�ch
        bindings, kter� maj� jako id pr�zdn� �et�zec.
        
        """
        assert is_sequence(bindings), ('Invalid binding type', bindings)
        self._bindings = tuple(bindings)
        assert not filter(lambda b: not isinstance(b, DBBinding),
                          bindings), \
               ('Invalid binding type', bindings)
        assert arguments is None or is_sequence(arguments), ('Invalid binding type', arguments)
        if arguments is None:
            self._arguments = None
        else:
            self._arguments = tuple(arguments)
            assert not filter(lambda b: not isinstance(b, DBBinding),
                              arguments), \
                   ('Invalid "argument" type', arguments)
        if __debug__: log(DEBUG, 'Bindings datab�zov� instance', self._bindings)
        columns, key = self._db_bindings_to_column_spec(self._bindings)
        if __debug__: log(DEBUG, 'Sloupce datab�zov� instance:', columns)
        if __debug__: log(DEBUG, 'Kl�� datab�zov� instance:', key)
        self._distinct_on = distinct_on
        try:
            del kwargs['key']
        except:
            pass
        super(DBData, self).__init__(columns=columns, key=key, ordering=ordering, **kwargs)

    def _db_bindings_to_column_spec(self, bindings):
        """Vra� dvojici (COLUMNS, KEY) odpov�daj�c� argument�m 'Data.__init__'.

        V�t�to t��d� metoda vr�t� jednodu�e seznam sloupc� v�podob� instanc�
        'ColumnSpec' s�n�zvy odpov�daj�c�mi identifik�tor�m 'bindings' (i�co
        se t��e jejich po�ad�), p�i�em� typ v�ech sloupc� je nastaven na
        nejobecn�j�� typ 'types_.Type'.  Jako KEY je vr�cen prvn� sloupec
        uveden� v�'bindings'; pokud jsou 'bindings' pr�zdn�, je vr�ceno 'None'.

        Tato implementace metody je velmi hrub� a metoda je ur�ena
        k�p�edefinov�n� v�potomc�ch t��dy.

        """
        columns = map(lambda b: ColumnSpec(b.id(), Type()), bindings)
        if columns:
            key = columns[0]
        else:
            key = None
        return columns, key

    def _db_column_binding(self, col_id):
        """Vra� binding sloupce 'col_id'.

        Pokud pro 'col_id' nen� ��dn� binding definov�no, vra� 'None'.

        Argumenty:
        
          col_id -- identifikace tabulkov�ho sloupce jako string
          
        """
        for b in self._bindings:
            if b.id() == col_id:
                return b
        return None

    def table(self, col_id):
        """Vra� tabulku sloupce 'col_id'.

        Argumenty:

           viz metoda '_db_column_binding'.
        """
        b = self._db_column_binding(col_id)
        if b:
            return b.table()
        return None

    def sleep(self):
        """Uvolni syst�mov� zdroje vyu��van� instanc� a umo�ni jej� zru�en�.

        Pokud instance vyu��v� n�kter� nedostatkov� syst�mov� zdroje jako
        nap��klad spojen� do datab�ze nebo procesy, tato metoda by je m�la
        uvolnit.  Druhou funkc� metody je uvolnit prost�edky (nap��klad hl�dac�
        procesy), kter� znemo��uj� zru�en� instance.

        Tato metoda by m�la b�t vol�na v�dy, kdy� je trvale nebo na del�� dobu
        ukon�eno pou��v�n� instance.

        Zavol�n�m t�to metody se neznemo��uje dal�� pou�it� instance, lze
        nad�le vyu��vat v�echny jej� ve�ejn� metody.  Je-li pak ov�em t�eba
        op�t uvolnit zdroje, je nutno tuto metodu zavolat znovu.

        """
        if __debug__: log(DEBUG, 'Usp�n�')
        self.close()

    def arguments(self):
        """Return the value of argument 'arguments' passed to the constructor."""
        return self._arguments
        

class DBConnectionPool:

    def __init__(self, connection_creator, connection_closer):
        if __debug__: log(DEBUG, 'Vytv���m nov� pool')
        self._lock = thread.allocate_lock()
        self._pool = {}
        self._connection_creator = connection_creator
        self._connection_closer = connection_closer
        self._allocated_connections = {}

    def __del__(self):
        # Pro jistotu uzav�r�me v�echna spojen�, p�esto�e by to m�lo b�t
        # zbyte�n� a zaji�t�no automaticky v�pyPgSQL; t�eba to pom��e
        # probl�mu poz�stal�ch spojen�.  Nav�c pro jistotu zamyk�me, co
        # kdyby�...
        def lfunction():
            for c in flatten(self._pool.values()):
                try:
                    self._connection_closer(c)
                except:
                    pass
        with_lock(self._lock, lfunction)

    def _connection_spec_id(self, connection_spec):
        c = connection_spec
        schemas = c.schemas()
        if isinstance(schemas, list):
            schemas = tuple(schemas)
        return (c.database(), c.host(), c.port(), c.user(), c.password(), c.sslmode(), schemas,)

    def get(self, connection_spec):
        import config
        pool = self._pool
        spec_id = self._connection_spec_id(connection_spec)
        def lfunction():
            try:
                connections = pool[spec_id]
            except KeyError:
                pool[spec_id] = connections = []
            try:
                allocated_connections = self._allocated_connections[spec_id]
            except KeyError:
                allocated_connections = self._allocated_connections[spec_id] \
                    = weakref.WeakKeyDictionary()
            if connections:
                if __debug__: log(DEBUG, 'Spojen� k�dispozici', connections)
                c = connections.pop()
            else:
                gc.collect()
                if (config.connection_limit is not None and
                    len(allocated_connections) >= config.connection_limit):
                    if __debug__:
                        log(EVENT, "P�ehled evidovan�ch spojen�:")
                        for c in allocated_connections.keys():
                            log(EVENT, "Spojen�:", c.connection_info('last_access'))
                    raise DBSystemException(_("P��li� mnoho datab�zov�ch spojen�"))
                else:
                    c = self._connection_creator(connection_spec)
                    if __debug__: log(DEBUG, 'Vytvo�eno nov� spojen�:', c)
                    allocated_connections[c] = True
            return c
        c = with_lock(self._lock, lfunction)
        if __debug__: log(DEBUG, 'P�ed�v�m spojen�:', c)
        return c

    def put_back(self, connection_spec, connection):
        pool = self._pool
        spec_id = self._connection_spec_id(connection_spec)
        def lfunction():
            try:
                connections = pool[spec_id]
            except KeyError:
                pool[spec_id] = connections = []
            import config
            if (config.max_pool_connections is None or
                len(connections) < config.max_pool_connections):
                connections.append(connection)
        with_lock(self._lock, lfunction)
        if __debug__: log(DEBUG, 'Do poolu vr�ceno spojen�:', connection)


### Specifika�n� t��dy

    
class DBConnection:
    """Specifikace parametr� pro p�ipojen� do datab�ze.

    Jedn� se �ist� o specifikaci, t��da sama ��dn� operace t�kaj�c� se napojen�
    do datab�ze neprov�d� ani neudr�uje skute�n� objekt spojen�.  Pro popis
    parametr� spojen� viz metoda '__init__'.

    Tato t��da je m�n�na jako immutable a tud� m��e b�t libovoln� sd�lena.
    Pro mal� �pravy specifikace lze vyu��t metodu 'modified()'.

    """
    _OPTIONS = ('user', 'password', 'host', 'port', 'database', 'sslmode', 'schemas',)
    
    def __init__(self, user=None, password=None, host=None, port=None,
                 database=None, sslmode='allow', schemas=None, alternatives={}, _name=None):
        """Initialize connection specification instance.

        Arguments:
        
          user -- database user as a string or 'None'
          password -- database password as a string or 'None'
          host -- database server name as a string or 'None'
          port -- database server port number as an int or 'None'
          database -- database name as a string or 'None'
          sslmode -- one of string constants accepted by PostgreSQL
          schemas -- non-empty sequence of schema identifiers (strings) to use
            in the database in the order of preference or 'None' (use default
            schemas)
          alternatives -- dictionary of alternative connection parameters.  Alternative
            database connections are identified by name and data object specifications may refer
            to these names to use connect to alternative data sources (thus the number and names
            of alternative connections is application specific).  The dictionary keys are
            connection names and the values are dictionaries of connection options ('user',
            'password', 'host', 'database', ...) with the same meaning as the corresponding
            arguments.

        Arguments with the value 'None' will be ignored when connecting to the database.

        """
        self._user = user
        self._password = password
        self._host = host
        self._port = port
        self._database = database
        self._sslmode = sslmode
        self._schemas = schemas
        if None not in alternatives:
            # Add the default connection to the alternatives if it is not already there, to be able
            # to `select()' back to it.
            alternatives[None] = self._options()
        self._alternatives = alternatives
        # Passing this private argument avoids unnecessay duplication of instances in `select()'.
        self._name = _name

    def _options(self, exclude=()):
        return dict([(option, self.__dict__['_'+option])
                     for option in self._OPTIONS
                     if option not in exclude and self.__dict__['_'+option] is not None])
    
    def __str__(self):
        options = ["%s='%s'" % item for item in self._options(exclude=('password',)).items()]
        return "<%s %s>" % (self.__class__.__name__, ", ".join(options))

    def user(self):
        """Vra� datab�zov�ho u�ivatele jako string nebo 'None'."""
        return self._user

    def password(self):
        """Vra� heslo datab�zov�ho u�ivatele jako string nebo 'None'."""
        return self._password

    def host(self):
        """Vra� jm�no datab�zov�ho serveru jako string nebo 'None'."""
        return self._host

    def port(self):
        """Vra� jm�no portu na serveru jako integer nebo 'None'."""
        return self._port

    def database(self):
        """Vra� jm�no datab�ze jako string nebo 'None'."""
        return self._database

    def sslmode(self):
        return self._sslmode

    def schemas(self):
        """Return schemas given in the constructor."""
        return self._schemas

    def __cmp__(self, other):
        """Vra� 0, pr�v� kdy� 'self' a 'other' definuj� tot� spojen�.

        Dv� instance t�to t��dy reprezentuj� tot� spojen�, pr�v� kdy� se
        rovnaj� odpov�daj�c� si parametry zadan� jejich konstruktor�m.

        """
        return compare_attr(self, other, ['_'+option for option in self._OPTIONS])

    def __hash__(self):
        return hash_attr(self, ['_'+option for option in self._OPTIONS])

    def select(self, name):
        """Return the specification instance activated for given connection name.

        Available connection names are defined by the 'alternatives' constructor argument.  'None'
        is reserved for the default connection.  The list of alternative connections is kept, so it
        is possible to switch back to a previous connection using 'select()' again.
        
        """
        if name == self._name:
            return self
        else:
            options = dict(self._alternatives[name], alternatives=self._alternatives, _name=name)
            return self.__class__(**options)

    def modified(self, **kwargs):
        """Return the new specification instance updated by given arguments.

        The new instance is the same as 'self', except for the values of passed keyword arguments.

        Arguments:

          kwargs -- keyword arguments same as in constructor.

        """
        options = dict(self._options(), alternatives=self._alternatives, **kwargs)
        return self.__class__(**options)

    def update_login_data(self, user, password):
        """Set given login parameters in the instance.

        Arguments:

          user -- database user as a string or 'None'
          password -- database password as a string or 'None'

        """
        self._user = user
        self._password = password


class DBBinding:
    """Definice napojen� dat do datab�ze.

    Tato definice je vyu��v�na t��dou 'DBData' a jej�mi potomky.
    
    Tato t��da je pouze abstraktn�m z�kladem, kter� definuje pouze stringov�
    identifik�tor instance napojen�.  Mechanismy pro skute�n� definice
    datab�zov�ch napojen� poskytuj� a� potomci t�to t��dy.

    T��da je specifikov�na jako immutable a jako takov� m��e b�t libovoln�
    sd�lena.
    
    """
    def __init__(self, id):
        """Definuj napojen�.

        Argumenty:
        
          id -- identifik�tor napojen�, libovoln� string
          
        """
        assert isinstance(id, str)
        self._id = id

    def id(self):
        """Vra� identifik�tor napojen� zadan� v�konstruktoru."""
        return self._id


class DBColumnBinding(DBBinding):
    """Vazba 1-1 tabulkov�ho sloupce na datab�zov� sloupec.

    Tato vazba je d�na jm�nem datab�zov� tabulky a jej�ho sloupce, na kter�
    se tabulkov� sloupec mapuje.
    
    T��da je specifikov�na jako immutable a jako takov� m��e b�t libovoln�
    sd�lena.
    
    """
    def __init__(self, id, table, column, related_to=None, type_=None, **kwargs):
        """Define a column binding.

        Argumenty:
        
          id -- id sloupce
          table -- jm�no datab�zov� tabulky, do kter� m� b�t tabulkov� sloupec napojen, jako string
          column -- jm�no sloupce datab�zov� tabulky, na kter� m� b�t tabulkov� sloupec napojen,
            jako string nebo sekvence (viz n�e)
          related_to -- instance 'DBColumnBinding' specifikuj�c�, se kter�m sloupcem jin�
            datab�zov� tabulky je tento sloupec v�relaci; pokud s���dn�m, je hodnotou 'None'
          type_ -- explicitn� specifikovan� typ sloupce jako instance t��dy 'Type' nebo 'None'.
            Je-li 'None', bude typ sloupce ur�en automaticky dle informac� z�skan�ch p��mo
            z�datab�ze.  V�opa�n�m p��pad� bude typem hodnota tohoto argumentu, kter� mus�
            odpov�dat typu sloupce v�datab�zi (b�t jeho specializac�).
          **kwargs -- explicitn� definovan� kl��ov� argumenty typu.  Pokud jsou definov�ny
            libovoln� kl��ov� argumenty, budou tyto p�ed�ny konstruktoru implicitn�ho datov�ho
            typu.  Typ v takov�m p��pad� nesm� b�t explicitn� ur�en argumentem 'type_'.

        Napojen� m��e b�t *skryt�*, co� znamen�, �e p��mo neodpov�d� ��dn�mu sloupci datov�
        tabulky.  To se m��e st�t nap��klad v�p��pad�, �e binding je definov�no *pouze* kv�li
        specifikaci relace mezi tabulkami (prost�ednictv�m argumentu 'related_to').  U�skryt�ho
        napojen� nez�le�� na hodnot� p��slu�n�ho sloupce a tud� k�t�mto hodnot�m ani nelze
        p�istupovat.  Napojen� je pova�ov�no za skryt�, pr�v� kdy� �et�zec 'id' je pr�zdn�.

        'related_to' je obecn� nesymetrick� relace p�ibli�n� odpov�daj�c� specifik�toru REFERENCES.

        """
        DBBinding.__init__(self, id)
        assert isinstance(table, str), table
        assert isinstance(column, str), column
        assert isinstance(type_, Type) or type(type_) == type(Type) or type_ is None, type_
        if __debug__:
            if isinstance(type_, Type):
                kwargs_copy = copy.copy(kwargs)
                if type_.not_null() == kwargs_copy.get('not_null', type_.not_null()):
                    try:
                        del kwargs_copy['not_null']
                    except KeyError:
                        pass                        
                assert kwargs == {}, (type_, kwargs)
        self._table = table
        self._column = column
        self._related_to = related_to
        self._type = type_
        self._kwargs = kwargs
        self._is_hidden = not id

    def table(self):
        """Vra� jm�no napojen� datab�zov� tabulky jako string."""
        return self._table

    def column(self):
        """Vra� jm�no napojen�ho sloupce jako string."""
        return self._column

    def related_to(self):
        """Vra� instanci DBColumnBinding napojen�ho sloupce nebo 'None'."""
        return self._related_to

    def type(self):
        """Vra� instanci typu sloupce z�konstruktoru nebo 'None'."""
        return self._type
    
    def kwargs(self):
        """Vra� slovn�k kl��ov�ch argument� konstruktoru dat. typu sloupce."""
        return self._kwargs
    
    def is_hidden(self):
        """Vra� pravdu, pr�v� kdy� sloupec nen� p��tomen v�datov� tabulce."""
        return self._is_hidden

    def __str__(self):
        return ('<DBCB: id=%s, table=%s, column=%s, related_to=%s, type=%s, is_hidden=%s>') % \
               (self._id, self._table, self._column, self._related_to, self._type, self._is_hidden)

    def __cmp__(self, other):
        return compare_attr(self, other,
                            ('_table', '_column', '_related_to', '_type', '_is_hidden'))
    
    def __hash__(self):
        return hash_attr(self, ('_table', '_column', '_related_to', '_type', '_is_hidden'))



### Datab�zov� v�jimky


class DBException(Exception):
    """V�jimka nahazovan� v�p��pad� vzniku datab�zov� chyby.

    V�echny datab�zov� v�jimky metan� ven ze t��dy 'DBData' jsou tohoto typu.

    T��da ve sv�m konstruktoru automaticky zaloguje z�kladn� informace
    o�v�jimce.

    """
    def __init__(self, message, exception=None, *args):
        """Inicializuj v�jimku a zaloguj informace o�n�.

        Argumenty:

          message -- lidsky �iteln� ozn�men� o�chyb�, string; m��e b�t t�
            'None', v�kter�m�to p��pad� se dopln� standardn� zpr�va
            o�datab�zov� chyb�, to je v�ak vhodn� pou��vat pouze v�p��pad�, kdy
            nem� smysl u�ivateli sd�lovat bli��� popis chyby
          exception -- v�jimka, kter� tuto v�jimku zp�sobila, instance t��dy
            'Exception'; nebo 'None' (neznamen� nutn�, �e chyba nebyla
            zp�sobena v�jimkou, tato v�jimka pouze nemus� b�t podstatn�)
          args -- libovoln� dal�� argumenty, kter� maj� b�t spolu s�'message' a
            'exception' p�ed�ny konstruktoru nadt��dy

        """
        if message == None:
            message = _("Datab�zov� chyba")
        super_(DBException).__init__(self, message, exception, *args)
        log(OPERATIONAL, 'Database exception', (message,) + args)
        if exception:
            log(OPERATIONAL, 'Wrapped database exception', (exception,) + exception.args)
        self._message = message
        self._exception = exception

    def message(self):
        """Vra� lidsky �itelnou zpr�vu zadanou v�konstruktoru, jako string."""
        return self._message

    def exception(self):
        """Vra� datab�zovou v�jimku zadanou v�konstruktoru nebo 'None'."""
        return self._exception


class DBSystemException(DBException):
    """V�jimka nahazovan� v�p��pad� vzniku syst�mov� datab�zov� chyby.

    Syst�mov� chyba je chyba souvisej�c� se syst�mov�mi prost�edky, nap��klad
    chyba spojen� do datab�ze.  Viz t� 'DBUserException'.

    Datab�zov� rozhran� generuje v�p��pad� datab�zov�ch chyb
    'DBSystemException' pouze tehdy, je-li schopno rozpoznat, �e se jedn�
    o�syst�movou chybu.  To znamen�, �e syst�mov� chyba m��e b�t signalizov�na
    i�jako prost� 'DBException'.
    
    """


class DBUserException(DBException):
    """V�jimka nahazovan� v�p��pad� vzniku u�ivatelsk� datab�zov� chyby.

    U�ivatelsk� chyba je chyba zp�soben� syntakticky nebo s�manticky chybnou
    datab�zovou operac�, nap��klad chybn�m pou�it�m SQL p��kazu nebo pokusem
    o�poru�en� referen�n� integrity.  Viz t� 'DBSystemException'.

    Datab�zov� rozhran� generuje v�p��pad� datab�zov�ch chyb
    'DBUserException' pouze tehdy, je-li schopno rozpoznat, �e se jedn�
    o�u�ivatelskou chybu.  To znamen�, �e u�ivatelsk� chyba m��e b�t
    signalizov�na i�jako prost� 'DBException'.
    
    """


class DBLoginException(DBException):
    """Datab�zov� v�jimka zp�soben� chybnou autentizac� jm�nem a heslem.

    Tato v�jimka je vyvol�v�na pouze p�i uveden�m zp�sobu autentizace, jestli�e
    u�ivatel zad� chybn� jm�no nebo heslo.

    """
    def __init__(self):
        """Inicializuj datab�zovou v�jimku s�pat�i�n�mi argumenty."""
        super_(DBLoginException).__init__\
          (self, _("Chybn� u�ivatelsk� jm�no nebo heslo"))


class DBLockException(DBException):
    """Exception thrown when trying to lock an already locked record.
    """
    def __init__(self):
        super_(DBLockException).__init__(self, None)


class DBInsertException(DBException):
    """Exception thrown when INSERT ... RETURNING can't be performed.
    """
    def __init__(self):
        super_(DBLockException).__init__(self, None)
