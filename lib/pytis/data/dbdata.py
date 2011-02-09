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

"""Implementace tabulkových dat pro relační databázové stroje.

Základem modulu je abstraktní třída 'DBData'.  Ta je potomkem třídy
'data.Data' a obohacuje ji o specifikaci napojení tabulkových dat do relační
databáze a rozšiřuje některé ji metody.

Jednotliví potomci třídy 'DBData' implementují databázový přístup pro
jednotlivé databázové stroje (na úrovni SQL) a jejich potomci pak pro
konkrétní pythonová rozhraní k těmto strojům.

Různé databázové třídy plní následující role:

- Překlad požadavků do SQL příslušného databázového stroje.

- Implementace komunikace s databází pomocí pythonového modulu.

- Interpretace column bindings.

Každá z těchto rolí je realizována samostatnou třídou.

Kromě toho modul obsahuje pomocnou třídu 'DBConnection' pro specifikaci
databázového spojení, pomocné třídy založené na 'DBBinding' pro specifikaci
napojení uvnitř 'DBData' a databázové výjimky odvozené ze třídy
'DBException'.  Třídě 'DBBinding' a jejím potomkům je třeba věnovat
obzvláštní pozornost, pomocí nich se definují nejdůležitější věci týkající se
tabulky.

"""


# TODO: V implementaci se počítá s tím, že z databáze nedostaneme nesmysly
# (z důvodu jiného než programátorské chyby).  Tyto situace sice obecně
# ošetřujeme metáním DBException, to ale nestačí, protože taková situace může
# vzniknout i nějakým problémem na DB serveru.  Je tedy v budoucnu
# bezpodmínečně nutno tyto situace řádně ošetřovat a prošetřovat.  Nejprve je
# ovšem nutno definovat nějaký mechanismus ošetřování chyb.


import gc
import thread
import weakref

from pytis.data import *


### Obecné třídy


class DBData(Data):
    """Tabulková data mapovaná do relační databáze.

    Třída rozšiřuje svého předka o specifikaci napojení sloupců tabulkových dat
    do relační databáze (blíže viz metoda '__init__') a o některé argumenty
    jeho veřejných metod.  Zavádí také veřejnou metodu 'sleep'.

    Tato třída je nezávislá na konkrétním použitém databázovém stroji a
    pythonovém modulu.  Závislé věci jsou implementovány v potomcích této
    třídy, přičemž se doporučuje v nejbližší podtřídě napsat potomky pro
    jednotlivé databázové stroje (tj. realizovat metody na úrovni SQL a jiných
    vlastností daného databázového stroje) a teprve nad nimi definovat třídy
    odpovídajícím jednotlivým pythonovým modulům.  Je též možné využít
    vícenásobné dědičnosti a definovat například \"odbočující\" třídu pro
    DB-SIG API.

    Všechny metody této třídy přístupující k datům mohou metat 'DBException'.
    
    """
    def __init__(self, bindings, ordering=None, distinct_on=(), arguments=None,
                 **kwargs):
        """Inicializuj tabulku s napojením do databáze.

        Argumenty:
        
          bindings -- sekvence instancí třídy 'DBBinding'
          ordering -- stejné jako v předkovi
          distinct_on -- sequence of column names to add as a DISTINCT TO part
            to SELECT commands
          arguments -- sequence of 'DBBinding' instances defining table
            arguments, when the table is actually a row returning function.
            Otherwise it must be 'None'.
          kwargs -- k předání předkovi

        Sloupce datové tabulky se určí automaticky na základě 'bindings'.
        Jejich typy jsou určeny typy odpovídajících dat v databázi(přesné
        mapování závisí na potomcích třídy a není zde specifikováno).  Klíčovým
        sloupcem tabulky je první sloupec z 'bindings', který je klíčovým
        sloupcem v databázi.

        Žádné dva prvky 'bindings' by neměly mít shodné id, s výjimkou skrytých
        bindings, která mají jako id prázdný řetězec.
        
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
        if __debug__: log(DEBUG, 'Bindings databázové instance', self._bindings)
        columns, key = self._db_bindings_to_column_spec(self._bindings)
        if __debug__: log(DEBUG, 'Sloupce databázové instance:', columns)
        if __debug__: log(DEBUG, 'Klíč databázové instance:', key)
        self._distinct_on = distinct_on
        try:
            del kwargs['key']
        except:
            pass
        super(DBData, self).__init__(columns=columns, key=key, ordering=ordering, **kwargs)

    def _db_bindings_to_column_spec(self, bindings):
        """Vrať dvojici (COLUMNS, KEY) odpovídající argumentům 'Data.__init__'.

        V této třídě metoda vrátí jednoduše seznam sloupců v podobě instancí
        'ColumnSpec' s názvy odpovídajícími identifikátorům 'bindings' (i co
        se týče jejich pořadí), přičemž typ všech sloupců je nastaven na
        nejobecnější typ 'types_.Type'.  Jako KEY je vrácen první sloupec
        uvedený v 'bindings'; pokud jsou 'bindings' prázdné, je vráceno 'None'.

        Tato implementace metody je velmi hrubá a metoda je určena
        k předefinování v potomcích třídy.

        """
        columns = map(lambda b: ColumnSpec(b.id(), Type()), bindings)
        if columns:
            key = columns[0]
        else:
            key = None
        return columns, key

    def _db_column_binding(self, col_id):
        """Vrať binding sloupce 'col_id'.

        Pokud pro 'col_id' není žádné binding definováno, vrať 'None'.

        Argumenty:
        
          col_id -- identifikace tabulkového sloupce jako string
          
        """
        for b in self._bindings:
            if b.id() == col_id:
                return b
        return None

    def table(self, col_id):
        """Vrať tabulku sloupce 'col_id'.

        Argumenty:

           viz metoda '_db_column_binding'.
        """
        b = self._db_column_binding(col_id)
        if b:
            return b.table()
        return None

    def sleep(self):
        """Uvolni systémové zdroje využívané instancí a umožni její zrušení.

        Pokud instance využívá některé nedostatkové systémové zdroje jako
        například spojení do databáze nebo procesy, tato metoda by je měla
        uvolnit.  Druhou funkcí metody je uvolnit prostředky (například hlídací
        procesy), které znemožňují zrušení instance.

        Tato metoda by měla být volána vždy, když je trvale nebo na delší dobu
        ukončeno používání instance.

        Zavoláním této metody se neznemožňuje další použití instance, lze
        nadále využívat všechny její veřejné metody.  Je-li pak ovšem třeba
        opět uvolnit zdroje, je nutno tuto metodu zavolat znovu.

        """
        if __debug__: log(DEBUG, 'Uspání')
        self.close()

    def arguments(self):
        """Return the value of argument 'arguments' passed to the constructor."""
        return self._arguments
        

class DBConnectionPool:

    def __init__(self, connection_creator, connection_closer):
        if __debug__: log(DEBUG, 'Vytvářím nový pool')
        self._lock = thread.allocate_lock()
        self._pool = {}
        self._connection_creator = connection_creator
        self._connection_closer = connection_closer
        self._allocated_connections = {}

    def __del__(self):
        # Pro jistotu uzavíráme všechna spojení, přestože by to mělo být
        # zbytečné a zajištěno automaticky v pyPgSQL; třeba to pomůže
        # problému pozůstalých spojení.  Navíc pro jistotu zamykáme, co
        # kdyby ...
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
                if __debug__: log(DEBUG, 'Spojení k dispozici', connections)
                c = connections.pop()
            else:
                gc.collect()
                if (config.connection_limit is not None and
                    len(allocated_connections) >= config.connection_limit):
                    if __debug__:
                        log(EVENT, "Přehled evidovaných spojení:")
                        for c in allocated_connections.keys():
                            log(EVENT, "Spojení:", c.connection_info('last_access'))
                    raise DBSystemException(_("Příliš mnoho databázových spojení"))
                else:
                    c = self._connection_creator(connection_spec)
                    if __debug__: log(DEBUG, 'Vytvořeno nové spojení:', c)
                    allocated_connections[c] = True
            return c
        c = with_lock(self._lock, lfunction)
        if __debug__: log(DEBUG, 'Předávám spojení:', c)
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
        if __debug__: log(DEBUG, 'Do poolu vráceno spojení:', connection)


### Specifikační třídy

    
class DBConnection:
    """Specifikace parametrů pro připojení do databáze.

    Jedná se čistě o specifikaci, třída sama žádné operace týkající se napojení
    do databáze neprovádí ani neudržuje skutečný objekt spojení.  Pro popis
    parametrů spojení viz metoda '__init__'.

    Tato třída je míněna jako immutable a tudíž může být libovolně sdílena.
    Pro malé úpravy specifikace lze využít metodu 'modified()'.

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
        """Vrať databázového uživatele jako string nebo 'None'."""
        return self._user

    def password(self):
        """Vrať heslo databázového uživatele jako string nebo 'None'."""
        return self._password

    def host(self):
        """Vrať jméno databázového serveru jako string nebo 'None'."""
        return self._host

    def port(self):
        """Vrať jméno portu na serveru jako integer nebo 'None'."""
        return self._port

    def database(self):
        """Vrať jméno databáze jako string nebo 'None'."""
        return self._database

    def sslmode(self):
        return self._sslmode

    def schemas(self):
        """Return schemas given in the constructor."""
        return self._schemas

    def __cmp__(self, other):
        """Vrať 0, právě když 'self' a 'other' definují totéž spojení.

        Dvě instance této třídy reprezentují totéž spojení, právě když se
        rovnají odpovídající si parametry zadané jejich konstruktorům.

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
    """Definice napojení dat do databáze.

    Tato definice je využívána třídou 'DBData' a jejími potomky.
    
    Tato třída je pouze abstraktním základem, který definuje pouze stringový
    identifikátor instance napojení.  Mechanismy pro skutečné definice
    databázových napojení poskytují až potomci této třídy.

    Třída je specifikována jako immutable a jako taková může být libovolně
    sdílena.
    
    """
    def __init__(self, id):
        """Definuj napojení.

        Argumenty:
        
          id -- identifikátor napojení, libovolný string
          
        """
        assert isinstance(id, str)
        self._id = id

    def id(self):
        """Vrať identifikátor napojení zadaný v konstruktoru."""
        return self._id


class DBColumnBinding(DBBinding):
    """Vazba 1-1 tabulkového sloupce na databázový sloupec.

    Tato vazba je dána jménem databázové tabulky a jejího sloupce, na který
    se tabulkový sloupec mapuje.
    
    Třída je specifikována jako immutable a jako taková může být libovolně
    sdílena.
    
    """
    def __init__(self, id, table, column, related_to=None, type_=None, **kwargs):
        """Define a column binding.

        Argumenty:
        
          id -- id sloupce
          table -- jméno databázové tabulky, do které má být tabulkový sloupec napojen, jako string
          column -- jméno sloupce databázové tabulky, na který má být tabulkový sloupec napojen,
            jako string nebo sekvence (viz níže)
          related_to -- instance 'DBColumnBinding' specifikující, se kterým sloupcem jiné
            databázové tabulky je tento sloupec v relaci; pokud s žádným, je hodnotou 'None'
          type_ -- explicitně specifikovaný typ sloupce jako instance třídy 'Type' nebo 'None'.
            Je-li 'None', bude typ sloupce určen automaticky dle informací získaných přímo
            z databáze.  V opačném případě bude typem hodnota tohoto argumentu, která musí
            odpovídat typu sloupce v databázi (být jeho specializací).
          **kwargs -- explicitně definované klíčové argumenty typu.  Pokud jsou definovány
            libovolné klíčové argumenty, budou tyto předány konstruktoru implicitního datového
            typu.  Typ v takovém případě nesmí být explicitně určen argumentem 'type_'.

        Napojení může být *skryté*, což znamená, že přímo neodpovídá žádnému sloupci datové
        tabulky.  To se může stát například v případě, že binding je definováno *pouze* kvůli
        specifikaci relace mezi tabulkami (prostřednictvím argumentu 'related_to').  U skrytého
        napojení nezáleží na hodnotě příslušného sloupce a tudíž k těmto hodnotám ani nelze
        přistupovat.  Napojení je považováno za skryté, právě když řetězec 'id' je prázdný.

        'related_to' je obecně nesymetrická relace přibližně odpovídající specifikátoru REFERENCES.

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
        """Vrať jméno napojené databázové tabulky jako string."""
        return self._table

    def column(self):
        """Vrať jméno napojeného sloupce jako string."""
        return self._column

    def related_to(self):
        """Vrať instanci DBColumnBinding napojeného sloupce nebo 'None'."""
        return self._related_to

    def type(self):
        """Vrať instanci typu sloupce z konstruktoru nebo 'None'."""
        return self._type
    
    def kwargs(self):
        """Vrať slovník klíčových argumentů konstruktoru dat. typu sloupce."""
        return self._kwargs
    
    def is_hidden(self):
        """Vrať pravdu, právě když sloupec není přítomen v datové tabulce."""
        return self._is_hidden

    def __str__(self):
        return ('<DBCB: id=%s, table=%s, column=%s, related_to=%s, type=%s, is_hidden=%s>') % \
               (self._id, self._table, self._column, self._related_to, self._type, self._is_hidden)

    def __cmp__(self, other):
        return compare_attr(self, other,
                            ('_table', '_column', '_related_to', '_type', '_is_hidden'))
    
    def __hash__(self):
        return hash_attr(self, ('_table', '_column', '_related_to', '_type', '_is_hidden'))



### Databázové výjimky


class DBException(Exception):
    """Výjimka nahazovaná v případě vzniku databázové chyby.

    Všechny databázové výjimky metané ven ze třídy 'DBData' jsou tohoto typu.

    Třída ve svém konstruktoru automaticky zaloguje základní informace
    o výjimce.

    """
    def __init__(self, message, exception=None, *args):
        """Inicializuj výjimku a zaloguj informace o ní.

        Argumenty:

          message -- lidsky čitelné oznámení o chybě, string; může být též
            'None', v kterémžto případě se doplní standardní zpráva
            o databázové chybě, to je však vhodné používat pouze v případě, kdy
            nemá smysl uživateli sdělovat bližší popis chyby
          exception -- výjimka, která tuto výjimku způsobila, instance třídy
            'Exception'; nebo 'None' (neznamená nutně, že chyba nebyla
            způsobena výjimkou, tato výjimka pouze nemusí být podstatná)
          args -- libovolné další argumenty, které mají být spolu s 'message' a
            'exception' předány konstruktoru nadtřídy

        """
        if message == None:
            message = _("Databázová chyba")
        super_(DBException).__init__(self, message, exception, *args)
        log(OPERATIONAL, 'Database exception', (message,) + args)
        if exception:
            log(OPERATIONAL, 'Wrapped database exception', (exception,) + exception.args)
        self._message = message
        self._exception = exception

    def message(self):
        """Vrať lidsky čitelnou zprávu zadanou v konstruktoru, jako string."""
        return self._message

    def exception(self):
        """Vrať databázovou výjimku zadanou v konstruktoru nebo 'None'."""
        return self._exception


class DBSystemException(DBException):
    """Výjimka nahazovaná v případě vzniku systémové databázové chyby.

    Systémová chyba je chyba související se systémovými prostředky, například
    chyba spojení do databáze.  Viz též 'DBUserException'.

    Databázové rozhraní generuje v případě databázových chyb
    'DBSystemException' pouze tehdy, je-li schopno rozpoznat, že se jedná
    o systémovou chybu.  To znamená, že systémová chyba může být signalizována
    i jako prostá 'DBException'.
    
    """


class DBUserException(DBException):
    """Výjimka nahazovaná v případě vzniku uživatelské databázové chyby.

    Uživatelská chyba je chyba způsobená syntakticky nebo sémanticky chybnou
    databázovou operací, například chybným použitím SQL příkazu nebo pokusem
    o porušení referenční integrity.  Viz též 'DBSystemException'.

    Databázové rozhraní generuje v případě databázových chyb
    'DBUserException' pouze tehdy, je-li schopno rozpoznat, že se jedná
    o uživatelskou chybu.  To znamená, že uživatelská chyba může být
    signalizována i jako prostá 'DBException'.
    
    """


class DBLoginException(DBException):
    """Databázová výjimka způsobená chybnou autentizací jménem a heslem.

    Tato výjimka je vyvolávána pouze při uvedeném způsobu autentizace, jestliže
    uživatel zadá chybné jméno nebo heslo.

    """
    def __init__(self):
        """Inicializuj databázovou výjimku s patřičnými argumenty."""
        super_(DBLoginException).__init__\
          (self, _("Chybné uživatelské jméno nebo heslo"))


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
