# -*- coding: utf-8 -*-

# Copyright (C) 2018, 2019 Tomáš Cerha <t.cerha@gmail.com>
# Copyright (C) 2001-2014 Brailcom, o.p.s.
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

import pytis
from pytis.data import ColumnSpec, Data, Type
from pytis.util import (
    compare_attr, flatten, hash_attr, is_sequence, log, rsa_encrypt,
    super_, translations, Locked, ProgramError, DEBUG, EVENT, OPERATIONAL,
)

_ = translations('pytis-data')

# Obecné třídy


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
                 crypto_names=(), **kwargs):
        """Inicializuj tabulku s napojením do databáze.

        Argumenty:

          bindings -- sekvence instancí třídy 'DBBinding'
          ordering -- stejné jako v předkovi
          distinct_on -- sequence of column names to add as a DISTINCT TO part
            to SELECT commands
          arguments -- sequence of 'DBBinding' instances defining table
            arguments, when the table is actually a row returning function.
            Otherwise it must be 'None'.
          crypto_names -- sequence of additional crypto names (strings)
            required by the object but not defined in bindings
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
        if __debug__:
            log(DEBUG, 'Database instance bindings:', self._bindings)
        columns, key = self._db_bindings_to_column_spec(self._bindings)
        if __debug__:
            log(DEBUG, 'Database instance columns:', columns)
        if __debug__:
            log(DEBUG, 'Database instance key:', key)
        self._distinct_on = distinct_on
        assert is_sequence(crypto_names), crypto_names
        if __debug__:
            assert all([isinstance(n, basestring) for n in crypto_names]), crypto_names
        crypto_names = list(crypto_names)
        for b in bindings:
            if isinstance(b, DBColumnBinding):
                n = b.crypto_name()
                if n is not None and n not in crypto_names:
                    crypto_names.append(n)
        self._crypto_names = crypto_names
        try:
            del kwargs['key']
        except Exception:
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
        if __debug__:
            log(DEBUG, 'Sleep')
        self.close()

    def arguments(self):
        """Return the value of argument 'arguments' passed to the constructor."""
        return self._arguments

    def crypto_names(self):
        """Return sequence of all crypto names (strings) required by the data object.

        It includes crypto names given in the constructor and crypto names
        defined in bindings.

        """
        return self._crypto_names


class DBConnectionPool:

    def __init__(self, connection_creator, connection_closer):
        if __debug__:
            log(DEBUG, 'Creating a new pool')
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
        with Locked(self._lock):
            for c in flatten(self._pool.values()):
                try:
                    self._connection_closer(c)
                except Exception:
                    pass

    def _connection_spec_id(self, connection_spec):
        c = connection_spec
        schemas = c.schemas()
        if isinstance(schemas, list):
            schemas = tuple(schemas)
        return (c.database(), c.host(), c.port(), c.user(), c.password(), c.sslmode(), schemas,)

    def get(self, connection_spec):
        pool = self._pool
        spec_id = self._connection_spec_id(connection_spec)
        with Locked(self._lock):
            try:
                connections = pool[spec_id]
            except KeyError:
                pool[spec_id] = connections = []
            try:
                allocated_connections = self._allocated_connections[spec_id]
            except KeyError:
                allocated_connections = self._allocated_connections[spec_id] \
                    = weakref.WeakKeyDictionary()
            c = None
            broken_connections_present = False
            while connections:
                c_candidate = connections.pop()
                if c_candidate.connection_info('broken'):
                    broken_connections_present = True
                else:
                    c = c_candidate
                    if __debug__:
                        log(DEBUG, 'Available connections:', connections)
                    break
            if c is None or broken_connections_present:
                gc.collect()
            if c is None:
                if ((pytis.config.connection_limit is not None and
                     len(allocated_connections) >= pytis.config.connection_limit)):
                    if __debug__:
                        log(EVENT, "Connections summary:")
                        for c in allocated_connections.keys():
                            log(EVENT, "Connection:", c.connection_info('last_access'))
                    raise DBSystemException(_(u"Too many database connections"))
                else:
                    c = self._connection_creator(connection_spec)
                    if __debug__:
                        log(DEBUG, 'New connection created:', c)
                    allocated_connections[c] = True
        if __debug__:
            log(DEBUG, 'Passing connection:', c)
        return c

    def put_back(self, connection_spec, connection):
        pool = self._pool
        spec_id = self._connection_spec_id(connection_spec)
        with Locked(self._lock):
            try:
                connections = pool[spec_id]
            except KeyError:
                pool[spec_id] = connections = []
            if ((pytis.config.max_pool_connections is None or
                 len(connections) < pytis.config.max_pool_connections)):
                connections.append(connection)
        if __debug__:
            log(DEBUG, 'Connection returned to pool:', connection)

    def flush(self, close):
        with Locked(self._lock):
            for connections in self._allocated_connections.values():
                for c in connections.keys():
                    try:
                        close(c)
                    except Exception:
                        pass
            for connections in self._pool.values():
                for c in connections:
                    try:
                        close(c)
                    except Exception:
                        pass
            self._allocated_connections = {}
            self._pool = {}

    def info(self):
        """Return list of current transaction commands of all known connections.

        It's useful when debugging idle transactions or connection leaks.

        """
        info = []
        for connections in self._allocated_connections.values():
            for c in connections.keys():
                info.append(c.connection_info('transaction_commands'))
        return info


# Specifikační třídy


class DBConnection:
    """Database connection parameters specification.

    The instance of this class just holds information about database connection
    parameters.  It doesn't perform any operations related to database
    connections and doesn't even hold any acual connection objects.

    The parameters are described in the '__init__' docstring.

    This connection parameters are mostly immutable, except for login
    credentials (username, password and encryption passwords), which may be
    updated within an existing instance when the application obtains them from
    the user in runtime.

    """
    _OPTIONS = ('user', 'password', 'host', 'port', 'database', 'sslmode', 'schemas',)

    def __init__(self, user=None, password=None, host=None, port=None,
                 database=None, sslmode='allow', schemas=None, alternatives={},
                 crypto_password=None, _name=None):
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
          crypto_password -- rsa encrypted password for database encrypted
            areas
          alternatives -- dictionary of alternative connection parameters.
            Alternative database connections are identified by name and data
            object specifications may refer to these names to use connect to
            alternative data sources (thus the number and names of alternative
            connections is application specific).  The dictionary keys are
            connection names and the values are dictionaries of connection
            options ('user', 'password', 'host', 'database', ...) with the same
            meaning as the corresponding arguments.

        Arguments with the value 'None' will be ignored when connecting to the
        database.

        """
        self._user = user
        self._password = password
        self._crypto_password = crypto_password
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
        # Save db communication key to avoid unnecessary db queries
        self._db_key = None

    def _options(self, exclude=()):
        return dict([(option, self.__dict__['_' + option])
                     for option in self._OPTIONS
                     if option not in exclude and self.__dict__['_' + option] is not None])

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
        return compare_attr(self, other, ['_' + option for option in self._OPTIONS])

    def __hash__(self):
        return hash_attr(self, ['_' + option for option in self._OPTIONS])

    def select(self, name):
        """Return the specification instance activated for given connection name.

        Available connection names are defined by the 'alternatives' constructor argument.  'None'
        is reserved for the default connection.  The list of alternative connections is kept, so it
        is possible to switch back to a previous connection using 'select()' again.

        """
        if name == self._name:
            return self
        else:
            connection_options = self._alternatives[name]
            for key, attr in (('user', '_user',), ('password', '_password',),):
                if key not in connection_options:
                    connection_options[key] = getattr(self, attr)
            options = dict(connection_options, alternatives=self._alternatives, _name=name)
            return self.__class__(**options)

    def update_login_data(self, user, password):
        """Set given login parameters in the instance.

        Arguments:

          user -- database user as a string or 'None'
          password -- database password as a string or 'None'

        """
        self._user = user
        self._password = password
        import pytis.extensions
        # If `password' is wrong then the following dbfunction call will
        # initiate new password dialog and will invoke this method recursively.
        # In such a case we must not set the wrong password here.
        self._crypto_password = None
        if self._db_key is None:
            self._db_key = pytis.extensions.dbfunction('pytis_crypto_db_key',
                                                       ('key_name_', pytis.data.sval('pytis'),))
        self._crypto_password = rsa_encrypt(self._db_key, password)

    def crypto_password(self):
        """Return crypto password, string.

        This is typically the same as the login password, but it may be
        different, e.g. when there is no login password.

        """
        return self._crypto_password

    def set_crypto_password(self, password):
        """Set instance crypto password to 'password'.

        Arguments:

          password -- the password, string

        """
        self._crypto_password = password

    def db_key(self):
        """Return db communication key, string."""
        return self._db_key

    def set_db_key(self, db_key):
        """Set instance db key.

        Arguments:

          db_key -- the database communication key

        """
        self._db_key = db_key

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
        assert isinstance(id, basestring)
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

    def __init__(self, id, table, column, related_to=None, type_=None, crypto_name=None,
                 encrypt_empty=True, **kwargs):
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
          crypto_name -- if not 'None' then the column is stored encrypted in
            the database and the argument value is a string identifier of the
            protection area.  There can be defined several different protection
            areas identified by corresponding crypto names in the application,
            protected by different passwords.  Not all types support
            encryption, it is an error to set encryption here for column types
            which don't support it.
          encrypt_empty -- if True (default) then encrypt also None values (and
            empty values when they are represented by None values).  Otherwise
            store empty values as NULLs in the database.  Empty values should
            be commonly encrypted in the databases so that there is no
            information about secret data.  But when you want to allow
            unauthorized users to work with encrypted data in a limited way,
            e.g. to insert new records with empty secret values, then setting
            this argument to False is useful.
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
        assert isinstance(table, basestring), table
        assert isinstance(column, basestring), column
        assert isinstance(type_, Type) or type(type_) == type(Type) or type_ is None, type_
        assert crypto_name is None or isinstance(crypto_name, basestring), crypto_name
        assert encrypt_empty is None or isinstance(encrypt_empty, bool), encrypt_empty
        self._table = table
        self._column = column
        self._related_to = related_to
        self._type = type_
        self._crypto_name = crypto_name
        self._encrypt_empty = encrypt_empty
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

    def crypto_name(self):
        """Return 'crypto_name' value given in the constructor, 'None' or string."""
        return self._crypto_name

    def encrypt_empty(self):
        """Return 'encrypt_empty' value given in the constructor, boolean."""
        return self._encrypt_empty

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


# Databázové výjimky


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
        if message is None:
            message = _(u"Database error")
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


class DBRetryException(DBSystemException):
    """Exception thrown on broken database connections.

    It is thrown when a database connection is broken and could be possibly
    recovered (this typically happens after database server restart), but
    automatic recovery inside low level database communication is not possible
    due to an open transaction.  Higher level parts of the database backend may
    catch this exception and attempt to reexecute the transaction
    queries(typically when it is raised at the beginning of a transaction).

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
        super_(DBLoginException).__init__(self, _(u"Invalid user name or password"))


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


class NotWithinSelect(ProgramError):
    """Exception thrown on an attempt for a cursor operation when there is no active select."""

    def __init__(self):
        ProgramError.__init__(self, 'Not within select')
