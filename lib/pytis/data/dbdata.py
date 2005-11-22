# -*- coding: iso-8859-2 -*-

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

"""Implementace tabulkových dat pro relační databázové stroje.

Základem modulu je abstraktní třída 'DBData'.  Ta je potomkem třídy
'data.Data' a obohacuje ji o specifikaci napojení tabulkových dat do relační
databáze a rozšiřuje některé ji metody.

Jednotliví potomci třídy 'DBData' implementují databázový přístup pro
jednotlivé databázové stroje (na úrovni SQL) a jejich potomci pak pro
konkrétní pythonová rozhraní k těmto strojům.  Globální proměnná modulu
'DBDataDefault' definuje tu z těchto tříd, která má být používána v aplikaci
pro přístup k databázovým datům.

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

Důležitá je globální proměnná 'DBDataDefault', která obsahuje konkrétní třídu
používanou aplikací pro přístup do databáze.  Aplikace by měla pro přístup do
databáze používat výhradně instance této třídy.

"""


# TODO: V implementaci se počítá s tím, že z databáze nedostaneme nesmysly
# (z důvodu jiného než programátorské chyby).  Tyto situace sice obecně
# ošetřujeme metáním DBException, to ale nestačí, protože taková situace může
# vzniknout i nějakým problémem na DB serveru.  Je tedy v budoucnu
# bezpodmínečně nutno tyto situace řádně ošetřovat a prošetřovat.  Nejprve je
# ovšem nutno definovat nějaký mechanismus ošetřování chyb.


import copy
import operator
import re
import select
import string
import thread
import time
import weakref
import types as pytypes

import mx.DateTime
from pyPgSQL import libpq

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
    def __init__(self, bindings, ordering=None):
        """Inicializuj tabulku s napojením do databáze.

        Argumenty:
        
          bindings -- sekvence instancí třídy 'DBBinding'
          ordering -- stejné jako v předkovi

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
        if __debug__: log(DEBUG, 'Bindings databázové instance', self._bindings)
        columns, key = self._db_bindings_to_column_spec(self._bindings)
        if __debug__: log(DEBUG, 'Sloupce databázové instance:', columns)
        if __debug__: log(DEBUG, 'Klíč databázové instance:', key)
        Data.__init__(self, columns, key, ordering)

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


class _DBConnectionPool:

    def __init__(self, connection_creator):
        if __debug__: log(DEBUG, 'Vytvářím nový pool')
        self._lock = thread.allocate_lock()
        self._pool = {}
        self._connection_creator = connection_creator

    def __del__(self):
        # Pro jistotu uzavíráme všechna spojení, přestože by to mělo být
        # zbytečné a zajištěno automaticky v pyPgSQL; třeba to pomůže
        # problému pozůstalých spojení.  Navíc pro jistotu zamykáme, co
        # kdyby ...
        self._lock.acquire()
        try:
            for c in flatten(self._pool.values()):
                try:
                    c.finish()
                except:
                    pass
        finally:
            self._lock.release()

    def get(self, connection_spec, data=None):
        pool = self._pool
        spec_id = tuple(connection_spec.__dict__.values())
        lock = self._lock
        lock.acquire()
        try:
            try:
                connections = pool[spec_id]
            except KeyError:
                pool[spec_id] = connections = []
            if connections:
                if __debug__: log(DEBUG, 'Spojení k dispozici', connections)
                c = connections.pop()
            else:
                c = self._connection_creator(connection_spec, data)
                if __debug__: log(DEBUG, 'Vytvořeno nové spojení:', c)
        finally:
            lock.release()
        if __debug__: log(DEBUG, 'Předávám spojení:', c)
        return c

    def put_back(self, connection_spec, connection):
        pool = self._pool
        spec_id = tuple(connection_spec.__dict__.values())
        lock = self._lock
        lock.acquire()
        try:
            try:
                connections = pool[spec_id]
            except IndexError:
                pool[spec_id] = connections = []
            connections.append(connection)
        finally:
            lock.release()        
        if __debug__: log(DEBUG, 'Do poolu vráceno spojení:', connection)



### Postgresové třídy a funkce


# Modifikace tabulek se oznamuje zasláním notifikace `MODIF_table', kde `table'
# je jméno modifikované tabulky.


def _pypg_query(connection, query, data=None, outside_transaction=True,
                conmaker=None):
    result = None
    def do_query(connection):
        try:
            return connection.query(query)
        except Exception, e:
            try:
                connection.finish()     # pro jistotu
            except:
                pass
            raise e
    try:
        result = do_query(connection)
    except libpq.InterfaceError, e:
        raise DBUserException(None, e, query)
    except libpq.ProgrammingError, e:
        raise DBUserException(None, e, query)
    except libpq.OperationalError, e:
        if not outside_transaction:
            connection = None
            raise DBSystemException(_("Operační chyba v databázi"), e, query)
        if conmaker:
            connection = conmaker()
            try:
                result = do_query(connection)
            except Exception, e:
                connection = None
                raise DBSystemException(_("Operační chyba v databázi"), e,
                                        query)
        else:
            connection = None
            raise DBSystemException(_("Operační chyba v databázi"), e, query)
    except libpq.InternalError, e:
        connection = None
        raise DBException(None, e, query)
    except libpq.IntegrityError, e:
        raise DBUserException(_("Pokus o porušení integrity dat"),
                              e, query)
    return result, connection


class DBDataPostgreSQL(DBData):
    """Datová tabulka s napojením do PostgreSQL.

    Tato třída překládá požadavky do SQL, není však implementačně závislá na
    konkrétním použitém postgresovém modulu pro Python.
    
    """
    # Pro funkčnost s konkrétním databázovým rozhraním k PostgreSQL je nutno
    # předefinovat metodu '_pg_query'.  Dále je nutno dodefinovat metodu
    # '_db_bindings_to_column_spec' a metody označené komentářem '#redefine'.

    _PG_LOCK_TABLE = '_rowlocks'
    _PG_LOCK_TABLE_LOCK = '_rowlocks_real'
    _PG_LOCK_TIMEOUT = 30         # perioda updatu v sekundách

    class _PgBuffer:
        # Dříve to býval buffer, nyní se přeměňuje na "buffer-cache".

        def __init__(self):
            if __debug__: log(DEBUG, 'Nový buffer')
            self.reset()
            
        def reset(self):
            """Kompletně resetuj buffer."""
            if __debug__: log(DEBUG, 'Resetuji buffer')
            self._buffer = []
            # _dbpointer ... pozice ukazovátka kursoru v databázi, na který
            #   prvek kursoru počínaje od 0 ukazuje
            # _dbposition ... pozice začátku bufferu v databázi, číslo prvku
            #   kurzoru počínaje 0, který odpovídá prvnímu prvku bufferu
            # _pointer ... pozice ukazovátka v bufferu, ukazuje na poslední
            #   přečtený prvek, nemusí vždy ukazovat dovnitř bufferu
            self._dbposition = 0
            self._dbpointer = self._pointer = -1

        def current(self):
            """Vrať aktuální řádek a jeho pozici v databázi počínaje od 0.

            Výsledek je vrácen jako dvojice (ROW, POSITION).  Je-li aktuální
            řádek mimo buffer, je ROW 'None'.  Je-li aktuální pozice mimo
            buffer, je POSITION je -1 nebo počet řádků selectu.

            """
            buffer = self._buffer
            pointer = self._pointer
            position = self._dbposition + pointer
            if pointer < 0 or pointer >= len(buffer):
                row = None
            else:
                row = buffer[pointer]
            return row, position

        def fetch(self, direction, number_of_rows):
            """Vrať řádek nebo 'None' a updatuj ukazovátka.

            Pokud řádek není v bufferu, je vráceno 'None' a předpokládá se
            následné volání metod 'correction()' a 'fill()'.

            """
            buffer = self._buffer
            pointer = self._pointer
            if direction == FORWARD:
                pointer += 1
            elif direction == BACKWARD:
                pointer -= 1
            else:
                raise ProgramError('Invalid direction', direction)
            if pointer < 0 or pointer >= len(buffer):
                if __debug__: log(DEBUG, 'Buffer miss:', pointer)
                pos = self._dbposition + pointer
                # Interní ukazovátko po obyčejném minutí neupdatujeme, protože
                # přijde fill a pokus o znovuvytažení hodnoty, s novým updatem
                # ukazovátka.  Avšak pokud jsme kompletně mimo rozsah dat, není
                # tato zdrženlivost namístě a je nutno ukazovátko posunout na
                # správnou pozici, tj. mimo rozsah dat.
                if pos < 0:
                    self._pointer = -1 - self._dbposition
                elif pos >= number_of_rows:
                    self._pointer = number_of_rows - self._dbposition
                return None
            self._pointer = pointer
            result = buffer[pointer]
            if __debug__: log(DEBUG, 'Buffer hit:', pointer) #, str(result))
            return result

        def correction(self, direction, number_of_rows):
            """Vrať argument pro DB operaci SKIP před naplněním bufferu.

            Kladná návratová hodnota odpovídá posunu vpřed, záporná posunu
            zpět.
            
            Databázové ukazovátko je updatováno jako kdyby SKIP byl proveden.

            """
            if __debug__: log(DEBUG, 'Žádost o korekci:',
                (self._dbpointer, self._dbposition, self._pointer, direction))
            pointer = self._pointer
            buflen = len(self._buffer)
            pos = self._dbposition + pointer
            if pointer > buflen or pointer < -1:
                # Dostali jsme se daleko za buffer, je nutno provést DB skip.
                # TODO: zde by mohlo být dobré nastavit pozici tak, aby byla
                # načtena ještě nějaká data proti směru bufferu.  Jak to ale
                # udělat čistě?
                #print "  *", self._dbpointer, self._dbposition, pointer, pos
                if pos >= 0:
                    pos = min(pos, number_of_rows)
                else:
                    pos = max(pos, -1)
                correction = pos - self._dbpointer
                self._buffer = []
                self._pointer = -1
                self._dbpointer = self._dbpointer + correction
                self._dbposition = self._dbpointer + 1
            elif (direction == FORWARD and pointer >= buflen - 1) or \
                 (direction == BACKWARD and pointer <= 1):
                # Jsme u hranice bufferu, provedeme DB skip bez mazání bufferu.
                # Rozsah v podmínce je zvolen tak, aby ošetřil i předchozí
                # buffer miss.
                correction = pos - self._dbpointer
                self._dbpointer = self._dbpointer + correction
            else:
                # Jsme uvnitř bufferu, žádný DB skip se nekoná.
                correction = 0
            if __debug__: log(DEBUG, 'Určená korekce:', correction)
            return correction

        def goto(self, position):
            """Updatuj databázovou pozici nastavenou bez vědomí bufferu.

            Argumenty:

              position -- číslo prvku cursoru začínajícího od 0, na který
                ukazovátko databázového kurzoru právě ukazuje

            """
            self._pointer = position - self._dbposition
            self._dbpointer = position
            
        def skip(self, count, direction, number_of_rows):
            """Proveď skip.

            Argumenty:

              count -- počet řádků, o kolik se má skok provést
              direction -- jedna ze směrových konstant modulu
              number_of_rows -- počet řádků v aktuálním selectu

            Vrací: Počet skutečně přeskočených řádků ve směru 'direction'.
            
            """
            pointer = self._pointer
            if direction == FORWARD:
                pointer = pointer + count
            elif direction == BACKWARD:
                pointer = pointer - count
            else:
                raise ProgramError('Invalid direction', direction)
            pos = self._dbposition + pointer
            if pos < -1:
                pointer = -1 - self._dbposition
            elif pos > number_of_rows:
                pointer = number_of_rows - self._dbposition
            result = pointer - self._pointer
            if direction == BACKWARD:
                result = -result
            self._pointer = pointer
            return result
        
        def fill(self, rows, direction, extra_move=False):
            """Naplň se daty 'rows' a updatuj ukazovátka."""
            # extra_move je tu kvůli tomu, že pokud dojde ve fetchmany
            # k překročení hranic dat ještě před získáním požadovaného počtu
            # řádků, musí být dbpointer přesunut ještě o jednu pozici dál (mimo
            # data).
            if __debug__: log(DEBUG, 'Plním buffer:', direction)            
            n = len(rows)
            buffer = self._buffer
            buflen = len(buffer)
            dbpointer = self._dbpointer
            dbposition = self._dbposition
            pointer = self._pointer + dbposition
            import config
            retain = max(config.cache_size - n, 0)
            cutoff = max(buflen - retain, 0)
            if direction == FORWARD:
                if dbposition + buflen - 1 == dbpointer:
                    buffer = buffer[cutoff:] + rows
                    dbposition = dbposition + cutoff
                else:
                    buffer = rows
                    dbposition = dbpointer + 1
                dbpointer = dbpointer + n
                if extra_move:
                    dbpointer = dbpointer + 1
            elif direction == BACKWARD:
                rows.reverse()
                if dbposition == dbpointer:
                    buffer = rows + buffer[:retain]
                else:
                    buffer = rows
                dbpointer = dbpointer - n
                dbposition = dbpointer
                if extra_move:
                    dbpointer = dbpointer - 1
            else:
                raise ProgramError('Invalid direction', direction)
            self._pointer = pointer - dbposition
            self._dbpointer = dbpointer
            self._dbposition = dbposition
            self._buffer = buffer

        def copy(self):
            """Vrať \"rozumnou\" kopii instance."""
            # Nepoužíváme postupy modulu `copy', protože potřebujeme něco mezi
            # hlubokou a mělkou kopií.
            copy_ = self.__class__()
            copy_._buffer = copy.copy(self._buffer)
            copy_._pointer = self._pointer
            copy_._dpointer = self._dpointer
            copy_._dbposition = self._dbposition
            return copy_

        def __str__(self):
            buffer = self._buffer
            pointer = self._pointer
            max = len(buffer) - 1
            if max < 0:
                bufstr = ''
            elif max == 0:
                bufstr = str(buffer[0])
            elif max == 1:
                bufstr = '%s\n%s' % (buffer[0], buffer[1])
            elif pointer <= 0 or pointer >= max:
                bufstr = '%s\n...\n%s' % (buffer[0], buffer[-1])
            elif max == 2:
                bufstr = '%s\n%s\n%s' % (buffer[0], buffer[1], buffer[2])
            elif pointer == 1:
                bufstr = '%s\n%s\n...\n%s' % (buffer[0], buffer[1], buffer[-1])
            elif pointer == max - 1:
                bufstr = '%s\n...\n%s\n%s' % \
                         (buffer[0], buffer[-2], buffer[-1])
            else:
                bufstr = '%s\n...\n%s\n...\n%s' % \
                         (buffer[0], buffer[pointer], buffer[-1])
            return '<PgBuffer: db=%d, start=%d, index=%d\n%s>' % \
                   (self._dbpointer, self._dbposition, self._pointer, bufstr)

    def __init__(self, bindings, key, dbconnection_spec, ordering=None):
        """Inicializuj databázovou tabulku dle uvedených specifikací.

        Argumenty:
        
          bindings -- stejné jako v předkovi
          key -- binding klíčového sloupce datové tabulky, musí být jeden
            z prvků 'bindings' nebo sekvence prvků z 'bindings'
          dbconnection_spec -- instance třídy 'DBConnection' definující
            parametry připojení, nebo funkce bez argumentů vracející takovou
            instanci 'DBConnection'
          ordering -- stejné jako v předkovi
        
        """
        if __debug__: log(DEBUG, 'Vytvářím databázovou tabulku')
        import config
        if isinstance(dbconnection_spec, DBConnection):
            self._pg_dbconnection_spec = lambda : dbconnection_spec
        else:
            self._pg_dbconnection_spec = dbconnection_spec
        if is_sequence(key):
            self._key_binding = tuple(key)
        else:
            self._key_binding = (key,)
        DBData.__init__(self, bindings, ordering)
        self._pg_is_in_select = False
        self._pg_buffer = self._PgBuffer()
        self._pg_number_of_rows = None
        self._pg_changed = False
        self._pg_initial_select = False
        self._pg_make_row_template = \
            self._pg_create_make_row_template(self._columns)
        # NASTAVENÍ CACHE
        # Protože pro různé parametry (rychlost linky mezi serverem a klientem,
        # velikost paměti atd.), je vhodné různé nastavení cache,
        # budeme parametry nastavovat z konfiguračního souboru.
        # Pozor, config.cache_size je využíváno přímo v _PgBuffer.
        # Zde tyto hodnoty zapamatujeme jako atributy objektu, protože jsou
        # potřeba v kritických částech kódu a čtení konfigurace přeci jen trvá.
        self._pg_initial_fetch_size = config.initial_fetch_size
        self._pg_fetch_size = config.fetch_size
        self._db_encoding = config.db_encoding
        
        
    # Abstraktní metody pro potomky
    
    def _pg_query(self, query, outside_transaction=False):
        """Proveď SQL příkaz 'query' a vrať výsledek.

        Argumenty:
        
          query -- SQL příkaz PostgreSQL jako string
          outside_transaction -- právě když je pravda, je query provedeno mimo
            aktuálně prováděnou transakci, je-li jaká

        Návratovou hodnotou je sekvence sekvencí jako tabulka vrácených řádků a
        sloupců.  Prvky vnitřních sekvencí jsou prostá data ve formě strings.
        Příklady návratových hodnot:
        
          (('1', 'prvni'), ('2', 'druhy'), ('3', 'treti'))
          ()
          [['42']]

        Metoda musí řádně ošetřovat výjimky a v případě jejich výskytu nahodit
        výjimku 'DBException' nebo některého jejího potomka.

        **Pozor**: Metoda může být volána již z konstruktoru nebo některé z jím
        volaných metod.  Jediná inicializace, na kterou se v instancích této
        třídy může spolehnout, je nastavení '_dbconnection_spec', vše ostatní
        může být ještě neinicializováno.

        V této třídě je tato metoda abstraktní a musí být v některém z potomků
        předefinována.
        
        """
        # Poznámka ke specifikaci: Reprezentace dat řetězci se může zdát
        # poněkud nevhodná, protože u některých rozhraní to může znamenat
        # konverzi dat na nějaký typ a pak zpět na řetězec.  To je ovšem cena,
        # kterou rádi zaplatíme za srozumitelnost celé záležitosti.  Problémem
        # není ani mrhání CPU cykly, protože kód poběží na klientech, kteří
        # se stejně vesměs flákají.
        pass

    # Metody pro transakce

    def _pg_begin_transaction (self):
        self._pg_query ('begin')
        
    def _pg_commit_transaction (self):
        self._pg_query ('commit')
        
    def _pg_rollback_transaction (self):
        self._pg_query ('rollback')

    # Pomocné metody

    def _pg_create_make_row_template(self, columns):
        template = []
        for c in columns:
            id = c.id()
            type = c.type()
            if isinstance(type, String):
                typid = 0
            elif isinstance(type, (Time, DateTime)):
                typid = 2
            else:
                typid = 99
            template.append((id, typid, type))
        return template
    
    def _pg_make_row_from_raw_data(self, data_):
        if not data_:
            return None
        row_data = []
        data_0 = data_[0]
        i = 0
        for id, typid, type in self._pg_make_row_template:            
            dbvalue = data_0[i]
            i += 1
            if typid == 0:              # string
                if dbvalue is None:
                    v = None
                else:
                    import config
                    v = unicode(dbvalue, self._db_encoding)
                value = Value(type, v)
            elif typid == 2:            # time
                value, err = type.validate(dbvalue, strict=False,
                                           format=type.SQL_FORMAT, local=False)
                assert err is None
            else:
                value, err = type.validate(dbvalue, strict=False)
                assert err is None
            row_data.append((id, value))
        return Row(row_data)

    def _pg_already_present(self, row):
        key = []
        for k in self.key():
            try:
                id = k.id()
            except:
                return False
            try:
                key.append(row[id])
            except KeyError:
                return False
        return self.row(key)
    
    _pg_dt = type(mx.DateTime.DateTimeFrom('2001-01-01'))
    def _pg_value(self, value):
        if is_sequence(value):
            return tuple(map(self._pg_value, value))
        v = value.value()
        if v == None:
            result = 'NULL'
        elif isinstance(value.type(), Boolean):
            result = "'%s'" % value.type().export(v)
        elif is_anystring(v):
            result = "'%s'" % _pg_escape(v)
        elif type(v) == self._pg_dt:
            if isinstance(value.type(), Date):
                result = "'%s'" % v.strftime('%Y-%m-%d')
            elif isinstance(value.type(), Time):
                result = "'%s'" % v.strftime('%H:%M:%S')
            else:                       # DateTime
                result = "'%s'" % v.strftime('%Y-%m-%d %H:%M:%S')
        elif isinstance(value.type(), Float):
            result = value.type().export(v, locale_format=False)
        else:
            result = value.type().export(v)
        return result

    def _pg_key_condition(self, key):
        if __debug__: log(DEBUG, 'Vytvářím podmínku z klíče:', key)
        key = xtuple(key)
        keycols = map(lambda b: b.id(), self._key_binding)
        assert len(keycols) == len(key), ('Invalid key length', key, keycols)
        ands = map(EQ, keycols, key)
        condition = apply(AND, ands)
        if __debug__: log(DEBUG, 'Podmínka z klíče vytvořena:', condition)
        return condition

    # Veřejné metody a jimi přímo volané abstraktní metody

    def row(self, key):
        log(EVENT, 'Zjištění obsahu řádku:', key)
        try:
            data = self._pg_row (self._pg_value(key))
        except Exception, e:
            try:
                self._pg_rollback_transaction()
            except:
                pass
            raise e
        result = self._pg_make_row_from_raw_data(data)
        log(EVENT, 'Vrácený obsah řádku', result)
        return result

    def _pg_row (self, value):
        """Vytáhni a vrať raw data odpovídající klíčové hodnotě 'value'."""
        #redefine
        return None
        
    def select(self, condition=None, sort=(), reuse=False):
        if __debug__: log(DEBUG, 'Zahájení selectu:', condition)
        if reuse and not self._pg_changed and self._pg_number_of_rows and \
               condition == self._pg_last_select_condition and \
               sort == self._pg_last_select_sorting:
            use_cache = True
        else:
            use_cache = False
        self.close()
        self._pg_begin_transaction ()
        self._pg_is_in_select = True
        self._pg_last_select_condition = condition
        self._pg_last_select_sorting = sort
        self._pg_last_fetch_row = None
        self._pg_changed = False
        try:
            number_of_rows = self._pg_select (condition, sort)
        except Exception, e:
            try:
                self._pg_rollback_transaction()
            except:
                pass
            self._pg_is_in_select = False
            raise e
        if use_cache and number_of_rows != self._pg_number_of_rows:
            use_cache = False
        if use_cache:
            self._pg_buffer.goto(-1)
        else:
            self._pg_buffer.reset()
            self._pg_initial_select = True
        self._pg_number_of_rows = number_of_rows
        return number_of_rows

    def select_aggregate(self, operation, condition=None):
        opid = operation[0]
        t = self.find_column(operation[1]).type()
        if opid == self.AGG_COUNT:
            t = Integer()
        elif opid == self.AGG_AVG:
            if not isinstance(t, Number):
                return None
            t = Float()
        else:
            if not isinstance(t, Number):
                return None
        close_select = False
        if not self._pg_is_in_select:
            self.select(condition=condition)
            close_select = True
        try:
            data = self._pg_select_aggregate(operation, condition)
        except Exception, e:
            try:
                self._pg_rollback_transaction()
            except:
                pass
            self._pg_is_in_select = False
            raise e
        if close_select:
            self.close()
        result, error = t.validate(data[0][0])
        assert error is None, error
        return result
        
    def _pg_select (self, condition, sort, operation=None):
        """Inicializuj select a vrať počet řádků v něm nebo 'None'.

        Argumenty:

          condition -- nezpracovaný podmínkový výraz nebo 'None'
          sort -- nezpracovaná specifikace třídění nebo 'None'
          operation -- nezpracovaná specifikace agregační funkce
          
        """
        #redefine
        return None

    def fetchone(self, direction=FORWARD):
        """Stejné jako v nadtřídě.

        Metoda automaticky nereaguje na notifikace o změně dat souvisejících
        tabulek a pokračuje (nedojde-li k přerušení transakce) v dodávce
        starých dat.  Automatické přenačítání dat při změně by mohlo vést
        k výkonnostním problémům, jeho provádění je tedy ponecháno na uvážení
        aplikace, která se může nechat o změnách informovat registrací
        prostřednictvím metody 'add_callback_on_change()'.

        """
        if __debug__:
            log(DEBUG, 'Vytažení řádku ze selectu ve směru:', direction)
        assert direction in(FORWARD, BACKWARD), \
               ('Invalid direction', direction)
        if not self._pg_is_in_select:
            # Pokusy o rekonstrukci fetche nevedou k ničemu dobrému.  Původně
            # tu bylo provedení nového selectu a hledání původního řádku
            # v něm.  Pomineme-li možné sémantické problémy této operace,
            # nepomůže nám toto ani výkonnostně, protože musíme prohledat celá
            # data až po kýžené místo -- to už je ovšem můžeme rovnou znovu
            # načíst.
            #
            # Lepší je používat explicitní nový select s argumentem `reuse'
            # v kombinaci s metodou `skip'.  Při "pokračování" selectu nám více
            # než o ten samý řádek jde spíše o tu samou pozici v datech, což
            # tento postup podporuje.  Při vhodném mechanismu bufferování
            # odpadnou i nejzávažnější výkonnostní problémy.
            raise ProgramError('Not within select')
        # Tady začíná opravdové vytažení aktuálních dat
        buffer = self._pg_buffer
        row = buffer.fetch(direction, self._pdbb_select_rows)
        if row:
            result = row
        else:
            # Kurzory v PostgreSQL mají spoustu chyb.  Například často
            # kolabují při překročení hranic dat a mnohdy správně nefunguje
            # FETCH BACKWARD.  V následujícím kódu se snažíme některé
            # nejčastější chyby PostgreSQL obejít.
            def skip():
                xcount = buffer.correction(FORWARD, self._pg_number_of_rows)
                if xcount < 0:
                    xcount = -xcount
                    skip_direction = BACKWARD
                else:
                    skip_direction = FORWARD
                if xcount > 0:
                    try:
                        result = self._pg_skip(xcount, skip_direction,
                                               exact_count=True)
                    except Exception, e:
                        try:
                            self._pg_rollback_transaction()
                        except:
                            pass
                        self._pg_is_in_select = False
                        raise e
            skip()
            import config
            if self._pg_initial_select:
                self._pg_initial_select = False
                std_size = self._pg_initial_fetch_size
            else:
                std_size = self._pg_fetch_size
            current_row_number = buffer.current()[1]
            last_row_number = min(current_row_number + 1,
                                  self._pg_number_of_rows)
            if direction == FORWARD:
                size = min(self._pg_number_of_rows-last_row_number, std_size)
            else:
                if current_row_number <= 0:
                    if current_row_number == 0:
                        skipped = buffer.skip(1, BACKWARD,
                                              self._pg_number_of_rows)
                        assert skipped == 1, skipped
                        skip()
                    return None
                size = min(self._pg_number_of_rows, std_size)
            assert size >= 0 and size <= self._pg_number_of_rows
            if direction == FORWARD:
                xskip = None
            else:
                xskip = buffer.skip(size, BACKWARD, self._pg_number_of_rows)
                skip()
            try:
                data_ = self._pg_fetchmany(size, FORWARD)
            except Exception, e:
                try:
                    self._pg_rollback_transaction()
                except:
                    pass
                self._pg_is_in_select = False
                raise e
            if data_:
                row_data = [self._pg_make_row_from_raw_data([d]) for d in data_]
                buffer.fill(row_data, FORWARD, len(row_data)!=size)
                if xskip:
                    buffer.skip(xskip, FORWARD, self._pg_number_of_rows)
                result = buffer.fetch(direction, self._pdbb_select_rows)
            else:
                result = None
        self._pg_last_fetch_row = result
        if __debug__: log(DEBUG, 'Vrácený řádek', str(result))
        return result

    def _pg_fetchmany (self, count, direction):
        """Vrať 'count' řádků selectu jako raw data."""
        #redefine
        return []
    
    def last_row_number(self):
        return self._pg_buffer.current()[1]

    def last_select_condition(self):
        return self._pg_last_select_condition

    def last_select_condition_sql(self):
        return self._pdbb_condition2sql(self._pg_last_select_condition)

    def skip(self, count, direction=FORWARD):
        if __debug__: log(DEBUG, 'Přeskočení řádků:', (direction, count))
        assert type(count) == type(0) and count >= 0, \
               ('Invalid count', count)
        assert direction in (FORWARD, BACKWARD), \
               ('Invalid direction', direction)
        result = self._pg_buffer.skip(count, direction,
                                      self._pg_number_of_rows)
        if count > 0:
            self._pg_last_fetch_row = None
        if __debug__: log(DEBUG, 'Přeskočeno řádků:', result)
        return result

    def _pg_skip(self, count, direction, exact_count=False):
        """Přeskoč 'count' řádků v 'direction' a vrať jejich počet nebo 'None'.
        """
        #redefine
        return None

    def rewind(self):
        if not self._pg_is_in_select:
            raise ProgramError('Not within select')
        __, pos = self._pg_buffer.current()
        if pos >= 0:
            self.skip(pos+1, BACKWARD)
        
    def search(self, condition, direction=FORWARD):
        if __debug__: log(DEBUG, 'Hledání řádku:', (condition, direction))
        assert direction in (FORWARD, BACKWARD), \
               ('Invalid direction', direction)
        if not self._pg_is_in_select:
            raise ProgramError('Not within select')
        row, pos = self._pg_buffer.current()
        if not row and pos >= 0 and pos < self._pg_number_of_rows:
            self.skip(1, BACKWARD)
            row = self.fetchone()
        if not row and (pos < 0 and direction == BACKWARD or \
                        pos >= 0 and direction == FORWARD):
            result = 0
        else:
            try:
                result = self._pg_search(row, condition, direction)
            except Exception, e:
                try:
                    self._pg_rollback_transaction()
                except:
                    pass
                self._pg_is_in_select = False
                raise e
        if __debug__: log(DEBUG, 'Výsledek hledání:', result)
        return result

    def _pg_search(self, row, condition, direction):
        """Vyhledej ve směru 'direction' první řádek od 'row' dle 'condition'.

        Vrací: Vzdálenost od řádku 'row' jako kladný integer nebo 0, pokud
        takový řádek neexistuje.

        """
        #redefine
        return None

    def close(self):
        if __debug__: log(DEBUG, 'Explicitní ukončení selectu')
        if self._pg_is_in_select:
            self._pg_commit_transaction()
            self._pg_is_in_select = False

    def insert(self, row, after=None, before=None):
        assert after is None or before is None, \
               'Both after and before specified'
        log(ACTION, 'Vložení řádku', (row, after, before))
        self._pg_begin_transaction ()
        try:
            # Jestliže je definováno ordering, které je součástí klíče, bude
            # nově vložený řádek nutně unikátní.
            if (not self._ordering or \
                (self._ordering[0] not in map(lambda c: c.id(), self.key()))
                ) and \
               self._pg_already_present(row):
                msg = 'Řádek s tímto klíčem již existuje'
                result = msg, False
                log(ACTION, msg)
            else:
                positioned = after or before
                if after:
                    neighbor = after = self.row(after)
                elif before:
                    neighbor = before = self.row(before)
                if positioned and (not neighbor):
                    msg = 'Zadaný sousední řádek nenalezen'
                    log(ACTION, msg,
                        (after, before))
                    result = msg, False
                else:
                    r = self._pg_insert (row, after=after, before=before)
                    result = r, True
        except Exception, e:
            try:
                self._pg_rollback_transaction()
            except:
                pass
            raise e
        self._pg_commit_transaction ()
        self._pg_send_notifications()
        if result[1]:
            log(ACTION, 'Řádek vložen', result)
        return result

    def _pg_insert(self, row, after=None, before=None):
        """Vlož 'row' a vrať jej jako nová raw data nebo vrať 'None'."""
        #redefine
        return None
    
    def update(self, key, row):
        key = xtuple(key)
        log(ACTION, 'Update řádku:', key)
        log(ACTION, 'Nová data', str(row))
        self._pg_begin_transaction ()
        try:
            origrow = self.row(key)
            if origrow:
                ordering = self._ordering
                if ordering:
                    row = copy.copy(row)
                    for id in ordering:
                        try:
                            row[id] = origrow[id]
                        except KeyError:
                            row.append(id, origrow[id])
                keys = map(ColumnSpec.id, self.key())
                new_key = []
                for i in range(len(keys)):
                    try:
                        v = row[keys[i]]
                    except KeyError:
                        v = key[i]
                    new_key.append(v)
                new_key = tuple(new_key)
                if new_key != key and self._pg_already_present(row):
                    msg = 'Řádek s tímto klíčem již existuje'
                    result = msg, False
                    log(ACTION, msg, key)
                else:
                    n = self._pg_update(self._pg_key_condition(key), row)
                    if n == 0:
                        result = None, False
                    else:
                        new_row = self.row(new_key)
                        result = new_row, True
            else: # not origrow
                msg = 'Řádek s daným klíčem neexistuje'
                result = msg, False
                log(ACTION, msg, key)
        except Exception, e:
            try:
                self._pg_rollback_transaction()
            except:
                pass
            raise e
        self._pg_commit_transaction ()
        self._pg_send_notifications()
        if result[1]:
            log(ACTION, 'Řádek updatován', result)
        return result
    
    def update_many(self, condition, row):
        log(ACTION, 'Update řádků:', condition)
        log(ACTION, 'Nová data', str(row))
        self._pg_begin_transaction ()
        try:
            ordering = self._ordering
            if ordering:
                new_row_items = []
                for k, v in row.items():
                    if k not in ordering:
                        new_row_items.append((k, v))
                row = Row(new_row_items)
            result = self._pg_update (condition, row)
        except Exception, e:
            try:
                self._pg_rollback_transaction()
            except:
                pass
            raise e
        self._pg_commit_transaction ()
        self._pg_send_notifications()
        if result:
            log(ACTION, 'Řádky updatovány:', result)
        return result
        
    def _pg_update(self, condition, row):
        """Updatuj řádky identifikované 'condition'.

        Vrací: Počet updatovaných řádků.

        """
        #redefine
        return None

    def delete(self, key):
        log(ACTION, 'Mazání řádku:', key)
        self._pg_begin_transaction ()
        try:
            result = self._pg_delete (self._pg_key_condition(key))
        except Exception, e:
            try:
                self._pg_rollback_transaction()
            except:
                pass
            raise e
        self._pg_commit_transaction ()
        self._pg_send_notifications()
        log(ACTION, 'Řádek smazán', result)
        return result
    
    def delete_many(self, condition):
        log(ACTION, 'Mazání řádků:', condition)
        self._pg_begin_transaction ()
        try:
            result = self._pg_delete (condition)
        except Exception, e:
            try:
                self._pg_rollback_transaction()
            except:
                pass
            raise e
        self._pg_commit_transaction ()
        self._pg_send_notifications()
        log(ACTION, 'Řádky smazány', result)
        return result

    def _pg_delete (self, condition):
        """Smaž řádek identifikovaný podmínkou 'condition'.

        Vrací: Počet smazaných řádků.

        """
        #redefine
        return 0

    def lock_row(self, key):
        # Pro zamykání je vyžadována existence zamykací tabulky se speciálními
        # vlastnostmi, která je definována v souboru `db.sql'.
        log(EVENT, 'Zamykám řádek:', map(str, xtuple(key)))
        self._pg_begin_transaction();
        try:
            self._pg_query('lock table %s' % self._PG_LOCK_TABLE_LOCK)
            row = self.row(key)
            if not row:
                self._pg_rollback_transaction()
                return 'Záznam neexistuje'
            oidcols = filter(lambda c: isinstance(c.type(), Oid),
                             self.columns())
            cids = map(lambda c: c.id(), oidcols)
            oids = map(lambda i, row=row: row[i].value(), cids)
            for oid in oids:
                data = self._pg_query('select usename from %s where row = %d'\
                                      % (self._PG_LOCK_TABLE, oid))
                if data:
                    self._pg_rollback_transaction()
                    return 'uživatel `%s\'' % data[0][0]
            lock_ids = []
            for oid in oids:
                self._pg_query('insert into %s (row) values (%d)' % \
                               (self._PG_LOCK_TABLE, oid))
                data = self._pg_query('select id from %s where row = %d' % \
                                      (self._PG_LOCK_TABLE, oid))
                lock_ids.append(data[0][0])
            self._pg_lock_ids = lock_ids
            self._pg_commit_transaction()
        except DBException, e:
            try:
                self._pg_rollback_transaction()
            except:
                pass
            raise e
        DBDataPostgreSQL.__bases__[0].lock_row(self, key)
        update_commands = \
          map(lambda id, self=self: 'update %s set id = id where id = %s' % \
              (self._PG_LOCK_TABLE, id),
              lock_ids)
        thread.start_new_thread(self._pg_locking_process,
                                (key, update_commands))
        log(EVENT, 'Řádek zamčen')
        return None

    def unlock_row(self):
        log(EVENT, 'Odemykám řádek')
        DBDataPostgreSQL.__bases__[0].unlock_row(self)
        for id in self._pg_lock_ids:
            try:
                self._pg_query('delete from %s where id = %s' % \
                               (self._PG_LOCK_TABLE, id),
                               outside_transaction=True)
            except DBException:
                pass
        self._pg_lock_ids = None
        log(EVENT, 'Řádek odemčen')

    def _pg_locking_process(self, locked_row, update_commands):
        if __debug__: log(DEBUG, 'Nastartován zamykací proces')
        while True:
            time.sleep(self._PG_LOCK_TIMEOUT)
            if self._locked_row != locked_row:
                return
            for command in update_commands:
                try:
                    self._pg_query(command, outside_transaction=True)
                except DBException, e:
                    if __debug__:
                        log(DEBUG, 'Chyba příkazu obnovy řádku', (e, command))
            if __debug__: log(DEBUG, 'Zámek updatován')


def _pg_escape(string_):
    return string_.replace("\\", "\\\\").replace("'", "\\'")


def _pg_encoding(enc):
    ENCODING_MAPPING = {'utf-8': 'unicode',
                        'iso-8859-1': 'latin1',
                        'iso-8859-2': 'latin2',
                        'iso-8859-3': 'latin3',
                        'iso-8859-4': 'latin4',
                        'iso-8859-9': 'latin5',
                        'iso-8859-10': 'latin6',
                        'iso-8859-13': 'latin7',
                        'iso-8859-14': 'latin8',
                        'iso-8859-15': 'latin9',
                        'iso-8859-16': 'latin10',
                        'iso-8859-5': 'iso_8859_5',
                        'iso-8859-6': 'iso_8859_6',
                        'iso-8859-7': 'iso_8859_7',
                        'iso-8859-8': 'iso_8859_8',
                        'iso8859-1': 'latin1',
                        'iso8859-2': 'latin2',
                        'iso8859-3': 'latin3',
                        'iso8859-4': 'latin4',
                        'iso8859-9': 'latin5',
                        'iso8859-10': 'latin6',
                        'iso8859-13': 'latin7',
                        'iso8859-14': 'latin8',
                        'iso8859-15': 'latin9',
                        'iso8859-16': 'latin10',
                        'iso8859-5': 'iso_8859_5',
                        'iso8859-6': 'iso_8859_6',
                        'iso8859-7': 'iso_8859_7',
                        'iso8859-8': 'iso_8859_8',
                        'ascii': 'sql_ascii',
                        }
    enc = enc.lower().strip()
    if ENCODING_MAPPING.has_key(enc):
        return ENCODING_MAPPING[enc]
    else:
        return enc
        
def _pypg_new_connection(spec, data):
    # Toto musí být funkce, protože nesmí být vázána na konkrétní instanci.
    if not isinstance(spec, DBConnection):
        spec = spec()
    if __debug__: log(DEBUG, 'Vytvářím nové DB spojení', spec)
    # Sestav connection string
    connection_string = ''
    for option, accessor in (('user', DBConnection.user),
                             ('password', DBConnection.password),
                             ('host', DBConnection.host),
                             ('port', DBConnection.port),
                             ('dbname', DBConnection.database)):
        value = accessor(spec)
        if value != None:
            connection_string = "%s %s='%s'" % \
                                (connection_string, option, _pg_escape(value))
    # Otevři spojení
    if __debug__: log(DEBUG, 'Připojovací řetězec:', connection_string)
    try:
        connection = libpq.PQconnectdb(connection_string)
        try:
            import config
            encoding = _pg_encoding(config.db_encoding)
            connection.query('set client_encoding to %s' % encoding)
        except libpq.DatabaseError, e:
            raise DBException(_("Nelze nastavit client_encoding na %s"  
                                % config.db_encoding), e)
    except libpq.DatabaseError, e:
        if e.args and string.find(string.lower(e.args[0]), 'password') != -1:
            raise DBLoginException()
        else:
            raise DBException(_("Nelze se připojit k databázi"), e)
    # Nastavujeme serializované transakce, abychom v rámci jedné transakce
    # nemohli dostat různé výsledky pro opakované selecty.
    try:
        connection.query('set session characteristics as transaction '+\
                         'isolation level serializable')
    except libpq.DatabaseError, e:
        try:
            connection.finish()         # pro jistotu
        except:
            pass
        raise DBSystemException(None, e)
    if __debug__: log(DEBUG, 'Spojení otevřeno')
    if data is not None:
        data.update_access_groups(spec)
    # Vrať spojení
    return connection


_pypg_connection_pool_ = None
def _pypg_connection_pool():
    # Connection pool nelze inicializovat přímo u jeho definice kvůli cyklickým
    # závislostem modulů (config -> dbdata -> log -> config).
    global _pypg_connection_pool_
    if _pypg_connection_pool_ is None:
        _pypg_connection_pool_ = _DBConnectionPool(_pypg_new_connection)
    return _pypg_connection_pool_


class DBDataPyPgSQL(DBDataPostgreSQL):
    """Implementace postgresové tabulkové třídy prostřednictvím pyPgSQL."""

    NOTIFIERS = {}

    class _PgNotifier:

        # Jsou tu dva zámky -- pozor na uváznutí!

        def __init__(self, connection_pool, connection_spec):
            if __debug__: log(DEBUG, 'Vytvoření')
            self._data_lock = thread.allocate_lock()
            self._data_objects = weakref.WeakKeyDictionary()
            self._connection_lock = thread.allocate_lock()
            self._connection = None
            thread.start_new_thread(self._listen,
                                    (connection_pool, connection_spec))

        def _register(self, notification):
            # Zamykáme zde kvůli možnosti současného vyvolání této metody
            # z `register' i naslouchacího threadu.
            if __debug__: log(DEBUG, 'Registruji notifikaci:', notification)
            connection = self._connection
            lock = self._connection_lock
            if connection:
                lock.acquire()
                try:
                    try:
                        connection.query('listen %s' % notification)
                    except Exception, e:
                        try:
                            connection.finish() # pro jistotu
                        except:
                            pass
                        raise DBSystemException(
                            _("Databázová chyba listen"), e)
                finally:
                    lock.release()
            if __debug__: log(DEBUG, 'Notifikace zaregistrována:', notification)

        def _listen(self, pool, spec):
            if __debug__: log(DEBUG, 'Nový listener')
            error_pause = 1
            while True:
                if __debug__: log(DEBUG, 'Napichuji se na nové spojení')
                connection = self._connection = pool.get(spec)
                notiflist = reduce(lambda x, y: x + y,
                                   self._data_objects.values(), [])
                if __debug__: log(DEBUG, 'Notifikace k registraci:', notiflist)
                try:
                    # connection do poolu nikdy nevracíme, takže na něj můžeme
                    # navěsit, co je nám libo.
                    for n in remove_duplicates(notiflist):
                        self._register(n)
                except DBException, e:
                    time.sleep(error_pause)
                    error_pause = error_pause * 2
                    continue
                while True:
                    if __debug__: log(DEBUG, 'Hlídám vstup', connection)
                    try:
                        select.select([connection.socket], [], [], None)
                    except Exception, e:
                        if __debug__: log(DEBUG, 'Chyba na socketu', e.args)
                        break
                    if __debug__: log(DEBUG, 'Přišel vstup')
                    lock = self._connection_lock
                    lock.acquire()
                    try:
                        try:
                            connection.consumeInput()
                            notice = connection.notifies()
                        except Exception, e:
                            if __debug__: log(DEBUG, 'Databázová chyba', e.args)
                            break
                        notifications = []
                        if notice:
                            self._pg_changed = True
                            if __debug__: log(DEBUG, 'Zaregistrována změna dat')
                        while notice:
                            n = string.lower(notice.relname)
                            notifications.append(n)
                            notice = connection.notifies()
                    finally:
                        lock.release()
                    if __debug__:
                        log(DEBUG, 'Načteny notifikace:', notifications)
                    self._invoke_callbacks(notifications)

        def _invoke_callbacks(self, notifications):
            if __debug__: log(DEBUG, 'Volám callbacky')
            lock = self._data_lock
            lock.acquire()
            try:
                data_objects = copy.copy(self._data_objects)
            finally:
                lock.release()
            for d, ns in data_objects.items():
                for n in ns:
                    if n in notifications:
                        if __debug__:
                            log(DEBUG, 'Volám callbacky datového objektu:', d)
                        d._call_on_change_callbacks()
                        break

        def register(self, data, notification):
            if __debug__: log(DEBUG, 'Registruji notifikaci:', notification)
            lock = self._data_lock
            lock.acquire()
            try:
                try:
                    notifications = self._data_objects[data]
                except KeyError:
                    self._data_objects[data] = notifications = []
                notification = string.lower(notification)
                notifications.append(notification)
            finally:
                lock.release()
            self._register(notification)
            if __debug__: log(DEBUG, 'Notifikace zaregistrována')

    # Metody hlavní třídy

    def __init__(self, bindings, key, dbconnection_spec, ordering=None):
        self._pypg_connection = []
        super_(DBDataPyPgSQL).__init__(self, bindings, key,
                                       dbconnection_spec, ordering)
        
    def _pypg_add_notifications(self):
        notifications = self._pg_notifications
        if not notifications:
            return
        s = self._pg_dbconnection_spec()
        spec = (s.host(), s.port(), s.database())
        try:
            notifier = DBDataPyPgSQL.NOTIFIERS[spec]
        except KeyError:
            notifier = DBDataPyPgSQL.NOTIFIERS[spec] = \
              DBDataPyPgSQL._PgNotifier(_pypg_connection_pool(), s)
        for n in notifications:
            notifier.register(self, n)

    def _pypg_new_connection(self):
        return _pypg_new_connection(self._pg_dbconnection_spec(), self)

    def _pypg_allocate_connection(self):
        if __debug__:
            if len(self._pypg_connection) >= 3:
                if __debug__:
                    log(DEBUG, 'Podezřele velká hloubka spojení:',
                        len(self._pypg_connection))
        connection = self._pypg_get_connection(self, outside_transaction=True)
        self._pypg_connection.append(connection)
        
    def _pypg_deallocate_connection(self):
        self._pypg_return_connection(self._pypg_connection.pop())
        
    def _pypg_get_connection(self, data, outside_transaction=False):
        if outside_transaction or not self._pypg_connection:
            pool = _pypg_connection_pool()
            return pool.get(self._pg_dbconnection_spec(), data)
        else:
            return self._pypg_connection[-1]
        
    def _pypg_return_connection(self, connection):
        pool = _pypg_connection_pool()
        pool.put_back(self._pg_dbconnection_spec(), connection)
        
    def _pg_begin_transaction (self):
        self._pypg_allocate_connection()
        DBDataPyPgSQL.__bases__[0]._pg_begin_transaction(self)
        
    def _pg_commit_transaction (self):
        DBDataPyPgSQL.__bases__[0]._pg_commit_transaction(self)
        self._pypg_deallocate_connection()
        
    def _pg_rollback_transaction (self):
        DBDataPyPgSQL.__bases__[0]._pg_rollback_transaction(self)
        self._pypg_deallocate_connection()

    def _pg_query(self, query, outside_transaction=False, backup=False,
                  group_update=True):
        import config
        if type(query) is pytypes.UnicodeType:
            query = query.encode(config.db_encoding)
        if group_update:
            data_arg = self
        else:
            data_arg = None
        # Získej DB spojení
        connection = self._pypg_get_connection(data_arg, outside_transaction)
        # Proveď query
        if __debug__: log(DEBUG, 'SQL dotaz', query)
        try:
            result, connection = \
                    _pypg_query(connection, query, data=data_arg,
                                outside_transaction=outside_transaction,
                                conmaker=self._pypg_new_connection)
        finally:
            # Vrať DB spojení zpět
            if connection and outside_transaction:
                self._pypg_return_connection(connection)
        if backup and self._pdbb_logging_command:
            assert not outside_transaction, \
                   ('Backed up SQL command outside transaction', query)
            # Zde nemůže dojít k významné záměně pořadí zalogovaných
            # příkazů, protože všechny DML příkazy jsou uzavřeny
            # v transakcích a ty konfliktní jsou díky serializaci
            # automaticky správně řazeny.
            _pypg_query(connection,
                        self._pdbb_logging_command % _pg_escape(query),
                        data=data_arg, outside_transaction=False)
        # Získej a vrať data
        if result.resultType == libpq.RESULT_DML:
            if result.cmdTuples:
                data = [[result.cmdTuples]]
            else:
                data = [['0']]
        elif result.resultType == libpq.RESULT_DDL: # move, begin, etc.
            data = result.cmdStatus
        else:
            data = []
            for row in range(result.ntuples):
                row_data = []
                for col in range(result.nfields):
                    try:
                        value = result.getvalue(row, col)                    
                    except libpq.InterfaceError, e:
                        raise DBUserException(None, e)
                    except libpq.TypeError, e:
                        raise DBUserException(None, e)
                    if value is libpq.PG_True:
                        value = 'T'
                    elif value is libpq.PG_False:
                        value = 'F'
                    elif value is None:
                        pass
                    elif (type(value) in (type(0), type(0L), type(0.0),
                                          type(libpq.PgInt2(0))) \
                          or hasattr(libpq,'PgInt8') \
                          and type(value) == type(libpq.PgInt8(0))):
                        value = str(value)                        
                    else:
                        assert is_string(value), \
                               DBException\
                               ('Unexpected pyPgSQL type received', None,
                                value)
                    row_data.append(value)
                data.append(row_data)
        if __debug__: log(DEBUG, 'Výsledek SQL dotazu', data)
        return data

        
class PostgreSQLStandardBindingHandler(object):
    """Interpretace sémantiky specifikace napojení do databáze.

    Tato třída řeší problematiku naplnění významu specifikace napojení sloupců
    datové tabulky na data v databázi PostgreSQL.  Nedědí žádnou datovou
    třídu, pouze implementuje metody týkající se interpretace specifikace
    sloupců, je tudíž vhodná k podědění v některém z potomků 'data.DBData'.

    Současná implementace této třídy podporuje sloupcovou specifikační třídu
    'DBColumnBinding' a jen tuto třídu.  Pro bindings navíc platí následující
    pravidla:

    - Musí být specifikováno alespoň jedno binding.

    - Modifikovat (insert, update, delete) lze pouze tabulku klíče.

    - Všechny složky klíče musí být z téže tabulky.

    Poslední pravidlo se může zdát příliš omezující, avšak není tomu tak,
    protože práci s vícenásobnými tabulkami je lepší a jednodušší implementovat
    pomocí rules na serveru, než pomocí tohoto aplikačního rozhraní.  Je pouze
    zapotřebí, aby databázový stroj tuto funkcionalitu podporoval a aby tato
    podpora fungovala.
 
    **Pozor**: Metody modifikující tabulku se nestarají o obecné udržení
    integrity dat, tj. ohlídání vlastností klíčů nebo referenční integrity je
    ponecháno na databázovém stroji.  Předpokládá se, že v případě porušení
    pravidel definovaných v databázovém stroji je příslušná transakce
    stornována.  Stejně tak metoda 'delete' pracuje na tom principu, že vymaže
    řádek *pouze z tabulky primárního klíče* napojení; předpokládá se, že data
    v ostatních tabulkách budou smazána automaticky databázovým strojem v rámci
    pravidel zachování referenční integrity.
    
    """
    _PDBB_CURSOR_NAME = 'selection'

    def __init__(self):
        self._pdbb_create_sql_commands()
        
    def _pdbb_tabcol(self, table_name, column_name):
        """Vrať zadaný sloupec zformátovaný pro SQL."""
        return '%s.%s' % (table_name, column_name)

    def _pdbb_btabcol(self, binding):
        """Vrať seznam sloupců z 'binding' zformátovaných pro SQL."""
        cols = xtuple(binding.column())
        return map(lambda c, tc=self._pdbb_tabcol, t=binding.table(): \
                   tc(t, c),
                   cols)

    def _pdbb_coalesce(self, ctype, value):
        """Vrať string 'value' zabezpečený pro typ sloupce 'ctype' v SQL."""
        if ctype is None or isinstance(ctype, String) or value == 'NULL':
            default = "''"
            cast = ''
        elif isinstance(ctype, Float):
            default = '0'
            cast = '::numeric'                
        elif isinstance(ctype, Number):
            default = '0'
            cast = ''
        elif isinstance(ctype, Time):
            default = "'00:00:01'"
            cast = '::time'
        elif isinstance(ctype, DateTime):
            default = "'0000-01-01'"
            cast = '::date'
        elif isinstance(ctype, Boolean):
            default = "'F'"
            cast = '::bool'
        else:
            default = "''"
            cast = ''
        #return 'coalesce(%s%s, %s%s)' % (value, cast, default, cast)
        return '%s%s' % (value, cast)

    def _pdbb_get_table_type(self, table, column, ctype, type_kwargs=None):
        d = self._pg_query(
            ("select pg_type.typname, pg_attribute.atttypmod, "+\
             "pg_attribute.attnotnull "+\
             "from pg_class, pg_attribute, pg_type "+\
             "where pg_class.oid = pg_attribute.attrelid and "+\
             "pg_class.relname = '%s' and "+\
             "pg_attribute.attname = '%s' and "+\
             "pg_attribute.atttypid = pg_type.oid") % \
            (table, column),
            outside_transaction=True)
        d1 = self._pg_query(
            ("select pg_attrdef.adsrc "+\
             "from pg_class, pg_attribute, pg_attrdef "+\
             "where pg_class.oid = pg_attrdef.adrelid and "+\
             "pg_class.oid = pg_attribute.attrelid and "+\
             "pg_class.relname = '%s' and "+\
             "pg_attribute.attname = '%s' and "+\
             "pg_attribute.attnum = pg_attrdef.adnum") % \
            (table, column),
            outside_transaction=True)
        try:
            type_, size_string, not_null = d[0]
        except:
            raise DBException(_("Není možno zjistit typ sloupce"), None,
                              table, column, d)
        try:
            default = d1[0][0]
        except:
            default = ''
        serial = (default[:len('nextval')] == 'nextval')
        return self._pdbb_get_type(type_, size_string, not_null, serial,
                                   ctype=ctype, type_kwargs=type_kwargs)
    
    def _pdbb_get_type(self, type_, size_string, not_null, serial,
                       ctype=None, type_kwargs=None):
        # Zde lze doplnit další používané standardní typy z PostgreSQL
        TYPE_MAPPING = {'bool': Boolean,
                        'bpchar': String,
                        'char': String,
                        'date': Date,
                        'time': Time,
                        'smallint': Integer,                        
                        'int2': Integer,                        
                        'int4': Integer,
                        'int8': Integer,
                        'numeric': Float,
                        'float8': Float,
                        'oid': Oid,
                        'name': String,
                        'text': String,
                        'timestamp': DateTime,
                        'timestamptz': DateTime,
                        'varchar': String}
        try:
            type_class_ = TYPE_MAPPING[type_]
        except KeyError:
            raise DBException('Unhandled database type', None, type_)
        if ctype is None:
            if type_kwargs is None:
                type_kwargs = {}
            if not_null in (1, 'T') and not type_kwargs.has_key('not_null'):
                type_kwargs['not_null'] = True
            if type_class_ == String:
                if type_ != 'text':
                    try:
                        size = int(size_string) - 4
                    except:
                        size = None
                    if size < 0:
                        size = None
                    type_kwargs['maxlen'] = size
            elif type_class_ == Float:
                spec = int(size_string)
                precision = (spec & 0xFFFF) - 4
                if precision < 0 or precision > 100:
                    precision = None
                type_kwargs['precision'] = precision
            elif type_class_ == Integer and serial:
                type_class_ = Serial
            result = type_class_(**type_kwargs)
        else:
            assert isinstance(ctype, type_class_), \
                   ("User type doesn't match DB type", ctype, type_class_)
            result = ctype
        return result

    def _db_bindings_to_column_spec(self, bindings):
        key = []
        columns = []
        for b in bindings:
            if not b.id():              # skrytý sloupec
                continue
            enumerator, kwargs = b.enumerator(), copy.copy(b.kwargs())
            if enumerator:
                df_kwargs = {'dbconnection_spec': self._pg_dbconnection_spec()}
                e_kwargs = {'data_factory_kwargs': df_kwargs}
                for a in ('value_column', 'validity_column'):
                    if kwargs.has_key(a):
                        e_kwargs[a] = kwargs[a]
                        del kwargs[a]
                kwargs['enumerator'] = DataEnumerator(enumerator, **e_kwargs)
                #TODO: Toto je hack kvůli zpětné kompatibilitě...
                if not kwargs.has_key('not_null'):
                    kwargs['not_null'] = True
            t = self._pdbb_get_table_type(b.table(), b.column(), b.type(),
                                          type_kwargs=kwargs)
            colspec = ColumnSpec(b.id(), t)
            columns.append(colspec)
            if b in self._key_binding:
                key.append(colspec)
        assert key, DBUserException('data key column not found')
        # Přidej oid
        if not some(lambda c: isinstance(c.type(), Oid), columns):
            columns.append(ColumnSpec('oid', Oid()))
        # Hotovo
        return columns, tuple(key)

    def _pdbb_create_sql_commands(self):
        """Vytvoř šablony SQL příkazů používané ve veřejných metodách."""
        bindings = self._bindings
        for b in bindings:
            assert isinstance(b, DBColumnBinding), \
                   ('Unsupported binding specification', b)
        # Připrav parametry
        visible_bindings = filter(lambda b: b.id(), bindings)
        column_names = map(lambda b: self._pdbb_btabcol(b), visible_bindings)
        column_names = reduce(lambda x, y: x + y, column_names, [])
        column_list = string.join(column_names, ', ')
        table_names = map(lambda b: b.table(), bindings)
        table_names = remove_duplicates(table_names)
        table_list = string.join(table_names, ', ')
        if len(table_names) <= 1:
            relation = 'true'
        else:
            rels = []
            for b in bindings:
                related = b.related_to()
                if related:
                    next_rels = map (lambda c, r: '%s=%s' % (c, r),
                                     self._pdbb_btabcol(b),
                                     self._pdbb_btabcol(related))
                    rels = rels + next_rels
            relation = string.join(rels, ' and ')
        main_table = self._key_binding[0].table()
        keytabcols = map(self._pdbb_btabcol, self._key_binding)
        keytabcols = reduce(operator.add, keytabcols, [])
        key_eqs = map(lambda k: '%s=%%s' % k, keytabcols)
        key_cond = string.join(key_eqs, ' and ')
        def sortspec(dir, self=self, keytabcols=keytabcols):
            items = []
            for i in range(len(keytabcols)):
                k = keytabcols[i]
                items.append('%s %s' % (k, dir))
            return string.join(items, ',')
        ordering = sortspec('ASC')
        rordering = sortspec('DESC')
        condition = key_cond
        relation_and_condition = '(%s) and (%s)' % (relation, condition)
        oids = filter(lambda c: isinstance(c.type(), Oid), self.columns())
        assert oids, ('No oids in the data object columns',
                      map(str, self.columns()))
        oidnames = map(lambda c, m=main_table: '%s.%s' % (m, c.id()), oids)
        oidstrings = string.join(oidnames, ', ')
        # Vytvoř šablony příkazů
        self._pdbb_command_row = \
          'select %s, %s from %s where %s order by %s' % \
          (column_list, oidstrings, table_list, relation_and_condition,
           ordering)
        self._pdbb_command_count = \
          'select count(*) from %s where %%s and (%s)' % \
          (table_list, relation)
        self._pdbb_command_select = \
          ('declare %s scroll cursor for select %s, %s from %s '+\
           'where %%s and (%s) order by %%s %s') % \
          (self._PDBB_CURSOR_NAME, column_list, oidstrings, table_list,
           relation, ordering)
        self._pdbb_command_select_agg = \
          ('select %%s(%%s) from %s where %%s and (%s)' %
           (table_list, relation))
        self._pdbb_command_fetch_forward = \
          'fetch forward %%d from %s' % self._PDBB_CURSOR_NAME
        self._pdbb_command_fetch_backward = \
          'fetch backward %%d from %s' % self._PDBB_CURSOR_NAME
        self._pdbb_command_move_forward = \
          'move forward %%d from %s' % self._PDBB_CURSOR_NAME
        self._pdbb_command_move_backward = \
          'move backward %%d from %s' % self._PDBB_CURSOR_NAME
        self._pdbb_command_search_first = \
          ('select %s, %s from %s where (%s) and %%s order by %%s %s '+\
           'limit 1') % \
           (column_list, oidstrings, main_table, relation, ordering)
        self._pdbb_command_search_last = \
          ('select %s, %s from %s where (%s) and %%s order by %%s %s '+\
           'limit 1') % \
           (column_list, oidstrings, main_table, relation, rordering)
        self._pdbb_command_search_distance = \
          'select count(*) from %s where (%s) and %%s' % \
          (main_table, relation)
        self._pdbb_command_insert = \
          'insert into %s (%%s) values (%%s)' % main_table
        if self._ordering:
            ordering = ()
            for o in self._ordering:
                for b in self._bindings:
                    if b.id() == o:
                        c = xtuple(b.column())
                        break
                else:
                    raise ProgramError('Invalid ordering id', o)
                ordering = ordering + c
            ocol = ordering[0]
            eqs = map(lambda o: '%s=%%s' % o, ordering[1:])
            if eqs:
                eqstring = string.join(eqs, ' AND ')                
                xeqstring = ' AND ' + eqstring
            else:
                eqstring = xeqstring = ''
            self._pdbb_command_insert_shift = \
              ('update %s set %s=%s+1 where %s>=%%s %s' % \
               (main_table, ocol, ocol, ocol, xeqstring))
            self._pdbb_command_insert_newpos = \
              ('select max(%s) from %s where %s' % \
               (ocol, main_table, eqstring))
        oidordering = string.join(map(lambda o: '%s DESC' % o, oidnames),
                                  ', ')
        self._pdbb_command_get_last = \
          ('select %s, %s from %s where %s order by %s limit 1') % \
           (column_list, oidstrings, table_list, relation, oidordering)
        self._pdbb_command_update = \
          'update %s set %%s where (%s) and (%%s)' % (main_table, relation)
        self._pdbb_command_broken_update_preselect = \
          'select count (*) from %s where (%s) and (%%s)' % \
          (main_table, relation)
        self._pdbb_command_test_broken_update = \
          ("select 'yes' from pg_class, pg_rewrite "+\
           "where pg_rewrite.ev_type = 2 and "+\
           "pg_rewrite.is_instead = 't' and "+\
           "pg_class.oid = pg_rewrite.ev_class and "+\
           "pg_class.relname = '%s'") % main_table
        self._pdbb_command_delete = \
          'delete from %s where %%s' % main_table
        self._pdbb_command_notify = \
          'notify MODIF_%s' % main_table
        self._pg_notifications = map(lambda t: 'MODIF_%s' % t, table_names)
        import config
        if config.dblogtable:
            self._pdbb_logging_command = \
              "insert into %s (command) values ('%%s')" % config.dblogtable
        else:
            self._pdbb_logging_command = None

    def _pdbb_condition2sql(self, condition):
        if condition == None:
            return 'true'
        op_name, op_args, op_kwargs = \
                 condition.name(), condition.args(), condition.kwargs()
        def relop(rel, args, kwargs):
            assert len(args) == 2, ('Invalid number or arguments', args)
            colid, value = args
            assert is_string(colid), \
                   ('Invalid column name type', colid)
            assert isinstance(value, Value), \
                   ('Invalid value type', value)
            col = self._db_column_binding(colid)
            assert col, ('Invalid column name', colid)
            case_insensitive = kwargs.has_key('ignore_case') and \
                               kwargs['ignore_case'] and \
                               isinstance(value.type(), String)
            t = self.find_column(colid).type()
            btabcols = self._pdbb_btabcol(col)
            val = xtuple(self._pg_value(value))
            items = []
            for i in range(len(btabcols)):
                tabcol = btabcols[i]
                dbvalue = self._pdbb_coalesce(t, val[i])
                if case_insensitive:
                    tabcol = 'lower(%s)' % tabcol
                    dbvalue = 'lower(%s)' % dbvalue
                items.append((tabcol, dbvalue))
            def itemize(item, val):
                colid, dbval = item
                if val is 'NULL':       # fuj
                    op = 'IS'
                else:
                    op = '='
                return '%s %s %s' % (colid, op, dbval)
            if rel == '=':
                eqs = map(itemize, items, val)
                result = '(%s)' % string.join(eqs, ' AND ')
            else:
                eqs = []
                for i in range(len(items)):
                    ii = items[:i+1]
                    minieqs = ['(%s %s %s)' % (ii[-1][0], rel, ii[-1][1])]
                    for j in ii[:-1]:
                        minieqs.append('(%s = %s)' % j)
                    eqs.append('(%s)' % string.join(minieqs, ' AND '))
                result = '(%s)' % string.join(eqs, ' OR ')
            return result
        if op_name == 'EQ':
            expression = relop('=', op_args, op_kwargs)
        elif op_name == 'LT':
            expression = relop('<', op_args, op_kwargs)
        elif op_name == 'WM':
            cid, spec = op_args[0], op_args[1].value()
            for old, new in (('%', '\%'), ('_', '\_')):
                spec = string.replace(spec, old, new)
            for old, new in (('*', '%'), ('?', '_')):
                i = -1
                while True:
                    i = string.find(spec, old, i+1)
                    if i < 0:
                        break
                    j = i - 1
                    while j >= 0 and spec[j] == '\\':
                        j = j - 1
                    if (i-j) % 2 == 1:
                        spec = spec[:i] + new + spec[i+1:]
            spec = Value(String(), spec)
            c = self.find_column(cid)
            t = c.type()
            # TODO:
            # prověřit, zda po přechodu na novou podobu ValueCodebooks, toto
            # již opravdu není zapotřebí.
            #
            # if (isinstance(t, pytis.data.xtypes.Codebook) and
            #   t.strict()):
            #    def cformat(data, colid):
            #        binding = data._db_column_binding(colid)
            #        return data._pdbb_btabcol(binding)[0]
            #    cname = cformat(self, cid)
            #    cdata = pytis.data.xtypes._codebook_data(t)
            #    ccid = cdata.key()[0].id()
            #    ccname = cformat(cdata, ccid)
            #    tname = cdata._db_column_binding(ccid).table()
            #    vcname = t.value_column()
            #    cuname = cformat(cdata, vcname)
            #    spec = self._pg_value(spec)
            #    if op_kwargs.get('ignore_case'):
            #       def lower(x):
            #            return "lower(%s)" % x
            #        cuname = lower(cuname)
            #        spec = lower(spec)
            #    expression = ("%s IN (SELECT %s FROM %s WHERE %s LIKE %s)" %
            #                  (cname, ccname, tname, cuname, spec))
            #else:
            #    expression = relop('LIKE', (cid, spec), op_kwargs)
            expression = relop('LIKE', (cid, spec), op_kwargs)
        elif op_name == 'NOT':
            assert len(op_args) == 1, ('Invalid number or arguments', op_args)
            arg = op_args[0]
            assert isinstance(arg, Operator)
            expression = 'not %s' % self._pdbb_condition2sql(arg)
        elif op_name == 'AND' or op_name == 'OR':
            if not op_args:
                expression = (op_name == 'AND' and 'true' or 'false')
            else:
                assert \
                  not filter(lambda a: \
                             a and not isinstance(a, Operator),
                             op_args), \
                             ('Invalid suboperator', op_args)
                exps = map(self._pdbb_condition2sql, op_args)
                sqlop = (' %s ' % (op_name == 'AND' and 'and' or 'or'))
                expression = string.join(exps, sqlop)
        elif op_name == 'IN':
            assert len(op_args) == 4, ('Invalid number or arguments', op_args)
            col, data, table_col, cond = op_args
            table = data._key_binding[0].table()
            condition = data._pdbb_condition2sql(cond)
            expression = '%s in (select %s from %s where %s)' % \
                         (col, table_col, table, condition)
        else:
            raise ProgramError('Unknown operator', op_name)
        return '(%s)' % expression

    def _pdbb_sort2sql(self, sort):
        def item2sql(item, self=self):
            if type(item) == type(()):
                id, dirspec = item
                if dirspec == ASCENDENT:
                    dir = 'ASC'
                elif dirspec == DESCENDANT:
                    dir = 'DESC'
                else:
                    raise ProgramError('Invalid sorting direction', dirspec)
            else:
                id, dir = item, 'ASC'
            b = self._db_column_binding(id)
            return map(lambda bt: '%s %s' % (bt, dir), self._pdbb_btabcol(b))
        items = map(item2sql, sort)
        sort_string = string.join(reduce(operator.add, items, []), ',')
        if sort_string:
            sort_string = sort_string + ','
        return sort_string
        
    # Metody související s exportovanými metodami DB operací
    
    def _pdbb_table_row_lists(self, row):
        table_bindings = filter(lambda b, t=self._key_binding[0].table(): \
                                b.table() == t,
                                self._bindings)
        columns = []
        values = []
        for b in table_bindings:
            try:
                value = self._pg_value(row[b.id()])
            except KeyError:
                continue
            colid = b.id()
            colspec = self.find_column(colid)
            assert colspec, ('Column not found', colid)
            if isinstance(colspec.type(), Oid):
                continue
            column = b.column()
            if is_sequence(column):
                columns = columns + list(column)
            else:                
                columns.append(column)
            if is_sequence(value):
                values = values + list(value)
            else:
                values.append(value)
        return columns, values

    def _pg_row (self, value):
        return self._pg_query(self._pdbb_command_row % value)
    
    def _pg_search(self, row, condition, direction):
        sorting = self._pg_last_select_sorting
        if direction == FORWARD:
            pass
        elif direction == BACKWARD:
            sorting = reversed_sorting(sorting)
        else:
            raise ProgramError('Invalid direction', direction)
        def sorting_condition(sorting, forwards, row, mayeq):
            # - forwards je True:
            #   pak je row řádek, na kterém stojíme a hledáme všechny řádky
            #   v směru pohybu vyhledávání
            # - forwards je False:
            #   pak je row řádek vyhledávaný řádek a hledáme všechny řádky,
            #   které jsou v protisměru pohybu vyhledávání.
            if row is None:
                return None
            sdirection = ecase(direction,
                               (FORWARD, ASCENDENT),
                               (BACKWARD, DESCENDANT))
            sorting = tuple(sorting) + \
                      tuple(map(lambda c: (c.id(), sdirection),
                                self.key()))
            processed = []
            conditions = []
            for cid, dir in sorting:
                if cid in processed:
                    continue
                conds = [EQ(c, row[c]) for c in processed]
                if (forwards and dir == ASCENDENT) or \
                       (not forwards and dir == DESCENDANT):
                    relop = GT
                else:
                    relop = LT
                if row[cid].value() is None:
                    if relop is LT:
                        conds.append(NE(cid, row[cid]))
                else:
                    neq = relop(cid, row[cid], ignore_case=False)
                    if relop is GT:
                        nullval = pytis.data.Value(row[cid].type(), None)
                        neq = OR(neq, EQ(cid, nullval))
                    conds.append(neq)
                if conds:
                    conditions.append(apply(AND, conds))
                processed.append(cid)
            if mayeq:
                eqs = [EQ(c, row[c], ignore_case=False) for c in processed]
                conditions.append(apply(AND, eqs))
            return apply(OR, conditions)
        select_cond = self._pg_last_select_condition
        common_cond = AND(select_cond,
                          sorting_condition(sorting, True, row, False))
        sort_string = self._pdbb_sort2sql(sorting)
        # Najdi první řádek splňující požadovanou podmínku
        search_cond = AND(common_cond, condition)
        cond_string = self._pdbb_condition2sql(search_cond)
        if direction == FORWARD:
            sql_command = self._pdbb_command_search_first
        elif direction == BACKWARD:
            sql_command = self._pdbb_command_search_last
        else:
            raise ProgramError('Unknown direction', direction)
        data_ = self._pg_query(sql_command % (cond_string, sort_string))
        if not data_:
            return 0
        # Zjisti vzdálenost mezi aktuálním a vyhledaným řádkem
        row_found = self._pg_make_row_from_raw_data(data_)
        search_cond = AND(common_cond,
                          sorting_condition(sorting, False,
                                            row_found, True))
        cond_string = self._pdbb_condition2sql(search_cond)
        data_ = self._pg_query(self._pdbb_command_search_distance % \
                               cond_string)
        try:
            result = int(data_[0][0])
        except:
            raise ProgramError('Unexpected result', data_)
        return result

    def _pg_select (self, condition, sort):
        cond_string = self._pdbb_condition2sql(condition)
        sort_string = self._pdbb_sort2sql(sort)
        data = self._pg_query(self._pdbb_command_count % cond_string)
        self._pg_query(self._pdbb_command_select %
                       (cond_string, sort_string))
        try:
            result = int(data[0][0])
        except:
            raise DBSystemException('Unexpected SELECT result', data)
        self._pdbb_select_rows = result
        return result

    def _pg_select_aggregate(self, operation, condition):
        cond_string = self._pdbb_condition2sql(condition)
        aggfun, colid = operation
        FMAPPING = {self.AGG_MIN: 'min',
                    self.AGG_MAX: 'max',
                    self.AGG_COUNT: 'count',
                    self.AGG_SUM: 'sum',
                    self.AGG_AVG: 'avg',}
        try:
            function = FMAPPING[aggfun]
        except KeyError:
            raise ProgramError('Invalid aggregate function identifier',
                               operation)
        return self._pg_query(self._pdbb_command_select_agg %
                              (function, colid, cond_string))
    
    def _pg_fetchmany (self, count, direction):
        if direction == FORWARD:
            query = self._pdbb_command_fetch_forward % count
        elif direction == BACKWARD:
            query = self._pdbb_command_fetch_backward % count
        else:
            raise ProgramError('Invalid direction', direction)
        return self._pg_query(query)

    def _pg_skip(self, count, direction, exact_count=False):
        if direction == FORWARD:
            self._pg_query(self._pdbb_command_move_forward % count)
        elif direction == BACKWARD:
            answer = self._pg_query(self._pdbb_command_move_backward % count)
            if is_sequence(answer):
                # Result type has changed with PostgreSQL 7.4 interface library.
                # This hack seems to help now.
                answer_count = answer[0][0]
            else:
                answer_count = int(string.split(answer, ' ')[1])
            if exact_count and answer_count != count:
                log(OPERATIONAL, "Nečekaný výsledek kurzorové operace MOVE:",
                    (answer_count, count))
        else:
            raise ProgramError('Invalid direction', direction)
        return None
        
    def _pg_insert(self, row, after=None, before=None):
        ordering = self._ordering
        if ordering:
            ocol = self._ordering[0]
            if after:
                neighbor = after
                n = neighbor[ocol].value() + 1
            elif before:
                neighbor = before
                n = neighbor[ocol].value()
            else:
                neighbor = row
                n = -1
            try:
                params = map(lambda o, s=self, n=neighbor: s._pg_value(n[o]),
                             self._ordering[1:])
            except KeyError:
                raise ProgramError('Invalid column id in ordering',
                                   self._ordering, row)
            if n >= 0:
                params = [str(n)] + params
            params = tuple(params)
            if n >= 0:
                self._pg_query(self._pdbb_command_insert_shift % params,
                               backup=True)
            else:
                result = self._pg_query(self._pdbb_command_insert_newpos % \
                                        params)
                if result:
                    raw = result[0][0]
                    if raw is None:
                        n = 1
                    else:
                        n = int(raw)
                else:
                    n = 1
            oval = Value(Integer(), n)
            try:
                row[ocol] = oval
            except KeyError:
                row.append(ocol, oval)
        cols, vals = self._pdbb_table_row_lists(row)
        self._pg_query(self._pdbb_command_insert % \
                       (string.join(cols, ','), string.join(vals, ',')),
                       backup=True)
        # Pokud data nemají klíč (protože je generován, např. jako SERIAL),
        # nezbývá, než se řídit oid.  Tento postup pak lze použít obecně, snad
        # v PostgreSQL funguje.  Je to založeno na předpokladu, že každý nově
        # vložený záznam má nejvyšší oid v tabulce.
        data = self._pg_query(self._pdbb_command_get_last)
        return self._pg_make_row_from_raw_data(data)
    
    def _pg_update(self, condition, row):
        # TODO: Při použití RULEs v PostgreSQL UPDATE vrací vždy 0.  Toto
        # chování je sporné, nicméně v tuto chvíli PostgreSQL nenabízí žádné
        # přímé řešení, jak výsledek UPDATE zjistit.  Proto zde aplikujeme
        # jakýsi hack, který nějakým způsobem ošetří alespoň některé situace,
        # aby nebyl signalizován neúspěch UPDATE v případě jeho úspěchu.
        cols, vals = self._pdbb_table_row_lists(row)
        s = map(lambda c, v: "%s=%s" % (c, v), cols, vals)
        settings = ','.join(s)
        cond_string = self._pdbb_condition2sql(condition)
        def extract_result(d):
            try:
                return int(d[0][0])
            except:
                raise DBSystemException('Unexpected UPDATE result', None, d)
        try:
            broken = self._pdbb_broken_update_result
        except AttributeError:
            broken = self._pdbb_broken_update_result = \
                     self._pg_query(self._pdbb_command_test_broken_update)
        if broken:
            d = self._pg_query(self._pdbb_command_broken_update_preselect % \
                               cond_string)
            result = extract_result(d)
        d = self._pg_query(self._pdbb_command_update % (settings, cond_string),
                           backup=True)
        if not broken:
            result = extract_result(d)
        if result >= 0:
            return result
        else:
            raise DBSystemException('Unexpected UPDATE value', None, result)

    def _pg_delete (self, condition):
        sql_condition = self._pdbb_condition2sql(condition)
        d = self._pg_query(self._pdbb_command_delete % sql_condition,
                           backup=True)
        try:
            result = int(d[0][0])
        except:
            raise DBSystemException('Unexpected DELETE result', None, d)
        if result >= 0:
            return result
        else:
            raise DBSystemException('Unexpected DELETE value', None, result)

    def _pg_send_notifications(self):
        """Rozešli notifikace o modifikaci tohoto datového objektu."""
        self._pg_query(self._pdbb_command_notify, outside_transaction=True)


class PostgreSQLUserGroups:
    """Třída pro zjišťování skupin uživatele."""
    
    _access_groups = {}

    def _pgg_connection_key(connection_spec):
        return connection_spec.user(), connection_spec.password()
    _pgg_connection_key = staticmethod(_pgg_connection_key)
    
    def _pgg_retrieve_access_groups(data):
        if __debug__: log(DEBUG, 'Updatuji seznam skupin uživatelů')
        d = data._pg_query("select groname, grolist from pg_group",
                           outside_transaction=True)
        regexp = None
        the_user = data._pg_dbconnection_spec().user()
        groups = []
        for group, uid_string in d:
            if uid_string is not None and regexp is None:
                if uid_string != '{}':
                    uids = uid_string[1:-1].split(',')
                    for u in uids:
                        d1 = data._pg_query("select pg_get_userbyid(%s)" % u,
                                            outside_transaction=True,
                                            group_update=False)
                        user = d1[0][0]
                        if user == the_user:
                            regexp = re.compile('[^0-9]%s[^0-9]' % u)
                            groups.append(group)
                            break
            else:
                if uid_string is not None and regexp.search(uid_string):
                    groups.append(group)
        if __debug__: log(DEBUG, 'Seznam skupin uživatelů updatován')
        return groups
    _pgg_retrieve_access_groups = staticmethod(_pgg_retrieve_access_groups)

    def access_groups(self):
        """Stejné jako 'class_access_groups()', avšak bez specifikace spojení.

        Spojení je převzato automaticky ze specifikace spojení instance.

        """
        connection_spec = self._pg_dbconnection_spec()
        return PostgreSQLUserGroups.class_access_groups(connection_spec)
    
    def update_access_groups(self, connection_spec):
        """Aktualizuj informace o skupinách uživatele."""
        key = PostgreSQLUserGroups._pgg_connection_key(connection_spec)
        PostgreSQLUserGroups._access_groups[key] = self
    
    def class_access_groups(connection_spec):
        """Vrať sekvenci jmen skupin, do kterých patří přihlášený uživatel.

        Nejsou-li skupiny uživatele známy, vrať 'None'.

        Argumenty:

          connection_spec -- specifikace spojení, jehož skupiny mají být
            vráceny

        Sekvence jmen skupin je updatována při každém vytvoření nového
        spojení.  Jména skupin jsou strings.

        """
        key = PostgreSQLUserGroups._pgg_connection_key(connection_spec)
        groups = PostgreSQLUserGroups._access_groups.get(key)
        if isinstance(groups, Data):
            groups = PostgreSQLUserGroups._access_groups[key] = \
              PostgreSQLUserGroups._pgg_retrieve_access_groups(groups)
        return groups
    class_access_groups = staticmethod(class_access_groups)


class DBDataDefaultClass(PostgreSQLUserGroups, RestrictedData,
                         PostgreSQLStandardBindingHandler,
                         DBDataPyPgSQL):
    """Datová třída, kterou v naší aplikaci standardně používáme.

    Je utvořena pouhým složením existujících tříd a nezavádí žádnou další novou
    funkcionalitu kromě konstruktoru.

    """    
    def __init__(self, bindings, key, dbconnection_spec, ordering=None,
                 access_rights=AccessRights((None, (None, Permission.ALL)))):
        """Stejné jako u 'DBDataPyPgSQL', volá však konstruktory předků."""
        RestrictedData.__init__(self, access_rights)
        DBDataPyPgSQL.__init__(self, bindings, key, dbconnection_spec,
                               ordering)
        PostgreSQLStandardBindingHandler.__init__(self)
        # Registrace notifikací sem jaksi nepatří, ale kam jinam ji dát, když
        # má být provedena až po uskutečnění všech inicializací?
        import config
        if config.dblisten:
            self._pypg_add_notifications()


DBDataDefault = DBDataDefaultClass
"""Podtřída 'DBData', kterou používáme pro přístup k databázi."""



### Čítače


class DBPyPgCounter(Counter):
    """Čítač uložený v PostgreSQL, zpřístupněný přes pyPgSQL."""
    
    def __init__(self, name, dbconnection_spec):
        """Inicializuj čítač.

        Argumenty:

          name -- identifikátor čítače v databázi, string

        """
        self._name = name
        self._dbconnection_spec = dbconnection_spec
        self._query = "select nextval('%s')" % name
        
    def next(self):
        pool = _pypg_connection_pool()
        connection = pool.get(self._dbconnection_spec)
        result, connection = _pypg_query(connection, self._query)
        if connection:
            pool.put_back(self._dbconnection_spec, connection)
        return int(result.getvalue(0, 0))


DBCounterDefault = DBPyPgCounter
"""Podtřída třídy 'Counter', která je standardně používána."""



### Databázové funkce a procedury


class DBPyPgFunction(Function, DBDataDefaultClass):
    """Implementace třídy 'Function' pro PostgreSQL.

    Podporovány jsou pouze funkce vracející jedinou hodnotu.

    """
    def __init__(self, name, dbconnection_spec):
        """Inicializuj instanci.

        Argumenty:

          name -- jméno funkce jako neprázdný string
          dbconnection_spec -- instance třídy 'DBConnection' definující
            parametry připojení, nebo funkce bez argumentů vracející takovou
            instanci 'DBConnection'

        """
        assert is_string(name)
        self._name = name
        bindings = ()
        DBDataDefaultClass.__init__(self, bindings, bindings,
                                    dbconnection_spec)
        arg_query = "select pronargs from pg_proc where proname='%s'" % name
        data = self._pg_query(arg_query, outside_transaction=True)
        narg = int(data[0][0])
        arguments = string.join(('%s',)*narg, ', ')
        self._pdbb_function_call = 'select %s(%s)' % (name, arguments)
        
    def _db_bindings_to_column_spec(self, __bindings):
        type_query = ("select proretset, prorettype, proargtypes from pg_proc"+
                      " where proname = '%s'") % self._name
        self._pg_begin_transaction()
        try:
            data = self._pg_query(type_query)
            assert data, ('No such function', self._name)
            assert len(data) == 1, ('Overloaded functions not supported',
                                    self._name)
            r_set, r_type, arg_types = data[0]
            assert r_set == 'F', \
                   ('Multiset functions not supported', self._name)
            def type_instance(tnum):
                query = ("select typname, typlen from pg_type "+
                         "where oid = '%s'") % tnum
                data = self._pg_query(query)
                type_, size_string = data[0]
                t = self._pdbb_get_type(type_, size_string, False, False)
                return t
            r_type_instance = type_instance(r_type)
            columns = [ColumnSpec('', r_type_instance)]
        finally:
            self._pg_commit_transaction()
        return columns, ()

    def _pdbb_create_sql_commands(self):
        self._pg_notifications = []
    
    def call(self, row):
        log(EVENT, ('Volání funkce `%s\'' % self._name))
        arguments = tuple(map(self._pg_value, row))
        data = self._pg_query(self._pdbb_function_call % arguments,
                              outside_transaction=True)
        result = self._pg_make_row_from_raw_data(data)
        log(EVENT, ('Výsledek volání funkce `%s\':' % self._name), result)
        return [result]


DBFunctionDefault = DBPyPgFunction
"""Podtřída třídy 'Function', která je standardně používána."""



### Specifikační třídy

    
class DBConnection:
    """Specifikace parametrů pro připojení do databáze.

    Jedná se čistě o specifikaci, třída sama žádné operace týkající se napojení
    do databáze neprovádí ani neudržuje skutečný objekt spojení.  Pro popis
    parametrů spojení viz metoda '__init__'.

    Tato třída je míněna jako immutable a tudíž může být libovolně sdílena.
    Pro malé úpravy specifikace lze využít metodu 'modified()'.

    """
    def __init__(self, user=None, password=None, host=None, port=None,
                 database=None):
        """Nastav parametry připojení.

        Argumenty:
        
          user -- databázový uživatel jako string nebo 'None'
          password -- heslo uživatele jako string nebo 'None'
          host -- jméno databázového serveru jako string nebo 'None'
          port -- číslo portu na serveru jako integer nebo 'None'
          database -- jméno databáze jako string nebo 'None'

        Je-li kterýkoliv z argumentů 'None', není při připojování uvažován.

        """
        self._user = user
        self._password = password
        self._host = host
        self._port = port
        self._database = database

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

    def __cmp__(self, other):
        """Vrať 0, právě když 'self' a 'other' definují totéž spojení.

        Dvě instance této třídy reprezentují totéž spojení, právě když se
        rovnají odpovídající si parametry zadané jejich konstruktorům.

        """
        return compare_attr(self, other, ('_user', '_password', '_host',
                                          '_port', '_database'))

    def __hash__(self):
        return hash_attr(self,
                         ('_user', '_password', '_host', '_port', '_database'))

    def modified(self, **kwargs):
        """Vrať novou instanci specifikace updatované zadanými argumenty.

        Nová instance má se 'self' shodné všechny parametry až na ty, jejichž
        nové hodnoty jsou specifikovány v 'kwargs'.

        Argumenty:

          kwargs -- klíčované argumenty stejné jako v konstruktoru

        """
        args = {}
        for k, v in self.__dict__.items():
            args[k[1:]] = v
        args.update(kwargs)
        new = apply(self.__class__, (), args)
        return new


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
        assert isinstance(id, types.StringType)
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
    def __init__(self, id, table, column, related_to=None, enumerator=None,
                 type_=None, **kwargs):
        """Definuj napojení.

        Argumenty:
        
          id -- id sloupce
          table -- jméno databázové tabulky, do které má být tabulkový
            sloupec napojen, jako string
          column -- jméno sloupce databázové tabulky, na který má být
            tabulkový sloupec napojen, jako string nebo sekvence (viz níže)
          related_to -- instance 'DBColumnBinding' specifikující, se kterým
            sloupcem jiné databázové tabulky je tento sloupec v relaci; pokud
            s žádným, je hodnotou 'None'
          enumerator -- 'None' nebo instance třídy 'DataFactory' odpovídající
            navázanému číselníku.
          type_ -- explicitně specifikovaný typ sloupce jako instance třídy
            'Type' nebo 'None'.  Je-li 'None', bude typ sloupce určen
            automaticky dle informací získaných přímo z databáze.  V opačném
            případě bude typem hodnota tohoto argumentu, která musí odpovídat
            typu sloupce v databázi (být jeho specializací).
          **kwargs -- explicitně definované klíčové argumenty typu.  Pokud jsou
            definovány libovolné klíčové argumenty, budou tyto předány
            konstruktoru implicitního datového typu.  Typ v takovém případě
            nesmí být explicitně určen argumentem 'type_'.

        Napojení může být *skryté*, což znamená, že přímo neodpovídá žádnému
        sloupci datové tabulky.  To se může stát například v případě, že
        binding je definováno *pouze* kvůli specifikaci relace mezi tabulkami
        (prostřednictvím argumentu 'related_to').  U skrytého napojení nezáleží
        na hodnotě příslušného sloupce a tudíž k těmto hodnotám ani nelze
        přistupovat.  Napojení je považováno za skryté, právě když řetězec 'id'
        je prázdný.

        'related_to' je obecně nesymetrická relace přibližně odpovídající
        specifikátoru REFERENCES.

        Pokud argument 'enumerator' není 'None', bude typem tohoto sloupce
        automaticky 'pytis.data.Codebook', jehož sloupce poskytující uživatelské
        hodnoty jsou dány druhým a dalšími prvky sekvence 'enumerator'
        (nejsou-li žádné, budou použity klíčové sloupce datového objektu).
        Pokud má 'enumerator' klíč skládající se z více sloupců, není argument
        'column' string, nýbrž sekvence o stejném počtu prvků, odpovídajících
        oněm sloupců včetně pořadí.  Datové objekty zpracovávající tuto
        specifikaci nejsou povinny jakkoli zohledňovat propojení tabulky
        'enumerator' a tohoto sloupce.
          
        """
        DBBinding.__init__(self, id)
        assert isinstance(table, types.StringType), table
        assert isinstance(column, types.StringType), column
        assert isinstance(enumerator, DataFactory) or enumerator is None, \
               enumerator
        assert isinstance(type_, Type) or type_ is None, type_
        assert kwargs == {} or type_ is None, (type_, kwargs)
        self._table = table
        self._column = column
        self._related_to = related_to
        self._enumerator = enumerator
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

    def enumerator(self):
        """Vrať číselníkový datový objekt z konstruktoru nebo 'None'."""
        return self._enumerator

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
        return ('<DBCB: table=%s, column=%s, related_to=%s, enumerator=%s, '+\
                'type=%s, is_hidden=%s>') % \
                (self._table, self._column, self._related_to, self._enumerator,
                 self._type, self._is_hidden)

    def __cmp__(self, other):
        return compare_attr(self, other, ('_table', '_column', '_related_to',
                                          '_enumerator', '_type', '_is_hidden'
                                          ))
    
    def __hash__(self):
        return hash_attr(self, ('_table', '_column', '_related_to',
                                '_enumerator', '_type', '_is_hidden'))



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
        log(OPERATIONAL, 'Databázová výjimka', (message,) + args)
        if exception:
            log(OPERATIONAL, 'Obalená databázová výjimka',
                (exception,) + exception.args)
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



### Jiné třídy 


class Oid(Integer):
    """Typ sloupce oid v PostgreSQL."""
