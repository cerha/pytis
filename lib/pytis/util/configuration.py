# -*- coding: iso-8859-2 -*-

# Prost�edky pro definici a zpracov�n� konfigurace b�hu aplikace
# 
# Copyright (C) 2002-2011 Brailcom, o.p.s.
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

"""Prost�edky pro definici a zpracov�n� konfigurace b�hu aplikace.

Cel� konfigurace je definov�na instanc� t��dy 'Configuration', dokumentace
v�t�to t��d� pov� v�ce.

"""

import getopt
import imp
import os
import stat
import string
import sys
import time
import copy

from pytis.util import *
import pytis.data


class _OrderedDefinitionClass(type):
    """A metaclass allowing us to find out the order of class definitions."""
    _class_counter = 0
    def __init__(cls, name, bases, dict):
        cls._class_definition_order = _OrderedDefinitionClass._class_counter
        _OrderedDefinitionClass._class_counter += 1


class Configuration(object):
    """Definition of configuration and its particular options."""

    class Option(object):
        """Specification of a configuration option (variable).

        Definic� potomka t�to t��dy se jm�nem za��naj�c�m prefixem '_Option_'
        jako vnit�n� t��dy t��dy 'Configuration' je automaticky definov�na nov�
        konfigura�n� volba aplikace.  Jm�no volby je shodn� s���st� jm�na
        takov� t��dy n�sleduj�c� za prefixem '_Option_', jej� popis je
        v�docstringu t��dy.  Ostatn� vlastnosti volby jsou definov�ny metodami
        a/nebo konstantami dan� t��dy.  Konkr�tn� hodnota je pak udr�ov�na
        v�jej� instanci.

        Docstring t��d nepodl�h� obvykl�m form�tovac�m pravidl�m.  M�l by m�t
        podobu, je� se dob�e vyj�m� v�koment��i pythonov�ho zdrojov�ho souboru.

        Standardn� konfigura�n� volby jsou uvedeny p��mo zde.  Aplikace m��e ve
        sv�m defini�n�m souboru definovat dal��, sv� vlastn�, konfigura�n�
        volby pou�it�m potomka t��dy 'Configuration' a dopln�n�m dal��ch
        vnit�n�ch t��d v�n�m roz���it dostupn� konfigura�n� volby.

        Po zpracov�n� konfigura�n�ch voleb je zb�vaj�c�, nezpracovan�, ��st
        p��kazov� ��dky p�i�azena do 'sys.argv'.

        """
        __metaclass__ = _OrderedDefinitionClass
        
        _DESCR = None
        """Stru�n� (jedno��dkov�) popis volby."""
        
        _DOC = None
        """Podrobn�j�� dokumentace v�znamu volby a p��pustn�ch hodnot."""
        
        _DEFAULT = None
        """V�choz� hodnota konfigura�n� volby.

        V�ce viz. dokumentace metody `default()'."""

        _DEFAULT_STRING = None
        """V�choz� hodnota konfigura�n� volby pro dump.

        V�ce viz. dokumentace metody `default_string()'."""

        _CMDLINE = False
        """P��znak, zda tato volba m��e b�t zad�na tak� z p��kazov� ��dky.

        Pravdivou hodnotou t�to konstanty definujeme, �e dan� volba."""
        
        _LONG_OPTION = None
        """Specifikace dlouh� volby p��kazov� ��dky pro 'getopt'."""
        
        _ENVIRONMENT = ()
        """Specifikace jmen prom�nn�ch prost�ed� obsahuj�c�ch hodnotu volby.

        V�ce viz. dokumentace metody `environment()'."""

        _VISIBLE = True
        """Specifikace viditelnosti volby.

        V�ce viz. dokumentace metody `visible()'."""

        _TYPE = None
        """Datov� typ volby jako instance t��dy 'pytis.data.Type' nebo None.

        Hodnota 'None' ur�uje bl�e nespecifikovan� typ.  Takov� volby umo��uj�
        p�ed�vat libovoln� Pythonov� objekt.

        Nam�sto p��m�ho nastavov�n� t�to konstanty je doporu�ov�no pou��t
        p�eddefinovan�ch t��d 'StringOption', 'NumericOption' nebo
        'BooleanOption' (viz n�e).

        """
        
        def __init__(self, configuration):
            """Inicializuj instanci volby.

            Argumenty:

              configuration -- instance t��dy 'Configuration', ve kter� je
                konfigura�n� volba p��tomna
                
            """
            self._configuration = configuration
            self._value = self._undefined = object()
            self._changed = False
            assert self._DESCR is not None, \
                   "Option '%s' doesn't define the description string." % \
                   self.name()

        def _compute_init_value(self, configuration):
            value = self._undefined
            long_option = self.long_option()
            if long_option:
                if long_option[-1] == '=':
                    boolean = False
                    long_option = long_option[:-1]
                else:
                    boolean = True
                try:
                    values = configuration.command_line_options[long_option]
                    if boolean:
                        value = True
                    else:
                        value = values[0]
                except KeyError:
                    pass
            if value is self._undefined:
                for var in self.environment():
                    varval = os.getenv(var)
                    if varval is not None:
                        value = varval
                        break
            if value is self._undefined:
                value = self._value
            if value is self._undefined:
                value = self.default()
            return value

        def name(self):
            return self.__class__.__name__[8:]
        
        def init_value(self, force=False):
            """Inicializuj hodnotu prom�nn�.

            Argumenty:

              force -- pr�v� kdy� je nepravdiv�, inicializuj hodnotu jen tehdy,
                je-li je�t� nedefinov�na

            """
            if force or self._value is self._undefined:
                value = self._compute_init_value(self._configuration)
                self._value = value

        def value(self):
            """Vra� aktu�ln� hodnotu konfigura�n� volby."""
            # Hodnotu nenastavujeme hned�v�konstruktoru, proto�e v�t� dob�
            # je�t� nemus� b�t inicializov�ny jin� volby, na kter�ch tato volba
            # p��padn� z�vis�.
            if self._value is self._undefined:
                self.init_value()
            return self._value

        def set_value(self, value, initialization=False):
            """Nastav hodnotu konfigura�n� volby na 'value'."""
            if not initialization:
                self._changed = True
            self._value = value

        def changed(self):
            """Vra� pravdu, pokud hodnota volby byla zm�n�na aplikac�.

            Za zm�nu je pova�ov�no jak�koliv nastaven� volby na jinou hodnotu, ne�
            jakou dan� volba nabyla b�hem inicializace (tj. p�i na��t�n� voleb
            p��kazov� ��dky a konfigura�n�ho souboru).
            
            """ 
            return self._changed

        def reset(self):
            """Set option value to the initial default value."""
            self._value = self._undefined
            self._changed = False

        def long_option(self):
            """Vra� specifikaci dlouh� volby pro 'getopt' jako string.

            Specifikace m��e m�t nap��klad podobu 'debug' nebo 'datadir='.
            Pokud konfigura�n� volba nen� spojena s���dnou volbou p��kazov�
            ��dky, vra� 'None'.

            Specifikaci lze upravit p�edefinov�n�m konstanty `_OPTION' v
            odvozen� t��d�.
            
            """
            if self._LONG_OPTION is not None:
                return self._LONG_OPTION
            elif self._CMDLINE:
                name = self.name().replace('_','-')
                if isinstance(self.type(), pytis.data.Boolean):
                    return name
                else:
                    return name + '='
            else:
                return None

        def environment(self):
            """Vra� tuple jmen prom�nn�ch prost�ed� obsahuj�c�ch hodnotu volby.

            Jm�na prom�nn�ch jsou strings.  Prom�nn� prost�ed� jsou zkoum�ny
            v�uveden�m po�ad� a platn� je prvn� z�nich, kter� je v�prost�ed�
            p��tomna (a�to i�kdy� je t�eba jej� hodnota pr�zdn�).  Prom�nn�
            prost�ed� maj� ni��� prioritu ne� volba p��kazov� ��dky nebo
            hodnota v�konfigura�n�m souboru, av�ak vy��� prioritu ne� hodnota
            vr�cen� metodou 'default'.

            Specifikaci lze upravit p�edefinov�n�m konstanty `_ENVIRONMENT' v
            odvozen� t��d�.
            
            """
            return self._ENVIRONMENT

        def default(self):
            """Vra� v�choz� hodnotu konfigura�n� volby.
            
            Hodnota vr�cen� touto metodou je pou�ita, pokud nebylo mo�no
            v�choz� hodnotu volby zjistit jinak.

            Specifikaci lze upravit p�edefinov�n�m konstanty `_DEFAULT' v
            odvozen� t��d�, nebo ve slo�it�j��ch p��padech p�edefinov�n�m t�to
            metody.
            
            """
            return self._DEFAULT
        
        def default_string(self):
            """Vra� v�choz� hodnotu konfigura�n� volby pro dump.

            Hodnota je vr�cena jako �et�zec, kter� bude vlo�en do vzorov�ho
            konfigura�n�ho souboru.  Tuto metodu je u�ite�n� p�edefinovat
            v�p��pad�, �e v�choz� hodnota volby vr�cen� metodou 'default()' je
            z�visl� na konkr�tn�m prost�ed� a/nebo nevystihuje zp�sob sv�ho
            z�sk�n�.

            Specifikaci lze upravit p�edefinov�n�m konstanty `_DEFAULT_STRING'
            v odvozen� t��d�, nebo ve slo�it�j��ch p��padech p�edefinov�n�m
            t�to metody.


            """

            if self._DEFAULT_STRING is not None:
                return self._DEFAULT_STRING
            else:
                import pprint
                return pprint.PrettyPrinter().pformat(self.default())

        def type(self):
            return self._TYPE
        
        def visible(self):
            """Vra� p��znak viditelnosti volby.

            Vr�cen� hodnota ur�uje, zda m� b�t volba p��tomna ve vzorov�m
            konfigura�n�m souboru.

            Specifikaci lze upravit p�edefinov�n�m konstanty `_VISIBLE'
            v odvozen� t��d�.

            """
            return self._VISIBLE

        def description(self):
            """Vra� stru�n� jedno��dkov� popis volby 'name' jako �et�zec."""
            return self._DESCR
        
        def documentation(self):
            """Vra� podrobn� popis volby 'name' jako �et�zec nebo None.
        
            �et�zec tak m��e b�t v�ce��dkov� a d�lka jednoho ��dku m��e p�esahovat 80 znak�.  Pokud
            podrobn� popis nen� definov�n, m��e vr�tit t� None.
            
            """
            return self._DOC

    class StringOption(Option):
        """T��da pro volby �et�zcov�ho typu."""
        _TYPE = pytis.data.String()        

    class BooleanOption(Option):
        """T��da pro volby typu boolean."""
        _TYPE = pytis.data.Boolean()        

    class ColorOption(Option):
        """T��da pro volby typu barva."""
        _TYPE = pytis.data.Color()
        _DOC = "Barva je reprezentov�na �et�zcem '#RRGGBB'."

    class NumericOption(Option):
        """T��da pro volby celo��seln�ho typu."""
        _TYPE = pytis.data.Integer()        

    class FileOption(StringOption):
        def _compute_init_value(self, *args, **kwargs):
            value = super(Configuration.FileOption, self).\
                    _compute_init_value(*args, **kwargs)
            if not os.path.isabs(value):
                value = os.path.join(os.getcwd(), value)
            return value

    class HiddenOption(Option):
        """Mix-in t��da pro skryt� volby."""
        _VISIBLE = False
        
    class CommandlineOption(Option):
        """Mix-in t��da pro volby p��kazov� ��dky."""
        _CMDLINE = True
        
    # Volba pro konfiguraci samu

    class _Option_config_file(StringOption, HiddenOption):
        _DESCR = _("Um�st�n� konfigura�n�ho souboru.")
        _LONG_OPTION = 'config='
        _ENVIRONMENT = ('PYTISCONFIG',)
        def default(self):
            for filename in ('./config.py', '/etc/pytis/config.py'):
                if os.access(filename, os.F_OK):
                    result = filename
                    break
            else:
                result = None
            return result

    class _Option_user_config_file(StringOption, HiddenOption):
        _DESCR = _("Um�st�n� dopl�uj�c�ho konfigura�n�ho souboru u�ivatele.")
        _DOC = _("Tento soubor, pokud, existuje, je na��t�n nav�c ke "
                 "standardn� konfiguraci a v�n�m definovan� volby maj� vy��� "
                 "prioritu ne� volby ve standardn�m konfigura�n�m souboru. "
                 "U�ite�n� p�ev�n� pro lad�n�.")
        def default(self):
            config_file = self._configuration.config_file
            if config_file:
                dir, file = os.path.split(config_file)
                user_config_file = os.path.join(dir, '_'+file)
                if os.path.exists(user_config_file):
                    result = user_config_file
                else:
                    result = None
            else:
                result = None 
            return result
        
    # Volby u�ite�n� hlavn� pro lad�n�

    class _Option_help(BooleanOption, CommandlineOption, HiddenOption):
        _DESCR = _("Volba odpov�daj�c� --help na p��kazov� ��dce.")
        _DEFAULT = False
        
    class _Option_debug(BooleanOption, CommandlineOption):
        _DESCR = _("P��znak lad�c�ho re�imu.")
        _DOC = _("Je-li zapnut, aplikace m��e b�et s�v�ce kontrolami "
                 "a vypisovat spoustu informac�, obvykle v�ak za cenu sv�ho "
                 "v�razn�ho zpomalen�.")
        _DEFAULT = False
        
    class _Option_debug_on_error(BooleanOption, CommandlineOption):
        _DESCR = _("P��znak vyvol�n� debuggeru p�i chyb�.")
        _DOC = _("Dojde-li k�odchycen� neo�ek�van� v�jimky a tato volba je "
                 "zapnuta, je vyvol�n interaktivn� debugger.  Je-li zapnuta "
                 "volba 'debug', je implicitn� zapnuta i�tato volba.  U�ite�n� "
                 "pouze pro lad�n�.")
        
        def default(self):
            return self._configuration.debug

    class _Option_debug_memory(BooleanOption, CommandlineOption):
        _DESCR = _("P��znak v�pisu lad�c�ch informac� o�pam�ti.")
        _DOC = _("Je-li zapnuta, aplikace vypisuje informativn� hl�ky "
                 "garbage collectoru a jin� �daje o�pam�ti.")
        _DEFAULT = False

    class _Option_bug_report_address(StringOption):
        _DESCR = _("E-mailov� adresa, na kterou maj� b�t pos�l�na ozn�men� "
                   "o�chyb�.")
        _DEFAULT = ''

    class _Option_bug_report_subject(StringOption):
        _DESCR = _("Subject mailu ozn�men� o�chyb� aplikace.")
        _DEFAULT = 'Bug report'

    class _Option_profile(BooleanOption, CommandlineOption):
        _DESCR = _("P��znak profilov�n�.")
        _DOC = _("Je-li zapnut, aplikace se spust� v�profilovac�m re�imu "
                 "a ukl�d� informace o�trv�n� jednotliv�ch vol�n� do souboru. "
                 "Zapnut� t�to volby velmi v�razn� zpomaluje b�h aplikace.")
        _DEFAULT = False
        
    class _Option_test_run_interactive(BooleanOption, HiddenOption):
        _DESCR = _("P��znak ur�uj�c�, zda maj� b�t spou�t�ny i�interaktivn� testy.")
        _DOC = _("T�k� se pouze regresivn�ho testov�n�.")

    class _Option_custom_debug(HiddenOption):
        _DESCR = _("Zvl�tn� lad�c� funkce, napojen� na p��kaz 'COMMAND_CUSTOM_DEBUG'.")
        _DEFAULT = (lambda: None)

    # Cesty a adres��e

    class _Option_def_dir(FileOption, CommandlineOption):
        _DESCR = _("Adres�� obsahuj�c� defini�n� soubory.")
        _DOC = _("Adres�� m��e b�t zad�n absolutn� i�relativn� vzhledem "
                 "k�aktu�ln�mu adres��i.")
        _DEFAULT = './defs'
        _ENVIRONMENT = ('PYTISDEFDIR',)

    class _Option_help_dir(FileOption, CommandlineOption):
        _DESCR = _("Adres�� obsahuj�c� soubory s n�pov�dou.")
        _DOC = _("M��e b�t zad�n absolutn� i�relativn� vzhledem k�aktu�ln�mu "
                 "adres��i.")
        _ENVIRONMENT = ('PYTISHELPDIR',)
        _DEFAULT = './help'

    # TODO: Do�asn� vr�ceno, dokud ve v�ech projektech nep�ejdeme na nov�
    # syst�m n�pov�dy.
    class _Option_doc_dir(FileOption, CommandlineOption):
        _DESCR = _("Adres�� obsahuj�c� dokumentaci.")
        _DEFAULT = './docs'

    class _Option_icon_dir(FileOption):
        _DESCR = _("Adres�� s�obr�zkov�mi soubory.")
        _DOC = _("M��e b�t zad�n absolutn� i�relativn� vzhledem k�aktu�ln�mu "
                 "adres��i.")
        _DEFAULT = './icons'

    class _Option_tmp_dir(StringOption):
        _DESCR = _("Adres�� pro do�asn� pomocn� soubory.")
        _DEFAULT_STRING = "'/tmp'"
        def default(self):
            dirs = ['/tmp', '/var/tmp', '/usr/tmp']
            tmpdir = os.getenv('TMPDIR')
            if tmpdir is not None:
                dirs = [tmpdir] + dirs
            for d in dirs:
                if os.access(d, os.W_OK):
                    result = d
                    break
            else:
                result = '.'
            return result

    class _Option_logo(FileOption, CommandlineOption):
        _DESCR = _("Cesta k souboru s logem.")
        _DOC = _("M��e b�t zad�n absolutn� i�relativn� vzhledem k�aktu�ln�mu "
                 "adres��i.")
        _DEFAULT = './icons/logo.bmp'

    class _Option_server(StringOption, CommandlineOption):
        _DESCR = _("Jm�no stroje (�et�zec), na kter�m b�� Pyro server.")
        _DOC = _("M��e b�t t� 'None', pak se klient nep�ipojuje na server "
                 "a pou��v� lok�ln� konfiguraci.")
        _DEFAULT = None

    # Datab�ze
    
    class _Option_dbuser(StringOption, CommandlineOption):
        _DESCR = _("Database user login name.")
        _DEFAULT_STRING = 'getpass.getuser()'
        def default(self):
            import getpass
            return getpass.getuser()

    class _Option_dbpass(StringOption, CommandlineOption):
        _DESCR = _("Database login password.")
        _DEFAULT = None
        
    class _Option_dbname(StringOption, CommandlineOption):
        _DESCR = _("Database name.")
        _DEFAULT = None

    class _Option_dbhost(StringOption, CommandlineOption):
        _DESCR = _("Database host name.")
        _DEFAULT = 'localhost'
    
    class _Option_dbport(NumericOption, CommandlineOption):
        _DESCR = _("Database port number.")
        _DEFAULT = None
    
    class _Option_dbsslm(StringOption, CommandlineOption):
        _DESCR = _("Database SSL mode (one of string constants supported by the DB system).")
        _DEFAULT = None

    class _Option_dbschemas(StringOption, CommandlineOption):
        _DESCR = _("List of database schemas to use in the order of their preference.")
        _DEFAULT = None        
    
    class _Option_dbconnections(HiddenOption):
        _DESCR = _("Alternative database connections")
        _DOC = ("The default database connection is normally defined by 'dbconnection'.  Certain "
                "applications, however, may require multiple database connections, which are "
                "configured using this option.  The value is a dictionary assigning a connection "
                "specification to each connection by name.  Connection names are defined by "
                "applications (each application should mention the names of used conections in "
                "its documentation).  The connection specification (the value assigned to a "
                "connection name) is a dictionary with keys 'dbname', 'dbhost', 'dbport', "
                "'dbuser', 'dbpass' and 'dbsslm'.  Only 'dbname' is mandatory.  Their meaning "
                "and default values are the same as for the configuration options of the same "
                "names specifying the properties of the default connection.")
        _DEFAULT = {}
        
    class _Option_dbconnection(HiddenOption):
        _DESCR = _("Database connection specification instance ('pytis.data.DBConnection').")
        _DOC = _("The instance is constructed from the above db* option by default.")
        def default(self):
            map = {'dbname': 'database',
                   'dbhost': 'host',
                   'dbport': 'port',
                   'dbuser': 'user',
                   'dbpass': 'password',
                   'dbsslm': 'sslmode'}
            def connection_options(items):
                # Transform configuration option names to DBConnection option names.
                return dict([(map[key], value) for key, value in items
                             if value is not None])
            cfg = self._configuration
            options = connection_options([(option, getattr(cfg, option))
                                          for option in map.keys() if hasattr(cfg, option)])
            alternatives = [(name, connection_options(opts.items()))
                            for name, opts in cfg.dbconnections.items()]
            schemas_string = cfg.dbschemas
            if schemas_string:
                schemas = [s.strip() for s in schemas_string.split(',')]
            else:
                schemas = None
            return pytis.data.DBConnection(alternatives=dict(alternatives),
                                           schemas=schemas,
                                           **options)

    class _Option_dblogtable(StringOption):
        _DESCR = _("Jm�no tabulky, do kter� maj� b�t logov�ny DML SQL p��kazy.")
        _DEFAULT = ''

    class _Option_dblisten(BooleanOption):
        _DESCR = _("Flag ur�uj�c�, zda m� b�t spou�t�n dohl�e� zm�n dat.")
        _DEFAULT = True

    class _Option_max_pool_connections(NumericOption):
        _DESCR = _("Maximum number of database connections stored in a pool "
                   "for a single connection specification. "
                   "If None then there is no limit.")
        _DEFAULT = None

    class _Option_connection_limit(NumericOption):
        _DESCR = _("Maximum number of concurrently open database connections "
                   "for a single connection specification in a database pool. "
                   "If None then there is no limit.")
        _DEFAULT = None

    # Logovac� volby

    class _Option_log_logger(Option):
        _DESCR = _("Specifikace logovac� t��dy.")
        _DOC = _("Trojice (CLASS, ARGS, KWARGS), kde CLASS je logovac� t��da a "
                 "ARGS, resp. KWARGS, jsou argumenty, resp. kl��ovan� "
                 "argumenty, jej�ho konstruktoru.  Standardn� dostupn� t��dy "
                 "jsou SyslogLogger a StreamLogger.  V�ce o�nich lze nal�zt "
                 "v�jejich dokumentaci.")
        
        _DEFAULT_STRING = '(log.StreamLogger, (sys.stderr,), {})'
        def default(self):
            import log
            return (log.StreamLogger, (sys.stderr,), {})

    class _Option_log_exclude(Option):
        _DESCR = _("Seznam typ� logovac�ch hl�ek, kter� maj� b�t "
                   "odfiltrov�ny.")
        _DOC = _("V�seznamu lze pou��t konstanty 'OPERATIONAL', 'ACTION', "
                 "'EVENT' a 'DEBUG'.")
        _DEFAULT_STRING = '[DEBUG]'
        def default(self):
            if self._configuration.debug:
                return []
            else:
                import log
                return [log.DEBUG]

    class _Option_log_one_line_preferred(BooleanOption):
        _DESCR = _("Ur�uje, zda je preferov�no stru�n� nebo jednotn� "
                   "form�tov�n�.")
        _DOC = _("Je-li tato volba nastavena na pravdu, jsou kr�tk� data "
                 "v�logovac�ch hl�k�ch doporu�uj�c�ch stru�nost p�ipojena "
                 "ihned za hl�ku m�sto vyps�n� na samostatn� ��dek.")
        _DEFAULT = True

    class _Option_log_module_filter(StringOption):
        _DESCR = _("Prefix jm�na modulu, jeho� debugovac� hl�ky jsou "
                   "propu�t�ny.")
        _DOC = _("Debugovac� logovac� hl�ky modul� s�jin�m prefixem jsou "
                 "odfiltrov�ny.  Nen�-li definov�no, jsou propu�t�ny v�echny "
                 "hl�ky (nestanov�-li jin� filtr jinak).  U�ite�n� pouze pro "
                 "lad�n�.")
        _DEFAULT = ''
        _DEFAULT_STRING = "'pytis.data'"

    class _Option_log_class_filter(Option):
        _DESCR = _("Sekvence jmen t��d, jejich� debugovac� hl�ky jsou "
                   "propu�t�ny.")
        _DOC = _("Debugovac� logovac� hl�ky ostatn�ch t��d jsou odfiltrov�ny. "
                 "Je-li 'None', jsou propu�t�ny v�echny hl�ky (nestanov�-li "
                 "jin� filtr jinak).  U�ite�n� pouze pro lad�n�.")
        _DEFAULT = None
        _DEFAULT_STRING = "('pytis.data.DBDefaultClass',)"
            
    # Extern� programy

    class _Option_printing_command(StringOption):
        _DESCR = _("Shellov� p��kaz pro proveden� tisku, v�etn� argument�.")
        _DOC = _("P��kaz mus� b�t schopen p�evz�t tiskov� data ze "
                 "standardn�ho vstupu.")
        _DEFAULT = 'lpr'

    class _Option_lout_command(StringOption):
        _DESCR = _("Jm�no programu Lout, s cestou nebo bez n�.")
        _DOC = _("Bude pou�ito p�i konstrukci p��kazov� ��dky Lout. "
                 "Nejedn�-li se o origin�ln� Lout, mus� b�t argumenty "
                 "programu s Loutem kompatibiln�.")
        _DEFAULT = 'lout'

    class _Option_postscript_viewer(StringOption):
        _DESCR = _("Shell command to be used for displaying print preview PostScript files. "
                   "It must take the name of the file to be displayed as its first argument. "
                   "If this option value is empty, Pytis internal viewer is used.")
        _DEFAULT = ''
        #_DEFAULT = 'gv'

    class _Option_sendmail_command(StringOption):
        # Pou��v�no ji� jen v pytis-extensions...
        _DESCR = _("Shellov� p��kaz sendmail v�etn� cel� cesty (DEPRECATED).")
        _DEFAULT = '/usr/lib/sendmail'
        
    class _Option_smtp_server(StringOption):
        _DESCR = _("Jm�no serveru odchoz� po�ty.")
        _DEFAULT = 'localhost'
        
    class _Option_image_viewer(StringOption):
        _DESCR = _("Shellov� p��kaz pro spu�t�n� prohl�e�e obr�zk�.  Pokud "
                   "p��kaz obsahuje �et�zec %f, bude tento nahrazen n�zvem "
                   "otev�ran�ho souboru, jinak je soubor p�ipojen na konec "
                   "p��kazu.")
        _DEFAULT = 'run-mailcap'
        
    # Ostatn� konfigura�n� volby

    class _Option_application_name(StringOption):
        _DESCR = _("Jm�no aplikace.")
        _DOC = _("Jm�no m��e b�t libovoln�, pou��v� se nap�. jako titulek "
                 "okna nebo p�i logov�n�.  Od n�ho je tak� odvozeno jm�no "
                 "v�choz�ho souboru pro ukl�d�n� u�ivatelsk�ch zm�n v "
                 "konfiguraci (po vypu�t�n� speci�ln�ch znak� a diakritiky)")
        _DEFAULT = 'Pytis'

    class _Option_date_time_format(StringOption):
        _DESCR = _("Form�t spole�n� uveden�ho data a �asu.")
        _DOC = _("�et�zec ve tvaru vy�adovan�m parametrem `format' "
                 "konstruktoru t��dy 'pytis.data.DateTime'.")
        _DEFAULT = pytis.data.DateTime.DEFAULT_FORMAT

    class _Option_date_format(StringOption):
        _DESCR = _("Form�t data.")
        _DOC = _("�et�zec ve tvaru vy�adovan�m parametrem `format' "
                 "konstruktoru t��dy 'pytis.data.Date'.")
        _DEFAULT = pytis.data.Date.DEFAULT_FORMAT

    class _Option_time_format(StringOption):
        _DESCR = _("Form�t �asu.")
        _DOC = _("�et�zec ve tvaru vy�adovan�m parametrem `format' "
                 "konstruktoru t��dy 'pytis.data.Time'.")
        _DEFAULT = pytis.data.Time.DEFAULT_FORMAT

    class _Option_lc_numeric(StringOption):
        _DESCR = _("Numeric locale.")
        _DOC = _("Hodnota mus� b�t string reprezentuj�c� locale pro "
                 "form�tov�n� ��seln�ch polo�ek.")
        _DEFAULT = 'C'

    class _Option_export_directory(StringOption):
        _DESCR = _("Adres�� pro export do CSV soubor�.")
        _DOC = _("Hodnota ud�v� cestu k adres��i, kde se budou ukl�dat textov� "
                 "CSV soubory.")
        _DEFAULT = '/tmp'

    class _Option_export_encoding(StringOption):
        _DESCR = _("K�dov�n� exportovan�ch dat.")
        _DOC = _("Hodnota mus� b�t jedn�m z podporovan�ch k�dov�n� v Pythonu.")
        _DEFAULT = 'iso8859-2'

    class _Option_cache_size(NumericOption):
        _DESCR = _("Velikost cache pro ��dky datov�ho objektu.")
        _DOC = _("Velikost je cel� ��slo, kter� ud�v� po�et ��dk� cache.")
        _DEFAULT = 20000

    class _Option_initial_fetch_size(NumericOption):
        _DESCR = _("Po�et ��dk�, kter� se p�edna�tou do cache p�i prvn�m "
                   "selectu z datov�ho objektu.")
        _DEFAULT = 100

    class _Option_fetch_size(NumericOption):
        _DESCR = _("Po�et ��dk�, kter� se p�ina��taj� do cache p�i dal��ch "
                   "selectech z datov�ho objektu.")
        _DEFAULT = 100

    class _Option_sender_address(StringOption):
        _DESCR = _("E-mailov� adresa odes�latele pou�it� nap�. jako odchoz� adresa bug-report� "
                   " apod.")
        _DEFAULT = None

    class _Option_clipboard_primary_selection(BooleanOption):
        _DESCR = _("Flag ur�uj�c�, zda se m� p�i exportu pou��t primary selection.")
        _DEFAULT = False

    class _Option_use_wx_clipboard(BooleanOption):
        _DESCR = _("Flag ur�uj�c�, zda se m� pro kop�rov�n� obsahu bu�ky pou��t wxClipboard")
        _DEFAULT = True

    class _Option_form_statistics(BooleanOption):
        _DESCR = _("Flag ur�uj�c�, zda maj� b�t do datab�ze ukl�d�ny statistick� informace "
                   "o otev�ran�ch formul���ch.")
        _DEFAULT = False
        
    # Volby p�izp�soben� u�ivatelsk�ho rozhran�
        
    class _Option_show_tooltips(BooleanOption):
        _DESCR = _("Zobrazovat bublinovou n�pov�du.")
        _DEFAULT = True
        
    class _Option_stretch_tables(BooleanOption):
        _DESCR = _("Roztahovat sloupce tabulek, aby vyu�ily celou ���ku okna.")
        _DEFAULT = True
        
    class _Option_show_splash(BooleanOption):
        _DESCR = _("Zobrazovat �vodn� uv�tac� dialog.")
        _DEFAULT = True
        
    class _Option_auto_menu_accel(BooleanOption):
        _DESCR = _("Automaticky doplnit polo�ky menu prefixy akceler�torov�ch kl�ves (zm�na "
                   "vy�aduje restart aplikace).")
        _DEFAULT = True
        
    class _Option_cache_spec_onstart(BooleanOption):
        _DESCR = _("P��znak cachov�n� specifikac� p�i startu aplikace.")
        _DEFAULT = True

    class _Option_startup_forms(StringOption, CommandlineOption):
        _DESCR = _("Seznam formul���, kter� maj� b�t otev�eny po spu�t�n� "
                   "aplikace.")
        _DEFAULT = None

    class _Option_row_focus_fg_color(ColorOption):
        _DESCR = _("Barva textu aktivn�ho ��dku tabulkov�ho formul��e.")
        _DEFAULT = '#ffffff'
        
    class _Option_row_focus_bg_color(ColorOption):
        _DESCR = _("Barva pozad� aktivn�ho ��dku tabulkov�ho formul��e.")
        _DOC   = _("Pokud barva nen� nastavena, bude pou�ita syst�mov� barva "
                   "zv�razn�n�.  Barva je reprezentov�na �et�zcem '#RRGGBB'.")
        _DEFAULT = None
        
    class _Option_row_nofocus_fg_color(ColorOption):
        _DESCR = _("Barva textu neaktivn�ho ��dku tabulkov�ho formul��e.")
        _DEFAULT = '#000000'
        
    class _Option_row_nofocus_bg_color(ColorOption):
        _DESCR = _("Barva pozad� neaktivn�ho ��dku tabulkov�ho formul��e.")
        _DEFAULT = '#b6b6b6'
        
    class _Option_row_edit_fg_color(ColorOption):
        _DESCR = _("Barva textu editovan�ho ��dku tabulkov�ho formul��e.")
        _DEFAULT = '#ffffff'

    class _Option_row_edit_bg_color(ColorOption):
        _DESCR = _("Barva pozad� editovan�ho ��dku.")
        _DEFAULT = '#c80000'

    class _Option_cell_highlight_color(ColorOption):
        _DESCR = _("Barva zv�razn�n� aktivn� bu�ky tabulkov�ho formul��e.")
        _DEFAULT = '#ffa000'

    class _Option_grid_line_color(ColorOption):
        _DESCR = _("Barva m��ky tabulkov�ho formul��e.")
        _DEFAULT = '#6482be'

    class _Option_grouping_background_downgrade(ColorOption):
        _DESCR = _("Ztmaven� barvy skupiny p�i seskupov�n� ��dk�.")
        _DOC = _("Proto�e barva pozad� ��dk� nen� v�dy b�l�, je tato hodnota "
                 "ch�p�na jako relativn�.  O kolik je zvolen� barva tmav�� "
                 "ne� b�l�, o tolik bude v�sledn� barva skupiny tmav��, ne� "
                 "barva pozad� ostatn�ch ��dk�.  Barva je reprezentov�na "
                 "�et�zcem '#RRGGBB'.")
        _DEFAULT = '#eceef0'

    class _Option_field_disabled_color(ColorOption):
        _DESCR = _("Barva pozad� needitovateln�ho vstupn�ho pol��ka.")
        _DEFAULT = '#d8d8d8'
        
    class _Option_field_denied_color(ColorOption):
        _DESCR = _("Pozad� pol��ka needitovateln�ho kv�li p��stupov�m pr�v�m.")
        _DEFAULT = '#e0e4f0'

    class _Option_field_hidden_color(ColorOption):
        _DESCR = _("Pozad� pol��ka se skrytou hodnotou.")
        _DEFAULT = '#404040'

    class _Option_field_invalid_color(ColorOption):
        _DESCR = _("Barva pozad� vstupn�ho pol��ka pokud aktu�ln� hodnota nen� validn�.")
        _DEFAULT = '#ffffc0'
        
    class _Option_filter_color(ColorOption):
        _DESCR = _("Barva z�hlav� tabulky p�i zapnut�m filtrov�n�.")
        _DEFAULT = '#82c882'

    # Metody

    def __init__(self):
        self._init_options()

    def add_command_line_options(self, command_line):
        """Nastav volby dle p��kazov� ��dky.

        Argumenty:

          command_line -- volby p��kazov� ��dky jako sekvence strings; typicky
            'sys.argv'

        """
        self._init_options(command_line=command_line)

    def _init_options(self, command_line=None):
        PREFIX = '_Option_'
        options = {}
        for k, v in self.__class__.__dict__.items():
            if starts_with(k, PREFIX):
                name = k[len(PREFIX):]
                options[name] = v(self)
        self.__dict__['_options'] = options
        if command_line is None:
            command_line_options = {}
        else:
            command_line_options = self._parse_command_line_options(command_line)
        self.__dict__['command_line_options'] = command_line_options
        for o in ('config_file', 'user_config_file'):
            if o in options:
                opt = options[o]
                opt.init_value()
                v = opt.value()
            else:
                v = None
            self.__dict__['_' + o] = v
        self._read_configuration()
        for o in options.values():
            o.init_value(force=True)

    def _parse_command_line_options(self, command_line):
        command_line_options = {}
        long_options = [o.long_option() for o in self._options.values()
                        if o.long_option() is not None]
        opts, args = getopt.getopt(command_line[1:], '', long_options)
        sys.argv[1:] = args
        for o, a in opts:
            try:
                arglist = command_line_options[o[2:]]
            except KeyError:
                arglist = []
                command_line_options[o[2:]] = arglist
            arglist.append(a)
        return command_line_options

    def _read_configuration(self):
        conffile = self._config_file
        if conffile is not None:
            self.__dict__['_config_mtime'] = self._read_configuration_file(conffile)
        uconffile = self._user_config_file
        if uconffile is not None:
            self.__dict__['_user_config_mtime'] = self._read_configuration_file(uconffile)

    def _read_configuration_file(self, filename=True):
        try:
            filetime = os.stat(filename)[stat.ST_MTIME]
        except:
            raise Exception(_("Unable to stat configuration file:"), filename)
        try:
            f = open(filename)
        except:
            raise Exception(_("Unable to open configuration file:"), filename)
        try:
            del sys.modules['_config']
        except:
            pass
        try:
            confmodule = imp.load_source('_config', filename, f)
        finally:
            f.close()
        options = self._options
        cloptions = self.command_line_options
        for o in dir(confmodule):
            if o in options:
                opt = options[o]
                if opt.long_option() not in cloptions:
                    value = confmodule.__dict__[o]
                    opt.set_value(value, initialization=False)
        for o in self._options.values():
            if not o.changed():
                o.reset()
        return filetime

    def read_configuration_file(self, filename):
        """Read options from given pytis configuration file.

        Previously set configuration values may get reset during that process.

        Arguments:

          filename -- name of the pytis configuration file, basestring
          
        """
        assert isinstance(filename, basestring), filename
        return self._read_configuration_file(filename)

    def __getattr__(self, name):
        """Return the current value of configuration option 'name'.

        'name' must be a string name of an existing configuration option.  'AttributeError' is
        raised if no such option exists.

        """
        if __debug__ and name not in ('config_file', 'user_config_file'):
            now = time.time()
            reread = False
            if self._config_file and now > self._config_mtime:
                t = os.stat(self._config_file)[stat.ST_MTIME]
                if t > self._config_mtime:
                    reread = True
            if not reread and self._user_config_file and now > self._user_config_mtime:
                ut = os.stat(self._user_config_file)[stat.ST_MTIME]
                if ut > self._user_config_mtime:
                    reread = True
            if reread:
                self._read_configuration()
        try:
            return self._options[name].value()
        except KeyError:
            raise AttributeError(name)

    def __setattr__(self, name, value):
        """Set the value of configuration option 'name'.

        'name' must be a string name of an existing configuration option.  'AttributeError' is
        raised if no such option exists.
        
        """
        if name == 'user_config_file':
            self.__dict__['_user_config_file'] = value
            self.__dict__['_user_config_mtime'] = 0
        if name in self.__dict__['_options']:
            self.__dict__['_options'][name].set_value(value)
        elif hasattr(self, name):
            self.__dict__[name] = value
        else:
            raise AttributeError(name)

    def merge(self, dict, override_cmdline=False):
        """Nastav aktu�ln� konfiguraci z hodnot dan�ho slovn�ku.

        Argumenty:
          dict -- slovn�k, ze kter�ho maj� b�t p�evzaty nov� hodnoty.
            P�evezmou se pouze hodnoty kl���, jejich� n�zvy odpov�daj�
            definovan�m konfigura�n�m volb�m a to pouze v p��pad�, �e jsou
            definov�ny (obsahuj� jinou hodnotu ne� None).  Ostatn� budou
            ignorov�ny.
          override_cmdline -- implicitn� nejsou p�enastavov�ny hodnoty p�evzat�
            z p��kazov� ��dky.  Pravdiv� hodnota tohoto argumentu zp�sob�, �e
            budou p�enastaveny v�echny nalezen� konfigura�n� volby v�etn� t�ch
            z p��kazov�ho ��dku.
            
        """
        options = self._options
        clopt = self.command_line_options
        for o in dict.keys():
            if o in options and dict[o] != None:
                opt = options[o]
                if override_cmdline or opt.long_option() not in clopt:
                    opt.set_value(dict[o])

    def dump_config_template(self, stream):
        """Write configuration file template to 'stream'.

        'stream' must be a stream opened for writing.

        """
        #stream.write('# -*- coding: iso-8859-2 -*-\n\n')
        from textwrap import wrap
        import pprint
        pp = pprint.PrettyPrinter()
        for option in self.options(sort=True):
            if option.visible():
                stream.write('# %s\n' % option.description())
                doc = option.documentation()
                if doc:
                    for line in wrap(doc, 77):
                        stream.write('# %s\n' % string.strip(line))
                value = option.default_string()
                indent = ' ' * (len(option.name()) + 3)
                stream.write('#%s = %s\n\n' % (option.name(), value.replace("\n", "\n#"+indent)))

    def options(self, sort=False):
        """Return a tuple of all configuration options as 'Configuration.Option' instances.

        If 'sort' is true, the options will be returned in the order of their definition.

        """
        options = self._options.values()
        if sort:
            def _cmp(o1, o2):
                return cmp(o1._class_definition_order, o2._class_definition_order)
            options.sort(_cmp)
        return tuple(options)

    def option(self, name):
        """Return the 'Configuration.Option' instance for the option of given 'name'.

        'name' must be a string name of an existing configuration option.  'AttributeError' is
        raised if no such option exists.

        """
        try:
            return self._options[name]
        except KeyError:
            raise AttributeError(name)
