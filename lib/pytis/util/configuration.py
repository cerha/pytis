# -*- coding: utf-8 -*-

# Copyright (C) 2018-2020 Tomáš Cerha <t.cerha@gmail.com>
# Copyright (C) 2002-2016 OUI Technology Ltd.
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

"""Prostředky pro definici a zpracování konfigurace běhu aplikace.

Celá konfigurace je definována instancí třídy 'Configuration', dokumentace
v této třídě poví více.

"""
from __future__ import print_function

from past.builtins import basestring
from future.utils import with_metaclass

import getopt
import imp
import os
import stat
import sys
import time

import pytis
from pytis.util import Resolver, translations
# The translation domain is pytis-wx because it is only needed with wx applications.
_ = translations('pytis-wx')


class _OrderedDefinitionClass(type):
    """A metaclass allowing us to find out the order of class definitions."""
    _class_counter = 0

    def __init__(cls, name, bases, dict):
        cls._class_definition_order = _OrderedDefinitionClass._class_counter
        _OrderedDefinitionClass._class_counter += 1


class Configuration(object):
    """Definition of configuration and its particular options."""

    class Option(with_metaclass(_OrderedDefinitionClass, object)):
        """Specification of a configuration option (variable).

        Definicí potomka této třídy se jménem začínajícím prefixem '_Option_'
        jako vnitřní třídy třídy 'Configuration' je automaticky definována nová
        konfigurační volba aplikace.  Jméno volby je shodné s částí jména
        takové třídy následující za prefixem '_Option_', její popis je
        v docstringu třídy.  Ostatní vlastnosti volby jsou definovány metodami
        a/nebo konstantami dané třídy.  Konkrétní hodnota je pak udržována
        v její instanci.

        Docstring tříd nepodléhá obvyklým formátovacím pravidlům.  Měl by mít
        podobu, jež se dobře vyjímá v komentáři pythonového zdrojového souboru.

        Standardní konfigurační volby jsou uvedeny přímo zde.  Aplikace může ve
        svém definičním souboru definovat další, své vlastní, konfigurační
        volby použitím potomka třídy 'Configuration' a doplněním dalších
        vnitřních tříd v něm rozšířit dostupné konfigurační volby.

        Po zpracování konfiguračních voleb je zbývající, nezpracovaná, část
        příkazové řádky přiřazena do 'sys.argv'.

        """

        _DESCR = None
        """Breif one line option description to be displayed in the user interface.

        Required only for options, which are supposed to be exposed in the user
        interface.

        """

        _DOC = None
        """Additional multiline description of the option to be displayed in the user interface.

        May be defined only for options, which are supposed to be exposed in the user
        interface and need more explanation than _DESCR gives..

        """

        _DEFAULT = None
        """Default value of the configuration option.

        See `default()' documentation for more information.

        """
        _DEFAULT_STRING = None
        """Výchozí hodnota konfigurační volby pro dump.

        Více viz. dokumentace metody `default_string()'."""

        _CMDLINE = False
        """Příznak, zda tato volba může být zadána také z příkazové řádky.

        Pravdivou hodnotou této konstanty definujeme, že daná volba."""

        _LONG_OPTION = None
        """Specifikace dlouhé volby příkazové řádky pro 'getopt'."""

        _ENVIRONMENT = ()
        """Specifikace jmen proměnných prostředí obsahujících hodnotu volby.

        Více viz. dokumentace metody `environment()'."""

        _VISIBLE = True
        """Specifikace viditelnosti volby.

        Více viz. dokumentace metody `visible()'."""

        _TYPE = None
        """Data type of the option value as 'pytis.data.Type' instance or None.

        See `_type()' documentation for more information.

        """

        def __init__(self, configuration):
            """Inicializuj instanci volby.

            Argumenty:

              configuration -- instance třídy 'Configuration', ve které je
                konfigurační volba přítomna

            """
            self._configuration = configuration
            self._value = self._undefined = object()
            self._changed = False
            self._type_ = self._type()

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

        def _type(self):
            """Return data type of the option value as 'pytis.data.Type' instance or None.

            'None' stands for unspecified type, allowing the value to be any
            Python object.  Otherwise the option values will need to be valid
            inner values of given type.

            Instead of overriding this method directly, it is recommended to
            use predefined classes 'StringOption', 'NumericOption' and
            'BooleanOption'.

            """
            return self._TYPE

        def name(self):
            return self.__class__.__name__[8:]

        def init_value(self, force=False):
            """Inicializuj hodnotu proměnné.

            Argumenty:

              force -- právě když je nepravdivé, inicializuj hodnotu jen tehdy,
                je-li ještě nedefinována

            """
            if force or self._value is self._undefined:
                value = self._compute_init_value(self._configuration)
                self._value = value

        def value(self):
            """Vrať aktuální hodnotu konfigurační volby."""
            # Hodnotu nenastavujeme hned v konstruktoru, protože v té době
            # ještě nemusí být inicializovány jiné volby, na kterých tato volba
            # případně závisí.
            if self._value is self._undefined:
                self.init_value()
            return self._value

        def set_value(self, value, initialization=False):
            """Nastav hodnotu konfigurační volby na 'value'."""
            if not initialization:
                self._changed = True
            self._value = value

        def changed(self):
            """Vrať pravdu, pokud hodnota volby byla změněna aplikací.

            Za změnu je považováno jakékoliv nastavení volby na jinou hodnotu, než
            jakou daná volba nabyla během inicializace (tj. při načítání voleb
            příkazové řádky a konfiguračního souboru).

            """
            return self._changed

        def reset(self):
            """Set option value to the initial default value."""
            self._value = self._undefined
            self._changed = False

        def long_option(self):
            """Vrať specifikaci dlouhé volby pro 'getopt' jako string.

            Specifikace může mít například podobu 'debug' nebo 'datadir='.
            Pokud konfigurační volba není spojena s žádnou volbou příkazové
            řádky, vrať 'None'.

            Specifikaci lze upravit předefinováním konstanty `_OPTION' v
            odvozené třídě.

            """
            if self._LONG_OPTION is not None:
                return self._LONG_OPTION
            elif self._CMDLINE:
                import pytis.data
                name = self.name().replace('_', '-')
                if isinstance(self.type(), pytis.data.Boolean):
                    return name
                else:
                    return name + '='
            else:
                return None

        def environment(self):
            """Vrať tuple jmen proměnných prostředí obsahujících hodnotu volby.

            Jména proměnných jsou strings.  Proměnné prostředí jsou zkoumány
            v uvedeném pořadí a platná je první z nich, která je v prostředí
            přítomna (a to i když je třeba její hodnota prázdná).  Proměnné
            prostředí mají nižší prioritu než volba příkazové řádky nebo
            hodnota v konfiguračním souboru, avšak vyšší prioritu než hodnota
            vrácená metodou 'default'.

            Specifikaci lze upravit předefinováním konstanty `_ENVIRONMENT' v
            odvozené třídě.

            """
            return self._ENVIRONMENT

        def default(self):
            """Return the default value of the configuration option.

            The value returned by this method is used if the option value is
            not set explicitly.

            Option specification may define the default value by overriding the
            constant `_DEFAULT' in simple cases or by overriding this
            method if needed.

            """
            return self._DEFAULT

        def default_string(self):
            """Vrať výchozí hodnotu konfigurační volby pro dump.

            Hodnota je vrácena jako řetězec, který bude vložen do vzorového
            konfiguračního souboru.  Tuto metodu je užitečné předefinovat
            v případě, že výchozí hodnota volby vrácená metodou 'default()' je
            závislá na konkrétním prostředí a/nebo nevystihuje způsob svého
            získání.

            Specifikaci lze upravit předefinováním konstanty `_DEFAULT_STRING'
            v odvozené třídě, nebo ve složitějších případech předefinováním
            této metody.


            """

            if self._DEFAULT_STRING is not None:
                return self._DEFAULT_STRING
            else:
                import pprint
                return pprint.PrettyPrinter().pformat(self.default())

        def type(self):
            """Return data type of the option value as 'pytis.data.Type' instance or None."""
            return self._type_

        def visible(self):
            """Vrať příznak viditelnosti volby.

            Vrácená hodnota určuje, zda má být volba přítomna ve vzorovém
            konfiguračním souboru.

            Specifikaci lze upravit předefinováním konstanty `_VISIBLE'
            v odvozené třídě.

            """
            return self._VISIBLE

        def description(self):
            """Vrať stručný jednořádkový popis volby 'name' jako řetězec."""
            return self._DESCR

        def documentation(self):
            """Vrať podrobný popis volby 'name' jako řetězec nebo None.

            Řetězec tak může být víceřádkový a délka jednoho řádku může přesahovat 80 znaků.  Pokud
            podrobný popis není definován, může vrátit též None.

            """
            return self._DOC

    class StringOption(Option):
        """Třída pro volby řetězcového typu."""

        def _type(self):
            import pytis.data
            return pytis.data.String()

    class BooleanOption(Option):
        """Třída pro volby typu boolean."""

        def _type(self):
            import pytis.data
            return pytis.data.Boolean()

    class ColorOption(Option):
        """Třída pro volby typu barva."""
        _DOC = "Barva je reprezentována řetězcem '#RRGGBB'."

        def _type(self):
            import pytis.data
            return pytis.data.Color()

    class NumericOption(Option):
        """Třída pro volby celočíselného typu."""

        def _type(self):
            import pytis.data
            return pytis.data.Integer()

    class FileOption(StringOption):

        def _compute_init_value(self, *args, **kwargs):
            value = super(Configuration.FileOption, self)._compute_init_value(*args, **kwargs)
            if value and not os.path.isabs(value):
                value = os.path.join(os.getcwd(), value)
            if value and not os.path.exists(value):
                print("Configuration option '{}' contains invalid path: {}"
                      .format(self.name(), value), file=sys.stderr)
            return value

        def _search_default(self):
            return None

        def default(self):
            search = self._search_default()
            if search is not None:
                for d in search:
                    if os.path.exists(d):
                        return d
            return super(Configuration.FileOption, self).default()

    class HiddenOption(Option):
        """Mix-in třída pro skryté volby."""
        _VISIBLE = False

    class CommandlineOption(Option):
        """Mix-in třída pro volby příkazové řádky."""
        _CMDLINE = True

    # Volba pro konfiguraci samu

    class _Option_config_file(StringOption, HiddenOption):
        """Configuration file location."""
        _LONG_OPTION = 'config='
        _ENVIRONMENT = ('PYTISCONFIG',)

    class _Option_user_config_file(StringOption, HiddenOption):
        """User specific configuration file location.

        This file, if exists, is loaded in addition to the main configuratiuon
        file (given by the 'config_file' option) and the options defined there
        have higher precedence than the options in the main configuration file.

        """
        pass

    # Volby užitečné hlavně pro ladění

    class _Option_help(BooleanOption, CommandlineOption, HiddenOption):
        """Option corresponding to --help command line option."""
        _DEFAULT = False

    class _Option_debug(BooleanOption, CommandlineOption):
        u"""Příznak ladícího režimu.

        Je-li zapnut, aplikace může běžet s více kontrolami a vypisovat spoustu
        informací, obvykle však za cenu svého výrazného zpomalení.

        """
        _DEFAULT = False

    class _Option_debug_on_error(BooleanOption, CommandlineOption):
        u"""Příznak vyvolání debuggeru při chybě.

        Dojde-li k odchycení neočekávané výjimky a tato volba je zapnuta, je
        vyvolán interaktivní debugger.  Je-li zapnuta volba 'debug', je
        implicitně zapnuta i tato volba.  Užitečné pouze pro ladění.

        """

        def default(self):
            return self._configuration.debug

    class _Option_debug_memory(BooleanOption, CommandlineOption):
        u"""Příznak výpisu ladících informací o paměti.

        Je-li zapnuta, aplikace vypisuje informativní hlášky garbage collectoru
        a jiné údaje o paměti.

        """
        _DEFAULT = False

    class _Option_bug_report_address(StringOption):
        u"""E-mailová adresa, na kterou mají být posílána oznámení o chybě."""
        _DEFAULT = ''

    class _Option_bug_report_subject(StringOption):
        u"""Subject mailu oznámení o chybě aplikace."""
        _DEFAULT = 'Bug report'

    class _Option_profile(BooleanOption, CommandlineOption):
        u"""Příznak profilování.

        Je-li zapnut, aplikace se spustí v profilovacím režimu a ukládá
        informace o trvání jednotlivých volání do souboru.  Zapnutí této volby
        velmi výrazně zpomaluje běh aplikace.

        """
        _DEFAULT = False

    class _Option_dump_queries(NumericOption, CommandlineOption):
        u"""Number of the most time consuming SQL queries to be printed.

        This option may be practical for debugging the efficiency of SQL queries.
        If non-zero, the times of all SQL queries throughout program execution
        is collected and the given number of the most time consuming queries is
        written to STDOUT when the program ends.

        """
        _DEFAULT = 0

    class _Option_test_run_interactive(BooleanOption, HiddenOption):
        u"""Příznak určující, zda mají být spouštěny i interaktivní testy.

        Týká se pouze regresivního testování.

        """

    class _Option_custom_debug(HiddenOption):
        u"""Zvláštní ladící funkce, napojená na příkaz 'COMMAND_CUSTOM_DEBUG'."""
        _DEFAULT = (lambda self: None)

    # Cesty a adresáře

    class _Option_search_modules(CommandlineOption):
        """Names of python modules containing pytis specifications.

        Sequence of names of python modules which should be searched for pytis
        specifications.  If empty, specification names must by fully qualified
        identifiers of python classes.  If a list is given, the names may be
        relative to one of the modules named in the list.

        """
        _DEFAULT = ()
        _DEFAULT_STRING = "()"

    class _Option_resolver(Option):
        """Specification name resolver.

        Instance of 'pytis.util.Resolver' used globally to resolve specification names.

        """

        def default(self):
            return Resolver(search=self._configuration.search_modules)

    class _Option_help_dir(FileOption, CommandlineOption):
        u"""Directory containing Pytis help files.

        Full path to the directory 'help' found in the root of the pytis
        package.

        """
        _ENVIRONMENT = ('PYTISHELPDIR',)

        def _search_default(self):
            from os.path import dirname
            return [os.path.join(d, 'help')
                    for d in (dirname(dirname(dirname(pytis.__file__))), os.getcwd())]

    class _Option_print_spec_dir(FileOption):
        u"""Adresář obsahující specifikace tiskových sestav.

        Může být zadán absolutně i relativně vzhledem k aktuálnímu adresáři.

        """
        _DEFAULT = './output'

    # TODO: Dočasně vráceno, dokud ve všech projektech nepřejdeme na nový
    # systém nápovědy.
    class _Option_doc_dir(FileOption, CommandlineOption):
        u"""Adresář obsahující dokumentaci."""
        _DEFAULT = './docs'

    class _Option_icon_dir(FileOption):
        u"""Adresář s obrázkovými soubory.

        Může být zadán absolutně i relativně vzhledem k aktuálnímu adresáři.

        """
        _DEFAULT = './icons'

    class _Option_tmp_dir(StringOption):
        u"""Adresář pro dočasné pomocné soubory."""
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
        u"""Cesta k souboru s logem.

        Může být zadán absolutně i relativně vzhledem k aktuálnímu adresáři.

        """
        _DEFAULT = './icons/logo.bmp'

    # Databáze

    class _Option_dbuser(StringOption, CommandlineOption):
        """Database user login name."""
        _DEFAULT_STRING = 'getpass.getuser()'

        def default(self):
            import getpass
            return getpass.getuser()

    class _Option_dbpass(StringOption, CommandlineOption):
        """Database login password."""
        _DEFAULT = None

    class _Option_dbname(StringOption, CommandlineOption):
        """Database name."""
        _DEFAULT = None

    class _Option_dbhost(StringOption, CommandlineOption):
        """Database host name."""
        _DEFAULT = None

    class _Option_dbport(NumericOption, CommandlineOption):
        """Database port number."""
        _DEFAULT = None

    class _Option_dbsslm(StringOption, CommandlineOption):
        """Database SSL mode (one of string constants supported by the DB system)."""
        _DEFAULT = None

    class _Option_dbschemas(StringOption, CommandlineOption):
        """List of database schemas to use in the order of their preference."""
        _DEFAULT = None

    class _Option_dbconnections(HiddenOption):
        """Alternative database connections"

        The default database connection is normally defined by 'dbconnection'.
        Certain applications, however, may require multiple database
        connections, which are configured using this option.  The value is a
        dictionary assigning a connection specification to each connection by
        name.  Connection names are defined by applications (each application
        should mention the names of used conections in its documentation).  The
        connection specification (the value assigned to a connection name) is a
        dictionary with keys 'dbname', 'dbhost', 'dbport', 'dbuser', 'dbpass'
        and 'dbsslm'.  Only 'dbname' is mandatory.  Their meaning and default
        values are the same as for the configuration options of the same names
        specifying the properties of the default connection.

        """
        _DEFAULT = {}

    class _Option_dbconnection(HiddenOption):
        """Database connection specification instance ('pytis.data.DBConnection').

        The instance is constructed from the above db* option by default.

        """

        def default(self):
            map = {'dbname': 'database',
                   'dbhost': 'host',
                   'dbport': 'port',
                   'dbuser': 'user',
                   'dbpass': 'password',
                   'dbschemas': 'schemas',
                   'dbsslm': 'sslmode'}

            def connection_options(items):
                # Transform configuration option names to DBConnection option names.
                options = {'schemas': None}
                for key, value in items:
                    if value is not None:
                        if key == 'dbschemas':
                            value = [s.strip() for s in value.split(',')]
                        options[map[key]] = value
                return options
            cfg = self._configuration
            options = connection_options([(option, getattr(cfg, option))
                                          for option in map.keys() if hasattr(cfg, option)])
            alternatives = [(name, connection_options(opts.items()))
                            for name, opts in cfg.dbconnections.items()]
            import pytis.data
            return pytis.data.DBConnection(alternatives=dict(alternatives), **options)

    class _Option_session_variables(HiddenOption):
        """Custom session variables.

        Dictionary of custom session variables which will be set for
        each connection.
        The dictionary keys have the form 'class.variable' and values
        have to be strings, e.g. {'myvars.myid': '10'}.
        """
        _DEFAULT = {}

    class _Option_dblogtable(StringOption):
        u"""Jméno tabulky, do které mají být logovány DML SQL příkazy."""
        _DEFAULT = ''

    class _Option_dblisten(BooleanOption):
        u"""Flag určující, zda má být spouštěn dohlížeč změn dat."""
        _DEFAULT = True

    class _Option_max_pool_connections(NumericOption):
        """Maximum number of connections in database connection pool.

        Maximum number of database connections stored in a pool for a single
        connection specification.  If None then there is no limit.

        """
        _DEFAULT = None

    class _Option_connection_limit(NumericOption):
        """Maximum number of concurrently open database connections.

        Maximum number of concurrently open database connections or a single
        connection specification in a database pool.  If None then there is no
        limit.

        """
        _DEFAULT = None

    # Access rights options

    class _Option_use_dmp_roles(BooleanOption):
        u"""Use of dmp roles if available.

        This option specifies if access groups should be taken
        from the database specific dmp tables. If True
        and if valid view 'ev_pytis_user_roles' is in the database
        then these specific application roles (groups) will be used,
        otherwise normal postgresql cluster roles will be used.

        """
        _DEFAULT = True

    class _Option_use_dmp_rights(BooleanOption):
        u"""Use of dmp rights if available.

        This option specifies if access rights for menus, application
        specifications and actions are stored in dmp tables.
        If True and valid dmp tables exists and 'use_dmp_roles' is set
        then dmp rights will be used, otherwise access rights from
        specifications will be used.

        """
        _DEFAULT = True

    # Logovací volby

    class _Option_log_logger(Option):
        """Logging class specification.

        "Tuple of (CLASS, ARGS, KWARGS), where CLASS is a logging class and
        ARGS, KWARGS are its constructor arguments and keyword argumnets.

        Standard availavle classes are SyslogLogger and StreamLogger.  More
        about them in their documentation.

        """

        _DEFAULT_STRING = '(log.StreamLogger, (sys.stderr,), {})'

        def default(self):
            from . import StreamLogger
            return (StreamLogger, (sys.stderr,), {})

    class _Option_log_exclude(Option):
        u"""Seznam typů logovacích hlášek, které mají být odfiltrovány.

        V seznamu lze použít konstanty 'OPERATIONAL', 'ACTION', 'EVENT' a
        'DEBUG'.

        """
        _DEFAULT_STRING = '[DEBUG]'

        def default(self):
            if self._configuration.debug:
                return []
            else:
                from . import DEBUG
                return [DEBUG]

    class _Option_log_one_line_preferred(BooleanOption):
        u"""Určuje, zda je preferováno stručné nebo jednotné formátování.

        Je-li tato volba nastavena na pravdu, jsou krátká data v logovacích
        hláškách doporučujících stručnost připojena ihned za hlášku místo
        vypsání na samostatný řádek.

        """
        _DEFAULT = True

    class _Option_log_module_filter(StringOption):
        u"""Prefix jména modulu, jehož debugovací hlášky jsou propuštěny.

        Debugovací logovací hlášky modulů s jiným prefixem jsou odfiltrovány.
        Není-li definováno, jsou propuštěny všechny hlášky (nestanoví-li jiný
        filtr jinak).  Užitečné pouze pro ladění.

        """
        _DEFAULT = ''
        _DEFAULT_STRING = "'pytis.data'"

    class _Option_log_class_filter(Option):
        u"""Sekvence jmen tříd, jejichž debugovací hlášky jsou propuštěny.

        Debugovací logovací hlášky ostatních tříd jsou odfiltrovány.  Je-li
        'None', jsou propuštěny všechny hlášky (nestanoví-li jiný filtr jinak).
        Užitečné pouze pro ladění.

        """
        _DEFAULT = None
        _DEFAULT_STRING = "('pytis.data.DBDefaultClass',)"

    # Externí programy

    class _Option_postscript_viewer(StringOption):
        """Shell command to be used for displaying print preview PDF files.

        It must take the name of the file to be displayed as its first argument.

        """
        _DEFAULT = ''

    class _Option_sendmail_command(StringOption):
        u"""Shellový příkaz sendmail včetně celé cesty (DEPRECATED)."""
        # Používáno již jen v pytis-extensions...
        _DEFAULT = '/usr/lib/sendmail'

    class _Option_smtp_server(StringOption):
        u"""Jméno serveru odchozí pošty."""
        _DEFAULT = 'localhost'

    # Komunikace s klientskými stanicemi

    class _Option_rpc_local_port(NumericOption):
        u"""Lokální komunikační port pro naslouchání pytisovým aplikacím."""
        _DEFAULT = 17984

    class _Option_rpc_remote_port(NumericOption):
        u"""Vzdálený komunikační port na klientských stanicích."""
        _DEFAULT = 17984

    class _Option_rpc_key_file(FileOption):
        u"""Soubor s klíčem certifikátu pro komunikaci s klientskými stanicemi."""
        _DEFAULT = 'linux.key'

    class _Option_rpc_certificate_file(FileOption):
        u"""Soubor s certifikátem pro komunikaci s klientskými stanicemi."""
        _DEFAULT = 'linux.crt'

    class _Option_rpc_communication_enabled(BooleanOption):
        """Enable RPC communication with client machines."""
        _DESCR = _("Enable RPC communication with client machines.")
        _DEFAULT = True

    class _Option_session_id(StringOption, CommandlineOption):
        """X2Go session id."""
        _DESCR = _("For internal use only.")
        _DEFAULT = None

    # Ostatní konfigurační volby

    class _Option_application_name(StringOption):
        u"""Jméno aplikace.

        Jméno může být libovolné, používá se např. jako titulek okna nebo při
        logování.  Od něho je také odvozeno jméno výchozího souboru pro
        ukládání uživatelských změn v konfiguraci (po vypuštění speciálních
        znaků a diakritiky)

        """
        _DEFAULT = 'Pytis'

    class _Option_date_time_format(StringOption):
        u"""Formát společně uvedeného data a času.

        Řetězec ve tvaru vyžadovaném parametrem `format' konstruktoru třídy
        'pytis.data.DateTime'.

        """

        def default(self):
            import pytis.data
            return pytis.data.DateTime.DEFAULT_FORMAT

    class _Option_date_format(StringOption):
        u"""Formát data.

        Řetězec ve tvaru vyžadovaném parametrem `format' konstruktoru třídy
        'pytis.data.Date'.

        """

        def default(self):
            import pytis.data
            return pytis.data.Date.DEFAULT_FORMAT

    class _Option_time_format(StringOption):
        u"""Formát času.

        Řetězec ve tvaru vyžadovaném parametrem `format' konstruktoru třídy
        'pytis.data.Time'.

        """

        def default(self):
            import pytis.data
            return pytis.data.Time.DEFAULT_FORMAT

    class _Option_export_directory(StringOption):
        """Target directory for exports.

        The exported files in CSV or XLS format will be saved into this
        directory.  Must be a valid filesystem path.

        """
        _DESCR = _("Target directory for saving exported files.")
        _DEFAULT = '/tmp'

    class _Option_export_encoding(StringOption):
        """Exported data encoding.

        The exported files in CSV or XLS format will be saved using this
        encoding.  Must be one of Pyhon supported encodings.

        """
        _DESCR = _("Exported data encoding.")
        _DOC = _("Must be one of Pyhon supported encodings.")
        _DEFAULT = 'iso8859-2'

    class _Option_cache_size(NumericOption):
        """Maximal total number of rows cached for a data object selection."""
        _DEFAULT = 20000

    class _Option_initial_fetch_size(NumericOption):
        """Number of rows fetched into the cache during the first read from a DB select."""
        _DEFAULT = 100

    class _Option_fetch_size(NumericOption):
        """Number of rows fetched into the cache during subsequent reads from a DB select."""
        _DEFAULT = 100

    class _Option_sender_address(StringOption):
        u"""E-mailová adresa odesílatele použitá např. jako odchozí adresa bug-reportů apod."""
        _DEFAULT = None

    class _Option_clipboard_primary_selection(BooleanOption):
        u"""Flag určující, zda se má při exportu použít primary selection."""
        _DEFAULT = False

    class _Option_use_wx_clipboard(BooleanOption):
        u"""Flag určující, zda se má pro kopírování obsahu buňky použít wxClipboard"""
        _DEFAULT = True

    class _Option_form_statistics(BooleanOption):
        u"""Flag povolující ukládání statistických informací o otevírání formulářů.

        Flag určující, zda mají být do databáze ukládány statistické informace
        o otevíraných formulářích.

        """
        _DEFAULT = False

    class _Option_http_proxy(StringOption):
        u"""HTTP proxy URI used for integrated web browser and other http services.

        Example: 'http://127.0.0.1:3129'

        """
        _DEFAULT = None

    class _Option_fallback_table_print(BooleanOption):
        u"""Flag povolující výchozí tisk formuláře.

        Flag určující, zda se má při tisku vytisknout prostá tabulka formuláře,
        pokud není aplikovatelný žádný specifičtější tisk.  Není-li nastaveno,
        nevytiskne se v takovém případě nic.

        """
        _DEFAULT = False

    class _Option_run_form_timeout(NumericOption):
        u"""Časový limit pro přerušení zdlouhavého otevírání formuláře.

        Počet sekund, po kterých se uživateli nabídne přerušení zdlouhavého
        otevírání formuláře.

        """
        _DEFAULT = 40

    class _Option_edit_form_timeout(NumericOption):
        u"""Časový limit pro uzavření editačního formuláře při nečinnosti.

        Po uvedeném počtu sekund, během nichž uživatel nepracoval s editačním
        formulářem, se tento formulář automaticky uzavře.  Nastavením hodnoty
        na 'None' se automatické uzavírání editačních formulářů vypne.

        """
        _DEFAULT = None

    class _Option_output_row_limit(NumericOption):
        u"""Limit počtu řádků tisku bez potvrzení.

        Maximální počet řádků, pro který se v tiskových sestavách formátuje
        datová tabulka bez potvrzení uživatele.

        """
        _DEFAULT = 1000

    class _Option_max_transaction_time(NumericOption):
        """Time limit for an open transaction in seconds.

        When the time is exceeded, some action may be taken on the transaction.

        """
        _DESCR = _("Maximum time in seconds to leave an open transaction untouched.")
        _DEFAULT = None

    class _Option_max_transaction_idle_time(NumericOption):
        """Time limit for an idle transaction in seconds.

        When the time is exceeded, some action may be taken on the transaction.

        """
        _DESCR = _("Maximum time in seconds to leave an idle transaction untouched.")
        _DEFAULT = None

    class _Option_login_selection(Option):
        """Selection of available login names for database login dialog.

        When set, the login dialog will only allow selection from given login
        names.  When None (the default), login name can be entered into a text
        field.  The value is a sequence of strings (login names) or tuples of
        two strings (login name and password).

        """
        _DEFAULT = None

    # Volby přizpůsobení uživatelského rozhraní

    class _Option_show_tooltips(BooleanOption):
        """Show tooltips."""
        _DESCR = _("Show tooltips.")
        _DEFAULT = True

    class _Option_stretch_tables(BooleanOption):
        """Stretch table columns to fit full window width."""
        _DESCR = _("Stretch table columns to fit full window width.")
        _DEFAULT = True

    class _Option_show_splash(BooleanOption):
        """Show the initial splash screen.

        The splash screen is actually displayed by application specific code,
        so the filal interpretation of this option depends on the application.

        """
        _DESCR = _("Show the initial splash screen.")
        _DEFAULT = True

    class _Option_auto_menu_accel(BooleanOption):
        """Enable automatic menu accelerators.

        When enabled, keyboard shortcuts are automatically assigned in an
        alhabetical order to all menus in the wx application.  Thus the menu
        items have shortcuts a, b, c, ... in the order in which they appear in
        the menu.

        """
        _DESCR = _("Enable automatic keyboard shortcut accelerators in menu items.")
        _DOC = _("Change requires the application to be restarted.")
        _DEFAULT = True

    class _Option_cache_spec_onstart(BooleanOption):
        """Cache all specifications on application startup.

        The caching is actually performed by application specific code, so the
        filal interpretation of this option depends on the application.

        """
        _DESCR = _("Cache all specifications on application startup.")
        _DOC = _("Initializes specifications of all forms when the application "
                 "starts which may slightly reduce their subsequent startup "
                 "time for the cost of a short initial delay.")
        _DEFAULT = True

    class _Option_startup_forms(StringOption, CommandlineOption):
        u"""Seznam formulářů, které mají být otevřeny po spuštění aplikace."""
        _DEFAULT = None

    class _Option_autostart_saved_forms(BooleanOption):
        """Dont' ask whether to start forms saved on last exit.
        """
        _DOC = _("Start forms saved on last exit without asking.")
        _DOC = _("If true, the forms saved on last exit will be automatically "
                 "loaded on application startup without asking.  If False (the "
                 "default), a dialog asking for confirmation of saved forms to "
                 "start will be displayed.")
        _CMDLINE = True
        _DEFAULT = False

    class _Option_keyboard_layouts(Option):
        """Sequence of keyboard layout specifications for built-in layout swither.

        The sequence consists of triplets (title, icon, command), where 'title'
        is the label for the UI layout selector (basestring), 'icon' is the
        corresponding icon identifier for 'pytis.form.get_icon()' -- name of a
        file located in 'config.icon_dir' without the '.png' suffix
        (basestring) and 'command' is the system command to be invoked to
        switch to given layout (basestring).

        """
        _DEFAULT = ()

    class _Option_initial_keyboard_layout(StringOption):
        """System command for switching the initial keyboard layout.

        The value must be one of the commands specified within the
        'keyboard_layouts' option.  Only if it is present there, the command
        will be invoked on application startup and the keyboard switcher will
        indicate the layout by the corresponding icon from 'keyboard_layouts'.

        """
        _DEFAULT = None

    class _Option_row_edit_fg_color(ColorOption):
        u"""Barva textu editovaného řádku tabulkového formuláře."""
        _DEFAULT = '#ffffff'

    class _Option_row_edit_bg_color(ColorOption):
        u"""Barva pozadí editovaného řádku."""
        _DEFAULT = '#c80000'

    class _Option_cell_highlight_color(ColorOption):
        """Current table cell highlight color.

        The current table cell has a thin colored border around.

        """
        _DESCR = _("Current cell highlight color in table forms.")
        _DEFAULT = '#ffa000'

    class _Option_row_highlight_color(ColorOption):
        """Current table row highlight color.

        The current table row has a thin colored border around.

        """
        _DESCR = _("Current row highlight color in table forms.")
        _DEFAULT = '#00a0ff'

    class _Option_row_highlight_edited_color(ColorOption):
        """Edited table row highlight color.

        The current row border has a different color during inline editation.

        """
        _DESCR = _("Current row highlight color in table forms during inline editation.")
        _DEFAULT = '#ff0000'

    class _Option_row_highlight_unfocused_color(ColorOption):
        """Unfocussed table row highlight color.

        The current row border has a different color when the form is not
        focussed (ie. in the inactive form of a dual form).

        """
        _DESCR = _("Current row highlight color in unfocussed table forms.")
        _DEFAULT = '#808080'

    class _Option_row_highlight_width(NumericOption):
        """Current table row border width.

        The border width in pixels.

        """
        _DESCR = _("Current table row highlight border width in pixels.")
        _DEFAULT = 3

        def _type(self):
            import pytis.data
            return pytis.data.Integer(minimum=0, maximum=10)

    class _Option_grid_line_color(ColorOption):
        """Table grid line color.

        Color of the lines between table cells.

        """
        _DESCR = _("Grid line color in table forms.")
        _DEFAULT = '#6482be'

    class _Option_grouping_background_downgrade(ColorOption):
        """Grouping background downgrade.

        Grouping may be used to visually distinguish table rows that belong to
        the same group.  Table background color is changing between the groups
        of rows, so that lighter colored rows are followed by darker colored
        rows and vice versa.  The color specification in this option determines
        the level and the tone of the darker rows.  When combined with white
        rows, the darker rows wil have the exactly same color as specified.
        But some rows may be already colored otherwise and in this case the two
        colors are combined.

        """
        _DESCR = _("Grouping background downgrade.")
        _DOC = _("Grouping may be used to visually distinguish table rows "
                 "that belong to the same group.  Table background color is "
                 "changing between the groups of rows, so that lighter "
                 "colored rows are followed by darker colored rows and vice "
                 "versa.  The color specification in this option determines "
                 "the level and the tone of the darker rows.  When combined "
                 "with white rows, the darker rows wil have the exactly same "
                 "color as specified.  But some rows may be already colored "
                 "otherwise and in this case the two colors are combined.")
        _DEFAULT = '#eceef0'

    class _Option_filter_color(ColorOption):
        u"""Barva záhlaví tabulky při zapnutém filtrování."""
        _DEFAULT = '#82c882'

    # Metody

    def __init__(self):
        self.__dict__['_reading_configuration'] = False
        self._init_options()

    def add_command_line_options(self, command_line):
        """Nastav volby dle příkazové řádky.

        Argumenty:

          command_line -- volby příkazové řádky jako sekvence strings; typicky
            'sys.argv'

        """
        self._init_options(command_line=command_line)

    def _init_options(self, command_line=None):
        PREFIX = '_Option_'
        options = {}
        for k, v in self.__class__.__dict__.items():
            if k.startswith(PREFIX):
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
        if self._reading_configuration:
            return
        self._reading_configuration = True
        try:
            conffile = self._config_file
            if conffile is not None:
                self.__dict__['_config_mtime'] = self._read_configuration_file(conffile)
            uconffile = self._user_config_file
            if uconffile is not None:
                self.__dict__['_user_config_mtime'] = self._read_configuration_file(uconffile)
        finally:
            self._reading_configuration = False

    def _read_configuration_file(self, filename=True):
        try:
            filetime = os.stat(filename)[stat.ST_MTIME]
        except Exception:
            raise Exception("Unable to stat configuration file:", filename)
        try:
            f = open(filename)
        except Exception:
            raise Exception("Unable to open configuration file:", filename)
        try:
            del sys.modules['_config']
        except Exception:
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
        if name in self.__dict__['_options']:
            self.__dict__['_options'][name].set_value(value)
        elif hasattr(self, name):
            self.__dict__[name] = value
        else:
            raise AttributeError(name)
        if name == 'user_config_file':
            if self.__dict__['_user_config_file'] != value:
                self.__dict__['_user_config_file'] = value
                self.__dict__['_user_config_mtime'] = 0
                self._read_configuration()

    def merge(self, dict, override_cmdline=False):
        """Nastav aktuální konfiguraci z hodnot daného slovníku.

        Argumenty:
          dict -- slovník, ze kterého mají být převzaty nové hodnoty.
            Převezmou se pouze hodnoty klíčů, jejichž názvy odpovídají
            definovaným konfiguračním volbám a to pouze v případě, že jsou
            definovány (obsahují jinou hodnotu než None).  Ostatní budou
            ignorovány.
          override_cmdline -- implicitně nejsou přenastavovány hodnoty převzaté
            z příkazové řádky.  Pravdivá hodnota tohoto argumentu způsobí, že
            budou přenastaveny všechny nalezené konfigurační volby včetně těch
            z příkazového řádku.

        """
        options = self._options
        clopt = self.command_line_options
        for o in dict.keys():
            if o in options and dict[o] is not None:
                opt = options[o]
                if override_cmdline or opt.long_option() not in clopt:
                    opt.set_value(dict[o])

    def dump_config_template(self, stream):
        """Write configuration file template to 'stream'.

        'stream' must be a stream opened for writing.

        """
        # stream.write('# -*- coding: utf-8 -*-\n\n')
        from textwrap import wrap
        for option in self.options(sort=True):
            if option.visible():
                stream.write('# %s\n' % option.description())
                doc = option.documentation()
                if doc:
                    for line in wrap(doc, 77):
                        stream.write('# %s\n' % line.strip())
                value = option.default_string()
                indent = ' ' * (len(option.name()) + 3)
                stream.write('#%s = %s\n\n' % (option.name(), value.replace("\n", "\n#" + indent)))

    def options(self, sort=False):
        """Return a tuple of all configuration options as 'Configuration.Option' instances.

        If 'sort' is true, the options will be returned in the order of their definition.

        """
        options = tuple(self._options.values())
        if sort:
            options = sorted(options, key=lambda x: x._class_definition_order)
        return options

    def option(self, name):
        """Return the 'Configuration.Option' instance for the option of given 'name'.

        'name' must be a string name of an existing configuration option.  'AttributeError' is
        raised if no such option exists.

        """
        try:
            return self._options[name]
        except KeyError:
            raise AttributeError(name)


def set_configuration_file(configuration_file):
    """Set configuration file and reset all configuration options.

    Arguments:

      configuration_file -- name of the configuration file, basestring, or
        'None' (in which case nothing happens)

    """
    if configuration_file is not None:
        for o in pytis.config.options():
            o.reset()
        pytis.config.config_file = configuration_file
        pytis.config.read_configuration_file(configuration_file)
