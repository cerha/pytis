# -*- coding: iso-8859-2 -*-

# Prostředky pro definici a zpracování konfigurace běhu aplikace
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

"""Prostředky pro definici a zpracování konfigurace běhu aplikace.

Celá konfigurace je definována instancí třídy 'Configuration', dokumentace
v této třídě poví více.

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
        __metaclass__ = _OrderedDefinitionClass
        
        _DESCR = None
        """Stručný (jednořádkový) popis volby."""
        
        _DOC = None
        """Podrobnější dokumentace významu volby a přípustných hodnot."""
        
        _DEFAULT = None
        """Výchozí hodnota konfigurační volby.

        Více viz. dokumentace metody `default()'."""

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
        """Datový typ volby jako instance třídy 'pytis.data.Type' nebo None.

        Hodnota 'None' určuje blíže nespecifikovaný typ.  Takové volby umožňují
        předávat libovolný Pythonový objekt.

        Namísto přímého nastavování této konstanty je doporučováno použít
        předdefinovaných tříd 'StringOption', 'NumericOption' nebo
        'BooleanOption' (viz níže).

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
                name = self.name().replace('_','-')
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
            """Vrať výchozí hodnotu konfigurační volby.
            
            Hodnota vrácená touto metodou je použita, pokud nebylo možno
            výchozí hodnotu volby zjistit jinak.

            Specifikaci lze upravit předefinováním konstanty `_DEFAULT' v
            odvozené třídů, nebo ve složitějších případech předefinováním této
            metody.
            
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
            return self._TYPE
        
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
        _TYPE = pytis.data.String()        

    class BooleanOption(Option):
        """Třída pro volby typu boolean."""
        _TYPE = pytis.data.Boolean()        

    class ColorOption(Option):
        """Třída pro volby typu barva."""
        _TYPE = pytis.data.Color()
        _DOC = "Barva je reprezentována řetězcem '#RRGGBB'."

    class NumericOption(Option):
        """Třída pro volby celočíselného typu."""
        _TYPE = pytis.data.Integer()        

    class FileOption(StringOption):
        def _compute_init_value(self, *args, **kwargs):
            value = super(Configuration.FileOption, self).\
                    _compute_init_value(*args, **kwargs)
            if not os.path.isabs(value):
                value = os.path.join(os.getcwd(), value)
            return value

    class HiddenOption(Option):
        """Mix-in třída pro skryté volby."""
        _VISIBLE = False
        
    class CommandlineOption(Option):
        """Mix-in třída pro volby příkazové řádky."""
        _CMDLINE = True
        
    # Volba pro konfiguraci samu

    class _Option_config_file(StringOption, HiddenOption):
        _DESCR = _("Umístění konfiguračního souboru.")
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
        _DESCR = _("Umístění doplňujícího konfiguračního souboru uživatele.")
        _DOC = _("Tento soubor, pokud, existuje, je načítán navíc ke "
                 "standardní konfiguraci a v něm definované volby mají vyšší "
                 "prioritu než volby ve standardním konfiguračním souboru. "
                 "Užitečné převážně pro ladění.")
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
        
    # Volby užitečné hlavně pro ladění

    class _Option_help(BooleanOption, CommandlineOption, HiddenOption):
        _DESCR = _("Volba odpovídající --help na příkazové řádce.")
        _DEFAULT = False
        
    class _Option_debug(BooleanOption, CommandlineOption):
        _DESCR = _("Příznak ladícího režimu.")
        _DOC = _("Je-li zapnut, aplikace může běžet s více kontrolami "
                 "a vypisovat spoustu informací, obvykle však za cenu svého "
                 "výrazného zpomalení.")
        _DEFAULT = False
        
    class _Option_debug_on_error(BooleanOption, CommandlineOption):
        _DESCR = _("Příznak vyvolání debuggeru při chybě.")
        _DOC = _("Dojde-li k odchycení neočekávané výjimky a tato volba je "
                 "zapnuta, je vyvolán interaktivní debugger.  Je-li zapnuta "
                 "volba 'debug', je implicitně zapnuta i tato volba.  Užitečné "
                 "pouze pro ladění.")
        
        def default(self):
            return self._configuration.debug

    class _Option_debug_memory(BooleanOption, CommandlineOption):
        _DESCR = _("Příznak výpisu ladících informací o paměti.")
        _DOC = _("Je-li zapnuta, aplikace vypisuje informativní hlášky "
                 "garbage collectoru a jiné údaje o paměti.")
        _DEFAULT = False

    class _Option_bug_report_address(StringOption):
        _DESCR = _("E-mailová adresa, na kterou mají být posílána oznámení "
                   "o chybě.")
        _DEFAULT = ''

    class _Option_bug_report_subject(StringOption):
        _DESCR = _("Subject mailu oznámení o chybě aplikace.")
        _DEFAULT = 'Bug report'

    class _Option_profile(BooleanOption, CommandlineOption):
        _DESCR = _("Příznak profilování.")
        _DOC = _("Je-li zapnut, aplikace se spustí v profilovacím režimu "
                 "a ukládá informace o trvání jednotlivých volání do souboru. "
                 "Zapnutí této volby velmi výrazně zpomaluje běh aplikace.")
        _DEFAULT = False
        
    class _Option_test_run_interactive(BooleanOption, HiddenOption):
        _DESCR = _("Příznak určující, zda mají být spouštěny i interaktivní testy.")
        _DOC = _("Týká se pouze regresivního testování.")

    class _Option_custom_debug(HiddenOption):
        _DESCR = _("Zvláštní ladící funkce, napojená na příkaz 'COMMAND_CUSTOM_DEBUG'.")
        _DEFAULT = (lambda: None)

    # Cesty a adresáře

    class _Option_def_dir(FileOption, CommandlineOption):
        _DESCR = _("Adresář obsahující definiční soubory.")
        _DOC = _("Adresář může být zadán absolutně i relativně vzhledem "
                 "k aktuálnímu adresáři.")
        _DEFAULT = './defs'
        _ENVIRONMENT = ('PYTISDEFDIR',)

    class _Option_help_dir(FileOption, CommandlineOption):
        _DESCR = _("Adresář obsahující soubory s nápovědou.")
        _DOC = _("Může být zadán absolutně i relativně vzhledem k aktuálnímu "
                 "adresáři.")
        _ENVIRONMENT = ('PYTISHELPDIR',)
        _DEFAULT = './help'

    # TODO: Dočasně vráceno, dokud ve všech projektech nepřejdeme na nový
    # systém nápovědy.
    class _Option_doc_dir(FileOption, CommandlineOption):
        _DESCR = _("Adresář obsahující dokumentaci.")
        _DEFAULT = './docs'

    class _Option_icon_dir(FileOption):
        _DESCR = _("Adresář s obrázkovými soubory.")
        _DOC = _("Může být zadán absolutně i relativně vzhledem k aktuálnímu "
                 "adresáři.")
        _DEFAULT = './icons'

    class _Option_tmp_dir(StringOption):
        _DESCR = _("Adresář pro dočasné pomocné soubory.")
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
        _DOC = _("Může být zadán absolutně i relativně vzhledem k aktuálnímu "
                 "adresáři.")
        _DEFAULT = './icons/logo.bmp'

    class _Option_server(StringOption, CommandlineOption):
        _DESCR = _("Jméno stroje (řetězec), na kterém běží Pyro server.")
        _DOC = _("Může být též 'None', pak se klient nepřipojuje na server "
                 "a používá lokální konfiguraci.")
        _DEFAULT = None

    # Databáze
    
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
        _DESCR = _("Jméno tabulky, do které mají být logovány DML SQL příkazy.")
        _DEFAULT = ''

    class _Option_dblisten(BooleanOption):
        _DESCR = _("Flag určující, zda má být spouštěn dohlížeč změn dat.")
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

    # Logovací volby

    class _Option_log_logger(Option):
        _DESCR = _("Specifikace logovací třídy.")
        _DOC = _("Trojice (CLASS, ARGS, KWARGS), kde CLASS je logovací třída a "
                 "ARGS, resp. KWARGS, jsou argumenty, resp. klíčované "
                 "argumenty, jejího konstruktoru.  Standardní dostupné třídy "
                 "jsou SyslogLogger a StreamLogger.  Více o nich lze nalézt "
                 "v jejich dokumentaci.")
        
        _DEFAULT_STRING = '(log.StreamLogger, (sys.stderr,), {})'
        def default(self):
            import log
            return (log.StreamLogger, (sys.stderr,), {})

    class _Option_log_exclude(Option):
        _DESCR = _("Seznam typů logovacích hlášek, které mají být "
                   "odfiltrovány.")
        _DOC = _("V seznamu lze použít konstanty 'OPERATIONAL', 'ACTION', "
                 "'EVENT' a 'DEBUG'.")
        _DEFAULT_STRING = '[DEBUG]'
        def default(self):
            if self._configuration.debug:
                return []
            else:
                import log
                return [log.DEBUG]

    class _Option_log_one_line_preferred(BooleanOption):
        _DESCR = _("Určuje, zda je preferováno stručné nebo jednotné "
                   "formátování.")
        _DOC = _("Je-li tato volba nastavena na pravdu, jsou krátká data "
                 "v logovacích hláškách doporučujících stručnost připojena "
                 "ihned za hlášku místo vypsání na samostatný řádek.")
        _DEFAULT = True

    class _Option_log_module_filter(StringOption):
        _DESCR = _("Prefix jména modulu, jehož debugovací hlášky jsou "
                   "propuštěny.")
        _DOC = _("Debugovací logovací hlášky modulů s jiným prefixem jsou "
                 "odfiltrovány.  Není-li definováno, jsou propuštěny všechny "
                 "hlášky (nestanoví-li jiný filtr jinak).  Užitečné pouze pro "
                 "ladění.")
        _DEFAULT = ''
        _DEFAULT_STRING = "'pytis.data'"

    class _Option_log_class_filter(Option):
        _DESCR = _("Sekvence jmen tříd, jejichž debugovací hlášky jsou "
                   "propuštěny.")
        _DOC = _("Debugovací logovací hlášky ostatních tříd jsou odfiltrovány. "
                 "Je-li 'None', jsou propuštěny všechny hlášky (nestanoví-li "
                 "jiný filtr jinak).  Užitečné pouze pro ladění.")
        _DEFAULT = None
        _DEFAULT_STRING = "('pytis.data.DBDefaultClass',)"
            
    # Externí programy

    class _Option_printing_command(StringOption):
        _DESCR = _("Shellový příkaz pro provedení tisku, včetně argumentů.")
        _DOC = _("Příkaz musí být schopen převzít tisková data ze "
                 "standardního vstupu.")
        _DEFAULT = 'lpr'

    class _Option_lout_command(StringOption):
        _DESCR = _("Jméno programu Lout, s cestou nebo bez ní.")
        _DOC = _("Bude použito při konstrukci příkazové řádky Lout. "
                 "Nejedná-li se o originální Lout, musí být argumenty "
                 "programu s Loutem kompatibilní.")
        _DEFAULT = 'lout'

    class _Option_postscript_viewer(StringOption):
        _DESCR = _("Shell command to be used for displaying print preview PostScript files. "
                   "It must take the name of the file to be displayed as its first argument. "
                   "If this option value is empty, Pytis internal viewer is used.")
        _DEFAULT = ''
        #_DEFAULT = 'gv'

    class _Option_sendmail_command(StringOption):
        # Používáno již jen v pytis-extensions...
        _DESCR = _("Shellový příkaz sendmail včetně celé cesty (DEPRECATED).")
        _DEFAULT = '/usr/lib/sendmail'
        
    class _Option_smtp_server(StringOption):
        _DESCR = _("Jméno serveru odchozí pošty.")
        _DEFAULT = 'localhost'
        
    class _Option_image_viewer(StringOption):
        _DESCR = _("Shellový příkaz pro spuštění prohlížeče obrázků.  Pokud "
                   "příkaz obsahuje řetězec %f, bude tento nahrazen názvem "
                   "otevíraného souboru, jinak je soubor připojen na konec "
                   "příkazu.")
        _DEFAULT = 'run-mailcap'
        
    # Ostatní konfigurační volby

    class _Option_application_name(StringOption):
        _DESCR = _("Jméno aplikace.")
        _DOC = _("Jméno může být libovolné, používá se např. jako titulek "
                 "okna nebo při logování.  Od něho je také odvozeno jméno "
                 "výchozího souboru pro ukládání uživatelských změn v "
                 "konfiguraci (po vypuštění speciálních znaků a diakritiky)")
        _DEFAULT = 'Pytis'

    class _Option_date_time_format(StringOption):
        _DESCR = _("Formát společně uvedeného data a času.")
        _DOC = _("Řetězec ve tvaru vyžadovaném parametrem `format' "
                 "konstruktoru třídy 'pytis.data.DateTime'.")
        _DEFAULT = pytis.data.DateTime.DEFAULT_FORMAT

    class _Option_date_format(StringOption):
        _DESCR = _("Formát data.")
        _DOC = _("Řetězec ve tvaru vyžadovaném parametrem `format' "
                 "konstruktoru třídy 'pytis.data.Date'.")
        _DEFAULT = pytis.data.Date.DEFAULT_FORMAT

    class _Option_time_format(StringOption):
        _DESCR = _("Formát času.")
        _DOC = _("Řetězec ve tvaru vyžadovaném parametrem `format' "
                 "konstruktoru třídy 'pytis.data.Time'.")
        _DEFAULT = pytis.data.Time.DEFAULT_FORMAT

    class _Option_lc_numeric(StringOption):
        _DESCR = _("Numeric locale.")
        _DOC = _("Hodnota musí být string reprezentující locale pro "
                 "formátování číselných položek.")
        _DEFAULT = 'C'

    class _Option_export_directory(StringOption):
        _DESCR = _("Adresář pro export do CSV souborů.")
        _DOC = _("Hodnota udává cestu k adresáři, kde se budou ukládat textové "
                 "CSV soubory.")
        _DEFAULT = '/tmp'

    class _Option_export_encoding(StringOption):
        _DESCR = _("Kódování exportovaných dat.")
        _DOC = _("Hodnota musí být jedním z podporovaných kódování v Pythonu.")
        _DEFAULT = 'iso8859-2'

    class _Option_cache_size(NumericOption):
        _DESCR = _("Velikost cache pro řádky datového objektu.")
        _DOC = _("Velikost je celé číslo, které udává počet řádků cache.")
        _DEFAULT = 20000

    class _Option_initial_fetch_size(NumericOption):
        _DESCR = _("Počet řádků, které se přednačtou do cache při prvním "
                   "selectu z datového objektu.")
        _DEFAULT = 100

    class _Option_fetch_size(NumericOption):
        _DESCR = _("Počet řádků, které se přinačítají do cache při dalších "
                   "selectech z datového objektu.")
        _DEFAULT = 100

    class _Option_sender_address(StringOption):
        _DESCR = _("E-mailová adresa odesílatele použitá např. jako odchozí adresa bug-reportů "
                   " apod.")
        _DEFAULT = None

    class _Option_clipboard_primary_selection(BooleanOption):
        _DESCR = _("Flag určující, zda se má při exportu použít primary selection.")
        _DEFAULT = False

    class _Option_use_wx_clipboard(BooleanOption):
        _DESCR = _("Flag určující, zda se má pro kopírování obsahu buňky použít wxClipboard")
        _DEFAULT = True

    class _Option_form_statistics(BooleanOption):
        _DESCR = _("Flag určující, zda mají být do databáze ukládány statistické informace "
                   "o otevíraných formulářích.")
        _DEFAULT = False
        
    # Volby přizpůsobení uživatelského rozhraní
        
    class _Option_show_tooltips(BooleanOption):
        _DESCR = _("Zobrazovat bublinovou nápovědu.")
        _DEFAULT = True
        
    class _Option_stretch_tables(BooleanOption):
        _DESCR = _("Roztahovat sloupce tabulek, aby využily celou šířku okna.")
        _DEFAULT = True
        
    class _Option_show_splash(BooleanOption):
        _DESCR = _("Zobrazovat úvodní uvítací dialog.")
        _DEFAULT = True
        
    class _Option_auto_menu_accel(BooleanOption):
        _DESCR = _("Automaticky doplnit položky menu prefixy akcelerátorových kláves (změna "
                   "vyžaduje restart aplikace).")
        _DEFAULT = True
        
    class _Option_cache_spec_onstart(BooleanOption):
        _DESCR = _("Příznak cachování specifikací při startu aplikace.")
        _DEFAULT = True

    class _Option_startup_forms(StringOption, CommandlineOption):
        _DESCR = _("Seznam formulářů, které mají být otevřeny po spuštění "
                   "aplikace.")
        _DEFAULT = None

    class _Option_row_focus_fg_color(ColorOption):
        _DESCR = _("Barva textu aktivního řádku tabulkového formuláře.")
        _DEFAULT = '#ffffff'
        
    class _Option_row_focus_bg_color(ColorOption):
        _DESCR = _("Barva pozadí aktivního řádku tabulkového formuláře.")
        _DOC   = _("Pokud barva není nastavena, bude použita systémová barva "
                   "zvýraznění.  Barva je reprezentována řetězcem '#RRGGBB'.")
        _DEFAULT = None
        
    class _Option_row_nofocus_fg_color(ColorOption):
        _DESCR = _("Barva textu neaktivního řádku tabulkového formuláře.")
        _DEFAULT = '#000000'
        
    class _Option_row_nofocus_bg_color(ColorOption):
        _DESCR = _("Barva pozadí neaktivního řádku tabulkového formuláře.")
        _DEFAULT = '#b6b6b6'
        
    class _Option_row_edit_fg_color(ColorOption):
        _DESCR = _("Barva textu editovaného řádku tabulkového formuláře.")
        _DEFAULT = '#ffffff'

    class _Option_row_edit_bg_color(ColorOption):
        _DESCR = _("Barva pozadí editovaného řádku.")
        _DEFAULT = '#c80000'

    class _Option_cell_highlight_color(ColorOption):
        _DESCR = _("Barva zvýraznění aktivní buňky tabulkového formuláře.")
        _DEFAULT = '#ffa000'

    class _Option_grid_line_color(ColorOption):
        _DESCR = _("Barva mřížky tabulkového formuláře.")
        _DEFAULT = '#6482be'

    class _Option_grouping_background_downgrade(ColorOption):
        _DESCR = _("Ztmavení barvy skupiny při seskupování řádků.")
        _DOC = _("Protože barva pozadí řádků není vždy bílá, je tato hodnota "
                 "chápána jako relativní.  O kolik je zvolená barva tmavší "
                 "než bílá, o tolik bude výsledná barva skupiny tmavší, než "
                 "barva pozadí ostatních řádků.  Barva je reprezentována "
                 "řetězcem '#RRGGBB'.")
        _DEFAULT = '#eceef0'

    class _Option_field_disabled_color(ColorOption):
        _DESCR = _("Barva pozadí needitovatelného vstupního políčka.")
        _DEFAULT = '#d8d8d8'
        
    class _Option_field_denied_color(ColorOption):
        _DESCR = _("Pozadí políčka needitovatelného kvůli přístupovým právům.")
        _DEFAULT = '#e0e4f0'

    class _Option_field_hidden_color(ColorOption):
        _DESCR = _("Pozadí políčka se skrytou hodnotou.")
        _DEFAULT = '#404040'

    class _Option_field_invalid_color(ColorOption):
        _DESCR = _("Barva pozadí vstupního políčka pokud aktuální hodnota není validní.")
        _DEFAULT = '#ffffc0'
        
    class _Option_filter_color(ColorOption):
        _DESCR = _("Barva záhlaví tabulky při zapnutém filtrování.")
        _DEFAULT = '#82c882'

    # Metody

    def __init__(self):
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
