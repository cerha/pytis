# -*- coding: iso-8859-2 -*-

# Prostředky pro definici a zpracování konfigurace běhu aplikace
# 
# Copyright (C) 2002, 2003, 2004, 2005 Brailcom, o.p.s.
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

from pytis.util import *


class Configuration:
    """Definice konfigurace a její konkrétní parametry."""

    class Option(object):
        """Specifikace konfigurační volby (proměnné).

        Definicí potomka této třídy se jménem začínajícím prefixem '_Option_'
        jako vnitřní třídy třídy 'Configuration' je automaticky definována nová
        konfigurační volba aplikace.  Jméno volby je shodné s částí jména
        takové třídy následující za prefixem '_Option_', její popis je
        v docstringu třídy.  Ostatní vlastnosti volby jsou definovány metodami
        dané třídy.  Konkrétní hodnota je pak udržována v její instanci.

        Docstring tříd nepodléhá obvyklým formátovacím pravidlům.  Měl by mít
        podobu, jež se dobře vyjímá v komentáři pythonového zdrojového souboru.

        Standardní konfigurační volby jsou uvedeny přímo zde.  Aplikace může ve
        svém definičním souboru definovat další, své vlastní, konfigurační
        volby použitím potomka třídy 'Configuration' a doplněním dalších
        vnitřních tříd v něm rozšířit dostupné konfigurační volby.

        Po zpracování konfiguračních voleb je zbývající, nezpracovaná, část
        příkazové řádky přiřazena do 'sys.argv'.

        """
        VISIBLE = 'VISIBLE'
        """Konstanta pro dump, viz 'visible()'."""
        HIDDEN = 'HIDDEN'
        """Konstanta pro dump, viz 'visible()'."""
        COMMENTED_OUT = 'COMMENTED_OUT'
        """Konstanta pro dump, viz 'visible()'."""
        
        def __init__(self, configuration):
            """Inicializuj instanci volby.

            Argumenty:

              configuration -- instance třídy 'Configuration', ve které je
                konfigurační volba přítomna
                
            """
            self._configuration = configuration
            self._value = self._undefined = object()

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

        def init_value(self, force=False):
            """Inicializuj hodnotu proměnné.

            Argumenty:

              force -- právě když je nepravdivé, inicializuj hodnotu jen tehdy,
                je-li ještě nedefinována

            """
            if force or self._value is self._undefined:
                self._value = self._compute_init_value(self._configuration)

        def value(self):
            """Vrať aktuální hodnotu konfigurační volby."""
            # Hodnotu nenastavujeme hned v konstruktoru, protože v té době
            # ještě nemusí být inicializovány jiné volby, na kterých tato volba
            # případně závisí.
            if self._value is self._undefined:
                self.init_value()
            return self._value

        def set_value(self, value):
            """Nastav hodnotu konfigurační volby na 'value'."""
            self._value = value

        def long_option(self):
            """Vrať specifikaci dlouhé volby pro 'getopt' jako string.

            Specifikace může mít například podobu 'debug' nebo 'datadir='.
            Pokud konfigurační volba není spojena s žádnou volbou příkazové
            řádky, vrať 'None'.

            """
            return None

        def environment(self):
            """Vrať tuple jmen proměnných prostředí obsahujících hodnotu volby.

            Jména proměnných jsou strings.  Proměnné prostředí jsou zkoumány
            v uvedeném pořadí a platná je první z nich, která je v prostředí
            přítomna (a to i když je třeba její hodnota prázdná).  Proměnné
            prostředí mají nižší prioritu než volba příkazové řádky nebo
            hodnota v konfiguračním souboru, avšak vyšší prioritu než hodnota
            vrácená metodou 'default'.
            
            """
            return ()

        def default(self):
            """Vrať implicitní hodnotu konfigurační volby.
            
            Hodnota vrácená touto metodou je použita, pokud nebylo možno
            implicitní hodnotu volby zjistit jinak.

            """
            return None
        
        def default_string(self):
            """Vrať implicitní hodnotu konfigurační volby pro dump.

            Hodnota je vrácena jako řetězec, který bude vložen do vzorového
            konfiguračního souboru.  Tuto metodu je užitečné předefinovat
            v případě, že implicitní hodnota volby vrácená metodou 'default()'
            je závislá na konkrétním prostředí a/nebo nevystihuje způsob svého
            získání.

            """
            return `self.default()`
        
        def visible(self):
            """Vrať jednotu z konstant viditelnosti volby.

            Vrácená hodnota určuje, zda ve vzorovém konfiguračním souboru má
            být volba přítomna a v jaké podobě, a může být jedna
            z následujících konstant třídy:

              VISIBLE -- volba bude ve vzorovém konfiguračním souboru uvedena
              HIDDEN -- volba nebude ve vzorovém konfiguračním souboru uvedena
              COMMENTED_OUT -- volba bude ve vzorovém konfiguračním souboru
                uvedena, avšak zakomentovaná

            """
            return self.COMMENTED_OUT

    class _FileOption(Option):
        def _compute_init_value(self, *args, **kwargs):
            value = super(Configuration._FileOption, self).\
                    _compute_init_value(*args, **kwargs)
            if not os.path.isabs(value):
                value = os.path.join(os.getcwd(), value)
            return value
        
    # Volba pro konfiguraci samu

    class _Option_config_file(Option):
        """Umístění konfiguračního souboru."""
        def long_option(self):
            return 'config='
        def environment(self):
            return ('EBASCONFIG',)
        def default(self):
            for filename in ('./config.py', '/etc/pytis/config.py'):
                if os.access(filename, os.F_OK):
                    result = filename
                    break
            else:
                result = None
            return result
        def visible(self):
            return self.HIDDEN

    class _Option_user_config_file(Option):
        """Umístění doplňujícího konfiguračního souboru uživatele.
        Tento soubor, pokud, existuje, je načítán navíc ke standardní
        konfiguraci a v něm definované volby mají vyšší prioritu než volby ve
        standardním konfiguračním souboru.
        Užitečné převážně pro ladění.
        """
        def default(self):
            config_file = self._configuration.config_file
            if config_file:
                dir, file = os.path.split(config_file)
                result = os.path.join(dir, '_'+file)
            else:
                result = None 
            return result
        def visible(self):
            return self.HIDDEN
        
    # Volby užitečné hlavně pro ladění

    class _Option_help(Option):
        """Volba odpovídající --help na příkazové řádce."""
        def long_option(self):
            return 'help'
        def default(self):
            return False
        def visible(self):
            return self.HIDDEN
        
    class _Option_debug(Option):
        """Příznak ladícího režimu.
        Je-li zapnut, aplikace může běžet s více kontrolami a vypisovat
        spoustu informací, obvykle však za cenu svého výrazného zpomalení.
        """
        def long_option(self):
            return 'debug'
        def default(self):
            return False
        def default_string(self):
            return 'False'
        
    class _Option_debug_on_error(Option):
        """Příznak vyvolání debuggeru při chybě.
        Dojde-li k odchycení neočekávané výjimky a tato volba je zapnuta, je
        vyvolán interaktivní debugger.  Je-li zapnuta volba 'debug', je
        implicitně zapnuta i tato volba.  Užitečné pouze pro ladění.
        """
        def long_option(self):
            return 'debug-on-error'
        def default(self):
            return self._configuration.debug
        def default_string(self):
            return 'False'

    class _Option_debug_memory(Option):
        """Příznak výpisu ladících informací o paměti.
        Je-li zapnuta, aplikace vypisuje informativní hlášky garbage collectoru
        a jiné údaje o paměti.
        """
        def long_option(self):
            return 'debug-memory'
        def default(self):
            return False
        def default_string(self):
            return 'False'

    class _Option_bug_report_address(Option):
        """E-mailová adresa, na kterou mají být posílána oznámení o chybě."""
        def default(self):
            return ''

    class _Option_bug_report_subject(Option):
        """Subject mailu oznámení o chybě aplikace."""
        def default(self):
            return 'Bug report: Unexpected exception'

    class _Option_profile(Option):
        """Příznak profilování.
        Je-li zapnut, aplikace se spustí v profilovacím režimu a ukládá
        informace o trvání jednotlivých volání do souboru.  Zapnutí této volby
        velmi výrazně zpomaluje běh aplikace.
        """
        def long_option(self):
            return 'profile'
        def default(self):
            return False
        def default_string(self):
            return 'False'        
        
    class _Option_auto_reload_defs(Option):
        """Příznak automatického přenačítání změněných definičních souborů.
        Je-li zapnut, je zaručeno přenačtení definičních souborů aplikace
        v případě jejich změny.  Někdy to může zpomalovat běh aplikace.
        Implicitně má tato volba stejnou hodnotu jako volba 'debug'.
        """
        def default(self):
            return self._configuration.debug
        def default_string(self):
            return 'False'

    class _Option_test_run_interactive(Option):
        """Příznak určující, zda mají být spouštěny i interaktivní testy.
        Týká se pouze regresivního testování.
        """
        def visible(self):
            return self.HIDDEN

    class _Option_custom_debug(Option):
        """Zvláštní ladící funkce, napojená na příkaz 'COMMAND_CUSTOM_DEBUG'.
        """
        def default(self):
            return (lambda: None)
        def visible(self):
            return self.HIDDEN

    # Cesty a adresáře

    class _Option_def_dir(_FileOption):
        """Adresář obsahující definiční soubory.
        Adresář může být zadán absolutně i relativně vzhledem k aktuálnímu
        adresáři.
        """
        def long_option(self):
            return 'defdir='
        def environment(self):
            return ('EBASDEFDIR',)
        def default(self):
            return './defs'

    class _Option_doc_dir(_FileOption):
        """Adresář obsahující dokumentační soubory.
        Adresář může být zadán absolutně i relativně vzhledem k aktuálnímu
        adresáři.
        """
        def long_option(self):
            return 'docdir='
        def environment(self):
            return ('EBASDOCDIR',)
        def default(self):
            return './docs'

    class _Option_icon_dir(_FileOption):
        """Adresář s obrázkovými soubory.
        Může být zadán absolutně i relativně vzhledem k aktuálnímu adresáři.
        """
        def default(self):
            return '../icons'

    class _Option_tmp_dir(Option):
        """Adresář pro dočasné pomocné soubory.
        """
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
        def default_string(self):
            return "'/tmp'"

    class _Option_server(Option):
        """Jméno stroje, na kterém běží Pyro server, jako string.
        Může být též 'None', pak se klient nepřipojuje na server a používá
        lokální konfiguraci.
        """
        def default(self):
            return None
        def long_option(self):
            return 'server='

    # Databáze
    
    class _Option_dbuser(Option):
        """Uživatelské jméno (login) pro databázové spojení."""
        def long_option(self):
            return 'dbuser='
        def default(self):
            import getpass
            return getpass.getuser()
        def default_string(self):
            return 'getpass.getuser()'
        
    class _Option_dbhost(Option):
        """Jméno databázového serveru."""
        def default(self):
            return 'localhost'
    
    class _Option_dbname(Option):
        """Jméno aplikační databáze."""
        def default(self):
            return 'pytis'

    class _Option_dbconnection(Option):
        """Instance specifikace spojení do databáze ('pytis.data.DBConnection').
        Implicitně se vytváří z výše uvedených databázových voleb.
        """
        def default(self):
            import pytis.data
            c = self._configuration
            return pytis.data.DBConnection(user=c.dbuser, host=c.dbhost,
                                         database=c.dbname)
        def visible(self):
            return self.HIDDEN

    class _Option_dblogtable(Option):
        """Jméno tabulky, do které mají být logovány DML SQL příkazy."""
        def default(self):
            return ''

    class _Option_dblisten(Option):
        """Flag určující, zda má být spouštěn dohlížeč změn dat."""
        def default(self):
            return True
        def default_string(self):
            return 'True'

    # Logovací volby

    class _Option_log_logger(Option):
        """Specifikace logovací třídy.
        Trojice (CLASS, ARGS, KWARGS), kde CLASS je logovací třída a ARGS,
        resp. KWARGS, jsou argumenty, resp. klíčované argumenty, jejího
        konstruktoru.  Standardní dostupné třídy jsou SyslogLogger a
        StreamLogger.  Více o nich lze nalézt v jejich dokumentaci.
        """
        def default(self):
            import log
            return (log.StreamLogger, (sys.stderr,), {})
        def default_string(self):
            return '(log.StreamLogger, (sys.stderr,), {})'

    class _Option_log_exclude(Option):
        """Seznam typů logovacích hlášek, které mají být odfiltrovány.
        V seznamu lze použít konstanty OPERATIONAL, ACTION, EVENT a DEBUG.
        """
        def default(self):
            if self._configuration.debug:
                return []
            else:
                import log
                return [log.DEBUG]
        def default_string(self):
            return '[DEBUG]'

    class _Option_log_one_line_preferred(Option):
        """Určuje, zda je preferováno stručné nebo jednotné formátování.
        Je-li tato volba nastavena na pravdu, jsou krátká data v logovacích
        hláškách doporučujících stručnost připojena ihned za hlášku místo
        vypsání na samostatný řádek.
        """
        def default(self):
            return True
        def default_string(self):
            return 'True'

    class _Option_log_module_filter(Option):
        """Prefix jména modulu, jehož debugovací hlášky jsou propuštěny.
        Debugovací logovací hlášky modulů s jiným prefixem jsou odfiltrovány.
        Není-li definováno, jsou propuštěny všechny hlášky (nestanoví-li jiný
        filtr jinak).
        Užitečné pouze pro ladění.
        """
        def default(self):
            return ''
        def default_string(self):
            return "'pytis.data'"

    class _Option_log_class_filter(Option):
        """Sekvence jmen tříd, jejichž debugovací hlášky jsou propuštěny.
        Debugovací logovací hlášky ostatních tříd jsou odfiltrovány.
        Je-li 'None', jsou propuštěny všechny hlášky (nestanoví-li jiný
        filtr jinak).
        Užitečné pouze pro ladění.
        """
        def default(self):
            return None
        def default_string(self):
            return "('pytis.data.DBDefaultClass',)"
            
    # Externí programy

    class _Option_printing_command(Option):
        """Shellový příkaz pro provedení tisku, včetně argumentů.
        Příkaz musí být schopen převzít tisková data ze standardního vstupu.
        """
        def default(self):
            return 'lpr'

    class _Option_sendmail_command(Option):
        """Shellový příkaz sendmail včetně celé cesty."""
        def default(self):
            return '/usr/lib/sendmail'
        
    # Ostatní konfigurační volby

    class _Option_application_name(Option):
        """Jméno aplikace.
        Jméno může být libovolné, používá se pouze ve věcech jako titulky oken
        nebo logování.
        """
        def default(self):
            return 'Pytis'

    class _Option_date_time_format(Option):
        """Formát společně uvedeného data a času.
        Formát musí být string a musí být ve tvaru vyžadovaném parametrem
        `format' konstruktoru třídy 'pytis.data.DateTime'.
        """
        def default(self):
            import pytis.data
            return pytis.data.DateTime.DEFAULT_FORMAT

    class _Option_date_format(Option):
        """Formát data.
        Formát musí být string a musí být ve tvaru vyžadovaném parametrem
        `format' konstruktoru třídy 'pytis.data.Date'.
        """
        def default(self):
            import pytis.data
            return pytis.data.Date.DEFAULT_FORMAT

    class _Option_time_format(Option):
        """Formát času.
        Formát musí být string a musí být ve tvaru vyžadovaném parametrem
        `format' konstruktoru třídy 'pytis.data.Time'.
        """
        def default(self):
            import pytis.data
            return pytis.data.Time.DEFAULT_FORMAT

    class _Option_lc_numeric(Option):
        """Numeric locale.
        Hodnota musí být string reprezentující locale pro formátování číselných
        položek. 
        """
        def default(self):
            return 'C'

    class _Option_export_directory(Option):
        """Adresář pro export textových souborů.
        Hodnota musí být řetězec udávající cestu k adresáři, kde se budou
        ukládat textové CSV soubory. 
        """
        def default(self):
            return '/tmp'

    class _Option_export_encoding(Option):
        """Kódování exportovaných řetězců
        Hodnota musí být jedním z podporovaných kódování pro metodu
        encode() unicodových řetězců v Pythonu. 
        """
        def default(self):
            return 'iso8859-2'

    class _Option_db_encoding(Option):
        """Interní kódování databáze
        Hodnota musí být jedním z podporovaných kódování pro metodu
        encode() unicodových řetězců v Pythonu. 
        """
        def default(self):
            return 'utf-8'

    class _Option_cache_size(Option):
        """Velikost cache pro řádky datového objektu. Velikost je integer,
        který udává počet řádků cache.
        """
        def default(self):
            return 20000

    class _Option_initial_fetch_size(Option):
        """Počet řádků, které se přednačtou do cache při prvním selectu
        z datového objektu.
        """
        def default(self):
            return 100

    class _Option_fetch_size(Option):
        """Počet řádků, které se přinačítají do cache při dalších selectech
        z datového objektu.
        """
        def default(self):
            return 100

    # Volby přizpůsobení uživatelského rozhraní
        
    class _Option_show_tooltips(Option):
        """Příznak zobrazování bublinové nápovědy."""
        def default(self):
            return True
        def default_string(self):
            return 'True'
        
    class _Option_show_splash(Option):
        """Příznak zobrazování úvodního uvítacího dialogu."""
        def default(self):
            return True
        def default_string(self):
            return 'True'
        
    class _Option_cache_spec_onstart(Option):
        """Příznak cachování specifikací při startu aplikace."""
        def default(self):
            return True
        def default_string(self):
            return 'True'

    class _Option_startup_forms(Option):
        """Seznam formulářů, které mají být otevřeny po spuštění aplikace."""
        def long_option(self):
            return 'startup-forms='
        def default(self):
            return None

    class _Option_row_focus_fg_color(Option):
        """Barva textu aktivního řádku tabulkového formuláře.
        Barva je dána řetězcem '#RRGGBB'.
        """
        def default(self):
            return '#ffffff'
        
    class _Option_row_focus_bg_color(Option):
        """Barva pozadí aktivního řádku tabulkového formuláře.
        Barva je dána řetězcem '#RRGGBB'.
        Pokud je None, bude použita systémová barva zvýraznění.
        """
        def default(self):
            return None
            
        
    class _Option_row_nofocus_fg_color(Option):
        """Barva textu neaktivního řádku tabulkového formuláře.
        Barva je dána řetězcem '#RRGGBB'.
        """
        def default(self):
            return '#000000'
        
    class _Option_row_nofocus_bg_color(Option):
        """Barva pozadí neaktivního řádku tabulkového formuláře.
        Barva je dána řetězcem '#RRGGBB'.
        """
        def default(self):
            return '#b6b6b6'
        
    class _Option_row_edit_fg_color(Option):
        """Barva textu editovaného řádku tabulkového formuláře.
        Barva je dána řetězcem '#RRGGBB'.
        """
        def default(self):
            return '#ffffff'

    class _Option_row_edit_bg_color(Option):
        """Barva pozadí editovaného řádku.
        Barva je dána řetězcem '#RRGGBB'.
        """
        def default(self):
            return '#c80000'

    class _Option_cell_highlight_color(Option):
        """Barva zvýraznění aktivní buňky tabulkového formuláře.
        Barva je dána řetězcem '#RRGGBB'.
        """
        def default(self):
            return '#ffa000'

    class _Option_grid_line_color(Option):
        """Barva mřížky tabulkového formuláře.
        Barva je dána řetězcem '#RRGGBB'.
        """
        def default(self):
            return '#6482be'

    class _Option_field_disabled_color(Option):
        """Barva pozadí needitovatelného vstupního políčka.
        Barva je dána řetězcem '#RRGGBB'.
        """
        def default(self):
            return '#c0c0c0'

    class _Option_field_inaccessible_color(Option):
        """Barva pozadí políčka needitovatelného kvůli přístupovým právům.
        Barva je dána řetězcem '#RRGGBB'.
        """
        def default(self):
            return '#e0e4f0'

    class _Option_filter_color(Option):
        """Barva záhlaví tabulkového formuláře při zapnutém filtrování.
        Barva je dána řetězcem '#RRGGBB'.
        """
        def default(self):
            return '#82c882'

    # Metody

    def __init__(self, command_line=None):
        """Inicializuj konfiguraci.

        Argumenty:

          command_line -- volby příkazové řádky jako sekvence strings; může být
            též 'None', pak je použito 'sys.argv'

        """
        PREFIX = '_Option_'
        options = {}
        for k, v in self.__class__.__dict__.items():
            if starts_with(k, PREFIX):
                name = k[len(PREFIX):]
                options[name] = v(self)
        self.__dict__['_options'] = options
        if command_line is None:
            command_line = sys.argv
        if command_line[0] == 'pytis':
            command_line_options = \
              self._parse_command_line_options(command_line)
        else:
            command_line_options = {}
        self.__dict__['command_line_options'] = command_line_options
        for o in ('config_file', 'user_config_file'):
            opt = options[o]
            opt.init_value()
            self.__dict__['_' + o] = opt.value()
        self._read_configuration()
        for o in options.values():
            o.init_value(force=True)

    def _parse_command_line_options(self, command_line):
        command_line_options = {}
        long_options = filter(identity,
                              map(lambda o: o.long_option(),
                                  self._options.values()))
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
        if conffile is None:
            return
        self.__dict__['_config_mtime'] = \
          self._read_configuration_file(conffile)
        uconffile = self._user_config_file
        if uconffile is None:
            return
        self.__dict__['_user_config_mtime'] = \
          self._read_configuration_file(uconffile, force=False)

    def _read_configuration_file(self, filename, force=True):
        try:
            filetime = os.stat(filename)[stat.ST_MTIME]
        except:
            if force:
                raise Exception(_("Konfigurační soubor je nepřístupný"),
                                filename)
            else:
                return 2**30
        try:
            f = open(filename)
        except:
            raise Exception(_("Nebylo lze otevřít konfigurační soubor"),
                            filename)
        try:
            confmodule = imp.load_module('config', f, filename,
                                         ('.py','r',imp.PY_SOURCE))
        finally:
            f.close()
        options = self._options
        cloptions = self.command_line_options
        for o in dir(confmodule):
            if options.has_key(o):
                opt = options[o]
                if not cloptions.has_key(opt.long_option()):
                    value = confmodule.__dict__[o]
                    opt.set_value(value)
        return filetime

    def __getattr__(self, name):
        """Vrať konfigurační volbu 'name'.

        'name' musí být string odpovídající jménu existující konfigurační
        volby.  Pokud taková konfigurační volba neexistuje, vyvolej výjimku
        'AttributeError'.

        """
        if __debug__ and self._config_file and \
               name not in ('config_file', 'user_config_file'):
            now = time.time()
            if now > self._config_mtime or now > self._user_config_mtime:
                t = os.stat(self._config_file)[stat.ST_MTIME]
                try:
                    ut = os.stat(self._user_config_file)[stat.ST_MTIME]
                except:
                    ut = 0
                if t > self._config_mtime or ut > self._user_config_mtime:
                    self._read_configuration()
        try:
            return self._options[name].value()
        except KeyError:
            raise AttributeError(name)

    def __setattr__(self, name, value):
        """Nastav atribut nebo konfigurační volbu 'name' na 'value'.

        Pokud takový atribut ani konfigurační volba neexistuje, vyvolej výjimku
        'AttributeError'.
        
        """
        if self.__dict__['_options'].has_key(name):
            self.__dict__['_options'][name].set_value(value)
        elif hasattr(self, name):
            self.__dict__[name] = name
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
            if options.has_key(o) and dict[o] != None:
                opt = options[o]
                if override_cmdline or not clopt.has_key(opt.long_option()):
                    opt.set_value(dict[o])

    def serial_number(self):
        """Vrať sériové číslo aktuální konfigurace.

        Sériové číslo je zvýšeno při každé změně konfigurace.  Číslo může být
        zvýšeno o libovolný kladný přírůstek.

        Pomocí sériového čísla lze zjišťovat, zda došlo ke změně konfigurace od
        posledního ověření.
        
        """
        return self._config_mtime
    
    def dump_config_template(self, stream):
        """Zapiš vzorový konfigurační soubor do 'stream'.

        'stream' musí být otevřený stream s možností zápisu.

        """
        stream.write('# -*- coding: iso-8859-2 -*-\n\n')
        for name, option in self._options.items():
            visibility = option.visible()
            if visibility != self.Option.HIDDEN:
                for line in string.split(option.__doc__, '\n'):
                    stream.write('# %s\n' % string.strip(line))
                if visibility == self.Option.COMMENTED_OUT:
                    stream.write('#')
                stream.write('%s = %s\n' % (name, option.default_string()))
                stream.write('\n')

    def print_options(self):
        """Vypiš na standardní výstup všechny volby a jejich hodnoty."""
        options = self._options
        keys = options.keys()
        keys.sort()
        for k in keys:
            sys.stdout.write('%s = %s\n' % (k, `options[k].value()`))

class ConfigDB:
    """Konfigurace uložená v datovém objektu s rozhraním slovníku."""

    def __init__(self, resolver, name, *args, **kwargs):
        """Inicializuj instanci.

        Argumenty:

          resolver -- resolver specifikací (instance 'pytis.util.Resolver').
          name -- jméno specifikace datového objektu, ze kterého má být
            konfigurace načtena.
          args, kwargs -- argumenty pro vytvoření datového objektu, které budou
            předány metodě 'pytis.data.Data.create()'.

        """
        data_spec = resolver.get(name, 'data_spec')
        self._data = data_spec.create(*args, **kwargs)
        self._data.select()
        self._row = self._data.fetchone()
        self._data.close()

    def __getitem__(self, key):
        return self._row[key].value()

    def __setitem__(self, key, value):
        self._row[key] = value
        self._data.update(self._row[self._data.key()[0].id()], self._row)

    def keys(self):
        return self._row.keys()
