# -*- coding: iso-8859-2 -*-

# Prost�edky pro definici a zpracov�n� konfigurace b�hu aplikace
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

from pytis.util import *


class Configuration:
    """Definice konfigurace a jej� konkr�tn� parametry."""

    class Option(object):
        """Specifikace konfigura�n� volby (prom�nn�).

        Definic� potomka t�to t��dy se jm�nem za��naj�c�m prefixem '_Option_'
        jako vnit�n� t��dy t��dy 'Configuration' je automaticky definov�na nov�
        konfigura�n� volba aplikace.  Jm�no volby je shodn� s���st� jm�na
        takov� t��dy n�sleduj�c� za prefixem '_Option_', jej� popis je
        v�docstringu t��dy.  Ostatn� vlastnosti volby jsou definov�ny metodami
        dan� t��dy.  Konkr�tn� hodnota je pak udr�ov�na v�jej� instanci.

        Docstring t��d nepodl�h� obvykl�m form�tovac�m pravidl�m.  M�l by m�t
        podobu, je� se dob�e vyj�m� v�koment��i pythonov�ho zdrojov�ho souboru.

        Standardn� konfigura�n� volby jsou uvedeny p��mo zde.  Aplikace m��e ve
        sv�m defini�n�m souboru definovat dal��, sv� vlastn�, konfigura�n�
        volby pou�it�m potomka t��dy 'Configuration' a dopln�n�m dal��ch
        vnit�n�ch t��d v�n�m roz���it dostupn� konfigura�n� volby.

        Po zpracov�n� konfigura�n�ch voleb je zb�vaj�c�, nezpracovan�, ��st
        p��kazov� ��dky p�i�azena do 'sys.argv'.

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

              configuration -- instance t��dy 'Configuration', ve kter� je
                konfigura�n� volba p��tomna
                
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
            """Inicializuj hodnotu prom�nn�.

            Argumenty:

              force -- pr�v� kdy� je nepravdiv�, inicializuj hodnotu jen tehdy,
                je-li je�t� nedefinov�na

            """
            if force or self._value is self._undefined:
                self._value = self._compute_init_value(self._configuration)

        def value(self):
            """Vra� aktu�ln� hodnotu konfigura�n� volby."""
            # Hodnotu nenastavujeme hned�v�konstruktoru, proto�e v�t� dob�
            # je�t� nemus� b�t inicializov�ny jin� volby, na kter�ch tato volba
            # p��padn� z�vis�.
            if self._value is self._undefined:
                self.init_value()
            return self._value

        def set_value(self, value):
            """Nastav hodnotu konfigura�n� volby na 'value'."""
            self._value = value

        def long_option(self):
            """Vra� specifikaci dlouh� volby pro 'getopt' jako string.

            Specifikace m��e m�t nap��klad podobu 'debug' nebo 'datadir='.
            Pokud konfigura�n� volba nen� spojena s���dnou volbou p��kazov�
            ��dky, vra� 'None'.

            """
            return None

        def environment(self):
            """Vra� tuple jmen prom�nn�ch prost�ed� obsahuj�c�ch hodnotu volby.

            Jm�na prom�nn�ch jsou strings.  Prom�nn� prost�ed� jsou zkoum�ny
            v�uveden�m po�ad� a platn� je prvn� z�nich, kter� je v�prost�ed�
            p��tomna (a�to i�kdy� je t�eba jej� hodnota pr�zdn�).  Prom�nn�
            prost�ed� maj� ni��� prioritu ne� volba p��kazov� ��dky nebo
            hodnota v�konfigura�n�m souboru, av�ak vy��� prioritu ne� hodnota
            vr�cen� metodou 'default'.
            
            """
            return ()

        def default(self):
            """Vra� implicitn� hodnotu konfigura�n� volby.
            
            Hodnota vr�cen� touto metodou je pou�ita, pokud nebylo mo�no
            implicitn� hodnotu volby zjistit jinak.

            """
            return None
        
        def default_string(self):
            """Vra� implicitn� hodnotu konfigura�n� volby pro dump.

            Hodnota je vr�cena jako �et�zec, kter� bude vlo�en do vzorov�ho
            konfigura�n�ho souboru.  Tuto metodu je u�ite�n� p�edefinovat
            v�p��pad�, �e implicitn� hodnota volby vr�cen� metodou 'default()'
            je z�visl� na konkr�tn�m prost�ed� a/nebo nevystihuje zp�sob sv�ho
            z�sk�n�.

            """
            return `self.default()`
        
        def visible(self):
            """Vra� jednotu z�konstant viditelnosti volby.

            Vr�cen� hodnota ur�uje, zda ve vzorov�m konfigura�n�m souboru m�
            b�t volba p��tomna a v�jak� podob�, a m��e b�t jedna
            z�n�sleduj�c�ch konstant t��dy:

              VISIBLE -- volba bude ve vzorov�m konfigura�n�m souboru uvedena
              HIDDEN -- volba nebude ve vzorov�m konfigura�n�m souboru uvedena
              COMMENTED_OUT -- volba bude ve vzorov�m konfigura�n�m souboru
                uvedena, av�ak zakomentovan�

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
        """Um�st�n� konfigura�n�ho souboru."""
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
        """Um�st�n� dopl�uj�c�ho konfigura�n�ho souboru u�ivatele.
        Tento soubor, pokud, existuje, je na��t�n nav�c ke standardn�
        konfiguraci a v�n�m definovan� volby maj� vy��� prioritu ne� volby ve
        standardn�m konfigura�n�m souboru.
        U�ite�n� p�ev�n� pro lad�n�.
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
        
    # Volby u�ite�n� hlavn� pro lad�n�

    class _Option_help(Option):
        """Volba odpov�daj�c� --help na p��kazov� ��dce."""
        def long_option(self):
            return 'help'
        def default(self):
            return False
        def visible(self):
            return self.HIDDEN
        
    class _Option_debug(Option):
        """P��znak lad�c�ho re�imu.
        Je-li zapnut, aplikace m��e b�et s�v�ce kontrolami a vypisovat
        spoustu informac�, obvykle v�ak za cenu sv�ho v�razn�ho zpomalen�.
        """
        def long_option(self):
            return 'debug'
        def default(self):
            return False
        def default_string(self):
            return 'False'
        
    class _Option_debug_on_error(Option):
        """P��znak vyvol�n� debuggeru p�i chyb�.
        Dojde-li k�odchycen� neo�ek�van� v�jimky a tato volba je zapnuta, je
        vyvol�n interaktivn� debugger.  Je-li zapnuta volba 'debug', je
        implicitn� zapnuta i�tato volba.  U�ite�n� pouze pro lad�n�.
        """
        def long_option(self):
            return 'debug-on-error'
        def default(self):
            return self._configuration.debug
        def default_string(self):
            return 'False'

    class _Option_debug_memory(Option):
        """P��znak v�pisu lad�c�ch informac� o�pam�ti.
        Je-li zapnuta, aplikace vypisuje informativn� hl�ky garbage collectoru
        a jin� �daje o�pam�ti.
        """
        def long_option(self):
            return 'debug-memory'
        def default(self):
            return False
        def default_string(self):
            return 'False'

    class _Option_bug_report_address(Option):
        """E-mailov� adresa, na kterou maj� b�t pos�l�na ozn�men� o�chyb�."""
        def default(self):
            return ''

    class _Option_bug_report_subject(Option):
        """Subject mailu ozn�men� o�chyb� aplikace."""
        def default(self):
            return 'Bug report: Unexpected exception'

    class _Option_profile(Option):
        """P��znak profilov�n�.
        Je-li zapnut, aplikace se spust� v�profilovac�m re�imu a ukl�d�
        informace o�trv�n� jednotliv�ch vol�n� do souboru.  Zapnut� t�to volby
        velmi v�razn� zpomaluje b�h aplikace.
        """
        def long_option(self):
            return 'profile'
        def default(self):
            return False
        def default_string(self):
            return 'False'        
        
    class _Option_auto_reload_defs(Option):
        """P��znak automatick�ho p�ena��t�n� zm�n�n�ch defini�n�ch soubor�.
        Je-li zapnut, je zaru�eno p�ena�ten� defini�n�ch soubor� aplikace
        v�p��pad� jejich zm�ny.  N�kdy to m��e zpomalovat b�h aplikace.
        Implicitn� m� tato volba stejnou hodnotu jako volba 'debug'.
        """
        def default(self):
            return self._configuration.debug
        def default_string(self):
            return 'False'

    class _Option_test_run_interactive(Option):
        """P��znak ur�uj�c�, zda maj� b�t spou�t�ny i�interaktivn� testy.
        T�k� se pouze regresivn�ho testov�n�.
        """
        def visible(self):
            return self.HIDDEN

    class _Option_custom_debug(Option):
        """Zvl�tn� lad�c� funkce, napojen� na p��kaz 'COMMAND_CUSTOM_DEBUG'.
        """
        def default(self):
            return (lambda: None)
        def visible(self):
            return self.HIDDEN

    # Cesty a adres��e

    class _Option_def_dir(_FileOption):
        """Adres�� obsahuj�c� defini�n� soubory.
        Adres�� m��e b�t zad�n absolutn� i�relativn� vzhledem k�aktu�ln�mu
        adres��i.
        """
        def long_option(self):
            return 'defdir='
        def environment(self):
            return ('EBASDEFDIR',)
        def default(self):
            return './defs'

    class _Option_doc_dir(_FileOption):
        """Adres�� obsahuj�c� dokumenta�n� soubory.
        Adres�� m��e b�t zad�n absolutn� i�relativn� vzhledem k�aktu�ln�mu
        adres��i.
        """
        def long_option(self):
            return 'docdir='
        def environment(self):
            return ('EBASDOCDIR',)
        def default(self):
            return './docs'

    class _Option_icon_dir(_FileOption):
        """Adres�� s�obr�zkov�mi soubory.
        M��e b�t zad�n absolutn� i�relativn� vzhledem k�aktu�ln�mu adres��i.
        """
        def default(self):
            return '../icons'

    class _Option_tmp_dir(Option):
        """Adres�� pro do�asn� pomocn� soubory.
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
        """Jm�no stroje, na kter�m b�� Pyro server, jako string.
        M��e b�t t� 'None', pak se klient nep�ipojuje na server a pou��v�
        lok�ln� konfiguraci.
        """
        def default(self):
            return None
        def long_option(self):
            return 'server='

    # Datab�ze
    
    class _Option_dbuser(Option):
        """U�ivatelsk� jm�no (login) pro datab�zov� spojen�."""
        def long_option(self):
            return 'dbuser='
        def default(self):
            import getpass
            return getpass.getuser()
        def default_string(self):
            return 'getpass.getuser()'
        
    class _Option_dbhost(Option):
        """Jm�no datab�zov�ho serveru."""
        def default(self):
            return 'localhost'
    
    class _Option_dbname(Option):
        """Jm�no aplika�n� datab�ze."""
        def default(self):
            return 'pytis'

    class _Option_dbconnection(Option):
        """Instance specifikace spojen� do datab�ze ('pytis.data.DBConnection').
        Implicitn� se vytv��� z�v��e uveden�ch datab�zov�ch voleb.
        """
        def default(self):
            import pytis.data
            c = self._configuration
            return pytis.data.DBConnection(user=c.dbuser, host=c.dbhost,
                                         database=c.dbname)
        def visible(self):
            return self.HIDDEN

    class _Option_dblogtable(Option):
        """Jm�no tabulky, do kter� maj� b�t logov�ny DML SQL p��kazy."""
        def default(self):
            return ''

    class _Option_dblisten(Option):
        """Flag ur�uj�c�, zda m� b�t spou�t�n dohl�e� zm�n dat."""
        def default(self):
            return True
        def default_string(self):
            return 'True'

    # Logovac� volby

    class _Option_log_logger(Option):
        """Specifikace logovac� t��dy.
        Trojice (CLASS, ARGS, KWARGS), kde CLASS je logovac� t��da a ARGS,
        resp. KWARGS, jsou argumenty, resp. kl��ovan� argumenty, jej�ho
        konstruktoru.  Standardn� dostupn� t��dy jsou SyslogLogger a
        StreamLogger.  V�ce o�nich lze nal�zt v�jejich dokumentaci.
        """
        def default(self):
            import log
            return (log.StreamLogger, (sys.stderr,), {})
        def default_string(self):
            return '(log.StreamLogger, (sys.stderr,), {})'

    class _Option_log_exclude(Option):
        """Seznam typ� logovac�ch hl�ek, kter� maj� b�t odfiltrov�ny.
        V�seznamu lze pou��t konstanty OPERATIONAL, ACTION, EVENT a DEBUG.
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
        """Ur�uje, zda je preferov�no stru�n� nebo jednotn� form�tov�n�.
        Je-li tato volba nastavena na pravdu, jsou kr�tk� data v�logovac�ch
        hl�k�ch doporu�uj�c�ch stru�nost p�ipojena ihned za hl�ku m�sto
        vyps�n� na samostatn� ��dek.
        """
        def default(self):
            return True
        def default_string(self):
            return 'True'

    class _Option_log_module_filter(Option):
        """Prefix jm�na modulu, jeho� debugovac� hl�ky jsou propu�t�ny.
        Debugovac� logovac� hl�ky modul� s�jin�m prefixem jsou odfiltrov�ny.
        Nen�-li definov�no, jsou propu�t�ny v�echny hl�ky (nestanov�-li jin�
        filtr jinak).
        U�ite�n� pouze pro lad�n�.
        """
        def default(self):
            return ''
        def default_string(self):
            return "'pytis.data'"

    class _Option_log_class_filter(Option):
        """Sekvence jmen t��d, jejich� debugovac� hl�ky jsou propu�t�ny.
        Debugovac� logovac� hl�ky ostatn�ch t��d jsou odfiltrov�ny.
        Je-li 'None', jsou propu�t�ny v�echny hl�ky (nestanov�-li jin�
        filtr jinak).
        U�ite�n� pouze pro lad�n�.
        """
        def default(self):
            return None
        def default_string(self):
            return "('pytis.data.DBDefaultClass',)"
            
    # Extern� programy

    class _Option_printing_command(Option):
        """Shellov� p��kaz pro proveden� tisku, v�etn� argument�.
        P��kaz mus� b�t schopen p�evz�t tiskov� data ze standardn�ho vstupu.
        """
        def default(self):
            return 'lpr'

    class _Option_sendmail_command(Option):
        """Shellov� p��kaz sendmail v�etn� cel� cesty."""
        def default(self):
            return '/usr/lib/sendmail'
        
    # Ostatn� konfigura�n� volby

    class _Option_application_name(Option):
        """Jm�no aplikace.
        Jm�no m��e b�t libovoln�, pou��v� se pouze ve v�cech jako titulky oken
        nebo logov�n�.
        """
        def default(self):
            return 'Pytis'

    class _Option_date_time_format(Option):
        """Form�t spole�n� uveden�ho data a �asu.
        Form�t mus� b�t string a mus� b�t ve tvaru vy�adovan�m parametrem
        `format' konstruktoru t��dy 'pytis.data.DateTime'.
        """
        def default(self):
            import pytis.data
            return pytis.data.DateTime.DEFAULT_FORMAT

    class _Option_date_format(Option):
        """Form�t data.
        Form�t mus� b�t string a mus� b�t ve tvaru vy�adovan�m parametrem
        `format' konstruktoru t��dy 'pytis.data.Date'.
        """
        def default(self):
            import pytis.data
            return pytis.data.Date.DEFAULT_FORMAT

    class _Option_time_format(Option):
        """Form�t �asu.
        Form�t mus� b�t string a mus� b�t ve tvaru vy�adovan�m parametrem
        `format' konstruktoru t��dy 'pytis.data.Time'.
        """
        def default(self):
            import pytis.data
            return pytis.data.Time.DEFAULT_FORMAT

    class _Option_lc_numeric(Option):
        """Numeric locale.
        Hodnota mus� b�t string reprezentuj�c� locale pro form�tov�n� ��seln�ch
        polo�ek. 
        """
        def default(self):
            return 'C'

    class _Option_export_directory(Option):
        """Adres�� pro export textov�ch soubor�.
        Hodnota mus� b�t �et�zec ud�vaj�c� cestu k adres��i, kde se budou
        ukl�dat textov� CSV soubory. 
        """
        def default(self):
            return '/tmp'

    class _Option_export_encoding(Option):
        """K�dov�n� exportovan�ch �et�zc�
        Hodnota mus� b�t jedn�m z podporovan�ch k�dov�n� pro metodu
        encode() unicodov�ch �et�zc� v Pythonu. 
        """
        def default(self):
            return 'iso8859-2'

    class _Option_db_encoding(Option):
        """Intern� k�dov�n� datab�ze
        Hodnota mus� b�t jedn�m z podporovan�ch k�dov�n� pro metodu
        encode() unicodov�ch �et�zc� v Pythonu. 
        """
        def default(self):
            return 'utf-8'

    class _Option_cache_size(Option):
        """Velikost cache pro ��dky datov�ho objektu. Velikost je integer,
        kter� ud�v� po�et ��dk� cache.
        """
        def default(self):
            return 20000

    class _Option_initial_fetch_size(Option):
        """Po�et ��dk�, kter� se p�edna�tou do cache p�i prvn�m selectu
        z datov�ho objektu.
        """
        def default(self):
            return 100

    class _Option_fetch_size(Option):
        """Po�et ��dk�, kter� se p�ina��taj� do cache p�i dal��ch selectech
        z datov�ho objektu.
        """
        def default(self):
            return 100

    # Volby p�izp�soben� u�ivatelsk�ho rozhran�
        
    class _Option_show_tooltips(Option):
        """P��znak zobrazov�n� bublinov� n�pov�dy."""
        def default(self):
            return True
        def default_string(self):
            return 'True'
        
    class _Option_show_splash(Option):
        """P��znak zobrazov�n� �vodn�ho uv�tac�ho dialogu."""
        def default(self):
            return True
        def default_string(self):
            return 'True'
        
    class _Option_cache_spec_onstart(Option):
        """P��znak cachov�n� specifikac� p�i startu aplikace."""
        def default(self):
            return True
        def default_string(self):
            return 'True'

    class _Option_startup_forms(Option):
        """Seznam formul���, kter� maj� b�t otev�eny po spu�t�n� aplikace."""
        def long_option(self):
            return 'startup-forms='
        def default(self):
            return None

    class _Option_row_focus_fg_color(Option):
        """Barva textu aktivn�ho ��dku tabulkov�ho formul��e.
        Barva je d�na �et�zcem '#RRGGBB'.
        """
        def default(self):
            return '#ffffff'
        
    class _Option_row_focus_bg_color(Option):
        """Barva pozad� aktivn�ho ��dku tabulkov�ho formul��e.
        Barva je d�na �et�zcem '#RRGGBB'.
        Pokud je None, bude pou�ita syst�mov� barva zv�razn�n�.
        """
        def default(self):
            return None
            
        
    class _Option_row_nofocus_fg_color(Option):
        """Barva textu neaktivn�ho ��dku tabulkov�ho formul��e.
        Barva je d�na �et�zcem '#RRGGBB'.
        """
        def default(self):
            return '#000000'
        
    class _Option_row_nofocus_bg_color(Option):
        """Barva pozad� neaktivn�ho ��dku tabulkov�ho formul��e.
        Barva je d�na �et�zcem '#RRGGBB'.
        """
        def default(self):
            return '#b6b6b6'
        
    class _Option_row_edit_fg_color(Option):
        """Barva textu editovan�ho ��dku tabulkov�ho formul��e.
        Barva je d�na �et�zcem '#RRGGBB'.
        """
        def default(self):
            return '#ffffff'

    class _Option_row_edit_bg_color(Option):
        """Barva pozad� editovan�ho ��dku.
        Barva je d�na �et�zcem '#RRGGBB'.
        """
        def default(self):
            return '#c80000'

    class _Option_cell_highlight_color(Option):
        """Barva zv�razn�n� aktivn� bu�ky tabulkov�ho formul��e.
        Barva je d�na �et�zcem '#RRGGBB'.
        """
        def default(self):
            return '#ffa000'

    class _Option_grid_line_color(Option):
        """Barva m��ky tabulkov�ho formul��e.
        Barva je d�na �et�zcem '#RRGGBB'.
        """
        def default(self):
            return '#6482be'

    class _Option_field_disabled_color(Option):
        """Barva pozad� needitovateln�ho vstupn�ho pol��ka.
        Barva je d�na �et�zcem '#RRGGBB'.
        """
        def default(self):
            return '#c0c0c0'

    class _Option_field_inaccessible_color(Option):
        """Barva pozad� pol��ka needitovateln�ho kv�li p��stupov�m pr�v�m.
        Barva je d�na �et�zcem '#RRGGBB'.
        """
        def default(self):
            return '#e0e4f0'

    class _Option_filter_color(Option):
        """Barva z�hlav� tabulkov�ho formul��e p�i zapnut�m filtrov�n�.
        Barva je d�na �et�zcem '#RRGGBB'.
        """
        def default(self):
            return '#82c882'

    # Metody

    def __init__(self, command_line=None):
        """Inicializuj konfiguraci.

        Argumenty:

          command_line -- volby p��kazov� ��dky jako sekvence strings; m��e b�t
            t� 'None', pak je pou�ito 'sys.argv'

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
                raise Exception(_("Konfigura�n� soubor je nep��stupn�"),
                                filename)
            else:
                return 2**30
        try:
            f = open(filename)
        except:
            raise Exception(_("Nebylo lze otev��t konfigura�n� soubor"),
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
        """Vra� konfigura�n� volbu 'name'.

        'name' mus� b�t string odpov�daj�c� jm�nu existuj�c� konfigura�n�
        volby.  Pokud takov� konfigura�n� volba neexistuje, vyvolej v�jimku
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
        """Nastav atribut nebo konfigura�n� volbu 'name' na 'value'.

        Pokud takov� atribut ani konfigura�n� volba neexistuje, vyvolej v�jimku
        'AttributeError'.
        
        """
        if self.__dict__['_options'].has_key(name):
            self.__dict__['_options'][name].set_value(value)
        elif hasattr(self, name):
            self.__dict__[name] = name
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
            if options.has_key(o) and dict[o] != None:
                opt = options[o]
                if override_cmdline or not clopt.has_key(opt.long_option()):
                    opt.set_value(dict[o])

    def serial_number(self):
        """Vra� s�riov� ��slo aktu�ln� konfigurace.

        S�riov� ��slo je zv��eno p�i ka�d� zm�n� konfigurace.  ��slo m��e b�t
        zv��eno o�libovoln� kladn� p��r�stek.

        Pomoc� s�riov�ho ��sla lze zji��ovat, zda do�lo ke zm�n� konfigurace od
        posledn�ho ov��en�.
        
        """
        return self._config_mtime
    
    def dump_config_template(self, stream):
        """Zapi� vzorov� konfigura�n� soubor do 'stream'.

        'stream' mus� b�t otev�en� stream s�mo�nost� z�pisu.

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
        """Vypi� na standardn� v�stup v�echny volby a jejich hodnoty."""
        options = self._options
        keys = options.keys()
        keys.sort()
        for k in keys:
            sys.stdout.write('%s = %s\n' % (k, `options[k].value()`))

class ConfigDB:
    """Konfigurace ulo�en� v datov�m objektu s rozhran�m slovn�ku."""

    def __init__(self, resolver, name, *args, **kwargs):
        """Inicializuj instanci.

        Argumenty:

          resolver -- resolver specifikac� (instance 'pytis.util.Resolver').
          name -- jm�no specifikace datov�ho objektu, ze kter�ho m� b�t
            konfigurace na�tena.
          args, kwargs -- argumenty pro vytvo�en� datov�ho objektu, kter� budou
            p�ed�ny metod� 'pytis.data.Data.create()'.

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
