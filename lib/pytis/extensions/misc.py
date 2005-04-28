# -*- coding: iso-8859-2 -*-

# Copyright (C) 2002, 2003, 2005 Brailcom, o.p.s.
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

"""Definice často používaných funkcí a utilit pro Pytis aplikace.""" 

from pytis.extensions import *

import lib.output
import lib.form
import lib.data

# TODO: je to tu potřeba?  Příležitostně smazat!
from lib.presentation import *
from lib.util import *
from lib.form import *

import config


def cfg_param(column, cfgspec='Nastaveni', value_column=None):
    """Vrací instanci Value pro konfigurační parametr.

    Argumenty:

      column -- název sloupce v konfigurační tabulce uvedené ve specifikaci
        udané druhým parametrem.
      cfgspec -- volitelný název specifikace s vazbou na konfigurační tabulku.
      value_column -- pokud je požadavaný sloupec Codebook, umožňuje získat
        hodnotu uživatelského sloupce.

    """
    global cfg_objects
    data_object = None
    try:
        data_object = cfg_objects[cfgspec]
    except NameError:
        cfg_objects = {}
    except KeyError:
        pass
    if not data_object:    
        resolver = lib.form.resolver()
        cfg = resolver.get(cfgspec, 'data_spec')
        data_object = cfg.create(dbconnection_spec=config.dbconnection)
        cfg_objects[cfgspec] = data_object
    data_object.select()
    cfg_row = data_object.fetchone()
    if not cfg_row.has_key(column):
        return lib.data.Value(None, None)
    value = cfg_row[column]
    if isinstance(value.type(), lib.data.Codebook):
        return cb2colvalue(value, column=value_column)
    else:
        return value


def cb_computer(codebook, column, default=None):
    """Vrať 'Computer' dopočítávající hodnotu ze sloupce číselníku.

    Vytvoř instanci 'Computer', jejíž dopočítávací funkce vrací hodnotu sloupce
    číselníku.  Computer automaticky závisí na daném číselníkovém políčku.

    Argumenty:
      'codebook' -- id číselníkového políčka, z jehož datového objektu má být
        hodnota vyzvednuta.
      'column' -- id sloupce v datovém objektu číselníku, jehož hodnota má být
        dopočítávací funkcí vrácena.
      'default' -- implicitní hodnota, která bude dopočítávací funkcí
        vrácena, pokud není hodnota číselníkového políčka definována (je None).
    
    """
    assert isinstance(codebook, types.StringType)
    assert isinstance(column, types.StringType)
    def func(row):
        cbvalue = row[codebook]
        if cbvalue.value() is None:
            value = default
        else:
            value = cb2colvalue(cbvalue, column=column).value()
        return value
    return Computer(func, depends=(codebook,))


def cb2colvalue(value, column=None):
    """Převeď instanci 'Value' typu 'Codebook' na 'Value' uvedeného sloupce.
    
    Pokud sloupec není uveden, vrátí instanci 'Value' sloupce odpovídajícího
    klíčovému sloupci.

    Argumenty:

      value -- Instance `Value' typu `lib.data.Codebook'.
      column -- název jiného sloupce číselníku; řetězec. Viz
        'lib.data.Codebook.data_value()'
        
    """
    assert isinstance(value, lib.data.Value)
    assert isinstance(value.type(), lib.data.Codebook)
    v, e = value.type().validate(value.export())
    if not v:
        #TODO: Co to je?
        return lib.data.Value(None, None)
    elif column is None:
        return v
    else:
        return v.type().data_value(v.value(), column=column)

    
def cb2strvalue(value, column=None):
    """Převeď instanci 'Value' typu 'Codebook' na 'Value' typu 'String'.

    Argumenty:

      value -- Instance `lib.data.Value' typu `lib.data.Codebook'.
      column -- název jiného sloupce číselníku; řetězec.  Viz
        `Codebook.data_value()'.

    """
    assert isinstance(value, lib.data.Value)
    assert isinstance(value.type(), lib.data.Codebook) 
    if column is None:
        v = value.value()
    else:
        col_value = cb2colvalue(value, column=column)
        if col_value:
            v = col_value.value()
        else:
            v = None
    return lib.data.Value(lib.data.String(), v)


def dbfunction(name, *args, **kwargs):
    """Zavolej databázovou funkci a vrať výsledek jako Pythonovou hodnotu.

    Argumenty:

      name -- název funkce.
      args -- argumenty volání funkce; sekvence dvouprvkových tuplů, kde první
        prvek je název argumentu a druhý jeho hodnota jako instance 'Value'.
      proceed_with_empty_values -- pokud je pravdivé, volá databázovou funkci
        vždy.  V opačném případě (vývchozí chování) testuje, zda všechny
        argumenty obsahují neprázdnou hodnotu (jejich vnitří hodnota není None
        ani prázdný řetězec) a pokud test neprojde, vrátí None bez volání
        databázové funkce.  To znamená úsporu pokud je tato funkce použita v
        computeru políčka, které je závislé na jiných políčkách, která ještě
        nejsou vyplněna.

    """
    def proceed_with_empty_values(proceed_with_empty_values=False):
        return proceed_with_empty_values
    if not proceed_with_empty_values(**kwargs):
        for id, v in args:
            value = v.value()
            if value is None or value == '':
                return None
    op = lambda: lib.data.DBFunctionDefault(name, config.dbconnection)
    success, function = lib.form.db_operation(op)
    op = lambda: function.call(lib.data.Row(args))[0][0]
    success, result = lib.form.db_operation(op)
    return result.value()


def dbselect(data_spec, *args, **kwargs):
    """Zavolej nad tabulkou dané specifikace select s danými argumenty.

    Argumenty:

      data_spec -- specifikace datového objektu nad kterým má být proveden
        select; instance třídy 'lib.data.DBDataDefault'
      args, kwargs -- argumenty volání 'lib.data.select()'.
        
    Vrací všechny řádky vrácené z databáze jako list.
    
    """
    op = lambda: data_spec.create(dbconnection_spec=config.dbconnection)
    success, data = lib.form.db_operation(op)
    condition=None
    sort=()
    if kwargs.has_key('condition'):
        condition=kwargs['condition']
    if kwargs.has_key('sort'):
        condition=kwargs['sort']
    data.select(condition=condition, sort=sort)
    result = []
    while True:
        row = data.fetchone()
        if row is None:
            data.close()
            break
        result.append(row)
    return result

def dbupdate_many(spec, condition=None, update_row=None):
    """Provede update nad tabulkou danou specifikací.

    Argumenty:

      spec -- specifikace datového objektu nad kterým má být proveden
        select; string'
      condition -- podmínka updatovaní.
      update_row -- řádek kterým se provede update, 
        
    Vrací počet updatovaných řádků.
    
    """
    resolver = lib.form.resolver()    
    if condition is None or not isinstance(condition,lib.data.Operator):
        raise "Nebyla předána pro update_many"
    elif update_row is None or not isinstance(update_row,lib.data.Row):
        raise "Nebyl předán řádek pro update"
    data_spec = resolver.get(spec, 'data_spec')
    if not data_spec:
        raise "Specifikace %s nebyla nalezena!" % (spec)
    op = lambda: data_spec.create(dbconnection_spec=config.dbconnection)
    success, data = lib.form.db_operation(op)
    if not success:
        raise "Nepodařilo se vytvořit datový objekt pro %s!" % (spec)
    result = data.update_many(condition, update_row) 
    return result


def session_date(*args):
    """Vrať vnitřní hodnotu nastaveného pracovního datumu."""
    return session_date_value().value()

def session_date_value():
    """Vrať nastavené pracovní datum přihlášeného uživatele."""
    return cfg_param('datum', 'NastaveniUser')

def start_date(*args):
    """Vrať vnitřní hodnotu nastaveného 'datumu od'."""
    return start_date_value().value()

def start_date_value():
    """Vrať nastavené 'datum od' přihlášeného uživatele."""
    return cfg_param('datum_od', 'NastaveniUser')

def end_date(*args):
    """Vrať vnitřní hodnotu nastaveného 'datumu do'."""
    return end_date_value().value()

def end_date_value():
    """Vrať nastavené 'datum do' přihlášeného uživatele."""
    return cfg_param('datum_do', 'NastaveniUser')

def printdirect(resolver, spec, print_spec, row):
    """Tiskni specifikaci pomocí příkazu config.printing_command."""
    class _PrintResolver (lib.output.OutputResolver):
        P_NAME = 'P_NAME'
        class _Spec:
            def body(self, resolver):
                return None
            def doc_header(self, resolver):
                return None
            def doc_footer(self, resolver):
                return None
            def coding(self, resolver):
                if wx.Font_GetDefaultEncoding() == \
                   wx.FONTENCODING_ISO8859_2:
                    result = lib.output.Coding.LATIN2
                else:
                    result = lib.output.Coding.ASCII
                return result
        def _get_module(self, module_name):
            try:
                result = lib.output.OutputResolver._get_module(self,
                                                               module_name)
            except ResolverModuleError:
                result = self._Spec()
            return result
        
    log(EVENT, 'Vyvolání tiskového formuláře')
    spec_path = os.path.join('output', print_spec)
    P = _PrintResolver    
    parameters = {(spec+'/'+lib.output.P_ROW): row}
    parameters.update({P.P_NAME: spec})
    print_resolver = P(resolver, parameters=parameters)
    resolvers = (print_resolver,)
    formatter = lib.output.Formatter(resolvers, spec_path)
    formatter.printdirect()


def smssend(tel, message):
    import os, os.path, commands

    SERVER='192.168.1.55'
    UID='sms'
    PWD='sms'
    DB='SMS'
    MAX_LENGTH=480 # 3 SMS po 160 znacích
    SQSH='/usr/bin/sqsh'
    TEMPLATE="""
    insert into sms_request
    (tel_num_to, message_typ, request_typ, id_module, user_data)
    values
    ('%s', 0, 0, 0, '%s')
    """


    if not os.path.exists(SQSH):
        return "Není nainstalován balík 'sqsh'. SMS nebude odeslána."
    if len(message) > MAX_LENGTH:
        return "Zpráva je delší než %s. SMS nebude odeslána." % (MAX_LENGTH)
    if len(tel) == 13 and tel[0:4] == '+420':
	tel = '00420' + tel[4:]
    elif (len(tel) == 14 and tel[0:5] <> '00420') or len(tel) <> 9:
	return ("Špatný fromát telefoního čísla %s." + \
	        "Číslo musí mít tvar:\n00420xxxxxxxxx\n+420xxxxxxxxx\n" + \
		"xxxxxxxxx") % (tel) 
    message.replace('"','')
    message.replace("'","")	
    sms_insert = TEMPLATE % (tel, message)
    cmd = '%s -U %s -D %s -S %s -P %s -C "%s"' % \
	  (SQSH, UID, DB, SERVER, PWD, sms_insert)
    test, msg = commands.getstatusoutput(cmd)
    if test:
	msg = "SMS se nepodařilo odeslat!\n\n" + msg
	return msg
    return None
        
def run_cb(spec, begin_search=None, condition=None, select_row=0,
           columns=None):
    """Vyvolá číselník určený specifikací.

    Argumenty:

      spec -- název specifikace obsahující metodu 'cb_spec'
      begin_search -- None nebo jméno sloupce, nad kterým se má vyvolat
        inkrementální vyhledávání.
      condition -- podmínka pro filtrování záznamů
      columns -- seznam sloupců, pokud se má lišit od seznamu uvedenému
        ve specifikaci
      returned_column -- název sloupce, jehož hodnota se má vrátit, pokud
        se liší od názvu uvedeného ve specifikaci
      select_row -- řádek, na který se má nastavit kurzor
        
    Vrací None (pokud není vybrán žádný řádek) nebo instanci 'Value'
    pro sloupec 'returned_column'.
    """
    resolver = lib.form.resolver()
    cbspec = resolver.get(spec, 'cb_spec')
    if not columns:
        columns = cbspec.columns()
    if not returned_column:    
        returned_column = cbspec.returned_column()
    result = run_form(CodeBook, cbspec.name(), columns=columns,
                     begin_search=begin_search,
                     condition=condition, 
                     select_row=select_row)
    if result:
        return result[returned_column]

def row_update(row, values=()):
    """Provede update nad předaným řádkem.

    Argumenty:

      row -- předaná instance aktuálního PresentedRow.
      values -- sekvence dvouprvkových sekvencí ('id', value) ,
        kde 'id' je řetězcový identifikátor políčka a value je
        instance, kterou se bude políčko aktualizovat.
    """        
    data = row.data()
    updaterow = row.row()
    key = data.key()
    if is_sequence(key):
        key = key[0]
    for col, val in values:
        updaterow[col] = val
    data.update(row[key.id()], updaterow)
