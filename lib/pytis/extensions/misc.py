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

import pytis.output
import pytis.form
import pytis.data
import re
# TODO: je to tu potřeba?  Příležitostně smazat!
from pytis.presentation import *
from pytis.util import *
from pytis.form import *

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
        resolver = pytis.form.resolver()
        cfg = resolver.get(cfgspec, 'data_spec')
        data_object = cfg.create(dbconnection_spec=config.dbconnection)
        cfg_objects[cfgspec] = data_object
    data_object.select()
    cfg_row = data_object.fetchone()
    if not cfg_row.has_key(column):
        return pytis.data.Value(None, None)
    value = cfg_row[column]
    if value.type().enumerator():
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
    """Převeď hodnotu políčka na hodnotu uvedeného sloupce navázaného číselníku.
    
    Argumenty:

      value -- Instance `Value' typu `pytis.data.Codebook'.
      column -- název jiného sloupce číselníku; řetězec. Viz
        'pytis.data.Codebook.data_value()'

    Pokud odpovídající řádek není nalezen, bude vrácena instance 'Value'
    stejného typu, jako je typ argumentu 'value' s hodnotou nastavenou na
    'None'.  Takováto hodnota nemusí být validní hodnotou typu, ale
    zjednodušuje se tím práce s výsledkem.  Pokud je zapotřebí korektnějšího
    chování, je doporučeno použít přímo metodu 'DataEnumerator.get()'
    (například voláním 'value.type().enumerator().get(value.value(), column))'.
        
    """
    assert isinstance(value, pytis.data.Value)
    assert value.type().enumerator() is not None
    if column is None:
        return value
    else:
        v = value.type().enumerator().get(value.value(), column=column)
        if v is None:
            return pytis.data.Value(value.type(), None)
        else:
            return v
    
def cb2strvalue(value, column=None):
    """Převeď instanci 'Value' typu 'Codebook' na 'Value' typu 'String'.

    Argumenty:

      value -- Instance `pytis.data.Value' typu `pytis.data.Codebook'.
      column -- název jiného sloupce číselníku; řetězec.  Viz
        `Codebook.data_value()'.

    """
    assert isinstance(value, pytis.data.Value)
    assert isinstance(value.type(), pytis.data.Codebook) 
    if column is None:
        v = value.value()
    else:
        col_value = cb2colvalue(value, column=column)
        if col_value:
            v = col_value.value()
        else:
            v = None
    return pytis.data.Value(pytis.data.String(), v)


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
    op = lambda: pytis.data.DBFunctionDefault(name, config.dbconnection)
    success, function = pytis.form.db_operation(op)
    op = lambda: function.call(pytis.data.Row(args))[0][0]
    success, result = pytis.form.db_operation(op)
    return result.value()


def dbselect(data_spec, *args, **kwargs):
    """Zavolej nad tabulkou dané specifikace select s danými argumenty.

    Argumenty:

      data_spec -- název specifikace datového objektu nad kterým má být proveden
        select nebo přímo instance třídy 'pytis.data.DataFactory'
      args, kwargs -- argumenty volání 'pytis.data.select()'.
        
    Vrací všechny řádky vrácené z databáze jako list.
    
    """    
    if isinstance(data_spec, types.StringType):
        data_spec = pytis.form.resolver().get(data_spec, 'data_spec')
    op = lambda: data_spec.create(dbconnection_spec=config.dbconnection)
    success, data = pytis.form.db_operation(op)
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
    resolver = pytis.form.resolver()    
    if condition is None or not isinstance(condition,pytis.data.Operator):
        raise "Nebyla předána pro update_many"
    elif update_row is None or not isinstance(update_row,pytis.data.Row):
        raise "Nebyl předán řádek pro update"
    data_spec = resolver.get(spec, 'data_spec')
    if not data_spec:
        raise "Specifikace %s nebyla nalezena!" % (spec)
    op = lambda: data_spec.create(dbconnection_spec=config.dbconnection)
    success, data = pytis.form.db_operation(op)
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
    class _PrintResolver (pytis.output.OutputResolver):
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
                    result = pytis.output.Coding.LATIN2
                else:
                    result = pytis.output.Coding.ASCII
                return result
        def _get_module(self, module_name):
            try:
                result = pytis.output.OutputResolver._get_module(self,
                                                               module_name)
            except ResolverModuleError:
                result = self._Spec()
            return result
        
    log(EVENT, 'Vyvolání tiskového formuláře')
    spec_path = os.path.join('output', print_spec)
    P = _PrintResolver    
    parameters = {(spec+'/'+pytis.output.P_ROW): row}
    parameters.update({P.P_NAME: spec})
    print_resolver = P(resolver, parameters=parameters)
    resolvers = (print_resolver,)
    formatter = pytis.output.Formatter(resolvers, spec_path)
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
        
def run_cb(spec, begin_search=None, condition=None, columns=None,
           returned_column=None, select_row=0):
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
    resolver = pytis.form.resolver()
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


# Application function

def flatten_menus():
    """Vrať linearizovaný seznam všech položek menu."""
    def flatten(queue, found, level=0):
        if queue:
            head, tail = queue[0], queue[1:]
            found.append(head)
            if isinstance(head, pytis.form.Menu):
                flatten(head.items(), found, level=level+1)
            result = flatten(tail, found, level=level)
        else:
            result = found                
        return result
    resolver = pytis.form.resolver()
    menus = resolver.get('application', 'menu')
    return flatten(menus, [])


def get_menu_defs(without_duals=False):
    resolver = pytis.form.resolver()
    RF = pytis.form.Application.COMMAND_RUN_FORM
    items = [item for item in flatten_menus()
             if isinstance(item, pytis.form.MItem) and item.command() == RF]
    duals = [item for item in items
             if issubclass(item.args()['form_class'], DualForm) \
             and not issubclass(item.args()['form_class'], DescriptiveDualForm)]
    notduals = [item for item in items if item not in duals]
    subduals = []
    for item in duals:
        dual_spec = resolver.get(item.args()['name'], 'dual_spec')
        subduals.append(dual_spec.main_name())
        subduals.append(dual_spec.side_name())
    specs = [item.args()['name'] for item in notduals] + subduals
    if not without_duals:
        specs = specs + [item.args()['name'] for m in duals]
    specs = remove_duplicates(specs)
    # Zjistíme i varianty podle konstanty VARIANTS
    variants = []
    for m in specs:
        try:
            vlist = resolver.get_object(m, 'VARIANTS')
            vlist = ['%s:%s' % (m, v) for v in vlist
                     if isinstance(v, types.StringType)]
            variants = variants + list(vlist)
        except Exception, e:
            pass
    return remove_duplicates(specs + variants)

def menu_report(*args, **kwargs):
    """Vytváří přehledný náhled na položky menu."""
    resolver = pytis.form.resolver()
    data_specs = []
    COMMAND_RUN_FORM = pytis.form.Application.COMMAND_RUN_FORM
    def spec(name):
        return '<a href="#%s">%s</a>' % (name, name)
    def make_list(menu):
        items = []
        for item in menu:
            if isinstance(item, MSeparator):
                x = '------'
            elif isinstance(item, Menu):
                x = item.title() + make_list(item.items())
            elif isinstance(item, MItem) and item.command() == COMMAND_RUN_FORM:
                args = item.args()
                form = args['form_class']
                spec_name = args['name']
                spec_link = spec(spec_name)
                data_specs.append(spec_name)
                if issubclass(form, DualForm) and \
                       not issubclass(form, DescriptiveDualForm):
                    dual_spec = resolver.get(spec_name, 'dual_spec')
                    main = dual_spec.main_name()
                    side = dual_spec.side_name()
                    spec_link += "(%s,%s)" % (spec(main), spec(side))
                    data_specs.extend((main, side))
                x = "%s: %s, %s" % (item.title(), spec_link, form.__name__)
            else:
                x = item.title()
            items.append(x)
        list_items = ["<li>%s</li>" % i for i in items]
        return "\n".join(("<ul>",) + tuple(list_items) + ("</ul>",))
    content = "<h3>Přehled položek menu a názvů specifikací</h3>"
    content += '<a name="menu"></a>'
    content += make_list(resolver.get('application', 'menu'))
    data_specs = remove_duplicates(data_specs)
    data_specs.sort()
    PRAVA = (pytis.data.Permission.VIEW,
             pytis.data.Permission.INSERT,
             pytis.data.Permission.UPDATE,
             pytis.data.Permission.DELETE,
             pytis.data.Permission.EXPORT)
    content += '<h1>Přehled práv pro jednotlivé specifikace</h1>\n'
    for spec_name in data_specs:
        content += '<a name="%s"></a>\n<h5>%s</h5>\n' % (spec_name, spec_name)
        try:
            data_spec = resolver.get(spec_name, 'data_spec')
        except Exception, e:
            content += "<p><b>Chyba</b>: Specifikace nenalezena.</p>"
            continue
        prava = data_spec.access_rights()
        if prava:
            all = ['<tr><td valign="top"><b>%s</b></td><td>%s</td></tr>' %
                   (p, ', '.join(prava.permitted_groups(p, None)))
                   for p in PRAVA]
            content += "<table>" + "\n".join(all) + "</table>"
        content += "<a href=#menu>Zpět na menu</a>"    
    pytis.form.InfoWindow("Přehled položek menu a názvů specifikací",
                          text=content, format=TextFormat.HTML)

    
def check_form(*args, **kwargs):
    """Zeptá se na název specifikace a zobrazí její report."""
    resolver = pytis.form.resolver()
    spec = pytis.form.run_dialog(pytis.form.InputDialog,
                               message="Kontrola defsu",
                               prompt="Specifikace",
                               input_width=30)
    if spec:
        try:
            data_spec = resolver.get(spec, 'data_spec')
            view_spec = resolver.get(spec, 'view_spec')                
        except ResolverError:
            msg = 'Specifikace nenalezena.'
            pytis.form.run_dialog(pytis.form.Warning, msg)
            return
        data = data_spec.create(dbconnection_spec=config.dbconnection)
        # Políčka v bindings
        cols = [c.id() for c in data.columns() if c.id()!='oid']
        obsah = "Políčka v data_spec:\n"
        obsah = obsah + "\n".join(cols)
        # Název tabulky
        table = data.table(cols[0])
        obsah = obsah + "\n\nTabulka: %s" % (table)
        # Políčka v bindings
        fields = [f.id() for f in view_spec.fields()]
        obsah = obsah + "\n\nPolíčka ve fields:\n"
        obsah = obsah + "\n".join(fields)
        # Title
        title = view_spec.title()
        obsah = obsah + "\n\n"
        obsah = obsah + "Title: %s" % (title)
        # Popup menu
        popup_menu = view_spec.popup_menu()
        if popup_menu:                
            popup_items = [p.title() for p in popup_menu]
            obsah = obsah + "\n\nPoložky popup_menu:\n"
            obsah = obsah + "\n".join(popup_items)
        pytis.form.run_dialog(pytis.form.Message,
                              "DEFS: %s" % spec,
                              report=obsah)

def check_defs(seznam):
    """Zkontroluje specifikace pro uvedený seznam.

    Argumenty:
      seznam -- seznam názvů specifikací

    """
    resolver = pytis.form.resolver()
    errors = []
    dbconn = dbconnection_spec=config.dbconnection
    def check_spec(update, seznam):
        total = len(seznam)
        last_error = ''
        step = 1 # aktualizujeme jen po každých 'step' procentech...
        for n, s in enumerate(seznam):
            newmsg = "\n".join(("Kontroluji datové specifikace...",
                                "Specifikace: " + s,
                                "Poslední chyba v: " + last_error))
            status = int(float(n)/total*100/step)
            if not update(status*step, newmsg=newmsg):
                break
            try:
                data_spec = resolver.get(s, 'data_spec')
                try:
                    op = lambda: data_spec.create(dbconnection_spec=dbconn)
                    success, data = pytis.form.db_operation(op)
                    if not success:
                        err = "Specifikace %s: Nepodařilo se vytvořit datový objekt." % (s)
                        errors.append()
                        last_error = "%s\n(Nepodařilo se vytvořit datový objekt)" % s
                        continue
                    data.select()
                    row = data.fetchone()
                    if row:
                        try:
                            view_spec = resolver.get(s, 'view_spec')
                            fields = view_spec.fields()
                            prow = PresentedRow(fields, data, row)
                        except Exception, e:
                            err = """Specifikace %s: %s""" % (s, str(e))
                            errors.append(err)
                            last_error = "%s\n%s...)" % (s, str(e)[:sirka-4])
                except Exception, e:
                    err = """Specifikace %s: %s""" % (s, str(e))
                    errors.append(err)
                    last_error = "%s\n%s...)" % (s, str(e)[:sirka-4])
            except ResolverError, e:
                err = """Specifikace %s: %s""" % (s, str(e))
                errors.append(err)                
                last_error = "%s\n%s...)" % (s, str(e)[:sirka-4])
    sirka = max([len(s) for s in seznam]) + len('Poslední chyba v: ') + 6
    msg = 'Kontroluji datové specifikace...'.ljust(sirka)
    msg = msg + '\n\n\n\n'
    pytis.form.run_dialog(pytis.form.ProgressDialog, check_spec, args=(seznam,),
                          message=msg, elapsed_time=True, can_abort=True)
    if errors:
        obsah = "\n".join(errors)
        pytis.form.run_dialog(pytis.form.Message,
                              "Chyby ve specifikacích",
                              report=obsah)
 
def check_menus_defs(*args, **kwargs):        
    return check_defs(get_menu_defs(without_duals=True))

def cache_spec(*args, **kwargs):
    resolver = pytis.form.resolver()
    def do(update, specs):
        total = len(specs)        
        last_status = 0
        step = 5 # aktualizujeme jen po každých 'step' procentech...
        for n, file in enumerate(specs):
            status = int(float(n)/total*100/step)
            if status != last_status:
                last_status = status 
                if not update(status*step):
                    break
            for spec in ('dual_spec', 'data_spec', 'view_spec',
                         'cb_spec', 'proc_spec'):
                try:
                    resolver.get(file, spec)
                except ResolverError:
                    pass
    msg = '\n'.join(('Načítám specifikace (přerušte pomocí Esc).', '',
                     'Načítání je možno trvale vypnout pomocí dialogu',
                     '"Nastavení uživatelského rozhraní"'))
    specs = get_menu_defs()
    pytis.form.run_dialog(pytis.form.ProgressDialog, do, args=(specs,),
                          message=msg, elapsed_time=True, can_abort=True)

def help_window(*args, **kwargs):
    if not kwargs.has_key('inputfile') or kwargs['inputfile'] is None:
        pytis.form.run_dialog(pytis.form.Warning,
                              _("Textový soubor nenalezen"))
        return
    inputfile = kwargs['inputfile']
    if not kwargs.has_key('format') or kwargs['format'] is None:
        format = TextFormat.PLAIN
    else:
        format = kwargs['format']
    path = config.doc_dir
    if not path.endswith('/'):
        path += '/' 
    path = path + inputfile
    try:
        f = open(path, 'r')
    except:
        pytis.form.run_dialog(pytis.form.Warning,
                              _("Textový soubor nenalezen"))
    text = f.read()
    f.close()
    pytis.form.InfoWindow("Nápověda", text=text, format=format)        


# Additional constraints
            
def constraints_email(email):
    """Kontroluje string podle re výrazu. Pokud odpovídá nebo je None funkce
    vrací None. Jinak vrací string s chybovou hláškou"""
    if email is None:
        return None
    mask=re.compile(r"^[A-Za-z0-9\d]([\w\d\.\-]?[A-Za-z0-9\_\&\.\d])*\@[A-Za-z0-9\d]([\w\d\.\-]?[A-Za-z0-9\_\&\d])*$")
    if mask.match(email.strip()) is None:
        return "Špatný tvar emailu " + email.strip()  + " !"
    return None

def constraints_email_many(emails):
    """Kontroluje string podle re výrazu pro každou jeho část odělenou čárkou.
    Pokud odpovídá nebo je None funkce vrací None. Jinak vrací string s
    chybovou hláškou"""
    if emails is None:
        return None
    not_match=[]
    for email in emails.split(','):
        result = constraints_email(email)
        if not result is None:
            not_match.append(result)
    if len(not_match) == 0:
        return None
    return '\n'.join(not_match)


