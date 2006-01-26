# -*- coding: iso-8859-2 -*-

# Copyright (C) 2002, 2003, 2005, 2006 Brailcom, o.p.s.
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

"""Definice �asto pou��van�ch funkc� a utilit pro Pytis aplikace.""" 

from pytis.extensions import *

import pytis.output
import pytis.form
import pytis.data
import re
# TODO: je to tu pot�eba?  P��le�itostn� smazat!
from pytis.presentation import *
from pytis.util import *
from pytis.form import *

import cPickle as pickle

import config


class DBConfig(object):
    """Konfigurace spojen� s datov�m objektem.

    Konfigurace vnit�n� pracuje s datov�m objektem vytvo�en�m nad specifikac�
    ur�enou argumentem konstruktoru.  P�edpokl�d� se, �e datov� objekt vrac�
    v�dy jen jeden ��dek (na �rovni SQL omezen� nap� na aktu�ln�ho u�ivatele).
    Hodnotu datov�ho sloupe�ku je potom mo�n� z tohoto objektu z�skat jako ze
    slovn�ku.

    Z�pis hodnoty do slovn�ku vyvol� zaps�n� zm�n�n� hodnoty do datab�ze.
    P��padn� zm�ny dat na �rvni datab�ze nejsou t�mto objektem v sou�asn�
    implementaci reflektov�ny.

    """

    def __init__(self, name):
        """Inicializuj instanci.

        Argument 'name' ur�uje n�zev specifikace datov�ho objektu pro resolver.

        """
        global data_object_cache
        try:
            cache = data_object_cache
        except NameError:
            cache = data_object_cache = {}
        try:
            data_object = cache[name]
        except KeyError:
            resolver = pytis.form.resolver()
            data_spec = resolver.get(name, 'data_spec')
            op = lambda: data_spec.create(dbconnection_spec=config.dbconnection)
            success, data_object = db_operation(op)
            if success:
                cache[name] = data_object
            else:
                data_object = None
        self._data = data_object
        self._data.select()
        self._row = self._data.fetchone()
        self._key = [self._row[c.id()] for c in self._data.key()]
        self._data.close()

    def value(self, key):
        """Vra� hodnotu 'key' jako instanci 'pytis.data.Value'."""
        return self._row[key]
        
    def __getitem__(self, key):
        """Vra� hodnotu 'key' jako Pythonovou hodnotu."""
        return self._row[key].value()

    def __setitem__(self, key, value):
        """Nastav hodnotu 'key' jako Pythonovou hodnotu."""
        type = self._row[key].type()
        self._row[key] = pytis.data.Value(type, value)
        self._data.update(self._key, self._row)

    def has_key(self, key):
        return self._row.has_key(key)

    def keys(self):
        return self._row.keys()

    def items(self):
        return tuple([(key, self[key]) for key in self._row.keys()])


class ConfigDB(DBConfig):
    """Wrapper pro zp�tnou kompatibilitu.  Nepou��vat!"""
    def __init__(self, resolver, name, **kwargs):
        super(ConfigDB, self).__init__(name)
    def __setitem__(self, key, value):
        super(ConfigDB, self).__setitem__(key, value.value())

def saved_config_reader(name, column):
    def reader():
        value = DBConfig(name)[column]
        try:
            return pickle.loads(str(value))
        except pickle.UnpicklingError, e:
            log(OPERATIONAL, "Nepoda�ilo se obnovit ulo�enou konfiguraci")
            return ()
    return reader

def saved_config_writer(name, column):
    def writer(items):
        DBConfig(name)[column] = pickle.dumps(items)
    return writer
    
def cfg_param(column, cfgspec='Nastaveni', value_column=None):
    """Vrac� instanci Value pro konfigura�n� parametr.

    Argumenty:

      column -- n�zev sloupce v konfigura�n� tabulce uveden� ve specifikaci
        udan� druh�m parametrem.
      cfgspec -- voliteln� n�zev specifikace s vazbou na konfigura�n� tabulku.
      value_column -- pokud je po�adavan� sloupec Codebook, umo��uje z�skat
        hodnotu u�ivatelsk�ho sloupce.

    """
    dbconfig = DBConfig(cfgspec)
    if not dbconfig.has_key(column):
        return pytis.data.Value(None, None)
    value = dbconfig.value(column)
    if value.type().enumerator():
        return cb2colvalue(value, column=value_column)
    else:
        return value

def cb_computer(codebook, column, default=None):
    """Vra� 'Computer' dopo��t�vaj�c� hodnotu ze sloupce ��seln�ku.

    Vytvo� instanci 'Computer', jej� dopo��t�vac� funkce vrac� hodnotu sloupce
    ��seln�ku.  Computer automaticky z�vis� na dan�m ��seln�kov�m pol��ku.

    Argumenty:
      'codebook' -- id ��seln�kov�ho pol��ka, z jeho� enumer�toru m� b�t
        hodnota zji�t�na.
      'column' -- id sloupce v datov�m objektu ��seln�ku, jeho� hodnota m� b�t
        dopo��t�vac� funkc� vr�cena.
      'default' -- implicitn� hodnota, kter� bude dopo��t�vac� funkc�
        vr�cena, pokud nen� hodnota ��seln�kov�ho pol��ka definov�na (je None).
    
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
    """P�eve� hodnotu pol��ka na hodnotu uveden�ho sloupce nav�zan�ho ��seln�ku.
    
    Argumenty:

      value -- Instance `Value', jej� typ m� definov�n enumer�tor typu
        'pytis.data.DataEnumerator'.
      column -- n�zev jin�ho sloupce ��seln�ku; �et�zec.  Viz
        'pytis.data.DataEnumerator.get()'

    Pokud odpov�daj�c� ��dek nen� nalezen, bude vr�cena instance 'Value'
    stejn�ho typu, jako je typ argumentu 'value' s hodnotou nastavenou na
    'None'.  Takov�to hodnota nemus� b�t validn� hodnotou typu, ale
    zjednodu�uje se t�m pr�ce s v�sledkem.  Pokud je zapot�eb� korektn�j��ho
    chov�n�, je doporu�eno pou��t p��mo metodu 'DataEnumerator.get()'
    (nap��klad vol�n�m 'value.type().enumerator().get(value.value(), column))'.
        
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
    """P�eve� instanci 'Value' typu 'Codebook' na 'Value' typu 'String'.

    Argumenty:

      value -- Instance `pytis.data.Value' typu `pytis.data.Codebook'.
      column -- n�zev jin�ho sloupce ��seln�ku; �et�zec.  Viz
        `Codebook.data_value()'.

    """
    assert isinstance(value, pytis.data.Value)
    assert value.type().enumerator() is not None
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
    """Zavolej datab�zovou funkci a vra� v�sledek jako Pythonovou hodnotu.

    Argumenty:

      name -- n�zev funkce.
      args -- argumenty vol�n� funkce; sekvence dvouprvkov�ch tupl�, kde prvn�
        prvek je n�zev argumentu a druh� jeho hodnota jako instance 'Value'.
      proceed_with_empty_values -- pokud je pravdiv�, vol� datab�zovou funkci
        v�dy.  V opa�n�m p��pad� (v�vchoz� chov�n�) testuje, zda v�echny
        argumenty obsahuj� nepr�zdnou hodnotu (jejich vnit�� hodnota nen� None
        ani pr�zdn� �et�zec) a pokud test neprojde, vr�t� None bez vol�n�
        datab�zov� funkce.  To znamen� �sporu pokud je tato funkce pou�ita v
        computeru pol��ka, kter� je z�visl� na jin�ch pol��k�ch, kter� je�t�
        nejsou vypln�na.

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
    """Zavolej nad tabulkou dan� specifikace select s dan�mi argumenty.

    Argumenty:

      data_spec -- n�zev specifikace datov�ho objektu nad kter�m m� b�t proveden
        select nebo p��mo instance t��dy 'pytis.data.DataFactory'
      args, kwargs -- argumenty vol�n� 'pytis.data.select()'.
        
    Vrac� v�echny ��dky vr�cen� z datab�ze jako list.
    
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
        sort=kwargs['sort']
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
    """Provede update nad tabulkou danou specifikac�.

    Argumenty:

      spec -- specifikace datov�ho objektu nad kter�m m� b�t proveden
        select; string'
      condition -- podm�nka updatovan�.
      update_row -- ��dek kter�m se provede update, 
        
    Vrac� po�et updatovan�ch ��dk�.
    
    """
    resolver = pytis.form.resolver()    
    if condition is None or not isinstance(condition,pytis.data.Operator):
        raise "Nebyla p�ed�na pro update_many"
    elif update_row is None or not isinstance(update_row,pytis.data.Row):
        raise "Nebyl p�ed�n ��dek pro update"
    data_spec = resolver.get(spec, 'data_spec')
    if not data_spec:
        raise "Specifikace %s nebyla nalezena!" % (spec)
    op = lambda: data_spec.create(dbconnection_spec=config.dbconnection)
    success, data = pytis.form.db_operation(op)
    if not success:
        raise "Nepoda�ilo se vytvo�it datov� objekt pro %s!" % (spec)
    result = data.update_many(condition, update_row) 
    return result

def dbinsert(spec, row=()):
    """Provede update nad tabulkou danou specifikac�.

    Argumenty:

      spec -- specifikace datov�ho objektu nad kter�m m� b�t proveden
        select; string'
      condition -- podm�nka updatovan�.
      update_row -- ��dek kter�m se provede update, 
        
    Vrac� po�et updatovan�ch ��dk�.
    
    """
    resolver = pytis.form.resolver()
    # Kontroly
    if not is_sequence(row):
        raise _("Argument must be a sequence")
    for item in row:
        if not is_sequence(item) or len(item) != 2:
            raise 'Column definition must be (ID, VALUE) pair'
        k, v = item
        if not is_string(k):
            raise 'Invalid column id %s' % k
        if not isinstance(v, pytis.data.Value):
            raise 'Invalid column value %s' % v
    data_spec = resolver.get(spec, 'data_spec')
    if not data_spec:
        raise "Specifikace %s nebyla nalezena!" % (spec)
    op = lambda: data_spec.create(dbconnection_spec=config.dbconnection)
    success, data = pytis.form.db_operation(op)
    if not success:
        raise "Nepoda�ilo se vytvo�it datov� objekt pro %s!" % (spec)
    op = lambda: data.insert(pytis.data.Row(data=row))
    success, result = pytis.form.db_operation(op)
    return result

def session_date(*args):
    """Vra� vnit�n� hodnotu nastaven�ho pracovn�ho datumu."""
    return session_date_value().value()

def session_date_value():
    """Vra� nastaven� pracovn� datum p�ihl�en�ho u�ivatele."""
    return cfg_param('datum', 'NastaveniUser')

def start_date(*args):
    """Vra� vnit�n� hodnotu nastaven�ho 'datumu od'."""
    return start_date_value().value()

def start_date_value():
    """Vra� nastaven� 'datum od' p�ihl�en�ho u�ivatele."""
    return cfg_param('datum_od', 'NastaveniUser')

def end_date(*args):
    """Vra� vnit�n� hodnotu nastaven�ho 'datumu do'."""
    return end_date_value().value()

def end_date_value():
    """Vra� nastaven� 'datum do' p�ihl�en�ho u�ivatele."""
    return cfg_param('datum_do', 'NastaveniUser')

def printdirect(resolver, spec, print_spec, row):
    """Tiskni specifikaci pomoc� p��kazu config.printing_command."""
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
        
    log(EVENT, 'Vyvol�n� tiskov�ho formul��e')
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
    MAX_LENGTH=480 # 3 SMS po 160 znac�ch
    SQSH='/usr/bin/sqsh'
    TEMPLATE="""
    insert into sms_request
    (tel_num_to, message_typ, request_typ, id_module, user_data)
    values
    ('%s', 0, 0, 0, '%s')
    """


    if not os.path.exists(SQSH):
        return "Nen� nainstalov�n bal�k 'sqsh'. SMS nebude odesl�na."
    if len(message) > MAX_LENGTH:
        return "Zpr�va je del�� ne� %s. SMS nebude odesl�na." % (MAX_LENGTH)
    if len(tel) not in (14,9):
	return ("�patn� from�t telefon�ho ��sla %s.\n\n"
	        "��slo mus� m�t tvar:\nPPPPPxxxxxxxxx - (p�t znak� p�edvolba "
                "- dev�t znak� ��slo)\nxxxxxxxxx - (dev�t znak� ��slo)") % (tel)
    message.replace('"','')
    message.replace("'","")	
    sms_insert = TEMPLATE % (tel, message)
    cmd = '%s -U %s -D %s -S %s -P %s -C "%s"' % \
	  (SQSH, UID, DB, SERVER, PWD, sms_insert)
    test, msg = commands.getstatusoutput(cmd)
    if test:
	msg = "SMS se nepoda�ilo odeslat!\n\n" + msg
	return msg
    return None

def emailsend(to, address, subject, msg, sendmail_command, content_type=None):
    """Ode�le email"""

    import os
    try:
        s = os.popen('%s %s' % (sendmail_command, to), 'w')
        s.write('From: %s\n' % address)
        s.write('To: %s\n' % to)
        s.write('Bcc: %s\n' % address)
        s.write('Subject: %s\n' % subject)
        if content_type:
            s.write('Content-Type: %s\n' % content_type)
        s.write('\n')
        s.write(msg)
        s.close()
        return 0
    except:
        print 'ERROR: e-mail se nepoda�ilo odeslat'
        return 1
    

def send_mail(to, address, subject, msg, sendmail_command='/usr/lib/sendmail',
              html=False, key=None, gpg_options=('--always-trust',)):
    """Ode�le jeden email s mo�nost� kryptov�n� pomoc� gpg/pgp kl��e."""
   
    def setup_gpg_options(gpg, options=()):
        gpg.options.armor = 1
        gpg.options.meta_interactive = 0
        gpg.options.extra_args.append('--no-secmem-warning')
        for o in options:            
            gpg.options.extra_args.append(o)

    def gpg_create_keyring(gpg, key, keyring):
        proc = gpg.run(['--import'], create_fhs=['stdin', 'stderr'])
        proc.handles['stdin'].write(key)
        proc.handles['stdin'].close()
        out = proc.handles['stderr'].read()
        proc.handles['stderr'].close()
        proc.wait()
        return keyring

    def gpg_encrypt_string(gpg, string, to):
        if isinstance(to, types.StringType):
            to = (to,)        
        gpg.options.recipients = to   # a list!        
        proc = gpg.run(['--encrypt'], create_fhs=['stdin', 'stdout'])        
        proc.handles['stdin'].write(string)       
        proc.handles['stdin'].close()
        output = proc.handles['stdout'].read()
        proc.handles['stdout'].close()        
        proc.wait()
        return output

    import os
    assert isinstance(to, types.StringTypes)
    assert isinstance(address, types.StringTypes)
    assert isinstance(subject, types.StringTypes)
    assert isinstance(msg, types.StringTypes)
    # O�et�en� p��padn�ho pou�it� UNICODE
    to = str(to)
    address = str(address)
    if html:
        msg = ("Content-Type: text/html;charset=ISO-8859-2\n"
               "Content-Transfer-Encoding: 8bit\n\n") + msg
    if key:
        try:
            import tempfile, GnuPGInterface
            keyring = tempfile.mkstemp()[1]
            gpg = GnuPGInterface.GnuPG()        
            gpg_options = ('--always-trust', '--no-default-keyring',
                           '--keyring=%s' % keyring)
            setup_gpg_options(gpg, gpg_options)
            gpg_create_keyring(gpg, key, keyring)
            msg = gpg_encrypt_string(gpg, msg, to)
            if not  isinstance(msg, types.StringType):
                print "What GnuPG gave back is not a string!"
                return 1
            try:
                os.remove(keyring)
            except:
                pass
        except:
            print "Couldn't encrypt message for %s" % to
            return 1
    if key and html:
        import email.Message
        import email.Utils
            
        # Main header
        mainMsg=email.Message.Message()
        mainMsg["To"]=to
        mainMsg["From"]=address
        mainMsg["Subject"]=subject
        mainMsg["Date"]=email.Utils.formatdate(localtime=1)
        mainMsg["Mime-version"]="1.0"
        mainMsg["Content-type"]="Multipart/encrypted"
        mainMsg["Content-transfer-encoding"]="8bit"
        mainMsg.preamble="This is an OpenPGP/MIME encrypted message (RFC 2440 and 3156)"
        # Part 1
        firstSubMsg=email.Message.Message()
        firstSubMsg["Content-Type"]="application/pgp-encrypted"
        firstSubMsg["Content-Description"]="PGP/MIME version identification"
        firstSubMsg.set_payload("Version: 1\n")
        # Part 2
        secondSubMsg=email.Message.Message()
        secondSubMsg.add_header("Content-Type", "application/octet-stream",
                                name="encrypted.html.pgp")
        secondSubMsg.add_header("Content-Description",
                                "OpenPGP encrypted message")
        secondSubMsg.add_header("Content-Disposition", "inline",
                                filename="encrypted.html.pgp")
        secondSubMsg.set_payload(msg)
        # P�id�n� ��st� do main
        mainMsg.attach(firstSubMsg)
        mainMsg.attach(secondSubMsg)
        msg = mainMsg.as_string()
    else:
        header = 'From: %s\n' % address
        header += 'To: %s\n' % to
        header += 'Subject: %s\n' % subject
        if html:
            header += 'Content-Type: text/html; charset=iso-8859-2\n'
        msg = header + '\n' + msg
    try:
        s = os.popen('%s %s' % (sendmail_command, to),'w')
        s.write(msg)
        s.close()
    except:        
        print "Couldn't send message for %s" % to
        return 1
    return 0

       
def run_cb(spec, begin_search=None, condition=None,
           columns=None, select_row=0):
    """Vyvol� ��seln�k ur�en� specifikac�.

    Argumenty:

      spec -- n�zev specifikace ��seln�ku.
      begin_search -- None nebo jm�no sloupce, nad kter�m se m� vyvolat
        inkrement�ln� vyhled�v�n�.
      condition -- podm�nka pro filtrov�n� z�znam�.
      columns -- seznam sloupc�, pokud se m� li�it od seznamu uveden�ho
        ve specifikaci.
      select_row -- ��dek, na kter� se m� nastavit kurzor.
        
    Vrac� None (pokud nen� vybr�n ��dn� ��dek) nebo vybran� ��dek.
    """
    return run_form(CodebookForm, spec, columns=columns,
                    begin_search=begin_search,
                    condition=condition,
                    select_row=select_row)

def row_update(row, values=()):
    """Provede update nad p�edan�m ��dkem.

    Argumenty:

      row -- p�edan� instance aktu�ln�ho PresentedRow.
      values -- sekvence dvouprvkov�ch sekvenc� ('id', value) ,
        kde 'id' je �et�zcov� identifik�tor pol��ka a value je
        instance, kterou se bude pol��ko aktualizovat.
    """
    data = row.data()
    updaterow = row.row()
    key = data.key()
    if is_sequence(key):
        key = key[0]
    for col, val in values:
        updaterow[col] = val
    op = lambda: data.update(row[key.id()], updaterow)
    return pytis.form.db_operation(op)

def is_in_groups(groups):
    if isinstance(groups, types.StringType):
        groups = xtuple(groups)
    from sets import Set
    conn = config.dbconnection
    dbgroups=pytis.data.PostgreSQLUserGroups.class_access_groups(conn)
    if Set(groups) & Set(dbgroups) == Set([]):
        return False
    else:
        return True


# Application function

def flatten_menus():
    """Vra� linearizovan� seznam v�ech polo�ek menu."""
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
             if isinstance(item, pytis.form.MItem) and item.command() == RF \
             and not issubclass(item.args()['form_class'],
                                pytis.form.ConfigForm)]
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
    # Zjist�me i varianty podle konstanty VARIANTS
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

def menu_report():
    """Vytv��� p�ehledn� n�hled na polo�ky menu."""
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
    content = "<h3>P�ehled polo�ek menu a n�zv� specifikac�</h3>"
    content += '<a name="menu"></a>'
    content += make_list(resolver.get('application', 'menu'))
    data_specs = remove_duplicates(data_specs)
    data_specs.sort()
    content += '<h1>P�ehled pr�v pro jednotliv� specifikace</h1>\n'
    for spec_name in data_specs:
        content += '<a name="%s"></a>\n<h5>%s</h5>\n' % (spec_name, spec_name)
        try:
            data_spec = resolver.get(spec_name, 'data_spec')
        except Exception, e:
            content += "<p><b>Chyba</b>: Specifikace nenalezena.</p>"
            continue
        rights = data_spec.access_rights()
        if rights:
            perms = (pytis.data.Permission.VIEW,
                     pytis.data.Permission.INSERT,
                     pytis.data.Permission.UPDATE,
                     pytis.data.Permission.DELETE,
                     pytis.data.Permission.EXPORT)
            content += "<table>"
            for perm in perms:
                groups = rights.permitted_groups(perm, None)
                content += '<tr><td valign="top">%s</td><td>%s</td></tr>' % \
                           ('<b>'+perm+'</b>', ', '.join(map(str, groups)))
            content += "</table>"
        content += "<a href=#menu>Zp�t na menu</a>"
    pytis.form.InfoWindow("P�ehled polo�ek menu a n�zv� specifikac�",
                          text=content, format=TextFormat.HTML)

cmd_menu_report = Command(Application, 'MENU_REPORT', handler=menu_report)
    
def check_form():
    """Zept� se na n�zev specifikace a zobraz� jej� report."""
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
        # Pol��ka v bindings
        cols = [c.id() for c in data.columns() if c.id()!='oid']
        obsah = "Pol��ka v data_spec:\n"
        obsah = obsah + "\n".join(cols)
        # N�zev tabulky
        table = data.table(cols[0])
        obsah = obsah + "\n\nTabulka: %s" % (table)
        # Pol��ka v bindings
        fields = [f.id() for f in view_spec.fields()]
        obsah = obsah + "\n\nPol��ka ve fields:\n"
        obsah = obsah + "\n".join(fields)
        # Title
        title = view_spec.title()
        obsah = obsah + "\n\n"
        obsah = obsah + "Title: %s" % (title)
        # Popup menu
        popup_menu = view_spec.popup_menu()
        if popup_menu:                
            popup_items = [p.title() for p in popup_menu]
            obsah = obsah + "\n\nPolo�ky popup_menu:\n"
            obsah = obsah + "\n".join(popup_items)
        pytis.form.run_dialog(pytis.form.Message,
                              "DEFS: %s" % spec,
                              report=obsah)

cmd_check_form = Command(Application, 'CHECK_FORM', handler=check_form)
        

def check_defs(seznam):
    """Zkontroluje specifikace pro uveden� seznam.

    Argumenty:
      seznam -- seznam n�zv� specifikac�

    """
    resolver = pytis.form.resolver()
    errors = []
    dbconn = dbconnection_spec=config.dbconnection
    def check_spec(update, seznam):
        total = len(seznam)
        last_error = ''
        step = 1 # aktualizujeme jen po ka�d�ch 'step' procentech...
        for n, s in enumerate(seznam):
            newmsg = "\n".join(("Kontroluji datov� specifikace...",
                                "Specifikace: " + s,
                                "Posledn� chyba v: " + last_error))
            status = int(float(n)/total*100/step)
            if not update(status*step, newmsg=newmsg):
                break
            try:
                data_spec = resolver.get(s, 'data_spec')
                try:
                    op = lambda: data_spec.create(dbconnection_spec=dbconn)
                    success, data = pytis.form.db_operation(op)
                    if not success:
                        err = "Specifikace %s: Nepoda�ilo se vytvo�it datov� objekt." % (s)
                        errors.append()
                        last_error = "%s\n(Nepoda�ilo se vytvo�it datov� objekt)" % s
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
    sirka = max([len(s) for s in seznam]) + len('Posledn� chyba v: ') + 6
    msg = 'Kontroluji datov� specifikace...'.ljust(sirka)
    msg = msg + '\n\n\n\n'
    pytis.form.run_dialog(pytis.form.ProgressDialog, check_spec, args=(seznam,),
                          message=msg, elapsed_time=True, can_abort=True)
    if errors:
        obsah = "\n".join(errors)
        pytis.form.run_dialog(pytis.form.Message,
                              "Chyby ve specifikac�ch",
                              report=obsah)
 
def check_menus_defs():
    return check_defs(get_menu_defs(without_duals=True))

cmd_check_menus_defs = Command(Application, 'CHECK_MENUS_DEFS',
                               handler=check_menus_defs)

def cache_spec(*args, **kwargs):
    resolver = pytis.form.resolver()
    def do(update, specs):
        total = len(specs)        
        last_status = 0
        step = 5 # aktualizujeme jen po ka�d�ch 'step' procentech...
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
    msg = '\n'.join(('Na��t�m specifikace (p�eru�te pomoc� Esc).', '',
                     'Na��t�n� je mo�no trvale vypnout pomoc� dialogu',
                     '"Nastaven� u�ivatelsk�ho rozhran�"'))
    specs = get_menu_defs()
    pytis.form.run_dialog(pytis.form.ProgressDialog, do, args=(specs,),
                          message=msg, elapsed_time=True, can_abort=True)

def help_window(inputfile=None, format=TextFormat.PLAIN):
    if not inputfile:
        pytis.form.run_dialog(pytis.form.Warning, _("Textov� soubor nenalezen"))
        return
    path = os.path.join(config.doc_dir, inputfile)
    if not os.path.exists(path):
        dir, xx = os.path.split(os.path.realpath(pytis.extensions.__file__))
        p = os.path.normpath(os.path.join(dir, '../../../doc', inputfile))
        if os.path.exists(p):
            path = p
        else:
            log(OPERATIONAL, "Soubor nenalezen:", p)
    try:
        f = open(path, 'r')
    except IOError, e:
        pytis.form.run_dialog(pytis.form.Error,
                              _("Nemohu otev��t soubor n�pov�dy: %s") % e)
    else:
        text = f.read()
        f.close()
        pytis.form.InfoWindow("N�pov�da", text=text, format=format)
        
cmd_help_window = Command(Application, 'HELP_WINDOW', handler=help_window)

def run_any_form():
    result = pytis.form.run_dialog(pytis.form.RunFormDialog)
    if result is not None:
        pytis.form.run_form(*result)
                                      
cmd_run_any_form = Command(Application, 'RUN_ANY_FORM', handler=run_any_form)

# Additional constraints
            
def constraints_email(email):
    """Kontroluje string podle re v�razu. Pokud odpov�d� nebo je None funkce
    vrac� None. Jinak vrac� string s chybovou hl�kou"""
    if email is None:
        return None
    mask=re.compile(r"^[A-Za-z0-9\d]([\w\d\.\-]?[A-Za-z0-9\_\&\.\-\d])*\@[A-Za-z0-9\d]([\w\d\.\-]?[A-Za-z0-9\_\&\d])*$")
    if mask.match(email.strip()) is None:
        return "�patn� tvar emailu " + email.strip()  + " !"
    return None

def constraints_email_many(emails):
    """Kontroluje string podle re v�razu pro ka�dou jeho ��st od�lenou ��rkou.
    Pokud odpov�d� nebo je None funkce vrac� None. Jinak vrac� string s
    chybovou hl�kou"""
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


