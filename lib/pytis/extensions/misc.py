# -*- coding: utf-8 -*-

# Copyright (C) 2018-2019 Tomáš Cerha <t.cerha@gmail.com>
# Copyright (C) 2002-2014 Brailcom, o.p.s.
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

"""Drobné doplňkové funkce.

Do tohoto modulu patří funkce, které nemají s Pytisem jako takovým nic
společného.  Nepracují přímo s jeho API, jen se zkrátka v defsech mohou nějak
hodit.

"""

import os
import re
import string

import pytis.data
from pytis.util import translations, ProgramError

_ = translations('pytis-wx')


def smssend(tel, message, server='192.168.1.55'):
    import commands

    SERVER = server
    UID = 'sms'
    PWD = 'sms'
    DB = 'SMS'
    MAX_LENGTH = 480  # 3 SMS po 160 znacích
    SQSH = '/usr/bin/sqsh'
    TEMPLATE = """
    insert into sms_request
    (tel_num_to, message_typ, request_typ, id_module, user_data)
    values
    ('%s', 0, 0, 0, '%s')
    """

    if not os.path.exists(SQSH):
        return "Není nainstalován balík 'sqsh'. SMS nebude odeslána."
    if len(message) > MAX_LENGTH:
        return "Zpráva je delší než %s. SMS nebude odeslána." % (MAX_LENGTH)
    if len(tel) not in (14, 9):
        return ("Špatný formát telefoního čísla %s.\n\n"
                "Číslo musí mít tvar:\nPPPPPxxxxxxxxx - (pět znaků předvolba "
                "- devět znaků číslo)\nxxxxxxxxx - (devět znaků číslo)") % (tel,)
    message.replace('"', '')
    message.replace("'", "")
    sms_insert = TEMPLATE % (tel, message)
    cmd = ('%s -U %s -D %s -S %s -P %s -C "%s"' %
           (SQSH, UID, DB, SERVER, PWD, sms_insert,))
    test, msg = commands.getstatusoutput(cmd)
    if test:
        msg = "SMS se nepodařilo odeslat!\n\n" + msg
        return msg
    return None


# TODO: this procedure is obsolete and superceeded with send_mail and should be removed
#       after checking its use in applications
def emailsend(to, address, subject, msg, sendmail_command, content_type=None):
    """Odešle email"""

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
    except Exception:
        print 'ERROR: e-mail se nepodařilo odeslat'
        return 1


# TODO: argument sendmail_command should be removed when all applications reflect this change
def send_mail(to, address, subject, msg, html=False, key=None, charset='ISO-8859-2',
              sendmail_command=None):
    """Send an email with the possibility to encrypt it with a GPG/PGP key."""
    assert isinstance(to, basestring), to
    assert isinstance(address, basestring), address
    assert isinstance(subject, basestring), subject
    assert isinstance(msg, basestring), msg

    def get_utf8_argument(arg):
        if isinstance(arg, str):
            try:
                arg = unicode(arg, charset)
            except Exception:
                raise ProgramError("Cannot convert argument to unicode for charset %s" % (charset,))
        return arg.encode('UTF-8')
    # Převedení na UTF-8
    to = get_utf8_argument(to)
    address = get_utf8_argument(address)
    subject = get_utf8_argument(subject)
    msg = get_utf8_argument(msg)
    if key is not None:
        # We will create GPG encrypted email
        from pytis.extensions.email_ import GPGEmail
        mail = GPGEmail(to, address, subject, msg, html=html, key=key, charset='UTF-8')
    else:
        # We will create simple text/html email
        from pytis.extensions.email_ import SimpleEmail
        mail = SimpleEmail(to, address, subject, msg, html=html, charset='UTF-8')
    result = mail.send()
    if not result:
        # Sending email failed -- return an error message
        return mail.get_error_msg()
    else:
        # Success - return no error message
        return None


# Additional handled actions for DMP

class UserDefaultPrinter(object):
    def __init__(self):
        try:
            lpoptions = os.environ["HOME"]
        except KeyError:
            try:
                lpoptions = "/home/" + os.environ["USER"]
            except KeyError:
                lpoptions = None
        if lpoptions:
            lpoptions += "/.cups/lpoptions"
        self.lpoptions = lpoptions

    def clear(self):
        if not self.lpoptions:
            return
        try:
            opts = open(self.lpoptions, 'r').readlines()
        except IOError:
            return
        for i in range(len(opts)):
            if opts[i].startswith("Default "):
                opts[i] = "Dest " + opts[i][8:]
        open(self.lpoptions, "w").writelines(opts)

    def get(self):
        if not self.lpoptions:
            return None
        try:
            opts = open(self.lpoptions, 'r').readlines()
        except IOError:
            return None
        for i in range(len(opts)):
            if opts[i].startswith("Default "):
                rest = opts[i][8:]
                slash = rest.find("/")
                if slash != -1:
                    space = rest[:slash].find(" ")
                else:
                    space = rest.find(" ")
                return rest[:space]
        return None

    def set(self, default):
        import subprocess
        p = subprocess.Popen(["lpoptions", "-d", default],
                             close_fds=True,
                             stdin=open("/dev/null", 'r'),
                             stdout=open("/dev/null", 'w'),
                             stderr=subprocess.PIPE)
        (stdout, stderr) = p.communicate()
        exitcode = p.wait()
        if exitcode != 0:
            raise ProgramError(stderr.strip())
        return

    def __repr__(self):
        return "<UserDefaultPrinter (%s)>" % repr(self.get())


def set_default_printer():
    from pytis.presentation import Field
    from pytis.form import Error, Text, InputForm, run_dialog, run_form
    try:
        import cups
        import cupshelpers
    except ImportError:
        run_dialog(Error,
                   _("Default printer setup failed.\n"
                     "CUPS Python interface not present.\n"
                     "Please, contact the system administrator."))
        return None
    connection = cups.Connection()
    user_default = UserDefaultPrinter()
    default_printer = user_default.get()
    if not default_printer:
        default_printer = connection.getDefault()
    printers = cupshelpers.getPrinters(connection)
    result = run_form(
        InputForm,
        title=_("Printer Selection"),
        fields=(
            Field('printer', "", width=40, not_null=True,
                  type=pytis.data.String,
                  enumerator=pytis.data.FixedEnumerator(printers.keys()),
                  default=default_printer),
        ),
        layout=(Text(_("Choose the default printer:")), 'printer'),
    )
    if result:
        user_default.set(result['printer'].value())
    return None


def cmd_set_default_printer():
    import pytis.form
    pytis.form.Application.COMMAND_HANDLED_ACTION(handler=set_default_printer)


# Additional constraints


def constraints_email(email):
    """Ověř platnost zápisu e-mailové adresy.

    Pokud má adresa platný tvar, nebo je None vrací None.  Jinak vrací řetězec
    s chybovou hláškou

    """
    if email is None:
        return None
    mask = re.compile(r"^[A-Za-z0-9\d]([\w\d\.\-]?[A-Za-z0-9\_\&\.\-\d])*\@[A-Za-z0-9\d]"
                      r"([\w\d\.\-]?[A-Za-z0-9\_\&\d])*$")
    if mask.match(email.strip()) is None:
        return "Špatný tvar emailu " + email.strip() + " !"
    return None


def constraints_email_many(emails):
    """Ověř platnost zápisu seznamu e=mailových adres oddělených čárkami.

    Pokud má seznam i každá adresa platný tvar, nebo je None funkce vrací None.
    Jinak vrací řetězec s chybovou hláškou.

    """
    if emails is None:
        return None
    not_match = []
    for email in emails.split(','):
        result = constraints_email(email)
        if result is not None:
            not_match.append(result)
    if len(not_match) == 0:
        return None
    return '\n'.join(not_match)


# Některé často používané konfigurační parametry.


def session_date(transaction=None):
    """Vrať vnitřní hodnotu nastaveného pracovního datumu."""
    return session_date_value().value()


def session_date_value(transaction=None):
    """Vrať nastavené pracovní datum přihlášeného uživatele."""
    from pytis.extensions import cfg_param
    return cfg_param('datum', 'Nastaveni.BvUsersCfg')


def start_date(transaction=None):
    """Vrať vnitřní hodnotu nastaveného 'datumu od'."""
    return start_date_value().value()


def start_date_value(transaction=None):
    """Vrať nastavené 'datum od' přihlášeného uživatele."""
    from pytis.extensions import cfg_param
    return cfg_param('datum_od', 'Nastaveni.BvUsersCfg')


def end_date(transaction=None):
    """Vrať vnitřní hodnotu nastaveného 'datumu do'."""
    return end_date_value().value()


def end_date_value(transaction=None):
    """Vrať nastavené 'datum do' přihlášeného uživatele."""
    from pytis.extensions import cfg_param
    return cfg_param('datum_do', 'Nastaveni.BvUsersCfg')


# Database encryption utilities


def crypto_key_table(connection_data):
    """Return data object corresponding to the table of crypto keys.

    Arguments:

      connection_data -- database connection data; 'pytis.data.DBConnection'
        instance

    """
    return pytis.data.dbtable('e_pytis_crypto_keys',
                              ('key_id', 'name', 'username', 'key', 'fresh',),
                              connection_data)


def crypto_admin_key(area, admin_user, connection_data):
    """Return crypto admin key for the given user and area.

    The return value is a pair (KEY_ID, KEY) where KEY_ID is the database
    record key and KEY is the actual key.

    Arguments:

      area -- name of the crypto area; basestring
      admin_user -- login name of the crypto area administrator; basestring
      connection_data -- database connection data; 'pytis.data.DBConnection'
        instance

    """
    data = crypto_key_table(connection_data)
    condition = pytis.data.AND(pytis.data.EQ('username', pytis.data.sval(admin_user)),
                               pytis.data.EQ('name', pytis.data.sval(area)))
    if not data.select(condition):
        return None, None
    row = data.fetchone()
    return row['key_id'], row['key']


def check_crypto_password(key, password, connection_data):
    """Return true iff the given key and password match.

    Arguments:

      key -- encrypted key; basestring
      password -- password to the key; basestring
      connection_data -- database connection data; 'pytis.data.DBConnection'
        instance

    """
    function = pytis.data.DBFunctionDefault('pytis_crypto_extract_key', connection_data)
    row = pytis.data.Row((('encrypted', key,), ('psw', pytis.data.sval(password),),))
    return True if function.call(row)[0][0].value() else False


def add_crypto_user(area, user, admin_user, admin_password, admin_address, connection_data,
                    transaction=None, user_password=None):
    """Add new crypto user for the given area.

    If the action succeeds, return 'None'.  Otherwise return an error
    description (basestring).

    Arguments:

      area -- name of the crypto area; basestring
      user -- login name of the user to get the access to the area; basestring
      admin_user -- login name of the crypto area administrator; basestring
      admin_password -- password to the admin key; basestring
      admin_address -- e-mail address of the admin user, to be used as the
        sender of the notification e-mail; basestring
      connection_data -- database connection data; 'pytis.data.DBConnection'
        instance
      transaction -- transaction to use
      user_password -- string to use as the password for given login name; in such a case
        password will not be sent by email.

    """
    key_id, key = crypto_admin_key(area, admin_user, connection_data)
    if key_id is None:
        return "admin key not found for the area: %s" % (area,)
    try:
        transaction_ = transaction
        if transaction_ is None:
            transaction_ = pytis.data.DBTransactionDefault(connection_data)
        condition = pytis.data.AND(pytis.data.EQ('name', pytis.data.sval(area)),
                                   pytis.data.EQ('username', pytis.data.sval(user)))
        data = pytis.data.dbtable('e_pytis_crypto_keys', ('name', 'username',), connection_data)
        if data.select(condition) > 0:
            return "key already exists for the given user and area: %s %s" % (user, area,)
        data.close()
        if user_password is None:
            function = pytis.data.DBFunctionDefault('pytis_crypto_user_contact', connection_data)
            row = pytis.data.Row((('username', pytis.data.sval(user),),))
            result = function.call(row, transaction=transaction_)[0]
            email, gpg_key = [v.value() for v in result]
            if gpg_key is None:
                return "crypto contact not found for user: %s" % (user,)
            characters = 'ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz23456789_-=$#%!'
            user_password = string.join([characters[ord(c) % 64] for c in os.urandom(32)], '')
            send_password = True
        else:
            send_password = False
        if not check_crypto_password(key, admin_password, connection_data):
            return "invalid password"
        function = pytis.data.DBFunctionDefault('pytis_crypto_copy_key', connection_data)
        row = pytis.data.Row((('name_', pytis.data.sval(area),),
                              ('from_user', pytis.data.sval(admin_user),),
                              ('to_user', pytis.data.sval(user),),
                              ('from_psw', pytis.data.sval(admin_password),),
                              ('to_psw', pytis.data.sval(user_password),),))
        if not function.call(row, transaction=transaction_)[0][0]:
            return "user key installation failed"
        if send_password:
            subject = u"Vaše heslo pro šifrovanou oblast %s" % (area,)
            text = u"Vaše heslo pro šifrovanou aplikační oblast %s je:\n%s\n" % (
                area, user_password
            )
            error = send_mail(email, admin_address, subject, text, key=gpg_key)
            if error:
                return "failure when sending mail to the user: %s" % (error,)
        else:
            data = pytis.data.dbtable('e_pytis_crypto_keys',
                                      ('key_id', 'name', 'username', 'fresh'),
                                      connection_data)
            row = pytis.data.Row((('fresh', pytis.data.bval(True),),))
            condition = pytis.data.AND(pytis.data.EQ('name', pytis.data.sval(area)),
                                       pytis.data.EQ('username', pytis.data.sval(user)))
            result = data.update_many(condition, row, transaction=transaction_)
            if result != 1:
                return "couldn't update fresh column for the new user key"
            data.close()
        if transaction is None:
            transaction_.commit()
        transaction_ = None
    finally:
        if transaction is None and transaction_ is not None:
            transaction_.rollback()
