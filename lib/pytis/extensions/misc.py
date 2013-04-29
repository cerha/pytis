# -*- coding: utf-8 -*-

# Copyright (C) 2002, 2003, 2005, 2006, 2011, 2012 Brailcom, o.p.s.
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

from pytis.util import translate as _

from pytis.extensions import *

import re
import types

def smssend(tel, message, server='192.168.1.55'):
    import os, os.path, commands

    SERVER=server
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
    if len(tel) not in (14,9):
	return ("Špatný formát telefoního čísla %s.\n\n"
	        "Číslo musí mít tvar:\nPPPPPxxxxxxxxx - (pět znaků předvolba "
                "- devět znaků číslo)\nxxxxxxxxx - (devět znaků číslo)") % (tel)
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


# TODO: this procedure is obsolete and superceeded with send_mail and should be removed
#       after checking its use in applications
def emailsend(to, address, subject, msg, sendmail_command, content_type=None):
    """Odešle email"""

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
            except:
                raise ProgramError("Cannot convert argument to unicode for charset %s" % (charset))
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
        for i in range (len (opts)):
            if opts[i].startswith ("Default "):
                opts[i] = "Dest " + opts[i][8:]
        open(self.lpoptions, "w").writelines (opts)

    def get(self):
        if not self.lpoptions:
            return None
        try:
            opts = open(self.lpoptions, 'r').readlines()
        except IOError:
            return None
        for i in range (len (opts)):
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
        p = subprocess.Popen([ "lpoptions", "-d", default ],
                             close_fds=True,
                             stdin=open("/dev/null", 'r'),
                             stdout=open("/dev/null", 'w'),
                             stderr=subprocess.PIPE)
        (stdout, stderr) = p.communicate()
        exitcode = p.wait()
        if exitcode != 0:
            raise ProgramError(stderr.strip())
        return

    def __repr__ (self):
        return "<UserDefaultPrinter (%s)>" % repr (self.get())
    
def set_default_printer():
    try:
        import cups
        import cupshelpers
    except ImportError as e:
        pytis.form.run_dialog(pytis.form.Error, _("Nastavení výchozí tiskárny nemůže být provedeno.\n"
                                                  "Kontaktujte správce systému."))
        return None
    connection = cups.Connection()
    user_default = UserDefaultPrinter()
    default_printer = user_default.get()
    if not default_printer:        
        default_printer = connection.getDefault()
    printers = cupshelpers.getPrinters(connection)
    printer_names = printers.keys()
    fields = (Field('printer', _(""), 
                    width=40, not_null=True,
                    type=pytis.data.String,
                    enumerator=pytis.data.FixedEnumerator(printer_names),
                    default=default_printer,
                    ),
              )
    layout = (pytis.form.Text(_("Zvolte výchozí tiskárnu:")), 'printer')
    result = pytis.form.run_form(pytis.form.InputForm,
                                 title=_("Výběr tiskárny"),
                                 fields=fields,
                                 layout=layout)
    if result:
        user_default.set(result['printer'].value())
    return None

cmd_set_default_printer = (pytis.form.Application.COMMAND_HANDLED_ACTION,
                           dict(handler=set_default_printer))

# Additional constraints
            
def constraints_email(email):
    """Ověř platnost zápisu e-mailové adresy.
    
    Pokud má adresa platný tvar, nebo je None vrací None.  Jinak vrací řetězec
    s chybovou hláškou

    """
    if email is None:
        return None
    mask=re.compile(r"^[A-Za-z0-9\d]([\w\d\.\-]?[A-Za-z0-9\_\&\.\-\d])*\@[A-Za-z0-9\d]([\w\d\.\-]?[A-Za-z0-9\_\&\d])*$")
    if mask.match(email.strip()) is None:
        return "Špatný tvar emailu " + email.strip()  + " !"
    return None

def constraints_email_many(emails):
    """Ověř platnost zápisu seznamu e=mailových adres oddělených čárkami.

    Pokud má seznam i každá adresa platný tvar, nebo je None funkce vrací None.
    Jinak vrací řetězec s chybovou hláškou.

    """
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

# Některé často používané konfigurační parametry.

def session_date(transaction=None):
    """Vrať vnitřní hodnotu nastaveného pracovního datumu."""
    return session_date_value().value()

def session_date_value(transaction=None):
    """Vrať nastavené pracovní datum přihlášeného uživatele."""
    return cfg_param('datum', 'Nastaveni.BvUsersCfg')

def start_date(transaction=None):
    """Vrať vnitřní hodnotu nastaveného 'datumu od'."""
    return start_date_value().value()

def start_date_value(transaction=None):
    """Vrať nastavené 'datum od' přihlášeného uživatele."""
    return cfg_param('datum_od', 'Nastaveni.BvUsersCfg')

def end_date(transaction=None):
    """Vrať vnitřní hodnotu nastaveného 'datumu do'."""
    return end_date_value().value()

def end_date_value(transaction=None):
    """Vrať nastavené 'datum do' přihlášeného uživatele."""
    return cfg_param('datum_do', 'Nastaveni.BvUsersCfg')

