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

"""Drobné doplňkové funkce.

Do tohoto modulu patří funkce, které nemají s Pytisem jako takovým nic
společného.  Nepracují přímo s jeho API, jen se zkrátka v defsech mohou nějak
hodit.

"""

from pytis.extensions import *

import re

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
	return ("Špatný fromát telefoního čísla %s.\n\n"
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
    

def send_mail(to, address, subject, msg, sendmail_command='/usr/lib/sendmail',
              html=False, key=None, gpg_options=('--always-trust',)):
    """Odešle jeden email s možností kryptování pomocí gpg/pgp klíče."""
   
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
    # Ošetření případného použití UNICODE
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
        # Přidání částí do main
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

