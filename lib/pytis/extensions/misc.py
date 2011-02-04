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

"""Drobn� dopl�kov� funkce.

Do tohoto modulu pat�� funkce, kter� nemaj� s Pytisem jako takov�m nic
spole�n�ho.  Nepracuj� p��mo s jeho API, jen se zkr�tka v defsech mohou n�jak
hodit.

"""

from pytis.extensions import *

import re
import types

def smssend(tel, message, server='192.168.1.55'):
    import os, os.path, commands

    SERVER=server
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
	return ("�patn� form�t telefon�ho ��sla %s.\n\n"
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


# TODO: this procedure is obsolete and superceeded with send_mail and should be removed
#       after checking its use in applications
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

# TODO: argument sendmail_command should be removed when all applications reflect this change    
def send_mail(to, address, subject, msg, html=False, key=None, charset='ISO-8859-2',
              sendmail_command=None):
    """Ode�le jeden email s mo�nost� kryptov�n� pomoc� gpg/pgp kl��e."""
    assert isinstance(to, types.StringTypes)
    assert isinstance(address, types.StringTypes)
    assert isinstance(subject, types.StringTypes)
    assert isinstance(msg, types.StringTypes)
    def get_utf8_argument(arg):
        if isinstance(arg, types.StringType):
            try:
                arg = unicode(arg, charset)
            except:
                raise ProgramError("Cannot convert argument to unicode for charset %s" % (charset))
        return arg.encode('UTF-8')
    # P�eveden� na UTF-8
    to = get_utf8_argument(to)
    address = get_utf8_argument(address)
    subject = get_utf8_argument(subject)
    msg = get_utf8_argument(msg)
    if key is not None:
        # Budeme vytv��et kryptovan� email
        from pytis.extensions.email_ import GPGEmail
        mail = GPGEmail(to, address, subject, msg, html=html, key=key, charset='UTF-8')
    else:    
        # Budeme vytv��et jednoduch� nekryptovan� mail
        from pytis.extensions.email_ import SimpleEmail
        mail = SimpleEmail(to, address, subject, msg, html=html, charset='UTF-8')
    result = mail.send()    
    return result


# Additional constraints
            
def constraints_email(email):
    """Ov�� platnost z�pisu e-mailov� adresy.
    
    Pokud m� adresa platn� tvar, nebo je None vrac� None.  Jinak vrac� �et�zec
    s chybovou hl�kou

    """
    if email is None:
        return None
    mask=re.compile(r"^[A-Za-z0-9\d]([\w\d\.\-]?[A-Za-z0-9\_\&\.\-\d])*\@[A-Za-z0-9\d]([\w\d\.\-]?[A-Za-z0-9\_\&\d])*$")
    if mask.match(email.strip()) is None:
        return "�patn� tvar emailu " + email.strip()  + " !"
    return None

def constraints_email_many(emails):
    """Ov�� platnost z�pisu seznamu e=mailov�ch adres odd�len�ch ��rkami.

    Pokud m� seznam i ka�d� adresa platn� tvar, nebo je None funkce vrac� None.
    Jinak vrac� �et�zec s chybovou hl�kou.

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

# N�kter� �asto pou��van� konfigura�n� parametry.

def session_date(transaction=None):
    """Vra� vnit�n� hodnotu nastaven�ho pracovn�ho datumu."""
    return session_date_value().value()

def session_date_value(transaction=None):
    """Vra� nastaven� pracovn� datum p�ihl�en�ho u�ivatele."""
    return cfg_param('datum', 'Nastaveni.BvUsersCfg')

def start_date(transaction=None):
    """Vra� vnit�n� hodnotu nastaven�ho 'datumu od'."""
    return start_date_value().value()

def start_date_value(transaction=None):
    """Vra� nastaven� 'datum od' p�ihl�en�ho u�ivatele."""
    return cfg_param('datum_od', 'Nastaveni.BvUsersCfg')

def end_date(transaction=None):
    """Vra� vnit�n� hodnotu nastaven�ho 'datumu do'."""
    return end_date_value().value()

def end_date_value(transaction=None):
    """Vra� nastaven� 'datum do' p�ihl�en�ho u�ivatele."""
    return cfg_param('datum_do', 'Nastaveni.BvUsersCfg')

