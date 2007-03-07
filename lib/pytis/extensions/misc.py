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
    

class SimpleEmail(object):
    """T��da pro vytvo�en� a odeslan� jednoduch�ho mailu."""

    DEFAULT_CHARSET = 'iso-8859-2'
    DEFAULT_CONTENT_TYPE = 'text/html'

    def __init__(self, to, from_, subject, content,
                 bcc=None,  smtp='localhost'):
        """Inicializuj instanci.

        Argumenty:

          to -- adresa p��jemce nebo sekvence p��jemc�
          from_ -- adresa odes�latele
          subject -- p�edm�t zpr�vy (m��e obsahovat bu� ascii �et�zec
            nebo unicode �et�zec nebo �et�zec v k�dov�n� iso-8859-2)
          content -- obsah zpr�vy
          bcc -- adresa p��jemce pro bcc nebo sekvence adres
          smtp -- adresa odes�lac�ho serveru
        """  
          
        assert isinstance(to, types.StringTypes) or \
               isinstance(to, types.TupleType) or \
               isinstance(to, types.ListType) 
        assert bcc is None or \
               isinstance(bcc, types.StringTypes) or \
               isinstance(bcc, types.TupleType) or \
               isinstance(bcc, types.ListType) 
        assert isinstance(from_, types.StringTypes)
        assert isinstance(subject, types.StringTypes)
        assert isinstance(subject, types.StringTypes)
        self.to = to
        self.from_ = from_
        self.bcc = bcc
        self.subject = subject
        self.content = content
        self.smtp = smtp

    def create_message(self):
        """Return instance of email.Message."""
        from email.Message import Message
        self.msg = Message()
        self.create_headers()
        self.create_content()      

    def _flatten_for_header(self, header):
        if not isinstance(header, types.StringTypes):
            return ', '.join(header)
        else:
            return header
        
    def create_headers(self):
        from email.Header import Header
        from_ = Header(self._from, self.DEFAULT_CHARSET)
        _to = self._flatten_for_header(self.to)
        to = Header(_to, self.DEFAULT_CHARSET)        
        subject = Header(self.subject, self.DEFAULT_CHARSET)
        self.msg['From'] = from_
        self.msg['To'] = to        
        self.msg['Subject'] = subject
        if self.bcc:
            _bcc = self._flatten_for_header(self.bcc)
            bcc = Header(_bcc, self.DEFAULT_CHARSET)
            self.msg['Bcc'] = bcc                    

    def create_content(self):
        self.msg.set_payload(self.content)        

    def __str__(self):
        self.create_message()
        return self.msg.as_string()

    def send(self):
        self.create_message()
        message = self.msg.as_string()
        success = False
        try:
            import smtplib
            server = smtplib.SMTP(self.smtp)
            server.sendmail(self.to, self.address, nessage)
            success = True
        finally:    
            server.quit()
        return success              


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

def session_date(*args):
    """Vra� vnit�n� hodnotu nastaven�ho pracovn�ho datumu."""
    return session_date_value().value()

def session_date_value():
    """Vra� nastaven� pracovn� datum p�ihl�en�ho u�ivatele."""
    return cfg_param('datum', 'Nastaveni.BvUsersCfg')

def start_date(*args):
    """Vra� vnit�n� hodnotu nastaven�ho 'datumu od'."""
    return start_date_value().value()

def start_date_value():
    """Vra� nastaven� 'datum od' p�ihl�en�ho u�ivatele."""
    return cfg_param('datum_od', 'Nastaveni.BvUsersCfg')

def end_date(*args):
    """Vra� vnit�n� hodnotu nastaven�ho 'datumu do'."""
    return end_date_value().value()

def end_date_value():
    """Vra� nastaven� 'datum do' p�ihl�en�ho u�ivatele."""
    return cfg_param('datum_do', 'Nastaveni.BvUsersCfg')

