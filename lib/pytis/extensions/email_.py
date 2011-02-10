# -*- coding: utf-8 -*-

# Copyright (C) 2002, 2003, 2005, 2006, 2011 Brailcom, o.p.s.
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

"""Funkce a třídy pro práci s emailem.

Do tohoto modulu patří pomocné funkce a třídy pro práci s email zprávami.
"""

import sys
import os
import mimetypes
import email
import types
from email import Encoders
from email.Message import Message
from email.MIMEAudio import MIMEAudio
from email.MIMEBase import MIMEBase
from email.MIMEImage import MIMEImage
from email.MIMEMultipart import MIMEMultipart
from email.MIMEText import MIMEText
from email.Header import Header

class SimpleEmail(object):
    """Třída pro vytvoření a odeslaní jednoduchého mailu."""

    ERR_CONNECTION = _(u"Could not connect to SMTP server")
    ERR_RECIPIENT = _(u"Recipient refused")
    ERR_SENDER = _(u"Sender refused")
    ERR_DISCONNECTED = _(u"SMTP server disconnected")
    ERR_DATA = _(u"Error by sending data")
    ERR_HELO = _(u"Error by sending helo")
    
    def __init__(self, to, from_, subject, content, html=False,
                 bcc=None,  smtp='localhost', charset='iso-8859-2'):
        """Inicializuj instanci.

        Argumenty:

          to -- adresa příjemce nebo sekvence příjemců
          from_ -- adresa odesílatele
          subject -- předmět zprávy (může obsahovat buď ascii řetězec
            nebo unicode řetězec nebo řetězec v kódování iso-8859-2)
          content -- obsah zprávy
          bcc -- adresa příjemce pro bcc nebo sekvence adres
          smtp -- adresa odesílacího serveru
        """  

        assert isinstance(to, (str, unicode, tuple, list))
        assert bcc is None or isinstance(bcc, (str, unicode, tuple, list))
        assert isinstance(from_, (str, unicode))
        assert isinstance(subject, (str, unicode))
        self.to = to
        self.from_ = from_
        self.bcc = bcc
        self.subject = subject
        self.content = content
        self.smtp = smtp
        self.html = html
        self.charset = charset
        self._error_msg = None

    def create_message(self):
        self._create_message()
        self.create_headers()
        self.create_content()      

    def _create_message(self):
        """Return basic instance of Message."""
        self.msg = self.get_content_text(self.content, self.html, self.charset)

    def _flatten_for_header(self, header):
        if not isinstance(header, (str, unicode)):
            return ', '.join(header)
        else:
            return header

    def reset_error_msg(self):
        self._error_msg = None

    def get_error_msg(self):
        return self._error_msg
        
    def smtp_to(self):
        if self.bcc is None:
            return self.to
        else:
            to = self.to
            bcc = self.bcc
            if isinstance(to, (str, unicode)):
                to = [to]
            if isinstance(bcc, (str, unicode)):
                bcc = [bcc]                
            return to + bcc
        
    def create_headers(self):
        def get_header(header):
            if isinstance(header, types.StringType):
                # Not unicode
                try:
                    test = unicode(header, 'us-ascii')
                    make_header = False
                except:
                    make_header = True
                charset = self.charset
            else:
                # Unicode
                header = header.encode('UTF-8')
                charset = 'UTF-8'
                make_header = True
            if make_header:
                return Header(header, charset)
            else:
                return header
        from_ = get_header(self.from_)
        to = get_header(self._flatten_for_header(self.to))
        subject = get_header(self.subject)
        date = email.Utils.formatdate(localtime=1)
        self.msg['From'] = from_
        self.msg['To'] = to        
        self.msg['Subject'] = subject
        self.msg['Date'] = date

    def get_content_text(self, data, html=False, charset=None):
        if not charset:
            charset = self.charset
        if html:
            subtype = 'html'
        else:
            subtype = 'plain'
        return MIMEText(data, subtype, _charset=charset)
            
    def create_content(self):
        pass
        # mime_text = self.get_content_text(self.content, html=self.html, charset=self.charset)        
        # self.msg.attach(mime_text) 

    def get_message(self):
        self.create_message()
        return self.msg

    def __str__(self):
        self.create_message()
        return self.msg.as_string()

    def send(self):
        import smtplib
        self.create_message()
        message = self.msg.as_string()
        success = True
        self.reset_error_msg()
        try:
            server = smtplib.SMTP(self.smtp)
        except:
            self._error_msg = "%s: %s" % (self.ERR_CONNECTION, self.smtp)
            return False
        try:
            server.sendmail(self.from_, self.smtp_to(), message)
        except smtplib.SMTPRecipientsRefused:            
            self._error_msg = "%s: %s" % (self.ERR_RECIPIENT, self.smtp_to())
            success = False
        except smtplib.SMTPSenderRefused:
            self._error_msg = "%s: %s" % (self.ERR_SENDER, self.from_)
            success = False
        except smtplib.SMTPHeloError:            
            self._error_msg = self.ERR_HELO
            success = False
        except smtplib.SMTPDataError:
            self._error_msg = self.ERR_DATA
            success = False
        except smtplib.SMTPServerDisconnected:
            self._error_msg = self.ERR_DISCONNECTED
            success = False
        try:
            server.quit()
        except:
            pass
        return success              

class GPGEmail(SimpleEmail):
    """Třída pro vytvoření a odeslaní jednoduchého kryptovaného mailu."""

    ERR_GPG_MODULE = _(u"Could not import Python GnuPG module")
    ERR_GPG_INSTANCE = _(u"Could not create GPG instance")
    ERR_GPG_KEYRING = _(u"Could not create a temporary GPG keyring")
    ERR_GPG_OUTPUT = _(u"GPG process did not return string.")
    
    def __init__(self, to, from_, subject, content, key, html=False, 
                 smtp='localhost', charset='iso-8859-2'):
        """Inicializuj instanci.
        
          to -- adresa příjemce (zatím podporujeme jen jednoho příjemce)
          from_ -- adresa odesílatele
          subject -- předmět zprávy (může obsahovat buď řetězec nebo unicode
          content -- vlastní obsah zprávy
          key -- veřejný klíč, kterým má být zpráva zakryptována
          html -- příznak, zda obsah je html
          smtp -- adresa odesílacího serveru
          charset -- v případě, že content není v unicode, je zde uveden charset pro content
        """
        super(GPGEmail, self).__init__(to, from_, subject, content, html=html,
                                           smtp=smtp, charset=charset)
        self.key = key

    def _create_message(self):
        """Return basic instance of Message."""
        self.msg = Message()
        
    def _setup_gpg(self):
        "Setup GPG process. Returns initialized gpg instance."
        try:
            import tempfile, GnuPGInterface
        except:
            self._error_msg = self.ERR_GPG_MODULE
            return None
        try:
            gpg = GnuPGInterface.GnuPG()
        except:
            self._error_msg = self.ERR_GPG_INSTANCE
            return None
        try:
            keyring = tempfile.mkstemp()[1]
        except:
            self._error_msg = self.ERR_GPG_KEYRING
            return None
        gpg.options.armor = 1
        gpg.options.meta_interactive = 0
        gpg.options.extra_args.append('--no-secmem-warning')
        for o in ('--always-trust', '--no-default-keyring', '--keyring=%s' % keyring):
            gpg.options.extra_args.append(o)
        proc = gpg.run(['--import'], create_fhs=['stdin', 'stderr'])
        proc.handles['stdin'].write(self.key)
        proc.handles['stdin'].close()
        out = proc.handles['stderr'].read()
        proc.handles['stderr'].close()
        proc.wait()
        return gpg
    
    def _gpg_encrypt_content(self):
        "Encrypt the content."
        gpg = self._setup_gpg()
        if not gpg:
            raise ProgramError(self._error_msg)
        if isinstance(self.to, types.StringType):
            to = (self.to,)
        elif isinstance(self.to, types.UnicodeType):
            # Unicode arguments are problem for GPG process, so we will convert them to UTF-8
            to = []
            for t in self.to:
                to.append(t.encode('UTF-8'))
        else:
            to = self.to
        content = self.get_content_text(self.content, html=self.html,
                                                   charset=self.charset)
        gpg.options.recipients = to   # a list or tuple!        
        proc = gpg.run(['--encrypt'], create_fhs=['stdin', 'stdout'])        
        proc.handles['stdin'].write(content.as_string())
        proc.handles['stdin'].close()
        output = proc.handles['stdout'].read()
        proc.handles['stdout'].close()        
        proc.wait()
        success = True
        if not isinstance(output, types.StringType):
            success = False
        try:
            os.remove(keyring)
        except:
            pass
        if not success:
            raise ProgramError(self.ERR_GPG_OUTPUT)
        else:
            return output    

    def create_headers(self):
        super(GPGEmail, self).create_headers()
        self.msg["Mime-version"]="1.0"
        self.msg["Content-type"]="Multipart/encrypted"
        self.msg["Content-transfer-encoding"]="8bit"
        self.msg.preamble="This is an OpenPGP/MIME encrypted message (RFC 2440 and 3156)"       
       
    def create_content(self):
        # Part 1
        firstSubMsg=email.Message.Message()
        firstSubMsg["Content-Type"]="application/pgp-encrypted"
        firstSubMsg["Content-Description"]="PGP/MIME version identification"
        firstSubMsg.set_payload("Version: 1\n")
        # Part 2
        if self.html:
            filename = 'content.html.pgp'
        else:
            filename = 'content.txt.pgp'            
        encrypted = self._gpg_encrypt_content()
        secondSubMsg=email.Message.Message()
        secondSubMsg.add_header("Content-Type", "application/octet-stream",
                                name=filename)
        secondSubMsg.add_header("Content-Description",
                                "OpenPGP encrypted message")
        secondSubMsg.add_header("Content-Disposition", "inline",
                                filename=filename)
        secondSubMsg.set_payload(encrypted)
        # Přidání částí do main
        self.msg.attach(firstSubMsg)
        self.msg.attach(secondSubMsg)
        
class ComplexEmail(SimpleEmail):
    """Třída pro vytvoření a odeslaní mailu s přílohami."""

    def __init__(self, to, from_, subject, content=None, html=False, bcc=None,  smtp='localhost',
                 charset='iso-8859-2'):
        """Inicializuj instanci.

        Argumenty:

          to -- adresa příjemce nebo sekvence příjemců
          from_ -- adresa odesílatele
          subject -- předmět zprávy (může obsahovat buď řetězec nebo unicode
          content -- obsah zprávy
          html -- indikace, zda je obsah content v html podobě
          bcc -- adresa příjemce pro bcc nebo sekvence adres
          smtp -- adresa odesílacího serveru
        """  
        super(ComplexEmail, self).__init__(to, from_, subject, content, html=html,
                                           bcc=bcc, smtp=smtp, charset=charset)
        self.parts = []        

    def _create_message(self):
        self.msg = MIMEMultipart()
        
    def create_headers(self):
        super(ComplexEmail, self).create_headers()
        self.msg.preamble = ('You will not see this '
                             'in a MIME-aware mail reader.\n')

    def get_content_text(self, data, html=False, charset=None):
        if not charset:
            charset = self.charset
        if html:
            subtype = 'html'
        else:
            subtype = 'plain'
        return MIMEText(data, subtype, _charset=charset)
        
    def get_content_data(self, data, filename, charset=None):
        "Vrátí data jako instanci třídy odvozené od MIMEBase."
        # Guess the content type based on the file's extension.  Encoding
        # will be ignored, although we should check for simple things like
        # gzip'd or compressed files.
        ctype, encoding = mimetypes.guess_type(filename)
        if ctype is None or encoding is not None:
            # No guess could be made, or the file is encoded (compressed), so
            # use a generic bag-of-bits type.
            ctype = 'application/octet-stream'
        maintype, subtype = ctype.split('/', 1)
        if maintype == 'text':
            # Note: we should handle calculating the charset
            if not charset: charset = self.charset
            content = MIMEText(data, _subtype=subtype,
                               _charset=charset)
        elif maintype == 'image':
            content = MIMEImage(data, _subtype=subtype)
        elif maintype == 'audio':
            content = MIMEAudio(data, _subtype=subtype)
        else:
            content = MIMEBase(maintype, subtype)
        return content

    def add_content_text(self, data, html=False, charset=None):
        """Přidá text nebo html jako MIME část."""
        content = self.get_content_text(data, html=html, charset=charset)
        self.parts.append(content)        
    
    def add_content_data(self, data, filename, charset=None):
        """Přidá data jako přílohu emailu."""
        content = self.get_content_data(data, filename, charset=charset)
        # Set the filename parameter
        content.add_header('Content-Disposition', 'attachment',
                           filename=filename)
        self.parts.append(content)

    def add_content_file(self, path, charset=None):
        """Připojí obsah souboru daný cestou path."""
        if not os.path.isfile(path):
            return False
        directory, filename = os.path.split(path)
        # Guess the content type based on the file's extension.  Encoding
        # will be ignored, although we should check for simple things like
        # gzip'd or compressed files.
        ctype, encoding = mimetypes.guess_type(path)        
        if ctype is None or encoding is not None:
            # No guess could be made, or the file is encoded (compressed), so
            # use a generic bag-of-bits type.
            ctype = 'application/octet-stream'
        maintype, subtype = ctype.split('/', 1)
        if maintype == 'text':
            mode = 'r'
        else:
            mode = 'rb'
        try:    
            fp = open(path)
            data = fp.read()
            fp.close()
        except:
            data = None
        if data:    
            self.add_content_data(data, filename, charset)
        return    
    
    def create_content(self):
        if self.content:
            self.msg.attach(self.get_content_text(self.content, html=self.html,
                                                   charset=self.charset)) 
        for part in self.parts:
            self.msg.attach(part)
