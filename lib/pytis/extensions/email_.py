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

"""Funkce a t��dy pro pr�ci s emailem.

Do tohoto modulu pat�� pomocn� funkce a t��dy pro pr�ci s email zpr�vami.
"""

import sys
import os
import mimetypes
import email
from email import Encoders
from email.Message import Message
from email.MIMEAudio import MIMEAudio
from email.MIMEBase import MIMEBase
from email.MIMEImage import MIMEImage
from email.MIMEMultipart import MIMEMultipart
from email.MIMEText import MIMEText
from email.Header import Header

class SimpleEmail(object):
    """T��da pro vytvo�en� a odeslan� jednoduch�ho mailu."""

    DEFAULT_CHARSET = 'iso-8859-2'
    DEFAULT_CONTENT_TYPE = 'text/html'

    ERR_CONNECTION = _("Could not connect to SMTP server")
    ERR_RECIPIENT = _("Recipient refused")
    ERR_SENDER = _("Sender refused")
    ERR_DISCONNECTED = _("SMTP server disconnected")
    ERR_DATA = _("Error by sending data")
    ERR_HELO = _("Error by sending helo")
    
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
        self._error_msg = None

    def create_message(self):
        self._create_message()
        self.create_headers()
        self.create_content()      

    def _create_message(self):
        """Return basic instance of Message."""
        self.msg = Message()

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
            if isinstance(header, (str, unicode)):
                try:
                    test = unicode(header, 'us-ascii')
                    make_header = False
                except:
                    make_header = True
            else:
                make_header = True
            if make_header:
                return Header(header, self.DEFAULT_CHARSET)
            else:
                return header
        from_ = get_header(self.from_)
        to = get_header(self._flatten_for_header(self.to))
        subject = get_header(self.subject)
        date = email.Utils.formatdate()
        self.msg['From'] = from_
        self.msg['To'] = to        
        self.msg['Subject'] = subject
        self.msg['Date'] = date

    def create_content(self):
        self.msg.set_payload(self.content)        

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
        except smtplib.SMTPConnectError:
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


class ComplexEmail(SimpleEmail):
    """T��da pro vytvo�en� a odeslan� mailu s p��lohami."""

    def __init__(self, to, from_, subject, content,
                 bcc=None,  smtp='localhost',
                 default_charset='iso-8859-2'):
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
        super(ComplexEmail, self).__init__(to, from_, subject, content,
                                           bcc=bcc, smtp=smtp)
        self.default_charset = default_charset
        self.parts = []
        if content:
            part = MIMEText(content, _charset=default_charset)
            self.parts.append(part)
        

    def _create_message(self):
        self.msg = MIMEMultipart()
        
    def create_headers(self):
        super(ComplexEmail, self).create_headers()
        self.msg.preamble = ('You will not see this '
                             'in a MIME-aware mail reader.\n')

    def add_content_data(self, data, filename, charset=None):
        """P�id� data jako p��lohu emailu."""
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
            if not charset: charset = self.default_charset
            content = MIMEText(data, _subtype=subtype,
                               _charset=charset)
        elif maintype == 'image':
            content = MIMEImage(data, _subtype=subtype)
        elif maintype == 'audio':
            content = MIMEAudio(data, _subtype=subtype)
        else:
            content = MIMEBase(maintype, subtype)
            content.set_payload(data)
            # Encode the payload using Base64
            Encoders.encode_base64(content)
        # Set the filename parameter
        content.add_header('Content-Disposition', 'attachment',
                           filename=filename)
        self.parts.append(content)


    def add_content_file(self, path, charset=None):
        """P�ipoj� obsah souboru dan� cestou path."""
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
        for part in self.parts:
            self.msg.attach(part)
