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

"""Funkce a třídy pro práci s emailem.

Do tohoto modulu patří pomocné funkce a třídy pro práci s email zprávami.
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
    """Třída pro vytvoření a odeslaní jednoduchého mailu."""

    DEFAULT_CHARSET = 'iso-8859-2'
    DEFAULT_CONTENT_TYPE = 'text/html'

    def __init__(self, to, from_, subject, content,
                 bcc=None,  smtp='localhost'):
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
        self.msg['From'] = from_
        self.msg['To'] = to        
        self.msg['Subject'] = subject
        if self.bcc:
            bcc = get_header(self._flatten_for_header(self.bcc))
            self.msg['Bcc'] = bcc                    

    def create_content(self):
        self.msg.set_payload(self.content)        

    def get_message(self):
        self.create_message()
        return self.msg

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
            server.sendmail(self.from_, self.to, message)
            success = True
        finally:    
            server.quit()
        return success              


class ComplexEmail(SimpleEmail):
    """Třída pro vytvoření a odeslaní mailu s přílohami."""

    def __init__(self, to, from_, subject, content,
                 bcc=None,  smtp='localhost',
                 default_charset='iso-8859-2'):
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
        """Přidá data jako přílohu emailu."""
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
        for part in self.parts:
            self.msg.attach(part)
