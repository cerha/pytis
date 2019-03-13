# -*- coding: utf-8 -*-

# Copyright (C) 2018, 2019 Tomáš Cerha <t.cerha@gmail.com>
# Copyright (C) 2002-2013 Brailcom, o.p.s.
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

import os
import mimetypes
import email
import pytis.util
from email.Message import Message
from email.MIMEAudio import MIMEAudio
from email.MIMEBase import MIMEBase
from email.MIMEImage import MIMEImage
from email.MIMEMultipart import MIMEMultipart
from email.MIMEText import MIMEText
from email.Header import Header

_ = pytis.util.translations('pytis-wx')


class SimpleEmail(object):
    """Třída pro vytvoření a odeslaní jednoduchého mailu."""

    ERR_CONNECTION = _("Could not connect to SMTP server")
    ERR_RECIPIENT = _("Recipient address refused")
    ERR_SENDER = _("Sender address refused")
    ERR_DISCONNECTED = _("SMTP server disconnected")
    ERR_DATA = _("Error by sending data")
    ERR_HELO = _("Error by sending helo")

    def __init__(self, to, from_, subject, content, html=False,
                 bcc=None, replyto=None, smtp='localhost', charset='iso-8859-2'):
        """Inicializuj instanci.

        Argumenty:

          to -- adresa příjemce nebo sekvence příjemců
          from_ -- adresa odesílatele
          subject -- předmět zprávy (může obsahovat buď ascii řetězec
            nebo unicode řetězec nebo řetězec v kódování iso-8859-2)
          content -- obsah zprávy
          bcc -- adresa příjemce pro bcc nebo sekvence adres
          replyto -- adresa odesilatele pro zaslání odpovědi
          smtp -- adresa odesílacího serveru

        """
        assert isinstance(to, (basestring, tuple, list))
        assert bcc is None or isinstance(bcc, (basestring, tuple, list))
        assert isinstance(from_, basestring)
        assert isinstance(subject, basestring)
        self.to = to
        self.from_ = from_
        self.bcc = bcc
        self.replyto = replyto
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
        if not isinstance(header, basestring):
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
            if isinstance(to, basestring):
                to = [to]
            if isinstance(bcc, basestring):
                bcc = [bcc]
            return to + bcc

    def create_headers(self):
        def get_header(header):
            if isinstance(header, basestring):
                # Not unicode
                try:
                    test = unicode(header, 'us-ascii')
                    test  # to make flake8 happy
                    make_header = False
                except Exception:
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
        if self.replyto:
            self.msg['Reply-to'] = get_header(self.replyto)
            # replyto = get_header(self.replyto)
            # self.msg.add_header('reply-to', replyto)

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
        except Exception:
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
        except Exception:
            pass
        return success


class GPGEmail(SimpleEmail):
    """Třída pro vytvoření a odeslaní jednoduchého kryptovaného mailu."""

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

    def _gpg_encrypt_content(self):
        import gnupg
        import tempfile
        keyring = tempfile.mkstemp()[1]
        options = ('--no-secmem-warning', '--always-trust', '--no-default-keyring',
                   '--keyring=%s' % keyring)
        gpg = gnupg.GPG(options=options)
        import_result = gpg.import_keys(self.key)
        fingerprints = import_result.fingerprints
        content = self.get_content_text(self.content, html=self.html, charset=self.charset)
        encryption_result = gpg.encrypt(str(content), fingerprints[0])
        if not encryption_result.ok:
            raise pytis.util.ProgramError(encryption_result.stderr)
        # BUG: There is no `keyring' defined here so the following
        # statement is effectively void:
        try:
            os.remove(keyring)
        except Exception:
            pass
        return str(encryption_result)

    def create_headers(self):
        super(GPGEmail, self).create_headers()
        self.msg["Mime-version"] = "1.0"
        self.msg["Content-type"] = "Multipart/encrypted"
        self.msg["Content-transfer-encoding"] = "8bit"
        self.msg.preamble = "This is an OpenPGP/MIME encrypted message (RFC 2440 and 3156)"

    def create_content(self):
        # Part 1
        firstSubMsg = email.Message.Message()
        firstSubMsg["Content-Type"] = "application/pgp-encrypted"
        firstSubMsg["Content-Description"] = "PGP/MIME version identification"
        firstSubMsg.set_payload("Version: 1\n")
        # Part 2
        if self.html:
            filename = 'content.html.pgp'
        else:
            filename = 'content.txt.pgp'
        encrypted = self._gpg_encrypt_content()
        secondSubMsg = email.Message.Message()
        secondSubMsg.add_header("Content-Type", "application/octet-stream",
                                name=filename)
        secondSubMsg.add_header("Content-Description",
                                "OpenPGP encrypted message")
        secondSubMsg.add_header("Content-Disposition", "attachment",
                                filename=filename)
        secondSubMsg.set_payload(encrypted)
        # Přidání částí do main
        self.msg.attach(firstSubMsg)
        self.msg.attach(secondSubMsg)


class ComplexEmail(SimpleEmail):
    """Třída pro vytvoření a odeslaní mailu s přílohami."""

    def __init__(self, to, from_, subject, content=None, html=False, bcc=None,
                 replyto=None, smtp='localhost', charset='iso-8859-2'):
        """Inicializuj instanci.

        Argumenty:

          to -- adresa příjemce nebo sekvence příjemců
          from_ -- adresa odesílatele
          subject -- předmět zprávy (může obsahovat buď řetězec nebo unicode
          content -- obsah zprávy
          html -- indikace, zda je obsah content v html podobě
          bcc -- adresa příjemce pro bcc nebo sekvence adres
          replyto -- adresa odesilatele pro zaslání odpovědi
          smtp -- adresa odesílacího serveru

        """
        super(ComplexEmail, self).__init__(to, from_, subject, content, html=html,
                                           bcc=bcc, replyto=replyto, smtp=smtp, charset=charset)
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
            if not charset:
                charset = self.charset
            content = MIMEText(data, _subtype=subtype, _charset=charset)
        elif maintype == 'image':
            content = MIMEImage(data, _subtype=subtype)
        elif maintype == 'audio':
            content = MIMEAudio(data, _subtype=subtype)
        else:
            content = MIMEBase(maintype, subtype)
            content.set_payload(data)
            email.Encoders.encode_base64(content)
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
            fp = open(path, mode)
            data = fp.read()
            fp.close()
        except Exception:
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


if __name__ == '__main__':
    import sys
    try:
        sender, recipient, subject, text, key_filename, smtp = sys.argv[1:]
    except ValueError:
        sys.stderr.write("Sends GPG mail to given e-mail address encrypted by given public key.\n")
        sys.stderr.write("Arguments: from to subject message-text key-filename smtp-server\n")
        sys.exit(1)
    mail = GPGEmail(recipient, sender, subject, text,
                    open(key_filename).read(),
                    html=False,
                    charset='UTF8',
                    smtp=smtp)
    if mail.send():
        print "Sent ok"
    else:
        print mail.get_error_msg()
