# -*- coding: utf-8 -*-

# Copyright (C) 2018-2019 Tomáš Cerha <t.cerha@gmail.com>
# Copyright (C) 2001-2014 Brailcom, o.p.s.
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

"""Logování.

Logování slouží k zaznamenávání následujících informací:

- Informace pro administrátora o stavu systému.

- Záznam uživatelských akcí pro případnou pozdější diagnostiku problémů.

- Změny všech dat.

- Ladící informace potřebné pouze během vývoje programu, které se nikdy
  nelogují za ostrého běhu aplikace.

Modul umožňuje zaznamenávání těchto informací prostřednictvím funkce 'log'.
Každé volání této funkce je definováno svým typem, slovním popisem události a
nepovinně libovolným datovým objektem, který obsahuje data vztahující se k dané
informaci.

Co se druhů informací týče, modul definuje konstantu pro každý typ informace
z výše uvedeného seznamu.

Konfigurace modulu je dána proměnnou 'config'.

Tento modul je doporučeno importovat následujícím způsobem:

  from pytis.util import *

"""

import getpass
import inspect
import os
import re
import socket
import string
import sys
import time

import pytis
from .util import ProgramError, deepstr, positive_id, some

OPERATIONAL = 'OPR'
"""Provozní hláška, související se stavem systému."""
ACTION = 'ACT'
"""Významná hláška týkající se změny uživatelského rozhraní nebo dat."""
EVENT = 'EVT'
"""Méně významná hláška týkající se uživatelské akce nebo dat."""
DEBUG = 'DBG'
"""Interní hláška pro ladění, neloguje se při ostrém spuštění aplikace."""


class Logger(object):
    """Abstraktní třída pro logování.

    Třída obsahuje jedinou veřejnou metodu 'log()', prostřednictvím které je
    možno logování kompletně obsloužit.

    Tato třída zajišťuje potřebnou logovací infrastrukturu, neposílá však
    hlášky na žádný výstup.  Zasílání hlášek do konkrétních cílů je záležitostí
    potomků třídy.

    Pro obecné logování je lépe nevyužívat tuto třídu nebo její potomky přímo,
    nýbrž použít funkci 'log.log()'.

    """

    def __init__(self):
        """Inicializuj logger."""
        self._host = socket.gethostname()
        self._database = pytis.config.dbname
        self._module_filter = pytis.config.log_module_filter
        try:
            class_ = list(pytis.config.log_class_filter)
            for i in range(len(class_)):
                c = class_[i]
                if isinstance(c, basestring):
                    pos = string.rfind(c, '.')
                    if pos:
                        mod, cls = c[:pos], c[pos + 1:]
                        imp = __import__(mod, None, None, [cls])
                        class_[i] = imp.__dict__[cls]
                    else:
                        class_[i] = eval(c)
            self._class_filter = class_
        except Exception:
            self._class_filter = ()

    def _retrieve_info(self):
        module = class_name = id_ = '?'
        class_ = None
        frame = inspect.currentframe().f_back.f_back.f_back
        if frame:
            if __debug__ and False:
                # TODO/Python: Zjišťování jména modulu je velmi pomalé (chyba
                # modulu `inspect').
                try:
                    # TODO: Z neznámého důvodu od jisté doby nefunguje zjištění
                    # modulu pro pytis.form.
                    module = inspect.getmodule(frame).__name__
                except Exception:
                    pass
            f_locals = frame.f_locals
            if 'self' in f_locals:
                s = f_locals['self']
                try:
                    class_ = s.__class__
                    class_name = class_.__name__
                except Exception:
                    pass
                id_ = '%x' % (positive_id(s),)
        self._module = module
        self._class_ = class_
        self._class_name = class_name
        self._id = id_

    def _is_accepted(self, kind, message, data):
        if not __debug__ and kind == DEBUG:
            return False
        if kind in pytis.config.log_exclude:
            return False
        if kind == DEBUG:
            if self._module_filter and not self._module.startswith(self._module_filter):
                return False
            if ((self._class_filter and
                 (self._class_ is None or
                  not some(lambda c: issubclass(self._class_, c), self._class_filter)))):
                return False
        return True

    def _prefix(self, kind, message, data):
        datetime = time.strftime('%Y-%m-%d %H:%M:%S',
                                 time.gmtime(time.time()))
        host = self._host
        user = getpass.getuser()
        pid = os.getpid()
        prefix = u'%s %s@%s/%s[%s] %s %s %s[%s]: ' % \
                 (datetime, user, host, self._database, pid, kind, self._module,
                  self._class_name, self._id)
        return prefix

    def _formatted_message(self, message, data):
        return message

    def _formatted_data(self, prefix, fmessage, data):
        def escape(text):
            return re.sub(r'[^\x01-\x7F]', '?', text)
        if data is not None:
            printable = deepstr(data)
            datalines = [u'%s%s' % (prefix, l) for l in string.split(printable, '\n')]
            n = len(datalines)
            if n <= 1:
                if fmessage and fmessage[-1] == ':':
                    if pytis.config.log_one_line_preferred:
                        try:
                            formatted = u'*%s%s %s' % (prefix, fmessage, printable)
                        except UnicodeDecodeError:
                            formatted = u'*%s%s %s' % (prefix, escape(fmessage), escape(printable))
                        return formatted
                data_string = u'=%s%s' % (prefix, printable)
            else:
                datalines[0] = u'<%s' % datalines[0]
                datalines[n - 1] = u'>%s' % datalines[n - 1]
                datalines[1:n - 1] = [u' %s' % l for l in datalines[1:n - 1]]
                data_string = string.join(datalines, '\n')
            try:
                formatted = u'@%s%s\n%s' % (prefix, fmessage, data_string)
            except UnicodeDecodeError:
                formatted = u'@%s%s\n%s' % (prefix, escape(fmessage), escape(data_string))
        else:
            try:
                formatted = u'*%s%s' % (prefix, fmessage)
            except UnicodeDecodeError:
                formatted = u'*%s%s' % (prefix, escape(fmessage))
        return formatted

    def _formatted(self, kind, message, data):
        prefix = self._prefix(kind, message, data)
        fmessage = self._formatted_message(message, data)
        formatted = self._formatted_data(prefix, fmessage, data)
        return formatted

    def _send(self, kind, formatted):
        pass

    def log(self, kind, message, data=None):
        """Zaloguj 'message'.

        Argumenty:

          kind -- druh hlášky, jedna z konstant modulu 'log'
          message -- slovní hláška, string; jestliže tento string končí
            dvojtečkou, 'data' mají jednořádkovou reprezentaci a konfigurační
            volba 'one_line_preferred' je pravda, jsou data do logu zapsána na
            stejný řádek jako 'message'
          data -- libovolná data; tento argument není nutno klíčovat

        """
        assert kind in (OPERATIONAL, ACTION, EVENT, DEBUG), \
            ('invalid logging kind', kind)
        assert isinstance(message, basestring)
        self._retrieve_info()
        if not self._is_accepted(kind, message, data):
            return
        formatted = self._formatted(kind, message, data)
        self._send(kind, formatted)


class StreamLogger(Logger):
    """Logger posílající hlášení do streamu.

    Cílový stream je třídě předán v konstruktoru.  Třída není zodpovědná za
    hlídání chyb streamu ani neprovádí žádné akce v případě, že stream je vně
    třídy uzavřen.

    """

    def __init__(self, stream):
        """Inicializuj logování.

        Argumenty:

          stream -- otevřený file object, do kterého lze logovat

        """
        super(StreamLogger, self).__init__()
        self._stream = stream

    def _send(self, kind, formatted):
        self._stream.write(formatted)
        self._stream.write('\n')
        self._stream.flush()


class SyslogLogger(Logger):

    """Logger posílající hlášení syslogu."""

    _MAX_MESSAGE_LENGTH = 1020

    def __init__(self, facility=None):
        super(SyslogLogger, self).__init__()
        self._facility = facility

    def _prefix(self, kind, message, data):
        pid = os.getpid()
        user = getpass.getuser()
        prefix = '[%s] %s %s %s %s %s[%s]: ' % \
                 (pid, self._database, user, kind, self._module, self._class_name, self._id)
        return prefix

    def _send(self, kind, formatted):
        import syslog
        if kind == OPERATIONAL:
            priority = syslog.LOG_ERR
        elif kind == ACTION:
            priority = syslog.LOG_NOTICE
        elif kind == EVENT:
            priority = syslog.LOG_INFO
        elif kind == DEBUG:
            priority = syslog.LOG_DEBUG
        else:
            raise ProgramError('Unknown message kind', kind)
        if self._facility is not None:
            priority = priority | self._facility
        while formatted:
            msg = formatted[:self._MAX_MESSAGE_LENGTH]
            if isinstance(msg, unicode):
                msg = msg.encode('utf-8')
            syslog.syslog(priority, msg)
            formatted = formatted[self._MAX_MESSAGE_LENGTH:]


###


class LoggingInterface:
    """Rozhraní ke standardnímu logovacímu objektu.

    Tato třída není určena k instanciaci mimo modul 'pytis.util.log'.

    """

    def __init__(self):
        self._logger = None  # nelze inicializovat teď, kvůli závislostem modulů
        self._hooks = []

    def __call__(self, kind, message, data=None):
        """Zaloguj 'message'.

        Argumenty:

          kind -- druh hlášky, jedna z konstant modulu
          message -- slovní hláška, string
          data -- libovolná data, tento argument není nutno klíčovat

        Pokud je '__debug__' nepravda a 'kind' je 'DEBUG', 'message' není
        zalogováno.

        """
        if __debug__ or kind is not DEBUG:  # optimalizační záležitost
            logger = self._logger
            if not logger:
                try:
                    cls, args, kwargs = pytis.config.log_logger
                except AttributeError:
                    cls, args, kwargs = (StreamLogger, (sys.stderr,), {})
                logger = self._logger = cls(*args, **kwargs)
            logger.log(kind, message, data)
        for hook in self._hooks:
            hook()

    def add_hook(self, hook):
        """Přidej 'hook' ke každému logování.

        Argumenty:

          hook -- funkce bez argumentů, která je zavolána při každém volání
            metody '__call__()', bez ohledu na to, zda nějaká zpráva byla
            skutečně zalogována

        Logovací hooky lze využít k opakovanému vykonání nějaké činnosti
        v kterékoliv části kódu.  Myšlenka vychází z toho, že veškerý kód by
        měl na všech důležitých místech, a také dostatečně často časově,
        logovat.  Navěšení volání něčeho na logování je pak nenásilnou metodou,
        jak zajistit opakované volání nějakého kódu i v případě, kdy je
        z jakéhokoliv důvodu nevhodné tak činit ve vedlejším threadu.

        """
        self._hooks.append(hook)


log = LoggingInterface()
