# -*- coding: iso-8859-2 -*-

# Copyright (C) 2001, 2002, 2004, 2005, 2006, 2007, 2011 Brailcom, o.p.s.
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

"""Logov�n�.

Logov�n� slou�� k�zaznamen�v�n� n�sleduj�c�ch informac�:

- Informace pro administr�tora o stavu syst�mu.

- Z�znam u�ivatelsk�ch akc� pro p��padnou pozd�j�� diagnostiku probl�m�.

- Zm�ny v�ech dat.

- Lad�c� informace pot�ebn� pouze b�hem v�voje programu, kter� se nikdy
  neloguj� za ostr�ho b�hu aplikace.

Modul umo��uje zaznamen�v�n� t�chto informac� prost�ednictv�m funkce 'log'.
Ka�d� vol�n� t�to funkce je definov�no sv�m typem, slovn�m popisem ud�losti a
nepovinn� libovoln�m datov�m objektem, kter� obsahuje data vztahuj�c� se k�dan�
informaci.

Co se druh� informac� t��e, modul definuje konstantu pro ka�d� typ informace
z�v��e uveden�ho seznamu.

Konfigurace modulu je d�na prom�nnou 'config'.

Tento modul je doporu�eno importovat n�sleduj�c�m zp�sobem:

  from pytis.util import *

"""

import getpass
import gettext
import inspect
import os
import socket
import string
import sys
import syslog
import time
import logging
import logging.handlers

from util import *


OPERATIONAL = 'OPR'
"""Provozn� hl�ka, souvisej�c� se stavem syst�mu."""
ACTION = 'ACT'
"""V�znamn� hl�ka t�kaj�c� se zm�ny u�ivatelsk�ho rozhran� nebo dat."""
EVENT = 'EVT'
"""M�n� v�znamn� hl�ka t�kaj�c� se u�ivatelsk� akce nebo dat."""
DEBUG = 'DBG'
"""Intern� hl�ka pro lad�n�, neloguje se p�i ostr�m spu�t�n� aplikace."""


class Logger(object):
    """Abstraktn� t��da pro logov�n�.

    T��da obsahuje jedinou ve�ejnou metodu 'log()', prost�ednictv�m kter� je
    mo�no logov�n� kompletn� obslou�it.

    Tato t��da zaji��uje pot�ebnou logovac� infrastrukturu, nepos�l� v�ak
    hl�ky na ��dn� v�stup.  Zas�l�n� hl�ek do konkr�tn�ch c�l� je z�le�itost�
    potomk� t��dy.

    Pro obecn� logov�n� je l�pe nevyu��vat tuto t��du nebo jej� potomky p��mo,
    n�br� pou��t funkci 'log.log()'.

    """
    def __init__(self):
        """Inicializuj logger."""
        import config
        globals()['config'] = locals()['config']
        try:
            self._translator = gettext.translation('pytis', languages=('en',))
        except IOError:
            self._translator = None
        self._host = socket.gethostname()
        self._module_filter = config.log_module_filter
        try:
            class_ = list(config.log_class_filter)
            for i in range(len(class_)):
                c = class_[i]
                if type(c) == type(''):
                    pos = string.rfind(c, '.')
                    if pos:
                        mod, cls = c[:pos], c[pos+1:]
                        imp = __import__(mod, None, None, [cls])
                        class_[i] = imp.__dict__[cls]
                    else:
                        class_[i] = eval(c)
            self._class_filter = class_
        except:
            self._class_filter = ()

    def _retrieve_info(self):
        module = class_name = id_ = '?'
        class_ = None
        frame = inspect.currentframe().f_back.f_back.f_back
        if frame:
            if __debug__ and False:
                # TODO/Python: Zji��ov�n� jm�na modulu je velmi pomal� (chyba
                # modulu `inspect').
                try:
                    # TODO: Z�nezn�m�ho d�vodu od jist� doby nefunguje zji�t�n�
                    # modulu pro pytis.form.
                    module = inspect.getmodule(frame).__name__
                except:
                    pass
            l = frame.f_locals
            if 'self' in l:
                s = l['self']
                try:
                    class_ = s.__class__
                    class_name = class_.__name__
                except:
                    pass
                id_ = '%x' % positive_id(s)
        self._module = module
        self._class_ = class_
        self._class_name = class_name
        self._id = id_
        
    def _is_accepted(self, kind, message, data):
        if not __debug__ and kind == DEBUG:
            return False
        if kind in config.log_exclude:
            return False
        if kind == DEBUG:
            if self._module_filter and not starts_with(self._module, self._module_filter):
                return False
            if self._class_filter \
                   and (self._class_ is None \
                        or not some(lambda c: issubclass(self._class_, c), self._class_filter)):
                return False
        return True

    def _translated(self, message):
        translator = self._translator
        if translator:
            return self._translator.gettext(message)
        else:
            return message

    def _prefix(self, kind, message, data):
        datetime = time.strftime('%Y-%m-%d %H:%M:%S',
                                 time.gmtime(time.time()))
        host = self._host
        user = getpass.getuser()
        pid = os.getpid()
        prefix = '%s %s@%s[%s] %s %s %s[%s]: ' % \
                 (datetime, user, host, pid, kind, self._module,
                  self._class_name, self._id)
        return prefix
        
    def _formatted_message(self, message, data):
        return message

    def _formatted_data(self, prefix, fmessage, data):
        if data is not None:
            if type(data) == type(()):
                repr = `tuple(map(str, data))`
            elif type(data) == type(''):
                repr = data
            else:
                repr = `data`
            datalines = map(lambda l, prefix=prefix: '%s%s' % (prefix, l),
                            string.split(repr, '\n'))
            n = len(datalines)
            if n <= 1:
                if fmessage and fmessage[-1] == ':':
                    import config
                    if config.log_one_line_preferred:
                        return '*%s%s %s' % (prefix, fmessage, repr)
                data_string = '=%s%s' % (prefix, repr)
            else:
                datalines[0] = '<%s' % datalines[0]
                datalines[n-1] = '>%s' % datalines[n-1]
                datalines[1:n-1] = map(lambda l: ' %s' % l, datalines[1:n-1])
                data_string = string.join(datalines, '\n')
            formatted = '@%s%s\n%s' % (prefix, fmessage, data_string)
        else:
            formatted = '*%s%s' % (prefix, fmessage)
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

          kind -- druh hl�ky, jedna z konstant modulu 'log'
          message -- slovn� hl�ka, string; jestli�e tento string kon��
            dvojte�kou, 'data' maj� jedno��dkovou reprezentaci a konfigura�n�
            volba 'one_line_preferred' je pravda, jsou data do logu zaps�na na
            stejn� ��dek jako 'message'
          data -- libovoln� data; tento argument nen� nutno kl��ovat

        """        
        assert kind in (OPERATIONAL, ACTION, EVENT, DEBUG), \
               ('invalid logging kind', kind)
        assert is_anystring(message)
        self._retrieve_info()
        if not self._is_accepted(kind, message, data):
            return
        message = self._translated(message)
        formatted = self._formatted(kind, message, data)
        self._send(kind, formatted)


class StreamLogger(Logger):
    """Logger pos�laj�c� hl�en� do streamu.

    C�lov� stream je t��d� p�ed�n v�konstruktoru.  T��da nen� zodpov�dn� za
    hl�d�n� chyb streamu ani neprov�d� ��dn� akce v�p��pad�, �e stream je vn�
    t��dy uzav�en.

    """
    def __init__(self, stream):
        """Inicializuj logov�n�.

        Argumenty:

          stream -- otev�en� file object, do kter�ho lze logovat
          
        """
        super(StreamLogger, self).__init__()
        self._stream = stream
        
    def _send(self, kind, formatted):
        self._stream.write(formatted)
        self._stream.write('\n')
        self._stream.flush()


class SyslogLogger(Logger):

    """Logger pos�laj�c� hl�en� syslogu."""

    _MAX_MESSAGE_LENGTH = 1020

    FACILITY_LOCAL0 = syslog.LOG_LOCAL0
    FACILITY_LOCAL1 = syslog.LOG_LOCAL1
    FACILITY_LOCAL2 = syslog.LOG_LOCAL2
    FACILITY_LOCAL3 = syslog.LOG_LOCAL3
    FACILITY_LOCAL4 = syslog.LOG_LOCAL4
    FACILITY_LOCAL5 = syslog.LOG_LOCAL5
    FACILITY_LOCAL6 = syslog.LOG_LOCAL6
    FACILITY_LOCAL7 = syslog.LOG_LOCAL7
    
    def __init__(self, facility=None):
        super(SyslogLogger, self).__init__()
        if __debug__:
            facilities = [getattr(SyslogLogger, "FACILITY_LOCAL%d" % i)
                          for i in range(8)]
            assert facility is None or facility in facilities
        self._facility = facility
        
    def _prefix(self, kind, message, data):
        pid = os.getpid()
        user = getpass.getuser()
        prefix = '[%s] %s %s %s %s[%s]: ' % \
                 (pid, user, kind, self._module, self._class_name, self._id)
        return prefix
        
    def _send(self, kind, formatted):
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
            syslog.syslog(priority, formatted[:self._MAX_MESSAGE_LENGTH])
            formatted = formatted[self._MAX_MESSAGE_LENGTH:]



###


class LoggingInterface:
    """Rozhran� ke standardn�mu logovac�mu objektu.

    Tato t��da nen� ur�ena k�instanciaci mimo modul 'pytis.util.log'.
    
    """
    def __init__(self):
        self._logger = None # nelze inicializovat te�, kv�li z�vislostem modul�
        self._hooks = []

    def __call__(self, kind, message, data=None):
        """Zaloguj 'message'.

        Argumenty:

          kind -- druh hl�ky, jedna z konstant modulu
          message -- slovn� hl�ka, string
          data -- libovoln� data, tento argument nen� nutno kl��ovat

        Pokud je '__debug__' nepravda a 'kind' je 'DEBUG', 'message' nen�
        zalogov�no.

        """
        if __debug__ or kind is not DEBUG: # optimaliza�n� z�le�itost
            logger = self._logger
            if not logger:
                import config
                try:
                    cls, args, kwargs = config.log_logger
                except AttributeError:
                    cls, args, kwargs = (StreamLogger, (sys.stderr,), {})
                logger = self._logger = cls(*args, **kwargs)
            logger.log(kind, message, data)
        for hook in self._hooks:
            hook()

    def add_hook(self, hook):
        """P�idej 'hook' ke ka�d�mu logov�n�.

        Argumenty:

          hook -- funkce bez argument�, kter� je zavol�na p�i ka�d�m vol�n�
            metody '__call__()', bez ohledu na to, zda n�jak� zpr�va byla
            skute�n� zalogov�na

        Logovac� hooky lze vyu��t k�opakovan�mu vykon�n� n�jak� �innosti
        v�kter�koliv ��sti k�du.  My�lenka vych�z� z�toho, �e ve�ker� k�d by
        m�l na v�ech d�le�it�ch m�stech, a�tak� dostate�n� �asto �asov�,
        logovat.  Nav�en� vol�n� n��eho na logov�n� je pak nen�silnou metodou,
        jak zajistit opakovan� vol�n� n�jak�ho k�du i�v�p��pad�, kdy je
        z�jak�hokoliv d�vodu nevhodn� tak �init ve vedlej��m threadu.

        """
        self._hooks.append(hook)


log = LoggingInterface()
