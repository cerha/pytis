# -*- coding: utf-8 -*-

# Copyright (C) 2018-2020 Tomáš Cerha <t.cerha@gmail.com>
# Copyright (C) 2001-2016 OUI Technology Ltd.
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

"""Formulář s tiskovým preview a tiskem.

Tento soubor definuje příslušný formulář a napojení tisku přes wxWidgets.
Neřeší tvorbu tiskových sestav jako takových, to je záležitost modulu
'pytis.output'.

Poznámka: Výstižnější než 'lib*output' by bylo 'lib*print', to je však
v konfliktu s klíčovým slovem Pythonu.

"""

import _thread
import tempfile


import lcg
import pytis.output
import pytis.util
import pytis.remote

from .event import UserBreakException
from .dialog import Error
from .form import Form, PopupForm
from .screen import launch_file
from .application import run_dialog

_ = pytis.util.translations('pytis-wx')


class PrintForm(Form, PopupForm):

    def __init__(self, parent, resolver, name, formatter, guardian=None, **kwargs):
        super(PrintForm, self).__init__(parent, resolver, name, guardian=guardian)
        self._formatter = formatter

    def show(self):
        pass

    def run(self, *args, **kwargs):
        def run_viewer(filename):
            launch_file(filename)
        output_file = tempfile.NamedTemporaryFile(suffix='.pdf', prefix='tmppytis', delete=False)
        try:
            self._formatter.printout(output_file)
        except lcg.SubstitutionIterator.NotStartedError:
            tbstring = pytis.util.format_traceback()
            pytis.util.log(pytis.util.OPERATIONAL, 'Print exception caught', tbstring)
            run_dialog(Error, _("Invalid use of identifier `data' in print specification.\n"
                                "Maybe use `current_row' instead?"))
            return
        except UserBreakException:
            return
        output_file.close()
        _thread.start_new_thread(run_viewer, (output_file.name,))
        self._formatter.cleanup()
