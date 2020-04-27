# -*- coding: utf-8 -*-

# Copyright (C) 2018-2020 Tomáš Cerha <t.cerha@gmail.com>
# Copyright (C) 2011-2018 OUI Technology Ltd.
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
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

try:
    from .proxy import run_proxy
except ImportError as e:
    def run_proxy(*args, **kwargs):
        raise e

from .remote import (  # noqa!
    client_ip, client_available,
    version, library_version, x2goclient_version,
    session_password, get_clipboard_text, set_clipboard_text,
    launch_file, launch_url, make_selected_file, make_temporary_file,
    open_file, open_selected_file, select_directory, select_file,
    run_python, x2go_session_id, x2go_display,
    pytis_x2go_info_file, read_x2go_info_file, parse_x2go_info_file,
    X2GoInfoException, X2GoInfoSoftException, X2GoInfoHardException,
    RPCInfo, Connector
)
