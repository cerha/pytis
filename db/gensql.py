# -*- coding: utf-8 -*-

# Copyright (C) 2012 Brailcom, o.p.s.
#
# COPYRIGHT NOTICE
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.


# This file serves just for gensql -> gensqlalchemy conversions.
import pytis.data
def include_file(filename):
    file_, pathname, description = imp.find_module(filename)
    include(pathname, globals())
include_file('db_common')
include_file('db_pytis_config')
include_file('db_pytis_menu')
include_file('db_statistics')
include_file('db_pytis_logging')
include_file('db_output')
include_file('db_pytis_crypto')
include_file('db_pytis_help')
cms_users_table = 'cms_users_table'
cms_rights = (('all', 'cms'),)
cms_rights_rw = (('all', 'cmsrw'),)
include_file('db_pytis_cms')
# The following must be placed after inclusion of db_pytis_cms in order to
# split that file in several pieces based on dependencies.
include_file('db_pytis_cms_user')
