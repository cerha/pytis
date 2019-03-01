# Copyright (C) 2009, 2010, 2012 Brailcom, o.p.s.
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

"""Pytis Content Management System.

Pytis CMS makes it possible to manage a web application through an ordinary pytis fat client
application.

It basically consists of two parts:

  * Pytis application for management of database structures, which define the behavior and content
    of a web application.
  * Wiking based web application, which reads these database structures, behaves according to
    them and displays the content defined by them.

Pytis CMS allows management of the following aspects of a web application:

  * Users
  * Roles and their assignment to particular users
  * Available language variants
  * Hierarchical main menu and content of the pages behind particular menu items
  * Assigning acces rights for particular menu items to user roles
  * Extension modules capable to display additional `inteligent' content within pages
  
Wiking modules of Pytis CMS implement the following features of a web application:

  * Authentication
  * Authorization
  * Session management
  * Main menu

Example application using Pytis CMS can be found in pytis-demo.  The steps needed to employ Pytis
CMS in an existing Pytis Application are:

  1) Include the file 'db/db_pytis_cms.py' in the application's top level gensql script (see the
     documentation in 'db/db_pytis_cms.py' for more instructions).

  2) Create a file named 'cms.py' in the application's defs directory and import there all needed
     Pytis CMS specifications from 'pytis.cms' (see 'cms.py' in pytis-demo).

  3) Add CMS forms into the main menu of your pytis application (see 'application.py' in
     pytis-demo).

  4) Import all Wiking modules from 'pytis.cms.web' into the python module which is in
     your web application's 'modules' search path.  See 'wwwdemo.py' in pytis-demo. 

"""

from cms import Menu, MenuParents, Languages, Modules, Actions, GenericActions, \
     Users, Roles, SystemRoles, AllRoles, UserRoles, RoleUsers, Rights, SessionLog, AccessLog, \
     UserSessionLog, Themes
import web
