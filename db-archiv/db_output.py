# -*- coding: utf-8 -*-

# Copyright (C) 2010, 2011 Brailcom, o.p.s.
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

"""Gensql definitions for print output formatting."""

db_rights = globals().get('Gall_pytis', None)

if not db_rights:
    raise ProgramError('No rights specified! Please define Gall_pytis')

_std_table('e_pytis_output_templates',
      (P('id', TSerial),
       C('module', TString, constraints=('not null',)), # form
       C('specification', TString, constraints=('not null',)), # user template name
       C('template', TString),
       C('rowtemplate', TString),
       C('header', TString),
       C('first_page_header', TString),
       C('footer', TString),
       C('style', TString),
       C('username', TString),
       ),
      grant=db_rights,
      doc="""Storage of print output templates handled by a DatabaseResolver.""",
      depends=())

viewng('ev_pytis_global_output_templates',
       (SelectRelation('e_pytis_output_templates', alias='templates',
                       condition="username is null"),
        ),
       insert_order=('e_pytis_output_templates',),
       update_order=('e_pytis_output_templates',),
       delete_order=('e_pytis_output_templates',),
       grant=db_rights,
       depends=('e_pytis_output_templates',))

current_user_condition = "username=current_user or (username is null and (module, specification) not in (select module, specification from e_pytis_output_templates where templates.module=module and templates.specification=specification and username=current_user))"

viewng('ev_pytis_user_output_templates',
       (SelectRelation('e_pytis_output_templates', alias='templates',
                       condition=current_user_condition),
        ),
       insert="insert into e_pytis_output_templates (module, specification, template, rowtemplate, header, first_page_header, footer, style, username) values (new.module, new.specification, new.template, new.rowtemplate, new.header, new.first_page_header, new.footer, new.style, current_user)",
       update="""(
       insert into e_pytis_output_templates (module, specification, template, rowtemplate, header, first_page_header, footer, style, username) values (new.module, new.specification, new.template, new.rowtemplate, new.header, new.first_page_header, new.footer, new.style, current_user);
       delete from e_pytis_output_templates where id=old.id and username=current_user;
       )
       """,
       delete="delete from e_pytis_output_templates where id=old.id and username=current_user",
       grant=db_rights,
       depends=('e_pytis_output_templates',))
