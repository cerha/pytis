# -*- coding: utf-8 -*-

# Copyright (C) 2018-2024 Tomáš Cerha <t.cerha@gmail.com>
# Copyright (C) 2005-2013 OUI Technology Ltd.
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

from __future__ import print_function

# Imports use 'X as X' form (PEP 484) to mark names as explicitly re-exported
# without maintaining a separate __all__ list.
from .email_ import SimpleEmail as SimpleEmail, GPGEmail as GPGEmail, ComplexEmail as ComplexEmail

from .dbutils import (
    dbselect as dbselect, dbinsert as dbinsert, dbupdate as dbupdate,
    row_update as row_update, dbupdate_many as dbupdate_many, dbfunction as dbfunction,
    enum as enum, is_in_groups as is_in_groups, safe_commit as safe_commit,
    safe_rollback as safe_rollback,
)

from .misc import (
    send_mail as send_mail, UserDefaultPrinter as UserDefaultPrinter,
    set_default_printer as set_default_printer,
    constraints_email as constraints_email, constraints_email_many as constraints_email_many,
    crypto_key_table as crypto_key_table, crypto_admin_key as crypto_admin_key,
    check_crypto_password as check_crypto_password, add_crypto_user as add_crypto_user,
)

from .deftypes import StringNotNull as StringNotNull, Price as Price, _TreeOrder as _TreeOrder

from .spec import (
    ASC as ASC, DESC as DESC, UPCASE as UPCASE, LOWER as LOWER, ALPHA as ALPHA,
    NUMERIC as NUMERIC, ALPHANUMERIC as ALPHANUMERIC, ASCII as ASCII, FLOAT as FLOAT,
    ALWAYS as ALWAYS, ONCE as ONCE, NEVER as NEVER,
    BROWSE_FORM as BROWSE_FORM, EDIT_FORM as EDIT_FORM,
    INSERT_FORM as INSERT_FORM, VIEW_FORM as VIEW_FORM,
    FIELD_STYLE_DEFAULT as FIELD_STYLE_DEFAULT,
    FIELD_STYLE_EMPHASIS as FIELD_STYLE_EMPHASIS,
    FIELD_STYLE_WARNING as FIELD_STYLE_WARNING,
    FieldStyle as FieldStyle, run_form_mitem as run_form_mitem,
    run_procedure_mitem as run_procedure_mitem,
    rp as rp, bf as bf, df as df, mf as mf, get_value as get_value,
    format_value as format_value, rp_handler as rp_handler,
    cb2colvalue as cb2colvalue, run_cb as run_cb, make_presented_row as make_presented_row,
    run_any_form as run_any_form, cmd_run_any_form as cmd_run_any_form,
    cmd_check_form as cmd_check_form,
    cmd_check_menus_defs as cmd_check_menus_defs, print2mail as print2mail,
    mime_type_constraint as mime_type_constraint,
)

from .defs import (
    get_form_defs as get_form_defs, get_menu_defs as get_menu_defs,
    _get_default_select as _get_default_select,
    CheckReporter as CheckReporter, MenuChecker as MenuChecker,
    AppChecker as AppChecker, DevelChecker as DevelChecker,
    check_form as check_form, check_menus_defs as check_menus_defs,
    check_access_rights as check_access_rights,
)

from .dmp import (
    dmp_menu as dmp_menu, dmp_add_member as dmp_add_member,
    dmp_add_action as dmp_add_action, dmp_change_rights as dmp_change_rights,
    dmp_commit as dmp_commit, dmp_import as dmp_import, dmp_ls as dmp_ls,
    dmp_reset_rights as dmp_reset_rights,
    dmp_update_form as dmp_update_form, dmp_delete_menu as dmp_delete_menu,
    dmp_delete_fullname as dmp_delete_fullname,
    dmp_delete_shortname as dmp_delete_shortname,
    dmp_convert_system_rights as dmp_convert_system_rights,
    dmp_copy_rights as dmp_copy_rights,
    dmp_rename_specification as dmp_rename_specification,
    DMPConfiguration as DMPConfiguration,
)
