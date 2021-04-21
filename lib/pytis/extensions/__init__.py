# -*- coding: utf-8 -*-

# Copyright (C) 2018-2021 Tomáš Cerha <t.cerha@gmail.com>
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

from .email_ import SimpleEmail, GPGEmail, ComplexEmail  # noqa: F401

from .dbconfig import DBConfig, cfg_param  # noqa: F401

from .dbutils import (  # noqa: F401
    dbselect, dbinsert, dbupdate, row_update, dbupdate_many, dbfunction,
    enum, is_in_groups, safe_commit, safe_rollback,
)

from .misc import (  # noqa: F401
    send_mail, UserDefaultPrinter, set_default_printer,
    cmd_set_default_printer, constraints_email, constraints_email_many,
    session_date, session_date_value, start_date, start_date_value, end_date,
    end_date_value, crypto_key_table, crypto_admin_key, check_crypto_password,
    add_crypto_user,
)

from .deftypes import StringNotNull, Price, _TreeOrder  # noqa: F401

from .spec import (  # noqa: F401
    ASC, DESC, UPCASE, LOWER, ALPHA, NUMERIC, ALPHANUMERIC, ASCII, FLOAT,
    ALWAYS, ONCE, NEVER, BROWSE_FORM, EDIT_FORM, INSERT_FORM, VIEW_FORM,
    FIELD_STYLE_DEFAULT, FIELD_STYLE_EMPHASIS, FIELD_STYLE_WARNING,
    FieldStyle, run_form_mitem, new_record_mitem, run_procedure_mitem,
    nr, rp, bf, df, mf, sf, ddf, ef, get_value, format_value, rp_handler,
    cb2colvalue, run_cb, make_presented_row, run_any_form,
    cmd_run_any_form, print2mail,
    mime_type_constraint,
)

from .defs import (  # noqa: F401
    get_form_defs, get_menu_forms, get_menu_defs, _get_default_select,
    check_form, cmd_check_form, CheckReporter, MenuChecker, AppChecker,
    DevelChecker, check_menus_defs, cmd_check_menus_defs,
    check_access_rights, cmd_check_access_rights, cache_spec,
)

from .dmp import (  # noqa: F401
    dmp_add_member, dmp_add_action, dmp_change_rights,
    dmp_commit, dmp_import, dmp_ls, dmp_reset_rights,
    dmp_update_form, dmp_delete_menu, dmp_delete_fullname,
    dmp_delete_shortname, dmp_convert_system_rights,
    dmp_copy_rights, dmp_rename_specification,
    DMPConfiguration,
)
