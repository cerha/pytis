# -*- coding: utf-8

from __future__ import unicode_literals
from past.builtins import long
from builtins import range

import sqlalchemy
import string

import pytis.data.gensqlalchemy as sql
from pytis.data.dbdefs import and_, or_, not_, sval, func
import pytis.data

from pytis.dbdefs.db_pytis_base import Base_LogSQLTable, Base_PyFunction, Base_PyTriggerFunction, \
    default_access_rights, dmp_schemas
from pytis.dbdefs.db_pytis_common import XChanges


class EPytisDisabledDmpTriggers(sql.SQLTable):
    """This table allows disabling some trigger calls.
    Supported values (flags) are:
    genmenu -- initial insertion and deletion on certain tables
    positions -- set during trigger update of menu positions to prevent recursive trigger calls;
    note this way of doing it is not safe in case of parallel table updates
    redundancy -- set during rights redundancy update to prevent recursive trigger calls;
    note this way of doing it is not safe in case of parallel table updates
    """
    name = 'e_pytis_disabled_dmp_triggers'
    schemas = dmp_schemas.value(globals())
    fields = (sql.PrimaryColumn('id', pytis.data.Name()),)
    inherits = (XChanges,)
    depends_on = ()
    access_rights = default_access_rights.value(globals())


class CPytisRolePurposes(sql.SQLTable):
    """There are three kinds of roles:
    1. Menu and access administrator roles.  Definitions of these roles may be changed
    only by the database administrators.
    2. Roles corresponding to system accounts (login roles).
    3. Pure application roles.
    """
    name = 'c_pytis_role_purposes'
    schemas = dmp_schemas.value(globals())
    fields = (sql.PrimaryColumn('purposeid', pytis.data.String(minlen=4, maxlen=4, not_null=False)),
              sql.Column('purpose', pytis.data.String(maxlen=32, not_null=True), unique=True),
              )
    inherits = (XChanges,)
    init_columns = ('purposeid', 'purpose')
    init_values = (('admn', 'Správcovská',),
                   ('user', 'Uživatelská',),
                   ('appl', 'Aplikační',),
                   )
    depends_on = ()
    access_rights = default_access_rights.value(globals())


class EPytisRoles(Base_LogSQLTable):
    """Application user roles."""
    name = 'e_pytis_roles'
    schemas = dmp_schemas.value(globals())
    fields = (sql.PrimaryColumn('name', pytis.data.Name()),
              sql.Column('description', pytis.data.String(not_null=False)),
              sql.Column('purposeid', pytis.data.String(minlen=4, maxlen=4, not_null=True),
                         default='appl', references=sql.r.CPytisRolePurposes.purposeid),
              sql.Column('deleted', pytis.data.Date(not_null=False)),
              )
    inherits = (XChanges,)
    init_columns = ('name', 'description', 'purposeid', 'deleted')
    init_values = (('*', 'Zástupná role pro všechny role', 'admn', None,),
                   ('admin_roles', 'Administrátor rolí', 'admn', None,),
                   ('admin_menu', 'Administrátor menu', 'admn', None,),
                   ('admin', 'Administrátor rolí a menu', 'admn', None,),
                   ('__pytis', 'Zástupná role pro číselník sloupců', 'admn', None,),
                   )
    depends_on = (CPytisRolePurposes,)
    access_rights = default_access_rights.value(globals())


class EvPytisValidRoles(sql.SQLView):
    name = 'ev_pytis_valid_roles'
    schemas = dmp_schemas.value(globals())

    @classmethod
    def query(cls):
        main = sql.t.EPytisRoles.alias('main')
        codebook = sql.t.CPytisRolePurposes.alias('codebook')
        return sqlalchemy.select(
            cls._exclude(main) +
            cls._exclude(codebook, 'purposeid'),
            from_obj=[main.join(codebook, main.c.purposeid == codebook.c.purposeid)],
            whereclause=or_(main.c.deleted.is_(None), main.c.deleted > func.now()),
        )

    insert_order = (EPytisRoles,)
    update_order = (EPytisRoles,)
    delete_order = (EPytisRoles,)
    depends_on = (EPytisRoles, CPytisRolePurposes,)
    access_rights = default_access_rights.value(globals())


class EvPytisRoles(sql.SQLView):
    name = 'ev_pytis_roles'
    schemas = dmp_schemas.value(globals())

    @classmethod
    def query(cls):
        t1 = sql.t.EPytisRoles.alias('t1')
        t2 = sql.t.CPytisRolePurposes.alias('t2')
        return sqlalchemy.select(
            cls._exclude(t1) +
            cls._exclude(t2, 'purposeid'),
            from_obj=[t1.join(t2, t1.c.purposeid == t2.c.purposeid)]
        )

    insert_order = (EPytisRoles,)
    update_order = (EPytisRoles,)
    delete_order = (EPytisRoles,)
    depends_on = (EPytisRoles, CPytisRolePurposes,)
    access_rights = default_access_rights.value(globals())


class EPytisRoleMembers(Base_LogSQLTable):
    """Mutual memberships of roles.
    Entries in this table define members of each roleid.
    """
    name = 'e_pytis_role_members'
    schemas = dmp_schemas.value(globals())
    fields = (sql.PrimaryColumn('id', pytis.data.Serial(), doc="Just to make logging happy"),
              sql.Column('roleid', pytis.data.Name(not_null=True),
                         references=sql.a(sql.r.EPytisRoles.name, onupdate='CASCADE')),
              sql.Column('member', pytis.data.Name(not_null=True),
                         references=sql.a(sql.r.EPytisRoles.name, onupdate='CASCADE')),
              )
    inherits = (XChanges,)
    init_columns = ('id', 'roleid', 'member')
    init_values = ((-1, 'admin_roles', 'admin',),
                   (-2, 'admin_menu', 'admin',),
                   )
    depends_on = (EPytisRoles,)
    access_rights = default_access_rights.value(globals())


class EvPytisValidRoleMembers(sql.SQLView):
    name = 'ev_pytis_valid_role_members'
    schemas = dmp_schemas.value(globals())

    @classmethod
    def query(cls):
        main = sql.t.EPytisRoleMembers.alias('main')
        roles1 = sql.t.EvPytisValidRoles.alias('roles1')
        roles2 = sql.t.EvPytisValidRoles.alias('roles2')
        return sqlalchemy.select(
            cls._exclude(main) +
            cls._exclude(roles1) +
            cls._alias(roles2.c, mname=roles2.c.name, mdescription=roles2.c.description,
                       mpurposeid=roles2.c.purposeid, mpurpose=roles2.c.purpose,
                       mdeleted=roles2.c.deleted),
            from_obj=[main.join(roles1, roles1.c.name == main.c.roleid).
                      join(roles2, roles2.c.name == main.c.member)]
        )

    insert_order = (EPytisRoleMembers,)
    update_order = (EPytisRoleMembers,)
    delete_order = (EPytisRoleMembers,)
    depends_on = (EPytisRoleMembers, EvPytisValidRoles,)
    access_rights = default_access_rights.value(globals())


class APytisValidRoleMembers(sql.SQLTable):
    """Complete membership of roles, including transitive relations.
    This table is modified only by triggers.
    """
    name = 'a_pytis_valid_role_members'
    schemas = dmp_schemas.value(globals())
    fields = (sql.Column('roleid', pytis.data.Name(not_null=True),
                         references=sql.a(sql.r.EPytisRoles.name,
                                          onupdate='CASCADE', ondelete='CASCADE')),
              sql.Column('member', pytis.data.Name(not_null=True),
                         references=sql.a(sql.r.EPytisRoles.name,
                                          onupdate='CASCADE', ondelete='CASCADE')),
              )
    inherits = (XChanges,)
    depends_on = (EPytisRoles,)
    access_rights = default_access_rights.value(globals())


class PytisUpdateTransitiveRoles(Base_PyFunction):
    name = 'pytis_update_transitive_roles'
    schemas = dmp_schemas.value(globals())
    arguments = ()
    result_type = pytis.data.Boolean()
    multirow = False
    stability = 'VOLATILE'
    depends_on = ()
    access_rights = ()

    @staticmethod
    def pytis_update_transitive_roles():
        membership = {}
        for row in plpy.execute("select name from ev_pytis_valid_roles"):
            role = row['name']
            membership[role] = []
        for row in plpy.execute("select name, mname from ev_pytis_valid_role_members"):
            role = row['name']
            member = row['mname']
            membership[member].append(role)
        total_membership = {}
        for role in membership.keys():
            all_roles = []
            new_roles = [role]
            while new_roles:
                r = new_roles.pop()
                if r in all_roles:
                    continue
                all_roles.append(r)
                new_roles += membership.get(r, [])
            total_membership[role] = all_roles
        pg_escape = PytisUpdateTransitiveRoles.Util.pg_escape
        plpy.execute("delete from a_pytis_valid_role_members")
        for role, total_roles in total_membership.items():
            for total in total_roles:
                plpy.execute(("insert into a_pytis_valid_role_members (roleid, member) "
                              "values ('%s', '%s')") %
                             (pg_escape(total), pg_escape(role)))
        return True


class EPytisRolesTrigger(Base_PyTriggerFunction):
    name = 'e_pytis_roles_trigger'
    schemas = dmp_schemas.value(globals())
    arguments = ()
    result_type = sql.G_CONVERT_THIS_FUNCTION_TO_TRIGGER
    multirow = False
    stability = 'VOLATILE'
    depends_on = (EPytisRoles, EPytisDisabledDmpTriggers, APytisValidRoleMembers,
                  PytisUpdateTransitiveRoles,)
    access_rights = ()

    @staticmethod
    def e_pytis_roles_trigger():
        class Roles(EPytisRolesTrigger.Util.BaseTriggerObject):

            def _update_roles(self):
                plpy.execute("select pytis_update_transitive_roles()")

            def _do_after_insert(self):
                if plpy.execute("select * from e_pytis_disabled_dmp_triggers where id='genmenu'"):
                    return
                role = EPytisRolesTrigger.Util.pg_escape(self._new['name'])
                plpy.execute(("insert into a_pytis_valid_role_members(roleid, member) "
                              "values ('%s', '%s')") %
                             (role, role,))

            def _do_after_update(self):
                if plpy.execute("select * from e_pytis_disabled_dmp_triggers where id='genmenu'"):
                    return
                if self._new['deleted'] != self._old['deleted']:
                    self._update_roles()

            def _do_after_delete(self):
                if plpy.execute("select * from e_pytis_disabled_dmp_triggers where id='genmenu'"):
                    return
                self._update_roles()
        roles = Roles(TD)
        return roles.do_trigger()


class EPytisRolesTriggers(sql.SQLRaw):
    name = 'e_pytis_roles_triggers'
    schemas = dmp_schemas.value(globals())

    @classmethod
    def sql(class_):
        return """
create trigger e_pytis_roles_update_after after insert or update or delete on e_pytis_roles
for each row execute procedure e_pytis_roles_trigger();
"""
    depends_on = (EPytisRolesTrigger, EPytisRoles,)


class PytisCopyRole(sql.SQLFunction):
    """Make application roles of a user the same as those of another user."""
    name = 'pytis_copy_role'
    schemas = dmp_schemas.value(globals())
    arguments = (sql.Column('', pytis.data.String()),
                 sql.Column('', pytis.data.String()),)
    result_type = None
    multirow = False
    stability = 'VOLATILE'
    depends_on = (EPytisRoles, EPytisRoleMembers,)
    access_rights = default_access_rights.value(globals())


class PytisUser(sql.SQLFunction):
    """Return current pytis user.
    By redefining this function, debugging of user behavior or sulogin may be possible."""
    name = 'pytis_user'
    schemas = dmp_schemas.value(globals())
    arguments = ()
    result_type = pytis.data.Name()
    multirow = False
    stability = 'VOLATILE'
    depends_on = ()
    access_rights = ()

    def body(self):
        return "select user"


class CPytisActionTypes(sql.SQLTable):
    """List of defined action types."""
    name = 'c_pytis_action_types'
    schemas = dmp_schemas.value(globals())
    fields = (sql.PrimaryColumn('type', pytis.data.String(minlen=4, maxlen=4, not_null=False)),
              sql.Column('description', pytis.data.String(not_null=False)),
              )
    inherits = (XChanges,)
    init_columns = ('type', 'description')
    init_values = (('----', '',),
                   ('item', 'Položka menu',),
                   ('menu', 'Menu',),
                   ('sepa', 'Separátor',),
                   ('spec', 'Specifikace',),
                   ('subf', 'Podformulář',),
                   ('proc', 'Procedura',),
                   ('actf', 'Akce formuláře',),
                   ('prnt', 'Tisk',),
                   )
    depends_on = ()
    access_rights = default_access_rights.value(globals())


class CPytisMenuActions(sql.SQLTable):
    """List of available application actions."""
    name = 'c_pytis_menu_actions'
    schemas = dmp_schemas.value(globals())
    fields = (sql.PrimaryColumn('fullname', pytis.data.String(not_null=False)),
              sql.Column('shortname', pytis.data.String(not_null=True)),
              sql.Column('action_title', pytis.data.String(not_null=False)),
              sql.Column('description', pytis.data.String(not_null=False)),
              sql.Column('parent_action', pytis.data.String(not_null=False)),
              sql.Column('spec_name', pytis.data.String(not_null=False)),
              )
    inherits = (XChanges,)
    depends_on = ()
    access_rights = default_access_rights.value(globals())


class CPytisMenuActionsTriggerBefore(sql.SQLRaw):
    name = 'c_pytis_menu_actions_trigger_before'
    schemas = dmp_schemas.value(globals())

    @classmethod
    def sql(class_):
        return """
create or replace function c_pytis_menu_actions_trigger_before() returns trigger as $$
declare
    kind text;
begin
    kind := split_part(new.shortname, '/', 1);
    new.spec_name = null;
    if kind in ('action', 'proc', 'print') then
       new.spec_name = split_part(new.shortname, '/', 3);
    end if;
    if kind in ('form', 'NEW_RECORD', 'handle') then
       new.spec_name = split_part(new.shortname, '/', 2);
    end if;
    return new;
end;
$$ language plpgsql;
"""
    depends_on = ()


class EvPytisShortActions(sql.SQLRaw):
    name = 'ev_pytis_short_actions'
    schemas = dmp_schemas.value(globals())

    @classmethod
    def sql(class_):
        return """
create or replace view ev_pytis_short_actions
as select distinct shortname from c_pytis_menu_actions ORDER BY c_pytis_menu_actions.shortname;
"""
    depends_on = (CPytisMenuActions,)


class EPytisMenu(Base_LogSQLTable):
    """Menu structure definition."""
    name = 'e_pytis_menu'
    schemas = dmp_schemas.value(globals())
    fields = (sql.PrimaryColumn('menuid', pytis.data.Serial()),
              sql.Column('name', pytis.data.String(not_null=False),
                         doc=("Unique identifiers of terminal menu items. "
                              "NULL for non-terminal items and separators."), unique=True),
              sql.Column('title', pytis.data.String(maxlen=64, not_null=False),
                         doc="User title of the item. If NULL then it is a separator."),
              sql.Column('position', pytis.data.LTree(not_null=False),
                         doc=("Unique identifier of menu item placement within menu. "
                              "The top-menu item position is ''. "
                              "Each submenu has exactly one label more than its parent. "),
                         unique=True, index={'method': 'gist'}),
              sql.Column('next_position', pytis.data.LTree(not_null=False),
                         doc="Free position just after this menu item.",
                         unique=True, default='dummy'),
              sql.Column('fullname', pytis.data.String(not_null=False),
                         doc=("Application action assigned to the menu item. "
                              "Menu items bound to submenus should have this value NULL; "
                              "if they do not, the assigned action is ignored."),
                         references=sql.a(sql.r.CPytisMenuActions.fullname, onupdate='CASCADE')),
              sql.Column('help', pytis.data.String(not_null=False),
                         doc="Arbitrary single-line help string."),
              sql.Column('hotkey', pytis.data.String(not_null=False),
                         doc=("Sequence of command keys, separated by single spaces. "
                              "The space key is represented by SPC string.")),
              sql.Column('locked', pytis.data.Boolean(not_null=False),
                         doc="Iff true, this item may not be edited."),
              )
    inherits = (XChanges,)
    depends_on = (CPytisMenuActions,)
    access_rights = default_access_rights.value(globals())


class EPytisMenuTrigger(Base_PyTriggerFunction):
    name = 'e_pytis_menu_trigger'
    schemas = dmp_schemas.value(globals())
    arguments = ()
    result_type = sql.G_CONVERT_THIS_FUNCTION_TO_TRIGGER
    multirow = False
    stability = 'VOLATILE'
    depends_on = (EPytisMenu, CPytisMenuActions, EPytisDisabledDmpTriggers,)
    access_rights = ()

    @staticmethod
    def e_pytis_menu_trigger():
        class Menu(EPytisMenuTrigger.Util.BaseTriggerObject):
            # BEFORE

            def _maybe_new_action(self, old=None):
                if ((not self._new['name'] and self._new['title'] and
                     (old is None or not old['title']))):
                    # New non-terminal menu item
                    self._new['fullname'] = action = 'menu/' + str(self._new['menuid'])
                    if not plpy.execute("select * from c_pytis_menu_actions where fullname='%s'" %
                                        (EPytisMenuTrigger.Util.pg_escape(action),)):
                        plpy.execute(("insert into c_pytis_menu_actions "
                                      "(fullname, shortname, description) "
                                      "values ('%s', '%s', '%s')") %
                                     (action, action,
                                      EPytisMenuTrigger.Util.pg_escape("Menu '%s'" %
                                                                       (self._new['title'])),))
                        self._return_code = self._RETURN_CODE_MODIFY

            def _check_parent(self, old_position=None):
                # Prevent menu item movement to non-existent parents or to self
                new_position = self._new['position']
                if new_position == old_position:
                    return
                if old_position is not None:
                    if (new_position[:len(old_position)] == old_position and
                        (len(new_position) == len(old_position) or
                         new_position[len(old_position)] == '.')):
                        raise Exception('error', "Can't move menu item to itself")
                components = new_position.split('.')
                parent = '.'.join(components[:-1])
                if (parent and
                    not plpy.execute("select menuid from e_pytis_menu where position='%s'" %
                                     (parent,))):
                    raise Exception('error', "No menu item parent")

            def _do_before_insert(self):
                self._maybe_new_action()
                if plpy.execute("select * from e_pytis_disabled_dmp_triggers where id='genmenu'"):
                    return
                self._check_parent()

            def _do_before_update(self):
                if plpy.execute("select * from e_pytis_disabled_dmp_triggers where id='positions'"):
                    return
                self._check_parent(self._old['position'])
                self._maybe_new_action(old=self._old)

            def _do_before_delete(self):
                if plpy.execute("select * from e_pytis_disabled_dmp_triggers where id='genmenu'"):
                    return
                # If there are any children, reject deletion
                old_position = self._old['position']
                data = plpy.execute(("select * from e_pytis_menu where position ~ '%s.*' and "
                                     "position != '%s'") %
                                    (old_position, old_position,),
                                    1)
                if data:
                    self._return_code = self._RETURN_CODE_SKIP

            # AFTER
            def _update_positions(self, new=None, old=None):
                if ((old and new and
                     old['position'] == new['position'] and
                     old['next_position'] == new['next_position'])):
                    return
                plpy.execute('lock e_pytis_menu in exclusive mode')
                if old and new:
                    old_position = old['position']
                    new_position = new['position']
                    if old_position != new_position:
                        plpy.execute(("update e_pytis_menu "
                                      "set position='%s'||subpath(position, nlevel('%s')) "
                                      "where position <@ '%s'") %
                                     (new_position, old_position, old_position,))
                if new:
                    data = plpy.execute("select position, next_position from e_pytis_menu "
                                        "where position != '' order by position")
                    sequences = {}
                    for row in data:
                        position = row['position'].split('.')
                        next_position = row['next_position'].split('.')
                        position_stamp = tuple(position[:-1])
                        if position_stamp not in sequences:
                            sequences[position_stamp] = []
                        sequences[position_stamp].append((position, next_position,))

                    def update_next_position(position, next_position):
                        plpy.execute(("update e_pytis_menu set next_position='%s' "
                                      "where position='%s'") %
                                     ('.'.join(next_position), '.'.join(position),))
                    for position_list in sequences.values():
                        position_list_len = len(position_list)
                        for i in range(position_list_len - 1):
                            position, next_position = position_list[i]
                            next_item_position = position_list[i + 1][0]
                            if ((len(position) != len(next_position) or
                                 position >= next_position or
                                 next_position >= next_item_position)):
                                suffix = position[-1]
                                next_suffix = next_item_position[-1]
                                suffix += '0' * max(len(next_suffix) - len(suffix), 0)
                                next_suffix += '0' * max(len(suffix) - len(next_suffix), 0)
                                new_suffix = str((long(suffix) + long(next_suffix)) // 2)
                                while len(new_suffix) < len(next_suffix):
                                    new_suffix = '0' + new_suffix
                                if new_suffix == suffix:
                                    new_suffix = suffix + '4'
                                next_position = position[:-1] + [new_suffix]
                                update_next_position(position, next_position)
                        last_item = position_list[position_list_len - 1]
                        position, next_position = last_item
                        if ((len(position) != len(next_position) or
                             position >= next_position)):
                            suffix = position[-1]
                            if suffix[-1] == '9':
                                next_position = position[:-1] + [position[-1] + '4']
                            else:
                                next_position = position[:-1] + [str(long(position[-1]) + 1)]
                            update_next_position(position, next_position)
                plpy.execute("select e_pytis_menu_check_positions()")

            def _do_after_insert(self):
                if plpy.execute("select * from e_pytis_disabled_dmp_triggers where id='import'"):
                    return
                plpy.execute("insert into e_pytis_disabled_dmp_triggers (id) values ('positions')")
                self._update_positions(new=self._new)
                plpy.execute("delete from e_pytis_disabled_dmp_triggers where id='positions'")

            def _do_after_update(self):
                if plpy.execute("select * from e_pytis_disabled_dmp_triggers where id='positions'"):
                    return
                plpy.execute("insert into e_pytis_disabled_dmp_triggers (id) values ('positions')")
                if not self._new['name'] and self._old['title'] and not self._new['title']:
                    # Non-terminal item changed to separator
                    plpy.execute("delete from c_pytis_menu_actions where fullname = '%s'" %
                                 (self._old['fullname'],))
                    plpy.execute("delete from e_pytis_action_rights where shortname = '%s'" %
                                 (self._old['fullname'],))
                self._update_positions(new=self._new, old=self._old)
                if self._old['title'] != self._new['title']:
                    plpy.execute("update e_pytis_menu_translations set dirty=true where menuid=%s" %
                                 (self._new['menuid'],))
                plpy.execute("delete from e_pytis_disabled_dmp_triggers where id='positions'")

            def _do_after_delete(self):
                if plpy.execute("select * from e_pytis_disabled_dmp_triggers where id='import'"):
                    return
                plpy.execute("insert into e_pytis_disabled_dmp_triggers (id) values ('positions')")
                if not self._old['name'] and self._old['title']:
                    # Non-terminal menu item
                    plpy.execute("delete from c_pytis_menu_actions where fullname = '%s'" %
                                 (self._old['fullname'],))
                    plpy.execute("delete from e_pytis_action_rights where shortname = '%s'" %
                                 (self._old['fullname'],))
                self._update_positions(old=self._old)
                plpy.execute("delete from e_pytis_disabled_dmp_triggers where id='positions'")
        menu = Menu(TD)
        return menu.do_trigger()


class EPytisMenuTriggers(sql.SQLRaw):
    name = 'e_pytis_menu_triggers'
    schemas = dmp_schemas.value(globals())

    @classmethod
    def sql(class_):
        return """
create trigger e_pytis_menu_all_before before insert or update or delete on e_pytis_menu
for each row execute procedure e_pytis_menu_trigger();
create trigger e_pytis_menu_all_after after insert or update or delete on e_pytis_menu
for each row execute procedure e_pytis_menu_trigger();
"""
    depends_on = (EPytisMenuTrigger, EPytisMenu,)


class EPytisMenuSpecialTriggers(sql.SQLRaw):
    name = 'e_pytis_menu_special_triggers'
    schemas = dmp_schemas.value(globals())

    @classmethod
    def sql(class_):
        return """
create or replace function e_pytis_menu_check_positions() returns void as $$
begin
  if (select count(*)>0 from (select position from e_pytis_menu intersect
                               select next_position from e_pytis_menu) positions) then
    raise exception 'position / next_position conflict';
  end if;
end;
$$ language plpgsql;

create or replace function e_pytis_menu_check_trigger() returns trigger as $$
begin
  if (select count(*)=0 from e_pytis_disabled_dmp_triggers where id='positions') then
    perform e_pytis_menu_check_positions();
  end if;
  return null;
end;
$$ language plpgsql;
create trigger e_pytis_menu_zcheck_after after insert or update or delete on e_pytis_menu
for each statement execute procedure e_pytis_menu_check_trigger();
"""
    depends_on = (EPytisMenu, EPytisDisabledDmpTriggers,)


class CPytisMenuLanguages(sql.SQLTable):
    """Codebook of available menu languages."""
    name = 'c_pytis_menu_languages'
    schemas = dmp_schemas.value(globals())
    fields = (sql.PrimaryColumn('language', pytis.data.String(not_null=False)),
              sql.Column('description', pytis.data.String(not_null=True)),
              )
    inherits = (XChanges,)
    init_columns = ('language', 'description')
    init_values = (('cs', 'čeština',),)
    depends_on = ()
    access_rights = default_access_rights.value(globals())


class EPytisMenuTranslations(sql.SQLTable):
    """Translations of menu titles."""
    name = 'e_pytis_menu_translations'
    schemas = dmp_schemas.value(globals())
    fields = (sql.Column('menuid', pytis.data.Integer(not_null=False),
                         references=sql.gA('e_pytis_menu', onupdate='CASCADE', ondelete='CASCADE'),
                         index=True),
              sql.Column('language', pytis.data.String(not_null=True),
                         references=sql.gA('c_pytis_menu_languages',
                                           onupdate='CASCADE', ondelete='CASCADE')),
              sql.Column('t_title', pytis.data.String(not_null=True)),
              sql.Column('dirty', pytis.data.Boolean(not_null=True)),
              )
    inherits = (XChanges,)
    depends_on = (EPytisMenu,)
    access_rights = default_access_rights.value(globals())


class EvPytisMenu(sql.SQLView):
    name = 'ev_pytis_menu'
    schemas = dmp_schemas.value(globals())
    primary_column = 'menuid'

    @classmethod
    def query(cls):
        main = sql.t.EPytisMenu.alias('main')
        actions = sql.t.CPytisMenuActions.alias('actions')
        return sqlalchemy.select(
            cls._exclude(main, 'fullname') +
            cls._exclude(actions, 'description', 'spec_name', 'parent_action') +
            [sql.gL("(select count(*)-1 from e_pytis_menu where position <@ main.position)")
             .label('position_nsub'),
             sql.gL("coalesce(main.title, '――――')").label('xtitle')],
            from_obj=[main.outerjoin(actions, main.c.fullname == actions.c.fullname)]
        )

    insert_order = (EPytisMenu,)
    no_insert_columns = ('position_nsub', 'xtitle',)
    update_order = (EPytisMenu,)
    no_update_columns = ('position_nsub', 'xtitle',)
    delete_order = (EPytisMenu,)
    depends_on = (EPytisMenu, CPytisMenuActions,)
    access_rights = default_access_rights.value(globals())


class UpdateEPytisMenuTranslations(sql.SQLRaw):
    name = 'update_e_pytis_menu_translations'
    schemas = dmp_schemas.value(globals())

    @classmethod
    def sql(class_):
        return """
create or replace function update_e_pytis_menu_translations(int, text, text, text, text)
        returns int as $$
  update e_pytis_menu_translations set language=$2, t_title=$3, dirty=(dirty and $3=$4)
        where menuid=$1 and language=$2;
  insert into e_pytis_menu_translations (menuid, language, t_title, dirty)
        (select $1, $2, $3, false where (select count(*)=0 from e_pytis_menu_translations
                where menuid=$1 and language=$2)) returning menuid;
$$ language sql;
"""
    depends_on = (EPytisMenuTranslations,)


class EvPytisTranslatedMenu(sql.SQLView):
    name = 'ev_pytis_translated_menu'
    schemas = dmp_schemas.value(globals())
    special_insert_columns = ((EvPytisMenu, 'menuid', "substring(new.id from '/(.*)$')::int",),)

    @classmethod
    def query(cls):
        menu = sql.t.EvPytisMenu.alias('menu')
        languages = sql.t.CPytisMenuLanguages.alias('languages')
        translations = sql.t.EPytisMenuTranslations.alias('translations')
        return sqlalchemy.select(
            cls._exclude(menu) +
            cls._exclude(languages, 'description') +
            cls._exclude(translations, 'menuid', 'language', 'dirty') +
            [sql.gL("coalesce(t_title, xtitle)").label('t_xtitle'),
             sql.gL("coalesce(dirty, title is not null)").label('dirty'),
             sql.gL("languages.language||'/'||menu.menuid").label('id')],
            from_obj=[
                menu.join(
                    languages, sqlalchemy.sql.true()
                ).outerjoin(
                    translations, and_(
                        menu.c.menuid == translations.c.menuid,
                        languages.c.language == translations.c.language
                    )
                )
            ]
        )

    insert_order = (EvPytisMenu,)
    no_insert_columns = ('t_xtitle', 'dirty', 'id',)

    def on_insert_also(self):
        return ("insert into e_pytis_menu_translations (menuid, language, t_title, dirty) "
                "(select "
                "substring(new.id from '/(.*)$')::int, "
                "substring(new.id from '^(.*)/'), coalesce(new.t_title, new.title), "
                "new.t_title is null "
                "where new.t_title is not null or new.title is not null)",)
    update_order = (EvPytisMenu,)
    no_update_columns = ('t_xtitle', 'dirty', 'id',)

    def on_update_also(self):
        return ("select update_e_pytis_menu_translations(substring(new.id from '/(.*)$')::int, "
                "new.language, new.t_title, old.t_title, new.title) "
                "where new.language=substring(new.id from '^(.*)/') and new.t_title is not null",)
    delete_order = (EvPytisMenu,)
    depends_on = (EvPytisMenu, CPytisMenuLanguages, EPytisMenuTranslations,
                  UpdateEPytisMenuTranslations,)
    access_rights = default_access_rights.value(globals())


class PytisFirstPosition(Base_PyFunction):
    name = 'pytis_first_position'
    schemas = dmp_schemas.value(globals())
    arguments = (sql.Column('', pytis.data.String()),)
    result_type = pytis.data.String()
    multirow = False
    stability = 'VOLATILE'
    depends_on = ()
    access_rights = ()

    @staticmethod
    def pytis_first_position(position):
        position = args[0]
        start = '1' + '0' * (len(position) - 1)
        first = str((long(start) + long(position)) // 2)
        if first == start:
            first += '8'
        return first


class EvPytisMenuAllPositions(sql.SQLView):
    name = 'ev_pytis_menu_all_positions'
    schemas = dmp_schemas.value(globals())

    @classmethod
    def query(cls):
        def select_1():
            menu1 = sql.t.EPytisMenu.alias('menu1')
            return sqlalchemy.select(
                sql.reorder_columns([sql.gL("position"),
                                     sql.gL("coalesce(menu1.title, '――――')").label('xtitle')],
                                    ['position', 'xtitle']),
                from_obj=[menu1]
            )

        def select_2():
            menu2 = sql.t.EPytisMenu.alias('menu2')
            return sqlalchemy.select(
                sql.reorder_columns([sql.gL("next_position").label('position'),
                                     sql.gL("''").label('xtitle')], ['position', 'xtitle']),
                from_obj=[menu2],
                whereclause=menu2.c.position != sval(''),
            )
        set_1 = sqlalchemy.union(select_1(), select_2())

        def select_3():
            menu3 = sql.t.EPytisMenu.alias('menu3')
            return sqlalchemy.select(
                sql.reorder_columns([
                    sql.gL("menu3.position||pytis_first_position(subpath((select position "
                           "from e_pytis_menu where position <@ menu3.position and "
                           "position != menu3.position union select '9' order by position limit 1),"
                           " -1)::text)::ltree").label('position'),
                    sql.gL("''").label('xtitle')], ['position', 'xtitle']),
                from_obj=[menu3],
                whereclause=and_(menu3.c.name.is_(None), not_(menu3.c.title.is_(None))),
            )
        return sqlalchemy.union(set_1, select_3())
    insert_order = ()
    update_order = ()
    delete_order = ()
    depends_on = (EPytisMenu, PytisFirstPosition,)
    access_rights = default_access_rights.value(globals())


class EvPytisMenuPositions(sql.SQLView):
    name = 'ev_pytis_menu_positions'
    schemas = dmp_schemas.value(globals())

    @classmethod
    def query(cls):
        positions = sql.t.EvPytisMenuAllPositions.alias('positions')
        menu = sql.t.EPytisMenu.alias('menu')
        return sqlalchemy.select(
            cls._exclude(positions) +
            cls._exclude(menu, 'name', 'fullname', 'position', 'hotkey', 'help', 'locked'),
            from_obj=[positions.outerjoin(menu, positions.c.position == menu.c.position)]
        )

    insert_order = ()
    update_order = ()
    delete_order = ()
    depends_on = (EvPytisMenu, EvPytisMenuAllPositions, EPytisMenu,)
    access_rights = default_access_rights.value(globals())


class CPytisAccessRights(sql.SQLTable):
    """Available rights.  Not all rights make sense for all actions and menus."""
    name = 'c_pytis_access_rights'
    schemas = dmp_schemas.value(globals())
    fields = (sql.PrimaryColumn('rightid', pytis.data.String(maxlen=8, not_null=False)),
              sql.Column('description', pytis.data.String(not_null=True)),
              )
    inherits = (XChanges,)
    init_columns = ('rightid', 'description')
    init_values = (('*', 'Všechna práva',),
                   ('show', 'Viditelnost položek menu',),
                   ('view', 'Prohlížení existujících záznamů',),
                   ('insert', 'Vkládání nových záznamů',),
                   ('update', 'Editace existujících záznamů',),
                   ('delete', 'Mazání záznamů',),
                   ('print', 'Tisky',),
                   ('export', 'Exporty',),
                   ('call', 'Spouštění aplikačních procedur',),
                   )
    depends_on = ()
    access_rights = default_access_rights.value(globals())


class EPytisActionRights(Base_LogSQLTable):
    """Assignments of access rights to actions.

    Extent of each action right is strictly limited by its granted system
    permissions.  Non-system rights can only further limit the system rights.  If
    there is no system permission for a given action, the action is forbidden.  The
    resulting right is not broader than the intersection of all the related
    permissions.

    Some actions, e.g. dual form actions, form its access rights set by inclusion
    of other action rights.

    Access rights of non-terminal menu items, identified by action name
    'menu/MENUID' define default rights, used when no non-system access right
    definition is present for a terminal item.  More nested access rights of
    non-terminal menu items have higher precedence.

    Action rights are supported and used in the application, but they are not
    exposed in the current user interface directly.  In future they may also
    support extended rights assignment, e.g. in context menus etc.
    """
    name = 'e_pytis_action_rights'
    fields = (sql.PrimaryColumn('id', pytis.data.Serial(), doc="Just to make logging happy"),
              sql.Column('shortname', pytis.data.String(not_null=True)),
              sql.Column('roleid', pytis.data.Name(not_null=True),
                         references=sql.a(sql.r.EPytisRoles.name, onupdate='CASCADE')),
              sql.Column('rightid', pytis.data.String(maxlen=8, not_null=True),
                         references=sql.a(sql.r.CPytisAccessRights.rightid, onupdate='CASCADE')),
              sql.Column('colname', pytis.data.Name()),
              sql.Column('system', pytis.data.Boolean(not_null=True),
                         doc="Iff true, this is a system (noneditable) permission.", default=False),
              sql.Column('granted', pytis.data.Boolean(not_null=True),
                         doc=("If true the right is granted, otherwise it is denied; "
                              "system rights are always granted."), default=True),
              sql.Column('redundant', pytis.data.Boolean(not_null=False),
                         doc="If true, the right is redundant in the current set of access rights.",
                         default=False),
              sql.Column('status', pytis.data.SmallInteger(not_null=True),
                         doc=("Status of the right: 0 = current; -1 = old "
                              "(to be deleted after the next global rights update); "
                              "1 = new (not yet active, to be activated after the next "
                              "global rights update)."), default=1),
              )
    inherits = (XChanges,)
    unique = (('shortname', 'roleid', 'rightid', 'colname', 'system', 'granted', 'status',),)
    depends_on = (CPytisMenuActions, EPytisRoles, CPytisAccessRights,)
    access_rights = default_access_rights.value(globals())


class PytisConvertSystemRights(Base_PyFunction):
    name = 'pytis_convert_system_rights'
    schemas = dmp_schemas.value(globals())
    arguments = (sql.Column('', pytis.data.String()),)
    result_type = None
    multirow = False
    stability = 'VOLATILE'
    depends_on = (EPytisActionRights,)
    access_rights = ()

    @staticmethod
    def pytis_convert_system_rights(shortname):
        shortname = args[0]
        q = """select count(*) as pocet from e_pytis_action_rights
                where shortname='%s' and system=True
            """ % shortname
        result = plpy.execute(q)
        if result[0]["pocet"] == 0:
            plpy.error('No system rights found for %s' % shortname)
        result = plpy.execute(q)
        temp_old = plpy.execute("select new_tempname() as jmeno")[0]["jmeno"]
        temp_new = plpy.execute("select new_tempname() as jmeno")[0]["jmeno"]
        q = (("create temp table %s as "
              "select * from pytis_view_summary_rights('%s', NULL, False, False)") %
             (temp_old, shortname,))
        result = plpy.execute(q)
        q = "delete from e_pytis_action_rights where shortname='%s' and redundant" % (shortname,)
        plpy.execute(q)
        q = """insert into e_pytis_action_rights (shortname, roleid, rightid, granted, system)
               values ('%s', '*', '*', False, False)
            """ % shortname
        plpy.execute(q)
        q = """update e_pytis_action_rights set system=False
                where shortname='%s' and system=True
            """ % shortname
        plpy.execute(q)
        q = "select pytis_update_summary_rights()"
        plpy.execute(q)
        q = (("create temp table %s as "
              "select * from pytis_view_summary_rights('%s', NULL, False, False)") %
             (temp_new, shortname,))
        result = plpy.execute(q)
        q = """select * from %s except select * from %s
                union
               select * from %s except select * from %s
            """ % (temp_old, temp_new, temp_new, temp_old)
        result = plpy.execute(q)
        if len(result) > 0:
            msg = "Converted rights are not the same as original rights"
            plpy.error(msg)


class PytisUpdateRightsRedundancy(Base_PyFunction):
    name = 'pytis_update_rights_redundancy'
    schemas = dmp_schemas.value(globals())
    arguments = ()
    result_type = pytis.data.Boolean()
    multirow = False
    stability = 'VOLATILE'
    depends_on = (EPytisDisabledDmpTriggers, APytisValidRoleMembers, EPytisActionRights,)
    access_rights = ()

    @staticmethod
    def pytis_update_rights_redundancy():
        plpy.execute("insert into e_pytis_disabled_dmp_triggers (id) values ('redundancy')")
        roles = {}
        for row in plpy.execute("select roleid, member from a_pytis_valid_role_members"):
            roleid, member = row['roleid'], row['member']
            role_list = roles.get(roleid)
            if role_list is None:
                role_list = roles[roleid] = [roleid]
            role_list.append(member)

        class Right(object):
            properties = ('id', 'shortname', 'roleid', 'rightid', 'granted', 'colname', 'system',
                          'redundant', 'status',)

            def __init__(self, **kwargs):
                for k, v in kwargs.items():
                    if k not in ('vytvoril', 'vytvoreno', 'zmenil', 'zmeneno',):
                        if k not in self.properties:
                            raise Exception('programming error', k)
                        setattr(self, k, v)

            def __eq__(self, other):
                if pytis.util.sameclass(self, other):
                    return self.id == other.id
                else:
                    return NotImplemented

            def __ne__(self, other):
                # Implied automatically in Python 3; Can be removed when dropping Python 2 support.
                return not self == other

            def strong_redundant(self, other):
                for attr in Right.properties:
                    if attr not in ('id', 'redundant', 'roleid', 'system', 'granted', 'status',):
                        if getattr(self, attr) != getattr(other, attr):
                            return False
                if self.system and not other.system:
                    return False
                if self.roleid not in roles.get(other.roleid, []):
                    return False
                if not self.granted and other.granted:
                    return False

                return True

            def default_redundant(self, other):
                for attr in Right.properties:
                    if attr not in ('id', 'redundant', 'rightid', 'roleid', 'colname', 'system',
                                    'granted', 'status',):
                        if getattr(self, attr) != getattr(other, attr):
                            return False
                if self.system and not other.system:
                    return False
                if (((self.rightid == other.rightid or other.rightid == '*') and
                     (self.roleid in roles.get(other.roleid, []) or other.roleid == '*') and
                     (self.colname == other.colname or other.colname is None))):
                    if self.granted == other.granted:
                        return True
                    else:
                        return other
                return False

            def matches_system_rights(self, system_rights):
                if self.system:
                    return True
                if not system_rights:
                    return True
                if self.rightid == 'show':
                    # Well, there are typically no SHOW system rights...
                    return True
                for r in system_rights:
                    if (((self.roleid not in roles.get(r.roleid, []) or
                          self.roleid == '*' or r.roleid == '*') and
                         (self.rightid == r.rightid or self.rightid == '*' or r.rightid == '*') and
                         (self.colname == r.colname or self.colname is None or r.colname is None))):
                        return True
                return False
        rights = {}
        for row in plpy.execute("select * from e_pytis_action_rights where status>=0"):
            r = Right(**row)
            comrades = rights.get(r.shortname)
            if comrades is None:
                comrades = rights[r.shortname] = []
            comrades.append(r)
        base_rights = []
        redundant_rights = []
        for key, comrades in rights.items():
            base = []
            while comrades:
                r = comrades.pop()
                for rr in comrades + base:
                    if r is not rr and r.strong_redundant(rr):
                        redundant_rights.append(r)
                        break
                else:
                    base.append(r)
            rights[key] = base
        for key, comrades in rights.items():
            base = []
            system_rights = [rc for rc in comrades if rc.system]
            for r in comrades:
                if r.matches_system_rights(system_rights):
                    base.append(r)
                else:
                    redundant_rights.append(r)
            rights[key] = base
        for comrades in rights.values():
            base = set()
            maybe_redundant = []
            sure_redundant = set()
            while comrades:
                r = comrades.pop()
                blockers = set()
                redundant = False
                # If the right differs from all other rights here completely, it's
                # not redundant.  If it is compatible with a default ("star")
                # right, it may be redundant, but only if it doesn't override
                # another non-redundant default right.  This algorithm may not give
                # perfect result, but it's important so that it at least doesn't
                # mark a non-redundant right as redundant.  We only care about
                # specific->general rules interactions; exact matches should be
                # already resolved by the strong redundancy pass, i.e. the graph of
                # blockers should be acyclic (it contains only edges from more
                # specific to less specific rights).
                for rr in comrades + list(base):
                    state = r.default_redundant(rr)
                    if state is True:
                        redundant = True
                    elif state is not False:
                        blockers.add(state)
                if redundant:
                    blockers = blockers - sure_redundant
                    if blockers:
                        if blockers.intersection(base):
                            base.add(r)
                        else:
                            maybe_redundant.append((r, blockers,))
                    else:
                        redundant_rights.append(r)
                        sure_redundant.add(r)
                else:
                    base.add(r)
            while maybe_redundant:
                new_maybe_redundant = []
                for r, blockers in maybe_redundant:
                    blockers = blockers - sure_redundant
                    if blockers:
                        if blockers.intersection(base):
                            base.add(r)
                        else:
                            new_maybe_redundant.append((r, blockers,))
                    else:
                        redundant_rights.append(r)
                        sure_redundant.add(r)
                if len(new_maybe_redundant) == len(maybe_redundant):
                    for r, blockers in new_maybe_redundant:
                        base.add(r)
                    break
                maybe_redundant = new_maybe_redundant
            base_rights += list(base)
        for r in base_rights:
            if r.redundant:
                plpy.execute("update e_pytis_action_rights set redundant='F' "
                             "where id='%d' and redundant!='F'" % (r.id,))
        for r in redundant_rights:
            if not r.redundant:
                plpy.execute("update e_pytis_action_rights set redundant='T' "
                             "where id='%d' and redundant!='T'" % (r.id,))
        plpy.execute("delete from e_pytis_disabled_dmp_triggers where id='redundancy'")


class EPytisRoleMembersTrigger(Base_PyTriggerFunction):
    name = 'e_pytis_role_members_trigger'
    schemas = dmp_schemas.value(globals())
    arguments = ()
    result_type = sql.G_CONVERT_THIS_FUNCTION_TO_TRIGGER
    multirow = False
    stability = 'VOLATILE'
    depends_on = (EPytisRoleMembers, EPytisDisabledDmpTriggers, APytisValidRoleMembers,
                  PytisUpdateTransitiveRoles, PytisUpdateRightsRedundancy,)
    access_rights = ()

    @staticmethod
    def e_pytis_role_members_trigger():
        class Roles(EPytisRoleMembersTrigger.Util.BaseTriggerObject):

            def _update_roles(self):
                plpy.execute("select pytis_update_transitive_roles()")

            def _update_redundancy(self):
                plpy.execute("select pytis_update_rights_redundancy()")

            def _update_all(self):
                if plpy.execute("select * from e_pytis_disabled_dmp_triggers where id='genmenu'"):
                    return
                self._update_roles()
                self._update_redundancy()

            def _do_after_insert(self):
                self._update_all()

            def _do_after_update(self):
                self._update_all()

            def _do_after_delete(self):
                self._update_all()
        roles = Roles(TD)
        return roles.do_trigger()


class EPytisRoleMembersTriggers(sql.SQLRaw):
    name = 'e_pytis_role_members_triggers'
    schemas = dmp_schemas.value(globals())

    @classmethod
    def sql(class_):
        return """
create trigger e_pytis_role_members_all_after after insert or update or delete
        on e_pytis_role_members
for each row execute procedure e_pytis_role_members_trigger();
"""
    depends_on = (EPytisRoleMembersTrigger, EPytisRoleMembers,)


class EPytisActionRightsTrigger(Base_PyTriggerFunction):
    name = 'e_pytis_action_rights_trigger'
    schemas = dmp_schemas.value(globals())
    arguments = ()
    result_type = sql.G_CONVERT_THIS_FUNCTION_TO_TRIGGER
    multirow = False
    stability = 'VOLATILE'
    depends_on = (EPytisActionRights, PytisUpdateRightsRedundancy, EPytisDisabledDmpTriggers,)
    access_rights = ()

    @staticmethod
    def e_pytis_action_rights_trigger():
        class Rights(EPytisActionRightsTrigger.Util.BaseTriggerObject):

            def _update_redundancy(self):
                if plpy.execute("select * from e_pytis_disabled_dmp_triggers where id='genmenu'"):
                    return
                plpy.execute("select pytis_update_rights_redundancy()")

            def _do_before_insert(self):
                if self._new['status'] is None:
                    self._new['status'] = 1
                    self._return_code = self._RETURN_CODE_MODIFY

            def _do_after_insert(self):
                self._update_redundancy()

            def _do_after_update(self):
                if plpy.execute("select * from e_pytis_disabled_dmp_triggers "
                                "where id='redundancy'"):
                    return
                self._update_redundancy()

            def _do_after_delete(self):
                self._update_redundancy()
        rights = Rights(TD)
        return rights.do_trigger()


class EPytisActionRightsTriggers(sql.SQLRaw):
    name = 'e_pytis_action_rights_triggers'
    schemas = dmp_schemas.value(globals())

    @classmethod
    def sql(class_):
        return """
create trigger e_pytis_action_rights_all_before before insert on e_pytis_action_rights
for each row execute procedure e_pytis_action_rights_trigger();
create trigger e_pytis_action_rights_all_after after insert or update or delete
        on e_pytis_action_rights
for each statement execute procedure e_pytis_action_rights_trigger();
"""
    depends_on = (EPytisActionRightsTrigger, EPytisActionRights,)


class EvPytisActionRights(sql.SQLView):
    name = 'ev_pytis_action_rights'
    schemas = dmp_schemas.value(globals())

    @classmethod
    def query(cls):
        rights = sql.t.EPytisActionRights.alias('rights')
        roles = sql.t.EPytisRoles.alias('roles')
        purposes = sql.t.CPytisRolePurposes.alias('purposes')
        return sqlalchemy.select(
            cls._exclude(rights) +
            cls._exclude(purposes, 'purposeid'),
            from_obj=[rights.outerjoin(roles, rights.c.roleid == roles.c.name).
                      outerjoin(purposes, roles.c.purposeid == purposes.c.purposeid)],
            whereclause=rights.c.status >= 0,
        )

    insert_order = (EPytisActionRights,)

    def on_update(self):
        return ("""(
insert into e_pytis_action_rights (shortname, roleid, rightid, colname, system, granted, status)
       values (new.shortname, new.roleid, new.rightid, new.colname, new.system, new.granted, 1);
update e_pytis_action_rights set status=-1 where id=new.id;
)""",)

    def on_delete(self):
        return ("""(
delete from e_pytis_action_rights where id=old.id and status=1;
update e_pytis_action_rights set status=-1 where id=old.id and status=0;
)""",)
    depends_on = (EPytisActionRights, EPytisRoles, CPytisRolePurposes,)
    access_rights = default_access_rights.value(globals())


class TypActionRightsFoldable(sql.SQLType):
    name = 'typ_action_rights_foldable'
    schemas = dmp_schemas.value(globals())
    fields = (sql.Column('id', pytis.data.Integer(not_null=False)),
              sql.Column('roleid', pytis.data.String(not_null=False)),
              sql.Column('purpose', pytis.data.String(not_null=False)),
              sql.Column('shortname', pytis.data.String(not_null=False)),
              sql.Column('colname', pytis.data.String(not_null=False)),
              sql.Column('rightid', pytis.data.String(not_null=False)),
              sql.Column('system', pytis.data.Boolean(not_null=False)),
              sql.Column('granted', pytis.data.Boolean(not_null=False)),
              sql.Column('redundant', pytis.data.Boolean(not_null=False)),
              sql.Column('tree', pytis.data.LTree()),
              sql.Column('subcount', pytis.data.Integer(not_null=False)),
              )
    depends_on = ()
    access_rights = ()


class PytisActionRightsFoldable(Base_PyFunction):
    name = 'pytis_action_rights_foldable'
    schemas = dmp_schemas.value(globals())
    arguments = (sql.Column('', pytis.data.String()),
                 sql.Column('', pytis.data.String()),)
    result_type = TypActionRightsFoldable
    multirow = True
    stability = 'VOLATILE'
    depends_on = (EvPytisActionRights, TypActionRightsFoldable,)
    access_rights = ()

    @staticmethod
    def pytis_action_rights_foldable(shortname, column):
        shortname, column = args
        if column is None:
            column = 'roleid'
        tree = {}
        if shortname:
            condition = "shortname='%s'" % (shortname,)
        else:
            condition = "true"
        query = ("select id, roleid, purpose, shortname, colname, rightid, system, "
                 "granted, redundant "
                 "from ev_pytis_action_rights where %s" % (condition,))
        for row in plpy.execute(query):
            column_value = row[column]
            column_rows = tree.get(column_value, [])
            column_rows.append(row)
            tree[column_value] = column_rows
        if column == 'roleid':
            other_column = 'colname'
        else:
            other_column = 'roleid'
        result = []

        def ltree_value(value):
            if not value:
                return '_'
            safe_value = []
            for c in value:
                if c not in string.ascii_letters and c not in string.digits:
                    c = '_'
                safe_value.append(c)
            return ''.join(safe_value)
        for column_value, rows in tree.items():
            def maybe_label(target_column):
                if target_column == column:
                    label = column_value
                else:
                    label = None
                return label
            result.append((-1, maybe_label('roleid'), None, shortname, maybe_label('colname'),
                           maybe_label('rightid'), None, None, None,
                           ltree_value(column_value), len(rows),))
            for row in rows:
                tree_id = ltree_value(column_value) + '.' + ltree_value(row[other_column])
                result.append((row['id'], row['roleid'], row['purpose'], row['shortname'],
                               row['colname'], row['rightid'], row['system'], row['granted'],
                               row['redundant'],
                               tree_id, 0,))
        return result


class PytisCopyRights(sql.SQLFunction):
    """Make access rights of a menu item the same as of another menu item."""
    name = 'pytis_copy_rights'
    schemas = dmp_schemas.value(globals())
    arguments = (sql.Column('', pytis.data.String()),
                 sql.Column('', pytis.data.String()),)
    result_type = None
    multirow = False
    stability = 'VOLATILE'
    depends_on = (EPytisActionRights,)
    access_rights = default_access_rights.value(globals())


class PytisColumnsInRights(sql.SQLFunction):
    """Return column names appearing in rights assigned to given shortname."""
    name = 'pytis_columns_in_rights'
    schemas = dmp_schemas.value(globals())
    arguments = (sql.Column('', pytis.data.String()),)
    result_type = pytis.data.Name()
    multirow = True
    stability = 'VOLATILE'
    depends_on = (EPytisActionRights,)
    access_rights = default_access_rights.value(globals())


class PytisRemoveRedundant(sql.SQLFunction):
    """Remove redundant rights of the given action."""
    name = 'pytis_remove_redundant'
    schemas = dmp_schemas.value(globals())
    arguments = (sql.Column('', pytis.data.String()),)
    result_type = None
    multirow = False
    stability = 'VOLATILE'
    depends_on = (EPytisActionRights, EvPytisActionRights,)
    access_rights = default_access_rights.value(globals())


class PytisActionsLockId(sql.SQLFunction):
    """Id of the advisory lock for a_pytis_actions_structure."""
    name = 'pytis_actions_lock_id'
    schemas = dmp_schemas.value(globals())
    arguments = ()
    result_type = pytis.data.LargeInteger()
    multirow = False
    stability = 'VOLATILE'
    depends_on = ()
    access_rights = default_access_rights.value(globals())

    def body(self):
        return "select 200910081415"


class TypSummaryRights(sql.SQLType):
    name = 'typ_summary_rights'
    schemas = dmp_schemas.value(globals())
    fields = (sql.Column('shortname', pytis.data.String(not_null=False)),
              sql.Column('roleid', pytis.data.String(not_null=False)),
              sql.Column('rights', pytis.data.String(not_null=False)),
              sql.Column('columns', pytis.data.String(not_null=False)),
              )
    depends_on = ()
    access_rights = ()


class PytisComputeSummaryRights(Base_PyFunction):
    name = 'pytis_compute_summary_rights'
    schemas = dmp_schemas.value(globals())
    arguments = (sql.Column('', pytis.data.String()),
                 sql.Column('', pytis.data.String()),
                 sql.Column('', pytis.data.Boolean()),
                 sql.Column('', pytis.data.Boolean()),
                 sql.Column('', pytis.data.Boolean()),)
    result_type = TypSummaryRights
    multirow = True
    stability = 'VOLATILE'
    depends_on = (APytisValidRoleMembers, EPytisActionRights, TypSummaryRights,)
    access_rights = ()

    @staticmethod
    def pytis_compute_summary_rights(shortname_arg, role_arg, new_arg, multirights_arg,
                                     compress_arg):
        shortname_arg, role_arg, new_arg, multirights_arg, compress_arg = args
        import copy
        pg_escape = PytisComputeSummaryRights.Util.pg_escape
        # Retrieve roles
        roles = {}
        query = "select roleid, member from a_pytis_valid_role_members"
        if role_arg:
            query = "%s where member='%s'" % (query, role_arg,)
        for row in plpy.execute(query):
            roleid, member = row['roleid'], row['member']
            members = roles.get(member)
            if members is None:
                roles[member] = members = []
            members.append(roleid)

        # Retrieve rights
        class RawRights(object):

            def __init__(self):
                self.system = set()
                self.allowed = set()
                self.forbidden = set()
        raw_rights = {}
        if new_arg:
            condition = 'status >= 0'
        else:
            condition = 'status <= 0'
        if shortname_arg:
            s = pg_escape(shortname_arg)
            q = (("select distinct shortname from c_pytis_menu_actions "
                  "where shortname='%s' or "
                  "substr(fullname, 8) in "
                  "(select fullname from c_pytis_menu_actions where shortname='%s')")
                 % (s, s,))
            related_shortnames_list = ["'%s'" % (pg_escape(row['shortname']),)
                                       for row in plpy.execute(q)]
            if not related_shortnames_list:
                return []
            related_shortnames = ', '.join(related_shortnames_list)
            condition = "%s and shortname in (%s)" % (condition, related_shortnames,)
        rights_query = ("select rightid, granted, roleid, shortname, colname, system "
                        "from e_pytis_action_rights "
                        "where %s") % (condition,)
        for row in plpy.execute(rights_query):
            rightid, granted, roleid, shortname, column, system = \
                row['rightid'], row['granted'], row['roleid'], row['shortname'], row['colname'], \
                row['system']
            key = shortname
            item_rights = raw_rights.get(key)
            if item_rights is None:
                raw_rights[key] = item_rights = {}
            role_rights = item_rights.get(roleid)
            if role_rights is None:
                item_rights[roleid] = role_rights = RawRights()
            if system:
                r = role_rights.system
            elif granted:
                r = role_rights.allowed
            else:
                r = role_rights.forbidden
            r.add((rightid, column,))
        # Retrieve subactions
        subactions = {}
        if shortname_arg:
            shortname_condition = "c_pytis_menu_actions.shortname in (%s)" % (related_shortnames,)
            condition = ('fullname in (select fullname from c_pytis_menu_actions where %s)' %
                         (shortname_condition,))
        else:
            shortname_condition = condition = 'true'
        for row in plpy.execute(("select fullname, shortname from c_pytis_menu_actions "
                                 "where fullname like 'sub/%%' and %s order by fullname") %
                                (condition,)):
            fullname, shortname = row['fullname'], row['shortname']
            parent = fullname[fullname.find('/', 4) + 1:]
            parent_subactions = subactions.get(parent)
            if parent_subactions is None:
                parent_subactions = subactions[parent] = []
            parent_subactions.append(shortname)
        # Compute rights
        standard_rights = ['view', 'insert', 'update', 'delete', 'print', 'export', 'call']

        class Rights(object):

            def __init__(self, total, allowed, forbidden, subforms, columns):
                self.total = total
                self.allowed = allowed
                self.forbidden = forbidden
                self.subforms = subforms
                self.columns = columns
        computed_rights = {}
        for row in plpy.execute("select fullname, shortname from c_pytis_menu_actions where %s" %
                                (shortname_condition,)):
            shortname, fullname = row['shortname'], row['fullname']
            item_rights = raw_rights.get(shortname, {})
            subforms = subactions.get(fullname, ())
            if shortname[:5] == 'form/' or shortname[:9] == 'RUN_FORM/':
                default_forbidden = set((('call', None,),))
            elif shortname[:7] in ('handle/', 'action/',):
                default_forbidden = set((('view', None,), ('insert', None,), ('update', None,),
                                         ('delete', None,), ('print', None,), ('export', None,),))
            elif shortname[:5] == 'menu/':
                default_forbidden = set((('view', None,), ('insert', None,), ('update', None,),
                                         ('delete', None,),
                                         ('print', None,), ('export', None,), ('call', None,),))
            else:
                default_forbidden = set()
            for roleid, role_roles in roles.items():
                columns = {}
                max_rights = set()
                allowed_rights = set()
                forbidden_rights = copy.copy(default_forbidden)
                for role in role_roles:
                    raw = item_rights.get(role) or RawRights()
                    max_rights.update(raw.system)
                    allowed_rights.update(raw.allowed)
                    forbidden_rights.update(raw.forbidden)
                allowed_rights.difference_update(forbidden_rights)
                raw_default = item_rights.get('*') or RawRights()
                max_rights.update(raw_default.system)
                forbidden_rights.update(raw_default.forbidden.difference(allowed_rights))
                allowed_rights.update(raw_default.allowed.difference(forbidden_rights))
                for r, c in (allowed_rights.union(max_rights)):
                    if c not in columns:
                        columns[c] = set()
                    columns[c].add(r)
                for r, c in forbidden_rights:
                    if c not in columns:
                        columns[c] = set()
                if not max_rights:
                    for r in item_rights.values():
                        if r.system:
                            break
                    else:
                        max_rights = set([(r, None,) for r in standard_rights])

                def store_rights(shortname, max_rights, allowed_rights, forbidden_rights):
                    if max_rights is None:
                        max_rights = allowed_rights
                    rights = set([right for right in max_rights
                                  if right not in forbidden_rights and
                                  ('*', right[1],) not in forbidden_rights])
                    for r in allowed_rights.difference(forbidden_rights):
                        if r in max_rights or (r[0], None,) in max_rights:
                            rights.add(r)
                    if ('show', None,) not in forbidden_rights:
                        rights.add(('show', None,))
                    computed_rights[(shortname, roleid,)] = Rights(total=rights,
                                                                   allowed=allowed_rights,
                                                                   forbidden=forbidden_rights,
                                                                   subforms=subforms,
                                                                   columns=columns)
                store_rights(shortname, max_rights, allowed_rights, forbidden_rights)
        # Output summary rights
        result = []
        for short_key, all_rights in computed_rights.items():
            shortname, roleid = short_key
            if shortname_arg is not None and shortname != shortname_arg:
                continue
            if role_arg is not None and roleid != role_arg:
                continue
            # Multiform view rights are valid if they are permitted by the main
            # form and (in case of the VIEW right) at least one of the side forms.
            total = all_rights.total
            subforms = all_rights.subforms
            if multirights_arg and subforms:
                r = computed_rights.get((subforms[0], roleid,))  # main form
                if r is not None:
                    total = set([rr for rr in total if rr in r.total or (rr[0], None,) in r.total])
                subforms_total = set()
                for sub in subforms[1:]:
                    r = computed_rights.get((sub, roleid,))
                    if r is None:
                        subforms_total = set([(sr, None,) for sr in standard_rights + ['show']])
                        break
                    else:
                        subforms_total.update(r.total)
                all_rights.total = set([rt for rt in total
                                        if rt[0] != 'view' or
                                        rt[0] in [rr[0] for rr in subforms_total]])
            # Format and return the rights
            if compress_arg:
                rights = []
                for r, column in all_rights.total:
                    if r not in rights:
                        rights.append(r)
                rights.sort()
                rights_string = ' '.join(rights)
                result.append((shortname, roleid, rights_string, ''))
            else:
                rights = all_rights.total
                general_rights = set([r for r, column in rights if column is None])
                column_rights = all_rights.columns
                for r, column in rights:
                    if column is not None and r in general_rights:
                        column_rights[column].add(r)
                summarized_rights = {}
                for column, crights in column_rights.items():
                    if column is None:
                        continue
                    if '*' in crights:
                        crights_list = standard_rights
                    else:
                        crights_list = list(crights)
                    crights_list = [r for r in crights
                                    if (r, None,) in rights or (r, column,) in rights]
                    crights_list = crights_list + [r for r in general_rights
                                                   if (r, column) not in all_rights.forbidden]
                    crights_list = list(set(crights_list))  # remove duplicates
                    crights_list.sort()
                    rights_string = ' '.join(crights_list)
                    summarized_rights[rights_string] = (summarized_rights.get(rights_string, []) +
                                                        [column])
                general_rights = list(general_rights)
                general_rights.sort()
                summarized_rights[' '.join(general_rights)] = None
                for rights_string, columns in summarized_rights.items():
                    if columns is None:
                        columns_string = ''
                    else:
                        columns.sort()
                        columns_string = ' '.join(columns)
                    result.append((shortname, roleid, rights_string, columns_string,))
        return result


class PytisUpdateSummaryRights(sql.SQLFunction):
    name = 'pytis_update_summary_rights'
    schemas = dmp_schemas.value(globals())
    arguments = ()
    result_type = None
    multirow = False
    stability = 'VOLATILE'
    depends_on = (EPytisActionRights,)
    access_rights = default_access_rights.value(globals())


class APytisActionsStructure(sql.SQLTable):
    """Precomputed actions structure as presented to menu admin.
    Item positions and indentations are determined by positions.
    This table is modified only by triggers.
    """
    name = 'a_pytis_actions_structure'
    schemas = dmp_schemas.value(globals())
    fields = (sql.Column('fullname', pytis.data.String(not_null=True)),
              sql.Column('shortname', pytis.data.String(not_null=True)),
              sql.Column('menuid', pytis.data.Integer(not_null=False)),
              sql.Column('position', pytis.data.LTree(not_null=True), index={'method': 'gist'}),
              sql.Column('type', pytis.data.String(minlen=4, maxlen=4, not_null=True),
                         references=sql.r.CPytisActionTypes.type),
              sql.Column('summaryid', pytis.data.String(not_null=False)),
              )
    inherits = (XChanges,)
    depends_on = (CPytisActionTypes,)
    access_rights = default_access_rights.value(globals())


class EvPytisMenuStructure(sql.SQLView):
    name = 'ev_pytis_menu_structure'
    schemas = dmp_schemas.value(globals())

    @classmethod
    def query(cls):
        structure = sql.t.APytisActionsStructure.alias('structure')
        menu = sql.t.EPytisMenu.alias('menu')
        atypes = sql.t.CPytisActionTypes.alias('atypes')
        actions = sql.t.CPytisMenuActions.alias('actions')
        return sqlalchemy.select(
            cls._exclude(structure, 'menuid', 'summary_id', 'summaryid') +
            cls._exclude(menu, 'name', 'fullname', 'position', 'title') +
            cls._alias(cls._exclude(atypes, 'type'), actiontype=atypes.c.description) +
            cls._exclude(actions, 'fullname', 'shortname', 'action_title', 'spec_name',
                         'parent_action') +
            [sql.gL("(select count(*)-1 from a_pytis_actions_structure "
                    "where position <@ structure.position)").label('position_nsub'),
             sql.gL("coalesce(menu.title, '('||actions.action_title||')')").label('title')],
            from_obj=[structure.outerjoin(menu, structure.c.menuid == menu.c.menuid).
                      outerjoin(atypes, structure.c.type == atypes.c.type).
                      outerjoin(actions, structure.c.fullname == actions.c.fullname)]
        )

    @classmethod
    def join_columns(cls):
        return ((sql.c.APytisActionsStructure.fullname, sql.c.CPytisMenuActions.fullname),)
    insert_order = ()
    update_order = (CPytisMenuActions,)
    no_update_columns = ('position_nsub', 'title',)
    delete_order = ()
    depends_on = (EPytisMenu, APytisActionsStructure, CPytisMenuActions, CPytisActionTypes,)
    access_rights = default_access_rights.value(globals())


class PytisUpdateActionsStructure(Base_PyFunction):
    name = 'pytis_update_actions_structure'
    schemas = dmp_schemas.value(globals())
    arguments = ()
    result_type = pytis.data.Boolean()
    multirow = False
    stability = 'VOLATILE'
    depends_on = (APytisActionsStructure, EPytisMenu, CPytisMenuActions, PytisActionsLockId,)
    access_rights = ()

    @staticmethod
    def pytis_update_actions_structure():
        for row in plpy.execute("select pytis_actions_lock_id() as lock_id"):
            lock_id = row['lock_id']
        plpy.execute("select pg_advisory_lock(%s)" % (lock_id,))
        try:
            pg_escape = PytisUpdateActionsStructure.Util.pg_escape
            plpy.execute("delete from a_pytis_actions_structure")
            subactions = {}
            for row in plpy.execute("select fullname, shortname from c_pytis_menu_actions "
                                    "where fullname like 'sub/%' order by fullname"):
                fullname, shortname = row['fullname'], row['shortname']
                parent = fullname[fullname.find('/', 4) + 1:]
                parent_subactions = subactions.get(parent)
                if parent_subactions is None:
                    parent_subactions = subactions[parent] = []
                parent_subactions.append((fullname, shortname,))
            formactions = {}
            for row in plpy.execute("select shortname, fullname from c_pytis_menu_actions "
                                    "where shortname like 'action/%' or shortname like 'print/%' "
                                    "order by shortname"):
                fullname = row['fullname']
                form_name = fullname[fullname.find('/', 7) + 1:]
                form_name_formactions = formactions.get(form_name)
                if form_name_formactions is None:
                    form_name_formactions = formactions[form_name] = []
                form_name_formactions.append((row['fullname'], row['shortname'],))
            actions = {}

            def add_row(fullname, shortname, menuid, position):
                if fullname[:4] == 'sub/':
                    item_type = 'subf'
                elif fullname[:7] == 'action/':
                    item_type = 'actf'
                elif fullname[:6] == 'print/':
                    item_type = 'prnt'
                elif position.find('.') == -1:
                    item_type = '----'
                elif menuid is None:
                    item_type = 'spec'
                elif not fullname:
                    item_type = 'sepa'
                elif fullname[:5] == 'menu/':
                    item_type = 'menu'
                else:
                    item_type = 'item'
                if menuid is None:
                    menuid = 'NULL'
                plpy.execute(("insert into a_pytis_actions_structure "
                              "(fullname, shortname, menuid, position, type) "
                              "values('%s', '%s', %s, '%s', '%s') ") %
                             (pg_escape(fullname), pg_escape(shortname), menuid, position,
                              item_type,))
                actions[shortname] = True

            def add_formactions(shortname, position):
                formaction_list = formactions.get(shortname[5:], ())
                for i in range(len(formaction_list)):
                    faction_fullname, faction_shortname = formaction_list[i]
                    subposition = '%s.%02d' % (position, (i + 50),)
                    add_row(faction_fullname, faction_shortname, None, subposition)
            for row in plpy.execute("select menuid, position, c_pytis_menu_actions.fullname, "
                                    "shortname "
                                    "from e_pytis_menu, c_pytis_menu_actions "
                                    "where e_pytis_menu.fullname = c_pytis_menu_actions.fullname "
                                    "order by position"):
                menuid, position, shortname, fullname = \
                    row['menuid'], row['position'], row['shortname'], row['fullname']
                add_row(fullname, shortname, menuid, position)
                action_components = shortname.split('/')
                if action_components[0] == 'form' or action_components[0] == 'RUN_FORM':
                    subaction_list = subactions.get(fullname, ())
                    for i in range(len(subaction_list)):
                        sub_fullname, sub_shortname = subaction_list[i]
                        subposition = '%s.%02d' % (position, i,)
                        add_row(sub_fullname, sub_shortname, None, subposition)
                        add_formactions(sub_shortname, subposition)
                    if not subaction_list:
                        add_formactions(shortname, position)
            position = '8.0001'
            add_row('label/1', 'label/1', None, '8')
            for row in plpy.execute("select fullname, shortname from c_pytis_menu_actions "
                                    "order by shortname"):
                fullname, shortname = row['fullname'], row['shortname']
                if shortname in actions:
                    continue
                add_row(fullname, shortname, None, position)
                position_components = position.split('.')
                position = '.'.join(position_components[:-1] +
                                    ['%04d' % (int(position_components[-1]) + 1)])
        finally:
            plpy.execute("select pg_advisory_unlock(%s)" % (lock_id,))


class CPytisMenuActionsTrigger(Base_PyTriggerFunction):
    name = 'c_pytis_menu_actions_trigger'
    schemas = dmp_schemas.value(globals())
    arguments = ()
    result_type = sql.G_CONVERT_THIS_FUNCTION_TO_TRIGGER
    multirow = False
    stability = 'VOLATILE'
    depends_on = (CPytisMenuActions, PytisUpdateActionsStructure, EPytisDisabledDmpTriggers,)
    access_rights = ()

    @staticmethod
    def c_pytis_menu_actions_trigger():
        class Menu(CPytisMenuActionsTrigger.Util.BaseTriggerObject):

            def _update_all(self):
                if plpy.execute("select * from e_pytis_disabled_dmp_triggers where id='genmenu'"):
                    return
                plpy.execute("select pytis_update_actions_structure()")

            def _do_after_insert(self):
                self._update_all()

            def _do_after_update(self):
                self._update_all()

            def _do_after_delete(self):
                self._update_all()
        menu = Menu(TD)
        return menu.do_trigger()


class CPytisMenuActionsTriggers(sql.SQLRaw):
    name = 'c_pytis_menu_actions_triggers'
    schemas = dmp_schemas.value(globals())

    @classmethod
    def sql(class_):
        return """
create trigger c_pytis_menu_actions_all_after_rights after insert or update or delete
        on c_pytis_menu_actions
for each statement execute procedure c_pytis_menu_actions_trigger();

create trigger c_pytis_menu_actions_all_trigger_before before insert or update
        on c_pytis_menu_actions
for each row execute procedure c_pytis_menu_actions_trigger_before();
"""
    depends_on = (CPytisMenuActionsTrigger, CPytisMenuActionsTriggerBefore, CPytisMenuActions,)


class EPytisMenuTriggerRights(Base_PyTriggerFunction):
    name = 'e_pytis_menu_trigger_rights'
    schemas = dmp_schemas.value(globals())
    arguments = ()
    result_type = sql.G_CONVERT_THIS_FUNCTION_TO_TRIGGER
    multirow = False
    stability = 'VOLATILE'
    depends_on = (EPytisMenu, PytisUpdateActionsStructure, EPytisDisabledDmpTriggers,)
    access_rights = ()

    @staticmethod
    def e_pytis_menu_trigger_rights():
        class Menu(EPytisMenuTriggerRights.Util.BaseTriggerObject):

            def _update_all(self):
                if plpy.execute("select * from e_pytis_disabled_dmp_triggers "
                                "where id='genmenu' or id='positions'"):
                    return
                plpy.execute("select pytis_update_actions_structure()")

            def _do_after_insert(self):
                self._update_all()

            def _do_after_update(self):
                self._update_all()

            def _do_after_delete(self):
                self._update_all()
        menu = Menu(TD)
        return menu.do_trigger()


class EPytisMenuTriggersRights(sql.SQLRaw):
    name = 'e_pytis_menu_triggers_rights'
    schemas = dmp_schemas.value(globals())

    @classmethod
    def sql(class_):
        return """
create trigger e_pytis_menu_all_after_rights after insert or update or delete on e_pytis_menu
for each statement execute procedure e_pytis_menu_trigger_rights();
"""
    depends_on = (EPytisMenuTriggerRights, EPytisMenu,)


class PytisMultiformSpec(sql.SQLFunction):
    name = 'pytis_multiform_spec'
    schemas = dmp_schemas.value(globals())
    arguments = (sql.Column('', pytis.data.String()),)
    result_type = pytis.data.Boolean()
    multirow = False
    stability = 'VOLATILE'
    depends_on = ()
    access_rights = ()

    def body(self):
        return "select $1 not like 'sub/%' and ($1 like '%::%' or $1 like '%.Multi%')"


class TypPreviewSummaryRights(sql.SQLType):
    name = 'typ_preview_summary_rights'
    schemas = dmp_schemas.value(globals())
    fields = (sql.Column('shortname', pytis.data.String(not_null=False)),
              sql.Column('roleid', pytis.data.String(not_null=False)),
              sql.Column('rights', pytis.data.String(not_null=False)),
              sql.Column('columns', pytis.data.String(not_null=False)),
              sql.Column('purpose', pytis.data.String(not_null=False)),
              sql.Column('rights_show', pytis.data.Boolean(not_null=False)),
              sql.Column('rights_view', pytis.data.Boolean(not_null=False)),
              sql.Column('rights_insert', pytis.data.Boolean(not_null=False)),
              sql.Column('rights_update', pytis.data.Boolean(not_null=False)),
              sql.Column('rights_delete', pytis.data.Boolean(not_null=False)),
              sql.Column('rights_print', pytis.data.Boolean(not_null=False)),
              sql.Column('rights_export', pytis.data.Boolean(not_null=False)),
              sql.Column('rights_call', pytis.data.Boolean(not_null=False)),
              )
    depends_on = ()
    access_rights = ()


class PytisViewSummaryRights(sql.SQLFunction):
    name = 'pytis_view_summary_rights'
    schemas = dmp_schemas.value(globals())
    arguments = (sql.Column('', pytis.data.String()),
                 sql.Column('', pytis.data.String()),
                 sql.Column('', pytis.data.Boolean()),
                 sql.Column('', pytis.data.Boolean()),)
    result_type = TypPreviewSummaryRights
    multirow = True
    stability = 'VOLATILE'
    depends_on = (TypPreviewSummaryRights, PytisComputeSummaryRights, EPytisRoles,
                  CPytisRolePurposes,)
    access_rights = default_access_rights.value(globals())


class TypPreviewRoleMenu(sql.SQLType):
    name = 'typ_preview_role_menu'
    schemas = dmp_schemas.value(globals())
    fields = (sql.Column('menuid', pytis.data.Integer(not_null=False)),
              sql.Column('title', pytis.data.String(not_null=False)),
              sql.Column('position', pytis.data.LTree()),
              sql.Column('position_nsub', pytis.data.LargeInteger()),
              sql.Column('roleid', pytis.data.String(not_null=False)),
              sql.Column('rights', pytis.data.String(not_null=False)),
              sql.Column('rights_show', pytis.data.Boolean(not_null=False)),
              sql.Column('rights_view', pytis.data.Boolean(not_null=False)),
              sql.Column('rights_insert', pytis.data.Boolean(not_null=False)),
              sql.Column('rights_update', pytis.data.Boolean(not_null=False)),
              sql.Column('rights_delete', pytis.data.Boolean(not_null=False)),
              sql.Column('rights_print', pytis.data.Boolean(not_null=False)),
              sql.Column('rights_export', pytis.data.Boolean(not_null=False)),
              sql.Column('rights_call', pytis.data.Boolean(not_null=False)),
              )
    depends_on = ()
    access_rights = ()


class PytisViewRoleMenu(sql.SQLFunction):
    name = 'pytis_view_role_menu'
    schemas = dmp_schemas.value(globals())
    arguments = (sql.Column('', pytis.data.String()),
                 sql.Column('', pytis.data.Boolean()),)
    result_type = TypPreviewRoleMenu
    multirow = True
    stability = 'VOLATILE'
    depends_on = (TypPreviewRoleMenu, PytisComputeSummaryRights, EPytisRoles,
                  CPytisRolePurposes, EvPytisMenu,)
    access_rights = default_access_rights.value(globals())


class TypPreviewExtendedRoleMenu(sql.SQLType):
    name = 'typ_preview_extended_role_menu'
    schemas = dmp_schemas.value(globals())
    fields = (sql.Column('shortname', pytis.data.String(not_null=False)),
              sql.Column('position', pytis.data.LTree()),
              sql.Column('type', pytis.data.String(minlen=4, maxlen=4)),
              sql.Column('menuid', pytis.data.Integer(not_null=False)),
              sql.Column('next_position', pytis.data.LTree()),
              sql.Column('help', pytis.data.String(not_null=False)),
              sql.Column('hotkey', pytis.data.String(not_null=False)),
              sql.Column('locked', pytis.data.Boolean(not_null=False)),
              sql.Column('actiontype', pytis.data.String(not_null=False)),
              sql.Column('position_nsub', pytis.data.LargeInteger()),
              sql.Column('title', pytis.data.String(not_null=False)),
              sql.Column('fullname', pytis.data.String(not_null=False)),
              sql.Column('roleid', pytis.data.String(not_null=False)),
              sql.Column('rights', pytis.data.String(not_null=False)),
              sql.Column('rights_show', pytis.data.Boolean(not_null=False)),
              sql.Column('rights_view', pytis.data.Boolean(not_null=False)),
              sql.Column('rights_insert', pytis.data.Boolean(not_null=False)),
              sql.Column('rights_update', pytis.data.Boolean(not_null=False)),
              sql.Column('rights_delete', pytis.data.Boolean(not_null=False)),
              sql.Column('rights_print', pytis.data.Boolean(not_null=False)),
              sql.Column('rights_export', pytis.data.Boolean(not_null=False)),
              sql.Column('rights_call', pytis.data.Boolean(not_null=False)),
              )
    depends_on = ()
    access_rights = ()


class PytisViewExtendedRoleMenu(sql.SQLFunction):
    name = 'pytis_view_extended_role_menu'
    schemas = dmp_schemas.value(globals())
    arguments = (sql.Column('', pytis.data.String()),
                 sql.Column('', pytis.data.Boolean()),)
    result_type = TypPreviewExtendedRoleMenu
    multirow = True
    stability = 'VOLATILE'
    depends_on = (TypPreviewExtendedRoleMenu, APytisActionsStructure, EPytisMenu,
                  EvPytisValidRoles, PytisComputeSummaryRights, CPytisActionTypes,
                  PytisMultiformSpec, CPytisMenuActions,)
    access_rights = default_access_rights.value(globals())


class TypPreviewUserMenu(sql.SQLType):
    name = 'typ_preview_user_menu'
    schemas = dmp_schemas.value(globals())
    fields = (sql.Column('menuid', pytis.data.Integer(not_null=False)),
              sql.Column('name', pytis.data.String(not_null=False)),
              sql.Column('title', pytis.data.String(not_null=False)),
              sql.Column('position', pytis.data.LTree()),
              sql.Column('next_position', pytis.data.LTree()),
              sql.Column('fullname', pytis.data.String(not_null=False)),
              sql.Column('help', pytis.data.String(not_null=False)),
              sql.Column('hotkey', pytis.data.String(not_null=False)),
              sql.Column('locked', pytis.data.Boolean(not_null=False)),
              sql.Column('language', pytis.data.String(not_null=False)),
              )
    depends_on = ()
    access_rights = ()


class PytisViewUserMenu(sql.SQLFunction):
    name = 'pytis_view_user_menu'
    schemas = dmp_schemas.value(globals())
    arguments = ()
    result_type = TypPreviewUserMenu
    multirow = True
    stability = 'VOLATILE'
    depends_on = (TypPreviewUserMenu, EvPytisTranslatedMenu, PytisComputeSummaryRights,
                  PytisMultiformSpec, PytisUser,)
    access_rights = default_access_rights.value(globals())


class TypPreviewRights(sql.SQLType):
    name = 'typ_preview_rights'
    schemas = dmp_schemas.value(globals())
    fields = (sql.Column('shortname', pytis.data.String(not_null=False)),
              sql.Column('rights', pytis.data.String(not_null=False)),
              sql.Column('columns', pytis.data.String(not_null=False)),
              )
    depends_on = ()
    access_rights = ()


class PytisViewUserRights(sql.SQLFunction):
    name = 'pytis_view_user_rights'
    schemas = dmp_schemas.value(globals())
    arguments = ()
    result_type = TypPreviewRights
    multirow = True
    stability = 'VOLATILE'
    depends_on = (TypPreviewRights, PytisComputeSummaryRights, PytisUser,)
    access_rights = default_access_rights.value(globals())


class EvPytisUserRoles(sql.SQLView):
    name = 'ev_pytis_user_roles'
    schemas = dmp_schemas.value(globals())

    @classmethod
    def query(cls):
        members = sql.t.APytisValidRoleMembers.alias('members')
        roles = sql.t.EPytisRoles.alias('roles')
        return sqlalchemy.select(
            cls._exclude(members, 'member'),
            from_obj=[
                members.join(
                    roles, and_(
                        members.c.member == roles.c.name,
                        roles.c.purposeid == sqlalchemy.text("'user'")
                    )
                )
            ],
            whereclause=members.c.member == func.pytis_user(),
        )

    depends_on = (EPytisRoles, APytisValidRoleMembers, PytisUser,)
    access_rights = default_access_rights.value(globals())


class EvPytisUserSystemRights(sql.SQLView):
    name = 'ev_pytis_user_system_rights'
    schemas = dmp_schemas.value(globals())

    @classmethod
    def query(cls):
        rights = sql.t.EPytisActionRights.alias('rights')
        roles = sql.t.EvPytisUserRoles.alias('roles')
        return sqlalchemy.select(
            cls._exclude(rights),
            from_obj=[rights],
            whereclause=or_(and_(rights.c.system.is_(True), rights.c.roleid == sval('*')),
                            rights.c.roleid.in_(sqlalchemy.select([roles.c.roleid],
                                                                  from_obj=[roles]))),
        )

    insert_order = (EPytisActionRights,)
    update_order = (EPytisActionRights,)
    delete_order = (EPytisActionRights,)
    depends_on = (EPytisActionRights, EvPytisUserRoles,)
    access_rights = default_access_rights.value(globals())


class EvPytisColnames(sql.SQLRaw):
    name = 'ev_pytis_colnames'
    schemas = dmp_schemas.value(globals())

    @classmethod
    def sql(class_):
        return """
create or replace view ev_pytis_colnames as select distinct shortname, colname
from e_pytis_action_rights where colname is not null;

create or replace rule ev_pytis_colnames_insert as on insert to ev_pytis_colnames do instead
insert into e_pytis_action_rights (shortname, roleid, rightid, colname, system, granted)
       values (new.shortname, '__pytis', '*', new.colname, False, False);
"""
    depends_on = (EPytisActionRights,)


class PytisCheckCodebookRights(sql.SQLFunction):
    name = 'pytis_check_codebook_rights'
    schemas = dmp_schemas.value(globals())
    arguments = (sql.Column('', pytis.data.String()),
                 sql.Column('', pytis.data.String()),
                 sql.Column('', pytis.data.String()),
                 sql.Column('', pytis.data.Boolean()),)
    result_type = pytis.data.String()
    multirow = True
    stability = 'VOLATILE'
    depends_on = (PytisComputeSummaryRights,)
    access_rights = default_access_rights.value(globals())


class TypChangedRights(sql.SQLType):
    name = 'typ_changed_rights'
    schemas = dmp_schemas.value(globals())
    fields = (sql.Column('shortname', pytis.data.String(not_null=False)),
              sql.Column('roleid', pytis.data.String(not_null=False)),
              sql.Column('rights', pytis.data.String(not_null=False)),
              sql.Column('columns', pytis.data.String(not_null=False)),
              sql.Column('purpose', pytis.data.String(not_null=False)),
              sql.Column('rights_show', pytis.data.Boolean(not_null=False)),
              sql.Column('rights_view', pytis.data.Boolean(not_null=False)),
              sql.Column('rights_insert', pytis.data.Boolean(not_null=False)),
              sql.Column('rights_update', pytis.data.Boolean(not_null=False)),
              sql.Column('rights_delete', pytis.data.Boolean(not_null=False)),
              sql.Column('rights_print', pytis.data.Boolean(not_null=False)),
              sql.Column('rights_export', pytis.data.Boolean(not_null=False)),
              sql.Column('rights_call', pytis.data.Boolean(not_null=False)),
              sql.Column('change', pytis.data.Boolean(not_null=False)),
              )
    depends_on = ()
    access_rights = ()


class PytisChangedRights(sql.SQLFunction):
    name = 'pytis_changed_rights'
    schemas = dmp_schemas.value(globals())
    arguments = (sql.Column('', pytis.data.String()),
                 sql.Column('', pytis.data.String()),
                 sql.Column('', pytis.data.Boolean()),)
    result_type = TypChangedRights
    multirow = True
    stability = 'VOLATILE'
    depends_on = (TypChangedRights, PytisViewSummaryRights,)
    access_rights = default_access_rights.value(globals())


class PytisChangeShortname(Base_PyFunction):
    name = 'pytis_change_shortname'
    schemas = dmp_schemas.value(globals())
    arguments = (sql.Column('', pytis.data.String()),
                 sql.Column('', pytis.data.String()),
                 sql.Column('', pytis.data.String()),)
    result_type = pytis.data.String()
    multirow = False
    stability = 'VOLATILE'
    depends_on = ()
    access_rights = ()

    @staticmethod
    def pytis_change_shortname(shortname, old_name, new_name):
        shortname, old_name, new_name = args
        if shortname is None:
            return shortname
        components = shortname.split('/')
        if ((len(components) == 3 and
             components[0] in ('action', 'print',) and components[2] == old_name)):
            components[2] = new_name
        elif (len(components) == 2 and
              components[0] == 'form' and components[1] == old_name):
            components[1] = new_name
        return '/'.join(components)


class PytisChangeFullname(Base_PyFunction):
    name = 'pytis_change_fullname'
    schemas = dmp_schemas.value(globals())
    arguments = (sql.Column('', pytis.data.String()),
                 sql.Column('', pytis.data.String()),
                 sql.Column('', pytis.data.String()),)
    result_type = pytis.data.String()
    multirow = False
    stability = 'VOLATILE'
    depends_on = ()
    access_rights = ()

    @staticmethod
    def pytis_change_fullname(fullname, old_name, new_name):
        fullname, old_name, new_name = args
        if fullname is None:
            return fullname
        components = fullname.split('/')
        if ((len(components) == 3 and
             components[0] in ('action', 'print',) and components[2] == old_name)):
            components[2] = new_name
        elif (len(components) >= 3 and
              components[0] == 'form' and components[2] == old_name):
            components[2] = new_name
        elif (len(components) >= 5 and
              components[0] == 'sub' and components[4] == old_name):
            components[4] = new_name
        return '/'.join(components)


class PytisChangeSpecificationName(sql.SQLFunction):
    name = 'pytis_change_specification_name'
    schemas = dmp_schemas.value(globals())
    arguments = (sql.Column('', pytis.data.String()),
                 sql.Column('', pytis.data.String()),)
    result_type = None
    multirow = False
    stability = 'VOLATILE'
    depends_on = (PytisChangeShortname, PytisChangeFullname, EPytisDisabledDmpTriggers,
                  CPytisMenuActions, EPytisMenu, EPytisActionRights,
                  APytisActionsStructure,)
    access_rights = ()
