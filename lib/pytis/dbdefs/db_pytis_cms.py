# -*- coding: utf-8

from __future__ import unicode_literals

import sqlalchemy
import pytis.data.gensqlalchemy as sql
import pytis.data
from pytis.dbdefs.db_pytis_base import cms_rights, cms_schemas, cms_users_table, cms_rights_rw


class CmsLanguages(sql.SQLTable):
    """Codebook of languages available in the CMS."""
    name = 'cms_languages'
    schemas = cms_schemas.value(globals())
    fields = (
        sql.PrimaryColumn('lang_id', pytis.data.Serial()),
        sql.Column('lang', pytis.data.String(minlen=2, maxlen=2, not_null=True), unique=True),
    )
    depends_on = ()
    access_rights = cms_rights.value(globals())


class CmsModules(sql.SQLTable):
    """Codebook of extension modules available in the CMS."""
    name = 'cms_modules'
    schemas = cms_schemas.value(globals())
    fields = (
        sql.PrimaryColumn('mod_id', pytis.data.Serial()),
        sql.Column('modname', pytis.data.String(maxlen=64, not_null=True), unique=True),
    )
    depends_on = ()
    access_rights = cms_rights.value(globals())


class CmsMenuStructure(sql.SQLTable):
    """Language independent menu structure."""
    name = 'cms_menu_structure'
    schemas = cms_schemas.value(globals())
    fields = (sql.PrimaryColumn('menu_item_id', pytis.data.Serial()),
              sql.Column('identifier', pytis.data.String(maxlen=32, not_null=True), unique=True),
              sql.Column('parent', pytis.data.Integer(not_null=False),
                         references=sql.gA('cms_menu_structure')),
              sql.Column('mod_id', pytis.data.Integer(not_null=False),
                         references=sql.gA('cms_modules')),
              sql.Column('ord', pytis.data.Integer(not_null=True)),
              sql.Column('tree_order', pytis.data.LTree(not_null=False)),
              )
    index_columns = (
        # ('ord', sqlalchemy.literal_column('coalesce(parent, 0)'),),
        ('parent', 'ord',),
    )
    depends_on = (CmsModules,)
    access_rights = cms_rights.value(globals())


class CmsMenuStructureUniqueTreeOrder(sql.SQLRaw):
    name = 'cms_menu_structure_unique_tree_order'
    schemas = cms_schemas.value(globals())

    @classmethod
    def sql(class_):
        return ("CREATE UNIQUE INDEX cms_menu_structure_unique_tree_order "
                "ON cms_menu_structure (ord, coalesce(parent, 0));")
    depends_on = (CmsMenuStructure,)


class CmsMenuStructureTreeOrder(sql.SQLFunction):
    """Generate a sortable string representing the hierarchical position of given menu item."""
    schemas = cms_schemas.value(globals())
    name = 'cms_menu_structure_tree_order'
    arguments = (sql.Column('', pytis.data.Integer()),)
    result_type = pytis.data.LTree()
    multirow = False
    stability = 'VOLATILE'
    depends_on = (CmsMenuStructure,)
    access_rights = ()


class CmsMenuTexts(sql.SQLTable):
    """Language dependent texts and properties for menu items."""
    name = 'cms_menu_texts'
    schemas = cms_schemas.value(globals())
    fields = (sql.Column('menu_item_id', pytis.data.Integer(not_null=True),
                         references=sql.gA('cms_menu_structure', ondelete='CASCADE')),
              sql.Column('lang', pytis.data.String(minlen=2, maxlen=2, not_null=True),
                         references=sql.gA('cms_languages(lang)', ondelete='CASCADE')),
              sql.Column('published', pytis.data.Boolean(not_null=True), default='TRUE'),
              sql.Column('title', pytis.data.String(not_null=True)),
              sql.Column('heading', pytis.data.String(not_null=False)),
              sql.Column('description', pytis.data.String(not_null=False)),
              sql.Column('content', pytis.data.String(not_null=False)),
              )
    depends_on = (CmsMenuStructure, CmsLanguages,)
    access_rights = cms_rights.value(globals())


class CmsMenu(sql.SQLView):
    """Complete menu structure with texts for each language defined in cms_languages."""
    name = 'cms_menu'
    schemas = cms_schemas.value(globals())

    @classmethod
    def query(cls):
        struc = sql.t.CmsMenuStructure.alias('s')
        lang = sql.t.CmsLanguages.alias('l')
        texts = sql.t.CmsMenuTexts.alias('t')
        mod = sql.t.CmsModules.alias('m')
        return sqlalchemy.select(
            cls._exclude(struc) +
            cls._exclude(lang, 'lang_id') +
            cls._exclude(texts, 'menu_item_id', 'lang', 'published') +
            cls._exclude(mod, 'mod_id') +
            [sql.gL("s.menu_item_id ||'.'|| l.lang").label('menu_id'),
             sql.gL("coalesce(t.published, 'FALSE')").label('published'),
             sql.gL("coalesce(t.title, s.identifier)").label('title_or_identifier'),
             sql.gL("(select count(*)-1 from cms_menu_structure "
                    "where tree_order <@ s.tree_order)").label('tree_order_nsub')],
            from_obj=[struc.join(lang, sqlalchemy.sql.true()).
                      outerjoin(texts, sql.gR('t.menu_item_id = s.menu_item_id '
                                              'AND t.lang = l.lang')).
                      outerjoin(mod, sql.gR('m.mod_id = s.mod_id'))]
        )

    def on_insert(self):
        return ("""(
       INSERT INTO cms_menu_structure (identifier, parent, mod_id, ord)
       VALUES (new.identifier, new.parent, new.mod_id,
               coalesce(new.ord, (SELECT max(ord)+100 FROM cms_menu_structure
                                  WHERE coalesce(parent, 0)=coalesce(new.parent, 0)), 100));
       UPDATE cms_menu_structure SET tree_order = cms_menu_structure_tree_order(menu_item_id);
       INSERT INTO cms_menu_texts (menu_item_id, lang, published,
                                   title, heading, description, content)
       SELECT (SELECT menu_item_id FROM cms_menu_structure WHERE identifier=new.identifier),
              new.lang, new.published, new.title, new.heading, new.description, new.content
       RETURNING
          menu_item_id, NULL::varchar(32), NULL::int, NULL::int, NULL::int, NULL::ltree,
          lang, title, heading, description, content, NULL::varchar(64),
          menu_item_id ||'.'|| lang, published, title, 0::bigint
       )""",)

    def on_update(self):
        return ("""(
       UPDATE cms_menu_structure SET
         identifier = new.identifier,
         parent = new.parent,
         mod_id = new.mod_id,
         ord = new.ord
       WHERE cms_menu_structure.menu_item_id = old.menu_item_id;
       UPDATE cms_menu_structure SET tree_order = cms_menu_structure_tree_order(menu_item_id);
       UPDATE cms_menu_texts SET
         published = new.published,
         title = new.title,
         heading = new.heading,
         description = new.description,
         content = new.content
       WHERE menu_item_id = old.menu_item_id AND lang = new.lang;
       INSERT INTO cms_menu_texts (menu_item_id, lang, published,
                                   title, heading, description, content)
         SELECT old.menu_item_id, new.lang, new.published,
                new.title, new.heading, new.description, new.content
         WHERE new.lang NOT IN (SELECT lang FROM cms_menu_texts WHERE menu_item_id=old.menu_item_id)
                AND coalesce(new.title, new.heading, new.description, new.content) IS NOT NULL;
       )""",)

    def on_delete(self):
        return ("(DELETE FROM cms_menu_structure WHERE menu_item_id = old.menu_item_id;)",)
    depends_on = (CmsMenuStructure, CmsLanguages, CmsMenuTexts, CmsModules,)
    access_rights = cms_rights.value(globals())


class CmsRoles(sql.SQLTable):
    """CMS roles."""
    name = 'cms_roles'
    schemas = cms_schemas.value(globals())
    fields = (sql.PrimaryColumn('role_id', pytis.data.Serial()),
              sql.Column('name', pytis.data.String(not_null=True)),
              sql.Column('system_role', pytis.data.String(not_null=False), unique=True),
              sql.Column('description', pytis.data.String(not_null=False)),
              )
    depends_on = ()
    access_rights = cms_rights.value(globals())
    init_columns = ('system_role', 'name')
    init_values = (
        ('ANYONE', 'Anyone'),
        ('USER', 'Logged user'),
    )


class CmsActions(sql.SQLTable):
    """Enumeration of valid actions.
    (Including both module independent actions and per module actions.)
    Module independent actions have NULL in the mod_id column.
    """
    name = 'cms_actions'
    schemas = cms_schemas.value(globals())
    fields = (sql.PrimaryColumn('action_id', pytis.data.Serial()),
              sql.Column('mod_id', pytis.data.Integer(not_null=False),
                         references=sql.gA('cms_modules', ondelete='CASCADE')),
              sql.Column('name', pytis.data.String(maxlen=16, not_null=True)),
              sql.Column('description', pytis.data.String(not_null=True)),
              )
    unique = (('mod_id', 'name',),)
    depends_on = (CmsModules,)
    access_rights = cms_rights.value(globals())
    init_columns = ('name', 'description')
    init_values = (
        ('visit', 'Display the item content'),
        ('show', 'See the item in the menu'),
    )


class CmsRightsAssignment(sql.SQLTable):
    """Underlying binding table between menu items, roles and module actions."""
    name = 'cms_rights_assignment'
    schemas = cms_schemas.value(globals())
    fields = (sql.PrimaryColumn('rights_assignment_id', pytis.data.Serial()),
              sql.Column('menu_item_id', pytis.data.Integer(not_null=True),
                         references=sql.gA('cms_menu_structure', ondelete='CASCADE')),
              sql.Column('role_id', pytis.data.Integer(not_null=True),
                         references=sql.gA('cms_roles', ondelete='CASCADE')),
              sql.Column('action_id', pytis.data.Integer(not_null=True),
                         references=sql.gA('cms_actions', ondelete='CASCADE')),
              )
    unique = (('menu_item_id', 'role_id', 'action_id',),)
    depends_on = (CmsMenuStructure, CmsRoles, CmsActions,)
    access_rights = cms_rights.value(globals())


class CmsRights(sql.SQLView):
    """User editable access rights assignment."""
    name = 'cms_rights'
    schemas = cms_schemas.value(globals())

    @classmethod
    def query(cls):
        x = sql.t.CmsRightsAssignment.alias('x')
        s = sql.t.CmsMenuStructure.alias('s')
        r_ = sql.t.CmsRoles.alias('r')
        a_ = sql.t.CmsActions.alias('a')
        return sqlalchemy.select(
            cls._exclude(x) +
            [r_.c.name.label('role_name'),
             s.c.mod_id.label('mod_id'),
             r_.c.description.label('role_description'),
             r_.c.system_role.label('system_role'),
             a_.c.name.label('action_name'),
             a_.c.description.label('action_description')],
            from_obj=[x.join(s, sql.gR('s.menu_item_id=x.menu_item_id')).
                      join(r_, sql.gR('r.role_id = x.role_id')).
                      join(a_, sql.gR('a.action_id = x.action_id'))]
        )

    insert_order = (CmsRightsAssignment,)
    update_order = (CmsRightsAssignment,)
    delete_order = (CmsRightsAssignment,)
    depends_on = (CmsRightsAssignment, CmsMenuStructure, CmsRoles, CmsActions,)
    access_rights = cms_rights.value(globals())


class CmsThemes(sql.SQLTable):
    """Definition of available color themes."""
    name = 'cms_themes'
    schemas = cms_schemas.value(globals())
    fields = (sql.PrimaryColumn('theme_id', pytis.data.Serial()),
              sql.Column('name', pytis.data.String(not_null=True), unique=True),
              sql.Column('foreground', pytis.data.Color(not_null=False)),
              sql.Column('background', pytis.data.Color(not_null=False)),
              sql.Column('border', pytis.data.Color(not_null=False)),
              sql.Column('heading_fg', pytis.data.Color(not_null=False)),
              sql.Column('heading_bg', pytis.data.Color(not_null=False)),
              sql.Column('heading_line', pytis.data.Color(not_null=False)),
              sql.Column('frame_fg', pytis.data.Color(not_null=False)),
              sql.Column('frame_bg', pytis.data.Color(not_null=False)),
              sql.Column('frame_border', pytis.data.Color(not_null=False)),
              sql.Column('link', pytis.data.Color(not_null=False)),
              sql.Column('link_visited', pytis.data.Color(not_null=False)),
              sql.Column('link_hover', pytis.data.Color(not_null=False)),
              sql.Column('meta_fg', pytis.data.Color(not_null=False)),
              sql.Column('meta_bg', pytis.data.Color(not_null=False)),
              sql.Column('help', pytis.data.Color(not_null=False)),
              sql.Column('error_fg', pytis.data.Color(not_null=False)),
              sql.Column('error_bg', pytis.data.Color(not_null=False)),
              sql.Column('error_border', pytis.data.Color(not_null=False)),
              sql.Column('message_fg', pytis.data.Color(not_null=False)),
              sql.Column('message_bg', pytis.data.Color(not_null=False)),
              sql.Column('message_border', pytis.data.Color(not_null=False)),
              sql.Column('table_cell', pytis.data.Color(not_null=False)),
              sql.Column('table_cell2', pytis.data.Color(not_null=False)),
              sql.Column('top_fg', pytis.data.Color(not_null=False)),
              sql.Column('top_bg', pytis.data.Color(not_null=False)),
              sql.Column('top_border', pytis.data.Color(not_null=False)),
              sql.Column('highlight_bg', pytis.data.Color(not_null=False)),
              sql.Column('inactive_folder', pytis.data.Color(not_null=False)),
              )
    depends_on = ()
    access_rights = cms_rights.value(globals())


class CmsUsersTable(sql.SQLTable):
    name = 'cms_users_table'
    schemas = cms_schemas.value(globals())
    fields = (sql.PrimaryColumn('uid', pytis.data.Serial()),)
    depends_on = ()
    access_rights = ()


class CmsUserRoleAssignment(sql.SQLTable):
    """Binding table assigning CMS roles to CMS users."""
    name = 'cms_user_role_assignment'
    schemas = cms_schemas.value(globals())
    fields = (sql.PrimaryColumn('user_role_id', pytis.data.Serial()),
              sql.Column('uid', pytis.data.Integer(not_null=True),
                         references=sql.gA(cms_users_table.value(globals()), ondelete='CASCADE')),
              sql.Column('role_id', pytis.data.Integer(not_null=True),
                         references=sql.gA('cms_roles', ondelete='CASCADE')),
              )
    unique = (('uid', 'role_id',),)
    depends_on = (CmsRoles,)
    access_rights = cms_rights.value(globals())


class CmsSession(sql.SQLTable):
    """Web user session information for authentication and login history."""
    name = 'cms_session'
    schemas = cms_schemas.value(globals())
    fields = (sql.PrimaryColumn('session_id', pytis.data.Serial()),
              sql.Column('uid', pytis.data.Integer(not_null=True),
                         references=sql.gA(cms_users_table.value(globals()), ondelete='CASCADE')),
              sql.Column('session_key', pytis.data.String(not_null=True)),
              sql.Column('last_access', pytis.data.DateTime(not_null=True)),
              )
    unique = (('uid', 'session_key',),)
    depends_on = ()
    access_rights = cms_rights_rw.value(globals())


class CmsSessionLogData(sql.SQLTable):
    """Log of web user logins (underlying data)."""
    name = 'cms_session_log_data'
    schemas = cms_schemas.value(globals())
    fields = (sql.PrimaryColumn('log_id', pytis.data.Serial()),
              sql.Column('session_id', pytis.data.Integer(not_null=False),
                         references=sql.gA('cms_session', ondelete='SET NULL')),
              sql.Column('uid', pytis.data.Integer(not_null=False),
                         references=sql.gA(cms_users_table.value(globals()), ondelete='CASCADE')),
              sql.Column('login', pytis.data.String(not_null=True)),
              sql.Column('success', pytis.data.Boolean(not_null=True), default=False),
              sql.Column('start_time', pytis.data.DateTime(not_null=True)),
              sql.Column('end_time', pytis.data.DateTime(not_null=False)),
              sql.Column('ip_address', pytis.data.String(not_null=True)),
              sql.Column('user_agent', pytis.data.String(not_null=False)),
              sql.Column('referer', pytis.data.String(not_null=False)),
              )
    depends_on = ()
    access_rights = cms_rights_rw.value(globals())


class CmsAccessLogData(sql.SQLTable):
    """Log of cms page access."""
    name = 'cms_access_log_data'
    schemas = cms_schemas.value(globals())
    fields = (sql.PrimaryColumn('log_id', pytis.data.Serial()),
              sql.Column('timestamp', pytis.data.DateTime(not_null=True)),
              sql.Column('uri', pytis.data.String(not_null=True)),
              sql.Column('uid', pytis.data.Integer(not_null=False),
                         references=sql.gA(cms_users_table.value(globals()), ondelete='CASCADE')),
              sql.Column('modname', pytis.data.String(not_null=False)),
              sql.Column('action', pytis.data.String(not_null=False)),
              sql.Column('ip_address', pytis.data.String(not_null=True)),
              sql.Column('user_agent', pytis.data.String(not_null=False)),
              sql.Column('referer', pytis.data.String(not_null=False)),
              )
    depends_on = ()
    access_rights = cms_rights_rw.value(globals())


class X186(sql.SQLRaw):
    name = '@186'
    schemas = cms_schemas.value(globals())

    @classmethod
    def sql(class_):
        return ("create or replace rule session_delete as on delete to cms_session do "
                "( update cms_session_log_data set end_time=old.last_access "
                "WHERE session_id=old.session_id;)")
    depends_on = (CmsSession, CmsSessionLogData,)


class CmsUsers(sql.SQLTable):
    name = 'pytis_cms_users'
    schemas = cms_schemas.value(globals())
    fields = (sql.PrimaryColumn('uid', pytis.data.Serial()),
              sql.Column('login', pytis.data.String(not_null=True), unique=True),
              sql.Column('fullname', pytis.data.String(not_null=True)),
              sql.Column('passwd', pytis.data.String(not_null=True)),
              )
    depends_on = ()
    access_rights = ()


class CmsUserRoles(sql.SQLView):
    name = 'cms_user_roles'
    schemas = cms_schemas.value(globals())

    @classmethod
    def query(cls):
        a_ = sql.t.CmsUserRoleAssignment.alias('a')
        u = sql.t.CmsUsers.alias('u')
        r_ = sql.t.CmsRoles.alias('r')
        return sqlalchemy.select(
            cls._exclude(a_) +
            cls._exclude(r_, 'role_id') +
            [u.c.login.label('login'),
             u.c.fullname.label('fullname')],
            from_obj=[
                a_
                .join(u, sql.gR('a.uid = u.uid'))
                .join(r_, sql.gR('a.role_id = r.role_id')),
            ]
        )

    def on_insert(self):
        return ("INSERT INTO cms_user_role_assignment (user_role_id, uid, role_id) "
                "VALUES (new.user_role_id, new.uid, new.role_id) "
                "RETURNING user_role_id, uid, role_id, NULL::text, NULL::text, NULL::text, "
                "NULL::text, NULL::text",)

    def on_update(self):
        return ("UPDATE cms_user_role_assignment SET uid = new.uid, role_id = new.role_id "
                "WHERE user_role_id=old.user_role_id",)

    def on_delete(self):
        return ("DELETE FROM cms_user_role_assignment WHERE user_role_id = old.user_role_id",)
    depends_on = (CmsUserRoleAssignment, CmsUsers, CmsRoles,)
    access_rights = cms_rights.value(globals())


class CmsSessionLog(sql.SQLView):
    """Log of web user logins (user visible information)."""
    name = 'cms_session_log'
    schemas = cms_schemas.value(globals())

    @classmethod
    def query(cls):
        log = sql.t.CmsSessionLogData.alias('l')
        session = sql.t.CmsSession.alias('s')
        users = sql.t.CmsUsers.alias('u')
        return sqlalchemy.select(
            cls._exclude(log, 'end_time') +
            [users.c.fullname.label('fullname'),
             sql.gL("coalesce(l.end_time, s.last_access) - l.start_time").label('duration'),
             sql.gL("s.session_id IS NOT NULL AND age(s.last_access)<'1 hour'").label('active')],
            from_obj=[log.outerjoin(session, sql.gR('l.session_id = s.session_id')).
                      join(users, sql.gR('l.uid = u.uid'))]
        )

    def on_insert(self):
        return ("""INSERT INTO cms_session_log_data (session_id, uid, login, success,
                                                    start_time, ip_address, user_agent, referer)
               VALUES (new.session_id, new.uid, new.login, new.success,
                       new.start_time, new.ip_address, new.user_agent, new.referer)
               RETURNING log_id, session_id, uid, login, success,
                         start_time, ip_address, user_agent, referer,
                         NULL::text, NULL::interval, NULL::boolean""",)
    update_order = (CmsSessionLogData,)
    no_update_columns = ('duration', 'active',)
    delete_order = (CmsSessionLogData,)
    depends_on = (CmsSession, CmsSessionLogData, CmsUsers,)
    access_rights = cms_rights.value(globals())
