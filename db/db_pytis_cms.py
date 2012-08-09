#-*- coding: utf-8 -*-
"""Gensql specification of database objects used by Pytis CMS.

This specification is designed to be included in project specific gensql script.

Requirements:

The top level gensql script should define the following objects:

  * table (or view) `cms_users' with (at least) the following columns:
      uid -- user id used also as a reference from other tables (int, unique, not null),
      login -- web user's login name (text, unique, not null),
      fullname -- web user's displayed name (text, not null),
      passwd -- password for web user's authentication (text, not null)

  * variables:
      cms_users_table -- name of the table of web users.
      cms_rights -- DB rights to be used for CMS DB objects.  These rights should contain 'select'
        (read only) rights for the role used by the webserver (such as 'www-data').
      cms_rights_rw -- DB rigts for CMS objects requiring read/write access by the webserver user.
        These rights should contain read/write rights for the DB role used by the webserver (such
        as 'www-data').

"""

import pytis.data as pd
Relation = SelectRelation

table('cms_languages',
      doc="Codebook of languages available in the CMS.",
      columns=(PrimaryColumn('lang_id', pd.Serial()),
               Column('lang', pd.String(minlen=2, maxlen=2), constraints=('UNIQUE', 'NOT NULL'))),
      grant=cms_rights)

table('cms_modules',
      doc="Codebook of extension modules available in the CMS.",
      columns=(PrimaryColumn('mod_id', pd.Serial()),
               Column('modname', pd.String(maxlen=64), constraints=('UNIQUE', 'NOT NULL'))),
      grant=cms_rights)

table('cms_menu_structure',
      doc="Language independent menu structure.",
      columns=(PrimaryColumn('menu_item_id', pd.Serial()),
               Column('identifier', pd.String(maxlen=32), constraints=('UNIQUE', 'NOT NULL')),
               Column('parent', pd.Integer(), references='cms_menu_structure'),
               Column('mod_id', pd.Integer(), references='cms_modules',),
               Column('ord', pd.Integer(), constraints=('NOT NULL',)),
               Column('tree_order', pd.LTree())),
      depends=('cms_modules',),
      grant=cms_rights)

sql_raw("CREATE UNIQUE INDEX cms_menu_structure_unique_tree_order "
        "ON cms_menu_structure (ord, coalesce(parent, 0));",
        name='cms_menu_structure_unique_tree_order',
        depends=('cms_menu_structure',))

function('cms_menu_structure_tree_order',
         doc="Generate a sortable string representing the hierarchical position of given menu item.",
         arguments=(pd.Integer(),), output_type=pd.LTree(),
         body="""
           select case when parent is null then text2ltree('''')
                       else cms_menu_structure_tree_order(parent)
                  end || to_char(coalesce(ord, 999999), ''FM000000'')::text as result
           from cms_menu_structure where menu_item_id=$1""",
         depends=('cms_menu_structure', ))

table('cms_menu_texts',
      doc="Language dependent texts and properties for menu items.",
      columns=(Column('menu_item_id', pd.Integer(), constraints=('NOT NULL',),
                      references='cms_menu_structure ON DELETE CASCADE'),
               Column('lang',  pd.String(minlen=2, maxlen=2),  constraints=('NOT NULL',),
                      references='cms_languages(lang) ON DELETE CASCADE'),
               Column('published', pd.Boolean(), constraints=('NOT NULL',),
                      default="'TRUE'"),
               Column('title', pd.String(), constraints=('NOT NULL',)),
               Column('heading', pd.String()),
               Column('description', pd.String()),
               Column('content', pd.String())),
      depends=('cms_menu_structure', 'cms_languages',),
      grant=cms_rights,
      sql='PRIMARY KEY (menu_item_id, lang)')

viewng('cms_menu',
       doc="Complete menu structure with texts for each language defined in cms_languages.",
       relations=(Relation('cms_menu_structure', alias='s', key_column='menu_item_id'),
                  Relation('cms_languages', alias='l', key_column='lang',
                           jointype=JoinType.CROSS,exclude_columns=('lang_id',)),
                  Relation('cms_menu_texts', alias='t', key_column=('menu_item_id', 'lang'),
                           jointype=JoinType.LEFT_OUTER,
                           condition='t.menu_item_id = s.menu_item_id AND t.lang = l.lang',
                           exclude_columns=('menu_item_id', 'lang', 'published')),
                  Relation('cms_modules', alias='m', key_column='mod_id',
                           jointype=JoinType.LEFT_OUTER,
                           condition='m.mod_id = s.mod_id',
                           exclude_columns=('mod_id',))),
       include_columns=(ViewColumn(None, alias='menu_id', sql="s.menu_item_id ||'.'|| l.lang"),
                        ViewColumn(None, alias='published', sql="coalesce(t.published, 'FALSE')"),
                        ViewColumn(None, alias='title_or_identifier',
                                   sql="coalesce(t.title, s.identifier)"), 
                        ViewColumn(None, 'tree_order_nsub',
                                   "(select count(*)-1 from cms_menu_structure where tree_order <@ s.tree_order)"),
                        ),
       insert="""(
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
       )""",
       update="""(
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
       )""",
       delete="(DELETE FROM cms_menu_structure WHERE menu_item_id = old.menu_item_id;)",
       depends=('cms_menu_structure', 'cms_languages', 'cms_menu_texts', 'cms_modules'),
       grant=cms_rights)

table('cms_roles',
      doc="CMS roles.",
      columns=(PrimaryColumn('role_id', pd.Serial()),
               Column('name', pd.String(), constraints=('NOT NULL',)),
               Column('system_role', pd.String(), constraints=('UNIQUE',)),
               Column('description', pd.String())),
      grant=cms_rights)

table('cms_user_role_assignment',
      doc="Binding table assigning CMS roles to CMS users.",
      columns=(PrimaryColumn('user_role_id', pd.Serial()),
               Column('uid', pd.Integer(), constraints=('NOT NULL',),
                      references=cms_users_table + ' ON DELETE CASCADE'),
               Column('role_id', pd.Integer(), constraints=('NOT NULL',),
                      references='cms_roles ON DELETE CASCADE')),
      sql='UNIQUE (uid, role_id)',
      depends=(cms_users_table, 'cms_roles'),
      grant=cms_rights)

viewng('cms_user_roles',
       relations=(Relation('cms_user_role_assignment', alias='a', key_column='user_role_id'),
                  Relation('cms_users', alias='u', key_column='uid', exclude_columns='*',
                           jointype=JoinType.INNER, condition='a.uid = u.uid'),
                  Relation('cms_roles', alias='r', key_column='role_id',
                           exclude_columns=('role_id',),
                           jointype=JoinType.INNER, condition='a.role_id = r.role_id')),
       include_columns=(ViewColumn('u.login'),
                        ViewColumn('u.fullname')),
       insert=("INSERT INTO cms_user_role_assignment (user_role_id, uid, role_id) "
               "VALUES (new.user_role_id, new.uid, new.role_id) "
               "RETURNING user_role_id, uid, role_id, "
                         "NULL::text, NULL::text, NULL::text, NULL::text, NULL::text"),
       update=("UPDATE cms_user_role_assignment SET uid = new.uid, role_id = new.role_id "
               "WHERE user_role_id=old.user_role_id"),
       delete=("DELETE FROM cms_user_role_assignment WHERE user_role_id = old.user_role_id"),
       depends=('cms_user_role_assignment', 'cms_users', 'cms_roles'),
       grant=cms_rights)

table('cms_actions',
      doc=("Enumeration of valid actions (including both module independent actions and per "
           "module actions).  Module independent actions have NULL in the mod_id column."),
      columns=(PrimaryColumn('action_id', pd.Serial()),
               Column('mod_id', pd.Integer(), references='cms_modules ON DELETE CASCADE'),
               Column('name', pd.String(maxlen=16), constraints=('NOT NULL',)),
               Column('description', pd.String(), constraints=('NOT NULL',))),
      sql='UNIQUE (mod_id, name)',
      depends=('cms_modules',),
      grant=cms_rights)

table('cms_rights_assignment',
      doc="Underlying binding table between menu items, roles and module actions.",
      columns=(PrimaryColumn('rights_assignment_id', pd.Serial()),
               Column('menu_item_id', pd.Integer(), constraints=('NOT NULL',),
                      references='cms_menu_structure ON DELETE CASCADE'),
               Column('role_id', pd.Integer(), constraints=('NOT NULL',),
                      references='cms_roles ON DELETE CASCADE'),
               Column('action_id', pd.Integer(), constraints=('NOT NULL',),
                      references='cms_actions ON DELETE CASCADE')),
      sql='UNIQUE (menu_item_id, role_id, action_id)',
      depends=('cms_menu_structure', 'cms_roles', 'cms_actions'),
      grant=cms_rights)

viewng('cms_rights',
       doc="User editable access rights assignment.",
       relations=(Relation('cms_rights_assignment', alias='x', key_column='rights_assignment_id'),
                  Relation('cms_menu_structure', alias='s', jointype=JoinType.INNER,
                           condition='s.menu_item_id=x.menu_item_id', exclude_columns=('*',)),
                  Relation('cms_roles', alias='r', jointype=JoinType.INNER,
                           condition='r.role_id = x.role_id', exclude_columns=('*',)),
                  Relation('cms_actions', alias='a', jointype=JoinType.INNER,
                           condition='a.action_id = x.action_id', exclude_columns=('*',)),
                  ),
       include_columns=(ViewColumn('r.name', alias='role_name'),
                        ViewColumn('s.mod_id'),
                        ViewColumn('r.description', alias='role_description'),
                        ViewColumn('r.system_role'),
                        ViewColumn('a.name', alias='action_name'),
                        ViewColumn('a.description', alias='action_description'),
                        ),
       insert_order=('cms_rights_assignment',),
       update_order=('cms_rights_assignment',),
       delete_order=('cms_rights_assignment',),
       depends=('cms_rights_assignment', 'cms_menu_structure', 'cms_roles', 'cms_actions'),
       grant=cms_rights)

table('cms_session',
      doc="Web user session information for authentication and login history.",
      columns=(PrimaryColumn('session_id', pd.Serial()), 
               Column('uid', pd.Integer(), constraints=('NOT NULL',),
                      references=cms_users_table + ' ON DELETE CASCADE'),
               Column('session_key', pd.String(), constraints=('NOT NULL',)),
               Column('last_access', pd.DateTime(), constraints=('NOT NULL',))),
      sql="UNIQUE (uid, session_key)",
      depends=(cms_users_table,),
      grant=cms_rights_rw)

table('cms_session_log_data',
      doc= "Log of web user logins (underlying data).",
      columns=(PrimaryColumn('log_id', pd.Serial()),
               Column('session_id', pd.Integer(),
                      references='cms_session ON DELETE SET NULL'),
               Column('uid', pd.Integer(), # may be null for invalid logins
                      references=cms_users_table + ' ON DELETE CASCADE'),
               Column('login', pd.String(), constraints=('NOT NULL',)),
               Column('success', pd.Boolean(), constraints=('NOT NULL',), default="False"),
               Column('start_time', pd.DateTime(), constraints=('NOT NULL',)),
               Column('end_time', pd.DateTime()),
               Column('ip_address', pd.String(), constraints=('NOT NULL',)),
               Column('user_agent', pd.String()),
               Column('referer', pd.String())),
      depends=(cms_users_table,),
      grant=cms_rights_rw)

table('cms_access_log_data',
      doc= "Log of cms page access.",
      columns=(PrimaryColumn('log_id', pd.Serial()),
               Column('timestamp', pd.DateTime(), constraints=('NOT NULL',)),
               Column('uri', pd.String(), constraints=('NOT NULL',)),
               Column('uid', pd.Integer(), references=cms_users_table + ' ON DELETE CASCADE'),
               Column('modname', pd.String()),
               Column('action', pd.String()),
               Column('ip_address', pd.String(), constraints=('NOT NULL',)),
               Column('user_agent', pd.String()),
               Column('referer', pd.String())),
      depends=(cms_users_table,),
      grant=cms_rights_rw)

sql_raw("create or replace rule session_delete as on delete to cms_session do ( "
        "update cms_session_log_data set end_time=old.last_access WHERE session_id=old.session_id;"
        ");",
        depends=('cms_session', 'cms_session_log_data'))

viewng('cms_session_log',
       doc= "Log of web user logins (user visible information).",
       relations=(Relation('cms_session_log_data', alias='l', key_column='log_id',
                            exclude_columns=('end_time',)),
                  Relation('cms_session', alias='s', key_column='session_id', exclude_columns='*',
                           jointype=JoinType.LEFT_OUTER, condition='l.session_id = s.session_id'),
                  Relation('cms_users', alias='u', key_column='uid', exclude_columns='*',
                           jointype=JoinType.INNER, condition='l.uid = u.uid')),
       include_columns=(ViewColumn('u.fullname'),
                        ViewColumn(None, alias='duration',
                                   sql="coalesce(l.end_time, s.last_access) - l.start_time"),
                        ViewColumn(None, alias='active',
                                   sql="s.session_id IS NOT NULL AND age(s.last_access)<'1 hour'"),
                        ),
       insert=("""INSERT INTO cms_session_log_data (session_id, uid, login, success, 
                                                    start_time, ip_address, user_agent, referer)
               VALUES (new.session_id, new.uid, new.login, new.success, 
                       new.start_time, new.ip_address, new.user_agent, new.referer)
               RETURNING log_id, session_id, uid, login, success, 
                         start_time, ip_address, user_agent, referer,
                         NULL::text, NULL::interval, NULL::boolean"""),
       update_order=('cms_session_log_data',),
       delete_order=('cms_session_log_data',),
       depends=('cms_session', 'cms_session_log_data', cms_users_table, 'cms_users',),
       grant=cms_rights)

table('cms_themes',
      doc= "Definition of available color themes.",
      columns=(PrimaryColumn('theme_id', pd.Serial()),
               Column('name', pd.String(), constraints=('UNIQUE', 'NOT NULL',)),
               Column('foreground', pd.Color()),
               Column('background', pd.Color()),
               Column('border', pd.Color()),
               Column('heading_fg', pd.Color()),
               Column('heading_bg', pd.Color()),
               Column('heading_line', pd.Color()),
               Column('frame_fg', pd.Color()),
               Column('frame_bg', pd.Color()),
               Column('frame_border', pd.Color()),
               Column('link', pd.Color()),
               Column('link_visited', pd.Color()),
               Column('link_hover', pd.Color()),
               Column('meta_fg', pd.Color()),
               Column('meta_bg', pd.Color()),
               Column('help', pd.Color()),
               Column('error_fg', pd.Color()),
               Column('error_bg', pd.Color()),
               Column('error_border', pd.Color()),
               Column('message_fg', pd.Color()),
               Column('message_bg', pd.Color()),
               Column('message_border', pd.Color()),
               Column('table_cell', pd.Color()),
               Column('table_cell2', pd.Color()),
               Column('top_fg', pd.Color()),
               Column('top_bg', pd.Color()),
               Column('top_border', pd.Color()),
               Column('highlight_bg', pd.Color()),
               Column('inactive_folder', pd.Color())),
      grant=cms_rights)
