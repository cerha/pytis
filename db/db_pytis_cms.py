"""Gensql specification of database objects used by Pytis CMS.

This specification is designed to be included in project specific gensql script.

Requirements:

The top level gensql script should define the following objects:

  * table (or view) `cms_users' with (at least) the following columns:
      uid -- user id used also as a reference from other tables (integer, unique, not null),
      login -- web user's login name (string, unique, not null),
      fullname -- web user's displayed name (string, not null),
      passwd -- password for web user's authentication (string, not null)

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
               Column('tree_order', pd.String())),
      depends=('cms_modules',),
      grant=cms_rights)

sql_raw("CREATE UNIQUE INDEX cms_menu_structure_unique_tree_order "
        "ON cms_menu_structure (ord, coalesce(parent, 0));",
        name='cms_menu_structure_unique_tree_order',
        depends=('cms_menu_structure',))

function('cms_menu_structure_tree_order',
         doc="Generate a sortable string representing the hierarchical position of given menu item.",
         arguments=(pd.Integer(),), output_type=pd.String(),
         body="""
         SELECT CASE WHEN $1 IS NULL THEN '''' ELSE
           (SELECT cms_menu_structure_tree_order(parent) || ''.'' ||
                   to_char(coalesce(ord, 999999), ''FM000000'')
            FROM cms_menu_structure where menu_item_id=$1)
         END AS RESULT""",
         depends=('cms_menu_structure', ))

table('cms_menu_texts',
      doc="Language dependent texts and properties for menu items.",
      columns=(Column('menu_item_id', pd.Integer(), constraints=('NOT NULL',),
                      references='cms_menu_structure ON DELETE CASCADE'),
               Column('lang',  pd.String(minlen=2, maxlen=2),  constraints=('NOT NULL',),
                      references='cms_languages(lang)'),
               Column('published', pd.Boolean(), constraints=('NOT NULL',),
                      default="'TRUE'"),
               Column('title', pd.String(), constraints=('NOT NULL',)),
               Column('description', pd.String()),
               Column('content', pd.String())),
      depends=('cms_menu_structure', 'cms_languages',),
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
                        ),
       insert="""(
       INSERT INTO cms_menu_structure (identifier, parent, mod_id, ord)
       VALUES (new.identifier, new.parent, new.mod_id,
               coalesce(new.ord, (SELECT max(ord)+100 FROM cms_menu_structure 
                                  WHERE coalesce(parent, 0)=coalesce(new.parent, 0)), 100));
       UPDATE cms_menu_structure SET tree_order = cms_menu_structure_tree_order(menu_item_id);
       INSERT INTO cms_menu_texts (menu_item_id, lang, published, title, description, content)
       SELECT (SELECT menu_item_id FROM cms_menu_structure WHERE identifier=new.identifier),
              new.lang, new.published, new.title, new.description, new.content
       RETURNING 
          menu_item_id, NULL::varchar(32), NULL::int, NULL::int, NULL::int, NULL::text, 
          lang, title, description, content, NULL::varchar(64), 
          menu_item_id ||'.'|| lang, published, title
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
         description = new.description,
         content = new.content
       WHERE menu_item_id = old.menu_item_id AND lang = new.lang;
       INSERT INTO cms_menu_texts (menu_item_id, lang, published, title, description, content)
         SELECT old.menu_item_id, new.lang, new.published, new.title, new.description, new.content
           WHERE new.lang NOT IN (SELECT lang FROM cms_menu_texts WHERE menu_item_id=old.menu_item_id)
                 AND (new.title IS NOT NULL OR new.description IS NOT NULL OR new.content IS NOT NULL);
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
                      references=cms_users_table),
               Column('role_id', pd.Integer(), constraints=('NOT NULL',),
                      references='cms_roles')),
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
               Column('mod_id', pd.Integer(), references='cms_modules'),
               Column('name', pd.String(maxlen=16), constraints=('NOT NULL',)),
               Column('description', pd.String(), constraints=('NOT NULL',))),
      sql='UNIQUE (mod_id, name)',
      depends=('cms_modules',),
      grant=cms_rights)

table('cms_rights_assignment',
      doc="Underlying binding table between menu items, roles and module actions.",
      columns=(PrimaryColumn('rights_assignment_id', pd.Serial()),
               Column('menu_item_id', pd.Integer(), constraints=('NOT NULL',), references='cms_menu_structure'),
               Column('role_id', pd.Integer(), constraints=('NOT NULL',), references='cms_roles'),
               Column('action_id', pd.Integer(), constraints=('NOT NULL',), references='cms_actions')),
      sql='UNIQUE (menu_item_id, role_id, action_id)',
      depends=('cms_menu_structure', 'cms_roles', 'cms_actions'),
      grant=cms_rights)

viewng('cms_rights',
       doc="User editable access rights assignment joining all menu items, roles and actions.",
       relations=(Relation('cms_menu_structure', alias='s', key_column='menu_item_id',
                           exclude_columns='*'),
                  Relation('cms_roles', alias='r', key_column='role_id',
                           exclude_columns=('name', 'description'),
                           jointype=JoinType.CROSS),
                  Relation('cms_actions', alias='a', key_column='action_id',
                           exclude_columns=('name', 'description'),
                           jointype=JoinType.INNER,
                           condition='a.mod_id = s.mod_id OR a.mod_id IS NULL'),
                  Relation('cms_rights_assignment', alias='x', key_column='rights_assignment_id',
                           jointype=JoinType.LEFT_OUTER,
                           condition=('x.menu_item_id = s.menu_item_id AND x.role_id = r.role_id '
                                      'AND x.action_id = a.action_id'),
                           exclude_columns=('menu_item_id', 'role_id', 'action_id'))),
       include_columns=(ViewColumn(None, alias='right_id',
                                   sql="s.menu_item_id ||'.'|| r.role_id ||'.'|| a.action_id"),
                        ViewColumn('s.menu_item_id'),
                        ViewColumn('r.name', alias='role_name'),
                        ViewColumn('r.description', alias='role_description'),
                        ViewColumn('a.name', alias='action_name'),
                        ViewColumn('a.description', alias='action_description'),
                        ViewColumn(None, alias='permitted',
                                   sql="x.rights_assignment_id IS NOT NULL"),
                        ),
       insert=None,
       update="""(
       INSERT INTO cms_rights_assignment (menu_item_id, role_id, action_id)
         SELECT new.menu_item_id, new.role_id, new.action_id
         WHERE new.permitted;
       DELETE FROM cms_rights_assignment
         WHERE menu_item_id=new.menu_item_id AND role_id=new.role_id
         AND action_id=new.action_id AND NOT new.permitted;
       )""",
       delete=None,
       depends=('cms_menu_structure', 'cms_roles', 'cms_actions', 'cms_rights_assignment'),
       grant=cms_rights)

table('cms_session',
      doc="Web user session information for authentication and login history.",
      columns=(PrimaryColumn('session_id', pd.Serial()), 
               Column('uid', pd.Integer(), constraints=('NOT NULL',),
                      references=cms_users_table),
               Column('key', pd.String(), constraints=('NOT NULL',)),
               Column('expire', pd.DateTime(), constraints=('NOT NULL',),
                      default='localtimestamp')),
      sql="UNIQUE (uid, key)",
      depends=(cms_users_table,),
      grant=cms_rights_rw)

table('cms_session_log_data',
      doc= "Log of web user logins (underlying data).",
      columns=(PrimaryColumn('id', pd.Serial()),
               Column('uid', pd.Integer(), constraints=('NOT NULL',),
                      references=cms_users_table+' ON DELETE CASCADE'),
               Column('start_time', pd.DateTime(), constraints=('NOT NULL',),
                      default='localtimestamp'),
               Column('ip', pd.String(), constraints=('NOT NULL',)),
               Column('success', pd.Boolean(), constraints=('NOT NULL',), default="False"),
               Column('user_agent', pd.String()),
               Column('referer', pd.String())),
      depends=(cms_users_table,),
      grant=cms_rights_rw)

viewng('cms_session_log',
       doc= "Log of web user logins (user visible information).",
       relations=(Relation('cms_session_log_data', alias='l', key_column='id'),
                  Relation('cms_users', alias='u', key_column='uid', exclude_columns='*',
                           jointype=JoinType.INNER, condition='l.uid = u.uid')),
       include_columns=(ViewColumn('u.login'),
                        ViewColumn('u.fullname')),
       insert_order=('cms_session_log_data',),
       update_order=('cms_session_log_data',),
       delete_order=('cms_session_log_data',),
       depends=('cms_session_log_data', cms_users_table),
       grant=cms_rights)
