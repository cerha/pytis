#-*- coding: utf-8 -*-
"""Gensql specification of database objects used by Pytis Help.

"""

import pytis.data as pd
Relation = SelectRelation

db_rights = globals().get('Gall_pytis', None)

_std_table('e_pytis_help_pages',
           doc="Structure of static help pages.",
           columns=(PrimaryColumn('page_id', pd.Serial()),
                    Column('parent', pd.Integer(), references='e_pytis_help_pages'),
                    Column('ord', pd.Integer(), constraints=('not null',)),
                    Column('position', pd.LTree(), constraints=('not null', 'unique')),
                    Column('title', pd.String(), constraints=('not null',)),
                    Column('description', pd.String()),
                    Column('content', pd.String()),
                    ),
           grant=db_rights)

sql_raw("create unique index e_pytis_help_pages_unique_position "
        "on e_pytis_help_pages (ord, coalesce(parent, 0));",
        name='e_pytis_help_pages_unique_position',
        depends=('e_pytis_help_pages',))

function('f_pytis_help_page_position',
         doc="Generate a sortable string representing the hierarchical position of given help item.",
         arguments=(pd.Integer(),), output_type=pd.LTree(),
         body="""
           select case when parent is null then text2ltree('''')
                       else f_pytis_help_page_position(parent)
                  end || to_char(coalesce(ord, 999999), ''FM000000'')::text as result
           from e_pytis_help_pages where page_id=$1""",
         depends=('e_pytis_help_pages', ))

_std_table('e_pytis_help_spec',
           doc="Help texts for specifications.",
           columns=(PrimaryColumn('spec_name', pd.String()),
                    Column('description', pd.String()),
                    Column('help', pd.String()),
                    Column('changed', pd.Boolean(), default='false', constraints=('not null',),
                           doc="True when the content was edited by hand."),
                    Column('removed', pd.Boolean(), default='false', constraints=('not null',),
                           doc="False when the specification still exists."),
                    ),
           grant=db_rights)

_std_table('e_pytis_help_spec_items',
           doc="Help texts for specification items, such as fields, bindings, actions.",
           columns=(PrimaryColumn('item_id', pd.Serial()),
                    Column('spec_name', pd.String(), constraints=('not null',),
                           references='e_pytis_help_spec on update cascade on delete cascade'),
                    Column('kind', pd.String(), constraints=('not null', "check (kind in ('field', 'profile', 'binding', 'action', 'proc'))")),
                    Column('identifier', pd.String(), constraints=('not null',)),
                    Column('content', pd.String()),
                    Column('changed', pd.Boolean(), default='false', constraints=('not null',),
                           doc="True when the content was edited by hand."),
                    Column('removed', pd.Boolean(), default='false', constraints=('not null',),
                           doc="False when the item still exists in specification."),
                    ),
           sql="unique (spec_name, kind, identifier)",
           grant=db_rights)

_std_table('e_pytis_help_menu',
           doc="Texts for help pages.",
           columns=(PrimaryColumn('fullname', TString,
                                  references='c_pytis_menu_actions on update cascade on delete cascade'),
                    Column('content', pd.String()),
                    Column('changed', pd.Boolean(), default='false', constraints=('not null',),
                           doc="True when the content was edited by hand."),
                    Column('removed', pd.Boolean(), default='false', constraints=('not null',),
                           doc="False when the item still exists in menu."),
                    ),
           depends=('e_pytis_menu', ),
           grant=db_rights)

viewng('ev_pytis_help',
       doc="Complete help structure including texts and DMP menu structure.",
       relations=(
        SelectSet(Select((SelectRelation('e_pytis_menu', alias='m', exclude_columns=('*',),
                                         key_column='id', condition="nlevel(position) >= 2"),
                          Relation('c_pytis_menu_actions', alias='a', jointype=JoinType.INNER,
                                   condition='a.fullname = m.fullname', exclude_columns=('*',)),
                          Relation('e_pytis_help_menu', alias='mh', jointype=JoinType.LEFT_OUTER,
                                   condition='mh.fullname = a.fullname', exclude_columns=('*',)),
                          Relation('e_pytis_help_spec', alias='sh', jointype=JoinType.LEFT_OUTER,
                                   condition=("sh.spec_name = a.spec_name"),
                                   exclude_columns=('*',))),
                         include_columns=(V(None, 'help_id', "'menu/'||m.menuid"),
                                          V('m.menuid'),
                                          V('m.fullname'),
                                          V('m.title'),
                                          V('m.help', 'description'),
                                          V('mh.content', 'menu_help'),
                                          V('a.spec_name'),
                                          V('sh.description', 'spec_description'),
                                          V('sh.help', 'spec_help'),
                                          V(None, 'page_id', 'null::int'),
                                          V(None, 'parent', 'null::int'),
                                          V(None, 'ord', 'null'),
                                          V(None, 'content', 'null'),
                                          V(None, 'position', "text2ltree('999999')||subpath(m.position, 1)", ),
                                          V(None, 'position_nsub',
                                            "(select count(*)-1 from e_pytis_menu where position <@ m.position)"),
                                          V(None, 'changed', "coalesce(mh.changed, false) or coalesce(sh.changed, false)", ),
                                          V(None, 'removed', "coalesce(mh.removed, false) or coalesce(sh.removed, false)", ),
                                          ))),
        SelectSet(Select((SelectRelation('generate_series(0, 0)', exclude_columns=('*',)),
                          ),
                         include_columns=(V(None, 'help_id', "'menu/'"),
                                          V(None, 'menuid', 'null'),
                                          V(None, 'fullname', 'null'),
                                          V(None, 'title', "'Aplikační menu '"),
                                          V(None, 'description', 'null'),
                                          V(None, 'menu_help', 'null'),
                                          V(None, 'spec_name', 'null'),
                                          V(None, 'spec_description', 'null'),
                                          V(None, 'spec_help', 'null'),
                                          V(None, 'page_id', 'null::int'),
                                          V(None, 'parent', 'null::int'),
                                          V(None, 'ord', 'null::int'),
                                          V(None, 'content', 'null'),
                                          V(None, 'position', "text2ltree('999999')", ),
                                          V(None, 'position_nsub',
                                            "(select count(*) from e_pytis_menu)"),
                                          V(None, 'changed', "false", ),
                                          V(None, 'removed', "false", ),
                                          )),
                  settype=UNION),
        SelectSet(Select((SelectRelation('e_pytis_help_pages', alias='p',),),
                         include_columns=(V(None, 'help_id', "'page/'||page_id"),
                                          V(None, 'menuid', 'null'),
                                          V(None, 'fullname', 'null'),
                                          V(None, 'menu_help', 'null'),
                                          V(None, 'spec_name', 'null'),
                                          V(None, 'spec_description', 'null'),
                                          V(None, 'spec_help', 'null'),
                                          V(None, 'position_nsub',
                                            "(select count(*)-1 from e_pytis_help_pages where position <@ p.position)"),
                                          V(None, 'changed', "false", ),
                                          V(None, 'removed', "false", ),
                                          )),
                  settype=UNION),
        #Relation('e_pytis_help_pages', alias='h', key_column='id'),
        ),
       insert="""(
       insert into e_pytis_help_pages (page_id, title, description, content, parent, position, ord)
       values (new.page_id, new.title, new.description, new.content, new.parent, '',
               coalesce(new.ord, (select max(ord)+100 from e_pytis_help_pages 
                                  where coalesce(parent, 0)=coalesce(new.parent, 0)), 100));
       update e_pytis_help_pages SET position = f_pytis_help_page_position(page_id);
       )""",
       update="""(
       update e_pytis_help_pages set
         title = new.title,
         description = new.description,
         content = new.content,
         parent = new.parent,
         ord = new.ord
         where e_pytis_help_pages.page_id = old.page_id;
       update e_pytis_help_pages set position = f_pytis_help_page_position(page_id);
       insert into e_pytis_help_menu (fullname, content, changed)
         select old.fullname, new.menu_help, true
         where new.menu_help is not null and old.fullname not in (select fullname from e_pytis_help_menu);
       update e_pytis_help_menu set
         content = new.menu_help,
         changed = coalesce(new.menu_help, '') != coalesce(old.menu_help, '')
         where fullname = old.fullname and new.menu_help is not null;
       delete from e_pytis_help_menu
         where fullname = old.fullname and new.menu_help is null;
       insert into e_pytis_help_spec (spec_name, description, help, changed)
         select old.spec_name, new.spec_description, new.spec_help, true
         where old.spec_name not in (select spec_name from e_pytis_help_spec);
       update e_pytis_help_spec set
         description = new.spec_description,
         help = new.spec_help,
         changed = coalesce(new.spec_help, '') != coalesce(old.spec_help, '')
                   or coalesce(new.spec_description, '') != coalesce(old.spec_description, '')
         where spec_name = old.spec_name;
       )""",
       delete="delete from e_pytis_help_pages where page_id = old.page_id;",
       depends=('e_pytis_help_pages', 'e_pytis_help_menu', ),
       grant=db_rights)


viewng('ev_pytis_user_help',
       doc="Help menu structure limited to the current user according to DMP rights.",
       relations=(
        SelectSet(Select((SelectRelation('ev_pytis_help', alias='h', key_column='help_id', ),
                          Relation('pytis_view_user_menu()', alias='u', jointype=JoinType.INNER,
                                   condition='h.menuid = u.menuid',
                                   key_column='menuid', exclude_columns=('*',))))),
        SelectSet(Select((SelectRelation('ev_pytis_help', alias='h',
                                         condition='h.menuid is null',),)),
                  settype=UNION),
        ),
       depends=('ev_pytis_help', 'pytis_view_user_menu',),
       insert=None,
       update=None,
       delete=None,
       grant=db_rights)


_std_table('e_pytis_help_spec_attachments',
           doc="Attachments for help texts (images etc.)",
           columns=(PrimaryColumn('file_id', pd.Serial()),
                    Column('spec_name', pd.String(),
                           references='e_pytis_help_spec(spec_name) on update cascade on delete cascade',
                           constraints=('not null',)),
                    Column('file_name', pd.String(), constraints=('not null',)),
                    Column('byte_size', pd.Integer(), constraints=('not null',)),
                    Column('width', pd.Integer()),
                    Column('height', pd.Integer()),
                    Column('resized_width', pd.Integer()),
                    Column('resized_height', pd.Integer()),
                    Column('thumbnail_width', pd.Integer()),
                    Column('thumbnail_height', pd.Integer()),
                    Column('file', pd.Binary(maxlen=2*1024*1024), constraints=('not null',)),
                    Column('resized', pd.Binary(maxlen=1024*1024)),
                    Column('thumbnail', pd.Binary(maxlen=512*1024)),
                    ),
           sql="unique (spec_name, file_name)",
           depends=('e_pytis_help_spec', ),
           grant=db_rights)

_std_table('e_pytis_help_pages_attachments',
           doc="Attachments for help texts (images etc.)",
           columns=(PrimaryColumn('file_id', pd.Serial()),
                    Column('page_id', pd.Integer(),
                           references='e_pytis_help_pages(page_id) on delete cascade',
                           constraints=('not null',)),
                    Column('file_name', pd.String(), constraints=('not null',)),
                    Column('byte_size', pd.Integer(), constraints=('not null',)),
                    Column('width', pd.Integer()),
                    Column('height', pd.Integer()),
                    Column('resized_width', pd.Integer()),
                    Column('resized_height', pd.Integer()),
                    Column('thumbnail_width', pd.Integer()),
                    Column('thumbnail_height', pd.Integer()),
                    Column('file', pd.Binary(maxlen=2*1024*1024), constraints=('not null',)),
                    Column('resized', pd.Binary(maxlen=1024*1024)),
                    Column('thumbnail', pd.Binary(maxlen=512*1024)),
                    ),
           sql="unique (page_id, file_name)",
           depends=('e_pytis_help_pages', ),
           grant=db_rights)

