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
                    Column('ord', pd.Integer(), constraints=('NOT NULL',)),
                    Column('position', pd.LTree(), constraints=('NOT NULL', 'UNIQUE')),
                    Column('title', pd.String(), constraints=('NOT NULL',)),
                    Column('description', pd.String()),
                    Column('content', pd.String()),
                    ),
           grant=db_rights)

sql_raw("CREATE UNIQUE INDEX e_pytis_help_pages_unique_position "
        "ON e_pytis_help_pages (ord, coalesce(parent, 0));",
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

_std_table('e_pytis_menu_help',
           doc="Texts for help pages.",
           columns=(PrimaryColumn('menuid', pd.Integer(), references='e_pytis_menu(menuid)'),
                    Column('content', pd.String()),
                    ),
           depends=('e_pytis_menu', ),
           grant=db_rights)

viewng('ev_pytis_help',
       doc="Complete help structure including texts and DMP menu structure.",
       relations=(
        SelectSet(Select((SelectRelation('e_pytis_menu', alias='m', exclude_columns=('*',),
                                         key_column='id', condition="nlevel(position) >= 2"),
                          Relation('e_pytis_menu_help', alias='h', jointype=JoinType.LEFT_OUTER,
                                   condition='h.menuid = m.menuid', exclude_columns=('*',))),
                         include_columns=(V(None, 'help_id', "'menu.'||m.menuid"),
                                          V('m.menuid'),
                                          V(None, 'page_id', 'null::int'),
                                          V('m.title'),
                                          V('m.help', 'description'),
                                          V('m.fullname'),
                                          V('h.content'),
                                          V(None, 'parent', 'null::int'),
                                          V(None, 'ord', 'null'),
                                          V(None, 'position', "text2ltree('999999')||subpath(m.position, 1)", ),
                                          V(None, 'position_nsub',
                                            "(select count(*)-1 from e_pytis_menu where position <@ m.position)"),
                                          ))),
        SelectSet(Select((SelectRelation('generate_series(0, 0)', exclude_columns=('*',)),
                          ),
                         include_columns=(V(None, 'help_id', "'menu'"),
                                          V(None, 'page_id', 'null::int'),
                                          V(None, 'menuid', 'null'),
                                          V(None, 'fullname', 'null'),
                                          V(None, 'position', "text2ltree('999999')", ),
                                          V(None, 'position_nsub',
                                            "(select count(*) from e_pytis_menu)"),
                                          V(None, 'title', "'Aplikační menu '"),
                                          V(None, 'description', 'null'),
                                          V(None, 'content', 'null'),
                                          V(None, 'parent', 'null::int'),
                                          V(None, 'ord', 'null::int'),
                                          )),
                  settype=UNION),
        SelectSet(Select((SelectRelation('e_pytis_help_pages', alias='p',
                                         #exclude_columns=('position',),
                                         ),),
                         include_columns=(V(None, 'help_id', "'page.'||page_id"),
                                          V(None, 'menuid', 'null'),
                                          V(None, 'fullname', 'null'),
                                          #V(None, 'position', "text2ltree('01page')||position", ),
                                          V(None, 'position_nsub',
                                            "(select count(*)-1 from e_pytis_help_pages where position <@ p.position)"),
                                          )),
                  settype=UNION),
        #Relation('e_pytis_help_pages', alias='h', key_column='id'),
        ),
       insert="""(
       insert into e_pytis_help_pages (title, description, content, parent, position, ord)
       values (new.title, new.description, new.content, new.parent, '',
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
       update e_pytis_menu_help set
         content = new.content
       where menuid = old.menuid;
       insert into e_pytis_menu_help (menuid, content)
         select old.menuid, new.content
         where old.menuid not in (select menuid from e_pytis_menu_help);
       )""",
       delete="delete from e_pytis_help_pages where page_id = old.page_id;",
       depends=('e_pytis_help_pages', 'e_pytis_menu_help', ),
       grant=db_rights)

