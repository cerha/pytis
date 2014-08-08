# -*- coding: utf-8

from __future__ import unicode_literals

import sqlalchemy
import pytis.data.gensqlalchemy as sql
import pytis.data
from pytis.dbdefs.db_pytis_base import Base_LogSQLTable, default_access_rights
from pytis.dbdefs.db_pytis_common import XChanges
from pytis.dbdefs.db_pytis_menu import EPytisMenu, PytisViewUserMenu

class EPytisHelpPages(Base_LogSQLTable):
    """Structure of static help pages."""
    name = 'e_pytis_help_pages'
    fields = (sql.PrimaryColumn('page_id', pytis.data.Serial()),
              sql.Column('parent', pytis.data.Integer(not_null=False),
                         references=sql.r.EPytisHelpPages.page_id),
              sql.Column('ord', pytis.data.Integer(not_null=True)),
              sql.Column('position', pytis.data.LTree(not_null=True), unique=True),
              sql.Column('title', pytis.data.String(not_null=True)),
              sql.Column('description', pytis.data.String(not_null=False)),
              sql.Column('content', pytis.data.String(not_null=False)),
              )
    inherits = (XChanges,)
    with_oids = True
    depends_on = ()
    access_rights = default_access_rights.value(globals())

class EPytisHelpPagesUniquePosition(sql.SQLRaw):
    name = 'e_pytis_help_pages_unique_position'
    @classmethod
    def sql(class_):
        return ("create unique index e_pytis_help_pages_unique_position on "
                "e_pytis_help_pages (ord, coalesce(parent, 0));")
    depends_on = (EPytisHelpPages,)

class FPytisHelpPagePosition(sql.SQLFunction):
    """Generate a sortable string representing the hierarchical position of given help item."""
    name = 'f_pytis_help_page_position'
    arguments = (sql.Column('', pytis.data.Integer()),)
    result_type = pytis.data.LTree()
    multirow = False
    stability = 'VOLATILE'
    depends_on = (EPytisHelpPages,)
    access_rights = ()

class EPytisHelpSpec(Base_LogSQLTable):
    """Help texts for specifications."""
    name = 'e_pytis_help_spec'
    fields = (sql.PrimaryColumn('spec_name', pytis.data.String(not_null=False)),
              sql.Column('description', pytis.data.String(not_null=False)),
              sql.Column('help', pytis.data.String(not_null=False)),
              sql.Column('changed', pytis.data.Boolean(not_null=True),
                         doc="True when the content was edited by hand.", default=False),
              sql.Column('removed', pytis.data.Boolean(not_null=True),
                         doc="False when the specification still exists.", default=False),
              )
    inherits = (XChanges,)
    with_oids = True
    depends_on = ()
    access_rights = default_access_rights.value(globals())

class EPytisHelpSpecItems(Base_LogSQLTable):
    """Help texts for specification items, such as fields, bindings, actions."""
    name = 'e_pytis_help_spec_items'
    fields = (sql.PrimaryColumn('item_id', pytis.data.Serial()),
              sql.Column('spec_name', pytis.data.String(not_null=True),
                         references=sql.a(sql.r.EPytisHelpSpec.spec_name,
                                          onupdate='CASCADE', ondelete='CASCADE')),
              sql.Column('kind', pytis.data.String(not_null=True),
                         check="kind in ('field', 'profile', 'binding', 'action', 'proc')"),
              sql.Column('identifier', pytis.data.String(not_null=True)),
              sql.Column('content', pytis.data.String(not_null=False)),
              sql.Column('changed', pytis.data.Boolean(not_null=True),
                         doc="True when the content was edited by hand.", default=False),
              sql.Column('removed', pytis.data.Boolean(not_null=True),
                         doc="False when the item still exists in specification.", default=False),
              )
    inherits = (XChanges,)
    with_oids = True
    unique = (('spec_name', 'kind', 'identifier',),)
    depends_on = ()
    access_rights = default_access_rights.value(globals())

class EPytisHelpMenu(Base_LogSQLTable):
    """Texts for help pages."""
    name = 'e_pytis_help_menu'
    fields = (sql.PrimaryColumn('fullname', pytis.data.String(not_null=False),
                                references=sql.a(sql.r.CPytisMenuActions.fullname,
                                                 onupdate='CASCADE', ondelete='CASCADE')),
              sql.Column('content', pytis.data.String(not_null=False)),
              sql.Column('changed', pytis.data.Boolean(not_null=True),
                         doc="True when the content was edited by hand.", default=False),
              sql.Column('removed', pytis.data.Boolean(not_null=True),
                         doc="False when the item still exists in menu.", default=False),
              )
    inherits = (XChanges,)
    with_oids = True
    depends_on = (EPytisMenu,)
    access_rights = default_access_rights.value(globals())

class EvPytisHelp(sql.SQLView):
    """Complete help structure including texts and DMP menu structure."""
    name = 'ev_pytis_help'
    @classmethod
    def query(cls):
        def select_1():
            m = sql.t.EPytisMenu.alias('m')
            a_ = sql.t.CPytisMenuActions.alias('a')
            mh = sql.t.EPytisHelpMenu.alias('mh')
            sh = sql.t.EPytisHelpSpec.alias('sh')
            return sqlalchemy.select(
                sql.reorder_columns(
                    [sql.gL("'menu/'||m.menuid").label('help_id'),
                     m.c.menuid.label('menuid'),
                     m.c.fullname.label('fullname'),
                     m.c.title.label('title'),
                     m.c.help.label('description'),
                     mh.c.content.label('menu_help'),
                     a_.c.spec_name.label('spec_name'),
                     sh.c.description.label('spec_description'),
                     sh.c.help.label('spec_help'),
                     sql.gL("null::int").label('page_id'),
                     sql.gL("null::int").label('parent'),
                     sql.gL("null").label('ord'),
                     sql.gL("null").label('content'),
                     sql.gL("text2ltree('999999')||subpath(m.position, 1)").label('position'),
                     sql.gL("(select count(*)-1 from e_pytis_menu where position <@ m.position)")
                     .label('position_nsub'),
                     sql.gL("coalesce(mh.changed, false) or coalesce(sh.changed, false)")
                     .label('changed'),
                     sql.gL("coalesce(mh.removed, false) or coalesce(sh.removed, false)")
                     .label('removed')],
                    ['help_id', 'menuid', 'fullname', 'title', 'description', 'menu_help',
                     'spec_name', 'spec_description', 'spec_help', 'page_id', 'parent', 'ord',
                     'content', 'position', 'position_nsub', 'changed', 'removed']),
                from_obj=[m.join(a_, a_.c.fullname == m.c.fullname).
                          outerjoin(mh, mh.c.fullname == a_.c.fullname).
                          outerjoin(sh, sh.c.spec_name == a_.c.spec_name)],
                whereclause='nlevel(position) >= 2'
            )
        def select_2():
            series = sqlalchemy.select(["*"], from_obj=["generate_series(0, 0)"]).alias('series')
            return sqlalchemy.select(
                sql.reorder_columns(
                    [sql.gL("'menu/'").label('help_id'),
                     sql.gL("null").label('menuid'),
                     sql.gL("null").label('fullname'),
                     sql.gL("'Aplikační menu '").label('title'),
                     sql.gL("null").label('description'),
                     sql.gL("null").label('menu_help'),
                     sql.gL("null").label('spec_name'),
                     sql.gL("null").label('spec_description'),
                     sql.gL("null").label('spec_help'),
                     sql.gL("null::int").label('page_id'),
                     sql.gL("null::int").label('parent'),
                     sql.gL("null::int").label('ord'),
                     sql.gL("null").label('content'),
                     sql.gL("text2ltree('999999')").label('position'),
                     sql.gL("(select count(*) from e_pytis_menu)").label('position_nsub'),
                     sql.gL("false").label('changed'),
                     sql.gL("false").label('removed')],
                    ['help_id', 'menuid', 'fullname', 'title', 'description', 'menu_help',
                     'spec_name', 'spec_description', 'spec_help', 'page_id', 'parent', 'ord',
                     'content', 'position', 'position_nsub', 'changed', 'removed']),
                from_obj=[series]
            )
        set_1 = sqlalchemy.union(select_1(), select_2())
        def select_3():
            p = sql.t.EPytisHelpPages.alias('p')
            return sqlalchemy.select(
                sql.reorder_columns(
                    cls._exclude(p) +
                    [sql.gL("'page/'||page_id").label('help_id'),
                     sql.gL("null").label('menuid'),
                     sql.gL("null").label('fullname'),
                     sql.gL("null").label('menu_help'),
                     sql.gL("null").label('spec_name'),
                     sql.gL("null").label('spec_description'),
                     sql.gL("null").label('spec_help'),
                     sql.gL("(select count(*)-1 from e_pytis_help_pages "
                           "where position <@ p.position)").label('position_nsub'),
                     sql.gL("false").label('changed'),
                     sql.gL("false").label('removed')],
                    ['help_id', 'menuid', 'fullname', 'title', 'description', 'menu_help',
                     'spec_name', 'spec_description', 'spec_help', 'page_id', 'parent', 'ord',
                     'content', 'position', 'position_nsub', 'changed', 'removed']),
                from_obj=[p]
            )
        return sqlalchemy.union(set_1, select_3())
    def on_insert(self):
        return ("""(
       insert into e_pytis_help_pages (page_id, title, description, content, parent, position, ord)
       values (new.page_id, new.title, new.description, new.content, new.parent, '',
               coalesce(new.ord, (select max(ord)+100 from e_pytis_help_pages
                                  where coalesce(parent, 0)=coalesce(new.parent, 0)), 100));
       update e_pytis_help_pages SET position = f_pytis_help_page_position(page_id);
       )""",)
    def on_update(self):
        return ("""(
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
         where new.menu_help is not null and
               old.fullname not in (select fullname from e_pytis_help_menu);
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
       )""",)
    def on_delete(self):
        return ("delete from e_pytis_help_pages where page_id = old.page_id;",)
    depends_on = (EPytisHelpPages, EPytisHelpMenu,)
    access_rights = default_access_rights.value(globals())

class EvPytisUserHelp(sql.SQLView):
    """Help menu structure limited to the current user according to DMP rights."""
    name = 'ev_pytis_user_help'
    @classmethod
    def query(cls):
        def select_1():
            h = sql.t.EvPytisHelp.alias('h')
            u = sqlalchemy.select(["*"], from_obj=["pytis_view_user_menu()"]).alias('u')
            return sqlalchemy.select(
                sql.reorder_columns(cls._exclude(h),
                                    ['help_id', 'menuid', 'fullname', 'title', 'description',
                                     'menu_help', 'spec_name', 'spec_description', 'spec_help',
                                     'page_id', 'parent', 'ord', 'content', 'position',
                                     'position_nsub', 'changed', 'removed']),
                from_obj=[h.join(u, sql.gR('h.menuid = u.menuid'))]
            )
        def select_2():
            h = sql.t.EvPytisHelp.alias('h')
            return sqlalchemy.select(
                sql.reorder_columns(cls._exclude(h),
                                    ['help_id', 'menuid', 'fullname', 'title', 'description',
                                     'menu_help', 'spec_name', 'spec_description', 'spec_help',
                                     'page_id', 'parent', 'ord', 'content', 'position',
                                     'position_nsub', 'changed', 'removed']),
                from_obj=[h],
                whereclause='h.menuid is null'
            )
        return sqlalchemy.union(select_1(), select_2())
    depends_on = (EvPytisHelp, PytisViewUserMenu,)
    access_rights = default_access_rights.value(globals())

class EPytisHelpSpecAttachments(Base_LogSQLTable):
    """Attachments for help texts (images etc.)"""
    name = 'e_pytis_help_spec_attachments'
    fields = (sql.PrimaryColumn('file_id', pytis.data.Serial()),
              sql.Column('spec_name', pytis.data.String(not_null=True),
                         references=sql.a(sql.r.EPytisHelpSpec.spec_name,
                                          onupdate='CASCADE', ondelete='CASCADE')),
              sql.Column('file_name', pytis.data.String(not_null=True)),
              sql.Column('byte_size', pytis.data.Integer(not_null=True)),
              sql.Column('width', pytis.data.Integer(not_null=False)),
              sql.Column('height', pytis.data.Integer(not_null=False)),
              sql.Column('resized_width', pytis.data.Integer(not_null=False)),
              sql.Column('resized_height', pytis.data.Integer(not_null=False)),
              sql.Column('thumbnail_width', pytis.data.Integer(not_null=False)),
              sql.Column('thumbnail_height', pytis.data.Integer(not_null=False)),
              sql.Column('file', pytis.data.Binary(not_null=True)),
              sql.Column('resized', pytis.data.Binary(not_null=False)),
              sql.Column('thumbnail', pytis.data.Binary(not_null=False)),
              )
    inherits = (XChanges,)
    with_oids = True
    unique = (('spec_name', 'file_name',),)
    depends_on = (EPytisHelpSpec,)
    access_rights = default_access_rights.value(globals())

class EPytisHelpPagesAttachments(Base_LogSQLTable):
    """Attachments for help texts (images etc.)"""
    name = 'e_pytis_help_pages_attachments'
    fields = (sql.PrimaryColumn('file_id', pytis.data.Serial()),
              sql.Column('page_id', pytis.data.Integer(not_null=True),
                         references=sql.a(sql.r.EPytisHelpPages.page_id, ondelete='CASCADE')),
              sql.Column('file_name', pytis.data.String(not_null=True)),
              sql.Column('byte_size', pytis.data.Integer(not_null=True)),
              sql.Column('width', pytis.data.Integer(not_null=False)),
              sql.Column('height', pytis.data.Integer(not_null=False)),
              sql.Column('resized_width', pytis.data.Integer(not_null=False)),
              sql.Column('resized_height', pytis.data.Integer(not_null=False)),
              sql.Column('thumbnail_width', pytis.data.Integer(not_null=False)),
              sql.Column('thumbnail_height', pytis.data.Integer(not_null=False)),
              sql.Column('file', pytis.data.Binary(not_null=True)),
              sql.Column('resized', pytis.data.Binary(not_null=False)),
              sql.Column('thumbnail', pytis.data.Binary(not_null=False)),
              )
    inherits = (XChanges,)
    with_oids = True
    unique = (('page_id', 'file_name',),)
    depends_on = (EPytisHelpPages,)
    access_rights = default_access_rights.value(globals())
