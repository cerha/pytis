# -*- coding: utf-8

from __future__ import unicode_literals
from __future__ import print_function

import sqlalchemy
import pytis.data.gensqlalchemy as sql
import pytis.data
from pytis.data.dbdefs import and_, or_, not_
from pytis.dbdefs.db_pytis_base import Base_LogSQLTable, default_access_rights, pytis_schemas
from pytis.dbdefs.db_pytis_common import XChanges


class EPytisOutputTemplates(Base_LogSQLTable):
    """Storage of print output templates handled by a DatabaseResolver."""
    name = 'e_pytis_output_templates'
    schemas = pytis_schemas.value(globals())

    fields = (sql.PrimaryColumn('id', pytis.data.Serial()),
              sql.Column('module', pytis.data.String(not_null=True)),
              sql.Column('specification', pytis.data.String(not_null=True)),
              sql.Column('template', pytis.data.String(not_null=False)),
              sql.Column('rowtemplate', pytis.data.String(not_null=False)),
              sql.Column('header', pytis.data.String(not_null=False)),
              sql.Column('first_page_header', pytis.data.String(not_null=False)),
              sql.Column('footer', pytis.data.String(not_null=False)),
              sql.Column('style', pytis.data.String(not_null=False)),
              sql.Column('username', pytis.data.String(not_null=False)),
              )
    inherits = (XChanges,)
    depends_on = ()
    access_rights = default_access_rights.value(globals())


class EvPytisGlobalOutputTemplates(sql.SQLView):
    name = 'ev_pytis_global_output_templates'
    schemas = pytis_schemas.value(globals())

    @classmethod
    def query(cls):
        templates = sql.t.EPytisOutputTemplates.alias('templates')
        return sqlalchemy.select(
            cls._exclude(templates),
            from_obj=[templates],
            whereclause=templates.c.username.is_(None),
        )

    insert_order = (EPytisOutputTemplates,)
    update_order = (EPytisOutputTemplates,)
    delete_order = (EPytisOutputTemplates,)
    depends_on = (EPytisOutputTemplates,)
    access_rights = default_access_rights.value(globals())


class EvPytisUserOutputTemplates(sql.SQLView):
    name = 'ev_pytis_user_output_templates'
    schemas = pytis_schemas.value(globals())

    @classmethod
    def query(cls):
        templates = sql.t.EPytisOutputTemplates.alias('templates')
        templates2 = sql.t.EPytisOutputTemplates.alias('templates2')
        return sqlalchemy.select(
            cls._exclude(templates),
            from_obj=[templates],
            whereclause=or_(
                templates.c.username == sqlalchemy.text('current_user'),
                and_(
                    templates.c.username.is_(None),
                    not_(sqlalchemy.tuple_(templates.c.module, templates.c.specification).in_(
                        sqlalchemy.select(
                            [templates2.c.module, templates2.c.specification],
                            from_obj=[templates2],
                            whereclause=and_(
                                templates2.c.module == templates.c.module,
                                templates2.c.specification == templates.c.specification,
                                templates2.c.username == sqlalchemy.text('current_user'),
                            ),
                        ),
                    )),
                ),
            ),
        )

    def on_insert(self):
        return ("insert into e_pytis_output_templates (module, specification, template, "
                "rowtemplate, header, first_page_header, footer, style, username) "
                "values (new.module, new.specification, new.template, new.rowtemplate, new.header, "
                "new.first_page_header, new.footer, new.style, current_user)",)

    def on_update(self):
        return ("""(
       insert into e_pytis_output_templates (module, specification, template, rowtemplate, header,
                                             first_page_header, footer, style, username)
              values (new.module, new.specification, new.template, new.rowtemplate, new.header,
                      new.first_page_header, new.footer, new.style, current_user);
       delete from e_pytis_output_templates where id=old.id and username=current_user;
       )
       """,)

    def on_delete(self):
        return ("delete from e_pytis_output_templates where id=old.id and username=current_user",)
    depends_on = (EPytisOutputTemplates,)
    access_rights = default_access_rights.value(globals())
