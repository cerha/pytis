# -*- coding: utf-8

import sqlalchemy
import pytis.extensions.gensqlalchemy as sql
import pytis.data
import dbdefs as db

class EPytisOutputTemplates(db.Base_LogSQLTable):
    """Storage of print output templates handled by a DatabaseResolver."""
    name = 'e_pytis_output_templates'
    fields = (
              sql.PrimaryColumn('id', pytis.data.Serial(not_null=False)),
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
    inherits = (db.XChanges,)
    with_oids = True
    depends_on = ()
    access_rights = db.default_access_rights.value(globals())

class EvPytisGlobalOutputTemplates(sql.SQLView):
    name = 'ev_pytis_global_output_templates'
    @classmethod
    def query(cls):
        templates = sql.t.EPytisOutputTemplates.alias('templates')
        return sqlalchemy.select(
            cls._exclude(templates),
            from_obj=[templates],
            whereclause='username is null'
            )

    insert_order = (EPytisOutputTemplates,)
    update_order = (EPytisOutputTemplates,)
    delete_order = (EPytisOutputTemplates,)
    depends_on = (EPytisOutputTemplates,)
    access_rights = db.default_access_rights.value(globals())

class EvPytisUserOutputTemplates(sql.SQLView):
    name = 'ev_pytis_user_output_templates'
    @classmethod
    def query(cls):
        templates = sql.t.EPytisOutputTemplates.alias('templates')
        return sqlalchemy.select(
            cls._exclude(templates),
            from_obj=[templates],
            whereclause='username=current_user or (username is null and (module, specification) not in (select module, specification from e_pytis_output_templates where templates.module=module and templates.specification=specification and username=current_user))'
            )

    def on_insert(self):
        return ("insert into e_pytis_output_templates (module, specification, template, rowtemplate, header, first_page_header, footer, style, username) values (new.module, new.specification, new.template, new.rowtemplate, new.header, new.first_page_header, new.footer, new.style, current_user)",)
    def on_update(self):
        return ("""(
       insert into e_pytis_output_templates (module, specification, template, rowtemplate, header, first_page_header, footer, style, username) values (new.module, new.specification, new.template, new.rowtemplate, new.header, new.first_page_header, new.footer, new.style, current_user);
       delete from e_pytis_output_templates where id=old.id and username=current_user;
       )
       """,)
    def on_delete(self):
        return ("delete from e_pytis_output_templates where id=old.id and username=current_user",)
    depends_on = (EPytisOutputTemplates,)
    access_rights = db.default_access_rights.value(globals())

