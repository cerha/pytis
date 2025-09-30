# -*- coding: utf-8

from __future__ import unicode_literals
from __future__ import print_function

import sqlalchemy
import pytis.data.gensqlalchemy as sql
import pytis.data
from pytis.dbdefs.db_pytis_base import default_access_rights, pytis_schemas
from pytis.dbdefs.db_pytis_common import XChanges
from pytis.data.dbdefs import and_, stype

class EPytisConfig(sql.SQLTable):
    """Pytis application configuration storage."""
    name = 'e_pytis_config'
    schemas = pytis_schemas.value(globals())
    fields = (sql.PrimaryColumn('id', pytis.data.Serial()),
              sql.Column('username', pytis.data.Name(not_null=True)),
              sql.Column('options', pytis.data.JSON(not_null=True)),
              )
    inherits = (XChanges,)
    unique = (('username',),)
    depends_on = ()
    access_rights = default_access_rights.value(globals())


class EPytisFormSettings(sql.SQLTable):
    """Storage of pytis profile independent form settings."""
    name = 'e_pytis_form_settings'
    schemas = pytis_schemas.value(globals())
    fields = (sql.PrimaryColumn('id', pytis.data.Serial()),
              sql.Column('username', pytis.data.Name(not_null=True)),
              sql.Column('spec_name', pytis.data.String(not_null=True)),
              sql.Column('form_name', pytis.data.String(not_null=True)),
              sql.Column('settings', pytis.data.JSON(not_null=True)),
              )
    inherits = (XChanges,)
    unique = (('username', 'spec_name', 'form_name',),)
    depends_on = ()
    access_rights = default_access_rights.value(globals())


class EPytisFormProfileBase(sql.SQLTable):
    """Pytis form configuration storage."""
    name = 'e_pytis_form_profile_base'
    schemas = pytis_schemas.value(globals())
    fields = (sql.PrimaryColumn('id', pytis.data.Serial()),
              sql.Column('username', pytis.data.Name(not_null=True)),
              sql.Column('spec_name', pytis.data.String(not_null=True)),
              sql.Column('profile_id', pytis.data.String(not_null=True)),
              sql.Column('title', pytis.data.String(not_null=True)),
              sql.Column('filter', pytis.data.JSON()),
              sql.Column('errors', pytis.data.String()),
              )
    inherits = (XChanges,)
    unique = (('username', 'spec_name', 'profile_id',),)
    depends_on = ()
    access_rights = default_access_rights.value(globals())


class EPytisFormProfileParams(sql.SQLTable):
    """Pytis form profile form type specific parameters."""
    name = 'e_pytis_form_profile_params'
    schemas = pytis_schemas.value(globals())
    fields = (sql.PrimaryColumn('id', pytis.data.Serial()),
              sql.Column('username', pytis.data.Name(not_null=True)),
              sql.Column('spec_name', pytis.data.String(not_null=True)),
              sql.Column('form_name', pytis.data.String(not_null=True)),
              sql.Column('profile_id', pytis.data.String(not_null=True)),
              sql.Column('params', pytis.data.JSON(not_null=True)),
              sql.Column('errors', pytis.data.String()),
              )
    inherits = (XChanges,)
    unique = (('username', 'spec_name', 'form_name', 'profile_id',),)
    depends_on = ()
    access_rights = default_access_rights.value(globals())


class EvPytisFormProfiles(sql.SQLView):
    """Pytis profiles."""
    name = 'ev_pytis_form_profiles'
    schemas = pytis_schemas.value(globals())

    @classmethod
    def query(cls):
        profile = sql.t.EPytisFormProfileBase.alias('profile')
        params = sql.t.EPytisFormProfileParams.alias('params')
        return sqlalchemy.select(
            cls._exclude(profile, 'id', 'username', 'spec_name', 'profile_id', 'errors') +
            cls._exclude(params, 'id', 'errors') +
            [
             (stype(profile.c.id) + "'.'" + stype(params.c.id)).label('id'),
             ("'form/'" + params.c.form_name + "'/'" + profile.c.spec_name +
              "'//'").label('fullname'),
             sqlalchemy.case(
                 [(sqlalchemy.and_(profile.c.errors.is_not(None),
                                  params.c.errors.is_not(None),
                                  ), profile.c.errors + "'\\n'" + params.c.errors)
                  ],
                 else_=(sqlalchemy.func.coalesce(profile.c.errors, params.c.errors))
             ).label('errors')
            ],
        ).select_from(
            profile.join(
                params, and_(
                    profile.c.username == params.c.username,
                    profile.c.spec_name == params.c.spec_name,
                    profile.c.profile_id == params.c.profile_id
                )
            )
        )

    def on_delete(self):
        return ("(delete from e_pytis_form_profile_base where id = split_part(old.id, '.', 1)::int;"
                "delete from e_pytis_form_profile_params where id = split_part(old.id, '.', 2)::int"
                ";)",)
    depends_on = (EPytisFormProfileBase, EPytisFormProfileParams,)
    access_rights = default_access_rights.value(globals())


class CopyUserProfile(sql.SQLFunction):
    """Zkopíruje profil z ev_pytis_form_profiles jinému uživateli."""
    schemas = pytis_schemas.value(globals())
    name = 'copy_user_profile'
    arguments = (sql.Column('profile_id', pytis.data.String()),
                 sql.Column('username', pytis.data.String()),)
    result_type = pytis.data.String()
    multirow = False
    stability = 'VOLATILE'
    depends_on = (EvPytisFormProfiles,)
    access_rights = ()


class EPytisAggregatedViews(sql.SQLTable):
    """Pytis aggregated views storage."""
    name = 'e_pytis_aggregated_views'
    schemas = pytis_schemas.value(globals())
    fields = (sql.PrimaryColumn('id', pytis.data.Serial()),
              sql.Column('username', pytis.data.Name(not_null=True)),
              sql.Column('spec_name', pytis.data.String(not_null=True)),
              sql.Column('aggregated_view_id', pytis.data.String(not_null=True)),
              sql.Column('params', pytis.data.JSON(not_null=True)),
              )
    inherits = (XChanges,)
    unique = (('username', 'spec_name', 'aggregated_view_id',),)
    depends_on = ()
    access_rights = default_access_rights.value(globals())
