# -*- coding: utf-8

from __future__ import unicode_literals

import sqlalchemy
import pytis.data.gensqlalchemy as sql
import pytis.data
from pytis.dbdefs.db_pytis_base import default_access_rights, pytis_schemas
from pytis.dbdefs.db_pytis_common import XChanges
from pytis.data.dbdefs import and_

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
              sql.Column('pickle', pytis.data.String(not_null=True)),
              sql.Column('dump', pytis.data.String(not_null=False)),
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
              sql.Column('pickle', pytis.data.String(not_null=True)),
              sql.Column('dump', pytis.data.String(not_null=False)),
              sql.Column('errors', pytis.data.String(not_null=False)),
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
              sql.Column('pickle', pytis.data.String(not_null=True)),
              sql.Column('dump', pytis.data.String(not_null=False)),
              sql.Column('errors', pytis.data.String(not_null=False)),
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
            cls._exclude(profile, 'id', 'username', 'spec_name', 'profile_id', 'pickle', 'dump',
                         'errors') +
            cls._exclude(params, 'id', 'pickle', 'dump', 'errors') +
            [sql.gL("profile.id||'.'||params.id").label('id'),
             sql.gL("'form/'|| params.form_name ||'/'|| profile.spec_name ||'//'")
             .label('fullname'),
             sql.gL("case when profile.errors is not null and params.errors is not null "
                    "then profile.errors ||'\n'||params.errors "
                    "else coalesce(profile.errors, params.errors) end").label('errors'),
             sql.gL("case when profile.dump is not null and params.dump is not null "
                    "then profile.dump ||'\n'||params.dump "
                    "else coalesce(profile.dump, params.dump) end").label('dump'),
             profile.c.pickle.label('pickled_filter'),
             params.c.pickle.label('pickled_params')],
            from_obj=[
                profile.join(
                    params, and_(
                        profile.c.username == params.c.username,
                        profile.c.spec_name == params.c.spec_name,
                        profile.c.profile_id == params.c.profile_id
                    )
                )
            ]
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
              sql.Column('title', pytis.data.String(not_null=True)),
              sql.Column('pickle', pytis.data.String(not_null=True)),
              )
    inherits = (XChanges,)
    unique = (('username', 'spec_name', 'aggregated_view_id',),)
    depends_on = ()
    access_rights = default_access_rights.value(globals())
