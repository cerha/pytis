# -*- coding: utf-8

from __future__ import unicode_literals

import sqlalchemy
import pytis.data.gensqlalchemy as sql
from pytis.data.dbdefs import ival, sval
from pytis.util import translations
import pytis.data

import pytis.dbdefs.db_pytis_base
import pytis.dbdefs.db_pytis_common
import pytis.dbdefs.db_pytis_config
import pytis.dbdefs.db_pytis_menu
import pytis.dbdefs.db_pytis_statistics
import pytis.dbdefs.db_pytis_logging
import pytis.dbdefs.db_pytis_output
import pytis.dbdefs.db_pytis_crypto_basic
import pytis.dbdefs.db_pytis_crypto
import pytis.dbdefs.db_pytis_help
import pytis.dbdefs.db_pytis_http_attachment_storage
import pytis.dbdefs.db_pytis_cms

from pytis.dbdefs.db_pytis_base import Base_PyFunction

_ = translations('pytis-demo')

sql.SQLFlexibleValue.set_value('app_default_access_rights', (('all', 'demo',),))
sql.SQLFlexibleValue.set_value('app_pytis_schemas', (('public',),))
sql.SQLFlexibleValue.set_value('app_cms_rights', (('all', 'demo',),))
sql.SQLFlexibleValue.set_value('app_cms_rights_rw', (('all', 'demo',),))
sql.SQLFlexibleValue.set_value('app_cms_schemas', (('public',),))
sql.SQLFlexibleValue.set_value('app_cms_users_table', 'cms_users')
sql.SQLFlexibleValue.set_value('app_http_attachment_storage_rights', (('insert', 'demo'),
                                                                      ('delete', 'demo'),
                                                                      ('select', 'demowebuser'),))


class CmsUsers(sql.SQLTable):
    name = 'cms_users'
    fields = (
        sql.PrimaryColumn('uid', pytis.data.Serial()),
        sql.Column('login', pytis.data.String(not_null=True), unique=True),
        sql.Column('passwd', pytis.data.String(not_null=True)),
        sql.Column('fullname', pytis.data.String(not_null=True)),
    )
    with_oids = True
    depends_on = ()
    access_rights = (('all', 'pytis-demo'), ('select', 'www-data'))


class Continents(sql.SQLTable):
    name = 'continents'
    fields = (sql.PrimaryColumn('id', pytis.data.String(minlen=2, maxlen=2, not_null=False),
                                "Code"),
              sql.Column('name', pytis.data.String(not_null=True), "Continent name",
                         unique=True),
              sql.Column('smallest', pytis.data.String(), "Smallest country"),
              )
    with_oids = True
    depends_on = ()
    access_rights = (('all', 'pytis-demo'), ('select', 'www-data'))


class Countries(sql.SQLTable):
    name = 'countries'
    fields = (sql.PrimaryColumn('id', pytis.data.String(minlen=2, maxlen=2, not_null=False),
                                "Alpha-2 code", "Uppercase ISO 3166 alpha-2 country code."),
              sql.Column('id3', pytis.data.String(minlen=3, maxlen=3, not_null=True),
                         "Alpha-3 code", "Uppercase ISO 3166 alpha-3 country code.", unique=True),
              sql.Column('num', pytis.data.String(minlen=3, maxlen=3, not_null=True),
                         "Numeric code", "ISO 3166 country number.", unique=True),
              sql.Column('continent', pytis.data.String(minlen=2, maxlen=2, not_null=True),
                         "Continent", "Uppercase alpha-2 continent code.",
                         references=sql.r.Continents),
              sql.Column('name', pytis.data.String(not_null=True),
                         "Short name", unique=True),
              sql.Column('fullname', pytis.data.String(not_null=True), "Full name", unique=True),
              )
    with_oids = True
    depends_on = ()
    access_rights = (('all', 'pytis-demo'), ('select', 'www-data'))


class Longtable(sql.SQLTable):
    name = 'longtable'
    fields = (sql.PrimaryColumn('id', pytis.data.Serial(), _("ID")),
              sql.Column('value', pytis.data.String(maxlen=6, not_null=False), _("Value")),
              )
    with_oids = True
    depends_on = ()
    access_rights = ()


class Slowlongtable(sql.SQLView):
    name = 'slowlongtable'

    @classmethod
    def query(cls):
        longtable = sql.t.Longtable.alias('long')
        return sqlalchemy.select(longtable.c +
                                 [(sval('x') + longtable.c.value).label('xvalue'),
                                  (ival(64) * longtable.c.id).label('id64')],
                                 from_obj=[longtable])

    depends_on = (Longtable,)
    access_rights = (('all', 'pytis-demo'), ('select', 'www-data'))


class Fastlongtable(sql.SQLMaterializedView, Slowlongtable):
    name = 'fastlongtable'
    index_columns = (('id',), ('value',),)
    access_rights = (('all', 'pytis-demo'), ('select', 'www-data'))


class TypeInsuranceFee(sql.SQLType):
    name = 'type_insurance_fee'
    fields = (sql.Column('fee', pytis.data.Integer(not_null=False), "Fee %"),
              sql.Column('risk', pytis.data.String(maxlen=8, not_null=False), "Risk"),
              )
    depends_on = ()
    access_rights = ()


class InsuranceFees(Base_PyFunction):
    name = 'insurance_fees'
    arguments = (sql.Column('value', pytis.data.Integer()),)
    result_type = TypeInsuranceFee
    multirow = True
    stability = 'VOLATILE'
    depends_on = (TypeInsuranceFee,)
    access_rights = ()

    @staticmethod
    def insurance_fees(value_arg):
        value_arg = args[0]
        if value_arg is None:
            return ((15, None), (25, None), (50, None), (5, None), (10, None), (20, None),
                    (1, None), (2, None), (3, None),)
        elif value_arg <= 1000:
            fees = (15, 25, 50,)
        elif value_arg <= 10000:
            fees = (5, 10, 20,)
        else:
            fees = (1, 2, 3,)
        return zip(fees, ('low', 'medium', 'high',))


class Insurance(sql.SQLTable):
    name = 'insurance'
    fields = (sql.PrimaryColumn('id', pytis.data.Serial(), "Id"),
              sql.Column('description', pytis.data.String(maxlen=32, not_null=False),
                         "Description"),
              sql.Column('value', pytis.data.Integer(not_null=False), "Value"),
              sql.Column('fee', pytis.data.Integer(not_null=True), "Fee %"),
              )
    with_oids = True
    depends_on = ()
    access_rights = ()


class Products(sql.SQLTable):
    name = 'products'
    fields = (sql.PrimaryColumn('product_id', pytis.data.Serial(), _("ID")),
              sql.Column('product', pytis.data.String(not_null=True), _("Product")),
              sql.Column('count', pytis.data.Integer(not_null=True), _("Count")),
              sql.Column('price', pytis.data.Float(precision=2, not_null=True), _("Price")),
              sql.Column('since', pytis.data.DateTime(not_null=True), _("Available since")),
              sql.Column('marked', pytis.data.Boolean(not_null=True), _("Marked"), default=False),
              sql.Column('notes', pytis.data.String(not_null=False), _("Notes")),
              )
    with_oids = True
    depends_on = ()
    access_rights = (('all', 'pytis-demo'), ('select', 'www-data'))


class RangeTypes(sql.SQLTable):
    name = 'range_types'
    fields = (sql.PrimaryColumn('range_id', pytis.data.Serial(), _("ID")),
              sql.Column('date_range', pytis.data.DateRange(not_null=True), _("Date")),
              sql.Column('datetime_range', pytis.data.DateTimeRange(not_null=True), _("DateTime")),
              sql.Column('int_range', pytis.data.IntegerRange(not_null=True), _("Integer")),
              )
    with_oids = True
    depends_on = ()
    access_rights = (('all', 'pytis-demo'), ('select', 'www-data'))


class RuntimeFilterDemo(sql.SQLTable):
    name = 'runtime_filter_demo'
    fields = (sql.PrimaryColumn('id', pytis.data.Serial()),
              sql.Column('product_id', pytis.data.Integer(not_null=True),
                         references=sql.r.Products),
              sql.Column('country', pytis.data.String(minlen=2, maxlen=2, not_null=True),
                         references=sql.r.Countries),
              sql.Column('continent', pytis.data.String(minlen=2, maxlen=2, not_null=True),
                         references=sql.r.Continents),
              )
    with_oids = True
    depends_on = ()
    access_rights = (('all', 'pytis-demo'), ('select', 'www-data'))


class Passwords(sql.SQLTable):
    name = 'passwords'
    fields = (sql.PrimaryColumn('id', pytis.data.Serial()),
              sql.Column('name', pytis.data.String(not_null=True), _("Name"), unique=True),
              sql.Column('passwd', pytis.data.String(not_null=True), _("Password")),
              )
    with_oids = True
    depends_on = ()
    access_rights = (('all', 'pytis-demo'), ('select', 'www-data'))


class BinaryData(sql.SQLTable):
    name = 'binary_data'
    fields = (sql.PrimaryColumn('id', pytis.data.Serial(), _("Id")),
              sql.Column('data', pytis.data.Binary(not_null=True), _("File")),
              sql.Column('descr', pytis.data.String(not_null=True), _("Description")),
              sql.Column('filename', pytis.data.String(not_null=True), _("File name")),
              )
    with_oids = True
    depends_on = ()
    access_rights = (('all', 'pytis-demo'), ('select', 'www-data'))


class Images(sql.SQLTable):
    name = 'images'
    fields = (sql.PrimaryColumn('id', pytis.data.Serial(), _("Id")),
              sql.Column('filename', pytis.data.String(not_null=True), _("Image")),
              sql.Column('data', pytis.data.Binary(not_null=True), _("Filename")),
              sql.Column('title', pytis.data.String(not_null=False), _("Title")),
              sql.Column('descr', pytis.data.String(not_null=False), _("Description")),
              sql.Column('size', pytis.data.String(not_null=True), _("Size")),
              )
    with_oids = True
    depends_on = ()
    access_rights = (('all', 'pytis-demo'), ('select', 'www-data'))


class XTree(sql.SQLTable):
    name = '_tree'
    fields = (sql.PrimaryColumn('id', pytis.data.LTree(not_null=False)),
              sql.Column('name', pytis.data.Name()),
              sql.Column('amount', pytis.data.Integer(not_null=False)),
              sql.Column('description', pytis.data.String(not_null=False)),
              )
    with_oids = True
    depends_on = ()
    access_rights = (('all', 'pytis-demo'), ('select', 'www-data'))


class Tree(sql.SQLView):
    name = 'tree'

    @classmethod
    def query(cls):
        main = sql.t.XTree.alias('main')
        return sqlalchemy.select(
            cls._exclude(main) +
            [main.c.id.label('tid'),
             ival("(select count(*)-1 from _tree where id <@ main.id)").label('id_nsub')],
            from_obj=[main]
        )

    insert_order = (XTree,)
    no_insert_columns = ('tid', 'id_nsub',)
    update_order = (XTree,)
    no_update_columns = ('tid', 'id_nsub',)
    delete_order = (XTree,)
    depends_on = (XTree,)
    access_rights = (('all', 'pytis-demo'), ('select', 'www-data'))


class Files(sql.SQLTable):
    name = 'files'
    fields = (sql.PrimaryColumn('id', pytis.data.Serial(), _("ID")),
              sql.Column('file', pytis.data.String(not_null=False), _("File")),
              sql.Column('url', pytis.data.String(not_null=False), _("URL")),
              )
    init_columns = ('id', 'file', 'url')
    init_values = ((0, '/Python26/README.txt', 'http://www.python.org',),)
    with_oids = True
    depends_on = ()
    access_rights = (('all', 'pytis-demo'), ('select', 'www-data'))


class TypeRandomNumbers(sql.SQLType):
    name = 'type_random_numbers'
    fields = (sql.Column('id', pytis.data.Integer(not_null=False), _("Id")),
              sql.Column('random', pytis.data.Integer(not_null=False), _("Random Number")),
              )
    depends_on = ()
    access_rights = ()


class RandomNumbers(Base_PyFunction):
    name = 'random_numbers'
    arguments = (sql.Column('count', pytis.data.Integer()),
                 sql.Column('minimum', pytis.data.Integer()),
                 sql.Column('maximum', pytis.data.Integer()),)
    result_type = TypeRandomNumbers
    multirow = True
    stability = 'VOLATILE'
    depends_on = (TypeRandomNumbers,)
    access_rights = ()

    @staticmethod
    def random_numbers(count, minimum, maximum):
        count, minimum, maximum = args
        import random
        return [(i, random.randint(minimum, maximum),) for i in range(1, count + 1)]
