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

# TODO: Rewrite to pytis.dbdefs.db_pytis_output.EPytisOutputTemplates.add_init_values()
# if this is still relevant...
#insert into e_pytis_output_templates (module, specification, template, rowtemplate)
#       values ('ContinentsDB', '', E'-- ${current_row.name} --\n\n'This template is taken from the database.  Selected continent: ${current_row.name}.', '${current_row.id} is a code of ${current_row.name}.');
#insert into e_pytis_output_templates (module, specification, template, rowtemplate)
#       values ('cb.Continents', 'Another database template', E'-- ${current_row.name} --\n\nThis template is only in the database, there is no corresponding entry in prints specification.  Selected continent: ${current_row.name}.', '${current_row.name} has code ${current_row.id}.');
#insert into e_pytis_output_templates (module, specification, template, rowtemplate)
#       values ('IteratedContinents', '', E'|   | *Header line* |\n| # | Continent: ${current_row.name} |\n', null);
#insert into e_pytis_output_templates (module, specification, template, rowtemplate)
#       values ('IteratedXContinents', '', E'|   | *Continents* |\n| # | Continent: ${current_row.name} |\n', E'Continent: ${current_row.name}\n\n| | *Countries* |\n| # | ${Binding.all_countries.data.name} |\n');


class CmsUsers(sql.SQLTable):
    name = 'cms_users'
    fields = (
        sql.PrimaryColumn('uid', pytis.data.Serial()),
        sql.Column('login', pytis.data.String(not_null=True), unique=True),
        sql.Column('passwd', pytis.data.String(not_null=True)),
        sql.Column('fullname', pytis.data.String(not_null=True)),
    )
    depends_on = ()
    access_rights = (('all', 'pytis-demo'), ('select', 'www-data'))


class CmsAttachments(sql.SQLTable):
    name = 'cms_attachments'
    fields = (
        sql.PrimaryColumn('file_id', pytis.data.LargeSerial()),
        sql.Column('menu_item_id', pytis.data.Integer(not_null=False),
                   references=sql.a(sql.r.CmsMenuStructure.menu_item_id)),
        sql.Column('file_name', pytis.data.String(not_null=True)),
        sql.Column('byte_size', pytis.data.Integer(not_null=True)),
        sql.Column('width', pytis.data.Integer()),
        sql.Column('height', pytis.data.Integer()),
        sql.Column('resized_width', pytis.data.Integer()),
        sql.Column('resized_height', pytis.data.Integer()),
        sql.Column('thumbnail_width', pytis.data.Integer()),
        sql.Column('thumbnail_height', pytis.data.Integer()),
        sql.Column('file', pytis.data.Binary(not_null=True)),
        sql.Column('resized', pytis.data.Binary()),
        sql.Column('thumbnail', pytis.data.Binary()),
    )
    unique = (('file_id', 'menu_item_id',),)


class Continents(sql.SQLTable):
    name = 'continents'
    fields = (
        sql.PrimaryColumn('id', pytis.data.String(minlen=2, maxlen=2, not_null=False), "Code"),
        sql.Column('name', pytis.data.String(not_null=True), "Continent name", unique=True),
        sql.Column('smallest', pytis.data.String(), "Smallest country"),
    )
    depends_on = ()
    access_rights = (('all', 'pytis-demo'), ('select', 'www-data'))

    init_columns = ('id', 'name')
    init_values = (
        ('AF', 'Africa'),
        ('AS', 'Asia'),
        ('EU', 'Europe'),
        ('NA', 'North America'),
        ('SA', 'South America'),
        ('OC', 'Oceania'),
        ('AN', 'Antarctica'),
    )


class Countries(sql.SQLTable):
    name = 'countries'
    fields = (
        sql.PrimaryColumn('id', pytis.data.String(minlen=2, maxlen=2, not_null=False),
                          "Alpha-2 code", "Uppercase ISO 3166 alpha-2 country code."),
        sql.Column('id3', pytis.data.String(minlen=3, maxlen=3, not_null=True),
                   "Alpha-3 code", "Uppercase ISO 3166 alpha-3 country code.", unique=True),
        sql.Column('num', pytis.data.String(minlen=3, maxlen=3, not_null=True),
                   "Numeric code", "ISO 3166 country number.", unique=True),
        sql.Column('continent', pytis.data.String(minlen=2, maxlen=2, not_null=True),
                   "Continent", "Uppercase alpha-2 continent code.", references=sql.r.Continents),
        sql.Column('name', pytis.data.String(not_null=True),
                   "Short name", unique=True),
        sql.Column('fullname', pytis.data.String(not_null=True), "Full name", unique=True),
    )
    depends_on = ()
    access_rights = (('all', 'pytis-demo'), ('select', 'www-data'))

    init_columns = ('id', 'id3', 'num', 'continent', 'name', 'fullname')
    init_values = (
        ('AF', 'AFG', '004', 'AS', 'Afghanistan', 'Afghanistan'),
        ('AX', 'ALA', '248', 'EU', 'Åland Islands', 'Åland Islands'),
        ('AL', 'ALB', '008', 'EU', 'Albania', 'Republic of Albania'),
        ('DZ', 'DZA', '012', 'AF', 'Algeria', 'People\'s Democratic Republic of Algeria'),
        ('AS', 'ASM', '016', 'OC', 'American Samoa', 'American Samoa'),
        ('AD', 'AND', '020', 'EU', 'Andorra', 'Principality of Andorra'),
        ('AO', 'AGO', '024', 'AF', 'Angola', 'Republic of Angola'),
        ('AI', 'AIA', '660', 'NA', 'Anguilla', 'Anguilla'),
        ('AQ', 'ATA', '010', 'AN', 'Antarctica', 'Antarctica (the territory South of 60 deg S)'),
        ('AG', 'ATG', '028', 'NA', 'Antigua and Barbuda', 'Antigua and Barbuda'),
        ('AR', 'ARG', '032', 'SA', 'Argentina', 'Argentine Republic'),
        ('AM', 'ARM', '051', 'AS', 'Armenia', 'Armenia'),
        ('AW', 'ABW', '533', 'NA', 'Aruba', 'Aruba'),
        ('AU', 'AUS', '036', 'OC', 'Australia', 'Commonwealth of Australia'),
        ('AT', 'AUT', '040', 'EU', 'Austria', 'Republic of Austria'),
        ('AZ', 'AZE', '031', 'AS', 'Azerbaijan', 'Republic of Azerbaijan'),
        ('BS', 'BHS', '044', 'NA', 'Bahamas', 'Commonwealth of the Bahamas'),
        ('BH', 'BHR', '048', 'AS', 'Bahrain', 'Kingdom of Bahrain'),
        ('BD', 'BGD', '050', 'AS', 'Bangladesh', 'People\'s Republic of Bangladesh'),
        ('BB', 'BRB', '052', 'NA', 'Barbados', 'Barbados'),
        ('BY', 'BLR', '112', 'EU', 'Belarus', 'Belarus'),
        ('BE', 'BEL', '056', 'EU', 'Belgium', 'Kingdom of Belgium'),
        ('BZ', 'BLZ', '084', 'NA', 'Belize', 'Belize'),
        ('BJ', 'BEN', '204', 'AF', 'Benin', 'People\'s Republic of Benin'),
        ('BM', 'BMU', '060', 'NA', 'Bermuda', 'Bermuda'),
        ('BT', 'BTN', '064', 'AS', 'Bhutan', 'Kingdom of Bhutan'),
        ('BO', 'BOL', '068', 'SA', 'Bolivia', 'Republic of Bolivia'),
        ('BA', 'BIH', '070', 'EU', 'Bosnia and Herzegovina', 'Bosnia and Herzegovina'),
        ('BW', 'BWA', '072', 'AF', 'Botswana', 'Republic of Botswana'),
        ('BV', 'BVT', '074', 'AN', 'Bouvet Island (Bouvetoya)', 'Bouvet Island (Bouvetoya)'),
        ('BR', 'BRA', '076', 'SA', 'Brazil', 'Federative Republic of Brazil'),
        ('IO', 'IOT', '086', 'AS', 'British Indian Ocean Territory',
         'British Indian Ocean Territory (Chagos Archipelago)'),
        ('VG', 'VGB', '092', 'NA', 'British Virgin Islands', 'British Virgin Islands'),
        ('BN', 'BRN', '096', 'AS', 'Brunei Darussalam', 'Brunei Darussalam'),
        ('BG', 'BGR', '100', 'EU', 'Bulgaria', 'People\'s Republic of Bulgaria'),
        ('BF', 'BFA', '854', 'AF', 'Burkina Faso', 'Burkina Faso'),
        ('BI', 'BDI', '108', 'AF', 'Burundi', 'Republic of Burundi'),
        ('KH', 'KHM', '116', 'AS', 'Cambodia', 'Kingdom of Cambodia'),
        ('CM', 'CMR', '120', 'AF', 'Cameroon', 'United Republic of Cameroon'),
        ('CA', 'CAN', '124', 'NA', 'Canada', 'Canada'),
        ('CV', 'CPV', '132', 'AF', 'Cape Verde', 'Republic of Cape Verde'),
        ('KY', 'CYM', '136', 'NA', 'Cayman Islands', 'Cayman Islands'),
        ('CF', 'CAF', '140', 'AF', 'Central African Republic', 'Central African Republic'),
        ('TD', 'TCD', '148', 'AF', 'Chad', 'Republic of Chad'),
        ('CL', 'CHL', '152', 'SA', 'Chile', 'Republic of Chile'),
        ('CN', 'CHN', '156', 'AS', 'China', 'People\'s Republic of China'),
        ('CX', 'CXR', '162', 'AS', 'Christmas Island', 'Christmas Island'),
        ('CC', 'CCK', '166', 'AS', 'Cocos (Keeling) Islands', 'Cocos (Keeling) Islands'),
        ('CO', 'COL', '170', 'SA', 'Colombia', 'Republic of Colombia'),
        ('KM', 'COM', '174', 'AF', 'Comoros', 'Union of the Comoros'),
        ('CD', 'COD', '180', 'AF', 'Congo', 'Democratic Republic of Congo'),
        ('CG', 'COG', '178', 'AF', 'People\'s Republic of Congo', 'People\'s Republic of Congo'),
        ('CK', 'COK', '184', 'OC', 'Cook Islands', 'Cook Islands'),
        ('CR', 'CRI', '188', 'NA', 'Costa Rica', 'Republic of Costa Rica'),
        ('CI', 'CIV', '384', 'AF', 'Cote D\'Ivoire', 'Republic of the Cote D\'Ivoire'),
        ('HR', 'HRV', '191', 'EU', 'Croatia', 'Republic of Croatia'),
        ('CU', 'CUB', '192', 'NA', 'Cuba', 'Republic of Cuba'),
        ('CY', 'CYP', '196', 'AS', 'Cyprus', 'Republic of Cyprus'),
        ('CZ', 'CZE', '203', 'EU', 'Czech Republic', 'Czech Republic'),
        ('DK', 'DNK', '208', 'EU', 'Denmark', 'Kingdom of Denmark'),
        ('DJ', 'DJI', '262', 'AF', 'Djibouti', 'Republic of Djibouti'),
        ('DM', 'DMA', '212', 'NA', 'Dominica', 'Commonwealth of Dominica'),
        ('DO', 'DOM', '214', 'NA', 'Dominican Republic', 'Dominican Republic'),
        ('EC', 'ECU', '218', 'SA', 'Ecuador', 'Republic of Ecuador'),
        ('EG', 'EGY', '818', 'AF', 'Egypt', 'Arab Republic of Egypt'),
        ('SV', 'SLV', '222', 'NA', 'El Salvador', 'Republic of El Salvador'),
        ('GQ', 'GNQ', '226', 'AF', 'Equatorial Guinea', 'Republic of Equatorial Guinea'),
        ('ER', 'ERI', '232', 'AF', 'Eritrea', 'Eritrea'),
        ('EE', 'EST', '233', 'EU', 'Estonia', 'Estonia'),
        ('ET', 'ETH', '231', 'AF', 'Ethiopia', 'Ethiopia'),
        ('FO', 'FRO', '234', 'EU', 'Faroe Islands', 'Faroe Islands'),
        ('FK', 'FLK', '238', 'SA', 'Falkland Islands (Malvinas)', 'Falkland Islands (Malvinas)'),
        ('FJ', 'FJI', '242', 'OC', 'Fiji', 'Republic of the Fiji Islands Fiji'),
        ('FI', 'FIN', '246', 'EU', 'Finland', 'Republic of Finland'),
        ('FR', 'FRA', '250', 'EU', 'France', 'French Republic'),
        ('GF', 'GUF', '254', 'SA', 'French Guiana', 'French Guiana'),
        ('PF', 'PYF', '258', 'OC', 'French Polynesia', 'French Polynesia'),
        ('TF', 'ATF', '260', 'AN', 'French Southern Territories', 'French Southern Territories'),
        ('GA', 'GAB', '266', 'AF', 'Gabon', 'Gabonese Republic'),
        ('GM', 'GMB', '270', 'AF', 'Gambia', 'Republic of the Gambia'),
        ('GE', 'GEO', '268', 'AS', 'Georgia', 'Georgia'),
        ('DE', 'DEU', '276', 'EU', 'Germany', 'Germany'),
        ('GH', 'GHA', '288', 'AF', 'Ghana', 'Republic of Ghana'),
        ('GI', 'GIB', '292', 'EU', 'Gibraltar', 'Gibraltar'),
        ('GR', 'GRC', '300', 'EU', 'Greece', 'Hellenic Republic'),
        ('GL', 'GRL', '304', 'NA', 'Greenland', 'Greenland'),
        ('GD', 'GRD', '308', 'NA', 'Grenada', 'Grenada'),
        ('GP', 'GLP', '312', 'NA', 'Guadaloupe', 'Guadaloupe'),
        ('GU', 'GUM', '316', 'OC', 'Guam', 'Guam'),
        ('GT', 'GTM', '320', 'NA', 'Guatemala', 'Republic of Guatemala'),
        ('GG', 'GGY', '831', 'EU', 'Guernsey', 'Bailiwick of Guernsey'),
        ('GN', 'GIN', '324', 'AF', 'Guinea', 'Revolutionary People\'s Rep\'c of Guinea'),
        ('GW', 'GNB', '624', 'AF', 'Guinea-Bissau', 'Republic of Guinea-Bissau'),
        ('GY', 'GUY', '328', 'SA', 'Guyana', 'Republic of Guyana'),
        ('HT', 'HTI', '332', 'NA', 'Haiti', 'Republic of Haiti'),
        ('HM', 'HMD', '334', 'AN', 'Heard and McDonald Islands', 'Heard and McDonald Islands'),
        ('VA', 'VAT', '336', 'EU', 'Vatican City State', 'Vatican City State'),
        ('HN', 'HND', '340', 'NA', 'Honduras', 'Republic of Honduras'),
        ('HK', 'HKG', '344', 'AS', 'Hong Kong',
         'Hong Kong, Special Administrative Region of China'),
        ('HU', 'HUN', '348', 'EU', 'Hungary', 'Hungarian People\'s Republic'),
        ('IS', 'ISL', '352', 'EU', 'Iceland', 'Republic of Iceland'),
        ('IN', 'IND', '356', 'AS', 'India', 'Republic of India'),
        ('ID', 'IDN', '360', 'AS', 'Indonesia', 'Republic of Indonesia'),
        ('IR', 'IRN', '364', 'AS', 'Iran', 'Islamic Republic of Iran'),
        ('IQ', 'IRQ', '368', 'AS', 'Iraq', 'Republic of Iraq'),
        ('IE', 'IRL', '372', 'EU', 'Ireland', 'Ireland'),
        ('IM', 'IMN', '833', 'EU', 'Isle of Man', 'Isle of Man'),
        ('IL', 'ISR', '376', 'AS', 'Israel', 'State of Israel'),
        ('IT', 'ITA', '380', 'EU', 'Italy', 'Italian Republic'),
        ('JM', 'JAM', '388', 'NA', 'Jamaica', 'Jamaica'),
        ('JP', 'JPN', '392', 'AS', 'Japan', 'Japan'),
        ('JE', 'JEY', '832', 'EU', 'Jersey', 'Bailiwick of Jersey'),
        ('JO', 'JOR', '400', 'AS', 'Jordan', 'Hashemite Kingdom of Jordan'),
        ('KZ', 'KAZ', '398', 'AS', 'Kazakhstan', 'Republic of Kazakhstan'),
        ('KE', 'KEN', '404', 'AF', 'Kenya', 'Republic of Kenya'),
        ('KI', 'KIR', '296', 'OC', 'Kiribati', 'Republic of Kiribati'),
        ('KP', 'PRK', '408', 'AS', 'North Korea', 'Democratic People\'s Republic of Korea'),
        ('KR', 'KOR', '410', 'AS', 'South Korea', 'Republic of Korea'),
        ('KW', 'KWT', '414', 'AS', 'Kuwait', 'State of Kuwait'),
        ('KG', 'KGZ', '417', 'AS', 'Kyrgyz Republic', 'Kyrgyz Republic'),
        ('LA', 'LAO', '418', 'AS', 'Lao', 'People\'s Democratic Republic of Lao'),
        ('LV', 'LVA', '428', 'EU', 'Latvia', 'Latvia'),
        ('LB', 'LBN', '422', 'AS', 'Lebanon', 'Lebanese Republic'),
        ('LS', 'LSO', '426', 'AF', 'Lesotho', 'Kingdom of Lesotho'),
        ('LR', 'LBR', '430', 'AF', 'Liberia', 'Republic of Liberia'),
        ('LY', 'LBY', '434', 'AF', 'Libyan Arab Jamahiriya', 'Libyan Arab Jamahiriya'),
        ('LI', 'LIE', '438', 'EU', 'Liechtenstein', 'Principality of Liechtenstein'),
        ('LT', 'LTU', '440', 'EU', 'Lithuania', 'Lithuania'),
        ('LU', 'LUX', '442', 'EU', 'Luxembourg', 'Grand Duchy of Luxembourg'),
        ('MO', 'MAC', '446', 'AS', 'Macao', 'Macao, Special Administrative Region of China'),
        ('MK', 'MKD', '807', 'EU', 'Macedonia', 'Macedonia'),
        ('MG', 'MDG', '450', 'AF', 'Madagascar', 'Republic of Madagascar'),
        ('MW', 'MWI', '454', 'AF', 'Malawi', 'Republic of Malawi'),
        ('MY', 'MYS', '458', 'AS', 'Malaysia', 'Malaysia'),
        ('MV', 'MDV', '462', 'AS', 'Maldives', 'Republic of Maldives'),
        ('ML', 'MLI', '466', 'AF', 'Mali', 'Republic of Mali'),
        ('MT', 'MLT', '470', 'EU', 'Malta', 'Republic of Malta'),
        ('MH', 'MHL', '584', 'OC', 'Marshall Islands', 'Marshall Islands'),
        ('MQ', 'MTQ', '474', 'NA', 'Martinique', 'Martinique'),
        ('MR', 'MRT', '478', 'AF', 'Mauritania', 'Islamic Republic of Mauritania'),
        ('MU', 'MUS', '480', 'AF', 'Mauritius', 'Mauritius'),
        ('YT', 'MYT', '175', 'AF', 'Mayotte', 'Mayotte'),
        ('MX', 'MEX', '484', 'NA', 'Mexico', 'United Mexican States'),
        ('FM', 'FSM', '583', 'OC', 'Micronesia', 'Federated States of Micronesia'),
        ('MD', 'MDA', '498', 'EU', 'Moldova', 'Republic of Moldova'),
        ('MC', 'MCO', '492', 'EU', 'Monaco', 'Principality of Monaco'),
        ('MN', 'MNG', '496', 'AS', 'Mongolia', 'Mongolian People\'s Republic'),
        ('ME', 'MNE', '499', 'EU', 'Montenegro', 'Republic of Montenegro'),
        ('MS', 'MSR', '500', 'NA', 'Montserrat', 'Montserrat'),
        ('MA', 'MAR', '504', 'AF', 'Morocco', 'Kingdom of Morocco'),
        ('MZ', 'MOZ', '508', 'AF', 'Mozambique', 'People\'s Republic of Mozambique'),
        ('MM', 'MMR', '104', 'AS', 'Myanmar', 'Myanmar'),
        ('NA', 'NAM', '516', 'AF', 'Namibia', 'Namibia'),
        ('NR', 'NRU', '520', 'OC', 'Nauru', 'Republic of Nauru'),
        ('NP', 'NPL', '524', 'AS', 'Nepal', 'Kingdom of Nepal'),
        ('AN', 'ANT', '530', 'NA', 'Netherlands Antilles', 'Netherlands Antilles'),
        ('NL', 'NLD', '528', 'EU', 'Netherlands', 'Kingdom of the Netherlands'),
        ('NC', 'NCL', '540', 'OC', 'New Caledonia', 'New Caledonia'),
        ('NZ', 'NZL', '554', 'OC', 'New Zealand', 'New Zealand'),
        ('NI', 'NIC', '558', 'NA', 'Nicaragua', 'Republic of Nicaragua'),
        ('NE', 'NER', '562', 'AF', 'Niger', 'Republic of the Niger'),
        ('NG', 'NGA', '566', 'AF', 'Nigeria', 'Federal Republic of Nigeria'),
        ('NU', 'NIU', '570', 'OC', 'Niue', 'Republic of Niue'),
        ('NF', 'NFK', '574', 'OC', 'Norfolk Island', 'Norfolk Island'),
        ('MP', 'MNP', '580', 'OC', 'Northern Mariana Islands', 'Northern Mariana Islands'),
        ('NO', 'NOR', '578', 'EU', 'Norway', 'Kingdom of Norway'),
        ('OM', 'OMN', '512', 'AS', 'Oman', 'Sultanate of Oman'),
        ('PK', 'PAK', '586', 'AS', 'Pakistan', 'Islamic Republic of Pakistan'),
        ('PW', 'PLW', '585', 'OC', 'Palau', 'Palau'),
        ('PS', 'PSE', '275', 'AS', 'Palestinian Territory', 'Occupied Palestinian Territory'),
        ('PA', 'PAN', '591', 'NA', 'Panama', 'Republic of Panama'),
        ('PG', 'PNG', '598', 'OC', 'Papua New Guinea', 'Papua New Guinea'),
        ('PY', 'PRY', '600', 'SA', 'Paraguay', 'Republic of Paraguay'),
        ('PE', 'PER', '604', 'SA', 'Peru', 'Republic of Peru'),
        ('PH', 'PHL', '608', 'AS', 'Philippines', 'Republic of the Philippines'),
        ('PN', 'PCN', '612', 'OC', 'Pitcairn Island', 'Pitcairn Island'),
        ('PL', 'POL', '616', 'EU', 'Poland', 'Polish People\'s Republic Poland'),
        ('PT', 'PRT', '620', 'EU', 'Portugal', 'Portuguese Republic'),
        ('PR', 'PRI', '630', 'NA', 'Puerto Rico', 'Puerto Rico'),
        ('QA', 'QAT', '634', 'AS', 'Qatar', 'State of Qatar'),
        ('RE', 'REU', '638', 'AF', 'Reunion', 'Reunion'),
        ('RO', 'ROU', '642', 'EU', 'Romania', 'Socialist Republic of Romania'),
        ('RU', 'RUS', '643', 'EU', 'Russian Federation', 'Russian Federation'),
        ('RW', 'RWA', '646', 'AF', 'Rwanda', 'Rwandese Republic Rwanda'),
        ('SH', 'SHN', '654', 'AF', 'St. Helena', 'St. Helena'),
        ('KN', 'KNA', '659', 'NA', 'St. Kitts and Nevis', 'St. Kitts and Nevis'),
        ('LC', 'LCA', '662', 'NA', 'St. Lucia', 'St. Lucia'),
        ('PM', 'SPM', '666', 'NA', 'St. Pierre and Miquelon', 'St. Pierre and Miquelon'),
        ('VC', 'VCT', '670', 'NA', 'St. Vincent and the Grenadines',
         'St. Vincent and the Grenadines'),
        ('WS', 'WSM', '882', 'OC', 'Samoa', 'Independent State of Samoa'),
        ('SM', 'SMR', '674', 'EU', 'San Marino', 'Republic of San Marino'),
        ('ST', 'STP', '678', 'AF', 'Sao Tome and Principe',
         'Democratic Republic of Sao Tome and Principe'),
        ('SA', 'SAU', '682', 'AS', 'Saudi Arabia', 'Kingdom of Saudi Arabia'),
        ('SN', 'SEN', '686', 'AF', 'Senegal', 'Republic of Senegal'),
        ('RS', 'SRB', '688', 'EU', 'Serbia', 'Republic of Serbia'),
        ('SC', 'SYC', '690', 'AF', 'Seychelles', 'Republic of Seychelles'),
        ('SL', 'SLE', '694', 'AF', 'Sierra Leone', 'Republic of Sierra Leone'),
        ('SG', 'SGP', '702', 'AS', 'Singapore', 'Republic of Singapore'),
        ('SK', 'SVK', '703', 'EU', 'Slovakia', 'Slovak Republic'),
        ('SI', 'SVN', '705', 'EU', 'Slovenia', 'Slovenia'),
        ('SB', 'SLB', '090', 'OC', 'Solomon Islands', 'Solomon Islands'),
        ('SO', 'SOM', '706', 'AF', 'Somalia', 'Somali Republic'),
        ('ZA', 'ZAF', '710', 'AF', 'South Africa', 'Republic of South Africa'),
        ('GS', 'SGS', '239', 'AN', 'South Georgia and the South Sandwich Islands',
         'South Georgia and the South Sandwich Islands'),
        ('ES', 'ESP', '724', 'EU', 'Spain', 'Spanish State'),
        ('LK', 'LKA', '144', 'AS', 'Sri Lanka', 'Democratic Socialist Republic of Sri Lanka'),
        ('SD', 'SDN', '736', 'AF', 'Sudan', 'Democratic Republic of the Sudan'),
        ('SR', 'SUR', '740', 'SA', 'Suriname', 'Republic of Suriname'),
        ('SJ', 'SJM', '744', 'EU', 'Svalbard & Jan Mayen Islands', 'Svalbard & Jan Mayen Islands'),
        ('SZ', 'SWZ', '748', 'AF', 'Swaziland', 'Kingdom of Swaziland'),
        ('SE', 'SWE', '752', 'EU', 'Sweden', 'Kingdom of Sweden'),
        ('CH', 'CHE', '756', 'EU', 'Switzerland', 'Swiss Confederation'),
        ('SY', 'SYR', '760', 'AS', 'Syrian Arab Republic', 'Syrian Arab Republic'),
        ('TW', 'TWN', '158', 'AS', 'Taiwan', 'Taiwan, Province of China'),
        ('TJ', 'TJK', '762', 'AS', 'Tajikistan', 'Tajikistan'),
        ('TZ', 'TZA', '834', 'AF', 'Tanzania', 'United Republic of Tanzania'),
        ('TH', 'THA', '764', 'AS', 'Thailand', 'Kingdom of Thailand'),
        ('TL', 'TLS', '626', 'AS', 'Timor-Leste', 'Democratic Republic of Timor-Leste'),
        ('TG', 'TGO', '768', 'AF', 'Togo', 'Togolese Republic'),
        ('TK', 'TKL', '772', 'OC', 'Tokelau', 'Tokelau Islands'),
        ('TO', 'TON', '776', 'OC', 'Tonga', 'Kingdom of Tonga'),
        ('TT', 'TTO', '780', 'NA', 'Trinidad and Tobago', 'Republic of Trinidad and Tobago'),
        ('TN', 'TUN', '788', 'AF', 'Tunisia', 'Republic of Tunisia'),
        ('TR', 'TUR', '792', 'AS', 'Turkey', 'Republic of Turkey'),
        ('TM', 'TKM', '795', 'AS', 'Turkmenistan', 'Turkmenistan'),
        ('TC', 'TCA', '796', 'NA', 'Turks and Caicos Islands', 'Turks and Caicos Islands'),
        ('TV', 'TUV', '798', 'OC', 'Tuvalu', 'Tuvalu'),
        ('VI', 'VIR', '850', 'NA', 'US Virgin Islands', 'US Virgin Islands'),
        ('UG', 'UGA', '800', 'AF', 'Uganda', 'Republic of Uganda'),
        ('UA', 'UKR', '804', 'EU', 'Ukraine', 'Ukraine'),
        ('AE', 'ARE', '784', 'AS', 'United Arab Emirates', 'United Arab Emirates'),
        ('GB', 'GBR', '826', 'EU', 'Great Britain', 'United Kingdom of Great Britain & N. Ireland'),
        ('UM', 'UMI', '581', 'OC', 'United States Minor Outlying Islands',
         'United States Minor Outlying Islands'),
        ('US', 'USA', '840', 'NA', 'United States of America', 'United States of America'),
        ('UY', 'URY', '858', 'SA', 'Uruguay', 'Eastern Republic of Uruguay'),
        ('UZ', 'UZB', '860', 'AS', 'Uzbekistan', 'Uzbekistan'),
        ('VU', 'VUT', '548', 'OC', 'Vanuatu', 'Vanuatu'),
        ('VE', 'VEN', '862', 'SA', 'Venezuela', 'Bolivarian Republic of Venezuela'),
        ('VN', 'VNM', '704', 'AS', 'Viet Nam', 'Socialist Republic of Viet Nam'),
        ('WF', 'WLF', '876', 'OC', 'Wallis and Futuna Islands', 'Wallis and Futuna Islands'),
        ('EH', 'ESH', '732', 'AF', 'Western Sahara', 'Western Sahara'),
        ('YE', 'YEM', '887', 'AS', 'Yemen', 'Yemen'),
        ('ZM', 'ZMB', '894', 'AF', 'Zambia', 'Republic of Zambia'),
        ('ZW', 'ZWE', '716', 'AF', 'Zimbabwe', 'Zimbabwe'),
    )

class Longtable(sql.SQLTable):
    name = 'longtable'
    fields = (
        sql.PrimaryColumn('id', pytis.data.Serial(), _("ID")),
        sql.Column('value', pytis.data.String(maxlen=6, not_null=False), _("Value")),
    )
    depends_on = ()
    access_rights = ()


class Slowlongtable(sql.SQLView):
    name = 'slowlongtable'

    @classmethod
    def query(cls):
        longtable = sql.t.Longtable.alias('long')
        return sqlalchemy.select(list(longtable.c) +
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
    fields = (
        sql.Column('fee', pytis.data.Integer(not_null=False), "Fee %"),
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
    fields = (
        sql.PrimaryColumn('id', pytis.data.Serial(), "Id"),
        sql.Column('description', pytis.data.String(maxlen=32, not_null=False), "Description"),
        sql.Column('value', pytis.data.Integer(not_null=False), "Value"),
        sql.Column('fee', pytis.data.Integer(not_null=True), "Fee %"),
    )
    depends_on = ()
    access_rights = ()
    init_columns = ('description', 'value', 'fee')
    init_values = (
        ('car crash', 5000, 10),
        ('lost diamond', 100000, 3),
    )


class Products(sql.SQLTable):
    name = 'products'
    fields = (
        sql.PrimaryColumn('product_id', pytis.data.Serial(), _("ID")),
        sql.Column('product', pytis.data.String(not_null=True), _("Product")),
        sql.Column('count', pytis.data.Integer(not_null=True), _("Count")),
        sql.Column('price', pytis.data.Float(precision=2, not_null=True), _("Price")),
        sql.Column('since', pytis.data.DateTime(not_null=True), _("Available since")),
        sql.Column('marked', pytis.data.Boolean(not_null=True), _("Marked"), default=False),
        sql.Column('notes', pytis.data.String(not_null=False), _("Notes")),
    )
    depends_on = ()
    access_rights = (('all', 'pytis-demo'), ('select', 'www-data'))

    init_columns = ('product', 'count', 'price', 'marked', 'since', 'notes')
    init_values = (
        ('HP LaserJet 5L', 0, 399.99, False, '1994-08-18', None),
        ('HP LaserJet 3050', 32, 299.99, True, '2004-04-08 10:55:20', 'Free standard shipping'),
        ('HP Photosmart C7180', 8, 399.99, True, '2006-10-11 16:32:41', None),
        ('HP LaserJet 1020', 5, 179.99, False, '2005-12-01 08:52:22', 'Free standard shipping'),
        ('HP DeskJet 640s', 2, 188.00, True, '2007-10-11 10:21:00', None),
        ('HP DeskJet 347A', 6, 219.99, True, '2007-10-11 11:07:00', None),
        ('HP DeskJet 24D', 9, 249.99, True, '2007-10-11 11:07:00', None),
    )


class RangeTypes(sql.SQLTable):
    name = 'range_types'
    fields = (
        sql.PrimaryColumn('range_id', pytis.data.Serial(), _("ID")),
        sql.Column('date_range', pytis.data.DateRange(not_null=True), _("Date")),
        sql.Column('datetime_range', pytis.data.DateTimeRange(not_null=True), _("DateTime")),
        sql.Column('int_range', pytis.data.IntegerRange(not_null=True), _("Integer")),
    )
    depends_on = ()
    access_rights = (('all', 'pytis-demo'), ('select', 'www-data'))


class RuntimeFilterDemo(sql.SQLTable):
    name = 'runtime_filter_demo'
    fields = (
        sql.PrimaryColumn('id', pytis.data.Serial()),
        sql.Column('product_id', pytis.data.Integer(not_null=True), references=sql.r.Products),
        sql.Column('country', pytis.data.String(minlen=2, maxlen=2, not_null=True),
                   references=sql.r.Countries),
        sql.Column('continent', pytis.data.String(minlen=2, maxlen=2, not_null=True),
                   references=sql.r.Continents),
    )
    depends_on = ()
    access_rights = (('all', 'pytis-demo'), ('select', 'www-data'))


class Passwords(sql.SQLTable):
    name = 'passwords'
    fields = (
        sql.PrimaryColumn('id', pytis.data.Serial()),
        sql.Column('name', pytis.data.String(not_null=True), _("Name"), unique=True),
        sql.Column('passwd', pytis.data.String(not_null=True), _("Password")),
    )
    depends_on = ()
    access_rights = (('all', 'pytis-demo'), ('select', 'www-data'))


class BinaryData(sql.SQLTable):
    name = 'binary_data'
    fields = (
        sql.PrimaryColumn('id', pytis.data.Serial(), _("Id")),
        sql.Column('data', pytis.data.Binary(not_null=True), _("File")),
        sql.Column('descr', pytis.data.String(not_null=True), _("Description")),
        sql.Column('filename', pytis.data.String(not_null=True), _("File name")),
    )
    depends_on = ()
    access_rights = (('all', 'pytis-demo'), ('select', 'www-data'))


class Images(sql.SQLTable):
    name = 'images'
    fields = (
        sql.PrimaryColumn('id', pytis.data.Serial(), _("Id")),
        sql.Column('filename', pytis.data.String(not_null=True), _("Image")),
        sql.Column('data', pytis.data.Binary(not_null=True), _("Filename")),
        sql.Column('title', pytis.data.String(not_null=False), _("Title")),
        sql.Column('descr', pytis.data.String(not_null=False), _("Description")),
        sql.Column('size', pytis.data.String(not_null=True), _("Size")),
    )
    depends_on = ()
    access_rights = (('all', 'pytis-demo'), ('select', 'www-data'))


class XTree(sql.SQLTable):
    name = '_tree'
    fields = (
        sql.PrimaryColumn('id', pytis.data.LTree(not_null=False)),
        sql.Column('name', pytis.data.Name()),
        sql.Column('amount', pytis.data.Integer(not_null=False)),
        sql.Column('description', pytis.data.String(not_null=False)),
    )
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

    init_columns = ('id, name', 'amount', 'description')
    init_values = (
        ('ovoce', 'Fruit', None, ''),
        ('ovoce.jablka', 'Apple', 5, 'Red balls'),
        ('ovoce.pomeranče', 'Orange', None, 'Orange balls'),
        ('ovoce.pomeranče.španělské', 'European Orange', 3, 'Orange balls from Spain'),
        ('ovoce.pomeranče.kubánské', 'American Orange', 2, 'Orange balls from Cuba'),
        ('zelenina', 'Vegetable', None, ''),
        ('zelenina.čočka', 'Lentils', 10, 'Many brown seeds'),
        ('zelenina.salát', 'Lettuce', 4, 'Green leaves'),
        ('zelenina.špenát', 'Spinach', 1, 'Other green leaves'),
    )

class Files(sql.SQLTable):
    name = 'files'
    fields = (
        sql.PrimaryColumn('id', pytis.data.Serial(), _("ID")),
        sql.Column('file', pytis.data.String(not_null=False), _("File")),
        sql.Column('url', pytis.data.String(not_null=False), _("URL")),
    )
    init_columns = ('id', 'file', 'url')
    init_values = ((0, '/Python26/README.txt', 'http://www.python.org',),)
    depends_on = ()
    access_rights = (('all', 'pytis-demo'), ('select', 'www-data'))


class TypeRandomNumbers(sql.SQLType):
    name = 'type_random_numbers'
    fields = (
        sql.Column('id', pytis.data.Integer(not_null=False), _("Id")),
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
