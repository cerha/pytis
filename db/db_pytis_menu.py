#-*- coding: iso-8859-2 -*-
"""Tento soubor obsahuje definice databázových objektů pro definici menu aplikace
udržované v přímo v databázi. Do hlavního db.py jednotlivých projektů
se přidá symbolickým přilinkováním do odpovídajícího adresáře db v projektu
a přidáním příkazů

Gpytis_menu = <práva pro objekty tohoto souboru>
execfile('db_pytis_menu.py', copy.copy(globals()))
"""

db_rights = globals().get('Gpytis_menu', None)

if not db_rights:
    raise ProgramError('No rights specified! Please define Gpytis_menu')


table('_pytis_menu_actions',
      (P('id', varchar(3)),
       C('action', TString),
       ),
      grant=db_rights,
      doc="Codebook of pytis menu actions.",
      init_columns=('id', 'action'),
      init_values=(
        ("'bf'", "'BrowseForm'"),
        ("'df'", "'BrowseDualForm'"),
        ("'sf'", "'ShowForm'"),
        ("'ddf'", "'DescriptiveDualForm'"),
        ("'ef'", "'PopupEditForm'"),
        ("'nr'", "'New Record'"),
        ("'rp'", "'Run Procedure'"),
      )
      
table('_pytis_spec',
      (P('id', TSerial),
       C('main_spec', TString, constraints=('NOT NULL',)),
       C('side_spec', TString),
       C('shortcut', TString),
       C('action', varchar(3), references='_pytis_menu_actions ON UPDATE CASCADE ON DELETE CASCADE'),
       ),
      grant=db_rights,
      doc="""Stores combinations of specifications used by _pytis_menu."""      
      )

table('_pytis_menu',
      (P('id', TSerial),
       C('module', TString),
       C('title', TString, constraints=('NOT NULL',)),
       C('id_spec', TInteger, references='_pytis_actions ON UPDATE CASCADE ON DELETE CASCADE',
         constraints=('NOT NULL',)),
       C('id_parent', TInteger, references='_pytis_menu'),
       C('ordering', TString)
       ),
      grant=db_rights,
      doc="""Stores menu structure."""      
      )


      CREATE OR REPLACE FUNCTION _tree_order(menu_id int) RETURNS text AS $$
  SELECT
    CASE WHEN $1 IS NULL THEN '' ELSE
      (SELECT _tree_order(parent) || '.' ||
       to_char(coalesce(ord, 999999), 'FM000000')
       FROM menu where menu_id=$1)
    END
  AS RESULT
$$ LANGUAGE SQL;
      
function('_pytis_menu_ordering',
         (TInteger,),
         TString,
         body=("insert into _pytis_config (uzivatel, config) "
               "select current_user, NULL::text "
               "where (select current_user) not in "
               "(select uzivatel from _pytis_config); "
               "select config from _pytis_config "
               "where uzivatel = (select current_user)"),
         doc="""Funkce na zjišťování configu pro daného uživatele""",
         depends=('_pytis_config',))

function('write_pytis_config',
         (TString,),
         TString,
         body=("update _pytis_config set config = $1 "
               "where uzivatel = (select current_user);"
               "insert into _pytis_config (uzivatel, config) "
               "select current_user, $1::text "
               "where (select current_user) not in "
               "(select uzivatel from _pytis_config); "               
               "select config from _pytis_config "
               "where uzivatel = (select current_user)"
               ),
         doc="""Funkce na zápis configu pro daného uživatele.""",
         depends=('_pytis_config',))

