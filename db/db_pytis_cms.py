import pytis.data as pd
Relation = SelectRelation

db_rights = ()
cms_rights = db_rights + (('select', '"www-data"'),)

table('cms_languages',
      doc="Codebook of languages available in the CMS.",
      columns=(PrimaryColumn('lang_id', pd.Serial()),
               Column('lang', pd.String(minlen=2, maxlen=2), constraints=('UNIQUE', 'NOT NULL'))),
      grant=cms_rights)

table('cms_modules',
      doc="Codebook of extension modules available in the CMS.",
      columns=(PrimaryColumn('mod_id', pd.Serial()),
               Column('modname', pd.String(maxlen=64), constraints=('UNIQUE', 'NOT NULL'))),
      grant=cms_rights)

table('cms_menu_structure',
      doc="Language independent menu structure.",
      columns=(PrimaryColumn('menu_item_id', pd.Serial()),
               Column('identifier', pd.String(maxlen=32), constraints=('UNIQUE', 'NOT NULL')),
               Column('parent', pd.Integer(), references='cms_menu_structure'),
               Column('mod_id', pd.Integer(), references='cms_modules',),
               Column('ord', pd.Integer(), constraints=('NOT NULL',)),
               Column('tree_order', pd.String())),
      grant=cms_rights)

sql_raw("CREATE UNIQUE INDEX cms_menu_structure_unique_tree_order "
        "ON cms_menu_structure (ord, coalesce(parent, 0));",
        name='cms_menu_structure_unique_tree_order',
        depends=('cms_menu_structure',))

function('cms_menu_structure_tree_order',
         doc="Generate a sortable string representing the hierarchical position of given menu item.",
         arguments=(pd.Integer(),), output_type=pd.String(),
         body="""
         SELECT CASE WHEN $1 IS NULL THEN '''' ELSE
           (SELECT cms_menu_structure_tree_order(parent) || ''.'' ||
                   to_char(coalesce(ord, 999999), ''FM000000'')
            FROM cms_menu_structure where menu_item_id=$1)
         END AS RESULT""",
         depends=('cms_menu_structure', ))

table('cms_menu_texts',
      doc="Language dependent texts and properties for menu items.",
      columns=(Column('menu_item_id', pd.Integer(), constraints=('NOT NULL',),
                      references='cms_menu_structure ON DELETE CASCADE'),
               Column('lang',  pd.String(minlen=2, maxlen=2),  constraints=('NOT NULL',),
                      references='cms_languages(lang)'),
               Column('published', pd.Boolean(), constraints=('NOT NULL',),
                      default="'TRUE'"),
               Column('title', pd.String(), constraints=('NOT NULL',)),
               Column('description', pd.String()),
               Column('content', pd.String())),
      sql='PRIMARY KEY (menu_item_id, lang)')

viewng('cms_menu',
       doc="Complete menu structure with texts for each language defined in cms_languages.",
       relations=(Relation('cms_menu_structure', alias='s', key_column='menu_item_id'),
                  Relation('cms_languages', alias='l', key_column='lang',
                           jointype=JoinType.CROSS,exclude_columns=('lang_id',)),
                  Relation('cms_menu_texts', alias='t', key_column=('menu_item_id', 'lang'),
                           jointype=JoinType.LEFT_OUTER,
                           condition='t.menu_item_id = s.menu_item_id AND t.lang = l.lang',
                           exclude_columns=('menu_item_id', 'lang', 'published')),
                  Relation('cms_modules', alias='m', key_column='mod_id',
                           jointype=JoinType.LEFT_OUTER,
                           condition='m.mod_id = s.mod_id',
                           exclude_columns=('mod_id',))),
       include_columns=(ViewColumn(None, alias='menu_id', sql="s.menu_item_id ||'.'|| l.lang"),
                        ViewColumn(None, alias='published', sql="coalesce(t.published, 'FALSE')"),
                        ViewColumn(None, alias='title_or_identifier',
                                   sql="coalesce(t.title, s.identifier)"), 
                        ),
       insert="""(
       INSERT INTO cms_menu_structure (identifier, parent, mod_id, ord)
       VALUES (new.identifier, new.parent, new.mod_id,
               coalesce(new.ord, (SELECT max(ord)+100 FROM cms_menu_structure 
                                  WHERE coalesce(parent, 0)=coalesce(new.parent, 0)), 100));
       UPDATE cms_menu_structure SET tree_order = cms_menu_structure_tree_order(menu_item_id);
       INSERT INTO cms_menu_texts (menu_item_id, lang, published, title, description, content)
       SELECT (SELECT menu_item_id FROM cms_menu_structure WHERE identifier=new.identifier),
              new.lang, new.published, new.title, new.description, new.content
       RETURNING 
          menu_item_id, NULL::varchar(32), NULL::int, NULL::int, NULL::int, NULL::text, 
          lang, title, description, content, NULL::varchar(64), 
          menu_item_id ||'.'|| lang, published, title
       )""",
       update="""(
       UPDATE cms_menu_structure SET
         identifier = new.identifier,
         parent = new.parent,
         mod_id = new.mod_id,
         ord = new.ord
       WHERE cms_menu_structure.menu_item_id = old.menu_item_id;
       UPDATE cms_menu_structure SET tree_order = cms_menu_structure_tree_order(menu_item_id);
       UPDATE cms_menu_texts SET
         published = new.published,
         title = new.title,
         description = new.description,
         content = new.content
       WHERE menu_item_id = old.menu_item_id AND lang = new.lang;
       INSERT INTO cms_menu_texts (menu_item_id, lang, published, title, description, content)
         SELECT old.menu_item_id, new.lang, new.published, new.title, new.description, new.content
           WHERE new.lang NOT IN (SELECT lang FROM cms_menu_texts WHERE menu_item_id=old.menu_item_id)
                 AND (new.title IS NOT NULL OR new.description IS NOT NULL OR new.content IS NOT NULL);
       )""",
       delete="(DELETE FROM cms_menu_structure WHERE menu_item_id = old.menu_item_id;)",
       grant=cms_rights,
       depends=('cms_menu_structure', 'cms_languages', 'cms_menu_texts', 'cms_modules'),
       )

