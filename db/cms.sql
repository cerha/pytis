-- Pytis Content Management System data structures.

CREATE TABLE cms_languages (
	lang_id serial PRIMARY KEY,
	lang char(2) UNIQUE NOT NULL
);

CREATE TABLE cms_modules (
       	mod_id serial PRIMARY KEY,
	modname varchar(64) UNIQUE NOT NULL
);

CREATE TABLE cms_menu_structure (
       	menu_item_id serial PRIMARY KEY,
	identifier varchar(32) UNIQUE NOT NULL,
	parent integer REFERENCES cms_menu_structure,
	mod_id integer REFERENCES cms_modules,
	ord int NOT NULL,
	tree_order text
);
CREATE UNIQUE INDEX cms_menu_structure_unique_tree_order ON cms_menu_structure (ord, coalesce(parent, 0));

CREATE OR REPLACE FUNCTION cms_menu_structure_tree_order(menu_item_id int) RETURNS text AS $$
  SELECT
    CASE WHEN $1 IS NULL THEN '' ELSE
      (SELECT cms_menu_structure_tree_order(parent) || '.' || to_char(coalesce(ord, 999999), 'FM000000')
       FROM cms_menu_structure where menu_item_id=$1)
    END
  AS RESULT
$$ LANGUAGE SQL;

-------------------------------------------------------------------------------

CREATE TABLE cms_menu_texts (
       menu_item_id integer NOT NULL REFERENCES cms_menu_structure ON DELETE CASCADE,
       lang char(2) NOT NULL REFERENCES cms_languages(lang),
       published boolean NOT NULL DEFAULT 'TRUE',
       title text NOT NULL,
       description text,
       content text,
       PRIMARY KEY (menu_item_id, lang)
);

CREATE OR REPLACE VIEW cms_menu AS 
SELECT s.menu_item_id ||'.'|| l.lang as menu_id, 
       s.menu_item_id, l.lang, s.identifier, s.parent, s.mod_id, s.ord, s.tree_order, m.modname,
       coalesce(t.published, 'FALSE') as published,
       coalesce(t.title, s.identifier) as title_or_identifier, 
       t.title, t.description, t.content
FROM cms_menu_structure s CROSS JOIN cms_languages l
     LEFT OUTER JOIN cms_menu_texts t USING (menu_item_id, lang)
     LEFT OUTER JOIN cms_modules m USING (mod_id);

CREATE OR REPLACE RULE cms_menu_insert AS
  ON INSERT TO cms_menu DO INSTEAD (
     INSERT INTO cms_menu_structure (identifier, parent, mod_id, ord)
     VALUES (new.identifier, new.parent, new.mod_id,
             coalesce(new.ord, (SELECT max(ord)+100 FROM cms_menu_structure 
                                WHERE coalesce(parent, 0)=coalesce(new.parent, 0)), 100));
     UPDATE cms_menu_structure SET tree_order = cms_menu_structure_tree_order(menu_item_id);
     INSERT INTO cms_menu_texts (menu_item_id, lang, published, title, description, content)
     SELECT (SELECT menu_item_id FROM cms_menu_structure WHERE identifier=new.identifier),
            new.lang, new.published, new.title, new.description, new.content
     RETURNING 
       menu_item_id ||'.'|| lang, menu_item_id, lang, 
       NULL::varchar(32), NULL::int, NULL::int, NULL::int, NULL::text, NULL::varchar(64),
       published, title, title, description, content
);

CREATE OR REPLACE RULE cms_menu_update AS
  ON UPDATE TO cms_menu DO INSTEAD (
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
);

CREATE OR REPLACE RULE cms_menu_delete AS
  ON DELETE TO cms_menu DO INSTEAD (
     DELETE FROM cms_menu_structure WHERE menu_item_id = old.menu_item_id;
);
