# -*- coding: utf-8

import sqlalchemy
import pytis.extensions.gensqlalchemy as sql
import pytis.data
import dbdefs as db

class CmsLanguages(sql.SQLTable):
    """Codebook of languages available in the CMS."""
    name = 'cms_languages'
    fields = (
              sql.PrimaryColumn('lang_id', pytis.data.Serial(not_null=False)),
              sql.Column('lang', pytis.data.String(minlen=2, maxlen=2, not_null=True), unique=True),
             )
    with_oids = True
    depends_on = ()
    access_rights = db.cms_rights.value(globals())

class CmsModules(sql.SQLTable):
    """Codebook of extension modules available in the CMS."""
    name = 'cms_modules'
    fields = (
              sql.PrimaryColumn('mod_id', pytis.data.Serial(not_null=False)),
              sql.Column('modname', pytis.data.String(maxlen=64, not_null=True), unique=True),
             )
    with_oids = True
    depends_on = ()
    access_rights = db.cms_rights.value(globals())

class CmsMenuStructure(sql.SQLTable):
    """Language independent menu structure."""
    name = 'cms_menu_structure'
    fields = (
              sql.PrimaryColumn('menu_item_id', pytis.data.Serial(not_null=False)),
              sql.Column('identifier', pytis.data.String(maxlen=32, not_null=True), unique=True),
              sql.Column('parent', pytis.data.Integer(not_null=False), references=sql.gA('cms_menu_structure')),
              sql.Column('mod_id', pytis.data.Integer(not_null=False), references=sql.gA('cms_modules')),
              sql.Column('ord', pytis.data.Integer(not_null=True)),
              sql.Column('tree_order', pytis.data.LTree(not_null=False)),
             )
    with_oids = True
    depends_on = (CmsModules,)
    access_rights = db.cms_rights.value(globals())

class CmsMenuStructureUniqueTreeOrder(sql.SQLRaw):
    name = 'cms_menu_structure_unique_tree_order'
    @classmethod
    def sql(class_):
        return """CREATE UNIQUE INDEX cms_menu_structure_unique_tree_order ON cms_menu_structure (ord, coalesce(parent, 0));"""
    depends_on = (CmsMenuStructure,)

class CmsMenuStructureTreeOrder(sql.SQLFunction):
    """Generate a sortable string representing the hierarchical position of given menu item."""
    name = 'cms_menu_structure_tree_order'
    arguments = (sql.Column('', pytis.data.Integer()),)
    result_type = pytis.data.LTree()
    multirow = False
    stability = 'VOLATILE'
    depends_on = (CmsMenuStructure,)
    access_rights = ()

    def body(self):
        return """
           select case when parent is null then text2ltree('')
                       else cms_menu_structure_tree_order(parent)
                  end || to_char(coalesce(ord, 999999), 'FM000000')::text as result
           from cms_menu_structure where menu_item_id=$1"""

class CmsMenuTexts(sql.SQLTable):
    """Language dependent texts and properties for menu items."""
    name = 'cms_menu_texts'
    fields = (
              sql.Column('menu_item_id', pytis.data.Integer(not_null=True), references=sql.gA('cms_menu_structure', ondelete='CASCADE')),
              sql.Column('lang', pytis.data.String(minlen=2, maxlen=2, not_null=True), references=sql.gA('cms_languages(lang)', ondelete='CASCADE')),
              sql.Column('published', pytis.data.Boolean(not_null=True), default='TRUE'),
              sql.Column('title', pytis.data.String(not_null=True)),
              sql.Column('heading', pytis.data.String(not_null=False)),
              sql.Column('description', pytis.data.String(not_null=False)),
              sql.Column('content', pytis.data.String(not_null=False)),
             )
    with_oids = True
    depends_on = (CmsMenuStructure, CmsLanguages,)
    access_rights = db.cms_rights.value(globals())

class CmsMenu(sql.SQLView):
    """Complete menu structure with texts for each language defined in cms_languages."""
    name = 'cms_menu'
    @classmethod
    def query(cls):
        s = sql.t.CmsMenuStructure.alias('s')
        l = sql.t.CmsLanguages.alias('l')
        t_ = sql.t.CmsMenuTexts.alias('t')
        m = sql.t.CmsModules.alias('m')
        return sqlalchemy.select(
            cls._exclude(s) +
            cls._exclude(l, 'lang_id') +
            cls._exclude(t_, 'menu_item_id', 'lang', 'published') +
            cls._exclude(m, 'mod_id') +
            [sql.gL("s.menu_item_id ||'.'|| l.lang").label('menu_id'),
             sql.gL("coalesce(t.published, 'FALSE')").label('published'),
             sql.gL("coalesce(t.title, s.identifier)").label('title_or_identifier'),
             sql.gL("(select count(*)-1 from cms_menu_structure where tree_order <@ s.tree_order)").label('tree_order_nsub')],
            from_obj=[s.join(l, sqlalchemy.sql.true()).outerjoin(t_, sql.gR('t.menu_item_id = s.menu_item_id AND t.lang = l.lang')).outerjoin(m, sql.gR('m.mod_id = s.mod_id'))]
            )

    def on_insert(self):
        return ("""(
       INSERT INTO cms_menu_structure (identifier, parent, mod_id, ord)
       VALUES (new.identifier, new.parent, new.mod_id,
               coalesce(new.ord, (SELECT max(ord)+100 FROM cms_menu_structure 
                                  WHERE coalesce(parent, 0)=coalesce(new.parent, 0)), 100));
       UPDATE cms_menu_structure SET tree_order = cms_menu_structure_tree_order(menu_item_id);
       INSERT INTO cms_menu_texts (menu_item_id, lang, published,
                                   title, heading, description, content)
       SELECT (SELECT menu_item_id FROM cms_menu_structure WHERE identifier=new.identifier),
              new.lang, new.published, new.title, new.heading, new.description, new.content
       RETURNING 
          menu_item_id, NULL::varchar(32), NULL::int, NULL::int, NULL::int, NULL::ltree, 
          lang, title, heading, description, content, NULL::varchar(64), 
          menu_item_id ||'.'|| lang, published, title, 0::bigint
       )""",)
    def on_update(self):
        return ("""(
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
         heading = new.heading,
         description = new.description,
         content = new.content
       WHERE menu_item_id = old.menu_item_id AND lang = new.lang;
       INSERT INTO cms_menu_texts (menu_item_id, lang, published,
                                   title, heading, description, content)
         SELECT old.menu_item_id, new.lang, new.published,
                new.title, new.heading, new.description, new.content
         WHERE new.lang NOT IN (SELECT lang FROM cms_menu_texts WHERE menu_item_id=old.menu_item_id)
                AND coalesce(new.title, new.heading, new.description, new.content) IS NOT NULL;
       )""",)
    def on_delete(self):
        return ("(DELETE FROM cms_menu_structure WHERE menu_item_id = old.menu_item_id;)",)
    depends_on = (CmsMenuStructure, CmsLanguages, CmsMenuTexts, CmsModules,)
    access_rights = db.cms_rights.value(globals())

class CmsRoles(sql.SQLTable):
    """CMS roles."""
    name = 'cms_roles'
    fields = (
              sql.PrimaryColumn('role_id', pytis.data.Serial(not_null=False)),
              sql.Column('name', pytis.data.String(not_null=True)),
              sql.Column('system_role', pytis.data.String(not_null=False), unique=True),
              sql.Column('description', pytis.data.String(not_null=False)),
             )
    with_oids = True
    depends_on = ()
    access_rights = db.cms_rights.value(globals())

class CmsActions(sql.SQLTable):
    """Enumeration of valid actions (including both module independent actions and per module actions).  Module independent actions have NULL in the mod_id column."""
    name = 'cms_actions'
    fields = (
              sql.PrimaryColumn('action_id', pytis.data.Serial(not_null=False)),
              sql.Column('mod_id', pytis.data.Integer(not_null=False), references=sql.gA('cms_modules', ondelete='CASCADE')),
              sql.Column('name', pytis.data.String(maxlen=16, not_null=True)),
              sql.Column('description', pytis.data.String(not_null=True)),
             )
    with_oids = True
    unique = (('mod_id', 'name',),)
    depends_on = (CmsModules,)
    access_rights = db.cms_rights.value(globals())

class CmsRightsAssignment(sql.SQLTable):
    """Underlying binding table between menu items, roles and module actions."""
    name = 'cms_rights_assignment'
    fields = (
              sql.PrimaryColumn('rights_assignment_id', pytis.data.Serial(not_null=False)),
              sql.Column('menu_item_id', pytis.data.Integer(not_null=True), references=sql.gA('cms_menu_structure', ondelete='CASCADE')),
              sql.Column('role_id', pytis.data.Integer(not_null=True), references=sql.gA('cms_roles', ondelete='CASCADE')),
              sql.Column('action_id', pytis.data.Integer(not_null=True), references=sql.gA('cms_actions', ondelete='CASCADE')),
             )
    with_oids = True
    unique = (('menu_item_id', 'role_id', 'action_id',),)
    depends_on = (CmsMenuStructure, CmsRoles, CmsActions,)
    access_rights = db.cms_rights.value(globals())

class CmsRights(sql.SQLView):
    """User editable access rights assignment."""
    name = 'cms_rights'
    @classmethod
    def query(cls):
        x = sql.t.CmsRightsAssignment.alias('x')
        s = sql.t.CmsMenuStructure.alias('s')
        r_ = sql.t.CmsRoles.alias('r')
        a_ = sql.t.CmsActions.alias('a')
        return sqlalchemy.select(
            cls._exclude(x) +
            [r_.c.name.label('role_name'),
             s.c.mod_id.label('mod_id'),
             r_.c.description.label('role_description'),
             r_.c.system_role.label('system_role'),
             a_.c.name.label('action_name'),
             a_.c.description.label('action_description')],
            from_obj=[x.join(s, sql.gR('s.menu_item_id=x.menu_item_id')).join(r_, sql.gR('r.role_id = x.role_id')).join(a_, sql.gR('a.action_id = x.action_id'))]
            )

    insert_order = (CmsRightsAssignment,)
    update_order = (CmsRightsAssignment,)
    delete_order = (CmsRightsAssignment,)
    depends_on = (CmsRightsAssignment, CmsMenuStructure, CmsRoles, CmsActions,)
    access_rights = db.cms_rights.value(globals())

class CmsThemes(sql.SQLTable):
    """Definition of available color themes."""
    name = 'cms_themes'
    fields = (
              sql.PrimaryColumn('theme_id', pytis.data.Serial(not_null=False)),
              sql.Column('name', pytis.data.String(not_null=True), unique=True),
              sql.Column('foreground', pytis.data.Color(not_null=False)),
              sql.Column('background', pytis.data.Color(not_null=False)),
              sql.Column('border', pytis.data.Color(not_null=False)),
              sql.Column('heading_fg', pytis.data.Color(not_null=False)),
              sql.Column('heading_bg', pytis.data.Color(not_null=False)),
              sql.Column('heading_line', pytis.data.Color(not_null=False)),
              sql.Column('frame_fg', pytis.data.Color(not_null=False)),
              sql.Column('frame_bg', pytis.data.Color(not_null=False)),
              sql.Column('frame_border', pytis.data.Color(not_null=False)),
              sql.Column('link', pytis.data.Color(not_null=False)),
              sql.Column('link_visited', pytis.data.Color(not_null=False)),
              sql.Column('link_hover', pytis.data.Color(not_null=False)),
              sql.Column('meta_fg', pytis.data.Color(not_null=False)),
              sql.Column('meta_bg', pytis.data.Color(not_null=False)),
              sql.Column('help', pytis.data.Color(not_null=False)),
              sql.Column('error_fg', pytis.data.Color(not_null=False)),
              sql.Column('error_bg', pytis.data.Color(not_null=False)),
              sql.Column('error_border', pytis.data.Color(not_null=False)),
              sql.Column('message_fg', pytis.data.Color(not_null=False)),
              sql.Column('message_bg', pytis.data.Color(not_null=False)),
              sql.Column('message_border', pytis.data.Color(not_null=False)),
              sql.Column('table_cell', pytis.data.Color(not_null=False)),
              sql.Column('table_cell2', pytis.data.Color(not_null=False)),
              sql.Column('top_fg', pytis.data.Color(not_null=False)),
              sql.Column('top_bg', pytis.data.Color(not_null=False)),
              sql.Column('top_border', pytis.data.Color(not_null=False)),
              sql.Column('highlight_bg', pytis.data.Color(not_null=False)),
              sql.Column('inactive_folder', pytis.data.Color(not_null=False)),
             )
    with_oids = True
    depends_on = ()
    access_rights = db.cms_rights.value(globals())

