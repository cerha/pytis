# -*- coding: utf-8 -*-

# Copyright (C) 2009, 2010 Brailcom, o.p.s.
#
# COPYRIGHT NOTICE
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

"""Objects in this file facilitate work with dynamic menus and permissions (\"DMP\").

Basic requirements on their functionality are:

- Reading data from specifications.

- Reading data from database DMP tables.

- Comparing data retrieved from different sources, including some sort of
  non-trivial comparison such as detecting identical menu items at different
  places within the menu.  Strictness of the comparison is adjustable.
  
- Writing updated data to database DMP tables.

- Generating SQL commands for updates of database DMP tables.

- Outputting Python specification source code corresponding to updated data.

Overall intended usages are:

- Importing DMP database tables from application specifications.

- Updating certain application specifications (such as application menu
  specification) from DMP tables.

- Detecting differences between application specifications and DMP tables and
  suggesting updates to DMP tables.  Especially updating DMP tables after
  adding new menu items, specifications, subform bindings, form actions, etc.
  This process may be performed globally or only on selected specifications.

- Using utility functions such as manipulation with DMP fullnames and
  shortnames or common command line processing for scripts using this library.

We are interested in the following kinds of data:

- Application menu.

- Forms (either present in the application menu or not) and their bindings and
  actions.

- Access rights.

We use proprietary classes for storing, examining and manipulating given kinds
of data instead of using standard available pytis classes.  The reason is that
for first there are attributes not present in the standard pytis classes such
as database rows numeric identifiers and for second we need simple and
intelligible data structures and operations just serving the purpose of
implementing and maintaining the required functionality easily.  But if pytis
classes provide useful functionality for DMP we use the functionality.

"""

import copy
import string

import pytis.data
import pytis.form
from pytis.util import *


class DMPMessage(Structure):
    """Message about DMP operation to be presented to the user."""
    ERROR_MESSAGE = 'Error'
    WARNING_MESSAGE = 'Warning'
    NOTE_MESSAGE = 'Note'
    SQL_MESSAGE = 'SQL'
    
    _attributes = (Attribute('kind', str),
                   Attribute('message', basestring),
                   Attribute('arguments', tuple, default=()),
                   )

    def format(self):
        """Return formatted message for the output as a basestring."""
        formatted = '%s: %s' % (self.kind(), self.message(),)
        if self.arguments():
            formatted = '%s: %s' % (formatted, string.join(self.arguments(), ', '),)
        return formatted


def add_message(messages, kind, message, arguments=()):
    """Add new message to 'messages'.

    Arguments:

      messages -- list of 'DMPMessage' instances
      kind -- kind of the message, one of 'DMPMessage' constants
      message -- the message itself, basestring
      arguments -- tuple of message arguments
      
    """
    m = DMPMessage(kind=kind, message=message, arguments=tuple([unicode(a) for a in arguments]))
    messages.append(m.format())


class DMPConfiguration(object):
    """Storage of various runtime objects for DMP processing."""

    def __init__(self, def_directory=None, schemas=None,
                 database=None, host=None, port=None, user=None, password=None, sslmode=None):
        """
        Arguments:

          def_directory -- directory containing application specifications,
            string or 'None'
          schemas -- schemas string (schema names separated by commas) or 'None'
          database, host, port, user, password, sslmode -- common database
            connection parameters
            
        """
        if def_directory is None:
            self._resolver = None
        else:
            self._resolver = FileResolver(def_directory)
        connection_parameters = {}
        for argument in ('database', 'host', 'port', 'user', 'password', 'sslmode',):
            if locals().has_key(argument):
                connection_parameters[argument] = locals()[argument]
        if schemas:
            schemas_list = [s.strip() for s in schemas_string.split(',')]
        else:
            schemas_list = None
        self._connection_data = pytis.data.DBConnection(schemas=schemas_list, **connection_parameters)

    def resolver(self):
        """Return specifications resolver instance."""
        return self._resolver

    def connection_data(self):
        """Return 'DBConnection' instance for access to the database."""
        return self._connection_data
        

class DMPItem(Structure):
    """Representation of single DMP items in DMP objects."""
    
    def signature(self):
        """Return hash of the given item for object comparison.

        Each two equal or semi-equal items must return the same hash.  Each two
        non-equal may or may not return the same hash.

        The return value may be an arbitrary immutable object.
        
        """
        return getattr(self, self._attributes[0].name())

    def equal(self, other):
        """Return whether 'self' is equal with 'other'.

        If the two items are completely semanticaly equal, return 'True'.  If
        they belong to the same item but they are not completely semanticaly
        equal (they are \"semi-equal\", e.g. two corresponding menu items with
        different title), return 'None'.  If they are non-equal (i.e. they are
        two basically unrelated items), return 'False'.

        Arguments:

          other -- instance of the same class as 'self'
          
        """
        for attribute in self._attributes:
            name = attribute.name()
            if name == 'id':
                continue
            if getattr(self, name) != getattr(other, name):
                return False
        return True

    
class DMPObject(object):
    """Base class of DMP data classes.

    It defines common abstract methods of those data classes.
    
    """
    _DB_TABLES = {'e_pytis_disabled_dmp_triggers': ('id',)}

    class Logger(object):
        def __init__(self):
            self._messages = []
        def write(self, message):
            self.append('SQL: %s' % (message,))
        def append(self, message):
            self._messages.append(message)            
        def clear(self):
            self._messages = []
        def messages(self):
            return copy.copy(self._messages)
    
    def __init__(self, configuration):
        """
        Arguments:

          configuration -- 'DMPConfiguration' instance

        """
        self._configuration = configuration
        self._logger = self.Logger()
        self._reset()
        
    def _reset(self):
        pass

    def _resolver(self):
        return self._configuration.resolver()

    def _data(self, table):
        data = pytis.data.dbtable(table, self._DB_TABLES[table],
                                  self._configuration.connection_data(),
                                  sql_logger=self._logger)
        self._logger.clear()
        return data

    def _dbfunction(self, function):
        dbfunction = pytis.data.DBFunctionDefault(function, self._configuration.connection_data(),
                                                  sql_logger=self._logger)
        self._logger.clear()
        return dbfunction

    def _transaction(self):
        return pytis.data.DBTransactionDefault(self._configuration.connection_data())

    def _b_(self, value):
        return pytis.data.Value(pytis.data.Boolean(), value)
    
    def _i_(self, value):
        return pytis.data.Value(pytis.data.Integer(), value)
    
    def _s_(self, value):
        return pytis.data.Value(pytis.data.String(), value)

    def _all_form_specification_names(self, messages):
        specification_names = []
        resolver = self._resolver()
        import config
        def_dir = config.def_dir
        def_dir_len = len(def_dir.split('/'))
        for root, dirs, files in os.walk(def_dir):
            relative_root_path = root.split('/')[def_dir_len:]
            if relative_root_path:
                relative_root = os.path.join(*relative_root_path) + '/'
            else:
                relative_root = ''
            for f in files:
                if f.endswith('.py'):
                    module_name = relative_root + f[:-3]
                    try:
                        module = resolver.get_module(module_name)
                    except pytis.util.ResolverFileError:
                        add_message(messages, DMPMessage.WARNING_MESSAGE, "Module not loaded", (module,))
                        continue
                    module_identifier = module_name.replace('/', '.')
                    for spec_attr in [o for o in dir(module)]:
                        spec = getattr(module, spec_attr)
                        if isinstance(spec, type) and issubclass(spec, pytis.form.Specification) and spec.public:
                            if spec_attr[0] == '_':
                                add_message(messages, DMPMessage.WARNING_MESSAGE,
                                            "Public specification starting with underscore",
                                            ('%s.%s' % (module_identifier, spec_attr,),))
                            spec_name = module_identifier + '.' + spec.__name__
                            specification_names.append(spec_name)
                        elif (isinstance(spec, type) and
                              issubclass(spec, pytis.form.Specification) and
                              spec_attr != 'Specification'):
                            add_message(messages, DMPMessage.NOTE_MESSAGE,
                                        "Private specification, ignored",
                                        ('%s.%s' % (module_identifier, spec_attr,),))
        return specification_names

    def _specification(self, name, messages):
        resolver = self._resolver()
        pos = name.rfind('.')
        if pos == -1:
            add_message(messages, DMPMessage.NOTE_MESSAGE,
                        "Ignoring specification without module", (name,))
            return None
        module_name = name[:pos].replace('.', '/')
        class_name = name[pos+1:]
        try:
            spec = resolver.get_object(module_name, class_name)
            spec_instance = spec(resolver)
        except Exception, e:
            add_message(messages, DMPMessage.ERROR_MESSAGE,
                        "Couldn't load specification", (name, e,))
            return None
        return spec_instance

    def _disable_triggers(self, transaction=None):
        data = self._data('e_pytis_disabled_dmp_triggers')
        data.insert(pytis.data.Row((('id', self._s_('genmenu'),),)), transaction=transaction)

    def _enable_triggers(self, transaction=None):
        data = self._data('e_pytis_disabled_dmp_triggers')
        data.delete_many(pytis.data.EQ('id', self._s_('genmenu')), transaction=transaction)

    def items(self):
        """Return sequence of all data items registered in the instance."""
        return []
    
    def load_specifications(self, **kwargs):
        """Load DMP data from specifications.

        Return sequence of 'DMPMessage' instances.

        Arguments:

          kwargs -- DMP object specific arguments

        """
        self._reset()
        return self._load_specifications(**kwargs)
    
    def retrieve_data(self):
        """Load DMP data from the database."""
        self._reset()
        self._retrieve_data()

    def _retrieve_data(self):
        pass
    
    def store_data(self, fake, transaction=None):
        """Store DMP data into the database.

        Arguments:

          fake -- iff True, don't actually store the data but return sequence
            of SQL commands (basestrings) that would do so
          transaction -- transaction object to use or 'None'; if not 'None' no
            commit nor rollback is performed in this method regardless 'fake'
            argument value

        """
        if transaction is None:
            transaction_ = self._transaction()
        else:
            transaction_ = transaction
        success = self._store_data(transaction_)
        messages = self._logger.messages()
        if transaction is None:
            if success and not fake:
                transaction_.commit()
            else:
                transaction_.rollback()
        return messages

    def _store_data(self, transaction):
        return False

    def delete_data(self, fake, transaction=None):
        """Delete DMP data from the database.

          fake -- iff True, don't actually delete the data but return sequence
            of SQL commands (basestrings) that would do so
          transaction -- transaction object to use or 'None'; if not 'None' no
            commit nor rollback is performed in this method regardless 'fake'
            argument value

        """
        if transaction is None:
            transaction_ = self._transaction()
        else:
            transaction_ = transaction
        success = self._delete_data(transaction_)
        messages = self._logger.messages()
        if transaction is None:
            if success and not fake:
                transaction_.commit()
            else:
                transaction_.rollback()
        return messages
    
    def dump_specifications(self, stream):
        raise Exception('Not implemented')

    def compare(self, other):
        """Compare DMP data in 'self' and 'other'.

        Return sequence of pairs (SELF_ITEM, OTHER_ITEM) where SELF_ITEM is
        'None' in case OTHER_ITEM is not present in 'self' at all, OTHER_ITEM
        is 'None' in case SELF_ITEM is not present in 'other' at all.  If both
        SELF_ITEM and OTHER_ITEM are not 'None', they represent a pair of
        semi-equal items in 'self' and 'other'.

        Arguments:

          other -- another instance of the same class

        """
        other_dictionary = {}
        for item in other.items():
            signature = item.signature()
            other_dictionary[signature] = other_dictionary.get(signature, []) + [item]
        differences = []
        for item in self.items():
            signature = item.signature()
            for other_item in other_dictionary.get(signature, []):
                equal = item.equal(other_item)
                if equal is True:
                    other_dictionary[signature].remove(other_item)
                    break
                elif equal is None:
                    differences.append((item, other_item,))
                    break
            else:
                differences.append((item, None,))
        for item_list in other_dictionary.values():
            for item in item_list:
                differences.append((None, item,))
        return differences

    
class DMPMenu(DMPObject):
    """Representation of an application menu.

    Each menu item is represented by the following properties:

    - Kind: action item, nested menu, separator.

    - Title (doesn't apply to separators).

    - Position within the menu.

    - Children (applies only to nested menus).

    - Action (applies only to action items).

    This class represents the whole menu, single menu items are represented by
    'MenuItem' instances.

    """
    class MenuItem(DMPItem):
        ACTION_ITEM = 'ACTION_ITEM'
        MENU_ITEM = 'MENU_ITEM'
        SEPARATOR_ITEM = 'SEPARATOR_ITEM'
        _attributes = (Attribute('id', int),
                       Attribute('kind', str),
                       Attribute('title', unicode),
                       Attribute('parent'),
                       Attribute('children', list, mutable=True),
                       Attribute('action', str),
                       Attribute('position', str, mutable=True),
                       Attribute('hotkey', str),
                       Attribute('help', unicode),
                       Attribute('locked', bool, mutable=True),
                       )
        def signature(self):
            return self.action
        def equal(self, other):
            equal = DMPItem.equal(self, other)
            if equal is False and self.signature() == other.signature():
                equal = None
            return equal

    _DB_TABLES = dict(DMPObject._DB_TABLES.items() +
                      [('e_pytis_menu', ('menuid', 'name', 'title', 'position', 'next_position', 'fullname', 'help', 'hotkey', 'locked'),)])

    def __init__(self, *args, **kwargs):
        super(DMPMenu, self).__init__(*args, **kwargs)
        self._counter = Counter()

    def _reset(self):
        self._menu = []
        self._top_item = None
        
    def items(self):
        return self._menu

    def add_item(self, kind, parent, title=None, action=None, position=None, hotkey=None, help=None):
        id_ = self._counter.next()
        if title is not None:
            title = unicode(title)
        if help is not None:
            help = unicode(help)
        menu_item = self.MenuItem(id=-id_, kind=kind, title=title,
                                  parent=parent, children=[],
                                  action=action, position=position,
                                  hotkey=hotkey, help=help)
        self._menu.append(menu_item)
        if parent is not None:
            parent.children().append(menu_item)
        return menu_item

    def _load_specifications(self):
        self._top_item = self.add_item(kind=self.MenuItem.MENU_ITEM, parent=None, title=u"CELÉ MENU", position='2')
        menu = self._resolver().get('application', 'menu')
        messages = []
        # Load menu
        def load(menu, parent):
            if isinstance(menu, pytis.form.Menu):
                item = self.add_item(kind=self.MenuItem.MENU_ITEM, parent=parent, title=menu.title())
                load(menu.items(), item)
            elif isinstance(menu, pytis.form.MSeparator):
                self.add_item(kind=self.MenuItem.SEPARATOR_ITEM, parent=parent)
            elif isinstance(menu, pytis.form.MItem):
                action_id = menu.action_id()
                if action_id is None:
                    add_message(messages, DMPMessage.ERROR_MESSAGE,
                                "Special menu item action, define command specification", (menu.title(),))
                    return
                hotkey_spec = string.join([(key or '').replace(' ', 'SPC') for key in menu.hotkey()], ' ')
                self.add_item(self.MenuItem.ACTION_ITEM, parent, title=menu.title(), action=action_id,
                              help=menu.help(), hotkey=hotkey_spec)
            elif isinstance(menu, tuple):
                for m in menu:
                    load(m, parent)
            else:
                add_message(messages, DMPMessage.ERROR_MESSAGE, "Unknown menu item", (menu,))
        load(menu, self._top_item)
        # Lock first submenu
        if self._top_item is not None:
            if self._top_item.locked() is None:
                self._top_item.set_locked(True)
            if self._top_item.children():
                item_list = [self._top_item.children()[0]]
                while item_list:
                    item = item_list.pop(0)
                    if item.locked() is None:
                        item.set_locked(True)
                    item_list += item.children()
        for item in self.items():
            if item.locked() is None:
                item.set_locked(False)
        # Assign positions
        def assign(item_list, position):
            i = 1111
            for item in item_list:
                item.set_position('%s.%s' % (position, i,))
                i = i + 1
                assign(item.children(), item.position())
        assign(self._top_item.children(), self._top_item.position())
        return messages        
    
    def _retrieve_data(self, transaction):
        data = self._data('e_pytis_menu')
        items_by_position = {}
        children_by_position = {}
        def process(row):
            position = row['position'].value()
            item = self.MenuItem(id=row['menuid'].value(),
                                 name=row['name'].value(),
                                 title=row['title'].value(),
                                 action=row['fullname'].value(),
                                 position=position,
                                 help=row['help'].value(),
                                 hotkey=row['hotkey'].value(),
                                 locked=row['locked'].value(),
                              )
            items_by_position[position] = item
            pos = position.rfind('.')
            if pos >= 0:
                parent_position = position[:pos]
                children_by_position[parent_position] = \
                  children_by_position.get(parent_position, []) + [item]
            return item
        self._menu = data.select_map(process)
        # Assign parents and children, find top item
        for item in self._menu:
            position = item.position()
            pos = position.rfind('.')
            if pos >= 0:
                parent_position = position[:pos]
                item.parent = items_by_position[parent_position]
            else:
                if self._top_item is None or item.position() < self._top_item.position():
                    self._top_item = item
            item.set_children(children_by_position.get(position, []))
    
    def _store_data(self, transaction):
        data = self._data('e_pytis_menu')
        B = self._b_
        I = self._i_
        S = self._s_
        for item in self.items():
            row = pytis.data.Row((('menuid', I(item.id()),),
                                  ('name', S(item.action()),),
                                  ('title', S(item.title()),),
                                  ('fullname', S(item.action()),),
                                  ('position', S(item.position()),),
                                  ('next_position', S(item.position() + '4'),),
                                  ('help', S(item.help()),),
                                  ('hotkey', S(item.hotkey()),),
                                  ('locked', B(item.locked()),),
                                  ))
            _, result = data.insert(row, transaction=transaction)
            if not result:
                return False
        return True
    
    def _delete_data(self, transaction):
        data = self._data('e_pytis_menu')
        data.delete_many(pytis.data.AND(), transaction=transaction)


class DMPRights(DMPObject):

    class Right(DMPItem):
        _attributes = (Attribute('shortname', str),
                       Attribute('roleid', str),
                       Attribute('rightid', str),
                       Attribute('colname', str),
                       Attribute('system', bool),
                       Attribute('granted', bool),
                       )

    _DB_TABLES = dict(DMPObject._DB_TABLES.items() +
                      [('e_pytis_action_rights', ('shortname', 'roleid', 'rightid', 'system', 'granted', 'colname', 'status',),)])

    def _reset(self):
        self._rights = []
        
    def items(self):
        return self._rights

    def _load_specifications(self):
        messages = []
        def add_rights(shortname, access_specification):
            for item in access_specification:
                column = item[0]
                for groups_permissions in item[1:]:
                    groups = groups_permissions[0]
                    permissions = groups_permissions[1:]
                    if pytis.data.Permission.ALL in permissions:
                        permissions = pytis.data.Permission.all_permissions()
                    else:
                        permissions = remove_duplicates(list(permissions))
                    if not is_sequence(groups):
                        groups = (groups,)
                    for g in groups:
                        for p in permissions:
                            self._rights.append(self.Right(shortname=shortname,
                                                           roleid=(g or '*'),
                                                           rightid=p.lower(),
                                                           colname=column,
                                                           system=True,
                                                           granted=True))            
        for spec_name in self._all_form_specification_names(messages):
            # Form access rights
            shortname = 'form/' + spec_name
            spec = self._specification(spec_name, messages)
            if spec is None:
                continue
            access_rights = spec.data_spec().access_rights()
            if access_rights is None:
                add_message(messages, DMPMessage.NOTE_MESSAGE,
                            "No access rights specified for form, assuming everything permitted",
                            (form_name,))
                access_specification = ((None, (None, pytis.data.Permission.ALL)),)
            else:
                access_specification = access_rights.specification()
            add_rights(shortname, access_specification)
            # Form actions access rights
            form_actions = spec.view_spec().actions()
            if form_actions:
                for a in form_actions:
                    form_action_name = 'action/%s/%s' % (a.id(), spec_name,)
                    form_action_rights = ((None, (a.access_groups(), pytis.data.Permission.CALL)),)
                    add_rights(form_action_name, form_action_rights)
        return messages

    def _retrieve_data(self, connection):
        data = self._data('e_pytis_action_rights')
        condition = pytis.data.LE('status', self._i_(0))
        def process(row):
            return self.Right(shortname=row['shortname'].value(),
                              roleid=row['roleid'].value(),
                              rightid=row['rightid'].value(),
                              colname=row['colname'].value(),
                              system=row['system'].value(),
                              granted=row['granted'].value(),
                              )
        self._rights = data.select_map(process, condition=condition)
    
    def _store_data(self, transaction):
        data = self._data('e_pytis_action_rights')
        B = self._b_
        I = self._i_
        S = self._s_
        for right in self.items():
            row = pytis.data.Row((('shortname', S(right.shortname()),),
                                  ('roleid', S(right.roleid()),),
                                  ('rightid', S(right.rightid()),),
                                  ('colname', S(right.colname()),),
                                  ('system', B(right.system()),),
                                  ('granted', B(right.granted(),),),
                                  ('status', I(0),),
                                  ))
            _, result = data.insert(row, transaction=transaction)
            if not result:
                return False
        return True

    def _delete_data(self, transaction):
        data = self._data('e_pytis_action_rights')
        data.delete_many(pytis.data.AND(), transaction=transaction)        


class DMPRoles(DMPObject):
    
    class Role(DMPItem):
        _attributes = (Attribute('name', str),
                       Attribute('description', basestring),
                       Attribute('purposeid', str),
                       )
        def signature(self):
            return self.name
        def equal(self, other):
            equal = DMPItem.equal(self, other)
            if equal is False and self.name == other.name:
                equal = None
            return equal

    _DB_TABLES = dict(DMPObject._DB_TABLES.items() +
                      [('e_pytis_roles', ('name', 'description', 'purposeid',),),
                       ('e_pytis_role_members', ('roleid', 'member',),)])

    def _reset(self):
        self._roles = []
        
    def items(self):
        return self._roles

    def _load_specifications(self, dmp_rights):
        roles = self._roles
        for right in dmp_rights.items():
            if right.roleid() not in roles:
                roles.append(self.Role(name=right.roleid(), description='', purposeid='appl'))
        return []

    def _retrieve_data(self, connection):
        data = self._data('e_pytis_roles')
        def process(row):
            return self.Role(name=row['name'].value(),
                             description=row['description'].value(),
                             purposeid=row['purposeid'].value(),
                             )
        self._roles = data.select_map(process)
    
    def _store_data(self, transaction):
        data = self._data('e_pytis_roles')
        S = self._s_
        for role in self.items():
            row = pytis.data.Row((('name', S(role.name()),),
                                  ('description', S(role.description()),),
                                  ('purposeid', S(role.purposeid()),),
                                  ))
            _, result = data.insert(row, transaction=transaction)
            if not result:
                return False
        return True

    def _delete_data(self, transaction):
        data = self._data('e_pytis_role_members')
        data.delete_many(pytis.data.AND(), transaction=transaction)
        data = self._data('e_pytis_roles')
        data.delete_many(pytis.data.AND(), transaction=transaction)


class DMPActions(DMPObject):

    class Action(DMPItem):
        """Representation of an DMP action.

        DMP action is uniquely identified by its fullname.  Other properties are
        derived from the fullname.

        """
        _attributes = (Attribute('fullname', str),
                       Attribute('title', basestring),
                       Attribute('description', basestring),
                       )

        def signature(self):
            return self.fullname

        def equal(self, other):
            equal = DMPItem.equal(self, other)
            if equal is False and self.fullname() == other.fullname():
                equal = None
            return equal

        def shortname(self):
            """Return shortname of the action as a string."""
            components = self.fullname().split('/')
            if components[0] == 'form':
                shortname = 'form/%s' % (components[2],)
            elif components[0] == 'sub':
                shortname = 'form/%s' % (components[4],)
            else:
                shortname = self.fullname()
            return shortname

        def form_name(self):
            """Return name of the specification related to the action.

            For non-form actions return 'None'.
            
            """
            components = self.fullname().split('/')
            if components[0] == 'form':
                name = components[2]
            else:
                name = None
            return name

        def form_class(self):
            """Return class of the action form.

            For non-form actions and dummy form actions return 'None'.
            
            """
            components = self.fullname().split('/')
            if components[0] == 'form' and components[1] != '*':
                class_ = eval(components[1])
            else:
                class_ = None
            return class_
            
        @classmethod
        def dummy_action(class_, shortname):
            components = shortname.split('/')
            if components[0] == 'form':
                fullname = 'form/*/%s' % (components[1],)
            else:
                fullname = shortname
            return class_(fullname=fullname)
        
    _DB_TABLES = dict(DMPObject._DB_TABLES.items() +
                      [('c_pytis_menu_actions', ('fullname', 'shortname', 'action_title', 'description',),)])

    def _reset(self):
        self._actions = []
        self._fullnames = {}
        self._shortnames = {}

    def _add_action(self, action):
        self._actions.append(action)
        self._fullnames[action.fullname()] = action
        shortname = action.shortname()
        self._shortnames[shortname] = self._shortnames.get(shortname, []) + [action]
        
    def items(self):
        return self._actions

    def _load_specifications(self, dmp_menu=None, dmp_rights=None):
        messages = []
        if dmp_menu is not None:
            self._load_from_menu(dmp_menu.items(), messages)
        if dmp_rights is not None:
            self._load_from_rights(dmp_rights.items(), messages)
        return messages

    def _load_from_menu(self, items, messages):
        for menu in items:
            # Create and register DMP action
            fullname = menu.action()
            if fullname is None:
                continue
            action = self.Action(fullname=fullname, title=menu.title())
            self._add_action(action)
            # Retrieve specification
            form_name = action.form_name()
            if form_name is None:
                continue
            spec = self._specification(form_name, messages)
            if spec is None:
                continue
            # Subforms
            form_class = action.form_class()
            def binding(name):
                spec = self._specification(name, messages)
                if spec is None:
                    title = ''
                else:
                    try:
                        title = spec(resolver).view_spec().title()
                    except Exception, e:
                        title = ''
                        add_message(messages, DMPMessage.ERROR_MESSAGE,
                                    "Can't create specification instance to get binding title",
                                    (name, e,))
                return pytis.presentation.Binding(id=name, title=title, name=name,
                                                  binding_column='dummy')
            if issubclass(form_class, pytis.form.DualForm):
                pos = form_name.find('::')
                if pos == -1:
                    bindings = spec.view_spec().bindings()
                    bindings = (binding(form_name),) + tuple(bindings)
                else:
                    bindings = (binding(form_name[:pos]), binding(form_name[pos+2:]),)
                if pytis.util.is_sequence(bindings):
                    for i in range(len(bindings)):
                        b = bindings[i]
                        subaction_fullname = 'sub/%02d/%s' % (i, action.fullname(),)
                        subaction_title = b.title()
                        self._add_action(self.Action(fullname=subaction_fullname, title=subaction_title))
            # Form actions
            form_actions = []
            def add_form_actions(actions):
                for a in actions:
                    if isinstance(a, pytis.presentation.Action):
                        form_actions.append(a)
                    elif isinstance(a, pytis.presentation.ActionGroup):
                        add_form_actions(a.actions())
                    elif pytis.util.is_sequence(a):
                        add_form_actions(a)                                    
                    else:
                        add_message(messages, DMPMessage.ERROR_MESSAGE,
                                    "Unknown form action class", (spec_name, a,))
            add_form_actions(spec.view_spec().actions())
            for a in form_actions:
                fullname = 'action/%s/%s' % (a.id(), form_name,)
                self._add_action(self.Action(fullname=fullname, title=a.title(raw=True)))
        
    def _load_from_rights(self, items, messages):
        for right in items:
            if not self._shortnames.get(right.shortname()):
                self._add_action(self.Action.dummy_action(right.shortname()))

    def _retrieve_data(self, connection):
        data = self._data('c_pytis_menu_actions')
        def process(row):
            return self.Action(fullname=row['fullname'].value(),
                               shortname=row['shortname'].value(),
                               title=row['title'].value(),
                               description=row['description'].value(),
                              )
        self._actions = data.select_map(process)
    
    def _store_data(self, transaction):
        data = self._data('c_pytis_menu_actions')
        S = self._s_
        for action in self.items():
            row = pytis.data.Row((('fullname', S(action.fullname()),),
                                  ('shortname', S(action.shortname()),),
                                  ('title', S(action.title()),),
                                  ('description', S(action.description()),),
                                  ))
            _, result = data.insert(row, transaction=transaction)
            if not result:
                return False
        return True

    def _delete_data(self, transaction):
        data = self._data('c_pytis_menu_actions')
        data.delete_many(pytis.data.AND(), transaction=transaction)


class DMPCommit(DMPObject):
    """This class just performs commits of simple access rights changes."""
    
    def _store_data(self, transaction):
        messages = []
        dbfunction = self._dbfunction('pytis_update_summary_rights')
        dbfunction.call(pytis.data.Row(()), transaction=transaction)
        return messages


class DMPImport(DMPObject):
    """Initial import functionality."""

    def _reset(self):
        configuration = self._configuration
        self._dmp_menu = DMPMenu(configuration)
        self._dmp_rights = DMPRights(configuration)
        self._dmp_roles = DMPRoles(configuration)
        self._dmp_actions = DMPActions(configuration)
        
    def load_specifications(self, **kwargs):
        messages = []
        messages += self._dmp_menu.load_specifications()
        messages += self._dmp_rights.load_specifications()
        messages += self._dmp_roles.load_specifications(dmp_rights=self._dmp_rights)
        messages += self._dmp_actions.load_specifications(dmp_menu=self._dmp_menu, dmp_rights=self._dmp_rights)
        return messages
    
    def delete_data(self, fake, transaction=None):
        if transaction is None:
            transaction_ = self._transaction()
        else:
            transaction_ = transaction
        messages = []
        #import pdb; pdb.set_trace()
        messages += self._dmp_menu.delete_data(fake=fake, transaction=transaction_)
        messages += self._dmp_rights.delete_data(fake=fake, transaction=transaction_)
        messages += self._dmp_roles.delete_data(fake=fake, transaction=transaction_)
        messages += self._dmp_actions.delete_data(fake=fake, transaction=transaction_)
        if transaction is None:
            if fake:
                transaction_.rollback()        
            else:
                transaction_.commit()
        return messages
    
    def store_data(self, fake, transaction=None):
        if transaction is None:
            transaction_ = self._transaction()
        else:
            transaction_ = transaction
        messages = []
        messages += self._dmp_actions.store_data(fake=fake, transaction=transaction_)
        messages += self._dmp_roles.store_data(fake=fake, transaction=transaction_)
        messages += self._dmp_rights.store_data(fake=fake, transaction=transaction_)
        messages += self._dmp_menu.store_data(fake=fake, transaction=transaction_)
        if transaction is None:
            if fake:
                transaction_.rollback()        
            else:
                transaction_.commit()
        return messages

    def dmp_import(self, fake):
        transaction = self._transaction()
        self._disable_triggers(transaction=transaction)
        messages = []
        messages += self.load_specifications()
        messages += self.delete_data(fake, transaction=transaction)
        messages += self.store_data(fake, transaction=transaction)
        self._enable_triggers(transaction=transaction)
        if fake:
            transaction.rollback()
        else:
            transaction.commit()
        return messages


def dmp_import(connection_parameters, fake, def_directory):
    configuration = DMPConfiguration(def_directory=def_directory, **connection_parameters)
    return DMPImport(configuration).dmp_import(fake)

def dmp_commit(connection_parameters, fake):
    configuration = DMPConfiguration(**connection_parameters)
    return DMPCommit(configuration).store_data(fake=fake)
