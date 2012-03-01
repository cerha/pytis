# -*- coding: utf-8 -*-

# Copyright (C) 2009-2012 Brailcom, o.p.s.
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

(Note that not all these functions are actually implemented here.)

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

- User roles and role membership.

- Access rights.

We use proprietary classes for storing, examining and manipulating given kinds
of data instead of using standard available pytis classes.  The reason is that
for first there are attributes not present in the standard pytis classes such
as database rows numeric identifiers and for second we need simple and
intelligible data structures and operations just serving the purpose of
implementing and maintaining the required functionality easily.  But if pytis
classes provide useful functionality for DMP we use the functionality.

This file defines several classes for the tasks defined above: 'DMPActions',
'DMPRoles', 'DMPMenu', 'DMPRights', 'DMPImport'.  There are also some utility
functions defined and exported here, serving mostly as an interface for 'dmp'
script.

"""

import copy
import re
import string

import pytis.data
import pytis.extensions
import pytis.form
from pytis.util import *


class DMPMessage(Structure):
    """Message about DMP operation to be presented to the user.

    This class and related facilities should be merged with other
    reporting/logging mechanisms in pytis.

    """
    ERROR_MESSAGE = 'Error'
    WARNING_MESSAGE = 'Warning'
    NOTE_MESSAGE = 'Note'
    SQL_MESSAGE = 'SQL'
    
    _attributes = (Attribute('kind', basestring),
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
      
    This function and related facilities should be merged with other
    reporting/logging mechanisms in pytis.

    """
    if messages is None:
        return
    m = DMPMessage(kind=kind, message=message, arguments=tuple([unicode(a) for a in arguments]))
    formatted = m.format()
    if not formatted or formatted[-1] != '\n':
        formatted += '\n'
    messages.append(formatted)


class DMPConfiguration(object):
    """Storage of various runtime objects for DMP processing."""

    def __init__(self, configuration_file=None, schemas=None,
                 database=None, host=None, port=None, user=None, password=None, sslmode=None):
        """
        Arguments:

          def_directory -- directory containing application specifications,
            string or 'None'
          schemas -- schemas string (schema names separated by commas) or 'None'
          database, host, port, user, password, sslmode -- common database
            connection parameters
            
        """
        import config
        if configuration_file is not None:
            for o in config.options():
                o.reset()
            config.config_file = configuration_file
            config.read_configuration_file(configuration_file)
        arguments_options = (('schemas', 'dbschemas'),
                             ('database', 'dbname'),
                             ('host', 'dbhost'),
                             ('port', 'dbport'),
                             ('user', 'dbuser'),
                             ('password', 'dbpass'),
                             ('sslmode', 'dbsslm'),)
        for argument, option in arguments_options:
            if locals()[argument] is not None:
                setattr(config, option, locals()[argument])
        if configuration_file is None:
            self._resolver = None
        else:
            self._resolver = config.resolver
        if schemas:
            schemas_list = [s.strip() for s in schemas_string.split(',')]
        else:
            schemas_list = None
        self._connection_data = config.dbconnection

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
        """Class for SQL commands logging."""
        def __init__(self):
            self._messages = []
            self._active = True
        def write(self, message):
            if self._active:
                for prefix in ('declare ', 'fetch ', 'move ', 'close ', 'select ',
                               'savepoint ', 'release ',):
                    if message.startswith(prefix) and not message.startswith('select * from pytis_'):
                        break
                else:
                    self.append('SQL: %s' % (message,))
        def append(self, message):
            self._messages.append(message)            
        def clear(self):
            self._messages = []
        def active(self):
            return self._active
        def disable(self):
            self._active = False
        def enable(self):
            self._active = True
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
        logger_active = self._logger.active()
        self._logger.disable()
        data = pytis.data.dbtable(table, self._DB_TABLES[table],
                                  self._configuration.connection_data(),
                                  sql_logger=self._logger)
        if logger_active:
            self._logger.enable()
        return data

    def _dbfunction(self, function):
        logger_active = self._logger.active()
        self._logger.disable()
        dbfunction = pytis.data.DBFunctionDefault(function, self._configuration.connection_data(),
                                                  sql_logger=self._logger)
        if logger_active:
            self._logger.enable()
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
        import config
        def strip_prefix(spec_name):
            for prefix in config.search_modules:
                prefix = prefix + '.'
                if spec_name.startswith(prefix) and len(spec_name) > len(prefix):
                    spec_name = spec_name[len(prefix):]
            return spec_name
        specification_names = pytis.extensions.get_form_defs(self._resolver(), messages)
        return [strip_prefix(name) for name in specification_names]

    def _specification(self, name, messages):
        resolver = self._resolver()
        try:
            return resolver.specification(name)
        except Exception as e:
            add_message(messages, DMPMessage.ERROR_MESSAGE,
                        "Couldn't load specification", (e,))
            return None

    def _disable_triggers(self, transaction=None, disable_import=False):
        data = self._data('e_pytis_disabled_dmp_triggers')
        data.insert(pytis.data.Row((('id', self._s_('genmenu'),),)), transaction=transaction)
        if disable_import:
            data.insert(pytis.data.Row((('id', self._s_('import'),),)), transaction=transaction)

    def _enable_triggers(self, transaction=None, disable_import=False):
        data = self._data('e_pytis_disabled_dmp_triggers')
        data.delete_many(pytis.data.EQ('id', self._s_('genmenu')), transaction=transaction)
        if disable_import:
            data.delete_many(pytis.data.EQ('id', self._s_('import')), transaction=transaction)

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
    
    def retrieve_data(self, transaction=None):
        """Load DMP data from the database."""
        import config
        config.initial_fetch_size = max(config.initial_fetch_size, 100000)
        config.fetch_size = max(config.fetch_size, 100000)
        self._reset()
        self._retrieve_data(transaction=transaction)

    def _retrieve_data(self, transaction=None):
        pass
    
    def store_data(self, fake, transaction=None, specifications=None):
        """Store DMP data into the database.

        Arguments:

          fake -- iff True, don't actually store the data but return sequence
            of SQL commands (basestrings) that would do so
          transaction -- transaction object to use or 'None'; if not 'None' no
            commit nor rollback is performed in this method regardless 'fake'
            argument value
          specifications -- if not 'None' then it is a sequence of specification
            names to restrict the operation to

        """
        messages = []
        if transaction is None:
            transaction_ = self._transaction()
        else:
            transaction_ = transaction
        success = self._store_data(transaction_, specifications)
        if fake:
            messages += self._logger.messages()
        if transaction is None:
            if success and not fake:
                transaction_.commit()
            else:
                transaction_.rollback()
        return messages

    def _store_data(self, transaction, specifications):
        return False

    def delete_data(self, fake, transaction=None, specifications=None):
        """Delete DMP data from the database.

        Arguments:
        
          fake -- iff True, don't actually delete the data but return sequence
            of SQL commands (basestrings) that would do so
          transaction -- transaction object to use or 'None'; if not 'None' no
            commit nor rollback is performed in this method regardless 'fake'
            argument value
          specifications -- if not 'None' then it is a sequence of specification
            names to restrict the operation to

        """
        messages = []
        if transaction is None:
            transaction_ = self._transaction()
        else:
            transaction_ = transaction
        condition = self._delete_condition(transaction_, specifications)
        success = self._delete_data(transaction_, condition)
        if fake:
            messages += self._logger.messages()
        if transaction is None:
            if success and not fake:
                transaction_.commit()
            else:
                transaction_.rollback()
        return messages

    def _delete_condition(self, transaction, specifications):
        if specifications is None:
            condition = pytis.data.AND()
        else:
            condition = self._specifications_condition(transaction, specifications)
        return condition

    def print_data(self, specifications=None):
        """Print object data.

        Arguments:
        
          specifications -- if not 'None' then it is a sequence of specification
            names to restrict the operation to

        """
        lines = self._print_data(specifications=specifications)
        for l in lines:
            sys.stdout.write('%s\n' % (l,))
        return []

    def _print_data(self, specifications=None):
        return []

    def _specifications_condition(self, transaction, specifications):
        def spec2cond(s):
            if len(s.split('/')) > 1:
                condition = pytis.data.EQ('shortname', self._s_(s))
            else:
                condition = pytis.data.WM('shortname', self._s_('*/%s' % (s,)), ignore_case=False)
            return condition
        return pytis.data.OR(*[spec2cond(s) for s in specifications])
    
    def dump_specifications(self, stream):
        """Dump DMP data in the form of Python source code."""
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

    Note that this class stores not only actual menu items, but also additional
    actions available in the application (in a menu like structure) in the form
    similar to menu rights editing form presentation.

    """
    class MenuItem(DMPItem):
        """Representation of a single menu item.

        There are several kinds of menu items:

          action item -- it invokes an action
          menu item -- it invokes a nested menu
          separator item -- visual separator in a menu

        """
        ACTION_ITEM = 'ACTION_ITEM'
        MENU_ITEM = 'MENU_ITEM'
        SEPARATOR_ITEM = 'SEPARATOR_ITEM'
        _attributes = (Attribute('id', int),
                       Attribute('name', basestring),
                       Attribute('kind', basestring),
                       Attribute('title', unicode),
                       Attribute('parent', mutable=True),
                       Attribute('children', list, mutable=True),
                       Attribute('action', basestring),
                       Attribute('position', basestring, mutable=True),
                       Attribute('next_position', basestring, default=None),
                       Attribute('hotkey', basestring),
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
                      [('e_pytis_menu', ('menuid', 'name', 'title', 'position', 'next_position', 'fullname', 'help', 'hotkey', 'locked'),),
                       ('ev_pytis_menu', ('menuid', 'name', 'title', 'position', 'next_position', 'help', 'hotkey', 'locked', 'fullname', 'shortname',),)])

    def __init__(self, *args, **kwargs):
        super(DMPMenu, self).__init__(*args, **kwargs)
        self._counter = Counter()

    def _reset(self):
        self._menu = []
        self._top_item = None
        
    def items(self):
        return self._menu

    def add_item(self, kind, parent=None, title=None, action=None, position=None, hotkey=None, help=None):
        id_ = self._counter.next()
        if title is not None:
            title = unicode(title)
        if help is not None:
            help = unicode(help)
        if parent is None and position is not None:
            pos = position.rfind('.')
            if pos >= 0:
                parent_position = position[:pos]
                for item in self.items():
                    if item.position == parent_position:
                        parent = item
                        break
        if action is None:
            name = None
        else:
            name = '%s/%s' % (id_, action,)
        menu_item = self.MenuItem(id=-id_, name=name, kind=kind, title=title,
                                  parent=parent, children=[],
                                  action=action, position=position,
                                  hotkey=hotkey, help=help)
        self._menu.append(menu_item)
        if parent is not None:
            parent.children().append(menu_item)
        return menu_item

    def _load_specifications(self):
        self._top_item = self.add_item(kind=self.MenuItem.MENU_ITEM, parent=None, title=u"CELÉ MENU", position='2')
        messages = []
        try:
            menu = self._resolver().get('application', 'menu')
        except ResolverError:
            add_message(messages, DMPMessage.WARNING_MESSAGE, "No application menu in specifications")
            return messages
        menu[0]._items = ((pytis.form.Menu(u"Správa menu a uživatelských rolí",
                                           (pytis.extensions.run_form_mitem(u"Menu", 'menu.ApplicationMenu',
                                                                            pytis.form.BrowseForm),
                                            pytis.extensions.run_form_mitem(u"Práva menu", 'menu.ApplicationMenuM',
                                                                            pytis.form.MultiBrowseDualForm),
                                            pytis.extensions.run_form_mitem(u"Uživatelské role", 'menu.ApplicationRoles',
                                                                            pytis.form.MultiBrowseDualForm),
                                            pytis.extensions.run_procedure_mitem(u"Aplikace změn práv",
                                                                                 'menu.ApplicationMenuRights', 'commit_changes'),
                                            pytis.form.MItem(u"Přenačtení menu a práv",
                                                             command=pytis.form.Application.COMMAND_RELOAD_RIGHTS),
                                            )),)
                          + menu[0]._items)
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
    
    def _retrieve_data(self, transaction=None):
        data = self._data('e_pytis_menu')
        items_by_position = {}
        children_by_position = {}
        def process(row):
            position = str(row['position'].value())
            if row['name'].value():
                kind = self.MenuItem.ACTION_ITEM
            elif row['title'].value():
                kind = self.MenuItem.MENU_ITEM
            else:
                kind = self.MenuItem.SEPARATOR_ITEM
            item = self.MenuItem(id=row['menuid'].value(),
                                 name=str(row['name'].value()),
                                 kind=kind,
                                 title=row['title'].value(),
                                 action=str(row['fullname'].value()),
                                 position=position,
                                 next_position=str(row['next_position'].value()),
                                 help=row['help'].value(),
                                 hotkey=str(row['hotkey'].value()),
                                 locked=row['locked'].value(),
                              )
            items_by_position[position] = item
            pos = position.rfind('.')
            if pos >= 0:
                parent_position = position[:pos]
                children_by_position[parent_position] = \
                  children_by_position.get(parent_position, []) + [item]
            return item
        self._menu = data.select_map(process, transaction=transaction)
        # Assign parents and children, find top item, reset counter
        min_id = 0
        for item in self._menu:
            position = item.position()
            pos = position.rfind('.')
            if pos >= 0:
                parent_position = position[:pos]
                item.set_parent(items_by_position[parent_position])
            else:
                if self._top_item is None or item.position() < self._top_item.position():
                    self._top_item = item
            item.set_children(children_by_position.get(position, []))
            if item.id() < min_id:
                min_id = item.id()
        self._counter = Counter(value=-min_id)
    
    def _store_data(self, transaction, specifications):
        data = self._data('e_pytis_menu')
        B = self._b_
        I = self._i_
        S = self._s_
        for item in self.items():
            fullname = item.action()
            if fullname is None:
                if specifications is not None:
                    continue
            else:
                action = DMPActions.Action(self._resolver(), None, fullname=fullname)
                if not action.specifications_match(specifications):
                    continue
            item_id_value = I(item.id())
            row = pytis.data.Row((('menuid', item_id_value,),
                                  ('name', S(item.name()),),
                                  ('title', S(item.title()),),
                                  ('fullname', S(fullname),),
                                  ('position', S(item.position()),),
                                  ('next_position', S(item.position()+'4'),),
                                  ('help', S(item.help()),),
                                  ('hotkey', S(item.hotkey()),),
                                  ('locked', B(item.locked()),),
                                  ))
            if not data.row(item_id_value):
                _, result = data.insert(row, transaction=transaction)
                if not result:
                    return False
        return True
    
    def _delete_data(self, transaction, condition):
        data = self._data('ev_pytis_menu')
        data.delete_many(condition, transaction=transaction)

    def delete_menu(self, fake, condition, transaction=None):
        messages = []
        data = self._data('e_pytis_menu')
        present = (data.select(condition=condition) > 0)
        data.close()
        if present:
            if transaction is None:
                transaction_ = self._transaction()
            else:
                transaction_ = transaction
            self._logger.clear()
            self._delete_data(transaction_, condition)
            if transaction is None:
                if fake:
                    messages += self._logger.messages()
                    transaction_.rollback()
                else:
                    transaction_.commit()            
        else:
            add_message(messages, DMPMessage.ERROR_MESSAGE, "No such position", (position,))
        return messages

    def _print_data(self, specifications=None):
        lines = []
        for item in self.items():
            fullname = item.action()
            if fullname is None:
                if specifications is not None:
                    continue
            else:
                action = DMPActions.Action(self._resolver(), None, fullname=fullname)
                if not action.specifications_match(specifications):
                    continue
            label = ('%s * %s' % (item.position(), item.title(),))[:40]
            lines.append('%4s %-40s %-30s %s' %
                         (item.id(), label, action.shortname() or '', fullname or '',))
        lines.reverse()
        return lines
    

class DMPRights(DMPObject):
    """Representation of DMP access rights."""

    class Right(DMPItem):
        _attributes = (Attribute('shortname', basestring),
                       Attribute('roleid', basestring),
                       Attribute('rightid', basestring),
                       Attribute('colname', basestring),
                       Attribute('system', bool),
                       Attribute('granted', bool),
                       Attribute('redundant', bool, default=None),
                       )

    _DB_TABLES = dict(DMPObject._DB_TABLES.items() +
                      [('e_pytis_action_rights', ('id', 'shortname', 'roleid', 'rightid', 'system', 'granted', 'colname', 'status', 'redundant',),)])

    def _reset(self):
        self._rights = []
        
    def items(self):
        return self._rights

    def add_item(self, shortname, roleid='*', rightid='*', colname=None, system=False, granted=True):
        item = self.Right(shortname=shortname, roleid=roleid, rightid=rightid, colname=colname,
                          system=system, granted=granted)
        self._rights.append(item)
        return item

    def remove_item(self, shortname, roleid, rightid, colname, system):
        rights = self._rights
        for i in range(len(rights)):
            r = rights[i]
            if (r.shortname() == shortname and r.roleid() == roleid and r.rightid() == rightid and
                r.colname() == colname and r.system() == system):
                del rights[i]
                return True
        return False

    def _load_specifications(self):
        messages = []
        def add_rights(shortname, access_specification):
            for item in access_specification:
                columns = item[0]
                if columns is None:
                    columns = [None]
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
                            for c in columns:
                                right = self.Right(shortname=shortname, roleid=(g or '*'),
                                                   rightid=p.lower(), colname=c,
                                                   system=True, granted=True)
                                self._rights.append(right)
        import config
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
            form_actions = spec.view_spec().actions(linear=True)
            if form_actions:
                for a in form_actions:
                    form_action_name = 'action/%s/%s' % (a.id(), spec_name,)
                    form_action_rights = ((None, (a.access_groups(), pytis.data.Permission.CALL)),)
                    add_rights(form_action_name, form_action_rights)
            # Print actions access rights
            for p in (spec.print_spec() or []):
                form_action_name = 'print/%s/%s' % (p.dmp_name(), spec_name,)
                if access_rights is None:
                    print_access_groups = None
                else:
                    print_access_groups = \
                      access_rights.permitted_groups(pytis.data.Permission.PRINT, None)
                print_action_rights = ((None, (print_access_groups, pytis.data.Permission.PRINT)),)
                add_rights(form_action_name, print_action_rights)
        return messages

    def _retrieve_data(self, transaction=None):
        data = self._data('e_pytis_action_rights')
        condition = pytis.data.LE('status', self._i_(0))
        def process(row):
            return self.Right(shortname=row['shortname'].value(),
                              roleid=row['roleid'].value(),
                              rightid=row['rightid'].value(),
                              colname=row['colname'].value(),
                              system=row['system'].value(),
                              granted=row['granted'].value(),
                              redundant=row['redundant'].value(),
                              )
        self._rights = data.select_map(process, condition=condition, transaction=transaction)
    
    def _store_data(self, transaction, specifications):
        data = self._data('e_pytis_action_rights')
        B = self._b_
        I = self._i_
        S = self._s_
        for right in self.items():
            shortname = right.shortname()
            if specifications is not None:
                if shortname not in specifications:
                    components = shortname.split('/')
                    if len(components) <= 1 or components[1] not in specifications:
                        continue
            row = pytis.data.Row((('shortname', S(shortname),),
                                  ('roleid', S(right.roleid()),),
                                  ('rightid', S(right.rightid()),),
                                  ('colname', S(right.colname()),),
                                  ('system', B(right.system()),),
                                  ('granted', B(right.granted(),),),
                                  ('status', I(0),),
                                  ))
            data.insert(row, transaction=transaction)
        return True

    def _delete_data(self, transaction, condition):
        data = self._data('e_pytis_action_rights')
        data.delete_many(condition, transaction=transaction)
        
    def _print_data(self, specifications=None):
        lines = []
        for right in self.items():
            shortname = right.shortname()
            if specifications is not None:
                components = shortname.split('/')
                if len(components) < 2 or components[1] not in specifications:
                    continue
            lines.append('%-60s %-32s %s%s %s %s %s' %
                         (shortname, right.roleid(), ('+' if right.granted() else '-'),
                          right.rightid()[:4], ('sys' if right.system() else '   '),
                          ('red' if right.redundant() else '   '), right.colname() or '',))
        return lines

    def dmp_restore(self, fake, specifications):
        """Restore access rights of the given specification.

        Access rights of the specification in the database are deleted and
        initialized again from application specifications.

        Arguments:

          fake -- iff True, don't actually change the data but return sequence
            of SQL commands (basestrings) that would do so
          def_directory -- directory containing application specifications,
            string or 'None'
          specifications -- sequence of specification names to restrict the
            operation to

        """
        messages = []
        transaction = self._transaction()
        self._disable_triggers(transaction=transaction)
        messages += self.delete_data(fake, transaction=transaction, specifications=specifications)
        self.load_specifications()
        messages += self.store_data(fake, transaction=transaction, specifications=specifications)
        messages += self.commit(fake, transaction=transaction)
        self._enable_triggers(transaction=transaction)
        if fake:
            transaction.rollback()
        else:
            transaction.commit()
        return messages

    def dmp_change_rights(self, fake, requests):
        transaction = self._transaction()
        self._disable_triggers(transaction=transaction)
        rights = DMPRights(self._configuration)
        rights.retrieve_data(transaction=transaction)
        roles = DMPRoles(self._configuration)
        roles.retrieve_data(transaction=transaction)
        known_roles = [r.name() for r in roles.items()]
        actions = DMPActions(self._configuration)
        actions.retrieve_data(transaction=transaction)
        known_shortnames = [a.shortname() for a in actions.items()]
        specifications = set()
        messages = []
        for r in requests:
            shortname, roleid, rightid, granted, colname, system = r
            if shortname not in known_shortnames:
                add_message(messages, DMPMessage.ERROR_MESSAGE, "No such action", (shortname,))
                continue
            if roleid != '*' and roleid not in known_roles:
                add_message(messages, DMPMessage.ERROR_MESSAGE, "No such role", (roleid,))
                continue
            rights.remove_item(shortname=shortname, roleid=roleid, rightid=rightid,
                               colname=colname, system=system)
            if granted is not None and (not system or granted):
                rights.add_item(shortname=shortname, roleid=roleid, rightid=rightid,
                                colname=colname, system=system, granted=granted)
            specifications.add(shortname)
        specifications = list(specifications)
        messages += rights.delete_data(fake, transaction, specifications=specifications)
        rights._logger.clear()
        messages += rights.store_data(fake, transaction, specifications=specifications)
        self._enable_triggers(transaction=transaction)
        if messages:
            transaction.rollback()
            add_message(messages, DMPMessage.ERROR_MESSAGE, "Rights not changed")
        else:
            transaction.commit()
        return messages

    def dmp_copy_rights(self, fake, from_shortname, to_shortname):
        transaction = self._transaction()
        row = pytis.data.Row((('copy_from', pytis.data.sval(from_shortname),),
                              ('copy_to', pytis.data.sval(to_shortname),),))
        dbfunction = self._dbfunction('pytis_copy_rights')
        dbfunction.call(row, transaction=transaction)
        if fake:
            messages = self._logger.messages()
            transaction.rollback()
        else:
            messages = []
            transaction.commit()
        return messages

    def commit(self, fake, transaction=None):
        """Commit changes in access rights stored in the database.

        This makes access rights being prepared in the database tables actually
        effective.
        
        """
        if transaction is None:
            transaction_ = self._transaction()
        else:
            transaction_ = transaction
        dbfunctions = [self._dbfunction(f) for f in ('pytis_update_transitive_roles',
                                                     'pytis_update_actions_structure',
                                                     'pytis_update_summary_rights',
                                                     'pytis_update_rights_redundancy',)]
        self._logger.clear()
        empty_row = pytis.data.Row(())
        for f in dbfunctions:
            f.call(empty_row, transaction=transaction_)
        if fake:
            messages = self._logger.messages()
        else:
            messages = []
        if transaction is None:
            if fake:
                transaction_.rollback()
            else:
                transaction_.commit()
        return messages


class DMPRoles(DMPObject):
    """Representation of DMP roles and their memberships."""
    
    class Role(DMPItem):
        _attributes = (Attribute('name', basestring),
                       Attribute('description', basestring),
                       Attribute('purposeid', basestring, mutable=True),
                       Attribute('members', list, mutable=True),
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
                       ('e_pytis_role_members', ('id', 'roleid', 'member',),),
                       ('pg_roles', ('oid', 'rolname', 'rolcanlogin',),),
                       ('pg_auth_members', ('roleid', 'member',),),
                       ])

    def _reset(self):
        self._roles = []
        
    def items(self):
        return self._roles

    def _load_specifications(self, dmp_rights):
        roles = self._roles
        for name, purpose, members in (('admin_roles', 'admn', ['admin'],),
                                       ('admin_menu', 'admn', ['admin'],),
                                       ('admin', 'admn', [],),):
            roles.append(self.Role(name=name, description='', purposeid=purpose, members=members))
        roleids = [r.name() for r in roles]
        for right in dmp_rights.items():
            roleid = right.roleid()
            if roleid not in roleids:
                roles.append(self.Role(name=roleid, description='', purposeid='appl'))
                roleids.append(roleid)
        return []

    def _retrieve_data(self, transaction=None):
        data = self._data('e_pytis_roles')
        def process(row):
            return self.Role(name=row['name'].value(),
                             description=row['description'].value(),
                             purposeid=row['purposeid'].value(),
                             )
        self._roles = data.select_map(process, transaction=transaction)
    
    def _store_data(self, transaction, specifications):
        S = self._s_
        data = self._data('e_pytis_roles')
        for role in self.items():
            row = pytis.data.Row((('name', S(role.name()),),
                                  ('description', S(role.description()),),
                                  ('purposeid', S(role.purposeid()),),
                                  ))
            data.insert(row, transaction=transaction)
        data = self._data('e_pytis_role_members')
        for role in self.items():
            name = role.name()
            for member in (role.members() or ()):
                row = pytis.data.Row((('roleid', S(name),),
                                      ('member', S(member),),))
                data.insert(row, transaction=transaction)
        return True

    def _delete_data(self, transaction, condition):
        data_members = self._data('e_pytis_role_members')
        data_roles = self._data('e_pytis_roles')
        self._logger.clear()
        data_members.delete_many(condition, transaction=transaction)
        data_roles.delete_many(condition, transaction=transaction)

    def _print_data(self, specifications=None):
        lines = []
        for role in self.items():
            lines.append('%-32s %s %s' %
                         (role.name(), role.purposeid(), role.description() or '',))
        return lines
        
    def load_system_roles(self):
        """Load PostgreSQL roles."""
        excluded_roles = ('postgres',)
        semi_excluded_roles = ('admin', 'admin_roles', 'admin_menu',) + excluded_roles
        roles_by_names = {}
        for role in self._roles:
            roles_by_names[role.name()] = role
        # Roles
        role_oids = {}
        data = self._data('pg_roles')
        def process(row):
            oid = row['oid'].value()
            role = str(row['rolname'].value())
            login = row['rolcanlogin'].value()
            role_oids[oid] = role
            if role not in semi_excluded_roles:
                if login:
                    purpose = 'user'
                else:
                    purpose = 'appl'
                if role not in roles_by_names:
                    new_role = self.Role(name=role, purposeid=purpose)
                    self._roles.append(new_role)
                    roles_by_names[role] = new_role
                elif purpose != 'appl':
                    roles_by_names[role].set_purposeid(purpose)
        data.select_map(process)
        # Membership
        data = self._data('pg_auth_members')
        def process(row):
            try:
                roleid = role_oids[row['roleid'].value()]
                member = role_oids[row['member'].value()]
            except KeyError:
                # It may happen that a pg_auth_members item is not present in
                # pg_roles.  We don't know why but let's not crash in such a
                # case.
                return
            try:
                role = roles_by_names[roleid]
            except KeyError:            # semi excluded roles
                return
            role_members = role.members() or []
            if member not in role_members:
                role.set_members(role_members + [member])
        data.select_map(process)

    def dmp_add_member(self, fake, member, role):
        """Add new member to role.

        Arguments:

          fake -- iff True, don't actually change the data but return sequence
            of SQL commands (basestrings) that would do so
          member -- name of the member to be added, string
          role -- name of the target role, string
          
        """
        messages = []
        member_value = pytis.data.sval(member)
        role_value = pytis.data.sval(role)
        condition = pytis.data.AND(pytis.data.EQ('member', member_value),
                                   pytis.data.EQ('roleid', role_value))
        data = self._data('e_pytis_role_members')
        already_present = (data.select(condition=condition) > 0)
        data.close()
        if already_present:
            add_message(messages, DMPMessage.ERROR_MESSAGE,
                        "Member already present in the role", (member, role,))
        else:
            row = pytis.data.Row((('member', member_value,),
                                  ('roleid', role_value,),
                                  ))
            transaction = self._transaction()
            self._logger.clear()
            data.insert(row, transaction=transaction)
            if fake:
                messages += self._logger.messages()
                transaction.rollback()
            else:
                transaction.commit()
        return messages


class DMPActions(DMPObject):
    """Representation of DMP actions.

    Note that actions are not loaded directly from specifications, but they are
    retrieved from loaded menu items, access rights and other loaded objects.

    """

    class Action(DMPItem):
        """Representation of an DMP action.

        DMP action is uniquely identified by its fullname.  Other properties are
        derived from the fullname.

        """
        _attributes = (Attribute('fullname', basestring),
                       Attribute('title', basestring, mutable=True),
                       Attribute('description', basestring),
                       Attribute('special_shortname', basestring),
                       )

        def __init__(self, resolver, messages, **kwargs):
            DMPItem.__init__(self, **kwargs)
            assert self.fullname() is not None
            self._make_alternate_fullname(resolver, messages)

        def _make_alternate_fullname(self, resolver, messages):
            fullname = self.fullname()
            components = fullname.split('/')
            if components[0] == 'RUN_FORM':
                form_string = components[1]
                try:
                    command, args = resolver.get('app_commands', form_string)
                    form_class = args['form_class']
                    if not issubclass(form_class, pytis.form.Form):
                        raise Exception()
                    form_name = args['name']
                except:
                    if messages is not None:
                        add_message(messages, DMPMessage.WARNING_MESSAGE,
                                    "Failed to retrieve RUN_FORM command", (form_string,))
                    form_name = None
                if form_name is not None:
                    self._alternate_fullname = pytis.form.make_fullname(form_class, form_name)
                else:
                    self._alternate_fullname = fullname
            elif components[0] == 'sub' and components[2] == 'RUN_FORM':
                subaction = self.__class__(resolver, messages,
                                           fullname=string.join(components[2:], '/'))
                self._alternate_fullname = '%s/%s/%s' % (components[0], components[1],
                                                         subaction._alternate_fullname)
            else:
                self._alternate_fullname = fullname
            
        def signature(self):
            return self.fullname()

        def equal(self, other):
            equal = DMPItem.equal(self, other)
            if equal is False and self.fullname() == other.fullname():
                equal = None
            return equal

        def _components(self):
            return self._alternate_fullname.split('/')

        def shortname(self):
            """Return shortname of the action as a string."""
            if self.special_shortname():
                return self.special_shortname()
            components = self._components()
            if components[0] == 'form':
                shortname = 'form/%s' % (components[2],)
            elif components[0] == 'sub':
                # Normally the shortname should be given as special_shortname.
                # But there may be no subform specification in case the subform
                # is a detail form, web form or so.
                if len(components) < 5: # only in wrong specifications
                    subform = 'INVALID'
                else:
                    subform = components[4]
                shortname = 'form/%s' % (subform,)
            else:
                shortname = self.fullname()
            return shortname

        def kind(self):
            """Return kind of the action as a string."""
            return self._components()[0]

        def form_name(self):
            """Return name of the specification related to the action.

            For non-form actions return 'None'.
            
            """
            components = self._components()
            if components[0] == 'form':
                name = components[2]
            else:
                name = None
            return name

        def form_class(self):
            """Return class of the action form.

            For non-form actions and dummy form actions return 'None'.
            
            """
            components = self._components()
            if components[0] == 'form' and components[1] != '*':
                class_ = eval(components[1])
            else:
                class_ = None
            return class_

        def specifications_match(self, specifications):
            if specifications is None:
                return True
            fullname = self.fullname()
            full_components = fullname.split('/')
            short_components = self.shortname().split('/')
            if short_components[0] == 'print':
                spec_name = full_components[-1]
            elif full_components[0] == 'sub':
                spec_name = full_components[4]
                fullname = string.join(full_components[2:], '/')
            else:
                spec_name = short_components[-1]
            for s in specifications:
                if spec_name == s or fullname == s:
                    return True
            return False
            
        @classmethod
        def dummy_action(class_, shortname):
            components = shortname.split('/')
            if components[0] == 'form':
                fullname = 'form/*/%s' % (components[1],)
            else:
                fullname = shortname
            return class_(None, None, fullname=fullname)
        
    _DB_TABLES = dict(DMPObject._DB_TABLES.items() +
                      [('c_pytis_menu_actions', ('fullname', 'shortname', 'action_title', 'description',),)])

    def _reset(self):
        self._actions = []
        self._fullnames = {}
        self._shortnames = {}

    def _add_action(self, action):
        fullname = action.fullname()
        if fullname in self._fullnames:
            return
        self._actions.append(action)
        self._fullnames[fullname] = action
        shortname = action.shortname()
        self._shortnames[shortname] = self._shortnames.get(shortname, []) + [action]
        
    def items(self):
        return self._actions

    def _load_specifications(self, dmp_menu=None, dmp_rights=None, actions=None):
        messages = []
        if dmp_menu is not None:
            self._load_from_menu(dmp_menu.items(), messages)
        if dmp_rights is not None:
            self._load_from_rights(dmp_rights.items(), messages)
        if actions is not None:
            self._load_from_actions(actions, messages)
        return messages

    def _load_from_menu(self, items, messages):
        for menu in items:
            fullname = menu.action()
            if fullname is None:
                continue
            action = self.Action(self._resolver(), messages, fullname=fullname, title=menu.title())
            self._load_complete_action(action, messages)
            
    def _load_from_actions(self, actions, messages):
        for action in actions:
            self._load_complete_action(action, messages)

    def _load_complete_action(self, action, messages):
        # Retrieve specification
        form_name = action.form_name()
        if form_name is None:
            spec = None
        else:
            spec = self._specification(form_name.split('::')[0], messages)
        # Register the main action
        if not action.title():
            if spec is not None:
                action.set_title(spec.view_spec().title())
        self._add_action(action)
        if spec is None:
            return
        # Subforms
        form_class = action.form_class()
        def binding(name):
            spec = self._specification(name, messages)
            if spec is None:
                title = ''
            else:
                try:
                    title = spec.view_spec().title()
                except Exception as e:
                    title = ''
                    add_message(messages, DMPMessage.ERROR_MESSAGE,
                                "Can't create specification instance to get binding title",
                                (name, e,))
            return pytis.presentation.Binding(id=name, title=title, name=name,
                                              binding_column='dummy')
        resolver = self._resolver()
        if form_class is not None and issubclass(form_class, pytis.form.DualForm):
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
                    bname = b.name()
                    subaction_shortname = 'form/%s' % (bname,)
                    self._add_action(self.Action(resolver, messages,
                                                 fullname=subaction_fullname,
                                                 title=subaction_title,
                                                 special_shortname=subaction_shortname))
                    subspec = self._specification(bname, [])
                    if subspec is not None:
                        for a in subspec.view_spec().actions(linear=True):
                            fullname = 'action/%s/%s' % (a.id(), bname,)
                            subsubaction = self.Action(resolver, messages,
                                                       fullname=fullname, title=a.title(raw=True))
                            self._add_action(subsubaction)
                        for p in subspec.print_spec():
                            fullname = 'print/%s/%s' % (p.dmp_name(), bname,)
                            subsubaction = self.Action(resolver, messages,
                                                       fullname=fullname, title=p.title())
                            self._add_action(subsubaction)
        # Form actions
        for a in spec.view_spec().actions(linear=True):
            fullname = 'action/%s/%s' % (a.id(), form_name,)
            action = self.Action(resolver, messages,
                                 fullname=fullname, title=a.title(raw=True))
            self._add_action(action)
        # Print actions
        for p in spec.print_spec():
            fullname = 'print/%s/%s' % (p.dmp_name(), form_name,)
            action = self.Action(resolver, messages,
                                 fullname=fullname, title=p.title())
            self._add_action(action)
        
    def _load_from_rights(self, items, messages):
        for right in items:
            if not self._shortnames.get(right.shortname()):
                self._add_action(self.Action.dummy_action(right.shortname()))

    def _retrieve_data(self, transaction=None):
        data = self._data('c_pytis_menu_actions')
        def process(row):
            action = self.Action(self._resolver(), None,
                                 fullname=str(row['fullname'].value()),
                                 special_shortname=str(row['shortname'].value()),
                                 title=row['action_title'].value(),
                                 description=row['description'].value())
            self._add_action(action)    
        data.select_map(process, transaction=transaction)
    
    def _store_data(self, transaction, specifications, subforms_only=False, original_actions=None):
        data = self._data('c_pytis_menu_actions')
        S = self._s_
        for action in self.items():
            if not action.specifications_match(specifications):
                continue
            if subforms_only and action.fullname().split('/')[0] != 'sub':
                continue
            if (original_actions is not None and
                action.fullname() in original_actions._fullnames):
                continue
            row = pytis.data.Row((('fullname', S(action.fullname()),),
                                  ('shortname', S(action.shortname()),),
                                  ('action_title', S(unicode(action.title() or '')),),
                                  ('description', S(unicode(action.description() or '')),),
                                  ))
            data.insert(row, transaction=transaction)
        return True

    def _delete_data(self, transaction, condition):
        data = self._data('c_pytis_menu_actions')
        data.delete_many(condition, transaction=transaction)

    def dmp_update_forms(self, fake, specification, new_fullname=None):
        transaction = self._transaction()
        self._disable_triggers(transaction=transaction)
        result = self.update_forms(fake, specification, new_fullname=new_fullname,
                                   transaction=transaction)
        self._enable_triggers(transaction=transaction)
        if fake:
            transaction.rollback()
        else:
            transaction.commit()
        return result
        
    def update_forms(self, fake, specification, new_fullname=None, transaction=None):
        """Check given form specifications and update the database.

        For given form specification name, load the specification and check
        its bindings and actions.  Delete old bindings and actions from the
        database and insert the new ones.

        Arguments:

          fake -- iff True, don't actually change the data but return sequence
            of SQL commands (basestrings) that would do so
          specification -- specification name, string
          new_fullname -- if not 'None', it defines new fullname of the
            specification, string
          transaction -- transaction object to use or 'None'; if not 'None' no
            commit nor rollback is performed in this method regardless 'fake'
            argument value

        """
        messages = []
        original_actions = DMPActions(self._configuration)
        original_actions.retrieve_data(transaction=transaction)
        menu = DMPMenu(self._configuration)
        menu.retrieve_data(transaction=transaction)
        rights = DMPRights(self._configuration)
        rights.load_specifications()
        self.load_specifications(dmp_menu=menu, dmp_rights=rights)
        if transaction is None:
            transaction_ = self._transaction()
        else:
            transaction_ = transaction
        self._logger.clear()
        condition = pytis.data.WM('fullname', self._s_('sub/*/%s/*' % (specification,)), ignore_case=False)
        self._delete_data(transaction=transaction_, condition=condition)
        self._store_data(transaction=transaction_, specifications=[specification],
                         subforms_only=True)
        self._store_data(transaction=transaction_, specifications=[specification],
                         original_actions=original_actions)
        if fake:
            messages += self._logger.messages()
        self._logger.clear()
        for action in original_actions.items():
            if (action.specifications_match([specification]) and
                action.kind() in ('action', 'print',) and
                action.fullname() not in self._fullnames):
                condition = pytis.data.EQ('fullname', self._s_(action.fullname()))
                self._delete_data(transaction_, condition)
        if new_fullname:
            data = self._data('c_pytis_menu_actions')
            actions_to_rename = [a for a in self.items()
                                 if a.specifications_match([specification]) and
                                    a.fullname().split('/')[0] == 'form']
            if not actions_to_rename:
                add_message(messages, DMPMessage.WARNING_MESSAGE, "No specification to rename")
            elif len(actions_to_rename) > 1:
                add_message(messages, DMPMessage.ERROR_MESSAGE,
                            "More than one specification to rename",
                            [a.fullname() for a in actions_to_rename])
            else:
                row = pytis.data.Row((('fullname', pytis.data.sval(new_fullname),),))
                data.update(pytis.data.sval(actions_to_rename[0].fullname()), row, transaction=transaction_)
        if fake:
            messages += self._logger.messages()
        if transaction is None:
            if fake:
                transaction_.rollback()
            else:
                transaction_.commit()
        return messages

    def dmp_delete_name(self, fake, name, action_type):
        messages = []
        data = self._data('c_pytis_menu_actions')
        condition = pytis.data.EQ(action_type, pytis.data.sval(name))
        data.select(condition=condition)
        row = data.fetchone()
        data.close()
        if row is None:
            add_message(messages, DMPMessage.ERROR_MESSAGE, "No such "+action_type, (name,))
        else:
            shortname = row['shortname'].value()
            menu = DMPMenu(self._configuration)
            rights = DMPRights(self._configuration)
            transaction = self._transaction()
            self._disable_triggers(transaction=transaction)
            self._logger.clear()
            menu.delete_data(fake, transaction=transaction, specifications=(shortname,))
            rights.delete_data(fake, transaction=transaction, specifications=(shortname,))
            self._delete_data(transaction, condition)
            self._enable_triggers(transaction=transaction)
            if fake:
                messages += self._logger.messages()
            if fake:
                transaction.rollback()
            else:
                transaction.commit()
        return messages

    def dmp_no_rights(self):
        messages = []
        self.retrieve_data()
        rights = DMPRights(self._configuration)
        rights.retrieve_data()
        known_shortnames = dict([(r.shortname(), None) for r in rights.items()])
        for action in self.items():
            shortname = action.shortname()
            prefix = shortname.split('/')[0]
            if (prefix not in ('menu', 'EXIT', 'RELOAD_RIGHTS', 'NEW_RECORD') and
                shortname not in known_shortnames):
                add_message(messages, DMPMessage.WARNING_MESSAGE,
                            "Action without rights", (action.shortname(), action.fullname(),))
        return messages

    def dmp_extra_rights(self):
        messages = []
        self.retrieve_data()
        rights = DMPRights(self._configuration)
        rights.retrieve_data()
        known_shortnames = set()
        for a in self.items():
            shortname = a.shortname()
            match = re.match('form/(.+)::(.+)', shortname)
            if match:
                known_shortnames.add('form/' + match.group(1))
                known_shortnames.add('form/' + match.group(2))
            else:
                known_shortnames.add(shortname)
        for r in rights.items():
            if r.shortname() not in known_shortnames:
                add_message(messages, DMPMessage.WARNING_MESSAGE,
                            "Right without action", (r.shortname(),))
        return messages        

    def _load_database_and_specifications(self, messages):
        # Load database actions
        self.retrieve_data()
        fullnames = [a.fullname() for a in self.items()
                     if not a.fullname().startswith('sub/')]
        known_names = set()
        form_actions = {}
        subforms = {}
        for action in self.items():
            fullname = action.fullname()
            shortname = action.shortname()
            if shortname.startswith('form/'):
                known_names.add(shortname[5:])
            if fullname.startswith('sub/'):
                parent_fullname = fullname[fullname.find('/', 4)+1:]
                subforms[parent_fullname] = subforms.get(parent_fullname, []) + [action]
            if (not fullname.startswith('form/') or
                action.form_class() is None):
                continue
            form_name = action.form_name()
            form_actions[form_name] = form_actions.get(form_name, []) + [action]
        # Load actions from specifications
        spec_actions = self.__class__(self._configuration)
        for name in self._all_form_specification_names(messages):
            actions = form_actions.get(name)
            if actions:
                for a in actions:
                    spec_actions._load_complete_action(a, messages)
            elif name not in known_names:
                add_message(messages, DMPMessage.WARNING_MESSAGE,
                            "Specification not present in the database", (name,))
        spec_fullnames = [a.fullname() for a in spec_actions.items()
                          if not a.fullname().startswith('sub/')]
        spec_subforms = {}
        for action in spec_actions.items():
            fullname = action.fullname()
            if fullname.startswith('sub/'):
                parent_fullname = fullname[fullname.find('/', 4)+1:]
                spec_subforms[parent_fullname] = spec_subforms.get(parent_fullname, []) + [action]
        # Return actions
        return fullnames, subforms, spec_fullnames, spec_subforms
        
    def dmp_missing(self):
        messages = []
        fullnames, subforms, spec_fullnames, spec_subforms = \
                   self._load_database_and_specifications(messages)
        for fullname in spec_fullnames:
            if fullname not in fullnames:
                add_message(messages, DMPMessage.WARNING_MESSAGE,
                            "Missing form component", (fullname,))
            else:
                spec_sub = spec_subforms.get(fullname)
                if spec_sub:
                    sub = (subforms.get(fullname) or [])
                    spec_short = set([s.shortname() for s in spec_sub])
                    short = set([s.shortname() for s in sub])
                    for shortname in (spec_short - short):
                        add_message(messages, DMPMessage.WARNING_MESSAGE,
                                    "Missing subform", (fullname, shortname,))
        return messages

    def dmp_extra(self):
        messages = []
        fullnames, subforms, spec_fullnames, spec_subforms = \
                   self._load_database_and_specifications(messages)
        known_spec_fullnames = set(spec_fullnames)
        for action_list in spec_subforms.values():
            fullnames_set = set([a.fullname() for a in action_list])
            known_spec_fullnames = known_spec_fullnames.union(fullnames_set)
        for fullname in fullnames:
            components = fullname.split('/')
            kind = components[0]
            if kind == 'menu' or kind == kind.upper():
                continue
            if kind in ('action', 'print',):
                if fullname not in known_spec_fullnames:
                    add_message(messages, DMPMessage.WARNING_MESSAGE,
                                "Extra action", (fullname,))
                continue
            if fullname in spec_fullnames:
                sub = subforms.get(fullname)
                if sub:
                    spec_sub = (spec_subforms.get(fullname) or [])
                    short = set([s.shortname() for s in sub])
                    spec_short = set([s.shortname() for s in spec_sub])
                    for shortname in (short - spec_short):
                        add_message(messages, DMPMessage.WARNING_MESSAGE,
                                    "Extra form action", (fullname, shortname,))                    
            else:
                add_message(messages, DMPMessage.WARNING_MESSAGE,
                            "Extra action", (fullname,))
        return messages
        
    def convert_system_rights(self, fake, shortname):
        row = pytis.data.Row((('shortname', pytis.data.sval(shortname),),))
        self._dbfunction('pytis_convert_system_rights').call(row)


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
        self._dmp_roles.load_system_roles()
        messages += self._dmp_actions.load_specifications(dmp_menu=self._dmp_menu, dmp_rights=self._dmp_rights)
        return messages
    
    def delete_data(self, fake, transaction=None):
        if transaction is None:
            transaction_ = self._transaction()
        else:
            transaction_ = transaction
        messages = []
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
        """Import data from specifications and store them to a database.

        Original database data are deleted.

        Arguments:

          fake -- iff True, don't actually change the data but return sequence
            of SQL commands (basestrings) that would do so
            
        """
        transaction = self._transaction()
        self._disable_triggers(transaction=transaction, disable_import=True)
        messages = []
        messages += self.load_specifications()
        messages += self.delete_data(fake, transaction=transaction)
        messages += self.store_data(fake, transaction=transaction)
        self._enable_triggers(transaction=transaction, disable_import=True)
        if fake:
            transaction.rollback()
        else:
            transaction.commit()
        return messages

    def dmp_add_form(self, fake, fullname, position):
        """Add new form invocation action.

        Arguments:

          fake -- iff True, don't actually change the data but return sequence
            of SQL commands (basestrings) that would do so
          fullname -- fullname of the form invoking action, string
          position -- menu position of the action or preceding item title,
            basestring, or True (in such a case the form is added to actions
            but no menu item is created)

        """
        transaction = self._transaction()
        self._disable_triggers(transaction=transaction)
        resolver = self._resolver()
        messages = []
        action = DMPActions.Action(resolver, messages, fullname=fullname)
        messages += self._dmp_actions.load_specifications(actions=[action])
        messages += self._dmp_actions.store_data(fake, transaction)
        specification = action.form_name()
        if position is not True:
            menu = self._dmp_menu
            menu.retrieve_data(transaction=transaction)
            if not re.match('[0-9.]+$', position):
                previous_items = [m for m in menu.items() if m.title() == position]
                if not previous_items:
                    add_message(messages, DMPMessage.ERROR_MESSAGE, "No such menu item", (position,))
                    position = None
                elif len(previous_items) > 1:
                    add_message(messages, DMPMessage.ERROR_MESSAGE, "Multiple menu items",
                                ('%s (%s)' % (position, ', '.join([m.position() for m in previous_items]),),))
                    position = None
                else:
                    position = previous_items[0].next_position()
        if position is not None:
            if position is not True:
                menu_item = menu.add_item(kind=DMPMenu.MenuItem.ACTION_ITEM,
                                          title=action.title(), action=fullname, position=position)
                messages += menu.store_data(fake, transaction=transaction, specifications=[fullname])
            messages += self._dmp_actions.update_forms(fake, specification, transaction=transaction)
            self._dmp_rights.retrieve_data(transaction=transaction)
            shortname = action.shortname()
            for a in self._dmp_rights.items():
                if a.shortname() == shortname:
                    break
            else:
                for m in ([] if position is True else menu.items()):
                    if (m is not menu_item and
                        DMPActions.Action(resolver, [], fullname=m.action()).shortname() == shortname):
                        break
                else:
                    self._dmp_rights.add_item(shortname, granted=False)
            components = shortname.split('/')
            if len(components) > 1:        
                messages += self._dmp_rights.store_data(fake, transaction=transaction,
                                                        specifications=[components[1]])
        self._enable_triggers(transaction=transaction)
        if fake:
            transaction.rollback()
        else:
            transaction.commit()
        return messages
    
    def dmp_rename_specification(self, fake, old_name, new_name):
        transaction = self._transaction()
        row = pytis.data.Row((('old_name', pytis.data.sval(old_name),),
                              ('new_name', pytis.data.sval(new_name),),))
        dbfunction = self._dbfunction('pytis_change_specification_name')
        dbfunction.call(row, transaction=transaction)
        if fake:
            messages = self._logger.messages()
            transaction.rollback()
        else:
            messages = []
            transaction.commit()
        return messages


def dmp_import(parameters, fake):
    """Import DMP data from application specifications to a database."""
    configuration = DMPConfiguration(**parameters)
    return DMPImport(configuration).dmp_import(fake)

def dmp_add_member(parameters, fake, member, role):
    """Add member to role."""
    configuration = DMPConfiguration(**parameters)
    return DMPRoles(configuration).dmp_add_member(fake, member, role)

def dmp_add_form(parameters, fake, fullname, position):
    """Add new form from specifications to database menu."""
    configuration = DMPConfiguration(**parameters)
    return DMPImport(configuration).dmp_add_form(fake, fullname, position)

def dmp_update_form(parameters, fake, specification, new_fullname):
    """Update form subforms and actions from specifications."""
    configuration = DMPConfiguration(**parameters)
    return DMPActions(configuration).dmp_update_forms(fake, specification, new_fullname=new_fullname)

def dmp_rename_specification(parameters, fake, old_name, new_name):
    configuration = DMPConfiguration(**parameters)
    return DMPImport(configuration).dmp_rename_specification(fake, old_name, new_name)

def dmp_reset_rights(parameters, fake, specification):
    """Restore access rights of the given specification.

    Delete them from the database, load them from application specification and
    store them again into the database.

    """
    configuration = DMPConfiguration(**parameters)
    return DMPRights(configuration).dmp_restore(fake, [specification])

def dmp_commit(parameters, fake):
    """Make access rights prepared in the database actually effective."""
    configuration = DMPConfiguration(**parameters)
    return DMPRights(configuration).commit(fake=fake)

def dmp_delete_menu(parameters, fake, position):
    configuration = DMPConfiguration(**parameters)
    condition = pytis.data.EQ('position', pytis.data.sval(position))
    return DMPMenu(configuration).delete_menu(fake, condition)

def dmp_delete_fullname(parameters, fake, fullname):
    configuration = DMPConfiguration(**parameters)
    return DMPActions(configuration).dmp_delete_name(fake, fullname, 'fullname')

def dmp_delete_shortname(parameters, fake, shortname):
    configuration = DMPConfiguration(**parameters)
    return DMPActions(configuration).dmp_delete_name(fake, shortname, 'shortname')

def dmp_convert_system_rights(parameters, fake, shortname):
    configuration = DMPConfiguration(**parameters)
    return DMPActions(configuration).convert_system_rights(fake, shortname)

def dmp_change_rights(parameters, fake, requests):
    configuration = DMPConfiguration(**parameters)
    return DMPRights(configuration).dmp_change_rights(fake, requests)

def dmp_copy_rights(parameters, fake, from_shortname, to_shortname):
    configuration = DMPConfiguration(**parameters)
    return DMPRights(configuration).dmp_copy_rights(fake, from_shortname, to_shortname)

def dmp_ls(parameters, fake, what, specifications=None):
    configuration = DMPConfiguration(**parameters)
    if what == 'norights':
        return DMPActions(configuration).dmp_no_rights()
    elif what == 'exrights':
        return DMPActions(configuration).dmp_extra_rights()
    elif what == 'missing':
        return DMPActions(configuration).dmp_missing()
    elif what == 'extra':
        return DMPActions(configuration).dmp_extra()
    elif what == 'menu':
        class_ = DMPMenu
    elif what == 'roles':
        class_ = DMPRoles
    elif what == 'rights':
        class_ = DMPRights
    else:
        raise Exception("Program error", what)
    instance = class_(configuration)
    instance.retrieve_data()
    return instance.print_data(specifications=specifications)
