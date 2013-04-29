# -*- coding: utf-8 -*-

# Copyright (C) 2011, 2012, 2013 Brailcom, o.p.s.
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

"""User settings managers.

This module defines classes for accessing various user specific data, such as
application configuration, form profiles and other settings.  The user's data
are stored in database tables but pytis internally uses ``managers'' to access
them.  All managers are similar, but each of them has a slightly different API.

A pytis application instance automatically creates instances of all managers
needed by a typical pytis application at startup and these managers are then
accessible from within pytis through functions defined in application.py, such
as 'profile_manager()', etc.

Most of the settings are stored as pickled python objects.  Pickling and
unpickling is done internally by the manager, so the API works transparently
with python objects.  These objects just must be safe to pickle and unpickle
(ideally they should consist just of basic python data types or they must
implement additional support for pickling).

Settings are always identified by the current user (the 'username' argument of
the manager's constructor) and additionally by other selectors such as
specification name ('spec_name') and/or form name ('form_name').  Where
'spec_name' and 'form_name' is used, it should follow the conventions for the
relevant parts of DMP fullnames, but the manager doesn't enforce that in any
way.

"""

from pytis.util import translate as _

import base64
import cPickle as pickle

import config
import pytis.data
import pytis.form
import pytis.presentation
import pprint

from pytis.presentation import Profile

class UserSetttingsManager(object):
    """Common base class for all user settings managers."""
    _TABLE = None
    _COLUMNS = ()

    def __init__(self, dbconnection, username=None):
        self._dbconnection = dbconnection
        self._username = username or config.dbuser
        self._data = pytis.data.dbtable(self._TABLE, self._COLUMNS, dbconnection)

    def _values(self, **kwargs):
        return [(key, pytis.data.Value(self._data.find_column(key).type(), value))
                for key, value in [('username', self._username)] + kwargs.items()]

    def _condition(self, **kwargs):
        return pytis.data.AND(*[pytis.data.EQ(key, value)
                                for key, value in self._values(**kwargs)])

    def _row(self, transaction=None, **kwargs):
        try:
            count = self._data.select(condition=self._condition(**kwargs),
                                      transaction=transaction)
            if count == 0:
                row = None
            else:
                assert count == 1
                row = self._data.fetchone(transaction=transaction)
        finally:
            try:
                self._data.close()
            except:
                pass
        return row

    def _rows(self, transaction=None, **kwargs):
        rows = []
        self._data.select(condition=self._condition(**kwargs), transaction=transaction)
        while True:
            row = self._data.fetchone()
            if row is None:
                break
            rows.append(row)
        return rows

    def _pickle(self, value):
        return base64.b64encode(pickle.dumps(value))

    def _unpickle(self, value):
        return pickle.loads(base64.b64decode(value))
    
    def _load(self, transaction=None, pickled_column='pickle', **key):
        row = self._row(transaction=transaction, **key)
        if row:
            return self._unpickle(row[pickled_column].value())
        else:
            return None
    
    def _save(self, values, transaction=None, **key):
        row = self._row(transaction=transaction, **key)
        if row:
            for k, v in self._values(**values):
                row[k] = v
            self._data.update(row['id'], row, transaction=transaction)
        else:
            row = pytis.data.Row(self._values(**key) + self._values(**values))
            result, success = self._data.insert(row, transaction=transaction)
            if not success:
                raise pd.DBException(result)

    def _drop(self, transaction=None, **key):
        row = self._row(transaction=transaction, **key)
        if row:
            self._data.delete(row['id'], transaction=transaction)


class ApplicationConfigManager(UserSetttingsManager):
    """Application configuration storage manager.

    The options are saved in a simple database table as a single string value
    per user containing the pickled sequence of options and values.  The
    methods 'load()' and 'save()' can be used to retrieve and store such
    sequences.

    """
    _TABLE = 'e_pytis_config'
    _COLUMNS = ('id', 'username', 'option', 'value')

    def load(self, transaction=None):
        """Return previously stored configuration options as a tuple of (name, value) pairs."""
        rows = self._rows()
        return [(r['option'].value(), self._unpickle(r['value'].value()))
                for r in rows]
    
    def save(self, config, transaction=None):
        """Store configuration options as a tuple of (name, value) pairs."""
        assert isinstance(config, (tuple, list))
        db_options = dict(self.load())
        inserts = []
        updates = []
        deletes = set(db_options.keys()) - set(dict(config).keys())
        for option in deletes:
            condition = self._condition(option=option)
            self._data.delete_many(condition, transaction=transaction)
        for option, value in config:
            if option in db_options.keys():
                if value != db_options[option]:
                    db_value = self._pickle(value)
                    row = pytis.data.Row(data=(('value', pytis.data.sval(db_value)),))
                    condition = self._condition(option=option)
                    self._data.update_many(condition, row, transaction=transaction)
            else:
                db_value = self._pickle(value)
                row = pytis.data.Row(data=(('username', pytis.data.sval(self._username)),
                                           ('option', pytis.data.sval(option)),
                                           ('value', pytis.data.sval(db_value)),
                                           ))
                result, success = self._data.insert(row, transaction=transaction)
                if not success:
                    raise pd.DBException(result)
                                            
            
class FormSettingsManager(UserSetttingsManager):
    """Accessor of database storage of form settings other than profiles.

    Form settings are those form properties, which don't depend on the current
    profile.  They don't change when a profile is changed (for example dualform
    sash position doesn't depend on the current profile).

    Form settings are simple pairs of string 'option' and a value of some
    primitive python data type, such as 'int', 'str' or 'bool'.  Different form
    types may store different settings (with different names).  The maneger
    internally keeps a dictionary of settings for each form and the methods
    'get()' and 'set()' may be used to retrieve/write individual options.

    """
    _TABLE = 'e_pytis_form_settings'
    _COLUMNS = ('id', 'username', 'spec_name', 'form_name', 'pickle', 'dump')

    def _settings(self, spec_name, form_name, transaction=None):
        settings = self._load(spec_name=spec_name, form_name=form_name, transaction=transaction)
        if settings is None:
            settings = {}
        return settings

    def set(self, spec_name, form_name, option, value, transaction=None):
        """Save value of user specific form configuration option.

        Arguments:
          spec_name -- specification name as a string.
          form_name -- string uniquely identifying the form type.
          option -- string option name.
          value -- option value as an instance of any python primitive data type.

        """
        settings = self._settings(spec_name, form_name, transaction=transaction)
        settings[option] = value
        values = dict(pickle=self._pickle(settings),
                      dump='\n'.join(['%s: %s' % item for item in settings.items()]))
        self._save(values, spec_name=spec_name, form_name=form_name, transaction=transaction)

    def get(self, spec_name, form_name, option, default=None, transaction=None):
        """Return previously saved user specific form configuration option.

        Arguments:
          spec_name -- specification name as a string.
          form_name -- string uniquely identifying the form type.
          option -- string option name.
          default -- default valuee to be used if the option was not previously set for given form.

        """
        settings = self._settings(spec_name, form_name, transaction=transaction)
        return settings.get(option, default)


class FormProfileManager(UserSetttingsManager):
    """Accessor of database storage of form profiles.

    This manager is a little more complicated then the others as it must
    understand the logic of the data it is saving/restoring.  The
    'load_profiles()' method must read profile data from two different sources,
    validate them against the current specification combine them into resulting
    'pytis.presentation.Profile' instances

    Moreover, the filters can not be simply pickled/unpickled as they are.  The
    'pytis.data.Operator' instances often refer to values with pytis data
    types, which have enumerators and other references to ``living'' data
    objects.  Thus we prefer to convert the Operator instances into our own
    simple structure of basic immutable python objects and restore
    'pytis.data.Operator' instances back after unpickling (the form's data
    object is needed to do that).
    
    Forms are referenced by unique string identifiers (see the 'spec_name' and
    'form_name' arguements of the manager's methods).
        
    """
    _TABLE = 'e_pytis_form_profile_base'
    _COLUMNS = ('id', 'username', 'spec_name', 'profile_id', 'title',
                'pickle', 'dump', 'errors')
    _OPERATORS = ('AND','OR','EQ','NE','WM','NW','LT','LE','GT','GE', 'IN') # NOT is not allowed!
    _PROFILE_PARAMS = ('sorting', 'columns', 'grouping', 'folding', 'aggregations', 'column_widths')

    USER_PROFILE_PREFIX = '_user_profile_'
    """Profile identifier prefix used for user defined profiles.

    User defined profiles are recognized from system profiles (defined in
    specifications) by this prefix.
    
    """

    def __init__(self, dbconnection, username=None):
        super(FormProfileManager, self).__init__(dbconnection, username=username)
        self._params_manager = FormProfileParamsManager(dbconnection, username=username)

    def _pack_filter(self, something):
        if isinstance(something, pytis.form.IN):
            return (something.name(), (something.column_id(), something.spec_name(),
                                       something.table_column_id(), something.profile_id()), {})
        elif isinstance(something, pytis.data.Operator):
            args = tuple([self._pack_filter(arg) for arg in something.args()])
            return (something.name(), args, something.kwargs())
        elif isinstance(something, pytis.data.Value):
            t = something.type()
            export_kwargs = {}
            if isinstance(t, pytis.data.Date):
                export_kwargs['format'] = '%Y-%m-%d'
            elif isinstance(t, pytis.data.Time):
                export_kwargs['format'] = '%H:%M:%S'
            elif isinstance(t, pytis.data.DateTime):
                export_kwargs['format'] = '%Y-%m-%d %H:%M:%S'
            elif isinstance(t, pytis.data.Float):
                export_kwargs['locale_format'] = False
            return [something.export(**export_kwargs)]
        elif isinstance(something, pytis.data.WMValue):
            return [something.value()]
        elif isinstance(something, basestring):
            return something
        else:
            raise pytis.util.ProgramError("Unknown object in filter operator:", something)

    def _unpack_filter(self, packed, data_object):
        name, packed_args, kwargs = packed
        if name not in self._OPERATORS:
            raise Exception("Invalid filter operator '%s'." % name)
        op = getattr(pytis.data, name)
        if name in ('AND', 'OR'):
            args = [self._unpack_filter(arg, data_object) for arg in packed_args]
        elif name == 'IN':
            op = pytis.form.IN
            args = packed_args
        else:
            if len(packed_args) != 2:
                raise Exception("Invalid number of filter operator arguments: %s" %
                                repr(packed_args))
            if isinstance(packed_args[1], list):
                col, val = packed_args[0], packed_args[1][0]
                if isinstance(val, str):
                    try:
                        val = val.decode('utf-8')
                    except UnicodeDecodeError:
                        val = val.decode('iso-8859-2')
                column = data_object.find_column(col)
                if column is None:
                    raise Exception("Unknown column '%s'" % col)
                t = column.type()
                if name in ('WM', 'NW'):
                    value, err = t.wm_validate(val)
                else:
                    validation_kwargs = {}
                    if isinstance(t, pytis.data.Date):
                        validation_kwargs['format'] = '%Y-%m-%d'
                    elif isinstance(t, pytis.data.Time):
                        validation_kwargs['format'] = '%H:%M:%S'
                    elif isinstance(t, pytis.data.DateTime):
                        validation_kwargs['format'] = '%Y-%m-%d %H:%M:%S'
                    elif isinstance(t, pytis.data.Float):
                        validation_kwargs['locale_format'] = False
                    value, err = t.validate(val, strict=False, **validation_kwargs)
                if err is not None:
                    raise Exception("Invalid filter operand value for '%s': %s" % (col, err))
                args = col, value
            else:
                args = packed_args
                for col in args:
                    if data_object.find_column(col) is None:
                        raise Exception("Unknown column '%s'" % col)
        return op(*args, **kwargs)

    def _format_item(self, key, value):
        import pprint
        pp = pprint.PrettyPrinter()
        indent = '\n  ' + ' ' * len(key)
        formatted = indent.join(pp.pformat(value).splitlines())
        return '%s: %s' % (key, formatted)
    
    def _dump_filter(self, filter):
        return self._format_item('filter', filter and self._pack_filter(filter))

    def _dump_params(self, params):
        lines = [self._format_item(key, params[key]) for key in self._PROFILE_PARAMS]
        if lines:
            return '\n'.join(lines)
        else:
            return None

    def _errors(self, profile, p):
        errors = ['%s: %s' % (param, error) for param, error in profile.errors() if p(param)]
        if errors:
            return "\n".join(errors)
        else:
            return None

    def _validate_filter(self, data_object, packed_filter):
        if packed_filter:
            try:
                filter = self._unpack_filter(packed_filter, data_object)
            except Exception as e:
                return None, (('filter', str(e)),)
            else:
                return filter, ()
        else:
            return None, ()

    def _validate_params(self, view_spec, params):
        errors = []
        for param, getcol in (('columns', lambda x: x),
                              ('sorting', lambda x: x[0]),
                              ('grouping', lambda x: x)):
            sequence = params.get(param)
            if sequence is not None:
                for x in sequence:
                    col = getcol(x)
                    if view_spec.field(col) is None:
                        errors.append((param, "Unknown column '%s'" % col))
        return tuple(errors)

    def _in_transaction(self, transaction, operation, *args, **kwargs):
        if transaction is None:
            transaction = pytis.data.DBTransactionDefault(self._dbconnection)
            kwargs['transaction'] = transaction
            try:
                operation(*args, **kwargs)
            except:
                try:
                    transaction.rollback()
                except:
                    pass
                raise
            else:
                transaction.commit()
        else:
            kwargs['transaction'] = transaction
            operation(*args, **kwargs)

    def _profile_params(self, profile):
        return dict([(param, getattr(profile, param)()) for param in self._PROFILE_PARAMS])
            
    def save_profile(self, spec_name, form_name, profile, transaction=None):
        """Save user specific configuration of a form.
        
        Arguments:

          spec_name, form_name -- unique string identification of a form to which the
            profile belongs (see 'FormProfileManager' class docuemntation).
          profile -- form profile as a 'pytis.presentation.Profile' instance.
          transaction -- Existing DB transaction if the operation should be
            performed as a part of it.  If None, a local transaction will be
            created if necessary.

        """
        def save_params(transaction):
            params = self._profile_params(profile)
            self._params_manager.save(spec_name, form_name, profile.id(), params,
                                      dump=self._dump_params(params),
                                      errors=self._errors(profile, lambda p: p != 'filter'),
                                      transaction=transaction)
        if profile.id().startswith(self.USER_PROFILE_PREFIX):
            # Save filter and form parameters for user defined profiles.
            def save_profile(transaction):
                filter = profile.filter()
                values = dict(title=profile.title(),
                              pickle=self._pickle(filter and self._pack_filter(filter)),
                              dump=self._dump_filter(filter),
                              errors=self._errors(profile, lambda p: p == 'filter'))
                self._save(values, spec_name=spec_name, profile_id=profile.id(),
                           transaction=transaction)
                save_params(transaction)
            self._in_transaction(transaction, save_profile)
        else:
            # Only save user specific form parameters for system profiles.
            save_params(transaction)

    def load_profiles(self, spec_name, form_name, view_spec, data_object, default_profile,
                      transaction=None):
        """Return list of form profiles including previously saved user customizations.

        Arguments:
          spec_name -- string specification name
          form_name -- unique string form identification
          view_spec -- ViewSpec instance of given specification to be used for
            profile validation
          data_object -- data object of given specification to be used for
            profile validation
          default_profile -- 'pytis.presentation.Profile' instance representing
            form's default profile

        Returns a list of 'pytis.presentation.Profile' instances including
        predefined system profiles (possibly with their user customizations) as
        well as user defined profiles.

        """
        def load_params(profile_id):
            params = self._params_manager.load(spec_name, form_name, profile_id, transaction=transaction)
            errors = self._validate_params(view_spec, params)
            return params, errors
        profiles = []
        # Load user customizations of system profiles first.
        for profile in (default_profile,) + tuple(view_spec.profiles()):
            # System profiles define the title and filter, which can not be
            # changed, so we only load saved form parameters if they are valid.
            # One of the reasons why filter of system profiles can not be
            # changed is that it often contains dynamic conditions, such as
            # EQ('date', now()) which are destroyed when saved (the saved
            # condition would be EQ('date', '2011-03-01') for example).
            params, errors = load_params(profile.id())
            if not errors:
                for param, value in self._profile_params(profile).items():
                    if params.get(param) is None:
                        params[param] = value
                profile = Profile(profile.id(), profile.title(), filter=profile.filter(), **params)
            profiles.append(profile)
        # Now load also user defined profiles.
        for row in self._rows(spec_name=spec_name, transaction=transaction):
            packed_filter = pickle.loads(base64.b64decode(row['pickle'].value()))
            filter, filter_errors = self._validate_filter(data_object, packed_filter)
            params, param_errors = load_params(row['profile_id'].value())
            profiles.append(Profile(row['profile_id'].value(),
                                    row['title'].value(),
                                    filter=filter,
                                    errors=filter_errors + param_errors,
                                    **params))
        return profiles
           
    def load_filter(self, spec_name, data_object, profile_id, transaction=None):
        """Return a previously saved user defined filter.

        Arguments:
          spec_name -- string form specification name
          data_object -- data object of given specification to be used for filter validation
          profile_id -- string identifier of the profile
          transaction -- Existing DB transaction if the operation should be
            performed as a part of it.  If None, a local transaction will be
            created if necessary.

        Returns a tuple (filter, title), where filter is a
        'pytis.data.Operator' instance or None when given profile exists, but
        has no filter and title is the user defined title of the profile.

        Raises exception if no such profile is found or is invalid.

        """
        row = self._row(transaction=transaction, spec_name=spec_name, profile_id=profile_id)
        if row:
            packed_filter = pickle.loads(base64.b64decode(row['pickle'].value()))
            filter, errors = self._validate_filter(data_object, packed_filter)
            if errors:
                raise Exception("Saved profile %s for %s is invalid!" % (profile_id, spec_name))
            return filter, row['title'].value()
        else:
            raise Exception("Saved profile %s for %s doesn't exist!" % (profile_id, spec_name))
           
    def drop_profile(self, spec_name, form_name, profile_id, transaction=None):
        """Remove the previously saved form configuration.

        Arguments:
          spec_name -- string specification name
          form_name -- unique string form identification
          profile_id -- string identifier of the profile to drop

        """
        if profile_id.startswith(self.USER_PROFILE_PREFIX):
            # Drop filter and form parameters for user defined profiles.
            def drop(spec_name, form_name, profile_id, transaction):
                self._drop(spec_name=spec_name, profile_id=profile_id, transaction=transaction)
                self._params_manager.drop(spec_name, form_name, profile_id, transaction=transaction)
            self._in_transaction(transaction, drop, spec_name, form_name, profile_id)
        else:
            # Only drop user specific form parameters for system profiles.
            self._params_manager.drop(spec_name, form_name, profile_id, transaction=transaction)
        
    def list_spec_names(self, transaction=None):
        """Return a sequence of distinct specification names for which profiles were saved."""
        values = self._data.distinct('spec_name', condition=self._condition(),
                                     transaction=transaction)
        return [v.value() for v in values]
    
    def list_form_names(self, spec_name, transaction=None):
        """Return a sequence of distinct form names for which profiles were saved."""
        return self._params_manager.list_form_names(spec_name, transaction=transaction)

    def new_user_profile_id(self, profiles):
        """Generate a new unique user profile id based on given list of existing profiles."""
        prefix = self.USER_PROFILE_PREFIX
        user_profile_numbers = [int(profile.id()[len(prefix):]) for profile in profiles
                                if profile.id().startswith(prefix)
                                and profile.id()[len(prefix):].isdigit()]
        return prefix + str(max(user_profile_numbers+[0])+1)

    
class FormProfileParamsManager(UserSetttingsManager):
    """Accessor of database storage of form profile parameters.

    This manager is only used internally by FormProfileManager to retrieve form
    specific profile parameters which are then combined with form independent
    profile parameters (filter).

    """
    _TABLE = 'e_pytis_form_profile_params'
    _COLUMNS = ('id', 'username', 'spec_name', 'profile_id', 'form_name', 'pickle', 'dump', 'errors')

    def load(self, spec_name, form_name, profile_id, transaction=None):
        """Return previously stored form profile parameters dictionary."""
        return self._load(spec_name=spec_name, form_name=form_name, profile_id=profile_id,
                          transaction=transaction) or {}
    
    def list_form_names(self, spec_name, transaction=None):
        """Return a sequence of form names for which profiles were saved."""
        condition = self._condition(spec_name=spec_name)
        values = self._data.distinct('form_name', condition=condition, transaction=transaction)
        return [v.value() for v in values] 

    def save(self, spec_name, form_name, profile_id, params, dump, errors, transaction=None):
        """Save form profile parameters dictionary."""
        assert isinstance(params, dict)
        values = dict(pickle=self._pickle(params),
                      dump=dump,
                      errors=errors)
        self._save(values, spec_name=spec_name, form_name=form_name, profile_id=profile_id,
                   transaction=transaction)
    
    def drop(self, spec_name, form_name, profile_id, transaction=None):
        """Remove the previously saved form parameters.

        Arguments:
          spec_name, form_name -- unique string identification of a form to which the
            profile belongs (see 'FormProfileManager' class docuemntation).
          profile_id -- string identifier of the profile to drop.

        """
        self._drop(spec_name=spec_name, form_name=form_name, profile_id=profile_id,
                   transaction=transaction)

        
class AggregatedViewsManager(UserSetttingsManager):
    """Accessor of database storage of saved aggregation form setups.

    Aggregation form setups are the properties defined by the user in the
    aggregation form setup dialog and represented by a
    'pytis.presentation.AggregatedView' instance.  Users may create several
    named setups for each specification and these setups will appear as
    separate items in the aggregarion menu in a form toolbar.  instance and are
    related to a specification, so all forms above given specification share
    the list of available aggregation setups.
        
    """
    _TABLE = 'e_pytis_aggregated_views'
    _COLUMNS = ('id', 'username', 'spec_name', 'aggregated_view_id', 'title', 'pickle')

    def save(self, spec_name, aggregated_view, transaction=None):
        """Save aggregation form setup.
        
        Arguments:

          spec_name -- specification name as a string.
          aggregated_view -- 'pytis.presentation.AggregatedView' instance.

        """
        assert isinstance(aggregated_view, pytis.presentation.AggregatedView)
        values = dict(title=aggregated_view.name(),
                      pickle=self._pickle(aggregated_view))
        self._save(values, spec_name=spec_name, aggregated_view_id=aggregated_view.id(),
                   transaction=transaction)

    def load(self, spec_name, aggregated_view_id, transaction=None):
        """Return previously saved aggregated view setup.

        Arguments:
          spec_name -- specification name as a string.
          aggregated_view_id -- string identifier of the aggregated view to load.

        Returns a 'pytis.presentation.AggregatedView' instance.  If no such aggregated
        view is found, None is returned.

        """
        return self._load(spec_name=spec_name, aggregated_view_id=aggregated_view_id,
                          transaction=transaction)

    def drop(self, spec_name, aggregated_view_id, transaction=None):
        """Remove the previously saved aggregated view setup.

        Arguments:
          spec_name -- specification name as a string.
          profile_id -- string identifier of the profile to drop.

        """
        row = self._row(spec_name=spec_name, aggregated_view_id=aggregated_view_id)
        if row:
            self._data.delete(row['id'], transaction=transaction)

    def list(self, spec_name, transaction=None):
        """Return a sequence of all previously saved setups for given specification.

        Arguments:
          spec_name -- specification name as a string.

        Returns a sequence of strings -- all distinct aggregated view
        identifiers previously saved using 'save()' for given 'spec_name'.

        """
        return tuple(row['aggregated_view_id'].value()
                     for row in self._rows(spec_name=spec_name, transaction=transaction))


