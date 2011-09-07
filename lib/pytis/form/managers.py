# -*- coding: utf-8 -*-

# Copyright (C) 2011 Brailcom, o.p.s.
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

    def _load(self, transaction=None, pickled_column='pickle', **key):
        row = self._row(transaction=transaction, **key)
        if row:
            return pickle.loads(base64.b64decode(row[pickled_column].value()))
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
            self._data.insert(row, transaction=transaction)

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
    _COLUMNS = ('id', 'username', 'pickle')

    def load(self, transaction=None):
        """Return previously stored configuration options as a tuple of (name, value) pairs."""
        return self._load(transaction=transaction) or ()
           
    def save(self, config, transaction=None):
        """Return previously stored configuration options as a tuple of (name, value) pairs."""
        assert isinstance(config, (tuple, list))
        self._save(dict(pickle=self._pickle(tuple(config))), transaction=transaction)

            
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
    'load_profile()' method must read profile data from two different sources,
    validate them against the current specification
    join them into a resulting 'pytis.presentation.Profile' instance

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
    _TABLE = 'e_pytis_form_profiles'
    _COLUMNS = ('id', 'username', 'spec_name', 'profile_id', 'title',
                'pickle', 'dump', 'errors')
    _OPERATORS = ('AND','OR','EQ','NE','WM','NW','LT','LE','GT','GE') # NOT is not allowed!
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
        if isinstance(something, pytis.data.Operator):
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

    def _unpack_filter(self, packed, view, data):
        name, packed_args, kwargs = packed
        if name not in self._OPERATORS:
            raise Exception("Invalid filter operator '%s'." % name)
        op = getattr(pytis.data, name)
        if name in ('AND', 'OR'):
            args = [self._unpack_filter(arg, view, data) for arg in packed_args]
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
                column = data.find_column(col)
                if column is None:
                    raise Exception("Unknown column '%s' in filter." % col)
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
                    if data.find_column(col) is None:
                        raise Exception("Unknown column '%s' in filter." % col)
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

    def _validate_filter(self, packed_filter, view, data):
        if packed_filter:
            try:
                filter = self._unpack_filter(packed_filter, view, data)
            except Exception as e:
                return None, (('filter', str(e)),)
            else:
                return filter, ()
        else:
            return None, ()

    def _validate_params(self, params, view):
        errors = []
        for param, getcol in (('columns', lambda x: x),
                              ('sorting', lambda x: x[0]),
                              ('grouping', lambda x: x)):
            sequence = params.get(param)
            if sequence is not None:
                for x in sequence:
                    col = getcol(x)
                    if view.field(col) is None:
                        errors.append((param, "Unknown column '%s'." % col))
        return tuple(errors)
        
    def save_profile(self, spec_name, form_name, profile, transaction=None):
        """Save user specific configuration of a form.
        
        Arguments:

          spec_name, form_name -- unique string identification of a form to which the
            profile belongs (see 'FormProfileManager' class docuemntation).
          profile -- form profile as a 'pytis.presentation.Profile' instance.
          config -- dictionary of form configuration parameters.

        """
        # TODO: Force transaction!
        # if transaction is None: transaction = pd.DBTransactionDefault(self._dbconnection)
        filter = profile.filter()
        values = dict(title=profile.name(),
                      pickle=self._pickle(filter and self._pack_filter(filter)),
                      dump=self._dump_filter(filter),
                      errors=self._errors(profile, lambda p: p == 'filter'))
        self._save(values, spec_name=spec_name, profile_id=profile.id(), transaction=transaction)
        params = dict([(param, getattr(profile, param)()) for param in self._PROFILE_PARAMS])
        self._params_manager.save(spec_name, form_name, profile.id(), params,
                                  dump=self._dump_params(params),
                                  errors=self._errors(profile, lambda p: p != 'filter'),
                                  transaction=transaction)

    def load_profile(self, spec_name, form_name, view, data, profile_id, filter=None,
                     transaction=None):
        """Return previously saved user specific configuration of a form.

        Arguments:
          spec_name, form_name -- unique string identification of a form to which the
            profile belongs (see 'FormProfileManager' class docuemntation).
          profile_id -- string identifier of the profile to load.

        Returns a 'pytis.presentation.Profile' instance.  If no such profile is
        found or if a problem occures reading it, None is returned.

        """
        row = self._row(transaction=transaction, spec_name=spec_name, profile_id=profile_id)
        if row:
            if filter is None:
                packed_filter = pickle.loads(base64.b64decode(row['pickle'].value()))
                filter, errors = self._validate_filter(packed_filter, view, data)
            else:
                errors = ()
            params = self._params_manager.load(spec_name, form_name, profile_id)
            errors += self._validate_params(params, view)
            return Profile(profile_id, row['title'].value(), filter=filter, errors=errors, **params)
        return None
           
    def drop_profile(self, spec_name, form_name, profile_id, transaction=None):
        """Remove the previously saved form configuration.

        Arguments:
          spec_name, form_name -- unique string identification of a form to which the
            profile belongs (see 'FormProfileManager' class docuemntation).
          profile_id -- string identifier of the profile to drop.

        """
        self._drop(spec_name=spec_name, profile_id=profile_id)
        
    def list_profile_ids(self, spec_name, transaction=None):
        """Return a sequence of identifiers of all previously saved profiles.

        Arguments:
          spec_name -- unique string identification of a form to which the
            profile belongs (see 'FormProfileManager' class docuemntation).

        Returns a sequence of strings -- all distinct profile identifiers
        previously saved using 'save_profile' for given 'spec_name'.

        """
        return tuple(row['profile_id'].value()
                     for row in self._rows(spec_name=spec_name, transaction=transaction))

    def list_spec_names(self, transaction=None):
        """Return a sequence form spec_names for which profiles were saved."""
        condition = self._condition()
        values = self._data.distinct('spec_name', condition=condition, transaction=transaction)
        return [v.value() for v in values]

    def list_form_names(self, spec_name, transaction=None):
        """Return a sequence of form names for which profiles were saved."""
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


