# -*- coding: utf-8 -*-

# Copyright (C) 2018-2024 Tomáš Cerha <t.cerha@gmail.com>
# Copyright (C) 2011-2017 OUI Technology Ltd.
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
accessible from within pytis through its properties, such as
'Application.profile_manager', etc.

Most of the settings are stored as JSON objects, but the API works with pytis
classes, such as 'pytis.presentation.Profile()'.  Conversion to/from these
classes is done internally by the manager.

Settings are always identified by the current user (the 'username' argument of
the manager's constructor) and additionally by other selectors such as
specification name ('spec_name') and/or form name ('form_name').  Where
'spec_name' and 'form_name' is used, it should follow the conventions for the
relevant parts of DMP fullnames, but the manager doesn't enforce that in any
way.

"""

from past.builtins import basestring

try:
    import cPickle as pickle
except ImportError:
    import pickle

import base64
import sys
import pytis.data
import pytis.form
import pytis.presentation

from pytis.presentation import Profile
from pytis.util import log, OPERATIONAL


class UserSetttingsManager(object):
    """Common base class for all user settings managers."""
    _TABLE = None
    _COLUMNS = ()

    def __init__(self, dbconnection, username=None):
        log(OPERATIONAL, "Initializing on {}({}).".format(self._TABLE, ', '.join(self._COLUMNS)))
        self._dbconnection = dbconnection
        self._username = username or pytis.config.dbuser
        self._data = pytis.data.dbtable(self._TABLE, self._COLUMNS, dbconnection)

    def _values(self, **kwargs):
        return [(key, pytis.data.Value(self._data.find_column(key).type(), value))
                for key, value in [('username', self._username)] + list(kwargs.items())]

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
                row = self._data.fetchone()
        finally:
            try:
                self._data.close()
            except Exception:
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
        self._data.close()
        return rows

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
                raise pytis.data.DBException(result)

    def _drop(self, transaction=None, **key):
        row = self._row(transaction=transaction, **key)
        if row:
            self._data.delete(row['id'], transaction=transaction)


class ApplicationConfigManager(UserSetttingsManager):
    """Application configuration storage manager.

    The options are saved in a simple database table as JSON objects, one per
    user.  The methods 'load()' and 'save()' can be used to retrieve and store
    the settings as Python dictionaries of options.

    """
    _TABLE = 'e_pytis_config'
    _COLUMNS = ('id', 'username', 'options')

    def load(self, transaction=None):
        """Return previously stored configuration as a dictionary of options."""
        row = self._row(transaction=transaction)
        if row:
            return row['options'].value()
        else:
            return {}

    def save(self, options, transaction=None):
        """Store given configuration options (dict instance)."""
        assert isinstance(options, dict)
        self._save(dict(options=options), transaction=transaction)


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
    _COLUMNS = ('id', 'username', 'spec_name', 'form_name', 'settings')

    def _settings(self, spec_name, form_name, transaction=None):
        row = self._row(spec_name=spec_name, form_name=form_name, transaction=transaction)
        if row:
            return row['settings'].value()
        else:
            return {}

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
        self._save(dict(settings=settings), spec_name=spec_name, form_name=form_name,
                   transaction=transaction)

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


class FormProfileParamsManager(UserSetttingsManager):
    """Accessor of database storage of form profile parameters.

    This manager is only used internally by FormProfileManager to retrieve form
    specific profile parameters which are then combined with form independent
    profile parameters (filter).

    """
    _TABLE = 'e_pytis_form_profile_params'
    _COLUMNS = ('id', 'username', 'spec_name', 'profile_id', 'form_name', 'params', 'errors')

    def load(self, spec_name, form_name, profile_id, transaction=None):
        """Return previously stored form profile parameters dictionary."""
        row = self._row(spec_name=spec_name, form_name=form_name, profile_id=profile_id,
                        transaction=transaction)
        if row:
            return row['params'].value()
        else:
            return {}

    def list_spec_names(self, transaction=None):
        """Return a sequence of distinct specification names for which profiles were saved."""
        values = self._data.distinct('spec_name', condition=self._condition(),
                                     transaction=transaction)
        return set(v.value() for v in values)

    def list_form_names(self, spec_name, transaction=None):
        """Return a sequence of form names for which profiles were saved."""
        condition = self._condition(spec_name=spec_name)
        values = self._data.distinct('form_name', condition=condition, transaction=transaction)
        return [v.value() for v in values]

    def save(self, spec_name, form_name, profile_id, params, errors, transaction=None):
        """Save form profile parameters dictionary."""
        assert isinstance(params, dict)
        self._save(dict(params=params, errors=errors),
                   spec_name=spec_name, form_name=form_name, profile_id=profile_id,
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


class FormProfileManager(UserSetttingsManager):
    """Accessor of database storage of form profiles.

    This manager is a little more complicated then the others as it must
    understand the logic of the data it is saving/restoring.  The
    'load_profiles()' method must read profile data from two different sources,
    validate them against the current specification combine them into resulting
    'pytis.presentation.Profile' instances

    The key aspect of this class is serialization of pytis filters
    ('pytis.data.Operator' instances).  The Operator instances ale converted
    (packed/unpacked) to/from our own structure of basic Python/JSON data
    types.

    Forms are referenced by unique string identifiers (see the 'spec_name' and
    'form_name' arguements of the manager's methods).

    """
    _TABLE = 'e_pytis_form_profile_base'
    _COLUMNS = ('id', 'username', 'spec_name', 'profile_id', 'title', 'filter', 'errors')
    _OPERATORS = ('AND', 'OR', 'NOT', 'EQ', 'NE', 'WM', 'NW', 'LT', 'LE', 'GT', 'GE', 'IN')
    _PROFILE_PARAMS = ('sorting', 'columns', 'grouping', 'folding', 'aggregations',
                       'column_widths')

    USER_PROFILE_PREFIX = '_user_profile_'
    """Profile identifier prefix used for user defined profiles.

    User defined profiles are recognized from system profiles (defined in
    specifications) by this prefix.

    """
    _PARAMS_MANAGER_CLASS = FormProfileParamsManager

    def __init__(self, dbconnection, username=None):
        super(FormProfileManager, self).__init__(dbconnection, username=username)
        self._params_manager = self._PARAMS_MANAGER_CLASS(dbconnection, username=username)

    def _pack_filter(self, something):
        if isinstance(something, pytis.presentation.IN):
            args = list(something.original_args())
            # Pack the additonal condition of pytis.presentation.IN.
            if args[-2]:
                args[-2] = self._pack_filter(args[-2])
            # DB function arguments passed of pytis.presentation.IN.
            if args[-1]:
                log(OPERATIONAL, "Ignoring arguments passed to pytis.presentation.IN:", args[-1])
                args[-1] = {}
            return [something.name(), args, {}]
        elif isinstance(something, pytis.data.Operator):
            args = [self._pack_filter(arg) for arg in something.args()]
            return [something.name(), args, something.kwargs()]
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

    def _unpack_filter(self, packed, data_object, delete_columns=(), rename_columns={}):
        def find_column(col):
            col = rename_columns.get(col, col)
            column = data_object.find_column(col)
            if column is None:
                note = " (can't remove from filter)" if col in delete_columns else ""
                raise Exception("Unknown column '%s'%s" % (col, note))
            return column
        name, packed_args, kwargs = packed
        if name not in self._OPERATORS:
            raise Exception("Invalid filter operator '%s'." % name)
        op = getattr(pytis.data, name)
        if name in ('AND', 'OR', 'NOT'):
            args = [self._unpack_filter(arg, data_object, delete_columns=delete_columns,
                                        rename_columns=rename_columns)
                    for arg in packed_args]
        elif name == 'IN':
            op = pytis.form.make_in_operator
            args = packed_args
            spec_name, condition = args[1], args[-2]
            # Unpack the additonal condition of pytis.presentation.IN.
            if condition:
                args[-2] = self._unpack_filter(condition, pytis.util.data_object(spec_name))
        else:
            if len(packed_args) != 2:
                raise Exception("Invalid number of filter operator arguments: %s" %
                                repr(packed_args))
            if isinstance(packed_args[1], list):
                column, val = find_column(packed_args[0]), packed_args[1][0]
                if isinstance(val, str if sys.version_info[0] == 2 else bytes):
                    # TODO: This is probably completely redundant in Python 3...
                    try:
                        val = val.decode('utf-8')
                    except UnicodeDecodeError:
                        val = val.decode('iso-8859-2')
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
                    raise Exception("Invalid filter operand for '%s': %s" % (column.id(), err))
                args = column.id(), value
            else:
                args = tuple([find_column(col).id() for col in packed_args])
        return op(*args, **kwargs)

    def _errors(self, profile, p):
        errors = ['%s: %s' % (param, error) for param, error in profile.errors() if p(param)]
        if errors:
            return "\n".join(errors)
        else:
            return None

    def _load_filter(self, data_object, packed_filter, delete_columns=(), rename_columns={}):
        if packed_filter:
            try:
                filter = self._unpack_filter(packed_filter, data_object,
                                             delete_columns=delete_columns,
                                             rename_columns=rename_columns)
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
                    f = view_spec.field(col)
                    if f is None:
                        errors.append((param, "Unknown column '%s'" % col))
                    elif param == 'columns' and f.disable_column():
                        errors.append((param, "Disabled column '%s'" % col))
        return tuple(errors)

    def _update_params(self, params, rename_columns, delete_columns):
        for param, getcol, getitem in (('columns', lambda x: x, lambda x, col: col),
                                       ('sorting', lambda x: x[0], lambda x, col: (col, x[1])),
                                       ('grouping', lambda x: x, lambda x, col: col)):
            items = params.get(param)
            if items is not None:
                params[param] = tuple(
                    getitem(item, rename_columns.get(col, col))
                    for item, col in [(x, getcol(x)) for x in items]
                    if col not in delete_columns
                )

    def _in_transaction(self, transaction, operation, *args, **kwargs):
        if transaction is None:
            transaction = pytis.data.DBTransactionDefault(self._dbconnection)
            kwargs['transaction'] = transaction
            try:
                operation(*args, **kwargs)
            except Exception:
                try:
                    transaction.rollback()
                except Exception:
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
                                      errors=self._errors(profile, lambda p: p != 'filter'),
                                      transaction=transaction)
        if profile.id().startswith(self.USER_PROFILE_PREFIX):
            # Save filter and form parameters for user defined profiles.
            def save_profile(transaction):
                self._save(dict(title=profile.title(),
                                filter=profile.filter() and self._pack_filter(profile.filter()),
                                errors=self._errors(profile, lambda p: p == 'filter')),
                           spec_name=spec_name, profile_id=profile.id(), transaction=transaction)
                save_params(transaction)
            self._in_transaction(transaction, save_profile)
        else:
            # Only save user specific form parameters for system profiles.
            save_params(transaction)

    def load_profiles(self, spec_name, form_name, view_spec, data_object, default_profile,
                      transaction=None, rename_columns={}, delete_columns=()):
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
          rename_columns -- dictionary to use for renaming columns in all
            loaded profiles.  Keys are the column identifiers to rename and
            values are the new column identifiers.
          delete_columns -- sequence of column ids to drop from all loaded profiles

        Returns a list of 'pytis.presentation.Profile' instances including
        predefined system profiles (possibly with their user customizations) as
        well as user defined profiles.

        """
        def load_params(profile_id):
            params = self._params_manager.load(spec_name, form_name, profile_id,
                                               transaction=transaction)
            if rename_columns or delete_columns:
                self._update_params(params, rename_columns, delete_columns)
            errors = self._validate_params(view_spec, params)
            return params, errors
        profiles = []
        # Load user customizations of system profiles first.
        for profile in (default_profile,) + tuple(view_spec.profiles().unnest()):
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
            filter_, filter_errors = self._load_filter(data_object, row['filter'].value(),
                                                       delete_columns=delete_columns,
                                                       rename_columns=rename_columns)
            params, param_errors = load_params(row['profile_id'].value())
            profiles.append(Profile(row['profile_id'].value(),
                                    row['title'].value(),
                                    filter=filter_,
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
            filter_, errors = self._load_filter(data_object, row['filter'].value())
            if errors:
                raise Exception("Saved profile %s for %s is invalid!" % (profile_id, spec_name))
            return filter_, row['title'].value()
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
        return set(v.value() for v in values).union(
            self._params_manager.list_spec_names(transaction=transaction)
        )

    def list_form_names(self, spec_name, transaction=None):
        """Return a sequence of distinct form names for which profiles were saved."""
        return self._params_manager.list_form_names(spec_name, transaction=transaction)

    def new_user_profile_id(self, profiles):
        """Generate a new unique user profile id based on given list of existing profiles."""
        prefix = self.USER_PROFILE_PREFIX
        user_profile_numbers = [int(profile.id()[len(prefix):]) for profile in profiles
                                if (profile.id().startswith(prefix) and
                                    profile.id()[len(prefix):].isdigit())]
        return prefix + str(max(user_profile_numbers + [0]) + 1)


class AggregatedViewsManager(UserSetttingsManager):
    """Accessor of database storage of saved aggregation form setups.

    Aggregation form setups are the properties defined by the user in the
    aggregation form setup dialog and represented by a
    'pytis.presentation.AggregatedView' instance.  Users may create several
    named setups for each specification and these setups will appear as
    separate items in the aggregation menu in a form toolbar.  Aggregated views
    are related to a specification, so all forms above given 'spec_name' share
    the list of available aggregation setups.

    """
    _TABLE = 'e_pytis_aggregated_views'
    _COLUMNS = ('id', 'username', 'spec_name', 'aggregated_view_id', 'params')

    def save(self, spec_name, aggregated_view, transaction=None):
        """Save aggregation form setup.

        Arguments:

          spec_name -- specification name as a string.
          aggregated_view -- 'pytis.presentation.AggregatedView' instance.

        """
        assert isinstance(aggregated_view, pytis.presentation.AggregatedView)
        self._save(dict(params=dict(
            name=aggregated_view.name(),
            group_by_columns=aggregated_view.group_by_columns(),
            aggregation_columns=aggregated_view.aggregation_columns(),
        )), spec_name=spec_name, aggregated_view_id=aggregated_view.id(), transaction=transaction)

    def load(self, spec_name, aggregated_view_id, transaction=None):
        """Return previously saved aggregated view setup.

        Arguments:
          spec_name -- specification name as a string.
          aggregated_view_id -- string identifier of the aggregated view to load.

        Returns a 'pytis.presentation.AggregatedView' instance.  If no such aggregated
        view is found, None is returned.

        """
        row = self._row(spec_name=spec_name, aggregated_view_id=aggregated_view_id,
                        transaction=transaction)
        if row:
            params = row['params'].value()
            return pytis.presentation.AggregatedView(
                id=aggregated_view_id,
                name=params['name'],
                group_by_columns=tuple(tuple(x) for x in params['group_by_columns']),
                aggregation_columns=tuple(tuple(x) for x in params['aggregation_columns']),
            )
        else:
            return None


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
