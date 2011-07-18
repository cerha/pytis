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

            
class FormProfileManager(UserSetttingsManager):
    """Accessor of database storage of form profiles.

    The actual form profile data are python dictionaries of arbitrary form
    settings at this level.  There are no rules, except that the data structure
    must be safe to pickle and unpickle (ideally they should consist just of
    basic python data types).  They are referenced by a string identifier and
    the upper layer is responsible for converting these structures into
    'pytis.presentation.Profile' instances.

    Forms are referenced by unique string identifiers (see the 'fullname'
    arguement of the manager's methods).  This string should follow the same
    structure as DMP fullnames by convention, but the manager doesn't enforce
    that in any way.
        
    """
    _TABLE = 'e_pytis_form_profiles'
    _COLUMNS = ('id', 'username', 'fullname', 'profile_id', 'title',
                'pickle', 'dump', 'errors')

    def save_profile(self, fullname, profile, transaction=None):
        """Save user specific configuration of a form.
        
        Arguments:

          fullname -- unique string identification of a form to which the
            profile belongs (see 'FormProfileManager' class docuemntation).
          profile -- form profile as a 'pytis.form.FormProfile' instance.
          config -- dictionary of form configuration parameters.

        """
        values = dict(title=profile.name(),
                      pickle=self._pickle(profile),
                      dump=profile.dump(),
                      errors="\n".join(profile.validation_errors()) or None)
        self._save(values, fullname=fullname, profile_id=profile.id(), transaction=transaction)

    def load_profile(self, fullname, profile_id, transaction=None):
        """Return previously saved user specific configuration of a form.

        Arguments:
          fullname -- unique string identification of a form to which the
            profile belongs (see 'FormProfileManager' class docuemntation).
          profile_id -- string identifier of the profile to load.

        Returns a 'pytis.form.FormProfile' instance.  If no such profile is
        found or if a problem occures reading it, None is returned.

        """
        return self._load(fullname=fullname, profile_id=profile_id, transaction=transaction)
           
    def drop_profile(self, fullname, profile_id, transaction=None):
        """Remove the previously saved form configuration.

        Arguments:
          fullname -- unique string identification of a form to which the
            profile belongs (see 'FormProfileManager' class docuemntation).
          profile_id -- string identifier of the profile to drop.

        """
        self._drop(fullname=fullname, profile_id=profile_id)
        
    def list_profile_ids(self, fullname, transaction=None):
        """Return a sequence of identifiers of all previously saved profiles.

        Arguments:
          fullname -- unique string identification of a form to which the
            profile belongs (see 'FormProfileManager' class docuemntation).

        Returns a sequence of strings -- all distinct profile identifiers
        previously saved using 'save_profile' for given 'fullname'.

        """
        return tuple(row['profile_id'].value()
                     for row in self._rows(fullname=fullname, transaction=transaction))

    def list_fullnames(self, pattern=None, transaction=None):
        """Return a sequence form fullnames for which profiles were saved.

        Arguments:
          pattern -- wildcard pattern (using * and ? in their usual meaning)
            to match the returned fullnames.  If None, all previously saved
            fullnames for given user are returned.

        """
        condition = pytis.data.EQ('username', pytis.data.Value(pytis.data.String(), self._username))
        if pattern:
            wm = pytis.data.WM('fullname', pytis.data.WMValue(pytis.data.String(), pattern),
                               ignore_case=False)
            condition = pytis.data.AND(condition, wm)
        values = self._data.distinct('fullname', condition=condition, transaction=transaction)
        return [v.value() for v in values]

    
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


