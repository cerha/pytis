# -*- coding: utf-8 -*-

# Copyright (C) 2018-2026 Tomáš Cerha <t.cerha@gmail.com>
# Copyright (C) 2001-2017 OUI Technology Ltd.
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
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

"""Implementation of the Pytis application API available without a user interface.

The public API which the Pytis applications rely on is defined by the sibling
module `api`.  Its implementation belongs to the classes representing the
particular user interface elements, such as `pytis.form.Application` for the wx
user interface.

`BaseApplication` defined here implements the part of the `Application` API which
works without any user interface at all.  It is used directly in scripts (where
`pytis.api.app` creates it automatically) and as a base class of the user
interface application classes.

"""

from __future__ import print_function
from __future__ import unicode_literals
from __future__ import absolute_import

import sys

import pytis.data as pd
import pytis.util

from pytis.presentation import Specification

# Import from the sibling module rather than from the package, which is not
# fully initialized yet when this module is imported from its init file.
from .api import Application, implements
from pytis.util import log, OPERATIONAL

try:
    from typing import (Any, IO, Mapping, Optional, Union)  # noqa: F401
except ImportError:
    pass


@implements(Application, partial=('echo', 'has_access', 'param', 'printout'))
class BaseApplication(object):
    """Base class for classes implementing the `Application` API.

    This class only implements the API members which work without a user
    interface -- they are listed in the `implements` decorator above and
    accessing any other member of the API raises `AttributeError`.  It may be
    used as a base class for other classes implementing the full `Application`
    API and its instance is available as a fallback app instance in scripts.

    Without the fallback instance, it would simply not be possible to even load
    specifications through the resolver in scripts (when the wx app is not
    running) because `app.param` and `app.has_access` are often used in
    specification construction methods.

    """
    class _Param:
        """Implementation of the `Params` API (access items as attributes)."""

        def __init__(self, items):
            self.__dict__.update(items)

        def __getattr__(self, name):
            raise AttributeError("The application defines no shared parameters '{}'"
                                 .format(name))

    def __init__(self):
        import pytis
        import pytis.api
        self._specification = pytis.config.resolver.specification('Application')
        # Create DBParams instances for all SharedParams specifications.
        self._param = self._Param(
            (item.name(), pytis.util.DBParams(item.spec_name(), item.condition()))
            for item in self._specification.params()
        )
        pytis.api.app.init(self)
        self._access_rights = None
        self._access_rights_initialized = False
        self._user_roles = ()
        super(BaseApplication, self).__init__()

    def _init_access_rights(self):  # type: () -> None
        """Read application access rights from the database."""
        # Must be called very early after start of an application.
        self._access_rights_initialized = True
        if not pytis.config.use_dmp_roles:
            return
        try:
            roles_data = pd.dbtable('ev_pytis_user_roles', ('roleid',), pytis.config.dbconnection)
            roles = [row[0].value() for row in roles_data.select_map(pytis.util.identity)]
        except pd.DBException:
            return
        if not roles:
            self._access_rights = 'nonuser'
            return
        self._user_roles = roles
        if not pytis.config.use_dmp_rights:
            return
        self._access_rights = {}
        # Prefill self._access_rights so that default access by specification rights in
        # has_action_access is possible only for shortnames without any rights
        # defined in DMP.
        actions_data = pd.dbtable('e_pytis_action_rights', ('shortname', 'status',),
                                  pytis.config.dbconnection)
        condition = pd.LE('status', pd.ival(0))
        for value in actions_data.distinct('shortname', condition=condition):
            self._access_rights[value.value()] = {}
        rights_data = pd.dbtable('pytis_view_user_rights', (('shortname', pd.String()),
                                                            ('rights', pd.String()),
                                                            ('columns', pd.String())),
                                 pytis.config.dbconnection, arguments=())

        def process(row):
            shortname, rights_string, columns_string = [row[i].value() for i in (0, 1, 2)]
            if columns_string:
                columns = columns_string.split(' ')
            else:
                columns = [None]
            rights = [r.upper() for r in rights_string.split(' ') if r != 'show']
            action_rights = self._access_rights[shortname] = self._access_rights.get(shortname, {})
            relaxed_action_rights = action_rights.get(True)
            if relaxed_action_rights is None:
                # Relaxed access rights are access rights to the action as a whole.
                # The action is accessible if it is accessible itself or if any of
                # its columns is accessible.
                action_rights[True] = relaxed_action_rights = []
            for c in columns:
                action_rights[c] = rights
                for r in rights:
                    if r not in relaxed_action_rights:
                        relaxed_action_rights.append(r)
        rights_data.select_map(process)
        Specification._init_access_rights(pytis.config.dbconnection)
        pytis.config.resolver.clear()
        if pytis.config.debug:
            self._dump_rights()

    def _dump_rights(self):  # type: () -> None
        import pytis.extensions
        registered_shortnames = set()
        if self._access_rights not in (None, 'nonuser',):
            registered_shortnames = registered_shortnames.union(self._access_rights.keys())
        if Specification._access_rights not in (None, 'nonuser'):
            registered_shortnames = registered_shortnames.union(
                Specification._access_rights.keys()
            )
        resolver = pytis.config.resolver
        output = sys.stderr
        output.write("--- BEGIN list of registered rights ---\n")
        output.write("# source shortname right column permitted\n")

        def find_columns(spec_name):
            try:
                specification = resolver.specification(spec_name)
            except pytis.util.ResolverError:
                specification = None
            if specification is None:
                columns = []
            else:
                columns = [f.id() for f in specification.view_spec().fields()]
            return columns
        all_permissions = pd.Permission.all_permissions()
        for shortname in registered_shortnames:
            if shortname.startswith('form/'):
                columns = find_columns(shortname[5:])
            else:
                columns = []
            for permission in all_permissions:
                permitted = self.action_has_access(shortname, permission)
                output.write('actions %s %s None %s\n' % (shortname, permission, permitted,))
                for c in columns:
                    permitted = self.action_has_access(shortname, permission, c)
                    output.write('actions %s %s %s %s\n' % (shortname, permission, c, permitted,))
        specification_names = pytis.extensions.get_form_defs()
        for spec_name in specification_names:
            columns = find_columns(spec_name)
            for permission in all_permissions:
                permitted = self.api_has_access(spec_name, permission)
                output.write('specifications %s %s None %s\n' % (spec_name, permission, permitted))
                for c in columns:
                    permitted = self.api_has_access(spec_name, permission, c)
                    output.write('specifications %s %s %s %s\n' %
                                 (spec_name, permission, c, permitted,))
        output.write("--- END list of registered rights ---\n")

    @property
    def api_param(self):  # type: () -> Any
        return self._param

    def _output_formatter(self, template_id, **kwargs):  # type: (str, **Any) -> Any
        import pytis.output
        output_resolver = pytis.output.OutputResolver(pytis.config.print_spec_dir,
                                                     pytis.config.resolver)
        return pytis.output.Formatter(pytis.config.resolver, (output_resolver,),
                                      template_id, **kwargs)

    def api_printout(self,
                     spec_name,  # type: str
                     template_id,  # type: str
                     row=None,  # type: Optional[Union[pd.Row, pytis.presentation.PresentedRow]]
                     parameters=None,  # type: Optional[Mapping[str, Any]]
                     output_file=None,  # type: Optional[IO[Any]]
                     language=None,  # type: Optional[str]
                     form=None,  # type: Optional[Any]
                     ):
        # type: (...) -> None
        import pytis.output
        if parameters is None:
            parameters = {}
        parameters[pytis.output.P_NAME] = spec_name
        parameters[spec_name + '/' + pytis.output.P_ROW] = row
        try:
            formatter = self._output_formatter(template_id, form=form, parameters=parameters,
                                               language=language or pytis.util.current_language(),
                                               translations=pytis.util.translation_path())
        except pytis.output.AbortOutput as e:
            log(OPERATIONAL, str(e))
        else:
            formatter.printout(output_file)
            formatter.cleanup()

    def api_has_access(self, name, perm=pd.Permission.VIEW, column=None):
        # type: (str, Any, Optional[str]) -> bool
        if not self.action_has_access('form/' + name, perm=perm, column=column):
            return False
        try:
            main, side = name.split('::')
        except ValueError:
            dual = False
        else:
            dual = True
        if dual:
            return self.api_has_access(main, perm=perm) and self.api_has_access(side, perm=perm)
        else:
            try:
                rights = pytis.config.resolver.get(name, 'data_spec').access_rights()
            except pytis.util.ResolverError:
                rights = None
            if rights:
                if not self._access_rights_initialized:
                    self._init_access_rights()
                groups = pd.default_access_groups(pytis.config.dbconnection)
                if not rights.permitted(perm, groups, column=column):
                    return False
        return self.action_has_access('form/' + name, perm=perm, column=column)

    def action_has_access(self, action, perm=pd.Permission.CALL, column=None):
        # type: (str, Any, Optional[str]) -> bool
        """Return true iff action has perm permission.

        Arguments:
          action (str): Action identifier.
          perm: Access permission as one of `pytis.data.Permission` constants.
          column (str): Identifier of the column to check or None (no specific
            column checked).

        """
        if self._access_rights == 'nonuser':
            return False
        if self._access_rights is None:
            result = True
        else:
            rights = self._access_rights.get(action)
            if rights is None:
                # No action rights defined => only system rights apply
                # (this function is *action* rights check).
                result = True
                access_rights = Specification.data_access_rights(action)
                if access_rights is not None:
                    result = access_rights.permitted(perm, self._user_roles, column=column)
            else:
                if column is None:
                    permissions = rights.get(True, ())
                else:
                    permissions = rights.get(column, None)
                    if permissions is None:
                        permissions = rights.get(None, ())
                result = perm in permissions
        return result

    def api_echo(self, message, kind='info'):  # type: (str, str) -> None
        print('{}: {}'.format(kind, message))
