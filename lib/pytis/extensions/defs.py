# -*- coding: utf-8 -*-

# Copyright (C) 2018-2024 Tomáš Cerha <t.cerha@gmail.com>
# Copyright (C) 2006-2015 OUI Technology Ltd.
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

"""Funkce pro načítání, caching, kontrolu a reporty z defsů."""
from __future__ import print_function
from __future__ import unicode_literals

from past.builtins import basestring

import pytis
import pytis.util
import pytis.data as pd

from pytis.api import app
from pytis.presentation import Field, PresentedRow, Menu, MenuItem, MenuSeparator, Command
from pytis.util import Resolver, ResolverError, identity, log, EVENT, OPERATIONAL


def get_form_defs(resolver=None, messages=None):
    """Return sequence of names of all public form specifications in the application.

    Arguments:

      resolver -- resolver to use when exploring application definitions modules
      messages -- list to use for adding notification messages about the
        specification lookup; or 'None'

    """
    assert resolver is None or isinstance(resolver, Resolver), resolver
    assert messages is None or isinstance(messages, list), messages
    from .dmp import DMPMessage, add_message
    specification_names = []
    if resolver is None:
        resolver = pytis.config.resolver
    for name, spec in resolver.walk():
        if spec.public:
            if spec.__name__.startswith('_'):
                add_message(messages, DMPMessage.WARNING_MESSAGE,
                            "Public specification starting with underscore", (name,))
            specification_names.append(name)
        else:
            add_message(messages, DMPMessage.NOTE_MESSAGE,
                        "Private specification, ignored", (name,))
    return specification_names


def get_menu_defs():
    """Return sequence of names of all specifications present in application menu."""
    import pytis.form

    def menu_names(items):
        result = []
        import pytis.form
        for item in items:
            if isinstance(item, Menu):
                result.extend(menu_names(item.items))
            elif not isinstance(item, MenuSeparator):
                assert isinstance(item, MenuItem), item
                if item.command.method == app.run_form:
                    specification = item.command.args['specification']
                    if isinstance(specification, basestring):
                        result.append(specification)
                    else:
                        result.append(specification._spec_name())
                # Handle legacy menu commands.
                elif item.command.method == pytis.form.Application.run_form:
                    result.append(item.command.args['name'])
        return result

    return pytis.util.remove_duplicates(menu_names(pytis.form.app.menu))


def _get_default_select(spec):
    import pytis.form

    def init_select(view, data):
        sorting = view.sorting()
        if sorting is None:
            sorting = tuple([(k.id(), pd.DESCENDANT) for k in data.key()
                             if view.field(k.id()) is not None])
        success, select_count = pytis.form.db_operation(data.select, sort=sorting, reuse=False)
        if not success:
            log(EVENT, 'Database operation failed')
            return None
        return select_count
    resolver = pytis.config.resolver
    try:
        view = resolver.get(spec, 'view_spec')
    except Exception:
        log(OPERATIONAL, "Failed creating view_spec")
        return None
    try:
        data = pytis.util.data_object(spec)
    except Exception:
        log(OPERATIONAL, "Failed creating data object")
        return None
    data = pytis.util.data_object(spec)
    select_count = init_select(view, data)
    if select_count:
        print("Default select pro specifikaci %s vrací %s řádků" % (spec, select_count,))
        data.fetchone()


class CheckReporter(object):

    def start(self, number_of_items):
        self._number_of_items = number_of_items

    def end(self):
        pass

    def info(self, message):
        print(message)

    def error(self, message):
        print("Error:", message)


class MenuChecker(object):
    _specnames = None
    _codebook_form_users_ = None

    def __init__(self, spec_name_prefix=None):
        """
        Arguments:

          spec_name_prefix -- if not None then only specification with given
            prefix (basestring) are tested

        """
        import pytis.output
        self._output_resolver = pytis.output.OutputResolver(pytis.config.print_spec_dir,
                                                            pytis.config.resolver)
        data = pd.dbtable('e_pytis_roles', ('name', 'purposeid'))
        condition = pd.NE('purposeid', pd.Value(pd.String(), 'user'))
        self._application_roles = [row[0].value()
                                   for row in data.select_map(identity, condition=condition)]
        self._spec_name_prefix = spec_name_prefix

    def _specification_names(self, errors=None):
        if self.__class__._specnames is None:
            self.__class__._specnames = self._find_specification_names(errors)
        if self._spec_name_prefix is None:
            names = self.__class__._specnames
        else:
            prefix = self._spec_name_prefix
            ln = len(prefix)
            names = [n for n in self.__class__._specnames if n[:ln] == prefix]
        return names

    def _find_specification_names(self, errors):
        return get_menu_defs()

    def check_public(self, spec_name):
        errors = []
        pos = spec_name.rfind('.')
        if pos >= 0:
            try:
                spec = pytis.config.resolver.specification(spec_name)
                print_spec = pytis.config.resolver.get(spec_name, 'print_spec')
            except ResolverError as e:
                return errors + [str(e)]
            if not spec.public:
                errors.append("Neveřejná specifikace v menu.")
            for p in (print_spec or ()):
                module = p.name()
                if module in self._output_specs:
                    continue
                self._output_specs[module] = True
                print_pos = module.rfind('.')
                if print_pos < 0:
                    errors.append("Invalid print specification: " + module)
                else:
                    print_module_name = module[:print_pos].replace('.', '/')
                    print_class_name = module[print_pos + 1:]
                    try:
                        self._output_resolver.get_object(print_module_name, print_class_name)
                    except ResolverError as e:
                        errors.append("Failed to load print specification: " + str(e))
        return errors

    def check_bindings(self, main, side):
        errors = []
        try:
            bindings = pytis.config.resolver.get(main, 'binding_spec')
        except ResolverError as e:
            return errors + [str(e)]
        if isinstance(bindings, dict):
            try:
                bindings[side]
            except KeyError:
                errors.append("Binding item for %s not found." % (side,))
        else:
            for b in bindings:
                if b.name() == side:
                    return errors
            else:
                errors.append("Binding item for %s not found." % (side,))
        return errors

    def check_codebook_rights(self, spec_name, field=None, new=False, no_spec_error=False):
        errors = []
        try:
            if field is None:
                try:
                    view_spec = pytis.config.resolver.get(spec_name, 'view_spec')
                except Exception:
                    if no_spec_error:
                        return []
                    else:
                        raise
                fields = view_spec.fields()
            else:
                fields = [field]
            for f in fields:
                codebook = f.codebook()
                if codebook is not None:
                    from pytis.dbdefs.db_pytis_menu import PytisCheckCodebookRights
                    users = [str(row[0].value()) for row in
                             pd.dbfunction(PytisCheckCodebookRights,
                                           spec_name, f.id(), codebook, new)]
                    users.sort()
                    for u in users:
                        if u not in self._application_roles:
                            errors.append(("Právo update nebo insert pro políčko %(field)s "
                                           "náhledu %(view)s "
                                           "je v rozporu s právem view číselníku %(codebook)s. "
                                           "Týká se to těchto rolí: %(roles)s.") %
                                          dict(codebook=codebook, view=spec_name, field=f.id(),
                                               roles=users))
                            break
        except Exception as e:
            errors.append(str(e))
        return errors

    def _codebook_form_users(self, codebook_name):
        if self._codebook_form_users_ is None:
            form_users = {}
            for name in self._specification_names():
                try:
                    view_spec = pytis.config.resolver.get(name, 'view_spec')
                except Exception:
                    continue
                fields = view_spec.fields()
                for f in fields:
                    codebook = f.codebook()
                    if codebook is not None:
                        form_users[codebook] = form_users.get(codebook, []) + [(name, f,)]
            self.__class__._codebook_form_users_ = form_users
        return self._codebook_form_users_.get(codebook_name, ())

    def check_reverse_codebook_rights(self, spec_name, new=False):
        errors = []
        for spec_name, field in self._codebook_form_users(spec_name):
            errors = errors + self.check_codebook_rights(spec_name, field=field, new=new)
        return errors

    def check_data(self, spec_name):
        import pytis.form
        errors = []
        resolver = pytis.config.resolver
        try:
            data_spec = resolver.get(spec_name, 'data_spec')
        except ResolverError as e:
            return errors + [str(e)]
        try:
            view_spec = resolver.get(spec_name, 'view_spec')
            fields = view_spec.fields()
            success, data = pytis.form.db_operation(data_spec.create,
                                                    dbconnection_spec=pytis.config.dbconnection)
            if not success:
                return errors + ["Nepodařilo se vytvořit datový objekt."]
            try:
                data.select(limit=1)
                try:
                    row = data.fetchone()
                except AssertionError as e:
                    # Hack to avoid printing errors on non-existent image files
                    # referred from the database.
                    if ((len(e.args) == 3 and
                         isinstance(e.args[2], pd.ValidationError) and
                         e.args[2][0] == 'Neplatný grafický formát')):
                        row = None
                    else:
                        raise
            finally:
                try:
                    data.close()
                except Exception:
                    pass
            if row:
                PresentedRow(fields, data, row)
        except Exception as e:
            errors.append(str(e))
        return errors

    def _check_spec(self, name):
        if name.find('::') != -1:
            main, side = name.split('::')
            return self.check_bindings(main, side) + self._check_spec(main) + self._check_spec(side)
        return (
            self.check_public(name) +
            self.check_data(name) +
            self.check_codebook_rights(name)
        )

    def interactive_check(self):
        def check(update, name):
            update("\n".join((
                "Kontroluji datové specifikace...",
                "Specifikace: " + name,
                "Poslední chyba: " + (errors[-1][0] if errors else ''),
                '  ' + errors[-1][1][:80] + '...' if errors else '',
            )))
            errors.extend([(name, e) for e in self._check_spec(name)])

        self._output_specs = {}
        errors = []
        app.run(check, over=self._specification_names(errors),
                elapsed_time=True, can_abort=True)
        if errors:
            app.message("Chyby ve specifikacích", content="\n".join(
                "Specifikace %s: %s" % (name, error)
                for name, error in pytis.util.remove_duplicates(errors)
            ))

    def batch_check(self, reporter):
        errors = []
        specnames = self._specification_names(errors)
        self._output_specs = {}
        for e in errors:
            reporter.error(e)
        reporter.start(len(specnames))
        for s in specnames:
            reporter.info("Specification: " + s)
            try:
                errors = self._check_spec(s)
            except Exception as e:
                errors = [str(e)]
            for e in errors:
                reporter.error(e)
        reporter.end()

    def access_check(self):
        def check(update, name):
            update("\n".join((
                "Kontroluji přístupová práva...",
                "Specifikace: " + name,
                "Poslední chyba: " + (errors[-1][0] if errors else ''),
                '  ' + errors[-1][1][:80] + '...' if errors else '',
            )))
            if name.find('::') != -1:
                main, side = name.split('::')
                errors.extend(
                    [(main, e) for e in self.check_codebook_rights(main, no_spec_error=True)] +
                    [(side, e) for e in self.check_codebook_rights(side, no_spec_error=True)]
                )
            else:
                errors.extend([(name, e) for e in
                               self.check_codebook_rights(name, no_spec_error=True)])

        self._output_specs = {}
        errors = []
        app.run(check, over=self._specification_names(errors), elapsed_time=True, can_abort=True)
        if errors:
            app.message("Chyby v přístupových právech", content="\n".join(
                "Specifikace %s: %s" % (name, error)
                for name, error in pytis.util.remove_duplicates(errors)
            ))

class AppChecker(MenuChecker):

    def _find_specification_names(self, errors):
        menu_specs = get_menu_defs()
        form_specs = get_form_defs(messages=errors)
        return pytis.util.remove_duplicates(menu_specs + form_specs)


class DevelChecker(MenuChecker):
    """This checker serves for application testing after global changes.

    Its purpose is to check that every function available to the user is
    working.  On the other hand, its purpose is not to check correctness or
    sanity of DMP data, etc.

    Of course this checker can't check corectness completely.  It performs only
    limited set of fully automated global tests.  It tries to detect mainly
    crashes.  If you want to test application logic or reactions to user
    actions, there is no other way than to implement special purpose tests.

    """
    def check_codebook_rights(self, *args, **kwargs):
        return []

    def check_reverse_codebook_rights(self, *args, **kwargs):
        return []


def check_form():
    """Show report for user selected specification.

    BEWARE: This function is referred from DMP identifiers of existing applications:

    'handle/pytis.extensions.defs.check_form/'

    """
    resolver = pytis.config.resolver
    result = app.input_form(title="Kontrola specifikace", fields=(
        Field('spec', "Specifikace", not_null=True, width=30),
    ))
    if result:
        spec = result['spec'].value()
        try:
            data_spec = resolver.get(spec, 'data_spec')
            view_spec = resolver.get(spec, 'view_spec')
        except ResolverError:
            app.warning('Specifikace nenalezena.')
            return
        data = data_spec.create(dbconnection_spec=pytis.config.dbconnection)
        # Title
        obsah = "Title: %s\n" % (view_spec.title())
        # Název tabulky
        obsah += "Tabulka: %s\n\n" % (data.table(data.columns()[0].id()))
        # Políčka v bindings
        obsah += "Políčka v data_spec:\n%s\n\n" % \
                 "\n".join(["  - %s" % c.id()
                            for c in data.columns() if c.id() != 'oid'])
        # Políčka ve fields
        obsah += "Políčka ve fields:\n%s\n\n" % \
                 "\n".join(["  - %s" % f.id() for f in view_spec.fields()])
        # Actions - TODO: Bude třeba zohlednit vnořené seznamy a ActionGroup.
        # obsah += "\n\nAkce kontextového menu:\n\n"
        # actions = view_spec.actions()
        # if actions:
        #     obsah += "\n".join([a.title() for a in actions])
        # else:
        #     obsah += "Nejsou definovány"
        # Default select
        _get_default_select(spec)
        app.message("DEFS: %s" % spec, content=obsah)


def check_menus_defs():
    """Check all specifications present in application menu interactively.

    BEWARE: This function is referred from DMP identifiers of existing applications:

    'handle/pytis.extensions.defs.check_menus_defs/'

    """
    MenuChecker().interactive_check()



def check_access_rights():
    """Check access rights of all specifications present in application menu.

    BEWARE: This function is referred from DMP identifiers of existing applications:

    'handle/pytis.extensions.defs.check_access_rights/'

    """
    MenuChecker().access_check()
