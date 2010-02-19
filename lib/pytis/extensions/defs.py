# -*- coding: iso-8859-2 -*-

# Copyright (C) 2006, 2007, 2009, 2010 Brailcom, o.p.s.
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

"""Funkce pro načítání, caching, kontrolu a reporty z defsů.""" 

import pytis.data
from pytis.extensions import *


def get_menu_defs():
    def flatten_menus(queue, found, level=0):
        if queue:
            head, tail = queue[0], queue[1:]
            found.append(head)
            if isinstance(head, pytis.form.Menu):
                flatten_menus(head.items(), found, level=level+1)
            result = flatten_menus(tail, found, level=level)
        else:
            result = found                
        return result
    try:
        data = pytis.data.dbtable('ev_pytis_menu', ('shortname', 'fullname',), config.dbconnection)
    except:
        data = None
    if data is None:
        resolver = pytis.util.resolver()
        specs = [item.args()['name']
                 for item in flatten_menus(resolver.get('application', 'menu'), [])
                 if (isinstance(item, pytis.form.MItem) \
                     and item.command() == pytis.form.Application.COMMAND_RUN_FORM \
                     and not issubclass(item.args()['form_class'], pytis.form.ConfigForm))]
    else:
        specs = []
        def get_values(row):
            return row['shortname'].value(), row['fullname'].value()
        for shortname, fullname in data.select_map(get_values):
            if (shortname and shortname[:5] == 'form/' and
                fullname.split('/')[1][-len('.ConfigForm'):] != '.ConfigForm'):
                specs.append(shortname[5:])
    specs = remove_duplicates(specs)
    # Zjistíme i varianty podle konstanty VARIANTS
    variants = []
    for m in specs:
        try:
            vlist = resolver.get_object(m, 'VARIANTS')
        except:
            pass
        else:
            variants += ['%s:%s' % (m, v) for v in vlist if isinstance(v, str)]
    return specs + remove_duplicates(variants)


def _get_default_select(spec):
    def init_select(view, data):
        sorting = view.sorting()
        if sorting is None:
            sorting = tuple([(k.id(), pytis.data.DESCENDANT) for k in data.key()
                             if view.field(k.id()) is not None])
        success, select_count = pytis.form.db_operation(data.select, sort=sorting, reuse=False)
        if not success:
            log(EVENT, 'Selhání databázové operace')
            return None
        return select_count
    resolver = pytis.util.resolver()
    try:
        view = resolver.get(spec, 'view_spec')                
    except:
        log(OPERATIONAL, "Nepodařilo se vytvořit view_spec")
        return None
    try:
        data = data_object(spec)
    except:
        log(OPERATIONAL, "Nepodařilo se vytvořit datový objekt")
        return None
    data = data_object(spec)
    select_count = init_select(view, data)
    if select_count:
        print "Default select pro specifikaci %s vrací %s řádků" % (spec,
                                                                    select_count)
        import time
        start_time = time.time()
        data.fetchone()

        
def check_form():
    """Zeptá se na název specifikace a zobrazí její report."""
    resolver = pytis.util.resolver()
    spec = pytis.form.run_dialog(pytis.form.InputDialog,
                                 message="Kontrola defsu",
                                 prompt="Specifikace",
                                 input_width=30)
    if spec:
        try:
            data_spec = resolver.get(spec, 'data_spec')
            view_spec = resolver.get(spec, 'view_spec')                
        except ResolverError:
            msg = 'Specifikace nenalezena.'
            pytis.form.run_dialog(pytis.form.Warning, msg)
            return
        data = data_spec.create(dbconnection_spec=config.dbconnection)
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
        #obsah += "\n\nAkce kontextového menu:\n\n"
        #actions = view_spec.actions()
        #if actions:                
        #    obsah += "\n".join([a.title() for a in actions])
        #else:
        #    obsah += "Nejsou definovány"
        # Default select
        _get_default_select(spec)
        pytis.form.run_dialog(pytis.form.Message, "DEFS: %s" % spec, report=obsah)
        
cmd_check_form = (pytis.form.Application.COMMAND_HANDLED_ACTION,
                  dict(handler=check_form))

class CheckReporter(object):

    def start(self, number_of_items):
        self._number_of_items = number_of_items

    def end(self):
        pass

    def info(self, message):
        print message

    def error(self, message):
        print "Error:", message

class MenuChecker(object):
    _specnames = None
    _codebook_form_users_ = None
    
    def __init__(self):
        self._resolver = pytis.util.resolver()
        self._dbconn = config.dbconnection
        if self.__class__._specnames is None:
            self.__class__._specnames = get_menu_defs()
        connection_data = config.dbconnection
        data = pytis.data.dbtable('e_pytis_roles', ('name', 'purposeid',), connection_data)
        condition = pytis.data.NE('purposeid', pytis.data.Value(pytis.data.String(), 'user'))
        self._application_roles = [row[0].value()
                                   for row in data.select_map(identity, condition=condition)]

    def check_public(self, spec_name):
        errors = []
        pos = spec_name.rfind('.')
        if pos >= 0:
            module_name = spec_name[:pos].replace('.', '/')
            class_name = spec_name[pos+1:]
            try:
                spec = self._resolver.get_object(module_name, class_name)
            except ResolverError, e:
                return errors + [str(e)]
            if not spec.public:
                errors.append("Neveřejná specifikace v menu.")
        return errors
    
    def check_bindings(self, main, side):
        errors = []
        try:
            bindings = self._resolver.get(main, 'binding_spec')
        except ResolverError, e:
            return errors + [str(e)]
        try:
            bspec = bindings[side]
        except KeyError:
            errors.append("Binding item for %s not found." % (side,))
        return errors
        
    def check_codebook_rights(self, spec_name, field=None, new=False):
        errors = []
        try:
            if field is None:
                view_spec = self._resolver.get(spec_name, 'view_spec')
                fields = view_spec.fields()
            else:
                fields = [field]
            for f in fields:
                codebook = f.codebook()
                if codebook is not None:
                    def arg(name, value):
                        return name, pytis.data.Value(pytis.data.String(), value)
                    arguments = (arg('form', spec_name),
                                 arg('field', f.id()),
                                 arg('codebook', codebook),
                                 ('new', (pytis.data.Value(pytis.data.Boolean(), new)),),
                                 )
                    users = pytis.extensions.dbfunction('pytis_check_codebook_rights', *arguments)
                    if users:
                        if is_sequence(users):
                            users = [str(row[0].value()) for row in users]
                            users.sort()
                        else:
                            users = [users]
                        for u in users:
                            if u not in self._application_roles:
                                errors.append(("Právo update nebo insert pro políčko %(field)s náhledu %(view)s "
                                               "je v rozporu s právem view číselníku %(codebook)s. "
                                               "Týká se to těchto rolí: %(roles)s.") %
                                              dict(codebook=codebook, view=spec_name, field=f.id(), roles=users))
                                break
        except Exception, e:
            errors.append(str(e))
        return errors

    def _codebook_form_users(self, codebook_name):
        if self._codebook_form_users_ is None:
            form_users = {}
            for name in self._specnames:
                try:
                    view_spec = self._resolver.get(name, 'view_spec')
                except:
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
        errors = []
        resolver = self._resolver
        try:
            data_spec = resolver.get(spec_name, 'data_spec')
        except ResolverError, e:
            return errors + [str(e)]
        try:
            view_spec = resolver.get(spec_name, 'view_spec')
            fields = view_spec.fields()
            success, data = pytis.form.db_operation(data_spec.create, dbconnection_spec=self._dbconn)
            if not success:
                return errors + ["Nepodařilo se vytvořit datový objekt."]
            data.select()
            row = data.fetchone()
            if row:
                PresentedRow(fields, data, row)
        except Exception, e:
            errors.append(str(e))
        return errors
    
    def _check_spec(self, name):
        return (self.check_public(name) +
                self.check_data(name) +
                self.check_codebook_rights(name))

    def interactive_check(self):
        specnames = self._specnames
        width = max([len(s) for s in specnames]) + len('Poslední chyba v: ') + 6
        errors = []
        def check_specs(update, specnames):
            check_spec = self._check_spec
            total = len(specnames)
            last_error = ''
            step = 1 # aktualizujeme jen po každých 'step' procentech...
            for n, name in enumerate(specnames):
                newmsg = "\n".join(("Kontroluji datové specifikace...",
                                    "Specifikace: " + name,
                                    "Poslední chyba v: " + last_error))
                status = int(float(n)/total*100/step)
                if not update(status*step, newmsg=newmsg):
                    break
                if name.find('::') != -1:
                    main, side = name.split('::')
                    results = self.check_bindings(main, side) + check_spec(main) + check_spec(side)
                else:
                    results = check_spec(name)
                for error in results:
                    errors.append("Specifikace %s: %s" % (name, error))
                    last_error = "%s\n%s...)" % (name, error[:width-4])
        pytis.form.run_dialog(pytis.form.ProgressDialog, check_specs, args=(specnames,),
                              message='Kontroluji datové specifikace...'.ljust(width) + '\n\n\n\n',
                              elapsed_time=True, can_abort=True)
        if errors:
            errors = remove_duplicates(errors)
            pytis.form.run_dialog(pytis.form.Message, "Chyby ve specifikacích",
                                  report="\n".join(errors))

    def batch_check(self, reporter):
        reporter.start(len(self._specnames))
        for s in self._specnames:
            reporter.info("Specifikace: " + s)
            try:
                errors = self._check_spec(s)
            except Exception, e:
                errors = [str(e)]
            for e in errors:
                reporter.error(e)
        reporter.end()
            
def check_menus_defs():
    """Zkontroluje všechny specifikace uvedené v menu aplikace."""
    MenuChecker().interactive_check()

cmd_check_menus_defs = (pytis.form.Application.COMMAND_HANDLED_ACTION,
                        dict(handler=check_menus_defs))

def cache_spec(*args, **kwargs):
    resolver = pytis.util.resolver()
    def do(update, specs):
        def cache(name):
            for spec in ('data_spec', 'view_spec',
                         'cb_spec', 'proc_spec', 'binding_spec'):
                try:
                    resolver.get(name, spec)
                except ResolverError:
                    pass
            
        total = len(specs)        
        last_status = 0
        step = 5 # aktualizujeme jen po každých 'step' procentech...
        for n, name in enumerate(specs):
            status = int(float(n)/total*100/step)
            if status != last_status:
                last_status = status 
                if not update(status*step):
                    break
            for x in name.split('::'):
                cache(x)
    msg = '\n'.join(('Načítám specifikace (přerušte pomocí Esc).', '',
                     'Načítání je možno trvale vypnout pomocí dialogu',
                     '"Nastavení uživatelského rozhraní"'))
    pytis.form.run_dialog(pytis.form.ProgressDialog, do, args=(get_menu_defs(),),
                          message=msg, elapsed_time=True, can_abort=True)
