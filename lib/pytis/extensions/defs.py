# -*- coding: iso-8859-2 -*-

# Copyright (C) 2006, 2007, 2009 Brailcom, o.p.s.
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


def check_menus_defs():
    """Zkontroluje všechny specifikace uvedené v menu aplikace."""
    resolver = pytis.util.resolver()
    dbconn = config.dbconnection
    specnames = get_menu_defs()
    width = max([len(s) for s in specnames]) + len('Poslední chyba v: ') + 6
    errors = []
    def check_specs(update, specnames):
        def check_bindings(main, side):
            try:
                bindings = resolver.get(main, 'binding_spec')
            except ResolverError, e:
                return str(e)
            try:
                bspec = bindings[side]
            except KeyError:
                return "Binding item for %s not found." % side
            return None
        def check_spec(name):
            try:
                data_spec = resolver.get(name, 'data_spec')
            except ResolverError, e:
                return str(e)
            try:
                success, data = pytis.form.db_operation(data_spec.create, dbconnection_spec=dbconn)
                if not success:
                    return "Nepodařilo se vytvořit datový objekt."
                data.select()
                row = data.fetchone()
                if row:
                    view_spec = resolver.get(name, 'view_spec')
                    fields = view_spec.fields()
                    prow = PresentedRow(fields, data, row)
            except Exception, e:
                return str(e)
            return None
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
                results = (check_bindings(main, side), check_spec(main), check_spec(side))
            else:
                results = (check_spec(name),)
            for error in results:
                if error is not None:
                    errors.append("Specifikace %s: %s" % (name, error))
                    last_error = "%s\n%s...)" % (name, error[:width-4])
    pytis.form.run_dialog(pytis.form.ProgressDialog, check_specs, args=(specnames,),
                          message='Kontroluji datové specifikace...'.ljust(width) + '\n\n\n\n',
                          elapsed_time=True, can_abort=True)
    if errors:
        pytis.form.run_dialog(pytis.form.Message, "Chyby ve specifikacích",
                              report="\n".join(errors))

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
