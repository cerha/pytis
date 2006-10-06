# -*- coding: iso-8859-2 -*-

# Copyright (C) 2006 Brailcom, o.p.s.
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

"""Funkce pro na��t�n�, caching, kontrolu a reporty z defs�.""" 

from pytis.extensions import *

def flatten_menus():
    """Vra� linearizovan� seznam v�ech polo�ek menu."""
    def flatten(queue, found, level=0):
        if queue:
            head, tail = queue[0], queue[1:]
            found.append(head)
            if isinstance(head, pytis.form.Menu):
                flatten(head.items(), found, level=level+1)
            result = flatten(tail, found, level=level)
        else:
            result = found                
        return result
    menus = pytis.util.resolver().get('application', 'menu')
    return flatten(menus, [])


def get_menu_defs(without_duals=False):
    resolver = pytis.util.resolver()
    RF = pytis.form.Application.COMMAND_RUN_FORM
    items = [item for item in flatten_menus()
             if isinstance(item, pytis.form.MItem) and item.command() == RF \
             and not issubclass(item.args()['form_class'],
                                pytis.form.ConfigForm)]
    duals = [item for item in items
             if issubclass(item.args()['form_class'], DualForm) \
                and not issubclass(item.args()['form_class'], DescriptiveDualForm)]
    notduals = [item for item in items if item not in duals]
    subduals = []
    for item in duals:
        dual_spec = resolver.get(item.args()['name'], 'dual_spec')
        subduals.append(dual_spec.main_name())
        subduals.append(dual_spec.side_name())
    specs = [item.args()['name'] for item in notduals] + subduals
    if not without_duals:
        specs = specs + [item.args()['name'] for m in duals]
    specs = remove_duplicates(specs)
    # Zjist�me i varianty podle konstanty VARIANTS
    variants = []
    for m in specs:
        try:
            vlist = resolver.get_object(m, 'VARIANTS')
            vlist = ['%s:%s' % (m, v) for v in vlist
                     if isinstance(v, types.StringType)]
            variants = variants + list(vlist)
        except Exception, e:
            pass
    return remove_duplicates(specs + variants)

def menu_report():
    """Vytv��� p�ehledn� n�hled na polo�ky menu."""
    resolver = pytis.util.resolver()
    data_specs = []
    COMMAND_RUN_FORM = pytis.form.Application.COMMAND_RUN_FORM
    def spec(name):
        return '<a href="#%s">%s</a>' % (name, name)
    def make_list(menu):
        items = []
        for item in menu:
            if isinstance(item, MSeparator):
                x = '------'
            elif isinstance(item, Menu):
                x = item.title() + make_list(item.items())
            elif isinstance(item, pytis.form.MItem) \
                     and item.command() == COMMAND_RUN_FORM:
                args = item.args()
                form = args['form_class']
                spec_name = args['name']
                spec_link = spec(spec_name)
                data_specs.append(spec_name)
                if issubclass(form, DualForm) and \
                       not issubclass(form, DescriptiveDualForm):
                    dual_spec = resolver.get(spec_name, 'dual_spec')
                    main = dual_spec.main_name()
                    side = dual_spec.side_name()
                    spec_link += "(%s,%s)" % (spec(main), spec(side))
                    data_specs.extend((main, side))
                x = "%s: %s, %s" % (item.title(), spec_link, form.__name__)
            else:
                x = item.title()
            items.append(x)
        list_items = ["<li>%s</li>" % i for i in items]
        return "\n".join(("<ul>",) + tuple(list_items) + ("</ul>",))
    content = "<h3>P�ehled polo�ek menu a n�zv� specifikac�</h3>"
    content += '<a name="menu"></a>'
    content += make_list(resolver.get('application', 'menu'))
    data_specs = remove_duplicates(data_specs)
    data_specs.sort()
    content += '<h1>P�ehled pr�v pro jednotliv� specifikace</h1>\n'
    for spec_name in data_specs:
        content += '<a name="%s"></a>\n<h5>%s</h5>\n' % (spec_name, spec_name)
        try:
            data_spec = resolver.get(spec_name, 'data_spec')
        except Exception, e:
            content += "<p><b>Chyba</b>: Specifikace nenalezena.</p>"
            continue
        rights = data_spec.access_rights()
        if rights:
            perms = (pytis.data.Permission.VIEW,
                     pytis.data.Permission.INSERT,
                     pytis.data.Permission.UPDATE,
                     pytis.data.Permission.DELETE,
                     pytis.data.Permission.EXPORT)
            content += "<table>"
            for perm in perms:
                groups = rights.permitted_groups(perm, None)
                content += '<tr><td valign="top">%s</td><td>%s</td></tr>' % \
                           ('<b>'+perm+'</b>', ', '.join(map(str, groups)))
            content += "</table>"
        content += "<a href=#menu>Zp�t na menu</a>"
    pytis.form.InfoWindow("P�ehled polo�ek menu a n�zv� specifikac�",
                          text=content, format=TextFormat.HTML)

cmd_menu_report = (pytis.form.Application.COMMAND_HANDLED_ACTION,
                   dict(handler=menu_report))
    

def get_default_select(spec):
    def init_select(view, data):
        sorting = view.sorting()
        if sorting is None:
            sorting = tuple([(k.id(), pytis.data.DESCENDANT) for k in data.key()
                             if view.field(k.id()) is not None])
        op = lambda : data.select(sort=sorting, reuse=False)
        success, select_count = db_operation(op)
        if not success:
            log(EVENT, 'Selh�n� datab�zov� operace')
            return None
        return select_count
    resolver = pytis.util.resolver()
    try:
        view = resolver.get(spec, 'view_spec')                
    except:
        log(OPERATIONAL, "Nepoda�ilo se vytvo�it view_spec")
        return None
    try:
        data = data_object(spec)
    except:
        log(OPERATIONAL, "Nepoda�ilo se vytvo�it datov� objekt")
        return None
    data = data_object(spec)
    select_count = init_select(view, data)
    if select_count:
        print "Default select pro specifikaci %s vrac� %s ��dk�" % (spec,
                                                                    select_count)
        import time
        start_time = time.time()
        data.fetchone()

        
def check_form():
    """Zept� se na n�zev specifikace a zobraz� jej� report."""
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
        # N�zev tabulky
        obsah += "Tabulka: %s\n\n" % (data.table(data.columns()[0].id()))
        # Pol��ka v bindings
        obsah += "Pol��ka v data_spec:\n%s\n\n" % \
                 "\n".join(["  - %s" % c.id()
                            for c in data.columns() if c.id() != 'oid'])
        # Pol��ka ve fields
        obsah += "Pol��ka ve fields:\n%s\n\n" % \
                 "\n".join(["  - %s" % f.id() for f in view_spec.fields()])
        # Actions - TODO: Bude t�eba zohlednit vno�en� seznamy a ActionGroup.
        #obsah += "\n\nAkce kontextov�ho menu:\n\n"
        #actions = view_spec.actions()
        #if actions:                
        #    obsah += "\n".join([a.title() for a in actions])
        #else:
        #    obsah += "Nejsou definov�ny"
        # Default select
        get_default_select(spec)
        pytis.form.run_dialog(pytis.form.Message,
                              "DEFS: %s" % spec,
                              report=obsah)
        
cmd_check_form = (pytis.form.Application.COMMAND_HANDLED_ACTION,
                  dict(handler=check_form))


def check_defs(seznam):
    """Zkontroluje specifikace pro uveden� seznam.

    Argumenty:
      seznam -- seznam n�zv� specifikac�

    """
    resolver = pytis.util.resolver()
    errors = []
    dbconn = dbconnection_spec=config.dbconnection
    def check_spec(update, seznam):
        total = len(seznam)
        last_error = ''
        step = 1 # aktualizujeme jen po ka�d�ch 'step' procentech...
        for n, s in enumerate(seznam):
            newmsg = "\n".join(("Kontroluji datov� specifikace...",
                                "Specifikace: " + s,
                                "Posledn� chyba v: " + last_error))
            status = int(float(n)/total*100/step)
            if not update(status*step, newmsg=newmsg):
                break
            try:
                data_spec = resolver.get(s, 'data_spec')
                try:
                    op = lambda: data_spec.create(dbconnection_spec=dbconn)
                    success, data = pytis.form.db_operation(op)
                    if not success:
                        err = "Specifikace %s: Nepoda�ilo se vytvo�it datov� objekt." % (s)
                        errors.append()
                        last_error = "%s\n(Nepoda�ilo se vytvo�it datov� objekt)" % s
                        continue
                    data.select()
                    row = data.fetchone()
                    if row:
                        try:
                            view_spec = resolver.get(s, 'view_spec')
                            fields = view_spec.fields()
                            prow = PresentedRow(fields, data, row)
                        except Exception, e:
                            err = """Specifikace %s: %s""" % (s, str(e))
                            errors.append(err)
                            last_error = "%s\n%s...)" % (s, str(e)[:sirka-4])
                except Exception, e:
                    err = """Specifikace %s: %s""" % (s, str(e))
                    errors.append(err)
                    last_error = "%s\n%s...)" % (s, str(e)[:sirka-4])
            except ResolverError, e:
                err = """Specifikace %s: %s""" % (s, str(e))
                errors.append(err)                
                last_error = "%s\n%s...)" % (s, str(e)[:sirka-4])
    sirka = max([len(s) for s in seznam]) + len('Posledn� chyba v: ') + 6
    msg = 'Kontroluji datov� specifikace...'.ljust(sirka)
    msg = msg + '\n\n\n\n'
    pytis.form.run_dialog(pytis.form.ProgressDialog, check_spec, args=(seznam,),
                          message=msg, elapsed_time=True, can_abort=True)
    if errors:
        obsah = "\n".join(errors)
        pytis.form.run_dialog(pytis.form.Message,
                              "Chyby ve specifikac�ch",
                              report=obsah)
 
def check_menus_defs():
    return check_defs(get_menu_defs(without_duals=True))

cmd_check_menus_defs = (pytis.form.Application.COMMAND_HANDLED_ACTION,
                        dict(handler=check_menus_defs))

def cache_spec(*args, **kwargs):
    resolver = pytis.util.resolver()
    def do(update, specs):
        total = len(specs)        
        last_status = 0
        step = 5 # aktualizujeme jen po ka�d�ch 'step' procentech...
        for n, file in enumerate(specs):
            status = int(float(n)/total*100/step)
            if status != last_status:
                last_status = status 
                if not update(status*step):
                    break
            for spec in ('dual_spec', 'data_spec', 'view_spec',
                         'cb_spec', 'proc_spec'):
                try:
                    resolver.get(file, spec)
                except ResolverError:
                    pass
    msg = '\n'.join(('Na��t�m specifikace (p�eru�te pomoc� Esc).', '',
                     'Na��t�n� je mo�no trvale vypnout pomoc� dialogu',
                     '"Nastaven� u�ivatelsk�ho rozhran�"'))
    specs = get_menu_defs()
    pytis.form.run_dialog(pytis.form.ProgressDialog, do, args=(specs,),
                          message=msg, elapsed_time=True, can_abort=True)
