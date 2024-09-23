# -*- coding: utf-8 -*-

# Copyright (C) 2018-2024 Tomáš Cerha <t.cerha@gmail.com>
# Copyright (C) 2001-2018 OUI Technology Ltd.
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

"""Interaktivní formuláře pro práci s daty.

Základem všech formulářů je třída 'Form'.  Dále tento modul obsahuje její
potomky pro konkrétní použití -- jednoduché editační formuláře (pro zobrazení a
editaci jednoho záznamu).  Jednoduché seznamové formuláře a duální formuláře
jsou v oddělených modulech 'list' a 'dualform'.  Blíže viz dokumentace
jednotlivých tříd.

"""
from __future__ import print_function

from past.builtins import basestring
from builtins import object
from future.utils import python_2_unicode_compatible

import contextlib
import copy
import lcg
import time
import wx

import pytis.api
import pytis.data
import pytis.form
import pytis.output
import pytis.util

from pytis.api import app
from pytis.presentation import (
    ActionContext, Button, Computer, Editable, Field, GroupSpec,
    Orientation, PresentedRow, PrintAction, Profile, Specification,
    TabGroup, Text, TextFormat, ViewSpec, Menu, MenuItem, MenuSeparator,
)
from pytis.util import (
    ACTION, EVENT, OPERATIONAL, ProgramError, ResolverError, UNDEFINED,
    find, format_traceback, log, super_, xlist, xtuple,
)

from .event import UserBreakException, wx_callback
from .command import CommandHandler
from .screen import (
    Browser, CallbackHandler, InfoWindow, KeyHandler,
    FileViewer, busy_cursor, dlg2px, char2px, orientation2wx,
    wx_button, wx_focused_window, wx_toolbar,
    DEFAULT_WINDOW_BACKGROUND_COLOUR,
)
from .application import (
    Application, db_operation, run_form,
)
from .search import (
    SearchDialog, FilterDialog, SortingDialog
)
from pytis.dbdefs.db_pytis_statistics import PytisLogForm


import functools

_ = pytis.util.translations('pytis-wx')

unistr = type(u'')  # Python 2/3 transition hack.


class FormProfile(object):
    """Temporary hack to avoid application crashing on unpickling old form profiles."""

    def __init__(self, *args, **kwargs):
        log(OPERATIONAL, "%s instantiated:" % self.__class__.__name__, (args, kwargs))


class FormSettings(FormProfile):
    pass

@pytis.api.implements(pytis.api.Form)
@python_2_unicode_compatible
class Form(wx.Panel, KeyHandler, CallbackHandler, CommandHandler):
    """Společná nadtřída formulářů.

    Formulář si podle jména specifikace předaného konstruktoru vyžádá od
    resolveru příslušnou datovou a prezentační specifikaci.  Z datové
    specifikace vytvoří datový objekt (instance třídy odvozené z
    'pytis.data.Data').  Datový objekt a prezentační specifikace jsou potom
    uloženy ve formě atributů instance formuláře ('self._view' a 'self._data')

    Instance tříd odvozených z této třídy jsou potom vytvářeny na základě
    interpretace prezentační specifikace a pracují s daty s pomocí datového
    objektu a jeho API (které je nezávislé na konkrétním zdroji dat).

    Formuláře jsou schopné popsat svůj stav a později tento stav dle onoho
    popisu obnovit.  K získání popisu stavu, resp. obnovení stavu, slouží
    metoda 'save()', resp. 'restore()'.

    Používané specifikační funkce:

      print_spec -- sequence of 'PrintAction' instances

    """

    CALL_USER_INTERACTION = 'CALL_USER_INTERACTION'
    """Konstanta callbacku interakce uživatele."""

    _LOG_STATISTICS = True
    DESCR = None

    _focused_form = None

    class InitError(Exception):
        """Exception signaling errors on form initializations."""

    @classmethod
    def _get_command_handler_instance(cls):
        return pytis.form.app.current_form(inner=False)

    @classmethod
    def descr(cls):
        """Vrať textový popis typu formuláře jako řetězec."""
        if cls.DESCR is not None:
            return cls.DESCR
        else:
            return cls.__name__

    @classmethod
    def focused_form(cls):
        """Return the currently focused form or 'None' if no form is focused."""
        return cls._focused_form

    def __init__(self, parent, resolver, name, full_init=True, **kwargs):
        """Inicializuj instanci.

        Argumenty:

          parent -- parent 'wx.Frame' instance.
          resolver -- resolver jmenných odkazů, instance 'pytis.util.Resolver'
          name -- jméno specifikačního souboru pro resolver; string
          guardian -- formulář (instance libovolné třídy), ve kterém je
            formulář vložen z hlediska struktury aplikace; není-li zadán, je
            použit 'parent'.  Tento parametr je využíván například při zasílání
            klávesových událostí \"nahoru\".  Typicky je to formulář, který
            tuto instanci vytváří.
          transaction -- transaction to use when manipulating data
          spec_kwargs -- dictionary of keyword arguments passed to the view
            specification constructor.
          full_init -- iff false, don't perform full form initialization.  This
            means performing just necessary wx initialization to make the form
            available without creating its content and data structures.  This
            is useful when you need to define a form and to register it
            somewhere (e.g. as a side form in a wx notebook) without actually
            showing it and without delaying other actions (e.g. because of
            unnecessary expensive querying database views in the hidden form).
            If this option is used, you are fully responsible to call full from
            initialization before the form is actually used for the first
            time.  You must do it using 'full_init()' method and you may call
            the method only once for the given form instance.
          kwargs -- viz níže.

        Resolver je použit k získání datové a prezentační specifikace a
        následnému vytvoření datového objektu. Ten je potom společně s
        prezentační specifikací uložen v podobě atributů vytvářené instance.

        Odkaz na resolver samotný je také zapamatován pro pozdější použití
        (vytváření dalších formulářů).

        Inicializace je rozdělena do několika kroků.  Nejprve jsou zpracovány
        všechny argumenty společné všem formuářovým třídám.  Ty zpracovává
        konstruktor bázové třídy 'Form'.  Jejich zpracování by nemělo být
        předefinováváno v odvozených třídách a ani žádné další argumenty by
        neměly být přidávány.  Konstruktor je možno předefinovat a provádět
        nějaké doplňující akce, ale argumenty by neměly být měněny.

        Po zpracování společných argumentů jsou načteny specifikace a vytvořen
        datový objekt.

        Poté jsou zpracovávány klíčové argumenty.  Každá odvozená třída může
        definovat své vlastní klíčové argumenty.  Ty potom zpracuje
        předefinováním metody '_init_attributes()'.  Ta již může využívat
        inicializovaného datového objetu a specifikací a případně inicializovat
        další atributy třídy.  Metoda '_init_attributes()' by měla vždy
        zpracovávat pouze klíčové argumenty, které jsou specifické pro danou
        třídu.  Zbylé předá metodě rodičovské třídy konstrukcí **kwargs.  Takto
        by mělo být zaručeno, že dojde postupně ke zpracování všech argumentů.

        Teprve po zpravování argumentů konstruktoru a inicializaci atributů je
        vytvářen vlastní obsah formuláře (viz. '_create_form()').  Toto by mělo
        být dodržováno i v odvozených třídách.

        """
        wx.Panel.__init__(self, parent, -1)
        self.SetOwnBackgroundColour(DEFAULT_WINDOW_BACKGROUND_COLOUR)
        self._name = name
        self._hide_form_requested = False
        self._full_init_started = False
        self._full_init_finished = False
        self._full_init_kwargs = dict(kwargs, resolver=resolver, name=name)
        if full_init:
            self.full_init()

    def __str__(self):
        return '<%s for "%s">' % (self.__class__.__name__, self._name)

    def __repr__(self):
        return str(self)

    def _full_init(self, resolver, name, guardian=None, transaction=None,
                   spec_kwargs={}, **kwargs):
        import pytis.extensions
        start_time = pytis.data.DateTime.now(without_timezone=True)
        self._resolver = resolver
        self._guardian = guardian or self.Parent
        self._governing_transaction = transaction
        self._spec_kwargs = copy.copy(spec_kwargs)
        KeyHandler.__init__(self)
        CallbackHandler.__init__(self)
        try:
            # Note, that some code relies on the order of calling these two methods.
            self._view = self._create_view_spec()
            self._data = self._create_data_object()
        except (ResolverError, ProgramError):
            log(OPERATIONAL, 'Form initialization error', format_traceback())
            raise self.InitError()
        self._transaction = transaction or self._default_transaction()
        self._init_attributes(**kwargs)
        self._result = None
        try:
            self._create_form()
        except Exception:
            # This is necessary to prevent database connection leaks
            self._cleanup()
            raise
        show_time = pytis.data.DateTime.now(without_timezone=True)
        if self._LOG_STATISTICS and name and pytis.config.form_statistics:
            pytis.data.dbfunction(PytisLogForm, name, self.__class__.__name__,
                                  info=self._form_log_info(),
                                  t_start=start_time.value(),
                                  t_show=show_time.value(),
                                  )
        wx_callback(wx.EVT_IDLE, self, self._on_idle)
        log(EVENT, 'Form created in %.3fs:' %
            (pytis.data.DateTime.diff_seconds(start_time, show_time),), self)

    def _init_attributes(self):
        """Process constructor keyword arguments and initialize the attributes.

        This method is called in the initial phase of form construction before any UI widget
        creation but after the initialization of specifications and the data object.  The derived
        classes should primarily process all their specific constructor arguments and initialize
        the attributes of the instance.  See also the constructor documentation for more details.

        """
        pass

    def _get_saved_setting(self, option, default=None):
        """Retrieve form parameter independent on current profile."""
        return pytis.form.app.form_settings_manager.get(self._profile_spec_name(),
                                                        self._form_name(),
                                                        option, default=default)

    def _set_saved_setting(self, option, value):
        """Update saved form parameter independent on current profile."""
        pytis.form.app.form_settings_manager.set(self._profile_spec_name(),
                                                 self._form_name(), option, value)

    def _create_view_spec(self):
        t = time.time()
        spec = self._resolver.get(self._name, 'view_spec', **self._spec_kwargs)
        log(EVENT, 'Specification read in %.3fs:' % (time.time() - t), spec)
        assert isinstance(spec, ViewSpec)
        return spec

    def _create_data_object(self):
        return pytis.util.data_object(self._name, spec_kwargs=self._spec_kwargs)

    def _create_form(self):
        # Build the form from parts
        sizer = wx.BoxSizer(wx.VERTICAL)
        self.SetSizer(sizer)
        self._create_form_parts()
        sizer.Fit(self)  # Set the size of window `self' to size of the sizer.

    def _create_form_parts(self):
        pass

    def _form_log_info(self):
        return ''

    def _default_transaction(self):
        return None

    def _open_transaction(self):
        if ((self._transaction is not None and not self._transaction.open() and
             self._transaction is not self._governing_transaction)):
            self._transaction = self._default_transaction()
        return self._transaction

    def _toolbar_commands(self):
        """Return toolbar commands for this form's toolbar as a sequence of command groups.

        The returned sequence of sequences represents command groups which
        should be added to the toolbar with visual separators between
        individual groups.  Items of the inner sequences must be 'UICommand'
        instances.

        """
        # In this class returns those commands from global TOOLBAR_COMMANDS
        # which belong to this form class.
        groups = []
        for group in pytis.form.TOOLBAR_COMMANDS:
            group = [uicmd for uicmd in group
                     if isinstance(self, uicmd.command().handler())]
            if group:
                groups.append(group)
        return groups

    def _create_toolbar(self):
        return wx_toolbar(self, self._toolbar_commands())

    def _form_name(self):
        cls = self.__class__
        module_name = cls.__module__
        if module_name.startswith('pytis.form.'):
            # Make sure the module name is always the same (depending on how
            # modules were imported, it may be sometimes 'pytis.form' and sometimes
            # 'pytis.form.list').
            module_name = 'pytis.form'
        return '%s.%s' % (module_name, cls.__name__)

    def _profile_spec_name(self):
        # Return form name for profile manager.  There is a separate method for
        # this as we need to construct a special name for aggregated forms.  So
        # this method normally returns simply self._name, but in special cases
        # it may be overriden to return something else as the name for profile
        # manager.
        return self._name

    def _release_data(self):
        if self._data is not None:
            self._data.sleep()

    def _on_idle(self, event):
        pass

    def _cleanup(self):
        """Perform cleanup before the form is definitively closed."""
        self._cleanup_data()

    def _cleanup_data(self):
        try:
            self._data.close()
        except Exception:
            pass
        self._data = None
        if self._transaction is not None and self._transaction is not self._governing_transaction:
            try:
                self._transaction.rollback()
            except Exception:
                pass

    # Zpracování příkazů

    def _cmd_help(self):
        Application.COMMAND_HELP.invoke(topic=('spec/' + self._name))

    def _cmd_leave_form(self):
        def leave():
            pytis.form.app.block_yield(True)
            try:
                return self.close()
            finally:
                pytis.form.app.block_yield(False)
        # We noticed segmentation faults inside wx key event processing
        # when the form had been closed using the Escape key.  Previously,
        # we worked around the problem by executing the leave form command
        # in the IDLE thread.  As it got a little too complicated, we now
        # try executing it through wx.CallAfter.
        self.Unbind(wx.EVT_IDLE)
        wx.CallAfter(leave)

    # Veřejné metody

    def name(self):
        """Vrať název specifikace formuláře."""
        return self._name

    def title(self):
        """Vrať titulek ze specifikace formuláře jako řetězec."""
        return self._view.title()

    def view_spec(self):
        """Return 'ViewSpec' instance corresponding to the form."""
        return self._view

    def guardian(self):
        """Vrať guardian zadané v konstruktoru (nebo parent)."""
        return self._guardian

    def check_permission(self, perm, quiet=True):
        """Vrať pravdu, pokud má uživatel daná práva k datovému objektu.

        Argumentem je konstanta  třídy 'pytis.data.Permission'.

        """
        VIEW = pytis.data.Permission.VIEW
        INSERT = pytis.data.Permission.INSERT
        UPDATE = pytis.data.Permission.UPDATE
        DELETE = pytis.data.Permission.DELETE
        EXPORT = pytis.data.Permission.EXPORT
        if perm == DELETE:
            result = app.has_access(self.name(), perm=perm)
        else:
            for col in self._data.columns():
                if app.has_access(self.name(), perm=perm, column=col.id()):
                    result = True
                    break
            else:
                result = False
        if not result and not quiet:
            msg = {
                VIEW: "Nemáte právo k zobrazení formuláře.",
                INSERT: "Nemáte právo vložit nový záznam.",
                UPDATE: "Nemáte právo změnit existující záznam.",
                DELETE: "Nemáte právo smazat existující záznam.",
                EXPORT: "Nemáte právo k exportu do CSV.",
            }[perm]
            app.echo(msg, kind='error')
        return result

    def set_status(self, field, message):
        """Zobraz zprávu `message' v poli `id' stavové řádky formuláře.

        Má-li formulář vlastní stavovou řádku a v ní pole `id' zobraz v něm
        danou zprávu a vrať pravdu.  V opačném případě vrať nepravdu.

        """
        return False

    def data(self):
        """Return a new instance of the data object used by the form."""
        return self._create_data_object()

    def full_init(self):
        """Finish initialization of a mostly unitialized form.

        This must be called before the first use of the form when
        'soft_init=True' was used in the constructor.  It may not be called
        otherwise nor may be called more than once.

        """
        assert not self._full_init_started, "Form initialization called more than once"
        self._full_init_started = True
        kwargs = self._full_init_kwargs
        self._full_init_kwargs = None
        self._full_init(**kwargs)
        self._full_init_finished = True

    def initialized(self):
        """Return true iff full form initialization was attempted.

        If it wasn't, it is necessary to call 'full_init()' for this form to
        use the form.

        """
        return self._full_init_started

    def can_command(self, command, **kwargs):
        # This was introduced to prevent tracebacks on Update UI events in uninitialized
        # side forms.  It seems, however, to make sense generally so it hopefuly is not
        # such a big hack...
        if not self._full_init_finished:
            return False
        return super(Form, self).can_command(command, **kwargs)

    def save(self):
        """Save form state to be able to restore it later when 'restore()' is called."""
        self._release_data()

    def restore(self):
        """Restore form state as saved by 'save()'."""
        pass

    def _exit_check(self):
        """Check whether it is ok to close the form.

        Performed on user's attempt to close the form.  Returns True if the
        form may be closed or False otherwise.

        """
        return True

    def close(self, force=False):
        """Close the form and destroy all its UI elements.

        If 'force' is False, '_exit_check()' is performed and closing is only
        really done if it returns True.  If 'force' is True, the form is closed
        unconditionally.

        """
        return self._close(force=force)

    def _close(self, force=False):
        if force or self._exit_check():
            self.hide()
            try:
                self._cleanup()
            finally:
                self.Close()
                self.Destroy()
            return True
        else:
            return False

    def resize(self):
        """Resize the form to fit the parent window."""
        self.Size = self.Parent.ClientSize

    def show(self):
        """Make the form active to user interaction and display it visually."""
        self.Enable(True)
        self.Show(True)
        self.focus()

    def hide(self):
        """Make the form inactive to user interaction and hide it visually."""
        orig_hide_form_requested = self._hide_form_requested
        self._hide_form_requested = True
        try:
            self.defocus()
            self.Enable(False)
            self.Show(False)  # nutné i před uzavřením
        finally:
            self._hide_form_requested = orig_hide_form_requested

    def focus(self):
        """Focus this form."""
        if Form._focused_form:
            Form._focused_form.defocus()
        Form._focused_form = self
        self.SetFocus()

    def defocus(self):
        """Defocus this form."""
        if Form._focused_form is self:
            Form._focused_form = None

    # Implementation of Public API 'pytis.api.Form'.

    @property
    def api_name(self):
        return self._name

    @property
    def api_field(self):
        return None

    @property
    def api_condition(self):
        return None

    @property
    def api_arguments(self):
        return None

    @property
    def api_sorting(self):
        return None

    @property
    def api_profile(self):
        return None

    @property
    def api_query_fields(self):
        return None

    @property
    def api_row(self):
        return None

    @property
    def api_selection(self):
        return None

    def api_refresh(self):
        pass

    @property
    def api_main_form(self):
        return None

    @property
    def api_side_form(self):
        return None

    def api_clear_selection(self):
        pass

    def api_select_row(self, position):
        pass

    def api_close(self, force=False):
        if self is not pytis.form.app.top_window():
            pytis.form.Application.COMMAND_RAISE_FORM.invoke(form=self)
        return self.close(force=force)


class InnerForm(Form):
    """Formulář, který zpracuje příkazy samostatně i unvitř duálního formuláře.

    Tato formulářová třída je zde především kvůli definici a zpracování
    příkazů.  Pokud je aktuálním formulářem jednoduchý formulář, je zpracování
    příkazu předáno tomuto formuláři.  Pokud je však aktuálním formulářem
    duální formulář, je třeba rozhodnout, zda bude příkaz zpracován přímo
    duálním formulářem, nebo jeho aktivním podformulářem.  Příkazy třídy 'Form'
    jsou zpracovávány vždy formulářem nejvyšší úrovně (duálním formulářem
    samotným, pokud je aktuálním formulářem duální formulář).

    Příkazy definované touto třídou a třídami z ní odvozenými jsou však vždy
    předávány aktivnímu vnitřnímu formuláři.

    """
    @classmethod
    def _get_command_handler_instance(cls):
        return pytis.form.app.current_form()

    def _init_attributes(self, **kwargs):
        super(InnerForm, self)._init_attributes(**kwargs)
        # The aggregation menu must be created dynamically, but we can find out just once,
        # whether the menu exists for given form.
        self._has_aggregation_menu = self._aggregation_menu() is not None
        # Print menu is no longer static, so we create it on demand.
        self._print_menu_ = UNDEFINED

    def _on_menu_button(self, items):
        self._run_callback(self.CALL_USER_INTERACTION)
        parent = wx_focused_window()
        if parent:
            pytis.form.app.popup_menu(parent, items, keymap=self._get_keymap())

    def _print_menu(self):
        # Vrať seznam položek tiskového menu.
        name = self._name
        try:
            print_spec = list(self._resolver.get(name, 'print_spec') or ())
        except ResolverError:
            print_spec = []
        # Default print currently disabled, since on a huge table it may extensively consume
        # resources and no one is using it anyway...
        # if not print_spec:
        #     print_spec = ((_("Default"), os.path.join('output', name)),)
        db_print_spec = []
        condition = pytis.data.EQ('module', pytis.data.Value(pytis.data.String(), name))
        i = 1
        for row in pytis.extensions.dbselect('printing.UserOutputTemplates', condition=condition):
            template_name = row['specification'].value()
            template_specification = row['module'].value() + '/' + template_name
            print_item = PrintAction('__db_%d' % (i,), template_name, template_specification,)
            print_spec.append(print_item)
            db_print_spec.append((name, template_name,))
            i += 1
        printing_form = 'printing.DirectUserOutputTemplates'
        menu = [MenuItem(p.title(), command=pytis.form.BrowseForm.COMMAND_PRINT(spec=p))
                for p in print_spec
                if pytis.form.app.action_has_access('print/%s' % (p.dmp_name(),),
                                                    perm=pytis.data.Permission.PRINT)]
        if app.has_access(self.name(), perm=pytis.data.Permission.PRINT):
            menu.append(MenuSeparator())
            menu.append(MenuItem(_("New report"),
                                 command=pytis.form.Application.COMMAND_NEW_RECORD(
                                     name=printing_form,
                                     prefill=dict(module=pytis.data.sval(name))
                                 )))
            if db_print_spec:
                mitem = pytis.extensions.run_form_mitem
                edit_submenu = [mitem(label, printing_form, PopupEditForm,
                                      select_row=dict(module=pytis.data.sval(module),
                                                      specification=pytis.data.sval(label)))
                                for module, label in db_print_spec]
                mitem = pytis.extensions.run_procedure_mitem
                delete_submenu = [mitem(label, printing_form, 'delete_template',
                                        module=module, specification=label)
                                  for module, label in db_print_spec]
                menu += [Menu(_("Edit report"), edit_submenu),
                         Menu(_("Delete report"), delete_submenu)]
        return menu

    def _get_print_menu(self):
        if self._print_menu_ is UNDEFINED:
            self._print_menu_ = self._print_menu()
        return self._print_menu_

    def _aggregation_menu(self):
        return None

    def _spec_description(self):
        try:
            description = self._cached_spec_description
        except AttributeError:
            try:
                data = pytis.data.dbtable('e_pytis_help_spec', ('spec_name', 'description', 'help'))
            except pytis.data.DBException:
                description = self._view.description()
            else:
                row = data.row((pytis.data.Value(data.find_column('spec_name').type(),
                                                 self._name),))
                if row:
                    description = row['description'].value()
                else:
                    description = self._view.description()
            self._cached_spec_description = description
        return description

    def _cmd_describe(self):
        title = self._view.title()
        description = self._spec_description()
        text = "= " + title + " =\n\n" + description
        InfoWindow(title, text=text, format=TextFormat.LCG)

    def _can_describe(self):
        return self._spec_description() is not None

    def _cmd_aggregation_menu(self):
        self._on_menu_button(self._aggregation_menu())

    def _can_aggregation_menu(self):
        return self._has_aggregation_menu

    def _can_print_menu(self):
        return bool(self._get_print_menu())

    def _cmd_print_menu(self):
        self._on_menu_button(self._get_print_menu())


class Refreshable(object):
    """Abstract class providing the method 'refresh()' with given meaning.

    All forms which may be refreshed from their data source should derive from
    this class and implement the '_refresh()' method.

    """
    _block_refresh = 0

    @classmethod
    @contextlib.contextmanager
    def block_refresh(cls):
        """Block performing any refresh within this contextmanager scope.

        Refresh is blocked globally for all existing forms.

        """
        Refreshable._block_refresh += 1
        yield
        Refreshable._block_refresh -= 1

    def refresh(self, interactive=False):
        """Refresh form data from data source.

        Arguments:

          interactive -- indicates whether the refresh was invoked by explicit
            user request

        Returns: True iff the refresh has been performed.

        """
        level = Refreshable._block_refresh
        if level == 0:
            self._refresh(interactive=interactive)
        elif level > 0:
            log(OPERATIONAL, "Refresh bloked:", level)
        else:
            raise ProgramError("Invalid _block_refresh level:", level)

    def _refresh(self, interactive=False):
        """Perform refresh.

        This method should be overriden in derived classes to implement the
        refresh itself.

        """
        pass

    def api_refresh(self):
        self.refresh()


class PopupForm(object):
    """Mixin class displaying the form in a separate (pop up) frame."""

    def _popup_frame(self, parent):
        """Vrať frame instance.

        Pokud frame ještě neexistuje, vytvoř jej.

        Argumenty:

          parent -- rodičovské okno, instance 'wx.Window'

        """
        try:
            frame = self._popup_frame_
        except AttributeError:
            frame = wx.Dialog(parent, style=self._popup_frame_style())
            self._popup_frame_ = frame
            wx_callback(wx.EVT_CLOSE, frame, self._on_frame_close)
        return frame

    def _popup_frame_style(self):
        return wx.DEFAULT_DIALOG_STYLE

    def _on_frame_close(self, event):
        if self:
            if self._exit_check():
                self.defocus()
                event.Skip()
            else:
                event.Veto()

    def close(self, force=False):
        # Tím se zavolá _on_frame_close() a tam provedeme zbytek.
        return self._popup_frame_.Close(force=force)

    def run(self, lock_key=None):
        """Show the form as a modal dialog.

        Arguments:

          lock_key -- lock the row with the given key

        """
        if (lock_key is not None and
            (not isinstance(self._data, pytis.data.DBDataDefault) or
             self._data.arguments() is not None)):
            lock_key = None
        try:
            busy_cursor(True)
            try:
                if lock_key is not None:
                    if not self._lock_record(lock_key):
                        return None
                pytis.form.unlock_callbacks()
                frame = self.Parent
                frame.SetTitle(self.title())
                frame.SetClientSize(self.GetSize())
            finally:
                busy_cursor(False)
            frame.ShowModal()
        finally:
            if ((self._governing_transaction is None and self._transaction is not None and
                 self._result is None)):
                db_operation(self._transaction.rollback)
            self._governing_transaction = None
            self._transaction = None
        result = self._result
        self._close(force=True)
        return result


class TitledForm(object):
    """Mix-in třída pro formuláře s titulkem.

    Lze využít buďto pouze metodu '_create_caption()', která vytváří samotný
    text titulku, nebo metodu '_create_title_bar()', která přidává 3d panel.

    """
    _TITLE_BORDER_WIDTH = 2

    def _create_caption(self, parent=None, size=None):
        # Create the title text as 'wxStaticText' instance.
        text = self.title()
        if parent is None:
            parent = self
        caption = wx.StaticText(parent, -1, text, style=wx.ALIGN_CENTER)
        if size is None:
            size = caption.GetFont().GetPointSize()
        font = wx.Font(size, wx.FONTFAMILY_DEFAULT, wx.FONTSTYLE_NORMAL, wx.FONTWEIGHT_BOLD,
                       encoding=wx.FONTENCODING_DEFAULT)
        caption.SetFont(font)
        width, height, d, e = self.GetFullTextExtent(text, font)
        caption.SetMinSize((width, height))
        return caption

    def _create_title_bar(self):
        """Vytvoř 3d panel s nadpisem formuláře."""
        panel = wx.Panel(self, -1, style=wx.RAISED_BORDER)
        caption = self._create_caption(panel)
        box = wx.BoxSizer()
        box.Add(caption, 1, wx.EXPAND | wx.ALL, self._TITLE_BORDER_WIDTH)
        panel.SetSizer(box)
        box.Fit(panel)
        return panel


class LookupForm(InnerForm):
    """Formulář s vyhledáváním a tříděním."""

    UNSORT = 'UNSORT'
    """Constant for 'COMMAND_SORT' 'direction' argument indicationg unsorting."""

    def _init_attributes(self, filter=None, sorting=None, columns=None, grouping=None,
                         profile_id=None, condition=None, arguments=None,
                         query_field_values=None, **kwargs):
        """Process constructor keyword arguments and initialize the attributes.

        Arguments:

          filter -- initial filter condition as a 'pytis.data.Operator'
            instance.  This filter is indicated to the user and can be modified
            as any other user-defined filter (as opposed to the 'condition'
            defined below).  If not None, overrides the filter of the default
            form profile.
          sorting -- specification of initial form sorting in the same format
            as the argument 'sorting' of the 'Profile' constructor.  If not
            None, overrides the sorting of the default form profile.
          columns -- specification of initial form columns in the same format
            as the argument 'columns' of the 'Profile' constructor.  If not
            None, overrides the columns of the default form profile.  Columns
            are actually used only by some derived classes (table forms).
          grouping -- specification of initial visual grouping of table rows in
            the same format as the argument 'grouping' of the 'Profile'
            constructor.  If not None, overrides the grouping of the default
            form profile.  Grouping is actually used only by some derived
            classes (table forms).
          profile_id -- id of the initial profile to be loaded.  If not None, it
            must be one of the available system profiles (defined in
            specification) and the arguments 'filter', sorting', 'columns',
             and 'grouping' must be None (they are all determined by
            the profile).
          condition -- 'pytis.data.Operator' instance filtering the rows of the
            underlying data object.  This filter is not indicated to the user
            nor is there a chance to turn it off.
          arguments -- dictionary of table function call arguments, with
            function argument identifiers as keys and 'pytis.data.Value'
            instances as values.  Useful only when the table is actually a row
            returning function, otherwise ignored.
          query_field_values -- sequence of name/value pairs or a dictionary
            containing initial values to be used for query fields (as inner
            Python values, not pytis.data.Value instances).  Only makes sense
            if the form specification defines `query_fields'.  Query fields,
            which don't have their initial values defined by this argument,
            will use their default value from the query field specification.
          kwargs -- arguments passed to the parent class

        If one of 'filter', 'sorting', 'columns' or 'grouping' is set, an
        additional profile named "Initial Profile" is added to form profiles
        with given parameters and this profile becomes the initial profile
        regardless the default profile specification or user settings.  It is
        not possible to save this profile and no parameters are influenced by
        previous user specific form settings.

        """
        super_(LookupForm)._init_attributes(self, **kwargs)
        assert columns is None or isinstance(columns, (list, tuple))
        self._transaction_close_scheduled = False
        # Create a Profile instance representing the form constructor
        # arguments.  Note, that the default profile is not necessarily the
        # initially selected profile.
        self._default_profile = Profile('__default_profile__', _(u"Default profile"))
        self._profiles = self._load_profiles()
        if filter or sorting or columns or grouping:
            assert profile_id is None
            # When profile parameters were passed to the constructor, create an
            # additional profile according to these paramaters.
            initial_profile = Profile('__constructor_profile__', _(u"Initial profile"),
                                      filter=filter, sorting=sorting, columns=columns,
                                      grouping=grouping)
            self._profiles.insert(0, initial_profile)
        else:
            # Note, that the profile 0 may not be the same as
            # self._default_profile, but a saved user customized version.
            default_profile = self._profiles[0]
            if profile_id:
                assert profile_id in [p.id() for p in self._profiles], profile_id
            else:
                profile_id = (self._get_saved_setting('initial_profile') or
                              self._view.profiles().default())
            if profile_id:
                initial_profile = (find(profile_id, self._profiles, key=lambda p: p.id()) or
                                   default_profile)
            else:
                initial_profile = default_profile
            if initial_profile.errors():
                # This may actually happen only for user defined profiles,
                # because invalid user customized system profiles are replaced
                # by the original system profiles automatically by profile
                # manager when loaded.
                wx.CallAfter(self._handle_invalid_profile, initial_profile)
                initial_profile = self._default_profile
        self._initial_profile = initial_profile
        # Profile instances may contain None values to denote default
        # values.  We need to remember the corresponding real values
        # to be able to compare profiles with the current form state
        # in '_current_profile_changed()'.  We rely on the fact that
        # '_apply_profile_parameters()' substitutes None values by
        # their corresponding default values and we don't want to
        # repeat this logic anywhere else.  Thus we apply the default
        # profile first and store the resulting profile parameters.
        # The user visible initial profile is applied in the later
        # call if necessary.
        self._apply_profile_parameters(self._default_profile)
        self._default_profile_parameters = self._profile_parameters_to_save()
        if initial_profile is not self._default_profile:
            self._apply_profile_parameters(initial_profile)
        self._lf_initial_sorting = self._lf_sorting
        # _lf_condition represents a static condition given by the constructor
        # argument, whereas _lf_filter represents the filtering condition,
        # which is part of the current user profile.  There is also a third
        # condition -- the one defined by the specification, but this one is
        # not visible to the form at all -- it is applied at the level of the
        # data object.
        self._lf_condition = condition
        self._lf_search_condition = None
        self._lf_provider_condition = None
        self._arguments = arguments
        self._query_field_values = query_field_values and dict(query_field_values) or None
        self._apply_providers(initial=True)
        self._lf_select_count_ = None
        self._init_select(async_count=True)

    def _lf_count(self, min_value=None, timeout=None):
        if self._lf_select_count_ is None or isinstance(self._lf_select_count_, int):
            result = self._lf_select_count_
        else:
            count, finished = self._lf_select_count_.count(min_value=min_value, timeout=timeout)
            if finished:
                self._lf_select_count_ = count
            result = count
        return result

    def _new_form_kwargs(self):
        return dict(condition=self._lf_condition, sorting=self._lf_sorting,
                    arguments=self._current_arguments())

    def _form_log_info(self):
        return 'sort=%s, filter=%s' % (self._lf_sorting, self._lf_filter,)

    def _default_sorting(self):
        sorting = self._view.sorting()
        if sorting is None:
            sorting = tuple([(k.id(), pytis.data.DESCENDANT)
                             for k in self._data.key()
                             if self._view.field(k.id()) is not None])
        return sorting

    def _current_condition(self, filter=None, display=False):
        conditions = (self._lf_condition,
                      filter or self._lf_filter,
                      self._lf_provider_condition)
        conditions = [c for c in conditions if c is not None]
        if len(conditions) == 0:
            return None
        elif len(conditions) == 1:
            return conditions[0]
        else:
            return pytis.data.AND(*conditions)

    def _current_arguments(self):
        return self._arguments

    def _query_fields_row(self):
        # This class doesn't support query fields UI.  If the specification
        # defines query fields, their values may be passed to the form
        # constructor as 'query_field_values' argument, otherwise the default
        # field value is used.  See list.py for an overloaded version, which
        # supports query fields UI.
        values = self._query_field_values or {}

        def value(f):
            t = f.type() or pytis.data.String()
            try:
                v = values[f.id()]
            except KeyError:
                v = f.default()
                if callable(v):
                    v = v()
            return pytis.data.Value(t, v)
        return pytis.data.Row([(f.id(), value(f)) for f in self._view.query_fields().fields()])

    def _provider_kwargs(self):
        kwargs = {}
        if self._view.query_fields():
            kwargs['query_fields'] = self._query_fields_row()
        return kwargs

    def _apply_providers(self, initial=False):
        kwargs = self._provider_kwargs()
        if 'query_fields' in kwargs and kwargs['query_fields'] is None:
            # If _query_fields_row() returns None, it means that the
            # query fields are not initialized yet and autoinit is off.
            # This happens during form initialization.  In this case we
            # want to display an empty form without calling the provider
            # functions at all.
            # Note that we want to show empty form even if neither
            # condition_provider nor argument_provider is defined as
            # the application may solely rely on on_apply or use query
            # fields in side form conditions, row styles etc.
            # UNKNOWN_ARGUMENTS result in an empty dummy select without
            # calling the underlying database function.
            self._arguments = self._data.UNKNOWN_ARGUMENTS
            # We use an empty OR() condition (always false) to make the
            # select return no data.  It might seem better to have
            # something like self._data.UNKNOWN_CONDITION, but it doesn't
            # really make much difference in case of tables/views.
            self._lf_provider_condition = pytis.data.OR()
        else:
            argument_provider = self._view.argument_provider()
            if argument_provider:
                self._arguments = argument_provider(**kwargs)
                if initial and self._arguments is None:
                    raise Form.InitError()
            condition_provider = self._view.condition_provider()
            if condition_provider:
                self._lf_provider_condition = condition_provider(**kwargs)
            else:
                # Make sure to unset pytis.data.OR() set above.
                self._lf_provider_condition = None

    def _load_profiles(self):
        return pytis.form.app.profile_manager.load_profiles(
            self._profile_spec_name(), self._form_name(),
            self._view, self._data, self._default_profile,
        )

    def _on_idle(self, event):
        super(LookupForm, self)._on_idle(event)
        if not self._transaction_close_scheduled and not pytis.form.app.headless:
            self._on_idle_close_transactions()
            self._transaction_close_scheduled = True

    def _on_idle_close_transactions(self):
        pytis.data.DBTransactionDefault.close_transactions()
        wx.CallLater(1000, self._on_idle_close_transactions)

    def _init_transaction_timeouts(self, data):
        if self._governing_transaction is None:
            def timeout_callback():
                f = pytis.form.app.current_form(inner=False)
                if isinstance(f, pytis.form.CodebookForm) and f is not self:
                    f.close()
                try:
                    data.close()
                    if self._transaction is not None:
                        db_operation(self._transaction.rollback)
                        self._transaction = None
                except pytis.data.DBException:
                    pass
        else:
            timeout_callback = None
        self._transaction_timeout_callback = timeout_callback

    def _init_data_select(self, data, async_count=False):
        self._init_transaction_timeouts(data)
        return data.select(condition=self._current_condition(display=True),
                           columns=self._select_columns(),
                           sort=self._lf_sorting,
                           arguments=self._current_arguments(),
                           transaction=self._open_transaction(), reuse=False,
                           async_count=async_count,
                           timeout_callback=self._transaction_timeout_callback)

    def _init_select(self, async_count=False):
        success, self._lf_select_count_ = db_operation(self._init_data_select, self._data,
                                                       async_count)
        if not success:
            log(EVENT, 'DB operation failed.')
            raise self.InitError()
        # Make sure at least one line is returned (if any is actually present),
        # otherwise segfault may happen when committing an edited line.
        return self._lf_count(timeout=0, min_value=1)

    def _sorting_columns(self):
        return [cid for cid, direction in self._lf_sorting]

    def _sorting_position(self, cid):
        try:
            return self._sorting_columns().index(cid)
        except ValueError:
            return None

    def _sorting_direction(self, cid):
        pos = self._sorting_position(cid)
        if pos is not None:
            return self._lf_sorting[pos][1]
        else:
            return None

    def _lf_sfs_columns(self):
        return pytis.form.sfs_columns(self._view.fields(), self._data)

    def _search(self, condition, direction, row_number=None, report_failure=True,
                initial_shift=False):
        if initial_shift:
            if direction == pytis.data.FORWARD:
                start_row_number = max(row_number - 1, 0)
            else:
                start_row_number = min(row_number + 1, self._lf_count(min_value=(row_number + 1)))
        else:
            start_row_number = row_number
        self._search_adjust_data_position(start_row_number)
        data = self._data
        skip = data.search(condition, direction=direction, transaction=self._open_transaction())
        if skip == 0:
            log(EVENT, 'Record not found')
            if report_failure:
                app.echo(_(u"Record not found"), kind='error')
            result = None
        else:
            if initial_shift:
                if direction == pytis.data.FORWARD:
                    skip = skip + (start_row_number - row_number)
                else:
                    skip = skip + (row_number - start_row_number)
            result = skip
            log(EVENT, 'Record found:', skip)
            self._search_skip(result, direction)
        return result

    def _search_adjust_data_position(self, row_number):
        pass

    def _search_skip(self, skip, direction):
        data = self._data
        data.skip(skip - 1, direction=direction)
        row = data.fetch(direction)
        self._select_row(row)

    def _cmd_jump(self):
        max_value = self._lf_count()
        if max_value > 0:
            result = app.input_form(title=_(u"Jump to record"), fields=(
                Field('row', _("Row number"),
                      type=pytis.data.Integer(not_null=True, minimum=1, maximum=max_value),
                      descr=_("Row number between 1 and %d", max_value)),
            ))
            if result:
                self.select_row(result['row'].value() - 1)

    def _cmd_first_record(self):
        self.select_row(0)

    def _cmd_last_record(self):
        self.select_row(self._lf_count() - 1)

    def _cmd_search(self, next=False, back=False):
        condition = self._lf_search_condition
        if condition is not None and next:
            direction = back and pytis.data.BACKWARD or pytis.data.FORWARD
        else:
            with Refreshable.block_refresh():
                direction, condition = pytis.form.app.run_dialog(
                    SearchDialog, self._lf_sfs_columns(), self.current_row(),
                    col=self._current_column_id(),
                    condition=self._lf_search_condition,
                )
        if direction is not None:
            self._lf_search_condition = condition
            self._search(condition, direction)

    def _compute_aggregate(self, operation, column_id, condition):
        condition = self._current_condition(filter=condition)
        return self._data.select_aggregate((operation, column_id), condition,
                                           transaction=self._open_transaction())

    def _filtered_columns(self):
        columns = []

        def analyze(operator):
            for a in operator.args():
                if isinstance(a, pytis.data.Operator):
                    analyze(a)
                elif isinstance(a, basestring) and a not in columns:
                    columns.append(a)
        if self._lf_filter is not None:
            analyze(self._lf_filter)
        return columns

    def _apply_filter(self, condition):
        self._lf_filter = condition
        self._init_select(async_count=False)
        self.select_row(self._current_key())

    def _save_profile(self, profile):
        pytis.form.app.profile_manager.save_profile(
            self._profile_spec_name(), self._form_name(), profile,
        )

    def _apply_profile_parameters(self, profile):
        """Set the form state attributes according to given 'Profile' instance.

        This method doesn't actually refresh the form display.  It only sets
        the profile related form attributes to match the parameters of given
        profile.  Only the attributes recognized by the class are set in the
        base class.  Derived classes, which also have attributes for other
        profile parameters should override this method to set their specific
        attributes too.

        Note: The attribute '_current_profile' always contains the 'Profile'
        instance passed to the last '_apply_profile_parameters()' call.  It is
        not updated when the form state later changes due to user actions.

        """
        sorting = profile.sorting()
        if sorting is None:
            sorting = self._default_sorting()
        self._current_profile = profile
        self._lf_filter = profile.filter()
        self._lf_sorting = sorting

    def _apply_profile(self, profile, refresh=True):
        """Change the current form state according to given 'Profile' instance.

        Call '_apply_profile_parameters()' and redraw the form to match the new
        state.

        """
        # BEWARE!  This method is overriden in ListForm without calling the
        # super class method.
        self._apply_profile_parameters(profile)
        self._init_select(async_count=False)
        self.select_row(self._current_key())

    def _create_profile(self, id, name):
        return Profile(id, name, **self._profile_parameters_to_save())

    def _profile_parameters_to_save(self):
        """Return the profile parameters representing the current state of the form.

        The returned dictionary is passed to 'Profile' constructor as keyword
        arguments when saving a profile.  Note, that the profile instance
        stored in `self._current_profile' may not have the same parameters if
        the user changed them since he switched to that profile
        (`self._current_profile' instance is not updated when the form state
        changes, it representes the previously saved state).

        """
        return dict(filter=self._lf_filter, sorting=self._lf_sorting)

    def _current_profile_changed(self):
        for param, current_value in self._profile_parameters_to_save().items():
            original_value = getattr(self._current_profile, param)()
            if original_value is None:
                original_value = self._default_profile_parameters[param]
            if self._profile_parameter_changed(param, current_value, original_value):
                return True
        return False

    def _profile_parameter_changed(self, param, current_value, original_value):
        # Allow overriding the comparison in derived classes.
        return current_value != original_value

    def _is_user_defined_profile(self, profile):
        return profile.id().startswith(pytis.form.FormProfileManager.USER_PROFILE_PREFIX)

    def _handle_invalid_profile(self, profile):
        keep, remove = (_(u"Keep"), _(u"Remove"))
        answer = app.question(
            _("User profile \"%s\" is invalid.\n"
              "The form specification has probably changed and the saved\n"
              "profile is incompatible now. You can either remove the profile,\n"
              "or keep it and ask the administrator to update it.", profile.title()),
            title=_("Invalid profile"),
            #icon=Question.ICON_ERROR,
            answers=(keep, remove), default=keep,
            content='\n'.join(['%s: %s' % (param, error) for param, error in profile.errors()]),
        )
        if answer == remove:
            if self._is_user_defined_profile(profile):
                self._profiles.remove(profile)
            else:
                index = self._profiles.index(profile)
                if profile.id() == self._default_profile.id():
                    profile = self._default_profile
                else:
                    profile = find(profile.id(), self._view.profiles().unnest(),
                                   key=lambda p: p.id())
                self._profiles[index] = profile
            pytis.form.app.profile_manager.drop_profile(self._profile_spec_name(),
                                                        self._form_name(), profile.id())

    def _cmd_apply_profile(self, index):
        profile = self._profiles[index]
        if profile.errors():
            self._handle_invalid_profile(profile)
        else:
            orig_profile = self._current_profile
            try:
                self._apply_profile(profile)
            except UserBreakException:
                self._apply_profile(orig_profile)
        self.focus()

    def _cmd_save_new_profile(self, title):
        if title in [profile.title() for profile in self._profiles]:
            app.echo(_(u"Profile of this name already exists."), kind='error')
            return
        profile_id = pytis.form.app.profile_manager.new_user_profile_id(self._profiles)
        profile = self._create_profile(profile_id, title)
        self._profiles.append(profile)
        self._save_profile(profile)
        self._current_profile = profile
        app.echo(_(u"Profile saved as '%s'.") % title)
        self.focus()

    def _can_rename_profile(self, title):
        return self._is_user_defined_profile(self._current_profile)

    def _cmd_rename_profile(self, title):
        if title in [p.title() for p in self._profiles if p is not self._current_profile]:
            app.echo(_(u"Profile of this name already exists."), kind='error')
            return
        index = self._profiles.index(self._current_profile)
        profile = self._create_profile(self._current_profile.id(), title)
        self._current_profile = self._profiles[index] = profile
        self._save_profile(self._current_profile)
        app.echo(_(u"Profile saved as '%s'.") % title)
        self.focus()

    def _can_update_profile(self):
        return (self._current_profile.id() != '__constructor_profile__' and
                self._current_profile_changed())

    def _cmd_update_profile(self):
        current = self._current_profile
        index = self._profiles.index(current)
        profile = self._create_profile(current.id(), current.title())
        self._profiles[index] = profile
        self._save_profile(profile)
        self._current_profile = profile

    def _can_delete_profile(self):
        return self._is_user_defined_profile(self._current_profile)

    def _cmd_delete_profile(self):
        pytis.form.app.profile_manager.drop_profile(self._profile_spec_name(), self._form_name(),
                                                    self._current_profile.id())
        self._profiles.remove(self._current_profile)
        profile_id = self._get_saved_setting('initial_profile') or self._view.profiles().default()
        profile = find(profile_id, self._profiles, key=lambda p: p.id()) or self._profiles[0]
        self._apply_profile(profile)

    def _can_reload_profile(self):
        return self._current_profile_changed()

    def _cmd_reload_profile(self):
        self._apply_profile(self._current_profile)

    def _can_reset_profile(self):
        return not self._is_user_defined_profile(self._current_profile)

    def _cmd_reset_profile(self):
        index = self._profiles.index(self._current_profile)
        profile_id = self._current_profile.id()
        if profile_id == self._default_profile.id():
            profile = self._default_profile
        elif profile_id == '__constructor_profile__':
            profile = self._initial_profile
        else:
            profile = find(profile_id, self._view.profiles().unnest(), key=lambda p: p.id())
        pytis.form.app.profile_manager.drop_profile(self._profile_spec_name(),
                                                    self._form_name(), profile_id)
        self._profiles[index] = profile
        self._apply_profile(profile)

    def _can_set_initial_profile(self):
        return self._current_profile.id() not in (self._get_saved_setting('initial_profile'),
                                                  '__constructor_profile__')

    def _cmd_set_initial_profile(self):
        profile_id = self._current_profile.id()
        self._set_saved_setting('initial_profile', profile_id)

    def _cmd_filter(self, condition=None):
        if condition:
            perform = True
        else:
            perform, condition = pytis.form.app.run_dialog(
                FilterDialog, self._lf_sfs_columns(),
                self.current_row(), self._compute_aggregate,
                col=self._current_column_id(),
                condition=self._lf_filter,
            )
        if perform and condition != self._lf_filter:
            self.filter(condition)

    def _can_unfilter(self):
        return self._lf_filter is not None

    def _cmd_unfilter(self):
        self.filter(None)

    def _cmd_filter_by_value(self, column_id, value):
        if column_id not in [c.id() for c in self._lf_sfs_columns()]:
            app.echo(_(u"This column can not be used for filtering."), kind='error')
        self.filter(pytis.data.EQ(column_id, value), append=True)

    def _cmd_sort(self, col=None, direction=None, primary=False):
        """Change sorting.

        Arguments:

          col -- column id to add/remove from current sorting or 'None' for
            global changes such as switching sorting completely.
          direction -- sorting direction as one of constants
            'pytis.data.ASCENDENT', 'pytis.data.DESCENDANT' or
            'pytis.form.LookupForm.UNSORT' (remove given column from sorting) .
            If None, a dialog is displayed to select sorting interactively.
          primary -- iff true, given column will be selected as the primary
            (and the only) sorting column.  Otherwise it will be just added to
            the end of current sorting specification.

        """
        sorting = self._determine_sorting(col, direction, primary)
        if sorting is not None and sorting != self._lf_sorting:
            self._lf_sorting = sorting
            self.select_row(self._current_key())
        return sorting

    def _can_sort(self, col=None, direction=None, primary=False):
        # `col' je zde identifikátor sloupce.
        sorting_columns = tuple(self._sorting_columns())
        if direction == self.UNSORT:
            return sorting_columns and (col is None or col in sorting_columns)
        elif direction is not None and col is not None:
            position = self._sorting_position(col)
            current_direction = self._sorting_direction(col)
            if primary:
                return position != 0 or direction != current_direction
            else:
                return position != 0 and direction != current_direction and sorting_columns
        else:
            return True

    def _determine_sorting(self, col, direction, primary):
        if col is None and direction == self.UNSORT:
            sorting = ()
        elif col is None or direction is None:
            columns = self._lf_sfs_columns()
            if col is None and self._lf_sorting:
                col = self._sorting_columns()[0]
            sorting = pytis.form.app.run_dialog(SortingDialog, columns, self._lf_sorting,
                                                col=col, direction=direction)
            if sorting is None:
                return None
            elif sorting == ():
                sorting = self._lf_initial_sorting
        elif col is not None:
            if ((not self._data.find_column(col) or
                 not self._data.permitted(col, pytis.data.Permission.VIEW))):
                app.echo(_(u"This column can not be used for sorting."), kind='error')
                return None
            position = self._sorting_position(col)
            sorting = xlist(self._lf_sorting)
            if direction == self.UNSORT:
                del sorting[position]
            else:
                assert direction in (pytis.data.ASCENDENT, pytis.data.DESCENDANT)
                new_sort_spec = (col, direction)
                if primary and position != 0:
                    sorting = (new_sort_spec,)
                elif position is None:
                    sorting.append(new_sort_spec)
                else:
                    sorting[position] = new_sort_spec
            sorting = tuple(sorting)
        else:
            raise ProgramError("Invalid sorting arguments:", (col, direction))
        return sorting

    # Veřejné metody

    def filter(self, condition, append=False):
        """Apply given filtering condition.

        Iff 'append' is True, add the condition to the current filter,
        otherwise replace the current filter by given condition.

        """
        if append and self._lf_filter:
            condition = pytis.data.AND(self._lf_filter, condition)
        old_filter = self._lf_filter
        start_time = time.time()
        success = False
        try:
            self._apply_filter(condition)
            success = True
        except UserBreakException:
            self._lf_filter = old_filter
            self._apply_filter(self._lf_filter)
        if not self._is_user_defined_profile(self._current_profile) \
                and condition != self._current_profile.filter():
            title = _(u"Unnamed profile")
            profile = find(title, self._profiles, key=lambda p: p.title())
            if profile:
                pytis.form.app.profile_manager.drop_profile(self._profile_spec_name(),
                                                            self._form_name(), profile.id())
                self._profiles.remove(profile)
            self._cmd_save_new_profile(title)
        log(EVENT, "%s filter application in %.3f seconds" %
            ("Successful" if success else "Unsuccessful", time.time() - start_time,))

    def apply_profile(self, profile_id):
        """Apply the profile given by profile_id.

        Arguments:

          profile_id -- id of the profile to be loaded.  It must be one of the
            available profiles (defined in specification or user defined
            profiles).

        """
        profile = find(profile_id, self._profiles, key=lambda p: p.id())
        if not profile:
            raise ProgramError("Unknown profile '%s'" % profile_id)
        self._apply_profile(profile)

    def data(self, init_select=True):
        """Return a new instance of the data object used by the form.

        The instance will have the data select initialized with the current
        profile parameters (filter condition, sorting etc).  This is often
        practical within application defined procedures, which retrieve this
        data object through the 'RecordForm.Record.data()' method.

        Arguments:

          init_select -- iff True the instance will have the data select
            initialized with the current filter condition and all its
            attributes, such as sorting etc.  This is often practical within
            application defined procedures, which retrieve this data object
            through the 'RecordForm.Record.data()' method.

        """
        data = super(LookupForm, self).data()
        if init_select:
            self._init_data_select(data)
        return data

    def condition(self):
        """Vrať specifikaci aktuální podmínky výběru dat.

        Podmínka je vrácena v podobě požadované argumentem 'condition'
        metody 'pytis.data.Data.select()'.

        """
        return self._current_condition()

    def sorting(self):
        """Return the current sorting specification."""
        return self._lf_sorting

    def view(self):
        """Return the form's presentation specification as a 'ViewSpec' instance."""
        return self._view

    def profiles(self):
        """Return the current form profiles as a list."""
        return self._profiles

    def current_profile(self):
        """Return the current form profile as 'pytis.presentation.Profile' instance."""
        return self._current_profile

    # Implementation of Public API 'pytis.api.Form'.

    @property
    def api_condition(self):
        return self._current_condition()

    @property
    def api_arguments(self):
        return self._current_arguments()

    @property
    def api_sorting(self):
        return self._lf_sorting

    @property
    def api_profile(self):
        return self._current_profile

    @property
    def api_query_fields(self):
        @pytis.api.implements(pytis.api.QueryFields)
        class QueryFields:
            def __init__(self, row):
                self._row = row

            @property
            def api_row(self):
                return self._row
        # TODO: Probably makes sense to keep one QueryFields instance for form life.
        if self._view.query_fields():
            return QueryFields(self._query_fields_row()).provider()
        else:
            return None


class RecordForm(LookupForm):
    """Formulář schopný nějakým způsobem zobrazit aktuální záznam."""

    _SINGLE_LINE = False

    CALL_SELECTION = 'CALL_SELECTION'
    """Konstanta callbacku výběru (změny aktuálního) záznamu.

    Argumentem callbackové funkce je nově vybraný záznam jako instance
    'RecordForm.Record'.

    """
    class Record(PresentedRow):
        """PresentedRow extension allowing access to public form API from application code."""

        def __init__(self, form, *args, **kwargs):
            self._form = form
            super(RecordForm.Record, self).__init__(*args, **kwargs)

        @property
        def form(self):
            """Returns pytis.api.Form representation of the form from which the row comes."""
            return self._form.provider()

        def data(self, init_select=True):
            # Return a new instance rather than giving the internally used data object.
            # Moreover this instance will have the select initialized in LookupForm.
            return self._form.data(init_select=init_select)

    def _init_attributes(self, prefill=None, select_row=None, _new=False, **kwargs):
        """Process constructor keyword arguments and initialize the attributes.

        Arguments:

          prefill -- a dictionary of values used to prefill the current form
            row.  The meaning is the same as for the same 'PresentedRow'
            constructor argument.
          select_row -- The initially selected row.  Same as the argument
            'position' of 'select_row()'.
          kwargs -- arguments passed to the parent class

        """
        assert prefill is None or isinstance(prefill, dict)
        self._prefill = prefill
        self._select_row_argument = select_row
        self._initial_select_row_called = False
        self._action_access_groups_cache = {}
        super_(RecordForm)._init_attributes(self, **kwargs)
        if select_row is not None:
            row = self._find_row(select_row)
            # assert row is not None, select_row
            if not row:
                # TODO: This has not been caught in the past, but it may lead to some
                # non-trivial debugging when select_row is invalid for some reason.
                # It seems an assertion would be appropriate here, but that might also
                # break things.  So for now we at least log the problem.
                log(OPERATIONAL, "Record referenced by select_row not found:", select_row)
        else:
            row = None
        self._row = self.record(row, prefill=prefill, new=_new)

    def _on_idle(self, event):
        super(RecordForm, self)._on_idle(event)
        if not self._initial_select_row_called:
            self._initial_select_row_called = True
            self._initial_select_row()

    def _initial_select_row(self):
        # The initial select_row() call is important mainly for the case
        # that the row corresponding to the 'select_row' argument does
        # not exist (which is ignored by self._find_row() called form
        # _init_attributes()).  This method must be called from _on_idle()
        # because it may call _apply_profile() which requires the grid
        # to be fully initialized in BrowseForm.
        if self._select_row_argument is not None:
            self.select_row(self._select_row_argument)

    def _signal_update(self):
        pass

    def _select_columns(self):
        return None

    def _find_row_by_number(self, row_number):
        # row_number starts with 0
        data = self._data
        current_row_number = data.last_row_number()

        def find():
            data.rewind()
            data.skip(row_number)
            return data.fetchone()

        def cleanup():
            data.rewind()
            data.skip(current_row_number + 1)

        success, row = db_operation(find)
        db_operation(cleanup)
        if not success or not row:
            return None
        else:
            return row

    def _find_row_by_values(self, values):
        data = self._data
        current_row_number = data.last_row_number()

        def find():
            try:
                data.rewind()
                position = data.search(pytis.data.AND(*[pytis.data.EQ(key, value)
                                                        for key, value in values]),
                                       transaction=self._open_transaction(),
                                       arguments=self._current_arguments())
                if position == 0:
                    return None
                else:
                    data.skip(position - 1)
                    return data.fetchone()
            finally:
                data.rewind()
                data.skip(current_row_number + 1)

        success, row = db_operation(find)
        if not success or not row:
            return None
        else:
            return row

    def _get_row_number(self, row):
        """Vrať číslo řádku odpovídající dané instanci 'pytis.data.Row'.

        Pokud odpovídaící řádek není nalezen, vrať None.

        """
        data = self._data
        key = data.key()[0].id()

        def dbop():
            data.rewind()
            return data.search(pytis.data.EQ(key, row[key]),
                               transaction=self._open_transaction(),
                               arguments=self._current_arguments())
        try:
            success, result = db_operation(dbop)
        except pytis.data.Data.UnsupportedOperation:    # MemData
            key_value = row[key].value()
            data.select(arguments=self._current_arguments())
            success = False
            result = 1
            while True:
                r = data.fetchone()
                if r is None:
                    return
                if r[key].value() == key_value:
                    success = True
                    break
                result += 1
        if not success or result == 0:
            return None
        else:
            return result - 1

    def _find_row(self, position):
        # Return the data row instance corresponding to given 'select_row()' argument.
        if position is None or isinstance(position, pytis.data.Row):
            row = position
        elif isinstance(position, int):
            row = self._find_row_by_number(position)
        elif isinstance(position, (tuple, pytis.data.Value)):
            row = self._find_row_by_values(zip([c.id() for c in self._data.key()],
                                               xtuple(position)))
        elif isinstance(position, dict):
            row = self._find_row_by_values(position.items())
        else:
            raise ProgramError("Invalid 'position':", position)
        return row

    def _select_row(self, row):
        # Set the form data according to given *data* row.
        self._row.set_row(row)
        self._run_callback(self.CALL_SELECTION, self._row)
        return True

    def _current_key(self):
        the_row = self.current_row()
        if the_row is not None:
            data_row = the_row.original_row(initialized=False)
            if data_row is None:
                data_row = the_row.row()
            return data_row.columns([c.id() for c in self._data.key()])
        return None

    def _current_column_id(self):
        return None

    def _new_form_kwargs(self):
        return dict(arguments=self._current_arguments())

    def _lock_record(self, key):
        success, locked = db_operation(self._data.lock_row, key,
                                       transaction=self._open_transaction())
        if success and locked is not None:
            log(EVENT, 'Record is locked')
            app.message(title=_(u"The record is locked."),
                        message=(_(u"The record is locked.") + '\n\n' +
                                 _(u"Another user is currently editing this record.\n"
                                   u"Please, try repeating the action later.")))
            return False
        else:
            return True

    def _on_closed_connection(self):
        app.error(_("The database connection was closed because of long inactivity.") +
                  "\n" + _("Please close the form using the Cancel button."))

    def _check_record(self, row):
        # Perform integrity checks for given PresentedRow instance.
        fields = [f.id() for f in self._view.fields()]
        for check in self._view.check():
            try:
                result = check(row)
            except (pytis.data.DBRetryException, pytis.data.DBSystemException):
                self._on_closed_connection()
                return False, None, None
            if result is not None:
                if isinstance(result, (tuple, list)):
                    # ViewSpec.check may return a pair (field_id, message).
                    field_id, msg = result
                elif result in fields:
                    # Older definition of ViewSpec.check allowed printing the
                    # message from inside the check function and returning only
                    # field_id, so we don't print anything to prevent overwriting
                    # the already displayed message.
                    field_id, msg = result, None
                else:
                    # Otherwise ViewSpec.check returns an error message.
                    field_id, msg = None, result
                log(EVENT, 'Integrity check failed:', field_id)
                return False, field_id, msg
        return True, None, None

    def _record_data(self, row, permission=None, updated=False):
        # We must retrieve all the values first, in order to recompute all
        # fields, even those which are not present in the form.  Only then we
        # can filter them to retain only those which are actually changed.
        rdata = [(f.id(), pytis.data.Value.reconceal(row[f.id()])) for f in row.fields()
                 if self._data.find_column(f.id()) is not None and
                 (permission is None or self._data.permitted(f.id(), permission))]
        if updated:
            original_row = row.original_row(initialized=False)
            rdata = [(key, value) for (key, value) in rdata
                     if original_row[key].value() != value.value()]
        return pytis.data.Row(rdata)

    def _row_copy_prefill(self, the_row):
        # Create a copy of the row, but exclude key columns and computed
        # columns which depend on key columns.
        if the_row:
            keys = [c.id() for c in self._data.key()]
            prefill = {}
            for cid in the_row.keys():
                fspec = self._view.field(cid)
                if cid in keys or fspec.nocopy():
                    continue
                computer = fspec.computer()
                if computer:
                    skip = False
                    for dep in computer.depends():
                        if dep in keys:
                            skip = True
                            break
                    if skip:
                        continue
                prefill[cid] = the_row[cid]
        else:
            prefill = {}
        return prefill

    def _dualform(self):
        # Pokud je formulář součástí duálního formuláře, vrať jej, jinak None.
        top = pytis.form.app.top_window()
        if isinstance(top, pytis.form.DualForm):
            main, side = top.main_form(), top.side_form()
            if self in (main, side):
                return top
            if isinstance(side, pytis.form.MultiForm) and self in side.forms():
                return top
        return None

    def _secondary_context_form(self):
        dual_form = self._dualform()
        if dual_form:
            if dual_form.main_form() is self:
                form = dual_form.side_form()
                if isinstance(form, pytis.form.MultiForm):
                    form = form.active_form()
            else:
                form = dual_form.main_form()
        else:
            form = None
        if isinstance(form, RecordForm):
            return form
        else:
            return None

    def _context_action_args(self, action):
        def selection(form):
            result = self.selected_rows()
            if len(result) == 0:
                class CurrentRowIterator(object):
                    """See '_grid.TableRowIterator' for documentation."""

                    def __init__(self, form):
                        self._form = form

                    def __len__(self):
                        return 1

                    def __iter__(self):
                        return iter([self._form.current_row()])

                    @property
                    def form(self):
                        return self._form.provider()

                result = CurrentRowIterator(form)
            return result
        context = action.context()
        if context == ActionContext.RECORD:
            args = (self.current_row(),)
        elif context == ActionContext.SELECTION:
            args = (selection(self),)
        else:
            raise ProgramError("Unsupported action context:", context)
        scontext = action.secondary_context()
        if scontext is not None:
            form = self._secondary_context_form()
            if form is None:
                args += (None,)
            elif scontext == ActionContext.RECORD:
                args += (form.current_row(),)
            elif scontext == ActionContext.SELECTION:
                args += (selection(form),)
            else:
                raise ProgramError("Unsupported action secondary_context:", scontext)
        return args

    def _cleanup(self):
        super(RecordForm, self)._cleanup()
        # PresentedRow may contain references to data objects
        self._row = None

    # Command handling

    def _cmd_new_record(self, copy=False, prefill=None):
        if not self.check_permission(pytis.data.Permission.INSERT, quiet=False):
            return False
        layout_field_ids = self._view.layout().order()
        for field in self._view.fields():
            if field is None:
                continue
            fid = field.id()
            if fid not in layout_field_ids:
                continue
            codebook = field.codebook()
            if codebook and not app.has_access(codebook) and field.computer() is None:
                editable = field.editable()
                if callable(editable):
                    editable = editable(self._row)
                if not editable:
                    continue
                if self._row[fid].type().not_null():
                    app.error(_(u"This form contains the mandatory field %s,\n"
                                u"but you don't have access to its codebook values.",
                                field.label()) + '\n' +
                              _(u"Please contact the access rights administrator."))
                    return False
                else:
                    app.warning(_(u"This form contains the field %s,\n"
                                  u"but you don't have access to its codebook values.",
                                  field.label()) + '\n' +
                                _(u"Please contact the access rights administrator."))
            crypto_name = field.crypto_name()
            if ((crypto_name and self._row[fid].type().not_null() and
                 crypto_name not in app.decrypted_areas())):
                app.error(_(u"This form contains the mandatory field %s,\n"
                            u"but this field is encrypted and the encryption "
                            u"area '%s' has not been unlocked.",
                            field.label(), crypto_name))
                return False

        import copy as copy_
        if prefill is None:
            prefill = self._prefill and copy_.copy(self._prefill) or {}
        if copy:
            copied_row = self.current_row()
            prefill.update(self._row_copy_prefill(copied_row))
        else:
            copied_row = None
        result = app.new_record(self._name, prefill=prefill, copied_row=copied_row)
        if result:
            if not self.select_row(result.row(), quiet=True):
                app.warning(_(u"The inserted record didn't appear in the current view."))

    def _can_edit_record(self):
        return self.current_row() is not None

    def _cmd_edit_record(self):
        if not self.check_permission(pytis.data.Permission.UPDATE, quiet=False):
            return
        row = self.current_row()
        app.edit_record(self._name, row, transaction=row.transaction())
        # TODO: This is here only for the case, that 'on_edit_record()'
        # was processed without actually opening an edit form.  Maybe we
        # should require 'on_edit_record()' to call 'app.refresh()' manually
        # if needed because in most cases it is redundant and it only causes
        # an unnecessary delay.
        self._signal_update()

    def _can_delete_record(self):
        return self.current_row() is not None

    def _cmd_delete_record(self):
        # The return value is used in derived classes!
        if not self.check_permission(pytis.data.Permission.DELETE, quiet=False):
            return False
        if app.delete_record(self._name, self.current_row(), transaction=self._transaction):
            self._signal_update()
            return True
        else:
            return False

    def _cmd_refresh_db(self):
        self._data.refresh()
        self.refresh()

    def _can_context_action(self, action):
        if pytis.config.use_dmp_rights:
            try:
                access_groups = self._action_access_groups_cache[action.id()]
            except KeyError:
                rights = Specification.data_access_rights('action/%s/%s' % (action.id(),
                                                                            self._name))
                if rights is None:
                    access_groups = None
                else:
                    access_groups = rights.permitted_groups(pytis.data.Permission.CALL, None)
                    if None in access_groups:
                        access_groups = None
                self._action_access_groups_cache[action.id()] = access_groups
        else:
            access_groups = action.access_groups()
        if not pytis.data.is_in_groups(access_groups):
            return False
        enabled = action.enabled()
        if callable(enabled):
            args = self._context_action_args(action)
            kwargs = action.kwargs()
            return enabled(*args, **kwargs)
        else:
            return enabled

    def _cmd_context_action(self, action):
        args = self._context_action_args(action)
        kwargs = action.kwargs()
        log(EVENT, 'Calling context action handler:', (args, kwargs))
        action.handler()(*args, **kwargs)
        if action.context() == ActionContext.SELECTION:
            # Clear rows selection to avoid problems when the context action modifies
            # the selected rows and some of these rows dismiss from the form because
            # they no longer match the current filter.  In this case the grid behaves
            # inconsistently - the selection sometimes moves to other rows and
            # sometimes it contains invalid row numbers.
            self.unselect_selected_rows()
        dual = self._dualform()
        if dual:
            # If we are a part of a dual form, refresh the whole form (feels little hacky).
            dual.refresh()
        else:
            self.refresh()
        return True

    def _cmd_import_interactive(self):
        class Separators(pytis.presentation.Enumeration):
            enumeration = (
                ('|', _("Pipe '|'")),
                (',', _("Comma ','")),
                (';', _("Semicolon ';'")),
                ('\t', _("TAB")),
                ('other', _.pgettext('custom', "Other")),
            )
            default = '|'
            selection_type = pytis.presentation.SelectionType.RADIO

        if not self._data.permitted(None, pytis.data.Permission.INSERT):
            app.echo(_(u"Insufficient permissions to insert records to this table."),
                     kind='error')
            return False
        order = self._view.layout().order()
        content = lcg.container(
            lcg.p(_("You will be able to choose the file containing the imported "
                    "data in the next step.  You will be prompted to check and "
                    "confirm each record separately afterwards.")),
            lcg.Section(title=_("Input file format"), content=(
                lcg.p(_("Each row contains a sequence of values separated by the "
                        "separator selected below.")),
                lcg.p(_("The first row must contain a sequence of column "
                        "identifiers. The following rows contain sequences "
                        "of data values for the columns named in the first "
                        "row.")),
                lcg.p(_("Possible column identifiers for this form are:")),
                lcg.fieldset([(lcg.strong(f.column_label()), lcg.code(f.id()))
                              for f in [self._view.field(fid) for fid in order]
                              if f.editable() is not False]),
            )),
        )
        result = app.input_form(title=_(u"Batch import"), fields=(
            Field('separator', _("Common separators"), enumerator=Separators,
                  not_null=True,
                  descr=_("The character used to separate column values "
                          "in each input file row.")),
            Field('custom', _("Custom separator"),
                  check=pytis.presentation.computer(
                      lambda r, separator, custom:
                      _("The value is mandatory when 'Other' is selected above.")
                      if separator == 'other' and not custom else None
                  ),
                  editable=pytis.presentation.computer(
                      lambda e, separator: separator == 'other'
                )),
        ), layout=(content, 'separator', 'custom'))
        if not result:
            return False
        separator = result['separator'].value()
        if separator == 'other':
            separator = result['custom'].value()
        fh = app.open_selected_file(mode='r', filetypes=('csv', 'txt'))
        if not fh:
            app.echo(_(u"No file given. Import aborted."), kind='warning')
            return False
        # Make a local copy of the remote file as long as transparent remote file buffering
        # is deactivated in Application._ExposedFileWrapper.__init__().
        if pytis.remote.client_connection_ok():
            import tempfile
            flocal = tempfile.NamedTemporaryFile()
            filename = flocal.name
            flocal.write(fh.read())
            fh.close()
            fh = flocal
            fh.seek(0)
        with fh:
            columns = [cid.strip() for cid in fh.readline().rstrip('\r\n').split(separator)]
            try:
                types = [self._row[cid].type() for cid in columns]
            except KeyError as e:
                app.error(_("Invalid column id in CSV file, line 1: '{}'").format(e.message))
                return

            class Continue(Exception):
                pass

            def validate(cid, t, value, line_number):
                kwargs = {'format': t.DEFAULT_FORMAT} if isinstance(t, pytis.data.DateTime) else {}
                result, error = t.validate(value, strict=False, **kwargs)
                if error:
                    app.error(_("Invalid data in CSV file, line {}, column {}:")
                              .format(line_number, cid)
                              + '\n' + value + ': ' + error.message() + '\n' +
                              _("Skipping line {}.").format(line_number))
                    raise Continue()
                return result.value()

            def inserted_data():
                for i, line in enumerate(fh):
                    values = line.rstrip('\r\n').split(separator)
                    try:
                        yield {cid: validate(cid, t, value, i + 2)  # i is 0 at line 2...
                               for cid, t, value in zip(columns, types, values)}
                    except Continue:
                        continue
            app.new_record(self._name, prefill=self._prefill, inserted_data=inserted_data())

    def _cmd_open_editor(self, field_id):
        run_form(StructuredTextEditor, self.name(),
                 field_id=field_id, select_row=self.current_key())

    def _can_view_field_pdf(self, field_id):
        field = find(field_id, self._row.fields(), key=lambda f: f.id())
        return field and field.text_format() == TextFormat.LCG

    def _cmd_view_field_pdf(self, field_id):
        import lcg.export.pdf
        row = self.current_row()
        storage = row.attachment_storage(field_id)
        if storage:
            resources = storage.resources(transaction=row.transaction())
        else:
            resources = ()
        content = lcg.Container(lcg.Parser().parse(row[field_id].value() or ''))
        if len(content.sections()) == 1:
            section = content.sections()[0]
            title = section.title()
            content = section.content()
        else:
            field = find(field_id, self._row.fields(), key=lambda f: f.id())
            title = field.label()
        node = lcg.ContentNode('preview', title=title, content=content,
                               resource_provider=lcg.ResourceProvider(dirs=(), resources=resources))
        exporter = lcg.export.pdf.PDFExporter()  # translations=cfg.translation_path)
        context = exporter.context(node, 'cs')
        pdf = exporter.export(context)
        app.launch_file(data=pdf, suffix='.pdf')

    # Public methods

    def record(self, row, **kwargs):
        """Create a new `RecordForm.Record' instance bound to this form."""
        fields = self._view.fields()
        data = self._create_data_object()
        return self.Record(self, fields, data, row, transaction=self._open_transaction(),
                           singleline=self._SINGLE_LINE, **kwargs)

    def select_row(self, position, quiet=False, retry=True):
        """Vyber řádek dle 'position'.

        Argument 'position' může mít některou z následujících hodnot:

          None -- nebude vybrán žádný řádek.
          Nezáporný integer -- bude vybrán řádek příslušného pořadí, přičemž
            řádky jsou číslovány od 0.
          Datový klíč -- bude vybrán řádek s tímto klíčem, kterým je instance
            třídy 'pytis.data.Value' nebo jejich tuple.
          Slovník hodnot -- bude vybrán první nalezený řádek obsahující
            hodnoty slovníku (instance 'pytis.data.Value') v sloupcích určených
            klíči slovníku.
          Instance třídy 'pytis.data.Row' -- bude převeden na datový klíč a
            zobrazen odpovídající řádek.  Instance musí být kompatibilní
            s datovým objektem formuláře.

        Pokud takový záznam neexistuje, zobraz chybový dialog.  Argumentem
        'quiet' lze zobrazení chybového dialogu potlačit.  Tím lze nenalezení
        řádku tiše ignorovat, nebo ošetřit vlastním způsobem na základě
        návratové hodnoty.

        Výběrem je myšlena akce relevantní pro daný typ formuláře (odvozené
        třídy).  Tedy například vysvícení řádku v tabulce, zobrazení záznamu v
        náhledovém formuláři apod.

        Vrací: Pravdu, pokud byl záznam úspěšně nalezen a vybrán, nepravdu v
        opačném případě.

        """
        row = self._find_row(position)
        if row is None and not quiet and not (position is None or
                                              (isinstance(position, tuple) and
                                               len(position) == 1 and
                                               position[0].value() is None)):
            if self._lf_filter and retry:
                profile = find(True, self._profiles, lambda p: p.filter() is None)
                if profile:
                    if app.question(_("The searched record was not found. "
                                      "It may be caused by the active filter.\n"
                                      "Do you want to activate the unfiltered "
                                      "profile %s and try searching again?",
                                      profile.title()),
                                    title=_("Record not found")):
                        self._apply_profile(profile)
                        return self.select_row(position, retry=False)
                    else:
                        return False
            app.warning(_("Record not found"))
        return self._select_row(row)

    def current_row(self):
        """Vrať instanci PresentedRow právě aktivního řádku.

        Není-li vybrán žádný řádek, vrať 'None'.

        """
        return self._row

    def selected_rows(self):
        """Return an iterator over all currently selected rows.

        The iterator returns all rows present in the current selection as
        'PresentedRow' instances in the order of ther presence in the form.

        """
        return iter([])

    def unselect_selected_rows(self):
        """Completely clear the current selection of rows in the form."""
        pass

    def current_key(self):
        """Vrať klíč aktuálně vybraného řádku.

        Vrací: Sekvenci instancí třídy 'pytis.data.Value' nebo 'None', pokud
        není vybrán žádný řádek.

        """
        return self._current_key()

    def readonly(self):
        return False

    def prefill(self):
        """Vrať data pro předvyplnění nového záznamu."""
        return self._prefill

    def presented_rows(self):
        """Return an iterator over all rows currently contained in the form."""
        class Iterator(object):

            def __init__(self, table):
                self._table = table
                self._row_number = 0

            def __iter__(self):
                return self

            def __next__(self):
                if self._row_number >= self._table.GetNumberRows():
                    raise StopIteration
                row = self._table.row(self._row_number)
                self._row_number += 1
                return row

            next = __next__  # for Python 2

        return Iterator(self._table)

    # Implementation of Public API 'pytis.api.Form'.

    @property
    def api_row(self):
        return self.current_row()

    @property
    def api_selection(self):
        return self.selected_rows()

    def api_clear_selection(self):
        self.unselect_selected_rows()

    def api_select_row(self, position):
        self.select_row(position)


# Editační formulář


class EditForm(RecordForm, TitledForm, Refreshable):
    """Form for editing a single record.

    The form consists of all fields defined by the specification.  These fields
    are arranged according to the 'layout' specification.

    Particular interactive fields in the UI are represented by
    'pytis.form.InputField' instances (actually instances of a subclass given
    by field type and specification).

    The form is primarily designed for editing data for insert or update, but
    can be also used in read-only mode for viewing the records in their natural
    layout.  See the constructor argument 'mode'.

    """

    _LOG_STATISTICS = False

    MODE_INSERT = 'MODE_INSERT'
    """Form mode for insertion of new records."""
    MODE_EDIT = 'MODE_EDIT'
    """Form mode for updating existing records."""
    MODE_VIEW = 'MODE_VIEW'
    """Form mode for viewing existing records."""

    def _full_init(self, *args, **kwargs):
        super(EditForm, self)._full_init(*args, **kwargs)
        if isinstance(self.Parent, wx.Dialog):
            wx_callback(wx.EVT_INIT_DIALOG, self.Parent, self._set_focus_field)
        else:
            self._set_focus_field()

    def _init_attributes(self, mode=MODE_EDIT, focus_field=None, set_values=None, layout=None,
                         inserted_data=None, on_commit_record=None, **kwargs):
        """Process constructor keyword arguments and initialize the attributes.

        Arguments:

          mode -- one of the 'MODE_*' constants.  Determines whether the form
            is primarily for viewing, editation or creation of records.
          focus_field -- identifier of the field which should be activated for
            user input on form startup.  If None, the first field is the
            default.  It is also possible to pass a function of one argument --
            the PresentedRow instance representing the current record.  This
            function must return a field identifier or None.
          set_values -- dictionary of row values to set in the newly openened
            form.  If not None, the dictionary keys are field identifiers and
            values are either the corresponding internal python values valid
            for the fields's data type or pytis.data.Value() instances
            directly.  These values will not affect the initial row state
            (unlike the 'prefill' argument of the parent class) and thus will
            appear as changed to the user.
          layout -- custom layout of the form overriding the specification
            defined layout.  Instance of 'pp.GroupSpec' or a sequence of items
            to be passed to a vertical 'GroupSpec'.
          inserted_data -- iterable providing items for batch insertion.  Each
            item may be a 'pytis.data.Row' instance or a dictionary as in
            'set_values'.  If not null, the form is gradually prefilled by
            given data and the user can individually accept or skip each row.
          on_commit_record -- callback to be called when the record is
            succesfully saved after the submit button is pressed.  Similar to
            'cleanup' in form specification and called just after cleanup when
            both defined.
          kwargs -- arguments passed to the parent class

        """
        assert mode in (self.MODE_EDIT, self.MODE_INSERT, self.MODE_VIEW)
        assert inserted_data is None or mode == self.MODE_INSERT, (inserted_data, mode)
        new = mode == self.MODE_INSERT
        super_(EditForm)._init_attributes(self, _new=new, **kwargs)
        self._mode = mode
        self._focus_field = focus_field or self._view.focus_field()
        self._edit_form_timeout = (None if self._transaction is None
                                   else pytis.config.edit_form_timeout)
        # Other attributes
        self._fields = []
        self._tab_navigated_widgets = []
        if set_values:
            for key, value in set_values.items():
                type = self._row.type(key)
                if isinstance(value, pytis.data.Value):
                    value = value.retype(type)
                else:
                    value = pytis.data.Value(type, value)
                self._row[key] = value
        self._layout = layout
        self._closed_connection_handled = False
        if inserted_data is not None:
            self._inserted_data = iter(enumerate(inserted_data))
            try:
                self._inserted_data_len = len(inserted_data)
            except TypeError:
                self._inserted_data_len = None
            self._inserted_data_index = 0
        else:
            self._inserted_data = None
        self._inserted_data_loaded = False
        self._on_commit_record = on_commit_record

    def _disable_buttons(self, w):
        for c in w.GetChildren():
            if isinstance(c, wx.Button):
                if c.GetId() != wx.ID_CANCEL:
                    c.Enable(False)
            else:
                self._disable_buttons(c)

    def _on_idle_close_transactions(self):
        age = pytis.form.last_event_age()
        if ((self._edit_form_timeout is not None and age > self._edit_form_timeout)):
            edit = app.question(_("The time limit for form editing has expired.\n"
                                  "Do you want to continue?"),
                                title=_("Continue editing?"),
                                timeout=20)
            if not edit:
                self._disable_buttons(self.Parent)
                callback = self._transaction_timeout_callback
                if callback is not None:
                    callback()
                self._edit_form_timeout = None
                if edit is None:
                    app.error(_("The time limit for form editation has elapsed."))
        if self._open_transaction() is not None:
            self._transaction.set_max_age(age)
        super(EditForm, self)._on_idle_close_transactions()

    def _set_focus_field(self, event=None):
        field = None
        if self._focus_field:
            if callable(self._focus_field):
                field_id = self._focus_field(self._row)
                assert field_id is None or isinstance(field_id, basestring), \
                    "Invalid result of focus_field() function: %s" % field_id
            else:
                field_id = self._focus_field
            if field_id:
                field = find(field_id, self._fields, key=lambda f: f.id())
                if field is None:
                    log(OPERATIONAL, "Unknown field returned by focus_field:", field_id)
        if not field and self._fields:
            field = find(True, self._fields, key=lambda f: f.enabled()) or self._fields[0]
        if field:
            field.set_focus()

    def _create_form_parts(self):
        # Create all parts and add them to top-level sizer.
        self.Sizer.Add(self._create_title_bar(), 0, wx.EXPAND)
        self.Sizer.Add(self._create_form_controls(), 1, wx.EXPAND)

    def _create_form_controls(self):
        self._fields = []
        if isinstance(self._layout, GroupSpec):
            group = self._layout
        elif self._layout:
            group = GroupSpec(self._layout, orientation=Orientation.VERTICAL)
        else:
            group = self._view.layout().group()
        if isinstance(group, TabGroup):
            window = wx.Notebook(self)
            for item in group.items():
                if len(item.items()) == 1 and isinstance(item.items()[0], GroupSpec):
                    group = item.items()[0]
                else:
                    group = GroupSpec(list(item.items()), orientation=Orientation.VERTICAL)
                panel = self._create_group_panel(window, group)
                panel.SetOwnBackgroundColour(DEFAULT_WINDOW_BACKGROUND_COLOUR)
                window.AddPage(panel, item.label())
        else:
            window = self._create_group_panel(self, group)
        return window

    def _create_group_panel(self, parent, group):
        panel = wx.ScrolledWindow(parent, style=wx.TAB_TRAVERSAL)
        panel.SetScrollRate(20, 20)
        # Create the form controls first, according to the order.
        fields = [pytis.form.InputField.create(panel, self._row, id, guardian=self,
                                               readonly=self.readonly())
                  for id in group.order() if self._view.field(id).width() != 0]
        self._fields.extend(fields)
        # Create the layout groups.
        group_sizer = self._create_group(panel, group)
        # Add outer sizer with margins and alignment.
        sizer = wx.BoxSizer(wx.HORIZONTAL)
        sizer.Add(group_sizer, 0, wx.LEFT | wx.RIGHT, 8)
        panel.SetSizer(sizer)
        # SetMinSize is necessary here in order to get correct results from
        # form.GetVirtualSize(), especially when wx.Notebook is involved.
        panel.SetMinSize(sizer.CalcMin())
        wx_callback(wx.EVT_KEY_DOWN, panel, self.on_key_down)
        return panel

    def _field(self, id):
        f = find(id, self._fields, key=lambda f: f.id())
        if f is None:
            raise ProgramError("Unknown field: %s" % id)
        return f

    def _create_button(self, parent, button):
        if button.handler():
            def handler(row):
                button.handler()(row)
                busy_cursor(False)
            label = button.label()
            tooltip = button.tooltip()
            cmd, args = Application.COMMAND_HANDLED_ACTION(handler=handler, row=self._row,
                                                           enabled=button.enabled())
        else:
            action = find(button.action(), self._view.actions(unnest=True), key=lambda a: a.name())
            label = button.label() or action.title()
            tooltip = button.tooltip() or action.descr()
            cmd, args = self.COMMAND_CONTEXT_ACTION(action=action)
        return wx_button(parent, label, command=(cmd, args), tooltip=tooltip,
                         enabled=(button.active_in_popup_form() or not
                                  isinstance(self, PopupForm)) and
                         (button.active_in_readonly_form() or not self.readonly()) and
                         cmd.enabled(**args),
                         width=button.width() and dlg2px(parent, 4 * button.width()))

    def _create_text(self, parent, text):
        return wx.StaticText(parent, -1, text.text(), style=wx.ALIGN_LEFT)

    def _create_lcg_content(self, parent, content):
        browser = Browser(parent)
        browser.load_content(content)
        browser.SetMinSize(char2px(parent, 72, 18))
        return browser

    def _create_group(self, parent, group, aligned=False):
        # Each continuous sequence of fields is first stored in an array and
        # finally packed into a grid sizer by self._pack_fields() and added to
        # this group's sizer.  'aligned' is True if this group is aligned
        # within a vertical pack.  In this case the first field label is
        # omitted, since it was already placed within the pack's labels column
        # and the outer borders around this group are suppressed.
        orientation = orientation2wx(group.orientation())
        if group.label() is not None:
            box = wx.StaticBox(parent, -1, group.label())
            sizer = wx.StaticBoxSizer(box, orientation)
        else:
            sizer = wx.BoxSizer(orientation)
        pack = []
        gap = dlg2px(parent, group.gap())
        border = dlg2px(parent, group.border())
        for i, item in enumerate(group.items()):
            if isinstance(item, basestring):
                if self._view.field(item).width() == 0:
                    continue
                item = self._field(item)
            if group.orientation() == Orientation.VERTICAL:
                if isinstance(item, (Button, Text)) \
                        or isinstance(item, pytis.form.InputField) \
                        and not item.spec().compact() \
                        or isinstance(item, GroupSpec) \
                        and item.label() is None \
                        and item.orientation() == Orientation.HORIZONTAL \
                        and isinstance(item.items()[0], basestring) \
                        and not self._field(item.items()[0]).spec().compact() \
                        and self._field(item.items()[0]).label() is not None:
                    # This item will become a part of the current aligned pack.
                    # Nested horizontal groups are aligned if they start with a labeled field.
                    pack.append(item)
                    continue
            if len(pack) != 0:
                # Add the latest aligned pack into the sizer (if there was one).
                sizer.Add(self._pack_fields(parent, pack, gap),
                          0, wx.ALIGN_TOP | wx.ALL, border)
                pack = []
            if isinstance(item, GroupSpec):
                x = self._create_group(parent, item)
            elif isinstance(item, pytis.form.InputField):
                if item.spec().compact():
                    # This is a compact field (not a part of the aligned pack).
                    label = item.label() or wx.StaticText(parent, -1, '')
                    x = wx.BoxSizer(wx.VERTICAL)
                    x.Add(label, 0, wx.ALIGN_LEFT)
                    x.Add(item.widget())
                else:
                    # Fields in a HORIZONTAL group are packed separately (label and ctrl).
                    x = self._pack_fields(parent, (item,), gap,
                                          suppress_label=(i == 0 and aligned))
            elif isinstance(item, Button):
                x = self._create_button(parent, item)
            elif isinstance(item, Text):
                x = self._create_text(parent, item)
            elif isinstance(item, lcg.Content):
                x = self._create_lcg_content(parent, item)
            else:
                raise ProgramError("Unsupported layout item!", item)
            if aligned:
                border_style = wx.RIGHT
            else:
                border_style = wx.ALL
            sizer.Add(x, 0, wx.ALIGN_TOP | border_style, border)
        if len(pack) != 0:
            # Add remaining fields pack, if any.
            sizer.Add(self._pack_fields(parent, pack, gap),
                      0, wx.ALIGN_TOP | wx.ALL, border)
        # If this is a labeled group, add small top margin, otherwise it is too tight.
        if group.label() is not None:
            s = wx.BoxSizer(orientation)
            s.Add(sizer, 0, wx.TOP, 2)
            sizer = s
        return sizer

    def _pack_fields(self, parent, items, gap, suppress_label=False):
        # Pack the sequence of fields and/or buttons aligned vertically into a grid.
        #  items -- sequence of InputField, GroupSpec or Button instances.
        #  gap -- space between the fields in dlg units; integer
        #  suppress_label -- True if the field label should be supressed.  Used
        #    for vertically aligned horizontal groups (the label is placed in
        #    the parent pack)
        grid = wx.FlexGridSizer(len(items), 2, gap, 2)
        for item in items:
            if isinstance(item, GroupSpec):
                field = self._field(item.items()[0])
                label = field.label()
                if label:
                    grid.Add(label, 0, wx.ALIGN_RIGHT | wx.ALIGN_CENTER_VERTICAL, 2)
                grid.Add(self._create_group(parent, item, aligned=True))
            elif isinstance(item, (Button, Text)):
                style = wx.ALIGN_RIGHT | wx.ALIGN_CENTER_VERTICAL
                label = wx.StaticText(parent, -1, "", style=wx.ALIGN_RIGHT)
                grid.Add(label, 0, style, 2)
                if isinstance(item, Button):
                    grid.Add(self._create_button(parent, item))
                else:
                    grid.Add(self._create_text(parent, item))
            else:
                if not suppress_label:
                    label = item.label() or wx.StaticText(parent, -1, '')
                    if item.height() and item.height() > 1:
                        style = wx.ALIGN_RIGHT | wx.ALIGN_TOP | wx.TOP
                    else:
                        style = wx.ALIGN_RIGHT | wx.ALIGN_CENTER_VERTICAL
                    grid.Add(label, 0, style, 2)
                grid.Add(item.widget())
        return grid

    def _signal_update(self):
        app.form.refresh()

    def _refresh(self, interactive=False):
        if interactive:
            self._apply_providers()
        self.Refresh()

    def _commit_data(self, op, args):
        if op is not None:
            success, result = db_operation(op, *args, **dict(transaction=self._open_transaction()))
        else:
            success, result = True, (None, True)
        return success, result

    def _do_check_record(self, row):
        # Check record integrity (the global 'check' function).
        success, field_id, msg = self._check_record(row)
        if msg:
            if field_id:
                msg = self._view.field(field_id).label() + ": " + msg
            app.error(msg, title=_("Integrity check failed"))
        if field_id:
            f = self._field(field_id)
            if f:
                f.set_focus()
            else:
                log(OPERATIONAL, "Unknown field returned by check():", field_id)
        return success

    def _on_closed_connection(self):
        super(EditForm, self)._on_closed_connection()
        self._disable_buttons(self.Parent)

    def _on_idle(self, event):
        super(EditForm, self)._on_idle(event)
        if not self._closed_connection_handled and any(f.connection_closed() for f in self._fields):
            self._closed_connection_handled = True
            self._on_closed_connection()
        if self._inserted_data and not self._inserted_data_loaded:
            self._inserted_data_loaded = True
            self._load_next_row()

    def _batch_position(self):
        return '{}{}'.format(
            self._inserted_data_index + 1,
            '/{}'.format(self._inserted_data_len) if self._inserted_data_len else '',
        )

    def _load_next_row(self, report_success=False):
        row = None
        prefill = self._prefill
        if self._inserted_data:
            def message(*messages):
                app.message('\n\n'.join([m for m in messages if m]))
            success = _("The record has been saved succesfully.") if report_success else None
            batch_info = _("Batch insertion progress:") + ' ' + self._batch_position()
            try:
                i, item = next(self._inserted_data)
            except StopIteration:
                self.set_status('progress', '')
                message(success, batch_info, _("All records have been processed."))
                self._inserted_data = None
                self.close()
            else:
                if report_success:
                    message(success, batch_info)
                if isinstance(item, pytis.data.Row):
                    row = item
                else:
                    prefill = dict(prefill or {}, **item)
                self._inserted_data_index = i
                self.set_status('progress', self._batch_position())
        self._row.set_row(row, reset=True, prefill=prefill)
        self._set_focus_field()

    def _commit_form(self, close=True):
        # Re-validate all fields.
        for f in self._fields:
            fid = f.id()
            if ((self._mode == self.MODE_EDIT and
                 not self._row.permitted(fid, pytis.data.Permission.VIEW) and
                 fid in self._row and not self._row[fid].value())):
                self._row[fid] = self._row.original_row()[fid]
            elif (self._mode == self.MODE_INSERT and
                  not self._row.permitted(fid, pytis.data.Permission.VIEW)):
                for fspec in self._row.fields():
                    if fspec.id() == fid:
                        type_ = self._row.type(fid)
                        default = fspec.default()
                        if default is not None:
                            if callable(default):
                                default = default()
                            value = pytis.data.Value(type_, default)
                        else:
                            value = type_.default_value()
                        self._row[fid] = value
                        break
            elif self._mode == self.MODE_INSERT or self._row.field_changed(fid):
                if f.enabled() and not f.validate():
                    f.set_focus()
                    return False
        # Check record integrity (the global 'check' function).
        if not self._do_check_record(self._row):
            return False
        # Create the data row.
        if self._mode == self.MODE_INSERT:
            permission = pytis.data.Permission.INSERT
        elif self._mode == self.MODE_EDIT:
            permission = pytis.data.Permission.UPDATE
        else:
            permission = None
        try:
            rdata = self._record_data(self._row, permission=permission,
                                      updated=(self._mode == self.MODE_EDIT))
        except (pytis.data.DBRetryException, pytis.data.DBSystemException):
            self._on_closed_connection()
            return False
        if not rdata.keys():
            # We don't want to insert/update the form row when it was not
            # changed, but we still want to commit the transaction, because it
            # may contain uncommited changes invoked through various form
            # actions.
            log(ACTION, 'Record unchanged')
            op, args = None, ()
        elif self._mode == self.MODE_INSERT:
            log(ACTION, 'Inserting record...')
            op, args = self._insert_op_args(rdata)
        elif self._mode == self.MODE_EDIT:
            log(ACTION, 'Updating record...')
            op, args = self._update_op_args(rdata)
        else:
            raise ProgramError("Can't commit in this mode:", self._mode)
        # Perform the DB operation.
        transaction = self._open_transaction()
        if transaction is not None:
            success, result = db_operation(transaction.set_point, 'commitform',
                                           allow_retry=False, quiet=True)
        else:
            success = True
        if success:
            success, result = self._commit_data(op, args)
        else:
            result = (None, False)
        if success and result[1]:
            new_row = result[0]
            if new_row is None:
                new_row = self._row.row()
            original_row = copy.copy(self._row)
            # The set_row() below is necessary to replace the original_row's
            # data row by a new copied instance, because deepcopy doesn't work here
            # (causes exception) and shallow copy leaves the data row instance
            # shared by the original and the copied instance, which is exactly
            # what we don't need.
            original_row.set_row(
                copy.copy(original_row.row()),
                # Use prefill to keep the previous virtual field values.
                prefill={k: self._row[k] for k in self._row.keys() if k not in new_row.keys()},
            )
            # Refresh the form values from the saved DB row (the DB operation may
            # actually change some values (triggers, rules, views, ...) but preserve
            # virtual fields.  We can't use set_row(), because it would reset all
            # virtual fields to their default values.  Only virtual fields depending
            # on changed DB values must be recomputed.
            # Doing this carefully is particularly necessary in InputForm, where
            # we need to preserve the values in the returned row.
            for k in new_row.keys():
                if self._row[k].value() != new_row[k].value():
                    self._row[k] = new_row[k]
            # Calling set_row() after previous cycle is needed just to mark the row
            # as unchanged without actually changing any field values (reset=True).
            self._row.set_row(
                self._row.row(), reset=True,
                prefill={k: self._row[k] for k in self._row.keys() if k not in new_row.keys()}
            )
            self._signal_update()
            if op is not None:
                if self._mode == self.MODE_INSERT:
                    log(ACTION, 'Record inserted')
                else:
                    log(ACTION, 'Record updated')
            cleanup = self._view.cleanup()
            if cleanup is not None:
                cleanup(self._row, original_row)
            if self._on_commit_record:
                self._on_commit_record(self._row)
            if close:
                self._result = self._row
                self.close()
            if self._governing_transaction is None and self._transaction is not None:
                db_operation(self._transaction.commit, allow_retry=False, quiet=True)
                if close:
                    self._transaction = None
                else:
                    self._transaction = self._default_transaction()
                self._row.set_transaction(self._transaction)
                log(ACTION, 'Transaction committed')
            return True
        else:
            if self._transaction is not None:
                success, __ = db_operation(self._transaction.cut, 'commitform',
                                           allow_retry=False, quiet=True)
            else:
                success = True
            if success:
                msg = _("Record update failed")
            else:
                msg = _("Transaction aborted, unable to continue")
            if ((isinstance(result, tuple) and
                 isinstance(result[0], basestring))):
                msg = "%s\n\n%s" % (result[0], msg)
            app.error(msg)
            return False

    def _insert_op_args(self, rdata):
        return self._data.insert, (rdata,)

    def _update_op_args(self, rdata):
        return self._data.update, (self._current_key(), rdata)

    def _exit_check(self):
        if self.changed():
            if not app.question(_(u"Unsaved changes in form data!") + "\n" +
                                _(u"Do you really want to quit without saving?")):
                return False
        if self._inserted_data:
            if self._inserted_data_len is not None:
                remaining = self._inserted_data_len - (self._inserted_data_index + 1)
            else:
                remaining = None
            next_, abort, back = _("Next record"), _("Abort batch"), _("Back")
            if remaining == 0:
                answer = app.question(
                    _(u"You are leaving the form without saving the current record\n"
                      u"while at the last record of batch insertion.") + '\n\n' +
                    _(u"Do you really want to quit without saving?"),
                    answers=(abort, back),
                )
            else:
                answer = app.question(
                    _(u"You are leaving the form without saving the current record\n"
                      u"during batch insertion.") + "\n\n" +
                    (_.ngettext(
                        u"There is %d more record after the current one until "
                        u"the end of the batch.",
                        u"There are %d more records after the current one until "
                        u"the end of the batch.",
                        remaining) + "\n\n" if remaining is not None else '') +
                    _(u"Do you want to:\n"
                      u"  • advance to the next record in the batch ({}),\n"
                      u"  • skip the rest of the batch ({}),\n"
                      u"  • return to the current record ({})?").format(next_, abort, back),
                    answers=(next_, abort, back),
                )
            if answer == next_:
                self._load_next_row()
                return False
            elif answer == abort:
                return True
            else:
                return False
        return True

    # Command handling

    def _can_commit_record(self, close=True):
        return self._mode != self.MODE_VIEW

    def _cmd_commit_record(self, close=True):
        try:
            # TODO: Busy cursor doesn't work over X2Go!
            busy_cursor(True)
            result = self._commit_form(close=close)
            if result:
                app.refresh()
                if not close:
                    self._load_next_row(report_success=True)
            return result
        finally:
            busy_cursor(False)

    def _can_navigate(self, back=False):
        return self._mode != self.MODE_VIEW

    def _cmd_navigate(self, back=False):
        widgets = (
            functools.reduce(lambda w, f: w + (f.tab_navigated_widgets() if f.enabled() else ()),
                             self._fields, ()) +
            tuple(self._tab_navigated_widgets)
        )
        order = [w for w in widgets if w.IsEnabled()]
        current = self.FindFocus()
        if current in order:
            i = (order.index(current) + (-1 if back else 1)) % len(order)
            target = order[i]
            target.SetFocus()

    # Public methods

    def title(self):
        """Return the form title as a string."""
        return self._view.layout().caption()

    def field(self, id):
        """Return the 'InputField' instance for the field 'id'.

        Raises 'ProgramError' if the field of given id does not exist.

        """
        return self._field(id)

    def fields(self):
        """Return all form fields as a tuple of 'InputField' instances."""
        return self._fields

    def changed(self):
        """Return true iff the form data was changed since last saved."""
        return self._row.changed()

    def readonly(self):
        """Return true iff the form is read only."""
        return self._mode == self.MODE_VIEW

    # Implementation of Public API 'pytis.api.Form'.

    @property
    def api_field(self):
        class FieldAccess(object):
            def __init__(self, fields):
                self._fields = fields

            def __getattr__(self, name):
                f = find(name, self._fields, key=lambda f: f.id())
                if f:
                    return f.provider()
                else:
                    raise AttributeError("Form has no field '{}'".format(name))

            def __call__(self, name):
                return self.__getattr__(name)

        return FieldAccess(self._fields)


class PopupEditForm(PopupForm, EditForm):
    """Stejné jako 'EditForm', avšak v popup podobě."""

    DESCR = _(u"edit form")

    def __init__(self, parent, *args, **kwargs):
        parent = self._popup_frame(parent)
        EditForm.__init__(self, parent, *args, **kwargs)

    def _full_init(self, *args, **kwargs):
        EditForm._full_init(self, *args, **kwargs)
        # Set the popup window size according to the ideal form size limited to
        # the screen size.  If the form size exceeds the screen, scrollbars
        # will appear.
        size = self.GetVirtualSize()
        size.height += 14  # Necessary empiric adjustment since wx 4.0.
        size.DecTo(wx.GetDisplaySize() - wx.Size(50, 80))
        self.SetClientSize(size)

    def _init_data_select(self, data, async_count=False):
        if ((isinstance(data, pytis.data.MemData) or
             isinstance(self._select_row_argument, (int, dict)))):
            # Virtual forms need opening the data select to initialize their data, but:
            # TODO: We shouldn't really care here.  Such MemData initialization should
            # be done in its constructor.
            # Also for the theoretical case that the constructor argument 'select_row'
            # contains a row number (int) or a dictionary of values, we need to
            # initialize the select in order to be able to perform _find_row() for
            # that position.  This would be, however, better to avoid this because
            # of the risk of slow selects when big binary data are present.  Pytis
            # never passes int or dict here, but we are not sure about the apps so we
            # keep this option, but:
            # TODO: Consider disabling int/dict position for PopupEditForm select_row.
            return super(PopupEditForm, self)._init_data_select(data, async_count=async_count)
        else:
            # Otherwise opening the select just delays opening the form (particularly
            # when it contains big (binary) data).
            self._init_transaction_timeouts(data)

    def _find_row(self, position):
        if isinstance(position, (tuple, pytis.data.Value)):
            # Using data.row() is faster and doesn't require the (potentially slow)
            # data select to be opened at form startup.  It also ignores any
            # conditions given by the current profile etc, which is desirable here.
            success, row = db_operation(self._data.row, position,
                                        columns=self._select_columns(),
                                        transaction=self._open_transaction(),
                                        arguments=self._current_arguments())
            return row if success and row else None
        else:
            return super(PopupEditForm, self)._find_row(position)

    def _initial_select_row(self):
        # Supress the initial select_row() call in popup forms.  We don't really
        # need it here as we rely on self._row being initialized according to
        # the select_row argument already.  The additional select_row() would
        # reset the values initialized by the 'set_values' argument.
        pass

    def _default_transaction(self):
        if isinstance(self._data, pytis.data.MemData):
            return None
        try:
            connection_name = pytis.config.resolver.get(self._name, 'data_spec').connection_name()
        except ResolverError:
            connection_name = None
        return pytis.data.DBTransactionDefault(pytis.config.dbconnection,
                                               connection_name=connection_name,
                                               ok_rollback_closed=True)

    def _init_attributes(self, multi_insert=True, **kwargs):
        """Process constructor keyword arguments and initialize the attributes.

        Arguments:

          multi_insert -- boolean flag indicating whether inserting multiple values is permitted.
            This option is only relevant in insert mode.  False value will disable this feature and
            the `Next' button will not be present on the form.
          kwargs -- arguments passed to the parent class

        """
        EditForm._init_attributes(self, **kwargs)
        assert isinstance(multi_insert, bool), multi_insert
        self._multi_insert = multi_insert

    def _create_form_parts(self):
        sizer = self.Sizer
        sizer.Add(self._create_caption(self, size=18), 0, wx.ALIGN_CENTER | wx.ALL, 8)
        sizer.Add(self._create_form_controls(), 1, wx.EXPAND)
        sizer.Add(self._create_buttons(), 0, wx.ALIGN_CENTER)
        sizer.Add(self._create_status_bar(), 0, wx.EXPAND)

    def _create_status_bar(self):
        # We use our own "statusbar implementation".
        spec = (('message', None, _(u"Notification area")),)
        if self._inserted_data:
            spec += (('progress', 9, _(u"Batch insertion progress indicator")),)
        box = wx.BoxSizer()
        self._status_fields = dict(
            [(id, self._create_status_bar_field(box, width, descr))
             for id, width, descr in spec])
        return box

    def _create_status_bar_field(self, sizer, width, descr):
        panel = wx.Panel(self, -1, style=wx.SUNKEN_BORDER)
        panel.SetToolTip(descr)
        panel.SetAutoLayout(True)
        box = wx.BoxSizer()
        field = wx.StaticText(panel, -1, '', style=wx.ALIGN_LEFT)
        box.Add(field, 1, wx.EXPAND | wx.ALL, 2)
        if width is not None:
            width = dlg2px(field, 4 * width)
            height = field.GetSize().GetHeight()
            field.SetMinSize((width, height))
            expansion = 0
        else:
            expansion = 1
        sizer.Add(panel, expansion, wx.EXPAND)
        panel.SetSizer(box)
        box.Fit(panel)
        return field

    def _buttons(self):
        buttons = (
            dict(label=_("Ok"),
                 tooltip=(_("Save the current record and advance to the next one.")
                          if self._inserted_data else
                          _("Save the record and close the form.")),
                 command=self.COMMAND_COMMIT_RECORD(close=not self._inserted_data)),
            dict(id=wx.ID_CANCEL,
                 tooltip=_("Close the form without saving"),
                 command=self.COMMAND_LEAVE_FORM()),
        )
        if self._mode == self.MODE_INSERT and self._multi_insert and not self._inserted_data:
            buttons += (
                dict(id=wx.ID_FORWARD, label=_("Next"),  # icon=wx.ART_GO_FORWARD,
                     tooltip=_("Save the current record without closing the form "
                               "to allow next record insertion."),
                     command=self.COMMAND_COMMIT_RECORD(close=False)),
            )
        return buttons

    def _create_buttons(self):
        sizer = wx.BoxSizer(wx.HORIZONTAL)
        for i, kwargs in enumerate(self._buttons()):
            button = wx_button(self, fullsize=True, **kwargs)
            if i == 0:
                button.SetDefault()
            wx_callback(wx.EVT_KEY_DOWN, button, self.on_key_down)
            self._tab_navigated_widgets.append(button)
            sizer.Add(button, 0, wx.ALL, 20)
        return sizer

    def _apply_profile(self, profile, refresh=True):
        # Popup forms ignore profiles.  It doesn't make much sense and they
        # were not designed for something like that.  It would be more lgical
        # to change the inheritance hierarchy so that single record forms don't
        # inherit profiles at all but that's a little too complicated for now.
        pass

    def _cmd_leave_form(self):
        # Leaving the form in wx.CallAfter (as in the parent method) causes
        # recursion in popup forms. Closing the form immediately seems to
        # work fine, on the other hand.
        self.Unbind(wx.EVT_IDLE)
        pytis.form.app.block_yield(True)
        try:
            return self.close()
        finally:
            pytis.form.app.block_yield(False)

    def can_command(self, command, **kwargs):
        if ((command.handler() in (LookupForm, RecordForm) and
             command not in (RecordForm.COMMAND_CONTEXT_ACTION,
                             RecordForm.COMMAND_VIEW_FIELD_PDF))):
            return False
        return super(PopupEditForm, self).can_command(command, **kwargs)

    def run(self):
        if self._mode == self.MODE_EDIT:
            key = self._current_key()
        else:
            key = None
        return PopupForm.run(self, lock_key=key)

    def set_status(self, field, message):
        if field in self._status_fields:
            self._status_fields[field].SetLabel(unistr(message or ''))
            return True
        else:
            return False

    def set_row(self, row):
        if self._transaction is None:
            self._transaction = pytis.data.DBTransactionDefault(pytis.config.dbconnection)
        super(PopupEditForm, self).set_row(row)


class _VirtualEditForm(EditForm):
    """Edit form defined in runtime by constructor arguments.

    Forms derived from this class don't need to have a statically defined
    Specification class.  The specification is created in runtime according to
    keyword arguments passed to the form constructor and the data object is
    created as MemData object so no "real" database objects are needed as well.

    Special constructor arguments:

      avoid_initial_selection -- the initial value of the initially focused
        input field (determines by 'focus_field') is by default also selected
        (when this is an editable text field).  This results in overwriting the
        whole value when the user starts typing.  Passing False here avoids
        this initial selection.

    """
    def _full_init(self, resolver, name, guardian=None, transaction=None,
                   prefill=None, avoid_initial_selection=False,
                   inserted_data=None, on_commit_record=None, **kwargs):
        self._specification = Specification.create_from_kwargs(
            resolver,
            data_cls=pytis.data.RestrictedMemData,
            **kwargs
        )
        self._avoid_initial_selection = avoid_initial_selection
        if isinstance(self, PopupEditForm):
            additional_kwargs = dict(multi_insert=False)
        else:
            additional_kwargs = dict()
        super(_VirtualEditForm, self)._full_init(resolver, name, guardian=guardian,
                                                 mode=self.MODE_INSERT, prefill=prefill,
                                                 transaction=transaction,
                                                 inserted_data=inserted_data,
                                                 on_commit_record=on_commit_record,
                                                 **additional_kwargs)

    def _set_focus_field(self, event=None):
        super(_VirtualEditForm, self)._set_focus_field(event=event)
        if self._avoid_initial_selection:
            w = wx_focused_window()
            if isinstance(w, wx.TextCtrl):
                w.SetSelection(0, 0)

    def _create_view_spec(self):
        return self._specification.view_spec()

    def _create_data_object(self):
        factory = self._specification.data_spec()
        return factory.create()

    def _get_saved_setting(self, option, default=None):
        return default

    def _load_profiles(self):
        return [self._default_profile]

    def _default_transaction(self):
        return None

    def _print_menu(self):
        return []

    def _exit_check(self):
        if self._inserted_data:
            return super(_VirtualEditForm, self)._exit_check()
        else:
            return True


class InputForm(_VirtualEditForm, PopupEditForm):
    """Dynamically created virtual form.

    This form is not bound to a specification acquired from the resolver, but
    the specification is created on the fly based on keyword arguments passed
    to the form constructor.  All keyword arguments passed to the constructor,
    except for 'guardian' and 'transaction' (which have the same meaning as in
    the parent class) are used as attributes of the
    'pytis.presentation.Specification' class.  The default 'data_cls' of the
    specification is 'pytis.data.RestrictedMemData' so the form will by default
    work with a virtual data object which is not bound to any database object.

    This form is mainly intended to be used as a single purpose input form when
    it is neccessary to query the user for several values and the fields can be
    easily described as standard pytis input fields.  All the features
    supported by pytis input fields are available and can be defined through
    standard pytis specification options.

    Example usage:

    result = run_form(InputForm, title=_("Enter the values"),
                      fields=(Field('title', _(u"Title"), not_null=True),
                              Field('date', _(u"Date"), type=pytis.data.Date)))

    The 'result' is None if the form was escaped without confirmation.  If
    confirmed, the 'result' is a 'pytis.presentation.PresentedRow' instance
    containing the entered values.  Other specification attributes, such as
    'layout', 'check', etc. may be used.

    However the form should rather be user through 'app.input_form()' Pytis API
    method.

    """
    pass


class QueryFieldsForm(_VirtualEditForm):
    """Virtual form to be used internally for query fields (see list.py)."""

    def _full_init(self, resolver, name, query_fields, callback, **kwargs):
        self._refresh_form_data = callback
        self._autoapply = autoapply = query_fields.autoapply()
        self._autoinit = autoinit = query_fields.autoinit()
        self._get_last_refresh = query_fields.last_refresh()
        self._unapplied_query_field_changes = not autoinit
        self._unapplied_query_field_changes_after_restore = False
        self._initialized = autoinit
        self._save = query_fields.save()
        self._on_apply = query_fields.on_apply()
        self._on_refresh = query_fields.on_refresh()
        self._last_refresh = None
        self._last_refresh_last_updated = None
        materialized_view = self._materialized_view = kwargs.pop('materialized_view', None)
        load = query_fields.load()
        kwargs.update(query_fields.view_spec_kwargs())
        fields = kwargs.pop('fields')
        fields += (
            # Add hidden virtual field just for tracking changes of other fields.
            Field('__changed', type=pytis.data.Boolean(), default=False, virtual=True,
                  editable=Editable.NEVER,
                  computer=Computer(lambda r: True, depends=[f.id() for f in fields])),
        )
        _VirtualEditForm._full_init(self, resolver, name, fields=fields, **kwargs)
        self._row.register_callback(self._row.CALL_CHANGE, '__changed',
                                    self._on_query_fields_changed)
        if not materialized_view and not autoapply and autoinit:
            self._query_fields_apply_button.Enable(False)
        # Set the popup window size according to the ideal form size limited to
        # the screen size.  If the form size exceeds the screen, scrollbars
        # will appear.
        size = self.GetVirtualSize()
        size.DecTo(wx.GetDisplaySize() - wx.Size(50, 80))
        self.SetClientSize(size)
        if load:
            load(self._row)

    def _create_form_parts(self):
        self.Sizer.Add(self._create_form_controls(), 1, wx.EXPAND)

    def _create_group_panel(self, parent, group):
        panel = super(QueryFieldsForm, self)._create_group_panel(parent, group)
        if self._materialized_view:
            self._query_fields_refresh_button = button = wx_button(
                panel, _("Refresh view"),
                callback=self._refresh_materialized_view,
                tooltip=_("Refresh the underlying database view."),
            )
            panel.Sizer.Add(button, 0, wx.ALIGN_BOTTOM | wx.LEFT | wx.TOP | wx.BOTTOM | wx.RIGHT, 6)
            self._last_refresh_label = label = wx.StaticText(
                panel, -1, '',
                size=(250, 18), style=wx.ALIGN_LEFT | wx.ST_NO_AUTORESIZE,
            )
            self._update_last_refresh(initial=True)
            panel.Sizer.Add((6, 0), 0)
            panel.Sizer.Add(label, 0, wx.ALIGN_BOTTOM | wx.BOTTOM, 9)
        elif not self._autoapply:
            self._query_fields_apply_button = button = wx_button(
                panel, _("Apply"),
                callback=lambda e: self._apply_query_fields(self._row),
                tooltip=_("Reload form data with current query field values."),
            )
            panel.Sizer.Add(button, 0, wx.ALIGN_BOTTOM | wx.BOTTOM, 6)
        panel.SetMinSize(panel.Sizer.CalcMin())
        return panel

    def _set_focus_field(self, event=None):
        # Avoid moving focus to the fields when autoinit is true
        # as we prefer having focus in the grid.  Also when the
        # query fields belong to a side form, don't focus the
        # fields to leave focus in the main form.
        if not self._autoinit and not isinstance(self.GetParent().GetParent(),
                                                 pytis.form.SideBrowseForm):
            super(QueryFieldsForm, self)._set_focus_field(event=event)

    def _on_idle(self, event):
        super(QueryFieldsForm, self)._on_idle(event)
        if self._materialized_view and self._last_refresh:
            # Update the last refresh displayed time each 10 seconds.
            now, last_updated = int(time.time()), self._last_refresh_last_updated
            if not last_updated or now - last_updated > 10:
                self._last_refresh_last_updated = now
                self._update_last_refresh_delta()
        if not self._autoapply and not self._materialized_view:
            enabled = self._unapplied_query_field_changes and all(f.valid() for f in self._fields)
            self._query_fields_apply_button.Enable(enabled)
        if self._unapplied_query_field_changes_after_restore:
            self._unapplied_query_field_changes_after_restore = False
            if app.question(_("Query fields contain unapplied changes. Apply now?")):
                self._apply_query_fields(self._row)

    def _on_query_fields_changed(self):
        # Due to the CALL_CHANGE definition, this callback is called only after
        # all fields become at least non-strictly valid (they can be converted
        # to an internal value of their type).  They may, however still not be
        # strictly valid, so further constraints (such as not_null or codebook)
        # may not be fulfiled.
        # Thus we can not immediately enable the Apply button here.  We do it
        # in _on_idle if the flag is set here and all fields are valid.
        if self._autoapply:
            self._apply_query_fields(self._row, interactive=False)
        else:
            self._unapplied_query_field_changes = True
            self._initialized = True

    def _apply_query_fields(self, row, interactive=True, refresh_form=True):
        if ((all(f.validate(interactive=interactive) for f in self._fields) and
             self._do_check_record(row))):
            self._initialized = True
            if self._save:
                self._save(row)
            if self._on_apply:
                self._on_apply(row)
            if refresh_form:
                self._refresh_form_data(row)
            self._unapplied_query_field_changes = False

    def _update_last_refresh(self, initial=False):
        if self._get_last_refresh:
            last_refresh = self._get_last_refresh()
        elif not initial:
            last_refresh = pytis.data.LocalDateTime.datetime()
        else:
            last_refresh = None
        self._last_refresh = last_refresh
        value = pytis.data.Value(pytis.data.LocalDateTime(), last_refresh)
        self._last_refresh_label.SetToolTip(value.export())
        self._update_last_refresh_delta()

    def _update_last_refresh_delta(self):
        if self._last_refresh is None:
            value = _('unknown')
        else:
            now = pytis.data.LocalDateTime.datetime()
            interval = pytis.data.Value(pytis.data.TimeInterval(), now - self._last_refresh)
            value = _("%s ago", interval.export(format=pytis.data.TimeInterval.NATURAL_FORMAT))
        self._last_refresh_label.SetLabel(_("Last refreshed:") + ' ' + value)

    def _refresh_materialized_view(self, row):
        busy_cursor(True)
        self._query_fields_refresh_button.Enable(False)
        pytis.form.app.wx_yield(full=True)
        try:
            self._materialized_view.refresh()
            if self._on_refresh:
                self._on_refresh(row)
            self._refresh_form_data(row)
            if not self._autoapply:
                self._apply_query_fields(row, interactive=True, refresh_form=False)
            self._update_last_refresh()
        finally:
            busy_cursor(False)
            self._query_fields_refresh_button.Enable(True)

    def restore(self):
        if not self._autoapply and self._unapplied_query_field_changes:
            self._unapplied_query_field_changes_after_restore = True
        return super(QueryFieldsForm, self).restore()

    def run(self):
        raise Exception("This form can not be run.")

    def row(self):
        if self._initialized:
            return self._row
        else:
            return None


class ResizableEditForm(object):
    """Mixin for resizable edit forms with fields expanded to the whole window.

    The fields returned by _resizable_fields() will take the whole space of the
    form and will expand with the form when its window is resized.  It is
    mostly useful for forms which should look like a text editor.

    """

    def _popup_frame_style(self):
        return super(ResizableEditForm, self)._popup_frame_style() | wx.RESIZE_BORDER

    def _resizable_fields(self):
        return self._view.layout().group().order()

    def _create_form_parts(self):
        panel = wx.Panel(self)
        sizer = wx.BoxSizer()
        panel.SetSizer(sizer)
        for field_id in self._resizable_fields():
            if self._view.field(field_id).width() != 0:
                field = pytis.form.InputField.create(panel, self._row, field_id, guardian=self,
                                                     readonly=self.readonly())
                self._fields.append(field)
                sizer.Add(field.widget(), 1, wx.EXPAND)
        self.Sizer.Add(panel, 1, wx.EXPAND)
        self.Sizer.Add(self._create_status_bar(), 0, wx.EXPAND)


class ResizableInputForm(ResizableEditForm, InputForm):
    """Resizable InputForm with fields expanded to the whole window."""
    # Used within StructuredTextField._cmd_open_in_editor().
    pass


class StructuredTextEditor(ResizableEditForm, PopupEditForm):
    """Text Editor of a single structured text field running outside transaction.

    It is assumed that structured text editation may take a "long time" and it
    is not desired to block the database by a long running transaction.  Thus
    this form will run outside transaction and will perform a check for
    conflicting changes in the text before saving the new value into the
    database.

    """

    def _init_attributes(self, field_id, **kwargs):
        """Process constructor keyword arguments and initialize the attributes.

        Arguments:

          field_id -- string identifier of the edited field.

        """
        super(StructuredTextEditor, self)._init_attributes(**kwargs)
        self._editor_field_id = field_id

    def _resizable_fields(self):
        return (self._editor_field_id,)

    def _default_transaction(self):
        # Run editor outside transaction to prevent long transactions (the
        # editation usually takes quite some time).
        return None

    def _lock_record(self, key):
        # Locking is not possible as we don't use transactions here (see
        # above).  Instead we rely on the check for conflicting changes before
        # commit implemented below in _check_record.
        return True

    def _check_record(self, row):
        success, field_id, msg = super(StructuredTextEditor, self)._check_record(row)
        if success:
            # Check for possible conflicting changes made since the record was
            # last saved.  If the current value in database doesn't match the
            # value before editation, someone else probably changed it.  The
            # resolution is left up to the user.
            data = pytis.util.data_object(self._name)
            success, db_row = db_operation(data.row, self._current_key(),
                                           columns=self._select_columns(),
                                           arguments=self._current_arguments())
            current_db_value = db_row[self._editor_field_id].value()
            value_before_edits = row.original_row()[self._editor_field_id].value()
            if current_db_value != value_before_edits:
                diff = pytis.util.html_diff(value_before_edits, current_db_value,
                                            _("Original text"), _("Concurrently changed version"))
                revert, merge, ignore = (_(u"Discard my changes"), _(u"Merge"),
                                         _(u"Ignore the concurrent changes"))
                answer = app.question(
                    _("Someone else has changed the same text while you made your "
                      "modifications. You can see the overview of the changes below.\n"
                      "The safest resolution is to discard your changes and start "
                      "over with the new version of the text. If your modifications\n"
                      "were too extensive, you can try incorporating the below listed "
                      "changes into your version."),
                    title=_(u"Conflicting modifications"),
                    report=diff, report_format=TextFormat.HTML,
                    answers=(revert, ignore), # TODO: Add merge button.
                )
                if answer == merge:
                    success, field_id = False, self._editor_field_id
                elif answer == ignore:
                    pass
                elif answer == revert:
                    value = pytis.data.Value(row.type(self._editor_field_id), current_db_value)
                    row[self._editor_field_id] = value
        return success, field_id, msg

    def size(self):
        return (700, 500)


class PopupInsertForm(PopupEditForm):

    DESCR = _(u"insert form")

    def _init_attributes(self, **kwargs):
        super_(PopupInsertForm)._init_attributes(self, mode=EditForm.MODE_INSERT, **kwargs)


class ShowForm(EditForm):
    """Formulář pro zobrazení náhledu.

    Layout je stejný jako u editačního formuláře (resp. 'EditForm'),
    pouze titulek má stejný vzhled, jako titulek formulářů typu 'ListForm'.
    Určen pro zobrazení v duálním formuláři.

    """

    DESCR = _(u"view form")

    def _init_attributes(self, mode=EditForm.MODE_VIEW, select_row=0, **kwargs):
        super_(ShowForm)._init_attributes(self, mode=mode, select_row=select_row, **kwargs)

    def changed(self):
        # Since the row is not reset when the current record changes, it would report a change...
        return False

    def _select_columns(self):
        big_field_ids = []
        for f in self._view.fields():
            t = f.type()
            if isinstance(t, pytis.data.Type):
                if isinstance(t, pytis.data.Big):
                    big_field_ids.append(f.id())
            elif t is not None and issubclass(t, pytis.data.Big):
                big_field_ids.append(f.id())
        result = [c.id() for c in self._data.columns()
                  if not isinstance(c.type(), pytis.data.Big) and c.id() not in big_field_ids]
        return result


class BrowsableShowForm(ShowForm):
    """Listovací formulář pro zobrazení náhledu.

    Formulář je needitovatelný, ale umožňuje pohyb po záznamech tabulky, nad
    kterou je vytvořen, vyhledávání atd.  Z uživatelského hlediska jde v
    podstatě o redukci prohlížecích možností formuláře typu 'BrowseForm' na
    jeden záznam zobrazený v Layoutu editačního formuláře.

    """

    def _cmd_next_record(self, back=False):
        current_row = self.current_row()
        if current_row:
            row_number = self._get_row_number(current_row.row())
        else:
            row_number = 0
        if not back:
            row_number += 1
            if row_number == self._lf_count(min_value=(row_number + 1)):
                app.echo(_("Last record"), kind='error')
                return
        else:
            if row_number == 0:
                app.echo(_("First record"), kind='error')
                return
            row_number -= 1
        self._select_row(self._find_row_by_number(row_number))

    def _select_row(self, row):
        result = super(BrowsableShowForm, self)._select_row(row)
        current_row = self.current_row()
        total = self._lf_count(timeout=0)
        if not isinstance(self._lf_select_count_, int):
            total = '%s?' % (total,)
        if current_row and total:
            n = self._get_row_number(current_row)
            self._list_position = "%d/%s" % (n is not None and n + 1 or 0, total)
        return result

    def list_position(self):
        # list_position() may be called on idle from Application._refresh_list_position()
        # before the form is fully initialized. Note that ListForm defines the same
        # method, but the _list_position attribute is managed differently and there
        # is no common base class that recognizes _list_position.
        return getattr(self, '_list_position', None)


class ViewerForm(Form, Refreshable):
    """Generic base class for forms viewing content."""

    def _create_view_spec(self):
        # This is quite a hack.  The base Form class should be independent of
        # pytis specifications and data objects as it shows, that we want also
        # forms which don't deal with the database at all.
        return None

    def _create_data_object(self):
        return None

    def _init_attributes(self, title=None, content=None, **kwargs):
        super_(ViewerForm)._init_attributes(self, **kwargs)
        assert (content is not None) == (title is not None), \
            "When 'content' specified, 'title' must be set too (and wice versa)."
        self._title = title
        self._content = content

    def _can_help(self):
        return False

    def title(self):
        return self._title or _("Document")


class WebForm(ViewerForm):
    """Web browser embedded in a Pytis form.

    The form shows a browser window as its main content.

    """
    DESCR = _(u"document view")

    def _create_form_parts(self):
        self._browser = browser = Browser(self, guardian=self)
        self.Sizer.Add(browser.toolbar(self), 0, wx.EXPAND)
        self.Sizer.Add(browser, 1, wx.EXPAND)
        content = self._content
        if content is not None:
            self.load_content(content)

    def _refresh(self, interactive=False):
        self._browser.reload()

    def load_content(self, content):
        if isinstance(content, basestring):
            self._browser.load_html(content, restrict_navigation='-')
        else:
            self._browser.load_content(content)


class FileViewerForm(ViewerForm):
    """File Viewer embedded in a Pytis form."""
    DESCR = _(u"file preview")

    def _create_form_parts(self):
        self._viewer = viewer = FileViewer(self)
        self.Sizer.Add(viewer.buttons(), 0, wx.EXPAND)
        self.Sizer.Add(viewer, 1, wx.EXPAND)
        if self._content is not None:
            self.load_file(self._content)

    def load_file(self, data):
        """Display preview of given file-like object in the form."""
        self._viewer.load_file(data)
