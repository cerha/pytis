# -*- coding: utf-8 -*-

# Copyright (C) 2001-2012 Brailcom, o.p.s.
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

import copy
import collections
import time

import pytis.data
import pytis.output
from pytis.presentation import PresentedRow
from pytis.form import *
import wx

class FormProfile(object):
    """Temporary hack to avoid application crashing on unpickling old form profiles."""
    def __init__(self, *args, **kwargs):
        log(OPERATIONAL, "%s instantiated:" % self.__class__.__name__, (args, kwargs))
        

class FormSettings(FormProfile):
    pass
        
class Form(Window, KeyHandler, CallbackHandler, CommandHandler):
    """Společná nadtřída formulářů.

    Formulář si podle jména specifikace předaného konstruktoru vyžádá od
    resolveru příslušnou datovou a prezentační specifikaci.  Z datové
    specifikace vytvoří datový objekt (instance třídy odvozené z
    'pytis.data.Data').  Datový objekt a prezentační specifikace jsou potom
    uloženy ve formě atributů instance formuláře ('self._view' a 'self._data')

    Instance tříd odvozených z této třídy jsou potom vytvářeny na základě
    interpretace prezentační specifikace a pracují s daty s pomocí datového
    objektu a jeho API (které je nezávislé na konkrétním zdroji dat).

    Form je potomkem 'Window', díky čemuž je možné jej ukládat na zásobník oken
    aplikace a provádět další operace, jako zaostřování, skrývání, zobrazování
    apod.

    Používané specifikační funkce:

      print_spec -- sequence of 'PrintAction' instances

    """

    CALL_USER_INTERACTION = 'CALL_USER_INTERACTION'
    """Konstanta callbacku interakce uživatele."""

    _STATUS_FIELDS = ()
    _LOG_STATISTICS = True
    DESCR = None

    class InitError(Exception):
        """Exception signaling errors on form initializations."""

    def _get_command_handler_instance(cls):
        return current_form(inner=False)
    _get_command_handler_instance = classmethod(_get_command_handler_instance)

    def descr(cls):
        """Vrať textový popis typu formuláře jako řetězec."""
        if cls.DESCR is not None:
            return cls.DESCR
        else:
            return cls.__class__.__name__
    descr = classmethod(descr)

    def __init__(self, parent, resolver, name, full_init=True, **kwargs):
        """Inicializuj instanci.

        Argumenty:
        
          parent -- instance 'wxFrame', do kterého formulář patří
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
          data_kwargs -- dictionary of additional keyword arguments passed to
            the data object constructor.
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

        Po zpracování společných argumwentů jsou načteny specifikace a vytvořen
        datový objekt.

        Poté jsou zpracovávány klíčové argumenty.  Každá odvozená třída může
        definovat své vlastní klíčové argumenty.  Ty potom zpracuje
        předefinováním metody '_init_attributes()'.  Ta již může využívat
        inicializovaného datového objetu a specifikací a případně initializovat
        další atributy třídy.  Metoda '_init_attributes()' by měla vždy
        zpracovávat pouze klíčové argumenty, které jsou specifické pro danou
        třídu.  Zbylé předá metodě rodičovské třídy konstrukcí **kwargs.  Takto
        by mělo být zaručeno, že dojde postupně ke zpracování všech argumentů.

        Teprve po zpravování argumentů konstruktoru a inicializaci atributů je
        vytvářen vlastní obsah formuláře (viz. '_create_form()').  Toto by mělo
        být dodržováno i v odvozených třídách.
        
        """
        Window.__init__(self, parent)
        self._name = name
        if full_init:
            self._full_init_called = True
            self._full_init(parent, resolver, name, **kwargs)
        else:
            self._full_init_called = False
            self._full_init_kwargs = dict(kwargs,
                                          parent=parent,
                                          resolver=resolver,
                                          name=name)
        
    def _full_init(self, parent, resolver, name, guardian=None, transaction=None,
                   spec_kwargs={}, data_kwargs={}, **kwargs):
        import pytis.extensions
        start_time = pytis.data.DateTime.now()
        self._parent = parent
        self._resolver = resolver
        self._guardian = guardian or parent
        self._governing_transaction = transaction
        self._transaction = transaction or self._default_transaction()
        self._spec_kwargs = copy.copy(spec_kwargs)
        self._data_kwargs = copy.copy(data_kwargs)
        self._leave_form_requested = False
        KeyHandler.__init__(self)
        CallbackHandler.__init__(self)
        try:
            # Note, that some code relies on the order of calling these two methods.
            self._view = self._create_view_spec()
            self._data = self._create_data_object()
        except (ResolverError, ProgramError):
            log(OPERATIONAL, 'Form initialization error', format_traceback())
            raise self.InitError()
        self._init_attributes(**kwargs)
        self._result = None
        try:
            self._create_form()
        except:
            # This is necessary to prevent database connection leaks
            self._cleanup()
            raise
        show_time = pytis.data.DateTime.now()
        if self._LOG_STATISTICS and config.form_statistics:
            pytis.extensions.dbfunction('pytis_log_form',
                                        ('form', pytis.data.Value(pytis.data.String(), name),),
                                        ('class', pytis.data.Value(pytis.data.String(), self.__class__.__name__),),
                                        ('info', pytis.data.Value(pytis.data.String(), self._form_log_info())),
                                        ('t_start', start_time,),
                                        ('t_show', show_time,),)
        wx_callback(wx.EVT_IDLE, self, self._on_idle)        
        log(EVENT, 'Form created in %.3fs:' % (pytis.data.DateTime.diff_seconds(start_time, show_time),), self)

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
        return form_settings_manager().get(self._profile_spec_name(), self._form_name(), option, default=default)
        
    def _set_saved_setting(self, option, value):
        """Update saved form parameter independent on current profile."""
        form_settings_manager().set(self._profile_spec_name(), self._form_name(), option, value)
    
    def _create_view_spec(self):
        t = time.time()
        spec = self._resolver.get(self._name, 'view_spec', **self._spec_kwargs)
        log(EVENT, 'Specification read in %.3fs:' % (time.time() - t), spec)
        assert isinstance(spec, ViewSpec)
        return spec        

    def _create_data_object(self):
        return create_data_object(self._name, spec_kwargs=self._spec_kwargs,
                                  kwargs=self._data_kwargs)

    def _create_form(self):
        # Build the form from parts
        self._top_level_sizer = sizer = wx.BoxSizer(wx.VERTICAL)
        self._create_form_parts(sizer)
        self.SetSizer(sizer)
        sizer.Fit(self) # Set the size of window `self' to size of the sizer.

    def _form_log_info(self):
        return ''

    def _default_transaction(self):
        return None
        
    def _create_form_parts(self, sizer):
        pass

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
        for group in TOOLBAR_COMMANDS:
            group = [uicmd for uicmd in group
                     if isinstance(self, uicmd.command().handler())]
            if group:
                groups.append(group)
        return groups

    def _create_toolbar(self):
        toolbar = wx.ToolBar(self)
        for i, group in enumerate(self._toolbar_commands()):
            if i != 0:
                toolbar.AddSeparator()
            for uicmd in group:
                uicmd.create_toolbar_ctrl(toolbar)
        toolbar.Realize()
        return toolbar
    
    def __str__(self):
        return '<%s for "%s">' % (self.__class__.__name__, self._name)

    def __repr__(self):
        return str(self)

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
        if self._leave_form_requested:
            try:
                self._cmd_leave_form()
            finally:
                self._leave_form_requested = False
            result = True
        else:
            result = False
        return result

    def _cleanup(self):
        super(Form, self)._cleanup()
        self._cleanup_data()
        for id in self._STATUS_FIELDS:
            set_status(id, '')

    def _cleanup_data(self):
        try:
            self._data.close()
        except:
            pass
        self._data = None
        
    # Zpracování příkazů
   
    def _cmd_help(self):
        Application.COMMAND_HELP.invoke(topic='spec/'+self._name)

    def _cmd_leave_form(self):
        block_yield(True)
        try:
            return self.close()
        finally:
            block_yield(False)

    def _cmd_safe_leave_form(self):
        # Segmentation fault may happen when closing a form using Escape key.
        # It happens inside wx key event processing.  We try to overcome the
        # problem by just setting a leave form flag and closing the form later,
        # in idle event processing.
        self._leave_form_requested = True

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
        VIEW   = pytis.data.Permission.VIEW
        INSERT = pytis.data.Permission.INSERT
        UPDATE = pytis.data.Permission.UPDATE
        DELETE = pytis.data.Permission.DELETE
        EXPORT = pytis.data.Permission.EXPORT
        if perm == DELETE:
            result = has_access(self.name(), perm=perm)
        else:
            for col in self._data.columns():
                if has_access(self.name(), perm=perm, column=col.id()):
                    result = True
                    break
            else:
                result = False
        if not result and not quiet:
            msg = {
                VIEW:   "Nemáte právo k zobrazení formuláře.",
                INSERT: "Nemáte právo vložit nový záznam.",
                UPDATE: "Nemáte právo změnit existující záznam.",
                DELETE: "Nemáte právo smazat existující záznam.",
                EXPORT: "Nemáte právo k exportu do CSV.",
                }[perm]
            message(msg, beep_=True)
        return result

    def set_status(self, field, message):
        """Zobraz zprávu `message' v poli `id' stavové řádky formuláře.

        Má-li formulář vlastní stavovou řádku a v ní pole `id' zobraz v něm
        danou zprávu a vrať pravdu.  V opačném případě vrať nepravdu.

        """
        return False

    def save(self):
        self._saved_state = map(lambda id: (id, get_status(id)), self._STATUS_FIELDS)
        self._release_data()

    def restore(self):
        for id, message in self._saved_state:
            set_status(id, message, log_=False)

    def data(self):
        """Return a new instance of the data object used by the form."""
        return self._create_data_object()

    def full_init(self):
        """Finish initialization of a mostly unitialized form.

        This must be called before the first use of the form when
        'soft_init=True' was used in the constructor.  It may not be called
        otherwise nor may be called more than once.

        """
        assert not self._full_init_called, "Form initialization called more than once"
        self._full_init_called = True
        self._full_init(**self._full_init_kwargs)
        self._full_init_kwargs = None

    def initialized(self):
        """Return true iff full form initialization was attempted.

        If it wasn't, it is necessary to call 'full_init()' for this form to
        use the form.
        
        """
        return self._full_init_called
        
    def size(self):
        """Return the prefered form size in pixels as a tuple of two integers (width, height).

        None may be returned if the prefered form size is not defined or known.

        """
        return None

    
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
    def _get_command_handler_instance(cls):
        return current_form()
    _get_command_handler_instance = classmethod(_get_command_handler_instance)
    
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
            popup_menu(parent, items, self._get_keymap())

    def _print_menu(self):
        # Vrať seznam položek tiskového menu.
        name = self._name
        try:
            print_spec = list(self._resolver.get(name, 'print_spec') or ())
        except ResolverError:
            print_spec = []
        # Default print currently disabled, since on a huge table it may extensively consume
        # resources and no one is using it anyway...
        #if not print_spec:
        #    print_spec = ((_("Výchozí"), os.path.join('output', name)),)
        db_print_spec = []
        condition = pytis.data.EQ('module', pytis.data.Value(pytis.data.String(), name))
        i = 1
        for row in pytis.extensions.dbselect('printing.UserOutputTemplates', condition=condition):
            template_name = row['specification'].value()
            template_specification = row['module'].value()+'/'+template_name
            print_item = PrintAction('__db_%d' % (i,), template_name, template_specification,)
            print_spec.append(print_item)
            db_print_spec.append((name, template_name,))
            i += 1
        printing_form = 'printing.DirectUserOutputTemplates'
        menu = [MItem(p.title(), command=BrowseForm.COMMAND_PRINT(print_spec_path=p.name()))
                for p in print_spec
                if action_has_access('print/%s' % (p.dmp_name(),), perm=pytis.data.Permission.PRINT)]
        if has_access(self.name(), perm=pytis.data.Permission.PRINT):
            menu.append(MSeparator())
            menu.append(pytis.extensions.new_record_mitem(_("Nová sestava"), printing_form,
                                                          prefill=dict(module=pytis.data.sval(name))))
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
                menu += [Menu(_("Editace sestav"), edit_submenu),
                         Menu(_("Mazání sestav"), delete_submenu)]
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
                data = pytis.data.dbtable('e_pytis_help_spec',
                                          ('spec_name', 'description', 'help'),
                                          config.dbconnection)
            except pytis.data.DBException:
                description = self._view.description()
            else:
                row = data.row((pytis.data.Value(data.find_column('spec_name').type(), self._name),))
                if row:
                    description = row['description'].value()
                else:
                    description = self._view.description()
            self._cached_spec_description = description
        return description
    
    def _cmd_describe(self):
        title = self._view.title()
        description = self._spec_description()
        text = "= "+ title +" =\n\n" + description
        InfoWindow(_(u"Popis náhledu %s") % title, text=text, format=TextFormat.LCG)
        
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

    
class Refreshable:
    """Třída zajišťující existenci metody 'refresh()' s daným významem.

    Tuto třídu by měly dědit všechny formuláře, které mají být obnoveny při
    změně dat (typicky způsobené jiným formulářem výše na zásobníku rámců).
    
    """

    DOIT_IMMEDIATELY = 'DOIT_IMMEDIATELY'
    """Konstanta pro 'refresh()' pro okamžitý update.

    Není-li seznam právě editován, je update proveden okamžitě.  Jinak je
    uživatel dotázán, zda má být update proveden ihned; odpoví-li uživatel
    negativně, je update proveden až po ukončení editace.

    """
    DOIT_AFTEREDIT = 'DOIT_AFTEREDIT'
    """Konstanta pro 'refresh()' pro update po skončení editace.

    Není-li seznam právě editován, je update proveden okamžitě.  Jinak je
    proveden až po ukončení editace.
    
    """
    DOIT_IFNEEDED = 'DOIT_IFNEEDED'
    """Konstanta pro 'refresh()' pro podmíněný update.

    Update je proveden pouze tehdy, je-li známo, že došlo ke změně dat.
    V takovém případě je proveden okamžitě pouze tehdy, jestliže seznam není
    práve editován a v poslední době nebyl proveden žádný jiný update;
    v opačném případě je update odložen \"až na vhodnější chvíli\" (nicméně
    proveden bude).

    """
    _block_refresh = 0

    def block_refresh(cls, function, *args, **kwargs):
        """Zablokuj veškerý refresh po dobu provádění funkce 'function'.

        Všechny argumenty jsou předány volané funkci.
        
        Vrací: výsledek vrácený volanou funkcí.

        Refresh je zablokován globálně, pro všechny existující formuláře.
        
        """
        Refreshable._block_refresh += 1
        try:
            result = function(*args, **kwargs)
        finally:
            Refreshable._block_refresh -= 1
        return result
    block_refresh = classmethod(block_refresh)
    
    def refresh(self, when=None, interactive=False):
        """Aktualizuj data formuláře z datového zdroje.

        Překresli data ve formuláři v okamžiku daném argumentem 'when'.

        Argumenty:

          when -- určuje, zda a kdy má být aktualizace provedena, musí to být
            jedna z 'DOIT_*' konstant třídy.  Implicitní hodnota je
            'DOIT_AFTEREDIT', je-li 'reset' 'None', 'DOIT_IMMEDIATELY' jinak.
          interactive -- indicates whether the refresh was invoked by explicit
            user request

        Vrací: Pravdu, právě když byla aktualizace provedena.

        """
        level = Refreshable._block_refresh
        if level == 0:
            self._refresh(when=when, interactive=interactive)
        elif level > 0:
            log(OPERATIONAL, "Refresh neproveden kvůli blokaci:", level)
        else:
            raise ProgramError("Nepřípustná hodnota _block_refresh:", level)

    def _refresh(self, when=None, interactive=False):
        """Proveď vlastní refresh.

        Tuto metodu nechť předefinují odvozené třídy.

        """
        pass


class PopupForm:
    """Formulář nacházející se v samostatném framu.

    Tato třída je určena k vložení mezi předky třídy, jejíž instance mají být
    vytvářeny v samostatných framech.  Pro získání framu slouží metoda
    '_popup_frame'.

    """
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
        return wx.DIALOG_MODAL|wx.DEFAULT_DIALOG_STYLE
    
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
            if lock_key is not None:
                if not self._lock_record(lock_key):
                    return None
            unlock_callbacks()
            frame = self._parent
            frame.SetTitle(self.title())
            frame.SetClientSize(self.GetSize())
            frame.ShowModal()
        finally:
            if self._governing_transaction is None and self._transaction is not None \
                   and self._result is None:
                db_operation(self._transaction.rollback)
            self._governing_transaction = None
            self._transaction = None
        result = self._result
        self._close(force=True)
        return result


class TitledForm:
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
        font = wx.Font(size, wx.DEFAULT, wx.NORMAL, wx.BOLD,
                       encoding=wx.FONTENCODING_DEFAULT)
        caption.SetFont(font)
        width, height, d, e = self.GetFullTextExtent(text, font)
        caption.SetSize(wx.Size(width, height))
        return caption

    def _create_title_bar(self):
        """Vytvoř 3d panel s nadpisem formuláře."""
        panel = wx.Panel(self, -1, style=wx.RAISED_BORDER)
        caption = self._create_caption(panel)
        box = wx.BoxSizer()
        box.Add(caption, 1, wx.EXPAND|wx.ALL, self._TITLE_BORDER_WIDTH)
        panel.SetSizer(box)
        box.Fit(panel)
        return panel


class LookupForm(InnerForm):
    """Formulář s vyhledáváním a tříděním."""
    
    SORTING_NONE = 'NONE'
    """Constant for 'COMMAND_SORT' 'direction' argument indicationg unsorting."""
    SORTING_ASCENDENT = pytis.data.ASCENDENT
    """Backwards compatibility alias for 'pytis.data.ASCENDENT'.

    Deprecated: Use 'pytis.data.ASCENDENT' directly.

    """
    SORTING_DESCENDANT = pytis.data.DESCENDANT
    """Backwards compatibility alias for 'pytis.data.DESCENDANT'.

    Deprecated: Use 'pytis.data.DESCENDANT' directly.

    """
    def _init_attributes(self, filter=None, sorting=None, columns=None, grouping=None,
                         profile_id=None, condition=None, arguments=None, **kwargs):
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
        # Create a Profile instance representing the form constructor
        # arguments.  Note, that the default profile is not necessarily the
        # initially selected profile.
        self._default_profile = Profile('__default_profile__', _(u"Výchozí profil"))
        self._profiles = self._load_profiles()
        if filter or sorting or columns or grouping:
            assert profile_id is None
            # When profile parameters were passed to the constructor, create an
            # additional profile according to these paramaters.
            self._initial_profile = Profile('__constructor_profile__', _(u"Počáteční profil"),
                                            filter=filter, sorting=sorting, columns=columns,
                                            grouping=grouping)
            self._profiles.insert(0, self._initial_profile)
        else:
            # Note, that the profile 0 may not be the same as
            # self._default_profile, but a saved user customized version.
            default_profile = self._profiles[0]
            if profile_id is not None:
                check_profile_id = True
            else:
                profile_id = (self._get_saved_setting('initial_profile')
                              or self._view.profiles().default())
                check_profile_id = False
            if profile_id:
                profile = find(profile_id, self._profiles, key=lambda p: p.id())
                if not profile:
                    if check_profile_id:
                        raise ProgramError("Unknown profile '%s'" % profile_id)
                    else:
                        profile = default_profile
                self._initial_profile = profile
            else:
                self._initial_profile = default_profile
        self._initial_profile_applied = False
        # The profile instances may contain None values to denote default
        # values.  We need to remember the corresponding real values to be able
        # to compare profiles with the current form state in
        # '_current_profile_changed()'.  We rely on the fact that
        # '_apply_profile_parameters()' substitutes None values by their
        # corresponding default values and we don't want to repeat this logic
        # anywhere else.  Thus we here apply the default profile and store the
        # resulting profile parameters.  The user visible initial profile is
        # applied later in _on_idle().
        self._apply_profile_parameters(self._default_profile)
        self._default_profile_parameters = self._profile_parameters_to_save()
        self._lf_initial_sorting = self._lf_sorting
        # _lf_condition represents a static condition given by the constructor
        # argument, whereas _lf_filter represents the filtering condition,
        # which is part of the current user profile.  There is also a third
        # condition -- the one defined by the specification, but this one is
        # not visible to the form at all -- it is applied at the level of the
        # data object.
        self._lf_condition = condition
        self._lf_search_condition = None
        self._arguments = arguments
        if not self._update_arguments():
            raise Form.InitError()
        self._lf_select_count_ = None
        self._init_select(async_count=True)
        
    def __getattr__(self, name):
        ## Compatibility with contingent external code using the old attribute
        if name == '_lf_select_count':
            return self._lf_count()
        try:
            return self.__dict__[name]
        except KeyError:
            raise AttributeError(name)

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
        conditions = (self._lf_condition, filter or self._lf_filter)
        conditions = [c for c in conditions if c is not None]
        if len(conditions) == 0:
            return None
        elif len(conditions) == 1:
            return conditions[0]
        else:
            return pytis.data.AND(*conditions)

    def _current_arguments(self):
        return self._arguments

    def _update_arguments(self):
        if self._view.argument_provider() is not None:
            arguments = self._view.argument_provider()(self._arguments or {})
            if arguments is None:
                return False
            self._arguments = arguments
        return True

    def _load_profiles(self):
        return profile_manager().load_profiles(self._profile_spec_name(), self._form_name(),
                                               self._view, self._data, self._default_profile)
    
    def _apply_initial_profile(self):
        # This must be called in _on_idle especially because the initial
        # profile may not be valid and we want to make use of handling invalid
        # profiles in _cmd_apply_profile().
        if self._initial_profile is not self._current_profile:
            self._cmd_apply_profile(self._profiles.index(self._initial_profile))
            dual = self._dualform()
            if dual and self is not dual.main_form():
                # Force focus back to mainform, as _apply_profile() (for
                # unknown reason) sometimes steals the focus when applied in a
                # sideform.
                dual.main_form().focus()
                
    def _on_idle(self, event):
        if super(LookupForm, self)._on_idle(event):
            return True
        if not self._initial_profile_applied:
            try:
                self._apply_initial_profile()
            finally:
                self._initial_profile_applied = True
        return False

    def _init_data_select(self, data, async_count=False):
        return data.select(condition=self._current_condition(display=True),
                           columns=self._select_columns(),
                           sort=self._lf_sorting,
                           arguments=self._current_arguments(),
                           transaction=self._transaction, reuse=False,
                           async_count=async_count)

    def _init_select(self, async_count=False):
        success, self._lf_select_count_ = db_operation(self._init_data_select, self._data,
                                                       async_count)
        if not success:
            log(EVENT, 'Selhání databázové operace')
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
        return sfs_columns(self._view.fields(), self._data)

    def _search(self, condition, direction, row_number=None, report_failure=True,
                initial_shift=False):
        if initial_shift:
            if direction == pytis.data.FORWARD:
                start_row_number = max(row_number-1, 0)
            else:
                start_row_number = min(row_number+1, self._lf_count(min_value=row_number+1))
        else:
            start_row_number = row_number
        self._search_adjust_data_position(start_row_number)
        data = self._data
        skip = data.search(condition, direction=direction, transaction=self._transaction)
        if skip == 0:
            log(EVENT, 'Záznam nenalezen')
            if report_failure:
                message(_(u"Záznam nenalezen"), beep_=True)
            result = None
        else:
            if initial_shift:
                if direction == pytis.data.FORWARD:
                    skip = skip + (start_row_number - row_number)
                else:
                    skip = skip + (row_number - start_row_number)
            result = skip
            log(EVENT, 'Záznam nalezen:', skip)
            self._search_skip(result, direction)
        return result

    def _search_adjust_data_position(self, row_number):
        pass

    def _search_skip(self, skip, direction):
        data = self._data
        data.skip(skip-1, direction=direction)
        row = data.fetchone(direction=direction)
        self._select_row(row)

    def _cmd_jump(self):
        max_value = self._lf_count()
        if max_value > 0:
            prompt = _(u"Záznam číslo (1-%s):") % (max_value,)
            result = run_dialog(InputNumeric, message=_(u"Skok na záznam"), prompt=prompt,
                                min_value=1, max_value=max_value)
            row = result.value()
            if row is not None:
                self.select_row(row-1)

    def _cmd_first_record(self):
        self.select_row(0)
        
    def _cmd_last_record(self):
        self.select_row(self._lf_count()-1)
        
    def _cmd_search(self, next=False, back=False):
        condition = self._lf_search_condition
        if condition is not None and next:
            direction = back and pytis.data.BACKWARD or pytis.data.FORWARD
        else:
            direction, condition = block_refresh(lambda:
                 run_dialog(SearchDialog, self._lf_sfs_columns(),
                            self.current_row(), col=self._current_column_id(),
                            condition=self._lf_search_condition))
        if direction is not None:
            self._lf_search_condition = condition
            self._search(condition, direction)

    def _compute_aggregate(self, operation, column_id, condition):
        condition = self._current_condition(filter=condition)
        return self._data.select_aggregate((operation, column_id), condition,
                                           transaction=self._transaction)

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
        profile_manager().save_profile(self._profile_spec_name(), self._form_name(), profile)
    
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
        return profile.id().startswith(FormProfileManager.USER_PROFILE_PREFIX)

    def _cmd_apply_profile(self, index):
        profile = self._profiles[index]
        if profile.errors():
            keep, remove = (_(u"Ponechat"), _(u"Odstranit"))
            msg = _(u"Uživatelský profil \"%s\" je neplatný.\n"
                    u"Pravděpodobně došlo ke změně definice náhledu a uložený\n"
                    u"profil nyní nelze použít. Profil můžete buďto odstranit,\n"
                    u"nebo ponechat a požádat správce aplikace o jeho obnovení."
                    ) % profile.title()
            errors = '\n'.join(['%s: %s' % (param, error) for param, error in profile.errors()])
            answer = run_dialog(MultiQuestion, title=_(u"Neplatný profil"), message=msg,
                                icon=Question.ICON_ERROR, buttons=(keep, remove),
                                report=errors, default=keep)
            if answer == remove:
                if self._is_user_defined_profile(profile):
                    self._profiles.remove(profile)
                else:
                    if profile.id() == self._default_profile.id():
                        profile = self._default_profile
                    else:
                        profile = find(profile.id(), self._view.profiles(), key=lambda p: p.id())
                    self._profiles[index] = profile
                profile_manager().drop_profile(self._profile_spec_name(), self._form_name(), profile.id())
        else:
            orig_profile = self._current_profile
            try:
                self._apply_profile(profile)
            except UserBreakException:
                self._apply_profile(orig_profile)
        self.focus()

    def _cmd_save_new_profile(self, title):
        if title in [profile.title() for profile in self._profiles]:
            message(_(u"Takto pojmenovaný profil již existuje."), beep_=True)
            return
        profile_id = profile_manager().new_user_profile_id(self._profiles)
        profile = self._create_profile(profile_id, title)
        self._profiles.append(profile)
        self._save_profile(profile)
        self._current_profile = profile
        message(_(u"Profil uložen pod názvem '%s'.") % title)
        self.focus()

    def _can_rename_profile(self, title):
        return self._is_user_defined_profile(self._current_profile)

    def _cmd_rename_profile(self, title):
        if title in [p.title() for p in self._profiles if p is not self._current_profile]:
            message(_(u"Takto pojmenovaný profil již existuje."), beep_=True)
            return
        index = self._profiles.index(self._current_profile)
        profile = self._create_profile(self._current_profile.id(), title)
        self._current_profile = self._profiles[index] = profile
        self._save_profile(self._current_profile)
        message(_(u"Profil uložen pod názvem '%s'.") % title)
        self.focus()

    def _can_update_profile(self):
        return (self._current_profile.id() != '__constructor_profile__'
                and self._current_profile_changed())
        
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
        profile_manager().drop_profile(self._profile_spec_name(), self._form_name(), self._current_profile.id())
        self._profiles.remove(self._current_profile)
        self._apply_profile(self._profiles[0])

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
            profile = find(profile_id, self._view.profiles(), key=lambda p: p.id())
        profile_manager().drop_profile(self._profile_spec_name(), self._form_name(), profile_id)
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
            perform, condition = \
                     run_dialog(FilterDialog, self._lf_sfs_columns(),
                                self.current_row(), self._compute_aggregate,
                                col=self._current_column_id(),
                                condition=self._lf_filter)
        if perform and condition != self._lf_filter:
            self.filter(condition)

    def _can_unfilter(self):
        return self._lf_filter is not None
        
    def _cmd_unfilter(self):
        self.filter(None)

    def _cmd_filter_by_value(self, column_id, value):
        if column_id not in [c.id() for c in self._lf_sfs_columns()]:
            message(_(u"Podle tohoto sloupce nelze filtrovat."), beep_=True)
        self.filter(pytis.data.EQ(column_id, value), append=True)

    def _cmd_sort(self, col=None, direction=None, primary=False):
        """Změň třídění.

        Argumenty:

          col -- id sloupce, podle kterého má být seznam setříděn, nebo
            'None' pro globální změny (například vypnutí veškerého třídění)
          direction -- směr třídění (sestupně/vzestupně/vůbec).  Hodnota daná
            konstantou 'LookupForm.SORTING_NONE' značí požadavek na zrušení
            třídění.  Jinak je očekávána jedna z konstant
            'LookupForm.SORTING_ASCENDENT' (pro sestupné třídění), nebo
            'LookupForm.SORTING_DESCENDANT' (pro vzestupné třídění).
          primary -- právě když je pravdivé, bude daný sloupec zvolen jako
            primární a *jediný* třídící sloupec.  V opačném případě bude pouze
            přidán na konec stávajícího seznamu třídících sloupců.
        
        Při nejednoznačné kombinaci argumentů 'col' a 'direction' je
        automaticky vyvolán dialog pro výběr třídících kritérií.
        
        """
        sorting = self._determine_sorting(col=col, direction=direction, primary=primary)
        if sorting is not None and sorting != self._lf_sorting:
            self._lf_sorting = sorting
            self.select_row(self._current_key())
        return sorting
    
    def _can_sort(self, col=None, direction=None, primary=False):
        # `col' je zde identifikátor sloupce.
        sorting_columns = tuple(self._sorting_columns())
        if direction == self.SORTING_NONE:
            return sorting_columns and (col is None or col in sorting_columns)
        elif direction is not None and col is not None:
            pos = self._sorting_position(col)
            dir = self._sorting_direction(col)
            if primary:
                return pos != 0 or direction != dir
            else:
                return pos != 0 and direction != dir and sorting_columns
        else:
            return True

    def _determine_sorting(self, col, direction, primary):
        if col is None and direction == self.SORTING_NONE:
            sorting = ()
        elif col is None or direction is None:
            columns = self._lf_sfs_columns()
            if col is None and self._lf_sorting: 
                col = self._sorting_columns()[0]
            if direction is not None:
                mapping = {self.SORTING_ASCENDENT:  pytis.data.ASCENDENT,
                           self.SORTING_DESCENDANT: pytis.data.DESCENDANT}
                direction = mapping[direction]
            sorting = run_dialog(SortingDialog, columns, self._lf_sorting,
                                 col=col, direction=direction)
            if sorting is None:
                return None
            elif sorting is ():
                sorting = self._lf_initial_sorting
            else:
                mapping = {pytis.data.ASCENDENT:  self.SORTING_ASCENDENT,
                           pytis.data.DESCENDANT: self.SORTING_DESCENDANT}
                sorting = tuple([(cid, mapping[dir]) for cid, dir in sorting])
        elif col is not None:
            if (not self._data.find_column(col) or
                not self._data.permitted(col, pytis.data.Permission.VIEW)):
                message(_(u"Podle tohoto sloupce nelze třídit"),
                        beep_=True)
                return None
            pos = self._sorting_position(col)
            sorting = xlist(self._lf_sorting)
            if direction == self.SORTING_NONE:
                del sorting[pos]
            else:
                assert direction in (self.SORTING_ASCENDENT,
                                     self.SORTING_DESCENDANT)
                new_sort_spec = (col, direction)
                if primary and pos !=0:
                    sorting = (new_sort_spec,)
                elif pos is None:
                    sorting.append(new_sort_spec)
                else:
                    sorting[pos] = new_sort_spec
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
        try:
            self._apply_filter(condition)
        except UserBreakException:
            self._lf_filter = old_filter
            self._apply_filter(self._lf_filter)
        if not self._is_user_defined_profile(self._current_profile) \
                and condition != self._current_profile.filter():
            title = _(u"Nepojmenovaný profil")
            profile = find(title, self._profiles, key=lambda p: p.title())
            if profile:
                profile_manager().drop_profile(self._profile_spec_name(), self._form_name(), profile.id())
                self._profiles.remove(profile)
            self._cmd_save_new_profile(title)

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

    def profiles(self):
        """Return the current form profiles as a list."""
        return self._profiles
    
    def current_profile(self):
        """Return the current form profile as 'pytis.presentation.Profile' instance."""
        return self._current_profile
        

class RecordForm(LookupForm):
    """Formulář schopný nějakým způsobem zobrazit aktuální záznam."""

    CALL_SELECTION = 'CALL_SELECTION'
    """Konstanta callbacku výběru (změny aktuálního) záznamu.

    Argumentem callbackové funkce je nově vybraný záznam jako instance
    'RecordForm.Record'.
    
    """
    class Record(PresentedRow):
        # Experimental PresentedRow extension aware of its parent form.  This might allow
        # application specific procedures more reliable access to the current form, from which the
        # row comes.
        def __init__(self, form, *args, **kwargs):
            self._form = form
            super(RecordForm.Record, self).__init__(*args, **kwargs)

        def form(self):
            return self._form

        def data(self):
            # Return a new instance rather than giving the internally used data object.
            # Moreover this instance will have the select initialized in LookupForm.
            return self._form.data()
    
    def _init_attributes(self, prefill=None, select_row=None, _new=False, _singleline=False,
                         **kwargs):
        """Process constructor keyword arguments and initialize the attributes.

        Arguments:

          prefill -- a dictionary of values used to prefill the current form row.  The meaning is
            the same as for the same 'PresentedRow' constructor argument.
          select_row -- The initially selected row -- the value is the same as the argument of the
            'select_row()' method.
          kwargs -- arguments passed to the parent class

        """
        super_(RecordForm)._init_attributes(self, **kwargs)
        assert prefill is None or isinstance(prefill, dict)
        self._prefill = prefill
        self._initial_select_row = select_row
        self._row = self.record(self._data_row(select_row), prefill=prefill, new=_new,
                                singleline=_singleline)

    def _signal_update(self):
        pass

    def _select_columns(self):
        return None

    def _on_idle(self, event):
        if super(RecordForm, self)._on_idle(event):
            return True
        if self._initial_select_row:
            # Repeat row selection even though the row should have been already
            # selected thanks to the constructor argument.  The problem with
            # the constructor argument, however, is that when the record is not
            # found, it is not anounced.  It must be done in _on_idle, because
            # we need the initial profile to be applied before.
            select_row = self._initial_select_row
            self._initial_select_row = None
            self.select_row(select_row)
            self.focus()
        return False

    def _find_row_by_number(self, row_number):
        # row_number starts with 0
        data = self._data
        current_row_number = data.last_row_number()
        def dbop():
            data.rewind()
            data.skip(row_number)
            return data.fetchone()
        success, row = db_operation(dbop)
        def dbop():
            data.rewind()
            data.skip(current_row_number + 1)
        db_operation(dbop)
        if not success or not row:
            return None
        else:
            return row
    
    def _find_row_by_values(self, cols, values):
        """Vrať datový řádek odpovídající daným hodnotám.

        Argumenty:

          cols -- sekvence názvů sloupců, které mají být prohledávány.
          values -- sekvence hodnot sloupců jako instancí 'pytis.data.Value' v
            pořadí odpovídajícím 'cols'.

        Pro obě sekvence platí, že pokud jsou jednoprvkové, mohou být hodnoty
        předány i přímo, bez obalení do sekvenčního typu.

        """
        cols = xtuple(cols)
        values = xtuple(values)
        assert len(cols) == len(values)
        data = self._data
        if cols == tuple([c.id() for c in data.key()]):
            # This saves the final _init_select call
            return self._find_row_by_key(values)
        cond = pytis.data.AND(*[pytis.data.EQ(c,v) for c,v in zip(cols, values)])
        condition = pytis.data.AND(cond, self._current_condition())
        def dbop(condition):            
            data.select(condition, columns=self._select_columns(),
                        transaction=self._transaction)
            return data.fetchone()
        success, row = db_operation(dbop, condition)
        self._init_select(async_count=True)
        return row
        
    def _find_row_by_key(self, key):
        cols = self._select_columns()
        success, row = db_operation(self._data.row, key, columns=cols,
                                    transaction=self._transaction,
                                    arguments=self._current_arguments())
        if success and row:
            return row
        else:
            return None
    
    def _get_row_number(self, row):
        """Vrať číslo řádku odpovídající dané instanci 'pytis.data.Row'.

        Pokud odpovídaící řádek není nalezen, vrať None.

        """
        data = self._data
        key = data.key()[0].id()
        def dbop():
            data.rewind()
            return data.search(pytis.data.EQ(key, row[key]), transaction=self._transaction,
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

    def _data_row(self, position):
        # Return the *data* row instance corresponding to given 'select_row()' argument.
        if position is None or isinstance(position, pytis.data.Row):
            row = position
        elif isinstance(position, int):
            row = self._find_row_by_number(position)
        elif isinstance(position, (tuple, pytis.data.Value)):
            row = self._find_row_by_key(xtuple(position))
        elif isinstance(position, dict):
            row = self._find_row_by_values(position.keys(), position.values())
        else:            
            raise ProgramError("Invalid 'position':", position)
        return row

    def _select_row(self, row, quiet=False):
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
        success, locked = db_operation(self._data.lock_row, key, transaction=self._transaction)
        if success and locked != None:
            log(EVENT, 'Record is locked')
            run_dialog(Message, _(u"Záznam je zamčen"))
            return False
        else:
            return True

    def _check_record(self, row):
        # Proveď kontrolu integrity dané instance PresentedRow.
        for check in self._view.check():
            result = check(row)
            if result is not None:
                if is_sequence(result):
                    failed_id, msg = result
                    message(msg)
                else:
                    failed_id = result
                    # TODO: Tím bychom přepsali zprávu nastavenou uvnitř
                    # 'check()'.  Pokud ale žádná zpráva nebyla nastavena,
                    # uživatel netuší...
                    #message(_(u"Kontrola integrity selhala!"))
                log(EVENT, 'Kontrola integrity selhala:', failed_id)
                return failed_id
        return None

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
        top = top_window()
        if isinstance(top, DualForm):
            main, side = top.main_form(), top.side_form()
            if self in (main, side):
                return top
            if isinstance(side, MultiForm) and self in side.forms():
                return top
        return None

    def _secondary_context_form(self):
        dual_form = self._dualform()
        if dual_form:
            if dual_form.main_form() is self:
                form = dual_form.side_form()
                if isinstance(form, MultiForm):
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
        context = action.context()
        if context == ActionContext.RECORD:
            args = (self.current_row(),)
        elif context == ActionContext.SELECTION:
            args = (self.selected_rows(),)
        else:
            raise ProgramError("Unsupported action context:", context)
        scontext = action.secondary_context()
        if scontext is not None:
            form = self._secondary_context_form()
            if scontext == ActionContext.RECORD:
                args += (form.current_row(),)
            elif scontext == ActionContext.SELECTION:
                args += (form.selected_rows(),)
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
        import copy as copy_
        if prefill is None:
            prefill = self._prefill and copy_.copy(self._prefill) or {}
        if copy:
            prefill.update(self._row_copy_prefill(self.current_row()))
        result = new_record(self._name, prefill=prefill)
        if result:
            if not self.select_row(result.row(), quiet=True):
                msg = _(u"Vložený záznam se neobjevil v aktuálním náhledu.")
                run_dialog(Warning, msg)
    
    def _can_edit_record(self):
        return self._current_key() is not None

    def _cmd_edit_record(self):
        if not self.check_permission(pytis.data.Permission.UPDATE, quiet=False):
            return
        row = self.current_row()
        on_edit_record = self._view.on_edit_record()
        if on_edit_record is not None:
            on_edit_record(row=row)
            # TODO: _signal_update vyvolá refresh.  To je tu jen pro případ, že
            # byla uživatelská procedura ošetřena jinak než vyvoláním
            # formuláře.  Protože to samo už je hack, tak ať si raději také
            # tvůrce provádí refresh sám, protože tady je volán ve všech
            # ostatních případech zbytečně a zdržuje.
            self._signal_update()
        else:
            name = self._name
            redirect = self._view.redirect()
            if redirect is None and self._data.arguments() is not None:
                message(_("Tento formulář je needitovatelný."), beep_=True)
                return
            if redirect is not None:
                redirected_name = redirect(row)
                if redirected_name is not None:
                    assert isinstance(redirected_name, basestring)
                    name = redirected_name
            kwargs = self._new_form_kwargs()
            key = self._current_key()
            run_form(PopupEditForm, name, select_row=key, **kwargs)

    def _can_delete_record(self):
        return self._current_key() is not None

    def _cmd_delete_record(self):
        # The return value is used in derived classes!
        if not self.check_permission(pytis.data.Permission.DELETE, quiet=False):
            return False
        if delete_record(self._view, self._data, self._transaction, self.current_row()):
            self._signal_update()
            return True
        else:
            return False
        
    def _can_context_action(self, action):
        if action.context() == ActionContext.SELECTION and len(self.selected_rows()) < 1:
            return False
        if action.secondary_context() is not None and self._secondary_context_form() is None:
            return False
        if not pytis.data.is_in_groups(action.access_groups()):
            return False
        enabled = action.enabled()
        if isinstance(enabled, collections.Callable):
            args = self._context_action_args(action)
            kwargs = action.kwargs()
            return enabled(*args, **kwargs)
        else:
            return enabled

    def _cmd_context_action(self, action):
        args = self._context_action_args(action)
        kwargs = action.kwargs()
        log(EVENT, 'Vyvolávám handler kontextové akce.', (args, kwargs))
        action.handler()(*args, **kwargs)
        # Hack: Pokud jsme součástí duálního formuláře, chceme refreshnout celý
        # dualform.  Jinak refreshujeme jen sebe sama.
        dual = self._dualform()
        if dual:
            dual.refresh()
        else:
            self.refresh()
        return True

    def _cmd_import_interactive(self):
        if not self._data.permitted(None, pytis.data.Permission.INSERT):
            msg = _(u"Nemáte práva pro vkládání záznamů do této tabulky.")
            message(msg, beep_=True)
            return False
        msg = _(u"Nejprve vyberte soubor obsahující importovaná data. " +
                u"Poté budete moci zkontrolovat a potvrdit každý záznam.\n\n" +
                u"*Formát vstupního souboru:*\n\n" +
                u"Každý řádek obsahuje seznam hodnot oddělených zvoleným " +
                u"znakem, nebo skupinou znaků (vyplňte níže). " +
                u"Tabelátor zapište jako ='\\t'=.\n\n" +
                u"První řádek obsahuje identifikátory sloupců a určuje tedy " +
                u"význam a pořadí hodnot v následujících (datových) řádcích.\n\n" +
                u"Identifikátory jednotlivých sloupců jsou následující:\n\n" +
                u"\n".join(["|*%s*|=%s=|" % (c.column_label(), c.id()) for c in
                           [self._view.field(id)
                            for id in self._view.layout().order()]]))
        separator = run_dialog(InputDialog, 
                               title=_(u"Hromadné vkládání dat"),
                               report=msg, report_format=TextFormat.LCG,
                               prompt="Oddělovač", value='|')
        if not separator:
            if separator is not None:
                message(_(u"Nebyl zadán oddělovač."), beep_=True)
            return False
        separator = separator.replace('\\t', '\t')
        while 1:
            filename = run_dialog(FileDialog)
            if filename is None:
                message(_(u"Nebyl zadán soubor. Proces ukončen."), beep_=True)
                return False
            try:
                fh = open(filename)
            except IOError as e:
                msg = _(u"Nepodařilo se otevřít soubor '%s': %s")
                run_dialog(Error, msg % (filename, str(e)))
                continue
            break
        try:
            columns = [str(id.strip()) for id in fh.readline().split(separator)]
            for id in columns:
                if id not in self._row:
                    run_dialog(Error, _(u"Neznámý sloupec: %s") % id)
                    return False
            types = [self._row.type(id) for id in columns]
            line_number = 1
            data = []
            for line in fh:
                line_number += 1
                values = line.rstrip('\r\n').split(separator)
                if len(values) != len(columns):
                    msg = _(u"Chyba dat na řádku %d:\n" +
                            u"Počet hodnot neodpovídá počtu sloupců.")
                    run_dialog(Error, msg % line_number)
                    return False
                row_data = []
                for id, type, val in zip(columns, types, values):
                    value, error = type.validate(val, transaction=self._transaction)
                    if error:
                        msg = _(u"Chyba dat na řádku %d, sloupec '%s':\n%s") % \
                              (line_number, id, error.message())
                        run_dialog(Error, msg)
                        return False
                    assert value.type() == type, (value.type(), type)
                    row_data.append((id, value))
                data.append(pytis.data.Row(row_data))
        finally:
            fh.close()
        new_record(self._name, prefill=self._prefill, inserted_data=data)

    def _cmd_open_editor(self, field_id):
        run_form(StructuredTextEditor, self.name(),
                 field_id=field_id, select_row=self.current_key())
            
    # Public methods

    def record(self, row, **kwargs):
        """Create a new `RecordForm.Record' instance bound to this form."""
        fields = self._view.fields()
        data = self._create_data_object()
        return self.Record(self, fields, data, row, transaction=self._transaction, **kwargs)
    
    def select_row(self, position, quiet=False):
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
        row = self._data_row(position)
        if (not quiet and
            position is not None and
            (not is_sequence(position) or len(position) != 1 or position[0].value() is not None) and
            row is None):
            if self._search_again_unfiltered():
                self._apply_profile(self._profiles[0])
                return self.select_row(position)
            return False
        return self._select_row(row, quiet=quiet)

    def _search_again_unfiltered(self):
        if self._lf_filter:
            if run_dialog(Question, title=_(u"Záznam nenalezen"),
                          message=_(u"Požadovaný záznam nebyl nalezen. "
                                    u"Může to být tím, že formulář má zapnutý filtr.\n"
                                    u"Chcete aktivovat výchozí profil a zkusit záznam "
                                    u"vyhledat znovu?")):
                return True
        else:
            run_dialog(Warning, _(u"Záznam nenalezen"))
        return False
    
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
        return ()

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
            def next(self):
                if self._row_number >= self._table.GetNumberRows():
                    raise StopIteration
                row = self._table.row(self._row_number)
                self._row_number += 1
                return row
        return Iterator(self._table)
    

### Editační formulář


class EditForm(RecordForm, TitledForm, Refreshable):
    """Formulář pro editaci všech vlastností jednoho záznamu.

    Formulář je vytvořen poskládáním jednotlivých vstupních políček daných
    specifikací do mřížky.  Pole mohou být různě seskupována a jejich rozložení
    je určeno specifikační třídou 'LayoutSpec' resp. 'GroupSpec'.

    Každé vstupní pole je reprezentováno objektem třídy 'InputField'.  To se
    stará o interakci s uživatelem, validaci vstupních dat apod.

    Formulář může sloužit jak k prohlížení či editaci stávajících dat, tak
    i k vytváření nových záznamů (viz argument konstruktoru 'mode').
 
    """

    _LOG_STATISTICS = False

    MODE_INSERT = 'MODE_INSERT'
    """Mód formuláře pro vkládání nových záznamů."""
    MODE_EDIT = 'MODE_EDIT'
    """Mód formuláře pro editaci stávajících záznamů."""
    MODE_VIEW = 'MODE_VIEW'
    """Mód formuláře pro zobrazení záznamů bez možnosti editace."""
    
    def _full_init(self, *args, **kwargs):
        super(EditForm, self)._full_init(*args, **kwargs)
        # Calculate the ideal total form size.  We first count the size of the sizer without the
        # form panel (that's why its hidden and shown below) and then add the manually computed
        # size of the panel to the result.  The size of the forma panel computed by wx Windows is
        # totaly useless.  It doesn't reflect the form content at all.
        panel = self._form_controls_window
        panel.Show(False)
        size = self.GetSizer().CalcMin()
        panel.Show(True)
        if isinstance(panel, (wx.ScrolledWindow, wx.Panel)):
            # The size of a scrollable window is simly its virtual size.
            panel_size = panel.GetVirtualSize()
            width = panel_size.width
            height = panel_size.height
            if (wx.MAJOR_VERSION, wx.MINOR_VERSION) == (2, 6):
                width += 16
                height += 16
        else:
            # If the form controls window is a wx.Notebook instance we need to
            # count the size of the largest tab page.
            width = height = 0
            for i in range(panel.GetPageCount()):
                page_size = panel.GetPage(i).GetSizer().CalcMin()
                width = max(page_size.width, width)
                height = max(page_size.height, height)
            # Modify the computed size by some empiric numbers...
            width += 8 # Add space for border manually.
            height += 53 # Add space for border and Notebook tabs.
        size.width = max(size.width, width)
        size.height += height
        self._size = size
        if isinstance(self._parent, wx.Dialog):
            wx_callback(wx.EVT_INIT_DIALOG, self._parent, self._set_focus_field)
        else:
            self._set_focus_field()

    def _init_attributes(self, mode=MODE_EDIT, focus_field=None, set_values=None, **kwargs):
        """Process constructor keyword arguments and initialize the attributes.

        Arguments:

          mode -- one of the 'MODE_*' constants.  Determines whether the form
            is primarily for viewing, editation or creation of records.
          focus_field -- identifier of the field which should be activated for
            user input on form startup.  If None, the first field is the
            default.  It is also possible to pass a function of one argument --
            the PresentedRow instance representing the current record.  This
            function must return a field identifier or None.
          kwargs -- arguments passed to the parent class
          set_values -- dictionary of row values to set in the newly openened
            form.  If not None, the dictionary keys are field identifiers and
            values are either the corresponding internal python values valid
            for the fields's data type or pytis.data.Value() instances
            directly.  These values will not affect the initial row state
            (unlike the 'prefill' argument of the parent class) and thus will
            appear as changed to the user.
         
        """
        assert mode in (self.MODE_EDIT, self.MODE_INSERT, self.MODE_VIEW)
        new = mode == self.MODE_INSERT
        super_(EditForm)._init_attributes(self, _new=new, **kwargs)
        self._mode = mode
        self._focus_field = focus_field or self._view.focus_field()
        # Other attributes
        self._fields = []
        if set_values:
            for key, value in set_values.items():
                type = self._row.type(key)
                if isinstance(value, pytis.data.Value):
                    value = value.retype(type)
                else:
                    value = pytis.data.Value(type, value)
                self._row[key] = value

    def _set_focus_field(self, event=None):
        """Inicalizuj dialog nastavením hodnot políček."""
        if self._focus_field:
            if isinstance(self._focus_field, collections.Callable):
                focused = self._focus_field(self._row)
            else:
                focused = self._focus_field
            if find(focused, self._fields, key=lambda f: f.id()):                
                f = self._field(focused)
        else:
            f = find(True, self._fields, key=lambda f: f.enabled())
            if f is None:
                f = self._fields[0]
        f.set_focus()

    def _create_form_parts(self, sizer):
        # Create all parts and add them to top-level sizer.
        sizer.Add(self._create_title_bar(), 0, wx.EXPAND)
        sizer.Add(self._create_form_controls(), 1, wx.EXPAND)

    def _create_form_controls(self):
        self._fields = []
        group = self._view.layout().group()
        if isinstance(group, TabGroup):
            window = wx.Notebook(self)
            for item in group.items():
                if len(item.items()) == 1 and isinstance(item.items()[0], GroupSpec):
                    group = item.items()[0]
                else:
                    group = GroupSpec(item.items(), orientation=Orientation.VERTICAL)
                panel = self._create_group_panel(window, group)
                window.AddPage(panel, item.label())
        else:
            window = self._create_group_panel(self, group)
        # Store the scrollable form panel to be able to compute the popup
        # window size.  If we want to keep the panel scrollable, it will not
        # propagate its size through the sizer automatically in wx 2.8.
        self._form_controls_window = window
        return window

    def _create_group_panel(self, parent, group):
        panel = wx.ScrolledWindow(parent, style=wx.TAB_TRAVERSAL)
        panel.SetScrollRate(20, 20)
        # Create the form controls first, according to the order.
        fields = [InputField.create(panel, self._row, id, guardian=self, readonly=self.readonly())
                  for id in group.order() if self._view.field(id).width() != 0]
        self._fields.extend(fields)
        # Create the layout groups.
        group_sizer = self._create_group(panel, group)
        # Add outer sizer with margins and alignment. 
        sizer = wx.BoxSizer(wx.VERTICAL)
        sizer.Add(group_sizer, 0, wx.ALIGN_CENTER|wx.LEFT|wx.RIGHT, 8)
        panel.SetSizer(sizer)
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
            action = find(button.action(), self._view.actions(linear=True), key=lambda a: a.name())
            label = button.label() or action.title()
            tooltip = button.tooltip() or action.descr()
            cmd, args = self.COMMAND_CONTEXT_ACTION(action=action)
        return wx_button(parent, label, command=(cmd, args), tooltip=tooltip,
                         enabled=(button.active_in_popup_form() or not isinstance(self, PopupForm))
                                  and (button.active_in_readonly_form() or not self.readonly())
                                  and cmd.enabled(**args),
                         width=button.width() and dlg2px(parent, 4*button.width()))

    def _create_text(self, parent, text):
        return wx.StaticText(parent, -1, text.text(), style=wx.ALIGN_LEFT)

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
        space = dlg2px(parent, group.space())
        gap = dlg2px(parent, group.gap())
        border = dlg2px(parent, group.border())
        border_style = border_style2wx(group.border_style())
        for i, item in enumerate(group.items()):
            if is_anystring(item):
                if self._view.field(item).width() == 0:
                    continue
                item = self._field(item)
            if group.orientation() == Orientation.VERTICAL:
                if isinstance(item, (Button, Text)) \
                        or isinstance(item, InputField) \
                        and not item.spec().compact() \
                        or isinstance(item, GroupSpec) \
                        and item.label() is None \
                        and item.orientation() == Orientation.HORIZONTAL \
                        and is_anystring(item.items()[0]) \
                        and not self._field(item.items()[0]).spec().compact() \
                        and not isinstance(self._field(item.items()[0]).type(), pytis.data.Boolean):
                    # This item will become a part of the current aligned pack.
                    # Nested horizontal groups are aligned if they start with a labeled field.
                    pack.append(item)
                    continue
            if len(pack) != 0:
                # Add the latest aligned pack into the sizer (if there was one).
                sizer.Add(self._pack_fields(parent, pack, space, gap),
                          0, wx.ALIGN_TOP|border_style, border)
                pack = []
            if isinstance(item, GroupSpec):
                x = self._create_group(parent, item)
            elif isinstance(item, InputField):
                if item.spec().compact():
                    # This is a compact field (not a part of the aligned pack).
                    x = wx.BoxSizer(wx.VERTICAL)
                    x.Add(item.label(), 0, wx.ALIGN_LEFT)
                    x.Add(item.widget())
                else:
                    # Fields in a HORIZONTAL group are packed separately (label and ctrl).
                    x = self._pack_fields(parent, (item,), space, gap,
                                          suppress_label=(i==0 and aligned))
            elif isinstance(item, Button):
                x = self._create_button(parent, item)
            elif isinstance(item, Text):
                x = self._create_text(parent, item)
            else:
                raise ProgramError("Unsupported layout item!", item)
            bstyle = border_style
            if aligned:
                bstyle = bstyle & ~(wx.LEFT|wx.TOP|wx.BOTTOM)
            sizer.Add(x, 0, wx.ALIGN_TOP|bstyle, border)
        if len(pack) != 0:
            # přidej zbylý sled políček (pokud nějaký byl)
            sizer.Add(self._pack_fields(parent, pack, space, gap),
                      0, wx.ALIGN_TOP|border_style, border)
        # pokud má skupina orámování, přidáme ji ještě do sizeru s horním
        # odsazením, jinak je horní odsazení příliš malé.
        if group.label() is not None:
            s = wx.BoxSizer(orientation)
            s.Add(sizer, 0, wx.TOP, 2)
            sizer = s
        return sizer

    def _pack_fields(self, parent, items, space, gap, suppress_label=False):
        # Pack the sequence of fields and/or buttons aligned vertically into a grid.
        #  items -- sequence of field identifiers and/or Button instances.
        #  space -- space between the control and its label in dlg units; integer
        #  gap -- space between the fields in dlg units; integer
        #  suppress_label -- True if the field label should be supressed.  Used
        #    for vertically aligned horizontal groups (the label is placed in
        #    the parent pack)
        grid = wx.FlexGridSizer(len(items), 2, gap, space)
        for item in items:
            if isinstance(item, GroupSpec):
                field = self._field(item.items()[0])
                label = field.label()
                if label: 
                    grid.Add(label, 0, wx.ALIGN_RIGHT|wx.ALIGN_CENTER_VERTICAL, 2)
                grid.Add(self._create_group(parent, item, aligned=True))
            elif isinstance(item, (Button, Text)):
                style = wx.ALIGN_RIGHT|wx.ALIGN_CENTER_VERTICAL
                label = wx.StaticText(parent, -1, "", style=wx.ALIGN_RIGHT)
                grid.Add(label, 0, style, 2)
                if isinstance(item, Button):
                    grid.Add(self._create_button(parent, item))
                else:
                    grid.Add(self._create_text(parent, item))
            else:
                if not suppress_label:
                    if item.height() > 1:
                        style = wx.ALIGN_RIGHT|wx.ALIGN_TOP|wx.TOP
                    else:
                        style = wx.ALIGN_RIGHT|wx.ALIGN_CENTER_VERTICAL
                    grid.Add(item.label(), 0, style, 2)
                grid.Add(item.widget())
        return grid

    def _signal_update(self):
        f = current_form()
        if isinstance(f, Refreshable):
            f.refresh()

    def _refresh(self, when=None, interactive=False):
        if interactive:
            self._update_arguments()
        self.Refresh()

    def _commit_data(self, op, args):
        if op is not None:
            success, result = db_op(op, args, dict(transaction=self._transaction),
                                    in_transaction=(self._transaction is not None))
        else:
            success, result = True, (None, True)
        return success, result

    def _commit_form(self, close=True):
        if self._mode == self.MODE_INSERT:
            permission = pytis.data.Permission.INSERT
        elif self._mode == self.MODE_EDIT:
            permission = pytis.data.Permission.UPDATE
        else:
            permission = None
        # Re-validate all fields.
        for f in self._fields:
            fid = f.id()
            if (self._mode == self.MODE_EDIT and
                not self._row.permitted(fid, pytis.data.Permission.VIEW) and
                fid in self._row and not self._row[fid].value()):
                self._row[fid] = self._row.original_row()[fid]
            elif (self._mode == self.MODE_INSERT and
                  not self._row.permitted(fid, pytis.data.Permission.VIEW)):
                for rf in self._row.fields():
                    if rf.id() == fid:
                        type_ = self._row.type(fid)
                        default = rf.default()
                        if default is not None:
                            if isinstance(default, collections.Callable):
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
        # Ověření integrity záznamu (funkce check).
        failed_id = self._check_record(self._row)
        if failed_id:
            f = self._field(failed_id)
            if f:
                f.set_focus()
            else:
                log(OPERATIONAL, "Unknown field returned by check():", failed_id)
            return False
        # Vytvoření datového řádku.
        rdata = self._record_data(self._row, permission=permission,
                                  updated=(self._mode == self.MODE_EDIT))
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
        # Provedení operace
        transaction = self._transaction
        if transaction is not None:
            success, result = db_op(transaction.set_point, ('commitform',),
                                    in_transaction=True, quiet=True)
        else:
            success = True
        if success:
            success, result = self._commit_data(op, args)
        else:
            result = (None, False)
        if success and result[1]:
            new_row = result[0]
            original_row = copy.copy(self._row)
            if new_row is None:
                new_row = self._row.row()
            self._row.set_row(new_row, reset=True)
            self._signal_update()
            if op is not None:
                if self._mode == self.MODE_INSERT:
                    log(ACTION, 'Record inserted')
                else:
                    log(ACTION, 'Record updated')
            cleanup = self._view.cleanup()
            if cleanup is not None:
                cleanup(self._row, original_row)
            if close:
                self._result = self._row
                self.close()
            if self._governing_transaction is None and self._transaction is not None:
                db_op(self._transaction.commit, in_transaction=True, quiet=True)
                if close:
                    self._transaction = None
                else:
                    self._transaction = self._default_transaction()
                self._row.set_transaction(self._transaction)
                log(ACTION, 'Transaction committed')
            return True
        else:
            if self._transaction is not None:
                success, __ = db_op(self._transaction.cut, ('commitform',), in_transaction=True,
                                    quiet=True)
            else:
                success = True
            if success:
                msg = _(u"Uložení záznamu se nezdařilo")
            else:
                msg = _(u"Transakce přerušena, nelze pokračovat")
            if type(result) == type(()) and \
               isinstance(result[0], basestring):
                msg = "%s\n\n%s" % (result[0], msg)
            run_dialog(Error, msg)
            return False

    def _insert_op_args(self, rdata):
        return self._data.insert, (rdata,)

    def _update_op_args(self, rdata):
        return self._data.update, (self._current_key(), rdata)
        
    def _exit_check(self):
        if self.changed():
            q = _(u"Data byla změněna a nebyla uložena!") + "\n" + \
                _(u"Opravdu chcete uzavřít formulář?")
            if not run_dialog(Question, q):
                return False
        return True

    # Command handling
    
    def _can_commit_record(self, close=True):
        return self._mode != self.MODE_VIEW
    
    def _cmd_commit_record(self, close=True):
        result = self._commit_form(close=close)
        if result:
            refresh()
        return result

    def _cmd_navigate(self, back=False):
        if self._mode != self.MODE_VIEW:
            # Vygeneruj událost navigace mezi políčky.
            w = wx_focused_window()
            if not w:
                self._fields[0].set_focus()
                w = wx_focused_window()
            if w:
                flags = not back and wx.NavigationKeyEvent.IsForward or 0
                w.Navigate(flags=flags)
    
    # Public methods

    def title(self):
        """Return the form title as a string."""        
        return self._view.layout().caption()

    def size(self):
        """Return the prefered form size in pixels as a tuple of two integers (width, height).

        The returned size in this class represents the total size of the form
        needed to display all fields without scrolling.  The current real size
        may be greater or smaller depending on the size of the window where the
        form is displayed.
        
        """
        return (self._size.width, self._size.height) 

    def field(self, id):
        """Return the 'InputField' instance for the field 'id'.

        Raises 'ProgramError' if the field of given id does not exist.
        
        """
        return self._field(id)
    
    def changed(self):
        """Return true iff the form data was changed since last saved."""
        return self._row.changed()

    def readonly(self):
        """Return true iff the form is read only."""
        return self._mode == self.MODE_VIEW

    
class PopupEditForm(PopupForm, EditForm):
    """Stejné jako 'EditForm', avšak v popup podobě."""

    DESCR = _(u"editační formulář")

    def __init__(self, parent, *args, **kwargs):
        parent = self._popup_frame(parent)
        EditForm.__init__(self, parent, *args, **kwargs)
    
    def _full_init(self, *args, **kwargs):
        EditForm._full_init(self, *args, **kwargs)
        if self._inserted_data is not None:
            self._load_next_row()
        # Set the popup window size according to the ideal form size limited to
        # the screen size.  If the form size exceeds the screen, scrollbars
        # will appear.
        size = wx.Size(*self.size())
        size.DecTo(wx.GetDisplaySize() - wx.Size(50, 80))
        self.SetClientSize(size)
        # HACK: Avoid initial selec_row call in popup forms.  It's purpose is
        # irrelevant here and it causes problems with set_values - the
        # additional select_row() call resets them.
        self._initial_select_row = None

    def _default_transaction(self):
        return pytis.data.DBTransactionDefault(config.dbconnection)
        
    def _init_attributes(self, inserted_data=None, multi_insert=True, **kwargs):
        """Process constructor keyword arguments and initialize the attributes.

        Arguments:

          inserted_data -- allows to pass a sequence of 'pytis.data.Row' instances to be inserted.
            The form is then gradually prefilled by values of these rows and the user can
            individually accept or skip each row.

          multi_insert -- boolean flag indicating whether inserting multiple values is permitted.
            This option is only relevant in insert mode.  False value will disable this feature and
            the `Next' button will not be present on the form.

          kwargs -- arguments passed to the parent class
            
        """
        EditForm._init_attributes(self, **kwargs)
        assert inserted_data is None or self._mode == self.MODE_INSERT, (inserted_data, self._mode)
        assert isinstance(multi_insert, bool), multi_insert
        assert multi_insert or inserted_data is None, (multi_insert, inserted_data)
        self._inserted_data = inserted_data
        self._inserted_data_pointer = 0
        self._multi_insert = multi_insert

    def _create_form_parts(self, sizer):
        # Create all parts and add them to top-level sizer.
        caption = self._create_caption(self, size=18)
        panel = self._create_form_controls()
        buttons = self._create_buttons()
        status_bar = self._create_status_bar()
        # Add parts to the sizer.
        sizer.Add(caption, 0, wx.ALIGN_CENTER|wx.ALL, 8)
        sizer.Add(panel, 1, wx.EXPAND)
        sizer.Add(buttons, 0, wx.ALIGN_CENTER)
        sizer.Add(status_bar, 0, wx.EXPAND)

    def _create_status_bar(self):
        # We use our own statusbar implementation
        spec = (('message', None, _(u"Oznamovací oblast")),)
        if self._inserted_data is not None:
            spec += (('progress', 9, _(u"Ukazatel pozice hromadného vkládání")),)
        box = wx.BoxSizer()
        self._status_fields = dict(
            [(id, self._create_status_bar_field(box, width, descr))
             for id, width, descr in spec])
        return box

    def _create_status_bar_field(self, sizer, width, descr):
        panel = wx.Panel(self, -1, style=wx.SUNKEN_BORDER)
        panel.SetToolTipString(descr)
        panel.SetAutoLayout(True)
        box = wx.BoxSizer()
        field = wx.StaticText(panel, -1, '', style=wx.ALIGN_LEFT)
        box.Add(field, 1, wx.EXPAND|wx.ALL, 2)
        if width is not None:
            width = dlg2px(field, 4*width)
            height = field.GetSize().GetHeight()
            field.SetMinSize((width, height))
            expansion = 0
        else:
            expansion = 1
        sizer.Add(panel, expansion, wx.EXPAND)
        panel.SetSizer(box)
        box.Fit(panel)
        return field

    def _load_next_row(self):
        data = self._inserted_data
        if data is None or self._inserted_data_pointer > 0:
            self._row.set_row(None, reset=True, prefill=self._prefill)
        if data is not None:
            i = self._inserted_data_pointer
            self._select_row(None)
            if i < len(data):
                self.set_status('progress', "%d/%d" % (i+1, len(data)))
                self._inserted_data_pointer += 1
                ok_button = wx.FindWindowById(wx.ID_OK, self._parent)
                ok_button.Enable(i == len(data)-1)
                for id, value in data[i].items():
                    self._row[id] = pytis.data.Value(self._row.type(id), value.value())
            else:
                self.set_status('progress', '')
                run_dialog(Message, _(u"Všechny záznamy byly zpracovány."))
                self._inserted_data = None
        self._set_focus_field()

    def _exit_check(self):
        i = self._inserted_data_pointer
        data = self._inserted_data
        if data is not None and i <= len(data):
            msg = _(u"Ještě nebyly zpracovány všechny řádky vstupních dat.\n" +
                    u"Chcete opravdu ukončit vkládání?")
            if not run_dialog(Question, msg, default=False):
                return False
        return super(PopupEditForm, self)._exit_check()

    def _on_skip_button(self, event):
        i = self._inserted_data_pointer
        if self._inserted_data is None:
            message(_(u"Není další záznam"), beep_=True)
        else:
            message(_(u"Záznam %d/%d přeskočen") % (i, len(self._inserted_data)))
            self._load_next_row()
    
    def _buttons(self):
        buttons = (dict(id=wx.ID_OK,
                        tooltip=_(u"Uložit záznam a uzavřít formulář"),
                        command=self.COMMAND_COMMIT_RECORD()),
                   dict(id=wx.ID_CANCEL,
                        tooltip=_(u"Uzavřít formulář bez uložení dat"),
                        command=self.COMMAND_LEAVE_FORM()))
        if self._mode == self.MODE_INSERT and self._multi_insert:
            buttons += (dict(id=wx.ID_FORWARD, label=_(u"Další"), #icon=wx.ART_GO_FORWARD, 
                             tooltip=_(u"Uložit záznam a reinicializovat formulář" +
                                       u" pro vložení dalšího záznamu"),
                             command=self.COMMAND_COMMIT_RECORD(next=True)),)
        if self._inserted_data is not None:
            buttons += (dict(label=_(u"Přeskočit"),
                             tooltip=_(u"Přeskočit tento záznam bez uložení"),
                             callback=self._on_skip_button),)
        return buttons
        
    def _create_buttons(self):
        sizer = wx.BoxSizer(wx.HORIZONTAL)
        for i, kwargs in enumerate(self._buttons()):
            button = wx_button(self, fullsize=True, **kwargs)
            if i == 0:
                button.SetDefault()
            sizer.Add(button, 0, wx.ALL, 20)
        return sizer

    def _apply_profile(self, profile, refresh=True):
        # Popup forms ignore profiles.  It doesn't make much sense and they
        # were not designed for something like that.  It would be more lgical
        # to change the inheritance hierarchy so that single record forms don't
        # inherit profiles at all but that's a little too complicated for now.
        pass

    def _apply_initial_profile(self):
        # See the comment for _apply_profile() above.
        pass
    
    def _can_commit_record(self, close=True, next=False):
        if next and (self._mode != self.MODE_INSERT or not self._multi_insert):
            return False
        return super(PopupEditForm, self)._can_commit_record()
    
    def _cmd_commit_record(self, close=True, next=False):
        result = super(PopupEditForm, self)._cmd_commit_record(close=close and not next)
        if result and next:
            message(_(u"Záznam uložen"))
            self._load_next_row()
        return result
        
    def _cmd_safe_leave_form(self):
        # Leaving form in idle thread causes recursion in popup forms, when a
        # dialog asking for confirmation of leaving unsaved form appears.  So
        # we just leave the form immediately here.  Hopefuly, the problems
        # which lead to moving _leave_form to the idle thread don't apply to
        # popup forms.
        self._cmd_leave_form()
        
    def can_command(self, command, **kwargs):
        if command.handler() in (LookupForm, RecordForm) \
               and command != RecordForm.COMMAND_CONTEXT_ACTION:
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
            self._status_fields[field].SetLabel(unicode(message or ''))
            return True
        else:
            return False

    def set_row(self, row):        
        if self._transaction is None:
             self._transaction = pytis.data.DBTransactionDefault(config.dbconnection) 
        super(PopupEditForm, self).set_row(row)


class InputForm(PopupEditForm):
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

    """
    def _full_init(self, parent, resolver, name, guardian=None, transaction=None,
                   prefill=None, **kwargs):
        class Spec(Specification):
            data_cls = pytis.data.RestrictedMemData
        for key, value in kwargs.items():
            if isinstance(value, collections.Callable):
                # This is necessary to avoid calling functions (such as 'check'
                # or 'row_style') as methods.
                funcion = value
                value = lambda self, *args, **kwargs: funcion(*args, **kwargs)
            setattr(Spec, key, value)
        self._specification = Spec(resolver)
        super(InputForm, self)._full_init(parent, resolver, name, guardian=guardian,
                                          mode=self.MODE_INSERT, multi_insert=False,
                                          prefill=prefill, transaction=transaction)
        
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
    
    def _create_form_parts(self, sizer):
        panel = wx.Panel(self)
        field_sizer = wx.BoxSizer()
        panel.SetSizer(field_sizer)
        for field_id in self._resizable_fields():
            if self._view.field(field_id).width() != 0:
                field = InputField.create(panel, self._row, field_id, guardian=self,
                                          readonly=self.readonly())
                self._fields.append(field)
                field_sizer.Add(field.widget(), 1, wx.EXPAND)
        self._form_controls_window = panel
        sizer.Add(panel, 1, wx.EXPAND)
        sizer.Add(self._create_status_bar(), 0, wx.EXPAND)


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
        result = super(StructuredTextEditor, self)._check_record(row)
        if result is None:
            # Check for possible conflicting changes made since the record was
            # last saved.  If the current value in database doesn't match the
            # value before editation, someone else probably changed it.  The
            # resolution is left up to the user.
            data = create_data_object(self._name)
            success, db_row = db_operation(data.row, self._current_key(),
                                           columns=self._select_columns(),
                                           arguments=self._current_arguments())
            current_db_value = db_row[self._editor_field_id].value()
            value_before_edits = row.original_row()[self._editor_field_id].value()
            if current_db_value != value_before_edits:
                diff = pytis.util.html_diff(value_before_edits, current_db_value,
                                            _("Původní text"), _("Kolegova nová verze"))
                msg = _(u"Někdo jiný změnil stejný text během doby, kdy jste prováděl(a) "
                        u"tyto úpravy.  Níže vidíte výpis změn, které byly provedeny. "
                        u"Nejbezpečnější je vaše změny zahodit a začít znovu upravovat\n"
                        u"novou podobu textu.  Pokud byly vaše úpravy příliš rozsáhlé, "
                        u"můžete se pokusit obě verze sloučit tím, že do Vašeho současného "
                        u"textu aplikujete níže uvedené změny.")
                revert, merge, ignore = (_(u"Zahodit moje změny"), _(u"Sloučit"),
                                         _(u"Ignorovat kolegovy změny"))
                answer = run_dialog(MultiQuestion, title=_(u"Konflikt editace"), message=msg,
                                    report=diff, report_format=TextFormat.HTML,
                                    buttons=(revert, ignore)) # TODO: Add merge button.
                if answer == merge:
                    result = self._editor_field_id
                elif answer == ignore:
                    pass
                elif answer == revert:
                    value = pytis.data.Value(row.type(self._editor_field_id), current_db_value)
                    row[self._editor_field_id] = value
        return result

    def size(self):
        return (700, 500)

        
class PopupInsertForm(PopupEditForm):
    
    DESCR = _(u"vkládací formulář")
    
    def _init_attributes(self, **kwargs):
        super_(PopupInsertForm)._init_attributes(self, mode=EditForm.MODE_INSERT, **kwargs)
        
        
class ShowForm(EditForm):
    """Formulář pro zobrazení náhledu.

    Layout je stejný jako u editačního formuláře (resp. 'EditForm'),
    pouze titulek má stejný vzhled, jako titulek formulářů typu 'ListForm'.
    Určen pro zobrazení v duálním formuláři.

    """

    DESCR = _(u"náhledový formulář")

    def _init_attributes(self, mode=EditForm.MODE_VIEW, select_row=0,**kwargs):
        super_(ShowForm)._init_attributes(self, mode=mode, select_row=select_row, **kwargs)
        
    def changed(self):
        # Since the row is not reset when the current record changes, it would report a change...
        return False

        
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
            if row_number == self._lf_count(min_value=row_number+1):
                message(_(u"Poslední záznam"), beep_=True)
                return
        else:
            if row_number == 0:
                message(_(u"První záznam"), beep_=True)
                return
            row_number -= 1
        self._select_row(self._find_row_by_number(row_number))

    def _select_row(self, row, quiet=False):
        result = super(BrowsableShowForm, self)._select_row(row, quiet=quiet)
        current_row = self.current_row()
        total = self._lf_count(timeout=0)
        if not isinstance(self._lf_select_count_, int):
            total = '%s?' % (total,)
        if current_row and total:
            n = self._get_row_number(current_row)
            position = "%d/%s" % (n is not None and n+1 or 0, total)
            set_status('list-position', position)
        return result
                     

class WebForm(Form, Refreshable):
    """Web browser embedded in a Pytis form.

    The form shows a browser window as its main content.

    """
    def _create_view_spec(self):
        # This is quite a hack.  The base Form class should be independent of
        # pytis specifications and data objects as it shows, that we want also
        # forms which don't deal with the database at all.
        return None

    def _create_data_object(self):
        return None
    
    def _init_attributes(self, **kwargs):
        super_(WebForm)._init_attributes(self, **kwargs)
        self._async_browser_interaction = None
        
    def _create_form_parts(self, sizer):
        self._browser = browser = Browser(self)
        sizer.Add(browser.toolbar(self), 0, wx.EXPAND|wx.FIXED_MINSIZE)
        sizer.Add(browser, 1, wx.EXPAND)

    def _refresh(self, when=None, interactive=False):
        self._browser.reload()
