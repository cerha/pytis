# -*- coding: iso-8859-2 -*-

# Copyright (C) 2001, 2002, 2003, 2004, 2005 Brailcom, o.p.s.
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

import time
import pytis.data
import pytis.output
from pytis.presentation import PresentedRow
from pytis.form import *
import wx

class Form(Window, KeyHandler, CallbackHandler):
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

      print_spec -- sekvence dvojic (POPIS, SOUBOR), kde POPIS je string se
        stručným slovním popisem specifikace (využívaným například jako titulek
        položky menu) a SOUBOR je string udávající jméno souboru se
        specifikací, relativní k adresáři s definičními soubory, bez přípony

    """
    ACT_FORM = 'ACT_FORM'
    """Aktivační konstanta formuláře."""
    
    ACTIVATIONS = Window.ACTIVATIONS + [ACT_FORM]
    """Seznam aktivačních kategorií pro tuto třídu."""

    _STATUS_FIELDS = ()
    _DESCR = None
    
    def __init__(self, parent, resolver, name, guardian=None, **kwargs):
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
          kwargs -- viz níže.

        Resolver je použit k získání datové a prezentační specifikace a
        následnému vytvoření datového objektu. Ten je potom společně s
        prezentační specifikací uložen v podobě atributů vytvářené instance.

        Odkaz na resolver samotný je také zapamatován pro pozdější použití
        (vytváření dalších formulářů).

          
        Inicializace je rozdělena do několika kroků.  Nejprve jsou zpracováný
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
        Pokud nějaké zbydou, vyvolá bázová třída výjimku 'AssertionError'.

        Teprve po zpravování argumentů konstruktoru a inicializaci atributů je
        vytvářen vlastní obsah formuláře (viz. '_create_form()').  Toto by mělo
        být dodržováno i v odvozených třídách.
        
        """
        self._parent = parent
        self._resolver = resolver
        self._name = name
        self._guardian = guardian or parent
        Window.__init__(self, parent)
        KeyHandler.__init__(self)
        CallbackHandler.__init__(self)
        start_time = time.time()
        spec_args = kwargs.get('spec_args', {})
        try:
            self._view = self._create_view_spec(**spec_args)
            self._data = self._create_data_object(**spec_args)
        except ResolverError:
            log(OPERATIONAL, 'Chyba z resolveru', format_traceback())
            throw('form-init-error')
        log(EVENT, 'Specifikace načteny za %.3fs' % (time.time() - start_time)) 
        self._init_attributes(**kwargs)
        self._result = None
        self._add_menus()
        start_time = time.time()
        self._create_form()
        log(EVENT, 'Formulář sestaven za %.3fs' % (time.time() - start_time))
        wx_callback(wx.EVT_CLOSE, self._parent, self._on_parent_close)

    def _init_attributes(self, spec_args={}):
        """Zpracuj klíčové argumenty konstruktoru a inicializuj atributy.
        
        Argumenty:
        
          kwargs -- klíčové argumenty konstruktoru (viz dokumentace metody
            '__init__()').

        
        Tato metoda je volána po základní inicializaci instance (především
        načtení specifikace a inicializaci datového objektu.  Metody
        vytvářející konkrétní prvky uživatelského rozhraní formuláře (například
        '_create_form()'), jsou však volány až poté.  Zde by měly být především
        zpracovány všechny klíčové argumenty konstruktoru (viz dokumentace
        metody '__init__()' a inicializovány atributy instance.

        """
        pass

    def _create_view_spec(self, **kwargs):
        spec = self._resolver.get(self._name, 'view_spec', **kwargs)
        assert isinstance(spec, ViewSpec)
        return spec        

    def _create_data_object(self, **kwargs):
        name = self._name
        data_spec = self._resolver.get(name, 'data_spec', **kwargs)
        import config
        if __debug__ and config.server:
            import pytis.remote
        else:    
            import pytis.data    
        assert isinstance(data_spec, pytis.data.DataFactory)
        assert isinstance(data_spec, pytis.data.DataFactory) or \
               isinstance(data_spec, pytis.remote.RemoteDataFactory)
        op = lambda : data_spec.create(dbconnection_spec=config.dbconnection)
        success, data_object = db_operation(op)
        if not success:
            throw('form-init-error')
        return data_object
    
    def _add_menus(self):
        for m in self._menus():
            add_menu(m, self)

    def _menus(self):
        return (Menu(_("Příkazy"),
                     (MItem(_("Přepnout na předchozí okno"),
                            command=Application.COMMAND_PREV_FORM),
                      MItem(_("Přepnout na následující okno"),
                            command=Application.COMMAND_NEXT_FORM),
                      MItem(_("Zavřít aktuální okno"),
                            command=Application.COMMAND_LEAVE_FORM),
                      MSeparator(),
                      MItem(_("Uložit"),
                            command=ListForm.COMMAND_LINE_COMMIT),
                      MItem(_("Zrušit změny"),
                            command=ListForm.COMMAND_LINE_ROLLBACK),
                      MSeparator(),
                      MItem(_("Skok na záznam"),
                            command=LookupForm.COMMAND_JUMP),
                      MItem(_("Hledat"),
                            command=LookupForm.COMMAND_SEARCH),
                      MItem(_("Hledat další"),
                            command=LookupForm.COMMAND_SEARCH_NEXT),
                      MItem(_("Hledat předchozí"),
                            command=LookupForm.COMMAND_SEARCH_PREVIOUS),
                      MItem(_("Inkrementální hledání"),
                            command=ListForm.COMMAND_INCREMENTAL_SEARCH),
                      MItem(_("Inkrementální hledání - část řetězce"),
                            command=ListForm.COMMAND_FULL_INCREMENTAL_SEARCH),
                      MSeparator(),
                      MItem(_("Třídění"),
                            command=LookupForm.COMMAND_SORT_COLUMN),
                      MItem(_("Filtrovat"),
                            command=LookupForm.COMMAND_FILTER),
                      MSeparator(),
                      # TODO: Všechny INSERT příkazy sloučit v jeden s args.
                      MItem(_("Nový záznam"),
                            command=BrowseForm.COMMAND_NEW_RECORD),
                      MItem(_("Nový záznam - kopie"),
                            command=BrowseForm.COMMAND_NEW_RECORD_COPY),
                      MItem(_("Editovat záznam"),
                            command=BrowseForm.COMMAND_RECORD_EDIT),
                      MItem(_("Vložit řádku nad"),
                            command=ListForm.COMMAND_NEW_LINE_BEFORE),
                      MItem(_("Vložit řádku pod"),
                            command=ListForm.COMMAND_NEW_LINE_AFTER),
                      MItem(_("Kopírovat řádku nad"),
                            command=ListForm.COMMAND_NEW_LINE_BEFORE_COPY),
                      MItem(_("Kopírovat řádku pod"),
                            command=ListForm.COMMAND_NEW_LINE_AFTER_COPY),
                      MItem(_("Editace buňky"),
                            command=ListForm.COMMAND_EDIT),
                      MItem(_("Smazat záznam"),
                            command=ListForm.COMMAND_LINE_DELETE),
                      MSeparator(),
                      MItem(_("Export do textového souboru"),
                            command=ListForm.COMMAND_EXPORT_CSV),
                      MSeparator(),
                      MItem(_("Zobrazit náhled na záznam"),
                            command=ListForm.COMMAND_ACTIVATE),
                      MItem(_("Zobrazit náhled na záznam v duálním formuláři"),
                            command=ListForm.COMMAND_ACTIVATE_ALTERNATE),
                      ),
                     activation=Form.ACT_FORM),
                Menu(_("Tisk"), (Form.print_menu,),
                     activation=Form.ACT_FORM),
                )
        
    def _create_form(self):
        # Build the form from parts
        sizer = wx.BoxSizer(wx.VERTICAL)
        self._create_form_parts(sizer)
        self.SetAutoLayout(True)
        self.SetSizer(sizer)
        self.Layout()
        sizer.Fit(self) # Set the size of window `self' to size of the sizer.

    def _create_form_parts(self, sizer):
        pass
    
    def _on_parent_close(self, event):
        """Handler události uzavření rodičovského okna formuláře.

        Tato metoda by měla být předefinována, pokud chce daný typ formuláře
        reagovat na uzavření rodičovského okna. Typické využití je pro popup
        formuláře.

        Pokud odvozená třída předefinuje tuto metodu a ta za určitých okolností
        nezavolá 'event.Skip()', nebude zpracování události dokončeno a
        rodičovské okno tedy nebude uzavřeno.
        """
        if __debug__: log(DEBUG, "Voláno Form._on_parent_close()")
        event.Skip()
        return False

    def __str__(self):
        return '<%s for "%s">' % (self.__class__.__name__, self._name)

    def __repr__(self):
        return str(self)
    
    # Veřejné metody
    
    def name(self):
        """Vrať název specifikace formuláře."""
        return self._name

    def on_command(self, command, **kwargs):
        """Zpracuj 'command'.

        Argumenty:

          command -- instance třídy 'Command'

        Metoda musí příkaz buď sama ošetřit, nebo jek vypropagovat do vnitřního
        prvku formuláře, pokud takový je a má metodu stejného názvu jako tato.

        Vrací: Pravdu, právě když metoda nebo jí volaná metoda příkaz
        zpracovala.
        
        V této třídě metoda nedělá nic a vrací False.
        
        """
        return False

    def descr(self):
        """Vrať textový popis typu formuláře jako řetězec."""
        if self._DESCR is not None:
            return self._DESCR
        else:
            return self.__class__.__name__
        
    def title(self):
        """Vrať titulek ze specifikace formuláře jako řetězec."""
        return self._view.title()

    def guardian(self):
        """Vrať guardian zadané v konstruktoru (nebo parent)."""
        return self._guardian

    def check_permission(self, perm):
        """Vrať pravdu, pokud má uživatel daná práva k datovému objektu.

        Argumentem je konstanta  třídy 'pytis.data.Permission'.

        """
        return self._data.accessible(None, perm)
    
    def set_status(self, field, message):
        """Zobraz zprávu `message' v poli `id' stavové řádky formuláře.

        Má-li formulář stavovou řádku a v ní pole `id' zobraz v něm danou
        zprávu a vrať pravdu.  V opačném případě vrať nepravdu.

        """
        return False

    def show_popup_menu(self):
        """Zobraz kontextové menu právě aktivního prvku, pokud to umožňuje. """
        pass
        
    def close(self):
        for id in self._STATUS_FIELDS:
            set_status(id, '')
        return super_(Form).close(self)
    
    def save(self):
        self._saved_state = map(lambda id: (id, get_status(id)),
                                self._STATUS_FIELDS)

    def restore(self):
        for id, message in self._saved_state:
            set_status(id, message, log_=False)

    def print_menu(self):
        """Vrať tuple položek tiskového menu."""
        name = self._name
        try:
            spec_paths = self._resolver.get(name, 'print_spec')
        except ResolverSpecError:
            spec_paths = None
        if not spec_paths:
            spec_paths = ((_("Implicitní"), os.path.join('output', name)),)
        return [MItem(p[0], command=pytis.form.Form.COMMAND_PRINT,
                      args={'print_spec_path': p[1]})
                for p in spec_paths]


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
    
    def refresh(self, when=None):
        """Aktualizuj data formuláře z datového zdroje.

        Překresli data ve formuláři v okamžiku daném argumentem 'when'.

        Argumenty:

          when -- určuje, zda a kdy má být aktualizace provedena, musí to být
            jedna z 'DOIT_*' konstant třídy.  Implicitní hodnota je
            'DOIT_AFTEREDIT', je-li 'reset' 'None', 'DOIT_IMMEDIATELY' jinak.

        Vrací: Pravdu, právě když byla aktualizace provedena.

        V této třídě metoda nedělá nic, musí být v potomkovi předefinována.
        
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
            style = wx.DIALOG_MODAL|wx.DEFAULT_DIALOG_STYLE
            frame = wx.Dialog(parent, style=style)
            self._popup_frame_ = frame
        return frame    

    def _leave_form(self):
        self._popup_frame_.Close() # tím se autom. zavolá _on_parent_close()

    def _on_parent_close(self, event):
        if hasattr(self, 'exit_check') and not self.exit_check():
            event.Veto()
            return True
        event.Skip()
        self._parent.EndModal(0)
        return False

    def run(self):
        """Zobraz formulář jako modální dialog."""
        unlock_callbacks()
        self._parent.SetTitle(self.title())
        self._parent.SetClientSize(self.GetSize())
        self._parent.ShowModal()
        return self._result


class TitledForm:
    """Přimíchávací třída pro formuláře s titulkem.
    
    Lze využít buďto pouze metodu '_create_caption()', která vytváří samotný
    text titulku, nebo metodu '_create_title_bar()', která přidává 3d panel.

    """    
    _TITLE_BORDER_WIDTH = 3
    
    def _create_caption(self, parent, text, size=None):
        # Create the title text as 'wxStaticText' instance.
        caption = wx.StaticText(parent, -1, text,
                                style=wx.ALIGN_CENTER)
        if size is None: 
            size = caption.GetFont().GetPointSize()
        font = wx.Font(size, wx.DEFAULT, wx.NORMAL, wx.BOLD,
                       encoding=wx.FONTENCODING_DEFAULT)
        caption.SetFont(font)
        width, height, d, e = self.GetFullTextExtent(text, font)
        caption.SetSize(wx.Size(width, height))
        return caption

    def _create_title_bar(self, text, size=None, description=None):
        """Vytvoř 3d panel s nadpisem formuláře."""
        panel = wx.Panel(self, -1, style=wx.RAISED_BORDER)
        caption = self._create_caption(panel, text, size=size)
        box = wx.BoxSizer()
        box.Add(caption, 1, wx.EXPAND|wx.ALL, self._TITLE_BORDER_WIDTH)
        panel.SetSizer(box)
        panel.SetAutoLayout(True)        
        box.Fit(panel)
        if description:
            panel.SetToolTipString(description)
        return panel


class RecordForm(Form):
    """Formulář schopný nějakým způsobem zobrazit aktuální záznam."""

    CALL_SELECTION = 'CALL_SELECTION'
    """Konstanta callbacku změny záznamu."""

    def _init_attributes(self, key=None, prefill=None, **kwargs):
        """Zpracuj klíčové argumenty konstruktoru a inicializuj atributy.

        Argumenty:
        
          key -- sekvence klíčových sloupců aktivovaného řádku jako instance
            třídy 'pytis.data.types_.Value'.  Není-li 'None', formulář by se měl
            naplnit hodnotami získanými z datového objektu pro řádek dat
            s daným klíčem.
          prefill -- slovník řetězcových (uživatelských) hodnot, které mají být
            předvyplněny při inicializaci formuláře
          kwargs -- argumenty předané volání předka

        """
        super_(RecordForm)._init_attributes(self, **kwargs)
        assert prefill is None or is_dictionary(prefill)
        self._prefill = prefill
        self._key = key
        self._row = None

    def _set_row(self, row):
        # Naplň formulář daty z daného *datového* řádku
        prow = PresentedRow(self._view.fields(), self._data, row,
                            prefill=self._prefill, new=(not self._key),
                            change_callback=self._on_field_change,
                        editability_change_callback=self._on_editability_change)
        self.set_row(prow)

    def _on_field_change(self, field_id, value=None):
        # Signalizace změny hodnoty políčka z _row
        pass

    def _on_editability_change(self, field_id, editable):
        # Callback změny editovatelnosti políčka
        pass
        
    # Veřejné metody

    def prefill(self):
        """Vrať data pro předvyplnění nového záznamu."""
        return self._prefill
    
    def set_prefill(self, data):
        """Nastav data pro předvyplnění nového záznamu.

        List si může zapamatovat hodnoty, které mají být automaticky použity
        pro předvyplnění nového záznamu při operacích vložení řádku nad tímto
        listem.  Pro argument 'data' zde platí stejné podmínky, jako pro
        argument 'prefill' konstruktoru třídy 'PresentedRow'.

        """
        self._prefill = data
    
    def set_row(self, row):
        """Naplň aktuální editaci záznamu formuláře daty z 'row'.

        Argumenty:

          row -- instance 'PresentedRow'

        """
        self._row = row
        self._run_callback(self.CALL_SELECTION, (row,))
        

class LookupForm(RecordForm):
    """Formulář s vyhledáváním a tříděním."""
    
    SORTING_CYCLE_DIRECTION = 'SORTING_CYCLE_DIRECTION'
    """Konstanta pro argument direction metody '_on_sort_column()'."""
    SORTING_NONE = 'SORTING_NONE'
    """Konstanta pro argument direction metody '_on_sort_column()'."""
    SORTING_ASCENDENT = 'SORTING_ASCENDENT'
    """Konstanta pro argument direction metody '_on_sort_column()'."""
    SORTING_DESCENDANT = 'SORTING_DESCENDANT'
    """Konstanta pro argument direction metody '_on_sort_column()'."""

    
    def _init_attributes(self, sorting=None, grouping=None, condition=None,
                         indicate_filter=False, **kwargs):
        """Zpracuj klíčové argumenty konstruktoru a inicializuj atributy.

        Argumenty:

          sorting -- specifikace počátečního třídění formuláře, viz argument
            'sort' metody 'pytis.data.Data.select()'
          grouping -- ???
          condition -- podmínka výběru dat, viz argument 'condition' metody
            'pytis.data.Data.select()'
          indicate_filter -- ???
          kwargs -- argumenty předané konstruktoru předka
        
        """
        super_(LookupForm)._init_attributes(self, **kwargs)
        self._lf_sorting = sorting or self._default_sorting()
        self._lf_grouping = grouping or self._default_grouping()
        self._lf_condition = condition
        self._lf_indicate_filter = indicate_filter
        self._lf_initial_sorting = self._lf_sorting
        self._lf_initial_grouping = self._lf_grouping
        self._lf_initial_condition = self._lf_condition
        self._lf_search_dialog = None
        self._lf_filter_dialog = None
        self._lf_select_count = None
        self._lf_filter = None

    def _default_sorting(self):
        return ()

    def _default_grouping(self):
        return None

    def _init_select(self):
        data = self._data
        if self._lf_condition and self._lf_filter:
            condition = pytis.data.AND(self._lf_condition, self._lf_filter)
        else:
            condition = self._lf_condition or self._lf_filter
        if self._lf_initial_condition:
            condition = pytis.data.AND(condition, self._lf_initial_condition)
        sorting = self._lf_translated_sorting()
        op = lambda : data.select(condition=condition, sort=sorting,
                                  reuse=False)
        success, self._lf_select_count = db_operation(op)
        if not success:
            log(EVENT, 'Selhání databázové operace')
            throw('form-init-error')
        return self._lf_select_count

    def _lf_translated_sorting(self):
        def trans(x):
            if x[1] == self.SORTING_ASCENDENT:
                t = pytis.data.ASCENDENT
            elif x[1] == self.SORTING_DESCENDANT:
                t = pytis.data.DESCENDANT
            else:
                raise ProgramError('Invalid sorting spec', x[1])
            return x[0], t
        return tuple(map(trans, self._lf_sorting))

    def _lf_sfs_columns(self):
        columns = map(lambda id: self._view.field(id), self._view.columns())
        return sfs_columns(columns, self._data,
                           labelfunc=FieldSpec.column_label,
                           widthfunc=FieldSpec.column_width)
    
    def _lf_sf_dialog(self, attr, class_):
        dialog = getattr(self, attr)
        if not dialog:
            columns = self._lf_sfs_columns()
            args = (self._parent, columns)
            if issubclass(class_, FilterDialog):
                args = args + (self._data, self._lf_initial_condition)
            dialog = class_(*args)
            setattr(self, attr, dialog)
        return dialog
        
    def _find_row(self, key, any_row=False):
        if key is None:
            return None
        def find_row(key):
            data = self._data
            result = self._data.row(key)
            if result is None and any_row:
                if self._lf_select_count is None:
                    self._init_select()
                else:
                    data.rewind()
                result = data.fetchone()
            return result
        success, row = db_operation(lambda : find_row(key))
        if success and row:
            return row
        else:
            run_dialog(Error, _("Záznam nenalezen"))
            return None
        
    def _search(self, condition, direction, row_number=None,
                report_failure=True):
        self._search_adjust_data_position(row_number)
        data = self._data
        skip = data.search(condition, direction=direction)
        if skip == 0:
            log(EVENT, 'Záznam nenalezen')
            if report_failure:
                run_dialog(Warning, _("Záznam nenalezen"))
            result = None
        else:
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
        self._set_row(row)

    def _on_jump(self):
        if self._lf_select_count > 0:
            prompt = u"Záznam číslo (1-%s): " % (self._lf_select_count)
            mask = "#" * len(str(self._lf_select_count))
            returned = pytis.form.run_dialog(pytis.form.InputDialog,
                                message=u"Skok na záznam",
                                prompt=prompt,
                                mask=mask,
                                formatcodes='_,Fr'
                                )
            try:
                row = int(str(returned.strip()))
                if row > 0 and row <= self._lf_select_count:
                    return row
            except:
                return None
            return None
        
    def _on_search(self, show_dialog=True, direction=pytis.data.FORWARD):
        sf_dialog = self._lf_sf_dialog('_lf_search_dialog', SearchDialog)
        if show_dialog:
            self._block_refresh = True  # TODO: quick&dirty, see ListForm
            try:
                condition, direction = run_dialog(sf_dialog, self._row)
            finally:
                self._block_refresh = False
        else:
            condition = sf_dialog.condition()
        if condition is not None:
            self._search(condition, direction)

    def _filter(self, condition):
        self._init_select()
        self._set_row(self._find_row(self._key, any_row=True))

    def _on_filter(self, row=None, col=None, show_dialog=True):
        sf_dialog = self._lf_sf_dialog('_lf_filter_dialog', FilterDialog)
        if show_dialog:
            if row is None:
                row = self._row
            perform, filter = run_dialog(sf_dialog, row, col=col)
        else:
            perform, filter = (True, sf_dialog.condition())
        if perform and filter != self._lf_filter:
            self._lf_filter = filter
            self._filter(filter)

    def _on_sort_column(self, col=None, direction=None, primary=False):
        """Změň třídění.

        Argumenty:

          col -- id sloupce, podle kterého má být seznam setříděn, nebo
            'None' pro globální změny (například vypnutí veškerého třídění)
          direction -- směr třídění (sestupně/vzestupně/vůbec/cyklicky).  Pokud
            je hodnotou konstanta 'LookupForm.SORTING_CYCLE_DIRECTION', bude
            třídění cyklicky přepnuto na další z variant
            (sestupně/vzestupně/vůbec).  Hodnota daná konstantou
            'LookupForm.SORTING_NONE' značí explicitní požadavek na zrušení
            třídění.  Jinak je očekávána jedna z konstant
            'LookupForm.SORTING_ASCENDENT' (pro sestupné třídění), nebo
            'LookupForm.SORTING_DESCENDANT' (pro vzestupné třídění).
          primary -- právě když je pravdivé, bude daný sloupec zvolen jako
            primární a *jediný* třídící sloupec.  V opačném případě bude pouze
            přidán na konec stávajícího seznamu třídících sloupců.
        
        Při nejednoznačné kombinaci argumentů 'col' a 'direction' je
        automaticky vyvolán dialog pro výběr třídících kritérií.
        
        """
        # TODO: Toto celé je bastl, nutno časem pročistit.
        sorting = xlist(self._lf_sorting)
        if direction is None or \
               col is None and direction != self.SORTING_NONE:
            columns = self._lf_sfs_columns()
            if col is None and self._lf_sorting:
                col, __dir = self._lf_sorting[0]                
            d = SortingDialog(self._parent, columns, self._lf_sorting,
                              col=col, direction=direction)
            sorting = run_dialog(d)
            if sorting is None:
                return None
            elif sorting is ():
                sorting = self._lf_initial_sorting
        else:
            if col is not None:
                if not self._data.find_column(col):
                    message(_("Podle tohoto sloupce nelze třídit"),
                            beep_=True)
                    return
            pos = position(col, sorting, key=lambda x: x[0])
            if direction == self.SORTING_CYCLE_DIRECTION:
                if pos is not None:
                    current_direction = sorting[pos][1]
                    if current_direction == self.SORTING_ASCENDENT:
                        direction = self.SORTING_DESCENDANT
                    elif current_direction == self.SORTING_DESCENDANT:
                        direction = self.SORTING_NONE
                    else:    
                        direction = self.SORTING_ASCENDENT
                else:    
                    direction = self.SORTING_ASCENDENT
            if direction == self.SORTING_NONE:
                if pos is not None:
                    del sorting[pos]
                elif col is None:
                    sorting = ()
            else:
                assert direction in (self.SORTING_ASCENDENT,
                                     self.SORTING_DESCENDANT)
                new_col_spec = (col, direction)
                if primary:
                    sorting = (new_col_spec,)
                elif pos is None:
                    sorting.append(new_col_spec)
                else:
                    sorting[pos] = new_col_spec
            sorting = tuple(sorting)
        if sorting is not None and sorting != self._lf_sorting:
            self._lf_sorting = sorting
            self._set_row(self._find_row(self._key, any_row=True))
        return sorting
    
    def can_sort_column(self, col=None, direction=None, primary=False):
        sorting = xtuple(self._lf_sorting)
        sortcols = [c for c,d in sorting]
        if direction == self.SORTING_NONE:
            return sorting and (col is None or col in sortcols)
        elif direction is not None and col is not None:
            if primary:
                return not sorting or col != sorting[0][0]
            else:
                return sorting and col not in sortcols
        else:
            return True
        
    # wx metody

    def Close(self):
        super_(LookupForm).Close(self)
        if self._lf_search_dialog:
            self._lf_search_dialog = None
        if self._lf_filter_dialog:
            self._lf_filter_dialog = None
    
    # Veřejné metody

    def condition(self):
        """Vrať specifikaci aktuální podmínky výběru dat.

        Podmínka je vrácena v podobě požadované argumentem 'condition'
        metody 'pytis.data.Data.select()'.

        """
        return self._lf_condition
    
    def sorting(self):
        """Vrať specifikaci aktuálního třídění seznamu.

        Podmínka je vrácena v podobě požadované argumentem 'sort'
        metody 'pytis.data.Data.select()'.

        """
        return self._lf_sorting

    def is_searching(self):
        """Vrať pravdu, je-li definována vyhledávací podmínka."""
        sd = self._lf_search_dialog
        return bool(sd and sd._condition)
    
    def on_command(self, command, **kwargs):
        if command == LookupForm.COMMAND_JUMP:
            self._on_jump()
            return True
        if command == LookupForm.COMMAND_SEARCH:
            self._on_search()
            return True
        elif command == LookupForm.COMMAND_SEARCH_NEXT:
            self._on_search(show_dialog=False, direction = pytis.data.FORWARD)
            return True
        elif command == LookupForm.COMMAND_SEARCH_PREVIOUS:
            self._on_search(show_dialog=False, direction = pytis.data.BACKWARD)
            return True
        elif command == LookupForm.COMMAND_FILTER:
            self._on_filter()
            return True
        elif command == LookupForm.COMMAND_SORT_COLUMN:
            self._on_sort_column()
            return True            
        else:
            return super_(LookupForm).on_command(self, command, **kwargs)

    def can_sort(cls, appl, cmd, args):
        f = appl.current_form()
        return f and isinstance(f, LookupForm) and f.can_sort_column(**args)
    can_sort = classmethod(can_sort)
    
        

### Editační formulář


class EditForm(LookupForm, TitledForm):
    """Formulář pro editaci všech vlastností jednoho záznamu.

    Formulář je vytvořen poskládáním jednotlivých vstupních políček daných
    specifikací do mřížky.  Pole mohou být různě seskupována a jejich rozložení
    je určeno specifikační třídou 'LayoutSpec' resp. 'GroupSpec'.

    Každé vstupní pole je reprezentováno objektem třídy 'InputField'.  To se
    stará o interakci s uživatelem, validaci vstupních dat apod.

    Formulář může sloužit jak k editaci stávajícího řádku dat, tak
    i k vytvoření řádku nového (viz argumenty konstruktoru 'key' a 'new').

    """
    ACT_EDITFORM = 'ACT_EDITFORM'
    """Aktivační konstanta formuláře."""
    
    ACTIVATIONS = Window.ACTIVATIONS + [ACT_EDITFORM]
    """Seznam aktivačních kategorií pro tuto třídu."""

    def __init__(self, *args, **kwargs):
        super_(EditForm).__init__(self, *args, **kwargs)
        self._size = self.GetSize() # Remember the original size.
        if self._key: # editace stávajícího záznamu nebo kopie
            self._set_row(self._find_row(self._key))
        else: # nový prázdný záznam
            self._set_row(None)
        if isinstance(self._parent, wx.Dialog):
            wx_callback(wx.EVT_INIT_DIALOG, self._parent, self.init)
        else:
            self.init()

    def _init_attributes(self, focus_field=None, editable=True, new=False,
                         **kwargs):
        """Zpracuj klíčové argumenty konstruktoru a inicializuj atributy.

        Argumenty:

          focus_field -- id políčka, které má být vybráno jako aktivní pro
            uživatelský vstup, případně funkce jednoho argumentu, kterým je
            aktuální PresentedRow, která vrací id políčka pro uživatelský
            vstup.
          editable -- právě když je pravdivé, lze formulář editovat
          new -- příznak, zda se jedná o nový záznam nebo editaci stávajícího;
            je-li 'key' 'None', považuje se záznam za nový vždy, bez ohledu na
            hodnotu 'new'
          kwargs -- argumenty předané konstruktoru prvního předka

        """
        super_(EditForm)._init_attributes(self, **kwargs)
        self._focus_field = focus_field or self._view.focus_field()
        self._editable = editable
        # TODO: zde bychom neměli sahat do argumentů předků ('key')...
        self._new = (not kwargs.get('key')) or new
        # Other attributes
        self._fields = []

    def init(self, event=None):
        """Inicalizuj dialog nastavením hodnot políček."""
        for f in self._fields:
            if self._editable and self._row.editable(f.id()):
                f.enable()
            else:
                f.disable(change_appearance=self._editable)
        if self._focus_field:
            if callable(self._focus_field):
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

    def _create_form(self):
        for id in self._view.layout().order():
            spec = self._view.field(id)
            if id in map(lambda c: c.id(), self._data.columns()):
                if self._new:
                    permission = pytis.data.Permission.INSERT
                else:
                    permission = pytis.data.Permission.UPDATE
                acc = self._data.accessible(id, permission)
            else:
                acc = True
            f = InputField.create(self, spec, self._data, guardian=self,
                                  accessible=acc)
            f.set_callback(InputField.CALL_SKIP_NAVIGATION, self._navigate)
            f.set_callback(InputField.CALL_FIELD_CHANGE, self._on_field_edit)
            f.set_callback(InputField.CALL_COMMIT_FIELD, self._navigate)
            self._fields.append(f)
        super_(EditForm)._create_form(self)

    def _field(self, id):
        f = find(id, self._fields, key=lambda f: f.id())
        assert f is not None, (_("Unknown field:"), id)
        return f
        
    def _create_form_parts(self, sizer):
        # Create all parts and add them to top-level sizer.
        layout = self._view.layout()
        # Create the parts
        caption = self._create_caption(self, self.title(), size=18)
        group = self._create_group(layout.group())
        # Add parts to the sizer.
        sizer.Add(caption, 0, wx.ALIGN_CENTER|wx.ALL, 8)
        sizer.Add(group,   0, wx.ALIGN_CENTER|wx.ALL, 8)

    def _create_button(self, item):
        b = wx.Button(self, -1, item.label())
        b.Enable(item.active_in_popup_form() \
                 or not isinstance(self, PopupForm))
        if item.width() is not None:
            width = dlg2px(b, 4*item.width())
            height = b.GetSize().GetHeight()
            b.SetMinSize((width, height))
        if item.tooltip() is not None:
            b.SetToolTipString(item.tooltip())
        def create_handler(handler):
            def _handler(event):
                refresh = handler(self._row)
                busy_cursor(False)
                self.set_row(self._row)
            return _handler
        wx_callback(wx.EVT_BUTTON, self, b.GetId(),
                    create_handler(item.handler()))
        return b
        
    def _create_group(self, group):
        """Vytvoř skupinu vstupních políček podle specifikace.

        Argumenty:

          group -- instance 'GroupSpec', která má být zpracována.

        Každou posloupnost za sebou následujících políček seskupí pod sebe
        a pro každou vnořenou skupinu políček zavolá sebe sama rekurzivně.
        Výsledek potom poskládá do instance 'wx.BoxSizer', kterou vytvoří.

        Specifikace skupiny ovlivňuje způsob seskupení:
        horizontální/vertikální, mezery mezi políčky, skupinami
        atd. Viz. dokuewntace třídy 'GroupSpec'

        Vrací: 'wx.BoxSizer' naplněný políčky a vnořenými skupinami.

        """
        orientation = orientation2wx(group.orientation())
        if group.label() is not None:
            box = wx.StaticBox(self, -1, group.label())
            sizer = wx.StaticBoxSizer(box, orientation)
        else:
            sizer = wx.BoxSizer(orientation)
        # každý souvislý sled políček ukládám do pole a teprve nakonec je
        # poskládám metodou self._pack_fields() a vložím do sizeru této
        # skupiny
        pack = []
        space = dlg2px(self, group.space())
        gap = dlg2px(self, group.gap())
        border = dlg2px(self, group.border())
        border_style = border_style2wx(group.border_style())
        for item in group.items():
            if (is_anystring(item) and
                not self._view.field(item).compact() or
                isinstance(item, Button)):
                # Field of this id will become a part of current pack
                pack.append(item)
                continue
            if len(pack) != 0:
                # přidej poslední sled políček (pokud nějaký byl)
                sizer.Add(self._pack_fields(pack, space, gap),
                          0, wx.ALIGN_TOP|border_style, border)
                pack = []
            if isinstance(item, GroupSpec):
                g = self._create_group(item)
                sizer.Add(g, 0, wx.ALIGN_TOP|border_style, border)
            else:
                # This is a compact field (not a part of the pack)
                field = self._field(item)
                w = field.widget()
                if w is not None:
                    s = wx.BoxSizer(wx.VERTICAL)
                    label = field.label()
                    s.Add(label, 0, wx.ALIGN_LEFT)
                    s.Add(w)
                    sizer.Add(s, 0, wx.ALIGN_TOP|border_style, border)
        if len(pack) != 0:
            # přidej zbylý sled políček (pokud nějaký byl)
            sizer.Add(self._pack_fields(pack, space, gap),
                      0, wx.ALIGN_TOP|border_style, border)
        # pokud má skupina orámování, přidáme ji ještě do sizeru s horním
        # odsazením, jinak je horní odsazení příliš malé.
        if group.label() is not None:
            s = wx.BoxSizer(orientation)
            s.Add(sizer, 0, wx.TOP, 3)
            sizer = s
        return sizer

    def _pack_fields(self, items, space, gap):
        """Sestav skupinu pod sebou umístěných políček/tlačítek do gridu.

        Argumenty:

          items -- sekvence identifikátorů políček nebo instancí Button.
          space -- mezera mezi ovládacím prvkem a labelem políčka v dlg units;
            integer
          gap -- mezera mezi jednotlivými políčky v dlg units; integer

        Pro každý prvek skupiny vytvoří tlačítko nebo políčko 'inputfield.InputField'
        a přidá jeho label a widget do vytvořené instance
        'wx.FlexGridSizer'.

        Vrací: instanci 'wx.FlexGridSizer' naplněnou políčky a tlačítky.

        """
        grid = wx.FlexGridSizer(len(items), 2,
                                  dlg2px(self,gap), dlg2px(self,space))
        for item in items:
            if isinstance(item, Button):
                button = self._create_button(item)
                style = wx.ALIGN_RIGHT|wx.ALIGN_CENTER_VERTICAL
                label = wx.StaticText(self, -1, "",
                                      style=wx.ALIGN_RIGHT)
                grid.Add(label, 0, style, 2)
                grid.Add(button)                
            else:    
                field = self._field(item)
                if field.height() > 1:
                    style = wx.ALIGN_RIGHT|wx.ALIGN_TOP|wx.TOP
                else:
                    style = wx.ALIGN_RIGHT|wx.ALIGN_CENTER_VERTICAL
                if not isinstance(field, HiddenField):
                    grid.Add(field.label(), 0, style, 2)
                    grid.Add(field.widget())
        return grid

    def _lock(self):
        key = self._key
        if not key:
            return True
        success, locked = db_operation(lambda : self._data.lock_row(key),
                                       quiet=True)
        if success and locked != None:
            log(EVENT, 'Záznam je zamčen', locked)
            run_dialog(Message, _("Záznam je zamčen: %s") % locked)
            return False
        else:
            return True

    def _unlock(self):
        if self._data.locked_row():
            db_operation(lambda : self._data.unlock_row(), quiet=True)

    def _validate(self):
        """Zvaliduj postupně všechna políčka.
        
        Vrací: None v případě chyby, jinak instanci 'pytis.data.Row', kterou je
        možno použít pro vložení/update datového zdroje.

        """
        for f in self._fields:
            if self._new or f.is_modified():
                value, error = f.validate()
                if error:
                    log(EVENT, 'Validace selhala:', (f.id(), f.get_value()))
                    f.set_focus()
                    return None
                #self._signal_update() # TODO: Tohle tu bylo kdoví proč...
        error = None
        check = self._view.check()
        if check is not None:
            error = check(self._row)
        if error is None:
            error = self._row.check()
        if error is not None:
            if is_sequence(error):
                field_id, msg = error
                message(msg)
            else:
                field_id = error
                log(EVENT, 'Kontrola integrity selhala:', field_id)
                # TODO: Tím bychom přepsali zprávu nastavenou uvnitř 'check()'.
                # Pokud ale žádná zpráva nebyla nastavena, uživatel netuší...
                #message(_("Kontrola integrity selhala!"))
            field = self._field(field_id)
            field.set_focus()
            return None
        # Data sestavíme až po check, protože tam mohou být měněny honoty.
        rdata = [(f.id(), self._row[f.id()]) for f in self._fields]
        return pytis.data.Row(rdata)

    def _edit_insert(self):
        log(ACTION, 'Vložení řádku')
        row = self._validate()
        if not row:
            return False
        success, result = db_operation(lambda : self._data.insert(row))
        if success and result[1]:
            self._row.set_row(result[0], reset=True)
            self.set_row(self._row)
        else:
            return False
        self._signal_update()
        log(ACTION, 'Řádek vložen')
        self._result = self._row
        return True

    def _edit_update(self):
        log(ACTION, 'Update řádku')
        key = self._key        
        if key == None:
            return False
        row = self._validate()
        if not row:
            return False
        success, result = db_operation(lambda : self._data.update(key, row))
        if success and result[1]:
            new_row = result[0]
            if new_row is not None:
                self._row.set_row(new_row, reset=True)
                self.set_row(self._row)
            else:
                # TODO: Lze provést něco chytřejšího?
                pass
        else:
            run_dialog(Error, _("Uložení řádku se nezdařilo"))
            return False
        # Políčka se tímto trikem budou tvářit nezmněněná s nynější hodnotou.
        for field in self._fields:
            field.init(field.get_value())
        self._signal_update()
        log(ACTION, 'Řádek updatován')
        self._result = self._row
        return True

    def _edit_delete(self):
        key = self._key
        if key == None:
            return False
        if not delete_record_question():
            return False
        success, result = db_operation(lambda : self._data.delete(key))
        if not success:
            return False
        self._signal_update()
        log(ACTION, 'Řádek smazán')
        return True

    def _signal_update(self):
        f = current_form()
        if isinstance(f, Refreshable):
            f.refresh()

    def _commit_form(self, close=True):
        if self._new:
            result = self._edit_insert()
        else:
            result = self._edit_update()
        if result:
            cleanup = self._view.cleanup()
            if cleanup is not None:
                cleanup(self._row)
            if close:    
                # tím je automaticky zavoláno _on_parent_close()
                # TODO: to nebude fungovat v embeded verzi!!!!!!!!!!
                self._parent.Close()
        return result

    def _menus(self):
        return (Menu(_("Příkazy"),
                     (MItem(_("Přepnout na předchozí okno"),
                            command=Application.COMMAND_PREV_FORM),
                      MItem(_("Přepnout na následující okno"),
                            command=Application.COMMAND_NEXT_FORM),
                      MItem(_("Zavřít aktuální okno"),
                            command=Application.COMMAND_LEAVE_FORM),
                      MSeparator(),
                      MItem(_("Hledat"),
                            command=LookupForm.COMMAND_SEARCH),
                      MItem(_("Hledat další"),
                            command=LookupForm.COMMAND_SEARCH_NEXT),
                      MItem(_("Hledat předchozí"),
                            command=LookupForm.COMMAND_SEARCH_PREVIOUS),
                      MItem(_("Třídění"),
                            command=LookupForm.COMMAND_SORT_COLUMN),
                      MItem(_("Filtrovat"),
                            command=LookupForm.COMMAND_FILTER),
                      MSeparator(),
                      MItem(_("Nový záznam"),
                            command=EditForm.COMMAND_RECORD_INSERT),
                      MItem(_("Editovat záznam"),
                            command=EditForm.COMMAND_RECORD_UPDATE),
                      MItem(_("Smazat záznam"),
                            command=EditForm.COMMAND_RECORD_DELETE),
                      ),
                     activation=EditForm.ACT_EDITFORM),
                Menu(_("Tisk"), (EditForm.print_menu,),
                     activation=EditForm.ACT_EDITFORM),
                )

    def set_row(self, row):
        """Naplň formulář daty z daného řádku (instance 'PresentedRow')."""
        for f in self._fields:
            f.init(row[f.id()].export())
        super_(EditForm).set_row(self, row)

    def title(self):
        """Vrať název formuláře jako řetězec."""        
        return self._view.layout().caption()

    def size(self):
        """Vrať skutečnou velikost formuláře (bez ohledu na aktuální velikost).

        Vrácená hodnota reprezentuje minimální velikost formuláře, tak aby byly
        všechny jeho prvky viditelné.  Skutečná velikost může být menší, nebo
        větší v závoslosti na velikost okna, ve kterém je formulář zobrazen.
        
        """
        return self._size

    def set_scrollbars(self):
        step = 20
        size = self.GetSize()
        self.SetScrollbars(step, step, size.width/step, size.height/step)
    
    def changed(self):
        """Vrať pravdu, pokud byla data změněna od posledního uložení."""
        field = find(True, self._fields, key=lambda f: f.is_modified())
        return field is not None

    def show_popup_menu(self):
        field = InputField.focused()
        if field is not None:
            field.show_popup_menu()

    def _on_field_edit(self, id, value):
        # Signalizace změny políčka z InputField
        self._row[id] = value

    def _on_field_change(self, id):
        # Signalizace změny políčka z PresentedRow
        field = find(id, self._fields, key=lambda f: f.id())
        if field is not None and self._row is not None:
            value = self._row.format(id)
            if field.initialized() and field.get_value() != value:
                field.set_value(value)
            
    def _on_editability_change(self, id, editable):
        if id in self._view.layout().order():
            if editable:
                self._field(id).enable()
            else:                
                self._field(id).disable()
                
    def _navigate(self, object=None, forward=True):
        # Vygeneruj událost navigace mezi políčky.
        if self._editable:
            nav = wx.NavigationKeyEvent()
            nav.SetDirection(forward)
            if object:
                nav.SetEventObject(object)
                nav.SetCurrentFocus(object)
            else:
                nav.SetCurrentFocus(self)
            self.GetEventHandler().ProcessEvent(nav)
        return True

    def exit_check(self):
        """Proveď kontrolu formuláře před uzavřením.

        Vrací: Pravdu právě tehdy když je možno formulář uzavřít.

        """
        if self.changed():
            q = _("Data byla změněna a nebyla uložena!") + "\n" + \
                _("Opravdu chcete uzavřít formulář?")
            if not run_dialog(Question, q):
                return False
        return True

    def on_command(self, command, **kwargs):
        if kwargs.has_key('originator') \
               and kwargs['originator'] in self._fields:
            field = kwargs['originator']
        else:
            field = InputField.focused()
        if field is not None and field.on_command(command, **kwargs):
            # Pokud se volal výběr položky seznamu z ListField,
            # musíme zajistit nastavení _refvalues v PresentedRow.
            if command == ListField.COMMAND_CHOOSE_KEY:
                if kwargs.has_key('id'):
                    id = kwargs['id']
                    val = field.get_item()
                    self._row.listfield_choose(id, val)
            return True
        if self._editable:
            if command == EditForm.COMMAND_RECORD_INSERT:
                self._edit_insert()
                return True
            elif command == EditForm.COMMAND_RECORD_UPDATE:
                self._edit_update()
                return True
            elif command == EditForm.COMMAND_RECORD_DELETE:
                self._edit_delete()
                return True
            elif command == EditForm.COMMAND_RECORD_COMMIT:
                self._commit_form()
                return True
            
        # Common commands
        if command == EditForm.COMMAND_NAVIGATE:
            return self._navigate()
        elif command == EditForm.COMMAND_NAVIGATE_BACK:
            return self._navigate(forward=False)
        else:
            return super_(EditForm).on_command(self, command, **kwargs)
        return False
        
    def _find_row_by_values(self, cols, values):
        """Vrať datový řádek odpovídající daným hodnotám.

        Arguemnty:

          cols -- sekvence názvů sloupců, které mají být prohledávány.
          values -- sekvence hodnot sloupců jako instancí 'pytis.data.Value' v
            pořadí odpovídajícím 'cols'.

        Pro obě sekvence platí, že pokud jsou jednoprvkové, mohou být hodnoty
        předány i přímo, bez obalení do sekvenčního typu.

        """
        cols = xtuple(cols)
        values = xtuple(values)
        assert len(cols) == len(values)
        condition = apply(pytis.data.AND, map(pytis.data.EQ, cols, values))
        data = self._data
        def find_row(condition):
            n = data.select(condition)
            return data.fetchone()
        success, result = db_operation((find_row, (condition,)))
        return result
    
    def select_row(self, position):
        """Vyber řádek dle 'position'.

        Argument 'position' může mít některou z následujících hodnot:
        
          None -- nebude zobrazen žádný řádek.
          Datový klíč -- bude zobrazen řádek s tímto klíčem, kterým je tuple
            instancí třídy 'pytis.data.Value'.
          Slovník hodnot -- bude zobrazen první nalezený řádek obsahující
            hodnoty slovníku (instance 'pytis.data.Value') v sloupcích určených
            klíči slovníku.
          Instance třídy 'pytis.data.Row', kompatibilní s datovým objektem
            seznamu -- bude zobrazen řádek odpovídajícího klíče.
        
        Pokud takový záznam neexistuje, zobraz chybový dialog a jinak nic.
        
        """
        if isinstance(position, pytis.data.Row):
            row = position
        elif isinstance(position, types.TupleType):
            cols = [c.id() for c in self._data.key()]
            row = self._find_row_by_values(cols, position)
        elif isinstance(position, types.DictType):
            row = self._find_row_by_values(position.keys(),
                                           position.values())
        else:
            ProgramError("Invalid 'position':", position)
        self._set_row(row)


class PopupEditForm(PopupForm, EditForm):
    """Stejné jako 'EditForm', avšak v popup podobě."""
    
    def __init__(self, parent, *args, **kwargs):
        parent = self._popup_frame(parent)
        EditForm.__init__(self, parent, *args, **kwargs)
        p = parent
        while not p.GetTitle() and p.GetParent():
            p = p.GetParent()
        parent.SetTitle('%s: %s' % (p.GetTitle(), self.title()))

    def _init_attributes(self, disable_new_button=False, **kwargs):
        EditForm._init_attributes(self, **kwargs)
        self._disable_new_button = disable_new_button
        
    def _create_form_parts(self, sizer):
        # Create all parts and add them to top-level sizer.
        layout = self._view.layout()
        # Create the parts.
        caption = self._create_caption(self, self.title(), size=18)
        group = self._create_group(layout.group())
        buttons = self._create_buttons()
        status_bar = self._create_status_bar()
        # Add parts to the sizer.
        sizer.Add(caption, 0, wx.ALIGN_CENTER|wx.ALL, 8)
        sizer.Add(group, 0, wx.ALIGN_CENTER|wx.ALL, 6)
        sizer.Add(buttons, 0, wx.ALIGN_CENTER)
        sizer.Add(status_bar, 0, wx.EXPAND)            

    def _create_status_bar(self):
        # Our own statusbar implementation
        status_bar = wx.Panel(self, -1, style=wx.SUNKEN_BORDER)
        box = wx.BoxSizer()
        status_bar.SetSizer(box)
        status_bar.SetAutoLayout(True)
        self._status = wx.StaticText(status_bar, -1, '',
                                     style=wx.ALIGN_LEFT)
        box.Add(self._status, 1, wx.EXPAND|wx.ALL, 2)
        box.Fit(status_bar)
        return status_bar

    def _on_submit(self, event):
        self._commit_form()
        return True

    def _on_next(self, event):
        result = self._commit_form(close=False)
        if result:
            message(_("Záznam uložen"))
            refresh()
            self._set_row(None)
            self.init()
        return False

    def _on_cancel(self, event):
        self._leave_form()
        return True
    
    def _create_buttons(self):
        ok, cancel = buttons = (wx.Button(self, wx.ID_OK, u"Ok"),
                                wx.Button(self, wx.ID_CANCEL, u"Zavřít"))
        wx_callback(wx.EVT_BUTTON, self, wx.ID_OK, self._on_submit)
        wx_callback(wx.EVT_BUTTON, self, wx.ID_CANCEL, self._on_cancel)
        ok.SetToolTipString(u"Uložit záznam a uzavřít formulář")
        cancel.SetToolTipString(u"Uzavřít formulář bez uložení dat")
        if self._new and not self._disable_new_button:
            next = wx.Button(self, wx.ID_FORWARD, u"Další")
            wx_callback(wx.EVT_BUTTON, self, wx.ID_FORWARD, self._on_next)
            next.SetToolTipString(u"Uložit záznam a reinicializovat formulář" +\
                                  u" pro vložení dalšího záznamu")
            buttons = (ok, cancel, next)
        ok.SetDefault()
        sizer = wx.BoxSizer(wx.HORIZONTAL)
        for b in buttons:
            sizer.Add(b, 0, wx.ALL, 20)
        return sizer
    
    def run(self):
        if self._editable:
            if not self._lock():
                return None
        try:
            return PopupForm.run(self)
        finally:
            self._unlock()

    def set_status(self, field, message):
        if field == 'message':
            if message is None:
                message = ''
            self._status.SetLabel(unicode(message))
            return True
        else:
            return False
       

class ShowForm(EditForm):
    """Formulář pro zobrazení náhledu.

    Layout je stejný jako u editačního formuláře (resp. 'EditForm'),
    pouze titulek má stejný vzhled, jako titulek formulářů typu 'ListForm'.
    Určen pro zobrazení v duálním formuláři.

    """

    _DESCR = _("náhled")

    def _init_attributes(self, editable=False, **kwargs):
        super_(ShowForm)._init_attributes(self, editable=editable, **kwargs)
        wx_callback(wx.EVT_SIZE, self, self._on_size)
        
    def _create_form_parts(self, sizer):
        # Create all parts and add them to top-level sizer.
        title = self._create_title_bar(self.title())
        group = self._create_group(self._view.layout().group())
        # Add parts to the sizer.
        sizer.Add(title, 0, wx.EXPAND)
        sizer.Add(group, 1, wx.ALIGN_CENTER|wx.BOTTOM, 8)
        
    def _on_size(self, event):
        self.set_scrollbars()
        event.Skip()

class BrowsableShowForm(ShowForm):
    """Listovací formulář pro zobrazení náhledu.

    Formulář je needitovatelný, ale umožňuje pohyb po záznamech tabulky, nad
    kterou je vytvořen, vyhledávání atd.  Z uživatelského hlediska jde v
    podstatě o redukci prohlížecích možností formuláře typu 'BrowseForm' na
    jeden záznam zobrazený v Layoutu editačního formuláře.
    
    """
    
    def __init__(self, *args, **kwargs):
        super_(BrowsableShowForm).__init__(self, *args, **kwargs)
        self._init_select()
        self._set_status()

    def _on_set_row(self, row_number):
        # row_number začíná od 0
        def get_it():
            data = self._data
            data.rewind()
            data.skip(row_number)
            return data.fetchone()
        success, row = db_operation(get_it)
        if not row:
            beep()
        if not success or not row:
            return
        self._set_row(row)       

    def _on_jump(self):
        row = super_(BrowsableShowForm)._on_jump(self)
        if row:
            self._on_set_row(row-1)
        else:
            message(_("Neplatné číslo záznamu"), beep_=True)                    

    def _on_next_record(self, direction=pytis.data.FORWARD):
        op = lambda : self._data.fetchone(direction=direction)
        success, row = db_operation(op)
        if not row:
            if direction == pytis.data.FORWARD:
                message(_("Poslední záznam"), beep_=True)
            else:
                message(_("První záznam"), beep_=True)
            # Přesuneme ukazovátko zpět na poslední záznam, to je chování
            # očekávané uživateli.
            antidirection = pytis.data.opposite_direction(direction)
            db_operation(lambda:
                         self._data.fetchone(direction=antidirection))
        if not success or not row:
            return
        self._set_row(row)

    def _set_row(self, row):
        super(BrowsableShowForm, self)._set_row(row)
        self._set_status()

    def _set_status(self):
        current, total = self._data.last_row_number(), self._lf_select_count
        if total:
            set_status('list-position', "%d/%d" % (current+1, total))        

    def on_command(self, command, **kwargs):
        if command == BrowsableShowForm.COMMAND_NEXT_RECORD:
            self._on_next_record()
            return True
        elif command == BrowsableShowForm.COMMAND_PREVIOUS_RECORD:
            self._on_next_record(direction=pytis.data.BACKWARD)
            return True
        elif command == BrowsableShowForm.COMMAND_FIRST_RECORD:
            self._on_set_row(0)
            return True
        elif command == BrowsableShowForm.COMMAND_LAST_RECORD:
            self._on_set_row(self._lf_select_count-1)
            return True
        else:
            return super(BrowsableShowForm, self).on_command(command, **kwargs)
