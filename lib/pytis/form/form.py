# -*- coding: iso-8859-2 -*-

# Copyright (C) 2001-2011 Brailcom, o.p.s.
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

"""Interaktivn� formul��e pro pr�ci s�daty.

Z�kladem v�ech formul��� je t��da 'Form'.  D�le tento modul obsahuje jej�
potomky pro konkr�tn� pou�it� -- jednoduch� edita�n� formul��e (pro zobrazen� a
editaci jednoho z�znamu).  Jednoduch� seznamov� formul��e a du�ln� formul��e
jsou v odd�len�ch modulech 'list' a 'dualform'.  Bl�e viz dokumentace
jednotliv�ch t��d.

"""

import time
import pytis.data
import pytis.output
from pytis.presentation import PresentedRow
from pytis.form import *
import wx

class FormProfile(pytis.presentation.Profile):
    """Form profile specification that can be saved and restored.

    Unlike 'pytis.presentation.Profile' (the base class), instances of this
    class can be safely pickled and unpickled (see notes below) and define also
    some additional profile attributes which are not exposed to specifications
    but the user interface needs to save/restore them.

    Thus instances of this class are used to save and restore user defined form
    profiles as well as user customizations of predefined form profiles
    originally defined in specifications.

    Important note: Pickling and unpickling is unfortunately not a symmetrical
    operation.  After unpickling an instance, the method 'validate()' must be
    called to check whether all profile parameters match with the current
    specification and also the stored pytis conditions (the 'filter' attribute
    of the profile) must be unpacked from the internal representation into
    'pytis.data.Operator()' instances.  This is because 'pytis.data.Operator()'
    instances refer to values with data type instances, which are often bound
    to data objects.  To prevent pickling whole structures of living objects,
    the filtering condition is stored in a packed form which doesn't refer to
    data types and objects.  A form's data object is needed to unpack them.
    This is why 'validate()' must be called passing it the data object of the
    form.

    """
    def __init__(self, id, name, column_widths=None,
                 group_by_columns=(), aggregation_columns=(), **kwargs):
        """Specific keyword arguments:

          group_by_columns -- tuple of group by columns (their string
            identifiers) for an aggregated view of given form
          aggregation_columns -- tuple of aggregation column specifications,
            where each item is a pair of string column identifier and the
            aggregation function as one of pytis.data.Data AGG_* constants.
            May only be used if 'group_by_columns' are also defiend.
          column_widths -- dictionary of pixel widths of form columns keyed by
            column identifiers

        All other arguments the sam as for the parent class.

        """
        super(FormProfile, self).__init__(id, name, **kwargs)
        self._group_by_columns = group_by_columns
        self._aggregation_columns = aggregation_columns
        self._column_widths = column_widths or {}
        self._state = None
        
    def __getstate__(self):
        # We prefer saving the condition in our custom format, since its safer
        # to pickle Python builtin types than instances of application defined
        # classes.
        def pack(something):
            if isinstance(something, pytis.data.Operator):
                args = tuple([pack(arg) for arg in something.args()])
                return (something.name(), args, something.kwargs())
            elif isinstance(something, pytis.data.Value):
                return [something.export()]
            elif isinstance(something, pytis.data.WMValue):
                return [something.value()]
            elif isinstance(something, str):
                return something
            else:
                raise ProgramError("Unknown object in filter operator:", something)
        if self._filter is None:
            filter = None
        else:
            filter = pack(self._filter)
        return dict(self.__dict__, _filter=filter)

    def __setstate__(self, state):
        # Don't restore the state here, to avoid accessing any attributes
        # before validation (see also __getattr__).
        self._state = state

    def __getattr__(self, name):
        if self._state is not None:
            raise ProgramError("Attempted to access unpacked profile: Call 'validate()' first!")
        else:
            raise AttributeError("%r object has no attribute %r" % (type(self).__name__, name))

    def rename(self, name):
        """Change the name of the profile to given 'name' (string)."""
        self._name = name

    def set_filter(self, filter):
        """Change the filter of the profile to given value (pytis.data.Operator instance)."""
        self._filter = filter

    def validate(self, view, data):
        """Validate the instance after loading (see the class docstring for more information)."""
        # NOT is not allowed!
        OPERATORS = ('AND','OR','EQ','NE','WM','NW','LT','LE','GT','GE')
        def unpack(packed):
            name, packed_args, kwargs = packed
            if name not in OPERATORS:
                raise Exception("Invalid operator '%s'" % name)
            op = getattr(pytis.data, name)
            if name in ('AND', 'OR'):
                args = [unpack(arg) for arg in packed_args]
            else:
                if len(packed_args) != 2:
                    raise Exception("Invalid number of operator arguments: %s" % repr(packed_args))
                if isinstance(packed_args[1], list):
                    col, val = packed_args[0], packed_args[1][0]
                    column = data.find_column(col)
                    if column is None:
                        raise Exception("Unknown column '%s'" % col)
                    if name in ('WM', 'NW'):
                        value, err = column.type().wm_validate(val)
                    else:
                        value, err = column.type().validate(val, strict=False)
                    if err is not None:
                        raise Exception("Invalid operand value for '%s': %s" % (col, err))
                    args = col, value
                else:
                    args = packed_args
                    for col in args:
                        if data.find_column(col) is None:
                            raise Exception("Unknown column '%s'" % col)
            return op(*args, **kwargs)
        if self._state is not None:
            state = self._state
            self._state = None
            self.__dict__.update(state)
        if self._filter:
            try:
                self._filter = unpack(self._filter)
            except Exception, e:
                log(OPERATIONAL, "Unable to restore fitler for profile '%s':" % self._name,
                    (self._filter, str(e)))
                return False
        # Check the column identifiers in all profile attributes (except for
        # filters, which are checked above)
        for attr, getcol in (('_columns', lambda x: x),
                             ('_sorting', lambda x: x[0]),
                             ('_grouping', lambda x: x),
                             ('_group_by_columns', lambda x: x),
                             ('_aggregation_columns', lambda x: x[0])):
            sequence = getattr(self, attr)
            if sequence is not None:
                for x in sequence:
                    col = getcol(x)
                    if view.field(col) is None:
                        log(OPERATIONAL, "Unknown column '%s' in %s of profile '%s':" % \
                                (col, attr[1:], self._name))
                        return False
        return True

    def group_by_columns(self):
        return self._group_by_columns
    
    def aggregation_columns(self):
        return self._aggregation_columns

    def column_widths(self):
        return self._column_widths
    

class Form(Window, KeyHandler, CallbackHandler, CommandHandler):
    """Spole�n� nadt��da formul���.

    Formul�� si podle jm�na specifikace p�edan�ho konstruktoru vy��d� od
    resolveru p��slu�nou datovou a prezenta�n� specifikaci.  Z datov�
    specifikace vytvo�� datov� objekt (instance t��dy odvozen� z
    'pytis.data.Data').  Datov� objekt a prezenta�n� specifikace jsou potom
    ulo�eny ve form� atribut� instance formul��e ('self._view' a 'self._data')

    Instance t��d odvozen�ch z t�to t��dy jsou potom vytv��eny na z�klad�
    interpretace prezenta�n� specifikace a pracuj� s daty s pomoc� datov�ho
    objektu a jeho API (kter� je nez�visl� na konkr�tn�m zdroji dat).

    Form je potomkem 'Window', d�ky �emu� je mo�n� jej ukl�dat na z�sobn�k oken
    aplikace a prov�d�t dal�� operace, jako zaost�ov�n�, skr�v�n�, zobrazov�n�
    apod.

    Pou��van� specifika�n� funkce:

      print_spec -- sekvence dvojic (POPIS, SOUBOR), kde POPIS je string se
        stru�n�m slovn�m popisem specifikace (vyu��van�m nap��klad jako titulek
        polo�ky menu) a SOUBOR je string ud�vaj�c� jm�no souboru se
        specifikac�, relativn� k�adres��i s�defini�n�mi soubory, bez p��pony

    """

    CALL_USER_INTERACTION = 'CALL_USER_INTERACTION'
    """Konstanta callbacku interakce u�ivatele."""

    _STATUS_FIELDS = ()
    _LOG_STATISTICS = True
    DESCR = None

    class InitError(Exception):
        """Exception signaling errors on form initializations."""

    def _get_command_handler_instance(cls):
        return current_form(inner=False)
    _get_command_handler_instance = classmethod(_get_command_handler_instance)

    def descr(cls):
        """Vra� textov� popis typu formul��e jako �et�zec."""
        if cls.DESCR is not None:
            return cls.DESCR
        else:
            return cls.__class__.__name__
    descr = classmethod(descr)
        
    def __init__(self, parent, resolver, name, guardian=None, transaction=None,
                 spec_kwargs={}, data_kwargs={}, **kwargs):
        """Inicializuj instanci.

        Argumenty:
        
          parent -- instance 'wxFrame', do kter�ho formul�� pat��
          resolver -- resolver jmenn�ch odkaz�, instance 'pytis.util.Resolver' 
          name -- jm�no specifika�n�ho souboru pro resolver; string
          guardian -- formul�� (instance libovoln� t��dy), ve kter�m je
            formul�� vlo�en z�hlediska struktury aplikace; nen�-li zad�n, je
            pou�it 'parent'.  Tento parametr je vyu��v�n nap��klad p�i zas�l�n�
            kl�vesov�ch ud�lost� \"nahoru\".  Typicky je to formul��, kter�
            tuto instanci vytv���.
          transaction -- transaction to use when manipulating data
          spec_kwargs -- dictionary of keyword arguments passed to the view
            specification constructor.
          data_kwargs -- dictionary of additional keyword arguments passed to
            the data object constructor.
          kwargs -- viz n�e.

        Resolver je pou�it k z�sk�n� datov� a prezenta�n� specifikace a
        n�sledn�mu vytvo�en� datov�ho objektu. Ten je potom spole�n� s
        prezenta�n� specifikac� ulo�en v podob� atribut� vytv��en� instance.

        Odkaz na resolver samotn� je tak� zapamatov�n pro pozd�j�� pou�it�
        (vytv��en� dal��ch formul���).
          
        Inicializace je rozd�lena do n�kolika krok�.  Nejprve jsou zpracov�ny
        v�echny argumenty spole�n� v�em formu��ov�m t��d�m.  Ty zpracov�v�
        konstruktor b�zov� t��dy 'Form'.  Jejich zpracov�n� by nem�lo b�t
        p�edefinov�v�no v odvozen�ch t��d�ch a ani ��dn� dal�� argumenty by
        nem�ly b�t p�id�v�ny.  Konstruktor je mo�no p�edefinovat a prov�d�t
        n�jak� dopl�uj�c� akce, ale argumenty by nem�ly b�t m�n�ny.

        Po zpracov�n� spole�n�ch argumwent� jsou na�teny specifikace a vytvo�en
        datov� objekt.

        Pot� jsou zpracov�v�ny kl��ov� argumenty.  Ka�d� odvozen� t��da m��e
        definovat sv� vlastn� kl��ov� argumenty.  Ty potom zpracuje
        p�edefinov�n�m metody '_init_attributes()'.  Ta ji� m��e vyu��vat
        inicializovan�ho datov�ho objetu a specifikac� a p��padn� initializovat
        dal�� atributy t��dy.  Metoda '_init_attributes()' by m�la v�dy
        zpracov�vat pouze kl��ov� argumenty, kter� jsou specifick� pro danou
        t��du.  Zbyl� p�ed� metod� rodi�ovsk� t��dy konstrukc� **kwargs.  Takto
        by m�lo b�t zaru�eno, �e dojde postupn� ke zpracov�n� v�ech argument�.

        Teprve po zpravov�n� argument� konstruktoru a inicializaci atribut� je
        vytv��en vlastn� obsah formul��e (viz. '_create_form()').  Toto by m�lo
        b�t dodr�ov�no i v odvozen�ch t��d�ch.
        
        """
        import pytis.extensions
        start_time = pytis.data.DateTime.now()
        self._parent = parent
        self._resolver = resolver
        self._name = name
        self._guardian = guardian or parent
        self._governing_transaction = transaction
        self._transaction = transaction or self._default_transaction()
        self._spec_kwargs = copy.copy(spec_kwargs)
        self._data_kwargs = copy.copy(data_kwargs)
        self._leave_form_requested = False
        Window.__init__(self, parent)
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
        log(EVENT, 'Form created in %.3fs:' % (show_time.value() - start_time.value(),), self)

    def _init_attributes(self):
        """Process constructor keyword arguments and initialize the attributes.

        This method is called in the initial phase of form construction before any UI widget
        creation but after the initialization of specifications and the data object.  The derived
        classes should primarily process all their specific constructor arguments and initialize
        the attributes of the instance.  See also the constructor documentation for more details.

        """
        pass
        
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

    def __str__(self):
        return '<%s for "%s">' % (self.__class__.__name__, self._name)

    def __repr__(self):
        return str(self)

    def _fullname(self):
        cls = self.__class__
        return 'form/%s.%s/%s//' % (cls.__module__, cls.__name__, self._name)

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
        
    # Zpracov�n� p��kaz�
   
    def _cmd_help(self):
        help(self._name.replace(':','-'))

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

    # Ve�ejn� metody
    
    def name(self):
        """Vra� n�zev specifikace formul��e."""
        return self._name

    def title(self):
        """Vra� titulek ze specifikace formul��e jako �et�zec."""
        return self._view.title()

    def guardian(self):
        """Vra� guardian zadan� v�konstruktoru (nebo parent)."""
        return self._guardian

    def check_permission(self, perm, quiet=True):
        """Vra� pravdu, pokud m� u�ivatel dan� pr�va k datov�mu objektu.

        Argumentem je konstanta  t��dy 'pytis.data.Permission'.

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
                VIEW:   "Nem�te pr�vo k�zobrazen� formul��e.",
                INSERT: "Nem�te pr�vo vlo�it nov� z�znam.",
                UPDATE: "Nem�te pr�vo zm�nit existuj�c� z�znam.",
                DELETE: "Nem�te pr�vo smazat existuj�c� z�znam.",
                EXPORT: "Nem�te pr�vo k exportu do CSV.",
                }[perm]
            message(msg, beep_=True)
        return result

    def set_status(self, field, message):
        """Zobraz zpr�vu `message' v poli `id' stavov� ��dky formul��e.

        M�-li formul�� vlastn� stavovou ��dku a v n� pole `id' zobraz v n�m
        danou zpr�vu a vra� pravdu.  V opa�n�m p��pad� vra� nepravdu.

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
        
    def size(self):
        """Return the prefered form size in pixels as a tuple of two integers (width, height).

        None may be returned if the prefered form size is not defined or known.

        """
        return None

    
class InnerForm(Form):
    """Formul��, kter� zpracuje p��kazy samostatn� i unvit� du�ln�ho formul��e.

    Tato formul��ov� t��da je zde p�edev��m kv�li definici a zpracov�n�
    p��kaz�.  Pokud je aktu�ln�m formul��em jednoduch� formul��, je zpracov�n�
    p��kazu p�ed�no tomuto formul��i.  Pokud je v�ak aktu�ln�m formul��em
    du�ln� formul��, je t�eba rozhodnout, zda bude p��kaz zpracov�n p��mo
    du�ln�m formul��em, nebo jeho aktivn�m podformul��em.  P��kazy t��dy 'Form'
    jsou zpracov�v�ny v�dy formul��em nejvy��� �rovn� (du�ln�m formul��em
    samotn�m, pokud je aktu�ln�m formul��em du�ln� formul��).

    P��kazy definovan� touto t��dou a t��dami z n� odvozen�mi jsou v�ak v�dy
    p�ed�v�ny aktivn�mu vnit�n�mu formul��i.
    
    """
    def _get_command_handler_instance(cls):
        return current_form()
    _get_command_handler_instance = classmethod(_get_command_handler_instance)
    
    def _init_attributes(self, **kwargs):
        super(InnerForm, self)._init_attributes(**kwargs)
        # The aggregation menu must be created dynamically, but we can find out just once,
        # whether the menu exists for given form.
        self._has_aggregation_menu = self._aggregation_menu() is not None
        # Print menu is static for given form instance, so we create it just once.
        self._print_menu_ = self._print_menu()
        
    def _on_menu_button(self, items):
        self._run_callback(self.CALL_USER_INTERACTION)
        parent = wx_focused_window()
        if parent:
            popup_menu(parent, items, self._get_keymap())

    def _print_menu(self):
        # Vra� seznam polo�ek tiskov�ho menu.
        name = self._name
        try:
            print_spec = self._resolver.get(name, 'print_spec') or ()
        except ResolverSpecError:
            print_spec = ()
        # Default print currently disabled, since on a huge table it may extensively cunsume
        # resources and no one is using it anyway...
        #if not print_spec:
        #    print_spec = ((_("V�choz�"), os.path.join('output', name)),)
        return [MItem(title, command=BrowseForm.COMMAND_PRINT(print_spec_path=path))
                for title, path in print_spec]

    def _aggregation_menu(self):
        return None
    
    def _cmd_describe(self):
        title = self._view.title()
        description = self._view.help() or self._view.description()
        text = "= "+ title +" =\n\n" + description
        InfoWindow(_("Popis n�hledu %s") % title, text=text, format=TextFormat.WIKI)
        
    def _can_describe(self):
        description = self._view.help() or self._view.description()
        return description is not None
        
    def _cmd_aggregation_menu(self):
        self._on_menu_button(self._aggregation_menu())
        
    def _can_aggregation_menu(self):
        return self._has_aggregation_menu
        
    def _can_print_menu(self):
        return bool(self._print_menu_)

    def _cmd_print_menu(self):
        self._on_menu_button(self._print_menu_)

    
class Refreshable:
    """T��da zaji��uj�c� existenci metody 'refresh()' s�dan�m v�znamem.

    Tuto t��du by m�ly d�dit v�echny formul��e, kter� maj� b�t obnoveny p�i
    zm�n� dat (typicky zp�soben� jin�m formul��em v��e na z�sobn�ku r�mc�).
    
    """

    DOIT_IMMEDIATELY = 'DOIT_IMMEDIATELY'
    """Konstanta pro 'refresh()' pro okam�it� update.

    Nen�-li seznam pr�v� editov�n, je update proveden okam�it�.  Jinak je
    u�ivatel dot�z�n, zda m� b�t update proveden ihned; odpov�-li u�ivatel
    negativn�, je update proveden a� po ukon�en� editace.

    """
    DOIT_AFTEREDIT = 'DOIT_AFTEREDIT'
    """Konstanta pro 'refresh()' pro update po skon�en� editace.

    Nen�-li seznam pr�v� editov�n, je update proveden okam�it�.  Jinak je
    proveden a� po ukon�en� editace.
    
    """
    DOIT_IFNEEDED = 'DOIT_IFNEEDED'
    """Konstanta pro 'refresh()' pro podm�n�n� update.

    Update je proveden pouze tehdy, je-li zn�mo, �e do�lo ke zm�n� dat.
    V�takov�m p��pad� je proveden okam�it� pouze tehdy, jestli�e seznam nen�
    pr�ve editov�n a v�posledn� dob� nebyl proveden ��dn� jin� update;
    v�opa�n�m p��pad� je update odlo�en \"a� na vhodn�j�� chv�li\" (nicm�n�
    proveden bude).

    """
    _block_refresh = 0

    def block_refresh(cls, function, *args, **kwargs):
        """Zablokuj ve�ker� refresh po dobu prov�d�n� funkce 'function'.

        V�echny argumenty jsou p�ed�ny volan� funkci.
        
        Vrac�: v�sledek vr�cen� volanou funkc�.

        Refresh je zablokov�n glob�ln�, pro v�echny existuj�c� formul��e.
        
        """
        Refreshable._block_refresh += 1
        try:
            result = function(*args, **kwargs)
        finally:
            Refreshable._block_refresh -= 1
        return result
    block_refresh = classmethod(block_refresh)
    
    def refresh(self, when=None):
        """Aktualizuj data formul��e z�datov�ho zdroje.

        P�ekresli data ve formul��i v�okam�iku dan�m argumentem 'when'.

        Argumenty:

          when -- ur�uje, zda a kdy m� b�t aktualizace provedena, mus� to b�t
            jedna z�'DOIT_*' konstant t��dy.  Implicitn� hodnota je
            'DOIT_AFTEREDIT', je-li 'reset' 'None', 'DOIT_IMMEDIATELY' jinak.

        Vrac�: Pravdu, pr�v� kdy� byla aktualizace provedena.

        """
        level = Refreshable._block_refresh
        if level == 0:
            self._refresh(when=when)
        elif level > 0:
            log(OPERATIONAL, "Refresh neproveden kv�li blokaci:", level)
        else:
            raise ProgramError("Nep��pustn� hodnota _block_refresh:", level)

    def _refresh(self, when=None):
        """Prove� vlastn� refresh.

        Tuto metodu nech� p�edefinuj� odvozen� t��dy.

        """
        pass


class PopupForm:
    """Formul�� nach�zej�c� se v�samostatn�m framu.

    Tato t��da je ur�ena k�vlo�en� mezi p�edky t��dy, jej� instance maj� b�t
    vytv��eny v�samostatn�ch framech.  Pro z�sk�n� framu slou�� metoda
    '_popup_frame'.

    """
    def _popup_frame(self, parent):
        """Vra� frame instance.

        Pokud frame je�t� neexistuje, vytvo� jej.

        Argumenty:
        
          parent -- rodi�ovsk� okno, instance 'wx.Window'

        """
        try:
            frame = self._popup_frame_
        except AttributeError:
            style = wx.DIALOG_MODAL|wx.DEFAULT_DIALOG_STYLE #|wx.RESIZE_BORDER
            frame = wx.Dialog(parent, style=style)
            self._popup_frame_ = frame
            wx_callback(wx.EVT_CLOSE, frame, self._on_frame_close)
        return frame    

    def _on_frame_close(self, event):
        if self:
            if self._exit_check():
                self.defocus()
                event.Skip()
            else:
                event.Veto()

    def close(self, force=False):
        # T�m se zavol� _on_frame_close() a tam provedeme zbytek.
        return self._popup_frame_.Close(force=force)
        
    def run(self, lock_key=None):
        """Show the form as a modal dialog.

        Arguments:

          lock_key -- lock the row with the given key

        """
        if lock_key is not None and not isinstance(self._data, pytis.data.DBDataDefault):
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
    """Mix-in t��da pro formul��e s�titulkem.
    
    Lze vyu��t bu�to pouze metodu '_create_caption()', kter� vytv��� samotn�
    text titulku, nebo metodu '_create_title_bar()', kter� p�id�v� 3d panel.

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
        """Vytvo� 3d panel s nadpisem formul��e."""
        panel = wx.Panel(self, -1, style=wx.RAISED_BORDER)
        caption = self._create_caption(panel)
        box = wx.BoxSizer()
        box.Add(caption, 1, wx.EXPAND|wx.ALL, self._TITLE_BORDER_WIDTH)
        panel.SetSizer(box)
        box.Fit(panel)
        return panel


class LookupForm(InnerForm):
    """Formul�� s�vyhled�v�n�m a t��d�n�m."""
    
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
    _USER_PROFILE_PREFIX = '_user_profile_'

    def _init_attributes(self, filter=None, sorting=None, columns=None, grouping=None,
                         condition=None, arguments=None, **kwargs):
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
          condition -- 'pytis.data.Operator' instance filtering the rows of the
            underlying data object.  This filter is not indicated to the user
            nor is there a chance to turn it off.
          arguments -- dictionary of table function call arguments, with
            function argument identifiers as keys and 'pytis.data.Value'
            instances as values.  Useful only when the table is actually a row
            returning function, otherwise ignored.
          kwargs -- arguments passed to the parent class
        
        """
        super_(LookupForm)._init_attributes(self, **kwargs)
        assert columns is None or isinstance(columns, (list, tuple))
        # Create a Profile instance representing the form constructor
        # arguments.  Note, that the default profile is not necessarily the
        # initially selected profile.
        self._default_profile = Profile('__default_profile__', _("V�choz� profil"),
                                        filter=filter, sorting=sorting, columns=columns,
                                        grouping=grouping)
        self._profiles, self._invalid_profiles = self._load_profiles()
        initial_profile_id = self._view.profiles().default()
        if initial_profile_id:
            current_profile = find(initial_profile_id, self._profiles, key=lambda p: p.id())
        else:
            current_profile = self._profiles[0]
        # The profile instances may contain None values to denote default
        # values.  We need to remember the corresponding real values to be able
        # to compare profiles with the current form state in
        # '_current_profile_changed()'.  We rely on the fact that
        # '_apply_profile_parameters()' substitutes None values by their
        # corresponding default values and we don't want to repeat this logic
        # anywhere else.  Thus we first apply the default profile, store the
        # resulting profile parameters and only then apply the current profile.
        self._apply_profile_parameters(self._default_profile)
        self._default_profile_parameters = self._profile_parameters_to_save()
        self._apply_profile_parameters(current_profile)
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
        return dict(condition=self._lf_condition, sorting=self._lf_sorting)

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
        return {}

    def _on_idle(self, event):
        if super(LookupForm, self)._on_idle(event):
            return True
        if self._invalid_profiles:
            profiles = self._invalid_profiles
            self._invalid_profiles = []
            self._delete_invalid_profiles(profiles)
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
            log(EVENT, 'Selh�n� datab�zov� operace')
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
            log(EVENT, 'Z�znam nenalezen')
            if report_failure:
                message(_("Z�znam nenalezen"), beep_=True)
            result = None
        else:
            if initial_shift:
                if direction == pytis.data.FORWARD:
                    skip = skip + (start_row_number - row_number)
                else:
                    skip = skip + (row_number - start_row_number)
            result = skip
            log(EVENT, 'Z�znam nalezen:', skip)
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
            prompt = _("Z�znam ��slo (1-%s):") % (max_value,)
            result = run_dialog(InputNumeric, message=_("Skok na z�znam"), prompt=prompt,
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
                elif isinstance(a, str) and a not in columns:
                    columns.append(a)
        if self._lf_filter is not None:
            analyze(self._lf_filter)
        return columns

    def _apply_filter(self, condition):
        self._lf_filter = condition
        self._init_select(async_count=False)
        self.select_row(self._current_key())

    def _save_profile(self, profile):
        profile_manager().save_profile(self._fullname(), profile)
    
    def _load_profiles(self):
        manager = profile_manager()
        fullname = self._fullname()
        profiles = []
        invalid_profiles = []
        for profile in (self._default_profile,) + tuple(self._view.profiles()):
            custom = manager.load_profile(fullname, profile.id())
            if custom:
                if custom.validate(self._view, self._data):
                    # Force the filter of system profiles to the filter from
                    # specification because it often contains dynamic
                    # conditions, such as EQ('date', now()) which are destroyed
                    # when saved (the saved condition would be EQ('date',
                    # '2011-03-01') for example).  That's also why filters of
                    # system profiles are not editable.
                    custom.set_filter(profile.filter())
                    profile = custom
                else:
                    invalid_profiles.append(custom)
            profiles.append(profile)
        for profile_id in manager.list_profile_ids(fullname):
            if profile_id.startswith(self._USER_PROFILE_PREFIX):
                profile = manager.load_profile(fullname, profile_id)
                if profile:
                    if profile.validate(self._view, self._data):
                        profiles.append(profile)
                    else:
                        invalid_profiles.append(profile)
        return profiles, invalid_profiles

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
        self._apply_profile_parameters(profile)
        self._init_select(async_count=False)
        self.select_row(self._current_key())

    def _create_profile(self, id, name):
        return FormProfile(id, name, **self._profile_parameters_to_save())

    def _profile_parameters_to_save(self):
        """Return the profile parameters representing the current state of the form.

        The returned dictionary is passed to 'FormProfile' constructor as
        keyword arguments when saving a profile.  Note, that the profile
        instance stored in `self._current_profile' may not have the same
        parameters if the user changed them since he switched to that profile
        (`self._current_profile' instance is not updated when the form state
        changes, it representes the previously saved state).
        
        """
        return dict(filter=self._lf_filter, sorting=self._lf_sorting)

    def _current_profile_changed(self):
        for param, current_value in self._profile_parameters_to_save().items():
            if hasattr(self._current_profile, param):
                original_value = getattr(self._current_profile, param)()
            else:
                # If the current profile is a 'Profile' instance, it lacks the
                # 'FormProfile' specific parameters.
                original_value = None
            if original_value is None:
                original_value = self._default_profile_parameters[param]
            if current_value != original_value:
                return True
        return False

    def _delete_invalid_profiles(self, profiles):
        # If the profile is not deleted, it is not used in the current form
        # anyway.  But the form will attempt to load it again next time.
        for profile in profiles:
            if run_dialog(Question, icon=Question.ICON_ERROR,
                          title=_("Neplatn� profil"),
                          message=_("U�ivatelsk� profil \"%s\" je neplatn�.\n"
                                    "Pravd�podobn� do�lo ke zm�n� definice n�hledu\n"
                                    "a ulo�en� profil ji� nelze pou��t.\n\n"
                                    "P�ejete si profil smazat?") % profile.name()):
                profile_manager().drop_profile(self._fullname(), profile.id())
    
    def _cmd_apply_profile(self, index):
        self._apply_profile(self._profiles[index])
        self.focus()

    def _cmd_save_new_profile(self, name):
        if name in [profile.name() for profile in self._profiles]:
            message(_("Takto pojmenovan� profil ji� existuje."), beep_=True)
            return
        user_profile_numbers = [int(profile.id()[len(self._USER_PROFILE_PREFIX):])
                                for profile in self._profiles
                                if profile.id().startswith(self._USER_PROFILE_PREFIX)
                                and profile.id()[len(self._USER_PROFILE_PREFIX):].isdigit()]
        profile_id = self._USER_PROFILE_PREFIX + str(max(user_profile_numbers+[0])+1)
        profile = self._create_profile(profile_id, name)
        self._profiles.append(profile)
        self._save_profile(profile)
        self._current_profile = profile
        message(_("Profil ulo�en pod n�zvem '%s'.") % name)
        self.focus()

    def _can_rename_profile(self, name):
        return self._current_profile.id().startswith(self._USER_PROFILE_PREFIX)

    def _cmd_rename_profile(self, name):
        if name in [p.name() for p in self._profiles if p is not self._current_profile]:
            message(_("Takto pojmenovan� profil ji� existuje."), beep_=True)
            return
        self._current_profile.rename(name)
        self._save_profile(self._current_profile)
        message(_("Profil ulo�en pod n�zvem '%s'.") % name)
        self.focus()

    def _can_update_profile(self):
        return self._current_profile_changed()
        
    def _cmd_update_profile(self):
        current = self._current_profile
        index = self._profiles.index(current)
        profile = self._create_profile(current.id(), current.name())
        self._profiles[index] = profile
        self._save_profile(profile)
        self._current_profile = profile
    
    def _can_delete_profile(self):
        return self._current_profile.id().startswith(self._USER_PROFILE_PREFIX)

    def _cmd_delete_profile(self):
        profile_manager().drop_profile(self._fullname(), self._current_profile.id())
        self._profiles.remove(self._current_profile)
        self._apply_profile(self._profiles[0])

    def _can_reload_profile(self):
        return self._current_profile_changed()
    
    def _cmd_reload_profile(self):
        self._apply_profile(self._current_profile)
        
    def _can_reset_profile(self):
        return (isinstance(self._current_profile, FormProfile)
                and not self._current_profile.id().startswith(self._USER_PROFILE_PREFIX))
        
    def _cmd_reset_profile(self):
        index = self._profiles.index(self._current_profile)
        profile_id = self._current_profile.id()
        if profile_id == self._default_profile.id():
            profile = self._default_profile
        else:
            profile = find(profile_id, self._view.profiles(), key=lambda p: p.id())
        profile_manager().drop_profile(self._fullname(), profile_id)
        self._profiles[index] = profile
        self._apply_profile(profile)
        
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
            message(_("Podle tohoto sloupce nelze filtrovat."), beep_=True)
        condition = pytis.data.EQ(column_id, value)
        if self._lf_filter is not None:
            condition = pytis.data.AND(self._lf_filter, condition)
        self.COMMAND_FILTER.invoke(condition=condition)

    def _cmd_sort(self, col=None, direction=None, primary=False):
        """Zm�� t��d�n�.

        Argumenty:

          col -- id sloupce, podle kter�ho m� b�t seznam set��d�n, nebo
            'None' pro glob�ln� zm�ny (nap��klad vypnut� ve�ker�ho t��d�n�)
          direction -- sm�r t��d�n� (sestupn�/vzestupn�/v�bec).  Hodnota dan�
            konstantou 'LookupForm.SORTING_NONE' zna�� po�adavek na zru�en�
            t��d�n�.  Jinak je o�ek�v�na jedna z konstant
            'LookupForm.SORTING_ASCENDENT' (pro sestupn� t��d�n�), nebo
            'LookupForm.SORTING_DESCENDANT' (pro vzestupn� t��d�n�).
          primary -- pr�v� kdy� je pravdiv�, bude dan� sloupec zvolen jako
            prim�rn� a *jedin�* t��d�c� sloupec.  V opa�n�m p��pad� bude pouze
            p�id�n na konec st�vaj�c�ho seznamu t��d�c�ch sloupc�.
        
        P�i nejednozna�n� kombinaci argument� 'col' a 'direction' je
        automaticky vyvol�n dialog pro v�b�r t��d�c�ch krit�ri�.
        
        """
        sorting = self._determine_sorting(col=col, direction=direction, primary=primary)
        if sorting is not None and sorting != self._lf_sorting:
            self._lf_sorting = sorting
            self.select_row(self._current_key())
        return sorting
    
    def _can_sort(self, col=None, direction=None, primary=False):
        # `col' je zde identifik�tor sloupce.
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
                message(_("Podle tohoto sloupce nelze t��dit"),
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
        
    # Ve�ejn� metody
    
    @classmethod
    def add_toolbar_ctrl(cls, toolbar, uicmd):
        cmd, kwargs = uicmd.command(), uicmd.args()
        if cmd == LookupForm.COMMAND_PROFILE_MENU:
            # Create the profile selection combo box.  The current 'Command'
            # implementation doesn't support command control updates other than
            # enabling/disabling.  This example, together with the hack for
            # DualForm.COMMAND_OTHER_FORM might serve as reference for futre
            # generalization of command control updates.
            ctrl = ProfileSelector(toolbar, size=(270, 25))
            ctrl.SetToolTipString(uicmd.title())
            tool = toolbar.AddControl(ctrl)
            toolbar.SetToolLongHelp(tool.GetId(), uicmd.descr()) # Doesn't work...
        else:
            InnerForm.add_toolbar_ctrl(toolbar, uicmd)
    
    def filter(self, condition):
        """Apply given filtering condition."""
        self._apply_filter(condition)
        if not self._current_profile.id().startswith(self._USER_PROFILE_PREFIX) \
                and condition != self._current_profile.filter():
            name = _("Nepojmenovan� profil")
            i = 1
            while name in [profile.name() for profile in self._profiles]:
                i += 1
                name = _("Nepojmenovan� profil") + " <%d>" % i
            self.COMMAND_SAVE_NEW_PROFILE.invoke(name=name)
            
    def data(self):
        """Return a new instance of the data object used by the form.

        The instance will have the data select initialized with the current
        profile parameters (filter condition, sorting etc).  This is often
        practical within application defined procedures, which retrieve this
        data object through the 'RecordForm.Record.data()' method.

        """
        data = super(LookupForm, self).data()
        self._init_data_select(data)
        return data
            
    def condition(self):
        """Vra� specifikaci aktu�ln� podm�nky v�b�ru dat.

        Podm�nka je vr�cena v�podob� po�adovan� argumentem 'condition'
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
    """Formul�� schopn� n�jak�m zp�sobem zobrazit aktu�ln� z�znam."""

    CALL_SELECTION = 'CALL_SELECTION'
    """Konstanta callbacku v�b�ru (zm�ny aktu�ln�ho) z�znamu.

    Argumentem callbackov� funkce je nov� vybran� z�znam jako instance
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
        self._row = self.record(self._data_row(select_row), prefill=prefill, new=_new,
                                singleline=_singleline)

    def _signal_update(self):
        pass

    def _select_columns(self):
        return None

    def _find_row_by_number(self, row_number):
        # row_number starts with�0
        data = self._data
        def dbop():
            data.rewind()
            data.skip(row_number)
            return data.fetchone()
        success, row = db_operation(dbop)
        self._init_select(async_count=True)
        if not success or not row:
            return None
        else:
            return row
    
    def _find_row_by_values(self, cols, values):
        """Vra� datov� ��dek odpov�daj�c� dan�m hodnot�m.

        Argumenty:

          cols -- sekvence n�zv� sloupc�, kter� maj� b�t prohled�v�ny.
          values -- sekvence hodnot sloupc� jako instanc� 'pytis.data.Value' v
            po�ad� odpov�daj�c�m 'cols'.

        Pro ob� sekvence plat�, �e pokud jsou jednoprvkov�, mohou b�t hodnoty
        p�ed�ny i p��mo, bez obalen� do sekven�n�ho typu.

        """
        cols = xtuple(cols)
        values = xtuple(values)
        assert len(cols) == len(values)
        cond = pytis.data.AND(*[pytis.data.EQ(c,v) for c,v in zip(cols, values)])
        condition = pytis.data.AND(cond, self._current_condition())
        data = self._data
        def dbop(condition):            
            data.rewind()
            n = data.select(condition, columns=self._select_columns(),
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
        """Vra� ��slo ��dku odpov�daj�c� dan� instanci 'pytis.data.Row'.

        Pokud odpov�da�c� ��dek nen� nalezen, vra� None.

        """
        data = self._data
        key = data.key()[0].id()
        def dbop():
            data.rewind()
            return data.search(pytis.data.EQ(key, row[key]), transaction=self._transaction,
                               arguments=self._current_arguments())
        success, result = db_operation(dbop)
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
            data_row = the_row.original_row(empty_as_none=True)
            if data_row is None:
                data_row = the_row.row()
            return data_row.columns([c.id() for c in self._data.key()])
        return None
    
    def _current_column_id(self):
        return None
    
    def _new_form_kwargs(self):
        return {}

    def _lock_record(self, key):
        success, locked = db_operation(self._data.lock_row, key, transaction=self._transaction)
        if success and locked != None:
            log(EVENT, 'Record is locked')
            run_dialog(Message, _("Z�znam je zam�en"))
            return False
        else:
            return True

    def _check_record(self, row):
        # Prove� kontrolu integrity dan� instance PresentedRow.
        for check in self._view.check():
            result = check(row)
            if result is not None:
                if is_sequence(result):
                    failed_id, msg = result
                    message(msg)
                else:
                    failed_id = result
                    # TODO: T�m bychom p�epsali zpr�vu nastavenou uvnit�
                    # 'check()'.  Pokud ale ��dn� zpr�va nebyla nastavena,
                    # u�ivatel netu��...
                    #message(_("Kontrola integrity selhala!"))
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
            rdata = [id_value for id_value in rdata if row.field_changed(id_value[0])]
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
        # Pokud je formul�� sou��st� du�ln�ho formul��e, vra� jej, jinak None.
        top = top_window()
        if isinstance(top, DualForm) and self in (top.active_form(), top.inactive_form()):
            return top
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
            dual = self._dualform()
            if dual.active_form() is self:
                form = dual.inactive_form()
            else:
                form = dual.active_form()
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
    
    def _cmd_new_record(self, copy=False):
        if not self.check_permission(pytis.data.Permission.INSERT, quiet=False):
            return False
        import copy as copy_
        prefill = self._prefill and copy_.copy(self._prefill) or {}
        if copy:
            prefill.update(self._row_copy_prefill(self.current_row()))
        result = new_record(self._name, prefill=prefill)
        if result:
            if not self.select_row(result.row(), quiet=True):
                msg = _("Vlo�en� z�znam se neobjevil v aktu�ln�m n�hledu.")
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
            # TODO: _signal_update vyvol� refresh.  To je tu jen pro p��pad, �e
            # byla u�ivatelsk� procedura o�et�ena jinak ne� vyvol�n�m
            # formul��e.  Proto�e to samo u� je hack, tak a� si rad�ji tak�
            # tv�rce prov�d� refresh s�m, proto�e tady je vol�n ve v�ech
            # ostatn�ch p��padech zbyte�n� a zdr�uje.
            self._signal_update()
        else:
            name = self._name
            redirect = self._view.redirect()
            if redirect is not None:
                redirected_name = redirect(row)
                if redirected_name is not None:
                    assert isinstance(redirected_name, str)
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
        if action.secondary_context() is not None and not self._dualform():
            return False
        if not pytis.data.is_in_groups(action.access_groups()):
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
        log(EVENT, 'Vyvol�v�m handler kontextov� akce.', (args, kwargs))
        apply(action.handler(), args, kwargs)
        # Hack: Pokud jsme sou��st� du�ln�ho formul��e, chceme refreshnout cel�
        # dualform.  Jinak refreshujeme jen sebe sama.
        dual = self._dualform()
        if dual:
            dual.refresh()
        else:
            self.refresh()
        return True

    def _cmd_import_interactive(self):
        if not self._data.permitted(None, pytis.data.Permission.INSERT):
            msg = _("Nem�te pr�va pro vkl�d�n� z�znam� do t�to tabulky.")
            message(msg, beep_=True)
            return False
        msg = _("Nejprve vyberte soubor obsahuj�c� importovan� data. "
                "Pot� budete moci zkontrolovat a potvrdit ka�d� z�znam.\n\n"
                "*Form�t vstupn�ho souboru:*\n\n"
                "Ka�d� ��dek obsahuje seznam hodnot odd�len�ch zvolen�m "
                "znakem, nebo skupinou znak� (vypl�te n�e). "
                "Tabel�tor zapi�te jako ='\\t'=.\n\n"
                "Prvn� ��dek obsahuje identifik�tory sloupc� a ur�uje tedy "
                "v�znam a po�ad� hodnot v n�sleduj�c�ch (datov�ch) ��dc�ch.\n\n"
                "Identifik�tory jednotliv�ch sloupc� jsou n�sleduj�c�:\n\n" + \
                "\n".join(["|*%s*|=%s=|" % (c.column_label(), c.id()) for c in
                           [self._view.field(id)
                            for id in self._view.layout().order()]]))
        separator = run_dialog(InputDialog, 
                               title=_("Hromadn� vkl�d�n� dat"),
                               report=msg, report_format=TextFormat.WIKI,
                               prompt="Odd�lova�", value='|')
        if not separator:
            if separator is not None:
                message(_("Nebyl zad�n odd�lova�."), beep_=True)
            return False
        separator = separator.replace('\\t', '\t')
        while 1:
            filename = run_dialog(FileDialog)
            if filename is None:
                message(_("Nebyl zad�n soubor. Proces ukon�en."), beep_=True)
                return False
            try:
                fh = open(filename)
            except IOError, e:
                msg = _("Nepoda�ilo se otev��t soubor '%s': %s")
                run_dialog(Error, msg % (filename, str(e)))
                continue
            break
        try:
            columns = [str(id.strip()) for id in fh.readline().split(separator)]
            for id in columns:
                if not self._row.has_key(id):
                    run_dialog(Error, _("Nezn�m� sloupec: %s") % id)
                    return False
            types = [self._row.type(id) for id in columns]
            line_number = 1
            data = []
            for line in fh:
                line_number += 1
                values = line.rstrip('\r\n').split(separator)
                if len(values) != len(columns):
                    msg = _("Chyba dat na ��dku %d:\n"
                            "Po�et hodnot neodpov�d� po�tu sloupc�.")
                    run_dialog(Error, msg % line_number)
                    return False
                row_data = []
                for id, type, val in zip(columns, types, values):
                    value, error = type.validate(val, transaction=self._transaction)
                    if error:
                        msg = _("Chyba dat na ��dku %d, sloupec '%s':\n%s") % \
                              (line_number, id, error.message())
                        run_dialog(Error, msg)
                        return False
                    assert value.type() == type, (value.type(), type)
                    row_data.append((id, value))
                data.append(pytis.data.Row(row_data))
        finally:
            fh.close()
        new_record(self._name, prefill=self._prefill, inserted_data=data)
            
    # Public methods

    def record(self, row, **kwargs):
        """Create a new `RecordForm.Record' instance bound to this form."""
        fields = self._view.fields()
        data = self._create_data_object()
        return self.Record(self, fields, data, row, transaction=self._transaction, **kwargs)
    
    def select_row(self, position, quiet=False):
        """Vyber ��dek dle 'position'.

        Argument 'position' m��e m�t n�kterou z�n�sleduj�c�ch hodnot:
        
          None -- nebude vybr�n ��dn� ��dek.
          Nez�porn� integer -- bude vybr�n ��dek p��slu�n�ho po�ad�, p�i�em�
            ��dky jsou ��slov�ny od�0.
          Datov� kl�� -- bude vybr�n ��dek s�t�mto kl��em, kter�m je instance
            t��dy 'pytis.data.Value' nebo jejich tuple.
          Slovn�k hodnot -- bude vybr�n prvn� nalezen� ��dek obsahuj�c�
            hodnoty slovn�ku (instance 'pytis.data.Value') v sloupc�ch ur�en�ch
            kl��i slovn�ku.
          Instance t��dy 'pytis.data.Row' -- bude p�eveden na datov� kl�� a
            zobrazen odpov�daj�c� ��dek.  Instance mus� b�t kompatibiln�
            s�datov�m objektem formul��e.
        
        Pokud takov� z�znam neexistuje, zobraz chybov� dialog.  Argumentem
        'quiet' lze zobrazen� chybov�ho dialogu potla�it.  T�m lze nenalezen�
        ��dku ti�e ignorovat, nebo o�et�it vlastn�m zp�sobem na z�klad�
        n�vratov� hodnoty.

        V�b�rem je my�lena akce relevantn� pro dan� typ formul��e (odvozen�
        t��dy).  Tedy nap��klad vysv�cen� ��dku v tabulce, zobrazen� z�znamu v
        n�hledov�m formul��i apod.

        Vrac�: Pravdu, pokud byl z�znam �sp�n� nalezen a vybr�n, nepravdu v
        opa�n�m p��pad�.
        
        """
        row = self._data_row(position)
        if (not quiet and
            position is not None and
            (not is_sequence(position) or len(position) != 1 or position[0].value() is not None) and
            row is None):
            run_dialog(Warning, _("Z�znam nenalezen"))
            return False
        return self._select_row(row, quiet=quiet)

    def current_row(self):
        """Vra� instanci PresentedRow pr�v� aktivn�ho ��dku.

        Nen�-li vybr�n ��dn� ��dek, vra� 'None'.

        """
        return self._row

    def selected_rows(self):
        return ()

    def current_key(self):
        """Vra� kl�� aktu�ln� vybran�ho ��dku.

        Vrac�: Sekvenci instanc� t��dy 'pytis.data.Value' nebo 'None', pokud
        nen� vybr�n ��dn� ��dek.

        """
        return self._current_key()

    def readonly(self):
        return False

    def prefill(self):
        """Vra� data pro p�edvypln�n� nov�ho z�znamu."""
        return self._prefill
    

### Edita�n� formul��


class EditForm(RecordForm, TitledForm, Refreshable):
    """Formul�� pro editaci v�ech vlastnost� jednoho z�znamu.

    Formul�� je vytvo�en poskl�d�n�m jednotliv�ch vstupn�ch pol��ek dan�ch
    specifikac� do m��ky.  Pole mohou b�t r�zn� seskupov�na a jejich rozlo�en�
    je ur�eno specifika�n� t��dou 'LayoutSpec' resp. 'GroupSpec'.

    Ka�d� vstupn� pole je reprezentov�no objektem t��dy 'InputField'.  To se
    star� o interakci s u�ivatelem, validaci vstupn�ch dat apod.

    Formul�� m��e slou�it jak k�prohl�en� �i editaci st�vaj�c�ch dat, tak
    i�k�vytv��en� nov�ch z�znam� (viz argument konstruktoru 'mode').
 
    """

    _LOG_STATISTICS = False

    MODE_INSERT = 'MODE_INSERT'
    """M�d formul��e pro vkl�d�n� nov�ch z�znam�."""
    MODE_EDIT = 'MODE_EDIT'
    """M�d formul��e pro editaci st�vaj�c�ch z�znam�."""
    MODE_VIEW = 'MODE_VIEW'
    """M�d formul��e pro zobrazen� z�znam� bez mo�nosti editace."""
    
    def __init__(self, *args, **kwargs):
        super(EditForm, self).__init__(*args, **kwargs)
        # Calculate the ideal total form size.  We first count the size of the sizer without the
        # form panel (that's why its hidden and shown below) and then add the manually computed
        # size of the panel to the result.  The size of the forma panel computed by wx Windows is
        # totaly useless.  It doesn't reflect the form content at all.
        panel = self._form_controls_window
        panel.Show(False)
        size = self.GetSizer().CalcMin()
        panel.Show(True)
        if isinstance(panel, wx.ScrolledWindow):
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

    def _init_attributes(self, mode=MODE_EDIT, focus_field=None, **kwargs):
        """Process constructor keyword arguments and initialize the attributes.

        Arguments:

          mode -- one of the 'MODE_*' constants.  Determines whether the form is primarily for
            viewing, editation or creation of records.
          focus_field -- identifier of the field which should be activated for user input on form
            startup.  If None, the first field is the default.  It is also possible to pass a
            function of one argument -- the PresentedRow instance representing the current record.
            This function must return a field identifier or None.
          kwargs -- arguments passed to the parent class
         
        """
        assert mode in (self.MODE_EDIT, self.MODE_INSERT, self.MODE_VIEW)
        new = mode == self.MODE_INSERT
        super_(EditForm)._init_attributes(self, _new=new, **kwargs)
        self._mode = mode
        self._focus_field = focus_field or self._view.focus_field()
        # Other attributes
        self._fields = []

    def _set_focus_field(self, event=None):
        """Inicalizuj dialog nastaven�m hodnot pol��ek."""
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
            if is_string(item):
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
                        and is_string(item.items()[0]) \
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
            # p�idej zbyl� sled pol��ek (pokud n�jak� byl)
            sizer.Add(self._pack_fields(parent, pack, space, gap),
                      0, wx.ALIGN_TOP|border_style, border)
        # pokud m� skupina or�mov�n�, p�id�me ji je�t� do sizeru s horn�m
        # odsazen�m, jinak je horn� odsazen� p��li� mal�.
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

    def _refresh(self, when=None):
        self.Refresh()

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
                self._row.has_key(fid) and not self._row[fid].value()):
                self._row[fid] = self._row.original_row()[fid]
            elif self._mode == self.MODE_INSERT or self._row.field_changed(fid):
                if f.enabled() and not f.validate():
                    f.set_focus()
                    return False
        # Ov��en� integrity z�znamu (funkce check).
        failed_id = self._check_record(self._row)
        if failed_id:
            f = self._field(failed_id)
            if f:
                f.set_focus()
            else:
                log(OPERATIONAL, "Unknown field returned by check():", failed_id)
            return False
        transaction = self._transaction
        # Vytvo�en� datov�ho ��dku.
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
            op, args = self._data.insert, (rdata,)
        elif self._mode == self.MODE_EDIT:
            log(ACTION, 'Updating record...')
            op, args = self._data.update, (self._current_key(), rdata)
        else:
            raise ProgramError("Can't commit in this mode:", self._mode)
        # Proveden� operace
        if transaction is not None:
            success, result = db_op(transaction.set_point, ('commitform',),
                                    in_transaction=True, quiet=True)
        else:
            success = True
        if success:
            if op is not None:
                success, result = db_op(op, args, dict(transaction=transaction),
                                        in_transaction=(transaction is not None))
            else:
                success, result = True, (None, True)
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
            if transaction is not None:
                success, __ = db_op(transaction.cut, ('commitform',), in_transaction=True,
                                    quiet=True)
            else:
                success = True
            if success:
                msg = _("Ulo�en� z�znamu se nezda�ilo")
            else:
                msg = _("Transakce p�eru�ena, nelze pokra�ovat")
            if type(result) == type(()) and \
               isinstance(result[0], (str, unicode)):
                msg = "%s\n\n%s" % (result[0], msg)
            run_dialog(Error, msg)
            return False

    def _exit_check(self):
        if self.changed():
            q = _("Data byla zm�n�na a nebyla ulo�ena!") + "\n" + \
                _("Opravdu chcete uzav��t formul��?")
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
            # Vygeneruj ud�lost navigace mezi pol��ky.
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
    """Stejn� jako 'EditForm', av�ak v�popup podob�."""

    DESCR = _("edita�n� formul��")
    
    def __init__(self, parent, *args, **kwargs):
        EditForm.__init__(self, self._popup_frame(parent), *args, **kwargs)
        if self._inserted_data is not None:
            self._load_next_row()
        # Set the popup window size according to the ideal form size limited to
        # the screen size.  If the form size exceeds the screen, scrollbars
        # will appear.
        size = wx.Size(*self.size())
        size.DecTo(wx.GetDisplaySize() - wx.Size(50, 80))
        self.SetClientSize(size)

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
        spec = (('message', None, _("Oznamovac� oblast")),)
        if self._inserted_data is not None:
            spec += (('progress', 9, _("Ukazatel pozice hromadn�ho vkl�d�n�")),)
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
                run_dialog(Message, _("V�echny z�znamy byly zpracov�ny."))
                self._inserted_data = None
        self._set_focus_field()

    def _exit_check(self):
        i = self._inserted_data_pointer
        data = self._inserted_data
        if data is not None and i <= len(data):
            msg = _("Je�t� nebyly zpracov�ny v�echny ��dky vstupn�ch dat.\n"
                    "Chcete opravdu ukon�it vkl�d�n�?")
            if not run_dialog(Question, msg, default=False):
                return False
        return super(PopupEditForm, self)._exit_check()

    def _on_skip_button(self, event):
        i = self._inserted_data_pointer
        if self._inserted_data is None:
            message(_("Nen� dal�� z�znam"), beep_=True)
        else:
            message(_("Z�znam %d/%d p�esko�en") % (i, len(self._inserted_data)))
            self._load_next_row()
    
    def _buttons(self):
        buttons = (dict(id=wx.ID_OK,
                        tooltip=_("Ulo�it z�znam a uzav��t formul��"),
                        command=self.COMMAND_COMMIT_RECORD()),
                   dict(id=wx.ID_CANCEL,
                        tooltip=_("Uzav��t formul�� bez ulo�en� dat"),
                        command=self.COMMAND_LEAVE_FORM()))
        if self._mode == self.MODE_INSERT and self._multi_insert:
            buttons += (dict(id=wx.ID_FORWARD, label=_("Dal��"), #icon=wx.ART_GO_FORWARD, 
                             tooltip=_("Ulo�it z�znam a reinicializovat formul��"
                                       " pro vlo�en� dal��ho z�znamu"),
                             command=self.COMMAND_COMMIT_RECORD(next=True)),)
        if self._inserted_data is not None:
            buttons += (dict(label=_("P�esko�it"),
                             tooltip=_("P�esko�it tento z�znam bez ulo�en�"),
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

    def _can_commit_record(self, close=True, next=False):
        if next and (self._mode != self.MODE_INSERT or not self._multi_insert):
            return False
        return super(PopupEditForm, self)._can_commit_record()
    
    def _cmd_commit_record(self, close=True, next=False):
        result = super(PopupEditForm, self)._cmd_commit_record(close=close and not next)
        if result and next:
            message(_("Z�znam ulo�en"))
            self._load_next_row()
        return result
        
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
        if self._status_fields.has_key(field):
            self._status_fields[field].SetLabel(unicode(message or ''))
            return True
        else:
            return False

    def set_row(self, row):        
        if self._transaction is None:
             self._transaction = pytis.data.DBTransactionDefault(config.dbconnection) 
        super(PopupEditForm, self).set_row(row)
        

class PopupInsertForm(PopupEditForm):
    
    DESCR = _("vkl�dac� formul��")
    
    def _init_attributes(self, **kwargs):
        super_(PopupInsertForm)._init_attributes(self, mode=EditForm.MODE_INSERT, **kwargs)
        
        
class ShowForm(EditForm):
    """Formul�� pro zobrazen� n�hledu.

    Layout je stejn� jako u edita�n�ho formul��e (resp. 'EditForm'),
    pouze titulek m� stejn� vzhled, jako titulek formul��� typu 'ListForm'.
    Ur�en pro zobrazen� v du�ln�m formul��i.

    """

    DESCR = _("n�hledov� formul��")

    def _init_attributes(self, mode=EditForm.MODE_VIEW, select_row=0,**kwargs):
        super_(ShowForm)._init_attributes(self, mode=mode, select_row=select_row, **kwargs)
        
    def changed(self):
        # Since the row is not reset when the current record changes, it would report a change...
        return False

        
class BrowsableShowForm(ShowForm):
    """Listovac� formul�� pro zobrazen� n�hledu.

    Formul�� je needitovateln�, ale umo��uje pohyb po z�znamech tabulky, nad
    kterou je vytvo�en, vyhled�v�n� atd.  Z u�ivatelsk�ho hlediska jde v
    podstat� o redukci prohl�ec�ch mo�nost� formul��e typu 'BrowseForm' na
    jeden z�znam zobrazen� v Layoutu edita�n�ho formul��e.
    
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
                message(_("Posledn� z�znam"), beep_=True)
                return
        else:
            if row_number == 0:
                message(_("Prvn� z�znam"), beep_=True)
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
                     

