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
    ACT_FORM = 'ACT_FORM'
    """Aktiva�n� konstanta formul��e."""
    
    ACTIVATIONS = Window.ACTIVATIONS + [ACT_FORM]
    """Seznam aktiva�n�ch kategori� pro tuto t��du."""

    _STATUS_FIELDS = ()
    _DESCR = None

    def get_command_handler_instance(cls, application):
        return application.current_form()
    get_command_handler_instance = classmethod(get_command_handler_instance)

    def __init__(self, parent, resolver, name, guardian=None, **kwargs):
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
          kwargs -- viz n�e.

        Resolver je pou�it k z�sk�n� datov� a prezenta�n� specifikace a
        n�sledn�mu vytvo�en� datov�ho objektu. Ten je potom spole�n� s
        prezenta�n� specifikac� ulo�en v podob� atribut� vytv��en� instance.

        Odkaz na resolver samotn� je tak� zapamatov�n pro pozd�j�� pou�it�
        (vytv��en� dal��ch formul���).

          
        Inicializace je rozd�lena do n�kolika krok�.  Nejprve jsou zpracov�n�
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
        Pokud n�jak� zbydou, vyvol� b�zov� t��da v�jimku 'AssertionError'.

        Teprve po zpravov�n� argument� konstruktoru a inicializaci atribut� je
        vytv��en vlastn� obsah formul��e (viz. '_create_form()').  Toto by m�lo
        b�t dodr�ov�no i v odvozen�ch t��d�ch.
        
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
            log(OPERATIONAL, 'Chyba z�resolveru', format_traceback())
            throw('form-init-error')
        log(EVENT, 'Specifikace na�teny za %.3fs' % (time.time() - start_time)) 
        self._init_attributes(**kwargs)
        self._result = None
        add_menu(Menu(_("Tisk"), (Form._print_menu,),
                      activation=Form.ACT_FORM), form=self)
        start_time = time.time()
        self._create_form()
        log(EVENT, 'Formul�� sestaven za %.3fs' % (time.time() - start_time))
        wx_callback(wx.EVT_CLOSE, self._parent, self._on_parent_close)

    def _init_attributes(self, spec_args={}):
        """Zpracuj kl��ov� argumenty konstruktoru a inicializuj atributy.
        
        Argumenty:
        
          kwargs -- kl��ov� argumenty konstruktoru (viz dokumentace metody
            '__init__()').

        
        Tato metoda je vol�na po z�kladn� inicializaci instance (p�edev��m
        na�ten� specifikace a inicializaci datov�ho objektu.  Metody
        vytv��ej�c� konkr�tn� prvky u�ivatelsk�ho rozhran� formul��e (nap��klad
        '_create_form()'), jsou v�ak vol�ny a� pot�.  Zde by m�ly b�t p�edev��m
        zpracov�ny v�echny kl��ov� argumenty konstruktoru (viz dokumentace
        metody '__init__()' a inicializov�ny atributy instance.

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
        """Handler ud�losti uzav�en� rodi�ovsk�ho okna formul��e.

        Tato metoda by m�la b�t p�edefinov�na, pokud chce dan� typ formul��e
        reagovat na uzav�en� rodi�ovsk�ho okna. Typick� vyu�it� je pro popup
        formul��e.

        Pokud odvozen� t��da p�edefinuje tuto metodu a ta za ur�it�ch okolnost�
        nezavol� 'event.Skip()', nebude zpracov�n� ud�losti dokon�eno a
        rodi�ovsk� okno tedy nebude uzav�eno.
        """
        if __debug__: log(DEBUG, "Vol�no Form._on_parent_close()")
        event.Skip()
        return False

    def _print_menu(self):
        # Vra� tuple polo�ek tiskov�ho menu.
        name = self._name
        try:
            print_spec = self._resolver.get(name, 'print_spec')
        except ResolverSpecError:
            print_spec = None
        if not print_spec:
            print_spec = ((_("Implicitn�"), os.path.join('output', name)),)
        return [MItem(title, command=Form.COMMAND_PRINT,
                      args={'print_spec_path': path})
                for title, path in print_spec]

    def __str__(self):
        return '<%s for "%s">' % (self.__class__.__name__, self._name)

    def __repr__(self):
        return str(self)
    
    # Ve�ejn� metody
    
    def name(self):
        """Vra� n�zev specifikace formul��e."""
        return self._name

    def on_command(self, command, **kwargs):
        """Zpracuj 'command'.

        Argumenty:

          command -- instance t��dy 'Command'

        Metoda mus� p��kaz bu� sama o�et�it, nebo jek vypropagovat do vnit�n�ho
        prvku formul��e, pokud takov� je a m� metodu stejn�ho n�zvu jako tato.

        Vrac�: Pravdu, pr�v� kdy� metoda nebo j� volan� metoda p��kaz
        zpracovala.
        
        V t�to t��d� metoda ned�l� nic a vrac� False.
        
        """
        return False

    def descr(self):
        """Vra� textov� popis typu formul��e jako �et�zec."""
        if self._DESCR is not None:
            return self._DESCR
        else:
            return self.__class__.__name__
        
    def title(self):
        """Vra� titulek ze specifikace formul��e jako �et�zec."""
        return self._view.title()

    def guardian(self):
        """Vra� guardian zadan� v�konstruktoru (nebo parent)."""
        return self._guardian

    def check_permission(self, perm, quiet=True):
        """Vra� pravdu, pokud m� u�ivatel dan� pr�va k datov�mu objektu.

        Argumentem je konstanta  t��dy 'pytis.data.Permission::'.

        """
        VIEW   = pytis.data.Permission.VIEW
        INSERT = pytis.data.Permission.INSERT
        UPDATE = pytis.data.Permission.UPDATE
        DELETE = pytis.data.Permission.DELETE
        EXPORT = pytis.data.Permission.EXPORT
        if perm == DELETE:
            result = self._data.accessible(None, perm)
        else:
            for col in self._data.columns():
                if self._data.accessible(col.id(), perm):
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

        M�-li formul�� stavovou ��dku a v n� pole `id' zobraz v n�m danou
        zpr�vu a vra� pravdu.  V opa�n�m p��pad� vra� nepravdu.

        """
        return False

    def show_popup_menu(self):
        """Zobraz kontextov� menu pr�v� aktivn�ho prvku, pokud to umo��uje. """
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
    
    def refresh(self, when=None):
        """Aktualizuj data formul��e z�datov�ho zdroje.

        P�ekresli data ve formul��i v�okam�iku dan�m argumentem 'when'.

        Argumenty:

          when -- ur�uje, zda a kdy m� b�t aktualizace provedena, mus� to b�t
            jedna z�'DOIT_*' konstant t��dy.  Implicitn� hodnota je
            'DOIT_AFTEREDIT', je-li 'reset' 'None', 'DOIT_IMMEDIATELY' jinak.

        Vrac�: Pravdu, pr�v� kdy� byla aktualizace provedena.

        V�t�to t��d� metoda ned�l� nic, mus� b�t v�potomkovi p�edefinov�na.
        
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
            style = wx.DIALOG_MODAL|wx.DEFAULT_DIALOG_STYLE
            frame = wx.Dialog(parent, style=style)
            self._popup_frame_ = frame
        return frame    

    def _leave_form(self):
        self._popup_frame_.Close() # t�m se autom. zavol� _on_parent_close()

    def _on_parent_close(self, event):
        if hasattr(self, 'exit_check') and not self.exit_check():
            event.Veto()
            return True
        event.Skip()
        self._parent.EndModal(0)
        return False

    def run(self):
        """Zobraz formul�� jako mod�ln� dialog."""
        unlock_callbacks()
        self._parent.SetTitle(self.title())
        self._parent.SetClientSize(self.GetSize())
        self._parent.ShowModal()
        return self._result


class TitledForm:
    """P�im�ch�vac� t��da pro formul��e s�titulkem.
    
    Lze vyu��t bu�to pouze metodu '_create_caption()', kter� vytv��� samotn�
    text titulku, nebo metodu '_create_title_bar()', kter� p�id�v� 3d panel.

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
        """Vytvo� 3d panel s nadpisem formul��e."""
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
    """Formul�� schopn� n�jak�m zp�sobem zobrazit aktu�ln� z�znam."""

    CALL_SELECTION = 'CALL_SELECTION'
    """Konstanta callbacku zm�ny z�znamu."""
    CALL_EDIT_RECORD = 'CALL_EDIT_RECORD'
    """Vol�no p�i po�adavku na editaci akt. z�znamu."""
    CALL_NEW_RECORD = 'CALL_NEW_RECORD'
    """Vol�no p�i po�adavku na vytvo�en� nov�ho z�znamu.

    M��e m�t jeden nepovinn� argument.  Pokud je pravdiv�, bude nov� z�znam
    p�edvypln�n zkop�rov�n�m dat aktu�ln�ho ��dku.

    """

    def __init__(self, *args, **kwargs):
        super_(RecordForm).__init__(self, *args, **kwargs)
        self.set_callback(self.CALL_NEW_RECORD,  self._on_new_record)
        self.set_callback(self.CALL_EDIT_RECORD,
                          lambda k: self._on_edit_record(k)),

    def _init_attributes(self, prefill=None, **kwargs):
        """Zpracuj kl��ov� argumenty konstruktoru a inicializuj atributy.

        Argumenty:

          prefill -- slovn�k �et�zcov�ch (u�ivatelsk�ch) hodnot, kter� maj� b�t
            p�edvypln�ny p�i inicializaci formul��e.
            
          kwargs -- argumenty p�edan� p�edkovi

        """
        super_(RecordForm)._init_attributes(self, **kwargs)
        assert prefill is None or is_dictionary(prefill)
        self._prefill = prefill
        self._row = None

    def _on_field_change(self, field_id, value=None):
        # Signalizace zm�ny hodnoty pol��ka z�_row
        pass

    def _on_editability_change(self, field_id, editable):
        # Callback zm�ny editovatelnosti pol��ka
        pass

    def _signal_update(self):
        pass

    def _find_row_by_number(self, row_number):
        # row_number za��n� od�0
        def get_it():
            data = self._data
            data.rewind()
            data.skip(row_number)
            return data.fetchone()
        success, row = db_operation(get_it)
        if not success or not row:
            return None
        else:
            return row
    
    def _find_row_by_values(self, cols, values):
        """Vra� datov� ��dek odpov�daj�c� dan�m hodnot�m.

        Arguemnty:

          cols -- sekvence n�zv� sloupc�, kter� maj� b�t prohled�v�ny.
          values -- sekvence hodnot sloupc� jako instanc� 'pytis.data.Value' v
            po�ad� odpov�daj�c�m 'cols'.

        Pro ob� sekvence plat�, �e pokud jsou jednoprvkov�, mohou b�t hodnoty
        p�ed�ny i p��mo, bez obalen� do sekven�n�ho typu.

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

    def _find_row_by_key(self, key):
        """Vra� datov� ��dek odpov�daj�c� dan�mu datov�mu kl��i."""
        if key is None:
            return None
        success, row = db_operation(lambda : self._data.row(key))
        if success and row:
            return row
        else:
            run_dialog(Error, _("Z�znam nenalezen"))
            return None

    
    def _get_row_number(self, row):
        """Vra� ��slo ��dku odpov�daj�c� dan� instanci 'pytis.data.Row'."""
        eqs = [pytis.data.EQ(c.id(), row[c.id()]) for c in self._data.key()]
        condition = pytis.data.AND(*eqs)
        data = self._data
        data.rewind()
        success, result = db_operation(lambda: data.search(condition))
        if not success:
            return None
        elif result == 0:
            return 0
        else:
            return result - 1
        
    def _select_row(self, row):
        # Napl� formul�� daty z dan�ho *datov�ho* ��dku
        raise ProgrammError("This method must be overridden.")

    def _current_key(self):
        the_row = self.current_row()
        if the_row is not None:
            kc = [c.id() for c in self._data.key()]
            try:
                return the_row.row().columns(kc)
            except KeyError:
                log(OPERATIONAL, 'Chyb� n�kter� z�kl��ov�ch sloupc�:', kc)
                run_dialog(Error, _("Chyba v�definici dat"))
        return None

    def _redirected_name(self, key):
        redirect = self._view.redirect()
        if redirect is not None:
            success, row = db_operation(lambda : self._data.row(key))
            if not success:
                raise ProgramError('Row read failure')
            name = redirect(row)
            if name is not None:
                assert isinstance(name, types.StringType)
                return name
        return None
    
    def _run_form(self, form, key):
        name = self._redirected_name(key) or self._name
        kwargs = self._new_form_kwargs()
        run_form(form, name, select_row=key, **kwargs)

    def _new_form_kwargs(self):
        return {}

    def _lock_record(self, key):
        success, locked = db_operation(lambda : self._data.lock_row(key),
                                       quiet=True)
        if success and locked != None:
            log(EVENT, 'Z�znam je zam�en', locked)
            run_dialog(Message, _("Z�znam je zam�en: %s") % locked)
            return False
        else:
            return True

    def _unlock_record(self):
        if self._data.locked_row():
            db_operation(lambda : self._data.unlock_row(), quiet=True)

    def _check_record(self, row):
        # Prove� kontrolu integrity dan� instance PresentedRow.
        error = None
        check = self._view.check()
        if check is not None:
            error = check(row)
        if error is None:
            error = row.check()
        if error is not None:
            if is_sequence(error):
                failed_id, msg = error
                message(msg)
            else:
                failed_id = error
                # TODO: T�m bychom p�epsali zpr�vu nastavenou uvnit� 'check()'.
                # Pokud ale ��dn� zpr�va nebyla nastavena, u�ivatel netu��...
                #message(_("Kontrola integrity selhala!"))
            log(EVENT, 'Kontrola integrity selhala:', failed_id)
            return failed_id
        else:
            return None

    def _record_data(self, row):
        rdata = [(f.id(), row[f.id()]) for f in row.fields()
                 if self._data.find_column(f.id()) is not None]
        return pytis.data.Row(rdata)

    def _on_new_record(self, copy=False):
        if not self.check_permission(pytis.data.Permission.INSERT, quiet=False):
            return False
        if copy:
            key = self._current_key()
        else:
            key = None
        result = new_record(self._name, key=key, prefill=self.prefill())
        if result is not None:
            self.select_row(result.row())
    
    def _on_edit_record(self, key):
        if not self.check_permission(pytis.data.Permission.UPDATE, quiet=False):
            return False
        on_edit_record = self._view.on_edit_record()
        if on_edit_record is not None:
            on_edit_record(row=self.current_row())
            # TODO: _signal_update vyvol� refresh.  To je tu jen pro p��pad, �e
            # byla u�ivatelsk� procedura o�et�ena jinak ne� vyvol�n�m
            # formul��e.  Proto�e to samo u� je hack, tak a� si rad�ji tak�
            # tv�rce prov�d� refresh s�m, proto�e tady je vol�n ve v�ech
            # ostatn�ch p��padech zbyte�n� a zdr�uje.
            self._signal_update()
        else:
            self._run_form(PopupEditForm, key)

    def _on_delete_record(self, key):
        log(EVENT, 'Pokus o�smaz�n� z�znamu:', key)
        if not self.check_permission(pytis.data.Permission.DELETE, quiet=False):
            return False
        # Implicitn� akce pro maz�n� 
        op = lambda : self._data.delete(key)
        # O�et�en� u�ivatelsk� funkce pro maz�n�
        on_delete_record = self._view.on_delete_record()
        if on_delete_record is not None:
            condition = on_delete_record(row=self.current_row())
            if condition is None:
                return True
            assert isinstance(condition, pytis.data.Operator)
            op = lambda : self._data.delete_many(condition)
        else:
            msg = _("Opravdu chcete z�znam zcela vymazat?")        
            if not run_dialog(Question, msg):
                log(EVENT, 'Maz�n� ��dku u�ivatelem zam�tnuto.')
                return False
            log(EVENT, 'Maz�n� ��dku u�ivatelem potvrzeno.')
        success, result = db_operation(op)
        if success:
            self._signal_update()
            log(ACTION, '��dek smaz�n')
            return True
        else:
            return False

    # Ve�ejn� metody
    
    def select_row(self, position):
        """Vyber ��dek dle 'position'.

        Argument 'position' m��e m�t n�kterou z�n�sleduj�c�ch hodnot:
        
          None -- nebude vybr�n ��dn� ��dek.
          Nez�porn� integer -- bude vybr�n ��dek p��slu�n�ho po�ad�, p�i�em�
            ��dky jsou ��slov�ny od�0.
          Datov� kl�� -- bude vybr�n ��dek s�t�mto kl��em, kter�m je tuple
            instanc� t��dy 'pytis.data.Value'.
          Slovn�k hodnot -- bude vybr�n prvn� nalezen� ��dek obsahuj�c�
            hodnoty slovn�ku (instance 'pytis.data.Value') v sloupc�ch ur�en�ch
            kl��i slovn�ku.
          Instance t��dy 'pytis.data.Row' -- bude p�eveden na datov� kl�� a
            zobrazen odpov�daj�c� ��dek.  Instance mus� b�t kompatibiln�
            s�datov�m objektem formul��e.
        
        Pokud takov� z�znam neexistuje, zobraz chybov� dialog a jinak nic.

        V�b�rem je my�lena akce relevantn� pro dan� typ formul��e (odvozen�
        t��dy).  Tedy nap��klad vysv�cen� ��dku v tabulce, zobrazen� z�znamu v
        n�hledov�m formul��i apod.
        
        """
        if isinstance(position, pytis.data.Row):
            row = position
        elif isinstance(position, types.IntType):
            row = self._find_row_by_number(position)
        elif isinstance(position, types.TupleType):
            row = self._find_row_by_key(position)
        elif isinstance(position, types.DictType):
            row = self._find_row_by_values(position.keys(), position.values())
        else:
            ProgramError("Invalid 'position':", position)
        self._select_row(row)

    def set_row(self, row):
        """Nastav aktu�ln� z�znam formul��e daty z instance 'PresentedRow'."""
        self._row = row
        self._run_callback(self.CALL_SELECTION, (row,))
        
    def current_row(self):
        """Vra� instanci PresentedRow pr�v� aktivn�ho ��dku.

        Nen�-li vybr�n ��dn� ��dek, vra� 'None'.

        """
        return self._row

    def current_key(self):
        """Vra� kl�� aktu�ln� vybran�ho ��dku.

        Vrac�: Sekvenci instanc� t��dy 'pytis.data.Value' nebo 'None', pokud
        nen� vybr�n ��dn� ��dek.

        """
        return self._current_key()

    def prefill(self):
        """Vra� data pro p�edvypln�n� nov�ho z�znamu."""
        return self._prefill
    
    def set_prefill(self, data):
        """Nastav data pro p�edvypln�n� nov�ho z�znamu.

        List si m��e zapamatovat hodnoty, kter� maj� b�t automaticky pou�ity
        pro p�edvypln�n� nov�ho z�znamu p�i operac�ch vlo�en� ��dku nad t�mto
        listem.  Pro argument 'data' zde plat� stejn� podm�nky, jako pro
        argument 'prefill' konstruktoru t��dy 'PresentedRow'.

        """
        self._prefill = data

    def can_delete_record(self):
        return self.check_permission(pytis.data.Permission.DELETE)
        
    def on_command(self, command, **kwargs):
        if command == RecordForm.COMMAND_DELETE_RECORD:
            key = self._current_key()
            self._on_delete_record(key)
        elif command == RecordForm.COMMAND_NEW_RECORD:
            self._run_callback(self.CALL_NEW_RECORD)
        elif command == RecordForm.COMMAND_NEW_RECORD_COPY:
            self._run_callback(self.CALL_NEW_RECORD, (True,))
        elif command == RecordForm.COMMAND_EDIT_RECORD:
            key = self.current_key()
            if key is not None:
                self._run_callback(self.CALL_EDIT_RECORD, (key,))
        else:
            return super(RecordForm, self).on_command(command, **kwargs)
        return True

        
class LookupForm(RecordForm):
    """Formul�� s�vyhled�v�n�m a t��d�n�m."""
    
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
        """Zpracuj kl��ov� argumenty konstruktoru a inicializuj atributy.

        Argumenty:

          sorting -- specifikace po��te�n�ho t��d�n� formul��e, viz argument
            'sort' metody 'pytis.data.Data.select()'
          grouping -- ???
          condition -- podm�nka v�b�ru dat, viz argument 'condition' metody
            'pytis.data.Data.select()'
          indicate_filter -- ???
          kwargs -- argumenty p�edan� konstruktoru p�edka
        
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

    def _new_form_kwargs(self):
        return dict(condition=self._lf_condition, sorting=self._lf_sorting)
    
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
            log(EVENT, 'Selh�n� datab�zov� operace')
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
        
    def _search(self, condition, direction, row_number=None,
                report_failure=True):
        self._search_adjust_data_position(row_number)
        data = self._data
        skip = data.search(condition, direction=direction)
        if skip == 0:
            log(EVENT, 'Z�znam nenalezen')
            if report_failure:
                run_dialog(Warning, _("Z�znam nenalezen"))
            result = None
        else:
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

    def _on_jump(self):
        if self._lf_select_count > 0:
            prompt = _("Z�znam ��slo (1-%s):") % (self._lf_select_count)
            number = run_dialog(InputNumeric, message=_("Skok na z�znam"),
                                prompt=prompt, min_value=1,
                                max_value=self._lf_select_count)
            if number:
                self.select_row(number.value()-1)
            #else:
            #    message(_("Neplatn� ��slo z�znamu"), beep_=True)
        
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

    def _is_searching(self):
        sd = self._lf_search_dialog
        return bool(sd and sd._condition)
            
    def can_search_next(self, **kwargs):
        return self._is_searching()

    def can_search_previous(self, **kwargs):
        return self._is_searching()
            
    def _filter(self, condition):
        self._init_select()
        self.select_row(self._row.row())

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
        """Zm�� t��d�n�.

        Argumenty:

          col -- id sloupce, podle kter�ho m� b�t seznam set��d�n, nebo
            'None' pro glob�ln� zm�ny (nap��klad vypnut� ve�ker�ho t��d�n�)
          direction -- sm�r t��d�n� (sestupn�/vzestupn�/v�bec/cyklicky).  Pokud
            je hodnotou konstanta 'LookupForm.SORTING_CYCLE_DIRECTION', bude
            t��d�n� cyklicky p�epnuto na dal�� z variant
            (sestupn�/vzestupn�/v�bec).  Hodnota dan� konstantou
            'LookupForm.SORTING_NONE' zna�� explicitn� po�adavek na zru�en�
            t��d�n�.  Jinak je o�ek�v�na jedna z konstant
            'LookupForm.SORTING_ASCENDENT' (pro sestupn� t��d�n�), nebo
            'LookupForm.SORTING_DESCENDANT' (pro vzestupn� t��d�n�).
          primary -- pr�v� kdy� je pravdiv�, bude dan� sloupec zvolen jako
            prim�rn� a *jedin�* t��d�c� sloupec.  V opa�n�m p��pad� bude pouze
            p�id�n na konec st�vaj�c�ho seznamu t��d�c�ch sloupc�.
        
        P�i nejednozna�n� kombinaci argument� 'col' a 'direction' je
        automaticky vyvol�n dialog pro v�b�r t��d�c�ch krit�ri�.
        
        """
        # TODO: Toto cel� je bastl, nutno �asem pro�istit.
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
                    message(_("Podle tohoto sloupce nelze t��dit"),
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
            self.select_row(self._row.row())
        return sorting
    
    def can_sort_column(self, col=None, direction=None, primary=False):
        sorting = xtuple(self._lf_sorting)
        if direction == self.SORTING_NONE:
            return sorting and (col is None or col in [c for c,d in sorting])
        elif direction is not None and col is not None and sorting:
            if primary:
                return (col, direction) != sorting[0]
            else:
                return (col, direction) not in sorting and col != sorting[0][0]
        else:
            return True
        
    # wx metody

    def Close(self):
        super_(LookupForm).Close(self)
        if self._lf_search_dialog:
            self._lf_search_dialog = None
        if self._lf_filter_dialog:
            self._lf_filter_dialog = None
    
    # Ve�ejn� metody

    def condition(self):
        """Vra� specifikaci aktu�ln� podm�nky v�b�ru dat.

        Podm�nka je vr�cena v�podob� po�adovan� argumentem 'condition'
        metody 'pytis.data.Data.select()'.

        """
        return self._lf_condition
    
    def sorting(self):
        """Vra� specifikaci aktu�ln�ho t��d�n� seznamu.

        Podm�nka je vr�cena v�podob� po�adovan� argumentem 'sort'
        metody 'pytis.data.Data.select()'.

        """
        return self._lf_sorting

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

        

### Edita�n� formul��


class EditForm(LookupForm, TitledForm):
    """Formul�� pro editaci v�ech vlastnost� jednoho z�znamu.

    Formul�� je vytvo�en poskl�d�n�m jednotliv�ch vstupn�ch pol��ek dan�ch
    specifikac� do m��ky.  Pole mohou b�t r�zn� seskupov�na a jejich rozlo�en�
    je ur�eno specifika�n� t��dou 'LayoutSpec' resp. 'GroupSpec'.

    Ka�d� vstupn� pole je reprezentov�no objektem t��dy 'InputField'.  To se
    star� o interakci s u�ivatelem, validaci vstupn�ch dat apod.

    Formul�� m��e slou�it jak k�prohl�en� �i editaci st�vaj�c�ch dat, tak
    i�k�vytv��en� nov�ch z�znam� (viz argument konstruktoru 'mode').
 
    """

    MODE_INSERT = 'MODE_INSERT'
    """M�d formul��e pro vkl�d�n� nov�ch z�znam�."""
    MODE_EDIT = 'MODE_EDIT'
    """M�d formul��e pro editaci st�vaj�c�ch z�znam�."""
    MODE_VIEW = 'MODE_VIEW'
    """M�d formul��e pro zobrazen� z�znam� bez mo�nosti editace."""
    
    def __init__(self, *args, **kwargs):
        super_(EditForm).__init__(self, *args, **kwargs)
        self._size = self.GetSize() # Remember the original size.
        if self._mode == self.MODE_INSERT:
            self._select_row(None)
        if isinstance(self._parent, wx.Dialog):
            wx_callback(wx.EVT_INIT_DIALOG, self._parent, self._init_fields)
        else:
            self._init_fields()
            

    def _init_attributes(self, mode=MODE_EDIT, focus_field=None, **kwargs):
        """Zpracuj kl��ov� argumenty konstruktoru a inicializuj atributy.

        Argumenty:

          mode -- jedna z 'MODE_*' konstant t��dy.  Ur�uje, zda formul�� slou��
            k prohl�en�, editaci �i vytv��en� z�znam�.

          focus_field -- id pol��ka, kter� m� b�t vybr�no jako aktivn� pro
            u�ivatelsk� vstup, p��padn� funkce jednoho argumentu, kter�m je
            aktu�ln� PresentedRow, kter� vrac� id pol��ka pro u�ivatelsk�
            vstup.

          
          kwargs -- argumenty p�edan� konstruktoru prvn�ho p�edka

        """
        super_(EditForm)._init_attributes(self, **kwargs)
        assert mode in (self.MODE_EDIT, self.MODE_INSERT, self.MODE_VIEW)
        #assert focus_field in [f.id() for f in self._view.fields()]
        self._mode = mode
        self._focus_field = focus_field or self._view.focus_field()
        # Other attributes
        self._fields = []

    def _init_fields(self, event=None):
        """Inicalizuj dialog nastaven�m hodnot pol��ek."""
        for f in self._fields:
            if self._mode == self.MODE_VIEW:
                f.disable(change_appearance=False)
            else:
                f.enable()
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
        if self._mode == self.MODE_INSERT:
            permission = pytis.data.Permission.INSERT
        elif self._mode == self.MODE_EDIT:
            permission = pytis.data.Permission.UPDATE
        else:
            permission = pytis.data.Permission.VIEW
        data_columns = [c.id() for c in self._data.columns()]
        for id in self._view.layout().order():
            spec = self._view.field(id)
            if id in data_columns:
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
        """Vytvo� skupinu vstupn�ch pol��ek podle specifikace.

        Argumenty:

          group -- instance 'GroupSpec', kter� m� b�t zpracov�na.

        Ka�dou posloupnost za sebou n�sleduj�c�ch pol��ek seskup� pod sebe
        a pro ka�dou vno�enou skupinu pol��ek zavol� sebe sama rekurzivn�.
        V�sledek potom poskl�d� do instance 'wx.BoxSizer', kterou vytvo��.

        Specifikace skupiny ovliv�uje zp�sob seskupen�:
        horizont�ln�/vertik�ln�, mezery mezi pol��ky, skupinami
        atd. Viz. dokuewntace t��dy 'GroupSpec'

        Vrac�: 'wx.BoxSizer' napln�n� pol��ky a vno�en�mi skupinami.

        """
        orientation = orientation2wx(group.orientation())
        if group.label() is not None:
            box = wx.StaticBox(self, -1, group.label())
            sizer = wx.StaticBoxSizer(box, orientation)
        else:
            sizer = wx.BoxSizer(orientation)
        # ka�d� souvisl� sled pol��ek ukl�d�m do pole a teprve nakonec je
        # poskl�d�m metodou self._pack_fields() a vlo��m do sizeru t�to
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
                # p�idej posledn� sled pol��ek (pokud n�jak� byl)
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
            # p�idej zbyl� sled pol��ek (pokud n�jak� byl)
            sizer.Add(self._pack_fields(pack, space, gap),
                      0, wx.ALIGN_TOP|border_style, border)
        # pokud m� skupina or�mov�n�, p�id�me ji je�t� do sizeru s horn�m
        # odsazen�m, jinak je horn� odsazen� p��li� mal�.
        if group.label() is not None:
            s = wx.BoxSizer(orientation)
            s.Add(sizer, 0, wx.TOP, 3)
            sizer = s
        return sizer

    def _pack_fields(self, items, space, gap):
        """Sestav skupinu pod sebou um�st�n�ch pol��ek/tla��tek do gridu.

        Argumenty:

          items -- sekvence identifik�tor� pol��ek nebo instanc� Button.
          space -- mezera mezi ovl�dac�m prvkem a labelem pol��ka v dlg units;
            integer
          gap -- mezera mezi jednotliv�mi pol��ky v dlg units; integer

        Pro ka�d� prvek skupiny vytvo�� tla��tko nebo pol��ko 'inputfield.InputField'
        a p�id� jeho label a widget do vytvo�en� instance
        'wx.FlexGridSizer'.

        Vrac�: instanci 'wx.FlexGridSizer' napln�nou pol��ky a tla��tky.

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

    def _signal_update(self):
        f = current_form()
        if isinstance(f, Refreshable):
            f.refresh()

    def _validate_fields(self):
        # Postupn� validace v�ech pol��ek.
        for f in self._fields:
            if self._mode == self.MODE_INSERT or f.is_modified():
                value, error = f.validate()
                if error:
                    log(EVENT, 'Validace selhala:', (f.id(), f.get_value()))
                    f.set_focus()
                    return False
        return True
            
    def _commit_form(self, close=True):
        # Validace v�ech pol��ek.
        if not self._validate_fields():
            return False
        # Ov��en� integrity z�znamu (funkce check).
        failed_id = self._check_record(self._row)
        if failed_id:
            self._field(failed_id).set_focus()
            return False
        # Vytvo�en� datov�ho ��dku.
        rdata = self._record_data(self._row)
        if self._mode == self.MODE_INSERT:
            log(ACTION, 'Vlo�en� ��dku')
            op = (self._data.insert, (rdata,))
        elif self._mode == self.MODE_EDIT:
            log(ACTION, 'Update ��dku')
            op = (self._data.update, (self._current_key(), rdata))
        else:
            raise ProgramError("Can't commit in this mode.")
        # Proveden� operace
        success, result = db_operation(op)
        if success and result[1]:
            new_row = result[0]
            if new_row is not None:
                self._row.set_row(new_row, reset=True)
                self.set_row(self._row)
            else:
                # TODO: Lze prov�st n�co chyt�ej��ho?
                pass
            self._signal_update()
            if self._mode == self.MODE_INSERT:
                log(ACTION, 'Z�znam vlo�en')
            else:
                log(ACTION, 'Z�znam updatov�n')
                # Pol��ka se t�mto trikem budou tv��it nezmn�n�n�.
                for field in self._fields:
                    field.init(field.get_value())
            cleanup = self._view.cleanup()
            if cleanup is not None:
                cleanup(self._row)
            if close:    
                self._result = self._row
                # t�m je automaticky zavol�no _on_parent_close()
                # TODO: to asi nebude fungovat v embeded verzi!!!!!!!!!!
                self._parent.Close()
            return True
        else:
            msg = _("Ulo�en� z�znamu se nezda�ilo")
            if type(result) == type(()) and \
               isinstance(result[0], types.StringTypes):
                msg = "%s\n\n%s" % (result[0], msg)
            run_dialog(Error, msg)
            return False

    def _select_row(self, row):
        # TODO: Tato implementace pat�� sp�e do odvozen� t��dy (EditForm)...
        prow = PresentedRow(self._view.fields(), self._data, row,
                            prefill=self._prefill,
                            new=self._mode == self.MODE_INSERT,
                            change_callback=self._on_field_change,
                        editability_change_callback=self._on_editability_change)
        self.set_row(prow)

    def set_row(self, row):
        """Napl� formul�� daty z dan�ho ��dku (instance 'PresentedRow')."""
        for f in self._fields:
            f.init(row[f.id()].export())
        super_(EditForm).set_row(self, row)
        
    def title(self):
        """Vra� n�zev formul��e jako �et�zec."""        
        return self._view.layout().caption()

    def size(self):
        """Vra� skute�nou velikost formul��e (bez ohledu na aktu�ln� velikost).

        Vr�cen� hodnota reprezentuje minim�ln� velikost formul��e, tak aby byly
        v�echny jeho prvky viditeln�.  Skute�n� velikost m��e b�t men��, nebo
        v�t�� v z�voslosti na velikost okna, ve kter�m je formul�� zobrazen.
        
        """
        return self._size

    def set_scrollbars(self):
        step = 20
        size = self.GetSize()
        self.SetScrollbars(step, step, size.width/step, size.height/step)
    
    def changed(self):
        """Vra� pravdu, pokud byla data zm�n�na od posledn�ho ulo�en�."""
        field = find(True, self._fields, key=lambda f: f.is_modified())
        return field is not None

    def show_popup_menu(self):
        field = InputField.focused()
        if field is not None:
            field.show_popup_menu()

    def _on_field_edit(self, id, value):
        # Signalizace zm�ny pol��ka z�InputField
        self._row[id] = value

    def _on_field_change(self, id):
        # Signalizace zm�ny pol��ka z�PresentedRow
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
        # Vygeneruj ud�lost navigace mezi pol��ky.
        if self._mode != self.MODE_VIEW:
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
        """Prove� kontrolu formul��e p�ed uzav�en�m.

        Vrac�: Pravdu pr�v� tehdy kdy� je mo�no formul�� uzav��t.

        """
        if self.changed():
            q = _("Data byla zm�n�na a nebyla ulo�ena!") + "\n" + \
                _("Opravdu chcete uzav��t formul��?")
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
            # Pokud se volal v�b�r polo�ky seznamu z ListField,
            # mus�me zajistit nastaven� _refvalues v PresentedRow.
            if command == ListField.COMMAND_CHOOSE_KEY:
                if kwargs.has_key('id'):
                    id = kwargs['id']
                    val = field.get_item()
                    self._row.listfield_choose(id, val)
            return True
        if self._mode != self.MODE_VIEW:
            if command == EditForm.COMMAND_COMMIT_RECORD:
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

    
class PopupEditForm(PopupForm, EditForm):
    """Stejn� jako 'EditForm', av�ak v�popup podob�."""
    
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
            message(_("Z�znam ulo�en"))
            refresh()
            self._select_row(None)
            self._init_fields()
        return False

    def _on_cancel(self, event):
        self._leave_form()
        return True
    
    def _create_buttons(self):
        ok, cancel = buttons = (wx.Button(self, wx.ID_OK, _("Ok")),
                                wx.Button(self, wx.ID_CANCEL, _("Zav��t")))
        wx_callback(wx.EVT_BUTTON, self, wx.ID_OK, self._on_submit)
        wx_callback(wx.EVT_BUTTON, self, wx.ID_CANCEL, self._on_cancel)
        ok.SetToolTipString(_("Ulo�it z�znam a uzav��t formul��"))
        cancel.SetToolTipString(_("Uzav��t formul�� bez ulo�en� dat"))
        if self._mode == self.MODE_INSERT and not self._disable_new_button:
            next = wx.Button(self, wx.ID_FORWARD, _("Dal��"))
            wx_callback(wx.EVT_BUTTON, self, wx.ID_FORWARD, self._on_next)
            next.SetToolTipString(_("Ulo�it z�znam a reinicializovat formul��"
                                    " pro vlo�en� dal��ho z�znamu"))
            buttons = (ok, cancel, next)
        ok.SetDefault()
        sizer = wx.BoxSizer(wx.HORIZONTAL)
        for b in buttons:
            sizer.Add(b, 0, wx.ALL, 20)
        return sizer
    
    def run(self):
        key = self._current_key()
        if self._mode == self.MODE_EDIT and key and not self._lock_record(key):
            return None
        try:
            return PopupForm.run(self)
        finally:
            self._unlock_record()

    def set_status(self, field, message):
        if field == 'message':
            if message is None:
                message = ''
            self._status.SetLabel(unicode(message))
            return True
        else:
            return False
       

class ShowForm(EditForm):
    """Formul�� pro zobrazen� n�hledu.

    Layout je stejn� jako u edita�n�ho formul��e (resp. 'EditForm'),
    pouze titulek m� stejn� vzhled, jako titulek formul��� typu 'ListForm'.
    Ur�en pro zobrazen� v du�ln�m formul��i.

    """

    _DESCR = _("n�hled")

    def _init_attributes(self, mode=EditForm.MODE_VIEW, **kwargs):
        super_(ShowForm)._init_attributes(self, mode=mode, **kwargs)
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
    """Listovac� formul�� pro zobrazen� n�hledu.

    Formul�� je needitovateln�, ale umo��uje pohyb po z�znamech tabulky, nad
    kterou je vytvo�en, vyhled�v�n� atd.  Z u�ivatelsk�ho hlediska jde v
    podstat� o redukci prohl�ec�ch mo�nost� formul��e typu 'BrowseForm' na
    jeden z�znam zobrazen� v Layoutu edita�n�ho formul��e.
    
    """
    
    def __init__(self, *args, **kwargs):
        super_(BrowsableShowForm).__init__(self, *args, **kwargs)
        self._init_select()
        self._set_status()

    def _on_next_record(self, direction=pytis.data.FORWARD):
        op = lambda : self._data.fetchone(direction=direction)
        success, row = db_operation(op)
        if not row:
            if direction == pytis.data.FORWARD:
                message(_("Posledn� z�znam"), beep_=True)
            else:
                message(_("Prvn� z�znam"), beep_=True)
            # P�esuneme ukazov�tko zp�t na posledn� z�znam, to je chov�n�
            # o�ek�van� u�ivateli.
            antidir = pytis.data.opposite_direction(direction)
            db_operation(lambda: self._data.fetchone(direction=antidir))
        if not success or not row:
            return
        self._select_row(row)

    def _select_row(self, row):
        super(BrowsableShowForm, self)._select_row(row)
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
            self.select_row(0)
            return True
        elif command == BrowsableShowForm.COMMAND_LAST_RECORD:
            self.select_row(self._lf_select_count-1)
            return True
        else:
            return super(BrowsableShowForm, self).on_command(command, **kwargs)
