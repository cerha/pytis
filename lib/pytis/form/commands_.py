# -*- coding: iso-8859-2 -*-

# Definice u�ivatelsk�ch p��kaz�
# 
# Copyright (C) 2002-2006, 2007 Brailcom, o.p.s.
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

"""Definice u�ivatelsk�ch p��kaz�.

V�echny p��kazy jsou centr�ln� definov�ny zde z�d�vodu konzistence a
p�ehlednosti, zejm�na z�pohledu tv�rc� definic aplikac�.  Pro v�echny
definovan� p��kazy jsou automaticky vytvo�eny konstanty ve t��d�, kter� je
p��kaz ur�en; n�zvy t�chto konstant za��naj� prefixem 'COMMAND_' (tuto konvenci
je nutno dodr�ovat).

Nap��klad definice Command(Application, 'EXIT', 'Ukon�en� aplikace.')  vytvo��
konstantu Application.COMMAND_EXIT a pod t�mto n�zvem je mo�n� s instanc�
p��kazu d�le pracovat.

P�i�azen� kl�ves p��kaz�m:

V�aplikaci nikde nepou��v�me p��m� z�pis kl�ves, n�br� pouze jim odpov�daj�c�
p��kazy.  To n�m umo��uje nestarat se v�aplika�n�m k�du o�p�i�azen� kl�ves.  To
definujeme na jedin�m m�st� v�tomto modulu.

"""

from pytis.form import *
    
Command(Application, 'EXIT',
        "Ukon�en� aplikace")
Command(Application, 'HELP',
        "Vyvol�n� n�pov�dy")
Command(Application, 'BREAK',
        "P�eru�en� aktu�ln� prov�d�n� operace")
Command(Application, 'REFRESH',
        "Vy��d�n� obnoven� obsahu aktivn�ho formul��e")
Command(Application, 'NEW_RECORD',
        "Vlo�en� nov�ho z�znamu")
Command(Application, 'RUN_FORM',
        "Spu�t�n� formul��e")
Command(Application, 'RUN_PROCEDURE',
        "Spu�t�n� procedury")
Command(Application, 'RAISE_FORM',
        "Vyzvednut� okna formul��e v okn� aplikace")
Command(Application, 'RAISE_PREV_FORM',
        "Vyzvednut� okna p�edchoz�ho formul��e")
Command(Application, 'RAISE_NEXT_FORM',
        "Vyzvednut� okna n�sleduj�c�ho formul��e")
Command(Application, 'RAISE_RECENT_FORM',
        "Vyzvednut� okna posledn� aktivn�ho formul��e")
Command(Application, 'CLEAR_RECENT_FORMS',
        "Vy�isti menu posledn� otev�en�ch formul���")
Command(Application, 'HANDLED_ACTION',
        "Prove� u�ivatelem definovanou akci.") # arg. 'handler', 'enabled'
Command(Application, 'NOTHING',
        "Fale�n� p��kaz, kter� nic ned�l�.") # arg. 'enabled'
Command(Form, 'LEAVE_FORM',
        "Uzav�en� formul��e")
Command(Form, 'HELP',
        "Zobrazen� n�pov�dy formul��e")
Command(InnerForm, 'PRINT',
        "Tisk aktu�ln�ho obsahu formul��e")
Command(InnerForm, 'RELOAD_FORM_STATE',
        "Zapome� u�iv. nastaven� formul��e a vra� posledn� ulo�en� hodnoty")
Command(InnerForm, 'RESET_FORM_STATE',
        "Zaho� ulo�en� u�iv. nastaven� formul��e a vra� v�choz� nastaven�")
Command(LookupForm, 'FILTER',
        "Filtrov�n� z�znam�")
Command(LookupForm, 'UNFILTER',
        "Zru�en� filtrov�n� z�znam�")
Command(LookupForm, 'FILTER_BY_VALUE',
        "Vyfiltrov�n� formul��e podle dan� hodnoty") # arg. 'column_id', 'value'
Command(LookupForm, 'JUMP',
        "Skok na z�znam")
Command(LookupForm, 'SEARCH',
        "Hled�n� z�znamu")
Command(LookupForm, 'SORT',
        "Set��d�n� podle sloupce")
Command(LookupForm, 'FIRST_RECORD',
        "Skok na prvn� z�znam")
Command(LookupForm, 'LAST_RECORD',
        "Skok na posledn� z�znam")
Command(RecordForm, 'NEW_RECORD',
        "Vlo�en� nov�ho z�znamu pomoc� edita�n�ho formul��e")
Command(RecordForm, 'IMPORT_INTERACTIVE',
        "Interaktivn� import dat z CSV souboru")
Command(RecordForm, 'EDIT_RECORD',
        "Editace aktu�ln�ho z�znamu v edita�n�m formul��i")
Command(RecordForm, 'DELETE_RECORD',
        "Vymaz�n� editovan�ho z�znamu z�datab�ze")
Command(ListForm, 'ACTIVATE',
        "Aktiva�n� funkce pro aktu�ln� ��dek formul��e") # arg. 'alternate'
Command(ListForm, 'FIRST_COLUMN',
        "P�echod na prvn� sloupec tabulky")
Command(ListForm, 'LAST_COLUMN',
        "P�echod na posledn� sloupec tabulky")
Command(ListForm, 'INCREMENTAL_SEARCH',
        "Prefixov� inkrement�ln� hled�n� z�znamu") # arg. 'full'
Command(ListForm, 'EDIT',
        "Vyvol�n� inline editace aktu�ln� bu�ky")
Command(ListForm, 'COPY_CELL',
        "Zkop�rov�n� obsahu aktu�ln� bu�ky do clipboardu")
Command(ListForm, 'FILTER_BY_CELL',
        "Vyfiltrov�n� formul��e podle hodnoty aktu�ln� bu�ky")
Command(ListForm, 'AUTOFILTER',
        "Zobrazen� menu autofilteru") # arg 'col' a 'position'
Command(ListForm, 'TOGGLE_AGGREGATION',
        "P�id�n�/odebr�n� agrega�n� funkce") # arg 'operation'
Command(ListForm, 'AGGREGATE',
        "P�id�n� agrega�n� funkce") # arg 'operation'
Command(ListForm, 'UNAGGREGATE',
        "Odebr�n� agrega�n� funkce") # arg 'operation'
Command(ListForm, 'EXPORT_CSV',
        "Export ��dkov�ho formul��e do csv souboru")
Command(ListForm, 'LINE_COMMIT',
        "Dokon�en� editace z�znamu (ulo�en�)")
Command(ListForm, 'LINE_ROLLBACK',
        "Zru�en� zm�n v editovan�m z�znamu") # arg. 'soft'
Command(ListForm, 'FINISH_EDITING',
        "Opu�t�n� editace ��dku")
Command(ListForm, 'CELL_COMMIT',
        "Ukon�en� editace pol��ka s�novou hodnotou")
Command(ListForm, 'CELL_ROLLBACK',
        "Ukon�en� editace pol��ka s�vr�cen�m p�vodn� hodnoty")
Command(ListForm, 'INSERT_LINE',
        "In-line vlo�en� nov�ho z�znamu") # arg. 'before' a 'copy'
Command(ListForm, 'SET_GROUPING_COLUMN',
        "Zm�na sloupce vizu�n�ho seskupov�n�")
Command(ListForm, 'RESIZE_COLUMN',
        "Roz���en�/z��en� sloupce") # arg. 'diff' ud�v� po�et +/- pixel�
Command(ListForm, 'MOVE_COLUMN',
        "P�esunut� sloupce doprava/doleva") # arg. 'diff' +/- o kolik sloupc�
Command(ListForm, 'TOGGLE_COLUMN',
        "Skryt�/zobrazen� sloupce")
Command(ListForm, 'TOGGLE_ROW_LABELS',
        "Skryt�/zobrazen� sloupce z�hlav� ��dk�")
Command(ListForm, 'CONTEXT_ACTION',
        "Vyvol�n� akce nad aktu�ln�m ��dkem formul��e")
        # Povinn� argument 'action' je instanc� specifikacn� t��dy 'Action'.
Command(ListForm, 'CONTEXT_MENU',
        "Zobrazen� kontextov�ho menu aktivn� bu�ky")
Command(EditForm, 'COMMIT_RECORD',
        "Ukon�en� edita�n�ho formul��e s ulo�en�m zm�n")
Command(EditForm, 'NAVIGATE',
        "Navigace mezi pol��ky edita�n�ho formul��e") # arg. 'back'
Command(BrowsableShowForm, 'NEXT_RECORD',
        "P�echod na dal�� z�znam vp�ed/vzad") # arg. 'back'
Command(DualForm, 'OTHER_FORM',
        "P�echod mezi podformul��i du�ln�ho formul��e")
Command(PrintFormInternal, 'NEXT_PAGE',
        "P�echod na dal�� str�nku tiskov�ho n�hledu")
Command(PrintFormInternal, 'PREVIOUS_PAGE',
        "P�echod na p�edchoz� str�nku tiskov�ho n�hledu")
Command(InputField, 'RESET',
        "Vr�cen� p�vodn� hodnoty vstupn�ho pol��ka")
Command(InputField, 'CONTEXT_MENU',
        "Zobrazen� kontextov�ho menu vstupn�ho pol��ka")
Command(InputField, 'LEAVE_INLINE_EDIT',
        "Odchod z editace vstupn�ho pol��ka")
Command(TextField, 'CUT',
        "Vyjmut� ozna�en�ho textu do schr�nky.")
Command(TextField, 'COPY',
        "Zkop�rov�n� ozna�en�ho textu do schr�nky.")
Command(TextField, 'PASTE',
        "Vlo�en� textu ze schr�nky do pol��ka.")
Command(TextField, 'SELECT_ALL',
        "Proveden� v�b�ru cel�ho textu pol��ka.")
Command(Invocable, 'INVOKE_SELECTION',
        "Vyvol�n� v�b�ru hodnoty vstupn�ho pol��ka") # arg. 'alternate'
Command(GenericCodebookField, 'INVOKE_CODEBOOK_FORM',
        "Vyvol�n� alternativn�ho v�b�ru hodnoty pol��ka")
Command(ListField, 'SELECT',
        "V�b�r aktu�ln�ho z�znamu")
Command(ListField, 'SHOW_SELECTED',
        "V�b�r aktu�ln�ho z�znamu")
Command(ListField, 'INVOKE_EDIT_FORM',
        "Vyvol�n� edita�n�ho formul��e nad akt. z�znamem")
Command(FileField, 'LOAD',
        "Nahr�n� soubodu jako nov� hodnoty pol��ka.")
Command(FileField, 'SAVE',
        "Ulo�en� objektu z datab�ze do soborov�ho syst�mu.")
Command(FileField, 'CLEAR',
        "Vynulov�n� nastaven� hodnoty pol��ka.")
Command(ImageField, 'VIEW',
        "Otev�en� n�hledu obr�zku v prohl�e�i.")

Command(Dialog, 'CLOSE_DIALOG',
        "Opu�t�n� dialogu bez potvrzen�")
Command(Dialog, 'COMMIT_DIALOG',
        "Potvrzen� dialogu") # arg. 'force' simuluje stisk v�choz�ho tla��tka
Command(Dialog, 'HELP',
        "Vyvol�n� n�pov�dy dialogu")

DEFAULT_KEYMAP = (
    ('F1',            Application.COMMAND_HELP(topic='pytis')),
    ('Ctrl-g',        Application.COMMAND_BREAK),
    ('Ctrl-Down',     Application.COMMAND_RAISE_NEXT_FORM),
    ('Ctrl-Up',       Application.COMMAND_RAISE_PREV_FORM),
    ('Ctrl-w',        Application.COMMAND_RAISE_RECENT_FORM),
    ('Ctrl-l',        Application.COMMAND_REFRESH),
    ('Ctrl-F1',       Form.COMMAND_HELP),
    ('Escape',        Form.COMMAND_LEAVE_FORM),
    ('Ctrl-p',        InnerForm.COMMAND_PRINT),
    ('Ctrl-Backspace',       InnerForm.COMMAND_RELOAD_FORM_STATE),
    ('Ctrl-Shift-Backspace', InnerForm.COMMAND_RESET_FORM_STATE),
    ('F6',            RecordForm.COMMAND_NEW_RECORD),
    ('Ctrl-F6',       RecordForm.COMMAND_NEW_RECORD(copy=True)),
    ('Alt-F6',        RecordForm.COMMAND_IMPORT_INTERACTIVE),
    ('F5',            RecordForm.COMMAND_EDIT_RECORD),
    ('F8',            RecordForm.COMMAND_DELETE_RECORD),
    ('Ctrl-f',        LookupForm.COMMAND_FILTER),
    ('Ctrl-Alt-f',    LookupForm.COMMAND_FILTER(last=True)),
    ('Ctrl-Shift-f',  LookupForm.COMMAND_UNFILTER),
    ('F4',            LookupForm.COMMAND_SORT),
    ('F3',            LookupForm.COMMAND_SEARCH),
    ('Ctrl-s',        LookupForm.COMMAND_SEARCH(next=True)),
    ('Ctrl-r',        LookupForm.COMMAND_SEARCH(next=True, back=True)),
    ('Ctrl-j',        LookupForm.COMMAND_JUMP),
    ('Ctrl-Home',     LookupForm.COMMAND_FIRST_RECORD),
    ('Ctrl-Prior',    LookupForm.COMMAND_FIRST_RECORD),
    ('Ctrl-End',      LookupForm.COMMAND_LAST_RECORD),
    ('Ctrl-Next',     LookupForm.COMMAND_LAST_RECORD),
    ('Ctrl-F3',       ListForm.COMMAND_INCREMENTAL_SEARCH),
    ('Alt-F3',        ListForm.COMMAND_INCREMENTAL_SEARCH(full=True)),
    ('Enter',         ListForm.COMMAND_ACTIVATE),
    (' ',             ListForm.COMMAND_ACTIVATE(alternate=True)),
    ('Ctrl-c',        ListForm.COMMAND_COPY_CELL),
    ('Home',          ListForm.COMMAND_FIRST_COLUMN),
    ('End',           ListForm.COMMAND_LAST_COLUMN),
    ('Ctrl-e',        ListForm.COMMAND_EXPORT_CSV),
    ('F2',            ListForm.COMMAND_EDIT),
    ('F9',            ListForm.COMMAND_EDIT),
    ('Ctrl-F12',      ListForm.COMMAND_LINE_ROLLBACK),
    ('Escape',        ListForm.COMMAND_FINISH_EDITING),
    ('F12',           ListForm.COMMAND_LINE_COMMIT),
    ('Enter',         ListForm.COMMAND_CELL_COMMIT),
    ('Escape',        ListForm.COMMAND_CELL_ROLLBACK),
    ('Insert',        ListForm.COMMAND_INSERT_LINE()),
    ('F7',            ListForm.COMMAND_INSERT_LINE(copy=True)),
    ('Ctrl-Insert',   ListForm.COMMAND_INSERT_LINE(before=True)),
    ('Ctrl-F7',       ListForm.COMMAND_INSERT_LINE(before=True, copy=True)),
    ('Alt-Right',     ListForm.COMMAND_RESIZE_COLUMN(diff=+5)),
    ('Alt-Left',      ListForm.COMMAND_RESIZE_COLUMN(diff=-5)),
    ('Shift-Right',   ListForm.COMMAND_MOVE_COLUMN(diff=+1)),
    ('Shift-Left',    ListForm.COMMAND_MOVE_COLUMN(diff=-1)),
    ('Alt-Down',      ListForm.COMMAND_CONTEXT_MENU),
    ('Alt-Enter',     ListForm.COMMAND_CONTEXT_MENU),
    ('F11',           ListForm.COMMAND_TOGGLE_ROW_LABELS),
    ('Ctrl-Enter',    EditForm.COMMAND_COMMIT_RECORD),
    ('Tab',           EditForm.COMMAND_NAVIGATE),
    ('Shift-Tab',     EditForm.COMMAND_NAVIGATE(back=True)),
    ('Next',          BrowsableShowForm.COMMAND_NEXT_RECORD),
    ('Prior',         BrowsableShowForm.COMMAND_NEXT_RECORD(back=True)),
    ('Ctrl-Tab',      DualForm.COMMAND_OTHER_FORM),
    ('Next',          PrintFormInternal.COMMAND_NEXT_PAGE),
    ('Prior',         PrintFormInternal.COMMAND_PREVIOUS_PAGE),
    ('Ctrl-Backspace',InputField.COMMAND_RESET),
    ('Alt-Down',      InputField.COMMAND_CONTEXT_MENU),
    ('Alt-Enter',     InputField.COMMAND_CONTEXT_MENU),
    ('Ctrl-x',        TextField.COMMAND_CUT),
    ('Ctrl-c',        TextField.COMMAND_COPY),
    ('Ctrl-v',        TextField.COMMAND_PASTE),
    ('F2',            Invocable.COMMAND_INVOKE_SELECTION),
    ('Ctrl-F2',       Invocable.COMMAND_INVOKE_SELECTION(alternate=True)),
    ('Backspace',     FileField.COMMAND_CLEAR),
    ('Delete',        FileField.COMMAND_CLEAR),
    ('Backspace',     ListField.COMMAND_SHOW_SELECTED),
    ('Escape',        Dialog.COMMAND_CLOSE_DIALOG),
    ('Enter',         Dialog.COMMAND_COMMIT_DIALOG),
    ('Ctrl-Enter',    Dialog.COMMAND_COMMIT_DIALOG(force=True)),

    # Just for backwards compatibility
    ('Ctrl-F4',       LookupForm.COMMAND_FILTER),

    )



if __debug__:
    Command(Application, 'CUSTOM_DEBUG',
            "Pomocn� p��kaz pro vyvol�n� pomocn� lad�c� funkce")
    DEFAULT_KEYMAP += \
        (('Ctrl-d', Application.COMMAND_CUSTOM_DEBUG),)


FORM_COMMAND_MENU = ((
    (LookupForm.COMMAND_JUMP,
     _("Skok na z�znam"),
     _("Skok na z�znam podle ��sla ��dku.")),
    (LookupForm.COMMAND_SEARCH,
     _("Hledat"),
     _("Vyhled�vat z�znam podle zadan�ch podm�nek.")),
    (LookupForm.COMMAND_SEARCH(next=True),
     _("Hledat dal��"),
     _("Vyhledat dal�� z�znam odpov�daj�c� zadan�m podm�nk�m.")),
    (LookupForm.COMMAND_SEARCH(next=True, back=True),
     _("Hledat p�edchoz�"),
     _("Vyhledat p�edchoz� z�znam odpov�daj�c� zadan�m podm�nk�m.")),
    (ListForm.COMMAND_INCREMENTAL_SEARCH,
     _("Inkrement�ln� hled�n�"),
     _("Postupn� vyhled�vat z�znam podle hodnoty pol��ka.")),
    (ListForm.COMMAND_INCREMENTAL_SEARCH(full=True),
     _("Inkrement�ln� hled�n� pod�et�zce"),
     _("Postupn� vyhled�vat z�znam podle ��sti hodnoty pol��ka.")),
    ),(#---------------
    (LookupForm.COMMAND_SORT,
     _("�azen�"),
     _("Ur�it podm�nky �azen� z�znam�.")),
    (LookupForm.COMMAND_FILTER,
     _("Filtrov�n�"),
     _("Filtrovat z�znamy podle zadan�ch podm�nek.")),
    (LookupForm.COMMAND_UNFILTER,
     _("Zru� filtr"),
     _("Zru�it filtrov�n� z�znam�.")),
    ),(#---------------
    (RecordForm.COMMAND_NEW_RECORD,
     _("Nov� z�znam"),
     _("Vlo�it nov� z�znam v samostatn�m formul��i.")),
    (RecordForm.COMMAND_NEW_RECORD(copy=True),
     _("Nov� z�znam - kopie"),
     _("Vlo�it nov� z�znam zkop�rov�n�m hodnot aktu�ln�ho z�znamu.")),
    (RecordForm.COMMAND_EDIT_RECORD,
     _("Editovat z�znam"),
     _("Upravit hodnoty z�znamu v samostatn�m formul��i.")),
    (RecordForm.COMMAND_DELETE_RECORD,
     _("Smazat z�znam"),
     _("Smazat aktu�ln� z�znam.")),
    ),(#---------------
    (ListForm.COMMAND_INSERT_LINE,
     _("Vlo�it ��dek pod"),
     _("Vlo�it nov� z�znam v re�imu inline editace.")),
    (ListForm.COMMAND_INSERT_LINE(before=True),
     _("Vlo�it ��dek nad"),
     _("Vlo�it nov� z�znam v re�imu inline editace.")),
    (ListForm.COMMAND_INSERT_LINE(copy=True),
     _("Kop�rovat ��dek pod"),
     _("Vlo�it nov� z�znam v re�imu inline editace jako kopii sou�asn�ho.")),
    (ListForm.COMMAND_INSERT_LINE(copy=True, before=True),
     _("Kop�rovat ��dek nad"),
     _("Vlo�it nov� z�znam v re�imu inline editace jako kopii sou�asn�ho.")),
    ),(#---------------
    (ListForm.COMMAND_EXPORT_CSV,
     _("Export do textov�ho souboru"),
     _("Exportovat data do textov�ho souboru ve form�tu CSV.")),
    (RecordForm.COMMAND_IMPORT_INTERACTIVE,
     _("Import z textov�ho souboru"),
     _("Importovat data z textov�ho souboru ve form�tu CSV.")),
    ),(#---------------
    (InnerForm.COMMAND_RELOAD_FORM_STATE,
     _("Vr�tit p�edchoz� nastaven� formul��e"),
     _("Zahodit zm�ny nastaven� formul��e proveden� b�hem tohoto spu�t�n� "
       "aplikace")),
    (InnerForm.COMMAND_RESET_FORM_STATE,
     _("Vr�tit v�choz� nastaven� formul��e"),
     _("Zahodit ve�ker� u�ivatelsk� nastaven� formul��e")),
     ))

_ASC = LookupForm.SORTING_ASCENDENT
_DESC = LookupForm.SORTING_DESCENDANT
_NONE = LookupForm.SORTING_NONE

COMMAND_ICONS = (
    (Application.COMMAND_HELP,                             wx.ART_HELP),
    (Application.COMMAND_EXIT,                             wx.ART_QUIT),
    (Application.COMMAND_RAISE_NEXT_FORM,                  'next-form'),
    (Application.COMMAND_RAISE_PREV_FORM,                  'prev-form'),
    (Application.COMMAND_RAISE_RECENT_FORM,                'recent-form'),
    #(Application.COMMAND_NEW_RECORD,                      'new-record'),
    #(Application.COMMAND_RUN_FORM,                        'run-form'),
    (Form.COMMAND_HELP,                                    'help-book'),
    (Form.COMMAND_LEAVE_FORM,                              'close'),
    (InnerForm.COMMAND_PRINT,                              wx.ART_PRINT),
    (InputField.COMMAND_RESET,                             wx.ART_UNDO),
    (TextField.COMMAND_CUT,                                wx.ART_CUT),
    (TextField.COMMAND_COPY,                               wx.ART_COPY),
    (TextField.COMMAND_PASTE,                              wx.ART_PASTE),
    (FileField.COMMAND_LOAD,                               wx.ART_FILE_OPEN),
    (FileField.COMMAND_SAVE,                               wx.ART_FILE_SAVE),
    (FileField.COMMAND_CLEAR,                              wx.ART_DELETE),
    (ListForm.COMMAND_COPY_CELL,                           wx.ART_COPY),
    (ListForm.COMMAND_EDIT,                                'edit-cell'),
    (ListForm.COMMAND_FILTER_BY_CELL,                      'filter-by-cell'),
    (ListForm.COMMAND_AUTOFILTER,                          'autofilter'),
    (ListForm.COMMAND_INCREMENTAL_SEARCH,                  'search-incremental'),
    (ListForm.COMMAND_EXPORT_CSV,                          'export'),
    (ListForm.COMMAND_IMPORT_INTERACTIVE,                  'import'),
    (ListForm.COMMAND_INSERT_LINE(before=True, copy=True), 'insert-line-before-copy'),
    (ListForm.COMMAND_INSERT_LINE(before=True),            'insert-line-before'),
    (ListForm.COMMAND_INSERT_LINE(copy=True),              'insert-line-copy'),
    (ListForm.COMMAND_INSERT_LINE,                         'insert-line'),
    (ListForm.COMMAND_TOGGLE_COLUMN(col=None),             'hide-column'),
    (ListForm.COMMAND_AGGREGATE,                           'aggregate'),
    (ListForm.COMMAND_UNAGGREGATE,                         'unaggregate'),
    (RecordForm.COMMAND_EDIT_RECORD,                       'edit-record'),
    (RecordForm.COMMAND_NEW_RECORD(copy=True),             'new-record-copy'),
    (RecordForm.COMMAND_NEW_RECORD,                        'new-record'),
    (RecordForm.COMMAND_DELETE_RECORD,                     'delete-record'),
    (LookupForm.COMMAND_SORT(direction=_ASC),               'sort-asc'),
    (LookupForm.COMMAND_SORT(direction=_DESC),              'sort-desc'),
    (LookupForm.COMMAND_SORT(direction=_NONE),              'unsort'),
    (LookupForm.COMMAND_SORT,                              'sort'),
    (LookupForm.COMMAND_FILTER_BY_VALUE,                   'filter-by-cell'),
    (LookupForm.COMMAND_FILTER(last=True),                 'filter'),
    (LookupForm.COMMAND_FILTER,                            'filter-form'),
    (LookupForm.COMMAND_UNFILTER,                          'unfilter'),
    (LookupForm.COMMAND_SEARCH(next=True, back=True),      'search-backwards'),
    (LookupForm.COMMAND_SEARCH(next=True),                 'search-forward'),
    (LookupForm.COMMAND_SEARCH,                            'search'),
    (LookupForm.COMMAND_JUMP,                              'jump'),
    (InnerForm.COMMAND_RESET_FORM_STATE,                   'reset-form-state'),
    (InnerForm.COMMAND_RELOAD_FORM_STATE,                  'reload-form-state'),
    )
