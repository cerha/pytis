# -*- coding: iso-8859-2 -*-

# Definice u�ivatelsk�ch p��kaz�
# 
# Copyright (C) 2002, 2003, 2004, 2005 Brailcom, o.p.s.
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
        "Ukon�en� aplikace.")
Command(Application, 'BREAK',
        "P�eru�en� aktu�ln� prov�d�n� operace.")
Command(Application, 'REFRESH',
        "Vy��d�n� obnoven� obsahu aktivn�ho formul��e.")
Command(Application, 'NEW_RECORD',
        "Vlo�en� nov�ho z�znamu.")
Command(Application, 'RUN_FORM',
        "Spu�t�n� formul��e.")
Command(Application, 'RUN_PROCEDURE',
        "Spu�t�n� procedury.")
Command(Application, 'LEAVE_FORM',
        "Odstran�n� aktivn�ho okna formul��e z�aplikace.")
Command(Application, 'RAISE_FORM',
        "Vyzvednut� okna formul��e v okn� aplikace.")
Command(Application, 'PREV_FORM',
        "Vyzvednut� okna p�edchoz�ho formul��e.")
Command(Application, 'NEXT_FORM',
        "Vyzvednut� okna n�sleduj�c�ho formul��e.")
Command(Application, 'SHOW_POPUP_MENU',
        "Zobraz kontextov� menu aktivn�ho prvku.")
Command(Form, 'PRINT',
        "Tisk aktu�ln�ho obsahu formul��e.")
Command(LookupForm, 'FILTER',
        "Filtrov�n� z�znam�.")
Command(LookupForm, 'JUMP',
        "Skok na z�znam.")
Command(LookupForm, 'SEARCH',
        "Hled�n� z�znamu.")
Command(LookupForm, 'SEARCH_PREVIOUS',
        "Hled�n� P�edchoz�ho z�znamu bez dialogu.")
Command(LookupForm, 'SEARCH_NEXT',
        "Hled�n� dal��ho z�znamu bez dialogu.")
Command(LookupForm, 'SORT_COLUMN',
        "Set��d�n� podle sloupce.")
Command(RecordForm, 'NEW_RECORD',
        "Vlo�en� nov�ho z�znamu pomoc� edita�n�ho formul��e.")
Command(RecordForm, 'NEW_RECORD_COPY',
        "Vlo�en� kopie z�znamu pomoc� edita�n�ho formul��e.")
Command(RecordForm, 'EDIT_RECORD',
        "Editace aktu�ln�ho z�znamu v edita�n�m formul��i.")
Command(RecordForm, 'DELETE_RECORD',
        "Vymaz�n� editovan�ho z�znamu z�datab�ze.")
Command(ListForm, 'ACTIVATE',
        "Aktiva�n� funkce pro aktu�ln� ��dek formul��e.")
Command(ListForm, 'ACTIVATE_ALTERNATE',
        "Alternativn� aktiva�n� funkce pro aktu�ln� ��dek.")
Command(ListForm, 'SHOW_CELL_CODEBOOK',
        "Vyvol�n� ��seln�ku aktivn� bu�ky ��dkov�ho formul��e.")
Command(ListForm, 'SELECT_CELL',
        "V�b�r bu�ky seznamu.")
Command(ListForm, 'FIRST_COLUMN',
        "P�echod na prvn� sloupec tabulky.")
Command(ListForm, 'LAST_COLUMN',
        "P�echod na posledn� sloupec tabulky.")
Command(ListForm, 'INCREMENTAL_SEARCH',
        "Prefixov� inkrement�ln� hled�n� z�znamu.")
Command(ListForm, 'FULL_INCREMENTAL_SEARCH',
        "Pln� inkrement�ln� hled�n� z�znamu.")
Command(ListForm, 'EDIT',
        "Vyvol�n� inline editace aktu�ln� bu�ky.")
Command(ListForm, 'COPY_CELL',
        "Zkop�rov�n� obsahu aktu�ln� bu�ky do clipboardu.")
Command(ListForm, 'FILTER_BY_CELL',
        "Vyfiltrov�n� formul��e podle hodnoty aktu�ln� bu�ky.")
Command(ListForm, 'EXPORT_CSV',
        "Export ��dkov�ho formul��e do csv souboru.")
Command(ListForm, 'LINE_COMMIT',
        "Dokon�en� editace z�znamu (ulo�en�).")
Command(ListForm, 'LINE_ROLLBACK',
        "Kompletn� zru�en� editace z�znamu.")
Command(ListForm, 'LINE_SOFT_ROLLBACK',
        "Kompletn� zru�en� editace zat�m nezm�n�n�ho z�znamu.")
Command(ListForm, 'FINISH_EDITING',
        "Opu�t�n� editace ��dku.")
Command(ListForm, 'CELL_COMMIT',
        "Ukon�en� editace pol��ka s�novou hodnotou.")
Command(ListForm, 'CELL_ROLLBACK',
        "Ukon�en� editace pol��ka s�vr�cen�m p�vodn� hodnoty.")
Command(ListForm, 'NEW_LINE_AFTER',
        "Vlo�en� nov�ho z�znamu za aktu�ln� ��dek.")
Command(ListForm, 'NEW_LINE_AFTER_COPY',
        "Vlo�en� z�znamu za aktu�ln� ��dek jako jeho kopie.")
Command(ListForm, 'NEW_LINE_BEFORE',
        "Vlo�en� nov�ho z�znamu p�ed aktu�ln� ��dek.")
Command(ListForm, 'NEW_LINE_BEFORE_COPY',
        "Vlo�en� z�znamu p�ed aktu�ln� ��dek jako jeho kopie.")
Command(ListForm, 'SET_GROUPING_COLUMN',
        "Zm�na sloupce vizu�n�ho seskupov�n�.")
Command(ListForm, 'ENLARGE_COLUMN',
        "Roz���en� sloupce.")
Command(ListForm, 'CONTRACT_COLUMN',
        "Z��en� sloupce.")
Command(ListForm, 'TOGGLE_COLUMN',
        "Skryt�/zobrazen� sloupce.")
Command(ListForm, 'RESET_COLUMNS',
        "Vr�cen� v�choz�ho nastaven� sloupc�.")
Command(BrowseForm, 'IMPORT_INTERACTIVE',
        "Interaktivn� import dat z CSV souboru.")
Command(EditForm, 'COMMIT_RECORD',
        "Ukon�en� edita�n�ho formul��e s ulo�en�m zm�n.")
Command(EditForm, 'NAVIGATE',
        "Navigace mezi pol��ky edita�n�ho formul��e.")
Command(EditForm, 'NAVIGATE_BACK',
        "Zp�tn� navigace mezi pol��ky edita�n�ho formul��e.")
Command(BrowsableShowForm, 'NEXT_RECORD',
        "P�echod na dal�� z�znam.")
Command(BrowsableShowForm, 'PREVIOUS_RECORD',
        "P�echod na p�edchoz� z�znam.")
Command(BrowsableShowForm, 'FIRST_RECORD',
        "P�echod na prvn� z�znam.")
Command(BrowsableShowForm, 'LAST_RECORD',
        "P�echod na posledn� z�znam.")
Command(DualForm, 'OTHER_FORM',
        "P�echod mezi podformul��i du�ln�ho formul��e.")
Command(PrintForm, 'NEXT_PAGE',
        "P�echod na dal�� str�nku tiskov�ho n�hledu.")
Command(PrintForm, 'PREVIOUS_PAGE',
        "P�echod na p�edchoz� str�nku tiskov�ho n�hledu.")
Command(InputField, 'RESET_FIELD',
        "Vr�cen� p�vodn� hodnoty vstupn�ho pol��ka.")
Command(InputField, 'COMMIT_FIELD',
        "�sp�n� ukon�en� editace vstupn�ho pol��ka.")
Command(InputField, 'LEAVE_FIELD',
        "Odchod z editace vstupn�ho pol��ka.")
Command(Invocable, 'INVOKE_SELECTION',
        "Vyvol�n� v�b�ru hodnoty vstupn�ho pol��ka.")
Command(Invocable, 'INVOKE_SELECTION_ALTERNATE',
        "Vyvol�n� alternativn�ho v�b�ru hodnoty pol��ka.")
Command(GenericCodebookField, 'INVOKE_CODEBOOK_FORM',
        "Vyvol�n� alternativn�ho v�b�ru hodnoty pol��ka.")
Command(ListField, 'SELECT',
        "V�b�r aktu�ln�ho z�znamu.")
Command(ListField, 'SHOW_SELECTED',
        "V�b�r aktu�ln�ho z�znamu.")
Command(ListField, 'INVOKE_EDIT_FORM',
        "Vyvol�n� edita�n�ho formul��e nad akt. z�znamem.")
Command(ListField, 'INVOKE_BROWSE_FORM',
        "Zobrazen� aktu�ln�ho z�znamu v nov�m formul��i.")
Command(Dialog, 'CLOSE_DIALOG',
        "Opu�t�n� dialogu bez potvrzen�.")
Command(Dialog, 'COMMIT_DIALOG',
        "Potvrzen� dialogu.")
Command(Dialog, 'FORCE_COMMIT_DIALOG',
        "Odesl�n� dialogu, jako by bylo stisknuto v�choz� tla��tko.")


DEFAULT_COMMAND_KEYS = (
    (Application.COMMAND_BREAK,                   'Ctrl-g'),
    (Application.COMMAND_LEAVE_FORM,              'Escape'),
    (Application.COMMAND_NEXT_FORM,               'Ctrl-Down'),
    (Application.COMMAND_PREV_FORM,               'Ctrl-Up'),
    (Application.COMMAND_REFRESH,                 'Ctrl-l'),
    (Application.COMMAND_SHOW_POPUP_MENU,         'Ctrl-M'),
    (Form.COMMAND_PRINT,                         ('Ctrl-x', 'p')),
    (RecordForm.COMMAND_NEW_RECORD,               'F6'),
    (RecordForm.COMMAND_NEW_RECORD_COPY,          'Ctrl-F6'),
    (RecordForm.COMMAND_EDIT_RECORD,              'F5'),
    (RecordForm.COMMAND_DELETE_RECORD,            'F8'),
    (LookupForm.COMMAND_SORT_COLUMN,              'F4'),
    (LookupForm.COMMAND_FILTER,                   'Ctrl-F4'),
    (LookupForm.COMMAND_SEARCH_NEXT,              'Ctrl-s'),
    (LookupForm.COMMAND_SEARCH_PREVIOUS,          'Ctrl-r'),
    (LookupForm.COMMAND_SEARCH,                   'F3'),
    (LookupForm.COMMAND_JUMP,                     'Ctrl-j'),
    (ListForm.COMMAND_INCREMENTAL_SEARCH,         'Ctrl-F3'),
    (ListForm.COMMAND_FULL_INCREMENTAL_SEARCH,   ('Ctrl-u', 'Ctrl-F3')),
    (ListForm.COMMAND_ACTIVATE,                   'Enter'),
    (ListForm.COMMAND_ACTIVATE_ALTERNATE,         ' '),
    (ListForm.COMMAND_COPY_CELL,                  'Ctrl-c'),
    (ListForm.COMMAND_FIRST_COLUMN,               'Home'),
    (ListForm.COMMAND_LAST_COLUMN,                'End'),
    (ListForm.COMMAND_EXPORT_CSV,                 'Ctrl-e'),
    (ListForm.COMMAND_EDIT,                       'F2'),
    (ListForm.COMMAND_EDIT,                       'F9'), # backw. compatibility
    (ListForm.COMMAND_LINE_ROLLBACK,              'Ctrl-F12'),
    (ListForm.COMMAND_FINISH_EDITING,             'Escape'),
    (ListForm.COMMAND_LINE_COMMIT,                'F12'),
    (ListForm.COMMAND_CELL_COMMIT,                'Enter'),
    (ListForm.COMMAND_CELL_ROLLBACK,              'Escape'),
    (ListForm.COMMAND_NEW_LINE_AFTER,             'Insert'),
    (ListForm.COMMAND_NEW_LINE_AFTER_COPY,        'F7'),
    (ListForm.COMMAND_NEW_LINE_BEFORE,            'Ctrl-Insert'),
    (ListForm.COMMAND_NEW_LINE_BEFORE_COPY,       'Ctrl-F7'),
    (ListForm.COMMAND_ENLARGE_COLUMN,             'Alt-Right'),
    (ListForm.COMMAND_CONTRACT_COLUMN,            'Alt-Left'),
    (BrowseForm.COMMAND_IMPORT_INTERACTIVE,       'Alt-F6'),
    (EditForm.COMMAND_COMMIT_RECORD,              'Ctrl-Enter'),
    (EditForm.COMMAND_NAVIGATE,                   'Tab'),
    (EditForm.COMMAND_NAVIGATE_BACK,              'Shift-Tab'),        
    (BrowsableShowForm.COMMAND_NEXT_RECORD,       'Next'),
    (BrowsableShowForm.COMMAND_PREVIOUS_RECORD,   'Prior'),
    (BrowsableShowForm.COMMAND_FIRST_RECORD,      'Home'),
    (BrowsableShowForm.COMMAND_LAST_RECORD,       'End'),
    (DualForm.COMMAND_OTHER_FORM,                 'Ctrl-Tab'),
    (PrintForm.COMMAND_NEXT_PAGE,                 'Next'),
    (PrintForm.COMMAND_PREVIOUS_PAGE,             'Prior'),
    (InputField.COMMAND_COMMIT_FIELD,             'Enter'),
    (InputField.COMMAND_LEAVE_FIELD,              'Escape'),
    (Invocable.COMMAND_INVOKE_SELECTION,          'F2'),
    (Invocable.COMMAND_INVOKE_SELECTION_ALTERNATE,'Ctrl-F2'),
    (ListField.COMMAND_SHOW_SELECTED,             'Backspace'),  
    (Dialog.COMMAND_CLOSE_DIALOG,                 'Escape'),
    (Dialog.COMMAND_COMMIT_DIALOG,                'Enter'),
    (Dialog.COMMAND_FORCE_COMMIT_DIALOG,          'Ctrl-Enter'))



if __debug__:
    Command(Application, 'CUSTOM_DEBUG',
            "Pomocn� p��kaz pro vyvol�n� pomocn� lad�c� funkce.")
    DEFAULT_COMMAND_KEYS += \
        ((Application.COMMAND_CUSTOM_DEBUG, 'Ctrl-Backspace'),)


FORM_COMMAND_MENU = ((
    (_("P�edchoz� okno"),             Application.COMMAND_PREV_FORM),
    (_("N�sleduj�c� okno"),           Application.COMMAND_NEXT_FORM),
    (_("Zav��t aktu�ln� okno"),       Application.COMMAND_LEAVE_FORM),
    ),(#---------------
    (_("Skok na z�znam"),             LookupForm.COMMAND_JUMP),
    (_("Hledat"),                     LookupForm.COMMAND_SEARCH),
    (_("Hledat dal��"),               LookupForm.COMMAND_SEARCH_NEXT),
    (_("Hledat p�edchoz�"),           LookupForm.COMMAND_SEARCH_PREVIOUS),
    (_("Inkrement�ln� hled�n�"),      ListForm.COMMAND_INCREMENTAL_SEARCH),
    (_("Inkrement�ln� hled�n� pod�et�zce"),
                                      ListForm.COMMAND_FULL_INCREMENTAL_SEARCH),
    ),(#---------------
    (_("T��d�n�"),                    LookupForm.COMMAND_SORT_COLUMN),
    (_("Filtrov�n�"),                 LookupForm.COMMAND_FILTER),
    ),(#---------------
    (_("Nov� z�znam"),                BrowseForm.COMMAND_NEW_RECORD),
    (_("Nov� z�znam - kopie"),        BrowseForm.COMMAND_NEW_RECORD_COPY),
    (_("Editovat z�znam"),            BrowseForm.COMMAND_EDIT_RECORD),
    (_("Vlo�it ��dku nad"),           ListForm.COMMAND_NEW_LINE_BEFORE),
    (_("Vlo�it ��dku pod"),           ListForm.COMMAND_NEW_LINE_AFTER),
    (_("Kop�rovat ��dku nad"),        ListForm.COMMAND_NEW_LINE_BEFORE_COPY),
    (_("Kop�rovat ��dku pod"),        ListForm.COMMAND_NEW_LINE_AFTER_COPY),
    (_("Editace bu�ky"),              ListForm.COMMAND_EDIT),
    (_("Smazat z�znam"),              RecordForm.COMMAND_DELETE_RECORD),
    ),(#---------------
    (_("Ulo�it"),                     ListForm.COMMAND_LINE_COMMIT),
    (_("Zru�it zm�ny"),               ListForm.COMMAND_LINE_ROLLBACK),
    ),(#---------------
    (_("Export do textov�ho souboru"),ListForm.COMMAND_EXPORT_CSV),
    ),(#---------------
    (_("Zobrazit n�hled z�znamu"),    ListForm.COMMAND_ACTIVATE),
    (_("N�hled v du�ln�m formul��i"), ListForm.COMMAND_ACTIVATE_ALTERNATE),
    ))
