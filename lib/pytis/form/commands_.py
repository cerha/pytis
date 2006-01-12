# -*- coding: iso-8859-2 -*-

# Definice u�ivatelsk�ch p��kaz�
# 
# Copyright (C) 2002, 2003, 2004, 2005, 2006 Brailcom, o.p.s.
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
Command(Application, 'CLEAR_RECENT_FORMS',
        "Vy�isti menu posledn� otev�en�ch formul���.")
Command(Form, 'PRINT',
        "Tisk aktu�ln�ho obsahu formul��e.")
Command(Form, 'RELOAD_FORM_STATE',
        "Zapome� u�iv. nastaven� formul��e a vra� posledn� ulo�en� hodnoty.")
Command(LookupForm, 'FILTER',
        "Filtrov�n� z�znam�.")
Command(LookupForm, 'JUMP',
        "Skok na z�znam.")
Command(LookupForm, 'SEARCH',
        "Hled�n� z�znamu.")
Command(LookupForm, 'SORT_COLUMN',
        "Set��d�n� podle sloupce.")
Command(RecordForm, 'NEW_RECORD',
        "Vlo�en� nov�ho z�znamu pomoc� edita�n�ho formul��e.")
Command(RecordForm, 'IMPORT_INTERACTIVE',
        "Interaktivn� import dat z CSV souboru.")
Command(RecordForm, 'EDIT_RECORD',
        "Editace aktu�ln�ho z�znamu v edita�n�m formul��i.")
Command(RecordForm, 'DELETE_RECORD',
        "Vymaz�n� editovan�ho z�znamu z�datab�ze.")
Command(ListForm, 'ACTIVATE',
        "Aktiva�n� funkce pro aktu�ln� ��dek formul��e. (arg. 'alternate')")
Command(ListForm, 'SHOW_CELL_CODEBOOK',
        "Vyvol�n� ��seln�ku aktivn� bu�ky ��dkov�ho formul��e.")
Command(ListForm, 'SELECT_CELL',
        "V�b�r bu�ky seznamu.")
Command(ListForm, 'FIRST_COLUMN',
        "P�echod na prvn� sloupec tabulky.")
Command(ListForm, 'LAST_COLUMN',
        "P�echod na posledn� sloupec tabulky.")
Command(ListForm, 'INCREMENTAL_SEARCH',
        "Prefixov� inkrement�ln� hled�n� z�znamu. (arg. 'full')")
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
        "Kompletn� zru�en� editace z�znamu. (arg. 'soft')")
Command(ListForm, 'FINISH_EDITING',
        "Opu�t�n� editace ��dku.")
Command(ListForm, 'CELL_COMMIT',
        "Ukon�en� editace pol��ka s�novou hodnotou.")
Command(ListForm, 'CELL_ROLLBACK',
        "Ukon�en� editace pol��ka s�vr�cen�m p�vodn� hodnoty.")
Command(ListForm, 'NEW_LINE',
        "In-line vlo�en� nov�ho z�znamu. (arg. 'before' a 'copy')")
Command(ListForm, 'SET_GROUPING_COLUMN',
        "Zm�na sloupce vizu�n�ho seskupov�n�.")
Command(ListForm, 'RESIZE_COLUMN',
        "Roz���en�/z��en� sloupce.  Argument 'diff' ud�v� po�et +/- pixel�.")
Command(ListForm, 'MOVE_COLUMN',
        "P�esunut� sloupce doprava/doleva.  'diff' ud�v� +/- o kolik sloupc�.")
Command(ListForm, 'TOGGLE_COLUMN',
        "Skryt�/zobrazen� sloupce.")
Command(ListForm, 'RESET_COLUMNS',
        "Vr�cen� v�choz�ho nastaven� sloupc�.")
Command(ListForm, 'CONTEXT_MENU_ACTION',
        """Akce kontextov�ho menu ��dku tabulky.
        
	Je ur�en k vytv��en� u�ivatelem definovan�ch akc� v kontextov�m menu
	��dku BrowseFormu.

	P��kaz m��e m�t libovoln� argumenty, av�ak dva z nich jsou
	zpracov�ny odli�n�:

           `handler' -- je povinn� a mus� odkazovat na funkci, kter� akci
              provede.

           `enabled' -- funkce, vracej�c� pravdu, pokud je p��kaz aktivn� a
              nepravdu v opa�n�m p��pad�.  Argument je nepovinn� a pokud nen�
              uveden, je p��kaz v�dy aktivn�.

	Funkce `handler' i `enabled' dostanou jako kl��ov� argumenty v�echny
	ostatn� argumenty p��kazu, vyjma sebe sama (argument� `handler' a
	`enabled').

        Nav�c, pokud tyto funkce p�ij�maj� jeden pozi�n� argument,
	dostanou jako prvn� argument instanci PresentedRow aktu�ln�ho ��dku,
	nad kter�m byla akce vyvol�na.  Pokud p�ij�maj� dva pozi�n� argumenty,
	bude na prvn�m m�st� p�ed�n je�t� datov� objekt formul��e.""")
Command(EditForm, 'COMMIT_RECORD',
        "Ukon�en� edita�n�ho formul��e s ulo�en�m zm�n.")
Command(EditForm, 'NAVIGATE',
        "Navigace mezi pol��ky edita�n�ho formul��e. (arg. 'back')")
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
        "Vyvol�n� v�b�ru hodnoty vstupn�ho pol��ka. (arg. 'alternate')")
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
        "Potvrzen� dialogu.  Arg. 'force' simuluje stisk v�choz�ho tla��tka.")

DEFAULT_KEYMAP = (
    ('Ctrl-g',      Application.COMMAND_BREAK),
    ('Escape',      Application.COMMAND_LEAVE_FORM),
    ('Ctrl-Down',   Application.COMMAND_NEXT_FORM),
    ('Ctrl-Up',     Application.COMMAND_PREV_FORM),
    ('Ctrl-l',      Application.COMMAND_REFRESH),
    ('Ctrl-M',      Application.COMMAND_SHOW_POPUP_MENU),
    (('Ctrl-x', 'p'),  Form.COMMAND_PRINT),
    ('Ctrl-Backspace', Form.COMMAND_RELOAD_FORM_STATE),
    ('F6',          RecordForm.COMMAND_NEW_RECORD),
    ('Ctrl-F6',     RecordForm.COMMAND_NEW_RECORD(copy=True)),
    ('Alt-F6',      RecordForm.COMMAND_IMPORT_INTERACTIVE),
    ('F5',          RecordForm.COMMAND_EDIT_RECORD),
    ('F8',          RecordForm.COMMAND_DELETE_RECORD),
    ('F4',          LookupForm.COMMAND_SORT_COLUMN),
    ('Ctrl-F4',     LookupForm.COMMAND_FILTER),
    ('F3',          LookupForm.COMMAND_SEARCH),
    ('Ctrl-s',      LookupForm.COMMAND_SEARCH(direction=pytis.data.FORWARD)),
    ('Ctrl-r',      LookupForm.COMMAND_SEARCH(direction=pytis.data.BACKWARD)),
    ('Ctrl-j',      LookupForm.COMMAND_JUMP),
    ('Ctrl-F3',     ListForm.COMMAND_INCREMENTAL_SEARCH),
    ('Alt-F3',      ListForm.COMMAND_INCREMENTAL_SEARCH(full=True)),
    ('Enter',       ListForm.COMMAND_ACTIVATE),
    (' ',           ListForm.COMMAND_ACTIVATE(alternate=True)),
    ('Ctrl-c',      ListForm.COMMAND_COPY_CELL),
    ('Home',        ListForm.COMMAND_FIRST_COLUMN),
    ('End',         ListForm.COMMAND_LAST_COLUMN),
    ('Ctrl-e',      ListForm.COMMAND_EXPORT_CSV),
    ('F2',          ListForm.COMMAND_EDIT),
    ('F9',          ListForm.COMMAND_EDIT),
    ('Ctrl-F12',    ListForm.COMMAND_LINE_ROLLBACK),
    ('Escape',      ListForm.COMMAND_FINISH_EDITING),
    ('F12',         ListForm.COMMAND_LINE_COMMIT),
    ('Enter',       ListForm.COMMAND_CELL_COMMIT),
    ('Escape',      ListForm.COMMAND_CELL_ROLLBACK),
    ('Insert',      ListForm.COMMAND_NEW_LINE()),
    ('F7',          ListForm.COMMAND_NEW_LINE(copy=True)),
    ('Ctrl-Insert', ListForm.COMMAND_NEW_LINE(before=True)),
    ('Ctrl-F7',     ListForm.COMMAND_NEW_LINE(before=True, copy=True)),
    ('Alt-Right',   ListForm.COMMAND_RESIZE_COLUMN(diff=+5)),
    ('Alt-Left',    ListForm.COMMAND_RESIZE_COLUMN(diff=-5)),
    ('Shift-Right', ListForm.COMMAND_MOVE_COLUMN(diff=+1)),
    ('Shift-Left',  ListForm.COMMAND_MOVE_COLUMN(diff=-1)),
    ('Ctrl-Enter',  EditForm.COMMAND_COMMIT_RECORD),
    ('Tab',         EditForm.COMMAND_NAVIGATE),
    ('Shift-Tab',   EditForm.COMMAND_NAVIGATE(back=True)),
    ('Next',        BrowsableShowForm.COMMAND_NEXT_RECORD),
    ('Prior',       BrowsableShowForm.COMMAND_PREVIOUS_RECORD),
    ('Home',        BrowsableShowForm.COMMAND_FIRST_RECORD),
    ('End',         BrowsableShowForm.COMMAND_LAST_RECORD),
    ('Ctrl-Tab',    DualForm.COMMAND_OTHER_FORM),
    ('Next',        PrintForm.COMMAND_NEXT_PAGE),
    ('Prior',       PrintForm.COMMAND_PREVIOUS_PAGE),
    ('Enter',       InputField.COMMAND_COMMIT_FIELD),
    ('Escape',      InputField.COMMAND_LEAVE_FIELD),
    ('F2',          Invocable.COMMAND_INVOKE_SELECTION),
    ('Ctrl-F2',     Invocable.COMMAND_INVOKE_SELECTION(alternate=True)),
    ('Backspace',   ListField.COMMAND_SHOW_SELECTED),
    ('Escape',      Dialog.COMMAND_CLOSE_DIALOG),
    ('Enter',       Dialog.COMMAND_COMMIT_DIALOG),
    ('Ctrl-Enter',  Dialog.COMMAND_COMMIT_DIALOG(force=True)),
    )



if __debug__:
    Command(Application, 'CUSTOM_DEBUG',
            "Pomocn� p��kaz pro vyvol�n� pomocn� lad�c� funkce.")
    DEFAULT_KEYMAP += \
        (('Ctrl-Backspace', Application.COMMAND_CUSTOM_DEBUG),)


FORM_COMMAND_MENU = ((
    (_("P�edchoz� okno"),       Application.COMMAND_PREV_FORM),
    (_("N�sleduj�c� okno"),     Application.COMMAND_NEXT_FORM),
    (_("Zav��t aktu�ln� okno"), Application.COMMAND_LEAVE_FORM),
    ),(#---------------
    (_("Skok na z�znam"),       LookupForm.COMMAND_JUMP),
    (_("Hledat"),               LookupForm.COMMAND_SEARCH),
    (_("Hledat dal��"),         LookupForm.COMMAND_SEARCH(direction=
                                                          pytis.data.FORWARD)),
    (_("Hledat p�edchoz�"),     LookupForm.COMMAND_SEARCH(direction=
                                                          pytis.data.BACKWARD)),
    (_("Inkrement�ln� hled�n�"), ListForm.COMMAND_INCREMENTAL_SEARCH),
    (_("Inkrement�ln� hled�n� pod�et�zce"),
                                ListForm.COMMAND_INCREMENTAL_SEARCH(full=True)),
    ),(#---------------
    (_("T��d�n�"),              LookupForm.COMMAND_SORT_COLUMN),
    (_("Filtrov�n�"),           LookupForm.COMMAND_FILTER),
    ),(#---------------
    (_("Nov� z�znam"),          BrowseForm.COMMAND_NEW_RECORD),
    (_("Nov� z�znam - kopie"),  BrowseForm.COMMAND_NEW_RECORD(copy=True)),
    (_("Editovat z�znam"),      BrowseForm.COMMAND_EDIT_RECORD),
    (_("Vlo�it ��dku pod"),     ListForm.COMMAND_NEW_LINE()),
    (_("Vlo�it ��dku nad"),     ListForm.COMMAND_NEW_LINE(before=True)),
    (_("Kop�rovat ��dku pod"),  ListForm.COMMAND_NEW_LINE(copy=True)),
    (_("Kop�rovat ��dku nad"),  ListForm.COMMAND_NEW_LINE(copy=True,
                                                          before=True)),
    (_("Editace bu�ky"),        ListForm.COMMAND_EDIT),
    (_("Smazat z�znam"),        RecordForm.COMMAND_DELETE_RECORD),
    ),(#---------------
    (_("Ulo�it"),               ListForm.COMMAND_LINE_COMMIT),
    (_("Zru�it zm�ny"),         ListForm.COMMAND_LINE_ROLLBACK),
    ),(#---------------
    (_("Export do textov�ho souboru"),ListForm.COMMAND_EXPORT_CSV),
    ),(#---------------
    (_("Zobrazit n�hled z�znamu"),    ListForm.COMMAND_ACTIVATE),
    (_("N�hled v du�ln�m formul��i"), ListForm.COMMAND_ACTIVATE(alternate=True)),
    ))
