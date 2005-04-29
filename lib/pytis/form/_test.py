#!/usr/bin/env python
# -*- coding: iso-8859-2 -*-

# Copyright (C) 2001, 2002, 2005 Brailcom, o.p.s.
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

import locale

from pytis.util import *
import wx
import pytis.util.test

locale.setlocale(locale.LC_NUMERIC, 'C')

tests = pytis.util.test.TestSuite()

#================#
# list.py        #
#================#

# Co je potřeba (bohužel ručně, protože tvorba testů je soustavně
# ignorována :-((((((( ) otestovat po změnách v inline editaci:
#
# - Vstup do editace políčka patřičnou klávesou.
# - Do editace políčka nelze vstoupit žádnou jinou klávesou ani clickem či
#   double-clickem.
# - Zrušení editace políčka s vrácením obsahu klávesou Escape.
# - Po zrušení editace políčka kurzor zůstane na onom políčku.
# - Potvrzení editace políčka klávesou Enter.
# - Po potvrzení editace políčka se kurzor přesune na další políčko vpravo.
#   Pokud takové políčko již není, pak: Jedná-li se o editaci existujícího
#   řádku, kurzor přeskočí na první sloupec téhož řádku; jedná-li se o nový
#   řádek, kurzor se zeptá na potvrzení jeho vložení -- je-li zamítnuto, skočí
#   na první sloupec, jinak je vložen další nový řádek (čistý nebo kopie, podle
#   způsobu posledního vložení nového řádku) a kurzor skočí na jeho první
#   sloupec.
# - Je-li editován nový řádek, po přesunu kurzoru po potvrzení políčka je nové
#   políčko v režimu editace.
# - Myší klik přesune kurzor na kliknuté políčko a vyselektuje řádek tohoto
#   políčka, bez dalších akcí (pokud se nejedná o přerušení editace).
# - Při jakémkoliv pokusu o opuštění rozeditovaného řádku naskočí dialog
#   s dotazem na zrušení editace.  Při kladné odpovědi jsou vráceny původní
#   hodnoty řádku a provedena požadovaná akce, v opačném případě akce provedena
#   není a kurzor zůstane na své pozici.
# - Výjimka: Pokud výsledkem editace nejsou žádné změny (týká se editace
#   existujícího i nového řádku), dialog nenaskakuje a akce se provede.
# - Do těchto pokusů o opuštění editace se počítá i opuštění formuláře a
#   ukončení aplikace, avšak s tím rozdílem, že se nelze vrátit do editace, je
#   ji možno pouze potrvdit nebo stornovat.
# - Při zrušení nového editovaného řádku dojde ke správnému určení počtu řádku
#   gridu.
# - Lze vložit nový prázdný řádek patřičnou klávesou před aktuální záznam.
# - Lze vložit nový prázdný řádek patřičnou klávesou za aktuální záznam.
# - Lze vložit kopii aktuálního řádku patřičnou klávesou za aktuální záznam.
# - Při jakémkoliv založení nového řádku je kurzor na jeho prvním sloupci a
#   ono políčko je automaticky v režimu editace.
# - Klávesa uložení řádku (nebo obdobná akce provedená přes dialog s dotazem)
#   řádek uložení provede.
# - Klávesa smazání řádku smazání provede.
# - Při smazání editovaného řádku dojde ke správnému určení počtu řádků gridu.
# - Při zadání nesprávné hodnoty do políčka naskočí ihned po potvrzení políčka
#   varovné dialogové okno a po jeho odklepnutí dojde k návratu do editace
#   políčka.


################


def get_tests():
    return tests

if __name__ == '__main__':
    unittest.main(defaultTest='get_tests',
                  argv=pytis.util.test.transform_args())
