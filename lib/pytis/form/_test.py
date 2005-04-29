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

# Co je pot�eba (bohu�el ru�n�, proto�e tvorba test� je soustavn�
# ignorov�na�:-((((((( ) otestovat po zm�n�ch v�inline editaci:
#
# - Vstup do editace pol��ka pat�i�nou kl�vesou.
# - Do editace pol��ka nelze vstoupit ��dnou jinou kl�vesou ani clickem �i
#   double-clickem.
# - Zru�en� editace pol��ka s�vr�cen�m obsahu kl�vesou Escape.
# - Po zru�en� editace pol��ka kurzor z�stane na onom pol��ku.
# - Potvrzen� editace pol��ka kl�vesou Enter.
# - Po potvrzen� editace pol��ka se kurzor p�esune na dal�� pol��ko vpravo.
#   Pokud takov� pol��ko ji� nen�, pak: Jedn�-li se o�editaci existuj�c�ho
#   ��dku, kurzor p�esko�� na prvn� sloupec t�ho� ��dku; jedn�-li se o�nov�
#   ��dek, kurzor se zept� na potvrzen� jeho vlo�en� -- je-li zam�tnuto, sko��
#   na prvn� sloupec, jinak je vlo�en dal�� nov� ��dek (�ist� nebo kopie, podle
#   zp�sobu posledn�ho vlo�en� nov�ho ��dku) a kurzor sko�� na jeho prvn�
#   sloupec.
# - Je-li editov�n nov� ��dek, po p�esunu kurzoru po potvrzen� pol��ka je nov�
#   pol��ko v�re�imu editace.
# - My�� klik p�esune kurzor na kliknut� pol��ko a vyselektuje ��dek tohoto
#   pol��ka, bez dal��ch akc� (pokud se nejedn� o�p�eru�en� editace).
# - P�i jak�mkoliv pokusu o�opu�t�n� rozeditovan�ho ��dku nasko�� dialog
#   s�dotazem na zru�en� editace.  P�i kladn� odpov�di jsou vr�ceny p�vodn�
#   hodnoty ��dku a provedena po�adovan� akce, v�opa�n�m p��pad� akce provedena
#   nen� a kurzor z�stane na sv� pozici.
# - V�jimka: Pokud v�sledkem editace nejsou ��dn� zm�ny (t�k� se editace
#   existuj�c�ho i�nov�ho ��dku), dialog nenaskakuje a akce se provede.
# - Do t�chto pokus� o�opu�t�n� editace se po��t� i�opu�t�n� formul��e a
#   ukon�en� aplikace, av�ak s�t�m rozd�lem, �e se nelze vr�tit do editace, je
#   ji mo�no pouze potrvdit nebo stornovat.
# - P�i zru�en� nov�ho editovan�ho ��dku dojde ke spr�vn�mu ur�en� po�tu ��dku
#   gridu.
# - Lze vlo�it nov� pr�zdn� ��dek pat�i�nou kl�vesou p�ed aktu�ln� z�znam.
# - Lze vlo�it nov� pr�zdn� ��dek pat�i�nou kl�vesou za aktu�ln� z�znam.
# - Lze vlo�it kopii aktu�ln�ho ��dku pat�i�nou kl�vesou za aktu�ln� z�znam.
# - P�i jak�mkoliv zalo�en� nov�ho ��dku je kurzor na jeho prvn�m sloupci a
#   ono pol��ko je automaticky v�re�imu editace.
# - Kl�vesa ulo�en� ��dku (nebo obdobn� akce proveden� p�es dialog s�dotazem)
#   ��dek ulo�en� provede.
# - Kl�vesa smaz�n� ��dku smaz�n� provede.
# - P�i smaz�n� editovan�ho ��dku dojde ke spr�vn�mu ur�en� po�tu ��dk� gridu.
# - P�i zad�n� nespr�vn� hodnoty do pol��ka nasko�� ihned po potvrzen� pol��ka
#   varovn� dialogov� okno a po jeho odklepnut� dojde k�n�vratu do editace
#   pol��ka.


################


def get_tests():
    return tests

if __name__ == '__main__':
    unittest.main(defaultTest='get_tests',
                  argv=pytis.util.test.transform_args())
