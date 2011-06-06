# -*- coding: iso-8859-2 -*-

# Copyright (C) 2001, 2002, 2004, 2005, 2006, 2011 Brailcom, o.p.s.
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

"""Pom�cky pro testov�n�.

Modul definuje t��dy, kter� m�rn� usnad�uj� vytv��en� sad test� v�na�ich
souborech '_test.py'.

"""
# Tento modul je vy�len�n z�util.py, aby je ne�inil z�visl�m na PyUnit, kter�
# nen� pro b�h syst�mu zapot�eb�.


import string
import sys
import types
import unittest

from pytis.util import *


class TestSuite(unittest.TestSuite):
    """M�rn� vylep�en� t��da 'unittest.TestSuite'.

    C�lem t��dy je je�t� v�ce zestru�nit n�kter� �asto pou��van� operace
    v�na�em k�du.  Oproti pod�d�n� t��d� poskytuje dopl�uj�c� metodu 'add'.

    U�ite�n� (pouze) pro tvorbu test�.
    
    """
    def add(self, class_):
        """P�idej do test� v�echny metody t��dy 'class_' s�prefixem 'check_'.

        Nep�id�vej metody, kter� jsou beze zm�ny pod�d�ny od p�edka.  Je-li
        'config.run_interactive_tests' nepravda, nep�id�vej metody za��naj�c�
        prefixem 'check_interactive_'.
        
        Testy jsou p�id�ny jako samostatn� test suite.
        
        """
        tests = [x for x in direct_public_members(class_)
                 if x.startswith('test_')]
        import config
        if not config.test_run_interactive:
            tests = [x for x in tests if not x.startswith('test_interactive_')]
        methods = map(class_, tests)
        self.addTest(unittest.TestSuite(methods))


class InteractiveTestCase(unittest.TestCase):
    """Z�kladn� t��da pro interaktivn� testy.

    T��da definuje novou metodu 'ask_user()' pro interakci s�obsluhou
    testov�n�.
    
    """
    def _format_instructions(self, instructions):
        lines = string.split(instructions, '\n')
        formatted_lines = map(lambda l: '|' + l, lines)
        instructions = string.join(formatted_lines, '\n')
        return instructions
    
    def _print_instructions(self, instructions):
        print
        print instructions
        if instructions and instructions[-1] != '\n':
            print

    def _ask_question(self, question):
        if question:
            while 1:
                answer = raw_input(_('|%s (A/N)? ') % question)
                if not answer:
                    continue
                the_answer = string.lower(string.lstrip(answer)[0])
                if the_answer == 'a':
                    print
                    return True
                elif the_answer == 'n':
                    print
                    return False
        else:
            raw_input(_("Stiskni Enter..."))
            return True

    def ask_user(self, instructions, question):
        """Vypi� 'instructions' a zeptej se, zda test usp�l.

        Argumenty:

          instructions -- libovoln� string, kter� bude u�ivateli vyps�n jako
            instrukce, co m� ud�lat a co m� zkontrolovat
          question -- jedno��dkov� string jako ot�zka polo�en� u�ivateli nebo
            'None'; string se neukon�uje otazn�kem, ten bude dopln�n
            automaticky

        Jestli�e 'question' je 'None', jsou u�ivateli pouze vyps�ny
        'instructions' a u�ivatel je po��d�n o�odklepnut�.  Jestli�e 'question'
        je string, je u�ivatel po��d�n o�odpov�� ANO/NE.

        Metoda nic nevrac�, v�p��pad� kladn� odpov�di na ot�zku neud�l� nic
        zvl�tn�ho, v�p��pad� z�porn� odpov�di automaticky provede 'fail()'.

        """
        self._print_instructions(self._format_instructions(instructions))
        if not self._ask_question(question):
            self.fail()


def transform_args():
    """Vra� seznam ztransformovan�ch argument� p��kazov�ho ��dku.

    Jsou prov�d�ny n�sleduj�c� transformace:

    - Ka�d� argument, kter� odpov�d� potomku t��dy 'unittest.TestCase', nahra�
      seznamem metod t�to t��dy za��naj�c�ch prefixem 'check_'.

    """
    import __main__
    argv = sys.argv
    for i in range(len(argv))[1:]:
        try:
            object = eval('__main__.%s' % argv[i])
        except:
            continue
        if type(object) == types.ClassType and \
           issubclass(object, unittest.TestCase):
            a = []
            for m in dir(object):
                if starts_with (m, 'check_'):
                    a.append('%s.%s' % (argv[i], m))
            argv[i:i+1] = a
    return argv


def run_tests(module, tests=None, verbosity=2):
    """Spus� \"interaktivn�\" testy modulu 'module'.

    Argumenty:

      module -- testovan� modul, nap�. 'pytis.data'
      tests -- sekvence po�adovan�ch test� nebo 'None', ka�d� �len sekvence je
        string obsahuj�c� jm�no t��dy (bez modulu) nebo jm�no metody t��dy (bez
        modulu, ale v�etn� jm�na t��dy); je-li argument 'None', spust� se
        v�echny testy dan�ho modulu
      verbosity -- argument pro 'unittest.TextTestRunner', integer

    """
    test_module = module._test
    suite = unittest.TestSuite()
    if tests is None:
        test_case = test_module.get_tests()
        suite.addTest(test_case)
    else:
        for t in tests:
            class_method = t.split('.')
            test_class = getattr(test_module, class_method[0])
            if len(class_method) == 1:
                suite.addTest(unittest.makeSuite(test_class))
            else:
                suite.addTest(test_class(class_method[1]))
    unittest.TextTestRunner(verbosity=verbosity).run(suite)
