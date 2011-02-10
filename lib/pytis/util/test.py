# -*- coding: utf-8 -*-

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

"""Pomůcky pro testování.

Modul definuje třídy, které mírně usnadňují vytváření sad testů v našich
souborech '_test.py'.

"""
# Tento modul je vyčleněn z util.py, aby je nečinil závislým na PyUnit, které
# není pro běh systému zapotřebí.


import string
import sys
import types
import unittest

from pytis.util import *


class TestSuite(unittest.TestSuite):
    """Mírně vylepšená třída 'unittest.TestSuite'.

    Cílem třídy je ještě více zestručnit některé často používané operace
    v našem kódu.  Oproti poděděné třídě poskytuje doplňující metodu 'add'.

    Užitečné (pouze) pro tvorbu testů.
    
    """
    def add(self, class_):
        """Přidej do testů všechny metody třídy 'class_' s prefixem 'check_'.

        Nepřidávej metody, které jsou beze změny poděděny od předka.  Je-li
        'config.run_interactive_tests' nepravda, nepřidávej metody začínající
        prefixem 'check_interactive_'.
        
        Testy jsou přidány jako samostatná test suite.
        
        """
        tests = [x for x in direct_public_members(class_)
                 if x.startswith('test_')]
        import config
        if not config.test_run_interactive:
            tests = [x for x in tests if not x.startswith('test_interactive_')]
        methods = map(class_, tests)
        self.addTest(unittest.TestSuite(methods))


class InteractiveTestCase(unittest.TestCase):
    """Základní třída pro interaktivní testy.

    Třída definuje novou metodu 'ask_user()' pro interakci s obsluhou
    testování.
    
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
            raw_input(_(u"Stiskni Enter..."))
            return True

    def ask_user(self, instructions, question):
        """Vypiš 'instructions' a zeptej se, zda test uspěl.

        Argumenty:

          instructions -- libovolný string, který bude uživateli vypsán jako
            instrukce, co má udělat a co má zkontrolovat
          question -- jednořádkový string jako otázka položená uživateli nebo
            'None'; string se neukončuje otazníkem, ten bude doplněn
            automaticky

        Jestliže 'question' je 'None', jsou uživateli pouze vypsány
        'instructions' a uživatel je požádán o odklepnutí.  Jestliže 'question'
        je string, je uživatel požádán o odpověď ANO/NE.

        Metoda nic nevrací, v případě kladné odpovědi na otázku neudělá nic
        zvláštního, v případě záporné odpovědi automaticky provede 'fail()'.

        """
        self._print_instructions(self._format_instructions(instructions))
        if not self._ask_question(question):
            self.fail()


def transform_args():
    """Vrať seznam ztransformovaných argumentů příkazového řádku.

    Jsou prováděny následující transformace:

    - Každý argument, který odpovídá potomku třídy 'unittest.TestCase', nahraď
      seznamem metod této třídy začínajících prefixem 'check_'.

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
    """Spusť \"interaktivně\" testy modulu 'module'.

    Argumenty:

      module -- testovaný modul, např. 'pytis.data'
      tests -- sekvence požadovaných testů nebo 'None', každý člen sekvence je
        string obsahující jméno třídy (bez modulu) nebo jméno metody třídy (bez
        modulu, ale včetně jména třídy); je-li argument 'None', spustí se
        všechny testy daného modulu
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
