#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (C) 2015, 2016 Brailcom, o.p.s.
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

import unittest
import pytis.util.test

TESTED_MODULES = (
    'pytis.data',
    'pytis.presentation',
    'pytis.util',
    'pytis.remote',
    'pytis.web',
)

tests = pytis.util.test.TestSuite()
for module_name in TESTED_MODULES:
    module = __import__(module_name + '._test', globals(), locals(), ['get_tests'])
    if hasattr(module, 'get_tests'):
        tests.addTest(module.get_tests())
    else:
        for attr in dir(module):
            obj = getattr(module, attr)
            if isinstance(obj, type) and issubclass(obj, unittest.TestCase):
                tests.add(obj)


def get_tests():
    return tests


if __name__ == '__main__':
    unittest.main(defaultTest='get_tests')
