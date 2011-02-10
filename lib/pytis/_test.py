#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (C) 2001, 2004, 2005 Brailcom, o.p.s.
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


tests = unittest.TestSuite()
for module_name in ('pytis.data._test', 'pytis.util._test'):
    module = __import__(module_name, globals(), locals(), ['get_tests'])
    tests.addTest(module.get_tests())


def get_tests():
    return tests

if __name__ == '__main__':
    unittest.main(defaultTest='get_tests')
