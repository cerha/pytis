#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (C) 2018-2019 Tomáš Cerha <t.cerha@gmail.com>
# Copyright (C) 2001-2018 Brailcom, o.p.s.
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


import operator
import unittest

from . import util
from . import caching


###########
# util.py #
###########


class Counter(unittest.TestCase):

    def test_it(self):
        counter = util.Counter()
        self.assertEqual(counter.current(), 0)
        self.assertEqual(counter.next(), 1)
        self.assertEqual(counter.next(), 2)
        self.assertEqual(counter.next(), 3)
        self.assertEqual(counter.current(), 3)
        counter.reset()
        self.assertEqual(counter.current(), 0)
        self.assertEqual(counter.next(), 1)
        self.assertEqual(counter.next(), 2)
        self.assertEqual(counter.current(), 2)


class Popen(unittest.TestCase):
    COMMAND = 'tr a b'
    SCOMMAND = ['tr', 'a', 'b']
    STRING = 'foobar'
    RSTRING = 'foobbr'

    def _test_auto(self, command):
        popen = util.Popen(command)
        from_, to = popen.from_child(), popen.to_child()
        to.write(self.STRING)
        to.close()
        self.assertEqual(from_.read(), self.RSTRING)

    def test_auto(self):
        self._test_auto(self.COMMAND)

    def test_command_as_sequence(self):
        self._test_auto(self.SCOMMAND)

    def test_explicit(self):
        cat = 'cat'
        popen1 = util.Popen(cat)
        popen3 = util.Popen(cat)
        util.Popen(self.COMMAND,
                   to_child=popen1.from_child(),
                   from_child=popen3.to_child())
        out = popen1.to_child()
        out.write(self.STRING)
        out.close()
        self.assertEqual(popen3.from_child().read(), self.RSTRING)


class Stack(unittest.TestCase):

    def test_it(self):
        stack = util.Stack()
        self.assertTrue(stack.empty())
        a = 'AAA'
        stack.push(a)
        self.assertFalse(stack.empty())
        self.assertIs(stack.top(), a)
        b = 'BBB'
        stack.push(b)
        self.assertFalse(stack.empty())
        self.assertIs(stack.top(), b)
        stack.pop()
        self.assertFalse(stack.empty())
        self.assertIs(stack.top(), a)
        stack.pop()
        self.assertTrue(stack.empty())


class XStack(unittest.TestCase):

    def test_it(self):
        xstack = util.XStack()
        self.assertTrue(xstack.empty())
        self.assertIsNone(xstack.active())
        self.assertIsNone(xstack.prev())
        self.assertIsNone(xstack.next())
        a = 'AAA'
        b = 'BBB'
        c = 'CCC'
        d = 'DDD'
        xstack.push(a)
        self.assertEqual(xstack.items(), (a,))
        self.assertIs(xstack.active(), a)
        self.assertIsNone(xstack.prev())
        self.assertIsNone(xstack.next())
        xstack.push(b)
        self.assertEqual(xstack.items(), (b, a,))
        self.assertIs(xstack.active(), b)
        self.assertIs(xstack.prev(), a)
        self.assertIs(xstack.next(), a)
        xstack.push(c)
        self.assertEqual(xstack.items(), (c, b, a,))
        self.assertIs(xstack.active(), c)
        self.assertIs(xstack.prev(), a)
        self.assertIs(xstack.next(), b)
        xstack.activate(a)
        self.assertIs(xstack.active(), a)
        self.assertIs(xstack.prev(), b)
        self.assertIs(xstack.next(), c)
        xstack.activate(b)
        self.assertIs(xstack.active(), b)
        self.assertIs(xstack.prev(), c)
        self.assertIs(xstack.next(), a)
        xstack.remove(b)
        self.assertEqual(xstack.items(), (c, a,))
        self.assertIs(xstack.active(), a)
        xstack.push(d)
        self.assertEqual(xstack.items(), (c, d, a,))
        self.assertIs(xstack.active(), d)
        xstack.pop()
        xstack.pop()
        self.assertIs(xstack.active(), c)
        self.assertIsNone(xstack.prev())
        self.assertIsNone(xstack.next())


class Sameclass(unittest.TestCase):

    class A(object):
        pass

    class B(object):
        pass

    def test_function(self):
        self.longMessage = True
        self.assertTrue(util.sameclass(Sameclass.A(), Sameclass.A()),
                        'same classes not recognized')
        self.assertFalse(util.sameclass(Sameclass.A(), Sameclass.B()),
                         'different classes not recognized')
        self.assertTrue(util.sameclass(1, 1), 'same inteeger classes not recognized')
        self.assertFalse(util.sameclass(1, 1.0), 'different classes not recognized')


class MiscFunctions(unittest.TestCase):

    def test_is_sequence(self):
        self.longMessage = True
        self.assertTrue(util.is_sequence((1, 2, 3)), 'sequence not a sequence')
        self.assertTrue(util.is_sequence(()), 'sequence not a sequence')
        self.assertTrue(util.is_sequence([]), 'sequence not a sequence')
        self.assertTrue(util.is_sequence([1]), 'sequence not a sequence')
        self.assertFalse(util.is_sequence(None), 'non-sequence as sequence')
        self.assertFalse(util.is_sequence(self), 'non-sequence as sequence')
        self.assertFalse(util.is_sequence({}), 'non-sequence as sequence')


class ListUtils(unittest.TestCase):

    def test_remove_duplicates(self):
        self.longMessage = True
        assert util.remove_duplicates([]) == []
        assert util.remove_duplicates([1, 1]) == [1]
        assert set(util.remove_duplicates(['a', 1, None, 'a'])) == set(['a', 1, None])
        assert sorted(util.remove_duplicates([2, 2, 1, 3, 1, 2])) == [1, 2, 3]

    def test_flatten(self):
        def assertEqualListOrTuple(actual, expected):
            assert isinstance(expected, (list, tuple)), "Test logic error"
            assert isinstance(actual, (list, tuple,))
            assert actual == expected
        assert util.flatten([]) == []
        assert util.flatten([[([])]]) == []
        assert util.flatten([[1, 2], 3, [[4]], [(5, [6, 7], 8)]]) == [1, 2, 3, 4, 5, 6, 7, 8]

    def test_nreverse(self):
        assert util.nreverse([1, 2, 3]) == [3, 2, 1]
        assert util.nreverse([]) == []


class FindUtils(unittest.TestCase):
    _ALIST = [(0, 5), (1, 6), (1, 7)]

    def test_position(self):
        self.assertEqual(util.position(42, []), None)
        self.assertEqual(util.position(42, [42]), 0)
        self.assertEqual(util.position(42, [1, 42]), 1)
        self.assertEqual(util.position(42, [1, 42, 2]), 1)
        self.assertEqual(util.position(42, [1, 2, 3]), None)

    def test_find(self):
        self.assertEqual(util.find(42, []), None)
        self.assertEqual(util.find(42, [42.0]), 42)
        self.assertEqual(util.find(42, [1, 2, 3]), None)
        self.assertEqual(util.find(42, [-41, -42, -43], key=operator.neg), -42)
        self.assertEqual(util.find(42, [41, 42, 43, 44], test=(lambda x, y: x < y)), 43)
        self.assertEqual(util.find(42, [-40, -41, -42, -43], key=operator.neg,
                                   test=(lambda x, y: x < y)), -43)

    def test_assoc(self):
        self.assertEqual(util.assoc(1, self._ALIST), (1, 6))
        self.assertEqual(util.assoc(2, self._ALIST), None)
        self.assertEqual(util.assoc(5, self._ALIST), None)

    def test_rassoc(self):
        self.assertEqual(util.rassoc(1, self._ALIST), None)
        self.assertEqual(util.rassoc(5, self._ALIST), (0, 5))

    def test_ecase(self):
        settings = ((1, 'alpha'), (2, 'beta'), (3, 'gamma'), (3, 'delta'))
        self.assertEqual(util.ecase(1, *settings), 'alpha')
        self.assertEqual(util.ecase(3, *settings), 'gamma')
        with self.assertRaises(util.ProgramError):
            util.ecase(0, *settings)


class DevNullStream(unittest.TestCase):

    def test_read(self):
        f = util.dev_null_stream('r')
        assert f.read() == ''
        assert f.read() == ''

    def test_write(self):
        f = util.dev_null_stream('w')
        f.write('a' * 100)
        f.write('b' * 100000)
        f.close()


class Mktempdir(unittest.TestCase):

    def test_it(self):
        import os
        dir1 = dir2 = dir3 = None
        try:
            dir1 = util.mktempdir('foo')
            dir2 = util.mktempdir('bar')
            dir3 = util.mktempdir('foo')
            self.assertNotEqual(dir1, dir3)
            for d in dir1, dir2, dir3:
                assert (os.stat(d)[0] & 0o7777) == 0o700
        finally:
            for d in dir1, dir2, dir3:
                if d:
                    try:
                        os.rmdir(d)
                    except Exception:  # pragma: no cover
                        pass


class Classes(unittest.TestCase):

    class _A(object):
        x = 1

    class _B(_A):
        y = 2

    class _C(_A):
        pass

    class _D(_B, _C):
        pass

    def test_public_attributes(self):
        self.longMessage = True
        assert sorted(util.public_attributes(self._B)) == ['x', 'y']

    def test_direct_public_members(self):
        assert util.direct_public_members(self._A) == ('x',)
        assert util.direct_public_members(self._B) == ('y',)
        assert util.direct_public_members(self._C) == ()
        assert util.direct_public_members(self._D) == ()
        assert sorted(util.direct_public_members(self)) == ['setUpClass',
                                                            'tearDownClass',
                                                            'test_direct_public_members',
                                                            'test_public_attributes']


class Caching(unittest.TestCase):

    def test_simple_cache(self):
        counter = [0]

        def get(key):
            counter[0] += 1
            return counter[0]
        c = caching.SimpleCache(get)
        self.assertEqual(c['a'], 1)
        self.assertEqual(c['b'], 2)
        self.assertEqual(c['a'], 1)
        c.reset()
        self.assertEqual(c['a'], 3)

    def test_limited_cache(self):
        def squarer(key):
            return key * key
        c = caching.LimitedCache(squarer, limit=2)
        self.assertEqual(c[2], 4)
        self.assertEqual(c[3], 9)
        self.assertEqual(len(c), 2)
        self.assertEqual(c[4], 16)
        self.assertLessEqual(len(c), 2)


class Resolver(unittest.TestCase):

    def test_resolver(self):
        from .resolver import Resolver
        import pytis.presentation
        r = Resolver(search=('pytis.defs', 'pytis.defs.profiles'))
        view = r.get('help.Help', 'view_spec')
        assert isinstance(view, pytis.presentation.ViewSpec)
        # Test top level specification name (from pytis.defs.profiles).
        spec2 = r.specification('FormProfiles')
        assert isinstance(spec2, pytis.presentation.Specification)
        specifications = [spec_ for name, spec_ in r.walk()]
        from pytis.defs.help import Help
        assert Help in specifications


if __name__ == '__main__':  # pragma: no cover
    unittest.main()
