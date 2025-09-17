#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (C) 2018-2025 Tomáš Cerha <t.cerha@gmail.com>
# Copyright (C) 2001-2018 OUI Technology Ltd.
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
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

from __future__ import unicode_literals
from __future__ import print_function

import operator
import pytest
import re
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
        self.assertTrue(util.sameclass(1, 1), 'same integer classes not recognized')
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

    def test_deepstr(self):
        class X(object):
            def __str__(self):
                return '<instance of "%s">' % self.__class__.__name__
        assert util.deepstr(123) == '123'
        assert util.deepstr('"čuně"') == '"\\"čuně\\""'
        assert util.deepstr((2, 1)) == '(2, 1,)'
        assert util.deepstr([2, 'čuně']) == '[2, "čuně"]'
        assert util.deepstr(X()) == '<instance of "X">'
        assert util.deepstr([1, X(), None]) == '[1, <instance of "X">, None]'


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

    class _E(_D):
        attr_x = 'x'
        attr_y = 'y'

    def test_public_attributes(self):
        self.longMessage = True
        assert sorted(util.public_attributes(self._B)) == ['x', 'y']
        assert sorted(util.public_attributes(self._E)) == ['attr_x', 'attr_y', 'x', 'y']
        assert sorted(util.public_attributes(self._E, prefix='attr_')) == ['attr_x', 'attr_y']

    def test_direct_public_members(self):
        assert util.direct_public_members(self._A) == ('x',)
        assert util.direct_public_members(self._B) == ('y',)
        assert util.direct_public_members(self._C) == ()
        assert util.direct_public_members(self._D) == ()
        assert 'test_direct_public_members' in util.direct_public_members(self)


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


class TestResolver:

    # Test internals.

    def test_import_module(self):
        from .resolver import Resolver, ResolverError
        import pytis.defs
        resolver = Resolver()
        assert pytis.defs == resolver._import_module('pytis.defs')
        with pytest.raises(ResolverError):
            resolver._import_module('foo_bar_xy')
        # The resolved module exists, but imports invalid module.
        with pytest.raises(ImportError):
            resolver._import_module('pytis.util.import_error_test')

    # Test public API.
    @pytest.fixture(scope="class")
    def resolver(self):
        from .resolver import Resolver
        return Resolver(search=('pytis.defs', 'pytis.defs.profiles'))

    def test_get(self, resolver):
        import pytis.presentation
        view = resolver.get('help.Help', 'view_spec')
        assert isinstance(view, pytis.presentation.ViewSpec)

    def test_specification(self, resolver):
        import pytis.presentation
        # Test top level specification name (from pytis.defs.profiles).
        spec = resolver.specification('FormProfiles')
        assert isinstance(spec, pytis.presentation.Specification)

    def test_walk(self, resolver):
        from pytis.defs.help import Help, Field
        assert ('pytis.defs.help.Help', Help) in resolver.walk()
        # Field is a class imported in pytis.defs.help, but not a specification subclass.
        assert ('pytis.defs.help.Field', Field) not in resolver.walk()

    def test_exceptions(self, resolver):
        from .resolver import ResolverError
        with pytest.raises(ResolverError) as e:
            resolver.get('foo.Bar', 'view_spec')
        assert 'Not found within pytis.defs, pytis.defs.profiles' in str(e)
        with pytest.raises(ResolverError) as e:
            # Field is a class present in pytis.defs, but not a specification subclass.
            resolver.get('Field', 'id')
        assert ' is not a pytis.presentation.Specification subclass' in str(e)

    def test_reload(self, resolver):
        p = resolver.specification('FormProfiles')
        assert p.public is True
        p.public = False
        p2 = resolver.specification('FormProfiles')
        assert p2.public is False
        resolver.reload()
        p3 = resolver.specification('FormProfiles')
        assert p3.public is True


def test_compose_mail():
    from .util import _compose_mail, Attachment

    def msg(*args, **kwargs):
        print(_compose_mail(*args, **kwargs).as_string())
        return _compose_mail(*args, **kwargs).as_string()

    assert re.match(
        # The newline in the first header (after ;) is produced by Python2 (why?).
        r'Content-Type: multipart/alternative;\n? boundary="===+\d+=="\n'
        r'MIME-Version: 1.0\n'
        r'Subject: Plain message\n'
        r'From: <bob@gnu.org>\n'
        r'To: hugo@gnu.org\n'
        r'Date: [A-Z][a-z][a-z], \d\d [A-Z][a-z][a-z] \d\d\d\d \d\d:\d\d:\d\d [\+\-]\d\d\d\d\n\n'
        r'--===+\d+==\n'
        r'Content-Type: text/plain; charset="utf-8"\n'
        r'MIME-Version: 1.0\n'
        r'Content-Transfer-Encoding: base64\n\n'
        r'TWVzc2FnZSB0ZXh0\n\n'
        r'--====+\d+==--\n',
        msg('Plain message', 'Message text', 'hugo@gnu.org', 'bob@gnu.org'),
    )

    assert re.match(
        r'Content-Type: multipart/alternative;\n? boundary="===+\d+=="\n'
        r'MIME-Version: 1.0\n'
        r'Subject: =\?utf-8\?b\?xb1sdcWlb3XEjWvDvSBrxa/FiA==\?=\n'
        r'From: <bob@gnu.org>\n'
        r'To: hugo@gnu.org\n'
        r'Date: .+\n\n'
        r'--===+\d+==\n'
        r'Content-Type: text/plain; charset="utf-8"\n'
        r'MIME-Version: 1.0\n'
        r'Content-Transfer-Encoding: base64\n\n'
        r'w5pwxJtsIMSPw6FiZWxza8OpIMOzZHk=\n\n'
        r'--===+\d+==--\n',
        msg(u'Žluťoučký kůň', u'Úpěl ďábelské ódy', 'hugo@gnu.org', 'bob@gnu.org'),
    )

    assert re.match(
        r'Content-Type: multipart/alternative;\n? boundary="===+\d+=="\n'
        r'MIME-Version: 1.0\n'
        r'Subject: HTML message\n'
        r'From: <bob@gnu.org>\n'
        r'To: hugo@gnu.org\n'
        r'Date: .+\n\n'
        r'--===+\d+==\n'
        r'Content-Type: text/html; charset="utf-8"\n'
        r'MIME-Version: 1.0\n'
        r'Content-Transfer-Encoding: base64\n\n'
        r'PGh0bWw\+PGJvZHk\+PGgxPkhlYWRpbmc8L2gxPHA\+TG9yZW0gaXBzdW0gZG9sb3IuLi48L3A\+PC9i\n'
        r'b2R5PjxodG1sPg==\n\n'
        r'--===+\d+==--\n',
        msg('HTML message', '<html><body><h1>Heading</h1<p>Lorem ipsum dolor...</p></body><html>',
            'hugo@gnu.org', 'bob@gnu.org', html=True),
    )


if __name__ == '__main__':  # pragma: no cover
    unittest.main()
