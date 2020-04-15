#!/usr/bin/env python
# -*- coding: utf-8 -*-
# Copyright (C) 2020 Tomáš Cerha <t.cerha@gmail.com>
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

from __future__ import print_function
from __future__ import unicode_literals

import pytis
import pytis.util
import pytis.help


class HelpExporter(pytis.help.HelpExporter):
    def _head(self, context):
        g = self._generator
        return (super(HelpExporter, self)._head(context) +
                [g.meta(http_equiv='Content-Type', content='text/html; charset=utf-8')])


def main(spec_name):
    """Generate HTML help page for given specification.

    """
    node = pytis.help.help_page('help:spec/' + spec_name)

    exporter = HelpExporter(get_resource_uri=lambda r, absolute=True: r.uri() or r.filename(),
                            translations=pytis.util.translation_path())
    context = exporter.context(node, pytis.util.environment_language())

    print(exporter.export(context).encode('utf-8'))


if __name__ == '__main__':
    pytis.util.run_as_script(main)
