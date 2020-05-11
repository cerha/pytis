# -*- coding: utf-8 -*-

# Copyright (C) 2018-2020 Tomáš Cerha <t.cerha@gmail.com>
# Copyright (C) 2007-2016 OUI Technology Ltd.
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
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

from __future__ import unicode_literals

import lcg
import datetime
from pytis.output import UMm, PAGE_WIDTH, PAGE_HEIGHT, PAGE_LANDSCAPE_MODE, PAGE_TOP_MARGIN, \
    PAGE_BOTTOM_MARGIN, PAGE_LEFT_MARGIN, PAGE_RIGHT_MARGIN, HGroup, VGroup, \
    HSpace, f_smaller, PageNumber, PrintSpecification
from pytis.util import translations

_ = translations('pytis-demo')


class ProductPage(PrintSpecification):

    def page_layout(self):
        return {
            PAGE_WIDTH: UMm(210),
            PAGE_HEIGHT: UMm(297),
            PAGE_LANDSCAPE_MODE: False,
            PAGE_TOP_MARGIN: UMm(10),
            PAGE_BOTTOM_MARGIN: UMm(10),
            PAGE_LEFT_MARGIN: UMm(10),
            PAGE_RIGHT_MARGIN: UMm(10),
        }

    def body(self):
        return VGroup(*[HGroup(label, ': ', self._parameter(param, '-'))
                        for param, label in (('product_id', _("Product ID")),
                                             ('product', _("Name")),
                                             ('price', _("Price")))])

    def page_header(self):
        return f_smaller(HGroup(
            _("Print Specification Demo"),
            HSpace(None),
            lcg.LocalizableDateTime(datetime.date.today()),
        ))

    def page_footer(self):
        return f_smaller(HGroup(
            _("Language: %s", self._parameter('language')),
            HSpace(None),
            PageNumber(),
        ))
