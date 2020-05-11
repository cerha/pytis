# -*- coding: utf-8 -*-

# Copyright (C) 2018-2019 Tomáš Cerha <t.cerha@gmail.com>
# Copyright (C) 2009-2015 OUI Technology Ltd.
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

"""Sample web application based on Pytis CMS.

Pytis CMS is a fat client CMS form Pytis Extensions.  See 'defs/cms.py' for more information.

This is a sample Web application using Wiking and managed through Pytis CMS.

The Wiking modules based on Pytis CMS are defined in 'pytis.cms.web'.  They are imported
here so may be further customized here.  They also depend of a few Wiking modules, so these modules
are also imported here from Wiking.

To use this application, simply configure a Wiking site to search this module for available Wiking
Modules.  For example put this line into the site's configuration file:

modules = ('demo',)

Of course, you need to have the path to this module in the Python path of your web server and the
web server must be configured to run Wiking (see Wiking documentation for more information).

"""
from __future__ import unicode_literals

import wiking
import pytis.cms.web
from pytis.presentation import Field, computer, Editable, SelectionType, CbComputer
from pytis.util import translations
import pytis.data as pd
import demo.defs

from pytis.cms.web import *  # noqa: F401,F403

_ = translations('pytis-demo')


class Application(pytis.cms.web.Application):

    def site_title(self, req):
        return _("Pytis Demo")


class Products(pytis.cms.web.EmbeddablePytisModule):
    """Sample Wiking module based on misc.Products from Pytis Demo.

    This module is derived from 'EmbeddablePytisModule' and thus may be embedded into page content
    from Pytis CMS.  The module name must be first added to the codebook of available CMS modules
    manually.

    """
    class Spec(wiking.Specification, demo.defs.misc.Products):
        # Include the complete specification without any modifications.
        pass


class Countries(pytis.cms.web.EmbeddablePytisModule):

    class Spec(wiking.Specification, demo.defs.cb.Countries):
        _continents = 'Continents'


class Continents(pytis.cms.web.EmbeddablePytisModule):

    class Spec(wiking.Specification, demo.defs.cb.Continents):
        pass


class RuntimeFilterDemo(pytis.cms.web.EmbeddablePytisModule):
    """Demonstration of runtime filters and few other AJAX form features.

    This module serves as an example of dynamic form features on the web (through AJAX).  The
    specification of computers, editability and runtime filters is just the same as in the fat
    client application.  This way the web forms gain nearly all the comfort we have in the fat
    client.

    This module doesn't exist in the fat client application, so its specification is defined
    directly here just for the web.

    """
    class Spec(wiking.Specification):
        title = _("Product prices in different countries")

        def fields(self):
            return (
                Field('id'),
                Field('continent', _("Continent"), width=3, codebook='Continents',
                      selection_type=SelectionType.RADIO, not_null=True,
                      descr=_("Select continent to limit country selection below to "
                              "given continent.")),
                Field('country', _("Country"),
                      codebook='Countries', selection_type=SelectionType.CHOICE,
                      runtime_filter=computer(self._country_filter), not_null=True,
                      editable=computer(lambda r, continent: continent is not None),
                      descr=_("Select the country. Use the radio buttons above to limit the "
                              "available options to countries of given continent.")),
                Field('filter', _("Product filter"), default='*',
                      descr=_("Enter a wildcard expression to filter only matching products in "
                              "the codebook field below."), virtual=True),
                Field('marked', _("Marked products only"), type=pd.Boolean(), virtual=True,
                      descr=_("Check this checkbox if you want to filter the products in the list "
                              "below to contain only marked products.")),
                Field('product_id', _("Product"),
                      codebook='Products', display='product', not_null=True,
                      runtime_filter=computer(self._product_filter),
                      descr=_("Select the product to set the price for.")),
                Field('price', _("Price"), editable=Editable.NEVER,
                      computer=CbComputer('product_id', 'price'), virtual=True),
                Field('expense', _("Expense"), editable=Editable.NEVER,
                      computer=computer(self._expense), virtual=True,
                      enumerator=pd.FixedEnumerator(('low', 'medium', 'high')), not_null=True,
                      # Uncomment to test runtime editability for radio buttons:
                      # editable=computer(lambda r, marked: not marked),
                      display=lambda x: x.capitalize(), selection_type=SelectionType.RADIO),
            )
        layout = ('continent', 'country', 'filter', 'marked', 'product_id', 'price', 'expense')

        def _country_filter(self, row, continent):
            if continent:
                return pd.EQ('continent', pd.Value(pd.String(), continent.upper()))
            else:
                return None

        def _product_filter(self, row, marked, filter):
            condition = pd.WM('product', pd.WMValue(pd.String(), filter or '*'))
            if marked:
                value = pd.Value(pd.Boolean(), True)
                condition = pd.AND(condition, pd.EQ('marked', value))
            return condition

        def _expense(self, row, price):
            if price is None:
                return None
            elif price > 400:
                return 'high'
            elif price > 200:
                return 'medium'
            else:
                return 'low'
