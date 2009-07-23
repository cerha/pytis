# -*- coding: utf-8 -*-

# Copyright (C) 2009 Brailcom, o.p.s.
#
# COPYRIGHT NOTICE
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

import pytis.data

class PrettyType(pytis.data.Type):
    """Type providing pretty exports.

    This is basically a standard type enhanced with 'pretty_export' method
    providing visually pleasant (whatever it means) formatted values.

    Pretty type classes are intended to be combined with data types by multiple
    inheritance.

    """
    def pretty_export(self, value, row, **kwargs):
        """Return pretty 'value' as a string or unicode.

        Arguments:

          value -- 'Value' instance to export
          row -- 'PresentedRow' instance containing the whole row where the
            value is present
          kwargs -- arguments to be relayed to 'export' method

        In this class the method is the same as 'export'.

        """
        return self.export(self, value, **kwargs)

class PrettyTreeOrder(PrettyType):
    """Type providing indented pretty values based on a tree order column.

    The type inserts indentation in front of the values based on another column
    of the 'TreeOrder' type providing indirect indentation information.  This
    is useful for presenting hierarchical records such as menus.
    
    """
    def __init__(self, tree_column_id, **kwargs):
        """
        Arguments:

          tree_column_id -- id of the column defining tree ordering; string
          kwargs -- arguments to be forwarded to the superclass constructor

        """
        super(PrettyTreeOrder, self).__init__(**kwargs)
        self._tree_column_id = tree_column_id

    def _indentation(self, level, row, form):
        if level > 0:
            indentation = 3*level*' ' + '- '
        else:
            indentation = ''
        return indentation
    
    def pretty_export(self, value, row, form=None, **kwargs):
        exported = self.export(value, **kwargs)
        order = row[self._tree_column_id].export()
        if order is not None:
            if order and order[0] == '.':
                order = order[1:]
            level = len(order.split('.')) - 1
            exported = self._indentation(level, row, form) + exported
        return exported

class PrettyFoldable(PrettyTreeOrder):
    """Similar as 'PrettyTreeOrder' but with an additional folding indicator.
    """
    def _indentation(self, level, row, form):
        if form is None:
            mark = ''
        else:
            folding = form.folding_level(row)
            if folding == '':
                return ''
            if folding == 0:
                mark = u'⊞ '
            else:
                mark = u'⊟ '
        return 3*level*' ' + mark
