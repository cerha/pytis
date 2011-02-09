# -*- coding: utf-8 -*-

# Copyright (C) 2009, 2010, 2011 Brailcom, o.p.s.
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

    The other column of 'TreeOrder' type must have unique non-NULL values
    (typically it should be a primary key).
    
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

    def tree_column_id(self):
        """Return id of the column defining the tree ordering."""
        return self._tree_column_id
    
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
    FOLDED_MARK = u'⊞'
    "Folding indicator for a foldable node in a folded state."
    UNFOLDED_MARK = u'⊟'
    "Folding indicator for a foldable node in a unfolded state."
    NON_FOLDABLE_MARK = u'⊙'
    "Folding indicator for an unfoldable node."
    
    def __init__(self, tree_column_id, subcount_column_id=None, **kwargs):
        """
        Arguments:
        
          tree_column_id -- id of the column defining tree ordering; string
          subcount_column_id -- id of the column providing number of subnodes
            of the given tree ordering column; string.  This is used to display
            proper folding indicators.  When 'None', no such column is used.
          kwargs -- arguments to be forwarded to the superclass constructor
          
        """
        super(PrettyFoldable, self).__init__(tree_column_id, **kwargs)
        self._tree_column_nsub_id = subcount_column_id
        
    def _indentation(self, level, row, form):
        if form is None:
            mark = ''
        else:
            folding = form.folding_level(row)
            if folding == '':
                return ''
            if (self._tree_column_nsub_id and
                self._tree_column_nsub_id in row and
                not row[self._tree_column_nsub_id].value()):
                mark = self.NON_FOLDABLE_MARK + ' '
            elif folding == 0:
                mark = self.FOLDED_MARK + ' '
            else:
                mark = self.UNFOLDED_MARK + ' '
        return 3*level*' ' + mark
