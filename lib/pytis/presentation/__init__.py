# -*- coding: utf-8 -*-

# Copyright (C) 2018-2020, 2023 Tomáš Cerha <t.cerha@gmail.com>
# Copyright (C) 2002-2017 OUI Technology Ltd.
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

"""Data transformations and presentation.

This module is intended to solve processing of data which does not belong to
the module 'pytis.data', is generally needed by user interfaces, but is not
specific for any particular user interface.  The typical functionality solved
by this module is computing the values of virtual fields, handling dynamic
changes of editability of input fields, their validation and integrity
checking, dynamic codebook filtering etc.

"""

from .spec import (  # noqa: F401
    TextFormat, Color, Style, Orientation, Text, Button, ActionContext,
    Action, PrintAction, ActionGroup, Profile, ProfileGroup, AggregatedView,
    Folding, Profiles, Filter, Condition, GroupSpec, HGroup, FieldSet, TabGroup,
    ColumnLayout, VGroup, LHGroup, LVGroup, LayoutSpec, QueryFields,
    ViewSpec, BindingSpec, Binding, Editable, SelectionType, PostProcess,
    TextFilter, Computer, computer, procedure, CbComputer, CodebookSpec,
    FormType, Link, ListLayout, Enumeration, Field, Fields, AttachmentStorage,
    FileAttachmentStorage, HttpAttachmentStorage, DbAttachmentStorage,
    SharedParams, StatusField, SpecificationBase, Application, Specification,
    specification_path, IN,
)

from .field import PresentedRow  # noqa: F401

from .types_ import PrettyType, PrettyTreeOrder, PrettyFoldable  # noqa: F401
