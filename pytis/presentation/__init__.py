# -*- coding: utf-8 -*-

# Copyright (C) 2018-2025 Tomáš Cerha <t.cerha@gmail.com>
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
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

"""Data transformations and presentation.

This module is intended to solve processing of data which does not belong to
the module `pytis.data`, is generally needed by user interfaces, but is not
specific for any particular user interface.  The typical functionality
solved by this module is computing the values of virtual fields, handling
dynamic changes of editability of input fields, their validation and
integrity checking, dynamic codebook filtering etc.

"""
from __future__ import print_function

# Imports use 'X as X' form (PEP 484) to mark names as explicitly re-exported
# without maintaining a separate __all__ list.
from .spec import (
    TextFormat as TextFormat, Color as Color, Style as Style,
    Orientation as Orientation, Text as Text, Button as Button,
    ActionContext as ActionContext,
    Action as Action, PrintAction as PrintAction, ActionGroup as ActionGroup,
    Profile as Profile, ProfileGroup as ProfileGroup, AggregatedView as AggregatedView,
    Folding as Folding, Profiles as Profiles, Filter as Filter, Condition as Condition,
    GroupSpec as GroupSpec, HGroup as HGroup, FieldSet as FieldSet, TabGroup as TabGroup,
    QueryFields as QueryFields, ViewSpec as ViewSpec, BindingSpec as BindingSpec,
    Binding as Binding, Editable as Editable, SelectionType as SelectionType,
    PostProcess as PostProcess,
    TextFilter as TextFilter, Computer as Computer, computer as computer,
    procedure as procedure, CbComputer as CbComputer, CodebookSpec as CodebookSpec,
    FormType as FormType, Link as Link, ListLayout as ListLayout,
    Enumeration as Enumeration, Field as Field, Fields as Fields,
    AttachmentStorage as AttachmentStorage,
    FileAttachmentStorage as FileAttachmentStorage,
    HttpAttachmentStorage as HttpAttachmentStorage,
    DbAttachmentStorage as DbAttachmentStorage,
    SharedParams as SharedParams, Menu as Menu, MenuItem as MenuItem,
    MenuSeparator as MenuSeparator, Command as Command,
    CommandHandler as CommandHandler,
    CommandHandlerMetaClass as CommandHandlerMetaClass, StatusField as StatusField,
    SpecificationBase as SpecificationBase, Application as Application,
    HelpProc as HelpProc, help_proc as help_proc,
    Specification as Specification, specification_path as specification_path, IN as IN,
    # Deprecated:
    ColumnLayout as ColumnLayout, VGroup as VGroup, LHGroup as LHGroup, LVGroup as LVGroup,
)


from .field import PresentedRow as PresentedRow

from .types_ import (
    PrettyType as PrettyType, PrettyTreeOrder as PrettyTreeOrder,
    PrettyFoldable as PrettyFoldable,
)
