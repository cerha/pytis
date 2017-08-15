# -*- coding: utf-8 -*-

# Copyright (C) 2001-2017 Brailcom, o.p.s.
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

"""Classes for specification of presentational propertis of pytis data views.

The class 'Specification' defines the actual properties of the view.  Other
classes of this module ('Field', 'Binding', 'GroupSpec', ...) are mostly used
for specification of particular attributes of the 'Specification'.

Form instances of particular backends (wx forms, web forms) then interpret the
properties defined in specifications.

All classes of this module are of specificational nature and can be considered
immutable.  Thus they can be shared as needed.

"""

import collections
import copy
import os
import re
import string
import types
import weakref
import BaseHTTPServer

import pytis.data
from pytis.util import argument_names, camel_case_to_lower, find, is_anystring, is_sequence, \
    public_attributes, public_attr_values, split_camel_case, xtuple, nextval, \
    log, OPERATIONAL, ProgramError, UNDEFINED
import pytis.presentation


def specification_path(specification_name):
    """Return specification path and the relative specification name.

    Given the fully qualified specification name, return pair (PATH, NAME)
    where PATH is the search path (one of 'config.search_modules' members) of
    the specification name and NAME is the specification name without the path.
    If no path is identified then PATH is empty and NAME is
    'specification_name'.

    Arguments:

      specification_name -- name of the specification; basestring

    """
    import config
    path = ''
    for prefix in config.search_modules:
        prefix = prefix + '.'
        if specification_name.startswith(prefix) and len(specification_name) > len(prefix):
            n = len(prefix)
            specification_name = specification_name[n:]
            path = specification_name[:n - 1]
    return path, specification_name


class TextFormat(object):
    """Constants for definition of text format.

    Not all formats defined below must be supported at all places where
    'TextFormat' is specified.  The documentation of each such place should
    specify the behavior for each of the supported text formats.

    """
    PLAIN = 'plain'
    """Plain text

    Indicates that no further formatting should be expected within the text and
    the text can be displayed as is.

    """
    HTML = 'html'
    """HTML text

    No particular HTML version is defined at this level.  The expected HTML
    version and the range of supported elements depends strictly upon the
    consumer of this text.  This constant only indicates, that the text is in
    HTML and should be treated as such.

    """
    LCG = WIKI = 'lcg'
    """LCG Structured Text

    Indicates that the text uses the markup described in LCG's Structured Text
    Formatting manual.  Such text should normally be processed by LCG before
    displaying.

    """


class Color(object):
    """Independent definition of generic named colors to be used within 'Style' specifications."""
    WHITE = (255, 255, 255)
    BLACK = (0, 0, 0)
    RED = (255, 0, 0)
    RED20 = (255, 200, 200)
    GREEN = (0, 255, 0)
    BLUE = (0, 0, 255)
    YELLOW = (255, 255, 160)
    GRAY = (50, 50, 50)
    GRAY90 = (25, 25, 25)
    GRAY80 = (50, 50, 50)
    GRAY70 = (75, 75, 75)
    GRAY60 = (100, 100, 100)
    GRAY50 = (125, 125, 125)
    GRAY40 = (150, 150, 150)
    GRAY30 = (175, 175, 175)
    GRAY20 = (200, 200, 200)
    GRAY10 = (225, 225, 225)
    BLANCHETALMOND = (255, 235, 205)
    LIGHTYELLOW = (255, 255, 224)
    PEACHPUFF2 = (238, 203, 173)
    SLATEGRAY2 = (185, 211, 238)
    LIGHTSALMON = (255, 160, 122)


class Style(object):
    """Text style specification.

    Style instance is returned by the 'style' attribute of 'Field' or 'row_style' attribute of
    'ViewSpec'.  Both specifiers may be functions and compute the style based on the values of the
    current row.  They may also return None to indicate the default style.  Field style has a
    higher precedence, so all properties not defined by field style default to those defined by row
    style and if they are not defined by row style, global defaults are used.

    """
    _COLOR_RE = re.compile('^\#[0-9a-fA-F]{3,3}([0-9a-fA-F]{3,3})?$')

    def __init__(self, foreground=None, background=None, bold=None, slanted=None,
                 overstrike=None, underline=None, name=None):
        """Arguments:

          foreground -- foreground color as one of 'Color' constants, a tuple of three integers
            (RGB), or a hexadecimal string representation (such as '#ff0000')
          background -- background color in the same format as the foreground color
          bold -- flag indicating bold text
          slanted -- flag indicating slanted (italics) text
          overstrike -- flag indicating that the text should be stroked through
          underline -- flag indicating that the text should be underlined
          name -- style name (string) refering to a common style definition in a stylesheet

        """
        self._foreground = foreground
        self._background = background
        self._bold = bold
        self._slanted = slanted
        self._overstrike = overstrike
        self._underline = underline
        self._name = name

    def foreground(self):
        return self._foreground

    def background(self):
        return self._background

    def bold(self):
        return self._bold

    def slanted(self):
        return self._slanted

    def overstrike(self):
        return self._overstrike

    def underline(self):
        return self._underline

    def name(self):
        return self._name

    def __str__(self):
        items = [k[1:] + '=' + repr(v) for k, v in self.__dict__.items()
                 if k.startswith('_') and v is not None]
        return "<%s %s>" % (self.__class__.__name__, ', '.join(items))

    def __radd__(self, other):
        if other is None:
            return self
        else:
            return other + self

    def __add__(self, other):
        def coalesce(x, y):
            if x is not None:
                return x
            return y
        if other is None:
            return self
        else:
            return Style(foreground=coalesce(self._foreground, other._foreground),
                         background=coalesce(self._background, other._background),
                         bold=coalesce(self._bold, other._bold),
                         slanted=coalesce(self._slanted, other._slanted),
                         overstrike=coalesce(self._overstrike, other._overstrike),
                         underline=coalesce(self._underline, other._underline),
                         name=coalesce(self._name, other._name))


class Orientation(object):
    """Definition of constants for orientation specification."""
    HORIZONTAL = 'HORIZONTAL'
    """Horizontal orientation."""
    VERTICAL = 'VERTICAL'
    """Vertical orientation."""


class Text(object):
    """Specification of a text embedded within form layout.

    Instances of this class may be included within form layout (in 'GroupSpec'
    constructor) to place arbitrary text to forms.

    """
    def __init__(self, text):
        assert isinstance(text, basestring), text
        self._text = text

    def text(self):
        return self._text


class Button(object):
    """Specification of a button embedded within form layout.

    Instances of this class may be included within form layout (in 'GroupSpec'
    constructor) to place action buttons into forms.  These buttons may invoke
    actions (defined by 'Action') or a user defined function.

    """

    def __init__(self, label=None, handler=None, enabled=None, action=None, width=None,
                 tooltip=None, active_in_popup_form=True, active_in_readonly_form=False):
        """Arguments:

          label -- button label as a string.  Can be None when 'action' is specified -- in this
            case the action title is used.
          handler -- function of one argument -- the 'PresentedRow' instance representing the
            current state of the form.  If None, 'action' must be specified.
          enabled -- function of one argument -- the 'PresentedRow' instance representing the
            current state of the form -- which returns True if the button is enabled or False
            otherwise.  This function will be called periodically on each user interface update, so
            the result may change during form editing.  Only relevant if 'handler' is used.  For
            'action' the button state depends on the referred action and its 'enabled' parameter.
          action -- name of an 'Action' specification as a string.  This allows to handle the
            button press by invoking one of 'actions' defined in the same 'ViewSpec' instead of
            passing the 'handler' function directly.  If used, 'handler' must be None, if None,
            'handler' must be specified.
          width -- button with (number of characters).  The default width is set automatically to
            fit the label, but this argument may override this default.
          tooltip -- button's tooltip text as a string.
          active_in_popup_form -- False value deactivate the button in popup (modal) forms.  This
            may be particularly useful when the button opens a new form, which is impossible in
            modal forms.
          active_in_readonly_form -- buttons are inactive in read-only forms by default, since they
            often modify form data.  True value will activate the button in even for readonly
            forms.  This may be particularly useful when the button performs some action, which
            doesn't modify the data.

        """
        if action is None:
            assert isinstance(label, basestring), label
            assert isinstance(handler, collections.Callable), handler
            assert enabled is None or isinstance(enabled, collections.Callable), enabled
        else:
            assert isinstance(action, basestring), action
            assert label is None or isinstance(label, basestring), label
            assert handler is None, handler
            assert enabled is None, enabled
        assert width is None or isinstance(width, int)
        assert tooltip is None or isinstance(tooltip, basestring), tooltip
        assert isinstance(active_in_popup_form, bool), active_in_popup_form
        assert isinstance(active_in_readonly_form, bool), active_in_readonly_form
        self._label = label
        self._handler = handler
        self._enabled = enabled
        self._action = action
        self._width = width
        self._tooltip = tooltip
        self._active_in_popup_form = active_in_popup_form
        self._active_in_readonly_form = active_in_readonly_form

    def label(self):
        return self._label

    def handler(self):
        return self._handler

    def enabled(self):
        return self._enabled

    def action(self):
        return self._action

    def width(self):
        return self._width

    def tooltip(self):
        return self._tooltip

    def active_in_popup_form(self):
        return self._active_in_popup_form

    def active_in_readonly_form(self):
        return self._active_in_readonly_form

class ActionContext(object):
    """Enumeration class defining available constants for 'Action' context specification."""

    RECORD = 'RECORD'
    """Action is performed on the current record (table row, show form record etc.).

    The action handler will receive the record as a 'PresentedRow' instance as
    its first positional argument.

    """
    CURRENT_ROW = RECORD
    """Depracated: Use RECORD instaed."""

    SELECTION = 'SELECTION'
    """Action is performed on the current selection (records selected in the UI).

    The action handler will receive the selection as an iterable object itering
    over the 'PresentedRow' instances as its first positional argument.  If no
    selection is currently active, the iterator will iter over just one item --
    the current row.

    """
    GLOBAL = 'GLOBAL'
    """Action is global for the view it belongs to.

    Global actions don't operate on particular records, they either don't
    operate on records at all or they operate on all records or they implement
    their own method of selecting the record(s) for the operation (e.g. pop-up
    a dialog).  The handler will receive no positional argument.

    """
    # TODO: Zde by ještě mohla být jedna hodnota, která by umožnila definovat
    # univerzální akce, které pracují implicitně s aktuálním řádkem, ale pokud
    # existuje výběr, tak s výběrem.


class Action(object):
    """User action specification.

    Actions typically appear in the user interface as action buttons or menu items and instances of
    this class define their properties, such as label, identifier, description, context in which
    they appear etc.  Instances of this class are used to define the 'actions' argument of
    'ViewSpec'.

    """
    def __init__(self, id, title, handler=None, context=ActionContext.RECORD,
                 secondary_context=None, enabled=True, visible=True, access_groups=None,
                 descr=None, hotkey=None, icon=None, kwargs=None, form_content=None,
                 **kwargs_):
        """Inicializuj instanci.

        Argumenty:

          id -- action identifier as a string.  It must be unique among all
            objects identifiers within a given form.
          title -- action title displayed in the user interface.
          descr -- brief textual description of the action to be displayed in
            the user intercace (for example as a tooltip).  Should be more
            descriptive than title, but should not exceed one or two sentences.
          handler -- callable object implementing the action.  This argument is
            only applicable for wx forms (fat client).  Web interface (based on
            Wiking) implements actions as Wiking module's methods of the name
            determined by the action id.  The handler will be called on action
            invocation and will receive the arguments according to the
            'context' and 'secondary_context' (see below).  This argument may
            also be specified as positional.
          context -- action context as one of 'ActionContext' constants.
            Determines the context in which the action is available and the
            first positional argument passed to the context sensitive methods
            ('handler', 'enabled' and 'visible').  See 'ActionContext'
            constants for description of what is passed for which kind of
            context.
          secondary_context -- secondary action context in dual form as one of
            'ActionContext' constants or None.  Certain actions may need also a
            contextual information from another form of a dual form.  If this
            argument is not None, the action is only available in dual forms
            and context sensitive methods ('handler', 'enabled', 'visible')
            will receive a second positional argument representing the second
            form's context (current record or selection).
          enabled -- a boolean value determining action availability in the
            user interface or a callable object (function) determining action
            availability dynamically.  The function receives arguments
            according to the 'context' (see above) and returns true iff the
            action should be available or false otherwise.  Uvavailable actions
            are still present in the user interface, but they are inactive
            (grayed out).  See 'visible' for hiding the action completely under
            certain conditions.
          visible -- a boolean value determining action visibility in the user
            interface or a callable object (function) determining the
            visibility dynamically.  The function receives arguments according
            to the 'context' (see above).  If 'False' or if 'False' is returned
            by the function, the action is competely removed from the user
            interface.  See 'enabled' for making the action inactive, but still
            visible.  Note: Visibility is currently not implemented in wx
            forms, so the argument only has effect in web applications.
          access_groups -- Depracated: DMP now implements full dynamic action
            access management.  The original docstring was: seznam
            uživatelských skupin, které mají právo akci vyvolat.  Akce se pro
            ostatní uživatele stane automaticky neaktivní.  Teprve pokud
            uživatel patří do jedné z vyjmenovaných skupin, je dostupnost akce
            zjištěna pomocí funkce 'enabled' (pouze wx formuláře).
          kwargs -- dictionary of additional keyword arguments passed to the
            action handler (and 'enabled' and 'visible' functions if defined
            dynamically) in runtime.  In Wiking web applications these
            arguments are passed as ordinary request parameters.  These
            arguments may also be passed directly as additional Action
            constructor keyword arguments for backwards compatibility, but this
            practice is deprecated.
          hotkey -- keyboard shortcut (implemented only in wx forms).
          icon -- action icon string identifier.  Implemented only in web
            forms, where the identifier refers to a CSS class name.
          form_content -- function of a three arguments 'generator', 'record'
            and 'enabled' returning list of additional form elements to prepend
            to the action button; applicable only to Wiking applications.
            'generator' is an HTML generator instance, 'record' is current
            record instance, 'enabled' is a boolean defining whether the given
            action is enabled.  If the argument is 'None', no additional
            content is added.

        """
        assert isinstance(title, basestring), title
        assert descr is None or isinstance(descr, basestring), descr
        assert handler is None or isinstance(handler, collections.Callable), handler
        assert context in public_attributes(ActionContext), context
        assert secondary_context is None or secondary_context in public_attributes(ActionContext), \
            secondary_context
        assert isinstance(enabled, collections.Callable) or isinstance(enabled, bool), enabled
        assert isinstance(visible, collections.Callable) or isinstance(visible, bool), visible
        assert access_groups is None or isinstance(access_groups, (basestring, tuple, list))
        assert hotkey is None or isinstance(hotkey, (basestring, tuple)), hotkey
        assert icon is None or isinstance(icon, basestring), icon
        assert kwargs is None or isinstance(kwargs, dict) and not kwargs_, kwargs_
        assert form_content is None or isinstance(form_content, collections.Callable), form_content
        self._id = id
        self._title = title
        self._handler = handler
        self._context = context
        self._secondary_context = secondary_context
        self._enabled = enabled
        self._visible = visible
        self._access_groups = access_groups
        self._descr = descr
        self._hotkey = hotkey
        self._icon = icon
        self._kwargs = kwargs or kwargs_
        self._form_content = form_content

    def id(self):
        return self._id

    def name(self):
        """Deprecated: Use id() instead."""
        return self.id()

    def title(self, raw=False):
        title = self._title
        if not raw:
            title = title.replace("&", "")
        return title

    def handler(self):
        return self._handler

    def context(self):
        return self._context

    def secondary_context(self):
        return self._secondary_context

    def enabled(self):
        return self._enabled

    def visible(self):
        return self._visible

    def access_groups(self):
        return self._access_groups

    def set_access_groups(self, access_groups):
        assert access_groups is None or is_sequence(access_groups)
        self._access_groups = access_groups

    def descr(self):
        return self._descr

    def hotkey(self):
        return self._hotkey

    def icon(self):
        return self._icon

    def kwargs(self):
        return self._kwargs

    def form_content(self, generator, record, enabled):
        """Return result of calling 'form_content' function given in constructor.

        Arguments:

          generator -- HTML generator instance
          record -- current 'Record' instance
          enabled -- boolean indicating whether the action is enabled

        Return value is a list of extra form elements (as produced by the
        generator).

        """
        if self._form_content is None:
            return []
        return self._form_content(generator, record, enabled)


class PrintAction(object):
    """Output (print) action specification."""
    def __init__(self, id, title, name, language=None, handler=None, context=None):
        """Arguments:

          id -- action identifier as a string.  It must be unique among all
            objects identifiers within a given form.
          title -- action title displayed in the user interface
          name -- name of the print specification, string
          language -- output language; two letter language code or None
          handler -- custom print handler function as a callable object; If
            None, the printing is by default performed by Pytis.  If a function
            is given, the function will be called on PrintAction invocation
            with one or no positional argument, according to 'context'.  By
            default, the context is 'ActionContext.RECORD', so one positional
            argument is passed (the current form row as a 'PresentedRow'
            instance).
          context -- context of records on which the action operates.
            Only relevant when 'handler' is defined.  It determines which
            arguments the handler receives.  The value is one of
            'ActionContext' constants.

        """
        assert isinstance(id, basestring), id
        assert isinstance(title, basestring), title
        assert isinstance(name, basestring), name
        assert handler is None or isinstance(handler, collections.Callable), handler
        assert context is None or context in public_attributes(ActionContext), context
        assert context is None or handler is not None, \
            'Context only makes sense when handler is used.'
        self._id = id
        self._title = title
        self._name = name
        self._language = language
        self._handler = handler
        self._context = context or ActionContext.RECORD

    def id(self):
        """Return action id given in the constructor."""
        return self._id

    def title(self):
        """Return action title given in the constructor."""
        return self._title

    def name(self):
        """Return print action name given in the constructor."""
        return self._name

    def language(self):
        """Return output language given in the constructor."""
        return self._language

    def handler(self):
        """Return custom print handler given in the constructor."""
        return self._handler

    def context(self):
        """Return print handler context given in the constructor."""
        return self._context

    def dmp_name(self):
        """Return print action name in the form useable for DMP."""
        return self._name.replace('/', '#')


class _TitledGroup(object):
    """Generic superclass for titled groups of items.

    Named groups can be used to logicaly group items into a hierarchy of
    sections, which may be represented as hierarchical menus or another kind of
    nested elements in the user interface.

    """

    _ITEM_TYPE = None
    """Defines the Python class that all group items must be instance of."""
    _ALLOW_SUBSEQUENCES = False
    """True if the group items may also be nested sequences.

    Nested sequences represent visually separated unnamed subgroups."""

    def __init__(self, title, *items):
        """Arguments:

          title -- group title as basestring
          items -- group items are passed as subsequent positional arguments of
            group constructor

        """
        assert isinstance(title, basestring), title
        if __debug__:
            for item in items:
                if self._ALLOW_SUBSEQUENCES and isinstance(item, (tuple, list)):
                    for x in item:
                        assert isinstance(x, (self.__class__, self._ITEM_TYPE)), x
                else:
                    assert isinstance(item, (self.__class__, self._ITEM_TYPE)), item
        self._title = title
        self._items = items

    def title(self):
        return self._title

    def items(self):
        return self._items

    @classmethod
    def unnest(cls, items):
        """Return the list of unnested end items within the given sequence of items.

        Unnests all nested groups or subsequences of items into one linear
        sequence of end items.

        """
        result = []
        for item in items:
            if isinstance(item, cls):
                result.extend(cls.unnest(item.items()))
            elif isinstance(item, (tuple, list)):
                result.extend(cls.unnest(item))
            else:
                assert isinstance(item, cls._ITEM_TYPE)
                result.append(item)
        return result


class ActionGroup(_TitledGroup):
    """Specification of titled group of user interface actions.

    Action groups can be used to group user interface actions into logical
    sections.  More details in the documentation of the argument 'actions' in
    'ViewSpec' constructor.

    """
    _ITEM_TYPE = Action
    _ALLOW_SUBSEQUENCES = True


class Profile(object):
    """Predefined form profile specification.

    Form profiles are a set of predefined form parameters, such as filtering
    condition, sorting, visible columns and their order, grouping, etc. (see
    constructor arguments for a full list).

    The user interface allows simple switching between available profiles and
    may also implement the option of saving user defined profiles.  Thus
    instances of this class may originate either from specification or from
    user data.

    """
    def __init__(self, id, title, filter=None, descr=None, sorting=None, columns=None,
                 grouping=None, aggregations=None, folding=None, column_widths=None, errors=()):
        """Arguments:

          id -- profile identifier as a string.  It must be unique among all
            profile identifiers within a given form.
          title -- user visible profile name as a string.
          descr -- profile description as a string.
          filter -- filtering condition as a 'pytis.data.Operator' instance.
            This condition is always applied together (in conjunction) with the
            forms default 'condition' given by it's specification (if not None).
          sorting -- sorting in the same format as accepted by the 'sort'
            argument of 'pytis.data.Data.select()'.  If None, the default
            'ViewSpec' sorting applies.
          columns -- sequence of visible form columns (string field
            identifiers) in the order in which they appear in the table.  If
            None, the 'ViewSpec' columns are displayed.
          grouping -- visual grouping of table rows in the same format as
            accepted by the 'grouping' argument of 'ViewSpec'.  If None, the
            'ViewSpec' grouping applies.
          aggregations -- aggregation functions enabled in this profile in the
            same format as accepted by the 'aggregations' argument of
            'ViewSpec'.  If None, 'ViewSpec' aggregations apply.
          folding -- folding specification ('FoldableForm.Folding' instance)
            If None, the 'ViewSpec' folding applies.
          column_widths -- dictionary of table column widths keyed by string
            column identifiers with integer values representing pixel width.
            This is not designed to be used in specifications, but rather for
            saving UI form state, so this is why the widths are in pixels...
          errors -- profile validation errors as a sequence of tuples (param,
            error), where 'param' is the string name of a profile parameter
            (one of the above arguments) and `error' is the related error
            message.  Not designed to be used in specifications.  This value is
            set automatically by profile manager when validating saved
            profiles.

        """
        assert isinstance(id, basestring)
        assert isinstance(title, basestring), title
        assert descr is None or isinstance(descr, basestring), descr
        assert filter is None or isinstance(filter, pytis.data.Operator), filter
        assert sorting is None or isinstance(sorting, tuple), sorting
        assert grouping is None or isinstance(grouping, (basestring, tuple)), grouping
        assert columns is None or isinstance(columns, (tuple, list)), columns
        assert aggregations is None or isinstance(aggregations, (tuple, list)), aggregations
        self._id = id
        self._title = title
        self._descr = descr
        self._filter = filter
        self._sorting = sorting
        self._grouping = grouping and xtuple(grouping)
        self._columns = columns and tuple(columns)
        self._aggregations = aggregations and tuple(aggregations)
        self._folding = folding
        self._column_widths = column_widths
        self._errors = errors

    def __str__(self):
        parameters = ['%s=%s' % (key[1:], value)
                      for key, value in self.__dict__.items()
                      if key not in ('_id', '_title')]
        try:
            return "<%s id='%s' %s>" % (self.__class__.__name__, self._id, ', '.join(parameters))
        except AttributeError:
            return '<%s.%s object at 0x%x>' % (self.__class__.__module__, self.__class__.__name__,
                                               id(self))

    def id(self):
        """Return the unique profile identifier."""
        return self._id

    def title(self):
        """Return the name passed to the constructor."""
        return self._title

    name = title
    """Deprecated: Use title instead."""

    def descr(self):
        """Return the descr passed to the constructor."""
        return self._descr

    def filter(self):
        """Return the condition passed to the constructor."""
        return self._filter

    def sorting(self):
        """Return the sorting specification passed to the constructor."""
        return self._sorting

    def grouping(self):
        """Return the grouping specification passed to the constructor."""
        return self._grouping

    def columns(self):
        return self._columns

    def aggregations(self):
        return self._aggregations

    def folding(self):
        return self._folding

    def column_widths(self):
        return self._column_widths

    def errors(self):
        return self._errors

    def is_user_defined_profile(self):
        return self._id.startswith(pytis.form.FormProfileManager.USER_PROFILE_PREFIX)


class ProfileGroup(_TitledGroup):
    """Specification of a titled group of form profiles.

    Profile groups can be used to group profiles in the user interface selector
    into logical sections.  More details in the documentation of the argument
    'profiles' in 'ViewSpec' constructor.

    """
    _ITEM_TYPE = Profile


class AggregatedView(object):
    """Predefined aggregated view specification.

    Aggregated view can be displayed for any pytis form.  It displays form data
    aggregated using given aggregation functions and grouped by given "group
    by" columns.

    The user interface allows simple invocation of aggregated views defined in
    specification as well as management of user defined aggregated views.  Thus
    instances of this class may originate either from specification or from
    user data.

    """

    def __init__(self, id, name, group_by_columns, aggregation_columns):
        """Arguments:

          id -- aggregation identifier as a string.  It must be unique among all
            aggregations within a given specification.
          name -- user visible name as a string.
          group_by_columns -- columns to be used in the "group by" clause of
            the aggregated view as a sequence of pairs (column_id, function),
            where column_id is a string column identifier and function is the
            name of the grouping function from 'grouping_functions'
            specification option or None if the column is used directly with no
            function applied.
          aggregation_columns -- tuple of aggregation column specifications,
            where each item is a pair of string column identifier and the
            aggregation function as one of pytis.data.Data AGG_* constants.
            May only be used if 'group_by_columns' are also defiend.

        """
        assert isinstance(id, basestring), id
        assert isinstance(name, basestring), name
        assert isinstance(aggregation_columns, (tuple, list)), aggregation_columns
        assert isinstance(group_by_columns, (tuple, list)), group_by_columns
        self._id = id
        self._name = name
        self._group_by_columns = group_by_columns
        self._aggregation_columns = aggregation_columns

    def id(self):
        return self._id

    def name(self):
        return self._name

    def group_by_columns(self):
        return self._group_by_columns

    def aggregation_columns(self):
        return self._aggregation_columns


class Profiles(list):
    """A sequence of form profiles with an optional specification of the default profile.

    Form profiles can be passed as an ordinary sequence to 'ViewSpec'
    constructor's 'profiles' argument, but when you need to specify which
    profile is selected by default in the user interface, you need to use this
    class and pass the 'default' argument to the constructor.

    """
    _ID_MATCHER = re.compile('[a-z0-9_]+')

    def __new__(cls, *args, **kwargs):
        return list.__new__(cls, args)

    def __init__(self, *profiles, **kwargs):
        """Arguments:

          profiles -- Profiles can be passed as a single argument (tuple or
            list of 'Profile' or 'ProfileGroup' instances) or as separate
            arguments (one 'Profile' or 'ProfileGroup' instance per one
            positional argument).

          default -- identifier of the profile (from 'profiles') to be selected
            by default
          label -- optional label of the profile selector in the user interface
            (currently only used in web forms).  A default label will be used
            when None.

        """
        default = kwargs.pop('default', None)
        label = kwargs.pop('label', None)
        if len(profiles) == 1 and isinstance(profiles[0], (tuple, list)):
            profiles = profiles[0]
        if __debug__:
            profile_ids = []
            for profile in ProfileGroup.unnest(profiles):
                assert isinstance(profile, Profile), profile
                assert profile.id() not in profile_ids, "Duplicate profile id '%s'" % profile.id()
                profile_ids.append(profile.id())
            assert not kwargs, "Invalid keyword argument(s): %s" % ', '.join(kwargs.keys())
            assert default is None or default in profile_ids, (default, profile_ids)
        super(Profiles, self).__init__(profiles)
        self._default = default
        self._label = label

    def default(self):
        """Return identifier of the profile to be selected by default."""
        return self._default

    def label(self):
        """Return the user interface label of profile selector or None."""
        return self._label

    def unnest(self):
        return ProfileGroup.unnest(self)


class Filter(Profile):
    """Deprecated: Use Profile instead."""
    def __init__(self, id, name, condition=None):
        super(Filter, self).__init__(id, name, filter=condition)

    def condition(self):
        """Return the condition passed to the constructor."""
        return self._filter


Condition = Filter
"""Deprecated: Use 'Profile' instead."""


class GroupSpec(object):
    """Specification of form field layout in a single record presentation.

    Pytis forms are always either column based (tables) or layout based (record
    view, record edit form).  The layout defined by this class applies to the
    second category and specifies grouping of fields of a single record in this
    kind of forms.

    The layout constitutes of a group of items, where each of the items is one
    of the following:

      * string identifier refering to one of the fields present in fields
        specification,

      * 'Button' instance to place action button inside the layout,

      * 'Text' instance to place arbitrary text inside the layout,

      * recursively embedded group as a 'GroupSpec' instance,

      * callable object (function) which returns one of the above.  The
        function must accept the current record ('PresentedRow' instance) as an
        argument.  This allows building layouts dynamically depending on the
        properties of the record to be displayed within the layout.

    The items are composed either vertically (above each other) or horizontally
    (side by side).  Sophisticated layouts may be created by combining multiple
    nested layouts.

    """
    def __init__(self, items, orientation=Orientation.HORIZONTAL, label=None,
                 gap=2, border=3, align_hgroups=True, flexible=False):
        """Arguments:

          items -- contents of the group as a sequence of layout items (see
            'GroupSpec' for details about possible types of items).

          orientation -- defines how the fields are composed together as one of
            'Orientation' class constants.  Vertical group has items above each
            other, horizontal has items side by side.
          label -- Group label as a (localizable) string displayed at the top
            of the group or None for unlabeled group.  Labeled groups are
            always framed.

          gap, border -- Depracated and unsupported by
            some form types (particularly by web forms).

          align_hgroups -- align contained horizontal groups so that their
            first field aligns with the fields of this group.  Contained
            horizontal groups within a vertical group of labeled fields are by
            default aligned with its parent group so that the label of the
            first field of the nested group is aligned with the labels of the
            fields of the parent group.  This option, when applied on the
            parent group, makes it possible to disable this alignment when it
            is not desired.  It is currently only implemented for web forms.

          flexible -- this option currently only makes a difference in web
            forms.  When False (the default), the layout is enforced using
            tables.  Such layouts are fixed and can not accommodate to
            different screen sizes.  When True, tables are avoided which makes
            the layout more flexible.  Horizontal groups will be automatically
            wrapped when there is not enough space to fit the items side by
            side.  Vertical groups will not align fields and labels, but may
            may be styled easily.  It is actually recommended to make all
            horizontal groups flexible to allow responsive layout, but the
            default is False for legacy reasons.

        """
        assert is_sequence(items), items
        assert label is None or isinstance(label, basestring)
        assert isinstance(gap, int), gap
        assert gap >= 0
        assert orientation in public_attributes(Orientation)
        assert isinstance(align_hgroups, bool), align_hgroups
        assert isinstance(flexible, bool), flexible
        self._allowed_item_types = (Button, Text, str, unicode)
        if __debug__:
            allowed_item_types = (GroupSpec, Button, Text, basestring)
            try:
                # Avoid the dependency on LCG, but allow LCG content if LCG is available.
                import lcg
            except:
                pass
            else:
                allowed_item_types += (lcg.Content,)
        for i, item in enumerate(items):
            if isinstance(item, (tuple, list)):
                if isinstance(items, tuple):
                    items = list(items)
                items[i] = GroupSpec(item, orientation=Orientation.VERTICAL)
            else:
                # No need for recursion, since the check is performed for each group on its level.
                assert (isinstance(item, allowed_item_types) or
                        isinstance(item, collections.Callable)), item
        self._items = tuple(items)
        self._label = label
        self._orientation = orientation
        self._gap = gap
        self._border = border
        self._align_hgroups = align_hgroups
        self._flexible = flexible

    def items(self):
        """Return the group contents as a tuple."""
        return tuple(self._items)

    def order(self):
        """Return the identifiers of all fields in this group and all subgroups."""
        fields = []
        for item in self._items:
            if isinstance(item, GroupSpec):
                fields.extend(item.order())
            elif isinstance(item, basestring):
                fields.append(item)
        return fields

    def label(self):
        """Vrať název skupiny."""
        return self._label

    def orientation(self):
        """Vrať orientaci skládání prvků; konstanta třídy 'Orientation'."""
        return self._orientation

    def gap(self):
        """Vrať šířku mezery vertikální mezi políčky v du."""
        return self._gap

    def border(self):
        """Vrať šířku mezery kolem celé skupiny v du."""
        return self._border

    def align_hgroups(self):
        return self._align_hgroups

    def flexible(self):
        return self._flexible


class HGroup(GroupSpec):
    """Horizontal group in 'layout' specification passed to 'ViewSpec'.

    The items contained in this group are arranged horizontally (side by side),
    while the default arrangement (when items are passed as a list or tuple) is
    vertical.

    """
    def __init__(self, *items, **kwargs):
        super(HGroup, self).__init__(items, orientation=Orientation.HORIZONTAL, **kwargs)


class FieldSet(GroupSpec):
    """Labeled group with a border in 'layout' specification passed to 'ViewSpec'.

    The items inside this group will be visually surrounded by a thin border
    around and given label will be displayed at the top of this rectangle.  The
    items are arranged vertically inside the rectangle.  Use an 'HGroup' within
    group items to force horizontal arrangement.

    """
    def __init__(self, label, items):
        super(FieldSet, self).__init__(items, label=label, orientation=Orientation.VERTICAL)


class TabGroup(GroupSpec):
    """Tabbed layout specification."""

    def __init__(self, *tabs):
        """Arguments:

          tabs -- tab specifications as (LABEL, GROUP) pairs, where LABEL is the tab label and
            GROUP is the tab contents as a 'GroupSpec' instance.  If GROUP is a sequence, it will
            be automatically turned into a vertical group.

        """
        items = [GroupSpec(xtuple(group), label=label, orientation=Orientation.VERTICAL)
                 for label, group in tabs]
        super(TabGroup, self).__init__(items, orientation=Orientation.VERTICAL)


ColumnLayout = HGroup
"""Deprecated: Use HGroup instead."""


class VGroup(GroupSpec):
    """Deprecated: Use a list or tuple instead."""
    def __init__(self, *items, **kwargs):
        super(VGroup, self).__init__(items, orientation=Orientation.VERTICAL, **kwargs)


class LHGroup(HGroup):
    """Deprecated: Use FieldSet with a HGroup."""
    def __init__(self, label, *items, **kwargs):
        kwargs['label'] = label
        super(LHGroup, self).__init__(*items, **kwargs)


class LVGroup(VGroup):
    """Deprecated: Use FieldSet instead."""
    def __init__(self, label, *items, **kwargs):
        kwargs['label'] = label
        super(LVGroup, self).__init__(*items, **kwargs)


class LayoutSpec(object):
    """Deprecated: Use 'GroupSpec' directly to specify 'ViewSpec' 'layout'."""
    def __init__(self, caption, group, order=None):
        """Inicializace a doplnění defaultních hodnot atributů.

        Argumenty:

          caption -- nadpis editačního formuláře jednoho záznamu

          group -- specifikace skupiny políček nejvýšší úrovně; instance
            'GroupSpec'. Tato skupina může obsahovat další vnořené skupiny
            (viz dokumentace třídy 'GroupSpec').

          order -- specifikace pořadí procházení mezi políčky jako sekvence
            řatězců - identifikátorů políček.  Pokud není None, je pořadí
            procházení políček určeno pořadím jejich identifikátorů v této
            sekvenci.  V takovém případě musí sekvence obsahovat identifikátory
            všech políček obsažených v 'group'.  Pokud je ponechána výchozí
            hodnota 'None', je pořadí procházení dáno pořadím políček v
            'group' při procházení stromu do hloubky.  Tento výchozí způsob
            určení pořadí v naprosté většině případú vyhovuje a je z pohledu
            uživatele nejpřirozenější, proto se použítí tohoto argumentu
            doporučuje jen v nevyhnutelných případech!  Prioritním řešením by
            vždy měla být reorganizace skupin formuláře.

        'caption' je vždy považován za jazykově závislý text a tudíž automaticky
        podléhá jazykové konverzi.

        """
        assert caption is None or isinstance(caption, basestring)
        assert isinstance(group, GroupSpec)
        assert order is None or is_sequence(order)
        self._caption = caption
        self._group = group
        if order is None:
            order = group.order()
        elif __debug__:
            for id in order:
                assert is_anystring(id)
        self._order = tuple(order)

    def caption(self):
        """Vrať nadpis pro editační formulář jednoho záznamu."""
        return self._caption

    def group(self):
        """Vrať skupinu políček nejvýšší úrovně; instance 'GroupSpec'."""
        return self._group

    def order(self):
        """Vrať tuple id všech políček editačního formuláře v pořadí procházení.

        Pokud nebylo pořadí v konstruktoru určeno, odpovídá pořadí ve skupinách.

        """
        return self._order


class QueryFields(object):
    """Query fields specification.

    The specification of query fields is expressed using this class.  The
    constructor arguments define complete presentational and functional aspects
    of the query fields form.

    """
    def __init__(self, fields, autoapply=False, autoinit=False, layout=None, load=None,
                 save=None, on_main_form_selection=None, **kwargs):
        """Arguments:

        fields -- field specifications as in ViewSpec

        autoapply -- if True, the query fields will be applied automatically
          after each change in field values.  When False (the default), the
          user needs to press a button to apply the field values explicitly.
        autoinit -- if True, the query fields will be applied with their
          default values automatically when the form is opened.  Otherwise, the
          form is initially displayed empty and query field values must be
          applied manually when the fields are filled.
        layout -- form layout as in ViewSpec, with the difference, that a tuple
          or list is converted to a horizontal group containing all fields
          (ViewSpec default layout is vertical).
        load -- callable for loading query field values as a function of one
          argument which is the query fields PresentedRow instance.
        save -- function for saving query field values as a function of one
          argument which is the query fields PresentedRow instance.
        on_main_form_selection -- when the form is used as a side form of a
          dual form, given callback function will be called on each row
          selection change in the main form.  Two arguments are passed to the
          callback, both are PresentedRow instances.  The first argument represents
          the selected main form row and the second represents the query fields row main form .
        kwargs -- other ViewSpec constructor arguments

        """
        assert fields and isinstance(fields, (tuple, list)), fields
        assert (on_main_form_selection is None or
                isinstance(on_main_form_selection, collections.Callable)), \
            on_main_form_selection
        if __debug__:
            for f in fields:
                assert isinstance(f, Field), f
        if layout is None:
            layout = [f.id() for f in fields if f.visible()]
        if isinstance(layout, (tuple, list)):
            layout = HGroup(*layout, flexible=True)
        self._autoapply = autoapply
        self._autoinit = autoinit
        self._load = load
        self._save = save
        self._on_main_form_selection = on_main_form_selection
        self._fields = tuple(fields)
        self._layout = layout
        self._kwargs = kwargs

    def fields(self):
        """Return the 'fields' passed to the constructor as a tuple."""
        return self._fields

    def autoapply(self):
        """Return value of the argument 'autoapply' passed to the constructor."""
        return self._autoapply

    def autoinit(self):
        """Return value of the argument 'autoinit' passed to the constructor."""
        return self._autoinit

    def load(self):
        """Return value of the argument 'load' passed to the constructor."""
        return self._load

    def save(self):
        """Return value of the argument 'save' passed to the constructor."""
        return self._save

    def on_main_form_selection(self):
        """Return value of the argument 'on_main_form_selection' passed to the constructor."""
        return self._on_main_form_selection

    def view_spec_kwargs(self):
        """Return constructor arguments for ViewSpec instance creation."""
        return dict(fields=self._fields, layout=self._layout, **self._kwargs)



class ViewSpec(object):
    """Kompletující specifikace prezentačních vlastností pro formuláře.

    Instance této třídy zná veškeré prezentační vlasnosti určité entity
    (tabulky z pohledu aplikace).  Třída definuje API pro přístup k těmto
    vlastnostem.  Toto API je využíváno formulářovými třídami.

    Každá instance této třídy definuje vlastnosti pro všechny způsoby
    zobrazení (editační formulář, editační seznam, apod.).

    Každý typ formuláře potom využívá ze specifikace pouze tu část, která je
    pro něj relevantní.

    """

    def __init__(self, title, fields, **kwargs):
        """Inicializuj instanci.

        Argumenty:

          title -- the title of this view as a (unicode) string.  The title is
            used in browse form headings and in other contexts, where the
            entity is referred as whole (all the records).  Thus the title
            should mostly be in plural (for example 'Invoices').

          singular -- the title of a single item (one record) of the entity as
            a (unicode) string.  If None, 'title' is used in both contexts.

          layout -- single record form layout specification as a 'GroupSpec'
            instance.  It is also possible to pass a sequence of items, which
            are automatically wrapped into a newly created 'GroupSpec' instance
            with vertical arrangement.  The items must be compatible with
            item types supported by 'GroupSpec'.  If None, the default layout
            will automatically contain all fields defined by 'fields'.
            'LayoutSpec' instance is also accepted for backwards compatibility,
            but its usage is deprecated.

          list_layout -- specification of list layout as a 'ListLayout'
            instance or None.

          columns -- specification of table form columns as a sequence of field
            identifiers from 'fields'.  If not defined, the default column list
            will include all fields which don't have 'column_width' set to zero
            or 'disable_column' to true.

          actions -- specification of available user intercafe actions as a
            sequence of 'Action' instances, nested sequences, or 'ActionGroup'
            instances.  Using 'ActionGroup' instances allows grouping of
            actions into titled groups for presentation purposes.  Titled
            groups are typically represented as nested submenus.  Nested
            sequences (tuples or lists), on the other hand, may be used to
            create unnamed groups which are visually separated from their
            neighbours.  Nested groups may be used inside other groups
            recursively.

          sorting -- default sorting in the same format as accepted by the
            'sort' argument of 'pytis.data.Data.select()'.  If None, the
            records will be sorted by the key column.

          grouping -- default visual grouping of table rows.  The value is a
            column identifier or a sequence of column identifiers.  Grouping
            allows you to visually group table rows, which have the same
            value(s) in grouping columns(s).  This usually only makes sense
            when the table is sorted by these columns.  Grouping is typically
            represented by slight changes in the background color.  Rows with
            the same values in grouping columns always have the same background
            color.  This color changes whenever one of the values changes.

          group_heading -- group heading allows additional representation of
            `grouping' (see above) and thus is only relevant when grouping is
            on.  If a column identifier is specified, the value of this column
            will apeear as a separate table row whenever a new group starts.
            An 'lcg.TranslatableText' instance can also be passed as an
            interpolation template.  In this case the group heading will be
            produced by interpolation of the template (with python string
            formatting syntax such as '%(field_id)s') by formatted field values
            of the first row of the group.  Most often, you will want to show
            group headings when the grouping columns are actually not shown in
            the table.  Group heading is currently only supported by web forms.
            In this case the title will be produced by interpolation of
            formatted row values within given string (with python string
            formatting syntax).

          check -- function to verify the integrity of the whole record.  May
            be specified as a single computer function (automatically wrapped
            by 'computer()') or a sequence of such functions/computers.  In the
            later case all functions will be called in the order in which they
            appear in the sequence.  As opposed to "validation" which only
            verifies whether a single value matches its data type and
            constraints (values of other fields are not available during
            validation), the check function verifies mutual compatibility of
            all form values.  The check function is called only after
            successful validation of all fields.  The check function returns
            None in case of success or an error message as a string in case of
            failure.  It is also possible to return a pair (field_id, message)
            where field_id is the id of the field which causes the problem.
            Given field will receive focus in this case, but it is recommended
            to use separate check functions in 'Field' specification for this
            purpose.

          cleanup -- a function for final actions after inserting/updating a
            record.  The function must accept two arguments -- the first one is
            the row after performing the database operation (insert/update) and
            the second is the edited/inserted row before the database operation
            (row values may be changed by the operation -- default values may
            be supplied and/or triggers/rules may modify the data).  Both
            arguments are `PresentedRow' instances.  Note, that you can also
            access the values before any user changes throught the
            'original_row()' method on the second argument.  The cleanup
            function is run after comitting the edit/insert form (using the
            ``Ok'' button) or after committing inline editation/insert
            regardless whether the record has been changed or not.  Note, that
            unlike 'check', 'cleanup' is called after the database operation,
            but you can still abort the operation by rollback of the
            transaction (if the underlying database engine supports it).

          on_new_record -- user defined record insertion function.  The default
            response to the users request to insert a new record is opening a
            form and inserting a new row into the underlying database object
            after its submission.  The user defined function may completely (or
            partially, see return value) override this behavior.  The function
            must accept a keyword argument 'prefill' which contains a
            dictionary with field identifiers as keys and 'Value' instances as
            values.  Optionally, the function may also accept a keyword
            argument 'transaction' containing the transaction for database
            operations.  When the insertion is successful, the function should
            return the inserted row as a 'PresentedRow' instance.  The user
            interface will refresh the current form and display this row as the
            current row if possible.  If the insertion didn't succeed (either
            for error or user's exit), None should be returned.  The third
            option is to return a dictionary.  In this case the dictionary is
            used as 'prefill' and default handling is invoked with this prefill
            replacing the original one.

          on_copy_record -- user defined record copying function.  If defined,
            it will be used instead of 'on_new_record' when the new record is
            created by copying an existing record.  This function (unlike
            'on_new_record') will receive one additional keyword argument 'row'
            which contains the copied record as a 'PresentedRow' instance.  The
            other arguments ('prefill' and 'transaction') have the same meaning
            as in 'on_new_record'.  If not defined and 'on_new_record' is
            defined, 'on_new_record' is used also for copying.  Handling the
            return value is the same as for 'on_new_record'.

          on_edit_record -- user defined record edit function.  The default
            response to the users request to edit an existing record is opening
            a form and updating the underlying database object after its
            submission.  The user defined function may completely (or
            partially) override this behavior.  The function must accept a
            keyword argument 'row' which contains the edited row as a
            'PresentedRow' instance.  The function may alco define a keyword
            argument 'transaction'.  In this case the current database
            transaction is passed through this argument.  Otherwise the
            transaction may be also accessed through 'row.transaction()'.  The
            function will receive an additional keyword argument 'prefill' in
            case that row editation is invoked from within a ListField
            (codebook field with selection_type='SelectionType.LIST') which
            defines 'codebook_update_prefill' (see 'Field' constructor
            arguments).  The function's return value is ignored.

          on_delete_record -- user defined record deletion function.  If
            defined, it must be a function of one argument (the current record
            as a PresentedRow instance) which will be called on users request
            to delete a record.  Further processing of the deletion request
            will depend on the result value of the function as follows: If the
            function returns True, the deletion will continue by the default
            action (user is asked for confirmation and record is deleted) as it
            the function was not defined.  If None or False is returned the
            deletion is aborted.  If a string or unicode is returned, the
            message is printed as an error message and the deletion is aborted.
            If 1 is returned, the deletion is considered being already
            successfuly applied so only after-deletion actions (such as
            refresh) are performed.  Finally, if a 'pytis.data.Operator'
            instance is returned, all records matching given condition are
            deleted without further prompting.

          redirect -- redirection for single record view/editation specified as
            a callable object (function) of one argument - the 'PresentedRow'
            instance.  The function should return the name of the specification
            to use for given record.  If the function is not defined or it
            returns None, no redirection takes place.  Redirection may be
            useful when a form lists records of different types (displaying
            only the fields which are common for all the subtypes) and you want
            to be able to open each particular record in a form which matches
            its subtype.  Note, that the same effect would be possible by
            defining the 'on_edit_record' function.

          focus_field -- identifier of the field, which should automatically
            gain focus when a form is open.  You may also pass a function of
            one argument ('PresentedRow' instance), which returns the field
            identifier.  When None, or None is returned by the function, the
            first form field is focused by default.

          description -- brief description of the view.  A short text (one or
            two sentences) without formatting.  Use the 'help' argument below
            to supply a detailed description.  But even if 'help' is present,
            this short description should still be defined.

          help -- detailed description of the view as a formatted text in the
            LCG Structured Text format.  This text is used for generating the
            on-line help and it is also possible to supply it in a separate
            file.  See the Help tutorial for more information.

          row_style -- a 'Style' instance determining the base style for all
            fields or a function of one argument (the 'PresentedRow' instance)
            returning the 'Style' for one row (based on its values).

          profiles -- predefined form profiles as 'Profiles' instance or an
            ordinary sequence of 'Profile' or 'ProfileGroup' instances which
            will be automatically turned into a 'Profiles' instance.  If not
            empty, the user interface will allow the user to switch between
            given profiles easily.  You will mostly need to use a 'Profiles'
            instance if you want to specify which of the profiles is selected
            in the user interface by default.  Using 'ProfileGroup' instances
            allows grouping of profiles into titled groups for presentation
            purposes.

          aggregations -- a sequence aggregation functions which should be
            turned on automatically for this view (in forms which support
            that).  The items are 'AGG_*' constants of 'pytis.data.Data'.

          grouping_functions -- specification of available functions aplicable
            to group by columns in an aggregated view as a sequence of
            (function, label, input_type, return_type), where function is
            identifier of an SQL function, label is the string title of given
            function, input_type is pytis data type class of function argument
            (the function must have just one argument) and return_type is the
            pytis data type instance of the function result.  When a user
            attempts to open an aggregated view of the form, she can select the
            columns used in the group by caluse.

          aggregated_views -- specification of predefined agregated views as a
            sequence of 'AggregatedView' instances.

          bindings -- a sequence of binding specifications as 'Binding'
            instances.  Defines the related side forms displayed typically as
            notebook tabs under the main form (or on the right side when
            'orientation' is vertical).

          orientation -- orientation of the splitter between the main form and
            side forms (defined by 'bindings').  One of the 'Orientation' class
            constants.  The defualt orientation is horizontal, so the side
            forms are displayed under the main form.  When set to
            'Orientation.VERTICAL', the side forms will be displayed on the
            right side.

          folding -- 'FoldableForm.Folding' instance defining initial
            folding.  'None' means use the standard folding.

          arguments -- sequence of 'DBBinding' instances defining table
            arguments, when the table is actually a row returning function.
            Otherwise it must be 'None'.

          query_fields -- 'QueryFields' instance defining fields of a "query
            panel" or None when query fields are not used.  If defined, an
            additional panel containing these fields is attached to the list
            form.  The user can interactively enter values into these fields
            and refresh the form.  These values are automatically passed to the
            'argument_provider' and 'condition_provider' functions so the form
            data may reflect these interactively entered values from the user
            interface.  If a sequence of field specifications is passed, it is
            automatically converted to a 'QueryFields' instance with given
            fields.

          argument_provider -- function returning a dictionary of database
            function argument names (strings) as keys and 'pytis.data.Value'
            instances as corresponding database function argument values.  If
            it is 'None', no arguments are provided.  If it returns 'None', the
            select should be cancelled.  The function is called with various
            keyword arguments, same as for 'condition_provider'.  These
            arguments are described below in section Provider Arguments.
            Defining 'argument_provider' makes sense only for database table
            functions.  It should be 'None' for standard tables and views.

          condition_provider -- function returning a form filtering condition
            as a 'pytis.data.Operator' instance.  If used (not None), the
            returned condition is applied in addition to any other conditions,
            such as the static condition defined by 'condition' or conditions
            implied by form profiles, filtering, etc.  The function is called
            with various keyword arguments, same as for 'argument_provider'.
            These arguments are described below in section Provider Arguments.

          referer -- id of the referer column as a string (one of the id's
            defined by 'fields') or None.  If None, the id of the data key
            column is used.  The exported value of the referer column is used
            to refer to the given record in URI's.  Thus it should be a unique
            column and its exported values must contain only characters valid
            in the context of URI.  For example, if the referer column value is
            123 and given pytis view (module) is mapped to the base URI
            '/some/path' by the web application, the record's URI will look
            like '/some/path/123'.  Obviously, this attribute is currently only
            used in web applications.

          spec_name -- name of the original form specification if any, string.

          public -- boolean flag indicating a public specification.

        The arguments 'layout' and 'columns' may be omitted.  Default layout
        and column list will be generated automatically based on the order of
        the field specifications in 'fields'.

        Provider Arguments

        Functions 'condition_provider' and 'argument_provider' are called with
        various keyword arguments.  Their list is below.  Only those arguments,
        which make sense in given case are actually passed.  The function
        should generally consume only the arguments it needs, but still accept
        all other keyword arguments (**kwargs) for the possibility of extending
        the available arguments in future pytis versions.

          query_fields -- 'PresentedRow' instance with the current query field
            values from the user interface.  Only passed, when 'query_fields'
            are defined in form specification.
          main_form_row -- 'PresentedRow' instance with the current main form
            row values.  Only passed, when the form is displayed as a side form
            and currently only supported by wx forms (not web forms).
          req -- current request object.  Only passed in web applications (not
            in wx forms).

        """
        self._kwargs = dict(kwargs, title=title, fields=fields)
        self._init(**self._kwargs)

    def _init(self, title, fields, singular=None, layout=None, list_layout=None, columns=None,
              actions=(), sorting=None, grouping=None, group_heading=None, check=(), cleanup=None,
              on_new_record=None, on_copy_record=None, on_edit_record=None, on_delete_record=None,
              redirect=None, focus_field=None, description=None, help=None, row_style=None,
              profiles=(), filters=(), default_filter=None,
              aggregations=(), grouping_functions=(), aggregated_views=(), bindings=(),
              orientation=Orientation.HORIZONTAL, folding=None, initial_folding=None,
              arguments=None, argument_provider=None, condition_provider=None,
              query_fields=None, referer=None, spec_name='', public=None):
        def err(msg, *args):
            """Return assertion error message."""
            return spec_name + ": " + msg % args
        assert isinstance(title, basestring)
        if singular is None:
            if isinstance(layout, LayoutSpec):
                singular = layout.caption()
            else:
                singular = title
        else:
            assert isinstance(singular, basestring)
        assert isinstance(fields, (tuple, list,)), fields
        self._field_dict = dict([(f.id(), f) for f in fields])
        self._fields = tuple(fields)
        self._spec_name = spec_name
        for action in ActionGroup.unnest(actions):
            rights = Specification.data_access_rights('action/%s/%s' %
                                                      (action.id(), self._spec_name,))
            if rights is None:
                groups = None
            else:
                groups = rights.permitted_groups(pytis.data.Permission.CALL, None)
                if None in groups:
                    groups = None
            action.set_access_groups(groups)
        # Initialize the layout
        if layout is None:
            layout = LayoutSpec(singular, GroupSpec([f.id() for f in self._fields],
                                                    orientation=Orientation.VERTICAL))
        elif isinstance(layout, GroupSpec):
            layout = LayoutSpec(singular, layout)
        elif isinstance(layout, (list, tuple)):
            layout = LayoutSpec(singular, GroupSpec(layout, orientation=Orientation.VERTICAL))
        if __debug__:
            assert isinstance(actions, (tuple, list)), actions
            assert isinstance(layout, LayoutSpec), layout
            action_ids = [action.id() for action in ActionGroup.unnest(actions)]

            def recourse_group(group):
                for item in group.items():
                    if isinstance(item, GroupSpec):
                        recourse_group(item)
                    elif isinstance(item, Button):
                        assert item.action() is None or item.action() in action_ids, \
                            err("Unknown button action in layout: %s", item.action())
                    elif isinstance(item, Text):
                        pass
                    elif isinstance(item, collections.Callable):
                        pass
                    else:
                        assert item in self._field_dict, err("Unknown field in layout: %r" % item,)
                        if self._field_dict[item].width() == 0:
                            log(OPERATIONAL,
                                "Zero width field in layout:", item)
            recourse_group(layout.group())
            for f in fields:
                assert isinstance(f, Field)
                assert not isinstance(f.computer(), CbComputer) \
                    or f.computer().field() in self._field_dict.keys()
                for (s, c) in (('computer', f.computer()),
                               ('editable', f.editable())):
                    if isinstance(c, Computer):
                        for dep in c.depends():
                            assert dep in self._field_dict, \
                                err("Unknown field '%s' in dependencies for '%s' "
                                    "specification of '%s'.", dep, s, f.id())
            if referer is not None:
                assert referer in [f.id() for f in fields], referer
        # Initialize `columns' specification parameter
        if columns is None:
            columns = tuple([f.id() for f in self._fields
                             if f.column_width() and not f.disable_column()])
        else:
            if __debug__:
                assert is_sequence(columns)
                for c in columns:
                    assert isinstance(c, basestring) and c in self._field_dict, \
                        err("Unknown column id in columns: %r", c)
                    f = self._field_dict[c]
                    assert not f.disable_column(), \
                        err("Disabled column in columns: %s", c)
        # Initialize other specification parameters
        if sorting is not None:
            assert is_sequence(sorting)
            if __debug__:
                for id, dir in sorting:
                    assert self.field(id) is not None, \
                        "Invalid sorting column '%s' in %s." % (id, spec_name)
                    assert dir in (pytis.data.ASCENDENT,
                                   pytis.data.DESCENDANT), \
                        "Invalid sorting direction '%s' in %s." % (dir, spec_name)
        if grouping is None:
            grouping = ()
            assert group_heading is None
        else:
            grouping = xtuple(grouping)
            assert group_heading is None or isinstance(group_heading, basestring), group_heading
            if __debug__:
                for id in grouping:
                    assert self.field(id) is not None, id
        if filters:
            # `filters' are for backwards compatibility.
            # Filters are compatible with profiles (they only define the
            # 'filter' property of the profile) so we can use them as
            # profiles directly.
            assert not profiles, "When using 'profiles', 'filters' can not be used."
            profiles = Profiles(*filters, **{'default': default_filter})
        else:
            assert default_filter is None, default_filter
        if not isinstance(profiles, Profiles):
            assert isinstance(profiles, (tuple, list)), profiles
            profiles = Profiles(*profiles)
        assert isinstance(aggregations, (tuple, list))
        if __debug__:
            for agg in aggregations:
                assert agg in [getattr(pytis.data.Data, attr)
                               for attr in public_attributes(pytis.data.Data)
                               if attr.startswith('AGG_')]
        assert isinstance(grouping_functions, (tuple, list))
        assert isinstance(aggregated_views, (tuple, list))
        if __debug__:
            for av in aggregated_views:
                assert isinstance(av, AggregatedView), av
        assert isinstance(bindings, (tuple, list))
        if __debug__:
            binding_identifiers = []
            for b in bindings:
                assert isinstance(b, Binding), b
                if b.id() is not None:
                    assert b.id() not in binding_identifiers, \
                        err("Duplicate binding id: %s", b.id())
                    binding_identifiers.append(b.id())
            for agg in aggregations:
                assert agg in [getattr(pytis.data.Data, attr)
                               for attr in public_attributes(pytis.data.Data)
                               if attr.startswith('AGG_')]
        assert orientation in public_attributes(Orientation)
        assert cleanup is None or isinstance(cleanup, collections.Callable)
        assert on_new_record is None or isinstance(on_new_record, collections.Callable)
        assert on_copy_record is None or isinstance(on_copy_record, collections.Callable)
        assert on_edit_record is None or isinstance(on_edit_record, collections.Callable)
        assert on_delete_record is None or isinstance(on_delete_record, collections.Callable)
        assert redirect is None or isinstance(redirect, collections.Callable)
        assert (focus_field is None or
                isinstance(focus_field, collections.Callable) or
                isinstance(focus_field, basestring)), focus_field
        assert (row_style is None or
                isinstance(row_style, Style) or
                isinstance(row_style, collections.Callable)), row_style
        assert description is None or isinstance(description, basestring)
        assert help is None or isinstance(help, basestring)
        assert argument_provider is None or isinstance(argument_provider, collections.Callable)
        assert condition_provider is None or isinstance(condition_provider, collections.Callable)
        if query_fields and isinstance(query_fields, (tuple, list)):
            query_fields = QueryFields(query_fields)
        else:
            assert query_fields is None or isinstance(query_fields, QueryFields), query_fields
        self._title = title
        self._singular = singular
        self._columns = columns
        self._layout = layout
        self._list_layout = list_layout
        self._actions = actions
        self._sorting = sorting
        self._grouping = grouping
        self._group_heading = group_heading
        self._check = tuple(map(computer, xtuple(check)))
        self._cleanup = cleanup
        self._on_new_record = on_new_record
        self._on_copy_record = on_copy_record
        self._on_edit_record = on_edit_record
        self._on_delete_record = on_delete_record
        self._redirect = redirect
        self._focus_field = focus_field
        self._description = description
        self._help = help
        self._row_style = row_style
        self._profiles = profiles
        self._aggregations = tuple(aggregations)
        self._grouping_functions = tuple(grouping_functions)
        self._aggregated_views = tuple(aggregated_views)
        self._bindings = tuple(bindings)
        self._orientation = orientation
        self._folding = folding or initial_folding  # initial_folding is deprecated!
        self._arguments = arguments
        self._argument_provider = argument_provider
        self._condition_provider = condition_provider
        self._query_fields = query_fields
        self._referer = referer

    def clone(self, other):
        """Clone this instance by another 'ViewSpec' instance and return the cloned instance.

        The cloned instance will inherit all attributes of this instance and
        the other instance passed as argument, where the attributes of the
        later instance take precedence.

        """
        assert isinstance(other, ViewSpec), other
        kwargs = dict(self._kwargs, **other._kwargs)
        return ViewSpec(**kwargs)

    def title(self):
        """Vrať název náhledu jako řetězec."""
        return self._title

    def singular(self):
        """Vrať název pro jednu položku náhledu jako řetězec."""
        return self._singular

    def fields(self):
        """Vrať tuple specifikací všech políček v layoutu.

        (A navíc to vrací i políčka, která v layoutu nejsou.)

        """
        return self._fields

    def field(self, id):
        """Vrať specifikaci políčka daného 'id' jako instanci 'Field'.

        Pokud takové políčko neexistuje, vrať 'None'.

        """
        return self._field_dict.get(id)

    def layout(self):
        """Vrať specifikaci rozvržení editačního formuláře."""
        return self._layout

    def list_layout(self):
        return self._list_layout

    def columns(self):
        """Vrať tuple identifikátorů sloupců pro tabulkový formulář."""
        return self._columns

    def actions(self, unnest=False):
        """Vrať specifikaci akcí."""
        if unnest:
            return ActionGroup.unnest(self._actions)
        else:
            return self._actions

    def sorting(self):
        """Vrať specifikaci výchozího řazení."""
        return self._sorting

    def grouping(self):
        """Vrať tuple id sloupců výchozího vizuálního seskupování."""
        return self._grouping

    def group_heading(self):
        """Vrať id sloupce záhlaví skupiny."""
        return self._group_heading

    def cleanup(self):
        """Vrať funkci provádějící akce při uzavření formuláře."""
        return self._cleanup

    def check(self):
        """Vrať tuple funkcí provádějících kontrolu integrity záznamu."""
        return self._check

    def on_new_record(self):
        """Vrať funkci provádějící vložení nového záznamu, nebo None."""
        return self._on_new_record

    def on_copy_record(self):
        """Vrať funkci provádějící vložení zkopírovaného záznamu, nebo None."""
        return self._on_copy_record

    def on_edit_record(self):
        """Vrať funkci provádějící editaci záznamu, nebo None."""
        return self._on_edit_record

    def on_delete_record(self):
        """Vrať funkci provádějící mazání záznamu, nebo None."""
        return self._on_delete_record

    def redirect(self):
        """Vrať funkci zajišťující přesměrování na jiný název specifikace."""
        return self._redirect

    def focus_field(self):
        """Vrať řetězec nebo funkci, určující políčko formuláře s fokusem."""
        return self._focus_field

    def description(self):
        """Vrať stručný popis náhledu."""
        return self._description

    def help(self):
        """Vrať podrobnou nápovědu."""
        return self._help

    def row_style(self):
        """Vrať výchozí styl řádku, nebo funkci, která jej vypočte."""
        return self._row_style

    def profiles(self):
        return self._profiles

    def aggregations(self):
        """Return default aggregation functions as a tuple."""
        return self._aggregations

    def grouping_functions(self):
        """Return specification of available grouping functions as a tuple."""
        return self._grouping_functions

    def aggregated_views(self):
        """Return default aggregation functions as a tuple."""
        return self._aggregated_views

    def bindings(self):
        """Return bindings as a tuple."""
        return self._bindings

    def orientation(self):
        return self._orientation

    def folding(self):
        """Return initial folding as a 'FoldableForm.Folding' instance or 'None'."""
        return self._folding

    def arguments(self):
        """Return 'None' or a sequence of table function arguments."""
        return self._arguments

    def argument_provider(self):
        """Return 'None' or a function generating database table function arguments."""
        return self._argument_provider

    def condition_provider(self):
        """Return 'None' or a function generating database table function arguments."""
        return self._condition_provider

    def query_fields(self):
        """Return a 'QueryFields' instance or None if no query fields are defined."""
        return self._query_fields

    def referer(self):
        """Return the id of the referer column as a string or 'None'."""
        return self._referer

    def spec_name(self):
        """Return name of the corresponding specification as a basestring.

        An empty string may be returned if the name is unknown.

        """
        return self._spec_name


class BindingSpec(object):
    """Specifikace vazby dvou náhledů při propojení do duálního formuláře.

    DEPRECATED: Use 'Binding' instead.

    Definuje vlastnosti napojení dvou formulářů při jejich spojení do duálního
    formuláře.  Definována je jak datová vazba, tak některé prezentační
    vlastnosti spojení.

    Použití je následující:

    Funkce 'binding_spec' ve specifikaci libovolného náhledu vrací slovník
    všech možných spojení tohoto náhledu s dalšími jinými náhledy.  Slovník je
    klíčován názvy specifikací a hodnotou je právě instance 'BindingSpec'.

    Když je tedy například vytvářen duální formulář 'A::B', bude ve specifikaci
    náhledu 'A' (v roli hlavního formuláře) získána z 'binding_spec' položka
    pro náhled 'B' (v roli vedlejšího formuláře).

    """

    def __init__(self, title=None, binding_column=None, side_binding_column=None,
                 hide_binding_column=True, append_condition=None, condition=None, description=None,
                 help=None, sash_ratio=0.5, orientation=Orientation.HORIZONTAL):
        """Inicialize the specification.

        Arguments:

          title -- The title of the dual form as a string.  If omitted, the title will be
            provided automatically as a concatenation of the titles of both involved forms.

          binding_column -- the identifier of the binding column (string).  The binding column
            determines filtering of the side form depending on the current main form row.  If
            specified, side form will be filtered to contain only the rows, which have the same
            values in the binding column as the current main form row.  Use the argument
            'side_binding_column' if the identifier of the binding column is different in the side
            form.  Using the binding column is optional and if omitted, the filtering depends
            solely on the 'condition' argument.

          side_binding_column -- identifier of the binding column in the side form.  The default
            value None means to use the same identifier as given by 'binding_column'.

          hide_binding_column -- the binding column is hidden by default in the side form.  Pass a
            false value to this argument to unhide it.

          condition -- a function of one argument (the PresentedRow instance) returning the current
            side form condition (a 'pytis.data.Operator' instance) for given main form row.  If
            used together with the binding column, the condition will be used in conjunction with
            the binding column condition.  If 'binding_column' is None, this condition will be used
            solely.

          description -- a brief description of this dual connection of the forms.

          help -- detailed description of this dual connection as a formatted text in the LCG
            Structured Text format.  This text is used for generating the on-line help and it is
            also possible to supply it in a separate file.  See the Help tutorial for more
            information.

          sash_ratio -- the relative proportion of the size of the two forms.  The value is a
            decimal number in the range from zero to one.  The default value of 0.5 denotes equal
            space for both forms.  Lower value means less space for the main form.  The ratio only
            has effect when displaying two table forms separated horizontally.  Layout forms are
            sized according to their width/height.

          orientation -- the default orientation of the dual form separator as one of 'Orientation'
            constants.  In horizontal orientation, the main form is above the side form, in
            vertical it is on the left side.

        """
        assert title is None or isinstance(title, basestring), title
        assert binding_column is None or isinstance(binding_column, basestring), binding_column
        assert side_binding_column is None or isinstance(side_binding_column, basestring)
        assert isinstance(hide_binding_column, bool), hide_binding_column
        if append_condition is not None:
            # Backwards compatibility
            assert condition is None, "Can't use 'append_condition' (deprecated) with 'condition'."
            condition = append_condition
        assert condition is None or isinstance(condition, collections.Callable), condition
        assert condition is not None or binding_column is not None, \
            "You must specify at least one of 'condition', 'binding_column'."
        assert description is None or isinstance(description, basestring), description
        assert help is None or isinstance(help, basestring), help
        assert orientation in public_attributes(Orientation), orientation
        assert isinstance(sash_ratio, float) and 0 < sash_ratio < 1, sash_ratio
        self._title = title
        self._binding_column = binding_column
        if side_binding_column is None:
            side_binding_column = binding_column
        self._side_binding_column = side_binding_column
        self._hide_binding_column = hide_binding_column
        self._description = description
        self._help = help
        self._condition = condition
        self._sash_ratio = sash_ratio
        self._orientation = orientation

    def title(self):
        return self._title

    def description(self):
        return self._description

    def help(self):
        return self._help

    def binding_column(self):
        return self._binding_column

    def side_binding_column(self):
        return self._side_binding_column

    def hide_binding_column(self):
        return self._hide_binding_column

    def condition(self):
        return self._condition

    def sash_ratio(self):
        return self._sash_ratio

    def orientation(self):
        return self._orientation

    def arguments(self):
        return None


class Binding(object):
    """Specification of a binding to other view.

    Defines the relation of two specifications for their use within a
    dual/multi form.  The presentation is usually implemented as a pair of
    forms where one is master (main form) and the other is slave (side form).
    Multi form has one main form and several side forms, but one binding always
    defines the relation of one main form and one side form.  This relation is
    defiend within the 'bindings' parameter of 'ViewSpec' specification of the
    main form and defines its connection to the side form.

    """
    def __init__(self, id, title, name=None, binding_column=None, condition=None,
                 descr=None, single=False, enabled=True, arguments=None,
                 prefill=None, search=None, uri=None, content=None):
        """Arguments:

          id -- identifier of the binding as a string.  It must be unique among
            all objects identifiers within a given form.
          title -- title used for the list of related records
          name -- name of the related specification.
          binding_column -- the string identifier of the binding column.  The
            meaning depends on the type of the binding.  The binding can be 1:1
            or 1:N and is determined by the 'single' argument (below).  Binding
            column can be combined with a 'condition' to further restrict the
            relation or can be completely omitted and only a generic condition
            may be used to define the relation.  In this case, however, the
            application has no information about the type of the binding and
            may not be able to offer some features which require this
            knowledge.  This argument may be used as positional.
          condition -- a function of one argument (the 'PresentedRow' instance)
            returning the current binding condition (a 'pytis.data.Operator'
            instance) to filter the data of the dependent form for given main
            form row.  If used together with the binding column, the condition
            will be used in conjunction with the binding column condition.  If
            'binding_column' is None, this condition will be used solely.
          arguments -- function of a single argument ('PresentedRow'
            instance) returning a dictionary of table arguments.  This function
            may be provided only when the side form table is actually a row
            returning function.  Otherwise 'arguments' must be 'None'.
          descr -- binding description text (to be used in on-line help etc).
          single -- boolean flag indicating whether this binding corresponds to
            a 1:1 relation (True value) or 1:N relation (False value).  Simply
            put, when single is False (the default), the side form is a list of
            related records (browse form), when True, the side form is a view
            of a single related record (show form).  Consequently, the value of
            this flag determines the meaning of the 'binding_column' argument.
            For the 1:1 relation, there is exactly one record in the related
            view corresponding to one record of the main form.  The
            'binding_column' in this case is the identifier of a column in main
            form specification which must have a codebook specification
            pointing to the related view.  For the 1:N relation the binding
            column must exist in the related view and must have a codebook
            (foreign key) specification pointing to the main form (the view for
            which the binding is used).
          enabled -- boolean value or a function of one argument
            ('PresentedRow' instance).  If the value is True or if the function
            returns True for given main form row, the side form defined by this
            binding is displayed, otherwise it is inactive (currently only
            implemented for web forms, where the side form is actually
            completely left out).
          prefill -- function of one argument (the main form record) returning
            a dictionary of values to prefill in the side form's new record.
            The dictionary keys are field identifiers and values are internal
            (python) values.  If 'prefill' is None and 'binding_column' is
            specified, the default prefill is automatically generated using the
            binding column value.
          search -- function of one argument ('PresentedRow' instance)
            returning search parameters to perform side form search on main
            form navigation.  The function will receive the current main form
            record and should return a side form search condition.  The first
            matching side form row will be automatically selected on each main
            form record change (after side form filtering by
            binding_column/condition).  The returned condition may be a
            'pytis.data.Operator' instance, 'pytis.data.Value' instance
            representing the side form key column value or a dictionary of
            column values (keys are column identifiers and values are
            pytis.data.Value instances).
          uri -- a function of one argument (PresentedRow instance) returning
            side form URI as a string.  If not None, the binding referes to a
            "web form" -- an embedded browser window showing given URI.  In
            this case the arguments 'name', 'binding_column', 'condition',
            'arguments' and 'prefill' make no sense and must be None.
          content -- function of one argument (PresentedRow instance) returning
            either an HTML string or 'lcg.ContentNode' instance to be displayed
            as side form content.  If not None, the binding referes to a "web
            form" -- an embedded browser window showing given content.  In this
            case the arguments 'name', 'binding_column', 'condition',
            'arguments' and 'prefill' make no sense and must be None.  The
            functions 'pytis.util.lcg_to_html()' or
            'pytis.util.parse_lcg_text()' may be useful if you need to display
            the formatted content of a field containing LCG Structured Text.

        """
        assert isinstance(id, basestring), id
        assert isinstance(title, basestring), title
        if name is not None:
            assert isinstance(name, basestring), name
            assert binding_column is None or isinstance(binding_column, basestring), binding_column
            assert condition is None or isinstance(condition, collections.Callable), condition
            assert arguments is None or isinstance(arguments, collections.Callable), arguments
            assert condition is not None or binding_column is not None or arguments is not None, \
                "At least one of 'binding_column', 'condition', `arguments' must be used."
            assert isinstance(single, bool), single
            assert prefill is None or isinstance(prefill, collections.Callable), prefill
            assert uri is None
        else:
            if uri is not None:
                assert isinstance(uri, collections.Callable), uri
                assert content is None, content
            else:
                assert isinstance(content, collections.Callable), content
                assert uri is None, uri
            assert name is binding_column is condition is arguments is prefill is None
        assert enabled is None or isinstance(enabled, (bool, collections.Callable)), enabled
        self._id = id
        self._title = title
        self._name = name
        self._binding_column = binding_column
        self._condition = condition
        self._descr = descr
        self._single = single
        self._enabled = enabled
        self._arguments = arguments
        self._prefill = prefill
        self._search = search
        self._uri = uri
        self._content = content

    def id(self):
        return self._id

    def title(self):
        return self._title

    def name(self):
        return self._name

    def binding_column(self):
        return self._binding_column

    def condition(self):
        return self._condition

    def descr(self):
        return self._descr

    def single(self):
        return self._single

    def enabled(self):
        return self._enabled

    def arguments(self):
        return self._arguments

    def prefill(self):
        return self._prefill

    def search(self):
        return self._search

    def uri(self):
        return self._uri

    def content(self):
        return self._content


class Editable(object):
    """Definition of available constants for field editability specification."""
    ALWAYS = 'ALWAYS'
    """The field is always editable."""
    ONCE = 'ONCE'
    """The field is only editable when a new record is being created."""
    NEVER = 'NEVER'
    """The field is never editable."""


class SelectionType(object):
    """Definition of available selection types for enumeration fields."""
    CHOICE = 'CHOICE'
    """Pull-down menu selection from enumaration display values."""
    RADIO = 'RADIO'
    """Radio box with a radio button for each enumeration display value."""
    LISTBOX = 'LISTBOX'
    """Scrollable list selection from enumaration display values."""
    LIST = 'LIST'
    """Scrollable list with all codebook columns."""
    CODEBOOK = 'CODEBOOK'
    """Hand-editable code entry field with a Codebook form invocation button."""
    CHECKLIST = 'CHECKLIST'
    """List with a checkbox for each enumeration value (only for multi-value/Array fields)."""
    # Backwards compatibility options
    RADIO_BOX = RADIO
    """Deprecated.  Equivalent to 'RADIO'"""
    LIST_BOX = LISTBOX
    """Deprecated.  Equivalent to 'LISTBOX'"""


class PostProcess(object):
    "Výčtová třída definující konstanty pro způsob zpracování uživ. vstupu."
    UPPER = 'UPPER'
    """Převeď veškerá písmena na velká."""
    LOWER = 'LOWER'
    """Převeď veškerá písmena na malá."""


class TextFilter(object):
    """Výčtová třída definující konstanty pro způsob filtrování uživ. vstupu.
    """
    ASCII = 'ASCII'
    """Non-ASCII characters are filtered out."""
    ALPHA = 'ALPHA'
    """Non-alpha characters are filtered out."""
    FLOAT = 'FLOAT'
    """Non-alpha characters exclude '.' are filtered out."""
    ALPHANUMERIC = 'ALPHANUMERIC'
    """Non-alphanumeric characters are filtered out."""
    NUMERIC = 'NUMERIC'
    """Non-numeric characters are filtered out."""
    INCLUDE_LIST = 'INCLUDE_LIST'
    """Use an include list.

    The validator checks if the user input is on the list, complaining if
    not.
    """
    EXCLUDE_LIST = 'EXCLUDE_LIST'
    """Use an exclude list.

    The validator checks if the user input is on the list, complaining if it
    is.
    """


class Computer(object):
    """Specification of a function computing a value based on row values.

    Computer functions compute a value which depends on values of fields within
    a 'PresentedRow' instance and possibly also other properties of the row.
    This class simply wraps the function (any callable object) and keeps track
    of the fields (their identifiers) on which the result depends.  This makes
    Pytis able to decide when it is necessary to recompute the value (by
    calling the function again).

    Computer functions are used at many places throughout Pytis specifications.
    They can compute for example:

      - values of virtual fields,
      - dynamic editability and visibility of form fields,
      - dynamic filters and runtime arguments for codebook field enumerators,
      - check the integrity of the row.

    """

    def __init__(self, function, depends=None, fallback=UNDEFINED):
        """Arguments:

          function -- callable object taking one argument - the 'PresentedRow'
            instance and returning the computed value.  The meaning of the
            returned value depends on the purpose for which the computer is
            used.

          depends -- sequence of field identifiers (strings), on which the
            function result depends.  It should contain all fields which are
            used in the computation.  The computed value will only be
            recomputed when the value of one of the listed fields changes.
            Empty sequence leads to no recomputations (the value is computed
            only once on initialization).

          fallback -- value used instead of function result in case, that at
            least one of the input fields (present in 'depends') contains an
            invalid value.  The computer function is not called at all in this
            case and the fallback value is returned instead.  If 'fallback' is
            undefined, the computer function is always called and it is
            responsible for taking care of invalid input explicitly.

        """
        assert isinstance(function, collections.Callable)
        assert is_sequence(depends)
        if depends is None:
            raise ProgramError("Computer has no dependency specification!")
        self._function = function
        self._depends = tuple(depends)
        self._fallback = fallback

    def __call__(self, row):
        if self._fallback is not UNDEFINED:
            for f in self._depends:
                v = row[f]
                if row.validated(f):
                    error = row.validation_error(f)
                else:
                    error = row.validate(f, v.export())
                if error:
                    return self._fallback
        return self._function(row)

    def __str__(self):
        return '<%s %s depends=%r>' % (self.__class__.__name__, self._function.__name__,
                                       self._depends)

    def function(self):
        """Return the value of 'function' as passed to the constructor."""
        return self._function

    def depends(self):
        """Return the value of 'depends' as passed to the constructor converted to a tuple."""
        return self._depends

    def fallback(self):
        """Return the value of 'fallback' as passed to the constructor."""
        return self._fallback


def computer(function=None, fallback=UNDEFINED):
    """Return a 'Computer' instance for given function.

    If necessary, wrap 'function' converting row values to named arguments.

    Computer functions are functions of one argument which is a 'PresentedRow'
    instance and return a result based on a computation which typically uses
    current row values and/or other row properties.  This wrapper makes it
    simple to define computer functions working with field values by passing
    these values as function arguments.

    If 'function' is None or if it already is a Computer instance, it is
    returned unchanged.  Otherwise it must be a function of one positional
    argument and zero or more additional arguments (positional or keyword).
    The first positional argument is a 'PresentedRow' instance.  If the
    function defines any additional arguments, it will be wrapped by a function
    which passes row values of fields matching the argument names as those
    arguments.  If no such additional arguemnts are defined, the 'function' is
    used as is.

    The argument 'fallback' corresponds to the same argument of 'Computer'
    constructor.  If used, it is simply passed on to the instance.

    For example:

        @computer
        def x(row, a, b):
            return a + b

    or:

        x = computer(lambda r, a, b: a + b)

    is equivalent to:

        def x(row):
             return row['a'].value() + row['b'].value()

    In the special case that 'fallback' is passed and 'function' is None, the
    function returns a decorator which passes 'fallback' along, so you can also
    simply write decorators as below:

        @computer(fallback=None):
        def x(row, a, b):
            return a + b

    which is equivalent to:

        x = Computer(lambda r: row['a'].value() + row['b'].value(), fallback=None)

    Note: Pytis specifications accept 'Computer' instances for many of its
    properties.  Some of them explicitly state in documentation, that they are
    automatically wrapped by 'computer()'.  In this case you may define the
    computer functions in the readable form and pass them directly to those
    properties.  They will be automatically wrapped by 'computer()' behind the
    scenes and converted to 'Computer' instances if needed.  It is actually
    planned to wrap by 'computer()' in all cases but it is added gradually so
    you can rely on it only where documented.  In other cases use 'computer()'
    explicitly or pass 'Computer' instances directly.

    """
    if function is None and fallback is not UNDEFINED:
        def result(function):
            return computer(function, fallback=fallback)
    elif function is None or isinstance(function, Computer):
        result = function
    else:
        assert isinstance(function, collections.Callable), function
        depends = argument_names(function)[1:]
        if depends:
            original_function = function
            def function(row):
                kwargs = {name: row[name].value() for name in depends}
                return original_function(row, **kwargs)
        result = Computer(function, depends=depends, fallback=fallback)
    return result


class CbComputer(Computer):
    """Specializovaný computer, který získává hodnotu z číselníku.

    Tento computer automaticky poskytuje dopočítávací funkci, která získává
    hodnotu z některého sloupce číselníku navázaného na jiné políčko stejného
    náhledu.  Toho lze využít u políček, která ve skutečnosti jen zobrazují
    doplňující informace z číselníku.

    Stejného efektu by sice šlo dosáhnout i použitím standardního computeru s
    příslušnou dopočítávací funkcí, ale tím by se uživatelské rozhraní
    nedozvědělo nic o vazbě dopočítávaného políčka na políčko s enumerátorem a
    nebylo by schopno poskytnout další užitečné funkce, jako například otevření
    náhledu číselníku jako akce kontextového menu buňky, zobrazení klíče
    číselníku při aktivaci buňky, automatické určení datového typu virtuálního
    políčka apod.

    """
    def __init__(self, field, column, default=None):
        """Inicialize the instance.

        Arguments:

          field -- identifier of a field in the same specification, which provides codebook
            values.  This field must have an enumerator of type 'DataEnumerator' (must be a
            codebook field).

          column -- identifier of a column within the enumerator data object.  The computer will
            return the value of this column as its result.


        """
        assert isinstance(field, basestring)
        assert column is None or isinstance(column, basestring)
        self._field = field
        self._column = column
        self._default = default
        super(CbComputer, self).__init__(self._compute, depends=(field,))

    def _compute(self, row):
        cbvalue = row[self._field]
        if cbvalue.value() is not None:
            e = cbvalue.type().enumerator()
            assert e is not None, "CbComputer field '%s' has no enumerator." % self._field
            row = e.row(cbvalue.value(), transaction=row.transaction(),
                        condition=row.runtime_filter(self._field),
                        arguments=row.runtime_arguments(self._field))
            if row:
                value = row[self._column].value()
                if value is None:
                    return self._default
                return value
        return self._default

    def field(self):
        """Vrať id políčka, jehož enumerátor je použit."""
        return self._field

    def column(self):
        """Vrať id sloupce datového objektu enumerátoru, který udává hodnotu."""
        return self._column


class CodebookSpec(object):
    """Specification of codebook properties of given view.

    The specification of any view may define the properties of the view, when used as a codebook
    (see the 'codebook' argument of 'Field' for more information about codebooks).

    'CodebookSpec' may be defined as the 'cb' attribute of a 'Specification'.

    """
    def __init__(self, columns=None, sorting=None, display=None, prefer_display=False,
                 display_size=20, enable_autocompletion=True, begin_search=None):
        """Arguments:

          display -- defines the method of retrieving the user visible value of
            the codebook item (see below for more information about user
            visible values).  None (the default value) means to use the
            codebook code as the user visible value (no conversion).  A string
            value refers to a column in the *data* object of the codebook.  The
            user visible value is retrieved from given column.  You may also
            pass a function (callable object).  The user visible value is then
            computed by invoking the function, passing it the code (internal
            Python value of the codebook field) as an argument.  The returned
            value must be a string.  If a function is passed and this function
            has just one argument named 'row', the function will receive the
            data row of the corresponding codebook data object as an argument
            (instead of just the internal codebook value).
          prefer_display -- If true, the user interface will show the display
            value instead of the codebook internal value wherever possible.
            For example the browse from will show the display value instead of
            the code (the display is normally only shown in the status line or
            tooltip).  See below for more details
          display_size -- width of the codebook display field in characters.
            It is possible to override this value by the argument of the same
            name within the field specification (for particular field).
          enable_autocompletion -- enable autocompletion (True by default).  If
            false, autocompletion will be disabled for all codebook fields
            refering to this codebook.  This may be practical for example for
            slow codebooks where autocompletion queries would slow down user
            interaction within the codebook field.
          begin_search -- None or an identifier of a column, where incremental
            search would be automatically started when a codebook form is
            invoked (GUI only).

        The user visible value of the codebook is used in several situations.  The codebook field
        ('SelectionType.CODEBOOK') will show it in a display next to the form control for entering
        the codebook value.  Other fields (such as 'CHOICE' or 'RADIO') will directly shoe the user
        value (id 'display' was defined) and the internal value is not visible.  Another usage is
        in browse forms (tables), where the value of the display is shown in the application status
        line (GUI) or in a tooltip (web).  If 'prefer_display' is true, however, the display value
        will be used in browse forms directly.

        Arguments 'columns' and 'sorting' are deprecated.  Use a derived specification if you need
        to modify the parameters of the underlying view.

        """
        assert columns is None or is_sequence(columns)
        assert sorting is None or is_sequence(sorting)
        assert (display is None or
                isinstance(display, basestring) or
                (isinstance(display, collections.Callable) and len(argument_names(display)) == 1))
        assert isinstance(prefer_display, bool)
        assert display_size is None or isinstance(display_size, int)
        assert begin_search is None or isinstance(begin_search, basestring)
        assert isinstance(enable_autocompletion, bool)
        self._columns = columns
        self._sorting = sorting
        self._display = display
        self._prefer_display = prefer_display
        self._display_size = display_size
        self._begin_search = begin_search
        self._enable_autocompletion = enable_autocompletion

    def columns(self):
        return self._columns

    def sorting(self):
        return self._sorting

    def display(self):
        """Return the 'display' specification as passed to the constructor."""
        return self._display

    def prefer_display(self):
        """Return true if the display should be preferred over the internal codebook value."""
        return self._prefer_display

    def display_size(self):
        """Return the display size in characters."""
        return self._display_size

    def begin_search(self):
        """Return the identifier of the column where incremental search should be started."""
        return self._begin_search

    def enable_autocompletion(self):
        return self._enable_autocompletion

    def arguments(self):
        return self._arguments


class FormType(object):
    """Specifikace abstraktního typu formuláře podle účelu jeho otevření.

    Tyto konstanty slouží k určení způsobu otevření náhledu.  Díky této
    abstrakci je specifikace nezávislá na použitých třídách uživatelského
    rozhraní.  Definujeme pouze účel, za kterým je formulář otevírán a necháme
    uživatelské rozhraní rozhodnout, který konkrétní formulář je v dané situaci
    nejvhodnější.

    """

    BROWSE = 'BROWSE'
    """Otevření řádkového náhledu v podobě tabulky."""

    VIEW = 'VIEW'
    """Otevření needitovatelného náhledu jednoho záznamu."""

    BROWSABLE_VIEW = 'BROWSABLE_VIEW'
    """Otevření needitovatelného náhledu jednoho záznamu s možností procházení záznamů."""

    EDIT = 'EDIT'
    """Otevření editačního formuláře jednoho záznamu."""

    INSERT = 'INSERT'
    """Otevření editačního formuláře pro vložení nového záznamu."""


class Link(object):
    """Specification of a link from field to a different view.

    Used as a value of 'Field' constructor argument  'link'.

    """
    def __init__(self, name, column, type=FormType.BROWSE, binding=None, label=None,
                 enabled=True, filter=None, arguments=None):
        """Arguments:

          name -- name of the referred specification as a string.

          column -- column identifier in the referred specification.  This
            column is used to locate the record corresponding to the current
            value of the refering field.

          type -- type of the form used to display the referred view/record as
            one of 'FormType' constants.  The default type is
            'FormType.BROWSE'.  This argument may also be used as positional.

          binding -- 'Binding' specification identifier (string) determining
            the current side view shown along with the referred record.  A
            binding with given identifier must exist in the 'bindings'
            specification of 'name'.

          label -- titulek odkazu v menu.  Pokud není uveden, bude odkaz
            pojmenován automaticky a zařazen mezi automaticky generované
            odkazy.  Pokud je titulek uveden, bude v uživatelském rozhraní
            odkaz uveden samostatně před všemi automaticky generovanými odkazy.

          enabled -- function of one argument ('PresentedRow' instance)
            returning True when the link is active for given record or False
            otherwise.  A boolean value may also be used directly instead of a
            function when the state can be defined statically (or determined in
            advance).

          filter -- function of one argument ('PresentedRow' instance)
            returning a filtering condition ('pytis.data.Operator' instance) to
            be used for filtering the newly opened form.  Only relevant for
            links with 'type'='FormType.BROWSE'.

          arguments -- function of one argument ('PresentedRow' instance)
            returning a dictionary of table function arguments to be passed to
            the form.  Only relevant for links with 'type'='FormType.BROWSE'
            where the target specifiation is based on a table function.

        """
        assert isinstance(name, basestring)
        assert isinstance(column, basestring)
        assert binding is None or isinstance(binding, basestring)
        assert type in public_attributes(FormType)
        assert label is None or isinstance(label, basestring)
        assert isinstance(enabled, collections.Callable) or isinstance(enabled, bool)
        assert filter is None or isinstance(filter, collections.Callable), filter
        assert filter is None or type == FormType.BROWSE, (filter, type)
        assert arguments is None or isinstance(arguments, collections.Callable), arguments
        assert arguments is None or type == FormType.BROWSE, (arguments, type)
        self._name = name
        self._column = column
        self._binding = binding
        self._type = type
        self._label = label
        self._enabled = enabled
        self._filter = filter
        self._arguments = arguments

    def __str__(self):
        return '<Link %s %s>' % (self._name, self._column,)

    def name(self):
        return self._name

    def column(self):
        return self._column

    def type(self):
        return self._type

    def binding(self):
        return self._binding

    def label(self):
        return self._label

    def enabled(self):
        return self._enabled

    def filter(self):
        return self._filter

    def arguments(self):
        return self._arguments


class ListLayout(object):
    """Specification of list layout.

    Currently only implemented in web forms.

    This layout defines an alternative presentation of lists of records.  The
    records are not presented as a table, but as sections, where each record
    has its own heading, meta information and text (description, annotation,
    message...).

    """
    def __init__(self, title, meta=(), layout=None, content=(), image=None, anchor=None,
                 meta_labels=False, columns=1, allow_index=False, popup_actions=False):
        """Arguments:

          title -- identifier of a field which will be used as a title for each
            item in the list (a string).  An 'lcg.TranslatableText' instance
            can also be passed as an interpolation template.  In this case the
            title will be produced by interpolation of formatted row values
            within given string (with python string formatting syntax).

          meta -- a sequence of field identifiers (strings) which will be
            printed underneath each item's title as records meta information.
            A single item may be passed as a string directly.

          layout -- 'GroupSpec' instance describing the layout of a fields
            within each item's section.  If used (not None), the fields will be
            displayed for each record in a manner simillar to a show form.
            Similarly as for the 'layout' argument in 'ViewSpec', it is also
            possible to pass a sequence of fields (or 'GroupFpec' instances)
            which will be turned into a vertical group automatically.

          content -- free content provider a field identifier or a function.
            If a function is used it must accept one argument (a PresentedRow
            instance) and return 'lcg.Content' or None.  If a field identifier
            is used, the field value must be a string formatted as LCG
            structured text.  The field text is parsed to produce the content.
            A sequence of functions or field identifiers may also be used to
            provide more pieces of content concatenated together on the output.

          image -- identifier of a field which provides an image to be
            displayed along with each record.

          meta_labels -- boolean flag indicating, whether 'meta' fields should
            be labeled.  If a sequence is passed, only meta fields with
            identifiers contained within the sequence will be babeled.

          columns -- number of columns; integer.

          allow_index -- boolean flag indicating, whether an index of all
            displayed records should be displayed at the top of the list.  Each
            item in this index makes it possible to jump directly to the
            record.

          popup_actions -- display row actions (if enabled) as a popup menu of
            record title heading, rather than action buttons below the item
            content.

        """
        if isinstance(layout, (list, tuple)):
            layout = GroupSpec(layout, orientation=Orientation.VERTICAL)
        else:
            assert layout is None or isinstance(layout, GroupSpec)
        if isinstance(meta_labels, bool):
            meta_labels = meta_labels and xtuple(meta) or ()
        else:
            assert isinstance(meta_labels, (bool, tuple, list))
        assert isinstance(columns, int)
        assert isinstance(allow_index, bool)
        self._title = title
        self._meta = xtuple(meta)
        self._content = xtuple(content)
        self._layout = layout
        self._image = image
        self._anchor = anchor
        self._meta_labels = meta_labels
        self._columns = columns
        self._allow_index = allow_index
        self._popup_actions = popup_actions

    def title(self):
        return self._title

    def meta(self):
        return self._meta

    def content(self):
        return self._content

    def layout(self):
        return self._layout

    def image(self):
        return self._image

    def anchor(self):
        return self._anchor

    def meta_labels(self):
        return self._meta_labels

    def columns(self):
        return self._columns

    def allow_index(self):
        return self._allow_index

    def popup_actions(self):
        return self._popup_actions


class Enumeration(object):
    """Specification of an enumeration.

    This class defines a code based enumeration (as oposed to database based
    enumeration typically implemented using a codebook with its own pytis
    'Specification').

    You can define an enumeration directly within 'Field' specification by
    defining 'enumerator', 'display', 'selection_type', 'prefer_display' and,
    'default' attributes.  The same combination of those attributes, however,
    typically tends to repeat several times within one application.  This is
    why you may group them into this class and refer to the same class several
    times.  When a class derived from 'Enumeration' is passed to the
    'enumerator' attribute of a 'Field' specification, all the above named
    attributes (when nor redefined explicitly for given field) will default to
    values defined by this enumeration.  Note, that the default values of some
    attributes may be different in this class than the default values of the
    same 'Field' specification attributes.  See the documentation of individual
    attributes below.

    Code based enumerations are typically practical when the application logic
    depends on the knowledge of enumeration values. They can also define
    translatable labels (the translations are often easier to manage within the
    code than in the database).

    """
    enumeration = ()
    """Sequence of pairs (value, label) determining inner values and displayed labels.

    The inner values must correspond to the python value of the data type of
    the 'Field' specification where the enumeration is used (typically python
    strings or ints).  The resulting enumerator will be a
    'pytis.data.FixedEnumerator' with these values.

    Labels are user interface strings representing the corresponding inner
    value.  The default 'display' of a Field using this enumeration will return these
    labels.
    """

    default = None
    """Overrides Field's 'default' attribute.

    Must be one of the enumeration's inner values.
    """

    selection_type = SelectionType.CHOICE
    """Overrides Field's 'selection_type' attribute."""

    orientation = Orientation.VERTICAL
    """Overrides Field's 'orientaion' attribute."""

    null_display = None
    """Overrides Field's 'null_display' attribute."""

    prefer_display = True
    """Overrides Field's 'prefer_display' attribute.

    Note, that the default value is True here, while the default value of the
    Field attribute is False, so when Enumeration is used, the default is to
    always show labels instead of enumeration inner values.

    """


class Field(object):
    """Specification of a generic form field representing a data value.

    This specification is used for all kinds of situations where fields are
    used to display and/or edit values, such as screen forms, tables, print
    reports etc.

    Only certain subset of information defined here is relevant for each
    situation.  The exact way of interpretation of this specification depends
    on classes implementing the user interface and the details also depend on
    the kind of user interface (GUI forms, web forms, printed reports).  Not
    all features may be supported by all user interface implementations.

    """
    def __init__(self, id=None, label=None, column_label=None, inherit=None, **kwargs):
        """Arguments:

          id -- field identifier as a string.  This identifier is used to refer
            to the field within all pytis operations.  The identifier is also
            used as the name of the related column in the underlying data
            object by default, but this may be overriden by the 'dbcolumn'
            argument.  This argument is typically passed as positional.
          label -- user visible field label as a string or unicode.  This
            argument is also typically passed as positional.
          column_label -- optional field label in the column view.  The column
            label is the same as 'label' by default, but may be overriden by
            passing a string or unicode value.  This argument (unlike the
            remaining arguments) may also be passed as positional.
          inherit -- deprecated - use the method 'clone()' to implement field
            inheritance.
          descr -- brief field description in the extent of approx. one
            sentence, suitable for example for tooltip text.
          virtual -- boolean flag indicating that the field is not bound to the
            underlying data object.  The value of a virtual field will most
            often be computed on the fly by a 'Computer'.  See the argument
            'computer' for more information.  Since the data type of a virtual
            field cannot be obtained from the data object, the hardcoded
            default type of virtual fields is 'pytis.data.String'.  Use the
            'type' argument to override it.
          dbcolumn -- name of the related column in the underlying data object.
            The name is the same as the field identifier by default.  It is not
            recommended to use different column name than the field identifier
            unless there is a serious reason for it.
          type -- explicit data type as a 'pytis.data.Type' class or instance.
            None value means to use the default type determined from the
            underlying data object (or the default type 'pytis.data.String'
            for virtual fields not present in the data object).  If a class is
            passed, the instance of this class will be created automatically
            and the system will pass it all arguments which it is able to
            determine.  If an instance is passed it is used as is even if the
            system would create it with other arguments.  So by passing a class
            you leave the system to do its best, while by passing an instance,
            you can force whatever you want.  In any case, the specified type
            must be compatible with the type determined by the underlying data
            object.  If None or if a class is specified, individual type
            constructor arguments may be passed separately as additional
            'Field' keyword arguments and they take precedence over the
            arguments determined by system.  Thus this is a more gentle way to
            force specific properties of the field data type individually.  The
            arguments 'codebook' and 'enumerator' determine the constructed
            type's enumerator.
          width -- field width in characters (integer).  Default width is
            determined automatically if not specified here.  Certain types of
            input fields may interpret the value differently (e.g. as a number
            of columns) when number of characters doesn't make sense.
          height -- field height in characters (integer).  Certain types of
            input fields may interpret the value differently (e.g. as a number
            of rows) when number of characters doesn't make sense.
          column_width -- table column width in characters (integer).  If not
            defined, defaults to 'width'.
          disable_column -- If true, it is not possible to display the field as
            a table column.  The field does not appear in the selection of
            columns to display and presence of such field in default columns
            ('ViewSpec' argument 'columns') is announced as an error.
          fixed -- passing True value will disable automatic scaling of column
            width when the table size is changed.  The default behavaior is to
            accommodate column widths to new form size, so that the available
            space is used evenly between all columns.  Fixed columns, however
            will be left out during these recomputations and will keep their
            prevoius widths.
          editable -- one of 'Editable' constants or a 'Computer' instance.
           The constants determine field editability statically, the computer
            may be used to compute editability dynamically based on the values
            of other fields of a record and return true iff the field is
            editable (see also notes about computer specifications below).  The
            default value is 'Editable.ALWAYS'.
          visible -- boolean value or a 'Computer' instance returning the
            visibility (boolean result) dynamically.  Returning false will
            exclude the field from all kinds of forms (table columns or or form
            layout).  Note that for table columns the visibility is not
            computed separately for each row.  The computer function is called
            just once with an initialized PresentedRow instance without
            particular row values.  Currently only implemented in web forms.
          compact -- true value results in the field label being displayed
            above the field, not on the left which is the default.  This way
            the field will span to the full width of the field group.
          nocopy -- iff true, the field's value will be omitted during record
            copying (user command for creation of a new record as a copy of an
            existing record).  Key columns and computed fields depending on key
            columns are omitted automatically.
          default -- default value or a function for computing the default
            value.  The default value is used when a new record is initialized.
            Please note, that if computer is defined, it has higher precedence
            than the default value.  You may pass a value directly or a
            callable object.  The callable object will be called with no
            arguments when the default value is needed and its returned value
            will be used.  In any case, the default value must be compatible
            with the internal Python representation for the data type of the
            field.  If not defined, the default value is determined by the data
            type (usually 'None').
          computer -- a 'Computer' instance for computing the field value based
            on the values of other fields of the same record.  See below for
            more details about computed fields.
          null_display -- display value (string) to use for the unselected
            state of an enumeration field (null field value).  Null value is
            not part of the enumeration, but if the field is not 'not_null', it
            is a valid field value, but as it is not within the enumeration,
            'display' may not be used.
          inline_display -- name of the codebook display column in the current
            table (string).  The 'display' column refers to a column in the
            codebook table.  Retrieving codebook values from the codebook table
            when 'prefer_display' is true would cause nested database queries
            for each row in the list.  To optimize such inefficiency, it is
            possible to specify the name of the display column in the current
            table (where the current codebook field is used).  The application
            developer is responsible to include the display column in the
            underlying database view (typically by joining it with the codebook
            table).  The inline_display column doesn't need to be explicitly
            defined in 'fields' specification -- it is appended automatically if
            not present.
          inline_referer -- name of the codebook referer column in the current
            table (string).  The 'referer' specification in the codebook's
            'ViewSpec' refers to a column in the codebook table.  Retrieving
            referer values from the codebook table would cause nested database
            queries for each row in the list in cases, where the referer column
            is not the same column as the 'value_column'.  To optimize such
            inefficiency, it is possible to specify the name of the referer
            column in the current table (where the current codebook field is
            used).  The application developer is responsible to include the
            referer column in the underlying database view (typically by
            joining it with the codebook table).
          formatter -- a function used to format the field value for
            presentation purposes.  The function will get the current field
            inner value as argument and must return a basestring.  The
            formatter function actually replaces field type's export method.
            It is applied by the method 'PresentedRow.format()' so it should be
            used for all read-only occurences of the field in the user
            interface.  Editable fields, on the other hand, work with the
            internally exported value, not the formatted one.
          line_separator -- line separator in single line field value
            presentation.  The method 'PresentedRow.format()' will concatenate
            multiline values into a single line using this string.  If
            'formatter' is defined, 'line_separator' is applied to its result
            if needed.
          codebook -- name (string) of the specification which acts as a
            codebook for this field.  This argument has two effects.  It is
            used as the default value of 'enumerator' (if 'enumerator is not
            set explicitly) and it determines the name of the specification
            used for codebook form invocation for 'SelectionType.CODEBOOK'.
            Specifying 'codebook' causes 'selection_type' to default to
            'SelectionType.CODEBOOK'.  From the other perspective
            'SelectionType.CODEBOOK' requires 'codebook' to be defined.
          enumerator -- determines the field'd data type enumerator.  You can
            pass a 'pytis.data.Enumerator' instance directly, a string
            specification name, which will be automatically converted to the
            corresponding 'pytis.data.DataEnumerator' instance,
            'pytis.data.DataFactory' instance (also converted to
            'pytis.data.DataEnumerator'), a list or tuple which will be
            converted to a 'pytis.data.FixedEnumerator' instance or a class
            derived from 'Enumeration'.  When an 'Enumeration' class is used,
            it will be converted to a 'pytis.data.FixedEnumerator' instance and
            other attributes, such as 'display', 'prefer_display' etc. of this
            field will be influenced as described in the 'Enumeration' class
            docstring.  If None, the enumerator will default to the value of
            'codebook'.
          display -- overrides the same 'CodebookSpec' option for this
            particular field.  If not defined, the value defaults to the value
            defined by the related codebook.
          prefer_display -- overrides the same 'CodebookSpec' option for this
            particular field.  If not defined, the value defaults to the value
            defined by the related codebook.
          display_size -- overrides the same 'CodebookSpec' option for this
            particular field.  If not defined, the value defaults to the value
            defined by the related codebook.
          allow_codebook_insert -- true value enables a button for codebook new
            record insertion.  This button is displayed next to the codebook
            field.
          codebook_insert_spec -- name of the specification to use for codebook
            insertion when 'allow_codebook_insert' is true.  If none, the value
            defined by 'codebook' is used.
          codebook_insert_prefill -- function returning a dictionary of values
            to prefill into a new codebook row when the new codebook record is
            created in the context of the current field.  The function will get
            a PresentedRow instance as an argument representing the current
            record.  Only relevant when 'allow_codebook_insert' is true.
          codebook_update_prefill -- function returning a dictionary of values
            to set on codebook row editation when a codebook record is updated
            from within an active value of a ListField.  The function will get
            a PresentedRow instance as an argument representing the current
            record.  If the codebook specification defines 'on_edit_record',
            the result is passed to 'on_edit_record' function as a keyword
            argument 'prefill'.  Otherwise it is directly prefilled within the
            edit form.
          runtime_filter -- provider of enumeration runtime filter as a
            'Computer' instance.  The computer function computes the filtering
            condition based on the current row data.  This condition is used to
            filter out enumerator data for codebook fields as well as available
            completions when autocompletion is enabled.  This is mostly useful
            for modification of available codebook values based on the current
            values of other fields within the form.  The returned value is a
            'pytis.data.Operator' instance when the enumerator is a
            'pytis.data.DataEnumerator' (enumeration values are in a database
            table) or a function otherwise (typically for static enumerations
            using 'pytis.data.FixedEnumerator').  None may be returned in both
            cases when no filtering is to be done.  The function in the later
            case must accept the enumeration value as an argument and return
            True when the value is valid or False if the value is to be
            filtered out.
          runtime_arguments -- provider of codebook table function arguments as a
            'Computer' instance.  This is similar to 'runtime_filter' argument,
            except that the computer function returns dictionary of table
            function arguments.  'runtime_arguments' may be provided only when the
            field is a codebook field and the codebook is actually a row
            returning function.  Otherwise 'runtime_arguments' must be 'None'.
          completer -- enumerator used for automatic completion.  The available
            completions are taken from an enumerator object.  If the field has
            an enumerator (defined by 'enumerator' or 'codebook'), it will be
            used for completions automatically (unless autocompletion is
            disabled by the relevant 'CodebookSpec').  This argument, however,
            makes it possible to specify a completer even for fields, which
            don't have an enumerator (the validation constraints imposed by
            enumerator are not desirable).  The value of this argument may be
            an enumerator instance directly (e.g. 'pytis.data.FixedEnumerator')
            or a name of the specification used to create a
            'pytis.data.DataEnumerator'.  Also a sequens (list or tuple) is
            accepted and converted to a 'FixedEnumerator' instance.
          selection_type -- one of 'SelectionType' constants defining the type
            of user interface element used to present the related enumeration.
            Only relevant for fields with an enumerator (specified either by
            'codebook' or 'enumerator').  If 'codebook' is not None,
            selection_type defaults to 'SelectionType.CODEBOOK'.  Also if
            selection_type is set to 'SelectionType.CODEBOOK', 'codebook' must
            be defined.
          orientation -- field orientation as one of 'Orientation' class
            constants; relevant only for certain field types, such as radio
            buttons, which may be arranged vertically or horizontally.
          post_process -- funkce upravující vkládaný text během psaní.  Jedná
            se o funkci jednoho argumentu, kterým je řetězcová hodnota políčka.
            Vrácená hodnota je potom nastavena jako nová hodnota políčka.  Tato
            funkce je volána při každé změně hodnoty textového políčka.
            Příkladem postprocessingu může být změna velikosti písmen, pokud
            chceme, aby textové políčko mohlo obsahovat jen velká písmena.
            Hodnotou tohoto argumentu může být také některá z konstant třídy
            'PostProcess', čímž je ušetřeno psaní některých často používaných
            funkcí.
          filter -- one of 'TextFilter' class constants for filtering the user
            input.
          filter_list -- sequence of included/excluded characters for 'filter'
            using 'TextFilter.INCLUDE_LIST' or 'TextFilter.EXCLUDE_LIST'.
          style -- visual field appearance as a 'Style' instance or a function
            of one argument ('PresentedRow' instance) returning the 'Style'
            instance dynamically.  If not None, it overrides the row level
            style defined by 'row_style' argument of 'ViewSpec'.  Useful for
            highlighting cells in a table view depending on their values (such
            as negative numbers in red).
          link -- specification of a link, or a series of links to other forms
            related to the current field value.  The value is a 'Link' instance
            or their sequence.  The links will be presented as separate menu
            items in the context menu of a record in the GUI.  The web forms
            currently only support one link per field and present it as a
            hypertext link on field's value.  The links will open the related
            form and locate the record corresponding to the value of the
            referring field.
          filename -- provides the file name for downloading/saving the field
            value into a file.  The value of this argument may be a function of
            one argument (a PresentedRow instance representing the current row)
            or a string identifier of the field, which contains the filename.
            This is relevant for binary fields, but may be also used for string
            fields, where it forces the user interface to provide controls for
            downloading/saving the content of the field as a file.
          filename_extensions -- sequence of allowed filename extensions to be
            used for filtering a file open dialog contents when selecting a
            file to be used as field value.  Relevant for binary fields, but
            may be also used for string fields with 'filename' set (see above).
          text_format -- One of the available 'TextFormat' constants defining
            the format of the field text.  Only relevant for textual fields (of
            type 'pytis.data.String').  The default format 'TextFormat.PLAIN'
            denotes that the field contains just ordinary text.  The format
            'TextFormat.LCG' may be used to indicate that the field contains a
            text with special formatting (see 'TextFormat.LCG' documentation
            for more information about the format) and the user interface
            should respect this (field editation may offer some extended
            controls, the field text is processed by LCG before displayed).
            Other formats are currently unsupported.
          attachment_storage -- instance of 'AttachmentStorage' subclass, None
            or a function of row (a 'PresentedRow' instance representing the
            current row), returning one of those.  Attachments are used with
            rich text fields to represent external resources used within the
            text, such as images or other media files.  This argument is
            currently supported by wx forms for fields with
            text_format='TextFormat.LCG'.
          printable -- iff True, the user interface should allow the value of
            this field to be printed as a separate document.  This is most
            often useful with fields containing structured text content, which
            may be directly exported into PDF.
          slider -- set to True to add a slider control to a numeric field.
            The slider can be dragged by mouse (or keyboard) to set the value
            instead of typing it.  Use 'minimum' and 'maximum' type constructor
            arguments (see 'pytis.data.Number') to set the slider range.  The
            default range when 'minimum' and 'maximum' are unset is 0..100.
          crypto_name -- if not 'None' then the field column values are stored
            encrypted in the database and the argument value is a string
            identifier of the protection area.  There can be defined several
            different protection areas identified by corresponding crypto names
            in the application, protected by different passwords.  Not all data
            types support encryption, it is an error to set encryption here for
            field types which don't support it.
          check -- 'Computer' instance to verify the integrity of the whole
            record.  May also by specified as a function in which case it will
            be automatically turned into a 'Computer' instance using
            'computer()'.  In contrast to "validation" which only verifies
            whether a single value matches its data type and constraints
            (values of other fields are not available during validation), the
            check function verifies mutual compatibility of all form values and
            is called only after successful validation.  The check function
            returns None in case of success or an error message as a string in
            case of failure.
          encrypt_empty -- if True (default) then encrypt also None values (and
            empty values when they are represented by None values).  Otherwise
            store empty values as NULLs in the database.  Empty values should
            be commonly encrypted in the databases so that there is no
            information about secret data.  But when you want to allow
            unauthorized users to work with encrypted data in a limited way,
            e.g. to insert new records with empty secret values, then setting
            this argument to False is useful.
          **kwargs -- all the remaining keyword arguments are passed to the
            constructor of field's data type instance.  These arguments
            override the values of arguments, that the system would normally
            use for data type construction, so you can override certain data
            type properties this way.  It is preferred to overriding the type
            completely by passing a 'pytis.data.Type' instance as the 'type'
            argument.  See also 'type' argument's documentation.

        Je-li specifikován argument 'computer' a jeho hodnota není 'None', pak
        hodnota sloupce, pokud ji nelze převzít z datového objektu, je
        počítána.  Takový sloupec může být plně \"virtuální\", tj. není
        přítomen v datovém objektu a jeho hodnota je vždy počítána, nebo může
        být v datovém objektu, avšak hodnota je dopočítávána v průběhu editace
        (i nově vytvořeného) záznamu.  Použití plně virtuálních sloupců není
        doporučováno z důvodu výkonnostních problémů v rozsáhlejších
        tabulkových náhledech.  U plně virtuálních políček je také nutné určit
        explicitně datový typ pomocí specifikátoru 'type', protože není možné
        jej převzít automaticky z datového objektu.  Jedinou výjimkou jsou
        dopočítávaná virtuální políčka typu 'CbComputer', kde je typ převzat z
        datového objektu enumerátoru.

        Dopočítávání pomocí 'computer' nelze zaměňovat s výpočtem výchozí
        hodnoty (specifikátor 'default').  Výpočet výchozí hodnoty je proveden
        pouze jednou při vytváření nového řádku.  Funkce pro výpočet výchozí
        hodnoty nezná hodnotu ostatních políček a v průběhu editace se již
        neuplatňuje.  Computer naproti tomu přepočítává hodnotu políčka vždy,
        když dojde ke změně hodnoty políčka, na kterém je závislý (viz
        dokumentace třídy 'Computer').

        Závislosti počítaných políček mohou být i tranzitivní (počítaná políčka
        mohou záviset na jiných počítaných políčkách), ale graf závislostí musí
        tvořit strom (nesmí vzniknout cyklus).

        """
        for key, value in (('id', id), ('label', label), ('column_label', column_label)):
            if value is not None:
                kwargs[key] = value
        if inherit:
            assert isinstance(inherit, Field), inherit
            kwargs = dict(inherit._kwargs, **kwargs)
        self._kwargs = kwargs
        self._init(**kwargs)

    def _init(self, id, label=None, column_label=None, descr=None, virtual=False,
              dbcolumn=None, type=None, type_=None, width=None, column_width=None,
              disable_column=False, fixed=False, height=None,
              editable=Editable.ALWAYS, visible=True, compact=False, nocopy=False,
              default=None, computer=None, formatter=None, line_separator=';',
              codebook=None, display=None, prefer_display=None, display_size=None,
              null_display=None, inline_display=None, inline_referer=None,
              allow_codebook_insert=False, codebook_insert_spec=None,
              codebook_insert_prefill=None, codebook_update_prefill=None,
              codebook_runtime_filter=None, runtime_filter=None,
              runtime_arguments=None, selection_type=None, completer=None,
              orientation=None, post_process=None, filter=None, filter_list=None,
              style=None, link=(), filename=None, filename_extensions=(),
              text_format=TextFormat.PLAIN, attachment_storage=None, printable=False,
              slider=False, enumerator=None, value_column=None, validity_column=None,
              validity_condition=None, check=None, crypto_name=None, encrypt_empty=True,
              **kwargs):
        def err(msg, *args):
            """Return assertion error message."""
            return "Field '%s': " % id + msg % args

        def log_(msg, *args):
            import inspect
            filename, line = inspect.stack()[3][1:3]
            log(OPERATIONAL, "%s, line %s: %s" % (filename, line, msg % args))
        assert isinstance(id, basestring)
        assert dbcolumn is None or isinstance(dbcolumn, basestring), dbcolumn
        if type_ is not None:
            # type_ is deprecated!
            assert type is None
            type = type_
        assert label is None or isinstance(label, basestring), label
        assert column_label is None or isinstance(column_label, basestring), column_label
        assert descr is None or isinstance(descr, basestring), descr
        assert type is None or isinstance(type, pytis.data.Type) \
            or issubclass(type, pytis.data.Type), type
        assert isinstance(virtual, bool), virtual
        assert isinstance(disable_column, bool), disable_column
        assert isinstance(fixed, bool), fixed
        if editable is None:
            # For backwards compatibility - some specifications use it...
            editable = Editable.ALWAYS
        assert isinstance(editable, Computer) or editable in public_attributes(Editable), editable
        assert isinstance(compact, bool), compact
        assert isinstance(nocopy, bool), nocopy
        assert computer is None or isinstance(computer, Computer), computer
        assert formatter is None or isinstance(formatter, collections.Callable), formatter
        assert line_separator is None or isinstance(line_separator, basestring), line_separator
        assert codebook is None or isinstance(codebook, basestring), codebook
        assert display is None or isinstance(display, (basestring, collections.Callable)), display
        assert (completer is None or
                isinstance(completer, (basestring, tuple, pytis.data.Enumerator))), completer
        assert prefer_display is None or isinstance(prefer_display, bool), prefer_display
        assert display_size is None or isinstance(display_size, int), display_size
        assert null_display is None or isinstance(null_display, basestring), null_display
        assert inline_display is None or isinstance(inline_display, basestring), inline_display
        assert inline_referer is None or isinstance(inline_referer, basestring), inline_referer
        assert isinstance(allow_codebook_insert, bool), allow_codebook_insert
        assert codebook_insert_spec is None \
            or isinstance(codebook_insert_spec, basestring), codebook_insert_spec
        assert codebook_insert_prefill is None \
            or isinstance(codebook_insert_prefill, collections.Callable), codebook_insert_prefill
        assert codebook_update_prefill is None \
            or isinstance(codebook_update_prefill, collections.Callable), \
            codebook_update_prefill
        assert width is None or isinstance(width, int)
        if codebook_runtime_filter is not None:
            assert runtime_filter is None
            runtime_filter = codebook_runtime_filter
        assert runtime_filter is None or isinstance(runtime_filter, Computer), runtime_filter
        assert (runtime_arguments is None or
                isinstance(runtime_arguments, Computer)), runtime_arguments
        assert (selection_type is None or
                selection_type in public_attributes(SelectionType)), selection_type
        assert orientation is None or orientation in public_attributes(Orientation), orientation
        assert post_process is None or isinstance(post_process, collections.Callable) \
            or post_process in public_attributes(PostProcess), post_process
        assert filter is None or filter in public_attributes(TextFilter), filter
        assert (filter not in ('INCLUDE_LIST', 'EXCLUDE_LIST') or
                is_sequence(filter_list)), filter_list
        assert style is None or isinstance(style, (Style, collections.Callable)), \
            err("Invalid 'style' specification: %s", style)
        assert (filename is None or
                isinstance(filename, (basestring, collections.Callable))), filename
        assert isinstance(filename_extensions, (list, tuple)), filename_extensions
        assert text_format in public_attr_values(TextFormat), text_format
        assert attachment_storage is None or \
            isinstance(attachment_storage, (AttachmentStorage, collections.Callable)), \
            attachment_storage
        assert isinstance(printable, bool), printable
        assert crypto_name is None or isinstance(crypto_name, basestring), crypto_name
        assert encrypt_empty is None or isinstance(encrypt_empty, bool), encrypt_empty
        if enumerator is None:
            enumerator = codebook
        elif isinstance(enumerator, (list, tuple)):
            enumerator = pytis.data.FixedEnumerator(enumerator)
        elif __builtins__['type'](enumerator) == __builtins__['type'](Enumeration) \
                and issubclass(enumerator, Enumeration):
            e = enumerator
            enumerator = pytis.data.FixedEnumerator([v for v, l in e.enumeration])
            assert e.selection_type in public_attributes(SelectionType), e.selection_type
            if selection_type is None:
                selection_type = e.selection_type
            if orientation is None:
                orientation = e.orientation
            if default is None and e.default is not None:
                assert e.default in [v for v, l in e.enumeration], (e.default, e.enumeration,)
                default = e.default
            if display is None:
                labels = dict(e.enumeration)

                def display(value):
                    return labels.get(value, value)
            if prefer_display is None:
                assert isinstance(e.prefer_display, bool)
                prefer_display = e.prefer_display
            if null_display is None:
                null_display = e.null_display
        else:
            assert isinstance(enumerator, (basestring, pytis.data.DataFactory,
                                           pytis.data.Enumerator)), enumerator
        enumerator_kwargs = dict([(k, v) for k, v
                                  in dict(value_column=value_column,
                                          validity_column=validity_column,
                                          validity_condition=validity_condition).items()
                                  if v is not None])
        # assert not enumerator_kwargs or not isinstance(enumerator, pytis.data.Enumerator), \
        #     err("'enumerator' defined as Enumerator instance and '%s' passed.",
        #         enumerator_kwargs.keys()[0])
        links = xtuple(link)
        if __debug__:
            # Temporary: The following test replaces the commented out assertion above.  The
            # assertion would break older applications, so we just log for now.
            if enumerator_kwargs and isinstance(enumerator, pytis.data.Enumerator):
                log_("'enumerator' defined as instance and '%s' passed.",
                     enumerator_kwargs.keys()[0])
            for lnk in links:
                assert isinstance(lnk, Link), err("Invalid object in links: %r", lnk)
            for k in kwargs.keys():
                assert k in ('not_null', 'unique', 'constraints', 'minlen', 'maxlen',
                             'minimum', 'maximum', 'encrypted', 'precision', 'format',
                             'mindate', 'maxdate', 'utc', 'validation_messages',
                             'inner_type', 'minsize', 'maxsize', 'formats',
                             'strength', 'md5', 'verify', 'text',), \
                    err("Invalid argument: %r", k)
            # Temporary: We won't even log the old way of passing type
            # arguments in the first step.  We will uncomment it when we decide
            # to start modifying the applications.
            # if kwargs:
            #     log_("Passing data type arguments to Field is deprecated: %r",
            #           tuple(kwargs.keys()))
        self._id = id
        self._dbcolumn = dbcolumn or id
        if label is None:
            label = id  # TODO: Allow unlabeled fields?
        self._label = label
        if column_label is None:
            column_label = label
        self._column_label = column_label
        self._virtual = virtual
        self._type = type
        self._type_kwargs = kwargs
        self._enumerator = enumerator
        self._enumerator_kwargs = enumerator_kwargs
        self._descr = descr
        self._width = width
        if column_width is None and width != 0:
            column_width = width
        self._column_width = column_width
        self._fixed = fixed
        self._disable_column = disable_column
        self._compact = compact
        self._nocopy = nocopy
        self._default = default
        self._computer = computer
        self._height = height
        if isinstance(editable, Computer):
            # For backwards compatibility
            e_func = editable.function()
            if len(argument_names(e_func)) == 2:
                editable = Computer(lambda r: e_func(r, id), depends=editable.depends())
        self._editable = editable
        assert isinstance(visible, (bool, Computer))
        self._visible = visible
        self._formatter = formatter
        self._line_separator = line_separator
        self._codebook = codebook
        self._display = display
        self._prefer_display = prefer_display
        self._display_size = display_size
        self._null_display = null_display
        self._inline_display = inline_display
        self._inline_referer = inline_referer
        self._allow_codebook_insert = allow_codebook_insert
        self._codebook_insert_spec = codebook_insert_spec
        self._codebook_insert_prefill = codebook_insert_prefill
        self._codebook_update_prefill = codebook_update_prefill
        self._runtime_filter = runtime_filter
        self._runtime_arguments = runtime_arguments
        self._selection_type = selection_type
        if isinstance(completer, (list, tuple)):
            completer = pytis.data.FixedEnumerator(completer)
        self._completer = completer
        self._orientation = orientation or Orientation.VERTICAL
        self._post_process = post_process
        self._filter = filter
        self._filter_list = filter_list
        if isinstance(style, collections.Callable) and len(argument_names(style)) == 2:
            s_func = style

            # For backwards compatibility
            def style(row):
                return s_func(id, row)
        self._style = style
        self._links = links
        self._filename = filename
        self._filename_extensions = filename_extensions
        self._text_format = text_format
        self._attachment_storage = attachment_storage
        self._printable = printable
        self._slider = slider
        self._check = pytis.presentation.computer(check)
        self._crypto_name = crypto_name
        self._encrypt_empty = encrypt_empty

    def __str__(self):
        return "<Field for '%s'>" % self.id()

    def __unicode__(self):
        properties = self._kwargs
        formatted = []
        beg_names = ['id', 'label', 'type']
        end_names = ['descr']
        for name in (beg_names +
                     [k for k in properties.keys() if k not in beg_names + end_names] +
                     end_names):
            value = properties.get(name)
            if value is not None:
                if isinstance(value, basestring):
                    formatted_value = '"%s"' % (value,)
                elif isinstance(value, types.FunctionType):
                    formatted_value = '<function %s>' % (value.func_name,)
                else:
                    formatted_value = value
                info_string = u'%s=%s' % (name, formatted_value,)
                formatted.append(info_string)
        return u"<Field: %s>" % (string.join(formatted, ', '),)

    def clone(self, field):
        """Clone this field by another field and return the cloned instance.

        The cloned instance will inherit all attributes of this field and the
        other field passed as argument, where the attributes of the later field
        take precedence.

        """
        assert isinstance(field, Field), field
        kwargs = dict(self._kwargs, **field._kwargs)
        if 'type' in self._kwargs and 'type' in field._kwargs:
            t1 = self._kwargs['type']
            if type(t1) == type(pytis.data.Type):
                t1 = t1()
            t2 = field._kwargs['type']
            if type(t2) == type(pytis.data.Type):
                t2 = t2()
            kwargs['type'] = t1.clone(t2)
        new_field = Field(**kwargs)
        type_ = kwargs.get('type') or field.type()
        if type_ is not None and kwargs.get('codebook') is not None:
            new_field.set_type(type_)
        elif isinstance(type_, pytis.data.Array):
            # This hack adds field's enumerator to the Array inner type.
            enumerator = dict(self.type_kwargs(), **field.type_kwargs()).get('enumerator')
            inner_type = type_.inner_type()
            if enumerator and not inner_type.enumerator():
                inner_type = inner_type.clone(inner_type.__class__(enumerator=enumerator))
                new_field.set_type(type_.clone(type_.__class__(inner_type=inner_type)))
        return new_field

    def id(self):
        return self._id

    def dbcolumn(self):
        return self._dbcolumn

    def type(self):
        """Return the specified 'type' argument passed to the constructor.

        This method should not be used outside the 'pytis.presentation' module.  The actual field
        type instance is initialized during the data object and 'PresentedRow' construction and
        their instances should always be queried to obtain the actual field type instance.

        """
        return self._type

    def virtual(self):
        return self._virtual

    def label(self):
        return self._label

    def column_label(self):
        return self._column_label

    def descr(self):
        return self._descr

    def width(self, default=12):
        """Return the width specified in constructor or 'default' if specified width was None."""
        if self._width is None:
            return default
        else:
            return self._width

    def column_width(self, default=10):
        """Return the specified 'column_width', 'width' or 'default' whichever is not None."""
        if self._column_width is None:
            return self.width(default)
        else:
            return self._column_width

    def disable_column(self):
        return self._disable_column

    def fixed(self):
        return self._fixed

    def height(self, default=1):
        """Return the height specified in constructor or 'default' if specified height was None."""
        if self._height is None:
            return default
        else:
            return self._height

    def editable(self):
        return self._editable

    def visible(self):
        return self._visible

    def compact(self):
        return self._compact

    def nocopy(self):
        return self._nocopy

    def default(self):
        return self._default

    def computer(self):
        return self._computer

    def formatter(self):
        return self._formatter

    def line_separator(self):
        return self._line_separator

    def codebook(self):
        """Return the 'codebook' name passeed to the constructor as a string."""
        return self._codebook

    def display(self):
        return self._display

    def prefer_display(self):
        return self._prefer_display

    def display_size(self):
        return self._display_size

    def null_display(self):
        return self._null_display

    def inline_display(self):
        return self._inline_display

    def inline_referer(self):
        return self._inline_referer

    def allow_codebook_insert(self):
        return self._allow_codebook_insert

    def codebook_insert_spec(self):
        return self._codebook_insert_spec

    def codebook_insert_prefill(self):
        return self._codebook_insert_prefill

    def codebook_update_prefill(self):
        return self._codebook_update_prefill

    def runtime_filter(self):
        return self._runtime_filter

    def runtime_arguments(self):
        return self._runtime_arguments

    def selection_type(self):
        return self._selection_type

    def orientation(self):
        return self._orientation

    def post_process(self):
        return self._post_process

    def filter(self):
        return self._filter

    def filter_list(self):
        return self._filter_list

    def style(self):
        return self._style

    def links(self):
        return self._links

    def filename(self):
        return self._filename

    def filename_extensions(self):
        return self._filename_extensions

    def text_format(self):
        return self._text_format

    def attachment_storage(self):
        return self._attachment_storage

    def printable(self):
        return self._printable

    def slider(self):
        return self._slider

    def check(self):
        return self._check

    def crypto_name(self):
        return self._crypto_name

    def encrypt_empty(self):
        return self._encrypt_empty

    def completer(self):
        """Return field completer as a 'pytis.data.Enumerator' instance."""
        completer = self._completer
        if isinstance(completer, basestring):
            # Completer was defined as a specification name.
            import config
            data_spec = config.resolver.get(completer, 'data_spec')
            completer = pytis.data.DataEnumerator(data_spec, **self._enumerator_kwargs)
        return completer

    def type_kwargs(self):
        """Return the keyword arguments for field's data type construction.

        This method should never be called from outside of the 'pytis.presentation' module.

        """
        kwargs = dict(self._type_kwargs)
        enumerator = self._enumerator
        if enumerator is None:
            enumerator = self._codebook
        if isinstance(enumerator, (pytis.data.DataFactory, basestring,)):
            enumerator = pytis.data.DataEnumerator(enumerator, **self._enumerator_kwargs)
        if enumerator:
            kwargs['enumerator'] = enumerator
        if 'inner_type' in kwargs:
            # This is a hack for the deprecated special case when inner_type
            # of Array type is passed as a class.  It assumes all type arguments
            # are the inner type arguments.  The main reason is to add enumerator
            # to inner type kwargs.  There is another similar hack in Field.clone()
            # which works for the case that types are instances (not classes).
            inner_type = kwargs.pop('inner_type')
            if type(inner_type) == type(pytis.data.Type):
                kwargs = {'inner_type': inner_type(**kwargs)}
        return kwargs

    def set_type(self, type_):
        # Setting the field type is a gross hack necessary due to our
        # codebook handlings.
        args, kwargs = type_.init_args()
        kwargs = copy.copy(kwargs)
        kwargs.update(self.type_kwargs())
        self._type = self._kwargs['type'] = type_.__class__(*args, **kwargs)


class Fields(object):
    """Deprecated -- use `Specification._inherited_fields()' instead."""

    def __init__(self, fields):
        self._fields = tuple(fields)
        self._dict = dict([(f.id(), f) for f in fields])

    def __getitem__(self, key):
        return self._dict[key]

    def __len__(self, key):
        return len(self._fields)

    def keys(self):
        return self._dict.keys()

    def fields(self, override=(), exclude=()):
        override = dict([(f.id(), f) for f in override])
        return [override.get(f.id(), f) for f in self._fields if f.id() not in exclude]


class AttachmentStorage(object):
    """Abstract base class defining the API for storing file attachments.

    Attachments are external file resources used within LCG Structured text
    content (within fields with text_format='TextFormat.LCG').  If it is
    desired to let the user use attachments within a particular structured text
    field, an instance of an 'AttachmentStorage' subclass must be passed as
    'attachment_storage' argument to the 'Field' specification.

    This class only defines an abstract API.  Use subclasses defined here or
    define your own subclasses implementing this API.  Different subclasses
    will usually define theyr own constructor arguments, but otherwise they
    will obey the here defined interface.

    It is up to the application developer's decision to choose the appropriate
    level on which the attachments are shared.  There can be just one common
    storage for the whole application, separate storage for each view, separate
    for each record (the 'attachment_storage' argument of 'Field' can be a row
    function) or any other logic.  It just depends on how the instances are
    created (which constructor arguments they receive).

    All public methods transaction accept the argument 'transaction'.  The
    particular attachment storage implementation may choose to use or ignore
    this argument as approppriate (obviously, it makes sense only when they
    store something in the same database from which the transaction
    originates).  The caller should always pass the transaction, however,
    unless he surely knows which storage implementation is in use.  Of course,
    the transaction may only be passed in contexts where one exists.  If there
    is no transaction in progress, None is passed.

    """

    class StorageError(Exception):
        """Exception raised by storage methods when a storage access problem occurres."""
        pass

    class InvalidImageFormat(StorageError):
        """Exception raised by 'insert()' when image of unknown or invalid type is inserted."""
        pass

    def _resource(self, filename, title=None, descr=None, size=None,
                  has_thumbnail=False, thumbnail_size=None, info=None):
        """Return the corresponding resource instance for given filename and args.

        This is a helper method for simple creation of resource instances in
        derived classes.  This method is not designed to be overriden in
        derived classes but used by the implementation of methods 'resource()'
        and 'resources()'.  It implements the logic which should be common to
        all derived classes - autodetection of resource class by resource
        filename and automatic creation of thumbnail resource for image
        resources.

        The derived classes will typically override the methods
        '_resource_uri()', '_image_uri()', '_thumbnail_uri()',
        '_resource_src_file()', '_image_src_file()' and
        '_thumbnail_src_file()'.

        """
        import lcg
        cls = self._resource_cls(filename)
        is_image = issubclass(cls, lcg.Image)
        if is_image and has_thumbnail:
            uri = self._image_uri(filename)
            src_file = self._image_src_file(filename)
            kwargs = dict(size=size,
                          thumbnail=lcg.Image('thumbnails/' + filename, title=title, descr=descr,
                                              size=thumbnail_size,
                                              uri=self._thumbnail_uri(filename),
                                              src_file=self._thumbnail_src_file(filename)))
        else:
            uri = self._resource_uri(filename)
            src_file = self._resource_src_file(filename)
            if is_image:
                kwargs = dict(size=size)
            else:
                kwargs = {}
        return cls(filename, title=title, descr=descr, uri=uri, src_file=src_file, info=info,
                   **kwargs)

    def _resource_cls(self, filename):
        import lcg
        return lcg.Resource.subclass(filename)

    def _resource_uri(self, filename):
        """Return the URI of a resource of given 'filename'.

        Used by '_resource()' to get the value for 'uri' argument for non-image
        resources and image resources which don't have a thumbnail (are
        displayed full-size within the document).

        """
        pass

    def _image_uri(self, filename):
        """Return the URI of an image resource of given 'filename'.

        Used by '_resource()' to get the value for 'uri' argument for image
        resources which have a thumbnail (this URI should lead to an enlarged
        variant of the image (not necessarily the original)).

        """
        pass

    def _thumbnail_uri(self, filename):
        """Return the URI of an image thumbnail returned by '_resource()'.

        Used by '_resource()' to get the value for 'uri' argument for an image
        thumbnail.

        """
        pass

    def _resource_src_file(self, filename):
        """Return source file path of a resource of given 'filename' or None.

        Used by '_resource()' to get the value for 'src_file' argument for
        non-image resources.  May return None when the storage doesn't support
        source files.

        """
        return None

    def _image_src_file(self, filename):
        """Return image source file path of a resource of given 'filename' or None.

        Used by '_resource()' to get the value for 'src_file' argument for
        image resources which have a thumbnail (this path should point to an
        enlarged variant of the image (not necessarily the original)).  May
        return None when the storage doesn't support source files.

        """
        return None

    def _thumbnail_src_file(self, filename):
        """Return thumbnail source file path of a resource of given 'filename' or None.

        Used by '_resource()' to get the value for 'src_file' argument for
        image thumbnails.  May return None when the storage doesn't support
        source files.

        """
        return None

    def _resized_image(self, image, size):
        """Return the given PIL.Image instance resized to 'size' as a PIL.Image instance.

        Helper method to be used by derived classes.

        """
        import PIL.Image
        resized = image.copy()
        resized.thumbnail(size, PIL.Image.ANTIALIAS)
        return resized

    def insert(self, filename, data, values, transaction=None):
        """Insert a new attachment into the storage.

        Arguments:
          filename -- unique file name of the attachment as a basestring.
          data -- file-like object which may be read to retrieve the attachment
            data.  The calling side is responsible for closing the file after
            this method returns.
          values -- dictionary of values of attachment parameters to set.  The
            keys may be 'title', 'descr' (corresponding to 'resource.title()'
            and 'resource.descr()') or any additional application defined
            attachment parameters (usually passed through 'resource.info()').
          transaction -- current database transaction instance or None.  See
            the class docstring for more details.


        Raises 'InvalidImageFormat' exception if an image of unknown or invalid
          type is inserted.
        Raises 'IOError' if storing the file fails.

        """
        pass

    def update(self, filename, values, transaction=None):
        """Update the information about given attachment.

        Arguments:

          filename -- unique file name (basestring) identifying the resource.
            Must be one of existing filenames as returned by
            'resource.filename()' of one of the resources returned by
            'resources()'.
          values -- dictionary of values of attachment parameters to
            update.  The keys may be 'title', 'descr' (corresponding to
            'resource.title()' and 'resource.descr()') or any additional
            application defined attachment parameters (usually passed through
            'resource.info()').
          transaction -- current database transaction instance or None.  See
            the class docstring for more details.

        Returns None when the update is performed ok or an error message string
        when error occurres.

        """
        pass

    def delete(self, filename, transaction=None):
        """Not supported yet"""
        pass

    def retrieve(self, filename, transaction=None):
        """Retieve the contents of an attachment file of given name.

        Arguments:

          filename -- unique file name (basestring) identifying the resource.
            Must be one of existing filenames as returned by
            'resource.filename()' of one of the resources returned by
            'resources()'.
          transaction -- current database transaction instance or None.  See
            the class docstring for more details.

        The returned value is an open file like object with methods 'read()'
        and 'close()'.  The calling side is responsible for calling 'close()'
        after reading file data.

        None is returned when a corresponding attachment is not found.

        """
        pass

    def resource(self, filename, transaction=None):
        """Return a 'lcg.Resource' instance for given 'filename' if it exists or None.

        Arguments:

          filename -- unique file name (basestring) identifying the resource.
            Must be one of existing filenames as returned by
            'resource.filename()' of one of the resources returned by
            'resources()'.
          transaction -- current database transaction instance or None.  See
            the class docstring for more details.

        Image resources will automatically have a 'thumbnail' attribute if the
        user requested to display a smaller version of the image.  The storage
        may pass also additional information about the attachment in the 'info'
        argument of 'lcg.Resource' (available through the method
        'resource.info()').

        """
        pass

    def resources(self, transaction=None):
        """Return a list of all files currently present in the storage.

        Arguments:

          transaction -- current database transaction instance or None.  See
            the class docstring for more details.

        The returned list consists of 'lcg.Resource' instances corresponding to
        attachment files.  See 'resource()' for more information about the
        returned resource instances.

        """
        pass

    # Helper methods which don't need to be implemented in derived classes.

    def find_resource_by_uri(self, uri, transaction=None):
        """Find resource corresponding to given resource URI.

        Arguments:

          uri -- the searched resource URI as a basestring.
          transaction -- current database transaction instance or None.  See
            the class docstring for more details.

        Searches all the resources returned by 'resources()' including nested
        resources, such as thumbnails of Image resources.

        None is returned when a corresponding resource is not found.

        """
        import lcg
        for resource in self.resources(transaction=transaction):
            if resource.uri() == uri:
                return resource
            if isinstance(resource, lcg.Image):
                thumbnail = resource.thumbnail()
                if thumbnail and thumbnail.uri() == uri:
                    return thumbnail
        return None


class FileAttachmentStorage(AttachmentStorage):
    """Simple AttachmentStorage API implementation storing files in a filesystem.

    The constructor argument 'directory' determines the filesystem path where
    attachment files are stored.  This directory must either exist and be
    writable by the users who are expected to edit the field for which the
    storage is used or when it doesn't exist, it will be created automatically,
    but in this case one of its parent directories must exist and be writable.

    The files are store the directory in a certain arrangement.  Normal files
    have just one file per resource in given directory, but images may have a
    resized version in the 'resized' subdirectory and a thumbnail in
    'thumbnails' subdirectory.  In such case the image is represented by a
    resource pointing to the resized version of the image, which defines the
    'thumbnail' pointing to the thumbnailed version.

    Attachment files are often shared between a wx and web application.  Both
    applications must have access to this directory.  Wx app users editing the
    fields must have write access, wx app users displaying the field contents
    need read access and the web server user needs read access (editing
    attachments is currently not supported by web forms).  If both applications
    run on a different machine, the directory must be shared through a network
    filesystem.

    """

    def __init__(self, directory, base_uri=None):
        """Arguments:

          directory -- full path name of the directory where file attachments
            are stored.  See class description for more information.
          base_uri -- attachments base URI as a string or None.  If not None,
            the resource instances will have their uri attribute set.  The
            base_uri will be used as a common prefix.  Otherwise the resources
            have their uri unset.

        """
        assert isinstance(directory, basestring)
        assert base_uri is None or isinstance(base_uri, basestring)
        self._directory = directory
        if base_uri and not base_uri.endswith('/'):
            base_uri += '/'
        self._base_uri = base_uri

    def _resource_uri(self, filename):
        if self._base_uri:
            return self._base_uri + filename
        else:
            return None

    def _image_uri(self, filename):
        if self._base_uri:
            return self._base_uri + 'resized/' + filename
        else:
            return None

    def _thumbnail_uri(self, filename):
        if self._base_uri:
            return self._base_uri + 'thumbnails/' + filename
        else:
            return None

    def _resource_src_file(self, filename):
        return os.path.join(self._directory, filename)

    def _image_src_file(self, filename):
        return os.path.join(self._directory, 'resized', filename)

    def _thumbnail_src_file(self, filename):
        return os.path.join(self._directory, 'thumbnails', filename)

    def insert(self, filename, data, values, transaction=None):
        import lcg
        import PIL.Image
        path = self._resource_src_file(filename)
        try:
            if not os.path.exists(self._directory):
                os.makedirs(self._directory)
            if issubclass(self._resource_cls(filename), lcg.Image):
                try:
                    image = PIL.Image.open(data)
                except IOError as e:
                    raise self.InvalidImageFormat(e)
                image.save(path)
                if values.get('has_thumbnail', False):
                    try:
                        self._save_resized_image(filename, image, values)
                    except:
                        for subdir in ('', 'thumbnails', 'resized'):
                            path = os.path.join(self._directory, subdir, filename)
                            if os.path.exists(path):
                                os.remove(path)
                        raise
            else:
                f = open(path, 'wb')
                try:
                    f.write(data.read())
                finally:
                    f.close()
        except IOError as e:
            raise self.StorageError(str(e))

    def _resource_kwargs(self, filename):
        import lcg
        if issubclass(self._resource_cls(filename), lcg.Image):
            import PIL.Image
            img = PIL.Image.open(self._resource_src_file(filename))
            kwargs = dict(size=img.size)
            thumbnail_filename = self._thumbnail_src_file(filename)
            if os.path.isfile(thumbnail_filename):
                img = PIL.Image.open(thumbnail_filename)
                kwargs = dict(kwargs, has_thumbnail=True, thumbnail_size=img.size)
            return kwargs
        else:
            return {}

    def resource(self, filename, transaction=None):
        path = self._resource_src_file(filename)
        if os.path.isfile(path):
            return self._resource(filename, **self._resource_kwargs(filename))
        else:
            return None

    def resources(self, transaction=None):
        directory = self._directory
        if os.path.isdir(directory):
            return [self._resource(filename, **self._resource_kwargs(filename))
                    for filename in sorted([filename for filename in os.listdir(directory)
                                            if os.path.isfile(os.path.join(directory, filename))])]
        else:
            return []

    def retrieve(self, filename, transaction=None):
        path = os.path.join(self._directory, filename)
        if os.path.exists(path):
            return open(path)
        else:
            return None

    def _save_resized_image(self, filename, image, values):
        image_size = values.get('image_size', (1024, 800))
        thumbnail_size = values.get('thumbnail_size', (200, 200))
        for size, path in ((thumbnail_size, self._thumbnail_src_file(filename)),
                           (image_size, self._image_src_file(filename))):
            directory = os.path.split(path)[0]
            if not os.path.exists(directory):
                os.makedirs(directory)
            self._resized_image(image, size).save(path)

    def update(self, filename, values, transaction=None):
        if 'has_thumbnail' in values:
            if values['has_thumbnail']:
                import PIL.Image
                path = self._resource_src_file(filename)
                image = PIL.Image.open(path)
                self._save_resized_image(filename, image, values)
            else:
                for subdir in ('thumbnails', 'resized'):
                    path = os.path.join(self._directory, subdir, filename)
                    if os.path.exists(path):
                        os.remove(path)

class HttpAttachmentStorage(AttachmentStorage):
    """Remote AttachmentStorage implementation storing files through HTTP.

    The constructor argument 'uri' determines the HTTP uri of a server
    application implementing the attachment storage protocol.  One sample
    implementation of the server side as a Wiking module is the module
    'pytis.cms.web.HttpAttachmentStorageBackend'.

    The storage provides its own authorization mechanism.  When the instance is
    created, an access key is generated as a random string.  This string is
    saved in the database table 'e_pytis_http_attachment_storage_keys' for
    given database user and storage URI.  The HTTP requests generated by the
    storage then pass the username and access key to the server side via the
    HTTP headers 'X-Pytis-Attachment-Storage-Username' and
    'X-Pytis-Attachment-Storage-Key'.  The server side is expected to check
    that these headers match with the values saved in the DB table
    'e_pytis_http_attachment_storage_keys' (the server side must have access to
    the same database).  This should limit the access to the server only to
    currently active instances of 'HttpAttachmentStorage' and avoid
    unauthorized access.

    The value of 'readonly' argument passed to the constructor is also saved in
    the database table, which allows the server side to limit the access to
    read-only for particular client requests.

    """

    def __init__(self, uri, readonly=True):
        """Arguments:

          uri -- full URI of the attachment storage server application
            (including the initial 'http://' or 'https://' protocol
            specification).
          readonly -- True if the storage is read only, False if read/write
            access should be allowed by the server side.

        """
        import random
        import config
        assert isinstance(uri, basestring) \
            and (uri.startswith('http://') or uri.startswith('https://'))
        self._uri = uri.rstrip('/')
        self._username = username = config.dbconnection.user()
        self._key = key = ''.join(random.sample(string.digits + string.ascii_letters, 40))
        dbconnection = config.dbconnection.select(None)
        data = pytis.data.dbtable('e_pytis_http_attachment_storage_keys',
                                  ('key_id', 'username', 'uri', 'readonly', 'key'),
                                  dbconnection)
        row = pytis.data.Row((('username', pytis.data.sval(username)),
                              ('uri', pytis.data.sval(uri)),
                              ('readonly', pytis.data.bval(readonly)),
                              ('key', pytis.data.sval(key)),
                              ))
        transaction = pytis.data.DBTransactionDefault(dbconnection)
        try:
            # Try to delete the old row -- the table is unique on (username, uri).
            data.delete_many(pytis.data.AND(pytis.data.EQ('username', pytis.data.sval(username)),
                                            pytis.data.EQ('uri', pytis.data.sval(uri))),
                             transaction=transaction)
            data.insert(row, transaction=transaction)
        except:
            try:
                transaction.rollback()
            except:
                pass
            raise
        else:
            transaction.commit()

    def _connect(self, uri, body=None, headers={}):
        import urllib2
        headers['User-Agent'] = 'Pytis/%s (HttpAttachmentStorage)' % pytis.__version__
        headers['X-Pytis-Attachment-Storage-Username'] = self._username
        headers['X-Pytis-Attachment-Storage-Key'] = self._key
        if body is not None:
            headers['Content-Length'] = len(body)
        req = urllib2.Request(uri.encode('utf-8'), body, headers)
        try:
            return urllib2.urlopen(req)
        except (urllib2.HTTPError, urllib2.URLError) as e:
            raise self.StorageError(str(e))

    def _resource_uri(self, filename):
        return self._uri + '/' + filename

    def _image_uri(self, filename):
        return self._uri + '/resized/' + filename

    def _thumbnail_uri(self, filename):
        return self._uri + '/thumbnails/' + filename

    def _json_data(self, uri):
        import json
        response = self._connect(uri)
        try:
            response_text = response.read()
        finally:
            response.close()
        if not response.info().getheader('Content-Type') == 'application/json':
            log(OPERATIONAL, "Invalid server response:", response_text)
            # TODO: Ošetřit "Přístup odepřen"
            raise self.StorageError('Invalid server response')
        return json.loads(response_text)

    def _post_data(self, uri, data, headers=None):
        response = self._connect(uri, body=data, headers=(headers or {}))
        try:
            response_text = response.read()
        finally:
            response.close()
        if not response.info().getheader('Content-Type').startswith('text/plain'):
            log(OPERATIONAL, "Invalid server response:", response_text)
            raise self.StorageError('Invalid server response')
        if response_text != 'OK':
            raise self.StorageError(response_text)

    def _resource_kwargs(self, info):
        kwargs = dict(title=info.get('title'),
                      descr=info.get('descr'),
                      has_thumbnail=info.get('has_thumbnail', False))
        if 'thumbnail_size' in info:
            kwargs['thumbnail_size'] = info['thumbnail_size']
        if 'size' in info:
            kwargs['size'] = info['size']
        return kwargs

    def resource(self, filename, transaction=None):
        try:
            info = self._json_data(self._uri + '/' + filename + '?action=info')
        except self.StorageError:
            return None
        return self._resource(filename, **self._resource_kwargs(info))

    def resources(self, transaction=None):
        return [self._resource(filename, **self._resource_kwargs(info))
                for filename, info in self._json_data(self._uri)]

    def retrieve(self, filename, transaction=None):
        try:
            return self._connect(self._uri + '/' + filename + '?action=retrieve')
        except self.StorageError:
            return None

    def insert(self, filename, data, values, transaction=None):
        import mimetools
        import json
        boundary = mimetools.choose_boundary()
        body = ['--' + boundary,
                'MIME-Version: 1.0',
                'Content-Disposition: file; name="data"; filename="%s"' % filename.encode('utf-8'),
                'Content-Transfer-Encoding: base64',
                'Content-Type: application/octet-stream',
                '',
                data.read()]
        body.extend(['--' + boundary,
                     'Content-Disposition: form-data; name="values"', '', json.dumps(values)])
        body.extend(('--' + boundary + '--', ''))
        self._post_data(self._uri, '\r\n'.join(body),
                        headers={'Content-Type': 'multipart/form-data; boundary=%s' % boundary})

    def update(self, filename, values, transaction=None):
        import urllib
        import json
        data = urllib.urlencode(dict(action='update', values=json.dumps(values)))
        self._post_data(self._uri + '/' + filename, data)


class DbAttachmentStorage(AttachmentStorage):
    """AttachmentStorage implementation storing files in the database.

    Attachments are stored within a database table si binary data and some
    additional information about them.  The table must have at least the
    following columns:

       file_id serial primary key,
       file_name text not null,
       byte_size int not null,
       width int,
       height int,
       resized_width int,
       resized_height int,
       thumbnail_width int,
       thumbnail_height int,
       file bytea not null,
       resized bytea,
       thumbnail bytea

    The table will additionally include one foreign key column referencing to
    the entity, to which the attachments are related.  Typically, attachments
    are related to some "page", which is stored in the database.

    For example, when pages are stored in table 'pages', the additional column
    will look like:

       page_id int not null references pages(page_id) on delete cascade,

    and we will typically want to ensure that there are only unique file names
    for one page, so there will be a constraint:

       unique page_id, file_name


    """

    class HTTPHandler(BaseHTTPServer.BaseHTTPRequestHandler):
        """Simple HTTP server to serve attachments to the built-in pytis browser.

        It is only intended for serving attachments in pytis wx application.
        The client is the built-in webkit browser.

        """
        def do_GET(self):
            storage = self.server.storage_weakref()
            if storage:
                data = storage.get_data_for_uri(self.path)
                if data:
                    import mimetypes
                    content_type = mimetypes.guess_type(self.path)[0] or 'application/octet-stream'
                    self.send_response(200)
                    self.send_header('Content-type', content_type)
                    self.end_headers()
                    self.wfile.write(data)
                    return
            self.send_error(404, 'Not Found: %s' % self.path)

    def __init__(self, table, ref_column, ref_value, base_uri=None):
        """Arguments:

          table -- string name of the database table to use for storing the
            attachments.  The table schema must match the specification
            described in the class docstring.
          ref_column -- Reference column name (foreign key).
          ref_value -- Reference column value matching the reference column data type.
          base_uri -- Unused; Kept only for backwards compatibility.

        """
        import config
        self._data = pytis.data.dbtable(table,
                                        ('file_id', ref_column, 'file_name', 'byte_size',
                                         'width', 'height', 'resized_width', 'resized_height',
                                         'thumbnail_width', 'thumbnail_height',
                                         'file', 'resized', 'thumbnail'),
                                        config.dbconnection)
        self._ref_column = ref_column
        self._ref_value = ref_value
        self._ref_type = self._data.find_column(ref_column).type()
        self._server = server = BaseHTTPServer.HTTPServer(("localhost", 0), self.HTTPHandler)
        # Avoid circular reference to allow shutting down the server
        # automatically when the storage instance is deleted (see __del__).
        server.storage_weakref = weakref.ref(self)
        self._ticket = ticket = self._generate_ticket()
        self._base_uri = 'http://localhost:%d/%s' % (server.socket.getsockname()[1], ticket)
        import threading
        threading.Thread(target=server.serve_forever).start()

    def __del__(self):
        # Shut down the server when the storage instance is being removed from
        # memory.  The storage reference is passed to the created resource
        # instances to make the server live as long as its attachments are in
        # use.
        self._server.shutdown()
        self._server.socket.close()

    def _generate_ticket(self):
        try:
            ticket = ''.join(['%02x' % ord(c) for c in os.urandom(16)])
        except NotImplementedError:
            import random
            random.seed()
            ticket = ''.join(['%02x' % random.randint(0, 255) for i in range(16)])
        return ticket

    def _condition(self, filename=None):
        condition = pytis.data.EQ(self._ref_column,
                                  pytis.data.Value(self._ref_type, self._ref_value))
        if filename is not None:
            condition = pytis.data.AND(condition,
                                       pytis.data.EQ('file_name', pytis.data.sval(filename)))
        return condition

    def _make_row(self, values):
        rowdata = []
        for column, value in values.items():
            t = self._data.find_column(column).type()
            if isinstance(t, pytis.data.Binary) and value is not None:
                value = t.Buffer(value)
            rowdata.append((column, pytis.data.Value(t, value)))
        return pytis.data.Row(rowdata)

    def _get_row(self, filename, transaction=None):
        self._data.select(condition=self._condition(filename), transaction=transaction)
        row = self._data.fetchone()
        self._data.close()
        return row

    def _row_resource(self, row):
        if row['thumbnail_width'].value():
            thumbnail_size = (row['thumbnail_width'].value(), row['thumbnail_height'].value())
        else:
            thumbnail_size = None
        return self._resource(row['file_name'].value(),
                              info=dict(byte_size=row['byte_size'].value(),
                                        # Pass storage reference to make the storage
                                        # live as long as necessary (see __del__).
                                        storage=self),
                              size=(row['width'].value(), row['height'].value()),
                              has_thumbnail=thumbnail_size is not None,
                              thumbnail_size=thumbnail_size)

    def _resource_uri(self, filename):
        return self._base_uri + '/' + filename

    def _image_uri(self, filename):
        return self._base_uri + '/resized/' + filename

    def _thumbnail_uri(self, filename):
        return self._base_uri + '/thumbnails/' + filename

    def _image(self, filedata):
        import PIL.Image
        import cStringIO
        try:
            image = PIL.Image.open(cStringIO.StringIO(filedata))
        except IOError:
            image = None
        return image

    def _computed_row_values(self, image, has_thumbnail=False,
                             image_size=(1024, 800), thumbnail_size=(200, 200), **values):
        import cStringIO
        row_values = {}
        for size, column in ((image_size, 'resized'), (thumbnail_size, 'thumbnail')):
            if has_thumbnail:
                stream = cStringIO.StringIO()
                resized_image = self._resized_image(image, size)
                image.save(stream, image.format)
                buffer_value = buffer(stream.getvalue())
                width, height = resized_image.size
            else:
                buffer_value, width, height = None, None, None
            row_values[column] = buffer_value
            row_values[column + '_width'] = width
            row_values[column + '_height'] = height
        return row_values

    def resource(self, filename, transaction=None):
        row = self._get_row(filename, transaction=transaction)
        if row:
            return self._row_resource(row)
        else:
            return None

    def resources(self, transaction=None):
        resources = []
        columns = [c.id() for c in self._data.columns()
                   if not isinstance(c.type(), pytis.data.Binary)]
        self._data.select(condition=self._condition(), columns=columns, transaction=transaction)
        while True:
            row = self._data.fetchone()
            if row is None:
                break
            resources.append(self._row_resource(row))
        self._data.close()
        return resources

    def insert(self, filename, data, values, transaction=None):
        filedata = buffer(data.read())
        rowdata = {self._ref_column: self._ref_value,
                   'file_name': filename,
                   'file': filedata,
                   'byte_size': len(filedata)}
        image = self._image(filedata)
        if image:
            rowdata['width'] = image.size[0]
            rowdata['height'] = image.size[1]
            rowdata.update(self._computed_row_values(image, **values))
        try:
            self._data.insert(self._make_row(rowdata), transaction=transaction)
        except pytis.data.DBException as e:
            return str(e)
        else:
            return None

    def update(self, filename, values, transaction=None):
        row = self._get_row(filename, transaction=transaction)
        if row:
            image = self._image(row['file'].value().buffer())
            if image:
                rowdata = self._computed_row_values(image, **values)
                try:
                    self._data.update(row['file_id'], self._make_row(rowdata),
                                      transaction=transaction)
                except pytis.data.DBException as e:
                    return str(e)
            return None
        else:
            return "Attachment '%s' not found!" % filename

    def retrieve(self, filename, transaction=None):
        row = self._get_row(filename, transaction=transaction)
        if row:
            import cStringIO
            return cStringIO.StringIO(row['file'].value().buffer())
        else:
            return None

    def get_data_for_uri(self, uri):
        """Special method for the DbAttachmentStorage HTTP server communication.

        This method is not part of the AttachmentStorage API and should not be
        used by applications.  It is only intended for communicatuion with the
        built-in HTTP server serving attachments in pytis application (the
        client is the built-in webkit browser).

        """
        path = uri.lstrip('/').split('/')
        if path and path[0] == self._ticket:
            del path[0]
            if path and path[0] == 'thumbnails':
                field = 'thumbnail'
                del path[0]
            elif path and path[0] == 'resized':
                field = 'resized'
                del path[0]
            else:
                field = 'file'
            if len(path) == 1:
                row = self._get_row(path[0])
                if row:
                    return row[field].value().buffer()
        return None


class StatusField(object):
    """Status bar field specification.

    Defines properties of a status bar fields returned by
    'Application.status_fields'.

    """
    ICON_LEFT = 'ICON_LEFT'
    """Place icon on left edge of the status field (the text is displayed right of the icon)."""
    ICON_RIGHT = 'ICON_RIGHT'
    """Place icon on right edge of the status field (the text is displayed left of the icon)."""

    def __init__(self, id, label=None, refresh=None, refresh_interval=None, width=10,
                 icon_position=ICON_LEFT, on_click=None):
        """Arguments:

          id -- field identifier as a basestring.
          label -- field label as a basestring.  The label will be displayed
            as a tooltip.
          refresh -- callable object returning the current field state when
            called with no arguments.  The returned value may be either a
            basestring or a tuple.  If basestring is returned, it is used as
            the diplayed field value text.  If a tuple is returned it may be
            either a pair of (value, icon) or a tripple (value, icon, tooltip).
            Icon is the icon identifier as in 'pytis.form.get_icon()' and
            tooltip is a basestring to be displayed as the field's tooltip.
            Note that field label is displeyed in field tooltip by default, so
            you may want to add the label manually here to make the meaning of
            the field clear.  The tooltip may be also passed as a function in
            which case the function will be called without arguments only when
            the tooltip is needed and should return the tooltip string.  The
            function will be called periodically on user interface refresh
            events (on idle).  If None the field will not be updated
            periodically.  Such fields may be updated imperatively by calling
            'pytis.form.set_status()'.  The function should not perform any
            demanding processing and should return promptly.
          refresh_interval -- minimal delay between two successive calls of the
            'refresh' function in miliseconds.
          width -- field width as a number of characters.
          icon_position -- position to be used for placement of an icon
            if the field status is set to contain an icon.  It may be
            one of class constants 'ICON_LEFT' or 'ICON_RIGHT'.
          on_click -- function called with no arguments when user clicks the
            status bar field.

        """
        assert isinstance(id, basestring), id
        assert label is None or isinstance(label, basestring), label
        assert refresh is None or isinstance(refresh, collections.Callable), refresh
        assert refresh_interval is None or isinstance(refresh_interval, int), refresh_interval
        assert width is None or isinstance(width, int) and width > 0
        assert icon_position in (self.ICON_LEFT, self.ICON_RIGHT), icon_position
        assert on_click is None or isinstance(on_click, collections.Callable), on_click
        self._id = id
        self._label = label
        self._refresh = refresh
        self._refresh_interval = refresh_interval
        self._width = width
        self._icon_position = icon_position
        self._on_click = on_click

    def id(self):
        return self._id

    def label(self):
        return self._label

    def refresh(self):
        return self._refresh

    def refresh_interval(self):
        return self._refresh_interval

    def width(self):
        return self._width

    def icon_position(self):
        return self._icon_position

    def on_click(self):
        return self._on_click


class SpecificationBase(object):
    """Base class for specification classes which may be retrieved through resolver."""

    def __init__(self):
        # Needed here because of some magic used inside the resolver...
        pass


class Application(SpecificationBase):
    """Specification of a Pytis application.

    Derive this class within your application's specification modules under the
    name 'Application' and override its methods and attributes described below
    to customize the application's look and behaviour.

    Custom commands may be used in menu items using string identifiers.  For
    all such identifiers, the derived class must also define a method named as
    this identider with a prefix 'cmd_'.  This method, when called with no
    arguments, must return the specification of the command to invoke as a pair
    (COMMAND, ARGUMENTS), where the first item is a 'pytis.form.Command'
    instance and the later item is a dictionary of arguments to passed to the
    command when invoked.  For example when command identifier is 'my_form',
    the application must define a method named 'cmd_my_form'.

    """

    def init(self):
        """Run custom application initialization code before startup forms are opened.

        This code is run before any forms are opened.  Use 'post_init()' for
        initialization after all automatically started forms are opened.

        """
        pass

    def post_init(self):
        """Run custom application initialization code after startup forms are opened.

        This code is run after all startup forms are opened.  Use 'init()' for
        initialization before any automatically started forms are opened.

        """
        pass

    def login_hook(self, success):
        """Run custom code after login attempt.

        The boolean argument indicates login success (True when login was
        successful, False otherwise).

        The method may be called several times during application startup as
        long as the login attempts are unsuccessful (with one or none final
        successful call).

        """

    def menu(self):
        """Return the application's main menu as a sequence of 'pytis.form.Menu' instances."""
        return ()

    def keymap(self):
        """Return the sequence of custom keyboard shortcuts as (KEY, COMMAND) pairs.

        Se the docstring of 'pytis.form.Keymap.define_key()' for description of
        the items of the squence.  COMMAND may be a tuple (COMMAND, ARGS) when
        arguments need to be passed.

        """
        return ()

    def status_fields(self):
        """Return status bar fields as a sequence of 'StatusField' instances.

        The application's main window status bar will be created according to
        this specification.  By default, the result of
        'pytis.form.built_in_status_fields()' is returned.  Derived application
        can extend, reorder or redefine the fields as needed.

        """
        import pytis.form
        return pytis.form.built_in_status_fields()

class _SpecificationMetaclass(type):

    def __init__(cls, clsname, bases, clsdict):
        type.__init__(cls, clsname, bases, clsdict)
        cls._pytis_map_db_name_to_spec(clsdict)

    def _pytis_map_db_name_to_spec(cls, clsdict):
        db_table = clsdict.get('table')
        if db_table is None:
            return
        if ((not isinstance(db_table, type) or
             not issubclass(db_table, pytis.data.gensqlalchemy.SQLObject))):
            return
        Specification.add_specification_by_db_spec_name(db_table.__name__, cls)


class Specification(SpecificationBase):

    """Souhrnná specifikační třída sestavující specifikace automaticky.

    Tato třída zjednodušuje vytváření specifikací tím, že definuje vlastní
    pravidla pro sestavování jak prezentační tak datové specifikace pouze na
    základě jediné specifikace políček a některých dalších vlastností.

    Použití: Specifikaci vytvoříme odvozením specifikační třídy náhledu od této
    třídy a předefinováním jejich veřejných atributů.  To ulehčuje tvorbu
    variant náhledů s využitím dědičnosti.

    Význam atributů: Některé atrubuty jsou definovány přímo touto třídou --
    jejich význam je zdokumentován v rámci jejich dokumentačních řetězců.
    Všechny ostatní veřejné atributy, které odvozená třída definuje budou
    předány jako stejnojmenné argumenty konstruktoru 'ViewSpec'.  Výchozí
    hodnotou argumentu 'help' pro 'ViewSpec' je dokumentační řetězec
    specifikační třídy.

    Set 'public' attribute to True in specifications intended to serve for
    making forms or running procedures in the user interface.

    """
    public = False
    """Flag indicating whether the specification may be used in interactive calls.

    Specifications can be either public or private.  Public specifications can
    be used by forms and procedures run by the user.  Private specifications
    are only available to internal procedures.

    Each specification should explicitly set this attribute in order to make
    the status of the specification clear.  Set it to True iff the
    specification is public.

    Note: This attribute is currently ignored in web applications based on
    Wiking.

    """

    table = None
    """Database specification of the data table, view or function.

    It must be a subclass of 'pytis.sqlalchemy.SQLObject'.

    For backward compatibility the value of this attribute may also be a
    string, the name of the database object.  In such a case there is no
    database specification available and all the fields must be defined
    explicitly and database introspection is used to define some of their
    attributes."""

    key = None
    """Data object key column identifier as a string or their sequence.

    Sequence may be used if the data object has a multicolumn key.  In any case all named columns
    must exist in the 'fields' specification.  In 'key' is not defined, the first column form
    'fields' is used."""

    fields = ()
    """Field specification as a sequence of 'Field' instances.

    Default fields are created from the underlying database specification given
    in 'table' attribute.  You can specify here additional (virtual) fields and
    add or override attributes of fields made from database specifications.
    All the fields and attributes are merged together, with fields specified
    here taking precedence.

    May be also defined as a method of the same name.

    """

    arguments = None
    """Specification of all table arguments as a sequence of 'Field' instances.

    Useful only when the table is actually a row returning function, otherwise
    it must be 'None'.

    May be also defined as a method of the same name."""

    argument_provider = None
    """Function generating database table function arguments.

    It is a function of a single argument returning a dictionary of database
    function argument names (strings) as keys and 'pytis.data.Value' instances
    as corresponding database function argument values.  If it is 'None', no
    arguments are provided.  If it returns 'None', the select should be
    cancelled.  The function argument is the current dictionary of arguments,
    it is useful e.g. when updating previously supplied arguments.  This
    specification makes sense only for database table functions, it should be
    'None' for standard tables and views."""

    access_rights = None
    """Access rights for the view as an 'AccessRights' instance.

    May be also defined as a method of the same name."""

    crypto_names = ()
    """Sequence of crypto names (strings) required by the table.

    Field crypto names are added to this sequence automatically.  Explicitly
    setting other crypto names here is useful when the underlying database
    table is actually a view or a function performing decryption (and possibly
    also encryption) transparently, assuming the passwords for given crypto
    names are already given.
    """

    condition = None
    """A hardcoded condition filtering data of the underlying data object.

    This condition is used permanently and the user is not able to switch it
    off or know that it exists.  It has the same effect as implementing the
    condition in the underlying data source.  The value is a
    'pytis.data.Operator' instance."""

    distinct_on = None
    """Sequence of column names to filter distinct rows of the underlying data object."""

    data_cls = pytis.data.DBDataDefault
    """Datová třída použitá pro vytvoření datového objektu."""

    connection = None
    """Name of the database connection to use.

    If None, the default database connection is used.  If not None, the value is a string
    identifier of connection options defined within 'dbconnections' configuration option.

    """

    bindings = {}
    """Specification of bindings for use in dual forms.

    A dictionary, where the key is the name of the side form specification and the value is a
    'BindingSpec' instance which determines how the current view interconnects with given side form
    in a dual form.

    May be also defined as a method of the same name."""

    cb = CodebookSpec()
    """'CodebookSpec' instance defining properties of the view when used as a codebook.

    May be also defined as a method of the same name."""

    sorting = None
    """Default sorting specification.

    May be also defined as a method of the same name."""

    prints = None
    """A sequence of print specifications as pairs (TITLE, NAME)."""

    folding = None
    """'FoldableForm.Folding' instance defining initial folding.

    'None' means use the standard folding.

    Obviously useful only in foldable forms.

    """

    ro_select = True
    """Iff true then database select operations are set read only if possible.

    This is useful to lessen burden on the database server and avoid various
    problems.  But it doesn't work when the underlying database object performs
    read-write operations, e.g. creates temporary tables for its operation; in
    such a case this property must be set to false.

    """

    __metaclass__ = _SpecificationMetaclass
    _specifications_by_db_spec_name = {}
    _access_rights = None

    class _Fields(list):
        """Wrapper around field list.

        It behaves as normal field list with additional methods for retrieving,
        setting, removing and modifying the fields in the list.

        """
        def get(self, id_):
            """Return field with id 'id_' from the field list.

            Arguments:

              id_ -- id of the field; basestring

            If no such field exists, raise 'KeyError'.

            """
            for f in self:
                if f.id() == id_:
                    return f
            raise KeyError(id_)
        def set(self, id_, field):
            """Replace field with id 'id_' by 'field' in the field list.

            Arguments:

              id_ -- id of the replaced field; basestring
              field -- new field to be put at the given place; 'Field' instance

            If no such field exists, raise 'KeyError'.

            """
            for i in range(len(self)):
                if self[i].id() == id_:
                    self[i] = field
                    return field
            raise KeyError(id_)
        def _modify(self, field_ids, **properties):
            for id_ in field_ids:
                property_field = Field(id_, **properties)
                self.set(id_, self.get(id_).clone(property_field))
        def modify(self, field_id, **properties):
            """Modify properties of the field identified by 'field_id'.

            Arguments:

              field_id -- id of the field to modify; basestring
              properties -- dictionary of property names (strings) as keys and
                their values as values

            If field_id is not found, raise 'KeyError'.

            """
            self._modify((field_id,), **properties)
        def modify_many(self, field_ids, **properties):
            """Modify properties of the fields identified by 'field_ids'.

            Arguments:

              field_ids -- sequence of ids (basestrings) of the fields to
                modify
              properties -- dictionary of property names (strings) as keys and
                their values as values

            If any of the field_ids is not found, raise 'KeyError'.

            """
            self._modify(field_ids, **properties)
        def modify_except(self, field_ids, **properties):
            """Modify properties of all fields except of those with 'field_ids'.

            Arguments:

              field_ids -- sequence of ids (basestrings) of the fields to
                exclude from modification
              properties -- dictionary of property names (strings) as keys and
                their values as values

            If you want to modify properties of all fields, use an empty
            sequence as 'field_ids' argument.

            """
            field_ids_to_modify = set([f.id() for f in self]) - set(field_ids)
            self._modify(field_ids_to_modify, **properties)
        def set_property(self, property_, **settings):
            """Set field 'property' of several fields to given values.

            Arguments:

              property_ -- name of the field property to set; basestring
              settings -- dictionary with field ids (basestrings) as keys and
                property values as values

            If any of the field_ids is not found, raise 'KeyError'.

            """
            for id_, value in settings.items():
                self._modify((id_,), **{property_: value})
        def exclude(self, field_ids):
            """Remove fields with ids in 'field_ids' from the list.

            Arguments:

              field_ids -- sequence of field ids (basestrings) identifying the
                fields to remove

            If any of the field_ids is not found, raise 'KeyError'.

            """
            fields_to_remove = [self.get(id_) for id_ in field_ids]
            for f in fields_to_remove:
                self.remove(f)
        def __add__(self, other):
            return self.__class__(list(self) + list(other))

    @staticmethod
    def _init_access_rights(connection_data):
        """Read access rights of data specifications from the database.

        Arguments:

          connection_data -- 'pytis.data.DBConnection' instance

        This is actually a public method, but pytis doesn't allow its name to
        start with something else than underscore.

        """
        # Read in and check roles
        try:
            roles_data = pytis.data.dbtable('ev_pytis_user_roles', ('roleid',), connection_data)
            roles = roles_data.select()
            roles_data.close()
        except pytis.data.DBException:
            return
        if roles == 0:
            Specification._access_rights = 'nonuser'
            return
        access_rights = {}
        # Assign computed user rights
        S = pytis.data.String()
        rights_data = pytis.data.dbtable('pytis_view_user_rights',
                                         (('shortname', S,), ('rights', S,), ('columns', S,),),
                                         connection_data, arguments=())

        def process(row):
            shortname, rights_string, columns_string = \
                row[0].value(), row[1].value(), row[2].value()
            if columns_string:
                columns = string.split(columns_string, ' ')
            else:
                # This trick sets default permissions without adding them to
                # column specific permissions in AccessRights
                columns = ['__pytis_magic_column']
            shortname_rights = access_rights.get(shortname)
            if shortname_rights is None:
                shortname_rights = access_rights[shortname] = {}
            if rights_string:
                rights = [r.upper() for r in rights_string.split(' ') if r != 'show']
                if '*' in rights:
                    rights.remove('*')
                    for r in pytis.data.Permission.all_permissions():
                        if r not in rights:
                            rights.append(r)
            else:
                rights = []
            for c in columns:
                if c == '__pytis_magic_column':
                    shortname_rights[False] = rights
                else:
                    shortname_rights[c] = rights
        rights_data.select_map(process)

        # Transform access rights specifications to AccessRights instances
        def process(column, permissions):
            return (column, (None,) + tuple(permissions),)
        for shortname, rights in access_rights.items():
            access_rights_spec = [process(column, permissions)
                                  for column, permissions in rights.items()]
            access_rights[shortname] = pytis.data.AccessRights(*access_rights_spec)
        # Forbid actions without any rights for the current user
        actions_data = pytis.data.dbtable('e_pytis_action_rights',
                                          ('shortname', 'system', 'status',),
                                          connection_data)
        condition = pytis.data.AND(pytis.data.EQ('system',
                                                 pytis.data.Value(pytis.data.Boolean(), True)),
                                   pytis.data.LE('status',
                                                 pytis.data.Value(pytis.data.Integer(), 0)))
        for value in actions_data.distinct('shortname', condition=condition):
            shortname = value.value()
            if shortname not in access_rights:
                access_rights[shortname] = pytis.data.AccessRights()
        # That's all
        Specification._access_rights = access_rights

    def __init__(self, resolver=None):
        # TODO: the `resolver' argument is not normally passed now.  It is here
        # just for backwards compatibility for specifications which override
        # the constructor for some reason (mostly because they define
        # additional kwargs).  Once the resolver argument is removed from
        # applications, it should be removed from here too.
        for attr in ('fields', 'arguments', 'crypto_names', 'access_rights', 'condition',
                     'distinct_on', 'bindings', 'cb', 'check', 'sorting', 'profiles', 'filters',
                     'folding', 'initial_folding', 'query_fields', 'ro_select',):
            if hasattr(self, attr):
                value = getattr(self, attr)
                if isinstance(value, collections.Callable) and len(argument_names(value)) == 0:
                    setattr(self, attr, value())
        assert isinstance(self.fields, (list, tuple,)), self.fields
        assert self.arguments is None or isinstance(self.arguments, (list, tuple))
        self._view_spec_kwargs = {'help': self.__class__.__doc__}
        for attr in dir(self):
            if ((not attr.startswith('_') and not attr.endswith('_spec') and
                 attr not in ('table', 'key', 'connection', 'access_rights', 'condition',
                              'distinct_on', 'data_cls', 'bindings', 'cb', 'prints',
                              'data_access_rights', 'crypto_names',
                              'add_specification_by_db_spec_name', 'create_from_kwargs',
                              'ro_select', 'oid',  # for backwards compatibility
                              ))):
                self._view_spec_kwargs[attr] = getattr(self, attr)
        if isinstance(self.bindings, (tuple, list)):
            # Only pass new style bindings to ViewSpec, old style bindings are accessed through the
            # 'binding_spec' resolver function.
            self._view_spec_kwargs['bindings'] = self.bindings
        else:
            assert isinstance(self.bindings, dict)
        table = self.table
        fields = self.fields
        if isinstance(fields, collections.Callable):
            fields = fields()
        if table is not None and not isinstance(table, basestring):
            fields = self._init_from_db_fields(table, fields)
            if issubclass(table, pytis.data.gensqlalchemy.SQLFunctional):
                self.arguments = [Field(a.id(), type=a.type())
                                  for a in table.arguments if not a.out()]
        if not isinstance(fields, self._Fields):
            fields = self._Fields(fields)
        self._customize_fields(fields)
        for field in fields:
            # We need to init virtual fields after _customize_fields() because
            # additional fields may be appended there.
            if field.virtual():
                kwargs = field.type_kwargs()
                if kwargs:
                    ftype = field.type()
                    if ftype is not None:
                        if type(ftype) == type(pytis.data.Type):
                            if issubclass(ftype, pytis.data.Array):
                                ftype = ftype(inner_type=kwargs['inner_type'])
                            else:
                                ftype = ftype()
                        field.set_type(ftype.clone(ftype.__class__(**kwargs)))
        self._view_spec_kwargs['fields'] = fields
        self._fields = fields
        for arg in ('layout', 'list_layout', 'actions', 'columns', 'grouping'):
            try:
                value = self._view_spec_kwargs[arg]
            except:
                continue
            if isinstance(value, collections.Callable):
                self._view_spec_kwargs[arg] = value()
        # if self.__class__.__doc__:
        #     parts = re.split('\n\s*\n', self.__class__.__doc__, maxsplit=2)
        #     if 'description' not in self._view_spec_kwargs:
        #        self._view_spec_kwargs['description'] = parts[0]
        #     if 'help' not in self._view_spec_kwargs and len(parts) > 1:
        #        self._view_spec_kwargs['help'] = parts[1]

    def _init_from_db_fields(self, table, fields):
        xfields = []
        xfields_map = {}
        self_spec_name = self._spec_name()
        i = 0
        for c in table.specification_fields():
            descr = None
            if c.label():
                descr = c.doc()
            editable = Editable.ALWAYS
            type_ = c.type()
            if isinstance(type_, pytis.data.Serial):
                editable = Editable.NEVER
            codebook = None
            ref = c.references()
            if ref is not None:
                if isinstance(ref, pytis.data.gensqlalchemy.Arguments):
                    args = ref.args()
                    assert len(args) > 0, ("Invalid reference", c,)
                    ref = args[0]
                try:
                    ref = ref.id  # make Reference from ColumnLookup if needed
                except:
                    pass
                db_spec_name = ref.specification_name()
                codebook = self._codebook_by_db_spec_name(db_spec_name)
                if codebook == self_spec_name:
                    # We ignore self references here to avoid infinite
                    # recursion in the resolver.
                    codebook = None
            default = c.default()
            if default is None and isinstance(type_, pytis.data.Serial):
                if isinstance(table, pytis.data.gensqlalchemy.SQLTable):
                    default = nextval('%s_%s_seq' % (table.pytis_name(real=True), c.id(),),
                                      connection_name=self.connection)
                else:
                    orig_c = c.original_column()
                    if orig_c is not None:
                        default = nextval('%s_%s_seq' % (orig_c.table.name, orig_c.name,),
                                          connection_name=self.connection)
            f = Field(c.id(), c.label(), type=type_, descr=descr, default=default,
                      editable=editable, codebook=codebook,  # not_null=type_.not_null(),
                      crypto_name=c.crypto_name())
            xfields.append(f)
            xfields_map[c.id()] = i
            i += 1
        for f in fields:
            n = xfields_map.get(f.dbcolumn())
            if f.virtual():
                if n is not None:
                    raise Exception("Virtual field collides with a database column", f.id())
                xfields.append(f)
            else:
                if n is None:
                    raise Exception("Field not present in the database specification", f.id())
                db_field = xfields[n]
                if not issubclass(f.__class__, db_field.__class__):
                    raise Exception("Field type incompatible with database specification", f.id())
                xfields[n] = result_field = db_field.clone(f)
                if f.codebook() is not None:
                    result_field.set_type(result_field.type())
        if __debug__:
            for f in xfields:
                assert f.type() is not None, ("Field type not specified", self, f.id(),)
        return xfields

    def _action_spec_name(self):
        spec_name = self.__class__.__name__
        if self.__class__.__module__:
            spec_name = self.__class__.__module__ + '.' + spec_name
        spec_name = spec_name.replace('/', '.')
        return pytis.presentation.specification_path(spec_name)[1]

    def _create_data_spec(self):
        db_spec = None
        if issubclass(self.data_cls, pytis.data.DBData):
            B = pytis.data.DBColumnBinding
            table = self.table
            if table is None:
                table_name = camel_case_to_lower(self.__class__.__name__, '_')
            elif isinstance(table, basestring):
                table_name = table
            else:
                db_spec = table
                table_name = table.pytis_name(real=True)
            bindings = [B(f.id(), table_name, f.dbcolumn(), type_=f.type(),
                          crypto_name=f.crypto_name(), encrypt_empty=f.encrypt_empty(),
                          **f.type_kwargs())
                        for f in self._fields if not f.virtual()]
            bindings.extend([B(f.inline_display(), table_name, f.inline_display())
                             for f in self._fields
                             if (f.inline_display() and
                                 f.inline_display() not in [b.id() for b in bindings])])
            if self.key:
                keyid = self.key
                if isinstance(keyid, (list, tuple)):
                    assert len(keyid) == 1, ("Multicolumn keys no longer supported:", keyid)
                    keyid = keyid[0]
                key = find(keyid, bindings, key=lambda b: b.id())
                assert key is not None, ("Invalid key column:", keyid)
            else:
                key = bindings[0]
            args = (bindings, key,)
            if self.arguments is None:
                arguments = None
            else:
                arguments = [B(f.id(), table_name, f.dbcolumn(), type_=f.type(),
                               **f.type_kwargs())
                             for f in self.arguments]
        else:
            def type_(f):
                t = f.type() or pytis.data.String
                if type(t) == type(pytis.data.Type):
                    t = t()
                kwargs = f.type_kwargs()
                if kwargs:
                    t = t.clone(t.__class__(**kwargs))
                return t
            columns = [pytis.data.ColumnSpec(f.id(), type_(f))
                       for f in self._fields if not f.virtual()]
            args = (columns,)
            arguments = None
        access_rights = self.data_access_rights('form/' + self._action_spec_name())
        if access_rights is None:
            access_rights = self.access_rights
            if access_rights is None:
                perm = pytis.data.Permission.ALL
                access_rights = pytis.data.AccessRights((None, (None, perm)))
        kwargs = dict(access_rights=access_rights, connection_name=self.connection,
                      condition=self.condition, distinct_on=self.distinct_on,
                      arguments=arguments, crypto_names=self.crypto_names,
                      db_spec=db_spec, ro_select=self.ro_select)
        return pytis.data.DataFactory(self.data_cls, *args, **kwargs)

    def _create_view_spec(self, title=None, **kwargs):
        if not title:
            title = ' '.join(split_camel_case(self.__class__.__name__))
        return ViewSpec(title, **kwargs)

    def _db_fields(self):
        table = self.table
        if table is None or isinstance(table, basestring):
            return ()
        return self._init_from_db_fields(table, ())

    def _customize_fields(self, fields):
        """Customize fields defined by the parent class or db specification.

        This method may be used to manipulate the list of specification fields.
        The 'fields' argument is a 'Specification._Fields' instance.  Use the
        methods of this class to customize the fields.

        An alternative way of modifying inherited fields is the method
        '_customize_fields()'.  Use which ever makes more sense in your case.

        """
        pass

    def _inherited_fields(self, cls, override=(), exclude=()):
        """Helper method for simplification of field inheritance.

        Sample usage:

        def fields(self):
            return self._inherited_fields(SpecName,
                override=(
                    Field('x', editable=NEVER, default='09'),
                    Field('y', editable=NEVER),
                    Field('z', maxlen=8)),
                exclude=('a', 'b'))

        This will return a tuple of all fields defined by the parent class
        except for fields 'a' and 'b'.  Fields 'x', 'y' and 'z' will be
        modified by the attributes passed to the corresponding Field
        constructors, but their other attributes will remain as in the parent
        class.

        An alternative way of modifying inherited fields is the method
        '_customize_fields()'.  Use which ever makes more sense in your case.

        """
        fields = super(cls, self).fields
        if fields is None or fields == ():
            fields = self._db_fields()
        elif isinstance(fields, collections.Callable):
            fields = fields()
        if not isinstance(fields, self._Fields):
            fields = self._Fields(fields)
        for o in override:
            fid = o.id()
            fields.set(fid, fields.get(fid).clone(o))
        if exclude:
            fields.exclude(exclude)
        return tuple(fields)

    def _spec_fields(self, **kwargs):
        return self._inherited_fields(self.__class__, **kwargs)

    def view_spec(self):
        """Vrať prezentační specifikaci jako instanci 'ViewSpec'."""
        try:
            spec = self._view_spec
        except AttributeError:
            kwargs = self._view_spec_kwargs
            spec = self._view_spec = self._create_view_spec(spec_name=self._action_spec_name(),
                                                            **kwargs)
        return spec

    def data_spec(self):
        """Vrať datovou specifikaci jako instanci datové třídy."""
        try:
            spec = self._data_spec
        except AttributeError:
            spec = self._data_spec = self._create_data_spec()
        return spec

    def cb_spec(self):
        """Vrať specifikaci číselníku jako instanci 'CodebookSpec'."""
        return self.cb

    def binding_spec(self):
        """Vrať specifikaci navázání v duálním formuláři jako slovník."""
        return self.bindings

    def print_spec(self):
        """Vrať sekvenci specifikací tiskových náhledů."""
        prints = self.prints or ()
        if isinstance(prints, collections.Callable):
            prints = prints()
        return [p if isinstance(p, PrintAction) else
                PrintAction('__print_action_%d' % (i,), p[0], p[1])
                for i, p in enumerate(prints)]

    def access_spec(self):
        """Return the 'access_rights' attribute value.

        This is here to make access rights easily accessible by method call from resolvers.

        """
        return self.access_rights

    @classmethod
    def data_access_rights(class_, name):
        """Return form 'name' AccessRights read from the database.

        If no access rights (of any form) are stored in the database, return
        'None'.

        """
        if class_._access_rights is None:
            access_rights = None
        else:
            access_rights_spec = class_._access_rights
            if access_rights_spec == 'nonuser':
                access_rights = pytis.data.AccessRights()
            else:
                access_rights = access_rights_spec.get(name)
            if access_rights is None:
                perm = pytis.data.Permission.ALL
                access_rights = pytis.data.AccessRights((None, (None, perm)))
        return access_rights

    @classmethod
    def _spec_name(class_):
        full_name = class_.__module__ + '.' + class_.__name__
        return pytis.presentation.specification_path(full_name)[1]

    @classmethod
    def _codebook_by_db_spec_name(class_, db_spec_name):
        specification_names = class_._specifications_by_db_spec_name.get(db_spec_name)
        if not specification_names:
            return None
        elif len(specification_names) != 1:
            log(OPERATIONAL, "Multiple codebook candidates, no codebook assigned: ",
                (db_spec_name, specification_names,))
            return None
        else:
            return specification_names[0]

    @classmethod
    def add_specification_by_db_spec_name(class_, db_spec_name, specification):
        if specification.cb is not None:
            # Note, we can not store specification classes here
            # because they may become obsolete after reloading the
            # resolver.  We are only interested in specification
            # names, so we store just the names.
            mapping = class_._specifications_by_db_spec_name
            try:
                specification_names = mapping[db_spec_name]
            except:
                specification_names = mapping[db_spec_name] = []
            name = specification._spec_name()
            if name not in specification_names:
                specification_names.append(name)

    @classmethod
    def create_from_kwargs(class_, resolver, **kwargs):
        class Spec(class_):
            pass
        for key, value in kwargs.items():
            if key != 'data_cls' and isinstance(value, collections.Callable):
                # This is necessary to avoid calling functions (such as 'check'
                # or 'row_style') as methods.
                function = value
                if len(argument_names(function)) > 0:
                    # This is an ugly hack.  It is necessary to make the introspection
                    # in Specification.__init__ work.  It actually makes sure that the
                    # condition len(argument_names(value)) == 0 returns the same results
                    # for 'value' and for 'function'.
                    value = lambda self, x, *args, **kwargs: function(x, *args, **kwargs)
                else:
                    value = lambda self, *args, **kwargs: function(*args, **kwargs)
            setattr(Spec, key, value)
        return Spec(resolver)
