# -*- coding: iso-8859-2 -*-

# Copyright (C) 2001-2011 Brailcom, o.p.s.
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

"""Třídy pro specifikaci prezentační vlastností formulářů.

Třída 'ViewSpec' zastřešuje ostatní specifikační třídy definované tímto
modulem ('Field', 'GroupSpec', ...).

Vytvoření instance formulářové třídy je potom v podstatě interpretací
příslušných specifikací.

Všechny třídy tohoto modulu mají specifikační charakter a jejich instance jsou
považovány za immutable, tudíž mohou být libovolně sdíleny.

"""

import re

import pytis.data

from pytis.util import *
from pytis.presentation import *

class TextFormat(object):
    """Konstanty pro definici vstupního formátu textu."""
    PLAIN = 'PLAIN'
    HTML = 'HTML'
    WIKI = 'WIKI'


class BorderStyle(object):
    """Výčtová třída definující konstanty pro styl orámování."""
    ALL    = 'ALL'
    """Mezera je kolem dokola."""
    TOP    = 'TOP'
    """Mezera je jen nahoře."""
    BOTTOM = 'BOTTOM'
    """Mezera je jen dole."""
    LEFT   = 'LEFT'
    """Mezera je jen vpravo."""
    RIGHT  = 'RIGHT'
    """Mezera je jen vlevo."""

    
class Color(object):
    """Independent definition of generic named colors to be used within 'Style' specifications."""
    WHITE  = (255, 255, 255)
    BLACK  = (  0,   0,   0)
    RED    = (255,   0,   0)
    RED20  = (255, 200, 200)
    GREEN  = (  0, 255,   0)
    BLUE   = (  0,   0, 255)
    YELLOW = (255, 255, 160)
    GRAY   = ( 50,  50,  50)
    GRAY90 = ( 25,  25,  25)
    GRAY80 = ( 50,  50,  50)
    GRAY70 = ( 75,  75,  75)
    GRAY60 = (100, 100, 100)
    GRAY50 = (125, 125, 125)
    GRAY40 = (150, 150, 150)
    GRAY30 = (175, 175, 175)
    GRAY20 = (200, 200, 200)
    GRAY10 = (225, 225, 225)
    BLANCHETALMOND = (255, 235, 205)
    LIGHTYELLOW    = (255, 255, 224)
    PEACHPUFF2     = (238, 203, 173)
    SLATEGRAY2     = (185, 211, 238)
    LIGHTSALMON    = (255, 160, 122)

    
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

          foreground -- background color as one of 'Color' constants, a tuple of three integers
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
        items = [k[1:]+'='+repr(v) for k,v in self.__dict__.items()
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
        assert isinstance(text, (str, unicode)), text
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
            current state of the form.  If None, 'action' must be sepecified.

          enabled -- function of one argument -- the 'PresentedRow' instance representing the
            current state of the form -- which returns True if the button is enabled or False
            otherwise.  This function will be called periodically on each user interface update, so
            the result may change during form editation.  Only relevant if 'handler' is used.  For
            'action' the button state depends on the refered action and its 'enabled' parameter.

          action -- name of an 'Action' specification as a string.  This allows to handle the
            button press by invoking one of 'actions' defined in the same 'ViewSpec' instead of
            passing the 'handler' function directly.  If used, 'handler' must be None, if None,
            'handler' must be specified.
            
          width -- button with (number of characters).  The default width is set automatically to
            fit the label, but this argument may override this default.
            
          tooltip -- button's tooltip text as a string.
            
          active_in_popup_form -- False value deactivate the button in popup (modal) forms.  This
            may be particularly usefull when the button opens a new form, which is impossible in
            modal forms.
            
          active_in_readonly_form -- buttons are inactive in read-only forms by default, since they
            often modify form data.  True value will activate the button in even for readonly
            forms.  This may be particularly usefull when the button performs some action, which
            doesnt't modify the data.
            
        """
        if action is None:
            assert isinstance(label, (str, unicode)), label
            assert callable(handler), handler
            assert enabled is None or callable(enabled), enabled
        else:
            assert isinstance(action, str), action
            assert label is None or isinstance(label, (str, unicode)), label
            assert handler is None, handler
            assert enabled is None, enabled
        assert width is None or isinstance(width, int)
        assert tooltip is None or isinstance(tooltip, (str, unicode)), tooltip
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
    over the 'PresentedRow' instances as its first positional argument.

    """
    GLOBAL = 'GLOBAL'
    """Action is global for the view it belongs to.

    Global actions don't operate on particular records, they either don't
    operate on records at all or they operate on all records or they implement
    their own method of selecting the record(s) for the operation (e.g. pop-up
    a dialog).  The handler wil receive no positional argument.

    """
    # TODO: Zde by ještě mohla být jedna hodnota, která by umožnila definovat
    # univerzální akce, které pracují implicitně s aktuálním řádkem, ale pokud
    # existuje výběr, tak s výběrem.
    

class _ActionItem(object):
    
    def __init__(self, title):
        assert isinstance(title, (str, unicode))
        self._title = title

    def title(self, raw=False):
        """Vrať název akce."""
        title = self._title
        if not raw:
            title = title.replace("&", "")
        return title
            
    
class Action(_ActionItem):
    """User action specification.

    Actions typically appear in the user interface as action buttons or menu items and instances of
    this class define their properties, such as label, identifier, description, context in which
    they appear etc.  Instances of this class are used to define the 'actions' argument of
    'ViewSpec'.

    """
    def __init__(self, id, title, handler=None, context=ActionContext.RECORD,
                 secondary_context=None, enabled=True, visible=True, access_groups=None,
                 descr=None, hotkey=None, kwargs=None, **kwargs_):
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
        
        """
        assert descr is None or isinstance(descr, (str, unicode)), descr
        assert handler is None or callable(handler), handler
        assert context in public_attributes(ActionContext), context
        assert secondary_context is None or secondary_context in public_attributes(ActionContext), \
            secondary_context
        assert callable(enabled) or isinstance(enabled, bool), enabled
        assert callable(visible) or isinstance(visible, bool), visible
        assert access_groups is None or isinstance(access_groups, (str, tuple, list))
        assert hotkey is None or isinstance(hotkey, (str, tuple)), hotkey
        assert kwargs is None or isinstance(kwargs, dict) and not kwargs_, kwargs_
        self._handler = handler
        self._context = context
        self._secondary_context = secondary_context
        self._id = id
        self._enabled = enabled
        self._visible = visible
        self._access_groups = access_groups
        self._descr = descr
        self._hotkey = hotkey
        self._kwargs = kwargs or kwargs_
        super(Action, self).__init__(title)

    def id(self):
        return self._id

    def name(self):
        """Deprecated: Use id() instead."""
        return self.id()
    
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
    
    def kwargs(self):
        return self._kwargs
    

class ActionGroup(_ActionItem):
    """Definice pojmenované logické skupiny akcí.

    Skupiny akcí slouží k logickému seskupení souvisejících akcí.  V
    uživatelském rozhraní se takto definované akce například zobrazí jako
    samostatné podmenu v menu akcí.

    """
    def __init__(self, title, *actions):
        """Inicializuj instanci.

        Argumenty:
        
          title -- název skupiny jako řetězec

          actions -- obsah této skupiny.  Zde platí rekurzívně stejná pravidla
            jako pro stejnojmennný argument konstruktoru ViesSpec.

        """
        assert isinstance(actions, (list, tuple))
        if __debug__:
            for x in actions:
                if isinstance(x, (tuple, list)):
                    for y in x:
                        assert isinstance(y, (Action, ActionGroup))
                else:
                    assert isinstance(x, (Action, ActionGroup))
        self._actions = actions
        super(ActionGroup, self).__init__(title)
        
    def actions(self):
        """Vrať seznam akcí jako tuple."""
        return self._actions

    
class Profile(object):
    """Predefined form profile specification.

    Form profiles are a set of predefined form parameters, such as filtering
    condition, sorting, visible columns and their order, grouping, etc. (see
    constructor arguemnts for a full list).

    The user interface allows simple switching between available profiles and
    may also implement the option of saving user defined profiles.  Thus
    instances of this class may originate either from specification or from
    user data.

    """
    
    def __init__(self, id, name, condition=None, sorting=None, columns=None):
        """Arguments:
        
          id -- profile identifier as a string.  It must be unique among all
            profile identifiers within a given form.
          name -- user visible profile name as a string.
          condition -- filtering condition as a 'pytis.data.Operator' instance.
            This condition is always applied together (in conjunction) with the
            forms default 'condition' given by it's specification (if not None).
          sorting -- sorting in the same format as accepted by the 'sort'
            argument of 'pytis.data.Data.select()'.  If None, the default
            sorting given by the specification applies.
          columns -- sequence of visible form columns (string field
            identifiers) in the order in which they appear in the table.  If
            None, the columns defined by the specification are displayed.
          
        """
        assert isinstance(id, basestring)
        assert isinstance(name, basestring), name
        assert condition is None or isinstance(condition, pytis.data.Operator), condition
        assert sorting is None or isinstance(sorting, tuple), sorting
        self._id = id
        self._name = name
        self._condition = condition
        self._sorting = sorting
    
    def id(self):
        """Return the unique profile identifier."""
        return self._id
        
    def name(self):
        """Return the name passed to the constructor."""
        return self._name

    def condition(self):
        """Return the condition passed to the constructor."""
        return self._condition
    
    def sorting(self):
        """Return the sorting specification passed to the constructor."""
        return self._sorting

# For backwards compatibility
Filter = Profile
"""Deprecated: Use 'Profile' instead."""
Condition = Profile
"""Deprecated: Use 'Profile' instead."""

class FilterSet(list):
    """Uniquely identified set of filters.

    'FilterSet', unlike a plain list, adds some properties to a set of 'Filter'
    instances.  This is currently used only in Web applications.

    You can work with 'FilterSet' the same way as with lists and additionaly
    you can examine the special 'FilterSet' properties (e.g. its unique
    identifier).

    """
    _ID_MATCHER = re.compile('[a-z0-9_]+')
    
    def __init__(self, id, title, filters, default=None):
        """Arguments:
        
          id -- filter set identifier as a non-empty string.  It must be unique
            among all objects identifiers within a given form and it may
            contain only lower-case English alphabet letters, digits and
            underscores.
          title -- label of the filter to be displayed to the user, basestring
          filters -- sequence of 'Filter' instances
          default -- identifier of the filter (from 'filters') to be selected by default
        
        """
        assert isinstance(id, str), id
        assert self._ID_MATCHER.match(id), id
        assert isinstance(title, basestring)
        assert is_sequence(filters), filters
        assert all([isinstance(f, Filter) for f in filters]), filters
        if __debug__:
            filter_identifiers = []
            for f in filters:
                assert f.id() not in filter_identifiers, \
                    "Duplicate filter id '%s' in filter set '%s'" % (f.id(), id)
                filter_identifiers.append(f.id())
            assert default is None or default in filter_identifiers, default
        super(FilterSet, self).__init__(filters)
        self._id = id
        self._title = title
        self._default = default

    def id(self):
        """Return identifier of the filter set, string."""
        return self._id

    def title(self):
        """Return title of the filter set, basestring."""
        return self._title

    def default(self):
        """Return identifier of the filter to be selected by default."""
        return self._default


class GroupSpec(object):
    """Specification of form field layout in a single record presentation.

    Pytis forms are always either column based (tables) or layout based (record
    view, record edit form).  The layout defined by this class applies to the
    second category and specifies grouping of fields of a single record in this
    kind of forms.

    The layout constitutes of a group of items, where each of the items one of
    the following:

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
                 gap=2, space=1, border=3, border_style=BorderStyle.ALL):
        """Arguments:

          items -- contents of the group as a sequence of layout items (see
            'GroupSpec' for details about possible types if items.
            
          orientation -- defines how the fields are composed together as one of
            'Orientation' class constants.  Vertical group has items above each
            other, horizontal has items side by side.

          label -- Group label as a (localizable) string displayed at the top
            of the group or None for unlabeled group.  Labeled groups are
            always framed.
            
          gap, space, border, border_style -- Depracated and unsupported by
            some form types (particularly by web forms).

        """
        assert is_sequence(items), items
        assert label is None or isinstance(label, (str, unicode))
        assert type(gap) == type(0)
        assert gap >= 0
        assert orientation in public_attributes(Orientation)
        assert border_style in public_attributes(BorderStyle)
        if __debug__:
            allowed_item_types = (GroupSpec, Button, Text, str, unicode)
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
                assert isinstance(item, allowed_item_types) or callable(item), item
        self._items = tuple(items)
        self._label = label
        self._orientation = orientation
        self._gap = gap
        self._space = space
        self._border = border
        self._border_style = border_style

    def items(self):
        """Return the group contents as a tuple."""
        return tuple(self._items)

    def order(self):
        """Return the identifiers of all fields in this group and all subgroups."""
        fields = []
        for item in self._items:
            if isinstance(item, GroupSpec):
                fields.extend(item.order())
            elif isinstance(item, (str, unicode)):
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

    def space(self):
        """Vrať šířku mezery mezi políčkem a jeho labelem v du."""
        return self._space

    def border(self):
        """Vrať šířku mezery kolem celé skupiny v du."""
        return self._border

    def border_style(self):
        """Vrať styl mezery kolem skupiny jako konstantu 'BorderStyle'."""
        return self._border_style


class FieldSet(GroupSpec):
    """Labeled field group with a frame for grouping of fields in form layout.

    This class is derived from 'GroupSpec' (it is actually just its
    specialization with more convenient constructor interface for this kind of
    layout) so its instances may be used as items of 'ViewSpec' 'layout'
    specification.
    
    """
    def __init__(self, label, items):
        super(FieldSet, self).__init__(items, label=label, orientation=Orientation.VERTICAL)
        

class ColumnLayout(GroupSpec):
    """Set of horizontally (side by side) composed field groups in form layout.
    
    This class is derived from 'GroupSpec' (it is actually just its
    specialization with more convenient constructor interface for this kind of
    layout) so its instances may be used as items of 'ViewSpec' 'layout'
    specification.
    
    """
    def __init__(self, *items):
        super(ColumnLayout, self).__init__(items, orientation=Orientation.HORIZONTAL)

    
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
        

class HGroup(GroupSpec):
    """Horizontální seskupení políček.

    Tato třída je pouze pohodlnějším rozhraním k třídě 'GroupSpec'.

    """
    def __init__(self, *items, **kwargs):
        super(HGroup, self).__init__(items, orientation=Orientation.HORIZONTAL, **kwargs)

        
class VGroup(GroupSpec):
    """Vertikální seskupení políček.

    Tato třída je pouze pohodlnějším rozhraním k třídě 'GroupSpec'.

    """
    def __init__(self, *items, **kwargs):
        super(VGroup, self).__init__(items, orientation=Orientation.VERTICAL, **kwargs)

        
class LHGroup(HGroup):
    """Horizontální seskupení políček s nadpisem a orámováním.

    Tato třída je pouze pohodlnějším rozhraním k třídě 'GroupSpec'.

    """
    def __init__(self, label, *items, **kwargs):
        kwargs['label'] = label
        super(LHGroup, self).__init__(*items, **kwargs)

        
class LVGroup(VGroup):
    """Vertikální seskupení políček s labelem a orámováním.

    Tato třída je pouze pohodlnějším rozhraním k třídě 'GroupSpec'.

    """
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
        assert caption is None or isinstance(caption, (str, unicode))
        assert isinstance(group, GroupSpec)
        assert order is None or is_sequence(order)
        self._caption = caption
        self._group = group
        if order is None:
            order = group.order()
        elif __debug__:
            found = find_fields(group)
            for id in order:
                assert is_string(id)
                assert id in found, \
                       (_("Invalid field id in 'order' specification:"), id)
            for id in found:
                assert id in order, \
                       (_("Field id missing in 'order' specification:"), id)
            assert len(found) == len(order), \
                   _("Duplicate field id in 'order' spcification.")
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


class ViewSpec(object):
    """Kompletující specifikace prezentačních vlastnoostí pro formuláře.

    Instance této třídy zná veškeré prezentační vlasnosti určité entity
    (tabulky z pohledu aplikace).  Třída definuje API pro přístup k těmto
    vlastnostem.  Toto API je využíváno formulářovými třídami.

    Každá instance této třídy definuje vlastnosti pro všechny způsoby
    zobrazení (editační formulář, editační seznam, apod.).

    Každý typ formuláře z potom využívá ze specifikace pouze tu část, která je
    pro něj relevantní.

    """

    def __init__(self, title, fields, **kwargs):
        """Inicializuj instanci.

        Argumenty:

          title -- the title of this view as a (unicode) string.  The title is
            used in browse form headings and in other contexts, where the
            entity is refered as whole (all the records).  Thus the title
            should mostly be in plural (for example 'Invoices').
            
          singular -- the title of a single item (one record) of the entity as
            a (unicode) string.  If None, 'title' is used in both contexts.
            
          layout -- single record form layout specification as a 'GroupSpec'
            instance.  It is also possible to pass a sequence of items, which
            are automatically wrapped into a newly created 'GroupSpec' instance
            with horizontal arrangement.  The items must be compatible with
            item types supported by 'GroupSpec'.  If None, the default layout
            will automatically contain all fields defined by 'fields'.
            'LayoutSpec' instance is also accepted for backwards compatibility,
            but its usage is deprecated.

          list_layout -- specification of list layout as a 'ListLayout' instance or None.
          
          columns -- specifikace sloupců tabulkového formuláře, sekvence
            indentifikátorů políček z 'fields'.  Pokud není určeno, bude
            výchozí seznam sloupců obsahovat všechna políčka z fields, která
            nemají 'column_width' nastaveno na nulu nebo 'disable_column' na
            True.
            
          actions -- specifikace dostupných uživatelských akcí jako sekvence
            instancí 'Action', vnořených sekvencí, nebo instancí 'ActionGroup'.
            V nejjednodušším případě jde o prostý seznam instancí 'Action'.
            Pokud chceme ovlivnit reprezentaci seznamu dostupných akcí v
            uživatelském rozhraní, je možné akce seskupit do vnořenách tuplů či
            listů.  Takto vytvořené skupiny akcí budou odděleny separátorem.
            Dále je možné vytvořit vnořenou pojmenovanou skupinu
            (reprezentovanou jako samostatné podmenu) použitím instance
            'ActionGroup'.  Prvky v rámci každé 'ActionGroup' lze dále
            seskupovat stejným způsobem.
                        
          sorting -- default sorting in the same format as accepted by the 'sort' argument of
            'pytis.data.Data.select()'.  If None, the records will be sorted by the key column.
            
          grouping -- default visual grouping of table rows.  The value is a column identifier or a
            sequence of column identifiers.  Grouping allows you to visually group table rows,
            which have the same value(s) in grouping columns(s).  This usually only makes sense
            when the table is sorted by these columns.  Grouping is typically represented by slight
            changes in the background color.  Rows with the same values in grouping columns always
            have the same background color.  This color changes whenever one of the values changes.

          group_heading -- group heading allows additional representation of `grouping' (see above)
            and thus is only relevant when grouping is on.  If a column identifier is specified,
            the value of this column will apeear as a separate table row whenever a new group
            starts.  An 'lcg.TranslatableText' instance can also be passed as an interpolation
            template.  In this case the group heading will be produced by interpolation of the
            template (with python string formatting syntax such as '%(field_id)s') by formatted
            field values of the first row of the group.  Most often, you will want to show group
            headings when the grouping columns are actually not shown in the table.  Group heading
            is currently only supported by web forms.  In this case the title will be produced by
            interpolation of formatted row values within given string (with python string
            formatting syntax).

          check -- funkce pro ověření integrity dat celého záznamu.  Jedná se o
            funkci jednoho argumentu, jímž je instance třídy `PresentedRow',
            reprezentující aktuální hodnoty všech políček formuláře.  Namísto
            jediné funkce lze předat také seznam takových funkcí -- v tom
            případě budou funkce volány v pořadí, ve kterém jsou uvedeny.  Na
            rozdíl od validace hodnot políček, která závisí na datovém typu a
            má k dispozici pouze vlastní obsah políčka, má tato funkce k
            dispozici i hodnoty ostatních políček, takže je vhodná pro ověření
            vzájemné slučitelnosti těchto hodnot.  Tato funkce vrací None,
            pokud je vše v pořádku a formulář může být v tomto stavu odeslán,
            nebo id políčka, jehož hodnota způsobila neplatnost záznamu.
            Formulář by potom měl uživatele vrátit do editace daného polčka.
            Je možné vrátit také dvojici (ID, MESSAGE), kde MESSAGE je chybová
            zpráva, která má být zobrazena uživateli.
            
          cleanup -- a function for final actions after inserting/updating a record.  The function
            must accept two arguments -- the first one is the row after performing the database
            operation (insert/update) and the second is the edited/inserted row before the database
            operation (row values may be changed by the operation -- default values may be supplied
            and/or triggers/rules may modify the data).  Both arguments are `PresentedRow'
            instances.  Note, that you can also access the values before any user changes throught
            the 'original_row()' method on the second argument.  The cleanup function is run after
            comitting the edit/insert form (using the ``Ok'' button) or after committing inline
            editation/insert regardless whether the record has been changed or not.  Note, that
            unlike 'check', 'cleanup' is called after the database operation, but you can still
            abort the operation by rollback of the transaction (if the underlying database engine
            supports it).
            
          on_new_record -- akce vložení nového záznamu.  Pokud je None, bude
            provedena výchozí akce (otevření PopupEditForm nad danou
            specifikací).  Předáním funkce lze předefinovat přidání nového
            záznamu v daném náhledu libovolnou vlastní funkcionalitou.  Funkce
            musí akceptovat klíčový argument 'prefill' (viz.
            'pytis.form.new_record()').
            
          on_edit_record -- akce editace záznamu.  Pokud je None, bude
            provedena výchozí akce (otevření PopupEditForm nad danou
            specifikací).  Předáním funkce jednoho klíčového argumentu,
            jímž je instance 'PresentedRow', lze předefinovat editaci záznamu
            libovolnou vlastní funkcionalitou.
            
          on_delete_record -- user defined record deletion function.  If defined, it must be a
            function of one argument (the current record as a PresentedRow instance) which will be
            called on users request to delete a record.  Further processing of the deletion request
            will depend on the result value of the function as follows: If the function returns
            True, the deletion will continue by the default action (user is asked for confirmation
            and record is deleted) as it the function was not defined.  If None or False is
            returned the deletion is aborted.  If a string or unicode is returned, the message is
            printed as an error message and the deletion is aborted.  If 1 is returned, the
            deletion is considered being already successfuly applied so only after-deletion actions
            (such as refresh) are performed.  Finally, if a 'pytis.data.Operator' instance is
            returned, all records matching given condition are deleted without further prompting.
            
          redirect -- redirection for single record view/editation specified as a callable object
            (function) of one argument - the 'PresentedRow' instance.  The function should return
            the name of the specification to use for given record.  If the function is not defined
            or it returns None, no redirection takes place.  Redirection may be useful when a form
            lists records of different types (displaying only the fields which are common for all
            the subtypes) and you want to be able to open each particular record in a form which
            matches its subtype.  Note, that the same effect would be possible by defining the
            'on_edit_record' function.
            
          focus_field -- identifier of the field, which should automatically gain focus when a form
            is open.  You may also pass a function of one argument ('PresentedRow' instance), which
            returns the field identifier.
            
          description -- brief description of the view.  A short text (one or two sentences)
            without formatting.  Use the 'help' argument below to supply a detailed description.
            But even if 'help' is present, this short description should still be defined.
          
          help -- detailed description of the view as a formatted text in the LCG Structured Text
            format.  This text is used for generating the on-line help and it is also possible to
            supply it in a separate file.  See the Help tutorial for more information.

          row_style -- a 'Style' instance determining the base style for all fields or a function
            of one argument (the 'PresentedRow' instance) returning the 'Style' for one row (based
            on its values).

          profiles -- a sequence of predefined form profiles ('Profile'
            instances) which the user can easilly switch from the user
            interface.

          default_profile -- a string identifier of the form profile, which
            should be automatically preselected in the user interface.  This
            must be an existing identifier of one of the profiles specified by
            'profiles'.

          filter_sets -- a sequence of filter sets as 'FilterSet' instances.
            Filter sets are only supported by web applications to present
            multiple filter selectors which can be combined into one final
            filtering condition.

          aggregations -- a sequence aggregation functions which should be turned on automatically
            for this view (in forms which support that).  The items are 'AGG_*' constants of
            'pytis.data.Data'.

          grouping_functions -- specification of available functions aplicable
            to group by columns in an aggregated view as a sequence of
            (function, label, input_type, return_type), where function is
            identifier of an SQL function, label is the string title of given
            function, input_type is pytis data type class of function argument
            (the function must have just one argument) and return_type is the
            pytis data type instance of the function result.  When a user
            attempts to open an aggregated view of the form, she can select the
            columns used in the group by caluse.
            
          bindings -- a sequence of binding specifications as 'Binding' instances.

          initial_folding -- 'FoldableForm.Folding' instance defining initial
            folding.  'None' means use the standard folding.
            
          arguments -- sequence of 'DBBinding' instances defining table
            arguments, when the table is actually a row returning function.
            Otherwise it must be 'None'.

          spec_name -- name of the original form specification if any, string.

          public -- boolean flag indicating a public specification.

        The arguments 'layout' and 'columns' may be omitted.  Default layout
        and column list will be generated automatically based on the order of
        the field specifications in 'fields'.
        
        """
        self._kwargs = dict(kwargs, title=title, fields=fields)
        self._init(**self._kwargs)
    
    def _init(self, title, fields, singular=None, layout=None, list_layout=None, columns=None,
              actions=(), sorting=None, grouping=None, group_heading=None, check=(),
              cleanup=None, on_new_record=None, on_edit_record=None, on_delete_record=None,
              redirect=None, focus_field=None, description=None, help=None, row_style=None,
              profiles=(), default_profile=None, filters=(), conditions=(), default_filter=None,
              filter_sets=(), aggregations=(), grouping_functions=(), bindings=(),
              initial_folding=None, spec_name='', arguments=None, public=None):
        assert isinstance(title, (str, unicode))
        if singular is None:
            if isinstance(layout, LayoutSpec):
                singular = layout.caption()
            else:
                singular = title
        else:
            assert isinstance(singular, (str, unicode))
        assert is_sequence(fields)
        self._field_dict = dict([(f.id(), f) for f in fields])
        self._fields = tuple(fields)
        self._spec_name = spec_name
        for action in self._linearize_actions(actions):
            rights = Specification.data_access_rights('action/%s/%s' % (action.id(), self._spec_name,))
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
            action_names = [action.name() for action in self._linearize_actions(actions)]
            def recourse_group(group):
                for item in group.items():
                    if isinstance(item, GroupSpec):
                        recourse_group(item)
                    elif isinstance(item, Button):
                        assert item.action() is None or item.action() in action_names, \
                               ("Unknown button action in layout of %s: %s" %
                                (spec_name, item.action(),))
                    elif isinstance(item, Text):
                        pass
                    elif callable(item):
                        pass
                    else:
                        assert self._field_dict.has_key(item), \
                               ("Unknown field in layout of %s: %r" % (spec_name, item,))
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
                            assert self._field_dict.has_key(dep), \
                                   _("Unknown field id '%s' in dependencies "
                                     "for '%s' specification of '%s'.") % \
                                     (dep, s, f.id())
        # Initialize `columns' specification parameter
        if columns is None:
            columns = tuple([f.id() for f in self._fields
                             if f.column_width() and not f.disable_column()])
        else:
            if __debug__:
                assert is_sequence(columns)
                for c in columns:
                    assert isinstance(c, str) and self._field_dict.has_key(c),\
                           (_("Unknown column id in 'columns' specification of %s: %r") %
                            (spec_name, c,))
                    f = self._field_dict[c]
                    assert not f.disable_column(), \
                           _("Disabled column in columns of %s: %s") % (spec_name, c,)
        # Initialize other specification parameters
        if sorting is not None:
            assert is_sequence(sorting)
            if __debug__:
                for id, dir in sorting:
                    assert self.field(id) is not None
                    assert dir in (pytis.data.ASCENDENT,
                                   pytis.data.DESCENDANT)
        if grouping is None:
            grouping = ()
            assert group_heading is None
        else:
            grouping = xtuple(grouping)
            assert group_heading is None or isinstance(group_heading, (unicode, str)), group_heading
            if __debug__:
                for id in grouping:
                    assert self.field(id) is not None, id
        assert callable(check) or isinstance(check, (list, tuple))
        check = xtuple(check)
        if __debug__:
            for f in check:
                assert callable(f)
        if conditions:
            # `conditions' are for backwards compatibility.
            assert not filters, "When using 'filters', 'conditions' can not be used."
            filters = conditions
        if filters:
            # `filters' are for backwards compatibility as well.
            if isinstance(filters[0], FilterSet):
                filter_sets = filters
            else:
                assert not profiles, \
                    "When using 'profiles', 'filters' and 'conditions' can not be used."
                profiles = filters
        if default_filter:
            assert not default_profile, "When using 'default_profile', 'default_filter' can not be used."
            default_profile = default_filter
        assert isinstance(profiles, (tuple, list))
        assert isinstance(filter_sets, (tuple, list))
        if __debug__:
            for fs in filter_sets:
                assert isinstance(fs, FilterSet)
            profile_identifiers = []
            for p in profiles:
                assert isinstance(p, Profile)
                assert p.id() not in profile_identifiers, \
                    "Duplicate profile id of %s: %s" % (spec_name, p.id())
                profile_identifiers.append(p.id())
            assert default_profile is None or default_profile in profile_identifiers, \
                "Default profile not found in profiles of %s: %s" % (spec_name, default_profile)
        assert isinstance(aggregations, (tuple, list))
        if __debug__:
            for agg in aggregations:
                assert agg in [getattr(pytis.data.Data, attr)
                               for attr in public_attributes(pytis.data.Data)
                               if attr.startswith('AGG_')]
        assert isinstance(grouping_functions, (tuple, list))
        assert isinstance(bindings, (tuple, list))
        if __debug__:
            binding_identifiers = []
            for b in bindings:
                assert isinstance(b, Binding), b
                if b.id() is not None:
                    assert b.id() not in binding_identifiers, \
                           "Duplicate binding id of %s: %s" % (spec_name, b.id(),)
                    binding_identifiers.append(b.id())
        assert cleanup is None or callable(cleanup)
        assert on_new_record is None or callable(on_new_record)
        assert on_edit_record is None or callable(on_edit_record)
        assert on_delete_record is None or callable(on_delete_record)
        assert redirect is None or callable(redirect)
        assert focus_field is None or callable(focus_field) or \
               isinstance(focus_field, (str, unicode))
        assert row_style is None or isinstance(row_style, Style) or callable(row_style)
        assert description is None or isinstance(description, (str, unicode))
        assert help is None or isinstance(help, (str, unicode))
        self._title = title
        self._singular = singular
        self._columns = columns
        self._layout = layout
        self._list_layout = list_layout
        self._actions = actions
        self._sorting = sorting
        self._grouping = grouping
        self._group_heading = group_heading
        self._check = check
        self._cleanup = cleanup
        self._on_new_record = on_new_record
        self._on_edit_record = on_edit_record
        self._on_delete_record = on_delete_record
        self._redirect = redirect
        self._focus_field = focus_field
        self._description = description
        self._help = help
        self._row_style = row_style
        self._profiles = tuple(profiles)
        self._default_profile = default_profile
        self._filter_sets = filter_sets
        self._aggregations = tuple(aggregations)
        self._grouping_functions = tuple(grouping_functions)
        self._bindings = tuple(bindings)
        self._initial_folding = initial_folding
        
    def _linearize_actions(self, spec):
        actions = []
        for x in spec:
            if isinstance(x, Action):
                actions.append(x)
            elif isinstance(x, ActionGroup):
                actions.extend(self._linearize_actions(x.actions()))
            elif isinstance(x, (tuple, list)):
                actions.extend(self._linearize_actions(x))
            else:
                raise ProgramError("Invalid action specification: %s" % x)
        return actions
    
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
        """Vrať tuple specifikací všech políček v layoutu."""
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

    def actions(self, linear=False):
        """Vrať specifikaci akcí."""
        if linear:
            return self._linearize_actions(self._actions)
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
        """Return predefined form profiles as a tuple of 'Profile' instances."""
        return self._profiles

    def default_profile(self):
        """Return the default profile identifier as a string."""
        return self._default_profile

    def filter_sets(self):
        """Return the filter sets as a tuple of 'FilterSet' instances."""
        return self._filter_sets

    def aggregations(self):
        """Return default aggregation functions as a tuple."""
        return self._aggregations

    def grouping_functions(self):
        """Return specification of available grouping functions as a tuple."""
        return self._grouping_functions

    def bindings(self):
        """Return bindings as a tuple."""
        return self._bindings

    def initial_folding(self):
        """Return initial folding as a 'FoldableForm.Folding' instance or 'None'."""
        return self._initial_folding

    
class BindingSpec(object):
    """Specifikace vazby dvou náhledů při propojení do duálního formuláře.

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
        assert title is None or isinstance(title, (str, unicode)), title
        assert binding_column is None or isinstance(binding_column, (str, unicode)), binding_column
        assert side_binding_column is None or isinstance(side_binding_column, (str, unicode))
        assert isinstance(hide_binding_column, bool), hide_binding_column
        if append_condition is not None:
            # Backwards compatibility
            assert condition is None, "Can't use 'append_condition' (deprecated) with 'condition'."
            condition = append_condition
        assert condition is None or callable(condition), condition
        assert condition is not None or binding_column is not None, \
               "You must specify at least one of 'condition', 'binding_column'."
        assert description is None or isinstance(description, (str, unicode)), description
        assert help is None or isinstance(help, (str, unicode)), help
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

    Experimental alternative to BindingSpec to be used with MultiBrowseDualForm.

    """
    def __init__(self, id, title, name, binding_column=None, condition=None, descr=None,
                 single=False, arguments=None, prefill=None):
        """Arguments:
          id -- identifier of the binding as a string.  It must be unique among
            all objects identifiers within a given form.
          title -- title used for the list of related records
          name -- name of the related specification
          binding_column -- the string identifier of the binding column.  The meaning depends on
            the type of the binding.  The binding can be 1:1 or 1:N and is determined by the
            'single' argument (below).  Binding column can be combined with a 'condition' to
            further restrict the relation or can be completely omitted and only a generic condition
            may be used to define the relation.  In this case, however, the application has no
            information about the type of the binding and may not be able to offer some features
            which require this knowledge.  This argument may be used as positional.
          condition -- a function of one argument (the 'PresentedRow' instance) returning the
            current binding condition (a 'pytis.data.Operator' instance) to filter the data of the
            dependent form for given main form row.  If used together with the binding column, the
            condition will be used in conjunction with the binding column condition.  If
            'binding_column' is None, this condition will be used solely.
          arguments -- function of a single argument (the 'PresentedRow'
            instance) returning a dictionary of table arguments.  This function
            may be provided only when the side form table is actually a row
            returning function.  Otherwise 'arguments' must be 'None'.
          descr -- binding description text (to be used in on-line help etc).
          single -- boolean flag indicating whether this binding corresponds to a 1:1 relation
            (True value) or 1:N relation (False value).  The value of this flag determines the
            meaning of the 'binding_column' argument.  For the 1:1 relation, there is exactly one
            record in the related view corresponding to one record of the main form.  The
            'binding_column' in this case is the identifier of a column in main form specification
            which must have a codebook specification pointing to the related view.  For the 1:N
            relation the binding column must exist in the related view and must have a codebook
            (foreign key) specification pointing to the main form (the view for which the binding
            is used).
          prefill -- function of one argument (the main form record) returning
            a dictionary of values to prefill in the side form's new record.
            The dictionary keys are field identifiers and values are internal
            (python) values.  If 'prefill' is None and 'binding_column' is
            specified, the default prefill is automatically generated using the
            binding column value.
          
        """
        assert isinstance(name, basestring), name
        assert isinstance(title, basestring), title
        assert binding_column is None or isinstance(binding_column, (str, unicode)), binding_column
        assert condition is None or callable(condition), condition
        assert condition is not None or binding_column is not None or arguments is not None, \
               "At least one of 'binding_column', 'condition', `arguments' must be used."
        assert isinstance(single, bool), single
        assert isinstance(id, basestring), id
        assert arguments is None or callable(arguments), arguments
        assert prefill is None or callable(prefill), prefill
        self._name = name
        self._title = title
        self._binding_column = binding_column
        self._condition = condition
        self._id = id
        self._descr = descr
        self._single = single
        self._arguments = arguments
        self._prefill = prefill
        
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

    def arguments(self):
        return self._arguments

    def prefill(self):
        return self._prefill


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
    """Pull-down menu selection."""
    RADIO = 'RADIO'
    """Radio box with a radio button for each selection value."""
    LIST_BOX = 'LIST_BOX'
    """Scrollable list selection."""
    CODEBOOK = 'CODEBOOK'
    """Hand-editable code entry field with a Codebook form invocation button."""
    LIST = 'LIST'
    """Scrollable list with all codebook columns."""
    # Backwards compatibility options
    RADIO_BOX = RADIO
    """Depricated."""
    
   
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
    """Specifikace funkce pro dopočítání hodnoty sloupce."""
    
    def __init__(self, function, depends=None):
        """Inicializuj specifikaci.

        Argumenty:

          function -- libovolná funkce vracející hodnotu kompatibilní s vnitřní
            hodnotou datového typu odpovídajícího sloupci, pro který je
            použita.
            
          depends -- seznam sloupců, na kterých dané počítané políčko závisí.
            Měl by obsahovat všechny sloupce, které počítací funkce používá pro
            určení výsledné hodnoty.  Hodnota potom bude přepočítána pouze
            při změně v uvedených políčkách. Pokud je uveden prázdný seznam,
            nebude hodnota přepočítána nikdy (stále však bude vypočítána při
            inicializaci formuláře). Jedná se o seznam identifikátorů sloupců
            jako řetězců.

        """
        import re
        assert callable(function)
        self._function = function
        if depends is None:
            raise ProgramError("Computer has no dependency specification!")
        assert is_sequence(depends)
        self._depends = depends

    def __call__(self, *args, **kwargs):
        return apply(self._function, args, kwargs)

    def function(self):
        """Vrať funkci zadanou v konstruktoru."""
        return self._function

    def depends(self):
        """Vrať seznam id sloupců, ne kterých počítaná hodnota závisí."""
        return self._depends    


def computer(function):
    """Return a Computer instance for given function.

    This convenience wrapper creates a Computer instance and wraps the computer function by a code,
    that automatically converts row values into function arguments.

    Any named (positional) arguments of given 'function' which follow the first argument (which is
    the current row as usual) are used for automatic construction of the 'depends' list and the
    function will receive the Python values of the corresponding fields in place of these arguments.

    Example:

        def func(row):
             return row['aa'].value() + row['bb'].value()
        Computer(func, depends=('aa', 'bb'))

    is equivalent to:

        def func(row, aa, bb):
            return aa + bb
        c = computer(func)
        
    or:

        c = computer(lambda r, aa, bb: aa + bb)

    The first argument (row) is always passed, but should not be used to access field values
    (fields accessed this way would not be visible in computer's dependencis).  It may still be
    usefull, however, to access other information needed by the computer function.

    """
    assert callable(function) and not isinstance(function, Computer)
    columns = argument_names(function)[1:]
    def func(row):
        kwargs = dict([(column, row[column].value()) for column in columns])
        return function(row, **kwargs)
    return Computer(func, depends=columns)
    

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
        assert isinstance(field, str)
        assert column is None or isinstance(column, str)
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
        
          display -- defines the method of retrieving the user visible value of the codebook item
            (see below for more information about user visible values).  None (the default value)
            means to use the codebook code as the user visible value (no conversion).  A string
            value refers to a column in the *data* object of the codebook.  The user visible value
            is retrieved from given column.  You may also pass a function (callable object).  The
            user visible value is then computed by invoking the function, passing it the code
            (internal Python value of the codebook field) as an argument.  The returned value must
            be a string.  If a function is passed and this function has just one argument named
            'row', the function will recieve the data row of the corresponding codebook data object
            as an argument (instead of just the internal codebook value).
          prefer_display -- If true, the user interface will show the display value instead of the
            codebook internal value wherever possible.  For example the browse from will show the
            display value instead of the code (the display is normally only shown in the status
            line or tooltip).  See below for more details
          display_size -- width of the codebook display field in characters.  It is possible to
            override this value by the argument of the same name within the field specification
            (for particular field).
          begin_search -- None or an identifier of a column, where incremental search whould be
            automatically started when a codebook form is invoked (GUI only).

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
        assert display is None or isinstance(display, str) or callable(display) and len(argument_names(display)) == 1
        assert isinstance(prefer_display, bool)
        assert display_size is None or isinstance(display_size, int)
        assert begin_search is None or isinstance(begin_search, str)
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
        """Return true if the display should be prefered over the internal codebook value."""
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
    
    EDIT = 'EDIT'
    """Otevření editačního formuláře jednoho záznamu."""
    
    INSERT = 'INSERT'
    """Otevření editačního formuláře pro vložení nového záznamu."""


class Link(object):
    """Specification of a link from field to a differnt view.

    Used as a value of 'Field' constructor argument  'link'.

    """
    
    def __init__(self, name, column, type=FormType.BROWSE, binding=None, label=None, enabled=True):
        """Arguments:

          name -- name of the refered specification as a string.

          column -- column identifier in the refered specification.  This column is used to locate
            the record corresponding to the current value of the refering field.

          type -- type of the form used to display the refered view/record as one of 'FormType'
            constants.  The default type is 'FormType.BROWSE'.  This argument may also be used as
            positional.

          binding -- 'Binding' specification identifier (string) determining the current side view
            shown along with the refered record.  A binding with given identifier must exist in the
            'bindings' specification of 'name'.

          label -- titulek odkazu v menu.  Pokud není uveden, bude odkaz
            pojmenován automaticky a zařazen mezi automaticky generované
            odkazy.  Pokud je titulek uveden, bude v uživatelském rozhraní
            odkaz uveden samostatně před všemi automaticky generovanými odkazy.
            
          enabled -- funkce, vracející pravdu, pokud má být odkaz aktivní a
            nepravdu v opačném případě.  Funkci je předán jeden argument --
            instance `PresentedRow' aktuálního řádku.  Namísto funkce může být
            předána též přímo boolean hodnota, která dostupnost akce určuje
            staticky.
            
        """
        assert isinstance(name, str)
        assert isinstance(column, str)
        assert binding is None or isinstance(binding, str)
        assert type in public_attributes(FormType)
        assert label is None or isinstance(label, (str, unicode))
        assert callable(enabled) or isinstance(enabled, bool)
        self._name = name
        self._column = column
        self._binding = binding
        self._type = type
        self._label = label
        self._enabled = enabled
                
    def name(self):
        """Vrať název specifikace odkazovaného náhledu."""
        return self._name

    def column(self):
        """Vrať id odpovídajícího sloupce v odkazovaném náhledu."""
        return self._column

    def type(self):
        """Vrať konstantu typu formuláře, který má být otevřen."""
        return self._type

    def binding(self):
        """Return the identifier of the target binding specification or None."""
        return self._binding

    def label(self):
        """Vrať typ formuláře, který má být otevřen."""
        return self._label

    def enabled(self):
        """Vrať funkci k zjištění dostupnosti akce, nebo přímo bool hodnotu."""
        return self._enabled
    

class ListLayout(object):
    """Specification of list layout.

    Currently only implemented in web forms.

    This layout defines an alternative presentation of lists of records.  The records are not
    presented as a table, but as sections, where each record has its own heading, meta information
    and text (description, annotation, message...).
    
    """
    def __init__(self, title, meta=(), layout=None, content=(), image=None, anchor=None,
                 meta_labels=False, columns=1, allow_index=False):
        """Arguments:

          title -- identifier of a field which will be used as a title for each item in the list (a
            string).  An 'lcg.TranslatableText' instance can also be passed as an interpolation
            template.  In this case the title will be produced by interpolation of formatted row
            values within given string (with python string formatting syntax).

          meta -- a sequence of field identifiers (strings) which will be printed underneath each
            item's title as records meta information.  A single item may be passed as a string
            directly.

          layout -- 'GroupSpec' instance describing the layout of a fields
            within each item's section.  If used (not None), the fields will be
            displayed for each record in a manner simillar to a show form.
            Similarly as for the 'layout' argument in 'ViewSpec', it is also
            possible to pass a sequence of fields (or 'GroupFpec' instances)
            which will be turned into a vertical group automatically.

          content -- free content provider a field identifier or a function.  If a function is used
            it must accept one argument (a PresentedRow instance) and return 'lcg.Content' or None.
            If a field identifier is used, the field value must be a string formatted as LCG
            structured text.  The field text is parsed to produce the content.  A sequence of
            functions or field identifiers may also be used to provide more pieces of content
            concatenated together on the output.

          image -- identifier of a field which provides an image to be displayed along with each
            record.

          meta_labels -- boolean flag indicating, whether 'meta' fields should be labeled.  If a
            sequence is passed, only meta fields with identifiers contained within the sequence
            will be babeled.

          columns -- number of columns; integer.

          allow_index -- boolean flag indicating, whether an index of all displayed records should
            be displayed at the top of the list.  Each item in this index makes it possible to jump
            directly to the record.

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
    
    
class Field(object):
    """Specification of a generic form field representing a data value.

    This specification is used for all kinds of situations where fields are
    used to display and/or edit values, such as screen forms, tables, print
    reports etc.

    Only certain subset of information defined here is relevant for each
    situation.  The exact way of interpretarion of this specification depends
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
          type -- explicit data type as a 'pytis.data.Type' class or instance.  None value means to
            use the default type determined from the underlying data object (or the default type
            'pyttis.data.String' for virtual fields not present in the data object).  If a class is
            passed, the instance of this class will be created automatically and the system will
            pass it all arguments which it is able to determine.  If an instance is passed it is
            used as is even if the system would create it with other arguments.  So by passing a
            class you leave the system to do its best, while by passing an instance, you can force
            whatever you want.  In any case, the specified type must be compatible with the type
            determined by the underlying data object.  If None or if a class is specified,
            individual type constructor arguments may be passed separately as additional 'Field'
            keyword arguments and they take precedence over the arguments determined by system.
            Thus this is a more gentle way to force specific properties of the field data type
            individually.  The arguments 'codebook' and 'enumerator' determine the constructed
            type's enumerator.
          width -- field width in characters (integer).  Default width is determined automatically
            if not specified here.  Certain types of input fields may interpret the value
            differently (e.g. as a number of columns) when number of characters doesn't make sense.
          height -- field height in characters (integer).  Certain types of input fields may
            interpret the value differently (e.g. as a number of rows) when number of characters
            doesn't make sense.
          column_width -- table column width in characters (integer).  If not defined, defaults to
            'width'.
          disable_column -- If true, it is not possible to display the field as a table
            column.  The field does not appear in the selection of columns to display and presence
            of such field in default columns ('ViewSpec' argument 'columns') is announced as
            an error.
          fixed -- passing True value will disable automatic scaling of column width when the table
            size is changed.  The default behavaior is to accommodate column widths to new form
            size, so that the available space is used evenly between all columns.  Fixed columns,
            however will be left out during these recomputations and will keep their prevoius
            widths.
          editable -- one of 'Editable' constants or a 'Computer' instance.  The constants
            determine field editability statically, the computer may be used to compute editability
            dynamically based on the values of other fields of a record and return true iff the
            field is editable (see also notes about computer specifications below).
            The default value is 'Editable.ALWAYS', but certain combinations of other specification
            parameters may lead to another default value (for example if a'computer' is defined, the
            default value is 'Editable.NEVER'). 
          compact -- true value results in the field label being displayed above the field, not on
            the left which is the default.  This way the field will span to the full width of the
            field group.
          nocopy -- iff true, the field's value will be omitted during record copying (user command
            for creation of a new record as a copy af an existing record).  Key columns and
            computed fields depending on key columns are omitted automatically.
          default -- default value or a function for computing the default value.  The default
            value is used when a new record is initialized.  Please note, that if computer is
            defined, it has higher precedence than the default value.  You may pass a value
            directly or a callable object.  The callable object will be called with no arguments
            when the default value is needed and its returned value will be used.  In any case, the
            default value must be compatible with the internal Python representation for the data
            type of the field.  If not defined, the default value is determined by the data type
            (usually 'None').
          computer -- a 'Computer' instance for computing the field value based on the values of
            other fields of the same record.  See below for more details about computed fields.
          null_display -- display value (string) to use for the unselected state of an enumeration
            field (null field value).  Null value is not part of the enumeration, but if the field
            is not 'not_null', it is a valid field value, but as it is not within the enumeration,
            'display' may not be used.
          line_separator -- line separator in single line field value presentation.
          codebook -- name (string) of the specification which acts as a codebook for this field.
            This argument has two effects.  It is used as the default value of 'enumerator' (if
            'enumerator is not set explicitly) and it determines the name of the specification used
            for codebook form invocation for 'SelectionType.CODEBOOK'.  Specifying 'codebook'
            causes 'selection_type' to default to 'SelectionType.CODEBOOK'.  From the other
            perspective 'SelectionType.CODEBOOK' requires 'codebook' to be defined.
          enumerator -- field data type enumerator as a string (name of a specification for
            'pytis.data.DataEnumerator' construction), 'pytis.data.DataFactory' instance (for
            'pytis.data.DataEnumerator' construction) or a 'pytis.data.Enumerator' instance
            directly.  Unlike 'codebook', 'enumerator' is only used for field's data type
            construction and has no effect on 'selection_type'.  If None, enumerator will default
            to the value of 'codebook'.
          display -- overrides the same 'CodebookSpec' option for this particular field.  If not
            defined, the value defaults to the value defined by the related codebook.
          prefer_display -- overrides the same 'CodebookSpec' option for this particular field.  If
            not defined, the value defaults to the value defined by the related codebook.
          display_size -- overrides the same 'CodebookSpec' option for this particular field.  If
            not defined, the value defaults to the value defined by the related codebook.
          allow_codebook_insert -- true value enables a button for codebook new record insertion.
            This button is displayed next to the codebook field.
          codebook_insert_spec -- name of the specification to use for codebook insertion when
            'allow_codebook_insert' is true.  If none, the value defined by 'codebook' is used.
          runtime_filter -- provider of enumeration runtime filter as a 'Computer' instance.  The
            computer function computes the filter condition based on the current row data and
            returns it as a 'pytis.data.Operator' instance.  This condition is used to filter out
            enumerator data for codebook fields as well as available completions when
            autocompletion is enabled.  This is mostly useful for modification of available
            codebook values based on the current values of other fields within the form.
          runtime_arguments -- provider of codebook table function arguments as a
            'Computer' instance.  This is similar to 'runtime_filter' argument,
            except that the computer function returns dictionary of table
            function arguments.  'runtime_arguments' may be provided only when the
            field is a codebook field and the codebook is actually a row
            returning function.  Otherwise 'runtime_arguments' must be 'None'.
          completer -- enumerator used for automatic completion.  The available completions are
            taken from an enumerator object.  If the field has an enumerator (defined by
            'enumerator' or 'codebook'), it will be used for completions automatically (unless
            autocompletion is disabled by the relevant 'CodebookSpec').  This argument, however,
            makes it possible to specify a completer even for fields, which don't have an
            enumerator (the validation constraints imposed by enumerator are not desirable).  The
            value of this argument may be an enumerator instance directly
            (e.g. 'pytis.data.FixedEnumerator') or a name of the specification used to create a
            'pytis.data.DataEnumerator'.  Also a sequens (list or tuple) is accepted and converted
            to a 'FixedEnumerator' instance.
          selection_type -- one of 'SelectionType' constants defining the type of user interface
            element used to present the related enumeration.  Only relevant for fields with an
            enumerator (specified either by 'codebook' or 'enumerator').  If 'codebook' is not
            None, selection_type defaults to 'SelectionType.CODEBOOK'.  Also if selection_type is
            set to 'SelectionType.CODEBOOK', 'codebook' must be defined.
          orientation -- field orientation as one of 'Orientation' class constants; relevant only
            for certain field types, such as radio buttons, which may be arranged vertically or
            horizontally..
          post_process -- funkce upravující vkládaný text během psaní.  Jedná
            se o funkci jednoho argumentu, kterým je řetězcová hodnota políčka.
            Vrácená hodnota je potom nastavena jako nová hodnota políčka.  Tato
            funkce je volána při každé změně hodnoty textového políčka.
            Příkladem postprocessingu může být změna velikosti písmen, pokud
            chceme, aby textové políčko mohlo obsahovat jen velká písmena.
            Hodnotou tohoto argumentu může být také některá z konstant třídy
            'PostProcess', čímž je ušetřeno psaní některých často používaných
            funkcí.
          filter -- specifikace jednoho z přednastavených filtrů znaků
            propouštěných do textového políčka z uživatelského vstupu.  Jedna
            z konstant třídy 'TextFilter'.
          filter_list -- sekvence povolených, nebo zakázaných znaků.
            Relevantní jen pro 'filter' typu 'INCLUDE_LIST' nebo
            'EXCLUDE_LIST'.
          style -- instance třídy 'Style' určující vizuální styl políčka
            nebo funkce dvou argumentů vracející instanci třídy 'Style'.
            Jedná-li se o funkci, jsou jejími argumenty id sloupce jako string
            a aktuální datový řádek jako instance 'PresentedRow'.  Pokud je
            'None', bude použit výchozí styl řádku (viz. argument 'row_style'
            konstruktoru 'ViewSpec').
          link -- specification of a link, or a series of links to other forms related to the
            current field value.  The value is a 'Link' instance or their sequence.  The links will
            be presented as separate menu items in the context menu of a record in the GUI.  The
            web forms currently only support one link per field and present it as a hypertext link
            on field's value.  The links will open the related form and locate the record
            corresponding to the value of the refering field.
          filename -- identifier of the field, which provides the filename for downloading/saving
            the value of this field into a file.  If not None, the user interface should offer
            downloading/saving the content of the field into a file.  This may be relevant for
            binary fields, as well as for ordinary string data.
          printable -- iff True, the user interface should allow the value of
            this field to be printed as a separate document.  This is most often
            useful with fields containing structured text content, which may be
            directly exported into PDF.
          **kwargs -- all the remaining keyword arguments are passed to the
            constructor of field's data type instance.  These arguments
            override the values of arguments, that the system would normally
            use for data type construction, so you can override certain data
            type properties this way.  It is prefered to overriding the type
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
        
        V každém případě je počítaný sloupec implicitně needitovatelný
        ('Editable.NEVER'), pokud není explicitně nastaven jako editovatelný
        pomocí specifikátoru 'editable'.

        """
        for key, value in (('id', id), ('label', label) ,('column_label', column_label)):
            if value is not None:
                kwargs[key] = value
        if inherit:
            assert isinstance(inherit, Field), inherit
            kwargs = dict(inherit._kwargs, **kwargs)
        self._kwargs = kwargs
        self._init(**kwargs)
                 
    def _init(self, id, label=None, column_label=None, descr=None, virtual=False, dbcolumn=None,
              type=None, type_=None, width=None, column_width=None, disable_column=False,
              fixed=False, height=None, editable=None, compact=False, nocopy=False, default=None,
              computer=None, line_separator=';', codebook=None, display=None, prefer_display=None,
              display_size=None, null_display=None, allow_codebook_insert=False,
              codebook_insert_spec=None, codebook_runtime_filter=None, runtime_filter=None,
              runtime_arguments=None, selection_type=None, completer=None,
              orientation=Orientation.VERTICAL, post_process=None, filter=None,
              filter_list=None, style=None, link=(), filename=None, printable=False,
              enumerator=None, value_column=None, validity_column=None,
              validity_condition=None,
              **kwargs):
        def err(msg, *args):
            """Return assertion error message."""
            return "Field '%s': " % id + msg % args
        def log_(msg, *args):
            """Return assertion error message."""
            log(OPERATIONAL, "Field '%s':" % id, msg % args)
        assert isinstance(id, str)
        assert dbcolumn is None or isinstance(dbcolumn, str)
        if type_ is not None:
            assert type is None
            type = type_
        assert label is None or isinstance(label, (str, unicode))
        assert descr is None or isinstance(descr, (str, unicode))
        assert type is None or isinstance(type, pytis.data.Type) \
            or issubclass(type, pytis.data.Type)
        assert isinstance(virtual, bool)
        assert isinstance(disable_column, bool)
        assert isinstance(fixed, bool)
        assert isinstance(compact, bool)
        assert isinstance(nocopy, bool)

        assert computer is None or isinstance(computer, Computer), computer
        
        assert codebook is None or isinstance(codebook, str)
        assert display is None or isinstance(display, str) or callable(display)
        assert completer is None or isinstance(completer, (str,list,tuple,pytis.data.Enumerator))
        assert prefer_display is None or isinstance(prefer_display, bool)
        assert display_size is None or isinstance(display_size, int)
        assert null_display is None or isinstance(null_display, basestring)
        # TODO: Enable this after merging data-type-cleanup! (belongs to the line above)
        # and not not_null and (codebook or enumerator)
        assert isinstance(allow_codebook_insert, bool)
        assert codebook_insert_spec is None or isinstance(codebook_insert_spec, str)
        assert width is None or isinstance(width, int)
        if codebook_runtime_filter is not None:
            assert runtime_filter is None
            runtime_filter = codebook_runtime_filter
        assert runtime_filter is None or isinstance(runtime_filter, Computer), runtime_filter
        assert runtime_arguments is None or isinstance(runtime_arguments, Computer), runtime_arguments
        assert selection_type is None or selection_type in public_attributes(SelectionType)
        assert selection_type is None or selection_type in public_attributes(SelectionType)
        assert orientation in public_attributes(Orientation)
        assert post_process is None or callable(post_process) \
            or post_process in public_attributes(PostProcess)
        assert filter is None or filter in public_attributes(TextFilter)
        assert filter not in ('INCLUDE_LIST', 'EXCLUDE_LIST') or is_sequence(filter_list)
        assert style is None or isinstance(style, Style) or callable(style), \
            err("Invalid 'style' specification: %s", style)
        assert filename is None or isinstance(filename, str)
        assert isinstance(printable, bool)
        if enumerator is None:
            enumerator = codebook
        else:
            assert isinstance(enumerator, (str, pytis.data.DataFactory, pytis.data.Enumerator))
        enumerator_kwargs = dict([(k, v) for k, v
                                  in dict(value_column=value_column,
                                          validity_column=validity_column,
                                          validity_condition=validity_condition).items()
                                  if v is not None])
        #assert not enumerator_kwargs or not isinstance(enumerator, pytis.data.Enumerator), \
        #    err("'enumerator' defined as Enumerator instance and '%s' passed.",
        #        enumerator_kwargs.keys()[0])
        links = xtuple(link)
        if __debug__:
            # Temporary: The following test replaces the commented out assertion above.  The
            # assertion would break older applications, so we just log for now.
            if enumerator_kwargs and isinstance(enumerator, pytis.data.Enumerator):
                log_("'enumerator' defined as Enumerator instance and '%s' passed.",
                     enumerator_kwargs.keys()[0])
            for lnk in links:
                assert isinstance(lnk, Link), err("Invalid object in links: %r", lnk)
            for k in kwargs.keys():
                assert k in ('not_null', 'unique', 'constraints', 'minlen', 'maxlen',
                             'precision', 'format', 'mindate', 'maxdate', 'utc',
                             'validation_messages', 'inner_type'), \
                             err("Invalid argument: %r", k)
            if isinstance(type, pytis.data.Type):
                for arg, value in (('codebook', codebook),
                                   ('enumerator', enumerator),
                                   (enumerator_kwargs, enumerator_kwargs.keys()),
                                   (kwargs, kwargs.keys())):
                    #assert not value, err("'type' defined as Type instance and '%s' passed.", arg)
                    # Temporary: Assertion would break older applications, so we just log for now.
                    if value:
                        log_("'type' defined as Type instance and '%s' passed.", arg)
        self._id = id
        self._dbcolumn = dbcolumn or id
        if label is None:
            label = id # TODO: Allow unlabeled fields?
        self._label = label
        if column_label is None:
            column_label = label
        self._column_label = column_label
        if (enumerator or codebook) and not kwargs.has_key('not_null'):
            # Enumeration fields are NOT NULL by default.  It is not very intuitive, but
            # we must keep it for backwards compatibility.
            kwargs['not_null'] = True
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
        if editable is None:
            if width == 0 or computer:
                editable = Editable.NEVER
            else:
                editable = Editable.ALWAYS
        elif isinstance(editable, Computer):
            # For backwards compatibility
            e_func = editable.function()
            if len(argument_names(e_func)) == 2:
                editable = Computer(lambda r: e_func(r, id), depends=editable.depends())
        else:
            assert editable in public_attributes(Editable), editable
        self._editable = editable
        self._line_separator = line_separator
        self._codebook = codebook
        self._display = display
        self._prefer_display = prefer_display
        self._display_size = display_size
        self._null_display = null_display
        self._allow_codebook_insert = allow_codebook_insert
        self._codebook_insert_spec = codebook_insert_spec
        self._runtime_filter = runtime_filter
        self._runtime_arguments = runtime_arguments
        self._selection_type = selection_type
        if isinstance(completer, (list, tuple)):
            completer = pytis.data.FixedEnumerator(completer)
        self._completer = completer
        self._orientation = orientation
        self._post_process = post_process
        self._filter = filter
        self._filter_list = filter_list
        if callable(style):
            s_func = style
            if len(argument_names(s_func)) == 2:
                # For backwards compatibility
                style = lambda r: s_func(id, r)
        self._style = style
        self._links = links
        self._filename = filename
        self._printable = printable

    def __str__(self):
        return "<Field for '%s'>" % self.id()

    def clone(self, field):
        """Clone this field by another field and return the cloned instance.

        The cloned instance will inherit all attributes of this field and the
        other field passed as argument, where the attributes of the later field
        take precedence.

        """
        assert isinstance(field, Field), field
        kwargs = dict(self._kwargs, **field._kwargs)
        return Field(**kwargs)

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

    def compact(self):
        return self._compact

    def nocopy(self):
        return self._nocopy

    def default(self):
        return self._default

    def computer(self):
        return self._computer

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

    def allow_codebook_insert(self):
        return self._allow_codebook_insert
    
    def codebook_insert_spec(self):
        return self._codebook_insert_spec
    
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

    def printable(self):
        return self._printable

    def completer(self, resolver):
        """Return field completer as a 'pytis.data.Enumerator' instance."""
        completer = self._completer
        if isinstance(completer, str):
            # Completer was defined as a specification name.
            data_spec = resolver.get(completer, 'data_spec')
            completer = pytis.data.DataEnumerator(data_spec, **self._enumerator_kwargs)
        return completer
    
    def type_kwargs(self, resolver):
        """Return the keyword arguments for field's data type construction.
        
        This method should never be called from outside of the 'pytis.presentation' module.

        """
        kwargs = dict(self._type_kwargs)
        enumerator = self._enumerator
        if enumerator is None:
            enumerator = self._codebook
        if isinstance(enumerator, str):
            enumerator = resolver.get(enumerator, 'data_spec')
        if isinstance(enumerator, pytis.data.DataFactory):
            enumerator = pytis.data.DataEnumerator(enumerator, **self._enumerator_kwargs)
        if enumerator:
            kwargs['enumerator'] = enumerator
        if 'inner_type' in kwargs:
            # This is quite a hack - it assumes all type arguments are the
            # inner type arguments.  If not, an instance must be passed
            # directly, but that doesn't allow the enumerator magic.
            inner_type = kwargs.pop('inner_type')
            if type(inner_type) == type(pytis.data.Type):
                kwargs = {'inner_type': inner_type(**kwargs)}
        return kwargs


# Backwards compatibility alias
FieldSpec = Field


class Fields(object):
    """Deprecarted -- use `Specification._inherited_fields()' instead."""

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
    

class Specification(object):
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
    
    """
    
    table = None
    """Název datové tabulky jako řetězec.

    Pokud název není určen, bude odvozen automaticky z názvu specifikační
    třídy.  Kapitálky jsou převedeny na slova oddělená podtržítkem, takže
    např. pro specifikační třídu 'UcetniOsnova' bude název tabulky
    'ucetni_osnova'.  Z hlediska přehlednosti je doporučováno volit toto jmenné
    schéma a vyhnout se tak explicitnímu určování názvů tabulek.

    """

    key = None
    """Data object key column identifier as a string or their sequence.
    
    Sequence may be used if the data object has a multicolumn key.  In any case all named columns
    must exist in the 'fields' specification.  In 'key' is not defined, the first column form
    'fields' is used."""

    fields = ()
    """Specification of all fields as a sequence of 'Field' instances.
    
    May be also defined as a method of the same name."""

    arguments = None
    """Specification of all table arguments as a sequence of 'Field' instances.

    Useful only when the table is actually a row returning function, otherwise
    it must be 'None'.
    
    May be also defined as a method of the same name."""
    
    access_rights = None
    """Access rights for the view as an 'AccessRights' instance.

    May be also defined as a method of the same name."""

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

    initial_folding = None
    """'FoldableForm.Folding' instance defining initial folding.

    'None' means use the standard folding.

    Obviously useful only in foldable forms.

    """

    _access_rights = None

    @staticmethod
    def _init_access_rights(connection_data):
        """Read access rights of data specifications from the database.

        Arguments:

          connection_data -- 'pytis.data.DBConnection' instance

        This is actually a public method, but pytis doesn't allow its name to
        start with something else than underscore.

        """
        import config
        # Read in and check roles
        try:
            roles_data = pytis.data.dbtable('ev_pytis_user_roles', ('roleid',), connection_data)
            roles = roles_data.select()
        except pytis.data.DBException:
            return
        if roles == 0:
            Specification._access_rights = 'nonuser'
            return
        access_rights = {}
        user_rights = {}
        # Assign computed user rights
        S = pytis.data.String()
        rights_data = pytis.data.dbtable('pytis_view_user_rights',
                                         (('shortname', S,), ('rights', S,), ('columns', S,),),
                                         connection_data, arguments=())
        def process(row):
            shortname, rights_string, columns_string = row[0].value(), row[1].value(), row[2].value()
            if not rights_string:
                return
            if columns_string:
                columns = string.split(columns_string, ' ')
            else:
                columns = [None]
            shortname_rights = access_rights.get(shortname)
            if shortname_rights is None:
                shortname_rights = access_rights[shortname] = {}
                user_rights[shortname] = True
            for r in rights_string.split(' '):
                if r != 'show':
                    shortname_rights[r] = shortname_rights.get(r, []) + columns
        rights_data.select_map(process)
        # System rights may limit rights to certain columns
        sysrights_data = pytis.data.dbtable('ev_pytis_user_system_rights',
                                            ('shortname', 'rightid', 'colname',),
                                            connection_data)
        def process(row):
            shortname, right, colname = row[0].value(), row[1].value(), row[2].value()
            shortname_rights = access_rights.get(shortname)
            columns = (shortname_rights or {}).get(right)
            if columns is None:
                if not user_rights.has_key(shortname):
                    if shortname_rights is None:
                        shortname_rights = access_rights[shortname] = {}
                    shortname_rights[right] = shortname_rights.get(right, []) + [colname]
        sysrights_data.select_map(process)
        # Transform access rights specifications to AccessRights instances
        def process(right, columns):
            if not columns or None in columns:
                columns = None
            return (columns, (None, str(right.upper()),),)
        for shortname, rights in access_rights.items():
            access_rights_spec = [process(right, columns) for right, columns in rights.items() if right != 'show']
            access_rights[shortname] = pytis.data.AccessRights(*access_rights_spec)
        # Forbid actions without any rights for the current user
        actions_data = pytis.data.dbtable('e_pytis_action_rights', ('shortname', 'system', 'status',),
                                          connection_data)
        condition = pytis.data.AND(pytis.data.EQ('system', pytis.data.Value(pytis.data.Boolean(), True)),
                                   pytis.data.LE('status', pytis.data.Value(pytis.data.Integer(), 0)))
        for value in actions_data.distinct('shortname', condition=condition):
            shortname = value.value()
            if not access_rights.has_key(shortname):
                access_rights[shortname] = pytis.data.AccessRights()
        # That's all
        Specification._access_rights = access_rights
        
    def __init__(self, resolver):
        self._resolver = resolver
        for attr in ('fields', 'arguments', 'access_rights', 'condition', 'distinct_on',
                     'bindings', 'cb', 'sorting', 'filters', 'conditions',
                     'initial_folding',):
            if hasattr(self, attr):
                value = getattr(self, attr)
                if callable(value):
                    setattr(self, attr, value())
        assert self.fields, 'No fields defined for %s.' % str(self)
        assert isinstance(self.fields, (list, tuple))
        assert self.arguments is None or isinstance(self.arguments, (list, tuple))
        self._view_spec_kwargs = {'help': self.__class__.__doc__}
        for attr in dir(self):
            if not attr.startswith('_') and not attr.endswith('_spec') and \
                   attr not in ('table', 'key', 'connection', 'access_rights', 'condition',
                                'distinct_on', 'data_cls', 'bindings', 'cb', 'prints',
                                'data_access_rights', 'arguments',
                                'oid', # for backward compatibility 
                                ):
                self._view_spec_kwargs[attr] = getattr(self, attr)
        if isinstance(self.bindings, (tuple, list)):
            # Only pass new style bindings to ViewSpec, old style bindings are accessed through the
            # 'binding_spec' resolver function. 
            self._view_spec_kwargs['bindings'] = self.bindings
        else:
            assert isinstance(self.bindings, dict)
        for arg in ('layout', 'list_layout', 'actions', 'columns', 'grouping'):
            try:
                value = self._view_spec_kwargs[arg]
            except:
                continue
            if callable(value):
                self._view_spec_kwargs[arg] = value()
        #if self.__class__.__doc__:
            #parts = re.split('\n\s*\n', self.__class__.__doc__, maxsplit=2)
            #if not self._view_spec_kwargs.has_key('description'):
            #    self._view_spec_kwargs['description'] = parts[0]
            #if not self._view_spec_kwargs.has_key('help') and len(parts) > 1:
            #    self._view_spec_kwargs['help'] = parts[1]

    def _action_spec_name(self):
        spec_name = self.__class__.__name__
        if self.__class__.__module__:
            spec_name = self.__class__.__module__ + '.' + spec_name
        spec_name = spec_name.replace('/', '.')
        return spec_name

    def _create_data_spec(self):
        resolver = self._resolver
        if issubclass(self.data_cls, pytis.data.DBData):
            B = pytis.data.DBColumnBinding
            table = self.table or camel_case_to_lower(self.__class__.__name__, '_')
            bindings = [B(f.id(), table, f.dbcolumn(), type_=f.type(), **f.type_kwargs(resolver))
                        for f in self.fields if not f.virtual()]
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
                arguments = [B(f.id(), table, f.dbcolumn(), type_=f.type(), **f.type_kwargs(resolver))
                             for f in self.arguments]
        else:
            def type_(f):
                t = f.type() or pytis.data.String
                if type(t) == type(pytis.data.Type):
                    kwargs = f.type_kwargs(resolver)
                    t = t(**kwargs)
                return t
            columns = [pytis.data.ColumnSpec(f.id(), type_(f))
                       for f in self.fields if not f.virtual()]
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
                      arguments=arguments)
        return pytis.data.DataFactory(self.data_cls, *args, **kwargs)

    def _create_view_spec(self, title=None, **kwargs):
        if not title:
            title = ' '.join(split_camel_case(self.__class__.__name__))
        return ViewSpec(title, **kwargs)

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

        """
        fields = super(cls, self).fields
        if callable(fields):
            fields = fields()
        if override:
            inherited = dict([(field.id(), field) for field in fields])
            overriden = dict([(field.id(), inherited[field.id()].clone(field))
                              for field in override])
        else:
            overriden = {}
        return tuple([overriden.get(field.id(), field)
                      for field in fields if field.id() not in exclude])
    
    def view_spec(self):
        """Vrať prezentační specifikaci jako instanci 'ViewSpec'."""
        try:
            spec = self._view_spec
        except AttributeError:
            kwargs = self._view_spec_kwargs
            spec = self._view_spec = self._create_view_spec(spec_name=self._action_spec_name(), **kwargs)
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
        return self.prints

    def access_spec(self):
        """Return the 'access_rights' attribute value.

        This is here to make access rights easily accessible by method call from resolvers.
        
        """
        return self.access_rights

    def initial_folding_spec(self):
        """Return initial folding."""
        return self.initial_folding

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
