# -*- coding: utf-8 -*-

# Copyright (C) 2018-2020 Tomáš Cerha <t.cerha@gmail.com>
# Copyright (C) 2007-2017 OUI Technology Ltd.
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

import datetime
import io
import os
import tempfile
import time

import pytis.data
import pytis.data as pd
import pytis.output
import pytis.presentation as pp
import pytis.extensions
import pytis.form
import lcg

from pytis.api import app
from pytis.util import nextval, translations
from pytis.presentation import (
    Specification, Field, FieldSet, HGroup, PostProcess,
    Editable, TabGroup, Binding, computer, procedure,
    SelectionType, Action, QueryFields, PrintAction
)
from pytis.output import PrintSpecification
import pytis.dbdefs.demo as dbdefs

_ = translations('pytis-demo')


class InputFields(Specification):
    """Demonstration of most available input field types.

    This example shows:

    1. "Basic fields", such as textual, numeric or date entries where the value
       is only limited by the field data type,

    2. "Range fields", where you enter the lower and the upper bounds of a
       range into two basic fields,

    3. "Enumeration fields", where the value is additionally limited to a
       certain set of available values from which the user selects (typically
       used to represent 1:N database relations).

    By `enumeration' we understand the set of valid values for a particular
    field.  This set of values is provided by an `enumerator' bound to the
    field's data type.  A simple enumerator may provide a fixed set of values
    defined in the specification, but the most typical usage is an enumerator
    bound to a data object.  This makes the enumeration user editable as any
    other data view of a Pytis application.  This type of enumeration is called
    a 'Codebook'.  Any Pytis specification may be used as a codebook.  This
    demo, for example, defines the codebooks of countries and continents.  See
    their specifications in the file `cb.py' for more details.

    There several kinds of enumeration fields.  The kind used for a particular
    field is determined by 'selection_type' in field specification.  The
    default is the `Codebook' field.  Other representations may use radio
    boxes, choices or other kinds of selections.  This demo shows how different
    selection types behave.

    Using `pd.RestrictedMemData' as the data object class (specified by the
    attrattribute 'data_cls') makes it possible to create a specification,
    which is not bound to any database table.  The data object doesn't hold any
    data, but otherwise it acts as any other data object.  This can be useful
    for example when you want to get some more complicated user input.  A form
    on top of a virtual specification can be used as a dialog, but allows the
    full power of pytis forms.  Form values can be retrieved as its return
    value.

    The following differences apply:

      * Since it is not possible to determine field data types automatically
        from the data specification (or database introspection), the dafault
        type is always `pd.String'

      * If you need to pass a codebook type (it is differnt not `pd.String'),
        you will also need to pass all its constructor arguments, including
        `enumerator'.  This makes it quite difficult, since you can't rely on
        automatic enumerator construction based on the `codebook' argument.

    Except for the restrictions mentioned above, everything else works just the
    same, so all the features used in this specification can be used generally
    in all pytis specifications.

    Virtual data objects can still have relations with other (non virtual) data
    objects, such as codebooks.

    """

    class _Fruits(pp.Enumeration):
        """Static enumeration of fruits (inner values and their UI labels)."""
        enumeration = (
            ('apples', _('Apples')),
            ('bananas', _('Bananas')),
            ('oranges', _('Oranges')),
        )

    public = True
    title = _("Enumeration fields")
    data_cls = pd.RestrictedMemData

    def __init__(self, resolver, title=_("Field Types")):
        """Dynamically parametrized specification example.

        The 'title' argument is just an example how to make the specification
        dynamically customizable.  In this case it just sets the title, but any
        arguments can be used to parametrize specification construction.  These
        parameters can be passed through the form constructor argument
        'spec_kwargs'.  This makes it possible to use a single specification in
        different contexts with different parameters.  In most situations, you
        want to create more specification classes (e.g. derived classes), but
        parametrization may be more practical in certain use cases.

        """
        self.title = title
        super(InputFields, self).__init__(resolver)

    def fields(self):
        return (
            # Basic fields
            Field('text', _("Text"), type=pd.String(maxlen=50), width=30,
                  editable=computer(self._editable)),
            Field('completion', _("Completion"), type=pd.String(maxlen=20),
                  editable=computer(self._editable),
                  completer=('John', 'Jane', 'Joe', 'Judith', 'Jeremy',
                             'Peter', 'Paul', 'Paula', 'Richard', 'Samantha', 'Samuel',
                             'Steve', 'Sue'),
                  descr=_("Text entry with autocompletion.  The 'completer' provides "
                          "available completions.  As opposed to enumerator (which "
                          "also provides autocompletion values), completer has no "
                          "effect on validation. You can define a static (like here), "
                          "dynamic or database connected list of completions.")),
            Field('multiline', _("Multiline"), type=pd.String(), width=30, height=4,
                  editable=computer(self._editable)),
            Field('password', _("Password"), type=pd.Password(maxlen=20, verify=True),
                  editable=computer(self._editable), width=14,
                  descr=_("Password field doesn't display the current value.  If 'verify' "
                          "is set to True in field's type, the field requires the value to "
                          "be entered twice and validation makes sure both values match.")),
            Field('number', _("Number"), type=pd.Integer(), default=0,
                  editable=computer(self._editable),
                  descr=_("A simple numeric field.")),
            Field('slider', _("Slider"), type=pd.Integer(minimum=20, maximum=50),
                  default=30, slider=True, width=2,
                  editable=computer(self._editable),
                  descr=_("A simple numeric field.")),
            Field('float', _("Decimal"), type=pd.Float(precision=2), default=0,
                  editable=computer(self._editable)),
            Field('date', _("Date"), default=pd.Date.datetime, type=pd.Date(),
                  editable=computer(self._editable),
                  descr=_("Date entry field with a calendar control and a default value.")),
            Field('time', _("Time"), type=pytis.data.Time(),
                  editable=computer(self._editable)),
            Field('datetime', _("Date and time"), type=pytis.data.DateTime(),
                  editable=computer(self._editable)),
            Field('file', _("File"), type=pytis.data.Binary(),
                  editable=computer(self._editable)),
            Field('image', _("Image"), type=pytis.data.Image(),
                  editable=computer(self._editable)),
            Field('color', _("Color"), type=pytis.data.Color(),
                  editable=computer(self._editable)),

            # Enumeration fields
            Field('editable', _("Check box"), type=pd.Boolean(), default=True,
                  descr=_("Check/uncheck this checkbox to enable/disable all other "
                          "form fields.  This field demonstrates a simple boolean "
                          "checkbox field.  It also demonstrates how to make fields "
                          "editable based on the values of some other fields (any "
                          "more complicated logic is possible in the 'computer' of "
                          "the 'editable' property in field specification). "
                          "Finally it demonstrates how all field types look in the "
                          "editable and non-editable state.")),
            Field('choice', _("Choice"), type=pd.String(not_null=True, maxlen=2),
                  codebook='cb.Continents',
                  selection_type=SelectionType.CHOICE,
                  editable=computer(self._editable),
                  descr=_("Simple pull-down menu selection.")),
            Field('radio', _("Radio box"), not_null=True,
                  enumerator=self._Fruits,
                  editable=computer(self._editable),
                  selection_type=SelectionType.RADIO,
                  descr=_("Radio box selection.  The enumeration is defined statically by "
                          "an Enumeration class.")),
            Field('listbox', _("List box"), not_null=True,
                  enumerator=self._Fruits,
                  selection_type=SelectionType.LISTBOX,
                  editable=computer(self._editable),
                  descr=_("List selection which becomes scrollable when the number of "
                          "items exceeds its height.")),
            Field('codebook', _("Codebook"), type=pd.String(not_null=True, maxlen=2),
                  codebook='cb.Countries', width=2,
                  # SelectionType.CODEBOOK is actually the default when 'codebook' is set.
                  selection_type=SelectionType.CODEBOOK,
                  post_process=PostProcess.UPPER,
                  editable=computer(self._editable),
                  descr=_(
                      "This field provides a text entry for writing down the code directly.  "
                      "It most convenient when the codebook contains a relatively large number of "
                      "items while their codes are short and simple.  Then the user most often "
                      "knows the codes without looking them up.  Right from the text entry field, "
                      "there is a codebook invocation button, which allows the user to select the "
                      "item from a codebook form (when he doesn't remember the code or wants to "
                      "see the full listing of all available items for some other reason).  "
                      "Further on the right, there is a gray field called `display'.  It shows "
                      "the description of the currently selected item (eg. the full name for the "
                      "selected code).  Thus it provides immediate feedback when the user "
                      "modifies the value.  It is empty when the entered code is invalid (not "
                      "contained in the codebook).  It is updated with any change of the field "
                      "value.")),
            Field('listfield', _("List Field"), type=pd.String(not_null=True, maxlen=2),
                  codebook='cb.Countries', compact=True, height=6,
                  selection_type=SelectionType.LIST,
                  editable=computer(self._editable),
                  descr=_("This field also allows direct codebook manipulations through the row "
                          "context menu.")),

            # Range fields
            Field('intrange', _("Numeric range"), type=pytis.data.IntegerRange(),
                  editable=computer(self._editable)),
            Field('daterange', _("Date range"), type=pytis.data.DateRange(),
                  editable=computer(self._editable)),

        )

    def layout(self):
        fields = self.fields
        return HGroup(
            FieldSet(_("Basic fields"),
                     [f.id() for f in fields
                      if ('enumerator' not in f.type_kwargs() and
                          not isinstance(f.type(), (pd.Range, pd.Boolean)))]),
            (FieldSet(_("Enumeration fields"),
                      [f.id() for f in fields
                       if 'enumerator' in f.type_kwargs() or
                       isinstance(f.type(), pd.Boolean)]),
             FieldSet(_("Range fields"),
                      [f.id() for f in fields if isinstance(f.type(), pd.Range)])),
        )

    def _editable(self, record, editable):
        return editable


class RuntimeFilter(Specification):
    """This specification demonstrates codebook runtime filtering.

    Runtime filter is a condition determining valid codebook items (available
    for selection).  This condition is recomputed in runtime similarly as the
    values of computed fields.  It can also depend on values of other fields in
    the current row.  The filter determines the items available for selection
    in the user interface controls as well as the validation process.

    Filters fan be applied as conditions for database selects
    ('runtime_filter') or as arguments to database or non-database functions
    ('runtime_arguments').  This specification shows database select condition
    and non-database function arguments.

    """
    public = True
    title = _("Codebook runtime filter demo")
    data_cls = pd.RestrictedMemData

    class _SystemGroups(pd.Enumerator):
        """Dynamic enumeration with values read from external source."""

        def values(self):
            groups = [[x.strip() for x in line.split(':')]
                      for line in open('/etc/group').readlines()
                      if not line.startswith('#')]
            return [group for group, _, gid, members in groups if members]

    class _SystemGroupMembers(pd.Enumerator):
        """Dynamic enumeration with values read from external source and filtering."""

        def values(self, group):
            if group is None:
                return ()
            else:
                return [line.split(':')[3].strip() for line in open('/etc/group').readlines()
                        if line and line.startswith(group + ':')][0].split(',')

    def fields(self):
        return (
            Field('filter', _("Country name filter"), default='*',
                  descr=_("Enter a wildcard expression to filter only matching countries in the "
                          "codebook field below.")),
            Field('switch', _("Show only european countries"), editable=Editable.ALWAYS,
                  type=pd.Boolean(), default=True,
                  descr=_("Check/uncheck this checkbox to enable/disable non-european "
                          "countries in the codebook field below.")),
            Field('country', _("Country"), width=2,
                  type=pd.String, not_null=True, maxlen=2,
                  codebook='cb.Countries',
                  runtime_filter=computer(self._country_filter),
                  descr=_("Lists countries filtered by the above two fields.")),
            Field('group', _("Group"),
                  enumerator=self._SystemGroups(), not_null=True,
                  descr=_("Lists system user groups.")),
            Field('user', _("User"),
                  enumerator=self._SystemGroupMembers(), not_null=True,
                  runtime_arguments=computer(lambda record, group: dict(group=group)),
                  descr=_("Lists users of the above selected group.")),
        )

    def _country_filter(self, row, filter, switch):
        """Return the validity condition as pd.Operator instance."""
        cond = pd.WM('name', pd.WMValue(pd.String(), filter or '*'))
        if switch:
            cond = pd.AND(cond, pd.EQ('continent', pd.Value(pd.String(), 'EU')))
        return cond


class Passwords(Specification):
    """Simple test of password fields.

    The form uses plain text passwords as well as MD5 passwords.  The input
    field contains two text entry widgets.  You must fill the same value into
    both of them to be able to set/change the password.  Empty password is
    indicated by an empty field.  When the password contains a value, the
    control shows eight positions, regardless how long the actual password is,
    just as an indication that the value is not empty.

    In practice, passwords will not be stored directly within the database.
    They will be typically protected by hashing.  This may be done by creating
    a virtual password field for the user interaction and a computed database
    fiels which generates the hashed value based on the UI field's plain text
    value.  This demo shpould be extended to give an example.

    """
    public = True
    table = dbdefs.Passwords
    layout = ('name', 'passwd')

    def _customize_fields(self, fields):
        fields.modify('id', fixed=True, editable=Editable.NEVER,
                      default=nextval('passwords_id_seq'))
        fields.modify('passwd', type=pd.Password(minlen=4, maxlen=16, verify=True),
                      disable_column=True),


class Products(Specification):
    public = True
    table = dbdefs.Products
    title = _("Products")
    layout = TabGroup((_("Product"),
                       ('product_id', 'product', 'count', 'price', 'marked')),
                      (_("Notes"),
                       ('since', 'notes')))
    grouping_functions = (
        ('f_date_year', _("Year"), pd.DateTime, pd.Integer()),
        ('f_date_month', _("Month"), pd.DateTime, pd.Integer()),
    )
    profiles = (
        pp.Profile('marked', _("Marked"), filter=pd.EQ('marked', pd.bval(True))),
        pp.Profile('unmarked', _("Unmarked"), filter=pd.EQ('marked', pd.bval(False))),
    )

    def _customize_fields(self, fields):
        fields.set_property('width', product_id=3, product=30, count=12, price=12, notes=50)
        fields.modify('product_id', column_width=6, fixed=True, editable=Editable.ALWAYS)
        fields.modify('product', style=lambda r: (pp.Style(overstrike=True)
                                                  if r['count'].value() == 0
                                                  else None))
        fields.modify('count', style=lambda r: (pp.Style(foreground='#f00')
                                                if r['count'].value() <= 5
                                                else None))
        fields.modify('price', type=pd.Monetary(not_null=True))
        fields.modify('since', descr=_("Date when the product was first included."),
                      default=pd.DateTime.datetime)
        fields.modify('notes', descr=_("Arbitrary internal notes about given product."),
                      height=3, text_format=pp.TextFormat.LCG)

    def prints(self):
        return (
            PrintAction('product-page', _("Product Page"), 'Products.ProductPage',
                        handler=self._print_handler),
            PrintAction('product-info', _("Product Info"), 'Products.ProductInfo'),
        )

    def _print_handler(self, row):
        """Demonstration of using PrintAction handler.

        The output document is created by merging the output of two printout
        results.

        """
        from PyPDF2 import PdfFileMerger, PdfFileReader
        merger = PdfFileMerger()
        for lang in ('cs', 'en'):
            output = io.BytesIO()
            pytis.form.printout(
                'Products', 'Products.ProductPage',
                parameters=dict([(k, row[k].export()) for k in ('product_id', 'product', 'price')],
                                language=lang),
                language=lang, output_file=output,
            )
            merger.append(PdfFileReader(io.BytesIO(output.getvalue())))
        with tempfile.NamedTemporaryFile(suffix='.pdf') as f:
            merger.write(f)
            f.flush()
            os.fsync(f)
            pytis.form.launch_file(f.name)
            time.sleep(1)

    def _content(self, record):
        par = os.path.pardir
        return pytis.util.lcg_node(
            (
                lcg.fieldset((
                    (_("Price"), record['price'].export()),
                    (_("Available since"), lcg.LocalizableDateTime(record['since'].value())),
                )),
                lcg.sec(_("Notes"), lcg.Parser().parse(record['notes'].value()), name='notes')
                if record['notes'].value() else lcg.Content(),
            ),
            title=record['product'].value(),
            resource_path=(os.path.normpath(os.path.join(__file__, par, par, par, par, 'css')),),
            resources=('pytis-demo-product.css',),
        )

    def bindings(self):
        return (
            Binding('webform', _("Product Info"), content=self._content,
                    descr=_("Shows web page generated from LCG structured text.")),
        )

    def actions(self):
        return (Action('toggle', _("Mark/unmark"), self._mark, hotkey='m'),
                Action('mark_all', _("Mark all"), self._mark, hotkey='Ctrl-a', mark_all=True),
                Action('unmark_all', _("Unmark all"), self._mark, hotkey='Ctrl-u', mark_all=True,
                       mark=False),
                Action('mark_selected', _("Mark selected"), self._mark_selected,
                       context=pp.ActionContext.SELECTION),
                Action('prices', _("Update prices"), self._update_prices),
                Action('print', _("Print price"), self._print),
                )

    def row_style(self, row):
        if app.form:
            # TODO: App.form returns None during form initialization, because
            # the application knows about it only after it is initialized and
            # put onto the stack.  This should be fixed within app.form.
            min_count = app.form.query_fields.row['min_count'].value()
        else:
            min_count = 10
        if row['count'].value() <= min_count:
            return pp.Style(background='#fdd')
        elif row['marked'].value():
            return pp.Style(background='#ffd')
        else:
            return None

    query_fields = QueryFields(
        (Field('min_count', _("Highlight low count"),
               type=pytis.data.Integer(not_null=True), default=10),),
        autoinit=True,
    )

    def _mark(self, row, mark_all=False, mark=True):
        if mark_all:
            product_id = None
        else:
            product_id = row["product_id"].value()
            mark = not row["marked"].value()
        count = pytis.form.run_procedure('misc', 'mark_products', product_id=product_id, mark=mark)
        pytis.form.message(_("Marked %d rows") % count)

    def _mark_selected(self, rows):
        count = pytis.form.run_procedure('misc', 'mark_products',
                                         product_id=[r['product_id'].value() for r in rows],
                                         mark=True)
        pytis.form.message(_("Marked %d rows") % count)

    def _update_prices(self, row):
        # This serves for testing user transactions.
        true_value = pd.Value(pd.Boolean(), True)
        condition = pd.EQ('marked', true_value)
        transaction = pd.transaction()
        try:
            def process(row):
                return row['product_id']
            product_ids = row.data().select_map(process, condition=condition)
            for product_id in product_ids:
                if not pytis.form.run_form(pytis.form.PopupEditForm, 'misc.Products',
                                           select_row=product_id,
                                           transaction=transaction):
                    pytis.form.run_dialog(pytis.form.Message, "Transaction aborted",
                                          title="Error", icon=pytis.form.Message.ICON_ERROR)
                    transaction.rollback()
                    return
        except Exception:
            transaction.rollback()
            raise
        transaction.commit()

    def _print(self, row):
        pytis.form.printout('misc.Products', 'misc.StandalonePrint', row)


class ProductInfo(PrintSpecification):

    def init(self):
        self._row = self._parameter((self._parameter(pytis.output.P_NAME), pytis.output.P_ROW))
        return True

    def page_header(self):
        return pytis.output.HGroup(
            self._row['product'].value(),
            pytis.output.HSpace(None),
            lcg.LocalizableDateTime(datetime.date.today()),
        )

    def body(self):
        return pytis.output.StructuredText(self._row['notes'].value() or _("-"))


class XProducts(Specification):
    # Just for DMP testing
    public = True
    title = _("XProducts")
    table = dbdefs.Products

    def _customize_fields(self, fields):
        fields.append(Field('price2', _("Price with VAT"), virtual=True,
                            type=pd.Float(precision=2),
                            computer=computer(lambda r, price: price and price * 1.21)))
        fields.set_property('width', product_id=3, product=30, count=12, price=12, price2=12,
                            notes=50)
        fields.modify('product_id', column_width=6, fixed=True)
        fields.modify('product', style=lambda r: (pp.Style(overstrike=True)
                                                  if r['count'].value() == 0
                                                  else None))
        fields.modify('count', style=lambda r: (pp.Style(foreground='#f00')
                                                if r['count'].value() <= 5
                                                else None))
        fields.modify('price', type=pd.Monetary(not_null=True))
        fields.modify('since', descr=_("Date when the product was first included."))
        fields.modify('notes', descr=_("Arbitrary internal notes about given product."),
                      height=3, text_format=pp.TextFormat.LCG)


"""Definition of reusable procedures.

Procedures are independent of the specifications they work on, so they can be called by name
from anywhere within the application or even externally from standalone python scripts.

The pytis built-in function 'run_procedure()' is used to invoke a procedure, pass it arguments
and retrieve the result (however procedures are typically called for their side effects).  See
the documentation of 'run_procedure()' for more detail.

"""


@procedure
def mark_products(product_id=None, mark=True):
    """Mark/unmark all products."""
    assert isinstance(mark, bool)
    assert product_id is None or isinstance(product_id, (int, tuple, list))
    row = pd.Row((('marked', pd.bval(mark)),))
    if product_id is None:
        condition = pd.EQ('marked', pd.bval(not mark))
    elif isinstance(product_id, (list, tuple)):
        condition = pd.OR(*[pd.EQ('product_id', pd.ival(x)) for x in product_id])
    else:
        condition = pd.EQ('product_id', pd.ival(product_id))
    import pytis.extensions
    return pytis.extensions.dbupdate_many('misc.Products', condition=condition, update_row=row)


# TreeOrder is dead, long live LTree! (see below)
class _TreeOrder(pp.PrettyTreeOrder, pd.Name):
    pass


class ObsoleteTree(Specification):
    """Demonstration of a foldable form."""
    public = True
    title = _("Tree")
    table = dbdefs.Tree
    fields = (
        Field('id', _("ID")),
        Field('tid', _("Tree ID")),
        Field('name', _("Name"), type=_TreeOrder(tree_column_id='tid')),
        Field('amount', _("Amount"), fixed=True),
        Field('description', _("Description")),
    )
    layout = ('id', 'name', 'amount', 'description',)
    columns = ('name', 'amount', 'description',)
    sorting = (('id', pd.ASCENDENT,),)
    aggregations = (pd.Data.AGG_SUM,)


class _TreeOrderLTree(pp.PrettyFoldable, pd.Name):
    pass


class Tree(Specification):
    """Demonstration of a foldable form."""
    public = True
    title = _("Foldable Tree")
    table = dbdefs.Tree
    layout = ('id', 'name', 'amount', 'description',)
    columns = ('name', 'amount', 'description',)
    sorting = (('id', pd.ASCENDENT,),)
    aggregations = (pd.Data.AGG_SUM,)

    def _customize_fields(self, fields):
        fields.set_property('label', id=_("ID"), name=_("Name"), amount=_("Amount"),
                            description=_("Description"))
        fields.modify('name', type=_TreeOrderLTree(tree_column_id='id',
                                                   subcount_column_id='id_nsub'))
        fields.modify('amount', fixed=True)


class LongTable(Specification):
    """Table containing a lot of rows."""
    public = True
    title = _("Long Table")
    table = dbdefs.Longtable
    sorting = (('id', pd.ASCENDENT,),)

    def _customize_fields(self, fields):
        fields.modify('id', fixed=True, editable=Editable.NEVER,
                      default=nextval('longtable_id_seq'))


class SlowLongTable(LongTable):
    """Read-only long table with slow default sorting.

    Useful for some testing purposes, not for real applications. :-)

    """
    public = True
    title = _("Slow Long Table")
    table = dbdefs.Slowlongtable
    sorting = (('value', pd.ASCENDENT,),)

    def _customize_fields(self, fields):
        fields.modify('value', style=lambda r: (pp.Style(overstrike=True)
                                                if (r['id'].value() ^ 4) % 2 == 0
                                                else None))

    def row_style(self, row):
        return (row['id'].value() ^ 3) % 2 and pp.Style(background='#ffd') or None


class FastLongTable(LongTable):
    """Read-only long table with faster default sorting."""
    public = True
    title = _("Fast Long Table")
    table = dbdefs.Fastlongtable
    sorting = (('value', pd.ASCENDENT,),)


class Files(Specification):
    """Files and URLs."""
    public = True
    title = _("Files and URLs")
    table = dbdefs.Files

    def _customize_fields(self, fields):
        fields.modify('id', fixed=True)

    def actions(self):
        return (Action('a_open_file', _("Open file"), self._open_file),
                Action('a_open_url', _("Open URL"), self._open_url),
                )

    def _open_file(self, row):
        pytis.form.launch_file(row['file'].value())

    def _open_url(self, row):
        import pytis.windows
        url = row['url'].value()
        pytis.windows.launch_url(url)


class StandalonePrint(pytis.output.PrintSpecification):

    def page_header(self):
        return lcg.coerce('Header Text')

    def page_footer(self):
        return lcg.coerce('Footer Text')

    def page_background(self):
        return pytis.output.Image('bg.tiff', standalone=False)

    def body(self):
        spec = self._parameter(pytis.output.P_NAME)
        row = self._parameter((spec, pytis.output.P_ROW,))
        p = pytis.output.Paragraph(u"Price of %s is %s." %
                                   (row['product'].value(),
                                    row['price'].value(),))
        presentation = lcg.Presentation()
        presentation.noindent = True
        doc = pytis.output.Document(pytis.output.Group(pytis.output.VSpace(10), p, vertical=True),
                                    page_header=self.page_header(),
                                    page_footer=self.page_footer(),
                                    page_background=self.page_background(),
                                    presentation={None: presentation})
        return [doc]


class RandomNumbers(Specification):
    """Demonstration of query fields and argument provider."""
    public = True
    title = _("Random numbers controlled by query fields")
    table = dbdefs.RandomNumbers
    query_fields = QueryFields(
        (Field('count', _("Row Count"), type=pytis.data.Integer(not_null=True), default=10),
         Field('minimum', _("Min"), type=pytis.data.Integer(not_null=True), default=0),
         Field('maximum', _("Max"), type=pytis.data.Integer(not_null=True), default=100,
               check=lambda r, minimum, maximum: (_("Max must be greater than Min!")
                                                  if maximum < minimum else None))),
        autoinit=True,
    )

    def argument_provider(self, query_fields, **kwargs):
        return dict([(k, query_fields[k]) for k in ('count', 'minimum', 'maximum')])


class RangeTypes(Specification):
    public = True
    table = dbdefs.RangeTypes
    title = _("RangeTypes")
    layout = ('int_range', 'date_range', 'datetime_range')

    def _customize_fields(self, fields):
        fields.modify('range_id', default=nextval('range_types_range_id_seq'))
        fields.modify('int_range', width=4)
