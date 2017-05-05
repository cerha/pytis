/* -*- coding: utf-8 -*-
 *
 * Copyright (C) 2009-2016 Brailcom, o.p.s.
 * Author: Tomas Cerha
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

/* This script implements pytis form updates through AJAX.
 *
 * The form is periodically checked for changes and the values are sent to the
 * server as asynchronous requests.  Based on the server response, the user
 * interface is then capable of:
 *   - displaying field validation info,
 *   - updating values of computed fields,
 *   - updating field editability dynamically,
 *   - updating enumerations based on pytis runtime filters and arguments.
 */
/*jshint browser: true */
/*jshint es3: true */
/*jshint -W097 */ // allow direct "use strict"
/*global Effect */
/*global Class */
/*global Element */
/*global Ajax */
/*global Hash */
/*global Gettext */
/*global Form */
/*global lcg */
/*global $ */
/*global $$ */
/*global $w */
/*global $F */
/*global $break */
/*global CKEDITOR */

"use strict";

var pytis = {

    gettext: new Gettext({domain: 'pytis-web'}),

    _: function (msg) {
        return pytis.gettext.gettext(msg);
    },

    show_tooltip: function (event, uri) {
        // This can't be implemented as a Field method as Field instances are not
        // created in BrowseForm (currently only in edit form).
        var element, tooltip;
        if (event) {
            element = event.target;
        } else {
            event = window.event;
            element = event.srcElement;
        }
        element._pytis_tooltip_timeout = setTimeout(function () {
            if (element._pytis_tooltip) {
                tooltip = element._pytis_tooltip;
            } else {
                tooltip = new lcg.Tooltip(uri);
                element._pytis_tooltip = tooltip;
            }
            tooltip.show(event.pointerX(), event.pointerY());
        }, 500);
    },

    hide_tooltip: function (element) {
        if (element._pytis_tooltip) {
            element._pytis_tooltip.hide();
        }
        if (element._pytis_tooltip_timeout) {
            clearTimeout(element._pytis_tooltip_timeout);
        }
    }

};

pytis.BrowseForm = Class.create({
    /* Handles asynchronous load of a Pytis browse form.

    The form content is loaded through an AJAX request after the page is
    displayed.  Special handling is applied to forms which are placed inside
    notebook tabs (of lcg.Notebook widget).  Such forms are not loaded
    immediately, but in a callback after the tab is activated.

    The instance of this class can be accessed by other JavaScript code through
    the form's DOM element.  It is automatically attached to the form's top
    level element as it's 'instance' attribute, so in Prototype.js, you can
    access it as '$(form_id).instance'.

     */
    initialize: function (form_id, form_name, uri, allow_insertion) {
        /* form_id ... HTML id of the pytis form top level element (string)
           form_name ... Form name used for distinguishing request parameters
             (see form_name in the python class).
           uri ... URI for AJAX requests
         */
        this.form = $(form_id);
        this.form.instance = this;
        this.form_name = form_name;
        this.uri = uri;
        this.ajax_container = this.form.down('.ajax-container');
        this.on_load_callbacks = [];
        if (this.ajax_container && uri) {
            this.async_load = true;
            var parameters = {};
            var query = window.location.search.replace(/;/g, '&').parseQuery();
            if (query.form_name === form_name) {
                parameters = query;
            }
            var page = this.form.up('.notebook-widget > div');
            if (page) {
                lcg.Notebook.on_activation(page, function () {
                    this.load_form_data(parameters);
                }.bind(this));
            } else {
                this.load_form_data(parameters);
            }
        } else {
            this.async_load = false;
            this.bind_search_controls(this.form.down('.list-form-controls', 0));
            this.bind_search_controls(this.form.down('.list-form-controls', 1));
            this.bind_table_headings(this.form.down('table.data-table thead'));
            this.bind_table_body(this.form.down('table.data-table tbody'));
        }
        if (allow_insertion) {
            var insert_button = new Element('button', {'class': 'new-row-button'});
            var insert_label = new Element('span').update(pytis._("New row"));
            insert_button.update(insert_label);
            insert_button.observe('click', this.on_insert_new_row.bind(this));
            this.form.insert(insert_button);
        }
    },

    send_ajax_request: function (form, parameters, on_success) {
        document.body.style.cursor = "wait";
        if (!form) {
            // If no form is passed, we simply create one just for the request.
            form = new Element('form', {action: this.uri, method: 'GET'});
        }
        form.request({
            parameters: parameters,
            onSuccess: function (transport) {
                try {
                    on_success.bind(this)(transport);
                } catch (e) {
                    // Errors in asynchronous handlers are otherwise silently
                    // ignored.  This will only work in some browsers,
                    // but it is only useful for debugging anyway...
                    console.log(e);
                } finally {
                    document.body.style.cursor = "default";
                }
            }.bind(this),
            onFailure: function (transport) {
                document.body.style.cursor = "default";
            }.bind(this)
        });
    },

    on_edit_cell: function (event) {
        var td = (event.element().nodeName === 'TD' ? event.element() : event.element().up('td'));
        this.send_edit_cell_request(td);
        event.stop();
    },

    send_edit_cell_request: function (element) {
        var parameters = {_pytis_form_update_request: 1,
                          _pytis_edit_cell: 1,
                          _pytis_row_key: this.pytis_row_key(element),
                          _pytis_column_id: this.pytis_column_id(element)};
        // Note, element.down('form') will not exist on first invocation, but the method
        // send_ajax_request can handle that.
        this.send_ajax_request(element.down('form'), parameters, function (transport) {
            element.update(transport.responseText);
            var edit_form = element.down('form');
            if (edit_form) {
                edit_form.down('button.save-edited-cell').on('click',  function (event) {
                    this.send_edit_cell_request(element);
                    event.stop();
                }.bind(this));
                edit_form[this.pytis_column_id(element)].focus();
            }
        }.bind(this));
    },

    slide_down: function (element) {
        if (Effect !== undefined) {
            Effect.SlideDown(element, {duration: 0.25});
        } else {
            element.show();
        }
    },

    slide_up: function (element) {
        if (Effect !== undefined) {
            Effect.SlideUp(element, {duration: 0.25});
        } else {
            element.show();
        }
    },

    on_toggle_row_expansion: function (event) {
        var tr = event.element().up('tr');
        var expansion;
        if (tr.hasClassName('row-expansion')) {
            expansion = tr;
            tr = expansion.previousSiblings()[0];
        } else {
            expansion = tr.nextSiblings()[0];
        }
        var link = tr.down('a.expand-row');
        if (tr.hasClassName('expanded')) {
            tr.removeClassName('expanded');
            this.slide_up(expansion.down('.row-expansion-content'));
            link.focus();
            setTimeout(function () {
                expansion.hide();
                link.setAttribute('aria-expanded', 'false');
                link.update(pytis._("Expand Row"));
            }, 250);
        } else {
            tr.addClassName('expanded');
            if (expansion && expansion.hasClassName('row-expansion')) {
                var content = expansion.down('.row-expansion-content');
                expansion.setAttribute('style', '');
                content.setAttribute('style', '');
                expansion.show();
                this.slide_down(content);
            } else {
                this.send_expand_row_request(tr);
            }
            setTimeout(function () {
                link.setAttribute('aria-expanded', 'true');
                link.update(pytis._("Collapse Row"));
            }, 250);
            event.stop();
        }
    },

    send_expand_row_request: function (tr) {
        var parameters = {_pytis_form_update_request: 1,
                          _pytis_expand_row: 1,
                          _pytis_row_key: this.pytis_row_key(tr)};
        this.send_ajax_request(this.form.down('form'), parameters, function (transport) {
            var content = new Element('div', {'class': 'row-expansion-content'});
            content.insert(transport.responseText);
            var collapse_ctrl = new Element('a', {'class': 'collapse-row'});
            collapse_ctrl.update(pytis._("Collapse Row"));
            collapse_ctrl.on('click', this.on_toggle_row_expansion.bind(this));
            content.insert(collapse_ctrl);
            content.hide();
            tr.insert({after: new Element('tr', {'class': 'row-expansion'}).insert(
                new Element('td', {'colspan': tr.childElements().length}).insert(content))});
            this.slide_down(content);
        }.bind(this));
    },

    pytis_row_key: function (element) {
        // Return pytis row key value for given HTML element inside the pytis table form.
        // Returns null if the element is not inside a pytis table or
        // if the table doesn't contain necessary information.
        var tr = (element.nodeName === 'TR' ? element : element.up('tr'));
        if (tr) {
            return tr.getAttribute('data-pytis-row-key');
        }
        return null;
    },

    pytis_column_id: function (element) {
        // Return pytis column id for given HTML element inside the pytis table form.
        /* The method works with with class names of table th elements, but this
           may be unreliable in some cases, so better we might pass column names
           explicitly to the JavaScript form class constructor or something
           similar...
        */
        var td = (element.nodeName === 'TD' ? element : element.up('td'));
        var tr = element.up('tr');
        var table = element.up('table');
        if (td && tr && table) {
            var thead = table.down('thead');
            if (thead) {
                var hr = thead.down('tr');
                if (hr) {
                    var th = hr.childElements()[tr.childElements().indexOf(td)];
                    if (th) {
                        var cls = th.classNames().find(function (x) {
                            return x.match('^column-id-');
                        });
                        if (cls) {
                            return cls.substr(10, cls.length);
                        }
                    }
                }
            }
        }
        return null;
    },

    load_form_data: function (parameters, busy_cursor) {
        if (busy_cursor) {
            document.body.style.cursor = "wait";
        }
        parameters._pytis_async_load_request = 1;
        new Ajax.Request(this.uri, {
            method: 'get',
            parameters: parameters,
            onSuccess: function (transport) {
                try {
                    var container = this.ajax_container;
                    var i, callback;
                    container.update(transport.responseText);
                    this.bind_controls(container.down('.list-form-controls', 0));
                    this.bind_controls(container.down('.list-form-controls', 1));
                    this.bind_table_headings(container.down('table.data-table thead'));
                    this.bind_table_body(container.down('table.data-table tbody'));
                    for (i=0; i<this.on_load_callbacks.length; i++) {
                        callback = this.on_load_callbacks[i];
                        callback(this.form);
                    }
                    if (container.down('#found-record')) {
                        window.location.hash = '#found-record';
                    }
                } catch (e) {
                    // Errors in asynchronous handlers are otherwise silently
                    // ignored.  This will only work in some browsers,
                    // but it is only useful for debugging anyway...
                    console.log(e);
                } finally {
                    if (busy_cursor) {
                        document.body.style.cursor = "default";
                    }
                }
            }.bind(this),
            onFailure: function (transport) {
                if (busy_cursor) {
                    document.body.style.cursor = "default";
                }
                var msg = (pytis._("Failed loading form:") +' '+
                           transport.status +' '+ transport.statusText);
                var tb_start = transport.responseText.search('here is the original traceback:');
                var traceback = '';
                if (tb_start !== -1) {
                    var tb_len = transport.responseText.slice(tb_start).search('-->');
                    traceback = ('<pre class="form-load-traceback" style="display: none">' +
                                     transport.responseText.slice(tb_start+32, tb_start+tb_len-3) +
                                     '</pre>');
                    msg += (' (<a href="#" onclick="' +
                            'this.parentNode.parentNode.down(\'.form-load-traceback\').toggle();' +
                            '">' + pytis._("show details") + '</a>)');
                }
                this.ajax_container.update('<div class="form-load-error">' + msg + '</div>' +
                                           traceback);
            }.bind(this)
        });
    },

    bind_controls: function (panel) {
        if (panel) {
            panel.select('button.prev-page, button.next-page').each(function (ctrl) {
                ctrl.observe('click', function (event) {
                    this.reload_form_data(ctrl);
                    event.stop();
                }.bind(this));
            }.bind(this));
            var apply_button = panel.down('button.apply-filters');
            if (apply_button) {
                apply_button.observe('click', function (event) {
                    this.reload_form_data(apply_button);
                    event.stop();
                }.bind(this));
            } else {
                panel.select('select, checkbox, radio').each(function (ctrl) {
                    ctrl.observe('change', function (event) {
                        this.reload_form_data(ctrl);
                        event.stop();
                    }.bind(this));
                }.bind(this));
            }
            panel.select('.index-search-controls a').each(function (ctrl) {
                var params = ctrl.href.replace(/;/g, '&').parseQuery();
                ctrl.observe('click', function (event) {
                    this.reload_form_data(ctrl, {index_search: params.index_search,
                                                 sort: params.sort,
                                                 dir: params.dir});
                    event.stop();
                }.bind(this));
            }.bind(this));
            this.bind_search_controls(panel);
        }
    },

    bind_table_headings: function (thead) {
        if (thead) {
            thead.select('th.column-heading').each(function (th) {
                if (th.hasClassName('sortable-column')) {
                    th.observe('click', this.on_table_heading_clicked.bind(this));
                }
            }.bind(this));
        }
        if (thead && thead.up('table').hasClassName('expansible-rows')) {
            var th = new Element('th', {'class': 'column-heading expansion-ctrl-heading'});
            thead.down('tr').insert({top: th});
        }
    },

    bind_table_body: function (tbody) {
        if (tbody) {
            tbody.select('td.editable-cell').each(function (element) {
                element.setAttribute('title', pytis._("Double click the cell to edit the value."));
                element.on('dblclick', this.on_edit_cell.bind(this));
            }.bind(this));
            var expansible_rows = tbody.up('table').hasClassName('expansible-rows');
            tbody.select('tr').each(function (tr) {
                var remove_row = tr.down('a.remove-row');
                if (remove_row) {
                    remove_row.on('click', this.on_remove_row.bind(this));
                }
                if (expansible_rows) {
                    var ctrl = new Element('a', {'class': 'expand-row'});
                    ctrl.update(pytis._("Expand Row"));
                    ctrl.on('click', this.on_toggle_row_expansion.bind(this));
                    ctrl.update(pytis._("Expand Row"));
                    tr.insert({top: new Element('td', {'class': 'expansion-ctrl'}).update(ctrl)});
                }
            }.bind(this));
        }
    },

    reload_form_data: function (ctrl, params) {
        var parameters = (params !== undefined ? params : {});
        ctrl.up('form').getElements().each(function (x) {
            if (x.tagName !== 'BUTTON') {
                parameters[x.name] = x.value;
            }
        });
        if (ctrl.name) {
            // Some buttons (apply-filters) have no value, others (prev-page/next-page) do...
            parameters[ctrl.name] = ctrl.value;
        }
        this.ajax_container.select('form.list-form-controls').each(function (f) { f.disable(); });
        this.load_form_data(parameters, true);
    },

    bind_search_controls: function (panel) {
        if (panel) {
            var search_button = panel.down('.paging-controls button.search');
            if (search_button) {
                search_button.observe('click', this.on_show_search_controls.bind(this));
            }
            var cancel_button = panel.down('div.query button.cancel-search');
            if (cancel_button) {
                cancel_button.observe('click', this.on_hide_search_controls.bind(this));
            }
        }
    },

    on_table_heading_clicked: function (event) {
        var th = event.element();
        if (th.nodeName !== 'TH') {
            th = th.up('th');
        }
        var colid_cls = $w(th.className).find(function (x) {
            return x.startsWith('column-id-');
        });
        if (colid_cls) {
            var column_id = colid_cls.substring(10);
            var dir = 'asc';
            if (th.down('.sort-direction-asc')) {
                dir='desc';
            }
            if (th.down('.sort-direction-desc')) {
                dir='';
            }
            var parameters = {form_name: this.form_name, sort: column_id, dir: dir};
            if (this.async_load) {
                this.form.select('form.list-form-controls').each(function (f) { f.disable(); });
                this.load_form_data(parameters, true);
            } else {
                window.location.search = new Hash(parameters).toQueryString();
            }
            event.stop();
        }
    },

    on_show_search_controls: function (event) {
        var search_controls = $(this.form).down('div.query');
        var i, panel, button;
        search_controls.show();
        search_controls.down('input.text-search-field').focus();
        search_controls.down('input[type=hidden]').value = '1';
        for (i=0; i<2; i++) {
            panel = this.form.down('.list-form-controls', i);
            if (panel) {
                button = panel.down('.paging-controls button.search');
                if (button) {
                    button.hide();
                }
            }
        }
        event.stop();
    },

    on_hide_search_controls: function (event) {
        var search_controls = $(this.form).down('div.query');
        var form = search_controls.up('form');
        form['show-search-field'].value = '';
        form.query.value = '';
        form.submit();
        event.stop();
    },

    on_insert_new_row: function (event) {
        var form = this.form.down('form');
        if (!form) {
            form = this.form.up('form');
        }
        var parameters = {'_pytis_form_update_request': 1,
                          '_pytis_insert_new_row': 1};
        if (form) {
            this.send_ajax_request(form, parameters, function (transport) {
                var tbody = this.form.down('table.data-table tbody');
                tbody.insert(transport.responseText);
                form['_pytis_inserted_rows_' + this.form_name].value++;
                this.bind_table_body(tbody);
            });
        }
        event.stop();
    },

    on_remove_row: function (event) {
        var tr = event.element().up('tr');
        tr.up('form').insert(new Element('input', {type: 'hidden',
                                                   name: '_pytis_removed_row_key_' + this.form_name,
                                                   value: tr.getAttribute('data-pytis-row-key')}));
        tr.remove();
        event.stop();
    },

    on_load: function (callback) {
        /* Call given callback function when the form is fully loaded.

           The callback will be called immediately if the form is loaded
           synchronously or after the asynchronous load in the other case.

           The callback function will receive the form's top level DOM element
           as an argument.

         */
        if (this.async_load) {
            this.on_load_callbacks[this.on_load_callbacks.length] = callback;
        } else {
            callback(this.form);
        }
    }
});

pytis.Form = Class.create({
    initialize: function (form_id, fields) {
        /* form_id ... HTML id of the pytis form top level element (string)
         * fields ... array of form fields as pytis.Field instances
         */
        var form = $(form_id).down('form') || $(form_id).up('form');
        this._form = form;
        this._fields = new Hash();
        var observe = false;
        var i, field;
        for (i=0; i<fields.length; i++) {
            field = fields[i];
            this._fields.set(field.id(), field);
            if (field.active()) {
                observe = true;
            }
        }
        this._last_request_number = 0;
        if (observe) {
            this._observer = new Form.Observer(form, 1, this.on_change.bind(this));
        }
    },

    on_change: function (form, value) {
        // Send AJAX request in reaction to user changes of form values.
        // TODO: Avoid AJAX request flooding during typing or other continuous
        // changes.  The problem is that we must always send a request as we
        // don't know whether another change comes soon.  Maybe we could send
        // the request after some delay if no other change comes in the
        // meantime, but this would slow down the UI responsivity.
        var values = value.parseQuery();
        var last_values = this._observer.lastValue.parseQuery();
        this._fields.each(function (item) {
            var id = item.key;
            var field = item.value;
            // Disabled fields are not present in values/last_values, but also
            // checkbox fields are not there if unchecked.
            if ((field.active() &&
                 (values.hasOwnProperty(id) || last_values.hasOwnProperty(id)) &&
                 values[id] !== last_values[id])) {
                document.body.style.cursor = "wait";
                var states = new Hash();
                this._fields.each(function (x) {
                    if (x.value.state()) {
                        states.set(x.key, x.value.state());
                    }
                });
                this._form.request({
                    parameters: {_pytis_form_update_request: ++this._last_request_number,
                                 _pytis_form_changed_field: id,
                                 _pytis_form_state: states ? states.toQueryString() : null},
                    onSuccess: this.update.bind(this)
                });
                throw $break;
            }
        }.bind(this));
    },

    update: function (response) {
        // Update the form state in reaction to previously sent AJAX request.
        var data = response.responseJSON;
        if (data) {
            var response_number = parseInt(data.request_number, 10);
            // Ignore the response if other requests were sent in the meantime.
            // Only the most recently sent request really corresponds to the
            // current form state!  This also prevents processing responses
            // coming in wrong order (earlier request may be processed longer
            // than a later one).
            if (response_number === this._last_request_number) {
                new Hash(data.fields).each(function (item) {
                    var field = this._fields.get(item.key);
                    if (field) {
                        var cdata = item.value;
                        try {
                            if (cdata.enumeration !== undefined) {
                                var original_value = field.value();
                                field.set_enumeration(cdata.enumeration, cdata.links);
                                if (cdata.value === undefined) {
                                    // Retain the value of enumeration fields
                                    // after enumeration changes if possible.
                                    field.set_value(original_value);
                                }
                            }
                            if (cdata.value !== undefined) {
                                field.set_value(cdata.value, cdata.localized_value);
                            }
                            if (cdata.editable !== undefined) {
                                field.set_editability(cdata.editable);
                            }
                            if (cdata.state !== undefined) {
                                field.set_state(cdata.state);
                            }
                        } catch (e) {
                            console.log(e);
                        }
                    }
                }.bind(this));
            }
            if (response_number === this._last_request_number) {
                document.body.style.cursor = "default";
            }
        } else {
            console.log("Empty AJAX response");
        }
    }

});

pytis.Field = Class.create({
    initialize: function (form_id, field_id, id, state, active, required) {
        /* form_id ... HTML id of the pytis form element to which the field
         *    belongs.
         * field_id ... HTML id of the form field element.  The element is
         *    usually the form control directly, but may be also a top level
         *    element (div) for compound fields, such as radio group or
         *    checklist.
         * id ... pytis field id (string) typically used as form control
         *    'name'.
         * state ... string representation of runtime filters and arguments
         *    initial state used for server side comparisons.
         * active ... boolean flag; True if this field may trigger changes of
         *    other fields.  The changes in this field will be observed and
         *    sent for server processing.
         * required ... boolean flag; True if this field is required (not null).
         */
        this._element = $(field_id);
        this._form = this._element.up('form');
        this._ctrl = this._form[id];
        this._id = id;
        this._state = state;
        this._active = active;
        if (required && this._ctrl && this._ctrl.length === undefined) {
            // TODO: handle aria-required also for compound fields (radio group, checklist).
            this._ctrl.setAttribute('aria-required', 'true');
        }
        // To allow accessing the field handler instance in pytis-ckeditor.js.
        this._element._pytis_field_instance = this;
    },

    id: function () {
        return this._id;
    },

    element: function () {
        return this._element;
    },

    state: function () {
        // Return the last runtime filter/arguments state string representation.
        return this._state;
    },

    active: function () {
        return this._active;
    },

    value: function () {
        return this._ctrl.value;
    },

    set_editability: function (value) {
        // Disable/enable field editability.
        var labels = $$('.field-label.id-'+this._id);
        if (labels) {
            var label = labels[0];
            if (label !== undefined) {
                if (value && label.hasClassName('disabled')) {
                    label.removeClassName('disabled');
                }
                if (!value && !label.hasClassName('disabled')) {
                    label.addClassName('disabled');
                }
            }
        }
        this._set_editability(value);
    },

    set_state: function (value) {
        // Set runtime filter/arguments state string representation.
        this._state = value;
    },

    _set_editability: function (value) {
        this._ctrl.disabled = !value;
    },

    set_value: function (value, localized_value) {
        // Set the field value.
        if (localized_value !== undefined) {
            this._ctrl.value = localized_value;
        } else {
            this._ctrl.value = value;
        }
    },

    set_enumeration: function (value, links) {
        // Update enumeration controls (only for enumeration fields).
        return;
    }

});

pytis.CheckboxField = Class.create(pytis.Field, {
    // Specific handler for a checkbox field.

    set_value: function (value, localized_value) {
        // Set the field value.
        this._ctrl.checked = value === 'T';
    }

});

pytis.RadioField = Class.create(pytis.Field, {
    // Specific handler for a radio button group.

    set_value: function (value, localized_value) {
        var i, radio;
        for (i=0; i<this._ctrl.length; i++) {
            radio = this._ctrl[i];
            radio.checked = radio.value === value;
        }
    },

    _set_editability: function (value) {
        var i;
        for (i=0; i<this._ctrl.length; i++) {
            this._ctrl[i].disabled = !value;
        }
    }

});

pytis.PasswordField = Class.create(pytis.Field, {
    // Specific handler a password field.

    set_value: function (value, localized_value) {
        var i;
        for (i=0; i<this._ctrl.length; i++) {
            this._ctrl[i].value = value;
        }
    },

    _set_editability: function ($super, value) {
        if (this._ctrl.length === undefined) {
            $super(value);
        } else {
            var i;
            for (i=0; i<this._ctrl.length; i++) {
                this._ctrl[i].disabled = !value;
            }
        }
    }

});

pytis.ChoiceField = Class.create(pytis.Field, {
    // Specific handler for codebook field represented by HTML select control.

    set_enumeration: function (value, links) {
        var options = this._ctrl.options;
        var selected = $F(this._ctrl);
        var i, len, option, item, attr, text;
        for (i=options.length-1; i>=0; --i) {
            //Remove all options except for the (first) NULL option (if present).
            option = $(options[i]);
            if (option.value !== '') {
                option.remove();
            }
        }
        this._ctrl.cleanWhitespace();
        for (i=0, len=value.length; i<len; ++i) {
            //Append options according to the new enumeration received;
            item = value[i];
            attr = {value: item[0], selected: item[0] === selected};
            text = item[1].escapeHTML().gsub(' ', '&nbsp;');
            option = new Element('option', attr).update(text);
            this._ctrl.insert(option);
        }
    }

});

pytis.ChecklistField = Class.create(pytis.Field, {
    // Specific handler for a multi select control represented by a group of checkboxes.

    _checkboxes: function () {
        return this._element.immediateDescendants().collect(function (item) {
            return item.firstDescendant();
        });
    },

    value: function () {
        var checkboxes = this._checkboxes();
        return checkboxes.map(function (checkbox) {
            return checkbox.value;
        });
    },

    set_value: function (value, localized_value) {
        this._checkboxes().each(function (checkbox) {
            checkbox.checked = false;
            if (value) {
                var j;
                for (j=0; j<value.length; j++) {
                    if (value[j] === checkbox.value) {
                        checkbox.checked = true;
                    }
                }
            }
        });
    },

    _set_editability: function (value) {
        this._checkboxes().each(function (checkbox) {
            checkbox.disabled = !value;
        });
    },

    set_enumeration: function (value, links) {
        var elem = this._element;
        var descendants = elem.immediateDescendants();
        var i, len, item, id, text, div, link;
        for (i=0; i<(descendants.length); i++) {
            $(descendants[i]).remove();
        }
        elem.cleanWhitespace();
        for (i=0, len=value.length; i<len; ++i) {
            //Append options according to the new enumeration received;
            item = value[i];
            id = elem.getAttribute('id') + '-' + i;
            text = (' ' + item[1]).escapeHTML().gsub(' ', '&nbsp;');
            div = new Element('div');
            div.insert(new Element('input', {'type': 'checkbox',
                                             'value': item[0],
                                             'name': this._id,
                                             'id': id}));
            div.insert(new Element('label', {'for': id}).update(text));
            link = (links ? links[item[0]] : null);
            if (link) {
                div.insert('&nbsp;[');
                div.insert(new Element('a', link).update(item[0]));
                div.insert(']');
            }
            elem.insert(div);
        }
    }

});

pytis.HtmlField = Class.create(pytis.Field, {
    initialize: function ($super, form_id, field_id, id, state, active, required) {
        $super(form_id, field_id, id, state, active, required);
        if (CKEDITOR !== undefined) {
            // The function pytis.HtmlField.plugin is defined in pytis-ckeditor.js.
            CKEDITOR.plugins.add('pytis-attachments', {init: pytis.HtmlField.plugin});
            CKEDITOR.on('dialogDefinition', pytis.HtmlField.on_dialog);
        }
    },

    _attachment_storage_request: function (request, parameters) {
        parameters._pytis_form_update_request = 1;
        parameters._pytis_attachment_storage_field = this._id;
        parameters._pytis_attachment_storage_request = request;
        var req = this._form.request({
            parameters: parameters,
            asynchronous: false
        });
        return req.transport.responseText.evalJSON();
    },

    get_attachment: function (filename) {
        return this._attachment_storage_request('get', {filename: filename});
    },

    list_attachments: function () {
        return this._attachment_storage_request('list', {});
    },

    // Insert attachment doesn't go through AJAX due to browser limitations.
    // Iframe upload is performed as implemented in CKeditor and
    // customized in ckeditor-plugin.js so a method is not needed here.

    update_attachment: function (filename, values) {
        return this._attachment_storage_request('update', {filename: filename,
                                                           values: Object.toJSON(values)});
    }
});

pytis.DateTimeField = Class.create(pytis.Field, {
    _set_editability: function (value) {
        this._ctrl.disabled = !value;
        var button = $(this._ctrl.id + '-button');
        button.disabled = !value;
    }
});

pytis.FileUploadField = Class.create(pytis.Field, {

    initialize: function ($super, form_id, field_id, id, state, active, required) {
        $super(form_id, field_id, id, state, active, required);
        // Observe file field changes separately as they are 1) ignored by
        // form.observe (by Prototype.js, not sure why) and 2) only "active"
        // fields are currently observed by pytis form updated (field is active
        // when computers depend on it).  When we ever decide to send ajax
        // updates for all fields (to support continuous validation) we can
        // solve file size validation within the main form updates.
        this._observer = new Form.Element.Observer(this._ctrl, 1, this.on_change.bind(this));
    },

    on_change: function (form, value) {
        if (this._ctrl.files && this._ctrl.files.length) {
            var file = this._ctrl.files[0];
            document.body.style.cursor = "wait";
            var parameters = {_pytis_form_update_request: true,
                              _pytis_form_changed_field: this._id};
            parameters['_pytis_file_size_'+this._id] = file.size;
            parameters[this._id] = null; // Avoid sending the whole file through ajax.
            this._form.request({parameters: parameters, onSuccess: this.update.bind(this)});
        }
    },

    update: function (response) {
        // Update the form state in reaction to previously sent AJAX request.
        var data = response.responseJSON;
        if (data) {
            var error = data.fields[this._id].error;
            var div = this._ctrl.next('.error');
            var submit = this._form.down('button[type="submit"]');
            if (error) {
                if (div) {
                    div = new Element('div', {'class': 'error'});
                    this._ctrl.insert({after: div});
                }
                div.update(error);
                if (submit) {
                    // Protect the server from invalid (most likely oversized) file uploads.
                    submit.disabled = true;
                }
            } else {
                if (div) {
                    div.remove();
                }
                if (submit) {
                    submit.disabled = false;
                }
            }
        }
        document.body.style.cursor = "default";
    }

});
