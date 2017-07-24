/* -*- coding: utf-8 -*-
 *
 * Copyright (C) 2009-2017 Brailcom, o.p.s.
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
    },

    on_success: function (callback) {
        // Errors in asynchronous handlers are otherwise silently
        // ignored.  This will only work in some browsers,
        // but it is only useful for debugging anyway...
        return function(transport) {
            try {
                callback(transport);
            } catch (e) {
                console.log(e);
            } finally {
                if (document.body.style.cursor === "wait") {
                document.body.style.cursor = "default";
                }
            }
        };
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
    initialize: function (form_id, form_name, uri, inline_editable) {
        /* form_id ... HTML id of the pytis form top level element (string)
           form_name ... Form name used for distinguishing request parameters
             (see form_name in the python class).
           uri ... URI for AJAX requests
           inline_editable ... perform basic operations inline.  Currently
             only the 'update' action is supported, but others, such as
             insert/delete may be supported in future too.  Inline means,
             that the operations are not performed on separate pages, but
             inside the current list form through asynchronous requests.
             For example 'update' will submit the action URI asynchronously
             and display the result returned by server inside the table
             row replacing the original row content.

         */
        this.form = $(form_id);
        this.form.instance = this;
        this.form_name = form_name;
        this.uri = uri;
        this.ajax_container = this.form.down('.ajax-container');
        this.on_load_callbacks = [];
        this.inline_editable = inline_editable;
        if (inline_editable) {
            this.form.select('button.action-insert').each(function (button) {
                button.on('click', function (event) {
                    button.disable();
                    this.send_inline_action_request(button, button.up('form'), {}, 'global');
                    event.stop();
                }.bind(this));
            }.bind(this));
        }
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
            this.apply(this.bind_search_controls, this.form.down('.list-form-controls', 0));
            this.apply(this.bind_search_controls, this.form.down('.list-form-controls', 1));
            this.apply(this.bind_table_headings, this.form.down('table.data-table thead'));
            this.apply(this.bind_table_body, this.form.down('table.data-table tbody'));
        }
    },

    apply: function(method, element) {
        if (element) {
            return method.bind(this)(element);
        }
    },

    send_ajax_request: function (form, parameters, on_success) {
        document.body.style.cursor = "wait";
        form.request({
            parameters: parameters,
            onSuccess: pytis.on_success(on_success.bind(this)),
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
        // element.down('form') will not exist on first invocation.
        var form = element.down('form') || new Element('form', {action: this.uri, method: 'GET'});
        this.send_ajax_request(form, parameters, function (transport) {
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
            expansion.down('.row-expansion-content').slideUp({duration: 0.25});
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
                content.slideDown({duration: 0.25});
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
        var form = this.form.down('form.list-form-controls');
        var parameters = {_pytis_form_update_request: 1,
                          _pytis_expand_row: 1,
                          _pytis_row_key: this.pytis_row_key(tr)};
        this.send_ajax_request(form, parameters, function (transport) {
            var content = new Element('div', {'class': 'row-expansion-content'});
            content.insert(transport.responseText);
            var collapse_ctrl = new Element('a', {'class': 'collapse-row'});
            collapse_ctrl.update(pytis._("Collapse Row"));
            collapse_ctrl.on('click', this.on_toggle_row_expansion.bind(this));
            content.insert(collapse_ctrl);
            content.hide();
            tr.insert({after: new Element('tr', {'class': 'row-expansion'}).insert(
                new Element('td', {'colspan': tr.childElements().length}).insert(content))});
            content.slideDown({duration: 0.25});
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
            onSuccess: pytis.on_success(function (transport) {
                var container = this.ajax_container;
                var i, callback;
                container.update(transport.responseText);
                this.apply(this.bind_controls, container.down('.list-form-controls', 0));
                this.apply(this.bind_controls, container.down('.list-form-controls', 1));
                this.apply(this.bind_table_headings, container.down('table.data-table thead'));
                this.apply(this.bind_table_body, container.down('table.data-table tbody'));
                for (i=0; i<this.on_load_callbacks.length; i++) {
                    callback = this.on_load_callbacks[i];
                    callback(this.form);
                }
                if (container.down('#found-record')) {
                    window.location.hash = '#found-record';
                }
            }.bind(this)),
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
    },

    bind_table_headings: function (thead) {
        thead.select('th.column-heading').each(function (th) {
            if (th.hasClassName('sortable-column')) {
                th.observe('click', this.on_table_heading_clicked.bind(this));
            }
        }.bind(this));
        if (thead.up('table').hasClassName('expansible-rows')) {
            var th = new Element('th', {'class': 'column-heading expansion-ctrl-heading'});
            thead.down('tr').insert({top: th});
        }
    },

    bind_table_body: function (tbody) {
        tbody.select('td.editable-cell').each(function (element) {
            element.setAttribute('title', pytis._("Double click the cell to edit the value."));
            element.on('dblclick', this.on_edit_cell.bind(this));
        }.bind(this));
        if (tbody.up('table').hasClassName('expansible-rows')) {
            tbody.select('tr').each(function (tr) {
                var ctrl = new Element('a', {'class': 'expand-row'});
                ctrl.update(pytis._("Expand Row"));
                ctrl.on('click', this.on_toggle_row_expansion.bind(this));
                ctrl.update(pytis._("Expand Row"));
                tr.insert({top: new Element('td', {'class': 'expansion-ctrl'}).update(ctrl)});
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
        var search_button = panel.down('.paging-controls button.search');
        if (search_button) {
            search_button.observe('click', this.on_show_search_controls.bind(this));
        }
        var cancel_button = panel.down('div.query button.cancel-search');
        if (cancel_button) {
            cancel_button.observe('click', this.on_hide_search_controls.bind(this));
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
    },

    on_popup_menu_inline_action: function (element, action, uri) {
        if (action === 'update' || action === 'copy' || action === 'delete') {
            var parameters = uri.parseQuery();
            var uri = uri.slice(0, uri.indexOf('?'));
            var form = new Element('form', {action: uri, method: 'GET'});
            var target = (action === 'copy' ? 'after' : 'replace');
            this.send_inline_action_request(element, form, parameters, target);
            return true;
        }
        return false;
    },

    send_inline_action_request: function (element, form, parameters, target) {
        // Action target can be:
        // 'global' ... the content is inserted below the '.actions' element
        //    (containing the global actions, such as insert) above or below
        //    the form data area.
        // 'replace' ... the content replaces the current record on which
        //    the action was invoked.
        // 'before' ... the content is inserted above the curent record on
        //    which the action was invoked.
        // 'after' ... the content is inserted below the curent record on
        //    which the action was invoked.
        parameters['_pytis_inline_form_request'] = '1';
        this.send_ajax_request(form, parameters, function (transport) {
            var found = this.form.down('#found-record');
            if (found) {
                // Unhighlight the previously highlighted record to avoid confusion
                // (the edited record is now the one worth to be noticed).
                found.setAttribute('id', '');
            }
            var content = this.process_inline_action_response(element, target, transport);
            content.down('button.cancel').on('click', function (event) {
                this.on_cancel_inline_action(element, target, content);
                event.stop();
            }.bind(this));
        }.bind(this));
    },

    process_inline_action_response: function (element, target, transport) {
        // Returns the element to be hidden by 'on_cancel_inline_action()'.
        // This extra div is necessary for the slide effects to work properly.
        var div = new Element('div').update(transport.responseText).hide();
        if (target === 'global') {
            element.up('.actions').insert({after: div});
        } else {
            var tr = element.up('tr');
            var tds = tr.childElements();
            var i, colspan = 0;
            for (i=0; i < tds.length; i++) {
                colspan += tds[i].colSpan;
                if (target === 'replace') {
                    tds[i].hide();
                }
            }
            var td = new Element('td', {colspan: colspan, class: 'inline-edit'});
            td.insert(div);
            if (target === 'after' || target === 'before') {
                var new_tr = new Element('tr', {class: 'data-row'});
                new_tr.insert(td);
                if (target === 'after') {
                    tr.insert({'after': new_tr});
                } else {
                    tr.insert({'before': new_tr});
                }
            } else {
                tr.insert(td);
            }
        }
        div.slideDown({duration: 0.25});
        return div;
    },

    on_cancel_inline_action: function (element, target, content) {
        // Here 'content' is the result returned by 'process_inline_action_response()'.
        content.slideUp({
            duration: 0.25,
            afterFinish: function () {
                if (target === 'global') {
                    content.remove();
                    element.enable(); // 'element' is the action button.
                } else {
                    // This is form specific, so it has a separate method...
                    this.remove_canceled_inline_action_content(element, content);
                }
            }.bind(this)
        });
    },

    remove_canceled_inline_action_content: function (element, content) {
        var tr = content.up('tr');
        content.up('td').remove();
        tr.childElements().each(function (x) { x.show(); });
    }

});

pytis.BrowseForm.on_action = function (event, element, action, uri) {
    // This must be a "static" method because the menu items don't
    // exist in the time of form creation so the form can not bind
    // the events to itself.  This method is assigned as popup menu
    // item callback and we may handle item invocation here.
    var form = element.up('.pytis-form').instance;
    if (form && form.inline_editable && form.on_popup_menu_inline_action(element, action, uri)) {
        event.stop();
    }
};


pytis.ListView = Class.create(pytis.BrowseForm, {

    initialize: function ($super, form_id, form_name, uri, inline_editable) {
        $super(form_id, form_name, uri, inline_editable);
        if (inline_editable) {
            this.form.down('.actions').select(
                'button.action-update, button.action-delete, button.action-copy'
            ).each(function (button) {
                var target = (button.hasClassName('action-copy') ? 'after' : 'replace');
                button.on('click', function (event) {
                    this.send_inline_action_request(button, button.up('form'), {}, target);
                    event.stop();
                }.bind(this));
            }.bind(this));
        }
    },

    process_inline_action_response: function ($super, element, target, transport) {
        if (target === 'global') {
            return $super(element, target, transport);
        } else {
            // Note that element may be a button or a popup menu item here.
            var container = new Element('div', {'class': 'inline-form-container'});
            container.update(transport.responseText).hide();
            var parent = element.up('.list-item').down('.list-item-content');
            parent.childElements().each(function (x) { x.slideUp({duration: 0.25}); });
            parent.insert(container);
            container.slideDown({delay: 0.2, duration: 0.25});
            return container;
        }
    },

    remove_canceled_inline_action_content: function (element, content) {
        var parent = content.parentNode;
        content.remove();
        parent.childElements().each(function (x) { x.slideDown({duration: 0.25}); });
    }
    
});


pytis.ItemizedView = Class.create(pytis.BrowseForm, {});


pytis.EditableBrowseForm = Class.create(pytis.BrowseForm, {
    initialize: function ($super, form_id, form_name, uri, inline_editable, allow_insertion) {
        /* form_id, form_name, uri, inline_editable ... defined by the
             super class.
           allow_insertion ... Add a button for insertion of new table
             rows.  Note that this is a different feature of this class
             than 'inline_editable' mode defined by the parent class.

        */
        $super(form_id, form_name, uri, inline_editable);
        if (allow_insertion) {
            var button = new Element('button', {'class': 'new-row-button'});
            button.update(new Element('span').update(pytis._("New row")));
            button.on('click', this.on_insert_new_row.bind(this));
            this.form.insert(button);
        }
    },

    bind_table_body: function (tbody) {
        // We don't call $super here as we don't want editable cells or
        // expansible rows in EditableBrowseForm, but we may need it in
        // future if the parent class adds something we want...
        tbody.select('tr a.remove-row').each(function (link) {
            link.on('click', this.on_remove_row.bind(this));
        }.bind(this));
    },

    on_insert_new_row: function (event) {
        var form = this.form.down('form') || this.form.up('form');
        if (form) {
            var parameters = {'_pytis_form_update_request': 1,
                              '_pytis_insert_new_row': 1};
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
        tr.up('form').insert(new Element('input', {
            type: 'hidden',
            name: '_pytis_removed_row_key_' + this.form_name,
            value: tr.getAttribute('data-pytis-row-key')
        }));
        tr.remove();
        event.stop();
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

pytis.Calendar = Class.create();

//------------------------------------------------------------------------------
// Customized copy of CalendarView library from calendarview.org
//------------------------------------------------------------------------------
//
// The original maintained by Justin Mecham <justin@aspect.net>
//
// Portions Copyright 2002-2005, 2009, 2011, 2012 Mihai Bazon
//
// This calendar is based very loosely on the Dynarch Calendar in that it was
// used as a base, but completely gutted and more or less rewritten in place
// to use the Prototype JavaScript library.
//
// As such, CalendarView is licensed under the terms of the GNU Lesser General
// Public License (LGPL). More information on the Dynarch Calendar can be
// found at:
//
//   www.dynarch.com/projects/calendar
//
// This version used in Pytis web forms includes minor modifications
// by Tomáš Cerha <cerha@brailcom.org> to allow calendar localization.


//------------------------------------------------------------------------------
// Constants
//------------------------------------------------------------------------------

pytis.Calendar.VERSION = '1.2'

pytis.Calendar.DAY_NAMES = new Array(
  'Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday',
  'Sunday'
)

pytis.Calendar.SHORT_DAY_NAMES = new Array(
  'S', 'M', 'T', 'W', 'T', 'F', 'S', 'S'
)

pytis.Calendar.MONTH_NAMES = new Array(
  'January', 'February', 'March', 'April', 'May', 'June', 'July', 'August',
  'September', 'October', 'November', 'December'
)

pytis.Calendar.SHORT_MONTH_NAMES = new Array(
  'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov',
  'Dec' 
)
pytis.Calendar.TODAY = 'Today'
pytis.Calendar.FIRST_WEEK_DAY = 0

pytis.Calendar.NAV_PREVIOUS_YEAR  = -2
pytis.Calendar.NAV_PREVIOUS_MONTH = -1
pytis.Calendar.NAV_TODAY          =  0
pytis.Calendar.NAV_NEXT_MONTH     =  1
pytis.Calendar.NAV_NEXT_YEAR      =  2

//------------------------------------------------------------------------------
// Static Methods
//------------------------------------------------------------------------------

// This gets called when the user presses a mouse button anywhere in the
// document, if the calendar is shown. If the click was outside the open
// calendar this function closes it.
pytis.Calendar._checkCalendar = function(event) {
  if (!window._popupCalendar)
    return false
  if (Element.descendantOf(Event.element(event), window._popupCalendar.container))
    return
  window._popupCalendar.callCloseHandler()
  return Event.stop(event)
}

//------------------------------------------------------------------------------
// Event Handlers
//------------------------------------------------------------------------------

pytis.Calendar.handleMouseDownEvent = function(event)
{
  Event.observe(document, 'mouseup', pytis.Calendar.handleMouseUpEvent)
  Event.stop(event)
}

// XXX I am not happy with how clicks of different actions are handled. Need to
// clean this up!
pytis.Calendar.handleMouseUpEvent = function(event)
{
  var el        = Event.element(event)
  var calendar  = el.calendar
  var isNewDate = false

  // If the element that was clicked on does not have an associated Calendar
  // object, return as we have nothing to do.
  if (!calendar) return false

  // Clicked on a day
  if (typeof el.navAction == 'undefined')
  {
    if (calendar.currentDateElement) {
      Element.removeClassName(calendar.currentDateElement, 'selected')
      Element.addClassName(el, 'selected')
      calendar.shouldClose = (calendar.currentDateElement == el)
      if (!calendar.shouldClose) calendar.currentDateElement = el
    }
    calendar.date.setDateOnly(el.date)
    isNewDate = true
    calendar.shouldClose = !el.hasClassName('otherDay')
    var isOtherMonth     = !calendar.shouldClose
    if (isOtherMonth) calendar.update(calendar.date)
  }

  // Clicked on an action button
  else
  {
    var date = new Date(calendar.date)

    if (el.navAction == pytis.Calendar.NAV_TODAY)
      date.setDateOnly(new Date())

    var year = date.getFullYear()
    var mon = date.getMonth()
    function setMonth(m) {
      var day = date.getDate()
      var max = date.getMonthDays(m)
      if (day > max) date.setDate(max)
      date.setMonth(m)
    }
    switch (el.navAction) {

      // Previous Year
      case pytis.Calendar.NAV_PREVIOUS_YEAR:
        if (year > calendar.minYear)
          date.setFullYear(year - 1)
        break

      // Previous Month
      case pytis.Calendar.NAV_PREVIOUS_MONTH:
        if (mon > 0) {
          setMonth(mon - 1)
        }
        else if (year-- > calendar.minYear) {
          date.setFullYear(year)
          setMonth(11)
        }
        break

      // Today
      case pytis.Calendar.NAV_TODAY:
        break

      // Next Month
      case pytis.Calendar.NAV_NEXT_MONTH:
        if (mon < 11) {
          setMonth(mon + 1)
        }
        else if (year < calendar.maxYear) {
          date.setFullYear(year + 1)
          setMonth(0)
        }
        break

      // Next Year
      case pytis.Calendar.NAV_NEXT_YEAR:
        if (year < calendar.maxYear)
          date.setFullYear(year + 1)
        break

    }

    if (!date.equalsTo(calendar.date)) {
      calendar.setDate(date)
      isNewDate = true
    } else if (el.navAction == 0) {
      isNewDate = (calendar.shouldClose = true)
    }
  }

  if (isNewDate) event && calendar.callSelectHandler()
  if (calendar.shouldClose) event && calendar.callCloseHandler()

  Event.stopObserving(document, 'mouseup', pytis.Calendar.handleMouseUpEvent)

  return Event.stop(event)
}

pytis.Calendar.defaultSelectHandler = function(calendar)
{
  if (!calendar.dateField) return false

  // Update dateField value
  if (calendar.dateField.tagName == 'DIV')
    Element.update(calendar.dateField, calendar.date.print(calendar.dateFormat))
  else if (calendar.dateField.tagName == 'INPUT') {
    calendar.dateField.value = calendar.date.print(calendar.dateFormat) }

  // Trigger the onchange callback on the dateField, if one has been defined
  if (typeof calendar.dateField.onchange == 'function')
    calendar.dateField.onchange()

  // Call the close handler, if necessary
  if (calendar.shouldClose) calendar.callCloseHandler()
}

pytis.Calendar.defaultCloseHandler = function(calendar)
{
  calendar.hide()
}


//------------------------------------------------------------------------------
// Calendar Setup
//------------------------------------------------------------------------------

pytis.Calendar.setup = function(params)
{

  function param_default(name, def) {
    if (!params[name]) params[name] = def
  }

  param_default('dateField', null)
  param_default('triggerElement', null)
  param_default('parentElement', null)
  param_default('selectHandler',  null)
  param_default('closeHandler', null)

  // In-Page Calendar
  if (params.parentElement)
  {
    var calendar = new pytis.Calendar(params.parentElement)
    calendar.setSelectHandler(params.selectHandler || pytis.Calendar.defaultSelectHandler)
    if (params.dateFormat)
      calendar.setDateFormat(params.dateFormat)
    if (params.dateField) {
      calendar.setDateField(params.dateField)
      calendar.parseDate(calendar.dateField.innerHTML || calendar.dateField.value)
    }
    calendar.show()
    return calendar
  }

  // Popup Calendars
  //
  // XXX There is significant optimization to be had here by creating the
  // calendar and storing it on the page, but then you will have issues with
  // multiple calendars on the same page.
  else
  {
    var triggerElement = $(params.triggerElement || params.dateField)
    triggerElement.onclick = function() {
      var calendar = new pytis.Calendar()
      calendar.setSelectHandler(params.selectHandler || pytis.Calendar.defaultSelectHandler)
      calendar.setCloseHandler(params.closeHandler || pytis.Calendar.defaultCloseHandler)
      if (params.dateFormat)
        calendar.setDateFormat(params.dateFormat)
      if (params.dateField) {
        calendar.setDateField(params.dateField)
        calendar.parseDate(calendar.dateField.innerHTML || calendar.dateField.value)
      }
      if (params.dateField)
        Date.parseDate(calendar.dateField.value || calendar.dateField.innerHTML,
                       calendar.dateFormat)
      calendar.showAtElement(triggerElement)
      return calendar
    }
  }

}



//------------------------------------------------------------------------------
// Calendar Instance
//------------------------------------------------------------------------------

pytis.Calendar.prototype = {

  // The HTML Container Element
  container: null,

  // Callbacks
  selectHandler: null,
  closeHandler: null,

  // Configuration
  minYear: 1900,
  maxYear: 2100,
  dateFormat: '%Y-%m-%d',

  // Dates
  date: new Date(),
  currentDateElement: null,

  // Status
  shouldClose: false,
  isPopup: true,

  dateField: null,

  //----------------------------------------------------------------------------
  // Initialize
  //----------------------------------------------------------------------------

  initialize: function(parent)
  {
    if (parent)
      this.create($(parent))
    else
      this.create()
  },

  //----------------------------------------------------------------------------
  // Update / (Re)initialize Calendar
  //----------------------------------------------------------------------------

  update: function(date)
  {
    var calendar   = this
    var today      = new Date()
    var thisYear   = today.getFullYear()
    var thisMonth  = today.getMonth()
    var thisDay    = today.getDate()
    var month      = date.getMonth();
    var dayOfMonth = date.getDate();

    // Ensure date is within the defined range
    if (date.getFullYear() < this.minYear)
      date.setFullYear(this.minYear)
    else if (date.getFullYear() > this.maxYear)
      date.setFullYear(this.maxYear)

    this.date = new Date(date)

    date.setDate(1)
    // Calculate the first day to display (including the previous month)
    var offset = (date.getDay() >= pytis.Calendar.FIRST_WEEK_DAY ? +1 : -6) 
    date.setDate(-date.getDay() + offset + pytis.Calendar.FIRST_WEEK_DAY)

    // Fill in the days of the month
    Element.getElementsBySelector(this.container, 'tbody tr').each(
      function(row, i) {
        var rowHasDays = false
        row.immediateDescendants().each(
          function(cell, j) {
            var day            = date.getDate()
            var dayOfWeek      = date.getDay()
            var isCurrentMonth = (date.getMonth() == month)

            // Reset classes on the cell
            cell.className = ''
            cell.date = new Date(date)
            cell.update(day)

            // Account for days of the month other than the current month
            if (!isCurrentMonth)
              cell.addClassName('otherDay')
            else
              rowHasDays = true

            // Ensure the current day is selected
            if (isCurrentMonth && day == dayOfMonth) {
              cell.addClassName('selected')
              calendar.currentDateElement = cell
            }

            // Today
            if (date.getFullYear() == thisYear && date.getMonth() == thisMonth && day == thisDay)
              cell.addClassName('today')

            // Weekend
            if ([0, 6].indexOf(dayOfWeek) != -1)
              cell.addClassName('weekend')

            // Set the date to tommorrow
            date.setDate(day + 1)
          }
        )
        // Hide the extra row if it contains only days from another month
        !rowHasDays ? row.hide() : row.show()
      }
    )

    this.container.getElementsBySelector('td.title')[0].update(
      pytis.Calendar.MONTH_NAMES[month] + ' ' + this.date.getFullYear()
    )
  },



  //----------------------------------------------------------------------------
  // Create/Draw the Calendar HTML Elements
  //----------------------------------------------------------------------------

  create: function(parent)
  {

    // If no parent was specified, assume that we are creating a popup calendar.
    if (!parent) {
      parent = document.getElementsByTagName('body')[0]
      this.isPopup = true
    } else {
      this.isPopup = false
    }

    // Calendar Table
    var table = new Element('table')

    // Calendar Header
    var thead = new Element('thead')
    table.appendChild(thead)

    // Title Placeholder
    var row  = new Element('tr')
    var cell = new Element('td', { colSpan: 7 } )
    cell.addClassName('title')
    row.appendChild(cell)
    thead.appendChild(row)

    // Calendar Navigation
    row = new Element('tr')
    this._drawButtonCell(row, '&#x00ab;', 1,     pytis.Calendar.NAV_PREVIOUS_YEAR)
    this._drawButtonCell(row, '&#x2039;', 1,     pytis.Calendar.NAV_PREVIOUS_MONTH)
    this._drawButtonCell(row, pytis.Calendar.TODAY, 3, pytis.Calendar.NAV_TODAY)
    this._drawButtonCell(row, '&#x203a;', 1,     pytis.Calendar.NAV_NEXT_MONTH)
    this._drawButtonCell(row, '&#x00bb;', 1,     pytis.Calendar.NAV_NEXT_YEAR)
    thead.appendChild(row)

    // Day Names
    row = new Element('tr')
    for (var i = 0; i < 7; ++i) {
      cell = new Element('th').update(pytis.Calendar.SHORT_DAY_NAMES[(i + pytis.Calendar.FIRST_WEEK_DAY) % 7]);
      if (i == 0 || i == 6)
        cell.addClassName('weekend')
      row.appendChild(cell)
    }
    thead.appendChild(row)

    // Calendar Days
    var tbody = table.appendChild(new Element('tbody'))
    for (i = 6; i > 0; --i) {
      row = tbody.appendChild(new Element('tr'))
      row.addClassName('days')
      for (var j = 7; j > 0; --j) {
        cell = row.appendChild(new Element('td'))
        cell.calendar = this
      }
    }

    // Calendar Container (div)
    this.container = new Element('div')
    this.container.addClassName('pytis-calendar')
    if (this.isPopup) {
      this.container.setStyle({ position: 'absolute', display: 'none' })
      this.container.addClassName('popup')
    }
    this.container.appendChild(table)

    // Initialize Calendar
    this.update(this.date)

    // Observe the container for mousedown events
    Event.observe(this.container, 'mousedown', pytis.Calendar.handleMouseDownEvent)

    // Append to parent element
    parent.appendChild(this.container)

  },

  _drawButtonCell: function(parent, text, colSpan, navAction)
  {
    var cell          = new Element('td')
    if (colSpan > 1) cell.colSpan = colSpan
    cell.className    = 'button'
    cell.calendar     = this
    cell.navAction    = navAction
    cell.innerHTML    = text
    cell.unselectable = 'on' // IE
    parent.appendChild(cell)
    return cell
  },



  //------------------------------------------------------------------------------
  // Callbacks
  //------------------------------------------------------------------------------

  // Calls the Select Handler (if defined)
  callSelectHandler: function()
  {
    if (this.selectHandler)
      this.selectHandler(this, this.date.print(this.dateFormat))
  },

  // Calls the Close Handler (if defined)
  callCloseHandler: function()
  {
    if (this.closeHandler)
      this.closeHandler(this)
  },



  //------------------------------------------------------------------------------
  // Calendar Display Functions
  //------------------------------------------------------------------------------

  // Shows the Calendar
  show: function()
  {
    this.container.show()
    if (this.isPopup) {
      window._popupCalendar = this
      Event.observe(document, 'mousedown', pytis.Calendar._checkCalendar)
    }
  },

  // Shows the calendar at the given absolute position
  showAt: function (x, y)
  {
    this.container.setStyle({ left: Math.max(x-40, 0) + 'px', top: Math.max(y-80, 0) + 'px' })
    this.show()
  },

  // Shows the Calendar at the coordinates of the provided element
  showAtElement: function(element)
  {
    var pos = Position.cumulativeOffset(element)
    this.showAt(pos[0], pos[1])
  },

  // Hides the Calendar
  hide: function()
  {
    if (this.isPopup)
      Event.stopObserving(document, 'mousedown', pytis.Calendar._checkCalendar)
    this.container.hide()
  },



  //------------------------------------------------------------------------------
  // Miscellaneous
  //------------------------------------------------------------------------------

  // Tries to identify the date represented in a string.  If successful it also
  // calls this.setDate which moves the calendar to the given date.
  parseDate: function(str, format)
  {
    if (!format)
      format = this.dateFormat
    this.setDate(Date.parseDate(str, format))
  },



  //------------------------------------------------------------------------------
  // Getters/Setters
  //------------------------------------------------------------------------------

  setSelectHandler: function(selectHandler)
  {
    this.selectHandler = selectHandler
  },

  setCloseHandler: function(closeHandler)
  {
    this.closeHandler = closeHandler
  },

  setDate: function(date)
  {
      if (!date.equalsTo(this.date)) {
	  this.update(date)
      }
  },

  setDateFormat: function(format)
  {
    this.dateFormat = format
  },

  setDateField: function(field)
  {
    this.dateField = $(field)
  },

  setRange: function(minYear, maxYear)
  {
    this.minYear = minYear
    this.maxYear = maxYear
  }

}

// global object that remembers the calendar
window._popupCalendar = null


//==============================================================================
//
// Date Object Patches
//
// This is pretty much untouched from the original. I really would like to get
// rid of these patches if at all possible and find a cleaner way of
// accomplishing the same things. It's a shame Prototype doesn't extend Date at
// all.
//
//==============================================================================

Date.DAYS_IN_MONTH = new Array(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
Date.SECOND        = 1000 /* milliseconds */
Date.MINUTE        = 60 * Date.SECOND
Date.HOUR          = 60 * Date.MINUTE
Date.DAY           = 24 * Date.HOUR
Date.WEEK          =  7 * Date.DAY

// Parses Date
Date.parseDate = function(str, fmt) {
  var today = new Date();
  var y     = 0;
  var m     = -1;
  var d     = 0;
  var a     = str.split(/\W+/);
  var b     = fmt.match(/%./g);
  var i     = 0, j = 0;
  var hr    = 0;
  var min   = 0;

  for (i = 0; i < a.length; ++i) {
    if (!a[i]) continue;
    switch (b[i]) {
      case "%d":
      case "%e":
        d = parseInt(a[i], 10);
        break;
      case "%m":
        m = parseInt(a[i], 10) - 1;
        break;
      case "%Y":
      case "%y":
        y = parseInt(a[i], 10);
        (y < 100) && (y += (y > 29) ? 1900 : 2000);
        break;
      case "%b":
      case "%B":
        for (j = 0; j < 12; ++j) {
          if (pytis.Calendar.MONTH_NAMES[j].substr(0, a[i].length).toLowerCase() == a[i].toLowerCase()) {
            m = j;
            break;
          }
        }
        break;
      case "%H":
      case "%I":
      case "%k":
      case "%l":
        hr = parseInt(a[i], 10);
        break;
      case "%P":
      case "%p":
        if (/pm/i.test(a[i]) && hr < 12)
          hr += 12;
        else if (/am/i.test(a[i]) && hr >= 12)
          hr -= 12;
        break;
      case "%M":
        min = parseInt(a[i], 10);
        break;
    }
  }
  if (isNaN(y)) y = today.getFullYear();
  if (isNaN(m)) m = today.getMonth();
  if (isNaN(d)) d = today.getDate();
  if (isNaN(hr)) hr = today.getHours();
  if (isNaN(min)) min = today.getMinutes();
  if (y != 0 && m != -1 && d != 0)
    return new Date(y, m, d, hr, min, 0);
  y = 0; m = -1; d = 0;
  for (i = 0; i < a.length; ++i) {
    if (a[i].search(/[a-zA-Z]+/) != -1) {
      var t = -1;
      for (j = 0; j < 12; ++j) {
        if (pytis.Calendar.MONTH_NAMES[j].substr(0, a[i].length).toLowerCase() == a[i].toLowerCase()) { t = j; break; }
      }
      if (t != -1) {
        if (m != -1) {
          d = m+1;
        }
        m = t;
      }
    } else if (parseInt(a[i], 10) <= 12 && m == -1) {
      m = a[i]-1;
    } else if (parseInt(a[i], 10) > 31 && y == 0) {
      y = parseInt(a[i], 10);
      (y < 100) && (y += (y > 29) ? 1900 : 2000);
    } else if (d == 0) {
      d = a[i];
    }
  }
  if (y == 0)
    y = today.getFullYear();
  if (m != -1 && d != 0)
    return new Date(y, m, d, hr, min, 0);
  return today;
};

// Returns the number of days in the current month
Date.prototype.getMonthDays = function(month) {
  var year = this.getFullYear()
  if (typeof month == "undefined")
    month = this.getMonth()
  if (((0 == (year % 4)) && ( (0 != (year % 100)) || (0 == (year % 400)))) && month == 1)
    return 29
  else
    return Date.DAYS_IN_MONTH[month]
};

// Returns the number of day in the year
Date.prototype.getDayOfYear = function() {
  var now = new Date(this.getFullYear(), this.getMonth(), this.getDate(), 0, 0, 0);
  var then = new Date(this.getFullYear(), 0, 0, 0, 0, 0);
  var time = now - then;
  return Math.floor(time / Date.DAY);
};

/** Returns the number of the week in year, as defined in ISO 8601. */
Date.prototype.getWeekNumber = function() {
  var d = new Date(this.getFullYear(), this.getMonth(), this.getDate(), 0, 0, 0);
  var DoW = d.getDay();
  d.setDate(d.getDate() - (DoW + 6) % 7 + 3); // Nearest Thu
  var ms = d.valueOf(); // GMT
  d.setMonth(0);
  d.setDate(4); // Thu in Week 1
  return Math.round((ms - d.valueOf()) / (7 * 864e5)) + 1;
};

/** Checks date and time equality */
Date.prototype.equalsTo = function(date) {
  return ((this.getFullYear() == date.getFullYear()) &&
   (this.getMonth() == date.getMonth()) &&
   (this.getDate() == date.getDate()) &&
   (this.getHours() == date.getHours()) &&
   (this.getMinutes() == date.getMinutes()));
};

/** Set only the year, month, date parts (keep existing time) */
Date.prototype.setDateOnly = function(date) {
  var tmp = new Date(date);
  this.setDate(1);
  this.setFullYear(tmp.getFullYear());
  this.setMonth(tmp.getMonth());
  this.setDate(tmp.getDate());
};

/** Prints the date in a string according to the given format. */
Date.prototype.print = function (str) {
  var m = this.getMonth();
  var d = this.getDate();
  var y = this.getFullYear();
  var wn = this.getWeekNumber();
  var w = this.getDay();
  var s = {};
  var hr = this.getHours();
  var pm = (hr >= 12);
  var ir = (pm) ? (hr - 12) : hr;
  var dy = this.getDayOfYear();
  if (ir == 0)
    ir = 12;
  var min = this.getMinutes();
  var sec = this.getSeconds();
  s["%a"] = pytis.Calendar.SHORT_DAY_NAMES[w]; // abbreviated weekday name [FIXME: I18N]
  s["%A"] = pytis.Calendar.DAY_NAMES[w]; // full weekday name
  s["%b"] = pytis.Calendar.SHORT_MONTH_NAMES[m]; // abbreviated month name [FIXME: I18N]
  s["%B"] = pytis.Calendar.MONTH_NAMES[m]; // full month name
  // FIXME: %c : preferred date and time representation for the current locale
  s["%C"] = 1 + Math.floor(y / 100); // the century number
  s["%d"] = (d < 10) ? ("0" + d) : d; // the day of the month (range 01 to 31)
  s["%e"] = d; // the day of the month (range 1 to 31)
  // FIXME: %D : american date style: %m/%d/%y
  // FIXME: %E, %F, %G, %g, %h (man strftime)
  s["%H"] = (hr < 10) ? ("0" + hr) : hr; // hour, range 00 to 23 (24h format)
  s["%I"] = (ir < 10) ? ("0" + ir) : ir; // hour, range 01 to 12 (12h format)
  s["%j"] = (dy < 100) ? ((dy < 10) ? ("00" + dy) : ("0" + dy)) : dy; // day of the year (range 001 to 366)
  s["%k"] = hr;   // hour, range 0 to 23 (24h format)
  s["%l"] = ir;   // hour, range 1 to 12 (12h format)
  s["%m"] = (m < 9) ? ("0" + (1+m)) : (1+m); // month, range 01 to 12
  s["%M"] = (min < 10) ? ("0" + min) : min; // minute, range 00 to 59
  s["%n"] = "\n";   // a newline character
  s["%p"] = pm ? "PM" : "AM";
  s["%P"] = pm ? "pm" : "am";
  // FIXME: %r : the time in am/pm notation %I:%M:%S %p
  // FIXME: %R : the time in 24-hour notation %H:%M
  s["%s"] = Math.floor(this.getTime() / 1000);
  s["%S"] = (sec < 10) ? ("0" + sec) : sec; // seconds, range 00 to 59
  s["%t"] = "\t";   // a tab character
  // FIXME: %T : the time in 24-hour notation (%H:%M:%S)
  s["%U"] = s["%W"] = s["%V"] = (wn < 10) ? ("0" + wn) : wn;
  s["%u"] = w + 1;  // the day of the week (range 1 to 7, 1 = MON)
  s["%w"] = w;    // the day of the week (range 0 to 6, 0 = SUN)
  // FIXME: %x : preferred date representation for the current locale without the time
  // FIXME: %X : preferred time representation for the current locale without the date
  s["%y"] = ('' + y).substr(2, 2); // year without the century (range 00 to 99)
  s["%Y"] = y;    // year with the century
  s["%%"] = "%";    // a literal '%' character

  return str.gsub(/%./, function(match) { return s[match] || match });
};

Date.prototype.__msh_oldSetFullYear = Date.prototype.setFullYear;
Date.prototype.setFullYear = function(y) {
  var d = new Date(this);
  d.__msh_oldSetFullYear(y);
  if (d.getMonth() != this.getMonth())
    this.setDate(28);
  this.__msh_oldSetFullYear(y);
}
