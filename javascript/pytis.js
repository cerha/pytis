/* -*- coding: utf-8 -*-
 *
 * Copyright (C) 2009-2017 OUI Technology Ltd.
 * Author: Tomas Cerha
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
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

/*jshint browser: true */
/*eslint no-unused-vars: 0 */
/*global lcg */
/*global $ */
/*global CKEDITOR */

"use strict"

let pytis = {
    gettext: new Gettext({domain: 'pytis-web'}),
}


pytis._ = function (msg) {
    return pytis.gettext.gettext(msg)
}


pytis.show_tooltip = function (event, uri) {
    // This can't be implemented as a Field method as Field instances are not
    // created in BrowseForm (currently only in edit form).
    let element = event.target
    element._pytis_tooltip_timeout = setTimeout(function () {
        element._pytis_tooltip = new lcg.Tooltip(uri, event.x, event.y)
    }, 500)
}


pytis.hide_tooltip = function (element) {
    if (element._pytis_tooltip) {
        element._pytis_tooltip.remove()
        element._pytis_tooltip = null
    }
    if (element._pytis_tooltip_timeout) {
        clearTimeout(element._pytis_tooltip_timeout)
    }
}


pytis.Widget = class extends lcg.Widget {

    _submit_form_xhr(form, parameters, callback) {
        document.body.style.cursor = "wait"
        let data = form.serializeArray();
        for (let param in parameters) {
            if (parameters.hasOwnProperty(param)) {
                data[data.length] = {name: param, value: parameters[param]}
            }
        }

        $.ajax({url: form.attr('action'),
                type: form.attr('method'),
                data: data})
            .done((response, status, xhr) => {
                // Errors in asynchronous handlers are otherwise silently
                // ignored.  This will only work in some browsers,
                // but it is only useful for debugging anyway...
                try {
                    callback(response, status, xhr)
                } catch (e) {
                    console.log(e)
                } finally {
                    if (document.body.style.cursor === "wait") {
                        document.body.style.cursor = "default"
                    }
                }
            })
            .fail((response, status, xhr) => document.body.style.cursor = "default")
    }

}


pytis.Form = class extends pytis.Widget {
    /* Common base class of all Pytis HTML forms. */
}


pytis.BrowseForm = class extends pytis.Form {
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
    constructor(form_id, form_name, uri, inline_editable) {
        /* form_id ... HTML id of the top level element (see parent class)
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
        super(form_id)
        this._form_name = form_name
        this._uri = uri
        this._inline_editable = inline_editable
        this._ajax_container = this.element.find('.ajax-container')
        this._on_load_callbacks = []
        if (inline_editable) {
            this.element.find('button.action-insert')
                .on('click', event => {
                    button.disable()
                    this._send_inline_action_request(button, button.closest('form'), {}, 'global')
                    return false
                })
        }
        if (uri && this._ajax_container.length != 0) {
            this._async_load = true
            let parameters = {};
            let query = new URLSearchParams(window.location.search.replace(/;/g, '&'))
            if (query.get('form_name') === form_name) {
                parameters = Object.fromEntries(query)
            }
            let page = this.element.closest('.notebook-widget > section')
            if (page.length) {
                lcg.Notebook.on_activation(page, function () {
                    this._load_form_data(parameters)
                }.bind(this))
            } else {
                this._load_form_data(parameters)
            }
        } else {
            this._async_load = false
            this._bind_search_controls(this.element.find('.list-form-controls:eq(0)'))
            this._bind_search_controls(this.element.find('.list-form-controls:eq(1)'))
            this._bind_table_headings(this.element.find('table.data-table thead'))
            this._bind_table_body(this.element.find('table.data-table tbody'))
        }
    }

    _on_edit_cell(event) {
        let td = $(event.target).closest('td')
        this._send_edit_cell_request(td)
        return false
    }

    _send_edit_cell_request(element) {
        let parameters = {_pytis_form_update_request: 1,
                          _pytis_edit_cell: 1,
                          _pytis_row_key: this._pytis_row_key(element),
                          _pytis_column_id: this._pytis_column_id(element)}
        // element.find('form') will not exist on first invocation.
        let form = element.find('form') || $(`<form action="${this._uri}" method="GET">`)
        this._submit_form_xhr(form, parameters, (response, status, xhr) => {
            element.html(response)
            let edit_form = element.find('form')
            if (edit_form) {
                edit_form.find('button.save-edited-cell').on('click',  function (event) {
                    this._send_edit_cell_request(element)
                    return false
                }.bind(this))
                edit_form[this._pytis_column_id(element)].focus()
            }
        })
    }

    _on_toggle_row_expansion(event) {
        let tr = $(event.target).closest('tr')
        let expansion
        if (tr.hasClass('row-expansion')) {
            expansion = tr
            tr = expansion.prev()
        } else {
            expansion = tr.next()
        }
        let link = tr.find('a.expand-row')
        if (tr.hasClass('expanded')) {
            tr.removeClass('expanded')
            expansion.find('.row-expansion-content').slideUp({duration: 0.25})
            link.focus()
            setTimeout(function () {
                expansion.hide()
                link.attr('aria-expanded', 'false')
                link.html(pytis._("Expand Row"))
            }, 250)
        } else {
            tr.addClass('expanded')
            if (expansion && expansion.hasClass('row-expansion')) {
                let content = expansion.find('.row-expansion-content')
                //expansion.attr('style', '')
                //content.attr('style', '')
                expansion.show()
                content.slideDown({duration: 0.25})
            } else {
                this._send_expand_row_request(tr)
            }
            setTimeout(function () {
                link.attr('aria-expanded', 'true')
                link.html(pytis._("Collapse Row"))
            }, 250)
            return false
        }
    }

    _send_expand_row_request(tr) {
        let form = this.element.find('form.list-form-controls')
        let parameters = {_pytis_form_update_request: 1,
                          _pytis_expand_row: 1,
                          _pytis_row_key: this._pytis_row_key(tr)}
        this._submit_form_xhr(form, parameters, (response, status, xhr) => {
            // Beware: The elements created here must follow the same structure as in
            // the Python code (the non-async version). The JS code doesn't make any
            // difference.
            let nr = $(`<tr class="row-expansion ${tr.hasClass('odd') ? 'odd' : 'even'}">` +
                       `<td colspan="${tr.children('td').length}">` +
                       `<div class"row-expansion-content">` +
                       response +
                       `<a class="collapse-row">${pytis._("Collapse Row")}</a>` +
                       `</div>` +
                       `</td>` +
                       `</tr>`)
            nr.find('a.collapse-row').on('click', this._on_toggle_row_expansion)
            nr.find('.row-expansion-content').hide()
            nr.insertAfter(tr)
            nr.find('.row-expansion-content').slideDown(0.25)
        })
    }

    _pytis_row_key(element) {
        // Return pytis row key value for given HTML element inside the pytis table form.
        // Returns null if the element is not inside a pytis table or
        // if the table doesn't contain necessary information.
        let tr = (element.nodeName === 'TR' ? element : element.closest('tr'))
        if (tr) {
            return tr.attr('data-pytis-row-key')
        }
        return null
    }

    _pytis_column_id(element) {
        // Return pytis column id for given HTML element inside the pytis table form.
        /* The method works with with class names of table th elements, but this
           may be unreliable in some cases, so better we might pass column names
           explicitly to the JavaScript form class constructor or something
           similar...
        */
        let td = (element.nodeName === 'TD' ? element : element.closest('td'))
        let tr = element.closest('tr')
        let table = element.closest('table')
        if (td && tr && table) {
            let thead = table.find('thead')
            if (thead) {
                let hr = thead.find('tr')
                if (hr) {
                    let th = hr.childElements()[tr.childElements().indexOf(td)]
                    if (th) {
                        let cls = th.classNames().find(function (x) {
                            return x.match('^column-id-')
                        })
                        if (cls) {
                            return cls.substr(10, cls.length)
                        }
                    }
                }
            }
        }
        return null
    }

    _load_form_data(parameters, busy_cursor) {
        let container = this._ajax_container
        if (busy_cursor) {
            document.body.style.cursor = "wait"
        }
        parameters._pytis_async_load_request = 1
        container.load(this._uri, parameters, (response, status, xhr) => {
            if (busy_cursor) {
                document.body.style.cursor = "default"
            }
            if (status == 'success') {
                this._bind_controls(container.find('.list-form-controls:eq(0)'))
                this._bind_controls(container.find('.list-form-controls:eq(1)'))
                this._bind_table_headings(container.find('table.data-table thead'))
                this._bind_table_body(container.find('table.data-table tbody'))
                for (let i=0; i < this._on_load_callbacks.length; i++) {
                    let callback = this._on_load_callbacks[i]
                    callback(this.element)
                }
                if (container.find('#found-record')) {
                    window.location.hash = '#found-record'
                }
            } else {
                let msg = pytis._("Failed loading form:") + ' ' + xhr.status + ' ' + xhr.statusText
                let tb_start = response.search('here is the original traceback:')
                let traceback = ''
                if (tb_start !== -1) {
                    let tb_len = response.slice(tb_start).search('-->')
                    traceback = (`<pre class="form-load-traceback" style="display: none">` +
                                 response.slice(tb_start + 32, tb_start + tb_len - 3) +
                                 `</pre>`)
                    msg += (` (<a href="#" onclick="` +
                            `$(this.parentNode.parentNode).find('.form-load-traceback').toggle()` +
                            `">${pytis._("show details")}</a>)`)
                }
                this._ajax_container.html(`<div class="form-load-error">${msg}</div>` + traceback)
            }
        })
    }

    _bind_controls(panel) {
        let form = panel.closest('form')
        panel.find('button.prev-page, button.next-page').on('click', event => {
            let b = $(event.target).closest('button')[0]
            this._reload_form_data(form, {[b.name]: b.value})
            return false
        })
        let apply_button = panel.find('button.apply-filters')
        apply_button.on('click', event => {
            this._reload_form_data(form, {})
            return false
        })
        if (apply_button.length == 0) {
            panel.find('select, checkbox, radio')
                .on('change', event => {
                    this._reload_form_data(form, {})
                    return false
                })
        }
        panel.find('.index-search-controls a').each(function(i, element) {
            $(element).on('click', event => {
                let params = (new URL(element.href.replace(/;/g, '&'))).searchParams
                this._reload_form_data(form, {index_search: params.get('index_search')})
                return false
            })
        }.bind(this))
        this._bind_search_controls(panel)
    }

    _bind_table_headings(thead) {
        thead.find('th.column-heading.sortable-column')
            .on('click', this._on_table_heading_clicked.bind(this))
        if (thead.parent('table').hasClass('expansible-rows')) {
            thead.find('tr').prepend('<th class="column-heading expansion-ctrl-heading"></th>')
        }
    }

    _bind_table_body(tbody) {
        tbody.find('td.editable-cell')
            .attr('title', pytis._("Double click the cell to edit the value."))
            .on('dblclick', this._on_edit_cell)
        let expansible = tbody.parent('table').hasClass('expansible-rows')
        if (expansible || this._inline_editable) {
            tbody.find('tr.data-row').each((i, tr) => {
                if (expansible) {
                    let ctrl = $(`<a class="expand-row">${pytis._("Expand Row")}</a>`)
                    ctrl.on('click', this._on_toggle_row_expansion.bind(this))
                    $(tr).prepend($(`<td class="expansion-ctrl"></td>`).html(ctrl))
                }
                if (this._inline_editable) {
                    let uri = $(tr).attr('data-pytis-row-update-uri')
                    if (uri) {
                        $(tr).on('dblclick', event => {
                            this._on_popup_menu_inline_action(tr, 'update', uri)
                            return false
                        })
                    }
                }
            })
        }
    }

    _reload_form_data(form, parameters) {
        for (let param of form.serializeArray()) {
            if (parameters.hasOwnProperty(param.name)) {
                let value = parameters[param.name]
                if (Array.isArray(value)) {
                    value[value.length] = param.value
                } else {
                    parameters[param.name] = [value, param.value]
                }
            } else {
                parameters[param.name] = param.value
            }
        }
        this._ajax_container.find('form.list-form-controls').prop('enabled', false)
        this._load_form_data(parameters, true)
    }

    _bind_search_controls(panel) {
        panel.find('.paging-controls button.search')
            .on('click', this._on_show_search_controls.bind(this))
        panel.find('div.query button.cancel-search')
            .on('click', this._on_hide_search_controls.bind(this))
    }

    _on_table_heading_clicked(event) {
        let th = $(event.target).closest('th')
        let classname = th.attr('class').split(' ').find(x => x.startsWith('column-id-'))
        if (classname) {
            let column_id = classname.substring(10)
            let dir = 'asc'
            if (th.find('.sort-direction-asc').length) {
                dir = 'desc'
            }
            if (th.find('.sort-direction-desc').length) {
                dir = ''
            }
            let parameters = {form_name: this._form_name, sort: column_id, dir: dir}
            if (this._async_load) {
                this.element.find('form.list-form-controls').prop('enabled', false)
                this._load_form_data(parameters, true)
            } else {
                window.location.search = (new URLSearchParams(parameters)).toString()
            }
            return false
        }
    }

    _on_show_search_controls(event) {
        let search_controls = this.element.find('div.query')
        let i, panel, button
        search_controls.show()
        search_controls.find('input.text-search-field').focus()
        search_controls.find('input[type=hidden]').value = '1'
        for (i=0; i<2; i++) {
            panel = this.element.find('.list-form-controls', i)
            if (panel) {
                button = panel.find('.paging-controls button.search')
                if (button) {
                    button.hide()
                }
            }
        }
        return false
    }

    _on_hide_search_controls(event) {
        let search_controls = this.element.find('div.query')
        let form = search_controls.closest('form')
        form['show-search-field'].value = ''
        form.query.value = ''
        form.submit()
        return false
    }

    _on_popup_menu_inline_action(element, action, uri) {
        if (action === 'update' || action === 'copy' || action === 'delete') {
            let parameters = uri.parseQuery()
            uri = uri.slice(0, uri.indexOf('?'))
            let form = $(`<form action="${uri}" method="GET"></form>`)
            let target = (action === 'copy' ? 'after' : 'replace')
            this._send_inline_action_request(element, form, parameters, target)
            return true
        }
        return false
    }

    _send_inline_action_request(element, form, parameters, target) {
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
        let content = this.element.find('.inline-form-container')
        if (content && !this._cancel_inline_action(content)) {
            return
        }
        parameters['_pytis_inline_form_request'] = '1'
        this._submit_form_xhr(form, parameters, (response, status, xhr) => {
            let found = this.element.find('#found-record')
            if (found) {
                // Unhighlight the previously highlighted record to avoid confusion
                // (the edited record is now the one worth to be noticed).
                found.attr('id', '')
            }
            // This extra div is necessary for the slide effects to work properly.
            let content = $(`<div class="inline-form-container target-${target}"></div>`)
            content.data('_pytis_inline_action_invoked_by', element)
            content.html(response).hide()
            this._insert_inline_action_content(element, target, content)
            content.find('button.cancel').on('click', event => {
                this._cancel_inline_action(content)
                return false
            })
            let form = content.find('form')
            if (form) {
                for (let i=0; i < form.length; ++i) {
                    let field = form[i]
                    if (field.type != 'hidden' && !field.disabled && field.visible()) {
                        setTimeout(function () {
                            field.focus()
                            Effect.ScrollTo(field, {duration: 0.3, offset: -50})
                        }, 330)
                        break
                    }
                }
            }
        })
    }

    _insert_inline_action_content(element, target, content) {
        // Insert the content returned by an inline action into a proper place
        // in the DOM.  The "proper place" depends on the form type and the
        // requested 'target' (see _send_inline_action_request).
        if (target === 'global') {
            content.insertAfter(element.closest('.actions'))
        } else {
            let tr = element.closest('tr')
            let colspan = 0
            for (let td of tr.children('td')) {
                colspan += td.attr('colspan')
                if (target === 'replace') {
                    td.hide()
                }
            }
            let td = `<td colspan="${colspan}" class="inline-edit">${content}</td>`
            if (target === 'after' || target === 'before') {
                let nr = $(`<tr class="data-row">${td}</tr>`)
                if (target === 'after') {
                    nr.insertAfter(tr)
                } else {
                    nr.insertBefore(tr)
                }
            } else {
                tr.insert(td)
            }
        }
        content.slideDown(0.25)
    }

    _cancel_inline_action(content) {
        // Here 'content' is the same container as passed to '_insert_inline_action_content()'.
        let form = lcg.widget_instance(content.find('.pytis-form.edit-form'))
        if (form && !form.cancel()) {
            return false
        }
        content.slideUp({
            duration: 0.25,
            afterFinish: function () {
                if (content.hasClass('target-global')) {
                    content._pytis_inline_action_invoked_by.enable()
                    content.remove()
                } else {
                    // This is form specific, so it has a separate method...
                    this._remove_canceled_inline_action_content(content)
                }
            }.bind(this)
        })
        return true
    }

    _remove_canceled_inline_action_content(content) {
        let tr = content.closest('tr')
        content.closest('td').remove()
        tr.childElements().each(function (x) { x.show(); })
    }

    on_load(callback) {
        /* Call given callback function when the form is fully loaded.

           The callback will be called immediately if the form is loaded
           synchronously or after the asynchronous load in the other case.

           The callback function will receive the form's top level DOM element
           as an argument.

        */
        if (this._async_load) {
            this._on_load_callbacks[this._on_load_callbacks.length] = callback
        } else {
            callback(this.element)
        }
    }

}

pytis.BrowseForm.on_action = function (event, element, action, uri) {
    // This must be a "static" method because the menu items don't
    // exist in the time of form creation so the form can not bind
    // the events to itself.  This method is assigned as popup menu
    // item callback and we may handle item invocation here.
    let form = element.closest('.pytis-form')[0].instance
    if (form && form._inline_editable && form._on_popup_menu_inline_action(element, action, uri)) {
        return false
    }
}


pytis.ListView = class extends pytis.BrowseForm {

    constructor(form_id, form_name, uri, inline_editable) {
        super(form_id, form_name, uri, inline_editable)
        if (inline_editable) {
            this.element.find(
                '.actions button.action-update, button.action-delete, button.action-copy'
            ).each((i, button) => {
                button = $(button)
                let target = (button.hasClass('action-copy') ? 'after' : 'replace')
                button.on('click', event => {
                    this._send_inline_action_request(button, button.closest('form'), {}, target)
                    return false
                })
            })
        }
    }

    _insert_inline_action_content(element, target, content) {
        if (target === 'global') {
            super._insert_inline_action_content(element, target, content)
        } else {
            // Note that element may be a button or a popup menu item here.
            let parent = element.closest('.list-item').find('.list-item-content')
            parent.childElements().each(function (x) { x.slideUp({duration: 0.25}); })
            parent.insert(content)
            content.slideDown({delay: 0.2, duration: 0.25})
        }
    }

    _remove_canceled_inline_action_content(content) {
        let parent = content.parentNode
        content.remove()
        parent.childElements().each(function (x) { x.slideDown({duration: 0.25}); })
    }

}


pytis.ItemizedView = class extends pytis.BrowseForm {}


pytis.EditableBrowseForm = class extends pytis.BrowseForm {
    constructor(form_id, form_name, uri, inline_editable, allow_insertion) {
        /* form_id, form_name, uri, inline_editable ... defined by the
             super class.
           allow_insertion ... Add a button for insertion of new table
             rows.  Note that this is a different feature of this class
             than 'inline_editable' mode defined by the parent class.

        */
        super(form_id, form_name, uri, inline_editable)
        if (allow_insertion) {
            let button = $('<button class="new-row-button"><span>' +
                           pytis._("New row") +
                          '</span></button>')
            button.on('click', this._on_insert_new_row.bind(this))
            this.element.append(button)
        }
    }

    _bind_table_body(tbody) {
        // We don't call super here as we don't want editable cells or
        // expansible rows in EditableBrowseForm, but we may need it in
        // future if the parent class adds something we want...
        tbody.find('tr a.remove-row').on('click', this._on_remove_row)
    }

    _on_insert_new_row(event) {
        let form = this.element.find('form') || this.element.closest('form')
        if (form) {
            let parameters = {'_pytis_form_update_request': 1,
                              '_pytis_insert_new_row': 1}
            this._submit_form_xhr(form, parameters, (response, status, xhr) => {
                let tbody = this.element.find('table.data-table tbody')
                tbody.insert(response)
                form['_pytis_inserted_rows_' + this._form_name].value++
                this._bind_table_body(tbody)
            })
        }
        return false
    }

    _on_remove_row(event) {
        let tr = $(event.target).closest('tr')
        tr.closest('form').insert(
            `<input type="hidden"` +
            ` name="${_pytis_removed_row_key_}${this._form_name}"`
            ` value="${tr.attr('data-pytis-row-key')}">`
        )
        tr.remove()
        return false
    }

}


pytis.EditForm = class extends pytis.Form {
    /* This class implements AJAX updates during EditForm editation.
     *
     * The form is periodically checked for changes and the values are sent to the
     * server as asynchronous requests.  Based on the server response, the user
     * interface is then capable of:
     *   - displaying field validation info,
     *   - updating values of computed fields,
     *   - updating field editability dynamically,
     *   - updating enumerations based on pytis runtime filters and arguments.
     */

    constructor(form_id, fields) {
        /* form_id ... HTML id of the top level element (see parent class)
         * fields ... array of form fields as pytis.Field instances
         */
        super(form_id)
        // Note: this.element.closest() applies in a QueryFieldsForm (list form controls).
        this._form = this.element.find('form') || this.element.closest('form')
        this._initial_state = this._form.serialize()
        this._fields = new Map()
        this._last_request_number = 0
        this._last_request_data = undefined
        for (let field of fields) {
            this._fields.set(field.id(), field)
            field.observe(this._on_change.bind(this))
        }
    }

    _on_change(field, ctrl, value) {
        // Send AJAX request in reaction to user changes of form values.
        // TODO: Avoid AJAX request flooding during typing or other continuous
        // changes.  The problem is that we must always send a request as we
        // don't know whether another change comes soon.  Maybe we could send
        // the request after some delay if no other change comes in the
        // meantime, but this would slow down the UI responsivity.
        let values = this._form.serialize()
        if (values != this._last_request_data) {
            this._last_request_data = values
            document.body.style.cursor = "wait"
            let states = {}
            for (var [id, f] of this._fields) {
                if (f.state()) {
                    states[f.id()] = f.state()
                }
            }
            let parameters = {
                _pytis_form_update_request: ++this._last_request_number,
                _pytis_form_changed_field: field.id(),
                _pytis_form_state: $.param(states),
            }
            this._submit_form_xhr(this._form, parameters, this._update.bind(this))
        }
    }

    _update(response, status, xhr) {
        // Update the form state in reaction to previously sent AJAX request.
        let data = xhr.responseJSON
        if (data) {
            let response_number = parseInt(data.request_number, 10)
            // Ignore the response if other requests were sent in the meantime.
            // Only the most recently sent request really corresponds to the
            // current form state!  This also prevents processing responses
            // coming in wrong order (earlier request may be processed longer
            // than a later one).
            if (response_number === this._last_request_number) {
                for (var [key, cdata] of Object.entries(data.fields)) {
                    let field = this._fields.get(key)
                    if (field) {
                        try {
                            if (cdata.enumeration !== undefined) {
                                let original_value = field.value()
                                field.set_enumeration(cdata.enumeration, cdata.links)
                                if (cdata.value === undefined) {
                                    // Retain the value of enumeration fields
                                    // after enumeration changes if possible.
                                    field.set_value(original_value)
                                }
                            }
                            if (cdata.value !== undefined) {
                                field.set_value(cdata.value, cdata.localized_value)
                            }
                            if (cdata.editable !== undefined) {
                                field.set_editability(cdata.editable)
                            }
                            if (cdata.state !== undefined) {
                                field.set_state(cdata.state)
                            }
                        } catch (e) {
                            console.log(e)
                        }
                    }
                }
                document.body.style.cursor = "default"
            }
        } else {
            console.log("Empty AJAX response")
        }
    }

    _changed() {
        return this._form.serialize() != this._initial_state
    }

    cancel() {
        if (this._changed()) {
            let msg = (pytis._("The form data have changed!") + '\n' +
                       pytis._("Do you really want to close the form?"))
            if (!window.confirm(msg)) {
                return false
            }
        }
        return true
    }

}


pytis.Field = class extends pytis.Widget {
    constructor(form_id, field_id, id, state, active, required) {
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
        super(field_id)
        this._ctrl = this.element.parent().find(`[name="${id}"]`)
        this._id = id
        this._state = state
        this._active = active
        if (required) {
            // TODO: handle aria-required also for compound fields (radio group, checklist).
            this._ctrl.attr('aria-required', 'true')
        }
    }

    id() {
        return this._id
    }

    state() {
        // Return the last runtime filter/arguments state string representation.
        return this._state
    }

    observe(callback) {
        if (this._active) {
            this._ctrl.on('input', event => {
                callback(this, this._ctrl, this.value())
            })
        }
    }

    value() {
        return this._ctrl.val()
    }

    set_editability(value) {
        // Disable/enable field editability.
        let label = $('.field-label.id-' + this._id).first()
        if (label !== undefined) {
            if (value && label.hasClass('disabled')) {
                label.removeClass('disabled')
            }
            if (!value && !label.hasClass('disabled')) {
                label.addClass('disabled')
            }
        }
        this._set_editability(value)
    }

    set_state(value) {
        // Set runtime filter/arguments state string representation.
        this._state = value
    }

    _set_editability(value) {
        this._ctrl.prop('disabled', !value)
    }

    set_value(value, localized_value) {
        // Set the field value.
        if (localized_value !== undefined) {
            this._ctrl.val(localized_value)
        } else {
            this._ctrl.val(value)
        }
    }

    set_enumeration(value, links) {
        // Update enumeration controls (only for enumeration fields).
        return
    }

}

pytis.CheckboxField = class extends pytis.Field {

    set_value(value, localized_value) {
        // Set the field value.
        this._ctrl.prop('checked', value === 'T')
    }

}

pytis.RadioField = class extends pytis.Field {

    set_value(value, localized_value) {
        for (let radio of this._ctrl) {
            radio.checked = radio.value === value
        }
    }

}

pytis.PasswordField = class extends pytis.Field {

}

pytis.ChoiceField = class extends pytis.Field {
    // Enumeration field represented by HTML select control.

    set_enumeration(value, links) {
        let selected = this._ctrl.val()
        let null_option = this._ctrl.find('option[value=""]')
        this._ctrl.empty()
        this._ctrl.insert(null_option)
        for (let item of value) {
            // Append options according to the new enumeration received.
            this._ctrl.insert(`<option value="${item[0]}" selected="${item[0] === selected}">` +
                              item[1].escapeHTML().gsub(' ', '&nbsp;') +
                              `</option>`)
        }
    }

}

pytis.ChecklistField = class extends pytis.Field {
    // Multi select control represented by a group of checkboxes.

    _checkboxes() {
        return this.element.immediateDescendants().collect(function (item) {
            return item.firstDescendant()
        })
    }

    value() {
        let checkboxes = this._checkboxes()
        return checkboxes.map(function (checkbox) {
            return checkbox.value
        })
    }

    set_value(value, localized_value) {
        this._checkboxes().each(function (checkbox) {
            checkbox.checked = false
            if (value) {
                let j
                for (j=0; j<value.length; j++) {
                    if (value[j] === checkbox.value) {
                        checkbox.checked = true
                    }
                }
            }
        })
    }

    _set_editability(value) {
        this._checkboxes().each(function (checkbox) {
            checkbox.disabled = !value
        })
    }

    set_enumeration(value, links) {
        this.element.children().remove()
        this.element[0].cleanWhitespace()
        for (let i=0, len=value.length; i < len; ++i) {
            //Append options according to the new enumeration received
            let [val, label] = value[i]
            let id = this.element.attr('id') + '-' + i
            let div = $(`<div>` +
                        `<input type="checkbox" value="${val}" name="${this._id}" id="${id}">` +
                        `&nbsp;` +
                        `<label for="${id}">${label.escapeHTML().gsub(' ', '&nbsp;')}</label>` +
                        `</div>`)
            if (links && links[val]) {
                div.insert(`&nbsp;[<a href="${links[val]}">${val}</a>]`)
            }
            this.element.insert(div)
        }
    }

}

pytis.HtmlField = class extends pytis.Field {
    constructor(form_id, field_id, id, state, active, required) {
        super(form_id, field_id, id, state, active, required)
        if (CKEDITOR !== undefined) {
            // The function pytis.HtmlField.plugin is defined in pytis-ckeditor.js.
            CKEDITOR.plugins.add('pytis-attachments', {init: pytis.HtmlField.plugin})
            CKEDITOR.on('dialogDefinition', pytis.HtmlField.on_dialog)
        }
    }

    _attachment_storage_request(request, parameters) {
        parameters._pytis_form_update_request = 1
        parameters._pytis_attachment_storage_field = this._id
        parameters._pytis_attachment_storage_request = request
        this._submit_form_xhr(this.element.closest('form'), parameters, callback)
    }

    get_attachment(filename, callback) {
        return this._attachment_storage_request('get', {filename: filename}, callback)
    }

    list_attachments(callback) {
        return this._attachment_storage_request('list', {}, callback)
    }

    // Insert attachment doesn't go through AJAX due to browser limitations.
    // Iframe upload is performed as implemented in CKeditor and
    // customized in ckeditor-plugin.js so a method is not needed here.

    update_attachment(filename, values, callback) {
        this._attachment_storage_request('update', {filename: filename,
                                                    values: Object.toJSON(values)}, callback)
    }
}

pytis.DateTimeField = class extends pytis.Field {
    _set_editability(value) {
        super._set_editability(value)
        $(`#${this._ctrl.attr('id')}-button`).prop('disabled', !value)
    }
}

pytis.FileField = class extends pytis.Field {

    constructor(form_id, field_id, id, state, active, required) {
        super(form_id, field_id, id, state, active, required)
        // Listen to file field changes separately because pytis form updates
        // currently only listen to "active" fields (field is active
        // when computers depend on it).  When we ever decide to send ajax
        // updates for all fields (to support continuous validation) we can
        // solve file size validation within the main form updates.
        this._ctrl.change(this._on_change)
    }

    _on_change(event) {
        let files = this._ctrl[0].files
        if (files && files.length) {
            let file = files[0]
            document.body.style.cursor = "wait"
            let parameters = {
                _pytis_form_update_request: true,
                _pytis_form_changed_field: this._id,
                ['_pytis_file_size_' + this._id]: file.size,
                [this._id]: null, // Avoid sending the whole file through ajax.
            }
            this._submit_form_xhr(this.element.closest('form'), parameters, this._update)
        }
    }

    _update(response) {
        let data = response.responseJSON
        if (data) {
            let error = data.fields[this._id].error
            let div = this._ctrl.next('.error').first()
            let submit = this.element.find('button[type="submit"]')
            if (error) {
                if (!div.length) {
                    div = $(`<div class="error"></div>`)
                    div.insertAfter(this._ctrl)
                }
                div.html(error)
                if (submit.length) {
                    // Protect the server from invalid (most likely oversized) file uploads.
                    submit.prop('disabled', true)
                }
            } else {
                div.remove()
                if (submit.length) {
                    submit.prop('disabled', false)
                }
            }
        }
        document.body.style.cursor = "default"
    }

}


/* -------------------------------------------------------------------------------------
 * Customized copy of W3C date picker from
 * https://www.w3.org/TR/wai-aria-practices/examples/dialog-modal/datepicker-dialog.html
 * -------------------------------------------------------------------------------------
 *
 * The code below is licensed according to the W3C Software License at
 * https://www.w3.org/Consortium/Legal/2015/copyright-software-and-document
 *
 * -------------------------------------------------------------------------------------
 */

var CalendarButtonInput = function (inputNode, buttonNode, datepicker) {
  this.inputNode    = inputNode
  this.buttonNode   = buttonNode
  this.imageNode    = false

  this.datepicker = datepicker

  this.defaultLabel = 'Choose Date'

  this.keyCode = Object.freeze({
    'ENTER': 13,
    'SPACE': 32
  })
}

CalendarButtonInput.prototype.init = function () {
  this.buttonNode.addEventListener('click', this.handleClick.bind(this))
  this.buttonNode.addEventListener('keydown', this.handleKeyDown.bind(this))
  this.buttonNode.addEventListener('focus', this.handleFocus.bind(this))
}

CalendarButtonInput.prototype.handleKeyDown = function (event) {
  var flag = false

  switch (event.keyCode) {

    case this.keyCode.SPACE:
    case this.keyCode.ENTER:
      this.datepicker.show()
      this.datepicker.setFocusDay()
      flag = true
      break

    default:
      break
  }

  if (flag) {
    event.stopPropagation()
    event.preventDefault()
  }
}

CalendarButtonInput.prototype.handleClick = function () {
  if (!this.datepicker.isOpen()) {
    this.datepicker.show()
    this.datepicker.setFocusDay()
  }
  else {
    this.datepicker.hide()
  }

  event.stopPropagation()
  event.preventDefault()

}

CalendarButtonInput.prototype.setLabel = function (str) {
  if (typeof str === 'string' && str.length) {
    str = ', ' + str
  }
  this.buttonNode.setAttribute('aria-label', this.defaultLabel + str)
}

CalendarButtonInput.prototype.setFocus = function () {
  this.buttonNode.focus()
}

CalendarButtonInput.prototype.setDate = function (day) {
  this.inputNode.value = (day.getMonth() + 1) + '/' + day.getDate() + '/' + day.getFullYear()
}

CalendarButtonInput.prototype.getDate = function () {
  return this.inputNode.value
}

CalendarButtonInput.prototype.getDateLabel = function () {
  var label = ''

  var parts = this.inputNode.value.split('/')

  if ((parts.length === 3) &&
      Number.isInteger(parseInt(parts[0])) &&
      Number.isInteger(parseInt(parts[1])) &&
      Number.isInteger(parseInt(parts[2]))) {
    var month = parseInt(parts[0]) - 1
    var day = parseInt(parts[1])
    var year = parseInt(parts[2])

    label = this.datepicker.getDateForButtonLabel(year, month, day)
  }

  return label
}

CalendarButtonInput.prototype.handleFocus = function () {
  var dateLabel = this.getDateLabel()

  if (dateLabel) {
    this.setLabel('selected date is ' + dateLabel)
  }
  else {
    this.setLabel('')
  }
}

// Initialize menu button date picker

window.addEventListener('load' , function () {

  var datePickers = document.querySelectorAll('.datepicker')

  datePickers.forEach(function (dp) {
    var inputNode   = dp.querySelector('input')
    var buttonNode  = dp.querySelector('button')
    var dialogNode  = dp.querySelector('[role=dialog]')

    var datePicker = new DatePicker(inputNode, buttonNode, dialogNode)
    datePicker.init()
  })

})


var DatePicker = function (inputNode, buttonNode, dialogNode) {
  this.dayLabels = ['Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday']
  this.monthLabels = ['January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December']

  this.messageCursorKeys = 'Cursor keys can navigate dates'
  this.lastMessage = ''

  this.inputNode   = inputNode
  this.buttonNode  = buttonNode
  this.dialogNode  = dialogNode
  this.messageNode = dialogNode.querySelector('.message')

  this.dateInput = new CalendarButtonInput(this.inputNode, this.buttonNode, this)

  this.MonthYearNode = this.dialogNode.querySelector('.monthYear')

  this.prevYearNode = this.dialogNode.querySelector('.prevYear')
  this.prevMonthNode = this.dialogNode.querySelector('.prevMonth')
  this.nextMonthNode = this.dialogNode.querySelector('.nextMonth')
  this.nextYearNode = this.dialogNode.querySelector('.nextYear')

  this.okButtonNode = this.dialogNode.querySelector('button[value="ok"]')
  this.cancelButtonNode = this.dialogNode.querySelector('button[value="cancel"]')

  this.tbodyNode = this.dialogNode.querySelector('table.dates tbody')

  this.lastRowNode = null

  this.days = []

  this.focusDay = new Date()
  this.selectedDay = new Date(0,0,1)

  this.isMouseDownOnBackground = false

  this.keyCode = Object.freeze({
    'TAB': 9,
    'ENTER': 13,
    'ESC': 27,
    'SPACE': 32,
    'PAGEUP': 33,
    'PAGEDOWN': 34,
    'END': 35,
    'HOME': 36,
    'LEFT': 37,
    'UP': 38,
    'RIGHT': 39,
    'DOWN': 40
  })

}

DatePicker.prototype.init = function () {

  this.dateInput.init()

  this.okButtonNode.addEventListener('click', this.handleOkButton.bind(this))
  this.okButtonNode.addEventListener('keydown', this.handleOkButton.bind(this))

  this.cancelButtonNode.addEventListener('click', this.handleCancelButton.bind(this))
  this.cancelButtonNode.addEventListener('keydown', this.handleCancelButton.bind(this))

  this.prevMonthNode.addEventListener('click', this.handlePreviousMonthButton.bind(this))
  this.nextMonthNode.addEventListener('click', this.handleNextMonthButton.bind(this))
  this.prevYearNode.addEventListener('click', this.handlePreviousYearButton.bind(this))
  this.nextYearNode.addEventListener('click', this.handleNextYearButton.bind(this))

  this.prevMonthNode.addEventListener('keydown', this.handlePreviousMonthButton.bind(this))
  this.nextMonthNode.addEventListener('keydown', this.handleNextMonthButton.bind(this))
  this.prevYearNode.addEventListener('keydown', this.handlePreviousYearButton.bind(this))

  this.nextYearNode.addEventListener('keydown', this.handleNextYearButton.bind(this))

  document.body.addEventListener('mousedown', this.handleBackgroundMouseDown.bind(this), true)
  document.body.addEventListener('mouseup', this.handleBackgroundMouseUp.bind(this), true)

  // Create Grid of Dates

  this.tbodyNode.innerHTML = ''
  var index = 0
  for (var i = 0; i < 6; i++) {
    var row = this.tbodyNode.insertRow(i)
    this.lastRowNode = row
    row.classList.add('dateRow')
    for (var j = 0; j < 7; j++) {
      var cell = document.createElement('td')
      cell.classList.add('dateCell')
      var cellButton = document.createElement('button')
      cellButton.classList.add('dateButton')
      cell.appendChild(cellButton)
      row.appendChild(cell)
      var dpDay = new DatePickerDay(cellButton, this, index, i, j)
      dpDay.init()
      this.days.push(dpDay)
      index++
    }
  }

  this.updateGrid()
  this.setFocusDay()
}

DatePicker.prototype.updateGrid = function () {

  var i, flag
  var fd = this.focusDay

  this.MonthYearNode.innerHTML = this.monthLabels[fd.getMonth()] + ' ' + fd.getFullYear()

  var firstDayOfMonth = new Date(fd.getFullYear(), fd.getMonth(), 1)
  var daysInMonth = new Date(fd.getFullYear(), fd.getMonth() + 1, 0).getDate()
  var dayOfWeek = firstDayOfMonth.getDay()

  firstDayOfMonth.setDate(firstDayOfMonth.getDate() - dayOfWeek)

  var d = new Date(firstDayOfMonth)

  for (i = 0; i < this.days.length; i++) {
    flag = d.getMonth() != fd.getMonth()
    this.days[i].updateDay(flag, d)
    if ((d.getFullYear() == this.selectedDay.getFullYear()) &&
        (d.getMonth() == this.selectedDay.getMonth()) &&
        (d.getDate() == this.selectedDay.getDate())) {
      this.days[i].domNode.setAttribute('aria-selected', 'true')
    }
    d.setDate(d.getDate() + 1)
  }

  if ((dayOfWeek + daysInMonth) < 36) {
    this.hideLastRow()
  }
  else {
    this.showLastRow()
  }

}

DatePicker.prototype.hideLastRow = function () {
  this.lastRowNode.style.visibility = 'hidden'
}

DatePicker.prototype.showLastRow = function () {
  this.lastRowNode.style.visibility = 'visible'
}

DatePicker.prototype.setFocusDay = function (flag) {

  if (typeof flag !== 'boolean') {
    flag = true
  }

  var fd = this.focusDay

  function checkDay (d) {
    d.domNode.setAttribute('tabindex', '-1')
    if ((d.day.getDate()  == fd.getDate()) &&
        (d.day.getMonth() == fd.getMonth()) &&
        (d.day.getFullYear() == fd.getFullYear())) {
      d.domNode.setAttribute('tabindex', '0')
      if (flag) {
        d.domNode.focus()
      }
    }
  }

  this.days.forEach(checkDay.bind(this))
}

DatePicker.prototype.updateDay = function (day) {
  var d = this.focusDay
  this.focusDay = day
  if ((d.getMonth() !== day.getMonth()) ||
      (d.getFullYear() !== day.getFullYear())) {
    this.updateGrid()
    this.setFocusDay()
  }
}

DatePicker.prototype.getDaysInLastMonth = function () {
  var fd = this.focusDay
  var lastDayOfMonth = new Date(fd.getFullYear(), fd.getMonth(), 0)
  return lastDayOfMonth.getDate()
}

DatePicker.prototype.getDaysInMonth = function () {
  var fd = this.focusDay
  var lastDayOfMonth = new Date(fd.getFullYear(), fd.getMonth() + 1, 0)
  return lastDayOfMonth.getDate()
}

DatePicker.prototype.show = function () {

  this.dialogNode.style.display = 'block'
  this.dialogNode.style.zIndex = 2

  this.getDateInput()
  this.updateGrid()
  this.setFocusDay()

}

DatePicker.prototype.isOpen = function () {
  return window.getComputedStyle(this.dialogNode).display !== 'none'
}

DatePicker.prototype.hide = function () {

  this.setMessage('')

  this.dialogNode.style.display = 'none'

  this.hasFocusFlag = false
  this.dateInput.setFocus()
}

DatePicker.prototype.handleBackgroundMouseDown = function (event) {
  if (!this.buttonNode.contains(event.target) &&
      !this.dialogNode.contains(event.target)) {

    this.isMouseDownOnBackground = true

    if (this.isOpen()) {
      this.hide()
      event.stopPropagation()
      event.preventDefault()
    }
  }
}

DatePicker.prototype.handleBackgroundMouseUp = function () {
  this.isMouseDownOnBackground = false
}


DatePicker.prototype.handleOkButton = function (event) {
  var flag = false

  switch (event.type) {
    case 'keydown':

      switch (event.keyCode) {
        case this.keyCode.ENTER:
        case this.keyCode.SPACE:

          this.setTextboxDate()

          this.hide()
          flag = true
          break

        case this.keyCode.TAB:
          if (!event.shiftKey) {
            this.prevYearNode.focus()
            flag = true
          }
          break

        case this.keyCode.ESC:
          this.hide()
          flag = true
          break

        default:
          break

      }
      break

    case 'click':
      this.setTextboxDate()
      this.hide()
      flag = true
      break

    default:
      break
  }

  if (flag) {
    event.stopPropagation()
    event.preventDefault()
  }
}

DatePicker.prototype.handleCancelButton = function (event) {
  var flag = false

  switch (event.type) {
    case 'keydown':

      switch (event.keyCode) {
        case this.keyCode.ENTER:
        case this.keyCode.SPACE:
          this.hide()
          flag = true
          break

        case this.keyCode.ESC:
          this.hide()
          flag = true
          break

        default:
          break

      }
      break

    case 'click':
      this.hide()
      flag = true
      break

    default:
      break
  }

  if (flag) {
    event.stopPropagation()
    event.preventDefault()
  }
}

DatePicker.prototype.handleNextYearButton = function (event) {
  var flag = false

  switch (event.type) {

    case 'keydown':

      switch (event.keyCode) {
        case this.keyCode.ESC:
          this.hide()
          flag = true
          break

        case this.keyCode.ENTER:
        case this.keyCode.SPACE:
          this.moveToNextYear()
          this.setFocusDay(false)
          flag = true
          break
      }

      break

    case 'click':
      this.moveToNextYear()
      this.setFocusDay(false)
      break

    default:
      break
  }

  if (flag) {
    event.stopPropagation()
    event.preventDefault()
  }
}

DatePicker.prototype.handlePreviousYearButton = function (event) {
  var flag = false

  switch (event.type) {

    case 'keydown':

      switch (event.keyCode) {

        case this.keyCode.ENTER:
        case this.keyCode.SPACE:
          this.moveToPreviousYear()
          this.setFocusDay(false)
          flag = true
          break

        case this.keyCode.TAB:
          if (event.shiftKey) {
            this.okButtonNode.focus()
            flag = true
          }
          break

        case this.keyCode.ESC:
          this.hide()
          flag = true
          break

        default:
          break
      }

      break

    case 'click':
      this.moveToPreviousYear()
      this.setFocusDay(false)
      break

    default:
      break
  }

  if (flag) {
    event.stopPropagation()
    event.preventDefault()
  }
}

DatePicker.prototype.handleNextMonthButton = function (event) {
  var flag = false

  switch (event.type) {

    case 'keydown':

      switch (event.keyCode) {
        case this.keyCode.ESC:
          this.hide()
          flag = true
          break

        case this.keyCode.ENTER:
        case this.keyCode.SPACE:
          this.moveToNextMonth()
          this.setFocusDay(false)
          flag = true
          break
      }

      break

    case 'click':
      this.moveToNextMonth()
      this.setFocusDay(false)
      break

    default:
      break
  }

  if (flag) {
    event.stopPropagation()
    event.preventDefault()
  }
}

DatePicker.prototype.handlePreviousMonthButton = function (event) {
  var flag = false

  switch (event.type) {

    case 'keydown':

      switch (event.keyCode) {
        case this.keyCode.ESC:
          this.hide()
          flag = true
          break

        case this.keyCode.ENTER:
        case this.keyCode.SPACE:
          this.moveToPreviousMonth()
          this.setFocusDay(false)
          flag = true
          break
      }

      break

    case 'click':
      this.moveToPreviousMonth()
      this.setFocusDay(false)
      flag = true
      break

    default:
      break
  }

  if (flag) {
    event.stopPropagation()
    event.preventDefault()
  }
}

DatePicker.prototype.moveToNextYear = function () {
  this.focusDay.setFullYear(this.focusDay.getFullYear() + 1)
  this.updateGrid()
}

DatePicker.prototype.moveToPreviousYear = function () {
  this.focusDay.setFullYear(this.focusDay.getFullYear() - 1)
  this.updateGrid()
}

DatePicker.prototype.moveToNextMonth = function () {
  this.focusDay.setMonth(this.focusDay.getMonth() + 1)
  this.updateGrid()
}

DatePicker.prototype.moveToPreviousMonth = function () {
  this.focusDay.setMonth(this.focusDay.getMonth() - 1)
  this.updateGrid()
}

DatePicker.prototype.moveFocusToDay = function (day) {
  var d = this.focusDay

  this.focusDay = day

  if ((d.getMonth() != this.focusDay.getMonth()) ||
      (d.getYear() != this.focusDay.getYear())) {
    this.updateGrid()
  }
  this.setFocusDay()
}

DatePicker.prototype.moveFocusToNextDay = function () {
  var d = new Date(this.focusDay)
  d.setDate(d.getDate() + 1)
  this.moveFocusToDay(d)
}

DatePicker.prototype.moveFocusToNextWeek = function () {
  var d = new Date(this.focusDay)
  d.setDate(d.getDate() + 7)
  this.moveFocusToDay(d)
}

DatePicker.prototype.moveFocusToPreviousDay = function () {
  var d = new Date(this.focusDay)
  d.setDate(d.getDate() - 1)
  this.moveFocusToDay(d)
}

DatePicker.prototype.moveFocusToPreviousWeek = function () {
  var d = new Date(this.focusDay)
  d.setDate(d.getDate() - 7)
  this.moveFocusToDay(d)
}

DatePicker.prototype.moveFocusToFirstDayOfWeek = function () {
  var d = new Date(this.focusDay)
  d.setDate(d.getDate() - d.getDay())
  this.moveFocusToDay(d)
}

DatePicker.prototype.moveFocusToLastDayOfWeek = function () {
  var d = new Date(this.focusDay)
  d.setDate(d.getDate() + (6 - d.getDay()))
  this.moveFocusToDay(d)
}

DatePicker.prototype.setTextboxDate = function (day) {
  if (day) {
    this.dateInput.setDate(day)
  }
  else {
    this.dateInput.setDate(this.focusDay)
  }
}

DatePicker.prototype.getDateInput = function () {

  var parts = this.dateInput.getDate().split('/')

  if ((parts.length === 3) &&
      Number.isInteger(parseInt(parts[0])) &&
      Number.isInteger(parseInt(parts[1])) &&
      Number.isInteger(parseInt(parts[2]))) {
    this.focusDay = new Date(parseInt(parts[2]), parseInt(parts[0]) - 1, parseInt(parts[1]))
    this.selectedDay = new Date(this.focusDay)
  }
  else {
    // If not a valid date (MM/DD/YY) initialize with todays date
    this.focusDay = new Date()
    this.selectedDay = new Date(0,0,1)
  }

}

DatePicker.prototype.getDateForButtonLabel = function (year, month, day) {
  if (typeof year !== 'number' || typeof month !== 'number' || typeof day !== 'number') {
    this.selectedDay = this.focusDay
  }
  else {
    this.selectedDay = new Date(year, month, day)
  }

  var label = this.dayLabels[this.selectedDay.getDay()]
  label += ' ' + this.monthLabels[this.selectedDay.getMonth()]
  label += ' ' + (this.selectedDay.getDate())
  label += ', ' + this.selectedDay.getFullYear()
  return label
}

DatePicker.prototype.setMessage = function (str) {

  function setMessageDelayed () {
    this.messageNode.textContent = str
  }

  if (str !== this.lastMessage) {
    setTimeout(setMessageDelayed.bind(this), 200)
    this.lastMessage = str
  }
}


var DatePickerDay = function (domNode, datepicker, index, row, column) {

  this.index = index
  this.row = row
  this.column = column

  this.day = new Date()

  this.domNode = domNode
  this.datepicker = datepicker

  this.keyCode = Object.freeze({
    'TAB': 9,
    'ENTER': 13,
    'ESC': 27,
    'SPACE': 32,
    'PAGEUP': 33,
    'PAGEDOWN': 34,
    'END': 35,
    'HOME': 36,
    'LEFT': 37,
    'UP': 38,
    'RIGHT': 39,
    'DOWN': 40
  })
}

DatePickerDay.prototype.init = function () {
  this.domNode.setAttribute('tabindex', '-1')
  this.domNode.addEventListener('mousedown', this.handleMouseDown.bind(this))
  this.domNode.addEventListener('keydown', this.handleKeyDown.bind(this))
  this.domNode.addEventListener('focus', this.handleFocus.bind(this))

  this.domNode.innerHTML = '-1'

}

DatePickerDay.prototype.isDisabled = function () {
  return this.domNode.classList.contains('disabled')
}

DatePickerDay.prototype.updateDay = function (disable, day) {

  if (disable) {
    this.domNode.classList.add('disabled')
  }
  else {
    this.domNode.classList.remove('disabled')
  }

  this.day = new Date(day)

  this.domNode.innerHTML = this.day.getDate()
  this.domNode.setAttribute('tabindex', '-1')
  this.domNode.removeAttribute('aria-selected')

  var d = this.day.getDate().toString()
  if (this.day.getDate() < 9) {
    d = '0' + d
  }

  var m = this.day.getMonth() + 1
  if (this.day.getMonth() < 9) {
    m = '0' + m
  }

  this.domNode.setAttribute('data-date', this.day.getFullYear() + '-' + m + '-' + d)

}

DatePickerDay.prototype.handleKeyDown = function (event) {
  var flag = false

  switch (event.keyCode) {

    case this.keyCode.ESC:
      this.datepicker.hide()
      break

    case this.keyCode.TAB:
      this.datepicker.cancelButtonNode.focus()
      if (event.shiftKey) {
        this.datepicker.nextYearNode.focus()
      }
      this.datepicker.setMessage('')
      flag = true
      break

    case this.keyCode.ENTER:
    case this.keyCode.SPACE:
      this.datepicker.setTextboxDate(this.day)
      this.datepicker.hide()
      flag = true
      break

    case this.keyCode.RIGHT:
      this.datepicker.moveFocusToNextDay()
      flag = true
      break

    case this.keyCode.LEFT:
      this.datepicker.moveFocusToPreviousDay()
      flag = true
      break

    case this.keyCode.DOWN:
      this.datepicker.moveFocusToNextWeek()
      flag = true
      break

    case this.keyCode.UP:
      this.datepicker.moveFocusToPreviousWeek()
      flag = true
      break

    case this.keyCode.PAGEUP:
      if (event.shiftKey) {
        this.datepicker.moveToPreviousYear()
      }
      else {
        this.datepicker.moveToPreviousMonth()
      }
      flag = true
      break

    case this.keyCode.PAGEDOWN:
      if (event.shiftKey) {
        this.datepicker.moveToNextYear()
      }
      else {
        this.datepicker.moveToNextMonth()
      }
      flag = true
      break

    case this.keyCode.HOME:
      this.datepicker.moveFocusToFirstDayOfWeek()
      flag = true
      break

    case this.keyCode.END:
      this.datepicker.moveFocusToLastDayOfWeek()
      flag = true
      break
  }

  if (flag) {
    event.stopPropagation()
    event.preventDefault()
  }

}

DatePickerDay.prototype.handleMouseDown = function (event) {

  if (this.isDisabled()) {
    this.datepicker.moveFocusToDay(this.date)
  }
  else {
    this.datepicker.setTextboxDate(this.day)
    this.datepicker.hide()
  }

  event.stopPropagation()
  event.preventDefault()

}

DatePickerDay.prototype.handleFocus = function () {
  this.datepicker.setMessage(this.datepicker.messageCursorKeys)
}
