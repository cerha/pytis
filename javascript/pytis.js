/* -*- coding: utf-8 -*-
 *
 * Copyright (C) 2009-2017 OUI Technology Ltd.
 * Copyright (C) 2019-2026 Tomáš Cerha <t.cerha@gmail.com>
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


window.pytis = {
    _: lcg.gettext('pytis-web'),
}


pytis.show_tooltip = function (event, url) {
    // This can't be implemented as a Field method as Field instances are not
    // created in BrowseForm (currently only in edit form).
    let element = event.target
    element._pytis_tooltip_timeout = setTimeout(function () {
        element._pytis_tooltip = new lcg.Tooltip(url, event.x, event.y)
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


pytis.Form = class extends lcg.Widget {
    /* Common base class of all Pytis HTML forms. */
}


pytis.BrowseForm = class extends pytis.Form {
    /* Handles asynchronous load of a Pytis browse form.

    The form content is loaded through an AJAX request after the page is
    displayed.  Special handling is applied to forms which are placed inside
    notebook tabs (of lcg.Notebook widget).  Such forms are not loaded
    immediately, but in a callback after the tab is activated.

     */
    constructor(form_id, form_name, url, inline_editable) {
        /* form_id ... HTML id of the top level element (see parent class)
           form_name ... Form name used for distinguishing request parameters
             (see form_name in the python class).
           url ... URL for AJAX requests
           inline_editable ... perform basic operations inline.  Currently
             only the 'update' action is supported, but others, such as
             insert/delete may be supported in future too.  Inline means,
             that the operations are not performed on separate pages, but
             inside the current list form through asynchronous requests.
             For example 'update' will submit the action URL asynchronously
             and display the result returned by server inside the table
             row replacing the original row content.

         */
        super(form_id)
        this._form_name = form_name
        this._url = url
        this._inline_editable = inline_editable
        this._ajax_container = this.element.find('.ajax-container')
        this._on_load_callbacks = []
        if (inline_editable) {
            this.element.find('button.action-insert')
                .on('click', event => {
                    let button = $(event.target).closest('button')
                    button.prop('disabled', true)
                    this._send_inline_action_request(button, button.closest('form'), {}, 'global')
                    return false
                })
        }
        if (url && this._ajax_container.length != 0) {
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
        this._send_edit_cell_request($(event.target).closest('td'))
        return false
    }

    _send_edit_cell_request(td, form) {
        let column_id = this._pytis_column_id(td)
        this._ajax({
            url: this._url,
            form: form,
            data: {
                _pytis_form_update_request: 1,
                _pytis_edit_cell: 1,
                _pytis_row_key: this._pytis_row_key(td),
                _pytis_column_id: column_id,
            },
        }, (response, status, xhr) => {
            td.html(response)
            let edit_form = td.find('form')
            if (edit_form.length) {
                edit_form.find('button.save-edited-cell').on('click', e => {
                    this._send_edit_cell_request(td, edit_form)
                    return false
                })
                edit_form.find(`[name=${column_id}]`).focus()
            }
        })
    }

    _toggle_row_expansion(tr) {
        if (tr.hasClass('expanded')) {
            this._collapse_row(tr)
        } else if (tr.next().hasClass('row-expansion')) {
            this._expand_row(tr)
        } else {
            this._send_expand_row_request(tr)
        }
    }

    _row_expansion_content(tr) {
        // No idea why this doesn't work:
        //return tr.next().find('.row-expansion-content')
        return tr.next().children().first().children().first()
    }

    _collapse_row(tr) {
        let link = tr.find('a.expand-row')
        let content = this._row_expansion_content(tr)
        tr.removeClass('expanded')
        content.slideUp({
            duration: 250,
            done: () => {
                content.closest('tr').hide()
                link.attr('aria-expanded', 'false')
                link.text(pytis._("Expand Row"))
            },
        })
        link.focus()
    }

    _expand_row(tr) {
        let link = tr.find('a.expand-row')
        let content = this._row_expansion_content(tr)
        tr.addClass('expanded')
        content.closest('tr').show()
        content.slideDown({
            duration: 250,
            done: () => {
                link.attr('aria-expanded', 'true')
                link.text(pytis._("Collapse Row"))
            },
        })
    }

    _send_expand_row_request(tr) {
        this._ajax({
            form: this.element.find('form.list-form-controls'),
            data: {
                _pytis_form_update_request: 1,
                _pytis_expand_row: 1,
                _pytis_row_key: this._pytis_row_key(tr),
            },
        }, (response, status, xhr) => {
            // Beware: The elements created here must follow the same structure as in
            // the Python code (the non-async version). The JS code doesn't make any
            // difference.
            $(`<tr class="row-expansion ${tr.hasClass('odd') ? 'odd' : 'even'}">`)
                .append($(`<td colspan="${tr.children('td').length}">`)
                        .append($('<div class"row-expansion-content">')
                                .hide()
                                .append(response)
                                .append($('<a class="collapse-row">').text(pytis._("Collapse Row"))
                                        .on('click', e => this._toggle_row_expansion(tr)))))
                .insertAfter(tr)
            this._expand_row(tr)
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

    _pytis_column_id(td) {
        // Return pytis column id for given HTML element inside the pytis table form.
        /* The method works with with class names of table th elements, but this
           may be unreliable in some cases, so better we might pass column names
           explicitly to the JavaScript form class constructor or something
           similar...
        */
        let tr = td.closest('tr')
        let hr = td.closest('table').find('thead tr')
        if (tr.length && hr.length) {
            let th = hr.children().eq(tr.children().index(td[0]))
            let cls = th.attr('class').split(' ').find(x => x.startsWith('column-id-'))
            if (cls) {
                return cls.substr(10, cls.length)
            }
        }
        return undefined
    }

    _load_form_data(parameters) {
        parameters._pytis_async_load_request = 1
        this._ajax({
            url: this._url,
            method: 'POST',
            data: parameters,
        }, (response, status, xhr) => {
            let container = this._ajax_container
            container.html(response)
            this._bind_controls(container.find('.list-form-controls:eq(0)'))
            this._bind_controls(container.find('.list-form-controls:eq(1)'))
            this._bind_table_headings(container.find('table.data-table thead'))
            this._bind_table_body(container.find('table.data-table tbody'))
            for (let callback of this._on_load_callbacks) {
                callback(this.element)
            }
            if (container.find('#found-record').length) {
                window.location.hash = '#found-record'
            }
        }, (xhr) => {
            let div = $('<div class="form-load-error">').text(
                pytis._("Failed loading form:") + ' ' + xhr.status + ' ' + xhr.statusText
            )
            this._ajax_container.html(div)
            let tb_start = xhr.responseText.search('here is the original traceback:')
            if (tb_start !== -1) {
                let tb_text = xhr.responseText.slice(tb_start + 32)
                let traceback = $(`<pre class="form-load-traceback" style="display: none">`)
                    .text(tb_text.slice(0, tb_text.search('-->')))
                let toggle = $('<a href="#">').text(pytis._("show details"))
                    .click(e => traceback.slideToggle())
                div.append([' (', toggle, ')'])
                this._ajax_container.append(traceback)
            }
        }).always(() => { document.body.style.cursor = "default" })
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
        if (apply_button.length === 0) {
            panel.find('select, checkbox, radio')
                .on('change', event => {
                    this._reload_form_data(form, {})
                    return false
                })
        }
        panel.find('.index-search-controls a').on('click', event => {
            let params = (new URL(event.target.href.replace(/;/g, '&'))).searchParams
            this._reload_form_data(form, {index_search: params.get('index_search')})
            return false
        })
        this._bind_search_controls(panel)
    }

    _bind_table_headings(thead) {
        thead.find('th.column-heading.sortable-column')
            .on('click', this._on_table_heading_clicked.bind(this))
        if (thead.parent('table').hasClass('expansible-rows')) {
            thead.find('tr').prepend('<th class="column-heading expansion-ctrl-heading">')
        }
    }

    _bind_table_body(tbody) {
        tbody.find('td.editable-cell')
            .attr('title', pytis._("Double click the cell to edit the value."))
            .on('dblclick', this._on_edit_cell.bind(this))
        let expansible = tbody.parent('table').hasClass('expansible-rows')
        if (expansible || this._inline_editable) {
            tbody.find('tr.data-row').each((i, tr) => {
                if (expansible) {
                    $(tr).prepend($('<td class="expansion-ctrl">').append(
                        $('<a class="expand-row">').text(pytis._("Expand Row"))
                            .on('click', e => this._toggle_row_expansion($(tr)))
                    ))
                }
                if (this._inline_editable) {
                    let url = $(tr).attr('data-pytis-row-update-url')
                    if (url) {
                        $(tr).on('dblclick', e => {
                            this._on_popup_menu_inline_action($(tr), 'update', url)
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
        this._load_form_data(parameters)
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
                this._load_form_data(parameters)
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
        search_controls.find('input[type=hidden]').val('1')
        this.element.find('.list-form-controls').each(function () {
            $(this).find('.paging-controls button.search').hide()
        })
        return false
    }

    _on_hide_search_controls(event) {
        let search_controls = this.element.find('div.query')
        let form = search_controls.closest('form')
        form.find('input[name="show-search-field"]').val('')
        form.find('input[name="query"]').val('')
        form.submit()
        return false
    }

    _on_popup_menu_inline_action(element, action, url) {
        if (action === 'update' || action === 'copy' || action === 'delete') {
            let u = new URL(url.replace(/;/g, '&'), window.location)
            let form = $(`<form action="${u.pathname}" method="GET">`)
            let parameters = Object.fromEntries(u.searchParams)
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
        if (content.length && !this._cancel_inline_action(content)) {
            return
        }
        parameters['_pytis_inline_form_request'] = '1'
        this._ajax({
            form: form,
            data: parameters,
        }, (response, status, xhr) => {
            let found = this.element.find('#found-record')
            if (found.length) {
                // Unhighlight the previously highlighted record to avoid confusion
                // (the edited record is now the one worth to be noticed).
                found.attr('id', '')
            }
            // This extra div is necessary for the slide effects to work properly.
            let content = $(`<div class="inline-form-container target-${target}">`).hide()
                .data('_pytis_inline_action_invoked_by', element)
                .html(response)
            this._insert_inline_action_content(element, target, content)
            content.find('button.cancel').on('click', e => {
                this._cancel_inline_action(content)
                return false
            })
            setTimeout(() => {
                content.find('form input:visible:enabled:first, form textarea').focus()
                let top = content.offset().top
                let bottom = top + content.height()
                let page_top = $(window).scrollTop()
                let page_bottom = page_top + $(window).height()
                if (bottom > page_bottom) {
                    $('html,body').animate({scrollTop: Math.min(bottom - $(window).height(), top)}, 300)
                } else if (top < page_top) {
                    $('html,body').animate({scrollTop: top}, 300)
                }
            }, 250)
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
            let width = tr.closest('table').width()
            for (let td of tr.children('td')) {
                colspan += $(td).attr('colspan') || 1
                if (target === 'replace') {
                    $(td).hide()
                }
            }
            let td = $(`<td colspan="${colspan}" class="inline-edit">`).append(content)
            td.css('width', width)
            if (target === 'after' || target === 'before') {
                let nr = $('<tr class="data-row">').append(td)
                if (target === 'after') {
                    nr.insertAfter(tr)
                } else {
                    nr.insertBefore(tr)
                }
            } else {
                tr.append(td)
            }
            td.animate({width: content.width()}, {
                done: () => td.css('width', ''),
                duration: 250,
            })
        }
        content.slideDown(250)
    }

    _cancel_inline_action(content) {
        // Here 'content' is the same container as passed to '_insert_inline_action_content()'.
        let form = lcg.widget_instance(content.find('.pytis-form.edit-form'))
        if (form && !form.cancel()) {
            return false
        }
        content.slideUp({
            duration: 250,
            done: () => {
                if (content.hasClass('target-global')) {
                    content.data('_pytis_inline_action_invoked_by').enable()
                    content.remove()
                } else {
                    // This is form specific, so it has a separate method...
                    this._remove_canceled_inline_action_content(content)
                }
            },
        })
        return true
    }

    _remove_canceled_inline_action_content(content) {
        let tr = content.closest('tr')
        content.closest('td').remove()
        tr.children().show()
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

pytis.BrowseForm.on_action = function (event, element, action, url) {
    // This must be a "static" method because the menu items don't
    // exist in the time of form creation so the form can not bind
    // the events to itself.  This method is assigned as popup menu
    // item callback and we may handle item invocation here.
    let form = lcg.widget_instance(element.closest('.pytis-form'))
    if (form && form._inline_editable && form._on_popup_menu_inline_action(element, action, url)) {
        // Don't perform default menu item handler (go to URL)
        return false
    }
}


pytis.ListView = class extends pytis.BrowseForm {

    constructor(form_id, form_name, url, inline_editable) {
        super(form_id, form_name, url, inline_editable)
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
            parent.children().slideUp({duration: 250})
            parent.append(content)
            content.slideDown({delay: 200, duration: 250})
        }
    }

    _remove_canceled_inline_action_content(content) {
        let parent = content.parent()
        content.remove()
        parent.children().slideDown(250)
    }

}


pytis.ItemizedView = class extends pytis.BrowseForm {}


pytis.EditableBrowseForm = class extends pytis.BrowseForm {
    constructor(form_id, form_name, url, inline_editable, allow_insertion) {
        /* form_id, form_name, url, inline_editable ... defined by the
             super class.
           allow_insertion ... Add a button for insertion of new table
             rows.  Note that this is a different feature of this class
             than 'inline_editable' mode defined by the parent class.

        */
        super(form_id, form_name, url, inline_editable)
        if (allow_insertion) {
            this.element.append($('<button class="new-row-button">')
                                .append($('<span>').text(pytis._("New row")))
                                .on('click', this._on_insert_new_row.bind(this)))
        }
    }

    _bind_table_body(tbody) {
        // We don't call super here as we don't want editable cells or
        // expansible rows in EditableBrowseForm, but we may need it in
        // future if the parent class adds something we want...
        tbody.find('tr a.remove-row').on('click', this._on_remove_row)
    }

    _on_insert_new_row(event) {
        this._ajax({
            url: this._url,
            method: 'GET',
            data: {
                _pytis_form_update_request: 1,
                _pytis_insert_new_row: 1,
            },
        }, (response, status, xhr) => {
            let tbody = this.element.find('table.data-table tbody')
            tbody.append(response)
            let rows = this.element.find(`input[name=_pytis_inserted_rows_${this._form_name}]`)
            rows.val(parseInt(rows.val()) + 1)
            this._bind_table_body(tbody)
        })
        return false
    }

    _on_remove_row(event) {
        let tr = $(event.target).closest('tr')
        let name = '_pytis_removed_row_key_' + this._form_name
        let value = tr.attr('data-pytis-row-key')
        tr.closest('form').append(`<input type="hidden" name="${name}" value="${value}">`)
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
        let form = this.element.find('form')
        this._form = form.length ? form : this.element.closest('form')
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
            let states = {}
            for (let [id, f] of this._fields) {
                if (f.state()) {
                    states[f.id()] = f.state()
                }
            }
            let parameters = {
                _pytis_form_update_request: ++this._last_request_number,
                _pytis_form_changed_field: field.id(),
                _pytis_form_state: $.param(states),
            }
            this._ajax({form: this._form, data: parameters}, this._update.bind(this))
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
                for (let [key, cdata] of Object.entries(data.fields)) {
                    let field = this._fields.get(key)
                    if (field) {
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
                    }
                }
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


pytis.Field = class extends lcg.Widget {
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
        if (label.length) {
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
        this._ctrl.prepend(null_option)
        for (let [val, label] of value) {
            // Append options according to the new enumeration received.
            this._ctrl.append(
                $(`<option value="${val}" selected="${val === selected ? 'true' : 'false'}">`).text(label)
            )
        }
    }

}

pytis.ChecklistField = class extends pytis.Field {
    // Multi select control represented by a group of checkboxes.

    _checkboxes() {
        return $.makeArray(this.element.find('input[type="checkbox"]')).map(checkbox => $(checkbox))
    }

    value() {
        return this._checkboxes().filter(x => x.prop('checked')).map(x => x.val())
    }

    set_value(value, localized_value) {
        for (let checkbox of this._checkboxes()) {
            let checked = value && value.find(v => v === checkbox.val()) !== undefined
            checkbox.prop('checked', checked)
        }
    }

    _set_editability(value) {
        this.element.find('input[type="checkbox"]').prop('disabled', !value)
    }

    set_enumeration(value, links) {
        this.element.empty()
        let i = 0
        for (let [val, label] of value) {
            // Append options according to the new enumeration received
            let id = this.element.attr('id') + '-' + i++
            let div = $(`<div>`).append([
                `<input type="checkbox" value="${val}" name="${this._id}" id="${id}">`,
                `&nbsp;`,
                $(`<label for="${id}">`).text(label.replace(' ', '&nbsp;')),
            ])
            if (links && links[val]) {
                div.append('&nbsp;[')
                    .append($(`<a href="${links[val]}">`).text(val))
                    .append(']')
            }
            this.element.append(div)
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

    _attachment_storage_request(request, parameters, callback) {
        parameters._pytis_form_update_request = 1
        parameters._pytis_attachment_storage_field = this._id
        parameters._pytis_attachment_storage_request = request
        this._ajax({form: this.element.closest('form'), data: parameters}, callback)
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
                                                    values: JSON.stringify(values)}, callback)
    }
}

pytis.DateTimeField = class extends pytis.Field {

    constructor(form_id, field_id, id, state, active, required, locale) {
        super(form_id, field_id, id, state, active, required)
        this._button = $('<button type="button" class="selection-invocation calendar-invocation">')
            .text('...')
            .prop('disabled', this.element.prop('disabled'))
            .on('click', this._on_button.bind(this))
            .insertAfter(this.element)
        this._locale = locale
        this._datepicker = null
    }

    _set_editability(value) {
        super._set_editability(value)
        this._button.prop('disabled', !value)
    }

    _on_button(event) {
        if (this._datepicker === null) {
            this._datepicker = new pytis.DatePicker(this._on_date_selected.bind(this),
                                                    this._on_datepicker_closed.bind(this),
                                                    this._locale)
        }
        if (this._datepicker.element.css('display') === 'block') {
            this._datepicker.close(false)
        } else {
            let position
            if (event.x) {
                position = {x: event.x, y: event.y}
            } else {
                let offset = this._button.offset()
                position = {x: offset.left, y: offset.top + this._button.height()}
            }
            this._datepicker.open(this._parse_date(this.value(), this._locale.format), position)
        }
    }

    _on_date_selected(date) {
        this.set_value(this._format_date(date, this._locale.format))
    }

    _on_datepicker_closed() {
        this._set_focus(this._ctrl)
    }

    _parse_date(string, format) {
        let today = new Date()
        let y = 0
        let m = -1
        let d = 0
        let a = string.split(/\W+/)
        let b = format.match(/%./g)
        let i = 0, j = 0
        let hr = 0
        let min = 0
        for (i = 0; i < a.length; ++i) {
            if (!a[i]) {
                continue
            }
            switch (b[i]) {
            case "%d":
            case "%e":
                d = parseInt(a[i], 10)
                break
            case "%m":
                m = parseInt(a[i], 10) - 1
                break
            case "%Y":
            case "%y":
                y = parseInt(a[i], 10)
                if (y < 100) {
                    y += (y > 29) ? 1900 : 2000
                }
                break
            case "%b":
            case "%B":
                for (let j = 0; j < 12; ++j) {
                    if (this._locale.month_names[j].substr(0, a[i].length).toLowerCase() == a[i].toLowerCase()) {
                        m = j
                        break
                    }
                }
                break
            case "%H":
            case "%I":
            case "%k":
            case "%l":
                hr = parseInt(a[i], 10)
                break
            case "%P":
            case "%p":
                if (/pm/i.test(a[i]) && hr < 12)
                    hr += 12
                else if (/am/i.test(a[i]) && hr >= 12)
                    hr -= 12
                break
            case "%M":
                min = parseInt(a[i], 10)
                break
            }
        }

        if (isNaN(y)) y = today.getFullYear()
        if (isNaN(m)) m = today.getMonth()
        if (isNaN(d)) d = today.getDate()
        if (isNaN(hr)) hr = today.getHours()
        if (isNaN(min)) min = today.getMinutes()
        if (y != 0 && m != -1 && d != 0) {
            return new Date(y, m, d, hr, min, 0)
        }
        y = 0
        m = -1
        d = 0
        for (i = 0; i < a.length; ++i) {
            if (a[i].search(/[a-zA-Z]+/) != -1) {
                let t = -1
                for (j = 0; j < 12; ++j) {
                    if (this._locale.month_names[j].substr(0, a[i].length).toLowerCase() == a[i].toLowerCase()) { t = j; break }
                }
                if (t != -1) {
                    if (m != -1) {
                        d = m+1
                    }
                    m = t
                }
            } else if (parseInt(a[i], 10) <= 12 && m == -1) {
                m = a[i]-1
            } else if (parseInt(a[i], 10) > 31 && y == 0) {
                y = parseInt(a[i], 10)
                (y < 100) && (y += (y > 29) ? 1900 : 2000)
            } else if (d == 0) {
                d = a[i]
            }
        }
        if (y == 0) {
            y = today.getFullYear()
        }
        if (m != -1 && d != 0) {
            return new Date(y, m, d, hr, min, 0)
        }
        return today
    }

    _format_date(date, format) {
        // We implement just a subset of strftime formatting characters
        // possibly present in LCG date_format locale settings.
        function zpad(n) {
            return (n < 10) ? ("0" + n) : n
        }
        let m = date.getMonth()
        let d = date.getDate()
        let y = date.getFullYear()
        let weekday = date.getDay()
        let hours = date.getHours()
        let hours12 = hours == 0 ? 12 : (hours > 12 ? (hours - 12) : hours)
        let pm = (hours >= 12)
        let s = {}
        s["%a"] = this._locale.short_day_names[weekday] // abbreviated weekday name
        s["%A"] = this._locale.day_names[weekday] // full weekday name
        s["%b"] = this._locale.short_month_names[m] // abbreviated month name
        s["%B"] = this._locale.month_names[m] // full month name
        s["%C"] = 1 + Math.floor(y / 100) // century number
        s["%d"] = zpad(d) // day of month (range 01 to 31)
        s["%e"] = d // day of month (range 1 to 31)
        s["%H"] = zpad(hours) // hour, range 00 to 23 (24h format)
        s["%I"] = zpad(hours12) // hour, range 01 to 12 (12h format)
        s["%k"] = hours // hour, range 0 to 23 (24h format)
        s["%l"] = hours12 // hour, range 1 to 12 (12h format)
        s["%m"] = zpad(m + 1) // month, range 01 to 12
        s["%M"] = zpad(date.getMinutes()) // minute, range 00 to 59
        s["%n"] = "\n" // a newline character
        s["%p"] = pm ? "PM" : "AM"
        s["%P"] = pm ? "pm" : "am"
        s["%s"] = Math.floor(date.getTime() / 1000)
        s["%S"] = zpad(date.getSeconds()) // seconds, range 00 to 59
        s["%t"] = "\t" // a tab character
        s["%u"] = weekday + 1  // day of week (range 1 to 7, 1 = MON)
        s["%w"] = weekday // day of week (range 0 to 6, 0 = SUN)
        s["%y"] = ('' + y).substr(2, 2) // year without century (range 00 to 99)
        s["%Y"] = y // year with century
        s["%%"] = "%" // a literal '%' character
        return format.replace(/%./g, match => s[match] || match)
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
        this._ctrl.on('change', this._on_change.bind(this))
    }

    _on_change(event) {
        let files = this._ctrl[0].files
        if (files && files.length) {
            this._ajax({
                form: this.element.closest('form'),
                data: {
                    _pytis_form_update_request: true,
                    _pytis_form_changed_field: this._id,
                    ['_pytis_file_size_' + this._id]: files[0].size,
                    [this._id]: null, // Avoid sending the whole file through ajax.
                },
            }, this._update)
        }
    }

    _update(response, status, xhr) {
        let data = xhr.responseJSON
        if (data) {
            let error = data.fields[this._id].error
            let div = this._ctrl.next('.error')
            let submit = this.element.find('button[type="submit"]')
            if (error) {
                if (!div.length) {
                    div = this._ctrl.after(`<div class="error">`)
                }
                div.text(error)
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


pytis.DatePicker = class extends lcg.Widget {
    /* Date picker popup widget
     *
     * The behavior was inspired by the W3C date picker from
     * https://www.w3.org/TR/wai-aria-practices/examples/dialog-modal/datepicker-dialog.html
     *
     */
    constructor(on_date_selected, on_close, locale) {
        let dialog = $(`
          <div class="datepicker" role="dialog" aria-modal="true">
            <div class="header">
              <button type="button" class="prev-year"></button>
              <button type="button" class="prev-month"></button>
              <h2 class="month-year" aria-live="polite"></h2>
              <button type="button" class="next-month"></button>
              <button type="button" class="next-year"></button>
            </div>
            <table class="dates" role="grid" aria-labelledby="id-grid-label">
              <thead></thead>
              <tbody></tbody>
            </table>
            <div class="dialog-ok-cancel-group">
              <button class="dialog-button" value="cancel"></button>
              <button class="dialog-button" value="ok"></button>
            </div>
            <div class="echo" aria-live="polite"></div>
          </div>`)
            .css({display: 'none'})
            .attr('aria-label', pytis._("Choose Date"))
        $('body').append(dialog)
        super(dialog)
        this._on_date_selected = on_date_selected
        this._on_close = on_close
        this._locale = locale
        this._focus_day = new Date()
        this._selected_day = new Date(0, 0, 1)
        this._cells = []
        this._last_message = ''
        // Create the grid of dates.
        let tr = dialog.find('table.dates thead').append($('<tr>'))
        for (let i = 0; i < 7; i++) {
            let day = (i + locale.first_week_day) % 7
            tr.append($('<th tabindex="-1">')
                      .attr('title', locale.day_names[day])
                      .text(locale.short_day_names[day]))
        }
        for (let i = 0; i < 6; i++) {
            let tr = $('<tr>')
            dialog.find('table.dates tbody').append(tr)
            for (let j = 0; j < 7; j++) {
                let td = $('<td tabindex="-1">')
                    .on('click', this._on_day_click.bind(this))
                    .on('keydown', this._on_key_down.bind(this))
                    .on('focus', e => this._set_message(pytis._("Cursor keys can navigate dates")))
                    .on('blur', e => this._set_message(''))
                tr.append(td)
                this._cells.push(td)
            }
        }

        this._ok_button = dialog.find('button[value="ok"]')
            .text(pytis._("Ok"))
            .on('click', this._on_ok_button_click.bind(this))
            .on('keydown', this._on_ok_button_keydown.bind(this))
        this._cancel_button = dialog.find('button[value="cancel"]')
            .text(pytis._("Cancel"))
            .on('click', this._on_cancel_button_click.bind(this))
            .on('keydown', this._on_cancel_button_keydown.bind(this))
        dialog.find('.prev-month')
            .text('<')
            .attr('title', pytis._("previous month"))
            .attr('aria-label', pytis._("previous month"))
            .on('click', this._on_prev_month_button_click.bind(this))
            .on('keydown', this._on_prev_month_button_keydown.bind(this))
        dialog.find('.next-month')
            .text('>')
            .attr('title', pytis._("next month"))
            .attr('aria-label', pytis._("next month"))
            .on('click', this._on_next_month_button_click.bind(this))
            .on('keydown', this._on_next_month_button_keydown.bind(this))
        this._prev_year_button = dialog.find('.prev-year')
            .text('<<')
            .attr('title', pytis._("previous year"))
            .attr('aria-label', pytis._("previous year"))
            .on('click', this._on_prev_year_button_click.bind(this))
            .on('keydown', this._on_prev_year_button_keydown.bind(this))
        this._next_year_button = dialog.find('.next-year')
            .text('>>')
            .attr('title', pytis._("next year"))
            .attr('aria-label', pytis._("next year"))
            .on('click', this._on_next_year_button_click.bind(this))
            .on('keydown', this._on_next_year_button_keydown.bind(this))
    }

    _update_grid() {
        let month = this._focus_day.getMonth()
        let year = this._focus_day.getFullYear()
        let date = new Date(Date.UTC(year, month, 1, 0, 0))
        this.element.find('.month-year').text(this._locale.month_names[month] + ' ' + year)
        let first_week_day = (this._locale.first_week_day + 8) % 7 // starting from Sunday as in getDay().
        date.setDate(date.getDate() - (date.getDay() + 7 - first_week_day) % 7)
        for (let td of this._cells) {
            td.attr('tabindex', '-1')
            td.removeAttr('aria-selected')
            td.attr('data-date', date.toISOString().substring(0, 10))
            if (date.getMonth() != this._focus_day.getMonth()) {
                td.addClass('disabled')
                td.text('')
            } else {
                td.removeClass('disabled')
                td.text(date.getDate())
                if (this._is_same_day(date, this._selected_day)) {
                    td.attr('aria-selected', 'true')
                    td.attr('tabindex', '0')
                }
            }
            date.setDate(date.getDate() + 1)
        }
        let td = this._cells[35]
        td.parent().css({visibility: td.hasClass('disabled') ? 'hidden' : 'visible'})
    }

    _set_message(message) {
        if (message !== this._last_message) {
            setTimeout(() => this.element.find('.echo').text(message), 200)
            this._last_message = message
        }
    }

    _is_same_day(day1, day2) {
        return (
            day1.getFullYear() == day2.getFullYear() &&
                day1.getMonth() == day2.getMonth() &&
                day1.getDate() == day2.getDate()
        )
    }

    _get_date_from_cell(td) {
        let parts = td.attr('data-date').split('-')
        return new Date(parts[0], parseInt(parts[1]) - 1, parts[2])

    }

    _commit(cell) {
        let date = cell ? this._get_date_from_cell(cell) : this._focus_day
        if (date) {
            this._on_date_selected(date)
        }
        this._close()
    }

    _move_focus_to_day(day) {
        let d = this._focus_day
        this._focus_day = day
        if (d.getMonth() != this._focus_day.getMonth() || d.getYear() != this._focus_day.getYear()) {
            this._update_grid()
        }
        this._set_focus_day(true)
    }

    _set_focus_day(focus) {
        for (let td of this._cells) {
            td.attr('tabindex', -1)
            if (this._is_same_day(this._get_date_from_cell(td), this._focus_day)) {
                td.attr('tabindex', 0)
                if (focus) {
                    td.focus()
                }
            }
        }
    }

    _move_to_next_year(focus) {
        this._focus_day.setFullYear(this._focus_day.getFullYear() + 1)
        this._update_grid()
        this._set_focus_day(focus)
    }

    _move_to_prev_year(focus) {
        this._focus_day.setFullYear(this._focus_day.getFullYear() - 1)
        this._update_grid()
        this._set_focus_day(focus)
    }

    _move_to_next_month(focus) {
        this._focus_day.setMonth(this._focus_day.getMonth() + 1)
        this._update_grid()
        this._set_focus_day(focus)
    }

    _move_to_prev_month(focus) {
        this._focus_day.setMonth(this._focus_day.getMonth() - 1)
        this._update_grid()
        this._set_focus_day(focus)
    }

    _move_focus_to_next_day() {
        let d = new Date(this._focus_day)
        d.setDate(d.getDate() + 1)
        this._move_focus_to_day(d)
    }

    _move_focus_to_next_week() {
        let d = new Date(this._focus_day)
        d.setDate(d.getDate() + 7)
        this._move_focus_to_day(d)
    }

    _move_focus_to_prev_day() {
        let d = new Date(this._focus_day)
        d.setDate(d.getDate() - 1)
        this._move_focus_to_day(d)
    }

    _move_focus_to_prev_week() {
        let d = new Date(this._focus_day)
        d.setDate(d.getDate() - 7)
        this._move_focus_to_day(d)
    }

    _move_focus_to_first_day_of_week() {
        let d = new Date(this._focus_day)
        d.setDate(d.getDate() - d.getDay())
        this._move_focus_to_day(d)
    }

    _move_focus_to_last_day_of_week() {
        let d = new Date(this._focus_day)
        d.setDate(d.getDate() + (6 - d.getDay()))
        this._move_focus_to_day(d)
    }

    // Calendar table cell keymap

    _define_keymap() {
        return {
            'Escape': (e, td) => this._close(),
            'Space': (e, td) => this._commit(td),
            'Enter': (e, td) => this._commit(td),
            'Tab': (e, td) => this._cancel_button.focus(),
            'Shift-Tab': (e, td) => this._next_year_button.focus(),
            'Right': (e, td) => this._move_focus_to_next_day(),
            'Left': (e, td) => this._move_focus_to_prev_day(),
            'Down': (e, td) => this._move_focus_to_next_week(),
            'Up': (e, td) => this._move_focus_to_prev_week(),
            'PageUp': (e, td) => this._move_to_prev_month(true),
            'Shift-PageUp': (e, td) => this._move_to_prev_year(true),
            'PageDown': (e, td) => this._move_to_next_month(true),
            'Shift-PageDown': (e, td) => this._move_to_next_year(true),
            'Home': (e, td) => this._move_focus_to_first_day_of_week(),
            'End': (e, td) => this._move_focus_to_last_day_of_week(),
        }
    }

    // Event handlers

    _on_ok_button_click(event) {
        this._commit()
        return false
    }

    _on_ok_button_keydown(event) {
        if (event.key === 'Escape') {
            this._close()
            return false
        } else if (event.key === 'Tab' && !event.shiftKey) {
            this._prev_year_button.focus()
            return false
        }
    }

    _on_cancel_button_click(event) {
        this._close()
        return false
    }

    _on_cancel_button_keydown(event) {
        if (event.key === 'Escape') {
            this._close()
            return false
        }
    }

    _on_next_year_button_click(event) {
        this._move_to_next_year(false)
        return false
    }

    _on_next_year_button_keydown(event) {
        if (event.key === 'Escape') {
            this._close()
            return false
        } else if (event.key === 'Enter') {
            return this._on_next_year_button_click()
        }
    }

    _on_prev_year_button_click(event) {
        this._move_to_prev_year(false)
        return false
    }

    _on_prev_year_button_keydown(event) {
        if (event.key === 'Escape') {
            this._close()
            return false
        } else if (event.key === 'Enter') {
            return this._on_prev_year_button_click()
        } else if (event.key === 'Tab' && event.shiftKey) {
            this._ok_button.focus()
            return false
        }
    }

    _on_next_month_button_click(event) {
        this._move_to_next_month(false)
        return false
    }

    _on_next_month_button_keydown(event) {
        if (event.key === 'Escape') {
            this._close()
            return false
        } else if (event.key === 'Enter') {
            return this._on_next_month_button_click()
        }
    }

    _on_prev_month_button_click(event) {
        this._move_to_prev_month(false)
        return false
    }

    _on_prev_month_button_keydown(event) {
        if (event.key === 'Escape') {
            this._close()
            return false
        } else if (event.key === 'Enter') {
            return this._on_prev_month_button_click()
        }
    }

    _on_day_click(event) {
        if (!event.currentTarget.classList.contains('disabled')) {
            this._commit($(event.currentTarget))
        }
        return false
    }

    _close() {
        $(document.body).off('mouseup.datepicker')
        this._set_message('')
        this.element.css({display: 'none'})
        this._on_close()
    }

    // Public methods.

    open(date, position) {
        if (typeof date !== 'undefined') {
            this._focus_day = date
            this._selected_day = new Date(date)
        } else {
            this._focus_day = new Date()
            this._selected_day = new Date(0, 0, 1)
        }
        if (typeof position !== 'undefined') {
            this.element.css({left: position.x + 'px', top: position.y + 'px'})
        }
        this._update_grid()
        this.element.css({display: 'block', zIndex: 2})
        this._set_focus_day(true)
        $(document.body).on('mouseup.datepicker', (e) => {
            if (!$.contains(this.element[0], e.target)) {
                this._close()
                return false
            }
        })
    }
}
