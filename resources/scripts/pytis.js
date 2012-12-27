/* -*- coding: utf-8 -*-
 *
 * Copyright (C) 2009, 2010, 2011, 2012 Brailcom, o.p.s.
 * Author: Tomas Cerha
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
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

var pytis = {};

pytis.gettext = new Gettext({domain:'pytis'});
pytis._ = function (msg) { return pytis.gettext.gettext(msg); };

pytis.BrowseFormHandler = Class.create({
    /* Handles asynchronous load of a Pytis browse form.

    The form content is loaded through an AJAX request after the page is
    displayed.  Special handling is applied to forms which are placed inside
    notebook tabs (of lcg.Notebook widget).  Such forms are not loaded
    immediately, but in a callback after the tab is activated.
      
     */
    initialize: function(form_id, form_name, uri) {
	/* form_id ... HTML id of the pytis form top level element (string)
 	   form_name ... Form name used for distinguishing request parameters
   	     (see form_name in the python class).
	   uri ... URI of the AJAX request to retrieve form data
	 */
	this.form = $(form_id);
	this.form_name = form_name;
	this.uri = uri;
	this.ajax_container = this.form.down('.ajax-container');
	if (this.ajax_container && uri) {
	    var parameters = {};
	    var query = window.location.search.replace(/;/g, '&').parseQuery();
	    if (query['form_name'] == form_name)
		var parameters = query;
	    else
		var parameters = {};
	    var page = this.form.up('.notebook-widget > div');
	    if (page)
		lcg.Notebook.on_activation(page, function() {
		    this.load_form_data(parameters);
		}.bind(this));
	    else
		this.load_form_data(parameters);
	} else {
	    this.bind_search_button(this.form.down('.list-form-controls', 0));
	    this.bind_search_button(this.form.down('.list-form-controls', 1));
	}
    },

    load_form_data: function(parameters) {
	parameters['_pytis_async_load_request'] = 1;
	new Ajax.Request(this.uri, {
	    method: 'get',
	    parameters: parameters,
	    onSuccess: function(transport) {
		try {
		    this.ajax_container.update(transport.responseText);
		    this.bind_controls(this.ajax_container.down('.list-form-controls', 0));
		    this.bind_controls(this.ajax_container.down('.list-form-controls', 1));
		    document.body.style.cursor = "default";
		    if (this.form.down('#found-record')) window.location.hash = '#found-record';
		}
		catch (e) {
		    // Errors in asynchronous handlers are otherwise silently
		    // ignored.  This will only work in Firefox with Firebug,
		    // but it is only useful for debugging anyway...
		    console.log(e);
		}
	    }.bind(this),
	    onFailure: function(transport) {
		this.ajax_container.update(pytis._("Failed loading form."));
		document.body.style.cursor = "default";
	    }
	});
    },

    bind_controls: function(panel) {
	if (panel) {
	    panel.select('.prev-page-button, .next-page-button').each(function(ctrl) {
		ctrl.observe('click', function(event) {
		    this.reload_form_data(ctrl);
		    event.stop();
		}.bind(this));
	    }.bind(this));
	    panel.select('select').each(function(ctrl) {
		ctrl.onchange = null; // Deactivate the original handler.
		ctrl.observe('change', function(event) {
		    this.reload_form_data(ctrl);
		    event.stop();
		}.bind(this));
	    }.bind(this));
	    panel.select('.index-search-controls a').each(function(ctrl) {
		var params = ctrl.href.replace(/;/g, '&').parseQuery();
		ctrl.observe('click', function(event) {
		    this.reload_form_data(ctrl, {index_search: params['index_search'],
						 sort: params['sort'],
						 dir: params['dir']});
		    event.stop();
		}.bind(this));
	    }.bind(this));
	    this.bind_search_button(panel);
	}
    },

    reload_form_data: function(ctrl, params) {
	var form = ctrl.up('form');
	var parameters = (typeof(params) != 'undefined' ? params : {});
	form.getElements().each(function(x) {
	    if (x.tagName != 'BUTTON')
		parameters[x.name] = x.value;
	});
	parameters[ctrl.name] = ctrl.value;
	document.body.style.cursor = "wait";
	form.disable();
	this.load_form_data(parameters);
    },

    bind_search_button: function(panel) {
	if (panel) {
	    var button = panel.down('.paging-controls button.search-button');
	    if (button)
		button.observe('click', this.on_show_search_controls.bind(this));
	}
    },

    on_show_search_controls: function(event) {
	var search_controls = $(this.form).down('div.query');
	search_controls.show();
	search_controls.down('input.query-field').focus();
	search_controls.down('input[type=hidden]').value = '1';
	for (var i=0; i<2; i++) {
	    var panel = this.form.down('.list-form-controls', i);
	    if (panel) {
		var button = panel.down('.paging-controls button.search-button');
		if (button)
		    button.hide();
	    }
	}
	event.stop();
    }

});

pytis.FormHandler = Class.create({
    initialize: function(form_id, fields, state) {
	/* form_id ... HTML id of the pytis form top level element (string)
	 * fields ... array of form fields as pytis.Field instances
	 * state ... initial state of form's runtime filters and arguments as
	 *    an associative array (hash) keyed by field id where value is a
	 *    string representation of the filter and arguments for server side
	 *    comparisons.
	 */
	var form = $(form_id).down('form');
	this._form = form;
	this._fields = {};
	this._state = state;
	var observe = false;
	for (var i=0; i<fields.length; i++) {
	    var field = fields[i];
	    this._fields[field.id()] = field;
	    if (field.active())
		observe = true;
	}
	this._last_request_number = 0;
	if (observe)
	    this._observer = new Form.Observer(form, 1, this.on_change.bind(this));
    },
    
    on_change: function(form, value) {
	// Send AJAX request in reaction to user changes of form values.
	// TODO: Avoid AJAX request flooding during typing or other continuous
	// changes.  The problem is that we must always send a request as we
	// don't know whether another change comes soon.  Maybe we could send
	// the request after some delay if no other change comes in the
	// meantime, but this would slow down the UI responsivity.
	var values = value.parseQuery(); 
	var last_values = this._observer.lastValue.parseQuery();
	for (var id in this._fields) {
	    var field = this._fields[id];
	    // Disabled fields are not present in values/last_values, but also
	    // checkbox fields are not there if unchecked.
	    if (field.active()
		&& (id in values || id in last_values) && values[id] != last_values[id]) {
		document.body.style.cursor = "wait";
		var state = (this._state ? $H(this._state).toQueryString() : null);
		this._form.request({
		    parameters: {_pytis_form_update_request: ++this._last_request_number,
				 _pytis_form_changed_field: id,
 				 _pytis_form_state: state},
		    onSuccess: this.update.bind(this)
		});
		break
	    }
	}
    },
    
    update: function(response) {
	// Update the form state in reaction to previously sent AJAX request.
	var data = response.responseJSON;
	if (data != null) {
	    var response_number = data['request_number'];
	    var field_data = data['fields'];
	    // Ignore the response if other requests were sent in the meantime.
	    // Only the most recently sent request really corresponds to the
	    // current form state!  This also prevents processing responses
	    // coming in wrong order (earlier request may be processed longer
	    // than a later one).
	    if (response_number == this._last_request_number && field_data != null) {
		for (var id in field_data) {
		    var field = this._fields[id];
 		    if (field) {
			var cdata = field_data[id];
			var original_value = field.value();
			for (var key in cdata) {
			    var value = cdata[key];
			    if (key == 'enumeration')   field.set_enumeration(value, cdata['links']);
			    else if (key == 'value')    field.set_value(value);
		            else if (key == 'editable') field.set_editability(value);
			    else if (key == 'state')    this._state[id] = value;
			}
			if (cdata.enumeration && !cdata.value) {
			    // Retain the value of enumeration fields even
			    // after the enumeration changes (if possible).
			    field.set_value(original_value);
			}
		    }
		}
		document.body.style.cursor = "default";
	    }
	}
    }
    
});

pytis.Field = Class.create({
    initialize: function(form_id, field_id, id, active, required) {

	/* form_id ... HTML id of the pytis form element to which the field
	 *    belongs.
	 * field_id ... HTML id of the form field element.  The element is
         *    usually the form control directly, but may be also a top level
         *    element (div) for compound fields, such as radio group or
	 *    checklist.
	 * id ... pytis field id (string) typically used as form control
	 *    'name'.
	 * active ... boolean flag; True if this field may trigger changes of
	 *    other fields.  The changes in this field will be observed and
	 *    sent for server processing.
	 * required ... boolean flag; True if this field is required (not null).
	 */
	this._form = $(form_id).down('form');
	this._element = $(field_id);
	this._ctrl = this._form[id];
	this._id = id;
	this._active = active;
	if (required && this._ctrl && this._ctrl.length == undefined) {
	    // TODO: handle aria-required also for compound fields (radio group, checklist).
	    this._ctrl.setAttribute('aria-required', 'true');
	}
	// To allow accessing the field handler instance in pytis-ckeditor.js.
	this._element._pytis_field_instance = this;
    },

    id: function() {
	return this._id;
    },

    active: function() {
	return this._active;
    },

    value: function() {
	return this._ctrl.value;
    },

    set_editability: function(value) {
	// Disable/enable field editability.
	var labels = $$('.field-label.id-'+this._id);
	if (labels) {
	    var label = labels[0]
	    if (value && label.hasClassName('disabled'))
		label.removeClassName('disabled');
	    if (!value && !label.hasClassName('disabled'))
		label.addClassName('disabled');
	}
	this._set_editability(value);
    },

    _set_editability: function(value) {
	this._ctrl.disabled = !value;
    },
    
    set_value: function(value) {
	// Set the field value.
	this._ctrl.value = value;
    },
    
    set_enumeration: function(value, links) {
	// Update enumeration controls (only for enumeration fields).
    }

});

pytis.CheckboxField = Class.create(pytis.Field, {
    // Specific handler for a checkbox field.

    set_value: function(value) {
	// Set the field value.
	this._ctrl.checked = value == 'T';
    }

});

pytis.RadioField = Class.create(pytis.Field, {
    // Specific handler for a radio button group.

    set_value: function(value) {
	for (var i=0; i<this._ctrl.length; i++) {
	    var radio = this._ctrl[i];
	    radio.checked = radio.value == value;
	}
    },

    _set_editability: function(value) {
	for (var i=0; i<this._ctrl.length; i++) {
	    this._ctrl[i].disabled = !value;
	}
    }

});

pytis.PasswordField = Class.create(pytis.Field, {
    // Specific handler for a radio button group.

    set_value: function(value) {
	for (var i=0; i<this._ctrl.length; i++) {
	    this._ctrl[i].value = value;
	}
    },

    _set_editability: function($super, value) {
	if (typeof(this._ctrl.length) == 'undefined') {
	    $super(value);
	} else {
	    for (var i=0; i<this._ctrl.length; i++) {
		this._ctrl[i].disabled = !value;
	    }
	}
    }

});

pytis.ChoiceField = Class.create(pytis.Field, {
    // Specific handler for codebook field represented by HTML select control.

    set_enumeration: function(value, links) {
	var options = this._ctrl.options;
	var selected = $F(this._ctrl);
	for (var i=options.length-1; i>=0; --i) {
	    //Remove all options except for the (first) NULL option (if present).
	    var option = $(options[i]);
	    if (option.value != '') option.remove();
	}
	this._ctrl.cleanWhitespace();
	for (var i=0, len=value.length; i<len; ++i) {
	    //Append options according to the new enumeration received;
	    var item = value[i];
	    var attr = {value: item[0], selected: item[0] == selected};
	    var text = item[1].escapeHTML().gsub(' ', '&nbsp;');
	    var option = new Element('option', attr).update(text);
	    this._ctrl.insert(option);
	}
    }

});

pytis.ChecklistField = Class.create(pytis.Field, {
    // Specific handler for a multi select control represented by a group of checkboxes.

    _checkboxes: function() {
	return this._element.immediateDescendants().collect(function(item) {return item.firstDescendant()});
    },
    
    value: function() {
	var checkboxes = this._checkboxes();
	return checkboxes.map(function(checkbox) {return checkbox.value});
    },

    set_value: function(value) {
	this._checkboxes().each(function(checkbox) {
	    checkbox.checked = false;
	    if (value) {
		for (var j=0; j<value.length; j++) {
		    if (value[j] == checkbox.value)
			checkbox.checked = true;
		}
	    }
	});
    },

    _set_editability: function(value) {
	this._checkboxes().each(function(checkbox) {
	    checkbox.disabled = !value;
	});
    },

    set_enumeration: function(value, links) {
	var elem = this._element;
	var descendants = elem.immediateDescendants();
	for (var i=0; i<(descendants.length); i++) {
	    $(descendants[i]).remove();
	}
	elem.cleanWhitespace();
	for (var i=0, len=value.length; i<len; ++i) {
	    //Append options according to the new enumeration received;
	    var item = value[i];
	    var id = elem.getAttribute('id')+'-'+i
	    var text = (' '+ item[1]).escapeHTML().gsub(' ', '&nbsp;');
	    var div = new Element('div');
	    div.insert(new Element('input', {'type': 'checkbox',
					     'value': item[0],
					     'name': this._id,
					     'id': id}));
	    div.insert(new Element('label', {'for': id}).update(text));
	    var link = (links ? links[item[0]] : null);
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
    initialize: function($super, form_id, field_id, id, active, required) {
	$super(form_id, field_id, id, active, required);
        if (typeof(CKEDITOR) != 'undefined') {
	    // The function pytis.HtmlField.plugin is defined in pytis-ckeditor.js.
	    CKEDITOR.plugins.add('pytis-attachments', {init: pytis.HtmlField.plugin});
	    CKEDITOR.on('dialogDefinition', pytis.HtmlField.on_dialog);
	}
    },
    
    _attachment_storage_request: function(request, parameters) {
	parameters['_pytis_form_update_request'] = 1;
	parameters['_pytis_attachment_storage_field'] = this._id;
	parameters['_pytis_attachment_storage_request'] = request;
	var req = this._form.request({
	    parameters: parameters,
	    asynchronous: false
	});
	return req.transport.responseText.evalJSON();
    },

    get_attachment: function(filename) {
	return this._attachment_storage_request('get', {filename: filename});
    },

    list_attachments: function() {
	return this._attachment_storage_request('list', {});
    },
    
    // Insert attachment doesn't go through AJAX due to browser limitations.
    // Iframe upload is performed as implemented in CKeditor and
    // customized in ckeditor-plugin.js so a method is not needed here.
    
    update_attachment: function(filename, values) {
	return this._attachment_storage_request('update', {filename: filename,
							   values: Object.toJSON(values)});
    }
});

pytis.DateTimeField = Class.create(pytis.Field, {
    _set_editability: function(value) {
	this._ctrl.disabled = !value;
	var button = $(this._ctrl.id+'-button')
	button.disabled = !value;
    }
});

pytis.FileUploadField = Class.create(pytis.Field, {

    initialize: function($super, form_id, field_id, id, active, required) {
	$super(form_id, field_id, id, active, required);
	// Observe file field changes separately as they are 1) ignored by
	// form.observe (by Prototype.js, not sure why) and 2) only "active"
	// fields are currently observed by pytis form updated (field is active
	// when computers depend on it).  When we ever decide to send ajax
	// updates for all fields (to support continuous validation) we can
	// solve file size validation within the main form updates.
	this._observer = new Form.Element.Observer(this._ctrl, 1, this.on_change.bind(this));
    },

    on_change: function(form, value) {
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
    
    update: function(response) {
	// Update the form state in reaction to previously sent AJAX request.
	var data = response.responseJSON;
	if (data != null) {
	    var error = data['fields'][this._id]['error'];
	    var div = this._ctrl.next('.error');
	    if (error) {
		if (div)
		    div.update(error);
		else
		    this._ctrl.insert({after: new Element('div', {'class': 'error'}).update(error)});
	    } 
	    else if (div)
		div.remove();
	    var submit = this._form.down('button[type="submit"]');
	    if (submit)
		submit.disabled = (error != null);
	}
	document.body.style.cursor = "default";
    }


});
