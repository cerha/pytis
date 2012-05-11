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

pytis.FormHandler = Class.create({
    initialize: function(form_id, fields, state) {
	/* form_id ... HTML id of the form to connect to (string)
	 * fields ... array of form fields as pytis.Field instances
	 * state ... initial state of form's runtime filters and arguments as
	 *    an associative array (hash) keyed by field id where value is a
	 *    string representation of the filter and arguments for server side
	 *    comparisons.
	 */
	var form = $(form_id);
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
	    var value = field.value();
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
	/* form_id ... HTML id of the form element to which the field belongs.
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
	this._form = $(form_id);
	this._element = $(field_id);
	this._ctrl = this._form[id];
	this._id = id;
	this._active = active;
	if (required && this._ctrl && this._ctrl.length == undefined) {
	    // TODO: handle aria-required also for compound fields (radio group, checklist).
	    this._ctrl.setAttribute('aria-required', 'true');
	}
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
	    CKEDITOR.on('dialogDefinition', function(event) {
		if (event.data.name == 'link') {
		    event.data.definition.removeContents('advanced');
		    event.data.definition.removeContents('target');
		}
		if (event.data.name == 'image') {
		    event.data.definition.removeContents('advanced');
		    event.data.definition.removeContents('Link');
		}
	    });
	}
    }
});

pytis.HtmlField.select_file = function (uri) {
    // Static method to be called when a file is selected in HtmlField's file
    // browser.  The browser window will be closed and the file uri will be
    // inserted into the HtmlField's file/image dialog.  The file browser is
    // invoked from this dialog using the URI provider with
    // `pytis.web.UriType.*_BROWSER' type.
    var re = new RegExp('(?:[\?&]|&amp;)CKEditorFuncNum=([^&]+)', 'i');
    var match = window.location.search.match(re);
    var func_num = (match && match.length > 1) ? match[1] : '';
    window.opener.CKEDITOR.tools.callFunction(func_num, uri, '');
    window.parent.close();
};

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
