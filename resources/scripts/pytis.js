/* -*- coding: utf-8 -*-
 *
 * Copyright (C) 2009, 2010 Brailcom, o.p.s.
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
 *   - updating enumerations based on pytis runtime codebook filters.
 */

var pytis = {};

pytis.FormHandler = Class.create({
      initialize: function(form_id, active_fields, filters) {
	 /* form_id ... HTML id of the form to connect to (string)
	  * active_fields ... array of field identifiers (strings) of fields
	  *    which may trigger changes of other fields.  The changes in the
	  *    active fields will be observed and sent for server processing.
	  * filters ... initial state of form's runtime filters as an
	  *    associative array (hash) keyed by field id where value is a
	  *    string representation of the filter for server side comparisons.
	  */
	 var form = $(form_id);
	 if (form != null) {
	    this._form = form;
	    this._active_fields = active_fields;
	    this._filters = filters;
	    this._last_request_number = 0;
	    this._observer = new Form.Observer(form, 1, this.on_change.bind(this));
	 }
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
	 for (var i=0; i<this._active_fields.length; i++) {
	    var field = this._active_fields[i];
	    // Disabled fields are not present in values/last_values, but also
	    // checkbox fields are not there if unchecked.
	    if ((field in values || field in last_values) && values[field] != last_values[field]) {
	       this._form.request({
		     parameters: {_pytis_form_update_request: ++this._last_request_number,
			          _pytis_form_changed_field: field,
 			          _pytis_form_filter_state: $H(this._filters).toJSON()},
		     onSuccess: this.update.bind(this)
	       });
	       break;
	    }
	 }
      },
      
      _set_editability: function(field, value) {
	 // Disable/enable field depending on type.
	 if (field.type == undefined && field.length)
	    // We get an array of input elements for a radio button group.
	    for (var i=0; i<field.length; i++)
	       field[i].disabled = !value;
	 else
	    field.disabled = !value;
      },
      
      _set_value: function(field, value) {
	 // Set the field value depending on type.
	 if (field.type == undefined && field.length) {
	    // We get an array of input elements for a radio button group.
	    for (var i=0; i<field.length; i++) {
	       var f = field[i];
	       f.checked = f.value == value;
	    }
	 } else if (field.type == 'checkbox')
	    field.checked = value == 'T';
	 else
	    field.value = value;
      },
      
      _set_enumeration: function(field, value) {
	 // Currently only supported for select boxes.
	 if (field.type != 'select-one') return;
	 var options = field.options;
	 var selected = $F(field);
	 for (var i=options.length-1; i>=0; --i) {
	    //Remove all options except for the (first) NULL option (if present).
	    var option = $(options[i]);
	    if (option.value != '') option.remove();
	 }
	 field.cleanWhitespace();
	 for (var i=0, len=value.length; i<len; ++i) {
	    //Append options according to the new enumeration received;
	    var item = value[i];
	    var attr = {value: item[0], selected: item[0] == selected};
	    var text = item[1].escapeHTML().gsub(' ', '&nbsp;');
	    var option = new Element('option', attr).update(text);
	    field.insert(option);
	 }
      },

      update: function(response) {
	 // Update the form state in reaction to previously sent AJAX request.
	 var data = response.responseJSON;
	 if (data != null) {
	    var response_number = data['request_number'];
	    var fields = data['fields'];
	    // Ignore the response if other requests were sent in the meantime.
	    // Only the most recently sent request really corresponds to the
	    // current form state!  This also prevents processing responses
	    // coming in wrong order (earlier request may be processed longer
	    // than a later one).
	    if (response_number == this._last_request_number && fields != null) {
	       for (var id in fields) {
		  var field = this._form[id];
		  if (field) {
		     var cdata = fields[id];
		     for (var key in cdata) {
			var value = cdata[key];
		        if      (key == 'editable')    this._set_editability(field, value);
			else if (key == 'value')       this._set_value(field, value);
			else if (key == 'filter')      this._filters[id] = value;
			else if (key == 'enumeration') this._set_enumeration(field, value);
		     }
		  }
	       }
	    }
	 }
      }

   });
