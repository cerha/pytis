/* -*- coding: utf-8 -*-
 *
 * Copyright (C) 2009 Brailcom, o.p.s.
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

var PytisFormHandler = Class.create({

      initialize: function(form_id, fields, filters) {
	 var form = $(form_id);
	 if (form != null) {
	    form._handler = this;
	    this._form = form;
	    this._fields = fields; 
	    this._filters = filters; 
	    this._last_keypress = null; 
	    new Form.Observer(form, 1, this.on_change);
	    //for (i=0; i<fields.length; i++)
	    //   form[fields[i]].onkeypress = this.on_keypress;
	 }
      },

      on_keypress: function(event) {
	 var handler = this.form._handler;
	 handler._last_keypress = new Date().valueOf();
	 return true;
      },

      on_change: function(form, value) {
	 // Send AJAX request in reaction to user changes of form values.
	 var handler = form._handler;
	 // TODO: Avoid AJAX requests during continuous typing.
	 //var now = new Date().valueOf();
	 //if (handler._last_keypress != null && handler._last_keypress + 500 > now) {
	 //   return;
	 //}
	 var values = value.parseQuery(); 
	 var last_values = this.lastValue.parseQuery();
	 var fields = handler._fields;
	 for (var i=0; i<fields.length; i++) {
	    var field = fields[i];
	    // Disabled fields are not present in values/last_values, but also
	    // checkbox fields are not there if unchecked.
	    if ((field in values || field in last_values) && values[field] != last_values[field]) {
	       var update = function(data) { handler.update(form, data) };
	       form.request({
		     parameters: {_pytis_form_update_request: field,
			          _pytis_form_filter_state: $H(handler._filters).toJSON()},
		     onSuccess: function(transport, data) { update(data) }
	       });
	       break;
	    }
	 }
      },

      update: function(form, data) {
	 // Update the form state in reaction to previously sent AJAX request.
	 for (id in data) {
	    var cdata = data[id];
	    for (key in cdata) {
	       var value = cdata[key];
	       var field = form[id];
	       if (field) { 
		  if (key == 'editable')
		     field.disabled = !value;
		  else if (key == 'value') {
		     // Set the field value depending on field type.
		     if (field.type == 'checkbox')
			field.checked = value == 'T';
		     else
			field.value = value;
		  } 
		  else if (key == 'filter')
		     form._handler._filters[id] = value;
		  else if (key == 'enumeration' && field.type == 'select-one') {
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
		  }
	       }
	    }
	 }
      }

   });
