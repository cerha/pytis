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

      initialize: function(form_id, fields) {
	 var form = $(form_id);
	 if (form != null) {
	    form._handler = this;
	    this._form = form;
	    this._fields = fields; 
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
	 var handler = form._handler;
	 // TODO: Avoid AJAX requests during continuous typing.
	 //var now = new Date().valueOf();
	 //if (handler._last_keypress != null && handler._last_keypress + 500 > now) {
	 //   return;
	 //}
	 var values = value.parseQuery(); 
	 var last_values = this.lastValue.parseQuery();
	 var fields = handler._fields;
	 for (i=0; i<fields.length; i++) {
	    var field = fields[i];
	    if (values[field].toJSON() != last_values[field].toJSON()) {
	       var update = function(data) { handler.update(form, data) };
	       form.request({
		     parameters: {_pytis_form_update_request: field},
		     onSuccess: function(transport, data) { update(data) }
	       });
	       break;
	    }
	 }
      },

      update: function(form, data) {
	 for (id in data) {
	    var cdata = data[id];
	    for (key in cdata) {
	       var value = cdata[key];
	       var field = form[id];
	       if (field) { 
		  // TODO: Statically disabled fields are not found, since they
		  // don't have `name' set to prevent faulty browsers to submit
		  // them.
		  if (key == 'editable')
		     field.disabled = !value;
		  else if (key == 'value')
		     // TODO: Set different field types differently.
		     field.value = value;
	       }
	    }
	 }
      }

   });
