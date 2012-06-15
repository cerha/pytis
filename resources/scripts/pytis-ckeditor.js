/* -*- coding: utf-8 -*-
 *
 * Copyright (C) 2012 Brailcom, o.p.s.
 * Author: Hynek Hanke
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

// Remember the base uri of the current script here for later use (hack).
pytis.HtmlField.scripts = document.getElementsByTagName('script');
pytis.HtmlField.base_uri = pytis.HtmlField.scripts[pytis.HtmlField.scripts.length-1].src.replace(/\/[^\/]+$/, '')


pytis.HtmlField.plugin = function(editor) {
    // Construct dialog and add toolbar button
    CKEDITOR.dialog.add('pytis-attachments', pytis.HtmlField.dialog);
    editor.addCommand('insertPytisAttachment', new CKEDITOR.dialogCommand('pytis-attachments'));
    var icon = pytis.HtmlField.base_uri + '/editor-image.png';
    editor.ui.addButton('PytisAttachment', {
	label: editor.lang.common.image,
	command: 'insertPytisAttachment',
	icon: icon
    });
    // Add entry to context menu
    if (editor.contextMenu) {
	editor.addMenuGroup('PytisGroup');
	editor.addMenuItem('editPytisAttachment', {
	    label: editor.lang.image.title,
	    command: 'insertPytisAttachment',
	    group: 'PytisGroup',
	    icon: icon
	});
	editor.contextMenu.addListener(function(element) {
	    if (element)
		element = element.getAscendant('a', true);
	    if (element && !element.isReadOnly() && !element.data('cke-realelement') && element.hasClass('lcg-image'))
 		return {editPytisAttachment: CKEDITOR.TRISTATE_OFF};
	    return null;
	});
    }
};

pytis.HtmlField.dialog = function(editor) {
    // Return the dialog
    return {
	title: pytis._('Image'),
	minWidth: 600,
	minHeight: 300,
	contents: [

	    // Tab Image
	    {id: 'image',
	     label: pytis._('Image'),
	     elements: [
		 {type: 'hbox',
		  children: [
		      {type: 'select',
		       id: 'identifier',
		       label: pytis._('Image'),
		       size: 12,
		       items: [],
		       updateAttachmentList: function(element) {
			   // Construct a list of Wiking attachments for this page
			   var field = $(editor.config.pytisFieldId)._pytis_field_instance;
			   var attachments = field.list_attachments()
			   var options = this.getInputElement().$.options
			   // Save field value before options update
			   value = this.getValue();
			   // Update options
			   options.length = 0;
			   for (var i = 0; i < attachments.length; i++) {
			  var a = attachments[i];
			       if (a.type == 'Image') {
				   var label = (a.title ? a.title + " (" + a.filename + ")": a.filename);
				   options.add(new Option(label, a.filename));
			       }
			   }
			   // Restore former value
			   if (value)
			       this.setValue(value);
		       },
		       onChange: function(element) {
			   // Update image description tab
			   var filename = this.getValue();
			   if (filename) {
			       var field = $(editor.config.pytisFieldId)._pytis_field_instance;
			       var attachment = field.get_attachment(filename);
			       if (attachment) {
				   var dialog = CKEDITOR.dialog.getCurrent();
				   var fields = ['title', 'description', 'thumbnail_size'];
				   for (var i = 0; i < fields.length; i++) {
				       dialog.setValueOf('details', fields[i], attachment[fields[i]]);
				   }
				   // Update image preview
				   if (attachment.thumbnail)
				       $('image-preview').src = attachment.thumbnail.uri;
				   else
				       $('image-preview').src = attachment.uri;
				   $('image-preview').alt = attachment.description;
			       }
			   }
		       },
		       setup: function(element) {
			   this.updateAttachmentList();
			   // Read identifier from the image link
			   var img = element.getFirst();
			   if (img) {
			       var link = img.getAttribute("src");
			       if (link) {
				   var filename = link.match(/\/([^\/]+)\?/)[1];
				   if (filename)
				       this.setValue(filename);
			       }
			   }
		       },
		       commit: function(element) {
			   // Set image source
			   var filename = this.getValue();
			   if (filename) {
			       var field = $(editor.config.pytisFieldId)._pytis_field_instance;
			       var attachment = field.get_attachment(filename);
			       if (attachment) {
				   var uri;
				   if (attachment.thumbnail)
				       uri = attachment.thumbnail.uri;
				   else
				       uri = attachment.uri;
				   var img = element.getFirst();
				   img.setAttribute('src', uri);
			       }
			   }
		       }
		      },
		      
		      // Image preview
		      {type: 'html',
		       id: 'image-preview',
		       html: '<img id="image-preview" src="" alt="" />'
		      }
		  ]},
		 {type: 'select',
		  id: 'align',
		  label: pytis._('Align'),
		  items: [[pytis._('Left'), 'left'], [pytis._('Right'), 'right']],
		  setup: function(element) {
		      // Read alignment of the image
		      var img = element.getFirst();
		      if (img)
			  this.setValue(img.getAttribute('align'));
		  },
		  commit: function(element) {
		      // Set image alignment
		      var img = element.getFirst();
		      img.setAttribute('align', this.getValue());
		  }
		 },

		 {type: 'select',
		  id: 'link-type',
		  label: pytis._('Link'),
		  items: [['Original', 'original', 'original-link'],
			  ['Anchor inside page', 'anchor', 'anchor-link'],
			  ['External link', 'external', 'external-link']],
		  setup: function(element) {
		      if (element.hasClass('original-link'))
			  this.setValue('original');
		      else if (element.hasClass('anchor-link'))
			  this.setValue('anchor');
		      else if (element.hasClass('external-link'))
			  this.setValue('external');
		      else {
			  // Handle cases where type is not specified
			  var link = element.getValue('href');
			  if (link && link.length > 0)
			      this.setValue('external');
			  else
			      this.setValue('original');
		      }
		      this.onChange(element);
		  },
		  commit: function(element) {
		      // Remove all link type classes from element and add the new class
		      for (var i = 0; i < this.items.length; i++) {
			  var val = this.items[i][1];
			  var cls = this.items[i][2];
			  if (val == this.getValue()) {
			      if (!element.hasClass(cls))
				  element.addClass(cls);
			  } else {
			      if (element.hasClass(cls))
				  element.removeClass(cls);
			  }
		      }
		      // Handle the 'original' type of link
		      if (this.getValue() == 'original') {
			  var dialog = CKEDITOR.dialog.getCurrent();
			  var field = $(editor.config.pytisFieldId)._pytis_field_instance;
			  var filename = dialog.getValueOf('image', 'identifier')
			  var attachment = field.get_attachment(filename);
			  if (attachment) {
			      element.setAttribute('rel', "lightbox[gallery]");
			      element.setAttribute('href', attachment.uri);
			  }
		      }
		      // Values for other types of links are handled in the corresponding fields
		  },
		  onChange: function(element) {
		      var dialog = CKEDITOR.dialog.getCurrent();
		      var fields = ['anchor-link', 'external-link'];
		      for (var i = 0; i < fields.length; i++) {
			  var image = dialog.getContentElement('image',  fields[i]);
			  image.getElement().getParent().getParent().hide();
		      }
		      if (this.getValue() == 'anchor') {
			  var image = dialog.getContentElement('image',  'anchor-link');
			  image.getElement().getParent().getParent().show();
		      }
		      if (this.getValue() == 'external') {
			  var image = dialog.getContentElement('image',  'external-link');
			  image.getElement().getParent().getParent().show();
		      }
		  }
		 },

		 {type: 'select',
		  id: 'anchor-link',
		  label: pytis._('Link to anchor'),
		  items: [],
		  onShow: function (element) {
		      // Construct a list of anchors in this page
		      var anchorList = new CKEDITOR.dom.nodeList(editor.document.$.anchors);
		      options = this.getInputElement().$.options;
		      options.length = 0;
		      for (var i = 0, count = anchorList.count(); i < count; i++) {
			  var item = anchorList.getItem(i);
			  options.add(new Option(item.getText() + " (" + item.getAttribute('name') + ")", item.getAttribute('name')));
		      }
		  },
		  setup: function(element) {
		      var dialog = CKEDITOR.dialog.getCurrent();
		      if (dialog.getValueOf('image', 'link-type') == 'anchor') {
			  var link = element.getAttribute("href");
			  if (link.substr(0, 1) == "#") {
			      this.setValue(link.substr(1));
			  }
		      }
		  },
		  commit: function(element) {
		      var dialog = CKEDITOR.dialog.getCurrent();
		      if (dialog.getValueOf('image', 'link-type') == 'anchor') {
			  element.setAttribute("href", "#" + this.getValue());
		      }
		  }
		 },

		 {type: 'text',
		  id: 'external-link',
		  label: pytis._('External link'),
		  setup: function(element) {
		      var dialog = CKEDITOR.dialog.getCurrent();
		      if (dialog.getValueOf('image', 'link-type') == 'external') {
			  this.setValue(element.getAttribute("href"));
		      }
		  },
		  commit: function(element) {
		      var dialog = CKEDITOR.dialog.getCurrent();
		      if (dialog.getValueOf('image', 'link-type') == 'external')
			  element.setAttribute('href', this.getValue());
		  }
		 },
	     ]
	    },

	    // Tab Image Details
	    {id: 'details',
	     label: pytis._('Image Details'),
	     elements: [
		 {type: 'text',
		  id: 'title',
		  label: pytis._('Title')},
		 {type: 'text',
		  id: 'description',
		  label: pytis._('Description')},
		 {type: 'select',
		  id: 'thumbnail_size',
		  label: 'Preview size',
		  items: [['full'], ['small'], ['medium'], ['large']]},
		 {type: 'button',
		  id: 'update',
		  label: pytis._('Update'),
		  onClick: function() {
		      var dialog = CKEDITOR.dialog.getCurrent();
		      var filename = dialog.getValueOf('image', 'identifier')
		      var field = $(editor.config.pytisFieldId)._pytis_field_instance;
		      var error = field.update_attachment(filename, {
			  'title': dialog.getValueOf('details', 'title'),
			  'description': dialog.getValueOf('details', 'description'),
			  'thumbnail_size': dialog.getValueOf('details', 'thumbnail_size')
		      });
		      // TODO: Display error message when error != null.
		      dialog.getContentElement('image', 'identifier').updateAttachmentList();
		  }}
	     ]
	    },

	    // Tab Upload
	    {id: 'upload',
	     label: pytis._('Upload new image'),
	     elements : [
		 // Image preview
		 {type: 'html',
		  id: 'upload-result',
		  html: '<div id="ckeditor-upload-result"></div>'
		 },
		 {type: 'file',
		  id: 'upload',
		  label: editor.lang.image.btnUpload,
		  style: 'height:50px',
		  setup: function(element) {
		      // Register onload hook to respond to upload actions
		      var frame = CKEDITOR.document.getById(this._.frameId);
		      // We need to unregister each event listener first to registering it multiple times
		      this.removeListener('formLoaded', this.onFormLoaded);
		      this.on('formLoaded', this.onFormLoaded, this);
		      frame.removeListener('load', this.onIFrameLoaded);
		      frame.on('load', this.onIFrameLoaded, this);
		  },
		  onFormLoaded: function() {
		      var field = $(editor.config.pytisFieldId)._pytis_field_instance;
		      var frameDocument = CKEDITOR.document.getById(this._.frameId).getFrameDocument();
		      if (frameDocument.$.forms.length > 0) {
			  /* This is a little tricky as the file upload
			   * form is inside an IFRAME.  It is not possible
			   * to submit the AttachmentStorage request
			   * through AJAX due to certain browser
			   * limitations so it is submitted within the
			   * iframe.  We must copy all hidden form fields
			   * from the main edited form to mimic the
			   * bahavior of pytis form AJAX updates and to get
			   * the uploaded data within the Python code in
			   * 'EditForm._attachment_storage_insert()'.
			   */
			  var form = frameDocument.$.forms[0];
			  field._file_upload_form = form;
			  if (form.getAttribute('action') != field._form.getAttribute('action')) {
			      form.setAttribute('action', field._form.getAttribute('action'));
			      hidden_fields = {'_pytis_form_update_request': 1,
					       '_pytis_attachment_storage_field': field._id,
					       '_pytis_attachment_storage_request': 'insert'};
			      for (var i = 0; i < field._form.elements.length; i++) {
				  var e = field._form.elements[i];
				  if (e.type == 'hidden') // && e.name != 'submit')
     				      hidden_fields[e.name] = e.value;
			      }
			      for (var name in hidden_fields) {
     				  Element.insert(form, new Element('input',
								   {'type': 'hidden',
								    'name': name,
								    'value': hidden_fields[name]}));
			      }
			  }
		      }
		  },
		  onIFrameLoaded: function() {
		      var dialog = CKEDITOR.dialog.getCurrent();
		      var body_childs = $(this._.frameId).contentWindow.document.body.childNodes;
		      if ((body_childs.length == 1) && (body_childs[0].tagName.toLowerCase() == 'pre')){
			  // This is a JSON reply
			  var reply = body_childs[0].innerHTML.evalJSON();
			  var msg, cls;
			  dialog.getContentElement('image', 'identifier').updateAttachmentList();
			  dialog.getContentElement('upload', 'upload').reset();
			  if (reply['success'] == true){
			      msg = pytis._("Upload successful");
			      cls = "ckeditor-success";
			  }else{
			      msg = "Error: " + reply['message'];
			      cls = "ckeditor-error";
			  }
			  $('ckeditor-upload-result').update("<p class=\""+cls+"\">"+msg+"</p>");
		      }
		  }
		 },
		 {type: 'fileButton',
		  filebrowser: 'upload:filename',
		  label: editor.lang.image.btnUpload,
		  'for': ['upload', 'upload'],
		  onClick: function() {
		      var field = $(editor.config.pytisFieldId)._pytis_field_instance;
		      // We can't simply call form.submit(), because Wiking
		      // uses a hidden field named 'submit' for its internal
		      // purposes and this hidden field masks the submit method
		      // (not really clever...).
		      var result = document.createElement('form').submit.call(field._file_upload_form);
		  }
		 }
	     ]}
	],

	onShow: function() {
	    // Check if editing an existing element or inserting a new one
	    var sel = editor.getSelection();
	    var element = sel.getStartElement();
	    if (element)
		element = element.getAscendant('a', true);
	    if (!element || element.getName() != 'a' || element.data('cke-realelement') || !element.hasClass('lcg-image')) {
		element = editor.document.createElement('a');
		element.addClass('lcg-image');
		var img = editor.document.createElement('img');
		element.append(img);
		this.insertMode = true;
	    }
	    else
		this.insertMode = false;
	    this.element = element;
	    this.setupContent(this.element);
	},

	onOk: function(element) {
	    // Insert or update the element
	    if (this.insertMode)
		editor.insertElement(this.element);
	    this.commitContent(this.element);
	}

    };
};

pytis.HtmlField.on_dialog = function(event) {
    if (event.data.name == 'link') {
	event.data.definition.removeContents('advanced');
	event.data.definition.removeContents('target');
    }
};

