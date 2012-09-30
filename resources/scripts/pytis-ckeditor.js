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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
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
    var types = ['Image', 'Audio', 'Video', 'Resource', 'Exercise', 'MathML'];
    editor.addMenuGroup('PytisGroup');
    for (var i=0; i<types.length; i++){
        var type = types[i];
        var ltype = types[i].toLowerCase();
        /* Add dialog */
        CKEDITOR.dialog.add('pytis-' + ltype, eval('pytis.HtmlField.' + ltype + '_dialog'));
        /* Add command */
        editor.addCommand('insertPytis' + type, new CKEDITOR.dialogCommand('pytis-' + ltype));
        var icon = pytis.HtmlField.base_uri + '/editor-' + ltype + '.png';
        /* Add UI button */
        editor.ui.addButton('Pytis' + type, {
            label: pytis._(types[i]),
            command: 'insertPytis' + type,
            icon: icon
        });
        /* Add context menu entry */
        if (editor.contextMenu) {
            editor.addMenuItem('editPytis' + type, {
                label: pytis._('Edit') + ' ' + pytis._(type),
                command: 'insertPytis' + type,
                group: 'PytisGroup',
                icon: icon
            });

        }
    }

    /* Create insertSpaceBefore and insertSpaceAfter menu items */
    function insert_space(direction) {
	var sel = editor.getSelection();
	var element = sel.getStartElement();
	element = element.getAscendant('div', true);
	var paragraph = new CKEDITOR.dom.element('p');
	if (direction == 'before')
	    paragraph.insertBefore(element);
	else if (direction == 'after'){
	    paragraph.insertAfter(element);
	}
    }
    editor.addCommand('insertSpaceBefore', new CKEDITOR.command(
	editor,
	{
	    exec : function( editor )
	    {
		insert_space('before');
	    }
	}));
    editor.addMenuItem('insertSpaceBefore', {
	label: pytis._("Insert space before"),
	command: 'insertSpaceBefore',
	group: 'PytisGroup',
    });
    editor.addCommand('insertSpaceAfter', new CKEDITOR.command(
	editor,
	{
	    exec : function( editor )
	    {
		insert_space('after');
	    }
	}));
    editor.addMenuItem('insertSpaceAfter', {
	label: pytis._("Insert space after"),
	command: 'insertSpaceAfter',
	group: 'PytisGroup',
    });

     /* Add a common context menu listener handling all the attachment types */
    if (editor.contextMenu) {
	editor.contextMenu.addListener(function(element) {
            if (element)
		element = element.getAscendant('a', true) || element.getAscendant('div', true)
		|| element.getAscendant('span', true);
            if (element && !element.data('cke-realelement')){
		var result = {};
		for (var i=0; i<types.length; i++){
                    if (element.hasClass('lcg-'+types[i].toLowerCase())){
			result['editPytis'+types[i]] = CKEDITOR.TRISTATE_OFF;
                    }
		}
		if ((element.getName() == 'div') && (element.isReadOnly())){
                    result['insertSpaceBefore'] = CKEDITOR.TRISTATE_OFF;
                    result['insertSpaceAfter'] = CKEDITOR.TRISTATE_OFF;
		}
		return result;
	    }
	});
    }
}


function ck_element (dialog, id) {
    /* Helper function to address a particular element in dialog definition by its id
     *
     * The function searches element definitions by their id among the elements
     * of the first dialog tab. It also searches for childern inside any 'hbox'
     * and 'vbox' containers.
     *
     * Arguments:
     *  dialog ... the dialog to search for the element
     *  id  ... id of the element
     *
     * Return value:
     *  Returns the element or nul if none is found
     */

    function ck_get_element_from_list (elements, id) {
        for (var i = 0; i < elements.length; i++) {
            if (elements[i].id == id){
                return elements[i];
            }
            else if (elements[i].type == 'hbox' || elements[i].type == 'vbox'){
                var el = ck_get_element_from_list(elements[i].children, id)
                if (el)
                    return el;
            }
        }
        return null;
    }
    for (var i=0; i < dialog['contents'].length; i++){
	var elements = dialog['contents'][i]['elements'];
	var result = ck_get_element_from_list(elements, id);
	if (result)
	    return result;
    }
    return null;
}

function ck_get_dom_subelement (element, path){
    /* Get a subelement of the given element according to descent path
     *
     * element ... DOM element whose subelement should be returned
     * path ... an array containing a full path to the subelement. Each entry in the
     *          array is either a tag name or a class name prepended by '.'
     *
     * Example:
     *  ck_get_dom_subelement(e, ['.lcg-example', 'div', 'a'])
     *  Will return an 'a' element from a 'div' element from any element with class '.lcg-example'
     *  being child of the former element _e_.
     *
     * Returns the first matching subelement or null.
     */

    var ch = element.getChildren();
    var item;
    for (var i=0; i < ch.count(); i++){
    	item = ch.getItem(i);
    	if (typeof(item.getName) == 'function'){ // e.g. comments do not have a name
    	    if ((item.getName() == path[0]) ||  // match element name
    		((path[0][0] == '.') && item.hasClass(path[0].slice(1)))) // match element class
    	    {
    		if (path.length > 1){
    		    return ck_get_dom_subelement(item, path.slice(1));
    		}else{
    		    return item;
    		}
    	    }
    	}
    }
    return null;
}

function ck_dialog_update_attachment_list (editor, field, attachment_type, keep_value) {
    /* Helper function to update attachment list in a select field
     *
     * Only entries of the given attachment_type are listed. keep_value
     * determines whether to keep the original value before the update
     * (if possible). keep_value needs to be set to false if called from
     * inside dialog element setup functions.
     */

    // Construct a list of Wiking attachments for this page
    var pytis_field = $(editor.config.pytisFieldId)._pytis_field_instance;
    var attachments = pytis_field.list_attachments()
    var options = field.getInputElement().$.options
    // Save field value before options update
    if (keep_value)
	var value = field.getValue();
    // Update options
    options.length = 0;
    for (var i = 0; i < attachments.length; i++) {
        var a = attachments[i];
        if (a.type == attachment_type) {
            var label = (a.title ? a.title + " (" + a.filename + ")": a.filename);
            options.add(new Option(label, a.filename));
        }
    }
    // Restore former value
    if (keep_value && value)
        field.setValue(value);
}

function ck_dialog_update_media_preview (attachment, id) {
    /* Helper function to update preview of a media attachment
     *
     * The preview si realized via JWPlayer inserted into DOM element with given id.
     */
    var flashvars = {'file': attachment.uri};
    var player_uri = '/_resources/flash/mediaplayer.swf';
    embed_swf_object(player_uri, id, 400, 400, flashvars, '9', '<p>Flash not available</p>', true);
}

pytis.HtmlField.attachment_dialog = function(editor, attachment_name, attachment_type, attachment_class, attachment_properties, html_elements) {
    /* Basic attachment dialog for the various types of attachments
     *
     * The dialog can be further modified according to the particular specifics
     * of each attachment type, such as handling additional attachment fields,
     * handling preview and HTML elements input and output.
     *
     * Arguments:
     *  editor ... the editor instance
     *  attachment_name ... name for the attachment type to be used in the UI (should be localized)
     *  attachment_type ... identifier of the attachment type as specified in Pytis (e.g. Image, Video, Resource...)
     *  attachment_class ... CSS class for the top-level HTML element of this object
     *  attachment_properties ... properties of the attachment as passed via the Pytis Attachment API
     *    (e.g. title, description) which should be editable through the dialog. Each listed property
     *    must have a field with the identical id defined in the dialog. Fields 'title' and 'description'
     *    are already defined by this dialog, possible additional fields must be defined by extending this
     *    dialog. See for example the additional thumbnail_size field used in the image dialog).
     *  html_elements ... array of element names for the HTML representation of this object. Each successive
     *    element will be nested in the previous one, so e.g. ['a', 'img'] means an 'img' element nested
     *    in an 'a' element.
     *
     * Return value:
     *  Returns a dictionary description of the dialog for the CKEDITOR.dialog.add factory.
     */

    return {
        minWidth: 600,
        minHeight: 520,
        title: attachment_name,
        contents: [
            // Main Tab
            {id: 'main',
             label: attachment_name,
             elements: [
                 {type : 'hbox',
                  widths : [ '60%', '40%'],
                  height: '150px',
                  children :
                  [
                      {type: 'select',
                       size: 14,
                       id: 'identifier',
                       label: attachment_name,
                       className: 'attachment-selector',
                       items: [],
		       updateAttachmentList: function(element, keep_value) {
			   ck_dialog_update_attachment_list(editor, this, attachment_type, keep_value);
		       },
                       updatePreview: function(attachment) {
                           // Update preview (to be overriden in children)
                       },
                       onChange: function(element) {
                           var filename = this.getValue();
                           if (filename) {
                               var field = $(editor.config.pytisFieldId)._pytis_field_instance;
                               attachment = field.get_attachment(filename);
                               if (attachment) {
                                   this['attachment'] = attachment;
                                   var dialog = CKEDITOR.dialog.getCurrent();
                                   var fields = attachment_properties;
                                   for (var i = 0; i < fields.length; i++) {
                                       dialog.setValueOf('main', fields[i], attachment[fields[i]]);
                                   }
                                   this.updatePreview(attachment);
                               }
                           }
                       },
                       setup: function(element) {
                           this.updateAttachmentList(false);
                           // Read identifier from the source
                           var resource = element.getAttribute('data-lcg-resource');
                           if (resource)
                               this.setValue(resource);
                       },
                       commit: function(element) {
                           if (this.attachment) {
                               element.setAttribute('data-lcg-resource', this.attachment.filename);
                               element.setAttribute('href', this.attachment.uri)
			   }
                       },
                      },
                      {type: 'html',
                       id: 'preview',
                       html: '',
                      }
                  ]},
                 {type: 'html',
                  id: 'upload-result',
                  html: '<div id="ckeditor-upload-result"></div>'
                 },
                 {type : 'hbox',
                  children :
                  [
                      {type: 'file',
                       id: 'upload',
                       label: editor.lang.image.btnUpload,
		       size: '30',
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
                               dialog.getContentElement('main', 'identifier').updateAttachmentList(true);
                               dialog.getContentElement('main', 'upload').reset();
                               if (reply['success'] == true){
                                   msg = pytis._("Upload successful");
                                   cls = "ckeditor-success";
                               } else {
                                   msg = reply['message'];
                                   cls = "ckeditor-error";
                               }
                               $('ckeditor-upload-result').update("<p class=\""+cls+"\">"+msg+"</p>");
                           }
                       }
                      },
                      {type: 'fileButton',
                       filebrowser: 'upload:filename',
                       label: pytis._("Upload new file"),
                       'for': ['main', 'upload'],
                       onClick: function() {
                           var dialog = CKEDITOR.dialog.getCurrent();
                           var field = $(editor.config.pytisFieldId)._pytis_field_instance;
			   if (dialog.getContentElement('main', 'upload').getValue()) {
                               // We can't simply call form.submit(), because Wiking
                               // uses a hidden field named 'submit' for its internal
                               // purposes and this hidden field masks the submit method
                               // (not really clever...).
                               var result = document.createElement('form').submit.call(field._file_upload_form);
			   } else {
                               $('ckeditor-upload-result').update(pytis._("Select a file to be uploaded."));
			   }
			   return false;
                       }
                      },
                  ]
                 },
                 {type: 'text',
                  id: 'title',
                  label: pytis._('Title'),
                  commit: function(element) {
		      var value = this.getValue();
		      if (value){
			  element.setText(this.getValue());
		      }else{
			  var dialog = CKEDITOR.dialog.getCurrent();
			  element.setText(dialog.getValueOf('main', 'identifier'));
		      }
                  },
                 },
                 {type: 'text',
                  id: 'description',
                  label: pytis._('Description'),
                  commit: function(element) {
                      element.setAttribute('alt', this.getValue());
                  },
                 },
             ]},
        ],
        onShow: function() {
            // Check if editing an existing element or inserting a new one
            var sel = editor.getSelection();
            var element = sel.getStartElement();
            if (element)
                element = element.getAscendant(html_elements[0], true);
            if (!element || element.getName() != html_elements[0] || element.data('cke-realelement') || !element.hasClass(attachment_class)) {
                // The element doesn't exist yet, create it together with all its descendants
                element = editor.document.createElement(html_elements[0]);
                element.addClass(attachment_class);
                var parent = element;
                for (var i=1; i<html_elements.length; i++){
                    var child = editor.document.createElement(html_elements[1]);
                    parent.append(child);
                    parent = child;
                }
                this.insertMode = true;
            }
            else
                this.insertMode = false;
            this.element = element;
            this.setupContent(this.element);
        },
        onOk: function(element) {
            // Update attachment attributes
            var dialog = CKEDITOR.dialog.getCurrent();
            var filename = dialog.getValueOf('main', 'identifier')
            var field = $(editor.config.pytisFieldId)._pytis_field_instance;
            attributes = {}
            for (var i=0; i<=attachment_properties.length; i++)
                attributes[attachment_properties[i]] = dialog.getValueOf('main', attachment_properties[i]);
            var error = field.update_attachment(filename, attributes); // TODO: Display error message when error != null.
            // Insert or update the HTML element
            if (this.insertMode){
                editor.insertElement(this.element);
            }
            this.commitContent(this.element);
        },
    };
};

pytis.HtmlField.image_dialog = function(editor) {

    dialog = pytis.HtmlField.attachment_dialog(
        editor, pytis._("Image"), 'Image', "lcg-image",
        ['title', 'description', 'thumbnail_size'],
        ['a', 'img']);

    ck_element(dialog, 'identifier').updatePreview = function(attachment) {
        if (attachment){
            if (attachment.thumbnail)
                $('image-preview').src = attachment.thumbnail.uri;
            else
                $('image-preview').src = attachment.uri;
            $('image-preview').alt = attachment.description;
        }
    }

    ck_element(dialog, 'identifier').setup = function(element) {
        this.updateAttachmentList(false);
        // Read identifier from the image link
        var img = element.getFirst();
        if (img) {
            var attachment = img.getAttribute('data-lcg-resource');
            if (attachment)
                this.setValue(attachment);
        }
    }

    ck_element(dialog, 'identifier').commit = function(element) {
        var attachment = this.attachment;
        if (attachment) {
            var img = element.getFirst();
            if (img) {
		var uri = (attachment.thumbnail ? attachment.thumbnail.uri : attachment.uri);
		img.setAttribute('src', uri);
		img.setAttribute('data-lcg-resource', attachment.filename);
	    }
        }
    }

    ck_element(dialog, 'preview').html = '<div class="preview-container"><img id="image-preview" src="" alt="" /></div>';

    ck_element(dialog, 'title').commit = function(element) {
        var img = element.getFirst();
        if (img)
            img.setAttribute('title', this.getValue());
    }

    ck_element(dialog, 'description').commit = function(element) {
        var img = element.getFirst()
        if (img)
            img.setAttribute('alt', this.getValue());
    }

    dialog['contents'][0].elements = dialog['contents'][0].elements.concat([
        {type: 'select',
         id: 'thumbnail_size',
         label: pytis._('Display as'),
         items: [[pytis._('Full size'), 'full'],
                 [pytis._('Small preview'), 'small'],
                 [pytis._('Medium preview'), 'medium'],
                 [pytis._('Large preview'), 'large']]},
                 //[pytis._('Title (text only)'), 'text']]},
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
             if (img)
		 img.setAttribute('align', this.getValue());
         }
	 // TODO: When 'full' is selected, don't allow 'enlarge' in 'link-type' selection.
        },
        {type : 'hbox',
         widths : [ '20%', '80%'],
         children :
         [
             {type: 'select',
              id: 'link-type',
              label: pytis._('Behavior'),
              items: [[pytis._('Enlarge on click'), 'enlarge', 'enlarge-image'],
                      //[pytis._('Download link'), 'download', 'download-link'],
                      [pytis._('Link'), 'external', 'external-link'],
                      [pytis._('Link to anchor'), 'anchor', 'anchor-link']],
                      //[pytis._('No action'), 'noact', 'no-action'],
              setup: function(element) {
                  if (element.hasClass('enlarge-image'))
                      this.setValue('enlarge');
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
                          this.setValue('enlarge');
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
                  // Handle the 'enlarge' type of link
                  if (this.getValue() == 'enlarge') {
                      var dialog = CKEDITOR.dialog.getCurrent();
		      var field = dialog.getContentElement('main', 'identifier')
                      var attachment = field.attachment;
                      if (attachment)
                          element.setAttribute('href', attachment.uri);
                  }
                  // Values for other types of links are handled in the corresponding fields
              },
              onChange: function(element) {
                  var dialog = CKEDITOR.dialog.getCurrent();
                  var fields = ['anchor-link', 'external-link'];
                  for (var i = 0; i < fields.length; i++) {
                      var image = dialog.getContentElement('main',  fields[i]);
                      image.getElement().getParent().hide();
                  }
                  if (this.getValue() == 'anchor') {
                      var image = dialog.getContentElement('main',  'anchor-link');
                      image.getElement().getParent().show();
                  }
                  if (this.getValue() == 'external') {
                      var image = dialog.getContentElement('main',  'external-link');
                      image.getElement().getParent().show();
                  }
              }
             },
             {type: 'select',
              id: 'anchor-link',
              label: pytis._('Select the anchor'),
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
                  if (dialog.getValueOf('main', 'link-type') == 'anchor') {
                      var link = element.getAttribute("href");
                      if (link.substr(0, 1) == "#") {
                          this.setValue(link.substr(1));
                      }
                  }
              },
              commit: function(element) {
                  var dialog = CKEDITOR.dialog.getCurrent();
                  if (dialog.getValueOf('main', 'link-type') == 'anchor') {
                      element.setAttribute("href", "#" + this.getValue());
                  }
              }
             },
             {type: 'text',
              id: 'external-link',
              label: pytis._('Link target (URL)'),
                       setup: function(element) {
                           var dialog = CKEDITOR.dialog.getCurrent();
                           if (dialog.getValueOf('main', 'link-type') == 'external') {
                               this.setValue(element.getAttribute("href"));
                           }
                       },
              commit: function(element) {
                  var dialog = CKEDITOR.dialog.getCurrent();
                  if (dialog.getValueOf('main', 'link-type') == 'external')
                      element.setAttribute('href', this.getValue());
              }
             },
         ]}]);
    return dialog;
}

pytis.HtmlField.audio_dialog = function(editor) {

    dialog = pytis.HtmlField.attachment_dialog(
        editor, pytis._("Audio"), 'Audio', "lcg-audio",
        ['title', 'description'],
        ['a']);

    ck_element(dialog, 'identifier').updatePreview = function(attachment) {
	if (attachment)
	    ck_dialog_update_media_preview(attachment, 'audio-preview');
    }

    ck_element(dialog, 'preview').html = '<div class="preview-container"><div id="audio-preview"></div></div>';

    return dialog;
}

pytis.HtmlField.video_dialog = function(editor) {

    dialog = pytis.HtmlField.attachment_dialog(
        editor, pytis._("Video"), 'Video', "lcg-video",
        ['title', 'description'],
        ['a']);

    ck_element(dialog, 'identifier').updatePreview = function(attachment) {
	if (attachment)
	    ck_dialog_update_media_preview(attachment, 'video-preview');
    }

    ck_element(dialog, 'preview').html = '<div class="preview-container"><div id="video-preview"></div></div>';

    return dialog;
}

pytis.HtmlField.resource_dialog = function(editor) {

    dialog = pytis.HtmlField.attachment_dialog(
        editor, pytis._("Attachment"), 'Resource', "lcg-resource",
        ['title', 'description'],
        ['a']);

    return dialog;
}

pytis.HtmlField.on_dialog = function(event) {
    if (event.data.name == 'link') {
        event.data.definition.removeContents('advanced');
        event.data.definition.removeContents('target');
    }
};

pytis.HtmlField.exercise_dialog = function(editor) {
    /* CKEditor dialog for editing LCG exercises.
     *
     * Arguments:
     *  editor ... the editor instance
     *
     * Return value:
     *  Returns a dictionary description of the dialog for the CKEDITOR.dialog.add factory.
     */

    var help_url = "/_doc/eurochance/exercises?framed=1";

    var sub_elements = [['instructions', 'pre'],
			['example', 'pre'],
			['src', 'pre'],
			['sound-file', 'a'],
			['audio-version', 'a'],
			['transcript', 'pre'],
			['reading', 'pre'],
			['explanation', 'pre'],
		       ];

    var dialog = {
        minWidth: 950,
        minHeight: 500,
        title: pytis._("Exercise"),
        contents: [
            {id: 'main',
             label: pytis._("Exercise"),
             elements: [
		 {type : 'hbox',
		  children :
		  [
		      {type : 'vbox',
		       children :
		       [
			   {type: 'select',
			    id: 'type',
			    label: pytis._('Type of exercise'),
			    items: [[pytis._('True-False Statements'), 'TrueFalseStatements'],
				    [pytis._('Multiple Choice Questions'), 'MultipleChoiceQuestions'],
				    [pytis._('Gap Filling'), 'GapFilling'],
				    [pytis._('Cloze'), 'Cloze'],
				    [pytis._('Exposed Cloze'), 'ExposedCloze'],
				    [pytis._('NumberedCloze'), 'NumberedCloze'],
				    [pytis._('NumberedExposedCloze'), 'NumberedExposedCloze'],
				    [pytis._('Selections'), 'Selections'],
				    [pytis._('SentenceCompletion'), 'SentenceCompletion'],
				    [pytis._('Transformation'), 'Transformation'],
				    [pytis._('Substitution'), 'Substitution'],
				    [pytis._('Dictation'), 'Dictation'],
				   ],
			   },
			   {type: 'textarea',
			    id: 'instructions',
			    label: pytis._('Instructions'),
			    rows: 3,
			    cols: 60,
			   },
			   {type: 'textarea',
			    id: 'example',
			    rows: 3,
			    cols: 60,
			    label: pytis._('Example')
			   },
			   {type: 'textarea',
			    id: 'src',
			    rows: 15,
			    cols: 60,
			    label: pytis._('Exercise body'),
			   },
		       ]},
		      {type : 'html',
		       id : 'help',
		       html: '<iframe id="exercise-help" src=""></iframe>'
		      }
		  ]},
	     ]},
            {id: 'audio',
             label: pytis._("Audio"),
             elements: [
		 {type : 'hbox',
		  children :
		  [
		      {type: 'select',
		       items: [[pytis._("No sound"), ''],],
		       id: 'sound-file',
		       className: 'attachment-selector',
                       size: 14,
		       label: pytis._('Sound file'),
                       updateAttachmentList: function(element, keep_value) {
			   ck_dialog_update_attachment_list(editor, this, 'Audio', keep_value);
		       },
		      },
		      {type: 'html',
		       id: 'sound-file-preview',
		       html: '<div class="preview-container"><div id="exercise-sound-file-preview"></div></div>'
		      }
		  ]},
		 {type : 'hbox',
		  children :
		  [
		      {type: 'select',
		       items: [[pytis._("No audio version"), ''],],
		       id: 'audio-version',
		       className: 'attachment-selector',
                       size: 14,
		       label: pytis._('Audio version'),
                       updateAttachmentList: function(element, keep_value) {
			   ck_dialog_update_attachment_list(editor, this, 'Audio', keep_value);
		       },
		      },
		      {type: 'html',
		       id: 'audio-version-preview',
		       html: '<div class="preview-container"><div id="exercise-audio-version-preview"></div></div>'
		      }
		  ]},
	     ]
	    },
            {id: 'transcript',
             label: pytis._("Transcript"),
             elements: [
		 {type: 'textarea',
		  id: 'transcript',
		  label: pytis._('Transcript'),
		  rows: 20,
		 },
	     ]
	    },
            {id: 'reading',
             label: pytis._("Reading"),
             elements: [
		 {type: 'textarea',
		  id: 'reading',
		  label: pytis._('Reading'),
		  rows: 20,
		 },
	     ]
	    },
            {id: 'explanation',
             label: pytis._("Explanation"),
             elements: [
		 {type: 'textarea',
		  id: 'explanation',
		  label: pytis._('Explanation'),
		  rows: 20,
		 },
	     ]
	    },
	],
	onShow: function() {
	    function insert_subelement(element, name, id){
		    var sub_element = editor.document.createElement(name);
		    sub_element.addClass('lcg-exercise-' + id);
		    element.append(sub_element);
	    }
            // Check if editing an existing element or inserting a new one
            var element = editor.getSelection().getStartElement();
            if (element)
		element = element.getAscendant('div', true);
            if (!element || element.getName() != 'div' || element.data('cke-realelement') || !element.hasClass('lcg-exercise')) {
		// Create a new exercise structure
		element = editor.document.createElement('div',
							{'attributes': {'contenteditable': 'false'},
							 'styles': {'display': 'inline-block'}});
		element.addClass('lcg-exercise');
		element.setAttribute('contenteditable', 'false');
		for (var i = 0; i < sub_elements.length; i++){
		    insert_subelement(element, sub_elements[i][1], sub_elements[i][0]);
		}
		this.insertMode = true;
            }
            else{
		// We have encountered an existing exercise structure, check it for completeness
		for (var i = 0; i < sub_elements.length; i++){
		    if (!ck_get_dom_subelement(element, ['.lcg-exercise-'+sub_elements[i][0]])){
			insert_subelement(element, sub_elements[i][1], sub_elements[i][0]);
		    }
		}
		this.insertMode = false;
	    }
            this.element = element;
            this.setupContent(this.element);
	},
	onOk: function(element) {
            if (this.insertMode){
		editor.insertElement(this.element);
            }
            this.commitContent(this.element);
	},
    };

    function get_content(id, element, field){
	var subel = ck_get_dom_subelement(element, ['.lcg-exercise-'+id])
    	field.setValue(subel.getHtml());
    }
    function put_content(id, element, field){
	var subel = ck_get_dom_subelement(element, ['.lcg-exercise-'+id])
    	subel.setHtml(field.getValue());
    }

    ck_element(dialog, 'type').setup = function(element){
	// Using HTML 5 data attributes to store exercise type
	this.setValue(element.data('type'));
    }

    ck_element(dialog, 'type').commit = function(element){
	element.data('type', this.getValue());
    }

    ck_element(dialog, 'type').onChange = function(element){
	$('exercise-help').src = help_url + '#' + this.getValue();
    }

    ck_element(dialog, 'instructions').setup = function(element){
	get_content('instructions', element, this);
    }
    ck_element(dialog, 'instructions').commit = function(element){
	put_content('instructions', element, this);
    }

    ck_element(dialog, 'example').setup = function(element){
	get_content('example', element, this);
    }

    ck_element(dialog, 'example').commit = function(element){
	put_content('example', element, this);
    }

    ck_element(dialog, 'src').setup = function(element){
	get_content('src', element, this);
    }

    ck_element(dialog, 'src').commit = function(element){
	put_content('src', element, this);
    }

    ck_element(dialog, 'reading').setup = function(element){
	get_content('reading', element, this);
    }

    ck_element(dialog, 'reading').commit = function(element){
	put_content('reading', element, this);
    }

    ck_element(dialog, 'transcript').setup = function(element){
	get_content('transcript', element, this);
    }

    ck_element(dialog, 'transcript').commit = function(element){
	put_content('transcript', element, this);
    }

    ck_element(dialog, 'explanation').setup = function(element){
	get_content('explanation', element, this);
    }

    ck_element(dialog, 'explanation').commit = function(element){
	put_content('explanation', element, this);
    }

    function get_resource(id, element, field){
	var subel = ck_get_dom_subelement(element, ['.lcg-exercise-'+id])
        var link = subel.getAttribute("href");
        if (link) {
            var filename = link.match(/\/([^\/]+)$/)[1];
            if (filename)
		field.setValue(filename);
        }
    }
    function put_resource(id, element, field){
	var subel = ck_get_dom_subelement(element, ['.lcg-exercise-'+id])
	var id = field.getValue();
	if (id){
	    if (field.attachment){
    		subel.setAttribute('href', field.attachment.uri);
		subel.setAttribute('data-lcg-resource', field.attachment.filename);
     		subel.setText(id);
	    }
	}else{
	    subel.setAttribute('href', '');
	    subel.setAttribute('data-lcg-resource', '');
    	    subel.setText('');
	}
    }

    ck_element(dialog, 'audio-version').setup = function(element){
	this.updateAttachmentList(false);
	get_resource('audio-version', element, this);
    }

    ck_element(dialog, 'audio-version').onChange = function(element){
        var pytis_field = $(editor.config.pytisFieldId)._pytis_field_instance;
	this.attachment = pytis_field.get_attachment(this.getValue());
	if (this.attachment)
	    ck_dialog_update_media_preview(this.attachment, 'exercise-audio-version-preview');
    }

    ck_element(dialog, 'audio-version').commit = function(element){
	put_resource('audio-version', element, this);
    }

    ck_element(dialog, 'sound-file').setup = function(element){
	this.updateAttachmentList(false);
	get_resource('sound-file', element, this);
    }

    ck_element(dialog, 'sound-file').onChange = function(element){
        var pytis_field = $(editor.config.pytisFieldId)._pytis_field_instance;
	this.attachment = pytis_field.get_attachment(this.getValue());
	if (this.attachment)
	    ck_dialog_update_media_preview(this.attachment, 'exercise-sound-file-preview');
    }

    ck_element(dialog, 'sound-file').commit = function(element){
	put_resource('sound-file', element, this);
    }

    ck_element(dialog, 'help').setup = function(element){
	$('exercise-help').src = help_url + '#' + 'exercise-header';
    }

    return dialog;
};

pytis.HtmlField.mathml_dialog = function(editor) {

    name = pytis._("MathML");

    var dialog = {
        minWidth: 500,
        minHeight: 380,
        title: name,
        contents: [
            {id: 'main',
             label: name,
             elements: [
		 {type : 'vbox',
		  id : 'source-ascii-box',
		  children :
		  [
                      {type: 'textarea',
                       id: 'source-ascii',
                       label: pytis._("ASCII"),
                      },
		      {type: 'html',
		       html: '<div class="ckeditor-help">'+pytis._("Guide on ")+'<a href="http://www1.chapman.edu/~jipsen/mathml/asciimathsyntax.html">http://www1.chapman.edu/~jipsen/mathml/asciimathsyntax.html</a></div>'
		      },
		  ]},
		 {type : 'vbox',
		  id : 'source-mathml-box',
		  children :
		  [
                      {type: 'textarea',
                       id: 'source-mathml',
                       label: pytis._("MathML"),
                      },
		      {type: 'html',
		       html: '<div class="ckeditor-help">'+pytis._("To copy text into external editor, use Ctrl+A Ctrl+X. To paste text from external editor, use Ctrl+V. (Press CMD instead of Ctrl on Mac.)")+'</div>'
		      },
		  ]},
                 {type: 'button',
                  id: 'toggle-input',
                  label: "Switch ASCII / MathML editing",
                 },
                 {type: 'html',
                  id: 'math-preview',
                  html: '<div id="math-preview-container"><div id="math-preview"></div></div>',
                 },
             ]
            }
        ],
        onShow: function() {
            // Check if editing an existing element or inserting a new one
            var sel = editor.getSelection();
            var element = sel.getStartElement();
            var tag = 'span';
            if (element)
                element = element.getAscendant(tag, true);
            if (!element || element.getName() != tag) {
                element = editor.document.createElement(
		    tag,
		    {'attributes': {'contenteditable': 'false'},
		     'styles': {'display': 'inline-block'}});
		element.addClass('lcg-mathml');
		// We do not setup the inner <math> element here because without
		// a reliable support for .setHtml()/.getHtml() in CKEditor or
		// innerHTML in browsers, we would have no way to acces its contents
                this.insertMode = true;
            }
            else{
                this.insertMode = false;
	    }
            this.element = element;
            this.setupContent(this.element);

	    if (this.insertMode){
		switch_method('ascii');
	    }else{
		if (this.getValueOf('main', 'source-ascii')){
		    switch_method('ascii');
		    this.setValueOf('main', 'source-mathml', "");
		}else{
		    switch_method('mathml');
		}
	    }

        },
        onOk: function(element) {
            if (this.insertMode){
                editor.insertElement(this.element);
            }
            this.commitContent(this.element);
        }
    };

    switch_method = function (method){
	var dialog = CKEDITOR.dialog.getCurrent();
        var mathml = dialog.getContentElement('main',  'source-mathml-box');
        var ascii = dialog.getContentElement('main',  'source-ascii-box');

	if (method == 'mathml'){
	    mathml.getElement().show();
	    ascii.getElement().hide();
	}
	if (method == 'ascii'){
	    mathml.getElement().hide();
	    ascii.getElement().show();
	}
    }

    clean_mathml = function(mathml, annotation, source) {
	var clean = mathml;
	var production_args = "";
	// Strip outer math tags, ignoring their attributes
	var inner_mathml = mathml.replace(/<\/?math.*?>/gi, "");
	// Rewrap in <math> tags with desired attributes and annotations
	if (!source)
	    production_args = 'contenteditable="false" style="display:inline-block"';
	if (annotation.length > 0){
	    return '<math xmlns="http://www.w3.org/1998/Math/MathML" ' + production_args + '>'
		+ '<semantics>'
		+ inner_mathml
	        + '<annotation encoding="ASCII">' + annotation + '</annotation>'
		+ '</semantics>'
		+ '</math>';
	} else {
	    return '<math xmlns="http://www.w3.org/1998/Math/MathML" ' + production_args + '>'
		+ inner_mathml
		+ '</math>'
	}
    }

    update_mathml_from_ascii = function(element) {
        var dialog = CKEDITOR.dialog.getCurrent();
	var mathml = AMparseMath(dialog.getValueOf('main', 'source-ascii')).innerHTML;
	dialog.setValueOf('main', 'source-mathml', clean_mathml(mathml, "", true));
    }


    ck_element(dialog, 'source-mathml').setup = function(element) {
        this.setValue(clean_mathml(element.$.innerHTML, "", true));
    }

    ck_element(dialog, 'source-mathml').commit = function(element) {
        var dialog = CKEDITOR.dialog.getCurrent();
	ascii_source = dialog.getValueOf('main', 'source-ascii');
     	element.$.innerHTML = clean_mathml(this.getValue(), ascii_source, false);
    }

    ck_element(dialog, 'source-mathml').onChange = function(element) {
	$('math-preview').innerHTML = clean_mathml(this.getValue(), "", false);
    }

    ck_element(dialog, 'source-ascii').setup = function(element) {
	annotation = ck_get_dom_subelement(element, ['math', 'semantics', 'annotation']);
	if (annotation){
	    this.setValue(annotation.$.textContent);
	}
    }

    ck_element(dialog, 'source-ascii').onShow = function(element) {
	if (this.getValue())
            update_mathml_from_ascii(element);
    }

    ck_element(dialog, 'source-ascii').onKeyUp = function(element) {
        update_mathml_from_ascii(element);
    }

    ck_element(dialog, 'toggle-input').onClick = function(element) {
	var dialog = CKEDITOR.dialog.getCurrent();
        var mathml_box = dialog.getContentElement('main',  'source-mathml-box');
        var ascii = dialog.getContentElement('main',  'source-ascii');

	if (mathml_box.getElement().isVisible()){
	    switch_method('ascii');
	}else{
	    if (ascii.getValue()){
		var answer = confirm(pytis._("Editing MathML will destroy your ASCII formula. Continue?"));
		if (answer == false)
		    return;
		ascii.setValue("");
	    }
	    switch_method('mathml');
	}
    }

    return dialog;

};
