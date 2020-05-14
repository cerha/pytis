# This Makefile is created according to the LCG's Makefile for translations.
# See the original for comments...

mo_files := $(foreach d, pytis-data pytis-defs pytis-web pytis-wx pytis-demo, \
	      $(patsubst $(d).%.po,%/LC_MESSAGES/$(d).mo,$(wildcard $(d).*.po)))
resource_po_files := $(patsubst %.po,../resources/translations/%.po, $(wildcard pytis-web.*.po))

all: $(mo_files) $(resource_po_files)
extract: $(wildcard *.po) $(mo_files)

ifeq ($(MAKECMDGOALS),extract)

pytis-data.pot: $(foreach dir, data presentation, $(shell find ../lib/pytis/$(dir) -name "*.py"))
pytis-defs.pot: $(foreach dir, defs cms, $(shell find ../lib/pytis/$(dir) -name "*.py"))
pytis-web.pot:  $(shell find ../lib/pytis/web -name "*.py") \
		$(shell find ../resources/scripts -name "*.js")
pytis-wx.pot:   $(foreach dir, form util extensions output help remote, \
	   	  $(shell find ../lib/pytis/$(dir) -name "*.py"))
pytis-demo.pot: $(shell find ../lib/pytis/demo -name "*.py")
$(wildcard *.pot):
	pybabel extract -F babel.cfg --add-comments=Translators: -s $(sort $(dir $^)) -o $@

$(wildcard pytis-data.*.po): pytis-data.%.po: pytis-data.pot
$(wildcard pytis-defs.*.po): pytis-defs.%.po: pytis-defs.pot
$(wildcard pytis-web.*.po): pytis-web.%.po: pytis-web.pot
$(wildcard pytis-wx.*.po): pytis-wx.%.po: pytis-wx.pot
$(wildcard pytis-demo.*.po): pytis-demo.%.po: pytis-demo.pot
$(wildcard *.po):
	msgmerge -q --backup=none --update $@ $< && touch $@

endif

%/LC_MESSAGES:
	mkdir -p $*/LC_MESSAGES/
%/LC_MESSAGES/pytis-data.mo: pytis-data.%.po %/LC_MESSAGES
	msgfmt -v $< -o $@
%/LC_MESSAGES/pytis-defs.mo: pytis-defs.%.po %/LC_MESSAGES
	msgfmt -v $< -o $@
%/LC_MESSAGES/pytis-web.mo: pytis-web.%.po %/LC_MESSAGES
	msgfmt -v $< -o $@
%/LC_MESSAGES/pytis-wx.mo: pytis-wx.%.po %/LC_MESSAGES
	msgfmt -v $< -o $@
%/LC_MESSAGES/pytis-demo.mo: pytis-demo.%.po %/LC_MESSAGES
	msgfmt -v $< -o $@

../resources/translations:
	mkdir -p ../resources/translations/
../resources/translations/pytis-web.%.po: pytis-web.%.po ../resources/translations
	cp $< $@

clean:
	rm -f *.d *.pot
