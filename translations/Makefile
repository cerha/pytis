# This Makefile is created according to the LCG's Makefile for translations.
# See the original for comments...

mo_files := $(foreach d, pytis-data pytis-defs pytis-web pytis-wx pytis-x2go pytis-x2goclient, \
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
pytis-x2go.pot: $(shell find ../lib/pytis/x2goclient -name "*.py")
pytis-x2goclient.pot: ../bin/x2goclient.py
$(wildcard *.pot):
	pybabel extract -F babel.cfg --add-comments=Translators: -s $(sort $(dir $^)) -o $@

$(wildcard pytis-data.*.po): pytis-data.%.po: pytis-data.pot
$(wildcard pytis-defs.*.po): pytis-defs.%.po: pytis-defs.pot
$(wildcard pytis-web.*.po): pytis-web.%.po: pytis-web.pot
$(wildcard pytis-wx.*.po): pytis-wx.%.po: pytis-wx.pot
$(wildcard pytis-x2go.*.po): pytis-x2go.%.po: pytis-x2go.pot
$(wildcard pytis-x2goclient.*.po): pytis-x2goclient.%.po: pytis-x2goclient.pot
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
%/LC_MESSAGES/pytis-x2go.mo: pytis-x2go.%.po %/LC_MESSAGES
	msgfmt -v $< -o $@
%/LC_MESSAGES/pytis-x2goclient.mo: pytis-x2goclient.%.po %/LC_MESSAGES
	msgfmt -v $< -o $@

../resources/translations:
	mkdir -p ../resources/translations/
../resources/translations/pytis-web.%.po: pytis-web.%.po ../resources/translations
	cp $< $@

clean:
	rm -f *.d *.pot
