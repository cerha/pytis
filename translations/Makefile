mo_files := $(patsubst pytis-data.%.po,%/LC_MESSAGES/pytis-data.mo,$(shell ls pytis-data.*.po)) $(patsubst pytis-web.%.po,%/LC_MESSAGES/pytis-web.mo,$(shell ls pytis-web.*.po)) $(patsubst pytis-wx.%.po,%/LC_MESSAGES/pytis-wx.mo,$(shell ls pytis-wx.*.po)) $(patsubst pytis-defs.%.po,%/LC_MESSAGES/pytis-defs.mo,$(shell ls pytis-defs.*.po))
resource_po_files := $(patsubst pytis-web.%.po,../resources/translations/pytis-web.%.po,$(shell ls *.po))

all: $(mo_files) $(resource_po_files)

%/LC_MESSAGES:
	mkdir -p $*/LC_MESSAGES/

../resources/translations:
	mkdir -p ../resources/translations/

%/LC_MESSAGES/pytis-data.mo: pytis-data.%.po %/LC_MESSAGES
	msgfmt -v $< -o $@

%/LC_MESSAGES/pytis-web.mo: pytis-web.%.po %/LC_MESSAGES
	msgfmt -v $< -o $@

%/LC_MESSAGES/pytis-wx.mo: pytis-wx.%.po %/LC_MESSAGES
	msgfmt -v $< -o $@

%/LC_MESSAGES/pytis-defs.mo: pytis-defs.%.po %/LC_MESSAGES
	msgfmt -v $< -o $@

../resources/translations/pytis-web.%.po: pytis-web.%.po ../resources/translations
	cp $< $@

pytis-data.pot: $(shell find ../lib/pytis/data -name "*.py")
	pybabel extract -F babel.cfg --add-comments=Translators: -s ../lib/pytis/data/ -o pytis-data.pot

pytis-web.pot: $(shell find ../lib/pytis/web -name "*.py") $(shell find ../resources/scripts -name "*.js")
	pybabel extract -F babel.cfg --add-comments=Translators: -s ../lib/pytis/web/ ../resources/scripts/ -o pytis-web.pot

pytis-wx.pot: $(shell find ../lib/pytis/form -name "*.py") $(shell find ../lib/pytis/util -name "*.py") $(shell find ../lib/pytis/extensions -name "*.py")  $(shell find ../lib/pytis/output -name "*.py") $(shell find ../lib/pytis/help -name "*.py")  $(shell find ../lib/pytis/windows -name "*.py")
	pybabel extract -F babel.cfg --add-comments=Translators: -s ../lib/pytis/form ../lib/pytis/util ../lib/pytis/extensions ../lib/pytis/output ../lib/pytis/help ../lib/pytis/windows -o pytis-wx.pot

pytis-defs.pot: $(shell find ../lib/pytis/defs -name "*.py")
	pybabel extract -F babel.cfg --add-comments=Translators: -s ../lib/pytis/defs -o pytis-defs.pot

$(shell ls pytis-data.*.po): pytis-data.%.po: pytis-data.pot
	msgmerge -q --backup=none --update $@ $< && touch $@
$(shell ls pytis-web.*.po): pytis-web.%.po: pytis-web.pot
	msgmerge -q --backup=none --update $@ $< && touch $@
$(shell ls pytis-wx.*.po): pytis-wx.%.po: pytis-wx.pot
	msgmerge -q --backup=none --update $@ $< && touch $@
$(shell ls pytis-defs.*.po): pytis-defs.%.po: pytis-defs.pot
	msgmerge -q --backup=none --update $@ $< && touch $@

clean:
	rm -f *.d *.pot
