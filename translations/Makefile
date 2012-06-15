mo_files := $(patsubst pytis.%.po,%/LC_MESSAGES/pytis.mo,$(shell ls *.po))
resource_po_files := $(patsubst pytis.%.po,../resources/translations/pytis.%.po,$(shell ls *.po))

all: $(mo_files) $(resource_po_files)

%/LC_MESSAGES:
	mkdir -p $*/LC_MESSAGES/

%/LC_MESSAGES/pytis.mo: pytis.%.po %/LC_MESSAGES
	msgfmt -v $< -o $@

../resources/translations:
	mkdir -p ../resources/translations/

../resources/translations/pytis.%.po: pytis.%.po ../resources/translations
	cp $< $@

pytis.pot: $(shell find ../lib/pytis/web -name "*.py") $(shell find ../resources/scripts -name "*.js")
	pybabel extract -F babel.cfg --add-comments=Translators: -s ../lib/pytis/web/ ../resources/scripts/ -o pytis.pot

$(shell ls *.po): pytis.%.po: pytis.pot
	msgmerge -q --backup=none --update $@ $< && touch $@

clean:
	rm -f *.d *.pot
