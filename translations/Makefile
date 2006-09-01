mo_files := $(patsubst pytis.%.po,%/LC_MESSAGES/pytis.mo,$(shell ls *.po))

all: $(mo_files)

%/LC_MESSAGES/pytis.mo: pytis.%.po %/LC_MESSAGES
	msgfmt -v $< -o $@

%/LC_MESSAGES:
	mkdir -p $*/LC_MESSAGES/

$(shell ls *.po): pytis.%.po: pytis.pot
	msgmerge -q --update $@ $< && touch $@

pytis.pot: $(shell find ../lib/pytis/web -name "*.py")
	xgettext $^ -o $@
