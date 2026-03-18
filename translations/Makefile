# This Makefile is created according to the LCG's Makefile for translations.
# See the original for comments...  The logic here slightly differs because we
# don't have separate .cfg file for each domain, so we define the source file
# dependencies directly in the Makefile using find.

domains := pytis-data pytis-defs pytis-web pytis-wx pytis-demo
web_domains := pytis-data pytis-web
mo_dir := ../pytis/translations
json_dir := ../pytis/resources/translations
mo_files := $(foreach d, $(domains), $(patsubst $(d).%.po,\
                                      $(mo_dir)/%/LC_MESSAGES/$(d).mo,\
                                      $(wildcard $(d).*.po)))
pot_files := $(patsubst %,%.pot,$(domains))
json_files := $(foreach d, $(web_domains), $(patsubst $(d).%.po,\
                                           $(json_dir)/$(d).%.po.json,\
                                           $(wildcard $(d).*.po)))

all: $(mo_files) $(json_files)
extract: $(wildcard *.po) $(pot_files) $(mo_files) $(json_files)

ifeq ($(MAKECMDGOALS),extract)

pytis-data.pot: $(foreach dir, data presentation, $(shell find ../pytis/$(dir) -name "*.py"))
pytis-defs.pot: $(foreach dir, defs cms, $(shell find ../pytis/$(dir) -name "*.py"))
pytis-web.pot:  $(shell find ../pytis/web -name "*.py") \
		$(shell find ../javascript -name "*.js")
pytis-wx.pot:   $(foreach dir, form util extensions output help remote, \
	   	  $(shell find ../pytis/$(dir) -name "*.py"))
pytis-demo.pot: $(shell find ../pytis/demo -name "*.py")
$(pot_files):
	pybabel extract -F babel.cfg --add-comments=Translators: -s $(sort $(dir $^)) -o $@

$(wildcard pytis-data.*.po): pytis-data.%.po: pytis-data.pot
$(wildcard pytis-defs.*.po): pytis-defs.%.po: pytis-defs.pot
$(wildcard pytis-web.*.po): pytis-web.%.po: pytis-web.pot
$(wildcard pytis-wx.*.po): pytis-wx.%.po: pytis-wx.pot
$(wildcard pytis-demo.*.po): pytis-demo.%.po: pytis-demo.pot
$(wildcard *.po):
	msgmerge -q --backup=none --update $@ $< && touch $@

endif

$(mo_dir)/%/LC_MESSAGES/pytis-data.mo: pytis-data.%.po
	@mkdir -p $(dir $@)
	msgfmt -v $< -o $@
$(mo_dir)/%/LC_MESSAGES/pytis-defs.mo: pytis-defs.%.po
	@mkdir -p $(dir $@)
	msgfmt -v $< -o $@
$(mo_dir)/%/LC_MESSAGES/pytis-web.mo: pytis-web.%.po
	@mkdir -p $(dir $@)
	msgfmt -v $< -o $@
$(mo_dir)/%/LC_MESSAGES/pytis-wx.mo: pytis-wx.%.po
	@mkdir -p $(dir $@)
	msgfmt -v $< -o $@
$(mo_dir)/%/LC_MESSAGES/pytis-demo.mo: pytis-demo.%.po
	@mkdir -p $(dir $@)
	msgfmt -v $< -o $@

$(json_dir)/%.po.json: %.po
	@mkdir -p $(dir $@)
	pojson $< > $@

clean:
	rm -rf *.pot $(mo_dir)

.PHONY: all extract clean
