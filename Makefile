.PHONY: all update assets sync-assets javascript translations extract doc test build install clean coverage

js_src := $(wildcard javascript/*.js)
js_out := $(js_src:javascript/%.js=pytis/assets/resources/scripts/%.js)

all: doc update

update: translations assets

assets: sync-assets javascript

sync-assets:
	git ls-files resources icons help | rsync -a --info=name --delete --files-from=- ./ pytis/assets/

javascript: $(js_out)

pytis/assets/resources/scripts/%.js: javascript/%.js
	mkdir -p $(@D)
	python3 -m rjsmin < $< > $@

translations:
	make -C translations

extract:
	make -C translations extract

doc:
	python -m lcg.make doc/tutorials/Fields.txt doc/html

test:
	python -m pytest doc pytis -v

build: update
	flit build

install:
	# Only for development installs.  Use pip for production/user installs.
	flit install --symlink

clean:
	rm -rf dist pytis/assets doc/html
	make -C translations clean

coverage:
	coverage run --source=pytis -m pytest doc pytis
	coverage report
