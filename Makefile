.PHONY: all update resources sync-resources javascript assets translations extract doc test build install clean coverage

js_src := $(wildcard javascript/*.js)
js_out := $(js_src:javascript/%.js=pytis/resources/scripts/%.js)

all: doc update

update: translations resources assets

resources: sync-resources javascript

sync-resources:
	git ls-files resources | rsync -av --delete --files-from=- ./ pytis/

javascript: $(js_out)

pytis/resources/scripts/%.js: javascript/%.js
	mkdir -p $(@D)
	python3 -m rjsmin < $< > $@

assets:
	git ls-files icons help | rsync -av --delete --files-from=- ./ pytis/assets/

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
	rm -rf dist pytis/resources doc/html
	make -C translations clean

coverage:
	coverage run --source=pytis -m pytest doc pytis
	coverage report
