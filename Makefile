.PHONY: doc test translations resources assets

all: doc compile update

update: translations resources assets

doc:
	python -m lcg.make doc/tutorials/Fields.txt doc/html

compile:
	python -m compileall -d . pytis
	python -O -m compileall -d . pytis

translations:
	make -C translations

extract:
	make -C translations extract

resources:
	git ls-files resources | rsync -av --delete --files-from=- ./ pytis/

assets:
	git ls-files icons help | rsync -av --delete --files-from=- ./ pytis/assets/

test:
	python -m pytest doc pytis -v

build: translations resources assets
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
