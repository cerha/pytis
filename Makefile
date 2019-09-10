.PHONY: translations doc test

all: compile translations

doc:
	LCGDIR=../lcg lcgmake doc/tutorials/Fields.txt doc/html

compile:
	@echo "Compiling Python libraries from source..."
	@python -c "import compileall; compileall.compile_dir('lib')" >/dev/null
	@python -O -c "import compileall; compileall.compile_dir('lib')" >/dev/null

translations:
	make -C translations
extract:
	make -C translations extract

test:
	pytest doc lib

coverage:
	PYTHONPATH="./lib:${PYTHONPATH}" coverage run --source lib/pytis -m pytest doc lib
	coverage report
