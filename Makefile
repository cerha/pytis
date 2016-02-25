.PHONY: translations doc

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
