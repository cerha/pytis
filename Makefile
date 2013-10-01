.PHONY: translations

all: compile translations

compile:
	@echo "Compiling Python libraries from source..."
	@python -c "import compileall; compileall.compile_dir('lib')" >/dev/null
	@python -O -c "import compileall; compileall.compile_dir('lib')" >/dev/null

translations:
	make -C translations
