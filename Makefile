LIB = /usr/local/lib/python2.4/site-packages
SHARE = /usr/local/share

.PHONY: translations

translations:
	make -C translations

install: $(SHARE)/pytis
	cp -ruv translations $(SHARE)/pytis
	cp -ruv lib/pytis $(LIB)

uninstall:
	rm -rf $(SHARE)/pytis
	rm -rf $(LIB)/pytis

cvs-install: compile translations $(SHARE)/pytis
	ln -s $(CURDIR)/lib/pytis $(LIB)/pytis
	ln -s $(CURDIR)/translations $(SHARE)/pytis

cvs-update: do-cvs-update compile translations

do-cvs-update:
	cvs update

$(SHARE)/pytis:
	mkdir $(SHARE)/pytis

compile:
	python -c "import compileall; compileall.compile_dir('lib')"

tags:
	./tools/make-tags.sh

version = $(shell echo 'import pytis; print pytis.__version__' | python)
dir = pytis-$(version)
file = pytis-$(version).tar.gz

release: compile translations
	@ln -s .. releases/$(dir)
	@if [ -e releases/$(file) ]; then \
	   echo "Removing old file $(file)"; rm releases/$(file); fi
	@echo "Generating $(file)..."
	@(cd releases; tar --exclude "CVS" --exclude "*~" --exclude "#*" \
	     --exclude ".#*" --exclude "*.pyo" \
	     --exclude demo --exclude releases --exclude extensions \
	     -czhf $(file) $(dir))
	@rm releases/$(dir)