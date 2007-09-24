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

cvs-install: compile translations link-lib link-share

link-lib:
	@if [ -d $(LIB)/pytis ]; then echo "$(LIB)/pytis already exists!"; \
	else echo "Linking Pytis libraries to $(LIB)/pytis"; \
	ln -s $(CURDIR)/lib/pytis $(LIB)/pytis; fi

link-share: link-share-translations

link-share-%: $(SHARE)/pytis
	@if [ -d $(SHARE)/pytis/$* ]; then echo "$(SHARE)/pytis/$* already exists!"; \
	else echo "Linking $* to $(SHARE)/pytis"; ln -s $(CURDIR)/$* $(SHARE)/pytis; fi

cvs-update: do-cvs-update compile translations

do-cvs-update:
	cvs update -dP

$(SHARE)/pytis:
	mkdir $(SHARE)/pytis

compile:
	@echo "Compiling Python libraries from source..."
	@python -c "import compileall; compileall.compile_dir('lib')" >/dev/null

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