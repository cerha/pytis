# Edit the paths below to suit your needs.
LIB = /usr/local/lib/python%d.%d/site-packages
SHARE = /usr/local/share

lib := $(shell python -c 'import sys; print "$(LIB)".find("%d") != -1 and \
	                 "$(LIB)" % sys.version_info[:2] or "$(LIB)"')

.PHONY: translations

all: check compile translations

check:
	@python -c "import sys; '$(lib)' not in sys.path and sys.exit(1)" || \
           echo 'WARNING: $(lib) not in Python path!'

compile:
	@echo "Compiling Python libraries from source..."
	@python -c "import compileall; compileall.compile_dir('lib')" >/dev/null

translations:
	make -C translations

install: $(SHARE)/pytis
	cp -ruv resources translations $(SHARE)/pytis
	cp -ruv lib/pytis $(lib)

uninstall:
	rm -rf $(SHARE)/pytis
	rm -rf $(lib)/pytis

install-links: check compile translations link-lib link-share

tags:
	./tools/make-tags.sh

link-lib:
	@if [ -d $(lib)/pytis ]; then echo "$(lib)/pytis already exists!"; \
	else echo "Linking Pytis libraries to $(lib)/pytis"; \
	ln -s $(CURDIR)/lib/pytis $(lib)/pytis; fi

link-share: link-share-translations link-share-resources

link-share-%: $(SHARE)/pytis
	@if [ -d $(SHARE)/pytis/$* ]; then echo "$(SHARE)/pytis/$* already exists!"; \
	else echo "Linking $* to $(SHARE)/pytis"; ln -s $(CURDIR)/$* $(SHARE)/pytis; fi

$(SHARE)/pytis:
	mkdir $(SHARE)/pytis

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