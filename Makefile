# make all: compile to bytecode
# make opt: compile to native code
# make install: install bytecode and/or native code
#----------------------------------------------------------------------

include Makefile.config

TOP=.

.PHONY: all opt install uninstall clean

all:
	for p in $(PARTS); do ( cd src/$$p; $(MAKE) all ); done
	$(MAKE) all-config

opt:
	for p in $(PARTS); do ( cd src/$$p; $(MAKE) opt ); done

install:
	for p in $(PARTS); do ( cd src/$$p; $(MAKE) install ); done
	$(MAKE) install-doc
	$(MAKE) install-meta
	cd src/findlib; $(MAKE) install-num-top
	$(MAKE) install-config

uninstall:
	$(MAKE) uninstall-doc
	$(MAKE) uninstall-meta
	for p in `cd src; echo *`; do ( cd src/$$p; $(MAKE) uninstall ); done
	$(MAKE) uninstall-config

clean:
	for p in `cd src; echo *`; do ( cd src/$$p; $(MAKE) clean ); done
	(cd itest-aux; $(MAKE) clean)
	rm -f findlib.conf

.PHONY: release
release: README
	XXX

README: doc/README
	ln -s doc/README .


.PHONY: all-config
all-config: findlib.conf

findlib.conf: findlib.conf.in 
	sed -e 's;@SITELIB@;$(OCAML_SITELIB);g' \
	    findlib.conf.in >findlib.conf

.PHONY: install-doc
install-doc:
	mkdir -p $(PREFIX)$(OCAMLFIND_MAN)/man1 $(PREFIX)$(OCAMLFIND_MAN)/man3 $(PREFIX)$(OCAMLFIND_MAN)/man5
	cp doc/ref-man/ocamlfind.1 $(PREFIX)$(OCAMLFIND_MAN)/man1
	cp doc/ref-man/Findlib.3 doc/ref-man/Topfind.3 $(PREFIX)$(OCAMLFIND_MAN)/man3
	cp doc/ref-man/META.5 doc/ref-man/site-lib.5 doc/ref-man/findlib.conf.5 $(PREFIX)$(OCAMLFIND_MAN)/man5

.PHONY: uninstall-doc
uninstall-doc:
	rm -f $(PREFIX)$(OCAMLFIND_MAN)/man1/ocamlfind.1
	rm -f $(PREFIX)$(OCAMLFIND_MAN)/man3/Findlib.3
	rm -f $(PREFIX)$(OCAMLFIND_MAN)/man3/Topfind.3
	rm -f $(PREFIX)$(OCAMLFIND_MAN)/man5/META.5
	rm -f $(PREFIX)$(OCAMLFIND_MAN)/man5/site-lib.5


.PHONY: install-meta
install-meta:
	for x in `ls site-lib-src`; do if [ "$$x" != "CVS" -a -f "site-lib-src/$$x/META" ]; then mkdir -p $(PREFIX)$(OCAML_SITELIB)/$$x; cp site-lib-src/$$x/META $(PREFIX)$(OCAML_SITELIB)/$$x; fi; done

.PHONY: uninstall-meta
uninstall-meta:
	for x in `ls site-lib-src`; do if [ "$$x" != "CVS" ]; then rm -rf $(PREFIX)$(OCAML_SITELIB)/$$x; fi; done

.PHONY: install-config
install-config:
	mkdir -p `dirname $(PREFIX)$(OCAMLFIND_CONF)`
	@if [ -f $(PREFIX)$(OCAMLFIND_CONF) ]; then echo "!!! Keeping old $(PREFIX)$(OCAMLFIND_CONF) !!!"; fi
	test -f $(PREFIX)$(OCAMLFIND_CONF) || cp findlib.conf $(PREFIX)$(OCAMLFIND_CONF)

.PHONY: uninstall-config
uninstall-config:
	@echo Leaving $(OCAMLFIND_CONF) installed, consider manual removal

.PHONY: interface-lists
interface-lists:
	d=`ocamlc -where`;                              \
	for x in `ls site-lib-src`; do                  \
	    iflist="";                                  \
            if [ ! -f "site-lib-src/$$x/interfaces.in" ]; then continue; fi; \
	    cma_spec=`cat site-lib-src/$$x/interfaces.in`;  \
	    for cma in $$d/$$cma_spec; do               \
		intf=`objinfo $$cma |                   \
		      grep 'Unit name:' |               \
		      sed -e 's/^  Unit name: //' |     \
		      sort |                            \
		      tr '\n' ' '`;                     \
		iflist="$$iflist $$intf";               \
	    done;                                       \
	    echo "$$iflist" >"site-lib-src/$$x/interfaces.out"; \
	done

