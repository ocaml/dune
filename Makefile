# make all: compile to bytecode
# make opt: compile to native code
# make install: install bytecode and/or native code
#
# See Makefile.config for configurable variables.
# Runtime configurations might also be necessary in the site-lib/*/META
# files.
#----------------------------------------------------------------------

# To package findlib:
PREFIX =

NAME = findlib

OCAMLC = ocamlc
OCAMLOPT = ocamlopt
OCAMLDEP = ocamldep
OCAMLLEX = ocamllex
CAMLP4O =  camlp4 pa_o.cmo pa_op.cmo pr_o.cmo --
#CAMLP4O =  camlp4 pa_o.cmo pa_op.cmo pr_dump.cmo --


OBJECTS        = findlib_config.cmo fl_split.cmo fl_metatoken.cmo fl_meta.cmo \
		 fl_metascanner.cmo fl_topo.cmo
OBJECTS_NONMT  = fl_metacache.cmo findlib.cmo
OBJECTS_MT     = fl_metacache.cmo fl_metacache_mt.cmo mt/findlib.cmo
OBJECTS_UNIX   = fl_metacache_unix.cmo findlib_guess.cmo
TOBJECTS       = topfind.cmo

XOBJECTS       = $(OBJECTS:.cmo=.cmx)
XOBJECTS_NONMT = $(OBJECTS_NONMT:.cmo=.cmx)
XOBJECTS_MT    = $(OBJECTS_MT:.cmo=.cmx)
XOBJECTS_UNIX  = $(OBJECTS_UNIX:.cmo=.cmx)

OCAMLFIND_OBJECTS = frontend.cmo
OCAMLFIND_XOBJECTS = frontend.cmx

NUMTOP_OBJECTS = num_top_printers.cmo num_top.cmo

all: ocamlfind$(EXEC_SUFFIX) findlib.cma findlib_mt.cma findlib_top.cma findlib.conf topfind mk_toolbox num_top.cma

opt: ocamlfind_opt$(EXEC_SUFFIX) findlib.cmxa findlib.conf topfind
	if [ "$(OCAML_THREADS)" = "posix" ]; then  \
		$(MAKE) findlib_mt.cmxa;           \
	fi

ocamlfind$(EXEC_SUFFIX): findlib.cma findlib_unix.cma $(OCAMLFIND_OBJECTS)
	$(OCAMLC) -custom -o ocamlfind$(EXEC_SUFFIX) findlib.cma unix.cma \
	          findlib_unix.cma $(OCAMLFIND_OBJECTS)

ocamlfind_opt$(EXEC_SUFFIX): findlib.cmxa findlib_unix.cmxa $(OCAMLFIND_XOBJECTS)
	$(OCAMLOPT) -o ocamlfind_opt$(EXEC_SUFFIX) findlib.cmxa unix.cmxa \
		  findlib_unix.cmxa $(OCAMLFIND_XOBJECTS)

findlib.cma: $(OBJECTS) $(OBJECTS_NONMT)
	$(OCAMLC) -a -o findlib.cma $(OBJECTS) $(OBJECTS_NONMT)

findlib_mt.cma: $(OBJECTS) $(OBJECTS_MT)
	$(OCAMLC) -a -o findlib_mt.cma $(OBJECTS) $(OBJECTS_MT)

findlib_unix.cma: $(OBJECTS_UNIX)
	$(OCAMLC) -a -o findlib_unix.cma $(OBJECTS_UNIX)

findlib_top.cma: $(TOBJECTS)
	$(OCAMLC) -a -o findlib_top.cma $(TOBJECTS)

findlib.cmxa: $(XOBJECTS) $(XOBJECTS_NONMT)
	$(OCAMLOPT) -a -o findlib.cmxa $(XOBJECTS) $(XOBJECTS_NONMT)

findlib_mt.cmxa: $(XOBJECTS) $(XOBJECTS_MT)
	$(OCAMLOPT) -a -o findlib_mt.cmxa $(XOBJECTS) $(XOBJECTS_MT)

findlib_unix.cmxa: $(XOBJECTS_UNIX)
	$(OCAMLOPT) -a -o findlib_unix.cmxa $(XOBJECTS_UNIX)

findlib_config.ml: findlib_config.mlp Makefile.config
	sed -e 's;@CONFIGFILE@;$(OCAMLFIND_CONF);g' \
	    -e 's;@STDLIB@;$(OCAML_CORE_STDLIB);g' \
	    -e 's;@AUTOLINK@;$(OCAML_AUTOLINK);g' \
	    findlib_config.mlp >findlib_config.ml

findlib.conf: findlib.conf.in Makefile.config
	sed -e 's;@SITELIB@;$(OCAML_SITELIB);g' \
	    findlib.conf.in >findlib.conf

topfind: topfind.p
	sed -e 's;@SITELIB@;$(OCAML_SITELIB);g' \
	    topfind.p >topfind

findlib.ml: findlib.mlp
	echo "(* THIS FILE IS GENERATED! DO NOT CHANGE! *)" >findlib.ml
	sed -e 's:@METACACHE@:Fl_metacache:g' \
	    findlib.mlp >>findlib.ml

mt/findlib.mml: findlib.mlp
	mkdir -p mt
	echo "(* THIS FILE IS GENERATED! DO NOT CHANGE! *)" >mt/findlib.mml
	sed -e 's:@METACACHE@:Fl_metacache_mt:g' \
	    findlib.mlp >>mt/findlib.mml

mt/findlib.cmo: findlib.cmo fl_metacache_mt.cmo mt/findlib.cmi
mt/findlib.cmx: findlib.cmx fl_metacache_mt.cmx mt/findlib.cmi
fl_metacache_mt.cmo: fl_metacache.cmo
fl_metacache_mt.cmx: fl_metacache.cmx

mt/findlib.cmi: findlib.cmi
	mkdir -p mt
	cp findlib.cmi findlib.mli mt

mk_toolbox:
	if [ "$(MAKE_TOOLBOX)" = "yes" ]; then      \
		( cd toolbox; make );               \
	fi

# num_top.cma: This archive really includes the whole nums.cma.
# This simplifies package making.

num_top.cma: $(NUMTOP_OBJECTS)
	$(OCAMLC) -a -o num_top.cma $(NUMTOP_OBJECTS)

clean:
	rm -f *.cmi *.cmo *.cma *.cmx *.a *.o *.cmxa findlib.ml findlib_mt.mml \
	  mt/*.* fl_meta.ml findlib_config.ml findlib.mml topfind \
	  ocamlfind$(EXEC_SUFFIX) ocamlfind_opt$(EXEC_SUFFIX)
	cd toolbox && make clean


mrproper: clean
	rm -f fl_metascanner.ml

# distclean: don't remove fl_metascanner.ml; it is distributed

distclean: clean
	rm -f *~ mini/*~ findlib.conf
	cd config; rm -f *.cmi *.cmo *.cma *.cmx *.a *.o *.cmxa *~ testdb.* simple err.out makehelper withtk
	cd scripts; rm -f *~
	cd site-lib; for x in `ls`; do rm -f $$x/*~ $$x/META; done
	$(MAKE) -C doc distclean

RELEASE: META
	awk '/version/ { print substr($$3,2,length($$3)-2) }' META >RELEASE

dist: RELEASE
	test -f fl_metascanner.ml
	./release

tag-release: RELEASE
	r=`head -1 RELEASE | sed -e s/\\\./-/g`; cd ..; cvs tag -F $(NAME)-$$r $(NAME)

.PHONY: release
release:
	$(MAKE) tag-release
	$(MAKE) dist


.PHONY: interface-lists
interface-lists:
	d=`ocamlc -where`;                              \
	for x in `ls site-lib`; do                      \
	    iflist="";                                  \
            if [ ! -f "site-lib/$$x/interfaces.in" ]; then continue; fi; \
	    cma_spec=`cat site-lib/$$x/interfaces.in`;  \
	    for cma in $$d/$$cma_spec; do               \
		intf=`objinfo $$cma |                   \
		      grep 'Unit name:' |               \
		      sed -e 's/^  Unit name: //' |     \
		      sort |                            \
		      tr '\n' ' '`;                     \
		iflist="$$iflist $$intf";               \
	    done;                                       \
	    echo "$$iflist" >"site-lib/$$x/interfaces.out"; \
	done

install: all
	mkdir -p $(PREFIX)$(OCAML_SITELIB)/$(NAME)
	mkdir -p $(PREFIX)$(OCAMLFIND_BIN)
	cp topfind $(PREFIX)$(OCAML_CORE_STDLIB)
	./file_exists $(PREFIX)$(OCAML_CORE_STDLIB)/findlib || ln -s topfind $(PREFIX)$(OCAML_CORE_STDLIB)/findlib # compat
	./file_exists $(PREFIX)$(OCAML_CORE_STDLIB)/ocamlfind || ln -s topfind $(PREFIX)$(OCAML_CORE_STDLIB)/ocamlfind # compat
	files=`./collect_files Makefile.config findlib.cmi findlib.mli findlib.cma findlib_mt.cma findlib_unix.cma topfind.cmi topfind.mli findlib_top.cma findlib.cmxa findlib.a findlib_mt.cmxa findlib_mt.a findlib_unix.cmxa findlib_unix.a META toolbox/make_wizard$(EXEC_SUFFIX) toolbox/make_wizard.pattern` && \
	cp $$files $(PREFIX)$(OCAML_SITELIB)/$(NAME)
	f="ocamlfind$(EXEC_SUFFIX)"; { test -f ocamlfind_opt$(EXEC_SUFFIX) && f="ocamlfind_opt$(EXEC_SUFFIX)"; }; \
	cp $$f $(PREFIX)$(OCAMLFIND_BIN)/ocamlfind$(EXEC_SUFFIX)
	mkdir -p $(PREFIX)$(OCAMLFIND_MAN)/man1 $(PREFIX)$(OCAMLFIND_MAN)/man3 $(PREFIX)$(OCAMLFIND_MAN)/man5
	cp doc/man/ocamlfind.1 $(PREFIX)$(OCAMLFIND_MAN)/man1
	cp doc/man/Findlib.3 doc/man/Topfind.3 $(PREFIX)$(OCAMLFIND_MAN)/man3
	cp doc/man/META.5 doc/man/site-lib.5 doc/man/findlib.conf.5 $(PREFIX)$(OCAMLFIND_MAN)/man5
	$(MAKE) install-meta
	mkdir -p `dirname $(PREFIX)$(OCAMLFIND_CONF)`
	@if [ -f $(PREFIX)$(OCAMLFIND_CONF) ]; then echo "!!! Keeping old $(PREFIX)$(OCAMLFIND_CONF) !!!"; fi
	test -f $(PREFIX)$(OCAMLFIND_CONF) || cp findlib.conf $(PREFIX)$(OCAMLFIND_CONF)

install-meta:
	for x in `ls site-lib`; do if [ "$$x" != "CVS" -a -f "site-lib/$$x/META" ]; then mkdir -p $(PREFIX)$(OCAML_SITELIB)/$$x; cp site-lib/$$x/META $(PREFIX)$(OCAML_SITELIB)/$$x; fi; done
	cp num_top.cma $(PREFIX)$(OCAML_SITELIB)/num-top

uninstall:
	$(MAKE) uninstall-meta
	rm -f $(PREFIX)$(OCAML_CORE_STDLIB)/findlib
	rm -rf $(PREFIX)$(OCAML_SITELIB)/$(NAME)
	rm -f $(PREFIX)$(OCAMLFIND_BIN)/ocamlfind$(EXEC_SUFFIX)
	rm -f $(PREFIX)$(OCAMLFIND_MAN)/man1/ocamlfind.1
	rm -f $(PREFIX)$(OCAMLFIND_MAN)/man3/Findlib.3
	rm -f $(PREFIX)$(OCAMLFIND_MAN)/man3/Topfind.3
	rm -f $(PREFIX)$(OCAMLFIND_MAN)/man5/META.5
	rm -f $(PREFIX)$(OCAMLFIND_MAN)/man5/site-lib.5

uninstall-meta:
	for x in `ls site-lib`; do if [ "$$x" != "CVS" ]; then rm -rf $(PREFIX)$(OCAML_SITELIB)/$$x; fi; done


depend: *.ml *.mli findlib.ml fl_meta.ml fl_metascanner.ml
	$(OCAMLDEP) *.ml *.mli >depend

Makefile.config:
	./configure

# Some 'make' implementations require that .SUFFIXES must occur before
# the first suffix rule. (E.g. AIX)
.SUFFIXES: .mll .cmo .cmi .cmx .ml .mli .mml .src

.mml.cmo:
	$(OCAMLC) -g -vmthread -c -impl $<

.mml.cmx:
	$(OCAMLOPT) -thread -c -impl $<

.ml.cmx:
	$(OCAMLOPT) -c $<

.ml.cmo:
	$(OCAMLC) -g -c $<

.mli.cmi:
	$(OCAMLC) -c $<

.src.ml:
	$(CAMLP4O) -impl $< -o $@

# Solaris make does not like the suffix rule .mll.ml,
# so I replaced it by its single application:
fl_meta.ml: fl_meta.mll
	$(OCAMLLEX) fl_meta.mll

# Don't remove fl_metascanner.ml:
.PRECIOUS: fl_metascanner.ml

include Makefile.config
include depend

