# make all: compile to bytecode
# make opt: compile to native code
# make install: install bytecode and/or native code
#
# See Makefile.config for configurable variables.
# Runtime configurations might also be necessary in the site-lib/*/META
# files.
#----------------------------------------------------------------------

TOP=../..

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

all: ocamlfind$(EXEC_SUFFIX) findlib.cma findlib_mt.cma findlib_top.cma topfind num_top.cma

opt: ocamlfind_opt$(EXEC_SUFFIX) findlib.cmxa topfind
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

findlib_config.ml: findlib_config.mlp 
	sed -e 's;@CONFIGFILE@;$(OCAMLFIND_CONF);g' \
	    -e 's;@STDLIB@;$(OCAML_CORE_STDLIB);g' \
	    -e 's;@AUTOLINK@;$(OCAML_AUTOLINK);g' \
	    findlib_config.mlp >findlib_config.ml

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

# num_top.cma: This archive really includes the whole nums.cma.
# This simplifies package making.

num_top.cma: $(NUMTOP_OBJECTS)
	$(OCAMLC) -a -o num_top.cma $(NUMTOP_OBJECTS)

clean:
	rm -f *.cmi *.cmo *.cma *.cmx *.a *.o *.cmxa findlib.ml findlib_mt.mml \
	  mt/*.* fl_meta.ml findlib_config.ml findlib.mml topfind \
	  ocamlfind$(EXEC_SUFFIX) ocamlfind_opt$(EXEC_SUFFIX)
	rm -rf mt

install: all
	mkdir -p $(PREFIX)$(OCAML_SITELIB)/$(NAME)
	mkdir -p $(PREFIX)$(OCAMLFIND_BIN)
	cp topfind $(PREFIX)$(OCAML_CORE_STDLIB)
	$(TOP)/tools/file_exists $(PREFIX)$(OCAML_CORE_STDLIB)/findlib || ln -s topfind $(PREFIX)$(OCAML_CORE_STDLIB)/findlib # compat
	$(TOP)/tools/file_exists $(PREFIX)$(OCAML_CORE_STDLIB)/ocamlfind || ln -s topfind $(PREFIX)$(OCAML_CORE_STDLIB)/ocamlfind # compat
	files=`$(TOP)/tools/collect_files $(TOP)/Makefile.config findlib.cmi findlib.mli findlib.cma findlib_mt.cma findlib_unix.cma topfind.cmi topfind.mli findlib_top.cma findlib.cmxa findlib.a findlib_mt.cmxa findlib_mt.a findlib_unix.cmxa findlib_unix.a META` && \
	cp $$files $(PREFIX)$(OCAML_SITELIB)/$(NAME)
	f="ocamlfind$(EXEC_SUFFIX)"; { test -f ocamlfind_opt$(EXEC_SUFFIX) && f="ocamlfind_opt$(EXEC_SUFFIX)"; }; \
	cp $$f $(PREFIX)$(OCAMLFIND_BIN)/ocamlfind$(EXEC_SUFFIX)

uninstall:
	rm -f $(PREFIX)$(OCAML_CORE_STDLIB)/findlib
	rm -rf $(PREFIX)$(OCAML_SITELIB)/$(NAME)
	rm -f $(PREFIX)$(OCAMLFIND_BIN)/ocamlfind$(EXEC_SUFFIX)


depend: *.ml *.mli findlib.ml fl_meta.ml fl_metascanner.ml
	$(OCAMLDEP) *.ml *.mli >depend

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

include $(TOP)/Makefile.config
include depend

