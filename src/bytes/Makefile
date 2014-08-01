BYTE_FILES=bytes.cmi bytes.cma
NATIVE_FILES=bytes.cmx bytes.a bytes.cmxa
NATIVE_FILES_DYNLINK=bytes.cmxs
OCAMLBUILD=ocamlbuild -classic-display -no-links

TOP=../..
include $(TOP)/Makefile.config

build: all opt

all:
	$(OCAMLBUILD) $(BYTE_FILES)

opt:
	files="$(NATIVE_FILES)"; \
	if [ $(HAVE_NATDYNLINK) = 1 ]; then \
	    files="$$files $(NATIVE_FILES_DYNLINK)"; \
	fi; \
	$(OCAMLBUILD) $$files

install: all
	cd _build/ && ocamlfind install bytes ../META $(BYTE_FILES) -optional $(NATIVE_FILES) $(NATIVE_FILES_DYNLINK)

uninstall:
	ocamlfind remove bytes

clean:
	ocamlbuild -clean
