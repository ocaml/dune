BYTE_FILES=bytes.cmi bytes.cma
NATIVE_FILES=bytes.cmx bytes.cmxa bytes.cmxs
OCAMLBUILD=ocamlbuild -classic-display -no-links

build: all opt

all:
	$(OCAMLBUILD) $(BYTE_FILES)

opt:
	$(OCAMLBUILD) $(NATIVE_FILES)

install: byte
	cd _build/ && ocamlfind install bytes ../META $(BYTE_FILES) -optional $(NATIVE_FILES)

uninstall:
	ocamlfind remove bytes

clean:
	ocamlbuild -clean
