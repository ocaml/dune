all: opam-file-format.cmxa opam-file-format.cma

%.ml: %.mll
	ocamllex $<

%.ml %.mli: %.mly
	ocamlyacc $<

%.cmi: %.mli
	ocamlc -c $<

%.cmo: %.ml %.cmi
	ocamlc -c $<

%.cmx: %.ml %.cmi
	ocamlopt -c $<

MODULES = $(sort $(basename $(wildcard *.ml *.mll *.mly)))

GENERATED = $(patsubst %.mly,%.ml,$(wildcard *.mly)) \
            $(patsubst %.mly,%.mli,$(wildcard *.mly)) \
            $(patsubst %.mll,%.ml,$(wildcard *.mll))

opam-file-format.cma: $(MODULES:%=%.cmo)
	ocamlc -a $^ -o $@

opam-file-format.cmxa: $(MODULES:%=%.cmx)
	ocamlopt -a $^ -o $@

opam-file-format.cmxs: $(MODULES:%=%.cmx)
	ocamlopt -shared $^ -o $@

.PHONY: clean
clean:
	rm -f *.cm* *.o *.a $(GENERATED) .depend

.depend: *.ml *.mli $(GENERATED)
	ocamldep *.mli *.ml > .depend

-include .depend
