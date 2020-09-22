all: tests

tests:
	ocamlfind ocamlc tests.ml -o tests -linkpkg -package alcotest -package opam-file-format

run: tests
	./tests

.PHONY: clean
clean:
	rm -f tests
	rm -f *.cmi *.cmo
	rm -rf _build
