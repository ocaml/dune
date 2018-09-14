INSTALL_ARGS := $(if $(PREFIX),--prefix $(PREFIX),)
BIN := ./_build/default/bin/main_dune.exe

-include Makefile.dev

default: boot.exe
	./boot.exe

release: boot.exe
	./boot.exe --release

boot.exe: bootstrap.ml
	ocaml bootstrap.ml

install:
	$(BIN) install $(INSTALL_ARGS) dune

uninstall:
	$(BIN) uninstall $(INSTALL_ARGS) dune

reinstall: uninstall reinstall

test:
	$(BIN) runtest

test-js:
	$(BIN) build @runtest-js

test-all:
	$(BIN) build @runtest @runtest-js

promote:
	$(BIN) promote

accept-corrections: promote

all-supported-ocaml-versions:
	$(BIN) build @install @runtest --workspace dune-workspace.dev --root .

clean:
	$(BIN) clean
	rm -f ./boot.exe $(wildcard ./bootstrap.cmi ./bootstrap.cmo ./bootstrap.exe)

distclean: clean
	rm -f src/setup.ml

doc:
	cd doc && sphinx-build . _build

livedoc:
	cd doc && sphinx-autobuild . _build \
	  -p 8888 -q  --host $(shell hostname) -r '\.#.*'

update-jbuilds: $(BIN)
	$(BIN) build @doc/runtest --auto-promote

.PHONY: default install uninstall reinstall clean test doc
.PHONY: promote accept-corrections opam-release

opam-release:
	dune-release distrib --skip-build --skip-lint --skip-tests
	dune-release publish distrib --verbose
	dune-release opam pkg
	dune-release opam submit
