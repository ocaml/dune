INSTALL_ARGS := $(if $(PREFIX),--prefix $(PREFIX),)
BIN := ./_build/default/bin/main.exe

-include Makefile.dev

default: boot.exe
	./boot.exe --dev

release: boot.exe
	./boot.exe

boot.exe: bootstrap.ml
	ocaml bootstrap.ml

install:
	$(BIN) install $(INSTALL_ARGS)

uninstall:
	$(BIN) uninstall $(INSTALL_ARGS)

reinstall: uninstall reinstall

test:
	$(BIN) runtest --dev

test-js:
	$(BIN) build @runtest-js --dev

test-all:
	$(BIN) build @runtest @runtest-js --dev

promote:
	$(BIN) promote

accept-corrections: promote

all-supported-ocaml-versions:
	$(BIN) build --dev @install @runtest --workspace jbuild-workspace.dev --root .

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
	$(BIN) build --dev @doc/runtest --auto-promote

update-sexp-parser:
	$(BIN) build --dev @update-sexp-parser --auto-promote

.PHONY: default install uninstall reinstall clean test doc
.PHONY: promote accept-corrections
