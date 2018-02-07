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

promote:
	$(BIN) promote

accept-corrections: promote

all-supported-ocaml-versions:
	$(BIN) build --dev @install @runtest --workspace jbuild-workspace.dev --root .

clean:
	$(BIN) clean
	rm -f ./boot.exe $(wildcard ./bootstrap.cmi ./bootstrap.cmo ./bootstrap.exe)

doc:
	cd doc && sphinx-build . _build

update-jbuilds: $(BIN)
	$(BIN) build --dev @doc/runtest --auto-promote

.DEFAULT_GOAL := default
.PHONY: default install uninstall reinstall clean test doc
.PHONY: promote accept-corrections
