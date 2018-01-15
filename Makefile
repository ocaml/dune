INSTALL_ARGS := $(if $(PREFIX),--prefix $(PREFIX),)
BIN := ./_build/default/bin/main.exe

-include Makefile.dev

default: boot.exe
	./boot.exe -j 4 --dev

release: boot.exe
	./boot.exe -j 4

boot.exe: bootstrap.ml
	ocaml bootstrap.ml

install:
	$(BIN) install $(INSTALL_ARGS)

uninstall:
	$(BIN) uninstall $(INSTALL_ARGS)

reinstall: uninstall reinstall

test:
	$(BIN) runtest --dev

all-supported-ocaml-versions:
	$(BIN) build --dev @install @runtest --workspace jbuild-workspace.dev --root .

clean:
	$(BIN) clean
	rm -f ./boot.exe $(wildcard ./bootstrap.cmi ./bootstrap.cmo ./bootstrap.exe)

doc:
	cd doc && sphinx-build . _build

update-jbuilds: $(BIN)
	$(BIN) build --dev @jbuild --promote copy

accept-corrections:
	for i in `find . -name \*.corrected`; do \
	  cp $$i $${i%.corrected}; \
	done

.DEFAULT_GOAL := default
.PHONY: default install uninstall reinstall clean test doc
.PHONY: accept-corrections
