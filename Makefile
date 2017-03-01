INSTALL_ARGS := $(if $(PREFIX),--prefix $(PREFIX),)
BIN := ./_jbuild/default/bin/main.exe

default: boot.exe
	./boot.exe -j 4 --dev

boot.exe: bootstrap.ml
	ocaml bootstrap.ml

install:
	$(BIN) install $(INSTALL_ARGS)

uninstall:
	$(BIN) uninstall $(INSTALL_ARGS)

reinstall: uninstall reinstall

all-supported-ocaml-versions:
	$(BIN) build @install --workspace jbuild-workspace.dev --root .

clean:
	rm -rf _jbuild

.PHONY: default install uninstall reinstall clean
