INSTALL_ARGS := $(if $(PREFIX),--prefix $(PREFIX),)
BIN := ./_build/default/bin/main.exe

default: boot.exe
	./boot.exe -j 4 --dev

boot.exe: bootstrap.ml
	ocaml bootstrap.ml

install:
	$(BIN) install $(INSTALL_ARGS)

uninstall:
	$(BIN) uninstall $(INSTALL_ARGS)

reinstall: uninstall reinstall

clean:
	rm -rf _build

.PHONY: default install uninstall reinstall clean
