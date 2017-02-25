BIN := ./_build/default/bin/main.exe

default: boot.exe
	./boot.exe -j 4 --dev

boot.exe: bootstrap.ml
	ocaml bootstrap.ml

install:
	$(BIN) install

uninstall:
	$(BIN) uninstall

reinstall: uninstall reinstall

clean:
	rm -rf _build

cinaps:
	cinaps -i doc/jbuild

.PHONY: default install uninstall reinstall clean cinaps
