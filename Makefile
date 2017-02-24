NAME := jbuilder

default: boot.exe
	./boot.exe -j 4 --dev

boot.exe: bootstrap.ml
	ocaml bootstrap.ml

install:
	opam-installer -i --prefix $(PREFIX) jbuilder.install

uninstall:
	opam-installer -u --prefix $(PREFIX) jbuilder.install

reinstall: uninstall reinstall

clean:
	rm -rf _build

cinaps:
	cinaps -i doc/jbuild

.PHONY: default install uninstall reinstall clean cinaps
