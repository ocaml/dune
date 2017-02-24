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

.PHONY: default install uninstall reinstall clean
