NAME := jbuilder

# Default rule
default:
	ocaml build.ml build-package jbuilder

install:
	opam-installer -i --prefix $(PREFIX) jbuilder.install

uninstall:
	opam-installer -u --prefix $(PREFIX) jbuilder.install

reinstall: uninstall reinstall

clean:
	rm -rf _build

.PHONY: default install uninstall reinstall clean
