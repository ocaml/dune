.DEFAULT_GOAL := help

PREFIX_ARG := $(if $(PREFIX),--prefix $(PREFIX),)
LIBDIR_ARG := $(if $(LIBDIR),--libdir $(LIBDIR),)
DESTDIR_ARG := $(if $(DESTDIR),--destdir $(DESTDIR),)
INSTALL_ARGS := $(PREFIX_ARG) $(LIBDIR_ARG) $(DESTDIR_ARG)
BIN := ./dune.exe

# Dependencies used for developing and testing dune
DEV_DEPS := \
core_bench \
menhir \
merlin \
ocamlformat \
odoc \
ppx_expect \
ppx_inline_test \
ppxlib \
utop

-include Makefile.dev

help:
	@cat doc/make-help.txt

release: $(BIN)
	$(BIN) build -p dune --profile dune-bootstrap

dune.exe: bootstrap.ml boot/libs.ml boot/duneboot.ml
	ocaml bootstrap.ml

dev: $(BIN)
	$(BIN) build @install

all: $(BIN)
	$(BIN) build

install:
	$(BIN) install $(INSTALL_ARGS) dune

uninstall:
	$(BIN) uninstall $(INSTALL_ARGS) dune

reinstall: uninstall install

dev-switch:
	opam switch create -y . --deps-only --with-test
	opam install -y $(DEV_DEPS)

test: $(BIN)
	$(BIN) runtest

test-js: $(BIN)
	$(BIN) build @runtest-js

test-coq: $(BIN)
	$(BIN) build @runtest-coq

test-all: $(BIN)
	$(BIN) build @runtest @runtest-js @runtest-coq

check: $(BIN)
	$(BIN) build @check

fmt: $(BIN)
	$(BIN) build @fmt --auto-promote

promote: $(BIN)
	$(BIN) promote

accept-corrections: promote

all-supported-ocaml-versions: $(BIN)
	$(BIN) build @install @runtest --workspace dune-workspace.dev --root .

clean: $(BIN)
	$(BIN) clean || true
	rm -rf _boot dune.exe

distclean: clean
	rm -f src/dune/setup.ml

doc:
	cd doc && sphinx-build . _build

livedoc:
	cd doc && sphinx-autobuild . _build \
	  -p 8888 -q  --host $(shell hostname) -r '\.#.*'

update-jbuilds: $(BIN)
	$(BIN) build @doc/runtest --auto-promote

# If the first argument is "run"...
ifeq (dune,$(firstword $(MAKECMDGOALS)))
  # use the rest as arguments for "run"
  RUN_ARGS := $(wordlist 2,$(words $(MAKECMDGOALS)),$(MAKECMDGOALS))
  # ...and turn them into do-nothing targets
  $(eval $(RUN_ARGS):;@:)
endif

dune: $(BIN)
	$(BIN) $(RUN_ARGS)

.PHONY: default install uninstall reinstall clean test doc dev-switch
.PHONY: promote accept-corrections opam-release dune check fmt

opam-release:
	dune-release distrib --skip-build --skip-lint --skip-tests -n dune
	# See https://github.com/ocamllabs/dune-release/issues/206
	DUNE_RELEASE_DELEGATE=github-dune-release-delegate dune-release publish distrib --verbose -n dune
	dune-release opam pkg -n dune
	dune-release opam submit -n dune
