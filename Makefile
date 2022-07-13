.DEFAULT_GOAL := help

PREFIX_ARG := $(if $(PREFIX),--prefix $(PREFIX),)
LIBDIR_ARG := $(if $(LIBDIR),--libdir $(LIBDIR),)
DESTDIR_ARG := $(if $(DESTDIR),--destdir $(DESTDIR),)
INSTALL_ARGS := $(PREFIX_ARG) $(LIBDIR_ARG) $(DESTDIR_ARG)
BIN := ./dune.exe

# Dependencies used for testing dune, when developed locally and
# when tested in CI
TEST_DEPS := \
lwt \
bisect_ppx \
cinaps \
coq-native \
"coq>=8.16.0" \
core_bench \
"csexp>=1.3.0" \
js_of_ocaml \
js_of_ocaml-compiler \
"mdx>=2.1.0" \
menhir \
"merlin>=3.4.0" \
ocamlfind \
ocamlformat.$$(awk -F = '$$1 == "version" {print $$2}' .ocamlformat) \
"odoc>=2.0.1" \
"ppx_expect.v0.15.0" \
ppx_inline_test \
ppxlib \
result \
ctypes \
"utop>=2.6.0"

# Dependencies recommended for developing dune locally,
# but not wanted in CI
DEV_DEPS := \
patdiff

TEST_OCAMLVERSION := 4.14.0

-include Makefile.dev

.PHONY: help
help:
	@cat doc/make-help.txt

.PHONY: release
release: $(BIN)
	@$(BIN) build -p dune --profile dune-bootstrap

dune.exe: bootstrap.ml boot/libs.ml boot/duneboot.ml
	@ocaml bootstrap.ml

dev: $(BIN)
	$(BIN) build @install

all: $(BIN)
	$(BIN) build

.PHONY: install
install:
	$(BIN) install $(INSTALL_ARGS) dune

.PHONY: uninstall
uninstall:
	$(BIN) uninstall $(INSTALL_ARGS) dune

.PHONY: reinstall
reinstall: uninstall install

install-ocamlformat:
	opam install -y ocamlformat.$$(awk -F = '$$1 == "version" {print $$2}' .ocamlformat)

dev-deps:
	opam install -y $(TEST_DEPS)

.PHONY: dev-switch
dev-switch:
	opam update
# Ensuring that either a dev switch already exists or a new one is created
	test "$(shell opam switch show)" = "$(shell pwd)" || \
		opam switch create -y . $(TEST_OCAMLVERSION) --deps-only --with-test
	opam install -y --update-invariant ocaml.$(TEST_OCAMLVERSION) $(TEST_DEPS) $(DEV_DEPS)

.PHONY: test
test: $(BIN)
	$(BIN) runtest

test-windows: $(BIN)
	$(BIN) build @runtest-windows

test-js: $(BIN)
	$(BIN) build @runtest-js

test-coq: $(BIN)
	$(BIN) build @runtest-coq

test-all: $(BIN)
	$(BIN) build @runtest @runtest-js @runtest-coq

.PHONY: check
check: $(BIN)
	$(BIN) build @check

.PHONY: fmt
fmt: $(BIN)
	$(BIN) fmt

.PHONY: promote
promote: $(BIN)
	$(BIN) promote

.PHONY: accept-corrections
accept-corrections: promote

all-supported-ocaml-versions: $(BIN)
	$(BIN) build @install @runtest --workspace dune-workspace.dev --root .

.PHONY: clean
clean: $(BIN)
	$(BIN) clean || true
	rm -rf _boot dune.exe

distclean: clean
	rm -f src/dune_rules/setup.ml

.PHONY: doc
doc:
	sphinx-build doc doc/_build

# livedoc-deps: you may need to [pip3 install sphinx-autobuild] and [pip3 install sphinx-rtd-theme]
livedoc:
	cd doc && sphinx-autobuild . _build \
	  --port 8888 -q  --host $(shell hostname) --re-ignore '\.#.*'

update-jbuilds: $(BIN)
	$(BIN) build @doc/runtest --auto-promote

# If the first argument is "run"...
ifeq (dune,$(firstword $(MAKECMDGOALS)))
  # use the rest as arguments for "run"
  RUN_ARGS := $(wordlist 2,$(words $(MAKECMDGOALS)),$(MAKECMDGOALS))
  # ...and turn them into do-nothing targets
  $(eval $(RUN_ARGS):;@:)
endif

.PHONY: bench
bench: release
	@$(BIN) exec -- ./bench/bench.exe _build/default/dune.exe

.PHONY: dune
dune: $(BIN)
	$(BIN) $(RUN_ARGS)

# Use this target to make sure that we always run the in source dune when making
# the release
.PHONY: opam-release
opam-release: dev
	$(BIN) exec -- $(MAKE) dune-release

dune-release:
	dune-release tag
	dune-release distrib --skip-build --skip-lint --skip-tests
# See https://github.com/ocamllabs/dune-release/issues/206
	DUNE_RELEASE_DELEGATE=github-dune-release-delegate dune-release publish distrib --verbose
	dune-release opam pkg
	dune-release opam submit

# see nix/default.nix for details
.PHONY: nix/opam-selection.nix
nix/opam-selection.nix: Makefile
	nix-shell -A resolve ./
