.DEFAULT_GOAL := help

PREFIX_ARG := $(if $(PREFIX),--prefix $(PREFIX),)
LIBDIR_ARG := $(if $(LIBDIR),--libdir $(LIBDIR),)
DESTDIR_ARG := $(if $(DESTDIR),--destdir $(DESTDIR),)
INSTALL_ARGS := $(PREFIX_ARG) $(LIBDIR_ARG) $(DESTDIR_ARG)
BIN := ./_boot/dune.exe

# Dependencies recommended for developing dune locally,
# but not wanted in CI
DEV_DEPS := \
core_bench \
patdiff

TEST_OCAMLVERSION := 5.3.0
# When updating this version, don't forget to also bump the number in the docs.

-include Makefile.dev

.PHONY: help
help:
	@cat doc/make-help.txt

.PHONY: bootstrap
bootstrap:
	rm -rf _boot
	$(MAKE) -B $(BIN)

.PHONY: test-bootstrap
test-bootstrap:
	rm -rf _test_boot
	@ocaml boot/bootstrap.ml --boot-dir _test_boot

.PHONY: test-bootstrap-script
test-bootstrap-script:
	@ocamlc -i boot/bootstrap.ml

.PHONY: release
release: $(BIN)
	@$(BIN) build @install -p dune --profile dune-bootstrap

$(BIN):
	@ocaml boot/bootstrap.ml

dev: $(BIN)
	$(BIN) build @install

watch: $(BIN)
	$(BIN) build @install --watch

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

.PHONY: install-ocamlformat
install-ocamlformat:
	opam install -y ocamlformat.$$(awk -F = '$$1 == "version" {print $$2}' .ocamlformat)

.PHONY: dev-deps
dev-deps:
	opam install -y . --deps-only --with-dev-setup

.PHONY: dev-deps-sans-melange
dev-deps-sans-melange: dev-deps

.PHONY: dev-switch
dev-switch:
	opam update
# Ensuring that either a dev switch already exists or a new one is created
	if test -d _opam ; then \
		opam install -y --update-invariant ocaml.$(TEST_OCAMLVERSION); \
	else \
		opam switch create -y . $(TEST_OCAMLVERSION) --no-install ; \
	fi
	opam pin add -y . -n --with-version=dev
	opam install -y . --deps-only --with-test --with-dev-setup
	$(MAKE) install-ocamlformat
	opam install -y $(DEV_DEPS)

.PHONY: test
test: $(BIN)
	$(BIN) runtest

test-windows: $(BIN)
	$(BIN) build @runtest-windows

test-js: $(BIN)
	$(BIN) build @runtest-js

test-wasm: $(BIN)
	DUNE_WASM_TEST=enable $(BIN) build @runtest-wasm

test-coq: $(BIN)
	DUNE_COQ_TEST=enable $(BIN) build @runtest-coq

test-melange: $(BIN)
	$(BIN) build @runtest-melange

test-all: $(BIN)
	$(BIN) build @runtest @runtest-js @runtest-coq @runtest-melange

test-all-sans-melange: $(BIN)
	$(BIN) build @runtest @runtest-js @runtest-coq

test-ox: $(BIN)
	$(BIN) runtest test/blackbox-tests/test-cases/oxcaml

.PHONY: check
check: $(BIN)
	@$(BIN) build @check

.PHONY: fmt
fmt: $(BIN)
	@$(BIN) fmt

.PHONY: promote
promote: $(BIN)
	@$(BIN) promote

.PHONY: accept-corrections
accept-corrections: promote

.PHONY: clean
clean:
	rm -rf _boot _build

distclean: clean
	rm -f src/dune_rules/setup.ml

.PHONY: doc
doc:
	sphinx-build -W doc doc/_build

# livedoc-deps: you may need to [pip3 install sphinx-autobuild] and [pip3 install -r doc/requirements.txt]
livedoc:
	cd doc && sphinx-autobuild . _build --port 8888 -q --re-ignore '\.#.*'

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
bench: $(BIN)
	@$(BIN) exec -- ./bench/bench.exe $(BIN)

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
	DUNE_RELEASE_DELEGATE=github-dune-release-delegate dune-release publish --verbose
	dune-release opam pkg
	dune-release opam submit

.PHONY: docker-build-image
docker-build-image:
	docker build -f docker/dev.Dockerfile -t dune .

.PHONY: docker-compose
docker-compose:
	docker compose -f docker/dev.yml run dune bash
