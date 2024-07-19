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
"dkml-workflows>=1.2.0" \
patdiff

TEST_OCAMLVERSION := 4.14.2

-include Makefile.dev

.PHONY: help
help:
	@cat doc/make-help.txt

.PHONY: bootstrap
bootstrap:
	$(MAKE) -B $(BIN)

.PHONY: release
release: $(BIN)
	@$(BIN) build @install -p dune --profile dune-bootstrap

$(BIN):
	@ocaml boot/bootstrap.ml

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

.PHONY: install-ocamlformat
install-ocamlformat:
	opam install -y ocamlformat.$$(awk -F = '$$1 == "version" {print $$2}' .ocamlformat)

.PHONY: dev-deps
dev-deps:
	opam install -y . --deps-only --with-dev-setup

.PHONY: coverage-deps
coverage-deps:
	opam install -y bisect_ppx

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

test-coq: $(BIN)
	DUNE_COQ_TEST=enable $(BIN) build @runtest-coq

test-melange: $(BIN)
	$(BIN) build @runtest-melange

test-all: $(BIN)
	$(BIN) build @runtest @runtest-js @runtest-coq @runtest-melange

test-all-sans-melange: $(BIN)
	$(BIN) build @runtest @runtest-js @runtest-coq

test-coverage: $(BIN)
	- $(BIN) build --instrument-with bisect_ppx --force @runtest
	bisect-ppx-report send-to Coveralls

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

all-supported-ocaml-versions: $(BIN)
	$(BIN) build @install @runtest --workspace dune-workspace.dev --root .

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

# you will need to use a dev-switch or do opam install dkml-workflows.
# we run --auto-promote twice since first may fail (ex. missing files with Unable to resolve symlink)
update-dkml: $(BIN)
	opam exec -- generate-setup-dkml-scaffold
	$(BIN) build @gen-dkml --auto-promote || $(BIN) build @gen-dkml --auto-promote
	rm -rf ci/setup-dkml/gh-darwin ci/setup-dkml/gh-linux ci/setup-dkml/gl
	rm -rf ci/setup-dkml/pc/setup-dkml-darwin_*.sh ci/setup-dkml/pc/setup-dkml-linux_*.sh
	$(BIN) build @ci/fmt --auto-promote || $(BIN) build @ci/fmt --auto-promote

# assumes MSYS2 or Cygwin, and Visual Studio. Do not use 'with-dkml make ...'
desktop-ci-windows_x86:
	if command -v pwsh; then \
	  pwsh ./ci/setup-dkml/pc/setup-dkml-windows_x86.ps1; \
	else \
	  powershell ./ci/setup-dkml/pc/setup-dkml-windows_x86.ps1; \
	fi
	/usr/bin/env PATH=/usr/bin \
	  dkml_host_abi=windows_x86 abi_pattern=win32-windows_x86_64; \
	  opam_root=.ci/o exe_ext=.exe \
	  /bin/sh ci/build-test.sh

# assumes MSYS2 or Cygwin, and Visual Studio. Do not use 'with-dkml make ...'
desktop-ci-windows_x86_64:
	if command -v pwsh; then \
	  pwsh ./ci/setup-dkml/pc/setup-dkml-windows_x86_64.ps1; \
	else \
	  powershell ./ci/setup-dkml/pc/setup-dkml-windows_x86_64.ps1; \
	fi
	/usr/bin/env PATH=/usr/bin \
	  dkml_host_abi=windows_x86 abi_pattern=win32-windows_x86_64 \
	  opam_root=.ci/o exe_ext=.exe \
	  /bin/sh ci/build-test.sh

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
	DUNE_RELEASE_DELEGATE=github-dune-release-delegate dune-release publish distrib --verbose
	dune-release opam pkg
	dune-release opam submit

.PHONY: docker-build-image
docker-build-image:
	docker build -f docker/dev.Dockerfile -t dune .

.PHONY: docker-compose
docker-compose:
	docker compose -f docker/dev.yml run dune bash
