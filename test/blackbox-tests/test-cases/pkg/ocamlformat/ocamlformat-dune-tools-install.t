Test that checks the interaction of `dune fmt` with `dune tools install
ocamlformat`.

  $ . ./helpers.sh
  $ mkrepo

Set up a ocamlformat via OPAM package:

  $ OCAMLFORMAT_VERSION="0.26.2"
  $ make_fake_ocamlformat "${OCAMLFORMAT_VERSION}"
  $ echo "ocamlformat-${OCAMLFORMAT_VERSION}.tar" > fake-curls
  $ PORT=1
  $ make_ocamlformat_opam_pkg "${OCAMLFORMAT_VERSION}" $PORT

Make another ocamlformat, from path:

  $ mkdir .fakebin
  $ cat > .fakebin/ocamlformat <<EOF
  > #!/bin/sh
  > echo "ocamlformat from PATH, not pkg" >&2
  > exit 1
  > EOF
  $ chmod +x .fakebin/ocamlformat

Add our fake ocamlformat to shadow whatever might be installed on the test
system with a binary with known behaviour:

  $ export PATH=$(pwd)/.fakebin:$PATH

Set up the project to be able to use ocamlformat:

  $ make_project_with_dev_tool_lockdir
  $ cat > .ocamlformat <<EOF
  > version = ${OCAMLFORMAT_VERSION}
  > EOF

We have no lock dir for ocamlformat, it should use the one from path

  $ dune fmt --preview
  ocamlformat from PATH, not pkg
  -> required by _build/default/.formatted/foo.ml
  -> required by alias .formatted/fmt
  -> required by alias fmt
  [1]

Installing ocamlformat via `dune tools install` should work:

  $ dune tools install ocamlformat
  Solution for _build/.dev-tools.locks/ocamlformat:
  - ocamlformat.0.26.2

Formatting should use the locked ocamlformat with the feature flag enabled:

  $ DUNE_CONFIG__LOCK_DEV_TOOL=enabled dune fmt --preview
  File "foo.ml", line 1, characters 0-0:
  Error: Files _build/default/foo.ml and _build/default/.formatted/foo.ml
  differ.
  [1]

It should also use the locked dev tool when the feature flag is not passed:

  $ dune fmt --preview
  ocamlformat from PATH, not pkg
  -> required by _build/default/.formatted/foo.ml
  -> required by alias .formatted/fmt
  -> required by alias fmt
  [1]

It should use the ocamlformat from PATH when the lock dir is deleted:

  $ rm -r "${dev_tool_lock_dir}"
  $ dune fmt --preview
  ocamlformat from PATH, not pkg
  -> required by _build/default/.formatted/foo.ml
  -> required by alias .formatted/fmt
  -> required by alias fmt
  [1]
