Test `dune tools which ocamlformat`:
  $ . ./helpers.sh
  $ mkrepo

  $ make_fake_ocamlformat "0.26.2"
  $ make_ocamlformat_opam_pkg "0.26.2"
  $ make_project_with_dev_tool_lockdir

Install ocamlformat as a dev tool:
  $ DUNE_CONFIG__LOCK_DEV_TOOL=enabled dune tools install ocamlformat
  Solution for dev-tools.locks/ocamlformat:
  - ocamlformat.0.26.2

Verify that ocamlformat is installed:
  $ DUNE_CONFIG__LOCK_DEV_TOOL=enabled dune tools which ocamlformat
  _build/_private/default/.dev-tool/ocamlformat/target/bin/ocamlformat
