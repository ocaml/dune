Test `dune tools which ocamlformat`:
  $ . ./helpers.sh
  $ mkrepo

  $ make_fake_ocamlformat "0.26.2"
  $ make_ocamlformat_opam_pkg "0.26.2"
  $ make_project_with_dev_tool_lockdir

Install ocamlformat as a dev tool:
  $ dune tools install ocamlformat
  Solution for .dune-tools-solution-cache/ocamlformat:
  - ocamlformat.0.26.2

Verify that ocamlformat is installed:
  $ dune tools which ocamlformat
  _build/_private/default/.dev-tool/ocamlformat/target/bin/ocamlformat
