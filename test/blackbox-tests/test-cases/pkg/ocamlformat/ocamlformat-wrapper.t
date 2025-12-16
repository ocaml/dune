Exercise running the ocamlformat wrapper command.

  $ mkrepo

  $ make_fake_ocamlformat "0.26.2"
  $ make_ocamlformat_opam_pkg "0.26.2"
  $ make_project_with_dev_tool_lockdir

  $ DUNE_CONFIG__LOCK_DEV_TOOL=enabled dune tools install ocamlformat
  Solution for _build/.dev-tools.locks/ocamlformat:
  - ocamlformat.0.26.2

  $ dune tools exec ocamlformat
       Running 'ocamlformat'
  formatted with version 0.26.2
