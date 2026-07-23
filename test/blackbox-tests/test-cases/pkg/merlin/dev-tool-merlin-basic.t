Test that the "dune tools exec ocamlmerlin" command causes merlin to be
locked, built and run when the command is run from a dune project with
a lockdir containing an "ocaml" lockfile.

  $ mkrepo
  $ make_mock_merlin_package
  $ mk_ocaml 5.2.0

  $ setup_merlin_workspace

  $ make_named_package_project foo 3.16 "(ocaml (= 5.2.0))"

  $ dune build

  $ DUNE_CONFIG__LOCK_DEV_TOOL=enabled dune tools exec ocamlmerlin
  Solution for _build/.dev-tools.locks/merlin:
  - merlin.0.0.1
  - ocaml-base-compiler.5.2.0
  - ocaml-compiler.5.2.0
       Running 'ocamlmerlin'
  hello from fake ocamlmerlin
