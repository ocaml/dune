Test that the "dune tools exec ocamlmerlin" command causes merlin to be
locked, built and run when the command is run from a dune project with
a lockdir containing an "ocaml" lockfile.

  $ mkrepo
  $ make_mock_merlin_package
  $ mk_ocaml 5.2.0

  $ setup_merlin_workspace

  $ cat > dune-project <<EOF
  > (lang dune 3.16)
  > 
  > (package
  >  (name foo)
  >  (allow_empty)
  >  (depends
  >    (ocaml (= 5.2.0))))
  > EOF

  $ dune build

  $ DUNE_CONFIG__LOCK_DEV_TOOL=enabled dune tools exec ocamlmerlin
  Solution for _build/.dev-tools.locks/merlin:
  - merlin.0.0.1
  - ocaml-base-compiler.5.2.0
  - ocaml-compiler.5.2.0
       Running 'ocamlmerlin'
  hello from fake ocamlmerlin
