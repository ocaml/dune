Test that the "dune tools exec ocamlmerlin" command causes merlin to be
locked, built and run when the command is run from a dune project with
a lockdir containing an "ocaml" lockfile.

  $ mkrepo
  $ make_mock_merlin_package
  $ cat >>mock-opam-repository/packages/merlin/merlin.0.0.1/opam <<'EOF'
  > depends: [ "ocaml" ]
  > EOF
  $ mk_ocaml 5.2.0
  $ cat >>mock-opam-repository/packages/ocaml-base-compiler/ocaml-base-compiler.5.2.0/opam <<'EOF'
  > build: [ [ "echo" "building fake compiler" ] ]
  > EOF

  $ setup_merlin_workspace

  $ make_named_package_project foo 3.16 "(ocaml (= 5.2.0))"

  $ DUNE_CACHE=disabled dune build 2>&1 | grep "building fake compiler"
  building fake compiler

The dev tool's non-portable lock directory describes the same compiler as the
project's portable lock directory. The dev tool reuses the compiler already
built for the project.

  $ DUNE_CACHE=disabled dune tools exec ocamlmerlin 2>&1 | tee output
  Solution for _build/.dev-tools.locks/merlin:
  - merlin.0.0.1
  - ocaml.5.2.0
  - ocaml-base-compiler.5.2.0
  - ocaml-compiler.5.2.0
       Running 'ocamlmerlin'
  hello from fake ocamlmerlin
  $ grep "building fake compiler" output
  [1]
