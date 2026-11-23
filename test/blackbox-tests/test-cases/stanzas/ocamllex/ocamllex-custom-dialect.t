An ocamllex input normally builds as a single generated module.

  $ make_dune_project 3.22
  $ make_trivial_ocamllex lexer.mll
  $ cat >dune <<'EOF'
  > (ocamllex lexer)
  > (library (name lexer))
  > EOF
  $ dune build lexer.cma

Registering the input's extension as an implementation dialect makes Dune 3.25
mistake it for a second implementation, even without a formatter.

  $ echo '(dialect (name mll) (implementation (extension mll)))' >>dune-project
  $ dune build lexer.cma
  Error: Too many files for module Lexer in .:
  - _build/default/lexer.mll
  - _build/default/lexer.ml
  [1]
