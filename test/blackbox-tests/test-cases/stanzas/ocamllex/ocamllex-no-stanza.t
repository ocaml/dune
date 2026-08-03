Test building a module group containing a `.mll` file, without `(ocamllex ..)`

  $ make_dune_project 3.22
  $ cat > dune <<EOF
  > (library (name foo))
  > EOF
  $ make_trivial_ocamllex lexer.mll

module `Lexer` can't be found if no `(ocamllex ..)` stanza defines it

  $ cat > foo.ml<<EOF
  > let x = Lexer.lex
  > EOF

  $ dune build foo.cma
  File "foo.ml", line 1, characters 8-13:
  1 | let x = Lexer.lex
              ^^^^^
  Error: Unbound module Lexer
  [1]

Same if ocamllex `(modules ..)` is empty

  $ cat > dune <<EOF
  > (ocamllex (modules))
  > (library (name foo))
  > EOF

  $ dune build foo.cma
  File "foo.ml", line 1, characters 8-13:
  1 | let x = Lexer.lex
              ^^^^^
  Error: Unbound module Lexer
  [1]
