Test building a module group containing a `.mll` file, without `(ocamllex ..)`

  $ cat > dune-project <<EOF
  > (lang dune 3.21)
  > EOF
  $ cat > dune <<EOF
  > (library (name foo))
  > EOF
  $ cat > lexer.mll <<EOF
  > {
  > }
  > rule lex = parse
  >   | _   { true  }
  >   | eof { false }
  > EOF

module `Lexer` can't be found if no `(ocamllex ..)` stanza defines it

  $ cat > foo.ml<<EOF
  > let x = Lexer.lex
  > EOF

  $ dune build foo.cma --display=short
        ocamlc .foo.objs/byte/foo.{cmi,cmo,cmt} (exit 2)
  File "foo.ml", line 1, characters 8-13:
  1 | let x = Lexer.lex
              ^^^^^
  Error: Unbound module Lexer
  [1]
