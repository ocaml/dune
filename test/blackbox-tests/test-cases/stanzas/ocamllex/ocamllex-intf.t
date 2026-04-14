Test `(ocamllex ..)` stanza with an interface file for the lexer

  $ make_dune_project 3.21

The unit `mod.mll` is present in the working tree, `lib.ml` uses it:

  $ cat > mod.mll <<EOF
  > {
  > }
  > rule lex = parse
  >   | _   { true  }
  >   | eof { false }
  > EOF

  $ cat > mod.mli <<EOF
  > val lex: Lexing.lexbuf -> bool
  > EOF

  $ cat >foo.ml <<EOF
  > let x = Mod.lex
  > EOF
  $ cat >dune <<EOF
  > (ocamllex mod)
  > (library (name foo))
  > EOF

  $ dune build foo.cma
