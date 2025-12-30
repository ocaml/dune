Show error when menhir sources don't exist

  $ cat > dune-project << EOF
  > (lang dune 3.22)
  > (using menhir 3.0)
  > EOF

We add a `(menhir ..)` stanza in the group root dune file

  $ mkdir -p src/a
  $ cat > src/dune << EOF
  > (library (name foo))
  > (ocamllex lexer)
  > (menhir
  >  (modules parser))
  > EOF
  $ cat >src/lexer.mll  <<EOF
  > {
  > }
  > rule lex = parse
  >   | _   { true  }
  >   | eof { false }
  > EOF

Show that the menhir stanza must live next to the source

  $ dune build
  File "src/dune", lines 3-4, characters 0-26:
  3 | (menhir
  4 |  (modules parser))
  Error: No rule found for src/parser.mly
  [1]
