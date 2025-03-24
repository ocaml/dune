
  $ cat > dune-project << EOF
  > (lang dune 3.13)
  > (using menhir 3.0)
  > EOF

  $ mkdir -p src/a
  $ cat > dune << EOF
  > (include_subdirs unqualified)
  > (library (name foo))
  > (menhir
  >  (modules parser)
  >  (flags --dump))
  > (ocamllex lexer)
  > EOF

  $ cat >lexer.mll  <<EOF
  > {
  > }
  > rule lex = parse
  >   | _   { true  }
  >   | eof { false }
  > EOF
  $ touch src/a/parser.mly
  $ dune build
  File "dune", lines 3-5, characters 0-42:
  3 | (menhir
  4 |  (modules parser)
  5 |  (flags --dump))
  Error: No rule found for parser.mly
  [1]

