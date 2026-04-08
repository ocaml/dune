Show an edge case of `(include_subdirs ..)` and ocamllex / menhir

  $ cat > dune-project << EOF
  > (lang dune 3.13)
  > (using menhir 3.0)
  > EOF

We add a `(menhir ..)` stanza in the group root dune file

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
  $ cat >src/a/parser.mly <<'EOF'
  > %token EOF
  > %start main
  > %type <unit> main
  > %%
  > main:
  >   | EOF { () }
  > EOF

Dune doesn't find the parser from the root dune file

  $ dune build
  File "dune", lines 3-5, characters 0-42:
  3 | (menhir
  4 |  (modules parser)
  5 |  (flags --dump))
  Error: No rule found for parser.mly
  [1]

Show that the menhir stanza must live next to the source

  $ cat > dune << EOF
  > (include_subdirs unqualified)
  > (library (name foo))
  > (ocamllex lexer)
  > EOF
  $ cat > src/a/dune << EOF
  > (menhir
  >  (modules parser)
  >  (flags --dump))
  > EOF

  $ dune build
