Show an edge case of `(include_subdirs ..)` and ocamllex / menhir

  $ make_menhir_project 3.13 3.0

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

  $ make_trivial_ocamllex lexer.mll
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
