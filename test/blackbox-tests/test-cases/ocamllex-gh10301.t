
  $ cat > dune-project << EOF
  > (lang dune 3.13)
  > EOF

  $ mkdir -p src/a
  $ cat > dune << EOF
  > (include_subdirs unqualified)
  > (library (name foo))
  > (ocamllex lexer)
  > EOF

  $ touch src/a/lexer.mll
  $ dune build
  File "dune", line 3, characters 0-16:
  3 | (ocamllex lexer)
      ^^^^^^^^^^^^^^^^
  Error: No rule found for lexer.mll
  [1]

