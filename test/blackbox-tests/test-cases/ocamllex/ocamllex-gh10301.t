Show an edge case of `(include_subdirs ..)` and ocamllex

  $ cat > dune-project << EOF
  > (lang dune 3.13)
  > EOF

  $ mkdir -p src/a
  $ cat > dune << EOF
  > (include_subdirs unqualified)
  > (library (name foo))
  > (ocamllex lexer)
  > EOF

  $ cat > src/a/lexer.mll <<EOF
  > {
  > }
  > rule lex = parse
  >   | _   { true  }
  >   | eof { false }
  > EOF

  $ dune build
  File "dune", line 3, characters 0-16:
  3 | (ocamllex lexer)
      ^^^^^^^^^^^^^^^^
  Error: The `ocamllex' stanza for a module must be specified in the same
  directory as the module it generates.
  - module directory: src/a
  - ocamllex directory: .
  [1]

The `(ocamllex ..)` stanza must live next to the source file

  $ cat > dune << EOF
  > (include_subdirs unqualified)
  > (library (name foo))
  > EOF

  $ cat > src/a/dune << EOF
  > (ocamllex lexer)
  > EOF

  $ dune build
