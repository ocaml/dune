Test `(include_subdirs qualified)` in the presence of invalid module name
directories that don't contain source files

  $ cat > dune-project <<EOF
  > (lang dune 3.22)
  > (using melange 1.0)
  > EOF

  $ cat > dune <<EOF
  > (library (name foo) (modes melange))
  > (ocamllex lexer)
  > EOF
  $ cat > lexer.mll <<EOF
  > {
  > }
  > rule lex = parse
  >   | _   { true  }
  >   | eof { false }
  > EOF
  $ dune build

Lexer ends up in the melange src

  $ find _build/default/.melange_src -type f | sort
  _build/default/.melange_src/foo.ml-gen
  _build/default/.melange_src/lexer.ml

Test the qualified case

  $ cat > dune <<EOF
  > (include_subdirs qualified)
  > (library (name foo) (modes melange))
  > EOF
  $ mkdir -p sub
  $ cat > sub/dune <<EOF
  > (ocamllex lexer)
  > EOF
  $ mv lexer.mll sub/
  $ dune build

  $ find _build/default/.melange_src -type f | sort
  _build/default/.melange_src/foo.ml-gen
  _build/default/.melange_src/foo__Sub.ml-gen
  _build/default/.melange_src/sub/lexer.ml
