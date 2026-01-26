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

  $ find _build/default | sort | grep lexer
  _build/default/lexer.ml
  _build/default/lexer.mll

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

  $ find _build/default | sort | grep lexer
  _build/default/sub/lexer.ml
  _build/default/sub/lexer.mll
