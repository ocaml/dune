Test `(include_subdirs qualified)` in the presence of invalid module name
directories that don't contain source files

  $ make_melange_project 3.22 1.0

  $ cat > dune <<EOF
  > (library (name foo) (modes melange))
  > (ocamllex lexer)
  > EOF
  $ make_trivial_ocamllex lexer.mll
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
