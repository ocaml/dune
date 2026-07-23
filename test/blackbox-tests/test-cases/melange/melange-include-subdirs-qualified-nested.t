Test Melange compilation for a `(include_subdirs qualified)` stanza appearing
in a nested subdirectory.

  $ make_melange_project 3.22 1.0

  $ mkdir -p a/b/c

  $ cat > a/dune <<EOF
  > (include_subdirs qualified)
  > (library (name foo) (modes melange))
  > EOF

  $ cat > a/b/c/dune <<EOF
  > (ocamllex lexer)
  > EOF
  $ make_trivial_ocamllex a/b/c/lexer.mll

  $ cat > a/foo.ml <<EOF
  > module L = B.C.Lexer
  > EOF


  $ dune build
  $ find _build/default/a/.melange_src | sort
  _build/default/a/.melange_src
  _build/default/a/.melange_src/b
  _build/default/a/.melange_src/b/c
  _build/default/a/.melange_src/b/c/lexer.ml
  _build/default/a/.melange_src/foo.ml
  _build/default/a/.melange_src/foo__.ml-gen
  _build/default/a/.melange_src/foo__B.ml-gen
  _build/default/a/.melange_src/foo__B__C.ml-gen
