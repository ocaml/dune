Builds `ocamllex` generators under `(include_subdirs unqualified)`.


  $ make_dune_project 3.21
  $ mkdir -p lib/foo
  $ cat > lib/dune <<EOF
  > (include_subdirs unqualified)
  > (library (name foo))
  > EOF

  $ cat > lib/foo/dune <<EOF
  > (ocamllex lexer)
  > EOF
  $ cat > lib/foo.ml <<EOF
  > let x = Lexer.lex
  > EOF
  $ make_trivial_ocamllex lib/foo/lexer.mll
  $ dune build
