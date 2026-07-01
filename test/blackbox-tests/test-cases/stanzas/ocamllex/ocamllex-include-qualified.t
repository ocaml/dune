Builds `ocamllex` generators under `(include_subdirs qualified)`.

  $ make_dune_project 3.21
  $ mkdir -p lib/bar
  $ cat > lib/dune <<EOF
  > (include_subdirs qualified)
  > (library (name foo))
  > EOF

  $ cat > lib/foo.ml <<EOF
  > let x = Bar.Lexer.lex
  > EOF

  $ cat > lib/bar/dune <<EOF
  > (ocamllex lexer)
  > EOF

  $ make_trivial_ocamllex lib/bar/lexer.mll

  $ dune build
