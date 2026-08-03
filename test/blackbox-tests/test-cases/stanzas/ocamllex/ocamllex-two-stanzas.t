Test building 2 ocamllex stanzas in the same group, by 2 different stanzas 
(library + executable)

  $ make_dune_project 3.21
  $ cat > dune <<EOF
  > (library (name foo) (modules foo foo_lex))
  > (executable (name bar) (modules bar bar_lex))
  > (ocamllex (modules bar_lex foo_lex))
  > EOF
  $ make_trivial_ocamllex bar_lex.mll

  $ make_trivial_ocamllex foo_lex.mll

  $ cat > foo.ml<<EOF
  > let x = Foo_lex.lex
  > EOF

  $ cat > bar.ml<<EOF
  > let x = Bar_lex.lex
  > EOF

  $ dune build
