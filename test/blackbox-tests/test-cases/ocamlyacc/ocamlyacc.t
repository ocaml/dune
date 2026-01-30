Test a library that uses a `ocamlyacc` parser.

  $ make_dune_project 3.21

  $ mkdir -p gen

  $ cat >my_parser.mly <<'EOF'
  > %token EOF
  > %start main
  > %type <unit> main
  > %%
  > main:
  >   | EOF { () }
  > EOF

  $ cat >lib.ml <<'EOF'
  > let _ = My_parser.main
  > EOF

  $ cat >dune <<EOF
  > (library
  >  (name lib))
  > (ocamlyacc
  >  (modules my_parser))
  > EOF

  $ dune build
