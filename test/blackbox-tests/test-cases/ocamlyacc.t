Test a library that uses a `ocamlyacc` parser.

  $ cat >dune-project <<EOF
  > (lang dune 3.21)
  > EOF

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
