Test the ability of `(modules ..)` to contain dynamic forms such as
`(:include)` and variables such as `%{read-lines:...}` in the `ocamlyacc`
stanza.

  $ cat >dune-project <<EOF
  > (lang dune 3.22)
  > EOF

  $ mkdir -p gen

We define rules that create files (in the syntax expected by `modules`) that
each contain a single module name:

  $ cat >gen/dune <<EOF
  > (rule (with-stdout-to lst (echo my_parser)))
  > EOF

`.mly` unit present in the working tree. `lib.ml` references it

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

The `ocamlyacc` stanza uses `%{read-lines:..}` inside the `(modules ..)` field:

  $ cat >dune <<EOF
  > (library
  >  (name lib))
  > (ocamlyacc
  >  (modules %{read-lines:gen/lst}))
  > EOF

  $ dune build lib.cma
