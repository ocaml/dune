Formatting dialects for parser inputs should coexist with generated OCaml files.

  $ make_menhir_project 3.22 3.0
  $ cat >>dune-project <<'EOF'
  > (formatting (enabled_for mly))
  > (dialect
  >  (name mly)
  >  (implementation
  >   (extension mly)
  >   (format (run cat %{input-file}))))
  > EOF
  $ cat >parser.mly <<'EOF'
  > %token EOF
  > %start main
  > %type <unit> main
  > %%
  > main:
  >   | EOF { () }
  > EOF
  $ cat >dune <<'EOF'
  > (menhir (modules parser))
  > (library (name parser))
  > EOF
  $ dune build parser.cma @fmt
  Error: Too many files for module Parser in .:
  - _build/default/parser.mly
  - _build/default/parser.ml
  [1]
  $ dune trace cat | jq_dune -c \
  >   'processesBrief | select(.prog == "cat") | .args'

The same applies to ocamlyacc.

  $ cat >dune <<'EOF'
  > (ocamlyacc parser)
  > (library (name parser))
  > EOF
  $ dune build parser.cma @fmt
  Error: Too many files for module Parser in .:
  - _build/default/parser.mly
  - _build/default/parser.ml
  [1]
