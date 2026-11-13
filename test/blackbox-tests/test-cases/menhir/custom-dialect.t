Reproduce the stanc3 regression in Dune 3.25: a dialect used to configure
formatting makes a parser generator's input conflict with its output.

  $ make_menhir_project 3.22 2.0
  $ cat >>dune-project <<'EOF'
  > (dialect
  >  (name menhir)
  >  (implementation
  >   (extension mly)
  >   (format (cat %{input-file}))))
  > EOF
  $ cat >dune <<'EOF'
  > (library (name frontend))
  > (menhir (modules parser))
  > EOF
  $ cat >parser.mly <<'EOF'
  > %token EOF
  > %start <unit> main
  > %%
  > main: EOF { () }
  > EOF

  $ dune build frontend.cma
  Error: Too many files for module Parser in .:
  - _build/default/parser.mly
  - _build/default/parser.ml
  [1]

The same regression affects ocamllex inputs with a formatting dialect.

  $ make_menhir_project 3.22 2.0
  $ cat >>dune-project <<'EOF'
  > (dialect
  >  (name ocamllex)
  >  (implementation
  >   (extension mll)
  >   (format (cat %{input-file}))))
  > EOF
  $ echo '(ocamllex lexer)' >>dune
  $ cat >lexer.mll <<'EOF'
  > rule token = parse
  > | eof { Parser.EOF }
  > EOF
  $ dune build frontend.cma
  Error: Too many files for module Lexer in .:
  - _build/default/lexer.mll
  - _build/default/lexer.ml
  [1]
