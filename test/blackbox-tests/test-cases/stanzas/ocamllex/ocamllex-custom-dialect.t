An ocamllex input normally builds as a single generated module.

  $ make_dune_project 3.22
  $ make_trivial_ocamllex lexer.mll
  $ cat >dune <<'EOF'
  > (ocamllex lexer)
  > (library (name lexer))
  > EOF
  $ dune build lexer.cma

Registering the input's extension as an implementation dialect must not make
it a second implementation, even without a formatter.

  $ echo '(dialect (name mll) (implementation (extension mll)))' >>dune-project
  $ dune build lexer.cma

The same input should remain available for formatting.

  $ cat >dune-project <<'EOF'
  > (lang dune 3.22)
  > (formatting (enabled_for mll))
  > (dialect
  >  (name mll)
  >  (implementation
  >   (extension mll)
  >   (format (run cat %{input-file}))))
  > EOF
  $ dune build lexer.cma @fmt
  $ dune trace cat | jq_dune -c \
  >   'processesBrief | select(.prog == "cat") | .args'
  ["./lexer.mll"]
