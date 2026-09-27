The private inference helper proposed in #16464 must not shadow a module with
the same name from another library.

Currently, inference opens the external module instead of the helper, so the
aliases needed by the parser are missing.

  $ make_menhir_project 3.25 3.0

  $ mkdir -p ext lib/lang
  $ cat >ext/dune <<'EOF'
  > (library
  >  (name ext)
  >  (wrapped false))
  > EOF
  $ echo 'let value = 2' >ext/dune__menhir__Lang__Parser__mock.ml

  $ cat >lib/dune <<'EOF'
  > (include_subdirs qualified)
  > (library
  >  (name foo)
  >  (libraries ext))
  > EOF
  $ echo 'module Parser = Parser' >lib/lang/lang.ml
  $ echo 'let value = 1' >lib/lang/atom.ml
  $ cat >lib/lang/dune <<'EOF'
  > (menhir
  >  (modules parser))
  > EOF
  $ cat >lib/lang/parser.mly <<'EOF'
  > %token EOF
  > %start <int> main
  > %%
  > main: EOF { Atom.value + Dune__menhir__Lang__Parser__mock.value }
  > EOF

  $ dune build lib/foo.cma
  File "lib/lang/parser.mly", line 4, characters 12-16:
  Error: Unbound module Atom
  [1]
