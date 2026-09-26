The private inference helper must not collide with an external module using
the path-based name introduced in #16464.

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
