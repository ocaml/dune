The @check alias should detect dependency cycles

  $ cat >dune-project <<EOF
  > (lang dune 3.2)
  > EOF

  $ cat >dune <<EOF
  > (library (name foo))
  > EOF

  $ cat >a.ml <<EOF
  > module B = B
  > EOF

  $ touch b.ml b.mli a.mli

  $ dune build @all

  $ cat >b.ml <<EOF
  > module A = A
  > EOF

  $ dune build @check
  $ dune build @all
  Error: dependency cycle between modules in _build/default:
     B
  -> A
  -> B
  -> required by _build/default/foo.cma
  -> required by alias all
  [1]
