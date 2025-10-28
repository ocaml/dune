Test for issue https://github.com/ocaml/dune/issues/12636

  $ cat > dune-project <<EOF
  > (lang dune 3.21)
  > EOF

  $ cat > dune <<EOF
  > (library
  >  (name test)
  >  (virtual_modules a))
  > EOF

  $ cat > a.mli <<EOF
  > val compute : int -> int
  > EOF

  $ cat > test.ml <<EOF
  > let run x = A.compute x * 2
  > EOF

First, test that regular build works:
  $ dune build

Now test @check which should work but crashes:
  $ dune build @check 2>&1 | grep "Internal"
  Internal error, please report upstream including the contents of _build/log.
