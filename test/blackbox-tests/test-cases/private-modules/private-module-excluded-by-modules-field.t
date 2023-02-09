Demonstrate the behavior when a module is listed by private_modules by not by
modules:

  $ cat >dune-project <<EOF
  > (lang dune 3.7)
  > EOF

  $ cat >dune <<EOF
  > (library
  >  (name foo)
  >  (wrapped false)
  >  (modules y)
  >  (private_modules x))
  > EOF

  $ cat >x.ml <<EOF
  > let foo = ()
  > EOF

  $ cat >y.ml <<EOF
  > let () = X.foo ()
  > EOF

X is silently ignored:

  $ dune build
  File "y.ml", line 1, characters 9-14:
  1 | let () = X.foo ()
               ^^^^^
  Error: Unbound module X
  [1]
