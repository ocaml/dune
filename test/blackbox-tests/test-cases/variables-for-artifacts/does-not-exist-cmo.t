The next test tries to build a module that does not exist.

  $ echo "(lang dune 2.1)" > dune-project
  $ cat > dune << EOF
  > (alias
  >  (name t)
  >  (deps %{cmo:foo}))
  > EOF
  $ dune build @t
  File "dune", line 3, characters 7-17:
  3 |  (deps %{cmo:foo}))
             ^^^^^^^^^^
  Error: Module Foo does not exist.
  [1]

