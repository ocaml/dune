This test tries to build a non-existent .cma.

  $ echo "(lang dune 2.1)" > dune-project
  $ cat > dune << EOF
  > (alias
  >  (name t)
  >  (deps %{cma:bar}))
  > EOF
  $ dune build @t
  File "dune", line 3, characters 7-17:
  3 |  (deps %{cma:bar}))
             ^^^^^^^^^^
  Error: Library bar does not exist.
  [1]
