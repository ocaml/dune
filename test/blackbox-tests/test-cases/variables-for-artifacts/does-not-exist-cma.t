This test tries to build a non-existent .cma.

  $ make_dune_project 2.1
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
