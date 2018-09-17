Various tests for optional libraries
------------------------------------

Regression test for non-wrapped optional libraries with missing
dependencies (#1281):

  $ dune build
  File "dune", line 2, characters 7-10:
  2 |  (name foo)
             ^^^
  Error: Library "foo" in _build/default is hidden (optional with unavailable dependencies).
  Hint: try: dune external-lib-deps --missing @@default
  [1]
