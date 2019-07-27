Test optional executable

  $ dune build @install

  $ dune build @run-x
  File "dune", line 3, characters 12-26:
  3 |  (libraries does-not-exist)
                  ^^^^^^^^^^^^^^
  Error: Library "does-not-exist" not found.
  Hint: try: dune external-lib-deps --missing @run-x
  [1]
