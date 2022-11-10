Path that needs to be searched:

  $ dune runtest
  File "dune", line 3, characters 14-32:
  3 |  (action (run foo-does-not-exist)))
                    ^^^^^^^^^^^^^^^^^^
  Error: Program foo-does-not-exist not found in the tree or in PATH
   (context: default)
  [1]
