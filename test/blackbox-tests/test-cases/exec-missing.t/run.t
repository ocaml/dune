When using dune exec, the external-lib-deps command refers to the executable:

  $ dune exec ./x.exe
  File "dune", line 3, characters 12-26:
  3 |  (libraries does-not-exist))
                  ^^^^^^^^^^^^^^
  Error: Library "does-not-exist" not found.
  -> required by _build/default/x.exe
  [1]
