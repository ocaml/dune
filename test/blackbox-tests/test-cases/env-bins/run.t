Basic test that we can use private binaries as public ones
  $ dune build --root private-bin-import
  Entering directory 'private-bin-import'
  File "using-priv/dune", line 11, characters 8-16:
  11 |    (run priv.exe)
               ^^^^^^^^
  Error: Error: Program priv.exe not found in the tree or in PATH (context: default)
  [1]

Override public binary in env
  $ dune build --root override-bins
  Entering directory 'override-bins'
           foo alias test/runtest
  public binary
           foo alias default
  public binary
