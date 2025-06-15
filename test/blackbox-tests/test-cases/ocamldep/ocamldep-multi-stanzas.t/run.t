  $ dune exec ./test.exe --debug-dep
  File "dune", line 1, characters 0-0:
  Error: Module "Lib" is used in several stanzas:
  - dune:1
  - dune:5
  To fix this error, you must specify an explicit "modules" field in every
  library, executable, and executables stanzas in this dune file. Note that
  each module cannot appear in more than one "modules" field - it must belong
  to a single library or executable.
  [1]

  $ dune build src/a.cma --debug-dep
  File "src/dune", line 1, characters 0-0:
  Error: Module "X" is used in several stanzas:
  - src/dune:1
  - src/dune:2
  To fix this error, you must specify an explicit "modules" field in every
  library, executable, and executables stanzas in this dune file. Note that
  each module cannot appear in more than one "modules" field - it must belong
  to a single library or executable.
  [1]
