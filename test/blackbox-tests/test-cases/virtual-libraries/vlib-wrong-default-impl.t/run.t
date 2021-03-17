Check that dune makes a proper error if the default implementation of a virtual
library is not actually an implementation of the virtual library.

  $ dune build @default
  File "exe/dune", line 5, characters 0-49:
  5 | (rule
  6 |  (alias default)
  7 |  (action (run ./exe.exe)))
  Error: "not_an_implem" is not an implementation of "vlibfoo".
  Error: "not_an_implem" is not an implementation of "vlibfoo".
  -> required by executable exe in exe/dune:2
  [1]
