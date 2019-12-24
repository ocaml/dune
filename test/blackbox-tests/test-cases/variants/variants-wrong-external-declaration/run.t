External declaration of an implementation, when the given library is not an
implementation, should fail.

  $ dune build exe/exe.exe
  Error: "not-an-implementation" is not an implementation of "vlibfoo-ext".
  -> required by executable exe in exe/dune:2
  [1]
