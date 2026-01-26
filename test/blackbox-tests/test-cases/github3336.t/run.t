Issue #3336 describes a bug where it's not possible to use dune_build_info from
ppx binaries.

Here we demonstrate that such a ppx .exe is built successfully.

  $ dune exec ./executable/exec.exe
  File "executable/dune", line 3, characters 13-22:
  3 |  (preprocess (pps ppx)))
                   ^^^^^^^^^
  Error: Rule failed to generate the following targets:
  - executable/exec.pp.ml
  [1]

  $ find _build | grep \.exe$
  _build/default/.ppx/0747fa92c6bc2f7f4818823e4f8871ab/ppx.exe
