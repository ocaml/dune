  $ opam_prefix="$(opam var prefix)"
  $ export BUILD_PATH_PREFIX_MAP=\
  > "/OPAM_PREFIX=$opam_prefix:$BUILD_PATH_PREFIX_MAP"

In the following tests, the first line of the output should be different
depending on the compilation mode but not the second one: 

  $ dune exec ./stubs_exe.exe 
  Running[]: Byte (0) or native (1) ? 1
  Running[]: Byte and native (42) ? 42

  $ dune exec ./stubs_exe.bc.exe 
  Running[]: Byte (0) or native (1) ? 0
  Running[]: Byte and native (42) ? 42
