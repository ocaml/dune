  $ opam_prefix="$(opam var prefix)"
  $ export BUILD_PATH_PREFIX_MAP=\
  > "/OPAM_PREFIX=$opam_prefix:$BUILD_PATH_PREFIX_MAP"

When building native executable, the output should be: 
# Running[]: Byte (0) or native (1) ? 1
  $ dune exec ./stubs_exe.exe 
  Running[]: Byte (0) or native (1) ? 1
  Running[]: Byte (0) or native (1) ? 42


When building bytecode executable, the output should be: 
# Running[]: Byte (0) or native (1) ? 0
$ dune clean
  $ dune exec ./stubs_exe.bc.exe 
  Running[]: Byte (0) or native (1) ? 0
  Running[]: Byte (0) or native (1) ? 42
