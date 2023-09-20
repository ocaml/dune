Build an example library as a DLL and set up the environment so that it looks
like a system/distro library that can be probed with pkg-config and dynamically
loaded.

Then generate cstubs for it, build an executable that uses those cstubs, and
run the executable that tests the library through the cstubs.

Setup project with mdx,ctypes and 

  $ LIBEX=$(realpath "$PWD/../libexample")

This silly looking hack is to make sure the .pc file points to the sandbox. We
cannot set ${prefix} to be interpreted relative to the .pc itself ufortunately

  $ awk "BEGIN{print \"prefix=$LIBEX\"} {print}" $LIBEX/libexample.pc > libexample.pc
 
  $ export DYLD_LIBRARY_PATH="$LIBEX:$PWD/_build/default/stubgen/"
  $ export LD_LIBRARY_PATH="$LIBEX:$PWD/_build/default/stubgen/"
  $ export PKG_CONFIG_PATH="$PKG_CONFIG_PATH:$PWD"

Execute native linked binary

  $ dune exec ./example.exe
  4

Execute example.bc (Broken need paths)

  $ dune exec ./example.bc
  Fatal error: cannot load shared library dllexamplelib_stubs
  Reason: dlopen(dllexamplelib_stubs.so, 0x000A): symbol not found in flat namespace '_example_add2'
  71644 Abort trap: 6           dune exec ./example.bc
  [134]
 

Execute UTOP


  $ dune utop ./ -- example.ml 
  Fatal error: cannot load shared library dllexamplelib_stubs
  Reason: dlopen($TESTCASE_ROOT/_build/default/stubgen/dllexamplelib_stubs.so, 0x000A): symbol not found in flat namespace '_example_add2'
  71633 Abort trap: 6           dune utop ./ -- example.ml
  [134]


Locate dllstubs files

  $ dune install
 
  $ find . -name "*example*so" 
  ./_build/default/stubgen/dllexamplelib_stubs.so
  $ find $LIBEX -name "*example*so" 
  /workspace_root/test/blackbox-tests/test-cases/ctypes/libexample/libexample.so
 
  $ dune runtest --always-show-command-line
  File "dune", line 6, characters 0-48:
  6 | (mdx
  7 |  (files README.md)
  8 |  (libraries examplelib))
  (cd _build/default && ./mdx_gen.bc README.md) > _build/default/.mdx/README.md.corrected
  Fatal error: cannot load shared library dllexamplelib_stubs
  Reason: dlopen(dllexamplelib_stubs.so, 0x000A): symbol not found in flat namespace '_example_add2'
  [1]



 $ echo $LIBEX



