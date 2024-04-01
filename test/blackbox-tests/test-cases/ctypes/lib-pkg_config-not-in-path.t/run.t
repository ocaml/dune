Build an example library as a DLL and set up the environment so that it looks
like a system/distro library that can be probed with pkg-config and dynamically
loaded.

Then generate cstubs for it, build an executable that uses those cstubs, and
run the executable that tests the library through the cstubs.

First, scrub `pkg-config` from the environment, if present

  $ unset PKG_CONFIG_PATH
  $ pkg_config_bindir=$(dirname $(command -v pkg-config))
  $ export PATH=$(echo $PATH | sed 's#'"${pkg_config_bindir}"'##g')

This test tries a single function description stanza.

  $ LIBEX=$(realpath "$PWD/../libexample")
  $ C_INCLUDE_PATH="$LIBEX" DYLD_LIBRARY_PATH="$LIBEX" LD_LIBRARY_PATH="$LIBEX" dune exec ./example.exe
  File "stubgen/dune", line 1, characters 0-384:
   1 | (library
   2 |   (name examplelib)
   3 |   (flags (:standard -w -9-27))
  ....
  12 |       (instance Functions)
  13 |       (functor Function_description))
  14 |     (generated_entry_point C)))
  ld: library not found for -llibexample
  clang-16: error: linker command failed with exit code 1 (use -v to see invocation)
  [1]
