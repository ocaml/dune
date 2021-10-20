Build an example library as a DLL and set up the environment so that it looks
like a system/distro library that can be probed with pkg-config and dynamically
loaded.

Then generate cstubs for it, build an executable that uses those cstubs, and
run the executable that tests the library through the cstubs.

This test also tests multiple function description modules.

  $ LIBEX=$(realpath "$PWD/../libexample")

This silly looking hack is to make sure the .pc file points to the sandbox. We
cannot set ${prefix} to be interpreted relative to the .pc itself ufortunately
  $ awk "BEGIN{print \"prefix=$LIBEX\"} {print}" $LIBEX/libexample.pc > libexample.pc

  $ DYLD_LIBRARY_PATH="$LIBEX" LD_LIBRARY_PATH="$LIBEX" PKG_CONFIG_PATH="$PKG_CONFIG_PATH:$PWD" dune exec ./example.exe
  6
