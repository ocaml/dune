Build an example library as a DLL and set up the environment so that it looks
like a system/distro library that can be probed with pkg-config and dynamically
loaded.

Then generate cstubs for it, build an executable that uses those cstubs, and
run the executable that tests the library through the cstubs.

This test tries a single function description stanza.

  $ cd stubgen/libexample
  $ make -s -f Makefile.unix
  $ cd ../..

  $ cat >libexample.pc <<EOF
  > prefix=$PWD/stubgen/libexample
  > exec_prefix=$PWD/stubgen/libexample
  > libdir=$PWD/stubgen/libexample
  > includedir=$PWD/stubgen/libexample
  > Name: libexample
  > Description: An example library for testing dune ctypes
  > Requires:
  > Version: 1.00.00
  > Libs: -L$PWD/stubgen/libexample -lexample
  > Cflags: -I$PWD/stubgen/libexample
  > EOF

  $ LD_LIBRARY_PATH="$PWD/stubgen/libexample" PKG_CONFIG_PATH="$PWD:$PKG_CONFIG_PATH" dune exec ./example.exe
  4
