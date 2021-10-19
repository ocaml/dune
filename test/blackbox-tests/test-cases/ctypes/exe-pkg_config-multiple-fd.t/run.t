Build an example library as a DLL and set up the environment so that it looks
like a system/distro library that can be probed with pkg-config and dynamically
loaded.

Then generate cstubs for it, build an executable that uses those cstubs, and
run the executable that tests the library through the cstubs.

This test also tests multiple function description modules.

  $ cd libexample
  $ make -s -f Makefile.unix
  $ cd ..

  $ cat >libexample.pc <<EOF
  > prefix=$PWD/libexample
  > exec_prefix=$PWD/libexample
  > libdir=$PWD/libexample
  > includedir=$PWD/libexample
  > Name: libexample
  > Description: An example library for testing dune ctypes
  > Requires:
  > Version: 1.00.00
  > Libs: -L$PWD/libexample -lexample
  > Cflags: -I$PWD/libexample
  > EOF

  $ LD_LIBRARY_PATH="$PWD/libexample" PKG_CONFIG_PATH="$PKG_CONFIG_PATH:$PWD" dune exec ./example.exe
  6
