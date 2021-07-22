#!/bin/sh

cat <<EOF
prefix=$MY_PREFIX_DIR/libexample
exec_prefix=${prefix}/libexample
libdir=${prefix}/libexample
includedir=${prefix}/libexample
Name: libexample
Description: An example library for testing dune ctypes
Requires:
Version: 1.00.00
Libs: -L${prefix}/libexample -lexample
Cflags: -I${prefix}/libexample
EOF
