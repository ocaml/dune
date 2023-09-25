Bind to a C library which has a name invalid in ocaml

  $ LIBEX=$(realpath "$PWD/../libneed-mangling")

  $ DYLD_LIBRARY_PATH="$LIBEX" LD_LIBRARY_PATH="$LIBEX"  PKG_CONFIG_PATH="$LIBEX/pkgconfig" PKG_CONFIG_ARGN="--define-prefix"  dune exec ./example.exe
  4
