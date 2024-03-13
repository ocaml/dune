Build an example library as a DLL and set up the environment so that it looks
like a system/distro library that can be probed with pkg-config and dynamically
loaded.

  $ LIBEX=$(realpath "$PWD/../libexample")

ocamlrun + requires CAML_LD_LIBRARY_PATH such that dlopen system call can find
dllexamplelib_stub.so

Explictly set {DYLD,LD}_LIBRARY_PATH at runtime for this testcase, otherwise
dlopen cannot find libexample, after loading dllexamplelib_stub.so

  $  PKG_CONFIG_PATH="$LIBEX/pkgconfig" PKG_CONFIG_ARGN="--define-prefix" DYLD_LIBRARY_PATH="$DYLD_LIBRARY_PATH:$LIBEX" LD_LIBRARY_PATH="$LD_LIBRARY_PATH:$LIBEX" CAML_LD_LIBRARY_PATH="$CAML_LD_LIBRARY_PATH:$PWD/_build/default/stubgen" dune exec ./example.bc
  4

Utop works with ctypes pkg-config external library.

  $   DYLD_LIBRARY_PATH="$DYLD_LIBRARY_PATH:$LIBEX" LD_LIBRARY_PATH="$LD_LIBRARY_PATH:$LIBEX"  PKG_CONFIG_PATH="$LIBEX/pkgconfig" PKG_CONFIG_ARGN="--define-prefix" dune utop --display=short ./ -- example.ml
    pkg-config stubgen/.pkg-config/libexample.cflags
    pkg-config stubgen/.pkg-config/libexample.libs
        ocamlc .utop/.utop.eobjs/byte/dune__exe__Utop.{cmi,cmo,cmt}
        ocamlc .utop/utop.bc
  4
