Build an example library as a DLL and set up the environment so that it looks
like a system/distro library that can be probed with pkg-config and dynamically
loaded.

Then generate cstubs for it, build an executable that uses those cstubs, and
run the executable that tests the library through the cstubs.

  $ LIBEX=$(realpath "$PWD/../libexample")
  $ DYLD_LIBRARY_PATH="$LIBEX" LD_LIBRARY_PATH="$LIBEX" PKG_CONFIG_PATH="$LIBEX/pkgconfig" PKG_CONFIG_ARGN="--define-prefix" dune exec ./example.exe
  File "dune", line 14, characters 38-46:
  14 |     "#include <example.h>\n#include \"%{foo_h}\"\n#include \"%{bar_h}\"\n#include \"baz.h\"\n#include \"%{dep:qux.h}\""))
                                             ^^^^^^^^
  Error: %{foo_h} isn't allowed in this position.
  File "dune", line 14, characters 61-69:
  14 |     "#include <example.h>\n#include \"%{foo_h}\"\n#include \"%{bar_h}\"\n#include \"baz.h\"\n#include \"%{dep:qux.h}\""))
                                                                    ^^^^^^^^
  Error: %{bar_h} isn't allowed in this position.
  [1]
