Bind to a C library which has a name invalid in ocaml

  $ LIBEX=$(realpath "$PWD/../libneed-mangling")

This silly looking hack is to make sure the .pc file points to the sandbox. We
cannot set ${prefix} to be interpreted relative to the .pc itself ufortunately
  $ awk "BEGIN{print \"prefix=$LIBEX\"} {print}" $LIBEX/need-mangling.pc > need-mangling.pc

  $ DYLD_LIBRARY_PATH="$LIBEX" LD_LIBRARY_PATH="$LIBEX" PKG_CONFIG_PATH="$PWD:$PKG_CONFIG_PATH" dune exec ./example.exe
  4
