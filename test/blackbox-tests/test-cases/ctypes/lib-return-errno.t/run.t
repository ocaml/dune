Generate cstubs for an example executable exercising the `return_errno` and the
default `ignore_errno` errno policies, build an executable, and run it.

  $ LIBEX=$(realpath "$PWD/../libexample")
  $ awk "BEGIN{print \"prefix=$LIBEX\"} 1" $LIBEX/libexample.pc > libexample.pc
  $ DYLD_LIBRARY_PATH="$LIBEX" LD_LIBRARY_PATH="$LIBEX" PKG_CONFIG_PATH="$PWD:$PKG_CONIG_PATH" dune exec ./example.exe
  6
