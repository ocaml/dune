Some environment variables are automatically exported by packages:

  $ make_lockdir
  $ make_lockpkg test <<EOF
  > (version 0.0.1)
  > EOF
  $ make_lockpkg usetest <<'EOF'
  > (version 0.0.1)
  > (depends test)
  > (build
  >  (system "\| echo MANPATH=$MANPATH
  >          "\| echo OCAMLPATH=$OCAMLPATH
  >          "\| echo CAML_LD_LIBRARY_PATH=$CAML_LD_LIBRARY_PATH
  >          "\| echo OCAMLTOP_INCLUDE_PATH=$OCAMLTOP_INCLUDE_PATH
  >          "\| echo PATH=$PATH
  >  ))
  > EOF

  $ mkdir .bin
  $ ln -s $(which ocamlc) .bin/ocamlc
  $ ln -s $(which sh) .bin/sh
  $ dune=$(which dune)
  $ MANPATH="" OCAMLPATH="" CAML_LD_LIBRARY_PATH="" OCAMLTOP_INCLUDE_PATH="" PATH="$PWD/.bin" build_pkg usetest 2>&1 | strip_sandbox | censor
  $SANDBOX/default/test/blackbox-tests/test-cases/pkg/_build/_private/default/.pkg/test.0.0.1-$DIGEST/target/man
  $SANDBOX/default/test/blackbox-tests/test-cases/pkg/_build/_private/default/.pkg/test.0.0.1-$DIGEST/target/lib
  $SANDBOX/default/test/blackbox-tests/test-cases/pkg/_build/_private/default/.pkg/test.0.0.1-$DIGEST/target/lib/stublibs
  $SANDBOX/default/test/blackbox-tests/test-cases/pkg/_build/_private/default/.pkg/test.0.0.1-$DIGEST/target/lib/toplevel
  $SANDBOX/default/test/blackbox-tests/test-cases/pkg/.bin
