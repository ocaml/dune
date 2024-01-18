Some environment variables are automatically exported by packages:

  $ . ./helpers.sh

  $ make_lockdir
  $ echo "(version 0.0.1)" > dune.lock/test.pkg
  $ cat >dune.lock/usetest.pkg <<'EOF'
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
  $ MANPATH="" OCAMLPATH="" CAML_LD_LIBRARY_PATH="" OCAMLTOP_INCLUDE_PATH="" PATH="$PWD/.bin" build_pkg usetest
  MANPATH=$TESTCASE_ROOT/_build/_private/default/.pkg/test/target/man
  OCAMLPATH=$TESTCASE_ROOT/_build/_private/default/.pkg/test/target/lib
  CAML_LD_LIBRARY_PATH=$TESTCASE_ROOT/_build/_private/default/.pkg/test/target/lib/stublibs
  OCAMLTOP_INCLUDE_PATH=$TESTCASE_ROOT/_build/_private/default/.pkg/test/target/lib/toplevel
  PATH=$TESTCASE_ROOT/_build/_private/default/.pkg/test/target/bin:$TESTCASE_ROOT/.bin
