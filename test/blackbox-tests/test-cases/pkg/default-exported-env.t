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
  $ MANPATH="" OCAMLPATH="" CAML_LD_LIBRARY_PATH="" OCAMLTOP_INCLUDE_PATH="" PATH="$PWD/.bin" build_pkg usetest
  MANPATH=$TESTCASE_ROOT/_build/_private/default/.pkg/test.0.0.1-58e701a6bb554b1906e02d898bc78509/target/man
  OCAMLPATH=$TESTCASE_ROOT/_build/_private/default/.pkg/test.0.0.1-58e701a6bb554b1906e02d898bc78509/target/lib
  CAML_LD_LIBRARY_PATH=$TESTCASE_ROOT/_build/_private/default/.pkg/test.0.0.1-58e701a6bb554b1906e02d898bc78509/target/lib/stublibs
  OCAMLTOP_INCLUDE_PATH=$TESTCASE_ROOT/_build/_private/default/.pkg/test.0.0.1-58e701a6bb554b1906e02d898bc78509/target/lib/toplevel
  PATH=$TESTCASE_ROOT/_build/_private/default/.pkg/test.0.0.1-58e701a6bb554b1906e02d898bc78509/target/bin:$TESTCASE_ROOT/.bin
