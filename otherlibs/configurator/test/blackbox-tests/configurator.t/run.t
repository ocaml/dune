Show that config values are present
  $ dune exec config/run.exe
  INSIDE_DUNE is present
  .dune/configurator.v2 file is present
  version is present

We're able to compile C program successfully
  $ dune exec c_test/run.exe
  Successfully compiled c program

Importing #define's from code is successful
  $ dune exec import-define/run.exe
  CAML_CONFIG_H=true
  Page_log=12
  CONFIGURATOR_TESTING=foobar
  CONFIGURATOR_NEG_INT=-127
  sizeof(char)=1

Configurator.main requires Dune metadata. A direct invocation must not silently
fall back to an unrelated compiler configuration.

  $ env -u INSIDE_DUNE OCAMLRUNPARAM= _build/default/c_test/run.exe 2>outside-error
  [2]
  $ grep -o 'Configurator scripts must be run with Dune' outside-error
  Configurator scripts must be run with Dune
  $ INSIDE_DUNE=1 OCAMLRUNPARAM= _build/default/c_test/run.exe 2>old-dune-error
  [2]
  $ grep -o 'running dune < 2.3' old-dune-error
  running dune < 2.3

Missing and malformed metadata produce useful errors before any C compilation.

  $ mkdir -p fake-build/.dune
  $ INSIDE_DUNE="$PWD/fake-build" _build/default/c_test/run.exe
  Error: You seem to be running dune < 2.6. This version of dune-configurator requires at least dune 2.6.
  [1]
  $ printf '4:atom' > fake-build/.dune/configurator.v2
  $ INSIDE_DUNE="$PWD/fake-build" _build/default/c_test/run.exe
  Error: Unable to parse "$TESTCASE_ROOT/fake-build/.dune/configurator.v2".@.unexpected atom@.
  [1]
  $ printf '()' > fake-build/.dune/configurator.v2
  $ INSIDE_DUNE="$PWD/fake-build" _build/default/c_test/run.exe
  Error: unable to find field "ocamlc"
  [1]
  $ printf '((6:ocamlc()))' > fake-build/.dune/configurator.v2
  $ INSIDE_DUNE="$PWD/fake-build" _build/default/c_test/run.exe
  Error: invalid ocamlc field
  [1]
  $ printf '((6:ocamlc6:ocamlc)(17:ocaml_config_vars()))' \
  >   > fake-build/.dune/configurator.v2
  $ INSIDE_DUNE="$PWD/fake-build" _build/default/c_test/run.exe
  Error: variable "bytecomp_c_libraries" not found in the output of `ocamlc -config`
  [1]
