Test that compile_commands.json includes -I flags for library dependencies.

  $ make_dune_project 3.23

Create a library that exposes C headers:

  $ mkdir -p provider
  $ cat >provider/dune <<EOF
  > (library
  >  (name provider)
  >  (install_c_headers api))
  > EOF
  $ cat >provider/api.h <<EOF
  > #define PROVIDER_VERSION 42
  > EOF
  $ cat >provider/provider.ml <<EOF
  > let version = 42
  > EOF

Create a library with foreign stubs that depends on provider:

  $ mkdir -p consumer
  $ cat >consumer/dune <<EOF
  > (library
  >  (name consumer)
  >  (libraries provider)
  >  (foreign_stubs (language c) (names stub)))
  > EOF
  $ cat >consumer/stub.c <<EOF
  > #include <caml/mlvalues.h>
  > #include "api.h"
  > value get_version(value unit) { return Val_int(PROVIDER_VERSION); }
  > EOF
  $ cat >consumer/consumer.ml <<EOF
  > external get_version : unit -> int = "get_version"
  > EOF

Build compile_commands.json:

  $ dune build compile_commands.json

Check that compile_commands.json includes -I for provider's headers:

  $ jq '.[] | select(.file == "stub.c") | .arguments | map(select(contains("provider")))' compile_commands.json
  [
    "../provider"
  ]
