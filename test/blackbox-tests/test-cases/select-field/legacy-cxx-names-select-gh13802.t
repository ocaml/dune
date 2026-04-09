Legacy `cxx_names` should keep working with non-OCaml `(select ..)` targets.

  $ cat >dune-project <<EOF
  > (lang dune 1.0)
  > EOF

  $ cat >dune <<EOF
  > (library
  >  (name test)
  >  (modules test)
  >  (libraries
  >   (select config.h from
  >    (missing_backend -> config_backend.h)
  >    (-> config_fallback.h)))
  >  (cxx_names stubs))
  > EOF

  $ cat >test.ml <<EOF
  > external value : unit -> int = "caml_test_value"
  > EOF

  $ cat >stubs.cpp <<EOF
  > #include <caml/mlvalues.h>
  > #include "config.h"
  > extern "C" CAMLprim value caml_test_value(value unit) {
  >   return Val_int(CONFIG_VALUE);
  > }
  > EOF

  $ cat >config_backend.h <<EOF
  > #define CONFIG_VALUE 42
  > EOF

  $ cat >config_fallback.h <<EOF
  > #define CONFIG_VALUE 7
  > EOF

  $ cat >main.ml <<EOF
  > let () = Printf.printf "%d\n" (Test.value ())
  > EOF

  $ cat >>dune <<EOF
  > (executable
  >  (name main)
  >  (modules main)
  >  (libraries test))
  > EOF

  $ dune exec ./main.exe
  7
