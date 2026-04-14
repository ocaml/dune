%{lib:...} should be usable in (flags) and other pform positions
(#3362). This previously produced "isn't allowed in this position".

  $ cat > dune-project <<EOF
  > (lang dune 3.23)
  > (name mypkg)
  > (package (name mypkg) (allow_empty))
  > EOF

Create a public library that installs a C header:

  $ mkdir -p mylib
  $ cat > mylib/myheader.h <<EOF
  > #define MY_VALUE 42
  > EOF

  $ cat > mylib/dune <<EOF
  > (library
  >  (public_name mypkg.mylib)
  >  (name mylib)
  >  (install_c_headers myheader))
  > EOF

  $ cat > mylib/mylib.ml <<EOF
  > let greeting = "hello"
  > EOF

Test 1: %{lib:...} in a rule action expands correctly:

  $ cat > dune <<EOF
  > (rule
  >  (target out.txt)
  >  (action (with-stdout-to %{target} (echo %{lib:mypkg.mylib:mylib.ml}))))
  > EOF

  $ dune build out.txt
  $ cat _build/default/out.txt
  ../install/default/lib/mypkg/mylib/mylib.ml

Test 2: %{lib:...} in (c_library_flags) to pass a C include path,
matching the original use case from #3362. The library's installed
header directory is referenced via %{lib:...}:

  $ cat > stub.c <<EOF
  > #include <caml/mlvalues.h>
  > #include "myheader.h"
  > CAMLprim value get_value(value unit) { return Val_int(MY_VALUE); }
  > EOF

  $ cat > dune <<EOF
  > (library
  >  (name consumer)
  >  (libraries mypkg.mylib)
  >  (foreign_stubs (language c) (names stub))
  >  (c_library_flags -I%{lib:mypkg.mylib:myheader.h}))
  > EOF

  $ cat > consumer.ml <<EOF
  > external get_value : unit -> int = "get_value"
  > EOF

  $ dune build
