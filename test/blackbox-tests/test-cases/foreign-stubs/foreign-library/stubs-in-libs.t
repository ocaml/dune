Make sure that foreign stubs referencing code in foreign libs works too.

  $ setup_foreign_library_project

  $ mkdir -p stubs_in_libs
  $ cat >stubs_in_libs/dune-project <<EOF
  > (lang dune 2.5)
  > EOF
  $ cat >stubs_in_libs/lib.h <<EOF
  > extern int foo(void);
  > EOF
  $ cat >stubs_in_libs/lib.c <<EOF
  > int foo(void) { return 12; }
  > EOF
  $ cat >stubs_in_libs/stubs.c <<EOF
  > #include <caml/mlvalues.h>
  > #include "lib.h"
  > value bar(value unit) { return Val_int(foo()); }
  > EOF
  $ cat >stubs_in_libs/main.ml <<EOF
  > external bar: unit -> int = "bar" [@@noalloc]
  > let () = print_endline (string_of_int (bar ()))
  > EOF
  $ cat >stubs_in_libs/dune <<EOF
  > (foreign_library
  >  (archive_name lib)
  >  (language c)
  >  (names lib))
  > 
  > (executable
  >  (name main)
  >  (foreign_archives lib)
  >  (foreign_stubs (language c) (names stubs)))
  > EOF
  $ dune build --root stubs_in_libs
  $ stubs_in_libs/_build/default/main.exe
  12
