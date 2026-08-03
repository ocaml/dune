----------------------------------------------------------------------------------
Test where a multiple include directories are added via a `(include ...)` statement

  $ make_dune_project 3.5

  $ make_foreign_stubs_include_dirs_project

  $ cat >bar.c <<EOF
  > #include <caml/mlvalues.h>
  > #include <a.h>
  > #include <b.h>
  > value bar(value unit) { return Val_int(A + B); }
  > EOF

  $ mkdir -p inc_a inc_b
  $ echo "#define A 40" > inc_a/a.h
  $ echo "#define B 2" > inc_b/b.h
  $ echo "(inc_a inc_b)" > foo

  $ dune build

