----------------------------------------------------------------------------------
Detect loops of `(include ...)` statements

  $ make_dune_project 3.5

  $ make_foreign_stubs_include_dirs_project

  $ cat >bar.c <<EOF
  > #include <caml/mlvalues.h>
  > #include <a.h>
  > #include <b.h>
  > value bar(value unit) { return Val_int(A + B); }
  > EOF

  $ echo "((include baz))" > foo
  $ echo "((include foo))" > baz

  $ dune build
  File "_build/default/baz", line 1, characters 10-13:
  1 | ((include foo))
                ^^^
  Error: Include loop detected via: _build/default/foo
  [1]
