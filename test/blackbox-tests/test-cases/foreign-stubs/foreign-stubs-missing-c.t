----------------------------------------------------------------------------------

* Error when a C source file is missing.

  $ ./sandboxed.sh
  $ echo "(lang dune 2.0)" > dune-project

  $ cat >dune <<EOF
  > (library
  >  (name foo)
  >  (foreign_stubs (language c) (names foo))
  >  (foreign_archives bar))
  > EOF

  $ dune build
  File "dune", line 3, characters 36-39:
  3 |  (foreign_stubs (language c) (names foo))
                                          ^^^
  Error: Object "foo" has no source; "foo.c" must be present.
  [1]

----------------------------------------------------------------------------------
* Error when a self-built archive is missing.

  $ cat >foo.c <<EOF
  > #include <caml/mlvalues.h>
  > value foo(value unit) { return Val_int(9); }
  > EOF

  $ dune build 2>&1 | dune_cmd sanitize
  File "dune", lines 1-4, characters 0-87:
  1 | (library
  2 |  (name foo)
  3 |  (foreign_stubs (language c) (names foo))
  4 |  (foreign_archives bar))
  Error: No rule found for libbar$ext_lib

----------------------------------------------------------------------------------
* Build succeeds when a self-built archive exists.

  $ cat >bar.c <<EOF
  > #include <caml/mlvalues.h>
  > value bar(value unit) { return Val_int(10); }
  > EOF

  $ cat >dune <<EOF
  > (library
  >  (name foo)
  >  (foreign_stubs (language c) (names foo))
  >  (foreign_archives bar))
  > (rule
  >  (targets bar%{ext_obj})
  >  (deps bar.c)
  >  (action (run %{ocaml-config:c_compiler} -c -I %{ocaml-config:standard_library} -o %{targets} %{deps})))
  > (rule
  >  (targets libbar.a)
  >  (deps bar%{ext_obj})
  >  (action (run ar rcs %{targets} %{deps})))
  > EOF

  $ dune build
