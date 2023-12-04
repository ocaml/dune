----------------------------------------------------------------------------------
Test where a single include directory is added via a `(include ...)` statement

----------------------------------------------------------------------------------
* Versions of dune before 3.5 do not support this feature

  $ echo "(lang dune 3.4)" > dune-project
  $ cat >dune <<EOF
  > (library
  >  (name foo)
  >  (foreign_stubs
  >   (language c)
  >   (names bar)
  >   (include_dirs (include foo))))
  > EOF
  $ dune build
  File "dune", line 6, characters 16-29:
  6 |   (include_dirs (include foo))))
                      ^^^^^^^^^^^^^
  Error: 'include' is only available since version 3.5 of the dune language.
  Please update your dune-project file to have (lang dune 3.5).
  [1]

----------------------------------------------------------------------------------
* Error if include file is missing

  $ echo "(lang dune 3.5)" > dune-project

  $ cat >bar.c <<EOF
  > #include <caml/mlvalues.h>
  > #include <a.h>
  > value bar(value unit) { return Val_int(A); }
  > EOF

  $ dune build
  File "dune", line 5, characters 9-12:
  5 |   (names bar)
               ^^^
  Error: No rule found for foo
  [1]

----------------------------------------------------------------------------------
* Simple example

  $ mkdir -p inc_a
  $ echo "#define A 42" > inc_a/a.h
  $ echo inc_a > foo

  $ dune build

