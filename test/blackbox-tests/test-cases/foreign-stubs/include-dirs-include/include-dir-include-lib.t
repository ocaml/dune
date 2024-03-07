----------------------------------------------------------------------------------
Test use of `(lib ...)` statements inside a file included with `(include ...)`

  $ echo "(lang dune 3.5)" > dune-project

  $ cat >dune <<EOF
  > (library
  >  (name foo)
  >  (foreign_stubs
  >   (language c)
  >   (names bar)
  >   (include_dirs (include foo))))
  > EOF

  $ echo "(inc_a inc_b (lib lib_a))" > foo

  $ mkdir -p inc_a inc_b
  $ echo "#define A 40" > inc_a/a.h
  $ echo "#define B 2" > inc_b/b.h

  $ mkdir -p lib_a
  $ cat >lib_a/dune <<EOF
  > (library (name lib_a))
  > EOF

  $ echo "#define C 2" > lib_a/c.h

  $ cat >bar.c <<EOF
  > #include <caml/mlvalues.h>
  > #include <a.h>
  > #include <b.h>
  > #include <c.h>
  > value bar(value unit) { return Val_int(A + B + C); }
  > EOF

  $ dune build
