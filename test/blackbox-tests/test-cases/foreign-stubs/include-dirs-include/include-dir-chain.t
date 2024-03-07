----------------------------------------------------------------------------------
Include a file with an `(include ...)` statement, which iteslf contains an
`(include ...`) statement.

  $ echo "(lang dune 3.5)" > dune-project

  $ cat >dune <<EOF
  > (library
  >  (name foo)
  >  (foreign_stubs
  >   (language c)
  >   (names bar)
  >   (include_dirs (include foo))))
  > EOF

  $ cat >bar.c <<EOF
  > #include <caml/mlvalues.h>
  > #include <a.h>
  > #include <b.h>
  > value bar(value unit) { return Val_int(A + B); }
  > EOF

  $ mkdir -p inc_a inc_b
  $ echo "#define A 40" > inc_a/a.h
  $ echo "#define B 2" > inc_b/b.h
  $ echo "((include baz))" > foo
  $ echo "(inc_a inc_b)" > baz

  $ dune build
