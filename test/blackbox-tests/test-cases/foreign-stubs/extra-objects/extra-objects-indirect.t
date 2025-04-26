----------------------------------------------------------------------------------
Build an library which indirectly depends on foreign object files.

----------------------------------------------------------------------------------
* Building library with indirect dependency on object file

  $ echo "(lang dune 3.5)" > dune-project

  $ mkdir -p lib

  $ cat >lib/dune <<EOF
  > (library
  >  (name calc)
  >  (extra_objects add)
  >  (foreign_stubs (language c) (names dep)))
  > EOF

  $ cat >lib/calc.ml <<EOF
  > external add : int -> int -> int = "add"
  > EOF

  $ cat >lib/dep.c <<EOF
  > #include <caml/mlvalues.h>
  > extern value add(value x, value y);
  > value dummy() { return add(Val_int(1), Val_int(2)); }
  > EOF

  $ cat >lib/add.c <<EOF
  > #include <caml/mlvalues.h>
  > value add(value x, value y) { return Val_int(Int_val(x) + Int_val(y)); }
  > EOF

  $ cat >>lib/dune <<EOF
  > (rule
  >  (target add.o)
  >  (deps add.c)
  >  (action (run %{bin:ocamlc} add.c)))
  > EOF

  $ dune build
