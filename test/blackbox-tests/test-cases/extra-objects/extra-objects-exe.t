----------------------------------------------------------------------------------
Build an executable which depends on foreign object files.

----------------------------------------------------------------------------------
* (extra_objects ...) is unavailable before Dune 3.5.

  $ echo "(lang dune 3.4)" > dune-project
  $ mkdir -p bin

  $ cat >bin/dune <<EOF
  > (executable
  >  (name calc)
  >  (extra_objects add mul))
  > EOF

  $ dune build
  File "bin/dune", line 3, characters 1-24:
  3 |  (extra_objects add mul))
       ^^^^^^^^^^^^^^^^^^^^^^^
  Error: 'extra_objects' is only available since version 3.5 of the dune
  language. Please update your dune-project file to have (lang dune 3.5).
  [1]

----------------------------------------------------------------------------------
* Error for missing object file

  $ echo "(lang dune 3.5)" > dune-project

  $ cat >bin/calc.ml <<EOF
  > external add : int -> int -> int = "add"
  > external mul : int -> int -> int = "mul"
  > let calc x y z = mul (add x y) z
  > let () = print_int (calc 3 4 6)
  > let () = print_string "\n"
  > EOF

  $ dune build
  File "bin/dune", line 2, characters 7-11:
  2 |  (name calc)
             ^^^^
  Error: No rule found for bin/add.o
  File "bin/dune", line 2, characters 7-11:
  2 |  (name calc)
             ^^^^
  Error: No rule found for bin/mul.o
  [1]

----------------------------------------------------------------------------------
* Successful build when all object files are available

  $ cat >bin/add.c <<EOF
  > #include <caml/mlvalues.h>
  > value add(value x, value y) { return Val_int(Int_val(x) + Int_val(y)); }
  > EOF

  $ cat >bin/mul.c <<EOF
  > #include <caml/mlvalues.h>
  > value mul(value x, value y) { return Val_int(Int_val(x) * Int_val(y)); }
  > EOF

  $ cat >>bin/dune <<EOF
  > (rule
  >  (target mul.o)
  >  (deps mul.c)
  >  (action (run %{bin:ocamlc} mul.c)))
  > (rule
  >  (target add.o)
  >  (deps add.c)
  >  (action (run %{bin:ocamlc} add.c)))
  > EOF

  $ dune exec ./bin/calc.exe
  42
