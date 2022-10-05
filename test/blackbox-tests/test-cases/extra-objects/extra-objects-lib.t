----------------------------------------------------------------------------------
Build a library which depends on foreign object files.

----------------------------------------------------------------------------------
* (extra_objects ...) is unavailable before Dune 3.5.

  $ echo "(lang dune 3.4)" > dune-project
  $ mkdir -p lib

  $ cat >lib/dune <<EOF
  > (library
  >  (name calc)
  >  (extra_objects add mul))
  > EOF

  $ dune build
  File "lib/dune", line 3, characters 1-24:
  3 |  (extra_objects add mul))
       ^^^^^^^^^^^^^^^^^^^^^^^
  Error: 'extra_objects' is only available since version 3.5 of the dune
  language. Please update your dune-project file to have (lang dune 3.5).
  [1]

----------------------------------------------------------------------------------
* Error for missing object file

  $ echo "(lang dune 3.5)" > dune-project

  $ cat >lib/calc.ml <<EOF
  > external add : int -> int -> int = "add"
  > external mul : int -> int -> int = "mul"
  > let calc x y z = mul (add x y) z
  > EOF

  $ cat >lib/calc.mli <<EOF
  > val calc : int -> int -> int -> int
  > EOF

  $ dune build
  File "lib/dune", line 1, characters 0-47:
  1 | (library
  2 |  (name calc)
  3 |  (extra_objects add mul))
  Error: No rule found for lib/add.o
  File "lib/dune", line 1, characters 0-47:
  1 | (library
  2 |  (name calc)
  3 |  (extra_objects add mul))
  Error: No rule found for lib/mul.o
  [1]

----------------------------------------------------------------------------------
* Successful build when all object files are available

  $ cat >lib/add.c <<EOF
  > #include <caml/mlvalues.h>
  > value add(value x, value y) { return Val_int(Int_val(x) + Int_val(y)); }
  > EOF

  $ cat >lib/mul.c <<EOF
  > #include <caml/mlvalues.h>
  > value mul(value x, value y) { return Val_int(Int_val(x) * Int_val(y)); }
  > EOF

  $ cat >>lib/dune <<EOF
  > (rule
  >  (target mul.o)
  >  (deps mul.c)
  >  (action (run %{bin:ocamlc} mul.c)))
  > (rule
  >  (target add.o)
  >  (deps add.c)
  >  (action (run %{bin:ocamlc} add.c)))
  > EOF

  $ dune build

----------------------------------------------------------------------------------
* Add an executable to test that we can link against the foreign object files

  $ mkdir -p bin

  $ cat >bin/dune <<EOF
  > (executable
  >  (name main)
  >  (libraries calc))
  > EOF

  $ cat >bin/main.ml <<EOF
  > let () = print_int (Calc.calc 3 4 6)
  > let () = print_string "\n"
  > EOF

  $ dune exec ./bin/main.exe
  42
