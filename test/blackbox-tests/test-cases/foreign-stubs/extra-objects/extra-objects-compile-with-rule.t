----------------------------------------------------------------------------------
Build an executable which depends on foreign object files compiled with a rule.

  $ echo "(lang dune 3.5)" > dune-project

  $ cat >dune <<EOF
  > (executable
  >  (name calc)
  >  (extra_objects add mul))
  > (rule
  >  (targets add.o mul.o)
  >  (deps add.c mul.c)
  >  (action (run %{cc} -c -I %{ocaml_where} %{deps})))
  > EOF

  $ cat >calc.ml <<EOF
  > external add : int -> int -> int = "add"
  > external mul : int -> int -> int = "mul"
  > let calc x y z = mul (add x y) z
  > let () = print_int (calc 3 4 6)
  > let () = print_string "\n"
  > EOF

  $ cat >add.c <<EOF
  > #include <caml/mlvalues.h>
  > value add(value x, value y) { return Val_int(Int_val(x) + Int_val(y)); }
  > EOF

  $ cat >mul.c <<EOF
  > #include <caml/mlvalues.h>
  > value mul(value x, value y) { return Val_int(Int_val(x) * Int_val(y)); }
  > EOF

  $ dune exec ./calc.exe
  42
