Each bytecode executable needs its own selected DLLs, even when another
executable uses a different DLL with the same basename. Updating a DLL or
changing the library dependencies must invalidate that selection. See #9979.

  $ make_dune_project 3.13
  $ mkdir a b
  $ cat >a/dune <<'EOF'
  > (foreign_library (archive_name shared) (language c) (names stubs))
  > (library (name a) (modules a) (foreign_archives shared))
  > EOF
  $ cat >b/dune <<'EOF'
  > (foreign_library (archive_name shared) (language c) (names stubs))
  > (library (name b) (modules b) (foreign_archives shared))
  > EOF
  $ echo 'external value : unit -> int = "a_value"' >a/a.ml
  $ echo 'external value : unit -> int = "b_value"' >b/b.ml
  $ cat >a/stubs.c <<'EOF'
  > #include <caml/mlvalues.h>
  > CAMLprim value a_value(value unit) { return Val_int(1); }
  > EOF
  $ cat >b/stubs.c <<'EOF'
  > #include <caml/mlvalues.h>
  > CAMLprim value b_value(value unit) { return Val_int(2); }
  > EOF
  $ cat >dune <<'EOF'
  > (executable
  >  (name main_a)
  >  (modules main_a)
  >  (modes byte)
  >  (libraries a))
  > (executable
  >  (name main_b)
  >  (modules main_b)
  >  (modes byte)
  >  (libraries b))
  > EOF
  $ echo 'let () = Printf.printf "%d\n" (A.value ())' >main_a.ml
  $ echo 'let () = Printf.printf "%d\n" (B.value ())' >main_b.ml
  $ dune build main_a.bc main_b.bc --sandbox=copy
  $ ocamlrun -I _build/default/a _build/default/main_a.bc
  1
  $ ocamlrun -I _build/default/b _build/default/main_b.bc
  2

The same independent selections work without sandboxing.

  $ dune build main_a.bc main_b.bc --sandbox=none --build-dir _build-no-sandbox
  $ ocamlrun -I _build-no-sandbox/default/a \
  >   _build-no-sandbox/default/main_a.bc
  1
  $ ocamlrun -I _build-no-sandbox/default/b \
  >   _build-no-sandbox/default/main_b.bc
  2

Only the executable depending on the modified stub should be relinked.

  $ cat >a/stubs.c <<'EOF'
  > #include <caml/mlvalues.h>
  > CAMLprim value a_value(value unit) { return Val_int(11); }
  > EOF
  $ dune build main_a.bc main_b.bc --sandbox=copy
  $ dune trace cat | jq_dune -sc '
  >   [ .[] | processes | .args | targets[]
  >     | select(endswith("main_a.bc") or endswith("main_b.bc"))
  >     | split("/")[-1] ] | sort'
  ["main_a.bc"]
  $ ocamlrun -I _build/default/a _build/default/main_a.bc
  11
  $ ocamlrun -I _build/default/b _build/default/main_b.bc
  2

Switch main_a to b without cleaning. Its old DLL has the same basename but
cannot satisfy b_value, so reusing the previous selection would fail.

  $ cat >dune <<'EOF'
  > (executable
  >  (name main_a)
  >  (modules main_a)
  >  (modes byte)
  >  (libraries b))
  > (executable
  >  (name main_b)
  >  (modules main_b)
  >  (modes byte)
  >  (libraries b))
  > EOF
  $ cp main_b.ml main_a.ml
  $ dune build main_a.bc main_b.bc --sandbox=copy
  $ ocamlrun -I _build/default/b _build/default/main_a.bc
  2
  $ ocamlrun -I _build/default/b _build/default/main_b.bc
  2

Removing the foreign dependency must not retain its DLL requirement.

  $ cat >dune <<'EOF'
  > (executable
  >  (name main_a)
  >  (modules main_a)
  >  (modes byte))
  > (executable
  >  (name main_b)
  >  (modules main_b)
  >  (modes byte)
  >  (libraries b))
  > EOF
  $ echo 'let () = print_endline "no stubs"' >main_a.ml
  $ dune exec ./main_a.bc
  no stubs
