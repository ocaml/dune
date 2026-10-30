Two OCaml libraries may share one foreign archive. This is different from
requiring distinct DLLs with the same basename, which cannot both be selected
by a single directory search order. See #9979.

  $ make_dune_project 3.13
  $ mkdir a b common
  $ cat >common/dune <<'EOF'
  > (foreign_library (archive_name shared) (language c) (names stubs))
  > EOF
  $ cat >common/stubs.c <<'EOF'
  > #include <caml/mlvalues.h>
  > CAMLprim value a_value(value unit) { return Val_int(1); }
  > CAMLprim value b_value(value unit) { return Val_int(2); }
  > EOF
  $ cat >a/dune <<'EOF'
  > (library
  >  (name a)
  >  (foreign_archives ../common/shared))
  > EOF
  $ cat >b/dune <<'EOF'
  > (library
  >  (name b)
  >  (foreign_archives ../common/shared))
  > EOF
  $ echo 'external value : unit -> int = "a_value"' >a/a.ml
  $ echo 'external value : unit -> int = "b_value"' >b/b.ml
  $ cat >dune <<'EOF'
  > (executable
  >  (name main)
  >  (modes byte)
  >  (libraries a b))
  > EOF
  $ cat >main.ml <<'EOF'
  > let () = Printf.printf "%d %d\n" (A.value ()) (B.value ())
  > EOF

Repeated references to the same archive must remain valid.

  $ dune build main.bc
  $ ocamlrun -I _build/default/common _build/default/main.bc
  1 2

Now give each library its own DLL with the same basename. Both libraries are
dependencies of main; neither DLL is an unrelated build artifact.

  $ cat >a/dune <<'EOF'
  > (foreign_library (archive_name shared) (language c) (names stubs))
  > (library (name a) (modules a) (foreign_archives shared))
  > EOF
  $ cat >b/dune <<'EOF'
  > (foreign_library (archive_name shared) (language c) (names stubs))
  > (library (name b) (modules b) (foreign_archives shared))
  > EOF
  $ cat >a/stubs.c <<'EOF'
  > #include <caml/mlvalues.h>
  > CAMLprim value a_value(value unit) { return Val_int(1); }
  > EOF
  $ cat >b/stubs.c <<'EOF'
  > #include <caml/mlvalues.h>
  > CAMLprim value b_value(value unit) { return Val_int(2); }
  > EOF
  $ dune build main.bc
  File "_none_", line 1:
  Error: Error while linking .main.eobjs/byte/dune__exe__Main.cmo:
         The external function b_value is not available
  [1]

Reversing the order only changes which required primitive is missing. A useful
diagnostic should identify the two conflicting DLL providers.

  $ cat >dune <<'EOF'
  > (executable
  >  (name main)
  >  (modes byte)
  >  (libraries b a))
  > EOF
  $ dune build main.bc
  File "_none_", line 1:
  Error: Error while linking .main.eobjs/byte/dune__exe__Main.cmo:
         The external function a_value is not available
  [1]
