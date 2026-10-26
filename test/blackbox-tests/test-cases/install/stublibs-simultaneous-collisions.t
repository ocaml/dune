Two shared search directories can each contain the wrong DLL for a selected
library. Neither ordering of the directories can link the executable.
See #9979 and #16789.

Install an old a alongside the b that the executable will use.

  $ mkdir -p producer local
  $ cat >producer/dune-project <<'EOF'
  > (lang dune 3.13)
  > (package (name pkg))
  > EOF
  $ cat >producer/dune <<'EOF'
  > (library
  >  (name a)
  >  (public_name pkg.a)
  >  (modules a)
  >  (foreign_stubs (language c) (names a_stubs)))
  > (library
  >  (name b)
  >  (public_name pkg.b)
  >  (modules b)
  >  (foreign_stubs (language c) (names b_stubs)))
  > EOF
  $ touch producer/a.ml
  $ echo 'external value : unit -> int = "selected_b"' >producer/b.ml
  $ cat >producer/a_stubs.c <<'EOF'
  > #include <caml/mlvalues.h>
  > CAMLprim value old_a(value unit) { return Val_int(0); }
  > EOF
  $ cat >producer/b_stubs.c <<'EOF'
  > #include <caml/mlvalues.h>
  > CAMLprim value selected_b(value unit) { return Val_int(2); }
  > EOF
  $ dune build --root producer @install
  $ dune install --root producer --prefix "$PWD/prefix" 2>/dev/null
  $ export OCAMLPATH="$PWD/prefix/lib"
  $ ext_dll=$(ocamlc -config-var ext_dll)

Build local a and an unrelated b in the same directory.

  $ make_dune_project 3.13
  $ echo '(data_only_dirs producer prefix)' >dune
  $ cat >local/dune <<'EOF'
  > (library
  >  (name a)
  >  (modules a)
  >  (foreign_stubs (language c) (names a_stubs)))
  > (library
  >  (name b)
  >  (modules b)
  >  (foreign_stubs (language c) (names b_stubs)))
  > EOF
  $ echo 'external value : unit -> int = "selected_a"' >local/a.ml
  $ touch local/b.ml
  $ cat >local/a_stubs.c <<'EOF'
  > #include <caml/mlvalues.h>
  > CAMLprim value selected_a(value unit) { return Val_int(1); }
  > EOF
  $ cat >local/b_stubs.c <<'EOF'
  > #include <caml/mlvalues.h>
  > CAMLprim value unrelated_b(value unit) { return Val_int(0); }
  > EOF
  $ dune build local/dllb_stubs$ext_dll
  $ echo 'let () = Printf.printf "%d %d\n" (A.value ()) (B.value ())' >main.ml

Installed directories first select the old a.

  $ cat >dune <<'EOF'
  > (data_only_dirs producer prefix)
  > (executable
  >  (name main)
  >  (modes byte)
  >  (libraries pkg.b a))
  > EOF
  $ dune build main.bc
  File "_none_", line 1:
  Error: Error while linking .main.eobjs/byte/dune__exe__Main.cmo:
         The external function selected_a is not available
  [1]

Local directories first select the unrelated b.

  $ cat >dune <<'EOF'
  > (data_only_dirs producer prefix)
  > (executable
  >  (name main)
  >  (modes byte)
  >  (libraries a pkg.b))
  > EOF
  $ dune build main.bc
  File "_none_", line 1:
  Error: Error while linking .main.eobjs/byte/dune__exe__Main.cmo:
         The external function selected_b is not available
  [1]
