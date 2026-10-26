Linking succeeds when stale and selected DLLs export the same primitive.
Execution must still use the implementation belonging to the selected library.
See #9979.

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
  $ echo 'external value : unit -> int = "a_value"' >producer/a.ml
  $ echo 'external value : unit -> int = "b_value"' >producer/b.ml
  $ cat >producer/a_stubs.c <<'EOF'
  > #include <caml/mlvalues.h>
  > CAMLprim value a_value(value unit) { return Val_int(1); }
  > EOF
  $ cat >producer/b_stubs.c <<'EOF'
  > #include <caml/mlvalues.h>
  > CAMLprim value b_value(value unit) { return Val_int(2); }
  > EOF
  $ dune build --root producer @install
  $ dune install --root producer --prefix "$PWD/prefix" 2>/dev/null
  $ export OCAMLPATH="$PWD/prefix/lib"
  $ export CAML_LD_LIBRARY_PATH="$PWD/prefix/lib/stublibs"

The executable uses private local a, whose primitive returns 10 rather than 1.

  $ make_dune_project 3.13
  $ cat >dune <<'EOF'
  > (data_only_dirs producer prefix)
  > (executable
  >  (name main)
  >  (modes byte exe)
  >  (libraries a pkg.b))
  > (rule
  >  (alias runtest)
  >  (action (run %{exe:main.bc})))
  > EOF
  $ cat >local/dune <<'EOF'
  > (library
  >  (name a)
  >  (foreign_stubs (language c) (names a_stubs)))
  > EOF
  $ cp producer/a.ml local/a.ml
  $ cat >local/a_stubs.c <<'EOF'
  > #include <caml/mlvalues.h>
  > CAMLprim value a_value(value unit) { return Val_int(10); }
  > EOF
  $ cat >main.ml <<'EOF'
  > let () = Printf.printf "%d %d\n" (A.value ()) (B.value ())
  > EOF
  $ dune build main.bc

Both normal execution paths currently load the installed a instead.

  $ dune exec ./main.bc
  1 2
  $ dune build @runtest
  1 2

Selecting local a's DLL explicitly demonstrates the intended result.

  $ ocamlrun -I _build/default/local _build/default/main.bc
  10 2

Native and statically linked bytecode already use the selected implementation.

  $ dune exec ./main.exe
  10 2
  $ cat >dune-workspace <<'EOF'
  > (lang dune 3.13)
  > (context
  >  (default (disable_dynamically_linked_foreign_archives true)))
  > EOF
  $ dune exec ./main.bc
  10 2
