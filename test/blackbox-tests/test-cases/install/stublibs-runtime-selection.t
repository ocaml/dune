Linking succeeds when stale and selected DLLs export the same primitive.
Execution must still use the implementation belonging to the selected library.
See #9979.

  $ mkdir -p producer local
  $ write_stublibs_ab_producer
  $ echo 'external value : unit -> int = "a_value"' >producer/a.ml
  $ echo 'external value : unit -> int = "b_value"' >producer/b.ml
  $ write_stublibs_primitive producer/a_stubs.c a_value 1
  $ write_stublibs_primitive producer/b_stubs.c b_value 2
  $ install_stublibs_producer
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
  $ write_stublibs_primitive local/a_stubs.c a_value 10
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
