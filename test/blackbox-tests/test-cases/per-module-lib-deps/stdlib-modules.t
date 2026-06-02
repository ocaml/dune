Baseline: modules using only stdlib are recompiled when a library changes.

A module that uses Printf (stdlib) but not the library dependency should
not need recompilation when the library changes.

See: https://github.com/ocaml/dune/issues/4572

  $ make_dune_project 3.0

  $ make_value_library lib mylib 42

  $ cat > dune <<EOF
  > (executable
  >  (name main)
  >  (libraries mylib))
  > EOF
  $ cat > uses_lib.ml <<EOF
  > let get_value () = Mylib.value
  > EOF
  $ cat > uses_lib.mli <<EOF
  > val get_value : unit -> int
  > EOF
  $ cat > uses_stdlib.ml <<EOF
  > let greet () = Printf.printf "hello\n"
  > EOF
  $ cat > uses_stdlib.mli <<EOF
  > val greet : unit -> unit
  > EOF
  $ cat > main.ml <<EOF
  > let () =
  >   print_int (Uses_lib.get_value ());
  >   Uses_stdlib.greet ()
  > EOF

  $ dune build ./main.exe

  $ write_mylib_with_new_function

Uses_stdlib is recompiled even though it only uses Printf, not Mylib:

  $ dune build ./main.exe
  $ dune trace cat | jq_dune -s '[.[] | targetsMatchingFilter(test("Uses_stdlib"))] | length'
  2
