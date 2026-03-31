Baseline: modules using only stdlib are recompiled when a library changes.

A module that uses Printf (stdlib) but not the library dependency should
not need recompilation when the library changes.

See: https://github.com/ocaml/dune/issues/4572

  $ cat > dune-project <<EOF
  > (lang dune 3.0)
  > EOF

  $ mkdir lib
  $ cat > lib/dune <<EOF
  > (library
  >  (name mylib))
  > EOF
  $ cat > lib/mylib.ml <<EOF
  > let value = 42
  > EOF
  $ cat > lib/mylib.mli <<EOF
  > val value : int
  > EOF

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

  $ cat > lib/mylib.mli <<EOF
  > val value : int
  > val new_function : unit -> string
  > EOF
  $ cat > lib/mylib.ml <<EOF
  > let value = 42
  > let new_function () = "hello"
  > EOF

Uses_stdlib is recompiled even though it only uses Printf, not Mylib:

  $ dune build ./main.exe
  $ dune trace cat | jq -s 'include "dune"; [.[] | targetsMatchingFilter(test("Uses_stdlib"))] | length'
  2
