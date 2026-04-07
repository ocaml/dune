Baseline: library dependency recompilation for a basic wrapped library.

When a wrapped library's interface changes, Dune currently recompiles ALL
modules in stanzas that depend on the library, even modules that don't
reference it.

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
  $ cat > no_use_lib.ml <<EOF
  > let compute x = x + 1
  > EOF
  $ cat > no_use_lib.mli <<EOF
  > val compute : int -> int
  > EOF
  $ cat > main.ml <<EOF
  > let () =
  >   print_int (Uses_lib.get_value ());
  >   print_int (No_use_lib.compute 5)
  > EOF

  $ dune build ./main.exe

Change mylib's interface:

  $ cat > lib/mylib.mli <<EOF
  > val value : int
  > val new_function : unit -> string
  > EOF
  $ cat > lib/mylib.ml <<EOF
  > let value = 42
  > let new_function () = "hello"
  > EOF

No_use_lib is recompiled even though it doesn't reference Mylib:

  $ dune build ./main.exe
  $ dune trace cat | jq -s 'include "dune"; [.[] | targetsMatchingFilter(test("No_use_lib"))] | length'
  2
