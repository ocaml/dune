Baseline: library dependency recompilation with multiple libraries.

When an executable depends on two libraries and only one changes, Dune
currently recompiles all modules, even those that only reference the unchanged
library.

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

  $ mkdir lib2
  $ cat > lib2/dune <<EOF
  > (library
  >  (name otherlib))
  > EOF
  $ cat > lib2/otherlib.ml <<EOF
  > let other_value = 100
  > EOF
  $ cat > lib2/otherlib.mli <<EOF
  > val other_value : int
  > EOF

  $ cat > uses_lib.ml <<EOF
  > let get_value () = Mylib.value
  > EOF
  $ cat > uses_lib.mli <<EOF
  > val get_value : unit -> int
  > EOF
  $ cat > uses_other.ml <<EOF
  > let get_other () = Otherlib.other_value
  > EOF
  $ cat > uses_other.mli <<EOF
  > val get_other : unit -> int
  > EOF
  $ cat > no_use_lib.ml <<EOF
  > let compute x = x + 1
  > EOF
  $ cat > no_use_lib.mli <<EOF
  > val compute : int -> int
  > EOF

  $ cat > dune <<EOF
  > (executable
  >  (name main)
  >  (libraries mylib otherlib))
  > EOF
  $ cat > main.ml <<EOF
  > let () =
  >   print_int (Uses_lib.get_value ());
  >   print_int (No_use_lib.compute 5);
  >   print_int (Uses_other.get_other ())
  > EOF

  $ dune build ./main.exe

Change only mylib's interface:

  $ cat > lib/mylib.mli <<EOF
  > val value : int
  > val new_function : unit -> string
  > EOF
  $ cat > lib/mylib.ml <<EOF
  > let value = 42
  > let new_function () = "hello"
  > EOF

Uses_other is recompiled even though it only uses Otherlib, not Mylib:

  $ dune build ./main.exe
  $ dune trace cat | jq -s 'include "dune"; [.[] | targetsMatchingFilter(test("Uses_other"))] | length'
  2

Change only otherlib's interface:

  $ cat > lib2/otherlib.mli <<EOF
  > val other_value : int
  > val new_other_fn : string -> string
  > EOF
  $ cat > lib2/otherlib.ml <<EOF
  > let other_value = 100
  > let new_other_fn s = s ^ "!"
  > EOF

Uses_lib is recompiled even though it only uses Mylib, not Otherlib:

  $ dune build ./main.exe
  $ dune trace cat | jq -s 'include "dune"; [.[] | targetsMatchingFilter(test("Uses_lib"))] | length'
  2
