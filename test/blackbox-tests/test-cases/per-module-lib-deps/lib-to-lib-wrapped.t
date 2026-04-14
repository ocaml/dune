Baseline: library-to-library recompilation (wrapped).

When library A depends on library B, and B's interface changes, all modules
in A are recompiled due to coarse dependency analysis.

See: https://github.com/ocaml/dune/issues/4572

  $ cat > dune-project <<EOF
  > (lang dune 3.0)
  > EOF

  $ mkdir base_lib
  $ cat > base_lib/dune <<EOF
  > (library
  >  (name base_lib))
  > EOF
  $ cat > base_lib/base_lib.ml <<EOF
  > let base_value = 42
  > EOF
  $ cat > base_lib/base_lib.mli <<EOF
  > val base_value : int
  > EOF

  $ mkdir middle_lib
  $ cat > middle_lib/dune <<EOF
  > (library
  >  (name middle_lib)
  >  (libraries base_lib))
  > EOF
  $ cat > middle_lib/uses_base.ml <<EOF
  > let from_base () = Base_lib.base_value
  > EOF
  $ cat > middle_lib/uses_base.mli <<EOF
  > val from_base : unit -> int
  > EOF
  $ cat > middle_lib/standalone.ml <<EOF
  > let own_value () = 999
  > EOF
  $ cat > middle_lib/standalone.mli <<EOF
  > val own_value : unit -> int
  > EOF

  $ cat > dune <<EOF
  > (executable
  >  (name main)
  >  (libraries middle_lib))
  > EOF
  $ cat > main.ml <<EOF
  > let () =
  >   print_int (Middle_lib.Uses_base.from_base ());
  >   print_int (Middle_lib.Standalone.own_value ())
  > EOF

  $ dune build ./main.exe

  $ cat > base_lib/base_lib.mli <<EOF
  > val base_value : int
  > val new_base_fn : unit -> string
  > EOF
  $ cat > base_lib/base_lib.ml <<EOF
  > let base_value = 42
  > let new_base_fn () = "hello"
  > EOF

Standalone in middle_lib is recompiled even though it doesn't use base_lib:

  $ dune build ./main.exe
  $ dune trace cat | jq -s 'include "dune"; [.[] | targetsMatchingFilter(test("Standalone"))] | length'
  2
