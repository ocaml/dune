Baseline: library-to-library recompilation (unwrapped).

When an unwrapped library A depends on an unwrapped library B with multiple
modules, and one module in B changes, all modules in A are recompiled due to
coarse dependency analysis.

See: https://github.com/ocaml/dune/issues/4572

  $ cat > dune-project <<EOF
  > (lang dune 3.0)
  > EOF

  $ mkdir base_unwrapped
  $ cat > base_unwrapped/dune <<EOF
  > (library
  >  (name base_unwrapped)
  >  (wrapped false))
  > EOF
  $ cat > base_unwrapped/alpha.ml <<EOF
  > let alpha_val = 1
  > EOF
  $ cat > base_unwrapped/alpha.mli <<EOF
  > val alpha_val : int
  > EOF
  $ cat > base_unwrapped/beta.ml <<EOF
  > let beta_val = 2
  > EOF
  $ cat > base_unwrapped/beta.mli <<EOF
  > val beta_val : int
  > EOF

  $ mkdir upper_unwrapped
  $ cat > upper_unwrapped/dune <<EOF
  > (library
  >  (name upper_unwrapped)
  >  (wrapped false)
  >  (libraries base_unwrapped))
  > EOF
  $ cat > upper_unwrapped/uses_alpha.ml <<EOF
  > let from_alpha () = Alpha.alpha_val
  > EOF
  $ cat > upper_unwrapped/uses_alpha.mli <<EOF
  > val from_alpha : unit -> int
  > EOF
  $ cat > upper_unwrapped/uses_beta.ml <<EOF
  > let from_beta () = Beta.beta_val
  > EOF
  $ cat > upper_unwrapped/uses_beta.mli <<EOF
  > val from_beta : unit -> int
  > EOF
  $ cat > upper_unwrapped/uses_neither.ml <<EOF
  > let own_thing () = 0
  > EOF
  $ cat > upper_unwrapped/uses_neither.mli <<EOF
  > val own_thing : unit -> int
  > EOF

  $ cat > dune <<EOF
  > (executable
  >  (name main)
  >  (libraries upper_unwrapped))
  > EOF
  $ cat > main.ml <<EOF
  > let () =
  >   print_int (Uses_alpha.from_alpha ());
  >   print_int (Uses_beta.from_beta ());
  >   print_int (Uses_neither.own_thing ())
  > EOF

  $ dune build ./main.exe

Change only alpha.mli:

  $ cat > base_unwrapped/alpha.mli <<EOF
  > val alpha_val : int
  > val new_alpha_fn : unit -> string
  > EOF
  $ cat > base_unwrapped/alpha.ml <<EOF
  > let alpha_val = 1
  > let new_alpha_fn () = "alpha"
  > EOF

uses_beta is recompiled even though it only references Beta, not Alpha:

  $ dune build ./main.exe
  $ dune trace cat | jq -s 'include "dune"; [.[] | targetsMatchingFilter(test("uses_beta"))] | length'
  2

Change only beta.mli:

  $ cat > base_unwrapped/beta.mli <<EOF
  > val beta_val : int
  > val new_beta_fn : unit -> string
  > EOF
  $ cat > base_unwrapped/beta.ml <<EOF
  > let beta_val = 2
  > let new_beta_fn () = "beta"
  > EOF

uses_alpha is recompiled even though it only references Alpha, not Beta:

  $ dune build ./main.exe
  $ dune trace cat | jq -s 'include "dune"; [.[] | targetsMatchingFilter(test("uses_alpha"))] | length'
  2
