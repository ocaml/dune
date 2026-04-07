Per-module filtering works for single-module library consumers.

When a consumer library has only one module, dune skips ocamldep for that
stanza. Without ocamldep data, the per-module filtering has nothing to work
with and falls back to all-library glob deps. Modifying an unused module in
a dependency triggers unnecessary recompilation.

See: https://github.com/ocaml/dune/issues/4572

  $ cat > dune-project <<EOF
  > (lang dune 3.0)
  > EOF

  $ mkdir base_lib
  $ cat > base_lib/dune <<EOF
  > (library (name base_lib) (wrapped false))
  > EOF
  $ cat > base_lib/alpha.ml <<EOF
  > let alpha_val = 1
  > EOF
  $ cat > base_lib/alpha.mli <<EOF
  > val alpha_val : int
  > EOF
  $ cat > base_lib/unused.ml <<EOF
  > let unused_val = 99
  > EOF
  $ cat > base_lib/unused.mli <<EOF
  > val unused_val : int
  > EOF

  $ mkdir consumer_lib
  $ cat > consumer_lib/dune <<EOF
  > (library (name consumer_lib) (wrapped false) (libraries base_lib))
  > EOF
  $ cat > consumer_lib/uses_alpha.ml <<EOF
  > let f () = Alpha.alpha_val
  > EOF
  $ cat > consumer_lib/uses_alpha.mli <<EOF
  > val f : unit -> int
  > EOF

  $ cat > dune <<EOF
  > (executable (name main) (libraries consumer_lib))
  > EOF
  $ cat > main.ml <<EOF
  > let () = print_int (Uses_alpha.f ())
  > EOF

  $ dune build ./main.exe

Modify only the unused module:

  $ cat > base_lib/unused.mli <<EOF
  > val unused_val : int
  > val new_fn : unit -> string
  > EOF
  $ cat > base_lib/unused.ml <<EOF
  > let unused_val = 99
  > let new_fn () = "new"
  > EOF

uses_alpha is no longer recompiled because it only references Alpha, not Unused:

  $ dune build ./main.exe
  $ dune trace cat | jq -s 'include "dune"; [.[] | targetsMatchingFilter(test("uses_alpha"))] | length'
  0
