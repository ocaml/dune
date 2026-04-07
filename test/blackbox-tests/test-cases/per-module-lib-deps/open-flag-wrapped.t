Per-module filtering: -open flag for wrapped library in user flags.

When an executable uses (flags (:standard -open Baselib)), ocamldep
does not receive user flags and won't report the opened library.
Per-module filtering must include the opened library for all modules,
but still filter other libraries not referenced by each module.

See: https://github.com/ocaml/dune/issues/4572

  $ cat > dune-project <<EOF
  > (lang dune 3.0)
  > EOF

  $ mkdir baselib
  $ cat > baselib/dune <<EOF
  > (library (name baselib))
  > EOF
  $ cat > baselib/types.ml <<EOF
  > type t = { x : int }
  > EOF
  $ cat > baselib/types.mli <<EOF
  > type t = { x : int }
  > EOF

  $ mkdir otherlib
  $ cat > otherlib/dune <<EOF
  > (library (name otherlib))
  > EOF
  $ cat > otherlib/otherlib.ml <<EOF
  > let other_value = 99
  > EOF
  $ cat > otherlib/otherlib.mli <<EOF
  > val other_value : int
  > EOF

  $ cat > dune <<EOF
  > (executable
  >  (name main)
  >  (libraries baselib otherlib)
  >  (flags (:standard -open Baselib)))
  > EOF
  $ cat > uses_open.ml <<EOF
  > let v : Types.t = { x = 1 }
  > EOF
  $ cat > uses_open.mli <<EOF
  > val v : Types.t
  > EOF
  $ cat > uses_other.ml <<EOF
  > let w = Otherlib.other_value
  > EOF
  $ cat > uses_other.mli <<EOF
  > val w : int
  > EOF
  $ cat > main.ml <<EOF
  > let () =
  >   print_int Uses_open.v.Types.x;
  >   print_int Uses_other.w
  > EOF

Build succeeds — -open Baselib works correctly:

  $ dune build ./main.exe

Change only otherlib's interface:

  $ cat > otherlib/otherlib.mli <<EOF
  > val other_value : int
  > val new_fn : unit -> string
  > EOF
  $ cat > otherlib/otherlib.ml <<EOF
  > let other_value = 99
  > let new_fn () = "hello"
  > EOF

Uses_open is not recompiled because it uses Baselib (via -open), not Otherlib:

  $ dune build ./main.exe
  $ dune trace cat | jq -s 'include "dune"; [.[] | targetsMatchingFilter(test("Uses_open"))] | length'
  0
