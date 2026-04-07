Baseline: library dependency recompilation with virtual libraries.

When a virtual library's interface changes, Dune currently recompiles all
modules in stanzas that depend on it, even those that don't reference it.

See: https://github.com/ocaml/dune/issues/4572

  $ cat > dune-project <<EOF
  > (lang dune 3.0)
  > EOF

  $ mkdir vlib
  $ cat > vlib/dune <<EOF
  > (library
  >  (name vlib)
  >  (virtual_modules vmod))
  > EOF
  $ cat > vlib/vmod.mli <<EOF
  > val virtual_fn : int -> int
  > EOF

  $ mkdir vlib_impl
  $ cat > vlib_impl/dune <<EOF
  > (library
  >  (name vlib_impl)
  >  (implements vlib))
  > EOF
  $ cat > vlib_impl/vmod.ml <<EOF
  > let virtual_fn x = x * 3
  > EOF

  $ cat > uses_vlib.ml <<EOF
  > let call_vlib x = Vlib.Vmod.virtual_fn x
  > EOF
  $ cat > uses_vlib.mli <<EOF
  > val call_vlib : int -> int
  > EOF
  $ cat > no_vlib.ml <<EOF
  > let no_virtual () = 42
  > EOF
  $ cat > no_vlib.mli <<EOF
  > val no_virtual : unit -> int
  > EOF

  $ cat > dune <<EOF
  > (executable
  >  (name main)
  >  (libraries vlib_impl))
  > EOF
  $ cat > main.ml <<EOF
  > let () =
  >   print_int (Uses_vlib.call_vlib 10);
  >   print_int (No_vlib.no_virtual ())
  > EOF

  $ dune build ./main.exe

  $ cat > vlib/vmod.mli <<EOF
  > val virtual_fn : int -> int
  > val new_virtual : string -> string
  > EOF
  $ cat > vlib_impl/vmod.ml <<EOF
  > let virtual_fn x = x * 3
  > let new_virtual s = s ^ "!"
  > EOF

No_vlib is recompiled even though it doesn't reference the virtual library:

  $ dune build ./main.exe
  $ dune trace cat | jq -s 'include "dune"; [.[] | targetsMatchingFilter(test("No_vlib"))] | length'
  2
