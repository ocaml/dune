Baseline: wrapped-compat module recompilation behavior.

Libraries using (wrapped (transition ...)) generate wrapped-compat modules.
Currently, all inner modules are recompiled when any library dependency changes.

See: https://github.com/ocaml/dune/issues/4572

  $ cat > dune-project <<EOF
  > (lang dune 2.0)
  > EOF

  $ mkdir baselib
  $ cat > baselib/dune <<EOF
  > (library
  >  (name baselib))
  > EOF
  $ cat > baselib/baselib.ml <<EOF
  > let base_value = 42
  > EOF
  $ cat > baselib/baselib.mli <<EOF
  > val base_value : int
  > EOF

  $ mkdir translib
  $ cat > translib/dune <<EOF
  > (library
  >  (name translib)
  >  (libraries baselib)
  >  (wrapped (transition "Use Translib.X instead")))
  > EOF
  $ cat > translib/translib.ml <<EOF
  > module Uses_base = Uses_base
  > module Standalone = Standalone
  > EOF
  $ cat > translib/uses_base.ml <<EOF
  > let from_base () = Baselib.base_value
  > EOF
  $ cat > translib/uses_base.mli <<EOF
  > val from_base : unit -> int
  > EOF
  $ cat > translib/standalone.ml <<EOF
  > let own_value () = 999
  > EOF
  $ cat > translib/standalone.mli <<EOF
  > val own_value : unit -> int
  > EOF

  $ cat > dune <<EOF
  > (executable
  >  (name main)
  >  (libraries translib))
  > EOF
  $ cat > main.ml <<EOF
  > let () =
  >   print_int (Translib.Uses_base.from_base ());
  >   print_int (Translib.Standalone.own_value ())
  > EOF

  $ dune build ./main.exe

  $ cat > baselib/baselib.mli <<EOF
  > val base_value : int
  > val new_fn : unit -> string
  > EOF
  $ cat > baselib/baselib.ml <<EOF
  > let base_value = 42
  > let new_fn () = "hello"
  > EOF

Standalone is recompiled even though it doesn't reference Baselib:

  $ dune build ./main.exe
  $ dune trace cat | jq -s 'include "dune"; [.[] | targetsMatchingFilter(test("Standalone"))] | length'
  2
