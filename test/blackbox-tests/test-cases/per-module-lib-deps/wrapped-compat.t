Baseline: wrapped-compat module recompilation behavior.

Libraries using (wrapped (transition ...)) generate wrapped-compat modules.
Currently, all inner modules are recompiled when any library dependency changes.

See: https://github.com/ocaml/dune/issues/4572

  $ cat > dune-project <<EOF
  > (lang dune 2.0)
  > EOF

Create a base library that will be a dependency:

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

Create a library using wrapped transition. One module uses baselib, the other
does not. Both will get wrapped-compat modules generated.

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

Create an executable:

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

Build first (clean build):

  $ dune build ./main.exe

Now change baselib's INTERFACE:

  $ cat > baselib/baselib.mli <<EOF
  > val base_value : int
  > val new_fn : unit -> string
  > EOF

  $ cat > baselib/baselib.ml <<EOF
  > let base_value = 42
  > let new_fn () = "hello"
  > EOF

Standalone is recompiled even though it doesn't reference Baselib:

  $ dune build ./main.exe --display short 2>&1 | grep Standalone
        ocamlc translib/.translib.objs/byte/translib__Standalone.{cmi,cmti}
      ocamlopt translib/.translib.objs/native/translib__Standalone.{cmx,o}
