Regression guard for soundness when a consumer depends on an
implementation that inherits its [(wrapped ...)] setting from the
virtual library (the implementation does not redeclare [wrapped]).

[vlib] declares [(wrapped true)] with a virtual module [virt_iface]
and a concrete sibling [helper]. [impl] implements [vlib] without
redeclaring [wrapped]. The executable [main] depends on [impl] and
reaches both modules via the vlib wrapper: [Vlib.Virt_iface.x] and
[Vlib.Helper.h].

The implementation's closure includes both [virt_iface]'s impl and
[vlib]'s concrete modules. [main]'s compile rule must therefore
cover [impl]'s [.cmi] directory. Any future per-module narrowing
that treats inherited-wrapped libraries as ordinary local libraries
must still keep that coverage; otherwise a change to [helper]'s
interface fails to invalidate [main].

  $ cat > dune-project <<EOF
  > (lang dune 3.23)
  > EOF

  $ mkdir vlib impl consumer

  $ cat > vlib/dune <<EOF
  > (library
  >  (name vlib)
  >  (wrapped true)
  >  (virtual_modules virt_iface))
  > EOF
  $ cat > vlib/virt_iface.mli <<EOF
  > val x : string
  > EOF
  $ cat > vlib/helper.ml <<EOF
  > let h = "h"
  > EOF
  $ cat > vlib/helper.mli <<EOF
  > val h : string
  > EOF

  $ cat > impl/dune <<EOF
  > (library
  >  (name impl)
  >  (implements vlib))
  > EOF
  $ cat > impl/virt_iface.ml <<EOF
  > let x = "impl"
  > EOF

  $ cat > consumer/dune <<EOF
  > (executable
  >  (name main)
  >  (libraries impl))
  > EOF
  $ cat > consumer/main.ml <<EOF
  > let () = print_string Vlib.Virt_iface.x; print_string Vlib.Helper.h
  > EOF

  $ dune build @check

Edit [helper]'s interface (a concrete vlib module). [main] reaches
[helper] through the vlib wrapper; the compile-rule deps must
cover [vlib__Helper.cmi], so [main] rebuilds:

  $ cat > vlib/helper.mli <<EOF
  > val h : string
  > val z : int
  > EOF
  $ cat > vlib/helper.ml <<EOF
  > let h = "h"
  > let z = 42
  > EOF
  $ dune build @check
  $ dune trace cat | jq -s 'include "dune"; [.[] | targetsMatchingFilter(test("consumer/\\.main\\.eobjs/byte/"))]'
  [
    {
      "target_files": [
        "_build/default/consumer/.main.eobjs/byte/dune__exe__Main.cmi",
        "_build/default/consumer/.main.eobjs/byte/dune__exe__Main.cmti"
      ]
    },
    {
      "target_files": [
        "_build/default/consumer/.main.eobjs/byte/dune__exe__Main.cmo",
        "_build/default/consumer/.main.eobjs/byte/dune__exe__Main.cmt"
      ]
    }
  ]
