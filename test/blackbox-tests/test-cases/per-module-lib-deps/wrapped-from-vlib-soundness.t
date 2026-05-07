Regression guard for soundness when a consumer depends on an
implementation of a virtual library whose [(wrapped ...)] setting
is *inherited* from the virtual library (the implementation does
not redeclare [wrapped]).

The implementation contributes both the virtual-module impl and
the vlib's concrete (non-virtual) modules into its closure. A
consumer that reaches one of the concrete modules through the
vlib wrapper (e.g. [Vlib.Helper.h]) needs that module's [.cmi] at
compile time. The consumer's compile rule must therefore cover the
implementation's [.cmi] directory; any future per-module narrowing
that treats inherited-wrapped libraries as ordinary local libraries
must still keep that coverage, otherwise a change to a concrete
module's interface fails to invalidate the consumer.

Structure: [vlib] declares [(wrapped true)], a virtual module
[virt_iface], and a concrete sibling [helper]. [impl] implements
[vlib] and inherits its [wrapped] setting. The executable [main]
depends on [impl] and references both modules through the vlib
wrapper.

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
cover [helper.cmi], so [main] rebuilds:

  $ cat > vlib/helper.mli <<EOF
  > val h : string
  > val z : int
  > EOF
  $ cat > vlib/helper.ml <<EOF
  > let h = "h"
  > let z = 42
  > EOF
  $ dune build @check
  $ dune trace cat | jq -s 'include "dune"; [.[] | targetsMatchingFilter(test("dune__exe__Main"))]'
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
