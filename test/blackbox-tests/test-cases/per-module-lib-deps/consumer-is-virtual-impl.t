Regression: a stanza with [(implements vlib)] correctly tracks
changes to its own [(libraries ...)] deps.

For a virtual-library implementation, future per-module
dependency filtering work (#14116) must preserve deps from the
implementation's own [(libraries ...)] closure — otherwise the
implementation may fail to rebuild when one of those libraries'
interfaces changes. This test checks that an [(implements vlib)]
consumer rebuilds correctly when a dep in its [(libraries ...)]
field changes interface. Trunk satisfies the property today via
cctx-wide compile-rule deps.

Test structure: [vlib] declares a virtual module [vmod];
[vlib_impl] implements [vmod] AND depends on [other_lib]; the
implementation of [vmod] references [Other_mod] from
[other_lib]. Editing [Other_mod]'s interface should rebuild
[vlib_impl]'s [vmod].

  $ cat > dune-project <<EOF
  > (lang dune 3.0)
  > EOF

  $ cat > dune <<EOF
  > (library (name other_lib) (wrapped false) (modules other_mod))
  > EOF
  $ cat > other_mod.ml <<EOF
  > let helper = "hi"
  > EOF
  $ cat > other_mod.mli <<EOF
  > val helper : string
  > EOF

  $ mkdir vlib
  $ cat > vlib/dune <<EOF
  > (library
  >  (name vlib)
  >  (virtual_modules vmod))
  > EOF
  $ cat > vlib/vmod.mli <<EOF
  > val do_thing : unit -> string
  > EOF

  $ mkdir vlib_impl
  $ cat > vlib_impl/dune <<EOF
  > (library
  >  (name vlib_impl)
  >  (implements vlib)
  >  (libraries other_lib))
  > EOF
  $ cat > vlib_impl/vmod.ml <<EOF
  > let do_thing () = Other_mod.helper
  > EOF

  $ dune build @check

Edit [other_lib]'s interface. [vlib_impl]'s implementation of
[vmod] references [Other_mod], so it must rebuild:

  $ cat > other_mod.mli <<EOF
  > val helper : string
  > val extra : int
  > EOF
  $ cat > other_mod.ml <<EOF
  > let helper = "hi"
  > let extra = 42
  > EOF
  $ dune build @check
  $ dune trace cat | jq -s 'include "dune"; [.[] | targetsMatchingFilter(test("vlib_impl") and test("Vmod"))] | length'
  1
