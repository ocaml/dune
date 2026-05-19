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
[vlib_impl] implements [vmod] AND depends on [dep_lib]; the
implementation of [vmod] references [Dep_module] from
[dep_lib]. Editing [Dep_module]'s interface should rebuild
[vlib_impl]'s [vmod].

  $ cat > dune-project <<EOF
  > (lang dune 3.23)
  > EOF

  $ cat > dune <<EOF
  > (library (name dep_lib) (wrapped false) (modules dep_module))
  > EOF
  $ cat > dep_module.ml <<EOF
  > let helper = "hi"
  > EOF
  $ cat > dep_module.mli <<EOF
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
  >  (libraries dep_lib))
  > EOF
  $ cat > vlib_impl/vmod.ml <<EOF
  > let do_thing () = Dep_module.helper
  > EOF

  $ dune build @check

Edit [dep_lib]'s interface. [vlib_impl]'s implementation of
[vmod] references [Dep_module], so it must rebuild:

  $ cat > dep_module.mli <<EOF
  > val helper : string
  > val extra : int
  > EOF
  $ cat > dep_module.ml <<EOF
  > let helper = "hi"
  > let extra = 42
  > EOF
  $ dune build @check
  $ dune trace cat | jq -s 'include "dune"; [.[] | targetsMatchingFilter(test("vlib_impl") and test("Vmod"))] | length'
  1
