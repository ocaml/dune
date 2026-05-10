Observational baseline: [dune ocaml top-module] over-invalidates the
top-module compile when an unreferenced library's interface changes.

[top_module.ml] derives a cctx via [Compilation_context.set_obj_dir]
to a private obj_dir, then calls [Module_compilation.build_module] on
a module value with [.mli] dropped via
[Module.set_source ~ml_kind:Intf None]. The derived cctx's compile
rule pulls cmi-deps from the parent library's full [(libraries ...)]
closure rather than from the actual references in [.ml] alone, so
editing any [(libraries ...)] dep's interface triggers a rebuild —
even libraries referenced ONLY from the (dropped) [.mli].

This test pins the current behaviour. A future change that filters
[top_module]'s compile-rule cmi-deps to match the [.ml]'s actual
references would flip the final assertion from REBUILT to
NOT REBUILT, and the test would need promotion.

  $ make_dune_project 3.24

[dep_for_intf] is referenced from [m.mli] only. [m.ml] never
mentions [Dep_for_intf].

  $ mkdir dep_for_intf
  $ cat > dep_for_intf/dune <<EOF
  > (library (name dep_for_intf))
  > EOF
  $ cat > dep_for_intf/dep_for_intf.ml <<EOF
  > type t = Tag
  > EOF
  $ cat > dep_for_intf/dep_for_intf.mli <<EOF
  > type t = Tag
  > EOF

[dep_for_impl] is referenced from [m.ml] only. [m.mli] never
mentions [Dep_for_impl].

  $ mkdir dep_for_impl
  $ cat > dep_for_impl/dune <<EOF
  > (library (name dep_for_impl))
  > EOF
  $ cat > dep_for_impl/dep_for_impl.ml <<EOF
  > let value = 7
  > EOF
  $ cat > dep_for_impl/dep_for_impl.mli <<EOF
  > val value : int
  > EOF

[mylib] declares both libraries in [(libraries ...)] but module
[m] splits them: [.ml] uses only [Dep_for_impl]; [.mli] uses only
[Dep_for_intf].

  $ mkdir mylib
  $ cat > mylib/dune <<EOF
  > (library
  >  (name mylib)
  >  (libraries dep_for_intf dep_for_impl))
  > EOF
  $ cat > mylib/m.mli <<EOF
  > val tag : Dep_for_intf.t
  > EOF
  $ cat > mylib/m.ml <<EOF
  > let _ = Dep_for_impl.value
  > let tag = Dep_for_intf.Tag
  > EOF

Initial regular build, then [dune ocaml top-module mylib/m.ml]:

  $ dune build @check 2>&1 | head -5
  $ dune ocaml top-module mylib/m.ml > /dev/null 2>&1
  $ stat -c '%y' _build/default/.topmod/mylib/m.ml/mylib__M.cmo > before-impl-edit.mtime

Control: edit [dep_for_impl]'s cmi (referenced from [m.ml]). The
top-module compile rebuilds — expected.

  $ cat > dep_for_impl/dep_for_impl.ml <<EOF
  > let value = 7
  > let extra = "x"
  > EOF
  $ cat > dep_for_impl/dep_for_impl.mli <<EOF
  > val value : int
  > val extra : string
  > EOF
  $ dune ocaml top-module mylib/m.ml > /dev/null 2>&1
  $ stat -c '%y' _build/default/.topmod/mylib/m.ml/mylib__M.cmo > after-impl-edit.mtime
  $ if [ "$(cat before-impl-edit.mtime)" != "$(cat after-impl-edit.mtime)" ]; then echo "REBUILT"; else echo "NOT REBUILT"; fi
  REBUILT
  $ cp after-impl-edit.mtime before-intf-edit.mtime

Probe: edit [dep_for_intf]'s cmi (NOT referenced from [m.ml] — the
only reference was in the discarded [m.mli]). The top-module
compile rebuilds despite the absence of any actual reference. This
is the over-invalidation under observation.

  $ cat > dep_for_intf/dep_for_intf.ml <<EOF
  > type t = Tag | Other
  > EOF
  $ cat > dep_for_intf/dep_for_intf.mli <<EOF
  > type t = Tag | Other
  > EOF
  $ dune ocaml top-module mylib/m.ml > /dev/null 2>&1
  $ stat -c '%y' _build/default/.topmod/mylib/m.ml/mylib__M.cmo > after-intf-edit.mtime
  $ if [ "$(cat before-intf-edit.mtime)" != "$(cat after-intf-edit.mtime)" ]; then echo "REBUILT"; else echo "NOT REBUILT"; fi
  REBUILT
