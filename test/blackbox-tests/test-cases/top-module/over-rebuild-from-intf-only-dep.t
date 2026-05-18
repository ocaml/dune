Reproduction: `dune ocaml top-module` over-rebuilds when an
unreferenced library's interface changes. The test passes while
the bug is present; promote when fixed.

`dune ocaml top-module` synthesises a compile for `m.ml` with the
`.mli` dropped, so only `m.ml`'s actual references should drive
the cmi-deps. Instead, the cmi-deps come from the parent library's
full `(libraries ...)` closure, so editing any of those deps'
interfaces triggers a rebuild — even libraries referenced ONLY
from the (dropped) `.mli`.

Code under test: `src/dune_rules/top_module.ml`.

  $ make_dune_project 3.24

`dep_for_intf` is referenced from `m.mli` only. `m.ml` never
mentions `Dep_for_intf`.

  $ mkdir dep_for_intf
  $ cat > dep_for_intf/dune <<EOF
  > (library (name dep_for_intf))
  > EOF
  $ cat > dep_for_intf/dep_for_intf.ml <<EOF
  > type t = int
  > let zero = 0
  > EOF
  $ cat > dep_for_intf/dep_for_intf.mli <<EOF
  > type t = int
  > EOF

`dep_for_impl` is referenced from `m.ml` only. `m.mli` never
mentions `Dep_for_impl`.

  $ mkdir dep_for_impl
  $ cat > dep_for_impl/dune <<EOF
  > (library (name dep_for_impl))
  > EOF
  $ cat > dep_for_impl/dep_for_impl.ml <<EOF
  > let value = 7
  > let extra = "x"
  > EOF
  $ cat > dep_for_impl/dep_for_impl.mli <<EOF
  > val value : int
  > EOF

`mylib` declares both libraries in `(libraries ...)` but module
`m` splits them: `.ml` uses only `Dep_for_impl`; `.mli` uses only
`Dep_for_intf`.

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
  > let tag = Dep_for_impl.value
  > EOF

Initial regular build, then `dune ocaml top-module mylib/m.ml`:

  $ dune build @check
  $ dune ocaml top-module mylib/m.ml > /dev/null 2>&1

Control: edit `dep_for_impl`'s `.mli` to expose `extra`. `m.ml`
references `Dep_for_impl.value`, so its top-module compile depends
on `dep_for_impl.cmi`. Rebuild expected. The trace contains the
`mylib__M.cmo` build action.

  $ cat > dep_for_impl/dep_for_impl.mli <<EOF
  > val value : int
  > val extra : string
  > EOF
  $ dune ocaml top-module mylib/m.ml --trace-file=_build/trace-impl.csexp > /dev/null 2>&1
  $ dune trace cat --trace-file=_build/trace-impl.csexp | jq -s 'include "dune"; [.[] | targetsMatchingFilter(test("\\.topmod/mylib/m\\.ml/mylib__M\\.cmo$"))]'
  [
    {
      "target_files": [
        "_build/default/.topmod/mylib/m.ml/mylib__M.cmo"
      ]
    }
  ]

Probe: edit `dep_for_intf`'s `.mli` to expose `zero`. `m.ml` never
references `Dep_for_intf` — the only reference was in the
discarded `m.mli`. The top-module compile rebuilds anyway; this is
the over-invalidation.

  $ cat > dep_for_intf/dep_for_intf.mli <<EOF
  > type t = int
  > val zero : t
  > EOF
  $ dune ocaml top-module mylib/m.ml --trace-file=_build/trace-intf.csexp > /dev/null 2>&1
  $ dune trace cat --trace-file=_build/trace-intf.csexp | jq -s 'include "dune"; [.[] | targetsMatchingFilter(test("\\.topmod/mylib/m\\.ml/mylib__M\\.cmo$"))]'
  [
    {
      "target_files": [
        "_build/default/.topmod/mylib/m.ml/mylib__M.cmo"
      ]
    }
  ]
