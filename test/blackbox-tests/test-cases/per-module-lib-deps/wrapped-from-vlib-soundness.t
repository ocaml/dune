Regression guards for soundness, with a forward-looking pin on
current behaviour, all when a consumer depends on an implementation
that inherits its `(wrapped ...)` setting from the virtual library
(the implementation does not redeclare `wrapped`).

`vlib` declares `(wrapped true)` with a virtual module `virt_iface`
and concrete siblings `helper` and `unused`. `impl` implements
`vlib` without redeclaring `wrapped`. The executable `main` depends
on `impl` and reaches `virt_iface` and `helper` via the vlib
wrapper: `Vlib.Virt_iface.x` and `Vlib.Helper.h`. `main` does not
reference `unused`.

The implementation's closure includes `virt_iface`'s impl and
`vlib`'s concrete modules. `main`'s compile rule must therefore
cover `vlib__Virt_iface.cmi` and `vlib__Helper.cmi`. Any future
per-module narrowing that treats inherited-wrapped libraries as
ordinary local libraries must still keep that coverage; otherwise
a change to either module's interface fails to invalidate `main`.

  $ make_dune_project 3.24

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
  > let z = 42
  > EOF
  $ cat > vlib/helper.mli <<EOF
  > val h : string
  > EOF
  $ cat > vlib/unused.ml <<EOF
  > let u = "u"
  > let w = "w"
  > EOF
  $ cat > vlib/unused.mli <<EOF
  > val u : string
  > EOF

  $ cat > impl/dune <<EOF
  > (library
  >  (name impl)
  >  (implements vlib))
  > EOF
  $ cat > impl/virt_iface.ml <<EOF
  > let x = "impl"
  > let z = 42
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

Glob coverage on `main.cmi`'s compile rule:

  $ dune rules --root . --format=json --deps '%{cmi:consumer/main}' |
  > jq -r 'include "dune"; .[] | depsGlobs | "\(.dir_kind) \(.dir) \(.predicate)"'
  In_build_dir _build/default/impl/.impl.objs/byte *.cmi

Case 1 (soundness): edit `helper`'s interface to expose `z`. `main`
reaches `helper` through the vlib wrapper; the compile-rule deps
must cover `vlib__Helper.cmi`, so `main` rebuilds:

  $ cat > vlib/helper.mli <<EOF
  > val h : string
  > val z : int
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

Case 2 (soundness): edit `virt_iface`'s interface to expose `z`.
`main` reaches `virt_iface` through the vlib wrapper; the compile-
rule deps must cover `vlib__Virt_iface.cmi`, so `main` rebuilds:

  $ cat > vlib/virt_iface.mli <<EOF
  > val x : string
  > val z : int
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

Case 3 (forward-looking pin on current behaviour): edit `unused`'s
interface to expose `w`. `main` does not reference `unused`, so
under a future per-module narrowing this edit would not rebuild
`main`. Today, the per-library filter rebuilds `main` anyway
because `impl`'s `.cmi` glob covers every module of the
implementation's closure. Promote when per-module narrowing within
a library lands.

  $ cat > vlib/unused.mli <<EOF
  > val u : string
  > val w : string
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
