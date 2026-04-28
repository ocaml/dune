Baseline: an intermediate library [intermediate_lib] declares
[(libraries dep_lib)] but its module does not actually reference
any of [dep_lib]'s modules. The consumer [main] uses
[intermediate_lib] and so transitively gains [dep_lib] in its
compilation context.

Today every consumer module declares a glob over each transitively-
reached library's public-cmi directory, so editing
[unreferenced_dep.ml] (which no source file references) re-
invalidates [Main]. The test records the current rebuild targets
for [Main] when [unreferenced_dep.ml] is touched.

This test is observational: a tighter dependency tracker that drops
unreferenced libraries from compile rules' deps would shrink the
recorded target list.

[intermediate_lib] and the [main] executable each include an unused
dummy module ([intermediate_dummy] / [main_dummy]) so that ocamldep
runs unconditionally for both stanzas. Single-module stanzas trigger
a short-circuit in [dep_rules.ml] that skips ocamldep, which would
mask the per-module dependency filtering being baselined here.

  $ cat > dune-project <<EOF
  > (lang dune 3.23)
  > EOF

  $ cat > dune <<EOF
  > (library
  >  (name dep_lib)
  >  (wrapped false)
  >  (modules unreferenced_dep referenced_dep))
  > (library
  >  (name intermediate_lib)
  >  (wrapped false)
  >  (modules intermediate_module intermediate_dummy)
  >  (libraries dep_lib))
  > (executable
  >  (name main)
  >  (modules main main_dummy)
  >  (libraries intermediate_lib))
  > EOF

  $ cat > unreferenced_dep.ml <<EOF
  > let x = 42
  > EOF
  $ cat > referenced_dep.ml <<EOF
  > let x = 43
  > EOF
  $ cat > intermediate_module.ml <<EOF
  > let x = 42
  > EOF
  $ cat > intermediate_dummy.ml <<EOF
  > let _ = ()
  > EOF
  $ cat > main.ml <<EOF
  > let _ = Intermediate_module.x
  > EOF
  $ cat > main_dummy.ml <<EOF
  > let _ = ()
  > EOF

  $ dune build @check

Confirm via dune's own unused-libraries detection that [dep_lib]
is genuinely unused. [intermediate_lib] declares
[(libraries dep_lib)] but no module of [intermediate_lib]
references any module of [dep_lib]:

  $ dune build @unused-libs
  File "dune", line 9, characters 12-19:
  9 |  (libraries dep_lib))
                  ^^^^^^^
  Error: Unused libraries:
  - dep_lib
  [1]

Edit [unreferenced_dep.ml]. Neither [main.ml] nor
[intermediate_module.ml] references [Unreferenced_dep] or any
other [dep_lib] module, so a tighter filter could leave [Main]
untouched. Today [Main] is rebuilt:

  $ echo > unreferenced_dep.ml
  $ dune build @check
  $ dune trace cat | jq -s 'include "dune"; [.[] | targetsMatchingFilter(test("dune__exe__Main\\."))]'
  [
    {
      "target_files": [
        "_build/default/.main.eobjs/byte/dune__exe__Main.cmi",
        "_build/default/.main.eobjs/byte/dune__exe__Main.cmti"
      ]
    },
    {
      "target_files": [
        "_build/default/.main.eobjs/byte/dune__exe__Main.cmo",
        "_build/default/.main.eobjs/byte/dune__exe__Main.cmt"
      ]
    }
  ]
