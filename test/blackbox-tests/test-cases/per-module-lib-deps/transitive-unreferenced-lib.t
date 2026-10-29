Baseline: an intermediate library [intermediate_lib] declares
[(libraries dep_lib)] but its module does not actually reference
any of [dep_lib]'s modules. The consumer [main] uses
[intermediate_lib] and so transitively gains [dep_lib] in its
compilation context.

The per-module filter detects that no consumer module references
any [dep_lib] module — directly or transitively through
[intermediate_lib] — and drops [dep_lib] entirely from [Main]'s
compile-rule deps, so editing [unreferenced_dep.ml] leaves [Main]
untouched. Previously every consumer module declared a glob over
each transitively-reached library's public-cmi directory, so the
edit re-invalidated [Main].

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
other [dep_lib] module, so the filter leaves [Main] untouched:

  $ echo > unreferenced_dep.ml
  $ dune build @check
  $ dune trace cat | jq -s 'include "dune"; [.[] | targetsMatchingFilter(test("dune__exe__Main\\."))]'
  []
