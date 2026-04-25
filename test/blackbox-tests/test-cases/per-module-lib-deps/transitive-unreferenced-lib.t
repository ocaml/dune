Baseline: an intermediate library [libB] declares [(libraries libA)]
but its module [modB] does not actually reference any of [libA]'s
modules. The consumer [main] uses [libB] and so transitively gains
[libA] in its compilation context.

Today every consumer module declares a glob over each transitively-
reached library's public-cmi directory, so editing [modA1.ml] (which
no source file references) re-invalidates [main]. The test records
the current rebuild count of [main] when [modA1.ml] is touched.

This test is observational: a tighter dependency tracker that drops
unreferenced libraries from compile rules' deps would lower the
count.

  $ cat > dune-project <<EOF
  > (lang dune 3.0)
  > EOF

  $ cat > dune <<EOF
  > (library (name libA) (wrapped false) (modules modA1 modA2))
  > (library (name libB) (wrapped false) (modules modB) (libraries libA))
  > (executable (name main) (modules main) (libraries libB))
  > EOF

  $ cat > modA1.ml <<EOF
  > let x = 42
  > EOF
  $ cat > modA2.ml <<EOF
  > let x = 43
  > EOF
  $ cat > modB.ml <<EOF
  > let x = 42
  > EOF
  $ cat > main.ml <<EOF
  > let _ = ModB.x
  > EOF

  $ dune build @check

Edit [modA1.ml]. Neither [main.ml] nor [modB.ml] references [modA1]
or any other [libA] module, so a tighter filter could leave [main]
untouched. Today [main] is rebuilt:

  $ echo > modA1.ml
  $ dune build @check
  $ dune trace cat | jq -s 'include "dune"; [.[] | targetsMatchingFilter(test("dune__exe__Main"))] | length'
  2
