An intermediate library [libB] declares [(libraries libA)] but its
module [modB] does not reference any of [libA]'s modules. The
consumer [main] uses [libB] and so transitively gains [libA] in its
compilation context.

The cross-library walk has full visibility into [libA] (local,
unwrapped, every entry has a known module), and finishes without
adding any of [libA]'s entry modules to the reference closure. The
filter therefore drops [libA] from [main]'s compile-rule deps
entirely; the link rule still pulls [libA] in for executables that
need it.

Editing [modA1.ml] does not invalidate [main].

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

  $ echo > modA1.ml
  $ dune build @check
  $ dune trace cat | jq -s 'include "dune"; [.[] | targetsMatchingFilter(test("dune__exe__Main"))] | length'
  0
