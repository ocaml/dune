Per-module filtering on a `(wrapped false)` library whose module set
is declared explicitly via `(modules ...)`. Matches the exact shape
of the example raised in #14492's review feedback. Behaviour must
match the implicit-discovery form covered by `lib-to-lib-unwrapped.t`
— but the explicit clause routes through a different parse path, so
a separate observational test pins the equivalence empirically rather
than by inspection.

  $ make_dune_project 3.24

`foo` is a `(wrapped false)` library with two modules `a`, `b`,
declared explicitly. Each module's `.ml` carries `extra` from the
start so the `.mli` edits below are interface-only.

  $ mkdir foo
  $ cat > foo/dune <<EOF
  > (library
  >  (name foo)
  >  (wrapped false)
  >  (modules a b))
  > EOF
  $ cat > foo/a.ml <<EOF
  > let v = 1
  > let extra = "x"
  > EOF
  $ cat > foo/a.mli <<EOF
  > val v : int
  > EOF
  $ cat > foo/b.ml <<EOF
  > let v = 2
  > let extra = "y"
  > EOF
  $ cat > foo/b.mli <<EOF
  > val v : int
  > EOF

`consumer` is a multi-module `(wrapped false)` library depending on
`foo`. `uses_a` references `A` only; `uses_b` references `B` only.
Multi-module so the consumer's compile rules satisfy `can_filter`.

  $ mkdir consumer
  $ cat > consumer/dune <<EOF
  > (library
  >  (name consumer)
  >  (wrapped false)
  >  (libraries foo))
  > EOF
  $ cat > consumer/uses_a.ml <<EOF
  > let _ = A.v
  > EOF
  $ cat > consumer/uses_b.ml <<EOF
  > let _ = B.v
  > EOF

  $ dune build @check

Case 1: edit `A`'s interface to expose `extra`. `uses_b` references
only `B`, so the per-module filter must drop `A.cmi` from `uses_b`'s
dep set — `uses_b.cm*` must not rebuild.

  $ cat > foo/a.mli <<EOF
  > val v : int
  > val extra : string
  > EOF
  $ dune build @check
  $ dune trace cat | jq -s 'include "dune"; [.[] | targetsMatching("uses_b.cm")]'
  []

Case 2: edit `B`'s interface to expose `extra`. `uses_a` references
only `A` — `uses_a.cm*` must not rebuild.

  $ cat > foo/b.mli <<EOF
  > val v : int
  > val extra : string
  > EOF
  $ dune build @check
  $ dune trace cat | jq -s 'include "dune"; [.[] | targetsMatching("uses_a.cm")]'
  []
