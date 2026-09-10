Per-module narrowing on a `(wrapped false)` library whose module
set is declared explicitly via `(modules ...)`. Under the per-
module narrowing, editing one sibling module's `.mli` invalidates
only modules that reference it — siblings that don't are
unaffected. Matches the implicit-discovery behaviour covered by
`lib-to-lib-unwrapped.t`, but the explicit `(modules ...)` clause
routes through a different parse path; this test pins the
equivalence observationally.

Matches the shape raised in #14492's review feedback.

  $ make_dune_project 3.24

`foo` is `(wrapped false)` with two modules `a`, `b`, declared
explicitly. Each module's `.ml` carries `extra` from the start so
the `.mli` edits below are interface-only.

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

`consumer` is `(wrapped false)` and depends on `foo`. `uses_a`
references `A` only; `uses_b` references `B` only.

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

Each consumer compile rule depends on only the specific cmi it
references — `uses_a` on `a.cmi`, `uses_b` on `b.cmi` — with no
wide glob over `foo`'s objdir.

  $ dune rules --root . --format=json --deps '%{cmo:consumer/uses_a}' > deps_a.json
  $ dune rules --root . --format=json --deps '%{cmo:consumer/uses_b}' > deps_b.json
  $ jq -r 'include "dune"; .[] | depsGlobs
  >   | select(.dir | endswith("foo/.foo.objs/byte"))
  >   | .dir + " " + .predicate' < deps_a.json
  $ jq -r 'include "dune"; .[] | depsGlobs
  >   | select(.dir | endswith("foo/.foo.objs/byte"))
  >   | .dir + " " + .predicate' < deps_b.json
  $ jq -r 'include "dune"; .[] | depsFilePaths
  >   | select(endswith("foo/.foo.objs/byte/a.cmi"))' < deps_a.json
  _build/default/foo/.foo.objs/byte/a.cmi
  $ jq -r 'include "dune"; .[] | depsFilePaths
  >   | select(endswith("foo/.foo.objs/byte/b.cmi"))' < deps_b.json
  _build/default/foo/.foo.objs/byte/b.cmi

Case 1: edit `A`'s interface to expose `extra`. `uses_b` references
only `B`, so the per-module narrowing must drop `A.cmi` from
`uses_b`'s dep set — `uses_b.cm*` must not rebuild.

  $ cat > foo/a.mli <<EOF
  > val v : int
  > val extra : string
  > EOF
  $ dune build @check
  $ dune trace cat | jq -s 'include "dune"; [.[] | targetsMatching("uses_b.cm")]'
  []

Case 2: edit `B`'s interface to expose `extra`. Symmetric —
`uses_a.cm*` must not rebuild.

  $ cat > foo/b.mli <<EOF
  > val v : int
  > val extra : string
  > EOF
  $ dune build @check
  $ dune trace cat | jq -s 'include "dune"; [.[] | targetsMatching("uses_a.cm")]'
  []
