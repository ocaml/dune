Regression baseline for the cctx-wide `.cmi` glob over an unwrapped
library whose module set is declared explicitly via `(modules ...)`.
Today, editing one sibling module's `.mli` invalidates every
sibling's compile rule — even siblings that don't reference it.

Matches the shape raised in #14492's review feedback. The explicit
`(modules ...)` clause routes through a different parse path from
the implicit form covered by `lib-to-lib-unwrapped.t`.

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

Each consumer compile rule carries a wide `.cmi` glob over `foo`'s
objdir; no specific cmi from `foo` appears as a file dep. The wide
glob is the structural cause of the sibling over-rebuilds asserted
below.

  $ dune rules --root . --format=json --deps '%{cmo:consumer/uses_a}' > deps_a.json
  $ dune rules --root . --format=json --deps '%{cmo:consumer/uses_b}' > deps_b.json
  $ jq -r 'include "dune"; .[] | depsGlobs
  >   | select(.dir | endswith("foo/.foo.objs/byte"))
  >   | .dir + " " + .predicate' < deps_a.json
  _build/default/foo/.foo.objs/byte *.cmi
  $ jq -r 'include "dune"; .[] | depsGlobs
  >   | select(.dir | endswith("foo/.foo.objs/byte"))
  >   | .dir + " " + .predicate' < deps_b.json
  _build/default/foo/.foo.objs/byte *.cmi
  $ jq -r 'include "dune"; .[] | depsFilePaths
  >   | select(endswith("foo/.foo.objs/byte/a.cmi"))' < deps_a.json
  $ jq -r 'include "dune"; .[] | depsFilePaths
  >   | select(endswith("foo/.foo.objs/byte/b.cmi"))' < deps_b.json

Case 1: edit `A`'s interface to expose `extra`. `uses_b` references
only `B`, but the cctx-wide `.cmi` glob over `foo`'s objdir includes
`A.cmi`, so today `uses_b.cm*` rebuilds anyway.

  $ cat > foo/a.mli <<EOF
  > val v : int
  > val extra : string
  > EOF
  $ dune build @check
  $ dune trace cat | jq -s 'include "dune"; [.[] | targetsMatching("uses_b.cm")]'
  [
    {
      "target_files": [
        "_build/default/consumer/.consumer.objs/byte/uses_b.cmi",
        "_build/default/consumer/.consumer.objs/byte/uses_b.cmo",
        "_build/default/consumer/.consumer.objs/byte/uses_b.cmt"
      ]
    }
  ]

Case 2: edit `B`'s interface to expose `extra`. Symmetric — today
`uses_a.cm*` rebuilds anyway.

  $ cat > foo/b.mli <<EOF
  > val v : int
  > val extra : string
  > EOF
  $ dune build @check
  $ dune trace cat | jq -s 'include "dune"; [.[] | targetsMatching("uses_a.cm")]'
  [
    {
      "target_files": [
        "_build/default/consumer/.consumer.objs/byte/uses_a.cmi",
        "_build/default/consumer/.consumer.objs/byte/uses_a.cmo",
        "_build/default/consumer/.consumer.objs/byte/uses_a.cmt"
      ]
    }
  ]
