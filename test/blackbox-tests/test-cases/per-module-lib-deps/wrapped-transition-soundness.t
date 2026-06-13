Regression guard for wrapped-library soundness, with a forward-
looking pin on current behaviour, both under
`(wrapped (transition ...))`.

`wrapped_lib` uses `(wrapped (transition ...))` with inner modules
`inner_a` and `inner_b` plus a hand-written wrapper module that
aliases both. The library `consumer` depends on `wrapped_lib` and
writes `Wrapped_lib.Inner_a.x` — naming the wrapper and one inner
module but not the other.

The wrapper's `.cmi` only carries an alias name; the type lives in
the inner module's mangled artifact `wrapped_lib__Inner_a.cmi` (not
the `inner_a.cmi` transition shim). So `consumer`'s compile rule
must cover `wrapped_lib__Inner_a.cmi` alongside `wrapped_lib.cmi`.
Any future per-module narrowing of compile-rule deps must keep that
coverage; otherwise a change to `inner_a`'s interface fails to
invalidate `consumer`.

  $ make_dune_project 3.24

  $ cat > dune <<EOF
  > (library
  >  (name wrapped_lib)
  >  (wrapped (transition "use Wrapped_lib.X instead of X"))
  >  (modules wrapped_lib inner_a inner_b))
  > (library
  >  (name consumer)
  >  (modules consumer)
  >  (libraries wrapped_lib))
  > EOF

  $ cat > wrapped_lib.ml <<EOF
  > module Inner_a = Inner_a
  > module Inner_b = Inner_b
  > EOF
  $ cat > inner_a.ml <<EOF
  > let x = "a"
  > let z = 42
  > EOF
  $ cat > inner_a.mli <<EOF
  > val x : string
  > EOF
  $ cat > inner_b.ml <<EOF
  > let y = "b"
  > let w = "w"
  > EOF
  $ cat > inner_b.mli <<EOF
  > val y : string
  > EOF

  $ cat > consumer.ml <<EOF
  > let _ = Wrapped_lib.Inner_a.x
  > EOF

  $ dune build @check

Glob coverage on `consumer.cmi`'s compile rule:

  $ dune rules --root . --format=json --deps '%{cmi:consumer}' |
  > jq -r 'include "dune"; .[] | depsGlobs | "\(.dir_kind) \(.dir) \(.predicate)"'
  In_build_dir _build/default/.wrapped_lib.objs/byte *.cmi

Case 1 (soundness): edit `inner_a`'s interface to expose `z`.
`consumer` reaches `inner_a` through the wrapper `Wrapped_lib`;
the compile-rule deps must cover `wrapped_lib__Inner_a.cmi`, so
`consumer` rebuilds:

  $ cat > inner_a.mli <<EOF
  > val x : string
  > val z : int
  > EOF
  $ dune build @check
  $ dune trace cat | jq -s 'include "dune"; [.[] | targetsMatchingFilter(test("\\.consumer\\.objs/byte/consumer\\.cm"))]'
  [
    {
      "target_files": [
        "_build/default/.consumer.objs/byte/consumer.cmi",
        "_build/default/.consumer.objs/byte/consumer.cmo",
        "_build/default/.consumer.objs/byte/consumer.cmt"
      ]
    }
  ]

Case 2 (forward-looking pin on current behaviour): edit `inner_b`'s
interface to expose `w`. `consumer` does not reference `inner_b`,
so under a future per-module narrowing this edit would not rebuild
`consumer`. Today, the per-library filter rebuilds `consumer`
anyway because `wrapped_lib`'s `.cmi` glob covers every module.
Promote when per-module narrowing within a library lands.

  $ cat > inner_b.mli <<EOF
  > val y : string
  > val w : string
  > EOF
  $ dune build @check
  $ dune trace cat | jq -s 'include "dune"; [.[] | targetsMatchingFilter(test("\\.consumer\\.objs/byte/consumer\\.cm"))]'
  [
    {
      "target_files": [
        "_build/default/.consumer.objs/byte/consumer.cmi",
        "_build/default/.consumer.objs/byte/consumer.cmo",
        "_build/default/.consumer.objs/byte/consumer.cmt"
      ]
    }
  ]
