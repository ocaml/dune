Regression guard for wrapped-library soundness under
[(wrapped (transition ...))].

[wrapped_lib] uses [(wrapped (transition ...))] with inner modules
[inner_a] and [inner_b] plus a hand-written wrapper module that
aliases both. The library [consumer] depends on [wrapped_lib] and
writes [Wrapped_lib.Inner_a.x] — naming the wrapper and one inner
module but not the other.

The wrapper's [.cmi] only carries an alias name; the type lives in
the inner module. So [consumer]'s compile rule must cover
[inner_a.cmi] alongside [wrapped_lib.cmi]. Any future per-module
narrowing of compile-rule deps must keep that coverage; otherwise
a change to [inner_a]'s interface fails to invalidate [consumer].

  $ cat > dune-project <<EOF
  > (lang dune 3.23)
  > EOF

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
  > EOF
  $ cat > inner_a.mli <<EOF
  > val x : string
  > EOF
  $ cat > inner_b.ml <<EOF
  > let y = "b"
  > EOF
  $ cat > inner_b.mli <<EOF
  > val y : string
  > EOF

  $ cat > consumer.ml <<EOF
  > let _ = Wrapped_lib.Inner_a.x
  > EOF

  $ dune build @check

Edit [inner_a]'s interface. [consumer] reaches [inner_a] through
the wrapper [Wrapped_lib]; the compile-rule deps must cover
[inner_a.cmi], so [consumer] rebuilds:

  $ cat > inner_a.mli <<EOF
  > val x : string
  > val z : int
  > EOF
  $ cat > inner_a.ml <<EOF
  > let x = "a"
  > let z = 42
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
