Regression guard for wrapped-library soundness when the library uses
[(wrapped (transition ...))].

A consumer that reaches a wrapped library's inner module via the
wrapper alias (e.g. [Wrapped_lib.Inner_a.x]) needs the inner
module's [.cmi] at compile time — the wrapper's [.cmi] only carries
an alias name, while the type information lives in the inner
module. The consumer's compile rule must therefore cover the inner
module's [.cmi] alongside the wrapper's. Any future per-module
narrowing of compile-rule deps must keep that coverage; otherwise
a change to the inner module's interface fails to invalidate the
consumer.

Structure: [wrapped_lib] uses [(wrapped (transition "msg"))] with
inner modules [inner_a] and [inner_b] plus a hand-written wrapper
[wrapped_lib.ml] that aliases both. [consumer_lib] depends on
[wrapped_lib] and writes [Wrapped_lib.Inner_a.x] — naming the
wrapper and one inner module but not the other.

  $ cat > dune-project <<EOF
  > (lang dune 3.23)
  > EOF

  $ cat > dune <<EOF
  > (library
  >  (name wrapped_lib)
  >  (wrapped (transition "use Wrapped_lib.X instead of X"))
  >  (modules wrapped_lib inner_a inner_b))
  > (library
  >  (name consumer_lib)
  >  (wrapped false)
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
  $ dune trace cat | jq -s 'include "dune"; [.[] | targetsMatchingFilter(test("consumer"))]'
  [
    {
      "target_files": [
        "_build/default/.consumer_lib.objs/byte/consumer.cmi",
        "_build/default/.consumer_lib.objs/byte/consumer.cmo",
        "_build/default/.consumer_lib.objs/byte/consumer.cmt"
      ]
    }
  ]
