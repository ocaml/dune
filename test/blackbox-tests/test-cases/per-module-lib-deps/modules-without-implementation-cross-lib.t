Regression: a [(modules_without_implementation)] entry whose
interface aliases a module from another library propagates the
dep correctly through the per-module filter — when the aliased
lib's interface changes, the consumer rebuilds without the
compiler complaining about inconsistent assumptions over
interface.

Reported by @art-w on #14116. The concern was that
[(modules_without_implementation)] entries' aliases to other
libs might escape the per-module dep filter (the intra-stanza
[trans_deps] graph skips them), leaving the consumer's compile
rule without a dep on the aliased lib's [.cmi] — which surfaces
as a hard build error on rebuild.

  $ cat > dune-project <<EOF
  > (lang dune 3.23)
  > EOF

  $ cat > dune <<EOF
  > (library (name dep_lib) (wrapped false) (modules original_name))
  > (library
  >  (name wrapper_lib)
  >  (wrapped false)
  >  (modules foo bar)
  >  (modules_without_implementation foo)
  >  (libraries dep_lib))
  > (library
  >  (name consumer_lib)
  >  (wrapped false)
  >  (modules consumer)
  >  (libraries wrapper_lib))
  > EOF

  $ cat > original_name.ml <<EOF
  > let x = "hello"
  > EOF
  $ cat > original_name.mli <<EOF
  > val x : string
  > EOF

  $ cat > foo.mli <<EOF
  > module Re = Original_name
  > EOF
  $ cat > bar.ml <<EOF
  > let placeholder = ()
  > EOF

  $ cat > consumer.ml <<EOF
  > let _ = Foo.Re.x
  > EOF

  $ dune build @check

Edit [dep_lib]'s interface. [consumer] reaches [Original_name]
through [Foo.Re], so it must rebuild — and must rebuild cleanly,
not error out with "inconsistent assumptions over interface":

  $ cat > original_name.mli <<EOF
  > val x : string
  > val y : int
  > EOF
  $ cat > original_name.ml <<EOF
  > let x = "hello"
  > let y = 42
  > EOF
  $ dune build @check
  $ dune trace cat | jq -s 'include "dune"; [.[] | targetsMatchingFilter(test("consumer"))] | length'
  1
