Observational baseline: a consumer reaches a dep library's modules
through a child of an auto-wrapped sibling library, where the
sibling is opened via the [-open] compiler flag and the child's
source includes the dep library's module via [include]. On trunk,
[consumer] correctly rebuilds when the dep library's interface
changes, because the cctx-wide compile-rule deps cover every
library in the stanza's [(libraries ...)] closure.

This is the structural shape of menhir's [base]/[middle]
arrangement: [base] is auto-wrapped (no [base.ml] in the source,
dune generates the wrapper), with a child [PPrint.ml] containing
[include Vendored_pprint]. [middle] uses [-open Base] in flags and
references [PPrint.foo] in source. The reference chain crosses
library boundaries through the auto-wrapped sibling's child, not
through a hand-written wrapper.

Records the consumer's rebuild count as a regression guard for
changes that narrow compile-rule deps per module.

Structure: [lib_a] is unwrapped with module [Original_name].
[lib_re_export] is wrapped by dune defaults (no [lib_re_export.ml]
file — dune auto-generates the wrapper); child [pprint.ml]
contains a transparent alias [module Re = Original_name];
[filler.ml] is just there to keep the lib non-singleton. [lib_b]
depends on [lib_re_export] and uses [-open Lib_re_export] in its
flags; [consumer.ml] writes [Pprint.Re.x] without naming
[lib_a], [lib_re_export], or any of its children in source.

The transparent-alias shape (rather than [include]) is what makes
[lib_a] genuinely invisible to a per-module dep filter that only
walks the auto-generated wrapper: with [-no-alias-deps], [Pprint]'s
[.cmi] content does not depend on [Original_name.cmi], so a glob
over [lib_re_export]'s objdir does not capture [Original_name.mli]
changes either. The cctx-wide compile-rule deps still cover
[lib_a] on trunk, so [consumer] rebuilds.

  $ cat > dune-project <<EOF
  > (lang dune 3.0)
  > EOF

  $ cat > dune <<EOF
  > (library (name lib_a) (wrapped false) (modules original_name))
  > (library
  >  (name lib_re_export)
  >  (modules pprint filler)
  >  (libraries lib_a))
  > (library
  >  (name lib_b)
  >  (wrapped false)
  >  (modules consumer)
  >  (libraries lib_re_export)
  >  (flags (:standard -open Lib_re_export)))
  > EOF

  $ cat > original_name.ml <<EOF
  > let x = "hello"
  > EOF
  $ cat > original_name.mli <<EOF
  > val x : string
  > EOF

  $ cat > pprint.ml <<EOF
  > module Re = Original_name
  > EOF
  $ cat > filler.ml <<EOF
  > let placeholder = ()
  > EOF

  $ cat > consumer.ml <<EOF
  > let _ = Pprint.Re.x
  > EOF

  $ dune build @check

Edit [lib_a]'s interface. [consumer] reaches [lib_a]'s
[Original_name] through [Pprint] (child of auto-wrapped
[Lib_re_export]). The cctx-wide compile-rule deps include
[lib_a], so [consumer] rebuilds:

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
