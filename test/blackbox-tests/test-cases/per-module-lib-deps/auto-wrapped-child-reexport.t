Observational baseline: a consumer reaches a dep library's modules
through a child of an auto-wrapped sibling library. The sibling is
opened via the [-open] compiler flag and the child's source exposes
the dep library through a transparent module alias. On trunk,
[consumer] correctly rebuilds when the dep library's interface
changes, because the cctx-wide compile-rule deps cover every
library in the stanza's [(libraries ...)] closure.

The structural shape mirrors menhir's [base]/[middle] arrangement:
[base] is auto-wrapped (no [base.ml] in source — dune generates
the wrapper) with a child module that re-exports a third-party
dep; [middle] uses [-open Base] and reaches the dep through that
child. Menhir uses [include Vendored_pprint]; this test uses a
transparent alias instead, for the precision-bug reason described
below. The reference chain still crosses library boundaries
through the auto-wrapped sibling's child, not through a
hand-written wrapper.

Records the consumer's rebuild count as a regression guard for
changes that narrow compile-rule deps per module.

Structure: [dep_lib] is unwrapped with module [Original_name].
[lib_re_export] is wrapped by dune defaults (no [lib_re_export.ml]
file — dune auto-generates the wrapper); child [pprint.ml]
contains a transparent alias [module Re = Original_name];
[filler.ml] is just there to keep the lib non-singleton. [consumer_lib]
depends on [lib_re_export] and uses [-open Lib_re_export] in its
flags; [consumer.ml] writes [Pprint.Re.x] — naming the wrapped
lib's child [Pprint] but not [dep_lib] or the wrapper
[Lib_re_export].

The transparent-alias shape (rather than [include]) is what makes
[dep_lib] genuinely invisible to a per-module dep filter that only
walks the auto-generated wrapper: with [-no-alias-deps], [Pprint]'s
[.cmi] content does not depend on [Original_name.cmi], so a glob
over [lib_re_export]'s objdir does not capture [Original_name.mli]
changes either. The cctx-wide compile-rule deps still cover
[dep_lib] on trunk, so [consumer] rebuilds.

  $ cat > dune-project <<EOF
  > (lang dune 3.23)
  > EOF

  $ cat > dune <<EOF
  > (library (name dep_lib) (wrapped false) (modules original_name))
  > (library
  >  (name lib_re_export)
  >  (modules pprint filler)
  >  (libraries dep_lib))
  > (library
  >  (name consumer_lib)
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

Edit [dep_lib]'s interface. [consumer] reaches [dep_lib]'s
[Original_name] through [Pprint] (child of auto-wrapped
[Lib_re_export]). The cctx-wide compile-rule deps include
[dep_lib], so [consumer] rebuilds:

  $ cat > original_name.mli <<EOF
  > val x : string
  > val y : int
  > EOF
  $ cat > original_name.ml <<EOF
  > let x = "hello"
  > let y = 42
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
