Observational baseline: a consumer reaches a dep library's modules
through a transparent alias declared inside a wrapped sibling
library, where the wrapped library is opened via the [-open]
compiler flag rather than named in source. On trunk, [consumer]
correctly rebuilds when the dep library's interface changes,
because the cctx-wide compile-rule deps cover every library in the
stanza's [(libraries ...)] closure.

This is the structural shape of menhir's [base]/[middle]
arrangement: [base] re-exports [Vendored_pprint] through aliases,
[middle] uses [-open Base] in its compile flags, and modules of
[middle] reference [Vendored_pprint]'s contents through unqualified
names brought into scope by the open. Records the consumer's
rebuild count as a regression guard for changes that narrow
compile-rule deps per module.

Structure: [dep_lib] is unwrapped with module [Original_name];
[lib_re_export] is wrapped with a hand-written wrapper containing
[module Re = Original_name]; [consumer_lib] depends on [lib_re_export]
and uses [-open Lib_re_export] in its flags; [consumer.ml] writes
[Re.x] without naming [dep_lib] or [lib_re_export] in source.

  $ cat > dune-project <<EOF
  > (lang dune 3.23)
  > EOF

  $ cat > dune <<EOF
  > (library (name dep_lib) (wrapped false) (modules original_name))
  > (library
  >  (name lib_re_export)
  >  (modules lib_re_export some_inner)
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

  $ cat > lib_re_export.ml <<EOF
  > module Some_inner = Some_inner
  > module Re = Original_name
  > EOF
  $ cat > some_inner.ml <<EOF
  > let placeholder = ()
  > EOF

  $ cat > consumer.ml <<EOF
  > let _ = Re.x
  > EOF

  $ dune build @check

Edit [dep_lib]'s interface. [consumer] reaches [dep_lib]'s
[Original_name] through the alias chain in the wrapped
[Lib_re_export] wrapper. The cctx-wide compile-rule deps include
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
