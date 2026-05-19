Baseline: consumer-module rebuild targets when individual modules
of an unwrapped dependency library change.

This is an observational test. It records the rebuild targets for
the consumer module [consumer] when each of three entry modules of
the dependency library [dep_lib] has its interface edited.
[consumer] references only [Referenced_dep] from [dep_lib];
[Unread_dep_a] and [Unread_dep_b] are present but unreferenced.

On current main, editing any of the three rebuilds [consumer]
because library-level dependency filtering (and, within that,
per-module tightening) is not yet in place: the consumer is
conservatively rebuilt whenever any entry module's cmi changes.
Work on https://github.com/ocaml/dune/issues/4572 is expected to
tighten this, at which point editing [Unread_dep_a] or
[Unread_dep_b] leaves [consumer] untouched and the emitted target
list becomes empty.

See: https://github.com/ocaml/dune/issues/4572

  $ cat > dune-project <<EOF
  > (lang dune 3.23)
  > EOF

[dep_lib] is an unwrapped library with three entry modules, each
with an explicit interface so signature changes propagate through
.cmi files:

  $ mkdir dep_lib
  $ cat > dep_lib/dune <<EOF
  > (library (name dep_lib) (wrapped false))
  > EOF
  $ cat > dep_lib/unread_dep_a.ml <<EOF
  > let v = 1
  > EOF
  $ cat > dep_lib/unread_dep_a.mli <<EOF
  > val v : int
  > EOF
  $ cat > dep_lib/referenced_dep.ml <<EOF
  > let v = 2
  > EOF
  $ cat > dep_lib/referenced_dep.mli <<EOF
  > val v : int
  > EOF
  $ cat > dep_lib/unread_dep_b.ml <<EOF
  > let v = 3
  > EOF
  $ cat > dep_lib/unread_dep_b.mli <<EOF
  > val v : int
  > EOF

[consumer_lib] has two modules. The module of interest, [consumer],
references only [Referenced_dep] from [dep_lib]. A second, unused
module [filler] is present only to keep [consumer_lib] a multi-
module stanza: dune skips ocamldep for single-module stanzas with
no library deps as an optimisation, and the per-module-lib-deps
filter depends on ocamldep output. Including [filler] isolates
this test from the skip-ocamldep optimisation so the rebuild
targets for [consumer] reflect only the per-module filter's work:

  $ mkdir consumer_lib
  $ cat > consumer_lib/dune <<EOF
  > (library (name consumer_lib) (wrapped false) (libraries dep_lib))
  > EOF
  $ cat > consumer_lib/consumer.ml <<EOF
  > let w = Referenced_dep.v
  > EOF
  $ cat > consumer_lib/filler.ml <<EOF
  > let _ = ()
  > EOF

  $ dune build @check

Edit [Unread_dep_a]'s interface — a module [consumer] does not
reference — and record the rebuild targets for [consumer]:

  $ cat > dep_lib/unread_dep_a.mli <<EOF
  > val v : int
  > val extra : unit -> int
  > EOF
  $ cat > dep_lib/unread_dep_a.ml <<EOF
  > let v = 1
  > let extra () = 7
  > EOF
  $ dune build @check
  $ dune trace cat | jq -s 'include "dune"; [.[] | targetsMatchingFilter(test("consumer_lib/\\.consumer_lib\\.objs/byte/consumer\\."))]'
  [
    {
      "target_files": [
        "_build/default/consumer_lib/.consumer_lib.objs/byte/consumer.cmi",
        "_build/default/consumer_lib/.consumer_lib.objs/byte/consumer.cmo",
        "_build/default/consumer_lib/.consumer_lib.objs/byte/consumer.cmt"
      ]
    }
  ]

Same for [Unread_dep_b]:

  $ cat > dep_lib/unread_dep_b.mli <<EOF
  > val v : int
  > val other : string
  > EOF
  $ cat > dep_lib/unread_dep_b.ml <<EOF
  > let v = 3
  > let other = "hi"
  > EOF
  $ dune build @check
  $ dune trace cat | jq -s 'include "dune"; [.[] | targetsMatchingFilter(test("consumer_lib/\\.consumer_lib\\.objs/byte/consumer\\."))]'
  [
    {
      "target_files": [
        "_build/default/consumer_lib/.consumer_lib.objs/byte/consumer.cmi",
        "_build/default/consumer_lib/.consumer_lib.objs/byte/consumer.cmo",
        "_build/default/consumer_lib/.consumer_lib.objs/byte/consumer.cmt"
      ]
    }
  ]

Edit [Referenced_dep]'s interface — the one module [consumer] does
reference — and record the rebuild targets ([consumer] must
rebuild):

  $ cat > dep_lib/referenced_dep.mli <<EOF
  > val v : int
  > val new_fn : int -> int
  > EOF
  $ cat > dep_lib/referenced_dep.ml <<EOF
  > let v = 2
  > let new_fn x = x + 1
  > EOF
  $ dune build @check
  $ dune trace cat | jq -s 'include "dune"; [.[] | targetsMatchingFilter(test("consumer_lib/\\.consumer_lib\\.objs/byte/consumer\\."))]'
  [
    {
      "target_files": [
        "_build/default/consumer_lib/.consumer_lib.objs/byte/consumer.cmi",
        "_build/default/consumer_lib/.consumer_lib.objs/byte/consumer.cmo",
        "_build/default/consumer_lib/.consumer_lib.objs/byte/consumer.cmt"
      ]
    }
  ]
