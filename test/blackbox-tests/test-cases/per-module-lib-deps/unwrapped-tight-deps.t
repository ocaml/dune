Baseline: consumer-module rebuild count when individual modules of
an unwrapped dependency library change.

This is an observational test. It records the number of rebuild
targets for a single consumer module C when each of three entry
modules (A1, A2, A3) of the dependency library [base] has its
interface edited. C references only A2.

On current main, editing any one of A1/A2/A3 causes C to rebuild
because library-level dependency filtering (and, within that,
per-module tightening) is not yet in place: the consumer is
conservatively rebuilt whenever any entry module's cmi changes.
Work on https://github.com/ocaml/dune/issues/4572 is expected to
tighten this, at which point editing A1 or A3 leaves C untouched
and the emitted counts are promoted.

See: https://github.com/ocaml/dune/issues/4572

  $ cat > dune-project <<EOF
  > (lang dune 3.22)
  > EOF

base is an unwrapped library with three entry modules, each with an
explicit interface so signature changes propagate through .cmi files:

  $ mkdir base
  $ cat > base/dune <<EOF
  > (library (name base) (wrapped false))
  > EOF
  $ cat > base/a1.ml <<EOF
  > let v = 1
  > EOF
  $ cat > base/a1.mli <<EOF
  > val v : int
  > EOF
  $ cat > base/a2.ml <<EOF
  > let v = 2
  > EOF
  $ cat > base/a2.mli <<EOF
  > val v : int
  > EOF
  $ cat > base/a3.ml <<EOF
  > let v = 3
  > EOF
  $ cat > base/a3.mli <<EOF
  > val v : int
  > EOF

consumer has two modules. The module of interest, [c.ml], references
only [A2] from [base]. A second, unused module [d.ml] is present only
to keep [consumer] a multi-module stanza: dune skips ocamldep for
single-module stanzas with no library deps as an optimisation, and
the per-module-lib-deps filter depends on ocamldep output. Including
[d.ml] isolates this test from the skip-ocamldep optimisation so the
rebuild count for [c] reflects only the per-module filter's work:

  $ mkdir consumer
  $ cat > consumer/dune <<EOF
  > (library (name consumer) (wrapped false) (libraries base))
  > EOF
  $ cat > consumer/c.ml <<EOF
  > let w = A2.v
  > EOF
  $ cat > consumer/d.ml <<EOF
  > let _ = ()
  > EOF

  $ dune build @check

Edit A1's interface — a module C does not reference — and record
the rebuild-target count for C:

  $ cat > base/a1.mli <<EOF
  > val v : int
  > val extra : unit -> int
  > EOF
  $ cat > base/a1.ml <<EOF
  > let v = 1
  > let extra () = 7
  > EOF
  $ dune build @check
  $ dune trace cat | jq -s 'include "dune"; [.[] | targetsMatchingFilter(test("consumer/\\.consumer\\.objs/byte/c\\."))] | length'
  1

Same for A3:

  $ cat > base/a3.mli <<EOF
  > val v : int
  > val other : string
  > EOF
  $ cat > base/a3.ml <<EOF
  > let v = 3
  > let other = "hi"
  > EOF
  $ dune build @check
  $ dune trace cat | jq -s 'include "dune"; [.[] | targetsMatchingFilter(test("consumer/\\.consumer\\.objs/byte/c\\."))] | length'
  1

Edit A2's interface — the one module C does reference — and check
that the count is positive (C must rebuild):

  $ cat > base/a2.mli <<EOF
  > val v : int
  > val new_fn : int -> int
  > EOF
  $ cat > base/a2.ml <<EOF
  > let v = 2
  > let new_fn x = x + 1
  > EOF
  $ dune build @check
  $ dune trace cat | jq -s 'include "dune"; [.[] | targetsMatchingFilter(test("consumer/\\.consumer\\.objs/byte/c\\."))] | length > 0'
  true
