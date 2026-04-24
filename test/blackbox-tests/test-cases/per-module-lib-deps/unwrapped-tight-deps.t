Per-module tight deps within an unwrapped library.

When a consumer module references a specific module of an unwrapped
dependency library, dune emits dynamic dependencies on exactly the
referenced module's .cmi/.cmx files — not a directory-wide glob over
every module of the library. The consumer's rule only invalidates
when a module it actually reads changes.

This test exercises the tightening with a consumer module [C] that
references only [A2] of three entry modules in the dependency [base].
Editing [A1] or [A3] must leave [C] untouched; editing [A2] must
rebuild [C].

See: https://github.com/ocaml/dune/issues/4572
See: https://github.com/ocaml/dune/pull/14116#issuecomment-4301275263

  $ cat > dune-project <<EOF
  > (lang dune 3.0)
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
to keep [consumer] a multi-module stanza so ocamldep is never short-
circuited and the rebuild count for [c] reflects the per-module
filter's work independently of any skip-ocamldep heuristic:

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

Changing A1's interface — a module C does not reference — must not
rebuild C:

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
  0

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
  0

Changing A2's interface — the one module C does reference — must
rebuild C:

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
