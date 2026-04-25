Two unwrapped libraries expose an entry module of the same name.
A consumer library lists both and references the name in its code.
OCaml's [-I] path resolution picks the lib whose [-I] comes first
on the command line (the order of declared libraries in
[(libraries ...)]), so the consumer compiles against that lib's
version of the module. The other lib's same-named module is
shadowed — unreachable from consumer code.

This test is observational: it records the rebuild-target count
for a consumer module after editing the *losing* (shadowed) lib's
same-named module. The expected count on current [main] is
positive because cross-library dep tracking is library-level: the
consumer depends on a glob over each declared library's public
cmi directory, so any change to the losing lib's public cmis
invalidates the consumer.

A future per-module dependency filter (#4572) would not
automatically improve this case: without qualified-path analysis
the filter cannot know which lib's [Shared] the consumer actually
resolves through, and must conservatively depend on both.

See: https://github.com/ocaml/dune/issues/4572

  $ cat > dune-project <<EOF
  > (lang dune 3.0)
  > EOF

  $ mkdir lib_a
  $ cat > lib_a/dune <<EOF
  > (library (name lib_a) (wrapped false))
  > EOF
  $ cat > lib_a/shared.ml <<EOF
  > let from_a = "a"
  > EOF

  $ mkdir lib_b
  $ cat > lib_b/dune <<EOF
  > (library (name lib_b) (wrapped false))
  > EOF
  $ cat > lib_b/shared.ml <<EOF
  > let from_b = "b"
  > EOF

[consumer] lists [lib_a] first, so [-I lib_a/.objs] comes before
[-I lib_b/.objs] on the command line and OCaml resolves [Shared]
to [lib_a/shared.cmi]:

  $ mkdir consumer
  $ cat > consumer/dune <<EOF
  > (library (name consumer) (wrapped false) (libraries lib_a lib_b))
  > EOF
  $ cat > consumer/c.ml <<EOF
  > let _ = Shared.from_a
  > EOF
  $ cat > consumer/d.ml <<EOF
  > let _ = ()
  > EOF

  $ dune build @check

Edit [lib_b]'s [Shared] — which the consumer does *not* resolve
through, because [lib_a] wins under [-I] order. Consumer still
rebuilds because its dependency on [lib_b]'s object directory is
a glob:

  $ cat > lib_b/shared.ml <<EOF
  > let from_b = "b"
  > let also_from_b = 42
  > EOF
  $ dune build @check
  $ dune trace cat | jq -s 'include "dune"; [.[] | targetsMatchingFilter(test("consumer/\\.consumer\\.objs/byte/c\\."))] | length > 0'
  true
