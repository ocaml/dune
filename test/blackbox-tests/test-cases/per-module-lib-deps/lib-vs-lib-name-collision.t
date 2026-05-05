Two unwrapped libraries expose an entry module of the same name.
A consumer library lists both and references the name in its code.
OCaml's [-I] path resolution picks the lib whose [-I] comes first
on the command line (the order of declared libraries in
[(libraries ...)]), so the consumer compiles against that lib's
version of the module. The other lib's same-named module is
shadowed — unreachable from consumer code.

This test is observational: it records the rebuild-target count
for a consumer module after editing the *shadowed* lib's
same-named module. The expected count on current [main] is
positive because cross-library dep tracking is library-level: the
consumer depends on a glob over each declared library's public
cmi directory, so any change to the shadowed lib's public cmis
invalidates the consumer.

A future per-module dependency filter (#4572) would not
automatically improve this case: without qualified-path analysis
the filter cannot know which lib's [Shared] the consumer actually
resolves through, and must conservatively depend on both.

See: https://github.com/ocaml/dune/issues/4572

  $ cat > dune-project <<EOF
  > (lang dune 3.23)
  > EOF

  $ mkdir active_lib
  $ cat > active_lib/dune <<EOF
  > (library (name active_lib) (wrapped false))
  > EOF
  $ cat > active_lib/shared.ml <<EOF
  > let from_active = "a"
  > EOF

  $ mkdir shadowed_lib
  $ cat > shadowed_lib/dune <<EOF
  > (library (name shadowed_lib) (wrapped false))
  > EOF
  $ cat > shadowed_lib/shared.ml <<EOF
  > let from_shadowed = "b"
  > EOF

[consumer_lib] lists [active_lib] first, so [-I active_lib/.objs]
comes before [-I shadowed_lib/.objs] on the command line and OCaml
resolves [Shared] to [active_lib/shared.cmi]:

  $ mkdir consumer_lib
  $ cat > consumer_lib/dune <<EOF
  > (library
  >  (name consumer_lib)
  >  (wrapped false)
  >  (libraries active_lib shadowed_lib))
  > EOF
  $ cat > consumer_lib/consumer.ml <<EOF
  > let _ = Shared.from_active
  > EOF
  $ cat > consumer_lib/filler.ml <<EOF
  > let _ = ()
  > EOF

  $ dune build @check

Edit [shadowed_lib]'s [Shared] — which [consumer] does *not* resolve
through, because [active_lib] wins under [-I] order. [consumer]
still rebuilds because its dependency on [shadowed_lib]'s object
directory is a glob:

  $ cat > shadowed_lib/shared.ml <<EOF
  > let from_shadowed = "b"
  > let also_from_shadowed = 42
  > EOF
  $ dune build @check
  $ dune trace cat | jq -s 'include "dune"; [.[] | targetsMatchingFilter(test("consumer_lib/\\.consumer_lib\\.objs/byte/consumer\\."))] | length > 0'
  true
