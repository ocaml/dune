The [@opam] diff action conditionally depends on the source opam file while it
exists. Removing that file must invalidate the action so Dune records a
creation promotion.

  $ cat > dune-project << EOF
  > (lang dune 3.24)
  > (generate_opam_files)
  > (package
  >  (name foo)
  >  (allow_empty))
  > EOF

Create [foo.opam], then prime [@opam]'s rule cache with a successful build
while the source file exists.

  $ dune build @opam > /dev/null 2>&1
  [1]
  $ dune promote
  Promoting _build/default/foo.opam.generated to foo.opam.
  $ dune build @opam

The cached action is incorrectly reused after deleting [foo.opam].

  $ rm foo.opam
  $ dune build @opam
  $ dune promotion list
  $ test ! -e foo.opam
