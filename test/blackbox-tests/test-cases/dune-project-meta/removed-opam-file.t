The [@opam] diff action conditionally depends on the source opam file while it
exists. Removing that file must not let a stale build-tree copy hide the
creation promotion.

  $ cat > dune-project << EOF
  > (lang dune 3.24)
  > (generate_opam_files)
  > (package
  >  (name foo)
  >  (allow_empty))
  > EOF

Create [foo.opam], then leave its build-tree copy behind by successfully
building [@opam] while the source file exists.

  $ dune build @opam > /dev/null 2>&1
  [1]
  $ dune promote
  Promoting _build/default/foo.opam.generated to foo.opam.
  $ dune build @opam

Deleting [foo.opam] must expose the generated file as a creation promotion.

  $ rm foo.opam
  $ dune build @opam > /dev/null 2>&1
  [1]
  $ dune promotion list
  foo.opam
  $ dune promote
  Promoting _build/default/foo.opam.generated to foo.opam.
  $ test -e foo.opam
