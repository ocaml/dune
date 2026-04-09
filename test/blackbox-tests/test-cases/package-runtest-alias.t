Test that we generate a directory specific runtest alias if dir is set for
a package:

  $ cat > dune-project << EOF
  > (lang dune 3.23)
  > (generate_opam_files true)
  > (package
  >  (name foo)
  >  (dir foo))
  > EOF

  $ dune build @runtest 2>&1 | sed -n '1,3p'
  File "foo.opam", line 1, characters 0-0:
  --- foo.opam
  +++ foo.opam.generated
  [1]
  $ dune promotion list
  foo.opam
  $ dune promote
  Promoting _build/default/foo.opam.generated to foo.opam.
  $ cat foo.opam | grep -o '"@runtest/foo" {with-test}'
  "@runtest/foo" {with-test}
