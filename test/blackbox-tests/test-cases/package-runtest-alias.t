Test that we generate a directory specific runtest alias if dir is set for
a package:

  $ cat > dune-project << EOF
  > (lang dune 3.23)
  > (generate_opam_files true)
  > (package
  >  (name foo)
  >  (dir foo))
  > EOF

  $ dune build @check
  $ cat foo.opam | grep -o '"@runtest/foo" {with-test}'
  "@runtest/foo" {with-test}
