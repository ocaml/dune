Test of a rule that tries to promote to a source directory that doesn't exist.

Taken from #3502
  $ make_dune_project 3.4
  $ cat >dune <<EOF
  > (subdir x
  >  (rule
  >   (mode (promote (until-clean)))
  >   (action (with-stdout-to y (echo "z")))))
  > EOF
  $ dune build x/y
  $ cat x/y
  z
