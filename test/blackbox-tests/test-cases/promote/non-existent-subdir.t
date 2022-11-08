Test of a rule that tries to promote to a source directory that doesn't exist.

Taken from #3502
  $ cat >dune-project <<EOF
  > (lang dune 3.4)
  > EOF
  $ cat >dune <<EOF
  > (subdir x
  >  (rule
  >   (mode (promote (until-clean)))
  >   (action (with-stdout-to y (echo "z")))))
  > EOF
  $ dune build x/y
  Error: x/.#y.dune-temp: No such file or directory
  -> required by _build/default/x/y
  [1]
  $ cat x/y
  cat: x/y: No such file or directory
  [1]
