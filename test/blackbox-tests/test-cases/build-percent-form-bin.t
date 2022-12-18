Build a binary using a %{bin:..}` form

  $  cat >dune-project <<EOF
  > (lang dune 3.7)
  > (package
  >  (name randompkg))
  > EOF

  $ mkdir bin
  $ touch bin/bar.ml
  $ cat >bin/dune <<EOF
  > (executable
  >  (public_name bar))
  > EOF

  $ dune build '%{bin:bar}'
  Error: File unavailable:
  $TESTCASE_ROOT/../install/default/bin/bar
  [1]
