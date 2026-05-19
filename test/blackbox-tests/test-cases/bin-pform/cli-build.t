Build a binary using a %{bin:..} form.

  $ cat >dune-project <<EOF
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

BUG: %{bin:bar} expands using _build/default as its base, producing
"../install/default/bin/bar" rather than a path to a buildable
target. See #3324.

CR-soon Alizter: fix the expansion to point at the build artifact.

  $ dune build '%{bin:bar}'
  Error: File unavailable:
  $TESTCASE_ROOT/../install/default/bin/bar
  [1]
