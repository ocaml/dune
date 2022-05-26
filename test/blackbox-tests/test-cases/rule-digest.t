----------------------------------------------------------------------------------
Test that rule digest doesn't depend on irrelevant details of the dune file

  $ echo "(lang dune 3.0)" > dune-project

  $ cat >dune <<EOF
  > (rule
  >  (target target)
  >  (mode promote)
  >  (deps (sandbox always))
  >  (action (bash "echo running...; echo hi > target; pwd")))
  > EOF

  $ dune build target
  running...
  $TESTCASE_ROOT/_build/.sandbox/c93525051ad351e1da2966547b75da6e/default

Let's add a comment to the dune file. It shouldn't affect the rule digest.

  $ cat >dune <<EOF
  > ; hello
  > (rule
  >  (target target)
  >  (mode promote)
  >  (deps (sandbox always))
  >  (action (bash "echo running...; echo hi > target; pwd")))
  > EOF

... and it doesn't.

  $ rm _build/default/target target
  $ dune build @default
  running...
  $TESTCASE_ROOT/_build/.sandbox/c93525051ad351e1da2966547b75da6e/default

Now the same but with an alias.

  $ cat >dune <<EOF
  > (rule
  >  (alias default)
  >  (deps (sandbox always))
  >  (action (bash "echo running...; pwd")))
  > EOF

  $ dune build @default
  running...
  $TESTCASE_ROOT/_build/.sandbox/ece87ba67cb29c741f43bdbaeec66cf1/default

Let's add a comment to the dune file. One might think that it doesn't affect
the rule digest, but it does because all the locations gets shifted.

  $ cat >dune <<EOF
  > ; hello
  > (rule
  >  (alias default)
  >  (deps (sandbox always))
  >  (action (bash "echo running...; pwd")))
  > EOF

... but it does! It would be nice to encode the locations in a way that would
make them more resilient to non semantic changes.

# CR-someday amokhov: Remove actual digests from this test so that we don't
# need to update it when rule digest version changes.

  $ dune build @default
  running...
  $TESTCASE_ROOT/_build/.sandbox/e3d27de4e9e96ac790179cbae28f51da/default
