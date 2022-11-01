Test that Dune mkdirs the right set of directories in the sandbox.

  $ . ./helpers.sh

----------------------------------------------------------------------------------
* Compile a simple rule

  $ echo "(lang dune 2.0)" > dune-project

  $ mkdir test
  $ cat >test/dune <<EOF
  > (rule
  >  (target target)
  >  (mode promote)
  >  (deps (sandbox always))
  >  (action (chdir subdir (system "echo hello > ../target; pwd"))))
  > EOF

  $ start_dune

  $ build test/target
  Success
  $ cat test/target
  hello

Now force a rebuild. This suceeds (in the past it could fail due to [mkdir]
memoization).

  $ rm _build/default/test/target test/target
  $ build test
  Success

  $ with_timeout dune shutdown
  $ cat .#dune-output | sed -e 's#.sandbox/[^/]*/default/test/subdir#.sandbox/<hash>/default/test/subdir#'
  $TESTCASE_ROOT/_build/.sandbox/<hash>/default/test/subdir
  Success, waiting for filesystem changes...
  $TESTCASE_ROOT/_build/.sandbox/<hash>/default/test/subdir
  Success, waiting for filesystem changes...
