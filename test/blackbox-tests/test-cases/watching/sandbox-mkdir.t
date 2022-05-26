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
  >  (action (chdir subdir (bash "echo hello > ../target; pwd"))))
  > EOF

  $ start_dune

  $ build test/target
  Success
  $ cat test/target
  hello

# CR-someday amokhov: Actually, it doesn't because of the broken memoization of
# mkdirs in [build_system.ml].

  $ rm _build/default/test/target test/target
  $ build test
  Failure

  $ with_timeout dune shutdown
  $ cat .#dune-output
  $TESTCASE_ROOT/_build/.sandbox/2e3efa702a8fa42966a2f897c1c27efc/default/test/subdir
  Success, waiting for filesystem changes...
  Error:
  chdir(_build/.sandbox/2e3efa702a8fa42966a2f897c1c27efc/default/test/subdir): No such file or directory
  -> required by _build/default/test/target
  -> required by alias test/all
  -> required by alias test/default
  Had errors, waiting for filesystem changes...
