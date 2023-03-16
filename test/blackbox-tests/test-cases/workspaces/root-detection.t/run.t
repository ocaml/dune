This test checks the 

  $ unset INSIDE_DUNE

  $ ls . dep
  .:
  dep
  dune-project
  dune-workspace
  test1.opam
  
  dep:
  dune-project
  test2.opam

Testing the root detection for:

- dune build -p packages
- dune build
- dune clean

First we test in the workspace root. We expect the root to be the same directory
since that is where the dune-workspace file lives.

  $ dune build -p test2 --verbose |& grep -A 1 'Workspace root:'
  Workspace root:
  $TESTCASE_ROOT
  $ dune build -p test1 --verbose |& grep -A 1 'Workspace root:'
  Workspace root:
  $TESTCASE_ROOT
  $ dune build --verbose |& grep -A 1 'Workspace root:'
  Workspace root:
  $TESTCASE_ROOT
  $ dune clean --verbose |& grep -A 1 'Workspace root:'
  Workspace root:
  $TESTCASE_ROOT

Next we try inside the dep directory. We expect the root to be the parent
directory since that is where the dune-workspace file lives. However strangely
for -p this doesn't appear to be the case.

  $ (cd dep && dune build -p test2 --verbose |& grep -A 1 'Workspace root:')
  Workspace root:
  $TESTCASE_ROOT/dep
  $ (cd dep && dune build -p test1 --verbose |& grep -A 1 'Workspace root:')
  Workspace root:
  $TESTCASE_ROOT/dep
  $ (cd dep && dune build --verbose |& grep -A 1 'Workspace root:')
  Workspace root:
  $TESTCASE_ROOT
  $ (cd dep && dune clean --verbose |& grep -A 1 'Workspace root:')
  Workspace root:
  $TESTCASE_ROOT
