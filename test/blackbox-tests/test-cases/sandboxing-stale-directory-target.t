A faulty test escapes the sandbox by creating its target outside the sandbox

  $ cat >dune-project <<EOF
  > (lang dune 3.11)
  > (using directory-targets 0.1)
  > EOF

  $ cat >dune <<EOF
  > (rule
  >  (target (dir foo))
  >  (action (system "mkdir $PWD/_build/default/foo && mkdir foo")))
  > EOF

  $ dune build foo/ --sandbox=copy 2>&1 | sed -E 's/characters [0-9]+-[0-9]+/characters <REDACTED>/'
  File "dune", lines 1-3, characters <REDACTED>:
  1 | (rule
  2 |  (target (dir foo))
  3 |  (action (system "mkdir $TESTCASE_ROOT/_build/default/foo && mkdir foo")))
  Error: Target _build/default/foo of kind "directory" already exists in the
  build directory
  Hint: delete this file manually or check the permissions of the parent
  directory of this file
