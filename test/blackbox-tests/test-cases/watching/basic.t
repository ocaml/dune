Basic tests for the file-watching mode.

  $ . ./helpers.sh

----------------------------------------------------------------------------------
* Compile a simple rule

  $ echo "(lang dune 2.0)" > dune-project

  $ cat > x <<EOF
  > original-contents
  > EOF

  $ cat >dune <<EOF
  > (rule
  >  (target y)
  >  (deps x)
  >  (action (bash "cat x > y")))
  > EOF

  $ start_dune y

  $ dune_wait
  Success
  $ cat _build/default/y
  original-contents

  $ echo new-contents > x

  $ dune_wait
  Success
  $ cat _build/default/y
  new-contents

  $ echo new-contents2 > x

  $ dune_wait
  Success
  $ cat _build/default/y
  new-contents2

----------------------------------------------------------------------------------
* File rename

  $ mv x z
  $ dune_wait
  Failure

  $ echo new-contents3 > z

  $ dune_wait
  Failure

  $ mv z x
  $ dune_wait
  Success
  $ cat _build/default/y
  new-contents3

  $ with_timeout dune shutdown
  $ cat dune-output
  Success, waiting for filesystem changes...
  Success, waiting for filesystem changes...
  Success, waiting for filesystem changes...
  File "dune", line 1, characters 0-57:
  1 | (rule
  2 |  (target y)
  3 |  (deps x)
  4 |  (action (bash "cat x > y")))
  Error: No rule found for x
  Had errors, waiting for filesystem changes...
  Success, waiting for filesystem changes...
