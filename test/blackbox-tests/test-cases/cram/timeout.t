Testing the timeout functionality of cram tests.

First we create a cram test that will take less than our time budget. This will
allow the test to fail. (Since "hi" needs to be promoted).

  $ cat > dune-project <<EOF
  > (lang dune 3.20)
  > EOF

  $ cat > dune <<EOF
  > (cram
  >  (timeout 1))
  > EOF

  $ cat > test.t <<EOF
  >   $ echo hi
  > EOF

  $ dune test test.t
  File "test.t", line 1, characters 0-0:
  Error: Files _build/default/test.t and _build/default/test.t.corrected
  differ.
  [1]

Next we create a cram test that will take longer than our timeout budget which
will cause dune to kill the test.

  $ cat > dune <<EOF
  > (cram
  >  (timeout 0.0))
  > EOF

  $ cat > test.t <<EOF
  >   $ echo hi
  >   $ sleep 2
  > EOF

The cram test will take 2 seconds to run unless it is killed. We make sure this
fails earlier by passing a timeout command in front of dune. Our expected
behaviour is for dune to kill the cram test immediately.

  $ timeout 1 dune test test.t 2>&1 | sed 's/echo hi/command/' | sed 's/sleep 2/command/'
  File "test.t", line 1, characters 0-0:
  Error: Cram test timed out while running command:
    $ command
  A time limit of 0.00s has been set in dune:2
  [1]

