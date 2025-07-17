Testing the timeout functionality of cram tests.

First we check the version guard.

  $ cat > dune-project <<EOF
  > (lang dune 3.19)
  > EOF

  $ cat > dune <<EOF
  > (cram
  >  (timeout 1))
  > EOF

  $ dune build
  File "dune", line 2, characters 1-12:
  2 |  (timeout 1))
       ^^^^^^^^^^^
  Error: 'timeout' is only available since version 3.20 of the dune language.
  Please update your dune-project file to have (lang dune 3.20).
  [1]

  $ cat > dune-project <<EOF
  > (lang dune 3.20)
  > EOF

Next we create a cram test that will take less than our time budget. This will
allow the test to fail. (Since "hi" needs to be promoted).

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

  $ timeout 1 dune test test.t
  File "test.t", line 1, characters 0-0:
  Error: Cram test timed out. A time limit of 0.00s has been set in dune:2.
  [1]

