Testing the timeout functionality of cram tests.

First we create a cram test that will take less than our time budget. This will
allow the test to fail. (Since "hi" needs to be promoted).

  $ make_dune_project 3.20

  $ cat > dune <<EOF
  > (cram
  >  (timeout 1))
  > EOF

  $ cat > test.t <<EOF
  >   $ echo hi
  > EOF

  $ dune test test.t
  File "test.t", line 1, characters 0-0:
  --- test.t
  +++ test.t.corrected
  @@ -1 +1,2 @@
     $ echo hi
  +  hi
  [1]

Next we create a cram test that will take longer than our timeout budget which
will cause dune to kill the test.

  $ cat > dune <<EOF
  > (cram
  >  (timeout 0.5))
  > EOF

  $ cat > test.t <<EOF
  >   $ echo hi
  >   $ echo partial; sleep 2
  >   $ echo skipped
  > EOF

The cram test will take 2 seconds to run unless it is killed. We make sure this
fails earlier by passing a timeout command in front of dune. Our expected
behaviour is for dune to present the partially assembled diff.

  $ timeout 3 dune test test.t 2>&1 | sed 's/echo hi/command/' | sed 's/sleep 2/command/'
  File "test.t", line 1, characters 0-0:
  --- test.t
  +++ test.t.corrected
  @@ -1,3 +1,7 @@
     $ command
  +  hi
     $ echo partial; command
  +  partial
  +  [timed out]
     $ echo skipped
  +  [not ran]
  File "test.t", line 1, characters 0-0:
  Error: Cram test timed out
  A time limit of 0.50s has been set in dune:2
  [1]

Timeouts should not be cached as successful cram output.

  $ cram_process_trace () {
  >   dune trace cat | jq_dune \
  >     'processes | select(.args.categories == ["cram"]) | .args | {error,name}'
  > }

  $ cat > test.t <<EOF
  >   $ echo hi; sleep 2
  > EOF

  $ timeout 3 dune test test.t >output 2>&1; [ $? -eq 1 ]
  $ cram_process_trace
  {
    "error": "got signal KILL",
    "name": "test.t"
  }
  $ timeout 3 dune test test.t >output 2>&1; [ $? -eq 1 ]
  $ cram_process_trace
  {
    "error": "got signal KILL",
    "name": "test.t"
  }

Timeouts should always fail, even if the timeout output already matches the
contents of the cram test.

  $ cat > test.t <<EOF
  >   $ echo hi
  >   hi
  >   $ echo partial; sleep 2
  >   partial
  >   [timed out]
  >   $ echo skipped
  >   [not ran]
  > EOF

  $ timeout 3 dune test test.t 2>&1 | sed 's/sleep 2/command/'
  File "test.t", line 1, characters 0-0:
  Error: Cram test timed out
  A time limit of 0.50s has been set in dune:2
  [1]

