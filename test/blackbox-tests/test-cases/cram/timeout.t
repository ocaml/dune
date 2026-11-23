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

Next we create a cram test that blocks until our timeout budget expires and dune
kills the test.

  $ cat > dune <<EOF
  > (cram
  >  (timeout 0.5))
  > EOF

  $ cat > test.t <<EOF
  >   $ echo hi
  >   $ echo partial
  >   > rm -f block
  >   > mkfifo block
  >   > cat block
  >   $ echo skipped
  > EOF

The cram test blocks forever unless it is killed. Give the outer watchdog
headroom for Dune to start and report its own 0.5-second timeout, including the
partial diff, even on a busy runner.

  $ timeout 10 dune test test.t 2>&1 | sed 's/echo hi/command/'
  File "test.t", line 1, characters 0-0:
  --- test.t
  +++ test.t.corrected
  @@ -1,6 +1,10 @@
     $ command
  +  hi
     $ echo partial
     > rm -f block
     > mkfifo block
     > cat block
  +  partial
  +  [timed out]
     $ echo skipped
  +  [not ran]
  File "test.t", line 1, characters 0-0:
  Error: Cram test timed out
  A time limit of 0.50s has been set in dune:2
  [1]

Partial output from timed-out commands should still be sanitized.

  $ cat > test.t <<EOF
  >   $ pwd
  >   > rm -f block
  >   > mkfifo block
  >   > cat block
  > EOF

  $ timeout 10 dune test test.t 2>&1
  File "test.t", line 1, characters 0-0:
  --- test.t
  +++ test.t.corrected
  @@ -2,3 +2,5 @@
     > rm -f block
     > mkfifo block
     > cat block
  +  $TESTCASE_ROOT
  +  [timed out]
  File "test.t", line 1, characters 0-0:
  Error: Cram test timed out
  A time limit of 0.50s has been set in dune:2
  [1]

Timeouts should not be cached as successful cram output.

  $ cram_process_trace () {
  >   dune trace cat | jq_dune \
  >     'processes | select(.args.categories == ["cram"]) | .args | {error,name}'
  > }
  $ expect_dune_timeout () {
  >   timeout 10 dune test test.t >output 2>&1
  >   status=$?
  >   if [ "$status" -ne 1 ] || ! grep -q '^Error: Cram test timed out$' output; then
  >     printf 'Expected a Dune cram timeout (exit 1); got exit %s\n' "$status"
  >     cat output
  >     return 1
  >   fi
  > }

  $ cat > test.t <<EOF
  >   $ echo hi
  >   > rm -f block
  >   > mkfifo block
  >   > cat block
  > EOF

  $ expect_dune_timeout
  $ cram_process_trace
  {
    "error": "got signal KILL",
    "name": "test.t"
  }
  $ expect_dune_timeout
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
  >   $ echo partial
  >   > rm -f block
  >   > mkfifo block
  >   > cat block
  >   partial
  >   [timed out]
  >   $ echo skipped
  >   [not ran]
  > EOF

  $ timeout 10 dune test test.t 2>&1
  File "test.t", line 1, characters 0-0:
  Error: Cram test timed out
  A time limit of 0.50s has been set in dune:2
  [1]

