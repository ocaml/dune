Test that cram tests that timeout still offer partial promotion.

  $ cat > dune-project <<EOF
  > (lang dune 3.20)
  > EOF

  $ cat > dune <<EOF
  > (cram
  >  (timeout 0.5))
  > EOF

Create a test with multiple commands where the second one will timeout:

  $ cat > test.t <<EOF
  >   \$ echo "first command"
  >   \$ sleep 10
  >   \$ echo "never reached"
  > EOF

Run the test - it should timeout but still generate a .corrected file.
The warning indicates which command timed out, and the error shows the diff is available:

  $ dune test test.t 2>&1
  File "test.t", line 1, characters 0-0:
  Warning: Cram test timed out while running command:
    $ sleep 10
  A time limit of 0.50s has been set in dune:2
  File "test.t", line 1, characters 0-0:
  Error: Files _build/default/test.t and _build/default/test.t.corrected
  differ.
  [1]

Check what files are generated:

  $ ls _build/default/test.t*
  _build/default/test.t

Check if there's a correction file in the staging area:

  $ ls _build/.promotion-staging/
  test.t

The staging file contains the partial output with the first command's result
and unreachable markers for commands that were cut off by the timeout:

  $ cat _build/.promotion-staging/test.t
    $ echo "first command"
    first command
    $ sleep 10
    ***** UNREACHABLE *****
    $ echo "never reached"
    ***** UNREACHABLE *****

Verify promotion works - it should copy the partial results to the source file:

  $ dune promote
  Promoting _build/default/test.t.corrected to test.t.

After promotion, test.t should have the partial output:

  $ cat test.t
    $ echo "first command"
    first command
    $ sleep 10
    ***** UNREACHABLE *****
    $ echo "never reached"
    ***** UNREACHABLE *****
